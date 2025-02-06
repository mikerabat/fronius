unit ufrmTestFroniusAPI;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, Forms,
  {$IFDEF FPC} fpjson, {$ELSE} System.JSON, VCL.Mask, {$ENDIF}
  Fronius.EnergyEstimate, Fronius.DBIntf, Fronius.DB, ExtCtrls, StdCtrls;

type

  { TfrmFroniusAPI }

  TfrmFroniusAPI = class(TForm)
    pnlLeft: TPanel;
    memLog: TMemo;
    edFroniusHost: TLabeledEdit;
    btnVersion: TButton;
    btnInverterInfo: TButton;
    btnInverterRealTimeData: TButton;
    btnMeterRealTimeData: TButton;
    btnPowerFlowRealTimeData: TButton;
    btnOhmPIlotRealtime: TButton;
    btnStorageRealTimeData: TButton;
    btnEngerestimate: TButton;
    btnConnectDB: TButton;
    procedure btnInverterInfoClick(Sender: TObject);
    procedure btnEngerestimateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectDBClick(Sender: TObject);
  private
    fEstimateThr : TFroniusEnergyEstThr;
    fDataMod : TFroniusDB;
    {$IFDEF FPC}
    fMsg : string;

    procedure LogMsg;
    {$ENDIF}
    procedure ClearEstimateThread;
    procedure OnEnergyUpdate(Sender : TObject; const period : TEnergyStatus; const PowerSample : TFlowDataSample);
    procedure OnMeterSample(Sender : TObject; const meter : TMeterRealTimeData; const inverterData : TInverterRealTimeData);
    procedure OnEnergyError(Sender : TObject; tick : int64; const msg : string);
    procedure OnLog( Sender : TObject; level : integer; const msg : string);
  public
    { Public-Deklarationen }
  end;

var
  frmFroniusAPI: TfrmFroniusAPI;

implementation

uses Fronius.Data, {$IFDEF UNIX} Fronius.Linux.DB {$ELSE} Fronius.Win.DB {$ENDIF};

{$IFDEF FPC}
{$R *.frm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TfrmFroniusAPI.btnConnectDBClick(Sender: TObject);
var dbAdapt : IFroniusDBAdapter;
    errStr : string;
begin
     if Assigned(fDataMod) then
     begin
          FreeAndNil(fDataMod);
          btnConnectDB.Caption := 'Connect DB';
     end
     else
     begin
          dbAdapt := TdmFronius.Create(nil);
          dbAdapt.SetLogEvt(OnLog);

          if not dbAdapt.ConnectToDB(errStr) then
          begin
               memLog.Lines.Add('Failed to connect to the database');
               memLog.Lines.Add('Failed with msg: ' + errStr);
          end
          else
          begin
               fDataMod := TFroniusDB.Create;
               fDataMod.OnLog := OnLog;

               fDataMod.SetAdapter(dbAdapt);
               btnConnectDB.Caption := 'Disconnect DB';
          end;
     end;
end;

{$IFDEF FPC}
procedure TfrmFroniusAPI.LogMsg;
begin
     memLog.Lines.Add(fMsg);
end;
{$ENDIF}

procedure TfrmFroniusAPI.btnEngerestimateClick(Sender: TObject);
begin
     if not Assigned(fEstimateThr) then
     begin
          btnEngerestimate.Caption := 'Stop Energy Estimate';

          fEstimateThr := TFroniusEnergyEstThr.Create(edFroniusHost.Text);
          fEstimateThr.OnUpdate := OnEnergyUpdate;
          fEstimateThr.OnConnectionErr := OnEnergyError;
          fEstimateThr.OnMeterSample := OnMeterSample;
          fEstimateThr.Start;
     end
     else
     begin
          btnEngerestimate.Caption := 'Start Energy Estimate';
          ClearEstimateThread;
     end;
end;

procedure TfrmFroniusAPI.btnInverterInfoClick(Sender: TObject);
var froniusObj : TFroniusBase;
begin
     froniusObj := nil;
     if Sender = btnInverterInfo
     then
         froniusObj := TFroniusInverterInfo.Create(edFroniusHost.Text)
     else if Sender = btnVersion
     then
         froniusObj := TFroniusAPIInfo.Create(edFroniusHost.Text)
     else if Sender = btnInverterRealTimeData
     then
         froniusObj := TFroniusInverterRealTimeData.Create(edFroniusHost.Text)
     else if Sender = btnMeterRealTimeData
     then
         froniusObj := TFroniusMeterRealTimeData.Create(edFroniusHost.Text)
     else if Sender = btnPowerFlowRealTimeData
     then
         froniusObj := TFroniusPowerFlowRealTimeData.Create(edFroniusHost.Text)
     else if Sender = btnOhmPIlotRealtime
     then
         froniusObj := TFroniusOhmPilotRealtimeData.Create(edFroniusHost.Text)
     else if Sender = btnStorageRealTimeData
     then
         froniusObj := TFroniusStorageRealTimeData.Create(edFroniusHost.Text);

     memLog.Clear;
     try
        if Assigned(froniusObj) then
        begin
             if froniusObj.Init
             then
                 memLog.Text := froniusObj.ToString
             else
                 memLog.Text := 'Failed to get api info: ' + froniusObj.LastErrorMsg;

             memLog.Lines.Add('');
             memLog.Lines.Add(froniusObj.JSONRes);
        end;
     finally
            froniusObj.Free;
     end;
end;

procedure TfrmFroniusAPI.ClearEstimateThread;
begin
     if Assigned(fEstimateThr) then
     begin
          fEstimateThr.SigTerminate;
          fEstimateThr.WaitFor;
          FreeAndNil(fEstimateThr);
     end;
end;

procedure TfrmFroniusAPI.FormDestroy(Sender: TObject);
begin
     ClearEstimateThread;

     if Assigned(fDataMod) then
        FreeAndNil(fDataMod);
end;

procedure TfrmFroniusAPI.OnEnergyError(Sender: TObject; tick: int64;
  const msg: string);
begin
     if Assigned(fDataMod) then
        fDataMod.OnEnergyError(Sender, tick, msg);

     {$IFDEF FPC}
     fMsg := 'Error in Poll thread: ' + msg;
     TThread.Queue(nil, LogMsg);
     {$ELSE}
     TThread.Queue(nil,
                   procedure
                   begin
                        memLog.Lines.Add('Error in Poll thread: ' + msg);
                   end
                   );
     {$ENDIF}
end;

procedure TfrmFroniusAPI.OnEnergyUpdate(Sender: TObject;
  const period: TEnergyStatus; const PowerSample : TFlowDataSample);
var eng, pwr : string;
begin
     if Assigned(fDataMod) then
        fDataMod.OnEnergyUpdate(Sender, period, PowerSample);
     eng := Format('%s to %s: From Grid: %.1f, ToGrid: %.1f, PV: %.1f, PVUse: %.1f, Load: %.1f Wh', [
                    TimeToStr( period.fromTime ),
                    TimeToStr( period.ToTime ),
                    period.EnergyFromGrid/3600,
                    period.EnergyToGrid/3600,
                    period.EnergyPV/3600,
                    period.EnergyPVUse/3600,
                    period.EnergyLoad/3600]);
     pwr := Format('%s: Grid: %.1f, PV: %.1f, Load: %.1f W',
                   [TimeToStr( PowerSample.sampleTime ),
                    PowerSample.PowerGrid,
                    PowerSample.PowerPV,
                    PowerSample.PowerLoad
                    ]);

     {$IFDEF FPC}
     fMsg := eng + #13#10 + pwr + #13#10;
     TThread.Queue(nil, LogMsg);
     {$ELSE}
     TThread.Queue(nil,
                   procedure
                   begin
                        memLog.Lines.Add(eng);
                        memLog.Lines.Add(pwr);
                        memLog.Lines.Add('');
                   end);
     {$ENDIF}
end;

procedure TfrmFroniusAPI.OnLog(Sender: TObject; level: integer;
  const msg: string);
begin
     memLog.Lines.Add(msg);
end;

procedure TfrmFroniusAPI.OnMeterSample(Sender: TObject;
  const meter: TMeterRealTimeData; const inverterData : TInverterRealTimeData);
var eng : string;
    pwr : string;
begin
     if Assigned(fDataMod) then
        fDataMod.OnMeterUpdate(Sender, meter, inverterData);
     eng := Format('%s: Min Abs: %.1f, Plus Abs: %.1f, SumCons: %.1f, SumProd: %.1f Wh', [
                    TimeToStr( meter.sampleTime ),
                    meter.EnergyMinusAbs,
                    meter.EnergyPlusAbs,
                    meter.EnergySumConsumed,
                    meter.EnergySumProduced]);
     pwr := Format('%s: P1: %.1f, P2: %.1f, P3:%.1f, PSum: %.1f W',
                   [TimeToStr( meter.sampleTime ),
                    meter.PowerPhase1,
                    meter.PowerPhase2,
                    meter.PowerPhase3,
                    meter.PowerSum
                    ]);

     {$IFDEF FPC}
     fMsg := eng + #13#10 + pwr + #13#10;
     TThread.Queue(nil, LogMsg);
     {$ELSE}
     TThread.Queue(nil,
                   procedure
                   begin
                        memLog.Lines.Add(eng);
                        memLog.Lines.Add(pwr);
                        memLog.Lines.Add('');
                   end);
     {$ENDIF}
end;

end.
