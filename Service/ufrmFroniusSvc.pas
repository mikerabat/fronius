unit ufrmFroniusSvc;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.SvcMgr, System.SyncObjs, Fronius.EnergyEstimate, Fronius.DB;

type
  TfrmFroniusSvc = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private-Deklarationen }
    fCR : TCriticalSection;
    fLogFile : TFileStream;
    fLogLevel : integer;

    fEstimateThr : TFroniusEnergyEstThr;
    fFroniusDB : TFroniusDB;

    procedure LLog( level : integer; const msg : string);
    procedure OnLog(Sender : TObject; level : integer; const msg : string);
    procedure OnEnergyError(Sender: TObject; tick: integer; const msg: string);
    procedure OnEnergyUpdate(Sender: TObject; const period: TEnergyStatus;
      const PowerSample: TFlowDataSample);
    procedure OnMeterUpdate(Sender : TObject; const meterData : TMeterRealTimeData; const inverterData : TInverterRealTimeData);
  public
    function GetServiceController: TServiceController; override;
    { Public-Deklarationen }
  end;

var
  frmFroniusSvc: TfrmFroniusSvc;

implementation

uses Fronius.Consts, Fronius.Data, Fronius.Win.DB;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
     frmFroniusSvc.Controller(CtrlCode);
end;

function TfrmFroniusSvc.GetServiceController: TServiceController;
begin
     Result := ServiceController;
end;

procedure TfrmFroniusSvc.LLog(level: integer; const msg: string);
var s : string;
begin
     if level < fLogLevel then
        exit;

     s := Format('%s: %s' + #13#10, [ FormatDateTime( 'dd.mm.yyyy hh:mm:ss', now ), msg ] );

     fCR.Enter;
     try
        fLogFile.Write(s[1], Length(s)*sizeof(char));
     finally
            fCR.Leave;
     end;
end;

procedure TfrmFroniusSvc.OnEnergyError(Sender: TObject; tick: integer;
  const msg: string);
begin
     LLog( 9, 'Error in Poll thread: ' + msg );

     if fFroniusDB.IsConnected then
        fFroniusDB.OnEnergyError(Sender, tick, msg);

     // ###########################################
     // #### Reconnect if not connected any more...
     // todo...
end;

procedure TfrmFroniusSvc.OnEnergyUpdate(Sender: TObject;
  const period: TEnergyStatus; const PowerSample : TFlowDataSample);
begin
     if fFroniusDB.IsConnected then
        fFroniusDB.OnEnergyUpdate(Sender, period, PowerSample);
end;

procedure TfrmFroniusSvc.OnLog(Sender: TObject; level: integer; const msg: string);
begin
     LLog(level, msg);
end;

procedure TfrmFroniusSvc.OnMeterUpdate(Sender: TObject;
  const meterData: TMeterRealTimeData; const inverterData : TInverterRealTimeData);
begin
     if fFroniusDB.IsConnected then
        fFroniusDB.OnMeterUpdate(Sender, meterData, inverterData);
end;

procedure TfrmFroniusSvc.ServiceCreate(Sender: TObject);
var preamble : TBytes;
    logFile : string;
begin
     fFroniusDB := TFroniusDB.Create;
     fFroniusDB.OnLog := OnLog;

     fCR := TCriticalSection.Create;

     fLogLevel := FroniusConfig.LogLevel;
     logFile := 'Froniussvc.log';

     // ############################################
     // #### prepare log file
     fLogFile := TFileStream.Create(logFile, fmCreate or fmOpenWrite or fmShareDenyWrite);
     preamble := TEncoding.Unicode.GetPreamble;
     fLogFile.Write(preamble[0], Length(preamble));

     LLog(0, 'Fronius service create');
end;

procedure TfrmFroniusSvc.ServiceDestroy(Sender: TObject);
begin
     fCR.Free;
     fLogFile.Free;
end;

procedure TfrmFroniusSvc.ServiceStart(Sender: TService; var Started: Boolean);
var froniusConf : IFroniusConf;
    info : TFroniusAPIInfo;
    dbAdapt : IFroninusDBAdapter;
    errMsg : string;
begin
     LLog(0, 'TFronius.ServiceStart');
     LLog(0, 'reset db adapter');
     fFroniusDB.SetAdapter(nil);

     LLog(1, 'Connecting to db');

     froniusConf := FroniusConfig;
     started := froniusConf.FroniusHost <> '';
     if not started then
        exit;

     // ###########################################
     // #### Now try to contact the host before we fire up the db
     info := TFroniusAPIInfo.Create(froniusConf.FroniusHost);
     try
        try
           started := info.Init;
           if not started then
           begin
                LLog(9, 'Failed to contact host: ' + info.LastErrorMsg);
                exit;
           end;
           LLog(1, 'API info: ' + info.ToString);
        except
              on E: Exception do
              begin
                   LLog(9, 'Error contacting fronius host: ' + E.Message);
                   started := False;
                   exit;
              end;
        end;
     finally
            info.Free;
     end;

     // ###########################################
     // #### fire up the db connection and poll thread
     try
        dbAdapt := TdmFronius.Create(nil);
        dbAdapt.SetLogEvt(OnLog);
        started := dbAdapt.ConnectToDB(errMsg);

        if not started then
           LLog(9, 'Failed to connect to db with message: ' + fFroniusDB.LastError );

        if started then
        begin
             fFroniusDB.SetAdapter(dbAdapt);
             fEstimateThr := TFroniusEnergyEstThr.Create(froniusConf.FroniusHost, 2000, froniusConf.MeterSampleInterval);
             fEstimateThr.OnUpdate := OnEnergyUpdate;
             fEstimateThr.OnConnectionErr := OnEnergyError;
             fEstimateThr.OnMeterSample := OnMeterUpdate;
             fEstimateThr.Start;
        end;
     except
           on E: Exception do
           begin
                LLog(9, 'Exception on db connect: ' + E.Message);
                started := False;
           end;
     end;
end;

procedure TfrmFroniusSvc.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
     fFroniusDB.SetAdapter(nil);
     stopped := True;
end;

end.
