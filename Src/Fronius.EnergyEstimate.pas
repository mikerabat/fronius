// ###########################################
// #### Energy consumption estimation based on a tight
// #### loop and integration.
// ###########################################
unit Fronius.EnergyEstimate;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, Classes, SyncObjs, Fronius.Data;

type
  TEnergyStatus = class(TObject)
  protected
    type
      TEnergyFieldType = (ftFromGrid, ftToGrid, ftLoad, ftPV, ftPVUse );
  private
    fC : Array[ftFromGrid..ftPVUse] of double;
    fEnergy : Array[ftFromGrid..ftPVUse] of double;

    fFromTime : TDateTime;
    fToTime : TDateTime;
  protected
    procedure Clear(dt : TDateTime);
    procedure AddEnergy( dt : TDateTime; field : TEnergyFieldType; en : Double );
  public
    property FromTime : TDateTime read fFromTime;
    property ToTime : TDateTime read fToTime;
    property EnergyFromGrid : double read fEnergy[ftFromGrid];
    property EnergyToGrid : double read fEnergy[ftTogrid];
    property EnergyLoad : double read fEnergy[ftLoad];
    property EnergyPV : double read fEnergy[ftPV];
    property EnergyPVUse : double read fEnergy[ftPVUse];
  end;

  TFlowDataSample = record
    sampleTime : TDateTime;
    PowerAkku : double;
    PowerGrid : double;
    PowerLoad : double;
    PowerPV : double;
    RelAutonomy : double;
    RelSelfConsumption : double;
  end;

  TMeterRealTimeData = record
    sampleTime : TDateTime;
    Serial : string;
    EnergyMinusAbs : double;     // in WH
    EnergyPlusAbs : double;
    EnergySumConsumed : double;
    EnergySumProduced : double;
    PowerPhase1 : double;         // in W
    PowerPhase2 : double;
    PowerPhase3 : double;
    PowerSum : double;
  end;

  TInverterRealTimeData = record
    DayEnergy : double;
    PAC : double;
    TotalEnergy : double;
    YearEnergy : double;
  end;

  TOnEnergyUpdate = procedure(Sender : TObject; const period : TEnergyStatus; const PowerSample : TFlowDataSample) of Object;
  TOnMeterSample = procedure(Sender : TObject; const meter : TMeterRealTimeData; const inverterData : TInverterRealTimeData ) of Object;
  TOnConnectionErr = procedure(Sender : TObject; tick : int64; const msg : string) of Object;
  TFroniusEnergyEstThr = class(TThread)
  private
    fOnUpd : TOnEnergyUpdate;
    fInterval : integer;
    fOnConErr: TOnConnectionErr;
    {$IFDEF FPC}
    fEvt : TEventObject;
    {$ELSE}
    fEvt : TSimpleEvent;
    {$ENDIF}

    fFroniusPowerFlow : TFroniusPowerFlowRealTimeData;
    fFroniusMeter : TFroniusMeterRealTimeData;
    fFroniusInverterData : TFroniusInverterRealTimeData;

    flastSampleTick : integer;
    fLastSample : TFlowDataSample;
    fSampleTick : integer;
    fLastErrMsg : string;
    fLastErrMeter : string;
    fLastErrInverter : string;
    fErrorInterval : integer;
    fMeterTick : integer;
    fMeterIntv : Integer;

    // current energy status
    fEnergyStat : TEnergyStatus;
    fCurDateTick : integer;
    fCurDate : TDateTime;
    fOnMeterSample: TOnMeterSample;

    function GetCurrTick : int64;
    function TickAsDateTime( tick : integer ) : TDateTime;
    procedure UpdateStats;
    procedure UpdateMeter;
    procedure ResetStats(dt : TDateTime);
  protected
    procedure Execute; override;
  public
    property OnUpdate : TOnEnergyUpdate read fOnUpd write fOnUpd;
    property OnConnectionErr : TOnConnectionErr read fOnConErr write fOnConErr;
    property OnMeterSample : TOnMeterSample read fOnMeterSample write fOnMeterSample;

    procedure SigTerminate;

    constructor Create( host : string; intv : integer = 2000; meterIntv : integer = 180 );
    destructor Destroy; override;
  end;

implementation

uses Math;

{ TFroniusEnergyEstThr }

constructor TFroniusEnergyEstThr.Create(host: string; intv: integer; meterIntv : integer);
begin
     {$IFDEF FPC}
     fEvt := TEventObject.Create(nil, True, False, '');
     {$ELSE}
     fEvt := TSimpleEvent.Create(nil, True, False, '');
     {$ENDIF}
     fInterval := intv;      // in ms

     fErrorInterval := 30000;
     fMeterIntv := meterIntv*1000; // intput is in seconds
     fMeterTick := 0;
     fSampleTick := 0;
     fFroniusPowerFlow := TFroniusPowerFlowRealTimeData.Create(host);
     fFroniusMeter := TFroniusMeterRealTimeData.Create(host);
     fFroniusInverterData := TFroniusInverterRealTimeData.Create(host);
     fEnergyStat := TEnergyStatus.Create;

     ResetStats(now);

     // we neeed to call start...
     inherited Create( True );
end;

function TFroniusEnergyEstThr.TickAsDateTime(tick : integer ) : TDateTime;
begin
     Result := fCurDate + (tick - fCurDateTick)/(1000*SecsPerDay);
end;

destructor TFroniusEnergyEstThr.Destroy;
begin
     fEvt.Free;
     fFroniusPowerFlow.Free;
     fEnergyStat.Free;

     inherited;
end;

procedure TFroniusEnergyEstThr.Execute;
var waitIntv : integer;
    intv : integer;
begin
     intv := Min(1000, fInterval);
     waitIntv := intv;
     // setup the internal variables so
     fSampleTick := fInterval;
     fMeterTick := fMeterIntv;
     while not Terminated do
     begin
          // ###########################################
          // #### Perform update every wait interval
          try
             if fEvt.WaitFor(waitIntv) = wrTimeout then
             begin
                  inc( fSampleTick, waitIntv);
                  inc( fMeterTick, waitIntv);

                  if fSampleTick >= fInterval then
                  begin
                       fSampleTick := 0;
                       UpdateStats;
                  end;

                  if fMeterTick >= fMeterIntv then
                  begin
                       fMeterTick := 0;
                       UpdateMeter;
                  end;

                  waitIntv := intv;
             end;
          except
                on E : Exception do
                begin
                     // in an error case slow down (network down?)
                     waitIntv := fErrorInterval;
                     if Assigned(fOnConErr) then
                     begin
                          fOnConErr( self, GetCurrTick, E.Message );
                     end;
                end;
          end;
     end;
end;

function TFroniusEnergyEstThr.GetCurrTick: int64;
begin
     Result := Int64(GetTickCount64);
end;

procedure TFroniusEnergyEstThr.ResetStats(dt : TDateTime);
begin
     fCurDate := dt;
     fCurDateTick := GetCurrTick;
     fEnergyStat.Clear(fCurDate);
end;

procedure TFroniusEnergyEstThr.SigTerminate;
begin
     Terminate;
     fEvt.SetEvent;
end;

procedure TFroniusEnergyEstThr.UpdateMeter;
var meter : TMeterRealTimeData;
    inverterData : TInverterRealTimeData;
begin
     if Assigned(fOnMeterSample) then
     begin
          if not fFroniusMeter.Init then
          begin
               // recurring errors shall only be reported once
               if Assigned(fOnConErr) and (fLastErrMeter <> fFroniusMeter.LastErrorMsg) then
                  fOnConErr(self, GetCurrTick, fFroniusMeter.LastErrorMsg);

               fLastErrMeter := fFroniusMeter.LastErrorMsg;
               exit;
          end;

          if not fFroniusInverterData.Init then
          begin
               // recurring errors shall only be reported once
               if Assigned(fOnConErr) and (fLastErrInverter <> fFroniusInverterData.LastErrorMsg) then
                  fOnConErr(self, GetCurrTick, fFroniusInverterData.LastErrorMsg);

               fLastErrInverter := fFroniusInverterData.LastErrorMsg;
               exit;
          end;

          // ###########################################
          // #### report back the sample
          if (fFroniusMeter.NumMeter > 0) and (fFroniusMeter.Enable[0]) then
          begin
               meter.sampleTime := fFroniusMeter.SampleTime[0];
               meter.Serial := fFroniusMeter.Serial[0];
               meter.EnergyMinusAbs := fFroniusMeter.EnergyMinusAbs[0];
               meter.EnergyPlusAbs := fFroniusMeter.EnergyPlusAbs[0];
               meter.EnergySumConsumed := fFroniusMeter.EnergySumConsumed[0];
               meter.EnergySumProduced := fFroniusMeter.EnergySumProduced[0];
               meter.PowerPhase1 := fFroniusMeter.PowerPhase1[0];
               meter.PowerPhase2 := fFroniusMeter.PowerPhase2[0];
               meter.PowerPhase3 := fFroniusMeter.PowerPhase3[0];
               meter.PowerSum := fFroniusMeter.PowerSum[0];


               inverterData.DayEnergy := fFroniusInverterData.DayEnergy;
               inverterData.PAC := fFroniusInverterData.PAC;
               inverterData.TotalEnergy := fFroniusInverterData.TotalEnergy;
               inverterData.YearEnergy := fFroniusInverterData.YearEnergy;

               fOnMeterSample(self, meter, inverterData);
          end;
     end;
end;

procedure TFroniusEnergyEstThr.UpdateStats;
var curTime : integer;
    dt : double;
    p1, p2 : double;
    tim : TDateTime;
  procedure SaveLastSample;
  begin
       fLastSample.sampleTime := fEnergyStat.ToTime;
       fLastSample.PowerAkku := fFroniusPowerFlow.Site.PowerAkku;
       fLastSample.PowerGrid := fFroniusPowerFlow.Site.PowerGrid;
       fLastSample.PowerLoad := fFroniusPowerFlow.Site.PowerLoad;
       fLastSample.PowerPV := fFroniusPowerFlow.Site.PowerPV;
       fLastSample.RelAutonomy := fFroniusPowerFlow.Site.RelAutonomy;
       fLastSample.RelSelfConsumption := fFroniusPowerFlow.Site.RelSelfConsumption;
  end;
begin
     if not fFroniusPowerFlow.Init then
     begin
          // recurring errors shall only be reported once
          if Assigned(fOnConErr) and (fLastErrMsg <> fFroniusPowerFlow.LastErrorMsg) then
             fOnConErr(self, GetCurrTick, fFroniusPowerFlow.LastErrorMsg);

          fLastErrMsg := fFroniusPowerFlow.LastErrorMsg;

          // reset the energy stats...
          FillChar(fLastSample, sizeof(fLastSample), 0);
          flastSampleTick := 0;
          fEnergyStat.Clear( 0 );

          exit;
     end;

     fLastErrMsg := '';
     if flastSampleTick = 0 then
     begin
          // ###########################################
          // #### initialize data struct
          flastSampleTick := GetCurrTick;
          FillChar(fLastSample, sizeof(fLastSample), 0);

          fLastSample.sampleTime := TickAsDateTime( flastSampleTick );
          fEnergyStat.Clear( fLastSample.sampleTime );
          SaveLastSample;
     end
     else
     begin
          // ###########################################
          // #### integrate over the period

          // we use our time reference - I can't find a time point for the power flow
          // sample
          curTime := GetCurrTick;
          dt := (curTime - flastSampleTick)/1000; // in seconds

          tim := TickAsDateTime( curTime );
          fEnergyStat.AddEnergy(tim, ftFromGrid, 0.5*(max(0, fLastSample.PowerGrid) + Max(0, fFroniusPowerFlow.Site.PowerGrid) )*dt);
          // power to grid comes with a negative sign
          fEnergyStat.AddEnergy(tim, ftToGrid, 0.5*(max(0, -fLastSample.PowerGrid) + Max(0, -fFroniusPowerFlow.Site.PowerGrid) )*dt);
          fEnergyStat.AddEnergy(tim, ftLoad, 0.5*(fLastSample.PowerLoad + fFroniusPowerFlow.Site.PowerLoad)*dt);
          fEnergyStat.AddEnergy(tim, ftPV, 0.5*(fLastSample.PowerPV + fFroniusPowerFlow.Site.PowerPV)*dt);

          p2 := Min(fLastSample.PowerLoad, fLastSample.PowerPV);
          p1 := Min(fFroniusPowerFlow.Site.PowerLoad, fFroniusPowerFlow.Site.PowerPV);
          // todo: take akku into account... I don't have one :(
          fEnergyStat.AddEnergy(tim, ftPVUse, 0.5*(p1 + p2)*dt);

          SaveLastSample;
          flastSampleTick := curTime;

          if Assigned(fOnUpd) then
             fOnUpd(Self, fEnergyStat, fLastSample );

          // check for day change -> if so reset the status tickers
          if Trunc(fCurDate) <> Trunc(fEnergyStat.ToTime) then
             ResetStats(fEnergyStat.ToTime);
     end;
end;

{ TEnergyStatus }

procedure TEnergyStatus.AddEnergy(dt: TDateTime; field: TEnergyFieldType;
  en: Double);
var Y, T : double;
begin
     fToTime := dt;

     // kanan add to get more precise adds
     Y := en - fC[field];
     T := fEnergy[field] + Y;
     fC[field] := T - fEnergy[field] - Y;
     fEnergy[field] := T;
end;

procedure TEnergyStatus.Clear(dt : TDateTime);
begin
     fFromTime := dt;
     fToTime := dt;
     Fillchar(fEnergy, sizeof(fEnergy), 0);
     FillChar(fC, sizeof(fC), 0);
end;

end.
