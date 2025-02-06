unit Fronius.DB;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, Classes, Fronius.Consts, Fronius.EnergyEstimate, SyncObjs,
     Fronius.DBIntf;


type
  TFroniusDB = class(TObject)
  private
    fStartSampleTick : int64;
    fSample : TFlowDataSample;
    fNumSamples : integer;
    fInterval : LongWord;
    fLastError : string;
    fCheckDate : TDateTime;
    fNumKeepDays : integer;
    fLastDay : TDateTime;

    fCritSect : TCriticalSection;
    fOnLog: TLogEvent;
    fAdapter : IFroniusDBAdapter;
    fFs : TFormatSettings;

    procedure UpdateDayStats(const meterData: TMeterRealTimeData; const inverterData : TInverterRealTimeData);
    procedure LLog( level : integer; const msg : string );
    procedure ClearLastDays;
    procedure StoreSample( PowerSample : TFlowDataSample; Num : integer; period : TEnergyStatus );
    procedure StoreDaySample( PowerSample : TFlowDataSample );

    procedure EstimatePower( atDT : TDateTime; var pwrPlus, pwrMinus, pvPower : double );
  public
    property LastError : string read fLastError;
    property OnLog : TLogEvent read fOnLog write fOnLog;

    { Public-Deklarationen }
    procedure SetAdapter( dbAdapter : IFroniusDBAdapter );
    function IsConnected : boolean;

    procedure OnEnergyUpdate(Sender : TObject; const period : TEnergyStatus; const PowerSample : TFlowDataSample);
    procedure OnEnergyError(Sender : TObject; tick : int64; const msg : string);
    procedure OnMeterUpdate(Sender : TObject; const meterData : TMeterRealTimeData; const inverterData : TInverterRealTimeData);

    // ###########################################
    // #### Functions to setup some fake test data for simulating the Smart Meter
    procedure CreateTestData( mon : integer; year : integer );

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Math;

{ TFroniusDB }

// ###########################################
// #### Create Free
// ###########################################

constructor TFroniusDB.Create;
begin
     fCritSect := TCriticalSection.Create;
     fLastDay := now;
     fInterval := 5*60*1000;
     {$IFDEF FPC}
     fFs := DefaultFormatSettings;
     {$ELSE}
     fFs := TFormatSettings.Create;
     {$ENDIF}

     inherited;
end;

procedure TFroniusDB.CreateTestData(mon, year: integer);
var dt1, dt2 : TDateTime;
    meterValues : TMeterRealTimeData;
    //inverterDAta : TInverterRealTimeData;
    actPlusPower : double;
    actMinusPower : double;
    actPV : double;
begin
     // ###########################################
     // #### Clear data for the month
     dt1 := EncodeDate(year, mon, 1);
     dt2 := dt1 + MonthDays[IsLeapYear(year), mon];


     fAdapter.SetSQLText('delete from metervalues where METER_TIMESTAMP>=:t1 and METER_TIMESTAMP < :t2');
     fAdapter.SetParamDT('t1', dt1);
     fAdapter.SetParamDT('t2', dt2);
     fAdapter.ExecQuery;

     fAdapter.SetSQLText('delete from daystats where sampleTime>=:t1 and SampleTime<:t2');
     fAdapter.SetParamDT('t1', dt1);
     fAdapter.SetParamDT('t2', dt2);
     fAdapter.ExecQuery;

     // ###########################################
     // #### Insert fake "meter data" every 15 minutes
     FillChar( meterValues, sizeof(meterValues), 0);
     meterValues.Serial := '123';

     // just any initial value...
     meterValues.EnergySumConsumed := 5000;
     meterValues.EnergySumProduced := 8000;

     while dt1 < dt2 do
     begin
          meterValues.sampleTime := dt1;

          EstimatePower( dt1, actPlusPower, actMinusPower, actPV );

          // store in db...

          dt1 := dt1 + 15*60/SecsPerDay;
     end;
end;

destructor TFroniusDB.Destroy;
begin
     fCritSect.Free;

     inherited;
end;

procedure TFroniusDB.EstimatePower(atDT: TDateTime; var pwrPlus,
  pwrMinus, pvPower: double);
const cMaxMonthPower : Array[0..11] of double = (2800, 3000, 3500, 4200, 5600, 6300, 5800, 4900, 4000, 3500, 3000, 2800);
      cNumSunHoursInMonth : Array[0..11] of double = (7, 8, 10, 12, 14, 16, 14, 12, 10, 8, 7, 6);
var dayFrac : double;
    aYear, aMon, aDay : Word;
    pvProd : double;
    normFact : double;
    pvConsume : double;
begin
     pwrPlus := 0;
     pwrMinus := 0;
     pvProd := 0;

     // maximum at noon - use a quadratic curve
     //
     dayFrac := Frac( atDT );
     DecodeDate(atDT, aYear, aMon, aDay);
     if SecsPerDay*Abs(dayFrac - 0.5) < 3600*(cNumSunHoursInMonth[aMon - 1]/2) then
     begin
          // we are in a window that produces some power
          pvProd := cMaxMonthPower[aMon - 1];
          normFact := 24/(cNumSunHoursInMonth[aMon - 1]/2)*abs(dayFrac - 0.5);
          pvProd := pvProd - pvProd*( 1 - sqr(normFact) );

          pvProd := pvProd*( 1 + random*0.1 );

          if random(100) > 95
          then
              pvConsume := 2500*(1 + random*0.2)
          else
              pvConsume := 150*(1 + random*0.4);
     end
     else
         pvConsume := 100*(1 + random*0.1);


     pwrPlus := Max(0, pvConsume - pvProd);
     pwrMinus := Max(0, pvProd - pvConsume);
end;

function TFroniusDB.IsConnected: boolean;
begin
     Result := Assigned(fAdapter) and fAdapter.IsConnected;
end;

// ###########################################
// #### private
// ###########################################

procedure TFroniusDB.ClearLastDays;
begin
     if not Assigned(fAdapter) then
        exit;
     LLog(8, 'Clearing daysamples < ' + FormatDateTime( fFs.LongDateFormat, now - fNumKeepDays ) );
     fCritSect.Enter;
     try
        fAdapter.SetSQLText('delete from daysamples where sampletime<:dt');
        fAdapter.SetParamDT('dt', now - fNumKeepDays);
        fAdapter.ExecQuery;
     finally
            fCritSect.Leave;
     end;
end;

procedure TFroniusDB.LLog(level: integer; const msg: string);
begin
     if Assigned(fOnLog) then
        fOnLog(self, level, msg);
end;

procedure TFroniusDB.OnEnergyError(Sender: TObject; tick: int64;
  const msg: string);
begin
     // cleanup current
     fStartSampleTick := 0;
     fLastError := msg;

     LLog( 9, msg );
end;

procedure TFroniusDB.OnEnergyUpdate(Sender: TObject;
  const period: TEnergyStatus; const PowerSample: TFlowDataSample);
var curTick : int64;
    locSample : TFlowDataSample;
    locPeriod : TEnergyStatus;
begin
     curTick := fAdapter.TickCnt;

     // at the end of the day cleanup the last 3 days
     if Trunc(fCheckDate) < Trunc(powerSample.sampleTime) then
     begin
          ClearLastDays;
          fCheckDate := powerSample.sampleTime;
     end;

     if fStartSampleTick = 0 then
     begin
          fSample := PowerSample;
          fNumSamples := 1;
          fSample.PowerGrid := Max(0, fSample.PowerGrid);
          fSample.PowerAkku := Max(0, fSample.PowerAkku);
          fStartSampleTick := curTick;
          StoreDaySample(PowerSample);
     end
     else if curTick - fStartSampleTick >= fInterval then
     begin
          // ###########################################
          // #### insert sample into db...
          locSample := fSample;
          locPeriod := period;

          // we are in a thread -> start another one to put the data to the db
          StoreSample( locSample, fNumSamples, locPeriod );

          // restart...
          fStartSampleTick := 0;
          OnEnergyUpdate(Self, period, PowerSample);
     end
     else
     begin
          StoreDaySample(PowerSample);

          fSample.PowerAkku := Max(fSample.PowerAkku, PowerSample.PowerAkku);
          fSample.PowerPV := Max(fSample.PowerPV, PowerSample.PowerPV);
          fSample.PowerGrid := Max(fSample.PowerGrid, PowerSample.PowerGrid);
          fSample.PowerLoad := Max(fSample.PowerLoad, PowerSample.PowerLoad);

          fSample.RelAutonomy := fSample.RelAutonomy + PowerSample.RelAutonomy;
          fSample.RelSelfConsumption := fSample.RelSelfConsumption + PowerSample.RelSelfConsumption;

          inc(fNumSamples);
     end;
end;

procedure TFroniusDB.OnMeterUpdate(Sender: TObject;
  const meterData: TMeterRealTimeData; const inverterData : TInverterRealTimeData);
begin
     if not Assigned(fAdapter) then
        exit;

     {$IFNDEF FPC}
     TThread.CreateAnonymousThread(
         procedure
         begin
     {$ENDIF}
     // in FPC there is no such construct -> as a workaround just update in the main thread
              fCritSect.Enter;
              try
                 fAdapter.SetSQLText('Insert into MeterValues Values(:dt, :ENMinAbs, :ENPlusAbs, :ENCONS, :ENPROD, :PP1, ' +
                                     ':PP2, :PP3, :PSUM, :INVTOTPROD, :INVPAC, :INVYEARPROD, :INVDAYPROD)');
                 fAdapter.SetParamDT('dt', meterData.sampleTime);
                 fAdapter.SetParam('ENMinAbs', meterData.EnergyMinusAbs);
                 fAdapter.SetParam('ENPlusAbs', meterData.EnergyPlusAbs);
                 fAdapter.SetParam('ENCONS', meterData.EnergySumConsumed);
                 fAdapter.SetParam('ENPROD', meterData.EnergySumProduced);
                 fAdapter.SetParam('PP1', meterData.PowerPhase1);
                 fAdapter.SetParam('PP2', meterData.PowerPhase2);
                 fAdapter.SetParam('PP3', meterData.PowerPhase3);
                 fAdapter.SetParam('PSUM', meterData.PowerSum);
                 fAdapter.SetParam('INVTOTPROD', inverterData.TotalEnergy);
                 fAdapter.SetParam('INVPAC', inverterData.PAC);
                 fAdapter.SetParam('INVYEARPROD', inverterData.YearEnergy);
                 fAdapter.SetParam('INVDAYPROD', inverterData.DayEnergy);

                 fAdapter.ExecQuery;

                 fAdapter.Commit;

                 // ###########################################
                 // #### update day statistics
                 UpdateDayStats(meterData, inverterData);
              finally
                     fCritSect.Leave;
              end;
     {$IFNDEF FPC}
         end
     ).Start;
     {$ENDIF}
end;

procedure TFroniusDB.SetAdapter(dbAdapter: IFroniusDBAdapter);
var fronConf : IFroniusConf;
begin
     fAdapter := dbAdapter;

     if fAdapter <> nil then
     begin
          // ###########################################
          // #### read config - init
          fLastError := '';

          fCheckDate := Trunc(now);

          fronConf := FroniusConfig;
          fNumKeepDays := fronConf.DBNumDaysToKeep;

          fInterval := fronConf.SampStoreInterval;

          LLog(8, 'Starting at day ' + DateTimeToStr(fCheckDate, fFS ));
          LLog(8, 'Interval: ' + fInterval.ToString);
          LLog(8, 'Num Days to keep: ' + fNumKeepDays.ToString);

          ClearLastDays;
     end;
end;

procedure TFroniusDB.StoreDaySample(PowerSample: TFlowDataSample);
begin
     if not Assigned(fAdapter) then
        exit;

     {$IFNDEF FPC}
     TThread.CreateAnonymousThread(
         procedure
         begin
     {$ENDIF}
     // in FPC there is no such construct -> as a workaround just update in the main thread
              fCritSect.Enter;
              try
                 fAdapter.SetSQLText('Insert into daysamples Values(:dt, :powerakku, :powergrid, :powerload, :powerpv, :RELAUTONOMY, :RELSELFCONSUMPTION)');
                 fAdapter.SetParamDT('dt', PowerSample.sampleTime);
                 fAdapter.SetParam('powerakku', PowerSample.PowerAkku);
                 fAdapter.SetParam('powerpv', PowerSample.PowerPV);
                 fAdapter.SetParam('powerload', PowerSample.PowerLoad);
                 fAdapter.SetParam('powergrid', PowerSample.PowerGrid);
                 fAdapter.SetParam('RELAUTONOMY', PowerSample.RelAutonomy);
                 fAdapter.SetParam('RELSELFCONSUMPTION', PowerSample.RelSelfConsumption);
                 fAdapter.ExecQuery;

                 fAdapter.Commit;
              finally
                     fCritSect.Leave;
              end;
     {$IFNDEF FPC}
         end
     ).Start;
     {$ENDIF}

end;

procedure TFroniusDB.StoreSample(PowerSample: TFlowDataSample; Num: integer; period : TEnergyStatus);
begin
     if not Assigned(fAdapter) then
        exit;

     // ###########################################
     // #### take the mean...
     if num > 0 then
     begin
          PowerSample.RelAutonomy := PowerSample.RelAutonomy/num;
          PowerSample.RelSelfConsumption := PowerSample.RelSelfConsumption/num;
     end;

     // ###########################################
     // #### store the sample in the database
     {$IFNDEF FPC}
     TThread.CreateAnonymousThread(
         procedure
         begin
     {$ENDIF}
     // in FPC there is no such thing as reference to procedure -> use in the main thread as workarount
              fCritSect.Enter;
              try
                 fAdapter.SetSQLText('Insert into samples Values(:dt, :PV, :Load, :GRID, :RELAUTONOMY, :RELSELFCONSUMPTION)');
                 fAdapter.SetParamDT('dt', PowerSample.sampleTime);
                 fAdapter.SetParam('PV', PowerSample.PowerPV);
                 fAdapter.SetParam('Load', PowerSample.PowerLoad);
                 fAdapter.SetParam('Grid', PowerSample.PowerGrid);
                 fAdapter.SetParam('RELAUTONOMY', PowerSample.RelAutonomy);
                 fAdapter.SetParam('RELSELFCONSUMPTION', PowerSample.RelSelfConsumption);
                 fAdapter.ExecQuery;

                 fAdapter.SetSQLText( 'Update or insert into ENERGYACCUM  ' +
                                      '(fromtime, totime, enfromgrid, entogrid, enload, enpv, enpvuse) ' +
                                      'Values(:fromtime, :totime, :enfromgrid, :entogrid, :enload, :enpv, :enpvuse) ' +
                                      'matching (fromtime)');
                 fAdapter.SetParamDT('fromtime', period.FromTime);
                 fAdapter.SetParamDT('totime', period.ToTime);
                 fAdapter.SetParam('enfromgrid', period.EnergyFromGrid);
                 fAdapter.SetParam('entogrid', period.EnergyToGrid);
                 fAdapter.SetParam('enload', period.EnergyLoad);
                 fAdapter.SetParam('enpv', period.EnergyPV);
                 fAdapter.SetParam('enpvuse', period.EnergyPVUse);
                 fAdapter.ExecQuery;

                 fAdapter.Commit;
              finally
                     fCritSect.Leave;
              end;
     {$IFNDEF FPC}
         end
         ).Start;
     {$ENDIF}
end;


procedure TFroniusDB.UpdateDayStats( const meterData: TMeterRealTimeData; const inverterData : TInverterRealTimeData );
var curDay : TDateTime;
    lastDay : TDateTime;
    meterStart : TMeterRealTimeData;
    enprod, entogrid, enconsume, engrid : double;
    dt : double;
    lastUpdate : TDateTime;
begin
     curDay := now;
     lastUpdate := 0;
     enconsume := 0;
     enprod := 0;
     engrid := 0;
     entogrid := 0;

     // ###########################################
     // #### Update the day statistics according to the measured energy
     fCritSect.Enter;
     try
        fAdapter.SetSQLText('Select first 1 * from DAYSTATS order by SAMPLETIME descending');
        fAdapter.ExecQuery;

        // plusabsolute = engrid: energy from grid
        // minusabsolute = entogrid: engergy to grid
        // enconsume: inverter.totalenergy - energytogrid + energy from grid
        // enproduced: inverter.totalenergy
        if not fAdapter.Eof then
        begin
             lastDay := fAdapter.GetResDT('SAMPLETIME', -1);
             enprod := fAdapter.GetRes('enproduced', 0);
             enconsume := fAdapter.GetRes('ENCONSUME', 0);
             engrid := fAdapter.GetRes('ENGRID', 0);
             entogrid := fAdapter.GetRes('ENTOGRID', 0);
             lastUpdate := fAdapter.GetResDT('LASTUPDATE', curday);
        end;

        fAdapter.Close;

        if Trunc(lastDay) <> Trunc(curDay) then
        begin
             meterStart := meterData;
             meterStart.EnergySumConsumed := inverterData.TotalEnergy - meterData.EnergyMinusAbs + meterData.EnergyPlusAbs;
             meterStart.EnergySumProduced := inverterData.TotalEnergy;

             // do the liner operation only in case the day line has been crossed
             // -> otherwise we just do not count the day...
             if Trunc(curDay) - Trunc(lastDay) = 1 then
             begin
                  // get the last sample and do a linear interpolation
                  // to get the values at the "border" of midnight
                  dt := curDay - lastUpdate;

                  meterStart.EnergyPlusAbs := engrid + (meterData.EnergyPlusAbs - engrid)/dt*(1 - Frac(lastUpdate));
                  meterStart.EnergyMinusAbs := entogrid + (meterData.EnergyMinusAbs - entogrid)/dt*(1 - Frac(lastUpdate));
                  meterStart.EnergySumConsumed := enconsume + (inverterData.TotalEnergy - meterData.EnergyMinusAbs + meterData.EnergyPlusAbs - enconsume)/dt*(1 - Frac(lastUpdate));
                  meterStart.EnergySumProduced := enprod + (inverterData.TotalEnergy - enprod)/dt*(1 - Frac(lastUpdate));

                  fAdapter.SetSQLText('Update Daystats set enproduced=:enproduced, enconsume=:enconsume, ENGRID=:ENGRID, ' +
                                      'entogrid=:entogrid, lastupdate=:lastupdate where SampleTime=:SampleTime');
                  fAdapter.SetParam('ENPRODUCED', meterStart.EnergySumProduced);
                  fAdapter.SetParam('ENCONSUME', meterStart.EnergySumConsumed);
                  fAdapter.SetParam('ENGRID', meterStart.EnergyPlusAbs);
                  fAdapter.SetParam('ENTOGRID', meterStart.EnergyMinusAbs);
                  fAdapter.SetParamDT('lastupdate', Trunc(curDay));

                  fAdapter.ExecQuery;
             end;

             // insert the new day
             fAdapter.SetSQLText('insert into DAYSTATS (SampleTime, ENPRODUCED, ENCONSUME, ENGRID, ENTOGRID, BeginProduced, ' +
                                 'BeginConsume, BeginGrid, BeginToGrid, LastUpdate) Values(:SampleTime, :ENPRODUCED, :ENCONSUME, :ENGRID, :ENTOGRID, ' +
                                 ':BeginProduced, :BeginConsume, :BeginGrid, :BeginToGrid, :LastUpdate)' );

             fAdapter.SetParamDT('SampleTime', Trunc(curDay));
             fAdapter.SetParam('ENPRODUCED', inverterData.TotalEnergy);
             fAdapter.SetParam('ENCONSUME', inverterData.TotalEnergy - meterData.EnergyMinusAbs + meterData.EnergyPlusAbs);
             fAdapter.SetParam('ENGRID', meterData.EnergyPlusAbs);
             fAdapter.SetParam('ENTOGRID', meterData.EnergyMinusAbs);

             fAdapter.SetParam('BeginProduced', meterStart.EnergySumProduced);
             fAdapter.SetParam('BeginConsume', meterStart.EnergySumConsumed);
             fAdapter.SetParam('BeginGrid', meterStart.EnergyPlusAbs);
             fAdapter.SetParam('BeginToGrid', meterStart.EnergyMinusAbs);
             fAdapter.SetParamDT('LastUpdate', curDay);

             fAdapter.ExecQuery;
             fAdapter.Commit;
        end
        else
        begin
             // just update the last elements
             fAdapter.SetSQLText('Update DAYSTATS set ENPRODUCED=:ENPRODUCED, ENCONSUME=:ENCONSUME, ' +
                                 'ENGRID=:ENGRID, ENTOGRID=:ENTOGRID, lastupdate=:lastupdate where sampletime=:Sampletime');
             fAdapter.SetParam('ENPRODUCED', inverterData.TotalEnergy);
             fAdapter.SetParam('ENCONSUME', inverterData.TotalEnergy - meterData.EnergyMinusAbs + meterData.EnergyPlusAbs);
             fAdapter.SetParam('ENGRID', meterData.EnergyPlusAbs);
             fAdapter.SetParam('ENTOGRID', meterData.EnergyMinusAbs);

             fAdapter.SetParamDT('lastupdate', curDay);
             fAdapter.SetParamDT('sampletime', Trunc(curDay));

             fAdapter.ExecQuery;
             fAdapter.Commit;
        end;
     finally
            fCritSect.Leave;
     end;
end;

end.
