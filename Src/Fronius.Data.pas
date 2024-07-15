// ###########################################
// #### JSON to object converters for the Fronius API
// ###########################################
unit Fronius.Data;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, SysUtils, {$IFDEF FPC} fpjson, {$ELSE} JSON, {$ENDIF} IdHTTP;

// ###########################################
// #### Each api call will be represented by a separate class
// the base class implements the calls to the Fronius converter
{$IFDEF FPC}
type
  TJSONValue = TJSONData;
{$ENDIF}
type

  { TFroniusBase }

  TFroniusBase = class(TObject)
  private
    fLastErrorMsg: string;
  protected
    fHost : string;
    fRequest : TIdHTTP;
    fRes : string;

    function Get(api: string; out retVal: TJSonValue): boolean; virtual;
    function JsonGetStr( jsObj : TJSONValue; name : string ) : string;
    function JsonGetInt( jsObj : TJSONValue; name : string ) : integer;
    function JsonGetInt64( jsObj : TJSONValue; name : string ) : int64;
    function JsonGetDouble( jsObj : TJSONValue; name : string ) : Double;
    function JsonGetBool( jsObj : TJSONValue; name : string ) : boolean;
    function JsonGetObj( jsObj : TJSONValue; name : string ) : TJSONValue;
    function TryJsonGetObj( jsObj : TJSONValue; name : string; var obj : TJSONValue ): boolean;
    function TryJsonGetDouble(jsObj: TJSONValue; name: string; var res: double
      ): boolean;
  public
    property LastErrorMsg : string read fLastErrorMsg;
    class var Host : string; // default Fronius host in case no host is set in the constructor

    function JSONRes : string;
    function Init : boolean; virtual; abstract;

    constructor Create(aHost : string = '');
    destructor Destroy; override;
  end;

// ###########################################
// #### Base API information
type
  TFroniusAPIInfo = class(TFroniusBase)
  private
    fVersion : string;
    fBaseUrl : string;
    fCompatibilitiyRange : string;
  public
    function ToString : string; override;
    function Init : boolean; override;
  end;

// ###########################################
// #### enums
type
  TFroniusOperatingState = (
       osdisabled = 0, osNormal = 1,
       osService = 2, osChareBoost = 3,
       osNearlyDepleted = 4, osSuspended = 5,
       osCalibrate = 6, osGridSupport = 7,
       osDepleteRecovery = 8, osNonOperableTemperature = 9,
       osNonOperableVoltage = 10, osPreheating = 11,
       osStartup = 12, osStopped, osBatteryFull = 14);

  TFroniusMeterLocation = (
        mlLoad = 0, mlGrid = 1, mlReserved = 2,
        mlAddACGenOnly = 3, mlAddACGenBatter = 4,
        mlReserved2 = 255, mlSubLoad = 511); // actuall mlSubload is 256 to 511, mlResrved2 is 5-255


// ###########################################
// #### Recurrent objects
type
  TFroniusBaseHeadInfo = class(TFroniusBase)
  private
    fRequestArguments : string;
    fStatusCode : integer;
    fReason : string;
    fUserMessage : string;
    fTimeStamp : string; // todo: convert to TDateTime
  protected
    function Get(api: string; out retVal: TJSONValue): boolean; override;
  public
    function ToString : string; override;
  end;

// ###########################################
// #### Inverter Information object
type
  TFroniusInverterInfo = class(TFroniusBaseHeadInfo)
  private
    type
      TFroniusInverter = class(TObject)
      private
        fCustomName : string;
        fDT : integer;
        fErrorCode : integer;
        fInverterState : string;
        fPVPower : integer;
        fShow : integer;
        fStatusCode : integer;
        fUniqueID : integer;
      public
        function ToString : string; override;
      end;
  private
    fInverters : Array of TFroniusInverter;
    procedure ClearInverters;
    function ReadOneInverterInfo( inverter : TJSONValue ) : TFroniusInverter;
  public
    function Init : boolean; override;
    function ToString : string; override;

    destructor Destroy; override;
  end;

// ###########################################
// #### Real time data...
type
  TFroniusInverterRealTimeData = class(TFroniusBaseHeadInfo)
  private
    type
      TDataRec = record
        value : double;   // maybe we need an array here
        sUnit : string;

        function ToString : string;
      end;
  private
    fDayEnergy : TDataRec;
    fPAC : TDataRec;
    fTotalEnergy : TDataRec;
    fYearEnergy : TDataRec;

    function MakeDataRec( val : TJSONValue; basePath : string ) : TDataRec;
    function GetDayEnergy: double;
    function GetPAC: double;
    function GetTotalEnergy: double;
    function GetYearEnergy: double;
  public
    property DayEnergy : double read GetDayEnergy;
    property PAC : double read GetPAC;
    property TotalEnergy : double read GetTotalEnergy;
    property YearEnergy : double read GetYearEnergy;

    function Init : boolean; override;
    function ToString : string; override;
  end;

  TFroniusMeterRealTimeData = class(TFroniusBaseHeadInfo)
  private
    type
      TFroniusMeterInfo = class(TObject)
      private
        fManufacturer : string;
        fModel : string;
        fSerial : string;

        fCurrent_AC_Phase_1 : single;
        fCurrent_AC_Phase_2 : single;
        fCurrent_AC_Phase_3 : single;
        fCurrent_AC_Sum : single;

        fEnable : boolean;
        fEnergyReactive_VArAC_Sum_Consumed : single;
        fEnergyReactive_VArAC_Sum_Produced : single;
        fEnergyReal_WAC_Minus_Absolute : single;           // seems to be the same as consumed/produced...
        fEnergyReal_WAC_Plus_Absolute : single;
        fEnergyReal_WAC_Sum_Consumed : single;             // Energy from grid
        fEnergyReal_WAC_Sum_Produced : single;             // Energy to to grid
        fFrequency_Phase_Average : single;
        fMeter_Location_Current : single;
        fPowerApparent_S_Phase_1 : single;
        fPowerApparent_S_Phase_2 : single;
        fPowerApparent_S_Phase_3 : single;
        fPowerApparent_S_Sum : single;
        fPowerFactor_Phase_1 : single;
        fPowerFactor_Phase_2 : single;
        fPowerFactor_Phase_3 : single;
        fPowerFactor_Sum : single;
        fPowerReactive_Q_Phase_1 : single;
        fPowerReactive_Q_Phase_2 : single;
        fPowerReactive_Q_Phase_3 : single;
        fPowerReactive_Q_Sum : single;
        fPowerReal_P_Phase_1 : single;
        fPowerReal_P_Phase_2 : single;
        fPowerReal_P_Phase_3 : single;
        fPowerReal_P_Sum : single;
        fTimeStamp : TDateTime;
        fVisible : boolean;
        fVoltage_AC_PhaseToPhase_12 : single;
        fVoltage_AC_PhaseToPhase_23 : single;
        fVoltage_AC_PhaseToPhase_31 : single;
        fVoltage_AC_Phase_1 : single;
        fVoltage_AC_Phase_2 : single;
        fVoltage_AC_Phase_3 : single;
      public
        function ToString : string; override;
      end;
  private
    fFroniusMeters : Array of TFroniusMeterInfo;

    function ReadOneMeter( jsonVal : TJSonvalue ) : TFroniusMeterInfo;
    procedure ClearMeterDataObjs;
    function GetEnable(index: integer): boolean;
    function GetEnergyMinusAbs(index: integer): double;
    function GetEnergyPlusAbs(index: integer): double;
    function GetEnergySumConsumed(index: integer): double;
    function GetEnergySumProduced(index: integer): double;
    function GetLocation(index: integer): integer;
    function GetModel(index: integer): string;
    function GetNumMeter: integer;
    function GetSampleTime(index: integer): TDateTime;
    function GetSerial(index: integer): string;
    function GetPowerPhase1(index: integer): double;
    function GetPowerSum(index: integer): double;
    function GetPowerPhase2(index: integer): double;
    function GetPowerPhase3(index: integer): double;
  public
    function Init : boolean; override;
    function ToString : string; override;

    property NumMeter : integer read GetNumMeter;
    property SampleTime[index : integer] : TDateTime read GetSampleTime;
    property Model[index : integer] : string read GetModel;
    property Serial[index : integer] : string read GetSerial;
    property Enable[index : integer] : boolean read GetEnable;
    property Location[index : integer] : integer read GetLocation;
    property EnergyMinusAbs[index : integer] : double read GetEnergyMinusAbs;
    property EnergyPlusAbs[index : integer] : double read GetEnergyPlusAbs;
    property EnergySumConsumed[index : integer] : double read GetEnergySumConsumed;
    property EnergySumProduced[index : integer] : double read GetEnergySumProduced;
    property PowerPhase1[index : integer] : double read GetPowerPhase1;
    property PowerPhase2[index : integer] : double read GetPowerPhase2;
    property PowerPhase3[index : integer] : double read GetPowerPhase3;
    property PowerSum[index : integer] : double read GetPowerSum;

    destructor Destroy; override;
  end;

  TFroniusPowerFlowRealTimeData = class(TFroniusBase)
  private
    type
      TFroniuseRealTimeInverterData = class(TObject)
      private
        fDT : integer;
        fEnergyDay : double;
        fEnergyTotal : double;
        fEnergyYear : double;
        fPower : double;
      public
        function ToString : string; override;
      end;
    type
      TFroniusSiteRealTimeData = class(TObject)
      private
        fBackupMode : boolean;
       // fBatteryStandby : boolean;
        fEnergyDay : double;
        fEnergyYear : double;
        fEnergyTotal : double;
        fMeterLocation : string;
        fMode : string;
        // streaming data...
        fPowerAkku : double;
        fPowerGrid : double;
        fPowerLoad : double;
        fPowerPV : double;

        fRelAutonomy : double;
        fRelSelfConsumption : double;
      public
        property BackupMode : boolean read fBackupMode;
//        property BatteryStandBy : boolean read fBatteryStandby;    // not there after update...
        property EnergyDay : double read fEnergyDay;
        property EnergyYear : double read fEnergyYear;
        property EnergyTotal : Double read fEnergyTotal;
        property MeterLocation : string read fMeterLocation;
        property Mode : string read fMode;
        property PowerAkku : double read fPowerAkku;
        property PowerGrid : double read fPowerGrid;
        property PowerLoad : double read fPowerLoad;
        property PowerPV : double read fPowerPV;

        property RelAutonomy : double read fRelAutonomy;
        property RelSelfConsumption : double read fRelSelfConsumption;

        function ToString : string; override;
      end;
  private
    fVersion : string;
    fFroniusInverters : Array of TFroniuseRealTimeInverterData;
    fFroniusSite : TFroniusSiteRealTimeData;

    procedure ClearInverterData;
    function ReadOneInverter( inverter : TJSONValue ) : TFroniuseRealTimeInverterData;
    function ReadFroniusSite( site : TJSONValue ) : TFroniusSiteRealTimeData;
  public
    property Site : TFroniusSiteRealTimeData read fFroniusSite;

    function Init : boolean; override;
    function ToString : string; override;

    destructor Destroy; override;
  end;

  // ###########################################
  // #### Api not implemented - I don't have these devices
  TFroniusOhmPilotRealtimeData = class(TFroniusBase)
  public
    function Init : boolean; override;
  end;

  TFroniusStorageRealTimeData = class(TFroniusBase)
  public
    function Init : boolean; override;
  end;



implementation

uses DateUtils, Windows, Math {$IFDEF FPC}, jsonparser{$ENDIF};

{ TFroniusBase }

constructor TFroniusBase.Create(aHost : string = '');
begin
     inherited Create;

     fHost := aHost;
     fRequest := TIdHTTP.Create(nil);
     fRequest.ReadTimeout := 3000;
     fRequest.ConnectTimeout := 3000;
end;

destructor TFroniusBase.Destroy;
begin
     fRequest.Free;

     inherited;
end;

function TFroniusBase.Get(api: string; out retVal: TJsonValue): boolean;
var {$IFNDEF FPC}
    obj : TJSONObject;
    {$ENDIF}
    aHost : string;
begin
     fLastErrorMsg := '';

     // use the global one in case
     aHost := fHost;
     if aHost = '' then
        aHost := Host;

     retVal := nil;

     try
        Result := api <> '';

        if Result then
        begin
             if api[1] <> '/' then
                api := '/' + api;
        end;
        fRes := fRequest.Get('http://' + aHost + api);

        if fRequest.ResponseCode <> 200 then
           raise Exception.Create('Bad Response Code: ' + fRequest.ResponseCode.ToString );

         {$IFDEF FPC}
         retVal := GetJSON( fRes );
         {$ELSE}
          obj := TJSONObject.Create;
          try
             retVal := obj.ParseJSONValue(fRes);
          finally
                 obj.Free;
          end;
          {$ENDIF}
     except
           on E: Exception do
           begin
                Result := False;
                fLastErrorMsg := E.Message;
           end;
     end;
end;

function TFroniusBase.JsonGetStr(jsObj: TJSONValue; name: string): string;
begin
     {$IFDEF FPC}
     Result := jsObj.GetPath(name).AsString;
     {$ELSE}
     Result := jsObj.GetValue<string>(name);
     {$ENDIF}
end;

function TFroniusBase.JsonGetInt(jsObj: TJSONValue; name: string): integer;
begin
     {$IFDEF FPC}
     Result := jsObj.GetPath(name).AsInteger;
     {$ELSE}
     Result := jsObj.GetValue<integer>(name);
     {$ENDIF}
end;

function TFroniusBase.JsonGetInt64(jsObj: TJSONValue; name: string): int64;
begin
     {$IFDEF FPC}
     Result := jsObj.GetPath(name).AsInt64;
     {$ELSE}
     Result := jsObj.GetValue<int64>(name);
     {$ENDIF}
end;

function TFroniusBase.JsonGetDouble(jsObj: TJSONValue; name: string): Double;
begin
     {$IFDEF FPC}
     Result := jsObj.GetPath(name).AsFloat;
     {$ELSE}
     Result := jsObj.GetValue<double>(name);
     {$ENDIF}
end;

function TFroniusBase.JsonGetBool(jsObj: TJSONValue; name: string): boolean;
begin
     {$IFDEF FPC}
     Result := jsObj.GetPath(name).AsBoolean;
     {$ELSE}
     Result := jsObj.GetValue<boolean>(name);
     {$ENDIF}
end;

function TFroniusBase.JsonGetObj(jsObj: TJSONValue; name: string): TJSONValue;
begin
     {$IFDEF FPC}
     Result := jsObj.GetPath(name);
     {$ELSE}
     Result := jsObj.GetValue<TJSONValue>(name, nil);
     {$ENDIF}
end;

function TFroniusBase.TryJsonGetObj(jsObj: TJSONValue; name: string;
  var obj: TJSONValue): boolean;
begin
     {$IFDEF FPC}
     obj := jsObj.FindPath(name);
     Result := obj <> nil;
     {$ELSE}
     Result := jsObj.TryGetValue<TJSONValue>(name, obj);
     {$ENDIF}
end;

function TFroniusBase.TryJsonGetDouble(jsObj: TJSONValue; name: string;
  var res: double): boolean;
{$IFDEF FPC}
var obj : TJSONValue;
{$ENDIF}
begin
     {$IFDEF FPC}
     obj := jsObj.FindPath(name);
     Result := (obj <> nil) and (not obj.IsNull);
     if Result then
        res := obj.AsFloat;
     {$ELSE}
     Result := jsObj.TryGetValue<Double>(name, res);
     {$ENDIF}
end;


function TFroniusBase.JSONRes: string;
begin
     Result := fRes;
end;

{ TFroniusAPIInfo }

function TFroniusAPIInfo.Init : boolean;
var jsonVal : TJSONValue;
begin
     Result := Get('solar_api/GetAPIVersion.cgi', jsonVal);

     if Result then
     begin
          fVersion := JsonGetStr(jsonVal, 'APIVersion');
          fBaseUrl := JsonGetStr(jsonVal, 'BaseURL');
          fCompatibilitiyRange := JsonGetStr(jsonVal, 'CompatibilityRange');

          jsonVal.Free;
     end;
end;

function TFroniusAPIInfo.ToString: string;
begin
     Result := 'Version = ' + fVersion + #13#10 +
               'BaseURL = ' + fBaseUrl + #13#10 +
               'CompatibilityRange = ' + fCompatibilitiyRange;
end;

{ TFroniusInverterInfo }

procedure TFroniusInverterInfo.ClearInverters;
var i: Integer;
begin
     for i := 0 to Length(fInverters) - 1 do
         fInverters[i].Free;
     fInverters := nil;
end;

destructor TFroniusInverterInfo.Destroy;
begin
     ClearInverters;

     inherited;
end;

{ TFroniusInverterInfo.TFroniusInverter }

function TFroniusInverterInfo.TFroniusInverter.ToString: string;
begin
     with TStringList.Create do
     try
        Add('CustomName = ' + fCustomName);
        Add('DT = ' + IntToStr(fDT));
        Add('Error Code = ' + IntToStr(fErrorCode));
        Add('Inverter State = ' + fInverterState );
        Add('PVPower = ' + IntToStr(fPVPower));
        Add('Show = ' + IntToStr(fShow));
        Add('StatusCode = ' + IntToStr(fStatusCode));
        Add('UniqueID = ' + IntToStr(fUniqueID));

        Result := Text;
     finally
            Free;
     end;
end;


function TFroniusInverterInfo.Init: boolean;
var jsonVal : TJSONValue;
    hlp : TJSONValue;
    idx : integer;
    inverterVal : TJSONValue;
begin
     Result := Get('solar_api/v1/GetInverterInfo.cgi', jsonVal);

     if Result then
     begin
          (*
          {"Body":{
            "Data":{
              "1":{"CustomName":"Mike Home",
                   "DT":1,"ErrorCode":1175,
                   "InverterState":"Sleeping",
                   "PVPower":6400,
                   "Show":1,
                   "StatusCode":13,
                   "UniqueID":"33410026"}
                  }
              },
            "Head":{
                "RequestArguments":{},
                "Status":{
                    "Code":0,
                    "Reason":"",
                    "UserMessage":""},

                "Timestamp":"2022-11-29T21:05:16+00:00"}}
                   *)
          // we support only one inverter
          hlp := JsonGetObj( jsonVal, 'Body.Data.1');

          if hlp <> nil then
          begin
               // Data Section - maybe there can be more??
               idx := 0;
               while TryJsonGetObj(jsonVal, 'Body.Data.' + IntToStr(idx + 1), inverterVal) do
               begin
                    SetLength(fInverters, Length(fInverters) + 1);
                    fInverters[idx] := ReadOneInverterInfo(inverterVal);
                    inc(idx);
               end;
          end;

          jsonVal.Free;
     end;
end;


function TFroniusInverterInfo.ReadOneInverterInfo(
  inverter: TJSONValue): TFroniusInverter;
begin
     Result := TFroniusInverter.Create;

     Result.fCustomName := JsonGetStr( inverter, 'CustomName');
     Result.fDT := JsonGetInt(inverter, 'DT');
     Result.fErrorCode := JsonGetInt(inverter, 'ErrorCode');
     Result.fInverterState := JsonGetStr(inverter, 'InverterState');
     Result.fPVPower := JsonGetInt(inverter, 'PVPower');
     Result.fShow := JsonGetInt(inverter, 'Show');
     Result.fStatusCode := JsonGetInt(inverter, 'StatusCode');
     Result.fUniqueID := JsonGetInt(inverter, 'UniqueID');
end;

function TFroniusInverterInfo.ToString: string;
var heaText : string;
  i: Integer;
begin
     heaText := inherited ToString;

     for i := 0 to Length(fInverters) - 1 do
         Result := Result + fInverters[i].ToString + #13#10;

     Result := Result + heaText;
end;

{ TFroniusRealTimeData }

function TFroniusInverterRealTimeData.GetDayEnergy: double;
begin
     Result := fDayEnergy.value;
end;

function TFroniusInverterRealTimeData.GetPAC: double;
begin
     Result := fPAC.value;
end;

function TFroniusInverterRealTimeData.GetTotalEnergy: double;
begin
     Result := fTotalEnergy.value;
end;

function TFroniusInverterRealTimeData.GetYearEnergy: double;
begin
     Result := fYearEnergy.value;
end;

function TFroniusInverterRealTimeData.Init: boolean;
var jsonVal, hlp : TJSONValue;
begin
     Result := Get( '/solar_api/v1/GetInverterRealtimeData.cgi', jsonVal );

     (*
     {
      "Body" : {
        "Data" : {
         "DAY_ENERGY" : {
            "Unit" : "Wh",
            "Values" : {
               "1" : null
            }
         },
         "PAC" : {
            "Unit" : "W",
            "Values" : {
               "1" : 0.0
            }
         },
         "TOTAL_ENERGY" : {
            "Unit" : "Wh",
            "Values" : {
               "1" : 22835.557222222222
            }
         },
         "YEAR_ENERGY" : {
            "Unit" : "Wh",
            "Values" : {
               "1" : null
            }
         }
      }
   },
   "Head" : {
      "RequestArguments" : {
         "Scope" : "System"
      },
      "Status" : {
         "Code" : 0,
         "Reason" : "",
         "UserMessage" : ""
      },
      "Timestamp" : "2022-12-02T21:40:33+00:00"
   }
}
     *)
     if Result then
     begin
          hlp := JsonGetObj( jsonVal, 'Body.Data');

          fDayEnergy := MakeDataRec(hlp, 'DAY_ENERGY');
          fPAC := MakeDataRec(hlp, 'PAC');
          fTotalEnergy := MakeDataRec(hlp, 'TOTAL_ENERGY');
          fYearEnergy := MakeDataRec(hlp, 'YEAR_ENERGY');
     end;

     jsonVal.Free;
end;

function TFroniusInverterRealTimeData.MakeDataRec(val: TJSONValue;
  basePath: string): TDataRec;
begin
     Result.sUnit := JsonGetStr( val, basePath + '.Unit');
     if not TryJsonGetDouble( val, basePath + '.Values.1', Result.value) then
        Result.value := -1;
end;

function TFroniusInverterRealTimeData.ToString: string;
var headStr : string;
    sl : TStringList;
begin
     headStr := inherited ToString;

     sl := TStringList.Create;
     try
        sl.Add( 'Day Energy = ' + fDayEnergy.ToString );
        sl.Add( 'PAC = ' + fPAC.ToString );
        sl.Add( 'Total Energy = ' + fTotalEnergy.ToString);
        sl.Add( 'Year Energy = ' + fYearEnergy.ToString);

        Result := sl.Text;
     finally
            sl.Free;
     end;

     Result := Result + #13#10 + headStr;
end;

{ TFroniusBaseHeadInfo }

function TFroniusBaseHeadInfo.Get(api: string; out retVal: TJSONValue): boolean;
var head : TJSONValue;
begin
     Result := inherited Get(api, retVal);

     if Result then
     begin
          head := JsonGetObj(retVal, 'Head');

          {$IFDEF FPC}
          fRequestArguments := JsonGetObj(head, 'RequestArguments').AsJson;
          {$ELSE}
          fRequestArguments := JsonGetObj(head, 'RequestArguments').ToJSON;
          {$ENDIF}

          fStatusCode := JsonGetInt(head, 'Status.Code');
          fReason := JsonGetStr(head, 'Status.Reason');
          fUserMessage := JsonGetStr(head, 'Status.UserMessage');
          fTimeStamp := JsonGetStr(head, 'Timestamp'); // todo: convert to TDateTime
     end;
end;

function TFroniusBaseHeadInfo.ToString: string;
begin
     with TStringList.Create do
     try
        Add('HeadRequestArguments = ' + fRequestArguments);
        Add('HeadStatusCode = ' + IntToStr(fStatusCode));
        Add('HeadReason = ' + fReason);
        Add('HeadUserMessage = ' + fUserMessage);
        Add('HeadTimeStamp = ' + fTimeStamp);

        Result := Text;
     finally
            Free;
     end;
end;

{ TFroniusRealTimeData.TDataRec }

function TFroniusInverterRealTimeData.TDataRec.ToString: string;
begin
     Result := Format('%.1f%s', [value, sUnit]);
end;

{ TFroniusMeterRealTimeData }

procedure TFroniusMeterRealTimeData.ClearMeterDataObjs;
var i: Integer;
begin
     for i := 0 to Length(fFroniusMeters) - 1 do
         fFroniusMeters[i].Free;
     fFroniusMeters := nil;
end;

destructor TFroniusMeterRealTimeData.Destroy;
begin
     ClearMeterDataObjs;

     inherited;
end;

function TFroniusMeterRealTimeData.GetEnable(index: integer): boolean;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fEnable;
end;

function TFroniusMeterRealTimeData.GetEnergyMinusAbs(index: integer): double;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fEnergyReal_WAC_Minus_Absolute;
end;

function TFroniusMeterRealTimeData.GetEnergyPlusAbs(index: integer): double;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fEnergyReal_WAC_Plus_Absolute;
end;

function TFroniusMeterRealTimeData.GetEnergySumConsumed(index: integer): double;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fEnergyReal_WAC_Sum_Consumed;
end;

function TFroniusMeterRealTimeData.GetEnergySumProduced(index: integer): double;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fEnergyReal_WAC_Sum_Produced;
end;

function TFroniusMeterRealTimeData.GetLocation(index: integer): integer;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := Round(fFroniusMeters[index].fMeter_Location_Current);
end;

function TFroniusMeterRealTimeData.GetModel(index: integer): string;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fModel;
end;

function TFroniusMeterRealTimeData.GetNumMeter: integer;
begin
     Result := Length(fFroniusMeters);
end;

function TFroniusMeterRealTimeData.GetPowerPhase1(index: integer): double;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fPowerReal_P_Phase_1;
end;

function TFroniusMeterRealTimeData.GetPowerPhase2(index: integer): double;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fPowerReal_P_Phase_2;
end;

function TFroniusMeterRealTimeData.GetPowerPhase3(index: integer): double;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fPowerReal_P_Phase_3;
end;

function TFroniusMeterRealTimeData.GetPowerSum(index: integer): double;
begin
     Result := fFroniusMeters[index].fPowerReal_P_Sum;
end;

function TFroniusMeterRealTimeData.GetSampleTime(index: integer): TDateTime;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fTimeStamp;
end;

function TFroniusMeterRealTimeData.GetSerial(index: integer): string;
begin
     if (index < 0) or (index >= Length(fFroniusMeters))then
        raise Exception.Create('Meter index out of bounds');

     Result := fFroniusMeters[index].fSerial;
end;

function TFroniusMeterRealTimeData.Init: boolean;
var jsonVal : TJSONValue;
    meterVal : TJSONValue;
    idx : integer;
begin
     ClearMeterDataObjs;
     Result := Get( '/solar_api/v1/GetMeterRealtimeData.cgi', jsonVal );

     if Result then
     begin
          idx := 0;
          while TryJsonGetObj(jsonVal, 'Body.Data.' + IntToStr(idx), meterVal) do
          begin
               SetLength(fFroniusMeters, Length(fFroniusMeters) + 1);
               fFroniusMeters[idx] := ReadOneMeter(meterVal);
               inc(idx);
          end;
     end;

     jsonVal.Free;
end;

function TFroniusMeterRealTimeData.ReadOneMeter(
  jsonVal: TJSonvalue): TFroniusMeterInfo;

begin
     Result := TFroniusMeterInfo.Create;

     Result.fCurrent_AC_Phase_1 := JsonGetDouble(jsonVal, 'Current_AC_Phase_1');
     Result.fCurrent_AC_Phase_2 := JsonGetDouble(jsonVal, 'Current_AC_Phase_2');
     Result.fCurrent_AC_Phase_3 := JsonGetDouble(jsonVal, 'Current_AC_Phase_3');
     Result.fCurrent_AC_Sum := JsonGetDouble(jsonVal, 'Current_AC_Sum');

     Result.fManufacturer := JsonGetStr(jsonVal, 'Details.Manufacturer');
     Result.fModel := JsonGetStr(jsonVal, 'Details.Model');
     Result.fSerial := JsonGetStr(jsonVal, 'Details.Serial');

     Result.fEnable := JsonGetBool(jsonVal, 'Enable');

     Result.fEnergyReactive_VArAC_Sum_Consumed := JsonGetDouble(jsonVal, 'EnergyReactive_VArAC_Sum_Consumed');
     Result.fEnergyReactive_VArAC_Sum_Produced := JsonGetDouble(jsonVal, 'EnergyReactive_VArAC_Sum_Produced');
     Result.fEnergyReal_WAC_Minus_Absolute := JsonGetDouble(jsonVal, 'EnergyReal_WAC_Minus_Absolute');
     Result.fEnergyReal_WAC_Plus_Absolute := JsonGetDouble(jsonVal, 'EnergyReal_WAC_Plus_Absolute');
     Result.fEnergyReal_WAC_Sum_Consumed := JsonGetDouble(jsonVal, 'EnergyReal_WAC_Sum_Consumed');
     Result.fEnergyReal_WAC_Sum_Produced := JsonGetDouble(jsonVal, 'EnergyReal_WAC_Sum_Produced');

     Result.fFrequency_Phase_Average := JsonGetDouble(jsonVal, 'Frequency_Phase_Average');
     Result.fMeter_Location_Current := JsonGetDouble(jsonVal, 'Meter_Location_Current');

     Result.fPowerApparent_S_Phase_1 := JsonGetDouble(jsonVal, 'PowerApparent_S_Phase_1');
     Result.fPowerApparent_S_Phase_2 := JsonGetDouble(jsonVal, 'PowerApparent_S_Phase_2');
     Result.fPowerApparent_S_Phase_3 := JsonGetDouble(jsonVal, 'PowerApparent_S_Phase_3');
     Result.fPowerApparent_S_Sum := JsonGetDouble(jsonVal, 'PowerApparent_S_Sum');

     Result.fPowerFactor_Phase_1 := JsonGetDouble(jsonVal, 'PowerFactor_Phase_1');
     Result.fPowerFactor_Phase_2 := JsonGetDouble(jsonVal, 'PowerFactor_Phase_2');
     Result.fPowerFactor_Phase_3 := JsonGetDouble(jsonVal, 'PowerFactor_Phase_3');
     Result.fPowerFactor_Sum := JsonGetDouble(jsonVal, 'PowerFactor_Sum');

     Result.fPowerReactive_Q_Phase_1 := JsonGetDouble(jsonVal, 'PowerReactive_Q_Phase_1');
     Result.fPowerReactive_Q_Phase_2 := JsonGetDouble(jsonVal, 'PowerReactive_Q_Phase_2');
     Result.fPowerReactive_Q_Phase_3 := JsonGetDouble(jsonVal, 'PowerReactive_Q_Phase_3');
     Result.fPowerReactive_Q_Sum := JsonGetDouble(jsonVal, 'PowerReactive_Q_Sum');

     Result.fPowerReal_P_Phase_1 := JsonGetDouble(jsonVal, 'PowerReal_P_Phase_1');
     Result.fPowerReal_P_Phase_2 := JsonGetDouble(jsonVal, 'PowerReal_P_Phase_2');
     Result.fPowerReal_P_Phase_3 := JsonGetDouble(jsonVal, 'PowerReal_P_Phase_3');
     Result.fPowerReal_P_Sum := JsonGetDouble(jsonVal, 'PowerReal_P_Sum');

     Result.fTimeStamp := UnixToDateTime( JsonGetInt64(jsonVal, 'TimeStamp'), True );

     Result.fVisible := JsonGetBool(jsonVal, 'Visible');

     Result.fVoltage_AC_PhaseToPhase_12 := JsonGetDouble(jsonVal, 'Voltage_AC_PhaseToPhase_12');
     Result.fVoltage_AC_PhaseToPhase_23 := JsonGetDouble(jsonVal, 'Voltage_AC_PhaseToPhase_23');
     Result.fVoltage_AC_PhaseToPhase_31 := JsonGetDouble(jsonVal, 'Voltage_AC_PhaseToPhase_31');
     Result.fVoltage_AC_Phase_1 := JsonGetDouble(jsonVal, 'Voltage_AC_Phase_1');
     Result.fVoltage_AC_Phase_2 := JsonGetDouble(jsonVal, 'Voltage_AC_Phase_2');
     Result.fVoltage_AC_Phase_3 := JsonGetDouble(jsonVal, 'Voltage_AC_Phase_3');
end;

function TFroniusMeterRealTimeData.ToString: string;
var idx : integer;
begin
     Result := '';

     for idx := 0 to Length(fFroniusMeters) - 1 do
     begin
          Result := Result + fFroniusMeters[idx].ToString;
          if idx <> Length(fFroniusMeters) - 1 then
             Result := Result + #13#10;
     end;
end;

{ TFroniusPowerFlowRealTimeData }

procedure TFroniusPowerFlowRealTimeData.ClearInverterData;
var i : integer;
begin
     FreeAndNil(fFroniusSite);

     for i := 0 to Length(fFroniusInverters) - 1 do
         fFroniusInverters[i].Free;
     fFroniusInverters := nil;
end;

destructor TFroniusPowerFlowRealTimeData.Destroy;
begin
     ClearInverterData;

     inherited;
end;

function TFroniusPowerFlowRealTimeData.Init: boolean;
var jsonVal, inverterVal : TJSONValue;
    idx : integer;
    hlp : TJSONValue;
begin
     ClearInverterData;
     Result := Get( '/solar_api/v1/GetPowerFlowRealtimeData.fcgi', jsonVal );

     if Result then
     begin
          fVersion := JsonGetStr(jsonVal, 'Body.Data.Version');

          // ###########################################
          // #### REad inverter data
          idx := 0;
          // idx tarts at 1??
          while TryJsonGetObj(jsonVal, 'Body.Data.Inverters.' + IntToStr(idx + 1), inverterVal) do
          begin
               SetLength(fFroniusInverters, Length(fFroniusInverters) + 1);
               fFroniusInverters[idx] := ReadOneInverter(inverterVal);
               inc(idx);
          end;

          // ###########################################
          // #### Read Site data
          hlp := JsonGetObj(jsonVal, 'Body.Data.Site' );
          if (hlp <> nil) and (not (hlp is TJSONNull)) then
          begin
               fFroniusSite := ReadFroniusSite(hlp);

               // todo: smartloads - I don't have an Ohmpilot
          end;
     end;

     jsonVal.Free;
     (*

{
   "Body" : {
      "Data" : {
         "Inverters" : {
            "1" : {
               "DT" : 1,
               "E_Day" : null,
               "E_Total" : 47182.641388888886,
               "E_Year" : null,
               "P" : 0.0
            }
         },
         "Site" : {
            "BackupMode" : false,
            "BatteryStandby" : true,
            "E_Day" : null,
            "E_Total" : 47182.641388888886,
            "E_Year" : null,
            "Meter_Location" : "grid",
            "Mode" : "meter",
            "P_Akku" : null,
            "P_Grid" : 244.19999999999999,
            "P_Load" : -244.19999999999999,
            "P_PV" : 0.0,
            "rel_Autonomy" : 0.0,
            "rel_SelfConsumption" : null
         },
         "Smartloads" : {
            "Ohmpilots" : {}
         },
         "Version" : "12"
      }
   },
   "Head" : {
      "RequestArguments" : {},
      "Status" : {
         "Code" : 0,
         "Reason" : "",
         "UserMessage" : ""
      },
      "Timestamp" : "2022-12-09T06:41:37+00:00"
   }
}

 *)
end;

function TFroniusPowerFlowRealTimeData.ReadFroniusSite(
  site: TJSONValue): TFroniusSiteRealTimeData;
var s : string;
begin
     s := site.ToJSON;

     outputdebugstring(PChar(s));

     Result := TFroniusSiteRealTimeData.Create;
     Result.fBackupMode :=  JsonGetBool(site, 'BackupMode');
     //Result.fBatteryStandby := JsonGetBool(site, 'BatteryStandby');
     if not TryJsonGetDouble(site, 'E_Day', Result.fEnergyDay ) then
        Result.fEnergyDay := -1;
     if not TryJsonGetDouble(site, 'E_Total', Result.fEnergyTotal ) then
        Result.fEnergyTotal := -1;
     if not TryJsonGetDouble(site, 'E_Year', Result.fEnergyYear ) then
        Result.fEnergyYear := -1;

     Result.fMeterLocation := JsonGetStr(site, 'Meter_Location');
     Result.fMode := JsonGetStr(site, 'Mode');

     if not TryJsonGetDouble(site, 'P_Akku', Result.fPowerAkku) then
        Result.fPowerAkku := -1;
     if not TryJsonGetDouble(site, 'P_Grid', Result.fPowerGrid) then
        Result.fPowerGrid := -1;

     // my installation shows a "negative" load...
     // -> make it positive

     if not TryJsonGetDouble(site, 'P_Load', Result.fPowerLoad)
     then
         Result.fPowerLoad := -1
     else
         Result.fPowerLoad := Abs(Result.fPowerLoad);

     if not TryJsonGetDouble(site, 'P_PV', Result.fPowerPV) then
        Result.fPowerPV := -1;

     if not TryJsonGetDouble(site, 'rel_Autonomy', Result.fRelAutonomy) then
        Result.fRelAutonomy := -1;
     if not TryJsonGetDouble(site, 'rel_SelfConsumption', Result.fRelSelfConsumption) then
        Result.fRelSelfConsumption := -1;
end;

function TFroniusPowerFlowRealTimeData.ReadOneInverter(
  inverter: TJSONValue): TFroniuseRealTimeInverterData;
begin
     Result := TFroniuseRealTimeInverterData.Create;

     Result.fDT := JsonGetInt(inverter, 'DT');
     if not TryJsonGetDouble(inverter, 'E_DAY', Result.fEnergyDay) then
        Result.fEnergyDay := -1;
     if not TryJsonGetDouble(inverter, 'E_Total', Result.fEnergyTotal) then
        Result.fEnergyTotal := -1;
     if not TryJsonGetDouble(inverter, 'E_Year', Result.fEnergyYear) then
        Result.fEnergyYear := -1;

     if not TryJsonGetDouble(inverter, 'P', Result.fPower) then
        Result.fPower := -1;
end;

function TFroniusPowerFlowRealTimeData.ToString: string;
var i : Integer;
begin
     Result := 'Version = ' + fVersion + #13#10;
     if Assigned(fFroniusSite) then
        Result := Result + #13#10 + fFroniusSite.ToString;

     for i := 0 to Length(fFroniusInverters) - 1 do
         Result := Result + #13#10 + 'Inverter Nr: ' + (i+1).ToString + #13#10 + fFroniusInverters[i].ToString;
end;

{ TFroniusPowerFlowRealTimeData.TFroniuseRealTimeInverterData }

function TFroniusPowerFlowRealTimeData.TFroniuseRealTimeInverterData.ToString: string;
begin
     Result := 'DT = ' + fDT.ToString + #13#10;
     Result := Result + Format('Day Energy = %.1f Wh' + #13#10, [fEnergyDay]);
     Result := Result + Format('Total Energy = %.1f Wh' + #13#10, [fEnergyTotal]);
     Result := Result + Format('Year Energy = %.1f Wh' + #13#10, [fEnergyYear]);
     Result := Result + Format('Cur Power = %.1f', [fPower]);
end;

{ TFroniusPowerFlowRealTimeData.TFroniusSiteRealTimeData }

function TFroniusPowerFlowRealTimeData.TFroniusSiteRealTimeData.ToString: string;
begin
     Result := 'Backup mode = ' + BoolToStr(fBackupMode, True) + #13#10 +
              // 'Battery Standby = ' + BoolToStr(fBatteryStandby, True) + #13#10 +
               Format('Day Energy = %.1f Wh', [ fEnergyDay ] ) + #13#10 +
               Format('Total Energy = %.1f Wh', [ fEnergyTotal ] ) + #13#10 +
               Format('Year Energy = %.1f Wh', [ fEnergyYear ] ) + #13#10 +
               'Meter Location = ' + fMeterLocation + #13#10 +
               'Mode = ' + fMode + #13#10 +
               Format('Akku Power = %.1f W', [ fPowerAkku ] ) + #13#10 +
               Format('Grid Power = %.1f W', [ fPowerGrid ] ) + #13#10 +
               Format('Load Power = %.1f W', [ fPowerLoad ] ) + #13#10 +
               Format('PV Power = %.1f W', [ fPowerPV ] ) + #13#10 +

               Format('Relative Autonomy = %d %%', [ Round( Min(100, Max(0, fRelAutonomy))) ] ) + #13#10 +
               Format('Rel Sel Consumption = %d %%', [ Round( Min(100, Max(0, fRelSelfConsumption))) ] );
end;

{ TFroniusMeterRealTimeData.TFroniusMeterInfo }

function TFroniusMeterRealTimeData.TFroniusMeterInfo.ToString: string;
begin
     Result := 'Manufacturer = ' + fManufacturer + #13#10 +
               'Model = ' + fModel + #13#10 +
               'Enabled = ' + BoolToStr(fEnable, True) + #13#10 +
               'Serial = ' + fSerial + #13#10 +
               'TimeStamp = ' +  FormatDateTime('dd.mm.yyyy hh:nn:ss', fTimeStamp) + #13#10 +
               'Visible = ' + BoolToStr(fVisible, True ) + #13#10 + #13#10;

     Result := Result +
               Format('Current_AC_Phase_1 = %.1f' + #13#10, [fCurrent_AC_Phase_1]) +
               Format('Current_AC_Phase_2 = %.1f' + #13#10, [fCurrent_AC_Phase_2]) +
               Format('Current_AC_Phase_3 = %.1f' + #13#10, [fCurrent_AC_Phase_3]) +
               Format('Current_AC_Sum = %.1f' + #13#10, [fCurrent_AC_Sum]) + #13#10;

     Result := Result +
               Format('EnergyReactive_VArAC_Sum_Consumed = %.1f' + #13#10, [fEnergyReactive_VArAC_Sum_Consumed]) +
               Format('EnergyReactive_VArAC_Sum_Produced = %.1f' + #13#10, [fEnergyReactive_VArAC_Sum_Produced]) +
               Format('EnergyReal_WAC_Minus_Absolute = %.1f' + #13#10, [fEnergyReal_WAC_Minus_Absolute]) +
               Format('EnergyReal_WAC_Plus_Absolute = %.1f' + #13#10, [fEnergyReal_WAC_Plus_Absolute]) +
               Format('EnergyReal_WAC_Sum_Consumed = %.1f' + #13#10, [fEnergyReal_WAC_Sum_Consumed]) +
               Format('EnergyReal_WAC_Sum_Produced = %.1f' + #13#10, [fEnergyReal_WAC_Sum_Produced]) + #13#10;

     Result := Result +
               Format('Frequency_Phase_Average = %.1f' + #13#10, [fFrequency_Phase_Average]) +
               Format('Meter_Location_Current = %.1f' + #13#10, [fMeter_Location_Current]) + #13#10;

     Result := Result +
               Format('PowerApparent_S_Phase_1 = %.1f' + #13#10, [fPowerApparent_S_Phase_1]) +
               Format('PowerApparent_S_Phase_2 = %.1f' + #13#10, [fPowerApparent_S_Phase_2]) +
               Format('PowerApparent_S_Phase_3 = %.1f' + #13#10, [fPowerApparent_S_Phase_3]) +
               Format('PowerApparent_S_Sum = %.1f' + #13#10, [fPowerApparent_S_Sum]) + #13#10;

     Result := Result +
               Format('PowerFactor_Phase_1 = %.1f' + #13#10, [fPowerFactor_Phase_1]) +
               Format('PowerFactor_Phase_2 = %.1f' + #13#10, [fPowerFactor_Phase_2]) +
               Format('PowerFactor_Phase_3 = %.1f' + #13#10, [fPowerFactor_Phase_3]) +
               Format('PowerFactor_Sum = %.1f' + #13#10, [fPowerFactor_Sum]) + #13#10;

     Result := Result +
               Format('PowerReactive_Q_Phase_1 = %.1f' + #13#10, [fPowerReactive_Q_Phase_1]) +
               Format('PowerReactive_Q_Phase_2 = %.1f' + #13#10, [fPowerReactive_Q_Phase_2]) +
               Format('PowerReactive_Q_Phase_3 = %.1f' + #13#10, [fPowerReactive_Q_Phase_3]) +
               Format('PowerReactive_Q_Sum = %.1f' + #13#10, [fPowerReactive_Q_Sum]) + #13#10;

     Result := Result +
               Format('PowerReal_P_Phase_1 = %.1f' + #13#10, [fPowerReal_P_Phase_1]) +
               Format('PowerReal_P_Phase_2 = %.1f' + #13#10, [fPowerReal_P_Phase_2]) +
               Format('PowerReal_P_Phase_3 = %.1f' + #13#10, [fPowerReal_P_Phase_3]) +
               Format('PowerReal_P_Sum = %.1f' + #13#10, [fPowerReal_P_Sum]) + #13#10;

     Result := Result +
               Format('Voltage_AC_PhaseToPhase_12 = %.1f' + #13#10, [fVoltage_AC_PhaseToPhase_12]) +
               Format('Voltage_AC_PhaseToPhase_23 = %.1f' + #13#10, [fVoltage_AC_PhaseToPhase_23]) +
               Format('Voltage_AC_PhaseToPhase_31 = %.1f' + #13#10, [fVoltage_AC_PhaseToPhase_31]) +
               Format('Voltage_AC_Phase_1 = %.1f' + #13#10, [fVoltage_AC_Phase_1]) +
               Format('Voltage_AC_Phase_2 = %.1f' + #13#10, [fVoltage_AC_Phase_2]) +
               Format('Voltage_AC_Phase_3 = %.1f' + #13#10, [fVoltage_AC_Phase_3]);
end;

{ TFroniusOhmPilotRealtimeData }

function TFroniusOhmPilotRealtimeData.Init: boolean;
var jsonVal : TJSONValue;
begin
     Result := Get('/solar_api/v1/GetOhmpilotRealtimeData.cgi', jsonVal);

     jsonVal.Free;
end;

{ TFroniusStorageRealTimeData }

function TFroniusStorageRealTimeData.Init: boolean;
var jsonVal : TJSONValue;
begin
     Result := Get('/solar_api/v1/GetStorageRealtimeData.cgi', jsonVal);

     if Result then
     begin
          // whatever comes in here...
     end;

     jsonVal.Free;
end;

initialization
  TFroniusBase.Host := '';

end.
