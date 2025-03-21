// ###########################################
// #### Constants used around the projects
// ###########################################
unit Fronius.Consts;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils;

// ###########################################
// #### Database
const {$IFDEF MSWINDOWS}
      cSectFronius =  '\Software\mrSoft\Fronius';
      cDefFroniusLog = 'fronius.log';
      {$ELSE}
      cDefFroniusConf = '/etc/froniusd.conf';
      cDefFroniusLog = '/var/log/fronius.log';
      {$ENDIF}
      cDefFroniusDB = 'fronius.fdb';

      cFroniusKeepDaySamples = 3; // keep 3 days
      cFroniusSessIdLen = 16; // 16 bytes of random numbers


type
  TLogEvent = procedure(Sender : TObject; level : integer; const msg : string) of Object;
  IFroniusConf = interface
    ['{3BE43D9E-FD05-444D-8BF4-CB510D85782B}']
    function DBName : string;
    function DBUser : String;
    function DBPwd : string;
    function TestFirebirdServer : boolean;
    function TestTimeout : integer;
    function FirebirdLibraryPathName : string;

    function DBNumDaysToKeep : integer;
    function PowerSampleInterval : integer;
    function MeterSampleInterval : integer;
    function SampStoreInterval : integer;
    function FroniusHost : string;
    function HTTPHost : string;
    function LogLevel : integer;
    function LogFile : string;
    function GetExpirePeriod : double;
    function GetExpireCheckPeriod : integer;
    function ResidentKey : boolean;
    function AllowMultipleCredPerUser : boolean;
  end;

function FroniusConfig : IFroniusConf;

procedure InitConfig( dbName : string; dbUser, dbPwd : string; fn : string; logfn : string; fbLib : string; froniusHost : string; httpHost : string);

implementation

uses {$IFDEF LINUX} inifiles {$ELSE}Windows, Registry{$ENDIF};

type
  { TFroniusConf }
  TFroniusConf = class(TInterfacedObject, IFroniusConf)
  private
    fDBName : string;
    fDBUser : string;
    fDBPwd : string;
    fFBLib : string;
    fNumDaysToKeep : integer;
    fFroniusHost : string;
    fLogLevel : integer;
    fTestFirebirdServer : boolean;
    fTestFirebirdTimeout : integer;
    fPowerSampleInterval : integer;
    fMeterSampleInterval : integer;
    fSampStoreInterval : integer;
    fExpirePeriod : double;
    fExpireCheckPeriod : integer;
    fLogFile : string;
    fHTTPHost : string;
    fResidentKey : boolean;
    fAllowMultipleCredPerUser : boolean;
  protected
    function DBName : string;
    function DBUser : String;
    function DBPwd : string;
    function FirebirdLibraryPathName : string;
    function TestFirebirdServer : boolean;
    function TestTimeout : integer;
    function PowerSampleInterval : integer;
    function MeterSampleInterval : integer;
    function SampStoreInterval : integer;
    function GetExpirePeriod : double;
    function GetExpireCheckPeriod : integer;
    function ResidentKey : boolean;
    function AllowMultipleCredPerUser : boolean;

    function FroniusHost : string;
    function HTTPHost : string;
    function DBNumDaysToKeep : integer;
    function LogLevel : integer;
    function LogFile : string;
  public
    constructor Create;
  end;

function FroniusConfig : IFroniusConf;
begin
     Result := TFroniusConf.Create;
end;


{ TFroniusConf }

function TFroniusConf.AllowMultipleCredPerUser: boolean;
begin
     Result := fAllowMultipleCredPerUser;
end;

constructor TFroniusConf.Create;
begin
     // ###########################################
     // #### Initialize the object
     {$IFDEF MSWINDOWS}
     with TRegIniFile.Create(KEY_READ or KEY_WOW64_64KEY) do
     {$ELSE}
     with TIniFile.Create( cDefFroniusConf ) do
     {$ENDIF}
     try
        {$IFDEF MSWINDOWS}
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKey(cSectFronius, True);
        {$ENDIF}

        fLogLevel := ReadInteger('Log', 'LogLevel', 9);
        fLogFile := ReadString('Log', 'LogFile', cDefFroniusLog);
        fFroniusHost := ReadString('Net', 'Host', '');
        fHTTPHost := ReadString('Net', 'HTTPHost', '');

        fDBName := ReadString('DB', 'DBName', cDefFroniusDB);
        fDBUser := ReadString('DB', 'DBUser', 'sysdba');
        fDBPwd := ReadString('DB', 'DBPwd', 'masterkey');
        fFBLib := ReadString('DB', 'FBLibPathName', '');
        fTestFirebirdServer := ReadBool('DB', 'TestFirebirdServer', True );
        fTestFirebirdTimeout := ReadInteger('DB', 'FirebirdTimeout', 3000);

        fSampStoreInterval := ReadInteger('PwrCfg', 'SampInterval', 1*60*1000); // 1min min/max
        fPowerSampleInterval := ReadInteger('PwrCfg', 'PowerSampleInterval', 2000); // 2 seconds for integration
        fMeterSampleInterval := ReadInteger('PwrCfg', 'MeterSampleInterval', 1800); // every 30 minutes

        fNumDaysToKeep := ReadInteger('DB', 'NumDaysToKeep', cFroniusKeepDaySamples);

        fExpirePeriod := ReadInteger('Session', 'SessionPeriod', 3000)/100; // in 0.01 days, 100 is one day...
        fExpireCheckPeriod := ReadInteger('Session', 'SessionCheckPeriod', 10000); // every ten seconds...
        fAllowMultipleCredPerUser := ReadBool('Session', 'AllowMultipleCredPerUser', True);
     finally
            Free;
     end;

     inherited Create;
end;

function TFroniusConf.DBName: string;
begin
     Result := fDBName;
end;

function TFroniusConf.DBNumDaysToKeep: integer;
begin
     Result := fNumDaysToKeep;
end;

function TFroniusConf.DBPwd: string;
begin
     Result := fDBPwd;
end;

function TFroniusConf.FirebirdLibraryPathName: string;
begin
     Result := fFBLib;
end;

function TFroniusConf.DBUser: String;
begin
     Result := fDBUser;
end;

function TFroniusConf.FroniusHost: string;
begin
     Result := fFroniusHost;
end;

function TFroniusConf.GetExpireCheckPeriod: integer;
begin
     Result := fExpireCheckPeriod;
end;

function TFroniusConf.GetExpirePeriod: double;
begin
     Result := fExpirePeriod;
end;

function TFroniusConf.HTTPHost: string;
begin
     REsult := fHTTPHost;
end;

function TFroniusConf.LogLevel: integer;
begin
     Result := fLogLevel;
end;

function TFroniusConf.MeterSampleInterval: integer;
begin
     Result := fMeterSampleInterval;
end;

function TFroniusConf.LogFile: string;
begin
     Result := fLogFile;
end;

function TFroniusConf.PowerSampleInterval: integer;
begin
     Result := fPowerSampleInterval;
end;

function TFroniusConf.ResidentKey: boolean;
begin
     Result := fResidentKey;
end;

function TFroniusConf.SampStoreInterval: integer;
begin
     Result := fSampStoreInterval;
end;

function TFroniusConf.TestFirebirdServer: boolean;
begin
     Result := fTestFirebirdServer;
end;

function TFroniusConf.TestTimeout: integer;
begin
     Result := fTestFirebirdTimeout;
end;

// ################################################
// ##### create the default config according to some params
// ################################################

// basically a copy of the init values
procedure InitConfig( dbName : string; dbUser, dbPwd : string; fn : string;
    logfn : string; fbLib : string; froniusHost : string; httpHost : string);
begin
     // ###########################################
     // #### Initialize the object
     {$IFDEF MSWINDOWS}
     with TRegIniFile.Create(KEY_READ or KEY_WOW64_64KEY) do
     {$ELSE}
     with TIniFile.Create( fn ) do
     {$ENDIF}
     try
        {$IFDEF MSWINDOWS}
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKey(cSectFronius, True);
        {$ENDIF}


        WriteInteger('Log', 'LogLevel', 9);
        WriteString('Log', 'LogFile', logfn);
        WriteString('Net', 'Host', froniusHost);
        WriteString('Net', 'HTTPHost', httpHost);

        WriteString('DB', 'DBName', dbName);
        WriteString('DB', 'DBUser', dbUser);
        WriteString('DB', 'DBPwd', dbPwd);
        WriteString('DB', 'FBLibPathName', fbLib);

        WriteBool('DB', 'TestFirebirdServer', True );
        WriteInteger('DB', 'FirebirdTimeout', 3000);

        WriteInteger('PwrCfg', 'SampleInterval', 5*60*1000); // 5 minutes min/max
        WriteInteger('PwrCfg', 'PowerSampleInterval', 2000); // 2 seconds
        WriteInteger('PwrCfg', 'MeterSampleInterval', 1800); // every 30 minutes

        WriteInteger('DB', 'NumDaysToKeep', cFroniusKeepDaySamples);

        WriteInteger('Session', 'SessionPeriod', 3000);
        WriteInteger('Session', 'SessionCheckPeriod', 10000);

        WriteBool('Session', 'ResidentKey', False);
        WriteBool('Session', 'AllowMultipleCredPerUser', True);
     finally
            Free;
     end;
end;

end.
