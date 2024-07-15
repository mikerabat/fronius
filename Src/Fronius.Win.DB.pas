unit Fronius.Win.DB;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF FPC} System.SysUtils, System.Classes, {$ENDIF}
  IBX.IBDatabase, Data.DB, IBX.IBSQL,
  IBX.IBCustomDataSet, IBX.IBQuery, Fronius.DBIntf,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TdmFronius = class(TDataModule, IFroniusDBAdapter)
    FroniusDB: TIBDatabase;
    trFronius: TIBTransaction;
    sqlSample: TIBSQL;
    idFirebirdTest: TIdTCPClient;
  private
    fOnLog: TLogEvent;

    procedure LLog(level: integer; const msg: string);
    function TestFirebirdPort(timeout: integer): boolean;

  protected
    procedure SetSQLText( const sql : string );
    procedure SetParam( param : string; val : integer ); overload;
    procedure SetParamDT( param : string; val : TDateTime );
    procedure SetParam( param : string; val : double ); overload;
    procedure SetParam( param : string; val : string ); overload;

    function GetResDT( field : string; defValue : TDateTime ) : TDateTime; overload;
    function GetRes( field : string; defVAlue : string) : string; overload;
    function GetRes( field : string; defValue : double) : double; overload;
    function GetRes( field : string; defValue : integer) : integer; overload;

    function Eof : boolean;
    procedure Next;

    procedure ExecQuery;
    procedure Commit;
    procedure Rollback;
    procedure Close;

    function TickCnt : int64;
    function ConnectToDB(out ErrMsg : string; DBName : string = '') : boolean;
    procedure SetLogEvt( evt : TLogEvent );
    function IsConnected: boolean;
  public
    { Public-Deklarationen }
  end;


var dmFronius: TdmFronius;

implementation

uses Fronius.Consts, {$IFDEF FPC} LCLIntf, LCLType, LMessages, {$ENDIF} Math,
     Windows;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$IFDEF FPC}
{$R *.frm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TdmFronius.LLog(level: integer; const msg: string);
begin
     if Assigned(fOnLog) then
        fOnLog(self, level, msg);
end;


procedure TdmFronius.Next;
begin
     sqlSample.Next;
end;

procedure TdmFronius.Rollback;
begin
     trFronius.Rollback;
end;

function TdmFronius.ConnectToDB(out ErrMsg : string; DBName: string): boolean;
var fronConf : IFroniusConf;
begin
     ErrMsg := '';
     try
        froniusDB.Close;

   //     UnicodeMemoParameters:=True;
        FroniusDB.SQLDialect := 3;
        //Database.Options.Charset:='UTF8';
        //Database.Options.UseUnicode:=True;

        // ####################

        fronConf := FroniusConfig;
        if DBName<>''
        then
            FroniusDB.DatabaseName := DBName
        else
            // Get Database-Location from registry (Local machine)
            FroniusDB.DatabaseName := fronConf.DBName;

        LLog(8, 'Connecting to db ' + FroniusDB.DatabaseName);
        LLog(8, 'Testing firebird server avail...' );
        if fronConf.TestFirebirdServer then
        begin
             if not TestFirebirdPort(fronConf.TestTimeout) then
                raise Exception.Create('Failed to connect to the database.');
        end;

        FroniusDB.Params.Clear;
        FroniusDB.Params.Add('user_name=' + fronConf.DBUser);
        FroniusDB.Params.Add('password=' + fronConf.DBPwd);

        FroniusDB.Connected := True;
        OutputDebugString(PChar('Connected to ' + FroniusDB.DatabaseName));
        Result := FroniusDB.Connected;

        if Result then
           trFronius.StartTransaction;
     except
           on E : Exception do
           begin
                ErrMsg := E.Message;
                Result := False;
           end;
     end;
end;

procedure TdmFronius.SetSQLText( const sql : string );
begin
     if not trFronius.Active then
        trFronius.StartTransaction;

     sqlSample.SQL.Text := sql;
end;

procedure TdmFronius.SetParam( param : string; val : integer );
begin
     sqlSample.ParamByName(param).AsInteger := val;
end;

procedure TdmFronius.SetParam(param, val: string);
begin
     sqlSample.ParamByName(param).AsString := val;
end;

procedure TdmFronius.SetParamDT( param : string; val : TDateTime );
begin
     sqlSample.ParamByName(param).AsDateTime := val;
end;

procedure TdmFronius.SetParam( param : string; val : double );
begin
     sqlSample.ParamByName(param).AsFloat := val;
end;

function TdmFronius.Eof: boolean;
begin
     Result := sqlSample.Eof;
end;

procedure TdmFronius.ExecQuery;
begin
     sqlSample.ExecQuery;
end;

function TdmFronius.GetRes(field: string; defValue: double): double;
var idx : integer;
begin
     idx := sqlSample.FieldIndex[field];

     if (idx >= 0) and not sqlSample.Fields[idx].IsNull
     then
         Result := sqlSample.Fields[idx].AsDouble
     else
         Result := defValue;
end;

function TdmFronius.GetResDT(field: string; defValue: TDateTime): TDateTime;
var idx : integer;
begin
     idx := sqlSample.FieldIndex[field];

     if (idx >= 0) and not sqlSample.Fields[idx].IsNull
     then
         Result := sqlSample.Fields[idx].AsDateTime
     else
         Result := defValue;
end;

function TdmFronius.GetRes(field: string; defValue: integer): integer;
var idx : integer;
begin
     idx := sqlSample.FieldIndex[field];

     if (idx >= 0) and not sqlSample.Fields[idx].IsNull
     then
         Result := sqlSample.Fields[idx].AsInteger
     else
         Result := defValue;
end;

function TdmFronius.GetRes(field, defVAlue: string): string;
var idx : integer;
begin
     idx := sqlSample.FieldIndex[field];

     if (idx >= 0) and not sqlSample.Fields[idx].IsNull
     then
         Result := sqlSample.Fields[idx].AsString
     else
         Result := defValue;
end;


function TdmFronius.IsConnected: boolean;
begin
     Result := FroniusDB.Connected;
end;

procedure TdmFronius.Close;
begin
     sqlSample.Close;
end;

procedure TdmFronius.Commit;
begin
     trFronius.Commit;
end;

function TdmFronius.TickCnt : int64;
begin
     Result := GetTickCount64;
end;

procedure TdmFronius.SetLogEvt( evt : TLogEvent );
begin
     fOnLog := evt;
end;


function TdmFronius.TestFirebirdPort(timeout: integer): boolean;
var aHost, aPortSN : string;
    iPort : integer;

  procedure ParseDatabaseName( var aHost, aPort : string );
  var idx : integer;
  begin
       aHost := '127.0.0.1';
       aPort := '3050';

       idx := Pos(':', FroniusDB.DatabaseName );
       if idx <= 2 then
          exit;

       aHost := Copy(FroniusDB.DatabaseName, 1, idx);
       idx := Pos('/', aHost);

       if idx <= 0 then
          exit;

       aPort := Copy(aHost, idx + 1, Length(aHost));
       aHost := Copy(aHost, 1, idx - 1);
  end;
begin
     Result := True;

     LLog(1, 'Test firebird connection');

     // http://www.firebirdfaq.org/faq123/
     // -> they do a telnet on the designated port - we try to connect with
     // a shorter timeout
     idFirebirdTest.ConnectTimeout := timeout;
     idFirebirdTest.ReadTimeout := timeout;

     // same function as used in the TIBCDatabase component:
     ParseDatabaseName(aHost, aPortSN);

     try
        // if no host is given then use the defaults:
        if (aHost <> '') and TryStrToInt( aPortSN, iPort ) then
        begin
             idFirebirdTest.Host := aHost;
             idFirebirdTest.Port := Word(iPort);
             // ###########################################
             // #### Just try to connect and disconnect...
             idFirebirdTest.Connect;
             idFirebirdTest.Disconnect;
        end
        else
            LLog(8, 'Failed to convert port: ' + aHost + ':' + aPortSN);
     except
           on E: Exception do
           begin
                LLog(8, 'Failed to connect to ' + aHost + ':' + aPortSN);
                Result := False;
           end;
     end;
end;

end.
