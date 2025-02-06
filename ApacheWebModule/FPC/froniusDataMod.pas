unit froniusDataMod;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, IBDatabase, IBQuery, httpdefs, fpHTTP, fpWeb,
  Fronius.Consts, Fronius.DBIntf, Fronius.WebAuthnModuleInit,
  Fronius.WebDataHandler, Fronius.Sessions;

type
  { TwmFronius }
  TwmFronius = class(TFPWebModule, IFroniusDBAdapter)
    FroniusDB: TIBDatabase;
    sqlSample: TIBQuery;
    trFronius: TIBTransaction;
    procedure DataModuleBeforeRequest(Sender: TObject; ARequest: TRequest);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure waDefaultRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    fLogEvt: TLogEvent;
    fWebauthn : TWebAuthnPathHandler;
    fDataHandler : TFroniusDataPathHandler;

  protected
    // db adapter routines:
    procedure SetSQLText( const sql : string );
    procedure SetParam( param : string; val : integer ); overload;
    procedure SetParamDT( param : string; val : TDateTime );
    procedure SetParam( param : string; val : double ); overload;
    procedure SetParam( param : string; val : string ); overload;

    function GetResDT( field : string; defValue : TDateTime ) : TDateTime; overload;
    function GetRes( field : string; defValue : string) : string; overload;
    function GetRes( field : string; defValue : double) : double; overload;
    function GetRes( field : String; defValue : integer) : integer; overload;

    function Eof : boolean;
    procedure Next;
    procedure ExecQuery;
    procedure Commit;
    procedure Rollback;
    procedure Close;

    function TickCnt : int64;

    procedure SetLogEvt( evt : TLogEvent );
    function ConnectToDB(out ErrMsg : string; DBName : string = '') : boolean;

    function IsConnected: boolean;

  public

  end;

var
  wmFronius: TwmFronius;

implementation

uses Fronius.Globals, db;

{$R *.frm}

// ###########################################
// #### DB interface
// ###########################################

{$Region 'DB Interface'}

function TwmFronius.Eof: boolean;
begin
     Result := sqlSample.Eof;
end;

procedure TwmFronius.ExecQuery;
begin
     if Pos('select', LowerCase(sqlSample.SQL.Text)) = 1
     then
         sqlSample.Open
     else
         sqlSample.ExecSQL;
end;

function TwmFronius.IsConnected: boolean;
begin
     Result := FroniusDB.Connected;
end;

function TwmFronius.ConnectToDB(out ErrMsg: string; DBName: string): boolean;
begin
     // already connected in create
     Result := True;
end;

procedure TwmFronius.Close;
begin
     sqlSample.Close;
end;

procedure TwmFronius.Commit;
begin
     trFronius.Commit;
end;

function TwmFronius.GetRes(field: string; defValue: double): double;
var aField : TField;
begin
     aField := sqlSample.FieldByName(field);

     if (afield <> nil) and (not aField.IsNull)
     then
         Result := aField.AsFloat
     else
         Result := defValue;
end;

function TwmFronius.GetResDT(field: string; defValue: TDateTime): TDateTime;
var aField : TField;
begin
     afield := sqlSample.FieldByName(field);

     if (afield <> nil) and (not aField.IsNull)
     then
         Result := aField.AsDateTime
     else
         Result := defValue;
end;

function TwmFronius.GetRes(field: string; defValue: string): string;
var aField : TField;
begin
     afield := sqlSample.FieldByName(field);

     if (afield <> nil) and (not aField.IsNull)
     then
         Result := aField.AsString
     else
         Result := defValue;
end;

procedure TwmFronius.Next;
begin
     sqlSample.Next;
end;

procedure TwmFronius.Rollback;
begin
     trFronius.Rollback;
end;

procedure TwmFronius.SetLogEvt(evt: TLogEvent);
begin
     fLogEvt := evt;
end;

procedure TwmFronius.SetParam(param : string; val: string);
begin
     sqlSample.ParamByName(param).AsString := val;
end;

procedure TwmFronius.SetParam(param: string; val: double);
begin
     sqlSample.ParamByName(param).AsFloat := val;
end;

procedure TwmFronius.SetParam(param: string; val: integer);
begin
     sqlSample.ParamByName(param).AsInteger := val;
end;

procedure TwmFronius.SetParamDT(param: string; val: TDateTime);
begin
     sqlSample.ParamByName(param).AsDateTime := val;
end;

procedure TwmFronius.DataModuleCreate(Sender: TObject);
begin
     // ###########################################
     // #### Initialize database
     FroniusDB.SQLDialect := 3;

     // Get Database-Location from registry (Local machine)
     FroniusDB.DatabaseName := froniusConf.DBName;
     FroniusDB.FirebirdLibraryPathName := froniusConf.FirebirdLibraryPathName;

     if firebirdConnected then
     begin
          FroniusDB.Params.Clear;
          FroniusDB.Params.Add('user_name=' + froniusConf.DBUser);
          FroniusDB.Params.Add('password=' + froniusConf.DBPwd);
          FroniusDB.Connected := True;
     end;

     // create the complete webauthn and session cookie handler:
     fWebauthn := TWebAuthnPathHandler.Create(self, self);
     fDataHandler := TFroniusDataPathHandler.Create(self, self);

     // this only works in cgi:
     InitSessionList( self );
end;

procedure TwmFronius.DataModuleBeforeRequest(Sender: TObject; ARequest: TRequest
  );
begin
     if Assigned(fLogEvt) then
        fLogEvt(self, 0, aRequest.PathInfo);
end;

procedure TwmFronius.DataModuleDestroy(Sender: TObject);
begin
     FroniusDB.Connected := False;

     fWebauthn.Free;
     fDataHandler.Free;
end;

procedure TwmFronius.waDefaultRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
     Response.Content :=
    '<html>' +
    '<head><title>Fronius - Solar App</title></head>' +
    '<body>Fronius solar app</body>' +
    '</html>';

     handled := True;
end;

procedure TwmFronius.SetSQLText(const sql: string);
begin
     if not trFronius.InTransaction then
        trFronius.StartTransaction;

     sqlSample.SQL.Text := sql;
end;

function TwmFronius.TickCnt: int64;
begin
     Result := GetTickCount64;
end;

function TwmFronius.GetRes(field: String; defValue: integer): integer;
var aField : TField;
begin
     afield := sqlSample.FieldByName(field);

     if (afield <> nil) and (not aField.IsNull)
     then
         Result := aField.AsInteger
     else
         Result := defValue;
end;

{$endRegion}

initialization
  RegisterHTTPModule('TwmFronius', TwmFronius);
end.

