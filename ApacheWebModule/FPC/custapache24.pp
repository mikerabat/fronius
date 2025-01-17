{
    This file is part of the Free Pascal fcl-web package
    Copyright (c) 1999-2022 by the Free Pascal development team

    Apache 2.4 customization

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit custapache24;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, FpWeb.Handler, FpWeb.Http.Defs, FpWeb.Http.Base, Api.Httpd24, Api.Httpd24.Apr, System.SyncObjs, FpWeb.Http.Protocol;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, CustWeb, httpDefs, fpHTTP, httpd24, apr24, SyncObjs, httpprotocol;
{$ENDIF FPC_DOTTEDUNITS}

Type

  TApacheHandler = Class;

  { TApacheRequest }

  TApacheRequest = Class(TRequest)
  Private
    FApache : TApacheHandler;
    FRequest : PRequest_rec;
  Protected
    function GetApacheHeaderValue(H: THeader): String;
    function GetApacheVariableValue(V: THTTPVariableType): String;
    Procedure InitFromRequest;
    procedure initrequestvars; override;
    procedure ReadContent; override;
  Public
    Constructor CreateReq(App : TApacheHandler; ARequest : PRequest_rec);
    Property ApacheRequest : Prequest_rec Read FRequest;
    Property ApacheApp : TApacheHandler Read FApache;
  end;

  { TApacheResponse }

  TApacheResponse = Class(TResponse)
  private
    FApache : TApacheHandler;
    FRequest : PRequest_rec;
    procedure SendStream(S: TStream);
  Protected
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
  Public
    Constructor CreateApache(Req : TApacheRequest);
    Property ApacheRequest : Prequest_rec Read FRequest;
    Property ApacheApp : TApacheHandler Read FApache;
  end;

  { TCustomApacheApplication }
  THandlerPriority = (hpFirst,hpMiddle,hpLast);
  TBeforeRequestEvent = Procedure(Sender : TObject; Const AHandler : String;
                                  Var AllowRequest : Boolean) of object;

  TApacheHandler = Class(TWebHandler)
  private
    FMaxRequests: Integer;             //Maximum number of simultaneous web module requests (default=64, if set to zero no limit)
    FWorkingWebModules: TList;         //List of currently running web modules handling requests
    FIdleWebModules: TList;            //List of idle web modules available
    FCriticalSection: TCriticalSection;
    FBaseLocation: String;
    FBeforeRequest: TBeforeRequestEvent;
    FHandlerName: String;
    FModuleName: String;
    FModules : Array[0..1] of TStrings;
    FPriority: THandlerPriority;
    FModuleRecord : PModule;
    function GetModules(Index: integer): TStrings;
    procedure SetModules(Index: integer; const AValue: TStrings);
    function GetIdleModuleCount : Integer;
    function GetWorkingModuleCount : Integer;
  Protected
    Function ProcessRequest(P : PRequest_Rec) : Integer; virtual;
    function WaitForRequest(out ARequest : TRequest; out AResponse : TResponse) : boolean; override;
    Function AllowRequest(P : PRequest_Rec) : Boolean; virtual;
    function GetApplicationURL(ARequest : TRequest): String; override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Run; override;
    Procedure SetModuleRecord(Var ModuleRecord : Module);
    Procedure Initialize;
    Procedure LogErrorMessage(const Msg : String; LogLevel : integer = APLOG_INFO); virtual;
    Procedure handleRequest(ARequest : TRequest; AResponse : TResponse); override;
    Property HandlerPriority : THandlerPriority Read FPriority Write FPriority default hpMiddle;
    Property BeforeModules : TStrings Index 0 Read GetModules Write SetModules;
    Property AfterModules : TStrings Index 1 Read GetModules Write SetModules;
    Property BaseLocation : String Read FBaseLocation Write FBaseLocation;
    Property ModuleName : String Read FModuleName Write FModuleName;
    Property HandlerName : String Read FHandlerName Write FHandlerName;
    Property BeforeRequest : TBeforeRequestEvent Read FBeforeRequest Write FBeforeRequest;
    Property MaxRequests: Integer read FMaxRequests write FMaxRequests;
    Property IdleWebModuleCount: Integer read GetIdleModuleCount;
    Property WorkingWebModuleCount: Integer read GetWorkingModuleCount;
  end;

  TCustomApacheApplication = Class(TCustomWebApplication)
  private
    function GetAfterModules: TStrings;
    function GetBaseLocation: String;
    function GetBeforeModules: TStrings;
    function GetBeforeRequest: TBeforeRequestEvent;
    function GetHandlerName: String;
    function GetIdleModuleCount: Integer;
    function GetMaxRequests: Integer;
    function GetModuleName: String;
    function GetPriority: THandlerPriority;
    function GetWorkingModuleCount: Integer;
    procedure SetAfterModules(const AValue: TStrings);
    procedure SetBaseLocation(const AValue: String);
    procedure SetBeforeModules(const AValue: TStrings);
    procedure SetBeforeRequest(const AValue: TBeforeRequestEvent);
    procedure SetHandlerName(const AValue: String);
    procedure SetMaxRequests(const AValue: Integer);
    procedure SetModuleName(const AValue: String);
    procedure SetPriority(const AValue: THandlerPriority);
  public
    function InitializeWebHandler: TWebHandler; override;
    Procedure Initialize;override;
    procedure ShowException(E: Exception); override;
    Function ProcessRequest(P : PRequest_Rec) : Integer; virtual;
    Function AllowRequest(P : PRequest_Rec) : Boolean; virtual;
    Procedure SetModuleRecord(Var ModuleRecord : Module);
    Property HandlerPriority : THandlerPriority Read GetPriority Write SetPriority default hpMiddle;
    Property BeforeModules : TStrings Read GetBeforeModules Write SetBeforeModules;
    Property AfterModules : TStrings Read GetAfterModules Write SetAfterModules;
    Property BaseLocation : String Read GetBaseLocation Write SetBaseLocation;
    Property ModuleName : String Read GetModuleName Write SetModuleName;
    Property HandlerName : String Read GetHandlerName Write SetHandlerName;
    Property BeforeRequest : TBeforeRequestEvent Read GetBeforeRequest Write SetBeforeRequest;
    Property MaxRequests: Integer read GetMaxRequests write SetMaxRequests;
    Property IdleWebModuleCount: Integer read GetIdleModuleCount;
    Property WorkingWebModuleCount: Integer read GetWorkingModuleCount;
  end;

  EFPApacheError = Class(EHTTP);

Var
  Application : TCustomApacheApplication = Nil;
  ShowCleanUpErrors : Boolean = False;
  AlternateHandler : ap_hook_handler_t = Nil;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Fcl.CustApp;
{$ELSE FPC_DOTTEDUNITS}
//uses CustApp;
{$ENDIF FPC_DOTTEDUNITS}

resourcestring
  SErrNoModuleNameForRequest = 'Could not determine HTTP module name for request';
  SErrNoModuleForRequest = 'Could not determine HTTP module for request "%s"';
  SErrNoModuleRecord = 'No module record location set.';
  SErrNoModuleName = 'No module name set';
  SErrTooManyRequests = 'Too many simultaneous requests.';


Function MaybeAnsi(S : String) : AnsiString; inline;

begin
{$IF SIZEOF(CHAR)=1}
  Result:=S;
{$ELSE}
  Result:=UTF8Encode(S);
{$ENDIF}
end;


const
  HPRIO : Array[THandlerPriority] of Integer
        = (APR_HOOK_FIRST,APR_HOOK_MIDDLE,APR_HOOK_LAST);



Function DefaultApacheHandler(P : PRequest_Rec) : integer;cdecl;

begin
  If (AlternateHandler<>Nil) then
    Result:=AlternateHandler(P)
  else
    If Application.AllowRequest(P) then
      Result:=Application.ProcessRequest(P)
    else
      Result:=DECLINED;
end;

Procedure RegisterApacheHooks(P: PApr_pool_t);cdecl;

Var
  H : ap_hook_handler_t;
  PP1,PP2 : PPAnsiChar;

begin
  H:=AlternateHandler;
  If (H=Nil) then
    H:=@DefaultApacheHandler;
  PP1:=Nil;
  PP2:=Nil;
  ap_hook_handler(H,PP1,PP2,HPRIO[Application.HandlerPriority]);
end;

{ TApacheHandler }

function TApacheHandler.GetModules(Index: integer): TStrings;
begin
  If (FModules[Index]=Nil) then
    FModules[Index]:=TStringList.Create;
  Result:=FModules[Index];
end;

procedure TApacheHandler.SetModules(Index: integer;
  const AValue: TStrings);
begin
  If (FModules[Index]=Nil) then
    FModules[Index]:=TStringList.Create;
  FModules[Index].Assign(AValue);
end;

Function TApacheHandler.ProcessRequest(P: PRequest_Rec) : Integer;

Var
  Req : TApacheRequest;
  Resp : TApacheResponse;

begin
  Req:=TApacheRequest.CreateReq(Self,P);
  Try
    InitRequest(Req);
    //Req.InitRequestVars;
    Req.InitFromRequest;   // content is not parsed otherwise...
    Resp:=TApacheResponse.CreateApache(Req);
    Try
      InitResponse(Resp);
      HandleRequest(Req,Resp);
      If Not Resp.ContentSent then
        Resp.SendContent;
    Finally
      Result:=OK;
      Resp.Free;
    end;
  Finally
    Req.Free;
  end;
end;

procedure TApacheHandler.Run;
begin
  // Do nothing. This is a library
  Initialize;
end;

function TApacheHandler.WaitForRequest(out ARequest: TRequest; out AResponse: TResponse): boolean;
begin
  Result:=False;
  ARequest:=Nil;
  AResponse:=Nil;
end;

function TApacheHandler.AllowRequest(P: PRequest_Rec): Boolean;

Var
  Hn : String;

begin
  HN:=StrPas(p^.Handler);
  Result:=CompareText(HN,FHandlerName)=0;
  If Assigned(FBeforeRequest) then
    FBeforeRequest(Self,HN,Result);
end;

function TApacheHandler.GetApplicationURL(ARequest: TRequest): String;
begin
  Result:=inherited GetApplicationURL(ARequest);
  If (Result='') then
    Result:=BaseLocation;
end;

constructor TApacheHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPriority:=hpMiddle;
  FMaxRequests:=64;
  FWorkingWebModules:=TList.Create;
  FIdleWebModules:=TList.Create;
  FCriticalSection:=TCriticalSection.Create;
end;

destructor TApacheHandler.Destroy;
var I:Integer;
begin
  FCriticalSection.Free;
  for I := FIdleWebModules.Count - 1 downto 0 do
    TComponent(FIdleWebModules[I]).Free;
  FIdleWebModules.Free;
  for I := FWorkingWebModules.Count - 1 downto 0 do
    TComponent(FWorkingWebModules[I]).Free;
  FWorkingWebModules.Free;
  inherited Destroy;
end;


procedure TApacheHandler.SetModuleRecord(var ModuleRecord: Module);
begin
  FModuleRecord:=@ModuleRecord;
  FillChar(ModuleRecord,SizeOf(ModuleRecord),0);
end;

procedure TApacheHandler.Initialize;

begin
  If (FModuleRecord=nil) then
    Raise EFPApacheError.Create(SErrNoModuleRecord);
  if (FModuleName='') and (FModuleRecord^.Name=Nil) then
    Raise EFPApacheError.Create(SErrNoModuleName);
  STANDARD20_MODULE_STUFF(FModuleRecord^);
  If (StrPas(FModuleRecord^.name)<>FModuleName) then
    FModuleRecord^.Name:=PAnsiChar(FModuleName);
  FModuleRecord^.register_hooks:=@RegisterApacheHooks;
end;

procedure TApacheHandler.LogErrorMessage(const Msg: String; LogLevel: integer);
begin
  ap_log_error(PAnsiChar(FModuleName),  //The file in which this function is called
               0,                   //The line number on which this function is called
               0,                   //The module_index of the module generating this message
               LogLevel,            //The level of this error message
               0,                   //The status code from the previous command
               Nil,                 //The server on which we are logging
               'module: %s',        //The format string
               [PAnsiChar(Msg)])        //The arguments to use to fill out fmt.
end;

function TApacheHandler.GetIdleModuleCount : Integer;
begin
  FCriticalSection.Enter;
  try
    Result := FIdleWebModules.Count;
  finally
    FCriticalSection.Leave;
  end;
end;

function TApacheHandler.GetWorkingModuleCount : Integer;
begin
  FCriticalSection.Enter;
  try
    Result := FWorkingWebModules.Count;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TApacheHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  MC : TCustomHTTPModuleClass;
  M  : TCustomHTTPModule;
  MN : String;
  MI : TModuleItem;

  Procedure GetAWebModule;
  Var II:Integer;
  begin
    FCriticalSection.Enter;
    try
      if (FMaxRequests>0) and (FWorkingWebModules.Count>=FMaxRequests) then
        Raise EFPApacheError.Create(SErrTooManyRequests);
      if (FIdleWebModules.Count>0) then
      begin
        II := FIdleWebModules.Count - 1;
        while (II>=0) and not (TComponent(FIdleWebModules[II]) is MC) do
          Dec(II);
        if (II>=0) then
        begin
          M:=TCustomHTTPModule(FIdleWebModules[II]);
          FIdleWebModules.Delete(II);
        end;
      end;
      if (M=nil) then
      begin
        M:=MC.Create(Self);
        M.Name := '';
      end;
      FWorkingWebModules.Add(M);
    finally
      FCriticalSection.Leave;
    end;
  end;

begin
  try
    MC:=Nil;
    M := Nil;
    If (OnGetModule<>Nil) then
      OnGetModule(Self,ARequest,MC);
    If (MC=Nil) then
    begin
      MN:=GetModuleName(ARequest);
      If (MN='') and Not AllowDefaultModule then
        Raise EFPApacheError.Create(SErrNoModuleNameForRequest);
      MI:=ModuleFactory.FindModule(MN);
      If (MI=Nil) and (ModuleFactory.Count=1) then
        MI:=ModuleFactory[0];
      if (MI=Nil) then
        Raise EFPApacheError.CreateFmt(SErrNoModuleForRequest,[MN]);

      MC:=MI.ModuleClass;
    end;
    GetAWebModule;
    M.HandleRequest(ARequest,AResponse);

    FCriticalSection.Enter;
    try
      FWorkingWebModules.Remove(M);
      FIdleWebModules.Add(M);
    finally
      FCriticalSection.Leave;
    end;
  except
    On E : Exception do
      begin
      LogErrorMessage(E.Message,APLOG_ERR);
      ShowRequestException(AResponse,E);
      end;
  end;
end;

{ TApacheRequest }

Function MaybeP(P : PAnsiChar) : String;

begin
  If (P<>Nil) then
    Result:=StrPas(P);
end;

function TApacheRequest.GetApacheVariableValue(V: THTTPVariableType): String;

var
  i : integer;

begin
  Result:='';
  if not Assigned(FRequest) then
    exit;
  case V of
    hvHTTPVersion:
      Result:=MaybeP(FRequest^.protocol); // ProtocolVersion
    hvPathInfo:
      Result:=MaybeP(FRequest^.path_info); // PathInfo
    hvPathTranslated:
      Result:=MaybeP(FRequest^.filename); // PathTranslated
    hvRemoteAddress :
      If (FRequest^.Connection<>Nil) then
        Result:=MaybeP(FRequest^.Connection^.remote_ip);
    hvRemoteHost:
      If (FRequest^.Connection<>Nil) then
        begin
        Result:=MaybeP(ap_get_remote_host(FRequest^.Connection,
                       FRequest^.per_dir_config,
//                     nil,
                       REMOTE_NAME,@i));
        end;
    hvScriptName:
      begin // ScriptName
      Result:=MaybeP(FRequest^.unparsed_uri);
      I:=Pos('?',Result)-1;
      If (I=-1) then
       I:=Length(Result);
      Result:=Copy(Result,1,I-Length(PathInfo));
      end;
    hvServerPort:
      Result:=IntToStr(ap_get_server_port(FRequest)); // ServerPort
    hvMethod:
      Result:=MaybeP(FRequest^.method); // Method
    hvURL:
      Result:=MaybeP(FRequest^.unparsed_uri); // URL
    hvQuery:
      Result:=MaybeP(FRequest^.args); // Query
    end;
end;

function TApacheRequest.GetApacheHeaderValue(H: THeader): String;

var
  FN : AnsiString;
  S : String;

begin
  Result:='';
  Str(H,S);
  If Not Assigned(FRequest) then
    exit;
  Case h of
    hhContentEncoding:
      Result:=MaybeP(FRequest^.content_encoding);
    hhHost:
      Result:=MaybeP(FRequest^.HostName);
  else
    FN:=MaybeAnsi(HeaderName(H));
    Result:=MaybeP(apr_table_get(FRequest^.headers_in,PAnsiChar(FN)));
  end;
end;


procedure TApacheRequest.ReadContent;

  Function MinS(A,B : Integer) : Integer;

  begin
    If A<B then
      Result:=A
    else
      Result:=B;
  end;

Var
  Left,Len,Count,Bytes : Integer;
  P : PAnsiChar;
  S : String;

begin
  ap_setup_client_block(FRequest,REQUEST_CHUNKED_DECHUNK);
  If (ap_should_client_block(FRequest)=1) then
    begin
    Len:=ContentLength;
    If (Len>0) then
      begin
      SetLength(S,Len);
      P:=PAnsiChar(S);
      Left:=Len;
      Count:=0;
      Repeat
        Bytes:=ap_get_client_block(FRequest,P,MinS(10*1024,Left));
        Dec(Left,Bytes);
        Inc(P,Bytes);
        Inc(Count,Bytes);
      Until (Count>=Len) or (Bytes=0);
      SetLength(S,Count);
      end;
    end;
  InitContent(S);
end;


procedure TApacheRequest.InitFromRequest;
begin
  Initrequestvars;
  Cookie := MaybeP(apr_table_get(FRequest^.headers_in,PAnsiChar(HeaderCookie)));
  ParseCookies;
  ReadContent;
end;

procedure TApacheRequest.initrequestvars;

Var
  H : THeader;
  V : THTTPVariableType;
  S : String;

begin
  For H in Theader do
    if hdRequest in HTTPHeaderDirections[H] then
      begin
      S:=GetApacheHeaderValue(H);
      if S<>'' then
        SetHeader(H,S);
      end;
  For V in THTTPVariableType do
    begin
    S:=GetApacheVariableValue(V);
    if S<>'' then
      SetHTTPVariable(V,S);
    end;
  inherited initrequestvars;
end;

constructor TApacheRequest.CreateReq(App: TApacheHandler; ARequest: PRequest_rec
  );

begin
  FApache:=App;
  FRequest:=Arequest;
  ReturnedPathInfo:=App.BaseLocation;
  Inherited Create;
end;

{ TApacheResponse }

procedure TApacheResponse.DoSendHeaders(Headers: TStrings);

Var
  I,P : Integer;
  N,V : AnsiString;

begin
  For I:=0 to Headers.Count-1 do
    begin
    V:=Headers[i];
    P:=Pos(':',V);
    If (P<>0) and (P<Length(V)) then
      begin
      N:=Copy(V,1,P-1);
      System.Delete(V,1,P);
      V := Trim(V);//no need space before the value, apache puts it there
      apr_table_set(FRequest^.headers_out,PAnsiChar(N),PAnsiChar(V));
      end;
    end;
end;


procedure TApacheResponse.DoSendContent;

Var
  S : AnsiString;

begin
  S:=MaybeAnsi(ContentType);
  If (S<>'') then
    FRequest^.content_type:=apr_pstrdup(FRequest^.pool,PAnsiChar(S));
  S:=MaybeAnsi(ContentEncoding);
  If (S<>'') then
    FRequest^.content_encoding:=apr_pstrdup(FRequest^.pool,PAnsiChar(S));
  If Code <> 200 then
    FRequest^.status := Code;
  If assigned(ContentStream) then
    SendStream(Contentstream)
  else
    begin
    S:=Content;
    ap_rwrite(PAnsiChar(S),Length(S),FRequest);
    end;
end;

Procedure TApacheResponse.SendStream(S : TStream);

Var
  Buf : Array[0..(10*1024)-1] of Byte;
  Count : Integer;

begin
  S.Seek(0,soBeginning);
  Repeat
    Count:=S.Read(Buf,SizeOf(Buf));
    If Count>0 then
      ap_rwrite(@Buf,Count,FRequest);
  Until (Count=0);
end;


Constructor TApacheResponse.CreateApache(Req : TApacheRequest);
begin
  FApache:=Req.ApacheApp;
  Frequest:=Req.ApacheRequest;
  Inherited Create(Req);
end;

function __dummythread(p: pointer): ptrint;
begin
  sleep(1000);
  Result:=0;
end;

{ TCustomApacheApplication }

function TCustomApacheApplication.GetAfterModules: TStrings;
begin
  result := TApacheHandler(WebHandler).AfterModules;
end;

function TCustomApacheApplication.GetBaseLocation: String;
begin
  result := TApacheHandler(WebHandler).BaseLocation;
end;

function TCustomApacheApplication.GetBeforeModules: TStrings;
begin
  result := TApacheHandler(WebHandler).BeforeModules;
end;

function TCustomApacheApplication.GetBeforeRequest: TBeforeRequestEvent;
begin
  result := TApacheHandler(WebHandler).BeforeRequest;
end;

function TCustomApacheApplication.GetHandlerName: String;
begin
  result := TApacheHandler(WebHandler).HandlerName;
end;

function TCustomApacheApplication.GetIdleModuleCount: Integer;
begin
  result := TApacheHandler(WebHandler).IdleWebModuleCount;
end;

function TCustomApacheApplication.GetMaxRequests: Integer;
begin
  result := TApacheHandler(WebHandler).MaxRequests;
end;

function TCustomApacheApplication.GetModuleName: String;
begin
  result := TApacheHandler(WebHandler).ModuleName;
end;

function TCustomApacheApplication.GetPriority: THandlerPriority;
begin
  result := TApacheHandler(WebHandler).HandlerPriority;
end;

function TCustomApacheApplication.GetWorkingModuleCount: Integer;
begin
  result := TApacheHandler(WebHandler).WorkingWebModuleCount;
end;

procedure TCustomApacheApplication.SetAfterModules(const AValue: TStrings);
begin
  TApacheHandler(WebHandler).AfterModules := AValue;
end;

procedure TCustomApacheApplication.SetBaseLocation(const AValue: String);
begin
  TApacheHandler(WebHandler).BaseLocation := AValue;
end;

procedure TCustomApacheApplication.SetBeforeModules(const AValue: TStrings);
begin
  TApacheHandler(WebHandler).BeforeModules := AValue;
end;

procedure TCustomApacheApplication.SetBeforeRequest(const AValue: TBeforeRequestEvent);
begin
  TApacheHandler(WebHandler).BeforeRequest := AValue;
end;

procedure TCustomApacheApplication.SetHandlerName(const AValue: String);
begin
  TApacheHandler(WebHandler).HandlerName := AValue;
end;

procedure TCustomApacheApplication.SetMaxRequests(const AValue: Integer);
begin
  TApacheHandler(WebHandler).MaxRequests := AValue;
end;

procedure TCustomApacheApplication.SetModuleName(const AValue: String);
begin
  TApacheHandler(WebHandler).ModuleName := AValue;
end;

procedure TCustomApacheApplication.SetPriority(const AValue: THandlerPriority);
begin
  TApacheHandler(WebHandler).HandlerPriority := AValue;
end;

function TCustomApacheApplication.InitializeWebHandler: TWebHandler;
begin
  Result:=TApacheHandler.Create(self);
end;

procedure TCustomApacheApplication.Initialize;
begin
  Inherited;
  TApacheHandler(WebHandler).Initialize;
end;

procedure TCustomApacheApplication.ShowException(E: Exception);
begin
  ap_log_error(PAnsiChar(TApacheHandler(WebHandler).ModuleName),  //The file in which this function is called
               0,                                             //The line number on which this function is called
               0,                                             //The module_index of the module generating this message
               APLOG_ERR,                                     //The level of this error message
               0,                                             //The status code from the previous command
               Nil,                                           //The server on which we are logging
               'module: %s',                                  //The format string
               [PAnsiChar(E.Message)]);                           //The arguments to use to fill out fmt.
end;

function TCustomApacheApplication.ProcessRequest(P: PRequest_Rec): Integer;
begin
  result := TApacheHandler(WebHandler).ProcessRequest(p);
end;

function TCustomApacheApplication.AllowRequest(P: PRequest_Rec): Boolean;
begin
  result := TApacheHandler(WebHandler).AllowRequest(p);
end;

procedure TCustomApacheApplication.SetModuleRecord(var ModuleRecord: Module);
begin
  TApacheHandler(WebHandler).SetModuleRecord(ModuleRecord);
end;

Initialization
  BeginThread(@__dummythread);//crash prevention for simultaneous requests
end.

