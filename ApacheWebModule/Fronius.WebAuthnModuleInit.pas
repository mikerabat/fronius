unit Fronius.WebAuthnModuleInit;

// ###########################################
// #### Initialization routin for the default webauthn handlers
// ###########################################

interface

uses SysUtils, Classes, Fronius.ActionsBase,
     {$IFDEF FPC} fpWeb, HTTPDefs {$ELSE} Web.HTTPApp {$ENDIF},
     Fronius.DBIntf, Fronius.Webauthn;

// ###########################################
// #### Initializes the webmodule - basically adding the paths...
type
  TWebAuthnPathHandler = class(TWebActionBase)
  private
    fWebAuthnHandler : TFroniusWebauthHandler;
    fDB : IFroniusDBAdapter;

    procedure HandleSettings( Request: TWebRequest; Response: TWebResponse);
    procedure HandleUserExists( Request: TWebRequest; Response: TWebResponse);
    procedure HandleStartEnroll( Request: TWebRequest; Response: TWebResponse);
    procedure HandleVerifyEnroll( Request: TWebRequest; Response: TWebResponse);
    procedure HandleStartAssert( Request: TWebRequest; Response: TWebResponse);
    procedure HandleVerifyAssert( Request: TWebRequest; Response: TWebResponse);

    procedure HandleLogout( Request: TWebRequest; Response: TWebResponse);
    procedure HandleCheckSession( Request: TWebRequest; Response: TWebResponse);
    procedure CreateSessionCookie( remoteAddr : string; Response : TWebResponse; uName : string );
  protected
    procedure Handler(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  public
    constructor Create( ref : {$IFDEF FPC} TFPWebModule {$ELSE} TWebModule {$ENDIF}; dbHandler : IFroniusDBAdapter );
    destructor Destroy; override;
  end;

implementation

uses {$IFDEF FPC} httpprotocol, (*custapache24,*) {$ENDIF}
     Fronius.Sessions, Fronius.Globals, WebauthnHandler;

{ TWebAuthnHandler }

type
  TWebauthnCmd = ( wcSettings, wcUserExist, wcStartEnroll, wcEnrollVerify, wcStartAssert, wcAssertVerify);

const
  cWebAuthnPaths : Array[wcSettings..wcAssertVerify] of string =
               (
                 '/settings',
                 '/userexists',
                 '/enrollstart',
                 '/enrollverify',
                 '/assertstart',
                 '/assertverify'
               );

constructor TWebAuthnPathHandler.Create(ref: {$IFDEF FPC} TFPWebModule {$ELSE} TWebModule {$ENDIF}; dbHandler: IFroniusDBAdapter);
var iter : TWebauthnCmd;
    item : {$IFDEF FPC} TFPWebAction {$ELSE} TWebActionItem {$ENDIF};
begin
     fWebAuthnHandler := TFroniusWebauthHandler.Create(dbHandler);
     fDB := dbHandler;

     // ###########################################
     // #### Add the additional "actions" to the web module
     // PFC: https://wiki.freepascal.org/fpWeb_Tutorial#Old_Mechanism
     for iter := wcSettings to wcAssertVerify do
     begin
          item := ref.Actions.Add;
          {$IFDEF FPC}
          item.name := Copy(cWebAuthnPaths[iter], 2, $FFFF);
          item.OnRequest := @Handler;
          {$ELSE}
          item.Enabled := True;
          item.PathInfo := cWebAuthnPaths[iter];
          item.OnAction := Handler;
          {$ENDIF}
     end;

     // add specific session handler
     item := ref.Actions.Add;
     {$IFDEF FPC}
     item.Name := 'logout';
     item.OnRequest := @Handler;

     item := ref.Actions.Add;
     item.Name := 'checksession';
     item.OnRequest := @Handler;
     {$ELSE}
     item.Enabled := True;
     item.PathInfo := '/logout';
     item.OnAction := Handler;

     item := ref.Actions.Add;
     item.Enabled := True;
     item.PathInfo := '/checksession';
     item.OnAction := Handler;
     {$ENDIF}
end;

procedure TWebAuthnPathHandler.CreateSessionCookie(remoteAddr : string; Response: TWebResponse;
  uName: string);
var sessObj : IFroniusSession;
    {$IFNDEF FPC} cookieFields : TStringList;  {$ENDIF}
    {$IFDEF FPC} cookie : TCookie; {$ENDIF}
    cookieTxt : string;
begin
     // muhaaaa... we need a session
     sessObj := Sessions.CreateSession(uname, RemoteAddr);

     // generate cookie: https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies
     // according to the site we need to set the path and domain (so it is restricted).
     // the cookie is set in /fronius/login -> so set the path to '/' so it is valid for all elements on the site
     {$IFDEF FPC}
     Response.Cookies.Clear;
     cookie := Response.Cookies.Add;
     cookie.Name := 'SessionID';
     cookie.Domain := froniusConf.HTTPHost;
     cookie.Path := '/';
     cookie.Expires := sessObj.GetExpire;
     cookie.Secure := True;
     cookie.HttpOnly := False;
     cookie.SameSite := ssStrict;
     cookie.Value := sessObj.GetSessionId;

     cookieTxt := cookie.AsString;
     {$ELSE}
     cookieFields := TStringList.Create;
     try
        cookieFields.Add('SessionID=' + sessObj.GetSessionId);
        Response.SetCookieField(cookieFields, froniusConf.HTTPHost, '/', sessObj.GetExpire, True, False, 'strict');
     finally
            cookieFields.Free;
     end;
     cookieTxt :=  Response.Cookies[0].HeaderValue;
     {$ENDIF}

     // return everything inclusive the session cookie
     Response.Content := '{"verified":true,"sessid":"' + sessObj.GetSessionId + '","msg":"New session"' + ',"sessionCookie":" ' +
          cookieTxt + '"}';
end;

destructor TWebAuthnPathHandler.Destroy;
begin
     fWebAuthnHandler.Free;

     inherited;
end;

procedure TWebAuthnPathHandler.HandleCheckSession(Request: TWebRequest;
  Response: TWebResponse);
var sessObj : IFroniusSession;
begin
     sessObj := nil;

     if Sessions.HasValidSession(request, sessObj)
     then
         Response.Content := '{"session":1,"uname":"' + sessObj.GetUsername + '","host":"' + FidoServer.RelyingParty + '","sessid":"' + sessObj.GetSessionId + '","expires":"' +
                               FormatDateTime(fmt.LongDateFormat + ' ' + fmt.LongTimeFormat, sessObj.GetExpire, fmt ) + '"}'
     else
         Response.Content := '{"session":0,"host":"' + FidoServer.RelyingParty + '","msg":"No session - redirect..."}';
end;

procedure TWebAuthnPathHandler.HandleLogout(Request: TWebRequest;
  Response: TWebResponse);
var sessObj : IFroniusSession;
begin
     if Sessions.HasValidSession(request, sessObj) then
     begin
          Sessions.RemoveSession(sessObj);
          Response.Content := '{"result":0,"msg":"Session successfully removed"}';
     end
     else
         Response.Content := '{"error":255,"msg":"Session not registered"}';
end;

procedure TWebAuthnPathHandler.Handler(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var aPath : string;
begin
     Handled := True;

     // ###########################################
     // #### Init headers
     InitResponse(Response);

     if not fDB.IsConnected then
     begin
          Response.Content := '{"error":-1,"msg":"No databse connected"}';
          exit;
     end;

     // ###########################################
     // #### Select the right handler
     aPath := Request.PathInfo;
     if SameText(aPath, cWebAuthnPaths[wcSettings])
     then
         HandleSettings(Request, Response)
     else if SameText(aPath, cWebAuthnPaths[wcUserExist])
     then
         HandleUserExists(Request, Response)
     else if SameText(aPath, cWebAuthnPaths[wcStartEnroll])
     then
         HandleStartEnroll(Request, Response)
     else if SameText(aPath, cWebAuthnPaths[wcEnrollVerify])
     then
         HandleVerifyEnroll(Request, Response)
     else if SameText(aPath, cWebAuthnPaths[wcStartAssert])
     then
         HandleStartAssert (Request, Response)
     else if SameText(aPath, cWebAuthnPaths[wcAssertVerify])
     then
         HandleVerifyAssert(Request, Response)
     else if SameText(aPath, '/logout')
     then
         HandleLogout( Request, Response )
     else if SameText(aPath, '/checksession')
     then
         HandleCheckSession(Request, Response)
     else
         raise Exception.Create('This path is not supported');
end;

procedure TWebAuthnPathHandler.HandleSettings(Request: TWebRequest;
  Response: TWebResponse);
begin
     Response.Content := fWebAuthnHandler.Settings( Request.RemoteAddr );
end;

procedure TWebAuthnPathHandler.HandleStartAssert(Request: TWebRequest;
  Response: TWebResponse);
begin
     Response.Content := fWebAuthnHandler.StartAssert( getStringParam(Request, 'uname', '') );
end;

procedure TWebAuthnPathHandler.HandleStartEnroll(Request: TWebRequest;
  Response: TWebResponse);
begin
     Response.Content := fWebAuthnHandler.StartEnroll( Request.RemoteAddr,
                                                       getStringParam(Request, 'uname', ''),
                                                       getStringParam(Request, 'realname', '') );
end;

procedure TWebAuthnPathHandler.HandleUserExists(Request: TWebRequest;
  Response: TWebResponse);
begin
     Response.Content := fWebAuthnHandler.UserExists( getStringParam(Request, 'uname', '') );
end;

procedure TWebAuthnPathHandler.HandleVerifyAssert(Request: TWebRequest;
  Response: TWebResponse);
var resStr : string;
    uname : string;
begin
     resStr := '';
     uname := '';

     if Length(Request.content) = 0 then
     begin
          Response.Content := '{"error":255,"msg":"Empty request"}';
          exit;
     end;

     if fWebAuthnHandler.AssertVerify(Request.Content, resStr, uname)
     then
         CreateSessionCookie(Request.RemoteAddr, Response, uname)
     else
         Response.Content := resStr;
end;

procedure TWebAuthnPathHandler.HandleVerifyEnroll(Request: TWebRequest;
  Response: TWebResponse);
var jsonRes : string;
    uname : string;
begin
     jsonRes := '';
     uname := '';

     if fWebAuthnHandler.EnrollVeriy(Request.RemoteAddr, Request.Content, jsonRes, uname)
     then
         CreateSessionCookie(Request.RemoteAddr, Response, uname)
     else
         Response.Content := jsonRes;
end;


end.
