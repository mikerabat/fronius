// ###########################################
// #### Webauthn handling for the Fronius Apache module
// ###########################################
unit Fronius.Webauthn;

interface

uses SysUtils, Classes, Fronius.DBIntf, cryptRandom, Webauthnhandler;

// ###########################################
// #### Handler for all the webauthn calls in the server
// the handler returns the "json" strings that are needed for the
// sites javascripts.
type
  TFroniusWebauthHandler = class(TObject)
  private
    fDB : IFroniusDBAdapter;
    fFidoDataHandler : IFidoDataHandling;
    fRand : IRndEngine;

    function IsLocalIPAddress( remoteAddr : string; var errCode : integer) : boolean;
  public
    function StartEnroll(remoteAddr : string; uname, realName : string) : string;
    function EnrollVeriy(remoteAddr : string; requestContent : string; var jsonRes, uname : string) : boolean;

    function Settings( remoteAddr : string ) : string;
    function UserExists( user : string ) : string;

    function StartAssert( uname : string ) : string;
    function AssertVerify( requestContent : string; var jsonRes, uname : string ) : boolean;

    constructor Create( dbIntf : IFroniusDBAdapter);
    destructor Destroy; override;
  end;

implementation

uses Types, SuperObject, Fronius.FidoDBData, Fronius.Globals;

{ TFroniusWebauthHandler }

function TFroniusWebauthHandler.AssertVerify(requestContent: string;
  var jsonRes, uname: string): boolean;
var err : ISuperObject;
begin
     try
        with TFidoUserAssert.Create(fRand) do
        try
           SetHandler(fFidoDataHandler);
           // assert success? if so then create a session cookie...
           Result := VerifyAssert(requestContent, jsonRes, uname);
        finally
               Free;
        end;
     except
           on E : Exception do
           begin
                err := SO;
                err.I['error'] := 1;
                err.S['msg'] := 'An error occured: ' + E.Message;

                jsonRes := err.AsJSon;
                Result := False;
           end;

     end;
end;

constructor TFroniusWebauthHandler.Create(dbIntf: IFroniusDBAdapter);
begin
     fDB := dbIntf;
     fRand := CreateRndObj;
     fFidoDataHandler := TFidoDBDataHandling.Create(dbIntf);

     inherited Create;
end;

destructor TFroniusWebauthHandler.Destroy;
begin
     inherited;
end;

function TFroniusWebauthHandler.EnrollVeriy(remoteAddr : string; requestContent: string; var jsonRes,
  uname: string): boolean;
var errCode : integer;
    err : ISuperObject;
begin
     // ###########################################
     // #### Enrollment is only allowed in the local area network...
     Result := False;
     if not IsLocalIPAddress(RemoteAddr, errCode) then
     begin
          case errCode of
            0: jsonRes := '{"error":-254,"msg":"No external addresses allowed to enroll."}';
            1: jsonRes := '{"error":-255,"msg":"The ip address is not in a valid format."}';
          else
              jsonRes := '{"error":-1,"msg":"An error occured"}';
          end;
          exit;
     end;

     try
        // ###########################################
        // #### Run the verification process on the content data
        with TFidoUserRegisterVerify.Create do
        try
           SetHandler(fFidoDataHandler);
           Result := VerifyAndSaveCred( requestContent, jsonRes, uname )
        finally
               Free;
        end;
     except
           on E : Exception do
           begin
                err := SO;
                err.I['error'] := 1;
                err.S['msg'] := 'An error occured: ' + E.Message;

                jsonRes := err.AsJSon;
           end;
     end;
end;

function TFroniusWebauthHandler.IsLocalIPAddress(remoteAddr: string; var errCode : integer): boolean;
var ipAddr : DWORD;
    sl : TStringList;
const cLocalHost = $7F000001; // 127.0.0.1
      cIPNet = $C0A80000;     // 192.168.0.0
      cIPNetMask = $FFFF0000;
begin
     errCode := 0;
     Result := False;

     // ipv4 check: todo! ipv6
     sl := TStringList.Create;
     try
        sl.Delimiter := '.';
        sl.DelimitedText := RemoteAddr;

        if sl.Count <> 4 then
        begin
             errCode := 1;

             exit;
        end;

        try
           ipAddr := StrToInt(sl[0]) shl 24 + StrToInt(sl[1]) shl 16 + StrToInt(sl[2]) shl 8 + strToInt(sl[3]);

           // either localhost or lan segment
           Result := (ipAddr = cLocalHost) or ( (ipAddr and cIPNetMask) = cIPNet);
        except
              on E : Exception do
              begin
                   errCode := 2;
                   exit;
              end;
        end;
     finally
            sl.Free;
     end;

end;

function TFroniusWebauthHandler.Settings(remoteAddr: string): string;
var oSet : ISuperObject;
    errCode : integer;
begin
     oSet := FidoServer.ToJSON;
     oSet.B['AllowEnroll'] := IsLocalIPAddress(remoteAddr, errCode);

     if fDB.IsConnected then
     begin
          fDB.SetSQLText('Select count(*) as cnt from froniususers');
          fDB.ExecQuery;
          oSet.B['CanLogin'] := fDB.GetRes('cnt', 0) > 0;
          fDB.Close;
          fDB.Commit;
     end;

     Result := oSet.AsJSon;
end;

function TFroniusWebauthHandler.StartAssert(uname: string): string;
var err : ISuperObject;
begin
     try
        with TFidoUserAssert.Create(fRand) do
        try
           SetHandler(fFidoDataHandler);
           Result := StartAssertion(uname);
        finally
               Free;
        end;
     except
           on E : Exception do
           begin
                err := SO;
                err.I['error'] := 1;
                err.S['msg'] := 'An error occured: ' + E.Message;

                Result := err.AsJSon;
           end;
     end;
end;

function TFroniusWebauthHandler.StartEnroll(remoteAddr, uname,
  realName: string): string;
var errCode : integer;
    user : TFidoUserStartRegister;
    obj : ISuperObject;
begin
     // ###########################################
     // #### this request is only allowed in the local area network
     if not IsLocalIPAddress(remoteAddr, errCode) then
     begin
          case errCode of
            0: Result := '{"error":-254,"msg":"No external addresses allowed to enroll."}';
            1: Result := '{"error":-255,"msg":"The ip address is not in a valid format."}';
          else
              Result := '{"error":-1,"msg":"An error occured"}';
          end;
          exit;
     end;

     // ###########################################
     // #### Enroll process
     if (uName = '') then
     begin
          Result := '{"result":1,"msg":"No user name detected"}';
          exit;
     end;

     // ###########################################
     // #### After extraction of the params prepare the response
     user := TFidoUserStartRegister.Create( uName, realName, fRand );
     try
        user.SetHandler(fFidoDataHandler);
        if not FroniusConf.AllowMultipleCredPerUser and not user.CheckUser( uname )
        then
            Result := '{"result":2,"msg":"User already registered"}'
        else
        begin
             // save the challenge to db or files
             user.SaveChallenge;

             // prepare response
             obj := user.ToJson;
             Result := obj.AsJSon;
        end;
     finally
            user.Free;
     end;
end;

function TFroniusWebauthHandler.UserExists(user: string): string;
begin
     // just map the user to a json output for the server...
     if (user = '') then
     begin
          Result := '{"result":1,"msg":"No user name detected"}';
          exit;
     end;

     if fFidoDataHandler.IsAlreadRegistered(user)
     then
         Result := '{"result":2,"msg":"User already exists"}'
     else
         Result := '{"result":0,"msg":"User does not exist"}';
end;

end.
