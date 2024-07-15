// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2019, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit Fronius.FidoDBData;

// this class handles the fido data in a firebird database
// two tables are used:
// usercredentials: stores the private stuff after enroll, sigcounter
// challenges table

interface

uses SysUtils, Classes, WebauthnHandler, AuthData, Fronius.DBIntf,
     Fido2, SuperObject;

// ###########################################
// #### User handling based on a db connection
type
  TFidoDBDataHandling = class(TInterfacedObject, IFidoDataHandling)
  private
    fDB : IFroniusDBAdapter;
    fChallengeTimeout : double;  // 5 minutes

    function GetOrInsertUser( uname : string ) : integer;
  private
    //type
//      TFidoDBGarbageCollector = class(TThread)
//      private
//        fDB : TIBDatabase;
//        fSQL : TIBSQL;
//        fSig : TSimpleEvent;
//      protected
//        procedure Execute; override;
//      public
//        procedure SigTerminate;
//        constructor Create;
//        destructor Destroy; override;
//      end;
  public
    procedure CleanupPendingChallenges(aChallenge : string = '');

    function IsAlreadRegistered( uname : string ) : boolean; overload;
    function IsAlreadRegistered( uname : string; var credID : string ) : boolean; overload;

    function IsChallengeInitiated( challenge : string; var data : ISuperObject ) : boolean;
    function CredentialDataFromId( credId : string; var data : string ) : TFidoCredentialFmt;

    procedure SaveUserInitChallenge( user : TFidoUserStartRegister );
    function SaveCred( fmt : string; id : string; userHandle : string; challenge : string; cred : TFidoCredVerify; authData : TAuthData ) : boolean;

    function CredToUser(credId: string; var uname: string): boolean;

    function CheckSigCounter(credId: string; authData: TAuthData): boolean;
    procedure SaveAssertChallengeData( challenge : ISuperObject );
    function LoadAssertChallengeData( challenge : string ) : ISuperObject;

    constructor Create(dbHandler : IFroniusDBAdapter);
    destructor Destroy; override;
  end;


implementation

uses cbor, Fronius.Globals;

{ TFileFidoUserHandling }

constructor TFidoDBDataHandling.Create( dbHandler : IFroniusDBAdapter);
begin
     inherited Create;
     fDB := dbHandler;

     fChallengeTimeout := 5*SecsPerMin/SecsPerDay;   // now - 5 minutes is the timeout
end;

destructor TFidoDBDataHandling.Destroy;
begin

     inherited;
end;

function TFidoDBDataHandling.GetOrInsertUser(uname: string): integer;
begin
     if Length(uname) >= 128 then
        raise Exception.Create('Username too long');

     fDB.SetSQLText('Select uid from FroniusUsers where uname=:uname');
     fDB.SetParam('uname', uname);
     fDB.ExecQuery;

     if fDB.Eof then
     begin
          fDB.Close;
          // create the user first
          fDB.SetSQLText( 'SELECT GEN_ID(GEN_FRONIUSUSERS_ID, 1) AS newUID FROM RDB$DATABASE' );
          fDB.ExecQuery;
          Result := fDB.GetRes('newUID', -1);
          fDB.Close;

          fDB.SetSQLText( 'Insert into FroniusUsers (uid, uname) Values(:uid, :uname)');
          fDB.SetParam('uid', Result);
          fDB.SetParam('uname', uname);
          fDB.ExecQuery;
     end
     else
         Result := fDB.GetRes('uid', -1);

     fDB.Close;
end;

function TFidoDBDataHandling.CredentialDataFromId(credId: string; var data : string): TFidoCredentialFmt;
var fmt : string;
begin
     data := '';
     fDB.SetSQLText('Select fmt, data from usercredentials where credentials=:credId');
     fDB.SetParam('credId', credId);
     fDB.ExecQuery;

     Result := fmNone;
     if not fDB.Eof then
     begin
          fmt := fDB.GetRes('fmt', '');

          if SameText(fmt, 'none')
          then
              Result := fmNone
          else if SameText(fmt, 'packed')
          then
              Result := fmFido2
          else if SameText(fmt, 'tpm')
          then
              Result := fmTPM
          else if SameText(fmt, 'fido-u2f')
          then
              Result := fmU2f;

          data := fDB.GetRes('data', '');
     end;

     fDB.Close;
     fDB.Commit;
end;

function TFidoDBDataHandling.CredToUser(credId: string;
  var uname: string): boolean;
begin
     fDB.SetSQLText('Select a.uname, b.credentials from FRONIUSUSERS a, usercredentials as b ' +
                    'where b.credentials=:cred and a.uid=b.uid and b.enabled=1 order by b.modified');
     fDB.SetParam('cred', credID);
     fDB.ExecQuery;
     Result := not fDB.Eof;

     if Result then
        uname := fDB.GetRes('uname', '');

     fDB.Close;
     fDB.Commit;
end;

function TFidoDBDataHandling.IsAlreadRegistered(uname: string): boolean;
var credIDFN : string;
begin
     Result := IsAlreadRegistered(uname, credIDFN);
end;

function TFidoDBDataHandling.IsAlreadRegistered(uname: string;
  var credID: string): boolean;
begin
     fDB.SetSQLText( 'Select a.uname, b.credentials from FRONIUSUSERS a, usercredentials as b ' +
                     'where a.uname=:uname and a.uid=b.uid and b.enabled=1 order by b.modified');
     fDB.SetParam('uname', uname);
     fDB.ExecQuery;

     Result := not fDB.Eof;
     if Result then
        credID := fDB.GetRes('credentials', '');

     fDB.Close;
     fDB.Commit;
end;

function TFidoDBDataHandling.IsChallengeInitiated(challenge: string; var data : ISuperObject): boolean;
begin
     data := nil;

     fDB.SetSQLText('Select * from fidochallenges where challenge=:challenge and modified>:modified');
     fDB.SetParam('challenge', challenge);
     fDB.SetParamDT('modified', now - fChallengeTimeout);
     fDB.ExecQuery;

     // check if the challenge was requested here -> we can associate it with the user now ;)
     Result := not fDB.Eof;

     try
     if Result then
        data := SO( fDB.GetRes('data', '') );
     except
           Writeln( 'Failed IsChallangeInit: ' + fDB.GetRes('data', '') );

     end;
     fDB.Close;
     fDB.Commit;
end;

procedure TFidoDBDataHandling.CleanupPendingChallenges(aChallenge : string = '');
begin
     fDB.SetSQLText('Delete from fidochallenges where modified<=:modified or challenge=:challenge');
     fDB.SetParamDT('modified', Now - fChallengeTimeout);
     fDB.SetParam('challenge', aChallenge);
     fDB.ExecQuery;
     fDB.Commit;
end;

function TFidoDBDataHandling.LoadAssertChallengeData(
  challenge: string): ISuperObject;
begin
     Result := nil;

     fDB.SetSQLText('Select * from fidochallenges where challenge=:challenge and modified>:modified');
     fDB.SetParamDT('modified', Now - fChallengeTimeout);
     fDB.SetParam('challenge', Challenge);
     fDB.ExecQuery;

     try
     if not fDB.Eof then
        Result := SO( fDB.GetRes('Data', '' ) );
     except
           Writeln( 'Failed LoadAssert: ' + fDB.GetRes('data', '') );
           raise
     end;
     fDB.Close;
     fDB.Commit;
end;

procedure TFidoDBDataHandling.SaveAssertChallengeData(challenge: ISuperObject);
begin
     fDB.SetSQLText('Insert into fidochallenges (challenge, data, modified) ' +
                         'Values(:challenge, :data, :modified)');
     fDB.SetParam('challenge', challenge.S['publicKey.challenge']);
     fDB.SetParam('data', challenge.AsJSon);
     fDB.SetParamDT('modified', now);
     fDB.ExecQuery;
     fDB.Commit;
end;

function TFidoDBDataHandling.SaveCred(fmt : string; id : string; userHandle : string; challenge : string; cred : TFidoCredVerify; authData : TAuthData ) : boolean;
var sCred : string;
    credData : ISuperObject;
    memStream : TMemoryStream;
    uid : integer;
    clientData : ISuperObject;
begin
     Result := False;

     // ###########################################
     // #### Checks - extract challenge; check for duplicate id
     try
        try
           fDB.SetSQLText('Select count(*) as cnt from usercredentials where CREDENTIALS=:CREDENTIALS');
           fDB.SetParam('CREDENTIALS', id);
           fDB.ExecQuery;

           if fDB.GetRes('cnt', 1) <> 0 then
              exit;

           fDB.Close;
           fDB.SetSQLText( 'Select * from FidoChallenges where challenge=:challenge' );
           fDB.SetParam('challenge', challenge);
           fDB.ExecQuery;

           if fDB.Eof then
              exit;
        finally
               fDB.Close;
        end;

        clientData := SO( fDB.GetRes('Data', '' ) );

        if fDB.GetResDT('modified', 0) < now - 5*SecsPerHour/SecsPerDay then
           raise Exception.Create('Challenge too old');

        // ###########################################
        // #### Prepare data as json
        credData := SO;

        if fmt = 'none'
        then
            sCred := Base64URLEncode(authData.RawData)
        else
        begin
             memStream := TMemoryStream.Create;
             try
                credData := SO;
                cred.SavePKToStream(memStream);
                credData.S['cert.pk'] := Base64Encode(memStream.Memory, memStream.Size );
                memStream.Clear;
                cred.SaveSigToStream(memStream);
                credData.S['cert.sig'] := Base64Encode(memStream.Memory, memStream.Size );
                memStream.Clear;
                cred.SaveX5cToStream(memStream);
                credData.S['cert.x5c'] := Base64Encode(memStream.Memory, memStream.Size );
                credData.O['user'] := clientData.O['publicKey.user'].Clone;
             finally
                    memStream.Free;
             end;
             sCred := credData.AsJSon;
        end;

        uid := GetOrInsertUser(clientData.S['publicKey.user.name']);

        // ###########################################
        // #### now insert the credentials
        fDB.SetSQLText( 'Insert into usercredentials (UID, LOGINTYPE, CREDENTIALS, ENABLED, DATA, MODIFIED, SIGCOUNTER, FMT, USERHANDLE) ' +
                        'Values(:UID, :LOGINTYPE, :CREDENTIALS, :ENABLED, :DATA, :MODIFIED, :SIGCOUNTER, :fmt, :USERHANDLE)' );
        fDB.SetParam('uid', uid);
        fDB.SetParam('logintype', 1);
        fDB.SetParam('credentials', id);
        fDB.SetParam('enabled', 1);
        fDB.SetParam('data', sCred);
        fDB.SetParamDT('modified', now);
        fDB.SetParam('sigcounter', authData.SigCount);
        fDB.SetParam('fmt', fmt);
        fDB.SetParam('userhandle', userHandle);
        fDB.ExecQuery;

        // cleanup old challenge
        fDB.SetSQLText( 'delete from fidochallenges where challenge=:challenge or modified<:modified' );
        fDB.SetParam('challenge', challenge);
        fDB.SetParamDT('modified', now - 5*SecsPerHour/SecsPerDay);
        fDB.ExecQuery;

        fDB.Commit;
     except
           on E : Exception do
           begin
                fDB.Rollback;
                raise;
           end;
     end;
     Result := True;
end;


procedure TFidoDBDataHandling.SaveUserInitChallenge(
  user: TFidoUserStartRegister);
var challenge : string;
    obj : ISuperObject;
begin
     obj := user.ToJson;
     challenge := Base64URLEncode( @user.Challenge[0], sizeof(user.Challenge));
     try
        fDB.SetSQLText( 'Insert into fidochallenges (challenge, data, modified) ' +
                        'Values(:challenge, :data, :modified)' );
        fDB.SetParam('challenge', challenge);
        fDB.SetParam('data', obj.AsJSon);
        fDB.SetParamDT('modified', now);
        fDB.ExecQuery;
        fDB.Commit;
     except
           on E : Exception do
           begin
               fDB.Rollback;
               raise;
           end;
     end;
end;

function TFidoDBDataHandling.CheckSigCounter(credId : string; authData: TAuthData): boolean;
var sigCnt: LongWord;
begin
     Result := False;

     try
        fDB.SetSQLText( 'select sigcounter from usercredentials where credentials=:credId' );
        fDB.SetParam('credid', credId);
        fDB.ExecQuery;

        if not fDB.Eof then
        begin
             sigCnt := fDB.GetRes('sigcounter', 0);

             Result := ((sigCnt = 0) and (authData.SigCount = 0)) or
                        (sigCnt < authData.SigCount);

             if Result then
             begin
                  // update sigcounter
                  fDB.Close;
                  fDB.SetSQLText( 'Update usercredentials set sigcounter=:sigcounter where credentials=:credId');
                  fDB.SetParam('credid', credId);
                  fDB.SetParam('sigcounter', authData.SigCount);
                  fDB.ExecQuery;
             end;
        end
        else
            fDB.Close;

        fDB.Commit;
     except
           on E : Exception do
           begin
                fDB.Rollback;
                raise;
           end;
     end;
end;

{ TFileFidoDatarHandling.TFidoDBGarbageCollector }

//constructor TFidoDBDataHandling.TFidoDBGarbageCollector.Create;
//begin
//     // ###########################################
//     // #### load the data directory from the registry - default is the current directory
//     fDB := TIBDatabase.Create(nil);
//     with fDB do
//     begin
//          SQLDialect := 3;
//          LoginPrompt := False;
//          ServerType := 'IBServer';
//          TraceFlags := [];
//          DefaultTransaction := TIBTransaction.Create(fDB);
//
//
//          // Get Database-Location from registry (Local machine)
//          DatabaseName := froniusConf.DBName;
//
//          if firebirdConnected then
//          begin
//               Params.Clear;
//               Params.Add('user_name=' + froniusConf.DBUser);
//               Params.Add('password=' + froniusConf.DBPwd);
//               Connected := True;
//          end;
//     end;
//
//     fSQL := TIBSQL.Create(nil);
//     fSQL.Database := fDB;
//
//     inherited Create;
//end;
//
//destructor TFidoDBDataHandling.TFidoDBGarbageCollector.Destroy;
//begin
//     fSQL.Free;
//     fDB.Free;
//
//     inherited;
//end;
//
//procedure TFidoDBDataHandling.TFidoDBGarbageCollector.Execute;
//begin
//     // ###########################################
//     // #### Garbage collector loop
//     while not Terminated do
//     begin
//          if fSig.WaitFor(60000) = wrTimeout then
//          begin
//               try
//                  fref.CleanupPendingChallenges;
//               except on E: Exception do
//               end;
//          end;
//     end;
//end;
//
//procedure TFidoDBDataHandling.TFidoDBGarbageCollector.SigTerminate;
//begin
//     Terminate;
//     fSig.SetEvent;
//end;

end.
