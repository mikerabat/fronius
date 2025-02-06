// ###########################################
// #### A simple list class that holds all the sessions
// ###########################################

unit Fronius.Sessions;

interface

uses SysUtils, Classes, cryptRandom, Fronius.DBIntf,
{$IFDEF FPC} HTTPDefs {$ELSE} Web.HTTPApp {$ENDIF},
     SyncObjs, WebauthnHandler;

{$IFDEF FPC}
type
  TWebRequest = TRequest;
  TWebResponse = TResponse;
{$ENDIF}

type
  IFroniusSession = interface
   ['{FE751E04-CEF3-4BDD-B91A-8814DE61B8C8}']
   function GetSessionId : string;
   function GetIP : string;
   function GetExpire : TDateTime;
   function GetUsername : string;
   function GetUID : integer;
  end;
  TFroniusSession = class(TInterfacedObject, IFroniusSession)
  private
    fExpire : TDateTime;
    fIP : string;
    fSessID : string;
    fUsername : string;
    fUID : integer;
  public
    function GetUID : integer;
    function GetSessionId : string;
    function GetIP : string;
    function GetExpire : TDateTime;
    function GetUserName : string;

    constructor Create( ses : IFroniusDBAdapter ); overload;
    constructor Create( sessid : string; ip : string; expire : TDateTime; uid : integer; uName : string ); overload;
  end;

  TFroniusSessions = class(TInterfaceList)
  private
    fDB : IFroniusDBAdapter;
    fRand : IRndEngine;
    fLock : TMultiReadExclusiveWriteSynchronizer;

    procedure ReadSessions;
    procedure DoGarbageCollect;

  private
    type
      TSessionGarbageCollector = class(TThread)
      private
        fExpireTimeout : integer;
        fRef : TFroniusSessions;
        fSig : TSimpleEvent;
      protected
        procedure Execute; override;
      public

        procedure SigTerminate;
        constructor Create(aRef : TFroniusSessions);
        destructor Destroy; override;
      end;
  private
    fGarbageCollector : TSessionGarbageCollector;
  public
     function HasValidSession( request : TWebRequest; out sessObj : IFroniusSession ) : boolean;
     function CreateSession(uname, remoteip : string ) : IFroniusSession;
     procedure RemoveSession( sessObj : IFroniusSession );

     constructor Create(db : IFroniusDBAdapter);
     destructor Destroy; override;
  end;

function Sessions : TFroniusSessions;

procedure InitSessionList( dbIntf : IFroniusDBAdapter);
procedure FinalizeSessionList;

implementation

uses Fronius.Globals, Fronius.Consts;

var loc_Sessions : TFroniusSessions = nil;

function Sessions : TFroniusSessions;
begin
     // not thread save -> the function needs to be called in an initialization routine
     Result := loc_Sessions;
end;


{ TFroniusSessions }

constructor TFroniusSessions.Create(db : IFroniusDBAdapter);
begin
     inherited Create;

     fLock := TMultiReadExclusiveWriteSynchronizer.Create;
     fRand := CreateRndObj;

     fDB := db;

     // ###########################################
     // #### Now initialize the session cache
     ReadSessions;

     // ###########################################
     // #### Initialize garbage collector
     fGarbageCollector := TSessionGarbageCollector.Create(self);
end;

function TFroniusSessions.CreateSession(uname, remoteip : string): IFroniusSession;
var expire : TDateTime;
    sessId : string;
    i : Integer;
    uid : integer;
begin
     if Length(uname) > 64 then
        exit(nil);

     // ###########################################
     // #### Create a random sequence...
     sessID := '';
     for i := 0 to cFroniusSessIdLen - 1 do
         sessId := sessId + IntToHex( fRand.Random, 2 );

     expire := now + froniusConf.GetExpirePeriod;

     fLock.BeginWrite;
     try
        try
           fDB.SetSQLText('Select uid from froniususers where uname=:uname');
           fDB.SetParam('uname', uname);
           fDB.ExecQuery;
           assert(not fdB.Eof, 'No user found!');

           uid := fDB.GetRes('uid', 0);
           fDB.Close;

           fDB.SetSQLText('Insert into froniusSessions (sessionid, ipaddress, expires, uid) values(:sessid, :ip, :expires, :uid)');
           fDB.SetParam('sessid', sessId);
           fDB.SetParam('ip', RemoteIP);
           fDB.SetParamDT('expires', expire);
           fDB.SetParam('uid', uid);
           fDB.ExecQuery;
        finally
               fDB.Commit;
        end;

        // add to cache...
        Result := TFroniusSession.Create(sessId, RemoteIP, expire, uid, uname);
        Add( Result );
     finally
            fLock.EndWrite;
     end;
end;

destructor TFroniusSessions.Destroy;
begin
     fGarbageCollector.SigTerminate;
     fGarbageCollector.WaitFor;
     fGarbageCollector.Free;

     fLock.Free;

     inherited;
end;

procedure TFroniusSessions.DoGarbageCollect;
var needCleanup : boolean;
    curTime : TDateTime;
    locSession : IFroniusSession;
    i : integer;
begin
     curTime := now;

     // ###########################################
     // #### First run - it's often unlikely to have an element removed
     // -> collect them in a read only lock...
     needCleanup := False;
     fLock.BeginRead;
     try
        for i := 0 to Count - 1 do
        begin
             locSession := Items[i] as IFroniusSession;

             needCleanup := needCleanup or (locSession.GetExpire < curTime);
        end;
     finally
            fLock.EndRead;
     end;

     // ###########################################
     // #### Second run if we need a cleanup -> write access here!
     if needCleanup then
     begin
          fLock.BeginWrite;
          try
             // ###########################################
             // #### Delete from DB
             try
                fDB.SetSQLText('Delete from froniussessions where expires<:t');
                fDB.SetParamDT('t', curTime);
                fDB.ExecQuery;
             finally
                    fDB.Commit;
             end;

             // ###########################################
             // #### Delete from cache
             for i := Count - 1 downto 0 do
             begin
                  locSession := Items[i] as IFroniusSession;

                  if (locSession.GetExpire < curTime) then
                     Delete(i);
             end;
          finally
                 fLock.EndWrite;
          end;
     end;
end;

function TFroniusSessions.HasValidSession(request: TWebRequest;
  out sessObj: IFroniusSession): boolean;
var sessId : string;
    ipAddress : string;
    i : integer;
    locSession : IFroniusSession;
begin
     sessObj := nil;
     Result := False;

     // ###########################################
     // #### Extract session from cookie data...
     sessId := request.CookieFields.Values['sessionID'];
     ipAddress := request.RemoteAddr;

     if sessID = '' then
        exit;

     // ###########################################
     // #### Check if it's in the cache
     fLock.BeginRead;
     try
        // just do a linear search - maybe not optimal but the list is short ;)
        // count backwards so we can savely remove sessions...
        for i := Count - 1 downto 0 do
        begin
             locSession := Items[i] as IFroniusSession;

             Result := SameStr(locSession.GetSessionId, sessID);
             Result := Result and SameStr(locSession.GetIP, ipAddress);

             if Result then
             begin
                  sessObj := locSession;
                  break;
             end;
        end;
     finally
            fLock.EndRead;
     end;
end;

procedure TFroniusSessions.ReadSessions;
var session : IFroniusSession;
begin
     Clear;

     fDB.SetSQLText('Select * from FroniusSessions, FroniusUsers where FroniusSessions.uid=FroniusUsers.uid');
     fDB.ExecQuery;

     // ###########################################
     // #### Read the stored sessions from the db
     while not fDB.Eof do
     begin
          session := TFroniusSession.Create(fDB);
          Add( session );

          fDB.Next;
     end;

     fDB.Close;
     fDB.Commit;
end;

procedure TFroniusSessions.RemoveSession(sessObj: IFroniusSession);
var sessId : string;
begin
     sessId := sessObj.GetSessionId;

     fLock.BeginWrite;
     try
        // ###########################################
        // #### Remove from DB
        try
           fDB.SetSQLText('delete from FroniusSessions where SessionID=:sessId');
           fDB.SetParam('sessId', sessId);
           fDB.ExecQuery;
        finally
               fDB.Commit;
        end;

        // ###########################################
        // #### Remove from cache
        Delete( IndexOf(sessObj) );
     finally
            fLock.EndWrite;
     end;
end;

{ TFroniusSession }

constructor TFroniusSession.Create(ses: IFroniusDBAdapter);
begin
     inherited Create;

     fExpire := ses.GetResDT( 'Expires', 0);
     fIP := ses.GetRes('IPAddress', '');
     fSessID := ses.GetRes('SessionID', '');
     fUsername := ses.GetRes('uname', '');
     fUid := Ses.GetRes('uid', -1);
end;

constructor TFroniusSession.Create(sessid, ip: string; expire: TDateTime; uid : Integer; uName : string);
begin
     inherited Create;

     fUsername := uName;
     fExpire := expire;
     fIP := ip;
     fSessID := sessid;
     fUid := uid;
end;

function TFroniusSession.GetExpire: TDateTime;
begin
     Result := fExpire;
end;

function TFroniusSession.GetIP: string;
begin
     Result := fIP;
end;

function TFroniusSession.GetSessionId: string;
begin
     Result := fSessID;
end;

function TFroniusSession.GetUID: integer;
begin
     Result := fUid;
end;

function TFroniusSession.GetUserName: string;
begin
     Result := fUsername;
end;

procedure InitSessionList(dbIntf : IFroniusDBAdapter);
begin
     if not Assigned(loc_Sessions) then
        loc_Sessions := TFroniusSessions.Create(dbIntf);
end;

procedure FinalizeSessionList;
begin
     FreeAndNil(loc_Sessions);
end;

{ TFroniusSessions.TSessionGarbageCollector }

constructor TFroniusSessions.TSessionGarbageCollector.Create(
  aRef: TFroniusSessions);
begin
     fRef := aRef;
     fSig := TSimpleEvent.Create; //(nil, True, False, '');
     fExpireTimeout := FroniusConfig.GetExpireCheckPeriod;

     inherited Create(False);
end;

destructor TFroniusSessions.TSessionGarbageCollector.Destroy;
begin
     fSig.Free;

     inherited;
end;

procedure TFroniusSessions.TSessionGarbageCollector.Execute;
begin
     while not Terminated and (fSig.WaitFor( fExpireTimeout ) = wrTimeout) do
     begin
          try
             // ###########################################
             // #### Call the reference objects cleanup procedure...
             fRef.DoGarbageCollect;
          except
                // todo: logging...
          end;
     end;
end;

procedure TFroniusSessions.TSessionGarbageCollector.SigTerminate;
begin
     Terminate;
     fSig.SetEvent;
end;

initialization
  loc_sessions := nil;

finalization

end.
