// ###########################################
// #### Shared memory for fronius deamon and the apache moduel (or cgi)
// ###########################################

unit Fronius.SharedMem;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Fronius.Consts, syncobjs {$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  TFroniusSharedPwrUpdate = procedure(Sender : TObject; const sample : TFlowDataSample) of Object;
  TFroniusSharedServerCmd = procedure(Sender : TObject; const cmd : integer ) of Object;
  TFroniusSharedOnError = procedure(Sender : TObject; const err : string ) of Object;

  // use the class in 2 different processes - messages are sent by changing the memory on one side and
  // get the update on the other side... when read the flag is cleared
  // there is no special care taken about synchronization: basically the state 0 means that data is either been written
  // or that the data has been read. If it may happen that there is no sync we have only a limited time no updated data...
  // note that named sempahores may be more apropriate here...
  { TFroniusSharedMemThread }

  TFroniusSharedMemThread = class(TThread)
  public
    const cSharedCmdRestart = 1;
  private
    // ###########################################
    // #### Shared memory
    const cSharedMemName = 'FroniusShared';

          cDaemonRestartFlag = 1;
          cDeamonCmdNone = 0;

    // structure shared in the shared memory space
    const cSharedStateIdle = 0;
          cSharedStatePowerUpdate = 1;
          cSharedStateError = 2;
          cSharedStateRead = 3;
          cSharedStateWrite = 4;


          cSharedCmdNone = 0;
    type
      TFroniusSharedMem = packed record
        state : integer;          // 0 - idle, 1 - PowerUpdated, 2 - error (see error message)
        sample : TFlowDataSample;
        errMsg : Array[0..511] of Char;  // valid if state <> 0
        serverCmd : integer;      // a flag from the receiving end (aka e.g. the apache server) that indicates a command to be executed - e.g. deamon restart
                                  // (for this to happen the fronius daeom needs to be still running and a watchdog thread needs to be allive...)
                                  // for now only a restart flag (1) is implemented
      end;
      PFroniusSharedMem = ^TFroniusSharedMem;
  private
    fOnLog: TLogEvent;
    fOnSampleUpd: TFroniusSharedPwrUpdate;
    fOnSharedError: TFroniusSharedOnError;
    fOnSharedServerCm: TFroniusSharedServerCmd;
    fOnSharedServerCmd: TFroniusSharedServerCmd;
    // update via shared memory
    fSharedFid : {$IFDEF MSWINDOWS}THandle{$ELSE}integer{$ENDIF};
    fSharedMem : PFroniusSharedMem;
    fLastSample : TFlowDataSample;
    fInterval : integer;
    fEvt : TSimpleEvent;

    procedure InitSharedMem(readOnly : boolean);
  protected
    procedure Execute; override;
  public
    property OnSampleUpd : TFroniusSharedPwrUpdate read fOnSampleUpd write fOnSampleUpd;
    property OnSharedServerCmd : TFroniusSharedServerCmd read fOnSharedServerCm write fOnSharedServerCmd;
    property OnSharedError : TFroniusSharedOnError read fOnSharedError write fOnSharedError;
    property OnLog : TLogEvent read fOnLog write fOnLog;

    procedure SetEnergyStatus( const PowerSample : TFlowDataSample );
    procedure SetState( errMsg : string );
    procedure SetCmd( newCmd : integer );

    function GetCurrentEnergyStatus(var PowerSample : TFlowDataSample) : boolean;

    procedure SignalTerminate;

    constructor Create(interval : integer);
    constructor CreateNonThreaded;
    destructor Destroy; override;
  end;

implementation

{$IFDEF LINUX}
uses BaseUnix, Unix;

// where are these defined?
function shm_open(__name:PAnsiChar; __oflag:longint; __mode:mode_t):longint;cdecl;external 'librt' name 'shm_open';
function shm_unlink(__name:PAnsiChar):longint;cdecl;external 'librt' name 'shm_unlink';
{$ENDIF}

{ TFroniusSharedMemThread }

procedure TFroniusSharedMemThread.Execute;
var cmd : integer;
begin
     while not Terminated do
     begin
          try
             if fEvt.WaitFor(fInterval) = wrTimeout then
             begin
                  if assigned(fOnSharedServerCmd) then
                  begin
                       if fSharedMem^.serverCmd <> cSharedCmdNone then
                       begin
                            cmd := fSharedMem^.serverCmd;
                            fSharedMem^.serverCmd := cSharedCmdNone;

                            // execute...
                            fOnSharedServerCmd(Self, cmd);
                       end;
                  end;

                  if Assigned(fOnSampleUpd) then
                  begin
                       if fSharedMem^.state = cSharedStatePowerUpdate then
                       begin
                            fLastSample := fSharedMem^.sample;
                            fSharedMem^.state := cSharedStateIdle;

                            // inform...
                            fOnSampleUpd(self, fLastSample);
                       end;
                  end;
             end;
          except
                // swallow here but report if event is there
                on E : Exception do
                begin
                     if Assigned(fOnLog) then
                        fOnLog(Self, 9, 'Shared mem error: ' + E.message);
                end;
          end;
     end;
end;

procedure TFroniusSharedMemThread.SetEnergyStatus(
  const PowerSample: TFlowDataSample);
var iter : integer;
begin
     iter := 0;
     while (fSharedMem^.state = cSharedStateRead) and (iter < 1000) do
     begin
          Yield;
          sleep(1);
          inc(iter);
     end;

     // do not override the state if set to error...
     if fSharedMem^.state <> cSharedStateError then
        fSharedMem^.state := cSharedStateWrite;

     fSharedMem^.Sample := PowerSample;

     // update state -> it can now be read on the "other" side
     if fSharedMem^.state <> cSharedStateError then
        fSharedMem^.state := cSharedStatePowerUpdate;
end;

procedure TFroniusSharedMemThread.SetState(errMsg: string);
begin
     fSharedMem^.state := cSharedStateIdle;
     FillChar(fSharedMem^.errMsg, sizeof(fSharedMem^.errMsg), 0);
     StrLCopy(fSharedMem^.errMsg, PChar(errMsg), Length(fSharedMem^.errMsg) - 1);
     fSharedMem^.state := cSharedStateError;
end;

procedure TFroniusSharedMemThread.SetCmd(newCmd: integer);
begin
     // check if already served
     if (fSharedMem^.serverCmd <> cSharedCmdNone) and Assigned(fOnLog) then
        fOnLog(self, 4, 'Command ' + IntToStr(fSharedMem^.serverCmd) + ' has not yet been served - will be overwritten');

     if Assigned(fOnLog) then
        fOnLog(self, 1, 'Serving command ' + IntToStr(newCmd) );

     fSharedMem^.serverCmd := newCmd;
end;

function TFroniusSharedMemThread.GetCurrentEnergyStatus(
  var PowerSample: TFlowDataSample): boolean;
begin
     Result := (fSharedMem <> nil) and (fSharedMem^.state = cSharedStatePowerUpdate);

     if Result then
        PowerSample := fSharedMem^.Sample;
end;

procedure TFroniusSharedMemThread.SignalTerminate;
begin
     Terminate;
     fEvt.SetEvent;
end;

procedure TFroniusSharedMemThread.InitSharedMem(readOnly : boolean);
var err : integer;
    flags : longint;
begin
     {$IFDEF LINUX}
     flags := O_RDONLY;
     if not readOnly then
        flags := O_CREAT or O_RDWR;
     fSharedFid := shm_open( '/' + cSharedMemName, flags, $1B6); // $1B6 = 0666found in examples all read and write permission - this should create a rw file for all but does not. need some investigation!

     if fSharedFid = -1 then
     begin
          err := errno;
          raise Exception.Create('Failed to create shared memory file handler: (' + IntToStr(err) + '): ' + SysErrorMessage(err) );
     end;

     if not readOnly then
     begin
          // setting the mapping size -> only needed if the mapping is created!
          if FpFtruncate( fSharedFid,  sizeof(TFroniusSharedMem) ) = -1 then    //ftruncate( fSharedFid, sizeof(TFroniusSharedMem) ) = -1 then
          begin
               err := errno;
               raise Exception.Create('Failed to set shared memory size: (' + IntToStr(err) + '): ' + SysErrorMessage(err) );
          end;
     end;

     // map to user space:
     flags := PROT_READ;
     if not readOnly then
        flags := flags or PROT_WRITE;
     fSharedMem := fpmmap( nil, sizeof(TFroniusSharedMem), flags, MAP_SHARED, fSharedFid, 0);
     if fSharedMem = MAP_FAILED then
     begin
          fSharedMem := nil;
          err := errno;
          raise Exception.Create('Failed to map memory: (' + IntToStr(err) + '): ' + SysErrorMessage(err) );
     end;
     {$ENDIF}
     {$IFDEF MSWINDOWS}
     flags := PAGE_READWRITE;
     if readOnly then
        flags := PAGE_READONLY;
     fSharedFid := CreateFileMapping(
                 INVALID_HANDLE_VALUE,    // use paging file
                 nil,                    // default security
                 flags,                  // read/write access
                 0,                       // maximum object size (high-order DWORD)
                 sizeof(TFroniusSharedMem),                // maximum object size (low-order DWORD)
                 cSharedMemName);
     if fSharedFid = 0 then
     begin
          err := GetLastError;
          raise Exception.Create('Failed to create shared memory (' + err.ToString + '): ' + SysErrorMessage(err));
     end;

     flags := FILE_MAP_ALL_ACCESS;
     if readOnly then
        flags := FILE_MAP_READ;

     fSharedMem := MapViewOfFile(fSharedFid,   // handle to map object
                                 flags, // read/write permission
                                 0,
                                 0,
                                 sizeof(TFroniusSharedMem));
     if fSharedMem = nil then
     begin
          fSharedMem := nil;
          err := GetLastError;
          raise Exception.Create('Failed to map memory: (' + IntToStr(err) + '): ' + SysErrorMessage(err) );
     end;

     {$ENDIF}
end;


constructor TFroniusSharedMemThread.Create(interval: integer);
begin
     fEvt := TSimpleEvent.Create;

     // #####################################
     // #### Create or open the shared mem object
     InitSharedMem(False);

     inherited Create(False);
end;

constructor TFroniusSharedMemThread.CreateNonThreaded;
begin
     fEvt := TSimpleEvent.Create;
     InitSharedMem(True);

     // do not start the thread -> we only want to read memory so no interval update is necessary
     inherited Create(True);
end;

destructor TFroniusSharedMemThread.Destroy;
begin
     fEvt.Free;

     {$IFDEF LINUX}
     if fSharedMem <> nil then
        fpMUnMap( fSharedMem, sizeof(TFroniusSharedMem));
     fSharedMem := nil;

     if fSharedFid > 0 then
        shm_unlink('/' + cSharedMemName);
     {$ENDIF}
     {$IFDEF MSWINDOWS}
     if fSharedMem <> nil then
        UnmapViewOfFile(fSharedMem);
     if fSharedFid <> 0 then
        CloseHandle(fSharedFId);
     {$ENDIF}

     inherited Destroy;
end;

end.

