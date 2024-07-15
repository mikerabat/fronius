// ###########################################
// #### Initialization of the apache webbroker module
// ###########################################
unit Fronius.ModuleInit;

interface

procedure InitModule;

implementation

uses SysUtils, {$IFDEF MS_WINDOWS} Windows, {$ENDIF} Fronius.Sessions,
     {$IFDEF FPC} custapache24, httpd24, apr24, froniusDataMod, {$ELSE} Web.WebBroker, Web.HTTPD24, modFronius, {$ENDIF}
     WebauthnHandler, Fronius.Globals, Fronius.DBIntf;

const APR_SUCCESS = 0;

{$IFDEF MS_WINDOWS}
// I got some strange "stopped working" windows when the debugger was attached.
// The following Stackoverflow question seem to describe a similar problem.
// See http://stackoverflow.com/questions/6414514/unhandled-exception-in-rad-studio-debugger-thread
procedure PatchINT3;
const INT3: Byte = $CC;
      NOP: Byte = $90;
var NTDLL: THandle;
    BytesWritten: NativeUInt;
    Address: PByte;
begin
     if Win32Platform <> VER_PLATFORM_WIN32_NT then
        Exit;

     NTDLL := GetModuleHandle('NTDLL.DLL');
     if NTDLL = 0 then
        Exit;

     Address := GetProcAddress(NTDLL, 'RtlQueryCriticalSectionOwner');
     if Address = nil then
       Exit;

     Inc(Address, $E8);
     try
        if Address^ <> INT3 then
           Exit;

       if WriteProcessMemory(GetCurrentProcess, Address, @NOP, 1, BytesWritten) and (BytesWritten = 1) then
          FlushInstructionCache(GetCurrentProcess, Address, 1);
     except
           //Do not panic if you see an EAccessViolation here, it is perfectly harmless!
           on EAccessViolation do
           ;
     else
         raise;
     end;
end;

{$ENDIF}

procedure HandleApacheException(E: Exception);
begin
     // todo: do some logging
end;

function ModuleCleanup(Data : Pointer) : apr_status_t; cdecl;
begin
     FinalizeSessionList;

     Result := APR_SUCCESS;
end;

function cleanupNull(Data : Pointer) : apr_status_t; cdecl;
begin
     Result := 0;
end;

procedure InitFidoServerObj;
var fidoSrv : TFidoServer;
begin
     // fill fido server properties
     fidoSrv := FidoServer;

     // init...
     fidoSrv.RelyingParty := froniusConf.HTTPHost;
     fidoSrv.RelyingPartyId := froniusConf.HTTPHost;
     fidoSrv.AttestType := atNone;
     fidoSrv.UserVerification := True;
     fidoSrv.RequireResidentKey := False;
end;

function ModulePreConfigHook(pConf : Papr_pool_t; plog : Papr_pool_t; ptemp : Papr_pool_t) : integer; cdecl;
var dbAdapter : IFroniusDBAdapter;
begin
     apr_pool_cleanup_register(pConf, nil, @ModuleCleanup, @cleanupNull);

     dbAdapter := TwmFronius.Create(nil);
     InitSessionList(dbAdapter);
     InitFidoServerObj;

     Result := APR_SUCCESS;
end;

procedure InitModule;
begin
     // The default number of concurrent connections (32) is too short -> use a higher one:
     // use a higher number of concurrent connections as defined in apache's httpd.conf file
     {$IFDEF FPC}
     Application.MaxRequests := 150;
     {$ELSE}
     Application.MaxConnections := 150;
     // set this variable to handle all occuring exceptions on shutdown
     HandleShutdownException := HandleApacheException;
     {$ENDIF}

     // add a hook to get informed about the server shutdown.
     // this is the only save place to unload the plugin dll's
     ap_hook_pre_config({$IFDEF FPC}@{$ENDIF}ModulePreConfigHook, nil, nil, APR_HOOK_LAST);
end;

initialization

  {$IFDEF MS_WINDOWS}
  PatchINT3;
  {$ENDIF}

end.
