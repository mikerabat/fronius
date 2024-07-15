library mod_fronius;

uses
  {$IFDEF MSWINDOWS}
  Winapi.ActiveX,
  System.Win.ComObj,
  {$ENDIF }
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPD24Impl,
  Fronius.Consts in '..\..\Src\Fronius.Consts.pas',
  Fronius.Globals in '..\Fronius.Globals.pas',
  Fronius.Sessions in '..\Fronius.Sessions.pas',
  Fronius.ModuleInit in '..\Fronius.ModuleInit.pas',
  authData in '..\extLibs\authData.pas',
  cbor in '..\extLibs\cbor.pas',
  Fido2 in '..\extLibs\Fido2.pas',
  Fido2dll in '..\extLibs\Fido2dll.pas',
  superobject in '..\extLibs\superobject.pas',
  WebauthnHandler in '..\extLibs\WebauthnHandler.pas',
  winCryptRandom in '..\extLibs\winCryptRandom.pas',
  Fronius.FidoDBData in '..\Fronius.FidoDBData.pas',
  OpenSSL1_1ForWebauthn in '..\extLibs\OpenSSL1_1ForWebauthn.pas',
  Fronius.Webauthn in '..\Fronius.Webauthn.pas',
  Fronius.DBIntf in '..\..\Src\Fronius.DBIntf.pas',
  Fronius.WebAuthnModuleInit in '..\Fronius.WebAuthnModuleInit.pas',
  Fronius.WebDataHandler in '..\Fronius.WebDataHandler.pas',
  modFronius in 'modFronius.pas' {wmFronius: TWebModule},
  Fronius.ActionsBase in '..\Fronius.ActionsBase.pas';

{$R *.res}

{$E so}             //change apache module file extension from .dll to .so

// example for httpd.conf
(*
 LoadModule fronius_module modules/mod_fronius.dll

 <Location /fronius>
    SetHandler mod_fronius-handler
 </Location>
*)
//
var
  GModuleData: TApacheModuleData;
exports
  GModuleData name 'fronius_module';

begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  InitModule;
  Application.Run;
end.
