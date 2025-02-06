// ###########################################
// #### Global defintiions - handling the configuration
// ###########################################
unit Fronius.Globals;

interface

uses Fronius.Consts, SysUtils;

var firebirdConnected : boolean = False;
    fmt : TFormatSettings;

function froniusConf : IFroniusConf;

implementation

uses IdTCPClient, {$IFDEF WINDOWS} Windows, {$ENDIF} SyncObjs, WebauthnHandler;

var loc_froniusConf : IFroniusConf = nil;
    locCS : TCriticalSection = nil;

function TestFirebirdServerCon : boolean;
var aHost, aPortSN : string;
    iPort : integer;
    idFirebirdTest : TIdTCPClient;

  procedure ParseDatabaseName( var aHost, aPort : string );
  var idx : integer;
  begin
       aHost := '127.0.0.1';
       aPort := '3050';

       idx := Pos(':', froniusConf.DBName );
       if idx <= 2 then
          exit;

       aHost := Copy(froniusConf.DBName, 1, idx - 1);
       idx := Pos('/', aHost);

       if idx <= 0 then
          exit;

       aPort := Copy(aHost, idx + 1, Length(aHost));
       aHost := Copy(aHost, 1, idx - 1);
  end;
begin
     Result := True;

     // http://www.firebirdfaq.org/faq123/
     // -> they do a telnet on the designated port - we try to connect with
     // a shorter timeout
     idFirebirdTest := TIdTCPClient.Create(nil);
     try
        idFirebirdTest.ConnectTimeout := froniusConf.TestTimeout;
        idFirebirdTest.ReadTimeout := froniusConf.TestTimeout;

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
               FidoServer.Log(9, 'Failed to convert port: ' + aHost + ':' + aPortSN);
        except
              on E: Exception do
              begin
                   FidoServer.Log(9, 'Failed to convert port: ' + aHost + ':' + aPortSN);
                   Result := False;
              end;
        end;
     finally
            idFirebirdTest.Free;
     end;
end;

function froniusConf : IFroniusConf;
begin
     locCS.Enter;
     try
        if not Assigned(loc_froniusConf) then
        begin
             loc_froniusConf := FroniusConfig;

             // ###########################################
             // #### Update Webauthn config
             FidoServer.RequireResidentKey := loc_froniusConf.ResidentKey;
             FidoServer.RelyingParty := loc_froniusConf.HTTPHost;
             FidoServer.RelyingPartyId := loc_froniusConf.HTTPHost;


             firebirdConnected := not loc_froniusConf.TestFirebirdServer;
             if not firebirdConnected then
                firebirdConnected := TestFirebirdServerCon;
        end;
     finally
            locCS.Leave;
     end;

     Result := loc_froniusConf;
end;

initialization
  locCS := TCriticalSection.Create;

  // ###########################################
  // #### Initialize with some fixed non localized (but German ;) ) settings.
  // ###########################################
  fmt := TFormatSettings.Invariant;
  fmt.DateSeparator := '.';
  fmt.TimeSeparator := ':';
  fmt.ShortTimeFormat := 'hh:nn:ss';
  fmt.LongTimeFormat := fmt.ShortTimeFormat;
  fmt.ShortDateFormat := 'dd.mm.yyyy';
  fmt.LongDateFormat := fmt.ShortDateFormat;

finalization
  FreeAndNil(locCS);

end.
