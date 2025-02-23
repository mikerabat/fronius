// ###########################################
// #### Create a database at the location that is provided in the params
// #### Params:
// ####  -db <host>:<PathtoDB>
// ####  -user <firebird DBUsername> - default is sysdba
// ####  -password <firebird DBPasswird> - default is masterkey
// ####  -sql <DDEFile> - the database definition - def is froniusdb.sql
// ###########################################
program CreateFroniusDB;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


{$IFNDEF FPC}
{$APPTYPE CONSOLE}
{$R *.res}
{$ENDIF}

uses
  SysUtils,
  Fronius.Consts,
  Classes,
  StrUtils,
  {$IFDEF LINUX}
  Fronius.Linux.DBCreate
  {$ELSE}
  Fronius.Win.DBCreate
  {$ENDIF};

procedure PrintHelp;
begin
     Writeln('Call CreateDBForninus.exe -db <dblocation> -user <auser e.g. sysdba> -pwd <apwd e.g. masterkey> -createconf -conf <filename>');
     Writeln('The tool can be used to create the fronius database at the given location');
     Writeln('Parameter');
     Writeln('-db <dblocation>  - the location in form of host:dblocation (host is optional). The parameter is mandatory');
     Writeln('-user auser - the database user that shall be used to create the database. Default is sysdba');
     Writeln('-password apwd - the db users password to connect to Firebird. Default is masterkey');
     Writeln('-sql DDEFile - the database definition file. Default is froniusdb.sql');
     Writeln('-fbclientlib <fn> - The name of the fbclient library that needs to be taken. Default is an empty string');
     Writeln('-createconf - create a standard configuration file according to the params used to create the db');
     Writeln('-cconf <fn> - configuration filename. Default is /etc/froniusd.conf');
     Writeln('-log <fn> - fronius daemon log. Default is /var/log/froniusd.log');
     Writeln('-httphost <url> - Url from which the server can be reached from the net.');
     Writeln('-createApacheConf - if set the apache configuration file is written.');
     Writeln('-sslcert <fn> - Path to the preallocated certificate (e.g. from certbot or a self signed certificate');
     Writeln('-sslkey <fn> - SSL private key (e.g. from certbot or a self signed certificate');
     Writeln('-email <address> - Email address put into the apache configuration.');
     Writeln('-httpDocRoot <dir> - Root directory of the fronius website. Default is /var/www/fronius');
     Writeln('-apacheCfgIn <fn> - The template used to create the apache configuration. Default is fronius.conf.tpl');
     Writeln('-apacheCfgOut <fn> - The apache configuration output file that the tool creates. Default is /etc/apache2/site-avail/fronius.conf');
end;

procedure InitApacheConf( httpHost, sslCert, sslKey, apacheTpl, apacheOut, email, docRoot : string );
var sl : TStringList;
    s : string;
    i : integer;
begin
     sl := TStringList.Create;
     try
        // load template
        sl.LoadFromFile(apacheTpl);

        // replace placeholders
        for i := 0 to sl.Count - 1 do
        begin
             s := ReplaceText(sl[i], '%email%', email);
             s := ReplaceText(s, '%documentroot%', docRoot);
             s := ReplaceText(s, '%httpHost%', httpHost);
             s := ReplaceText(s, '%httpAlias%', httpHost);
             s := ReplaceText(s, '%sslCert%', sslCert);
             s := ReplaceText(s, '%sllKey%', sslKey);

             sl[i] := s;
        end;

        sl.SaveToFile(apacheOut, TEncoding.ASCII);
     finally
            sl.Free;
     end;
end;

var i : integer;
    dbLocation : string;
    dbUser : string;
    dbPwd : string;
    dbSQL : string;
    createConf : boolean;
    confFn : string;
    logFn : string;
    fbClientLib : string;
    httpHost : string;
    froniusHost : string;
    sslCert : string;
    sslKey : string;
    createApacheConf : boolean;
    apacheTpl : string;
    apacheOut : string;
    httpDocRoot : string;
    email : string;
begin
     try
        // ###########################################
        // #### extract params
        dbUser := 'sysdba';
        dbPwd := 'masterkey';
        dbLocation := '';
        dbSQL := 'froniusdb.sql';
        confFn := '/etc/froniusd.conf';
        createConf := False;
        logFn := cDefFroniusLog;
        fbClientLib := '';
        froniusHost := 'fronius';
        httpHost := '';
        sslCert := '';
        sslKey := '';
        apacheTpl := 'fronius.conf.tpl';
        apacheOut := '/etc/apache2/site-avail/fronius1.conf';
        httpDocRoot := '/var/www/fronius';

        for i := 1 to ParamCount do
        begin
             if SameText(ParamStr(i), '-db') then
                dbLocation := ParamStr(i + 1);
             if SameText(ParamStr(i), '-user') then
                dbUser := ParamStr(i + 1);
             if SameText(ParamStr(i), '-password') then
                dbPwd := ParamStr(i + 1);
             if SameText(ParamStr(i), '-sql') then
                dbSQL := ParamStr(i + 1);
             if SameText(ParamStr(i), '-createconf') then
                createConf := True;
             if SameText(ParamStr(i), '-log') then
                logFn := ParamStr(i + 1);
             if SameText(ParamStr(i), '-fbclientlib') then
                fbClientLib := ParamStr(i + 1);
             if SameText(ParamStr(i), '-froniusHost') then
                froniusHost := ParamStr(i + 1);
             if SameText(ParamStr(i), '-httpHost') then
                httpHost := ParamStr(i + 1);
             if SameText(ParamStr(i), '-sslcert') then
                sslCert := ParamStr(i + 1);
             if SameText(ParamStr(i), '-sslkey') then
                sslKey := ParamStr(i + 1);
             if SameText(ParamStr(i), '-email') then
                email := ParamStr(i + 1);
             if SameText(ParamStr(i), '-httpDocRoot') then
                httpDocRoot := ParamStr(i + 1);
             if SameText(ParamStr(i), '-createApacheConf') then
                createApacheConf := True;
             if SameText(ParamStr(i), '-apacheCfgOut') then
                apacheOut := ParamStr(i + 1);
             if SameText(ParamStr(i), '-apacheCfgIn') then
                apacheTpl := ParamStr(i + 1);
        end;

        if (not createApacheConf) and ( (dbLocation = '') or not FileExists(dbSQL) ) then
        begin
             PrintHelp;
             exit;
        end;

        // ###########################################
        // #### create dataase
        if dbLocation <> '' then
        begin
             CreateDB(dbLocation, dbUser, dbPwd, dbSQL, fbClientLib);

             Writeln('Successfully created db');
        end;

        // ###########################################
        // #### create config file
        if createConf then
        begin
             InitConfig(dbLocation, dbUser, dbPwd, confFn, logFn, fbClientLib, froniusHost, httpHost );

             Writeln('Created default configuration file ' + confFN);
        end;

        if createApacheConf then
        begin
             InitApacheConf( httpHost, sslCert, sslKey, apacheTpl, apacheOut, email, httpDocRoot );
             Writeln('Created apache config');
        end;
     except
       on E: Exception do
         Writeln(E.ClassName, ': ', E.Message);
     end;
end.
