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
{$ENDIF}

{$R *.res}

uses
  SysUtils,
  Fronius.Consts,
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
     Writeln('-createconf - create a standard configuration file according to the params used to create the db');
     Writeln('-cconf <fn> - configuration filename. Default is /etc/froniusd.conf');
end;

var i : integer;
    dbLocation : string;
    dbUser : string;
    dbPwd : string;
    dbSQL : string;
    createConf : boolean;
    confFn : string;
    logFn : string;
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
        logFn := cDefFroniusLog;;

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
        end;

        if (dbLocation = '') or not FileExists(dbSQL) then
        begin
             PrintHelp;
             exit;
        end;

        // ###########################################
        // #### create dataase
        CreateDB(dbLocation, dbUser, dbPwd, dbSQL);

        Writeln('Successfully created db');

        // ###########################################
        // #### create config file
        if createConf then
        begin
             InitConfig(dbLocation, dbUser, dbPwd, confFn, logFn );

             Writeln('Created default configuration file ' + confFN);
        end;
     except
       on E: Exception do
         Writeln(E.ClassName, ': ', E.Message);
     end;
end.
