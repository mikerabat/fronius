program FroniusAdm;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Fronius.Win.DBCreate in 'Fronius.Win.DBCreate.pas' {dmFroniusDBCreate: TDataModule};

procedure PrintHelp;
begin
     Writeln('Call FroniusAdm.exe -cleardevice -clearusr -db <dblocation> -user <auser e.g. sysdba> -pwd <apwd e.g. masterkey>');
     Writeln('The tool can be used to clear the registered users and even remove the users');
     Writeln('Parameter');
     Writeln('-db <dblocation>  - the location in form of host:dblocation (host is optional). The parameter is mandatory');
     Writeln('-user auser - the database user that shall be used to create the database. Default is sysdba');
     Writeln('-password apwd - the db users password to connect to Firebird. Default is masterkey');
     Writeln('-clearusr Removes the user and the registered devices');
     Writeln('-cleardevice Clears all the devices/credentials');
end;

var i : integer;
    dbLocation : string;
    dbUser : string;
    dbPwd : string;
    doclearUser : boolean;
    doclearCredentials : boolean;
begin
     try
        // ###########################################
        // #### extract params
        dbUser := 'sysdba';
        dbPwd := 'masterkey';
        dbLocation := '';
        doclearUser := False;
        doclearCredentials := False;

        for i := 1 to ParamCount do
        begin
             if SameText(ParamStr(i), '-db') then
                dbLocation := ParamStr(i + 1);
             if SameText(ParamStr(i), '-user') then
                dbUser := ParamStr(i + 1);
             if SameText(ParamStr(i), '-password') then
                dbPwd := ParamStr(i + 1);
             if SameText(ParamStr(i), '-clearusr') then
                doclearUser := True;
             if SameText(ParamStr(i), '-cleardevice') then
                doclearCredentials := True;
        end;

        if (dbLocation = '') or not (doclearUser or doclearCredentials) then
        begin
             PrintHelp;
             exit;
        end;

        // ###########################################
        // #### create dataase
        if doclearCredentials
        then
            ClearCredentials( dbLocation, dbUser, dbPwd )
        else
            ClearUsers( dbLocation, dbUser, dbPwd );

        Writeln('Successfully executed clear operation');
     except
           on E: Exception do
              Writeln(E.ClassName, ': ', E.Message);
     end;
end.
