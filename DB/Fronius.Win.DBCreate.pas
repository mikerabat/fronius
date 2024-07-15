unit Fronius.Win.DBCreate;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$IFnDEF FPC}
uses
  System.SysUtils, System.Classes, IBX.IBDatabase, Data.DB, IBX.IBScript,
  IBX.IBSQL;
{$ELSE}
{$ENDIF}


type
  TdmFroniusDBCreate = class(TDataModule)
    scFronius: TIBScript;
    dbFronius: TIBDatabase;
    trFronius: TIBTransaction;
    ibClearSQL: TIBSQL;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  dmFroniusDBCreate: TdmFroniusDBCreate;


procedure CreateDB(location, dbUser, dbPwd, dbSQL : string);
procedure ClearCredentials(location, dbUser, dbPwd : string);
procedure ClearUsers(location, dbUser, dbPwd : string);

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure CreateDB(location, dbUser, dbPwd, dbSQL : string);
begin
     dmFroniusDBCreate.dbFronius.DatabaseName := location;
     dmFroniusDBCreate.dbFronius.Params.Add('user_name=' + dbUser);
     dmFroniusDBCreate.dbFronius.Params.Add('password=' + dbPwd);

     // Create the database
     //dmFroniusDBCreate.scFronius.NoPreconnect := True;
     dmFroniusDBCreate.scFronius.SCRIPT.Text:='CREATE DATABASE '''+location+
          ''' USER '''+dbUser+
          ''' PASSWORD '''+dbPwd+
          ''' PAGE_SIZE 16384 DEFAULT CHARACTER SET UTF8;';
     dmFroniusDBCreate.scFronius.ExecuteScript;

     // Create the table structure and fill in some basic values
     dmFroniusDBCreate.dbFronius.Open;
     dmFroniusDBCreate.scFronius.Script.LoadFromFile( dbSQL );
     dmFroniusDBCreate.scFronius.ExecuteScript;
     dmFroniusDBCreate.dbFronius.Close;
end;

procedure InternalClearData(location, dbUser, dbPwd : string; clearUsr : boolean );
begin
     dmFroniusDBCreate.dbFronius.DatabaseName := location;
     dmFroniusDBCreate.dbFronius.Params.Add('user_name=' + dbUser);
     dmFroniusDBCreate.dbFronius.Params.Add('password=' + dbPwd);

     // Cleanup
     dmFroniusDBCreate.dbFronius.Open;
     dmFroniusDBCreate.trFronius.StartTransaction;
     if clearUsr then
     begin
          dmFroniusDBCreate.ibClearSQL.SQL.Text := 'delete from fidochallenges';
          dmFroniusDBCreate.ibClearSQL.ExecQuery;
          dmFroniusDBCreate.ibClearSQL.SQL.Text := 'delete from FRONIUSSESSIONS';
          dmFroniusDBCreate.ibClearSQL.ExecQuery;
          dmFroniusDBCreate.ibClearSQL.SQL.Text := 'delete from USERCREDENTIALS';
          dmFroniusDBCreate.ibClearSQL.ExecQuery;
          dmFroniusDBCreate.ibClearSQL.SQL.Text := 'delete from FRONIUSUSERS';
          dmFroniusDBCreate.ibClearSQL.ExecQuery;
     end
     else
     begin
          dmFroniusDBCreate.ibClearSQL.SQL.Text := 'delete from fidochallenges';
          dmFroniusDBCreate.ibClearSQL.ExecQuery;
          dmFroniusDBCreate.ibClearSQL.SQL.Text := 'delete from USERCREDENTIALS';
          dmFroniusDBCreate.ibClearSQL.ExecQuery;
     end;

     dmFroniusDBCreate.trFronius.Commit;
     dmFroniusDBCreate.dbFronius.Close;
end;

procedure ClearCredentials(location, dbUser, dbPwd : string);
begin
     InternalClearData(location, dbUser, dbPwd, False);
end;

procedure ClearUsers(location, dbUser, dbPwd : string);
begin
     InternalClearData(location, dbUser, dbPwd, True);
end;

initialization
  dmFroniusDBCreate := TdmFroniusDBCreate.Create(nil);
finalization
  dmFroniusDBCreate.Free;

end.
