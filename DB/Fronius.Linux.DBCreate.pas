unit Fronius.Linux.DBCreate;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, IBDatabase, ibxscript;


type

  { TdmFroniusDBCreate }

  TdmFroniusDBCreate = class(TDataModule)
    dbFronius : TIBDatabase;
    ibTransaction : TIBTransaction;
    scFronius: TIBXScript;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  dmFroniusDBCreate: TdmFroniusDBCreate;


procedure CreateDB(location, dbUser, dbPwd, dbSQL : string);

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.frm}

procedure CreateDB(location, dbUser, dbPwd, dbSQL : string);
var script : TStringList;
begin
     dmFroniusDBCreate.dbFronius.DatabaseName := location;
     dmFroniusDBCreate.dbFronius.Params.Add('user_name=' + dbUser);
     dmFroniusDBCreate.dbFronius.Params.Add('password=' + dbPwd);

     // Create the database
     //dmFroniusDBCreate.scFronius.NoPreconnect := True;
     script := TStringList.Create;
     try
        script.text := 'CREATE DATABASE '''+location+
             ''' USER '''+dbUser+
             ''' PASSWORD '''+dbPwd+
             ''' PAGE_SIZE 16384 DEFAULT CHARACTER SET UTF8;';
        dmFroniusDBCreate.scFronius.RunScript(script);

        // Create the table structure and fill in some basic values
        dmFroniusDBCreate.dbFronius.Open;
        script.LoadFromFile( dbSQL );
        dmFroniusDBCreate.scFronius.RunScript(script);
        dmFroniusDBCreate.dbFronius.Close;
      finally
             script.Free;
      end;
end;

initialization
  dmFroniusDBCreate := TdmFroniusDBCreate.Create(nil);
finalization
  dmFroniusDBCreate.Free;

end.
