program TestFroniusAPI;

uses
  Vcl.Forms,
  ufrmTestFroniusAPI in 'ufrmTestFroniusAPI.pas' {frmFroniusAPI},
  Fronius.Data in '..\Src\Fronius.Data.pas',
  Fronius.Win.DB in '..\Src\Fronius.Win.DB.pas' {dmFronius: TDataModule},
  Fronius.EnergyEstimate in '..\Src\Fronius.EnergyEstimate.pas',
  Fronius.Consts in '..\Src\Fronius.Consts.pas',
  Fronius.DB in '..\Src\Fronius.DB.pas',
  Fronius.DBIntf in '..\Src\Fronius.DBIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFroniusAPI, frmFroniusAPI);
  Application.CreateForm(TdmFronius, dmFronius);
  Application.Run;
end.
