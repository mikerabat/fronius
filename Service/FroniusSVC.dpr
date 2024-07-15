// ###########################################
// #### Fronius data accumulating/loggin service
// ###########################################
program FroniusSVC;

uses
  Vcl.SvcMgr,
  ufrmFroniusSvc in 'ufrmFroniusSvc.pas' {frmFroniusSvc: TService},
  Fronius.Data in '..\Src\Fronius.Data.pas',
  Fronius.Win.DB in '..\Src\Fronius.Win.DB.pas' {dmFronius: TDataModule},
  Fronius.EnergyEstimate in '..\Src\Fronius.EnergyEstimate.pas',
  Fronius.Consts in '..\Src\Fronius.Consts.pas',
  Fronius.DB in '..\Src\Fronius.DB.pas';

{$R *.RES}

begin
  // delayInitialize only necessary for win2003 server and ole object initialization
  // -> we do not have that...
  if not Application.DelayInitialize or Application.Installing then
     Application.Initialize;

  Application.CreateForm(TfrmFroniusSvc, frmFroniusSvc);
  Application.Run;
end.
