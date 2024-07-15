object frmFroniusSvc: TfrmFroniusSvc
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  Dependencies = <
    item
      Name = 'FirebirdServerDefaultInstance'
      IsGroup = False
    end>
  DisplayName = 'Fronius'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 480
  Width = 640
end
