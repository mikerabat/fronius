object dmFroniusDBCreate: TdmFroniusDBCreate
  OldCreateOrder = False
  Height = 191
  HorizontalOffset = 86
  VerticalOffset = 86
  Width = 252
  object dbFronius: TIBDatabase
    Connected = False
    CreateIfNotExists = False
    AllowStreamedConnected = False
    FirebirdLibraryPathName = 'libfbclient.so.2'
    DefaultTransaction = ibTransaction
    IdleTimer = 0
    TraceFlags = []
    UseDefaultSystemCodePage = False
    Left = 56
    Top = 24
  end
  object ibTransaction: TIBTransaction
    TransactionName = '{F400BF15-BF54-486F-B08A-AE9016428FAC}'
    Active = False
    Left = 56
    Top = 88
  end
  object scFronius: TIBXScript
    Database = dbFronius
    Echo = False
    IgnoreGrants = False
    IgnoreCreateDatabase = False
    Transaction = ibTransaction
    ShowAffectedRows = False
    ShowPerformanceStats = False
    Left = 152
    Top = 24
  end
end
