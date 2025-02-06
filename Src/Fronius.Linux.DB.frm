object dmFronius: TdmFronius
  OldCreateOrder = False
  Height = 309
  HorizontalOffset = 86
  VerticalOffset = 86
  Width = 369
  object FroniusDB: TIBDatabase
    Connected = False
    LoginPrompt = False
    CreateIfNotExists = False
    AllowStreamedConnected = False
    DefaultTransaction = trFronius
    IdleTimer = 0
    TraceFlags = []
    UseDefaultSystemCodePage = False
    Left = 40
    Top = 32
  end
  object sqlSample: TIBSQL
    Database = FroniusDB
    CaseSensitiveParameterNames = False
    GenerateParamNames = False
    UniqueParamNames = False
    ParamCheck = True
    Scrollable = False
    Transaction = trFronius
    Left = 152
    Top = 32
  end
  object trFronius: TIBTransaction
    TransactionName = '{FEC8B4BF-C4F0-4259-8814-71D137E893ED}'
    Active = False
    Left = 40
    Top = 112
  end
  object idFirebirdTest: TIdTCPClient
    ConnectTimeout = 0
    Port = 0
    ReadTimeout = -1
    Left = 152
    Top = 112
  end
end
