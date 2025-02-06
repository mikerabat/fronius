object wmFronius: TwmFronius
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  OldCreateOrder = False
  Actions = <  
    item
      Name = 'waDefault'
      Default = True
      OnRequest = waDefaultRequest
      Template.AllowTagParams = False
    end>
  CreateSession = False
  CORS.Enabled = False
  CORS.Options = [coAllowCredentials, coEmptyDomainToOrigin]
  CORS.AllowedMethods = 'GET, PUT, POST, OPTIONS, HEAD'
  CORS.AllowedOrigins = '*'
  CORS.AllowedHeaders = 'x-requested-with, content-type, authorization'
  CORS.MaxAge = 0
  Height = 235
  HorizontalOffset = 86
  VerticalOffset = 86
  Width = 296
  object FroniusDB: TIBDatabase
    Connected = False
    LoginPrompt = False
    CreateIfNotExists = False
    AllowStreamedConnected = False
    IdleTimer = 0
    TraceFlags = []
    UseDefaultSystemCodePage = False
    Left = 40
    Top = 32
  end
  object trFronius: TIBTransaction
    TransactionName = '{66B781EE-6B79-46D0-B246-9317D278EB25}'
    Active = False
    DefaultDatabase = FroniusDB
    Left = 136
    Top = 32
  end
  object sqlSample: TIBQuery
    AllowAutoActivateTransaction = False
    Database = FroniusDB
    Transaction = trFronius
    BufferChunks = 1000
    BufferChunksInFirstBlock = 50
    CachedUpdates = False
    CaseSensitiveParameterNames = False
    EnableStatistics = False
    GenerateParamNames = False
    GeneratorField.ApplyOnEvent = gaeOnNewRecord
    MasterDetailDelay = 0
    Params = <>
    SQLFiltered = False
    DataSetCloseAction = dcDiscardChanges
    TZTextOption = tzOffset
    DefaultTZDate = 43831
    Left = 40
    Top = 120
  end
end
