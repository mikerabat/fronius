object dmFronius: TdmFronius
  Height = 383
  Width = 439
  object FroniusDB: TIBDatabase
    Connected = True
    DatabaseName = 
      'C:\Users\rabatscher\Documents\Embarcadero\Studio\Projekte\Froniu' +
      's\DB\FRONIUS.FDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = trFronius
    ServerType = 'IBServer'
    Left = 48
    Top = 80
  end
  object trFronius: TIBTransaction
    DefaultDatabase = FroniusDB
    Left = 48
    Top = 152
  end
  object sqlSample: TIBSQL
    Database = FroniusDB
    Transaction = trFronius
    Left = 168
    Top = 80
  end
  object idFirebirdTest: TIdTCPClient
    ConnectTimeout = 0
    Port = 0
    ReadTimeout = -1
    Left = 48
    Top = 232
  end
end
