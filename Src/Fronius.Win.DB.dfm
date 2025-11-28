object dmFronius: TdmFronius
  Height = 479
  Width = 549
  PixelsPerInch = 120
  object FroniusDB: TIBDatabase
    DatabaseName = 'C:\Users\mraba\Documents\DriveD\fronius\DB\FRONIUS.FDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = trFronius
    ServerType = 'IBServer'
    Left = 60
    Top = 100
  end
  object trFronius: TIBTransaction
    DefaultDatabase = FroniusDB
    Left = 60
    Top = 190
  end
  object sqlSample: TIBSQL
    Database = FroniusDB
    Transaction = trFronius
    Left = 210
    Top = 100
  end
  object idFirebirdTest: TIdTCPClient
    ConnectTimeout = 0
    Port = 0
    ReadTimeout = -1
    Left = 60
    Top = 290
  end
end
