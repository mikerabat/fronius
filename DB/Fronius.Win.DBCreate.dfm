object dmFroniusDBCreate: TdmFroniusDBCreate
  Height = 191
  Width = 252
  object scFronius: TIBScript
    Database = dbFronius
    Transaction = trFronius
    Terminator = ';'
    Left = 144
    Top = 48
  end
  object dbFronius: TIBDatabase
    LoginPrompt = False
    DefaultTransaction = trFronius
    ServerType = 'IBServer'
    Left = 40
    Top = 48
  end
  object trFronius: TIBTransaction
    DefaultDatabase = dbFronius
    Left = 40
    Top = 112
  end
  object ibClearSQL: TIBSQL
    Database = dbFronius
    Transaction = trFronius
    Left = 144
    Top = 112
  end
end
