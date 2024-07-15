object wmFronius: TwmFronius
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end>
  Height = 230
  Width = 415
  object trFronius: TIBTransaction
    DefaultDatabase = FroniusDB
    Left = 24
    Top = 88
  end
  object FroniusDB: TIBDatabase
    DatabaseName = 
      'C:\Users\rabatscher\Documents\Embarcadero\Studio\Projekte\Froniu' +
      's\DB\FRONIUS.FDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = trFronius
    ServerType = 'IBServer'
    Left = 24
    Top = 16
  end
  object sqlSample: TIBSQL
    Database = FroniusDB
    Transaction = trFronius
    Left = 144
    Top = 16
  end
end
