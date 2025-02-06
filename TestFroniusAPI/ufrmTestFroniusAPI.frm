object frmFroniusAPI: TfrmFroniusAPI
  Left = 0
  Height = 441
  Top = 0
  Width = 624
  Caption = 'Test Fronius API'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  LCLVersion = '8.4'
  OnDestroy = FormDestroy
  object pnlLeft: TPanel
    Left = 0
    Height = 441
    Top = 0
    Width = 185
    Align = alLeft
    BevelOuter = bvNone
    ClientHeight = 441
    ClientWidth = 185
    ParentBackground = False
    TabOrder = 0
    object edFroniusHost: TLabeledEdit
      Left = 16
      Height = 25
      Top = 48
      Width = 153
      EditLabel.Height = 15
      EditLabel.Width = 153
      EditLabel.Caption = 'Host or Address'
      TabOrder = 0
      Text = '192.168.178.119'
    end
    object btnVersion: TButton
      Left = 16
      Height = 25
      Top = 96
      Width = 153
      Caption = 'Version'
      TabOrder = 1
      OnClick = btnInverterInfoClick
    end
    object btnInverterInfo: TButton
      Left = 16
      Height = 25
      Top = 127
      Width = 153
      Caption = 'Inverter Info'
      TabOrder = 2
      OnClick = btnInverterInfoClick
    end
    object btnInverterRealTimeData: TButton
      Left = 16
      Height = 25
      Top = 158
      Width = 153
      Caption = 'Inverter real time data'
      TabOrder = 3
      OnClick = btnInverterInfoClick
    end
    object btnMeterRealTimeData: TButton
      Left = 16
      Height = 25
      Top = 189
      Width = 153
      Caption = 'Meter real time data'
      TabOrder = 4
      OnClick = btnInverterInfoClick
    end
    object btnPowerFlowRealTimeData: TButton
      Left = 16
      Height = 25
      Top = 220
      Width = 153
      Caption = 'Powerflow Realtime Dta'
      TabOrder = 5
      OnClick = btnInverterInfoClick
    end
    object btnOhmPIlotRealtime: TButton
      Left = 16
      Height = 25
      Top = 251
      Width = 153
      Caption = 'Ohmpilot Realtime '
      TabOrder = 6
      OnClick = btnInverterInfoClick
    end
    object btnStorageRealTimeData: TButton
      Left = 16
      Height = 25
      Top = 282
      Width = 153
      Caption = 'Storage Realtime data'
      TabOrder = 7
      OnClick = btnInverterInfoClick
    end
    object btnEngerestimate: TButton
      Left = 16
      Height = 25
      Top = 352
      Width = 153
      Caption = 'Start Energy Estimate'
      TabOrder = 8
      OnClick = btnEngerestimateClick
    end
    object btnConnectDB: TButton
      Left = 16
      Height = 25
      Top = 383
      Width = 153
      Caption = 'Connect DB'
      TabOrder = 9
      OnClick = btnConnectDBClick
    end
  end
  object memLog: TMemo
    Left = 185
    Height = 441
    Top = 0
    Width = 439
    Align = alClient
    TabOrder = 1
  end
end
