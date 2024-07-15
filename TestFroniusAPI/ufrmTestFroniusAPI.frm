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
  OnDestroy = FormDestroy
  LCLVersion = '8.0'
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
      Height = 32
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
      OnClick = btnInverterInfoClick
      TabOrder = 1
    end
    object btnInverterInfo: TButton
      Left = 16
      Height = 25
      Top = 127
      Width = 153
      Caption = 'Inverter Info'
      OnClick = btnInverterInfoClick
      TabOrder = 2
    end
    object btnInverterRealTimeData: TButton
      Left = 16
      Height = 25
      Top = 158
      Width = 153
      Caption = 'Inverter real time data'
      OnClick = btnInverterInfoClick
      TabOrder = 3
    end
    object btnMeterRealTimeData: TButton
      Left = 16
      Height = 25
      Top = 189
      Width = 153
      Caption = 'Meter real time data'
      OnClick = btnInverterInfoClick
      TabOrder = 4
    end
    object btnPowerFlowRealTimeData: TButton
      Left = 16
      Height = 25
      Top = 220
      Width = 153
      Caption = 'Powerflow Realtime Dta'
      OnClick = btnInverterInfoClick
      TabOrder = 5
    end
    object btnOhmPIlotRealtime: TButton
      Left = 16
      Height = 25
      Top = 251
      Width = 153
      Caption = 'Ohmpilot Realtime '
      OnClick = btnInverterInfoClick
      TabOrder = 6
    end
    object btnStorageRealTimeData: TButton
      Left = 16
      Height = 25
      Top = 282
      Width = 153
      Caption = 'Storage Realtime data'
      OnClick = btnInverterInfoClick
      TabOrder = 7
    end
    object btnEngerestimate: TButton
      Left = 16
      Height = 25
      Top = 352
      Width = 153
      Caption = 'Start Energy Estimate'
      OnClick = btnEngerestimateClick
      TabOrder = 8
    end
    object btnConnectDB: TButton
      Left = 16
      Height = 25
      Top = 383
      Width = 153
      Caption = 'Connect DB'
      OnClick = btnConnectDBClick
      TabOrder = 9
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
