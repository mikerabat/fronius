object frmFroniusAPI: TfrmFroniusAPI
  Left = 0
  Top = 0
  Caption = 'Test Fronius API'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnDestroy = FormDestroy
  TextHeight = 15
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 441
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 440
    object edFroniusHost: TLabeledEdit
      Left = 16
      Top = 48
      Width = 121
      Height = 23
      EditLabel.Width = 84
      EditLabel.Height = 15
      EditLabel.Caption = 'Host or Address'
      TabOrder = 0
      Text = 'fronius'
    end
    object btnVersion: TButton
      Left = 16
      Top = 96
      Width = 153
      Height = 25
      Caption = 'Version'
      TabOrder = 1
      OnClick = btnInverterInfoClick
    end
    object btnInverterInfo: TButton
      Left = 16
      Top = 127
      Width = 153
      Height = 25
      Caption = 'Inverter Info'
      TabOrder = 2
      OnClick = btnInverterInfoClick
    end
    object btnInverterRealTimeData: TButton
      Left = 16
      Top = 158
      Width = 153
      Height = 25
      Caption = 'Inverter real time data'
      TabOrder = 3
      OnClick = btnInverterInfoClick
    end
    object btnMeterRealTimeData: TButton
      Left = 16
      Top = 189
      Width = 153
      Height = 25
      Caption = 'Meter real time data'
      TabOrder = 4
      OnClick = btnInverterInfoClick
    end
    object btnPowerFlowRealTimeData: TButton
      Left = 16
      Top = 220
      Width = 153
      Height = 25
      Caption = 'Powerflow Realtime Data'
      TabOrder = 5
      OnClick = btnInverterInfoClick
    end
    object btnOhmPIlotRealtime: TButton
      Left = 16
      Top = 251
      Width = 153
      Height = 25
      Caption = 'Ohmpilot Realtime '
      TabOrder = 6
      OnClick = btnInverterInfoClick
    end
    object btnStorageRealTimeData: TButton
      Left = 16
      Top = 282
      Width = 153
      Height = 25
      Caption = 'Storage Realtime data'
      TabOrder = 7
      OnClick = btnInverterInfoClick
    end
    object btnEngerestimate: TButton
      Left = 16
      Top = 352
      Width = 153
      Height = 25
      Caption = 'Start Energy Estimate'
      TabOrder = 8
      OnClick = btnEngerestimateClick
    end
    object btnConnectDB: TButton
      Left = 16
      Top = 383
      Width = 153
      Height = 25
      Caption = 'Connect DB'
      TabOrder = 9
      OnClick = btnConnectDBClick
    end
  end
  object memLog: TMemo
    Left = 185
    Top = 0
    Width = 439
    Height = 441
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 435
    ExplicitHeight = 440
  end
end
