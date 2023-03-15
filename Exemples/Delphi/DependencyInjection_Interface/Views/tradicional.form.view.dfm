inherited FormTradicional: TFormTradicional
  Caption = 'FormTradicional'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 44
    Top = 58
    Width = 493
    Height = 51
    Anchors = []
    AutoSize = False
    Caption = 
      'N'#195'O RECOMENDADO: Nessa forma voc'#234' ver'#225' que vai exitir duas depen' +
      'd'#234'ncias a unit da Interface e tamb'#233'm a unit da Classe'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  inherited PanelTitle: TPanel
    inherited LabelTitle: TLabel
      Width = 561
      Caption = 'Uso da Classe SEM Dependency Injection'
      ExplicitWidth = 347
    end
  end
  object ButtonOpen: TButton
    Left = 220
    Top = 233
    Width = 136
    Height = 25
    Anchors = []
    Caption = 'Execute Engine'
    TabOrder = 1
    OnClick = ButtonOpenClick
  end
end
