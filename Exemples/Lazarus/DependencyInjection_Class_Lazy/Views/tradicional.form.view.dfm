inherited FormTradicional: TFormTradicional
  Caption = 'FormTradicional'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelTitle: TPanel
    inherited LabelTitle: TLabel
      Width = 561
      Caption = 'Uso da Classe SEM Dependency Injection'
      ExplicitWidth = 347
    end
  end
  object ButtonOpen: TButton
    Left = 186
    Top = 197
    Width = 179
    Height = 25
    Anchors = []
    Caption = 'Execute Engine ACBr'
    TabOrder = 1
    OnClick = ButtonOpenClick
  end
  object Button1: TButton
    Left = 186
    Top = 251
    Width = 179
    Height = 25
    Anchors = []
    Caption = 'Execute Engine TecnoSpeed'
    TabOrder = 2
    OnClick = Button1Click
  end
end
