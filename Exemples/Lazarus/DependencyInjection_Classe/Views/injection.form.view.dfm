inherited FormInjctorBr: TFormInjctorBr
  Caption = ''
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelTitle: TPanel
    inherited LabelTitle: TLabel
      Width = 561
      Caption = 'Uso da Classe COM Dependency Injection'
      ExplicitWidth = 349
    end
  end
  object ButtonEngine: TButton
    Left = 198
    Top = 194
    Width = 181
    Height = 25
    Anchors = []
    Caption = 'Execute Engine ACBr'
    TabOrder = 1
    OnClick = ButtonEngineClick
  end
  object Button1: TButton
    Left = 198
    Top = 256
    Width = 181
    Height = 25
    Anchors = []
    Caption = 'Execute Engine TecnoSpeed'
    TabOrder = 2
    OnClick = Button1Click
  end
end
