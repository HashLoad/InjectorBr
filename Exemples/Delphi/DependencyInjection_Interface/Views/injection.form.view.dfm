inherited FormInjctorBr: TFormInjctorBr
  Caption = ''
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 44
    Top = 58
    Width = 493
    Height = 65
    Anchors = []
    AutoSize = False
    Caption = 
      'RECOMENDADO: Nessa forma voc'#234' ver'#225' que vai exitir depend'#234'ncias s' +
      'omente da unit da Interface, essa '#233' a melhor forma de se usar in' +
      'je'#231#227'o de depend'#234'ncia'
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
      Caption = 'Uso da Classe COM Dependency Injection'
      ExplicitWidth = 349
    end
  end
  object ButtonEngine: TButton
    Left = 221
    Top = 240
    Width = 136
    Height = 25
    Anchors = []
    Caption = 'Execute Engine'
    TabOrder = 1
    OnClick = ButtonEngineClick
  end
end
