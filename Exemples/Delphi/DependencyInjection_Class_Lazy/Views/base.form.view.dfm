object FormBase: TFormBase
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'FormBase'
  ClientHeight = 511
  ClientWidth = 584
  Color = clSilver
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTitle: TPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelTitle: TLabel
      Left = 0
      Top = 0
      Width = 88
      Height = 23
      Align = alClient
      Caption = 'Form Base'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ButtonClose: TButton
      Left = 561
      Top = 0
      Width = 23
      Height = 23
      Align = alRight
      Caption = 'X'
      ModalResult = 1
      TabOrder = 0
      OnClick = ButtonCloseClick
    end
  end
end
