object dgSpinOptions: TdgSpinOptions
  Left = 192
  Top = 107
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Spin Options'
  ClientHeight = 94
  ClientWidth = 170
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btOk: TButton
    Left = 6
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 90
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object chspoAutoRepeat: TCheckBox
    Left = 24
    Top = 4
    Width = 97
    Height = 17
    Caption = 'AutoRepeat'
    TabOrder = 2
  end
  object chspoAutoIncrement: TCheckBox
    Left = 24
    Top = 23
    Width = 121
    Height = 17
    Caption = 'AutoIncrement'
    TabOrder = 3
  end
  object chspoKeyEdit: TCheckBox
    Left = 24
    Top = 42
    Width = 97
    Height = 17
    Caption = 'KeyEdit'
    TabOrder = 4
  end
end
