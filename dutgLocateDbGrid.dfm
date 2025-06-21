object dgGetDbGrid: TdgGetDbGrid
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Locate Db Grid'
  ClientHeight = 80
  ClientWidth = 308
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
  object Label1: TLabel
    Left = 8
    Top = 6
    Width = 149
    Height = 13
    Caption = 'Select TopGrid 2.20 TtsDbGrid:'
  end
  object cbDbGrids: TComboBox
    Left = 8
    Top = 22
    Width = 287
    Height = 21
    Style = csDropDownList
    DropDownCount = 12
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbDbGridsChange
  end
  object btOk: TButton
    Left = 138
    Top = 52
    Width = 75
    Height = 20
    Caption = '&Ok'
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object btCancel: TButton
    Left = 218
    Top = 52
    Width = 75
    Height = 20
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
