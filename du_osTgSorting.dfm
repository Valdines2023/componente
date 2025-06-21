object dgTgSort: TdgTgSort
  Left = 210
  Top = 117
  BorderStyle = bsDialog
  Caption = 'TopGrid Sorting and Grouping'
  ClientHeight = 205
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 4
    Top = 5
    Width = 51
    Height = 14
    Caption = 'Grouping'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label9: TLabel
    Left = 4
    Top = 80
    Width = 41
    Height = 15
    Caption = 'Sorting'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btOk: TButton
    Left = 220
    Top = 182
    Width = 75
    Height = 20
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 298
    Top = 182
    Width = 75
    Height = 20
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 4
    Top = 22
    Width = 371
    Height = 55
    BevelOuter = bvNone
    Color = clTeal
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object laGroup1: TLabel
      Left = 20
      Top = 8
      Width = 50
      Height = 14
      Caption = 'Group On:'
    end
    object Label2: TLabel
      Left = 20
      Top = 33
      Width = 44
      Height = 14
      Caption = 'Then On:'
    end
    object Label3: TLabel
      Left = 222
      Top = 8
      Width = 45
      Height = 14
      Caption = 'Segment:'
    end
    object Label4: TLabel
      Left = 222
      Top = 33
      Width = 45
      Height = 14
      Caption = 'Segment:'
    end
    object btClearG1: TSpeedButton
      Left = 2
      Top = 8
      Width = 16
      Height = 16
      Hint = 'Clear'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Glyph.Data = {
        E6010000424DE60100000000000036000000280000000C0000000C0000000100
        180000000000B001000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000
        00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFF00
        0000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
        C0000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        000000000000C0C0C0FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFF
        FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphTop
      ParentFont = False
      Spacing = 0
      OnClick = btClearG1Click
    end
    object btClearG2: TSpeedButton
      Left = 2
      Top = 32
      Width = 16
      Height = 16
      Hint = 'Clear'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Glyph.Data = {
        E6010000424DE60100000000000036000000280000000C0000000C0000000100
        180000000000B001000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000
        00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFF00
        0000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
        C0000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        000000000000C0C0C0FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFF
        FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphTop
      ParentFont = False
      Spacing = 0
      OnClick = btClearG2Click
    end
    object cbGroup1: TComboBox
      Left = 72
      Top = 4
      Width = 145
      Height = 22
      Style = csDropDownList
      DropDownCount = 15
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 0
      OnChange = cbGroup1Change
      OnKeyDown = cbGroup1KeyDown
    end
    object cbGroup2: TComboBox
      Left = 72
      Top = 30
      Width = 145
      Height = 22
      Style = csDropDownList
      DropDownCount = 15
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 2
      OnChange = cbGroup2Change
      OnDropDown = cbGroup2DropDown
      OnKeyDown = cbGroup2KeyDown
    end
    object cbSegment1: TComboBox
      Left = 270
      Top = 4
      Width = 97
      Height = 22
      Style = csDropDownList
      Color = clBtnFace
      DropDownCount = 12
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 1
      OnChange = cbSegment1Change
      Items.Strings = (
        'None'
        'Year'
        'Year Month'
        'Year Quarter'
        'YearMonthDay'
        'Quarter'
        'Month #'
        'Month Name'
        'Half-Month'
        'Week'
        'Day'
        'Dayo of Week')
    end
    object cbSegment2: TComboBox
      Left = 270
      Top = 30
      Width = 97
      Height = 22
      Style = csDropDownList
      Color = clBtnFace
      DropDownCount = 12
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 3
      OnChange = cbSegment2Change
      Items.Strings = (
        'None'
        'Year'
        'Year Month'
        'Year Quarter'
        'YearMonthDay'
        'Quarter'
        'Month #'
        'Month Name'
        'Half-Month'
        'Week'
        'Day'
        'Dayo of Week')
    end
  end
  object Panel2: TPanel
    Left = 4
    Top = 96
    Width = 371
    Height = 83
    BevelOuter = bvNone
    Color = clTeal
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object Label5: TLabel
      Left = 20
      Top = 8
      Width = 40
      Height = 14
      Caption = 'Sort On:'
    end
    object Label6: TLabel
      Left = 20
      Top = 34
      Width = 44
      Height = 14
      Caption = 'Then On:'
    end
    object Label7: TLabel
      Left = 20
      Top = 60
      Width = 44
      Height = 14
      Caption = 'Then On:'
    end
    object btClearS1: TSpeedButton
      Left = 2
      Top = 8
      Width = 16
      Height = 16
      Hint = 'Clear'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Glyph.Data = {
        E6010000424DE60100000000000036000000280000000C0000000C0000000100
        180000000000B001000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000
        00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFF00
        0000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
        C0000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        000000000000C0C0C0FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFF
        FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphTop
      ParentFont = False
      Spacing = 0
      OnClick = btClearS1Click
    end
    object btClearS2: TSpeedButton
      Left = 2
      Top = 34
      Width = 16
      Height = 16
      Hint = 'Clear'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Glyph.Data = {
        E6010000424DE60100000000000036000000280000000C0000000C0000000100
        180000000000B001000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000
        00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFF00
        0000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
        C0000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        000000000000C0C0C0FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFF
        FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphTop
      ParentFont = False
      Spacing = 0
      OnClick = btClearS2Click
    end
    object btClearS3: TSpeedButton
      Left = 2
      Top = 60
      Width = 16
      Height = 16
      Hint = 'Clear'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Glyph.Data = {
        E6010000424DE60100000000000036000000280000000C0000000C0000000100
        180000000000B001000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000
        00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFF00
        0000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
        C0000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        000000000000C0C0C0FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFF
        FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphTop
      ParentFont = False
      Spacing = 0
      OnClick = btClearS3Click
    end
    object cbSort1: TComboBox
      Left = 72
      Top = 4
      Width = 195
      Height = 22
      Style = csDropDownList
      DropDownCount = 15
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 0
      OnChange = cbSort1Change
      OnKeyDown = cbSort1KeyDown
    end
    object cbSort2: TComboBox
      Left = 72
      Top = 30
      Width = 195
      Height = 22
      Style = csDropDownList
      DropDownCount = 15
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 2
      OnChange = cbSort1Change
      OnKeyDown = cbSort1KeyDown
    end
    object cbSort3: TComboBox
      Left = 72
      Top = 56
      Width = 195
      Height = 22
      Style = csDropDownList
      DropDownCount = 15
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 4
      OnChange = cbSort1Change
      OnKeyDown = cbSort1KeyDown
    end
    object chDescending1: TCheckBox
      Left = 274
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Descending'
      TabOrder = 1
      OnClick = chDescending1Click
    end
    object chDescending2: TCheckBox
      Left = 274
      Top = 33
      Width = 97
      Height = 17
      Caption = 'Descending'
      TabOrder = 3
      OnClick = chDescending1Click
    end
    object chDescending3: TCheckBox
      Left = 274
      Top = 58
      Width = 97
      Height = 17
      Caption = 'Descending'
      TabOrder = 5
      OnClick = chDescending1Click
    end
  end
  object btClear: TButton
    Left = 10
    Top = 182
    Width = 75
    Height = 20
    Caption = 'Clear'
    TabOrder = 4
    OnClick = btClearClick
  end
  object chFooters: TCheckBox
    Left = 62
    Top = 4
    Width = 141
    Height = 17
    Caption = 'with subtotals'
    TabOrder = 5
    OnClick = chFootersClick
  end
end
