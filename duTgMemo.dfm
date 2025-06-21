object dgTgMemo: TdgTgMemo
  Left = 420
  Top = 177
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'dgTgMemo'
  ClientHeight = 201
  ClientWidth = 390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object shLeft: TShape
    Left = 0
    Top = 1
    Width = 1
    Height = 199
    Align = alLeft
  end
  object shRight: TShape
    Left = 389
    Top = 1
    Width = 1
    Height = 199
    Align = alRight
  end
  object shTop: TShape
    Left = 0
    Top = 0
    Width = 390
    Height = 1
    Align = alTop
  end
  object shBottom: TShape
    Left = 0
    Top = 200
    Width = 390
    Height = 1
    Align = alBottom
  end
  object Panel2: TPanel
    Left = 1
    Top = 1
    Width = 388
    Height = 199
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 0
    object moText: TMemo
      Left = 1
      Top = 1
      Width = 386
      Height = 174
      Align = alClient
      BorderStyle = bsNone
      Lines.Strings = (
        'moText')
      ScrollBars = ssVertical
      TabOrder = 0
      WantTabs = True
      OnChange = moTextChange
      OnKeyDown = moTextKeyDown
    end
    object Panel1: TPanel
      Left = 1
      Top = 175
      Width = 386
      Height = 23
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      OnResize = Panel1Resize
      object igSizer: TImage
        Left = 367
        Top = 5
        Width = 16
        Height = 16
        Cursor = crSizeNWSE
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          0000100000000100180000000000000300000000000000000000000000000000
          0000D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFF99A8
          AC99A8ACD8E9ECFFFFFF99A8AC99A8ACD8E9ECFFFFFF99A8AC99A8ACD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFF99A8AC99A8ACD8E9ECFFFFFF99A8AC
          99A8ACD8E9ECFFFFFF99A8AC99A8ACD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECFFFFFF99A8AC99A8ACD8E9ECFFFFFF99A8AC99A8ACD8E9ECFFFFFF99A8ACD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFF99A8AC99A8ACD8E9EC
          FFFFFF99A8AC99A8ACD8E9ECFFFFFFD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECFFFFFF99A8AC99A8ACD8E9ECFFFFFF99A8AC99A8ACD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFF99A8AC
          99A8ACD8E9ECFFFFFF99A8AC99A8ACD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFF99A8AC99A8ACD8E9ECFFFFFF99A8ACD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          FFFFFF99A8AC99A8ACD8E9ECFFFFFFD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFF99A8AC99A8ACD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECFFFFFF99A8AC99A8ACD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFF99A8ACD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECFFFFFFD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9EC}
        OnMouseDown = igSizerMouseDown
        OnMouseMove = igSizerMouseMove
      end
      object btOk: TButton
        Left = 128
        Top = 2
        Width = 55
        Height = 20
        Caption = '&Ok'
        Default = True
        TabOrder = 0
        OnClick = btOkClick
      end
      object btCancel: TButton
        Left = 186
        Top = 2
        Width = 55
        Height = 20
        Caption = '&Cancel'
        TabOrder = 1
        OnClick = btCancelClick
      end
    end
  end
end
