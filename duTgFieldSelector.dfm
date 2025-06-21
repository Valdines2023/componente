object dgTgFieldSelector: TdgTgFieldSelector
  Left = 293
  Top = 114
  Width = 217
  Height = 279
  BorderIcons = [biSystemMenu]
  Caption = 'Grid Columns'
  Color = clBtnFace
  Constraints.MaxHeight = 480
  Constraints.MinHeight = 150
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDragDrop = Panel1DragDrop
  OnDragOver = FormDragOver
  PixelsPerInch = 96
  TextHeight = 13
  object gdFields: TtsGrid
    Left = 0
    Top = 21
    Width = 209
    Height = 224
    Align = alClient
    AlwaysShowEditor = False
    CheckBoxStyle = stCheck
    ColMoving = False
    Color = clBtnFace
    Cols = 1
    ColSelectMode = csNone
    ExportDelimiter = ','
    FocusColor = clHighlight
    FocusFontColor = clHighlightText
    GridLines = glNone
    GridMode = gmBrowse
    HeadingFont.Charset = DEFAULT_CHARSET
    HeadingFont.Color = clWindowText
    HeadingFont.Height = -11
    HeadingFont.Name = 'MS Sans Serif'
    HeadingFont.Style = []
    HeadingOn = False
    Is3D = True
    PrintTotals = False
    PrintWithGridFormats = False
    ResizeCols = rcNone
    ResizeRows = rrNone
    RowBarOn = False
    Rows = 4
    RowSelectMode = rsNone
    SelectionType = sltColor
    StoreData = True
    TabOrder = 0
    ThumbTracking = True
    Version = '3.01.02'
    VertAlignment = vtaCenter
    XMLExport.Version = '1.0'
    XMLExport.DataPacketVersion = '2.0'
    AutoSizeColumns = True
    OnDragDrop = gdFieldsDragDrop
    OnDragOver = gdFieldsDragOver
    OnEndDrag = gdFieldsEndDrag
    OnMouseDown = gdFieldsMouseDown
    OnMouseMove = gdFieldsMouseMove
    ColProperties = <
      item
        DataCol = 1
        Col.TextFormatting = tfGrid
        Col.Width = 203
      end>
    Data = {0000000000000000}
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 209
    Height = 21
    Align = alTop
    TabOrder = 1
    object laMsg: TLabel
      Left = 4
      Top = 2
      Width = 193
      Height = 17
      AutoSize = False
      Caption = 'Drag and drop columns onto grid...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      OnDragDrop = Panel1DragDrop
      OnDragOver = Panel1DragOver
    end
  end
end
