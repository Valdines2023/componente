unit osAdvDbGrid;

{$INCLUDE TSCmpVer}

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    Data.DB, Menus,
    Dialogs, StdCtrls, Consts, Grids_ts, TSCommon, TSSetLib, TSGLib,
    TSMask, TSImageList, TSDateTimeDef, ExtCtrls, Registry, osSortLib,
    TsGrid {$IFDEF TSVER_V6}, SQLTimSt, Variants {$ENDIF};

type
  // new enumerated types
  TosAssignedValue  = (avAlignment, avWidth, avVisible, avMaxLength,
                       avControlType, avAllowGrayed);
  TosAssignedValues = set of TosAssignedValue;
  TtsDataEditMode        = (demNone, demEdit, demAppend, demInsert);
  TSQLProc = procedure(Value : TStrings) of object;

  TosGridData = class;
  TosDBCol = class;
  TosAdvDbGrid = class;
  TosCustomAdvDbGrid = class;
  TosDbSortedRow = class;
  TosDbCombo = class;

  TosBookmarkList = class(TObject)
  private
    FList: TStringList;
    FDataset: TDataSet;
    FCache: TBookmark; //TBookmarkStr;
    FCacheIndex: Integer;
    FCacheFind: Boolean;
    FLinkActive: Boolean;
    function GetCount: Integer;
    function GetCurrentRowSelected: Boolean;
    function GetItem(Index: Integer): TBookmarkStr;
    procedure SetCurrentRowSelected(Value: Boolean);
    procedure StringsChanged(Sender: TObject);
  protected
    function CurrentRow: TBookmark; //TBookmarkStr;
    function Compare(const Item1, Item2: TBookmark {TBookmarkStr}): Integer;
    procedure LinkActive(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;           // free all bookmarks
    procedure Delete;          // delete all selected rows from dataset
    function  Find(const Item: TBookmark {TBookmarkStr}; var Index: Integer): Boolean;
    function  IndexOf(const Item: TBookmark {TBookmarkStr}): Integer;
    function  Refresh: Boolean;// drop orphaned bookmarks; True = orphans found
    property Count: Integer read GetCount;
    property CurrentRowSelected: Boolean read GetCurrentRowSelected write SetCurrentRowSelected;
    property Items[Index: Integer]: TBookmarkStr read GetItem; default;
  end;


  TosDbSortedRows = class(TosSortedRows)
  private
    FDataLink : TDataLink;
    oldRows : TStringList;
    FGrid : TosCustomAdvDbGrid;
    FBookmarks : TosBookmarkList;
    FReloadData : Boolean;
    
  protected
    function  GetSortedRow(dataRow : Integer) : TosDbSortedRow;
    function  GetDataSource : TDataSource;
    procedure SetDataSource(Value : TDataSource);
    function  DbRecSortText : String;
    procedure ReloadStoredData;
    procedure Reload(RemapVisible : Boolean); override;
    procedure RemoveDeletedRecords;
    function  BinarySearch(aName : TBookmarkStr) : Integer;
    function  NewGroupHeader(value : String; iGrp , iLevel, iPos: Integer) : TosGroupHeaderRow; override;
    function  AddDataRow(activeRow : Integer; theValue : Variant) : TosDbSortedRow;
  public
    constructor Create; override;
    destructor Destroy; override;

    function  GroupHeaderText(onCol, onRow : Integer) : String; override;
    procedure DoSort; override;
    procedure Clear; override;
    function  RowExists(dataRow : Integer) : Boolean;

    property SortedRow[dataRow : Integer] : TosDbSortedRow read GetSortedRow;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
    property ReloadData : Boolean read FReloadData;
  end;

  TosDbSortedRow = class(TosSortedRow)
  private
    FBookmark : TBookmark;
  protected
  public
    function FieldByName(fieldName : String) : TField;
    function FieldValue(fieldName : String) : Variant; override;
    function FieldValueAsDouble(dataCol : Variant) : Double; override;

    function GroupValue(fieldName : String) : variant; override;
    function GroupText : String; override;

    property Owner;
    property Bookmark : TBookmark read FBookmark write FBookmark;
  end;

  TosMemoOptions = class(TPersistent)
  private
    FGrid  : TosCustomAdvDbGrid;
    function GetEditorOptions: TosMemoEditorOptions;
    function GetScrollBars: TScrollStyle;
    procedure SetEditorOptions(const Value: TosMemoEditorOptions);
    procedure SetScrollBars(const Value: TScrollStyle);
  protected
    function GetEditorShortCut: TShortCut;
    procedure SetEditorShortCut(const Value: TShortCut);
  public
    procedure Assign(srcMemoOptions : TPersistent); override;
  published
    property EditorShortCut : TShortCut read GetEditorShortCut write SetEditorShortCut;
    property EditorOptions : TosMemoEditorOptions read GetEditorOptions write SetEditorOptions;
    property ScrollBars : TScrollStyle read GetScrollBars write SetScrollBars;
  end;

  TosPrintOptions = class(TPersistent)
  private
    FGrid : TosCustomAdvDbGrid;
  protected
    function  GetPrintTitle: String;
    procedure SetPrintTitle(const Value: String);
    function  GetPrintCols: Integer;
    function  GetPrintTotals: Boolean;
    function  GetPrintWithGridFormats: Boolean;
    procedure SetPrintCols(const Value: Integer);
    procedure SetPrintTotals(const Value: Boolean);
    procedure SetPrintWithGridFormats(const Value : Boolean);
  public
    procedure Assign(srcPrintOptions : TPersistent); override;
  published
    property PrintTitle : String read GetPrintTitle write SetPrintTitle;
    property PrintTotals : Boolean read GetPrintTotals write SetPrintTotals default True;
    property PrintCols : Integer read GetPrintCols write SetPrintCols default 0;
    property PrintWithGridFormats : Boolean read GetPrintWithGridFormats write SetPrintWithGridFormats default False;
  end;

  TosEditOptions = class(TPersistent)
  private
    FGrid  : TosCustomAdvDbGrid;
    //FAutoInsert    : Boolean;
    FConfirmDelete : Boolean;
  protected
    function  GetAlwaysShowEditor : Boolean;
    procedure SetAlwaysShowEditor(Value : Boolean) ;
    function  GetAutoInsert : Boolean;
    procedure SetAutoInsert(Value : Boolean);
    function  GetCheckBoxStyle : TtsCheckBoxStyle;
    procedure SetCheckBoxStyle(Value : TtsCheckBoxStyle);
    function  GetCheckBoxValues : String;
    procedure SetCheckBoxValues(Value : String);
    function  GetCheckMouseFocus : Boolean;
    procedure SetCheckMouseFocus(Value : Boolean);
    function  GetConfirmDelete : Boolean;
    procedure SetConfirmDelete(Value : Boolean);
    function  GetDateTimeDef : TtsDateTimeDefComponent;
    procedure SetDateTimeDef(Value : TtsDateTimeDefComponent);
    function  GetEditColor : TColor;
    procedure SetEditColor(Value : TColor);
    function  GetEditFontColor : TColor;
    procedure SetEditFontColor(Value : TColor);
    function  GetSpinButtonHeight : Integer;
    procedure SetSpinButtonHeight(Value : Integer);
    function  GetSpinButtonWidth : Integer;
    procedure SetSpinButtonWidth(Value : Integer);
    function  GetSpinRepeatDelay : Integer;
    procedure SetSpinRepeatDelay(Value : Integer);
    function  GetSpinStartDelay : Integer;
    procedure SetSpinStartDelay(Value : Integer);    
  public
    procedure Assign(srcEditOptions : TPersistent); override;
  published
    property AlwaysShowEditor : Boolean read GetAlwaysShowEditor write SetAlwaysShowEditor default True;
    property AutoInsert : Boolean read GetAutoInsert write SetAutoInsert default True;
    property CheckBoxStyle : TtsCheckBoxStyle read GetCheckBoxStyle write SetCheckBoxStyle default stCheck;
    property CheckBoxValues : String read GetCheckBoxValues write SetCheckBoxValues;
    property CheckMouseFocus : Boolean read GetCheckMouseFocus write SetCheckMouseFocus default True;
    property ConfirmDelete : Boolean read GetConfirmDelete write SetConfirmDelete default False;
    property DateTimeDef : TtsDateTimeDefComponent read GetDateTimeDef write SetDateTimeDef;
    property EditColor : TColor read GetEditColor write SetEditColor default clNone;
    property EditFontColor : TColor read GetEditFontColor write SetEditFontColor default clNone;
    property SpinButtonHeight : Integer read GetSpinButtonHeight write SetSpinButtonHeight default 9;
    property SpinButtonWidth : Integer  read GetSpinButtonWidth write SetSpinButtonWidth default 11;
    property SpinRepeatDelay : Integer read GetSpinRepeatDelay write SetSpinRepeatDelay default 80;
    property SpinStartDelay : Integer  read GetSpinStartDelay write SetSpinStartDelay default 500;
  end;

  TosGridOptions = class(TPersistent)
  private
    FGrid  : TosCustomAdvDbGrid;

  protected
    function  GetHighlightEditRow: Boolean;
    procedure SetHighlightEditRow(const Value: Boolean);  
    function  GetDrawingMode: TosDrawingMode;
    procedure SetDrawingMode(const Value: TosDrawingMode);
    function  GetDefaultButtonHeight: Integer;
    function  GetDefaultButtonWidth: Integer;
    procedure SetDefaultButtonHeight(const Value: Integer);
    procedure SetDefaultButtonWidth(const Value: Integer);  
    function  GetAutoLoadLayout: TosOnOffOption;
    procedure SetAutoLoadLayout(const Value: TosOnOffOption);
    function  GetAutoSaveLayout: TosOnOffOption;
    procedure SetAutoSaveLayout(const Value: TosOnOffOption);
    function  GetProvideGridMenu: Boolean;
    procedure SetProvideGridMenu(const Value: Boolean);
    function  GetShowTextEllipsis: TosTextEllipsis;
    procedure SetShowTextEllipsis(const Value: TosTextEllipsis);  
    function  GetExitGridOnTab: Boolean;
    procedure SetExitGridOnTab(const Value: Boolean);
    function  GetCellPadding: Integer;
    procedure SetCellPadding(const Value: Integer);
    function  GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function  GetGridLines: TtsGridLines;
    procedure SetGridLines(const Value: TtsGridLines);
    function  GetAlwaysShowFocus : Boolean;
    procedure SetAlwaysShowFocus(Value : Boolean);
    function  GetAutoScale : Boolean;
    procedure SetAutoScale(Value : Boolean);
    function  GetButtonEdgeWidth : Integer;
    procedure SetButtonEdgeWidth(Value : Integer);
    function  GetCenterPicture : Boolean;
    procedure SetCenterPicture(Value : Boolean);
    function  GetStretchPicture : Boolean;
    procedure SetStretchPicture(Value : Boolean);
    function  GetShrinkPicture : Boolean;
    procedure SetShrinkPicture(Value : Boolean);
    function  GetKeepAspectRatio : Boolean;
    procedure SetKeepAspectRatio(Value : Boolean);
    function  GetDrawOverlap : TtsDrawOverlap;
    procedure SetDrawOverlap(Value : TtsDrawOverlap);
    function  GetGridMode : TtsGridMode;
    procedure SetGridMode(Value : TtsGridMode);
    function  GetFixedLineColor : TColor;
    procedure SetFixedLineColor(Value : TColor);
    function  GetFlatButtons : Boolean;
    procedure SetFlatButtons(Value : Boolean);
    function  GetFocusBorder : TtsFocusBorder;
    procedure SetFocusBorder(Value : TtsFocusBorder);
    function  GetFocusBorderColor : TColor;
    procedure SetFocusBorderColor(Value : TColor);
    function  GetFocusColor : TColor;
    procedure SetFocusColor(Value : TColor);
    function  GetFocusFontColor : TColor;
    procedure SetFocusFontColor(Value : TColor);
    function  GetIs3D : Boolean;
    procedure SetIs3D(Value : Boolean);    
    function  GetImageList : TtsImageListComponent;
    procedure SetImageList(Value : TtsImageListComponent);
    function  GetInactiveButtonState : TtsInactiveButtonState;
    procedure SetInactiveButtonState(Value : TtsInactiveButtonState);
    function  GetLineColor : TColor;
    procedure SetLineColor(Value : TColor);
    function  GetMaskDefs : TtsMaskDefsComponent;
    procedure SetMaskDefs(Value : TtsMaskDefsComponent);
    function  GetParentColor : Boolean;
    procedure SetParentColor(Value : Boolean);
    function  GetParentFont : Boolean;
    procedure SetParentFont(Value : Boolean);
    function  GetReadOnlyButton : Boolean;
    procedure SetReadOnlyButton(Value : Boolean);
    function  GetTransparentColor : TColor;
    procedure SetTransparentColor(Value : TColor);
    function  GetSkipReadOnly : Boolean;
    procedure SetSkipReadOnly(Value : Boolean);
    function  GetWordWrap : TtsWordWrap;
    procedure SetWordWrap(Value : TtsWordWrap);
    function  GetWantTabs : Boolean;
    procedure SetWantTabs(Value : Boolean);
    function  GetEnterKeyOp : TosEnterKeyOp;
    procedure SetEnterKeyOp(const Value : TosEnterKeyOp);
    function  GetTextFormatting : TosTextFormatting;
    procedure SetTextFormatting(const Value : TosTextFormatting);
    function GetTotalBandHeight: Integer;
    function GetTotalBandOn: Boolean;
    procedure SetTotalBandHeight(const Value: Integer);
    procedure SetTotalBandOn(const Value: Boolean);
    function GetTotalBandColor: TColor;
    procedure SetTotalBandColor(const Value: TColor);    
  public

    procedure Assign(srcGridOptions : TPersistent); override;
  published

    property AlwaysShowFocus : Boolean read GetAlwaysShowFocus write SetAlwaysShowFocus default False;
    property AutoScale : Boolean read GetAutoScale write SetAutoScale default False;
    property AutoLoadLayout : TosOnOffOption read GetAutoLoadLayout write SetAutoLoadLayout default ofDefault;
    property AutoSaveLayout : TosOnOffOption read GetAutoSaveLayout write SetAutoSaveLayout default ofDefault;
    property ButtonEdgeWidth : Integer read GetButtonEdgeWidth write SetButtonEdgeWidth default 2;
    property Color : TColor read GetColor write SetColor;
    property CellPadding : Integer read GetCellPadding write SetCellPadding default 3;
    property DefaultButtonWidth : Integer read GetDefaultButtonWidth write SetDefaultButtonWidth;
    property DefaultButtonHeight : Integer read GetDefaultButtonHeight write SetDefaultButtonHeight;
    property DrawingMode : TosDrawingMode read GetDrawingMode write SetDrawingMode default dmStandard;
    property EnterKeyOp : TosEnterKeyOp read GetEnterKeyOp write SetEnterKeyOp default ekTab;
    property CenterPicture : Boolean read GetCenterPicture write SetCenterPicture default True;
    property StretchPicture : Boolean read GetStretchPicture write SetStretchPicture default True;
    property ShrinkPicture : Boolean read GetShrinkPicture write SetShrinkPicture default True;
    property KeepAspectRatio : Boolean read GetKeepAspectRatio write SetKeepAspectRatio default False;
    property DrawOverlap : TtsDrawOverlap read GetDrawOverlap write SetDrawOverlap default doDrawColOnTop;
    property GridLines : TtsGridLines read GetGridLines write SetGridLines default glBoth;
    property GridMode : TtsGridMode read GetGridMode write SetGridMode default gmEdit;
    property FixedLineColor : TColor read GetFixedLineColor write SetFixedLineColor  default clBlack;
    property FlatButtons : Boolean read GetFlatButtons write SetFlatButtons default True;
    property FocusBorder : TtsFocusBorder read GetFocusBorder write SetFocusBorder default fbDot;
    property FocusBorderColor : TColor read GetFocusBorderColor write SetFocusBorderColor default clNone;
    property FocusColor : TColor read GetFocusColor write SetFocusColor default clNone;
    property FocusFontColor : TColor read GetFocusFontColor write SetFocusFontColor default clNone;
    property HighlightEditRow : Boolean read GetHighlightEditRow write SetHighlightEditRow default False;
    property ImageList : TtsImageListComponent read GetImageList write SetImageList;
    property InactiveButtonState : TtsInactiveButtonState read GetInactiveButtonState write SetInactiveButtonState default ibsBackGround;
    property Is3D : Boolean read GetIs3D write SetIs3D default False;
    property LineColor : TColor read GetLineColor write SetLineColor default clSilver;
    property MaskDefs : TtsMaskDefsComponent read GetMaskDefs write SetMaskDefs;
    property ParentColor : Boolean read GetParentColor write SetParentColor default False;
    property ParentFont : Boolean read GetParentFont write SetParentFont default True;
    property ProvideGridMenu : Boolean read GetProvideGridMenu write SetProvideGridMenu default False;
    property ReadOnlyButton : Boolean read GetReadOnlyButton write SetReadOnlyButton default True;
    property TransparentColor : TColor read GetTransparentColor write SetTransparentColor default clNone;
    property SkipReadOnly : Boolean read GetSkipReadOnly write SetSkipReadOnly default True;
    property WordWrap : TtsWordWrap read GetWordWrap write SetWordWrap default wwDefault;
    property WantTabs : Boolean read GetWantTabs write SetWantTabs default True;
    property TextFormatting : TosTextFormatting read GetTextFormatting write SetTextFormatting default tfDefault;
    property ExitGridOnTab : Boolean read GetExitGridOnTab write SetExitGridOnTab default False;
    property ShowTextEllipsis : TosTextEllipsis read GetShowTextEllipsis write SetShowTextEllipsis default teDefault;
    property TotalBandOn : Boolean read GetTotalBandOn write SetTotalBandOn default False;
    property TotalBandHeight : Integer read GetTotalBandHeight write SetTotalBandHeight default 20;
    property TotalBandColor : TColor read GetTotalBandColor write SetTotalBandColor default clGray;
  end;

  TosSelectionOptions = class(TPersistent)
  private
    FGrid  : TosCustomAdvDbGrid;
  protected
    function  GetCellSelectMode : TtsCellSelectMode;
    procedure SetCellSelectMode(Value : TtsCellSelectMode);
    function  GetColSelectMode : TtsColSelectMode;
    procedure SetColSelectMode(Value : TtsColSelectMode);
    function  GetRowSelectMode : TtsRowSelectMode;
    procedure SetRowSelectMode(Value : TtsRowSelectMode);
    function  GetSelectedAreaCursor : TCursor;
    procedure SetSelectedAreaCursor(Value : TCursor);
    function  GetSelectFixed : Boolean;
    procedure SetSelectFixed(Value : Boolean);
    function  GetSelectionColor : TColor;
    procedure SetSelectionColor(Value : TColor);
    function  GetSelectionType : TtsSelectionType;
    procedure SetSelectionType(Value : TtsSelectionType);
    function GetGradientSelect: TosGradientSelect;
    procedure SetGradientSelect(Value: TosGradientSelect);    
  public
    procedure Assign(srcSelectionOptions : TPersistent); override;
  published
    property CellSelectMode : TtsCellSelectMode read GetCellSelectMode write SetCellSelectMode  default cmRange;
    property ColSelectMode : TtsColSelectMode read GetColSelectMode write SetColSelectMode default csMulti;
    property GradientSelect : TosGradientSelect read GetGradientSelect write SetGradientSelect default gsOff;
    property RowSelectMode : TtsRowSelectMode read GetRowSelectMode write SetRowSelectMode default rsMulti;
    property SelectedAreaCursor : TCursor read GetSelectedAreaCursor write SetSelectedAreaCursor default crDefault;
    property SelectFixed : Boolean read GetSelectFixed write SetSelectFixed default True;
    property SelectionColor : TColor read GetSelectionColor write SetSelectionColor default clHighlight;
    property SelectionType : TtsSelectionType read GetSelectionType write SetSelectionType default sltDefault;
  end;

  TosColumnOptions = class(TPersistent)
  private
    FGrid  : TosCustomAdvDbGrid;

  protected
    function GetDefaultColWidth: Integer;
    procedure SetDefaultColWidth(const Value: Integer);  
    function GetAutoSizeColumns: Boolean;
    procedure SetAutoSizeColumns(const Value: Boolean);
    function  GetColMoving : Boolean;
    procedure SetColMoving(Value : Boolean);
    function  GetFixedColCount : Integer;
    procedure SetFixedColCount(Value : Integer);
    function  GetResizeCols : TtsResizeCols;
    procedure SetResizeCols(Value : TtsResizeCols);
    function  GetResizeColsInGrid : Boolean;
    procedure SetResizeColsInGrid(Value : Boolean);
  public
    constructor Create;

    procedure Assign(srcColumnOptions : TPersistent); override;
  published
    property AutoSizeColumns : Boolean read GetAutoSizeColumns write SetAutoSizeColumns default False;
    property ColMoving : Boolean read GetColMoving write SetColMoving default True;
    property DefaultColWidth : Integer read GetDefaultColWidth write SetDefaultColWidth;
    property FixedColCount : Integer read GetFixedColCount write SetFixedColCount default 0;
    property ResizeCols : TtsResizeCols read GetResizeCols write SetResizeCols default rcSingle;
    property ResizeColsInGrid : Boolean read GetResizeColsInGrid write SetResizeColsInGrid default false;
  end;

  TosRowOptions = class(TPersistent)
  private
    FGrid  : TosCustomAdvDbGrid;
  protected
    function  GetHotTrack: Boolean;
    procedure SetHotTrack(const Value: Boolean);
    function  GetRowNavigation: TosRowNavigation;
    procedure SetRowNavigation(const Value: TosRowNavigation);
    function  GetRowBarDisplay: TosRowBarDisplay;
    procedure SetRowBarDisplay(const Value: TosRowBarDisplay);
    function  GetAltRowColor: TColor;
    procedure SetAltRowColor(const Value: TColor);
    function  GetResizeRowsInGrid: Boolean;
    procedure SetResizeRowsInGrid(const Value: Boolean);
    function  GetRowBarIndicator: Boolean;
    procedure SetRowBarIndicator(const Value: Boolean);
    function  GetVertAlignment: TtsVertAlignment;
    procedure SetVertAlignment(const Value: TtsVertAlignment);
    function  GetDefaultRowHeight: Integer;
    function  GetFixedRowCount: Integer;
    function  GetResizeRows: TtsResizeRows;
    function  GetRowBarAlignment: TtsVertAlignment;
    function  GetRowBarOn: Boolean;
    function  GetRowBarWidth: Integer;
    function  GetRowChangedIndicator: TtsRowChangedIndicator;
    function  GetRowMoving: Boolean;
    function  GetTabRowWrap: Boolean;
    procedure SetDefaultRowHeight(const Value: Integer);
    procedure SetFixedRowCount(const Value: Integer);
    procedure SetResizeRows(const Value: TtsResizeRows);
    procedure SetRowBarAlignment(const Value: TtsVertAlignment);
    procedure SetRowBarOn(const Value: Boolean);
    procedure SetRowBarWidth(const Value: Integer);
    procedure SetRowChangedIndicator(const Value: TtsRowChangedIndicator);
    procedure SetRowMoving(const Value: Boolean);
    procedure SetTabRowWrap(const Value: Boolean);
  public
    procedure Assign(srcRowOptions : TPersistent); override;
  published
    property AltRowColor : TColor read GetAltRowColor write SetAltRowColor default clNone;
    property FixedRowCount : Integer read GetFixedRowCount write SetFixedRowCount default 0;
    property HotTrack : Boolean read GetHotTrack write SetHotTrack default False;
    property ResizeRows : TtsResizeRows read GetResizeRows write SetResizeRows default rrAll;
    property ResizeRowsInGrid : Boolean read GetResizeRowsInGrid write SetResizeRowsInGrid default False;
    property RowBarAlignment : TtsVertAlignment read GetRowBarAlignment write SetRowBarAlignment default vtaDefault;
    property RowBarDisplay : TosRowBarDisplay read GetRowBarDisplay write SetRowBarDisplay default rbdIndicators;
    property RowBarIndicator : Boolean read GetRowBarIndicator write SetRowBarIndicator default True;
    property RowBarOn : Boolean read GetRowBarOn write SetRowBarOn default True;
    property RowBarWidth : Integer read GetRowBarWidth write SetRowBarWidth default 14;
    property RowChangedIndicator : TtsRowChangedIndicator read GetRowChangedIndicator write SetRowChangedIndicator default riOn;
    property RowMoving : Boolean read GetRowMoving write SetRowMoving default True;
    property RowNavigation : TosRowNavigation read GetRowNavigation write SetRowNavigation default rnAll;
    property DefaultRowHeight : Integer read GetDefaultRowHeight write SetDefaultRowHeight default 14;
    property TabRowWrap : Boolean read GetTabRowWrap write SetTabRowWrap default True;
    property VertAlignment : TtsVertAlignment read GetVertAlignment write SetVertAlignment default vtaDefault;
  end;

  TosGroupingSortingOptions = class(TPersistent)
  private
    FGrid  : TosCustomAdvDbGrid;
    function GetGroupFootersOn: Boolean;
    procedure SetGroupFootersOn(const Value: Boolean);
    //FGroupHeaderFormat: TosGroupHeaderFormat;
    //FGroupFooterFrame: TosGroupRowFrame;
    //FGroupHeaderFrame: TosGroupRowFrame;

  protected
    function  GetSortOnHeadingClick: Boolean;
    procedure SetSortOnHeadingClick(const Value: Boolean);
    function  GetShowSummaryOpText: TosSummaryOpText;
    procedure SetShowSummaryOpText(const Value: TosSummaryOpText);
    function  GetGroupFooterFrame: TosGroupRowFrame;
    function  GetGroupHeaderFormat: TosGroupHeaderFormat;
    function  GetGroupHeaderFrame: TosGroupRowFrame;
    procedure SetGroupFooterFrame(const Value: TosGroupRowFrame);
    procedure SetGroupHeaderFormat(const Value: TosGroupHeaderFormat);
    procedure SetGroupHeaderFrame(const Value: TosGroupRowFrame);
    function  GetGroupFont: TFont;
    procedure SetGroupFont(const Value: TFont);
    function  GetGroupColor : TColor;
    procedure SetGroupColor(const Value : TColor);
    function  GetGroupIndent : Integer;
    procedure SetGroupIndent(const Value : Integer);
    function  GetGroupHeaderHeight : Integer;
    procedure SetGroupHeaderHeight(const Value : Integer);
    function  GetGroupFooterHeight : Integer;
    procedure SetGroupFooterHeight(const Value : Integer);
    function  GetSortCaseInsensitive : Boolean;
    procedure SetSortCaseInsensitive(const Value : Boolean);
    function  GetAnsiSort : Boolean;
    procedure SetAnsiSort(const Value : Boolean);
    function  GetGroupColumnsHidden : Boolean;
    procedure SetGroupColumnsHidden(const Value : Boolean);
  public
    procedure Assign(srcGroupingSortingOptions : TPersistent); override;
  published

    property AnsiSort : Boolean read GetAnsiSort write SetAnsiSort;
    property ShowSummaryOpText : TosSummaryOpText read GetShowSummaryOpText write SetShowSummaryOpText default soDefault;
    property SortCaseInsensitive : Boolean read GetSortCaseInsensitive write SetSortCaseInsensitive default True;
    property SortOnHeadingClick : Boolean read GetSortOnHeadingClick write SetSortOnHeadingClick default True;
    property GroupFont : TFont read GetGroupFont write SetGroupFont;
    property GroupColor : TColor read GetGroupColor write SetGroupColor default clSilver;
    property GroupIndent : Integer read GetGroupIndent write SetGroupIndent default 15;
    property GroupHeaderHeight : Integer read GetGroupHeaderHeight write SetGroupHeaderHeight default 20;
    property GroupFooterHeight : Integer read GetGroupFooterHeight write SetGroupFooterHeight default 20;
    property GroupHeaderFrame : TosGroupRowFrame read GetGroupHeaderFrame write SetGroupHeaderFrame default gfRaised;
    property GroupFooterFrame : TosGroupRowFrame read GetGroupFooterFrame write SetGroupFooterFrame default gfSunken;
    property GroupHeaderFormat : TosGroupHeaderFormat read GetGroupHeaderFormat write SetGroupHeaderFormat default hfName;
    property GroupColumnsHidden : Boolean read GetGroupColumnsHidden write SetGroupColumnsHidden default True;
    property GroupFootersOn : Boolean read GetGroupFootersOn write SetGroupFootersOn;
  end;

  TosScrollingOptions = class(TPersistent)
  private
    FGrid  : TosCustomAdvDbGrid;
  protected    
    function GetScrollBars: TScrollStyle;
    function GetScrollSpeed: TtsScrollSpeed;
    function GetThumbTracking: Boolean;
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetScrollSpeed(const Value: TtsScrollSpeed);
    procedure SetThumbTracking(const Value: Boolean);
  public
    procedure Assign(srcScrollingOptions : TPersistent); override;
  published
    property ScrollBars : TScrollStyle read GetScrollBars write SetScrollBars default ssBoth;
    property ScrollSpeed : TtsScrollSpeed read GetScrollSpeed write SetScrollSpeed default spVariable;
    property ThumbTracking : Boolean read GetThumbTracking write SetThumbTracking default False;
  end;

  TosHeadingOptions = class(TPersistent)
  private
    FGrid  : TosAdvDbGrid;
    function GetHeadingImageAlignment: TtsHorzAlignment;
    procedure SetHeadingImageAlignment(const Value: TtsHorzAlignment);
  protected
    function  GetIs3D : Boolean;
    procedure SetIs3D(Value : Boolean);
    function  GetButton : TtsHeadingButton;
    procedure SetButton(Value : TtsHeadingButton);
    function  GetColor : TColor;
    procedure SetColor(Value : TColor);
    function  GetFont : TFont;
    procedure SetFont(Value : TFont);
    function  GetHeight : Integer;
    procedure SetHeight(Value : Integer);
    function  GetHorzAlignment : TtsHorzAlignment;
    procedure SetHorzAlignment(Value : TtsHorzAlignment);
    function  GetVisible : Boolean;
    procedure SetVisible(Value : Boolean);
    function  GetParentFont : Boolean;
    procedure SetParentFont(Value : Boolean);
    function  GetVertAlignment : TtsVertAlignment;
    procedure SetVertAlignment(Value : TtsVertAlignment);
    function  GetWordWrap : TtsWordWrap;
    procedure SetWordWrap(Value : TtsWordWrap);
    function  GetProvideHeadingMenu : Boolean;
    procedure SetProvideHeadingMenu(const Value : Boolean);    
  public
    constructor Create;
    procedure Assign(srcHeadingOptions : TPersistent); override;
  published
    property Is3D : Boolean read GetIs3D write SetIs3D default True;
    property Button : TtsHeadingButton read GetButton write SetButton default hbCell;
    property Color : TColor read GetColor write SetColor default clNone;
    property Font : TFont read GetFont write SetFont;
    property Height : Integer read GetHeight write SetHeight default 15;
    property HeadingImageAlignment : TtsHorzAlignment read GetHeadingImageAlignment write SetHeadingImageAlignment default htaDefault; 
    property HorzAlignment : TtsHorzAlignment read GetHorzAlignment write SetHorzAlignment default htaDefault;
    property Visible : Boolean read GetVisible write SetVisible default True;
    property ParentFont : Boolean read GetParentFont write SetParentFont default True;
    property ProvideHeadingMenu : Boolean read GetProvideHeadingMenu write SetProvideHeadingMenu default false;
    property VertAlignment : TtsVertAlignment read GetVertAlignment write SetVertAlignment default vtaDefault;
    property WordWrap : TtsWordWrap read GetWordWrap write SetWordWrap default wwDefault;
  end;

  TosPanelOptions = class(TPersistent)
  private
    FGrid   : TosAdvDbGrid;
    FHeight : Integer;
    FColor  : TColor;
    FVisible : Boolean;
    FFont  : TFont;
    FBevelInner : TBevelCut;
    FBevelOuter : TBevelCut;
  protected
    procedure SetHeight(Value : Integer);
    procedure SetColor(Value : TColor);
    procedure SetVisible(Value : Boolean);
    procedure SetFont(Value : TFont);
    procedure SetBevelInner(Value : TBevelCut);
    procedure SetBevelOuter(Value : TBevelCut);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property BevelInner: TBevelCut read FBevelInner write SetBevelInner default bvLowered;
    property BevelOuter: TBevelCut read FBevelOuter write SetBevelOuter default bvRaised;  
    property Height: Integer read FHeight write SetHeight default 0;
    property Color: TColor read FColor write SetColor default clRed;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write SetVisible stored True;
  end;

  TosGrouperPanelOptions = class(TosPanelOptions)
  private
  protected
  public
  published
  end;

  TosFooterPanelOptions = class(TosPanelOptions)
  private
  protected
  public
  published
  end;



  {TosGridData}
  TosGridData = class(TObject)
  private
    FSortedData : TosDbSortedRows;
    FDataSet : TDataSet;
    FGrid : TosCustomAdvDbGrid;
    FFieldLayout: TList;
    FActiveRow : Integer;
    FOldRow    : Integer;
    FReapplyFooters : Boolean;
    FInsertRow : TosDbSortedRow;

    procedure CheckDatalinkBufferOk(OldSize: Integer);
    procedure SetDatasetBufferCount(BufSize: Integer);
  protected
    function  CheckForPost(dataRow : Integer) : Boolean;
    procedure ResetFieldLayout;
    procedure SaveFieldLayout;
    function  FieldLayoutChanged: Boolean;
    function  FieldOrderChanged: Boolean;
    function  ReverseChar(aChar : Char) : Char;

    function  GetActiveRecord: Integer;
    procedure SetActiveRecord(Value : Integer);
    function  GetBufferRows: Integer;
    procedure DataSetScrolled(Distance : Integer);
    procedure SetActiveRow(Value : Integer);
    function  GetActiveBookmark : TBookmark; //TBookmarkStr;
    procedure SetActiveBookmark(Value : TBookmark {TBookmarkStr});
    procedure SetDataSet(Value : TDataSet);
 
    procedure LinkActive(value : Boolean);
    function  LPadZero(aString : String) : String;
    function  ReverseText(aString : String) : String;
    function  SortString(aField : TField) : String;
    procedure ReseqUp(fromActiveRow : Integer);
    procedure ReseqDown(fromActiveRow, toActiveRow : Integer);

    function GetSortedRow(dataRow : Integer) : TosDbSortedRow;
    function GetRecordCount : Integer;
    function GetCellField(dataCol : Variant; dataRow : Integer) : TField;
    function GetCellValue(dataCol : Variant; dataRow : Integer) : Variant;    
    function GetBookmark(dataRow : Integer) : TBookmarkStr;
    function GetRecordRow(DataRow : Integer) : Integer;
    function GetDataLink : TDataLink;
    procedure ResetRowProperties;
    procedure ResetRowCount;
    procedure RecalcFooterTotals;
    function  AppendCurrentRecord : TosDbSortedRow;
  public
    constructor Create;
    destructor Destroy; override;

    function  DataSetRows : String;
    function  SortedDataRows : String;
    procedure PositionOnDataRow(DataRow : Integer);
    function  GridDataRowForActiveRow(actRow : Integer) : Integer;
    procedure SelectDataRow(dataRow : Integer);
    //function  PositionOnBookmark(Value : TBookmarkStr) : Boolean;
    procedure ClearSortFields;
    procedure RemoveSort(aFieldName : String);
    procedure AddSortField(colName : String; dataCol : Integer; aSortType : TosSortType);
    procedure AddGroupField(colName : String; dataCol : Integer);

    function  CheckBookmarks : Boolean;
    function  RecordCountsDiffer : Boolean;
    procedure Refresh;
    procedure Clear;
    procedure ClearSorts;
    procedure ToggleSortOnCol(theCol : Variant);
    //procedure AddSortOnCol(theCol : Variant; sortMode : TosSortType);
    procedure AddGroupFooters;
    procedure RemoveGroupFooters;
    procedure RemoveRow(dataRow : Integer);

    procedure MoveFirst;
    procedure MoveLast;
    procedure MoveRow(fromRow, toRow : Integer);

    property ActiveRow: Integer read FActiveRow write SetActiveRow;
    property ActiveBookmark : TBookmark {TBookmarkStr} read GetActiveBookmark write SetActiveBookmark;
    property ActiveRecord : Integer read GetActiveRecord write SetActiveRecord;
    property Bookmark[dataRow : Integer] : TBookmarkStr read GetBookmark;
    property RecordRow[DataRow : Integer] : Integer read GetRecordRow;
    property BufferRows : Integer read GetBufferRows;
    property SortedRow[dataRow : Integer] : TosDbSortedRow read GetSortedRow;

    property DataLink : TDataLink read GetDataLink;
    property DataSet: TDataSet read FDataSet write SetDataSet;

    property CellValue[dataCol : Variant; dataRow : Integer] : Variant read GetCellValue;
    property CellField[dataCol : Variant; dataRow : Integer] : TField read GetCellField;
        
    property RecordCount : Integer read GetRecordCount;
    property SortedData : TosDbSortedRows read FSortedData;
  end;


  {TosFieldLayout}
  {Class for saving layout properties of TField objects}

  TosFieldLayout = class(TObject)
  protected
      FFieldName: string;
      FDisplayLabel: string;
      FDisplayWidth: Integer;
      FReadOnly: Boolean;
      FVisible: Boolean;
      FDisplayValues: string;

      constructor Create;

      procedure AssignFieldLayout(Field: TField);
      function  FieldLayoutEqual(Field: TField): Boolean;

      property FieldName: string read FFieldName write FFieldName;
      property DisplayLabel: string read FDisplayLabel write FDisplayLabel;
      property DisplayWidth: Integer read FDisplayWidth write FDisplayWidth;
      property ReadOnly: Boolean read FReadOnly write FReadOnly;
      property Visible: Boolean read FVisible write FVisible;
      property DisplayValues: string read FDisplayValues write FDisplayValues;
  end;


  {TosDBField}
  {Base field class for column fields in TosCustomAdvDbGrid. This class corresponds
   to the TField class for the TosAdbDbGrid component with.}

  TosDBField = class(TPersistent)
  protected
      FGrid: TosCustomAdvDbGrid;
      FCol: TosDBCol;
      FDatasetField: TField;
      FLookupOffset: Integer;

      function  IsEditField: Boolean;
      function  FieldEditOk: Boolean;
      function  GetDataSize: Word;
      function  GetSize: Word;
      procedure SetSize(Value: Word);
      procedure ClearLookup;
      function  GetLookupData(Buffer: Pointer): Boolean;
      procedure SetLookupData(Buffer: Pointer);
      procedure GetLookupValue;
      function  GetData(Buffer: Pointer): Boolean;
      function  GetText(DisplayText: Boolean): string; virtual;
      function  GetAsBoolean: Boolean; virtual;
      function  GetAsCurrency: Currency; virtual;
      function  GetAsDateTime: TDateTime; virtual;
      function  GetAsFloat: Double; virtual;
      function  GetAsInteger: Longint; virtual;
      function  GetAsString: string; virtual;
      function  GetAsVariant: Variant; virtual;
      function  IsBooleanField: Boolean; virtual;
      function  IsIntegerField: Boolean; virtual;

      procedure SetAsBoolean(Value: Boolean); virtual;
      procedure SetAsCurrency(Value: Currency); virtual;
      procedure SetAsDateTime(Value: TDateTime); virtual;
      procedure SetAsFloat(Value: Double); virtual;
      procedure SetAsInteger(Value: Longint); virtual;
      procedure SetAsString(const Value: string); virtual;
      procedure SetAsVariant(const Value: Variant); virtual;
      procedure SetLookupVarValue(Value: Variant); virtual;
      procedure SetLookupValue(const Value: Variant); virtual;
      function  SetField(RecBuf: PChar; Value: string): Boolean; virtual;

      function  GetDisplayText: string; virtual;
      function  GetFieldClass: TClass;
      function  GetDisplayLabel: string;
      procedure SetDisplayLabel(Value: string);
      function  GetDisplayName: string;
      function  GetFieldName: string;
      function  GetReadOnly: Boolean;
      procedure SetReadOnly(Value: Boolean);
      function  GetAlignment: TAlignment;
      procedure SetAlignment(Value: TAlignment);
      function  GetFieldNo: Integer;
      function  GetDataType: TFieldType;
      function  GetDisplayFormat: string; virtual;
      function  GetEditFormat: string; virtual;
      function  GetVisible: Boolean;
      procedure SetVisible(Value: Boolean);
      function  GetDisplayWidth: Integer;
      procedure SetDisplayWidth(Value: Integer);
      function  CanModify: Boolean;
      function  IsBlobField: Boolean;
      procedure SetEditText(Value: string); virtual;
      function  GetEditText: string; virtual;
      function  GetTransliterate: Boolean; virtual;
      procedure SetTransliterate(Value: Boolean); virtual;
      function  GetEditMask: string;
      procedure SetEditMask(Value: string);
      function  GetControlType: TtsControlType; virtual;
      function  GetMaxLength: Integer; virtual;
      function  GetIsNull: Boolean;
      function  GetLookup: Boolean;

      property FieldClass: TClass read GetFieldClass;
      property DisplayLabel: string read GetDisplayLabel write SetDisplayLabel;
      property DisplayName: string read GetDisplayName;
      property FieldName: string read GetFieldName;
      property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
      property Alignment: TAlignment read GetAlignment write SetAlignment;
      property FieldNo: Integer read GetFieldNo;
      property DataType: TFieldType read GetDataType;
      property DisplayFormat: string read GetDisplayFormat;
      property EditFormat: string read GetEditFormat;
      property Visible: Boolean read GetVisible write SetVisible;
      property DisplayWidth: Integer read GetDisplayWidth write SetDisplayWidth;
      property Transliterate: Boolean read GetTransliterate write SetTransliterate;
      property EditMask: string read GetEditMask write SetEditMask;
      property DataSize: Word read GetDataSize;
      property Size: Word read GetSize write SetSize;
      property ControlType: TtsControlType read GetControlType;
      property MaxLength: Integer read GetMaxLength;
      property Lookup: Boolean read GetLookup;
      property LookupValue: Variant read GetAsVariant write SetLookupValue;

  public
      constructor Create(Grid: TosCustomAdvDbGrid; Col: TosDBCol; Field: TField); virtual;
      destructor Destroy; override;

      property Text: string read GetEditText write SetEditText;
      property DisplayText: string read GetDisplayText;
      property AsString: string read GetAsString write SetAsString;
      property Value: Variant read GetAsVariant write SetAsVariant;
      property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
      property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
      property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
      property AsFloat: Double read GetAsFloat write SetAsFloat;
      property AsInteger: Longint read GetAsInteger write SetAsInteger;
      property AsVariant: Variant read GetAsVariant write SetAsVariant;
      property IsNull: Boolean read GetIsNull;

      property Col: TosDBCol read FCol;
      property DatasetField: TField read FDatasetField;
  end;

  {TosDBCol}
  {Individual column for TosCustomAdvDGrid}

  TosDBCol = class(TtsCol)
  private
      FtempDateTime : TDateTime;
  protected
      FDBField: TosDBField;
      FAssignedValues: TosAssignedValues;

      procedure KeepAssignedValues; override;
      function  GetComboSQL: TStrings;
      procedure SetComboSQL(const Value: TStrings);      
      procedure SetFieldName(Value: string); override;
      procedure SetDatasetField(Value: TField);
      function  GetDatasetField: TField;
      function  SetField(Value: TField): Boolean;
      procedure ResetField(ResetFieldName: Boolean);
      function  GetDBGrid: TosCustomAdvDbGrid;
      function  DefaultWidth: Integer;
      procedure SetDefaultWidth;
      procedure SetDefaultVisible;
      procedure SetDefaultControlType;
      procedure SetDefaultMaxLength;
      procedure SetDefaultAllowGrayed;
      procedure InitField;

      function  GetDisplayFormat : String; override;

      function  DefaultProps: Boolean; override;
      procedure AssignProperties(Source: TtsCol); override;
      function  GetHeading: string; override;
      function  GetAlignment: TAlignment; override;
      function  GetHorzAlignment: TtsHorzAlignment; override;
      function  GetReadOnly: Boolean; override;
      procedure SetAlignment(Value: TAlignment); override;
      procedure SetHorzAlignment(Value: TtsHorzAlignment); override;
      procedure SetWidth(Value: Integer); override;
      procedure SetVisible(Value: Boolean); override;
      procedure SetMaxLength(Value: Integer); override;
      procedure SetControlType(Value: TtsControlType); override;
      procedure SetAllowGrayed(Value: Boolean); override;
      procedure SetDBCombo(Value: TosDbCombo);
      function  GetDBCombo: TosDbCombo;
      procedure SetComboDataSource(Value: TDatasource);
      function  GetComboDataSource: TDatasource;
      function  UseCheckBoxValues: Boolean; override;
      procedure WriteAssignedValues(Writer: TWriter);
      procedure ReadAssignedValues(Reader: TReader);
      procedure DefineProperties(Filer: TFiler); override;
      function  GetDisplayText : String;
      function  GetGroupText : String; override;
      function  GetVarType : Word; override;
      function  GetDataType : TosDataType; override;
      
      property Grid: TosCustomAdvDbGrid read GetDBGrid;

  public
      constructor Create(Grid: TtsBaseGrid); override;
      destructor  Destroy; override;

      // Values
      function  HalfMonth : String;
      function  Month : String;
      function  MonthNumber : String;
      function  MonthYear : String;
      function  YearQuarter : String;
      function  Quarter : String;
      function  DayOfWeekText : String;
      function  WeekText : String;

      function  UseDisplayFormat : String; override;
      function  ColumnSortName : String;
      //procedure AssignCombo;
      //procedure ResetCombo;
      procedure Assign(Source: TPersistent); override;
      function  Lookup: Boolean;
      procedure Reset(Properties: TtsProperties); override;
      procedure ResetAlignment;
      procedure ResetAllowGrayed;
      procedure ResetControlType;
      procedure ResetMaxLength;
      procedure ResetVisible;
      procedure ResetWidth;

      property Combo: TosDbCombo read GetDBCombo write SetDBCombo;
      property DatasetField: TField read GetDatasetField write SetDatasetField;
      property Field: TosDBField read FDBField;
      property DisplayText : String read GetDisplayText;
      property GroupText : String read GetGroupText;
      property VarType : Word read GetVarType;

  published
      property ComboDatasource: TDatasource read GetComboDataSource write SetComboDataSource;
      property DisplayFormat;
      property ComboSQL : TStrings read GetComboSQL write SetComboSQL;
  end;

  TosDBGridCols = class(TtsGridCols)
  protected
      FFieldState: TtsFieldState;

      function  GetDbCol(DataCol: Integer): TosDBCol;
      function  GetDbGrid: TosCustomAdvDbGrid;
      procedure SetFieldState(Value: TtsFieldState);
      function  FindField(Field: TField): TosDBCol;
      function  CreateCol: TtsCol; override;
  public

      procedure Assign(srcCols : TtsGridCols); override;

      property Grid: TosCustomAdvDbGrid read GetDbGrid;
      property Col[DataCol: Integer]: TosDBCol read GetDbCol; default;
      property FieldState: TtsFieldState read FFieldState write SetFieldState;
  end;

  TosGridDataLink = class(TDataLink)
  protected
      FGrid: TosCustomAdvDbGrid;
      FFieldCount: Integer;
      FFieldMapSize: Integer;
      FFieldMap: Pointer;
      FInUpdateData: Boolean;
      FSparseMap: Boolean;
      FStates : TStringList;
      FLastState : TDataSetState;
      FLastCount : Integer;

      function  GetFields(DataCol: Longint): TField;
      procedure ActiveChanged; override;
      procedure CheckBrowseMode; override;
      procedure DataSetChanged; override;
      procedure DataSetScrolled(Distance: Integer); override;
      procedure EditingChanged; override;
      procedure LayoutChanged; override;
      procedure RecordChanged(Field: TField); override;
      procedure UpdateData; override;
      function  GetMappedIndex(DataCol: Longint): Integer;

  public
      constructor Create(Grid: TosCustomAdvDbGrid);
      destructor  Destroy; override;

      function  AddMapping(const FieldName: string; Field: TField): Boolean;
      procedure ClearMapping;
      procedure Reset;

      property  FieldCount: Integer read FFieldCount;
      property  Fields[DataCol: Longint]: TField read GetFields;
      property  SparseMap: Boolean read FSparseMap write FSparseMap;
  end;

  {TosFieldList}
  {Class for maintaining a list of all fields currently in the dataset
   attached to the grid. These fields can, but do not have to be attached to
   a column.}

  TosFieldList = class(TList)
  protected
      FGrid: TosCustomAdvDbGrid;

      procedure Reset;
      function  CreateField(Value: TField): TosDBField;
      procedure CreateFields;
      function  FindField(FieldName: string): TosDBField;
      function  FieldIndex(Field: TField): Integer;

      property Grid: TosCustomAdvDbGrid read FGrid;
  public
      constructor Create(Grid: TosCustomAdvDbGrid);
      destructor  Destroy; override;
  end;

  TosCustomAdvDbGrid = class(TtsCustomGrid)
  private
    FGridData : TosGridData;
    FComboQuery : TDataSet;
    FFieldList: TosFieldList;
    FDataLink : TosGridDataLink;
    FDataSource : TDataSource;
    FAutoInsert        : Boolean;
    FActive            : Boolean;
    FLinkActive        : Boolean;
    FDataModified      : Boolean;
    //FLayoutFromDataset : Boolean;
    FInFieldLayout     : Boolean;
    FInSyncDataset     : Integer;
    FInDatasetEvent    : Integer;
    FInDoCellLoaded    : Boolean;
    FUpdateDataDone    : Boolean;
    FCellLoadBitmap    : TBitmap;
    FInComboInit       : Boolean;
    FEditState         : TDatasetState;
    FEditing           : Boolean;
    FClosingEdit       : Boolean;
    FOpeningEdit       : Boolean;
    FRecordChangedDone : Boolean;
    FCheckBrowseModeDone: Boolean;
    FDataEditMode      : TtsDataEditMode;
    FInsertingDataRow  : Integer;
    FDataBound         : Boolean;
    FSettingRowCount   : Boolean;
    FCurrentRowBookmark : TBookmark; //TBookmarkStr;
    FLoadFirstRow      : Boolean;
    FLastRowLoaded     : Integer;

  protected
    procedure OnGroupingSortingDialogClick(Sender : TObject);
    procedure OpenGroupingSortingDialog; override;

    function  IsSameRow(Rownr1, Rownr2 : Integer; CheckRownr : Boolean) : Boolean;
    function  GetGridRowCount : Longint; override;
    procedure SetGridRowCount(Value: Longint); override;
    property  GridRows: Longint read GetGridRowCount write SetGridRowCount;

    // Editing...
    function  StartCellEdit: Boolean; override;
    function  StartRowEdit: Boolean; override;
    function  EndCellEdit: Boolean; override;
    function  EndRowEdit: Boolean; override;
    function  UndoRowEdit(ByUser: Boolean; RowEditing: Boolean): Boolean; override;
    procedure PostData(Cancelled : Boolean);
    procedure StartInsert;
    procedure EndInsert;
    procedure StartAppend;
    procedure EndAppend;
    procedure EndEditing;
    function  GetInSyncDataset: Boolean;
    procedure SetInSyncDataset(Value: Boolean);
    function  GetInDatasetEvent: Boolean;
    procedure SetInDatasetEvent(Value: Boolean);
    procedure SetDisplayRownr(DataRow: Longint; Value: Longint); override;
    function  GetCellValue(DataCol: Longint; DataRow: Longint): Variant; override;
    procedure SetCellValue(DataCol: Longint; DataRow: Longint; const Value: Variant); override;
    procedure SetFieldValue(Col: TosDBCol; ControlType: TtsControlType; const Value: Variant);
    procedure SetComboValue(Value: Variant); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ComponentRemoved(AComponent: TComponent);
    procedure ComponentInserted(AComponent: TComponent);
    property  DataLink: TosGridDataLink read FDataLink;
    function  GetDataset: TDataset;
    function  MaxDisplayRows: Integer;
    function  StoreRows: Boolean; override;
    procedure AdjustTopLeft(ACol, ARow: Longint; DoPaint: Boolean);
    procedure MoveToPosition(Position: TtsDataPosition; ByUser: Boolean);
    procedure CheckRowPosition;
    function  CtrlKeyDownVK_Delete: Boolean;
    function  KeyDownVK_Insert: Boolean;
    procedure KeyDownVK_Down; override;
    procedure ProcessKeyDown(var Key: Word; Shift: TShiftState); override;

    // Combo
    function  CreateCombo: TtsCombo; override;
    function  CreateComboGrid: TtsBaseGrid; override;
    procedure ComboInit(DataCol, DataRow: Longint); override;
    procedure InitComboData(CellHasCombo: Boolean); override;
    function  GetCombo: TtsCombo; override;
    function  GiveCellCombo(DataCol, DataRow: Longint): TosDbCombo; 
    function  GetCol(DataCol: Variant): TtsCol; override;

    function  GetDataValue(DataCol, DataRow: Longint; ControlType: TtsControlType): Variant; override;
    procedure CellLoadedEvent(DataCol, DataRow: Longint; ControlType: TtsControlType; var Value: Variant); override;
    procedure GetFieldValue(Col: TosDBCol; Rownr: Longint; CheckRownr: Boolean; ControlType: TtsControlType; var Value: Variant);
    procedure DBCellLoaded(DataCol: Longint; DataRow: Longint;
                                       CheckRownr: Boolean;
                                       ControlType: TtsControlType; var Value: Variant);
    // Events...
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;                                       

    procedure DoCellChanged(OldDataCol, NewDataCol: Longint; OldDataRow, NewDataRow: Variant); override;
    procedure DoRowChanged(OldDataRow, NewDataRow: Variant); override;
    procedure UpdateRowCount(DoUpdate: Boolean);
    function  CreateCols : TtsGridCols; override;
    procedure SetDataSource(Value: TDataSource);
    procedure LinkActive(Value : Boolean); virtual;
    procedure InitActiveFields;
    procedure Loaded; override;
    procedure CheckBrowseMode; virtual;
    procedure DataSetScrolled(Distance: Integer); virtual;
    procedure DataSetChanged; virtual;
    procedure InternalDataSetChanged; override;
    procedure UpdateData;  virtual;
    procedure EditingChanged; virtual;
    procedure RecordChanged(Field: TField); virtual;
    procedure ResetRowSelection;

    function DoRowProps(Ancestor: TPersistent): Boolean; override;
    function DoCellProps(Ancestor: TPersistent): Boolean; override;

    {Column and row change procedures}
    procedure SingleRowHeightChanged(DisplayRow: Longint); override;
    procedure ChangeAllRowHeights(NewHeight: integer; SetNewTop: Boolean); override;
    procedure CheckTopLeftChanged(ByUser: Boolean); override;
    procedure TopLeftChangedEvent(OldCol, OldRow, NewCol, NewRow: Longint;
                                  ByUser: Boolean); override;
    function  CheckRowChanged(var OldRow: Variant): Boolean; override;
    procedure ActivateCellChanged(OldCol: Integer; OldRow: Variant); override;
    procedure CheckColSizes; override;
    procedure RowInserted(DataRow: Longint; ByUser: Boolean); override;
    procedure RowDeleted(DataRow: Longint; ByUser: Boolean); override;
    procedure UpdateRowSelection(dataRow : Integer; selected : Boolean); override;

    function  GetDBCol(DataCol: Variant): TosDBCol;
    function  GetFieldState : TtsFieldState;
    procedure SetFieldState(Value : TtsFieldState);
    procedure SetSortCaseInsensitive(Value : Boolean); override;
    function  GetGridDbCols : TosDBGridCols;
    function  GetSortEntry(index : Integer) : TosSortEntry; override;
    function  InternalRowForDataRow(dataRow : Longint) : Integer; override;
    function  GetGroupHeader(dataRow : Integer) : TosGroupHeaderRow;
    function  GetGroupFooter(dataRow : Integer) : TosGroupFooterRow;

    procedure ResetFields(ResetFieldName: Boolean);
    procedure FieldLayoutChanged(SetFields, ClearAll, CreateFields: Boolean);
    procedure DefineFieldMap;
    procedure DoFieldLayoutChange;

    procedure AddGridMenuItems(aPopupMenu : TPopupMenu); override;

    procedure Paint; override;
    procedure DoPaintCell(DataCol, DataRow: Longint; DrawRect:TRect;
                          State: TtsPaintCellState; var Cancel: Boolean); override;
    procedure DoGetDrawInfo(DataCol, DataRow: Longint; var DrawInfo: TtsDrawInfo); override;

    // properties...
    property Active: Boolean read FActive;
    property FieldState: TtsFieldState read GetFieldState write SetFieldState default fsDefault;
    property GridCols: TosDBGridCols read GetGridDbCols;
    property Rows: Longint read GetGridRowCount write SetGridRowCount stored StoreRows;

    property InSyncDataset: Boolean read GetInSyncDataset write SetInSyncDataset;
    property InDatasetEvent: Boolean read GetInDatasetEvent write SetInDatasetEvent;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AssignColsFromDbGrid(aDbGrid : TtsBaseGrid);
    procedure RecalcGrandTotal; override;

    procedure ResetVisiblePointers; override;
    function  RowType(DisplayRow : Longint) : TosRowType; override;
    function  RowIsGroupHeader(DisplayRow : Longint) : Boolean; virtual;
    function  RowIsGroupFooter(DisplayRow : Longint) : Boolean; virtual;
    function  GroupNumber(DisplayRow : Longint) : Integer; override;
    function  GetSortedRow(DisplayRow : Longint) : TosSortedRow; override;
    property  GroupHeader[dataRow : Integer] : TosGroupHeaderRow read GetGroupHeader;
    property  GroupFooter[dataRow : Integer] : TosGroupFooterRow read GetGroupFooter;
    function  GroupHeaderCount : Integer; override;
    function  GroupFooterCount : Integer; override;

    procedure ReGroupAndSort; override;
    procedure ClearGroups; override;
    procedure ClearSorts; override;
    procedure ClearAll; override;
    procedure ApplyGroupingSorting; override;
    procedure LoadGridLayout; override;

    procedure AddGroupFooters; override;
    procedure RemoveGroupFooters; override;
    procedure ContractGroup(theGroupNumber, onRow : Integer); override;
    procedure ExpandGroup(theGroupNumber, onRow : Integer); override;
    procedure RecalcSubTotals; override;
    function  CellValueAsFloat(dataCol, dataRow : Integer) : Double; override;

    procedure InsertRow(DisplayRow: Longint); override;
    procedure DeleteRows(FromDataRow, ToDataRow: Longint); override;
    procedure DeleteSelectedRows; override;

    procedure MoveBy(Count: Longint);
    procedure MoveFirst;
    procedure MoveLast;

    function  GroupFieldCount : Integer; override;
    function  SortFieldCount : Integer; override;
    function  GroupSortCount : Integer; override;
    procedure RemoveSort(DataCol: Variant); override;
    procedure GroupOnCol(DataCol: Variant; bClear : Boolean = False; dtsDateTimeSegment : TosDateTimeSegment = dgoNone); override;
    procedure SortOnCol(DataCol: Variant; sortType : TosSortType = stAscending; bClear : Boolean = False); override;
    function  LocateColumn(columnName : String) : TosDbCol;
    function  ReadOnly : Boolean; virtual;
    function  CurrentBookMark : TBookmark; //TBookmarkStr;
    function  BookMarkForRow(dataRow : Integer) : TBookmark; //TBookmarkStr;
    function  RowForBookMark(bkMark : TBookmarkStr) : Integer;
    function  RawTextValue(dataCol, dataRow : Integer) : String; override;
    function  GroupHeaderText(dataCol, dataRow : Integer) : String; override;

    property Editing: Boolean read FEditing;
    property DataSet : TDataSet read GetDataSet;
    property Col[DataCol: Variant]: TosDBCol read GetDBCol;
    property Combo : TtsCombo read GetCombo;
    property Cell[DataCol: Longint; DataRow: Longint]: Variant read GetCellValue write SetCellValue;

    property GridData : TosGridData read FGridData;
    property Databound : Boolean read FDatabound;

  published
    property DataSource : TDataSource read FDataSource write SetDataSource;
    property ComboQuery : TDataSet read FComboQuery write FComboQuery;

    property Ctl3D;
    property ParentCtl3D;
  end;

  TosAdvDbGrid = class(TosCustomAdvDbGrid)
  private
    //FGrouperPanelOptions : TosGrouperPanelOptions;
    //FFooterPanelOptions  : TosFooterPanelOptions;
    FHeadingOptions : TosHeadingOptions;
    FEditOptions : TosEditOptions;
    FColumnOptions : TosColumnOptions;
    FSelectionOptions : TosSelectionOptions;
    FGridOptions : TosGridOptions;
    FScrollingOptions : TosScrollingOptions;
    FGroupingSortingOptions : TosGroupingSortingOptions;
    FRowOptions : TosRowOptions;
    FPrintOptions : TosPrintOptions;
    FMemoOptions : TosMemoOptions;

  protected
    procedure Loaded; override;
    //procedure SetGrouperPanelOptions(Value : TosGrouperPanelOptions);
    //procedure SetFooterPanelOptions(Value : TosFooterPanelOptions);
    procedure SetHeadingOptions(Value : TosHeadingOptions);
    procedure SetEditOptions(Value : TosEditOptions);
    procedure SetColumnOptions(Value : TosColumnOptions);
    procedure SetSelectionOptions(Value : TosSelectionOptions);
    procedure SetGridOptions(Value : TosGridOptions);
    procedure SetScrollingOptions(Value : TosScrollingOptions);
    procedure SetRowOptions(Value : TosRowOptions);
    procedure SetGroupingSortingOptions(Value : TosGroupingSortingOptions);
    procedure SetPrintOptions(Value : TosPrintOptions);
    procedure SetMemoOptions(Value : TosMemoOptions);

    function  GetCopyAdvDbGrid : TosAdvDbGrid;
    procedure SetCopyAdvDbGrid(aValue : TosAdvDbGrid);
    procedure AdjustedGrouperPanel;
    procedure AdjustedFooterPanel;
    procedure OpenGroupingSortingDialog; override;
   // procedure AdjustedHeadingOptions;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;

    property Canvas;
    property Cell;
    property CellTag;
    property CellData;
    property CellAlign;
    property CellAlignment;
    property CellButtonType;
    property CellDateTimeDef;
    property CellCenterPicture;
    property CellColor;
    property CellCheckBoxState;
    property CellCombo;
    property CellControlType;
    property CellDropDownStyle;
    property CellEditing;
    property CellFont;
    property CellHorzAlignment;
    property CellIs3D;
    property CellKeepAspectRatio;
    property CellMaskName;
    property CellParentCombo;
    property CellParentFont;
    property CellPText;
    property CellReadOnly;
    property CellShrinkPicture;
    property CellSpinIncrement;
    property CellSpinOptions;
    property CellStretchPicture;
    property CellTextHeight;
    property CellTextWidth;
    property CellTextLines;
    property CellVertAlignment;
    property CellWordWrap;
    property CheckBoxGrayedBitmap;
    property CheckBoxOffBitmap;
    property CheckBoxOnBitmap;
    property Col;
    property Combo;
    property CurrentCell;
    property CurrentDataCol;
    property CurrentDataRow;
    property DataColnr;
    property DataRownr;
    property DisplayColnr;
    property DisplayRownr;
    property EditMode;
    property EnablePaint;
    property EnableRedraw;
    property FastAssign;
    property GridData;
    property GridHeight;
    property GridStatus;
    property GridWidth;
    property HeadingAlignment;
    property HeadingTextHeight;
    property HeadingTextLines;
    property HorzScrollBarHeight;
    property HorzScrollBarVisible;
    property IdDataCol;
    property IdDataRow;
    property InsertionRow;
    property LeftCol;
    property MaxLeftCol;
    property MaxTopRow;
    property MouseStatus;
    property RowAlign;
    property RowAlignment;
    property RowButtonType;
    property RowDateTimeDef;
    property RowCenterPicture;
    property RowChanged;
    property RowColor;
    property RowCombo;
    property RowControlType;
    property RowDropDownStyle;
    property RowEditing;
    property RowFont;
    property RowHeight;
    property RowHorzAlignment;
    property RowId;
    property RowIs3D;
    property RowKeepAspectRatio;
    property RowMaskName;
    property RowParentCombo;
    property RowParentFont;
    property RowReadOnly;
    property RowSelected;
    property RowShrinkPicture;
    property RowSpinIncrement;
    property RowSpinOptions;
    property RowStretchPicture;
    property RowVertAlignment;
    property RowVisible;
    property RowWordWrap;
    property SelectedCells;
    property SelectedCols;
    property SelectedRows;
    property SortEntry;
    property TopRow;
    property VertScrollBarVisible;
    property VertScrollBarWidth;
    property VisibleCols;
    property VisibleRows;
  published
    property CopyAdvDbGrid : TosAdvDbGrid read GetCopyAdvDbGrid write SetCopyAdvDbGrid;

    property Align;
    property Anchors;
    property Height;
    property Width;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupBorders;
    property StoreData;
    property Ctl3D;

    property ExportDelimiter;

    property FieldState;
    property ImageList;
    property Rows;
    property Cols;
    property HorzAlignment;
    property Constraints;
    property LayoutVersion;
    property Version;

    property DataSource;    
    property DateTimeDef;
    property MaskDefs;
    property GridReport;
    property LayoutManager;
    property XMLExport;

    property GridOptions : TosGridOptions read FGridOptions write SetGridOptions;
    property ColumnOptions: TosColumnOptions read FColumnOptions write SetColumnOptions;
    property EditOptions: TosEditOptions read FEditOptions write SetEditOptions;
    property MemoOptions : TosMemoOptions read FMemoOptions write SetMemoOptions;
    //property GrouperPanelOptions: TosGrouperPanelOptions read FGrouperPanelOptions write SetGrouperPanelOptions;
    //property FooterPanelOptions: TosFooterPanelOptions read FFooterPanelOptions write SetFooterPanelOptions;
    property HeadingOptions: TosHeadingOptions read FHeadingOptions write SetHeadingOptions;
    property SelectionOptions : TosSelectionOptions read FSelectionOptions write SetSelectionOptions;
    property ScrollingOptions : TosScrollingOptions read FScrollingOptions write SetScrollingOptions;
    property RowOptions : TosRowOptions read FRowOptions write SetRowOptions;
    property GroupingSortingOptions : TosGroupingSortingOptions read FGroupingSortingOptions write SetGroupingSortingOptions;
    property PrintOptions : TosPrintOptions read FPrintOptions write SetPrintOptions;

    property OnGrouping;
    property OnSorting;

    property OnButtonClick;
    property OnButtonDown;
    property OnButtonUp;
    property OnDateTimeDropDown;
    property OnDateTimeGetValue;
    property OnDateTimeInit;
    property OnDateTimeRollUp;
    property OnCanStartDrag;
    property OnCellChanged;
    property OnCellEdit;
    property OnCellLoaded;
    property OnClick;
    property OnClickCell;
    {$IFDEF TSVER_V5}
    property OnContextPopup;
    {$ENDIF}
    property OnColChanged;
    property OnColCountChanged;
    property OnColMoved;
    property OnColResized;
    property OnComboCellLoaded;
    property OnComboCompareValue;
    property OnComboLCompareValue;
    property OnComboDropDown;
    property OnComboGetValue;
    property OnComboInit;
    property OnComboRollUp;
    property OnDblClick;
    property OnDblClickCell;
    property OnDeleteCol;
    property OnDeleteRow;
    property OnDragDrop;
    property OnDragOver;
    property OnEditTextResized;
    property OnEndCellEdit;
    {$IFDEF TSVER_V4}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnEndRowEdit;
    property OnEnter;
    property OnExit;
    property OnGetDrawInfo;
    property OnGridStatusChanged;
    property OnHeadingClick;
    property OnHeadingDown;
    property OnHeadingUp;
    property OnInsertCol;
    property OnInsertRow;
    property OnInvalidMaskValue;
    property OnInvalidMaskEdit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseStatusChanged;
    property OnMouseUp;
    {$IFDEF TSVER_V4}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}
    property OnPrintRow;
    property OnPrintCell;
    property OnPaint;
    property OnPaintCell;
    property OnResize;
    property OnRowChanged;
    property OnRowCountChanged;
    property OnRowLoaded;
    property OnRowMoved;
    property OnRowResized;
    property OnSelectChanged;
    property OnShowEditor;
    property OnSpinButtonClick;
    property OnSpinButtonDown;
    property OnSpinButtonUp;
    property OnSpinIncrement;
    property OnSpinRepeat;
    property OnStartCellEdit;
    {$IFDEF TSVER_V4}
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;
    property OnStartRowEdit;
    property OnTopLeftChanged;
    property OnUndoCellEdit;
    property OnUndoRowEdit;

    property OnPrintGrid;
    property OnLoadLayout;
    property OnSaveLayout;
    property OnSortCompare;
  end;

  TosDbComboGrid = class;
  TosDbCombo = class(TtsCombo)
  private
    FDataSource : TDataSource;
    FRefreshComboData : Boolean;
    FSQL : TStrings;
  protected
    procedure SetSQL(Value : TStrings);
    procedure SetDataSource(Value : TDataSource);
    function  CanOpenSQL : Boolean;
    function  OpenSQL(bClose : Boolean) : TDataSet;
    function GetComboGrid : TosDbComboGrid;
    procedure RefreshData; override;
  public
    constructor Create(onParentGrid: TtsBaseGrid);
    destructor  Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure InitCombo; override;
    procedure ReloadWithSQL(sSQL : String);
    function  DisplayText(forLookupValue : Variant) : String; override;

    property DataSource : TDataSource read FDataSource write SetDataSource;
    property RefreshComboData : Boolean read FRefreshComboData write FRefreshComboData;
    property SQL : TStrings read FSQL write SetSQL;
    property ParentGrid;
    property ComboGrid : TosDbComboGrid read GetComboGrid;
  end;

  TosDBComboGrid = class(TosCustomAdvDbGrid)
  protected
      FGrid: TtsBaseGrid;
      FCombo: TtsCombo;
      FParentGrid: TtsBaseGrid;
      FDropDownRows: Integer;
      FDropDownCols: Integer;
      FValueCol: Longint;
      FValueColSorted: Boolean;
      FCompareType: TtsComboCompareType;
      FAutoSearch: TtsComboAutoSearchType;
      FAutoFill: Boolean;
      FAutoFillConvertCase: TtsConvertCase;
      FDropDownStyle: TtsDropDownStyle;
      FAutoLookup: Boolean;

      function  GetDropDownRows: Longint; override;
      function  GetDropDownCols: Longint; override;
      function  GetValueCol: Longint; override;
      function  GetValueColSorted: Boolean; override;
      function  GetCompareType: TtsComboCompareType; override;
      function  GetAutoSearch: TtsComboAutoSearchType; override;
      function  GetAutoFill: Boolean; override;
      function  GetAutoFillConvertCase: TtsConvertCase;
      function  GetAutoLookup: Boolean; override;
      function  GetDropDownStyle: TtsDropDownStyle; override;
      function  GetGrid: TosAdvDbGrid;
      procedure SetDropDownRows(Value: Longint); override;
      procedure SetDropDownCols(Value: Longint); override;
      procedure SetValueCol(Value: Longint); override;
      procedure SetValueColSorted(Value: Boolean); override;
      procedure SetCompareType(Value: TtsComboCompareType); override;
      procedure SetAutoSearch(value: TtsComboAutoSearchType); override;
      procedure SetAutoFill(Value: Boolean); override;
      procedure SetAutoFillConvertCase(Value: TtsConvertCase);
      procedure SetAutoLookup(Value: Boolean); override;
      procedure SetDropDownStyle(Value: TtsDropDownStyle); override;
      function  GetParentGrid: TtsBaseGrid; override;
      procedure SetParentGrid(Value: TtsBaseGrid); override;
      function  GetParentGridCombo: TtsCombo; override;
      procedure SetParentGridCombo(Value: TtsCombo); override;

  public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property Cell;
      property CellTag;
      property CellData;
      property DataRownr;

  published
      property AutoFill: Boolean read GetAutoFill write SetAutoFill default False;
      property AutoFillConvertCase: TtsConvertCase read GetAutoFillConvertCase write SetAutoFillConvertCase default afcOnEdit;
      property AutoLookup read GetAutoLookup write SetAutoLookup default False;
      property AutoSearch: TtsComboAutoSearchType read GetAutoSearch write SetAutoSearch default asNone;
      property CompareType: TtsComboCompareType read GetCompareType write SetCompareType default ctCaseInsensitive;
      property DropDownRows;
      property DropDownCols;
      property DropDownStyle: TtsDropDownStyle read GetDropDownStyle write SetDropDownStyle default ddDropDown;
      property ValueCol: Longint read GetValueCol write SetValueCol default 1;
      property ValueColSorted: Boolean read GetValueColSorted write SetValueColSorted default False;

      property AutoScale;
      property BorderStyle;
      property ButtonEdgeWidth;
      property DateTimeDef;
      property CellSelectMode;
      property CenterPicture;
      property CheckBoxStyle;
      property CheckBoxValues;
      property CheckMouseFocus;
      property ColMoving;
      property Color;
      property Cols;
      property ColSelectMode;
      {$IFDEF TSVER_V4}
      property Constraints;
      {$ENDIF}
      property Ctl3D;
      property DefaultButtonHeight;
      property DefaultButtonWidth;
      property DefaultColWidth;
      property DefaultRowHeight;
      property DragCursor;
      {$IFDEF TSVER_V4}
      property DragKind;
      {$ENDIF}
      property DragMode;
      property DrawOverlap;
      property EditColor;
      property EditFontColor;
      property Enabled;
      property ExportDelimiter;
      property FixedColCount;
      property FixedLineColor;
      property FixedRowCount;
      property FieldState;
      property FocusBorder;
      property FocusColor;
      property FocusBorderColor;
      property FocusFontColor;
      property FlatButtons;
      property Font;
      property GridLines;
      property GridMode;
      property Heading3D;
      property HeadingHorzAlignment;
      property HeadingButton;
      property HeadingColor;
      property HeadingFont;
      property HeadingHeight;
      property HeadingOn;
      property HeadingParentFont;
      property HeadingVertAlignment;
      property HeadingWordWrap;
      property HorzAlignment;
      property ImageList;
      property InactiveButtonState;
      property Is3D;
      property KeepAspectRatio;
      property LineColor;
      property MaskDefs;
      property ParentColor;
      property ParentCtl3D;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property PrintTitle;
      property PrintTotals;
      property PrintCols;
      property PrintWithGridFormats;
      property ProvideGridMenu;
      property AlwaysDetectButton;
      property ProvideHeadingMenu;
      property AltRowColor;
      property RowNavigation;
      property RowBarDisplay;
      property ShowSummaryOpText;
      property HeadingImageAlignment;
      property GridReport;
      property PrintLinesPerPage;
      property PrintOrientation;
      property ReadOnlyButton;
      property ResizeCols;
      property ResizeColsInGrid;
      property ResizeRows;
      property ResizeRowsInGrid;
      property RowBarAlignment;
      property RowBarIndicator;
      property RowBarOn;
      property RowBarWidth;
      property RowChangedIndicator;
      property RowMoving;
      property Rows;
      property RowSelectMode;
      property ScrollBars;
      property ScrollSpeed;
      property SelectionColor;
      property SelectionFontColor;
      property SelectionType;
      property SelectedAreaCursor;
      property SelectFixed;
      property ShowHint;
      property ShrinkPicture;
      property SkipReadOnly;
      property SpinButtonHeight;
      property SpinButtonWidth;
      property SpinRepeatDelay;
      property SpinStartDelay;
      property StoreData;
      property StretchPicture;
      property TabOrder;
      property TabRowWrap;
      property ThumbTracking;
      property TransparentColor;
      property Version;
      property VertAlignment;
      property Visible;
      property WantTabs;
      property WordWrap;
      property XMLExport;

      property AutoSizeColumns;
      property CellPadding;

      property TextFormatting;
      property EnterKeyOp;
      property AnsiSort;
      property SortCaseInsensitive;
      property GroupColor;
      property GroupIndent;
      property GroupHeaderHeight;
      property GroupFooterHeight;
      property GroupFont;
      property GroupHeaderFrame;
      property GroupFooterFrame;
      property GroupHeaderFormat;

      property ExitGridOnTab;
      property ShowTextEllipsis;
      property HotTrack;

      property MemoEditorShortCut;
      property MemoEditorOptions;
      property PopupBorders;
      property MemoScrollbars;
     
  end;  


function CompareDbItems(Item1, Item2: Pointer): Integer;

var theSortedRows : TosDbSortedRows;

implementation

uses du_osTgSorting, Contnrs;

{$R *.dcr}


function CompareDbItems(Item1, Item2: Pointer): Integer;
var var1, var2 : variant;
    i : Integer;
    sortEntry : TosSortEntry;

  function GridCompareText(str1, str2 : String) : Integer;
  begin
    if TtsCustomGrid(theSortedRows.FGrid).DoSortCompare(str1, str2, Result) then
       exit;

    if (TtsCustomGrid(theSortedRows.FGrid).SortCaseInsensitive) then
    begin
      if (TtsCustomGrid(theSortedRows.FGrid).AnsiSort) then
         Result := AnsiCompareText(str1, str2)
      else
         result := CompareText(str1, str2);
    end
    else
    begin
      if (TtsCustomGrid(theSortedRows.FGrid).AnsiSort) then
         Result := AnsiCompareStr(str1, str2)
      else
         result := CompareStr(str1, str2);
    end;
  end;
  function CheckForLookupValue(onSortEntry : TosSortEntry; var lkupValue : Variant) : Boolean;
  var theCol : TosDbCol;
  begin
    Result := False;
    theCol := TosAdvDbGrid(theSortedRows.FGrid).Col[onSortEntry.Column];
    if (theCol <> Nil) and
       (theCol.ButtonType = btCombo) and
       (theCol.Combo <> Nil) and
       (theCol.Combo.AutoLookup) then
    begin
      Result := True;
      lkupValue := theCol.Combo.DisplayText(lkupValue);
    end;
  end;
  function HeaderFooterAdjustment : Integer;
  begin
    Result := 0;
    if (TosDbSortedRow(Item1).RowType = rtGroupHeader) and
       (TosDbSortedRow(Item2).RowType <> rtGroupHeader) then
       Result := -1
    else if (TosDbSortedRow(Item1).RowType <> rtGroupHeader) and
            (TosDbSortedRow(Item2).RowType = rtGroupHeader) then
       Result := 1
    else if (TosDbSortedRow(Item1).RowType = rtGroupFooter) and
            (TosDbSortedRow(Item2).RowType <> rtGroupFooter) then
       Result := 1
    else if (TosDbSortedRow(Item1).RowType <> rtGroupFooter) and
            (TosDbSortedRow(Item2).RowType = rtGroupFooter) then
       Result := -1;
  end;
begin
  Result := 0;
  if (Item1 = Item2) then exit;
  
  // Item1 - Item2 comparison...
  Result := TosDbSortedRow(Item1).SortPositionValue - TosDbSortedRow(Item2).SortPositionValue;
  if (Result = 0) and
     (TosDbSortedRow(Item1).GroupNumber > 0) then
     Result := TosDbSortedRow(Item1).GroupLevel - TosDbSortedRow(Item2).GroupLevel;
  if (Result = 0) and
     ((TosDbSortedRow(Item1).RowType <> rtData) or
      (TosDbSortedRow(Item2).RowType <> rtData)) then
     Result := HeaderFooterAdjustment;
  if Result <> 0 then exit;
  
  for i := 0 to theSortedRows.SortFields.Count - 1 do
  begin
    sortEntry := theSortedRows.SortFields.SortEntry[i];
    if (sortEntry.GroupSort) then
    begin
      var1 := TosDbSortedRow(Item1).GroupValue(sortEntry.FieldName);
      var2 := TosDbSortedRow(Item2).GroupValue(sortEntry.FieldName);
    end
    else
    begin
      var1 := TosDbSortedRow(Item1).FieldValue(sortEntry.FieldName);
      var2 := TosDbSortedRow(Item2).FieldValue(sortEntry.FieldName);
    end;
    CheckForLookupValue(sortEntry, var1);
    CheckForLookupValue(sortEntry, var2); 

    if (VarToStr(var1) = '') and (VarToStr(var2) = '') then
       Result := HeaderFooterAdjustment
    else if (VarType(var1) = varNull) then
       Result := -1
    else if (VarType(var2) = varNull) then
       Result := 1
    else
      case sortEntry.DataType of
        dyBoolean : Result := var1 - var2;
        dyDate    : Result := var1 - var2;
        dyString  : Result := GridCompareText(VarToStr(var1), VarToStr(var2));
        dyInteger :
           Result := VarAsType(var1, varInteger) - VarAsType(var2, varInteger);
        dyFloat, dyCurrency :
           Result := VarAsType(var1, varDouble) - VarAsType(var2, varDouble);
      end;
    if (Result <> 0) then
    begin
      if (sortEntry.SortMode = stDescending) then
         Result := 0 - Result;
      break;
    end;
  end;
  if (Result = 0) then
     Result := HeaderFooterAdjustment;
end;

const
    MaxMapSize = (MaxInt div 2) div SizeOf(Integer);
    SFieldValueError = 'Field Value Error';

constructor TosPanelOptions.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FBevelInner := bvLowered;
  FBevelOuter := bvRaised;
  FHeight := 0;
  Color := clRed;
end;

destructor TosPanelOptions.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TosPanelOptions.SetHeight(Value : Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    if Self is TosGrouperPanelOptions then
       FGrid.AdjustedGrouperPanel
    else
       FGrid.AdjustedFooterPanel;
  end;
end;

procedure TosPanelOptions.SetColor(Value : TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    if Self is TosGrouperPanelOptions then
       FGrid.AdjustedGrouperPanel
    else
       FGrid.AdjustedFooterPanel;
  end;
end;

procedure TosPanelOptions.SetVisible(Value : Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    if Self is TosGrouperPanelOptions then
       FGrid.AdjustedGrouperPanel
    else
       FGrid.AdjustedFooterPanel;
  end; 
end;

procedure TosPanelOptions.SetFont(Value : TFont);
begin
  if Value <> FFont then
  begin
    FFont.Assign(Value);
    if Self is TosGrouperPanelOptions then
       FGrid.AdjustedGrouperPanel
    else
       FGrid.AdjustedFooterPanel;
  end;
end;

procedure TosPanelOptions.SetBevelInner(Value : TBevelCut);
begin
  if Value <> FBevelInner then
  begin
    FBevelInner := Value;
    if Self is TosGrouperPanelOptions then
       FGrid.AdjustedGrouperPanel
    else
       FGrid.AdjustedFooterPanel;
  end;
end;

procedure TosPanelOptions.SetBevelOuter(Value : TBevelCut);
begin
  if Value <> FBevelOuter then
  begin
    FBevelOuter := Value;
    if Self is TosGrouperPanelOptions then
       FGrid.AdjustedGrouperPanel
    else
       FGrid.AdjustedFooterPanel;
  end;
end;

 { TosEditOptions }

function  TosEditOptions.GetAlwaysShowEditor : Boolean;
begin
  Result := FGrid.AlwaysShowEditor;
end;
procedure TosEditOptions.SetAlwaysShowEditor(Value : Boolean) ;
begin
  FGrid.AlwaysShowEditor := Value;
end;
function  TosEditOptions.GetAutoInsert : Boolean;
begin
  Result := FGrid.FAutoInsert;
end;
procedure TosEditOptions.SetAutoInsert(Value : Boolean);
begin
  FGrid.FAutoInsert := Value;
end;
function  TosEditOptions.GetCheckBoxStyle : TtsCheckBoxStyle;
begin
  Result := FGrid.CheckBoxStyle;
end;
procedure TosEditOptions.SetCheckBoxStyle(Value : TtsCheckBoxStyle);
begin
  FGrid.CheckBoxStyle := Value;
end;
function  TosEditOptions.GetCheckBoxValues : String;
begin
  Result := FGrid.CheckBoxValues;
end;
procedure TosEditOptions.SetCheckBoxValues(Value : String);
begin
  FGrid.CheckBoxValues := Value;
end;
function  TosEditOptions.GetCheckMouseFocus : Boolean;
begin
  Result := FGrid.CheckMouseFocus;
end;
procedure TosEditOptions.SetCheckMouseFocus(Value : Boolean);
begin
  FGrid.CheckMouseFocus := Value;
end;
function  TosEditOptions.GetConfirmDelete : Boolean;
begin
  Result := FConfirmDelete;
end;
procedure TosEditOptions.SetConfirmDelete(Value : Boolean);
begin
  FConfirmDelete := Value;
end;
function  TosEditOptions.GetDateTimeDef : TtsDateTimeDefComponent;
begin
  Result := FGrid.DateTimeDef;
end;
procedure TosEditOptions.SetDateTimeDef(Value : TtsDateTimeDefComponent);
begin
  FGrid.DateTimeDef := Value;
end;
function  TosEditOptions.GetEditColor : TColor;
begin
  Result := FGrid.EditColor;
end;
procedure TosEditOptions.SetEditColor(Value : TColor);
begin
  FGrid.EditColor := Value;
end;
function  TosEditOptions.GetEditFontColor : TColor;
begin
  Result := FGrid.EditFontColor;
end;
procedure TosEditOptions.SetEditFontColor(Value : TColor);
begin
  FGrid.EditFontColor := Value;
end;
function  TosEditOptions.GetSpinButtonHeight : Integer;
begin
  Result := FGrid.SpinButtonHeight
end;
procedure TosEditOptions.SetSpinButtonHeight(Value : Integer);
begin
  FGrid.SpinButtonHeight := Value;
end;
function  TosEditOptions.GetSpinButtonWidth : Integer;
begin
  Result := FGrid.SpinButtonWidth;
end;
procedure TosEditOptions.SetSpinButtonWidth(Value : Integer);
begin
  FGrid.SpinButtonWidth := Value;
end;
function  TosEditOptions.GetSpinRepeatDelay : Integer;
begin
  Result := FGrid.SpinRepeatDelay;
end;
procedure TosEditOptions.SetSpinRepeatDelay(Value : Integer);
begin
  FGrid.SpinRepeatDelay := Value;
end;
function  TosEditOptions.GetSpinStartDelay : Integer;
begin
  Result := FGrid.SpinStartDelay;
end;
procedure TosEditOptions.SetSpinStartDelay(Value : Integer);
begin
  FGrid.SpinStartDelay := Value;
end;

procedure TosEditOptions.Assign(srcEditOptions: TPersistent);
begin
  with TosEditOptions(srcEditOptions) do
  begin
    Self.AlwaysShowEditor := AlwaysShowEditor;
    Self.CheckBoxStyle    := CheckBoxStyle;
    Self.CheckBoxValues   := GetCheckBoxValues;
    Self.CheckMouseFocus  :=CheckMouseFocus;
    Self.ConfirmDelete    := ConfirmDelete;
    Self.DateTimeDef      := DateTimeDef;
    Self.EditColor        := EditColor;
    Self.EditFontColor    := EditFontColor;
    Self.SpinButtonHeight := SpinButtonHeight;
    Self.SpinButtonWidth  := SpinButtonWidth;
    Self.SpinRepeatDelay  := SpinRepeatDelay;
    Self.SpinStartDelay   := SpinStartDelay;
  end;
end;

{ TosColumnOptions }

function  TosColumnOptions.GetColMoving : Boolean;
begin
  Result := FGrid.ColMoving;
end;

procedure TosColumnOptions.SetColMoving(Value : Boolean);
begin
  FGrid.ColMoving := Value;
end;

function  TosColumnOptions.GetFixedColCount : Integer;
begin
  Result := FGrid.FixedColCount;
end;

procedure TosColumnOptions.SetFixedColCount(Value : Integer);
begin
  FGrid.FixedColCount := Value;
end;

function  TosColumnOptions.GetResizeCols : TtsResizeCols;
begin
  Result := FGrid.ResizeCols;
end;

procedure TosColumnOptions.SetResizeCols(Value : TtsResizeCols);
begin
  FGrid.ResizeCols := Value;
end;

function  TosColumnOptions.GetResizeColsInGrid : Boolean;
begin
  Result := FGrid.ResizeColsInGrid;
end;

procedure TosColumnOptions.SetResizeColsInGrid(Value : Boolean);
begin
  FGrid.ResizeColsInGrid := Value;
end;

constructor TosColumnOptions.Create;
begin
  inherited Create;
end;

procedure TosColumnOptions.Assign(srcColumnOptions : TPersistent);
begin
  {Self.ColMoving        := TosColumnOptions(srcColumnOptions).ColMoving;
  Self.FixedColCount    := TosColumnOptions(srcColumnOptions).ColMoving;
  Self.ResizeCols       := TosColumnOptions(srcColumnOptions).ColMoving;
  Self.ResizeColsInGrid := TosColumnOptions(srcColumnOptions).ColMoving;
  Self.AutoSizeColumns  := TosColumnOptions(srcColumnOptions).ColMoving; }
end;

function TosColumnOptions.GetAutoSizeColumns: Boolean;
begin
  Result := FGrid.AutoSizeColumns;
end;

procedure TosColumnOptions.SetAutoSizeColumns(const Value: Boolean);
begin
  FGrid.AutoSizeColumns := Value;
end;

function TosColumnOptions.GetDefaultColWidth: Integer;
begin
  Result := FGrid.DefaultColWidth;
end;

procedure TosColumnOptions.SetDefaultColWidth(const Value: Integer);
begin
  FGrid.DefaultColWidth := Value;
end;

{ TosGridOptions }

function  TosGridOptions.GetAlwaysShowFocus : Boolean;
begin
  Result := FGrid.AlwaysShowFocus;
end;

procedure TosGridOptions.SetAlwaysShowFocus;
begin
  FGrid.AlwaysShowFocus := Value;
end;

function  TosGridOptions.GetAutoScale : Boolean;
begin
  Result := FGrid.AutoScale;
end;

procedure TosGridOptions.SetAutoScale(Value : Boolean);
begin
  FGrid.AutoScale := Value;
end;

function  TosGridOptions.GetButtonEdgeWidth : Integer;
begin
  Result := FGrid.ButtonEdgeWidth;
end;

procedure TosGridOptions.SetButtonEdgeWidth(Value : Integer);
begin
  FGrid.ButtonEdgeWidth := Value;
end;

function  TosGridOptions.GetCenterPicture : Boolean;
begin
  Result := FGrid.CenterPicture;
end;

procedure TosGridOptions.SetCenterPicture(Value : Boolean);
begin
  FGrid.CenterPicture := Value;
end;

function  TosGridOptions.GetStretchPicture : Boolean;
begin
  Result := FGrid.StretchPicture;
end;

procedure TosGridOptions.SetStretchPicture(Value : Boolean);
begin
  FGrid.StretchPicture := Value;
end;

function  TosGridOptions.GetShrinkPicture : Boolean;
begin
  Result := FGrid.ShrinkPicture;
end;

procedure TosGridOptions.SetShrinkPicture(Value : Boolean);
begin
  FGrid.ShrinkPicture := Value;
end;

function  TosGridOptions.GetKeepAspectRatio : Boolean;
begin
  Result := FGrid.KeepAspectRatio;
end;

procedure TosGridOptions.SetKeepAspectRatio(Value : Boolean);
begin
  FGrid.KeepAspectRatio := Value;
end;

function  TosGridOptions.GetDrawOverlap : TtsDrawOverlap;
begin
  Result := FGrid.DrawOverlap;
end;

procedure TosGridOptions.SetDrawOverlap(Value : TtsDrawOverlap);
begin
  FGrid.DrawOverlap := Value;
end;

function  TosGridOptions.GetGridMode : TtsGridMode;
begin
  Result := FGrid.GridMode;
end;

procedure TosGridOptions.SetGridMode(Value : TtsGridMode);
begin
  FGrid.GridMode := Value;
end;

function  TosGridOptions.GetFixedLineColor : TColor;
begin
  Result := FGrid.FixedLineColor;
end;

procedure TosGridOptions.SetFixedLineColor(Value : TColor);
begin
  FGrid.FixedLineColor := Value;
end;

function  TosGridOptions.GetFlatButtons : Boolean;
begin
  Result := FGrid.FlatButtons;
end;

procedure TosGridOptions.SetFlatButtons(Value : Boolean);
begin
  FGrid.FlatButtons := Value;
end;

function  TosGridOptions.GetFocusBorder : TtsFocusBorder;
begin
  Result := FGrid.FocusBorder;
end;

procedure TosGridOptions.SetFocusBorder(Value : TtsFocusBorder);
begin
  FGrid.FocusBorder := Value;
end;

function  TosGridOptions.GetFocusBorderColor : TColor;
begin
  Result := FGrid.FocusBorderColor;
end;

procedure TosGridOptions.SetFocusBorderColor(Value : TColor);
begin
  FGrid.FocusBorderColor := Value;
end;

function  TosGridOptions.GetFocusColor : TColor;
begin
  Result := FGrid.FocusColor;
end;

procedure TosGridOptions.SetFocusColor(Value : TColor);
begin
  FGrid.FocusColor := Value;
end;

function  TosGridOptions.GetFocusFontColor : TColor;
begin
  Result := FGrid.FocusFontColor;
end;

procedure TosGridOptions.SetFocusFontColor(Value : TColor);
begin
  FGrid.FocusFontColor := Value
end;

function  TosGridOptions.GetIs3D : Boolean;
begin
  Result := FGrid.Is3D;
end;

procedure TosGridOptions.SetIs3D(Value : Boolean);
begin
  FGrid.Is3D := Value;
end;

function  TosGridOptions.GetImageList : TtsImageListComponent;
begin
  Result := FGrid.ImageList;
end;

procedure TosGridOptions.SetImageList(Value : TtsImageListComponent);
begin
  FGrid.ImageList := Value;
end;

function  TosGridOptions.GetInactiveButtonState : TtsInactiveButtonState;
begin
  Result := FGrid.InactiveButtonState;
end;

procedure TosGridOptions.SetInactiveButtonState(Value : TtsInactiveButtonState);
begin
  FGrid.InactiveButtonState := Value;
end;

function  TosGridOptions.GetLineColor : TColor;
begin
  Result := FGrid.LineColor;
end;

procedure TosGridOptions.SetLineColor(Value : TColor);
begin
  FGrid.LineColor := Value;
end;

function  TosGridOptions.GetMaskDefs : TtsMaskDefsComponent;
begin
  Result := FGrid.MaskDefs;
end;

procedure TosGridOptions.SetMaskDefs(Value : TtsMaskDefsComponent);
begin
  FGrid.MaskDefs := Value;
end;

function  TosGridOptions.GetParentColor : Boolean;
begin
  Result := FGrid.ParentColor;
end;

procedure TosGridOptions.SetParentColor(Value : Boolean);
begin
  FGrid.ParentColor := Value;
end;

function  TosGridOptions.GetParentFont : Boolean;
begin
  Result := FGrid.ParentFont;
end;

procedure TosGridOptions.SetParentFont(Value : Boolean);
begin
  FGrid.ParentFont := Value;
end;

function TosGridOptions.GetProvideGridMenu: Boolean;
begin
  Result := FGrid.ProvideGridMenu;
end;

procedure TosGridOptions.SetProvideGridMenu(const Value: Boolean);
begin
  FGrid.ProvideGridMenu := Value;
end;

function  TosGridOptions.GetReadOnlyButton : Boolean;
begin
  Result := FGrid.ReadOnlyButton;
end;

procedure TosGridOptions.SetReadOnlyButton(Value : Boolean);
begin
  FGrid.ReadOnlyButton := Value;
end;

function  TosGridOptions.GetTransparentColor : TColor;
begin
  Result := FGrid.TransparentColor;
end;

procedure TosGridOptions.SetTransparentColor(Value : TColor);
begin
  FGrid.TransparentColor := Value;
end;

function  TosGridOptions.GetSkipReadOnly : Boolean;
begin
  Result := FGrid.SkipReadOnly;
end;

procedure TosGridOptions.SetSkipReadOnly(Value : Boolean);
begin
  FGrid.SkipReadOnly := Value;
end;

function  TosGridOptions.GetWordWrap : TtsWordWrap;
begin
  Result := FGrid.WordWrap;
end;

procedure TosGridOptions.SetWordWrap(Value : TtsWordWrap);
begin
  FGrid.WordWrap := Value;
end;

function  TosGridOptions.GetWantTabs : Boolean;
begin
  Result := FGrid.WantTabs;
end;

procedure TosGridOptions.SetWantTabs(Value : Boolean);
begin
  FGrid.WantTabs := Value;
end;

function TosGridOptions.GetColor: TColor;
begin
  Result := FGrid.Color;
end;

procedure TosGridOptions.SetColor(const Value: TColor);
begin
  FGrid.Color := Value;
end;

function TosGridOptions.GetGridLines: TtsGridLines;
begin
  Result := FGrid.GridLines;
end;

procedure TosGridOptions.SetGridLines(const Value: TtsGridLines);
begin
  FGrid.GridLines := Value;
end;

function TosGridOptions.GetEnterKeyOp: TosEnterKeyOp;
begin
  Result := FGrid.EnterKeyOp;
end;

procedure TosGridOptions.SetEnterKeyOp(const Value: TosEnterKeyOp);
begin
  FGrid.EnterKeyOp := Value;
end;

function TosGridOptions.GetTextFormatting: TosTextFormatting;
begin
  Result := FGrid.TextFormatting;
end;

procedure TosGridOptions.SetTextFormatting(const Value: TosTextFormatting);
begin
  FGrid.TextFormatting := Value;
end;

function TosGridOptions.GetCellPadding: Integer;
begin
  Result := FGrid.CellPadding;
end;

procedure TosGridOptions.SetCellPadding(const Value: Integer);
begin
  FGrid.CellPadding := Value;
end;

function TosGridOptions.GetExitGridOnTab: Boolean;
begin
  Result := FGrid.ExitGridOnTab;
end;

procedure TosGridOptions.SetExitGridOnTab(const Value: Boolean);
begin
  FGrid.ExitGridOnTab := Value;
end;

function TosGridOptions.GetShowTextEllipsis: TosTextEllipsis;
begin
  Result := FGrid.ShowTextEllipsis;
end;

procedure TosGridOptions.SetShowTextEllipsis(const Value: TosTextEllipsis);
begin
  FGrid.ShowTextEllipsis := Value;
end;

function TosGridOptions.GetAutoLoadLayout: TosOnOffOption;
begin
  Result := FGrid.AutoLoadLayout;
end;

procedure TosGridOptions.SetAutoLoadLayout(const Value: TosOnOffOption);
begin
  FGrid.AutoLoadLayout := Value;
end;

function TosGridOptions.GetAutoSaveLayout: TosOnOffOption;
begin
  Result := FGrid.AutoSaveLayout;
end;

procedure TosGridOptions.SetAutoSaveLayout(const Value: TosOnOffOption);
begin
  FGrid.AutoSaveLayout := Value;
end;

procedure TosGridOptions.Assign(srcGridOptions: TPersistent);
begin
  with TosGridOptions(srcGridOptions) do
  begin
    Self.AlwaysShowFocus := AlwaysShowFocus;
    Self.AutoScale       := AutoScale;
    Self.AutoLoadLayout  := AutoLoadLayout;
    Self.AutoSaveLayout  := AutoSaveLayout;
    Self.ButtonEdgeWidth := ButtonEdgeWidth;
    Self.Color           := Color;
    Self.CellPadding     := CellPadding;
    Self.EnterKeyOp      := EnterKeyOp;
    Self.CenterPicture   := CenterPicture;
    Self.KeepAspectRatio := KeepAspectRatio;
    Self.ShrinkPicture   := ShrinkPicture;
    Self.StretchPicture  := StretchPicture;
    Self.DrawOverlap     := DrawOverlap;
    Self.GridLines       := GridLines;
    Self.GridMode        := GridMode;
    Self.FixedLineColor  := FixedLineColor;
    Self.FlatButtons     := FlatButtons;
    Self.FocusBorder     := FocusBorder;
    Self.FocusBorderColor := FocusBorderColor;
    Self.FocusColor      := FocusColor;
    Self.FocusFontColor  := FocusFontColor;
    Self.ImageList       := ImageList;
    Self.InactiveButtonState := InactiveButtonState;
    Self.Is3D            := Is3D;
    Self.LineColor       := LineColor;
    Self.MaskDefs        := MaskDefs;
    Self.ParentColor     := ParentColor;
    Self.ParentFont      := ParentFont;
    Self.ProvideGridMenu := ProvideGridMenu;
    Self.ReadOnlyButton  := ReadOnlyButton;
    Self.TransparentColor := TransparentColor;
    Self.SkipReadOnly    := SkipReadOnly;
    Self.WordWrap        := WordWrap;
    Self.WantTabs        := WantTabs;
    Self.TextFormatting  := TextFormatting;
    Self.ExitGridOnTab   := ExitGridOnTab;
    Self.ShowTextEllipsis := ShowTextEllipsis;
    Self.TotalBandOn     := TotalBandOn;
    Self.TotalBandHeight := TotalBandHeight;
    Self.TotalBandColor  := TotalBandColor;
  end;
end;


function TosGridOptions.GetTotalBandHeight: Integer;
begin
  Result := FGrid.TotalBandHeight;
end;

function TosGridOptions.GetTotalBandOn: Boolean;
begin
  Result := FGrid.TotalBandOn;
end;

procedure TosGridOptions.SetTotalBandHeight(const Value: Integer);
begin
  FGrid.TotalBandHeight := Value;
end;

procedure TosGridOptions.SetTotalBandOn(const Value: Boolean);
begin
  FGrid.TotalBandOn := Value;
end;

function TosGridOptions.GetTotalBandColor: TColor;
begin
  Result := FGrid.TotalBandColor;
end;

procedure TosGridOptions.SetTotalBandColor(const Value: TColor);
begin
  FGrid.TotalBandColor := Value;
end;

function TosGridOptions.GetDefaultButtonHeight: Integer;
begin
  Result := FGrid.DefaultButtonHeight;
end;

function TosGridOptions.GetDefaultButtonWidth: Integer;
begin
  Result := FGrid.DefaultButtonWidth;
end;

procedure TosGridOptions.SetDefaultButtonHeight(const Value: Integer);
begin
  FGrid.DefaultButtonHeight := Value;
end;

procedure TosGridOptions.SetDefaultButtonWidth(const Value: Integer);
begin
  FGrid.DefaultButtonWidth := Value;
end;

function TosGridOptions.GetDrawingMode: TosDrawingMode;
begin
  Result := FGrid.DrawingMode;
end;

procedure TosGridOptions.SetDrawingMode(const Value: TosDrawingMode);
begin
  FGrid.DrawingMode := Value;
end;

function TosGridOptions.GetHighlightEditRow: Boolean;
begin
  Result := FGrid.HighlightEditRow;
end;

procedure TosGridOptions.SetHighlightEditRow(const Value: Boolean);
begin
  FGrid.HighlightEditRow := Value;
end;

{ TosHeadingOptions }

constructor TosHeadingOptions.Create;
begin
  inherited;
  //Self.Font.Name := 'MS Sans Serif';
end;

function  TosHeadingOptions.GetIs3D : Boolean;
begin
  Result := FGrid.Heading3D;
end;

procedure TosHeadingOptions.SetIs3D(Value : Boolean);
begin
  if Value <> FGrid.Heading3D then
  begin
    FGrid.Heading3D := Value;
  end;
end;

function  TosHeadingOptions.GetButton : TtsHeadingButton;
begin
  Result := FGrid.HeadingButton;
end;

procedure TosHeadingOptions.SetButton(Value : TtsHeadingButton);
begin
  if Value <> FGrid.HeadingButton then
  begin
    FGrid.HeadingButton := Value;
  end;
end;

function  TosHeadingOptions.GetColor : TColor;
begin
  Result := FGrid.HeadingColor;
end;

procedure TosHeadingOptions.SetColor(Value : TColor);
begin
  if Value <> FGrid.HeadingColor then
  begin
    FGrid.HeadingColor := Value;
  end;
end;

function  TosHeadingOptions.GetFont : TFont;
begin
  Result := FGrid.HeadingFont;
end;

procedure TosHeadingOptions.SetFont(Value : TFont);
begin
  if Value <> FGrid.HeadingFont then
  begin
    FGrid.HeadingFont := Value;
  end;
end;

function  TosHeadingOptions.GetHeight : Integer;
begin
  Result := FGrid.HeadingHeight;
end;

procedure TosHeadingOptions.SetHeight(Value : Integer);
begin
  if Value <> FGrid.HeadingHeight then
  begin
    FGrid.HeadingHeight := Value;
  end;
end;

function  TosHeadingOptions.GetHorzAlignment : TtsHorzAlignment;
begin
  Result := FGrid.HeadingHorzAlignment;
end;

procedure TosHeadingOptions.SetHorzAlignment(Value : TtsHorzAlignment);
begin
  if Value <> FGrid.HeadingHorzAlignment then
  begin
    FGrid.HeadingHorzAlignment := Value;
  end;
end;

function  TosHeadingOptions.GetVisible : Boolean;
begin
  Result := FGrid.HeadingOn;
end;

procedure TosHeadingOptions.SetVisible(Value : Boolean);
begin
  if Value <> FGrid.HeadingOn then
  begin
    FGrid.HeadingOn := Value;
  end;
end;

function  TosHeadingOptions.GetParentFont : Boolean;
begin
  Result := FGrid.HeadingParentFont;
end;

procedure TosHeadingOptions.SetParentFont(Value : Boolean);
begin
  if Value <> FGrid.HeadingParentFont then
  begin
    FGrid.HeadingParentFont := Value;
  end;
end;

function TosHeadingOptions.GetProvideHeadingMenu: Boolean;
begin
  Result := FGrid.ProvideHeadingMenu;
end;

procedure TosHeadingOptions.SetProvideHeadingMenu(const Value: Boolean);
begin
  FGrid.ProvideHeadingMenu := Value;
end;

function  TosHeadingOptions.GetVertAlignment : TtsVertAlignment;
begin
  Result := FGrid.HeadingVertAlignment;
end;

procedure TosHeadingOptions.SetVertAlignment(Value : TtsVertAlignment);
begin
  if Value <> FGrid.HeadingVertAlignment then
  begin
    FGrid.HeadingVertAlignment := Value;
  end;
end;

function  TosHeadingOptions.GetWordWrap : TtsWordWrap;
begin
  Result := FGrid.HeadingWordWrap;
end;

procedure TosHeadingOptions.SetWordWrap(Value : TtsWordWrap);
begin
  if Value <> FGrid.HeadingWordWrap then
  begin
    FGrid.HeadingWordWrap := Value;
  end;
end;

function TosHeadingOptions.GetHeadingImageAlignment: TtsHorzAlignment;
begin
  Result := FGrid.HeadingImageAlignment;
end;

procedure TosHeadingOptions.SetHeadingImageAlignment(const Value: TtsHorzAlignment);
begin
  FGrid.HeadingImageAlignment := Value;
end;

procedure TosHeadingOptions.Assign(srcHeadingOptions: TPersistent);
begin
  with TosHeadingOptions(srcHeadingOptions) do
  begin
    Self.Is3D               := Is3D;
    Self.Button             := Button;
    Self.Color              := Color;
    Self.Font               := Font;
    Self.Height             := Height;
    Self.HeadingImageAlignment := HeadingImageAlignment;
    Self.HorzAlignment      := HorzAlignment;
    Self.Visible            := Visible;
    Self.ParentFont         := ParentFont;
    Self.ProvideHeadingMenu := ProvideHeadingMenu;
    Self.VertAlignment      := VertAlignment;
    Self.WordWrap           := WordWrap;
  end;
end;

{ TosGridData }
constructor TosGridData.Create;
begin
  inherited Create;
  //FSortFields := TosSortFieldList.Create;
  FFieldLayout := TList.Create;
  FSortedData := TosDbSortedRows.Create;
end;

destructor TosGridData.Destroy;
begin
  Clear;
  //FSortFields.Free;
  ResetFieldLayout;
  FFieldLayout.Free;
  FreeAndNil(FSortedData);
  inherited Destroy;
end;

procedure TosGridData.ResetFieldLayout;
var I: Integer;
begin
  for I := 1 to FFieldLayout.Count do
     TosFieldLayout(FFieldLayout[I-1]).Free;
  FFieldLayout.Count := 0;
end;

procedure TosGridData.SaveFieldLayout;
var
    I: Integer;
begin
  if not Assigned(Dataset) then
  begin
    ResetFieldLayout;
    Exit;
  end;

  try
    FFieldLayout.Count := Dataset.FieldCount;
    for I := 1 to Dataset.FieldCount do
    begin
      if not Assigned(FFieldLayout[I-1]) then
         FFieldLayout[I-1] := TosFieldLayout.Create;
      TosFieldLayout(FFieldLayout[I-1]).AssignFieldLayout(Dataset.Fields[I-1]);
    end;
  except on Exception do
    begin
      ResetFieldLayout;
      raise;
    end;
  end;
end;

function TosGridData.FieldLayoutChanged: Boolean;
var
    I: Integer;
    Layout: TosFieldLayout;
    Changed: Boolean;
begin
  if not Assigned(Dataset) then
  begin
    Result := FFieldLayout.Count <> 0;
    Exit;
  end;

  Changed := FFieldLayout.Count <> Dataset.FieldCount;
  if not Changed then
  begin
    for I := 1 to Dataset.FieldCount do
    begin
      Layout := FFieldLayout[I-1];
      Changed := not Layout.FieldLayoutEqual(Dataset.Fields[I-1]);
      if Changed then Break;
    end;
  end;

  Result := Changed;
end;

function TosGridData.FieldOrderChanged: Boolean;
var
    I, Count: Integer;
    Layout: TosFieldLayout;
    Changed: Boolean;
begin
    Result := False;
    if not Assigned(Dataset) then Exit;

    Changed := False;
    Count := CalcMin(FFieldLayout.Count, Dataset.FieldCount);
    for I := 1 to Count do
    begin
      Layout := FFieldLayout[I-1];
      Changed := Layout.FieldName <> Dataset.Fields[I-1].FieldName;
      if Changed then Break;
    end;

    Result := Changed;
end;

function TosGridData.CheckForPost(dataRow : Integer) : Boolean;
begin
  Result := False;
  if (FSortedData = Nil) or
     (dataRow = 0) or
     (DataSet = Nil) then exit;

  FSortedData.FDataLink.ActiveRecord := FSortedData.SortedRow[dataRow].ActiveRow; //FSortedData.Node[DataRow].NodeValue.ActiveRow;
  if (DataSet.Modified) then
  begin
    DataSet.Post;
    TosDbSortedRow(FSortedData.SortedRow[dataRow]).FKeyValue := DataSet.Bookmark;
    Result := True;
  end
  else
    Result := False;
end;

function TosGridData.GetSortedRow(dataRow : Integer) : TosDbSortedRow;
begin
  Result := TosDbSortedRow(FSortedData.SortedRow[dataRow]);
end;

procedure TosGridData.ClearSortFields;
begin
  FSortedData.ClearSorts;
end;

procedure TosGridData.ResetRowProperties;
var i : Integer;
begin
  if FGrid.FVisibleRows.ItemsCreated then  // Reset visibleRows
  begin
    FGrid.BeginUpdate;
    FSortedData.BeginUpdate;
    try
      FGrid.ResetRowProperties([prVisible]);
      for i := 1 to FSortedData.Count do
        if (not FSortedData.SortedRow[i].Visible) or
           (not FSortedData.SortedRow[i].GroupVisible) then
           FGrid.RowVisible[i] := False;
    finally
      FSortedData.EndUpdate;
      FGrid.EndUpdate;
    end;
  end;
  if (FSortedData.MixedRowHeights) then
     FGrid.ResetRowHeights;
end;

procedure TosGridData.RemoveSort(aFieldName : String);
var i : Integer;
    sortEntry : TosSortEntry;
begin
  i := FSortedData.SortFields.IndexOf(aFieldName);
  if (i >= 0) then
  begin
    FGrid.BeginUpdate;
    try
      sortEntry := FSortedData.SortFields.SortEntry[i];
      if (sortEntry.GroupSort) then
      begin
        if (FSortedData.FootersOn) then
        begin
          FSortedData.RemoveGroupFooters;
          if (FSortedData.SortFields.GroupCount > 1) then
             Self.FReapplyFooters := True;
        end;
        // Now remove group headers for this level/column if it is the lowest level,
        // otherwise, we need to remove all groups and start over...
        if (FSortedData.SortFields.GroupLevelOf(aFieldName) = FSortedData.SortFields.GroupCount) then
           FSortedData.RemoveGroupHeaders(sortEntry.Column)
        else
           FSortedData.RemoveGroupHeaders(0);
      end;
      FSortedData.SortFields.Delete(i);
      if (FSortedData.SortFields.GroupCount = 0) then
         FSortedData.RemoveNonDataRows;
      FSortedData.Reload(FGrid.FVisibleRows.ItemsCreated);
      if (FSortedData.SortFields.Count > 0) then
         FSortedData.DoSort;
      FSortedData.CheckGrouping(FReapplyFooters);
      FSortedData.DoSort;
      ResetRowCount;
      if FReapplyFooters then
         RecalcFooterTotals;
      FReapplyFooters := False;
    finally
      FGrid.EndUpdate;
    end;      
  end;
end;

procedure TosGridData.AddSortField(colName : String; dataCol : Integer; aSortType : TosSortType);
var useDataType : TosDataType;
    theCol : TosDbCol;
begin
  theCol := FGrid.Col[dataCol];
  if (theCol.DatasetField = Nil) and
     (not FGrid.StoreData) then exit;

  FGrid.BeginUpdate;
  try
    useDataType := theCol.DataType;
    //if (useVarType = varDate) then
    //   useVarType := varString;     // TP removed Jan 22,2005
    if (theCol.ButtonType = btCombo) and
       (theCol.Combo <> Nil) and
       (theCol.Combo.AutoLookup) then
       useDataType := dyString;
    FSortedData.AddSortField(colName, dataCol, aSortType, useDataType);
    ResetRowCount;
  finally
    FGrid.EndUpdate;
  end;
end;

procedure TosGridData.AddGroupField(colName : String; dataCol : Integer);
var useDataType : TosDataType;
    theCol : TosDbCol;
begin
  theCol := FGrid.Col[dataCol];
  if (FGrid.Col[dataCol].DatasetField = Nil) and
     (not FGrid.StoreData) then exit;

  FGrid.BeginUpdate;
  try
    useDataType := theCol.DataType;
    if (useDataType = dyDate) then
       useDataType := dyString;  // Grouping for dates is on a date segment
    if (theCol.ButtonType = btCombo) and
       (theCol.Combo <> Nil) and
       (theCol.Combo.AutoLookup) then
       useDataType := dyString;
    FReapplyFooters := SortedData.FootersOn and (SortedData.SortFields.GroupCount > 0);
    if FReapplyFooters then
       FSortedData.RemoveGroupFooters;
    FSortedData.AddGroupField(colName, dataCol, useDataType, FReapplyFooters);
    if FReapplyFooters then
    begin
      FSortedData.FootersOn := True;
      RecalcFooterTotals;
    end;
    FReapplyFooters := False;
    ResetRowCount;
  finally
    FGrid.EndUpdate;
  end;
end;

procedure TosGridData.Refresh;
begin
  if (Assigned(DataSet)) then
     SetDatasetBuffercount(DataSet.RecordCount);
  FSortedData.Reload(False);
  FSortedData.CheckGrouping(FReapplyFooters or FSortedData.FootersOn);
  ResetRowCount;
  if (FSortedData.FootersOn) then
     RecalcFooterTotals;
end;

function TosGridData.RecordCountsDiffer : Boolean;
begin
  Result := (FSortedData.DataCount <> DataLink.RecordCount);
end;

function TosGridData.CheckBookmarks : Boolean;
begin
  Result := True;
  if (FSortedData.FBookmarks.Count > 0) then
  begin
    if (not DataLink.DataSet.BookmarkValid(TBookmark(FSortedData.FBookmarks.Items[FSortedData.FBookmarks.Count-1]))) then
    begin
      Clear;
      Result := False;
    end;
  end;
end;

procedure TosGridData.ResetRowCount;
begin
  //FGrid.Rows := FSortedData.Count;
  FGrid.UpdateRowCount(True);
  ResetRowProperties;
end;

function TosGridData.GridDataRowForActiveRow(actRow : Integer) : Integer;
var i : integer;
begin
  Result := 0;
  for i := 1 to FSortedData.Count do
    if (FSortedData.SortedRow[i].ActiveRow = actRow) then
    begin
      Result := i;
      break;
    end;
end;

function TosGridData.AppendCurrentRecord : TosDbSortedRow;
begin
  Result := FSortedData.AddDataRow(FSortedData.FDataLink.ActiveRecord, FSortedData.DbRecSortText + Format('%8.7d', [FSortedData.Count]));
end;

procedure TosGridData.SetDataSet(Value : TDataSet);
begin
  if (FDataSet <> Value) then
  begin
    FDataSet := Value;
    if (FDataSet <> Nil) and
       (FDataSet.Active) then
    begin
      FSortedData.FDataLink.BufferCount := DataSet.RecordCount;
      FSortedData.Reload(False);
    end
    else
      FSortedData.Clear;
  end;
end;

procedure TosGridData.MoveFirst;
begin
  FGrid.InDataSetEvent := True;
  try
    Dataset.First;
  finally
    FGrid.InDataSetEvent := False;
  end;
end;

procedure TosGridData.MoveLast;
begin
  FGrid.InDataSetEvent := True;
  try
    Dataset.Last;
  finally
    FGrid.InDataSetEvent := False;
  end;
end;

procedure TosGridData.MoveRow(fromRow, toRow : Integer);
var oldSortedRow : TosDbSortedRow;
begin
  oldSortedRow := SortedRow[toRow];
  FSortedData.Move(fromRow-1, toRow-1);
  SortedRow[toRow].FGroupNumber := oldSortedRow.GroupNumber;
end;

procedure TosGridData.SetActiveRecord(Value: Integer);
begin
  if (DataLink.DataSet <> Nil) then
     DataLink.ActiveRecord := Value;
  FActiveRow := Value;
end;

function TosGridData.GetActiveRecord: Integer;
begin
  Result := DataLink.ActiveRecord;
end;

function  TosGridData.GetDataLink : TDataLink;
begin
  Result := FGrid.DataLink;
end;

function TosGridData.GetBufferRows: Integer;
begin
  if (FDataSet = Nil) then
     Result := 0
  else
     Result := Self.FDataSet.RecordCount; //DataLink.RecordCount;
end;

function TosGridData.GetCellValue(dataCol : Variant; dataRow: Integer): Variant;
begin
  if (VarType(dataCol) = varString) then
     Result := FSortedData.SortedRow[DataRow].FieldValue(dataCol)
  else
     Result := FSortedData.SortedRow[DataRow].FieldValue(FGrid.Col[dataCol].FieldName);
end;

function TosGridData.GetCellField(dataCol : Variant; dataRow: Integer): TField;
begin
  if (VarType(dataCol) = varString) then
     Result := TosDbSortedRow(FSortedData.SortedRow[DataRow]).FieldByName(dataCol)
  else
     Result := TosDbSortedRow(FSortedData.SortedRow[DataRow]).FieldByName(FGrid.Col[dataCol].FieldName);
end;

procedure TosGridData.CheckDatalinkBufferOk(OldSize: Integer);
const MaxRetry = 1000;
var Empty: Boolean;
    Count: Integer;
    CurBufSize: Integer;
    OldActive: Integer;
begin
    //Datalink bug work-around: FFirstRecord is not always correct after
    //increasing the buffer size. Check and force to correct position by
    //testing for empty records at the end of the buffer.

    CurBufSize := Datalink.BufferCount;
    if OldSize >= CurBufSize then Exit;

    try
        Count := 0;
        while Count < MaxRetry do
        begin
            OldActive := ActiveRecord;
            try
                ActiveRecord := BufferRows;
                {$IFDEF TSVER_V3}
                    Empty := Dataset.IsEmpty;
                {$ELSE}
                    Empty := False;
                {$ENDIF}
            finally
                ActiveRecord := OldActive;
            end;

            if not Empty then Break;

            Datalink.BufferCount := Datalink.BufferCount + 1;
            Inc(Count);
        end;
    finally
        Datalink.BufferCount := CurBufSize;
    end;
end;

procedure TosGridData.SetDatasetBufferCount(BufSize: Integer);
var oldActive : Integer;
    //OldSize : Integer;
begin
  //if not FGrid.HandleAllocated then Exit;

  //OldSize := Datalink.BufferCount;
  if (BufSize <> DataLink.BufferCount) then
  begin
    oldActive := DataLink.ActiveRecord;
    try
      Datalink.BufferCount := BufSize;
    finally
      DataLink.ActiveRecord := oldActive;
    end;
  end;
  //CheckDatalinkBufferOk(OldSize); //do not need anymore TP 03/02/2005
end;

procedure TosGridData.LinkActive(value : Boolean);
begin
  if Value then
  begin
    FSortedData.FBookmarks.FDataSet := FGrid.DataLink.DataSet;
    FDataSet := FGrid.DataLink.DataSet;
    FSortedData.FBookmarks.LinkActive(Value);
    SetDatasetBufferCount(BufferRows);         // TP Feb 1 Moved down one line
  end
  else
  begin
    Clear;
    ClearSortFields;
    DataSet := Nil;
    FSortedData.FBookmarks.Clear;
    FActiveRow := 0;
    FOldRow := 0;
  end;
end;

function TosGridData.ReverseChar(aChar : Char) : Char;
begin
  if aChar = '''' then Result := 'z'
  else if aChar = '-' then Result := 'y'
  else if aChar = '!' then Result := 'x'
  else if aChar = '"' then Result := 'w'
  else if aChar = '#' then Result := 'v'
  else if aChar = '$' then Result := 'u'
  else if aChar = '%' then Result := 't'
  else if aChar = '&' then Result := 's'
  else if aChar = '(' then Result := 'r'
  else if aChar = ')' then Result := 'q'
  else if aChar = '*' then Result := 'p'
  else if aChar = ',' then Result := 'o'
  else if aChar = '.' then Result := 'n'
  else if aChar = '/' then Result := 'm'
  else if aChar = ':' then Result := 'l'
  else if aChar = ';' then Result := 'k'
  else if aChar = '?' then Result := 'j'
  else if aChar = '@' then Result := 'i'
  else if aChar = '\' then Result := 'h'
  else if aChar = '^' then Result := 'g'
  else if aChar = '_' then Result := 'f'
  else if aChar = '|' then Result := 'e'
  else if aChar = ' ' then Result := 'd'
  else if aChar = '+' then Result := 'c'
  else if aChar = '<' then Result := 'b'
  else if aChar = '=' then Result := 'a'
  else if aChar = '>' then Result := '9'
  else if aChar = '0' then Result := '8'
  else if aChar = '1' then Result := '7'
  else if aChar = '2' then Result := '6'
  else if aChar = '3' then Result := '5'
  else if aChar = '4' then Result := '4'
  else if aChar = '5' then Result := '3'
  else if aChar = '6' then Result := '2'
  else if aChar = '7' then Result := '1'
  else if aChar = '8' then Result := '0'
  else if aChar = '9' then Result := '>'
  else if aChar = 'a' then Result := '='
  else if aChar = 'b' then Result := '<'
  else if aChar = 'c' then Result := '+'
  else if aChar = 'd' then Result := '|'
  else if aChar = 'e' then Result := '_'
  else if aChar = 'f' then Result := '^'
  else if aChar = 'g' then Result := '\'
  else if aChar = 'h' then Result := '@'
  else if aChar = 'i' then Result := '?'
  else if aChar = 'j' then Result := ';'
  else if aChar = 'k' then Result := ':'
  else if aChar = 'l' then Result := '/'
  else if aChar = 'm' then Result := '.'
  else if aChar = 'n' then Result := ','
  else if aChar = 'o' then Result := '*'
  else if aChar = 'p' then Result := ')'
  else if aChar = 'q' then Result := '('
  else if aChar = 'r' then Result := '&'
  else if aChar = 's' then Result := '%'
  else if aChar = 't' then Result := '$'
  else if aChar = 'u' then Result := '#'
  else if aChar = 'v' then Result := '"'
  else if aChar = 'w' then Result := '!'
  else if aChar = 'x' then Result := ''''
  else if aChar = 'y' then Result := '-'
  else if aChar = 'z' then Result := ' '
  else Result := ' ';
end;

function TosGridData.GetActiveBookmark : TBookmark; //TBookmarkStr;
begin
  Result := nil; //'';
  if (Self.DataSet <> Nil) and
     (Self.ActiveRow >= 0) then
     Result := DataSet.Bookmark;
  //if ActiveDataRec <> Nil then
  //   Result := ActiveDataRec.Bookmark;
end;

procedure TosGridData.SetActiveBookmark(Value : TBookmark {TBookmarkStr});
begin
  {for i := 1 to Count do    // Iterate
  begin
    if Self.DataRec[i].Bookmark = Value then
    begin
      ActiveRow := i;
      break;
    end;
  end; }
end;

procedure TosGridData.DataSetScrolled(Distance : Integer);
begin
 { if Distance < 0 then
  begin
    if (ActiveRow + Distance) < 0 then
       beep
    else
       ActiveRow := ActiveRow + Distance;
  end
  else
  begin
    if (ActiveRow + Distance) > Count then
       beep
    else
       ActiveRow := ActiveRow + Distance;
  end;  }
end;

procedure TosGridData.ReseqDown(fromActiveRow, toActiveRow : Integer);
var i : Integer;
    srtRow : TosDbSortedRow;
begin
  for i := 1 to Self.FSortedData.Count do
  begin
    srtRow := FSortedData.SortedRow[i];
    if (srtRow.IsData) and
       (srtRow.ActiveRow >= fromActiveRow) and
       (srtRow.ActiveRow <= toActiveRow) then
       srtRow.FActiveRow := srtRow.ActiveRow - 1;
  end;
end;

procedure TosGridData.ReseqUp(fromActiveRow : Integer);
var i : Integer;
    srtRow : TosDbSortedRow;
begin
  for i := 1 to Self.FSortedData.Count do
  begin
    srtRow := FSortedData.SortedRow[i];
    if (srtRow.IsData) and
       (srtRow.ActiveRow >= fromActiveRow) then
       srtRow.FActiveRow := srtRow.ActiveRow + 1;
  end;
end;

procedure TosGridData.Clear;
begin
  FSortedData.Clear;
end;

procedure TosGridData.ClearSorts;
begin
  Self.FSortedData.ClearSorts;
end;

function TosGridData.GetRecordCount : Integer;
begin
  Result := FSortedData.Count;
end;

{procedure TosGridData.AddSortOnCol(theCol : Variant; sortMode : TosSortType);
var sFieldName : String;
    useDataType : TosDataType;
begin
  if (VarType(theCol) = varString) then
     sFieldName := theCol
  else
     sFieldName := FGrid.Col[theCol].FieldName;

  useDataType := VarType(FGrid.Col[theCol].DatasetField.Value);
  if (useVarType = varDate) then
     useVarType := varString;     
  SortedData.AddSortField(sFieldName, FGrid.Col[theCol].DataCol, sortMode, useVarType);
  FGrid.ResetVisiblePointers;
  FGrid.Invalidate;
end;}

procedure TosGridData.ToggleSortOnCol(theCol : Variant);
var srtEntry : TosSortEntry;
begin
  if (VarType(theCol) = varString) then
    srtEntry := FSortedData.SortFields.SortEntryByName(theCol)
  else
    srtEntry := FSortedData.SortFields.SortEntryByNumber(theCol);
  if (srtEntry <> Nil) then
  begin
    srtEntry.ToggleSort;
    SortedData.Reload(False);
    FGrid.Invalidate;
  end;
end;

procedure TosGridData.SelectDataRow(dataRow : Integer);
var goDirection : Integer;
begin
  if (FSortedData <> Nil) and
     (FSortedData.FDataLink <> Nil) and
     (DataSet <> Nil) and
     (dataRow <= FSortedData.Count) and
     (not FGrid.FEditing) then
  begin
    goDirection := 1;
    if (DataSet.Eof) then
       goDirection := -1;
    if (FSortedData.SortedRow[DataRow].IsGroupHeader) then
       ActiveRecord := FSortedData.NextDataRow(dataRow).ActiveRow - goDirection
    else if (FSortedData.SortedRow[DataRow].IsGroupFooter) then
       ActiveRecord := FSortedData.PriorDataRow(dataRow).ActiveRow - goDirection
    else
       ActiveRecord := FSortedData.SortedRow[DataRow].ActiveRow - goDirection;
    if (DataSet <> Nil) then
    begin
      DataSet.MoveBy(goDirection);
      FActiveRow := ActiveRecord;
    end;
  end;
end;

procedure TosGridData.SetActiveRow(Value : Integer);
begin
  if RecordCount = 0 then
  begin
    FActiveRow := 0;
    exit;
  end;
    
  if Value > RecordCount then
     raise Exception.Create('ActiveRow Value is less than grid row count of ' + IntToStr(RecordCount))
  else if Value < 0 then
     raise Exception.Create('ActiveRow Value must be greater than zero');

  FOldRow := FActiveRow;
  if (FActiveRow = 0) then
     FActiveRow := 1;

  FActiveRow := Value;
  DataLink.ActiveRecord := FActiveRow;
end;

function TosGridData.GetBookmark(dataRow : Integer) : TBookmarkStr;
begin
  //Result := DataRec[iRow].Bookmark;
end;

function TosGridData.GetRecordRow(DataRow : Integer) : Integer;
begin
  if (DataRow <= FSortedData.Count) then
     Result := FSortedData.SortedRow[DataRow].ActiveRow
  else
     raise Exception.Create('Unable to index into SortedData at row # ' + IntToStr(DataRow));
end;

function  TosGridData.DataSetRows : String;
begin
  Result := '';
  with Self.DataLink.DataSet do
  begin
    First;
    while not eof do
    begin
      Result := Result + IntToStr(DataLink.ActiveRecord) + ',';
      Next;
    end;
  end;
end;

function  TosGridData.SortedDataRows : String;
var i : Integer;
begin
  Result := '';
  for i := 1 to Self.SortedData.Count do
    if (SortedData.SortedRow[i].RowType = rtData) then
       Result := Result + IntToStr(SortedData.SortedRow[i].ActiveRow) + ',';
end;

procedure TosGridData.PositionOnDataRow(DataRow : Integer);
var iRow : Integer;
begin
  if (FSortedData <> Nil) and
     (dataRow <= FSortedData.Count) then
  begin
    if (FSortedData.FDataLink <> Nil) then
    begin
      for iRow := DataRow to FSortedData.Count do
      begin
        if (FSortedData.SortedRow[iRow].IsData) then
        begin
          ActiveRecord := FSortedData.SortedRow[iRow].ActiveRow;
          break;
        end;
      end;
    end
    else if (FGrid.StoreData) then
    begin
      FSortedData.RowForActiveRow(DataRow);
    end;
  end;
end;

{function TosGridData.PositionOnBookmark(Value : TBookmarkStr) : Boolean;
begin
  Result := False;
  DataSet.Bookmark := Value;
  Result := True;
end; }

function TosGridData.SortString(aField : TField) : String;
begin
  // Strings must be padded with spaces to sort properly with secondary field sorts!
  if aField.DataType = ftString then
		Result := Format('%-' + IntToStr(aField.Size) + 's', [Lowercase(aField.AsString)])
  else if (aField.DataType = ftInteger) or
          (aField.DataType = ftSmallInt) then
		Result := Format('%16.15d', [aField.AsInteger])
 { else if aField.DataType = ftDatetime then
  begin
   if Self.GroupDateTimeSegment = dgoNone then
  		 Result := FormatDateTime('yyyymmddhhmmss', aField.AsDateTime)
    else
       Result := aField.AsDateSegment(GroupDateTimeSegment);
  end }
  else if aField.DataType = ftFloat then
  begin
		Result := Format('%16.2f', [aField.AsFloat]);
    Result := LPadZero(Result);
  end
  else
		Result := Lowercase(aField.AsString);
end;

function TosGridData.LPadZero(aString : String) : String;
var i : Integer;
begin
  Result := aString;
  for i := 1 to Length(aString) do    // Iterate
  begin
    if aString[i] = ' ' then
       Result[i] := '0'
    else
       break;
  end;    // for
end;

function TosGridData.ReverseText(aString : String) : String;
var i : integer;
begin
  Result := '';
  for i := 1 to Length(aString) do    // Iterate
    Result := Result + ReverseChar(aString[i]);
end;

procedure TosGridData.RecalcFooterTotals;
var dataCol, dataRow, currRow : Integer;
begin
  if (not SortedData.FootersOn) or
     (SortedData.InUpdate) then exit;
  if (not FGrid.StoreData) and
     (FGrid.DataSet = Nil) then exit;

  if (DataSet <> Nil) then
  begin
    //DataSet.DisableControls;
    currRow := DataLink.ActiveRecord;
  end
  else
    currRow := 0;
    
  try
    for dataRow := SortedData.Count downto 1 do
    begin
      if (SortedRow[dataRow].RowType = rtGroupFooter) then
         for dataCol := 1 to FGrid.Cols do
           if (FGrid.Col[dataCol].GroupSummaryOp <> gsoNone) then
              TosGroupFooterRow(SortedRow[dataRow]).EnsureSubTotal(dataCol, FGrid.Col[dataCol].GroupSummaryOp);
    end;
    SortedData.RecalcFooterTotals;
  finally
    if (DataSet <> Nil) then
    begin
      DataLink.ActiveRecord := currRow;
      //DataSet.EnableControls;
    end;
  end;
end;

procedure TosGridData.AddGroupFooters;
begin
  if (FSortedData.GroupCount = 0) then exit;
  
  FGrid.BeginUpdate;
  try
    FSortedData.AddGroupFooters;
    FSortedData.DoSort;
    ResetRowCount;    
    RecalcFooterTotals;
    FReapplyFooters := False;
  finally
    FGrid.EndUpdate;
  end;
end;

procedure TosGridData.RemoveGroupFooters;
begin
  FGrid.BeginUpdate;
  try
    FSortedData.RemoveGroupFooters;
    ResetRowCount;
  finally
    FGrid.EndUpdate;
  end;
end;

procedure TosGridData.RemoveRow(dataRow : Integer);
begin
  FGrid.BeginUpdate;
  try
    FSortedData.RemoveEntry(Self.GetSortedRow(dataRow));
    Self.SetDatasetBufferCount(FSortedData.Count);
    ResetRowCount;
  finally
    FGrid.EndUpdate;
  end;
end;

{TosFieldList}

constructor TosFieldList.Create(Grid: TosCustomAdvDbGrid);
begin
    inherited Create;
    FGrid := Grid;
end;

destructor TosFieldList.Destroy;
begin
    Reset;
    inherited Destroy;
end;

procedure TosFieldList.Reset;
var
    I: Integer;
begin
    for I := 1 to Count do
    begin
        TosDBField(Items[I-1]).Free;
        Items[I-1] := nil;
    end;
    Count := 0;
end;

function TosFieldList.CreateField(Value: TField): TosDBField;
begin
   { if Value is TStringField then
        Result := TtsDBStringField.Create(Grid, nil, Value)
    else if Value is TIntegerField then
        Result := TtsDBIntegerField.Create(Grid, nil, Value)
    else if Value is TSmallintField then
        Result := TtsDBSmallintField.Create(Grid, nil, Value)
    else if Value is TWordField then
        Result := TtsDBWordField.Create(Grid, nil, Value)
    else if Value is TAutoIncField then
        Result := TtsDBAutoIncField.Create(Grid, nil, Value)
    else if Value is TFloatField then
        Result := TtsDBFloatField.Create(Grid, nil, Value)
    else if Value is TCurrencyField then
        Result := TtsDBCurrencyField.Create(Grid, nil, Value)
    else if Value is TBCDField then
        Result := TtsDBBCDField.Create(Grid, nil, Value)
    else if Value is TBooleanField then
        Result := TtsDBBooleanField.Create(Grid, nil, Value)
    else if Value is TDateTimeField then
        Result := TtsDBDateTimeField.Create(Grid, nil, Value)
    else if Value is TDateField then
        Result := TtsDBDateField.Create(Grid, nil, Value)
    else if Value is TTimeField then
        Result := TtsDBTimeField.Create(Grid, nil, Value)
    else if Value is TBinaryField then
        Result := TtsDBBinaryField.Create(Grid, nil, Value)
    else if Value is TBytesField then
        Result := TtsDBBytesField.Create(Grid, nil, Value)
    else if Value is TVarBytesField then
        Result := TtsDBVarBytesField.Create(Grid, nil, Value)
    else if Value is TMemoField then
        Result := TtsDBMemoField.Create(Grid, nil, Value)
    else if Value is TGraphicField then
        Result := TtsDBGraphicField.Create(Grid, nil, Value)
    else if Value is TBlobField then
        Result := TtsDBBlobField.Create(Grid, nil, Value)
    else      }
        Result := TosDBField.Create(Grid, nil, Value);
end;

procedure TosFieldList.CreateFields;
var
    I: Integer;
    Cnt: Integer;
begin
    Reset;
    if not Assigned(Grid.Dataset) then Exit;
    if not Grid.Dataset.Active and Grid.Dataset.DefaultFields then Exit;

    with Grid.Dataset do
    begin
        Cnt := FieldCount;
        for I := 1 to Cnt do
        begin
            Self.Count := Self.Count + 1;
            Items[I-1] := CreateField(Fields[I-1]);
        end;
    end;
end;

function TosFieldList.FindField(FieldName: string): TosDBField;
var
    I: Integer;
begin
    Result := nil;
    I := 0;
    while I < Count do
    begin
        if AnsiCompareText(FieldName, TosDBField(Items[I]).FieldName) = 0 then
        begin
            Result := Items[I];
            Break;
        end;
        Inc(I);
    end;
end;

function TosFieldList.FieldIndex(Field: TField): Integer;
var
    I: Integer;
begin
    Result := -1;
    for I := 0 to Count - 1 do
    begin
        if TosDBField(Items[I]).DatasetField = Field then
        begin
            Result := I;
            Break;
        end;
    end;
end;

{TosFieldLayout}

constructor TosFieldLayout.Create;
begin
    inherited Create;

    FFieldName := '';
    FDisplayLabel := '';
    FDisplayWidth := 0;
    FReadOnly := False;
    FVisible := False;
    FDisplayValues := '';
end;

procedure TosFieldLayout.AssignFieldLayout(Field: TField);
begin
    FFieldName := Field.FieldName;
    FDisplayLabel := Field.DisplayLabel;
    FDisplayWidth := Field.DisplayWidth;
    FReadOnly := Field.ReadOnly;
    FVisible := Field.Visible;

    FDisplayValues := '';
    if Field is TBooleanField then FDisplayValues := TBooleanField(Field).DisplayValues;
end;

function TosFieldLayout.FieldLayoutEqual(Field: TField): Boolean;
var
    Values: string;
begin
    Result := (FFieldName = Field.FieldName) and
              (FDisplayLabel = Field.DisplayLabel) and
              (FDisplayWidth = Field.DisplayWidth) and
              (FReadOnly = Field.ReadOnly) and
              (FVisible = Field.Visible);

    if Result then
    begin
        Values := '';
        if Field is TBooleanField then Values := TBooleanField(Field).DisplayValues;
        Result := FDisplayValues = Values;
    end;
end;

{TosDBField}

constructor TosDbField.Create(Grid: TosCustomAdvDbGrid; Col: TosDBCol; Field: TField);
begin
    FGrid := Grid;
    FCol := Col;
    FDatasetField := Field;
end;

destructor TosDbField.Destroy;
begin
    inherited Destroy;
end;

function TosDbField.IsEditField: Boolean;
begin
    Result := not FGrid.ReadOnly and
              not DatasetField.ReadOnly and
              not DatasetField.Calculated and
              not DatasetField.Lookup;
end;

function TosDbField.FieldEditOk: Boolean;
var
    DataCol: Longint;
begin
    Result := IsEditField;
    if not Result then Exit;

    if Assigned(FCol) then
    begin
      DataCol := FCol.DataCol;
      if FGrid.CurrentCell.DataCol = DataCol then Result := FGrid.StartCellEdit;
      if Result then Result := FGrid.StartRowEdit;
      if Result then Result := FGrid.StartCellChange(DataCol, False);
    end
    else
    begin
      Result := FGrid.StartRowEdit;
      if Result then FGrid.Datalink.Edit;
    end;
end;

function TosDbField.GetDataSize: Word;
begin
    Result := DatasetField.DataSize;
end;

function TosDbField.GetSize: Word;
begin
    Result := DatasetField.Size;
end;

procedure TosDbField.SetSize(Value: Word);
begin
    DatasetField.Size := Value;
end;

procedure TosDbField.ClearLookup;
//var RecBuf: PChar;
begin
    if not Lookup then Exit;

  {  with FGrid.FScrollDataset do
    begin
        RecBuf := ActiveRecordBuffer;
        Boolean(RecBuf[FLookupStart + FLookupOffset]) := False;
    end; }
end;

function TosDbField.GetLookupData(Buffer: Pointer): Boolean;
//var RecBuf: PChar;
begin
    Result := False;
  {  with FGrid.FScrollDataset do
    begin
        RecBuf := ActiveRecordBuffer;
        if Assigned(RecBuf) then
        begin
            Result := Boolean(RecBuf[FLookupStart + FLookupOffset]);
            if Result and Assigned(Buffer) then
                Move(RecBuf[FLookupStart + FLookupOffset + 1], Buffer^, DataSize);
        end;
    end;  }
end;

procedure TosDbField.SetLookupData(Buffer: Pointer);
//var RecBuf: PChar;
begin
  {  with FGrid.FScrollDataset do
    begin
        RecBuf := ActiveRecordBuffer;
        Boolean(RecBuf[FLookupStart + FLookupOffset]) := True;
        Move(Buffer^, RecBuf[FLookupStart + FLookupOffset + 1], DataSize);
    end; }
end;

procedure TosDbField.GetLookupValue;
begin
  {  with DatasetField do
    begin
        if Assigned(LookupDataSet) and LookupDataSet.Active then
        begin
            LookupValue := LookupDataSet.Lookup(LookupKeyFields,
                                                FGrid.FScrollDataSet.FieldValues[KeyFields],
                                                LookupResultField);
        end
        else
            LookupValue := Null;
    end; }
end;

function TosDbField.GetData(Buffer: Pointer): Boolean;
//var RecBuf: PChar;
//    IsBlank: LongBool;
begin
    Result := False;
    if FDatasetField.Calculated then Exit;

 {   with FGrid.FScrollDataset do
    begin
        if (FGrid.Editing or FGrid.StartEditing) and
           (CompareBkm(FDataset.Bookmark, ActiveBookmark) = 0) then
            Result := FDatasetField.GetData(Buffer)
        else if Lookup then
            Result := GetLookupData(Buffer)
        else
        begin
            RecBuf := ActiveRecordBuffer;
            if Assigned(RecBuf) then
            begin
                CheckErr(DbiGetField(FHandle, FieldNo, RecBuf, Buffer, IsBlank));
                Result := not IsBlank;
            end;
        end;
    end; }
end;

function TosDbField.GetText(DisplayText: Boolean): string;
begin
    Result := AsString;
end;

procedure TosDbField.SetEditText(Value: string);
begin
    if not FieldEditOk then Exit;
    FDatasetField.Text := Value;
end;

function TosDbField.GetEditText: string;
begin
  if (DataSetField.DataType = ftMemo) then
     Result := DatasetField.AsString
  else
     Result := DatasetField.Text;
end;

function TosDbField.GetDisplayText: string;
begin
  Result := DatasetField.DisplayText;
end;

function TosDbField.GetAsBoolean: Boolean;
begin
    Result := DatasetField.AsBoolean;
end;

function TosDbField.GetAsCurrency: Currency;
begin
  Result := DatasetField.AsCurrency;
end;

function TosDbField.GetAsDateTime: TDateTime;
begin
    Result := DatasetField.AsDateTime;
end;

function TosDbField.GetAsFloat: Double;
begin
    Result := DatasetField.AsFloat;
end;

function TosDbField.GetAsInteger: Longint;
begin
    Result := DatasetField.AsInteger;
end;

function TosDbField.GetAsString: string;
begin
    Result := DatasetField.AsString;
end;

function TosDbField.GetAsVariant: Variant;
begin
  {  if FGrid.UseStandardScrolling
        then Result := DatasetField.AsVariant
        else Result := Text; }
  Result := DatasetField.AsVariant
end;

function TosDbField.IsBooleanField: Boolean;
begin
    Result := False;
end;

function TosDbField.IsIntegerField: Boolean;
begin
    Result := False;
end;

procedure TosDbField.SetAsBoolean(Value: Boolean);
begin
    if not FieldEditOk then Exit;
    DatasetField.AsBoolean := Value;
end;

procedure TosDbField.SetAsCurrency(Value: Currency);
begin
    if not FieldEditOk then Exit;
    DatasetField.AsCurrency := Value;
end;

procedure TosDbField.SetAsDateTime(Value: TDateTime);
begin
    if not FieldEditOk then Exit;
    DatasetField.AsDateTime := Value;
end;

procedure TosDbField.SetAsFloat(Value: Double);
begin
    if not FieldEditOk then Exit;
    DatasetField.AsFloat := Value;
end;

procedure TosDbField.SetAsInteger(Value: Longint);
begin
    if not FieldEditOk then Exit;
    DatasetField.AsInteger := Value;
end;

procedure TosDbField.SetAsString(const Value: string);
begin
    if not FieldEditOk then Exit;
    DatasetField.AsString := Value;
end;

procedure TosDbField.SetAsVariant(const Value: Variant);
begin
    if not FieldEditOk then Exit;
    DatasetField.AsVariant := Value;
end;

procedure TosDbField.SetLookupVarValue(Value: Variant);
begin
end;

procedure TosDbField.SetLookupValue(const Value: Variant);
begin
    if VarIsNull(Value) then
        ClearLookup
    else
        SetLookupVarValue(Value);
end;

function TosDbField.SetField(RecBuf: PChar; Value: string): Boolean;
begin
    Result := False;
end;

function TosDbField.GetFieldClass: TClass;
begin
    Result := DatasetField.ClassType;
end;

function TosDbField.GetDisplayLabel: string;
begin
    Result := DatasetField.DisplayLabel;
end;

procedure TosDbField.SetDisplayLabel(Value: string);
begin
    DatasetField.DisplayLabel := Value;
end;

function TosDbField.GetDisplayName: string;
begin
    Result := DatasetField.DisplayName;
end;

function TosDbField.GetFieldName: string;
begin
    Result := DatasetField.FieldName;
end;

function TosDbField.GetReadOnly: Boolean;
begin
    Result := DatasetField.ReadOnly;
end;

procedure TosDbField.SetReadOnly(Value: Boolean);
begin
    DatasetField.ReadOnly := Value;
end;

function TosDbField.GetAlignment: TAlignment;
begin
    Result := DatasetField.Alignment;
end;

procedure TosDbField.SetAlignment(Value: TAlignment);
begin
    DatasetField.Alignment := Value;
end;

function TosDbField.GetFieldNo: Integer;
begin
    Result := DatasetField.FieldNo;
end;

function TosDbField.GetDataType: TFieldType;
begin
    Result := DatasetField.DataType;
end;

function TosDbField.GetDisplayFormat: string;
begin
    Result := '';
end;

function TosDbField.GetEditFormat: string;
begin
    Result := '';
end;

function TosDbField.GetVisible: Boolean;
begin
    Result := DatasetField.Visible;
end;

procedure TosDbField.SetVisible(Value: Boolean);
begin
    DatasetField.Visible := Value;
end;

function TosDbField.GetDisplayWidth: Integer;
begin
    Result := DatasetField.DisplayWidth;
end;

procedure TosDbField.SetDisplayWidth(Value: Integer);
begin
    DatasetField.DisplayWidth := Value;
end;

function TosDbField.CanModify: Boolean;
begin
    Result := DatasetField.CanModify;
end;

function TosDbField.IsBlobField: Boolean;
begin
    Result := (DatasetField is TBlobField);
end;

function TosDbField.GetTransliterate: Boolean;
begin
    Result := False;
end;

procedure TosDbField.SetTransliterate(Value: Boolean);
begin
end;

function TosDbField.GetEditMask: string;
begin
    Result := DatasetField.EditMask;
end;

procedure TosDbField.SetEditMask(Value: string);
begin
    DatasetField.EditMask := Value;
end;

function TosDbField.GetControlType: TtsControlType;
begin
  if (Self.FDatasetField = Nil) then
     Result := ctText
  else
  begin
    case FDataSetField.DataType of
      ftDate, ftDateTime : Result := ctText;
      ftBoolean          : Result := ctCheck;
      ftGraphic          : Result := ctPicture;
      ftBlob             : if (TBlobField(FDataSetField).BlobType = ftMemo) or
                              (TBlobField(FDataSetField).BlobType = ftWideString) then
                              Result := ctText
                           else
                              Result := ctPicture;
    else
      Result := ctText;
    end;
  end;
end;

function TosDbField.GetMaxLength: Integer;
begin
    Result := 0;
end;

function TosDbField.GetIsNull: Boolean;
begin
  if (Self.DatasetField <> Nil) then
     Result := DataSetField.IsNull
  else
     Result := False;
  {  if FGrid.UseStandardScrolling
        then Result := DatasetField.IsNull
        else Result := not GetData(nil); }
end;

function TosDbField.GetLookup: Boolean;
begin
    Result := DatasetField.Lookup;
end;



{TosDBCol}

constructor TosDbCol.Create(Grid: TtsBaseGrid);
begin
    inherited Create(Grid);
    FDBField := nil;
    FFieldName := '';
    FAssignedValues := [];
end;

destructor TosDbCol.Destroy;
begin
    if Assigned(FDBField) and (FDBField.FCol = Self) then FDBField.FCol := nil;
    inherited Destroy;
end;

function TosDbCol.DefaultProps: Boolean;
begin
    Result := inherited DefaultProps;
    if Result then Result := (FAssignedValues = []);
end;

procedure TosDbCol.AssignProperties(Source: TtsCol);
begin
    inherited AssignProperties(Source);
    if Source is TosDbCol then
    begin
        SetField(TosDbCol(Source).DatasetField);
        FAssignedValues := TosDbCol(Source).FAssignedValues;
    end;
end;

procedure TosDbCol.Assign(Source: TPersistent);
var
    OldWidth: Integer;
    Invalidated: Boolean;
begin
    if Source is TosDbCol then
    begin
        if Source <> Self then
        begin
            OldWidth := FWidth;
            AssignProperties(TosDbCol(Source));
            CheckWidth(OldWidth);

            Invalidated := Grid.CheckInvalidateCol(DisplayCol);
            if Invalidated and (Grid.CurDisplayCol = DisplayCol) then
                Grid.RedisplayControl(True);
        end;
    end
    else
        inherited;
end;

function TosDbCol.Lookup: Boolean;
begin
    Result := False;
    if not Assigned(Field) then Exit;
    Result := Field.DatasetField.Lookup;
end;

procedure TosDbCol.Reset(Properties: TtsProperties);
begin
    inherited;

    if prWidth in Properties then ResetWidth;
    if prVisible in Properties then ResetVisible;
    if prAlignment in Properties then ResetAlignment;
    if prMaxLength in Properties then ResetMaxLength;
    if prAllowGrayed in Properties then ResetAllowGrayed;
end;

function TosDbCol.ColumnSortName : String;
begin
  if (Trim(FieldName) <> '') then
     Result := Trim(FieldName)
  else
     Result := 'gdc' + IntToStr(DataCol);
end;

procedure TosDbCol.SetFieldName(Value: string);
var
    AField: TField;
begin
    if (FFieldName <> Value) or
       (Field = Nil) then
    begin
        AField := nil;

        if Assigned(Grid.DataLink.DataSet) and
           (not (csLoading in Grid.ComponentState)) and
           (Length(Value) > 0) then
           AField := Grid.DataLink.DataSet.FindField(Value);

        FFieldName := Value;
        SetField(AField);

        Grid.InvalidateCol(DisplayCol);
        if (Grid.CurDisplayCol = DisplayCol) then
           Grid.RedisplayControl(True);
    end;
end;

procedure TosDbCol.ResetField(ResetFieldName: Boolean);
begin
    FDBField := nil;
    if (Grid.FieldState <> fsCustomized) and
       ResetFieldName then
       FFieldName := '';
end;

function TosDbCol.SetField(Value: TField): Boolean;
var
    Index: Integer;
begin
    Result := False;
    if DatasetField <> Value then
    begin
        Result := True;
        ResetField(True);

        if Value <> nil then
        begin
            Index := Grid.FFieldList.FieldIndex(Value);
            if (Index >= 0) and (Index < Grid.FFieldList.Count) then
            begin
                FDBField := Grid.FFieldList[Index];
                FDBField.FCol := Self;
                FFieldName := FDBField.FieldName;
            end;
        end;
    end;
end;

procedure TosDbCol.SetDatasetField(Value: TField);
begin
    if SetField(Value) then
    begin
        Grid.InvalidateCol(DisplayCol);
        if (Grid.CurDisplayCol = DisplayCol) then
        begin
            Grid.RedisplayControl(True);
        end;
    end;
end;

function TosDbCol.GetDatasetField: TField;
begin
  Result := nil;
  if Assigned(Field) then
     Result := Field.DatasetField;
end;

function TosDbCol.GetDataType : TosDataType;
begin
  if (not Assigned(Field)) then
     Result := inherited GetDataType
  else
     case DataSetField.DataType of
       ftSmallInt : Result := dyInteger;
       ftInteger  : Result := dyInteger;
       ftWord     : Result := dyInteger;
       ftBoolean  : Result := dyBoolean;
       ftFloat    : Result := dyFloat;
       ftCurrency : Result := dyCurrency;
       ftBCD      : Result := dyFloat;
       ftDate, ftTime, ftDateTime : Result := dyDate;
       ftBytes, ftVarBytes  : Result := dyString;
       ftAutoInc            : Result := dyInteger;
       ftBlob, ftOraBlob    : Result := dyString;
       ftMemo               : Result := dyString;
       ftGraphic            : Result := dyPicture;
       ftWideString         : Result := dyString;
       ftLargeint           : Result := dyInteger;
       ftVariant            : Result := dyVariant;
{$IFDEF TSVER_V6}
       ftTimeStamp          : Result := dyDate;
{$ENDIF}
     else
       Result := dyString;
     end;
end;

function TosDbCol.GetVarType : Word;
begin
  if (DataSetField <> Nil) then
  begin
     case DataSetField.DataType of
       ftSmallInt : Result := varSmallInt;
       ftInteger  : Result := varInteger;
       ftBoolean  : Result := varBoolean;
       ftFloat    : Result := varSingle;
       ftCurrency : Result := varCurrency;
       ftBCD      : Result := varSingle;
       ftDate, ftTime, ftDateTime : Result := varDate;
       ftBytes, ftVarBytes  : Result := varByte;
       ftAutoInc            : Result := varInteger;
       ftBlob, ftOraBlob    : Result := varString;
       ftMemo               : Result := varString;
       ftGraphic            : Result := varString;
       ftWideString         : Result := varString;
       ftVariant            : Result := varVariant;
{$IFDEF TSVER_V6}
       ftTimeStamp          : Result := varDate;
       ftWord               : Result := varWord;
       ftLargeint           : Result := varInt64;
{$ENDIF}
     else
       Result := varString;
     end;
  end
  else
    Result := inherited GetVarType;
end;

function TosDbCol.GetDBGrid: TosCustomAdvDbGrid;
begin
    Result := TosCustomAdvDbGrid(inherited Grid);
end;

function TosDbCol.UseDisplayFormat : String;
begin
  if (Self.TextFormatting = tfGrid) or
     ((Self.TextFormatting = tfDefault) and
      (Self.Grid.TextFormatting = tfGrid)) then
      Result := Self.FDisplayFormat
  else if (Self.DatasetField <> Nil) then
  begin
    if (DatasetField is TNumericField) and
       (Trim(TNumericField(DatasetField).DisplayFormat) <> '') then
       Result := TNumericField(DatasetField).DisplayFormat
    else if (DatasetField is TFloatField) then
    begin
      if (TFloatField(DatasetField).currency) then
         Result := FormatSettings.CurrencyString + '#' + FormatSettings.ThousandSeparator + '#' + FormatSettings.DecimalSeparator + StringOfChar('0', FormatSettings.CurrencyDecimals) +
                   ';-' + FormatSettings.CurrencyString + '#' + FormatSettings.ThousandSeparator + '#' + FormatSettings.DecimalSeparator + StringOfChar('0', FormatSettings.CurrencyDecimals) +
                   ';' + FormatSettings.CurrencyString + '0' + FormatSettings.DecimalSeparator + StringOfChar('0', FormatSettings.CurrencyDecimals)
      else
         Result := '';
    end
    else
      Result := '';
  end
  else 
    Result := FDisplayFormat;
end;

function TosDbCol.GetDisplayFormat : String;
begin
  Result := FDisplayFormat;
end;

function TosDbCol.HalfMonth : String;

  function PullHalfMonth(forDateTime : TDateTime) : String;
  var wYr, wMn, wDy : Word;
      halfDate : TDateTime;
  begin
    DecodeDate(forDateTime, wYr, wMn, wDy);
    if (wDy >= 1) and (wDy <= 15) then
    begin
      wDy := 1;
      halfDate := EncodeDate(wYr, wMn, wDy);
    end
    else
    begin
      wDy := 16;
      halfDate := EncodeDate(wYr, wMn, wDy) - 1;
    end;
    Result := FormatDateTime('mmm d, yyyy', halfDate);
  end;

begin
  Result := '';
  // Result is either blank, 15th of the month of the last day of the month
  if (Field.DataType = ftDateTime) or
     (Field.DataType = ftDate) and
     (Field.AsDateTime > 0) then
     Result := PullHalfMonth(Field.asDateTime)
{$IFDEF TSVER_V6}
  else if (Field.DataType = ftTimeStamp) and
          (Field.DisplayText <> '') then
     Result := PullHalfMonth(SQLTimestampToDateTime(TSQLTimeStampField(Field.DataSetField).AsSQLTimeStamp));
{$ELSE}
;
{$ENDIF}
end;

function TosDbCol.Month : String;
begin
  Result := '';
  if (Field.DataType = ftDateTime) or
     (Field.DataType = ftDate) and
     (Field.AsDateTime > 0) then
     Result := FormatDateTime('mmmm', Field.AsDateTime)
{$IFDEF TSVER_V6}
  else if (Field.DataType = ftTimeStamp) and
          (Field.DisplayText <> '') then
     Result := FormatDateTime('mmmm', SQLTimestampToDateTime(TSQLTimeStampField(Field.DataSetField).AsSQLTimeStamp));
{$ELSE}
;
{$ENDIF}
end;

function TosDbCol.MonthNumber : String;
begin
  Result := '';
  if (Field.DataType = ftDateTime) or
     (Field.DataType = ftDate) and
     (Field.AsDateTime > 0) then
     Result := FormatDateTime('mm', Field.AsDateTime)
{$IFDEF TSVER_V6}
  else if (Field.DataType = ftTimeStamp) and
          (Field.DisplayText <> '') then
     Result := FormatDateTime('mmmm', SQLTimestampToDateTime(TSQLTimeStampField(Field.DataSetField).AsSQLTimeStamp));
{$ELSE}
;
{$ENDIF}
end;

function TosDbCol.MonthYear : String;
begin
  Result := '';
  if (Field.DataType = ftDateTime) or
     (Field.DataType = ftDate) and
     (Field.AsDateTime > 0) then
     Result := FormatDateTime('mmmm, yyyy', Field.AsDateTime)
{$IFDEF TSVER_V6}
  else if (Field.DataType = ftTimeStamp) and
          (Field.DisplayText <> '') then
     Result := FormatDateTime('mmmm, yyyy', SQLTimestampToDateTime(TSQLTimeStampField(Field.DataSetField).AsSQLTimeStamp));
{$ELSE}
;
{$ENDIF}
end;

function TosDbCol.YearQuarter : String;

  function PullYearQuarter(forDateTime : TDateTime) : String;
  var wYr, wMn, wDy : Word;
  begin
    DecodeDate(forDateTime, wYr, wMn, wDy);
    if (wMn >= 1) and
       (wMn <= 3) then
       Result := IntToStr(wYr) + ' Q1'
    else if (wMn >= 4) and
            (wMn <= 6) then
       Result := IntToStr(wYr) + ' Q2'
    else if (wMn >= 7) and
            (wMn <= 9) then
       Result := IntToStr(wYr) + ' Q3'
    else
       Result := IntToStr(wYr) + ' Q4';
  end;
begin
  Result := '';
  if (Field.DataType = ftDateTime) or
     (Field.DataType = ftDate) and
     (Field.AsDateTime > 0) then
     Result := PullYearQuarter(Field.AsDateTime)
{$IFDEF TSVER_V6}
  else if (Field.DataType = ftTimeStamp) and
          (Field.DisplayText <> '') then
     Result := PullYearQuarter(SQLTimestampToDateTime(TSQLTimeStampField(Field.DataSetField).AsSQLTimeStamp));
{$ELSE}
;
{$ENDIF}
end;

function TosDbCol.Quarter : String;

  function PullQuarter(forDateTime : TDateTime) : String;
  var wYr, wMn, wDy : Word;
  begin
    DecodeDate(forDateTime, wYr, wMn, wDy);
    if (wMn >= 1) and
       (wMn <= 3) then
       Result := 'Q1'
    else if (wMn >= 4) and
            (wMn <= 6) then
       Result := 'Q2'
    else if (wMn >= 7) and
            (wMn <= 9) then
       Result := 'Q3'
    else
       Result := 'Q4';
  end;
begin
  Result := '';
  if (Field.DataType = ftDateTime) or
     (Field.DataType = ftDate) and
     (Field.AsDateTime > 0) then
     Result := PullQuarter(Field.AsDateTime)
{$IFDEF TSVER_V6}
  else if (Field.DataType = ftTimeStamp) and
          (Field.DisplayText <> '') then
     Result := PullQuarter(SQLTimestampToDateTime(TSQLTimeStampField(Field.DataSetField).AsSQLTimeStamp));
{$ELSE}
;
{$ENDIF}
end;

function TosDbCol.DayOfWeekText : String;
var theDateTime : TDateTime;
begin
{$IFDEF TSVER_V6}
  if (Field.DataType = ftTimeStamp) then
     theDateTime := SQLTimestampToDateTime(TSQLTimeStampField(Field.DataSetField).AsSQLTimeStamp)
  else
{$ENDIF}  
     theDateTime := Field.AsDateTime;
  case DayOfWeek(theDateTime) of
    1: Result := 'Sunday';
    2: Result := 'Monday';
    3: Result := 'Tuesday';
    4: Result := 'Wednesday';
    5: Result := 'Thursday';
    6: Result := 'Friday';
    7: Result := 'Saturday';
  end;    // case
end;

function TosDbCol.WeekText : String;

  function PullWeekText(forDateTime : TDateTime) : String;
  var wYr, wMn, wDy : Word;
    iWeek : Integer;
  begin
    DecodeDate(forDateTime, wYr, wMn, wDy);
    iWeek := (wMn - 1) * 4 + Round(wDy / 7);
    Result := IntToStr(iWeek);
  end;
begin
  Result := '';
  if (Field.DataType = ftDateTime) or
     (Field.DataType = ftDate) and
     (Field.AsDateTime > 0) then
     Result := PullWeekText(Field.AsDateTime)
{$IFDEF TSVER_V6}
  else if (Field.DataType = ftTimeStamp) and
          (Field.DisplayText <> '') then
     Result := PullWeekText(SQLTimestampToDateTime(TSQLTimeStampField(Field.DataSetField).AsSQLTimeStamp));
{$ELSE}
;
{$ENDIF}
end;

function TosDbCol.GetDisplayText : String;
{$IFDEF TSVER_V6}
var tmpDateTime : TDateTime;
{$ENDIF}
begin
  if (DisplayFormat = '') then
  begin
    if (Field.DataType = ftMemo) then
       Result := Field.Text
    else
       Result := Field.DisplayText;
  end
  else
  begin
    if (Field.DataType = ftDate) or
       (Field.DataType = ftDateTime) then
    begin
      if (Field.AsDateTime = 0) then
         Result := ''
      else
         Result := FormatDateTime(DisplayFormat, Field.AsDateTime);
    end
{$IFDEF TSVER_V6}
    else if (Field.DataType = ftTimeStamp) then
    begin
      tmpDateTime := SQLTimeStampToDateTime(TSQLTimestampField(Field.DataSetField).AsSQLTimestamp);
      if (tmpDateTime = 0) then
         Result := ''
      else
         Result := FormatDateTime(DisplayFormat, tmpDateTime);
    end
{$ENDIF}
    else if (Field.DataType = ftInteger) then
       Result := Format(DisplayFormat, [Field.AsInteger])
    else if (Field.DataType = ftString) then
       Result := Format(DisplayFormat, [Field.AsString])
    else if (Field.DataType = ftFloat) then
       Result := FormatFloat(DisplayFormat, Field.AsFloat)
    else if (Field.DataType = ftCurrency) then
       Result := FormatFloat(DisplayFormat, Field.AsFloat)
    else if (Field.DataType = ftBoolean) then
       Result := Format(DisplayFormat, [Field.AsBoolean])
    else
       Result := Field.DisplayText;
  end;
end;

function TosDbCol.GetGroupText : String;
begin
  if (Field = Nil) and
     (TosAdvDbGrid(FGrid).StoreData) then
  begin
    Result := FormatText(TosAdvDbGrid(FGrid).InternalData.GetValue(DataCol, TosAdvDbGrid(FGrid).GridData.ActiveRow));
    exit;
  end;

{$IFDEF TSVER_V6}
  if (Field.DataType = ftTimestamp) then
     FtempDateTime := SQLTimestampToDateTime(TSQLTimestampField(Field.DataSetField).AsSQLTimeStamp)
  else {$ENDIF} if (Field.DataType = ftDateTime) or
          (Field.DataType = ftDate) then
     FtempDateTime := Field.AsDateTime
  else
  begin
    Result := DisplayText;
    exit;
  end;

   case GroupDateTimeSegment of
      dgoNone        : Result := FormatDateTime('yyyymmddhhnnss', FtempDateTime);
      dgoYear        : Result := FormatDateTime('yyyy', FtempDateTime);
      dgoYearMonth   : Result := FormatDateTime('yyyy/mm', FtempDateTime);
      dgoYearQuarter : Result := YearQuarter;
      dgoYearMonthDay: Result := FormatDateTime('yyyy/mm/dd', FtempDateTime);
      dgoQuarter     : Result := Quarter;
      dgoMonthNumber : Result := MonthNumber;
      dgoMonthName   : Result := FormatDateTime('mmmm', FtempDateTime);
      dgoHalfMonth   : Result := Self.HalfMonth;
      dgoWeek        : Result := WeekText;
      dgoDay         : Result := FormatDateTime('dd', FtempDateTime);
      dgoDayofWeek   : Result := DayOfWeekText;
   end;
end;

function TosDbCol.GetHeading: string;
begin
    Result := FHeading;
    if (Result = '') and Assigned(Field) then Result := Field.DisplayLabel;
    if (Result = '') and Assigned(Field) then Result := Field.FieldName;
    if (Result = '') then Result := FFieldName;
end;

function TosDbCol.GetAlignment: TAlignment;
begin
    Result := taLeftJustify;
    if (not Assigned(Field)) or (avAlignment in FAssignedValues) then
        Result := inherited GetAlignment
    else if Assigned(Field) then
        Result := Field.Alignment;
end;

function TosDbCol.GetHorzAlignment: TtsHorzAlignment;
begin
    Result := htaDefault;
    if (not Assigned(Field)) or (avAlignment in FAssignedValues) then
        Result := inherited GetHorzAlignment
    else if Assigned(Field) then
        Result := AlignmentToHorzAlignment(Field.Alignment, True);
end;

function TosDbCol.GetReadOnly: Boolean;
begin
    Result := inherited GetReadOnly;
    if Assigned(Field) then Result := Result or Field.ReadOnly
end;

procedure TosDbCol.KeepAssignedValues;
begin
  Include(FAssignedValues, avAlignment);
  Include(FAssignedValues, avWidth);
  Include(FAssignedValues, avVisible);
  Include(FAssignedValues, avMaxLength);
  Include(FAssignedValues, avControlType);
  Include(FAssignedValues, avAllowGrayed);
end;

procedure TosDbCol.SetAlignment(Value: TAlignment);
var
    OldAlignment: TAlignment;
begin
    OldAlignment := Alignment;
    Include(FAssignedValues, avAlignment);
    inherited SetAlignment(Value);

    if OldAlignment <> Alignment then
    begin
        Grid.InvalidateCol(DisplayCol);
        if (Grid.CurDisplayCol = DisplayCol) then Grid.RedisplayControl(True);
    end;
end;

procedure TosDbCol.SetHorzAlignment(Value: TtsHorzAlignment);
var
    OldAlignment: TtsHorzAlignment;
begin
    OldAlignment := HorzAlignment;
    Include(FAssignedValues, avAlignment);
    inherited SetHorzAlignment(Value);

    if OldAlignment <> HorzAlignment then
    begin
        Grid.InvalidateCol(DisplayCol);
        if (Grid.CurDisplayCol = DisplayCol) then Grid.RedisplayControl(True);
    end;
    Grid.LayoutChanged := True;
end;

procedure TosDbCol.SetWidth(Value: Integer);
begin
    Include(FAssignedValues, avWidth);
    inherited SetWidth(Value);
end;

procedure TosDbCol.SetVisible(Value: Boolean);
begin
    Include(FAssignedValues, avVisible);
    inherited SetVisible(Value);
end;

procedure TosDbCol.SetMaxLength(Value: Integer);
begin
    Include(FAssignedValues, avMaxLength);
    inherited SetMaxLength(Value);
end;

procedure TosDbCol.SetControlType(Value: TtsControlType);
begin
    Include(FAssignedValues, avControlType);
    inherited SetControlType(Value);
end;

procedure TosDbCol.SetAllowGrayed(Value: Boolean);
begin
    Include(FAssignedValues, avAllowGrayed);
    inherited SetAllowGrayed(Value);
end;

procedure TosDbCol.SetDBCombo(Value: TosDbCombo);
begin
  inherited SetCombo(Value);

  {if DbCombo <> Value then
  begin
    if Value = nil then
       ResetCombo
    else
    begin
      AssignCombo;
      FCombo.Assign(Value);
      if DisplayCol = Grid.CurDisplayCol then
         Grid.CheckResetComboInit(FCombo, False)
      else
         Grid.CheckLastUsedCombo(FCombo);

      if Grid.InDesignMode and (DisplayCol = Grid.CurDisplayCol) and
         (FCombo.DropDownStyle = ddDropDownList) then
      begin
        Grid.CurrentCell.SelectAll;
        Grid.RedisplayControl(True);
      end;
    end;
  end; }
end;

function TosDbCol.GetDBCombo: TosDbCombo;
begin
  Result := TosDbCombo(inherited GetCombo);
end;

{procedure TosDbCol.AssignCombo;
begin
  if not Assigned(FCombo) then
  begin
    FCombo := TosDbCombo(Grid.CreateCombo);
    FParentCombo := False;
    FCombo.DropDownStyle := FDropDownStyle;
    FCombo.AutoDropDown  := FAutoDropDown;
    FCombo.AutoAdvance   := FAutoAdvance;
    FCombo.ComboGrid.DefaultColWidth := Width + 10;
    if (Grid.CurDisplayCol = DisplayCol) then Grid.ResetComboInit(False);
  end;
end;

procedure TosDbCol.ResetCombo;
begin
  if Assigned(FCombo) then
  begin
    if (Grid.CurDisplayCol = DisplayCol) then
        Grid.RedisplayControl(True);
    Grid.CheckResetComboInit(FCombo, True);

    FCombo.Free;
    FCombo := nil;
  end;
end;}

procedure TosDbCol.SetComboDataSource(Value: TDatasource);
begin
  if Assigned(Combo) then
     TosDbCombo(Combo).Datasource := Value;
end;

function TosDbCol.GetComboDataSource: TDatasource;
begin
  Result := nil;
  if Assigned(Combo) then
     Result := Combo.DataSource;
end;

function TosDbCol.UseCheckBoxValues: Boolean;
begin
  Result := inherited UseCheckBoxValues;
  if Result and Assigned(Field) then Result := not (Field.IsIntegerField or Field.IsBooleanField);
end;

procedure TosDbCol.WriteAssignedValues(Writer: TWriter);
var
    PropStr: string;
begin
    SetString(PropStr, PChar(@FAssignedValues), SizeOf(FAssignedValues));
    Writer.WriteString(PropStr);
end;

procedure TosDbCol.ReadAssignedValues(Reader: TReader);
var
    PropStr: string;
begin
    PropStr := Reader.ReadString;
    FAssignedValues := [];
    CopyMemory(@FAssignedValues, PChar(PropStr), CalcMin(SizeOf(FAssignedValues), Length(PropStr)));
    InitField;
end;

procedure TosDbCol.DefineProperties(Filer: TFiler);
begin
    inherited;
    with Filer do
    begin
        DefineProperty('AssignedValues', ReadAssignedValues, WriteAssignedValues, True);
    end;
end;

procedure TosDbCol.ResetAlignment;
begin
    if avAlignment in FAssignedValues then
    begin
        Exclude(FAssignedValues, avAlignment);
        Grid.InvalidateCol(DisplayCol);
        if (Grid.CurDisplayCol = DisplayCol) then Grid.RedisplayControl(True);
    end;
end;

procedure TosDbCol.ResetAllowGrayed;
begin
    if avAllowGrayed in FAssignedValues then
    begin
        Exclude(FAssignedValues, avAllowGrayed);
        SetDefaultAllowGrayed;
    end;
end;

procedure TosDbCol.ResetControlType;
begin
    if avControlType in FAssignedValues then
    begin
        Exclude(FAssignedValues, avControlType);
        SetDefaultControlType;
    end;
end;

procedure TosDbCol.ResetWidth;
begin
    if avWidth in FAssignedValues then
    begin
        Exclude(FAssignedValues, avWidth);
        SetDefaultWidth;
    end;
end;

procedure TosDbCol.ResetVisible;
begin
    if avVisible in FAssignedValues then
    begin
        Exclude(FAssignedValues, avVisible);
        if Field <> nil then inherited SetVisible(Field.Visible);
    end;
end;

procedure TosDbCol.ResetMaxLength;
begin
    if avMaxLength in FAssignedValues then
    begin
        Exclude(FAssignedValues, avMaxLength);
        if Field <> nil then inherited SetMaxLength(Field.MaxLength);
    end;
end;

function TosDbCol.DefaultWidth: Integer;
var
    Width: Integer;
    RestoreCanvas: Boolean;
    Tm: TTextMetric;
begin
    if not Assigned(Field) then
    begin
        Result := Grid.DefaultColWidth;
        Exit;
    end;

    RestoreCanvas := not Grid.HandleAllocated;
    if RestoreCanvas then Grid.Canvas.Handle := GetDC(0);

    try
        if (FFont <> nil) and (not ParentFont) then
            Grid.Canvas.Font := Self.FFont
        else
            Grid.Canvas.Font := Grid.Font;

        GetTextMetrics(Grid.Canvas.Handle, TM);
        Result := Field.DisplayWidth * (Grid.Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 6;
        if Grid.HeadingOn then
        begin
            if HeadingFont <> nil then
                Grid.Canvas.Font := HeadingFont
            else
                Grid.Canvas.Font := Grid.HeadingFont;

            Width := Grid.Canvas.TextWidth(Heading) + 6;
            if Result < Width then Result := Width;
        end;
    finally
        if RestoreCanvas then
        begin
            ReleaseDC(0, Grid.Canvas.Handle);
            Grid.Canvas.Handle := 0;
        end;
    end;
end;

procedure TosDbCol.SetDefaultWidth;
begin
    if not (avWidth in FAssignedValues) then
    begin
        inherited SetWidth(DefaultWidth);
        Exclude(FAssignedValues, avWidth);
    end;
end;

procedure TosDbCol.SetDefaultVisible;
begin
    if not (avVisible in FAssignedValues) then
    begin
        if Assigned(Field) then
        begin
            inherited SetVisible(Field.Visible);
            Exclude(FAssignedValues, avVisible);
        end;
    end;
end;

procedure TosDbCol.SetDefaultControlType;
begin
    if not (avControlType in FAssignedValues) then
    begin
        if Assigned(Field) then
        begin
            inherited ControlType := Field.ControlType;
            Exclude(FAssignedValues, avControlType);
        end;
    end;
end;

procedure TosDbCol.SetDefaultMaxLength;
begin
    if not (avMaxLength in FAssignedValues) then
    begin
        if Assigned(Field) then
        begin
            inherited MaxLength := Field.MaxLength;
            Exclude(FAssignedValues, avMaxLength);
        end;
    end;
end;

procedure TosDbCol.SetDefaultAllowGrayed;
begin
    if not (avAllowGrayed in FAssignedValues) then
    begin
        if Assigned(Field) then
        begin
            if DatasetField is TBooleanField then
                inherited AllowGrayed := not TBooleanField(DatasetField).Required;
            Exclude(FAssignedValues, avAllowGrayed);
        end;
    end;
end;

procedure TosDbCol.InitField;
begin
    if Assigned(Field) then
    begin
        SetDefaultWidth;
        SetDefaultVisible;
        SetDefaultControlType;
        SetDefaultMaxLength;
        SetDefaultAllowGrayed;
    end;
end;

function TosDBCol.GetComboSQL: TStrings;
begin
  Result := Nil;
  if (Combo <> Nil) then
     Result := Combo.SQL;
end;

procedure TosDBCol.SetComboSQL(const Value: TStrings);
begin
  if (Combo <> Nil) then
     Combo.SQL := Value;
end;

{TosDBGridCols}

function TosDBGridCols.CreateCol: TtsCol;
begin
    Result := TosDbCol.Create(Grid);
end;

function TosDBGridCols.GetDBCol(DataCol: Integer): TosDbCol;
begin
    Result := TosDbCol(GetCol(DataCol));
end;

function TosDBGridCols.GetDBGrid: TosCustomAdvDbGrid;
begin
    Result := TosCustomAdvDbGrid(inherited Grid);
end;

procedure TosDBGridCols.SetFieldState(Value: TtsFieldState);
begin
    if Value <> FFieldState then
    begin
        FFieldState := Value;
        if FFieldState = fsDefault then
        begin
            Grid.FieldLayoutChanged(True, False, False);
            Grid.DoFieldLayoutChange;
        end;
    end;
end;

function TosDBGridCols.FindField(Field: TField): TosDbCol;
var
    DataCol: Longint;
begin
    Result := nil;
    DataCol := 0;
    while DataCol < Size - 1 do
    begin
        Inc(DataCol);
        if Field = Col[DataCol].DatasetField then
        begin
            Result := Col[DataCol];
            Break;
        end;
    end;
end;

procedure TosDBGridCols.Assign(srcCols: TtsGridCols);
begin
  inherited Assign(srcCols); 
end;

  {TosGridDataLink}

type
  TIntArray = array[0..MaxMapSize] of Integer;
  PIntArray = ^TIntArray;

constructor TosGridDataLink.Create(Grid: TosCustomAdvDbGrid);
begin
  inherited Create;
  FGrid := Grid;
  FStates := TStringList.Create;
end;

destructor TosGridDataLink.Destroy;
begin
    ClearMapping;
    FreeAndNil(FStates);
    inherited Destroy;
end;

function TosGridDataLink.AddMapping(const FieldName: string; Field: TField): Boolean;
var
    NewSize: Integer;
begin
  Result := True;
  if FFieldCount >= MaxMapSize then Exit;

  if SparseMap then
  begin
    if not Assigned(Field) then Field := DataSet.FindField(FieldName)
  end
  else
  begin
    if not Assigned(Field) then Field := DataSet.FieldByName(FieldName);
  end;

  if FFieldCount = FFieldMapSize then
  begin
    NewSize := FFieldMapSize;
    if NewSize = 0 then NewSize := 8
                   else Inc(NewSize, NewSize);

    if (NewSize < FFieldCount) then NewSize := FFieldCount + 1;
    if (NewSize > MaxMapSize)  then NewSize := MaxMapSize;

    ReallocMem(FFieldMap, NewSize * SizeOf(Integer));
    FFieldMapSize := NewSize;
  end;

  if Assigned(Field) then PIntArray(FFieldMap)^[FFieldCount] := Field.Index
                     else PIntArray(FFieldMap)^[FFieldCount] := -1;
  Inc(FFieldCount);
end;

procedure TosGridDataLink.ClearMapping;
begin
  if FFieldMap <> nil then
  begin
    FreeMem(FFieldMap, FFieldMapSize * SizeOf(Integer));
    FFieldMap := nil;
    FFieldMapSize := 0;
    FFieldCount := 0;
  end;
end;

function TosGridDataLink.GetFields(DataCol: Longint): TField;
begin
  Result := nil;
  if (DataCol >= 0) and (DataCol < FFieldCount) and
     (PIntArray(FFieldMap)^[DataCol] >= 0) then
      Result := DataSet.Fields[PIntArray(FFieldMap)^[DataCol]];
end;

procedure TosGridDataLink.ActiveChanged;
begin
  FGrid.LinkActive(Active);
end;

procedure TosGridDataLink.CheckBrowseMode;
begin
   FGrid.CheckBrowseMode;
end;

procedure TosGridDataLink.DataSetScrolled(Distance: Integer);
begin
  FGrid.DataSetScrolled(Distance);
end;

procedure TosGridDataLink.DataSetChanged;
begin
  FGrid.DataSetChanged;
  exit;


  if (FGrid.InSyncDataSet) then exit;

  if (Self.DataSet.State <> FLastState) then
  begin
    FLastState := Self.DataSet.State;
    case FLastState of
      dsInactive : FStates.Add('dsInactive');
      dsBrowse   : FStates.Add('dsBrowse');
      dsEdit     : FStates.Add('dsEdit');
      dsInsert   : FStates.Add('dsInsert');
      dsSetKey   : FStates.Add('dsSetKey');
      dsCalcFields : FStates.Add('dsCalcFields');
    else
      FStates.Add(IntToStr(Integer(FLastState)));
    end;
    FGrid.DataSetChanged;
  end
  else if (FGrid.FGridData.RecordCountsDiffer) then
    FGrid.DataSetChanged;
  FGrid.Invalidate;
end;

procedure TosGridDataLink.LayoutChanged;
begin
    //SaveState := DataSet.Active;
    FGrid.LinkActive(False);
    if DataSet.Active then
       FGrid.LinkActive(True);
end;

procedure TosGridDataLink.EditingChanged;
begin
  FGrid.EditingChanged;
  if (DataSet.State = dsEdit) then
     FLastState := dsEdit;
end;

procedure TosGridDataLink.RecordChanged(Field: TField);
begin
  FGrid.RecordChanged(Field);
end;

procedure TosGridDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    FGrid.UpdateData;
  finally
    FInUpdateData := False;
  end;
end;

function TosGridDataLink.GetMappedIndex(DataCol: Longint): Integer;
begin
    if (0 <= DataCol) and (DataCol < FFieldCount) then
        Result := PIntArray(FFieldMap)^[DataCol]
    else
        Result := -1;
end;

procedure TosGridDataLink.Reset;
begin
 { if FGrid.FDataModified then
     RecordChanged(nil)
  else
     Dataset.Cancel; }
end;

 { TosCustomAdvDbGrid }

constructor TosCustomAdvDbGrid.Create(AOwner : TComponent);
begin
  tgTraceEntry('TosCustomAdvDbGrid.Create Start');
  inherited Create(AOwner);
  //Rows := 0;

  FGridData := TosGridData.Create;
  FGridData.FGrid := Self;
  FGridData.FSortedData.FGrid := Self;
  FGridData.FSortedData.CaseInsensitive := Self.SortCaseInsensitive;
  FDataLink := TosGridDataLink.Create(Self);
  FGridData.FSortedData.FDataLink := FDataLink;
  FFieldList := TosFieldList.Create(Self);
  FInSyncDataset := 0;
  FInDatasetEvent := 0;
  FInsertingDataRow := 0;
  FDatabound := False;
  FSettingRowCount := False;
  InitActiveFields;
  FCurrentRowBookmark := nil;
  FAutoInsert := True;

  Self.HeadingButton := hbCell;
  tgTraceEntry('TosCustomAdvDbGrid.Create End');
end;

destructor TosCustomAdvDbGrid.Destroy;
begin
  tgTraceEntry('TosCustomAdvDbGrid.Destroy Start');
  FreeAndNil(FCellLoadBitmap);
  FreeAndNil(FDataLink);
  FGridData.FSortedData.FDataLink := Nil;
  FGridData.Free;
  ResetFields(True);
  FFieldList.Reset;
  FFieldList.Free;
  inherited Destroy;
  tgTraceEntry('TosCustomAdvDbGrid.Destroy End');
end;


function  TosCustomAdvDbGrid.GetInDatasetEvent: Boolean;
begin
    Result := (FInDatasetEvent <> 0)
end;

procedure TosCustomAdvDbGrid.SetInDatasetEvent(Value: Boolean);
begin
    if Value then
        Inc(FInDatasetEvent)
    else if FInDatasetEvent > 0 then
        Dec(FInDatasetEvent);
end;

function  TosCustomAdvDbGrid.GetInSyncDataset: Boolean;
begin
  Result := (FInSyncDataset <> 0)
end;

procedure TosCustomAdvDbGrid.SetInSyncDataset(Value: Boolean);
begin
  if Value then
     Inc(FInSyncDataset)
  else if (FInSyncDataset > 0) then
     Dec(FInSyncDataset);
end;

function TosCustomAdvDbGrid.CreateCols : TtsGridCols;
begin
  Result := TosDBGridCols.Create(ColCount, Self);
end;

function TosCustomAdvDbGrid.CreateCombo: TtsCombo;
begin
  Result := TosDbCombo.Create(Self);
end;

function TosCustomAdvDbGrid.CreateComboGrid: TtsBaseGrid;
begin
    Result := TosDbComboGrid.Create(ComboForm);
end;

function TosCustomAdvDbGrid.GetCombo: TtsCombo;
begin
  if FCombo = nil then CheckComboCreated;
  Result := TosDbCombo(FCombo); 
end;

function TosCustomAdvDbGrid.GiveCellCombo(DataCol, DataRow: Longint): TosDbCombo;
begin
  Result := nil;
  if (DataCol <= 0) or (DataCol > Cols) then Exit;
  if (DataRow <= 0) or (DataRow > RowCount - 1) then Exit;

  if (CellButtonType[DataCol,DataRow] = btCombo) and
     (not CellParentCombo[DataCol,DataRow] or InDesignMode) then
  begin
      Result := TosDbCombo(CellCombo[DataCol, DataRow])
  end
  else
  begin
    if FDrawOverlap = doDrawColOnTop then
    begin
      if (Self.Col[DataCol].ButtonType = btCombo) and
         (not Self.Col[DataCol].ParentCombo or InDesignMode) then
          Result := Self.Col[DataCol].Combo
      else if RowButtonType[DataRow] = btCombo then
          Result := TosDbCombo(RowCombo[DataRow]);
    end
    else
    begin
      if (RowButtonType[DataRow] = btCombo) and
         (not RowParentCombo[DataRow] or InDesignMode) then
          Result := TosDbCombo(RowCombo[DataRow])
      else if GridCols[DataCol].ButtonType = btCombo then
          Result := GridCols[DataCol].Combo;
    end;
  end;
end;

procedure TosCustomAdvDbGrid.ComboInit(DataCol, DataRow: Longint);
var CurCombo: TosDbCombo;
begin
  CheckComboCreated;
  FInComboInit := True;
  try
    if (not FComboInitialized) or InDesignMode then
    begin
      CurCombo := TosDbCombo(GiveCellCombo(DataCol, DataRow));
      if Assigned(CurCombo) and
         ((CurCombo <> FLastUsedCombo) or InDesignMode) then
      begin
        FLastUsedCombo := CurCombo;
        FActiveCombo := CurCombo;
        if (CurCombo.RefreshComboData) then
           CurCombo.RefreshData;
      end
      else
      begin
        if not Assigned(CurCombo) then
        begin
          if InDesignMode then
             Combo.Reset
          else if (FLastUsedCombo <> CurCombo) then
             Combo.Reset
          else
             Combo.InitCombo;
          Combo.DropDownStyle := GiveCellDropDownStyle(DataCol, DataRow);
          Combo.ComboGrid.DefaultColWidth := Round(ColWidths[GetDisplayCol(DataCol)] + 10);
          Combo.ComboGrid.DefaultRowHeight := 16; //CalcMax(FScaledRowHeight, Combo.FGrid.CalcTextHeight);
          //Combo.ComboGrid.UpdateScrollRange;
          CurCombo := TosDbCombo(Combo);
        end;
        FLastUsedCombo := TosDbCombo(CurCombo);
        //CurCombo.ComboGrid.Cols := 0;
        //CurCombo.ComboGrid.Cols := 1; // Resets invisible columns! TP removed Mar 17, 2005 as it cleared storedata
        FActiveCombo := CurCombo;
      end;

      ActivateComboInit(DataCol, DataRow);
      FComboInitialized := True;
    end;
  finally
    FInComboInit := False;
  end;
end;

procedure TosCustomAdvDbGrid.InitComboData(CellHasCombo: Boolean);
begin

end;

procedure TosCustomAdvDbGrid.SetDataSource(Value: TDataSource);
begin
  if Value <> FDataSource then
  begin
    FDataSource := Value;
    EnablePaint := False;
    try
      LinkActive(False);
      FDataBound := Assigned(Value);
      if (FDataBound and (Rows > 0)) then
         Rows := 0;
      try
        DataLink.DataSource := Value;
        FGridData.FDataSet := DataLink.DataSet;
      except on e: Exception do
        begin
          FDataBound := False;
          DataLink.DataSource := nil;
          raise;
        end;
      end;
      StoreData := False;

      if FieldState <> fsCustomized then
         ResetProperties(tsAllProperties)
      else
      begin
         ResetRowProperties(tsAllProperties);
         ResetCellProperties(tsAllProperties);
      end;

      FieldLayoutChanged(True, True, True); 
    finally 
      EnablePaint := True;
    end;
  end;
end;

function TosCustomAdvDbGrid.GetCellValue(DataCol: Longint; DataRow: Longint): Variant;
begin
    Result := inherited GetCellValue(DataCol, DataRow);
end;

procedure TosCustomAdvDbGrid.SetCellValue(DataCol: Longint; DataRow: Longint; const Value: Variant);
var
    ControlType: TtsControlType;
begin
    if (not DataBound) or StoreData then
       inherited SetCellValue(DataCol, DataRow, Value)
    else
    begin
      if (DataCol < 1) or (DataCol > Cols) then Exit;
      if (DataRow <> CurrentDataRow) then Exit;

      ControlType := GiveCellControlType(DataCol, DataRow);
      if ControlType = ctNone then Exit;

      if Assigned(Col[DataCol].Field) then
          SetFieldValue(Col[DataCol], ControlType, CheckStrValue(ControlType, Value));
    end;
end;

procedure TosCustomAdvDbGrid.SetFieldValue(Col: TosDBCol; ControlType: TtsControlType; const Value: Variant);
begin
    if not Assigned(Col.Field) then Exit;

    if VarIsNull(Value) then
        Col.Field.Value := Value
    else if VarIsEmpty(Value) and not (ControlType = ctText) then
        Col.Field.Value := Value
    else
    case ControlType of
        ctText:
            if Col.Field.IsBlobField then
               Col.Field.Value := Value
            else
               Col.Field.Text := Value;
        ctCheck:
            if Col.Field.IsBooleanField then
            begin
                if VarIsNull(Value) then
                    Col.Field.Value := Null
                else
                case Value of
                    Ord(cbChecked)  : Col.Field.Value := True;
                    Ord(cbUnchecked): Col.Field.Value := False;
                    Ord(cbGrayed)   : Col.Field.Value := Null;
                end;
            end
            else
                Col.Field.Value := Value;

        ctPicture:
            Col.Field.Assign(VariantToBitmap(Value));
    end;
end;

procedure TosCustomAdvDbGrid.SetComboValue(Value: Variant);
var lookupValue : String;
    theCombo : TosDbCombo;
begin
  theCombo := GiveCellCombo(CurrentDataCol, CurrentDataRow);
  if (theCombo <> Nil) and
     (theCombo.AutoLookup) then
  begin
    lookupValue := theCombo.DisplayText(Value);
    inherited SetComboValue(lookupValue);
  end
  else
     inherited SetComboValue(Value);
  SetFieldValue(Self.GridCols[CurrentDataCol], Self.GiveCellControlType(CurrentDataCol, CurrentDataRow), Value);
end;

function TosCustomAdvDbGrid.GetDBCol(DataCol: Variant): TosDBCol;
begin
  Result := TosDBCol(GetCol(DataCol));
end;

function TosCustomAdvDbGrid.GetCol(DataCol: Variant): TtsCol;
var colNumber : Integer;
begin
  Result := nil;
  case VarType(DataCol) of
    varString:
      begin
        Result := GridCols.FindCol(DataCol);
        if (Result = nil) then
        begin
          if (Copy(DataCol, 1, 3) = 'gdc') and
             (Length(DataCol) > 3) then
          begin
            colNumber := StrToInt(Copy(DataCol, 4, Length(DataCol)));
            if CheckIndex(itCol, colNumber, 1, ColCount - 1) then
               Result := GridCols.GetDbCol(colNumber);
          end
          else
             CheckRaise(Format(StsUnknownField, [DataCol]));
        end;
      end;
  else
      if CheckIndex(itCol, DataCol, 1, ColCount - 1) then
      begin
        //Index := DataCol;
        Result := GridCols.GetDbCol(DataCol);
      end;
  end;
end;

function TosCustomAdvDbGrid.GetFieldState : TtsFieldState;
begin
  Result := GridCols.FFieldState;
end;

procedure TosCustomAdvDbGrid.SetFieldState(Value : TtsFieldState);
begin
  if (Value <> FieldState) then
  begin
    GridCols.FieldState := Value;
  end;
end;

procedure TosCustomAdvDbGrid.SetSortCaseInsensitive(Value : Boolean);
begin
  inherited;
  FGridData.FSortedData.CaseInsensitive := Value;
end;

function TosCustomAdvDbGrid.StoreRows: Boolean;
begin
  Result := not Databound;
end;

function TosCustomAdvDbGrid.MaxDisplayRows: Integer;
var MaxRows: Double;
begin
  Result := 0;
  try
    if (not HandleAllocated) then Exit;

    MaxRows := (ClientHeight + FHorzLineWidth - FVertFixedHeight) / DefaultRowHeight;
    Result := Trunc(MaxRows);
    if Result <= 0 then
       Result := 1;
  except
  end;
end;

procedure TosCustomAdvDbGrid.DefineFieldMap;
var
  I: Integer;
begin
  tgTraceEntry('AdvDbGrid.DefineFieldMap');
  if FieldState = fsCustomized then
  begin
    DataLink.SparseMap := True;
    for I := 1 to Cols do
        DataLink.AddMapping(GridCols[I].FieldName, nil);
  end
  else
  begin
    FDataLink.SparseMap := False;
    with Datalink.Dataset do
    for I := 0 to FieldCount - 1 do
        Datalink.AddMapping(Fields[I].FieldName, Fields[I]);
  end;
end;

procedure TosCustomAdvDbGrid.ResetFields(ResetFieldName: Boolean);
var i: Longint;
begin
  tgTraceEntry('AdvDbGrid.ResetFields');
  for i := 1 to Cols do
    GridCols[I].ResetField(ResetFieldName);
end;

procedure TosCustomAdvDbGrid.DoFieldLayoutChange;
begin

end;

procedure TosCustomAdvDbGrid.FieldLayoutChanged(SetFields, ClearAll, CreateFields: Boolean);
var
  I: Longint;
  Field: TField;
  FieldCol: TosDbCol;
  Count, FieldCount: Longint;
  ColsEmpty: Boolean;
begin
  tgTraceEntry('AdvDbGrid.FieldLayoutChanged');
  //if not DataBound then Exit;
  if FInFieldLayout then Exit;

  FInFieldLayout := True;
  try
    EnablePaint := False;
    try
      DataLink.ClearMapping;
      if DataLink.Active then DefineFieldMap;
      if CreateFields or ClearAll then
      begin
        ResetFields(False);
        FFieldList.CreateFields;
      end;

      if FieldState = fsDefault then
      begin
        if ClearAll or (not SetFields) or (not DataLink.Active) then
        begin
         {Remove all fields}
          Cols := 0;
        end
        else
        begin
          {Remove fields that are no longer in the fieldmap}
          I := 1;
          FieldCount := Cols;
          for Count := 1 to FieldCount do
          begin
            Field := Dataset.FindField(GridCols[I].FieldName);
            if not Assigned(Field) then
               DoDeleteCols(I, I)
            else
            begin
              Col[I].ResetField(True);
              Col[I].SetField(Field);
              Inc(I);
            end;
          end;
          end;

          if SetFields and DataLink.Active then
          begin
            {Add fields that are not yet in the cols list}
            FieldCount := DataLink.FieldCount;
            ColsEmpty := (Cols = 0);
            if ColsEmpty then Cols := FieldCount;

            for I := 0 to FieldCount - 1 do
            begin
              Field := DataLink.Fields[I];
              if ColsEmpty then
                 FieldCol := nil
              else
                 FieldCol := GridCols.FindField(Field);

              if (not Assigned(FieldCol)) then
              begin
                if not ColsEmpty then
                   AddColumn(I + 1);
                FieldCol := GridCols[I + 1];
                FieldCol.SetField(Field);
                FieldCol.InitField;
                DoInsertCol(I + 1, False);
              end
              else
              begin
                FieldCol.SetDefaultWidth;
                FieldCol.SetDefaultVisible;
              end;
            end;
          end;
      end
      else
      begin
          if SetFields and Assigned(DataLink.DataSet) and
             (DataLink.Active or (not DataLink.Dataset.DefaultFields)) then
          begin
            for I := 1 to Cols do
            begin
              FieldCol := GridCols[I];
              FieldCol.ResetField(True);
              if FieldCol.FieldName <> '' then
                 FieldCol.SetField(DataLink.DataSet.FindField(FieldCol.FieldName));
              if (SetFields) then
                 FieldCol.InitField;
            end;
          end
          else
            ResetFields(True);
      end;

      if (Cols = 0) and ((not SetFields) or (not DataLink.Active)) and
         (FieldState = fsDefault) then
      begin
        Cols := DefaultColCount;
      end;
      Invalidate;

    finally
      EnablePaint := True;
    end;
  finally
    FInFieldLayout := False;
  end;
end;


function  TosCustomAdvDbGrid.StartCellEdit: Boolean;
begin
  Result := inherited StartCellEdit;
  if (DataSet <> Nil) and
     (not DataSet.Active) then
     Result := False;
end;

function  TosCustomAdvDbGrid.StartRowEdit: Boolean;
begin
  Result := True;
  if RowEditing then Exit;

  Result := inherited StartRowEdit;
  
  if Result and
     (DataSet <> Nil) and
     (DataSet.CanModify) then
  begin
    FGridData.PositionOnDataRow(CurrentDataRow);
    DataSet.Edit; //DataLink.Edit;
  end;
end;

function  TosCustomAdvDbGrid.EndCellEdit: Boolean;
var theField : TField;
    theValue : Variant;
    deltaValue, currValue : Double;
    bRefresh : Boolean;
    iLevel : Integer;
    theFooter : TosGroupFooterRow;
    theCombo : TosDbCombo;

  function ConvertVarToFloat(theVar : Variant) : Double;
  begin
    Result := 0;
    case VarType(theVar) of
      varInteger,
      varSmallInt  : Result := theVar;
      varDate      : Result := Double(theVar);
      varSingle,
      varDouble,
      varCurrency  : Result := theVar;
      varString    : if theVar <> '' then Result := StrToFloat(theVar);
{$IFDEF TSVER_V6}
      varShortInt  : Result := theVar;
      varWord      : Result := Double(theVar);
      varLongWord,
      varInt64     : Result := Double(theVar);      
{$ENDIF}
    end;
  end;

  procedure CalcBoundDelta;
  begin
    deltaValue := 0;
    theField := FGridData.CellField[CurrentDataCol, CurrentDataRow];
    if ((VarType(theValue) = varEmpty) or
        (VarType(theValue) = varNull)) or
       (VarToStr(theValue) = '') then
    begin
      if ((VarType(theField.Value) = varEmpty) or
          (VarType(theField.Value) = varNull)) or
          (VarToStr(theField.Value) = '') then
          deltaValue := 0
      else
      begin
        if (theField.DataType = ftInteger) or
           (theField.DataType = ftFloat) or
           (theField.DataType = ftDate) or
           (theField.DataType = ftDateTime) or
           (theField.DataType = ftCurrency) then
           deltaValue := 0 - theField.AsFloat;
        theField.Clear;
      end;
    end
    else
    begin
      DataSet.Edit;
      case theField.DataType of
        ftInteger  : begin
                       deltaValue := StrToInt(theValue) - theField.AsInteger;
                       theField.AsInteger  := StrToInt(theValue);
                     end;
        ftSmallInt : begin
                       deltaValue := StrToInt(theValue) - theField.AsInteger;
                       theField.AsInteger  := StrToInt(theValue);
                     end;
        ftDate,
        ftDateTime : begin
                       deltaValue := VarToDateTime(theValue) - theField.AsDateTime;
                       theField.AsDateTime := VarToDateTime(theValue);
                     end;
        ftBoolean  : theField.AsBoolean  := (Self.CellCheckBoxState[CurrentDataCol, CurrentDataRow] = cbChecked);
        ftFloat    : begin
                       deltaValue := TranslateValueAsFloat(theValue) - theField.AsFloat;
                       theField.AsFloat := TranslateValueAsFloat(theValue);
                     end;
        ftCurrency : begin
                       deltaValue := TranslateValueAsFloat(theValue) - theField.AsFloat;
                       theField.AsCurrency := TranslateValueAsFloat(theValue);
                     end;
      else
        theField.AsString := theValue;
      end;
    end;
  end;

  procedure CalcUnboundDelta;
  begin
    if (Col[CurrentDataCol].GroupSummaryOp <> gsoNone) then
    begin
      if ((VarType(theValue) = varEmpty) or
          (VarType(theValue) = varNull)) then
         deltaValue := 0 - ConvertVarToFloat(Self.FpreEditCellValue)
      else if ((VarType(FpreEditCellValue) = varEmpty) or
               (VarType(FpreEditCellValue) = varNull)) then
         deltaValue := currValue
      else
         deltaValue := currValue - ConvertVarToFloat(Self.FpreEditCellValue);
    end;
  end;
begin
  Result := False;
  if (csLoading in ComponentState) or
     (CurrentDataRow = 0) then exit;  // CurrentDataRow may = 0 when delete row in progress

  if (FGridData.SortedData.Count = 0) then
  begin
    if (not DataBound) then Result := inherited EndCellEdit;
    exit;
  end;

  theValue := Unassigned;
  theCombo := GiveCellCombo(CurrentDataCol, CurrentDataRow);
  if (theCombo <> Nil) and
     (theCombo.AutoLookup) then
     theValue := theCombo.SelectedValue;

  Result := inherited EndCellEdit;

  if Result then
  begin
    deltaValue := 0;
    if (StoreData) then
    begin
      if (Col[CurrentDataCol].GroupSummaryOp <> gsoNone) and
         (Col[CurrentDataCol].IsNumeric) then
      begin
        if (VarIsEmpty(theValue)) then
           theValue := Cell[CurrentDataCol, CurrentDataRow];
        currValue := CellValueAsFloat(CurrentDataCol, CurrentDataRow);
        CalcUnboundDelta;
      end;
    end
    else
    begin
      if (VarIsEmpty(theValue)) then
         theValue := Cell[CurrentDataCol, CurrentDataRow];
      CalcBoundDelta;
    end;

    if (deltaValue <> 0) then
    begin
      if (FGridData.SortedData.FootersOn) and
         (FGridData.SortedRow[CurrentDataRow].GroupHeader <> Nil) and
         (FGridData.SortedRow[CurrentDataRow].GroupHeader.GroupFooter <> Nil) then
      begin
        bRefresh := False;
        theFooter := FGridData.SortedRow[CurrentDataRow].GroupHeader.GroupFooter;
        for iLevel := FGridData.SortedRow[CurrentDataRow].GroupLevel downto 1 do
        begin
          if (StoreData) then
             bRefresh := theFooter.AdjustSubTotal(ConvertVarToFloat(theValue), deltaValue, CurrentDataCol)
          else
             bRefresh := theFooter.AdjustSubTotal(theField.AsFloat, deltaValue, CurrentDataCol);
          if (theFooter.GroupHeader.GroupHeader = Nil) then break;
          theFooter := theFooter.GroupHeader.GroupHeader.GroupFooter;
        end;
        FGridData.SelectDataRow(CurrentDataRow);
        if (bRefresh) then
           InvalidateCol(CurrentDataCol);
      end;
    end;
  end;
end;

function  TosCustomAdvDbGrid.EndRowEdit: Boolean;
begin
  Result := inherited EndRowEdit;
  if (StoreData) then exit;
  if (FGridData.SortedData.Count = 0) then exit;
  
  if Result then
  begin
    PostData(False);
    Result := (Dataset.State = dsBrowse);
  end;
end;

function TosCustomAdvDbGrid.UndoRowEdit(ByUser: Boolean; RowEditing: Boolean): Boolean;
begin
  Result := inherited UndoRowEdit(ByUser, RowEditing);
  if (StoreData) then exit;
  if DataSet = Nil then exit;
  if not Active then exit;

  //ActiveRecord := -1;
  {Result := True;
  if RowEditing then
  begin
      DoUndoRowEdit(FCurBookmark, ByUser, Cancel);
      Result := not Cancel;
  end; }
  if not Result then PostData(True);
  if not Result then Result := (Dataset.State = dsBrowse);
end;

function TosCustomAdvDbGrid.CurrentBookMark : TBookmark; //TBookmarkStr;
begin
  Result := Self.FGridData.ActiveBookmark;
end;

procedure TosCustomAdvDbGrid.DeleteSelectedRows;
begin
  if (Self.SelectedRows.Count > 0) then
  begin
    inherited;
    Self.DataSetChanged;
  end;
end;

procedure TosCustomAdvDbGrid.DeleteRows(FromDataRow, ToDataRow: Longint);
var currRow, iDeleted, origRow : Integer;
    sortRow : TosDbSortedRow;
begin
  if (DataSet = Nil) then
  begin
    inherited;
    exit;
  end;

  if (not Self.Active) then
     raise Exception.Create('Error Deleting Rows - No Active DataSet associated with Grid!');

  BeginUpdate;
  try
    DataSet.DisableControls;
    iDeleted := 0;
    currRow := FromDataRow;
    origRow := FromDataRow;
    try
      while (currRow <= ToDataRow) do
      begin
        sortRow := GridData.SortedRow[currRow];
        DataLink.ActiveRecord := sortRow.ActiveRow;
        //FGridData.FSortedData.RemoveEntry(sortRow);
        DataSet.Delete;
        Inc(currRow);
        Inc(iDeleted);
      end;
    finally
      //FGridData.FSortedData.FBookmarks.Refresh;
      //FGridData.SetDataSetBufferCount(DataLink.RecordCount+1);
      Rows := Rows - iDeleted;
      if (origRow > Rows) then
         Self.CurrentDataRow := Rows
      else
         Self.CurrentDataRow := origRow;
      DataSet.EnableControls;
    end;
  finally
    EndUpdate;
  end;

end;

procedure TosCustomAdvDbGrid.InsertRow(DisplayRow: Longint);
var OldRowCount: Longint;
begin
  if (DataSet = Nil) then
  begin
    if (StoreData) then
    begin
      BeginUpdate;
      FInsertingRow := True;
      try
          OldRowCount := Rows;
          Rows := Rows + 1;
          if (OldRowCount <> Rows) then
          begin
              RowInserted(Rows, False);
              DisplayRownr[Rows] := Rows;
              UpdateScrollRange;              
              // Move SortedRow to DisplayRow position...
              if (DisplayRow <> Rows) then
                 GridData.FSortedData.Move(Rows-1, DisplayRow-1);
          end;
      finally
          FInsertingRow := False;
          CurrentCell.Refresh;
          EndUpdate;
      end;
    end
    else
      inherited;

    exit;
  end;
  if (not Self.Active) then
     raise Exception.Create('Error Inserting Row - No Active DataSet associated with Grid!');
  if (DisplayRow <= 0) or
     (DisplayRow > Rows) then
     raise Exception.Create('Error Inserting Row - Invalid Row parameter specified ' + IntToStr(DisplayRow));

  CurrentDataRow := DataRowNr[DisplayRow];
  FInsertingDataRow := Self.CurrentDataRow;
  BeginUpdate;
  try
    DataSet.DisableControls;
    try
      //for i := 0 to FGridData.FSortedData.SortFields.Count - 1 do
      //  currValues[i] := DataSet.FieldByName(FGridData.FSortedData.SortFields.SortEntry[i].FieldName).Value;
      DataSet.Append;
      //for i := 0 to FGridData.FSortedData.SortFields.Count - 1 do
      //   DataSet.FieldByName(FGridData.FSortedData.SortFields.SortEntry[i].FieldName).AsString := currValues[i];
    finally
      DataSet.EnableControls;
    end;
  finally
    EndUpdate;
  end;
end;

function TosCustomAdvDbGrid.RowForBookMark(bkMark : TBookmarkStr) : Integer;
var iRow : Integer;
begin
  Result := 0;
  for iRow := 1 to Rows do
    if (Self.GridData.SortedRow[iRow].KeyValue = bkMark) then
    begin
      Result := iRow;
      break;
    end;
end;

function TosCustomAdvDbGrid.BookMarkForRow(dataRow : Integer) : TBookmark; //TBookmarkStr;
var currRow : Integer;
begin
  currRow := FGridData.ActiveRow;
  try
    //FGridData.DataSet.DisableControls;
    InSyncDataSet := True;
    try
      FGridData.DataLink.ActiveRecord := Self.FGridData.FSortedData.SortedRow[dataRow].ActiveRow;
      Result := DataSet.Bookmark;
      FGridData.DataLink.ActiveRecord := currRow;
    finally
      InSyncDataSet := False;
      //FGridData.DataSet.EnableControls;
    end;
  except on E:Exception do
    raise Exception.Create('Error in TosCustomAdvDbGrid.BookMarkForRow...' + E.Message);
  end;
end;

procedure TosCustomAdvDbGrid.ComponentRemoved(AComponent: TComponent);
var FieldCol: TosDBCol;
begin
  if FDatalink <> nil then
  begin
    if (AComponent is TDatasource) and (AComponent = Self.DataSource)  then
       DataSource := nil
    else if (AComponent is TField) then
    begin
      FieldCol := GridCols.FindField(TField(AComponent));
      if Assigned(FieldCol) then
      begin
        FieldCol.ResetField(True);
        FieldLayoutChanged(True, False, True);
        DoFieldLayoutChange;
      end;
    end;
  end;
end;

procedure TosCustomAdvDbGrid.ComponentInserted(AComponent: TComponent);
begin
  if (csLoading in ComponentState) then Exit;
  if Dataset <> nil then
  begin
    if (AComponent is TField) then
    begin
      FieldLayoutChanged(True, False, True);
      DoFieldLayoutChange;
    end;
  end;
end;

procedure TosCustomAdvDbGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then ComponentRemoved(AComponent);
  if (Operation = opInsert) then ComponentInserted(AComponent);
end;

function TosCustomAdvDbGrid.GetDataset: TDataset;
begin
  Result := nil;
  if Assigned(DataLink) then
     Result := DataLink.Dataset;
end;

function TosCustomAdvDbGrid.GetGridDbCols : TosDBGridCols;
begin
  Result := TosDBGridCols(inherited GridCols);
end;

function TosCustomAdvDbGrid.GetSortEntry(index : Integer) : TosSortEntry;
begin
  Result := Nil;
  if (index < GroupSortCount) then
     Result := GridData.FSortedData.SortFields.SortEntry[index];
end;

procedure TosCustomAdvDbGrid.Loaded;
begin
  {if ReadBoundProps then
  begin
      if FResetReadDataBound then
      begin
          FReadBoundProps := False;
          FResetReadDataBound := False;
          CheckApplyProps(False);
      end;
  end;}
  tgTraceEntry('AdvDbGrid.Loaded');
  inherited;
  if (not StoreData) then
     FieldLayoutChanged(True, True, True);  // Changed 1st arg from False to True April 5, 2005
end;

procedure TosCustomAdvDbGrid.InitActiveFields;
begin
    //ClearCurPosition;
    FEditing := False;
    //FStartEditing := False;
    //FDataPosted := False;
    FEditState := dsBrowse;
    FClosingEdit := False;
    FOpeningEdit := False;
    //FCancelReread := False;
    FDataEditMode := demNone;
    //FOldRowCount := 0;
    //FOldLeftCol := 0;
    FUpdateDataDone := False;
    FRecordChangedDone := False;
    FCheckBrowseModeDone := False;
    FDataModified := False;
end;

procedure TosCustomAdvDbGrid.LinkActive(Value : Boolean);

  procedure BindGroupingSortingColumns;
  var i : Integer;
  begin
    Self.FGridData.FSortedData.BeginUpdate;
    try
      for i := 1 to Self.Cols do
      begin
        case Col[i].GroupSortOp of
          gsGroup : Self.FGridData.AddGroupField(Col[i].FieldName, Col[i].DataCol);
          gsSort  : Self.FGridData.AddSortField(Col[i].FieldName, Col[i].DataCol, stAscending);
        end;
      end;
    finally
      Self.FGridData.FSortedData.EndUpdate;
    end;
  end;
begin
  tgTraceEntry('AdvDbGrid.LinkActive');
  if Value <> FLinkActive then
  begin
    EnablePaint := False;
    try
      //ResetLastRow;
      //ResetLastCell;
      InitActiveFields;
      if (not Value) and
         (Self.RowEditing) and
         (CurrentDataRow > 0) and
         (CurrentDataRow <= Rows) then
      begin
        RowChanged[CurDataRow] := False;
        ResetEdit(CurrentDataRow);
      end;
      ClearCellBuffer;
      HideGridControl(True);
      if not FLinkActive then
      begin
        UndoInsertRow;
        SetCurrentPosition(CurDisplayCol, 0, True, True);
      end;

      //if InRowSelectMode then
      DeleteAllRowSelection;
      FLinkActive := Value;

      if not FLinkActive then
      begin
        FGridData.LinkActive(Value);
        FActive := False;
        UpdateRowCount(True);
        FieldLayoutChanged(True, True, True);
      end
      else
      begin
        FieldLayoutChanged(True, True, True);
        BindGroupingSortingColumns;
        InSyncDataset := True;
        try
          FGridData.LinkActive(Value);
          FActive := FGridData.DataSet.Active;
          UpdateRowCount(True);
        finally
          InSyncDataset := False;
        end;
        DataSetChanged;
      end;
      ShowGridControl;
      DisplayAsControl := True;
    finally
      EnablePaint := True;
      DoFieldLayoutChange;
      DeleteAllRowSelection;
      UpdateScrollRange;
      CheckRowColChanged;
      CheckRowSelection(True);
      SelectionsChanged(False);
      CheckTopLeftChanged(False);
      GridStatusChanged;
      ResetColProperties([prSortPicture]);
      if (Rows >= 1) then
         FCurDataRow := 1;
      Invalidate;
    end;    
  end;
end;

procedure TosCustomAdvDbGrid.RemoveSort(DataCol: Variant);
var curRow : Integer;
    sortEntry : TosSortEntry;
    columnName : String;
begin
  curRow := Self.CurrentDataRow;
  if (curRow = 0) then curRow := 1;
  if (VarType(DataCol) = varString) then
     columnName := DataCol
  else
     columnName := Col[DataCol].columnSortName;
  sortEntry := FGridData.SortedData.SortFields.SortEntryByName(columnName);
  if (sortEntry = Nil) then exit;

  if (FGridData.SortedData.SortFields.Count = 1) then
  begin
    ClearAll;
  end
  else
  begin
    FGridData.RemoveSort(columnName);
    Col[DataCol].SortPicture := spNone;
  end;
  Self.CurrentDataRow := curRow;
  if (GridMode = gmListBox) then
     RowSelected[curRow] := True;
  Self.PutCellInView(DataCol, curRow);
  Self.Invalidate;
end;

function TosCustomAdvDbGrid.LocateColumn(columnName : String) : TosDbCol;
var i : Integer;
begin
  Result := Nil;
  for i := 1 to Cols do
    if (AnsiCompareText(Col[i].ColumnSortName, columnName) = 0) then
    begin
      Result := Col[i];
      break;
    end;
end;

procedure TosCustomAdvDbGrid.LoadGridLayout;
var sFileName : String;
    origDataSource : TDataSource;
    origLayoutManager : TosCustomLayoutManager;
    bCancel : Boolean;
    currLayoutVersion : Integer;

  function GetErrorMessage(exceptionMsg : String) : String;
  begin
    if (Trim(FLayoutManager.ErrorMessage) = '') then
       Result := Format('Error saving grid layout on grid %s. Exception raised: %s.', [Self.Name, exceptionMsg])
    else
       Result := Format(FLayoutManager.ErrorMessage, [Self.Name, exceptionMsg]);
  end;

  procedure CheckForNewColumns;
  var i : Integer;
      sFieldName : String;
      newCol : TosDbCol;
  begin
    for i := 1 to FDesignGridCols.Size do
    begin
      sFieldName := FDesignGridCols.Col[i].FieldName;
      if (sFieldName <> '') and
         (Self.GridCols.FindCol(sFieldName) = Nil) then
      begin
        Cols := Cols + 1;
        newCol := Col[Cols];
        newCol.Copy(FDesignGridCols.Col[i]);
        newCol.DisplayCol := 1;
        newCol.Visible := False;
        newCol.DisplayCol := Cols;
      end;
    end;
  end;

begin
  if (FLayoutManager = Nil) or
     (FLoadingLayout) or
     (csDesigning in ComponentState) then
     exit;

  if (not AnsiSameText('TosAdvDbGrid', Self.ClassName)) then exit;

  // check local first, then central
  sFileName := FLayoutManager.AdjustedLocalPath + Self.Name + '.glf';
  if (not FileExists(sFileName)) then
  begin
    sFileName := FLayoutManager.AdjustedCentralPath + Self.Name + '.glf';
    if (not FileExists(sFileName)) then
       exit;
  end;

  bCancel := False;
  DoLoadLayout(Self.Name, bCancel);
  if bCancel then exit;

  FLoadingLayout := True;
  origDataSource := Self.DataSource;
  origLayoutManager := Self.LayoutManager;
  currLayoutVersion := Self.LayoutVersion;
  FDesignGridCols := TtsGridCols.Create(Cols, Self);
  FDesignGridCols.Assign(Self.GridCols);
  try
    Self.DataSource := Nil;
    try
      Self.LoadFromFile(sFileName, cmaNone);
    except on E:Exception do
      begin
        if (not LayoutManager.SilentFileErrors) then
           raise Exception.Create(GetErrorMessage(E.Message));
      end;
    end;
  finally
    Self.DataSource := Nil;
    Self.DataSource := origDataSource;
    Self.LayoutManager := origLayoutManager;
    if (currLayoutVersion > Self.LayoutVersion) then
    begin
      ResetDesignLayout;
      FreeAndNil(FDesignGridCols);
    end
    else
       CheckForNewColumns;
    LayoutVersion := currLayoutVersion;
    FLoadingLayout := False;
  end;
end;


procedure TosCustomAdvDbGrid.ApplyGroupingSorting;
begin
  if (FInSortOp) then exit;

  FInSortOp := True;
  try
    GridData.FReapplyFooters := GridData.FSortedData.FootersOn;
    GridData.Refresh;
  finally
    FInSortOp := False;
  end;
end;

procedure TosCustomAdvDbGrid.SortOnCol(DataCol: Variant; sortType : TosSortType; bClear : Boolean);
var curRow, i, theDataCol : Integer;
    bCancel : Boolean;
    sortEntry : TosSortEntry;
    theCol : TosDbCol;
begin
  if (InDesignMode) then exit;
  if (FInSortOp) then exit;

  if (not CanEndEdit(True)) then
     Exit;

  theDataCol := Col[DataCol].DataCol;
  theCol := Col[theDataCol];

  bCancel := False;
  DoSorting(theDataCol, bCancel);
  if (bCancel) then exit;

  if (theCol.FieldName = '') then
     sortEntry := FGridData.SortedData.SortFields.SortEntryByNumber(theCol.DataCol)
  else
     sortEntry := FGridData.SortedData.SortFields.SortEntryByName(theCol.FieldName);

  FInSortOp := True;
  try
    curRow := Self.CurrentDataRow;
    if (curRow = 0) then curRow := 1;
    if bClear then
    begin
      FGridData.ClearSorts;
      for i := 1 to Cols do
         if (Col[i].GroupSortOp = gsSort) then
            Col[i].ClearSortOp;
      sortEntry := Nil;
    end;
    
    if (sortEntry = Nil) or
       (sortEntry.GroupSort) then
       FGridData.AddSortField(theCol.ColumnSortName, theCol.DataCol, sortType)
    else if (sortEntry.SortMode <> sortType) then
       FGridData.ToggleSortOnCol(theDataCol);
    if bClear then ResetColProperties([prSortPicture]);
    if (sortType = stAscending) then
       theCol.SortPicture := spDown
    else
       theCol.SortPicture := spUp;
    theCol.FGroupSortOp := gsSort;
    LayoutChanged := True;

    Self.CurrentDataRow := curRow;
    Self.PutCellInView(theDataCol, curRow);
    CurrentCell.Refresh;
    GridData.SelectDataRow(curRow);
    ResetRowSelection;
    Self.Invalidate;
  finally
    FInSortOp := False;
  end;
end;

procedure TosCustomAdvDbGrid.ResetRowSelection;
var i : Integer;
begin
  DeleteAllRowSelection;
  for i := 1 to Rows do
    if (Self.GetSortedRow(i).Selected) then
       Self.RowSelected[i] := True;
end;

procedure TosCustomAdvDbGrid.GroupOnCol(dataCol : Variant; bClear : Boolean; dtsDateTimeSegment : TosDateTimeSegment);
var curRow, theDataCol : Integer;
    bCancel : Boolean;
    theCol : TosDbCol;
begin
  if (csLoading in ComponentState) or
     (InDesignMode) then exit;
  if (FInSortOp) then exit;

  if not CheckEndEdit(CurrentDataCol, CurrentDataRow, True) then
     Exit;     

  FInSortOp := True;   
  try   
    theDataCol := Col[DataCol].DataCol;
    theCol := Col[theDataCol];

    bCancel := False;
    DoGrouping(theDataCol, bCancel);
    if (bCancel) then exit;

    curRow := Self.CurrentDataRow;
    if (curRow = 0) then curRow := 1;
    BeginUpdate;
    try
      if bClear then Self.ClearGroups;
      theCol.FGroupDateTimeSegment := dtsDateTimeSegment;
      FGridData.AddGroupField(theCol.ColumnSortName, dataCol);
      LayoutChanged := True;
    finally
      EndUpdate;
    end;

    for curRow := curRow to Rows do
      if (Self.RowType(curRow) = rtData) then
         break;
    Self.CurrentDataRow := curRow;
    Self.PutCellInView(theDataCol, curRow);
    CurrentCell.Refresh;
    if (GridMode = gmListBox) then
       Self.RowSelected[curRow] := True;
    Self.Invalidate;
    GridData.SelectDataRow(curRow);
    ResetRowSelection;
    DoRowChanged(Self.CurDataRow, Self.CurDataRow);
  finally
    FInSortOp := False;
  end;
end;

procedure TosCustomAdvDbGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var dataCol, dataRow, displayCol : Integer;
begin
  Self.CellFromXY(X, Y, displayCol, dataRow);
  if (Self.SortOnHeadingClick) and
     (Self.MouseStatus <> msColResize) and
     (Self.MouseStatus <> msColSelect) and
     (Button = mbLeft) and
     (dataRow = 0) and
     (displayCol >= 1) and
     (displayCol = FDownDisplayCol) then
  begin
    dataCol := DataColnr[displayCol];
    if (Col[dataCol].SortPicture = spUp) or
       (Col[dataCol].SortPicture = spNone) then
       SortOnCol(dataCol, stAscending, Shift <> [ssCtrl])
    else
       SortOnCol(dataCol, stDescending, Shift <> [ssCtrl]);
    DoRowChanged(Self.CurDataRow, Self.CurDataRow);
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TosCustomAdvDbGrid.MoveFirst;
begin
    HideGridControl(True);
    MoveToPosition(dpTop, False);
    ShowGridControl;
end;

procedure TosCustomAdvDbGrid.MoveLast;
begin
    HideGridControl(True);
    MoveToPosition(dpBottom, False);
    ShowGridControl;
end;

procedure TosCustomAdvDbGrid.MoveBy(Count: Longint);
var CurActive: Integer;
begin
    if (Rows = 0) or
       (not Assigned(DataSet)) then
       exit;

    CurActive := FGridData.ActiveRecord;
    if (Count > 0) then
        Dataset.MoveBy(GridRows - CurActive + Count)
    else
        Dataset.MoveBy(-(CurActive - 1) + Count);
end;

procedure TosCustomAdvDbGrid.AdjustTopLeft(ACol, ARow: Longint; DoPaint: Boolean);
begin
    if (TopRow = ARow) and (LeftCol = ACol) then Exit;

    if not DoPaint then EnablePaint := False;
    try
        MoveTopLeft(ACol, ARow);
    finally
        if not DoPaint then EnablePaint := True;
    end;
end;

procedure TosCustomAdvDbGrid.MoveToPosition(Position: TtsDataPosition; ByUser: Boolean);
var
    DoUpdate: Boolean;
begin
    DoUpdate := False;
    if (Position = dpBottom) and
       ((not ByUser) or (not DataSet.Eof) or (Self.TopRow < GetMaxTopRow)) then
    begin
        if (not ByUser) or (not DataSet.Eof) then FGridData.MoveLast;
        CheckRowPosition;
        UpdateRowCount(False);
        AdjustTopLeft(LeftCol, FGridData.BufferRows - VisibleRowCount + 1 + InsertionRow, False);
        UpdateScrollRange;
        DoUpdate := True;
    end
    else if (Position = dpTop) and
            ((not ByUser) or (not DataSet.Bof) or (Self.TopRow <> FixedRows)) then
    begin
        if (not ByUser) or (not DataSet.Bof) then FGridData.MoveFirst;
        CheckRowPosition;
        UpdateRowCount(False);
        AdjustTopLeft(LeftCol, FixedRows, False);
        UpdateScrollRange;
        DoUpdate := True;
    end;

    if DoUpdate then
    begin
        EnablePaint := False;
        try
            FGridData.SetActiveRow(CurrentDataRow);
        finally
            EnablePaint := True;
        end;
        Invalidate;
    end;

    CheckRowSelection(False);
    SelectionsChanged(False);
    CheckTopLeftChanged(ByUser);
end;

function TosCustomAdvDbGrid.RawTextValue(dataCol, dataRow : Integer) : String;
var theValue : Variant;
begin
  theValue := '';
  if (dataCol <= Cols) then
  begin
    if (StoreData) then
       theValue := Cell[dataCol, dataRow]
    else if (DataBound) and
            (Self.GridCols[dataCol].Field <> nil) then
    begin
      GridData.PositionOnDataRow(dataRow);
      theValue := Self.GridCols[dataCol].Field.AsString;
    end;
  end;
  Result := VarToStr(theValue);
end;

function TosCustomAdvDbGrid.GetDataValue(DataCol, DataRow: Longint; ControlType: TtsControlType): Variant;
var
    ValueType: TtsValueType;
    internalRow : Integer;
begin
    Result := Unassigned;
    if StoreData then
    begin
        if (FGridData.SortedData.Count = 0) then
           internalRow := DataRow
        else
        begin
          if (FGridData.SortedRow[dataRow].IsData) then
             internalRow := FGridData.SortedRow[dataRow].ActiveRow
          else
          begin
            Result := '';
            exit;
          end;
        end;
        Result := InternalData.GetValue(DataCol, internalRow);
        ValueType := InternalData.GetValueType(DataCol, internalRow);
        if (ValueType = vtpPictureName) or
           ((ValueType = vtpString) and (ControlType = ctPicture)) then
        begin
            Result := CheckPictureValue(DataCol, DataRow, ControlType, Result);
        end;
    end;
end;

procedure TosCustomAdvDbGrid.DoGetDrawInfo(DataCol, DataRow: Longint; var DrawInfo: TtsDrawInfo);
var OldActive: Integer;
begin
  if (not DataBound) then
     inherited
  else
  begin
    OldActive := FGridData.ActiveRecord;
    InSyncDataset := True;
    try
      FGridData.PositionOnDataRow(DataRow);
      inherited;
    finally
      FGridData.SetActiveRow(OldActive);
      InSyncDataset := False;
    end;
  end;
end;

procedure TosCustomAdvDbGrid.Paint;
begin
    FLoadFirstRow := True;
    FLastRowLoaded := -1;
    inherited;
end;

procedure TosCustomAdvDbGrid.DoPaintCell(DataCol, DataRow: Longint; DrawRect:TRect;
 State: TtsPaintCellState; var Cancel: Boolean);
var OldActive: Integer;
begin
  if (StoreData or (DataSet=Nil)) then
  begin
    inherited;
    exit;
  end;

  if (not Active) or
     (csDestroying in ComponentState) then Exit;

  if (DataRow = 0) then
     inherited
  else
  begin
    OldActive := FGridData.ActiveRecord;
    if (OldActive < 0) then
       OldActive := 0;
    InSyncDataset := True;
    try
      FGridData.PositionOnDataRow(DataRow);
      inherited;
    finally
      FGridData.SetActiveRow(OldActive);
      InSyncDataset := False;
    end;
  end;
end;

procedure TosCustomAdvDbGrid.CellLoadedEvent(DataCol, DataRow: Longint; ControlType: TtsControlType; var Value: Variant);
var OldActive: Integer;
    theCombo, prevActiveCombo : TtsCombo;
begin
  if (StoreData or (DataSet=Nil)) then
  begin
    inherited;
    exit;
  end;

  if (not Active) or
     (csDestroying in ComponentState) then Exit;

  OldActive := FGridData.ActiveRecord;
  if (OldActive < 0) then
     OldActive := 0;
  InSyncDataset := True;
  try
    FGridData.PositionOnDataRow(DataRow);
    GetFieldValue(GridCols[DataCol], DataRow, True, ControlType, Value);
    if (not FAsCombo) and
       (GiveCellButtonType(DataCol, DataRow) = btCombo) then
    begin
      prevActiveCombo := FActiveCombo;
      try
        theCombo := GiveCellCombo(DataCol, DataRow);
        if (theCombo <> Nil) and
           (theCombo.AutoLookup) then
           Value := theCombo.DisplayText(Value);
      finally
        FActiveCombo := prevActiveCombo;
      end;
    end;
    DBCellLoaded(DataCol, DataRow, True, ControlType, Value);
  finally
    FGridData.SetActiveRow(OldActive);
    InSyncDataset := False;
  end;
end;

procedure TosCustomAdvDbGrid.DBCellLoaded(DataCol: Longint; DataRow: Longint;
           CheckRownr: Boolean; ControlType: TtsControlType; var Value: Variant);
var origValue : Variant;

    {function ValuesDiffer : Boolean;
    var sVal1, sVal2 : String;
    begin
      if (VarType(Value) = VarType(origValue)) then
         Result := (VarCompareValue(Value, origValue) <> vrEqual)
      else
      begin
         sVal1 := VarToStr(Value);
         sVal2 := VarToStr(origValue);
         Result := (CompareStr(sVal1, sVal2) <> 0);
      end;
    end;}
begin
    if FLoadFirstRow or
       not IsSameRow(DataRow, FLastRowLoaded, CheckRownr) then
    begin
        FLastRowLoaded := DataRow;
        FLoadFirstRow := False;
        DoRowLoaded(DataRow);
    end;

    origValue := Value;
    FInDoCellLoaded := True;
    try
      DoCellLoaded(DataCol, DataRow, Value);
    finally
      FInDoCellLoaded := False;
    end;

    //if (ValuesDiffer) then
    {if not VarIsEmpty(Value) then
    begin
        CheckVarType(ControlType, Value);
        if (ControlType = ctText) and (VarType(Value) <> varString) then
            Value := CheckStrValue(ControlType, Value);
    end; }
end;

function TosCustomAdvDbGrid.IsSameRow(Rownr1, Rownr2 : Integer; CheckRownr : Boolean) : Boolean;
begin
  if CheckRownr then
     Result := (Rownr1 = Rownr2)
  else
     Result := False; //CompareBkm(Bkm1, Bkm2) = 0
end;

procedure TosCustomAdvDbGrid.GetFieldValue(Col: TosDBCol; Rownr: Longint; CheckRownr: Boolean; ControlType: TtsControlType; var Value: Variant);
var Bool: Boolean;
begin
  if (Col.Field <> nil) then
  begin
    case ControlType of
    ctText,ctMemo:
      begin
        if EditorActive and InEditMode and (Col.DisplayCol = CurDisplayCol) and
           IsSameRow(Rownr, CurDataRow, CheckRownr) and
           (not (csDesigning in ComponentState)) then
        begin
{$IFDEF TSVER_V6}
          if (Col.Field.DataType = ftTimestamp) then
             Value := Col.Field.AsDateTime
          else
{$ENDIF}          
             Value := Col.Field.Value;   // Instead of Text TP 2/2/2005
        end
        else
        begin
{$IFDEF TSVER_V6}
          if (Col.Field.DataType = ftTimestamp) then
             Value := Col.Field.AsDateTime
          else
{$ENDIF}          
             Value := Col.Field.Value;  // DisplayText TP 12/14/2004
        end;
      end;
    ctCheck:
      begin
        if Col.Field.IsBooleanField then
        begin
          if Col.Field.IsNull then
             Value := cbGrayed
          else
          begin
            Bool := Col.Field.AsBoolean;
            if Bool then Value := cbChecked
                    else Value := cbUnchecked;
          end;
        end
        else
          Value := Col.Field.AsVariant;
      end;
    ctPicture:
      begin
        if (Col.Field.ControlType = ctPicture) then
        begin
          if not Assigned(FCellLoadBitmap) then
             FCellLoadBitmap := TBitmap.Create;

          try
            FCellLoadBitmap.Assign(Col.DatasetField);
            Value := BitmapToVariant(FCellLoadBitmap);
          except
            Value := Unassigned;
          end;
        end
        else if (Col.Field.ControlType = ctText) then
           Value := Col.Field.Text;
      end;
    end;
  end;
end;

procedure TosCustomAdvDbGrid.OnGroupingSortingDialogClick(Sender : TObject);
begin
  OpenGroupingSortingDialog;
end;

procedure TosCustomAdvDbGrid.OpenGroupingSortingDialog;
begin
end;

procedure TosCustomAdvDbGrid.AddGridMenuItems(aPopupMenu : TPopupMenu);
var newMenuItem : TMenuItem;
    i : integer;

  function AddMenuItem(withText : String) : TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Tag := 999;
    Result.Caption := withText;
  	aPopupMenu.Items.Add(Result);
  end;
begin
  for i := 0 to aPopupMenu.Items.Count - 1 do
     if PopupMenu.Items[i].Tag = 999 then
        exit;

  i := aPopupMenu.Items.Count;
  inherited AddGridMenuItems(aPopupMenu);

  newMenuItem := AddMenuItem('Grouping && Sorting...');
  if (i = 0) then
     newMenuItem.MenuIndex := i
  else
     newMenuItem.MenuIndex := i + 1;
  newMenuItem.OnClick := OnGroupingSortingDialogClick;
end;

procedure TosCustomAdvDbGrid.UpdateRowCount(DoUpdate: Boolean);
begin
  if (StoreData) then
  begin
      if (GridRows <> FGridData.RecordCount) and
         (not FSettingRowCount) then
      begin
        FSettingRowCount := True;
        try
          GridRows := FGridData.RecordCount;
        finally
          FSettingRowCount := False;
        end;
        RecalcGrandTotal;
      end;
  end
  else
  begin
    if not Active then
    begin
       GridRows := 0;
       RecalcGrandTotal;
    end
    else
    begin
      if (GridRows <> FGridData.RecordCount) then
      begin
        GridRows := FGridData.RecordCount;
        RecalcGrandTotal;
      end;
    end;
  end;

  if DoUpdate then UpdateScrollRange;
end;

procedure TosCustomAdvDbGrid.CheckTopLeftChanged(ByUser: Boolean);
begin
  inherited CheckTopLeftChanged(ByUser);
end;

procedure TosCustomAdvDbGrid.ChangeAllRowHeights(NewHeight: integer; SetNewTop: Boolean);
begin
  inherited ChangeAllRowHeights(NewHeight, SetNewTop);
end;

procedure TosCustomAdvDbGrid.SingleRowHeightChanged(DisplayRow: Longint);
begin
  inherited SingleRowHeightChanged(DisplayRow);
  if (FGridData <> Nil) and
     (not Reading) and
     (DisplayRow > 0) then
  begin
    if (FGridData.FSortedData.Count > DisplayRow) then
    begin
      FGridData.SortedRow[DisplayRow].RowHeight := RowHeights[DisplayRow];
      FGridData.FSortedData.MixedRowHeights := True;
    end;
  end;
end;

function TosCustomAdvDbGrid.DoRowProps(Ancestor: TPersistent): Boolean;
begin
  Result := False;
end;

function TosCustomAdvDbGrid.DoCellProps(Ancestor: TPersistent): Boolean;
begin
  Result := False;
end;

procedure TosCustomAdvDbGrid.TopLeftChangedEvent(OldCol, OldRow, NewCol, NewRow: Longint;
                              ByUser: Boolean);
begin
  inherited TopLeftChangedEvent(OldCol, OldRow, NewCol, NewRow, ByUser);
end;

procedure TosCustomAdvDbGrid.UpdateRowSelection(dataRow : Integer; selected : Boolean);
var i : Integer;
begin
  if FAsCombo or
     FInSortOp then exit;

  if (dataRow > 0) then
     FGridData.SortedRow[dataRow].Selected := selected
  else if (dataRow = -1) then // Clear all
  begin
    for i := 1 to FGridData.FSortedData.Count do
      FGridData.SortedRow[i].Selected := False;
  end;
end;

function TosCustomAdvDbGrid.CheckRowChanged(var OldRow: Variant): Boolean;
begin
  Result := inherited CheckRowChanged(OldRow);
  if Result and
     (not InSyncDataSet) and
     (CurrentDataRow > 0) then
     FGridData.SelectDataRow(Self.CurrentDataRow);
end;

procedure TosCustomAdvDbGrid.RowDeleted(DataRow: Longint; ByUser: Boolean);
var theActiveRow : Integer;
    bResetGrid : Boolean;
begin
  bResetGrid := False;
  if (DataSet = Nil) then
  begin
    FGridData.FSortedData.RemoveEntry(Self.GetSortedRow(dataRow), True);
    inherited;
    exit;
  end;

  if (DataRow = 0) or
     (not Self.FGridData.SortedData.RowExists(DataRow)) then exit;
  
  BeginUpdate;
  InSyncDataset := True;
  try
    theActiveRow := Self.GetSortedRow(DataRow).ActiveRow;
    DataLink.ActiveRecord := theActiveRow;
    if (DataSet.State = dsInsert) then
    begin
      FGridData.FSortedData.RemoveEntry(Self.GetSortedRow(dataRow));
      Self.PostData(True);

      FGridData.FSortedData.FBookmarks.Refresh;
      FGridData.SetDataSetBufferCount(DataLink.RecordCount);
    end
    else
    begin
      try
        DataSet.Delete;
      except
        bResetGrid := True;
      end;
      //FGridData.SetDataSetBufferCount(DataLink.RecordCount);
    end;
    inherited RowDeleted(DataRow, ByUser);
  finally
    InSyncDataset := False;
    EndUpdate;
    if bResetGrid then
       DataSetChanged;
  end;
end;

procedure TosCustomAdvDbGrid.RowInserted(DataRow: Longint; ByUser: Boolean);
begin
  if (DataSet = Nil) then
  begin
     inherited;
     if (Self.StoreData) then
     begin
       FGridData.FSortedData.AddDataRow(DataRow, Format('%8.7d', [FGridData.FSortedData.Count]));
     end;
  end
  else if (DataSet.Active) then
  begin
    DataSet.Append;
    inherited RowInserted(DataRow, ByUser);
  end;
end;

function TosCustomAdvDbGrid.CtrlKeyDownVK_Delete: Boolean;
var  Msg: string;
begin
    Result := False;
    if not Active then Exit;
    if ReadOnly or FAsCombo then Exit;
    //if (GridStatus <> grRowSelect) then Exit;

    Msg := '';
    if (Self.SelectedRows.Count = 0) and
       (Self.CurrentDataRow > 0) and
       (Self.RowType(CurrentDataRow) = rtData) then
       Msg := 'Delete the current row?'
    else if (Self.SelectedRows.Count = 1) then
       Msg := 'Delete the selected row?'
    else if (Self.SelectedRows.Count > 1) then
       Msg := 'Delete all ' + IntToStr(SelectedRows.Count) + ' rows?';

    Result := True;
    if (not TosAdvDbGrid(Self).EditOptions.ConfirmDelete) or
       (MessageDlg(Msg, mtConfirmation, mbOKCancel, 0) <> idCancel) then
    begin
      if (Self.CellEditing) then
         EndEdit(True);
      if (Self.SelectedRows.Count = 0) and
         (Self.CurrentDataRow > 0) and
         (Self.RowType(CurrentDataRow) = rtData) then
         Self.DeleteRows(CurrentDataRow, CurrentDataRow)
      else
         Self.DeleteSelectedRows;
    end;
end;

procedure TosCustomAdvDbGrid.KeyDownVK_Down;
var InInsert: Boolean;
begin
    if not CanMoveToPos(CurDisplayCol, CurDisplayRow) then Exit;

    if not InEditMode and not FocusRectActive then
    begin
        VK_DownRowSelect;
        Exit;
    end;

    if CurDisplayRow <= 0 then Exit;
    //if not CanMoveDown then Exit;

    InInsert := FDataEditMode = demInsert;
    if not CheckEndEdit(GetDataCol(CurDisplayCol), -1, True) then Exit;
    //if InInsert and not FDataPosted then Exit;

    if (CurrentCell.DataRow = Rows) and
       (not FAsCombo) and
       (FAutoInsert or (GridMode = gmEditInsert)) then
       DataSet.Append
    else
       inherited;
end;

function TosCustomAdvDbGrid.KeyDownVK_Insert: Boolean;
begin
    Result := False;
    if not Active then Exit;
    if (GridMode in [gmBrowse, gmListBox]) or ReadOnly then Exit;

    if (Assigned(DataSet)) then
    begin
      FDataLink.DataSet.Insert;
      Result := True;
    end;
end;

procedure TosCustomAdvDbGrid.ProcessKeyDown(var Key: Word; Shift: TShiftState);
begin
    if Key = 0 then Exit;
    if not DataBound then begin inherited; Exit end;

    inherited ProcessKeyDown(Key, Shift);
    if (Shift = [ssCtrl]) and (Key = VK_DELETE) then
    begin
        if CtrlKeyDownVK_Delete then Key := 0;
    end;

    if (Shift = []) and (Key = VK_INSERT) then
    begin
        if KeyDownVK_Insert then Key := 0;
    end;
end;

procedure TosCustomAdvDbGrid.ActivateCellChanged(OldCol: Integer; OldRow: Variant);
begin
  inherited ActivateCellChanged(OldCol, OldRow);
end;

procedure TosCustomAdvDbGrid.CheckColSizes;
begin
  inherited CheckColSizes;
end;

function TosCustomAdvDbGrid.GroupNumber(DisplayRow : Longint) : Integer;
begin
  Result := FGridData.FSortedData.SortedRow[DisplayRow].GroupNumber;
end;

function TosCustomAdvDbGrid.GetGroupHeader(dataRow : Integer) : TosGroupHeaderRow;
begin
  Result := TosGroupHeaderRow(GetSortedRow(dataRow));
  if (Result.RowType <> rtGroupHeader) then
     Result := Nil;
end;

function TosCustomAdvDbGrid.GetGroupFooter(dataRow : Integer) : TosGroupFooterRow;
begin
  Result := TosGroupFooterRow(GetSortedRow(dataRow));
  if (Result.RowType <> rtGroupFooter) then
     Result := Nil;
end;

function TosCustomAdvDbGrid.RowType(DisplayRow : Longint) : TosRowType;
begin
  Result := rtData;
  if (FGridData <> Nil) and
     (FGridData.FSortedData <> Nil) and
     (DisplayRow > 0) and
     (DisplayRow <= FGridData.FSortedData.Count) then
     Result := FGridData.FSortedData.SortedRow[DisplayRow].RowType;
end;

function TosCustomAdvDbGrid.RowIsGroupHeader(DisplayRow : Longint) : Boolean;
begin
  Result := (RowType(DisplayRow) = rtGroupHeader);
end;

function TosCustomAdvDbGrid.RowIsGroupFooter(DisplayRow : Longint) : Boolean;
begin
  Result := (RowType(DisplayRow) = rtGroupFooter);
end;

function TosCustomAdvDbGrid.GroupHeaderCount : Integer;
begin
  Result := 0;
  if (FGridData <> Nil) and
     (FGridData.FSortedData <> Nil) then
     Result := GridData.SortedData.GroupCount;
end;

function TosCustomAdvDbGrid.GroupFooterCount : Integer;
begin
  Result := 0;
  if (FGridData <> Nil) and
     (FGridData.FSortedData <> Nil) and
     (FGridData.FSortedData.FootersOn) then
     Result := GridData.SortedData.GroupCount;
end;

function TosCustomAdvDbGrid.GetSortedRow(DisplayRow : Longint) : TosSortedRow;
begin
  Result := TosSortedRow(FGridData.FSortedData.SortedRow[DisplayRow]);
end;

function TosCustomAdvDbGrid.InternalRowForDataRow(dataRow : Longint) : Integer;
begin
  if (StoreData) then
  begin
    if (FGridData.SortedData.Count = 0) then
       Result := dataRow
    else
    begin
      if (dataRow <= FGridData.SortedData.Count) then
         Result := FGridData.SortedRow[dataRow].ActiveRow
      else
         Result := -1;
    end;
  end
  else
    Result := FGridData.SortedRow[dataRow].ActiveRow;
end;

procedure TosCustomAdvDbGrid.RecalcGrandTotal;
var dataCol, dataRow, currRow, exRow, exCol : Integer;
    theCol : TtsCol;
begin
  if (not TotalBandOn) then exit;

  if (DataSet = Nil) and
     (not StoreData) then exit;

  currRow := 0;
  if (DataSet <> Nil) then
  begin
    //DataSet.DisableControls;
    currRow := DataLink.ActiveRecord;
  end;
  try
    for dataCol := 1 to Cols do
      if (Col[dataCol].GroupSummaryOp <> gsoNone) then
         Col[dataCol].ResetSubTotal;
    exRow := 1; exCol := 1;
    try
      for dataRow := 1 to Rows do
      begin
        exRow := dataRow;
        for dataCol := 1 to Cols do
        begin
          exCol := dataCol;
          theCol := Col[dataCol];
          if (theCol.GroupSummaryOp <> gsoNone) and
             (getSortedRow(dataRow).IsData) then
          begin
            if (theCol.GroupSummaryOp = gsoCount) then
               theCol.IncRunningSubTotal(1)
            else
               theCol.IncRunningSubTotal(CellValueAsFloat(dataCol, dataRow));
          end;
        end;
      end;
    except on E:Exception do
      raise Exception.Create('TosCustomAdvDbGrid Error performing grand total operation at col, row ' + IntToStr(exCol) + ',' + IntToStr(exRow));
    end;
    for dataCol := 1 to Cols do
      if (Col[dataCol].GroupSummaryOp <> gsoNone) then
         Col[dataCol].CompleteGrandTotal;
  finally
    if (DataSet <> Nil) then
    begin
      DataLink.ActiveRecord := currRow;
      //DataSet.EnableControls;
    end;
  end;
end;

function TosCustomAdvDbGrid.CellValueAsFloat(dataCol, dataRow : Integer) : Double;
var OldActive: Integer;
    Value : Variant;
    theCol : TosDbCol;
begin
  if (Self.Col[dataCol].DatasetField <> Nil) then
  begin
    if (DataSet = Nil) or
       (not DataSet.Active) then
    begin
       Result := 0;
       exit;
    end;
    OldActive := FGridData.ActiveRow;
    if (OldActive < 0) then
       OldActive := 0;
    //FGridData.DataSet.DisableControls;
    InSyncDataset := True;
    try
      FGridData.PositionOnDataRow(DataRow);
      //Value := FGridData.CellValue[DataCol, DataRow];
      //GetFieldValue(GridCols[DataCol], DataRow, True, ControlType, Value);
      //DBCellLoaded(DataCol, DataRow, True, ControlType, Value);
      theCol := Col[dataCol];
      if (theCol.DatasetField <> Nil) then
      begin
        Value := theCol.DatasetField.Value;
{$IFDEF TSVER_V6}
        if (VarIsNumeric(Value)) then
           Result := theCol.DatasetField.AsFloat
        else
           Result := 0;
{$ELSE}
         Result := theCol.DatasetField.AsFloat;
{$ENDIF}
      end
      else
        Result := 0;
      FGridData.SetActiveRow(OldActive);
    finally
      InSyncDataset := False;
      //FGridData.DataSet.EnableControls;
    end;
  end
  else
    Result := inherited CellValueAsFloat(dataCol, dataRow);
end;

procedure TosCustomAdvDbGrid.ResetVisiblePointers;
begin
  inherited;
end;

procedure TosCustomAdvDbGrid.DoCellChanged(OldDataCol, NewDataCol: Longint; OldDataRow, NewDataRow: Variant); 
begin
  if Assigned(FOnCellChanged) and CanActivateEvent and (NewDataRow > 0) then
  begin
    FGridData.PositionOnDataRow(NewDataRow);
    FOnCellChanged(Self, OldDataCol, NewDataCol, OldDataRow, NewDataRow);
  end;
end;

procedure TosCustomAdvDbGrid.DoRowChanged(OldDataRow, NewDataRow: Variant);
begin
  if Assigned(FOnRowChanged) and CanActivateEvent and (NewDataRow > 0) then
  begin
    FGridData.PositionOnDataRow(NewDataRow);
    FOnRowChanged(Self, OldDataRow, NewDataRow);
  end;
end;

procedure TosCustomAdvDbGrid.CheckBrowseMode;
begin
  FCheckBrowseModeDone := True;
end;

procedure TosCustomAdvDbGrid.PostData(Cancelled: Boolean);
var activeRw, iPos : Integer;
    activeRec : TosDbSortedRow;
    bNewRec : Boolean;

  procedure ResetPointers;
  var i : Integer;
  begin
    InSyncDataset := True;
    try
      for i := 1 to FGridData.FSortedData.Count do
      begin
        activeRec := FGridData.SortedRow[i];
        if (activeRec.IsData) then
        begin
          DataSet.Bookmark := activeRec.KeyValue;
          activeRec.ActiveRow := DataLink.ActiveRecord;
        end;
      end;
    finally
      InSyncDataset := False;
    end;
  end;
begin
  if Cancelled then
  begin
    FGridData.FSortedData.FBookmarks.CurrentRowSelected := False;
    DataSet.Cancel;
  end
  else
  begin
    bNewRec := (DataSet.State = dsInsert);
    activeRw := DataLink.ActiveRecord;
    if (CurrentDataRow = 0) then // happens when error occurs and no row selected...
       CurrentDataRow := GridData.GridDataRowForActiveRow(activeRw);
    activeRec := GridData.SortedRow[CurrentDataRow];
    InDatasetEvent := True;
    try
      DataSet.Post;
    finally
      InDatasetEvent := False;
    end;
    if (bNewRec) then
    begin
      activeRec := FGridData.FInsertRow;
      FGridData.FInsertRow := Nil;
      FGridData.FSortedData.FBookmarks.CurrentRowSelected := True;
      iPos := FGridData.FSortedData.FBookmarks.FList.Count-1;
      FGridData.FSortedData.FBookmarks.FList.Objects[iPos] := activeRec;
    end;
    activeRec.FKeyValue := DataSet.Bookmark;
    activeRec.FBookmark := DataSet.GetBookmark;
    if (bNewRec) then
       ResetPointers;
    if bNewRec and
       (Self.GroupFieldCount > 0) then
       activeRec.ReCalcGroupNumber;
  end;
end;

procedure TosCustomAdvDbGrid.CheckRowPosition;
var gridRow : Integer;
begin
  if (Assigned(DataSet)) and
     (GridData.ActiveRow <> DataLink.ActiveRecord) then
  begin
    gridRow := GridData.GridDataRowForActiveRow(DataLink.ActiveRecord);
    if (gridRow <> CurrentDataRow) then
    begin
      InSyncDataSet := True;
      try
        PositionCurrentCell(CurrentDataCol, gridRow);
        UpdateScrollRange;
        FDataModified := False;
        CheckRowColChanged;
        CheckRowSelection(False);
        SelectionsChanged(False);
        CheckTopLeftChanged(False);
      finally
        InSyncDataSet := False;
        PutCellInView(CurrentDataCol, CurrentDataRow);
        Invalidate;
      end;
    end;
  end;
end;

procedure TosCustomAdvDbGrid.DataSetScrolled(Distance: Integer);
begin
  Self.FGridData.DataSetScrolled(Distance);
  FCurrentRowBookmark := DataLink.DataSet.Bookmark;
  CheckRowPosition;
end;

procedure TosCustomAdvDbGrid.InternalDataSetChanged;
begin
  if InSyncDataset or FInSortOp then exit;
  if InDataSetEvent then exit;
  if Reading then exit;

  InDatasetEvent := True;
  try
    BeginUpdate;
    try
      FGridData.FSortedData.FReloadData := True;
      FGridData.Refresh;
    finally
      if (CurrentDataRow > Self.Rows) then
         CurrentDataRow := Rows
      else if (CurrentDataRow = 0) and
              (Rows > 0) then
      begin
         CurrentDataRow := 1;
      end;
      EndUpdate;
    end;
  finally
    InDatasetEvent := False;
  end;
end;

procedure TosCustomAdvDbGrid.DataSetChanged;
begin
  if InSyncDataset then exit;
  if InDataSetEvent then exit;
  if Reading then exit;
  if (FDataEditMode = demAppend) or
     (FDataEditMode = demInsert) then exit;

  InDatasetEvent := True;
  try
    BeginUpdate;
    try
      FGridData.Clear; //CheckBookmarks;
      FGridData.FSortedData.FReloadData := True;
      FGridData.Refresh;
    finally
      if (CurrentDataRow > Self.Rows) then
         CurrentDataRow := Rows
      else if (CurrentDataRow = 0) and
              (Rows > 0) then
         CurrentDataRow := 1
      else
         CheckRowPosition;
      FCurrentRowBookmark := FGridData.ActiveBookmark;
      EndUpdate;
      CheckRowColChanged;
      CheckTopLeftChanged(True);
      CheckRowSelection(False);
      SelectionsChanged(False);
      if (CurrentDataCol > 0) and
         (CurrentDataRow > 0) then
      begin
        PutCellInView(CurrentDataCol, CurrentDataRow);
        if (AlwaysShowEditor) then
           CurrentCell.Refresh;
      end;
    end;
  finally
    InDatasetEvent := False;
    Invalidate;
  end;
end;

procedure TosCustomAdvDbGrid.UpdateData;
var
    Column: TosDBCol;
    Value: Variant;
begin
  FUpdateDataDone := FDataModified or Dataset.Modified;
  if not FCheckBrowseModeDone then
      FRecordChangedDone := FDataModified or Dataset.Modified;

  if FDataModified then
  begin
    Column := GridCols[GetDataCol(CurDisplayCol)];
    if CurrentCell.IsClear then
       Value := Unassigned
    else
    begin
      if CellEditing and not FInCheckEndEdit then
      begin
        if not CheckMaskValue then
           InvalidMaskOp(Format(ResourceStr(SFieldValueError), [Column.FieldName]));
      end;

      Value := CurrentCell.Value;
      if CurCellControlType = ctPicture then
         if VariantToBitmap(CurrentCell.Value).Empty then Value := Null;
    end;

    //DoOnUpdateField(CurDataCol, FCurBookmark, Value, Cancel);
    //if (not Cancel) then SetFieldValue(Column, CurCellControlType, Value);
    FDataModified := False;
  end;
end;

procedure TosCustomAdvDbGrid.EditingChanged;
begin
    if Editing <> DataLink.Editing then
    begin
        FClosingEdit := FEditing;
        FOpeningEdit := DataLink.Editing;
        FEditing := DataLink.Editing;
        FEditState := DataLink.Dataset.State;

        if FOpeningEdit then
        begin
            if DataLink.Dataset.State = dsInsert then
            begin
                CheckDropDownOff(False);
                //ClearCacheBuffers;
                if not DataLink.Dataset.Eof then
                   StartInsert
                else
                   StartAppend;
            end
            else
            begin
                FDataEditMode := demEdit;
                //FEditBookmark := FCurBookmark;
            end;
        end
        else
        begin
            if (CurDataRow > Rows) then exit;
            RowChanged[CurDataRow] := False;
            ResetEdit(CurDataRow);
            //FDataPosted := not CheckCanceled;

            case FDataEditMode of
                demInsert: EndInsert;
                demAppend: EndAppend;
                demEdit:   EndEditing;
            end;

            //if FDataEditMode <> demEdit then
            //   ResetInsertProperties;

            FDataEditMode := demNone;
            ClearCellBuffer;
            CheckDisableControl;
        end;

        DrawCurrentFocusRect(False);
        FUpdateDataDone := False;
        FRecordChangedDone := False;
        FCheckBrowseModeDone := False;
    end;
end;

procedure TosCustomAdvDbGrid.RecordChanged(Field: TField);
var FieldCol: TosDBCol;
begin
  if (not FDataModified) and InEditMode then
  begin
    if Assigned(Field) then
    begin
      FieldCol := GridCols.FindField(Field);
      if Assigned(FieldCol) then
      begin
        //if CheckClearBuffer(FieldCol) then
        //   SetControlSelectMode(tsCurrent);
      end;
    end;

    if (CurDataRow > GridRows) then
       GridRows := GridRows + 1;
  end;

  {if (not FOpeningEdit or (Dataset.State <> dsInsert)) and
     (not RowEditing) and Dataset.Modified then
  begin
    FRowEditing := True;
    FCurRowChanged := True;
  end; }

  InvalidateRow(CurDataRow);
  if not FInCheckEndEdit then
     FDataModified := False;
  //FRecordChangedDone := (not FOpeningEdit or (Dataset.State <> dsInsert)) and
  //                      Dataset.Modified;
end;

procedure TosCustomAdvDbGrid.SetDisplayRownr(DataRow: Longint; Value: Longint);
var
    MaxRow: Longint;
    OldDisplayRow: Longint;
begin
    if (DataRow < 1) or (DataRow > Rows) then Exit;
    if (Value < 1) or (Value > Rows) then Exit;

    if (GridData.SortedData.Count = 0) then
       inherited
    else
    begin
      BeginUpdate;
      try
        OldDisplayRow := GetSortedRow(DataRow).ActiveRow;
        GridData.SortedData.Move(DataRow-1, Value-1);
        ChangeRowHeights(DataRow, Value);
        ChangeSelectedRows(DataRow, Value);
        ChangeCurrentRowPosition(DataRow, Value);
        DoRowMoved(Value, 1, False);

        MaxRow := MaxVisibleRow;
        if (OldDisplayRow < FixedRows) or
           ((OldDisplayRow >= TopRow) and (OldDisplayRow <= MaxRow)) or
           (Value < FixedRows) or
           ((Value >= TopRow) and (Value <= MaxRow)) then
        begin
            Invalidate;
        end;

        UpdateScrollRange;
        CheckTopLeft(False);
      finally
        EndUpdate;
      end;
      RedisplayControl(True);
    end;
end;

function TosCustomAdvDbGrid.ReadOnly: Boolean;
begin
  Result := False;
  if DataSet.Active then
     Result := not DataLink.Dataset.CanModify;
end;

function TosCustomAdvDbGrid.GetGridRowCount: Longint;
begin
    Result := inherited GetGridRowCount;
end;

procedure TosCustomAdvDbGrid.SetGridRowCount(Value: Longint);
var I: Integer;
begin
  tgTraceEntry('AdvDbGrid.SetGridRowCount');
  {if (csLoading in ComponentState) or
     (csReading in ComponentState) then
     Value := 0; }
  if (Value < 0) then Value := 0;

  if (StoreData) then
  begin
    if (not FInSortOp) then
    begin
      if (Value = 0) then
      begin
        GridData.SortedData.ClearAll;
        GridData.SortedData.Clear;
      end;
    end;

    inherited;
    CheckTotalBand;
  end
  else
  begin
    if GridRows <> Value then
    begin
        if (Value < GridRows) and
           (Value <> 0) then
        begin
            for I := (RowCount - 1 - InsertionRow) downto (Value + 1) do
                DeleteAllAtRow(I, I + InsertionRow);
        end;

        EnablePaint := False;
        try
            SetNewRowCount(Value);
            CheckTopLeft(True);
        finally
            EnablePaint := True;
        end;
    end;
  end;
end;


procedure TosCustomAdvDbGrid.StartInsert;
var ActiveRec, CurRow, i: Longint;
begin
    FDataEditMode := demInsert;

    curRow := CurrentDataRow;
    ActiveRec := FDataLink.ActiveRecord;
    FGridData.SetDatasetBufferCount(FGridData.DataLink.BufferCount);

    for i := 1 to FGridData.FSortedData.Count do
      if (FGridData.SortedRow[i].ActiveRow >= ActiveRec) and
         (FGridData.SortedRow[i].RowType = rtData) then
         FGridData.SortedRow[i].FActiveRow := FGridData.SortedRow[i].FActiveRow + 1;
    FGridData.FInsertRow := FGridData.AppendCurrentRecord;
    UpdateRowCount(True);
    GridData.MoveRow(FGridData.FSortedData.Count, curRow);
    FInsertingDataRow := 0;
    curRow := FGridData.GridDataRowForActiveRow(ActiveRec);

    if GridMode = gmEditInsert then
    begin
        //if (NewRow = GridTopRow) or (GridTopRow = GetMaxTopRow) then
        //   ClearCurrent(CurDisplayRow);
        PositionCell(CurDisplayCol, CurRow);
        SetScrollMode(smGridControl);
        //try
        //    RepositionTopRow(GridTopRow, GetMaxTopRow, True);
        //finally
        //    SetScrollMode(smWindow);
        //end;
    end
    else
    begin
        //OldTopRow := GridTopRow;
        PositionCell(CurDisplayCol, CurRow);
        Self.StartEdit;
        //RepositionTopRow(GridTopRow, GetMaxTopRow, False);
        //Inc(ScrollCount, GridTopRow - OldTopRow);
    end;
    RedisplayControl(True);    
    ClearCellBuffer;
    Invalidate;
end;

procedure TosCustomAdvDbGrid.EndInsert;
begin
    //FCancelReread := True;
    //FScrollDataset.EndInsert(NewBkm, CheckCanceled, True, GridTopRow, RowMoved);
    UpdateRowCount(False);

    //NewRow := FScrollDataset.BufferRow(NewBkm);
    //if not CheckCanceled and RowMoved then
    //    AdjustTopLeft(LeftCol, NewRow, False)
    //else if CheckCanceled and FScrollDataset.AtEnd then
    //    AdjustTopLeft(LeftCol, GridTopRow + 1, False);

    Invalidate;
    FDataEditMode := demNone;
end;

procedure TosCustomAdvDbGrid.StartAppend;
var
    OldTopRow, CurRow: Longint;
    ScrollCount: Integer;
    curSortRow, sortRow : TosSortedRow;
begin
    FDataEditMode := demAppend;

    if (GridMode = gmEditInsert) then
    begin
        //SetInsertRowOff;
        //FScrollDataset.ResizeAppend(1);
    end;

    //CurRow := FScrollDataset.BufferRow(FCurBookmark);
    //FEditBookmark := FScrollDataset.AppendRecord(FCurBookmark);
    //NewRow := FScrollDataset.BufferRow(FEditBookmark);
    if (CurrentDataRow > 0) and
       (not Self.IsInsertRow(CurrentDataRow)) then
       curSortRow := getSortedRow(CurrentDataRow)
    else
       curSortRow := Nil;
    FDataLink.BufferCount := FDataLink.BufferCount + 1;
    FGridData.FInsertRow := FGridData.AppendCurrentRecord;
    sortRow := FGridData.FInsertRow;
    if (Self.GroupFieldCount > 0) and
       (curSortRow <> Nil) then
    begin
      sortRow.GroupNumber := curSortRow.GroupNumber;
      sortRow.GroupLevel  := curSortRow.GroupLevel;
    end;
    UpdateRowCount(True);
    curRow := FGridData.GridDataRowForActiveRow(FDataLink.ActiveRecord);
    if (FInsertingDataRow > 0) then
    begin
      GridData.MoveRow(FGridData.FSortedData.Count, FInsertingDataRow);
      curRow := FInsertingDataRow;
      FInsertingDataRow := 0;
    end;

    ScrollCount := 0;
    if (GridMode = gmEditInsert) then
    begin
        //if (NewRow = GridTopRow) or (GridTopRow = GetMaxTopRow) then
        //   ClearCurrent(CurDisplayRow);
        PositionCell(CurDisplayCol, curRow);
        SetScrollMode(smGridControl);
        //try
        //    RepositionTopRow(GridTopRow, GetMaxTopRow, True);
        //finally
        //    SetScrollMode(smWindow);
        //end;
    end
    else
    begin
        OldTopRow := TopRow;
        PositionCell(CurDisplayCol, CurRow);
        Self.StartEdit;
        //RepositionTopRow(GridTopRow, GetMaxTopRow, False);
        Inc(ScrollCount, Self.TopRow - OldTopRow);
    end;

    ClearCellBuffer;
    if (AlwaysShowEditor) then
      CurrentCell.Refresh;
    CurrentCell.MoveTo(CurDisplayCol, curRow);

    if (GridMode <> gmEditInsert) then
    begin
      Inc(ScrollCount, 1 - (TopRow + VisibleRowCount - 1 - CurDataRow));
      if (ScrollCount > 0) then MoveTopLeft(LeftCol, TopRow + ScrollCount);
    end;

    Invalidate;
    CheckTopLeftChanged(True);
    ChangeSelectedRow(CurrentDataRow);
end;

procedure TosCustomAdvDbGrid.EndAppend;
begin
    //FCancelReread := True;
    //OldTopRow := GridTopRow;
    //CurRow := FGridData.BufferRow(FEditBookmark);

    SetInsertRowOn;
    //UndoAppendedRow;

    //FScrollDataset.EndInsert(NewBkm, CheckCanceled, True, CalcMax(1, GridTopRow - 1), RowMoved);
    //NewRow := FGridData.BufferRow(NewBkm);
    UpdateRowCount(False);

    {if not CheckCanceled and (NewRow <> 0) then
    begin
        if RowMoved and (CurRow = NewRow + 1) then
            AdjustTopLeft(LeftCol, CalcMax(1, OldTopRow - 1), False)
        else if RowMoved then
            AdjustTopLeft(LeftCol, NewRow, False)
        else
            AdjustTopLeft(LeftCol, CalcMax(1, OldTopRow - 1), False);
    end
    else if (VisibleRows <= 1) and (InsertionRow <> 0) and (GridTopRow > 1) then
        AdjustTopLeft(LeftCol, 1, False);  }

    Invalidate;
    FDataEditMode := demNone;
end;

procedure TosCustomAdvDbGrid.EndEditing;
{var
    NewBkm: TBookmarkStr;
    NewRow, CurRow: Longint;
    RowMoved: Boolean;
    BufferMoved: Boolean; }
begin
    {FCancelReread := True;
    CurRow := FGridData.BufferRow(FEditBookmark);
    FGridData.ActiveRecord := FGridData.BufferRow(FEditBookmark);
    FScrollDataset.EndUpdate(NewBkm, GridTopRow, RowMoved, BufferMoved);

    if RowMoved then
        Invalidate
    else
        InvalidateRow(CurRow);}
end;

function TosCustomAdvDbGrid.GroupHeaderText(dataCol, dataRow : Integer) : String;
var thisRow, theDataRow : TosDbSortedRow;
    dateValue : TDateTime;
    theField : TField;
    theCol : TosDbCol;
begin
  Result := '';
  thisRow := TosDbSortedRow(GetSortedRow(dataRow));
  theCol := Col[dataCol];
  if (theCol.GroupDateTimeSegment = dgoNone) then
  begin
     Result := VarToStr(thisRow.FieldValue(theCol.ColumnSortName));
     if (theCol.ButtonType = btCombo) and
        (theCol.Combo <> Nil) and
        (theCol.Combo.AutoLookup) then
        Result := theCol.Combo.DisplayText(Result);
  end
  else
  begin
    if (thisRow.IsGroupHeader) then
       theDataRow := TosDbSortedRow(Self.GridData.SortedData.NextDataRow(dataRow))
    else
       theDataRow := thisRow;
    try
      if (StoreData) then
      begin
         Result := InternalData.GetValue(DataCol, theDataRow.ActiveRow);
         if (Result = '') then
            dateValue := 0
         else
            dateValue := StrToDate(Result);
      end
      else
      begin
        theField :=  theDataRow.FieldByName(theCol.ColumnSortName);
        if (theField <> Nil) then
           dateValue := theField.AsDateTime
        else
           dateValue := 0;
      end;
      Result := ExtractDateTimeSegment(dateValue, theCol.GroupDateTimeSegment);
    except
      Result := thisRow.FieldValue(theCol.ColumnSortName);
    end;
  end;
end;

function TosCustomAdvDbGrid.GroupFieldCount : Integer;
begin
  Result := FGridData.SortedData.SortFields.GroupCount;
end;

function TosCustomAdvDbGrid.SortFieldCount : Integer;
begin
  Result := FGridData.SortedData.SortFields.SortCount;
end;

function  TosCustomAdvDbGrid.GroupSortCount : Integer;
begin
  Result := GroupFieldCount + SortFieldCount;
end;

procedure TosCustomAdvDbGrid.AssignColsFromDbGrid(aDbGrid : TtsBaseGrid);
begin
  Self.AssignColProperties(aDbGrid);
end;

procedure TosCustomAdvDbGrid.ReGroupAndSort;
begin
  FGridData.FSortedData.RemoveNonDataRows;
  FGridData.Refresh;
  Invalidate;
end;

procedure TosCustomAdvDbGrid.AddGroupFooters;
begin
  FGridData.AddGroupFooters;
end;

procedure TosCustomAdvDbGrid.RemoveGroupFooters;
begin
  FGridData.RemoveGroupFooters;
end;

procedure TosCustomAdvDbGrid.RecalcSubTotals;
begin
  FGridData.RecalcFooterTotals;
  Invalidate;
end;

procedure TosCustomAdvDbGrid.ContractGroup(theGroupNumber, onRow : Integer);
begin
  FGridData.FSortedData.BeginGroupOp;
  try
    inherited ContractGroup(theGroupNumber, onRow);
  finally
    FGridData.FSortedData.EndGroupOp;
  end;
end;

procedure TosCustomAdvDbGrid.ExpandGroup(theGroupNumber, onRow : Integer);
begin
  FGridData.FSortedData.BeginGroupOp;
  try
    inherited ExpandGroup(theGroupNumber, onRow);
  finally
    FGridData.FSortedData.EndGroupOp;
  end;
end;

procedure TosCustomAdvDbGrid.ClearAll;
var i : Integer;
begin
  BeginUpdate;
  try
    FGridData.FSortedData.ClearAll;
    for i := 1 to Cols do
    begin
      if (not Col[i].Visible) and
         (Col[i].GroupSortOp = gsGroup) then
         Col[i].Visible := True; 
      Col[i].ClearSortOp;
    end;
    FGridData.FSortedData.Reload(False);
    FGridData.ResetRowCount;
    Self.LayoutChanged := True;
    SetGridRowCount(FGridData.FSortedData.Count);
    ResetColProperties([prSortPicture]);
  finally
    EndUpdate;
  end;
end;

procedure TosCustomAdvDbGrid.ClearGroups;
var i : Integer;
begin
  BeginUpdate;
  try
    FGridData.FSortedData.ClearGroups;
    for i := 1 to Cols do
      if (Col[i].GroupSortOp = gsGroup) then
      begin
        Col[i].ClearSortOp;
        Col[i].Visible := True;
      end;
    FGridData.FSortedData.Reload(False);
    SetGridRowCount(FGridData.FSortedData.Count);
  finally
    EndUpdate;
  end;
end;

procedure TosCustomAdvDbGrid.ClearSorts;
var i : Integer;
begin
  BeginUpdate;
  try
    FGridData.FSortedData.ClearSorts;
    for i := 1 to Cols do
      if (Col[i].GroupSortOp = gsSort) then
         Col[i].ClearSortOp;
    FGridData.FSortedData.SortFields.ClearSorts;
    FGridData.FSortedData.Reload(False);
    SetGridRowCount(FGridData.FSortedData.Count);
  finally
    EndUpdate;
  end;
end;


{ TosAdvDbGrid }

constructor TosAdvDbGrid.Create(AOwner : TComponent);
begin
  tgTraceEntry('AdvDbGrid.Create');
  inherited Create(AOwner);
  Width := 300;
  Height := 200;

  {FGrouperPanelOptions := TosGrouperPanelOptions.Create;
  FGrouperPanelOptions.FGrid := Self;
  FGrouperPanelOptions.Visible := True;
  FGrouperPanelOptions.Color := clBtnFace;
  FGrouperPanelOptions.Height := 35;

  FFooterPanelOptions := TosFooterPanelOptions.Create;
  FFooterPanelOptions.FGrid := Self;
  FFooterPanelOptions.Visible := True;
  FFooterPanelOptions.Color := clBtnFace;
  FFooterPanelOptions.Height := 35;  }

  FHeadingOptions := TosHeadingOptions.Create;
  FHeadingOptions.FGrid := Self;

  FEditOptions := TosEditOptions.Create;
  FEditOptions.FGrid := Self;

  FColumnOptions := TosColumnOptions.Create;
  FColumnOptions.FGrid := Self;

  FGridOptions := TosGridOptions.Create;
  FGridOptions.FGrid := Self;

  FSelectionOptions := TosSelectionOptions.Create;
  FSelectionOptions.FGrid := Self;

  FScrollingOptions := TosScrollingOptions.Create;
  FScrollingOptions.FGrid := Self;

  FRowOptions := TosRowOptions.Create;
  FRowOptions.FGrid := Self;

  FGroupingSortingOptions := TosGroupingSortingOptions.Create;
  FGroupingSortingOptions.FGrid := Self;

  FPrintOptions := TosPrintOptions.Create;
  FPrintOptions.FGrid := Self;

  FMemoOptions := TosMemoOptions.Create;
  FMemoOptions.FGrid := Self;
end;

destructor TosAdvDbGrid.Destroy;
begin
  tgTraceEntry('AdvDbGrid.Destroy Start');

  if GridLayoutRemembered and
     LayoutChanged then
     SaveGridLayout;

  //FGrouperPanelOptions.Free;
  //FFooterPanelOptions.Free;
  FreeAndNil(FHeadingOptions);
  FreeAndNil(FEditOptions);
  FreeAndNil(FScrollingOptions);
  FreeAndNil(FRowOptions);
  FreeAndNil(FGroupingSortingOptions);
  FreeAndNil(FPrintOptions);
  FreeAndNil(FColumnOptions);
  FreeAndNil(FSelectionOptions);
  FreeAndNil(FGridOptions);
  FreeAndNil(FMemoOptions);

  inherited Destroy;
  tgTraceEntry('AdvDbGrid.Destroy End', True);
end;

procedure TosAdvDbGrid.Loaded;
begin
  inherited;
  AdjustedGrouperPanel;
  AdjustedFooterPanel;
  //AdjustedHeadingOptions;
end;

{procedure TosAdvDbGrid.SetGrouperPanelOptions(Value : TosGrouperPanelOptions);
begin
  FGrouperPanelOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetFooterPanelOptions(Value : TosFooterPanelOptions);
begin
  FFooterPanelOptions.Assign(Value);
end;}

procedure TosAdvDbGrid.SetHeadingOptions(Value : TosHeadingOptions);
begin
  FHeadingOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetEditOptions(Value : TosEditOptions);
begin
  FEditOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetMemoOptions(Value : TosMemoOptions);
begin
  FMemoOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetColumnOptions(Value : TosColumnOptions);
begin
  FColumnOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetSelectionOptions(Value : TosSelectionOptions);
begin
  FSelectionOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetGridOptions(Value : TosGridOptions);
begin
  FGridOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetScrollingOptions(Value : TosScrollingOptions);
begin
  FScrollingOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetRowOptions(Value : TosRowOptions);
begin
  FRowOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetPrintOptions(Value : TosPrintOptions);
begin
  FPrintOptions.Assign(Value);
end;

procedure TosAdvDbGrid.SetGroupingSortingOptions(Value : TosGroupingSortingOptions);
begin
  FGroupingSortingOptions.Assign(Value);
end;

procedure TosAdvDbGrid.Invalidate;
begin
  inherited Invalidate;
end;

procedure TosAdvDbGrid.AdjustedGrouperPanel;
begin

end;

procedure TosAdvDbGrid.AdjustedFooterPanel;
begin

end;

procedure TosAdvDbGrid.OpenGroupingSortingDialog;
begin
  dgTgSort := TdgTgSort.Create(GetParentForm(Self));
  try
    dgTgSort.Execute(Self);
  finally
    dgTgSort.free;
  end;
end;

function TosAdvDbGrid.GetCopyAdvDbGrid : TosAdvDbGrid;
begin
  Result := Nil;
end;

procedure TosAdvDbGrid.SetCopyAdvDbGrid(aValue : TosAdvDbGrid);
begin
  if (aValue <> Nil) then
     Self.CopyGridProperties(aValue);
end;

{ TosSelectionOptions }

procedure TosSelectionOptions.Assign(srcSelectionOptions: TPersistent);
begin
  with TosSelectionOptions(srcSelectionOptions) do
  begin
    Self.CellSelectMode     := CellSelectMode;
    Self.ColSelectMode      := ColSelectMode;
    Self.GradientSelect     := GradientSelect;
    Self.RowSelectMode      := RowSelectMode;
    Self.SelectedAreaCursor := SelectedAreaCursor;
    Self.SelectFixed        := SelectFixed;
    Self.SelectionColor     := SelectionColor;
    Self.SelectionType      := SelectionType;
  end;
end;

function TosSelectionOptions.GetCellSelectMode: TtsCellSelectMode;
begin
  Result := FGrid.CellSelectMode;
end;

function TosSelectionOptions.GetColSelectMode: TtsColSelectMode;
begin
  Result := FGrid.ColSelectMode;
end;

function TosSelectionOptions.GetGradientSelect: TosGradientSelect;
begin
  Result := FGrid.GradientSelect;
end;

function TosSelectionOptions.GetRowSelectMode: TtsRowSelectMode;
begin
  Result := FGrid.RowSelectMode;
end;

function TosSelectionOptions.GetSelectedAreaCursor: TCursor;
begin
  Result := FGrid.SelectedAreaCursor;
end;

function TosSelectionOptions.GetSelectFixed: Boolean;
begin
  Result := FGrid.SelectFixed;
end;

function TosSelectionOptions.GetSelectionColor: TColor;
begin
  Result := FGrid.SelectionColor;
end;

function TosSelectionOptions.GetSelectionType: TtsSelectionType;
begin
  Result := FGrid.SelectionType;
end;

procedure TosSelectionOptions.SetCellSelectMode(Value: TtsCellSelectMode);
begin
  FGrid.CellSelectMode := Value;
end;

procedure TosSelectionOptions.SetColSelectMode(Value: TtsColSelectMode);
begin
  FGrid.ColSelectMode := Value;
end;

procedure TosSelectionOptions.SetGradientSelect(Value: TosGradientSelect);
begin
  FGrid.GradientSelect := Value;
end;

procedure TosSelectionOptions.SetRowSelectMode(Value: TtsRowSelectMode);
begin
  FGrid.RowSelectMode := Value;
end;

procedure TosSelectionOptions.SetSelectedAreaCursor(Value: TCursor);
begin
  FGrid.SelectedAreaCursor := Value;
end;

procedure TosSelectionOptions.SetSelectFixed(Value: Boolean);
begin
  FGrid.SelectFixed := Value;
end;

procedure TosSelectionOptions.SetSelectionColor(Value: TColor);
begin
  FGrid.SelectionColor := Value;
end;

procedure TosSelectionOptions.SetSelectionType(Value: TtsSelectionType);
begin
  FGrid.SelectionType := Value;
end;



{ TosScrollingOptions }

procedure TosScrollingOptions.Assign(srcScrollingOptions: TPersistent);
begin
  with TosScrollingOptions(srcScrollingOptions) do
  begin
    Self.ScrollBars    := ScrollBars;
    Self.ScrollSpeed   := ScrollSpeed;
    Self.ThumbTracking := ThumbTracking;
  end;
end;

function TosScrollingOptions.GetScrollBars: TScrollStyle;
begin
  Result := FGrid.ScrollBars;
end;

function TosScrollingOptions.GetScrollSpeed: TtsScrollSpeed;
begin
  Result := FGrid.ScrollSpeed;
end;

function TosScrollingOptions.GetThumbTracking: Boolean;
begin
  Result := FGrid.ThumbTracking;
end;

procedure TosScrollingOptions.SetScrollBars(const Value: TScrollStyle);
begin
  FGrid.ScrollBars := Value;
end;

procedure TosScrollingOptions.SetScrollSpeed(const Value: TtsScrollSpeed);
begin
  FGrid.ScrollSpeed := Value;
end;

procedure TosScrollingOptions.SetThumbTracking(const Value: Boolean);
begin
  FGrid.ThumbTracking := Value;
end;

{ TosRowOptions }

procedure TosRowOptions.Assign(srcRowOptions: TPersistent);
begin
  with TosRowOptions(srcRowOptions) do
  begin
    Self.AltRowColor         := AltRowColor;
    Self.FixedRowCount       := FixedRowCount;
    Self.HotTrack            := HotTrack;
    Self.ResizeRows          := ResizeRows;
    Self.ResizeRowsInGrid    := ResizeRowsInGrid;
    Self.RowBarAlignment     := RowBarAlignment;
    Self.RowBarDisplay       := RowBarDisplay;
    Self.RowBarIndicator     := RowBarIndicator;
    Self.RowBarOn            := RowBarOn;
    Self.RowBarWidth         := RowBarWidth;
    Self.RowChangedIndicator := RowChangedIndicator;
    Self.RowMoving           := RowMoving;
    Self.RowNavigation       := RowNavigation;
    Self.DefaultRowHeight    := DefaultRowHeight;
    Self.TabRowWrap          := TabRowWrap;
    Self.VertAlignment       := VertAlignment;
  end;
end;

function TosRowOptions.GetAltRowColor: TColor;
begin
  Result := FGrid.AltRowColor;
end;

function TosRowOptions.GetDefaultRowHeight: Integer;
begin
  Result := FGrid.DefaultRowHeight;
end;

function TosRowOptions.GetFixedRowCount: Integer;
begin
  Result := FGrid.FixedRowCount;
end;

function TosRowOptions.GetHotTrack: Boolean;
begin
  Result := FGrid.HotTrack;
end;

function TosRowOptions.GetResizeRows: TtsResizeRows;
begin
  Result := FGrid.ResizeRows;
end;

function TosRowOptions.GetResizeRowsInGrid: Boolean;
begin
  Result := FGrid.ResizeRowsInGrid;
end;

function TosRowOptions.GetRowBarAlignment: TtsVertAlignment;
begin
  Result := FGrid.RowBarAlignment;
end;

function TosRowOptions.GetRowBarDisplay: TosRowBarDisplay;
begin
  Result := FGrid.RowBarDisplay;
end;

function TosRowOptions.GetRowBarIndicator: Boolean;
begin
  Result := FGrid.RowBarIndicator;
end;

function TosRowOptions.GetRowBarOn: Boolean;
begin
  Result := FGrid.RowBarOn;
end;

function TosRowOptions.GetRowBarWidth: Integer;
begin
  Result := FGrid.RowBarWidth;
end;

function TosRowOptions.GetRowChangedIndicator: TtsRowChangedIndicator;
begin
  Result := FGrid.RowChangedIndicator;
end;

function TosRowOptions.GetRowMoving: Boolean;
begin
  Result := FGrid.RowMoving;
end;

function TosRowOptions.GetRowNavigation: TosRowNavigation;
begin
  Result := FGrid.RowNavigation;
end;

function TosRowOptions.GetTabRowWrap: Boolean;
begin
  Result := FGrid.TabRowWrap;
end;

function TosRowOptions.GetVertAlignment: TtsVertAlignment;
begin
  Result := FGrid.VertAlignment;
end;

procedure TosRowOptions.SetAltRowColor(const Value: TColor);
begin
  FGrid.AltRowColor := Value;
end;

procedure TosRowOptions.SetDefaultRowHeight(const Value: Integer);
begin
  FGrid.DefaultRowHeight := Value;
end;

procedure TosRowOptions.SetFixedRowCount(const Value: Integer);
begin
  FGrid.FixedRowCount := Value;
end;

procedure TosRowOptions.SetHotTrack(const Value: Boolean);
begin
  FGrid.HotTrack := Value;
end;

procedure TosRowOptions.SetResizeRows(const Value: TtsResizeRows);
begin
  FGrid.ResizeRows := Value;
end;

procedure TosRowOptions.SetResizeRowsInGrid(const Value: Boolean);
begin
  FGrid.ResizeRowsInGrid := Value;
end;

procedure TosRowOptions.SetRowBarAlignment(const Value: TtsVertAlignment);
begin
  FGrid.RowBarAlignment := Value;
end;

procedure TosRowOptions.SetRowBarDisplay(const Value: TosRowBarDisplay);
begin
  FGrid.RowBarDisplay := Value;
end;

procedure TosRowOptions.SetRowBarIndicator(const Value: Boolean);
begin
  FGrid.RowBarIndicator := Value;
end;

procedure TosRowOptions.SetRowBarOn(const Value: Boolean);
begin
  FGrid.RowBarOn := Value;
end;

procedure TosRowOptions.SetRowBarWidth(const Value: Integer);
begin
  FGrid.RowBarWidth := Value;
end;

procedure TosRowOptions.SetRowChangedIndicator(
  const Value: TtsRowChangedIndicator);
begin
   FGrid.RowChangedIndicator := Value;
end;

procedure TosRowOptions.SetRowMoving(const Value: Boolean);
begin
  FGrid.RowMoving := Value;
end;

procedure TosRowOptions.SetRowNavigation(const Value: TosRowNavigation);
begin
  FGrid.RowNavigation := Value;
end;

procedure TosRowOptions.SetTabRowWrap(const Value: Boolean);
begin
  FGrid.TabRowWrap := Value;
end;

procedure TosRowOptions.SetVertAlignment(const Value: TtsVertAlignment);
begin
  FGrid.VertAlignment := Value;
end;


  { TosDBCombo }

constructor TosDBCombo.Create(onParentGrid: TtsBaseGrid);
begin
  inherited Create(onParentGrid);
  FSQL := TStringList.Create;
  FRefreshComboData := True;
end;

destructor  TosDBCombo.Destroy;
begin
  FSQL.Free;
  inherited Destroy;
end;

function TosDBCombo.DisplayText(forLookupValue : Variant) : String;
var i, ComboRow : Integer;
    Found: Boolean;
begin
  Result := '';
  if AutoSearch <> asNone then
  begin
    TosCustomAdvDbGrid(ParentGrid).FActiveCombo := Self;
    if (FRefreshComboData) then
       RefreshData;
    ComboRow := FindComboRow(VartoStr(forLookupValue), True, False, Found);
    if (ComboRow > 0) and Found then
    begin
      for i := 1 to ComboGrid.Cols do
        if (ComboGrid.Col[i].Visible) then
           Result := Result + VarToStr(ComboGrid.Cell[i, ComboRow]) + ', ';

      if (Length(Result) > 0) then
         System.Delete(Result, Length(Result)-1, 2);
    end;
  end;
end;

procedure TosDBCombo.Assign(Source: TPersistent);
begin
  if Source is TosDbCombo then
  begin
    FDataSource := TosDbCombo(Source).DataSource;
    SQL.Clear;
    SQL.AddStrings(TosDbCombo(Source).SQL);
    FRefreshComboData := TosDbCombo(Source).FRefreshComboData;
  end;
  inherited;
end;

procedure TosDBCombo.SetDataSource(Value : TDataSource);
begin
  if (Value <> FDataSource) then
  begin
    FDataSource := Value;
    TosDbComboGrid(ComboGrid).DataSource := Value;
    {if (Value <> Nil) then
       Self.ComboGrid.StoreData := False;
    RefreshData;}
  end;
end;

function TosDBCombo.GetComboGrid : TosDbComboGrid;
begin
  Result := TosDbComboGrid(inherited GetComboGrid);
end;

function TosDBCombo.CanOpenSQL : Boolean;
begin
  Result := (TosAdvDbGrid(Self.FParentGrid).ComboQuery <> Nil);
end;

function TosDbCombo.OpenSQL(bClose : Boolean) : TDataSet;
var theDataSet : TDataSet;
    i : Integer;
    //adrPtr : Pointer;
    //SQLProc : TSQLProc;
begin
  theDataSet := TosAdvDbGrid(Self.FParentGrid).ComboQuery;
  //adrPtr := TosAdvDbGrid(Self.FParentGrid).ComboQuery.FieldAddress('SQL');
  //if (adrPtr = Nil) then
  //   raise Exception.Create('Error setting ComboSQL - associated ComboQuery must have a SQL property!');

  {@SQLProc := adrPtr;
  SQLProc(Nil);
  SQLProc(SQL);}
  //TQuery(theDataSet).SQL.Clear;
  //TQuery(theDataSet).SQL.Add(SQL.Text);
  IProviderSupport(theDataSet).PSSetCommandText(SQL.Text);
  theDataSet.Open;
  DropDownCols := theDataSet.FieldCount;
  ComboGrid.Cols := theDataSet.FieldCount;
  for i := 0 to theDataSet.FieldCount - 1 do
     ComboGrid.Col[i+1].FieldName := theDataSet.Fields.Fields[i].FieldName;
  Result := theDataSet;
  if (bClose) then
     theDataSet.Close;
end;

procedure TosDBCombo.RefreshData;
var i, iRow : Integer;
    fieldName : String;
    theField : TField;
    theDataSet : TDataSet;
begin
  theDataSet := Nil;
  if (not FRefreshComboData) then exit;

  if (Self.SQL.Count > 0) then
  begin
    if (not CanOpenSQL) then
       raise Exception.Create('TosAdvDbGrid error - Unable to execute Combo SQL');
    theDataSet := OpenSQL(False);
  end
  else if (FDataSource <> Nil) and
          (FDataSource.DataSet <> Nil) and
          (FDataSource.DataSet.Active) then
    exit
    //theDataSet := FDataSource.DataSet
  else if (ComboGrid.StoreData) then
  begin
    Self.FRefreshComboData := False;
    exit;
  end;

  if (theDataSet = Nil) or
     (Self.ComboGrid.Cols = 0) then
     Self.ComboGrid.Rows := 0
  else
  begin
    try
      with theDataSet do
      begin
        try
          DisableControls;
          First;
          ComboGrid.StoreData := True;
          ComboGrid.Rows := RecordCount;
          iRow := 1;
          while not eof do
          begin
            for i := 1 to ComboGrid.Cols do
            begin
              fieldName := ComboGrid.Col[i].FieldName;
              theField := FindField(fieldName);
              if (theField <> Nil) then
                 ComboGrid.SetDataValue(i, iRow, ctText, theField.AsString);
            end;
            Next;
            Inc(iRow);
          end;
          Self.FRefreshComboData := False;
        finally
          if (TosAdvDbGrid(Self.FParentGrid).ComboQuery = theDataSet) then
             theDataSet.Close;
          EnableControls;
        end;
      end;
    except
      raise Exception.Create('Error in Combo.RefreshData!');
    end;
  end;
end;

procedure TosDBCombo.InitCombo;
begin
  inherited;
  ComboGrid.FInSyncDataSet := 0;
  FDataSource := Nil;
end;

procedure TosDbCombo.SetSQL(Value: TStrings);
begin
  FSQL.BeginUpdate;
  try
    FSQL.Assign(Value);
  finally
    FSQL.EndUpdate;
  end;
end;

procedure TosDbCombo.ReloadWithSQL(sSQL: String);
begin
  if (not Self.CanOpenSQL) then
     raise Exception.Create('No ComboQuery associated with Grid!');
      
  SQL.Clear;
  SQL.Add(sSQL);
  RefreshComboData := True;
  RefreshData;
end;

{ TosDbSortedRows }

constructor TosDbSortedRows.Create;
begin
  inherited;
  //FDataLink := TDataLink.Create;
  FBookmarks := TosBookmarkList.Create;
end;

destructor TosDbSortedRows.Destroy;
begin
  //FDataLink.Free;
  FreeAndNil(FBookmarks);
  inherited;
end;

function TosDbSortedRows.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TosDbSortedRows.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if (Value = Nil) then
  begin
    Clear;
    FBookmarks.Clear;
    FBookmarks.FDataset := Nil;
  end
  else
  begin
    FBookmarks.FDataset := FDataLink.DataSet;
    FDataLink.BufferCount := FDataLink.DataSet.RecordCount;
    Reload(False);
  end;
end;

function TosDbSortedRows.DbRecSortText : String;
var j : Integer;
    sortValue : String;
    theField : TField;
begin
  Result := '';
  with FDataLink.DataSet do
  begin
    for j := 0 to SortFields.Count - 1 do
    begin
      theField := FieldByName(SortFields.SortEntry[j].FieldName);
      case theField.DataType of
        ftInteger  : sortValue := Format('%16.15d', [theField.AsInteger]);
        ftSmallint : sortValue := Format('%5.4d', [theField.AsInteger]);
        ftBoolean  : if theField.AsBoolean then sortValue := '1' else sortValue := '0';
        ftFloat    : begin
                       sortValue := Format('%32.' + IntToStr(TFloatField(theField).Precision) + 'f', [theField.AsFloat]);
                       sortValue := StringReplace(sortValue, ' ', '0', [rfReplaceAll]);
                     end;
        ftCurrency : sortValue := Format('%16.2f', [theField.AsFloat]);
        ftDate     : sortValue := FormatDateTime('yyyymmdd', theField.AsDatetime);
        ftDateTime : sortValue := FormatDateTime('yyyymmddhhnnss', theField.AsDatetime);
      else
        sortValue := Format('%-' + IntToStr(theField.DataSize) + 's', [theField.AsString]);
      end;
      if (SortFields.SortEntry[j].SortMode = stDescending) then
         Result := Result + ReverseText(sortValue)
      else
         Result := Result + sortValue;
    end;
  end;
end;

procedure TosDbSortedRows.RemoveDeletedRecords;
var i, j : Integer;
    theRow : TosDbSortedRow;
begin
  j := 0;
  for i := Count downto 1 do
  begin
    theRow := SortedRow[i];
    if (theRow.RowType = rtData) and
       (not FDataLink.DataSet.BookmarkValid(theRow.FBookmark)) then
    begin
      Delete(i-1);
      Inc(j);
      DataCount := DataCount - 1;
    end;
  end;
  if (j > 0) then
     Self.FBookmarks.Refresh;
end;

function TosDbSortedRows.BinarySearch(aName : TBookmarkStr) : Integer;
var i, iLo, iHi, iLast, iCompare : Integer;
begin
  // BinarySearch starts in the middle of the sorted list
  // and goes half way up or down until the item is found...
  Result := -1;
  if oldRows.Count = 0 then
     exit;

  iLo := 0; iLast := -1;
  iHi := oldRows.Count - 1;
  while (Result = -1) do
  begin
    i := ((iHi - iLo) SHR 1) + iLo;
    if i = iLast then  // Not found
    begin
      if (i > 0) and
         (oldRows.Strings[i] = aName) then
         Result := i - 1
      else if (i >= 0) and (i+1 < Count) and
              (oldRows.Strings[i] = aName) then
         Result := i + 1
      else if oldRows.IndexOf(aName) >= 0 then
         Result := oldRows.IndexOf(aName);
      break;
    end;
    iLast := i;
    //iCompare := FDataLink.DataSet.CompareBookmarks(TBookmark(aName), TosDbSortedRow(oldRows.Objects[i]).FBookmark);
    iCompare := AnsiCompareText(aName, TBookmarkStr(TosDbSortedRow(oldRows.Objects[i]).FBookmark));
    if iCompare = 0 then
    begin
      Result := i;
      break;
    end
    else if iCompare > 0 then
      iLo := i
    else
      iHi := i;
  end;    
end;

procedure TosDbSortedRows.ReloadStoredData;
var activeRow : Integer;
    locateRow : TosDbSortedRow;
begin
  try
    if (Count = 0) or
       (ReloadData) then
    begin
      for activeRow := 1 to FGrid.StoredData.RowsInData-1 do
      begin
        locateRow := TosDbSortedRow(Self.RowForActiveRow(activeRow));
        if (locateRow = Nil) then
           AddDataRow(activeRow, '');
      end;
      FReloadData := False;
    end;
  finally
    if (Self.SortFields.Count > 0) then
       DoSort;
  end;
end;

procedure TosDbSortedRows.Reload(RemapVisible : Boolean);
var i, oldActive : Integer;
    locateRow : TosDbSortedRow;
    bLocateExisting : Boolean;

  function findRow(theBm : TBookmark {TBookmarkStr}) : TosDbSortedRow;
  var bmIndex : Integer;
  begin
    Result := Nil;
    bmIndex := FBookmarks.IndexOf(theBm); 
    if (bmIndex >= 0) then
       Result := TosDbSortedRow(FBookmarks.FList.Objects[bmIndex]);
  end;
begin
  if (Self.InUpdate) then exit;

  if (FGrid.StoreData) then
  begin
    ReloadStoredData;
    exit;
  end;

  if (FDataLink.DataSource = Nil) or
     (FDataLink.DataSet.RecordCount = 0) then
     exit;

  try
    if (FDataLink.RecordCount < Self.DataCount) then // Some rows need to be removed...
       RemoveDeletedRecords;
    bLocateExisting := (Count > 0);

    if (Count = 0) or
       (ReloadData) then
    begin
      with FDataLink.DataSet do
      begin
        oldActive := FDataLink.ActiveRecord;
        try
          i := 0;
          while (i < FDataLink.RecordCount) do
          begin
            FDataLink.ActiveRecord := i;
            locateRow := Nil;
            if bLocateExisting then
               locateRow := findRow(Bookmark);

            if (locateRow <> nil) then
            begin
              if (locateRow.IsData) then
              begin
                locateRow.FActiveRow := i;
                locateRow.FSortText := DbRecSortText + Format('%8.7d', [i]) + '-2';
                locateRow.FKeyValue := Bookmark;
              end;
            end
            else
               AddDataRow(i, DbRecSortText + Format('%8.7d', [i]) + '-2');
            Inc(i);
          end;
        finally
          if (FDataLink.Eof) and
             (oldActive = FDataLink.BufferCount - 1) then
             FDataLink.DataSet.Last
          else if (oldActive < 0) then
          begin
            if (FDataLink.DataSet.RecordCount > 0) then
               FDataLink.DataSet.First;
          end
          else
          begin
            FDataLink.ActiveRecord := oldActive - 1;
            FDataLink.DataSet.MoveBy(1);
          end;
        end;
      end;
      FReloadData := False;
    end;
  finally
    if (Self.SortFields.Count > 0) then
       DoSort;
  end;
end;
procedure TosDbSortedRows.Clear;
begin
  inherited;
  if (Assigned(FBookmarks)) then
     FBookmarks.Clear;  
end;

procedure TosDbSortedRows.DoSort;
var curCursor : TCursor;
begin
  curCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    theSortedRows := Self;
    Sort(@CompareDbItems);
  finally
    Screen.Cursor := curCursor;
  end;
end;

function TosDbSortedRows.GroupHeaderText(onCol, onRow : Integer) : String;
begin
  Result := FGrid.GroupHeaderText(onCol, onRow);
end;

function TosDbSortedRows.AddDataRow(activeRow : Integer; theValue : Variant) : TosDbSortedRow;
var aRow : TosDbSortedRow;
    bmIndex : Integer;
begin
  aRow := TosDbSortedRow.Create;
  aRow.FVisible   := True;
  aRow.FGroupHeader := Nil;
  aRow.FActiveRow := activeRow;
  aRow.FSortText  := VartoStr(theValue);
  if (FDataLink.DataSet <> Nil) then
  begin
    aRow.FKeyValue  := FDataLink.DataSet.Bookmark;
    aRow.FBookmark  := FDataLink.DataSet.GetBookmark;
  end;
  aRow.FGroupVisible := True;
  aRow.FRowHeight := FGrid.DefaultRowHeight;
  aRow.FOwner     := Self;
  Add(aRow);
  Result := aRow;
  if (FDataLink.DataSet <> Nil) then
  begin
    bmIndex := FBookmarks.IndexOf(FDataLink.DataSet.Bookmark);
    if (bmIndex >= 0) then
       FBookmarks.FList.Objects[bmIndex] := Result
    else if (FDataLink.DataSet.Bookmark <> nil) then
    begin
      FBookmarks.CurrentRowSelected := True;
      bmIndex := FBookmarks.IndexOf(FDataLink.DataSet.Bookmark);
      if (bmIndex >= 0) then
         FBookmarks.FList.Objects[bmIndex] := Result;
    end;
  end;
  DataCount := DataCount + 1;
end;

function TosDbSortedRows.NewGroupHeader(value : String; iGrp, iLevel, iPos : Integer) : TosGroupHeaderRow;
var i, j : Integer;
begin
  Result := TosGroupHeaderRow.Create;
  Result.Owner := Self;
  Result.ActiveRow := -1;
  Result.GroupNumber := iGrp;
  Result.GroupLevel  := iLevel;
  Result.RowType := rtGroupHeader;
  if (Copy(value, Length(value)-1, 2) = '-2') then
     Result.SortText := Copy(value, 1, Length(value)-2) + '-1'
  else
     Result.SortText := value + '-1';
  j := 0;
  for i := 0 to SortFields.Count - 1 do
    if (SortFields.SortEntry[i].GroupSort) then
    begin
      Inc(j);
      if (j >= iLevel) then
      begin
        Result.GroupColumn := SortFields.SortEntry[i].Column;
        break;
      end;
    end;
  Result.ResetGroupHeader(iPos);
  Result.Expanded := True;
  Result.Visible := True;
  Result.GroupVisible := True;
  Result.RowHeight := TosAdvDbGrid(Self.FGrid).FGroupingSortingOptions.GroupHeaderHeight;
  Result.KeyValue := '';
  if (iGrp < High(Self.FOldExpanded)) then
     Result.Expanded := FOldExpanded[iGrp];
  GroupCount := GroupCount + 1;
  MixedRowHeights := True;
  Self.Insert(iPos, Result);
end;

function TosDbSortedRows.RowExists(dataRow : Integer) : Boolean;
begin
  Result := (dataRow <= Count);
end;

function TosDbSortedRows.GetSortedRow(dataRow : Integer) : TosDbSortedRow;
begin
  if (dataRow > Count) then
     raise Exception.Create('Invalid dataRow in TosAdvDbGrid.GetSortedRow - Row ' + IntToStr(dataRow));
  Result := TosDbSortedRow(Items[dataRow-1]);
end;

{ TosDbSortedRow }

function TosDbSortedRow.FieldValue(fieldName : String) : Variant;
var theField : TField;
    theCol : TosDbCol;
begin
  Result := '';
  if (Owner <> Nil) then
  begin
    if (TosDbSortedRows(Owner).FGrid.StoreData) then
    begin
      TosDbSortedRows(FOwner).FGrid.GridData.FActiveRow := ActiveRow;
      theCol := TosDbSortedRows(FOwner).FGrid.Col[fieldName];
      Result := theCol.TextAsDataType(TosDbSortedRows(FOwner).FGrid.InternalData.GetValue(theCol.DataCol, TosDbSortedRows(FOwner).FGrid.GridData.ActiveRow));
      //if (theCol.DataType = dyDate) and
      //   (theCol.GroupDateTimeSegment <> dgoNone) then
      //   Result := ExtractDateTimeSegment(Result, theCol.GroupDateTimeSegment);
    end
    else if (TosDbSortedRows(Owner).FDataLink <> Nil) and
            (fieldName <> '') then
    begin
      theField := FieldByName(fieldName);
      if (theField <> Nil) then
      begin
        if (theField.DataType = ftMemo) then
           Result := theField.AsString
        else if (theField.DataType = ftString) then
           Result := theField.DisplayText
{$IFDEF TSVER_V6}
        else if (theField.DataType = ftTimestamp) then
           Result := theField.AsDateTime
{$ENDIF}
        else
           Result := theField.Value;
      end;
    end;
  end;
end;

function TosDbSortedRow.FieldValueAsDouble(dataCol : Variant) : Double;
var fieldName : String;
    theField : TField;
    theCol : TosDbCol;
begin
  if (TosDbSortedRows(Owner).FGrid.StoreData) then
  begin
    theCol := TosDbSortedRows(FOwner).FGrid.Col[dataCol];
    if (theCol <> Nil) then
       Result := theCol.TextAsDataType(TosDbSortedRows(FOwner).FGrid.InternalData.GetValue(DataCol, Self.ActiveRow))
    else
       Result := 0;
  end
  else
  begin
    fieldName := TosDbSortedRows(Self.Owner).FGrid.Col[dataCol].FieldName;
    theField  := FieldByName(fieldName);
    if (theField <> Nil) then
       Result := theField.AsFloat
    else
       Result := 0;
  end;
end;

function TosDbSortedRow.GroupValue(fieldName : String) : variant;
var theField : TField;
    theCol : TosDbCol;
begin
  if (RowType = rtData) then
  begin
    theField := Self.FieldByName(fieldName);
    if (theField = Nil) then
    begin
      Result := '';
      if (TosDbSortedRows(Self.Owner).FGrid.StoreData) then
      begin
        TosDbSortedRows(FOwner).FGrid.GridData.FActiveRow := ActiveRow;
        theCol := TosDbSortedRows(FOwner).FGrid.Col[fieldName];
        Result := theCol.TextAsDataType(TosDbSortedRows(FOwner).FGrid.InternalData.GetValue(theCol.DataCol, TosDbSortedRows(FOwner).FGrid.GridData.ActiveRow));
        if (theCol.DataType = dyDate) and
           (theCol.GroupDateTimeSegment <> dgoNone) then
           Result := ExtractDateTimeSegment(Result, theCol.GroupDateTimeSegment);
      end;
      exit;
    end;
    Result := theField.Value;

    if (VarType(Result) = varNull) then exit;

    if (theField.DataType = ftDate) or
       (theField.DataType = ftDateTime)
{$IFDEF TSVER_V6}
       or (theField.DataType = ftTimestamp)
{$ENDIF}
       then
       Result := TosDbSortedRows(FOwner).FGrid.Col[fieldName].GroupText;
  end
  else
     Result := Self.KeyValue;
end;

function TosDbSortedRow.GroupText : String;
var i : Integer;
begin
  if (IsData) then
  begin
    Result := '';
    for i := 0 to Self.FOwner.SortFields.Count - 1 do
      if (Self.FOwner.SortFields.SortEntry[i].GroupSort) then
         Result := Result + VarToStr(GroupValue(Self.FOwner.SortFields.SortEntry[i].FieldName)) + '-';
    if (Length(Result) > 0) then
       System.Delete(Result, Length(Result), 1);
  end
  else
    Result := VarToStr(KeyValue); 
end;

function TosDbSortedRow.FieldByName(fieldName : String) : TField;
var theRow : TosSortedRow;
begin
  Result := Nil;
  if (FOwner <> Nil) and
     (TosDbSortedRows(Owner).FDataLink.DataSet <> Nil) then
  begin
    case RowType of
      rtData        : //TosDbSortedRows(Owner).FDataLink.DataSet.Bookmark := Self.FKeyValue;
                      TosDbSortedRows(Owner).FDataLink.ActiveRecord := Self.ActiveRow;
      rtGroupHeader : begin
                        if (ActiveRow < 0) then
                        begin
                          theRow := TosDbSortedRows(Owner).NextDataRow(TosDbSortedRows(Owner).IndexOf(Self));
                          if (theRow <> Nil) then
                             ActiveRow := theRow.ActiveRow;
                        end;
                        TosDbSortedRows(Owner).FDataLink.ActiveRecord := Self.ActiveRow
                      end;
      rtGroupFooter : begin
                        if (ActiveRow < 0) then
                        begin
                          theRow := TosDbSortedRows(Owner).PriorDataRow(TosDbSortedRows(Owner).IndexOf(Self));
                          if (theRow <> Nil) then
                             ActiveRow := theRow.ActiveRow;
                        end;
                        TosDbSortedRows(Owner).FDataLink.ActiveRecord := Self.ActiveRow
                      end;
    end;
    Result := TosDbSortedRows(Owner).FDataLink.DataSet.FindField(fieldName);
   //     FOwner.FDataLink.DataSet.FieldByName(fieldName);
  end;
end;


{ TosGroupingSortingOptions }

function TosGroupingSortingOptions.GetGroupColor: TColor;
begin
  Result := FGrid.GroupColor;
end;

function TosGroupingSortingOptions.GetGroupHeaderHeight: Integer;
begin
  Result := FGrid.GroupHeaderHeight;
end;

function TosGroupingSortingOptions.GetGroupIndent: Integer;
begin
  Result := FGrid.GroupIndent;
end;

function TosGroupingSortingOptions.GetGroupFooterHeight: Integer;
begin
  Result := FGrid.GroupFooterHeight;
end;

procedure TosGroupingSortingOptions.SetGroupFooterHeight(const Value: Integer);
begin
  FGrid.GroupFooterHeight := Value;
end;

procedure TosGroupingSortingOptions.SetGroupColor(const Value: TColor);
begin
  FGrid.GroupColor := Value;
end;

procedure TosGroupingSortingOptions.SetGroupHeaderHeight(const Value: Integer);
begin
  FGrid.GroupHeaderHeight := Value;
end;

procedure TosGroupingSortingOptions.SetGroupIndent(const Value: Integer);
begin
  FGrid.GroupIndent := Value;
end;

function TosGroupingSortingOptions.GetGroupFont: TFont;
begin
  Result := FGrid.GroupFont;
end;

procedure TosGroupingSortingOptions.SetGroupFont(const Value: TFont);
begin
  FGrid.GroupFont := Value;
end;

function TosGroupingSortingOptions.GetAnsiSort : Boolean;
begin
  Result := FGrid.AnsiSort;
end;

procedure TosGroupingSortingOptions.SetAnsiSort(const Value: Boolean);
begin
  FGrid.AnsiSort := Value;
end;

function TosGroupingSortingOptions.GetSortCaseInsensitive: Boolean;
begin
  Result := FGrid.SortCaseInsensitive;
end;

procedure TosGroupingSortingOptions.SetSortCaseInsensitive(const Value: Boolean);
begin
  FGrid.SortCaseInsensitive := Value;
end;

function TosGroupingSortingOptions.GetGroupColumnsHidden : Boolean;
begin
  Result := FGrid.GroupColumnsHidden;
end;

procedure TosGroupingSortingOptions.SetGroupColumnsHidden(const Value : Boolean);
begin
  FGrid.GroupColumnsHidden := Value;
end;

procedure TosGroupingSortingOptions.SetGroupFooterFrame(const Value: TosGroupRowFrame);
begin
  FGrid.GroupFooterFrame := Value;
end;

procedure TosGroupingSortingOptions.SetGroupHeaderFormat(const Value: TosGroupHeaderFormat);
begin
  FGrid.GroupHeaderFormat := Value;
end;

procedure TosGroupingSortingOptions.SetGroupHeaderFrame(const Value: TosGroupRowFrame);
begin
  FGrid.GroupHeaderFrame := Value;
end;

function TosGroupingSortingOptions.GetGroupFooterFrame: TosGroupRowFrame;
begin
  Result := FGrid.GroupFooterFrame;
end;

function TosGroupingSortingOptions.GetGroupHeaderFormat: TosGroupHeaderFormat;
begin
  Result := FGrid.GroupHeaderFormat;
end;

function TosGroupingSortingOptions.GetGroupHeaderFrame: TosGroupRowFrame;
begin
  Result := FGrid.GroupHeaderFrame;
end;

function TosGroupingSortingOptions.GetShowSummaryOpText: TosSummaryOpText;
begin
  Result := FGrid.ShowSummaryOpText;
end;

procedure TosGroupingSortingOptions.SetShowSummaryOpText(const Value: TosSummaryOpText);
begin
  FGrid.ShowSummaryOpText := Value;
end;

function TosGroupingSortingOptions.GetSortOnHeadingClick: Boolean;
begin
  Result := FGrid.SortOnHeadingClick;
end;

procedure TosGroupingSortingOptions.SetSortOnHeadingClick(const Value: Boolean);
begin
  FGrid.SortOnHeadingClick := Value;
end;

function TosGroupingSortingOptions.GetGroupFootersOn: Boolean;
begin
  Result := FGrid.GroupFootersOn;
end;

procedure TosGroupingSortingOptions.SetGroupFootersOn(const Value: Boolean);
begin
  FGrid.GroupFootersOn := Value;
end;

procedure TosGroupingSortingOptions.Assign(srcGroupingSortingOptions: TPersistent);
begin
  with TosGroupingSortingOptions(srcGroupingSortingOptions) do
  begin
    Self.ShowSummaryOpText   := ShowSummaryOpText;
    Self.SortCaseInsensitive := SortCaseInsensitive;
    Self.SortOnHeadingClick  := SortOnHeadingClick;
    Self.GroupFont           := GroupFont;
    Self.GroupColor          := GroupColor;
    Self.GroupIndent         := GroupIndent;
    Self.GroupHeaderHeight   := GroupHeaderHeight;
    Self.GroupFooterHeight   := GroupFooterHeight;
    Self.GroupHeaderFrame    := GroupHeaderFrame;
    Self.GroupFooterFrame    := GroupFooterFrame;
    Self.GroupHeaderFormat   := GroupHeaderFormat;
    Self.GroupColumnsHidden  := GroupColumnsHidden;
    Self.GroupFootersOn      := GroupFootersOn;
  end;
end;


{ TosBookmarkList }

constructor TosBookmarkList.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.OnChange := StringsChanged;
end;

destructor TosBookmarkList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TosBookmarkList.Clear;
begin
  if FList.Count = 0 then Exit;
  FList.Clear;
end;

function TosBookmarkList.Compare(const Item1, Item2: TBookmark {TBookmarkStr}): Integer;
begin
  with FDataset do
    Result := CompareBookmarks(TBookmark(Item1), TBookmark(Item2));
end;

function TosBookmarkList.CurrentRow: TBookmark{TBookmarkStr};
begin
  if not FLinkActive then raise Exception.Create('DataSet not active');
  Result := FDataset.Bookmark;
end;

function TosBookmarkList.GetCurrentRowSelected: Boolean;
var Index: Integer;
begin
  Result := Find(CurrentRow, Index);
end;

function TosBookmarkList.Find(const Item: TBookmark {TBookmarkStr}; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  if (Item = FCache) and (FCacheIndex >= 0) then
  begin
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare( TBookmark( FList[I] ), Item);
    if C < 0 then
       L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
  FCache := Item;
  FCacheIndex := Index;
  FCacheFind := Result;
end;

function TosBookmarkList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TosBookmarkList.GetItem(Index: Integer): TBookmarkStr;
begin
  Result := FList[Index];
end;

function TosBookmarkList.IndexOf(const Item: TBookmark {TBookmarkStr}): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

procedure TosBookmarkList.LinkActive(Value: Boolean);
begin
  Clear;
  FLinkActive := Value;
  if (Value) and
     (FDataSet <> Nil) then
  begin
    with FDataSet do
    begin
      DisableControls;
      try
        First;
        while not eof do
        begin
          CurrentRowSelected := True;
          Next;
        end;
      finally
        First;
        EnableControls;
      end;
    end;
  end;
end;

procedure TosBookmarkList.Delete;
var
  I: Integer;
begin
  with FDataset do
  begin
    DisableControls;
    try
      for I := FList.Count-1 downto 0 do
      begin
        Bookmark := TBookmark( FList[I] );
        Delete;
        FList.Delete(I);
      end;
    finally
      EnableControls;
    end;
  end;
end;

function TosBookmarkList.Refresh: Boolean;
var
  I: Integer;
begin
  Result := False;
  with FDataset do
  try
    CheckBrowseMode;
    for I := FList.Count - 1 downto 0 do
      if not BookmarkValid(TBookmark(FList[I])) then
      begin
        Result := True;
        FList.Delete(I);
      end;
  finally
    UpdateCursorPos;
  end;
end;

procedure TosBookmarkList.SetCurrentRowSelected(Value: Boolean);
var
  Index: Integer;
  Current: TBookmark; //TBookmarkStr;
begin
  Current := CurrentRow;
  if (Length(Current) = 0) or (Find(Current, Index) = Value) then
     exit;

  if Value then
     FList.Insert(Index, TBookmarkStr(Current) )
  else
     FList.Delete(Index);
end;

procedure TosBookmarkList.StringsChanged(Sender: TObject);
begin
  FCache := nil;
  FCacheIndex := -1;
end;

{ TosPrintOptions }

procedure TosPrintOptions.Assign(srcPrintOptions: TPersistent);
begin
  with TosPrintOptions(srcPrintOptions) do
  begin
    Self.PrintTitle           := PrintTitle;
    Self.PrintTotals          := PrintTotals;
    Self.PrintCols            := PrintCols;
    Self.PrintWithGridFormats := PrintWithGridFormats;
  end;
end;

function TosPrintOptions.GetPrintCols: Integer;
begin
  Result := FGrid.PrintCols;
end;

function TosPrintOptions.GetPrintTitle: String;
begin
  Result := FGrid.PrintTitle;
end;

function TosPrintOptions.GetPrintTotals: Boolean;
begin
  Result := FGrid.PrintTotals;
end;

function TosPrintOptions.GetPrintWithGridFormats: Boolean;
begin
 Result := FGrid.PrintWithGridFormats;
end;

procedure TosPrintOptions.SetPrintCols(const Value: Integer);
begin
  FGrid.PrintCols := Value;
end;

procedure TosPrintOptions.SetPrintTitle(const Value: String);
begin
  FGrid.PrintTitle := Value;
end;

procedure TosPrintOptions.SetPrintTotals(const Value: Boolean);
begin
  FGrid.PrintTotals := Value;
end;

procedure TosPrintOptions.SetPrintWithGridFormats(const Value : Boolean);
begin
  FGrid.PrintWithGridFormats := Value;
end;

{ TosMemoOptions }

procedure TosMemoOptions.Assign(srcMemoOptions: TPersistent);
begin
  with TosMemoOptions(srcMemoOptions) do
  begin
    Self.EditorShortCut := EditorShortCut;
    Self.EditorOptions  := EditorOptions;
    Self.ScrollBars     := ScrollBars;
  end;
end;

function TosMemoOptions.GetEditorOptions: TosMemoEditorOptions;
begin
  Result := FGrid.MemoEditorOptions;
end;

function TosMemoOptions.GetEditorShortCut: TShortCut;
begin
  Result := FGrid.MemoEditorShortCut;
end;

function TosMemoOptions.GetScrollBars: TScrollStyle;
begin
  Result := FGrid.MemoScrollbars;
end;

procedure TosMemoOptions.SetEditorOptions(
  const Value: TosMemoEditorOptions);
begin
  FGrid.MemoEditorOptions := Value;
end;

procedure TosMemoOptions.SetEditorShortCut(const Value: TShortCut);
begin
  FGrid.MemoEditorShortCut := Value;
end;

procedure TosMemoOptions.SetScrollBars(const Value: TScrollStyle);
begin
  FGrid.MemoScrollbars := Value;
end;

{ TosDBComboGrid }

constructor TosDBComboGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAsCombo := True;
  FGrid := Self;
  Parent := TWinControl(AOwner);
  ParentGrid := TtsBaseGrid(Parent.Owner);
  FDesigning := TosAdvDbGrid(ParentGrid).InDesignMode;

  if not Reading then
  begin
      Ctl3D := False;
      ParentCtl3D := False;
  end;

  FDropDownRows := 4;
  FDropDownCols := 1;
  FDropDownStyle := ddDropDown;
  FValueCol := 1;
  FCompareType := ctCaseInsensitive;
  FAutoSearch := asNone;
  FAutoFill := False;
  FAutoFillConvertCase := afcOnEdit;
  FAutoLookup := False;
end;

destructor TosDBComboGrid.Destroy;
begin
  FGrid := nil;
  inherited Destroy;
end;

function TosDBComboGrid.GetDropDownRows: Longint;
begin
    Result := FDropDownRows;
end;

function TosDBComboGrid.GetDropDownCols: Longint;
begin
    Result := FDropDownCols;
end;

function TosDBComboGrid.GetValueCol: Longint;
begin
    Result := FValueCol;
end;

function TosDBComboGrid.GetValueColSorted: Boolean;
begin
    Result := FValueColSorted;
end;

function TosDBComboGrid.GetCompareType: TtsComboCompareType;
begin
    Result := FCompareType;
end;

function TosDBComboGrid.GetAutoSearch: TtsComboAutoSearchType;
begin
    Result := FAutoSearch;
end;

function TosDBComboGrid.GetAutoFill: Boolean;
begin
    Result := FAutoFill;
end;

function TosDBComboGrid.GetAutoFillConvertCase: TtsConvertCase;
begin
    Result := FAutoFillConvertCase;
end;

function TosDBComboGrid.GetAutoLookup: Boolean;
begin
    Result := FAutoLookup;
end;

function TosDBComboGrid.GetDropDownStyle: TtsDropDownStyle;
begin
    Result := FDropDownStyle;
end;

function TosDBComboGrid.GetGrid: TosAdvDbGrid;
begin
    Result := TosAdvDbGrid(FGrid);
end;

procedure TosDBComboGrid.SetDropDownRows(Value: Longint);
begin
    if Value <> FDropDownRows then
    begin
        FDropDownRows := Value;
        if CanSetComboExtents then
            SetComboGridExtents(DropDownCols, DropDownRows);
    end;
end;

procedure TosDBComboGrid.SetDropDownCols(Value: Longint);
begin
    if Value <> FDropDownCols then
    begin
        FDropDownCols := Value;
        Self.FieldState := fsCustomized;
        AdjustComboGridExtents(DropDownCols, DropDownRows);
    end;
end;

procedure TosDBComboGrid.SetValueCol(Value: Longint);
begin
    if Value <> FValueCol then FValueCol := Value;
end;

procedure TosDBComboGrid.SetValueColSorted(Value: Boolean);
begin
    if Value <> FValueColSorted then FValueColSorted := Value;
end;

procedure TosDBComboGrid.SetCompareType(Value: TtsComboCompareType);
begin
    if Value <> FCompareType then FCompareType := Value;
end;

procedure TosDBComboGrid.SetAutoSearch(Value: TtsComboAutoSearchType);
begin
    if Value <> FAutoSearch then FAutoSearch := Value;
end;

procedure TosDBComboGrid.SetAutoFill(Value: Boolean);
begin
    if Value <> FAutoFill then
    begin
        FAutoFill := Value;
        if FAutoFill and not (csLoading in ComponentState) then
        begin
            if (AutoSearch = asNone) then AutoSearch := asTop;
        end;
    end;
end;

procedure TosDBComboGrid.SetAutoFillConvertCase(Value: TtsConvertCase);
begin
    if FAutoFillConvertCase <> Value then FAutoFillConvertCase := Value;
end;

procedure TosDBComboGrid.SetAutoLookup(Value: Boolean);
begin
    if Value <> FAutoLookup then
    begin
        if not Value then
        begin
            FAutoLookup:= Value;
            //if Assigned(FLookupDatasource) then Datasource := nil;
        end
        else
        begin
            StoreData := False;
            DataSource := nil;
            FAutoLookup:= Value;
            if InDesignMode and Visible then
            begin
                TosAdvDbGrid(ParentGrid).InitComboData(False);
                AdjustComboGridExtents(DropDownCols, DropDownRows);
                //CheckSetLookupDataset;
            end;
        end;
    end;
end;

procedure TosDBComboGrid.SetDropDownStyle(Value: TtsDropDownStyle);
begin
    if Value <> FDropDownStyle then
    begin
        FDropDownStyle := Value;
        //TtsCustomDBGrid(ParentGrid).RedisplayControl(True);
    end;
end;

function TosDBComboGrid.GetParentGrid: TtsBaseGrid;
begin
    Result := FParentGrid;
end;

procedure TosDBComboGrid.SetParentGrid(Value: TtsBaseGrid);
begin
    FParentGrid := Value;
end;

function TosDBComboGrid.GetParentGridCombo: TtsCombo;
begin
    Result := FCombo;
end;

procedure TosDBComboGrid.SetParentGridCombo(Value: TtsCombo);
begin
    FCombo := Value;
end;


end.
