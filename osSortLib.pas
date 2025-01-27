unit osSortLib;

{$INCLUDE TSCmpVer}

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, TSSetLib, TSCommon
  {$IFDEF TSVER_V6}, Variants {$ENDIF};

type
  TosSortType = (stNone, stAscending, stDescending);
  TosRowType  = (rtData, rtGroupHeader, rtGroupFooter);
  TosGroupSummaryOp = (gsoNone, gsoCount, gsoSum, gsoMax, gsoMin, gsoAvg);

  TosSortedRows = class;
  TosSortFieldList = class;
  TosGroupHeaderRow = class;
  TosGroupFooterRow = class;

  TosSortEntry = class(TObject)
  private
    FFieldName : String;
    FSortMode  : TosSortType;
    FGroupSort : Boolean;
    FColumn    : Integer;
    FDataType   : TosDataType;
  protected
  public
    procedure ToggleSort;

    property Column : Integer read FColumn write FColumn;
    property FieldName: String read FFieldName write FFieldName;
    property SortMode: TosSortType read FSortMode write FSortMode;
    property GroupSort : Boolean read FGroupSort write FGroupSort;
    property DataType : TosDataType read FDataType write FDataType default dyString;
  end;

  TosSortedRow = class(TObject)
  private
  protected
    FOwner        : TosSortedRows;
    FGroupHeader  : TosGroupHeaderRow;
    FActiveRow    : Integer;
    FGroupNumber  : Integer;
    FGroupLevel   : Integer;
    FVisible      : Boolean;
    FGroupVisible : Boolean;
    FSelected     : Boolean;
    FRowType      : TosRowType;
    FSortText     : String;
    FKeyValue     : variant;
    FRowHeight    : Integer;
    FGroupBreak   : Boolean;

    procedure SetVisible(Value : Boolean);
    procedure SetGroupVisible(Value : Boolean);
    procedure SetRowHeight(Value : Integer);
    procedure SetSelected(const Value: Boolean);
    function  GetGroupHeader : TosGroupHeaderRow;
    function  GetSortPositionValue : Integer; virtual;
  public

    function FieldValue(fieldName : String) : Variant; virtual;
    function FieldValueAsDouble(dataCol : Variant) : Double; virtual;
    function GroupValue(fieldName : String) : variant; virtual;
    function IsData : Boolean;
    function IsGroupHeader : Boolean;
    function IsGroupFooter : Boolean;
    function GroupText : String; virtual;
    procedure IncFooterTotals; virtual;
    procedure ReCalcGroupNumber;

    property SortPositionValue : Integer read GetSortPositionValue;
    property Owner : TosSortedRows read FOwner write FOwner;
    property GroupHeader : TosGroupHeaderRow read GetGroupHeader;
    property ActiveRow : Integer read FActiveRow write FActiveRow;
    property SortText : String read FSortText write FSortText;
    property KeyValue : Variant read FKeyValue write FKeyValue;
    property RowType : TosRowType read FRowType write FRowType;
    property GroupNumber : Integer read FGroupNumber write FGroupNumber;
    property GroupLevel : Integer read FGroupLevel write FGroupLevel;
    property Visible : Boolean read FVisible write SetVisible;
    property GroupVisible : Boolean read FGroupVisible write SetGroupVisible;
    property RowHeight : Integer read FRowHeight write SetRowHeight;
    property Selected : Boolean read FSelected write SetSelected;
  end;

  TosGroupHeaderRow = class(TosSortedRow)
  private
  protected
    FGroupCount   : Integer;
    FExpanded     : Boolean;
    FGroupColumn  : Integer;
    FGroupFooter  : TosGroupFooterRow;
    FNextSiblingHeader : TosGroupHeaderRow;

    function GetSortPositionValue : Integer; override;
  public
    function GroupValue(fieldName : String) : variant; override;
    function GroupText : String; override;
    procedure ResetGroupHeader(fromPos : Integer);
    
    property GroupColumn : Integer read FGroupColumn write FGroupColumn;
    property Expanded : Boolean read FExpanded write FExpanded;
    property GroupCount : Integer read FGroupCount write FGroupCount;
    property GroupFooter : TosGroupFooterRow read FGroupFooter;
    property NextSiblingHeader : TosGroupHeaderRow read FNextSiblingHeader;
  end;

  TosSubTotalField = class(TObject)
  private
    FSubTotal : Double;
    FDataCol  : Integer;
    FSummaryOp : TosGroupSummaryOp;
  protected
  public

    property SubTotal : Double read FSubTotal write FSubTotal;
    property DataCol : Integer read FDataCol;
    property SummaryOp : TosGroupSummaryOp read FSummaryOp;
  end;

  TosGroupFooterRow = class(TosSortedRow)
  private
    FSortPositionValue : Integer;
  protected
    FSubTotalFields : TObjectList;
    //FHeader : TosGroupHeaderRow;

    procedure CalcAverages;
    function  CalcMin(onSubTotalField : TosSubTotalField) : Double;
    function  CalcMax(onSubTotalField : TosSubTotalField) : Double;
    function  GetSortPositionValue : Integer; override;
    function  GetSubTotalField(index: Integer): TosSubTotalField;
  public
    constructor Create;
    destructor Destroy; override;
    function  GroupValue(fieldName : String) : variant; override;

    function  SubTotalFieldCount : Integer;
    function  EnsureSubTotal(dataCol : Integer; summOp : TosGroupSummaryOp) : TosSubTotalField;
    function  SubTotalFieldForDataCol(dataCol : Integer) : TosSubTotalField;
    function  AdjustSubTotal(newValue, deltaValue : Double; dataCol : Integer) : Boolean;
    procedure RefreshSubTotalFields;
    procedure ClearSubTotalFields;
    procedure ReCalcAll;

    property SubTotalField[index : Integer] : TosSubTotalField read GetSubTotalField;
    //property Header : TosGroupHeaderRow read FHeader;
  end;

  TosSortedRows = class(TObjectList)
  private
    FAllowDuplicates : Boolean;
    FCaseInsensitive : Boolean;
    FMixedRowHeights : Boolean;
    FSortFields : TosSortFieldList;
    FGroupCount : Integer;
    FFootersOn  : Boolean;
    FDataCount  : Integer;
    FGroupOp    : Integer;
    FUpdateOp   : Integer;
  protected
    FOldExpanded : array of boolean;

    function  GetRow(dataRow : Integer) : TosSortedRow; virtual;
    procedure Reload(RemapVisible : Boolean); virtual;
    procedure CheckGrouping(reapplyFooters : Boolean = false); virtual;
    procedure RecalcGroupCounts; virtual;
    function  ReverseText(const aString : String) : String;
    function  NewGroupFooter(forHeader : TosGroupHeaderRow; iPos : Integer) : TosGroupFooterRow; virtual;
    function  NewGroupHeader(value : String; iGrp, iLevel, iPos : Integer) : TosGroupHeaderRow; virtual;
    procedure InsertGroupHeaders;
    procedure RemapEntries(fromRow : Integer; iDirection : Integer);

    function  GroupValuesDiffer(onRow : Integer; var onLevel, diffLevel : Integer) : Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure CalcGroupNumbers;
    procedure BeginUpdate;
    procedure EndUpdate;
    function  InUpdate : Boolean;
    procedure BeginGroupOp;
    procedure EndGroupOp;
    function  InGroupOp : Boolean;

    procedure Clear; override;
    procedure ClearSorts; virtual;
    procedure ClearGroups; virtual;
    procedure ClearAll; virtual;
    procedure RemoveNonDataRows;
    procedure RemoveGroupHeaders(forCol : Integer = 0);
    procedure RemoveGroupFooters;
    procedure AddGroupFooters;
    procedure DoSort; virtual;
    function  GroupHeaderText(onCol, onRow : Integer) : String; virtual;
    procedure RecalcFooterTotals;
    function  RemoveEntry(theRow : TosSortedRow; bReMap : Boolean = False) : Boolean; virtual;
    function  RowForActiveRow(findActiveRow : Integer) : TosSortedRow;

    property SortedRow[dataRow : Integer] : TosSortedRow read GetRow;

    function NextDataRow(fromRow : Integer) : TosSortedRow;
    function PriorDataRow(fromRow : Integer) : TosSortedRow;
    function AddSortField(colName : String; dataCol : Integer; sortMode : TosSortType; theVarType : TosDataType) : TosSortEntry;
    function AddGroupField(colName : String; dataCol : Integer; theVarType : TosDataType; reapplyFooters : Boolean = false) : TosSortEntry;

    property DataCount : Integer read FDataCount write FDataCount;
    property GroupCount : Integer read FGroupCount write FGroupCount;
    property FootersOn : Boolean read FFootersOn write FFootersOn;
    property SortFields : TosSortFieldList read FSortFields;
    property AllowDuplicates : Boolean read FAllowDuplicates write FAllowDuplicates;
    property CaseInsensitive : Boolean read FCaseInsensitive write FCaseInsensitive;
    property MixedRowHeights : Boolean read FMixedRowHeights write FMixedRowHeights;
  end;


  {TosSortFieldList}
  TosSortFieldList = class(TStringList)
  private
    FSortedData : TosSortedRows;
  protected
    function GetSortEntry(i : Integer) : TosSortEntry;
  public

    procedure ClearGroups;
    procedure ClearSorts;
    procedure ClearAll;
    function  GroupLevelOf(fldName : String) : Integer;

    procedure AddGroupField(fldName : String; addEntry : TosSortEntry);
    function GroupCount : Integer;
    function SortCount : Integer;
    function SortEntryByName(fieldName : String) : TosSortEntry;
    function SortEntryByNumber(colNumber : Integer) : TosSortEntry;
    property SortEntry[i : Integer] : TosSortEntry read GetSortEntry;
    property SortedData : TosSortedRows read FSortedData write FSortedData;
  end;

function CompareItems(Item1, Item2: Pointer): Integer;

implementation


function CompareItems(Item1, Item2: Pointer): Integer;
begin
  // Item1 - Item2 comparison...
  Result := CompareText(TosSortedRow(Item1).FSortText, TosSortedRow(Item2).FSortText);
end;

{ TosSortedRows }

constructor TosSortedRows.Create;
begin
  inherited Create;
  FSortFields := TosSortFieldList.Create;
  FSortFields.FSortedData := Self;
  FAllowDuplicates := False;
  FMixedRowHeights := False;

  FGroupOp := 0;
  FGroupCount := 0;
  FDataCount := 0;
end;

destructor TosSortedRows.Destroy;
begin
  FSortFields.Free;
  inherited;
end;

function TosSortedRows.GetRow(dataRow : Integer) : TosSortedRow;
begin
  if (dataRow > Self.Count) then
     raise Exception.Create('Invalid Row ' + IntToStr(dataRow) + ' indexing into SortedNodes!');
  Result := TosSortedRow(Self.Items[DataRow-1]);
end;

function TosSortedRows.AddGroupField(colName : String; dataCol : Integer; theVarType : TosDataType; reapplyFooters : Boolean) : TosSortEntry;
var newSortEntry : TosSortEntry;
    j : Integer;
begin
  j := Self.FSortFields.IndexOf(colName);
  if (j = -1) then
     newSortEntry := TosSortEntry.Create
  else
     newSortEntry := TosSortEntry(FSortFields.Objects[j]);

  newSortEntry.FieldName := colName;
  newSortEntry.SortMode := stAscending;
  newSortEntry.Column := dataCol;
  newSortEntry.GroupSort := True;
  newSortEntry.DataType := theVarType;
  if (j = -1) then
     FSortFields.AddGroupField(colName, newSortEntry);
  Reload(False);
  CheckGrouping(reapplyFooters);
  Result := newSortEntry;
end;

procedure TosSortedRows.BeginGroupOp;
begin
  Inc(FGroupOp);
end;

procedure TosSortedRows.EndGroupOp;
begin
  Dec(FGroupOp);
end;

function TosSortedRows.InGroupOp : Boolean;
begin
  Result := (FGroupOp > 0);
end;

procedure TosSortedRows.CheckGrouping(reapplyFooters : Boolean);
begin
  if InUpdate then exit;
  
  if (SortFields.GroupCount > 0) then
  begin
    CalcGroupNumbers;
    InsertGroupHeaders;
    if reapplyFooters then
       AddGroupFooters;
    DoSort;
    if (SortFields.GroupCount >= 1) then
       RecalcGroupCounts;
    DoSort;
  end;
end;

procedure TosSortedRows.RecalcGroupCounts;
var dRow, grpNo, i, jGrpCt : Integer;
    theRow : TosSortedRow;
    grpCts : TStringList;
    lastGroupHeaderOnLevel : TosGroupHeaderRow;
begin
  if (SortFields.GroupCount = 0) then exit;

  grpNo := 0;
  grpCts := TStringList.Create;
  jGrpCt := SortFields.GroupCount;
  for i := 0 to jGrpCt - 1 do
    grpCts.Add('0');
  try
    for dRow := 1 to Self.Count do
    begin
      theRow := Self.SortedRow[dRow];
      case theRow.RowType of
        rtGroupHeader : begin
                          Inc(grpNo);
                          theRow.GroupNumber := grpNo;
                          if (grpCts.Objects[theRow.GroupLevel-1] <> Nil) then
                          begin
                            lastGroupHeaderOnLevel := TosGroupHeaderRow(grpCts.Objects[theRow.GroupLevel-1]);
                            lastGroupHeaderOnLevel.GroupCount := StrToInt(grpCts.Strings[theRow.GroupLevel-1]);
                            if (lastGroupHeaderOnLevel.GroupHeader = TosGroupHeaderRow(theRow).GroupHeader) or
                               (lastGroupHeaderOnLevel.GroupColumn = TosGroupHeaderRow(theRow).GroupColumn) then
                               lastGroupHeaderOnLevel.FNextSiblingHeader := TosGroupHeaderRow(theRow);
                          end;
                          grpCts.Objects[theRow.GroupLevel-1] := theRow;
                          grpCts.Strings[theRow.GroupLevel-1] := '0';
                        end;
        rtData        : begin
                          theRow.GroupNumber := grpNo;
                          theRow.FGroupHeader := Nil;
                          for i := 0 to jGrpCt - 1 do
                             grpCts.Strings[i] := IntToStr(StrToInt(grpCts.Strings[i])+1);
                        end;
        rtGroupFooter : begin
                          theRow.GroupNumber := grpNo;
                        end;
      end;
    end;
    for i := 0 to jGrpCt - 1 do
    begin
      if (grpCts.Objects[i] <> Nil) then
         TosGroupHeaderRow(grpCts.Objects[i]).GroupCount := StrToInt(grpCts.Strings[i]);
    end;
  finally
    grpCts.Free;
  end;
  FGroupCount := grpNo;
end;

function TosSortedRows.GroupHeaderText(onCol, onRow : Integer) : String;
begin

end;

function TosSortedRows.NextDataRow(fromRow : Integer) : TosSortedRow;
var i : Integer;
begin
  Result := Nil;
  for i := fromRow + 1 to Self.Count do
    if (Self.SortedRow[i].IsData) then
    begin
      Result := SortedRow[i];
      break;
    end;
end;

function TosSortedRows.PriorDataRow(fromRow : Integer) : TosSortedRow;
var i : Integer;
begin
  Result := Nil;
  for i := fromRow - 1 downto 1 do
    if (Self.SortedRow[i].IsData) then
    begin
      Result := SortedRow[i];
      break;
    end;
end;

function TosSortedRows.GroupValuesDiffer(onRow : Integer; var onLevel, diffLevel : Integer) : Boolean;
var i : Integer;
    prevRow, currRow : TosSortedRow;
    sFldName, s1, s2 : String;
begin
  Result := False;
  prevRow := SortedRow[onRow-1];
  currRow := SortedRow[onRow];
  onLevel := 1;
  diffLevel := SortFields.GroupCount;
  if (prevRow.IsData and currRow.IsData) then
  begin
    // Only compare if both rows are data rows...
    for i := 0 to Self.FSortFields.Count - 1 do
    begin
      if SortFields.SortEntry[i].GroupSort then
      begin
        sFldName := SortFields.SortEntry[i].FieldName;
        s1 := VarToStr(prevRow.GroupValue(sFldName));
        s2 := VarToStr(currRow.GroupValue(sFldName));
        if (CompareText(s1, s2) <> 0) then
        begin
          diffLevel := diffLevel - i;
          Result := True;
          break;
        end;
      end;
    end;
    onLevel := SortFields.GroupCount;
  end
  else if (currRow.IsData) then
  begin
    onLevel := SortFields.GroupCount;
    Result := (prevRow.GroupLevel <> onLevel);
    diffLevel := 1;
  end
  else if (not currRow.IsData) then
  begin
    onLevel := currRow.GroupLevel;
    if (onLevel > SortFields.GroupCount) then
       onLevel := SortFields.GroupCount;
    Result := (currRow.GroupNumber <> prevRow.GroupNumber) or
              (currRow.GroupLevel <> prevRow.GroupLevel);
    diffLevel := 1;
  end;
end;

procedure TosSortedRows.CalcGroupNumbers;
var iRow, jGroup, gpLevel, diffLevel : Integer;
    entryRow : TosSortedRow;
begin
  if (Count = 0) then exit;
  
  jGroup := 1; //SortFields.GroupCount;
  SortedRow[1].FGroupNumber := jGroup;
  if (SortedRow[1].IsData) then
     SortedRow[1].FGroupLevel := SortFields.GroupCount
  else
     SortedRow[1].FGroupLevel := 1;
  SortedRow[1].FGroupBreak := True;
  for iRow := 2 to Self.Count do
  begin
    entryRow := SortedRow[iRow];
    entryRow.FGroupBreak := False;
    if (entryRow.IsGroupFooter) then
    begin
      entryRow.FGroupNumber := entryRow.GroupHeader.GroupNumber;
      entryRow.FGroupLevel  := entryRow.GroupHeader.GroupLevel;
    end
    else
    begin
      if (GroupValuesDiffer(iRow, gpLevel, diffLevel)) then
      begin
         Inc(jGroup, 1);
         entryRow.FGroupBreak := True;
      end;
      entryRow.FGroupNumber := jGroup;
      entryRow.FGroupLevel  := gpLevel;
    end;
  end;
end;

function TosSortedRows.NewGroupFooter(forHeader : TosGroupHeaderRow; iPos : Integer) : TosGroupFooterRow;
begin
  Result := TosGroupFooterRow.Create;
  Result.FOwner := Self;
  Result.FGroupHeader := forHeader;
  Result.FGroupNumber := forHeader.GroupNumber;
  Result.FGroupLevel  := forHeader.GroupLevel;
  Result.FRowType := rtGroupFooter;
  Result.FVisible := True;
  Result.FGroupVisible := True;
  Result.FRowHeight := 20;
  Result.FKeyValue := forHeader.FKeyValue;
  forHeader.FGroupFooter := Result;
  Insert(iPos, Result);
end;

procedure TosSortedRows.AddGroupFooters;
var iRow : Integer;
    entryRow : TosSortedRow;
begin
  // Run thru the list and for each header add a footer if there
  // already is not a footer...
  for iRow := Count downto 1 do
  begin
    entryRow := SortedRow[iRow];
    if (entryRow.RowType = rtGroupHeader) and
       (TosGroupHeaderRow(entryRow).GroupFooter = Nil) then
       NewGroupFooter(TosGroupHeaderRow(entryRow), iRow);
  end;
  FFootersOn := True;
end;

function TosSortedRows.NewGroupHeader(value : String; iGrp, iLevel, iPos : Integer) : TosGroupHeaderRow;
var i, j : Integer;
begin
  Result := TosGroupHeaderRow.Create;
  Result.FOwner := Self;  
  Result.FGroupNumber := iGrp;
  Result.FGroupLevel  := iLevel;
  Result.FRowType := rtGroupHeader;
  if (Copy(value, Length(value)-1, 2) = '-2') then
     Result.FSortText := Copy(value, 1, Length(value)-2) + '-1'
  else
     Result.FSortText := value + '-1';
  j := 0;
  for i := 0 to FSortFields.Count - 1 do
    if (FSortFields.SortEntry[i].GroupSort) then
    begin
      Inc(j);
      if (j >= iLevel) then
      begin
        Result.FGroupColumn := FSortFields.SortEntry[i].Column;
        break;
      end;
    end;
  Result.FExpanded := True;
  Result.FVisible := True;
  Result.FGroupVisible := True;
  Result.FRowHeight := 20;
  Result.FKeyValue := value;
  if (iGrp < High(Self.FOldExpanded)) then
     Result.FExpanded := FOldExpanded[iGrp];
  Inc(FGroupCount);
  Insert(iPos, Result);
end;

procedure TosSortedRows.InsertGroupHeaders;
var i, iGrp, jGrpCount, iLevel, addHeaderCnt : Integer;
    lastGroupHeader : TosGroupHeaderRow;
    onRow : TosSortedRow;

  function LocateParentHeader(atLevel, atPos : Integer; withText : String) : TosGroupHeaderRow;
  var j : Integer;
      hdrText : String;
  begin
    Result := Nil;
    for j := atPos downto 1 do
    begin
      if (j = 0) then break;
      if (SortedRow[j].IsGroupHeader) then
      begin
        hdrText := Self.GroupHeaderText(SortFields.SortEntry[atLevel-1].Column, j);
        if (SortedRow[j].GroupLevel = atLevel) and
           (AnsiCompareText(hdrText, withText) = 0) then
        begin
          Result := TosGroupHeaderRow(SortedRow[j]);
          break;
        end;
      end;
    end;
  end;

  function InsertHeaders(withText : String; atLevel, atPos : Integer; var insertCount : Integer) : TosGroupHeaderRow;
  var testLevel : Integer;
      hdrText : String;
  begin
    // check to make sure all parent headers are present starting from level 1 up...
    for testLevel := 1 to atLevel - 1 do
    begin
      hdrText := Self.GroupHeaderText(SortFields.SortEntry[testLevel-1].Column, atPos);
      if (LocateParentHeader(testLevel, atPos-1, hdrText) = Nil) then
      begin
        NewGroupHeader(withText, iGrp, testLevel, atPos-1);
        Inc(insertCount);
        Inc(iGrp);
      end;
    end;
    Result := NewGroupHeader(withText, iGrp, atLevel, atPos);
    Inc(insertCount);
  end;
begin
  lastGroupHeader := Nil;
  i := 1;
  iGrp := 0; 
  jGrpCount := 0;
  while (i <= Count) do
  begin
    onRow := SortedRow[i];
    if (onRow.FGroupBreak) then
    begin
      iGrp := onRow.GroupNumber;
      iLevel := onRow.GroupLevel;
      if (onRow.IsData) then
      begin
        if (lastGroupHeader <> nil) then
        begin
           lastGroupHeader.FGroupCount := jGrpCount;
           iGrp := lastGroupHeader.GroupNumber + 1;
        end;
        addHeaderCnt := 0;
        lastGroupHeader := InsertHeaders(onRow.SortText, iLevel, i, addHeaderCnt);
        Inc(i, addHeaderCnt);
        jGrpCount := 0;
      end
      else if (onRow.IsGroupHeader) then
      begin
        if (lastGroupHeader <> nil) then
           lastGroupHeader.FGroupCount := jGrpCount;
        lastGroupHeader := TosGroupHeaderRow(onRow);
        jGrpCount := 0;
      end;
    end;
    if (onRow.IsData) then
    begin
      onRow.GroupNumber := lastGroupHeader.GroupNumber;
      Inc(jGrpCount);
      if (jGrpCount = 1) then
         lastGroupHeader.FKeyValue := onRow.GroupText;
    end;
    Inc(i);
  end;
  if (lastGroupHeader <> nil) then
     lastGroupHeader.FGroupCount := jGrpCount;
end;

function TosSortedRows.AddSortField(colName : String; dataCol : Integer; sortMode : TosSortType; theVarType : TosDataType) : TosSortEntry;
begin
  if (Self.FSortFields.IndexOf(colName) = -1) then
  begin
    Result := TosSortEntry.Create;
    Result.FieldName := colName;
    Result.FColumn := dataCol;
    Result.SortMode := sortMode;
    Result.GroupSort := False;
    Result.DataType := theVarType;
    FSortFields.AddObject(colName, Result);
    Reload(False);
    //CheckGrouping;
  end
  else
    Result := Nil;
end;

procedure TosSortedRows.DoSort;
begin
  Sort(@CompareItems);
end;

function TosSortedRows.ReverseText(const aString : String) : String;
var cChar : Char;
    i, iOrd, jLen : Integer;
    theText : String;
begin
  Result := '';
  jLen := Length(aString);
  theText := Uppercase(aString);
  for i := 1 to jLen do
  begin
    cChar := theText[i];
    iOrd := Ord(cChar);
    if (iOrd = 32) then // Space
       iOrd := Ord('Z')
    else
       iOrd := iOrd + (90 - iOrd) - (iOrd - 65);
       Result := Result + Char(iOrd);
  end;
end;

procedure TosSortedRows.Reload;
begin
  if InUpdate then exit;
end;

procedure TosSortedRows.Clear;
begin
  inherited;
  FDataCount := 0;
  FGroupCount := 0;
  FMixedRowHeights := False;
end;

procedure TosSortedRows.ClearSorts;
var i : Integer;
begin
  for i := FSortFields.Count - 1 downto 0 do
    if (not FSortFields.SortEntry[i].GroupSort) then
       FSortFields.Delete(i);
end;

procedure TosSortedRows.ClearGroups;
var i : Integer;
begin
  for i := FSortFields.Count - 1 downto 0 do
    if (FSortFields.SortEntry[i].GroupSort) then
       FSortFields.Delete(i);
  RemoveNonDataRows;
end;

procedure TosSortedRows.ClearAll;
var i : Integer;
begin
  for i := FSortFields.Count - 1 downto 0 do
    FSortFields.Delete(i);
  RemoveNonDataRows;
end;

function TosSortedRows.RowForActiveRow(findActiveRow : Integer) : TosSortedRow;
var i : Integer;
begin
  Result := Nil;
  for i := 0 to Self.Count - 1 do
    if (TosSortedRow(Items[i]).ActiveRow = findActiveRow) then
    begin
      Result := TosSortedRow(Items[i]);
      break;
    end;
end;

procedure TosSortedRows.RemapEntries(fromRow : Integer; iDirection : Integer);
var i : Integer;
begin
  for i := 0 to Self.Count - 1 do
     if (TosSortedRow(Items[i]).ActiveRow >= fromRow) then
        TosSortedRow(Items[i]).ActiveRow := TosSortedRow(Items[i]).ActiveRow + iDirection;
end;

function TosSortedRows.RemoveEntry(theRow : TosSortedRow; bReMap : Boolean) : Boolean;
var iIndex, remapRow : Integer;
begin
  Result := False;
  remapRow := 0;
  iIndex := Self.IndexOf(theRow);
  if (iIndex >= 0) then
  begin
    if bRemap then
       remapRow := TosSortedRow(Items[iIndex]).ActiveRow;
    Self.Delete(iIndex);
    Dec(FDataCount);
    Result := True;
  end;
  if bRemap then RemapEntries(remapRow, -1)
end;

procedure TosSortedRows.RemoveNonDataRows;
var i : integer;
begin
  i := Count;
  while (i > 0) do
  begin
    if (SortedRow[i].RowType = rtGroupHeader) or
       (SortedRow[i].RowType = rtGroupFooter) then
    begin
       Delete(i-1);
       Dec(FGroupCount);
       if (FGroupCount < 0) then FGroupCount := 0;
    end
    else
    begin
       SortedRow[i].FGroupVisible := True;
       SortedRow[i].FGroupNumber := 0;
       SortedRow[i].FGroupHeader := Nil;
    end;
    Dec(i);
    if (Count = 0) then break;
  end;
  FFootersOn := False;
end;

procedure TosSortedRows.RemoveGroupFooters;
var i : integer;
begin
  i := Count;
  while (i > 0) do
  begin
    if (SortedRow[i].RowType = rtGroupFooter) then
       Delete(i-1)
    else if (SortedRow[i].RowType = rtGroupHeader) then
       TosGroupHeaderRow(SortedRow[i]).FGroupFooter := Nil;
    Dec(i);
    if (Count = 0) then break;
  end;
  FFootersOn := False;
end;

procedure TosSortedRows.RecalcFooterTotals;
var iRow : Integer;
    theRow : TosSortedRow;
begin
  for iRow := Self.Count downto 1 do
  begin
    theRow := Self.SortedRow[iRow];
    if (theRow.IsGroupFooter) then
       TosGroupFooterRow(theRow).ClearSubTotalFields
    else if (theRow.IsData) then
       theRow.IncFooterTotals
    else if (theRow.IsGroupHeader) and
            (TosGroupHeaderRow(theRow).GroupFooter <> Nil) then
       TosGroupHeaderRow(theRow).GroupFooter.CalcAverages;
  end;
  // Now do a pass to compute any averages...
end;

procedure TosSortedRows.RemoveGroupHeaders(forCol : Integer);
var i : integer;
    headerList : TStringList;
begin
  headerList := TStringList.Create;
  try
    i := Count;
    while (i > 0) do
    begin
      if (SortedRow[i].RowType = rtGroupHeader) then
      begin
        if (forCol = 0) then
           Delete(i-1)
        else if (TosGroupHeaderRow(SortedRow[i]).GroupColumn = forCol) then
           headerList.AddObject(IntToStr(i-1), SortedRow[i]);
      end
      else if (SortedRow[i].IsData) then // Need to clear off GroupHeader pointer on data rows
      begin
        if (forCol = 0) then
        begin
           SortedRow[i].FGroupHeader := Nil;
           SortedRow[i].GroupNumber := 0;
        end
        else if (SortedRow[i].FGroupHeader <> Nil) and
                (SortedRow[i].FGroupHeader.GroupColumn = forCol) then
           SortedRow[i].FGroupHeader := Nil;
      end;
      Dec(i);
      if (Count = 0) then break;
    end;
  finally
    if (forCol > 0) then // Remove specific headers now...
    begin
      for i := 0 to headerList.Count - 1 do
      begin
        Delete(StrToInt(headerList.Strings[i]));
        Dec(FGroupCount);
      end;
      if (FGroupCount < 0) then FGroupCount := 0;
    end;
    headerList.Free;
  end;
  FFootersOn := False;
end;

procedure TosSortedRows.BeginUpdate;
begin
  Inc(FUpdateOp);
end;

procedure TosSortedRows.EndUpdate;
begin
  Dec(FUpdateOp);
end;

function TosSortedRows.InUpdate: Boolean;
begin
  Result := (FUpdateOp > 0);
end;

{ TosSortedRow }

function TosSortedRow.FieldValue(fieldName : String) : Variant;
var theDataRow : TosSortedRow;
begin
  // GroupHeader needs to find the first data row and
  // GroupFooter needs to find the last data row of the group...
  Result := '';
  if (Self.IsGroupHeader) then
  begin
    theDataRow := Self.Owner.NextDataRow(Owner.IndexOf(Self));
    if (theDataRow <> Nil) then
       Result := theDataRow.FieldValue(fieldName);
  end
  else if (Self.IsGroupFooter) then
  begin
    theDataRow := Self.Owner.PriorDataRow(Owner.IndexOf(Self));
    if (theDataRow <> Nil) then
       Result := theDataRow.FieldValue(fieldName);
  end
end;

function TosSortedRow.FieldValueAsDouble(dataCol : Variant) : Double;
begin
  Result := 0;
end;

procedure TosSortedRow.ReCalcGroupNumber;
var i : Integer;
    sGroupText : String;
    currGroupHeader : TosGroupHeaderRow;
begin
  // loop thru and locate the header for this data record - used on new records
  sGroupText := Self.GroupText;
  for i := 1 to Self.FOwner.Count do
    if (FOwner.SortedRow[i].IsGroupHeader) then
    begin
       currGroupHeader := TosGroupHeaderRow(FOwner.SortedRow[i]);
       if (AnsiSameText(VarToStr(currGroupHeader.KeyValue), sGroupText)) then
       begin
         Self.FGroupNumber := currGroupHeader.GroupNumber;
         Self.FGroupLevel  := currGroupHeader.GroupLevel;
         Self.FGroupHeader := currGroupHeader;
         break;
       end;
    end;
end;

function TosSortedRow.GroupText: String;
var nextDataRow : TosSortedRow;
begin
  if (IsGroupHeader or IsGroupFooter) then
  begin
    nextDataRow := Owner.NextDataRow(Owner.IndexOf(Self));
    if (nextDataRow <> Nil) then
       Result := nextDataRow.GroupText;
  end
  else
     Result := '';
end;

function TosSortedRow.GroupValue(fieldName : String) : variant;
begin

end;

procedure TosSortEntry.ToggleSort;
begin
  if (FSortMode = stAscending) then
     FSortMode := stDescending
  else
     FSortMode := stAscending;
end;

function TosSortedRow.IsData: Boolean;
begin
  Result := (RowType = rtData);
end;

function TosSortedRow.IsGroupFooter: Boolean;
begin
  Result := (RowType = rtGroupFooter);
end;

function TosSortedRow.IsGroupHeader: Boolean;
begin
  Result := (RowType = rtGroupHeader);
end;

procedure TosSortedRow.SetGroupVisible(Value: Boolean);
begin
  if (FGroupVisible <> Value) then
  begin
    FGroupVisible := Value;
  end;
end;

procedure TosSortedRow.SetRowHeight(Value: Integer);
begin
  if (not Self.FOwner.InGroupOp) and
     (not Self.FOwner.InUpdate) then
     FRowHeight := Value;
end;

procedure TosSortedRow.SetSelected(const Value: Boolean);
begin
  if (not Self.FOwner.InGroupOp) and
     (not Self.FOwner.InUpdate) then
     FSelected := Value;
end;

procedure TosSortedRow.SetVisible(Value: Boolean);
begin
  if (not Self.FOwner.InGroupOp) and
     (not Self.FOwner.InUpdate) then
     FVisible := Value;
end;

procedure TosSortedRow.IncFooterTotals;
var iLevel, i : Integer;
    footerRow : TosGroupFooterRow;
    headerRow : TosGroupHeaderRow;
    subtotalField : TosSubTotalField;
    dValue : Double;
begin
  // Increment all footers associated with this data record...
  for iLevel := Owner.SortFields.GroupCount downto 1 do
  begin
    headerRow := Self.GroupHeader;
    while (headerRow.GroupLevel <> iLevel) do
      headerRow := headerRow.GroupHeader;
    footerRow := headerRow.GroupFooter;
    if (footerRow <> Nil) then
      for i := 0 to footerRow.SubTotalFieldCount - 1 do
      begin
        subtotalField := footerRow.SubTotalField[i];
        case subtotalField.SummaryOp of
          gsoCount : subtotalField.SubTotal := subtotalField.SubTotal + 1;
          gsoSum   : subtotalField.SubTotal := subtotalField.SubTotal + Self.FieldValueAsDouble(subTotalField.DataCol);
          gsoMax   : begin
                       dValue := Self.FieldValueAsDouble(subTotalField.DataCol);
                       if (dValue > subTotalField.SubTotal) then
                          subtotalField.SubTotal := dValue;
                     end;
          gsoMin   : begin
                       dValue := Self.FieldValueAsDouble(subTotalField.DataCol);
                       if (dValue < subTotalField.SubTotal) then
                          subtotalField.SubTotal := dValue;
                     end;
          gsoAvg   : subtotalField.SubTotal := subtotalField.SubTotal + Self.FieldValueAsDouble(subTotalField.DataCol);
        end;
      end;
  end;
end;

function TosSortedRow.GetGroupHeader : TosGroupHeaderRow;
var i : Integer;
    theHeader : TosSortedRow;
begin
  Result := Self.FGroupHeader;
  if (Result = Nil) and
     (Self.Owner.GroupCount > 0) then
  begin
    if (Self.IsGroupHeader) or
       (Self.IsGroupFooter) then
       exit;
       
    i := Owner.IndexOf(Self) + 1;
    for i := i downto 1 do
    begin
      theHeader := Owner.SortedRow[i];
      if (theHeader.IsGroupHeader) then
      begin
        if ((Self.IsData) and
            (theHeader.GroupLevel = Self.GroupLevel)) or
           ((not Self.IsData) and
            (theHeader.GroupLevel < Self.GroupLevel)) then
        begin
          FGroupHeader := TosGroupHeaderRow(theHeader);
          Result := FGroupHeader;
          break;
        end;
      end;
    end;
  end;
end;

function TosSortedRow.GetSortPositionValue : Integer;
begin
  Result := GroupNumber * 10 + 2;
end;

{ TosSortFieldList }
function TosSortFieldList.GroupCount : Integer;
var i : Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if (SortEntry[i].GroupSort) then
       Inc(Result);
end;

function TosSortFieldList.SortCount : Integer;
var i : Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if (not SortEntry[i].GroupSort) then
       Inc(Result);
end;

function TosSortFieldList.GetSortEntry(i : Integer) : TosSortEntry;
begin
  if i < Self.Count then
     Result := TosSortEntry(Objects[i])
  else
     Result := Nil;
end;

function TosSortFieldList.SortEntryByName(fieldName : String) : TosSortEntry;
var i : Integer;
begin
  Result := Nil;
  for i := 0 to Self.Count - 1 do
    if (AnsiCompareText(TosSortEntry(Objects[i]).FieldName, fieldName) = 0) then
       Result := TosSortEntry(Objects[i]);
end;

function TosSortFieldList.SortEntryByNumber(colNumber : Integer) : TosSortEntry;
var i : Integer;
begin
  Result := Nil;
  for i := 0 to Self.Count - 1 do
    if (TosSortEntry(Objects[i]).Column = colNumber) then
       Result := TosSortEntry(Objects[i]);
end;

procedure TosSortFieldList.ClearAll;
begin
  Self.Clear;
end;

procedure TosSortFieldList.ClearGroups;
var i : Integer;
begin
  for i := Count - 1 downto 0 do
    if (SortEntry[i].GroupSort) then
       Delete(i);
end;

procedure TosSortFieldList.ClearSorts;
var i : Integer;
begin
  for i := Count - 1 downto 0 do
    if (not SortEntry[i].GroupSort) then
       Delete(i);
end;

procedure TosSortFieldList.AddGroupField(fldName : String; addEntry : TosSortEntry);
var i : Integer;
begin
  i := 0;
  while (i < Count) do
  begin
    if (not SortEntry[i].GroupSort) then
       break;
    Inc(i);
  end;
  Self.InsertObject(i, fldName, addEntry);
end;

function TosSortFieldList.GroupLevelOf(fldName : String) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := 0 to Self.Count - 1 do
  begin
    if (TosSortEntry(Objects[i]).GroupSort) then
    begin
      if (AnsiCompareText(TosSortEntry(Objects[i]).FieldName, fldName) = 0) then
      begin
        Inc(Result);
        break;
      end
      else
         Inc(Result);
    end;
  end;
end;

{ TosGroupHeaderRow }

function TosGroupHeaderRow.GroupValue(fieldName: String): variant;
begin
  Result := Self.FKeyValue;
end;

function TosGroupHeaderRow.GroupText : String;
var nextDataRow : TosSortedRow;
    sortEntry : TosSortEntry;
begin
  nextDataRow := Owner.NextDataRow(Owner.IndexOf(Self));
  if (nextDataRow <> Nil) then
  begin
    sortEntry := Owner.SortFields.SortEntryByNumber(Self.GroupColumn);
    if (sortEntry <> Nil) then
    begin
      if (Owner.SortFields.GroupLevelOf(sortEntry.FieldName) = Owner.SortFields.GroupCount) then
         Result := nextDataRow.GroupText
      else
         Result := nextDataRow.GroupValue(sortEntry.FieldName);
    end;
  end;
end;

procedure TosGroupHeaderRow.ResetGroupHeader(fromPos: Integer);
var theHeader : TosSortedRow;
    i : Integer;
begin
  for i := fromPos downto 1 do
  begin
    theHeader := Owner.SortedRow[i];
    if (theHeader.IsGroupHeader) then
    begin
      if (theHeader.GroupLevel < Self.FGroupLevel) then
      begin
        FGroupHeader := TosGroupHeaderRow(theHeader);
        break;
      end;
    end
  end;
end;

function TosGroupHeaderRow.GetSortPositionValue : Integer;
begin
  Result := GroupNumber * 10 + 1;
end;

{ TosGroupFooterRow }

constructor TosGroupFooterRow.Create;
begin
  FSubTotalFields := TObjectList.Create;
  FSortPositionValue := 0;
end;

destructor TosGroupFooterRow.Destroy;
begin
  FreeAndNil(FSubTotalFields);
end;

function TosGroupFooterRow.GetSubTotalField(index: Integer): TosSubTotalField;
begin
  Result := TosSubTotalField(FSubTotalFields.Items[index]);
end;

function TosGroupFooterRow.GroupValue(fieldName: String): variant;
begin
  Result := FKeyValue;
end;

function TosGroupFooterRow.GetSortPositionValue : Integer;
var i, j : Integer;
    entryRow : TosSortedRow;
begin
  if (FSortPositionValue = 0) then // need to scan down from Header to find Next Sibling Header...
  begin
    j := Self.Owner.IndexOf(Self.GroupHeader);
    for i := j + 2 to Owner.Count do
    begin
      entryRow := Owner.SortedRow[i];
      if (entryRow.RowType = rtGroupHeader) and
         (entryRow.GroupLevel <= Self.GroupLevel) then
      begin
        FSortPositionValue := TosGroupHeaderRow(entryRow).SortPositionValue - 1 - Self.GroupLevel + entryRow.GroupLevel;
        break;
      end;
    end;
    if (FSortPositionValue = 0) then
       FSortPositionValue := (Owner.GroupCount + 1) * 10 - Self.GroupLevel;
  end;
  Result := FSortPositionValue;
end;

procedure TosGroupFooterRow.ReCalcAll;
begin

end;

procedure TosGroupFooterRow.RefreshSubTotalFields;
begin

end;

procedure TosGroupFooterRow.ClearSubTotalFields;
var i : Integer;
begin
  for i := 0 to FSubTotalFields.Count - 1 do
  begin
    if (SubTotalField[i].SummaryOp = gsoMin) then
       SubTotalField[i].SubTotal := 9999999999.99
    else
       SubTotalField[i].SubTotal := 0.0;
  end;
end;

procedure TosGroupFooterRow.CalcAverages;
var i : Integer;
begin
  for i := 0 to FSubTotalFields.Count - 1 do
    if (SubTotalField[i].SummaryOp = gsoAvg) and
       (GroupHeader <> Nil) and
       (GroupHeader.GroupCount > 0) then
       SubTotalField[i].SubTotal := SubTotalField[i].SubTotal / Self.GroupHeader.GroupCount;
end;

function TosGroupFooterRow.AdjustSubTotal(newValue, deltaValue : Double; dataCol : Integer) : Boolean;
var stField : TosSubTotalField;
begin
  Result := False;
  stField := Self.SubTotalFieldForDataCol(dataCol);
  if (stField <> Nil) then
  begin
    Result := (stField.SummaryOp <> gsoCount);
    case stField.SummaryOp of
      gsoSum : stField.SubTotal := stField.SubTotal + deltaValue;
      gsoAvg : if (GroupHeader.GroupCount > 0) then
                  stField.SubTotal := ((stField.SubTotal * GroupHeader.GroupCount)+deltaValue)/GroupHeader.GroupCount;
      gsoMin : stField.SubTotal := CalcMin(stField);
      gsoMax : stField.SubTotal := CalcMax(stField);
    end;
  end;
end;

function TosGroupFooterRow.CalcMin(onSubTotalField : TosSubTotalField) : Double;
var iRow : Integer;
    dValue : Double;
begin
  Result := 999999999999.99;
  // Start at the header row and come down to this footer...
  iRow := Owner.IndexOf(Self.GroupHeader) + 1;
  while (Owner.SortedRow[iRow] <> Self) and
        (iRow <= Owner.Count) do
  begin
    if (Owner.SortedRow[iRow].IsData) then
    begin
      dValue := Owner.SortedRow[iRow].FieldValueAsDouble(onSubTotalField.DataCol);
      if (dValue < Result) then
         Result := dValue;
    end;
    Inc(iRow);
  end;
end;

function TosGroupFooterRow.CalcMax(onSubTotalField : TosSubTotalField) : Double;
var iRow : Integer;
    dValue : Double;
begin
  Result := -999999999999.99;
  // Start at the header row and come down to this footer...
  iRow := Owner.IndexOf(Self.GroupHeader) + 1;
  while (Owner.SortedRow[iRow] <> Self) and
        (iRow <= Owner.Count) do
  begin
    if (Owner.SortedRow[iRow].IsData) then
    begin
      dValue := Owner.SortedRow[iRow].FieldValueAsDouble(onSubTotalField.DataCol);
      if (dValue > Result) then
         Result := dValue;
    end;
    Inc(iRow);
  end;
end;

function TosGroupFooterRow.EnsureSubTotal(dataCol : Integer; summOp : TosGroupSummaryOp) : TosSubTotalField;
begin
  Result := SubTotalFieldForDataCol(dataCol);
  if (Result = Nil) then
  begin
    Result := TosSubTotalField.Create;
    Result.FDataCol := dataCol;
    Result.FSubTotal := 0;
    Result.FSummaryOp := summOp;
    FSubTotalFields.Add(Result);
  end;
end;

function TosGroupFooterRow.SubTotalFieldCount : Integer;
begin
  Result := FSubTotalFields.Count;
end;

function TosGroupFooterRow.SubTotalFieldForDataCol(dataCol: Integer): TosSubTotalField;
var i : Integer;
begin
  Result := Nil;
  for i := 0 to FSubTotalFields.Count - 1 do
    if (SubTotalField[i].DataCol = dataCol) then
    begin
      Result := SubTotalField[i];
      break;
    end;
end;

end.
