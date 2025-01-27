unit duEditTgColumns;

interface

{$INCLUDE TSCmpVer}
//{$DEFINE rtTest}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Dialogs, osAdvDbGrid, StdCtrls, Buttons, ExtCtrls, db, DBTables, ComCtrls,
  Grids_ts, TSGrid, TsDbGrid, ImgList, tsCommon, TypInfo, registry, TsDateTimeDef
  {$IFDEF TSVER_V6}, Variants, ValEdit {$ENDIF}
  , Menus, osColorComboBox;

const
  TG_REGENTRIES = '\Software\ObjectSight\TopGrid\';

type
  TdgEditTgColumns = class(TForm)
    ImageList1: TImageList;
    dgFont: TFontDialog;
    dgColor: TColorDialog;
    pnTop: TPanel;
    Splitter1: TSplitter;
    laSampleGrid: TLabel;
    VisibleGrid: TosAdvDbGrid;
    InvisibleGrid: TosAdvDbGrid;
    propertyGrid: TosAdvDbGrid;
    edHeading: TEdit;
    hcAlign: TPanel;
    btLeft: TSpeedButton;
    btCenter: TSpeedButton;
    btRight: TSpeedButton;
    btDelete: TSpeedButton;
    pnSelected: TPanel;
    pmOptions: TPopupMenu;
    miAddColumn: TMenuItem;
    miRemoveColumn: TMenuItem;
    N1: TMenuItem;
    miHideColumn: TMenuItem;
    N2: TMenuItem;
    miComboProperties: TMenuItem;
    ChangeTo1: TMenuItem;
    Combo1: TMenuItem;
    CheckBox1: TMenuItem;
    Image1: TMenuItem;
    DateTimePopup1: TMenuItem;
    DateTimeDropdown1: TMenuItem;
    SpinEdit1: TMenuItem;
    miTextControl: TMenuItem;
    N3: TMenuItem;
    CopyPropertiesfromTtsDbGrid1m: TMenuItem;
    pnBottom: TPanel;
    Panel2: TPanel;
    btDone: TSpeedButton;
    btCancel: TSpeedButton;
    cbColors: TosColorComboBox;
    pnMid: TPanel;
    btAdd: TSpeedButton;
    btRemove: TSpeedButton;
    pnFields: TPanel;
    Label1: TLabel;
    lbFields: TListBox;
    pnColumns: TPanel;
    Label2: TLabel;
    lbColumns: TListBox;
    udPosition: TUpDown;
    chHidden: TCheckBox;
    pnProperties: TPanel;
    laColumn: TLabel;
    gdProperties: TtsGrid;
    chShowAll: TCheckBox;
    gbComboProperties: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txDisplayCol: TLabel;
    cbComboDataSource: TComboBox;
    btAdvancedCombo: TButton;
    cbValueCol: TComboBox;
    cbDisplayCol: TComboBox;
    procedure udPositionClick(Sender: TObject; Button: TUDBtnType);
    procedure btAddClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure lbFieldsDblClick(Sender: TObject);
    procedure lbColumnsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chShowAllClick(Sender: TObject);
    procedure gdPropertiesComboInit(Sender: TObject; Combo: TtsComboGrid;
      DataCol, DataRow: Integer);
    procedure gdPropertiesComboDropDown(Sender: TObject;
      Combo: TtsComboGrid; DataCol, DataRow: Integer);
    procedure gdPropertiesComboCellLoaded(Sender: TObject;
      Combo: TtsComboGrid; DataCol, DataRow: Integer; var Value: Variant);
    procedure gdPropertiesComboGetValue(Sender: TObject;
      Combo: TtsComboGrid; GridDataCol, GridDataRow, ComboDataRow: Integer;
      var Value: Variant);
    procedure gdPropertiesDblClick(Sender: TObject);
    procedure gdPropertiesEndCellEdit(Sender: TObject; DataCol,
      DataRow: Integer; var Cancel: Boolean);
    procedure gdPropertiesCellChanged(Sender: TObject; OldCol, NewCol,
      OldRow, NewRow: Integer);
    procedure gdPropertiesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbColumnsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure miAddColumnClick(Sender: TObject);
    procedure miRemoveColumnClick(Sender: TObject);
    procedure miHideColumnClick(Sender: TObject);
    procedure Combo1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure DateTimePopup1Click(Sender: TObject);
    procedure DateTimeDropdown1Click(Sender: TObject);
    procedure SpinEdit1Click(Sender: TObject);
    procedure miTextControlClick(Sender: TObject);
    procedure edHeadingChange(Sender: TObject);
    procedure edHeadingKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure pnFieldsResize(Sender: TObject);
    procedure pnColumnsResize(Sender: TObject);
    procedure pnPropertiesResize(Sender: TObject);
    procedure gdPropertiesResize(Sender: TObject);
    procedure propertyGridColMoved(Sender: TObject; ToDisplayCol,
      Count: Integer; ByUser: Boolean);
    procedure pmOptionsPopup(Sender: TObject);
    procedure miComboPropertiesClick(Sender: TObject);
    procedure btAdvancedComboClick(Sender: TObject);
    procedure cbComboDataSourceClick(Sender: TObject);
    procedure cbValueColClick(Sender: TObject);
    procedure cbDisplayColClick(Sender: TObject);
    procedure propertyGridSorting(Sender: TObject; DataCol: Integer;
      var Cancel: Boolean);
    procedure propertyGridGrouping(Sender: TObject; DataCol: Integer;
      var Cancel: Boolean);
    procedure chHiddenClick(Sender: TObject);
    procedure propertyGridColResized(Sender: TObject; RowColnr: Integer);
    procedure lbColumnsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbColumnsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbColumnsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbColumnsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbColumnsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gdPropertiesButtonClick(Sender: TObject; DataCol,
      DataRow: Integer);
    procedure propertyGridMouseStatusChanged(Sender: TObject; OldStatus,
      NewStatus: TtsMouseStatus);
    procedure CopyPropertiesfromTtsDbGrid1mClick(Sender: TObject);
    procedure propertyGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbComboDataSourceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btLeftClick(Sender: TObject);
    procedure btCenterClick(Sender: TObject);
    procedure btRightClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure propertyGridTopLeftChanged(Sender: TObject; OldCol, OldRow,
      NewCol, NewRow: Integer; ByUser: Boolean);
    procedure pnTopResize(Sender: TObject);
    procedure pnMidResize(Sender: TObject);
  private
    { Private declarations }
    FSourceGrid : TosAdvDbGrid;
    FDataSet : TDataSet;
    FSelectedColumn, FIsReadyCount, FDragIndex : Integer;
    FColPropertiesRefresh, FSelecting, FUpdating, FBinding, FChangingHeading : Boolean;
    FColumnsChanged, FColumnResized : Boolean;
    FEditingProperty : String;
    FComboDisplayColName : String;
    FlsDataSources : TStringList;

    procedure SaveWindowState;
    procedure ResetWindowState;
    procedure RefreshComboFields;
    function  ComboColCount : Integer;
    procedure AdjustHiddenComboColumn;
    procedure RefreshComboDisplay;
    //procedure SetupDefaultCombo;
    procedure DisplayColumnButtons;
    procedure AdjustPropertyPanel;
    procedure RefreshColProperties;
    procedure ReselectColumn;
    procedure AddField(iIndex : Integer);
    procedure ResetSelection;
    procedure RemoveColumn(iIndex : Integer);
    procedure SetSelectedColumn(Value : Integer);
    function  GetColPropertyValueText(PropertyName: String; forRow : Integer) : String;
    function  SelectedDbCol : TosDbCol;
    procedure BindColProperty(DataRow : Integer);
    function  GetIsReady : Boolean;
    procedure SetIsReady(Value : Boolean);
    procedure EditFont;

    procedure LoadVisibleColumns;
    procedure LoadHiddenColumns;
    procedure BindGridProperties(toGrid, fromGrid : TosAdvDbGrid; bIncludeData : Boolean = False);
    procedure SetColumnsChanged(Value : Boolean);
    procedure SetHeading(Value : String);
  public
    { Public declarations }

    function Execute(adjustGrid : TosAdvDbGrid; withDataSet : TDataSet) : Boolean;
    procedure SaveChanges;
    
    property SelectedColumn : Integer read FSelectedColumn write SetSelectedColumn;
    property lsDataSources : TStringList read FlsDataSources;
    property IsReady : Boolean read GetIsReady write SetIsReady;
    property ColumnsChanged : Boolean read FColumnsChanged write SetColumnsChanged;
  end;


var
  dgEditTgColumns: TdgEditTgColumns;
  lsDateTimeDefs, EnumList : TStringList;

implementation

uses duOsComboWizard, duSpinOptions, dutgLocateDbGrid,
     {$IFNDEF rtTest} osAdvGridEditors, {$ENDIF}
     {$IFDEF TSVER_V6} Types, {$ENDIF}
     auOsDesignUtils, TSImageList;

{$R *.dfm}

function TdgEditTgColumns.Execute(adjustGrid : TosAdvDbGrid; withDataSet : TDataSet) : Boolean;
var i : Integer;
begin
  SelectedColumn := 0;
  ColumnsChanged := False;

  adjustGrid.Designing := True;
  VisibleGrid.Designing := True;
  InvisibleGrid.Designing := True;
  try
    FDataSet := withDataSet;
    lbFields.Items.Clear;
    if (FDataSet <> Nil) then
       FDataSet.GetFieldNames(lbFields.Items);
      {for i := 0 to FDataSet.FieldDefs.Count - 1 do
        if (adjustGrid.LocateColumn(FDataSet.FieldDefs.Items[i].Name) = Nil) then
           lbFields.Items.Add(FDataSet.FieldDefs.Items[i].Name);}
    if (lbFields.Items.Count > 0) then
       lbFields.ItemIndex := 0;
    if (adjustGrid.HeadingOptions.Height > 35) then
       edHeading.Height := 35
    else
       edHeading.Height := adjustGrid.HeadingOptions.Height;
    edHeading.Top := propertyGrid.Top - edHeading.Height - 5;
    if (Assigned(adjustGrid.HeadingOptions.Font)) then
       edHeading.Font.Assign(adjustGrid.HeadingOptions.Font)
    else
       edHeading.Font.Assign(adjustGrid.Font);
    edHeading.Color := adjustGrid.HeadingOptions.Color;
    if (edHeading.Color = clBtnFace) then
       edHeading.Color := clWindow;

    FSourceGrid := adjustGrid;
    BindGridProperties(VisibleGrid, FSourceGrid);
    CustomColumnDbAssign(VisibleGrid, FSourceGrid);
    for i := VisibleGrid.Cols downto 1 do
      if (not VisibleGrid.Col[i].Visible) then
         VisibleGrid.DeleteCols(i,i);
    BindGridProperties(InvisibleGrid, FSourceGrid);
    CustomColumnDbAssign(InvisibleGrid, FSourceGrid);
    for i := InvisibleGrid.Cols downto 1 do
    begin
      if (InvisibleGrid.Col[i].Visible) then
         InvisibleGrid.DeleteCols(i,i)
      else
         InvisibleGrid.Col[i].Visible := True;
    end;
    BindGridProperties(propertyGrid, VisibleGrid, True);
    LoadVisibleColumns;
    propertyGrid.DataSource := FSourceGrid.DataSource;

    // Now determine panel positions depending on the height of the heading...
    propertyGrid.Height := propertyGrid.HeadingOptions.Height + propertyGrid.RowOptions.DefaultRowHeight + 40;
    propertyGrid.GroupingSortingOptions.GroupColumnsHidden := False;
    //pnFields.Top  := propertyGrid.Top + propertyGrid.Height + 3;
    //pnColumns.Top := pnFields.Top;
    //pnProperties.Top := pnFields.Top;
    FormResize(Self);

    Result := False;
    if (ShowModal = mrOk) then
       Result := True;
  finally
    adjustGrid.Designing := False;
    VisibleGrid.Designing := False;
    InvisibleGrid.Designing := False;
  end;
end;

procedure TdgEditTgColumns.BindGridProperties(toGrid, fromGrid : TosAdvDbGrid; bIncludeData : Boolean);
begin
  toGrid.Cols := fromGrid.Cols;
  if bIncludeData then
     toGrid.DataSource := fromGrid.DataSource;
  toGrid.ComboQuery := fromGrid.ComboQuery;
  toGrid.FieldState := fsCustomized;
  toGrid.HeadingOptions.Visible := True;
  toGrid.GridOptions.Color := fromGrid.GridOptions.Color;
  toGrid.HeadingOptions.Color := fromGrid.HeadingOptions.Color;
  toGrid.RowOptions.RowBarOn := fromGrid.RowOptions.RowBarOn;
  toGrid.HeadingOptions.Height := fromGrid.HeadingOptions.Height;
  toGrid.HeadingOptions.Font.Assign(fromGrid.HeadingOptions.Font);
  toGrid.HeadingOptions.VertAlignment := fromGrid.HeadingOptions.VertAlignment;
  toGrid.HeadingOptions.HorzAlignment := fromGrid.HeadingOptions.HorzAlignment;
  toGrid.HeadingOptions.WordWrap      := fromGrid.HeadingOptions.WordWrap;
  toGrid.HeadingOptions.ParentFont    := fromGrid.HeadingOptions.ParentFont;

  toGrid.GridOptions.Assign(fromGrid.GridOptions);
  toGrid.RowOptions.Assign(fromGrid.RowOptions);
  toGrid.ColumnOptions.Assign(fromGrid.ColumnOptions);
  toGrid.GroupingSortingOptions.Assign(fromGrid.GroupingSortingOptions);
  toGrid.GroupingSortingOptions.GroupColumnsHidden := False;

  toGrid.ImageList := fromGrid.ImageList;
  if fromGrid.HeadingOptions.ParentFont then
     edHeading.Font.Assign(fromGrid.Font)
  else
     edHeading.Font.Assign(fromGrid.HeadingOptions.Font);

  toGrid.GridOptions.ProvideGridMenu := False;
  toGrid.HeadingOptions.ProvideHeadingMenu := False;
end;

procedure TdgEditTgColumns.LoadVisibleColumns;
var i, j : Integer;
begin
  lbColumns.Items.Clear;
  propertyGrid.BeginUpdate;
  SelectedColumn := 0;
  IsReady := False;
  lbFields.Items.BeginUpdate;
  try
    CustomColumnDbAssign(propertyGrid, VisibleGrid);
    lbFields.Items.Clear;
    if (FDataSet <> Nil) then
       FDataSet.GetFieldNames(lbFields.Items);
    for i := 1 to propertyGrid.Cols do
    begin
      if (propertyGrid.Col[i].Visible) then
      begin
        if (Trim(propertyGrid.Col[i].FieldName) = '') then
           lbColumns.Items.Add(propertyGrid.Col[i].Heading)
        else
           lbColumns.Items.Add(propertyGrid.Col[i].FieldName);
      end;
      j := lbFields.Items.IndexOf(propertyGrid.Col[i].FieldName);
      if (j >= 0) then
         lbFields.Items.Delete(j);
      propertyGrid.Col[i].Tag := 0;
    end;
    for i := 1 to InvisibleGrid.Cols do
    begin
      j := lbFields.Items.IndexOf(InvisibleGrid.Col[i].FieldName);
      if (j >= 0) then
         lbFields.Items.Delete(j);
    end;
  finally
    IsReady := True;
    propertyGrid.EndUpdate;
    lbFields.Items.EndUpdate;
  end;
  if (propertyGrid.Cols > 0) and
     (IsReady) then
  begin
    SelectedColumn := 1;
    for i := 0 to lbColumns.Items.Count - 1 do
       lbColumns.Selected[i] := False;
    lbColumns.Selected[lbColumns.ItemIndex] := True;
  end;
  laSampleGrid.Caption := 'Sample Grid Layout (Visible Columns)';
  propertyGrid.DataSource := FSourceGrid.DataSource;
end;

procedure TdgEditTgColumns.LoadHiddenColumns;
var i, j : Integer;
begin
  lbColumns.Items.Clear;
  propertyGrid.BeginUpdate;
  SelectedColumn := 0;
  IsReady := False;
  lbFields.Items.BeginUpdate;
  try
    CustomColumnDbAssign(propertyGrid, InvisibleGrid);
    lbFields.Items.Clear;
    if (FDataSet <> Nil) then
       FDataSet.GetFieldNames(lbFields.Items);
    for i := 1 to propertyGrid.Cols do
    begin
      if (propertyGrid.Col[i].Visible) then
      begin
        if (Trim(propertyGrid.Col[i].FieldName) = '') then
           lbColumns.Items.Add(propertyGrid.Col[i].Heading)
        else
           lbColumns.Items.Add(propertyGrid.Col[i].FieldName);
      end;
      j := lbFields.Items.IndexOf(propertyGrid.Col[i].FieldName);
      if (j >= 0) then
         lbFields.Items.Delete(j);
      propertyGrid.Col[i].Tag := 0;
    end;
    for i := 1 to VisibleGrid.Cols do
    begin
      j := lbFields.Items.IndexOf(VisibleGrid.Col[i].FieldName);
      if (j >= 0) then
         lbFields.Items.Delete(j);
    end;
  finally
    IsReady := True;
    propertyGrid.EndUpdate;
    lbFields.Items.EndUpdate;
  end;
  if (propertyGrid.Cols > 0) and
     (IsReady) then
  begin
    SelectedColumn := 1;
    for i := 0 to lbColumns.Items.Count - 1 do
       lbColumns.Selected[i] := False;
    lbColumns.Selected[lbColumns.ItemIndex] := True;
  end;
  laSampleGrid.Caption := 'Sample Grid Layout (Hidden Columns)';
end;

procedure TdgEditTgColumns.udPositionClick(Sender: TObject;
  Button: TUDBtnType);
var i : Integer;
begin
  i := lbColumns.ItemIndex;
  if (Button = btPrev) then // down
  begin
    if (lbColumns.ItemIndex < lbColumns.Items.Count - 1) then
    begin
      lbColumns.Items.Move(lbColumns.ItemIndex, lbColumns.ItemIndex + 1);
      lbColumns.ItemIndex := i + 1;
      propertyGrid.Col[SelectedColumn].DisplayCol := lbColumns.ItemIndex + 1;
    end;
  end
  else
  begin
    if (lbColumns.ItemIndex > 0) then
    begin
      lbColumns.Items.Move(lbColumns.ItemIndex, lbColumns.ItemIndex - 1);
      lbColumns.ItemIndex := i - 1;
      propertyGrid.Col[SelectedColumn].DisplayCol := lbColumns.ItemIndex + 1;
    end;
  end;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.btAddClick(Sender: TObject);
begin
  AddField(lbFields.ItemIndex);
end;

procedure TdgEditTgColumns.btRemoveClick(Sender: TObject);
var i, j : Integer;
begin
  FUpdating := True;
  try
    j := lbColumns.ItemIndex;
    for i := lbColumns.Items.Count - 1 downto 0 do
      if (lbColumns.Selected[i]) then
         RemoveColumn(i);
  finally
    FUpdating := False;
  end;
  if (j < lbColumns.Items.Count) then
     lbColumns.ItemIndex := j
  else
     lbColumns.ItemIndex := lbColumns.Items.Count - 1;
  ResetSelection;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.AddField(iIndex : Integer);
var i, j : Integer;
begin
  j := lbColumns.ItemIndex;
  if (j = -1) then j := 0;
  for i := 0 to lbColumns.Items.Count - 1 do
     lbColumns.Selected[i] := False;
  if (j = 0) then
  begin
    lbColumns.Items.Insert(j, lbFields.Items.Strings[iIndex]);
    lbColumns.ItemIndex := j;
  end
  else
  begin
    lbColumns.Items.Insert(j+1, lbFields.Items.Strings[iIndex]);
    lbColumns.ItemIndex := j+1;
  end;
  lbColumns.Selected[lbColumns.ItemIndex] := True;
  
  lbFields.Items.Delete(iIndex);
  if (iIndex < lbFields.Items.Count) then
     lbFields.ItemIndex := iIndex
  else
     lbFields.ItemIndex := iIndex - 1;

  propertyGrid.Cols := propertyGrid.Cols + 1;
  propertyGrid.Col[propertyGrid.Cols].DisplayCol := j + 2;
  propertyGrid.Col[propertyGrid.Cols].FieldName := lbColumns.Items.Strings[lbColumns.ItemIndex];
  propertyGrid.Col[propertyGrid.Cols].Heading := lbColumns.Items.Strings[lbColumns.ItemIndex];
  SelectedColumn := propertyGrid.Col[propertyGrid.Cols].DataCol;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.RemoveColumn(iIndex : Integer);
begin
  if (lbColumns.Items.Strings[iIndex] <> '') then
     lbFields.ItemIndex := lbFields.Items.Add(lbColumns.Items.Strings[iIndex]);
  lbColumns.Items.Delete(iIndex);
  if (iIndex < lbColumns.Items.Count) then
     lbColumns.ItemIndex := iIndex
  else
     lbColumns.ItemIndex := iIndex - 1;
  propertyGrid.DeleteCols(propertyGrid.DataColnr[iIndex+1], propertyGrid.DataColnr[iIndex+1]);
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.ResetSelection;
begin
  if (lbColumns.ItemIndex >= 0) then
  begin
    lbColumns.Selected[lbColumns.ItemIndex] := True;
    FSelectedColumn := 0;
    ReselectColumn;
  end
  else
    SelectedColumn := 0;
end;

procedure TdgEditTgColumns.btDoneClick(Sender: TObject);
begin
  SaveChanges;
  ModalResult := mrOk;
end;

procedure TdgEditTgColumns.SaveChanges;
var i, j : Integer;
begin
  //if (FSelectedColumn > 0) then
  //   propertyGrid.Col[FSelectedColumn].HeadingColor := TColor(propertyGrid.Col[FSelectedColumn].Tag);

  if (chHidden.Checked) then
     CustomColumnDbAssign(InvisibleGrid, propertyGrid)
  else
     CustomColumnDbAssign(VisibleGrid, propertyGrid);

  try
    CustomColumnDbAssign(FSourceGrid, VisibleGrid);
    if (InvisibleGrid.Cols > 0) then
    begin
      j := FSourceGrid.Cols + 1;
      FSourceGrid.BeginUpdate;
      try
        FSourceGrid.Cols := FSourceGrid.Cols + InvisibleGrid.Cols;
        for i := 1 to InvisibleGrid.Cols do
        begin
          FSourceGrid.Col[j].Assign(InvisibleGrid.Col[i]);
          FSourceGrid.Col[j].Visible := False;
          Inc(j);
        end;
      finally
        FSourceGrid.EndUpdate;
      end;
    end;
    if (FColumnsChanged) then
       FSourceGrid.FieldState := fsCustomized;
  except on E:Exception do
    raise Exception.Create('Error saving Combo changes...' + #10#13 + E.Message);
  end;
end;

procedure TdgEditTgColumns.btCancelClick(Sender: TObject);
var wResp : Word;
begin
  if (Self.FColumnsChanged) then
  begin
    wResp := MessageDlg('You have unapplied changes - apply them?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if (wResp = mrYes) then
       SaveChanges
    else if (wResp = mrCancel) then
       exit;
  end;
  ModalResult := mrCancel;
end;

procedure TdgEditTgColumns.SetSelectedColumn(Value : Integer);
var currentControl : TWinControl;
    theDisplayCol, i : Integer;
begin
  if FUpdating then exit;

  propertyGrid.ResetColProperties([prSelected]);
  FColumnResized := False;
  if (Value <> FSelectedColumn) then
  begin
    if (Value = 0) then
    begin
      gdProperties.Rows := 0;
      laColumn.Caption := 'No Column Selected';
      FSelectedColumn := 0;
      hcAlign.Hide;
      edHeading.Hide;
      pnSelected.Hide;
      lbColumns.ItemIndex := -1;
    end
    else
    begin
      FSelectedColumn := Value;
      theDisplayCol := propertyGrid.DisplayColnr[FSelectedColumn];
      laColumn.Caption := propertyGrid.Col[FSelectedColumn].FieldName;

      if (IsReady) then
      begin
        lbColumns.ItemIndex := -1;
        if (lbColumns.ItemIndex <> theDisplayCol - 1) then
        begin
          lbColumns.ItemIndex := theDisplayCol - 1;
          for i := 0 to lbColumns.Items.Count - 1 do
             lbColumns.Selected[i] := False;
          lbColumns.Selected[lbColumns.ItemIndex] := True;
        end;
        currentControl := Self.ActiveControl;
        if (currentControl = Nil) then
           currentControl := lbColumns;
        propertyGrid.SetFocus;
        propertyGrid.CurrentDataCol := FSelectedColumn;
        propertyGrid.PutCellInView(FSelectedColumn, 0);
        if (propertyGrid.Rows >= 1) then
           propertyGrid.CurrentCell.MoveTo(Value, 1)
        else
           propertyGrid.SelectCols(theDisplayCol, theDisplayCol, True);
        currentControl.SetFocus;

        DisplayColumnButtons;
        AdjustPropertyPanel;
        if (SelectedDbCol.ButtonType = btCombo) then
           RefreshComboDisplay;

        RefreshColProperties;
        currentControl.SetFocus;
        edHeading.SetFocus;
        propertyGrid.Invalidate;
      end;
    end;
  end;
end;

procedure TdgEditTgColumns.AdjustPropertyPanel;
begin
  if (SelectedColumn = 0) then exit;
  
  case propertyGrid.Col[SelectedColumn].ButtonType of
    btCombo              : begin
                             gbComboProperties.Show;
                             gdProperties.Height := pnProperties.Height - gbComboProperties.Height - 45;
                           end;
    btVertSpin,
    btHorzSpin           : begin
                             gbComboProperties.Hide;
                             gdProperties.Height := pnProperties.Height - 50;
                           end;
    btDateTimeDropDown,
    btDateTimePopup      : begin
                             gbComboProperties.Hide;
                             gdProperties.Height := pnProperties.Height - 50;
                           end;
  else
    gbComboProperties.Hide;
    gdProperties.Height := pnProperties.Height - 50;
  end;
  gbComboProperties.Top := gdProperties.Top + gdProperties.Height + 10;
end;

procedure TdgEditTgColumns.DisplayColumnButtons;
var j, newLeft : Integer;
begin
  try
    edHeading.Hide;
    j := propertyGrid.CalcColOffset(propertyGrid.DisplayColnr[SelectedColumn]);
    newLeft := propertyGrid.Left + j + 2;
    pnSelected.Left := newLeft;
    if (pnSelected.Left + propertyGrid.Col[SelectedColumn].Width - 2 > Self.Width) then
       pnSelected.Width := Self.Width - pnSelected.Left - 3
    else
       pnSelected.Width := propertyGrid.Col[SelectedColumn].Width - 2;
    if ((newLeft + hcAlign.Width) > Self.Width) then
       newLeft := Self.Width - hcAlign.Width - 10;

    hcAlign.Left := newLeft;

    edHeading.Width := Min(300, propertyGrid.Col[SelectedColumn].Width);
    if ((hcAlign.Left + hcAlign.Width + 5 + edHeading.Width) > (Self.Width - 20)) then
       edHeading.Left := hcAlign.Left - edHeading.Width - 5
    else
       edHeading.Left := hcAlign.Left + hcAlign.Width + 5;

    SetHeading(propertyGrid.Col[SelectedColumn].Heading)
  finally
    hcAlign.Show;
    edHeading.Show;
    pnSelected.Show;
    edHeading.SelectAll;
  end;
end;

procedure TdgEditTgColumns.ReselectColumn;
var i : Integer;
begin
  for i := 1 to propertyGrid.Cols do
    if (propertyGrid.Col[i].DisplayCol = lbColumns.ItemIndex + 1) then
    begin
      SelectedColumn := propertyGrid.Col[i].DataCol;
      break;
    end;
end;

procedure TdgEditTgColumns.lbFieldsDblClick(Sender: TObject);
begin
  btAddClick(btAdd);
end;

procedure TdgEditTgColumns.lbColumnsDblClick(Sender: TObject);
begin
  btRemoveClick(btRemove);
end;

procedure TdgEditTgColumns.FormCreate(Sender: TObject);
begin
  IsReady := False;
  FEditingProperty := '';
  FlsDataSources  := TStringList.Create;
  lsDateTimeDefs := TStringList.Create;
  EnumList := TStringList.Create;
end;

procedure TdgEditTgColumns.FormDestroy(Sender: TObject);
begin
  lsDataSources.Free;
  lsDateTimeDefs.Free;
  EnumList.Free;
  dgEditTgColumns := Nil;
end;

procedure TdgEditTgColumns.FormShow(Sender: TObject);
begin
  IsReady := True;
  if (lbColumns.Items.Count > 0) then
  begin
    lbColumns.ItemIndex := 0;
    SelectedColumn := lbColumns.ItemIndex + 1;
    lbColumns.Selected[0] := True;
  end;
  ResetWindowState;
end;

procedure TdgEditTgColumns.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  IsReady := False;

  if not FBinding then
     SaveWindowState;  
end;

function TdgEditTgColumns.GetColPropertyValueText(PropertyName: String; forRow : Integer) : String;
begin
  Result := VarToStr(GetPropValue(SelectedDbCol, PropertyName, False));
  if (AnsiCompareText(PropertyName, 'DateTimeDef') = 0) then
  begin
    Result := '';
    if SelectedDbCol.DateTimeDef <> Nil then
       Result := SelectedDbCol.DateTimeDef.Name;
    gdProperties.CellButtonType[2, forRow] := btCombo;
  end
  else
  case GetPropertyType(SelectedDbCol, PropertyName) of
    ptSpin    : gdProperties.CellButtonType[2, forRow] := btVertSpin;
    ptCombo   :
        begin
          gdProperties.CellButtonType[2, forRow] := btCombo;
          if (PropertyName = 'HeadingImage') then
             Result := SelectedDbCol.HeadingImage
          else
             Result := GetEnumProp(SelectedDbCol, PropertyName);
        end;
    ptBoolean :
        begin
          Result := GetEnumProp(SelectedDbCol, PropertyName);
          gdProperties.CellButtonType[2, forRow] := btCombo;
        end;
    ptButton  :
        begin
          gdProperties.CellButtonType[2, forRow] := btNormal;
          Result := '(' + PropertyName + ')';
        end;
    ptColor   :
        begin
          if (PropertyName = 'HeadingColor') and
             (SelectedDbCol <> Nil) and
             (SelectedDbCol.Tag > 0) then
          begin
            if not ColorToIdent(SelectedDbCol.Tag, Result) then
               Result := ColorToString(SelectedDbCol.Tag)
          end
          else
          begin
            if not ColorToIdent(StrToInt(Result), Result) then
               Result := ColorToString(StrToInt(Result));
          end;
          gdProperties.CellButtonType[2, forRow] := btCombo;
        end;
    ptEdit :
        begin
          if PropertyName = 'SpinOptions' then
          begin
            Result := SpinOptionToText(SelectedDbCol.SpinOptions);
            gdProperties.CellButtonType[2, forRow] := btNormal;
          end;
        end;
  end;
end;

function TdgEditTgColumns.SelectedDbCol : TosDbCol;
begin
  Result := Nil;
  if (SelectedColumn > 0) then
  begin
    //CustomColumnAssign(TtsGrid(propertyDbGrid), TtsGrid(propertyGrid));
    Result := propertyGrid.Col[SelectedColumn];
  end;
end;

procedure TdgEditTgColumns.RefreshColProperties;
var
  PropertyIndex,
  PropertyCount, iIndex, iRow : Integer;
  PropList : TPropList;
  sName, sText : String;
begin
  if SelectedColumn <= 0 then exit;

  FColPropertiesRefresh := True;
  gdProperties.BeginUpdate;
  try
    PropertyCount := GetPropList(SelectedDbCol.ClassInfo, tkProperties, @PropList);
    if chShowAll.Checked then
       gdProperties.Rows := PropertyCount
    else
    begin
       gdProperties.Rows := 11;
       gdProperties.CellControlType[2, 4] := ctText;
       gdProperties.CellButtonType[2, 4] := btNone;
    end;
    iRow := 1;
    for PropertyIndex := 0 to PropertyCount -1 do
    begin
      sName := PropList[PropertyIndex].Name;
      if chShowAll.Checked or
         ((AnsiCompareText(sName, 'Heading') = 0) or
          (AnsiCompareText(sName, 'FieldName') = 0) or
          (AnsiCompareText(sName, 'Width') = 0) or
          (AnsiCompareText(sName, 'HorzAlignment') = 0) or
          (AnsiCompareText(sName, 'ControlType') = 0) or
          (AnsiCompareText(sName, 'DisplayCol') = 0) or
          (AnsiCompareText(sName, 'DisplayFormat') = 0) or
          (AnsiCompareText(sName, 'GroupSortOp') = 0) or
          (AnsiCompareText(sName, 'GroupSummaryOp') = 0) or
          (AnsiCompareText(sName, 'GroupDateTimeSegment') = 0) or
          (AnsiCompareText(sName, 'ButtonType') = 0)) then
      begin
        sText := GetColPropertyValueText(sName, iRow);
        gdProperties.Cell[1, iRow] := sName;
        if (sName = 'Visible') and
           (SelectedDbCol.Tag = 1) then
          gdProperties.Cell[2, iRow] := 'False'
        else
          gdProperties.Cell[2, iRow] := sText;
        Inc(iRow);
      end;
    end;
  finally
    gdProperties.EndUpdate;
    FColPropertiesRefresh := False;
  end;

  for iIndex := 1 to gdProperties.Rows do
    if (gdProperties.Cell[1, iIndex] = FEditingProperty) then
    begin
      gdProperties.SetFocus;
      gdProperties.PutCellInView(1, iIndex);
      gdProperties.CurrentDataRow := iIndex;
      break;
    end;
end;

procedure TdgEditTgColumns.chShowAllClick(Sender: TObject);
begin
  RefreshColProperties;
end;

procedure TdgEditTgColumns.gdPropertiesComboInit(Sender: TObject;
  Combo: TtsComboGrid; DataCol, DataRow: Integer);
begin
  Combo.Width := gdProperties.Col[2].Width - 15;
  Combo.Color := clWindow;
  Combo.DropDownStyle := ddDropDownList;
  Combo.GridLines := glNone;
  Combo.AutoSearch := asTop;
end;

procedure TdgEditTgColumns.gdPropertiesComboDropDown(Sender: TObject;
  Combo: TtsComboGrid; DataCol, DataRow: Integer);
begin
  if (AnsiSameText(gdProperties.Cell[1, DataRow], 'DateTimeDef')) then
  begin
{$IFNDEF rtTest}
    Combo.Rows := osAdvGridEditors.lsDateTimeDefs.Count;
{$ENDIF}
    Combo.Tag := 5;
  end
  else if (AnsiSameText(gdProperties.Cell[1, DataRow], 'HeadingImage')) then
  begin
    Combo.Rows := 0;
    if (Self.FSourceGrid.ImageList <> Nil) then
    begin
      Combo.Tag := 6;
      Combo.Rows := FSourceGrid.ImageList.ImageCollection.Count;
      Combo.DropDownRows := 8;
      if (SelectedDbCol.HeadingImage <> '') then
         Combo.CurrentDataRow := FSourceGrid.ImageList.NameIndex(SelectedDbCol.HeadingImage) + 1;
    end;
  end
  else
    PropertyComboDropDown(Combo, gdProperties, SelectedDbCol, DataRow, EnumList);
end;

procedure TdgEditTgColumns.gdPropertiesComboCellLoaded(Sender: TObject;
  Combo: TtsComboGrid; DataCol, DataRow: Integer; var Value: Variant);
begin
  if Combo.Tag = 1 then // Boolean
  begin
    if DataRow = 1 then
       Value := 'True'
    else
       Value := 'False';
  end
  else if Combo.Tag = 2 then // Colors
    Value := 'cl' + cbColors.Items.Strings[DataRow-1]
  else if (Combo.Tag = 3) and
          (DataRow <= EnumList.Count) then // Enumerated list
    Value := EnumList.Strings[DataRow-1]
  else if (Combo.Tag = 5) then
  begin
{$IFNDEF rtTest}
    if (DataRow <= osAdvGridEditors.lsDateTimeDefs.Count) then
       Value := osAdvGridEditors.lsDateTimeDefs.Strings[DataRow-1]
    else
{$ENDIF}
       Value := '';
  end
  else if (Combo.Tag = 6) and
          (FSourceGrid.ImageList <> Nil) then
    Value := FSourceGrid.ImageList.Image[DataRow-1].Name;
end;

procedure TdgEditTgColumns.gdPropertiesComboGetValue(Sender: TObject;
  Combo: TtsComboGrid; GridDataCol, GridDataRow, ComboDataRow: Integer;
  var Value: Variant);
begin
  Value := Combo.Cell[1, ComboDataRow];
  gdProperties.Cell[GridDataCol, GridDataRow] := Value;
  BindColProperty(GridDataRow);
  AdjustPropertyPanel;
  if (Self.SelectedDbCol.ButtonType = btCombo) then
     RefreshComboDisplay;
  //if (AnsiSameText(gdProperties.Cell[1, GridDataRow], 'HeadingColor')) then
  //   propertyGrid.Col[FSelectedColumn].Tag := Integer(SelectedDbCol.HeadingColor);
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.gdPropertiesDblClick(Sender: TObject);
begin
  if LowerCase(gdProperties.Cell[gdProperties.CurrentDataCol, gdProperties.CurrentDataRow]) = 'true' then
  begin
     gdProperties.Cell[gdProperties.CurrentDataCol, gdProperties.CurrentDataRow] := 'False';
     BindColProperty(gdProperties.CurrentDataRow);
     ColumnsChanged := True;
  end
  else if LowerCase(gdProperties.Cell[gdProperties.CurrentDataCol, gdProperties.CurrentDataRow]) = 'false' then
  begin
     gdProperties.Cell[gdProperties.CurrentDataCol, gdProperties.CurrentDataRow] := 'True';
     BindColProperty(gdProperties.CurrentDataRow);
     ColumnsChanged := True;
  end
  else if LowerCase(gdProperties.Cell[1, gdProperties.CurrentDataRow]) = 'color' then
  begin
     dgColor.Color := SelectedDbCol.Color;
     if (dgColor.Execute) then
     begin
       SelectedDbCol.Color := dgColor.Color;
       ColumnsChanged := True;
       gdProperties.Cell[2, gdProperties.CurrentDataRow] := ColorToString(dgColor.Color);
     end;
  end
  else if LowerCase(gdProperties.Cell[1, gdProperties.CurrentDataRow]) = 'headingcolor' then
  begin
     dgColor.Color := SelectedDbCol.HeadingColor;
     if (dgColor.Execute) then
     begin
       SelectedDbCol.HeadingColor := dgColor.Color;
       //SelectedDbCol.Tag := Integer(dgColor.Color);
       ColumnsChanged := True;
       gdProperties.Cell[2, gdProperties.CurrentDataRow] := ColorToString(dgColor.Color);
     end;
  end;
end;

procedure TdgEditTgColumns.BindColProperty(DataRow : Integer);
var iIndex : Integer;
begin
  if GetPropertyType(SelectedDbCol, gdProperties.Cell[1, DataRow]) = ptColor then
     SetPropValue(SelectedDbCol, gdProperties.Cell[1, DataRow], StringToColor(FullColorName(gdProperties.Cell[2, DataRow])))
  else
  begin
    if (AnsiCompareText(gdProperties.Cell[1, DataRow], 'ReadOnly') = 0) then
    begin
      FSelecting := True;
      try
        SetPropValue(SelectedDbCol, gdProperties.Cell[1, DataRow], gdProperties.Cell[2, DataRow]);
      finally
        FSelecting := False;
      end;
    end
    else if (AnsiCompareText(gdProperties.Cell[1, DataRow], 'DateTimeDef') = 0) then
    begin
{$IFNDEF rtTest}
      iIndex := osAdvGridEditors.lsDateTimeDefs.IndexOf(gdProperties.Cell[2, DataRow]);
      if (gdProperties.Cell[2, DataRow] = '') or
         (iIndex = -1) then
         SelectedDbCol.DateTimeDef := Nil
      else
         SelectedDbCol.DateTimeDef := TtsDateTimeDefComponent(osAdvGridEditors.lsDateTimeDefs.Objects[iIndex]);
{$ENDIF}
    end
    else
       SetPropValue(SelectedDbCol, gdProperties.Cell[1, DataRow], gdProperties.Cell[2, DataRow]);
  end;
end;

procedure TdgEditTgColumns.gdPropertiesEndCellEdit(Sender: TObject;
  DataCol, DataRow: Integer; var Cancel: Boolean);

  function WidthRow : Integer;
  var i : Integer;
  begin
    Result := 1;
    for i := 1 to gdProperties.Rows do
      if (gdProperties.Cell[1, i] = 'Width') then
      begin
        Result := i;
        break;
      end;
  end;
begin
  BindColProperty(DataRow);
  if (gdProperties.Cell[1, DataRow] = 'FieldName') then
     lbColumns.Items.Strings[lbColumns.ItemIndex] := gdProperties.Cell[2, DataRow]
  else if (gdProperties.Cell[1, DataRow] = 'Heading') then
     SetHeading(SelectedDbCol.Heading)
  //else if (AnsiSameText(gdProperties.Cell[1, DataRow], 'HeadingColor')) then
  //   propertyGrid.Col[FSelectedColumn].Tag := Integer(SelectedDbCol.HeadingColor)
  else if (gdProperties.Cell[1, DataRow] = 'MaxWidth') or
          (gdProperties.Cell[1, DataRow] = 'MinWidth') then
  begin
    gdProperties.Cell[2, WidthRow] := IntToStr(SelectedDbCol.Width);
  end;
  AdjustPropertyPanel;
  if (Self.SelectedDbCol.ButtonType = btCombo) then
     RefreshComboDisplay;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.gdPropertiesCellChanged(Sender: TObject; OldCol,
  NewCol, OldRow, NewRow: Integer);
begin
  if not FColPropertiesRefresh then
     FEditingProperty := gdProperties.Cell[1, NewRow];
end;

procedure TdgEditTgColumns.gdPropertiesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = vk_Right) and
     (ssCtrl in Shift) and
     (lbColumns.ItemIndex < lbColumns.Items.Count - 1) then
  begin
    gdProperties.EndEdit(False);
    lbColumns.ItemIndex := lbColumns.ItemIndex + 1;
    ReselectColumn;
  end
  else if (Key = vk_Left) and
          (ssCtrl in Shift) and
          (lbColumns.ItemIndex >= 1) then
  begin
    gdProperties.EndEdit(False);
    lbColumns.ItemIndex := lbColumns.ItemIndex - 1;
    ReselectColumn;
  end
end;

procedure TdgEditTgColumns.lbColumnsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Delete) then
  begin
    RemoveColumn(lbColumns.ItemIndex);
    ResetSelection;
    Key := 0;
  end;
end;

procedure TdgEditTgColumns.miAddColumnClick(Sender: TObject);
begin
  lbColumns.ItemIndex := lbColumns.Items.Add('<New Column>');
  propertyGrid.Cols := propertyGrid.Cols + 1;
  propertyGrid.Col[propertyGrid.Cols].FieldName := '';
  propertyGrid.Col[propertyGrid.Cols].Heading := '<New Column>';
  SelectedColumn := propertyGrid.Cols;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.miRemoveColumnClick(Sender: TObject);
begin
  RemoveColumn(Self.SelectedColumn-1);
  ResetSelection;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.miHideColumnClick(Sender: TObject);
var i : Integer;
    removeCol : TosDbCol;
begin
  //if (SelectedColumn > 0) then
  //   SelectedDbCol.HeadingColor := TColor(SelectedDbCol.Tag);
  i := lbColumns.ItemIndex;
  removeCol := Self.SelectedDbCol;
  if (chHidden.Checked) then // Put back to visible
  begin
    VisibleGrid.Cols := VisibleGrid.Cols + 1;
    VisibleGrid.Col[VisibleGrid.Cols].Assign(removeCol);
  end
  else
  begin
    InvisibleGrid.Cols := InvisibleGrid.Cols + 1;
    InvisibleGrid.Col[InvisibleGrid.Cols].Assign(removeCol);
  end;
  propertyGrid.DeleteCols(removeCol.DataCol, removeCol.DataCol);
  lbColumns.Items.Delete(lbColumns.ItemIndex);
  if (i > lbColumns.Items.Count - 1) then
     lbColumns.ItemIndex := lbColumns.Items.Count - 1
  else
     lbColumns.ItemIndex := i;
  if (not FUpdating) then
  begin
    FSelectedColumn := 0;
    ReselectColumn;
  end;
  for i := 0 to lbColumns.Items.Count - 1 do
     lbColumns.Selected[i] := False;
  lbColumns.Selected[lbColumns.ItemIndex] := True;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.Combo1Click(Sender: TObject);
begin
  SelectedDbCol.ControlType := ctText;
  SelectedDbCol.ButtonType := btCombo;
  RefreshColProperties;
  AdjustPropertyPanel;
  RefreshComboDisplay;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.CheckBox1Click(Sender: TObject);
begin
  SelectedDbCol.ControlType := ctCheck;
  SelectedDbCol.ButtonType := btNone;
  AdjustPropertyPanel;
  RefreshColProperties;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.Image1Click(Sender: TObject);
begin
  SelectedDbCol.ControlType := ctPicture;
  SelectedDbCol.ButtonType := btNone;
  AdjustPropertyPanel;
  RefreshColProperties;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.DateTimePopup1Click(Sender: TObject);
begin
  SelectedDbCol.ControlType := ctText;
  SelectedDbCol.ButtonType := btDateTimePopup;
  AdjustPropertyPanel;
  RefreshColProperties;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.DateTimeDropdown1Click(Sender: TObject);
begin
  SelectedDbCol.ControlType := ctText;
  SelectedDbCol.ButtonType := btDateTimeDropdown;
  AdjustPropertyPanel;
  RefreshColProperties;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.SpinEdit1Click(Sender: TObject);
begin
  SelectedDbCol.ControlType := ctText;
  SelectedDbCol.ButtonType := btVertSpin;
  AdjustPropertyPanel;
  RefreshColProperties;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.miTextControlClick(Sender: TObject);
begin
  SelectedDbCol.ControlType := ctText;
  SelectedDbCol.ButtonType := btNone;
  AdjustPropertyPanel;
  RefreshColProperties;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.edHeadingChange(Sender: TObject);
begin
  if not FChangingHeading then
  begin
    propertyGrid.Col[SelectedColumn].Heading := edHeading.Text;
    if (propertyGrid.Col[SelectedColumn].FieldName = '') then
       lbColumns.Items.Strings[SelectedColumn-1] := edHeading.Text;
    ColumnsChanged := True;
  end;
end;

procedure TdgEditTgColumns.edHeadingKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = vk_Return) and (Shift = [])) or
     ((Key = vk_Right) and (ssCtrl in Shift)) or
     (Key = vk_Down) then
  begin
    if (propertyGrid.DisplayColnr[SelectedColumn] = propertyGrid.Cols) then
       SelectedColumn := propertyGrid.DataColnr[1]
    else
       SelectedColumn := propertyGrid.DataColnr[propertyGrid.DisplayColnr[SelectedColumn]+1];
    Key := 0;
  end
  else if ((Key = vk_Return) and (ssShift in Shift)) or
          ((Key = vk_Left) and (ssCtrl in Shift)) or
          (Key = vk_Up) then
  begin
    if (propertyGrid.DisplayColnr[SelectedColumn] = 1) then
       SelectedColumn := propertyGrid.DataColnr[propertyGrid.Cols]
    else
       SelectedColumn := propertyGrid.DataColnr[propertyGrid.DisplayColnr[SelectedColumn]-1];
    Key := 0;
  end;
end;

procedure TdgEditTgColumns.FormResize(Sender: TObject);
begin
  propertyGrid.Width := Self.Width - 25;
  // default panel widths are 160, 180, 217 for width 630
  // or percentges of 26%, 29 and 35% respectively...
  // Top of panels is 96 width with heigth 217 form height of 388 (percentage = 56%)
  pnFields.Width  := Round(Self.Width*0.26);
  pnColumns.Width := Round(Self.Width*0.29);
  pnProperties.Width := Self.Width - pnFields.Width - pnColumns.Width - 70;

  pnFields.Height := pnMid.Height - 4;
  pnColumns.Height := pnFields.Height;
  pnProperties.Height := pnFields.Height;

  btAdd.Left := pnFields.Left + pnFields.Width + 8;
  btRemove.Left := btAdd.Left;

  pnColumns.Left := btAdd.Left + 28;
  pnProperties.Left := pnColumns.Left + pnColumns.Width + 8;
  udPosition.Left := pnColumns.Width - 22;
  chShowAll.Left  := pnProperties.Width - chShowAll.Width - 3;

  //btDone.Top := Self.Height - 68;
  //btCancel.Top := btDone.Top;
  btDone.Left := Trunc(Self.Width/2) - btDone.Width - 3;
  btCancel.Left := btDone.Left + btDone.Width + 3;
end;

procedure TdgEditTgColumns.pnFieldsResize(Sender: TObject);
begin
  lbFields.Width := pnFields.Width - 20;
  lbFields.Height := pnFields.Height - 35;
end;

procedure TdgEditTgColumns.pnColumnsResize(Sender: TObject);
begin
  lbColumns.Width := pnColumns.Width - 40;
  lbColumns.Height := pnColumns.Height - 35;
end;

procedure TdgEditTgColumns.pnPropertiesResize(Sender: TObject);
begin
  gdProperties.Width := pnProperties.Width - 20;
  gdProperties.Height := pnProperties.Height - 35;
  AdjustPropertyPanel;
  gbComboProperties.Width := pnProperties.Width - 20;
  cbComboDataSource.Width := gbComboProperties.Width - 80;
  cbValueCol.Width   := cbComboDataSource.Width;
  cbDisplayCol.Width := cbComboDataSource.Width;
  btAdvancedCombo.Width := gbComboProperties.Width - 15;
  gbComboProperties.Top := gdProperties.Top + gdProperties.Height + 10;
end;

procedure TdgEditTgColumns.gdPropertiesResize(Sender: TObject);
begin
  if (chShowAll.Checked) then   
  begin
    gdProperties.Col[1].Width := Trunc(gdProperties.Width/2)-25;
    gdProperties.Col[2].Width := gdProperties.Width - gdProperties.Col[1].Width-25;
  end
  else
  begin
    gdProperties.Col[1].Width := Trunc(gdProperties.Width/2)-2;
    gdProperties.Col[2].Width := gdProperties.Width - gdProperties.Col[1].Width - 2;
  end;
  //if gdProperties.CurrentDataRow > 0 then
  //   gdProperties.PutCellInView(1, gdProperties.CurrentDataRow);
end;

procedure TdgEditTgColumns.propertyGridColMoved(Sender: TObject;
  ToDisplayCol, Count: Integer; ByUser: Boolean);
begin
  if (FBinding) then exit;

  lbColumns.Items.Move(lbColumns.ItemIndex, ToDisplayCol-1);
  lbColumns.ItemIndex := ToDisplayCol-1;
  lbColumns.Selected[ToDisplayCol-1] := True;
  DisplayColumnButtons;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.pmOptionsPopup(Sender: TObject);
begin
  if (SelectedColumn <= 0) then
     miComboProperties.Enabled := False
  else
     miComboProperties.Enabled := propertyGrid.Col[SelectedColumn].ButtonType = btCombo;
  if (chHidden.Checked) then
     miHideColumn.Caption := 'Unhide Columm'
  else
     miHideColumn.Caption := 'Hide Columm';
end;

procedure TdgEditTgColumns.miComboPropertiesClick(Sender: TObject);
begin
  dgOsCombo := TdgOsCombo.Create(Self);
  try
    dgOsCombo.Execute(Self.SelectedDbCol, propertyGrid.ComboQuery);
    if (Self.SelectedDbCol.Combo <> Nil) and
       (Self.SelectedDbCol.ComboSQL.Count > 0) and
       (Self.SelectedDbCol.Combo.ComboGrid.Rows > 0) then
       Self.SelectedDbCol.Combo.ComboGrid.Rows := 0;
    Self.RefreshComboDisplay;
  finally
    dgOsCombo.Free;
  end;
end;

procedure TdgEditTgColumns.btAdvancedComboClick(Sender: TObject);
begin
  miComboPropertiesClick(Self);
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.RefreshComboDisplay;
var i : Integer;
begin
  cbComboDataSource.Items.Clear;
  //ShowMessage('DS Count = ' + IntToStr(osAdvGridEditors.lsDataSources.Count));
{$IFNDEF rtTest}
  if (Assigned(osAdvGridEditors.lsDataSources)) then
  begin
     for i := 0 to osAdvGridEditors.lsDataSources.Count - 1 do
        cbComboDataSource.Items.AddObject(osAdvGridEditors.lsDataSources.Strings[i], osAdvGridEditors.lsDataSources.Objects[i])
  end
  else
{$ENDIF}
  if (lsDataSources.Count > 0) then
     for i := 0 to lsDataSources.Count - 1 do
        cbComboDataSource.Items.AddObject(lsDataSources.Strings[i], lsDataSources.Objects[i]);

  if (SelectedDbCol.ComboDatasource <> Nil) then
  begin
    cbComboDataSource.Items.IndexOfObject(SelectedDbCol.ComboDatasource);
    if (cbComboDataSource.ItemIndex = -1) then
       cbComboDataSource.ItemIndex := cbComboDataSource.Items.AddObject(SelectedDbCol.ComboDatasource.Name, SelectedDbCol.ComboDatasource);
  end;
  RefreshComboFields;  
end;

procedure TdgEditTgColumns.RefreshComboFields;
var i, iCnt : Integer;
    fieldName, displayColumns : String;
begin
  cbValueCol.Items.Clear;
  cbDisplayCol.Items.Clear;
  FComboDisplayColName := '';
  if (SelectedDbCol.Combo <> Nil) and
     (SelectedDbCol.Combo.Datasource <> Nil) and
     (SelectedDbCol.Combo.Datasource.DataSet <> Nil) then
  begin
    for i := 0 to SelectedDbCol.Combo.Datasource.DataSet.Fields.Count - 1 do
    begin
      cbValueCol.Items.Add(SelectedDbCol.Combo.Datasource.DataSet.Fields.Fields[i].FieldName);
      cbDisplayCol.Items.Add(SelectedDbCol.Combo.Datasource.DataSet.Fields.Fields[i].FieldName);
    end;
    if (SelectedDbCol.Combo.ValueCol > 0) and
       (SelectedDbCol.Combo.ValueCol <= SelectedDbCol.Combo.ComboGrid.Cols) then
    begin
      fieldName := SelectedDbCol.Combo.ComboGrid.Col[SelectedDbCol.Combo.ValueCol].FieldName;
      if (fieldName = '') and
         (SelectedDbCol.Combo.Datasource.DataSet.Fields.Count >= SelectedDbCol.Combo.ValueCol) then
         fieldName := SelectedDbCol.Combo.Datasource.DataSet.Fields.Fields[SelectedDbCol.Combo.ValueCol-1].FieldName;
      cbValueCol.ItemIndex := cbValueCol.Items.IndexOf(fieldName);
    end;
    iCnt := 0;
    displayColumns := '';
    for i := 1 to SelectedDbCol.Combo.ComboGrid.Cols do
      if (i <> SelectedDbCol.Combo.ValueCol) and
         (SelectedDbCol.Combo.ComboGrid.Col[i].Visible) then
      begin
        fieldName := SelectedDbCol.Combo.ComboGrid.Col[i].FieldName;
        if (iCnt = 0) then
        begin
          FComboDisplayColName := fieldName;
          cbDisplayCol.ItemIndex := cbDisplayCol.Items.IndexOf(fieldName);
        end;
        displayColumns := displayColumns + fieldName + ',';
        Inc(iCnt);
      end;
    cbDisplayCol.Visible := (iCnt <= 1);
    if (displayColumns <> '') then
       System.Delete(displayColumns, Length(displayColumns), 1);
    txDisplayCol.Caption := displayColumns;
  end;
end;

procedure TdgEditTgColumns.cbComboDataSourceClick(Sender: TObject);
var sCurrDS : String;
    theDS : TDataSource;
begin
  sCurrDS := '';
  if (SelectedDbCol.ComboDatasource <> Nil) then
     sCurrDS := SelectedDbCol.ComboDatasource.Name;

  if (cbComboDataSource.ItemIndex >= 0) then
  begin
    if (SelectedDbCol.Combo = Nil) then
       SelectedDbCol.AssignCombo;
    theDS := TDataSource(cbComboDataSource.Items.Objects[cbComboDataSource.ItemIndex]);
    SelectedDbCol.Combo.DataSource := theDS;
    if (theDS.DataSet.FieldCount >= 1) then
    begin
      SelectedDbCol.Combo.ComboGrid.FieldState := fsCustomized;
      SelectedDbCol.Combo.ComboGrid.Col[1].FieldName := theDS.DataSet.Fields[0].FieldName;
      SelectedDbCol.Combo.ComboGrid.Col[1].Visible := False;
      if (theDS.DataSet.FieldCount >= 2) then
      begin
        SelectedDbCol.Combo.ComboGrid.Cols := 2;
        SelectedDbCol.Combo.ComboGrid.Col[2].FieldName := theDS.DataSet.Fields[1].FieldName;
      end;
    end;
  end
  else
  begin
    SelectedDbCol.Combo.DataSource := Nil;
    exit;
  end;
  
  SelectedDbCol.Combo.AutoSearch := asCenter;
  SelectedDbCol.Combo.RefreshComboData := True;
  RefreshComboFields;
  if (SelectedDbCol.ComboDatasource <> Nil) and
     (sCurrDS <> SelectedDbCol.ComboDatasource.Name) and
     (cbDisplayCol.ItemIndex = -1) and
     (cbDisplayCol.Items.Count >= 2) then
  begin
    cbDisplayCol.ItemIndex := 1;
    cbDisplayColClick(cbDisplayCol);
  end;
  ColumnsChanged := True;
end;

function TdgEditTgColumns.ComboColCount : Integer;
begin
  Result := 0;
  if (cbValueCol.ItemIndex >= 0) then
     Inc(Result);
  if (cbDisplayCol.ItemIndex >= 0) and
     (cbDisplayCol.ItemIndex <> cbValueCol.ItemIndex) then
     Inc(Result);   
end;

procedure TdgEditTgColumns.cbValueColClick(Sender: TObject);
var dataCol : Integer;
begin
  if (SelectedDbCol.Combo = Nil) then exit;

  if (cbValueCol.ItemIndex >= 0) then
  begin
     SelectedDbCol.Combo.ComboGrid.Cols := ComboColCount;
     if (SelectedDbCol.Combo.ComboGrid.LocateColumn(cbValueCol.Text) = Nil) then
     begin
        for dataCol := 1 to SelectedDbCol.Combo.ComboGrid.Cols do
           if (SelectedDbCol.Combo.ComboGrid.Col[dataCol].FieldName = '') then
           begin
             SelectedDbCol.Combo.ComboGrid.Col[dataCol].FieldName := cbValueCol.Text;
             break;
           end;
     end
     else
       dataCol := SelectedDbCol.Combo.ComboGrid.Col[cbValueCol.Text].DataCol;
     SelectedDbCol.Combo.ValueCol := dataCol;
     SelectedDbCol.Combo.DropDownCols := 1;
  end
  else
  begin
    SelectedDbCol.Combo.ValueCol := 0;
    SelectedDbCol.Combo.DropDownCols := SelectedDbCol.Combo.DropDownCols - 1;
  end;
  AdjustHiddenComboColumn;
  SelectedDbCol.Combo.RefreshComboData := True;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.cbDisplayColClick(Sender: TObject);
var dataCol : Integer;
    bVisible : Boolean;
begin
  if (SelectedDbCol.Combo = Nil) then exit;

  if (cbDisplayCol.ItemIndex >= 0) then
  begin
     SelectedDbCol.Combo.ComboGrid.Cols := ComboColCount;
     if (SelectedDbCol.Combo.ComboGrid.LocateColumn(cbDisplayCol.Text) = Nil) then
     begin
        for dataCol := 1 to SelectedDbCol.Combo.ComboGrid.Cols do
           if (SelectedDbCol.Combo.ComboGrid.Col[dataCol].FieldName = '') then
           begin
             SelectedDbCol.Combo.ComboGrid.Col[dataCol].FieldName := cbDisplayCol.Text;
             break;
           end;
     end;
     SelectedDbCol.Combo.DropDownCols := 1;
     FComboDisplayColName := cbDisplayCol.Text;
  end
  else if (FComboDisplayColName <> '') then
  begin
    for dataCol := 1 to SelectedDbCol.Combo.ComboGrid.Cols do
    begin
      if (SelectedDbCol.Combo.ComboGrid.Col[dataCol].FieldName = FComboDisplayColName) then
      begin
        bVisible := SelectedDbCol.Combo.ComboGrid.Col[dataCol].Visible;
        SelectedDbCol.Combo.ComboGrid.DeleteCols(dataCol, dataCol);
        if (bVisible) then
           SelectedDbCol.Combo.DropDownCols := SelectedDbCol.Combo.DropDownCols - 1;
      end;
    end;
  end;
  AdjustHiddenComboColumn;
  SelectedDbCol.Combo.RefreshComboData := True;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.AdjustHiddenComboColumn;
begin
  // if both value Col and display column are selected then make the value column invisible
  if (Self.ComboColCount > 1) and
     (SelectedDbCol.Combo.ValueCol > 0) then
     SelectedDbCol.Combo.ComboGrid.Col[SelectedDbCol.Combo.ValueCol].Visible := False
  else if (cbDisplayCol.ItemIndex = cbValueCol.ItemIndex) or
          (ComboColCount = 1) then
     SelectedDbCol.Combo.ComboGrid.Col[1].Visible := True;
end;

procedure TdgEditTgColumns.propertyGridSorting(Sender: TObject;
  DataCol: Integer; var Cancel: Boolean);
begin
  Cancel := True;
end;

procedure TdgEditTgColumns.propertyGridGrouping(Sender: TObject;
  DataCol: Integer; var Cancel: Boolean);
begin
  Cancel := True;
end;

procedure TdgEditTgColumns.SaveWindowState;
var Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(TG_REGENTRIES + 'Designer', True);
    //Reg.WriteInteger('EditorHeight', pgEditor.Height);
    Reg.WriteInteger('FormHeight', Self.Height);
    Reg.WriteInteger('FormWidth', Self.Width);
    Reg.WriteInteger('FormLeft', Self.Left);
    Reg.WriteInteger('FormTop', Self.Top);
    Reg.WriteInteger('BottomHeight', pnBottom.Height);
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

procedure TdgEditTgColumns.ResetWindowState;
var Reg: TRegistry;
    iVal : Integer;
begin
  FBinding := True;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.OpenKey(TG_REGENTRIES + 'Designer', False) then
       Reg.CreateKey(TG_REGENTRIES + 'Designer')
    else
    begin
      if Reg.ValueExists('FormHeight') then
      begin
        iVal   := Reg.ReadInteger('FormHeight');
        if iVal > 300 then
           Self.Height := iVal;
      end;
      if Reg.ValueExists('FormWidth') then
      begin
        iVal := Reg.ReadInteger('FormWidth');
        if iVal > 300 then
           Self.Width := iVal;
      end;
      if Reg.ValueExists('FormLeft') then
      begin
        iVal   := Reg.ReadInteger('FormLeft');
        if (iVal > 0) and (iVal < (Screen.Width - Self.Width)) then
           Self.Left := iVal;
      end;
      if Reg.ValueExists('FormTop') then
      begin
        iVal   := Reg.ReadInteger('FormTop');
        if (iVal > 0) and (iVal < (Screen.Height - Self.Height)) then
           Self.Top := iVal;
      end;
      if Reg.ValueExists('BottomHeight') then
      begin
        iVal   := Reg.ReadInteger('BottomHeight');
        if (iVal > 0) and (iVal < 1000) then
           pnBottom.Height := iVal;
      end;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
    FBinding := False;
  end;
end;

procedure TdgEditTgColumns.chHiddenClick(Sender: TObject);
begin
  if chHidden.Checked then
  begin
    CustomColumnDbAssign(VisibleGrid, propertyGrid);
    LoadHiddenColumns;
  end
  else
  begin
    CustomColumnDbAssign(InvisibleGrid, propertyGrid);
    LoadVisibleColumns;
  end;
end;

procedure TdgEditTgColumns.propertyGridColResized(Sender: TObject;
  RowColnr: Integer);
begin
  if IsReady then
  begin
    edHeading.Width := propertyGrid.Col[SelectedColumn].Width;
    ColumnsChanged := True;
    FColumnResized := True;
  end;
end;

function  TdgEditTgColumns.GetIsReady : Boolean;
begin
  Result := FIsReadyCount = 0;
end;

procedure TdgEditTgColumns.SetIsReady(Value : Boolean);
begin
  if (Value) then
     Inc(FIsReadyCount)
  else
     Dec(FIsReadyCount);
end;

procedure TdgEditTgColumns.SetColumnsChanged(Value : Boolean);
begin
  FColumnsChanged := Value;
  btDone.Enabled := Value;
end;

procedure TdgEditTgColumns.SetHeading(Value : String);
begin
  FChangingHeading := True;
  try
    edHeading.Text := Value;
  finally
    FChangingHeading := False;
  end;
end;

procedure TdgEditTgColumns.lbColumnsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  lbColumns.BeginDrag(False);
  FDragIndex := lbColumns.ItemAtPos(Point(X,Y), True);
end;

procedure TdgEditTgColumns.lbColumnsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbColumns);
end;

procedure TdgEditTgColumns.lbColumnsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var iIndex, i : Integer;
    theCol : TosDbCol;
begin
  if (Y < 0) then
     iIndex := 0
  else
  begin
    iIndex := lbColumns.ItemAtPos(Point(X,Y), True);
    if (iIndex < 0) then
       iIndex := lbColumns.Items.Count - 1;
  end;
  if (FDragIndex >= 0) and
     (iIndex >= 0) and
     (iIndex < lbColumns.Items.Count) and
     (FDragIndex <> iIndex) then
  begin
      theCol := propertyGrid.Col[lbColumns.Items.Strings[FDragIndex]];
      lbColumns.Items.Move(FDragIndex, iIndex);
      lbColumns.ItemIndex := iIndex;
      for i := 0 to lbColumns.Items.Count - 1 do
         lbColumns.Selected[i] := False;
      lbColumns.Selected[iIndex] := True;
      theCol.DisplayCol := iIndex + 1;
      propertyGrid.PutCellInView(theCol.DataCol, 0);
      //propertyGrid.Col[FSelectedColumn].HeadingColor := clLime;
      edHeading.SetFocus;
      FDragIndex := -1;
  end;
end;

procedure TdgEditTgColumns.lbColumnsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReselectColumn;
end;

procedure TdgEditTgColumns.lbColumnsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Down) or (Key = vk_Up) then
     ReselectColumn;
end;

procedure TdgEditTgColumns.EditFont;
begin
  if (AnsiCompareText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'Font') = 0) then
  begin
    if (SelectedDbCol.Font = Nil) then SelectedDbCol.ParentFont := False;
    dgFont.Font.Assign(SelectedDbCol.Font)
  end
  else if (AnsiCompareText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'HeadingFont') = 0) then
  begin
    if (SelectedDbCol.HeadingFont = Nil) then SelectedDbCol.HeadingParentFont := False;
    dgFont.Font.Assign(SelectedDbCol.HeadingFont);
  end
  else
     exit;
     
  if dgFont.Execute then
  begin
    if (AnsiCompareText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'Font') = 0) then
    begin
       SelectedDbCol.ParentFont := False;
       SelectedDbCol.Font.Assign(dgFont.Font);
       Self.ColumnsChanged := True;
    end
    else if (AnsiCompareText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'HeadingFont') = 0) then
    begin
       SelectedDbCol.HeadingParentFont := False;
       SelectedDbCol.HeadingFont.Assign(dgFont.Font);
       Self.ColumnsChanged := True;
    end;
  end;
end;

procedure TdgEditTgColumns.gdPropertiesButtonClick(Sender: TObject;
  DataCol, DataRow: Integer);
begin
  if (AnsiCompareText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'Font') = 0) or
     (AnsiCompareText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'HeadingFont') = 0) then
     EditFont
  else if (AnsiCompareText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'SpinOptions') = 0) then
  begin
    dgSpinOptions := TdgSpinOptions.Create(Self);
    try
      if (dgSpinOptions.EditOptions(SelectedDbCol.SpinOptions)) then
      begin
        SelectedDbCol.SpinOptions := dgSpinOptions.GetSpinOptions;
        gdProperties.Cell[2, gdProperties.CurrentDataRow] := SpinOptionToText(SelectedDbCol.SpinOptions);
        ColumnsChanged := True;
      end;
    finally
      FreeAndNil(dgSpinOptions);
    end;
  end;
end;

procedure TdgEditTgColumns.propertyGridMouseStatusChanged(Sender: TObject;
  OldStatus, NewStatus: TtsMouseStatus);
begin
  if (NewStatus = msColResize) then
     FColumnResized := True
  else if (FColumnResized) then
  begin
    FColumnResized := False;
    DisplayColumnButtons;
  end;
end;

procedure TdgEditTgColumns.CopyPropertiesfromTtsDbGrid1mClick(
  Sender: TObject);
var i : Integer;
    parentForm : TForm;
    theDbGrid : TtsDbGrid;
    sDesc : String;
begin
  dgGetDbGrid := TdgGetDbGrid.Create(Self);
  try
    parentForm := TForm(GetParentForm(Self.FSourceGrid));
    for i := 0 to parentForm.ComponentCount - 1 do
      if (parentForm.Components[i] is TtsDbGrid) then
      begin
        sDesc := parentForm.Components[i].Name;
        if (TtsDbGrid(parentForm.Components[i]).DataSource <> Nil) then
           sDesc := sDesc + ' (' + TtsDbGrid(parentForm.Components[i]).DataSource.Name + ')'
        else
           sDesc := sDesc + ' (No Datasource)';
        dgGetDbGrid.cbDbGrids.Items.AddObject(sDesc, parentForm.Components[i]);
      end;
    if (dgGetDbGrid.cbDbGrids.Items.Count = 1) then
    begin
      dgGetDbGrid.cbDbGrids.ItemIndex := 0;
      dgGetDbGrid.btOk.Enabled := True;
    end;
    if (dgGetDbGrid.ShowModal = mrOk) then
    begin
      theDbGrid := TtsDbGrid(dgGetDbGrid.cbDbGrids.Items.Objects[dgGetDbGrid.cbDbGrids.ItemIndex]);
      VisibleGrid.Cols := 0;
      VisibleGrid.Cols := theDbGrid.Cols;
      FBinding := True;
      try
        VisibleGrid.AssignColsFromDbGrid(theDbGrid);
      finally
        FBinding := False;
      end;
      LoadVisibleColumns;
      if (propertyGrid.Cols > 0) then
         Self.SelectedColumn := 1;
      if (propertyGrid.Cols > 0) then
         Self.SelectedColumn := 1;
      ColumnsChanged := True;
    end;
  finally
    FreeAndNil(dgGetDbGrid);
  end;
end;

procedure TdgEditTgColumns.propertyGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var col, row : Integer;
begin
  if (FColumnResized) then
  begin
    FColumnResized := False;
    exit;
  end;
      
  propertyGrid.CellFromXY(X, Y, col, row);
  if (col > 0) and
     (propertyGrid.DataColnr[col] <> SelectedColumn) then
     SelectedColumn := propertyGrid.DataColnr[col];
end;

procedure TdgEditTgColumns.cbComboDataSourceKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = vk_Delete) and
     (SelectedDbCol.Combo <> Nil) then
  begin
    SelectedDbCol.Combo.DataSource := Nil;
    cbComboDataSource.ItemIndex := -1;
    Key := 0;
  end;
end;

procedure TdgEditTgColumns.btLeftClick(Sender: TObject);
begin
  if (SelectedDbCol <> Nil) then
  begin
    SelectedDbCol.HeadingHorzAlignment := htaLeft;
    SelectedDbCol.HorzAlignment        := htaLeft;
    ColumnsChanged := True;
  end;
end;

procedure TdgEditTgColumns.btCenterClick(Sender: TObject);
begin
  if (SelectedDbCol <> Nil) then
  begin
    SelectedDbCol.HeadingHorzAlignment := htaCenter;
    SelectedDbCol.HorzAlignment        := htaCenter;
    ColumnsChanged := True;
  end;
end;

procedure TdgEditTgColumns.btRightClick(Sender: TObject);
begin
  if (SelectedDbCol <> Nil) then
  begin
    SelectedDbCol.HeadingHorzAlignment := htaRight;
    SelectedDbCol.HorzAlignment        := htaRight;
    ColumnsChanged := True;
  end;
end;

procedure TdgEditTgColumns.btDeleteClick(Sender: TObject);
begin
  btRemoveClick(btRemove);
  ResetSelection;
  ColumnsChanged := True;
end;

procedure TdgEditTgColumns.propertyGridTopLeftChanged(Sender: TObject;
  OldCol, OldRow, NewCol, NewRow: Integer; ByUser: Boolean);
begin
  if FUpdating then exit;
  
  if (SelectedColumn >= 1) and
     (propertyGrid.Cols >= 1) then
     Self.DisplayColumnButtons;
end;

procedure TdgEditTgColumns.pnTopResize(Sender: TObject);
begin
  propertyGrid.Height := pnTop.Height - 45;
  propertyGrid.Width  := pnTop.Width - 20;
end;

procedure TdgEditTgColumns.pnMidResize(Sender: TObject);
begin
  pnFields.Height := pnMid.Height - 4;
  pnColumns.Height := pnFields.Height;
  pnProperties.Height := pnFields.Height;
end;

end.
