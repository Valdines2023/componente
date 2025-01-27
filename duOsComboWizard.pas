unit duOsComboWizard;

interface

{$INCLUDE TSCmpVer}
//{$DEFINE rtTest}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids_ts, TSGrid, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  db,tsCommon, TypInfo, TsDateTimeDef, osAdvDbGrid
  {$IFDEF TSVER_V6}, Variants, ValEdit {$ENDIF}
  , osColorComboBox, Menus, ImgList;

type
  TdgOsCombo = class(TForm)
    Panel1: TPanel;
    pgCombo: TPageControl;
    tsDataSource: TTabSheet;
    tsFields: TTabSheet;
    tsProperties: TTabSheet;
    tsData: TTabSheet;
    tsLayout: TTabSheet;
    gdFields: TtsGrid;
    Label2: TLabel;
    Label3: TLabel;
    chAutoFill: TCheckBox;
    chParentCombo: TCheckBox;
    Label4: TLabel;
    cbStyle: TComboBox;
    Label5: TLabel;
    edDropdownRows: TEdit;
    udDropdownRows: TUpDown;
    Label6: TLabel;
    Label7: TLabel;
    cbValueCol: TComboBox;
    Label8: TLabel;
    cbAutoSearch: TComboBox;
    Label9: TLabel;
    Label11: TLabel;
    cbCompare: TComboBox;
    Label12: TLabel;
    gdData: TtsGrid;
    laData: TLabel;
    gbGrid: TGroupBox;
    gbColProperties: TGroupBox;
    Panel2: TPanel;
    cbColumn: TComboBox;
    gdProperties: TtsGrid;
    gdGridProperties: TtsGrid;
    btDone: TSpeedButton;
    btCancel: TSpeedButton;
    btPrev: TSpeedButton;
    btNext: TSpeedButton;
    Label14: TLabel;
    chAutoDropdown: TCheckBox;
    laSelectedColumn: TLabel;
    cbColors: TosColorComboBox;
    gdSample: TosAdvDbGrid;
    Label15: TLabel;
    edDropDownCols: TEdit;
    udDropDownCols: TUpDown;
    ImageList1: TImageList;
    lbDataSources: TComboBox;
    Label13: TLabel;
    rbRuntime: TRadioButton;
    rbStoreData: TRadioButton;
    rbDatasource: TRadioButton;
    rbComboSQL: TRadioButton;
    moSQL: TMemo;
    dgColor: TColorDialog;
    chAutoAdvance: TCheckBox;
    hcAlign: TPanel;
    btLeft: TSpeedButton;
    btCenter: TSpeedButton;
    btRight: TSpeedButton;
    btDelete: TSpeedButton;
    dgFont: TFontDialog;
    btAddRow: TButton;
    btDeleteRow: TButton;
    Label1: TLabel;
    edCols: TEdit;
    udCols: TUpDown;
    chAutoLookup: TCheckBox;
    chValueColSorted: TCheckBox;
    procedure btCancelClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btPrevClick(Sender: TObject);
    procedure pgComboChange(Sender: TObject);
    procedure gdGridPropertiesComboCellLoaded(Sender: TObject;
      Combo: TtsComboGrid; DataCol, DataRow: Integer; var Value: Variant);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gdGridPropertiesComboDropDown(Sender: TObject;
      Combo: TtsComboGrid; DataCol, DataRow: Integer);
    procedure gdGridPropertiesComboGetValue(Sender: TObject;
      Combo: TtsComboGrid; GridDataCol, GridDataRow, ComboDataRow: Integer;
      var Value: Variant);
    procedure gdGridPropertiesComboInit(Sender: TObject;
      Combo: TtsComboGrid; DataCol, DataRow: Integer);
    procedure gdGridPropertiesDblClick(Sender: TObject);
    procedure gdGridPropertiesEndCellEdit(Sender: TObject; DataCol,
      DataRow: Integer; var Cancel: Boolean);
    procedure btDoneClick(Sender: TObject);
    procedure lbDataSourcesClick(Sender: TObject);
    procedure gdFieldsCellEdit(Sender: TObject; DataCol, DataRow: Integer;
      ByUser: Boolean);
    procedure gdFieldsGetDrawInfo(Sender: TObject; DataCol,
      DataRow: Integer; var DrawInfo: TtsDrawInfo);
    procedure pgComboChanging(Sender: TObject; var AllowChange: Boolean);
    procedure gdDataCellLoaded(Sender: TObject; DataCol, DataRow: Integer;
      var Value: Variant);
    procedure gdDataColResized(Sender: TObject; RowColnr: Integer);
    procedure gdDataCellChanged(Sender: TObject; OldCol, NewCol, OldRow,
      NewRow: Integer);
    procedure chParentComboClick(Sender: TObject);
    procedure gdDataEndCellEdit(Sender: TObject; DataCol, DataRow: Integer;
      var Cancel: Boolean);
    procedure rbComboSQLClick(Sender: TObject);
    procedure rbRuntimeClick(Sender: TObject);
    procedure rbStoreDataClick(Sender: TObject);
    procedure rbDatasourceClick(Sender: TObject);
    procedure btLeftClick(Sender: TObject);
    procedure btCenterClick(Sender: TObject);
    procedure btRightClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure gdGridPropertiesButtonClick(Sender: TObject; DataCol,
      DataRow: Integer);
    procedure btAddRowClick(Sender: TObject);
    procedure btDeleteRowClick(Sender: TObject);
    procedure cbColumnClick(Sender: TObject);
    procedure gdPropertiesCellChanged(Sender: TObject; OldCol, NewCol,
      OldRow, NewRow: Integer);
    procedure gdPropertiesEndCellEdit(Sender: TObject; DataCol,
      DataRow: Integer; var Cancel: Boolean);
    procedure gdPropertiesComboDropDown(Sender: TObject;
      Combo: TtsComboGrid; DataCol, DataRow: Integer);
    procedure gdPropertiesComboCellLoaded(Sender: TObject;
      Combo: TtsComboGrid; DataCol, DataRow: Integer; var Value: Variant);
    procedure gdPropertiesComboGetValue(Sender: TObject;
      Combo: TtsComboGrid; GridDataCol, GridDataRow, ComboDataRow: Integer;
      var Value: Variant);
    procedure gdPropertiesComboInit(Sender: TObject; Combo: TtsComboGrid;
      DataCol, DataRow: Integer);
    procedure gdPropertiesButtonClick(Sender: TObject; DataCol,
      DataRow: Integer);
    procedure gdPropertiesDblClick(Sender: TObject);
    procedure pgComboResize(Sender: TObject);
    procedure tsPropertiesExit(Sender: TObject);
  private
    { Private declarations }
    FDataSource : TDataSource;
    FCombo : TosDbCombo;
    FSelectedColumn, FSourceColumn : TosDbCol;
    FGridPropertiesRefresh, FSelecting, FColPropertiesRefresh : Boolean;
    FEditingProperty : String;

    procedure CheckAllowSQL;
    procedure ShowFieldsForSQL;
    procedure EnableComboProperties(onOff : Boolean);
    procedure DisplayColumnButtons;
    procedure SetCombo(Value : TosDbCombo);
    procedure SetDataSource(Value : TDataSource);
    procedure SetSelectedColumn(Value : TosDbCol);
    procedure RefreshComboProperties;
    procedure RefreshGridProperties;
    procedure RefreshComboData;
    procedure RefreshFields;
    function  CheckComboCreated : TosDbCombo;
    procedure BindColProperty(DataRow : Integer);
    procedure BindGridProperty(dataRow : Integer);
    procedure RefreshColProperties;
    function  GetColPropertyValueText(PropertyName: String; forRow : Integer) : String;
    function  GetGridPropertyValueText(PropertyName: String; forRow : Integer) : String;
  public
    { Public declarations }

    function Execute(onCol : TosDbCol; cbQuery : TDataSet) : Boolean;

    property Combo : TosDbCombo read FCombo write SetCombo;
    property DataSource : TDataSource read FDataSource write SetDataSource;
    property SelectedColumn : TosDbCol read FSelectedColumn write SetSelectedColumn;
  end;

var
  dgOsCombo: TdgOsCombo;
  EnumList : TStringList;
  SelectedColNo : Integer;

implementation

{$R *.dfm}

uses {$IFNDEF rtTest} osAdvGridEditors, {$ENDIF} auOsDesignUtils;


function TdgOsCombo.Execute(onCol : TosDbCol; cbQuery : TDataSet) : Boolean;
var i : Integer;
begin
  pgCombo.ActivePageIndex := 0;
  FSourceColumn := onCol;
  gdSample.Col[1].Width := onCol.Width;
  gdSample.Col[1].FieldName := onCol.FieldName;
  gdSample.Col[1].Heading   := onCol.Heading;
  gdSample.Col[1].AssignCombo;
  gdSample.ComboQuery := cbQuery;
  btPrev.Enabled := False;
  {if (gdSample.ComboQuery = Nil) then
     ShowMessage('No Combo Query '); }
  CustomComboAssign(gdSample.Col[1], onCol);

  lbDataSources.Items.Clear;
{$IFNDEF rtTest}
  if (Assigned(osAdvGridEditors.lsDataSources)) then
     for i := 0 to osAdvGridEditors.lsDataSources.Count - 1 do
        lbDataSources.Items.AddObject(osAdvGridEditors.lsDataSources.Strings[i], osAdvGridEditors.lsDataSources.Objects[i]);
{$ENDIF}
  if (lbDataSources.Items.Count = 0) and
     (onCol.ComboDatasource <> nil) then
     lbDataSources.ItemIndex := lbDataSources.Items.AddObject(onCol.ComboDatasource.Name, onCol.ComboDatasource);
  SelectedColumn := gdSample.Col[1];
  if (onCol.Combo <> Nil) then
     DataSource := onCol.Combo.DataSource;
  CheckAllowSQL;
  
  Result := False;
  if ShowModal = mrOk then
     Result := True;
end;

function TdgOsCombo.CheckComboCreated : TosDbCombo;
begin
  Result := Combo;
  if (Result = Nil) then
  begin
    SelectedColumn.AssignCombo;
    FCombo := SelectedColumn.Combo;
  end;
  Result := Combo;
end;

procedure TdgOsCombo.SetSelectedColumn(Value : TosDbCol);
begin
  if (Value <> FSelectedColumn) then
  begin
    FSelectedColumn := Value;
    if (Value = Nil) then
    begin
      DataSource := Nil;
      laSelectedColumn.Caption := 'None';
    end
    else
    begin
      if FSelectedColumn.FieldName <> '' then
         laSelectedColumn.Caption := FSelectedColumn.FieldName
      else if (FSelectedColumn.Heading <> '') then
         laSelectedColumn.Caption := FSelectedColumn.Heading
      else
         laSelectedColumn.Caption := 'Column # ' + IntToStr(FSelectedColumn.DisplayCol);
    end;
    Combo := Value.Combo;
  end;
end;

procedure TdgOsCombo.SetCombo(Value : TosDbCombo);
begin
  if (Value <> FCombo) then
  begin
    FCombo := Value;
    moSQL.Text := '';
    if (Value = Nil) then
    begin
      DataSource := Nil;
      rbRuntime.Checked := True;
    end
    else
    begin
      DataSource := Value.DataSource;
      rbDataSource.Checked := (Value.DataSource <> Nil);
      if (Value.SQL.Count > 0) then
      begin
        rbComboSQL.Checked := True;
        moSQL.Text := Value.SQL.Text;
      end
      else if not rbDataSource.Checked then
        rbStoreData.Checked := Value.ComboGrid.StoreData;

      if (not rbStoreData.Checked) and
         (not rbComboSQL.Checked) and
         (not rbDataSource.Checked) then
         rbRuntime.Checked := True;
    end;

    RefreshComboProperties;
    //tsData.TabVisible := chStoreData.Checked;
    //tsFields.TabVisible := not chStoreData.Checked;
  end;
end;

procedure TdgOsCombo.SetDataSource(Value : TDataSource);
var i : Integer;
begin
  if (Value <> FDataSource) then
  begin
    FDataSource := Value;
    if (Value = Nil) or
       (FDataSource.DataSet = Nil) or
       (not FDataSource.DataSet.Active) then
    begin
      gdFields.Rows := 0;
      lbDataSources.ItemIndex := -1;
    end
    else
    begin
      gdFields.Rows := FDataSource.DataSet.FieldCount;
      for i := 1 to gdFields.Rows do
      begin
        gdFields.Cell[1, i] := '0';
        gdFields.Cell[2, i] := FDataSource.DataSet.Fields.Fields[i-1].FieldName;
      end;
      lbDataSources.ItemIndex := -1;
      for i := 0 to lbDataSources.Items.Count - 1 do
        if (TDataSource(lbDataSources.Items.Objects[i]) = Value) then
        begin
          lbDataSources.ItemIndex := i;
          break;
        end;
    end;
  end;
end;

procedure TdgOsCombo.ShowFieldsForSQL;
var i : Integer;
begin
  // Assume that sql has already been executed and Combo cols are configured...
  gdFields.Rows := 0;
  gdFields.Rows := Combo.ComboGrid.Cols;
  for i := 1 to gdFields.Rows do
  begin
    gdFields.Cell[1, i] := '1';
    gdFields.Cell[2, i] := Combo.ComboGrid.Col[i].FieldName;
    gdFields.Cell[3, i] := IntToStr(i);
    gdFields.Cell[4, i] := '1';
  end;
end;

procedure TdgOsCombo.EnableComboProperties(onOff : Boolean);
begin
  chAutoAdvance.Enabled := onOff;
  chAutoDropdown.Enabled := onOff;
  udDropdownRows.Enabled := onOff;
  edDropdownRows.Enabled := onOff;
  udDropDownCols.Enabled := onOff;
  edDropDownCols.Enabled := onOff;
  chAutoLookup.Enabled := onOff;
  cbStyle.Enabled := onOff;
  cbAutoSearch.Enabled := onOff;
  chAutoFill.Enabled := onOff;
  cbCompare.Enabled := onOff;
  cbValueCol.Enabled := onOff;
  chValueColSorted.Enabled := onOff;
end;

procedure TdgOsCombo.RefreshComboProperties;
var i : Integer;
begin
  if (Combo = Nil) then
  begin
    chParentCombo.Checked := True;
    chAutoDropdown.Checked := False;
    chAutoAdvance.Checked := False;
    udDropdownRows.Position := 4;
    udDropDownCols.Position := 1;
    udCols.Position := 1;
    cbStyle.ItemIndex := -1;
    cbAutoSearch.ItemIndex := -1;
    chAutoFill.Checked := False;
    chAutoLookup.Checked := False;
    cbCompare.ItemIndex := -1;
    cbValueCol.ItemIndex := -1;
    chValueColSorted.Checked := False;
    EnableComboProperties(False);
  end
  else
  begin
    chParentCombo.Checked := False;
    chAutoAdvance.Checked  := Combo.AutoAdvance;
    chAutoDropdown.Checked := Combo.AutoDropDown;
    udDropdownRows.Position := Combo.DropDownRows;
    udDropDownCols.Position := Combo.DropDownCols;
    udCols.Position := Combo.ComboGrid.Cols;
    cbStyle.ItemIndex := Integer(Combo.DropDownStyle);
    cbAutoSearch.ItemIndex := Integer(Combo.AutoSearch);
    chAutoFill.Checked := Combo.AutoFill;
    chAutoLookup.Checked := Combo.AutoLookup;
    cbCompare.ItemIndex := Integer(Combo.CompareType);
    cbValueCol.ItemIndex := -1;
    cbValueCol.Items.Clear;
    for i := 1 to Combo.ComboGrid.Cols do
      cbValueCol.Items.Add(Combo.ComboGrid.Col[i].FieldName);
    if (Combo.ValueCol > 0) then
       cbValueCol.ItemIndex := Combo.ValueCol - 1;
    chValueColSorted.Checked := Combo.ValueColSorted;
    EnableComboProperties(True);
  end;
  if (pgCombo.ActivePage = tsProperties) and
     (cbStyle.Showing) then
     cbStyle.SetFocus;
end;

procedure TdgOsCombo.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TdgOsCombo.btNextClick(Sender: TObject);
var bChange : Boolean;
begin
  if (pgCombo.ActivePageIndex < 4) then
  begin
    bChange := True;
    pgComboChanging(pgCombo, bChange);
    if bChange then
    begin
      pgCombo.ActivePageIndex := pgCombo.ActivePageIndex + 1;
      pgComboChange(pgCombo);
    end;
  end;
end;

procedure TdgOsCombo.btPrevClick(Sender: TObject);
var bChange : Boolean;
begin
  if (pgCombo.ActivePageIndex > 0) then
  begin
    bChange := True;
    pgComboChanging(pgCombo, bChange);
    if bChange then
    begin
      pgCombo.ActivePageIndex := pgCombo.ActivePageIndex - 1;
      pgComboChange(pgCombo);
    end;
  end;
end;

procedure TdgOsCombo.RefreshFields;
var i, j : Integer;
begin
  if (Combo = Nil) then exit;

  if (Combo.SQL.Count > 0) then
     ShowFieldsForSQL
  else
  begin
    for i := 1 to Combo.ComboGrid.Cols do
    begin
      for j := 1 to gdFields.Rows do
      begin
        if (gdFields.Cell[2, j] = Combo.ComboGrid.Col[i].FieldName) then
        begin
          gdFields.CellCheckBoxState[1, j] := cbChecked;
          gdFields.Cell[3, j] := IntToStr(Combo.ComboGrid.Col[i].DisplayCol);
          if Combo.ComboGrid.Col[i].Visible then
             gdFields.Cell[4, j] := '1'
          else
             gdFields.Cell[4, j] := '0';
        end;
      end;
    end;
  end;
end;

procedure TdgOsCombo.RefreshGridProperties;
var
  PropertyIndex,
  PropertyCount, iRow, iCol : Integer;
  PropList : TPropList;
  sName, sText : String;

  function ShowProperty(propName : String) : Boolean;
  begin
    Result := False;
    if (AnsiCompareText(propName, 'Color') = 0) or
       (AnsiCompareText(propName, 'Font') = 0) or
       (AnsiCompareText(propName, 'DefaultRowHeight') = 0) or
       (AnsiCompareText(propName, 'DefaultColWidth') = 0) or
       (AnsiCompareText(propName, 'GridLines') = 0) or
       (AnsiCompareText(propName, 'HorzAlignment') = 0) or
       (AnsiCompareText(propName, 'VertAlignment') = 0) or
       (AnsiCompareText(propName, 'Is3D') = 0) or
       (AnsiCompareText(propName, 'Rows') = 0) or
       (AnsiCompareText(propName, 'LineColor') = 0) or
       (AnsiCompareText(propName, 'SelectionColor') = 0) or
       (AnsiCompareText(propName, 'SelectionFontColor') = 0) or
       (AnsiCompareText(propName, 'WordWrap') = 0) or
       (AnsiCompareText(propName, 'HeadingOn') = 0) or
       (AnsiCompareText(propName, 'HeadingColor') = 0) or
       (AnsiCompareText(propName, 'HeadingFont') = 0) or
       (AnsiCompareText(propName, 'FixedRowCount') = 0) or
       (AnsiCompareText(propName, 'RowBarOn') = 0) or
       (AnsiCompareText(propName, 'SelectionType') = 0) or
       (AnsiCompareText(propName, 'ParentFont') = 0) then
       Result := True;
  end;
begin
  if (Combo = Nil) or
     (Combo.ComboGrid = Nil) then
     exit;

  FGridPropertiesRefresh := True;
  gdGridProperties.BeginUpdate;
  try
    PropertyCount := GetPropList(Combo.ComboGrid.ClassInfo, tkProperties, @PropList);
    iRow := 1;
    gdGridProperties.Rows := 20;
    for PropertyIndex := 0 to PropertyCount -1 do
    begin
      sName := PropList[PropertyIndex].Name;
      if ShowProperty(sName) then
      begin
        sText := GetGridPropertyValueText(sName, iRow);
        gdGridProperties.Cell[1, iRow] := sName;
        gdGridProperties.Cell[2, iRow] := sText;
        Inc(iRow);
      end;
    end;
  finally
    gdGridProperties.EndUpdate;
    FGridPropertiesRefresh := False;
  end;

  cbColumn.Items.Clear;
  for iCol := 1 to Combo.ComboGrid.Cols do
  begin
    if (Combo.ComboGrid.Col[iCol].Fieldname = '') then
       cbColumn.Items.Add('Column ' + IntToStr(iCol))
    else
       cbColumn.Items.Add(Combo.ComboGrid.Col[iCol].Fieldname);
  end;
  if (cbColumn.Items.Count > 0) then
  begin
    cbColumn.ItemIndex := 0;
    RefreshColProperties;
  end
  else
    gdProperties.Rows := 0;
end;

function TdgOsCombo.GetGridPropertyValueText(PropertyName: String; forRow : Integer) : String;
begin
  Result := VarToStr(GetPropValue(Combo.ComboGrid, PropertyName, False));
  case GetPropertyType(Combo.ComboGrid, PropertyName) of
    ptSpin    : gdGridProperties.CellButtonType[2, forRow] := btVertSpin;
    ptCombo   :
        begin
          Result := GetEnumProp(Combo.ComboGrid, PropertyName);
          gdGridProperties.CellButtonType[2, forRow] := btCombo;
        end;
    ptBoolean :
        begin
          Result := GetEnumProp(Combo.ComboGrid, PropertyName);
          gdGridProperties.CellButtonType[2, forRow] := btCombo;
        end;
    ptButton  :
        begin
          gdGridProperties.CellButtonType[2, forRow] := btNormal;
          Result := '(' + PropertyName + ')';
        end;
    ptColor   :
        begin
          ColorToIdent(StrToint(Result), Result);
          gdGridProperties.CellButtonType[2, forRow] := btCombo;
        end;
  end;
end;

procedure TdgOsCombo.pgComboChange(Sender: TObject);
begin
  if (pgCombo.ActivePage = tsLayout) then
     RefreshGridProperties
  else if (pgCombo.ActivePage = tsFields) then
     RefreshFields
  else if (pgCombo.ActivePage = tsProperties) then
     RefreshComboProperties
  else if (pgCombo.ActivePage = tsData) then
     RefreshComboData;
  btPrev.Enabled := (pgCombo.ActivePageIndex > 0);
  btNext.Enabled := (pgCombo.ActivePageIndex < 4);
end;

procedure TdgOsCombo.RefreshComboData;
var i : Integer;
begin
  gdData.Rows := 0;
  if (Combo = Nil) or
     (Combo.ComboGrid = Nil) then
     exit;

  //if (Combo.ComboGrid.DataSource = Nil) and
  //   (Combo.DataSource <> Nil) then
  Combo.ComboGrid.DataSource := Combo.DataSource;

  gdData.GridMode := gmBrowse;
  gdData.Rows := Combo.DropDownRows;
  if (rbStoreData.Checked) then
  begin
    gdData.GridMode := gmEditInsert;
    gdData.AlwaysShowEditor := True;
    gdData.Rows := Combo.ComboGrid.Rows;
  end;
  btAddRow.Enabled    := Combo.ComboGrid.StoreData;
  btDeleteRow.Enabled := Combo.ComboGrid.StoreData;
  gdData.Color              := TtsGrid(Combo.ComboGrid).Color;
  gdData.Font               := Combo.ComboGrid.Font;
  //gdData.GridLines          := TtsGrid(Combo.ComboGrid).GridLines;
  gdData.Cols               := Combo.ComboGrid.Cols;
  //gdData.HeadingOn          := TtsGrid(Combo.ComboGrid).HeadingOn;
  gdData.SelectionType      := TtsGrid(Combo.ComboGrid).SelectionType;
  gdData.RowBarOn           := TtsGrid(Combo.ComboGrid).RowBarOn;
  gdData.DefaultRowHeight   := TtsGrid(Combo.ComboGrid).DefaultRowHeight;
  gdData.SelectionColor     := TtsGrid(Combo.ComboGrid).SelectionColor;
  gdData.SelectionFontColor := TtsGrid(Combo.ComboGrid).SelectionFontColor;
  gdData.LineColor          := TtsGrid(Combo.ComboGrid).LineColor;
  gdData.Is3D               := TtsGrid(Combo.ComboGrid).Is3D;

  for i := 1 to gdData.Cols do
  begin
    gdData.Col[i].HeadingAlignment := Combo.ComboGrid.Col[i].Alignment;
    gdData.Col[i].Alignment        := Combo.ComboGrid.Col[i].Alignment;
    gdData.Col[i].Width            := Combo.ComboGrid.Col[i].Width;
    gdData.Col[i].Color            := Combo.ComboGrid.Col[i].Color;
    gdData.Col[i].Visible          := Combo.ComboGrid.Col[i].Visible;
    gdData.Col[i].Heading          := Combo.ComboGrid.Col[i].Heading;
    if (gdData.Col[i].Heading = '') and
       (rbStoreData.Checked) then
       gdData.Col[i].Heading := 'Column ' + IntToStr(i);
  end;
  if (gdData.Cols = 0) then
  begin
     SelectedColNo := 0;
     hcAlign.Hide;
  end
  else
     SelectedColNo := 1;

  DisplayColumnButtons;

  if (gdData.Rows = 0) and
     (Combo.ComboGrid.StoreData) then
  begin
    Combo.ComboGrid.Rows := 1;
    Combo.ComboGrid.Cell[1,1] := '<Key in Data here>';
    gdData.Rows := 1;
    gdData.Invalidate;
  end;
end;

procedure TdgOsCombo.gdGridPropertiesComboCellLoaded(Sender: TObject;
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
    Value := EnumList.Strings[DataRow-1];
end;

procedure TdgOsCombo.FormCreate(Sender: TObject);
begin
  EnumList := TStringList.Create;
end;

procedure TdgOsCombo.FormDestroy(Sender: TObject);
begin
  EnumList.Free;
end;

procedure TdgOsCombo.gdGridPropertiesComboDropDown(Sender: TObject;
  Combo: TtsComboGrid; DataCol, DataRow: Integer);
var sPropName, sValue : String;
begin
  sPropName := gdGridProperties.Cell[1, DataRow];
  Combo.Tag := 0;
  if GetPropertyType(Self.Combo.ComboGrid, sPropName) = ptColor then
  begin
    Combo.Tag := 2;
    Combo.Rows := 42;
    Combo.DropDownRows := 12;
  end
  else if GetPropertyType(Self.Combo.ComboGrid, sPropName) = ptBoolean then
  begin
    Combo.Tag := 1;
    Combo.Rows := 2;
    if LowerCase(sValue) = 'true' then
       Combo.CurrentDataRow := 1
    else
       Combo.CurrentDataRow := 2;
   end
   else if GetPropertyType(Self.Combo.ComboGrid, sPropName) = ptCombo then
   begin
     Combo.Rows := LoadEnumList(EnumList, Self.Combo.ComboGrid, sPropName);
     if Combo.Rows <= 8 then
        Combo.DropDownRows := Combo.Rows
     else
        Combo.DropDownRows := 8;
     sValue := GetEnumProp(Self.Combo.ComboGrid, sPropName);
     Combo.CurrentDataRow := EnumList.IndexOf(sValue) + 1;
     Combo.Tag := 3;
   end;
end;

procedure TdgOsCombo.gdGridPropertiesComboGetValue(Sender: TObject;
  Combo: TtsComboGrid; GridDataCol, GridDataRow, ComboDataRow: Integer;
  var Value: Variant);
begin
  Value := Combo.Cell[1, ComboDataRow];
  gdGridProperties.Cell[GridDataCol, GridDataRow] := Value;
  BindGridProperty(GridDataRow);
end;

procedure TdgOsCombo.BindGridProperty(dataRow : Integer);
begin
  {GridModified;
  if (SelectedtsCol.ButtonType = btCombo) and
     (AnsiCompareText(gdProperties.Cell[1, DataRow], 'ButtonType') = 0) and
     (AnsiCompareText(gdProperties.Cell[2, DataRow], 'btCombo') <> 0) then
     RemoveCombo(SelectedtsCol); }
  if GetPropertyType(Combo.ComboGrid, gdGridProperties.Cell[1, DataRow]) = ptColor then
     SetPropValue(Combo.ComboGrid, gdGridProperties.Cell[1, DataRow], StringToColor(FullColorName(gdGridProperties.Cell[2, DataRow])))
  else
  begin
    if (AnsiCompareText(gdGridProperties.Cell[1, DataRow], 'ReadOnly') = 0) then
    begin
      FSelecting := True;
      try
        SetPropValue(Combo.ComboGrid, gdGridProperties.Cell[1, DataRow], gdGridProperties.Cell[2, DataRow]);
      finally
        FSelecting := False;
      end;
    end
    else
       SetPropValue(Combo.ComboGrid, gdGridProperties.Cell[1, DataRow], gdGridProperties.Cell[2, DataRow]);
  end;
  //AddModifiedColProperty(gdProperties.Cell[1, DataRow]);
  //if (AnsiCompareText(gdProperties.Cell[1, DataRow], 'DisplayCol') = 0) then
  //   FColumnsChanged := True;
end;

procedure TdgOsCombo.gdGridPropertiesComboInit(Sender: TObject;
  Combo: TtsComboGrid; DataCol, DataRow: Integer);
begin
  Combo.Width := gdGridProperties.Col[2].Width - 15;
  Combo.Color := clWindow;
  Combo.DropDownStyle := ddDropDownList;
  Combo.GridLines := glNone;
  Combo.AutoSearch := asTop;
end;

procedure TdgOsCombo.gdGridPropertiesDblClick(Sender: TObject);
begin
  if LowerCase(gdGridProperties.Cell[gdGridProperties.CurrentDataCol, gdGridProperties.CurrentDataRow]) = 'true' then
  begin
     gdGridProperties.Cell[gdGridProperties.CurrentDataCol, gdGridProperties.CurrentDataRow] := 'False';
     BindGridProperty(gdGridProperties.CurrentDataRow);
  end
  else if LowerCase(gdGridProperties.Cell[gdGridProperties.CurrentDataCol, gdGridProperties.CurrentDataRow]) = 'false' then
  begin
     gdGridProperties.Cell[gdGridProperties.CurrentDataCol, gdGridProperties.CurrentDataRow] := 'True';
     BindGridProperty(gdGridProperties.CurrentDataRow);
  end
  else if (Pos('Color', gdGridProperties.Cell[1, gdGridProperties.CurrentDataRow]) > 0) then
  begin
    if dgColor.Execute then
    begin
      Combo.ComboGrid.Color := dgColor.Color;
      gdGridProperties.Cell[2, gdGridProperties.CurrentDataRow] := ColorToString(dgColor.Color);
    end;
  end;
end;

procedure TdgOsCombo.gdGridPropertiesEndCellEdit(Sender: TObject; DataCol,
  DataRow: Integer; var Cancel: Boolean);
begin
  BindGridProperty(DataRow);
end;

procedure TdgOsCombo.btDoneClick(Sender: TObject);
var bAllowed : Boolean;
begin
  bAllowed := True;
  pgComboChanging(pgCombo, bAllowed);

  try
    if (Combo <> Nil) then
    begin
      Combo.AutoAdvance   := chAutoAdvance.Checked;
      Combo.AutoDropDown  := chAutoDropdown.Checked;
      Combo.DropDownRows  := udDropdownRows.Position;
      Combo.DropDownStyle := TtsDropDownStyle(cbStyle.ItemIndex);
      Combo.AutoSearch    := TtsComboAutoSearchType(cbAutoSearch.ItemIndex);
      Combo.AutoFill      := chAutoFill.Checked;
      Combo.AutoLookup    := chAutoLookup.Checked;
      Combo.CompareType   := TtsComboCompareType(cbCompare.ItemIndex);
      if (cbValueCol.ItemIndex >= 0) then
         Combo.ValueCol := cbValueCol.ItemIndex + 1;
      Combo.ValueColSorted := chValueColSorted.Checked;
    end;

    CustomComboAssign(FSourceColumn, SelectedColumn);

  except on E:Exception do
    raise Exception.Create('Error saving Column changes...' + #10#13 + E.Message);
  end;
  
  ModalResult := mrOk;
end;

procedure TdgOsCombo.lbDataSourcesClick(Sender: TObject);
begin
  if (lbDataSources.ItemIndex >= 0) then
     Self.DataSource := TDataSource(lbDataSources.Items.Objects[lbDataSources.ItemIndex]);
end;

procedure TdgOsCombo.gdFieldsCellEdit(Sender: TObject; DataCol,
  DataRow: Integer; ByUser: Boolean);
var i, j : Integer;

  function NextPos : Integer;
  var i : Integer;
  begin
    Result := 1;
    for i := 1 to gdFields.Rows do
      if (i <> gdFields.CurrentDataRow) and
         (gdFields.CellCheckBoxState[1, i] = cbChecked) then
         Inc(Result);
  end;
begin
  if (DataCol = 1) then
  begin
    if (gdFields.Cell[1, DataRow] = '1') then
    begin
      gdFields.Cell[3, DataRow] := NextPos;
      gdFields.Cell[4, DataRow] := '1';
    end
    else
    begin
      j := StrToInt(gdFields.Cell[3, DataRow]);
      gdFields.Cell[3, DataRow] := '';
      gdFields.Cell[4, DataRow] := '0';
      for i := 1 to gdFields.Rows do
        if (not VarIsEmpty(gdFields.Cell[4, i])) and
           (gdFields.Cell[4, i] = '1') and
           (StrToInt(gdFields.Cell[3, i]) > j) then
           gdFields.Cell[3, i] := IntToStr(StrToInt(gdFields.Cell[3, i]) - 1);
    end;
  end;
  gdFields.RowInvalidate(DataRow);
end;

procedure TdgOsCombo.gdFieldsGetDrawInfo(Sender: TObject; DataCol,
  DataRow: Integer; var DrawInfo: TtsDrawInfo);
begin
  if (gdFields.CellCheckBoxState[1, DataRow] = cbChecked) then
     DrawInfo.Font.Style := [fsBold];
end;

procedure TdgOsCombo.pgComboChanging(Sender: TObject; var AllowChange: Boolean);
var i, j : Integer;

  function SelectedCount : Integer;
  var i : Integer;
  begin
    Result := 0;
    for i := 1 to gdFields.Rows do
      if (gdFields.CellCheckBoxState[1, i] = cbChecked) then
         Inc(Result);
  end;
  function VisibleCount : Integer;
  var i : Integer;
  begin
    Result := 0;
    for i := 1 to gdFields.Rows do
      if (gdFields.CellCheckBoxState[4, i] = cbChecked) then
         Inc(Result);
  end;  
begin
  if (pgCombo.ActivePage = tsFields) and
     (gdFields.Rows > 0) then
  begin
    AllowChange := True;
    // bind fields to combo...
    Combo.DropDownCols   := VisibleCount;
    Combo.ComboGrid.Cols := SelectedCount;
    for j := 1 to gdFields.Rows do
    begin
      for i := 1 to gdFields.Rows do
      begin
        if (not VarIsEmpty(gdFields.Cell[3,i])) and
           (gdFields.Cell[3,i] = IntToStr(j)) then  // Position filled in
        begin
          Combo.ComboGrid.Col[j].FieldName := gdFields.Cell[2, i];
          Combo.ComboGrid.Col[j].DisplayCol := j;
          Combo.ComboGrid.Col[j].Visible := (gdFields.Cell[4,i]='1');
        end;
      end;
    end;
  end
  else if (pgCombo.ActivePage = tsDataSource) then
  begin
    AllowChange := True;
    if rbRunTime.Checked then
    begin
      if (Combo <> Nil) then
      begin
        Combo.DataSource := Nil;
        Combo.ComboGrid.StoreData := False;
        Combo.SQL.Clear;
      end;
    end
    else if rbStoreData.Checked then
    begin
      CheckComboCreated;
      Combo.ComboGrid.StoreData := True;
      if (Combo.ComboGrid.Cols = 0) then
         Combo.ComboGrid.Cols := 1;
      Combo.DataSource := Nil;
      Combo.SQL.Clear;
    end
    else if rbDatasource.Checked then
    begin
      CheckComboCreated;
      Combo.ComboGrid.StoreData := False;
      if (lbDataSources.ItemIndex < 0) then
         Combo.DataSource := Nil
      else
         Combo.DataSource := TDataSource(lbDataSources.Items.Objects[lbDataSources.ItemIndex]);
      DataSource := Combo.DataSource;
      Combo.SQL.Clear;
      Combo.RefreshComboData := True;
    end
    else if rbComboSQL.Checked then
    begin
      CheckComboCreated;
      Combo.ComboGrid.StoreData := False;
      Combo.SQL.Clear;
      //Combo.SQL.Add(moSQL.Text);
      //if (not Combo.CanOpenSQL) then
      //   raise Exception.Create('Error - Unable to execute SQL to obtain field layuout!');
      Combo.ReloadWithSQL(moSQL.Text);
      Combo.DataSource := Nil;
      Combo.RefreshComboData := True;
    end
    else
      AllowChange := False;
  end;
end;

procedure TdgOsCombo.gdDataCellLoaded(Sender: TObject; DataCol,
  DataRow: Integer; var Value: Variant);
var comboRow : Integer;
begin
  if (Combo <> Nil) and
     (Combo.ComboGrid <> Nil) then
  begin
    comboRow := Combo.ComboGrid.DataRownr[DataRow];
    try
      Value := VarToStr(Combo.ComboGrid.Cell[DataCol, comboRow]);
    except
    end;
  end;
end;

procedure TdgOsCombo.gdDataColResized(Sender: TObject; RowColnr: Integer);
begin
  Combo.ComboGrid.Col[RowColnr].Width := gdData.Col[RowColnr].Width;
end;

procedure TdgOsCombo.gdDataCellChanged(Sender: TObject; OldCol, NewCol,
  OldRow, NewRow: Integer);
var i : Integer;
begin
  SelectedColNo := NewCol;
  DisplayColumnButtons;
  if (SelectedColNo > 0) then
  begin
    for i := 1 to gdData.Cols do
      gdData.Col[i].Color := TtsGrid(Combo.ComboGrid).Color;
    gdData.Col[SelectedColNo].Color := $00E6F9BD;
  end;
end;

procedure TdgOsCombo.DisplayColumnButtons;
var i, j : Integer;
begin
  j := 3;
  for i := 1 to gdData.Cols do
  begin
    if (gdData.DataColnr[i] = SelectedColNo) then break;
    if (gdData.DataColnr[i] >= gdData.LeftCol) then
       j := j + gdData.Col[gdData.DataColnr[i]].Width;
  end;
  if (gdData.RowBarOn) then
     hcAlign.Left := gdData.Left + j + gdData.RowBarWidth
  else
     hcAlign.Left := gdData.Left + j;
  hcAlign.Show;

  for i := 1 to gdData.Cols do
      gdData.Col[i].Color := TtsGrid(Combo.ComboGrid).Color;
  if (SelectedColNo > 0) then
     gdData.Col[SelectedColNo].Color := $00E6F9BD;
end;

procedure TdgOsCombo.chParentComboClick(Sender: TObject);
begin
  if (not chParentCombo.Checked) and
     (Self.Combo = Nil) then
  begin
    FSelectedColumn.AssignCombo;
    FCombo := FSelectedColumn.Combo;
    EnableComboProperties(True);
    RefreshComboProperties;
  end
  else if (chParentCombo.Checked) then
  begin
    FSelectedColumn.ResetCombo;
    FCombo := Nil;
    EnableComboProperties(False);
    RefreshComboProperties;
  end;
end;

procedure TdgOsCombo.gdDataEndCellEdit(Sender: TObject; DataCol,
  DataRow: Integer; var Cancel: Boolean);
var comboRow : Integer;
begin
  if (Combo = Nil) or
     (Combo.ComboGrid = Nil) or
     (Combo.ComboGrid.StoreData = False) then
     exit;

  comboRow := Combo.ComboGrid.DataRownr[DataRow];
  if (Combo.ComboGrid.Rows < DataRow) then
  begin
    Combo.ComboGrid.Rows := DataRow;
    comboRow := Combo.ComboGrid.Rows;
  end;
  Combo.ComboGrid.Cell[DataCol, comboRow] := gdData.Cell[DataCol, DataRow];
end;

procedure TdgOsCombo.rbComboSQLClick(Sender: TObject);
begin
  CheckAllowSQL;
end;

procedure TdgOsCombo.CheckAllowSQL;
begin
  moSQL.Enabled := rbComboSQL.Checked;
  if moSQL.Enabled then
     moSQL.Color := clWindow
  else
     moSQL.Color := clBtnFace;
end;

procedure TdgOsCombo.rbRuntimeClick(Sender: TObject);
begin
  CheckAllowSQL;
end;

procedure TdgOsCombo.rbStoreDataClick(Sender: TObject);
begin
  CheckAllowSQL;
end;

procedure TdgOsCombo.rbDatasourceClick(Sender: TObject);
begin
  CheckAllowSQL;
end;

procedure TdgOsCombo.btLeftClick(Sender: TObject);
begin
  gdData.Col[SelectedColNo].HeadingHorzAlignment := htaLeft;
  gdData.Col[SelectedColNo].HorzAlignment := htaLeft;
  Combo.ComboGrid.Col[SelectedColNo].HeadingHorzAlignment := htaLeft;
  Combo.ComboGrid.Col[SelectedColNo].HorzAlignment := htaLeft;
end;

procedure TdgOsCombo.btCenterClick(Sender: TObject);
begin
  gdData.Col[SelectedColNo].HeadingHorzAlignment := htaCenter;
  gdData.Col[SelectedColNo].HorzAlignment := htaCenter;
  Combo.ComboGrid.Col[SelectedColNo].HeadingHorzAlignment := htaCenter;
  Combo.ComboGrid.Col[SelectedColNo].HorzAlignment := htaCenter;
end;

procedure TdgOsCombo.btRightClick(Sender: TObject);
begin
  gdData.Col[SelectedColNo].HeadingHorzAlignment := htaRight;
  gdData.Col[SelectedColNo].HorzAlignment := htaRight;
  Combo.ComboGrid.Col[SelectedColNo].HeadingHorzAlignment := htaRight;
  Combo.ComboGrid.Col[SelectedColNo].HorzAlignment := htaRight;
end;

procedure TdgOsCombo.btDeleteClick(Sender: TObject);
begin
  if (SelectedColNo > 0) and
     (SelectedColNo <= gdData.Cols) then
  begin
    gdData.DeleteCols(SelectedColNo, SelectedColNo);
    Combo.ComboGrid.DeleteCols(SelectedColNo, SelectedColNo);
    if (SelectedColNo > 1) then
       SelectedColNo := SelectedColNo - 1
    else if (SelectedColNo > gdData.Cols) then
       SelectedColNo := gdData.Cols;
    
  end;
end;

procedure TdgOsCombo.gdGridPropertiesButtonClick(Sender: TObject; DataCol,
  DataRow: Integer);
var theFont : TFont;
begin
  if (AnsiSameText(gdGridProperties.Cell[1, gdGridProperties.CurrentDataRow], 'Font')) then
  begin
    theFont := TFont.Create;
    try
      theFont.Assign(Combo.ComboGrid.Font);
      dgFont.Font.Assign(theFont);
      if (dgFont.Execute) then
         Combo.ComboGrid.Font.Assign(dgFont.Font);
    finally
      theFont.Free;
    end;
  end
  else if (AnsiSameText(gdGridProperties.Cell[1, gdGridProperties.CurrentDataRow], 'HeadingFont')) then
  begin
    theFont := TFont.Create;
    try
      if (TtsGrid(Combo.ComboGrid).HeadingParentFont) then
         theFont.Assign(Combo.ComboGrid.Font)
      else
         theFont.Assign(TtsGrid(Combo.ComboGrid).HeadingFont);
      dgFont.Font.Assign(theFont);
      if (dgFont.Execute) then
      begin
        if (TtsGrid(Combo.ComboGrid).HeadingParentFont) then
           TtsGrid(Combo.ComboGrid).HeadingParentFont := False;
        TtsGrid(Combo.ComboGrid).HeadingFont.Assign(dgFont.Font);
      end;
    finally
      theFont.Free;
    end;      
  end;
end;

procedure TdgOsCombo.btAddRowClick(Sender: TObject);
var theRow : Integer;
begin
  if (Combo.ComboGrid.StoreData) then
  begin
    theRow := gdData.CurrentDataRow;
    Combo.ComboGrid.InsertRow(gdData.CurrentDataRow);
    gdData.Rows := Combo.ComboGrid.Rows;
    if (theRow > 0) then
       gdData.CurrentDataRow := theRow
    else
       gdData.CurrentDataRow := gdData.Rows;
    gdData.SetFocus;
    gdData.CurrentCell.Value := '';
  end;
end;

procedure TdgOsCombo.btDeleteRowClick(Sender: TObject);
var theRow : Integer;
begin
  if (Combo.ComboGrid.StoreData) and
     (gdData.CurrentDataRow > 0) then
  begin
    theRow := Combo.ComboGrid.DataRownr[gdData.CurrentDataRow];
    Combo.ComboGrid.DeleteRows(theRow, theRow);
    gdData.Rows := Combo.ComboGrid.Rows;
    if (theRow > 0) and
       (theRow <= gdData.Rows) then
       gdData.CurrentDataRow := theRow
    else
       gdData.CurrentDataRow := gdData.Rows;
    DisplayColumnButtons;
  end;
end;

function TdgOsCombo.GetColPropertyValueText(PropertyName: String; forRow : Integer) : String;
var theCol : TosDbCol;
begin
  theCol := Combo.ComboGrid.Col[cbColumn.ItemIndex+1];
  if (theCol = Nil) then exit;

  Result := VarToStr(GetPropValue(theCol, PropertyName, False));
  if (AnsiCompareText(PropertyName, 'DateTimeDef') = 0) then
  begin
    Result := '';
    if theCol.DateTimeDef <> Nil then
       Result := theCol.DateTimeDef.Name;
    gdProperties.CellButtonType[2, forRow] := btCombo;
  end
  else
  case GetPropertyType(theCol, PropertyName) of
    ptSpin    : gdProperties.CellButtonType[2, forRow] := btVertSpin;
    ptCombo   :
        begin
          gdProperties.CellButtonType[2, forRow] := btCombo;
          if (PropertyName = 'HeadingImage') then
             Result := theCol.HeadingImage
          else
             Result := GetEnumProp(theCol, PropertyName);
        end;
    ptBoolean :
        begin
          Result := GetEnumProp(theCol, PropertyName);
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
             (theCol <> Nil) and
             (theCol.Tag > 0) then
          begin
            if not ColorToIdent(theCol.Tag, Result) then
               Result := ColorToString(theCol.Tag)
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
            Result := SpinOptionToText(theCol.SpinOptions);
            gdProperties.CellButtonType[2, forRow] := btNormal;
          end;
        end;
  end;
end;

procedure TdgOsCombo.RefreshColProperties;
var
  PropertyIndex,
  PropertyCount, iIndex, iRow : Integer;
  PropList : TPropList;
  sName, sText : String;

  function ValidProperty(propName : String) : Boolean;
  begin
    Result := (AnsiSameText(propName, 'CheckBoxValues')) or
              (AnsiSameText(propName, 'Color')) or
              (AnsiSameText(propName, 'ControlType')) or
              (AnsiSameText(propName, 'DisplayCol')) or
              (AnsiSameText(propName, 'DisplayFormat')) or
              (AnsiSameText(propName, 'FieldName')) or
              (AnsiSameText(propName, 'Font')) or
              (AnsiSameText(propName, 'Heading')) or
              (AnsiSameText(propName, 'HeadingColor')) or
              (AnsiSameText(propName, 'HeadingImage')) or
              (AnsiSameText(propName, 'HeadingImageAlignment')) or
              (AnsiSameText(propName, 'HeadingVertAlignment')) or
              (AnsiSameText(propName, 'HeadingWordWrap')) or
              (AnsiSameText(propName, 'HorzAlignment')) or
              (AnsiSameText(propName, 'Is3D')) or
              (AnsiSameText(propName, 'ShowTextEllipsis')) or
              (AnsiSameText(propName, 'TextFormatting')) or
              (AnsiSameText(propName, 'Visible')) or
              (AnsiSameText(propName, 'WordWrap')) or
              (AnsiSameText(propName, 'Width'));
  end;
begin
  if cbColumn.ItemIndex < 0 then exit;

  FColPropertiesRefresh := True;
  gdProperties.BeginUpdate;
  try
    PropertyCount := GetPropList(Combo.ComboGrid.Col[cbColumn.ItemIndex+1].ClassInfo, tkProperties, @PropList);
    gdProperties.Rows := 20;
    iRow := 1;
    for PropertyIndex := 0 to PropertyCount -1 do
    begin
      sName := PropList[PropertyIndex].Name;
      if ValidProperty(sName) then
      begin
        sText := GetColPropertyValueText(sName, iRow);
        gdProperties.Cell[1, iRow] := sName;
        if (sName = 'Visible') and
           (Combo.ComboGrid.Col[cbColumn.ItemIndex+1].Tag = 1) then
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

procedure TdgOsCombo.cbColumnClick(Sender: TObject);
begin
  if (cbColumn.ItemIndex >= 0) then
     RefreshColProperties;
end;

procedure TdgOsCombo.gdPropertiesCellChanged(Sender: TObject; OldCol,
  NewCol, OldRow, NewRow: Integer);
begin
  if not FColPropertiesRefresh then
     FEditingProperty := gdProperties.Cell[1, NewRow];
end;

procedure TdgOsCombo.gdPropertiesEndCellEdit(Sender: TObject; DataCol,
  DataRow: Integer; var Cancel: Boolean);
begin
  BindColProperty(DataRow);
end;

procedure TdgOsCombo.BindColProperty(DataRow : Integer);
var theCol : TosDbCol;
begin
  theCol := Combo.ComboGrid.Col[cbColumn.Text];
  if (theCol = Nil) then exit;
  
  if GetPropertyType(theCol, gdProperties.Cell[1, DataRow]) = ptColor then
     SetPropValue(theCol, gdProperties.Cell[1, DataRow], StringToColor(FullColorName(gdProperties.Cell[2, DataRow])))
  else
  begin
    if (AnsiCompareText(gdProperties.Cell[1, DataRow], 'ReadOnly') = 0) then
    begin
      FSelecting := True;
      try
        SetPropValue(theCol, gdProperties.Cell[1, DataRow], gdProperties.Cell[2, DataRow]);
      finally
        FSelecting := False;
      end;
    end
    else
       SetPropValue(theCol, gdProperties.Cell[1, DataRow], gdProperties.Cell[2, DataRow]);
  end;
end;

procedure TdgOsCombo.gdPropertiesComboDropDown(Sender: TObject;
  Combo: TtsComboGrid; DataCol, DataRow: Integer);
begin
  PropertyComboDropDown(Combo, gdProperties, Self.Combo.ComboGrid.Col[cbColumn.Text], DataRow, EnumList);
end;

procedure TdgOsCombo.gdPropertiesComboCellLoaded(Sender: TObject;
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
    Value := EnumList.Strings[DataRow-1];
  {else if (Combo.Tag = 6) and
          (FSourceGrid.ImageList <> Nil) then
    Value := FSourceGrid.ImageList.Image[DataRow-1].Name;}
end;

procedure TdgOsCombo.gdPropertiesComboGetValue(Sender: TObject;
  Combo: TtsComboGrid; GridDataCol, GridDataRow, ComboDataRow: Integer;
  var Value: Variant);
begin
  Value := Combo.Cell[1, ComboDataRow];
  gdProperties.Cell[GridDataCol, GridDataRow] := Value;
  BindColProperty(GridDataRow);;
end;

procedure TdgOsCombo.gdPropertiesComboInit(Sender: TObject;
  Combo: TtsComboGrid; DataCol, DataRow: Integer);
begin
  Combo.Width := gdProperties.Col[2].Width - 15;
  Combo.Color := clWindow;
  Combo.DropDownStyle := ddDropDownList;
  Combo.GridLines := glNone;
  Combo.AutoSearch := asTop;
end;

procedure TdgOsCombo.gdPropertiesButtonClick(Sender: TObject; DataCol,
  DataRow: Integer);
var theFont : TFont;
    theCol : TosDbCol;
begin
  theCol := Combo.ComboGrid.Col[cbColumn.Text];
  if (theCol = Nil) then exit;
  
  if (AnsiSameText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'Font')) then
  begin
    theFont := TFont.Create;
    try
      if (theCol.ParentFont) then
         theFont.Assign(Combo.ComboGrid.Font)
      else
         theFont.Assign(theCol.Font);
      dgFont.Font.Assign(theFont);
      if (dgFont.Execute) then
      begin
        if (theCol.ParentFont) then
           theCol.AssignFont;
        theCol.Font.Assign(dgFont.Font);
      end;
    finally
      theFont.Free;
    end;
  end
  else if (AnsiSameText(gdProperties.Cell[1, gdProperties.CurrentDataRow], 'HeadingFont')) then
  begin
    theFont := TFont.Create;
    try
      if (theCol.HeadingParentFont) then
         theFont.Assign(theCol.Font)
      else
         theFont.Assign(theCol.HeadingFont);
      dgFont.Font.Assign(theFont);
      if (dgFont.Execute) then
      begin
        if (theCol.HeadingParentFont) then
           theCol.HeadingParentFont := False;
        theCol.HeadingFont.Assign(dgFont.Font);
      end;
    finally
      theFont.Free;
    end;      
  end;
end;

procedure TdgOsCombo.gdPropertiesDblClick(Sender: TObject);
var theCol : TosDbCol;
begin
  theCol := Combo.ComboGrid.Col[cbColumn.Text];
  if (theCol = Nil) then exit;
  
  if LowerCase(gdProperties.Cell[gdProperties.CurrentDataCol, gdProperties.CurrentDataRow]) = 'true' then
  begin
     gdProperties.Cell[gdProperties.CurrentDataCol, gdProperties.CurrentDataRow] := 'False';
     BindColProperty(gdProperties.CurrentDataRow);
  end
  else if LowerCase(gdProperties.Cell[gdProperties.CurrentDataCol, gdProperties.CurrentDataRow]) = 'false' then
  begin
     gdProperties.Cell[gdProperties.CurrentDataCol, gdProperties.CurrentDataRow] := 'True';
     BindColProperty(gdProperties.CurrentDataRow);
  end
  else if LowerCase(gdProperties.Cell[1, gdProperties.CurrentDataRow]) = 'color' then
  begin
     dgColor.Color := theCol.Color;
     if (dgColor.Execute) then
     begin
       theCol.Color := dgColor.Color;
       gdProperties.Cell[2, gdProperties.CurrentDataRow] := ColorToString(dgColor.Color);
     end;
  end
  else if LowerCase(gdProperties.Cell[1, gdProperties.CurrentDataRow]) = 'headingcolor' then
  begin
     dgColor.Color := theCol.HeadingColor;
     if (dgColor.Execute) then
     begin
       theCol.HeadingColor := dgColor.Color;
       gdProperties.Cell[2, gdProperties.CurrentDataRow] := ColorToString(dgColor.Color);
     end;
  end;
end;

procedure TdgOsCombo.pgComboResize(Sender: TObject);
begin
  gbGrid.Height := tsLayout.Height - 29;
  gbColProperties.Height := gbGrid.Height;
  gbGrid.Width := Round(tsLayout.Width/2) - 25;
  gbColProperties.Width := gbGrid.Width;
  gbColProperties.Left  := gbGrid.Left + gbGrid.Width + 19;
end;

procedure TdgOsCombo.tsPropertiesExit(Sender: TObject);
begin
  if (Combo <> Nil) then
  begin
    Combo.AutoAdvance   := chAutoAdvance.Checked;
    Combo.AutoDropDown  := chAutoDropdown.Checked;
    Combo.DropDownRows  := udDropdownRows.Position;
    Combo.DropDownStyle := TtsDropDownStyle(cbStyle.ItemIndex);
    Combo.AutoSearch    := TtsComboAutoSearchType(cbAutoSearch.ItemIndex);
    Combo.AutoFill      := chAutoFill.Checked;
    Combo.AutoLookup    := chAutoLookup.Checked;
    Combo.CompareType   := TtsComboCompareType(cbCompare.ItemIndex);
    Combo.ComboGrid.Cols := udCols.Position;
    Combo.DropDownCols   := udDropDownCols.Position;
    if (cbValueCol.ItemIndex >= 0) then
       Combo.ValueCol := cbValueCol.ItemIndex + 1;
    Combo.ValueColSorted :=  chValueColSorted.Checked;
  end;
end;

end.
