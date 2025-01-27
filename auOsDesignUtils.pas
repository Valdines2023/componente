unit auOsDesignUtils;

interface

{$INCLUDE TSCmpVer}
//{$DEFINE rtTest}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, osAdvDbGrid, StdCtrls, Buttons, ExtCtrls, db, DBTables, ComCtrls,
  Grids_ts, TSGrid, ImgList, tsCommon, TypInfo, TsDateTimeDef
  {$IFNDEF rtTest}
    {$IFDEF TSVER_V6}
       , DesignIntf, DesignEditors, VCLEditors, TsImageListEditor
    {$ELSE}
       , DsgnIntf, TSImagelistEditor
    {$ENDIF}
  {$ELSE}
  {$ENDIF}
  {$IFDEF TSVER_V6}, Variants, ValEdit, osColorComboBox, Menus {$ENDIF}
  ;

type
  TosPropertyType = (ptEdit, ptSpin, ptBoolean, ptCombo, ptColor, ptButton);

{$IFNDEF TSVER_V5}
function GetPropValue(Instance: TObject; const PropName: string;
  PreferStrings: Boolean = True): Variant;
procedure SetPropValue(Instance: TObject; const PropName: string;
  const Value: Variant);
function PropType(Instance: TObject; const PropName: string): TTypeKind;
function GetEnumProp(Instance: TObject; const PropName: string): string;
function GetEnumProp2(Instance: TObject; PropInfo: PPropInfo): string;
function PropertyIsBoolean(Instance: TObject; const PropName: string) : Boolean;
{$ENDIF}
procedure CustomColumnDbAssign(tgtGrid, srcGrid : TosAdvDbGrid);
procedure CustomColumnAssign(tgtGrid, srcGrid : TtsGrid);
procedure CustomComboAssign(tgtCol, srcCol : TosDbCol);
function BooleanToStr(Value : Boolean) : String;
function SpinOptionToText(Value : TtsSpinOptions) : String;
function TestColorProperty(PropertyName : String) : Boolean;
function TestBooleanProperty(OnObject : TObject; PropertyName : String) : Boolean;
function GetPropertyType(OnObject : TObject; PropertyName : String) : TosPropertyType;
function LoadEnumList(EnumList : TStringList; anObject : TPersistent; sProperty : String) : Integer;
function FullColorName(colorValue : String) : String;
procedure PropertyComboDropDown(Combo: TtsComboGrid; onGrid : TtsGrid; onCol : TosDbCol; DataRow : Integer; EnumList : TStringList);


implementation


procedure CustomColumnDbAssign(tgtGrid, srcGrid : TosAdvDbGrid);
var
  PropertyIndex, PropertyCount, iCol : Integer;
  PropList      : TPropList;
  sName : String;
  Value : Variant;
  aCol : TosDbCol;
  Columns : TStringList;
  bAutoSize : Boolean;
begin
  tgTraceEntry('CustomColumnDbAssign Start...');
  bAutoSize := tgtGrid.ColumnOptions.AutoSizeColumns;
  tgtGrid.ColumnOptions.AutoSizeColumns := False;
  tgtGrid.BeginUpdate;
  Columns := TStringList.Create;
  try
    tgtGrid.Cols := 0;
    tgtGrid.Cols := srcGrid.Cols;
    if tgtGrid.Cols = 0 then exit;
    
    PropertyCount := GetPropList(srcGrid.Col[1].ClassInfo, tkProperties, @PropList);
    // Load the columns by displaycol to sort by the displayCol...
    for iCol := 1 to srcGrid.Cols do
      Columns.AddObject(Format('%8.7d', [srcGrid.Col[iCol].DisplayCol]), srcGrid.Col[iCol]);
    Columns.Sort;
    for iCol := 1 to srcGrid.Cols do
    begin
      aCol := TosDbCol(Columns.Objects[iCol-1]);
      tgTraceEntry('Column ' + aCol.FieldName);
      for PropertyIndex := 0 to PropertyCount - 1 do
      begin
        sName := PropList[PropertyIndex].Name;
        if aCol.Tag = 1 then
           tgtGrid.Col[iCol].Visible := False;
        if (AnsiCompareText(sName, 'Font') = 0) then
        begin
          if (aCol.ParentFont = False) then
          begin
            tgtGrid.Col[iCol].ParentFont := False;
            tgtGrid.Col[iCol].Font.Assign(aCol.Font);
          end
          else Continue;
        end
        else if (AnsiCompareText(sName, 'DateTimeDef') = 0) then
          tgtGrid.Col[iCol].DateTimeDef := aCol.DateTimeDef
        else if (AnsiCompareText(sName, 'HeadingFont') = 0) then
        begin
          if (aCol.HeadingParentFont = False) then
          begin
            tgtGrid.Col[iCol].HeadingParentFont := False;
            tgtGrid.Col[iCol].HeadingFont.Assign(aCol.HeadingFont);
          end
          else Continue;
        end
        else if (sName = 'ComboSQL') then
        begin
          Continue;
          //tgtGrid.Col[iCol].ComboSQL := aCol.ComboSQL;
        end
        else if (sName <> 'ComboDatasource') and (sName <> 'DateTimeDef') and
                (sName <> 'DisplayCol') then
           try
             tgTraceEntry('Column ' + aCol.FieldName + ' SetProperty - ' + sName);
             Value := GetPropValue(aCol, sName, False);
             if VarType(Value) <> varEmpty then
             begin
               if VarType(Value) = varBoolean then
                  SetPropValue(tgtGrid.Col[iCol], sName, BooleanToStr(Value))
               else
                  SetPropValue(tgtGrid.Col[iCol], sName, Value);
             end;
           except on E:Exception do
             //ShowMessage('Error setting col property ' + sName);
           end;
      end; // Property assignments...
      // Now Combo properties if present...
      if aCol.ButtonType = btCombo then
         CustomComboAssign(tgtGrid.Col[iCol], aCol);
    end;
  finally
    Columns.Free;
    tgtGrid.ColumnOptions.AutoSizeColumns := bAutoSize;
    tgtGrid.EndUpdate;
    tgtGrid.Invalidate;
    tgTraceEntry('CustomColumnDbAssign End...', True);
  end;
end;

procedure CustomComboAssign(tgtCol, srcCol : TosDbCol);
var jCol, dataRow, dataCol : Integer;
    srcCombo, tgtCombo : TosDbCombo;
begin
  if (srcCol.Combo <> Nil) then
  begin
    tgTraceEntry('CustomComboAssign Start...');
    srcCombo := srcCol.Combo;
    tgtCol.AssignCombo;
    tgtCombo := tgtCol.Combo;
    srcCombo.RefreshComboData := False;
    tgtCombo.Assign(srcCombo);
    tgtCombo.Datasource := srcCombo.Datasource;
    tgtCombo.RefreshComboData := srcCombo.RefreshComboData;
    tgtCombo.SQL.Clear;
    if (srcCombo.SQL.Count > 0) then
       tgtCombo.SQL.AddStrings(srcCombo.SQL);
    for jCol := 1 to srcCombo.ComboGrid.Cols do
      tgtCol.Combo.ComboGrid.Col[jCol].Assign(srcCombo.ComboGrid.Col[jCol]);
    if srcCombo.ComboGrid.StoreData and
       (srcCombo.DataSource = Nil) and
       (srcCombo.SQL.Count = 0) then
    begin
       tgtCombo.ComboGrid.Rows := srcCombo.ComboGrid.Rows;
       tgtCombo.ComboGrid.Cols := srcCombo.ComboGrid.Cols;
       for dataRow := 1 to srcCombo.ComboGrid.Rows do
          for dataCol := 1 to srcCombo.ComboGrid.Cols do
            tgtCombo.ComboGrid.Cell[dataCol, dataRow] := srcCombo.ComboGrid.Cell[dataCol, dataRow];
    end;
    tgtCombo.RefreshComboData := True;  // TP Added Mar 5, 2005 for Designer to load combo data
    tgTraceEntry('CustomComboAssign End...');
  end
  else
  begin
    tgtCol.ParentCombo := True;
    tgtCol.ResetCombo;
  end;
end;


procedure CustomColumnAssign(tgtGrid, srcGrid : TtsGrid);
var
  PropertyIndex, PropertyCount, iCol, jCol, dataCol, dataRow : Integer;
  PropList      : TPropList;
  sName : String;
  Value : Variant;
  aCol : TtsCol;
  Columns : TStringList;
begin
  tgtGrid.BeginUpdate;
  Columns := TStringList.Create;
  try
    tgtGrid.Cols := 0;
    tgtGrid.Cols := srcGrid.Cols;
    if tgtGrid.Cols = 0 then exit;
    
    PropertyCount := GetPropList(srcGrid.Col[1].ClassInfo, tkProperties, @PropList);
    // Load the columns by displaycol to sort by the displayCol...
    for iCol := 1 to srcGrid.Cols do
      Columns.AddObject(Format('%8.7d', [srcGrid.Col[iCol].DisplayCol]), srcGrid.Col[iCol]);
    Columns.Sort;
    for iCol := 1 to srcGrid.Cols do
    begin
      aCol := TosDbCol(Columns.Objects[iCol-1]);
      for PropertyIndex := 0 to PropertyCount - 1 do
      begin
        sName := PropList[PropertyIndex].Name;
        if aCol.Tag = 1 then
           tgtGrid.Col[iCol].Visible := False;
        if (AnsiCompareText(sName, 'Font') = 0) then
        begin
          if (aCol.ParentFont = False) then
          begin
            tgtGrid.Col[iCol].ParentFont := False;
            tgtGrid.Col[iCol].Font.Assign(aCol.Font);
          end
          else Continue;
        end
        else if (AnsiCompareText(sName, 'DateTimeDef') = 0) then
          tgtGrid.Col[iCol].DateTimeDef := aCol.DateTimeDef
        else if (AnsiCompareText(sName, 'HeadingFont') = 0) then
        begin
          if (aCol.HeadingParentFont = False) then
          begin
            tgtGrid.Col[iCol].HeadingParentFont := False;
            tgtGrid.Col[iCol].HeadingFont.Assign(aCol.HeadingFont);
          end
          else Continue;
        end
        else if (sName <> 'ComboDatasource') and (sName <> 'DateTimeDef') and
                (sName <> 'DisplayCol') then
           try
             Value := GetPropValue(aCol, sName, False);
             if VarType(Value) <> varEmpty then
             begin
               if VarType(Value) = varBoolean then
                  SetPropValue(tgtGrid.Col[iCol], sName, BooleanToStr(Value))
               else
                  SetPropValue(tgtGrid.Col[iCol], sName, Value);
             end;
           except on E:Exception do
             //ShowMessage('Error setting col property ' + sName);
           end;
      end; // Property assignments...
      // Now Combo properties if present...
      if aCol.ButtonType = btCombo then
      begin
        if (aCol.Combo <> Nil) then
        begin
          tgtGrid.Col[iCol].AssignCombo;
          tgtGrid.Col[iCol].Combo.Assign(aCol.Combo);
          for jCol := 1 to aCol.Combo.ComboGrid.Cols do
            tgtGrid.Col[iCol].Combo.ComboGrid.Col[jCol].Assign(aCol.Combo.ComboGrid.Col[jCol]);
          if aCol.Combo.ComboGrid.StoreData then
             for dataRow := 1 to aCol.Combo.ComboGrid.Rows do
                for dataCol := 1 to aCol.Combo.ComboGrid.Cols do
                  tgtGrid.Col[iCol].Combo.ComboGrid.Cell[dataCol, dataRow] := aCol.Combo.ComboGrid.Cell[dataCol, dataRow];
        end;
      end;
    end;
  finally
    Columns.Free;
    tgtGrid.EndUpdate;
    tgtGrid.Invalidate;
  end;
end;

function BooleanToStr(Value : Boolean) : String;
begin
  if Value then
     Result := 'True'
  else
     Result := 'False';
end;

{$IFNDEF TSVER_V5}

function PropType(Instance: TObject; const PropName: string): TTypeKind;
var pInfo : PPropInfo;
begin
  pInfo := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);
  Result := pInfo^.PropType^^.Kind;
end;

function GetPropValue(Instance: TObject; const PropName: string;
  PreferStrings: Boolean = True): Variant;
var pInfo : PPropInfo;
    sEnumText : String;
    iVal : Integer;
begin
  pInfo := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);
  if pInfo = Nil then
     raise Exception.Create('Unable to locate property ' + PropName);
  if (PropType(Instance, PropName) = tkEnumeration) then
  begin
     iVal := GetOrdProp(Instance, pInfo);
     sEnumText := GetEnumName(pInfo^.PropType^, iVal);
     Result := sEnumText;
  end
  else if (PropType(Instance, PropName) = tkInteger) then
     Result := GetOrdProp(Instance, pInfo)
  else if (PropType(Instance, PropName) = tkClass) then
     Result := GetOrdProp(Instance, pInfo)
  else if (PropType(Instance, PropName) = tkLString) or
          (PropType(Instance, PropName) = tkWString) then
     Result := GetStrProp(Instance, pInfo)
  else
     Result := GetVariantProp(Instance, pInfo);
end;

procedure SetPropValue(Instance: TObject; const PropName: string;
  const Value: Variant);
var pInfo : PPropInfo;
    iEnumValue : Integer;
begin
  pInfo := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);
  if pInfo = Nil then
     raise Exception.Create('Unable to locate property ' + PropName);
  if (PropType(Instance, PropName) = tkEnumeration) then
  begin
    iEnumValue := GetEnumValue(pInfo^.PropType^, Value);
    SetOrdProp(Instance, pInfo, iEnumValue)
  end
  else if (PropType(Instance, PropName) = tkInteger) then
     SetOrdProp(Instance, pInfo, Value)
  else if (PropType(Instance, PropName) = tkClass) then
     SetOrdProp(Instance, pInfo, Value)
  else if (PropType(Instance, PropName) = tkLString) or
          (PropType(Instance, PropName) = tkWString) then
     SetStrProp(Instance, pInfo, Value)
  else
     SetVariantProp(Instance, pInfo, Value);
end;

function GetEnumProp(Instance: TObject; const PropName: string): string;
var pInfo : PPropInfo;
begin
  pInfo := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);
  if pInfo = Nil then
     raise Exception.Create('Unable to locate property ' + PropName);
  Result := GetEnumProp2(Instance, pInfo);
end;

function GetEnumProp2(Instance: TObject; PropInfo: PPropInfo): string;
begin
  Result := GetEnumName(PropInfo^.PropType^, GetOrdProp(Instance, PropInfo));
end;

function PropertyIsBoolean(Instance: TObject; const PropName: string) : Boolean;
var pInfo : PPropInfo;
begin
  pInfo := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);
  if pInfo = Nil then
     raise Exception.Create('Unable to locate property ' + PropName);
  Result := (pInfo^.PropType^^.Name = 'Boolean');
end;

{$ENDIF}

function SpinOptionToText(Value : TtsSpinOptions) : String;
begin
  Result := '[';
  if spoAutoRepeat in Value then
     Result := Result + 'AutoRepeat,';
  if spoAutoIncrement in Value then
     Result := Result + 'AutoIncrement,';
  if spoKeyEdit in Value then
     Result := Result + 'KeyEdit,';
  if Result = '[' then
     Result := ''
  else
  begin
    Delete(Result, Length(Result), 1);
    Result := Result + ']';
  end;
end;

function TestColorProperty(PropertyName : String) : Boolean;
begin
  if (PropertyName = 'Color') or
     (PropertyName = 'HeadingColor') or
     (PropertyName = 'SelectionColor') or
     (PropertyName = 'SelectionFontColor') or
     (PropertyName = 'LineColor') then
     Result := True
  else
     Result := False;
end;

function TestBooleanProperty(OnObject : TObject; PropertyName : String) : Boolean;
var sValue : String;
begin
  Result := False;
  if PropType(OnObject, PropertyName) = tkEnumeration then
  begin
    sValue := GetEnumProp(OnObject, PropertyName);
    Result := (LowerCase(sValue) = 'true') or (Lowercase(sValue) = 'false');
  end;
end;

function GetPropertyType(OnObject : TObject; PropertyName : String) : TosPropertyType;
begin
  if TestColorProperty(PropertyName) then
     Result := ptColor
  else if TestBooleanProperty(OnObject, PropertyName) then
     Result := ptBoolean
  else if (PropType(OnObject, PropertyName) = tkEnumeration) or
          (PropertyName = 'HeadingImage') then
     Result := ptCombo
  else if (PropertyName = 'DateTimeDef') then
     Result := ptCombo
  else if PropType(OnObject, PropertyName) = tkClass then
     Result := ptButton
  else if PropType(OnObject, PropertyName) = tkInteger then
     Result := ptSpin
  else
     Result := ptEdit;
end;

function LoadEnumList(EnumList : TStringList; anObject : TPersistent; sProperty : String) : Integer;
var pInfo : PPropInfo;
    sCode, sName : String;
    Value, iCount : Integer;
begin
  EnumList.Clear;
  Result := 0;
  if sProperty = '' then exit;
  {$IFNDEF TSVER_V5}
  pInfo := GetPropInfo(PTypeInfo(anObject.ClassInfo), sProperty);
  {$ELSE}
  pInfo := GetPropInfo(anObject, sProperty);
  {$ENDIF}
  if pInfo.PropType^.Kind = tkEnumeration then
  begin
    //T := GetTypeData(@pInfo^.PropType^);
    iCount := GetTypeData(pInfo^.PropType^).MaxValue;
    for Value := 0 to iCount do
    begin
      sName := GetEnumName(pInfo^.PropType^, Value);
      if (Value = 0) or
         ((Value > 0) and
          (Copy(sName, 1, 2) = sCode)) then
         EnumList.Add(sName);
      if Value = 0 then
         sCode := Copy(sName, 1, 2);
    end;
    Result := EnumList.Count;
  end;
end;

function FullColorName(colorValue : String) : String;
begin
  if Lowercase(Copy(colorValue, 1, 2)) <> 'cl'  then
     Result := 'cl' + colorValue
  else
     Result := colorValue;
end;

procedure PropertyComboDropDown(Combo: TtsComboGrid; onGrid : TtsGrid; onCol : TosDbCol; DataRow : Integer; EnumList : TStringList);
var sPropName, sValue : String;
begin
  sPropName := onGrid.Cell[1, DataRow];
  Combo.Tag := 0;
  if GetPropertyType(onCol, sPropName) = ptColor then
  begin
    Combo.Tag := 2;
    Combo.Rows := 42;
    Combo.DropDownRows := 12;
  end
  else if GetPropertyType(onCol, sPropName) = ptBoolean then
  begin
    Combo.Tag := 1;
    Combo.Rows := 2;
    if LowerCase(sValue) = 'true' then
       Combo.CurrentDataRow := 1
    else
       Combo.CurrentDataRow := 2;
   end
   else if GetPropertyType(onCol, sPropName) = ptCombo then
   begin
     Combo.Rows := LoadEnumList(EnumList, onCol, sPropName);
     if Combo.Rows <= 8 then
        Combo.DropDownRows := Combo.Rows
     else
        Combo.DropDownRows := 8;
     sValue := GetEnumProp(onCol, sPropName);
     Combo.CurrentDataRow := EnumList.IndexOf(sValue) + 1;
     Combo.Tag := 3;
   end;
end;

end.
