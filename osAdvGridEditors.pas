unit osAdvGridEditors;

interface

{$INCLUDE TSCmpVer}
//{$DEFINE rtTest}
                  
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, TypInfo, ExtCtrls, Grids_ts, Registry,
  ComCtrls, ToolWin, DB, DBTables, ImgList, Menus, TSGrid, TSCommon, TsMask,
  propertyDescs, TSImageList, Buttons, osColorComboBox, TsDateTimeDef, osAdvDbGrid
  {$IFNDEF rtTest}
    {$IFDEF TSVER_V6}
       , Variants, ValEdit, DesignIntf, DesignEditors, VCLEditors, TsImageListEditor
    {$ELSE}
       , DsgnIntf, TSImagelistEditor
    {$ENDIF}
  {$ELSE}
    {$IFDEF TSVER_V6}, Variants, ValEdit {$ENDIF}
  {$ENDIF};

{$IFNDEF rtTest}
type
  TosAdvGridColumnEditor = class(TComponentEditor)
  public
    procedure GetDatasourceName(const sValue : String);
    procedure GetDateTimeDefName(const sValue : String);
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
  end;
{$ENDIF}

var
  lsDataSources, lsDateTimeDefs, srcFieldNames : TStringList;
  SourceGrid : TosAdvDbGrid = nil;
  SaveResult, StartedFromDesigntime, FirstActivate, DesignActivatesFirstTime : Boolean;
  PrevLeft, PrevTop, PrevWidth, PrevHeight : Integer;
  PrevState : TWindowState;

implementation

uses duEditTgColumns;

{$IFNDEF rtTest}

procedure TosAdvGridColumnEditor.GetDateTimeDefName(const sValue : String);
var aDateTimeDef : TComponent;
begin
  aDateTimeDef := Designer.GetComponent(sValue);
  lsDateTimeDefs.AddObject(sValue, aDateTimeDef)
end;

procedure TosAdvGridColumnEditor.GetDatasourceName(const sValue : String);
var aDs : TComponent;
begin
  aDs := Designer.GetComponent(sValue);
  lsDataSources.AddObject(sValue, aDs);
end;

procedure TosAdvGridColumnEditor.Edit;
var i : integer;
    pInfo : PPropInfo;
    T: PTypeData;
    theDataSet : TDataSet;
begin
    SaveResult := False;
    StartedFromDesigntime := True;
    FirstActivate := True;
    srcFieldNames := TStringList.Create;
    lsDataSources := TStringList.Create;
    lsDateTimeDefs := TStringList.Create;
    Screen.Cursor := crHourglass;
    try
        SourceGrid := TosAdvDbGrid(Component);
        dgEditTgColumns := TdgEditTgColumns.Create(Application);
    except
        Screen.Cursor := crDefault;
        dgEditTgColumns.Free;
        dgEditTgColumns := nil;
        raise;
    end;

    if SourceGrid is TosAdvDbGrid then
    begin
      {$IFNDEF TSVER_V5}
      pInfo := GetPropInfo(PTypeInfo(TosAdvDbGrid(SourceGrid).ClassInfo), 'DataSource');
      {$ELSE}
      pInfo := GetPropInfo(TosAdvDbGrid(SourceGrid), 'DataSource');
      {$ENDIF}
      T := GetTypeData(pInfo^.PropType^);
      Designer.GetComponentNames(T, GetDatasourceName);
    end;
    // Now Load DateTimeDefs
    {$IFNDEF TSVER_V5}
    pInfo := GetPropInfo(PTypeInfo(TosAdvDbGrid(SourceGrid).ClassInfo), 'DateTimeDef');
    {$ELSE}
    pInfo := GetPropInfo(TosAdvDbGrid(SourceGrid), 'DateTimeDef');
    {$ENDIF}
    T := GetTypeData(pInfo^.PropType^);
    Designer.GetComponentNames(T, GetDateTimeDefName);
          
    try
{$IFDEF TSVER_V6}
        dgEditTgColumns.Caption := TForm(Designer.Root).Name + '.' + Component.Name + ' - Column Editor';
{$ELSE}
        dgEditTgColumns.Caption := Designer.Form.Name + '.' + Component.Name + ' - Column Editor';
{$ENDIF}
        if DesignActivatesFirstTime then
        begin
            dgEditTgColumns.Left := Trunc((Screen.Width - dgEditTgColumns.Width)/2);
            dgEditTgColumns.Top  := Trunc((Screen.Height - dgEditTgColumns.Height)/2);

            PrevLeft := dgEditTgColumns.Left;
            PrevTop   := dgEditTgColumns.Top;
            PrevWidth := dgEditTgColumns.Width;
            PrevHeight := dgEditTgColumns.Height;

            DesignActivatesFirstTime := False;
        end
        else
        begin
            dgEditTgColumns.Left := PrevLeft;
            dgEditTgColumns.Top := PrevTop;
        end;

        if (SourceGrid is TosAdvDbGrid) and
           (TosAdvDbGrid(SourceGrid).DataSource <> Nil) then
        begin
          if TosAdvDbGrid(SourceGrid).DataSource.DataSet = Nil then
             raise Exception.Create('Unable to open TopGrid Designer - no dataset associated to TDatasource!');
           for i := 0 to TosAdvDbGrid(SourceGrid).DataSource.DataSet.FieldCount - 1 do
              srcFieldNames.Add(TosAdvDbGrid(SourceGrid).DataSource.DataSet.Fields.Fields[i].FieldName);
        end;
        
        //ShowMessage('ShowModal');
        SaveResult := True;  // temporary
        theDataSet := Nil;
        if (SourceGrid.DataSource <> Nil) and
           (SourceGrid.DataSource.DataSet <> Nil) then
           theDataSet := SourceGrid.DataSource.DataSet;
        if dgEditTgColumns.Execute(SourceGrid, theDataSet) then
           Designer.Modified;
       { if (not SaveResult) and
           (dgEditTgColumns.tbSave.Enabled) and
           (MessageDlg('You have unapplied changes - apply them?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
           SaveResult := True; }

        //ShowMessage('Exiting Editor');

        PrevState := dgEditTgColumns.WindowState;
        if PrevState = wsNormal then
        begin
            PrevLeft := dgEditTgColumns.Left;
            PrevTop := dgEditTgColumns.Top;
            PrevWidth := dgEditTgColumns.Width;
            PrevHeight := dgEditTgColumns.Height;
        end;

    finally
        srcFieldNames.Free;
        srcFieldNames := Nil;
        lsDateTimeDefs.Free;
        lsDateTimeDefs := Nil;
        lsDataSources.Free;
        lsDataSources := Nil;
        dgEditTgColumns.Free;
        dgEditTgColumns := Nil;

        Screen.Cursor := crDefault;
    end;
end;

function TosAdvGridColumnEditor.GetVerbCount: Integer;
begin
    Result := 1;
end;

function TosAdvGridColumnEditor.GetVerb(Index: integer): string;
begin
    result := '&Column Editor...'
end;

procedure TosAdvGridColumnEditor.ExecuteVerb(Index: integer);
begin
    Edit;
end;
{$ENDIF}


end.
 