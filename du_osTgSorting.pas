unit du_osTgSorting;

{$INCLUDE TSCmpVer}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tsGrid, osAdvDbGrid, osSortLib, db,
  Buttons, TsCommon
  {$IFDEF TSVER_V6}, Variants {$ENDIF};

type
  TdgTgSort = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    laGroup1: TLabel;
    Label2: TLabel;
    cbGroup1: TComboBox;
    cbGroup2: TComboBox;
    cbSegment1: TComboBox;
    Label3: TLabel;
    cbSegment2: TComboBox;
    Label4: TLabel;
    Panel2: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    cbSort1: TComboBox;
    cbSort2: TComboBox;
    Label9: TLabel;
    cbSort3: TComboBox;
    Label7: TLabel;
    btClear: TButton;
    chDescending1: TCheckBox;
    chDescending2: TCheckBox;
    chDescending3: TCheckBox;
    btClearG1: TSpeedButton;
    btClearG2: TSpeedButton;
    btClearS1: TSpeedButton;
    btClearS2: TSpeedButton;
    btClearS3: TSpeedButton;
    chFooters: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure cbGroup2DropDown(Sender: TObject);
    procedure cbGroup1Change(Sender: TObject);
    procedure cbGroup2Change(Sender: TObject);
    procedure cbSort1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbGroup1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbGroup2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btClearClick(Sender: TObject);
    procedure btClearG1Click(Sender: TObject);
    procedure btClearG2Click(Sender: TObject);
    procedure btClearS1Click(Sender: TObject);
    procedure btClearS2Click(Sender: TObject);
    procedure btClearS3Click(Sender: TObject);
    procedure chFootersClick(Sender: TObject);
    procedure cbSort1Change(Sender: TObject);
    procedure cbSegment1Change(Sender: TObject);
    procedure cbSegment2Change(Sender: TObject);
    procedure chDescending1Click(Sender: TObject);
  private
    { Private declarations }
    FGrid : TosAdvDbGrid;
    FBinding, FChanged : Boolean;

    procedure BindFromGrid;
    procedure BindToGrid;
    function  DataColForIndex(iIndex : Integer) : Integer;
    procedure LoadCombo(theCombo : TComboBox);
    procedure AdjustSegment(columnCombo, segmentCombo : TComboBox);
  public
    { Public declarations }
    function Execute(forGrid: TosAdvDbGrid): Boolean;
  end;

var
  dgTgSort: TdgTgSort;

implementation

{$R *.dfm}

{ TdgTgSort }

procedure TdgTgSort.BindFromGrid;
var i, j : Integer;

  procedure SetIndex(onCombo : TComboBox; sFldName : String);
  begin
    onCombo.ItemIndex := -1;
    if (FGrid.LocateColumn(sFldName) <> Nil) then
    begin
      onCombo.ItemIndex := FGrid.Col[sFldName].DisplayCol - 1;
      if (onCombo = cbSort1) then
         chDescending1.Checked := (FGrid.Col[onCombo.ItemIndex+1].SortPicture = spUp)
      else if (onCombo = cbSort2) then
         chDescending2.Checked := (FGrid.Col[onCombo.ItemIndex+1].SortPicture = spUp)
      else
         chDescending3.Checked := (FGrid.Col[onCombo.ItemIndex+1].SortPicture = spUp);
    end;
  end;
begin
  FBinding := True;
  try
    LoadCombo(cbGroup1);
    LoadCombo(cbGroup2);
    LoadCombo(cbSort1);
    LoadCombo(cbSort2);
    LoadCombo(cbSort3);
    chFooters.Checked := FGrid.GridData.SortedData.FootersOn;
  finally
    FBinding := False;
  end;

  for i := 0 to FGrid.GroupFieldCount - 1 do
  begin
    if (i <= 2) and
       (FGrid.SortEntry[i].GroupSort) then
      case i of
        0 : begin
              cbGroup1.ItemIndex := FGrid.SortEntry[i].Column - 1;
              if (FGrid.Col[FGrid.SortEntry[i].Column].GroupDateTimeSegment <> dgoNone) then
                 cbSegment1.ItemIndex := Integer(FGrid.Col[FGrid.SortEntry[i].Column].GroupDateTimeSegment);
            end;
        1 : begin
              cbGroup2.ItemIndex := FGrid.SortEntry[i].Column - 1;
              if (FGrid.Col[FGrid.SortEntry[i].Column].GroupDateTimeSegment <> dgoNone) then
                 cbSegment1.ItemIndex := Integer(FGrid.Col[FGrid.SortEntry[i].Column].GroupDateTimeSegment);
              cbSegment2.Enabled := (cbSegment1.ItemIndex >= 0) or
                                    ((FGrid.Col[FGrid.SortEntry[i].Column].DatasetField <> Nil) and
                                     (FGrid.Col[FGrid.SortEntry[i].Column].DatasetField.DataType = ftDateTime));
              if (cbSegment2.Enabled) then cbSegment2.Color := clWindow
              else cbSegment2.Color := clBtnFace;
            end;
      end;
  end;
  AdjustSegment(cbGroup1, cbSegment1);
  AdjustSegment(cbGroup2, cbSegment2);
  j := 1;
  for i := 0 to FGrid.GroupSortCount - 1 do
  begin
    if (not FGrid.SortEntry[i].GroupSort) and
       (j <= 3) then
    begin
      case j of
        1 : SetIndex(cbSort1, FGrid.SortEntry[i].FieldName);
        2 : SetIndex(cbSort2, FGrid.SortEntry[i].FieldName);
        3 : SetIndex(cbSort3, FGrid.SortEntry[i].FieldName);
      end;
      Inc(j);
    end;
  end;
end;

function TdgTgSort.DataColForIndex(iIndex : Integer) : Integer;
begin
  Result := FGrid.DataColnr[iIndex+1];
end;

procedure TdgTgSort.BindToGrid;
begin
  // SortFields list must be reset and properties on the
  // columns themselves must be reset...
  FGrid.BeginUpdate;
  FGrid.GridData.SortedData.BeginUpdate;
  try
    FGrid.ClearAll;
    if (cbGroup1.ItemIndex >= 0) then
    begin
      FGrid.Col[DataColForIndex(cbGroup1.ItemIndex)].GroupSortOp := gsGroup;
      if (cbSegment1.ItemIndex >= 0) then
         FGrid.Col[DataColForIndex(cbGroup1.ItemIndex)].GroupDateTimeSegment := TosDateTimeSegment(cbSegment1.ItemIndex);
    end;
    if (cbGroup2.ItemIndex >= 0) then
    begin
      FGrid.Col[DataColForIndex(cbGroup2.ItemIndex)].GroupSortOp := gsGroup;
      if (cbSegment2.ItemIndex >= 0) then
         FGrid.Col[DataColForIndex(cbGroup2.ItemIndex)].GroupDateTimeSegment := TosDateTimeSegment(cbSegment2.ItemIndex);
    end;
    if (cbSort1.ItemIndex >= 0) then
    begin
      FGrid.Col[DataColForIndex(cbSort1.ItemIndex)].GroupSortOp := gsSort;
      if (chDescending1.Checked) then
      begin
        FGrid.Col[DataColForIndex(cbSort1.ItemIndex)].SortPicture := spUp;
        FGrid.SortEntryForCol(DataColForIndex(cbSort1.ItemIndex)).SortMode := stDescending;
      end
      else
         FGrid.Col[DataColForIndex(cbSort1.ItemIndex)].SortPicture := spDown;
    end;
    if (cbSort2.ItemIndex >= 0) then
    begin
      FGrid.Col[DataColForIndex(cbSort2.ItemIndex)].GroupSortOp := gsSort;
      if (chDescending2.Checked) then
      begin
        FGrid.Col[DataColForIndex(cbSort2.ItemIndex)].SortPicture := spUp;
        FGrid.SortEntryForCol(DataColForIndex(cbSort2.ItemIndex)).SortMode := stDescending;
      end
      else
         FGrid.Col[DataColForIndex(cbSort2.ItemIndex)].SortPicture := spDown;
    end;
    if (cbSort3.ItemIndex >= 0) then
    begin
      FGrid.Col[DataColForIndex(cbSort3.ItemIndex)].GroupSortOp := gsSort;
      if (chDescending3.Checked) then
      begin
        FGrid.Col[DataColForIndex(cbSort3.ItemIndex)].SortPicture := spUp;
        FGrid.SortEntryForCol(DataColForIndex(cbSort3.ItemIndex)).SortMode := stDescending;
      end
      else
         FGrid.Col[DataColForIndex(cbSort3.ItemIndex)].SortPicture := spDown;
    end;
    if (FGrid.GroupFieldCount > 0) then
       FGrid.GridData.SortedData.FootersOn := chFooters.Checked;
  finally
    FGrid.GridData.SortedData.EndUpdate;
    FGrid.ApplyGroupingSorting;
    FGrid.EndUpdate;
  end;
end;

function TdgTgSort.Execute(forGrid: TosAdvDbGrid): Boolean;
begin
  FGrid := forGrid;
  FChanged := False;
  BindFromGrid;
  Result := False;
  if (ShowModal = mrOk) and
     (FChanged) then
  begin
     BindToGrid;
     Result := True;
  end;
end;

procedure TdgTgSort.LoadCombo(theCombo : TComboBox);
var i : Integer;
begin
  theCombo.Items.Clear;
  for i := 1 to FGrid.cols do
  begin
    if (FGrid.Col[i].Heading <> '') then
       theCombo.Items.Add(FGrid.Col[FGrid.DataColnr[i]].Heading)
    else if (FGrid.Col[i].FieldName <> '') then
       theCombo.Items.Add(FGrid.Col[FGrid.DataColnr[i]].FieldName)
    else
       theCombo.Items.Add('Col ' + IntToStr(FGrid.DataColnr[i]));
  end;
end;

procedure TdgTgSort.FormShow(Sender: TObject);
begin
  cbGroup1.SetFocus;
end;

procedure TdgTgSort.cbGroup2DropDown(Sender: TObject);
begin
  if (cbGroup1.ItemIndex = -1) then
     cbGroup2.DroppedDown := False;
end;

procedure TdgTgSort.AdjustSegment(columnCombo, segmentCombo : TComboBox);
var theDbCol : TosDbCol;
begin
  if (columnCombo.ItemIndex = -1) then
  begin
    segmentCombo.ItemIndex := -1;
    segmentCombo.Enabled := False;
  end
  else
  begin
    theDbCol := FGrid.Col[FGrid.DataColnr[columnCombo.ItemIndex + 1]];
    if (theDbCol.DataSetField <> Nil) then
{$IFDEF TSVER_V6}
       segmentCombo.Enabled := ((theDbCol.DataSetField.DataType = ftDateTime) or
                                (theDbCol.DataSetField.DataType = ftDate) or
                                (theDbCol.DataSetField.DataType = ftTimestamp))
{$ELSE}
       segmentCombo.Enabled := ((theDbCol.DataSetField.DataType = ftDateTime) or
                                (theDbCol.DataSetField.DataType = ftDate))
{$ENDIF}
    else
       segmentCombo.Enabled := (theDbCol.DataType = dyDate);
    if (theDbCol.GroupDateTimeSegment = dgoNone) then
       segmentCombo.ItemIndex := -1
    else
       segmentCombo.ItemIndex := Integer(theDbCol.GroupDateTimeSegment);
  end;
  if (segmentCombo.Enabled) then
     segmentCombo.Color := clWindow
  else
  begin
    segmentCombo.Color := clBtnFace;
    segmentCombo.ItemIndex := -1;
  end
end;

procedure TdgTgSort.cbGroup1Change(Sender: TObject);
begin
  if not FBinding then
  begin
     AdjustSegment(cbGroup1, cbSegment1);
     FChanged := True;
  end;
end;

procedure TdgTgSort.cbGroup2Change(Sender: TObject);
begin
  if not FBinding then
  begin
     AdjustSegment(cbGroup2, cbSegment2);
     FChanged := True;
  end;
end;

procedure TdgTgSort.cbSort1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Delete) then
  begin
    TComboBox(Sender).ItemIndex := -1;
    FChanged := True;
    Key := 0;
  end;
end;

procedure TdgTgSort.cbGroup1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Delete) then
  begin
    cbGroup1.ItemIndex := -1;
    FChanged := True;
    AdjustSegment(cbGroup1, cbSegment1);
    Key := 0;
  end;
end;

procedure TdgTgSort.cbGroup2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Delete) then
  begin
    cbGroup2.ItemIndex := -1;
    FChanged := True;
    AdjustSegment(cbGroup2, cbSegment2);
    Key := 0;
  end;
end;

procedure TdgTgSort.btClearClick(Sender: TObject);
begin
  cbGroup1.ItemIndex := -1;
  AdjustSegment(cbGroup1, cbSegment1);
  cbGroup2.ItemIndex := -1;
  AdjustSegment(cbGroup2, cbSegment2);
  cbSort1.ItemIndex := -1;
  cbSort2.ItemIndex := -1;
  cbSort3.ItemIndex := -1;
  chDescending1.Checked := False;
  chDescending2.Checked := False;
  chDescending3.Checked := False;
  chFooters.Checked := False;
  FChanged := True;
end;

procedure TdgTgSort.btClearG1Click(Sender: TObject);
begin
  cbGroup1.ItemIndex := -1;
  if not FBinding then
  begin
     AdjustSegment(cbGroup1, cbSegment1);
     FChanged := True;
  end;
end;

procedure TdgTgSort.btClearG2Click(Sender: TObject);
begin
  cbGroup2.ItemIndex := -1;
  if not FBinding then
  begin
     AdjustSegment(cbGroup2, cbSegment2);
     FChanged := True;
  end;
end;

procedure TdgTgSort.btClearS1Click(Sender: TObject);
begin
  cbSort1.ItemIndex := -1;
  chDescending1.Checked := False;
  FChanged := True;
end;

procedure TdgTgSort.btClearS2Click(Sender: TObject);
begin
  cbSort2.ItemIndex := -1;
  chDescending2.Checked := False;
  FChanged := True;
end;

procedure TdgTgSort.btClearS3Click(Sender: TObject);
begin
  cbSort3.ItemIndex := -1;
  chDescending3.Checked := False;
  FChanged := True;  
end;

procedure TdgTgSort.chFootersClick(Sender: TObject);
begin
  if (not FBinding) then
     FChanged := True;
end;

procedure TdgTgSort.cbSort1Change(Sender: TObject);
begin
  if not FBinding then
     FChanged := True;
end;

procedure TdgTgSort.cbSegment1Change(Sender: TObject);
begin
  if not FBinding then
     FChanged := True;
end;

procedure TdgTgSort.cbSegment2Change(Sender: TObject);
begin
  if not FBinding then
     FChanged := True;
end;

procedure TdgTgSort.chDescending1Click(Sender: TObject);
begin
  if not FBinding then
     FChanged := True;
end;

end.
