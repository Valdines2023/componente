unit duTgMemo;

interface

  { Notes
      1. Need image for size handle at bottom right
      2. Need smaller ok cancel buttons
      3. No header on form
      4. Cursor changes to resize over sizer image
      5. MaxHeight and MaxWidth properties on memo window
      6. FitRowHeightToText property on grid
      7. Scrollbar management on memo
   }

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, tsGrid, ComCtrls, Menus;

type
  TdgTgMemo = class(TForm)
    Panel2: TPanel;
    moText: TMemo;
    shLeft: TShape;
    shRight: TShape;
    shTop: TShape;
    shBottom: TShape;
    Panel1: TPanel;
    igSizer: TImage;
    btOk: TButton;
    btCancel: TButton;
    procedure Panel1Resize(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure moTextChange(Sender: TObject);
    procedure igSizerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure igSizerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure moTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FGrid : TtsGrid;
    FX, FY : Integer;
    FAdjusted, FReady : Boolean;

  public
    { Public declarations }
    procedure Provide(grid : TtsGrid; DataCol, DataRow : Integer; CanEdit : Boolean);
  end;

var
  dgTgMemo: TdgTgMemo;

implementation

{$R *.DFM}

procedure TdgTgMemo.Provide(grid : TtsGrid; DataCol, DataRow : Integer; CanEdit : Boolean);

  procedure AutoAdjustHeight;
  var iLineHt : Integer;
      bm : TBitmap;
  begin
    bm := TBitmap.Create;
    try
      bm.Width := 100;
      bm.Height := 100;
      bm.Canvas.Font.Assign(motext.Font);
      moText.PaintTo(bm.Handle, 0, 0);
      iLineHt := bm.Canvas.TextHeight('A');
    finally
      bm.Free;
    end;
    Self.Height := moText.Lines.Count * (iLineHt + 2) + Panel1.Height + 5;
  end;
  procedure Set3D;
  begin
    Panel2.BorderWidth := 1;
    Panel2.BorderStyle := bsSingle;
    Panel2.BevelOuter := bvRaised;
    //moText.BorderStyle := bsSingle;
    Panel1.BevelInner := bvRaised;

    shTop.Visible := False;
    shBottom.Visible := False;
    shLeft.Visible := False;
    shRight.Visible := False;
  end;
begin
  FReady := False;
  FGrid := grid;
  moText.Lines.Clear;
  moText.Lines.Text := grid.CurrentCell.Value;
  moText.Color := grid.Color;
  moText.Font.Assign(grid.Font);
  Self.Left   := grid.ClientOrigin.x + grid.CellRect(DataCol, DataRow).Left;
  Self.Top    := grid.ClientOrigin.y + grid.CellRect(DataCol, DataRow).Top;
  Self.Constraints.MaxHeight := Round(Screen.Height * 0.8);
  Self.Constraints.MaxWidth  := Round(Screen.Width * 0.8);
  if (moAutoSize in grid.MemoEditorOptions) then
  begin
    AutoAdjustHeight;
    Self.Width  := grid.Col[DataCol].Width;
  end
  else if (not FAdjusted) then
  begin
    if (Self.Height < grid.RowHeight[DataRow]) then
       Self.Height := grid.RowHeight[DataRow]
    else if (Self.Height > grid.RowHeight[DataRow]) and
            (Self.Height > 120) then
       Self.Height := 120;  
    Self.Width  := grid.Col[DataCol].Width;
  end;
  if (grid.PopupBorders = pb3d)then
     Set3D;
                            
  //if (Self.Top + Self.Height > Screen.WorkAreaHeight) then
 //    Self.Top := Screen.WorkAreaHeight - Self.Height - 3;
  if (Self.Top < 0) then
     Self.Top := 1;
  moText.ReadOnly := not CanEdit;
  moText.SelStart := grid.CurrentCell.SelStart;
  if (grid.CurrentCell.SelLength > 0) then
     motext.SelLength := grid.CurrentCell.SelLength;
  btOk.Enabled := False;
  Show;
  FReady := True;
end;

procedure TdgTgMemo.Panel1Resize(Sender: TObject);
begin
  btCancel.Left := Panel1.Width - igSizer.Width - btCancel.Width - 5;
  btOk.Left := btCancel.Left - btOk.Width - 2;
  if (shTop.Visible) then
  begin
    igSizer.Top  := Panel1.Height - igSizer.Height;
    igSizer.Left := Panel1.Width - igSizer.Width;
  end
  else
  begin
    igSizer.Top  := Panel1.Height - igSizer.Height - 1;
    igSizer.Left := Panel1.Width - igSizer.Width - 3;
  end;
end;

procedure TdgTgMemo.btOkClick(Sender: TObject);
begin
  FGrid.CurrentCell.Value := moText.Text;
  //FGrid.RowHeight[FGrid.CurrentDataRow] := FGrid.CellTextHeight[2, FGrid.CurrentDataRow];
  Self.Hide;
  FReady := False;
end;

procedure TdgTgMemo.FormDeactivate(Sender: TObject);
begin
  Self.Hide;
  FReady := False;
end;

procedure TdgTgMemo.btCancelClick(Sender: TObject);
begin
  Self.Hide;
  FReady := False;
end;

procedure TdgTgMemo.moTextChange(Sender: TObject);
begin
  btOk.Enabled := True;
end;

procedure TdgTgMemo.igSizerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var thePt : TPoint;
begin
  thePt := igSizer.ClientToScreen(Point(X, Y));
  FX := thePt.X;
  FY := thePt.Y;
end;

procedure TdgTgMemo.igSizerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var thePt : TPoint;
begin
  if (ssLeft in Shift) then
  begin
    thePt := igSizer.ClientToScreen(Point(X, Y));
    if (thePt.X > FX) then
    begin
      Self.Width := Self.Width + thePt.X - FX;
      FX := thePt.X;
      FAdjusted := True;
    end
    else if (thePt.X < FX) then
    begin
      Self.Width := Self.Width - (FX - thePt.X);
      FX := thePt.X;
      FAdjusted := True;
    end;
    if (thePt.Y > FY) then
    begin
      Self.Height := Self.Height + thePt.Y - FY;
      FY := thePt.Y;
      FAdjusted := True;
    end
    else if (thePt.Y < FY) then
    begin
      Self.Height := Self.Height - (FY - thePt.Y);
      FY := thePt.Y;
      FAdjusted := True;
    end;
    if (FAdjusted) then motext.SetFocus;
  end;
end;

procedure TdgTgMemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
  FReady := False;
end;

procedure TdgTgMemo.FormShow(Sender: TObject);
begin
  if (not moText.ReadOnly) then
     moText.SetFocus;
end;

procedure TdgTgMemo.moTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var memoKey : Word;
    memoShift : TShiftState;
begin
  if FReady and
     (FGrid.MemoEditorShortCut > 0) and
     (Key > 0) then
  begin
    ShortCutToKey(FGrid.MemoEditorShortCut, memoKey, memoShift);
    if (memoShift = Shift) and (memoKey = Key) then
    begin
      if (moAutoSave in FGrid.MemoEditorOptions) then
         FGrid.CurrentCell.Value := moText.Text;
      Close;
    end;
  end;
end;

end.
