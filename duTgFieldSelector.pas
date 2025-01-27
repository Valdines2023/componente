unit duTgFieldSelector;

{$INCLUDE TSCmpVer}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, Grids_ts, TSGrid, math
  {$IFDEF TSVER_V6} , StrUtils, Variants {$ENDIF};

type
  TdgTgFieldSelector = class(TForm)
    Panel2: TPanel;
    laMsg: TLabel;
    gdFields : TtsGrid;        
    procedure btCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure gdFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure gdFieldsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gdFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure gdFieldsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure gdFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDragColumns : TDragImageList;
    FWindowHandle : HWND;
    FGrid : TtsBaseGrid;
    FX, FY : Integer;
    FDragColumn : String;
    FDragDataCol : Integer;

    function XOffSet : Integer;
    function YOffSet : Integer;
  public
    { Public declarations }

    procedure RemoveColumn(columnNo : Integer);

    property DragColumn : String read FDragColumn;
    property DragDataCol : Integer read FDragDataCol;
    property DragColumns : TDragImageList read FDragColumns write FDragColumns;
    property WindowHandle : HWND read FWindowHandle write FWindowHandle;
    property Grid : TtsBaseGrid read FGrid write FGrid;
  end;

var
  dgTgFieldSelector: TdgTgFieldSelector;

implementation

{$R *.dfm}

procedure TdgTgFieldSelector.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TdgTgFieldSelector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TdgTgFieldSelector.FormDestroy(Sender: TObject);
begin
  dgTgFieldSelector := Nil;
end;

procedure TdgTgFieldSelector.RemoveColumn(columnNo : Integer);
var i : Integer;
begin
  for i := 1 to gdFields.Rows do
    if (gdFields.CellTag[1,i] = columnNo) then
    begin
      gdFields.DeleteRows(i,i);
      FDragColumn := '';
      FDragDataCol := 0;
      break;
    end;
end;

procedure TdgTgFieldSelector.gdFieldsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  DragColumns.EndDrag;
end;

procedure TdgTgFieldSelector.gdFieldsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aPoint : TPoint;
begin
  GetCursorPos(aPoint);
  FX := aPoint.X;
  FY := aPoint.Y;
end;

procedure TdgTgFieldSelector.gdFieldsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  DragColumns.DragMove(Self.Left + gdFields.Left + X - XOffSet, Self.Top + gdFields.Top + Y - YOffset);
end;

procedure TdgTgFieldSelector.Panel1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  DragColumns.DragMove(Self.Left + TControl(Sender).Left + X - XOffSet, Self.Top + TControl(Sender).Top + Y - YOffSet);
  Accept := False;
end;

procedure TdgTgFieldSelector.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  DragColumns.EndDrag;
end;

function TdgTgFieldSelector.XOffSet : Integer;
begin
  Result := GetParentForm(FGrid).Left;
end;

function TdgTgFieldSelector.YOffSet : Integer;
begin
  Result := GetParentForm(FGrid).Top;
end;

procedure TdgTgFieldSelector.gdFieldsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var cellRect, textRect : TRect;
    dragBitmap : TBitmap;
    aPoint : TPoint;
    dCol, dRow : Integer;

  function travelDistance : Integer;
  begin
    Result := Round(sqrt(Power(Abs(aPoint.X-FX),2) + Power(Abs(aPoint.Y-FY),2)));
  end;
begin
  if (not (ssLeft in Shift)) then exit;

  GetCursorPos(aPoint);
  if (travelDistance < 10) then exit;
  aPoint := Self.ScreenToClient(Point(FX, FY));
  gdFields.CellFromXY(aPoint.X-gdFields.Left, aPoint.Y-gdFields.Top, dCol, dRow);
  if (dCol <> 1) or (dRow <= 0) or (dRow > gdFields.Rows) then exit;

  gdFields.BeginDrag(True);
  dragBitmap := TBitmap.Create;
  try
    textRect.Left := 0; textRect.Top := 0;
    textRect.Bottom := gdFields.DefaultRowHeight;
    textRect.Right  := 125;
    cellRect := gdFields.CellRect(1, dRow);
    cellRect.Right := cellRect.Left + 125;
    dragBitmap.Height := gdFields.DefaultRowHeight;
    dragBitmap.Width  := 125;
    dragBitmap.Canvas.CopyRect(textRect, gdFields.Canvas, cellRect);

    DragColumns.EndDrag;
    DragColumns.Clear;
    DragColumns.Width  := dragBitmap.Width;
    DragColumns.Height := dragBitmap.Height;
    DragColumns.Add(dragBitmap, Nil);
    DragColumns.SetDragImage(0, dragBitmap.Width div 2, dragBitmap.Height div 2);
    DragColumns.BeginDrag(FWindowHandle, aPoint.X - XOffSet, aPoint.Y - YOffset) ;
    DragColumns.DragCursor := crDrag;
    FDragColumn  := gdFields.Cell[1, dRow];
    FDragDataCol := gdFields.CellTag[1, dRow];
  finally
    dragBitmap.Free;
  end;
end;

procedure TdgTgFieldSelector.FormDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  DragColumns.DragMove(Self.Left + X - XOffSet, Self.Top + Y - YOffSet);
  Accept := False;
end;

procedure TdgTgFieldSelector.gdFieldsEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  DragColumns.EndDrag;
  FGrid.Invalidate;
end;

procedure TdgTgFieldSelector.FormCreate(Sender: TObject);
begin
  FDragDataCol := 0;
  FDragColumn := '';
end;

end.
