unit osEditButton;

interface

uses
  Windows, Buttons, Classes, Controls, ExtCtrls, Forms,
  Graphics, Menus, Messages, StdCtrls, SysUtils;

const
  MsgClose = WM_USER+0;
  MsgOpen  = WM_USER+1;

type
  TosEdButton = class(TBitBtn)
  public
     procedure Click; override;
  end;

  TosPopupEvent = procedure(Sender : TObject) of object;
  TosPopupAnchor = (paLeft, paRight);

  TosEditButton = class(TCustomEdit)
  protected
    FButton        : TosEdButton;
    FButtonGlyph   : TBitmap;
    FPopupActive   : Boolean;
    FPopupAnchor   : TosPopupAnchor;
    FOnPopupClose  : TosPopupEvent;
    FOnPopupOpen   : TosPopupEvent;
    FShowButton    : Boolean;

    {property methods}
    function GetButtonGlyph : TBitmap;
    procedure SetButtonGlyph(Value : TBitmap);
    procedure SetShowButton(Value : Boolean);

    {internal methods}
    function GetButtonWidth : Integer;                                                              {!!.04}

  protected
    procedure CreateParams(var Params : TCreateParams); override;
    procedure CreateWnd; override;
    function GetButtonEnabled : Boolean; dynamic;
    procedure GlyphChanged; dynamic;
    procedure Loaded; override;

    procedure OnMsgClose(var M : TMessage); message MsgClose;
    procedure OnMsgOpen(var M : TMessage); message MsgOpen;

    property PopupAnchor : TosPopupAnchor read FPopupAnchor write FPopupAnchor;
    property ShowButton : Boolean read FShowButton write SetShowButton;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure PopupClose(Sender : TObject); dynamic;
    procedure PopupOpen; dynamic;

    property PopupActive : Boolean read FPopupActive;
    //property Controller;

  published

    property OnPopupClose : TosPopupEvent read FOnPopupClose write FOnPopupClose;
    property OnPopupOpen : TosPopupEvent read FOnPopupOpen write FOnPopupOpen;
    property ButtonGlyph : TBitmap read GetButtonGlyph write SetButtonGlyph;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;  
  end;


implementation


{*** TosEdButton ***}

procedure TosEdButton.Click;
begin
  TosEditButton(Parent).PopupOpen;
end;


{*** TosEditButton ***}

constructor TosEditButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];

  FShowButton := True;
  FButton := TosEdButton.Create(Self);
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.Caption := '';
  FButton.TabStop := False;
  FButton.Style := bsNew;

  FButtonGlyph := TBitmap.Create;
end;

procedure TosEditButton.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TosEditButton.CreateWnd;
begin
  inherited CreateWnd;

  SetBounds(Left, Top, Width, Height);

  FButton.Enabled := GetButtonEnabled;
end;


destructor TosEditButton.Destroy;
begin
  FButton.Free;
  FButton := nil;

  FButtonGlyph.Free;
  FButtonGlyph := nil;

  inherited Destroy;
end;

function TosEditButton.GetButtonEnabled : Boolean;
begin
  Result := not ReadOnly;
end;

function TosEditButton.GetButtonWidth : Integer;
begin
  if FShowButton then
  begin
    Result := GetSystemMetrics(SM_CXHSCROLL);
    if Assigned(FButtonGlyph) and not FButtonGlyph.Empty then
      if FButtonGlyph.Width + 4 > Result then
        Result := FButtonGlyph.Width + 4;
  end
  else
    Result := 0;
end;

function TosEditButton.GetButtonGlyph : TBitmap;
begin
  if not Assigned(FButtonGlyph) then
     FButtonGlyph := TBitmap.Create;

  Result := FButtonGlyph
end;

procedure TosEditButton.GlyphChanged;
begin
end;

procedure TosEditButton.Loaded;
begin
  inherited Loaded;

  if Assigned(FButtonGlyph) then
    FButton.Glyph.Assign(FButtonGlyph);
end;

procedure TosEditButton.OnMsgClose(var M : TMessage);
begin
  if (Assigned(FOnPopupClose)) then
    FOnPopupClose(Self);
end;

procedure TosEditButton.OnMsgOpen(var M : TMessage);
begin
  if (Assigned(FOnPopupOpen)) then
    FOnPopupOpen(Self);
end;


procedure TosEditButton.PopupClose;
begin
 // if (Assigned(FOnPopupClose)) then
 //    FOnPopupClose(Self);
  FPopupActive := False;
  PostMessage(Handle, MsgClose, 0, 0);
end;

procedure TosEditButton.PopupOpen;
begin
  if (Assigned(FOnPopupOpen)) then
     FOnPopupOpen(Self);
  FPopupActive := True;
  PostMessage(Handle, MsgOpen, 0, 0);
end;

procedure TosEditButton.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
var
  H : Integer;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if not HandleAllocated then
    Exit;

  if not FShowButton then
  begin
    FButton.Height := 0;
    FButton.Width := 0;
    Exit;
  end;

  H := ClientHeight;
  if BorderStyle = bsNone then
  begin
    FButton.Height := H;
    FButton.Width := GetButtonWidth;
    FButton.Left := Width - FButton.Width;
    FButton.Top := 0;
  end
  else if Ctl3D then
  begin
    FButton.Height := H;
    FButton.Width := GetButtonWidth;
    FButton.Left := Width - FButton.Width - 4;
    FButton.Top := 0;
  end
  else
  begin
    FButton.Height := H - 2;
    FButton.Width := GetButtonWidth;
    FButton.Left := Width - FButton.Width - 1;
    FButton.Top := 1;
  end;
end;

procedure TosEditButton.SetButtonGlyph(Value : TBitmap);
begin
  if not Assigned(FButtonGlyph) then
    FButtonGlyph := TBitmap.Create;

  if not Assigned(Value) then
  begin
    FButtonGlyph.Free;
    FButtonGlyph := TBitmap.Create;
  end
  else
    FButtonGlyph.Assign(Value);

  GlyphChanged;

  FButton.Glyph.Assign(FButtonGlyph);
  SetBounds(Left, Top, Width, Height);
end;

procedure TosEditButton.SetShowButton(Value : Boolean);
begin
  if Value <> FShowButton then
  begin
    FShowButton := Value;
    SetBounds(Left, Top, Width, Height);
  end;
end;


end.
