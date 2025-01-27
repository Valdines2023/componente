unit osEditCalendar;

{$INCLUDE TSCmpVer}

interface

uses
  Windows, Buttons, Classes, Controls, Forms, Graphics,
  Menus, Messages, StdCtrls, SysUtils, db, dbCtrls,
  osEditButton, TsDateTime, TsDateTimeDef;

const
  tsMonthConvertError = 501;
  tsMonthNameConvertError = 502;
  tsMonthRequired = 503;
  tsYearConvertError = 504;
  tsYearRequired = 505;
  tsDayConvertError = 506;
  tsDayRequired = 507;
  tsHourConvertError = 508;
  tsMinConvertError = 509;
  tsSecConvertError = 510;

  tsInvalidDay   = 'Invalid day entered';
  tsInvalidMonth = 'Invalid month number entered';
  tsInvalidMonthName = 'Invalid month name entered';
  tsInvalidYear  = 'Invalid year entered';
  tsDayRequiredMsg = 'Day entry required';
  tsMonthRequiredMsg = 'Month entry required';
  tsYearRequiredMsg = 'Year entry required';
  tsInvalidHour   = 'Invalid hour entered';
  tsInvalidMin    = 'Invalid minutes entered';
  tsInvalidSec    = 'Invalid seconds entered';

type
  EosException = class(Exception);

  TosDateOrder = (doMDY, doDMY, doYMD);
  TosRequiredDateField = (rfYear, rfMonth, rfDay);
  TosRequiredDateFields = set of TosRequiredDateField;

  TosGetDateEvent = procedure(Sender : TObject; var Value : string) of object;
  TosGetDateMaskEvent = procedure(Sender : TObject; var Mask : string) of object;

  TosCalendar = class(TCustomForm)
  private
    FDateTimeDef : TtsDateTimeDef;
  protected
    FDropDownForm: Boolean;

    procedure Deactivate; override;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;

    function  SetChildStyleOff: Integer;
    function  SetChildStyleOn: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;

    function GetCalendarControl : TtsDateTime;
    procedure SetDateTime(Value : TDateTime);
    function  GetDateTime : TDateTime;
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;

    procedure Close; 

    property CalendarControl : TtsDateTime read GetCalendarControl;
    property DateTime : TDateTime read GetDateTime write SetDateTime;
    property DropDownForm: Boolean read FDropDownForm write FDropDownForm default True;
  end;

  TosCustomTgDateEdit = class(TosEditButton)
  protected
    FAllowIncDec         : Boolean;
    FDateTimeDef         : TtsDateTimeDef;
    FCalendar            : TosCalendar;
    FDate                : TDateTime;
    FEpoch               : Integer;
    FForceCentury        : Boolean;
    FRequiredFields      : TosRequiredDateFields;
    FTodayString         : string;

    {event variables}
    FOnGetDate           : TosGetDateEvent;
    FOnSetDate           : TNotifyEvent;

    {internal variables}
    DateOrder            : TosDateOrder;
    HoldCursor           : TCursor;
    PopupClosing         : Boolean;
    WasAutoScroll        : Boolean;

    {property methods}
    function GetDate : TDateTime;
    function GetEpoch : Integer;
    function GetReadOnly : Boolean;
    procedure SetEpoch(Value : Integer);
    procedure SetForceCentury(Value : Boolean);
    procedure SetReadOnly(Value : Boolean);
    procedure SetDateTimeDef(Value : TtsDateTimeDef);
    {internal methods}
    procedure PopupSetDate(Sender : TObject); virtual;
    function  ParseDate(const Value : string) : string;
    procedure PopupDateChange(Sender : TObject; Date : TDateTime);
    procedure PopupKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure PopupKeyPress(Sender : TObject; var Key : Char);
    procedure PopupMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoExit; override;
    procedure GlyphChanged; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure KeyPress(var Key : Char); override;
    procedure SetDate(Value : TDateTime); virtual;

    {protected properties}
    property AllowIncDec : Boolean read FAllowIncDec write FAllowIncDec;
    property Epoch : Integer read GetEpoch write SetEpoch;
    property ForceCentury : Boolean read FForceCentury write SetForceCentury;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    property RequiredFields : TosRequiredDateFields read FRequiredFields write FRequiredFields;
    property TodayString : string read FTodayString write FTodayString;

    {protected events}
    property OnGetDate : TosGetDateEvent read FOnGetDate write FOnGetDate;
    property OnSetDate : TNotifyEvent read FOnSetDate write FOnSetDate;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function DateString(const Mask : string) : string;
    function FormatDate(Value : TDateTime) : string; dynamic;
    procedure PopupClose(Sender : TObject); override;
    procedure PopupOpen; override;
    procedure SetDateText(Value : string); dynamic;

    {public properties}
    property Calendar : TosCalendar read FCalendar;
    property Date: TDateTime read GetDate write SetDate;
    property DateTimeDef : TtsDateTimeDef read FDateTimeDef write SetDateTimeDef;

  end;

  TosDbTgDateEdit = class(TosCustomTgDateEdit)
  private
    FDataLink: TFieldDataLink;
    FFocused: Boolean;
  protected
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;

    procedure DoExit; override;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;

    procedure Change; override;
    function EditCanModify: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset;
    procedure SetDate(Value : TDateTime); override;
    procedure PopupSetDate(Sender : TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PopupOpen; override;
    function FormatDate(Value : TDateTime) : string; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
      
    property DateTimeDef;
    property AllowIncDec;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property ButtonGlyph;
    property CharCase;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Epoch;
    property Font;
    property ForceCentury;
    property HideSelection;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RequiredFields;
    property ShowButton;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TodayString;
    property Visible;

    {inherited events}
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetDate;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPopupClose;
    property OnPopupOpen;
    property OnSetDate;

    property OnStartDrag;
  end;

  TosTgDateEdit = class(TosCustomTgDateEdit)
  published
    property DateTimeDef;
    property AllowIncDec;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property ButtonGlyph;
    property CharCase;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Epoch;
    property Font;
    property ForceCentury;
    property HideSelection;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RequiredFields;
    property ShowButton;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TodayString;
    property Visible;

    {inherited events}
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetDate;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPopupClose;
    property OnPopupOpen;
    property OnSetDate;

    property OnStartDrag;
  end;

implementation

{$R *.RES}
{$R *.dcr}

constructor TosCalendar.Create(Owner : TComponent);
begin
{$IFDEF TSVER_DELPHI}
    CreateNew(Owner);
{$ENDIF}

{$IFDEF TSVER_CBUILD}
    CreateNew(Owner, 1);
{$ENDIF}

  Parent := nil;

  AutoScroll := False;
  Width := 200;
  Height := 200;
  BorderStyle := bsNone;
    
  FDateTimeDef := TtsDateTimeDef.Create(Self);
  FDateTimeDef.DateTimeControl.Parent := Self;
  FDateTimeDef.DateTimeControl.ShowWeekNumbers := False;
  FDateTimeDef.DateTimeControl.ShowDayNames := sdnShortDayNames;
  FDateTimeDef.DateTimeControl.InitializeDisplay;
  FDateTimeDef.DateTimeControl.Show;
  Visible := False;
  Enabled := False;
  Ctl3D := False;
end;

destructor TosCalendar.Destroy;
begin
  FDateTimeDef.Free;
  inherited Destroy;
end;

procedure TosCalendar.Close;
begin
  TosCustomTgDateEdit(Self.Owner).PopupClose(Owner);
end;

procedure TosCalendar.SetDateTime(Value : TDateTime);
begin
  FDateTimeDef.DateTimeControl.DateTime := Value;
end;

function  TosCalendar.GetDateTime : TDateTime;
begin
  Result := FDateTimeDef.DateTimeControl.DateTime;
end;

function TosCalendar.GetCalendarControl : TtsDateTime;
begin
  Result := FDateTimeDef.DateTimeControl;
end;

procedure TosCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if DropDownForm then
  begin
      with Params do
      begin
        Style := 0;
        WndParent := 0;
        if not (csDesigning in Owner.ComponentState) then
            WndParent := TWinControl(Owner).Handle;
        Style := Style or WS_POPUP;
        WindowClass.Style := CS_DBLCLKS;
      end;
  end;
end;

procedure TosCalendar.CreateWnd;
var
    Style: Longint;
begin
    inherited CreateWnd;

    if DropDownForm then
    begin
        Style := WS_CHILD;
        Windows.SetWindowLong(Handle, GWL_STYLE, Style);
    end;
end;

function TosCalendar.SetChildStyleOff: Integer;
begin
    Result := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, Result and not WS_CHILD);
end;

function TosCalendar.SetChildStyleOn: Integer;
begin
    Result := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, Result or WS_CHILD);
end;

procedure TosCalendar.Deactivate;
begin
  inherited Deactivate;
  if Visible then
     Close;
end;

procedure TosCalendar.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
end;

procedure TosCalendar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var
    Style: Integer;
begin
    Style := SetChildStyleOff;
    try
        inherited;
    finally
        SetWindowLong(Handle, GWL_STYLE, Style);
    end;
end;

procedure TosCalendar.WMSize(var Message: TWMSize);
var
    Style: Integer;
begin
    Style := SetChildStyleOff;
    try
        inherited;
    finally
        SetWindowLong(Handle, GWL_STYLE, Style);
    end;
end;

procedure TosCalendar.WMMove(var Message: TWMMove);
var
    Style: Integer;
begin
    Style := SetChildStyleOff;
    try
        inherited;
    finally
        SetWindowLong(Handle, GWL_STYLE, Style);
    end;
end;

{*** TosCustomTgDateEdit ***}

constructor TosCustomTgDateEdit.Create(AOwner : TComponent);
var
  C : array[0..1] of Char;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];

  FEpoch               := 1950;
  FAllowIncDec         := True;
  FForceCentury        := False;
  FRequiredFields      := [rfMonth, rfDay];
  FTodayString         := DateSeparator;

  {get the date order from windows}
  C[0] := '0'; {default}
  GetProfileString('intl', 'iDate', '0', C, 2);
  DateOrder := TosDateOrder(Ord(C[0])-Ord('0'));

  {load button glyph}
  FButtonGlyph.LoadFromResourceName(HInstance, 'OSCLNDRCOMBO');
  FButton.Glyph.Assign(FButtonGlyph);

  FCalendar := TosCalendar.Create(Self);
  //FCalendar.OnChange     := PopupDateChange;
  FCalendar.OnExit       := PopupClose;
  FCalendar.OnKeyDown    := PopupKeyDown;
  FCalendar.OnKeyPress   := PopupKeyPress;
  FCalendar.OnMouseDown  := PopupMouseDown;
  FCalendar.Visible      := False;
  FCalendar.BorderStyle  := bsNone;
  FCalendar.ParentFont   := False;
  FCalendar.Parent       := GetParentForm(Self);                                                                     {!!.06}
  //FCalendar.Parent       := GetImmediateParentForm(Self);             
end;

destructor TosCustomTgDateEdit.Destroy;
begin
  FCalendar.Free;
  inherited Destroy;
end;

procedure TosCustomTgDateEdit.SetDateTimeDef(Value : TtsDateTimeDef);
begin
  if Value <> FDateTimeDef then
  begin
    FDateTimeDef := Value;
    if Assigned(Value) then
       Self.FCalendar.FDateTimeDef.Assign(Value);
  end;
end;

procedure TosCustomTgDateEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = Self.FDateTimeDef) then
       FDateTimeDef := Nil;
  end;
end;

procedure TosCustomTgDateEdit.DoExit;
begin
  try
    SetDateText(Text);
  except
    SetFocus;
    raise;
  end;

  if not PopupActive then
     inherited DoExit;
end;

function TosCustomTgDateEdit.DateString(const Mask : string) : string;
begin
 // Result := OvcIntlSup.DateToDateString(Mask, DateTimeToSTDate(Date), False);
end;

function TosCustomTgDateEdit.FormatDate(Value : TDateTime) : string;
begin
  Result := DateToStr(Value);
end;

function TosCustomTgDateEdit.GetDate : TDateTime;
begin
  SetDateText(Text);
  Result := FDate;
end;

function TosCustomTgDateEdit.GetEpoch : Integer;
begin
  Result := FEpoch;

  if (csWriting in ComponentState) then                            
    Exit;
end;

function TosCustomTgDateEdit.GetReadOnly : Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TosCustomTgDateEdit.GlyphChanged;
begin
  inherited GlyphChanged;

  if FButtonGlyph.Empty then
     FButtonGlyph.LoadFromResourceName(HInstance, 'OSCLNDRCOMBO');
end;

procedure TosCustomTgDateEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if ShowButton and (Key = VK_DOWN) and (ssAlt in Shift) then
    PopupOpen;
end;

procedure TosCustomTgDateEdit.KeyPress(var Key : Char);
begin
  inherited KeyPress(Key);

  if (ReadOnly) then Exit;
end;

function TosCustomTgDateEdit.ParseDate(const Value : string) : string;
var
  S           : string;
 { ThisYear    : Word;
  ThisMonth   : Word;
  ThisDay     : Word;
  DefaultDate : TDateTime;
  Increment   : Integer;
  Occurrence  : Integer;
  StartDate   : TDateTime;   }
begin
  {The following code provides the user the ability to enter dates
  using text descriptions.  All descriptions assume the current
  date as a reference date.  The following descriptions are currently
  supported:
    <day of week>              Next is assumed; may be abbreviated -- 1st 3 chars
    Next <day of week>
    Last                       current day of week is assumed
    Last <day of week>
    First  | 1st               current day of week is assumed
    First  | 1st <day of week>
    Second | 2nd               current day of week is assumed
    Second | 2nd <day of week>
    Third  | 3rd               current day of week is assumed
    Third  | 3rd <day of week>
    Fourth | 4th               current day of week is assumed
    Fourth | 4th <day of week>
    Final  | lst               current day of week is assumed
    Final  | lst <day of week>
    BOM    | Begin             returns first day of current month
    EOM    | End               returns last day of current month
    Yesterday                  returns yesterday's date
    Today                      returns today's date
    Tomorrow                   returns tomorrow's date}

  S := Uppercase(Value);
  {if Pos(GetOrphStr(tsCalYesterday), S) > 0 then
  begin
    Result := FormatDate(StDateToDateTime(DateTimeToStDate(SysUtils.Date) - 1));
  end else if Pos(GetOrphStr(SCCalToday), S) > 0 then
  begin
    Result := FormatDate(StDateToDateTime(DateTimeToStDate(SysUtils.Date)));
  end else if Pos(GetOrphStr(SCCalTomorrow), S) > 0 then
  begin
    Result := FormatDate(StDateToDateTime(DateTimeToStDate(SysUtils.Date) + 1));
  end else if Pos(GetOrphStr(SCCalNext), S) > 0 then
  begin
    Increment   := 1;
    Occurrence  := 1;
    StartDate   := DateTimeToStDate(SysUtils.Date);
    DefaultDate := StartDate + 7;
    DoSetDate;
  end else if Pos(GetOrphStr(SCCalLast), S) > 0 then
  begin
    Increment   := -1;
    Occurrence  := 1;
    StartDate   := DateTimeToStDate(SysUtils.Date);
    DefaultDate := StartDate - 7;
    DoSetDate;
  end else if Pos(GetOrphStr(SCCalPrev), S) > 0 then
  begin
    Increment   := -1;
    Occurrence  := 1;
    StartDate   := DateTimeToStDate(SysUtils.Date);
    DefaultDate := StartDate - 7;
    DoSetDate;
  end else if (Pos(GetOrphStr(SCCalFirst), S) > 0)
           or (Pos(GetOrphStr(SCCal1st), S) > 0) then
  begin
    Increment   := 1;
    Occurrence  := 1;
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    StartDate   := DMYToStDate(1, ThisMonth, ThisYear, Epoch) - 1;
    DefaultDate := 0;
    DoSetDate;
  end else if (Pos(GetOrphStr(SCCalSecond), S) > 0)
           or (Pos(GetOrphStr(SCCal2nd), S) > 0) then
  begin
    Increment   := 1;
    Occurrence  := 2;
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    StartDate   := DMYToStDate(1, ThisMonth, ThisYear, Epoch) - 1;
    DefaultDate := 0;
    DoSetDate;
  end else if (Pos(GetOrphStr(SCCalThird), S) > 0)
           or (Pos(GetOrphStr(SCCal3rd), S) > 0) then
  begin
    Increment   := 1;
    Occurrence  := 3;
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    StartDate   := DMYToStDate(1, ThisMonth, ThisYear, Epoch) - 1;
    DefaultDate := 0;
    DoSetDate;
  end else if (Pos(GetOrphStr(SCCalFourth), S) > 0)
           or (Pos(GetOrphStr(SCCal4th), S) > 0) then
  begin
    Increment   := 1;
    Occurrence  := 4;
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    StartDate   := DMYToStDate(1, ThisMonth, ThisYear, Epoch) - 1;
    DefaultDate := 0;
    DoSetDate;
  end else if Pos(GetOrphStr(SCCalFinal), S) > 0 then
  begin
    Increment   := -1;
    Occurrence  := 1;
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    StartDate   := DMYToStDate(DaysInMonth(ThisMonth,
                                           ThisYear, Epoch),
                                ThisMonth, ThisYear, Epoch) + 1;
    DefaultDate := 0;
    DoSetDate;
  end else if (Pos(GetOrphStr(SCCalBOM), S) > 0)
           or (Pos(GetOrphStr(SCCalBegin), S) > 0) then
  begin
    Increment   := 0;
    Occurrence  := 0;
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    StartDate   := DMYToStDate(1, ThisMonth, ThisYear, Epoch);
    DefaultDate := StartDate;
    DoSetDate;
  end else if (Pos(GetOrphStr(SCCalEOM), S) > 0)
           or (Pos(GetOrphStr(SCCalEnd), S) > 0) then
  begin
    Increment   := 0;
    Occurrence  := 0;
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    StartDate   := DMYToStDate(DaysInMonth(ThisMonth,
                                           ThisYear, Epoch),
                                ThisMonth, ThisYear, Epoch);
    DefaultDate := StartDate;
    DoSetDate;
  end else
  begin
    Increment   := 1;
    Occurrence  := 1;
    StartDate   := DateTimeToStDate(SysUtils.Date);
    DefaultDate := -1;
    DoSetDate;
  end; }
  Result := S;
end;

procedure TosCustomTgDateEdit.PopupSetDate(Sender : TObject);
begin
  Self.SetDate(FCalendar.DateTime);
  PopupClose(Self);
end;

procedure TosCustomTgDateEdit.PopupClose(Sender : TObject);
begin
  if not FCalendar.Visible then
    Exit; {already closed, exit}

  if PopupClosing then
    Exit;

  PopupClosing := True; {avoid recursion}
  try
    inherited PopupClose(Sender);

    if GetCapture = FCalendar.Handle then
       ReleaseCapture;

    SetFocus;
    FCalendar.Hide;  
    if FCalendar.Parent is TForm then
      TForm(FCalendar.Parent).AutoScroll := WasAutoScroll;

    Cursor := HoldCursor;
    //Self.Text := DateToStr(Calendar.DateTime);
    {change parentage so that we control the window handle destruction}
    FCalendar.Parent := Self;
  finally
    PopupClosing := False;
  end;
end;

procedure TosCustomTgDateEdit.PopupKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
var
  X : Integer;
begin
  case Key of
    VK_TAB :
      begin
        if Shift = [ssShift] then
        begin
          PopupClose(Sender);
          PostMessage(Handle, WM_KeyDown, VK_TAB, Integer(ssShift));
        end
        else if Shift = [] then
        begin
          PopupClose(Sender);
          PostMessage(Handle, WM_KeyDown, VK_TAB, 0);
        end;
      end;
    VK_UP  :
      begin
        if Shift = [ssAlt] then
        begin
          PopupClose(Sender);
          X := SelStart;
          SetFocus;
          SelStart := X;
          SelLength := 0;
        end;
      end;
  end;
end;

procedure TosCustomTgDateEdit.PopupKeyPress(Sender : TObject; var Key : Char);
var
  X : Integer;
begin
  case Key of
    #13,
    #32,
    #27 :
      begin
        PopupClose(Sender);
        X := SelStart;
        SetFocus;
        SelStart := X;
        SelLength := 0;
      end;
  end;
end;

procedure TosCustomTgDateEdit.PopupMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  P : TPoint;
  I : Integer;
begin
  P := Point(X,Y);
  if not PtInRect(FCalendar.ClientRect, P) then
    PopUpClose(Sender);

  {convert to our coordinate system}
  P := ScreenToClient(FCalendar.ClientToScreen(P));

  if PtInRect(ClientRect, P) then
  begin
    I := SelStart;
    SetFocus;
    SelStart := I;
    SelLength := 0;
  end;
end;

procedure TosCustomTgDateEdit.PopupOpen;
begin
  if FCalendar.Visible then
    Exit;  {already popped up, exit}

  inherited PopupOpen;

  try
    SetDateText(Text);
  except
    SetFocus;
    raise;
  end;

  FCalendar.Parent := GetParentForm(Self);
  if FCalendar.Parent is TForm then
  begin
    WasAutoScroll := TForm(FCalendar.Parent).AutoScroll;
    TForm(FCalendar.Parent).AutoScroll := False;
  end;

  {set 3d to be the same as our own}
  FCalendar.ParentCtl3D  := False;
  FCalendar.Ctl3D := False;
  FCalendar.BorderStyle := bsNone;

  if Text = '' then
     FCalendar.DateTime := SysUtils.Date
  else
     FCalendar.DateTime := FDate;

  HoldCursor := Cursor;
  Cursor := crArrow;
  FCalendar.Parent := Nil;
  FCalendar.Width  := FCalendar.CalendarControl.Width + 2;
  FCalendar.Height := FCalendar.CalendarControl.Height + 2;
  FCalendar.Left   := Self.ClientOrigin.x;
  FCalendar.Top    := Self.ClientOrigin.y + Self.Height + 1;
  FCalendar.Enabled := True;
  FCalendar.Visible := True;
  FCalendar.CalendarControl.OnSelect := PopupSetDate;
  FCalendar.CalendarControl.InitializeFocus;
end;

procedure TosCustomTgDateEdit.PopupDateChange(Sender : TObject; Date : TDateTime);
begin
  {get the current value}
  SetDate(FCalendar.DateTime);
  Modified := True;

 // if FCalendar.Browsing then
 //   Exit;

  {hide the Calendar}
  PopupClose(Sender);
  SetFocus;
  SelStart := Length(Text);
  SelLength := 0;
end;

procedure TosCustomTgDateEdit.SetDate(Value : TDateTime);
begin
  if (FDate <> Value) then
  begin
    FDate := Value;
    if FDate = 0 then
       Text := ''
    else
       Text := FormatDate(FDate);
    Modified := True;
    if Assigned(FOnSetDate) then
      FOnSetDate(Self);
  end;
end;

procedure TosCustomTgDateEdit.SetDateText(Value : string);
var Field, I1, I2, Error : Integer;
    ThisYear, ThisMonth, ThisDay : Word;
    Year, Month, Day, Hr, Mn, Sc : Word;
    EpochYear, EpochCent  : Integer;
    StringList : TStringList;
    FieldOrder : string[7];
    S, ampm    : string;
    bParseTime : Boolean;


  function ParseTime(inText : String; sendError, maxValue : Integer) : Word;
  begin
    if (Pos('P', inText) > 0) or
       (Pos('A', inText) > 0) then
    begin
      if (Pos('P', inText) > 0) then
      begin
        ampm := 'pm';
        inText := Copy(inText, 1, Pos('P', inText)-1);
      end
      else
        inText := Copy(inText, 1, Pos('A', inText)-1);
    end;
    try
      if inText = '' then
        Result := 0
      else
        Result := StrToInt(inText);
    except
      Result := 0;
      Error := sendError;
    end;
    if (Result > maxValue) then
    begin
      Result := 0;
      Error := sendError;
    end;
    if StringList.Count > 0 then
       StringList.Delete(0);
  end;
begin
  if Assigned(FOnGetDate) then
     FOnGetDate(Self, Value);

  if (Value = '') and (FRequiredFields <> []) then
  begin
    FDate := 0;
    Text := '';
    Exit;
  end;

  if AnsiCompareText(Value, TodayString) = 0 then
  begin
    Text := FormatDate(SysUtils.Date);
  end
  else
  begin
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    Value := UpperCase(Value);
    StringList := TStringList.Create;
    try
      //parse the string into subfields using a string list to hold the parts
      I1 := 1;
      while (I1 <= Length(Value)) and not (Value[I1] in ['0'..'9', 'A'..'Z']) do
        Inc(I1);
      while I1 <= Length(Value) do
      begin
        I2 := I1;
        while (I2 <= Length(Value)) and (Value[I2] in ['0'..'9', 'A'..'Z']) do
          Inc(I2);
        StringList.Add(Copy(Value, I1, I2-I1));
        while (I2 <= Length(Value)) and not (Value[I2] in ['0'..'9', 'A'..'Z']) do
          Inc(I2);
        I1 := I2;
      end;

      case DateOrder of
        doMDY : FieldOrder := 'MDY';
        doDMY : FieldOrder := 'DMY';
        doYMD : FieldOrder := 'YMD';
      end;
      // check for time entry
      bParseTime := (StringList.Count > 3);
      if (bParseTime) then
      begin
        if (StringList.Count = 7) then // secs and ampm are there
           FieldOrder := FieldOrder + 'hnsp'
        else
           FieldOrder := FieldOrder + 'hnp';
      end;

      Year := 0; Month := 0; Day := 0;
      Hr := 0; Mn := 0; Sc := 0; ampm := 'am';
      Error := 0;
      for Field := 1 to Length(FieldOrder) do
      begin
        if StringList.Count > 0 then
          S := StringList[0]
        else
          S := '';

        case FieldOrder[Field] of
          'M' :
            begin
              if (S = '') or (S[1] in ['0'..'9']) then
              begin {numeric month}
                try
                  if S = '' then
                    Month := 0
                  else
                    Month := StrToInt(S);
                except
                  Month := 0;
                  Error := tsMonthConvertError; //error converting month number
                end;
                if not (Month in [1..12]) then
                  Month := 0;
              end
              else
              begin {one or more letters in month}
                Month := 0;
                I1 := 1;
                S := Copy(S, 1, 3);
                Error := tsMonthNameConvertError; 
                repeat
                  if S = UpperCase(Copy(ShortMonthNames[I1], 1, Length(S))) then
                  begin
                    Month := I1;
                    I1 := 13;
                    Error := 0;
                  end
                  else
                    Inc(I1);
                until I1 = 13;
              end;

              if Month = 0 then
              begin
                if rfMonth in FRequiredFields then
                  Error := tsMonthRequired
                else
                  Month := ThisMonth;
              end
              else if StringList.Count > 0 then
                StringList.Delete(0);

              if Error > 0 then
                Break;
            end;
          'Y' :
            begin
              try
                if S = '' then
                  Year := 0
                else
                  Year := StrToInt(S);
              except
                Year := 0;
                Error := tsYearConvertError;
              end;
              if (Epoch = 0) and (Year < 100) and (S <> '') then
                {default to current century if Epoch is zero}
                Year := Year + (ThisYear div 100 * 100)
              else if (Epoch > 0) and (Year < 100) and (S <> '') then
              begin
                {use epoch}
                EpochYear := Epoch mod 100;
                EpochCent := (Epoch div 100) * 100;
                if (Year < EpochYear) then
                  Inc(Year,EpochCent+100)
                else
                  Inc(Year,EpochCent);
              end;
              if Year = 0 then
              begin
                if rfYear in FRequiredFields then
                  Error := tsYearRequired //year is required
                else
                  Year := ThisYear;
              end else if StringList.Count > 0 then
                StringList.Delete(0);
              if Error > 0 then
                Break;
            end;
          'D' :
            begin
              try
                if S = '' then
                  Day := 0
                else
                  Day := StrToInt(S);
              except
                Day := 0;
                Error := tsDayConvertError;
              end;
              if not (Day in [1..31]) then
                Day := 0;
              if Day = 0 then begin
                if rfDay in FRequiredFields then
                  Error := tsDayRequired
                else
                  Day := ThisDay;
                end
              else if StringList.Count > 0 then
                StringList.Delete(0);

              if Error > 0 then
                Break;
            end;
          'h' :
            begin
              Hr := ParseTime(S, tsHourConvertError, 12);

              if Error > 0 then
                 Break;
            end;
          'n' :
            begin
              Mn := ParseTime(S, tsMinConvertError, 60);

              if Error > 0 then
                 Break;
            end;
          's' :
            begin
              Sc := ParseTime(S, tsSecConvertError, 60);

              if Error > 0 then
                 Break;
            end;
          'p' :
            begin
              if (Length(S) > 0) and
                 (S[1] in ['0'..'9']) then
              begin
                Sc := ParseTime(S, tsSecConvertError, 60);
                if (Pos('P', S) > 0) then
                   ampm := 'pm';
              end
              else if (Lowercase(S) = 'pm') then
                 ampm := 'pm';
            end;
        end;
      end;

      case Error of
        tsSecConvertError :
          if S = '' then
            raise Exception.Create(tsInvalidSec + ' "' + Value + '"')
          else
            raise Exception.Create(tsInvalidSec + ' "' + S + '"');
        tsMinConvertError :
          if S = '' then
            raise Exception.Create(tsInvalidMin + ' "' + Value + '"')
          else
            raise Exception.Create(tsInvalidMin + ' "' + S + '"');
        tsHourConvertError :
          if S = '' then
            raise Exception.Create(tsInvalidHour + ' "' + Value + '"')
          else
            raise Exception.Create(tsInvalidHour + ' "' + S + '"');
        tsDayConvertError :
          if S = '' then
            raise Exception.Create(tsInvalidDay + ' "' + Value + '"')
          else
            raise Exception.Create(tsInvalidDay + ' "' + S + '"');
        tsMonthConvertError :
          if S = '' then
            raise Exception.Create(tsInvalidMonth + ' "' + Value + '"')
          else
            raise Exception.Create(tsInvalidMonth + ' "' + S + '"');
        tsMonthNameConvertError :
          if S = '' then
            raise Exception.Create(tsInvalidMonthName + ' "' + Value + '"')
          else
            raise Exception.Create(tsInvalidMonthName + ' "' + S + '"');
        tsYearConvertError :
          if S = '' then
            raise Exception.Create(tsInvalidYear + ' "' + Value + '"')
          else
            raise Exception.Create(tsInvalidYear + ' "' + S + '"');
        tsDayRequired :
          raise Exception.Create(tsDayRequiredMsg);
        tsMonthRequired :
          raise Exception.Create(tsMonthRequiredMsg);
        tsYearRequired :
          raise Exception.Create(tsYearRequiredMsg);
      end;

      try
        FDate := EncodeDate(Year, Month, Day);
        if (bParseTime) then
        begin
          if (ampm = 'pm') and
             (Hr < 12) then
             Hr := Hr + 12;
          FDate := FDate + EncodeTime(Hr, Mn, Sc, 0);
          Text := DateTimeToStr(FDate);
        end
        else
          Text := DateToStr(FDate);
      except on E:Exception do
        raise Exception.Create('Date Conversion Error ' + E.Message);
      end;

    finally
      StringList.Free;
    end;
  end;
end;

procedure TosCustomTgDateEdit.SetEpoch(Value : Integer);
begin
  if Value <> FEpoch then
     FEpoch := Value;
end;

procedure TosCustomTgDateEdit.SetForceCentury(Value : Boolean);
begin
  if Value <> FForceCentury then
  begin
    FForceCentury := Value;
    SetDate(FCalendar.DateTime);
  end;
end;

procedure TosCustomTgDateEdit.SetReadOnly(Value : Boolean);
begin
  inherited ReadOnly := Value;

  FButton.Enabled := not ReadOnly;
end;


{ TosDbTgDateEdit }

constructor TosDbTgDateEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  //FDataLink.OnActiveChange := ActiveChange;
end;

destructor TosDbTgDateEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TosDbTgDateEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TosDbTgDateEdit.SetDate(Value : TDateTime);
begin
  if (FDate <> Value) then
  begin
    FDate := Value;
    Modified := True;
    try
      FDataLink.Field.AsDateTime := FDate;
    except
      SelectAll;
      SetFocus;
      raise;
    end;
    Text := FDataLink.Field.Text;
    if Assigned(FOnSetDate) then
      FOnSetDate(Self);    
  end;
end;

procedure TosDbTgDateEdit.PopupSetDate(Sender : TObject);
begin
  inherited;
end;

procedure TosDbTgDateEdit.PopupOpen;
begin
  inherited;
  FDataLink.Edit;
end;

procedure TosDbTgDateEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TosDbTgDateEdit.FormatDate(Value : TDateTime) : string;
begin
  Result := DateToStr(Value);
end;

procedure TosDbTgDateEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      Self.Text := FDataLink.Field.DisplayText;
      if FDataLink.Editing then // and FDataLink.FModified then
         Modified := True;
    end;
  end else
  begin
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end;
end;

function TosDbTgDateEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TosDbTgDateEdit.EditingChange(Sender: TObject);
begin
 // inherited ReadOnly := not FDataLink.Editing;
end;

function TosDbTgDateEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TosDbTgDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TosDbTgDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TosDbTgDateEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TosDbTgDateEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;


procedure TosDbTgDateEdit.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TosDbTgDateEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TosDbTgDateEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TosDbTgDateEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TosDbTgDateEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  DoExit;
end;

procedure TosDbTgDateEdit.DoExit;
begin
  if not PopupActive then
  begin
    if (Assigned(OnExit)) then
       Self.OnExit(Self);
  end;
end;

procedure TosDbTgDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
     FDataLink.Edit;
end;

procedure TosDbTgDateEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TosDbTgDateEdit.Loaded;
begin
  inherited;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TosDbTgDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TosDbTgDateEdit.Reset;
begin
  inherited;
  FDataLink.Reset;
  SelectAll;
end;

procedure TosDbTgDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TosDbTgDateEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TosDbTgDateEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    FDataLink.Reset;
  end;
end;

procedure TosDbTgDateEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TosDbTgDateEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TosDbTgDateEdit.UpdateData(Sender: TObject);
begin
  //ValidateEdit;
  FDataLink.Field.AsDateTime := Self.Date;
end;

end.




