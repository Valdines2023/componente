unit TSMbcs;

{$INCLUDE TSCmpVer}

interface

const
    CH_LINEFEED = 10;

var
    tsIsFarEast: Boolean;

{$IFNDEF TSVER_V3}
//=========================================================================
//MBCS support types and routines for Delphi 2 and C++Builder 1
type
    TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);

var
    LeadBytes: set of Char = [];

function StrByteType(Text: PAnsiChar; Index: Cardinal): TMbcsByteType;
function AnsiStrComp(S1, S2: PAnsiChar): Integer;
function AnsiStrIComp(S1, S2: PAnsiChar): Integer;
function AnsiStrLComp(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
function AnsiStrLIComp(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
function AnsiStrPos(Str, SubStr: PAnsiChar): PAnsiChar;
function AnsiPos(const Substr, S: string): Integer;
//=========================================================================}
{$ENDIF}

function CompareChar(S1: PAnsiChar; S2: PAnsiChar): Integer;
function LargerChar(S1, S2: PAnsiChar): Boolean;
function SmallerChar(S1, S2: PAnsiChar): Boolean;

function EofLineChar(PText: PAnsiChar; Index: Integer) : Boolean;
function GetEofLine(Text : PAnsiChar; var Pos : Integer) : Boolean;
function EndOfText(Text : PAnsiChar; Pos : Integer) : Boolean;
function NextCharCount(PText : PAnsiChar; Offset : Integer) : Integer;
function PrevCharCount(PText : PAnsiChar; Offset : Integer) : Integer;

implementation

uses
    Windows, SysUtils;

{$IFNDEF TSVER_V3}
//=========================================================================
//MBCS support types and routines for Delhpi 2 and C++Builder 1

function StrByteType(Text: PAnsiChar; Index: Cardinal): TMbcsByteType;
begin
    Result := mbSingleByte;
    if not tsIsFarEast then Exit;

    if (Index = 0) then
    begin
        if Text[Index] in LeadBytes then Result := mbLeadByte;
    end
    else
    begin
        if (Text[Index - 1] in LeadBytes) and
           (StrByteType(Text, Index - 1) = mbLeadByte) then
            Result := mbTrailByte
        else if Text[Index] in LeadBytes then
            Result := mbLeadByte;
    end;
end;

function AnsiStrComp(S1, S2: PAnsiChar): Integer;
begin
    Result := CompareString(LOCALE_USER_DEFAULT, 0, S1, -1, S2, -1) - 2;
end;

function AnsiStrIComp(S1, S2: PAnsiChar): Integer;
begin
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1, S2, -1) - 2;
end;

function AnsiStrLComp(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
    Result := CompareString(LOCALE_USER_DEFAULT, 0, S1, MaxLen, S2, MaxLen) - 2;
end;

function AnsiStrLIComp(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, MaxLen, S2, MaxLen) - 2;
end;

function AnsiStrPos(Str, SubStr: PAnsiChar): PAnsiChar;
var
    StrLength, SubStrLength: Cardinal;
    ByteType : TMbcsByteType;
begin
    Result := nil;
    if (Str = nil) or (Str^ = #0) or (SubStr = nil) or (SubStr^ = #0) then Exit;

    StrLength := StrLen(Str);
    SubStrLength := StrLen(SubStr);
    Result := StrPos(Str, SubStr);
    while (Result <> nil) and ((StrLength - (Result - Str)) >= SubStrLength) do
    begin
        ByteType := StrByteType(Str, Integer(Result-Str));
        if (ByteType <> mbTrailByte) then
        begin
            if (CompareString(LOCALE_USER_DEFAULT, 0, Result, SubStrLength, SubStr, SubStrLength) = 2) then
                Exit;
        end;

        if (ByteType = mbLeadByte) then Inc(Result);
        Inc(Result);
        Result := StrPos(Result, SubStr);
    end;
    Result := nil;
end;

function AnsiPos(const Substr, S: string): Integer;
var
    P: PAnsiChar;
begin
    Result := 0;
    P := AnsiStrPos(PAnsiChar(S), PAnsiChar(SubStr));
    if P <> nil then Result := Integer(P) - Integer(PAnsiChar(S)) + 1;
end;

//=========================================================================}
{$ENDIF}

function CompareChar(S1: PAnsiChar; S2: PAnsiChar): Integer;
begin
    if tsIsFarEast then
        Result := AnsiStrLComp(S1, S2, 1)
    else
    begin
        if S1[0] < S2[0] then
            Result := -1
        else if S1[0] = S2[0] then
            Result := 0
        else
            Result := 1;
    end;
end;

function LargerChar(S1, S2: PAnsiChar): Boolean;
begin
    if tsIsFarEast
        then Result := AnsiStrLComp(S1, S2, 1) > 0
        else Result := S1[0] > S2[0];
end;

function SmallerChar(S1, S2: PAnsiChar): Boolean;
begin
    if tsIsFarEast
        then Result := AnsiStrLComp(S1, S2, 1) < 0
        else Result := S1[0] < S2[0];
end;

function EofLineChar(PText: PAnsiChar; Index: Integer) : Boolean;
begin
    Result := False;
    if StrByteType(PText, Index) <> mbSingleByte then Exit;
    Result := (Ord(PText[Index]) = VK_RETURN) or (Ord(PText[Index]) = CH_LINEFEED);
end;

function GetEofLine(Text : PAnsiChar; var Pos : Integer) : Boolean;
var
    EofChar: AnsiChar;
begin
    Result := false;

    if EofLineChar(Text, Pos) then
    begin
        Result := True;
        EofChar := (Text + Pos)^;
        Pos := Pos + 1;
        if EofLineChar(Text, Pos) and ((Text + Pos)^ <> EofChar) then
            Pos := Pos + 1;
    end;
end;

function EndOfText(Text : PAnsiChar; Pos : Integer) : Boolean;
begin
    Result := ((Text + Pos)^ = #0);
end;

function NextCharCount(PText : PAnsiChar; Offset : Integer) : Integer;
var
    Chars : Integer;
begin
    Result := 0;
    if not EndOfText(PText, Offset) then
    begin
        Chars := 0;
        if GetEofLine(PText + Offset, Chars) then
            Result := Chars
        else
        begin
            Result := 1;
            if StrByteType(PText, Offset) <> mbSingleByte then Inc(Result);
        end;
    end;
end;

function PrevCharCount(PText : PAnsiChar; Offset : Integer) : Integer;
var
    Chars : Integer;
begin
    Result := 0;
    if Offset > 0 then
    begin
        if Offset > 1 then
        begin
            Chars := 0;
            if GetEofLine(PText + Offset - 2, Chars) then
                Result := Chars
            else
            begin
                Result := 1;
                if StrByteType(PText, Offset - 1) <> mbSingleByte then Inc(Result);
            end;
        end
        else
            Result := 1;
    end;
end;

procedure InitSysLocale;
var
    I: Integer;
    J: Byte;
    CPInfo: TCPInfo;
begin
    {$IFDEF TSVER_V3}
    tsIsFarEast := SysLocale.FarEast;
    Exit;
    {$ENDIF}

    tsIsFarEast := GetSystemMetrics(SM_DBCSENABLED) <> 0;
    if tsIsFarEast then
    begin
        GetCPInfo(CP_ACP, CPInfo);

        I := 0;
        while I < MAX_LEADBYTES do
        begin
            if (CPInfo.LeadByte[I] = 0) and (CPInfo.LeadByte[I + 1] = 0) then Break;

            for J := CPInfo.LeadByte[I] to CPInfo.LeadByte[I + 1] do
                Include(LeadBytes, AnsiChar(J));
            I := I + 2;
        end;
    end;
end;

initialization
begin
    InitSysLocale;
end;

end.
