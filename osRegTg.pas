unit osRegTg;

interface

uses Classes, TSGrid, TSDBGrid, TSMask, TSImageList, TSDateTime;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('TopGrid', [TtsGrid]);
    RegisterComponents('TopGrid', [TtsDBGrid]);
    RegisterComponents('TopGrid', [TtsMaskDefs]);
    RegisterComponents('TopGrid', [TtsImageList]);
    RegisterComponents('TopGrid', [TtsDateTimeDef]);
end;

end.
 