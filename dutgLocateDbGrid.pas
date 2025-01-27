unit dutgLocateDbGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls
  {$IFDEF TSVER_V6} , Variants {$ENDIF};

type
  TdgGetDbGrid = class(TForm)
    Label1: TLabel;
    cbDbGrids: TComboBox;
    btOk: TButton;
    btCancel: TButton;
    procedure cbDbGridsChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dgGetDbGrid: TdgGetDbGrid;

implementation

{$R *.dfm}

procedure TdgGetDbGrid.cbDbGridsChange(Sender: TObject);
begin
  btOk.Enabled := (cbDbGrids.ItemIndex >= 0);
end;

end.
