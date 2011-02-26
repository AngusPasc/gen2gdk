unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm3 = class(TForm)
    grp1: TGroupBox;
    cbb1: TComboBox;
    chk1: TCheckBox;
    chk2: TCheckBox;
    chk4: TCheckBox;
    chk3: TCheckBox;
    lbl1: TLabel;
    btn1: TButton;
    lbl2: TLabel;
    cbb2: TComboBox;
    procedure btn1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.btn1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm3.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

end.
