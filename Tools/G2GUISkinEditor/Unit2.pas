unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, sGauge, StdCtrls, sButton, sListBox, sSkinManager, ExtDlgs, sDialogs;

type
  TForm2 = class(TForm)
    sListBox1: TsListBox;
    sButton1: TsButton;
    sButton2: TsButton;
    sGauge1: TsGauge;
    opd1: TsOpenPictureDialog;
    procedure sButton1Click(Sender: TObject);
    procedure sButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm2.sButton1Click(Sender: TObject);
begin
  Unit1.App.AddTextures;
end;

procedure TForm2.sButton2Click(Sender: TObject);
begin
  Unit1.App.RemoveTextures;
end;

end.
