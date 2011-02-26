unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TForm2 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    MemoBinary: TMemo;
    Button1: TButton;
    MemoInline: TMemo;
    Button2: TButton;
    MemoAssembly: TMemo;
    sdbinary: TSaveDialog;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
  CompileBinary: array of Byte;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm2.btn1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.Button1Click(Sender: TObject);
  var fs: TFileStream;
begin
  if sdbinary.Execute() then
  begin
    fs := TFileStream.Create(sdbinary.FileName, fmCreate);
    try
      fs.Write(CompileBinary[0], Length(CompileBinary));
    finally
      fs.Free;
    end;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  MemoInline.SelectAll;
  MemoInline.CopyToClipboard;
  MemoInline.SelStart := 0;
  MemoInline.SelLength := 0;
end;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

end.
