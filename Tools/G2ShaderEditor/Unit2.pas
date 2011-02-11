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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

end.
