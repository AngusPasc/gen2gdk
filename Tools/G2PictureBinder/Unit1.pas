unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  ShellAPI,
  Variants,
  Classes,
  Graphics,
  Controls,
  Types,
  Math,
  Forms,
  Dialogs,
  DirectInput,
  Direct3D9,
  DXTypes,
  Gen2,
  G2Math,
  G2Landscape,
  G2PerlinNoise,
  G2Tools,
  Res;

type
  TImagePreview = class;

  TForm1 = class(TForm)
    od1: TOpenDialog;
    sd1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    procedure AcceptDropFiles(var msg: TMessage); message WM_DROPFILES;
  end;

  TConfigFile = record
  public
    var Left: Integer;
    var Top: Integer;
    var Width: Integer;
    var Height: Integer;
    var FullScreen: Boolean;
  end;

  TMyApp = class (TG2App)
  public
    AppClose: Boolean;
    Font: TG2Font;
    pn: TG2PerlinNoise;
    MgrTextures: TG2TextureMgr;
    TexBorder: TG2Texture2D;
    TexTopBG: TG2Texture2D;
    TexGrad: TG2Texture2D;
    TexTitle: TG2Texture2D;
    bg: array [0..1] of array[0..15] of Single;
    bgGrad: Single;
    Menu: TG2Menu;
    Book: TG2Book;
    Images: TG2ImageCollection;
    ImageList: TG2ImageList;
    ImageAssembly: TG2ImageAssembly;
    ImagePreview: TImagePreview;
    ColCountEdit: TG2SpinEdit;
    RowCountEdit: TG2SpinEdit;
    PatWidthEdit: TG2SpinEdit;
    PatHeightEdit: TG2SpinEdit;
    PreviewSpeed: TG2SpinEdit;
    Saving: Boolean;
    Config: TConfigFile;
    procedure OnParamsChange; override;
    procedure OnRender; override;
    procedure OnUpdate; override;
    procedure OnKeyDown(const Key: Byte); override;
    procedure OnKeyUp(const Key: Byte); override;
    procedure RenderBorder;
    procedure RenderTitle;
    procedure RenderBackground;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure AddImages;
    procedure ClearImages;
    procedure SetPageImages;
    procedure SetPageBind;
    procedure SetPagePreview;
    procedure ColCountChange;
    procedure RowCountChange;
    procedure PatWidthChange;
    procedure PatHeightChange;
    procedure AssemblyAssemble;
    procedure AssemblyClear;
    procedure DeleteImageItem(const Item: Pointer);
    procedure SaveImage;
    procedure LoadConfig;
    procedure SaveConfig;
  end;

  TImagePreview = class (TG2BookPageItem)
  strict private
    const bs = 8;
    procedure OnWheelMove(const Shift: Integer); override;
  public
    var Assembly: TG2ImageAssembly;
    var Items: array of PG2ImageItem;
    var ItemCount: Integer;
    var CurItem: Single;
    var Scale: Single;
    constructor Create; override;
    destructor Destroy; override;
    procedure Render; override;
    procedure Update; override;
  end;

  TScene = class
  strict private
    m_X: Single;
    m_Y: Single;
    m_W: Single;
    m_H: Single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render;
    procedure Update;
  end;

var
  Form1: TForm1;
  App: TMyApp;

implementation

{$R *.dfm}

//TMyApp BEGIN
procedure TMyApp.OnParamsChange;
begin
  Images.R^.Right := Gfx.Params.Width - 32;
  Images.R^.Bottom := Gfx.Params.Height - 32;
  ImageList.R^.Bottom := Gfx.Params.Height - 32;
  ImageAssembly.R^.Right := Gfx.Params.Width - 32;
  ImageAssembly.R^.Bottom := Gfx.Params.Height - 32;
  ImagePreview.R^.Right := Gfx.Params.Width - 32;
  ImagePreview.R^.Bottom := Gfx.Params.Height - 32;
end;

procedure TMyApp.OnRender;
begin
  Render.RenderStart;
  Render.Clear(False, False, True, $ff888888);

  RenderBackground;
  RenderTitle;
  RenderBorder;

  Menu.Render;
  Book.Render;

  if Saving then
  Font.Print(
    (Gfx.Params.Width - Font.GetTextWidth('Saving...')) * 0.5,
    (Gfx.Params.Height - Font.GetTextHeight('Saving...')) * 0.5,
    $ffffffff, 'Saving...'
  );

  //Font.Print(10, 10, $ff0000ff, 'FPS: ' + IntToStr(Tmr.FPS));

  Render.RenderStop;
  Render.Present;
end;

procedure TMyApp.OnUpdate;
begin
  if AppClose then
  begin
    Tmr.Enabled := False;
    Form1.Close;
  end
  else
  begin
    bgGrad := bgGrad + 0.003;
    Book.Update;
  end;
end;

procedure TMyApp.OnKeyDown(const Key: Byte);
begin
  if Key = DIK_ESCAPE then
  AppClose := True;
end;

procedure TMyApp.OnKeyUp(const Key: Byte);
begin

end;

procedure TMyApp.RenderBorder;
const
  BS = 32;
var
  w, h: Integer;
begin
  w := Gfx.Params.Width;
  h := Gfx.Params.Height;

  Render.TextureSet(TexBorder);

  RenderModes.FilteringLinear();

  Render2D.DrawBegin(ptTriangleList);
  Render2D.BaseVertexIndex := 0;

  Render2D.AddPos(G2Vec2(0, 0));
  Render2D.AddPos(G2Vec2(BS, 0));
  Render2D.AddPos(G2Vec2(w - BS, 0));
  Render2D.AddPos(G2Vec2(w, 0));
  Render2D.AddPos(G2Vec2(w, BS));
  Render2D.AddPos(G2Vec2(w, h - BS));
  Render2D.AddPos(G2Vec2(w, h));
  Render2D.AddPos(G2Vec2(w - BS, h));
  Render2D.AddPos(G2Vec2(BS, h));
  Render2D.AddPos(G2Vec2(0, h));
  Render2D.AddPos(G2Vec2(0, h - BS));
  Render2D.AddPos(G2Vec2(0, BS));
  Render2D.AddPos(G2Vec2(BS, BS));
  Render2D.AddPos(G2Vec2(w - BS, BS));
  Render2D.AddPos(G2Vec2(w - BS, h - BS));
  Render2D.AddPos(G2Vec2(BS, h - BS));

  Render2D.AddTex(G2Vec2(0, 0));
  Render2D.AddTex(G2Vec2(0.5, 0));
  Render2D.AddTex(G2Vec2(0.5, 0));
  Render2D.AddTex(G2Vec2(1, 0));
  Render2D.AddTex(G2Vec2(1, 0.5));
  Render2D.AddTex(G2Vec2(1, 0.5));
  Render2D.AddTex(G2Vec2(1, 1));
  Render2D.AddTex(G2Vec2(0.5, 1));
  Render2D.AddTex(G2Vec2(0.5, 1));
  Render2D.AddTex(G2Vec2(0, 1));
  Render2D.AddTex(G2Vec2(0, 0.5));
  Render2D.AddTex(G2Vec2(0, 0.5));
  Render2D.AddTex(G2Vec2(0.5, 0.5));
  Render2D.AddTex(G2Vec2(0.5, 0.5));
  Render2D.AddTex(G2Vec2(0.5, 0.5));
  Render2D.AddTex(G2Vec2(0.5, 0.5));

  Render2D.AddInd(0); Render2D.AddInd(1); Render2D.AddInd(11);
  Render2D.AddInd(11); Render2D.AddInd(1); Render2D.AddInd(12);
  
  Render2D.AddInd(1); Render2D.AddInd(2); Render2D.AddInd(12);
  Render2D.AddInd(12); Render2D.AddInd(2); Render2D.AddInd(13);

  Render2D.AddInd(2); Render2D.AddInd(3); Render2D.AddInd(13);
  Render2D.AddInd(13); Render2D.AddInd(3); Render2D.AddInd(4);

  Render2D.AddInd(11); Render2D.AddInd(12); Render2D.AddInd(10);
  Render2D.AddInd(10); Render2D.AddInd(12); Render2D.AddInd(15);
  
  Render2D.AddInd(13); Render2D.AddInd(4); Render2D.AddInd(14);
  Render2D.AddInd(14); Render2D.AddInd(4); Render2D.AddInd(5);

  Render2D.AddInd(10); Render2D.AddInd(15); Render2D.AddInd(9);
  Render2D.AddInd(9); Render2D.AddInd(15); Render2D.AddInd(8);

  Render2D.AddInd(15); Render2D.AddInd(14); Render2D.AddInd(8);
  Render2D.AddInd(8); Render2D.AddInd(14); Render2D.AddInd(7);

  Render2D.AddInd(14); Render2D.AddInd(5); Render2D.AddInd(7);
  Render2D.AddInd(7); Render2D.AddInd(5); Render2D.AddInd(6);

  Render2D.DrawEnd;

  RenderModes.FilteringPoint();
end;

procedure TMyApp.RenderTitle;
var
  w: Integer;
  str: AnsiString;
begin
  w := Gfx.Params.Width;
  Render.TextureSet(TexTopBG);

  RenderModes.FilteringLinear();
  Render2D.DrawBegin(ptTriangleStrip);
  Render2D.BaseVertexIndex := 0;

  Render2D.AddPos(G2Vec2(0, 64));
  Render2D.AddPos(G2Vec2(0, 0));
  Render2D.AddPos(G2Vec2(128, 64));
  Render2D.AddPos(G2Vec2(128, 0));
  Render2D.AddPos(G2Vec2(w - 128, 64));
  Render2D.AddPos(G2Vec2(w - 128, 0));
  Render2D.AddPos(G2Vec2(w, 64));
  Render2D.AddPos(G2Vec2(w, 0));

  Render2D.AddTex(G2Vec2(0, 1));
  Render2D.AddTex(G2Vec2(0, 0));
  Render2D.AddTex(G2Vec2(0.5, 1));
  Render2D.AddTex(G2Vec2(0.5, 0));
  Render2D.AddTex(G2Vec2(0.5, 1));
  Render2D.AddTex(G2Vec2(0.5, 0));
  Render2D.AddTex(G2Vec2(1, 1));
  Render2D.AddTex(G2Vec2(1, 0));

  Render2D.DrawEnd;
  RenderModes.FilteringPoint();
  
  str := 'v 1.0';
  Font.Print(w - Font.GetTextWidth(str) - 16, 16, $ff000000, str);
  Render2D.DrawRect((w - TexTitle.Width) * 0.5, -8, TexTitle);
end;

procedure TMyApp.RenderBackground;
var
  i, j: Integer;
  w, h: Integer;
  sw: Single;
  v1, v2: Integer;
  g, hv1, hv2: Single;
begin
  RenderModes.FilteringLinear();
  w := Gfx.Params.Width;
  h := Gfx.Params.Height;
  sw := w / High(bg[0]);
  Render.TextureSet(TexGrad);
  Render2D.DrawBegin(ptTriangleList);
  Render2D.BaseVertexIndex := 0;
  for i := 0 to High(bg[0]) do
  begin
    v1 := Trunc(i + bgGrad);
    v2 := (v1 + 1);
    g := (i + bgGrad) - v1;
    v1 := v1 mod Length(bg[0]);
    v2 := v2 mod Length(bg[0]);
    hv1 := G2LerpFloat(bg[0][v1], bg[0][v2], g);
    hv2 := G2LerpFloat(bg[1][v1], bg[1][v2], g);
    Render2D.AddPos(G2Vec2(i * sw, 0 * h));
    Render2D.AddPos(G2Vec2(i * sw, hv1 * h));
    Render2D.AddPos(G2Vec2(i * sw, hv2 * h));
    Render2D.AddPos(G2Vec2(i * sw, 1 * h));
    Render2D.AddTex(G2Vec2(0.5, 0));
    Render2D.AddTex(G2Vec2(0.5, 0.5));
    Render2D.AddTex(G2Vec2(0.5, 0.5));
    Render2D.AddTex(G2Vec2(0.5, 1));
    Render2D.AddCol($ffffffff);
    Render2D.AddCol($ffffffff);
    Render2D.AddCol($ffffffff);
    Render2D.AddCol($ffffffff);
    if i < High(bg[0]) then
    begin
      for j := 0 to 2 do
      begin
        Render2D.AddInd((i + 0) * 4 + 0 + j);
        Render2D.AddInd((i + 1) * 4 + 0 + j);
        Render2D.AddInd((i + 0) * 4 + 1 + j);

        Render2D.AddInd((i + 0) * 4 + 1 + j);
        Render2D.AddInd((i + 1) * 4 + 0 + j);
        Render2D.AddInd((i + 1) * 4 + 1 + j);
      end;
    end;
  end;
  Render2D.DrawEnd;
  RenderModes.FilteringPoint();
end;

procedure TMyApp.Initialize;
var
  i: Integer;
begin
  LoadConfig;
  AppClose := False;
  Saving := False;
  Tmr.MaxFPS := 0;
  inherited Initialize;
  G2Tools.App := Self;
  G2Tools.ToolsInitialize;
  g2.RequestMod(TG2TextureMgr, @MgrTextures);
  Font := Gfx.Shared.RequestFont('Arial', 12);
  Cam.SetPerspective(QuatPi, 4/3, 0.1, 10000);
  Cam.SetView(250, 300, 250, 500, 100, 500, 0, 1, 0);
  Gfx.Transforms.P := Cam.Proj;
  Gfx.Transforms.V := Cam.View;
  Gfx.Transforms.ApplyV;
  Gfx.Transforms.ApplyP;

  TexBorder := MgrTextures.CreateTexture2DFromBuffer('Border', @BinBorder, Length(BinBorder), 1);
  TexTopBG := MgrTextures.CreateTexture2DFromBuffer('TopBG', @BinTopBG, Length(BinTopBG), 1);
  TexGrad := MgrTextures.CreateTexture2DFromBuffer('Grad', @BinGrad, Length(BinGrad), 1);
  TexTitle := MgrTextures.CreateTexture2DFromBuffer('Title', @BinTitle, Length(BinTitle), 1);

  Randomize;
  pn := TG2PerlinNoise.Create;
  pn.Seamless := True;
  pn.PatternWidth := 64;
  for i := 0 to High(bg[0]) do
  begin
    bg[0][i] := 0.5 - pn.PerlinNoise2D((i / High(bg[0])) * 64, 1, 6) * 0.2;
    bg[1][i] := 0.5 + pn.PerlinNoise2D((i / High(bg[0])) * 64, 100, 6) * 0.2;
  end;
  pn.Free;
  bgGrad := 0;

  MgrTextures.CreateTexture2DFromBuffer('Button1', @BinButton1, SizeOf(BinButton1), 1, D3DFMT_DXT3);
  MgrTextures.CreateTexture2DFromBuffer('Button1H', @BinButton1Hover, SizeOf(BinButton1Hover), 1, D3DFMT_DXT3);
  MgrTextures.CreateTexture2DFromBuffer('Button1D', @BinButton1Down, SizeOf(BinButton1Down), 1, D3DFMT_DXT3);
  MgrTextures.CreateTexture2DFromBuffer('Button2', @BinButton2, SizeOf(BinButton2), 1, D3DFMT_DXT3);
  MgrTextures.CreateTexture2DFromBuffer('Button2H', @BinButton2Hover, SizeOf(BinButton2Hover), 1, D3DFMT_DXT3);
  MgrTextures.CreateTexture2DFromBuffer('Button2D', @BinButton2Down, SizeOf(BinButton2Down), 1, D3DFMT_DXT3);
  MgrTextures.CreateTexture2DFromBuffer('Button3', @BinButton3, SizeOf(BinButton3), 1, D3DFMT_DXT3);
  MgrTextures.CreateTexture2DFromBuffer('Button3H', @BinButton3Hover, SizeOf(BinButton3Hover), 1, D3DFMT_DXT3);
  MgrTextures.CreateTexture2DFromBuffer('Button3D', @BinButton3Down, SizeOf(BinButton3Down), 1, D3DFMT_DXT3);
  G2Tools.MgrImages.CreateTexture2DFromBuffer('IconDel', @BinDel, SizeOf(BinDel), 1, D3DFMT_DXT3);
  G2Tools.MgrImages.CreateTexture2DFromBuffer('IconUpDown', @BinUpDown, SizeOf(BinUpDown), 1, D3DFMT_DXT3);

  Menu := TG2Menu.Create(g2);
  Menu.X := 32;
  Menu.Y := 64;
  Menu.W := 216;
  Menu.H := 128;
  Menu.AddItem('Images', SetPageImages);
  Menu.AddItem('Bind', SetPageBind);
  Menu.AddItem('Preview', SetPagePreview);

  Book := TG2Book.Create;
  with Book.AddPage('Images') do
  begin
    AddButton(
      256, 64, 160, 'Add Images',
      TG2Texture2D(MgrTextures.FindTexture('Button1')),
      TG2Texture2D(MgrTextures.FindTexture('Button1H')),
      TG2Texture2D(MgrTextures.FindTexture('Button1D')),
      AddImages
    );
    AddButton(
      424, 64, 160, 'Clear Images',
      TG2Texture2D(MgrTextures.FindTexture('Button1')),
      TG2Texture2D(MgrTextures.FindTexture('Button1H')),
      TG2Texture2D(MgrTextures.FindTexture('Button1D')),
      ClearImages
    );
    Images := AddImageCollection(256, 96, 512, 256);
    Images.OnRemoveItem := DeleteImageItem;
  end;
  with Book.AddPage('Bind') do
  begin
    ImageList := AddImageList;
    with ImageList do
    begin
      R^ := Rect(32, 192, 248, 512);
      ImageCollection := Images;
    end;
    ImageAssembly := AddImageAssembly;
    with ImageAssembly do
    begin
      R^ := Rect(256, 192, 768, 512);
      RowCount := 4;
      ColCount := 4;
      PatWidth := 64;
      PatHeight := 64;
    end;
    ImageAssembly.ImageList := ImageList;
    ColCountEdit := AddSpinEdit;
    with ColCountEdit do
    begin
      R^ := Rect(256, 96, 368, 128);
      Text := 'Col Count';
      MinVal := 1;
      MaxVal := 64;
      Value := 4;
      OnChange := ColCountChange;
    end;
    RowCountEdit := AddSpinEdit;
    with RowCountEdit do
    begin
      R^ := Rect(384, 96, 496, 128);
      Text := 'Row Count';
      MinVal := 1;
      MaxVal := 64;
      Value := 4;
      OnChange := RowCountChange;
    end;
    PatWidthEdit := AddSpinEdit;
    with PatWidthEdit do
    begin
      R^ := Rect(512, 96, 624, 128);
      Text := 'Pattern Width';
      MinVal := 16;
      MaxVal := 512;
      Value := 64;
      OnChange := PatWidthChange;
    end;
    PatHeightEdit := AddSpinEdit;
    with PatHeightEdit do
    begin
      R^ := Rect(640, 96, 752, 128);
      Text := 'Pattern Height';
      MinVal := 16;
      MaxVal := 512;
      Value := 64;
      OnChange := PatHeightChange;
    end;
    AddButton(
      256, 144, 160, 'Assemble',
      TG2Texture2D(MgrTextures.FindTexture('Button1')),
      TG2Texture2D(MgrTextures.FindTexture('Button1H')),
      TG2Texture2D(MgrTextures.FindTexture('Button1D')),
      AssemblyAssemble
    );
    AddButton(
      424, 144, 160, 'Clear',
      TG2Texture2D(MgrTextures.FindTexture('Button1')),
      TG2Texture2D(MgrTextures.FindTexture('Button1H')),
      TG2Texture2D(MgrTextures.FindTexture('Button1D')),
      AssemblyClear
    );
    AddButton(
      600, 144, 160, 'Save',
      TG2Texture2D(MgrTextures.FindTexture('Button1')),
      TG2Texture2D(MgrTextures.FindTexture('Button1H')),
      TG2Texture2D(MgrTextures.FindTexture('Button1D')),
      SaveImage
    );
  end;
  with Book.AddPage('Preview') do
  begin
    ImagePreview := TImagePreview.Create;
    AddItem(ImagePreview);
    with ImagePreview do
    begin
      R^ := Rect(256, 144, 512, 512);
      Assembly := ImageAssembly;
    end;
    PreviewSpeed := AddSpinEdit;
    with PreviewSpeed do
    begin
      R^ := Rect(256, 96, 368, 128);
      Text := 'FPS';
      MinVal := 1;
      MaxVal := 120;
      Value := 24;
      //OnChange := PatHeightChange;
    end;
  end;

  OnParamsChange;

  Menu.SetItem(0);

  Tmr.Enabled := True;
end;

procedure TMyApp.Finalize;
begin
  SaveConfig;
  Book.Free;
  Menu.Free;
  G2Tools.ToolsFinalize;
  inherited Finalize;
end;

procedure TMyApp.AddImages;
  var i: Integer;
begin
  if Form1.od1.Execute then
  begin
    for i := 0 to Form1.od1.Files.Count - 1 do
    Images.AddImage(Form1.od1.Files[i]);
  end;
end;

procedure TMyApp.ClearImages;
begin
  Images.ClearImages;
end;

procedure TMyApp.SetPageImages;
begin
  Book.OpenPage('Images');
  Book.ClosePage('Bind');
  Book.ClosePage('Preview');
end;

procedure TMyApp.SetPageBind;
begin
  Book.ClosePage('Images');
  Book.OpenPage('Bind');
  Book.ClosePage('Preview');
end;

procedure TMyApp.SetPagePreview;
begin
  Book.ClosePage('Images');
  Book.ClosePage('Bind');
  Book.OpenPage('Preview');
  ImagePreview.Scale := 1;
end;

procedure TMyApp.ColCountChange;
begin
  if ColCountEdit.Value * PatWidthEdit.Value > 2048 then
  PatWidthEdit.Value := 2048 div ColCountEdit.Value;
  ImageAssembly.ColCount := ColCountEdit.Value;
end;

procedure TMyApp.RowCountChange;
begin
  if RowCountEdit.Value * PatHeightEdit.Value > 2048 then
  PatHeightEdit.Value := 2048 div RowCountEdit.Value;
  ImageAssembly.RowCount := RowCountEdit.Value;
end;

procedure TMyApp.PatWidthChange;
begin
  if ColCountEdit.Value * PatWidthEdit.Value > 2048 then
  ColCountEdit.Value := 2048 div PatWidthEdit.Value;
  ImageAssembly.PatWidth := PatWidthEdit.Value;
end;

procedure TMyApp.PatHeightChange;
begin
  if RowCountEdit.Value * PatHeightEdit.Value > 2048 then
  RowCountEdit.Value := 2048 div PatHeightEdit.Value;
  ImageAssembly.PatHeight := PatHeightEdit.Value;
end;

procedure TMyApp.AssemblyAssemble;
begin
  ImageAssembly.Assemble;
end;

procedure TMyApp.AssemblyClear;
begin
  ImageAssembly.Clear;
end;

procedure TMyApp.DeleteImageItem(const Item: Pointer);
begin
  ImageAssembly.Remove(PG2ImageItem(Item));
end;

procedure TMyApp.SaveImage;
  const Filters: array[1..9] of AnsiString = (
    '.png', '.tga', '.bmp', '.jpg', '.dds', '.ppm', '.dib', '.hdr', '.pfm'
  );
  var f: String;
begin
  Menu.Enabled := False;
  Book.Enabled := False;
  Saving := True;
  if Form1.sd1.Execute() then
  begin
    f := Form1.sd1.FileName;
    if Length(ExtractFileExt(Form1.sd1.FileName)) < 2 then
    f := f + Filters[Form1.sd1.FilterIndex];
    ImageAssembly.Save(f);
  end;
  Saving := False;
  Book.Enabled := True;
  Menu.Enabled := True;
end;

procedure TMyApp.LoadConfig;
  var rw: TG2FileRW;
  var FName: String;
begin
  FName := ExtractFilePath(ParamStr(0)) + 'Config.cfg';
  if FileExists(FName) then
  begin
    rw := TG2FileRW.Create;
    try
      rw.OpenRead(FName);
      rw.Compression := True;
      rw.ReadBuffer(Config, SizeOf(Config));
    finally
      rw.Free;
    end;
    Form1.Position := poDefault;
    g2.Handle := Form1.Handle;
    Form1.Left := Config.Left;
    Form1.Top := Config.Top;
    Form1.Width := Config.Width;
    Form1.Height := Config.Height;
    if Config.FullScreen then
    Form1.WindowState := wsMaximized
    else
    Form1.WindowState := wsNormal;
  end
  else
  begin
    Config.Left := Form1.Left;
    Config.Top := Form1.Top;
    Config.Width := Form1.Width;
    Config.Height := Form1.Height;
    Config.FullScreen := Form1.WindowState = wsMaximized;
  end;
end;

procedure TMyApp.SaveConfig;
  var rw: TG2FileRW;
  var FName: String;
begin
  FName := ExtractFilePath(ParamStr(0)) + 'Config.cfg';
  rw := TG2FileRW.Create;
  try
    rw.OpenWrite(FName);
    rw.Compression := True;
    Config.FullScreen := Form1.WindowState = wsMaximized;
    if Config.FullScreen then
    Form1.WindowState := wsNormal;
    Config.Left := Form1.Left;
    Config.Top := Form1.Top;
    Config.Width := Form1.Width;
    Config.Height := Form1.Height;
    rw.WriteBuffer(Config, SizeOf(Config));
  finally
    rw.Free;
  end;
end;
//TMyApp END

//TImagePreview BEGIN
procedure TImagePreview.OnWheelMove(const Shift: Integer);
begin
  if MCInRect then
  begin
    if Shift > 0 then
    Scale := Scale * 1.1
    else
    Scale := Scale * 0.9;
  end;
end;

constructor TImagePreview.Create;
begin
  inherited Create;
  CurItem := 0;
  SetLength(Items, 64 * 64);
  ItemCount := 0;
  Scale := 1;
end;

destructor TImagePreview.Destroy;
begin
  inherited Destroy;
end;

procedure TImagePreview.Render;
  var i, j: Integer;
  var Im0, Im1: Integer;
  var g: Single;
  var sr: TRect;
  var x, y: Single;
  var s: AnsiString;
begin
  with App do
  begin
    Render.TextureClear();
    Render2D.BaseVertexIndex := 0;
    Render2D.DrawBegin(ptTriangleList);
    with Render2D do
    begin
      AddPos(R.Left, R.Top + bs);
      AddPos(R.Left + bs, R.Top);
      AddPos(R.Left + bs, R.Top + bs);
      AddPos(R.Right - bs, R.Top);
      AddPos(R.Right, R.Top + bs);
      AddPos(R.Right - bs, R.Top + bs);
      AddPos(R.Right, R.Bottom - bs);
      AddPos(R.Right - bs, R.Bottom);
      AddPos(R.Right - bs, R.Bottom - bs);
      AddPos(R.Left + bs, R.Bottom);
      AddPos(R.Left, R.Bottom - bs);
      AddPos(R.Left + bs, R.Bottom - bs);
      AddCol($00ffffff, 2); AddCol($ffffffff);
      AddCol($00ffffff, 2); AddCol($ffffffff);
      AddCol($00ffffff, 2); AddCol($ffffffff);
      AddCol($00ffffff, 2); AddCol($ffffffff);
      AddFace(0, 1, 2);
      AddFace(2, 1, 3);
      AddFace(2, 3, 5);
      AddFace(3, 4, 5);
      AddFace(8, 5, 4);
      AddFace(8, 4, 6);
      AddFace(7, 8, 6);
      AddFace(9, 8, 7);
      AddFace(9, 11, 8);
      AddFace(9, 10, 11);
      AddFace(10, 0, 2);
      AddFace(10, 2, 11);
      AddFace(11, 2, 5);
      AddFace(11, 5, 8);
    end;
    Render2D.DrawEnd;
    sr := Rect(R.Left + bs, R.Top + bs, R.Right - bs, R.Bottom - bs);
    Gfx.RenderStates.ScissorTestEnable := True;
    Gfx.Device.SetScissorRect(@sr);
    ItemCount := 0;
    for j := 0 to Assembly.RowCount - 1 do
    for i := 0 to Assembly.ColCount - 1 do
    begin
      if Assembly.Items[i, j].Occupied
      and Assembly.Items[i, j].Image.Loaded
      and not Assembly.Items[i, j].Image.Invalid then
      begin
        Items[ItemCount] := Assembly.Items[i, j].Image;
        Inc(ItemCount);
      end;
    end;
    if ItemCount > 0 then
    begin
      x := sr.Left + (sr.Right - sr.Left - Assembly.PatWidth) * 0.5;
      y := sr.Top + (sr.Bottom - sr.Top - Assembly.PatHeight) * 0.5;
      while CurItem >= ItemCount do
      CurItem := CurItem - ItemCount;
      Im0 := Trunc(CurItem);
      {Im1 := (Im0 + 1) mod ItemCount;
      g := Frac(CurItem);
      Render2D.DrawRect(
        R.Left + bs, R.Top + bs,
        Items[Im1].Texture.Width,
        Items[Im1].Texture.Height,
        G2Color($ff, $ff, $ff, Trunc(g * 255)),
        Items[Im1].Texture
      );}
      RenderModes.FilteringLinear();
      Render2D.DrawRect(
        x, y,
        Assembly.PatWidth,
        Assembly.PatHeight,
        $ffffffff,
        Items[Im0].Texture,
        0, Scale, Scale, False, False
      );
      RenderModes.FilteringPoint();
    end
    else
    begin
      s := 'No images!';
      Font.Print(
        sr.Left + (sr.Right - sr.Left - Font.GetTextWidth(s)) * 0.5,
        sr.Top + (sr.Bottom - sr.Top - Font.GetTextHeight(s)) * 0.5,
        $ff000000,
        s
      );
    end;
    Gfx.RenderStates.ScissorTestEnable := False;
    Font.Print(sr.Left + 4, sr.Bottom - Font.GetTextHeight('A') - 4, $ff000000, IntToStr(Trunc(Scale * 100)) + '%');
  end;
end;

procedure TImagePreview.Update;
begin
  CurItem := CurItem + App.PreviewSpeed.Value / App.Tmr.TargetUPS;
end;
//TImagePreview END

//TScene BEGIN
constructor TScene.Create;
begin
  inherited Create;
  m_X := 0;
  m_Y := 0;
  m_W := 256;
  m_H := 256;
end;

destructor TScene.Destroy;
begin
  inherited Destroy;
end;

procedure TScene.Render;
begin
  with App do
  begin
    
  end;
end;

procedure TScene.Update;
begin

end;
//TScene END

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  App.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Constraints.MinWidth := 800;
  Form1.Constraints.MinHeight := 600;
  Form1.ClientWidth := 800;
  Form1.ClientHeight := 600;
  App := TMyApp.Create;
  App.Handle := Form1.Handle;
  App.Gfx.InitParams.Width := Form1.ClientWidth;
  App.Gfx.InitParams.Height := Form1.ClientHeight;
  App.Initialize;
  DragAcceptFiles(Form1.Handle, True);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if Assigned(App)
  and App.g2.Initialized then
  with App do
  begin
    Gfx.Params.Width := Form1.ClientWidth;
    Gfx.Params.Height := Form1.ClientHeight;
    Gfx.Params.Apply;
    Tmr.OnTimer;
  end;
end;

procedure TForm1.AcceptDropFiles(var msg: TMessage);
  var FileName: array[Byte] of WideChar;
  var i, c: Integer;
  var ext, f: String;
begin
  c := DragQueryFileW(
    msg.WParam,
    $ffffffff,
    @FileName,
    255
  );
  for i := 0 to c - 1 do
  begin
    DragQueryFileW(
      msg.WParam,
      i,
      @FileName,
       255
    );
    f := FileName;
    ext := LowerCase(ExtractFileExt(f));
    if (ext = '.jpg')
    or (ext = '.jpeg')
    or (ext = '.bmp')
    or (ext = '.png')
    or (ext = '.tga')
    or (ext = '.dds')
    or (ext = '.hdr')
    or (ext = '.dib')
    or (ext = '.ppm')
    or (ext = '.pfm') then
    begin
      App.Images.AddImage(f);
      App.Menu.SetItem(0);
    end;
  end;
  DragFinish(msg.WParam);
end;

end.
