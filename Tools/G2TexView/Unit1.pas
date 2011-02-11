unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
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
  D3DX9,
  DXTypes,
  Gen2,
  G2Math,
  G2Landscape,
  G2PerlinNoise,
  Res;

type
  TForm1 = class(TForm)
    od1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
  public
    { Public declarations }
  end;

  TMenu = record
  strict private
    type TMenuItem = record
      var Image: TG2Texture2D;
      var Action: TG2ProcObj;
    end;
    var m_Enabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    const Radius = 48;
  public
    var Pos: TG2Vec2;
    var Buttons: array of TMenuItem;
    property Enabled: Boolean read m_Enabled write SetEnabled;
    procedure AddButton(const Image: TG2Texture2D; const Action: TG2ProcObj);
    procedure Render;
    procedure Click;
    function McInBtn(const Index: Integer): Boolean;
  end;

  TTextureMode = (tmNone, tm2D, tmCube);

  TMyApp = class (TG2App)
  public
    var AppClose: Boolean;
    var MgrTextures: TG2TextureMgr;
    var TexGrid: TG2Texture2D;
    var TexBtnBlob: TG2Texture2D;
    var TexBtnBlobH: TG2Texture2D;
    var Font: TG2Font;
    var Menu: TMenu;
    var TexMode: TTextureMode;
    var Tex: TG2TextureBase;
    var TexFile: WideString;
    var Scale: Single;
    var CubeVB: TG2VB;
    var CubeIB: TG2IB;
    procedure OnRender; override;
    procedure OnUpdate; override;
    procedure OnKeyDown(const Key: Byte); override;
    procedure OnKeyUp(const Key: Byte); override;
    procedure OnMouseDown(const Button: Byte); override;
    procedure OnWheelMove(const Shift: Integer); override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure InitCube;
    procedure MenuLoad;
    procedure FillDirFiles(const sl: TStringList);
    procedure OpenNext;
    procedure OpenPrev;
    procedure OpenImage(const f: WideString);
  end;

var
  Form1: TForm1;
  App: TMyApp;
  FormCanResize: Boolean = False;

implementation

{$R *.dfm}

//TMenu BEGIN
procedure TMenu.SetEnabled(const Value: Boolean);
  const Border = Radius + 32;
begin
  m_Enabled := Value;
  if m_Enabled then
  begin
    Pos := App.PlugInput.MousePos;
    Pos.x := Min(Max(Pos.x, Border), App.Gfx.Params.Width - Border);
    Pos.y := Min(Max(Pos.y, Border), App.Gfx.Params.Height - Border);
  end;
end;

procedure TMenu.AddButton(const Image: TG2Texture2D; const Action: TG2ProcObj);
begin
  SetLength(Buttons, Length(Buttons) + 1);
  Buttons[High(Buttons)].Image := Image;
  Buttons[High(Buttons)].Action := Action;
end;

procedure TMenu.Render;
  var i: Integer;
  var Ang, s, c, x, y: Single;
begin
  if m_Enabled
  and (Length(Buttons) > 0) then
  with App do
  begin
    Ang := TwoPi / Length(Buttons);
    for i := 0 to High(Buttons) do
    begin
      G2SinCos(Ang * i, s, c);
      x := Pos.x + c * Radius;
      y := Pos.y + s * Radius;
      if McInBtn(i) then
      Render2D.DrawRect(x - 32, y - 32, TexBtnBlobH)
      else
      Render2D.DrawRect(x - 32, y - 32, TexBtnBlob);
      Render2D.DrawRect(x - Buttons[i].Image.Width * 0.5, y - Buttons[i].Image.Height * 0.5, Buttons[i].Image);
    end;
  end;
end;

procedure TMenu.Click;
  var i: Integer;
begin
  if Length(Buttons) > 0 then
  for i := 0 to High(Buttons) do
  if McInBtn(i) then
  begin
    if Assigned(Buttons[i].Action) then
    Buttons[i].Action;
    Break;
  end;
  Enabled := False;
end;

function TMenu.McInBtn(const Index: Integer): Boolean;
  var Ang, s, c: Single;
  var R: TRect;
begin
  if Length(Buttons) > 0 then
  begin
    Ang := TwoPi / Length(Buttons);
    G2SinCos(Ang * Index, s, c);
    Result := (G2Vec2(Pos.x + c * Radius, Pos.y + s * Radius) - App.PlugInput.MousePos).Len < 32;
  end
  else
  Result := False;
end;
//TMenu END

//TMyApp BEGIN
procedure TMyApp.OnRender;
  var ScrCenter: TG2Vec2;
  var w, h: Single;
  var m: TG2Mat;
begin
  Render.RenderStart;
  Render.Clear(False, True, True, $ff888888);

  Render2D.DrawRect(
    0, 0, Gfx.Params.Width, Gfx.Params.Height,
    Rect(0, 0, Gfx.Params.Width, Gfx.Params.Height),
    TexGrid
  );

  ScrCenter.x := Gfx.Params.Width * 0.5;
  ScrCenter.y := Gfx.Params.Height * 0.5;
  RenderModes.FilteringLinear();
  case TexMode of
    tmNone:
    begin
    end;
    tm2D:
    begin
      w := TG2Texture2D(Tex).Width * Scale;
      h := TG2Texture2D(Tex).Height * Scale;
      Render2D.DrawRect(
        ScrCenter.x - w * 0.5,
        ScrCenter.y - h * 0.5,
        w, h, $ffffffff,
        TG2Texture2D(Tex)
      );
    end;
    tmCube:
    begin
      Cam.SetPerspective(QuatPi, Gfx.Params.Width / Gfx.Params.Height, 0.1, 10);
      Gfx.RenderStates.ZEnable := True;
      m.SetIdentity;
      Gfx.Transforms.W[0] := m;
      Gfx.Transforms.V := Cam.View;
      Gfx.Transforms.P := Cam.Proj;
      Gfx.Transforms.ApplyW(0);
      Gfx.Transforms.ApplyV;
      Gfx.Transforms.ApplyP;
      Gfx.Device.SetTexture(0, Tex.Texture);
      Gfx.TextureStageStages.TextureTransformFlags[0] := D3DTTFF_COUNT3;
      CubeVB.SetToDevice;
      CubeIB.SetToDevice;
      Gfx.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, 8, 0, 12);
      //Prim3D.DrawBox(-0.5, -0.5, -0.5, 1, 1, 1, $ffffffff);
      Gfx.TextureStageStages.TextureTransformFlags[0] := D3DTTFF_COUNT2;
      Gfx.RenderStates.ZEnable := False;
    end;
  end;
  RenderModes.FilteringPoint();

  if TexMode <> tmNone then
  Font.Print(10, 10, $ff000000, TexFile);

  Menu.Render;

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
    if TexMode = tmCube then
    begin
      Cam.DragMode := cdDragPos;
      Cam.UpdateDir := PlugInput.MouseDown[0];
    end;
  end;
end;

procedure TMyApp.OnKeyDown(const Key: Byte);
begin
  if (Key = DIK_SPACE)
  or (Key = DIK_DOWN)
  or (Key = DIK_RIGHT) then
  OpenNext;
  if (Key = DIK_UP)
  or (Key = DIK_LEFT) then
  OpenPrev;
  if Key = DIK_ESCAPE then
  AppClose := True;
end;

procedure TMyApp.OnKeyUp(const Key: Byte);
begin

end;

procedure TMyApp.OnMouseDown(const Button: Byte);
begin
  if Button = 0 then
  Menu.Click;
  if Button = 1 then
  Menu.Enabled := not Menu.Enabled;
end;

procedure TMyApp.OnWheelMove(const Shift: Integer);
begin
  if TexMode <> tmNone then
  begin
    if (Shift > 0) then
    begin
      if Scale < 10 then
      Scale := Scale * 1.1;
    end
    else
    begin
      if Scale > 0.1 then
      Scale := Scale * 0.9;
    end;
  end;
end;

procedure TMyApp.Initialize;
begin
  AppClose := False;
  Tmr.MaxFPS := 0;
  Tmr.Mode := tmWinTimer;
  inherited Initialize;
  Font := Gfx.Shared.RequestFont('Arial', 16);
  g2.RequestMod(TG2TextureMgr, @MgrTextures);
  TexGrid := MgrTextures.CreateTexture2DFromBuffer('Grid', @BinGrid, SizeOf(BinGrid), 1, D3DFMT_DXT5);
  TexBtnBlob := MgrTextures.CreateTexture2DFromBuffer('BtnBlob', @BinBtnBlob, SizeOf(BinBtnBlob), 1, D3DFMT_DXT5);
  TexBtnBlobH := MgrTextures.CreateTexture2DFromBuffer('BtnBlobH', @BinBtnBlobH, SizeOf(BinBtnBlobH), 1, D3DFMT_DXT5);
  MgrTextures.CreateTexture2DFromBuffer('BtnSave', @BinSave, SizeOf(BinSave), 1, D3DFMT_DXT5);
  MgrTextures.CreateTexture2DFromBuffer('BtnLoad', @BinLoad, SizeOf(BinLoad), 1, D3DFMT_DXT5);

  Menu.Enabled := False;
  Menu.AddButton(TG2Texture2D(MgrTextures.FindTexture('BtnSave')), nil);
  Menu.AddButton(TG2Texture2D(MgrTextures.FindTexture('BtnLoad')), MenuLoad);

  TexMode := tmNone;
  Tex := nil;
  TexFile := '';

  CubeVB := TG2VB.Create;
  CubeVB.Initialize(g2);
  CubeIB := TG2IB.Create;
  CubeIB.Initialize(g2);

  InitCube;

  if FileExists(ParamStr(1)) then
  OpenImage(ParamStr(1));

  Tmr.Enabled := True;
end;

procedure TMyApp.Finalize;
begin
  CubeVB.Finalize;
  CubeVB.Free;
  CubeIB.Finalize;
  CubeIB.Free;
  inherited Finalize;
end;

procedure TMyApp.InitCube;
  type TVertex = packed record
    var Position: TG2Vec3;
    var TexCoord: TG2Vec3;
  end;
  type TVertexArray = array[Word] of TVertex;
  type PVertexArray = ^TVertexArray;
  var Vertices: PVertexArray;
  var Indices: PG2Index16Array;
  var i: Integer;
  var CurInd: Integer;
  procedure AddIndicesQuad(const i0, i1, i2, i3: Integer);
  begin
    Indices^[CurInd + 0] := i0;
    Indices^[CurInd + 1] := i1;
    Indices^[CurInd + 2] := i2;
    Indices^[CurInd + 3] := i0;
    Indices^[CurInd + 4] := i2;
    Indices^[CurInd + 5] := i3;
    CurInd := CurInd + 6;
  end;
begin
  CubeVB.Verify(
    SizeOf(TVertex),
    8,
    D3DUSAGE_WRITEONLY,
    D3DFVF_XYZ or D3DFVF_TEX1 or D3DFVF_TEXCOORDSIZE3(0),
    D3DPOOL_MANAGED
  );
  CubeIB.Verify(
    36,
    D3DUSAGE_WRITEONLY,
    D3DFMT_INDEX16,
    D3DPOOL_MANAGED
  );
  CubeVB.Lock(
    0, 8 * CubeVB.Stride, Pointer(Vertices), D3DLOCK_DISCARD
  );
  Vertices[0].Position.SetValue(-0.5, -0.5, -0.5);
  Vertices[1].Position.SetValue(-0.5, -0.5, 0.5);
  Vertices[2].Position.SetValue(0.5, -0.5, 0.5);
  Vertices[3].Position.SetValue(0.5, -0.5, -0.5);
  Vertices[4].Position.SetValue(-0.5, 0.5, -0.5);
  Vertices[5].Position.SetValue(-0.5, 0.5, 0.5);
  Vertices[6].Position.SetValue(0.5, 0.5, 0.5);
  Vertices[7].Position.SetValue(0.5, 0.5, -0.5);
  for i := 0 to 7 do
  Vertices[i].TexCoord := Vertices[i].Position.Normalized;
  CubeVB.UnLock;

  CubeIB.Lock(
    0, 36 * 2, Pointer(Indices), D3DLOCK_DISCARD
  );
  CurInd := 0;
  AddIndicesQuad(3, 2, 1, 0);
  AddIndicesQuad(4, 5, 6, 7);
  AddIndicesQuad(0, 4, 7, 3);
  AddIndicesQuad(3, 7, 6, 2);
  AddIndicesQuad(2, 6, 5, 1);
  AddIndicesQuad(1, 5, 4, 0);
  CubeIB.UnLock;
end;

procedure TMyApp.MenuLoad;
begin
  if Form1.od1.Execute() then
  OpenImage(WideString(Form1.od1.FileName));
end;

procedure TMyApp.FillDirFiles(const sl: TStringList);
  var Dir, Ext: WideString;
  var SR: TSearchRec;
  var Files: TStringList;
  var i: Integer;
begin
  Dir := ExtractFileDir(TexFile) + '\';
  FindFirst(Dir + '*.*', 0, SR);
  repeat
    Ext := LowerCase(ExtractFileExt(SR.Name));
    if (Ext = '.bmp')
    or (Ext = '.jpg')
    or (Ext = '.jpeg')
    or (Ext = '.tga')
    or (Ext = '.png')
    or (Ext = '.ppm')
    or (Ext = '.dib')
    or (Ext = '.hdr')
    or (Ext = '.pfm')
    or (Ext = '.dds') then
    sl.Add(UpperCase(ExtractFileName(SR.Name)));
  until FindNext(SR) <> 0;
  sl.Sort;
end;

procedure TMyApp.OpenNext;
  var f, Dir: WideString;
  var Files: TStringList;
  var i: Integer;
begin
  if TexMode <> tmNone then
  begin
    f := UpperCase(ExtractFileName(TexFile));
    Dir := ExtractFileDir(TexFile) + '\';
    Files := TStringList.Create;
    FillDirFiles(Files);
    if Files.Count > 1 then
    for i := 0 to Files.Count - 1 do
    if Files[i] = f then
    begin
      OpenImage(Dir + Files[(i + 1) mod Files.Count]);
      Break;
    end;
    Files.Free;
  end;
end;

procedure TMyApp.OpenPrev;
  var f, Dir: WideString;
  var Files: TStringList;
  var i, n: Integer;
begin
  if TexMode <> tmNone then
  begin
    f := UpperCase(ExtractFileName(TexFile));
    Dir := ExtractFileDir(TexFile) + '\';
    Files := TStringList.Create;
    FillDirFiles(Files);
    if Files.Count > 1 then
    for i := 0 to Files.Count - 1 do
    if Files[i] = f then
    begin
      n := i - 1;
      if n < 0 then n := Files.Count - 1;
      OpenImage(Dir + Files[n]);
      Break;
    end;
    Files.Free;
  end;
end;

procedure TMyApp.OpenImage(const f: WideString);
  var ImageInfo: TD3DXImageInfo;
begin
  if TexMode <> tmNone then
  begin
    Tex.Free;
    Tex := nil;
    TexMode := tmNone;
    TexFile := '';
  end;
  Scale := 1;
  if Succeeded(D3DXGetImageInfoFromFileW(PWideChar(f), ImageInfo)) then
  begin
    case ImageInfo.ResourceType of
      D3DRTYPE_TEXTURE:
      begin
        TexMode := tm2D;
        Tex := MgrTextures.CreateTexture2DFromFile('Tex', f, 8, D3DFMT_A8R8G8B8);
        TexFile := f;
      end;
      D3DRTYPE_CUBETEXTURE:
      begin
        TexMode := tmCube;
        Tex := MgrTextures.CreateTextureCubeFromFile('Tex', f, 8, D3DFMT_A8R8G8B8);
        TexFile := f;
        Cam.SetView(0, 0, -1.8, 0, 0, 0, 0, 1, 0);
      end;
    end;
  end;
end;
//TMyApp END

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormCanResize := False;
  App.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.ClientWidth := 800;
  Form1.ClientHeight := 600;
  App := TMyApp.Create;
  App.Handle := Form1.Handle;
  App.Gfx.InitParams.Width := Form1.ClientWidth;
  App.Gfx.InitParams.Height := Form1.ClientHeight;
  App.Initialize;
  FormCanResize := True;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if FormCanResize then
  with App do
  begin
    Menu.Enabled := False;
    Gfx.Params.Width := Form1.ClientWidth;
    Gfx.Params.Height := Form1.ClientHeight;
    Gfx.Params.Apply;
  end;
end;

end.
