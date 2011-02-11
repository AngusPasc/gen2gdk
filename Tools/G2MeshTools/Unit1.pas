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
  DXTypes,
  Gen2,
  G2Math,
  G2Landscape,
  G2PerlinNoise,
  G2MeshLoaderG2M,
  G2MeshLoaderX,
  Res;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TTopMenu = class;
  TScene = class;

  TMyApp = class (TG2App)
  public
    AppClose: Boolean;
    Font: TG2Font;
    MgrTextures: TG2TextureMgr;
    MgrMeshTex: TG2TextureMgr;
    MgrMeshes: TG2MeshMgr;
    Mesh: TG2Mesh;
    MeshInst: TG2MeshInst;
    TexBorder: TG2Texture2D;
    TexTopBG: TG2Texture2D;
    TexGrad: TG2Texture2D;
    TexDefDiffuse: TG2Texture2D;
    TexDefSpecular: TG2Texture2D;
    TexDefNormals: TG2Texture2D;
    bg: array [0..1] of array[0..7] of Single;
    bgGrad: Single;
    od1: TOpenDialog;
    Scene: TScene;
    TopMenu: TTopMenu;
    procedure OnRender; override;
    procedure OnUpdate; override;
    procedure OnKeyDown(const Key: Byte); override;
    procedure OnKeyUp(const Key: Byte); override;
    procedure OnMouseDown(const Button: Byte); override;
    procedure OnMouseMove(const Shift: TPoint); override;
    procedure OnWheelMove(const Shift: Integer); override;
    procedure RenderBorder;
    procedure RenderTitle;
    procedure RenderBackground;
    procedure LoadFileBtn;
    procedure LoadFile(const f: String = '');
    procedure Initialize; override;
    procedure Finalize; override;
  end;

  TTopMenu = class
  strict private
    type TTopMenuItem = record
      var Text: AnsiString;
      var Proc: TG2ProcObj;
    end;
    type TTopMenuButton = record
      var Image: TG2Texture2D;
      var Controller: PBoolean;
    end;
    var m_X: Single;
    var m_Y: Single;
    var m_BtnX: Integer;
    var m_BtnY: Integer;
    var m_Items: array of TTopMenuItem;
    var m_Buttons: array of TTopMenuButton;
  public
    constructor Create;
    destructor Destroy; override;
    property X: Single read m_X write m_X;
    property Y: Single read m_Y write m_Y;
    procedure AddItem(const Text: AnsiString; const Proc: TG2ProcObj);
    procedure AddButton(const Image: TG2Texture2D; const Controller: PBoolean);
    procedure Render;
    procedure MouseClick;
    function MouseInItem(const Ind: Integer): Boolean;
    function MouseInButton(const Ind: Integer): Boolean;
  end;

  TScene = class
  strict private
    m_X: Single;
    m_Y: Single;
    m_W: Single;
    m_H: Single;
    m_BaseScale: Single;
    m_BasePos: TG2Vec3;
    m_LookAt: TG2Vec3;
    m_InitLookAt: TG2Vec3;
    m_AngY, m_AngX: Single;
    m_Scale: Single;
    m_WireFrame: Boolean;
    m_Structure: Boolean;
    m_ShowBBox: Boolean;
    m_Font: TG2Font;
    m_StructScroll: Integer;
    m_StructureArr: array of AnsiString;
    m_PlugInput: TG2PlugInput;
    m_MDInScreen: array [0..2] of Boolean;
    procedure AdjustScroll;
    procedure OnMouseDown(const Button: Byte);
    procedure OnMouseUp(const Button: Byte);
    procedure OnMouseMove(const Shift: TPoint);
    procedure OnWheelMove(const Shift: Integer);
    procedure RenderBBox(const Box: TG2AABox);
  public
    constructor Create;
    destructor Destroy; override;
    property BaseScale: Single read m_BaseScale write m_BaseScale;
    property BasePos: TG2Vec3 read m_BasePos write m_BasePos;
    property LookAt: TG2Vec3 read m_LookAt write m_LookAt;
    property InitLookAt: TG2Vec3 read m_InitLookAt write m_InitLookAt;
    property AngY: Single read m_AngY write m_AngY;
    property AngX: Single read m_AngX write m_AngX;
    property Scale: Single read m_Scale write m_Scale;
    property WireFrame: Boolean read m_WireFrame write m_WireFrame;
    property Structure: Boolean read m_Structure write m_Structure;
    property ShowBBox: Boolean read m_ShowBBox write m_ShowBBox;
    procedure GenerateStructure;
    procedure RenderBorder;
    procedure RenderScene;
    procedure RenderStructure;
    procedure Render;
    procedure Update;
    procedure Adjust;
  end;

var
  Form1: TForm1;
  App: TMyApp;

implementation

{$R *.dfm}

//TMyApp BEGIN
procedure TMyApp.OnRender;
begin
  Render.RenderStart;
  Render.Clear(False, True, True, $ff888888);

  RenderBackground;
  RenderTitle;
  RenderBorder;

  TopMenu.Render;

  Scene.Render;

  Font.Print(10, 10, $ff0000ff, 'FPS: ' + IntToStr(Tmr.FPS));

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
    Scene.Update;
    Cam.Update;
    bgGrad := bgGrad + 0.0015;
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

procedure TMyApp.OnMouseDown(const Button: Byte);
begin
  TopMenu.MouseClick;
end;

procedure TMyApp.OnMouseMove(const Shift: TPoint);
begin
  
end;

procedure TMyApp.OnWheelMove(const Shift: Integer);
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

  Render2D.BaseVertexIndex := 0;
  Render2D.DrawBegin(ptTriangleList);

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
  Render2D.BaseVertexIndex := 0;
  Render2D.DrawBegin(ptTriangleStrip);

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
  Render2D.BaseVertexIndex := 0;
  Render2D.DrawBegin(ptTriangleList);
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

procedure TMyApp.LoadFileBtn;
begin
  LoadFile('');
end;

procedure TMyApp.LoadFile(const f: String = '');
  var i: Integer;
  var FDir: AnsiString;
  var BBox: TG2AABox;
  var FName: String;
begin
  if (Length(f) > 0) and (FileExists(f)) then
  FName := f
  else if od1.Execute then
  FName := od1.FileName;
  if FileExists(FName) then
  begin
    if Assigned(MeshInst) then
    begin
      MeshInst.Free;
      MeshInst := nil;
    end;
    if Assigned(Mesh) then
    begin
      Mesh.Finalize;
      Mesh.Free;
      Mesh := nil;
    end;
    MgrMeshTex.FreeTextures;
    Mesh := MgrMeshes.CreateMeshFromFile('Mesh', FName);
    if Assigned(Mesh) then
    begin
      FDir := ExtractFileDir(FName) + '\';
      MeshInst := Mesh.InstanceCreate;
      for i := 0 to Mesh.MaterialCount - 1 do
      begin
        if FileExists(FDir + Mesh.Materials[i].DiffuseMap) then
        MeshInst.Materials[i].MapDiffuse := MgrMeshTex.CreateTexture2DFromFile(
          Mesh.Materials[i].DiffuseMap,
          FDir + Mesh.Materials[i].DiffuseMap
        )
        else
        MeshInst.Materials[i].MapDiffuse := TexDefDiffuse;
        if FileExists(FDir + Mesh.Materials[i].SpecularMap) then
        MeshInst.Materials[i].MapSpecular := MgrMeshTex.CreateTexture2DFromFile(
          Mesh.Materials[i].SpecularMap,
          FDir + Mesh.Materials[i].SpecularMap
        )
        else
        MeshInst.Materials[i].MapSpecular := TexDefSpecular;
        if FileExists(FDir + Mesh.Materials[i].NormalMap) then
        MeshInst.Materials[i].MapNormals := MgrMeshTex.CreateTexture2DFromFile(
          Mesh.Materials[i].NormalMap,
          FDir + Mesh.Materials[i].NormalMap
        )
        else
        MeshInst.Materials[i].MapNormals := TexDefNormals;
      end;
      BBox := MeshInst.BBox.AABox;
      Scene.BaseScale := Min(
        Min(
          15 / Abs(BBox.MaxV.x - BBox.MinV.x),
          15 / Abs(BBox.MaxV.y - BBox.MinV.y)
        ),
        15 / Abs(BBox.MaxV.z - BBox.MinV.z)
      );
      Scene.BasePos := G2Vec3(
        -(BBox.MaxV.x + BBox.MinV.x) * 0.5,
        -BBox.MinV.y,
        -(BBox.MaxV.z + BBox.MinV.z) * 0.5
      );
      Scene.LookAt := (BBox.MaxV + BBox.MinV) * 0.5 * Scene.BaseScale;
      Scene.InitLookAt := Scene.LookAt;
      Scene.GenerateStructure;
    end;
  end;
end;

procedure TMyApp.Initialize;
var
  i, j: Integer;
begin
  AppClose := False;
  Tmr.MaxFPS := 0;
  Gfx.InitParams.MultiThreaded := True;
  inherited Initialize;
  g2.RequestMod(TG2TextureMgr, @MgrTextures);
  g2.RequestMod(TG2TextureMgr, @MgrMeshTex);
  g2.RequestMod(TG2MeshMgr, @MgrMeshes);
  Font := Gfx.Shared.RequestFont('Arial', 12);
  Cam.SetPerspective(QuatPi, 4/3, 0.1, 10000);
  Cam.SetView(250, 300, 250, 500, 100, 500, 0, 1, 0);
  Gfx.Transforms.P := Cam.Proj;
  Gfx.Transforms.V := Cam.View;
  Gfx.Transforms.ApplyV;
  Gfx.Transforms.ApplyP;

  TexBorder := MgrTextures.CreateTexture2DFromBuffer('Border', @BinBorder, SizeOf(BinBorder), 1);
  TexTopBG := MgrTextures.CreateTexture2DFromBuffer('TopBG', @BinTopBG, SizeOf(BinTopBG), 1);
  TexGrad := MgrTextures.CreateTexture2DFromBuffer('Grad', @BinGrad, SizeOf(BinGrad), 1);

  TexDefDiffuse := MgrTextures.CreateTexture2D('DefDiffuse', 4, 4, 1, 0, D3DFMT_A8R8G8B8);
  TexDefSpecular := MgrTextures.CreateTexture2D('DefSpecular', 4, 4, 1, 0, D3DFMT_A8R8G8B8);
  TexDefNormals := MgrTextures.CreateTexture2D('DefNormals', 4, 4, 1, 0, D3DFMT_A8R8G8B8);
  TexDefDiffuse.Surfaces[0].Lock(D3DLOCK_DISCARD);
  TexDefSpecular.Surfaces[0].Lock(D3DLOCK_DISCARD);
  TexDefNormals.Surfaces[0].Lock(D3DLOCK_DISCARD);
  for i := 0 to 3 do
  for j := 0 to 3 do
  begin
    TexDefDiffuse.Surfaces[0].Pixels[i, j] := $ffffffff;
    TexDefSpecular.Surfaces[0].Pixels[i, j] := $ff808080;
    TexDefNormals.Surfaces[0].Pixels[i, j] := $ff8080ff;
  end;
  TexDefDiffuse.Surfaces[0].UnLock;
  TexDefSpecular.Surfaces[0].UnLock;
  TexDefNormals.Surfaces[0].UnLock;

  Randomize;
  for i := 0 to High(bg[0]) do
  begin
    bg[0][i] := 0.5 - Random(31) * 0.01;
    bg[1][i] := 0.5 + Random(31) * 0.01;
  end;
  bgGrad := 0;

  od1 := TOpenDialog.Create(Form1);

  Scene := TScene.Create;
  Scene.Adjust;

  TopMenu := TTopMenu.Create;
  TopMenu.AddItem('Load', LoadFileBtn);

  TopMenu.AddButton(
    MgrTextures.CreateTexture2DFromBuffer('Wireframe', @BinWireframe, SizeOf(BinWireframe), 2),
    @Scene.WireFrame
  );
  TopMenu.AddButton(
    MgrTextures.CreateTexture2DFromBuffer('BBox', @BinBBox, SizeOf(BinBBox), 2),
    @Scene.ShowBBox
  );
  TopMenu.AddButton(
    MgrTextures.CreateTexture2DFromBuffer('Structure', @BinStructure, SizeOf(BinStructure), 2),
    @Scene.Structure
  );

  Tmr.Enabled := True;

  if FileExists(ParamStr(1)) then
  begin
    G2WriteLog('Opening file: ' + ParamStr(1));
    LoadFile(ParamStr(1));
  end;
end;

procedure TMyApp.Finalize;
begin
  if Assigned(MeshInst) then
  begin
    MeshInst.Free;
    MeshInst := nil;
  end;
  TopMenu.Free;
  Scene.Free;
  od1.Free;
  inherited Finalize;
end;
//TMyApp END

//TTopMenu BEGIN
constructor TTopMenu.Create;
begin
  inherited Create;
  m_X := 16;
  m_Y := 54;
  m_BtnX := 10;
  m_BtnY := 118;
end;

destructor TTopMenu.Destroy;
begin
  inherited Destroy;
end;

procedure TTopMenu.AddItem(const Text: AnsiString; const Proc: TG2ProcObj);
begin
  SetLength(m_Items, Length(m_Items) + 1);
  m_Items[High(m_Items)].Text := Text;
  m_Items[High(m_Items)].Proc := Proc;
end;

procedure TTopMenu.AddButton(const Image: TG2Texture2D; const Controller: PBoolean);
begin
  SetLength(m_Buttons, Length(m_Buttons) + 1);
  m_Buttons[High(m_Buttons)].Image := Image;
  m_Buttons[High(m_Buttons)].Controller := Controller;
end;

procedure TTopMenu.Render;
var
  i, w, n: Integer;
  c, c1, c2: TG2Color;
  x0, x1, x2, x3, y0, y1, y2, y3: Single;
begin
  with App do
  begin
    w := 0;
    for i := 0 to High(m_Items) do
    begin
      n := Font.GetTextWidth(m_Items[i].Text) + 10;
      c := $ff000000;
      if MouseInItem(i) then
      begin
        Render.TextureClear();
        Prim2D.DrawRect4col(
          m_X + w, m_Y, n, 20,
          $0048aafd, $0048aafd,
          $aa48aafd, $aa48aafd
        );
        c := $ffffffff;
      end;
      Font.Print(m_X + w + 5, m_Y + 2, c, m_Items[i].Text);
      w := w + n;
    end;
    Render.TextureClear();
    Prim2D.DrawRect4col(
      m_X, m_Y + 18, w, 2,
      $00000000, $00000000,
      $ff000000, $ff000000
    );
    Prim2D.DrawRect4col(
      m_X, m_Y + 20, w, 2,
      $ff000000, $ff000000,
      $00000000, $00000000
    );
    x0 := m_BtnX;
    x1 := x0 + 4;
    x2 := x1 + 32;
    x3 := x2 + 4;
    Render2D.BaseVertexIndex := 0;
    Render2D.DrawBegin(ptTriangleList);
    for i := 0 to High(m_Buttons) do
    begin
      if m_Buttons[i].Controller^ then
      begin
        c1 := $00ffffff;
        c2 := $ffffffff;
      end
      else
      begin
        c1 := $00aaaaaa;
        c2 := $ffaaaaaa;
      end;
      y0 := m_BtnY + 42 * i;
      y1 := y0 + 4;
      y2 := y1 + 32;
      y3 := y2 + 4;
      if PtInRect(
        Rect(Trunc(x1), Trunc(y1), Trunc(x2), Trunc(y2)),
        PlugInput.MousePos
      ) then
      c1 := $00ff0000;
      Render2D.AddPos(x0, y0);
      Render2D.AddPos(x3, y0);
      Render2D.AddPos(x3, y3);
      Render2D.AddPos(x0, y3);
      Render2D.AddPos(x1, y1);
      Render2D.AddPos(x2, y1);
      Render2D.AddPos(x2, y2);
      Render2D.AddPos(x1, y2);
      Render2D.AddCol(c1, 4);
      Render2D.AddCol(c2, 4);
      Render2D.AddFace(0, 1, 4);
      Render2D.AddFace(4, 1, 5);
      Render2D.AddFace(1, 2, 5);
      Render2D.AddFace(5, 2, 6);
      Render2D.AddFace(2, 3, 6);
      Render2D.AddFace(6, 3, 7);
      Render2D.AddFace(3, 0, 7);
      Render2D.AddFace(7, 0, 4);
      Render2D.BaseVertexIndex := Render2D.BaseVertexIndex + 8;
      Render2D.DrawRect(x1, y1, 32, 32, c2, m_Buttons[i].Image);
    end;
    Render.TextureClear();
    Render2D.DrawEnd;
  end;
end;

procedure TTopMenu.MouseClick;
var
  i: Integer;
begin
  for i := 0 to High(m_Items) do
  if MouseInItem(i)
  and Assigned(m_Items[i].Proc) then
  m_Items[i].Proc;
  for i := 0 to High(m_Buttons) do
  if MouseInButton(i) then
  m_Buttons[i].Controller^ := not m_Buttons[i].Controller^;
end;

function TTopMenu.MouseInItem(const Ind: Integer): Boolean;
var
  i, w, n: Integer;
begin
  w := 0;
  for i := 0 to High(m_Items) do
  begin
    n := App.Font.GetTextWidth(m_Items[i].Text) + 10;
    if i = Ind then
    begin
      Result := PtInRect(
        Rect(Round(m_X + w), Round(m_Y), Round(m_X + n + w), Round(m_Y + 20)),
        App.PlugInput.MousePos
      );
      Exit;
    end;
    w := w + n;
  end;
  Result := False;
end;

function TTopMenu.MouseInButton(const Ind: Integer): Boolean;
  var r: TRect;
begin
  r.Left := m_BtnX;
  r.Right := m_BtnX + 40;
  r.Top := m_BtnY + 42 * Ind;
  r.Bottom := r.Top + 40;
  Result := PtInRect(r, App.PlugInput.MousePos);
end;
//TTopMenu END

//TScene BEGIN
procedure TScene.AdjustScroll;
  var sh: Integer;
begin
  sh := Length(m_StructureArr) * m_Font.GetTextHeight('A') + 8;
  if m_H + m_StructScroll > sh then
  m_StructScroll := sh - Trunc(m_H);
  if m_StructScroll < 0 then
  m_StructScroll := 0;
end;

procedure TScene.OnMouseDown(const Button: Byte);
begin
  if Button < 3 then
  m_MDInScreen[Button] := PtInRect(
    Rect(Round(m_X), Round(m_Y), Round(m_X + m_W), Round(m_Y + m_H)),
    m_PlugInput.MousePos
  );
end;

procedure TScene.OnMouseUp(const Button: Byte);
begin
  if Button < 3 then
  m_MDInScreen[Button] := False;
end;

procedure TScene.OnMouseMove(const Shift: TPoint);
  var CamSpaceX, CamSpaceY, CamSpaceZ: TG2Vec3;
  var SpaceLimit: Single;
begin
  if m_MDInScreen[1] and m_PlugInput.MouseDown[1] then
  begin
    m_AngY := m_AngY - Shift.X * 0.01;
    m_AngX := m_AngX - Shift.Y * 0.01;
    if m_AngX < -(HalfPi - 0.01) then m_AngX := -(HalfPi - 0.01);
    if m_AngX > (HalfPi - 0.01) then m_AngX := (HalfPi - 0.01);
  end;
  if m_MDInScreen[2] and m_PlugInput.MouseDown[2] then
  begin
    App.Cam.GetViewSpaceVectors(CamSpaceX, CamSpaceY, CamSpaceZ);
    m_LookAt := m_LookAt + (CamSpaceX * (Shift.X * (0.1 / m_Scale)) + CamSpaceY * (Shift.Y * (0.1 / m_Scale)));
    SpaceLimit := 80;
    if m_LookAt.x > SpaceLimit then m_LookAt.x := SpaceLimit;
    if m_LookAt.y > SpaceLimit then m_LookAt.y := SpaceLimit;
    if m_LookAt.z > SpaceLimit then m_LookAt.z := SpaceLimit;
    if m_LookAt.x < -SpaceLimit then m_LookAt.x := -SpaceLimit;
    if m_LookAt.y < -SpaceLimit then m_LookAt.y := -SpaceLimit;
    if m_LookAt.z < -SpaceLimit then m_LookAt.z := -SpaceLimit;
  end;
end;

procedure TScene.OnWheelMove(const Shift: Integer);
begin
  if m_Structure then
  begin
    m_StructScroll := m_StructScroll - Shift div 2;
    AdjustScroll;
  end
  else
  m_Scale := Min(Max(m_Scale + Shift * 0.001, 0.5), 2);
end;

procedure TScene.RenderBBox(const Box: TG2AABox);
begin
  App.Render.TextureClear();
  App.Prim3D.DrawLine(G2Vec3(Box.MinV.x, Box.MinV.y, Box.MinV.z), G2Vec3(Box.MaxV.x, Box.MinV.y, Box.MinV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MaxV.x, Box.MinV.y, Box.MinV.z), G2Vec3(Box.MaxV.x, Box.MinV.y, Box.MaxV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MaxV.x, Box.MinV.y, Box.MaxV.z), G2Vec3(Box.MinV.x, Box.MinV.y, Box.MaxV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MinV.x, Box.MinV.y, Box.MaxV.z), G2Vec3(Box.MinV.x, Box.MinV.y, Box.MinV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MinV.x, Box.MaxV.y, Box.MinV.z), G2Vec3(Box.MaxV.x, Box.MaxV.y, Box.MinV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MaxV.x, Box.MaxV.y, Box.MinV.z), G2Vec3(Box.MaxV.x, Box.MaxV.y, Box.MaxV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MaxV.x, Box.MaxV.y, Box.MaxV.z), G2Vec3(Box.MinV.x, Box.MaxV.y, Box.MaxV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MinV.x, Box.MaxV.y, Box.MaxV.z), G2Vec3(Box.MinV.x, Box.MaxV.y, Box.MinV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MinV.x, Box.MinV.y, Box.MinV.z), G2Vec3(Box.MinV.x, Box.MaxV.y, Box.MinV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MaxV.x, Box.MinV.y, Box.MinV.z), G2Vec3(Box.MaxV.x, Box.MaxV.y, Box.MinV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MinV.x, Box.MinV.y, Box.MaxV.z), G2Vec3(Box.MinV.x, Box.MaxV.y, Box.MaxV.z), $ff000000);
  App.Prim3D.DrawLine(G2Vec3(Box.MaxV.x, Box.MinV.y, Box.MaxV.z), G2Vec3(Box.MaxV.x, Box.MaxV.y, Box.MaxV.z), $ff000000);
end;

constructor TScene.Create;
begin
  inherited Create;
  m_X := 0;
  m_Y := 0;
  m_W := 256;
  m_H := 256;
  m_BaseScale := 1;
  m_BasePos.SetValue(0, 0, 0);
  m_AngY := 0;
  m_AngX := 0;
  m_Scale := 1;
  m_WireFrame := False;
  m_Structure := False;
  m_ShowBBox := False;
  m_Font := App.Gfx.Shared.RequestFont('Courier New', 8);
  m_StructScroll := 0;
  ZeroMemory(@m_MDInScreen, 3);
  App.g2.RequestPlug(TG2PlugInput, @m_PlugInput);
  m_PlugInput.OnMouseDown := OnMouseDown;
  m_PlugInput.OnMouseUp := OnMouseUp;
  m_PlugInput.OnWheelMove := OnWheelMove;
  m_PlugInput.OnMouseMove := OnMouseMove;
end;

destructor TScene.Destroy;
begin
  App.g2.ReleasePlug(@m_PlugInput);
  inherited Destroy;
end;

procedure TScene.GenerateStructure;
  var m: TG2Mesh;
  var l, ct, pnc: Integer;
  var TabRow: array of Integer;
  procedure PrintList(const Str: AnsiString; const Tabs: Integer = 0);
    var Str2: AnsiString;
    var t: Integer;
  begin
    Str2 := '';
    for t := 0 to Tabs - 1 do
    begin
      if TabRow[t] > 0 then
      begin
        if t = Tabs - 1 then
        Str2 := Str2 + '|_'
        else
        Str2 := Str2 + '| ';
      end
      else
      Str2 := Str2 + '  ';
    end;
    SetLength(m_StructureArr, Length(m_StructureArr) + 1);
    m_StructureArr[High(m_StructureArr)] := Str2 + Str;
    Inc(l);
  end;
  procedure PrintNode(const n: Integer);
    var s: AnsiString;
    var i, j: Integer;
  begin
    s := 'Node[' + m.Nodes[n].Name + ']';
    PrintList(s, ct);
    Dec(TabRow[ct - 1]);
    TabRow[ct] := 1;
    for j := 0 to m.GeomCount - 1 do
    if m.Geoms[j].NodeID = n then
    begin
      PrintList('Geom', ct + 1);
      if Length(m.Nodes[n].SlaveID) = 0 then
      TabRow[ct] := 0;
      if Length(TabRow) <= ct then
      SetLength(TabRow, Length(TabRow) + 8);
      Inc(ct);
      TabRow[ct] := 1;
      if m.Geoms[j].Skinned then
      begin
        PrintList('Skinned', ct + 1);
        PrintList('Bone Count: ' + IntToStr(m.Geoms[j].BoneCount), ct + 1);
      end;
      PrintList('Vertex Count: ' + IntToStr(m.Geoms[j].Mesh.GetNumVertices), ct + 1);
      PrintList('Face Count: ' + IntToStr(m.Geoms[j].Mesh.GetNumFaces), ct + 1);
      if m.Geoms[j].MaterialCount > 0 then
      begin
        PrintList('Materials[' + IntToStr(m.Geoms[j].MaterialCount) + ']:', ct + 1);
        if Length(TabRow) <= ct then
        SetLength(TabRow, Length(TabRow) + 8);
        TabRow[ct] := 0;
        Inc(ct);
        TabRow[ct] := 1;
        for i := 0 to m.Geoms[j].MaterialCount - 1 do
        PrintList('Material[' + m.Materials[m.Geoms[j].Materials[i]].Name + ']', ct + 1);
        TabRow[ct] := 0;
        Dec(ct);
        TabRow[ct] := 1;
      end;
      Dec(ct);
    end;
    TabRow[ct] := Length(m.Nodes[n].SlaveID);
    if Length(m.Nodes[n].SlaveID) > 0 then
    begin
      if Length(TabRow) <= ct then
      SetLength(TabRow, Length(TabRow) + 8);
      Inc(ct);
      for i := 0 to High(m.Nodes[n].SlaveID) do
      PrintNode(m.Nodes[n].SlaveID[i]);
      Dec(ct);
    end;
  end;
  var i, j: Integer;
  var s: AnsiString;
begin
  with App do
  begin
    if Assigned(MeshInst) then
    begin
      m_StructureArr := nil;
      SetLength(TabRow, 2);
      ZeroMemory(@TabRow[0], Length(TabRow));
      m := MeshInst.Mesh;
      l := 0;
      if m.NodeCount > 0 then
      begin
        PrintList('Nodes[' + IntToStr(m.NodeCount) + ']:');
        pnc := 0;
        for i := 0 to m.NodeCount - 1 do
        if m.Nodes[i].OwnerID = -1 then
        pnc := pnc + 1;
        TabRow[0] := pnc;
        ct := 1;
        for i := 0 to m.NodeCount - 1 do
        if m.Nodes[i].OwnerID = -1 then
        PrintNode(i);
      end;
      if m.AnimCount > 0 then
      begin
        PrintList('');
        PrintList('Animations[' + IntToStr(m.AnimCount) + ']:');
        TabRow[0] := m.AnimCount;
        for i := 0 to m.AnimCount - 1 do
        begin
          TabRow[1] := 1;
          PrintList('Animation[' + m.Anims[i].Name + ']:', 1);
          Dec(TabRow[0]);
          PrintList('Frame Rate: ' + IntToStr(m.Anims[i].FrameRate), 2);
          PrintList('Frame Count: ' + IntToStr(m.Anims[i].FrameCount), 2);
          PrintList('Node Count: ' + IntToStr(m.Anims[i].NodeCount), 2);
        end;
      end;
      if m.MaterialCount > 0 then
      begin
        PrintList('');
        PrintList('Materials[' + IntToStr(m.MaterialCount) + ']:');
        TabRow[0] := m.MaterialCount;
        for i := 0 to m.MaterialCount - 1 do
        begin
          TabRow[1] := 1;
          PrintList('Material[' + m.Materials[i].Name + ']:', 1);
          Dec(TabRow[0]);
          if m.Materials[i].TwoSided then
          PrintList('Two Sided', 2);
          PrintList('Diffuse Map: ' + m.Materials[i].DiffuseMap, 2);
          PrintList('Specular Map: ' + m.Materials[i].SpecularMap, 2);
          PrintList('Normal Map: ' + m.Materials[i].NormalMap, 2);
        end;
      end;
    end;
  end;
end;

procedure TScene.RenderBorder;
const
  bs = 10;
  c1 = $00ffffff;
  c2 = $ffffffff;
begin
  with App do
  begin
    Render2D.DrawBegin(ptTriangleStrip);
    with Render2D do
    begin
      AddPos(G2Vec2(m_X, m_Y));
      AddPos(G2Vec2(m_X - bs, m_Y - bs));
      AddPos(G2Vec2(m_X + m_W, m_Y));
      AddPos(G2Vec2(m_X + m_W + bs, m_Y - bs));
      AddPos(G2Vec2(m_X + m_W, m_Y + m_H));
      AddPos(G2Vec2(m_X + m_W + bs, m_Y + m_H + bs));
      AddPos(G2Vec2(m_X, m_Y + m_H));
      AddPos(G2Vec2(m_X - bs, m_Y + m_H + bs));
      AddPos(G2Vec2(m_X, m_Y));
      AddPos(G2Vec2(m_X - bs, m_Y - bs));
      AddCol(c2); AddCol(c1);
      AddCol(c2); AddCol(c1);
      AddCol(c2); AddCol(c1);
      AddCol(c2); AddCol(c1);
      AddCol(c2); AddCol(c1);
    end;
    Render2D.DrawEnd;
    Render2D.DrawBegin(ptLineList);
    with Render2D do
    begin
      AddPos(G2Vec2(m_X - 1, m_Y - 1)); AddPos(G2Vec2(m_X + m_W, m_Y - 1));
      AddPos(G2Vec2(m_X + m_W, m_Y - 1)); AddPos(G2Vec2(m_X + m_W, m_Y + m_H));
      AddPos(G2Vec2(m_X + m_W, m_Y + m_H)); AddPos(G2Vec2(m_X - 1, m_Y + m_H));
      AddPos(G2Vec2(m_X - 1, m_Y + m_H)); AddPos(G2Vec2(m_X - 1, m_Y - 1));
      AddCol($ff000000, 8);
    end;
    Render2D.DrawEnd;
  end;
end;

procedure TScene.RenderScene;
  var m, mw: TG2Mat;
  var s: Single;
  var i: Integer;
begin
  with App do
  begin
    if Assigned(MeshInst) then
    begin
      Gfx.Lights[0].SetAmbientLight($202020);
      Gfx.Lights[0].Enabled := True;
      Gfx.Lights[1].SetDirectionalLight(1, -1, 1, $ffffffff);
      Gfx.Lights[1].Enabled := True;
      Gfx.Lights[2].SetDirectionalLight(-1, 1, -1, $ffffe3d3);
      Gfx.Lights[2].Enabled := True;
      Gfx.RenderStates.ZEnable := True;
      s := 4;
      m.SetScaling(s, s, s);
      Gfx.Transforms.W[0] := m;
      Gfx.Transforms.ApplyW(0);
      Render.TextureClear();
      Render3D.DrawBegin(ptLineList);
      for i := -20 to 20 do
      begin
        Render3D.AddPos(i, 0, -20);
        Render3D.AddPos(i, 0, 20);
        Render3D.AddPos(-20, 0, i);
        Render3D.AddPos(20, 0, i);
        Render3D.AddCol($ff000000, 4);
      end;
      Render3D.DrawEnd;
      s := m_BaseScale;
      m.SetTranslation(m_BasePos);
      m.Scale(s, s, s);
      Gfx.Transforms.W[0] := m;
      Gfx.Transforms.ApplyW(0);
      if m_WireFrame then
      Gfx.RenderStates.FillMode := D3DFILL_WIREFRAME;
      App.RenderModes.FilteringAnisotropic();
      MeshInst.Render;
      App.RenderModes.FilteringPoint();
      Gfx.RenderStates.FillMode := D3DFILL_SOLID;
      if m_ShowBBox then
      begin
        mw := m;
        m.SetIdentity;
        Gfx.Transforms.W[0] := m;
        Gfx.Transforms.ApplyW(0);
        for i := 0 to Mesh.GeomCount - 1 do
        RenderBBox((MeshInst.GeomBBox[i] * mw).AABox);
      end;
      Gfx.RenderStates.ZEnable := False;
    end;
  end;
end;

procedure TScene.RenderStructure;
  var i, lh, sh, sp: Integer;
begin
  lh := m_Font.GetTextHeight('A');
  for i := 0 to High(m_StructureArr) do
  m_Font.Print(m_X + 4, m_Y + 4 + i * lh - m_StructScroll, $ff000000, m_StructureArr[i]);
  sh := 8 + lh * Length(m_StructureArr);
  sp := Trunc((m_StructScroll / sh) * m_H);
  if sh > m_H then
  begin
    sh := Trunc(m_H * m_H / sh);
    App.Render.TextureClear();
    App.Prim2D.DrawRect(m_X + m_W - 24, m_Y + sp, 24, sh, $ffaaaaaa);
  end;
end;

procedure TScene.Render;
  var Dir: TG2Vec3;
  var sh, ch, sv, cv, s: Single;
begin
  RenderBorder;
  with App do
  begin
    Gfx.SetViewPort(
      Round(m_X),
      Round(m_Y),
      Round(m_W),
      Round(m_H)
    );
    G2SinCos(m_AngY, sh, ch);
    G2SinCos(m_AngX, sv, cv);
    s := 20 / m_Scale;
    Dir.x := ch * cv * s; Dir.y := sv * s; Dir.z := sh * cv * s;
    Cam.SetView(m_LookAt - Dir, m_LookAt, G2Vec3(0, 1, 0));
    Cam.SetPerspective(QuatPi, m_W / m_H, 1, 1000);
    Render.Clear(False, True, True, $ffcccccc);
    Gfx.Transforms.V := App.Cam.View;
    Gfx.Transforms.ApplyV;
    Gfx.Transforms.P := App.Cam.Proj;
    Gfx.Transforms.ApplyP;
    if m_Structure then
    RenderStructure
    else
    RenderScene;
    Gfx.SetViewPortDefault;
  end;
end;

procedure TScene.Update;
  var ShiftVec: TG2Vec3;
begin
  with App do
  begin
    if Assigned(App.MeshInst) then
    begin
      if m_MDInScreen[1] and m_MDInScreen[2]
      and m_PlugInput.MouseDown[1] and m_PlugInput.MouseDown[2] then
      begin
        ShiftVec := (m_InitLookAt - m_LookAt) * 0.1;
        m_LookAt := m_LookAt + ShiftVec;
      end;
      AdjustScroll;
      if App.Mesh.AnimCount > 0 then
      begin
        App.MeshInst.FrameSet(Mesh.Anims[0].Name, Mesh.Anims[0].FrameCount * G2TimeInterval(5000));
        //App.MeshInst.FrameSetFast(Mesh.Anims[0].Name, 20);
      end;
    end;
  end;
end;

procedure TScene.Adjust;
var
  w, h: Integer;
begin
  w := App.Gfx.Params.Width;
  h := App.Gfx.Params.Height;
  m_X := 64;
  m_Y := 128;
  m_W := w - m_X - 256;
  m_H := h - m_Y - 64;
end;
//TScene END

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  App.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.ClientWidth := 800;
  Form1.ClientHeight := 600;
  Form1.Constraints.MinHeight := 600;
  Form1.Constraints.MinWidth := 800;
  App := TMyApp.Create;
  App.Handle := Form1.Handle;
  App.Gfx.InitParams.Width := Form1.ClientWidth;
  App.Gfx.InitParams.Height := Form1.ClientHeight;
  App.Initialize;
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
    Scene.Adjust;
    Tmr.OnTimer;
  end;
end;

end.
