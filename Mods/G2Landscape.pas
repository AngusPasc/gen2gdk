//G2Landscape v1.0
unit G2Landscape;

{$include ../Gen2.inc}   

interface

uses
  Windows,
  Classes,
  Types,
  SysUtils,
  Math,
  Direct3D9,
  D3DX9,
  DXTypes,
  Gen2,
  G2Math,
  G2PerlinNoise;

type
  TG2HeightMap = class;
  TG2LandTexLayer = class;
  TG2Landscape = class;
  TG2GrassEngine = class;

  TG2HeightMap = class
  strict private
    m_Heights: array of array of Single;
    m_Width: Integer;
    m_Height: Integer;
    procedure SetWidth(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetHeight(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAltitude(const PosX, PosY: Single): Single;
    procedure SetAltitude(const PosX, PosY: Single; const Value: Single);
    function GetHeights(const PosX, PosY: Integer): Single;
    procedure SetHeights(const PosX, PosY: Integer; const Value: Single);
  public
    constructor Create;
    destructor Destroy; override;
    property Width: Integer read m_Width write SetWidth;
    property Height: Integer read m_Height write SetHeight;
    property Altitude[const X, Y: Single]: Single read GetAltitude write SetAltitude;
    property Heights[const X, Y: Integer]: Single read GetHeights write SetHeights;
    procedure SetSize(const NewWidth, NewHeight: Integer);
    procedure GenerateNormal(const NewWidth, NewHeight: Integer);
    procedure GenerateBumpy(const NewWidth, NewHeight: Integer);
    procedure LoadFromTexture(const Texture: TG2Texture2D);
    procedure Resample(const NewWidth, NewHeight: Integer);
  end;

  TG2LandTexLayer = class
  strict private
    m_DiffuseSpecular: TG2Texture2D;
    m_NormalsHeights: TG2Texture2D;
    m_Mask: TG2Texture2D;
    m_TexRepeatU: Single;
    m_TexRepeatV: Single;
    m_SpecularPower: Single;
    m_HeightMapScale: Single;
    m_MipThreshold: Integer;
    m_MinSamples: Integer;
    m_MaxSamples: Integer;
    m_Visible: Boolean;
    m_Updated: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property DiffuseSpecular: TG2Texture2D read m_DiffuseSpecular write m_DiffuseSpecular;
    property NormalsHeights: TG2Texture2D read m_NormalsHeights write m_NormalsHeights;
    property Mask: TG2Texture2D read m_Mask write m_Mask;
    property TexRepeatU: Single read m_TexRepeatU write m_TexRepeatU;
    property TexRepeatV: Single read m_TexRepeatV write m_TexRepeatV;
    property SpecularPower: Single read m_SpecularPower write m_SpecularPower;
    property HeightMapScale: Single read m_HeightMapScale write m_HeightMapScale;
    property MipThreshold: Integer read m_MipThreshold write m_MipThreshold;
    property MinSamples: Integer read m_MinSamples write m_MinSamples;
    property MaxSamples: Integer read m_MaxSamples write m_MaxSamples;
    property Visible: Boolean read m_Visible write m_Visible;
    property Updated: Boolean read m_Updated write m_Updated;
  end;

  TG2LandLight = class
  strict private
    m_LightDir: TG2Vec4;
    m_LightDiffuse: TG2Vec4;
    m_LightAmbient: TG2Vec4;
    function GetLightDir: TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetLightDir(const Value: TG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLightDiffuse: TG2Color;
    procedure SetLightDiffuse(const Value: TG2Color);
    function GetLightAmbient: TG2Color;
    procedure SetLightAmbient(const Value: TG2Color);
  private
    property LightDirPriv: TG2Vec4 read m_LightDir;
    property LightDiffusePriv: TG2Vec4 read m_LightDiffuse;
    property LightAmbientPriv: TG2Vec4 read m_LightAmbient;
  public
    constructor Create;
    destructor Destroy; override;
    property LightDir: TG2Vec3 read GetLightDir write SetLightDir;
    property LightDiffuse: TG2Color read GetLightDiffuse write SetLightDiffuse;
    property LightAmbient: TG2Color read GetLightAmbient write SetLightAmbient;
  end;

  TG2LandNodeType = (
    ntNoDiv,
    ntDivH,
    ntDivV,
    ntDivQ
  );

  TG2LandMode = (
    lmPlain,
    lmBump,
    lmParallax,
    lmParallaxOcclusion
  );

  TG2Landscape = class (TG2Module)
  strict private
  type
    PG2LandNode = ^TG2LandNode;
    TG2LandNode = record
      Parent: PG2LandNode;
      Children: array of PG2LandNode;
      Sibs: array[0..3] of PG2LandNode;
      LOD: Word;
      Vertices: array[0..8] of TPoint;
      MinBounds, MaxBounds: TG2Vec3;
      Center: TG2Vec3;
      NodeType: TG2LandNodeType;
      MagDist: Single;
      Magnify: Boolean;
      FrustumCheck: TG2FrustumCheck;
      LayerVis: array of Boolean;
    end;
    PVertex = ^TVertex;
    TVertex = packed record
      Pos0: TG2Vec3;
      Pos1: TG2Vec3;
    end;
    PVertexArray = ^TVertexArray;
    TVertexArray = array[0..0] of TVertex;
    TG2LandLayerBuffer = record
      BufferStartV: DWord;
      BufferEndV: DWord;
      BufferStartI: DWord;
      BufferEndI: DWord;
      TriCount: DWord;
    end;
  var
    m_PosX: Single;
    m_PosY: Single;
    m_PosZ: Single;
    m_SizeX: Single;
    m_SizeY: Single;
    m_SizeZ: Single;
    m_LOD: Word;
    m_LODPrg: Single;
    m_MagDist: Single;
    m_MagDistRelative: Boolean;
    m_HM: TG2HeightMap;
    m_Mode: TG2LandMode;
    m_Optimized: Boolean;
    m_Effect: TG2Effect;
    m_RootNode: PG2LandNode;
    m_Frustum: TG2Frustum;
    m_Built: Boolean;
    m_Normals: TG2Texture2D;
    m_Layers: array of TG2LandTexLayer;
    m_Light: TG2LandLight;
    m_Vertices: array of array of TVertex;
    m_VArr: array of TVertex;
    m_IArr: array of DWord;
    m_VISet: array of array of DWord;
    m_LayerBuffer: array of TG2LandLayerBuffer;
    m_VB: IDirect3DVertexBuffer9;
    m_IB: IDirect3DIndexBuffer9;
    m_Decl: IDirect3DVertexDeclaration9;
    m_VCount: DWord;
    m_PrevViewMat: TG2Mat;
    m_ViewPos: TG2Vec3;
    m_ForceUpdate: Boolean;
    m_ForceReBuild: Boolean;
    m_GrassEngine: TG2GrassEngine;
    m_TriCount: DWord;
    m_DrawCalls: DWord;
    m_PlugGraphics: TG2PlugGraphics;
    procedure SetPosX(const Value: Single);
    procedure SetPosY(const Value: Single);
    procedure SetPosZ(const Value: Single);
    procedure SetSizeX(const Value: Single);
    procedure SetSizeY(const Value: Single);
    procedure SetSizeZ(const Value: Single);
    procedure SetLOD(const Value: Word);
    procedure SetLODPrg(const Value: Single);
    function NeedUpdate: Boolean;
    function GetAltitude(const X, Z: Single): Single;
    procedure SetAltitude(const X, Z: Single; const Value: Single);
    procedure SetLayerCount(const Value: DWord);
    function GetLayerCount: DWord;
    function GetLayer(const Index: Integer): TG2LandTexLayer;
    function GetVCountX: Integer;
    function GetVCountZ: Integer;
    procedure InitTextures;
    procedure QuickReBuild;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  private
    property Frustum: TG2Frustum read m_Frustum write m_Frustum;
  public
    constructor Create; override;
    destructor Destroy; override;
    property HeightMap: TG2HeightMap read m_HM;
    property InitPosX: Single read m_PosX write SetPosX;
    property InitPosY: Single read m_PosY write SetPosY;
    property InitPosZ: Single read m_PosZ write SetPosZ;
    property InitSizeX: Single read m_SizeX write SetSizeX;
    property InitSizeY: Single read m_SizeY write SetSizeY;
    property InitSizeZ: Single read m_SizeZ write SetSizeZ;
    property InitLOD: Word read m_LOD write SetLOD;
    property InitLODPrg: Single read m_LODPrg write SetLODPrg;
    property InitMagDist: Single read m_MagDist write m_MagDist;
    property InitMagDistRelative: Boolean read m_MagDistRelative write m_MagDistRelative;
    property InitLayerCount: DWord read GetLayerCount write SetLayerCount;
    property InitOptimized: Boolean read m_Optimized write m_Optimized;
    property VCountX: Integer read GetVCountX;
    property VCountZ: Integer read GetVCountZ;
    property Mode: TG2LandMode read m_Mode write m_Mode;
    property Layers[const Index: Integer]: TG2LandTexLayer read GetLayer;
    property Light: TG2LandLight read m_Light;
    property Normals: TG2Texture2D read m_Normals;
    property Altitude[const X, Z: Single]: Single read GetAltitude write SetAltitude;
    property GrassEngine: TG2GrassEngine read m_GrassEngine;
    property TriangleCount: DWord read m_TriCount;
    property DrawCalls: DWord read m_DrawCalls;
    property ForceReBuild: Boolean read m_ForceRebuild write m_ForceReBuild;
    function NormalAtPoint(const X, Z: Single): TG2Vec3;
    function Pick(const Ray: TG2Ray; var Dist: Single; var HitPos: TG2Vec3): Boolean;
    procedure LandBuild;
    procedure LandDestroy;
    procedure Render;
    procedure Update;
    procedure ComputeNormals;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;

  PG2GrassElement = ^TG2GrassElement;
  TG2GrassElement = record
  public
    Texture: TG2Texture2D;
    Segments: Integer;
    Size: Single;
    Height: Single;
    VBIndex: DWord;
    VBCount: DWord;
    VPI: Word;
    IBIndex: DWord;
    IBCount: DWord;
    IPI: Word;
    Radius: Single;
    Swing: Single;
  end;

  PG2GrassItem = ^TG2GrassItem;
  TG2GrassItem = record
  public
    Element: PG2GrassElement;
    SceneItem: PG2Scene3DItem;
    Transform: TG2Mat;
    Center: TG2Vec3;
    Radius: Single;
    SwingDir: TG2Vec2;
    SwingRnd: DWord;
    Alpha: Single;
    Dist: Single;
  end;

  TG2GrassMode = (
    gmNormal,
    gmBlended,
    gmSampled
  );

  TG2GrassEngine = class
  strict private
  type
    TVertex = packed record
      Pos0: TG2Vec4;
      Tex0: TG2Vec3;
    end;
    PVertexArray = ^TVertexArray;
    TVertexArray = array[0..0] of TVertex;
  var
    m_Landscape: TG2Landscape;
    m_Gfx: TG2Graphics;
    m_Scene: TG2Scene3D;
    m_Elements: TList;
    m_Items: TList;
    m_SortedItems: TList;
    m_VS: TG2VertexShader;
    m_VB: IDirect3DVertexBuffer9;
    m_IB: IDirect3DIndexBuffer9;
    m_Decl: IDirect3DVertexDeclaration9;
    m_Mode: TG2GrassMode;
    m_AlphaRef: DWord;
    m_VisDist: Single;
    m_FadeDist: Single;
    m_ItemsPerDraw: DWord;
    m_PrevViewMat: TG2Mat;
    m_ForceUpdate: Boolean;
    m_TrMS: Boolean;
    m_TrSS: Boolean;
    m_Built: Boolean;
    m_TriCount: DWord;
    m_DrawCalls: DWord;
    procedure SetMode(const Value: TG2GrassMode);
    function NeedUpdate: Boolean;
  public
    constructor Create(const Landscape: TG2Landscape);
    destructor Destroy; override;
    property Mode: TG2GrassMode read m_Mode write SetMode;
    property AlphaRef: DWord read m_AlphaRef write m_AlphaRef;
    property VisDist: Single read m_VisDist write m_VisDist;
    property FadeDist: Single read m_FadeDist write m_FadeDist;
    property TriangleCount: DWord read m_TriCount;
    property DrawCalls: DWord read m_DrawCalls;
    function AddGrassElement(
      const Texture: TG2Texture2D;
      const Segments: Integer;
      const Size, Height, Swing: Single
    ): PG2GrassElement;
    procedure AddGrassItem(
      const X, Z: Single;
      const Scale: Single;
      const Element: PG2GrassElement
    );
    procedure GrassBuild;
    procedure GrassDestroy;
    procedure Render;
    procedure Update;
    procedure RenderScene;
    procedure FreeElements;
    procedure FreeItems;
  end;

implementation

//TG2HeightMap BEGIN
constructor TG2HeightMap.Create;
begin
  inherited Create;
  m_Width := 0;
  m_Height := 0;
end;

destructor TG2HeightMap.Destroy;
begin
  inherited Destroy;
end;

procedure TG2HeightMap.SetWidth(const Value: Integer);
begin
  m_Width := Value;
  SetLength(m_Heights, m_Width, m_Height);
end;

procedure TG2HeightMap.SetHeight(const Value: Integer);
begin
  m_Height := Value;
  SetLength(m_Heights, m_Width, m_Height);
end;

function TG2HeightMap.GetAltitude(const PosX, PosY: Single): Single;
var
  IntX1, IntY1, IntX2, IntY2: Integer;
  FracX, FracY: Single;
  g1, g2: Single;
  X, Y: Single;
begin
  X := Min(Max(PosX * (m_Width - 1), 0), m_Width - 1);
  Y := Min(Max(PosY * (m_Height - 1), 0), m_Height - 1);
  IntX1 := Trunc(X);
  FracX := Frac(X);
  IntY1 := Trunc(Y);
  FracY := Frac(Y);
  IntX2 := Min(IntX1 + 1, m_Width - 1);
  IntY2 := Min(IntY1 + 1, m_Height - 1);
  g1 := G2LerpFloat(m_Heights[IntX1, IntY1], m_Heights[IntX2, IntY1], FracX);
  g2 := G2LerpFloat(m_Heights[IntX1, IntY2], m_Heights[IntX2, IntY2], FracX);
  Result := G2LerpFloat(g1, g2, FracY);
end;

procedure TG2HeightMap.SetAltitude(const PosX, PosY: Single; const Value: Single);
var
  IntX1, IntY1, IntX2, IntY2: Integer;
  FracX, FracY: Single;
  gx1, gx2, gy1, gy2: Single;
  X, Y: Single;
begin
  X := Min(Max(PosX * (m_Width - 1), 0), m_Width - 1);
  Y := Min(Max(PosY * (m_Height - 1), 0), m_Height - 1);
  IntX1 := Trunc(X);
  FracX := Frac(X);
  IntY1 := Trunc(Y);
  FracY := Frac(Y);
  IntX2 := Min(IntX1 + 1, m_Width - 1);
  IntY2 := Min(IntY1 + 1, m_Height - 1);
  gy1 := 1 - FracY; gy2 := FracY;
  gx1 := 1 - FracX; gx2 := FracX;
  m_Heights[IntX1, IntY1] := G2LerpFloat(m_Heights[IntX1, IntY1], Value, gx1 * gy1);
  m_Heights[IntX2, IntY1] := G2LerpFloat(m_Heights[IntX2, IntY1], Value, gx2 * gy1);
  m_Heights[IntX1, IntY2] := G2LerpFloat(m_Heights[IntX1, IntY2], Value, gx1 * gy2);
  m_Heights[IntX2, IntY2] := G2LerpFloat(m_Heights[IntX2, IntY2], Value, gx2 * gy2);
end;

function TG2HeightMap.GetHeights(const PosX, PosY: Integer): Single;
var
  X, Y: Integer;
begin
  X := Min(Max(PosX, 0), m_Width - 1);
  Y := Min(Max(PosY, 0), m_Height - 1);
  Result := m_Heights[X, Y];
end;

procedure TG2HeightMap.SetHeights(const PosX, PosY: Integer; const Value: Single);
var
  X, Y: Integer;
begin
  X := Min(Max(PosX, 0), m_Width - 1);
  Y := Min(Max(PosY, 0), m_Height - 1);
  m_Heights[X, Y] := Value;
end;

procedure TG2HeightMap.SetSize(const NewWidth, NewHeight: Integer);
begin
  m_Width := NewWidth;
  m_Height := NewHeight;
  SetLength(m_Heights, m_Width, m_Height);
end;

procedure TG2HeightMap.GenerateNormal(const NewWidth, NewHeight: Integer);
var
  PerlinNoise: TG2PerlinNoise;
  r, i, j: Integer;
begin
  m_Width := NewWidth;
  m_Height := NewHeight;
  SetLength(m_Heights, m_Width, m_Height);
  PerlinNoise := TG2PerlinNoise.Create;
  PerlinNoise.PatternWidth := m_Width;
  PerlinNoise.PatternHeight := m_Height;
  PerlinNoise.Seamless := False;
  r := Random(101);
  for j := 0 to m_Height - 1 do
  for i := 0 to m_Width - 1 do
  begin
    m_Heights[i, j] := PerlinNoise.PerlinNoise2D(
      i, j, 4, 0.5, 0.25 / 4, 1, r
    ) * 0.5 + 0.5;
  end;
  PerlinNoise.Free;
end;

procedure TG2HeightMap.GenerateBumpy(const NewWidth, NewHeight: Integer);
var
  PerlinNoise: TG2PerlinNoise;
  r, i, j: Integer;
begin
  m_Width := NewWidth;
  m_Height := NewHeight;
  SetLength(m_Heights, m_Width, m_Height);
  PerlinNoise := TG2PerlinNoise.Create;
  PerlinNoise.PatternWidth := m_Width;
  PerlinNoise.PatternHeight := m_Height;
  PerlinNoise.Seamless := False;
  r := Random(101);
  for j := 0 to m_Height - 1 do
  for i := 0 to m_Width - 1 do
  begin
    m_Heights[i, j] := 1 - Abs(
      PerlinNoise.PerlinNoise2D(
        i, j, 4, 0.5, 0.25 / 4, 1, r
      )
    );
  end;
  PerlinNoise.Free;
end;

procedure TG2HeightMap.LoadFromTexture(const Texture: TG2Texture2D);
var
  i, j: Integer;
begin
  SetSize(Texture.Width, Texture.Height);
  Texture.Surfaces[0].Lock;
  for j := 0 to m_Height - 1 do
  for i := 0 to m_Width - 1 do
  m_Heights[i, j] := Texture.Surfaces[0].Pixels[i, j].r / 255;
  Texture.Surfaces[0].UnLock;
end;

procedure TG2HeightMap.Resample(const NewWidth, NewHeight: Integer);
var
  i, j: Integer;
  x, y, gx, gy, h1, h2: Single;
  smx1, smx2, smy1, smy2: Integer;
  NewHeights: array of array of Single;
begin
  SetLength(NewHeights, NewWidth, NewHeight);
  for i := 0 to NewWidth - 1 do
  for j := 0 to NewHeight - 1 do
  begin
    x := i / (NewWidth - 1) * (m_Width - 1);
    y := j / (NewHeight - 1) * (m_Height - 1);
    smx1 := Trunc(x);
    smx2 := Min(smx1 + 1, m_Width - 1);
    gx := x - smx1;
    smy1 := Trunc(y);
    smy2 := Min(smy1 + 1, m_Height - 1);
    gy := y - smy1;
    h1 := G2LerpFloat(m_Heights[smx1, smy1], m_Heights[smx2, smy1], gx);
    h2 := G2LerpFloat(m_Heights[smx1, smy2], m_Heights[smx2, smy2], gx);
    NewHeights[i, j] := G2LerpFloat(h1, h2, gy);
  end;
  m_Width := NewWidth;
  m_Height := NewHeight;
  SetLength(m_Heights, NewWidth, NewHeight);
  for i := 0 to High(m_Heights) do
  Move(NewHeights[i][0], m_Heights[i][0], SizeOf(Single) * Length(m_Heights[i]));
end;
//TG2HeightMap END

//TG2LandTexLayer BEGIN
constructor TG2LandTexLayer.Create;
begin
  inherited Create;
  m_TexRepeatU := 1;
  m_TexRepeatV := 1;
  m_SpecularPower := 10;
  m_HeightMapScale := 0.03;
  m_MipThreshold := 4;
  m_MinSamples := 16;
  m_MaxSamples := 32;
  m_Visible := True;
  m_Updated := True;
end;

destructor TG2LandTexLayer.Destroy;
begin
  inherited Destroy;
end;
//TG2LandTexLayer END

//TG2LandLight BEGIN
constructor TG2LandLight.Create;
begin
  inherited Create;
  m_LightDir := G2Vec4(0, 1, 0, 0);
  m_LightDiffuse := G2Vec4(1, 1, 1, 1);
  m_LightAmbient := G2Vec4(0.01, 0.01, 0.01, 0.01);
end;

destructor TG2LandLight.Destroy;
begin
  inherited Destroy;
end;

function TG2LandLight.GetLightDir: TG2Vec3;
begin
  Result := -PG2Vec3(@m_LightDir)^;
end;

procedure TG2LandLight.SetLightDir(const Value: TG2Vec3);
var
  v: TG2Vec3;
begin
  v := Value;
  v.Normalize;
  m_LightDir := -G2Vec4(v, 0);
end;

function TG2LandLight.GetLightDiffuse: TG2Color;
begin
  Result := G2Color(m_LightDiffuse);
end;

procedure TG2LandLight.SetLightDiffuse(const Value: TG2Color);
const
  Rsp255 = 1 / 255;
begin
  m_LightDiffuse := G2Vec4(
    Value.r * Rsp255,
    Value.g * Rsp255,
    Value.b * Rsp255,
    Value.a * Rsp255
  );
end;

function TG2LandLight.GetLightAmbient: TG2Color;
begin
  Result := G2Color(m_LightAmbient);
end;

procedure TG2LandLight.SetLightAmbient(const Value: TG2Color);
const
  Rsp255 = 1 / 255;
begin
  m_LightAmbient := G2Vec4(
    Value.r * Rsp255,
    Value.g * Rsp255,
    Value.b * Rsp255,
    Value.a * Rsp255
  );
end;
//TG2LandLight END

//TG2Landscape BEGIN
constructor TG2Landscape.Create;
begin
  inherited Create;
end;

destructor TG2Landscape.Destroy;
begin
  inherited Destroy;
end;

procedure TG2Landscape.SetPosX(const Value: Single);
begin
  if not m_Built then
  m_PosX := Value;
end;

procedure TG2Landscape.SetPosY(const Value: Single);
begin
  if not m_Built then
  m_PosY := Value;
end;

procedure TG2Landscape.SetPosZ(const Value: Single);  
begin
  if not m_Built then
  m_PosZ := Value;
end;

procedure TG2Landscape.SetSizeX(const Value: Single);   
begin
  if not m_Built then
  m_SizeX := Value;
end;

procedure TG2Landscape.SetSizeY(const Value: Single); 
begin
  if not m_Built then
  m_SizeY := Value;
end;

procedure TG2Landscape.SetSizeZ(const Value: Single);  
begin
  if not m_Built then
  m_SizeZ := Value;
end;

procedure TG2Landscape.SetLOD(const Value: Word);       
begin
  if not m_Built then
  m_LOD := Value;
end;

procedure TG2Landscape.SetLODPrg(const Value: Single);    
begin
  if not m_Built then
  m_LODPrg := Min(Value, 0.6875);
end;

function TG2Landscape.NeedUpdate: Boolean;
const
  Threshold = 1E-3;
var
  i, j: Integer;
  V: TG2Mat;
begin
  if m_ForceUpdate then
  begin
    Result := True;
    m_ForceUpdate := False;
    Exit;
  end;
  Result := False;
  V := Core.Graphics.Transforms.V;
  for j := 0 to 3 do
  for i := 0 to 3 do
  if Abs(PG2MatRef(@V)^.m[i, j] - PG2MatRef(@m_PrevViewMat)^.m[i, j]) > Threshold then
  begin
    Result := True;
    Exit;
  end;
end;

function TG2Landscape.GetAltitude(const X, Z: Single): Single;
begin
  Result := m_HM.Altitude[X / m_SizeX, Z / m_SizeZ] * m_SizeY;
end;

procedure TG2Landscape.SetAltitude(const X, Z: Single; const Value: Single);
begin
  m_HM.Altitude[X / m_SizeX, Z / m_SizeZ] := Min(Value / m_SizeY, 1);
  m_ForceReBuild := True;
end;

procedure TG2Landscape.SetLayerCount(const Value: DWord);
var
  i: Integer;
  PrevCount: DWord;
begin
  if m_Built then Exit;
  PrevCount := Length(m_Layers);
  if Value = PrevCount then Exit;
  for i := Value to High(m_Layers) do
  m_Layers[i].Free;
  SetLength(m_Layers, Value);
  for i := PrevCount to High(m_Layers) do
  m_Layers[i] := TG2LandTexLayer.Create;
end;

function TG2Landscape.GetLayerCount: DWord;
begin
  Result := Length(m_Layers);
end;

function TG2Landscape.GetLayer(const Index: Integer): TG2LandTexLayer;
begin
  if (Index < 0) or (Index > High(m_Layers)) then
  begin
    Result := nil;
    Exit;
  end;
  Result := m_Layers[Index];
end;

function TG2Landscape.GetVCountX: Integer;
begin
  Result := Length(m_Vertices);
end;

function TG2Landscape.GetVCountZ: Integer;
begin
  Result := Length(m_Vertices[0]);
end;

procedure TG2Landscape.InitTextures;
var
  w, h: Integer;
begin
  w := 1; h := 1;
  while w < m_HM.Width do w := w shl 1;
  while h < m_HM.Height do h := h shl 1;
  m_Normals.MakeTexture(
    w, h, 0, 0, D3DFMT_A8R8G8B8
  );
  ComputeNormals;
end;

procedure TG2Landscape.QuickReBuild;
  procedure SetNodeBounds(const Node: PG2LandNode);
  var
    c, i: Integer;
  begin
    Move(m_Vertices[Node^.Vertices[0].X, Node^.Vertices[0].Y].Pos0, Node^.MinBounds, SizeOf(TG2Vec3));
    Move(m_Vertices[Node^.Vertices[0].X, Node^.Vertices[0].Y].Pos0, Node^.MaxBounds, SizeOf(TG2Vec3));
    for i := 1 to High(Node^.Vertices) do
    begin
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.x > Node^.MaxBounds.x then
      Node^.MaxBounds.x := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.x;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.y > Node^.MaxBounds.y then
      Node^.MaxBounds.y := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.y;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.z > Node^.MaxBounds.z then
      Node^.MaxBounds.z := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.z;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.x < Node^.MinBounds.x then
      Node^.MinBounds.x := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.x;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.y < Node^.MinBounds.y then
      Node^.MinBounds.y := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.y;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.z < Node^.MinBounds.z then
      Node^.MinBounds.z := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.z;
    end;
    if Node^.NodeType <> ntNoDiv then
    begin
      for c := 0 to High(Node^.Children) do
      begin
        SetNodeBounds(Node^.Children[c]);
        if Node^.Children[c]^.MinBounds.x < Node^.MinBounds.x then
        Node^.MinBounds.x := Node^.Children[c]^.MinBounds.x;
        if Node^.Children[c]^.MinBounds.y < Node^.MinBounds.y then
        Node^.MinBounds.y := Node^.Children[c]^.MinBounds.y;
        if Node^.Children[c]^.MinBounds.z < Node^.MinBounds.z then
        Node^.MinBounds.z := Node^.Children[c]^.MinBounds.z;
        if Node^.Children[c]^.MaxBounds.x > Node^.MaxBounds.x then
        Node^.MaxBounds.x := Node^.Children[c]^.MaxBounds.x;
        if Node^.Children[c]^.MaxBounds.y > Node^.MaxBounds.y then
        Node^.MaxBounds.y := Node^.Children[c]^.MaxBounds.y;
        if Node^.Children[c]^.MaxBounds.z > Node^.MaxBounds.z then
        Node^.MaxBounds.z := Node^.Children[c]^.MaxBounds.z;
      end;
    end;
    Node^.Center.x := (Node^.MinBounds.x + Node^.MaxBounds.x) * 0.5;
    Node^.Center.y := (Node^.MinBounds.y + Node^.MaxBounds.y) * 0.5;
    Node^.Center.z := (Node^.MinBounds.z + Node^.MaxBounds.z) * 0.5;
  end;
var
  i, j: Integer;
  X, Z: Single;
begin
  for j := 0 to High(m_Vertices[0]) do
  for i := 0 to High(m_Vertices) do
  begin
    X := i / High(m_Vertices);
    Z := j / High(m_Vertices[0]);
    m_Vertices[i, j].Pos0 := G2Vec3(
      X * m_SizeX,
      m_HM.Altitude[X, Z] * m_SizeY,
      Z * m_SizeZ
    );
  end;
  SetNodeBounds(m_RootNode);
  m_ForceRebuild := False;
  m_ForceUpdate := True;
end;

procedure TG2Landscape.OnDeviceLost;
begin
  SafeRelease(m_VB);
  SafeRelease(m_IB);
end;

procedure TG2Landscape.OnDeviceReset;
begin
  Core.Graphics.Device.CreateVertexBuffer(
    SizeOf(TVertex) * Length(m_Vertices) * Length(m_Vertices[0]) * Length(m_Layers),
    D3DUSAGE_WRITEONLY,
    0,
    D3DPOOL_DEFAULT,
    m_VB,
    nil
  );
  Core.Graphics.Device.CreateIndexBuffer(
    SizeOf(DWord) * (Length(m_Vertices) - 1) * (Length(m_Vertices[0]) - 1) * 6 * Length(m_Layers),
    D3DUSAGE_WRITEONLY,
    D3DFMT_INDEX32,
    D3DPOOL_DEFAULT,
    m_IB,
    nil
  );
  m_ForceUpdate := True;
end;

function TG2Landscape.NormalAtPoint(const X, Z: Single): TG2Vec3;
var
  v0, v1, v2: TG2Vec3;
  n: TG2Vec3;
  rx, rz: Single;
begin
  rx := (m_SizeX / High(m_Vertices)) * 0.5;
  rz := (m_SizeZ / High(m_Vertices[0])) * 0.5;
  v0 := G2Vec3(X, GetAltitude(X, Z), Z);
  v1 := G2Vec3(v0.x, 0, v0.z + rz);
  v2 := G2Vec3(v0.x + rx, 0, v0.z);
  v1.y := GetAltitude(v1.x, v1.z);
  v2.y := GetAltitude(v2.x, v2.z);
  Result := G2TriangleNormal(v0, v1, v2);

  v1 := G2Vec3(v0.x + rx, 0, v0.z);
  v2 := G2Vec3(v0.x, 0, v0.z - rz);
  v1.y := GetAltitude(v1.x, v1.z);
  v2.y := GetAltitude(v2.x, v2.z);
  n := G2TriangleNormal(v0, v1, v2);
  Result := Result + n;

  v1 := G2Vec3(v0.x, 0, v0.z - rz);
  v2 := G2Vec3(v0.x - rx, 0, v0.z);
  v1.y := GetAltitude(v1.x, v1.z);
  v2.y := GetAltitude(v2.x, v2.z);
  n := G2TriangleNormal(v0, v1, v2);
  Result := Result + n;

  v1 := G2Vec3(v0.x - rx, 0, v0.z);
  v2 := G2Vec3(v0.x, 0, v0.z + rz);
  v1.y := GetAltitude(v1.x, v1.z);
  v2.y := GetAltitude(v2.x, v2.z);
  n := G2TriangleNormal(v0, v1, v2);
  Result := Result + n; 

  Result.Normalize;
end;

function TG2Landscape.Pick(const Ray: TG2Ray; var Dist: Single; var HitPos: TG2Vec3): Boolean;
var
  MinDist: Single;
  procedure PickNode(const n: PG2LandNode);
  var
    i: Integer;
    U, V, d: Single;
  const
    LoopPts: array[0..8] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 1);
  begin
    if D3DXBoxBoundProbe(
      n^.MinBounds,
      n^.MaxBounds,
      Ray.Origin,
      Ray.Dir
    ) then
    begin
      if n^.Magnify then
      for i := 0 to High(n^.Children) do
      PickNode(n^.Children[i])
      else
      begin
        for i := 0 to High(LoopPts) - 1 do
        if Ray.IntersectTri(
          m_Vertices[n^.Vertices[0].x, n^.Vertices[0].y].Pos0,
          m_Vertices[n^.Vertices[LoopPts[i]].x, n^.Vertices[LoopPts[i]].y].Pos0,
          m_Vertices[n^.Vertices[LoopPts[i + 1]].x, n^.Vertices[LoopPts[i + 1]].y].Pos0,
          U, V, d
        ) then
        begin
          if (d < MinDist) or (MinDist < 0) then
          begin
            MinDist := d;
            Result := True;
          end;
        end;
      end;
    end;
  end;
begin
  MinDist := -1;
  Result := False;
  PickNode(m_RootNode);
  if Result then
  begin
    Dist := MinDist;
    HitPos := Ray.Origin + (Ray.Dir * MinDist);
  end;
end;

procedure TG2Landscape.LandBuild;
  //Node Vertices:
  //  1-2-3
  //  |\|/|
  //  8-0-4
  //  |/|\|
  //  7-6-5
  procedure BuildNode(const Node: PG2LandNode);
    procedure SetUpChildNodes;
    var
      i: Integer;
    begin
      for i := 0 to High(Node^.Children) do
      begin
        Node^.Children[i] := New(PG2LandNode);
        Node^.Children[i]^.LOD := Node^.LOD + 1;
        Node^.Children[i]^.Parent := Node;
        Node^.Children[i]^.NodeType := ntNoDiv;
        Node^.Children[i]^.MagDist := Node^.MagDist * m_LODPrg;
        Node^.Children[i]^.Magnify := False;
      end;
    end;
    procedure SetChildVertices;
    var
      i: Integer;
      MidX, MidZ: Integer;
    begin
      for i := 0 to High(Node^.Children) do
      begin
        MidX := Node^.Children[i]^.Vertices[1].X + Abs(Node^.Children[i]^.Vertices[3].X - Node^.Children[i]^.Vertices[1].X) div 2;
        MidZ := Node^.Children[i]^.Vertices[1].Y + Abs(Node^.Children[i]^.Vertices[7].Y - Node^.Children[i]^.Vertices[1].Y) div 2;
        Node^.Children[i]^.Vertices[0] := Point(MidX, MidZ);
        Node^.Children[i]^.Vertices[2] := Point(MidX, Node^.Children[i]^.Vertices[1].Y);
        Node^.Children[i]^.Vertices[4] := Point(Node^.Children[i]^.Vertices[3].X, MidZ);
        Node^.Children[i]^.Vertices[6] := Point(MidX, Node^.Children[i]^.Vertices[7].Y);
        Node^.Children[i]^.Vertices[8] := Point(Node^.Children[i]^.Vertices[1].X, MidZ);
      end;
    end;
  var
    Asp: Single;
    i: Integer;
  begin
    Asp := Abs(
      (m_Vertices[Node^.Vertices[4].X, Node^.Vertices[4].Y].Pos0.x) -
      (m_Vertices[Node^.Vertices[7].X, Node^.Vertices[7].Y].Pos0.x)
    ) / Abs(
      (m_Vertices[Node^.Vertices[6].X, Node^.Vertices[6].Y].Pos0.z) -
      (m_Vertices[Node^.Vertices[2].X, Node^.Vertices[2].Y].Pos0.z)
    );
    if Asp >= 2 then
    begin
      Node^.NodeType := ntDivH;
      SetLength(Node^.Children, 2);
      SetUpChildNodes;
      Move(Node^.Vertices[1], Node^.Children[0].Vertices[1], SizeOf(TPoint));
      Move(Node^.Vertices[2], Node^.Children[0].Vertices[3], SizeOf(TPoint));
      Move(Node^.Vertices[6], Node^.Children[0].Vertices[5], SizeOf(TPoint));
      Move(Node^.Vertices[7], Node^.Children[0].Vertices[7], SizeOf(TPoint));

      Move(Node^.Vertices[2], Node^.Children[1].Vertices[1], SizeOf(TPoint));
      Move(Node^.Vertices[3], Node^.Children[1].Vertices[3], SizeOf(TPoint));
      Move(Node^.Vertices[5], Node^.Children[1].Vertices[5], SizeOf(TPoint));
      Move(Node^.Vertices[6], Node^.Children[1].Vertices[7], SizeOf(TPoint));
    end
    else if Asp <= 0.5 then
    begin
      Node^.NodeType := ntDivV;
      SetLength(Node^.Children, 2);
      SetUpChildNodes;
      Move(Node^.Vertices[1], Node^.Children[0].Vertices[1], SizeOf(TPoint));
      Move(Node^.Vertices[3], Node^.Children[0].Vertices[3], SizeOf(TPoint));
      Move(Node^.Vertices[4], Node^.Children[0].Vertices[5], SizeOf(TPoint));
      Move(Node^.Vertices[8], Node^.Children[0].Vertices[7], SizeOf(TPoint));

      Move(Node^.Vertices[8], Node^.Children[1].Vertices[1], SizeOf(TPoint));
      Move(Node^.Vertices[4], Node^.Children[1].Vertices[3], SizeOf(TPoint));
      Move(Node^.Vertices[5], Node^.Children[1].Vertices[5], SizeOf(TPoint));
      Move(Node^.Vertices[7], Node^.Children[1].Vertices[7], SizeOf(TPoint));
    end
    else
    begin
      Node^.NodeType := ntDivQ;
      SetLength(Node^.Children, 4);
      SetUpChildNodes;
      Move(Node^.Vertices[1], Node^.Children[0].Vertices[1], SizeOf(TPoint));
      Move(Node^.Vertices[2], Node^.Children[0].Vertices[3], SizeOf(TPoint));
      Move(Node^.Vertices[0], Node^.Children[0].Vertices[5], SizeOf(TPoint));
      Move(Node^.Vertices[8], Node^.Children[0].Vertices[7], SizeOf(TPoint));

      Move(Node^.Vertices[2], Node^.Children[1].Vertices[1], SizeOf(TPoint));
      Move(Node^.Vertices[3], Node^.Children[1].Vertices[3], SizeOf(TPoint));
      Move(Node^.Vertices[4], Node^.Children[1].Vertices[5], SizeOf(TPoint));
      Move(Node^.Vertices[0], Node^.Children[1].Vertices[7], SizeOf(TPoint));

      Move(Node^.Vertices[0], Node^.Children[2].Vertices[1], SizeOf(TPoint));
      Move(Node^.Vertices[4], Node^.Children[2].Vertices[3], SizeOf(TPoint));
      Move(Node^.Vertices[5], Node^.Children[2].Vertices[5], SizeOf(TPoint));
      Move(Node^.Vertices[6], Node^.Children[2].Vertices[7], SizeOf(TPoint));

      Move(Node^.Vertices[8], Node^.Children[3].Vertices[1], SizeOf(TPoint));
      Move(Node^.Vertices[0], Node^.Children[3].Vertices[3], SizeOf(TPoint));
      Move(Node^.Vertices[6], Node^.Children[3].Vertices[5], SizeOf(TPoint));
      Move(Node^.Vertices[7], Node^.Children[3].Vertices[7], SizeOf(TPoint));
    end;
    SetChildVertices;
    if Node^.LOD + 1 < m_LOD then
    for i := 0 to High(Node^.Children) do
    BuildNode(Node^.Children[i]);
  end;
  procedure SetNodeBounds(const Node: PG2LandNode);
  var
    c, i: Integer;
  begin
    SetLength(Node^.LayerVis, Length(m_Layers));
    for i := 0 to High(Node^.LayerVis) do
    Node^.LayerVis[i] := True;
    Node^.MagDist := Sqr(Node^.MagDist);
    Move(m_Vertices[Node^.Vertices[0].X, Node^.Vertices[0].Y].Pos0, Node^.MinBounds, SizeOf(TD3DXVector3));
    Move(m_Vertices[Node^.Vertices[0].X, Node^.Vertices[0].Y].Pos0, Node^.MaxBounds, SizeOf(TD3DXVector3));
    for i := 1 to High(Node^.Vertices) do
    begin
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.x > Node^.MaxBounds.x then
      Node^.MaxBounds.x := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.x;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.y > Node^.MaxBounds.y then
      Node^.MaxBounds.y := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.y;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.z > Node^.MaxBounds.z then
      Node^.MaxBounds.z := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.z;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.x < Node^.MinBounds.x then
      Node^.MinBounds.x := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.x;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.y < Node^.MinBounds.y then
      Node^.MinBounds.y := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.y;
      if m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.z < Node^.MinBounds.z then
      Node^.MinBounds.z := m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y].Pos0.z;
    end;
    if Node^.NodeType <> ntNoDiv then
    begin
      for c := 0 to High(Node^.Children) do
      begin
        SetNodeBounds(Node^.Children[c]);
        if Node^.Children[c]^.MinBounds.x < Node^.MinBounds.x then
        Node^.MinBounds.x := Node^.Children[c]^.MinBounds.x;
        if Node^.Children[c]^.MinBounds.y < Node^.MinBounds.y then
        Node^.MinBounds.y := Node^.Children[c]^.MinBounds.y;
        if Node^.Children[c]^.MinBounds.z < Node^.MinBounds.z then
        Node^.MinBounds.z := Node^.Children[c]^.MinBounds.z;
        if Node^.Children[c]^.MaxBounds.x > Node^.MaxBounds.x then
        Node^.MaxBounds.x := Node^.Children[c]^.MaxBounds.x;
        if Node^.Children[c]^.MaxBounds.y > Node^.MaxBounds.y then
        Node^.MaxBounds.y := Node^.Children[c]^.MaxBounds.y;
        if Node^.Children[c]^.MaxBounds.z > Node^.MaxBounds.z then
        Node^.MaxBounds.z := Node^.Children[c]^.MaxBounds.z;
      end;
    end;
    Node^.Center.x := (Node^.MinBounds.x + Node^.MaxBounds.x) * 0.5;
    Node^.Center.y := (Node^.MinBounds.y + Node^.MaxBounds.y) * 0.5;
    Node^.Center.z := (Node^.MinBounds.z + Node^.MaxBounds.z) * 0.5;
  end;
  procedure SetNodeSibs(Node: PG2LandNode);
    function GetNodeChild(const SibNode: PG2LandNode; const ChildIndex: Integer): PG2LandNode;
    begin
      Result := nil;
      if SibNode = nil then Exit;
      case SibNode^.NodeType of
        ntDivQ: Result := SibNode^.Children[ChildIndex];
        ntDivH:
        case ChildIndex of
          0, 2: Result := SibNode^.Children[0];
          1, 3: Result := SibNode^.Children[1];
        end;
        ntDivV:
        case ChildIndex of
          0, 1: Result := SibNode^.Children[0];
          2, 3: Result := SibNode^.Children[1];
        end;
      end;
    end;
  var
    i: Integer;
  begin
    case Node^.NodeType of
      ntDivH:
      begin
        Node^.Children[0].Sibs[0] := GetNodeChild(Node^.Sibs[0], 1);
        Node^.Children[0].Sibs[1] := GetNodeChild(Node^.Sibs[1], 3);
        Node^.Children[0].Sibs[2] := Node^.Children[1];
        Node^.Children[0].Sibs[3] := GetNodeChild(Node^.Sibs[3], 0);

        Node^.Children[1].Sibs[0] := Node^.Children[0];
        Node^.Children[1].Sibs[1] := GetNodeChild(Node^.Sibs[1], 2);
        Node^.Children[1].Sibs[2] := GetNodeChild(Node^.Sibs[2], 0);
        Node^.Children[1].Sibs[3] := GetNodeChild(Node^.Sibs[3], 1);
      end;
      ntDivV:
      begin
        Node^.Children[0].Sibs[0] := GetNodeChild(Node^.Sibs[0], 1);
        Node^.Children[0].Sibs[1] := GetNodeChild(Node^.Sibs[1], 3);
        Node^.Children[0].Sibs[2] := GetNodeChild(Node^.Sibs[2], 0);
        Node^.Children[0].Sibs[3] := Node^.Children[1];

        Node^.Children[1].Sibs[0] := GetNodeChild(Node^.Sibs[0], 2);
        Node^.Children[1].Sibs[1] := Node^.Children[0];
        Node^.Children[1].Sibs[2] := GetNodeChild(Node^.Sibs[2], 3);
        Node^.Children[1].Sibs[3] := GetNodeChild(Node^.Sibs[3], 0);
      end;
      ntDivQ:
      begin
        Node^.Children[0].Sibs[0] := GetNodeChild(Node^.Sibs[0], 1);
        Node^.Children[0].Sibs[1] := GetNodeChild(Node^.Sibs[1], 3);
        Node^.Children[0].Sibs[2] := Node^.Children[1];
        Node^.Children[0].Sibs[3] := Node^.Children[3];

        Node^.Children[1].Sibs[0] := Node^.Children[0];
        Node^.Children[1].Sibs[1] := GetNodeChild(Node^.Sibs[1], 2);
        Node^.Children[1].Sibs[2] := GetNodeChild(Node^.Sibs[2], 0);
        Node^.Children[1].Sibs[3] := Node^.Children[2];

        Node^.Children[2].Sibs[0] := Node^.Children[3];
        Node^.Children[2].Sibs[1] := Node^.Children[1];
        Node^.Children[2].Sibs[2] := GetNodeChild(Node^.Sibs[2], 3);
        Node^.Children[2].Sibs[3] := GetNodeChild(Node^.Sibs[3], 1);

        Node^.Children[3].Sibs[0] := GetNodeChild(Node^.Sibs[0], 2);
        Node^.Children[3].Sibs[1] := Node^.Children[0];
        Node^.Children[3].Sibs[2] := Node^.Children[2];
        Node^.Children[3].Sibs[3] := GetNodeChild(Node^.Sibs[3], 0);
      end;
    end;
    if Node^.NodeType <> ntNoDiv then
    for i := 0 to High(Node^.Children) do
    SetNodeSibs(Node^.Children[i]);
  end;
  procedure SetNodeAlpha(const Node: PG2LandNode);
  var
    LowX, HighX, LowY, HighY: Integer;
    l, u, i, j, Alpha: Integer;
    Transp: array of Integer;
    c: TG2Color;
  begin
    SetLength(Transp, Length(m_Layers));
    ZeroMemory(@Transp[0], Length(Transp) * SizeOf(DWord));
    if Node^.NodeType = ntNoDiv then
    for l := High(m_Layers) downto 0 do
    begin
      LowX := Round(
        (
          m_Vertices[
            Node^.Vertices[1].X,
            Node^.Vertices[1].Y
          ].Pos0.x - m_PosX
        ) / m_SizeX * (m_Layers[l].Mask.Width - 1)
      );
      LowY := Round(
        (
          m_Vertices[
            Node^.Vertices[1].X,
            Node^.Vertices[1].Y
          ].Pos0.z - m_PosZ
        ) / m_SizeZ * (m_Layers[l].Mask.Width - 1)
      );
      HighX := Round(
        (
          m_Vertices[
            Node^.Vertices[5].X,
            Node^.Vertices[5].Y
          ].Pos0.x - m_PosX
        ) / m_SizeX * (m_Layers[l].Mask.Width - 1)
      );
      HighY := Round(
        (
          m_Vertices[
            Node^.Vertices[5].X,
            Node^.Vertices[5].Y
          ].Pos0.z - m_PosZ
        ) / m_SizeZ * (m_Layers[l].Mask.Width - 1)
      );
      Alpha := 0;
      m_Layers[l].Mask.Surfaces[0].Lock;
      for j := LowY to HighY do
      for i := LowX to HighX do
      begin
        c := m_Layers[l].Mask.Surfaces[0].Pixels[i, j];
        Alpha := Alpha + c.a;
        Transp[l] := Transp[l] + (255 - c.a);
      end;
      m_Layers[l].Mask.Surfaces[0].UnLock;
      for u := l + 1 to High(m_Layers) do
      if Transp[u] < $8 then
      Alpha := 0;
      Node^.LayerVis[l] := Alpha > $f;
    end
    else
    begin
      for i := 0 to High(Node^.Children) do
      SetNodeAlpha(Node^.Children[i]);
      for l := 0 to High(m_Layers) do
      begin
        Node^.LayerVis[l] := False;
        for i := 0 to High(Node^.Children) do
        Node^.LayerVis[l] := Node^.LayerVis[l] or Node^.Children[i].LayerVis[l];
      end;
    end;
  end;
  procedure SetNodeMorph(const Node: PG2LandNode);
    procedure SetMorph(const v0, v1, v2: PVertex);
    begin
      v0^.Pos1.x := (v1^.Pos0.y + v2^.Pos0.y) * 0.5;
    end;
  type
    TVInd = array[0..1] of Integer;
  const
    vindh: array[0..5] of TVInd = (
      (1, 2), (8, 0), (7, 6), (2, 3), (0, 4), (6, 5)
    );
    vindv: array[0..5] of TVInd = (
      (1, 8), (2, 0), (3, 4), (8, 7), (0, 6), (4, 5)
    );
    vindq: array[0..15] of TVInd = (
      (1, 2), (2, 3), (1, 8), (2, 0),
      (3, 4), (8, 0), (0, 4), (8, 7),
      (0, 6), (4, 5), (7, 6), (6, 5),
      (1, 0), (0, 3), (7, 0), (0, 5)
    );
    vind: array[0..4] of Integer = (0, 2, 4, 6, 8);
  var
    i: Integer;
    v0, v1, v2: PVertex;
    dist: Single;
  begin
    if not Assigned(Node^.Parent) then
    for i := 0 to 8 do
    with m_Vertices[Node^.Vertices[i].X, Node^.Vertices[i].Y] do
    begin
      Pos1.x := Pos0.y;
      Pos1.y := Node^.MagDist;
      Pos1.z := Node^.MagDist / m_LODPrg;
    end
    else
    for i := 0 to High(vind) do
    begin
      v0 := @m_Vertices[Node^.Vertices[0].X, Node^.Vertices[0].Y];
      v1 := @m_Vertices[Node^.Vertices[vind[i]].X, Node^.Vertices[vind[i]].Y];
      v2 := @m_Vertices[Node^.Parent^.Vertices[0].X, Node^.Parent^.Vertices[0].Y];
      dist := (v0^.Pos0 - v1^.Pos0).Len;
      v1^.Pos1.y := Max(Sqr(Sqrt(Node^.MagDist) + dist), v1^.Pos1.y);
      dist := (v2^.Pos0 - v0^.Pos0).Len * 1.2;
      if v1^.Pos1.z > 0.01 then
      v1^.Pos1.z := Min(Sqr(Sqrt(Node^.Parent^.MagDist) - dist), v1^.Pos1.z)
      else
      v1^.Pos1.z := Sqr(Sqrt(Node^.Parent^.MagDist) - dist);
      v1^.Pos1.z := Max(v1^.Pos1.y + 0.01, v1^.Pos1.z);
    end;
    if Node^.NodeType = ntNoDiv then Exit;
    if Node^.NodeType = ntDivH then
    for i := 0 to High(vindh) do
    begin
      v0 := @m_Vertices[
        (Node^.Vertices[vindh[i][0]].X + Node^.Vertices[vindh[i][1]].X) div 2,
        Node^.Vertices[vindh[i][0]].Y
      ];
      v1 := @m_Vertices[Node^.Vertices[vindh[i][0]].X, Node^.Vertices[vindh[i][0]].Y];
      v2 := @m_Vertices[Node^.Vertices[vindh[i][1]].X, Node^.Vertices[vindh[i][1]].Y];
      SetMorph(v0, v1, v2);
    end
    else if Node^.NodeType = ntDivV then
    for i := 0 to High(vindv) do
    begin
      v0 := @m_Vertices[
        Node^.Vertices[vindv[i][0]].X,
        (Node^.Vertices[vindv[i][0]].Y + Node^.Vertices[vindv[i][1]].Y) div 2
      ];
      v1 := @m_Vertices[Node^.Vertices[vindv[i][0]].X, Node^.Vertices[vindv[i][0]].Y];
      v2 := @m_Vertices[Node^.Vertices[vindv[i][1]].X, Node^.Vertices[vindv[i][1]].Y];
      SetMorph(v0, v1, v2);
    end
    else if Node^.NodeType = ntDivQ then
    for i := 0 to High(vindq) do
    begin
      v0 := @m_Vertices[
        (Node^.Vertices[vindq[i][0]].X + Node^.Vertices[vindq[i][1]].X) div 2,
        (Node^.Vertices[vindq[i][0]].Y + Node^.Vertices[vindq[i][1]].Y) div 2
      ];
      v1 := @m_Vertices[Node^.Vertices[vindq[i][0]].X, Node^.Vertices[vindq[i][0]].Y];
      v2 := @m_Vertices[Node^.Vertices[vindq[i][1]].X, Node^.Vertices[vindq[i][1]].Y];
      SetMorph(v0, v1, v2);
    end;
    for i := 0 to High(Node^.Children) do
    SetNodeMorph(Node^.Children[i]);
  end;
var
  XAsp, ZAsp, XSc, ZSc: Integer;
  i, j, XCount, ZCount, l: Integer;
  X, Z: Single;
  FVFDecl: TFVFDeclaration;
  {$IFDEF G2_WRITE_LOG}
  MemSize: DWord;
  s: AnsiString;
  {$ENDIF}
begin
  XAsp := Max(Trunc(m_SizeX / m_SizeZ), 1);
  ZAsp := Max(Trunc(m_SizeZ / m_SizeX), 1);
  XSc := 1;
  ZSc := 1;
  if XAsp > 1 then
  while XSc shl 1 <= XSc do
  XSc := XSc shl 1;
  if ZAsp > 1 then
  while ZSc shl 1 <= ZSc do
  ZSc := ZSc shl 1;
  XCount := Round(Power(2, m_LOD + 1) * XSc + 1);
  ZCount := Round(Power(2, m_LOD + 1) * ZSc + 1);
  SetLength(m_Vertices, XCount, ZCount);
  if (XCount <> m_HM.Width)
  or (ZCount <> m_HM.Height)
  then
  m_HM.Resample(XCount, ZCount);
  for j := 0 to ZCount - 1 do
  for i := 0 to XCount - 1 do
  begin
    X := i / (XCount - 1);
    Z := j / (ZCount - 1);
    m_Vertices[i, j].Pos0 := G2Vec3(
      X * m_SizeX,
      m_HM.Altitude[X, Z] * m_SizeY,
      Z * m_SizeZ
    );
    m_Vertices[i, j].Pos1.x := 0;
    m_Vertices[i, j].Pos1.y := 0;
    m_Vertices[i, j].Pos1.z := 0;
  end;
  m_RootNode := New(PG2LandNode);
  m_RootNode^.Parent := nil;
  m_RootNode^.LOD := 0;
  if m_MagDistRelative then
  m_RootNode^.MagDist := (m_SizeX + m_SizeY + m_SizeZ) * m_MagDist
  else
  m_RootNode^.MagDist := m_MagDist;
  m_RootNode^.Magnify := False;
  m_RootNode^.NodeType := ntNoDiv;                           
  m_RootNode^.Vertices[0] := Point((XCount - 1) div 2, (ZCount - 1) div 2);
  m_RootNode^.Vertices[1] := Point(0, 0);
  m_RootNode^.Vertices[2] := Point((XCount - 1) div 2, 0);
  m_RootNode^.Vertices[3] := Point((XCount - 1), 0);
  m_RootNode^.Vertices[4] := Point((XCount - 1), (ZCount - 1) div 2);
  m_RootNode^.Vertices[5] := Point((XCount - 1), (ZCount - 1));
  m_RootNode^.Vertices[6] := Point((XCount - 1) div 2, (ZCount - 1));
  m_RootNode^.Vertices[7] := Point(0, (ZCount - 1));
  m_RootNode^.Vertices[8] := Point(0, (ZCount - 1) div 2);
  ZeroMemory(@m_RootNode^.Sibs, Length(m_RootNode^.Sibs) * SizeOf(PG2LandNode));
  if m_LOD > 0 then
  BuildNode(m_RootNode);
  SetNodeBounds(m_RootNode);
  SetNodeSibs(m_RootNode);
  SetNodeMorph(m_RootNode);
  for l := 0 to High(m_Layers) do
  if (m_Layers[l].Mask.Format = D3DFMT_DXT1)
  or (m_Layers[l].Mask.Format = D3DFMT_DXT2)
  or (m_Layers[l].Mask.Format = D3DFMT_DXT3)
  or (m_Layers[l].Mask.Format = D3DFMT_DXT4)
  or (m_Layers[l].Mask.Format = D3DFMT_DXT5)
  then
  m_Layers[l].Mask.ChangeFormat(D3DFMT_A8R8G8B8);
  if m_Optimized then
  SetNodeAlpha(m_RootNode);
  SetLength(m_VArr, XCount * ZCount * Length(m_Layers));
  SetLength(m_IArr, (XCount - 1) * (ZCount - 1) * 6 * Length(m_Layers));
  SetLength(m_VISet, XCount, ZCount);
  SetLength(m_LayerBuffer, Length(m_Layers));
  FVFDecl[0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  FVFDecl[1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  FVFDecl[2] := D3DDECL_END;
  Core.Graphics.Device.CreateVertexDeclaration(
    @FVFDecl[0], m_Decl
  );
  Core.Graphics.Device.CreateVertexBuffer(
    SizeOf(TVertex) * XCount * ZCount * Length(m_Layers),
    D3DUSAGE_WRITEONLY,
    0,
    D3DPOOL_DEFAULT,
    m_VB,
    nil
  );
  Core.Graphics.Device.CreateIndexBuffer(
    SizeOf(DWord) * (XCount - 1) * (ZCount - 1) * 6 * Length(m_Layers),
    D3DUSAGE_WRITEONLY,
    D3DFMT_INDEX32,
    D3DPOOL_DEFAULT,
    m_IB,
    nil
  );
  m_ForceUpdate := True;
  InitTextures;
  m_Built := True;
  {$IFDEF G2_WRITE_LOG}
  MemSize := XCount * ZCount * SizeOf(TVertex) * Length(m_Layers);
  if MemSize < 1024 then s := AnsiString(IntToStr(MemSize)) + 'Bytes'
  else if MemSize < 1048576 then s := AnsiString(IntToStr(MemSize div 1024)) + 'Kb'
  else s := AnsiString(IntToStr(MemSize div 1048576)) + 'Mb';
  G2WriteLogTimed(
    '(>) Landscape Generated: Vertex Data = ' + s +
    ' (' + AnsiString(IntToStr(Core.Graphics.Specs.GetVRAMFree div 1048576)) + 'Mb VRAM Remaining)',
    'Landscape'
  );
  {$ENDIF}
end;

procedure TG2Landscape.LandDestroy;
  procedure FreeNode(Node: PG2LandNode);
  var
    i: Integer;
  begin
    if Node^.NodeType <> ntNoDiv then
    for i := 0 to High(Node^.Children) do
    FreeNode(Node^.Children[i]);
    Dispose(Node);
  end;
begin
  if not m_Built then Exit;
  FreeNode(m_RootNode);
  SafeRelease(m_Decl);
  SafeRelease(m_VB);
  SafeRelease(m_IB);
  m_Built := False;
end;

procedure TG2Landscape.Render;
var
  Tex: Integer;
  PrevDepthBias: Single;
  PrevAlphaTestEnable: Boolean;
  PrevAlphaRef: DWord;
begin
  if not m_Built then Exit;

  m_DrawCalls := 0;

  Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  Core.Graphics.Device.SetVertexDeclaration(m_Decl);
  Core.Graphics.Device.SetIndices(m_IB);

  PrevDepthBias := Core.Graphics.RenderStates.DepthBias;
  PrevAlphaTestEnable := Core.Graphics.RenderStates.AlphaTestEnable;
  PrevAlphaRef := Core.Graphics.RenderStates.AlphaRef;

  Core.Graphics.RenderStates.AlphaTestEnable := True;
  Core.Graphics.RenderStates.AlphaRef := $8;

  if Length(m_Layers) > 0 then
  case m_Mode of
    lmPlain:
    begin
      m_Effect.Technique := 'g2LandPlain';
      m_Effect.SetMatrix('g_WVP', Core.Graphics.Transforms.WVP);
      m_Effect.SetMatrix('g_WV', Core.Graphics.Transforms.WV);
      m_Effect.SetMatrix('g_W', Core.Graphics.Transforms.W[0]);
      m_Effect.SetVector('g_LightDir', m_Light.LightDirPriv);
      m_Effect.SetVector('g_LightDiffuse', m_Light.LightDiffusePriv);
      m_Effect.SetVector('g_LightAmbient', m_Light.LightAmbientPriv);
      m_Effect.SetVector('g_LandProps', G2Vec4(m_PosX, m_PosZ, 1 / m_SizeX, 1 / m_SizeZ));
      m_Effect.SetVector('g_CamPos', G2Vec4(Core.Graphics.Transforms.Vpos, 1));
      m_Effect.SetVector('g_TexRepeat', G2Vec4(m_Layers[0].TexRepeatU, m_Layers[0].TexRepeatV, 0, 0));
      m_Effect.SetFloat('g_SpecularPower', m_Layers[0].SpecularPower);
      m_Effect.SetTexture('TexNormals', m_Normals.Texture);
      m_Effect.SetTexture('TexMask', m_Layers[0].Mask.Texture);
      m_Effect.SetTexture('TexDiffuseSpecularMap', m_Layers[0].DiffuseSpecular.Texture);
      m_Effect.BeginEffect(nil);
      m_Effect.BeginPass(0);
      for Tex := 0 to High(m_Layers) do
      if m_Layers[Tex].Visible
      and (m_LayerBuffer[Tex].TriCount > 0) then
      begin
        Core.Graphics.RenderStates.DepthBias := -Tex * 1E-5;
        if Tex > 0 then
        begin
          m_Effect.SetVector('g_TexRepeat', G2Vec4(m_Layers[Tex].TexRepeatU, m_Layers[Tex].TexRepeatV, 0, 0));
          m_Effect.SetFloat('g_SpecularPower', m_Layers[Tex].SpecularPower);
          m_Effect.SetTexture('TexMask', m_Layers[Tex].Mask.Texture);
          m_Effect.SetTexture('TexDiffuseSpecularMap', m_Layers[Tex].DiffuseSpecular.Texture);
          m_Effect.CommitChanges;
        end;
        Core.Graphics.Device.DrawIndexedPrimitive(
          D3DPT_TRIANGLELIST,
          0,
          m_LayerBuffer[Tex].BufferStartV,
          m_LayerBuffer[Tex].BufferEndV - m_LayerBuffer[Tex].BufferStartV,
          m_LayerBuffer[Tex].BufferStartI,
          m_LayerBuffer[Tex].TriCount
        );
        Inc(m_DrawCalls);
      end;
      m_Effect.EndPass;
      m_Effect.EndEffect;   
    end;
    lmBump:
    begin
      m_Effect.Technique := 'g2LandBump';
      m_Effect.SetMatrix('g_WVP', Core.Graphics.Transforms.WVP);
      m_Effect.SetMatrix('g_WV', Core.Graphics.Transforms.WV);
      m_Effect.SetMatrix('g_W', Core.Graphics.Transforms.W[0]);
      m_Effect.SetVector('g_LightDir', m_Light.LightDirPriv);
      m_Effect.SetVector('g_LightDiffuse', m_Light.LightDiffusePriv);
      m_Effect.SetVector('g_LightAmbient', m_Light.LightAmbientPriv);
      m_Effect.SetVector('g_LandProps', G2Vec4(m_PosX, m_PosZ, 1 / m_SizeX, 1 / m_SizeZ));
      m_Effect.SetVector('g_CamPos', G2Vec4(Core.Graphics.Transforms.Vpos, 1));
      m_Effect.SetVector('g_TexRepeat', G2Vec4(m_Layers[0].TexRepeatU, m_Layers[0].TexRepeatV, 0, 0));
      m_Effect.SetFloat('g_SpecularPower', m_Layers[0].SpecularPower);
      m_Effect.SetTexture('TexNormals', m_Normals.Texture);
      m_Effect.SetTexture('TexMask', m_Layers[0].Mask.Texture);
      m_Effect.SetTexture('TexDiffuseSpecularMap', m_Layers[0].DiffuseSpecular.Texture);
      m_Effect.SetTexture('TexNormalHeightMap', m_Layers[0].NormalsHeights.Texture);
      m_Effect.BeginEffect(nil);
      m_Effect.BeginPass(0);
      for Tex := 0 to High(m_Layers) do
      if m_Layers[Tex].Visible
      and (m_LayerBuffer[Tex].TriCount > 0) then
      begin
        Core.Graphics.RenderStates.DepthBias := -Tex * 1E-5;
        if Tex > 0 then
        begin
          m_Effect.SetVector('g_TexRepeat', G2Vec4(m_Layers[Tex].TexRepeatU, m_Layers[Tex].TexRepeatV, 0, 0));
          m_Effect.SetFloat('g_SpecularPower', m_Layers[Tex].SpecularPower);
          m_Effect.SetTexture('TexMask', m_Layers[Tex].Mask.Texture);
          m_Effect.SetTexture('TexDiffuseSpecularMap', m_Layers[Tex].DiffuseSpecular.Texture);
          m_Effect.SetTexture('TexNormalHeightMap', m_Layers[Tex].NormalsHeights.Texture);
          m_Effect.CommitChanges;
        end;
        Core.Graphics.Device.DrawIndexedPrimitive(
          D3DPT_TRIANGLELIST,
          0,
          m_LayerBuffer[Tex].BufferStartV,
          m_LayerBuffer[Tex].BufferEndV - m_LayerBuffer[Tex].BufferStartV,
          m_LayerBuffer[Tex].BufferStartI,
          m_LayerBuffer[Tex].TriCount
        );
        Inc(m_DrawCalls);
      end;
      m_Effect.EndPass;
      m_Effect.EndEffect;
    end;
    lmParallax:
    begin
      m_Effect.Technique := 'g2LandParallax';
      m_Effect.SetMatrix('g_WVP', Core.Graphics.Transforms.WVP);
      m_Effect.SetMatrix('g_WV', Core.Graphics.Transforms.WV);
      m_Effect.SetMatrix('g_W', Core.Graphics.Transforms.W[0]);
      m_Effect.SetVector('g_LightDir', m_Light.LightDirPriv);
      m_Effect.SetVector('g_LightDiffuse', m_Light.LightDiffusePriv);
      m_Effect.SetVector('g_LightAmbient', m_Light.LightAmbientPriv);
      m_Effect.SetVector('g_LandProps', G2Vec4(m_PosX, m_PosZ, 1 / m_SizeX, 1 / m_SizeZ));
      m_Effect.SetVector('g_CamPos', G2Vec4(Core.Graphics.Transforms.Vpos, 1));
      m_Effect.SetVector('g_TexRepeat', G2Vec4(m_Layers[0].TexRepeatU, m_Layers[0].TexRepeatV, 0, 0));
      m_Effect.SetFloat('g_SpecularPower', m_Layers[0].SpecularPower);
      m_Effect.SetFloat('g_HeightMapScale', m_Layers[0].HeightMapScale);
      m_Effect.SetTexture('TexNormals', m_Normals.Texture);
      m_Effect.SetTexture('TexMask', m_Layers[0].Mask.Texture);
      m_Effect.SetTexture('TexDiffuseSpecularMap', m_Layers[0].DiffuseSpecular.Texture);
      m_Effect.SetTexture('TexNormalHeightMap', m_Layers[0].NormalsHeights.Texture);
      m_Effect.BeginEffect(nil);
      m_Effect.BeginPass(0);
      for Tex := 0 to High(m_Layers) do
      if m_Layers[Tex].Visible
      and (m_LayerBuffer[Tex].TriCount > 0) then
      begin
        Core.Graphics.RenderStates.DepthBias := -Tex * 1E-5;
        if Tex > 0 then
        begin
          m_Effect.SetVector('g_TexRepeat', G2Vec4(m_Layers[Tex].TexRepeatU, m_Layers[Tex].TexRepeatV, 0, 0));
          m_Effect.SetFloat('g_SpecularPower', m_Layers[Tex].SpecularPower);
          m_Effect.SetFloat('g_HeightMapScale', m_Layers[Tex].HeightMapScale);
          m_Effect.SetTexture('TexMask', m_Layers[Tex].Mask.Texture);
          m_Effect.SetTexture('TexDiffuseSpecularMap', m_Layers[Tex].DiffuseSpecular.Texture);
          m_Effect.SetTexture('TexNormalHeightMap', m_Layers[Tex].NormalsHeights.Texture);
          m_Effect.CommitChanges;
        end;
        Core.Graphics.Device.DrawIndexedPrimitive(
          D3DPT_TRIANGLELIST,
          0,
          m_LayerBuffer[Tex].BufferStartV,
          m_LayerBuffer[Tex].BufferEndV - m_LayerBuffer[Tex].BufferStartV,
          m_LayerBuffer[Tex].BufferStartI,
          m_LayerBuffer[Tex].TriCount
        );
        Inc(m_DrawCalls);
      end;
      m_Effect.EndPass;
      m_Effect.EndEffect;
    end;
    lmParallaxOcclusion:
    begin
      m_Effect.Technique := 'g2LandParallaxOcclusion';
      m_Effect.SetMatrix('g_WVP', Core.Graphics.Transforms.WVP);
      m_Effect.SetMatrix('g_WV', Core.Graphics.Transforms.WV);
      m_Effect.SetMatrix('g_W', Core.Graphics.Transforms.W[0]);
      m_Effect.SetVector('g_LightDir', m_Light.LightDirPriv);
      m_Effect.SetVector('g_LightDiffuse', m_Light.LightDiffusePriv);
      m_Effect.SetVector('g_LightAmbient', m_Light.LightAmbientPriv);
      m_Effect.SetVector('g_LandProps', G2Vec4(m_PosX, m_PosZ, 1 / m_SizeX, 1 / m_SizeZ));
      m_Effect.SetVector('g_CamPos', G2Vec4(Core.Graphics.Transforms.Vpos, 1));
      m_Effect.SetVector('g_TexRepeat', G2Vec4(m_Layers[0].TexRepeatU, m_Layers[0].TexRepeatV, 0, 0));
      m_Effect.SetVector('g_TexDims', G2Vec4(m_Layers[0].DiffuseSpecular.Width, m_Layers[0].DiffuseSpecular.Height, 0, 0));
      m_Effect.SetFloat('g_SpecularPower', m_Layers[0].SpecularPower);
      m_Effect.SetFloat('g_HeightMapScale', m_Layers[0].HeightMapScale);
      m_Effect.SetInt('g_MipThreshold', m_Layers[0].MipThreshold);
      m_Effect.SetFloat('g_MinSamples', m_Layers[0].MinSamples);
      m_Effect.SetFloat('g_MaxSamples', m_Layers[0].MaxSamples);
      m_Effect.SetTexture('TexNormals', m_Normals.Texture);
      m_Effect.SetTexture('TexMask', m_Layers[0].Mask.Texture);
      m_Effect.SetTexture('TexDiffuseSpecularMap', m_Layers[0].DiffuseSpecular.Texture);
      m_Effect.SetTexture('TexNormalHeightMap', m_Layers[0].NormalsHeights.Texture);
      m_Effect.BeginEffect(nil);
      m_Effect.BeginPass(0);
      for Tex := 0 to High(m_Layers) do
      if m_Layers[Tex].Visible
      and (m_LayerBuffer[Tex].TriCount > 0) then
      begin
        Core.Graphics.RenderStates.DepthBias := -Tex * 1E-5;
        if Tex > 0 then
        begin
          m_Effect.SetVector('g_TexRepeat', G2Vec4(m_Layers[Tex].TexRepeatU, m_Layers[Tex].TexRepeatV, 0, 0));
          m_Effect.SetVector('g_TexDims', G2Vec4(m_Layers[Tex].DiffuseSpecular.Width, m_Layers[Tex].DiffuseSpecular.Height, 0, 0));
          m_Effect.SetFloat('g_SpecularPower', m_Layers[Tex].SpecularPower);
          m_Effect.SetFloat('g_HeightMapScale', m_Layers[Tex].HeightMapScale);
          m_Effect.SetInt('g_MipThreshold', m_Layers[Tex].MipThreshold);
          m_Effect.SetFloat('g_MinSamples', m_Layers[Tex].MinSamples);
          m_Effect.SetFloat('g_MaxSamples', m_Layers[Tex].MaxSamples);
          m_Effect.SetTexture('TexMask', m_Layers[Tex].Mask.Texture);
          m_Effect.SetTexture('TexDiffuseSpecularMap', m_Layers[Tex].DiffuseSpecular.Texture);
          m_Effect.SetTexture('TexNormalHeightMap', m_Layers[Tex].NormalsHeights.Texture);
          m_Effect.CommitChanges;
        end;
        Core.Graphics.Device.DrawIndexedPrimitive(
          D3DPT_TRIANGLELIST,
          0,
          m_LayerBuffer[Tex].BufferStartV,
          m_LayerBuffer[Tex].BufferEndV - m_LayerBuffer[Tex].BufferStartV,
          m_LayerBuffer[Tex].BufferStartI,
          m_LayerBuffer[Tex].TriCount
        );
        Inc(m_DrawCalls);
      end;
      m_Effect.EndPass;
      m_Effect.EndEffect;
    end;
  end;
  Core.Graphics.RenderStates.DepthBias := PrevDepthBias;
  Core.Graphics.RenderStates.AlphaTestEnable := PrevAlphaTestEnable;
  Core.Graphics.RenderStates.AlphaRef := PrevAlphaRef;
end;

procedure TG2Landscape.Update;
var
  CurV: DWord;
  CurI: DWord;
  procedure AddTri(const v1, v2, v3: TPoint);
    function GetVertexIndex(const v: TPoint): Word;
    begin
      if m_VISet[v.X, v.Y] < $ffffffff then
      Result := m_VISet[v.X, v.Y]
      else
      begin
        Move(m_Vertices[v.X, v.Y], m_VArr[CurV], SizeOf(TVertex));
        m_VISet[v.X, v.Y] := CurV;
        Result := CurV;
        CurV := CurV + 1;
      end;
    end;
    procedure AddIndex(const Ind: Word);
    begin
      m_IArr[CurI] := Ind;
      CurI := CurI + 1;
    end;
  begin
    AddIndex(GetVertexIndex(v1));
    AddIndex(GetVertexIndex(v3));
    AddIndex(GetVertexIndex(v2));
    m_TriCount := m_TriCount + 1;
  end;
  procedure AddNode(const Node: PG2LandNode);
  begin
    if (Node^.Sibs[1] <> nil)
    and (Node^.Sibs[1]^.Parent <> nil)
    and (not Node^.Sibs[1]^.Parent^.Magnify) then
    AddTri(Node^.Vertices[0], Node^.Vertices[1], Node^.Vertices[3])
    else
    begin
      AddTri(Node^.Vertices[0], Node^.Vertices[1], Node^.Vertices[2]);
      AddTri(Node^.Vertices[0], Node^.Vertices[2], Node^.Vertices[3]);
    end;
    if (Node^.Sibs[2] <> nil)
    and (Node^.Sibs[2]^.Parent <> nil)
    and (not Node^.Sibs[2]^.Parent^.Magnify) then
    AddTri(Node^.Vertices[0], Node^.Vertices[3], Node^.Vertices[5])
    else
    begin
      AddTri(Node^.Vertices[0], Node^.Vertices[3], Node^.Vertices[4]);
      AddTri(Node^.Vertices[0], Node^.Vertices[4], Node^.Vertices[5]);
    end;
    if (Node^.Sibs[3] <> nil)
    and (Node^.Sibs[3]^.Parent <> nil)
    and (not Node^.Sibs[3]^.Parent^.Magnify) then
    AddTri(Node^.Vertices[0], Node^.Vertices[5], Node^.Vertices[7])
    else
    begin
      AddTri(Node^.Vertices[0], Node^.Vertices[5], Node^.Vertices[6]);
      AddTri(Node^.Vertices[0], Node^.Vertices[6], Node^.Vertices[7]);
    end;
    if (Node^.Sibs[0] <> nil)
    and (Node^.Sibs[0]^.Parent <> nil)
    and (not Node^.Sibs[0]^.Parent^.Magnify) then
    AddTri(Node^.Vertices[0], Node^.Vertices[7], Node^.Vertices[1])
    else
    begin
      AddTri(Node^.Vertices[0], Node^.Vertices[7], Node^.Vertices[8]);
      AddTri(Node^.Vertices[0], Node^.Vertices[8], Node^.Vertices[1]);
    end;
  end;
  procedure BufferNode(const Node: PG2LandNode);
  var
    i: Integer;
  begin
    if Node^.FrustumCheck = fcOutside then
    Exit;
    if Node^.Magnify then
    for i := 0 to High(Node^.Children) do
    BufferNode(Node^.Children[i])
    else
    AddNode(Node);
  end;
  procedure UpdateNode(const Node: PG2LandNode; const CurLayer: Integer);
  var
    CamNodeDist: Single;
    i: Integer;
  begin
    if not Node^.LayerVis[CurLayer] then
    Node^.FrustumCheck := fcOutside
    else
    begin
      if (Node^.Parent = nil)
      or (Node^.Parent^.FrustumCheck = fcIntersect) then
      Node^.FrustumCheck := m_Frustum.FrustumCheckBox(
        Node^.MinBounds, Node^.MaxBounds
      )
      else
      Node^.FrustumCheck := Node^.Parent^.FrustumCheck;
    end;
    if (Node^.NodeType = ntNoDiv)
    or (Node^.FrustumCheck = fcOutside) then Exit;
    CamNodeDist := (
      Sqr(m_ViewPos.x - Node^.Center.x) +
      Sqr(m_ViewPos.y - Node^.Center.y) +
      Sqr(m_ViewPos.z - Node^.Center.z)
    );
    Node^.Magnify := CamNodeDist <= Node^.MagDist;
    if Node^.Magnify then
    for i := 0 to High(Node^.Children) do
    UpdateNode(Node.Children[i], CurLayer);       
  end;
var
  i, Tex: Integer;
  Ptr: Pointer;
  VBSize: DWORD;
  IBSize: DWORD;
  PrevTri: DWord;
begin
  if not m_Built then Exit;
  if m_ForceRebuild then
  QuickReBuild;
  if not NeedUpdate then Exit;
  m_ViewPos := Core.Graphics.Transforms.Vpos;
  m_Frustum.Update;
  m_TriCount := 0;
  CurV := 0;
  CurI := 0;
  for Tex := 0 to High(m_Layers) do
  if m_Layers[Tex].Updated then
  begin
    PrevTri := m_TriCount;
    m_LayerBuffer[Tex].BufferStartI := CurI;
    m_LayerBuffer[Tex].BufferStartV := CurV;
    for i := 0 to High(m_VISet) do
    FillChar(m_VISet[i][0], Length(m_VISet[i]) * SizeOf(DWord), $ff);
    UpdateNode(m_RootNode, Tex);
    BufferNode(m_RootNode);
    m_LayerBuffer[Tex].BufferEndI := CurI;
    m_LayerBuffer[Tex].BufferEndV := CurV;
    m_LayerBuffer[Tex].TriCount := m_TriCount - PrevTri;
  end;
  VBSize := SizeOf(TVertex) * CurV;
  IBSize := SizeOf(DWORD) * CurI;
  m_VB.Lock(0, VBSize, Ptr, D3DLOCK_DISCARD);
  Move(m_VArr[0], Ptr^, VBSize);
  m_VB.Unlock;
  m_IB.Lock(0, IBSize, Ptr, D3DLOCK_DISCARD);
  Move(m_IArr[0], Ptr^, IBSize);
  m_IB.Unlock;
  m_VCount := CurV;
  m_PrevViewMat := Core.Graphics.Transforms.V;
end;

procedure TG2Landscape.ComputeNormals;
var
  i, j: Integer;
  n: TG2Vec3;
begin
  m_Normals.Surfaces[0].Lock;
  for j := 0 to m_Normals.Surfaces[0].Height - 1 do
  for i := 0 to m_Normals.Surfaces[0].Width - 1 do
  begin
    n := NormalAtPoint(
      (i / (m_Normals.Surfaces[0].Width - 1)) * m_SizeX,
      (j / (m_Normals.Surfaces[0].Height - 1)) * m_SizeZ
    );
    m_Normals.Surfaces[0].Pixels[i, j] := G2Color(G2Vec3(n.x, n.y, n.z));
  end;
  m_Normals.Surfaces[0].UnLock;
  m_Normals.GenerateMipMaps;
end;

function TG2Landscape.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_PosX := 0;
  m_PosY := 0;
  m_PosZ := 0;
  m_SizeX := 100;
  m_SizeY := 20;
  m_SizeZ := 100;
  m_LOD := 3;
  m_LODPrg := 0.5;
  m_MagDist := 2;
  m_MagDistRelative := True;
  m_Normals := TG2Texture2D.Create;
  m_Normals.Name := 'Landscape Normal Map';
  m_Normals.Initialize(Core);
  m_Built := False;
  m_ForceUpdate := False;
  m_ForceReBuild := False;
  m_TriCount := 0;
  m_DrawCalls := 0;
  m_Frustum.RefV := @Core.Graphics.Transforms.V;
  m_Frustum.RefP := @Core.Graphics.Transforms.P;
  m_HM := TG2HeightMap.Create;
  m_HM.SetSize(256, 256);
  m_Mode := lmPlain;
  m_Optimized := True;
  m_Effect := Core.Graphics.ShaderLib.RequestEffect('fx_Landscape');
  InitLayerCount := 1;
  m_Light := TG2LandLight.Create;
  m_GrassEngine := TG2GrassEngine.Create(Self);
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  Result := grOk;
end;

function TG2Landscape.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  m_GrassEngine.Free;
  m_Light.Free;
  if m_Built then
  LandDestroy;
  m_Normals.Finalize;
  m_Normals.Free;
  InitLayerCount := 0;
  m_HM.Free;
  Result := grOk;
end;
//TG2Landscape END

//TG2GrassEngine BEGIN
constructor TG2GrassEngine.Create(const Landscape: TG2Landscape);
begin
  inherited Create;
  m_Landscape := Landscape;
  m_Gfx := m_Landscape.Core.Graphics;
  m_TrMS := (
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Gfx.Params.Adapter,
      m_Gfx.Params.DeviceType,
      D3DFMT_X8R8G8B8,
      0,
      D3DRTYPE_SURFACE,
      D3DFMT_TRMS
    ) = S_OK
  );
  m_TrSS := (
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Gfx.Params.Adapter,
      m_Gfx.Params.DeviceType,
      D3DFMT_X8R8G8B8,
      0,
      D3DRTYPE_SURFACE,
      D3DFMT_TRSS
    ) = S_OK
  );
  m_Mode := gmNormal;
  m_AlphaRef := 200;
  m_VisDist := 100;
  m_FadeDist := 20;
  m_TriCount := 0;
  m_DrawCalls := 0;
  m_Elements := TList.Create;
  m_Items := TList.Create;
  m_SortedItems := TList.Create;
  m_VS := m_Gfx.ShaderLib.RequestVertexShader('vs_Grass');
  m_Scene := TG2Scene3D.Create;
  m_Scene.Initialize(m_Landscape.Core);
  m_Built := False;
end;

destructor TG2GrassEngine.Destroy;
begin
  GrassDestroy;
  m_Scene.Finalize;
  m_Scene.Free;
  FreeItems;
  FreeElements;
  m_SortedItems.Free;
  m_Items.Free;
  m_Elements.Free;
  inherited Destroy;
end;

procedure TG2GrassEngine.SetMode(const Value: TG2GrassMode);
begin
  if Value = gmSampled then
  begin
    if m_TrMS
    and m_Gfx.Params.Antialiasing
    and (m_Gfx.Params.AntialiasingSampleCount > 0) then
    m_Mode := Value;
  end
  else
  m_Mode := Value;
end;

function TG2GrassEngine.NeedUpdate: Boolean;
const
  Threshold = 1E-3;
var
  i, j: Integer;
  V: TG2Mat;
begin
  if m_ForceUpdate then
  begin
    Result := True;
    m_ForceUpdate := False;
    Exit;
  end;
  Result := False;
  V := m_Landscape.Core.Graphics.Transforms.V;
  for j := 0 to 3 do
  for i := 0 to 3 do
  if Abs(PG2MatRef(@V)^.m[i, j] - PG2MatRef(@m_PrevViewMat)^.m[i, j]) > Threshold then
  begin
    Result := True;
    Exit;
  end;
end;

function TG2GrassEngine.AddGrassElement(
      const Texture: TG2Texture2D;
      const Segments: Integer;
      const Size, Height, Swing: Single
    ): PG2GrassElement;
begin
  New(Result);
  Result^.Texture := Texture;
  Result^.Segments := Segments;
  Result^.Size := Size;
  Result^.Height := Height;
  Result^.VPI := Segments * 4;
  Result^.IPI := Segments * 6;
  Result^.Radius := Sqrt(Sqr(Size * 0.5) + Sqr(Height * 0.5)) + Swing;
  Result^.Swing := Swing;
  m_Elements.Add(Result);
end;

procedure TG2GrassEngine.AddGrassItem(
      const X, Z: Single;
      const Scale: Single;
      const Element: PG2GrassElement
    );
var
  Item: PG2GrassItem;
  Align: TG2Quat;
  Ang: Single;
begin
  New(Item);
  Item^.SceneItem := m_Scene.ItemCreate(0);
  Item^.Element := Element;
  Align := G2Vec3Rotation(G2Vec3(0, 1, 0), m_Landscape.NormalAtPoint(X, Z));
  Item^.Transform.SetScaling(Scale, Scale, Scale);
  Item^.Transform.RotateY(G2Random2Pi);
  Item^.Transform.Rotate(Align);
  Item^.Transform.Translate(X, m_Landscape.Altitude[X, Z], Z);
  Item^.Center := G2Vec3(0, Element^.Height * 0.5, 0) * Item^.Transform;
  Item^.Transform := Item^.Transform.Transpose;
  Item^.Radius := Item^.Element^.Radius * Scale;
  Ang := G2Random2Pi;
  Item^.SwingDir := G2Vec2(
    Sin(Ang) * Element^.Swing * Scale,
    Cos(Ang) * Element^.Swing * Scale
  );
  Item^.SwingRnd := 1000 + Random(1000);
  Item^.Alpha := 1;
  Item^.SceneItem^.Data := Item;
  Item^.SceneItem^.C := Item^.Center;
  Item^.SceneItem^.R := Item^.Radius;
  Item^.SceneItem^.MinV := Item^.SceneItem^.C;
  Item^.SceneItem^.MaxV := Item^.SceneItem^.C;
  m_Scene.ItemAdd(Item^.SceneItem);
  m_Items.Add(Item);
end;

procedure TG2GrassEngine.GrassBuild;
var
  e, i, j: Integer;
  VCount: Integer;
  ICount: Integer;
  pe: PG2GrassElement;
  FVFDecl: TFVFDeclaration;
  Vertices: PVertexArray;
  Indices: PDWordArray;
  v: array of TVertex;
  Ang, hs: Single;
  s, c: Extended;
  CurV: DWord;
  CurI: DWord;
begin
  if m_Built then GrassDestroy;
  m_ItemsPerDraw := (m_Gfx.Caps.MaxVertexShaderConst - 6) div 4;
  VCount := 0;
  ICount := 0;
  for i := 0 to m_Elements.Count - 1 do
  begin
    pe := PG2GrassElement(m_Elements[i]);
    pe^.VBIndex := VCount;
    pe^.VBCount := m_ItemsPerDraw * pe^.VPI;
    Inc(VCount, pe^.VBCount);
    pe^.IBIndex := ICount;
    pe^.IBCount := m_ItemsPerDraw * pe^.IPI;
    Inc(ICount, pe^.IBCount);
  end;
  FVFDecl[0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  FVFDecl[1] := D3DVertexElement(0, 4 * 4, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  FVFDecl[2] := D3DDECL_END;
  m_Gfx.Device.CreateVertexDeclaration(
    @FVFDecl[0], m_Decl
  );
  m_Gfx.Device.CreateVertexBuffer(
    VCount * SizeOf(TVertex),
    D3DUSAGE_WRITEONLY,
    0,
    D3DPOOL_MANAGED,
    m_VB,
    nil
  );
  m_Gfx.Device.CreateIndexBuffer(
    ICount * 4,
    D3DUSAGE_WRITEONLY,
    D3DFMT_INDEX32,
    D3DPOOL_MANAGED,
    m_IB,
    nil
  );
  m_VB.Lock(0, VCount * SizeOf(TVertex), Pointer(Vertices), D3DLOCK_DISCARD);
  m_IB.Lock(0, ICount * 4, Pointer(Indices), D3DLOCK_DISCARD);
  CurV := 0; CurI := 0;
  for e := 0 to m_Elements.Count - 1 do
  begin
    pe := PG2GrassElement(m_Elements[e]);
    SetLength(v, pe^.VPI);
    for i := 0 to pe^.Segments - 1 do
    begin
      hs := pe^.Size * 0.5;
      Ang := (i / pe^.Segments) * Pi;
      SinCos(Ang, s, c);
      v[i * 4 + 0].Pos0 := G2Vec4(s * hs, 0, c * hs, 0);
      v[i * 4 + 1].Pos0 := G2Vec4(s * hs, pe^.Height, c * hs, 0);
      v[i * 4 + 2].Pos0 := G2Vec4(-s * hs, 0, -c * hs, 0);
      v[i * 4 + 3].Pos0 := G2Vec4(-s * hs, pe^.Height, -c * hs, 0);
      v[i * 4 + 0].Tex0 := G2Vec3(0, 1, 0);
      v[i * 4 + 1].Tex0 := G2Vec3(0, 0, 1);
      v[i * 4 + 2].Tex0 := G2Vec3(1, 1, 0);
      v[i * 4 + 3].Tex0 := G2Vec3(1, 0, 1);
    end;
    for i := 0 to m_ItemsPerDraw - 1 do
    begin
      for j := 0 to High(v) do
      v[j].Pos0.w := i;
      Move(v[0], Vertices^[0], SizeOf(TVertex) * pe^.VPI);
      Inc(Vertices, pe^.VPI);
      for j := 0 to pe^.Segments - 1 do
      begin
        Indices^[CurI + 0] := CurV + 0; Indices^[CurI + 1] := CurV + 1; Indices^[CurI + 2] := CurV + 2;
        Indices^[CurI + 3] := CurV + 2; Indices^[CurI + 4] := CurV + 1; Indices^[CurI + 5] := CurV + 3;
        Inc(CurV, 4);
        Inc(CurI, 6);
      end;
    end;
  end;
  m_IB.Unlock;
  m_VB.Unlock;
  m_Scene.InitLOD := 3;
  m_Scene.InitMinV := G2Vec3(
    m_Landscape.InitPosX - m_Landscape.InitSizeX * 0.5,
    m_Landscape.InitPosY - m_Landscape.InitSizeY * 0.5,
    m_Landscape.InitPosZ - m_Landscape.InitSizeZ * 0.5
  );
  m_Scene.InitMaxV := G2Vec3(
    m_Landscape.InitPosX + m_Landscape.InitSizeX * 1.5,
    m_Landscape.InitPosY + m_Landscape.InitSizeY * 1.5,
    m_Landscape.InitPosZ + m_Landscape.InitSizeZ * 1.5
  );
  m_Scene.SceneBuild;
  m_Scene.FetchItems := m_SortedItems;
  m_ForceUpdate := True;
  m_Built := True;
end;

procedure TG2GrassEngine.GrassDestroy;
begin
  if not m_Built then Exit;
  m_Scene.SceneDestroy;
  SafeRelease(m_IB);
  SafeRelease(m_VB);
  SafeRelease(m_Decl);
  m_Built := False;
end;

procedure TG2GrassEngine.Render;
var
  i: Integer;
  Item: PG2GrassItem;
  WVPt: TG2Mat;
  PrevElement: PG2GrassElement;
  PrevAlphaBlendEnable: Boolean;
  PrevAlphaTestEnable: Boolean;
  PrevAddressU: DWord;
  PrevAddressV: DWord;
  PrevAlphaRef: DWord;
  ItemsToDraw: DWord;
  v4: TG2Vec4;
  procedure Flush;
  begin
    m_Gfx.Device.SetTexture(0, PrevElement^.Texture.Texture);
    m_Gfx.Device.DrawIndexedPrimitive(
      D3DPT_TRIANGLELIST,
      0,
      PrevElement^.VBIndex,
      ItemsToDraw * PrevElement^.VPI,
      PrevElement^.IBIndex,
      ItemsToDraw * PrevElement^.IPI div 3
    );
    ItemsToDraw := 0;
    Inc(m_DrawCalls);
  end;
begin
  if not m_Built then Exit;

  m_DrawCalls := 0;

  PrevAddressU := m_Gfx.SamplerStates.AddressU[0];
  PrevAddressV := m_Gfx.SamplerStates.AddressV[0];
  PrevAlphaBlendEnable := m_Gfx.RenderStates.AlphaBlendEnable;
  PrevAlphaTestEnable := m_Gfx.RenderStates.AlphaTestEnable;
  PrevAlphaRef := m_Gfx.RenderStates.AlphaRef;

  m_Gfx.SamplerStates.AddressU[0] := D3DTADDRESS_CLAMP;
  m_Gfx.SamplerStates.AddressV[0] := D3DTADDRESS_CLAMP;

  case m_Mode of
    gmNormal:
    begin
      m_Gfx.RenderStates.AlphaBlendEnable := False;
    end;
    gmBlended:
    begin
      m_Gfx.RenderStates.AlphaBlendEnable := True;
    end;
    gmSampled:
    begin
      m_Gfx.RenderStates.AlphaBlendEnable := False;
      m_Gfx.RenderStates.AdaptiveTessY := DWord(D3DFMT_TRMS);
    end;
  end;
  m_Gfx.RenderStates.AlphaTestEnable := True;
  m_Gfx.RenderStates.AlphaRef := m_AlphaRef;

  m_Gfx.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  m_Gfx.Device.SetVertexDeclaration(m_Decl);
  m_Gfx.Device.SetIndices(m_IB);
  m_VS.SetToDevice;

  WVPt := m_Gfx.Transforms.WVPt;
  m_VS.SetConstantF(2, @WVPt, 4);
  PrevElement := nil;
  ItemsToDraw := 0;
  if m_Mode = gmBlended then
  for i := m_SortedItems.Count - 1 downto 0 do
  begin
    Item := PG2GrassItem(m_SortedItems[i]);
    if (PrevElement <> Item^.Element)
    or (ItemsToDraw >= m_ItemsPerDraw) then
    begin
      if ItemsToDraw > 0 then
      Flush;
      PrevElement := Item^.Element;
    end;
    v4 := G2Vec4(Item^.SwingDir, Sin(G2PiTime(Item^.SwingRnd)), Item^.Alpha);
    m_VS.SetConstantF(ItemsToDraw * 4 + 6, @v4, 1);
    m_VS.SetConstantF(ItemsToDraw * 4 + 6 + 1, @Item^.Transform, 3);
    Inc(ItemsToDraw);
  end
  else
  for i := 0 to m_SortedItems.Count - 1 do
  begin
    Item := PG2GrassItem(m_SortedItems[i]);
    if (PrevElement <> Item^.Element)
    or (ItemsToDraw >= m_ItemsPerDraw) then
    begin
      if ItemsToDraw > 0 then
      Flush;
      PrevElement := Item^.Element;
    end;
    v4 := G2Vec4(Item^.SwingDir, Sin(G2PiTime(Item^.SwingRnd)), Item^.Alpha);
    m_VS.SetConstantF(ItemsToDraw * 4 + 6, @v4, 1);
    m_VS.SetConstantF(ItemsToDraw * 4 + 6 + 1, @Item^.Transform, 3);
    Inc(ItemsToDraw);
  end;
  if ItemsToDraw > 0 then Flush;
  
  m_Gfx.Device.SetVertexShader(nil);

  if m_Mode = gmSampled then
  m_Gfx.RenderStates.AdaptiveTessY := DWord(D3DFMT_UNKNOWN);
  m_Gfx.RenderStates.AlphaRef := PrevAlphaRef;
  m_Gfx.RenderStates.AlphaBlendEnable := PrevAlphaBlendEnable;
  m_Gfx.RenderStates.AlphaTestEnable := PrevAlphaTestEnable;
  m_Gfx.SamplerStates.AddressV[0] := PrevAddressV;
  m_Gfx.SamplerStates.AddressU[0] := PrevAddressU;
end;

procedure TG2GrassEngine.Update;
var
  i, j: Integer;
  Item: PG2Scene3DItem;
  Frustum: TG2Frustum;
  CamDir: TG2Vec3;
  CamPos: TG2Vec3;
  procedure AddItemDistanceSorted(const GrassItem: PG2GrassItem);
  var
    l, h, m: Integer;
    dif: Single;
  begin
    GrassItem^.Dist := (GrassItem^.Center - CamPos).Len;
    GrassItem^.Alpha := (
      1 - Min(Max(GrassItem^.Dist - (m_VisDist - m_FadeDist), 0), m_FadeDist) / m_FadeDist
    );
    if GrassItem^.Alpha > 0.01 then
    begin
      l := 0;
      h := m_Scene.FetchItems.Count - 1;
      while l <= h do
      begin
        m := (l + h) div 2;
        dif := PG2GrassItem(m_Scene.FetchItems[m])^.Dist - GrassItem^.Dist;
        if dif < 0 then l := m + 1
        else h := m - 1;
      end;
      m_Scene.FetchItems.Insert(l, GrassItem);
    end;
  end;
  procedure AddItemElementSorted(const GrassItem: PG2GrassItem);
  var
    l, h, m: Integer;
    dif: Int64;
  begin
    GrassItem^.Dist := (GrassItem^.Center - CamPos).Len;
    GrassItem^.Alpha := (
      1 - Min(Max(GrassItem^.Dist - (m_VisDist - m_FadeDist), 0), m_FadeDist) / m_FadeDist
    );
    if GrassItem^.Alpha > 0.01 then
    begin
      l := 0;
      h := m_Scene.FetchItems.Count - 1;
      while l <= h do
      begin
        m := (l + h) div 2;
        dif := DWord(PG2GrassItem(m_Scene.FetchItems[m])^.Element) - DWord(GrassItem^.Element);
        if dif < 0 then l := m + 1
        else h := m - 1;
      end;
      m_Scene.FetchItems.Insert(l, GrassItem);
    end;
  end;
begin
  if not m_Built then Exit;
  if not NeedUpdate then Exit;
  m_TriCount := 0;
  CamDir := m_Gfx.Transforms.Vdir;
  CamDir.Normalize;
  CamPos := m_Gfx.Transforms.Vpos;
  Frustum := m_Landscape.Frustum;
  with m_Scene do
  begin
    FetchItems.Clear;
    if CurFetchID >= High(DWord) - 1 then
    ResetFetchID;
    CurFetchID := CurFetchID + 1;
    QueryNodes(Frustum);
    for i := 0 to FetchNodes.Count - 1 do
    if PG2Scene3DNode(FetchNodes[i])^.FrustumCheck = fcInside then
    begin
      for j := 0 to PG2Scene3DNode(FetchNodes[i])^.Items.Count - 1 do
      begin
        Item := PG2Scene3DItem(PG2Scene3DNode(FetchNodes[i])^.Items[j]);
        if Item^.FetchID < CurFetchID then
        begin
          Item^.FetchID := CurFetchID;
          if m_Mode = gmSampled then
          AddItemElementSorted(PG2GrassItem(Item^.Data))
          else
          AddItemDistanceSorted(PG2GrassItem(Item^.Data));
          Inc(m_TriCount, PG2GrassItem(Item^.Data)^.Element^.Segments * 2);
        end;
      end;
    end
    else
    begin
      for j := 0 to PG2Scene3DNode(FetchNodes[i])^.Items.Count - 1 do
      begin
        Item := PG2Scene3DItem(PG2Scene3DNode(FetchNodes[i])^.Items[j]);
        if (Item^.FetchID < CurFetchID)
        and (
          Frustum.SphereInFrustum(
            PG2GrassItem(Item^.Data)^.Center,
            PG2GrassItem(Item^.Data)^.Radius
          )
        ) then
        begin
          Item^.FetchID := CurFetchID;
          if m_Mode = gmSampled then
          AddItemElementSorted(PG2GrassItem(Item^.Data))
          else
          AddItemDistanceSorted(PG2GrassItem(Item^.Data));
          Inc(m_TriCount, PG2GrassItem(Item^.Data)^.Element^.Segments * 2);
        end;
      end;
    end;
  end;
  m_PrevViewMat := m_Landscape.Core.Graphics.Transforms.V;
end;

procedure TG2GrassEngine.RenderScene;
var
  Prim3D: TG2Primitives3D;
  procedure RenderNode(const n: PG2Scene3DNode);
  var
    nx, ny, nz: Integer;
  begin
    if n^.DivN then
    with n^ do
    begin
      Prim3D.DrawLine(G2Vec3(MinV.x, MinV.y, MinV.z), G2Vec3(MaxV.x, MinV.y, MinV.z), $ffff0000);
      Prim3D.DrawLine(G2Vec3(MinV.x, MinV.y, MinV.z), G2Vec3(MinV.x, MaxV.y, MinV.z), $ffff0000);
      Prim3D.DrawLine(G2Vec3(MinV.x, MinV.y, MinV.z), G2Vec3(MinV.x, MinV.y, MaxV.z), $ffff0000);
    end
    else
    for nx := n^.NodeMinX to n^.NodeMaxX do
    for ny := n^.NodeMinY to n^.NodeMaxY do
    for nz := n^.NodeMinZ to n^.NodeMaxZ do
    RenderNode(n^.Children[nx, ny, nz]);
  end;
begin
  m_Landscape.Core.RequestMod(TG2Primitives3D, @Prim3D);
  RenderNode(m_Scene.RootNode);
  m_Landscape.Core.ReleaseMod(@Prim3D);
end;

procedure TG2GrassEngine.FreeElements;
var
  i: Integer;
begin
  for i := 0 to m_Elements.Count - 1 do
  Dispose(PG2GrassElement(m_Elements[i]));
  m_Elements.Clear;
end;

procedure TG2GrassEngine.FreeItems;
var
  i: Integer;
begin
  for i := 0 to m_Items.Count - 1 do
  begin
    m_Scene.ItemDestroy(PG2GrassItem(m_Items[i])^.SceneItem);
    Dispose(PG2GrassItem(m_Items[i]));
  end;
  m_Items.Clear;
end;
//TG2GrassEngine END

end.
