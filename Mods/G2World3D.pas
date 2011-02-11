//G2World3D v1.0
unit G2World3D;

{$include ../Gen2.inc}

interface

uses
  Windows,
  Classes,
  Types,
  Math,
  SysUtils,
  Direct3D9,
  D3DX9,
  DXTypes,
  Gen2,
  G2Math,
  G2MeshLoader,
  G2MeshLoaderG2M;

type
  TG2World3D = class;
  TG2WCharMgr = class;
  TG2WChar = class;

  TG2World3D = class (TG2Module)
  strict private
    type TG2WSampler = (
      ssX1 = 0, ssX4 = 1, ssX9 = 2, ssX16 = 3
    );
    type TG2WShadowMode = (
      smPCF, smVSM
    );
    type TG2WRenderMode = (
      rmLightMap,
      rmMultiPass
    );
    type TG2WNode = record
      var OwnerID: Integer;
      var Name: AnsiString;
      var TransformDef: TG2Mat;
      var TransformCur: TG2Mat;
      var TransformRen: TG2Mat;
      var Slaves: array of Integer;
    end;
    type PG2WNode = ^TG2WNode;
    type TG2WGeom = record
      const VertexStride: array[0..4] of Byte = (64, 60, 72, 80, 88);
      var NodeID: Integer;
      var VCount: Integer;
      var FCount: Integer;
      var GCount: Integer;
      var VB: IDirect3DVertexBuffer9;
      var IB: IDirect3DIndexBuffer9;
      var Groups: array of record
        var MatID: DWord;
        var VStart: DWord;
        var VCount: DWord;
        var FStart: DWord;
        var FCount: DWord;
      end;
      var Skinned: Boolean;
      var MaxWeights: Integer;
      var BCount: Integer;
      var Bones: array of record
        var NodeID: Integer;
        var Bind: TG2Mat;
        var BBox: TG2Box;
        var VCount: Integer;
      end;
      var SkinTransforms: array of TG2Mat;
      var BBox: TG2Box;
      var AABox: TG2AABox;
    end;
    type PG2WGeom = ^TG2WGeom;
    type TG2WAnim = record
      var Name: AnsiString;
      var FrameRate: Integer;
      var FrameCount: Integer;
      var NodeCount: Integer;
      var Nodes: array of record
      public
        var NodeID: Integer;
        var Frames: array of record
        public
          var Scale: TG2Vec3;
          var Rotation: TG2Quat;
          var Translation: TG2Vec3;
        end;
      end;
    end;
    type PG2WAnim = ^TG2WAnim;
    type TG2WMat = record
      var Name: AnsiString;
      var TexDiffuse: AnsiString;
      var TexNormals: AnsiString;
      var TexSpecular: AnsiString;
      var TexLightMap: AnsiString;
    end;
    type TG2WLightOmni = record
      var NodeID: Integer;
      var Color: TG2Color;
      var AttStart: Single;
      var AttEnd: Single;
      var Depth: TG2TextureCubeRT;
      var Visible: Boolean;
      var VisibleFaces: array[0..5] of Boolean;
    end;
    type PG2WLightOmni = ^TG2WLightOmni;
    type TG2WLightSpot = record
      var NodeID: Integer;
      var Color: TG2Color;
      var AttStart: Single;
      var AttEnd: Single;
      var SpotInner: Single;
      var SpotOutter: Single;
      var SpotFOV: Single;
      var Depth: TG2Texture2DRT;
      var Visible: Boolean;
    end;
    type PG2WLightSpot = ^TG2WLightSpot;
    type PG2WMat = ^TG2WMat;
    type TG2WCollider = record
      var NodeID: Integer;
      var AABox: TG2AABox;
      var Vertices: array of TG2Vec3;
      var VerticesT: array of TG2Vec3;
      var Faces: array of record
        var Face: array[0..2] of Word;
        var Plane: TG2Plane;
        var Edges: array[0..2] of record
          var Indices: array[0..1] of Word;
          var N: TG2Vec3;
          var D: Single;
        end;
        var ColID: DWord;
      end;
    end;
    type PG2WCollider = ^TG2WCollider;
    var m_Render2D: TG2Render2D;
    var m_Prim2D: TG2Primitives2D;
    var m_TextureDir: AnsiString;
    var m_MgrTextures: TG2TextureMgr;
    var m_Effect: TG2Effect;
    var m_RootNodes: array of Integer;
    var m_NodeCount: Integer;
    var m_GeomCount: Integer;
    var m_AnimCount: Integer;
    var m_MatCount: Integer;
    var m_LightOmniCount: Integer;
    var m_LightSpotCount: Integer;
    var m_ColliderCount: Integer;
    var m_Nodes: array of TG2WNode;
    var m_Geoms: array of TG2WGeom;
    var m_Anims: array of TG2WAnim;
    var m_Mats: array of TG2WMat;
    var m_LightsOmni: array of TG2WLightOmni;
    var m_LightsSpot: array of TG2WLightSpot;
    var m_Colliders: array of TG2WCollider;
    var m_LightDepthSurface: TG2SurfaceDS;
    var m_BlurRT: TG2Texture2DRT;
    var m_ShadowProjRT: TG2Texture2DRT;
    var m_ShadowProjDS: TG2SurfaceDS;
    var m_ShadowProjV: TG2Mat;
    var m_ShadowProjP: TG2Mat;
    var m_FVFDecl: array [0..4] of TFVFDeclaration;
    var m_Decl: array [0..4] of IDirect3DVertexDeclaration9;
    var m_AnimIndex: Integer;
    var m_AnimFrame: Single;
    var m_AnimSpeed: Single;
    var m_ShadowMode: TG2WShadowMode;
    var m_ShadowMapSize: Word;
    var m_ShadowCurLight: Integer;
    var m_ShadowCubeSmapleCount: TG2WSampler;
    var m_ShadowProjSmapleCount: TG2WSampler;
    var m_ShadowCubeSamplerScale: Single;
    var m_ShadowProjSamplerScale: Single;
    var m_ShadowCubeStep: Single;
    var m_ShadowProjStep: Single;
    var m_DepthBias: Single;
    var m_SlopeBias: Single;
    var m_CurColID: DWord;
    var m_CubeSampleMap: array[0..15] of TG2Vec4;
    var m_ProjSampleMap: array[0..15] of TG2Vec4;
    var m_MgrMeshes: TG2MeshMgr;
    var m_MgrChars: TG2WCharMgr;
    var m_RenderMode: TG2WRenderMode;
    var m_Gravity: TG2Vec3;
    procedure ComputeTransforms;
    function GetAnimation: AnsiString;
    procedure SetAnimation(const Value: AnsiString);
    function GetGeomBBox(const GeomID: Integer): TG2Box;
    function GetNode(const Index: Integer): PG2WNode; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RenderLM;
    procedure RenderMP;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Effect: TG2Effect read m_Effect;
    property TextureDir: AnsiString read m_TextureDir write m_TextureDir;
    property Animation: AnsiString read GetAnimation write SetAnimation;
    property AnimSpeed: Single read m_AnimSpeed write m_AnimSpeed;
    property DepthBias: Single read m_DepthBias write m_DepthBias;
    property SlopeBias: Single read m_SlopeBias write m_SlopeBias;
    property ShadowProjV: TG2Mat read m_ShadowProjV write m_ShadowProjV;
    property ShadowProjP: TG2Mat read m_ShadowProjP write m_ShadowProjP;
    property MgrMeshes: TG2MeshMgr read m_MgrMeshes;
    property MgrChars: TG2WCharMgr read m_MgrChars;
    property RenderMode: TG2WRenderMode read m_RenderMode write m_RenderMode;
    property Nodes[const Index: Integer]: PG2WNode read GetNode;
    property NodeCount: Integer read m_NodeCount;
    property Gravity: TG2Vec3 read m_Gravity write m_Gravity;
    procedure Load(const FileName: String);
    procedure Render;
    procedure Update;
    procedure Collide(var c, v: TG2Vec3; var Grounded: Boolean; const r: Single; const StepHeight: Single = 0);
    function RayCollide(const r: TG2Ray; const Len: Single; var d: Single; var n: TG2Vec3): Boolean;
    function FindNode(const Name: AnsiString): PG2WNode;
    function FindGeom(const Name: AnsiString): PG2WGeom;
    function FindTexture(const Name: AnsiString): TG2Texture2D;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;

  TG2WCharMgr = class (TG2ResMgr)
  strict private
    var m_World3D: TG2World3D;
    function GetChar(const Index: Integer): TG2WChar; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property World3D: TG2World3D read m_World3D write m_World3D;
    property Chars[const Index: Integer]: TG2WChar read GetChar; default;
    constructor Create; override;
    destructor Destroy; override;
    function CreateCharacter(const Name: AnsiString; const Mesh: TG2Mesh): TG2WChar;
  end;

  TG2WChar = class (TG2Res)
  strict protected
    var m_World3D: TG2World3D;
    var m_Mesh: TG2Mesh;
    var m_MeshInst: TG2MeshInst;
    var m_UserData: Pointer;
  public
    var Pos: TG2Vec3;
    var Vel: TG2Vec3;
    var Rad: Single;
    var Ang: Single;
    var StepHeight: Single;
    var Scale: TG2Vec3;
    var LightAmount: Single;
    var Alpha: Single;
    var Grounded: Boolean;
    var DoFriction: Boolean;
    var Active: Boolean;
    property World3D: TG2World3D read m_World3D write m_World3D;
    property Mesh: TG2Mesh read m_Mesh write m_Mesh;
    property MeshInst: TG2MeshInst read m_MeshInst write m_MeshInst;
    property UserData: Pointer read m_UserData write m_UserData;
    function GetFinalTransform(const NodeName: AnsiString): TG2Mat;
    constructor Create; override;
    destructor Destroy; override;
    procedure Render; virtual;
    procedure Update; virtual;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;

implementation

//TG2World3D BEGIN
constructor TG2World3D.Create;
begin
  inherited Create;
  m_TextureDir := AppPath;
  m_ShadowMode := smVSM;
  m_ShadowMapSize := 256;
  m_ShadowCurLight := 0;
  m_AnimIndex := -1;
  m_AnimSpeed := 1;
  m_ShadowCubeSamplerScale := 0.2;
  m_ShadowProjSamplerScale := 0.5;
  m_ShadowCubeSmapleCount := ssX1;
  m_ShadowProjSmapleCount := ssX1;
  m_DepthBias := 32 * G2EPS;
  m_SlopeBias := 32 * G2EPS;
  m_RenderMode := rmLightMap;
  m_Gravity.SetValue(0, -0.1, 0);
end;

destructor TG2World3D.Destroy;
begin
  inherited Destroy;
end;

procedure TG2World3D.ComputeTransforms;
  procedure ComputeNode(const n: PG2WNode);
  var
    i: Integer;
  begin
    if n^.OwnerID = -1 then
    n^.TransformRen := n^.TransformCur
    else
    n^.TransformRen := n^.TransformCur * m_Nodes[n^.OwnerID].TransformRen;
    for i := 0 to High(n^.Slaves) do
    ComputeNode(@m_Nodes[n^.Slaves[i]]);
  end;
var
  i, j, k: Integer;
  c: TG2Vec3;
  b: Boolean;
  d: Single;
  v: TG2Vec3;
begin
  for i := 0 to High(m_RootNodes) do
  ComputeNode(@m_Nodes[m_RootNodes[i]]);
  for i := 0 to m_GeomCount - 1 do
  if m_Geoms[i].Skinned then
  for j := 0 to m_Geoms[i].BCount - 1 do
  m_Geoms[i].SkinTransforms[j] := m_Geoms[i].Bones[j].Bind * m_Nodes[m_Geoms[i].Bones[j].NodeID].TransformRen;
  for i := 0 to m_ColliderCount - 1 do
  begin
    D3DXVec3TransformCoordArray(
      @m_Colliders[i].VerticesT[0],
      12,
      @m_Colliders[i].Vertices[0],
      12,
      m_Nodes[m_Colliders[i].NodeID].TransformRen,
      Length(m_Colliders[i].Vertices)
    );
    m_Colliders[i].AABox.MinV := m_Colliders[i].VerticesT[0];
    m_Colliders[i].AABox.MaxV := m_Colliders[i].AABox.MinV;
    for j := 1 to High(m_Colliders[i].VerticesT) do
    m_Colliders[i].AABox := m_Colliders[i].AABox + m_Colliders[i].VerticesT[j];
    for j := 0 to High(m_Colliders[i].Faces) do
    begin
      m_Colliders[i].Faces[j].Plane.SetPlane(
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[0]],
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[1]],
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[2]]
      );
      m_Colliders[i].Faces[j].Plane.Normalize;
      m_Colliders[i].Faces[j].Edges[0].Indices[0] := m_Colliders[i].Faces[j].Face[0];
      m_Colliders[i].Faces[j].Edges[0].Indices[1] := m_Colliders[i].Faces[j].Face[1];
      m_Colliders[i].Faces[j].Edges[1].Indices[0] := m_Colliders[i].Faces[j].Face[1];
      m_Colliders[i].Faces[j].Edges[1].Indices[1] := m_Colliders[i].Faces[j].Face[2];
      m_Colliders[i].Faces[j].Edges[2].Indices[0] := m_Colliders[i].Faces[j].Face[2];
      m_Colliders[i].Faces[j].Edges[2].Indices[1] := m_Colliders[i].Faces[j].Face[0];
      c := (
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[0]] +
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[1]] +
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[2]]
      ) / 3;
      for k := 0 to 2 do
      begin
        d := G2Vec3ToLine(
          m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Edges[k].Indices[0]],
          m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Edges[k].Indices[1]],
          c, v, b
        );
        m_Colliders[i].Faces[j].Edges[k].N := (v - c) / d;
        m_Colliders[i].Faces[j].Edges[k].D := m_Colliders[i].Faces[j].Edges[k].N.Dot(v);
      end;
    end;
  end;
end;

function TG2World3D.GetAnimation: AnsiString;
begin
  if (m_AnimIndex > -1) and (m_AnimIndex < m_AnimCount) then
  Result := m_Anims[m_AnimIndex].Name
  else
  Result := '';
end;

procedure TG2World3D.SetAnimation(const Value: AnsiString);
var
  i: Integer;
begin
  for i := 0 to m_AnimCount - 1 do
  if m_Anims[i].Name = Value then
  begin
    m_AnimIndex := i;
    m_AnimFrame := 0;
    Exit;
  end;
end;

function TG2World3D.GetGeomBBox(const GeomID: Integer): TG2Box;
  var AABox: TG2AABox;
  var ib, b: Integer;
begin
  if m_Geoms[GeomID].Skinned and (m_Geoms[GeomID].BCount > 0) then
  begin
    for ib := 0 to m_Geoms[GeomID].BCount - 1 do
    if m_Geoms[GeomID].Bones[ib].VCount > 0 then
    begin
      AABox := (m_Geoms[GeomID].Bones[ib].BBox * m_Geoms[GeomID].SkinTransforms[ib]).AABox;
      Break;
    end;
    for b := ib + 1 to m_Geoms[GeomID].BCount - 1 do
    if m_Geoms[GeomID].Bones[b].VCount > 0 then
    AABox := AABox + (m_Geoms[GeomID].Bones[b].BBox * m_Geoms[GeomID].SkinTransforms[b]).AABox;
    Result.C := (AABox.MaxV + AABox.MinV) * 0.5;
    Result.vx.SetValue((AABox.MaxV.x - AABox.MinV.x) * 0.5, 0, 0);
    Result.vy.SetValue(0, (AABox.MaxV.y - AABox.MinV.y) * 0.5, 0);
    Result.vz.SetValue(0, 0, (AABox.MaxV.z - AABox.MinV.z) * 0.5);
  end
  else
  Result := m_Geoms[GeomID].BBox * m_Nodes[m_Geoms[GeomID].NodeID].TransformRen;
end;

function TG2World3D.GetNode(const Index: Integer): PG2WNode;
begin
  Result := @m_Nodes[Index];
end;

procedure TG2World3D.RenderLM;
  var i, j, g: Integer;
  var W, WVP: TG2Mat;
  var CamPos, v: TG2Vec3;
  var c: TG2WChar;
  var BBox: TG2Box;
  var Frustum: TG2Frustum;
  var ProjVP: TG2Mat;
  var Ambient: TG2Color;
  var PrevRT, PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
  var PrevAlphaBlendEnable: Boolean;
  var PrevSrcBlend: DWord;
  var PrevDstBlend: DWord;
  var ViewPort: TD3DViewPort9;
begin
  PrevZEnable := Core.Graphics.RenderStates.ZEnable;
  PrevAlphaBlendEnable := Core.Graphics.RenderStates.AlphaBlendEnable;
  PrevSrcBlend := Core.Graphics.RenderStates.SrcBlend;
  PrevDstBlend := Core.Graphics.RenderStates.DestBlend;
  CamPos := Core.Graphics.Transforms.Vpos;
  Ambient := $ff0b0024;
  Core.Graphics.RenderStates.ZEnable := True;
  Core.Graphics.RenderStates.ZWriteEnable := True;

  Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  Core.Graphics.Device.GetViewport(ViewPort);
  Core.Graphics.SetRenderTargetTexture2D(m_ShadowProjRT);
  Core.Graphics.SetDepthStencilSurface(m_ShadowProjDS);

  Core.Graphics.Device.Clear(0, nil, D3DCLEAR_ZBUFFER or D3DCLEAR_TARGET, $ffffffff, 1, 0);
  Frustum.RefV := @m_ShadowProjV;
  Frustum.RefP := @m_ShadowProjP;
  Frustum.Update;
  ProjVP := m_ShadowProjV * m_ShadowProjP;

  Core.Graphics.RenderStates.AlphaBlendEnable := False;
  Core.Graphics.Device.SetTexture(0, nil);

  {Core.Graphics.RenderStates.ZEnable := False;
  m_Effect.Technique := 'g2DepthClear';
  m_Effect.BeginEffect(nil);
  m_Effect.BeginPass(0);
  m_Prim2D.DrawRect(0, 0, m_ShadowProjRT.Width, m_ShadowProjRT.Height, $ffffffff);
  m_Effect.EndPass;
  m_Effect.EndEffect;
  Core.Graphics.RenderStates.ZEnable := True;  }

  m_Effect.Technique := 'g2ShadowDepth';
  m_Effect.BeginEffect(nil);
  m_Effect.BeginPass(0);
  for i := 0 to m_MgrChars.Count - 1 do
  if m_MgrChars[i].Active then
  begin
    c := m_MgrChars[i];
    for j := 0 to c.MeshInst.Mesh.GeomCount - 1 do
    begin
      W.SetScaling(c.Scale);
      W.RotateY(c.Ang);
      W.Translate(c.Pos);
      BBox := c.MeshInst.GeomBBox[j] * W;
      if Frustum.BoxInFrustum(BBox.AABox) then
      begin
        m_Effect.SetInt('VS_Index', c.MeshInst.Mesh.Geoms[j].MaxWeights);
        if c.MeshInst.Mesh.Geoms[j].Skinned then
        m_Effect.SetMatrixArray('g_SkinPallete', PD3DXMatrix(c.MeshInst.SkinTransforms[j]), c.Mesh.Geoms[j].BoneCount)
        else
        W := c.MeshInst.NodeTransforms[c.Mesh.Geoms[j].NodeID].TransformRen * W;
        WVP := W * ProjVP;
        m_Effect.SetMatrix('g_WVP', WVP);
        m_Effect.CommitChanges;
        for g := 0 to c.Mesh.Geoms[j].MaterialCount - 1 do
        c.MeshInst.Mesh.Geoms[j].Mesh.DrawSubset(g);
      end;
    end;
  end;
  m_Effect.EndPass;
  m_Effect.EndEffect;

  Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  Core.Graphics.Device.SetViewport(ViewPort);

  Core.Graphics.RenderStates.AlphaBlendEnable := True;
  Core.Graphics.RenderStates.SrcBlend := D3DBLEND_ZERO;
  Core.Graphics.RenderStates.DestBlend := D3DBLEND_ONE;
  Core.Graphics.Device.SetVertexDeclaration(m_Decl[0]);
  Core.Graphics.Device.SetVertexShader(nil);
  Core.Graphics.Device.SetPixelShader(nil);
  Core.Graphics.Device.SetTexture(0, nil);
  Core.Graphics.RenderStates.DepthBias := m_DepthBias;
  Core.Graphics.RenderStates.SlopeScaleDepthBias := m_SlopeBias;

  for i := 0 to m_GeomCount - 1 do
  if not m_Geoms[i].Skinned
  and Frustum.BoxInFrustum(m_Geoms[i].AABox) then
  begin
    Core.Graphics.Device.SetStreamSource(0, m_Geoms[i].VB, 0, TG2WGeom.VertexStride[0]);
    Core.Graphics.Device.SetIndices(m_Geoms[i].IB);
    W := m_Nodes[m_Geoms[i].NodeID].TransformRen;
    Core.Graphics.Device.SetTransform(D3DTS_WORLD, W);
    Core.Graphics.Device.SetTransform(D3DTS_VIEW, Core.Graphics.Transforms.V);
    Core.Graphics.Device.SetTransform(D3DTS_PROJECTION, Core.Graphics.Transforms.P);
    Core.Graphics.Device.DrawIndexedPrimitive(
      D3DPT_TRIANGLELIST,
      0,
      0, m_Geoms[i].VCount,
      0, m_Geoms[i].FCount
    );
  end;

  Core.Graphics.RenderStates.DepthBias := 0;
  Core.Graphics.RenderStates.SlopeScaleDepthBias := 0;
  Core.Graphics.RenderStates.SrcBlend := PrevSrcBlend;
  Core.Graphics.RenderStates.DestBlend := PrevDstBlend;
  Core.Graphics.RenderStates.AlphaBlendEnable := False;

  Core.Graphics.RenderStates.ZWriteEnable := False;
  Core.Graphics.Device.SetVertexDeclaration(m_Decl[0]);
  m_Effect.Technique := 'g2LightMap';
  m_Effect.BeginEffect(nil);
  m_Effect.BeginPass(0);
  v := Ambient;//SetValue(0.2, 0.2, 0.2);
  m_Effect.SetValue('g_Ambient', @v, 12);
  v.SetValue(1, 1, -1); v.Normalize;
  m_Effect.SetValue('g_LightDir', @v, 12);
  v.SetValue(1, 1, 1);
  m_Effect.SetValue('g_LightColor', @v, 12);
  m_Effect.SetValue('g_CamPosW', @CamPos, 12);
  m_Effect.SetMatrix('g_LightProj', m_ShadowProjRT.ProjMatrix);
  m_Effect.SetTexture('TexShadowProj', m_ShadowProjRT.Texture);
  for i := 0 to m_GeomCount - 1 do
  if not m_Geoms[i].Skinned
  and Core.Graphics.Transforms.Frustum.BoxInFrustum(m_Geoms[i].AABox) then
  begin
    Core.Graphics.Device.SetStreamSource(0, m_Geoms[i].VB, 0, TG2WGeom.VertexStride[0]);
    Core.Graphics.Device.SetIndices(m_Geoms[i].IB);
    W := m_Nodes[m_Geoms[i].NodeID].TransformRen;
    m_Effect.SetMatrix('g_W', W);
    m_Effect.SetMatrix('g_WVP', W * Core.Graphics.Transforms.VP);
    m_Effect.SetMatrix('g_LightWVP', W * ProjVP);
    for g := 0 to m_Geoms[i].GCount - 1 do
    begin
      m_Effect.SetTexture('TexDiffuse', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexDiffuse).Texture);
      m_Effect.SetTexture('TexNormals', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexNormals).Texture);
      m_Effect.SetTexture('TexSpecular', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexDiffuse).Texture);
      m_Effect.SetTexture('TexLightMap', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexLightMap).Texture);
      m_Effect.CommitChanges;
      Core.Graphics.Device.DrawIndexedPrimitive(
        D3DPT_TRIANGLELIST,
        0,
        m_Geoms[i].Groups[g].VStart, m_Geoms[i].Groups[g].VCount,
        m_Geoms[i].Groups[g].FStart * 3, m_Geoms[i].Groups[g].FCount
      );
    end;
  end;
  m_Effect.EndPass;
  m_Effect.EndEffect;
  Core.Graphics.RenderStates.ZEnable := True;
  Core.Graphics.RenderStates.ZWriteEnable := True;
  Core.Graphics.RenderStates.AlphaBlendEnable := True;
  m_Effect.Technique := 'g2Simple';
  m_Effect.BeginEffect(nil);
  m_Effect.BeginPass(0);
  v := Ambient;
  m_Effect.SetValue('g_Ambient', @v, 12);
  v.SetValue(1, 1, -1); v.Normalize;
  m_Effect.SetValue('g_LightDir', @v, 12);
  v.SetValue(1, 1, 1);
  m_Effect.SetValue('g_LightColor', @v, 12);
  m_Effect.SetValue('g_CamPosW', @CamPos, 12);
  for i := 0 to m_MgrChars.Count - 1 do
  if m_MgrChars[i].Active then
  begin
    c := m_MgrChars[i];
    for j := 0 to c.MeshInst.Mesh.GeomCount - 1 do
    begin
      v.SetValue(1, 1, 1);
      v := v * c.LightAmount;
      m_Effect.SetValue('g_LightColor', @v, 12);
      m_Effect.SetFloat('g_CharAlpha', c.Alpha);
      W.SetScaling(c.Scale);
      W.RotateY(c.Ang);
      W.Translate(c.Pos);
      BBox := c.MeshInst.GeomBBox[j] * W;
      if Core.Graphics.Transforms.Frustum.BoxInFrustum(BBox.AABox) then
      begin
        m_Effect.SetInt('VS_Index', c.MeshInst.Mesh.Geoms[j].MaxWeights);
        if c.MeshInst.Mesh.Geoms[j].Skinned then
        m_Effect.SetMatrixArray('g_SkinPallete', PD3DXMatrix(c.MeshInst.SkinTransforms[j]), c.Mesh.Geoms[j].BoneCount)
        else
        W := c.MeshInst.NodeTransforms[c.Mesh.Geoms[j].NodeID].TransformRen * W;
        WVP := W * Core.Graphics.Transforms.VP;
        m_Effect.SetMatrix('g_W', W);
        m_Effect.SetMatrix('g_WVP', WVP);
        for g := 0 to c.Mesh.Geoms[j].MaterialCount - 1 do
        begin
          m_Effect.SetTexture('TexDiffuse', c.MeshInst.Materials[c.Mesh.Geoms[j].Materials[g]].MapDiffuse.Texture);
          m_Effect.SetTexture('TexNormals', c.MeshInst.Materials[c.Mesh.Geoms[j].Materials[g]].MapNormals.Texture);
          m_Effect.SetTexture('TexSpecular', c.MeshInst.Materials[c.Mesh.Geoms[j].Materials[g]].MapSpecular.Texture);
          m_Effect.CommitChanges;
          c.MeshInst.Mesh.Geoms[j].Mesh.DrawSubset(g);
        end;
      end;
    end;
  end;
  m_Effect.EndPass;
  m_Effect.EndEffect;
  Core.Graphics.RenderStates.AlphaBlendEnable := PrevAlphaBlendEnable;
  Core.Graphics.RenderStates.ZEnable := PrevZEnable;

  //m_Render2D.DrawRect(0, 0, m_ShadowProjRT);
end;

procedure TG2World3D.RenderMP;
type TLightDir = record
    Dir: TG2Vec3;
    Up: TG2Vec3;
  end;
const
  LightDirs: array[0..5] of TLightDir = (
    (Dir:(x: 1; y: 0; z: 0); Up:(x: 0; y: 1; z: 0)),
    (Dir:(x: -1; y: 0; z: 0); Up:(x: 0; y: 1; z: 0)),
    (Dir:(x: 0; y: 1; z: 0); Up:(x: 0; y: 0; z: -1)),
    (Dir:(x: 0; y: -1; z: 0); Up:(x: 0; y: 0; z: 1)),
    (Dir:(x: 0; y: 0; z: 1); Up:(x: 0; y: 1; z: 0)),
    (Dir:(x: 0; y: 0; z: -1); Up:(x: 0; y: 1; z: 0))
  );
var
  CurDecl: Integer;
  procedure SetDecl(const NewDecl: Integer);
  begin
    if NewDecl <> CurDecl then
    begin
      Core.Graphics.Device.SetVertexDeclaration(m_Decl[NewDecl]);
      CurDecl := NewDecl;
    end;
  end;
var
  PrevZEnable: Boolean;
  PrevZWriteEnable: Boolean;
  PrevSrcBlend: DWord;
  PrevDestBlend: DWord;
  PrevDepthBias: Single;
  PrevRT, PrevDS: IDirect3DSurface9;
  i, j, g, l, f: Integer;
  W, WV, WVP: TG2Mat;
  DepthV, DepthP: TG2Mat;
  LightCol: TG2Vec4;
  LightPos, LightDst, LightUp: TG2Vec3;
  LightColor: TG2Vec4;
  LightAtt: TG2Vec2;
  v2: TG2Vec2;
  CamPos: TG2Vec3;
  Frustum: TG2Frustum;
  c: TG2WChar;
  vb: IDirect3DVertexBuffer9;
  ib: IDirect3DIndexBuffer9;
begin
  CamPos := Core.Graphics.Transforms.Vpos;
  CurDecl := -1;
  PrevZEnable := Core.Graphics.RenderStates.ZEnable;
  PrevZWriteEnable := Core.Graphics.RenderStates.ZWriteEnable;
  PrevSrcBlend := Core.Graphics.RenderStates.SrcBlend;
  PrevDestBlend := Core.Graphics.RenderStates.DestBlend;
  PrevDepthBias := Core.Graphics.RenderStates.DepthBias;
  Core.Graphics.RenderStates.ZEnable := True;
  Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  Core.Graphics.Device.SetDepthStencilSurface(m_LightDepthSurface.Surface);

  if m_LightOmniCount > 0 then
  for l := 0 to m_LightOmniCount - 1 do
  begin
    LightPos := m_Nodes[m_LightsOmni[l].NodeID].TransformRen.GetTranslation;
    m_LightsOmni[l].Visible := Core.Graphics.Transforms.Frustum.SphereInFrustum(LightPos, m_LightsOmni[l].AttEnd);
    if m_LightsOmni[l].Visible then
    for f := 0 to 5 do
    begin
      DepthV.SetView(LightPos, LightPos + LightDirs[f].Dir, LightDirs[f].Up);
      DepthP.SetPerspective(HalfPi, 1, 0.01, m_LightsOmni[l].AttEnd + 0.01);
      Frustum.RefV := @DepthV;
      Frustum.RefP := @DepthP;
      Frustum.Update;
      m_LightsOmni[l].VisibleFaces[f] := Core.Graphics.Transforms.Frustum.IntersectFrustum(@Frustum);
      if m_LightsOmni[l].VisibleFaces[f] then
      begin
        Core.Graphics.Device.SetRenderTarget(0, m_LightsOmni[l].Depth.SurfacesRT[TD3DCubemapFaces(f)].Surface);
        Core.Graphics.Device.Clear(0, nil, D3DCLEAR_ZBUFFER or D3DCLEAR_TARGET, $ffffffff, 1, 0);
        m_Effect.Technique := 'g2Depth';
        m_Effect.SetInt('VS_Index', 0);
        if m_ShadowMode = smPCF then
        m_Effect.SetInt('PS_Index', 2)
        else
        m_Effect.SetInt('PS_Index', 3);
        m_Effect.BeginEffect(nil);
        m_Effect.BeginPass(0);
        m_Effect.SetValue('g_LightPosW', @LightPos, 12);
        m_Effect.SetFloat('g_DepthScale', 1.02);
        m_Effect.SetFloat('g_DepthBias', 0);
        m_Effect.SetFloat('g_SlopeScaleDepthBias', 1);
        m_Effect.SetValue('g_CamPosW', @CamPos, 12);
        for i := 0 to m_GeomCount - 1 do
        if Frustum.BoxInFrustum(m_Geoms[i].AABox) then
        begin
          SetDecl(m_Geoms[i].MaxWeights);
          Core.Graphics.Device.SetStreamSource(0, m_Geoms[i].VB, 0, TG2WGeom.VertexStride[m_Geoms[i].MaxWeights]);
          Core.Graphics.Device.SetIndices(m_Geoms[i].IB);
          m_Effect.SetInt('VS_Index', 5 + m_Geoms[i].MaxWeights);
          if m_Geoms[i].Skinned then
          begin
            m_Effect.SetMatrixArray('g_SkinPallete', @m_Geoms[i].SkinTransforms[0], m_Geoms[i].BCount);
            W.SetIdentity;
          end
          else
          W := m_Nodes[m_Geoms[i].NodeID].TransformRen;
          WV := W * DepthV;
          WVP := WV * DepthP;
          m_Effect.SetMatrix('g_W', W);
          m_Effect.SetMatrix('g_WVP', WVP);
          m_Effect.CommitChanges;
          Core.Graphics.Device.DrawIndexedPrimitive(
            D3DPT_TRIANGLELIST,
            0,
            0, m_Geoms[i].VCount,
            0, m_Geoms[i].FCount * 3
          );
        end;
        for i := 0 to m_MgrChars.Count - 1 do
        begin
          c := m_MgrChars[i];
          for j := 0 to c.MeshInst.Mesh.GeomCount - 1 do
          begin
            SetDecl(c.MeshInst.Mesh.Geoms[j].MaxWeights);
            c.MeshInst.Mesh.Geoms[j].Mesh.GetVertexBuffer(vb);
            c.MeshInst.Mesh.Geoms[j].Mesh.GetIndexBuffer(ib);
            Core.Graphics.Device.SetStreamSource(0, vb, 0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumBytesPerVertex);
            Core.Graphics.Device.SetIndices(ib);
            m_Effect.SetInt('VS_Index', 5 + c.MeshInst.Mesh.Geoms[j].MaxWeights);
            if c.MeshInst.Mesh.Geoms[j].Skinned then
            begin
              m_Effect.SetMatrixArray('g_SkinPallete', PD3DXMatrix(c.MeshInst.SkinTransforms[j]), c.Mesh.Geoms[j].BoneCount);
              W.SetIdentity;
            end
            else
            W := c.MeshInst.NodeTransforms[c.Mesh.Geoms[j].NodeID].TransformRen;
            W.Scale(c.Scale);
            W.RotateY(c.Ang);
            W.Translate(c.Pos);
            WV := W * DepthV;
            WVP := WV * DepthP;
            m_Effect.SetMatrix('g_W', W);
            m_Effect.SetMatrix('g_WVP', WVP);
            m_Effect.CommitChanges;
            Core.Graphics.Device.DrawIndexedPrimitive(
              D3DPT_TRIANGLELIST,
              0,
              0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumVertices,
              0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumFaces * 3
            );
            SafeRelease(vb);
            SafeRelease(ib);
          end;
        end;
        m_Effect.EndPass;
        m_Effect.EndEffect;
      end;
    end;
  end;

  for l := 0 to m_LightSpotCount - 1 do
  begin
    LightPos := m_Nodes[m_LightsSpot[l].NodeID].TransformRen.GetTranslation;
    LightDst := G2Vec3(0, -1, 0).Transform4x3(m_Nodes[m_LightsSpot[l].NodeID].TransformRen);
    LightUp := G2Vec3(0, 0, 1).Transform3x3(m_Nodes[m_LightsSpot[l].NodeID].TransformRen);
    DepthV.SetView(LightPos, LightDst, LightUp);
    DepthP.SetPerspective(m_LightsSpot[l].SpotFOV, 1, 0.1, m_LightsSpot[l].AttEnd + 0.1);
    Frustum.RefV := @DepthV;
    Frustum.RefP := @DepthP;
    Frustum.Update;
    m_LightsSpot[l].Visible := Core.Graphics.Transforms.Frustum.IntersectFrustum(@Frustum);
    if m_LightsSpot[l].Visible then
    begin
      Core.Graphics.Device.SetRenderTarget(0, m_LightsSpot[l].Depth.SurfaceRT.Surface);
      Core.Graphics.Device.Clear(0, nil, D3DCLEAR_ZBUFFER or D3DCLEAR_TARGET, $ffffffff, 1, 0);
      m_Effect.Technique := 'g2Depth';
      m_Effect.SetInt('VS_Index', 0);
      if m_ShadowMode = smPCF then
      m_Effect.SetInt('PS_Index', 0)
      else
      m_Effect.SetInt('PS_Index', 1);
      m_Effect.SetFloat('g_DepthScale', 1.02);
      m_Effect.SetFloat('g_DepthBias', 0);
      m_Effect.SetFloat('g_SlopeScaleDepthBias', 1);
      m_Effect.SetValue('g_CamPosW', @CamPos, 12);
      m_Effect.BeginEffect(nil);
      m_Effect.BeginPass(0);
      for i := 0 to m_GeomCount - 1 do
      if Frustum.BoxInFrustum(m_Geoms[i].AABox) then
      begin
        SetDecl(m_Geoms[i].MaxWeights);
        Core.Graphics.Device.SetStreamSource(0, m_Geoms[i].VB, 0, TG2WGeom.VertexStride[m_Geoms[i].MaxWeights]);
        Core.Graphics.Device.SetIndices(m_Geoms[i].IB);
        m_Effect.SetInt('VS_Index', m_Geoms[i].MaxWeights);
        if m_Geoms[i].Skinned then
        begin
          m_Effect.SetMatrixArray('g_SkinPallete', @m_Geoms[i].SkinTransforms[0], m_Geoms[i].BCount);
          W.SetIdentity;
        end
        else
        W := m_Nodes[m_Geoms[i].NodeID].TransformRen;
        WV := W * DepthV;
        WVP := WV * DepthP;
        m_Effect.SetMatrix('g_W', W);
        m_Effect.SetMatrix('g_WVP', WVP);
        m_Effect.CommitChanges;
        Core.Graphics.Device.DrawIndexedPrimitive(
          D3DPT_TRIANGLELIST,
          0,
          0, m_Geoms[i].VCount,
          0, m_Geoms[i].FCount * 3
        );
      end;
      for i := 0 to m_MgrChars.Count - 1 do
      begin
        c := m_MgrChars[i];
        for j := 0 to c.MeshInst.Mesh.GeomCount - 1 do
        begin
          SetDecl(c.MeshInst.Mesh.Geoms[j].MaxWeights);
          c.MeshInst.Mesh.Geoms[j].Mesh.GetVertexBuffer(vb);
          c.MeshInst.Mesh.Geoms[j].Mesh.GetIndexBuffer(ib);
          Core.Graphics.Device.SetStreamSource(0, vb, 0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumBytesPerVertex);
          Core.Graphics.Device.SetIndices(ib);
          m_Effect.SetInt('VS_Index', c.MeshInst.Mesh.Geoms[j].MaxWeights);
          if c.MeshInst.Mesh.Geoms[j].Skinned then
          begin
            m_Effect.SetMatrixArray('g_SkinPallete', PD3DXMatrix(c.MeshInst.SkinTransforms[j]), c.Mesh.Geoms[j].BoneCount);
            W.SetIdentity;
          end
          else
          W := c.MeshInst.NodeTransforms[c.Mesh.Geoms[j].NodeID].TransformRen;
          W.Scale(c.Scale);
          W.RotateY(c.Ang);
          W.Translate(c.Pos);
          WV := W * DepthV;
          WVP := WV * DepthP;
          m_Effect.SetMatrix('g_W', W);
          m_Effect.SetMatrix('g_WVP', WVP);
          m_Effect.CommitChanges;
          Core.Graphics.Device.DrawIndexedPrimitive(
            D3DPT_TRIANGLELIST,
            0,
            0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumVertices,
            0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumFaces * 3
          );
          SafeRelease(vb);
          SafeRelease(ib);
        end;
      end;
      m_Effect.EndPass;
      m_Effect.EndEffect;

      if m_ShadowMode = smVSM then
      begin
        Core.Graphics.RenderStates.ZEnable := False;
        m_Effect.Technique := 'g2Blur';
        m_Effect.SetFloat('g_ShadowMapStep', 2 / m_ShadowMapSize);
        m_Effect.SetTexture('TexBlur', m_LightsSpot[l].Depth.Texture);
        v2.SetValue(1, 0);
        m_Effect.SetValue('g_BlurOffset', @v2, 8);
        m_Effect.BeginEffect(nil);
        m_Effect.BeginPass(0);

        Core.Graphics.Device.SetRenderTarget(0, m_BlurRT.SurfaceRT.Surface);
        m_Render2D.DrawQuadRaw(
          G2Vec2(0, 0), G2Vec2(m_ShadowMapSize - 1, 0),
          G2Vec2(0, m_ShadowMapSize - 1), G2Vec2(m_ShadowMapSize - 1, m_ShadowMapSize - 1),
          $ffffffff, $ffffffff, $ffffffff, $ffffffff,
          G2Vec2(0, 0), G2Vec2(1, 0),
          G2Vec2(0, 1), G2Vec2(1, 1)
        );

        m_Effect.SetTexture('TexBlur', m_BlurRT.Texture);
        v2.SetValue(0, 1);
        m_Effect.SetValue('g_BlurOffset', @v2, 8);
        Core.Graphics.Device.SetRenderTarget(0, m_LightsSpot[l].Depth.SurfaceRT.Surface);
        m_Effect.CommitChanges;
        m_Render2D.DrawQuadRaw(
          G2Vec2(0, 0), G2Vec2(m_ShadowMapSize - 1, 0),
          G2Vec2(0, m_ShadowMapSize - 1), G2Vec2(m_ShadowMapSize - 1, m_ShadowMapSize - 1),
          $ffffffff, $ffffffff, $ffffffff, $ffffffff,
          G2Vec2(0, 0), G2Vec2(1, 0),
          G2Vec2(0, 1), G2Vec2(1, 1)
        );

        m_Effect.EndPass;
        m_Effect.EndEffect;
        Core.Graphics.RenderStates.ZEnable := True;
        CurDecl := -1;
      end;
    end;
  end;

  Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  SafeRelease(PrevDS);
  SafeRelease(PrevRT);
  Core.Graphics.RenderStates.ZWriteEnable := True;
  Core.Graphics.RenderStates.DepthBias := m_DepthBias;
  Core.Graphics.RenderStates.SlopeScaleDepthBias := m_SlopeBias;

  m_Effect.Technique := 'g2Ambience';
  m_Effect.SetInt('PS_Index', 0);
  m_Effect.SetInt('VS_Index', 0);
  m_Effect.BeginEffect(nil);
  m_Effect.BeginPass(0);
  for i := 0 to m_GeomCount - 1 do
  if Core.Graphics.Transforms.Frustum.BoxInFrustum(m_Geoms[i].AABox) then
  begin
    SetDecl(m_Geoms[i].MaxWeights);
    Core.Graphics.Device.SetStreamSource(0, m_Geoms[i].VB, 0, TG2WGeom.VertexStride[m_Geoms[i].MaxWeights]);
    Core.Graphics.Device.SetIndices(m_Geoms[i].IB);
    m_Effect.SetInt('VS_Index', m_Geoms[i].MaxWeights);
    if m_Geoms[i].Skinned then
    begin
      m_Effect.SetMatrixArray('g_SkinPallete', @m_Geoms[i].SkinTransforms[0], m_Geoms[i].BCount);
      W.SetIdentity;
    end
    else
    W := m_Nodes[m_Geoms[i].NodeID].TransformRen;
    m_Effect.SetMatrix('g_WVP', W * Core.Graphics.Transforms.VP);
    for g := 0 to m_Geoms[i].GCount - 1 do
    begin
      m_Effect.SetTexture('TexDiffuse', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexDiffuse).Texture);
      m_Effect.SetTexture('TexLightMap', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexLightMap).Texture);
      m_Effect.CommitChanges;
      Core.Graphics.Device.DrawIndexedPrimitive(
        D3DPT_TRIANGLELIST,
        0,
        m_Geoms[i].Groups[g].VStart, m_Geoms[i].Groups[g].VCount,
        m_Geoms[i].Groups[g].FStart * 3, m_Geoms[i].Groups[g].FCount
      );
    end;
  end;
  for i := 0 to m_MgrChars.Count - 1 do
  begin
    c := m_MgrChars[i];
    for j := 0 to c.MeshInst.Mesh.GeomCount - 1 do
    begin
      SetDecl(c.MeshInst.Mesh.Geoms[j].MaxWeights);
      c.MeshInst.Mesh.Geoms[j].Mesh.GetVertexBuffer(vb);
      c.MeshInst.Mesh.Geoms[j].Mesh.GetIndexBuffer(ib);
      Core.Graphics.Device.SetStreamSource(0, vb, 0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumBytesPerVertex);
      Core.Graphics.Device.SetIndices(ib);
      m_Effect.SetInt('VS_Index', c.MeshInst.Mesh.Geoms[j].MaxWeights);
      if c.MeshInst.Mesh.Geoms[j].Skinned then
      begin
        m_Effect.SetMatrixArray('g_SkinPallete', PD3DXMatrix(c.MeshInst.SkinTransforms[j]), c.Mesh.Geoms[j].BoneCount);
        W.SetIdentity;
      end
      else
      W := c.MeshInst.NodeTransforms[c.Mesh.Geoms[j].NodeID].TransformRen;
      W.Scale(c.Scale);
      W.RotateY(c.Ang);
      W.Translate(c.Pos);
      m_Effect.SetMatrix('g_WVP', W * Core.Graphics.Transforms.VP);
      m_Effect.SetTexture('TexDiffuse', FindTexture('NULL').Texture);
      m_Effect.SetTexture('TexLightMap', FindTexture('NULL').Texture);
      m_Effect.CommitChanges;
      c.MeshInst.Mesh.Geoms[j].Mesh.DrawSubset(0);
      Core.Graphics.Device.DrawIndexedPrimitive(
        D3DPT_TRIANGLELIST,
        0,
        0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumVertices,
        0, c.MeshInst.Mesh.Geoms[j].Mesh.GetNumFaces
      );
      SafeRelease(vb);
      SafeRelease(ib);
    end;
  end;
  m_Effect.EndPass;
  m_Effect.EndEffect;
  Core.Graphics.RenderStates.ZWriteEnable := False;
  Core.Graphics.RenderStates.SrcBlend := D3DBLEND_ONE;
  Core.Graphics.RenderStates.DestBlend := D3DBLEND_ONE;
  Core.Graphics.RenderStates.DepthBias := 0;
  Core.Graphics.RenderStates.SlopeScaleDepthBias := 0;

  m_Effect.Technique := 'g2ShadowMap';
  if m_ShadowMode = smPCF then
  m_Effect.SetInt('PS_Index', 5 + Byte(m_ShadowCubeSmapleCount))
  else
  m_Effect.SetInt('PS_Index', 9);
  m_Effect.SetInt('VS_Index', 0);
  m_Effect.SetValue('g_CamPosW', @CamPos, 12);
  m_Effect.SetVectorArray('g_CubeSampleMap', @m_CubeSampleMap[0], 16);
  m_Effect.BeginEffect(nil);
  m_Effect.BeginPass(0);
  Core.Graphics.RenderStates.ScissorTestEnable := True;
  for i := 0 to m_GeomCount - 1 do
  if Core.Graphics.Transforms.Frustum.BoxInFrustum(m_Geoms[i].AABox) then
  begin
    SetDecl(m_Geoms[i].MaxWeights);
    Core.Graphics.Device.SetStreamSource(0, m_Geoms[i].VB, 0, TG2WGeom.VertexStride[m_Geoms[i].MaxWeights]);
    Core.Graphics.Device.SetIndices(m_Geoms[i].IB);
    m_Effect.SetInt('VS_Index', 5 + m_Geoms[i].MaxWeights);
    if m_Geoms[i].Skinned then
    begin
      m_Effect.SetMatrixArray('g_SkinPallete', @m_Geoms[i].SkinTransforms[0], m_Geoms[i].BCount);
      W.SetIdentity;
    end
    else
    W := m_Nodes[m_Geoms[i].NodeID].TransformRen;
    WV := W * Core.Graphics.Transforms.V;
    WVP := WV * Core.Graphics.Transforms.P;
    m_Effect.SetMatrix('g_W', W);
    m_Effect.SetMatrix('g_WVP', WVP);
    for g := 0 to m_Geoms[i].GCount - 1 do
    begin
      m_Effect.SetTexture('TexDiffuse', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexDiffuse).Texture);
      m_Effect.SetTexture('TexNormals', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexNormals).Texture);
      for l := 0 to m_LightOmniCount - 1 do
      begin
        if m_LightsOmni[l].Visible then
        begin
          LightPos := m_Nodes[m_LightsOmni[l].NodeID].TransformRen.GetTranslation;
          LightColor := m_LightsOmni[l].Color;
          LightAtt.SetValue(Sqr(m_LightsOmni[l].AttStart), Sqr(m_LightsOmni[l].AttEnd));
          m_Effect.SetTexture('TexDepth0', m_LightsOmni[l].Depth.Texture);
          m_Effect.SetValue('g_LightPosW', @LightPos, 12);
          m_Effect.SetValue('g_LightColor', @LightColor, 16);
          m_Effect.SetValue('g_LightAtt', @LightAtt, 8);
          m_Effect.CommitChanges;
          Core.Graphics.Device.DrawIndexedPrimitive(
            D3DPT_TRIANGLELIST,
            0,
            m_Geoms[i].Groups[g].VStart, m_Geoms[i].Groups[g].VCount,
            m_Geoms[i].Groups[g].FStart * 3, m_Geoms[i].Groups[g].FCount
          );
        end;
      end;
    end;
  end;
  for i := 0 to m_MgrChars.Count - 1 do
  begin
    c := m_MgrChars[i];
    for j := 0 to c.MeshInst.Mesh.GeomCount - 1 do
    begin
      m_Effect.SetInt('VS_Index', 5 + c.MeshInst.Mesh.Geoms[j].MaxWeights);
      if c.MeshInst.Mesh.Geoms[j].Skinned then
      begin
        m_Effect.SetMatrixArray('g_SkinPallete', PD3DXMatrix(c.MeshInst.SkinTransforms[j]), c.Mesh.Geoms[j].BoneCount);
        W.SetIdentity;
      end
      else
      W := c.MeshInst.NodeTransforms[c.Mesh.Geoms[j].NodeID].TransformRen;
      W.Scale(c.Scale);
      W.RotateY(c.Ang);
      W.Translate(c.Pos);
      WV := W * Core.Graphics.Transforms.V;
      WVP := WV * Core.Graphics.Transforms.P;
      m_Effect.SetMatrix('g_W', W);
      m_Effect.SetMatrix('g_WVP', WVP);
      for g := 0 to c.Mesh.Geoms[j].MaterialCount - 1 do
      begin
        m_Effect.SetTexture('TexDiffuse', c.MeshInst.Materials[c.Mesh.Geoms[j].Materials[g]].MapDiffuse.Texture);
        m_Effect.SetTexture('TexNormals', c.MeshInst.Materials[c.Mesh.Geoms[j].Materials[g]].MapNormals.Texture);
        for l := 0 to m_LightOmniCount - 1 do
        begin
          if m_LightsOmni[l].Visible then
          begin
            LightPos := m_Nodes[m_LightsOmni[l].NodeID].TransformRen.GetTranslation;
            LightColor := m_LightsOmni[l].Color;
            LightAtt.SetValue(Sqr(m_LightsOmni[l].AttStart), Sqr(m_LightsOmni[l].AttEnd));
            m_Effect.SetTexture('TexDepth0', m_LightsOmni[l].Depth.Texture);
            m_Effect.SetValue('g_LightPosW', @LightPos, 12);
            m_Effect.SetValue('g_LightColor', @LightColor, 16);
            m_Effect.SetValue('g_LightAtt', @LightAtt, 8);
            m_Effect.CommitChanges;
            c.MeshInst.Mesh.Geoms[j].Mesh.DrawSubset(g);
          end;
        end;
      end;
    end;
  end;
  m_Effect.EndPass;
  m_Effect.EndEffect;

  m_Effect.Technique := 'g2ShadowMap';
  if m_ShadowMode = smPCF then
  m_Effect.SetInt('PS_Index', Byte(m_ShadowProjSmapleCount))
  else
  m_Effect.SetInt('PS_Index', 4);
  m_Effect.SetInt('VS_Index', 0);
  m_Effect.SetValue('g_CamPosW', @CamPos, 12);
  m_Effect.SetFloat('g_ShadowMapStep', m_ShadowProjStep);
  m_Effect.SetVectorArray('g_ProjSampleMap', @m_ProjSampleMap, 16);
  m_Effect.BeginEffect(nil);
  m_Effect.BeginPass(0);
  for l := 0 to m_LightSpotCount - 1 do
  if m_LightsSpot[l].Visible then
  begin
    LightPos := m_Nodes[m_LightsSpot[l].NodeID].TransformRen.GetTranslation;
    LightDst := G2Vec3(0, -1, 0).Transform4x3(m_Nodes[m_LightsSpot[l].NodeID].TransformRen);
    LightUp := G2Vec3(0, 0, 1).Transform3x3(m_Nodes[m_LightsSpot[l].NodeID].TransformRen);
    DepthV.SetView(LightPos, LightDst, LightUp);
    LightDst := (LightDst - LightPos).Normalized;
    DepthP.SetPerspective(m_LightsSpot[l].SpotFOV, 1, 0.1, m_LightsSpot[l].AttEnd + 0.1);
    m_Effect.SetTexture('TexDepth', m_LightsSpot[l].Depth.Texture);
    m_Effect.SetValue('g_LightPosW', @LightPos, 12);
    m_Effect.SetValue('g_LightDir', @LightDst, 12);
    v2.SetValue(m_LightsSpot[l].AttStart, m_LightsSpot[l].AttEnd);
    m_Effect.SetValue('g_LightAtt', @v2, 8);
    m_Effect.SetFloat('g_SpotInner', m_LightsSpot[l].SpotInner);
    m_Effect.SetFloat('g_SpotOutter', m_LightsSpot[l].SpotOutter);
    for i := 0 to m_GeomCount - 1 do
    if Core.Graphics.Transforms.Frustum.BoxInFrustum(m_Geoms[i].AABox) then
    begin
      SetDecl(m_Geoms[i].MaxWeights);
      Core.Graphics.Device.SetStreamSource(0, m_Geoms[i].VB, 0, TG2WGeom.VertexStride[m_Geoms[i].MaxWeights]);
      Core.Graphics.Device.SetIndices(m_Geoms[i].IB);
      m_Effect.SetInt('VS_Index', m_Geoms[i].MaxWeights);
      if m_Geoms[i].Skinned then
      begin
        m_Effect.SetMatrixArray('g_SkinPallete', @m_Geoms[i].SkinTransforms[0], m_Geoms[i].BCount);
        W.SetIdentity;
      end
      else
      W := m_Nodes[m_Geoms[i].NodeID].TransformRen;
      m_Effect.SetMatrix('g_W', W);
      m_Effect.SetMatrix('g_WVP', W * Core.Graphics.Transforms.VP);
      m_Effect.SetMatrix('g_LightWVP', W * DepthV * DepthP);
      m_Effect.SetMatrix('g_LightProj', m_LightsSpot[l].Depth.ProjMatrix);
      for g := 0 to m_Geoms[i].GCount - 1 do
      begin
        m_Effect.SetTexture('TexDiffuse', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexDiffuse).Texture);
        m_Effect.SetTexture('TexNormals', FindTexture(m_Mats[m_Geoms[i].Groups[g].MatID].TexNormals).Texture);
        m_Effect.CommitChanges;
        Core.Graphics.Device.DrawIndexedPrimitive(
          D3DPT_TRIANGLELIST,
          0,
          m_Geoms[i].Groups[g].VStart, m_Geoms[i].Groups[g].VCount,
          m_Geoms[i].Groups[g].FStart * 3, m_Geoms[i].Groups[g].FCount
        );
      end;
    end;
    for i := 0 to m_MgrChars.Count - 1 do
    begin
      c := m_MgrChars[i];
      for j := 0 to c.MeshInst.Mesh.GeomCount - 1 do
      begin
        m_Effect.SetInt('VS_Index', c.MeshInst.Mesh.Geoms[j].MaxWeights);
        if c.MeshInst.Mesh.Geoms[j].Skinned then
        begin
          m_Effect.SetMatrixArray('g_SkinPallete', PD3DXMatrix(c.MeshInst.SkinTransforms[j]), c.Mesh.Geoms[j].BoneCount);
          W.SetIdentity;
        end
        else
        W := c.MeshInst.NodeTransforms[c.Mesh.Geoms[j].NodeID].TransformRen;
        W.Scale(c.Scale);
        W.RotateY(c.Ang);
        W.Translate(c.Pos);
        m_Effect.SetMatrix('g_W', W);
        m_Effect.SetMatrix('g_WVP', W * Core.Graphics.Transforms.VP);
        m_Effect.SetMatrix('g_LightWVP', W * DepthV * DepthP);
        m_Effect.SetMatrix('g_LightProj', m_LightsSpot[l].Depth.ProjMatrix);
        for g := 0 to c.Mesh.Geoms[j].MaterialCount - 1 do
        begin
          m_Effect.SetTexture('TexDiffuse', c.MeshInst.Materials[c.Mesh.Geoms[j].Materials[g]].MapDiffuse.Texture);
          m_Effect.SetTexture('TexNormals', c.MeshInst.Materials[c.Mesh.Geoms[j].Materials[g]].MapNormals.Texture);
          m_Effect.CommitChanges;
          c.Mesh.Geoms[j].Mesh.DrawSubset(g);
        end;
      end;
    end;
  end;
  Core.Graphics.RenderStates.ScissorTestEnable := False;
  m_Effect.EndPass;
  m_Effect.EndEffect;

  Core.Graphics.RenderStates.ZEnable := PrevZEnable;
  Core.Graphics.RenderStates.ZWriteEnable := PrevZWriteEnable;
  Core.Graphics.RenderStates.SrcBlend := PrevSrcBlend;
  Core.Graphics.RenderStates.DestBlend := PrevDestBlend;
  Core.Graphics.RenderStates.DepthBias := PrevDepthBias;
end;

procedure TG2World3D.Collide(var c, v: TG2Vec3; var Grounded: Boolean; const r: Single; const StepHeight: Single = 0);
var
  i, j, k, l: Integer;
  t, n, cv: TG2Vec3;
  TriU, TriV, d: Single;
  InSegment: Boolean;
  AABox: TG2AABox;
const
  STEP_SPEED = 0.1;
begin
  if m_CurColID >= High(DWord) then
  begin
    m_CurColID := 0;
    for i := 0 to m_ColliderCount - 1 do
    for j := 0 to High(m_Colliders[i].Faces) do
    m_Colliders[i].Faces[j].ColID := 0;
  end;
  Inc(m_CurColID);
  Grounded := False;
  cv.SetValue(0, 0, 0);
  AABox.SetValue(c - G2Vec3(r, r + StepHeight, r), c + r);
  for i := 0 to m_ColliderCount - 1 do
  begin
    if AABox.Intersect(m_Colliders[i].AABox) then
    for j := 0 to High(m_Colliders[i].Faces) do
    begin
      if m_Colliders[i].Faces[j].Plane.N.Dot(G2Vec3(0, 1, 0)) > 0.7 then
      begin
        if G2Ray(c, G2Vec3(0, -1, 0)).IntersectTri(
          m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[0]],
          m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[1]],
          m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[2]],
          TriU, TriV, d
        ) then
        begin
          if d < r then
          begin
            c.y := c.y - d + (r + StepHeight * STEP_SPEED);
            m_Colliders[i].Faces[j].ColID := m_CurColID;
            v.y := 0;
          end
          else if d < r + StepHeight then
          begin
            if d + StepHeight * STEP_SPEED < r + StepHeight then
            c.y := c.y + StepHeight * STEP_SPEED
            else
            c.y := c.y - d + (r + StepHeight);
            m_Colliders[i].Faces[j].ColID := m_CurColID;
            v.y := 0;
          end;
          if (d < r + StepHeight) or ((d < r + StepHeight * 1.1) and (v.y < 0)) then
          Grounded := True;
        end;
      end;
      if (m_Colliders[i].Faces[j].ColID <> m_CurColID)
      and c.InTriangle(
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[0]],
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[1]],
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[2]]
      ) then
      begin
        m_Colliders[i].Faces[j].ColID := m_CurColID;
        d := Abs(m_Colliders[i].Faces[j].Plane.DistanceToPoint(c));
        if d < r then
        begin
          t := m_Colliders[i].Faces[j].Plane.Project(c);
          n := (c - t).Normalized;
          c := t + n * r;
          cv := cv + n;
        end;
      end;
    end;
  end;
  for i := 0 to m_ColliderCount - 1 do
  begin
    if AABox.Intersect(m_Colliders[i].AABox) then
    for j := 0 to High(m_Colliders[i].Faces) do
    if m_Colliders[i].Faces[j].ColID <> m_CurColID then
    for k := 0 to 2 do
    begin
      d := G2Vec3ToLine(
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Edges[k].Indices[0]],
        m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Edges[k].Indices[1]],
        c, t, InSegment
      );
      if InSegment and (d < r) then
      begin
        n := (c - t).Normalized;
        c := t + (n * r);
        cv := cv + n;
        m_Colliders[i].Faces[j].ColID := m_CurColID;
        Break;
      end;
    end;
  end;
  for i := 0 to m_ColliderCount - 1 do
  begin
    if AABox.Intersect(m_Colliders[i].AABox) then
    for j := 0 to High(m_Colliders[i].Faces) do
    if m_Colliders[i].Faces[j].ColID <> m_CurColID then
    begin
      for l := 0 to 2 do
      begin
        n := c - m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[l]];
        d := n.Len;
        if d < r then
        begin
          n.Normalize;
          c := m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[l]] + (n * r);
          cv := cv + n;
          Break;
        end;
      end;
    end;
  end;
  cv.Normalize;
  v := v - cv * cv.Dot(v);
end;

function TG2World3D.RayCollide(const r: TG2Ray; const Len: Single; var d: Single; var n: TG2Vec3): Boolean;
  var AABox: TG2AABox;
  var i, j: Integer;
  var u, v, nd: Single;
begin
  AABox.SetValue(r.Origin, r.Origin);
  AABox := AABox + (r.Origin + r.Dir * Len);
  d := Len + 1;
  for i := 0 to m_ColliderCount - 1 do
  if AABox.Intersect(m_Colliders[i].AABox)
  and r.IntersectAABox(m_Colliders[i].AABox) then
  for j := 0 to High(m_Colliders[i].Faces) do
  begin
    if r.IntersectTri(
      m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[0]],
      m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[1]],
      m_Colliders[i].VerticesT[m_Colliders[i].Faces[j].Face[2]],
      u, v, nd
    ) then
    begin
      if nd < d then
      begin
        d := nd;
        n := m_Colliders[i].Faces[j].Plane.N;
      end;
    end;
  end;
  Result := d <= Len;
end;

function TG2World3D.FindNode(const Name: AnsiString): PG2WNode;
var
  i: Integer;
begin
  for i := 0 to m_NodeCount - 1 do
  if m_Nodes[i].Name = Name then
  begin
    Result := @m_Nodes[i];
    Exit;
  end;
  Result := nil;
end;

function TG2World3D.FindGeom(const Name: AnsiString): PG2WGeom;
var
  i: Integer;
begin
  for i := 0 to m_GeomCount - 1 do
  if m_Nodes[m_Geoms[i].NodeID].Name = Name then
  begin
    Result := @m_Geoms[i];
    Exit;
  end;
  Result := nil;
end;

function TG2World3D.FindTexture(const Name: AnsiString): TG2Texture2D;
  var Dir, n, ext: AnsiString;
begin
  Result := TG2Texture2D(m_MgrTextures.FindTexture(Name));
  if Result = nil then
  begin
    if (Length(m_TextureDir) > 0)
    and (m_TextureDir[Length(m_TextureDir)] <> '\') then
    Dir := m_TextureDir + '\'
    else
    Dir := m_TextureDir;
    if FileExists(Dir + Name) then
    Result := m_MgrTextures.CreateTexture2DFromFile(Name, Dir + Name, 16, D3DFMT_DXT5);
    if Result = nil then
    begin
      n := ExtractFileName(Name);
      ext := ExtractFileExt(n);
      Delete(n, Length(n) - Length(ext) + 1, Length(ext));
      Result := m_MgrTextures.CreateTexture2DFromPack(Name, m_TextureDir, n, 16, D3DFMT_DXT5);
      if Result = nil then
      begin
        if Length(Name) > 0 then
        G2WriteLogTimed('(W) Texture missing: ' + Name, 'World3D');
        Result := TG2Texture2D(m_MgrTextures.FindTexture('NULL'));
      end;
    end;
  end;
end;

procedure TG2World3D.Load(const FileName: String);
type
  TG2GeomVertex = packed record
    Position: TG2Vec3;
    Tangent: TG2Vec3;
    Binormal: TG2Vec3;
    Normal: TG2Vec3;
    TexCoords0: TG2Vec2;
    TexCoords1: TG2Vec2;
  end;
  PG2GeomVertex = ^TG2GeomVertex;
  TG2GeomVertexArr = array[Word] of TG2GeomVertex;
  PG2GeomVertexArr = ^TG2GeomVertexArr;
var
  i, j, w: Integer;
  Loader: TG2MeshLoaderG2M;
  MeshData: TG2MeshData;
  Vertices: PG2GeomVertexArr;
  Vertex: PG2GeomVertex;
  Indices: PWordArray;
  Attribs: PDWordArray;
  AttribTable: array of TD3DXAttributeRange;
  Mesh: ID3DXMesh;
  Adj: array of DWord;
  Ptr1, Ptr2: Pointer;
  PtrBIndices: PFloatArray;
  PtrBWeights: PFloatArray;
  DepthFormat: TD3DFormat;
  BoneVertices: array of TG2AABox;
  BoneVerticesCount: array of Integer;
begin
  Loader := TG2MeshLoaderG2M.Create;
  try
    if Loader.CanLoadFile(FileName) then
    begin
      Loader.LoadFile(FileName);
      Loader.ExportMesh(Core.Graphics.Device, @MeshData);
      m_NodeCount := MeshData.NodeCount;
      SetLength(m_Nodes, m_NodeCount);
      for i := 0 to m_NodeCount - 1 do
      begin
        m_Nodes[i].OwnerID := MeshData.Nodes[i].OwnerID;
        m_Nodes[i].Name := MeshData.Nodes[i].Name;
        m_Nodes[i].TransformDef := MeshData.Nodes[i].Transform;
        m_Nodes[i].TransformCur := m_Nodes[i].TransformDef;
        m_Nodes[i].TransformRen := m_Nodes[i].TransformDef;
        if m_Nodes[i].OwnerID > -1 then
        begin
          SetLength(m_Nodes[m_Nodes[i].OwnerID].Slaves, Length(m_Nodes[m_Nodes[i].OwnerID].Slaves) + 1);
          m_Nodes[m_Nodes[i].OwnerID].Slaves[High(m_Nodes[m_Nodes[i].OwnerID].Slaves)] := i;
        end
        else
        begin
          SetLength(m_RootNodes, Length(m_RootNodes) + 1);
          m_RootNodes[High(m_RootNodes)] := i;
        end;
      end;
      m_GeomCount := MeshData.GeomCount;
      SetLength(m_Geoms, m_GeomCount);
      for i := 0 to m_GeomCount - 1 do
      begin
        m_Geoms[i].Skinned := MeshData.Geoms[i].SkinID > -1;
        m_Geoms[i].NodeID := MeshData.Geoms[i].NodeID;
        m_Geoms[i].VCount := MeshData.Geoms[i].VCount;
        m_Geoms[i].FCount := MeshData.Geoms[i].FCount;
        if m_Geoms[i].Skinned then
        begin
          m_Geoms[i].MaxWeights := MeshData.Skins[MeshData.Geoms[i].SkinID].MaxWeights;
          m_Geoms[i].BCount := MeshData.Skins[MeshData.Geoms[i].SkinID].BoneCount;
          SetLength(m_Geoms[i].Bones, m_Geoms[i].BCount);
          SetLength(m_Geoms[i].SkinTransforms, m_Geoms[i].BCount);
          for j := 0 to m_Geoms[i].BCount - 1 do
          begin
            m_Geoms[i].Bones[j].NodeID := MeshData.Skins[MeshData.Geoms[i].SkinID].Bones[j].NodeID;
            m_Geoms[i].Bones[j].Bind := MeshData.Skins[MeshData.Geoms[i].SkinID].Bones[j].Bind;
          end;
        end
        else
        begin
          m_Geoms[i].MaxWeights := 0;
          m_Geoms[i].BCount := 0;
        end;
        D3DXCreateMesh(
          m_Geoms[i].FCount,
          m_Geoms[i].VCount,
          D3DXMESH_SYSTEMMEM,
          @m_FVFDecl[m_Geoms[i].MaxWeights][0],
          Core.Graphics.Device,
          Mesh
        );
        m_Geoms[i].AABox.MinV := MeshData.Geoms[i].Vertices[0].Position;
        m_Geoms[i].AABox.MaxV := m_Geoms[i].AABox.MinV;
        if m_Geoms[i].Skinned then
        begin
          SetLength(BoneVertices, m_Geoms[i].BCount);
          SetLength(BoneVerticesCount, m_Geoms[i].BCount);
          ZeroMemory(@BoneVerticesCount[0], m_Geoms[i].BCount * 4);
          Mesh.LockVertexBuffer(
            0, Ptr1
          );
          for j := 0 to m_Geoms[i].VCount - 1 do
          begin
            Vertex := PG2GeomVertex(DWord(Ptr1) + j * TG2WGeom.VertexStride[m_Geoms[i].MaxWeights]);
            PtrBIndices := PFloatArray(DWord(Vertex) + TG2WGeom.VertexStride[0]);
            PtrBWeights := PFloatArray(DWord(Vertex) + TG2WGeom.VertexStride[0] - 8 + 4 * m_Geoms[i].MaxWeights);
            Vertex^.Position := MeshData.Geoms[i].Vertices[j].Position;
            Vertex^.Tangent := MeshData.Geoms[i].Vertices[j].Tangent;
            Vertex^.Binormal := MeshData.Geoms[i].Vertices[j].Binormal;
            Vertex^.Normal := MeshData.Geoms[i].Vertices[j].Normal;
            if MeshData.Geoms[i].TCount > 0 then
            Vertex^.TexCoords0 := MeshData.Geoms[i].Vertices[j].TexCoords[0]
            else
            Vertex^.TexCoords0.SetValue(0, 0);
            ZeroMemory(PtrBIndices, 4 * m_Geoms[i].MaxWeights);
            for w := 0 to MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].WeightCount - 1 do
            begin
              PtrBIndices^[w] := MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[w].BoneID;
              if BoneVerticesCount[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[w].BoneID] = 0 then
              begin
                BoneVertices[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[w].BoneID].MinV := MeshData.Geoms[i].Vertices[j].Position;
                BoneVertices[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[w].BoneID].MaxV := MeshData.Geoms[i].Vertices[j].Position;
              end
              else
              BoneVertices[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[w].BoneID] := (
                BoneVertices[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[w].BoneID] + MeshData.Geoms[i].Vertices[j].Position
              );
              Inc(BoneVerticesCount[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[w].BoneID]);
            end;
            if m_Geoms[i].MaxWeights > 1 then
            begin
              ZeroMemory(PtrBWeights, 4 * m_Geoms[i].MaxWeights);
              for w := 0 to MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].WeightCount - 1 do
              PtrBWeights^[w] := MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[w].Weight;
            end;
            m_Geoms[i].AABox := m_Geoms[i].AABox + MeshData.Geoms[i].Vertices[j].Position;
          end;
          Mesh.UnlockVertexBuffer;
          for j := 0 to m_Geoms[i].BCount - 1 do
          begin
            m_Geoms[i].Bones[j].VCount := BoneVerticesCount[j];
            if BoneVerticesCount[j] > 0 then
            begin
              m_Geoms[i].Bones[j].BBox.C := (BoneVertices[j].MaxV + BoneVertices[j].MinV) * 0.5;
              m_Geoms[i].Bones[j].BBox.vx.SetValue((BoneVertices[j].MaxV.x - BoneVertices[j].MinV.x) * 0.5, 0, 0);
              m_Geoms[i].Bones[j].BBox.vy.SetValue(0, (BoneVertices[j].MaxV.y - BoneVertices[j].MinV.y) * 0.5, 0);
              m_Geoms[i].Bones[j].BBox.vz.SetValue(0, 0, (BoneVertices[j].MaxV.z - BoneVertices[j].MinV.z) * 0.5);
            end;
          end;
        end
        else
        begin
          Mesh.LockVertexBuffer(
            0, Pointer(Vertices)
          );
          for j := 0 to m_Geoms[i].VCount - 1 do
          begin
            Vertices^[j].Position := MeshData.Geoms[i].Vertices[j].Position;
            Vertices^[j].Tangent := MeshData.Geoms[i].Vertices[j].Tangent;
            Vertices^[j].Binormal := MeshData.Geoms[i].Vertices[j].Binormal;
            Vertices^[j].Normal := MeshData.Geoms[i].Vertices[j].Normal;
            if MeshData.Geoms[i].TCount > 0 then
            Vertices^[j].TexCoords0 := MeshData.Geoms[i].Vertices[j].TexCoords[0]
            else
            Vertices^[j].TexCoords0.SetValue(0, 0);
            if MeshData.Geoms[i].TCount > 1 then
            Vertices^[j].TexCoords1 := MeshData.Geoms[i].Vertices[j].TexCoords[1]
            else
            Vertices^[j].TexCoords1.SetValue(0, 0);
            m_Geoms[i].AABox := m_Geoms[i].AABox + MeshData.Geoms[i].Vertices[j].Position;
          end;
          Mesh.UnlockVertexBuffer;
        end;
        m_Geoms[i].BBox.C := (m_Geoms[i].AABox.MaxV + m_Geoms[i].AABox.MinV) * 0.5;
        m_Geoms[i].BBox.vx.SetValue((m_Geoms[i].AABox.MaxV.x - m_Geoms[i].AABox.MinV.x) * 0.5, 0, 0);
        m_Geoms[i].BBox.vy.SetValue(0, (m_Geoms[i].AABox.MaxV.y - m_Geoms[i].AABox.MinV.y) * 0.5, 0);
        m_Geoms[i].BBox.vz.SetValue(0, 0, (m_Geoms[i].AABox.MaxV.z - m_Geoms[i].AABox.MinV.z) * 0.5);
        Mesh.LockIndexBuffer(
          0, Pointer(Indices)
        );
        for j := 0 to m_Geoms[i].FCount - 1 do
        begin
          Indices^[j * 3 + 0] := MeshData.Geoms[i].Faces[j].Indices[0];
          Indices^[j * 3 + 1] := MeshData.Geoms[i].Faces[j].Indices[1];
          Indices^[j * 3 + 2] := MeshData.Geoms[i].Faces[j].Indices[2];
        end;
        Mesh.UnlockIndexBuffer;
        Mesh.LockAttributeBuffer(
          0, PDWord(Attribs)
        );
        if MeshData.Geoms[i].MCount > 0 then
        for j := 0 to m_Geoms[i].FCount - 1 do
        Attribs^[j] := MeshData.Geoms[i].Materials[MeshData.Geoms[i].Faces[j].MaterialID]
        else
        for j := 0 to m_Geoms[i].FCount - 1 do
        Attribs^[j] := 0;
        Mesh.UnlockAttributeBuffer;
        SetLength(Adj, m_Geoms[i].FCount * 3);
        Mesh.GenerateAdjacency(G2EPS, @Adj[0]);
        Mesh.OptimizeInplace(
          D3DXMESHOPT_COMPACT or
          D3DXMESHOPT_ATTRSORT or
          D3DXMESHOPT_VERTEXCACHE,
          @Adj[0], nil, nil, nil
        );
        Mesh.GetAttributeTable(nil, @m_Geoms[i].GCount);
        SetLength(m_Geoms[i].Groups, m_Geoms[i].GCount);
        SetLength(AttribTable, m_Geoms[i].GCount);
        Mesh.GetAttributeTable(@AttribTable[0], nil);
        for j := 0 to m_Geoms[i].GCount - 1 do
        begin
          m_Geoms[i].Groups[j].MatID := AttribTable[j].AttribId;
          m_Geoms[i].Groups[j].VStart := AttribTable[j].VertexStart;
          m_Geoms[i].Groups[j].VCount := AttribTable[j].VertexCount;
          m_Geoms[i].Groups[j].FStart := AttribTable[j].FaceStart;
          m_Geoms[i].Groups[j].FCount := AttribTable[j].FaceCount;
        end;
        m_Geoms[i].VCount := Mesh.GetNumVertices;
        m_Geoms[i].FCount := Mesh.GetNumFaces;
        Core.Graphics.Device.CreateVertexBuffer(
          m_Geoms[i].VCount * TG2WGeom.VertexStride[m_Geoms[i].MaxWeights],
          D3DUSAGE_WRITEONLY,
          0,
          D3DPOOL_MANAGED,
          m_Geoms[i].VB,
          nil
        );
        Core.Graphics.Device.CreateIndexBuffer(
          m_Geoms[i].FCount * 6,
          D3DUSAGE_WRITEONLY,
          D3DFMT_INDEX16,
          D3DPOOL_MANAGED,
          m_Geoms[i].IB,
          nil
        );
        Mesh.LockVertexBuffer(D3DLOCK_READONLY, Ptr1);
        m_Geoms[i].VB.Lock(0, m_Geoms[i].VCount * TG2WGeom.VertexStride[m_Geoms[i].MaxWeights], Ptr2, 0);
        Move(Ptr1^, Ptr2^, m_Geoms[i].VCount * TG2WGeom.VertexStride[m_Geoms[i].MaxWeights]);
        m_Geoms[i].VB.Unlock;
        Mesh.UnlockVertexBuffer;
        Mesh.LockIndexBuffer(D3DLOCK_READONLY, Ptr1);
        m_Geoms[i].IB.Lock(0, m_Geoms[i].FCount * 6, Ptr2, 0);
        Move(Ptr1^, Ptr2^, m_Geoms[i].FCount * 6);
        m_Geoms[i].IB.Unlock;
        Mesh.UnlockIndexBuffer;
        SafeRelease(Mesh);
      end;
      m_AnimCount := MeshData.AnimCount;
      SetLength(m_Anims, m_AnimCount);
      for i := 0 to m_AnimCount - 1 do
      begin
        m_Anims[i].Name := MeshData.Anims[i].Name;
        m_Anims[i].FrameRate := MeshData.Anims[i].FrameRate;
        m_Anims[i].FrameCount := MeshData.Anims[i].FrameCount;
        m_Anims[i].NodeCount := MeshData.Anims[i].NodeCount;
        SetLength(m_Anims[i].Nodes, m_Anims[i].NodeCount);
        for j := 0 to m_Anims[i].NodeCount - 1 do
        begin
          m_Anims[i].Nodes[j].NodeID := MeshData.Anims[i].Nodes[j].NodeID;
          SetLength(m_Anims[i].Nodes[j].Frames, m_Anims[i].FrameCount);
          Move(MeshData.Anims[i].Nodes[j].Frames[0], m_Anims[i].Nodes[j].Frames[0], m_Anims[i].FrameCount * 40);
        end;
      end;
      m_MatCount := MeshData.MaterialCount;
      SetLength(m_Mats, m_MatCount);
      for i := 0 to m_MatCount - 1 do
      begin
        m_Mats[i].Name := MeshData.Materials[i].Name;
        m_Mats[i].TexDiffuse := MeshData.Materials[i].DiffuseMap;
        m_Mats[i].TexNormals := MeshData.Materials[i].NormalMap;
        m_Mats[i].TexSpecular := MeshData.Materials[i].SpecularMap;
        m_Mats[i].TexLightMap := MeshData.Materials[i].LightMap;
      end;
      j := Loader.MeshData^.LightCount;
      m_LightOmniCount := 0;
      m_LightSpotCount := 0;
      if m_ShadowMode = smPCF then
      DepthFormat := D3DFMT_R32F
      else
      DepthFormat := D3DFMT_G16R16F;
      for i := 0 to j - 1 do
      begin
        if Loader.MeshData^.Lights[i].LightType = 1 then
        begin
          SetLength(m_LightsSpot, m_LightSpotCount + 1);
          m_LightsSpot[m_LightSpotCount].NodeID := Loader.MeshData^.Lights[i].NodeID;
          m_LightsSpot[m_LightSpotCount].Color := Loader.MeshData^.Lights[i].Color;
          m_LightsSpot[m_LightSpotCount].AttStart := Loader.MeshData^.Lights[i].AttStart;
          m_LightsSpot[m_LightSpotCount].AttEnd := Loader.MeshData^.Lights[i].AttEnd;
          m_LightsSpot[m_LightSpotCount].SpotInner := Cos(Loader.MeshData^.Lights[i].SpotInner * 0.5);
          m_LightsSpot[m_LightSpotCount].SpotOutter := Cos(Loader.MeshData^.Lights[i].SpotOutter * 0.5);
          m_LightsSpot[m_LightSpotCount].SpotFOV := Loader.MeshData^.Lights[i].SpotOutter;
          m_LightsSpot[m_LightSpotCount].Depth := TG2Texture2DRT.Create;
          m_LightsSpot[m_LightSpotCount].Depth.Initialize(Core);
          m_LightsSpot[m_LightSpotCount].Depth.MakeRenderTarget(m_ShadowMapSize, m_ShadowMapSize, DepthFormat);
          Inc(m_LightSpotCount);
        end
        else
        begin
          SetLength(m_LightsOmni, m_LightOmniCount + 1);
          m_LightsOmni[m_LightOmniCount].NodeID := Loader.MeshData^.Lights[i].NodeID;
          m_LightsOmni[m_LightOmniCount].Color := Loader.MeshData^.Lights[i].Color;
          m_LightsOmni[m_LightOmniCount].AttStart := Loader.MeshData^.Lights[i].AttStart;
          m_LightsOmni[m_LightOmniCount].AttEnd := Loader.MeshData^.Lights[i].AttEnd;
          m_LightsOmni[m_LightOmniCount].Depth := TG2TextureCubeRT.Create;
          m_LightsOmni[m_LightOmniCount].Depth.Initialize(Core);
          m_LightsOmni[m_LightOmniCount].Depth.MakeRenderTarget(m_ShadowMapSize, DepthFormat);
          Inc(m_LightOmniCount);
        end;
      end;
      m_ColliderCount := Loader.MeshData.GeomCount;
      SetLength(m_Colliders, m_ColliderCount);
      for i := 0 to m_ColliderCount - 1 do
      begin
        m_Colliders[i].NodeID := Loader.MeshData.Geoms[i].NodeID;
        if m_Geoms[i].Skinned then
        begin
          SetLength(m_Colliders[i].Vertices, 0);
          SetLength(m_Colliders[i].VerticesT, 0);
          SetLength(m_Colliders[i].Faces, 0);
        end
        else
        begin
          SetLength(m_Colliders[i].Vertices, Loader.MeshData.Geoms[i].VCount);
          SetLength(m_Colliders[i].VerticesT, Loader.MeshData.Geoms[i].VCount);
          Move(Loader.MeshData.Geoms[i].Vertices[0], m_Colliders[i].Vertices[0], Loader.MeshData.Geoms[i].VCount * 12);
          SetLength(m_Colliders[i].Faces, Loader.MeshData.Geoms[i].FCount);
          for j := 0 to Loader.MeshData.Geoms[i].FCount - 1 do
          begin
            m_Colliders[i].Faces[j].Face[0] := Loader.MeshData.Geoms[i].Faces[j].Vertices[0];
            m_Colliders[i].Faces[j].Face[1] := Loader.MeshData.Geoms[i].Faces[j].Vertices[1];
            m_Colliders[i].Faces[j].Face[2] := Loader.MeshData.Geoms[i].Faces[j].Vertices[2];
            m_Colliders[i].Faces[j].ColID := 0;
          end;
        end;
      end;
    end;
    ComputeTransforms;
    m_CurColID := 0;
  finally
    Loader.Free;
  end;
end;

procedure TG2World3D.Render;
begin
  case m_RenderMode of
    rmLightMap: RenderLM;
    rmMultiPass: RenderMP;
  end;
end;

procedure TG2World3D.Update;
var
  i, j: Integer;
  f: Single;
  f0, f1: Integer;
  r0: TG2Quat;
  t0: TG2Vec3;
  s0: TG2Vec3;
  c: TG2Vec3;
begin
  if m_AnimIndex > -1 then
  with m_Anims[m_AnimIndex] do
  begin
    m_AnimFrame := m_AnimFrame + (FrameRate / Core.Timer.TargetUPS) * m_AnimSpeed;
    if m_AnimFrame < 0 then
    m_AnimFrame := FrameCount - (Trunc(Abs(m_AnimFrame)) mod FrameCount + Frac(Abs(m_AnimFrame)));
    if m_AnimFrame >= FrameCount then
    m_AnimFrame := (Trunc(m_AnimFrame) mod FrameCount) + Frac(m_AnimFrame);
    f0 := Trunc(m_AnimFrame);
    f1 := (f0 + 1) mod FrameCount;
    f := Frac(m_AnimFrame);
    for i := 0 to NodeCount - 1 do
    begin
      s0 := G2LerpVec3(Nodes[i].Frames[f0].Scale, Nodes[i].Frames[f1].Scale, f);
      r0 := G2SLerpQuat(Nodes[i].Frames[f0].Rotation, Nodes[i].Frames[f1].Rotation, f);
      t0 := G2LerpVec3(Nodes[i].Frames[f0].Translation, Nodes[i].Frames[f1].Translation, f);
      m_Nodes[Nodes[i].NodeID].TransformCur.SetScaling(s0);
      m_Nodes[Nodes[i].NodeID].TransformCur.Rotate(r0);
      m_Nodes[Nodes[i].NodeID].TransformCur.Translate(t0);
    end;
    ComputeTransforms;
  end;
  for i := 0 to m_GeomCount - 1 do
  m_Geoms[i].AABox := GetGeomBBox(i).AABox;
  for i := 0 to m_MgrChars.Count - 1 do
  if m_MgrChars[i].Active then
  begin
    m_MgrChars[i].Pos := m_MgrChars[i].Pos + m_MgrChars[i].Vel;
    if m_MgrChars[i].Grounded and m_MgrChars[i].DoFriction then
    m_MgrChars[i].Vel := m_MgrChars[i].Vel * 0.9;
    c := m_MgrChars[i].Pos;
    c.y := (c.y + m_MgrChars[i].Rad + m_MgrChars[i].StepHeight);
    Collide(c, m_MgrChars[i].Vel, m_MgrChars[i].Grounded, m_MgrChars[i].Rad, m_MgrChars[i].StepHeight);
    c.y := c.y - m_MgrChars[i].Rad - m_MgrChars[i].StepHeight;
    m_MgrChars[i].Pos := c;
    m_MgrChars[i].Vel := m_MgrChars[i].Vel + m_Gravity;
  end;
end;

function TG2World3D.Initialize(const G2Core: TG2Core): TG2Result;
var
  i, j, s, r: Integer;
  Samp: array [0..2] of Integer;
  SampleArr: array [0..15, 0..2] of Integer;
  SampDstArr: array [0..15] of Single;
  b: Boolean;
  MaxDst: Single;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_FVFDecl[0][0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  m_FVFDecl[0][1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
  m_FVFDecl[0][2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
  m_FVFDecl[0][3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
  m_FVFDecl[0][4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  m_FVFDecl[0][5] := D3DVertexElement(0, 4 * 14, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 1);
  m_FVFDecl[0][6] := D3DDECL_END;
  m_FVFDecl[1][0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  m_FVFDecl[1][1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
  m_FVFDecl[1][2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
  m_FVFDecl[1][3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
  m_FVFDecl[1][4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  m_FVFDecl[1][5] := D3DVertexElement(0, 4 * 14, D3DDECLTYPE_FLOAT1, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDINDICES, 0);
  m_FVFDecl[1][6] := D3DDECL_END;
  m_FVFDecl[2][0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  m_FVFDecl[2][1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
  m_FVFDecl[2][2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
  m_FVFDecl[2][3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
  m_FVFDecl[2][4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  m_FVFDecl[2][5] := D3DVertexElement(0, 4 * 14, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDINDICES, 0);
  m_FVFDecl[2][6] := D3DVertexElement(0, 4 * 16, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDWEIGHT, 0);
  m_FVFDecl[2][7] := D3DDECL_END;
  m_FVFDecl[3][0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  m_FVFDecl[3][1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
  m_FVFDecl[3][2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
  m_FVFDecl[3][3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
  m_FVFDecl[3][4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  m_FVFDecl[3][5] := D3DVertexElement(0, 4 * 14, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDINDICES, 0);
  m_FVFDecl[3][6] := D3DVertexElement(0, 4 * 17, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDWEIGHT, 0);
  m_FVFDecl[3][7] := D3DDECL_END;
  m_FVFDecl[4][0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  m_FVFDecl[4][1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
  m_FVFDecl[4][2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
  m_FVFDecl[4][3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
  m_FVFDecl[4][4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  m_FVFDecl[4][5] := D3DVertexElement(0, 4 * 14, D3DDECLTYPE_FLOAT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDINDICES, 0);
  m_FVFDecl[4][6] := D3DVertexElement(0, 4 * 18, D3DDECLTYPE_FLOAT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDWEIGHT, 0);
  m_FVFDecl[4][7] := D3DDECL_END;
  Core.Graphics.Device.CreateVertexDeclaration(@m_FVFDecl[0][0], m_Decl[0]);
  Core.Graphics.Device.CreateVertexDeclaration(@m_FVFDecl[1][0], m_Decl[1]);
  Core.Graphics.Device.CreateVertexDeclaration(@m_FVFDecl[2][0], m_Decl[2]);
  Core.Graphics.Device.CreateVertexDeclaration(@m_FVFDecl[3][0], m_Decl[3]);
  Core.Graphics.Device.CreateVertexDeclaration(@m_FVFDecl[4][0], m_Decl[4]);
  m_Effect := Core.Graphics.ShaderLib.RequestEffect('fx_World3D');
  m_Render2D := TG2Render2D.Create;
  m_Render2D.Initialize(Core);
  m_Prim2D := TG2Primitives2D.Create;
  m_Prim2D.Initialize(Core);
  m_MgrTextures := TG2TextureMgr.Create;
  m_MgrTextures.Initialize(G2Core);
  with m_MgrTextures.CreateTexture2D('NULL', 4, 4, 1, 0, D3DFMT_A8R8G8B8) do
  begin
    Surfaces[0].Lock;
    for i := 0 to 3 do
    for j := 0 to 3 do
    Surfaces[0].Pixels[i, j] := $008080ff;
    Surfaces[0].UnLock;
  end;
  m_ShadowProjRT := TG2Texture2DRT.Create;
  m_ShadowProjRT.Initialize(Core);
  m_ShadowProjRT.MakeRenderTarget(1024, 1024, D3DFMT_R32F);
  m_ShadowProjDS := TG2SurfaceDS.Create;
  m_ShadowProjDS.Initialize(Core);
  m_ShadowProjDS.CreateDepthStencil(1024, 1024, D3DFMT_D16);
  m_ShadowProjV.SetView(G2Vec3(100, 100, -100), G2Vec3(0, 0, 0), G2Vec3(0, 1, 0));
  m_ShadowProjP.SetOrthogonal(512, 512, 1, 1000);
  m_LightDepthSurface := TG2SurfaceDS.Create;
  m_LightDepthSurface.Initialize(Core);
  m_LightDepthSurface.CreateDepthStencil(m_ShadowMapSize, m_ShadowMapSize, D3DFMT_D24S8);
  m_BlurRT := TG2Texture2DRT.Create;
  m_BlurRT.Initialize(Core);
  m_BlurRT.MakeRenderTarget(m_ShadowMapSize, m_ShadowMapSize, D3DFMT_G16R16F);
  r := RandSeed;
  Randomize;
  m_ShadowCubeStep := m_ShadowCubeSamplerScale / m_ShadowMapSize;
  m_ShadowProjStep := (m_ShadowProjSamplerScale * 100) / m_ShadowMapSize;
  FillChar(SampleArr, SizeOf(SampleArr), $ff);
  MaxDst := G2Vec3(15, 15, 15).Len;
  for i := 0 to 15 do
  begin
    for s := 0 to 2 do
    begin
      Samp[s] := 0;
      b := True;
      while b do
      begin
        b := False;
        for j := 0 to i - 1 do
        if SampleArr[j][0] = Samp[s] then
        begin
          Samp[s] := Random(31) - 15;
          b := True;
          Break;
        end;
      end;
    end;
    Move(Samp, SampleArr[i], 4 * 3);
    SampDstArr[i] := MaxDst - G2Vec3(Samp[0], Samp[1], Samp[2]).Len;
    m_CubeSampleMap[i] := G2Vec4(Samp[0], Samp[1], Samp[2], 0) * m_ShadowCubeStep;
  end;
  MaxDst := SampDstArr[0];
  for i := 1 to 15 do
  MaxDst := MaxDst + SampDstArr[i];
  MaxDst := 1 / MaxDst;
  for i := 0 to 15 do
  m_CubeSampleMap[i].w := SampDstArr[i] * MaxDst;
  FillChar(SampleArr, SizeOf(SampleArr), $ff);
  SampleArr[0][0] := 0;
  SampleArr[0][1] := 0;
  m_ProjSampleMap[0].SetValue(0, 0, 0, 0);
  for i := 1 to 15 do
  begin
    b := True;
    while b do
    begin
      Samp[0] := Random(5) - 2;
      Samp[1] := Random(5) - 2;
      b := False;
      for j := 0 to i - 1 do
      if (SampleArr[i][0] = Samp[0]) and (SampleArr[i][1] = Samp[1]) then
      begin
        b := True;
        Break;
      end;
    end;
    m_ProjSampleMap[i].SetValue(Samp[0] * m_ShadowProjStep, Samp[1] * m_ShadowProjStep, 0, 0);
  end;
  RandSeed := r;
  m_MgrMeshes := TG2MeshMgr.Create;
  m_MgrMeshes.Initialize(G2Core);
  m_MgrChars := TG2WCharMgr.Create;
  m_MgrChars.World3D := Self;
  m_MgrChars.Initialize(G2Core);
  Result := grOk;
end;

function TG2World3D.Finalize: TG2Result;
var
  i: Integer;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  m_MgrChars.Finalize;
  m_MgrChars.Free;
  m_MgrMeshes.Finalize;
  m_MgrMeshes.Free;
  for i := 0 to m_LightOmniCount - 1 do
  begin
    m_LightsOmni[i].Depth.Finalize;
    m_LightsOmni[i].Depth.Free;
  end;
  for i := 0 to m_LightSpotCount - 1 do
  begin
    m_LightsSpot[i].Depth.Finalize;
    m_LightsSpot[i].Depth.Free;
  end;
  m_ShadowProjDS.Finalize;
  m_ShadowProjDS.Free;
  m_ShadowProjRT.Finalize;
  m_ShadowProjRT.Free;
  m_LightDepthSurface.Finalize;
  m_LightDepthSurface.Free;
  m_BlurRT.Finalize;
  m_BlurRT.Free;
  m_Prim2D.Finalize;
  m_Prim2D.Free;
  m_Render2D.Finalize;
  m_Render2D.Free;
  m_MgrTextures.Finalize;
  m_MgrTextures.Free;
  SafeRelease(m_Decl);
  Result := grOk;
end;
//TG2World3D END

//TG2WCharMgr BEGIN
function TG2WCharMgr.GetChar(const Index: Integer): TG2WChar;
begin
  Result := TG2WChar(m_Resources[Index]);
end;

constructor TG2WCharMgr.Create;
begin
  inherited Create;
end;

destructor TG2WCharMgr.Destroy;
begin
  inherited Destroy;
end;

function TG2WCharMgr.CreateCharacter(const Name: AnsiString; const Mesh: TG2Mesh): TG2WChar;
begin
  Result := TG2WChar.Create;
  Result.Name := Name;
  Result.World3D := m_World3D;
  Result.Mesh := Mesh;
  if G2ResOk(Result.Initialize(Core)) then
  AddResource(Result) else FreeAndNil(Result);
end;
//TG2WCharMgr END

//TG2WChar BEGIN
function TG2WChar.GetFinalTransform(const NodeName: AnsiString): TG2Mat;
  var i: Integer;
begin
  i := Mesh.NodeIndex(NodeName);
  if i > -1 then
  begin
    Result.SetScaling(Scale);
    Result.RotateY(Ang);
    Result.Translate(Pos);
    Result := MeshInst.NodeTransforms[i].TransformRen * Result;
  end
  else Result.SetIdentity;
end;

constructor TG2WChar.Create;
begin
  inherited Create;
end;

destructor TG2WChar.Destroy;
begin
  inherited Destroy;
end;

procedure TG2WChar.Render;
begin

end;

procedure TG2WChar.Update;
begin

end;

function TG2WChar.Initialize(const G2Core: TG2Core): TG2Result;
var
  m: Integer;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_MeshInst := m_Mesh.InstanceCreate;
  for m := 0 to Mesh.MaterialCount - 1  do
  begin
    m_MeshInst.Materials[m].MapDiffuse := m_World3D.FindTexture(m_Mesh.Materials[m].DiffuseMap);
    m_MeshInst.Materials[m].MapNormals := m_World3D.FindTexture(m_Mesh.Materials[m].NormalMap);
    m_MeshInst.Materials[m].MapSpecular := m_World3D.FindTexture(m_Mesh.Materials[m].SpecularMap);
  end;
  Scale.SetValue(1, 1, 1);
  Rad := 1;
  StepHeight := 0;
  LightAmount := 1;
  Alpha := 1;
  Grounded := False;
  DoFriction := True;
  Active := False;
  Result := grOk;
end;

function TG2WChar.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  FreeAndNil(m_MeshInst);
  Result := grOk;
end;
//TG2WChar END

end.
