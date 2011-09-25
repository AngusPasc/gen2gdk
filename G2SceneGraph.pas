unit G2SceneGraph;

{$include Gen2.inc}

interface

uses
  Classes,
  Types,
  Windows,
  SysUtils,
  DXTypes,
  Direct3D9,
  D3DX9,
  Math,
  SyncObjs,
  G2Math,
  Gen2,
  G2MeshLoader,
  G2MeshLoaderG2M;

  const G2QL_GEOMS = 0;
  const G2QL_CHARS = 1;
  const G2QL_LIGHTS = 2;

type
  TG2SceneGraph = class;
  TG2SGNode = class;
  TG2SGFrame = class;
  TG2SGGeom = class;
  TG2SGChar = class;
  TG2SGLight = class;
  TG2SGLightPoint = class;
  TG2SGLightSpot = class;
  TG2SGLightDir = class;

  CG2SGGeomClass = class of TG2SGGeom;
  CG2SGCharClass = class of TG2SGChar;
  CG2SGLightPointClass = class of TG2SGLightPoint;
  CG2SGLightSpotClass = class of TG2SGLightSpot;
  CG2SGLightDirClass = class of TG2SGLightDir;

  TG2SGDataFormats = record
  public
    const VertexStrideGeom = 68;
    var FVFGeom: TFVFDeclaration;
    var DeclGeom: IDirect3DVertexDeclaration9;
    var DefClassOfGeom: CG2SGGeomClass;
    var DefClassOfChar: CG2SGCharClass;
    var DefClassOfLightPoint: CG2SGLightPointClass;
    var DefClassOfLightSpot: CG2SGLightSpotClass;
    var DefClassOfLightDir: CG2SGLightDirClass;
  end;
  PG2SGDataFormats = ^TG2SGDataFormats;

  TG2SGMaterial = record
  public
    var ChannelCount: Integer;
    var Channels: array of record
    public
      var Name: AnsiString;
      var TwoSided: Boolean;
      var TexDiffuse: TG2Texture2D;
      var TexNormals: TG2Texture2D;
      var TexSpecular: TG2Texture2D;
      var TexLightMap: TG2Texture2D;
    end;
  end;
  PG2SGMaterial = ^TG2SGMaterial;

  TG2SGMaterialGroup = record
  public
    var Material: PG2SGMaterial;
    var VStart: Integer;
    var VCount: Integer;
    var FStart: Integer;
    var FCount: Integer;
  end;
  PG2SGMaterialGroup = ^TG2SGMaterialGroup;

  TG2SGVertexGeom = packed record
  public
    var Position: TG2Vec3;
    var Tangent: TG2Vec3;
    var Binormal: TG2Vec3;
    var Normal: TG2Vec3;
    var TexCoords: array[0..1] of TG2Vec2;
    var Color: TG2Color;
  end;
  PG2SGVertexGeom = ^TG2SGVertexGeom;
  TG2SGVertexGeomArr = array[Word] of TG2SGVertexGeom;
  PG2SGVertexGeomArr = ^TG2SGVertexGeomArr;

  TG2SGCollider = record
  public
    var Vertices: array of TG2Vec3;
    var Faces: array of record
    public
      var Indices: array[0..2] of Word;
      var Edges: array[0..2] of record
      public
        var Indices: array[0..1] of Word;
        var N: TG2Vec3;
      end;
      var AABox: TG2AABox;
      var Plane: TG2Plane;
      var CillideID: DWord;
    end;
  end;
  PG2SGCollider = ^TG2SGCollider;

  PG2SGGeomOcTreeNode = ^TG2SGGeomOcTreeNode;
  TG2SGGeomOcTreeNode = record
  public
    var Parent: PG2SGGeomOcTreeNode;
    var AABox: TG2AABox;
    var CountX: Integer;
    var CountY: Integer;
    var CountZ: Integer;
    var NoDiv: Boolean;
    var SubNodes: array of array of array of TG2SGGeomOcTreeNode;
    var Faces: array of Word;
    var Depth: Integer;
    var TotalFaces: Integer;
  end;

  PG2SGOcTreeNode = ^TG2SGOcTreeNode;
  TG2SGOcTreeNode = record
  public
    var AABox: TG2AABox;
    var Parent: PG2SGOcTreeNode;
    var SubNodes: array of array of array of TG2SGOcTreeNode;
    var DivX: Boolean;
    var DivY: Boolean;
    var DivZ: Boolean;
    var DivN: Boolean;
    var MinX, MaxX, MinY, MaxY, MinZ, MaxZ: Integer;
    var TotalItemCount: Integer;
    var Items: TG2QuickList;
    var QueryLinks: TG2QuickList;
    procedure TotalItemsInc(const Amount: Integer = 1);
    procedure TotalItemsDec(const Amount: Integer = 1);
  end;

  TG2SGOcTreeItem = record
  public
    var Frame: TG2SGFrame;
    var QueryLinks: TG2QuickList;
    var OcTreeNodes: TG2QuickList;
  end;
  PG2SGOcTreeItem = ^TG2SGOcTreeItem;

  TG2SGQueryStatistics = record
  public
    var ObjectsCulled: Integer;
    var ObjectsRendered: Integer;
  end;

  TG2SGOcclusionCull = (ocOcTree, ocObjects);
  TG2SGOcclusionCullSet = set of TG2SGOcclusionCull;

  TG2SGQuery = record
  strict private
    var m_OcclusionCull: TG2SGOcclusionCullSet;
    var m_OcclusionCheckTimeBias: DWord;
    var m_DistanceSortEnable: Boolean;
  private
    var ID: Integer;
    var FetchID: DWord;
    var SceneGraph: TG2SceneGraph;
    var QueryLists: array [0..2] of TG2QuickSortList;
    var QueryNodes: TG2QuickList;
    var QueryGeoms: TG2QuickList;
    var OcclusionRT: TG2SurfaceRT;
    procedure Init;
    procedure UnInit;
    procedure CheckFetchID;
    function GetGeomCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetGeoms(const Index: Integer): TG2SGGeom; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCharCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetChars(const Index: Integer): TG2SGChar; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLightCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLights(const Index: Integer): TG2SGLight; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var Stats: TG2SGQueryStatistics;
    property OcclusionCull: TG2SGOcclusionCullSet read m_OcclusionCull write m_OcclusionCull;
    property OcclusionCheckTimeBias: DWord read m_OcclusionCheckTimeBias write m_OcclusionCheckTimeBias;
    property DistanceSortEnable: Boolean read m_DistanceSortEnable write m_DistanceSortEnable;
    property GeomCount: Integer read GetGeomCount;
    property Geoms[const Index: Integer]: TG2SGGeom read GetGeoms;
    property CharCount: Integer read GetCharCount;
    property Chars[const Index: Integer]: TG2SGChar read GetChars;
    property LightCount: Integer read GetLightCount;
    property Lights[const Index: Integer]: TG2SGLight read GetLights;
    procedure CheckOcclusion(const Frustum: TG2Frustum);
  end;
  PG2SGQuery = ^TG2SGQuery;

  TG2SGQueryItemLink = record
  public
    var Query: PG2SGQuery;
    var FetchID: DWord;
    var OcclusionQuery: IDirect3DQuery9;
    var OcclusionQueryID: DWord;
    var OcclusionQueryEnabled: Boolean;
    var OcclusionVisible: Boolean;
    var OcclusionCheckTime: DWord;
    procedure Init;
    procedure UnInit;
  end;
  PG2SGQueryItemLink = ^TG2SGQueryItemLink;

  TG2SGQueryNodeLink = record
  public
    var Query: PG2SGQuery;
    var FetchID: DWord;
    var OcclusionQuery: IDirect3DQuery9;
    var OcclusionQueryID: DWord;
    var OcclusionQueryEnabled: Boolean;
    var OcclusionVisible: Boolean;
    var OcclusionCheckTime: DWord;
    var FrustumTest: TG2FrustumCheck;
    procedure Init;
    procedure UnInit;
  end;
  PG2SGQueryNodeLink = ^TG2SGQueryNodeLink;

  TG2SceneGraph = class
  strict private
    var m_OcTreeGrid: array of array of array of PG2SGOcTreeNode;
    var m_OcTreeNodeFetch: TG2QuickList;
    var m_GeomCollideList: TG2QuickList;
    var m_Prim3D: TG2Primitives3D;
    var m_CurCollideID: DWord;
    var m_PlugGraphics: TG2PlugGraphics;
    procedure ResetCollideID;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
    function GetAABox: TG2AABox; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var RootNode: TG2SGOcTreeNode;
    var OcTreeBuilt: Boolean;
    var VB: TG2VB;
    var IB: TG2IB;
    var CS: TCriticalSection;
    function OcTreeAutoBuild: Boolean;
    procedure OcTreeBuild(const AABox: TG2AABox; const MinDivSize: Single);
    procedure OcTreeDestroy;
    procedure OcTreeAddItem(const Item: PG2SGOcTreeItem);
    procedure OcTreeRemoveItem(const Item: PG2SGOcTreeItem);
    procedure OcTreeFetchNodes(const AABox: TG2AABox; const List: PG2QuickList; const IncludeEmpty: Boolean = False); overload;
    procedure OcTreeFetchNodes(const Frustum: TG2Frustum; const Query: PG2SGQuery); overload;
    procedure OcTreeGetIndices(const AABox: TG2AABox; var IndMinX, IndMinY, IndMinZ, IndMaxX, IndMaxY, IndMaxZ: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var DataFormats: TG2SGDataFormats;
    var Core: TG2Core;
    var Queries: TG2QuickList;
    var Materials: TG2QuickList;
    var SearchPaths: array of String;
    var MgrTextures: TG2TextureMgr;
    var MgrCharMeshes: TG2MeshMgr;
    var Nodes: TG2QuickList;
    var Frames: TG2QuickList;
    var Geoms: TG2QuickList;
    var Chars: TG2QuickList;
    var Lights: TG2QuickList;
    var Depth: Integer;
    property AABox: TG2AABox read GetAABox;
    constructor Create(const G2Core: TG2Core);
    destructor Destroy; override;
    procedure SearchPathAdd(const Dir: String);
    procedure SearchPathsClear;
    function FindTexture(
      const Name: WideString;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
    function FindTextureDiffuse(const Name: WideString): TG2Texture2D;
    function FindTextureSpecular(const Name: WideString): TG2Texture2D;
    function FindTextureNormals(const Name: WideString): TG2Texture2D;
    function FindTextureLightMap(const Name: WideString): TG2Texture2D;
    function FindNode(const Name: AnsiString): TG2SGNode;
    function MaterialAdd: PG2SGMaterial;
    procedure MaterialsClear;
    function QueryCreate: PG2SGQuery;
    procedure QueryDestroy(const q: PG2SGQuery);
    procedure QueryFetch(const Frustum: TG2Frustum; const Query: PG2SGQuery);
    function CollideSphere(var s: TG2Sphere; const Query: PG2SGQuery): Boolean;
    function CollideChar(const c: TG2SGChar; const Query: PG2SGQuery): Boolean;
    function CollideRay(const r: TG2Ray; var GeomID: Integer; var FaceID: Word; var U, V, D: Single): Boolean; overload;
    function CollideRay(const r: TG2Ray; const Query: PG2SGQuery; const Distance: Single; var GeomID: Integer; var FaceID: Word; var U, V, D: Single): Boolean; overload;
    procedure LoadG2M(const Loader: TG2MeshLoaderG2M); overload;
    procedure LoadG2M(const f: String); overload;
    procedure LoadG2M(const s: TStream); overload;
    procedure LoadG2M(const Buffer: Pointer; const Size: Integer); overload;
    procedure Clear;
    procedure Update(const Query: PG2SGQuery);
    procedure Render(const Query: PG2SGQuery);
  end;

  TG2SGNode = class
  strict private
    m_UserData: Pointer;
  strict protected
    m_SceneGraph: TG2SceneGraph;
  public
    var Name: AnsiString;
    var Parent: TG2SGNode;
    var Children: array of TG2SGNode;
    var Transform: TG2Mat;
    property UserData: Pointer read m_UserData write m_UserData;
    constructor Create(const SceneGraph: TG2SceneGraph); virtual;
    destructor Destroy; override;
    procedure Update; virtual;
  end;

  TG2SGFrame = class (TG2SGNode)
  strict private
    var m_MinGridX, m_MinGridY, m_MinGridZ: Integer;
    var m_MaxGridX, m_MaxGridY, m_MaxGridZ: Integer;
  strict protected
    var m_Render: Boolean;
    procedure SetRender(const Value: Boolean); virtual;
  protected
    var QueryListID: Integer;
    function GetAABox: TG2AABox; virtual; abstract;
  public
    var OcTreeItem: TG2SGOcTreeItem;
    var AABox: TG2AABox;
    property Render: Boolean read m_Render write SetRender;
    constructor Create(const SceneGraph: TG2SceneGraph); override;
    destructor Destroy; override;
    procedure Update; override;
  end;

  TG2SGGeom = class (TG2SGFrame)
  strict private
    var m_PrevTransform: TG2Mat;
    var m_Collide: Boolean;
    var m_OcTree: Boolean;
    var m_FaceFetchID: array of Int64;
    var m_CurFaceFetchID: Int64;
    procedure SetCollide(const Value: Boolean);
    procedure UpdateCollider;
    procedure SetOcTree(const Value: Boolean);
    procedure ResetFaceFectchID;
  protected
    var OcTreeNode: TG2SGGeomOcTreeNode;
    property OcTree: Boolean read m_OcTree write SetOcTree;
    procedure SetRender(const Value: Boolean); override;
    function GetAABox: TG2AABox; override;
  public
    var VCount: Integer;
    var FCount: Integer;
    var GCount: Integer;
    var Vertices: array of TG2SGVertexGeom;
    var Faces: array of array[0..2] of Word;
    var Groups: array of TG2SGMaterialGroup;
    var VB: IDirect3DVertexBuffer9;
    var IB: IDirect3DIndexBuffer9;
    var Collider: TG2SGCollider;
    var OOBox: TG2Box;
    property Collide: Boolean read m_Collide write SetCollide;
    constructor Create(const SceneGraph: TG2SceneGraph); override;
    destructor Destroy; override;
    procedure Update; override;
    function IntersectRay(const r: TG2Ray): Boolean; overload;
    function IntersectRay(const r: TG2Ray; var FaceID: Word; var U, V, Dist: Single): Boolean; overload;
  end;

  TG2SGChar = class (TG2SGFrame)
  strict private
    var m_MinGridX, m_MinGridY, m_MinGridZ: Integer;
    var m_MaxGridX, m_MaxGridY, m_MaxGridZ: Integer;
    var m_Mesh: TG2Mesh;
    var m_MeshInst: TG2MeshInst;
    var m_Collide: Boolean;
    procedure SetMesh(const Value: TG2Mesh);
  protected
    function GetAABox: TG2AABox; override;
  public
    var Radius: Single;
    var Step: TG2Vec3;
    var Vel: TG2Vec3;
    var Grounded: Boolean;
    var StepHardness: Single;
    property Mesh: TG2Mesh read m_Mesh write SetMesh;
    property MeshInst: TG2MeshInst read m_MeshInst;
    property Collide: Boolean read m_Collide write m_Collide;
    constructor Create(const SceneGraph: TG2SceneGraph); override;
    destructor Destroy; override;
    procedure Update; override;
  end;

  TG2SGLight = class (TG2SGFrame)
  public
    type TG2SGLightType = (ltNone, ltPoint, ltSpot, ltDir);
    var LightType: TG2SGLightType;
    var Color: TG2Color;
    var AttStart: Single;
    var AttEnd: Single;
    constructor Create(const SceneGraph: TG2SceneGraph); override;
    destructor Destroy; override;
  end;

  TG2SGLightPoint = class (TG2SGLight)
  protected
    function GetAABox: TG2AABox; override;
  public
    constructor Create(const SceneGraph: TG2SceneGraph); override;
  end;

  TG2SGLightSpot = class (TG2SGLight)
  protected
    function GetAABox: TG2AABox; override;
  public
    var SpotInner: Single;
    var SpotOutter: Single;
    constructor Create(const SceneGraph: TG2SceneGraph); override;
  end;

  TG2SGLightDir = class (TG2SGLight)
  protected
    function GetAABox: TG2AABox; override;
  public
    var WidthInner: Single;
    var WidthOutter: Single;
    var HeightInner: Single;
    var HeightOutter: Single;
    var Aspect: Single;
    constructor Create(const SceneGraph: TG2SceneGraph); override;
  end;

implementation

//TG2SGOcTreeNode BEGIN
procedure TG2SGOcTreeNode.TotalItemsInc(const Amount: Integer = 1);
  procedure IncItemCount(const n: PG2SGOcTreeNode);
  begin
    Inc(n^.TotalItemCount, Amount);
    if Assigned(n^.Parent) then
    IncItemCount(n^.Parent);
  end;
begin
  IncItemCount(@Self);
end;

procedure TG2SGOcTreeNode.TotalItemsDec(const Amount: Integer = 1);
  procedure DecItemCount(const n: PG2SGOcTreeNode);
  begin
    Dec(n^.TotalItemCount, Amount);
    if Assigned(n^.Parent) then
    DecItemCount(n^.Parent);
  end;
begin
  DecItemCount(@Self);
end;
//TG2SGOcTreeNode END

//TG2SGQuery BEGIN
procedure TG2SGQuery.Init;
  var i: Integer;
  var Link: PG2SGqueryItemLink;
  procedure LinkNode(const n: PG2SGOcTreeNode);
    var Link: PG2SGQueryNodeLink;
    var nx, ny, nz: Integer;
  begin
    New(Link);
    Link^.Query := @Self;
    Link^.Init;
    n^.QueryLinks.Add(Link);
    if not n^.DivN then
    for nx := n^.MinX to n^.MaxX do
    for ny := n^.MinY to n^.MaxY do
    for nz := n^.MinZ to n^.MaxZ do
    LinkNode(@n^.SubNodes[nx, ny, nz]);
  end;
begin
  QueryNodes.Clear;
  QueryNodes.Capacity := 32;
  QueryGeoms.Clear;
  QueryGeoms.Capacity := 32;
  for i := 0 to 2 do
  begin
    QueryLists[i].Capacity := 32;
    QueryLists[i].Clear;
  end;
  for i := 0 to SceneGraph.Frames.Count - 1 do
  begin
    New(Link);
    Link^.Query := @Self;
    Link^.Init;
    TG2SGFrame(SceneGraph.Frames[i]).OcTreeItem.QueryLinks.Add(Link);
  end;
  if SceneGraph.OcTreeBuilt then
  LinkNode(@SceneGraph.RootNode);
  m_OcclusionCull := [];
  m_OcclusionCheckTimeBias := 1000;
  m_DistanceSortEnable := False;
  Stats.ObjectsCulled := 0;
  Stats.ObjectsRendered := 0;
  OcclusionRT := TG2SurfaceRT.Create;
  OcclusionRT.Initialize(SceneGraph.Core);
end;

procedure TG2SGQuery.UnInit;
  var i: Integer;
  var Link: PG2SGQueryItemLink;
  procedure UnLinkNode(const n: PG2SGOcTreeNode);
    var Link: PG2SGQueryNodeLink;
    var nx, ny, nz: Integer;
  begin
    Link := n^.QueryLinks[ID];
    n^.QueryLinks.Delete(ID);
    Link^.UnInit;
    Dispose(Link);
    if not n^.DivN then
    for nx := n^.MinX to n^.MaxX do
    for ny := n^.MinY to n^.MaxY do
    for nz := n^.MinZ to n^.MaxZ do
    UnLinkNode(@n^.SubNodes[nx, ny, nz]);
  end;
begin
  OcclusionRT.Finalize;
  OcclusionRT.Free;
  if SceneGraph.OcTreeBuilt then
  UnLinkNode(@SceneGraph.RootNode);
  for i := 0 to SceneGraph.Frames.Count - 1 do
  begin
    Link := PG2SGQueryItemLink(TG2SGFrame(SceneGraph.Frames[i]).OcTreeItem.QueryLinks[ID]);
    TG2SGFrame(SceneGraph.Frames[i]).OcTreeItem.QueryLinks.Delete(ID);
    Link^.UnInit;
    Dispose(Link);
  end;
end;

procedure TG2SGQuery.CheckFetchID;
  procedure ResetNodeFetchID(const n: PG2SGOcTreeNode);
    var nx, ny, nz: Integer;
  begin
    PG2SGQueryNodeLink(n^.QueryLinks[ID])^.FetchID := 0;
    PG2SGQueryNodeLink(n^.QueryLinks[ID])^.OcclusionQueryID := 0;
    if not n^.DivN then
    for nx := n^.MinX to n^.MaxX do
    for ny := n^.MinY to n^.MaxY do
    for nz := n^.MinZ to n^.MaxZ do
    ResetNodeFetchID(@n^.SubNodes[nx, ny, nz]);
  end;
  var i: Integer;
begin
  if FetchID >= High(DWord) - 1 then
  begin
    FetchID := 0;
    for i := 0 to SceneGraph.Frames.Count - 1 do
    PG2SGQueryItemLink(TG2SGFrame(SceneGraph.Frames[i]).OcTreeItem.QueryLinks[ID])^.FetchID := 0;
  end;
end;

function TG2SGQuery.GetGeomCount: Integer;
begin
  Result := QueryLists[G2QL_GEOMS].Count;
end;

function TG2SGQuery.GetGeoms(const Index: Integer): TG2SGGeom;
begin
  Result := TG2SGGeom(QueryLists[G2QL_GEOMS][Index]);
end;

function TG2SGQuery.GetCharCount: Integer;
begin
  Result := QueryLists[G2QL_CHARS].Count;
end;

function TG2SGQuery.GetChars(const Index: Integer): TG2SGChar;
begin
  Result := TG2SGChar(QueryLists[G2QL_CHARS][Index]);
end;

function TG2SGQuery.GetLightCount: Integer;
begin
  Result := QueryLists[G2QL_LIGHTS].Count;
end;

function TG2SGQuery.GetLights(const Index: Integer): TG2SGLight;
begin
  Result := TG2SGLight(QueryLists[G2QL_LIGHTS][Index]);
end;

procedure TG2SGQuery.CheckOcclusion(const Frustum: TG2Frustum);
  var RenderSurface, DepthSurface: IDirect3DSurface9;
  var DepthDesc: TD3DSurfaceDesc;
  var i: Integer;
  var Node: PG2SGOcTreeNode;
  var Geom: TG2SGGeom;
  var LinkNode: PG2SGQueryNodeLink;
  var LinkItem: PG2SGQueryItemLink;
  var m: TG2Mat;
  var PrevZEnable: Boolean;
  var PrevZWriteEnable: Boolean;
  var PrevV, PrevP, PrevW: TG2Mat;
  var PrevTexture: IDirect3DBaseTexture9;
  var VPos: TG2Vec3;
  var AABox: TG2AABox;
  var NearPlaneBias: Single;
begin
  if m_OcclusionCull = [] then Exit;
  SceneGraph.Core.Graphics.Device.GetDepthStencilSurface(DepthSurface);
  SceneGraph.Core.Graphics.Device.GetRenderTarget(0, RenderSurface);
  DepthSurface.GetDesc(DepthDesc);
  if (OcclusionRT.Width <> DepthDesc.Width)
  or (OcclusionRT.Height <> DepthDesc.Height) then
  OcclusionRT.CreateRenderTarget(DepthDesc.Width, DepthDesc.Height);
  SafeRelease(DepthSurface);
  SceneGraph.Core.Graphics.Device.SetRenderTarget(0, OcclusionRT.Surface);
  PrevZEnable := SceneGraph.Core.Graphics.RenderStates.ZEnable;
  PrevZWriteEnable := SceneGraph.Core.Graphics.RenderStates.ZWriteEnable;
  SceneGraph.Core.Graphics.Device.GetTexture(0, PrevTexture);
  SceneGraph.Core.Graphics.Device.GetTransform(D3DTS_VIEW, PG2MatRef(@PrevV)^);
  SceneGraph.Core.Graphics.Device.GetTransform(D3DTS_PROJECTION, PG2MatRef(@PrevP)^);
  SceneGraph.Core.Graphics.Device.GetTransform(D3DTS_WORLD, PG2MatRef(@PrevW)^);
  SceneGraph.Core.Graphics.Device.SetTransform(D3DTS_VIEW, Frustum.RefV^);
  SceneGraph.Core.Graphics.Device.SetTransform(D3DTS_PROJECTION, Frustum.RefP^);
  SceneGraph.Core.Graphics.RenderStates.ZEnable := True;
  SceneGraph.Core.Graphics.RenderStates.ZWriteEnable := False;
  SceneGraph.Core.Graphics.RenderStates.DepthBias := -0.0001;
  SceneGraph.Core.Graphics.Device.SetTexture(0, nil);
  SceneGraph.Core.Graphics.Device.SetFVF(D3DFVF_XYZ);
  SceneGraph.IB.SetToDevice;
  SceneGraph.VB.SetToDevice;
  VPos.x := -Frustum.RefV^.e30;
  VPos.y := -Frustum.RefV^.e31;
  VPos.z := -Frustum.RefV^.e32;
  VPos := VPos.Transform3x3(Frustum.RefV^.Transpose);
  NearPlaneBias := -Frustum.RefP^.e32 * 1.4;
  if ocOcTree in m_OcclusionCull then
  for i := 0 to QueryNodes.Count - 1 do
  begin
    Node := PG2SGOcTreeNode(QueryNodes[i]);
    LinkNode := PG2SGQueryNodeLink(Node^.QueryLinks[ID]);
    AABox := Node^.AABox;
    AABox.MinV := AABox.MinV - NearPlaneBias;
    AABox.MaxV := AABox.MaxV + NearPlaneBias;
    if (GetTickCount - LinkNode^.OcclusionCheckTime >= m_OcclusionCheckTimeBias)
    and (not AABox.PointInside(VPos)) then
    begin
      LinkNode^.OcclusionQueryID := FetchID;
      LinkNode^.OcclusionQueryEnabled := True;
      LinkNode^.OcclusionQuery.Issue(D3DISSUE_BEGIN);
      m.SetScaling(
        Node^.AABox.MaxV.x - Node^.AABox.MinV.x,
        Node^.AABox.MaxV.y - Node^.AABox.MinV.y,
        Node^.AABox.MaxV.z - Node^.AABox.MinV.z
      );
      m.Translate(Node^.AABox.MinV);
      SceneGraph.Core.Graphics.Device.SetTransform(D3DTS_WORLD, m);
      SceneGraph.Core.Graphics.Device.DrawIndexedPrimitive(
        D3DPT_TRIANGLELIST, 0,
        0, 8, 0, 12
      );
      LinkNode^.OcclusionQuery.Issue(D3DISSUE_END);
      LinkNode^.OcclusionCheckTime := GetTickCount;
    end;
  end;
  if ocObjects in m_OcclusionCull then
  for i := 0 to QueryGeoms.Count - 1 do
  begin
    Geom := TG2SGGeom(QueryGeoms[i]);
    LinkItem := PG2SGQueryItemLink(Geom.OcTreeItem.QueryLinks[ID]);
    AABox := Geom.AABox;
    AABox.MinV := AABox.MinV - NearPlaneBias;
    AABox.MaxV := AABox.MaxV + NearPlaneBias;
    if (GetTickCount - LinkItem^.OcclusionCheckTime >= m_OcclusionCheckTimeBias)
    and (not AABox.PointInside(VPos)) then
    begin
      LinkItem^.OcclusionQueryID := FetchID;
      LinkItem^.OcclusionQueryEnabled := True;
      LinkItem^.OcclusionQuery.Issue(D3DISSUE_BEGIN);
      m.SetScaling(
        Geom.AABox.MaxV.x - Geom.AABox.MinV.x,
        Geom.AABox.MaxV.y - Geom.AABox.MinV.y,
        Geom.AABox.MaxV.z - Geom.AABox.MinV.z
      );
      m.Translate(Geom.AABox.MinV);
      SceneGraph.Core.Graphics.Device.SetTransform(D3DTS_WORLD, m);
      SceneGraph.Core.Graphics.Device.DrawIndexedPrimitive(
        D3DPT_TRIANGLELIST, 0,
        0, 8, 0, 12
      );
      LinkItem^.OcclusionQuery.Issue(D3DISSUE_END);
      LinkItem^.OcclusionCheckTime := GetTickCount;
    end;
  end;
  SceneGraph.Core.Graphics.Device.SetRenderTarget(0, RenderSurface);
  SafeRelease(RenderSurface);
  SceneGraph.Core.Graphics.RenderStates.DepthBias := 0;
  SceneGraph.Core.Graphics.RenderStates.ZEnable := PrevZEnable;
  SceneGraph.Core.Graphics.RenderStates.ZWriteEnable := PrevZWriteEnable;
  SceneGraph.Core.Graphics.Device.SetTransform(D3DTS_WORLD, PrevW);
  SceneGraph.Core.Graphics.Device.SetTransform(D3DTS_VIEW, PrevV);
  SceneGraph.Core.Graphics.Device.SetTransform(D3DTS_PROJECTION, PrevP);
  SceneGraph.Core.Graphics.Device.SetTexture(0, PrevTexture);
end;
//TG2SGQuery END

//TG2SGQueryItemLink BEGIN
procedure TG2SGQueryItemLink.Init;
begin
  Query.SceneGraph.Core.Graphics.Device.CreateQuery(
    D3DQUERYTYPE_OCCLUSION,
    OcclusionQuery
  );
  FetchID := 0;
  OcclusionQueryID := 0;
  OcclusionQueryEnabled := False;
  OcclusionVisible := True;
  OcclusionCheckTime := GetTickCount;
end;

procedure TG2SGQueryItemLink.UnInit;
begin
  SafeRelease(OcclusionQuery);
end;
//TG2SGQueryItemLink END

//TG2SGQueryNodeLink BEGIN
procedure TG2SGQueryNodeLink.Init;
begin
  Query.SceneGraph.Core.Graphics.Device.CreateQuery(
    D3DQUERYTYPE_OCCLUSION,
    OcclusionQuery
  );
  FetchID := 0;
  OcclusionQueryID := 0;
  OcclusionQueryEnabled := False;
  OcclusionVisible := True;
  OcclusionCheckTime := GetTickCount;
end;

procedure TG2SGQueryNodeLink.UnInit;
begin
  SafeRelease(OcclusionQuery);
end;
//TG2SGQueryNodeLink END

//TG2SceneGraph BEGIN
procedure TG2SceneGraph.ResetCollideID;
  var i, j: Integer;
begin
  m_CurCollideID := 0;
  for i := 0 to Geoms.Count - 1 do
  if TG2SGGeom(Geoms[i]).Collide then
  for j := 0 to TG2SGGeom(Geoms[i]).FCount - 1 do
  TG2SGGeom(Geoms[i]).Collider.Faces[j].CillideID := 0;
end;

procedure TG2SceneGraph.OnDeviceLost;
  var i, j: Integer;
begin
  for i := 0 to Queries.Count - 1 do
  PG2SGQuery(Queries[i])^.OcclusionRT.OnDeviceLost;
  for i := 0 to Frames.Count - 1 do
  for j := 0 to TG2SGFrame(Frames[i]).OcTreeItem.QueryLinks.Count - 1 do
  PG2SGQueryItemLink(TG2SGFrame(Frames[i]).OcTreeItem.QueryLinks[j])^.UnInit;
end;

procedure TG2SceneGraph.OnDeviceReset;
  var i, j: Integer;
begin
  for i := 0 to Queries.Count - 1 do
  PG2SGQuery(Queries[i])^.OcclusionRT.OnDeviceReset;
  for i := 0 to Frames.Count - 1 do
  for j := 0 to TG2SGFrame(Frames[i]).OcTreeItem.QueryLinks.Count - 1 do
  PG2SGQueryItemLink(TG2SGFrame(Frames[i]).OcTreeItem.QueryLinks[j])^.Init;
end;

function TG2SceneGraph.GetAABox: TG2AABox;
begin
  Result := RootNode.AABox;
end;

function TG2SceneGraph.OcTreeAutoBuild: Boolean;
  var i: Integer;
  var FrameAABox: TG2AABox;
  var bs: TG2Vec3;
  var AvFrameSize: TG2Vec3;
  var MaxFrameSize: Single;
  var MinDivSize: Single;
begin
  Result := False;
  if Frames.Count = 0 then Exit;
  FrameAABox := TG2SGFrame(Frames[0]).AABox;
  AvFrameSize := FrameAABox.MaxV - FrameAABox.MinV;
  for i := 1 to Frames.Count - 1 do
  begin
    FrameAABox := FrameAABox + TG2SGFrame(Frames[i]).AABox;
    AvFrameSize := AvFrameSize + (TG2SGFrame(Frames[i]).AABox.MaxV - TG2SGFrame(Frames[i]).AABox.MinV);
  end;
  bs := (FrameAABox.MaxV - FrameAABox.MinV) * 0.1;
  FrameAABox.SetValue(
    FrameAABox.MinV - bs,
    FrameAABox.MaxV + bs
  );
  if (FrameAABox.MinV.x < RootNode.AABox.MinV.x)
  or (FrameAABox.MinV.y < RootNode.AABox.MinV.y)
  or (FrameAABox.MinV.z < RootNode.AABox.MinV.z)
  or (FrameAABox.MaxV.x > RootNode.AABox.MaxV.x)
  or (FrameAABox.MaxV.y > RootNode.AABox.MaxV.y)
  or (FrameAABox.MaxV.z > RootNode.AABox.MaxV.z) then
  begin
    AvFrameSize := AvFrameSize * (2 / Frames.Count);
    MaxFrameSize := Max(
      Max(
        FrameAABox.MaxV.x - FrameAABox.MinV.x,
        FrameAABox.MaxV.y - FrameAABox.MinV.y
      ), FrameAABox.MaxV.z - FrameAABox.MinV.z
    );
    MinDivSize := Max(
      Max(
        Max(
          AvFrameSize.x,
          AvFrameSize.y
        ), AvFrameSize.z
      ), MaxFrameSize / 64
    );
    OcTreeBuild(FrameAABox, MinDivSize);
    for i := 0 to Frames.Count - 1 do
    begin
      TG2SGFrame(Frames[i]).OcTreeItem.OcTreeNodes.Clear;
      OcTreeAddItem(@TG2SGFrame(Frames[i]).OcTreeItem);
    end;
    Result := True;
  end;
end;

procedure TG2SceneGraph.OcTreeBuild(const AABox: TG2AABox; const MinDivSize: Single);
  var DivStopSize: Single;
  procedure BuildNode(const n: PG2SGOcTreeNode);
    var gx, gy, gz, sx, sy, sz: Single;
    var nx, ny, nz, i: Integer;
    var Link: PG2SGQueryNodeLink;
  begin
    n^.TotalItemCount := 0;
    sx := n^.AABox.MaxV.x - n^.AABox.MinV.x;
    sy := n^.AABox.MaxV.y - n^.AABox.MinV.y;
    sz := n^.AABox.MaxV.z - n^.AABox.MinV.z;
    gx := Min(sx / sy, sx / sz);
    gy := Min(sy / sx, sy / sz);
    gz := Min(sz / sx, sz / sy);
    n^.DivX := (not (gx < 0.5)) and (sx >= DivStopSize);
    n^.DivY := (not (gy < 0.5)) and (sy >= DivStopSize);
    n^.DivZ := (not (gz < 0.5)) and (sz >= DivStopSize);
    n^.DivN := not (n^.DivX or n^.DivY or n^.DivZ);
    n^.MinX := 0; n^.MaxX := 0;
    n^.MinY := 0; n^.MaxY := 0;
    n^.MinZ := 0; n^.MaxZ := 0;
    if n^.DivX then
    begin
      n^.MaxX := 1;
      sx := sx * 0.5;
    end;
    if n^.DivY then
    begin
      n^.MaxY := 1;
      sy := sy * 0.5;
    end;
    if n^.DivZ then
    begin
      n^.MaxZ := 1;
      sz := sz * 0.5;
    end;
    if n^.DivN then
    begin
      n^.Items.Capacity := 32;
      n^.Items.Clear;
      nx := Round(((n^.AABox.MinV.x - AABox.MinV.x) / (AABox.MaxV.x - AABox.MinV.x)) * Length(m_OcTreeGrid));
      ny := Round(((n^.AABox.MinV.y - AABox.MinV.y) / (AABox.MaxV.y - AABox.MinV.y)) * Length(m_OcTreeGrid[0]));
      nz := Round(((n^.AABox.MinV.z - AABox.MinV.z) / (AABox.MaxV.z - AABox.MinV.z)) * Length(m_OcTreeGrid[0, 0]));
      m_OcTreeGrid[nx, ny, nz] := n;
    end
    else
    begin
      SetLength(n^.SubNodes, n^.MaxX + 1, n^.MaxY + 1, n^.MaxZ + 1);
      for nx := n^.MinX to n^.MaxX do
      for ny := n^.MinY to n^.MaxY do
      for nz := n^.MinZ to n^.MaxZ do
      begin
        n^.SubNodes[nx, ny, nz].Parent := n;
        n^.SubNodes[nx, ny, nz].AABox.SetValue(
          n^.AABox.MinV + G2Vec3(sx * nx, sy * ny, sz * nz),
          n^.AABox.MinV + G2Vec3(sx * (nx + 1), sy * (ny + 1), sz * (nz + 1))
        );
        BuildNode(@n^.SubNodes[nx, ny, nz]);
      end;
    end;
    n^.QueryLinks.Clear;
    for i := 0 to Queries.Count - 1 do
    begin
      New(Link);
      Link^.Query := PG2SGQuery(Queries[i]);
      Link^.Init;
      n^.QueryLinks.Add(Link);
    end;
  end;
  var MaxDim: Single;
  var sx, sy, sz: Single;
  var DivX, DivY, DivZ: Integer;
begin
  OcTreeDestroy;
  DivStopSize := MinDivSize * 2;
  sx := AABox.MaxV.x - AABox.MinV.x;
  sy := AABox.MaxV.y - AABox.MinV.y;
  sz := AABox.MaxV.z - AABox.MinV.z;
  DivX := 1; while sx / DivX > DivStopSize do DivX := DivX * 2;
  DivY := 1; while sy / DivY > DivStopSize do DivY := DivY * 2;
  DivZ := 1; while sz / DivZ > DivStopSize do DivZ := DivZ * 2;
  SetLength(m_OcTreeGrid, DivX, DivY, DivZ);
  MaxDim := Max(Max(sx, sy), sz);
  Depth := 0;
  while MaxDim > DivStopSize do
  begin
    Inc(Depth);
    MaxDim := MaxDim * 0.5;
  end;
  RootNode.AABox := AABox;
  RootNode.DivX := False;
  RootNode.DivY := False;
  RootNode.DivZ := False;
  RootNode.DivN := True;
  RootNode.Parent := nil;
  BuildNode(@RootNode);
  OcTreeBuilt := True;
end;

procedure TG2SceneGraph.OcTreeDestroy;
  procedure DestroyNode(const n: PG2SGOcTreeNode);
    var nx, ny, nz, i: Integer;
  begin
    if not n^.DivN then
    for nx := n^.MinX to n^.MaxX do
    for ny := n^.MinY to n^.MaxY do
    for nz := n^.MinZ to n^.MaxZ do
    DestroyNode(@n^.SubNodes[nx, ny, nz]);
    for i := 0 to n^.QueryLinks.Count - 1 do
    begin
      PG2SGQueryNodeLink(n^.QueryLinks[i])^.UnInit;
      Dispose(PG2SGQueryNodeLink(n^.QueryLinks[i]));
    end;
  end;
begin
  if OcTreeBuilt then
  DestroyNode(@RootNode);
end;

procedure TG2SceneGraph.OcTreeAddItem(const Item: PG2SGOcTreeItem);
  var i: Integer;
begin
  OcTreeFetchNodes(Item^.Frame.AABox, @Item^.OcTreeNodes, True);
  for i := 0 to Item^.OcTreeNodes.Count - 1 do
  begin
    PG2SGOcTreeNode(Item^.OcTreeNodes[i])^.Items.Add(Item);
    PG2SGOcTreeNode(Item^.OcTreeNodes[i])^.TotalItemsInc();
  end;
end;

procedure TG2SceneGraph.OcTreeRemoveItem(const Item: PG2SGOcTreeItem);
  var i: Integer;
begin
  for i := 0 to Item^.OcTreeNodes.Count - 1 do
  begin
    PG2SGOcTreeNode(Item^.OcTreeNodes[i])^.Items.Remove(Item);
    PG2SGOcTreeNode(Item^.OcTreeNodes[i])^.TotalItemsDec();
  end;
  Item^.OcTreeNodes.Clear;
end;

procedure TG2SceneGraph.OcTreeFetchNodes(const AABox: TG2AABox; const List: PG2QuickList; const IncludeEmpty: Boolean = False);
  var xl, xh, yl, yh, zl, zh: Integer;
  var nx, ny, nz: Integer;
begin
  OcTreeGetIndices(AABox, xl, yl, zl, xh, yh, zh);
  for nx := xl to xh do
  for ny := yl to yh do
  for nz := zl to zh do
  if IncludeEmpty
  or (m_OcTreeGrid[nx, ny, nz]^.Items.Count > 0) then
  List^.Add(m_OcTreeGrid[nx, ny, nz]);
end;

procedure TG2SceneGraph.OcTreeFetchNodes(const Frustum: TG2Frustum; const Query: PG2SGQuery);
  procedure AddNode(const n: PG2SGOcTreeNode);
    var Link: PG2SGQueryNodeLink;
    var DataSize: DWord;
    var VisiblePixels: DWord;
  begin
    Query^.QueryNodes.Add(n);
    Link := PG2SGQueryNodeLink(n^.QueryLinks[Query^.ID]);
    Link^.OcclusionVisible := True;
    if Link^.OcclusionQueryEnabled then
    begin
      if
      (Link^.OcclusionQueryID >= Query^.FetchID - 1) then
      begin
        DataSize := Link^.OcclusionQuery.GetDataSize;
        while Link^.OcclusionQuery.GetData(@VisiblePixels, DataSize, D3DGETDATA_FLUSH) = S_FALSE do ;
        if VisiblePixels <= 1 then
        begin
          Link^.OcclusionVisible := False;
          Link^.OcclusionCheckTime := GetTickCount - Query^.OcclusionCheckTimeBias;
        end;
      end;
      Link^.OcclusionQueryEnabled := False;
    end;
  end;
  procedure IncludeNode(const n: PG2SGOcTreeNode);
    var nx, ny, nz: Integer;
  begin
    PG2SGQueryNodeLink(n^.QueryLinks[Query^.ID])^.FrustumTest := fcInside;
    if n^.DivN then
    begin
      if n^.Items.Count > 0 then
      AddNode(n);
    end
    else
    for nx := n^.MinX to n^.MaxX do
    for ny := n^.MinY to n^.MaxY do
    for nz := n^.MinZ to n^.MaxZ do
    if n^.SubNodes[nx, ny, nz].TotalItemCount > 0 then
    IncludeNode(@n^.SubNodes[nx, ny, nz]);
  end;
  procedure CheckNode(const n: PG2SGOcTreeNode);
    var nx, ny, nz: Integer;
  begin
    PG2SGQueryNodeLink(n^.QueryLinks[Query^.ID])^.FrustumTest := Frustum.FrustumCheckBox(n^.AABox.MinV, n^.AABox.MaxV);
    if (PG2SGQueryNodeLink(n^.QueryLinks[Query^.ID])^.FrustumTest = fcInside) then
    IncludeNode(n)
    else
    if (PG2SGQueryNodeLink(n^.QueryLinks[Query^.ID])^.FrustumTest = fcIntersect) then
    begin
      if n^.DivN then
      begin
        if n^.Items.Count > 0 then
        AddNode(n);
      end
      else
      for nx := n^.MinX to n^.MaxX do
      for ny := n^.MinY to n^.MaxY do
      for nz := n^.MinZ to n^.MaxZ do
      if n^.SubNodes[nx, ny, nz].TotalItemCount > 0 then
      CheckNode(@n^.SubNodes[nx, ny, nz]);
    end;
  end;
begin
  if RootNode.TotalItemCount > 0 then
  CheckNode(@RootNode);
end;

procedure TG2SceneGraph.OcTreeGetIndices(const AABox: TG2AABox; var IndMinX, IndMinY, IndMinZ, IndMaxX, IndMaxY, IndMaxZ: Integer);
  var sx, sy, sz: Single;
begin
  sx := RootNode.AABox.MaxV.x - RootNode.AABox.MinV.x;
  sy := RootNode.AABox.MaxV.y - RootNode.AABox.MinV.y;
  sz := RootNode.AABox.MaxV.z - RootNode.AABox.MinV.z;
  IndMinX := Min(Max(Trunc((AABox.MinV.x - RootNode.AABox.MinV.x) / sx * Length(m_OcTreeGrid)), 0), Length(m_OcTreeGrid));
  IndMaxX := Min(Max(Trunc((AABox.MaxV.x - RootNode.AABox.MinV.x) / sx * Length(m_OcTreeGrid)), -1), High(m_OcTreeGrid));
  IndMinY := Min(Max(Trunc((AABox.MinV.y - RootNode.AABox.MinV.y) / sy * Length(m_OcTreeGrid[0])), 0), Length(m_OcTreeGrid[0]));
  IndMaxY := Min(Max(Trunc((AABox.MaxV.y - RootNode.AABox.MinV.y) / sy * Length(m_OcTreeGrid[0])), -1), High(m_OcTreeGrid[0]));
  IndMinZ := Min(Max(Trunc((AABox.MinV.z - RootNode.AABox.MinV.z) / sz * Length(m_OcTreeGrid[0, 0])), 0), Length(m_OcTreeGrid[0, 0]));
  IndMaxZ := Min(Max(Trunc((AABox.MaxV.z - RootNode.AABox.MinV.z) / sz * Length(m_OcTreeGrid[0, 0])), -1), High(m_OcTreeGrid[0, 0]));
end;

constructor TG2SceneGraph.Create(const G2Core: TG2Core);
  type TVertexArr = array[Word] of TG2Vec3;
  type PVertexArr = ^TVertexArr;
  var i, j: Integer;
  var PtrVertices: PVertexArr;
  var PtrIndices: PG2Index16Array;
begin
  inherited Create;
  Core := G2Core;
  Queries.Capacity := 32;
  Queries.Clear;
  Materials.Capacity := 32;
  Materials.Clear;
  Nodes.Capacity := 32;
  Nodes.Clear;
  Frames.Capacity := 32;
  Frames.Clear;
  Geoms.Capacity := 32;
  Geoms.Clear;
  Chars.Capacity := 32;
  Chars.Clear;
  Lights.Capacity := 32;
  Lights.Clear;
  OcTreeBuilt := False;
  m_OcTreeNodeFetch.Capacity := 32;
  m_OcTreeNodeFetch.Clear;
  m_GeomCollideList.Capacity := 32;
  m_GeomCollideList.Clear;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  DataFormats.FVFGeom[0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  DataFormats.FVFGeom[1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
  DataFormats.FVFGeom[2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
  DataFormats.FVFGeom[3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
  DataFormats.FVFGeom[4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  DataFormats.FVFGeom[5] := D3DVertexElement(0, 4 * 14, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 1);
  DataFormats.FVFGeom[6] := D3DVertexElement(0, 4 * 16, D3DDECLTYPE_D3DCOLOR, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0);
  DataFormats.FVFGeom[7] := D3DDECL_END;
  DataFormats.DefClassOfGeom := TG2SGGeom;
  DataFormats.DefClassOfChar := TG2SGChar;
  DataFormats.DefClassOfLightPoint := TG2SGLightPoint;
  DataFormats.DefClassOfLightSpot := TG2SGLightSpot;
  DataFormats.DefClassOfLightDir := TG2SGLightDir;
  Core.Graphics.Device.CreateVertexDeclaration(@DataFormats.FVFGeom, DataFormats.DeclGeom);
  VB := TG2VB.Create;
  VB.Initialize(Core);
  VB.Verify(SizeOf(TG2Vec3), 8, 0, D3DFVF_XYZ, D3DPOOL_MANAGED);
  VB.Lock(0, SizeOf(TG2Vec3) * 8, Pointer(PtrVertices));
  PtrVertices^[0].SetValue(0, 0, 0);
  PtrVertices^[1].SetValue(0, 0, 1);
  PtrVertices^[2].SetValue(1, 0, 1);
  PtrVertices^[3].SetValue(1, 0, 0);
  PtrVertices^[4].SetValue(0, 1, 0);
  PtrVertices^[5].SetValue(0, 1, 1);
  PtrVertices^[6].SetValue(1, 1, 1);
  PtrVertices^[7].SetValue(1, 1, 0);
  VB.UnLock;
  IB := TG2IB.Create;
  IB.Initialize(Core);
  IB.Verify(36, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED);
  IB.Lock(0, 36, Pointer(PtrIndices));
  PtrIndices^[0] := 1; PtrIndices^[1] := 0; PtrIndices^[2] := 3;
  PtrIndices^[3] := 1; PtrIndices^[4] := 3; PtrIndices^[5] := 2;

  PtrIndices^[6] := 0; PtrIndices^[7] := 4; PtrIndices^[8] := 7;
  PtrIndices^[9] := 0; PtrIndices^[10] := 7; PtrIndices^[11] := 3;

  PtrIndices^[12] := 1; PtrIndices^[13] := 5; PtrIndices^[14] := 4;
  PtrIndices^[15] := 1; PtrIndices^[16] := 4; PtrIndices^[17] := 0;

  PtrIndices^[18] := 3; PtrIndices^[19] := 7; PtrIndices^[20] := 6;
  PtrIndices^[21] := 3; PtrIndices^[22] := 6; PtrIndices^[23] := 2;

  PtrIndices^[24] := 2; PtrIndices^[25] := 6; PtrIndices^[26] := 5;
  PtrIndices^[27] := 2; PtrIndices^[28] := 5; PtrIndices^[29] := 1;

  PtrIndices^[30] := 4; PtrIndices^[31] := 5; PtrIndices^[32] := 6;
  PtrIndices^[33] := 4; PtrIndices^[34] := 6; PtrIndices^[35] := 7;
  IB.UnLock;
  MgrTextures := TG2TextureMgr.Create;
  MgrTextures.Initialize(Core);
  with MgrTextures.CreateTexture2D('NULL_DIFFUSE', 4, 4, 1, 0, D3DFMT_A8R8G8B8) do
  begin
    Surfaces[0].Lock(D3DLOCK_DISCARD);
    for j := 0 to 3 do
    for i := 0 to 3 do
    Surfaces[0].Pixels[i, j] := $ffffffff;
    Surfaces[0].UnLock;
  end;
  with MgrTextures.CreateTexture2D('NULL_SPECULAR', 4, 4, 1, 0, D3DFMT_A8R8G8B8) do
  begin
    Surfaces[0].Lock(D3DLOCK_DISCARD);
    for j := 0 to 3 do
    for i := 0 to 3 do
    Surfaces[0].Pixels[i, j] := $ff808080;
    Surfaces[0].UnLock;
  end;
  with MgrTextures.CreateTexture2D('NULL_NORMALS', 4, 4, 1, 0, D3DFMT_A8R8G8B8) do
  begin
    Surfaces[0].Lock(D3DLOCK_DISCARD);
    for j := 0 to 3 do
    for i := 0 to 3 do
    Surfaces[0].Pixels[i, j] := $ff8080ff;
    Surfaces[0].UnLock;
  end;
  with MgrTextures.CreateTexture2D('NULL_LIGHT', 4, 4, 1, 0, D3DFMT_A8R8G8B8) do
  begin
    Surfaces[0].Lock(D3DLOCK_DISCARD);
    for j := 0 to 3 do
    for i := 0 to 3 do
    Surfaces[0].Pixels[i, j] := $ffffffff;
    Surfaces[0].UnLock;
  end;
  MgrCharMeshes := TG2MeshMgr.Create;
  MgrCharMeshes.Initialize(Core);
  m_Prim3D := TG2Primitives3D.Create;
  m_Prim3D.Initialize(Core);
  SearchPathAdd(String(AppPath));
  RootNode.AABox.SetValue(G2Vec3(0, 0, 0), G2Vec3(0, 0, 0));
  Depth := 0;
  CS := TCriticalSection.Create;
end;

destructor TG2SceneGraph.Destroy;
begin
  CS.Free;
  while Queries.Count > 0 do
  QueryDestroy(PG2SGQuery(Queries[0]));
  Clear;
  MaterialsClear;
  OcTreeDestroy;
  IB.Finalize;
  IB.Free;
  VB.Finalize;
  VB.Free;
  m_Prim3D.Finalize;
  m_Prim3D.Free;
  SafeRelease(DataFormats.DeclGeom);
  Core.ReleasePlug(@m_PlugGraphics);
  MgrCharMeshes.Finalize;
  MgrCharMeshes.Free;
  MgrTextures.Finalize;
  MgrTextures.Free;
  inherited Destroy;
end;

procedure TG2SceneGraph.SearchPathAdd(const Dir: String);
  var DirStr: String;
begin
  if (Length(Dir) = 0) then Exit;
  DirStr := Dir;
  if (Dir[Length(Dir)] = '\') then
  Delete(DirStr, Length(DirStr), 1);
  SetLength(SearchPaths, Length(SearchPaths) + 1);
  SearchPaths[High(SearchPaths)] := DirStr;
end;

procedure TG2SceneGraph.SearchPathsClear;
begin
  SearchPaths := nil;
end;

function TG2SceneGraph.FindTexture(
      const Name: WideString;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
  var i: Integer;
  var SearchStr: WideString;
  var NameNoExt: WideString;
  var Ext: WideString;
begin
  Result := TG2Texture2D(MgrTextures.FindTexture(Name));
  if Assigned(Result) then Exit;
  for i := 0 to High(SearchPaths) do
  begin
    SearchStr := SearchPaths[i] + '\' + Name;
    if FileExists(SearchStr) then
    begin
      Result := MgrTextures.CreateTexture2DFromFile(Name, SearchStr, MipLevels, Format);
      if Assigned(Result) then Exit;
    end;
  end;
  Ext := ExtractFileExt(Name);
  NameNoExt := Name;
  Delete(NameNoExt, Length(NameNoExt) - Length(Ext) + 1, Length(Ext));
  for i := 0 to High(SearchPaths) do
  begin
    Result := MgrTextures.CreateTexture2DFromPack(Name, AnsiString(SearchPaths[i]), AnsiString(NameNoExt), MipLevels, Format);
    if Assigned(Result) then Exit;
  end;
  Result := nil;
end;

function TG2SceneGraph.FindTextureDiffuse(const Name: WideString): TG2Texture2D;
begin
  if Length(Name) > 0 then
  begin
    Result := FindTexture(Name);
    if not Assigned(Result) then
    begin
      if Length(Name) > 0 then
      G2WriteLogTimed(AnsiString('(W) Missing texture: ' + Name), 'SceneGraph');
      Result := TG2Texture2D(MgrTextures.FindTexture('NULL_DIFFUSE'));
    end;
  end
  else
  Result := TG2Texture2D(MgrTextures.FindTexture('NULL_DIFFUSE'));
end;

function TG2SceneGraph.FindTextureSpecular(const Name: WideString): TG2Texture2D;
begin
  if Length(Name) > 0 then
  begin
    Result := FindTexture(Name);
    if not Assigned(Result) then
    begin
      if Length(Name) > 0 then
      G2WriteLogTimed(AnsiString('(W) Missing texture: ' + Name), 'SceneGraph');
      Result := TG2Texture2D(MgrTextures.FindTexture('NULL_SPECULAR'));
    end;
  end
  else
  Result := TG2Texture2D(MgrTextures.FindTexture('NULL_SPECULAR'));
end;

function TG2SceneGraph.FindTextureNormals(const Name: WideString): TG2Texture2D;
begin
  if Length(Name) > 0 then
  begin
    Result := FindTexture(Name);
    if not Assigned(Result) then
    begin
      if Length(Name) > 0 then
      G2WriteLogTimed(AnsiString('(W) Missing texture: ' + Name), 'SceneGraph');
      Result := TG2Texture2D(MgrTextures.FindTexture('NULL_NORMALS'));
    end;
  end
  else
  Result := TG2Texture2D(MgrTextures.FindTexture('NULL_NORMALS'));
end;

function TG2SceneGraph.FindTextureLightMap(const Name: WideString): TG2Texture2D;
begin
  if Length(Name) > 0 then
  begin
    Result := FindTexture(Name, 4, D3DFMT_DXT1);
    if not Assigned(Result) then
    begin
      if Length(Name) > 0 then
      G2WriteLogTimed(AnsiString('(W) Missing texture: ' + Name), 'SceneGraph');
      Result := TG2Texture2D(MgrTextures.FindTexture('NULL_LIGHT'));
    end;
  end
  else
  Result := TG2Texture2D(MgrTextures.FindTexture('NULL_LIGHT'));
end;

function TG2SceneGraph.FindNode(const Name: AnsiString): TG2SGNode;
  var i: Integer;
begin
  for i := 0 to Nodes.Count - 1 do
  if TG2SGNode(Nodes[i]).Name = Name then
  begin
    Result := TG2SGNode(Nodes[i]);
    Exit;
  end;
  Result := nil;
end;

function TG2SceneGraph.MaterialAdd: PG2SGMaterial;
begin
  New(Result);
  Materials.Add(Result);
end;

procedure TG2SceneGraph.MaterialsClear;
  var i: Integer;
begin
  for i := 0 to Materials.Count - 1 do
  Dispose(PG2SGMaterial(Materials[i]));
  Materials.Clear;
end;

function TG2SceneGraph.QueryCreate: PG2SGQuery;
  var i: Integer;
begin
  New(Result);
  Result^.SceneGraph := Self;
  i := Queries.Count;
  Queries.Add(Result);
  Result^.ID := i;
  Result^.Init;
end;

procedure TG2SceneGraph.QueryDestroy(const q: PG2SGQuery);
  var i: Integer;
begin
  Queries.Remove(q);
  q^.UnInit;
  Dispose(q);
  for i := 0 to Queries.Count - 1 do
  PG2SGQuery(Queries[i])^.ID := i;
end;

procedure TG2SceneGraph.QueryFetch(const Frustum: TG2Frustum; const Query: PG2SGQuery);
  var i, j: Integer;
  var Frame: TG2SGFrame;
  var Link: PG2SGQueryItemLink;
  var VisiblePixels: DWord;
  var DataSize: DWord;
begin
  Query.QueryNodes.Clear;
  OcTreeFetchNodes(Frustum, Query);
  Query.CheckFetchID;
  Inc(Query.FetchID);
  Query^.QueryLists[G2QL_CHARS].Clear;
  Query^.QueryLists[G2QL_LIGHTS].Clear;
  Query^.QueryLists[G2QL_GEOMS].Clear;
  Query^.QueryGeoms.Clear;

  Query^.Stats.ObjectsRendered := 0;
  for i := 0 to Query.QueryNodes.Count - 1 do
  if PG2SGQueryNodeLink(PG2SGOcTreeNode(Query.QueryNodes[i])^.QueryLinks[Query^.ID])^.OcclusionVisible then
  for j := 0 to PG2SGOcTreeNode(Query.QueryNodes[i])^.Items.Count - 1 do
  if PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(Query.QueryNodes[i])^.Items[j])^.QueryLinks[Query.ID])^.FetchID < Query.FetchID then
  begin
    PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(Query.QueryNodes[i])^.Items[j])^.QueryLinks[Query.ID])^.FetchID := Query.FetchID;
    Frame := PG2SGOcTreeItem(PG2SGOcTreeNode(Query.QueryNodes[i])^.Items[j])^.Frame;
    if Frame.Render
    and (
      (PG2SGQueryNodeLink(PG2SGOcTreeNode(Query.QueryNodes[i])^.QueryLinks[Query^.ID])^.FrustumTest = fcInside)
      or Frustum.BoxInFrustum(Frame.AABox)
    ) then
    begin
      if (Frame is TG2SGGeom) then
      begin
        Link := PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(Query.QueryNodes[i])^.Items[j])^.QueryLinks[Query.ID]);
        Link^.OcclusionVisible := True;
        Query^.QueryGeoms.Add(Frame);
        if Link^.OcclusionQueryEnabled then
        begin
          if Link^.OcclusionQueryID >= Query^.FetchID - 1 then
          begin
            DataSize := Link^.OcclusionQuery.GetDataSize;
            while Link^.OcclusionQuery.GetData(@VisiblePixels, DataSize, D3DGETDATA_FLUSH) = S_FALSE do ;
            if VisiblePixels <= 1 then
            begin
              Link^.OcclusionVisible := False;
              Link^.OcclusionCheckTime := GetTickCount - Query^.OcclusionCheckTimeBias;
            end;
          end;
          Link^.OcclusionQueryEnabled := False;
        end;
        if Link^.OcclusionVisible then
        begin
          if Query^.DistanceSortEnable then
          Query^.QueryLists[Frame.QueryListID].Add(Frame, Frustum.Planes[4].DistanceToPoint((Frame.AABox.MaxV + Frame.AABox.MinV) * 0.5))
          else
          Query^.QueryLists[Frame.QueryListID].Add(Frame);
        end;
      end
      else
      begin
        if Query^.DistanceSortEnable then
        Query^.QueryLists[Frame.QueryListID].Add(Frame, Frustum.Planes[4].DistanceToPoint((Frame.AABox.MaxV + Frame.AABox.MinV) * 0.5))
        else
        Query^.QueryLists[Frame.QueryListID].Add(Frame);
      end;
    end;
  end;
  Query^.Stats.ObjectsRendered := Query^.QueryLists[G2QL_GEOMS].Count + Query^.QueryLists[G2QL_CHARS].Count + Query^.QueryLists[G2QL_LIGHTS].Count;
  Query^.Stats.ObjectsCulled := Geoms.Count + Chars.Count + Lights.Count - Query^.Stats.ObjectsRendered;
end;

function TG2SceneGraph.CollideSphere(var s: TG2Sphere; const Query: PG2SGQuery): Boolean;
  var AABox: TG2AABox;
  var Geom: TG2SGGeom;
  var ShiftVec: TG2Vec3;
  var i, j, k: Integer;
  var d: Single;
  var n, t: TG2Vec3;
  var InSegment: Boolean;
begin
  Result := False;
  AABox.MinV := s.C - s.R;
  AABox.MaxV := s.C + s.R;
  m_OcTreeNodeFetch.Clear;
  OcTreeFetchNodes(AABox, @m_OcTreeNodeFetch);
  if m_OcTreeNodeFetch.Count = 0 then Exit;
  if m_CurCollideID > High(DWord) - 1 then
  ResetCollideID;
  Inc(m_CurCollideID);
  Query.CheckFetchID;
  Inc(Query.FetchID);
  m_GeomCollideList.Clear;
  for i := 0 to m_OcTreeNodeFetch.Count - 1 do
  for j := 0 to PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items.Count - 1 do
  if (PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame is TG2SGGeom)
  and (TG2SGGeom(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame).Collide)
  and (PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.QueryLinks[Query.ID])^.FetchID < Query.FetchID) then
  begin
    PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.QueryLinks[Query.ID])^.FetchID := Query.FetchID;
    if AABox.Intersect(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame.AABox) then
    m_GeomCollideList.Add(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame);
  end;
  for i := 0 to m_GeomCollideList.Count - 1 do
  begin
    Geom := TG2SGGeom(m_GeomCollideList[i]);
    for j := 0 to Geom.FCount - 1 do
    begin
      if AABox.Intersect(Geom.Collider.Faces[j].AABox) then
      begin
        if (Geom.Collider.Faces[j].CillideID <> m_CurCollideID)
        and s.C.InTriangle(
          Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[0]],
          Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[1]],
          Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[2]]
        ) then
        begin
          Geom.Collider.Faces[j].CillideID := m_CurCollideID;
          d := Abs(Geom.Collider.Faces[j].Plane.DistanceToPoint(s.C));
          if d < s.R then
          begin
            t := Geom.Collider.Faces[j].Plane.Project(s.C);
            n := (s.C - t).Normalized;
            ShiftVec := (t + (n * s.R)) - s.C;
            s.C := s.C + ShiftVec;
            AABox.MinV := AABox.MinV + ShiftVec;
            AABox.MaxV := AABox.MaxV + ShiftVec;
            Result := True;
          end;
        end;
      end
      else
      Geom.Collider.Faces[j].CillideID := m_CurCollideID;
    end;
  end;
  for i := 0 to m_GeomCollideList.Count - 1 do
  begin
    Geom := TG2SGGeom(m_GeomCollideList[i]);
    for j := 0 to Geom.FCount - 1 do
    if Geom.Collider.Faces[j].CillideID <> m_CurCollideID then
    for k := 0 to 2 do
    begin
      d := G2Vec3ToLine(
        Geom.Collider.Vertices[Geom.Collider.Faces[j].Edges[k].Indices[0]],
        Geom.Collider.Vertices[Geom.Collider.Faces[j].Edges[k].Indices[1]],
        s.C, t, InSegment
      );
      if InSegment
      and (d < s.R)
      and (Geom.Collider.Faces[j].Edges[k].N.Dot((s.C - t).Normalized) > 0) then
      begin
        Geom.Collider.Faces[j].CillideID := m_CurCollideID;
        n := (s.C - t).Normalized;
        s.C := t + (n * s.R);
        Result := True;
        Break;
      end;
    end;
  end;
  for i := 0 to m_GeomCollideList.Count - 1 do
  begin
    Geom := TG2SGGeom(m_GeomCollideList[i]);
    for j := 0 to Geom.FCount - 1 do
    if Geom.Collider.Faces[j].CillideID <> m_CurCollideID then
    begin
      for k := 0 to 2 do
      begin
        n := s.C - Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[k]];
        d := n.Len;
        if d < s.R then
        begin
          n.Normalize;
          s.C := Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[k]] + (n * s.R);
          Result := True;
        end;
      end;
    end;
  end;
end;

function TG2SceneGraph.CollideChar(const c: TG2SGChar; const Query: PG2SGQuery): Boolean;
  var s: TG2Sphere;
  var AABoxChar, AABoxTop: TG2AABox;
  var StepNorm: TG2Vec3;
  var StepLen, StepLen2: Single;
  var ShiftVec: TG2Vec3;
  var Geom: TG2SGGeom;
  var i, j, k: Integer;
  var TriU, TriV, d: Single;
  var n, t, sv: TG2Vec3;
  var InSegment: Boolean;
  var PrevGrounded: Boolean;
begin
  Result := False;
  s.SetValue(c.Transform.GetTranslation - c.Step, c.Radius);
  AABoxTop.MinV := s.C - s.R;
  AABoxTop.MaxV := s.C + s.R;
  AABoxChar := AABoxTop + (s.C + (c.Step * 1.5));
  StepNorm := c.Step.Normalized;
  StepLen := c.Step.Len;
  StepLen2 := StepLen * 1.5;
  PrevGrounded := c.Grounded;
  c.Grounded := False;
  sv.SetValue(0, 0, 0);
  m_OcTreeNodeFetch.Clear;
  OcTreeFetchNodes(AABoxChar, @m_OcTreeNodeFetch);
  if m_OcTreeNodeFetch.Count = 0 then Exit;
  if m_CurCollideID > High(DWord) - 1 then
  ResetCollideID;
  Inc(m_CurCollideID);
  Query.CheckFetchID;
  Inc(Query.FetchID);
  m_GeomCollideList.Clear;
  for i := 0 to m_OcTreeNodeFetch.Count - 1 do
  for j := 0 to PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items.Count - 1 do
  if (PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame is TG2SGGeom)
  and (TG2SGGeom(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame).Collide)
  and (PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.QueryLinks[Query.ID])^.FetchID < Query.FetchID) then
  begin
    PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.QueryLinks[Query.ID])^.FetchID := Query.FetchID;
    if AABoxChar.Intersect(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame.AABox) then
    m_GeomCollideList.Add(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame);
  end;
  for i := 0 to m_GeomCollideList.Count - 1 do
  begin
    Geom := TG2SGGeom(m_GeomCollideList[i]);
    for j := 0 to Geom.FCount - 1 do
    begin
      if AABoxChar.Intersect(Geom.Collider.Faces[j].AABox) then
      begin
        if Geom.Collider.Faces[j].Plane.N.Dot(StepNorm) < -0.8 then
        begin
          if G2Ray(s.C, StepNorm).IntersectTri(
            Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[0]],
            Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[1]],
            Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[2]],
            TriU, TriV, d
          ) then
          begin
            if d < StepLen2 then
            begin
              c.Grounded := PrevGrounded;
              if d < StepLen then
              begin
                c.Grounded := True;
                c.Vel := c.Vel - StepNorm * (StepNorm.Dot(c.Vel));
                ShiftVec := -StepNorm * ((StepLen - d) * c.StepHardness);
                s.C := s.C + ShiftVec;
                AABoxTop.MinV := AABoxTop.MinV + ShiftVec;
                AABoxTop.MaxV := AABoxTop.MaxV + ShiftVec;
                AABoxChar.MinV := AABoxChar.MinV + ShiftVec;
                AABoxChar.MaxV := AABoxChar.MaxV + ShiftVec;
                Geom.Collider.Faces[j].CillideID := m_CurCollideID;
                Result := True;
              end;
            end;
          end;
        end;
        if (Geom.Collider.Faces[j].CillideID <> m_CurCollideID)
        and (AABoxTop.Intersect(Geom.Collider.Faces[j].AABox))
        and s.C.InTriangle(
          Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[0]],
          Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[1]],
          Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[2]]
        ) then
        begin
          Geom.Collider.Faces[j].CillideID := m_CurCollideID;
          d := Abs(Geom.Collider.Faces[j].Plane.DistanceToPoint(s.C));
          if d < s.R then
          begin
            t := Geom.Collider.Faces[j].Plane.Project(s.C);
            n := (s.C - t).Normalized;
            sv := sv + n;
            ShiftVec := (t + n * s.R) - s.C;
            s.C := s.C + ShiftVec;
            AABoxTop.MinV := AABoxTop.MinV + ShiftVec;
            AABoxTop.MaxV := AABoxTop.MaxV + ShiftVec;
            AABoxChar.MinV := AABoxChar.MinV + ShiftVec;
            AABoxChar.MaxV := AABoxChar.MaxV + ShiftVec;
            Result := True;
          end;
        end;
      end
      else
      Geom.Collider.Faces[j].CillideID := m_CurCollideID;
    end;
  end;
  for i := 0 to m_GeomCollideList.Count - 1 do
  begin
    Geom := TG2SGGeom(m_GeomCollideList[i]);
    for j := 0 to Geom.FCount - 1 do
    if Geom.Collider.Faces[j].CillideID <> m_CurCollideID then
    for k := 0 to 2 do
    begin
      d := G2Vec3ToLine(
        Geom.Collider.Vertices[Geom.Collider.Faces[j].Edges[k].Indices[0]],
        Geom.Collider.Vertices[Geom.Collider.Faces[j].Edges[k].Indices[1]],
        s.C, t, InSegment
      );
      if InSegment
      and (d < s.R)
      and (Geom.Collider.Faces[j].Edges[k].N.Dot((s.C - t).Normalized) > 0) then
      begin
        Geom.Collider.Faces[j].CillideID := m_CurCollideID;
        n := (s.C - t).Normalized;
        sv := sv + n;
        s.C := t + (n * s.R);
        Result := True;
        Break;
      end;
    end;
  end;
  for i := 0 to m_GeomCollideList.Count - 1 do
  begin
    Geom := TG2SGGeom(m_GeomCollideList[i]);
    for j := 0 to Geom.FCount - 1 do
    if Geom.Collider.Faces[j].CillideID <> m_CurCollideID then
    begin
      for k := 0 to 2 do
      begin
        n := s.C - Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[k]];
        d := n.Len;
        if d < s.R then
        begin
          n.Normalize;
          sv := sv + n;
          s.C := Geom.Collider.Vertices[Geom.Collider.Faces[j].Indices[k]] + (n * s.R);
          Result := True;
        end;
      end;
    end;
  end;
  if Result then
  begin
    sv.Normalize;
    c.Vel := c.Vel - sv * sv.Dot(c.Vel);
    PG2Vec3(@c.Transform.e30)^ := s.C + c.Step;
  end;
end;

function TG2SceneGraph.CollideRay(const r: TG2Ray; var GeomID: Integer; var FaceID: Word; var U, V, D: Single): Boolean;
  var i: Integer;
  var CurFaceID: Word;
  var FirstHit: Boolean;
  var CurU, CurV, CurD: Single;
begin
  Result := False;
  FirstHit := True;
  for i := 0 to Geoms.Count - 1 do
  if TG2SGGeom(Geoms[i]).Collide then
  begin
    if TG2SGGeom(Geoms[i]).IntersectRay(r, CurFaceID, CurU, CurV, CurD) then
    begin
      if FirstHit or (CurD < D) then
      begin
        Result := True;
        FirstHit := False;
        FaceID := CurFaceID;
        U := CurU;
        V := CurV;
        D := CurD;
        GeomID := i;
      end;
    end;
  end;
end;

function TG2SceneGraph.CollideRay(const r: TG2Ray; const Query: PG2SGQuery; const Distance: Single; var GeomID: Integer; var FaceID: Word; var U, V, D: Single): Boolean;
  var i, j: Integer;
  var CurFaceID: Word;
  var FirstHit: Boolean;
  var CurU, CurV, CurD: Single;
  var AABox: TG2AABox;
begin
  Result := False;
  FirstHit := True;
  AABox.MinV := r.Origin;
  AABox.MaxV := r.Origin;
  AABox := AABox + (r.Origin + r.Dir * Distance);
  m_OcTreeNodeFetch.Clear;
  OcTreeFetchNodes(AABox, @m_OcTreeNodeFetch);
  Query.CheckFetchID;
  Inc(Query.FetchID);
  for i := 0 to m_OcTreeNodeFetch.Count - 1 do
  for j := 0 to PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items.Count - 1 do
  if PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j]).QueryLinks[Query.ID]).FetchID < Query.FetchID then
  begin
    PG2SGQueryItemLink(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j]).QueryLinks[Query.ID]).FetchID := Query.FetchID;
    if PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j]).Frame is TG2SGGeom then
    if TG2SGGeom(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j]).Frame).Collide then
    begin
      if TG2SGGeom(PG2SGOcTreeItem(PG2SGOcTreeNode(m_OcTreeNodeFetch[i])^.Items[j]).Frame).IntersectRay(r, CurFaceID, CurU, CurV, CurD) then
      begin
        if FirstHit or (CurD < D) then
        begin
          Result := True;
          FirstHit := False;
          FaceID := CurFaceID;
          U := CurU;
          V := CurV;
          D := CurD;
          GeomID := i;
        end;
      end;
    end;
  end;
end;

procedure TG2SceneGraph.LoadG2M(const Loader: TG2MeshLoaderG2M);
  procedure ComputeNodeTransform(const n: TG2SGNode);
    var i: Integer;
  begin
    if Assigned(n.Parent) then
    n.Transform := n.Transform * n.Parent.Transform;
    if n is TG2SGFrame then
    TG2SGFrame(n).AABox := TG2SGFrame(n).GetAABox;
    for i := 0 to High(n.Children) do
    ComputeNodeTransform(n.Children[i]);
  end;
  var MeshData: TG2MeshData;
  var NodeRef: array of TG2SGNode;
  var TopNodes: array of TG2SGNode;
  var MatRef: array of PG2SGMaterial;
  var i, j: Integer;
  var Node: TG2SGNode;
  var Geom: TG2SGGeom;
  var LightPoint: TG2SGLightPoint;
  var LightSpot: TG2SGLightSpot;
  var LightDir: TG2SGLightDir;
  var Material: PG2SGMaterial;
  var TmpMesh: ID3DXMesh;
  var Vertices: PG2SGVertexGeomArr;
  var Indices: PG2Index16Array;
  var Attribs: PDWordArray;
  var AttribTable: array of TD3DXAttributeRange;
  var Adj: array of DWord;
  var Ptr1, Ptr2: Pointer;
begin
  Loader.ExportMesh(Core.Graphics.Device, @MeshData);
  SetLength(NodeRef, MeshData.NodeCount);
  FillChar(NodeRef[0], Length(NodeRef) * 4, 0);
  SetLength(MatRef, MeshData.MaterialCount);
  FillChar(MatRef[0], Length(MatRef) * 4, 0);
  for i := 0 to MeshData.MaterialCount - 1 do
  begin
    Material := MaterialAdd;
    Material^.ChannelCount := MeshData.Materials[i].ChannelCount;
    SetLength(Material^.Channels, Material^.ChannelCount);
    for j := 0 to Material^.ChannelCount - 1 do
    begin
      Material^.Channels[j].Name := MeshData.Materials[i].Channels[j].Name;
      Material^.Channels[j].TwoSided := MeshData.Materials[i].Channels[j].TwoSided;
      Material^.Channels[j].TexDiffuse := FindTextureDiffuse(WideString(MeshData.Materials[i].Channels[j].DiffuseMap));
      Material^.Channels[j].TexNormals := FindTextureNormals(WideString(MeshData.Materials[i].Channels[j].NormalMap));
      Material^.Channels[j].TexSpecular := FindTextureSpecular(WideString(MeshData.Materials[i].Channels[j].SpecularMap));
      Material^.Channels[j].TexLightMap := FindTextureLightMap(WideString(MeshData.Materials[i].Channels[j].LightMap));
    end;
    MatRef[i] := Material;
  end;
  for i := 0 to MeshData.GeomCount - 1 do
  begin
    Geom := DataFormats.DefClassOfGeom.Create(Self);
    NodeRef[MeshData.Geoms[i].NodeID] := Geom;
    Geom.VCount := MeshData.Geoms[i].VCount;
    Geom.FCount := MeshData.Geoms[i].FCount;
    D3DXCreateMesh(
      Geom.FCount,
      Geom.VCount,
      D3DXMESH_SYSTEMMEM,
      @DataFormats.FVFGeom,
      Core.Graphics.Device,
      TmpMesh
    );
    Geom.AABox.MinV := MeshData.Geoms[i].Vertices[0].Position;
    Geom. AABox.MaxV := Geom.AABox.MinV;
    TmpMesh.LockVertexBuffer(D3DLOCK_DISCARD, Pointer(Vertices));
    for j := 0 to Geom.VCount - 1 do
    begin
      Geom.AABox := Geom.AABox + MeshData.Geoms[i].Vertices[j].Position;
      Vertices^[j].Position := MeshData.Geoms[i].Vertices[j].Position;
      Vertices^[j].Tangent := MeshData.Geoms[i].Vertices[j].Tangent;
      Vertices^[j].Binormal := MeshData.Geoms[i].Vertices[j].Binormal;
      Vertices^[j].Normal := MeshData.Geoms[i].Vertices[j].Normal;
      if MeshData.Geoms[i].TCount > 0 then
      Vertices^[j].TexCoords[0] := MeshData.Geoms[i].Vertices[j].TexCoords[0]
      else
      Vertices^[j].TexCoords[0].SetValue(0, 0);
      if MeshData.Geoms[i].TCount > 1 then
      Vertices^[j].TexCoords[1] := MeshData.Geoms[i].Vertices[j].TexCoords[1]
      else
      Vertices^[j].TexCoords[1].SetValue(0, 0);
      Vertices^[j].Color := MeshData.Geoms[i].Vertices[j].Color;
    end;
    TmpMesh.UnlockVertexBuffer;
    Geom.OOBox := Geom.AABox;
    TmpMesh.LockIndexBuffer(0, Pointer(Indices));
    for j := 0 to Geom.FCount - 1 do
    begin
      Indices^[j * 3 + 0] := MeshData.Geoms[i].Faces[j].Indices[0];
      Indices^[j * 3 + 1] := MeshData.Geoms[i].Faces[j].Indices[1];
      Indices^[j * 3 + 2] := MeshData.Geoms[i].Faces[j].Indices[2];
    end;
    TmpMesh.UnlockIndexBuffer;
    TmpMesh.LockAttributeBuffer(D3DLOCK_DISCARD, PDword(Attribs));
    if MeshData.Geoms[i].MCount > 0 then
    for j := 0 to Geom.FCount - 1 do
    Attribs^[j] := MeshData.Geoms[i].Materials[MeshData.Geoms[i].Faces[j].MaterialID]
    else
    for j := 0 to Geom.FCount - 1 do
    Attribs^[j] := 0;
    TmpMesh.UnlockAttributeBuffer;
    if Length(Adj) < Geom.FCount * 3 then
    SetLength(Adj, Geom.FCount * 3);
    TmpMesh.GenerateAdjacency(G2EPS, @Adj[0]);
    TmpMesh.OptimizeInplace(
      D3DXMESHOPT_COMPACT or
      D3DXMESHOPT_ATTRSORT or
      D3DXMESHOPT_VERTEXCACHE,
      @Adj[0], nil, nil, nil
    );
    Geom.VCount := TmpMesh.GetNumVertices;
    Geom.FCount := TmpMesh.GetNumFaces;
    SetLength(Geom.Vertices, Geom.VCount);
    SetLength(Geom.Faces, Geom.FCount);
    TmpMesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(Vertices));
    for j := 0 to Geom.VCount - 1 do
    begin
      Geom.Vertices[j].Position := Vertices^[j].Position;
      Geom.Vertices[j].Tangent := Vertices^[j].Tangent;
      Geom.Vertices[j].Binormal := Vertices^[j].Binormal;
      Geom.Vertices[j].Normal := Vertices^[j].Normal;
      Geom.Vertices[j].TexCoords[0] := Vertices^[j].TexCoords[0];
      Geom.Vertices[j].TexCoords[1] := Vertices^[j].TexCoords[1];
      Geom.Vertices[j].Color := Vertices^[j].Color;
    end;
    TmpMesh.UnlockVertexBuffer;
    TmpMesh.LockIndexBuffer(D3DLOCK_READONLY, Pointer(Indices));
    for j := 0 to Geom.FCount * 3 - 1 do
    PG2Index16Array(@Geom.Faces[0])^[j] := Indices^[j];
    TmpMesh.UnlockIndexBuffer;
    TmpMesh.GetAttributeTable(nil, @Geom.GCount);
    SetLength(Geom.Groups, Geom.GCount);
    SetLength(AttribTable, Geom.GCount);
    TmpMesh.GetAttributeTable(@AttribTable[0], nil);
    for j := 0 to Geom.GCount - 1 do
    begin
      Geom.Groups[j].Material := MatRef[AttribTable[j].AttribId];
      Geom.Groups[j].VStart := AttribTable[j].VertexStart;
      Geom.Groups[j].VCount := AttribTable[j].VertexCount;
      Geom.Groups[j].FStart := AttribTable[j].FaceStart;
      Geom.Groups[j].FCount := AttribTable[j].FaceCount;
    end;
    SafeRelease(TmpMesh);
    Geom.Render := False;
    Geom.Collide := False;
    if Geom.FCount > 60 then
    Geom.OcTree := True;
  end;
  for i := 0 to Loader.MeshData^.LightCount - 1 do
  begin
    case Loader.MeshData^.Lights[i].LightType of
      0:
      begin
        LightPoint := DataFormats.DefClassOfLightPoint.Create(Self);
        NodeRef[Loader.MeshData^.Lights[i].NodeID] := LightPoint;
        LightPoint.Render := Loader.MeshData^.Lights[i].Enabled;
        LightPoint.Color := Loader.MeshData^.Lights[i].Color;
        LightPoint.AttStart := Loader.MeshData^.Lights[i].AttStart;
        LightPoint.AttEnd := Loader.MeshData^.Lights[i].AttEnd;
      end;
      1:
      begin
        LightSpot := DataFormats.DefClassOfLightSpot.Create(Self);
        NodeRef[Loader.MeshData^.Lights[i].NodeID] := LightSpot;
        LightSpot.Render := Loader.MeshData^.Lights[i].Enabled;
        LightSpot.Color := Loader.MeshData^.Lights[i].Color;
        LightSpot.AttStart := Loader.MeshData^.Lights[i].AttStart;
        LightSpot.AttEnd := Loader.MeshData^.Lights[i].AttEnd;
        LightSpot.SpotInner := Loader.MeshData^.Lights[i].SpotInner;
        LightSpot.SpotOutter := Loader.MeshData^.Lights[i].SpotOutter;
      end;
      2:
      begin
        LightDir := DataFormats.DefClassOfLightDir.Create(Self);
        NodeRef[Loader.MeshData^.Lights[i].NodeID] := LightDir;
        LightDir.Render := Loader.MeshData^.Lights[i].Enabled;
        LightDir.Color := Loader.MeshData^.Lights[i].Color;
        LightDir.AttStart := Loader.MeshData^.Lights[i].AttStart;
        LightDir.AttEnd := Loader.MeshData^.Lights[i].AttEnd;
        LightDir.WidthInner := Loader.MeshData^.Lights[i].WidthInner;
        LightDir.WidthOutter := Loader.MeshData^.Lights[i].WidthOutter;
        LightDir.HeightInner := Loader.MeshData^.Lights[i].HeightInner;
        LightDir.HeightOutter := Loader.MeshData^.Lights[i].HeightOutter;
      end;
    end;
  end;
  TopNodes := nil;
  for i := 0 to MeshData.NodeCount - 1 do
  begin
    if not Assigned(NodeRef[i]) then
    NodeRef[i] := TG2SGNode.Create(Self);
    Node := NodeRef[i];
    Node.Name := MeshData.Nodes[i].Name;
    Node.Transform := MeshData.Nodes[i].Transform;
    if MeshData.Nodes[i].OwnerID > -1 then
    begin
      Node.Parent := NodeRef[MeshData.Nodes[i].OwnerID];
      SetLength(Node.Parent.Children, Length(Node.Parent.Children) + 1);
      Node.Parent.Children[High(Node.Parent.Children)] := Node;
    end
    else
    begin
      Node.Parent := nil;
      SetLength(TopNodes, Length(TopNodes) + 1);
      TopNodes[High(TopNodes)] := Node;
    end;
  end;
  for i := 0 to High(TopNodes) do
  ComputeNodeTransform(TopNodes[i]);
  if not OcTreeAutoBuild then
  for i := 0 to High(NodeRef) do
  NodeRef[i].Update;
end;

procedure TG2SceneGraph.LoadG2M(const f: String);
  var Loader: TG2MeshLoaderG2M;
begin
  Loader := TG2MeshLoaderG2M.Create;
  if not Loader.CanLoadFile(f) then
  begin
    Loader.Free;
    Exit;
  end;
  try
    Loader.LoadFile(f);
    LoadG2M(Loader);
  finally
    Loader.Free;
  end;
end;

procedure TG2SceneGraph.LoadG2M(const s: TStream);
  var Loader: TG2MeshLoaderG2M;
begin
  Loader := TG2MeshLoaderG2M.Create;
  if not Loader.CanLoadStream(s) then
  begin
    Loader.Free;
    Exit;
  end;
  try
    Loader.LoadStream(s);
    LoadG2M(Loader);
  finally
    Loader.Free;
  end;
end;

procedure TG2SceneGraph.LoadG2M(const Buffer: Pointer; const Size: Integer);
  var s: TMemoryStream;
begin
  s := TMemoryStream.Create;
  s.Write(Buffer^, Size);
  s.Position := 0;
  LoadG2M(s);
  s.Free;
end;

procedure TG2SceneGraph.Clear;
begin
  while Nodes.Count > 0 do
  TG2SGNode(Nodes[0]).Free;
end;

procedure TG2SceneGraph.Update(const Query: PG2SGQuery);
  var i: Integer;
begin
  for i := 0 to Nodes.Count - 1 do
  TG2SGNode(Nodes[i]).Update;
  for i := 0 to Chars.Count - 1 do
  if TG2SGChar(Chars[i]).Collide then
  CollideChar(TG2SGChar(Chars[i]), Query);
end;

procedure TG2SceneGraph.Render(const Query: PG2SGQuery);
  var m: TG2Mat;
  var i, g: Integer;
  var Geom: TG2SGGeom;
begin
  QueryFetch(Core.Graphics.Transforms.Frustum, Query);
  Core.Graphics.RenderStates.ZEnable := True;
  Core.Graphics.Device.SetVertexDeclaration(DataFormats.DeclGeom);
  Core.Graphics.Transforms.PushW;
  for i := 0 to Query.GeomCount - 1 do
  begin
    Geom := Query.Geoms[i];
    m := Geom.Transform;
    Core.Graphics.Transforms.W[0] := m;
    Core.Graphics.Transforms.ApplyW(0);
    Core.Graphics.Device.SetStreamSource(0, Geom.VB, 0, DataFormats.VertexStrideGeom);
    Core.Graphics.Device.SetIndices(Geom.IB);
    for g := 0 to Geom.GCount - 1 do
    begin
      if Geom.Groups[g].Material^.Channels[0].TexDiffuse <> nil then
      Core.Graphics.Device.SetTexture(0, Geom.Groups[g].Material^.Channels[0].TexDiffuse.Texture);
      Core.Graphics.Device.DrawIndexedPrimitive(
        D3DPT_TRIANGLELIST, 0,
        Geom.Groups[g].VStart, Geom.Groups[g].VCount,
        Geom.Groups[g].FStart * 3, Geom.Groups[g].FCount
      );

    end;
  end;
  for i := 0 to Query.CharCount - 1 do
  if Query.Chars[i].Mesh <> nil then
  begin
    Core.Graphics.Transforms.W[0] := Query.Chars[i].Transform;
    Query.Chars[i].MeshInst.Render;
  end;
  Core.Graphics.Transforms.PopW;
  if Query^.OcclusionCull <> [] then
  Query.CheckOcclusion(Core.Graphics.Transforms.Frustum);
  Core.Graphics.RenderStates.ZEnable := False;
end;
//TG2SceneGraph END

//TG2SGNode BEGIN
constructor TG2SGNode.Create(const SceneGraph: TG2SceneGraph);
begin
  inherited Create;
  m_SceneGraph := SceneGraph;
  m_SceneGraph.Nodes.Add(Self);
  m_UserData := nil;
end;

destructor TG2SGNode.Destroy;
begin
  m_SceneGraph.Nodes.Remove(Self);
  inherited Destroy;
end;

procedure TG2SGNode.Update;
begin

end;
//TG2SGNode END

//TG2SGFrame BEGIN
procedure TG2SGFrame.SetRender(const Value: Boolean);
begin
  m_Render := Value;
end;

constructor TG2SGFrame.Create(const SceneGraph: TG2SceneGraph);
  var i: Integer;
  var Link: PG2SGQueryItemLink;
begin
  inherited Create(SceneGraph);
  m_SceneGraph.Frames.Add(Self);
  OcTreeItem.Frame := Self;
  OcTreeItem.QueryLinks.Capacity := 32;
  OcTreeItem.QueryLinks.Clear;
  OcTreeItem.OcTreeNodes.Capacity := 32;
  OcTreeItem.OcTreeNodes.Clear;
  for i := 0 to m_SceneGraph.Queries.Count - 1 do
  begin
    New(Link);
    Link^.Query := PG2SGQuery(m_SceneGraph.Queries[i]);
    Link^.Init;
    OcTreeItem.QueryLinks.Add(Link);
  end;
  Transform.SetIdentity;
  m_MinGridX := 0; m_MinGridY := 0; m_MinGridX := 0;
  m_MaxGridX := -1; m_MaxGridY := -1; m_MaxGridZ := -1;
  QueryListID := -1;
end;

destructor TG2SGFrame.Destroy;
  var i: Integer;
  var Link: PG2SGQueryItemLink;
begin
  for i := 0 to m_SceneGraph.Queries.Count - 1 do
  begin
    Link := PG2SGQueryItemLink(OcTreeItem.QueryLinks[PG2SGQuery(m_SceneGraph.Queries[i])^.ID]);
    Link^.UnInit;
    Dispose(Link);
  end;
  m_SceneGraph.OcTreeRemoveItem(@OcTreeItem);
  m_SceneGraph.Frames.Remove(Self);
  inherited Destroy;
end;

procedure TG2SGFrame.Update;
  var MinGridX, MinGridY, MinGridZ: Integer;
  var MaxGridX, MaxGridY, MaxGridZ: Integer;
begin
  AABox := GetAABox;
  m_SceneGraph.OcTreeGetIndices(AABox, MinGridX, MinGridY, MinGridZ, MaxGridX, MaxGridY, MaxGridZ);
  if (MinGridX <> m_MinGridX)
  or (MinGridY <> m_MinGridY)
  or (MinGridZ <> m_MinGridZ)
  or (MaxGridX <> m_MaxGridX)
  or (MaxGridY <> m_MaxGridY)
  or (MaxGridZ <> m_MaxGridZ) then
  begin
    m_MinGridX := MinGridX;
    m_MinGridY := MinGridY;
    m_MinGridZ := MinGridZ;
    m_MinGridX := MinGridX;
    m_MinGridY := MinGridY;
    m_MinGridZ := MinGridZ;
    m_SceneGraph.OcTreeRemoveItem(@OcTreeItem);
    m_SceneGraph.OcTreeAddItem(@OcTreeItem);
  end;
end;
//TG2SGFrame END

//TG2SGGeom BEGIN
procedure TG2SGGeom.SetCollide(const Value: Boolean);
  var i: Integer;
begin
  if Value = m_Collide then Exit;
  m_Collide := Value;
  if m_Collide then
  begin
    SetLength(Collider.Vertices, VCount);
    SetLength(Collider.Faces, FCount);
    for i := 0 to FCount - 1 do
    begin
      Collider.Faces[i].Indices[0] := Faces[i][0];
      Collider.Faces[i].Indices[1] := Faces[i][1];
      Collider.Faces[i].Indices[2] := Faces[i][2];
      Collider.Faces[i].Edges[0].Indices[0] := Collider.Faces[i].Indices[0];
      Collider.Faces[i].Edges[0].Indices[1] := Collider.Faces[i].Indices[1];
      Collider.Faces[i].Edges[1].Indices[0] := Collider.Faces[i].Indices[1];
      Collider.Faces[i].Edges[1].Indices[1] := Collider.Faces[i].Indices[2];
      Collider.Faces[i].Edges[2].Indices[0] := Collider.Faces[i].Indices[2];
      Collider.Faces[i].Edges[2].Indices[1] := Collider.Faces[i].Indices[0];
      Collider.Faces[i].CillideID := 0;
    end;
    UpdateCollider;
  end
  else
  begin
    Collider.Vertices := nil;
    Collider.Faces := nil;
  end;
end;

procedure TG2SGGeom.UpdateCollider;
  var i, j: Integer;
  var v: TG2Vec3;
  var b: Boolean;
begin
  D3DXVec3TransformCoordArray(
    @Collider.Vertices[0], 12,
    @Vertices[0], m_SceneGraph.DataFormats.VertexStrideGeom,
    Transform, VCount
  );
  for i := 0 to FCount - 1 do
  begin
    Collider.Faces[i].AABox.MinV := Collider.Vertices[Collider.Faces[i].Indices[0]];
    Collider.Faces[i].AABox.MaxV := Collider.Faces[i].AABox.MinV;
    for j := 1 to 2 do
    Collider.Faces[i].AABox := Collider.Faces[i].AABox + Collider.Vertices[Collider.Faces[i].Indices[j]];
    Collider.Faces[i].Plane.SetPlane(
      Collider.Vertices[Collider.Faces[i].Indices[0]],
      G2TriangleNormal(
        Collider.Vertices[Collider.Faces[i].Indices[0]],
        Collider.Vertices[Collider.Faces[i].Indices[1]],
        Collider.Vertices[Collider.Faces[i].Indices[2]]
      )
    );
    G2Vec3ToLine(
      Collider.Vertices[Collider.Faces[i].Indices[0]],
      Collider.Vertices[Collider.Faces[i].Indices[1]],
      Collider.Vertices[Collider.Faces[i].Indices[2]],
      v, b
    );
    Collider.Faces[i].Edges[0].N := (v - Collider.Vertices[Collider.Faces[i].Indices[2]]).Normalized;
    G2Vec3ToLine(
      Collider.Vertices[Collider.Faces[i].Indices[1]],
      Collider.Vertices[Collider.Faces[i].Indices[2]],
      Collider.Vertices[Collider.Faces[i].Indices[0]],
      v, b
    );
    Collider.Faces[i].Edges[1].N := (v - Collider.Vertices[Collider.Faces[i].Indices[0]]).Normalized;
    G2Vec3ToLine(
      Collider.Vertices[Collider.Faces[i].Indices[2]],
      Collider.Vertices[Collider.Faces[i].Indices[0]],
      Collider.Vertices[Collider.Faces[i].Indices[1]],
      v, b
    );
    Collider.Faces[i].Edges[2].N := (v - Collider.Vertices[Collider.Faces[i].Indices[1]]).Normalized;
  end;
end;

procedure TG2SGGeom.SetOcTree(const Value: Boolean);
  var AvSize: TG2Vec3;
  var FaceAABox: array of TG2AABox;
  var FaceList: TG2QuickList;
  procedure InitNode(const n: PG2SGGeomOcTreeNode);
    var dx, dy, dz, x, y, z, i: Integer;
    var sx, sy, sz: Single;
    var np: PG2SGGeomOcTreeNode;
  begin
    sx := n^.AABox.MaxV.x - n^.AABox.MinV.x;
    sy := n^.AABox.MaxV.y - n^.AABox.MinV.y;
    sz := n^.AABox.MaxV.z - n^.AABox.MinV.z;
    if n^.Depth >= 8 then
    begin
      dx := 0; dy := 0; dz := 0;
    end
    else
    begin
      if sx > AvSize.x then dx := 1 else dx := 0;
      if sy > AvSize.y then dy := 1 else dy := 0;
      if sz > AvSize.z then dz := 1 else dz := 0;
    end;
    sx := sx / (dx + 1);
    sy := sy / (dy + 1);
    sz := sz / (dz + 1);
    if (dx > 0) or (dy > 0) or (dz > 0) then
    begin
      n^.NoDiv := False;
      n^.CountX := dx + 1;
      n^.CountY := dy + 1;
      n^.CountZ := dz + 1;
      SetLength(n^.SubNodes, n^.CountX, n^.CountY, n^.CountZ);
      for x := 0 to dx do
      for y := 0 to dy do
      for z := 0 to dz do
      begin
        n^.SubNodes[x, y, z].AABox.MinV.x := n^.AABox.MinV.x + sx * x;
        n^.SubNodes[x, y, z].AABox.MinV.y := n^.AABox.MinV.y + sy * y;
        n^.SubNodes[x, y, z].AABox.MinV.z := n^.AABox.MinV.z + sz * z;
        n^.SubNodes[x, y, z].AABox.MaxV := n^.SubNodes[x, y, z].AABox.MinV + G2Vec3(sx, sy, sz);
        n^.SubNodes[x, y, z].Depth := n^.Depth + 1;
        n^.SubNodes[x, y, z].Parent := n;
        n^.SubNodes[x, y, z].TotalFaces := 0;
        InitNode(@n^.SubNodes[x, y, z]);
      end;
    end
    else
    begin
      n^.NoDiv := True;
      FaceList.Clear;
      for i := 0 to FCount - 1 do
      if n^.AABox.Intersect(FaceAABox[i]) then
      FaceList.Add(Pointer(i));
      SetLength(n^.Faces, FaceList.Count);
      for i := 0 to FaceList.Count - 1 do
      n^.Faces[i] := Integer(FaceList[i]);
      n^.TotalFaces := FaceList.Count;
      np := n^.Parent;
      while np <> nil do
      begin
        np^.TotalFaces := np^.TotalFaces + n^.TotalFaces;
        np := np^.Parent;
      end;
    end;
  end;
  var i, j: Integer;
begin
  if m_OcTree = Value then Exit;
  m_OcTree := Value;
  if m_OcTree then
  begin
    if (VCount > 0) and (FCount > 0) then
    begin
      OcTreeNode.AABox.MinV := Vertices[0].Position;
      OcTreeNode.AABox.MaxV := OcTreeNode.AABox.MinV;
      for i := 1 to VCount - 1 do
      OcTreeNode.AABox := OcTreeNode.AABox + Vertices[i].Position;
      OcTreeNode.AABox.MinV.x := OcTreeNode.AABox.MinV.x - 0.1;
      OcTreeNode.AABox.MinV.y := OcTreeNode.AABox.MinV.y - 0.1;
      OcTreeNode.AABox.MinV.z := OcTreeNode.AABox.MinV.z - 0.1;
      OcTreeNode.AABox.MaxV.x := OcTreeNode.AABox.MaxV.x + 0.1;
      OcTreeNode.AABox.MaxV.y := OcTreeNode.AABox.MaxV.y + 0.1;
      OcTreeNode.AABox.MaxV.z := OcTreeNode.AABox.MaxV.z + 0.1;
      OcTreeNode.TotalFaces := 0;
      OcTreeNode.Parent := nil;
      AvSize.SetValue(0, 0, 0);
      SetLength(FaceAABox, FCount);
      for i := 0 to FCount - 1 do
      begin
        FaceAABox[i].MinV := Vertices[Faces[i][0]].Position;
        FaceAABox[i].MaxV := FaceAABox[i].MinV;
        for j := 1 to 2 do
        FaceAABox[i] := FaceAABox[i] + Vertices[Faces[i][j]].Position;
        AvSize.x := AvSize.x + FaceAABox[i].MaxV.x - FaceAABox[i].MinV.x;
        AvSize.y := AvSize.y + FaceAABox[i].MaxV.y - FaceAABox[i].MinV.y;
        AvSize.z := AvSize.z + FaceAABox[i].MaxV.z - FaceAABox[i].MinV.z;
      end;
      AvSize := (AvSize / FCount) * 4;
      FaceList.Clear;
      OcTreeNode.Depth := 0;
      InitNode(@OcTreeNode);
      SetLength(m_FaceFetchID, FCount);
      ResetFaceFectchID;
    end
    else
    m_OcTree := False;
  end;
end;

procedure TG2SGGeom.ResetFaceFectchID;
  var i: Integer;
begin
  for i := 0 to FCount - 1 do
  m_FaceFetchID[i] := 0;
  m_CurFaceFetchID := 0;
end;

constructor TG2SGGeom.Create(const SceneGraph: TG2SceneGraph);
begin
  inherited Create(SceneGraph);
  m_SceneGraph.Geoms.Add(Self);
  m_Render := False;
  m_Collide := False;
  m_OcTree := False;
  m_PrevTransform.SetValue(
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0
  );
  QueryListID := G2QL_GEOMS;
end;

destructor TG2SGGeom.Destroy;
begin
  SafeRelease(VB);
  SafeRelease(IB);
  m_SceneGraph.Geoms.Remove(Self);
  inherited Destroy;
end;

procedure TG2SGGeom.Update;
begin
  inherited Update;
  if m_Collide
  and (m_PrevTransform <> Transform) then
  begin
    m_PrevTransform := Transform;
    UpdateCollider;
  end;
end;

function TG2SGGeom.IntersectRay(const r: TG2Ray): Boolean;
  var FaceID: Word;
  var u, v, d: Single;
begin
  Result := IntersectRay(r, FaceID, u, v, d);
end;

function TG2SGGeom.IntersectRay(const r: TG2Ray; var FaceID: Word; var U, V, Dist: Single): Boolean;
  var ro: TG2Ray;
  var cu, cv, d: Single;
  procedure CheckNode(const n: PG2SGGeomOcTreeNode);
    var x, y, z, i, f: Integer;
    var v0, v1, v2: TG2Vec3;
  begin
    if (n^.TotalFaces > 0)
    and  ro.IntersectAABox(n^.AABox) then
    begin
      if n^.NoDiv then
      begin
        if m_Collide then
        begin
          for i := 0 to High(n^.Faces) do
          if m_FaceFetchID[n^.Faces[i]] < m_CurFaceFetchID then
          begin
            f := n^.Faces[i];
            m_FaceFetchID[f] := m_CurFaceFetchID;
            if r.IntersectAABox(Collider.Faces[f].AABox)
            and r.IntersectTri(
              Collider.Vertices[Collider.Faces[f].Indices[0]],
              Collider.Vertices[Collider.Faces[f].Indices[1]],
              Collider.Vertices[Collider.Faces[f].Indices[2]],
              cu, cv, d
            ) then
            begin
              if not Result
              or (d < Dist) then
              begin
                Result := True;
                FaceID := f;
                U := cu;
                V := cv;
                Dist := d;
              end;
            end;
          end;
        end
        else
        begin
          for i := 0 to High(n^.Faces) do
          if m_FaceFetchID[n^.Faces[i]] < m_CurFaceFetchID then
          begin
            f := n^.Faces[i];
            m_FaceFetchID[f] := m_CurFaceFetchID;
            v0 := Vertices[Faces[f][0]].Position.Transform4x3(Transform);
            v1 := Vertices[Faces[f][1]].Position.Transform4x3(Transform);
            v2 := Vertices[Faces[f][2]].Position.Transform4x3(Transform);
            if r.IntersectTri(v0, v1, v2, cu, cv, d) then
            begin
              if not Result
              or (d < Dist) then
              begin
                Result := True;
                FaceID := f;
                U := cu;
                V := cv;
                Dist := d;
              end;
            end;
          end;
        end;
      end
      else
      begin
        for x := 0 to n^.CountX - 1 do
        for y := 0 to n^.CountY - 1 do
        for z := 0 to n^.CountZ - 1 do
        CheckNode(@n^.SubNodes[x, y, z]);
      end;
    end;
  end;
  var i: Integer;
  var v0, v1, v2: TG2Vec3;
begin
  Result := False;
  if m_OcTree then
  begin
    m_SceneGraph.CS.Enter;
    try
      if m_CurFaceFetchID >= High(Int64) - 1 then
      ResetFaceFectchID;
      Inc(m_CurFaceFetchID);
      ro := r;
      ro.TransformInverse(Transform);
      CheckNode(@OcTreeNode);
    finally
      m_SceneGraph.CS.Leave;
    end;
  end
  else
  begin
    if m_Collide then
    for i := 0 to High(Collider.Faces) do
    begin
      if r.IntersectAABox(Collider.Faces[i].AABox)
      and r.IntersectTri(
        Collider.Vertices[Collider.Faces[i].Indices[0]],
        Collider.Vertices[Collider.Faces[i].Indices[1]],
        Collider.Vertices[Collider.Faces[i].Indices[2]],
        cu, cv, d
      ) then
      begin
        if not Result
        or (d < Dist) then
        begin
          Result := True;
          FaceID := i;
          U := cu;
          V := cv;
          Dist := d;
        end;
      end;
    end
    else
    begin
      for i := 0 to FCount - 1 do
      begin
        v0 := Vertices[Faces[i][0]].Position.Transform4x3(Transform);
        v1 := Vertices[Faces[i][1]].Position.Transform4x3(Transform);
        v2 := Vertices[Faces[i][2]].Position.Transform4x3(Transform);
        if r.IntersectTri(v0, v1, v2, cu, cv, d) then
        begin
          if not Result
          or (d < Dist) then
          begin
            Result := True;
            FaceID := i;
            U := cu;
            V := cv;
            Dist := d;
          end;
        end;
      end;
    end;
  end;
end;

procedure TG2SGGeom.SetRender(const Value: Boolean);
  var i: Integer;
  var PtrVertices: PG2SGVertexGeomArr;
  var PtrIndices: PG2Index16Array;
begin
  if Value = m_Render then Exit;
  m_Render := Value;
  if m_Render then
  begin
    m_SceneGraph.Core.Graphics.Device.CreateVertexBuffer(
      VCount * 2 * m_SceneGraph.DataFormats.VertexStrideGeom,
      D3DUSAGE_WRITEONLY,
      0,
      D3DPOOL_MANAGED,
      VB,
      nil
    );
    m_SceneGraph.Core.Graphics.Device.CreateIndexBuffer(
      FCount * 6,
      D3DUSAGE_WRITEONLY,
      D3DFMT_INDEX16,
      D3DPOOL_MANAGED,
      IB,
      nil
    );
    VB.Lock(0, VCount * m_SceneGraph.DataFormats.VertexStrideGeom, Pointer(PtrVertices), 0);
    for i := 0 to VCount - 1 do
    begin
      PtrVertices^[i].Position := Vertices[i].Position;
      PtrVertices^[i].Tangent := Vertices[i].Tangent;
      PtrVertices^[i].Binormal := Vertices[i].Binormal;
      PtrVertices^[i].Normal := Vertices[i].Normal;
      PtrVertices^[i].TexCoords[0] := Vertices[i].TexCoords[0];
      PtrVertices^[i].TexCoords[1] := Vertices[i].TexCoords[1];
      PtrVertices^[i].Color := Vertices[i].Color;
    end;
    VB.Unlock;
    IB.Lock(0, FCount * 6, Pointer(PtrIndices), 0);
    for i := 0 to FCount * 3 - 1 do
    PtrIndices^[i] := PG2Index16Array(@Faces[0])^[i];
    IB.Unlock;
  end
  else
  begin
    SafeRelease(VB);
    SafeRelease(IB);
  end;
end;

function TG2SGGeom.GetAABox: TG2AABox;
begin
  Result := (OOBox * Transform).AABox;
end;
//TG2SGGeom END

//TG2SGChar BEGIN
procedure TG2SGChar.SetMesh(const Value: TG2Mesh);
  var i: Integer;
begin
  if m_Mesh = Value then Exit;
  if Assigned(m_Mesh) then
  m_MeshInst.Free;
  m_Mesh := Value;
  if Assigned(m_Mesh) then
  begin
    m_MeshInst := m_Mesh.InstanceCreate;
    for i := 0 to m_Mesh.MaterialCount - 1 do
    begin
      m_MeshInst.Materials[i].MapDiffuse := m_SceneGraph.FindTextureDiffuse(WideString(m_Mesh.Materials[i].DiffuseMap));
      m_MeshInst.Materials[i].MapSpecular := m_SceneGraph.FindTextureSpecular(WideString(m_Mesh.Materials[i].SpecularMap));
      m_MeshInst.Materials[i].MapNormals := m_SceneGraph.FindTextureNormals(WideString(m_Mesh.Materials[i].NormalMap));
    end;
  end;
end;

constructor TG2SGChar.Create(const SceneGraph: TG2SceneGraph);
begin
  inherited Create(SceneGraph);
  m_SceneGraph.Chars.Add(Self);
  m_Mesh := nil;
  m_MeshInst := nil;
  m_MinGridX := 0; m_MinGridY := 0; m_MinGridZ := 0;
  m_MaxGridX := -1; m_MaxGridY := -1; m_MaxGridZ := -1;
  Radius := 1;
  Step.SetValue(0, -2, 0);
  Vel.SetValue(0, 0, 0);
  StepHardness := 0.9;
  Grounded := False;
  m_Render := True;
  m_Collide := True;
  QueryListID := G2QL_CHARS;
end;

destructor TG2SGChar.Destroy;
begin
  if Assigned(m_Mesh) then
  m_MeshInst.Free;
  m_SceneGraph.Chars.Remove(Self);
  inherited Destroy;
end;

procedure TG2SGChar.Update;
begin
  if Grounded then
  Vel := Vel * 0.9;
  PG2Vec3(@Transform.e30)^ := PG2Vec3(@Transform.e30)^ + Vel;
  inherited Update;
end;

function TG2SGChar.GetAABox: TG2AABox;
begin
  if Assigned(MeshInst) then
  Result := (MeshInst.BBox * Transform).AABox
  else
  Result.SetValue(G2Vec3(0, 0, 0), G2Vec3(-1, -1, -1));
end;
//TG2SGChar END

//TG2SGLight BEGIN
constructor TG2SGLight.Create(const SceneGraph: TG2SceneGraph);
begin
  inherited Create(SceneGraph);
  m_SceneGraph.Lights.Add(Self);
  QueryListID := G2QL_LIGHTS;
  LightType := ltNone;
end;

destructor TG2SGLight.Destroy;
begin
  m_SceneGraph.Lights.Remove(Self);
  inherited Destroy;
end;
//TG2SGLight END

//TG2SGLightPoint BEGIN
function TG2SGLightPoint.GetAABox: TG2AABox;
  var v: TG2Vec3;
begin
  v := Transform.GetTranslation;
  Result.SetValue(
    G2Vec3(v.x - AttEnd, v.y - AttEnd, v.z - AttEnd),
    G2Vec3(v.x + AttEnd, v.y + AttEnd, v.z + AttEnd)
  );
end;

constructor TG2SGLightPoint.Create(const SceneGraph: TG2SceneGraph);
begin
  inherited Create(SceneGraph);
  LightType := ltPoint;
end;
//TG2SGLightPoint END

//TG2SGLightSpot BEGIN
function TG2SGLightSpot.GetAABox: TG2AABox;
  var OOBox: TG2Box;
  var w: Single;
begin
  w := Tan(SpotOutter * 0.5) * AttEnd;
  OOBox.vx.SetValue(w, 0, 0);
  OOBox.vy.SetValue(0, AttEnd * 0.5, 0);
  OOBox.vz.SetValue(0, 0, w);
  OOBox.C.SetValue(0, -AttEnd * 0.5, 0);
  Result := (OOBox * Transform).AABox;
end;

constructor TG2SGLightSpot.Create(const SceneGraph: TG2SceneGraph);
begin
  inherited Create(SceneGraph);
  LightType := ltSpot;
end;
//TG2SGLightSpot END

//TG2SGLightDir BEGIN
function TG2SGLightDir.GetAABox: TG2AABox;
  var OOBox: TG2Box;
begin
  OOBox.vx.SetValue(WidthOutter * 0.5, 0, 0);
  OOBox.vy.SetValue(0, AttEnd * 0.5, 0);
  OOBox.vz.SetValue(0, 0, HeightOutter * 0.5);
  OOBox.C.SetValue(0, -AttEnd * 0.5, 0);
  Result := (OOBox * Transform).AABox;
end;

constructor TG2SGLightDir.Create(const SceneGraph: TG2SceneGraph);
begin
  inherited Create(SceneGraph);
  LightType := ltDir;
end;
//TG2SGLightDir END

end.