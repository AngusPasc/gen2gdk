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

  TG2SGQueryStatistics = record
  public
    var ObjectsCulled: Integer;
    var ObjectsRendered: Integer;
  end;

  TG2SGQuery = record
  strict private
    type TOcclusionCullQuality = (cqFastest, cqLow, cqBalanced, cqBest);
    var m_OcclusionCullEnable: Boolean;
    var m_OcclusionCullQuality: TOcclusionCullQuality;
    var m_DistanceSortEnable: Boolean;
    procedure SetOcclusionCullEnable(const Value: Boolean);
    procedure SetOcclusionCullQuality(const Value: TOcclusionCullQuality);
    procedure SetOcclusionCullParams;
  private
    var ID: Integer;
    var SceneGraph: TG2SceneGraph;
    var QueryLists: array [0..2] of TG2QuickSortList;
    var OcclusionRT: TG2SurfaceRT;
    var OcclusionDS: TG2SurfaceDS;
    procedure Init;
    procedure UnInit;
    function GetGeomCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetGeoms(const Index: Integer): TG2SGGeom; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCharCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetChars(const Index: Integer): TG2SGChar; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLightCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLights(const Index: Integer): TG2SGLight; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var Stats: TG2SGQueryStatistics;
    property OcclusionCullEnable: Boolean read m_OcclusionCullEnable write SetOcclusionCullEnable;
    property OcclusionCullQuality: TOcclusionCullQuality read m_OcclusionCullQuality write SetOcclusionCullQuality;
    property DistanceSortEnable: Boolean read m_DistanceSortEnable write m_DistanceSortEnable;
    property GeomCount: Integer read GetGeomCount;
    property Geoms[const Index: Integer]: TG2SGGeom read GetGeoms;
    property CharCount: Integer read GetCharCount;
    property Chars[const Index: Integer]: TG2SGChar read GetChars;
    property LightCount: Integer read GetLightCount;
    property Lights[const Index: Integer]: TG2SGLight read GetLights;
  end;
  PG2SGQuery = ^TG2SGQuery;

  TG2SGQueryLink = record
  public
    var Query: PG2SGQuery;
    var OcclusionQuery: IDirect3DQuery9;
    var OcclusionTest: Boolean;
    var OcclusionVisible: Boolean;
    var OcclusioonTestTime: DWord;
    procedure Init;
    procedure UnInit;
  end;
  PG2SGQueryLink = ^TG2SGQueryLink;

  TG2SGOcTreeItem = record
  public
    var Frame: TG2SGFrame;
    var FetchID: DWord;
    var QueryLinks: TG2QuickList;
    var OcTreeNodes: TG2QuickList;
  end;
  PG2SGOcTreeItem = ^TG2SGOcTreeItem;

  TG2SceneGraph = class
  strict private
    type POcTreeNode = ^TOcTreeNode;
    TOcTreeNode = record
    public
      var AABox: TG2AABox;
      var Parent: POcTreeNode;
      var SubNodes: array of array of array of TOcTreeNode;
      var DivX: Boolean;
      var DivY: Boolean;
      var DivZ: Boolean;
      var DivN: Boolean;
      var MinX, MaxX, MinY, MaxY, MinZ, MaxZ: Integer;
      var TotalItemCount: Integer;
      var Items: TG2QuickList;
      var FrustumTest: TG2FrustumCheck;
      procedure TotalItemsInc(const Amount: Integer = 1);
      procedure TotalItemsDec(const Amount: Integer = 1);
    end;
    var m_RootNode: TOcTreeNode;
    var m_OcTreeGrid: array of array of array of POcTreeNode;
    var m_OcTreeNodeFetch: TG2QuickList;
    var m_GeomCollideList: TG2QuickList;
    var m_Prim3D: TG2Primitives3D;
    var m_CurFetchID: DWord;
    var m_CurCollideID: DWord;
    var m_PlugGraphics: TG2PlugGraphics;
    procedure ResetFetchID;
    procedure ResetCollideID;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  private
    function OcTreeAutoBuild: Boolean;
    procedure OcTreeBuild(const AABox: TG2AABox; const MinDivSize: Single);
    procedure OcTreeAddItem(const Item: PG2SGOcTreeItem);
    procedure OcTreeRemoveItem(const Item: PG2SGOcTreeItem);
    procedure OcTreeFetchNodes(const AABox: TG2AABox; const List: PG2QuickList; const IncludeEmpty: Boolean = False); overload;
    procedure OcTreeFetchNodes(const Frustum: TG2Frustum; const List: PG2QuickList); overload;
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
    function CollideSphere(var s: TG2Sphere): Boolean;
    function CollideChar(const c: TG2SGChar): Boolean;
    procedure LoadG2M(const Loader: TG2MeshLoaderG2M); overload;
    procedure LoadG2M(const f: String); overload;
    procedure LoadG2M(const s: TStream); overload;
    procedure LoadG2M(const Buffer: Pointer; const Size: Integer); overload;
    procedure Clear;
    procedure Update;
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
    procedure SetCollide(const Value: Boolean);
    procedure UpdateCollider;
  protected
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
    function IntersectRay(const r: TG2Ray; var U, V, Dist: Single): Boolean; overload;
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

//TG2SGQuery BEGIN
procedure TG2SGQuery.SetOcclusionCullEnable(const Value: Boolean);
begin
  if m_OcclusionCullEnable <> Value then
  begin
    m_OcclusionCullEnable := Value;
    SetOcclusionCullParams;
  end;
end;

procedure TG2SGQuery.SetOcclusionCullQuality(const Value: TOcclusionCullQuality);
begin
  if m_OcclusionCullQuality <> Value then
  begin
    m_OcclusionCullQuality := Value;
    if m_OcclusionCullEnable then
    SetOcclusionCullParams;
  end;
end;

procedure TG2SGQuery.SetOcclusionCullParams;
  var w, h: DWord;
begin
  if m_OcclusionCullEnable then
  begin
    case m_OcclusionCullQuality of
      cqFastest:
      begin
        w := SceneGraph.Core.Graphics.Params.Width div 16;
        h := SceneGraph.Core.Graphics.Params.Height div 16;
      end;
      cqLow:
      begin
        w := SceneGraph.Core.Graphics.Params.Width div 8;
        h := SceneGraph.Core.Graphics.Params.Height div 8;
      end;
      cqBalanced:
      begin
        w := SceneGraph.Core.Graphics.Params.Width div 4;
        h := SceneGraph.Core.Graphics.Params.Height div 4;
      end;
      cqBest:
      begin
        w := SceneGraph.Core.Graphics.Params.Width div 2;
        h := SceneGraph.Core.Graphics.Params.Height div 2;
      end;
      else
      begin
        w := SceneGraph.Core.Graphics.Params.Width;
        h := SceneGraph.Core.Graphics.Params.Height;
      end;
    end;
    if Assigned(OcclusionRT) then
    begin
      if (OcclusionRT.Width <> w)
      or (OcclusionRT.Height <> h) then
      begin
        OcclusionRT.Release;
        OcclusionRT.CreateRenderTarget(w, h, D3DFMT_X8R8G8B8);
      end;
    end
    else
    begin
      OcclusionRT := TG2SurfaceRT.Create;
      OcclusionRT.Initialize(SceneGraph.Core);
      OcclusionRT.CreateRenderTarget(w, h, D3DFMT_X8R8G8B8);
    end;
    if Assigned(OcclusionDS) then
    begin
      if (OcclusionDS.Width <> w)
      or (OcclusionDS.Height <> h) then
      begin
        OcclusionDS.Release;
        OcclusionDS.CreateDepthStencil(w, h, D3DFMT_D16);
      end;
    end
    else
    begin
      OcclusionDS := TG2SurfaceDS.Create;
      OcclusionDS.Initialize(SceneGraph.Core);
      OcclusionDS.CreateDepthStencil(w, h, D3DFMT_D16);
    end;
  end
  else
  begin
    if Assigned(OcclusionRT) then
    begin
      OcclusionRT.Finalize;
      OcclusionRT.Free;
      OcclusionRT := nil;
    end;
    if Assigned(OcclusionDS) then
    begin
      OcclusionDS.Finalize;
      OcclusionDS.Free;
      OcclusionDS := nil;
    end;
  end;
end;

procedure TG2SGQuery.Init;
  var i: Integer;
  var Link: PG2SGqueryLink;
begin
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
  m_OcclusionCullEnable := False;
  m_OcclusionCullQuality := cqFastest;
  m_DistanceSortEnable := False;
  OcclusionRT := nil;
  OcclusionDS := nil;
  Stats.ObjectsCulled := 0;
  Stats.ObjectsRendered := 0;
end;

procedure TG2SGQuery.UnInit;
  var i: Integer;
  var Link: PG2SGqueryLink;
begin
  if Assigned(OcclusionRT) then
  begin
    OcclusionRT.Finalize;
    OcclusionRT.Free;
  end;
  if Assigned(OcclusionDS) then
  begin
    OcclusionDS.Finalize;
    OcclusionDS.Free;
  end;
  for i := 0 to SceneGraph.Frames.Count - 1 do
  begin
    Link := PG2SGQueryLink(TG2SGFrame(SceneGraph.Frames[i]).OcTreeItem.QueryLinks[ID]);
    TG2SGFrame(SceneGraph.Frames[i]).OcTreeItem.QueryLinks.Remove(Link);
    Link^.UnInit;
    Dispose(Link);
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
//TG2SGQuery END

//TG2SGQueryLink BEGIN
procedure TG2SGQueryLink.Init;
begin
  Query.SceneGraph.Core.Graphics.Device.CreateQuery(
    D3DQUERYTYPE_OCCLUSION,
    OcclusionQuery
  );
  OcclusionTest := False;
  OcclusionVisible := True;
  OcclusioonTestTime := GetTickCount - 1000;
end;

procedure TG2SGQueryLink.UnInit;
begin
  SafeRelease(OcclusionQuery);
end;
//TG2SGQueryLink END

//TG2SceneGraph BEGIN
procedure TG2SceneGraph.TOcTreeNode.TotalItemsInc(const Amount: Integer = 1);
  procedure IncItemCount(const n: TG2SceneGraph.POcTreeNode);
  begin
    Inc(n^.TotalItemCount, Amount);
    if Assigned(n^.Parent) then
    IncItemCount(n^.Parent);
  end;
begin
  IncItemCount(@Self);
end;

procedure TG2SceneGraph.TOcTreeNode.TotalItemsDec(const Amount: Integer = 1);
  procedure DecItemCount(const n: TG2SceneGraph.POcTreeNode);
  begin
    Dec(n^.TotalItemCount, Amount);
    if Assigned(n^.Parent) then
    DecItemCount(n^.Parent);
  end;
begin
  DecItemCount(@Self);
end;

procedure TG2SceneGraph.ResetFetchID;
  var i: Integer;
begin
  m_CurFetchID := 0;
  for i := 0 to Frames.Count - 1 do
  TG2SGFrame(Frames[i]).OcTreeItem.FetchID := 0;
end;

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
  if PG2SGQuery(Queries[i])^.OcclusionCullEnable then
  begin
    PG2SGQuery(Queries[i])^.OcclusionRT.OnDeviceLost;
    PG2SGQuery(Queries[i])^.OcclusionDS.OnDeviceLost;
  end;
  for i := 0 to Frames.Count - 1 do
  for j := 0 to TG2SGFrame(Frames[i]).OcTreeItem.QueryLinks.Count - 1 do
  PG2SGQueryLink(TG2SGFrame(Frames[i]).OcTreeItem.QueryLinks[j])^.UnInit;
end;

procedure TG2SceneGraph.OnDeviceReset;
  var i, j: Integer;
begin
  for i := 0 to Queries.Count - 1 do
  if PG2SGQuery(Queries[i])^.OcclusionCullEnable then
  begin
    PG2SGQuery(Queries[i])^.OcclusionRT.OnDeviceReset;
    PG2SGQuery(Queries[i])^.OcclusionDS.OnDeviceReset;
  end;
  for i := 0 to Frames.Count - 1 do
  for j := 0 to TG2SGFrame(Frames[i]).OcTreeItem.QueryLinks.Count - 1 do
  PG2SGQueryLink(TG2SGFrame(Frames[i]).OcTreeItem.QueryLinks[j])^.Init;
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
  if (FrameAABox.MinV.x < m_RootNode.AABox.MinV.x)
  or (FrameAABox.MinV.y < m_RootNode.AABox.MinV.y)
  or (FrameAABox.MinV.z < m_RootNode.AABox.MinV.z)
  or (FrameAABox.MaxV.x > m_RootNode.AABox.MaxV.x)
  or (FrameAABox.MaxV.y > m_RootNode.AABox.MaxV.y)
  or (FrameAABox.MaxV.z > m_RootNode.AABox.MaxV.z) then
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
  procedure BuildNode(const n: POcTreeNode);
    var gx, gy, gz, sx, sy, sz: Single;
    var nx, ny, nz: Integer;
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
  end;
  var MaxDim: Single;
  var sx, sy, sz: Single;
  var DivX, DivY, DivZ: Integer;
begin
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
  m_RootNode.AABox := AABox;
  m_RootNode.DivX := False;
  m_RootNode.DivY := False;
  m_RootNode.DivZ := False;
  m_RootNode.DivN := True;
  m_RootNode.Parent := nil;
  BuildNode(@m_RootNode);
end;

procedure TG2SceneGraph.OcTreeAddItem(const Item: PG2SGOcTreeItem);
  var i: Integer;
begin
  OcTreeFetchNodes(Item^.Frame.AABox, @Item^.OcTreeNodes, True);
  for i := 0 to Item^.OcTreeNodes.Count - 1 do
  begin
    POcTreeNode(Item^.OcTreeNodes[i])^.Items.Add(Item);
    POcTreeNode(Item^.OcTreeNodes[i])^.TotalItemsInc();
  end;
end;

procedure TG2SceneGraph.OcTreeRemoveItem(const Item: PG2SGOcTreeItem);
  var i: Integer;
begin
  for i := 0 to Item^.OcTreeNodes.Count - 1 do
  begin
    POcTreeNode(Item^.OcTreeNodes[i])^.Items.Remove(Item);
    POcTreeNode(Item^.OcTreeNodes[i])^.TotalItemsDec();
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

procedure TG2SceneGraph.OcTreeFetchNodes(const Frustum: TG2Frustum; const List: PG2QuickList);
  procedure IncludeNode(const n: POcTreeNode);
    var nx, ny, nz: Integer;
  begin
    n^.FrustumTest := fcInside;
    if n^.DivN then
    begin
      if n^.Items.Count > 0 then
      List.Add(n);
    end
    else
    for nx := n^.MinX to n^.MaxX do
    for ny := n^.MinY to n^.MaxY do
    for nz := n^.MinZ to n^.MaxZ do
    if n^.SubNodes[nx, ny, nz].TotalItemCount > 0 then
    IncludeNode(@n^.SubNodes[nx, ny, nz]);
  end;
  procedure CheckNode(const n: POcTreeNode);
    var nx, ny, nz: Integer;
  begin
    n^.FrustumTest := Frustum.FrustumCheckBox(n^.AABox.MinV, n^.AABox.MaxV);
    if (n^.FrustumTest = fcInside) then
    IncludeNode(n)
    else
    if (n^.FrustumTest = fcIntersect) then
    begin
      if n^.DivN then
      begin
        if n^.Items.Count > 0 then
        List.Add(n);
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
  if m_RootNode.TotalItemCount > 0 then
  CheckNode(@m_RootNode);
end;

procedure TG2SceneGraph.OcTreeGetIndices(const AABox: TG2AABox; var IndMinX, IndMinY, IndMinZ, IndMaxX, IndMaxY, IndMaxZ: Integer);
  var sx, sy, sz: Single;
begin
  sx := m_RootNode.AABox.MaxV.x - m_RootNode.AABox.MinV.x;
  sy := m_RootNode.AABox.MaxV.y - m_RootNode.AABox.MinV.y;
  sz := m_RootNode.AABox.MaxV.z - m_RootNode.AABox.MinV.z;
  IndMinX := Min(Max(Trunc((AABox.MinV.x - m_RootNode.AABox.MinV.x) / sx * Length(m_OcTreeGrid)), 0), Length(m_OcTreeGrid));
  IndMaxX := Min(Max(Trunc((AABox.MaxV.x - m_RootNode.AABox.MinV.x) / sx * Length(m_OcTreeGrid)), -1), High(m_OcTreeGrid));
  IndMinY := Min(Max(Trunc((AABox.MinV.y - m_RootNode.AABox.MinV.y) / sy * Length(m_OcTreeGrid[0])), 0), Length(m_OcTreeGrid[0]));
  IndMaxY := Min(Max(Trunc((AABox.MaxV.y - m_RootNode.AABox.MinV.y) / sy * Length(m_OcTreeGrid[0])), -1), High(m_OcTreeGrid[0]));
  IndMinZ := Min(Max(Trunc((AABox.MinV.z - m_RootNode.AABox.MinV.z) / sz * Length(m_OcTreeGrid[0, 0])), 0), Length(m_OcTreeGrid[0, 0]));
  IndMaxZ := Min(Max(Trunc((AABox.MaxV.z - m_RootNode.AABox.MinV.z) / sz * Length(m_OcTreeGrid[0, 0])), -1), High(m_OcTreeGrid[0, 0]));
end;

constructor TG2SceneGraph.Create(const G2Core: TG2Core);
  var i, j: Integer;
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
  m_RootNode.AABox.SetValue(G2Vec3(0, 0, 0), G2Vec3(0, 0, 0));
  Depth := 0;
end;

destructor TG2SceneGraph.Destroy;
begin
  Clear;
  MaterialsClear;
  while Queries.Count > 0 do
  QueryDestroy(PG2SGQuery(Queries[0]));
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
    Result := FindTexture(Name);
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
  var Link: PG2SGQueryLink;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevV, PrevP, PrevW: TG2Mat;
  var PrevViewport: TD3DViewport9;
  var PrevZEnable, PrevZWriteEnable: Boolean;
  var PrevDstBlend, PrevSrcBlend: DWord;
  var PrevCullMode: DWord;
  var VisiblePixels: DWord;
  var DataSize: DWord;
  var ViewPos: TG2Vec3;
  var m: TG2Mat;
begin
  m_OcTreeNodeFetch.Clear;
  OcTreeFetchNodes(Frustum, @m_OcTreeNodeFetch);
  if m_CurFetchID >= High(DWord) - 1 then
  ResetFetchID;
  Inc(m_CurFetchID);
  Query^.QueryLists[G2QL_CHARS].Clear;
  Query^.QueryLists[G2QL_LIGHTS].Clear;
  if Query^.OcclusionCullEnable then
  begin
    ViewPos.SetValue(-Frustum.RefV^.e30, -Frustum.RefV^.e31, -Frustum.RefV^.e32);
    ViewPos := ViewPos.Transform3x3(Frustum.RefV^.Transpose);
    Core.Graphics.Device.GetTransform(D3DTS_VIEW, PG2MatRef(@PrevV)^);
    Core.Graphics.Device.GetTransform(D3DTS_PROJECTION, PG2MatRef(@PrevP)^);
    Core.Graphics.Device.GetTransform(D3DTS_WORLD, PG2MatRef(@PrevW)^);
    Core.Graphics.Device.SetTransform(D3DTS_VIEW, Frustum.RefV^);
    Core.Graphics.Device.SetTransform(D3DTS_PROJECTION, Frustum.RefP^);
    Core.Graphics.Device.GetRenderTarget(0, PrevRT);
    Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
    PrevViewport := Core.Graphics.GetViewPort;
    Core.Graphics.Device.SetRenderTarget(0, Query^.OcclusionRT.Surface);
    Core.Graphics.Device.SetDepthStencilSurface(Query^.OcclusionDS.Surface);
    Core.Graphics.SetViewPort(Query^.OcclusionRT.ViewPort^);
    Core.Graphics.Device.Clear(0, nil, D3DCLEAR_ZBUFFER, 0, 1, 0);
    Core.Graphics.Device.SetTexture(0, nil);
    Core.Graphics.Device.SetVertexDeclaration(DataFormats.DeclGeom);
    PrevZEnable := Core.Graphics.RenderStates.ZEnable;
    PrevZWriteEnable := Core.Graphics.RenderStates.ZWriteEnable;
    PrevSrcBlend := Core.Graphics.RenderStates.SrcBlend;
    PrevDstBlend := Core.Graphics.RenderStates.DestBlend;
    Core.Graphics.RenderStates.SrcBlend := D3DBLEND_ZERO;
    Core.Graphics.RenderStates.DestBlend := D3DBLEND_ZERO;
    PrevCullMode := Core.Graphics.RenderStates.CullMode;
    Core.Graphics.RenderStates.ZEnable := True;
    Core.Graphics.RenderStates.ZWriteEnable := True;
    Core.Graphics.RenderStates.DepthBias := 0.0001;
    for i := 0 to Query^.QueryLists[G2QL_GEOMS].Count - 1 do
    begin
      Core.Graphics.Device.SetTransform(D3DTS_WORLD, TG2SGGeom(Query^.QueryLists[G2QL_GEOMS][i]).Transform);
      Core.Graphics.Device.SetStreamSource(0, TG2SGGeom(Query^.QueryLists[G2QL_GEOMS][i]).VB, 0, DataFormats.VertexStrideGeom);
      Core.Graphics.Device.SetIndices(TG2SGGeom(Query^.QueryLists[G2QL_GEOMS][i]).IB);
      Core.Graphics.Device.DrawIndexedPrimitive(
        D3DPT_TRIANGLELIST, 0,
        0, TG2SGGeom(Query^.QueryLists[G2QL_GEOMS][i]).VCount,
        0, TG2SGGeom(Query^.QueryLists[G2QL_GEOMS][i]).FCount
      );
    end;
    Query^.QueryLists[G2QL_GEOMS].Clear;
    m.SetIdentity;
    Core.Graphics.RenderStates.DepthBias := 0;
    Core.Graphics.RenderStates.ZWriteEnable := False;
    Core.Graphics.RenderStates.CullMode := D3DCULL_NONE;
    Core.Graphics.Device.SetTransform(D3DTS_WORLD, m);
    for i := 0 to m_OcTreeNodeFetch.Count - 1 do
    for j := 0 to POcTreeNode(m_OcTreeNodeFetch[i])^.Items.Count - 1 do
    if PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.FetchID < m_CurFetchID then
    begin
      PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.FetchID := m_CurFetchID;
      Frame := PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame;
      Link := PG2SGQueryLink(Frame.OcTreeItem.QueryLinks[Query^.ID]);
      if Frame.Render
      and (
        (POcTreeNode(m_OcTreeNodeFetch[i])^.FrustumTest = fcInside)
        or Frustum.BoxInFrustum(Frame.AABox)
      ) then
      begin
        if Link^.OcclusionTest then
        begin
          DataSize := Link^.OcclusionQuery.GetDataSize;
          while Link^.OcclusionQuery.GetData(@VisiblePixels, DataSize, D3DGETDATA_FLUSH) = S_FALSE do ;
          Link^.OcclusionVisible := (VisiblePixels > 0);
        end;
        if Link^.OcclusionVisible then
        begin
          Link^.OcclusionTest := False;
          if Query^.DistanceSortEnable then
          Query^.QueryLists[Frame.QueryListID].Add(Frame, Frustum.Planes[4].DistanceToPoint((Frame.AABox.MaxV + Frame.AABox.MinV) * 0.5))
          else
          Query^.QueryLists[Frame.QueryListID].Add(Frame);
          if GetTickCount - Link^.OcclusioonTestTime > 1000 then
          Link^.OcclusionVisible := False;
        end;
        if not Link^.OcclusionVisible then
        begin
          Link^.OcclusioonTestTime := GetTickCount;
          if Frame.AABox.PointInside(ViewPos) then
          Link^.OcclusionVisible := True
          else
          begin
            Link^.OcclusionTest := True;
            Link^.OcclusionQuery.Issue(D3DISSUE_BEGIN);
            m_Prim3D.DrawBox(
              Frame.AABox.MinV.x,
              Frame.AABox.MinV.y,
              Frame.AABox.MinV.z,
              Frame.AABox.MaxV.x - Frame.AABox.MinV.x,
              Frame.AABox.MaxV.y - Frame.AABox.MinV.y,
              Frame.AABox.MaxV.z - Frame.AABox.MinV.z,
              $ffffffff
            );
            Link^.OcclusionQuery.Issue(D3DISSUE_END);
          end;
        end;

      end;
    end;
    Core.Graphics.RenderStates.CullMode := PrevCullMode;
    Core.Graphics.RenderStates.SrcBlend := PrevSrcBlend;
    Core.Graphics.RenderStates.DestBlend := PrevDstBlend;
    Core.Graphics.RenderStates.ZEnable := PrevZEnable;
    Core.Graphics.RenderStates.ZWriteEnable := PrevZWriteEnable;
    Core.Graphics.Device.SetRenderTarget(0, PrevRT);
    Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
    Core.Graphics.Device.SetTransform(D3DTS_VIEW, PrevV);
    Core.Graphics.Device.SetTransform(D3DTS_PROJECTION, PrevP);
    Core.Graphics.Device.SetTransform(D3DTS_WORLD, PrevW);
  end
  else
  begin
    Query^.QueryLists[G2QL_GEOMS].Clear;
    for i := 0 to m_OcTreeNodeFetch.Count - 1 do
    for j := 0 to POcTreeNode(m_OcTreeNodeFetch[i])^.Items.Count - 1 do
    if PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.FetchID < m_CurFetchID then
    begin
      PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.FetchID := m_CurFetchID;
      Frame := PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame;
      if Frame.Render
      and (
        (POcTreeNode(m_OcTreeNodeFetch[i])^.FrustumTest = fcInside)
        or Frustum.BoxInFrustum(Frame.AABox)
      ) then
      begin
        if Frame.QueryListID > -1 then
        begin
          if Query^.DistanceSortEnable then
          Query^.QueryLists[Frame.QueryListID].Add(Frame, Frustum.Planes[4].DistanceToPoint((Frame.AABox.MaxV + Frame.AABox.MinV) * 0.5))
          else
          Query^.QueryLists[Frame.QueryListID].Add(Frame);
        end;
      end;
    end;
  end;
  Query^.Stats.ObjectsRendered := Query^.QueryLists[G2QL_GEOMS].Count;
  Query^.Stats.ObjectsCulled := Geoms.Count - Query^.QueryLists[G2QL_GEOMS].Count;
end;

function TG2SceneGraph.CollideSphere(var s: TG2Sphere): Boolean;
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
  if m_CurFetchID >= High(DWord) - 1 then
  ResetFetchID;
  Inc(m_CurFetchID);
  m_GeomCollideList.Clear;
  for i := 0 to m_OcTreeNodeFetch.Count - 1 do
  for j := 0 to POcTreeNode(m_OcTreeNodeFetch[i])^.Items.Count - 1 do
  if (PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame is TG2SGGeom)
  and (TG2SGGeom(PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame).Collide)
  and (PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.FetchID < m_CurFetchID) then
  begin
    PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.FetchID := m_CurFetchID;
    if AABox.Intersect(PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame.AABox) then
    m_GeomCollideList.Add(PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame);
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

function TG2SceneGraph.CollideChar(const c: TG2SGChar): Boolean;
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
  s.SetValue(c.Transform.GetTranslation, c.Radius);
  s.C := s.C - c.Step;
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
  if m_CurFetchID >= High(DWord) - 1 then
  ResetFetchID;
  Inc(m_CurFetchID);
  m_GeomCollideList.Clear;
  for i := 0 to m_OcTreeNodeFetch.Count - 1 do
  for j := 0 to POcTreeNode(m_OcTreeNodeFetch[i])^.Items.Count - 1 do
  if (PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame is TG2SGGeom)
  and (TG2SGGeom(PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame).Collide)
  and (PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.FetchID < m_CurFetchID) then
  begin
    PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.FetchID := m_CurFetchID;
    if AABoxChar.Intersect(PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame.AABox) then
    m_GeomCollideList.Add(PG2SGOcTreeItem(POcTreeNode(m_OcTreeNodeFetch[i])^.Items[j])^.Frame);
  end;
  for i := 0 to m_GeomCollideList.Count - 1 do
  begin
    Geom := TG2SGGeom(m_GeomCollideList[i]);
    for j := 0 to Geom.FCount - 1 do
    begin
      if AABoxChar.Intersect(Geom.Collider.Faces[j].AABox) then
      begin
        if Geom.Collider.Faces[j].Plane.N.Dot(StepNorm) < -0.7 then
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
                ShiftVec := -StepNorm * (StepLen - d);
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

procedure TG2SceneGraph.Update;
  var i: Integer;
begin
  for i := 0 to Nodes.Count - 1 do
  TG2SGNode(Nodes[i]).Update;
  for i := 0 to Chars.Count - 1 do
  if TG2SGChar(Chars[i]).Collide then
  CollideChar(TG2SGChar(Chars[i]));
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
  var Link: PG2SGQueryLink;
begin
  inherited Create(SceneGraph);
  m_SceneGraph.Frames.Add(Self);
  OcTreeItem.Frame := Self;
  OcTreeItem.FetchID := 0;
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
  var Link: PG2SGQueryLink;
begin
  for i := 0 to m_SceneGraph.Queries.Count - 1 do
  begin
    Link := PG2SGQueryLink(OcTreeItem.QueryLinks[PG2SGQuery(m_SceneGraph.Queries[i])^.ID]);
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

constructor TG2SGGeom.Create(const SceneGraph: TG2SceneGraph);
begin
  inherited Create(SceneGraph);
  m_SceneGraph.Geoms.Add(Self);
  m_Render := False;
  m_Collide := False;
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
  var i: Integer;
  var u, v, d: Single;
  var v0, v1, v2: TG2Vec3;
begin
  if m_Collide then
  for i := 0 to High(Collider.Faces) do
  begin
    if r.IntersectAABox(Collider.Faces[i].AABox)
    and r.IntersectTri(
      Collider.Vertices[Collider.Faces[i].Indices[0]],
      Collider.Vertices[Collider.Faces[i].Indices[1]],
      Collider.Vertices[Collider.Faces[i].Indices[2]],
      u, v, d
    ) then
    begin
      Result := True;
      Exit;
    end;
  end
  else
  begin
    for i := 0 to FCount - 1 do
    begin
      v0 := Vertices[Faces[i][0]].Position.Transform4x3(Transform);
      v1 := Vertices[Faces[i][1]].Position.Transform4x3(Transform);
      v2 := Vertices[Faces[i][2]].Position.Transform4x3(Transform);
      if r.IntersectTri(
        Collider.Vertices[Collider.Faces[i].Indices[0]],
        Collider.Vertices[Collider.Faces[i].Indices[1]],
        Collider.Vertices[Collider.Faces[i].Indices[2]],
        u, v, d
      ) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TG2SGGeom.IntersectRay(const r: TG2Ray; var U, V, Dist: Single): Boolean;
  var i: Integer;
  var cu, cv, d: Single;
  var v0, v1, v2: TG2Vec3;
begin
  Result := False;
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
          U := cu;
          V := cv;
          Dist := d;
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