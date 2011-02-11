unit G2SpriteEngine;

interface

{$include Gen2.inc}

uses
  Windows,
  Types,
  DXTypes,
  Direct3D9,
  D3DX9,
  Gen2,
  G2Math;

type
  TG2SpriteEngine = class;
  TG2Sprite = class;
  TG2SpriteCollider = class;
  TG2SpriteImage = class;

  TG2SpriteGroup = record
  public
    var MinV, MaxV: TPoint;
    var Sprites: array of TG2Sprite;
    var SpriteCount: Integer;
    var MaxSize: Integer;
  end;
  PG2SpriteGroup = ^TG2SpriteGroup;

  TG2SpriteEngine = class(TG2Module)
  strict private
    type TVertex = record
      var x, y, z, rhw: Single;
      var Color: TG2Color;
      var tu, tv: Single;
    end;
    type TVertexArray = array[Word] of TVertex;
    type PVertexArray = ^TVertexArray;
    type TRenderItem = record
    public
      var Order: Integer;
      var Texture: TG2Texture2D;
      var BlendMode: TG2BlendMode;
      var Vertices: array[0..3] of TVertex;
    end;
    type PRenderItem = ^TRenderItem;
    type TQuad = record
    private
      var ListID: Integer;
      var MinV, MaxV: TPoint;
      var Group: Pointer;
    public
      var Order: Integer;
      var Texture: TG2Texture2D;
      var BlendMode: TG2BlendMode;
      var Vertices: array[0..3] of TVertex;
    end;
    type PQuad = ^TQuad;
    type TQuadGroup = record
    public
      var MinV, MaxV: TPoint;
      var Quads: array of PQuad;
      var QuadCount: Integer;
      var MaxSize: Integer;
    end;
    type PQuadGroup = ^TQuadGroup;
    var m_TimeStart: DWord;
    var m_TimeCur: DWord;
    var m_SpriteGroups: array of PG2SpriteGroup;
    var m_SpriteGroupCount: Integer;
    var m_DefMaxGroupSize: Integer;
    var m_MaxQuads: Integer;
    var m_VB: IDirect3DVertexBuffer9;
    var m_IB: IDirect3DIndexBuffer9;
    var m_RenderQueue: array of PRenderItem;
    var m_RenderQueueSize: Integer;
    var m_Quads: array of PQuad;
    var m_QuadCount: Integer;
    var m_QuadGroups: array of PQuadGroup;
    var m_QuadGroupCount: Integer;
    var m_Colliders: array of TG2SpriteCollider;
    var m_ColliderCount: Integer;
    var m_Prim2D: TG2Primitives2D;
    var m_PhysDebug: Boolean;
    function GetQuad(const Index: Integer): PQuad; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RenderCollider(const c: TG2SpriteCollider);
  private
    var DeadSprites: array of TG2Sprite;
    var DeadSpriteCount: Integer;
    procedure EnableSprite(const Sprite: TG2Sprite);
    procedure DisableSprite(const Sprite: TG2Sprite);
  public
    var Sprites: array of TG2Sprite;
    var SpriteCount: Integer;
    var Gravity: TG2Vec2;
    property CurTime: DWord read m_TimeCur;
    property Quads[const Index: Integer]: PQuad read GetQuad;
    property QuadCount: Integer read m_QuadCount;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    procedure Update;
    procedure Render;
    function AddQuad(
      const v0, v1, v2, v3, t0, t1, t2, t3: TG2Vec2;
      const Order: Integer;
      const Texture: TG2Texture2D;
      const BlendMode: TG2BlendMode
    ): PQuad;
    procedure AddCollider(const c: TG2SpriteCollider);
    procedure RemoveCollider(const c: TG2SpriteCollider);
  end;

  TG2Sprite = class
  strict private
    var m_Enabled: Boolean;
    var m_Rotation: Single;
    var m_Scale: Single;
    var m_Collider: TG2SpriteCollider;
    procedure SetScale(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetEnabled(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var ListID: Integer;
    var HSW: Integer;
    var HSH: Integer;
    var Engine: TG2SpriteEngine;
    var Group: PG2SpriteGroup;
    var Images: array of TG2SpriteImage;
    var ImageCount: Integer;
    var NeedUpdate: Boolean;
    var Dead: Boolean;
    procedure ComputeSize;
  public
    var Position: TG2Vec2;
    property Rotation: Single read m_Rotation write m_Rotation;
    property Scale: Single read m_Scale write SetScale;
    property Enabled: Boolean read m_Enabled write SetEnabled;
    property Collider: TG2SpriteCollider read m_Collider write m_Collider;
    constructor Create(const SpriteEngine: TG2SpriteEngine);
    destructor Destroy; override;
    procedure AddImage(const Image: TG2SpriteImage); overload;
    function AddImage: TG2SpriteImage; overload;
    procedure Update; virtual;
    procedure Die;
  end;

  TG2SpriteCollider = class
  strict private
    type TList = record
    strict private
      m_Arr: array of Pointer;
      m_Count: Integer;
      procedure SetElement(const Index: Integer; const Value: Pointer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      function GetElement(const Index: Integer): Pointer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      procedure SetCapacity(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      function GetCapacity: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    public
      property e[const Index: Integer]: Pointer read GetElement write SetElement; default;
      property Count: Integer read m_Count write m_Count;
      property Capacity: Integer read GetCapacity write SetCapacity;
      procedure Add(const Ptr: Pointer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      procedure Remove(const Ptr: Pointer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    end;
    type PList = ^TList;
    type TCont = record
    public
      var p: TG2Vec2;
      var n: TG2Vec2;
    end;
    type PCont = ^TCont;
    type TContGroup = record
    strict private
      procedure SetCapacity(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      function GetCapacity: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    public
      var c: array of TCont;
      var Count: Integer;
      property Capacity: Integer read GetCapacity write SetCapacity;
      procedure Add(const Cont: TCont); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    end;
    PContGroup = ^TContGroup;
    type TResp = record
    public
      var n: TG2Vec2;
      var d: Single;
    end;
    type PResp = ^TResp;
    type TColliderType = (ctStatic, ctOriented, ctDynamic);
    var m_Engine: TG2SpriteEngine;
    var m_ColliderType: TColliderType;
    class var RespArr: array of TResp;
    class var RespCount: Integer;
    class var sp1: TList;
    class var sp2: TList;
    class var cg: TContGroup;
  private
    type TShape = class
    protected
      class var AxisArr: array of TG2Vec2;
      class var AxisCount: Integer;
    public
      procedure AxisProject(const Axis: TG2Vec2; var MinP, MaxP: Single); virtual; abstract;
      function Center: TG2Vec2; virtual; abstract;
      function Clone: TShape; virtual; abstract;
      procedure Transform(const Pos: TG2Vec2; const Rot: TG2Mat2; const OutS: TShape); virtual; abstract;
    end;
    type TShapeCircle = class (TShape)
    public
      var c: TG2Vec2;
      var r: Single;
      procedure AxisProject(const Axis: TG2Vec2; var MinP, MaxP: Single); override;
      function Center: TG2Vec2; override;
      function Clone: TShape; override;
      procedure Transform(const Pos: TG2Vec2; const Rot: TG2Mat2; const OutS: TShape); override;
    end;
    type TShapePoly = class (TShape)
      var p: array of TG2Vec2;
      procedure AxisProject(const Axis: TG2Vec2; var MinP, MaxP: Single); override;
      function Center: TG2Vec2; override;
      function Clone: TShape; override;
      procedure Transform(const Pos: TG2Vec2; const Rot: TG2Mat2; const OutS: TShape); override;
      procedure SupportPoints(const n: TG2Vec2; const sp: PList);
      procedure Edge(const EdgeIndex: Integer; var Index1, Index2: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      function EdgeDir(const EdgeIndex: Integer): TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      function EdgeNormal(const EdgeIndex: Integer): TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    end;
    var SpriteEngine: TG2SpriteEngine;
    var Shapes: TList;
    var ShapesCur: TList;
    var ListID: Integer;
    class function AxisSeparate(const Axis: TG2Vec2; const s1, s2: TShape; const r: PResp = nil): Boolean;
    class function IntersectCircles(const c1, c2: TShapeCircle; const r: PResp = nil): Boolean;
    class function IntersectPolys(const p1, p2: TShapePoly; const r: PResp = nil): Boolean;
    class function IntersectPolyCircle(const p: TShapePoly; c: TShapeCircle; const r: PResp = nil): Boolean;
    class function FindMTD: TResp;
    class procedure FindContactsPoly(const n: TG2Vec2; const p1, p2: TShapePoly; const Cont: PContGroup); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    class procedure FindContactsCircle(const n: TG2Vec2; const c1, c2: TShapeCircle; const Cont: PContGroup); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    class procedure FindContactsPolyCircle(const n: TG2Vec2; const p: TShapePoly; const c: TShapeCircle; const Cont: PContGroup); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    class procedure ResolveIntersection(const c1, c2: TG2SpriteCollider; const r: TResp); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    class procedure ResolveContacts(const c1, c2: TG2SpriteCollider; const Contacts: TContGroup);
    class function ResolveForces(
      const N1, N2, F1, F2: TG2Vec2;
      var Direct1: TG2Vec2;
      var Friction1: TG2Vec2;
      var Direct2: TG2Vec2;
      var Friction2: TG2Vec2;
      const MagHardness: Single = 1;
      const MagFriction: Single = 1
    ): Boolean;
  public
    var Pos: TG2Vec2;
    var Ang: Single;
    var VelLin: TG2Vec2;
    var VelAng: Single;
    var Hardness: Single;
    var Friction: Single;
    property ColliderType: TColliderType read m_ColliderType write m_ColliderType;
    constructor Create(const SpriteEngine: TG2SpriteEngine);
    destructor Destroy; override;
    procedure Update;
    procedure Collide(const c: TG2SpriteCollider);
    function GetForceAtPoint(const p: TG2Vec2): TG2Vec2;
    procedure SetForceAtPoint(const p, f: TG2Vec2);
    procedure AddShapePoly(const v: PG2Vec2; const Count: Integer);
    procedure AddShapeCircle(const c: TG2Vec2; const r: Single);
    procedure CenterPos;
  end;

  TG2SpriteImage = class
  strict private
    var m_Playing: Boolean;
    var m_PlayTime: DWord;
    var m_PauseTime: DWord;
  private
    var Engine: TG2SpriteEngine;
    var Textures: array of record
    public
      Texture: TG2Texture2D;
      PatternWidth: Integer;
      PatternHeight: Integer;
      PatternCount: Integer;
    end;
    var TextureCount: Integer;
    var Animations: array of record
    public
      AnimName: AnsiString;
      TexName: AnsiString;
      TexID: Integer;
      NextAnim: AnsiString;
      Loop: Boolean;
      FrameStart: Integer;
      FrameEnd: Integer;
      FrameRate: Single;
    end;
    var AnimationCount: Integer;
    var CurAnimID: Integer;
    var CurFrame: Integer;
    procedure Update;
    function FindAnimID(const Name: AnsiString): Integer;
    function FindTexID(const Name: AnsiString): Integer;
    function GetCurAnim: AnsiString; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCurAnim(const Value: AnsiString); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var BlendMode: TG2BlendMode;
    var Order: Integer;
    var Pos: TPoint;
    var Width: Integer;
    var Height: Integer;
    var Rotation: Single;
    var Scale: Single;
    var FlipLeftRight: Boolean;
    var FlipTopBottom: Boolean;
    property Playing: Boolean read m_Playing;
    property CurAnim: AnsiString read GetCurAnim write SetCurAnim;
    constructor Create(const SpriteEngine: TG2SpriteEngine);
    destructor Destroy; override;
    procedure AddTexture(
      const Texture: TG2Texture2D;
      const PatternWidth: Integer = 0;
      const PatternHeight: Integer = 0;
      const PatternCount: Integer = 1
    );
    procedure AddAnimation(
      const AnimName, TexName, NextAnimName: AnsiString;
      const Loop: Boolean;
      const FrameStart, FrameEnd: Integer;
      const FrameRate: Single
    );
    procedure Play;
    procedure Pause;
  end;

implementation

//TG2SpriteEngine BEGIN
function TG2SpriteEngine.GetQuad(const Index: Integer): PQuad;
begin
  Result := m_Quads[Index];
end;

procedure TG2SpriteEngine.RenderCollider(const c: TG2SpriteCollider);
  var i, j: Integer;
begin
  for i := 0 to c.Shapes.Count - 1 do
  begin
    if TG2SpriteCollider.TShape(c.ShapesCur[i]) is TG2SpriteCollider.TShapeCircle then
    with TG2SpriteCollider.TShapeCircle(c.ShapesCur[i]) do
    m_Prim2D.DrawCircleHollow(c, r - 1, r, $ffff0000)
    else if TG2SpriteCollider.TShape(c.ShapesCur[i]) is TG2SpriteCollider.TShapePoly then
    with TG2SpriteCollider.TShapePoly(c.ShapesCur[i]) do
    for j := 0 to High(p) do
    begin
      if j = High(p) then
      m_Prim2D.DrawLine(p[j], p[0], $ffff0000)
      else
      m_Prim2D.DrawLine(p[j], p[j + 1], $ffff0000);
    end;
  end;
end;

procedure TG2SpriteEngine.EnableSprite(const Sprite: TG2Sprite);
  var i, n: Integer;
  var sl, st, sr, sb: Integer;
begin
  Sprite.ComputeSize;
  sl := Trunc(Sprite.Position.x - Sprite.HSW);
  st := Trunc(Sprite.Position.y - Sprite.HSH);
  sr := Trunc(Sprite.Position.x + Sprite.HSW);
  sb := Trunc(Sprite.Position.y - Sprite.HSH);
  n := -1;
  for i := 0 to m_SpriteGroupCount - 1 do
  begin
    if (
      (
        (sl > m_SpriteGroups[i]^.MinV.X)
        and (sr < m_SpriteGroups[i]^.MinV.x + m_SpriteGroups[i]^.MaxSize)
      ) or (
        (sl > m_SpriteGroups[i]^.MaxV.x - m_SpriteGroups[i]^.MaxSize)
        and (sr < m_SpriteGroups[i]^.MaxV.x)
      )
    ) and (
      (
        (st > m_SpriteGroups[i]^.MinV.y)
        and (sb < m_SpriteGroups[i]^.MinV.y + m_SpriteGroups[i]^.MaxSize)
      ) or (
        (st > m_SpriteGroups[i]^.MaxV.y - m_SpriteGroups[i]^.MaxSize)
        and (sb < m_SpriteGroups[i]^.MaxV.y)
      )
    ) then
    begin
      n := i;
      Break;
    end;
  end;
  if n = -1 then
  begin
    if Length(m_SpriteGroups) <= m_SpriteGroupCount then
    SetLength(m_SpriteGroups, Length(m_SpriteGroups) + 16);
    n := m_SpriteGroupCount;
    Inc(m_SpriteGroupCount);
    New(m_SpriteGroups[n]);
    m_SpriteGroups[n]^.MaxSize := m_DefMaxGroupSize;
    m_SpriteGroups[n]^.MinV.x := sl;
    m_SpriteGroups[n]^.MinV.y := st;
    m_SpriteGroups[n]^.MaxV.x := sr;
    m_SpriteGroups[n]^.MaxV.y := sb;
    m_SpriteGroups[n]^.SpriteCount := 0;
  end
  else
  begin
    if sl < m_SpriteGroups[n]^.MinV.x then m_SpriteGroups[n]^.MinV.x := sl;
    if st < m_SpriteGroups[n]^.MinV.y then m_SpriteGroups[n]^.MinV.y := st;
    if sr > m_SpriteGroups[n]^.MaxV.x then m_SpriteGroups[n]^.MaxV.x := sr;
    if sb > m_SpriteGroups[n]^.MaxV.y then m_SpriteGroups[n]^.MaxV.y := sb;
  end;
  if Length(m_SpriteGroups[n]^.Sprites) <= m_SpriteGroups[n]^.SpriteCount then
  SetLength(m_SpriteGroups[n]^.Sprites, Length(m_SpriteGroups[n]^.Sprites) + 32);
  m_SpriteGroups[n]^.Sprites[m_SpriteGroups[n]^.SpriteCount] := Sprite;
  Sprite.Group := m_SpriteGroups[n];
  Inc(m_SpriteGroups[n]^.SpriteCount);
end;

procedure TG2SpriteEngine.DisableSprite(const Sprite: TG2Sprite);
  var i, j: Integer;
  var g: PG2SpriteGroup;
begin
  if Sprite.Enabled then
  begin
    g := Sprite.Group;
    for i := 0 to g^.SpriteCount - 1 do
    if g^.Sprites[i] = Sprite then
    begin
      for j := i to g^.SpriteCount - 2 do
      g^.Sprites[j] := g^.Sprites[j + 1];
      Dec(g^.SpriteCount);
      Sprite.Group := nil;
      Exit;
    end;
  end;
end;

function TG2SpriteEngine.Initialize(const G2Core: TG2Core): TG2Result;
  var Indices: PG2Index16Array;
  var i: Integer;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_TimeStart := GetTickCount;
  m_TimeCur := 0;
  m_SpriteGroupCount := 0;
  m_DefMaxGroupSize := 256;
  m_MaxQuads := 1024;
  m_ColliderCount := 0;
  m_PhysDebug := True;
  SpriteCount := 0;
  DeadSpriteCount := 0;
  Gravity.SetValue(0, 0);
  Core.Graphics.Device.CreateVertexBuffer(
    m_MaxQuads * SizeOf(TVertex) * 4,
    D3DUSAGE_WRITEONLY,
    D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1,
    D3DPOOL_MANAGED,
    m_VB,
    nil
  );
  Core.Graphics.Device.CreateIndexBuffer(
    m_MaxQuads * 6 * 2,
    D3DUSAGE_WRITEONLY,
    D3DFMT_INDEX16,
    D3DPOOL_MANAGED,
    m_IB,
    nil
  );
  m_IB.Lock(0, m_MaxQuads * 6 * 2, Pointer(Indices), D3DLOCK_DISCARD);
  for i := 0 to m_MaxQuads - 1 do
  begin
    Indices[i * 6 + 0] := i * 4 + 0;
    Indices[i * 6 + 1] := i * 4 + 1;
    Indices[i * 6 + 2] := i * 4 + 2;
    Indices[i * 6 + 3] := i * 4 + 2;
    Indices[i * 6 + 4] := i * 4 + 1;
    Indices[i * 6 + 5] := i * 4 + 3;
  end;
  m_IB.Unlock;
  m_Prim2D := TG2Primitives2D.Create;
  m_Prim2D.Initialize(Core);
end;

function TG2SpriteEngine.Finalize: TG2Result;
  var i: Integer;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  m_Prim2D.Finalize;
  m_Prim2D.Free;
  SafeRelease(m_VB);
  SafeRelease(m_IB);
  while m_ColliderCount > 0 do
  m_Colliders[0].Free;
  for i := 0 to High(m_RenderQueue) do
  Dispose(m_RenderQueue[i]);
  for i := 0 to m_SpriteGroupCount - 1 do
  Dispose(m_SpriteGroups[i]);
  while SpriteCount > 0 do
  Sprites[0].Free;
  for i := 0 to m_QuadGroupCount - 1 do
  Dispose(m_QuadGroups[i]);
  for i := 0 to m_QuadCount - 1 do
  Dispose(m_Quads[i]);
end;

procedure TG2SpriteEngine.Update;
  var i, j: Integer;
  var bl, bt, br, bb: Integer;
begin
  m_TimeCur := GetTickCount - m_TimeStart;
  for i := 0 to m_ColliderCount - 1 do
  m_Colliders[i].Update;
  for i := 0 to m_ColliderCount - 1 do
  for j := i + 1 to m_ColliderCount - 1 do
  m_Colliders[i].Collide(m_Colliders[j]);
  for i := 0 to SpriteCount - 1 do
  begin
    if Assigned(Sprites[i].Collider) then
    begin
      Sprites[i].Position := Sprites[i].Collider.Pos;
      Sprites[i].Rotation := Sprites[i].Collider.Ang;
    end;
    for j := 0 to Sprites[i].ImageCount - 1 do
    Sprites[i].Images[j].Update;
    Sprites[i].Update;
    bl := Round(Sprites[i].Position.x - Sprites[i].HSW);
    bt := Round(Sprites[i].Position.y - Sprites[i].HSH);
    br := Round(Sprites[i].Position.x + Sprites[i].HSW);
    bb := Round(Sprites[i].Position.y + Sprites[i].HSH);
    if bl < Sprites[i].Group^.MinV.x then Sprites[i].Group^.MinV.x := bl;
    if bt < Sprites[i].Group^.MinV.y then Sprites[i].Group^.MinV.y := bt;
    if br > Sprites[i].Group^.MaxV.x then Sprites[i].Group^.MaxV.x := br;
    if bb > Sprites[i].Group^.MaxV.y then Sprites[i].Group^.MaxV.y := bb;
  end;
  for i := 0 to DeadSpriteCount - 1 do
  DeadSprites[i].Free;
  DeadSpriteCount := 0;
end;

procedure TG2SpriteEngine.Render;
  var i, j, im, k, n: Integer;
  var l, h, dif, m, v, qc: Integer;
  var tx0, ty0, tx1, ty1, tsx, tsy: Single;
  var SR: TRect;
  var spr: TG2Sprite;
  var img: TG2SpriteImage;
  var qi: PRenderItem;
  var hw, hh: Single;
  var mat: TG2Mat;
  var CurTexture: TG2Texture2D;
  var CurBlendMode: TG2BlendMode;
  var PrevBlendMode: TG2BlendMode;
  var CurQuad: Integer;
  var LastQuad: Integer;
  var Vertices: PVertexArray;
  var q: PQuad;
begin
  m_RenderQueueSize := 0;
  SR := Rect(0, 0, Core.Graphics.Params.Width, Core.Graphics.Params.Height);
  for i := 0 to m_SpriteGroupCount - 1 do
  if (m_SpriteGroups[i].MaxV.X > SR.Left)
  and (m_SpriteGroups[i].MinV.X < SR.Right)
  and (m_SpriteGroups[i].MaxV.Y > SR.Top)
  and (m_SpriteGroups[i].MinV.Y < SR.Bottom) then
  begin
    for j := 0 to m_SpriteGroups[i].SpriteCount - 1 do
    begin
      spr := m_SpriteGroups[i].Sprites[j];
      if (spr.Position.x + spr.HSW > SR.Left)
      and (spr.Position.x - spr.HSW < SR.Right)
      and (spr.Position.y + spr.HSH > SR.Top)
      and (spr.Position.y - spr.HSH < SR.Bottom) then
      for im := 0 to spr.ImageCount - 1 do
      begin
        img := spr.Images[im];
        if Length(m_RenderQueue) <= m_RenderQueueSize then
        begin
          n := Length(m_RenderQueue);
          SetLength(m_RenderQueue, n + 64);
          for k := n to High(m_RenderQueue) do
          New(m_RenderQueue[k]);
        end;
        qi := m_RenderQueue[m_RenderQueueSize];
        qi^.Order := img.Order;
        qi^.Texture := img.Textures[img.Animations[img.CurAnimID].TexID].Texture;
        n := img.Textures[img.Animations[img.CurAnimID].TexID].Texture.Width div img.Textures[img.Animations[img.CurAnimID].TexID].PatternWidth;
        if n > 0 then
        begin
          l := img.CurFrame mod n;
          h := img.CurFrame div n;
        end
        else
        begin
          l := 0;
          h := 0;
        end;
        tsx := img.Textures[img.Animations[img.CurAnimID].TexID].PatternWidth / img.Textures[img.Animations[img.CurAnimID].TexID].Texture.RealWidth;
        tsy := img.Textures[img.Animations[img.CurAnimID].TexID].PatternHeight / img.Textures[img.Animations[img.CurAnimID].TexID].Texture.RealHeight;
        tx0 := tsx * l; tx1 := tsx * (l + 1);
        ty0 := tsy * h; ty1 := tsy * (h + 1);
        qi^.BlendMode := img.BlendMode;
        hw := img.Width * 0.5 * img.Scale;
        hh := img.Height * 0.5 * img.Scale;
        mat.SetRotationZ(img.Rotation);
        mat.Translate(G2Vec3(img.Pos, 0));
        mat.Scale(spr.Scale, spr.Scale, 0);
        mat.RotateZ(spr.Rotation);
        mat.Translate(G2Vec3(spr.Position, 0));
        qi^.Vertices[0].x := -hw;
        qi^.Vertices[0].y := -hh;
        qi^.Vertices[0].z := 0;
        qi^.Vertices[0].rhw := 1;
        qi^.Vertices[0].Color := $ffffffff;
        qi^.Vertices[0].tu := tx0;
        qi^.Vertices[0].tv := ty0;
        qi^.Vertices[1].x := hw;
        qi^.Vertices[1].y := -hh;
        qi^.Vertices[1].z := 0;
        qi^.Vertices[1].rhw := 1;
        qi^.Vertices[1].Color := $ffffffff;
        qi^.Vertices[1].tu := tx1;
        qi^.Vertices[1].tv := ty0;
        qi^.Vertices[2].x := -hw;
        qi^.Vertices[2].y := hh;
        qi^.Vertices[2].z := 0;
        qi^.Vertices[2].rhw := 1;
        qi^.Vertices[2].Color := $ffffffff;
        qi^.Vertices[2].tu := tx0;
        qi^.Vertices[2].tv := ty1;
        qi^.Vertices[3].x := hw;
        qi^.Vertices[3].y := hh;
        qi^.Vertices[3].z := 0;
        qi^.Vertices[3].rhw := 1;
        qi^.Vertices[3].Color := $ffffffff;
        qi^.Vertices[3].tu := tx1;
        qi^.Vertices[3].tv := ty1;
        D3DXVec2TransformCoordArray(
          @qi^.Vertices[0], SizeOf(TVertex),
          @qi^.Vertices[0], SizeOf(TVertex),
          mat, 4
        );
        l := 0;
        h := m_RenderQueueSize - 1;
        while l <= h do
        begin
          m := (l + h) div 2;
          if m_RenderQueue[m]^.Order = qi^.Order then
          begin
            if m_RenderQueue[m]^.Texture = qi^.Texture then
            dif := DWord(m_RenderQueue[m]^.BlendMode) - DWord(qi^.BlendMode)
            else
            dif := DWord(m_RenderQueue[m]^.Texture) - DWord(qi^.Texture);
          end
          else
          dif := m_RenderQueue[m]^.Order - qi^.Order;
          if dif < 0 then l := m + 1
          else h := m - 1;
        end;
        for n := m_RenderQueueSize downto l + 1 do
        m_RenderQueue[n] := m_RenderQueue[n - 1];
        m_RenderQueue[l] := qi;
        Inc(m_RenderQueueSize);
      end;
    end;
  end;
  for i := 0 to m_QuadGroupCount - 1 do
  if (m_QuadGroups[i].MaxV.X > SR.Left)
  and (m_QuadGroups[i].MinV.X < SR.Right)
  and (m_QuadGroups[i].MaxV.Y > SR.Top)
  and (m_QuadGroups[i].MinV.Y < SR.Bottom) then
  begin
    for j := 0 to m_QuadGroups[i].QuadCount - 1 do
    begin
      q := m_QuadGroups[i].Quads[j];
      if (q^.MaxV.x > SR.Left)
      and (q^.MinV.x < SR.Right)
      and (q^.MaxV.y > SR.Top)
      and (q^.MinV.y < SR.Bottom) then
      begin
        if Length(m_RenderQueue) <= m_RenderQueueSize then
        begin
          n := Length(m_RenderQueue);
          SetLength(m_RenderQueue, n + 64);
          for k := n to High(m_RenderQueue) do
          New(m_RenderQueue[k]);
        end;
        qi := m_RenderQueue[m_RenderQueueSize];
        qi^.Order := q^.Order;
        qi^.Texture := q^.Texture;
        qi^.BlendMode := q^.BlendMode;
        for v := 0 to 3 do
        qi^.Vertices[v] := q^.Vertices[v];
        l := 0;
        h := m_RenderQueueSize - 1;
        while l <= h do
        begin
          m := (l + h) div 2;
          if m_RenderQueue[m]^.Order = qi^.Order then
          begin
            if m_RenderQueue[m]^.Texture = qi^.Texture then
            dif := DWord(m_RenderQueue[m]^.BlendMode) - DWord(qi^.BlendMode)
            else
            dif := DWord(m_RenderQueue[m]^.Texture) - DWord(qi^.Texture);
          end
          else
          dif := m_RenderQueue[m]^.Order - qi^.Order;
          if dif < 0 then l := m + 1
          else h := m - 1;
        end;
        for n := m_RenderQueueSize downto l + 1 do
        m_RenderQueue[n] := m_RenderQueue[n - 1];
        m_RenderQueue[l] := qi;
        Inc(m_RenderQueueSize);
      end;
    end;
  end;
  CurTexture := nil;
  CurBlendMode := $ffffffff;
  CurQuad := 0;
  LastQuad := 0;
  if m_RenderQueueSize > 0 then
  begin
    CurTexture := m_RenderQueue[0]^.Texture;
    CurBlendMode := m_RenderQueue[0]^.BlendMode;
    Core.Graphics.Device.SetTexture(0, CurTexture.Texture);
    Core.Graphics.RenderStates.SrcBlend := CurBlendMode.ColSrc;
    Core.Graphics.RenderStates.DestBlend := CurBlendMode.ColDst;
    if CurBlendMode.SeparateAlpha then
    begin
      Core.Graphics.RenderStates.SrcBlendAlpha := CurBlendMode.AlphaSrc;
      Core.Graphics.RenderStates.DestBlendAlpha := CurBlendMode.AlphaDst;
    end;
    Core.Graphics.Device.SetFVF(D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1);
    Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
    Core.Graphics.Device.SetIndices(m_IB);
    for i := 0 to m_RenderQueueSize - 1 do
    begin
      qc := CurQuad - LastQuad;
      if (
        (CurTexture <> m_RenderQueue[i]^.Texture)
        or (CurBlendMode <> m_RenderQueue[i]^.BlendMode)
        or (qc >= m_MaxQuads)
      )
       then
      begin
        m_VB.Lock(0, qc * 4 * SizeOf(TVertex), Pointer(Vertices), D3DLOCK_DISCARD);
        v := 0;
        for j := LastQuad to CurQuad - 1 do
        begin
          Vertices^[v + 0] := m_RenderQueue[j].Vertices[0];
          Vertices^[v + 1] := m_RenderQueue[j].Vertices[1];
          Vertices^[v + 2] := m_RenderQueue[j].Vertices[2];
          Vertices^[v + 3] := m_RenderQueue[j].Vertices[3];
          Inc(v, 4);
        end;
        m_VB.Unlock;
        Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, v, 0, qc * 2);
        LastQuad := CurQuad;
        if CurTexture <> m_RenderQueue[i]^.Texture then
        begin
          CurTexture := m_RenderQueue[i]^.Texture;
          Core.Graphics.Device.SetTexture(0, CurTexture.Texture);
        end;
        if CurBlendMode <> m_RenderQueue[i]^.BlendMode then
        begin
          CurBlendMode := m_RenderQueue[i]^.BlendMode;
          Core.Graphics.RenderStates.SrcBlend := CurBlendMode.ColSrc;
          Core.Graphics.RenderStates.DestBlend := CurBlendMode.ColDst;
          if CurBlendMode.SeparateAlpha then
          begin
            Core.Graphics.RenderStates.SrcBlendAlpha := CurBlendMode.AlphaSrc;
            Core.Graphics.RenderStates.DestBlendAlpha := CurBlendMode.AlphaDst;
          end;
        end;
      end;
      Inc(CurQuad);
    end;
    if CurQuad > LastQuad then
    begin
      qc := CurQuad - LastQuad;
      m_VB.Lock(0, qc * 4 * SizeOf(TVertex), Pointer(Vertices), D3DLOCK_DISCARD);
      v := 0;
      for j := LastQuad to CurQuad - 1 do
      begin
        Vertices^[v + 0] := m_RenderQueue[j].Vertices[0];
        Vertices^[v + 1] := m_RenderQueue[j].Vertices[1];
        Vertices^[v + 2] := m_RenderQueue[j].Vertices[2];
        Vertices^[v + 3] := m_RenderQueue[j].Vertices[3];
        Inc(v, 4);
      end;
      m_VB.Unlock;
      Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, v, 0, qc * 2);
    end;
  end;
  if m_PhysDebug then
  begin
    Core.Graphics.Device.SetTexture(0, nil);
    for i := 0 to m_ColliderCount - 1 do
    RenderCollider(m_Colliders[i]);
  end;
end;

function TG2SpriteEngine.AddQuad(
      const v0, v1, v2, v3, t0, t1, t2, t3: TG2Vec2;
      const Order: Integer;
      const Texture: TG2Texture2D;
      const BlendMode: TG2BlendMode
    ): PQuad;
  var i, n: Integer;
begin
  if Length(m_Quads) <= m_QuadCount then
  SetLength(m_Quads, Length(m_Quads) + 64);
  New(m_Quads[m_QuadCount]);
  Result := m_Quads[m_QuadCount];
  Result^.ListID := m_QuadCount;
  Result^.Order := Order;
  Result^.Texture := Texture;
  Result^.BlendMode := BlendMode;
  Result^.Vertices[0].x := v0.x;
  Result^.Vertices[0].y := v0.y;
  Result^.Vertices[0].z := 0;
  Result^.Vertices[0].rhw := 1;
  Result^.Vertices[0].Color := $ffffffff;
  Result^.Vertices[0].tu := t0.x;
  Result^.Vertices[0].tv := t0.y;
  Result^.Vertices[1].x := v1.x;
  Result^.Vertices[1].y := v1.y;
  Result^.Vertices[1].z := 0;
  Result^.Vertices[1].rhw := 1;
  Result^.Vertices[1].Color := $ffffffff;
  Result^.Vertices[1].tu := t1.x;
  Result^.Vertices[1].tv := t1.y;
  Result^.Vertices[2].x := v2.x;
  Result^.Vertices[2].y := v2.y;
  Result^.Vertices[2].z := 0;
  Result^.Vertices[2].rhw := 1;
  Result^.Vertices[2].Color := $ffffffff;
  Result^.Vertices[2].tu := t2.x;
  Result^.Vertices[2].tv := t2.y;
  Result^.Vertices[3].x := v3.x;
  Result^.Vertices[3].y := v3.y;
  Result^.Vertices[3].z := 0;
  Result^.Vertices[3].rhw := 1;
  Result^.Vertices[3].Color := $ffffffff;
  Result^.Vertices[3].tu := t3.x;
  Result^.Vertices[3].tv := t3.y;
  Result^.MinV := Point(Round(Result^.Vertices[0].x), Round(Result^.Vertices[0].y));
  for i := 1 to 3 do
  begin
    if Result^.Vertices[i].x < Result^.MinV.x then
    Result^.MinV.x := Round(Result^.Vertices[i].x);
    if Result^.Vertices[i].y < Result^.MinV.y then
    Result^.MinV.y := Round(Result^.Vertices[i].y);
    if Result^.Vertices[i].x > Result^.MaxV.x then
    Result^.MaxV.x := Round(Result^.Vertices[i].x);
    if Result^.Vertices[i].y > Result^.MaxV.y then
    Result^.MaxV.y := Round(Result^.Vertices[i].y);
  end;
  Inc(m_QuadCount);
  n := -1;
  for i := 0 to m_QuadGroupCount - 1 do
  begin
    if (
      (
        (Result^.MinV.x > m_QuadGroups[i]^.MinV.X)
        and (Result^.MaxV.x < m_QuadGroups[i]^.MinV.x + m_QuadGroups[i]^.MaxSize)
      ) or (
        (Result^.MinV.x > m_QuadGroups[i]^.MaxV.x - m_QuadGroups[i]^.MaxSize)
        and (Result^.MaxV.x < m_QuadGroups[i]^.MaxV.x)
      )
    ) and (
      (
        (Result^.MinV.y > m_QuadGroups[i]^.MinV.y)
        and (Result^.MaxV.y < m_QuadGroups[i]^.MinV.y + m_QuadGroups[i]^.MaxSize)
      ) or (
        (Result^.MinV.y > m_QuadGroups[i]^.MaxV.y - m_QuadGroups[i]^.MaxSize)
        and (Result^.MaxV.y < m_QuadGroups[i]^.MaxV.y)
      )
    ) then
    begin
      n := i;
      Break;
    end;
  end;
  if n = -1 then
  begin
    if Length(m_QuadGroups) <= m_QuadGroupCount then
    SetLength(m_QuadGroups, Length(m_QuadGroups) + 16);
    n := m_QuadGroupCount;
    Inc(m_QuadGroupCount);
    New(m_QuadGroups[n]);
    m_QuadGroups[n]^.MaxSize := m_DefMaxGroupSize;
    m_QuadGroups[n]^.MinV := Result^.MinV;
    m_QuadGroups[n]^.MaxV := Result^.MaxV;
    m_QuadGroups[n]^.QuadCount := 0;
  end
  else
  begin
    if Result^.MinV.x < m_QuadGroups[n]^.MinV.x then m_QuadGroups[n]^.MinV.x := Result^.MinV.x;
    if Result^.MinV.y < m_QuadGroups[n]^.MinV.y then m_QuadGroups[n]^.MinV.y := Result^.MinV.y;
    if Result^.MaxV.x > m_QuadGroups[n]^.MaxV.x then m_QuadGroups[n]^.MaxV.x := Result^.MaxV.x;
    if Result^.MaxV.y > m_QuadGroups[n]^.MaxV.y then m_QuadGroups[n]^.MaxV.y := Result^.MaxV.y;
  end;
  if Length(m_QuadGroups[n]^.Quads) <= m_QuadGroups[n]^.QuadCount then
  SetLength(m_QuadGroups[n]^.Quads, Length(m_QuadGroups[n]^.Quads) + 32);
  m_QuadGroups[n]^.Quads[m_QuadGroups[n]^.QuadCount] := Result;
  Result^.Group := m_QuadGroups[n];
  Inc(m_QuadGroups[n]^.QuadCount);
end;

procedure TG2SpriteEngine.AddCollider(const c: TG2SpriteCollider);
begin
  if Length(m_Colliders) <= m_ColliderCount then
  SetLength(m_Colliders, Length(m_Colliders) + 64);
  m_Colliders[m_ColliderCount] := c;
  c.ListID := m_ColliderCount;
  Inc(m_ColliderCount);
end;

procedure TG2SpriteEngine.RemoveCollider(const c: TG2SpriteCollider);
  var i: Integer;
begin
  for i := c.ListID to m_ColliderCount - 2 do
  begin
    m_Colliders[i] := m_Colliders[i + 1];
    m_Colliders[i].ListID := i;
  end;
  Dec(m_ColliderCount);
end;
//TG2SpriteEngine END

//TG2Sprite BEGIN
procedure TG2Sprite.SetScale(const Value: Single);
begin
  m_Scale := Value;
  NeedUpdate := True;
end;

procedure TG2Sprite.SetEnabled(const Value: Boolean);
  var i, j: Integer;
begin
  if m_Enabled <> Value then
  begin
    m_Enabled := Value;
    if m_Enabled then
    begin
      for i := 0 to ImageCount - 1 do
      for j := 0 to Images[i].AnimationCount - 1 do
      Images[i].Animations[j].TexID := Images[i].FindTexID(Images[i].Animations[j].TexName);
      Engine.EnableSprite(Self);
    end
    else
    Engine.DisableSprite(Self);
  end;
end;

procedure TG2Sprite.ComputeSize;
  var i, t: Integer;
begin
  HSW := 0;
  HSH := 0;
  for i := 0 to ImageCount - 1 do
  begin
    if (Images[i].Width + Abs(Images[i].Pos.x)) * Images[i].Scale > HSW then
    HSW := Round((Images[i].Width + Abs(Images[i].Pos.x)) * Images[i].Scale);
    if (Images[i].Height + Abs(Images[i].Pos.y)) * Images[i].Scale > HSH then
    HSH := Round((Images[i].Height + Abs(Images[i].Pos.y)) * Images[i].Scale);
  end;
  HSW := Trunc(HSW * 0.71 * Scale);
  HSH := Trunc(HSH * 0.71 * Scale);
end;

constructor TG2Sprite.Create(const SpriteEngine: TG2SpriteEngine);
begin
  inherited Create;
  Engine := SpriteEngine;
  if Length(Engine.Sprites) <= Engine.SpriteCount then
  SetLength(Engine.Sprites, Length(Engine.Sprites) + 128);
  Engine.Sprites[Engine.SpriteCount] := Self;
  ListID := Engine.SpriteCount;
  Inc(Engine.SpriteCount);
  Position.SetValue(0, 0);
  Rotation := 0;
  Scale := 1;
  HSW := 0;
  HSH := 0;
  Dead := False;
  NeedUpdate := False;
end;

destructor TG2Sprite.Destroy;
  var i: Integer;
begin
  for i := 0 to ImageCount - 1 do
  Images[i].Free;
  for i := ListID to Engine.SpriteCount - 2 do
  begin
    Engine.Sprites[i] := Engine.Sprites[i + 1];
    Engine.Sprites[i].ListID := i;
  end;
  Dec(Engine.SpriteCount);
  inherited Destroy;
end;

procedure TG2Sprite.AddImage(const Image: TG2SpriteImage);
begin
  if Length(Images) <= ImageCount then
  SetLength(Images, Length(Images) + 1);
  Images[ImageCount] := Image;
  Inc(ImageCount);
  NeedUpdate := True;
end;

function TG2Sprite.AddImage: TG2SpriteImage;
begin
  Result := TG2SpriteImage.Create(Engine);
  AddImage(Result);
end;

procedure TG2Sprite.Update;
begin

end;

procedure TG2Sprite.Die;
begin
  if not Dead then
  begin
    Dead := True;
    if Length(Engine.DeadSprites) <= Engine.DeadSpriteCount then
    SetLength(Engine.DeadSprites, Length(Engine.DeadSprites) + 64);
    Engine.DeadSprites[Engine.DeadSpriteCount] := Self;
    Inc(Engine.DeadSpriteCount);
  end;
end;
//TG2Sprite END

//TG2SpriteCollider BEGIN
procedure TG2SpriteCollider.TList.SetElement(const Index: Integer; const Value: Pointer);
begin
  m_Arr[Index] := Value;
end;

function TG2SpriteCollider.TList.GetElement(const Index: Integer): Pointer;
begin
  Result := m_Arr[Index];
end;

procedure TG2SpriteCollider.TList.SetCapacity(const Value: Integer);
begin
  SetLength(m_Arr, Value);
end;

function TG2SpriteCollider.TList.GetCapacity: Integer;
begin
  Result := Length(m_Arr);
end;

procedure TG2SpriteCollider.TList.Add(const Ptr: Pointer);
begin
  if m_Count >= Length(m_Arr) then
  SetLength(m_Arr, Length(m_Arr) + 32);
  m_Arr[m_Count] := Ptr;
  Inc(m_Count);
end;

procedure TG2SpriteCollider.TList.Remove(const Ptr: Pointer);
  var i: Integer;
begin
  for i := m_Count - 1 downto 0 do
  if m_Arr[i] = Ptr then
  begin
    Move(m_Arr[i + 1], m_Arr[i], (m_Count - (i + 1)) * 4);
    Dec(m_Count);
    Exit;
  end;
end;

procedure TG2SpriteCollider.TContGroup.SetCapacity(const Value: Integer);
begin
  if Length(c) < Value then
  SetLength(c, Value);
end;

function TG2SpriteCollider.TContGroup.GetCapacity: Integer;
begin
  Result := Length(c);
end;

procedure TG2SpriteCollider.TContGroup.Add(const Cont: TCont);
begin
  if Count >= Length(c) then
  SetLength(c, Length(c) + 32);
  c[Count] := Cont;
  Inc(Count);
end;

procedure TG2SpriteCollider.TShapeCircle.AxisProject(const Axis: TG2Vec2; var MinP, MaxP: Single);
begin
  MinP := Axis.Dot(c);
  MaxP := MinP + r;
  MinP := MinP - r;
end;

function TG2SpriteCollider.TShapeCircle.Center: TG2Vec2;
begin
  Result := c;
end;

function TG2SpriteCollider.TShapeCircle.Clone: TShape;
begin
  Result := TShapeCircle.Create;
  TShapeCircle(Result).c := c;
  TShapeCircle(Result).r := r;
end;

procedure TG2SpriteCollider.TShapeCircle.Transform(const Pos: TG2Vec2; const Rot: TG2Mat2; const OutS: TShape);
begin
  TShapeCircle(OutS).c := c * Rot + Pos;
end;

procedure TG2SpriteCollider.TShapePoly.AxisProject(const Axis: TG2Vec2; var MinP, MaxP: Single);
  var d: Single;
  var i: Integer;
begin
  d := Axis.Dot(p[0]);
  MinP := d;
  MaxP := d;
  for i := 1 to High(p) do
  begin
    d := Axis.Dot(p[i]);
    if d < MinP then MinP := d
    else if d > MaxP then MaxP := d;
  end;
end;

function TG2SpriteCollider.TShapePoly.Center: TG2Vec2;
  var i: Integer;
  var MinV, MaxV: TG2Vec2;
begin
  MinV := p[0];
  MaxV := MinV;
  for i := 1 to High(p) do
  begin
    if p[i].x < MinV.x then MinV.x := p[i].x;
    if p[i].x > MaxV.x then MaxV.x := p[i].x;
    if p[i].y < MinV.y then MinV.y := p[i].y;
    if p[i].y > MaxV.y then MaxV.y := p[i].y;
  end;
  Result := (MinV + MaxV) * 0.5;
end;

function TG2SpriteCollider.TShapePoly.Clone: TShape;
  var i: Integer;
begin
  Result := TShapePoly.Create;
  SetLength(TShapePoly(Result).p, Length(p));
  for i := 0 to High(p) do
  TShapePoly(Result).p[i] := p[i];
end;

procedure TG2SpriteCollider.TShapePoly.Transform(const Pos: TG2Vec2; const Rot: TG2Mat2; const OutS: TShape);
  var i: Integer;
begin
  for i := 0 to High(p) do
  TShapePoly(OutS).p[i] := p[i] * Rot + Pos;
end;

procedure TG2SpriteCollider.TShapePoly.SupportPoints(const n: TG2Vec2; const sp: PList);
  var i: Integer;
  var d, pd: Single;
const
  Threshold = 0.5;
begin
  d := n.Dot(p[0]);
  sp^.Add(@p[0]);
  for i := 1 to High(p) do
  begin
    pd := n.Dot(p[i]);
    if pd >= d - Threshold then
    begin
      if pd > d + Threshold then
      begin
        sp^.Count := 0;
        d := pd;
      end;
      sp^.Add(@p[i]);
    end;
  end;
end;

procedure TG2SpriteCollider.TShapePoly.Edge(const EdgeIndex: Integer; var Index1, Index2: Integer);
begin
  Index1 := EdgeIndex mod Length(p);
  Index2 := (EdgeIndex + 1) mod Length(p);
end;

function TG2SpriteCollider.TShapePoly.EdgeDir(const EdgeIndex: Integer): TG2Vec2;
  var Ind1, Ind2: integer;
begin
  Edge(EdgeIndex, Ind1, Ind2);
  Result := (p[Ind2] - p[Ind1]).Normalized;
end;

function TG2SpriteCollider.TShapePoly.EdgeNormal(const EdgeIndex: Integer): TG2Vec2;
begin
  Result := EdgeDir(EdgeIndex).Perp;
end;

class function TG2SpriteCollider.AxisSeparate(const Axis: TG2Vec2; const s1, s2: TShape; const r: PResp = nil): Boolean;
  var Min1, Max1, Min2, Max2: Single;
  var d, d1, d2: Single;
begin
  s1.AxisProject(Axis, Min1, Max1);
  s2.AxisProject(Axis, Min2, Max2);
  Result := (Min1 > Max2) or (Min2 > Max1);
  if Assigned(r) then
  begin
    d1 := Max1 - Min2;
    d2 := Max2 - Min1;
    if d1 < d2 then d := d1 else d := d2;
    r^.n := Axis;
    r^.d := Abs(d);
  end;
end;

class function TG2SpriteCollider.IntersectCircles(const c1, c2: TShapeCircle; const r: PResp = nil): Boolean;
  var Axis: TG2Vec2;
begin
  Axis := c1.c - c2.c;
  Axis.Normalize;
  Result := not AxisSeparate(Axis, c1, c2, r);
end;

class function TG2SpriteCollider.IntersectPolys(const p1, p2: TShapePoly; const r: PResp = nil): Boolean;
  var i: Integer;
  var n: TG2Vec2;
  var CurResp: Integer;
begin
  Result := True;
  i := Length(p1.p) + Length(p2.p);
  if Length(RespArr) < i then
  SetLength(RespArr, i);
  CurResp := 0;
  for i := 0 to High(p1.p) do
  begin
    n := p1.EdgeNormal(i);
    if AxisSeparate(n, p1, p2, @RespArr[CurResp]) then
    begin
      Result := False;
      Exit;
    end;
    Inc(CurResp);
  end;
  for i := 0 to High(p2.p) do
  begin
    n := p2.EdgeNormal(i);
    if AxisSeparate(n, p1, p2, @RespArr[CurResp]) then
    begin
      Result := False;
      Exit;
    end;
    Inc(CurResp);
  end;
  if Assigned(r) then
  begin
    RespCount := CurResp;
    r^ := FindMTD;
    if (p1.Center - p2.Center).Dot(r^.n) < 0 then
    r^.n := -r^.n;
  end;
end;

class function TG2SpriteCollider.IntersectPolyCircle(const p: TShapePoly; c: TShapeCircle; const r: PResp = nil): Boolean;
  var i: Integer;
  var n: TG2Vec2;
  var CurResp: Integer;
  var d, md: Single;
  var MinVecInd: Integer;
begin
  Result := True;
  i := Length(p.p) + 1;
  if Length(RespArr) < i then
  SetLength(RespArr, i);
  CurResp := 0;
  for i := 0 to High(p.p) do
  begin
    n := p.EdgeNormal(i);
    if AxisSeparate(n, p, c, @RespArr[CurResp]) then
    begin
      Result := False;
      Exit;
    end;
    Inc(CurResp);
  end;
  MinVecInd := 0;
  d := (c.c - p.p[0]).LenSq;
  md := d;
  for i := 1 to High(p.p) do
  begin
    d := (c.c - p.p[i]).LenSq;
    if d < md then
    begin
      md := d;
      MinVecInd := i;
    end;
  end;
  n := (c.c - p.p[MinVecInd]).Normalized;
  if AxisSeparate(n, p, c, @RespArr[CurResp]) then
  begin
    Result := False;
    Exit;
  end;
  Inc(CurResp);
  if Assigned(r) then
  begin
    RespCount := CurResp;
    r^ := FindMTD;
    if (p.Center - c.c).Dot(r^.n) < 0 then
    r^.n := -r^.n;
  end;
end;

class function TG2SpriteCollider.FindMTD: TResp;
  var ResultInd, i: Integer;
  var md: Single;
begin
  ResultInd := 0;
  md := RespArr[0].d;
  for i := 1 to RespCount - 1 do
  begin
    if RespArr[i].d < md then
    begin
      md := RespArr[i].d;
      ResultInd := i;
    end;
  end;
  Result := RespArr[ResultInd];
end;

class procedure TG2SpriteCollider.FindContactsPoly(const n: TG2Vec2; const p1, p2: TShapePoly; const Cont: PContGroup);
  var i: Integer;
  var d, Min1, Max1, Min2, Max2: Single;
  var Perp: TG2Vec2;
  var Mag: Single;
  var c: TCont;
begin
  sp1.Count := 0;
  sp2.Count := 0;
  p1.SupportPoints(-n, @sp1);
  p2.SupportPoints(n, @sp2);
  if (sp1.Count = 1) and (sp2.Count = 1) then
  begin
    c.n := n;
    c.p := (PG2Vec2(sp1[0])^ + PG2Vec2(sp2[0])^) * 0.5;
    Cont^.Add(c);
  end
  else if (sp1.Count = 1) and (sp2.Count > 1) then
  begin
    c.n := n;
    c.p := PG2Vec2(sp1[0])^;
    Cont^.Add(c);
  end
  else if (sp1.Count > 1) and (sp2.Count = 1) then
  begin
    c.n := n;
    c.p := PG2Vec2(sp2[0])^;
    Cont^.Add(c);
  end
  else
  begin
    Perp := n.Perp;
    Min1 := Perp.Dot(PG2Vec2(sp1[0])^);
    Max1 := Min1;
    Min2 := Perp.Dot(PG2Vec2(sp2[0])^);
    Max2 := Min2;
    for i := 1 to sp1.Count - 1 do
    begin
      d := Perp.Dot(PG2Vec2(sp1[i])^);
      if d < Min1 then Min1 := d;
      if d > Max1 then Max1 := d;
    end;
    for i := 1 to sp2.Count - 1 do
    begin
      d := Perp.Dot(PG2Vec2(sp2[i])^);
      if d < Min2 then Min2 := d;
      if d > Max2 then Max2 := d;
    end;
    for i := 0 to sp1.Count - 1 do
    begin
      d := Perp.Dot(PG2Vec2(sp1[i])^);
      if (d >= Min2) and (d <= Max2) then
      begin
        c.n := n;
        c.p := PG2Vec2(sp1[i])^;
        Cont^.Add(c);
      end;
    end;
    for i := 0 to sp2.Count - 1 do
    begin
      d := Perp.Dot(PG2Vec2(sp2[i])^);
      if (d >= Min1) and (d <= Max1) then
      begin
        c.n := n;
        c.p := PG2Vec2(sp2[i])^;
        Cont^.Add(c);
      end;
    end;
  end;
end;

class procedure TG2SpriteCollider.FindContactsCircle(const n: TG2Vec2; const c1, c2: TShapeCircle; const Cont: PContGroup);
begin
  Cont^.Capacity := 1;
  Cont^.Count := 1;
  Cont^.c[0].p := c2.c + n * c2.r;
  Cont^.c[0].n := n;
end;

class procedure TG2SpriteCollider.FindContactsPolyCircle(const n: TG2Vec2; const p: TShapePoly; const c: TShapeCircle; const Cont: PContGroup);
begin
  Cont^.Capacity := 1;
  Cont^.Count := 1;
  Cont^.c[0].p := c.c + n * c.r;
  Cont^.c[0].n := n;
end;

class procedure TG2SpriteCollider.ResolveIntersection(const c1, c2: TG2SpriteCollider; const r: TResp);
  var g1, g2: Single;
begin
  if c1.ColliderType = ctStatic then
  begin
    g1 := 0;
    g2 := 1;
  end
  else
  if c2.ColliderType = ctStatic then
  begin
    g1 := 1;
    g2 := 0;
  end
  else
  begin
    g1 := 0.5;
    g2 := 0.5;
  end;
  c1.Pos := c1.Pos + r.n * (r.d * g1);
  c2.Pos := c2.Pos - r.n * (r.d * g2);
end;

class procedure TG2SpriteCollider.ResolveContacts(const c1, c2: TG2SpriteCollider; const Contacts: TContGroup);
  var TM, M1, M2, TH, TF, CM: Single;
  var FTotal1, FDirect1, FFriction1: TG2Vec2;
  var FTotal2, FDirect2, FFriction2: TG2Vec2;
  var F: TG2Vec2;
  var i: Integer;
begin
  TH := c1.Hardness * c2.Hardness;
  TF := c1.Friction * c2.Friction;

  CM := 1 / Contacts.Count;
  for i := 0 to Contacts.Count - 1 do
  begin
    FTotal1 := c1.GetForceAtPoint(Contacts.c[i].p);
    FTotal2 := c2.GetForceAtPoint(Contacts.c[i].p);
    if ResolveForces(
      -Contacts.c[i].n, Contacts.c[i].n,
      FTotal1, FTotal2,
      FDirect1, FFriction1,
      FDirect2, FFriction2,
      TH, TF
    ) then
    begin
      F := ((FDirect2 - FDirect1) + (FFriction2 - FFriction1)) * CM;
      if c1.ColliderType = ctStatic then
      c2.SetForceAtPoint(Contacts.c[i].p, -F)
      else if c2.ColliderType = ctStatic then
      c1.SetForceAtPoint(Contacts.c[i].p, F)
      else
      begin
        c1.SetForceAtPoint(Contacts.c[i].p, F);
        c2.SetForceAtPoint(Contacts.c[i].p, -F);
      end;
    end;
  end;
end;

class function TG2SpriteCollider.ResolveForces(
      const N1, N2, F1, F2: TG2Vec2;
      var Direct1: TG2Vec2;
      var Friction1: TG2Vec2;
      var Direct2: TG2Vec2;
      var Friction2: TG2Vec2;
      const MagHardness: Single = 1;
      const MagFriction: Single = 1
    ): Boolean;
  var FD1, FD2, FF1, FF2, FDR: Single;
  var NF1, NF2: TG2Vec2;
begin
  FD1 := N1.Dot(F1);
  FD2 := N2.Dot(F2);
  FDR := FD1 + FD2;
  if FDR <= 0 then
  begin
    Direct1.SetValue(0, 0);
    Friction1.SetValue(0, 0);
    Direct2.SetValue(0, 0);
    Friction2.SetValue(0, 0);
    Result := False;
    Exit;
  end;
  NF1 := N1.Perp;
  NF2 := -NF1;
  FF1 := F1.Dot(NF1);
  FF2 := F2.Dot(NF2);
  if FF1 < 0 then
  begin
    NF1 := -NF1;
    FF1 := -FF1;
  end;
  if FF2 < 0 then
  begin
    NF2 := -NF2;
    FF2 := -FF2;
  end;
  Direct1 := N1 * (FD1 * MagHardness);
  Direct2 := N2 * (FD2 * MagHardness);
  Friction1 := NF1 * (FF1 * MagFriction);
  Friction2 := NF2 * (FF2 * MagFriction);
  Result := True;
end;

constructor TG2SpriteCollider.Create(const SpriteEngine: TG2SpriteEngine);
begin
  inherited Create;
  m_Engine := SpriteEngine;
  m_Engine.AddCollider(Self);
  Pos.SetValue(0, 0);
  Ang := 0;
  VelLin.SetValue(0, 0);
  VelAng := 0;
  Hardness := 0.7;
  Friction := 0.7;
end;

destructor TG2SpriteCollider.Destroy;
  var i: Integer;
begin
  m_Engine.RemoveCollider(Self);
  for i := 0 to Shapes.Count - 1 do
  begin
    TShape(Shapes[i]).Free;
    TShape(ShapesCur[i]).Free;
  end;
  inherited Destroy;
end;

procedure TG2SpriteCollider.Update;
  var i: Integer;
  var m: TG2Mat2;
begin
  if m_ColliderType <> ctStatic then
  begin
    VelLin := VelLin + m_Engine.Gravity;
    Pos := Pos + VelLin;
    if m_ColliderType <> ctOriented then
    Ang := Ang + VelAng;
  end;
  m.SetRotation(Ang);
  for i := 0 to Shapes.Count - 1 do
  TShape(Shapes[i]).Transform(Pos, m, TShape(ShapesCur[i]));
end;

procedure TG2SpriteCollider.Collide(const c: TG2SpriteCollider);
  var c1, c2: TG2SpriteCollider;
  var i, j, k, n: Integer;
  var r: TResp;
begin
  c1 := Self;
  c2 := c;
  cg.Count := 0;
  if (c1.ColliderType = ctStatic) and (c2.ColliderType = ctStatic) then
  Exit;
  for i := 0 to c1.ShapesCur.Count - 1 do
  for j := 0 to c2.ShapesCur.Count - 1 do
  begin
    if (TShape(c1.ShapesCur[i]) is TShapePoly) and (TShape(c2.ShapesCur[j]) is TShapePoly)
    and (IntersectPolys(TShapePoly(c1.ShapesCur[i]), TShapePoly(c2.ShapesCur[j]), @r)) then
    begin
      ResolveIntersection(c1, c2, r);
      FindContactsPoly(r.n, TShapePoly(c1.ShapesCur[i]), TShapePoly(c2.ShapesCur[j]), @cg);
    end
    else if (TShape(c1.ShapesCur[i]) is TShapeCircle) and (TShape(c2.ShapesCur[j]) is TShapeCircle)
    and (IntersectCircles(TShapeCircle(c1.ShapesCur[i]), TShapeCircle(c2.ShapesCur[j]), @r)) then
    begin
      ResolveIntersection(c1, c2, r);
      FindContactsCircle(r.n, TShapeCircle(c1.ShapesCur[i]), TShapeCircle(c2.ShapesCur[j]), @cg);
    end
    else if (TShape(c1.ShapesCur[i]) is TShapePoly) and (TShape(c2.ShapesCur[j]) is TShapeCircle)
    and (IntersectPolyCircle(TShapePoly(c1.ShapesCur[i]), TShapeCircle(c2.ShapesCur[j]), @r)) then
    begin
      ResolveIntersection(c1, c2, r);
      FindContactsPolyCircle(r.n, TShapePoly(c1.ShapesCur[i]), TShapeCircle(c2.ShapesCur[j]), @cg);
    end
    else if (TShape(c1.ShapesCur[i]) is TShapeCircle) and (TShape(c2.ShapesCur[j]) is TShapePoly)
    and (IntersectPolyCircle(TShapePoly(c2.ShapesCur[j]), TShapeCircle(c1.ShapesCur[i]), @r)) then
    begin
      ResolveIntersection(c2, c1, r);
      n := cg.Count;
      FindContactsPolyCircle(r.n, TShapePoly(c2.ShapesCur[j]), TShapeCircle(c1.ShapesCur[i]), @cg);
      for k := n to cg.Count - 1 do
      cg.c[k].n := -cg.c[k].n;
    end;
  end;
  if cg.Count > 0 then
  ResolveContacts(c1, c2, cg);
end;

function TG2SpriteCollider.GetForceAtPoint(const p: TG2Vec2): TG2Vec2;
begin
  Result := (p - Pos).Perp * VelAng + VelLin;
end;

procedure TG2SpriteCollider.SetForceAtPoint(const p, f: TG2Vec2);
var
  ModelPoint: TG2Vec2;
  d: Single;
begin
  case m_ColliderType of
    ctDynamic:
    begin
      ModelPoint := p - Pos;
      d := Abs(ModelPoint.Normalized.Dot(f.Normalized));
      VelLin := VelLin + (f * d);
      d := ModelPoint.Len;
      if d <= 1 then Exit;
      VelAng := VelAng + (ModelPoint.Perp.Normalized.Dot(f) / d);
    end;
    ctOriented:
    begin
      VelLin := VelLin + f;
    end;
  end;
end;

procedure TG2SpriteCollider.AddShapePoly(const v: PG2Vec2; const Count: Integer);
  var pv: PG2Vec2;
  var s: TShapePoly;
  var i: Integer;
begin
  pv := v;
  s := TShapePoly.Create;
  SetLength(s.p, Count);
  for i := 0 to Count - 1 do
  begin
    s.p[i] := pv^;
    Inc(pv);
  end;
  Shapes.Add(s);
  ShapesCur.Add(s.Clone);
end;

procedure TG2SpriteCollider.AddShapeCircle(const c: TG2Vec2; const r: Single);
  var s: TShapeCircle;
begin
  s := TShapeCircle.Create;
  s.c := c;
  s.r := r;
  Shapes.Add(s);
  ShapesCur.Add(s.Clone);
end;

procedure TG2SpriteCollider.CenterPos;
  var i, j: Integer;
begin
  Pos.SetValue(0, 0);
  for i := 0 to Shapes.Count - 1 do
  Pos := Pos + TShape(Shapes[i]).Center;
  Pos := Pos / Shapes.Count;
  for i := 0 to Shapes.Count - 1 do
  begin
    if TShape(Shapes[i]) is TShapePoly then
    with TShapePoly(Shapes[i]) do
    for j := 0 to High(p) do
    p[j] := p[j] - Pos
    else if TShape(Shapes[i]) is TShapeCircle then
    with TShapeCircle(Shapes[i]) do
    c := c - Pos;
  end;
end;
//TG2SpriteCollider END

//TG2SpriteImage BEGIN
procedure TG2SpriteImage.Update;
  var n: Integer;
  var t: DWord;
  var TimePassed: DWord;
  var FrameCount: Integer;
begin
  if m_Playing and (CurAnimID > -1) then
  begin
    FrameCount := (Animations[CurAnimID].FrameEnd - Animations[CurAnimID].FrameStart) + 1;
    TimePassed := Engine.CurTime - m_PlayTime;
    t := Trunc((TimePassed * 0.001) * Animations[CurAnimID].FrameRate);
    if Animations[CurAnimID].Loop then
    begin
      t := t mod FrameCount;
      CurFrame := Animations[CurAnimID].FrameStart + t;
    end
    else
    begin
      if t >= FrameCount then
      begin
        n := FindAnimID(Animations[CurAnimID].NextAnim);
        if n > -1 then
        begin
          m_PlayTime := Engine.CurTime - TimePassed + Trunc(FrameCount * 1000 / Animations[CurAnimID].FrameRate);
          CurAnimID := n;
          CurFrame := Animations[CurAnimID].FrameStart;
        end
        else
        begin
          CurFrame := Animations[CurAnimID].FrameEnd;
          m_Playing := False;
        end;
      end
      else
      CurFrame := Animations[CurAnimID].FrameStart + t;
    end;
  end;
end;

function TG2SpriteImage.FindAnimID(const Name: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to AnimationCount - 1 do
  if Animations[i].AnimName = Name then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TG2SpriteImage.FindTexID(const Name: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to TextureCount - 1 do
  if Textures[i].Texture.Name = Name then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TG2SpriteImage.GetCurAnim: AnsiString;
begin
  if (CurAnimID > -1) and (CurAnimID < AnimationCount) then
  Result := Animations[CurAnimID].AnimName
  else
  Result := '';
end;

procedure TG2SpriteImage.SetCurAnim(const Value: AnsiString);
begin
  CurAnimID := FindAnimID(Value);
  if CurAnimID = -1 then
  begin
    m_Playing := False;
    m_PlayTime := Engine.CurTime;
    m_PauseTime := Engine.CurTime;
  end;
end;

constructor TG2SpriteImage.Create(const SpriteEngine: TG2SpriteEngine);
begin
  inherited Create;
  Engine := SpriteEngine;
  m_Playing := False;
  m_PlayTime := Engine.CurTime;
  m_PauseTime := Engine.CurTime;
  Order := 0;
  BlendMode.SetNormal;
  Pos := Point(0, 0);
  Width := 0;
  Height := 0;
  Scale := 1;
  Rotation := 0;
  FlipLeftRight := False;
  FlipTopBottom := False;
  TextureCount := 0;
  AnimationCount := 0;
  CurAnimID := -1;
  CurFrame := 0;
end;

destructor TG2SpriteImage.Destroy;
begin
  inherited Destroy;
end;

procedure TG2SpriteImage.AddTexture(
      const Texture: TG2Texture2D;
      const PatternWidth: Integer = 0;
      const PatternHeight: Integer = 0;
      const PatternCount: Integer = 1
    );
begin
  if Length(Textures) <= TextureCount then
  SetLength(Textures, Length(Textures) + 8);
  Textures[TextureCount].Texture := Texture;
  if PatternWidth = 0 then
  Textures[TextureCount].PatternWidth := Texture.Width
  else
  Textures[TextureCount].PatternWidth := PatternWidth;
  if PatternHeight = 0 then
  Textures[TextureCount].PatternHeight := Texture.Height
  else
  Textures[TextureCount].PatternHeight := PatternHeight;
  Textures[TextureCount].PatternCount := PatternCount;
  if Width < Textures[TextureCount].PatternWidth then
  Width := Textures[TextureCount].PatternWidth;
  if Height < Textures[TextureCount].PatternHeight then
  Height := Textures[TextureCount].PatternHeight;
  Inc(TextureCount);
end;

procedure TG2SpriteImage.AddAnimation(
      const AnimName, TexName, NextAnimName: AnsiString;
      const Loop: Boolean;
      const FrameStart, FrameEnd: Integer;
      const FrameRate: Single
    );
begin
  if Length(Animations) <= AnimationCount then
  SetLength(Animations, Length(Animations) + 8);
  Animations[AnimationCount].AnimName := AnimName;
  Animations[AnimationCount].TexName := TexName;
  Animations[AnimationCount].TexID := -1;
  Animations[AnimationCount].NextAnim := NextAnimName;
  Animations[AnimationCount].Loop := Loop;
  Animations[AnimationCount].FrameStart := FrameStart;
  Animations[AnimationCount].FrameEnd := FrameEnd;
  Animations[AnimationCount].FrameRate := FrameRate;
  Inc(AnimationCount);
end;

procedure TG2SpriteImage.Play;
begin
  if CurAnimID = -1 then Exit;
  m_Playing := True;
  m_PlayTime := Engine.CurTime;
end;

procedure TG2SpriteImage.Pause;
begin
  if CurAnimID = -1 then Exit;
  m_Playing := not m_Playing;
  if m_Playing then
  m_PlayTime := m_PlayTime + (Engine.CurTime - m_PauseTime)
  else
  m_PauseTime := Engine.CurTime;
end;
//TG2SpriteImage END

end.