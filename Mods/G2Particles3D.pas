//G2Particles3D v1.0
unit G2Particles3D;

{$include ../Gen2.inc}

interface

uses
  Types,
  Windows,
  Classes,
  Math,
  DXTypes,
  Direct3D9,
  D3DX9,
  Gen2,
  G2Math;

type
  TG2Particles3D = class;
  TG2Particle3D = class;
  TG2Particle3DRender = class;
  TG2Particle3DEmmitter = class;

  CG2Particle3DRenderClass = class of TG2Particle3DRender;
  CG2Particle3DClass = class of TG2Particle3D;

  TG2Particles3D = class (TG2Module)
  strict private
    type TGroup = record
      var ParticleClass: CG2Particle3DClass;
      var BMin, BMax: TG2Vec3;
      var Items: array of TG2Particle3D;
      var ItemCount: Integer;
      var MaxSize: Single;
    end;
    type PGroup = ^TGroup;
    var m_Groups: array of PGroup;
    var m_GroupCount: Integer;
    var m_NewGroups: array of PGroup;
    var m_NewGroupCount: Integer;
    var m_Emmitters: array of TG2Particle3DEmmitter;
    var m_EmmitterCount: Integer;
    var m_RenderQueue: array of TG2Particle3D;
    var m_RenderQueueSize: Integer;
    var m_ParticlesRendered: Integer;
    procedure SplitGroup(const g: PGroup; const Axis: TG2Plane);
  private
    var Renders: array of TG2Particle3DRender;
    var RenderCount: Integer;
    procedure AddGroup(const g: PGroup);
    procedure AddEmmitter(const Emmitter: TG2Particle3DEmmitter);
    procedure RemoveEmmitter(const Emmitter: TG2Particle3DEmmitter);
  public
    property ParticlesRendered: Integer read m_ParticlesRendered;
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    procedure Update;
    procedure Render;
    procedure AddParticle(const Particle: TG2Particle3D);
    procedure Clear;
  end;

  TG2Particle3D = class
  strict private
    var m_X, m_Y, m_Z: Single;
    var m_Dead: Boolean;
    var m_ParticleRender: TG2Particle3DRender;
    var m_Sorted: Boolean;
    procedure SetSizeX(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSizeY(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSizeZ(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSizeX: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSizeY: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSizeZ: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var sx, sy, sz: Single;
    var sort: Single;
    procedure GetBounds(var lx, ly, lz, hx, hy, hz: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  protected
    var RenderClass: CG2Particle3DRenderClass;
  public
    property X: Single read m_X write m_X;
    property Y: Single read m_Y write m_Y;
    property Z: Single read m_Z write m_Z;
    property SizeX: Single read GetSizeX write SetSizeX;
    property SizeY: Single read GetSizeY write SetSizeY;
    property SizeZ: Single read GetSizeZ write SetSizeZ;
    property Dead: Boolean read m_Dead;
    property DistanceSorted: Boolean read m_Sorted write m_Sorted;
    property ParticleRender: TG2Particle3DRender read m_ParticleRender write m_ParticleRender;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Update; virtual;
    procedure Die; virtual;
  end;

  TG2Particle3DRender = class
  strict protected
    var m_Core: TG2Core;
  public
    constructor Create(const G2Core: TG2Core); virtual;
    destructor Destroy; override;
    procedure RenderBegin; virtual;
    procedure RenderEnd; virtual;
    procedure RenderParticle(const Particle: TG2Particle3D); virtual;
  end;

  TG2Particle3DEmmitter = class
  strict protected
    var m_Particles: TG2Particles3D;
  public
    constructor Create(const Particles: TG2Particles3D); virtual;
    destructor Destroy; override;
    procedure Update; virtual; abstract;
  end;

  TG2Particle3DEmmitterSingle = class (TG2Particle3DEmmitter)
  strict private
    var m_Frequency: DWord;
    var m_CreateTime: DWord;
    var m_ParticleType: CG2Particle3DClass;
    var m_UpdateTime: DWord;
  protected
    var t: Single;
  public
    property Frequency: DWord read m_Frequency write m_Frequency;
    property ParticleType: CG2Particle3DClass read m_ParticleType write m_ParticleType;
    constructor Create(const Particles: TG2Particles3D); override;
    destructor Destroy; override;
    procedure Update; override;
    procedure CreateParticle(const p: TG2Particle3D); virtual; abstract;
  end;

  TG2Particle3DEmmitterSingleSphere = class (TG2Particle3DEmmitterSingle)
  public
    var Pos: TG2Vec3;
    var Radius: Single;
    constructor Create(const Particles: TG2Particles3D); override;
    procedure CreateParticle(const p: TG2Particle3D); override;
  end;

  TG2Particle3DEmmitterSingleBox = class (TG2Particle3DEmmitterSingle)
  public
    var Box: TG2AABox;
    constructor Create(const Particles: TG2Particles3D); override;
    procedure CreateParticle(const p: TG2Particle3D); override;
  end;

  TG2Particle3DQuadTex = class (TG2Particle3D)
  public
    var Texture: TG2Texture2D;
    var BlendMode: TG2BlendMode;
  end;

  TG2Particle3DBillboard = class (TG2Particle3DQuadTex)
  strict private
    var m_Scale: Single;
    var m_Rotation: Single;
    var m_Color: TG2Color;
    procedure SetScale(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property Scale: Single read m_Scale write SetScale;
    property Rotation: Single read m_Rotation write m_Rotation;
    property Color: TG2Color read m_Color write m_Color;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TG2Particle3DBillboardAxis = class (TG2Particle3DQuadTex)
  strict private
    var m_PosSrc: TG2Vec3;
    var m_PosDst: TG2Vec3;
    var m_Size: Single;
    var m_Color: TG2Color;
    property X;
    property Y;
    property Z;
    property SizeX;
    property SizeY;
    property SizeZ;
    procedure Reallocate;
    procedure SetSize(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPosSrc(const Value: TG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPosDst(const Value: TG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property Size: Single read m_Size write SetSize;
    property PosSrc: TG2Vec3 read m_PosSrc write SetPosSrc;
    property PosDst: TG2Vec3 read m_PosDst write SetPosDst;
    property Color: TG2Color read m_Color write m_Color;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TG2Particle3DFixed = class (TG2Particle3DQuadTex)
  strict private
    var m_DirX: TG2Vec3;
    var m_DirY: TG2Vec3;
    var m_Color: TG2Color;
    procedure ResetSize;
    procedure SetDirX(const Value: TG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDirY(const Value: TG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property DirX: TG2Vec3 read m_DirX write SetDirX;
    property DirY: TG2Vec3 read m_DirY write SetDirY;
    property Color: TG2Color read m_Color write m_Color;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TG2Particle3DRenderQuadArr = class (TG2Particle3DRender)
  strict private
    var m_VB: TG2VB;
    var m_IB: TG2IB;
    var m_Decl: IDirect3DVertexDeclaration9;
    var m_Shader: TG2Effect;
    var m_MaxQuads: Integer;
    var m_CurQuad: Integer;
    var m_CurTexture: TG2Texture2D;
    var m_CurBlendMode: TG2BlendMode;
    var m_PrevSrcColorBlend: DWord;
    var m_PrevDstColorBlend: DWord;
    var m_PrevSrcAlphaBlend: DWord;
    var m_PrevDstAlphaBlend: DWord;
  strict protected
    procedure RenderFlush;
    property CurQuad: Integer read m_CurQuad write m_CurQuad;
  public
    constructor Create(const G2Core: TG2Core); override;
    destructor Destroy; override;
    procedure RenderBegin; override;
    procedure RenderEnd; override;
    procedure RenderParticle(const Particle: TG2Particle3D); override;
  end;

  TG2Particle3DRenderBillboard = class (TG2Particle3DRenderQuadArr)
  strict private
    var m_ViewFlip: TG2Mat;
  public
    procedure RenderBegin; override;
    procedure RenderParticle(const Particle: TG2Particle3D); override;
  end;

  TG2Particle3DRenderBillboardAxis = class (TG2Particle3DRenderQuadArr)
  public
    procedure RenderParticle(const Particle: TG2Particle3D); override;
  end;

  TG2Particle3DRenderFixed = class (TG2Particle3DRenderQuadArr)
  public
    procedure RenderParticle(const Particle: TG2Particle3D); override;
  end;

implementation

//TG2Particles3D BEGIN
procedure TG2Particles3D.SplitGroup(const g: PGroup; const Axis: TG2Plane);
  var i: Integer;
  var g0, g1, g2: PGroup;
begin
  New(g1);
  New(g2);
  g1^.ParticleClass := g^.ParticleClass;
  g1^.BMin := g^.BMin;
  g1^.BMax := g^.BMax;
  g1^.MaxSize := g^.MaxSize;
  g1^.ItemCount := 0;
  g2^.ParticleClass := g^.ParticleClass;
  g2^.BMin := g^.BMin;
  g2^.BMax := g^.BMax;
  g2^.MaxSize := g^.MaxSize;
  g2^.ItemCount := 0;
  for i := 0 to g^.ItemCount - 1 do
  begin
    if Axis.DistanceToPoint(G2Vec3(g^.Items[i].X, g^.Items[i].Y, g^.Items[i].Z)) > 0 then
    g0 := g1 else g0 := g2;
    if Length(g0^.Items) <= g0^.ItemCount then
    SetLength(g0^.Items, Length(g0^.Items) + 64);
    g0^.Items[g0^.ItemCount] := g^.Items[i];
    Inc(g0^.ItemCount);
  end;
  if Length(m_NewGroups) <= m_NewGroupCount + 1 then
  SetLength(m_NewGroups, Length(m_NewGroups) + 16);
  m_NewGroups[m_NewGroupCount] := g1;
  Inc(m_NewGroupCount);
  m_NewGroups[m_NewGroupCount] := g2;
  Inc(m_NewGroupCount);
end;

procedure TG2Particles3D.AddGroup(const g: PGroup);
begin
  if Length(m_Groups) <= m_GroupCount then
  SetLength(m_Groups, Length(m_Groups) + 16);
  m_Groups[m_GroupCount] := g;
  Inc(m_GroupCount);
end;

procedure TG2Particles3D.AddEmmitter(const Emmitter: TG2Particle3DEmmitter);
begin
  if Length(m_Emmitters) <= m_EmmitterCount then
  SetLength(m_Emmitters, Length(m_Emmitters) + 32);
  m_Emmitters[m_EmmitterCount] := Emmitter;
  Inc(m_EmmitterCount);
end;

procedure TG2Particles3D.RemoveEmmitter(const Emmitter: TG2Particle3DEmmitter);
  var i, n: Integer;
begin
  n := -1;
  for i := 0 to m_EmmitterCount - 1 do
  if m_Emmitters[i] = Emmitter then
  begin
    n := i;
    Break;
  end;
  if n >= 0 then
  begin
    Dec(m_EmmitterCount);
    for i := n to m_EmmitterCount - 1 do
    m_Emmitters[i] := m_Emmitters[i + 1];
  end;
end;

constructor TG2Particles3D.Create;
begin
  inherited Create;
  m_ParticlesRendered := 0;
end;

destructor TG2Particles3D.Destroy;
begin
  inherited Destroy;
end;

function TG2Particles3D.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_GroupCount := 0;
  m_NewGroupCount := 0;
  m_EmmitterCount := 0;
  m_RenderQueueSize := 0;
  RenderCount := 0;
end;

function TG2Particles3D.Finalize: TG2Result;
  var i: Integer;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  for i := 0 to RenderCount - 1 do
  Renders[i].Free;
  Clear;
  RenderCount := 0;
end;

procedure TG2Particles3D.Update;
  var i, pi, pc, gi, gc: Integer;
  var g: PGroup;
  var lx, ly, lz, hx, hy, hz: Single;
  var gmid: TG2Vec3;
  var KillGroup: Boolean;
begin
  for i := 0 to m_EmmitterCount - 1 do
  m_Emmitters[i].Update;
  gc := m_GroupCount;
  gi := 0;
  while gi < gc do
  begin
    KillGroup := False;
    g := m_Groups[gi];
    pc := g^.ItemCount;
    if pc > 0 then
    begin
      pi := 0;
      g^.Items[0].GetBounds(lx, ly, lz, hx, hy, hz);
      g^.BMin.SetValue(lx, ly, lz);
      g^.BMax.SetValue(hx, hy, hz);
      while pi < pc do
      begin
        g^.Items[pi].Update;
        if g^.Items[pi].Dead then
        begin
          g^.Items[pi].Free;
          Dec(pc);
          for i := pi to pc do
          g^.Items[i] := g^.Items[i + 1];
        end
        else
        begin
          g^.Items[pi].GetBounds(lx, ly, lz, hx, hy, hz);
          if lx < g^.BMin.x then g^.BMin.x := lx;
          if ly < g^.BMin.y then g^.BMin.y := ly;
          if lz < g^.BMin.z then g^.BMin.z := lz;
          if hx > g^.BMax.x then g^.BMax.x := hx;
          if hy > g^.BMax.y then g^.BMax.y := hy;
          if hz > g^.BMax.z then g^.BMax.z := hz;
          Inc(pi);
        end;
      end;
      g^.ItemCount := pc;
      gmid := (g^.BMax + g^.BMin) * 0.5;
      if (g^.BMax.x - g^.BMin.x) > g^.MaxSize then
      begin
        SplitGroup(g, G2Plane(gmid, G2Vec3(1, 0, 0)));
        KillGroup := True;
      end
      else if (g^.BMax.y - g^.BMin.y) > g^.MaxSize then
      begin
        SplitGroup(g, G2Plane(gmid, G2Vec3(0, 1, 0)));
        KillGroup := True;
      end
      else if (g^.BMax.z - g^.BMin.z) > g^.MaxSize then
      begin
        SplitGroup(g, G2Plane(gmid, G2Vec3(0, 0, 1)));
        KillGroup := True;
      end;
    end
    else
    KillGroup := True;
    if KillGroup then
    begin
      Dispose(g);
      Dec(gc);
      for i := gi to gc do
      m_Groups[i] := m_Groups[i + 1];
    end
    else
    Inc(gi);
  end;
  m_GroupCount := gc;
  for i := 0 to m_NewGroupCount - 1 do
  AddGroup(m_NewGroups[i]);
  m_NewGroupCount := 0;
end;

procedure TG2Particles3D.Render;
  var gi, i, l, h, m: Integer;
  var CamDir: TG2Vec3;
  var CurRender: TG2Particle3DRender;
begin
  m_ParticlesRendered := 0;
  CurRender := nil;
  m_RenderQueueSize := 0;
  CamDir := Core.Graphics.Transforms.Vdir;
  for gi := 0 to m_GroupCount - 1 do
  if Core.Graphics.Transforms.Frustum.BoxInFrustum(m_Groups[gi].BMin, m_Groups[gi].BMax) then
  begin
    for i := 0 to m_Groups[gi].ItemCount - 1 do
    begin
      if m_Groups[gi].Items[i].DistanceSorted then
      begin
        m_Groups[gi].Items[i].Sort := CamDir.Dot(G2Vec3(m_Groups[gi].Items[i].X, m_Groups[gi].Items[i].Y, m_Groups[gi].Items[i].Z));
        if Length(m_RenderQueue) <= m_RenderQueueSize then
        SetLength(m_RenderQueue, Length(m_RenderQueue) + 64);
        l := 0;
        h := m_RenderQueueSize - 1;
        while l <= h do
        begin
          m := (l + h) div 2;
          if (m_RenderQueue[m].sort - m_Groups[gi].Items[i].Sort) < 0 then
          l := m + 1
          else
          h := m - 1;
        end;
        for m := m_RenderQueueSize downto l + 1 do
        m_RenderQueue[m] := m_RenderQueue[m - 1];
        m_RenderQueue[l] := m_Groups[gi].Items[i];
        Inc(m_RenderQueueSize);
      end
      else
      begin
        if CurRender <> m_Groups[gi].Items[i].ParticleRender then
        begin
          if CurRender <> nil then
          CurRender.RenderEnd;
          CurRender := m_Groups[gi].Items[i].ParticleRender;
          CurRender.RenderBegin;
        end;
        CurRender.RenderParticle(m_Groups[gi].Items[i]);
      end;
      Inc(m_ParticlesRendered);
    end;
  end;
  for i := m_RenderQueueSize - 1 downto 0 do
  begin
    if CurRender <> m_RenderQueue[i].ParticleRender then
    begin
      if CurRender <> nil then
      CurRender.RenderEnd;
      CurRender := m_RenderQueue[i].ParticleRender;
      CurRender.RenderBegin;
    end;
    CurRender.RenderParticle(m_RenderQueue[i]);
  end;
  if CurRender <> nil then
  CurRender.RenderEnd;
end;

procedure TG2Particles3D.AddParticle(const Particle: TG2Particle3D);
  var g, cg: PGroup;
  var i, n: Integer;
  var lx, ly, lz, hx, hy, hz, ms: Single;
  var glx, gly, glz, ghx, ghy, ghz, gsx, gsy, gsz: Single;
begin
  n := -1;
  for i := 0 to RenderCount - 1 do
  if Renders[i] is Particle.RenderClass then
  begin
    n := i;
    Break;
  end;
  if n = -1 then
  begin
    n := RenderCount;
    if Length(Renders) <= n then
    SetLength(Renders, Length(Renders) + 8);
    Renders[n] := Particle.RenderClass.Create(Core);
    Inc(RenderCount);
  end;
  Particle.ParticleRender := Renders[n];
  Particle.GetBounds(lx, ly, lz, hx, hy, hz);
  ms := (Particle.sx + Particle.sy + Particle.sz) * 8;
  g := nil;
  for i := 0 to m_GroupCount - 1 do
  if m_Groups[i]^.ParticleClass = CG2Particle3DClass(Particle.ClassType) then
  begin
    cg := m_Groups[i];
    gsx := cg^.MaxSize - (cg^.BMax.x - cg^.BMin.x);
    gsy := cg^.MaxSize - (cg^.BMax.y - cg^.BMin.y);
    gsz := cg^.MaxSize - (cg^.BMax.z - cg^.BMin.z);
    glx := cg^.BMin.x - gsx;
    gly := cg^.BMin.y - gsy;
    glz := cg^.BMin.z - gsz;
    ghx := cg^.BMax.x + gsx;
    ghy := cg^.BMax.y + gsy;
    ghz := cg^.BMax.z + gsz;
    if (lx > glx) and (ly > gly) and (lz > glz)
    and (hx < ghx) and (hy < ghy) and (hz < ghz) then
    begin
      g := cg;
      Break;
    end;
  end;
  if g = nil then
  begin
    New(g);
    AddGroup(g);
    g^.ParticleClass := CG2Particle3DClass(Particle.ClassType);
    g^.BMin.SetValue(lx, ly, lz);
    g^.BMax.SetValue(hx, hy, hz);
    g^.ItemCount := 0;
    g^.MaxSize := ms;
  end;
  if Length(g^.Items) <= g^.ItemCount then
  SetLength(g^.Items, Length(g^.Items) + 64);
  g^.Items[g^.ItemCount] := Particle;
  Inc(g^.ItemCount);
  if g^.MaxSize < ms then
  g^.MaxSize := ms;
  if lx < g^.BMin.x then g^.BMin.x := lx;
  if ly < g^.BMin.y then g^.BMin.y := ly;
  if lz < g^.BMin.z then g^.BMin.z := lz;
  if hx > g^.BMax.x then g^.BMax.x := hx;
  if hy > g^.BMax.y then g^.BMax.y := hy;
  if hz > g^.BMax.z then g^.BMax.z := hz;
end;

procedure TG2Particles3D.Clear;
  var i, j: Integer;
begin
  for i := 0 to m_GroupCount - 1 do
  begin
    for j := 0 to m_Groups[i].ItemCount - 1 do
    m_Groups[i].Items[j].Free;
    Dispose(m_Groups[i]);
  end;
  m_GroupCount := 0;
end;
//TG2Particles3D END

//TG2Particle3D BEGIN
procedure TG2Particle3D.SetSizeX(const Value: Single);
begin
  sx := Value * 0.5;
end;

procedure TG2Particle3D.SetSizeY(const Value: Single);
begin
  sy := Value * 0.5;
end;

procedure TG2Particle3D.SetSizeZ(const Value: Single);
begin
  sz := Value * 0.5;
end;

function TG2Particle3D.GetSizeX: Single;
begin
  Result := sx * 2;
end;

function TG2Particle3D.GetSizeY: Single;
begin
  Result := sy * 2;
end;

function TG2Particle3D.GetSizeZ: Single;
begin
  Result := sz * 2;
end;

procedure TG2Particle3D.GetBounds(var lx, ly, lz, hx, hy, hz: Single);
begin
  lx := m_X - sx;
  ly := m_Y - sy;
  lz := m_Z - sz;
  hx := m_X + sx;
  hy := m_Y + sy;
  hz := m_Z + sz;
end;

constructor TG2Particle3D.Create;
begin
  inherited Create;
  m_Dead := False;
  m_Sorted := False;
end;

destructor TG2Particle3D.Destroy;
begin
  inherited Destroy;
end;

procedure TG2Particle3D.Update;
begin

end;

procedure TG2Particle3D.Die;
begin
  m_Dead := True;
end;
//TG2Particle3D END

//TG2Particle3DRender BEGIN
constructor TG2Particle3DRender.Create(const G2Core: TG2Core);
begin
  inherited Create;
  m_Core := G2Core;
end;

destructor TG2Particle3DRender.Destroy;
begin
  inherited Destroy;
end;

procedure TG2Particle3DRender.RenderBegin;
begin

end;

procedure TG2Particle3DRender.RenderEnd;
begin

end;

procedure TG2Particle3DRender.RenderParticle(const Particle: TG2Particle3D);
begin

end;
//TG2Particle3DRender END

//TG2Particle3DEmmitter BEGIN
constructor TG2Particle3DEmmitter.Create(const Particles: TG2Particles3D);
begin
  inherited Create;
  m_Particles := Particles;
  m_Particles.AddEmmitter(Self);
end;

destructor TG2Particle3DEmmitter.Destroy;
begin
  m_Particles.RemoveEmmitter(Self);
  inherited Destroy;
end;
//TG2Particle3DEmmitter END

//TG2Particle3DEmmitterSingle BEGIN
constructor TG2Particle3DEmmitterSingle.Create(const Particles: TG2Particles3D);
begin
  inherited Create(Particles);
  m_Frequency := 1000;
  m_CreateTime := GetTickCount;
  ParticleType := TG2Particle3DBillboard;
end;

destructor TG2Particle3DEmmitterSingle.Destroy;
begin
  inherited Destroy;
end;

procedure TG2Particle3DEmmitterSingle.Update;
  var p: TG2Particle3D;
  var i: Integer;
  var n: DWord;
begin
  m_UpdateTime := m_UpdateTime + GetTickCount - m_CreateTime;
  n := m_UpdateTime div m_Frequency;
  m_UpdateTime := m_UpdateTime - n * m_Frequency;
  for i := 0 to n - 1 do
  begin
    p := ParticleType.Create;
    CreateParticle(p);
    m_CreateTime := GetTickCount;
    m_Particles.AddParticle(p);
  end;
end;
//TG2Particle3DEmmitterSingle END

//TG2Particle3DEmmitterSingleSphere BEGIN
constructor TG2Particle3DEmmitterSingleSphere.Create(const Particles: TG2Particles3D);
begin
  inherited Create(Particles);
  Pos.SetValue(0, 0, 0);
  Radius := 1;
end;

procedure TG2Particle3DEmmitterSingleSphere.CreateParticle(const p: TG2Particle3D);
  var v: TG2Vec3;
begin
  v := G2RandomSpherePoint;
  v := (v * Radius) + Pos;
  p.X := v.x;
  p.Y := v.y;
  p.Z := v.z;
end;
//TG2Particle3DEmmitterSingleSphere END

//TG2Particle3DEmmitterSingleBox BEGIN
constructor TG2Particle3DEmmitterSingleBox.Create(const Particles: TG2Particles3D);
begin
  inherited Create(Particles);
  Box.SetValue(G2Vec3(-0.5, -0.5, -0.5), G2Vec3(0.5, 0.5, 0.5));
end;

procedure TG2Particle3DEmmitterSingleBox.CreateParticle(const p: TG2Particle3D);
  var sx, sy, sz: Integer;
begin
  sx := Trunc(Box.MaxV.x - Box.MinV.x) * 100;
  sy := Trunc(Box.MaxV.y - Box.MinV.y) * 100;
  sz := Trunc(Box.MaxV.z - Box.MinV.z) * 100;
  p.X := (Random(sx + 1) / sx) * (Box.MaxV.x - Box.MinV.x) + Box.MinV.x;
  p.Y := (Random(sy + 1) / sy) * (Box.MaxV.y - Box.MinV.y) + Box.MinV.y;
  p.Z := (Random(sz + 1) / sz) * (Box.MaxV.z - Box.MinV.z) + Box.MinV.z;
end;
//TG2Particle3DEmmitterSingleBox END

//TG2Particle3DBillboard BEGIN
procedure TG2Particle3DBillboard.SetScale(const Value: Single);
begin
  m_Scale := Value;
  SizeX := m_Scale;
  SizeY := m_Scale;
  SizeZ := m_Scale;
end;

constructor TG2Particle3DBillboard.Create;
begin
  inherited Create;
  RenderClass := TG2Particle3DRenderBillboard;
  BlendMode.ColSrc := D3DBLEND_SRCCOLOR;
  BlendMode.ColDst := D3DBLEND_ONE;
  BlendMode.AlphaSrc := D3DBLEND_SRCCOLOR;
  BlendMode.AlphaDst := D3DBLEND_ONE;
  m_Scale := 1;
  m_Rotation := 0;
  m_Color := $ffffffff;
end;

destructor TG2Particle3DBillboard.Destroy;
begin
  inherited Destroy;
end;
//TG2Particle3DBillboard END

//TG2Particle3DBillboardAxis BEGIN
procedure TG2Particle3DBillboardAxis.Reallocate;
begin
  X := (m_PosSrc.x + m_PosDst.x) * 0.5;
  Y := (m_PosSrc.y + m_PosDst.y) * 0.5;
  Z := (m_PosSrc.z + m_PosDst.z) * 0.5;
  SizeX := Abs(m_PosSrc.x - m_PosDst.x) + m_Size * 2;
  SizeY := Abs(m_PosSrc.y - m_PosDst.y) + m_Size * 2;
  SizeZ := Abs(m_PosSrc.z - m_PosDst.z) + m_Size * 2;
end;

procedure TG2Particle3DBillboardAxis.SetSize(const Value: Single);
begin
  m_Size := Value;
  Reallocate;
end;

procedure TG2Particle3DBillboardAxis.SetPosSrc(const Value: TG2Vec3);
begin
  m_PosSrc := Value;
  Reallocate;
end;

procedure TG2Particle3DBillboardAxis.SetPosDst(const Value: TG2Vec3);
begin
  m_PosDst := Value;
  Reallocate;
end;

constructor TG2Particle3DBillboardAxis.Create;
begin
  inherited Create;
  RenderClass := TG2Particle3DRenderBillboardAxis;
  BlendMode.ColSrc := D3DBLEND_SRCCOLOR;
  BlendMode.ColDst := D3DBLEND_ONE;
  BlendMode.AlphaSrc := D3DBLEND_SRCCOLOR;
  BlendMode.AlphaDst := D3DBLEND_ONE;
  m_Size := 1;
  m_PosSrc.SetValue(0, 0, 0);
  m_PosDst.SetValue(0, 0, 0);
  m_Color := $ffffffff;
end;

destructor TG2Particle3DBillboardAxis.Destroy;
begin
  inherited Destroy;
end;
//TG2Particle3DBillboardAxis END

//TG2Particle3DFixed BEGIN
procedure TG2Particle3DFixed.ResetSize;
  var AABox: TG2AABox;
begin
  AABox.MinV.SetValue(0, 0, 0);
  AABox.MaxV.SetValue(0, 0, 0);
  AABox := AABox + (m_DirX * 0.5);
  AABox := AABox + (-m_DirX * 0.5);
  AABox := AABox + (m_DirY * 0.5);
  AABox := AABox + (-m_DirY * 0.5);
  SizeX := AABox.MaxV.x - AABox.MinV.x;
  SizeY := AABox.MaxV.y - AABox.MinV.y;
  SizeZ := AABox.MaxV.z - AABox.MinV.z;
end;

procedure TG2Particle3DFixed.SetDirX(const Value: TG2Vec3);
begin
  m_DirX := Value;
  ResetSize;
end;

procedure TG2Particle3DFixed.SetDirY(const Value: TG2Vec3);
begin
  m_DirY := Value;
  ResetSize;
end;

constructor TG2Particle3DFixed.Create;
begin
  inherited Create;
  RenderClass := TG2Particle3DRenderFixed;
  BlendMode.SetAdd;
  DirX := G2Vec3(1, 0, 0);
  DirX := G2Vec3(0, 0, 1);
  m_Color := $ffffffff;
end;

destructor TG2Particle3DFixed.Destroy;
begin
  inherited Destroy;
end;
//TG2Particle3DFixed END

//TG2Particle3DRenderBillboard BEGIN
procedure TG2Particle3DRenderQuadArr.RenderFlush;
  var m: TG2Mat;
  var PrevSrcCol, PrevDstCol, PrevSrcAlpha, PrevDstAlpha: DWord;
  var PrevSeparateAlpha: Boolean;
begin
  if m_CurQuad > 0 then
  begin
    PrevSeparateAlpha := m_Core.Graphics.RenderStates.SeparateAlphaBlendEnable;
    PrevSrcCol := m_Core.Graphics.RenderStates.SrcBlend;
    PrevDstCol := m_Core.Graphics.RenderStates.DestBlend;
    PrevSrcAlpha := m_Core.Graphics.RenderStates.SrcBlendAlpha;
    PrevDstAlpha := m_Core.Graphics.RenderStates.DestBlendAlpha;
    m_Core.Graphics.RenderStates.SrcBlend := m_CurBlendMode.ColSrc;
    m_Core.Graphics.RenderStates.DestBlend := m_CurBlendMode.ColDst;
    m_Core.Graphics.RenderStates.SrcBlendAlpha := m_CurBlendMode.AlphaSrc;
    m_Core.Graphics.RenderStates.DestBlendAlpha := m_CurBlendMode.AlphaDst;
    m_Core.Graphics.RenderStates.SeparateAlphaBlendEnable := m_CurBlendMode.SeparateAlpha;
    m := m_Core.Graphics.Transforms.WVP;
    m_Shader.SetMatrix('g_WVP', m);
    m_Core.Graphics.Device.SetTexture(0, m_CurTexture.Texture);
    m_Shader.CommitChanges;
    m_Core.Graphics.Device.DrawIndexedPrimitive(
      D3DPT_TRIANGLELIST,
      0,
      0, m_CurQuad * 4,
      0, m_CurQuad * 2
    );
    m_CurQuad := 0;
    m_Core.Graphics.RenderStates.SrcBlend := PrevSrcCol;
    m_Core.Graphics.RenderStates.DestBlend := PrevDstCol;
    m_Core.Graphics.RenderStates.SrcBlendAlpha := PrevSrcAlpha;
    m_Core.Graphics.RenderStates.DestBlendAlpha := PrevDstAlpha;
    m_Core.Graphics.RenderStates.SeparateAlphaBlendEnable := PrevSeparateAlpha;
  end;
end;

constructor TG2Particle3DRenderQuadArr.Create(const G2Core: TG2Core);
  type TVertex = record
    x, y, z, w: Single;
    tu, tv: Single;
  end;
  type TVertexArr = array[Word] of TVertex;
  type PVertexArr = ^TVertexArr;
  var Vertices: PVertexArr;
  var Indices: PG2Index16Array;
  var i: Integer;
  var FVFDecl: TFVFDeclaration;
begin
  inherited Create(G2Core);
  m_MaxQuads := (m_Core.Graphics.Caps.MaxVertexShaderConst - 8) div 4;
  m_Shader := m_Core.Graphics.ShaderLib.RequestEffect('fx_Particles3D');
  m_VB := TG2VB.Create;
  m_VB.Initialize(m_Core);
  m_IB := TG2IB.Create;
  m_IB.Initialize(m_Core);
  m_VB.Verify(
    SizeOf(TVertex),
    m_MaxQuads * 4,
    D3DUSAGE_WRITEONLY,
    0,
    D3DPOOL_MANAGED
  );
  m_IB.Verify(
    m_MaxQuads * 6,
    D3DUSAGE_WRITEONLY,
    D3DFMT_INDEX16,
    D3DPOOL_MANAGED
  );
  FVFDecl[0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  FVFDecl[1] := D3DVertexElement(0, 4 * 4, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  FVFDecl[2] := D3DDECL_END;
  m_Core.Graphics.Device.CreateVertexDeclaration(@FVFDecl, m_Decl);
  m_VB.Lock(0, SizeOf(TVertex) * m_MaxQuads * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  m_IB.Lock(0, m_MaxQuads * 12, Pointer(Indices), D3DLOCK_DISCARD);
  for i := 0 to m_MaxQuads - 1 do
  begin
    Vertices^[i * 4 + 0].x := -0.5;
    Vertices^[i * 4 + 0].y := 0.5;
    Vertices^[i * 4 + 0].z := 0;
    Vertices^[i * 4 + 0].w := i;
    Vertices^[i * 4 + 0].tu := 0;
    Vertices^[i * 4 + 0].tv := 0;
    Vertices^[i * 4 + 1].x := 0.5;
    Vertices^[i * 4 + 1].y := 0.5;
    Vertices^[i * 4 + 1].z := 0;
    Vertices^[i * 4 + 1].w := i;
    Vertices^[i * 4 + 1].tu := 1;
    Vertices^[i * 4 + 1].tv := 0;
    Vertices^[i * 4 + 2].x := -0.5;
    Vertices^[i * 4 + 2].y := -0.5;
    Vertices^[i * 4 + 2].z := 0;
    Vertices^[i * 4 + 2].w := i;
    Vertices^[i * 4 + 2].tu := 0;
    Vertices^[i * 4 + 2].tv := 1;
    Vertices^[i * 4 + 3].x := 0.5;
    Vertices^[i * 4 + 3].y := -0.5;
    Vertices^[i * 4 + 3].z := 0;
    Vertices^[i * 4 + 3].w := i;
    Vertices^[i * 4 + 3].tu := 1;
    Vertices^[i * 4 + 3].tv := 1;
    Indices^[i * 6 + 0] := i * 4 + 0;
    Indices^[i * 6 + 1] := i * 4 + 1;
    Indices^[i * 6 + 2] := i * 4 + 2;
    Indices^[i * 6 + 3] := i * 4 + 2;
    Indices^[i * 6 + 4] := i * 4 + 1;
    Indices^[i * 6 + 5] := i * 4 + 3;
  end;
  m_VB.UnLock;
  m_IB.UnLock;
end;

destructor TG2Particle3DRenderQuadArr.Destroy;
begin
  SafeRelease(m_Decl);
  m_IB.Finalize;
  m_IB.Free;
  m_VB.Finalize;
  m_VB.Free;
  inherited Destroy;
end;

procedure TG2Particle3DRenderQuadArr.RenderBegin;
begin
  m_CurQuad := 0;
  m_Core.Graphics.Device.SetVertexDeclaration(m_Decl);
  m_PrevSrcColorBlend := m_Core.Graphics.RenderStates.SrcBlend;
  m_PrevDstColorBlend := m_Core.Graphics.RenderStates.DestBlend;
  m_PrevSrcAlphaBlend := m_Core.Graphics.RenderStates.SrcBlendAlpha;
  m_PrevDstAlphaBlend := m_Core.Graphics.RenderStates.DestBlendAlpha;
  m_VB.SetToDevice;
  m_IB.SetToDevice;
  m_CurTexture := nil;
  m_CurBlendMode.ColSrc := $ff;
  m_CurBlendMode.ColDst := $ff;
  m_CurBlendMode.AlphaSrc := $ff;
  m_CurBlendMode.AlphaDst := $ff;
  m_Shader.Technique := 'QuadArr';
  m_Shader.BeginEffect(nil);
  m_Shader.BeginPass(0);
end;

procedure TG2Particle3DRenderQuadArr.RenderEnd;
begin
  RenderFlush;
  m_Core.Graphics.RenderStates.SrcBlend := m_PrevSrcColorBlend;
  m_Core.Graphics.RenderStates.DestBlend := m_PrevDstColorBlend;
  m_Core.Graphics.RenderStates.SrcBlendAlpha := m_PrevSrcAlphaBlend;
  m_Core.Graphics.RenderStates.DestBlendAlpha := m_PrevDstAlphaBlend;
  m_Shader.EndPass;
  m_Shader.EndEffect;
end;

procedure TG2Particle3DRenderQuadArr.RenderParticle(const Particle: TG2Particle3D);
  var p: TG2Particle3DQuadTex;
begin
  p := TG2Particle3DQuadTex(Particle);
  if (m_CurQuad >= m_MaxQuads)
  or (p.Texture <> m_CurTexture)
  or (p.BlendMode <> m_CurBlendMode) then
  begin
    RenderFlush;
    m_CurTexture := p.Texture;
    m_CurBlendMode := p.BlendMode;
  end;
end;
//TG2Particle3DRenderQuadArr END

//TG2Particle3DRenderBillboard BEGIN
procedure TG2Particle3DRenderBillboard.RenderBegin;
begin
  inherited RenderBegin;
  m_ViewFlip := m_Core.Graphics.Transforms.Vb;
end;

procedure TG2Particle3DRenderBillboard.RenderParticle(const Particle: TG2Particle3D);
  var p: TG2Particle3DBillboard;
  var VSConst: packed record
    Col: TD3DColorValue;
    m: TG2Mat;
  end;
begin
  inherited RenderParticle(Particle);
  p := TG2Particle3DBillboard(Particle);
  VSConst.Col := p.Color;
  VSConst.m.SetScaling(p.Scale, p.Scale, p.Scale);
  if p.Rotation <> 0 then
  VSConst.m.RotateZ(p.Rotation);
  VSConst.m := VSConst.m * m_ViewFlip;
  VSConst.m.Translate(p.X, p.Y, p.Z);
  VSConst.m := VSConst.m.Transpose;
  m_Core.Graphics.Device.SetVertexShaderConstantF(6 + CurQuad * 4, @VSConst, 4);
  CurQuad := CurQuad + 1;
end;
//TG2Particle3DRenderBillboard END

//TG2Particle3DRenderBillboardAxis BEGIN
procedure TG2Particle3DRenderBillboardAxis.RenderParticle(const Particle: TG2Particle3D);
  var p: TG2Particle3DBillboardAxis;
  var VSConst: packed record
    Col: TD3DColorValue;
    m: TG2Mat;
  end;
  var AxisX, AxisY, AxisZ, Pos: TG2Vec3;
begin
  inherited RenderParticle(Particle);
  p := TG2Particle3DBillboardAxis(Particle);
  VSConst.Col := p.Color;
  Pos.SetValue(p.X, p.Y, p.Z);
  AxisX := p.PosDst - p.PosSrc;
  AxisZ := m_Core.Graphics.Transforms.Vpos - Pos;
  AxisY := AxisX.Cross(AxisZ).Normalized * p.Size;
  AxisZ := AxisY.Cross(AxisX).Normalized;
  VSConst.m.SetValue(AxisX, AxisY, AxisZ, Pos);
  VSConst.m := VSConst.m.Transpose;
  m_Core.Graphics.Device.SetVertexShaderConstantF(6 + CurQuad * 4, @VSConst, 4);
  CurQuad := CurQuad + 1;
end;
//TG2Particle3DRenderBillboardAxis END

//TG2Particle3DRenderFixed BEGIN
procedure TG2Particle3DRenderFixed.RenderParticle(const Particle: TG2Particle3D);
  var p: TG2Particle3DFixed;
  var VSConst: packed record
    Col: TD3DColorValue;
    m: TG2Mat;
  end;
  var AxisX, AxisY, AxisZ, Pos: TG2Vec3;
begin
  inherited RenderParticle(Particle);
  p := TG2Particle3DFixed(Particle);
  VSConst.Col := p.Color;
  Pos.SetValue(p.X, p.Y, p.Z);
  AxisX := p.DirX;
  AxisZ := G2Vec3(0, 0, 0);
  AxisY := p.DirY;
  VSConst.m.SetValue(AxisX, AxisY, AxisZ, Pos);
  VSConst.m := VSConst.m.Transpose;
  m_Core.Graphics.Device.SetVertexShaderConstantF(6 + CurQuad * 4, @VSConst, 4);
  CurQuad := CurQuad + 1;
end;
//TG2Particle3DRenderFixed END

end.