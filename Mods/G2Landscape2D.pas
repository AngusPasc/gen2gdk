//G2Landscape2D v1.0
unit G2Landscape2D;

{$include ../Gen2.inc}

interface

uses
  Classes,
  Gen2,
  G2Math,
  Math,
  Types,
  Direct3D9;

type
  TG2Landscape2D = class (TG2Module)
  strict private
  type
    PVProp = ^TVProp;
    TVProp = record
      Alpha: array of Byte;
      Color: TG2Color;
      Height: Single;
      Index: Word;
    end;
    TQuadBounds = record
      Current: Boolean;
      QStart: Integer;
      QEnd: Integer;
    end;
    TLayer = class
    strict private
      m_Landscape: TG2Landscape2D;
      m_Texture: TG2Texture2D;
      m_Layer: Integer;
      procedure SetTexture(const Value: TG2Texture2D); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      procedure SetAlpha(const x, y: Integer; const Value: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
      function GetAlpha(const x, y: Integer): Byte; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    private
      VStart: Word;
      VCount: Word;
      IStart: Word;
      PCount: Word;
    public
      constructor Create(const Landscape: TG2Landscape2D; const Layer: Integer);
      property Texture: TG2Texture2D read m_Texture write SetTexture;
      property Alpha[const x, y: Integer]: Byte read GetAlpha write SetAlpha;
    end;
  var
    m_Gfx: TG2Graphics;
    m_X: Integer;
    m_Y: Integer;
    m_QCountX: Integer;
    m_QCountY: Integer;
    m_QSizeX: Integer;
    m_QSizeY: Integer;
    m_LCount: Integer;
    m_VProps: array of array of TVProp;
    m_QBounds: array of array of TQuadBounds;
    m_Layers: array of TLayer;
    m_VB: TG2VB;
    m_IB: TG2IB;
    m_UpdateBuffers: Boolean;
    m_UpdateData: Boolean;
    m_UpdateLayers: Boolean;
    m_UpdateLight: Boolean;
    m_LightRect: TRect;
    m_ShadowRange: Integer;
    procedure ComputeDrawBounds(const qx, qy: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVProp(const x, y: Integer): PVProp; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetX(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetY(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetQCountX(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetQCountY(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetQSizeX(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetQSizeY(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLayer(const Index: Integer): TLayer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetLCount(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetHeight(const x, y: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetHeight(const x, y: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure DoUpdateLight;
  private
    property UpdateData: Boolean read m_UpdateData write m_UpdateData;
    procedure SetAlpha(const x, y, l: Integer; const Value: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlpha(const x, y, l: Integer): Byte; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property X: Integer read m_X write SetX;
    property Y: Integer read m_Y write SetY;
    property QCountX: Integer read m_QCountX write SetQCountX;
    property QCountY: Integer read m_QCountY write SetQCountY;
    property QSizeX: Integer read m_QSizeX write SetQSizeX;
    property QSizeY: Integer read m_QSizeY write SetQSizeY;
    property LCount: Integer read m_LCount write SetLCount;
    property Heights[const x, y: Integer]: Single read GetHeight write SetHeight;
    property VProps[const x, y: Integer]: PVProp read GetVProp;
    property Layers[const Index: Integer]: TLayer read GetLayer;
    procedure Render;
    procedure Update;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;

implementation

//TG2Landscape2D.TLayer BEGIN
constructor TG2Landscape2D.TLayer.Create(const Landscape: TG2Landscape2D; const Layer: Integer);
begin
  inherited Create;
  m_Landscape := Landscape;
  m_Layer := Layer;
end;

procedure TG2Landscape2D.TLayer.SetTexture(const Value: TG2Texture2D);
begin
  if not Assigned(m_Texture) and Assigned(Value) then
  m_Landscape.UpdateData := True;
  m_Texture := Value;
end;

procedure TG2Landscape2D.TLayer.SetAlpha(const x, y: Integer; const Value: Byte);
begin
  m_Landscape.SetAlpha(x, y, m_Layer, Value);
end;

function TG2Landscape2D.TLayer.GetAlpha(const x, y: Integer): Byte;
begin
  Result := m_Landscape.GetAlpha(x, y, m_Layer);
end;
//TG2Landscape2D.TLayer END

//TG2Landscape2D BEGIN
procedure TG2Landscape2D.ComputeDrawBounds(const qx, qy: Integer);
var
  i, h1, h2: Integer;
  v: Single;
begin
  h1 := qy * m_QSizeY;
  h2 := h1 + m_QSizeY;
  m_QBounds[qx, qy].QStart := qy;
  for i := qy - 1 downto 0 do
  begin
    v := (i + 1) * m_QSizeY;
    if (v - m_VProps[qx, i + 1].Height > h1)
    or (v - m_VProps[qx + 1, i + 1].Height > h1) then
    m_QBounds[qx, qy].QStart := i
    else
    Break;
  end;
  m_QBounds[qx, qy].QEnd := qy;
  for i := qy + 1 to m_QCountY - 1  do
  begin
    v := i * m_QSizeY;
    if (v - m_VProps[qx, i].Height < h2)
    or (v - m_VProps[qx + 1, i].Height < h2) then
    m_QBounds[qx, qy].QEnd := i;
  end;
  m_QBounds[qx, qy].Current := True;
end;

function TG2Landscape2D.GetVProp(const x, y: Integer): PVProp;
begin
  Result := @m_VProps[x, y];
end;

procedure TG2Landscape2D.SetX(const Value: Integer);
begin
  if m_X <> Value then
  begin
    m_X := Value;
    m_UpdateData := True;
  end;
end;

procedure TG2Landscape2D.SetY(const Value: Integer);
begin
  if m_Y <> Value then
  begin
    m_Y := Value;
    m_UpdateData := True;
  end;
end;

procedure TG2Landscape2D.SetQCountX(const Value: Integer);
begin
  if m_QCountX <> Value then
  begin
    m_QCountX := Value;
    m_UpdateBuffers := True;
  end;
end;

procedure TG2Landscape2D.SetQCountY(const Value: Integer);
begin
  if m_QCountY <> Value then
  begin
    m_QCountY := Value;
    m_UpdateBuffers := True;
  end;
end;

procedure TG2Landscape2D.SetQSizeX(const Value: Integer);
begin
  if m_QSizeX <> Value then
  begin
    m_QSizeX := Value;
    m_UpdateData := True;
  end;
end;

procedure TG2Landscape2D.SetQSizeY(const Value: Integer);
begin
  if m_QSizeY <> Value then
  begin
    m_QSizeY := Value;
    m_UpdateData := True;
  end;
end;

function TG2Landscape2D.GetLayer(const Index: Integer): TLayer;
begin
  Result := m_Layers[Index];
end;

procedure TG2Landscape2D.SetLCount(const Value: Integer);
begin
  m_LCount := Value;
  m_UpdateLayers := True;
end;

procedure TG2Landscape2D.SetHeight(const x, y: Integer; const Value: Single);
var
  yh: Integer;
begin
  if (x >= 0) and (x <= m_QCountX) and (y >= 0) and (y <= m_QCountY) then
  begin
    m_VProps[x, y].Height := Value;
    m_UpdateData := True;
    if (x < m_QCountX) and (y < m_QCountY) then
    m_QBounds[x, y].Current := False;
    if (x > 0) and (y < m_QCountY) then
    m_QBounds[x - 1, y].Current := False;
    if (x < m_QCountX) and (y > 0) then
    m_QBounds[x, y - 1].Current := False;
    if (x > 0) and (y > 0) then
    m_QBounds[x - 1, y - 1].Current := False;
    yh := y - Trunc(Value) div m_QSizeY;
    if (x < m_QCountX) and (yh > 0) then
    m_QBounds[x, yh].Current := False;
    if (x > 0) and (yh > 0) then
    m_QBounds[x - 1, yh].Current := False;
    if not m_UpdateLight then
    begin
      m_LightRect.Left := x;
      m_LightRect.Right := x;
      m_LightRect.Top := y;
      m_LightRect.Bottom := y;
    end
    else
    begin
      m_LightRect.Left := Min(m_LightRect.Left, x);
      m_LightRect.Right := Max(m_LightRect.Right, x);
      m_LightRect.Top := Min(m_LightRect.Top, y);
      m_LightRect.Bottom := Max(m_LightRect.Bottom, y);
    end;
    m_UpdateLight := True;
  end;
end;

function TG2Landscape2D.GetHeight(const x, y: Integer): Single;
var
  vx, vy: Integer;
begin
  vx := x;
  vy := y;
  if vx < 0 then vx := 0;
  if vx > m_QCountX then vx := m_QCountX;
  if vy < 0 then vy := 0;
  if vy > m_QCountY then vy := m_QCountY;
  Result := m_VProps[vx, vy].Height;
end;

procedure TG2Landscape2D.DoUpdateLight;
var
  i, j, d, r, n, c, h: Integer;
  LR: TRect;
  vn, vl, nrm: TG2Vec3;
begin
  vl.SetValue(-1, 1, 1);
  vl.Normalize;
  d := Trunc(Sqrt(m_QSizeX * m_QSizeX + m_QSizeY * m_QSizeY));
  r := Max(m_ShadowRange div d + 1, 1);
  d := d div 4;
  LR.Left := m_LightRect.Left - r;
  LR.Top := m_LightRect.Top - r;
  LR.Right := m_LightRect.Right + r;
  LR.Bottom := m_LightRect.Bottom + r;
  if LR.Left < 0 then LR.Left := 0;
  if LR.Top < 0 then LR.Top := 0;
  if LR.Right > m_QCountX then LR.Right := m_QCountX;
  if LR.Bottom > m_QCountY then LR.Bottom := m_QCountY;
  for j := LR.Top to LR.Bottom do
  for i := LR.Left to LR.Right do
  begin
    vn.SetValue(0, 0, 0);
    if (i > 0) and (j > 0) then
    begin
      nrm := G2TriangleNormal(
        G2Vec3(0, -m_QSizeY, m_VProps[i, j - 1].Height),
        G2Vec3(0, 0, m_VProps[i, j].Height),
        G2Vec3(-m_QSizeX, 0, m_VProps[i - 1, j].Height)
      );
      vn := vn + nrm;
    end;
    if (i < m_QCountX) and (j > 0) then
    begin
      nrm := G2TriangleNormal(
        G2Vec3(m_QCountX, 0, m_VProps[i + 1, j].Height),
        G2Vec3(0, 0, m_VProps[i, j].Height),
        G2Vec3(0, -m_QSizeY, m_VProps[i, j - 1].Height)
      );
      vn := vn + nrm;
    end;
    if (i < m_QCountX) and (j < m_QCountY) then
    begin
      nrm := G2TriangleNormal(
        G2Vec3(0, m_QSizeY, m_VProps[i, j + 1].Height),
        G2Vec3(0, 0, m_VProps[i, j].Height),
        G2Vec3(m_QSizeX, 0, m_VProps[i + 1, j].Height)
      );
      vn := vn + nrm;
    end;
    if (i > 0) and (j < m_QCountY) then
    begin
      nrm := G2TriangleNormal(
        G2Vec3(-m_QSizeX, 0, m_VProps[i - 1, j].Height),
        G2Vec3(0, 0, m_VProps[i, j].Height),
        G2Vec3(0, m_QSizeY, m_VProps[i, j + 1].Height)
      );
      vn := vn + nrm;
    end;
    vn.Normalize;
//    c := Round(255 * Min(Max((vn.Dot(vl) - 0.2) * 4, 0.1), 1));
    c := Round(255 * Min(Max(vn.Dot(vl) * 0.5 + 0.5, 0.1), 1));
    for n := 1 to r do
    begin
      h := Round(GetHeight(i - n, j + n) - (m_VProps[i, j].Height + d * n));
      if h > 0 then
      c := Round(c * (1 - Sin((n / r) * Pi * 0.6)));
    end;
    m_VProps[i, j].Color.r := c;
    m_VProps[i, j].Color.g := c;
    m_VProps[i, j].Color.b := c;

  end;
  m_UpdateLight := False;
  m_UpdateData := True;
end;

procedure TG2Landscape2D.SetAlpha(const x, y, l: Integer; const Value: Byte);
begin
  m_VProps[x, y].Alpha[l] := Value;
  m_UpdateData := True;
end;

function TG2Landscape2D.GetAlpha(const x, y, l: Integer): Byte;
begin
  Result := m_VProps[x, y].Alpha[l];
end;

procedure TG2Landscape2D.Render;
var
  l: Integer;
  PrevZEnable: Boolean;
begin
  m_VB.SetToDevice;
  m_IB.SetToDevice;
  PrevZEnable := m_Gfx.RenderStates.ZEnable;
  m_Gfx.RenderStates.ZEnable := True;
  for l := 0 to m_LCount - 1 do
  if Assigned(m_Layers[l].Texture) then
  begin
    m_Gfx.Device.SetTexture(0, m_Layers[l].Texture.Texture);
    m_Gfx.Device.DrawIndexedPrimitive(
      D3DPT_TRIANGLELIST,
      0,
      m_Layers[l].VStart,
      m_Layers[l].VCount,
      m_Layers[l].IStart,
      m_Layers[l].PCount
    );
  end;
  m_Gfx.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2Landscape2D.Update;
type
  TVertex = packed record
    Pos: TG2Vec4;
    Color: TG2Color;
    TexCoord: TG2Vec2;
  end;
  TVertexArray = array[Word] of TVertex;
  PVertexArray = ^TVertexArray;
var
  ResizeMap: Boolean;
  OldQSX, OldQSY, NewQSX, NewQSY, OldLCount, i, j, l, vx, vy: Integer;
  Vertices: PVertexArray;
  Indices: PG2Index16Array;
  QRect, DRect: TRect;
  TPQX, TPQY: Single;
  q, CurV: Word;
begin
  if m_UpdateBuffers then
  begin
    m_VB.Verify(
      SizeOf(TVertex),
      (m_QCountX + 1) * (m_QCountY + 1) * m_LCount,
      D3DUSAGE_WRITEONLY,
      D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1,
      D3DPOOL_DEFAULT
    );
    m_IB.Verify(
      m_QCountX * m_QCountY * 6 * m_LCount,
      D3DUSAGE_WRITEONLY,
      D3DFMT_INDEX16,
      D3DPOOL_DEFAULT
    );
    ResizeMap := False;
    OldQSX := Length(m_QBounds);
    if OldQSX > 0 then
    OldQSY := Length(m_QBounds[0])
    else
    OldQSY := 0;
    NewQSX := OldQSX;
    NewQSY := OldQSY;
    if m_QCountX > OldQSX then
    begin
      ResizeMap := True;
      NewQSX := m_QCountX;
    end;
    if m_QCountY > OldQSY then
    begin
      ResizeMap := True;
      NewQSY := m_QCountY;
    end;
    if ResizeMap then
    begin
      SetLength(m_VProps, NewQSX + 1, NewQSY + 1);
      SetLength(m_QBounds, NewQSX, NewQSY);
      OldLCount := Length(m_Layers);
      if (OldQSX = 0) and (OldQSY = 0) then
      begin
        for i := 0 to NewQSX - 1 do
        for j := 0 to NewQSY - 1 do
        begin
          m_QBounds[i, j].Current := False;
          m_QBounds[i, j].QStart := j;
          m_QBounds[i, j].QEnd := j;
        end;
        for i := 0 to NewQSX do
        for j := 0 to NewQSY do
        begin
          SetLength(m_VProps[i, j].Alpha, OldLCount);
          for l := 0 to OldLCount - 1 do
          m_VProps[i, j].Alpha[l] := $ff;
          m_VProps[i, j].Color := $ffffffff;
          m_VProps[i, j].Height := 0;
        end;
      end
      else
      begin
        for i := 0 to OldQSX - 1 do
        for j := OldQSY to NewQSY - 1 do
        begin
          m_QBounds[i, j].Current := False;
          m_QBounds[i, j].QStart := j;
          m_QBounds[i, j].QEnd := j;
        end;
        for i := OldQSX to NewQSX - 1 do
        for j := 0 to NewQSY - 1 do
        begin
          m_QBounds[i, j].Current := False;
          m_QBounds[i, j].QStart := j;
          m_QBounds[i, j].QEnd := j;
        end;
        for i := 0 to OldQSX do
        for j := OldQSY + 1 to NewQSY do
        begin
          SetLength(m_VProps[i, j].Alpha, OldLCount);
          for l := 0 to OldLCount - 1 do
          m_VProps[i, j].Alpha[l] := $ff;
          m_VProps[i, j].Color := $ffffffff;
          m_VProps[i, j].Height := 0;
        end;
        for i := OldQSX + 1 to NewQSX do
        for j := 0 to NewQSY do
        begin
          SetLength(m_VProps[i, j].Alpha, OldLCount);
          for l := 0 to OldLCount - 1 do
          m_VProps[i, j].Alpha[l] := $ff;
          m_VProps[i, j].Color := $ffffffff;
          m_VProps[i, j].Height := 0;
        end;
      end;
    end;
    m_UpdateData := True;
    m_UpdateBuffers := False;
  end;
  if m_UpdateLayers then
  begin
    OldLCount := Length(m_Layers);
    if OldLCount < m_LCount then
    begin
      SetLength(m_Layers, m_LCount);
      for i := 0 to m_QCountX do
      for j := 0 to m_QCountY do
      begin
        SetLength(m_VProps[i, j].Alpha, m_LCount);
        for l := OldLCount to m_LCount - 1 do
        m_VProps[i, j].Alpha[l] := $ff;
      end;
      for l := OldLCount to m_LCount - 1 do
      begin
        m_Layers[l] := TLayer.Create(Self, l);
        m_Layers[l].Texture := nil;
      end;
    end;
    m_UpdateLayers := False;
  end;
  if m_UpdateLight then
  begin
    DoUpdateLight;
  end;
  if m_UpdateData then
  begin
    QRect.Left := -m_X div m_QSizeX;
    QRect.Top := -m_Y div m_QSizeY;
    QRect.Right := (-m_X + m_Gfx.Params.Width) div m_QSizeX;
    QRect.Bottom := (-m_Y + m_Gfx.Params.Height) div m_QSizeY;
    if QRect.Left < 0 then QRect.Left := 0;
    if QRect.Top < 0 then QRect.Top := 0;
    if QRect.Right > m_QCountX - 1 then QRect.Right := m_QCountX - 1;
    if QRect.Bottom > m_QCountY - 1 then QRect.Bottom := m_QCountY - 1;
    m_VB.Lock(
      0, (QRect.Right - QRect.Left + 2) * (m_QCountY + 1) * SizeOf(TVertex),
      Pointer(Vertices), 0
    );
    m_IB.Lock(
      0, (QRect.Right - QRect.Left + 1) * (m_QCountY) * 12,
      Pointer(Indices), 0
    );
    q := 0;
    CurV := 0;
    for l := 0 to m_LCount - 1 do
    if Assigned(m_Layers[l].Texture) then
    begin
      for i := 0 to m_QCountX do
      for j := 0 to m_QCountY do
      m_VProps[i, j].Index := $ffff;
      m_Layers[l].VStart := CurV;
      m_Layers[l].IStart := q * 6;
      TPQX := m_QSizeX / m_Layers[l].Texture.RealWidth;
      TPQY := m_QSizeY / m_Layers[l].Texture.RealHeight;
      for i := QRect.Left to QRect.Right do
      begin
        if not m_QBounds[i, QRect.Top].Current then
        ComputeDrawBounds(i, QRect.Top);
        if not m_QBounds[i, QRect.Bottom].Current then
        ComputeDrawBounds(i, QRect.Bottom);
        for j := m_QBounds[i, QRect.Top].QStart to m_QBounds[i, QRect.Bottom].QEnd do
        begin
          for vy := j to j + 1 do
          for vx := i to i + 1 do
          if m_VProps[vx, vy].Index = $ffff then
          begin
            Vertices^[CurV].Pos.x := vx * m_QSizeX + m_X + 0.5;
            Vertices^[CurV].Pos.y := vy * m_QSizeY - m_VProps[vx, vy].Height + m_Y + 0.5;
            Vertices^[CurV].Pos.z := 1 - vy / (m_QCountY + 1);
            Vertices^[CurV].Pos.w := 1;
            Vertices^[CurV].Color.r := m_VProps[vx, vy].Color.r;
            Vertices^[CurV].Color.g := m_VProps[vx, vy].Color.g;
            Vertices^[CurV].Color.b := m_VProps[vx, vy].Color.b;
            Vertices^[CurV].Color.a := m_VProps[vx, vy].Alpha[l];
            Vertices^[CurV].TexCoord.x := vx * TPQX;
            Vertices^[CurV].TexCoord.y := vy * TPQY;
            m_VProps[vx, vy].Index := CurV;
            Inc(CurV);
          end;
          Indices^[q * 6 + 0] := m_VProps[i, j].Index;
          Indices^[q * 6 + 1] := m_VProps[i + 1, j].Index;
          Indices^[q * 6 + 2] := m_VProps[i, j + 1].Index;
          Indices^[q * 6 + 3] := m_VProps[i, j + 1].Index;
          Indices^[q * 6 + 4] := m_VProps[i + 1, j].Index;
          Indices^[q * 6 + 5] := m_VProps[i + 1, j + 1].Index;
          Inc(q);
        end;
      end;
      m_Layers[l].VCount := CurV - m_Layers[l].VStart;
      m_Layers[l].PCount := (q * 6 - m_Layers[l].IStart) div 3;
    end;
    m_IB.UnLock;
    m_VB.UnLock;
    m_UpdateData := False;
  end;
end;

function TG2Landscape2D.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Gfx := Core.Graphics;
  m_QCountX := 32;
  m_QCountY := 32;
  m_QSizeX := 64;
  m_QSizeY := 64;
  m_LCount := 0;
  m_LightRect := Rect(0, 0, m_QCountX, m_QCountY);
  m_ShadowRange := 512;
  m_UpdateLight := True;
  m_UpdateBuffers := True;
  m_UpdateData := True;
  m_VB := TG2VB.Create;
  m_VB.Initialize(Core);
  m_IB := TG2IB.Create;
  m_IB.Initialize(Core);
  Result := grOk;
end;

function TG2Landscape2D.Finalize: TG2Result;
var
  i: Integer;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  for i := 0 to m_LCount - 1 do
  m_Layers[i].Free;
  m_IB.Finalize;
  m_IB.Free;
  m_VB.Finalize;
  m_VB.Free;
  Result := grOk;
end;
//TG2Landscape2D END

end.