//G2PerlinNoise v1.0
unit G2PerlinNoise;

{$include Gen2.inc}

interface

uses
  G2Math;

type
  TG2PerlinNoise = class
  strict private
    m_Seamless: Boolean;
    m_PatternWidth: Integer;
    m_PatternHeight: Integer;
    m_PatternDepth: Integer;
    m_CurPW: Integer;
    m_CurPH: Integer;
    m_CurPD: Integer;
    m_RandFactor: Integer;
    m_ProcNoise2D: function (const x, y: Integer): Single of Object;
    m_ProcNoise3D: function (const x, y, z: Integer): Single of Object;
    function Noise2DStandard(const x, y: Integer): Single;
    function Noise2DSeamless(const x, y: Integer): Single;
    function Noise2DSmooth(const x, y: Integer): Single;
    function Noise2DInterpolated(const x, y: Single): Single;
    function Noise3DStandard(const x, y, z: Integer): Single;
    function Noise3DSeamless(const x, y, z: Integer): Single;
    function Noise3DSmooth(const x, y, z: Integer): Single;
    function Noise3DInterpolated(const x, y, z: Single): Single;
    procedure SetSeamless(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Seamless: Boolean read m_Seamless write SetSeamless;
    property PatternWidth: Integer read m_PatternWidth write m_PatternWidth;
    property PatternHeight: Integer read m_PatternHeight write m_PatternHeight;
    property PatternDepth: Integer read m_PatternDepth write m_PatternDepth;
    function PerlinNoise2D(
      const x, y: Single;
      const Octaves: Integer;
      const Persistence: Single = 0.5;
      const Frequency: Single = 0.25;
      const Amplitude: Single = 1;
      const RandFactor: Integer = 0
    ): Single;
    function PerlinNoise3D(
      const x, y, z: Single;
      const Octaves: Integer;
      const Persistence: Single = 0.5;
      const Frequency: Single = 0.25;
      const Amplitude: Single = 1;
      const RandFactor: Integer = 0
    ): Single;
  end;

implementation

//TG2PerlinNoise BEGIN
constructor TG2PerlinNoise.Create;
begin
  inherited Create;
  m_Seamless := False;
  m_ProcNoise2D := Noise2DStandard;
  m_ProcNoise3D := Noise3DStandard;
  m_PatternWidth := 256;
  m_PatternHeight := 256;
  m_PatternDepth := 256;
end;

destructor TG2PerlinNoise.Destroy;
begin
  inherited Destroy;
end;

function TG2PerlinNoise.Noise2DStandard(const x, y: Integer): Single;
var
  n: Integer;
  l_x, l_y: Integer;
const
  c1 = 1 / 1073741824;
begin
  l_x := x + m_RandFactor;
  l_y := y + m_RandFactor;
  n := l_x + l_y * 57;
  n := (n shl 13) xor n;
  Result := (1.0 - ( (n * (n * n * 15731 + 789221) + 1376312589) and $7fffffff) * c1);
end;

function TG2PerlinNoise.Noise2DSeamless(const x, y: Integer): Single;
var
  n: Integer;
  l_x, l_y: Integer;
const
  c1 = 1 / 1073741824;
begin
  if x >= 0 then
  l_x := (x mod m_CurPW) + m_RandFactor
  else
  l_x := (m_CurPW - (Abs(x) mod m_CurPW)) + m_RandFactor;
  if y >= 0 then
  l_y := (y mod m_CurPH) + m_RandFactor
  else
  l_y := (m_CurPH - (Abs(y) mod m_CurPH)) + m_RandFactor;
  n := l_x + l_y * 57;
  n := (n shl 13) xor n;
  Result := (1.0 - ( (n * (n * n * 15731 + 789221) + 1376312589) and $7fffffff) * c1);
end;

function TG2PerlinNoise.Noise2DSmooth(const x, y: Integer): Single;
var
  Corners, Sides, Center: Single;
const
  c1 = 1 / 16;
  c2 = 1 / 8;
  c3 = 1 / 4;
begin
  Corners := (m_ProcNoise2D(x - 1, y - 1) + m_ProcNoise2D(x + 1, y - 1) + m_ProcNoise2D(x - 1, y + 1) + m_ProcNoise2D(x + 1, y + 1)) * c1;
  Sides := (m_ProcNoise2D(x - 1, y) + m_ProcNoise2D(x + 1, y) + m_ProcNoise2D(x, y - 1) + m_ProcNoise2D(x, y + 1)) * c2;
  Center :=  m_ProcNoise2D(x, y) * c3;
  Result := Corners + Sides + Center;
end;

function TG2PerlinNoise.Noise2DInterpolated(const x, y: Single): Single;
var
  IntX, IntY: Integer;
  FracX, FracY: Single;
  v1, v2, v3, v4, i1, i2: Single;
begin
  IntX := Trunc(x);
  FracX := x - IntX;
  IntY := Trunc(y);
  FracY := y - IntY;
  v1 := Noise2DSmooth(IntX, IntY);
  v2 := Noise2DSmooth(IntX + 1, IntY);
  v3 := Noise2DSmooth(IntX, IntY + 1);
  v4 := Noise2DSmooth(IntX + 1, IntY + 1);
  i1 := G2CosrpFloat(v1, v2, FracX);
  i2 := G2CosrpFloat(v3, v4, FracX);
  Result := G2CosrpFloat(i1, i2, FracY);
end;

function TG2PerlinNoise.Noise3DStandard(const x, y, z: Integer): Single;
var
  n: Integer;
  l_x, l_y, l_z: Integer;
const
  c1 = 1 / 1073741824;
begin
  l_x := x + m_RandFactor;
  l_y := y + m_RandFactor;
  l_z := z + m_RandFactor;
  n := l_x + l_y * 57 + l_z * 113;
  n := (n shl 13) xor n;
  Result := (1 - ( (n * (n * n * 15731 + 789221) + 1376312589) and $7fffffff) * c1);
end;

function TG2PerlinNoise.Noise3DSeamless(const x, y, z: Integer): Single;
var
  n: Integer;
  l_x, l_y, l_z: Integer;
const
  c1 = 1 / 1073741824;
begin
  if x >= 0 then
  l_x := (x mod m_CurPW) + m_RandFactor
  else
  l_x := (m_CurPW - (Abs(x) mod m_CurPW)) + m_RandFactor;
  if y >= 0 then
  l_y := (y mod m_CurPH) + m_RandFactor
  else
  l_y := (m_CurPH - (Abs(y) mod m_CurPH)) + m_RandFactor;
  if z >= 0 then
  l_z := (z mod m_CurPD) + m_RandFactor
  else
  l_z := (m_CurPD - (Abs(z) mod m_CurPD)) + m_RandFactor;
  n := l_x + l_y * 57 + l_z * 113;
  n := (n shl 13) xor n;
  Result := (1 - ( (n * (n * n * 15731 + 789221) + 1376312589) and $7fffffff) * c1);
end;

function TG2PerlinNoise.Noise3DSmooth(const x, y, z: Integer): Single;
var
  Corners, Sides, Center: Single;
const
  c1 = 1 / 32;
  c2 = 1 / 24;
  c3 = 1 / 4;
begin
  Corners := (
    m_ProcNoise3D(x - 1, y - 1, z - 1) +
    m_ProcNoise3D(x + 1, y - 1, z - 1) +
    m_ProcNoise3D(x - 1, y + 1, z - 1) +
    m_ProcNoise3D(x + 1, y + 1, z - 1) +
    m_ProcNoise3D(x - 1, y - 1, z + 1) +
    m_ProcNoise3D(x + 1, y - 1, z + 1) +
    m_ProcNoise3D(x - 1, y + 1, z + 1) +
    m_ProcNoise3D(x + 1, y + 1, z + 1)
  ) * c1;
  Sides := (
    m_ProcNoise3D(x - 1, y, z - 1) +
    m_ProcNoise3D(x + 1, y, z - 1) +
    m_ProcNoise3D(x, y - 1, z - 1) +
    m_ProcNoise3D(x, y + 1, z - 1) +
    m_ProcNoise3D(x - 1, y, z) +
    m_ProcNoise3D(x + 1, y, z) +
    m_ProcNoise3D(x, y - 1, z) +
    m_ProcNoise3D(x, y + 1, z) +
    m_ProcNoise3D(x - 1, y, z + 1) +
    m_ProcNoise3D(x + 1, y, z + 1) +
    m_ProcNoise3D(x, y - 1, z + 1) +
    m_ProcNoise3D(x, y + 1, z + 1)
  ) * c2;
  Center :=  m_ProcNoise3D(x, y, z) * c3;
  Result := Corners + Sides + Center;
end;

function TG2PerlinNoise.Noise3DInterpolated(const x, y, z: Single): Single;
var
  IntX, IntY, IntZ: Integer;
  FracX, FracY, FracZ: Single;
  v: array[0..3, 0..1] of Single;
  s1, s2, n1, n2: Single;
begin
  IntX := Trunc(x);
  FracX := x - IntX;
  IntY := Trunc(y);
  FracY := y - IntY;
  IntZ := Trunc(z);
  FracZ := z - IntZ;
  v[0, 0] := Noise3DSmooth(IntX, IntY, IntZ);
  v[1, 0] := Noise3DSmooth(IntX + 1, IntY, IntZ);
  v[2, 0] := Noise3DSmooth(IntX, IntY + 1, IntZ);
  v[3, 0] := Noise3DSmooth(IntX + 1, IntY + 1, IntZ);
  v[0, 1] := Noise3DSmooth(IntX, IntY, IntZ + 1);
  v[1, 1] := Noise3DSmooth(IntX + 1, IntY, IntZ + 1);
  v[2, 1] := Noise3DSmooth(IntX, IntY + 1, IntZ + 1);
  v[3, 1] := Noise3DSmooth(IntX + 1, IntY + 1, IntZ + 1);
  s1 := G2CosrpFloat(v[0, 0], v[1, 0], FracX);
  s2 := G2CosrpFloat(v[2, 0], v[3, 0], FracX);
  n1 := G2CosrpFloat(s1, s2, FracY);
  s1 := G2CosrpFloat(v[0, 1], v[1, 1], FracX);
  s2 := G2CosrpFloat(v[2, 1], v[3, 1], FracX);
  n2 := G2CosrpFloat(s1, s2, FracY);
  Result := G2CosrpFloat(n1, n2, FracZ);
end;

procedure TG2PerlinNoise.SetSeamless(const Value: Boolean);
begin
  m_Seamless := Value;
  if m_Seamless then
  begin
    m_ProcNoise2D := Noise2DSeamless;
    m_ProcNoise3D := Noise3DSeamless;
  end
  else
  begin
    m_ProcNoise2D := Noise2DStandard;
    m_ProcNoise3D := Noise3DStandard;
  end;
end;

function TG2PerlinNoise.PerlinNoise2D(
      const x, y: Single;
      const Octaves: Integer;
      const Persistence: Single = 0.5;
      const Frequency: Single = 0.25;
      const Amplitude: Single = 1;
      const RandFactor: Integer = 0
    ): Single;
var
  i: Integer;
  Total: Single;
  l_Persistence: Single;
  l_Frequency: Single;
  l_Amplitude: Single;
begin
  if Octaves <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  Total := 0;
  l_Persistence := Persistence;

  l_Frequency := Frequency;
  l_Amplitude := Amplitude;

  m_RandFactor := RandFactor;

  for i := 0 to Octaves - 1 do
  begin
    m_CurPW := Trunc(m_PatternWidth * l_Frequency);
    m_CurPH := Trunc(m_PatternHeight * l_Frequency);
    Total := Total + Noise2DInterpolated(x * l_Frequency, y * l_Frequency) * l_Amplitude;
    l_Amplitude := l_Amplitude * l_Persistence;
    l_Frequency := l_Frequency * 2;
  end;
  //if Total > 1 then Total := 1 else if Total < 0 then Total := 0;
  Result := Total;
end;

function TG2PerlinNoise.PerlinNoise3D(
      const x, y, z: Single;
      const Octaves: Integer;
      const Persistence: Single = 0.5;
      const Frequency: Single = 0.25;
      const Amplitude: Single = 1;
      const RandFactor: Integer = 0
    ): Single;
var
  i: Integer;
  Total: Single;
  l_Persistence: Single;
  l_Frequency: Single;
  l_Amplitude: Single;
begin
  if Octaves <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  Total := 0;
  l_Persistence := Persistence;

  l_Frequency := Frequency;
  l_Amplitude := Amplitude;

  m_RandFactor := RandFactor;

  for i := 0 to Octaves - 1 do
  begin
    m_CurPW := Trunc(m_PatternWidth * l_Frequency);
    m_CurPH := Trunc(m_PatternHeight * l_Frequency);
    m_CurPD := Trunc(m_PatternDepth * l_Frequency);
    Total := Total + Noise3DInterpolated(
      x * l_Frequency,
      y * l_Frequency,
      z * l_Frequency
    ) * l_Amplitude;
    l_Amplitude := l_Amplitude * l_Persistence;
    l_Frequency := l_Frequency * 2;
  end;
  Result := Total;   
end;
//TG2PerlinNoise END

end.