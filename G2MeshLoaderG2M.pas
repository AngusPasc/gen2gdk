//G2MeshLoaderG2M v1.0
//Author: Dan

unit G2MeshLoaderG2M;

{$include Gen2.inc}

{-------------------------------------------------------------------------------
G2Mesh Format:
  Header: String4 = "G2M "

  Blocks:
    Block Name: String4
    Block Size: Int4

  Block "NMAP":
    NodeCount: Int4
    NodeOffsets[NodeCount]: Int4

  Block "NDAT":
    Nodes[NodeCount]:
      OwnerID: Int4
      NodeName: StringNT
      NodeTransform: Mat4x3

  Block "GMAP":
    GeomCount: Int4
    GeomOffsets[GeomCount]: Int4

  Block "GDAT":
    Geoms[GeomCount]:
      NodeID: Int4
      VCount: Int4
      CCount: Int4
      FCount: Int4
      MCount: Int4
      TCount: Int4
      Positions[VCount]: Vec3f
      Colors[CCount]: Col3
      TexChannels[TCount]:
        TVCount: Int4
        TexCoords[TVCount]: Vec2f
      Materials[MCount]: Int4
      Faces[FCount]:
        FaceVertices: Vec3i
        FaceColors: Vec3i
        FaceTexCoords[TCount]: Vec3i
        FaceGroup: Int4
        FaceMaterial: Int4

  Block "AMAP":
    AnimCount: Int4
    AnimOffsets[AnimCount]: Int4

  Block "ADAT":
    Animations[AnimCount]:
      Name: StringNT
      FrameCount: Int4
      FPS: Int4
      AnimNodeCount: Int4
      AnimNodes[AnimNodeCount]:
        NodeID: Int4
        Frames[FrameCount]:
          FrameTransform: Mat4x3

  Block "SMAP":
    SkinCount: Int4
    SkinOffsets[SkinCount]: Int4

  Block "SDAT":
    Skins[SkinCount]:
      GeomID: Int4
      BoneCount: Int4
      Bones[BoneCount]:
        NodeID: Int4
        Bind: Mat4x3
      Vertices[Geoms[GeomID].VCount]:
        WeightCount: Int4
        Weights[WeightCount]:
          BoneID: Int4
          Weight: Float4

  Block "MMAP":
    MaterialCount: Int4
    MaterialOffsets[MaterialCount]: Int4

  Block "MDAT":
    Materials[MaterialCount]:
      ChannelCount: Int4
      Channels[ChannelCount]:
        MatName: StringNT
        TwoSided: Bool
        AmbientColor: Byte[3]
        DiffuseColor: Byte[3]
        SpecularColor: Byte[3]
        SpecularColorAmount: Float4
        SpecularPower: Float4
        EmmissiveColor: Byte[3]
        EmmissiveColorAmount: Float4
        AmbientMapEnable: Bool
        AmbientMap: StringNT
        AmbientMapAmount: Float4
        DiffuseMapEnable: Bool
        DiffuseMap: StringNT
        DiffuseMapAmount: Float4
        SpecularMapEnable: Bool
        SpecularMap: StringNT
        SpecularMapAmount: Float4
        OpacityMapEnable: Bool
        OpacityMap: StringNT
        OpacityMapAmount: Float4
        IlluminationMapEnable: Bool
        IlluminationMap: StringNT
        IlluminationMapAmount: Float4
        BumpMapEnable: Bool
        BumpMap: StringNT
        BumpMapAmount: Float4
-------------------------------------------------------------------------------}

interface

uses
  Gen2,
  G2Math,
  SysUtils,
  Types,
  Classes,
  Windows,
  G2MeshLoader,
  Math,
  Direct3D9;

type
  PG2MeshDataG2M = ^TG2MeshDataG2M;
  TG2MeshDataG2M = packed record
  public
    NodeCount: Integer;
    Nodes: array of packed record
    public
      OwnerID: Integer;
      Name: AnsiString;
      Transform: TG2Mat;
    end;
    GeomCount: Integer;
    Geoms: array of packed record
    public
      NodeID: Integer;
      VCount: Integer;
      CCount: Integer;
      FCount: Integer;
      MCount: Integer;
      TCount: Integer;
      Vertices: array of TG2Vec3;
      Colors: array of TG2Color;
      TexChannels: array of packed record
      public
        TVCount: Integer;
        TexCoords: array of TG2Vec2;
      end;
      Materials: array of Integer;
      Faces: array of packed record
      public
        Vertices: array[0..2] of Integer;
        Colors: array[0..2] of Integer;
        TexChannels: array of array[0..2] of Integer;
        SmoothGroup: Integer;
        Material: Integer;
      end;
    end;
    AnimCount: Integer;
    Anims: array of packed record
    public
      Name: AnsiString;
      FrameCount: Integer;
      FrameRate: Integer;
      AnimNodeCount: Integer;
      AnimNodes: array of packed record
      public
        NodeID: Integer;
        Frames: array of TG2Mat;
      end;
    end;
    SkinCount: Integer;
    Skins: array of packed record
    public
      GeomID: Integer;
      BoneCount: Integer;
      Bones: array of packed record
      public
        NodeID: Integer;
        Bind: TG2Mat;
      end;
      Vertices: array of packed record
      public
        WeightCount: Integer;
        Weights: array of packed record
        public
          BoneID: Integer;
          Weight: Single;
        end;
      end;
    end;
    MaterialCount: Integer;
    Materials: array of packed record
    public
      ChannelCount: Integer;
      Channels: array of record
      public
        Name: AnsiString;
        TwoSided: Boolean;
        AmbientColor: TG2Color;
        DiffuseColor: TG2Color;
        SpecularColor: TG2Color;
        SpecularColorAmount: Single;
        SpecularPower: Single;
        EmmissiveColor: TG2Color;
        EmmissiveColorAmount: Single;
        AmbientMapEnable: Boolean;
        AmbientMap: AnsiString;
        AmbientMapAmount: Single;
        DiffuseMapEnable: Boolean;
        DiffuseMap: AnsiString;
        DiffuseMapAmount: Single;
        SpecularMapEnable: Boolean;
        SpecularMap: AnsiString;
        SpecularMapAmount: Single;
        OpacityMapEnable: Boolean;
        OpacityMap: AnsiString;
        OpacityMapAmount: Single;
        IlluminationMapEnable: Boolean;
        IlluminationMap: AnsiString;
        IlluminationMapAmount: Single;
        BumpMapEnable: Boolean;
        BumpMap: AnsiString;
        BumpMapAmount: Single;
      end;
    end;
    LightCount: Integer;
    Lights: array of packed record
    public
      NodeID: Integer;
      LightType: Byte;
      Enabled: Boolean;
      Color: TG2Color;
      AttStart: Single;
      AttEnd: Single;
      SpotInner: Single;
      SpotOutter: Single;
      WidthInner: Single;
      WidthOutter: Single;
      HeightInner: Single;
      HeightOutter: Single;
    end;
    RagDollCount: Integer;
    RagDolls: array of packed record
    public
      NodeID: Integer;
      Head: TG2RagdollObject;
      Neck: TG2RagdollObject;
      Pelvis: TG2RagdollObject;
      BodyNodeCount: Integer;
      BodyNodes: array of TG2RagdollObject;
      ArmRNodeCount: Integer;
      ArmRNodes: array of TG2RagdollObject;
      ArmLNodeCount: Integer;
      ArmLNodes: array of TG2RagdollObject;
      LegRNodeCount: Integer;
      LegRNodes: array of TG2RagdollObject;
      LegLNodeCount: Integer;
      LegLNodes: array of TG2RagdollObject;
    end;
  end;

//TG2MeshLoaderG2M BEGIN
  TG2MeshLoaderG2M = class (TG2MeshLoader)
  strict private
    m_Stream: TStream;
    m_InitPos: Int64;
    m_MeshData: TG2MeshDataG2M;
    function GetMeshData: PG2MeshDataG2M; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  strict protected
    function ReadHeader: AnsiString; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadString: AnsiString;
    function ReadQuaternion: TG2Quat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadVector3: TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadVector2: TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadInteger: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadFloat: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadDWord: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadWord: Word; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadSmallInt: SmallInt; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadByte: Byte; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadShortInt: ShortInt; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadBoolean: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Advance(const Count: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadMatrix: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadColor3: TG2Color; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function FindBlock(const Header: AnsiString): Boolean;
    procedure LoadNodes;
    procedure LoadNode(const ID: Integer);
    procedure LoadGeoms;
    procedure LoadGeom(const ID: Integer);
    procedure LoadAnims;
    procedure LoadAnim(const ID: Integer);
    procedure LoadSkins;
    procedure LoadSkin(const ID: Integer);
    procedure LoadMaterials;
    procedure LoadMaterial(const ID: Integer);
    procedure LoadLights;
    procedure LoadLight(const ID: Integer);
    procedure LoadRagdolls;
    procedure LoadRagdollObject(const ro: PG2RagdollObject);
  public
    property MeshData: PG2MeshDataG2M read GetMeshData;
    class function GetDifinition: AnsiString; override;
    class function CanLoadStream(const s: TStream): Boolean; override;
    procedure LoadStream(const s: TStream); override;
    procedure ExportMesh(const Device: IDirect3DDevice9; const MeshData: PG2MeshData); override;
  end;
//TG2MeshLoaderG2M END

implementation

uses
  D3DX9;

//TG2MeshLoaderG2M BEGIN
class function TG2MeshLoaderG2M.GetDifinition: AnsiString;
begin
  Result := 'Gen2 Mesh Loader';
end;

class function TG2MeshLoaderG2M.CanLoadStream(const s: TStream): Boolean;
var
  Def: AnsiString;
  Pos: Int64;
begin
  Pos := s.Position;
  SetLength(Def, 4);
  s.Read(Def[1], 4);
  s.Position := Pos;
  Result := Def = 'G2M ';
end;

function TG2MeshLoaderG2M.GetMeshData: PG2MeshDataG2M;
begin
  Result := @m_MeshData;
end;

function TG2MeshLoaderG2M.ReadHeader: AnsiString;
begin
  SetLength(Result, 4);
  m_Stream.Read(Result[1], 4);
end;

function TG2MeshLoaderG2M.ReadString: AnsiString;
var
  Pos: Int64;
  b: Byte;
  Len: Integer;
begin
  if not Assigned(m_Stream) then Exit;
  Pos := m_Stream.Position;
  Len := 0;
  repeat
    m_Stream.Read(b, 1);
    Inc(Len);
  until b = 0;
  Dec(Len);
  m_Stream.Position := Pos;
  SetLength(Result, Len);
  m_Stream.Read(Result[1], Len);
  m_Stream.Position := m_Stream.Position + 1;
end;

function TG2MeshLoaderG2M.ReadQuaternion: TG2Quat;
begin
  m_Stream.Read(Result, 16);
end;

function TG2MeshLoaderG2M.ReadVector3: TG2Vec3;
begin
  m_Stream.Read(Result, 12);
end;

function TG2MeshLoaderG2M.ReadVector2: TG2Vec2;
begin
  m_Stream.Read(Result, 8);
end;

function TG2MeshLoaderG2M.ReadInteger: Integer;
begin
  m_Stream.Read(Result, 4);
end;

function TG2MeshLoaderG2M.ReadFloat: Single;
begin
  m_Stream.Read(Result, 4);
end;

function TG2MeshLoaderG2M.ReadDWord: DWord;
begin
  m_Stream.Read(Result, 4);
end;

function TG2MeshLoaderG2M.ReadWord: Word;
begin
  m_Stream.Read(Result, 2)
end;

function TG2MeshLoaderG2M.ReadSmallInt: SmallInt;
begin
  m_Stream.Read(Result, 2);
end;

function TG2MeshLoaderG2M.ReadByte: Byte;
begin
  m_Stream.Read(Result, 1);
end;

function TG2MeshLoaderG2M.ReadShortInt: ShortInt;
begin
  m_Stream.Read(Result, 1);
end;

function TG2MeshLoaderG2M.ReadBoolean: Boolean;
begin
  m_Stream.Read(Result, 1);
end;

procedure TG2MeshLoaderG2M.Advance(const Count: Integer);
begin
  m_Stream.Position := m_Stream.Position + Count;
end;

function TG2MeshLoaderG2M.ReadMatrix: TG2Mat;
var
  m4x3: array[0..3, 0..2] of Single;
  x, y: Integer;
  m: TG2MatRef absolute Result;
begin
  m_Stream.ReadBuffer(m4x3, SizeOf(m4x3));
  Result.SetIdentity;
  for x := 0 to 3 do
  for y := 0 to 2 do
  m.m[x, y] := m4x3[x, y];
end;

function TG2MeshLoaderG2M.ReadColor3: TG2Color;
begin
  Result.r := ReadByte;
  Result.g := ReadByte;
  Result.b := ReadByte;
  Result.a := $ff;
end;

function TG2MeshLoaderG2M.FindBlock(const Header: AnsiString): Boolean;
var
  Len: Integer;
  CurHeader: AnsiString;
begin
  m_Stream.Position := m_InitPos;
  while m_Stream.Position < m_Stream.Size - 8 do
  begin
    CurHeader := ReadHeader;
    if UpperCase(String(CurHeader)) = UpperCase(String(Header)) then
    begin
      Result := True;
      m_Stream.Position := m_Stream.Position + 4;
      Exit;
    end
    else
    begin
      m_Stream.Read(Len, 4);
      m_Stream.Position := m_Stream.Position + Len;
    end;
  end;
  Result := False;
end;

procedure TG2MeshLoaderG2M.LoadNodes;
var
  Offsets: array of Integer;
  i: Integer;
begin
  with m_MeshData do
  begin
    NodeCount := ReadInteger;
    SetLength(Nodes, NodeCount);
    SetLength(Offsets, NodeCount);
    m_Stream.Read(Offsets[0], NodeCount * 4);
    if FindBlock('NDAT') then
    for i := 0 to NodeCount - 1 do
    begin
      m_Stream.Position := Offsets[i];
      LoadNode(i);
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadNode(const ID: Integer);
begin
  with m_MeshData do
  begin
    with Nodes[ID] do
    begin
      OwnerID := ReadInteger;
      Name := ReadString;
      Transform := ReadMatrix;
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadGeoms;
var
  Offsets: array of Integer;
  i: Integer;
begin
  with m_MeshData do
  begin
    GeomCount := ReadInteger;
    SetLength(Geoms, GeomCount);
    SetLength(Offsets, GeomCount);
    m_Stream.Read(Offsets[0], GeomCount * 4);
    if FindBlock('GDAT') then
    for i := 0 to GeomCount - 1 do
    begin
      m_Stream.Position := Offsets[i];
      LoadGeom(i);
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadGeom(const ID: Integer);
var
  i, t, f: Integer;
begin
  with m_MeshData do
  begin
    with Geoms[ID] do
    begin
      NodeID := ReadInteger;
      VCount := ReadInteger;
      CCount := ReadInteger;
      FCount := ReadInteger;
      MCount := ReadInteger;
      TCount := ReadInteger;
      SetLength(Vertices, VCount);
      SetLength(Colors, CCount);
      SetLength(Faces, FCount);
      SetLength(Materials, MCount);
      SetLength(TexChannels, TCount);
      m_Stream.Read(Vertices[0], 12 * VCount);
      for i := 0 to CCount - 1 do
      Colors[i] := ReadColor3;
      for t := 0 to TCount - 1 do
      with TexChannels[t] do
      begin
        TVCount := ReadInteger;
        SetLength(TexCoords, TVCount);
        m_Stream.Read(TexCoords[0], TVCount * 8);
      end;
      m_Stream.Read(Materials[0], MCount * 4);
      for f := 0 to FCount - 1 do
      with Faces[f] do
      begin
        m_Stream.Read(Vertices, 12);
        m_Stream.Read(Colors, 12);
        SetLength(TexChannels, TCount);
        m_Stream.Read(TexChannels[0], TCount * 12);
        SmoothGroup := ReadInteger;
        Material := ReadInteger;
      end;
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadAnims;
var
  Offsets: array of Integer;
  i: Integer;
begin
  with m_MeshData do
  begin
    AnimCount := ReadInteger;
    SetLength(Anims, AnimCount);
    SetLength(Offsets, AnimCount);
    m_Stream.Read(Offsets[0], AnimCount * 4);
    if FindBlock('GDAT') then
    for i := 0 to AnimCount - 1 do
    begin
      m_Stream.Position := Offsets[i];
      LoadAnim(i);
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadAnim(const ID: Integer);
var
  n, f: Integer;
begin
  with m_MeshData do
  begin
    with Anims[ID] do
    begin
      Name := ReadString;
      FrameCount := ReadInteger;
      FrameRate := ReadInteger;
      AnimNodeCount := ReadInteger;
      SetLength(AnimNodes, AnimNodeCount);
      for n := 0 to AnimNodeCount - 1 do
      with AnimNodes[n] do
      begin
        NodeID := ReadInteger;
        SetLength(Frames, FrameCount);
        for f := 0 to FrameCount - 1 do
        Frames[f] := ReadMatrix;
      end;
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadSkins;
var
  Offsets: array of Integer;
  i: Integer;
begin
  with m_MeshData do
  begin
    SkinCount := ReadInteger;
    SetLength(Skins, SkinCount);
    SetLength(Offsets, SkinCount);
    m_Stream.Read(Offsets[0], SkinCount * 4);
    if FindBlock('SDAT') then
    for i := 0 to SkinCount - 1 do
    begin
      m_Stream.Position := Offsets[i];
      LoadSkin(i);
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadSkin(const ID: Integer);
var
  b, v, w: Integer;
begin
  with m_MeshData do
  begin
    with Skins[ID] do
    begin
      GeomID := ReadInteger;
      BoneCount := ReadInteger;
      SetLength(Bones, BoneCount);
      for b := 0 to BoneCount - 1 do
      with Bones[b] do
      begin
        NodeID := ReadInteger;
        Bind := ReadMatrix;
      end;
      SetLength(Vertices, Geoms[GeomID].VCount);
      for v := 0 to Geoms[GeomID].VCount - 1 do
      with Vertices[v] do
      begin
        WeightCount := ReadInteger;
        SetLength(Weights, WeightCount);
        for w := 0 to WeightCount - 1 do
        with Weights[w] do
        begin
          BoneID := ReadInteger;
          Weight := ReadFloat;
        end;
      end;
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadMaterials;
var
  Offsets: array of Integer;
  i: Integer;
begin
  with m_MeshData do
  begin
    MaterialCount := ReadInteger;
    SetLength(Materials, MaterialCount);
    SetLength(Offsets, MaterialCount);
    m_Stream.Read(Offsets[0], MaterialCount * 4);
    if FindBlock('MDAT') then
    for i := 0 to MaterialCount - 1 do
    begin
      m_Stream.Position := Offsets[i];
      LoadMaterial(i);
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadMaterial(const ID: Integer);
  var i: Integer;
begin
  with m_MeshData do
  begin
    with Materials[ID] do
    begin
      ChannelCount := ReadInteger;
      SetLength(Channels, ChannelCount);
      for i := 0 to ChannelCount - 1 do
      with Channels[i] do
      begin
        Name := ReadString;
        TwoSided := ReadBoolean;
        AmbientColor := ReadColor3;
        DiffuseColor := ReadColor3;
        SpecularColor := ReadColor3;
        SpecularColorAmount := ReadFloat;
        SpecularPower := ReadFloat;
        EmmissiveColor := ReadColor3;
        EmmissiveColorAmount := ReadFloat;
        AmbientMapEnable := ReadBoolean;
        AmbientMap := ReadString;
        AmbientMapAmount := ReadFloat;
        DiffuseMapEnable := ReadBoolean;
        DiffuseMap := ReadString;
        DiffuseMapAmount := ReadFloat;
        SpecularMapEnable := ReadBoolean;
        SpecularMap := ReadString;
        SpecularMapAmount := ReadFloat;
        OpacityMapEnable := ReadBoolean;
        OpacityMap := ReadString;
        OpacityMapAmount := ReadFloat;
        IlluminationMapEnable := ReadBoolean;
        IlluminationMap := ReadString;
        IlluminationMapAmount := ReadFloat;
        BumpMapEnable := ReadBoolean;
        BumpMap := ReadString;
        BumpMapAmount := ReadFloat;
      end;
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadLights;
var
  Offsets: array of Integer;
  i: Integer;
begin
  with m_MeshData do
  begin
    LightCount := ReadInteger;
    SetLength(Lights, LightCount);
    SetLength(Offsets, LightCount);
    m_Stream.Read(Offsets[0], LightCount * 4);
    if FindBlock('LDAT') then
    for i := 0 to LightCount - 1 do
    begin
      m_Stream.Position := Offsets[i];
      LoadLight(i);
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadLight(const ID: Integer);
begin
  with m_MeshData do
  begin
    with Lights[ID] do
    begin
      NodeID := ReadInteger;
      LightType := ReadByte;
      Enabled := ReadBoolean;
      Color := ReadColor3;
      AttStart := ReadFloat;
      AttEnd := ReadFloat;
      SpotInner := ReadFloat;
      SpotOutter := ReadFloat;
      WidthInner := ReadFloat;
      WidthOutter := ReadFloat;
      HeightInner := ReadFloat;
      HeightOutter := ReadFloat;
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadRagdolls;
  var i, j: Integer;
begin
  with m_MeshData do
  begin
    RagdollCount := ReadInteger;
    SetLength(Ragdolls, RagdollCount);
    for i := 0 to RagdollCount - 1 do
    begin
      Ragdolls[i].NodeID := ReadInteger;
      LoadRagdollObject(@Ragdolls[i].Head);
      LoadRagdollObject(@Ragdolls[i].Neck);
      LoadRagdollObject(@Ragdolls[i].Pelvis);
      Ragdolls[i].BodyNodeCount := ReadInteger;
      SetLength(Ragdolls[i].BodyNodes, Ragdolls[i].BodyNodeCount);
      for j := 0 to Ragdolls[i].BodyNodeCount - 1 do
      LoadRagdollObject(@Ragdolls[i].BodyNodes[j]);
      Ragdolls[i].ArmRNodeCount := ReadInteger;
      SetLength(Ragdolls[i].ArmRNodes, Ragdolls[i].ArmRNodeCount);
      for j := 0 to Ragdolls[i].ArmRNodeCount - 1 do
      LoadRagdollObject(@Ragdolls[i].ArmRNodes[j]);
      Ragdolls[i].ArmLNodeCount := ReadInteger;
      SetLength(Ragdolls[i].ArmLNodes, Ragdolls[i].ArmLNodeCount);
      for j := 0 to Ragdolls[i].ArmLNodeCount - 1 do
      LoadRagdollObject(@Ragdolls[i].ArmLNodes[j]);
      Ragdolls[i].LegRNodeCount := ReadInteger;
      SetLength(Ragdolls[i].LegRNodes, Ragdolls[i].LegRNodeCount);
      for j := 0 to Ragdolls[i].LegRNodeCount - 1 do
      LoadRagdollObject(@Ragdolls[i].LegRNodes[j]);
      Ragdolls[i].LegLNodeCount := ReadInteger;
      SetLength(Ragdolls[i].LegLNodes, Ragdolls[i].LegLNodeCount);
      for j := 0 to Ragdolls[i].LegLNodeCount - 1 do
      LoadRagdollObject(@Ragdolls[i].LegLNodes[j]);
    end;
  end;
end;

procedure TG2MeshLoaderG2M.LoadRagdollObject(const ro: PG2RagdollObject);
  var i: Integer;
begin
  ro^.NodeID := ReadInteger;
  ro^.MinV := ReadVector3;
  ro^.MaxV := ReadVector3;
  ro^.Transform := ReadMatrix;
  ro^.DependantCount := ReadInteger;
  SetLength(ro^.Dependants, ro^.DependantCount);
  for i := 0 to ro^.DependantCount - 1 do
  begin
    ro^.Dependants[i].NodeID := ReadInteger;
    ro^.Dependants[i].Offset := ReadMatrix;
  end;
end;

procedure TG2MeshLoaderG2M.LoadStream(const s: TStream);
begin
  m_Stream := s;
  if ReadHeader <> 'G2M ' then Exit;
  m_InitPos := m_Stream.Position;
  if FindBlock('NMAP') then
  begin
    LoadNodes;
    if FindBlock('AMAP') then
    LoadAnims;
    if FindBlock('GMAP') then
    begin
      LoadGeoms;
      if FindBlock('SMAP') then
      LoadSkins;
    end;
    if FindBlock('MMAP') then
    LoadMaterials;
    if FindBlock('LMAP') then
    LoadLights;
    if FindBlock('RGDL') then
    LoadRagdolls;
  end;
end;

procedure TG2MeshLoaderG2M.ExportMesh(const Device: IDirect3DDevice9; const MeshData: PG2MeshData);
  procedure CopyRagdollObject(const Src, Dst: PG2RagdollObject);
    var i: Integer;
  begin
    Dst^.NodeID := Src^.NodeID;
    Dst^.MinV := Src^.MinV;
    Dst^.MaxV := Src^.MaxV;
    Dst^.Transform := Src^.Transform;
    Dst^.DependantCount := Src^.DependantCount;
    SetLength(Dst^.Dependants, Dst^.DependantCount);
    for i := 0 to Dst^.DependantCount - 1 do
    begin
      Dst^.Dependants[i].NodeID := Src^.Dependants[i].NodeID;
      Dst^.Dependants[i].Offset := Src^.Dependants[i].Offset;
    end;
  end;
type
  TGroupVertex = record
    PosID: Integer;
    ColID: Integer;
    TexIDs: array of Integer;
    GroupID: Integer;
    NormalID: Integer;
  end;
  TNormalGroup = record
    PosID: Integer;
    GroupID: Integer;
    Normal: TG2Vec3;
  end;
  TGeomVertex = packed record
    Position: TG2Vec3;
    Tangent: TG2Vec3;
    Binormal: TG2Vec3;
    Normal: TG2Vec3;
    TexCoords: TG2Vec2;
    Color: TG2Color;
  end;
  PGeomVertexArray = ^TGeomVertexArray;
  TGeomVertexArray = array[Word] of TGeomVertex;
var
  i, j, k, l, m, r, g: Integer;
  GroupVertices: array of TGroupVertex;
  GroupVertexCount: Integer;
  GroupIndices: array of Word;
  NormalGroups: array of TNormalGroup;
  NormalGroupCount: Integer;
  n: TG2Vec3;
  Mesh, TmpMesh: ID3DXMesh;
  VertexRemaps: array of array of DWord;
  Decl: TFVFDeclaration;
  Vertices: PGeomVertexArray;
  Ptr: Pointer;
  Adj: array of DWord;
  VertexMapping: ID3DXBuffer;
begin
  inherited ExportMesh(Device, MeshData);
  MeshData^.NodeCount := m_MeshData.NodeCount;
  SetLength(MeshData^.Nodes, MeshData^.NodeCount);
  MeshData^.GeomCount := m_MeshData.GeomCount;
  SetLength(MeshData^.Geoms, MeshData^.GeomCount);
  MeshData^.SkinCount := m_MeshData.SkinCount;
  SetLength(MeshData^.Skins, MeshData^.SkinCount);
  MeshData^.AnimCount := m_MeshData.AnimCount;
  SetLength(MeshData^.Anims, MeshData^.AnimCount);
  MeshData^.MaterialCount := m_MeshData.MaterialCount;
  SetLength(MeshData^.Materials, MeshData^.MaterialCount);
  MeshData^.RagDollCount := m_MeshData.RagDollCount;
  SetLength(MeshData^.RagDolls, MeshData^.RagDollCount);
  for i := 0 to MeshData^.NodeCount - 1 do
  begin
    MeshData^.Nodes[i].OwnerID := m_MeshData.Nodes[i].OwnerID;
    MeshData^.Nodes[i].Name := m_MeshData.Nodes[i].Name;
    MeshData^.Nodes[i].Transform := m_MeshData.Nodes[i].Transform;
  end;
  SetLength(VertexRemaps, MeshData^.GeomCount);
  for i := 0 to MeshData^.GeomCount - 1 do
  begin
    MeshData^.Geoms[i].NodeID := m_MeshData.Geoms[i].NodeID;
    MeshData^.Geoms[i].SkinID := -1;
    MeshData^.Geoms[i].FCount := m_MeshData.Geoms[i].FCount;
    MeshData^.Geoms[i].TCount := m_MeshData.Geoms[i].TCount;
    MeshData^.Geoms[i].MCount := m_MeshData.Geoms[i].MCount;
    SetLength(MeshData^.Geoms[i].Materials, MeshData^.Geoms[i].MCount);
    SetLength(MeshData^.Geoms[i].Faces, MeshData^.Geoms[i].FCount);
    SetLength(GroupIndices, m_MeshData.Geoms[i].FCount * 3);
    GroupVertexCount := 0;
    NormalGroupCount := 0;
    for j := 0 to m_MeshData.Geoms[i].FCount - 1 do
    begin
      n := G2TriangleNormal(
        m_MeshData.Geoms[i].Vertices[m_MeshData.Geoms[i].Faces[j].Vertices[0]],
        m_MeshData.Geoms[i].Vertices[m_MeshData.Geoms[i].Faces[j].Vertices[1]],
        m_MeshData.Geoms[i].Vertices[m_MeshData.Geoms[i].Faces[j].Vertices[2]]
      );
      MeshData^.Geoms[i].Faces[j].MaterialID := m_MeshData.Geoms[i].Faces[j].Material;
      for k := 0 to 2 do
      begin
        g := -1;
        for l := 0 to NormalGroupCount - 1 do
        if (NormalGroups[l].PosID = m_MeshData.Geoms[i].Faces[j].Vertices[k])
        and (NormalGroups[l].GroupID = m_MeshData.Geoms[i].Faces[j].SmoothGroup)then
        begin
          g := l;
          Break;
        end;
        r := -1;
        if g > -1 then
        begin
          NormalGroups[g].Normal := NormalGroups[g].Normal + n;
          for l := 0 to GroupVertexCount - 1 do
          if (GroupVertices[l].PosID = m_MeshData.Geoms[i].Faces[j].Vertices[k])
          and (GroupVertices[l].ColID = m_MeshData.Geoms[i].Faces[j].Colors[k])
          and (GroupVertices[l].GroupID = m_MeshData.Geoms[i].Faces[j].SmoothGroup) then
          begin
            r := l;
            for m := 0 to m_MeshData.Geoms[i].TCount - 1 do
            if GroupVertices[l].TexIDs[m] <> m_MeshData.Geoms[i].Faces[j].TexChannels[m][k] then
            r := -1;
            if r > -1 then Break;
          end;
        end
        else
        begin
          if Length(NormalGroups) <= NormalGroupCount then
          begin
            if Length(NormalGroups) < 16 then
            SetLength(NormalGroups, 16)
            else
            SetLength(NormalGroups, Length(NormalGroups) * 2);
          end;
          g := NormalGroupCount;
          Inc(NormalGroupCount);
          NormalGroups[g].PosID := m_MeshData.Geoms[i].Faces[j].Vertices[k];
          NormalGroups[g].GroupID := m_MeshData.Geoms[i].Faces[j].SmoothGroup;
          NormalGroups[g].Normal := n;
        end;
        if r = -1 then
        begin
          if Length(GroupVertices) <= GroupVertexCount then
          begin
            if Length(GroupVertices) < 16 then
            SetLength(GroupVertices, 16)
            else
            SetLength(GroupVertices, Length(GroupVertices) * 2);
          end;
          r := GroupVertexCount;
          Inc(GroupVertexCount);
          GroupVertices[r].PosID := m_MeshData.Geoms[i].Faces[j].Vertices[k];
          GroupVertices[r].ColID := m_MeshData.Geoms[i].Faces[j].Colors[k];
          SetLength(GroupVertices[r].TexIDs, m_MeshData.Geoms[i].TCount);
          for m := 0 to m_MeshData.Geoms[i].TCount - 1 do
          GroupVertices[r].TexIDs[m] := m_MeshData.Geoms[i].Faces[j].TexChannels[m][k];
          GroupVertices[r].GroupID := m_MeshData.Geoms[i].Faces[j].SmoothGroup;
        end;
        GroupVertices[r].NormalID := g;
        GroupIndices[j * 3 + k] := r;
      end;
    end;
    for j := 0 to NormalGroupCount - 1 do
    NormalGroups[j].Normal.Normalize;
    Decl[0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
    Decl[1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
    Decl[2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
    Decl[3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
    Decl[4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
    Decl[5] := D3DVertexElement(0, 4 * 14, D3DDECLTYPE_D3DCOLOR, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0);
    Decl[6] := D3DDECL_END;
    D3DXCreateMesh(
      m_MeshData.Geoms[i].FCount,
      GroupVertexCount,
      D3DXMESH_SYSTEMMEM,
      @Decl[0],
      Device,
      Mesh
    );
    Mesh.LockVertexBuffer(0, Pointer(Vertices));
    for j := 0 to GroupVertexCount - 1 do
    begin
      Vertices^[j].Position := m_MeshData.Geoms[i].Vertices[GroupVertices[j].PosID];
      Vertices^[j].Tangent.SetValue(0, 0, 0);
      Vertices^[j].Binormal.SetValue(0, 0, 0);
      Vertices^[j].Normal := NormalGroups[GroupVertices[j].NormalID].Normal;
      if m_MeshData.Geoms[i].TCount > 0 then
      Vertices^[j].TexCoords := m_MeshData.Geoms[i].TexChannels[0].TexCoords[GroupVertices[j].TexIDs[0]]
      else
      Vertices^[j].TexCoords.SetValue(j / GroupVertexCount, j / GroupVertexCount);
      Vertices^[j].Color := m_MeshData.Geoms[i].Colors[GroupVertices[j].ColID];
    end;
    Mesh.UnlockVertexBuffer;
    Mesh.LockIndexBuffer(0, Ptr);
    Move(GroupIndices[0], Ptr^, m_MeshData.Geoms[i].FCount * 6);
    Mesh.UnlockIndexBuffer;
    SetLength(Adj, Mesh.GetNumFaces * 3);
    Mesh.GenerateAdjacency(G2EPS, @Adj[0]);
    if Succeeded(
      D3DXComputeTangentFrameEx(
        Mesh,
        DWord(D3DDECLUSAGE_TEXCOORD), 0,
        DWord(D3DDECLUSAGE_TANGENT), 0,
        DWord(D3DDECLUSAGE_BINORMAL), 0,
        DWord(D3DDECLUSAGE_NORMAL), 0,
        0,
        @Adj[0],
        0.01,
        0.25,
        0.01,
        TmpMesh,
        @VertexMapping
      )
    ) then
    begin
      SafeRelease(Mesh);
      Mesh := TmpMesh;
      SafeRelease(TmpMesh);
    end;
    SetLength(VertexRemaps[i], Mesh.GetNumVertices);
    Move(VertexMapping.GetBufferPointer^, VertexRemaps[i][0], Mesh.GetNumVertices * 4);
    SafeRelease(VertexMapping);
    MeshData^.Geoms[i].VCount := Mesh.GetNumVertices;
    Mesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(Vertices));
    SetLength(MeshData^.Geoms[i].Vertices, MeshData^.Geoms[i].VCount);
    for j := 0 to Mesh.GetNumVertices - 1 do
    begin
      MeshData^.Geoms[i].Vertices[j].Position := Vertices^[j].Position;
      MeshData^.Geoms[i].Vertices[j].Tangent := Vertices^[j].Tangent;
      MeshData^.Geoms[i].Vertices[j].Binormal := Vertices^[j].Binormal;
      MeshData^.Geoms[i].Vertices[j].Normal := Vertices^[j].Normal;
      SetLength(MeshData^.Geoms[i].Vertices[j].TexCoords, MeshData^.Geoms[i].TCount);
      for k := 0 to MeshData^.Geoms[i].TCount - 1 do
      MeshData^.Geoms[i].Vertices[j].TexCoords[k] := m_MeshData.Geoms[i].TexChannels[k].TexCoords[GroupVertices[VertexRemaps[i][j]].TexIDs[k]];
      MeshData^.Geoms[i].Vertices[j].Color := Vertices^[j].Color;
    end;
    Mesh.UnlockVertexBuffer;
    for j := 0 to Mesh.GetNumVertices - 1 do
    VertexRemaps[i][j] := GroupVertices[VertexRemaps[i][j]].PosID;
    Mesh.LockIndexBuffer(D3DLOCK_READONLY, Ptr);
    for j := 0 to MeshData^.Geoms[i].FCount - 1 do
    begin
      for k := 0 to 2 do
      MeshData^.Geoms[i].Faces[j].Indices[k] := PWordArray(Ptr)^[j * 3 + k];
    end;
    Mesh.UnlockIndexBuffer;
    SafeRelease(Mesh);
    Move(m_MeshData.Geoms[i].Materials[0], MeshData^.Geoms[i].Materials[0], m_MeshData.Geoms[i].MCount * 4);
  end;
  for i := 0 to MeshData^.SkinCount - 1 do
  begin
    MeshData^.Skins[i].GeomID := m_MeshData.Skins[i].GeomID;
    MeshData^.Skins[i].MaxWeights := 0;
    MeshData^.Geoms[MeshData^.Skins[i].GeomID].SkinID := i;
    MeshData^.Skins[i].BoneCount := m_MeshData.Skins[i].BoneCount;
    SetLength(MeshData^.Skins[i].Bones, MeshData^.Skins[i].BoneCount);
    for j := 0 to MeshData^.Skins[i].BoneCount - 1 do
    begin
      MeshData^.Skins[i].Bones[j].NodeID := m_MeshData.Skins[i].Bones[j].NodeID;
      MeshData^.Skins[i].Bones[j].Bind := m_MeshData.Skins[i].Bones[j].Bind;
    end;
    SetLength(MeshData^.Skins[i].Vertices, MeshData^.Geoms[MeshData^.Skins[i].GeomID].VCount);
    for j := 0 to MeshData^.Geoms[MeshData^.Skins[i].GeomID].VCount - 1 do
    begin
      MeshData^.Skins[i].Vertices[j].WeightCount := m_MeshData.Skins[i].Vertices[VertexRemaps[MeshData^.Skins[i].GeomID][j]].WeightCount;
      SetLength(MeshData^.Skins[i].Vertices[j].Weights, MeshData^.Skins[i].Vertices[j].WeightCount);
      MeshData^.Skins[i].MaxWeights := Max(MeshData^.Skins[i].MaxWeights, MeshData^.Skins[i].Vertices[j].WeightCount);
      for k := 0 to MeshData^.Skins[i].Vertices[j].WeightCount - 1 do
      begin
        MeshData^.Skins[i].Vertices[j].Weights[k].BoneID := m_MeshData.Skins[i].Vertices[VertexRemaps[MeshData^.Skins[i].GeomID][j]].Weights[k].BoneID;
        MeshData^.Skins[i].Vertices[j].Weights[k].Weight := m_MeshData.Skins[i].Vertices[VertexRemaps[MeshData^.Skins[i].GeomID][j]].Weights[k].Weight;
      end;
    end;
  end;
  for i := 0 to MeshData^.AnimCount - 1 do
  begin
    MeshData^.Anims[i].Name := m_MeshData.Anims[i].Name;
    MeshData^.Anims[i].FrameRate := m_MeshData.Anims[i].FrameRate;
    MeshData^.Anims[i].FrameCount := m_MeshData.Anims[i].FrameCount;
    MeshData^.Anims[i].NodeCount := m_MeshData.Anims[i].AnimNodeCount;
    SetLength(MeshData^.Anims[i].Nodes, MeshData^.Anims[i].NodeCount);
    for j := 0 to MeshData^.Anims[i].NodeCount - 1 do
    begin
      MeshData^.Anims[i].Nodes[j].NodeID := m_MeshData.Anims[i].AnimNodes[j].NodeID;
      SetLength(MeshData^.Anims[i].Nodes[j].Frames, MeshData^.Anims[i].FrameCount);
      for k := 0 to MeshData^.Anims[i].FrameCount - 1 do
      m_MeshData.Anims[i].AnimNodes[j].Frames[k].Decompose(
        @MeshData^.Anims[i].Nodes[j].Frames[k].Scaling,
        @MeshData^.Anims[i].Nodes[j].Frames[k].Rotation,
        @MeshData^.Anims[i].Nodes[j].Frames[k].Translation
      );
    end;
  end;
  for i := 0 to MeshData^.MaterialCount - 1 do
  begin
    MeshData^.Materials[i].ChannelCount := m_MeshData.Materials[i].ChannelCount;
    SetLength(MeshData^.Materials[i].Channels, MeshData^.Materials[i].ChannelCount);
    for j := 0 to MeshData^.Materials[i].ChannelCount - 1 do
    begin
      MeshData^.Materials[i].Channels[j].Name := m_MeshData.Materials[i].Channels[j].Name;
      MeshData^.Materials[i].Channels[j].TwoSided := m_MeshData.Materials[i].Channels[j].TwoSided;
      MeshData^.Materials[i].Channels[j].AmbientColor := m_MeshData.Materials[i].Channels[j].AmbientColor;
      MeshData^.Materials[i].Channels[j].DiffuseColor := m_MeshData.Materials[i].Channels[j].DiffuseColor;
      MeshData^.Materials[i].Channels[j].SpecularColor := m_MeshData.Materials[i].Channels[j].SpecularColor;
      MeshData^.Materials[i].Channels[j].SpecularColorAmount := m_MeshData.Materials[i].Channels[j].SpecularColorAmount;
      MeshData^.Materials[i].Channels[j].SpecularPower := m_MeshData.Materials[i].Channels[j].SpecularPower;
      MeshData^.Materials[i].Channels[j].EmmissiveColor := m_MeshData.Materials[i].Channels[j].EmmissiveColor;
      MeshData^.Materials[i].Channels[j].EmmissiveColorAmount := m_MeshData.Materials[i].Channels[j].EmmissiveColorAmount;
      MeshData^.Materials[i].Channels[j].AmbientMapEnable := m_MeshData.Materials[i].Channels[j].AmbientMapEnable;
      MeshData^.Materials[i].Channels[j].AmbientMap := m_MeshData.Materials[i].Channels[j].AmbientMap;
      MeshData^.Materials[i].Channels[j].AmbientMapAmount := m_MeshData.Materials[i].Channels[j].AmbientMapAmount;
      MeshData^.Materials[i].Channels[j].DiffuseMapEnable := m_MeshData.Materials[i].Channels[j].DiffuseMapEnable;
      MeshData^.Materials[i].Channels[j].DiffuseMap := m_MeshData.Materials[i].Channels[j].DiffuseMap;
      MeshData^.Materials[i].Channels[j].DiffuseMapAmount := m_MeshData.Materials[i].Channels[j].DiffuseMapAmount;
      MeshData^.Materials[i].Channels[j].SpecularMapEnable := m_MeshData.Materials[i].Channels[j].SpecularMapEnable;
      MeshData^.Materials[i].Channels[j].SpecularMap := m_MeshData.Materials[i].Channels[j].SpecularMap;
      MeshData^.Materials[i].Channels[j].SpecularMapAmount := m_MeshData.Materials[i].Channels[j].SpecularMapAmount;
      MeshData^.Materials[i].Channels[j].OpacityMapEnable := m_MeshData.Materials[i].Channels[j].OpacityMapEnable;
      MeshData^.Materials[i].Channels[j].OpacityMap := m_MeshData.Materials[i].Channels[j].OpacityMap;
      MeshData^.Materials[i].Channels[j].OpacityMapAmount := m_MeshData.Materials[i].Channels[j].OpacityMapAmount;
      MeshData^.Materials[i].Channels[j].LightMapEnable := m_MeshData.Materials[i].Channels[j].IlluminationMapEnable;
      MeshData^.Materials[i].Channels[j].LightMap := m_MeshData.Materials[i].Channels[j].IlluminationMap;
      MeshData^.Materials[i].Channels[j].LightMapAmount := m_MeshData.Materials[i].Channels[j].IlluminationMapAmount;
      MeshData^.Materials[i].Channels[j].NormalMapEnable := m_MeshData.Materials[i].Channels[j].BumpMapEnable;
      MeshData^.Materials[i].Channels[j].NormalMap := m_MeshData.Materials[i].Channels[j].BumpMap;
      MeshData^.Materials[i].Channels[j].NormalMapAmount := m_MeshData.Materials[i].Channels[j].BumpMapAmount;
    end;
  end;
  for i := 0 to MeshData^.RagDollCount - 1 do
  begin
    MeshData^.RagDolls[i].NodeID := m_MeshData.RagDolls[i].NodeID;
    MeshData^.RagDolls[i].BodyNodeCount := m_MeshData.RagDolls[i].BodyNodeCount;
    MeshData^.RagDolls[i].ArmRNodeCount := m_MeshData.RagDolls[i].ArmRNodeCount;
    MeshData^.RagDolls[i].ArmLNodeCount := m_MeshData.RagDolls[i].ArmLNodeCount;
    MeshData^.RagDolls[i].LegRNodeCount := m_MeshData.RagDolls[i].LegRNodeCount;
    MeshData^.RagDolls[i].LegLNodeCount := m_MeshData.RagDolls[i].LegLNodeCount;
    SetLength(MeshData^.RagDolls[i].BodyNodes, MeshData^.RagDolls[i].BodyNodeCount);
    SetLength(MeshData^.RagDolls[i].ArmRNodes, MeshData^.RagDolls[i].ArmRNodeCount);
    SetLength(MeshData^.RagDolls[i].ArmLNodes, MeshData^.RagDolls[i].ArmLNodeCount);
    SetLength(MeshData^.RagDolls[i].LegRNodes, MeshData^.RagDolls[i].LegRNodeCount);
    SetLength(MeshData^.RagDolls[i].LegLNodes, MeshData^.RagDolls[i].LegLNodeCount);
    CopyRagdollObject(@m_MeshData.RagDolls[i].Head, @MeshData^.RagDolls[i].Head);
    CopyRagdollObject(@m_MeshData.RagDolls[i].Neck, @MeshData^.RagDolls[i].Neck);
    CopyRagdollObject(@m_MeshData.RagDolls[i].Pelvis, @MeshData^.RagDolls[i].Pelvis);
    for j := 0 to MeshData^.RagDolls[i].BodyNodeCount - 1 do
    CopyRagdollObject(@m_MeshData.RagDolls[i].BodyNodes[j], @MeshData^.RagDolls[i].BodyNodes[j]);
    for j := 0 to MeshData^.RagDolls[i].ArmRNodeCount - 1 do
    CopyRagdollObject(@m_MeshData.RagDolls[i].ArmRNodes[j], @MeshData^.RagDolls[i].ArmRNodes[j]);
    for j := 0 to MeshData^.RagDolls[i].ArmLNodeCount - 1 do
    CopyRagdollObject(@m_MeshData.RagDolls[i].ArmLNodes[j], @MeshData^.RagDolls[i].ArmLNodes[j]);
    for j := 0 to MeshData^.RagDolls[i].LegRNodeCount - 1 do
    CopyRagdollObject(@m_MeshData.RagDolls[i].LegRNodes[j], @MeshData^.RagDolls[i].LegRNodes[j]);
    for j := 0 to MeshData^.RagDolls[i].LegLNodeCount - 1 do
    CopyRagdollObject(@m_MeshData.RagDolls[i].LegLNodes[j], @MeshData^.RagDolls[i].LegLNodes[j]);
  end;
  G2MeshDataLimitSkin(MeshData, 4);
end;
//TG2MeshLoaderG2M END

initialization
  G2RegMeshLoader(TG2MeshLoaderG2M);

end.