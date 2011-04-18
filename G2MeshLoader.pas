//G2MeshLoader v1.0
//Author: Dan
unit G2MeshLoader;

interface

uses
  Types,
  SysUtils,
  Classes,
  G2Math,
  Direct3D9;

type
  TG2MeshLoader = class;

  CG2MeshLoaderClass = class of TG2MeshLoader;

//TG2MeshData BEGIN
  TG2RagdollObject = packed record
  public
    var NodeID: Integer;
    var MinV, MaxV: TG2Vec3;
    var Transform: TG2Mat;
    var DependantCount: Integer;
    var Dependants: array of record
    public
      var NodeID: Integer;
      var Offset: TG2Mat;
    end;
  end;
  PG2RagdollObject = ^TG2RagdollObject;

  PG2MeshData = ^TG2MeshData;
  TG2MeshData = packed record
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
      SkinID: Integer;
      VCount: Integer;
      FCount: Integer;
      TCount: Integer;
      MCount: Integer;
      Vertices: array of packed record
      public
        Position: TG2Vec3;
        Tangent: TG2Vec3;
        Binormal: TG2Vec3;
        Normal: TG2Vec3;
        TexCoords: array of TG2Vec2;
        Color: DWord;
      end;
      Faces: array of packed record
      public
        Indices: array[0..2] of Word;
        MaterialID: Integer;
      end;
      Materials: array of Integer;
    end;
    SkinCount: Integer;
    Skins: array of packed record
    public
      GeomID: Integer;
      MaxWeights: Integer;
      BoneCount: Integer;
      Bones: array of packed record
        NodeID: Integer;
        Bind: TG2Mat;
      end;
      Vertices: array of packed record
        WeightCount: Integer;
        Weights: array of packed record
          BoneID: Integer;
          Weight: Single;
        end;
      end;
    end;
    AnimCount: Integer;
    Anims: array of packed record
    public
      Name: AnsiString;
      FrameRate: Integer;
      FrameCount: Integer;
      NodeCount: Integer;
      Nodes: array of record
        NodeID: Integer;
        Frames: array of record
          Scaling: TG2Vec3;
          Rotation: TG2Quat;
          Translation: TG2Vec3;
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
        AmbientColor: DWord;
        DiffuseColor: DWord;
        SpecularColor: DWord;
        SpecularColorAmount: Single;
        SpecularPower: Single;
        EmmissiveColor: DWord;
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
        LightMapEnable: Boolean;
        LightMap: AnsiString;
        LightMapAmount: Single;
        NormalMapEnable: Boolean;
        NormalMap: AnsiString;
        NormalMapAmount: Single;
      end;
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
//TG2MeshData END

//TG2MeshLoader BEGIN
  TG2MeshLoader = class
  strict protected
  public
    constructor Create;
    destructor Destroy; override;
    class function GetDifinition: AnsiString; virtual;
    class function CanLoadFile(const f: WideString): Boolean; virtual;
    class function CanLoadStream(const s: TStream): Boolean; virtual;
    procedure LoadFile(const f: WideString); virtual;
    procedure LoadStream(const s: TStream); virtual;
    procedure ExportMesh(const Device: IDirect3DDevice9; const MeshData: PG2MeshData); virtual;
  end;
//TG2MeshLoader END

var
  G2MeshLoaders: array of CG2MeshLoaderClass;

procedure G2RegMeshLoader(const LoaderClass: CG2MeshLoaderClass);
procedure G2MeshDataClear(const MeshData: PG2MeshData);
procedure G2MeshDataCopy(const Src, Dst: PG2MeshData);
procedure G2MeshDataLimitSkin(const MeshData: PG2MeshData; const MaxWeights: Integer = 4);

implementation

uses
  Gen2;

//TG2MeshLoader BEGIN
constructor TG2MeshLoader.Create;
begin
  inherited Create;
end;

destructor TG2MeshLoader.Destroy;
begin
  inherited Destroy;
end;

class function TG2MeshLoader.GetDifinition: AnsiString;
begin
  Result := 'Undifined.';
end;

class function TG2MeshLoader.CanLoadFile(const f: WideString): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(f, fmOpenRead);
  Result := CanLoadStream(fs);
  fs.Free;
end;

class function TG2MeshLoader.CanLoadStream(const s: TStream): Boolean;
begin
  Result := False;
end;

procedure TG2MeshLoader.LoadFile(const f: WideString);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(f, fmOpenRead);
  LoadStream(fs);
  fs.Free;
end;

procedure TG2MeshLoader.LoadStream(const s: TStream);
begin

end;

procedure TG2MeshLoader.ExportMesh(const Device: IDirect3DDevice9; const MeshData: PG2MeshData);
begin
  G2MeshDataClear(MeshData);
end;
//TG2MeshLoader END

procedure G2RegMeshLoader(const LoaderClass: CG2MeshLoaderClass);
begin
  SetLength(G2MeshLoaders, Length(G2MeshLoaders) + 1);
  G2MeshLoaders[High(G2MeshLoaders)] := LoaderClass;
  G2WriteLogTimed('Mesh Loader Registered (' + LoaderClass.GetDifinition + ')', 'Globals');
end;

procedure G2MeshDataClear(const MeshData: PG2MeshData);
begin
  MeshData^.NodeCount := 0;
  MeshData^.Nodes := nil;
  MeshData^.GeomCount := 0;
  MeshData^.Geoms := nil;
  MeshData^.SkinCount := 0;
  MeshData^.Skins := nil;
  MeshData^.AnimCount := 0;
  MeshData^.Anims := nil;
  MeshData^.MaterialCount := 0;
  MeshData^.Materials := nil;
  MeshData^.RagDollCount := 0;
  MeshData^.RagDolls := nil;
end;

procedure G2MeshDataCopy(const Src, Dst: PG2MeshData);
  procedure CopyRagdollObject(const SrcObj: TG2RagdollObject; var DstObj: TG2RagdollObject);
    var i: Integer;
  begin
    DstObj.NodeID := SrcObj.NodeID;
    DstObj.MinV := SrcObj.MinV;
    DstObj.MaxV := SrcObj.MaxV;
    DstObj.Transform := SrcObj.Transform;
    DstObj.DependantCount := SrcObj.DependantCount;
    SetLength(DstObj.Dependants, DstObj.DependantCount);
    for i := 0 to DstObj.DependantCount - 1 do
    begin
      DstObj.Dependants[i].NodeID := SrcObj.Dependants[i].NodeID;
      DstObj.Dependants[i].Offset := SrcObj.Dependants[i].Offset;
    end;
  end;
  var i, j, k: Integer;
begin
  Dst^.NodeCount := Src^.NodeCount;
  SetLength(Dst^.Nodes, Dst^.NodeCount);
  for i := 0 to Dst^.NodeCount - 1 do
  begin
    Dst^.Nodes[i].OwnerID := Src^.Nodes[i].OwnerID;
    Dst^.Nodes[i].Name := Src^.Nodes[i].Name;
    Dst^.Nodes[i].Transform := Src^.Nodes[i].Transform;
  end;
  Dst^.GeomCount := Src^.GeomCount;
  SetLength(Dst^.Geoms, Src^.GeomCount);
  for i := 0 to Dst^.GeomCount - 1 do
  begin
    Dst^.Geoms[i].NodeID := Src^.Geoms[i].NodeID;
    Dst^.Geoms[i].SkinID := Src^.Geoms[i].SkinID;
    Dst^.Geoms[i].VCount := Src^.Geoms[i].VCount;
    Dst^.Geoms[i].FCount := Src^.Geoms[i].FCount;
    Dst^.Geoms[i].TCount := Src^.Geoms[i].TCount;
    Dst^.Geoms[i].MCount := Src^.Geoms[i].MCount;
    SetLength(Dst^.Geoms[i].Vertices, Dst^.Geoms[i].VCount);
    for j := 0 to Dst^.Geoms[i].VCount - 1 do
    begin
      Dst^.Geoms[i].Vertices[j].Position := Src^.Geoms[i].Vertices[j].Position;
      Dst^.Geoms[i].Vertices[j].Tangent := Src^.Geoms[i].Vertices[j].Tangent;
      Dst^.Geoms[i].Vertices[j].Binormal := Src^.Geoms[i].Vertices[j].Binormal;
      Dst^.Geoms[i].Vertices[j].Normal := Src^.Geoms[i].Vertices[j].Normal;
      SetLength(Dst^.Geoms[i].Vertices[j].TexCoords, Dst^.Geoms[i].TCount);
      for k := 0 to Dst^.Geoms[i].TCount - 1 do
      Dst^.Geoms[i].Vertices[j].TexCoords[k] := Src^.Geoms[i].Vertices[j].TexCoords[k];
    end;
    SetLength(Dst^.Geoms[i].Faces, Dst^.Geoms[i].FCount);
    for j := 0 to Dst^.Geoms[i].FCount - 1 do
    begin
      for k := 0 to 2 do
      Dst^.Geoms[i].Faces[j].Indices[k] := Src^.Geoms[i].Faces[j].Indices[k];
      Dst^.Geoms[i].Faces[j].MaterialID := Src^.Geoms[i].Faces[j].MaterialID;
    end;
    SetLength(Dst^.Geoms[i].Materials, Dst^.Geoms[i].MCount);
    for j := 0 to Dst^.Geoms[i].MCount - 1 do
    Dst^.Geoms[i].Materials[j] := Src^.Geoms[i].Materials[j];
  end;
  Dst^.SkinCount := Src^.SkinCount;
  SetLength(Dst^.Skins, Dst^.SkinCount);
  for i := 0 to Dst^.SkinCount - 1 do
  begin
    Dst^.Skins[i].GeomID := Src^.Skins[i].GeomID;
    Dst^.Skins[i].BoneCount := Src^.Skins[i].BoneCount;
    SetLength(Dst^.Skins[i].Bones, Dst^.Skins[i].BoneCount);
    for j := 0 to Dst^.Skins[i].BoneCount - 1 do
    begin
      Dst^.Skins[i].Bones[j].NodeID := Src^.Skins[i].Bones[j].NodeID;
      Dst^.Skins[i].Bones[j].Bind := Src^.Skins[i].Bones[j].Bind;
    end;
    SetLength(Dst^.Skins[i].Vertices, Dst^.Geoms[Dst^.Skins[i].GeomID].VCount);
    for j := 0 to Dst^.Geoms[Dst^.Skins[i].GeomID].VCount - 1 do
    begin
      Dst^.Skins[i].Vertices[j].WeightCount := Src^.Skins[i].Vertices[j].WeightCount;
      SetLength(Dst^.Skins[i].Vertices[j].Weights, Dst^.Skins[i].Vertices[j].WeightCount);
      for k := 0 to Dst^.Skins[i].Vertices[j].WeightCount - 1 do
      begin
        Dst^.Skins[i].Vertices[j].Weights[k].BoneID := Src^.Skins[i].Vertices[j].Weights[k].BoneID;
        Dst^.Skins[i].Vertices[j].Weights[k].Weight := Src^.Skins[i].Vertices[j].Weights[k].Weight;
      end;
    end;
  end;
  Dst^.AnimCount := Src^.AnimCount;
  SetLength(Dst^.Anims, Dst^.AnimCount);
  for i := 0 to Dst^.AnimCount - 1 do
  begin
    Dst^.Anims[i].Name := Src^.Anims[i].Name;
    Dst^.Anims[i].FrameRate := Src^.Anims[i].FrameRate;
    Dst^.Anims[i].FrameCount := Src^.Anims[i].FrameCount;
    Dst^.Anims[i].NodeCount := Src^.Anims[i].NodeCount;
    SetLength(Dst^.Anims[i].Nodes, Dst^.Anims[i].NodeCount);
    for j := 0 to Dst^.Anims[i].NodeCount - 1 do
    begin
      Dst^.Anims[i].Nodes[j].NodeID := Src^.Anims[i].Nodes[j].NodeID;
      SetLength(Dst^.Anims[i].Nodes[j].Frames, Dst^.Anims[i].FrameCount);
      for k := 0 to Dst^.Anims[i].FrameCount - 1 do
      begin
        Dst^.Anims[i].Nodes[j].Frames[k].Scaling := Src^.Anims[i].Nodes[j].Frames[k].Scaling;
        Dst^.Anims[i].Nodes[j].Frames[k].Rotation := Src^.Anims[i].Nodes[j].Frames[k].Rotation;
        Dst^.Anims[i].Nodes[j].Frames[k].Translation := Src^.Anims[i].Nodes[j].Frames[k].Translation;
      end;
    end;
  end;
  Dst^.MaterialCount := Src^.MaterialCount;
  SetLength(Dst^.Materials, Dst^.MaterialCount);
  for i := 0 to Dst^.MaterialCount - 1 do
  begin
    Dst^.Materials[i].ChannelCount := Src^.Materials[i].ChannelCount;
    SetLength(Dst^.Materials[i].Channels, Dst^.Materials[i].ChannelCount);
    for j := 0 to Dst^.Materials[i].ChannelCount - 1 do
    begin
      Dst^.Materials[i].Channels[j].Name := Src^.Materials[i].Channels[j].Name;
      Dst^.Materials[i].Channels[j].TwoSided := Src^.Materials[i].Channels[j].TwoSided;
      Dst^.Materials[i].Channels[j].AmbientColor := Src^.Materials[i].Channels[j].AmbientColor;
      Dst^.Materials[i].Channels[j].DiffuseColor := Src^.Materials[i].Channels[j].DiffuseColor;
      Dst^.Materials[i].Channels[j].SpecularColor := Src^.Materials[i].Channels[j].SpecularColor;
      Dst^.Materials[i].Channels[j].SpecularColorAmount := Src^.Materials[i].Channels[j].SpecularColorAmount;
      Dst^.Materials[i].Channels[j].SpecularPower := Src^.Materials[i].Channels[j].SpecularPower;
      Dst^.Materials[i].Channels[j].EmmissiveColor := Src^.Materials[i].Channels[j].EmmissiveColor;
      Dst^.Materials[i].Channels[j].EmmissiveColorAmount := Src^.Materials[i].Channels[j].EmmissiveColorAmount;
      Dst^.Materials[i].Channels[j].AmbientMapEnable := Src^.Materials[i].Channels[j].AmbientMapEnable;
      Dst^.Materials[i].Channels[j].AmbientMap := Src^.Materials[i].Channels[j].AmbientMap;
      Dst^.Materials[i].Channels[j].AmbientMapAmount := Src^.Materials[i].Channels[j].AmbientMapAmount;
      Dst^.Materials[i].Channels[j].DiffuseMapEnable := Src^.Materials[i].Channels[j].DiffuseMapEnable;
      Dst^.Materials[i].Channels[j].DiffuseMap := Src^.Materials[i].Channels[j].DiffuseMap;
      Dst^.Materials[i].Channels[j].DiffuseMapAmount := Src^.Materials[i].Channels[j].DiffuseMapAmount;
      Dst^.Materials[i].Channels[j].SpecularMapEnable := Src^.Materials[i].Channels[j].SpecularMapEnable;
      Dst^.Materials[i].Channels[j].SpecularMap := Src^.Materials[i].Channels[j].SpecularMap;
      Dst^.Materials[i].Channels[j].SpecularMapAmount := Src^.Materials[i].Channels[j].SpecularMapAmount;
      Dst^.Materials[i].Channels[j].OpacityMapEnable := Src^.Materials[i].Channels[j].OpacityMapEnable;
      Dst^.Materials[i].Channels[j].OpacityMap := Src^.Materials[i].Channels[j].OpacityMap;
      Dst^.Materials[i].Channels[j].OpacityMapAmount := Src^.Materials[i].Channels[j].OpacityMapAmount;
      Dst^.Materials[i].Channels[j].LightMapEnable := Src^.Materials[i].Channels[j].LightMapEnable;
      Dst^.Materials[i].Channels[j].LightMap := Src^.Materials[i].Channels[j].LightMap;
      Dst^.Materials[i].Channels[j].LightMapAmount := Src^.Materials[i].Channels[j].LightMapAmount;
      Dst^.Materials[i].Channels[j].NormalMapEnable := Src^.Materials[i].Channels[j].NormalMapEnable;
      Dst^.Materials[i].Channels[j].NormalMap := Src^.Materials[i].Channels[j].NormalMap;
      Dst^.Materials[i].Channels[j].NormalMapAmount := Src^.Materials[i].Channels[j].NormalMapAmount;
    end;
  end;
  Dst^.RagDollCount := Src^.RagDollCount;
  SetLength(Dst^.RagDolls, Dst^.RagDollCount);
  for i := 0 to Dst^.RagDollCount - 1 do
  begin
    Dst^.RagDolls[i].NodeID := Src^.RagDolls[i].NodeID;
    CopyRagdollObject(Src^.RagDolls[i].Head, Dst^.RagDolls[i].Head);
    CopyRagdollObject(Src^.RagDolls[i].Neck, Dst^.RagDolls[i].Neck);
    CopyRagdollObject(Src^.RagDolls[i].Pelvis, Dst^.RagDolls[i].Pelvis);
    Dst^.RagDolls[i].BodyNodeCount := Src^.RagDolls[i].BodyNodeCount;
    SetLength(Dst^.RagDolls[i].BodyNodes, Dst^.RagDolls[i].BodyNodeCount);
    for j := 0 to Dst^.RagDolls[i].BodyNodeCount - 1 do
    CopyRagdollObject(Src^.RagDolls[i].BodyNodes[j], Dst^.RagDolls[i].BodyNodes[j]);
    Dst^.RagDolls[i].ArmRNodeCount := Src^.RagDolls[i].ArmRNodeCount;
    SetLength(Dst^.RagDolls[i].ArmRNodes, Dst^.RagDolls[i].ArmRNodeCount);
    for j := 0 to Dst^.RagDolls[i].ArmRNodeCount - 1 do
    CopyRagdollObject(Src^.RagDolls[i].ArmRNodes[j], Dst^.RagDolls[i].ArmRNodes[j]);
    Dst^.RagDolls[i].ArmLNodeCount := Src^.RagDolls[i].ArmLNodeCount;
    SetLength(Dst^.RagDolls[i].ArmLNodes, Dst^.RagDolls[i].ArmLNodeCount);
    for j := 0 to Dst^.RagDolls[i].ArmLNodeCount - 1 do
    CopyRagdollObject(Src^.RagDolls[i].ArmLNodes[j], Dst^.RagDolls[i].ArmLNodes[j]);
    Dst^.RagDolls[i].LegRNodeCount := Src^.RagDolls[i].LegRNodeCount;
    SetLength(Dst^.RagDolls[i].LegRNodes, Dst^.RagDolls[i].LegRNodeCount);
    for j := 0 to Dst^.RagDolls[i].LegRNodeCount - 1 do
    CopyRagdollObject(Src^.RagDolls[i].LegRNodes[j], Dst^.RagDolls[i].LegRNodes[j]);
    Dst^.RagDolls[i].LegLNodeCount := Src^.RagDolls[i].LegLNodeCount;
    SetLength(Dst^.RagDolls[i].LegLNodes, Dst^.RagDolls[i].LegLNodeCount);
    for j := 0 to Dst^.RagDolls[i].LegLNodeCount - 1 do
    CopyRagdollObject(Src^.RagDolls[i].LegLNodes[j], Dst^.RagDolls[i].LegLNodes[j]);
  end;
end;

procedure G2MeshDataLimitSkin(const MeshData: PG2MeshData; const MaxWeights: Integer = 4);
var
  s, v, w, n, i, CurWeights: Integer;
  TotalWeight: Single;
  WeightsRemap: array of record
  public
    Weight: Single;
    Index: Integer;
  end;
begin
  for s := 0 to MeshData^.SkinCount - 1 do
  for v := 0 to High(MeshData^.Skins[s].Vertices) do
  if MeshData^.Skins[s].Vertices[v].WeightCount > MaxWeights then
  begin
    if MeshData^.Skins[s].Vertices[v].WeightCount > Length(WeightsRemap) then
    SetLength(WeightsRemap, MeshData^.Skins[s].Vertices[v].WeightCount);
    CurWeights := 0;
    for w := 0 to MeshData^.Skins[s].Vertices[v].WeightCount - 1 do
    begin
      n := CurWeights;
      for i := 0 to CurWeights - 1 do
      if WeightsRemap[i].Weight < MeshData^.Skins[s].Vertices[v].Weights[w].Weight then
      begin
        n := i;
        Move(WeightsRemap[i], WeightsRemap[i + 1], 8 * (CurWeights - i));
        Break;
      end;
      WeightsRemap[n].Weight := MeshData^.Skins[s].Vertices[v].Weights[w].Weight;
      WeightsRemap[n].Index := MeshData^.Skins[s].Vertices[v].Weights[w].BoneID;
      Inc(CurWeights);
    end;
    TotalWeight := WeightsRemap[0].Weight;
    for w := 1 to MaxWeights - 1 do
    TotalWeight := TotalWeight + WeightsRemap[w].Weight;
    TotalWeight := 1 / TotalWeight;
    SetLength(MeshData^.Skins[s].Vertices[v].Weights, MaxWeights);
    for w := 0 to MaxWeights - 1 do
    begin
      MeshData^.Skins[s].Vertices[v].Weights[w].BoneID := WeightsRemap[w].Index;
      MeshData^.Skins[s].Vertices[v].Weights[w].Weight := WeightsRemap[w].Weight * TotalWeight;
    end;
    MeshData^.Skins[s].Vertices[v].WeightCount := MaxWeights;
  end;
end;

end.
