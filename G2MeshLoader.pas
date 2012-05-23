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
procedure G2MeshDataGenerateMapping(const MeshData: PG2MeshData);
procedure G2MeshRagdollObjectCopy(const Src, Dst: PG2RagdollObject);

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
  s, v, w, n, i, j, CurWeights: Integer;
  TotalWeight: Single;
  WeightsRemap: array of record
  public
    Weight: Single;
    Index: Integer;
  end;
begin
  for s := 0 to MeshData^.SkinCount - 1 do
  begin
    for v := 0 to High(MeshData^.Skins[s].Vertices) do
    begin
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
      n := MeshData^.Skins[s].Vertices[v].WeightCount;
      w := 0;
      while w < n do
      begin
        if MeshData^.Skins[s].Vertices[v].Weights[w].Weight < G2EPS then
        begin
          for j := w to n - 2 do
          begin
            MeshData^.Skins[s].Vertices[v].Weights[j].Weight := MeshData^.Skins[s].Vertices[v].Weights[j + 1].Weight;
            MeshData^.Skins[s].Vertices[v].Weights[j].BoneID := MeshData^.Skins[s].Vertices[v].Weights[j + 1].BoneID;
          end;
          Dec(n);
        end
        else
        Inc(w);
      end;
      if n < MeshData^.Skins[s].Vertices[v].WeightCount then
      begin
        SetLength(MeshData^.Skins[s].Vertices[v].Weights, n);
        MeshData^.Skins[s].Vertices[v].WeightCount := n;
      end;
    end;
    if MeshData^.Skins[s].MaxWeights > MaxWeights then
    MeshData^.Skins[s].MaxWeights := MaxWeights;
  end;
end;

procedure G2MeshDataGenerateMapping(const MeshData: PG2MeshData);
  type TArrV3 = array of TG2Vec3;
  type TArrV2 = array of TG2Vec2;
  type TArrCol = array of TG2Color;
  type TGroupMap = record
  public
    var GroupsSorted: TG2QuickSortList;
    var GroupsAdded: TG2QuickList;
    var Bounds: TG2Vec2;
  end;
  type TFaceMapGroup = record
  public
    var Normal, U, V: TG2Vec3;
    var Faces: TG2QuickList;
    var MinV, MaxV: TG2Vec2;
  end;
  type PFaceMapGroup = ^TFaceMapGroup;
  var Faces: array of record
  public
    var Position: array[0..2] of DWord;
    var Tangent: array[0..2] of DWord;
    var Binormal: array[0..2] of DWord;
    var Normal: array[0..2] of DWord;
    var TexCoord: array[0..2] of DWord;
    var Color: array[0..2] of DWord;
    var Checked: Boolean;
    var TmpTexCoords: array[0..2] of TG2Vec2;
    var Vertices: array[0..2] of DWord;
  end;
  var Vertices: array of record
  public
    var Position: DWord;
    var Tangent: DWord;
    var Binormal: DWord;
    var Normal: DWord;
    var TexCoord: DWord;
    var Color: DWord;
  end;
  var VertexCount: Integer;
  var Positions: TArrV3;
  var PositionCount: Integer;
  var Tangents: TArrV3;
  var TangentCount: Integer;
  var Binormals: TArrV3;
  var BinormalCount: Integer;
  var Normals: TArrV3;
  var NormalCount: Integer;
  var TexCoords: TArrV2;
  var TexCoordCount: Integer;
  var Colors: TArrCol;
  var ColorCount: Integer;
  var Adj: array of DWord;
  function AddToArrV3(var Arr: TArrV3; var ArrLen: Integer; const v: TG2Vec3): DWord;
    var i, n: Integer;
  begin
    for i := 0 to (ArrLen - 1) do
    begin
      if (Arr[i] - v).Len <= 0.00001 then
      begin
        Result := i;
        Exit;
      end;
    end;
    if ArrLen >= Length(Arr) then
    SetLength(Arr, ArrLen + 32);
    Arr[ArrLen] := v;
    Result := ArrLen;
    Inc(ArrLen);
  end;
  function AddToArrV2(var Arr: TArrV2; var ArrLen: Integer; const v: TG2Vec2): DWord;
    var i, n: Integer;
  begin
    for i := 0 to (ArrLen - 1) do
    begin
      if (Arr[i] - v).Len <= 0.00001 then
      begin
        Result := i;
        Exit;
      end;
    end;
    if ArrLen >= Length(Arr) then
    SetLength(Arr, ArrLen + 32);
    Arr[ArrLen] := v;
    Result := ArrLen;
    Inc(ArrLen);
  end;
  function AddToArrCol(var Arr: TArrCol; var ArrLen: Integer; const c: TG2Color): DWord;
    var i, n: Integer;
  begin
    for i := 0 to (ArrLen - 1) do
    begin
      if Arr[i] = c then
      begin
        Result := i;
        Exit;
      end;
    end;
    if ArrLen >= Length(Arr) then
    SetLength(Arr, ArrLen + 32);
    Arr[ArrLen] := c;
    Result := ArrLen;
    Inc(ArrLen);
  end;
  procedure AddVertex(const GeomID, VertexID: Integer; var PositionID, TangentID, BinormalID, NormalID, TexCoordID, ColorID: DWord);
  begin
    PositionID := AddToArrV3(Positions, PositionCount, MeshData^.Geoms[GeomID].Vertices[VertexID].Position);
    TangentID := AddToArrV3(Tangents, TangentCount, MeshData^.Geoms[GeomID].Vertices[VertexID].Tangent);
    BinormalID := AddToArrV3(Binormals, BinormalCount, MeshData^.Geoms[GeomID].Vertices[VertexID].Binormal);
    NormalID := AddToArrV3(Normals, NormalCount, MeshData^.Geoms[GeomID].Vertices[VertexID].Normal);
    ColorID := AddToArrCol(Colors, ColorCount, MeshData^.Geoms[GeomID].Vertices[VertexID].Color);
    TexCoordID := $ffffffff;
  end;
  function NewVertex(const Position, Tangent, Binormal, Normal, TexCoord, Color: DWord): Integer;
    var i: Integer;
  begin
    for i := 0 to VertexCount - 1 do
    if (Vertices[i].Position = Position)
    and (Vertices[i].Tangent = Tangent)
    and (Vertices[i].Binormal = Binormal)
    and (Vertices[i].Normal = Normal)
    and (Vertices[i].TexCoord = TexCoord)
    and (Vertices[i].Color = Color) then
    begin
      Result := i;
      Exit;
    end;
    if VertexCount >= Length(Vertices) then
    SetLength(Vertices, VertexCount + 32);
    Vertices[VertexCount].Position := Position;
    Vertices[VertexCount].Tangent := Tangent;
    Vertices[VertexCount].Binormal := Binormal;
    Vertices[VertexCount].Normal := Normal;
    Vertices[VertexCount].TexCoord := TexCoord;
    Vertices[VertexCount].Color := Color;
    Result := VertexCount;
    Inc(VertexCount);
  end;
  procedure AddAdj(const Face0, Face1: DWord);
    var i: Integer;
  begin
    for i := 0 to 2 do
    if Adj[Face0 * 3 + i] = $ffffffff then
    begin
      Adj[Face0 * 3 + i] := Face1;
      Break;
    end;
    for i := 0 to 2 do
    if Adj[Face1 * 3 + i] = $ffffffff then
    begin
      Adj[Face1 * 3 + i] := Face0;
      Break;
    end;
  end;
  function CheckAdj(const Face0, Face1: Integer): Boolean;
    function CheckEdges(const e0, e1: Integer): Boolean;
      var e0i0, e0i1, e1i0, e1i1: Integer;
    begin
      e0i0 := e0; e0i1 := (e0 + 1) mod 3;
      e1i0 := e1; e1i1 := (e1 + 1) mod 3;
      Result := ((Faces[Face0].Position[e0i0] = Faces[Face1].Position[e1i0]) and (Faces[Face0].Position[e0i1] = Faces[Face1].Position[e1i1]))
      or ((Faces[Face0].Position[e0i0] = Faces[Face1].Position[e1i1]) and (Faces[Face0].Position[e0i1] = Faces[Face1].Position[e1i0]));
    end;
    var i, j: Integer;
  begin
    for i := 0 to 2 do
    for j := 0 to 2 do
    if CheckEdges(i, j) then
    begin
      Result := True;
      Exit;
    end;
    Result := False;
  end;
  function GroupFaceMatch(const Group: PFaceMapGroup; const FaceID: DWord): Boolean;
    var i, j: Integer;
    var f: DWord;
    var SkipFace: Boolean;
  begin
    for i := 0 to Group^.Faces.Count - 1 do
    begin
      f := DWord(Group^.Faces[i]);
      SkipFace := False;
      for j := 0 to 2 do
      if Adj[FaceID * 3 + j] = f then
      begin
        SkipFace := True;
        Break;
      end;
      if not SkipFace
      and G2IntersectTri(
        Faces[FaceID].TmpTexCoords[0],
        Faces[FaceID].TmpTexCoords[1],
        Faces[FaceID].TmpTexCoords[2],
        Faces[f].TmpTexCoords[0],
        Faces[f].TmpTexCoords[1],
        Faces[f].TmpTexCoords[2]
      ) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
  var i, j, n, t, x, y, px, py: Integer;
  var StepSize: TG2Vec2;
  var FacesToCheck: TG2QuickList;
  var NewFaces: TG2QuickList;
  var FaceMapGroups: TG2QuickList;
  var CurFace: DWord;
  var CurMapGroup: PFaceMapGroup;
  var GroupMap: TGroupMap;
  var MapPlaced, b: Boolean;
  var PlaceVolume, PlaceVolumeMin: Single;
  var PlaceRect: TG2Rect;
  var TmpBounds: TG2Vec2;
  var MapScale: Single;
begin
  for i := 0 to MeshData^.GeomCount - 1 do
  begin
    PositionCount := 0;
    TangentCount := 0;
    BinormalCount := 0;
    NormalCount := 0;
    TexCoordCount := 0;
    ColorCount := 0;
    MeshData^.Geoms[i].TCount := 1;
    SetLength(Faces, MeshData^.Geoms[i].FCount);
    SetLength(Adj, MeshData^.Geoms[i].FCount * 3);
    FacesToCheck.Clear;
    for j := 0 to MeshData^.Geoms[i].FCount - 1 do
    begin
      for n := 0 to 2 do
      begin
        AddVertex(
          i,
          MeshData^.Geoms[i].Faces[j].Indices[n],
          Faces[j].Position[n],
          Faces[j].Tangent[n],
          Faces[j].Binormal[n],
          Faces[j].Normal[n],
          Faces[j].TexCoord[n],
          Faces[j].Color[n]
        );
      end;
      Adj[j * 3 + 0] := $ffffffff;
      Adj[j * 3 + 1] := $ffffffff;
      Adj[j * 3 + 2] := $ffffffff;
      FacesToCheck.Add(Pointer(j));
    end;
    if FacesToCheck.Count < 1 then Exit;
    for j := 0 to MeshData^.Geoms[i].FCount - 1 do
    for n := j + 1 to MeshData^.Geoms[i].FCount - 1 do
    if CheckAdj(j, n) then AddAdj(j, n);
    FaceMapGroups.Clear;
    while FacesToCheck.Count > 0 do
    begin
      NewFaces.Clear;
      for j := 0 to FacesToCheck.Count - 1 do
      Faces[DWord(FacesToCheck[j])].Checked := False;
      CurFace := DWord(FacesToCheck[0]);
      Faces[CurFace].Checked := True;
      New(CurMapGroup);
      FaceMapGroups.Add(CurMapGroup);
      CurMapGroup^.Normal := G2TriangleNormal(
        Positions[Faces[CurFace].Position[0]],
        Positions[Faces[CurFace].Position[1]],
        Positions[Faces[CurFace].Position[2]]
      );
      CurMapGroup^.U := G2RandomSpherePoint;
      while Abs(CurMapGroup^.Normal.Dot(CurMapGroup^.U)) > 0.95 do
      CurMapGroup^.U := G2RandomSpherePoint;
      CurMapGroup^.V := CurMapGroup^.Normal.Cross(CurMapGroup^.U).Normalized;
      CurMapGroup^.U := CurMapGroup^.V.Cross(CurMapGroup^.Normal).Normalized;
      CurMapGroup^.Faces.Clear;
      NewFaces.Add(FacesToCheck[0]);
      while NewFaces.Count > 0 do
      begin
        CurFace := DWord(NewFaces[0]);
        NewFaces.Delete(0);
        for t := 0 to 2 do
        begin
          Faces[CurFace].TmpTexCoords[t].x := CurMapGroup^.U.Dot(Positions[Faces[CurFace].Position[t]]);
          Faces[CurFace].TmpTexCoords[t].y := CurMapGroup^.V.Dot(Positions[Faces[CurFace].Position[t]]);
        end;
        //StretchFace(CurFace);
        if (
          G2TriangleNormal(
            Positions[Faces[CurFace].Position[0]],
            Positions[Faces[CurFace].Position[1]],
            Positions[Faces[CurFace].Position[2]]
          ).Dot(CurMapGroup^.Normal) > 0.3
        ) and GroupFaceMatch(CurMapGroup, CurFace) then
        begin
          FacesToCheck.Remove(Pointer(CurFace));
          CurMapGroup^.Faces.Add(Pointer(CurFace));
          for j := 0 to 2 do
          if (Adj[CurFace * 3 + j] <> $ffffffff)
          and (not Faces[Adj[CurFace * 3 + j]].Checked) then
          begin
            Faces[Adj[CurFace * 3 + j]].Checked := True;
            NewFaces.Add(Pointer(Adj[CurFace * 3 + j]));
          end;
        end;
      end;
    end;
    GroupMap.GroupsSorted.Clear;
    GroupMap.GroupsAdded.Clear;
    for j := 0 to FaceMapGroups.Count - 1 do
    begin
      CurMapGroup := PFaceMapGroup(FaceMapGroups[j]);
      for n := 0 to CurMapGroup^.Faces.Count - 1 do
      begin
        CurFace := DWord(CurMapGroup^.Faces[n]);
        for t := 0 to 2 do
        begin
          //Faces[CurFace].TmpTexCoords[t].x := CurMapGroup^.U.Dot(Positions[Faces[CurFace].Position[t]]);
          //Faces[CurFace].TmpTexCoords[t].y := CurMapGroup^.V.Dot(Positions[Faces[CurFace].Position[t]]);
          if (n = 0) and (t = 0) then
          begin
            CurMapGroup^.MinV := Faces[CurFace].TmpTexCoords[t];
            CurMapGroup^.MaxV := CurMapGroup^.MinV;
          end
          else
          begin
            if Faces[CurFace].TmpTexCoords[t].x < CurMapGroup^.MinV.x then CurMapGroup^.MinV.x := Faces[CurFace].TmpTexCoords[t].x;
            if Faces[CurFace].TmpTexCoords[t].y < CurMapGroup^.MinV.y then CurMapGroup^.MinV.y := Faces[CurFace].TmpTexCoords[t].y;
            if Faces[CurFace].TmpTexCoords[t].x > CurMapGroup^.MaxV.x then CurMapGroup^.MaxV.x := Faces[CurFace].TmpTexCoords[t].x;
            if Faces[CurFace].TmpTexCoords[t].y > CurMapGroup^.MaxV.y then CurMapGroup^.MaxV.y := Faces[CurFace].TmpTexCoords[t].y;
          end;
        end;
      end;
      for n := 0 to CurMapGroup^.Faces.Count - 1 do
      begin
        CurFace := DWord(CurMapGroup^.Faces[n]);
        for t := 0 to 2 do
        Faces[CurFace].TmpTexCoords[t] := Faces[CurFace].TmpTexCoords[t] - CurMapGroup^.MinV;
      end;
      CurMapGroup^.MaxV := CurMapGroup^.MaxV - CurMapGroup^.MinV;
      CurMapGroup^.MinV.SetValue(0, 0);
      GroupMap.GroupsSorted.Add(CurMapGroup, CurMapGroup^.MaxV.x * CurMapGroup^.MaxV.y);
    end;
    CurMapGroup := PFaceMapGroup(GroupMap.GroupsSorted[GroupMap.GroupsSorted.Count - 1]);
    GroupMap.GroupsSorted.Delete(GroupMap.GroupsSorted.Count - 1);
    GroupMap.GroupsAdded.Add(CurMapGroup);
    GroupMap.Bounds := CurMapGroup^.MaxV;
    while GroupMap.GroupsSorted.Count > 0 do
    begin
      CurMapGroup := PFaceMapGroup(GroupMap.GroupsSorted[GroupMap.GroupsSorted.Count - 1]);
      GroupMap.GroupsSorted.Delete(GroupMap.GroupsSorted.Count - 1);
      GroupMap.GroupsAdded.Add(CurMapGroup);
      StepSize := GroupMap.Bounds * 0.01;
      MapPlaced := False;
      PlaceVolumeMin := (GroupMap.Bounds.x * GroupMap.Bounds.y) * 3 + 10000;
      for y := 0 to 99 do
      for x := 0 to 99 do
      begin
        PlaceRect.Left := x * StepSize.x;
        PlaceRect.Top := y * StepSize.y;
        PlaceRect.Right := PlaceRect.Left + CurMapGroup^.MaxV.x;
        PlaceRect.Bottom := PlaceRect.Top + CurMapGroup^.MaxV.y;
        b := True;
        for j := 0 to GroupMap.GroupsAdded.Count - 1 do
        if G2RectVsRect(
          G2Rect(
            PFaceMapGroup(GroupMap.GroupsAdded[j])^.MinV,
            PFaceMapGroup(GroupMap.GroupsAdded[j])^.MaxV
          ),
          PlaceRect
        ) then
        begin
          b := False;
          Break;
        end;
        if b then
        begin
          TmpBounds := GroupMap.Bounds;
          if CurMapGroup^.MaxV.x > TmpBounds.x then TmpBounds.x := CurMapGroup^.MaxV.x;
          if CurMapGroup^.MaxV.y > TmpBounds.y then TmpBounds.y := CurMapGroup^.MaxV.y;
          PlaceVolume := TmpBounds.x * TmpBounds.y + x * y + Abs(x - y);
          if not MapPlaced or (PlaceVolume + 0.0001 < PlaceVolumeMin) then
          begin
            PlaceVolumeMin := PlaceVolume;
            px := x;
            py := y;
            MapPlaced := True;
          end;
        end;
      end;
      if MapPlaced then
      begin
        CurMapGroup^.MinV.x := px * StepSize.x;
        CurMapGroup^.MinV.y := py * StepSize.y;
        CurMapGroup^.MaxV := CurMapGroup^.MinV + CurMapGroup^.MaxV;
      end
      else
      begin
        if GroupMap.Bounds.x > GroupMap.Bounds.y then
        begin
          CurMapGroup^.MinV.x := 0;
          CurMapGroup^.MinV.y := GroupMap.Bounds.y;
          CurMapGroup^.MaxV := CurMapGroup^.MinV + CurMapGroup^.MaxV;
        end
        else
        begin
          CurMapGroup^.MinV.x := GroupMap.Bounds.x;
          CurMapGroup^.MinV.y := 0;
          CurMapGroup^.MaxV := CurMapGroup^.MinV + CurMapGroup^.MaxV;
        end;
      end;
      if CurMapGroup^.MaxV.x > GroupMap.Bounds.x then GroupMap.Bounds.x := CurMapGroup^.MaxV.x;
      if CurMapGroup^.MaxV.y > GroupMap.Bounds.y then GroupMap.Bounds.y := CurMapGroup^.MaxV.y;
    end;
    if GroupMap.Bounds.x > GroupMap.Bounds.y then
    MapScale := 1 / GroupMap.Bounds.x
    else
    MapScale := 1 / GroupMap.Bounds.y;
    for j := 0 to GroupMap.GroupsAdded.Count - 1 do
    begin
      CurMapGroup := PFaceMapGroup(GroupMap.GroupsAdded[j]);
      for n := 0 to CurMapGroup^.Faces.Count - 1 do
      begin
        CurFace := DWord(CurMapGroup^.Faces[n]);
        for t := 0 to 2 do
        begin
          Faces[CurFace].TmpTexCoords[t] := (Faces[CurFace].TmpTexCoords[t] + CurMapGroup^.MinV) * MapScale;
          Faces[CurFace].TexCoord[t] := AddToArrV2(TexCoords, TexCoordCount, Faces[CurFace].TmpTexCoords[t]);
        end;
      end;
      Dispose(CurMapGroup);
    end;
    VertexCount := 0;
    for j := 0 to MeshData.Geoms[i].FCount - 1 do
    begin
      for n := 0 to 2 do
      Faces[j].Vertices[n] := NewVertex(
        Faces[j].Position[n],
        Faces[j].Tangent[n],
        Faces[j].Binormal[n],
        Faces[j].Normal[n],
        Faces[j].TexCoord[n],
        Faces[j].Color[n]
      );
    end;
    MeshData.Geoms[i].VCount := VertexCount;
    SetLength(MeshData.Geoms[i].Vertices, VertexCount);
    for j := 0 to MeshData.Geoms[i].VCount - 1 do
    begin
      MeshData.Geoms[i].Vertices[j].Position := Positions[Vertices[j].Position];
      MeshData.Geoms[i].Vertices[j].Tangent := Tangents[Vertices[j].Tangent];
      MeshData.Geoms[i].Vertices[j].Binormal := Binormals[Vertices[j].Binormal];
      MeshData.Geoms[i].Vertices[j].Normal := Normals[Vertices[j].Normal];
      SetLength(MeshData.Geoms[i].Vertices[j].TexCoords, 1);
      MeshData.Geoms[i].Vertices[j].TexCoords[0] := TexCoords[Vertices[j].TexCoord];
      MeshData.Geoms[i].Vertices[j].Color := Colors[Vertices[j].Color];
    end;
    for j := 0 to MeshData.Geoms[i].FCount - 1 do
    for n := 0 to 2 do
    MeshData.Geoms[i].Faces[j].Indices[n] := Faces[j].Vertices[n];
  end;
end;

procedure G2MeshRagdollObjectCopy(const Src, Dst: PG2RagdollObject);
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

end.
