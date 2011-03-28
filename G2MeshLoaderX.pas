//G2MeshLoaderX v1.0
//Author: Dan
unit G2MeshLoaderX;

{$include Gen2.inc}

{-------------------------------------------------------------------------------
DirectX File Format:
  Header:
    Header: String4 = "xof "
    Version:
      Major: String2
      Minor: String2
    FormatType: String4 = "txt ", "bin ", "tzip", "bzip"
    FloatSize: String4 = "0032", "0064"

  Templates:
    Header:
      <3D82AB43-62DA-11cf-AB39-0020AF71E433>
      Major: UInt2;
      Minor: UInt2;
      Flags: UInt4;

    Vector:
      <3d82ab5e-62da-11cf-ab39-0020af71e433>
      x: Float;
      y: Float;
      z: Float;

    Coords2d:
      <F6F23F44-7686-11cf-8F52-0040333594A3>
      u: Float;
      v: Float;

    Quaternion:
      <10DD46A3-775B-11cf-8F52-0040333594A3>
      s: Float;
      v: Vector;

    Matrix4x4:
      <F6F23F45-7686-11cf-8F52-0040333594A3>
      m[16]: Float;

    ColorRGBA:
      <35FF44E0-6C7C-11cf-8F52-0040333594A3>
      r: Float;
      g: Float;
      b: Float;
      a: Float;

    ColorRGB:
      <D3E16E81-7835-11cf-8F52-0040333594A3>
      r: Float;
      g: Float;
      b: Float;

    IndexedColor:
      <1630B820-7842-11cf-8F52-0040333594A3>
      Index: UInt4;
      Color: ColorRGBA

    Boolean:
      <4885AE61-78E8-11cf-8F52-0040333594A3>
      TrueFalse: UInt4;

    Boolean2D:
      <4885AE63-78E8-11cf-8F52-0040333594A3>
      u: Boolean;
      v: Boolean;

    Material:
      <3D82AB4D-62DA-11cf-AB39-0020AF71E433>
      FaceColor: ColorRGBA;
      Power: Float;
      SpecularColor: ColorRGB;
      EmissiveColor: ColorRGB;
      [TextureFileName] If this is not present the face or entire mesh is not textured.
      [ ... ]

    MaterialWrap:
      <4885AE60-78E8-11CF-8F52-0040333594A3>
      u: Boolean;
      v: Boolean;

    TextureFilename:
      <A42790E1-7810-11cf-8F52-0040333594A3>
      FileName: String;

    MeshFace:
      <3D82AB5F-62DA-11cf-AB39-0020AF71E433>
      NumFaceVertexIndices: UInt4;
      FaceVertexIndices[NumFaceVertexIndices]: UInt4;

    MeshFaceWraps:
      <4885AE62-78E8-11cf-8F52-0040333594A3>
      NumFaceWrapValues: UInt4;
      FaceWrapValues[NumFaceWrapValues]: Boolean2d;

    MeshTextureCoords:
      <F6F23F40-7686-11cf-8F52-0040333594A3>
      NumTextureCoords: UInt4;
      TextureCoords[NumTextureCoords]: Coords2d;

    MeshNormals:
      <F6F23F43-7686-11cf-8F52-0040333594A3>
      NumNormals: UInt4;
      Normals[NumNormals]: Vector;
      NunFaceNormals: UInt4;
      FaceNormals[NunFaceNormals]: MeshFace;

    MeshVertexColors:
      <1630B821-7842-11cf-8F52-0040333594A3>
      NumVertexColors: UInt4;
      VertexColors[NumVertexColors]: IndexedColor;

    MeshMaterialList:
      <F6F23F42-7686-11cf-8F52-0040333594A3>
      NumMaterials: UInt4;
      NumFaceIndexes: UInt4;
      FaceIndexes[NumFaceIndexes]: UInt4;
      [Material]

    Mesh:
      <3D82AB44-62DA-11cf-AB39-0020AF71E433>
      NumVertices: UInt4;
      Vertices[NumVertices]: Vector;
      NumFaces: UInt4;
      Faces[NumFaces]: MeshFace;
      [MeshFaceWraps] If this is not present, wrapping for both u and v defaults to false.
      [MeshTextureCoords] If this is not present, there are no texture coordinates.
      [MeshNormals] If this is not present, normals are generated using the GenerateNormals method of the API.
      [MeshVertexColors] If this is not present, the colors default to white.
      [MeshMaterialList] If this is not present, the material defaults to white.
      [XSkinMeshHeader] If this is not present, the mesh is not skinned.
      [SkinWeights] An instance of this template for every bone (Frame) influencing the mesh.
      [Material] Specifies a material for the entire mesh.
      [TextureFileName] Also possible to have a texture without a material.
      [ ... ]

    FrameTransformMatrix:
      <F6F23F41-7686-11cf-8F52-0040333594A3>
      FrameMatrix: Matrix4x4;

    Frame:
      <3D82AB46-62DA-11cf-AB39-0020AF71E433>
      [Frame]
      [FrameTransformMatrix]
      [Mesh]
      [ ... ]

    FloatKeys:
      <10DD46A9-775B-11cf-8F52-0040333594A3>
      NunValues: UInt4;
      Values[NunValues]: Float;

    TimedFloatKeys:
      <F406B180-7B3B-11cf-8F52-0040333594A3>
      Time: UInt4;
      Keys: FloatKeys;

    AnimationKey:
      <10DD46A8-775B-11cf-8F52-0040333594A3>
      KeyType: UInt4 = (0 - Rotation: Float4, 1 - Scale: Float3, 2 - Position: Float3, 3 - Matrix: Float16);
      NumKeys: UInt4;
      Keys[NumKeys]: TimedFloatKeys;

    AnimationOptions:
      <E2BF56C0-840F-11cf-8F52-0040333594A3>
      OpenClosed: UInt4 = (0 - Closed, 1 - Open);
      PositionQuality: UInt4 = (0 - Spline, 1 - Linear);

    Animation:
      <3D82AB4F-62DA-11cf-AB39-0020AF71E433>
      [AnimationKey]
      [AnimationOptions] 	If this element is not present, an animation is closed.
      [...]

    AnimationSet:
      <3D82AB50-62DA-11cf-AB39-0020AF71E433>
      [Animation]

    XSkinMeshHeader:
      <3CF169CE-FF7C-44ab-93C0-F78F62D172E2>
      NumMaxSkinWeightsPerVertex: UInt4;
      NumMaxSkinWeightsPerFace: UInt4;
      NumBones: UInt4;

    VertexDuplicationIndices:
      <B8D65549-D7C9-4995-89CF-53A9A8B031E3>
      NumIndices: UInt4;
      NumOriginalVertices: UInt4;
      Indices[NumIndices]: UInt4;

    FaceAdjacency:
      <A64C844A-E282-4756-8B80-250CDE04398C>
      NumIndices: UInt4;
      Indices[NumIndices]: UInt4;

    SkinWeights:
      <6F0D123B-BAD2-4167-A0D0-80224F25FABB>
      TransformNodeName: String; 
      NumWeights: UInt4;
      VertexIndices[NumWeights]: UInt4;
      Weights[NumWeights]: Float;
      MatrixOffset: Matrix4x4;

    Patch:
      <A3EB5D44-FC22-429D-9AFB-3221CB9719A6>
      NumControlIndices: UInt4;
      ControlIndices[NumControlIndices]: DWord;

    PatchMesh:
      <D02C95CC-EDBA-4305-9B5D-1820D7704BBF>
      NumVertices: UInt4;
      Vertices[NumVertices]: Vector;
      NumPatches: UInt4;
      Patches[NumPatches]: Patch;
      [ ... ]

    PatchMesh9:
      <B9EC94E1-B9A6-4251-BA18-94893F02C0EA>
      Type: UInt4;
      Degree: UInt4;
      Basis: UInt4;
      NumVertices: UInt4;
      Vertices[NumVertices]: Vector;
      NumPatches: UInt4;
      Patches[NumPatches]: Patch;
      [ ... ]

    EffectFloats:
      <F1CFE2B3-0DE3-4e28-AFA1-155A750A282D>
      NumFloats: UInt4;
      Floats[NumFloats]: Float;

    EffectString:
      <D55B097E-BDB6-4c52-B03D-6051C89D0E42>
      Value: String;

    EffectDWord:
      <622C0ED0-956E-4da9-908A-2AF94F3CE716>
      Value: UInt4;

    EffectParamFloats:
      <3014B9A0-62F5-478c-9B86-E4AC9F4E418B>
      ParamName: String;
      NumFloats: UInt4;
      Floats[NumFloats]: Float;

    EffectParamString:
      <1DBC4C88-94C1-46ee-9076-2C28818C9481>
      ParamName: String;
      Value: String;

    EffectParamDWord:
      <E13963BC-AE51-4c5d-B00F-CFA3A9D97CE5>
      ParamName: String;
      Value: UInt4;

    EffectInstance:
      <E331F7E4-0559-4cc2-8E99-1CEC1657928F>
      EffectFilename: String;
      [ ... ]

    AnimTicksPerSecond: If not present the value defaults to 4800
      <9E415A43-7BA6-4a73-8743-B73D47E88476>
      AnimTicksPerSecond: UInt4;

    CompressedAnimationSet:
      <7F9B00B3-F125-4890-876E-1C42BF697C4D>
      CompressedBlockSize: UInt4;
      TicksPerSec: Float;
      PlaybackType: UInt4;
      BufferLength: UInt4;
      CompressedData[BufferLength]: UInt4;

    FVFData:
      <B6E70A0E-8EF9-4e83-94AD-ECC8B0C04897>
      FVF: UInt4;
      NumDWords: UInt4;
      Data[NumDWords]: UInt4;

    VertexElement:
      <F752461C-1E23-48f6-B9F8-8350850F336F>
      Type: UInt4;
      Method: UInt4;
      Usage: UInt4;
      UsageIndex: UInt4;

    DeclData:
      <BF22E553-292C-4781-9FEA-62BD554BDD93>
      NumElements: UInt4;
      Elements[NumElements]: VertexElement;
      NumDWords: UInt4;
      Data[NumDWords]: UInt4;

    PMAttributeRange:
      <917E0427-C61E-4a14-9C64-AFE65F9E9844>
      FaceOffset: UInt4;
      FacesMin: UInt4;
      FacesMax: UInt4;
      VertexOffset: UInt4;
      VerticesMin: UInt4;
      VerticesMax: UInt4;

    PMVSplitRecord:
      <574CCC14-F0B3-4333-822D-93E8A8A08E4C>
      FaceCLW: UInt4;
      VlrOffset: UInt4;
      Code: UInt4;

    PMInfo:
      <B6C3E656-EC8B-4b92-9B62-681659522947>
      NumAttributes: UInt4;
      AttributeRanges[NumAttributes]: PMAttributeRange;
      MaxValence: UInt4;
      MinLogicalVertices: UInt4;
      MaxLogicalVertices: UInt4;
      NumVSplits: UInt4;
      SplitRecords[NumVSplits]: PMVSplitRecord;
      NumAttributeMispredicts: UInt4;
      AttributeMispredicts[NumAttributeMispredicts]: UInt4;

    Guid:
      <A42790E0-7810-11CF-8F52-0040333594A3>
      Data1: UInt4;
      Data2: UInt2;
      Data3: UInt2;
      Data4[8]: Byte;
-------------------------------------------------------------------------------}

interface

uses
  Gen2,
  G2Math,
  G2MeshLoader,
  Windows,
  Classes,
  Controls,
  Graphics,
  SysUtils,
  SyncObjs,
  ActiveX,
  Forms,
  Math,
  Dialogs,
  DirectInput,
  DirectShow9,
  DirectMusic,
  DirectSound,
  Direct3D9,
  DXTypes,
  DXErr9,
  D3DX9;

const
  GUID_TemplateHeader: TGUID = '{3D82AB43-62DA-11cf-AB39-0020AF71E433}';
  GUID_TemplateVector: TGUID = '{3D82AB5E-62DA-11cf-AB39-0020AF71E433}';
  GUID_TemplateCoords2D: TGUID = '{F6F23F44-7686-11cf-8F52-0040333594A3}';
  GUID_TemplateQuaternion: TGUID = '{10DD46A3-775B-11cf-8F52-0040333594A3}';
  GUID_TemplateMatrix4x4: TGUID = '{F6F23F45-7686-11cf-8F52-0040333594A3}';
  GUID_TemplateColorRGBA: TGUID = '{35FF44E0-6C7C-11cf-8F52-0040333594A3}';
  GUID_TemplateColorRGB: TGUID = '{D3E16E81-7835-11cf-8F52-0040333594A3}';
  GUID_TemplateIndexedColor: TGUID = '{1630B820-7842-11cf-8F52-0040333594A3}';
  GUID_TemplateBoolean: TGUID = '{4885AE61-78E8-11cf-8F52-0040333594A3}';
  GUID_TemplateBoolean2D: TGUID = '{4885AE63-78E8-11cf-8F52-0040333594A3}';
  GUID_TemplateMaterial: TGUID = '{3D82AB4D-62DA-11cf-AB39-0020AF71E433}';
  GUID_TemplateTextureFileName: TGUID = '{A42790E1-7810-11cf-8F52-0040333594A3}';
  GUID_TemplateMeshFace: TGUID = '{3D82AB5F-62DA-11cf-AB39-0020AF71E433}';
  GUID_TemplateMeshFaceWraps: TGUID = '{4885AE62-78E8-11cf-8F52-0040333594A3}';
  GUID_TemplateMeshTextureCoords: TGUID = '{F6F23F40-7686-11cf-8F52-0040333594A3}';
  GUID_TemplateMeshNormals: TGUID = '{F6F23F43-7686-11cf-8F52-0040333594A3}';
  GUID_TemplateMeshVertexColors: TGUID = '{1630B821-7842-11cf-8F52-0040333594A3}';
  GUID_TemplateMeshMaterialList: TGUID = '{F6F23F42-7686-11cf-8F52-0040333594A3}';
  GUID_TemplateMesh: TGUID = '{3D82AB44-62DA-11cf-AB39-0020AF71E433}';
  GUID_TemplateFrameTransformMatrix: TGUID = '{F6F23F41-7686-11cf-8F52-0040333594A3}';
  GUID_TemplateFrame: TGUID = '{3D82AB46-62DA-11cf-AB39-0020AF71E433}';
  GUID_TemplateFloatKeys: TGUID = '{10DD46A9-775B-11cf-8F52-0040333594A3}';
  GUID_TemplateTimedFloatKeys: TGUID = '{F406B180-7B3B-11cf-8F52-0040333594A3}';
  GUID_TemplateAnimationKey: TGUID = '{10DD46A8-775B-11cf-8F52-0040333594A3}';
  GUID_TemplateAnimationOptions: TGUID = '{E2BF56C0-840F-11cf-8F52-0040333594A3}';
  GUID_TemplateAnimation: TGUID = '{3D82AB4F-62DA-11cf-AB39-0020AF71E433}';
  GUID_TemplateAnimationSet: TGUID = '{3D82AB50-62DA-11cf-AB39-0020AF71E433}';
  GUID_TemplateXSkinMeshHeader: TGUID = '{3CF169CE-FF7C-44ab-93C0-F78F62D172E2}';
  GUID_TemplateVertexDuplicationIndices: TGUID = '{B8D65549-D7C9-4995-89CF-53A9A8B031E3}';
  GUID_TemplateFaceAdjacency: TGUID = '{A64C844A-E282-4756-8B80-250CDE04398C}';
  GUID_TemplateSkinWeights: TGUID = '{6F0D123B-BAD2-4167-A0D0-80224F25FABB}';
  GUID_TemplatePatch: TGUID = '{A3EB5D44-FC22-429D-9AFB-3221CB9719A6}';
  GUID_TemplatePatchMesh: TGUID = '{D02C95CC-EDBA-4305-9B5D-1820D7704BBF}';
  GUID_TemplatePatchMesh9: TGUID = '{B9EC94E1-B9A6-4251-BA18-94893F02C0EA}';
  GUID_TemplateEffectFloats: TGUID = '{F1CFE2B3-0DE3-4e28-AFA1-155A750A282D}';
  GUID_TemplateEffectString: TGUID = '{D55B097E-BDB6-4c52-B03D-6051C89D0E42}';
  GUID_TemplateEffectDWord: TGUID = '{622C0ED0-956E-4da9-908A-2AF94F3CE716}';
  GUID_TemplateEffectParamFloats: TGUID = '{3014B9A0-62F5-478c-9B86-E4AC9F4E418B}';
  GUID_TemplateEffectParamString: TGUID = '{1DBC4C88-94C1-46ee-9076-2C28818C9481}';
  GUID_TemplateEffectParamDWord: TGUID = '{E13963BC-AE51-4c5d-B00F-CFA3A9D97CE5}';
  GUID_TemplateEffectInstance: TGUID = '{E331F7E4-0559-4cc2-8E99-1CEC1657928F}';
  GUID_TemplateAnimTicksPerSecond: TGUID = '{9E415A43-7BA6-4a73-8743-B73D47E88476}';
  GUID_TemplateCompressedAnimationSet: TGUID = '{7F9B00B3-F125-4890-876E-1C42BF697C4D}';
  GUID_TemplateFVFData: TGUID = '{B6E70A0E-8EF9-4e83-94AD-ECC8B0C04897}';
  GUID_TemplateVertexElement: TGUID = '{F752461C-1E23-48f6-B9F8-8350850F336F}';
  GUID_TemplateDeclData: TGUID = '{BF22E553-292C-4781-9FEA-62BD554BDD93}';
  GUID_TemplatePMAttributeRange: TGUID = '{917E0427-C61E-4a14-9C64-AFE65F9E9844}';
  GUID_TemplatePMVSplitRecord: TGUID = '{574CCC14-F0B3-4333-822D-93E8A8A08E4C}';
  GUID_TemplatePMInfo: TGUID = '{B6C3E656-EC8B-4b92-9B62-681659522947}';
  GUID_TemplateGuid: TGUID = '{A42790E0-7810-11CF-8F52-0040333594A3}';

  XTemplates: AnsiString =
    'xof 0303txt 0032 ' +
    'template ColorRGBA { ' +
    ' <35ff44e0-6c7c-11cf-8f52-0040333594a3> ' +
    ' FLOAT red; ' +
    ' FLOAT green; ' +
    ' FLOAT blue; ' +
    ' FLOAT alpha; ' +
    '} ' +
    'template ColorRGB { ' +
    ' <d3e16e81-7835-11cf-8f52-0040333594a3> ' +
    ' FLOAT red; ' +
    ' FLOAT green; ' +
    ' FLOAT blue; ' +
    '} ' +
    'template Material { ' +
    ' <3d82ab4d-62da-11cf-ab39-0020af71e433> ' +
    ' ColorRGBA faceColor; ' +
    ' FLOAT power; ' +
    ' ColorRGB specularColor; ' +
    ' ColorRGB emissiveColor; ' +
    ' [...] ' +
    '} ' +
    'template TextureFilename { ' +
    ' <a42790e1-7810-11cf-8f52-0040333594a3> ' +
    ' STRING filename; ' +
    '} ' +
    'template Frame { ' +
    ' <3d82ab46-62da-11cf-ab39-0020af71e433> ' +
    ' [...] ' +
    '} ' +
    'template Matrix4x4 { ' +
    ' <f6f23f45-7686-11cf-8f52-0040333594a3> ' +
    ' array FLOAT matrix[16]; ' +
    '} ' +
    'template FrameTransformMatrix { ' +
    ' <f6f23f41-7686-11cf-8f52-0040333594a3> ' +
    ' Matrix4x4 frameMatrix; ' +
    '} ' +
    'template Vector { ' +
    ' <3d82ab5e-62da-11cf-ab39-0020af71e433> ' +
    ' FLOAT x; ' +
    ' FLOAT y; ' +
    ' FLOAT z; ' +
    '} ' +
    'template MeshFace { ' +
    ' <3d82ab5f-62da-11cf-ab39-0020af71e433> ' +
    ' DWORD nFaceVertexIndices; ' +
    ' array DWORD faceVertexIndices[nFaceVertexIndices]; ' +
    '} ' +
    'template Mesh { ' +
    ' <3d82ab44-62da-11cf-ab39-0020af71e433> ' +
    ' DWORD nVertices; ' +
    ' array Vector vertices[nVertices]; ' +
    ' DWORD nFaces; ' +
    ' array MeshFace faces[nFaces]; ' +
    ' [...] ' +
    '} ' +
    'template MeshNormals { ' +
    ' <f6f23f43-7686-11cf-8f52-0040333594a3> ' +
    ' DWORD nNormals; ' +
    ' array Vector normals[nNormals]; ' +
    ' DWORD nFaceNormals; ' +
    ' array MeshFace faceNormals[nFaceNormals]; ' +
    '} ' +
    'template MeshMaterialList { ' +
    ' <f6f23f42-7686-11cf-8f52-0040333594a3> ' +
    ' DWORD nMaterials; ' +
    ' DWORD nFaceIndexes; ' +
    ' array DWORD faceIndexes[nFaceIndexes]; ' +
    ' [Material <3d82ab4d-62da-11cf-ab39-0020af71e433>] ' +
    '} ' +
    'template Coords2d { ' +
    ' <f6f23f44-7686-11cf-8f52-0040333594a3> ' +
    ' FLOAT u; ' +
    ' FLOAT v; ' +
    '} ' +
    'template MeshTextureCoords { ' +
    ' <f6f23f40-7686-11cf-8f52-0040333594a3> ' +
    ' DWORD nTextureCoords; ' +
    ' array Coords2d textureCoords[nTextureCoords]; ' +
    '} ' +
    'template XSkinMeshHeader { ' +
    ' <3cf169ce-ff7c-44ab-93c0-f78f62d172e2> ' +
    ' WORD nMaxSkinWeightsPerVertex; ' +
    ' WORD nMaxSkinWeightsPerFace; ' +
    ' WORD nBones; ' +
    '} ' +
    'template SkinWeights { ' +
    ' <6f0d123b-bad2-4167-a0d0-80224f25fabb> ' +
    ' STRING transformNodeName; ' +
    ' DWORD nWeights; ' +
    ' array DWORD vertexIndices[nWeights]; ' +
    ' array FLOAT weights[nWeights]; ' +
    ' Matrix4x4 matrixOffset; ' +
    '} ' +
    'template Animation { ' +
    ' <3d82ab4f-62da-11cf-ab39-0020af71e433> ' +
    ' [...] ' +
    '} ' +
    'template AnimationSet { ' +
    ' <3d82ab50-62da-11cf-ab39-0020af71e433> ' +
    ' [Animation <3d82ab4f-62da-11cf-ab39-0020af71e433>] ' +
    '} ' +
    'template FloatKeys { ' +
    ' <10dd46a9-775b-11cf-8f52-0040333594a3> ' +
    ' DWORD nValues; ' +
    ' array FLOAT values[nValues]; ' +
    '} ' +
    'template TimedFloatKeys { ' +
    ' <f406b180-7b3b-11cf-8f52-0040333594a3> ' +
    ' DWORD time; ' +
    ' FloatKeys tfkeys; ' +
    '} ' +
    'template AnimationKey { ' +
    ' <10dd46a8-775b-11cf-8f52-0040333594a3> ' +
    ' DWORD keyType; ' +
    ' DWORD nKeys; ' +
    ' array TimedFloatKeys keys[nKeys]; ' +
    '} ' +
    'template AnimTicksPerSecond {' +
    ' <9e415a43-7ba6-4a73-8743-b73d47e88476>' +
    ' DWORD ticksPerSecond; ' +
    '}';

type
  PXFrame = ^TXFrame;
  TXFrame = record
  public
    xid: TGUID;
    Name: AnsiString;
    Index: Integer;
    Transform: TG2Mat;
    ParentID: Integer;
    MeshID: Integer;
  end;

  PXMesh = ^TXMesh;
  TXMesh = record
  public
    xid: TGUID;
    Name: AnsiString;
    Index: Integer;
    XFrameID: Integer;
    XSkinID: Integer;
    VCount: DWord;
    FCount: DWord;
    Vertices: array of record
      Position: TG2Vec3;
      Normal: TG2Vec3;
      TexCoord: TG2Vec2;
    end;
    Indices: array of DWord;
  end;

  PXAnimKey = ^TXAnimKey;
  TXAnimKey = record
  public
    Time: DWord;
    KeyMatrix: TG2Mat;
    KeyScale: TG2Vec3;
    KeyRotation: TG2Quat;
    KeyTranslation: TG2Vec3;
    LoadMatrix: Boolean;
    LoadScale: Boolean;
    LoadRotation: Boolean;
    LoadTranslation: Boolean;
  end;

  PXAnim = ^TXAnim;
  TXAnim = record
  public
    FrameID: Integer;
    KeyCount: DWord;
    FrameCount: DWord;
    Keys: array of TXAnimKey;
  end;

  PXAnimSet = ^TXAnimSet;
  TXAnimSet = record
  public
    Name: AnsiString;
    Index: Integer;
    KeyCount: DWord;
    FrameCount: DWord;
    XAnims: array of TXAnim;
  end;

  PXSkinVertex = ^TXSkinVertex;
  TXSkinVertex = record
  public
    FrameCount: DWord;
    FrameWeights: array of record
    public
      FrameID: Integer;
      FrameWeight: Single;
    end;
  end;

  PXSkin = ^TXSkin;
  TXSkin = record
  public
    MeshID: Integer;
    FrameCount: DWord;
    Frames: array of record
    public
      FrameName: AnsiString;
      FrameID: Integer;
      Bind: TG2Mat;
    end;
    VertexCount: DWord;
    Vertices: array of TXSkinVertex;
  end;

  TG2MeshLoaderX = class (TG2MeshLoader)
  strict private
    m_XFrames: array of TXFrame;
    m_XMeshes: array of TXMesh;
    m_XAnimSets: array of TXAnimSet;
    m_XSkins: array of TXSkin;
    m_TicksPerSecond: DWord;
    m_CurFrame: PXFrame;
  strict protected
    function CheckDataType(const Data: ID3DXFileData; const CheckType: TGUID): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDataName(const Data: ID3DXFileData; const MaxSize: Integer = 256): AnsiString;
    function GetTemplateName(const Template: TGUID): AnsiString; overload;
    function GetTemplateName(const Data: ID3DXFileData): AnsiString; overload;
    function GetFrameIDByXID(const XID: TGUID): Integer;
    function GetMeshIDByXID(const XID: TGUID): Integer;
    function GetFrameIDByName(const Name: AnsiString): Integer;
    procedure LoadXFrame(const Data: ID3DXFileData; const ParentID: Integer = -1);
    procedure LoadXMesh(const Data: ID3DXFileData; const XFrameID: Integer = -1);
    procedure LoadXAnimSet(const Data: ID3DXFileData);
    procedure LoadXAnim(const Data: ID3DXFileData; const XAnimSetID: Integer);
    procedure NormalizeSkins;
    function GetXFrame(const Index: Integer): PXFrame; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetXFrameCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetXMesh(const Index: Integer): PXMesh; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetXMeshCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetXAnim(const Index: Integer): PXAnimSet; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetXAnimCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetXSkin(const Index: Integer): PXSkin; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetXSkinCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property DataFrames[const Index: Integer]: PXFrame read GetXFrame;
    property DataFrameCount: Integer read GetXFrameCount;
    property DataMeshes[const Index: Integer]: PXMesh read GetXMesh;
    property DataMeshCount: Integer read GetXMeshCount;
    property DataAnims[const Index: Integer]: PXAnimSet read GetXAnim;
    property DataAnimCount: Integer read GetXAnimCount;
    property DataSkins[const Index: Integer]: PXSkin read GetXSkin;
    property DataSkinCount: Integer read GetXSkinCount;
    property DataTicksPerSeond: DWord read m_TicksPerSecond;
    procedure ExportMesh(const Device: IDirect3DDevice9; const MeshData: PG2MeshData); override;
    procedure LoadStream(const s: TStream); override;
    class function GetDifinition: AnsiString; override;
    class function CanLoadStream(const s: TStream): Boolean; override;
  end;

  TXDataControl = class
  strict private
    m_Data: ID3DXFileData;
    m_Locked: Boolean;
    m_Pos: Integer;
    m_pb: PByteArray;
    m_LockSize: DWord;
    procedure SetData(const Value: ID3DXFileData);
  public
    constructor Create;
    destructor Destroy; override;
    property Data: ID3DXFileData read m_Data write SetData;
    property Pos: Integer read m_Pos write m_Pos;
    procedure Lock(const Pos, Size: DWord); overload;
    procedure Lock(const Size: DWord); overload;
    procedure Unlock;
    procedure Skip(const Offset: Integer);
    procedure ReadBuffer(var i; const Size: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function ReadWord: Word;
    function ReadDWord: DWord;
    function ReadFloat: Single;
    function ReadString: AnsiString;
    function ReadVec3: TG2Vec3;
    function ReadVec2: TG2Vec2;
    function ReadQuat: TG2Quat;
    function ReadMat: TG2Mat;
  end;

implementation

//TG2MeshLoaderX BEGIN
function TG2MeshLoaderX.CheckDataType(const Data: ID3DXFileData; const CheckType: TGUID): Boolean;
var
  DataType: TGUID;
begin
  Data.GetType(DataType);
  Result := IsEqualGUID(DataType, CheckType);
end;

function TG2MeshLoaderX.GetDataName(const Data: ID3DXFileData; const MaxSize: Integer = 256): AnsiString;
var
  NameSize: Integer;
  TmpName: AnsiString;
begin
  NameSize := MaxSize;
  SetLength(TmpName, NameSize);
  if Succeeded(
    Data.GetName(@TmpName[1], @NameSize)
  ) then
  begin
    NameSize := 0;
    while (NameSize < MaxSize) and (TmpName[NameSize + 1] <> #0) do
    Inc(NameSize);
    SetLength(Result, NameSize);
    Move(TmpName[1], Result[1], NameSize);
  end
  else
  Result := '';
end;

function TG2MeshLoaderX.GetTemplateName(const Template: TGUID): AnsiString;
type
  TTemplateName = record
  public
    Template: PGUID;
    Name: AnsiString;
  end;
const
  TemplateNames: array[0..49] of TTemplateName = (
    (Template: @GUID_TemplateHeader; Name: 'Header'),
    (Template: @GUID_TemplateVector; Name: 'Vector'),
    (Template: @GUID_TemplateCoords2D; Name: 'Coords2D'),
    (Template: @GUID_TemplateQuaternion; Name: 'Quaternion'),
    (Template: @GUID_TemplateMatrix4x4; Name: 'Matrix4x4'),
    (Template: @GUID_TemplateColorRGBA; Name: 'RGBA'),
    (Template: @GUID_TemplateColorRGB; Name: 'RGB'),
    (Template: @GUID_TemplateIndexedColor; Name: 'IndexedColor'),
    (Template: @GUID_TemplateBoolean; Name: 'Boolean'),
    (Template: @GUID_TemplateBoolean2D; Name: 'Boolean2D'),
    (Template: @GUID_TemplateMaterial; Name: 'Material'),
    (Template: @GUID_TemplateTextureFileName; Name: 'TextureFileName'),
    (Template: @GUID_TemplateMeshFace; Name: 'MeshFace'),
    (Template: @GUID_TemplateMeshFaceWraps; Name: 'MeshFaceWraps'),
    (Template: @GUID_TemplateMeshTextureCoords; Name: 'MeshTextureCoords'),
    (Template: @GUID_TemplateMeshNormals; Name: 'MeshNormals'),
    (Template: @GUID_TemplateMeshVertexColors; Name: 'MeshVertexColors'),
    (Template: @GUID_TemplateMeshMaterialList; Name: 'MeshMaterialList'),
    (Template: @GUID_TemplateMesh; Name: 'Mesh'),
    (Template: @GUID_TemplateFrameTransformMatrix; Name: 'FrameTransformMatrix'),
    (Template: @GUID_TemplateFrame; Name: 'Frame'),
    (Template: @GUID_TemplateFloatKeys; Name: 'FloatKeys'),
    (Template: @GUID_TemplateTimedFloatKeys; Name: 'TimedFloatKeys'),
    (Template: @GUID_TemplateAnimationKey; Name: 'AnimationKey'),
    (Template: @GUID_TemplateAnimationOptions; Name: 'AnimationOptions'),
    (Template: @GUID_TemplateAnimation; Name: 'Animation'),
    (Template: @GUID_TemplateAnimationSet; Name: 'AnimationSet'),
    (Template: @GUID_TemplateXSkinMeshHeader; Name: 'XSkinMeshHeader'),
    (Template: @GUID_TemplateVertexDuplicationIndices; Name: 'VertexDuplicationIndices'),
    (Template: @GUID_TemplateFaceAdjacency; Name: 'FaceAdjacency'),
    (Template: @GUID_TemplateSkinWeights; Name: 'SkinWeights'),
    (Template: @GUID_TemplatePatch; Name: 'Patch'),
    (Template: @GUID_TemplatePatchMesh; Name: 'PatchMesh'),
    (Template: @GUID_TemplatePatchMesh9; Name: 'PatchMesh9'),
    (Template: @GUID_TemplateEffectFloats; Name: 'EffectFloats'),
    (Template: @GUID_TemplateEffectString; Name: 'EffectString'),
    (Template: @GUID_TemplateEffectDWord; Name: 'EffectDWord'),
    (Template: @GUID_TemplateEffectParamFloats; Name: 'EffectParamFloats'),
    (Template: @GUID_TemplateEffectParamString; Name: 'EffectParamString'),
    (Template: @GUID_TemplateEffectParamDWord; Name: 'EffectParamDWord'),
    (Template: @GUID_TemplateEffectInstance; Name: 'EffectInstance'),
    (Template: @GUID_TemplateAnimTicksPerSecond; Name: 'AnimTicksPerSecond'),
    (Template: @GUID_TemplateCompressedAnimationSet; Name: 'CompressedAnimationSet'),
    (Template: @GUID_TemplateFVFData; Name: 'FVFData'),
    (Template: @GUID_TemplateVertexElement; Name: 'VertexElement'),
    (Template: @GUID_TemplateDeclData; Name: 'DeclData'),
    (Template: @GUID_TemplatePMAttributeRange; Name: 'PMAttributeRange'),
    (Template: @GUID_TemplatePMVSplitRecord; Name: 'PMVSplitRecord'),
    (Template: @GUID_TemplatePMInfo; Name: 'PMInfo'),
    (Template: @GUID_TemplateGuid; Name: 'Guid')
  );
var
  i: Integer;
begin
  for i := 0 to High(TemplateNames) do
  if IsEqualGUID(TemplateNames[i].Template^, Template) then
  begin
    Result := TemplateNames[i].Name;
    Exit;
  end;
  Result := 'Unknown';
end;

function TG2MeshLoaderX.GetTemplateName(const Data: ID3DXFileData): AnsiString;
var
  DataType: TGUID;
begin
  Data.GetType(DataType);
  Result := GetTemplateName(DataType);
end;

function TG2MeshLoaderX.GetFrameIDByXID(const XID: TGUID): Integer;
var
  i: Integer;
begin
  for i := 0 to High(m_XFrames) do
  if IsEqualGUID(m_XFrames[i].xid, XID) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TG2MeshLoaderX.GetMeshIDByXID(const XID: TGUID): Integer;
var
  i: Integer;
begin
  for i := 0 to High(m_XMeshes) do
  if IsEqualGUID(m_XMeshes[i].xid, XID) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TG2MeshLoaderX.GetFrameIDByName(const Name: AnsiString): Integer;
var
  i: Integer;
begin
  for i := 0 to High(m_XFrames) do
  if m_XFrames[i].Name = Name then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

procedure TG2MeshLoaderX.LoadXFrame(const Data: ID3DXFileData; const ParentID: Integer = -1);
var
  XFrame: PXFrame;
  ChildrenCount: DWord;
  ChildData: ID3DXFileData;
  ChildDataType: TGUID;
  XData: TXDataControl;
  CurIndex, i, n: Integer;
begin
  if not CheckDataType(Data, GUID_TemplateFrame) then Exit;
  SetLength(m_XFrames, Length(m_XFrames) + 1);
  XFrame := @m_XFrames[High(m_XFrames)];
  Data.GetId(XFrame^.xid);
  XFrame^.Name := GetDataName(Data);
  XFrame^.Index := High(m_XFrames);
  XFrame^.ParentID := ParentID;
  XFrame^.MeshID := -1;
  XFrame^.Transform.SetIdentity;
  n := XFrame^.Index;
  CurIndex := XFrame^.Index;
  Data.GetChildren(ChildrenCount);
  for i := 0 to ChildrenCount - 1 do
  begin
    Data.GetChild(i, ChildData);
    ChildData.GetType(ChildDataType);
    if IsEqualGUID(ChildDataType, GUID_TemplateFrame) then
    LoadXFrame(ChildData, CurIndex)
    else if IsEqualGUID(ChildDataType, GUID_TemplateMesh) then
    LoadXMesh(ChildData, CurIndex)
    else if IsEqualGUID(ChildDataType, GUID_TemplateFrameTransformMatrix) then
    begin
      XData := TXDataControl.Create;
      XData.Data := ChildData;
      m_XFrames[n].Transform := XData.ReadMat;
      XData.Free;
    end
    {$IFDEF G2_WRITE_LOG}
    else
    G2WriteLogTimed(
      '(W) Template ignored in Frame (' + GetDataName(Data) + '): ' +
      GetTemplateName(ChildDataType),
      'LoaderX'
    );
    {$ELSE};{$ENDIF}
  end;
end;

procedure TG2MeshLoaderX.LoadXMesh(const Data: ID3DXFileData; const XFrameID: Integer = -1);
var
  XMesh: PXMesh;
var
  ChildrenCount: DWord;
  ChildData: ID3DXFileData;
  ChildDataType: TGUID;
  f, i, j, n, k: Integer;
  v: DWord;
  PositionCount: DWord;
  Positions: array of TG2Vec3;
  TexCoordCount: DWord;
  TexCoords: array of TG2Vec2;
  NormalCount: DWord;
  SkinVertexCount: DWord;
  SkinVertices: array of TXSkinVertex;
  SkinFrameCount: DWord;
  SkinFrames: array of record
  public
    FrameName: AnsiString;
    Pos: TG2Mat;
    WeightCount: DWord;
  end;
  VertexIndices: array of DWord;
  VertexWeights: array of Single;
  Normals: array of TG2Vec3;
  PosRemap: array of DWord;
  TexRemap: array of DWord;
  NormRemap: array of DWord;
  SkinRemap: array of DWord;
  TexCoordsLoaded: Boolean;
  NormalsLoaded: Boolean;
  SkinLoaded: Boolean;
  XSkin: PXSkin;
  XData: TXDataControl;
  fn, CurI: Integer;
  IndArr: array of DWord;
  Faces: array of array of DWord;
begin
  if not CheckDataType(Data, GUID_TemplateMesh) then Exit;

  XData := TXDataControl.Create;
  XData.Data := Data;

  SetLength(m_XMeshes, Length(m_XMeshes) + 1);
  XMesh := @m_XMeshes[High(m_XMeshes)];
  Data.GetId(XMesh^.xid);
  XMesh^.Name := GetDataName(Data);
  XMesh^.Index := High(m_XMeshes);
  XMesh^.XFrameID := XFrameID;
  m_XFrames[XMesh^.XFrameID].MeshID := XMesh^.Index;
  XMesh^.XSkinID := -1;

  XMesh^.VCount := XData.ReadDWord;
  SetLength(XMesh^.Vertices, XMesh^.VCount);
  XData.Lock(XMesh^.VCount * SizeOf(TG2Vec3));
  for i := 0 to XMesh^.VCount - 1 do
  begin
    XMesh^.Vertices[i].Position := XData.ReadVec3;
    XMesh^.Vertices[i].Normal.SetValue(0, 0, 0);
    XMesh^.Vertices[i].TexCoord.SetValue(0, 0);
  end;
  XData.Unlock;

  XMesh^.FCount := XData.ReadDWord;
  SetLength(Faces, XMesh^.FCount);
  SetLength(XMesh^.Indices, XMesh^.FCount * 3);
  CurI := 0;
  XData.Lock(XMesh^.FCount * 16);
  for i := 0 to XMesh^.FCount - 1 do
  begin
    fn := XData.ReadDWord;
    SetLength(Faces[i], fn);
    XData.ReadBuffer(Faces[i][0], 4 * fn);
    if fn = 3 then
    begin
      if CurI + 3 > Length(XMesh^.Indices) then
      SetLength(XMesh^.Indices, Length(XMesh^.Indices) + 128);
      Move(Faces[i][0], XMesh^.Indices[CurI], 4 * fn);
      Inc(CurI, 3);
    end
    else
    begin
      if CurI + (fn - 2) * 3 > Length(XMesh^.Indices) then
      SetLength(XMesh^.Indices, Length(XMesh^.Indices) + 128);
      for j := 0 to fn - 3 do
      begin
        XMesh^.Indices[CurI + 0] := Faces[i][0];
        XMesh^.Indices[CurI + 1] := Faces[i][1 + j];
        XMesh^.Indices[CurI + 2] := Faces[i][2 + j];
        Inc(CurI, 3);
      end;
    end;
  end;
  XMesh^.FCount := CurI div 3;
  SetLength(XMesh^.Indices, CurI);
  XData.Unlock;

  TexCoordsLoaded := False;
  NormalsLoaded := False;
  SkinLoaded := False;
  TexCoordCount := 0;
  NormalCount := 0;
  SkinVertexCount := 0;
  SkinFrameCount := 0;

  Data.GetChildren(ChildrenCount);
  for i := 0 to ChildrenCount - 1 do
  begin
    Data.GetChild(i, ChildData);
    ChildData.GetType(ChildDataType);
    XData.Data := ChildData;
    if IsEqualGUID(ChildDataType, GUID_TemplateMeshTextureCoords) then
    begin
      TexCoordsLoaded := True;
      XData.Skip(4);
      for j := 0 to XMesh^.VCount - 1 do
      XMesh^.Vertices[j].TexCoord := XData.ReadVec2;
    end
    else if IsEqualGUID(ChildDataType, GUID_TemplateMeshNormals) then
    begin
      NormalsLoaded := True;
      NormalCount := XData.ReadDWord;
      SetLength(Normals, NormalCount);
      XData.ReadBuffer(Normals[0], SizeOf(TG2Vec3) * NormalCount);
      XData.Skip(4);
      XData.Lock(Length(Faces) * 16);
      for j := 0 to High(Faces) do
      begin
        XData.Skip(4);
        for k := 0 to High(Faces[j]) do
        XMesh^.Vertices[Faces[j][k]].Normal := Normals[XData.ReadDWord];
      end;
    end
    else if IsEqualGUID(ChildDataType, GUID_TemplateXSkinMeshHeader) then
    begin
      SkinLoaded := True;
      SkinVertexCount := XMesh^.VCount;
      SetLength(SkinVertices, SkinVertexCount);
      XData.Skip(4);
      SkinFrameCount := XData.ReadWord;
      SetLength(SkinFrames, SkinFrameCount);
      f := 0;
      for j := 0 to ChildrenCount - 1 do
      begin
        Data.GetChild(j, ChildData);
        if CheckDataType(ChildData, GUID_TemplateSkinWeights) then
        begin
          XData.Data := ChildData;
          SkinFrames[f].FrameName := XData.ReadString;
          SkinFrames[f].WeightCount := XData.ReadDWord;
          SetLength(VertexIndices, SkinFrames[f].WeightCount);
          SetLength(VertexWeights, SkinFrames[f].WeightCount);
          for k := 0 to SkinFrames[f].WeightCount - 1 do
          VertexIndices[k] := XData.ReadDWord;
          for k := 0 to SkinFrames[f].WeightCount - 1 do
          VertexWeights[k] := XData.ReadFloat;
          SkinFrames[f].Pos := XData.ReadMat;
          for k := 0 to SkinFrames[f].WeightCount - 1 do
          with SkinVertices[VertexIndices[k]] do
          begin
            Inc(FrameCount);
            SetLength(FrameWeights, FrameCount);
            FrameWeights[FrameCount - 1].FrameID := f;
            FrameWeights[FrameCount - 1].FrameWeight := VertexWeights[k];
          end;
          Inc(f);
        end;
      end;
    end;
  end;
  XData.Free;

  if SkinLoaded then
  begin
    SetLength(m_XSkins, Length(m_XSkins) + 1);
    XMesh^.XSkinID := High(m_XSkins);
    XSkin := @m_XSkins[High(m_XSkins)];
    XSkin^.MeshID := XMesh^.Index;
    XSkin^.FrameCount := SkinFrameCount;
    SetLength(XSkin^.Frames, XSkin^.FrameCount);
    for i := 0 to XSkin^.FrameCount - 1 do
    begin
      XSkin^.Frames[i].FrameName := SkinFrames[i].FrameName;
      XSkin^.Frames[i].Bind := SkinFrames[i].Pos;
    end;
    XSkin^.VertexCount := SkinVertexCount;
    SetLength(XSkin^.Vertices, XSkin^.VertexCount);
    for i := 0 to SkinVertexCount - 1 do
    begin
      XSkin^.Vertices[i].FrameCount := SkinVertices[i].FrameCount;
      SetLength(XSkin^.Vertices[i].FrameWeights, Length(SkinVertices[i].FrameWeights));
      for j := 0 to High(SkinVertices[i].FrameWeights) do
      begin
        XSkin^.Vertices[i].FrameWeights[j].FrameID := SkinVertices[i].FrameWeights[j].FrameID;
        XSkin^.Vertices[i].FrameWeights[j].FrameWeight := SkinVertices[i].FrameWeights[j].FrameWeight;
      end;
    end;
  end;

  {$IFDEF G2_WRITE_LOG}
  for i := 0 to ChildrenCount - 1 do
  begin
    Data.GetChild(i, ChildData);
    if not (
      CheckDataType(ChildData, GUID_TemplateMeshTextureCoords)
      or CheckDataType(ChildData, GUID_TemplateMeshNormals)
      or CheckDataType(ChildData, GUID_TemplateXSkinMeshHeader)
      or CheckDataType(ChildData, GUID_TemplateSkinWeights)
    )then
    G2WriteLogTimed(
      '(W) Template ignored in Mesh (' + GetDataName(Data) + '): ' +
      GetTemplateName(ChildData),
      'LoaderX'
    );
  end;
  {$ENDIF}
end;

procedure TG2MeshLoaderX.LoadXAnimSet(const Data: ID3DXFileData);
var
  ChildrenCount: DWord;
  ChildData: ID3DXFileData;
  XAnimSet: PXAnimSet;
  i: Integer;
begin
  if not CheckDataType(Data, GUID_TemplateAnimationSet) then Exit;

  SetLength(m_XAnimSets, Length(m_XAnimSets) + 1);
  XAnimSet := @m_XAnimSets[High(m_XAnimSets)];
  XAnimSet^.Name := GetDataName(Data);
  XAnimSet^.Index := High(m_XAnimSets);

  Data.GetChildren(ChildrenCount);
  for i := 0 to ChildrenCount - 1 do
  begin
    Data.GetChild(i, ChildData);
    if CheckDataType(ChildData, GUID_TemplateAnimation) then
    LoadXAnim(ChildData, XAnimSet^.Index)
    {$IFDEF G2_WRITE_LOG}
    else
    G2WriteLogTimed(
      '(W) Template ignored in AnimationSet (' + GetDataName(Data) + '): ' +
      GetTemplateName(ChildData),
      'LoaderX'
    );
    {$ELSE};{$ENDIF}
  end;

  XAnimSet^.KeyCount := 0;
  XAnimSet^.FrameCount := 0;
  for i := 0 to High(XAnimSet^.XAnims) do
  begin
    if XAnimSet^.XAnims[i].KeyCount > XAnimSet^.KeyCount then
    XAnimSet^.KeyCount := XAnimSet^.XAnims[i].KeyCount;
    if XAnimSet^.XAnims[i].FrameCount > XAnimSet^.FrameCount then
    XAnimSet^.FrameCount := XAnimSet^.XAnims[i].FrameCount;
  end;
end;

procedure TG2MeshLoaderX.LoadXAnim(const Data: ID3DXFileData; const XAnimSetID: Integer);
var
  ChildrenCount: DWord;
  ChildData: ID3DXFileData;
  FrameName: AnsiString;
  XAnim: PXAnim;
  t: DWord;
  KeyCount, KeyType, i, j, k, n: Integer;
  XData: TXDataControl;
  TimeZero: DWord;
  TimeResample: Single;
begin
  if not CheckDataType(Data, GUID_TemplateAnimation) then Exit;

  SetLength(m_XAnimSets[XAnimSetID].XAnims, Length(m_XAnimSets[XAnimSetID].XAnims) + 1);
  XAnim := @m_XAnimSets[XAnimSetID].XAnims[High(m_XAnimSets[XAnimSetID].XAnims)];
  XAnim^.FrameID := -1;

  XData := TXDataControl.Create;
  Data.GetChildren(ChildrenCount);
  for i := 0 to ChildrenCount - 1 do
  begin
    Data.GetChild(i, ChildData);
    if CheckDataType(ChildData, GUID_TemplateFrame) then
    begin
      FrameName := GetDataName(ChildData);
      XAnim^.FrameID := GetFrameIDByName(FrameName);
    end
    else if CheckDataType(ChildData, GUID_TemplateAnimationKey) then
    begin
      XData.Data := ChildData;
      KeyType := XData.ReadDWord;
      KeyCount := XData.ReadDWord;
      XData.Lock((SizeOf(TG2Mat) + 8) * KeyCount);
      for j := 0 to KeyCount - 1 do
      begin
        t := XData.ReadDWord;
        n := -1;
        for k := 0 to High(XAnim^.Keys) do
        if XAnim^.Keys[k].Time = t then
        begin
          n := k;
          Break;
        end;
        if n = -1 then
        begin
          n := Length(XAnim^.Keys);
          SetLength(XAnim^.Keys, n + 1);
          XAnim^.Keys[n].Time := t;
          XAnim^.Keys[n].KeyMatrix.SetIdentity;
          XAnim^.Keys[n].LoadMatrix := False;
          XAnim^.Keys[n].LoadScale := False;
          XAnim^.Keys[n].LoadRotation := False;
          XAnim^.Keys[n].LoadTranslation := False;
        end;
        XData.Skip(4);
        case KeyType of
          0: //Rotation
          begin
            XAnim^.Keys[n].KeyRotation := XData.ReadQuat;
            XAnim^.Keys[n].LoadRotation := True;
          end;
          1: //Scale
          begin
            XAnim^.Keys[n].KeyScale := XData.ReadVec3;
            XAnim^.Keys[n].LoadScale := True;
          end;
          2: //Translation
          begin
            XAnim^.Keys[n].KeyTranslation := XData.ReadVec3;
            XAnim^.Keys[n].LoadTranslation := True;
          end;
          else
          begin
            XAnim^.Keys[j].KeyMatrix := XData.ReadMat;
            XAnim^.Keys[n].LoadMatrix := True;
          end;
        end;
      end;
    end;
  end;
  XData.Free;

  XAnim^.KeyCount := Length(XAnim^.Keys);
  TimeZero := XAnim^.Keys[0].Time;
  for i := 1 to XAnim^.KeyCount - 1 do
  begin
    if XAnim^.Keys[i].Time < TimeZero then
    TimeZero := XAnim^.Keys[i].Time;
  end;
  TimeResample := 30 / m_TicksPerSecond;
  for i := 0 to XAnim^.KeyCount - 1 do
  begin
    XAnim^.Keys[i].Time := Round((XAnim^.Keys[i].Time - TimeZero) * TimeResample);
    if XAnim^.Keys[i].LoadScale
    or XAnim^.Keys[i].LoadRotation
    or XAnim^.Keys[i].LoadTranslation then
    begin
      if XAnim^.Keys[i].LoadScale then
      XAnim^.Keys[i].KeyMatrix.Scale(XAnim^.Keys[i].KeyScale);
      if XAnim^.Keys[i].LoadRotation then
      XAnim^.Keys[i].KeyMatrix.Rotate(XAnim^.Keys[i].KeyRotation);
      if XAnim^.Keys[i].LoadTranslation then
      XAnim^.Keys[i].KeyMatrix.Translate(XAnim^.Keys[i].KeyTranslation);
    end;
  end;
  XAnim^.FrameCount := 0;
  for i := 0 to XAnim^.KeyCount - 1 do
  if XAnim^.Keys[i].Time > XAnim^.FrameCount then
  XAnim^.FrameCount := XAnim^.Keys[i].Time;
  Inc(XAnim^.FrameCount);

  {$IFDEF G2_WRITE_LOG}
  for i := 0 to ChildrenCount - 1 do
  begin
    Data.GetChild(i, ChildData);
    if not (
      CheckDataType(ChildData, GUID_TemplateFrame)
      or CheckDataType(ChildData, GUID_TemplateAnimationKey)
    ) then
    G2WriteLogTimed(
      '(W) Template ignored in Animation (' + GetDataName(Data) + '): ' +
      GetTemplateName(ChildData),
      'LoaderX'
    );
  end;
  {$ENDIF}
end;

procedure TG2MeshLoaderX.NormalizeSkins;
var
  Weights: array of record
  public
    FrameIndex: DWord;
    FrameWeight: Single;
  end;
  procedure AddWeight(const Index: DWord; const Weight: Single);
  var
    n, i: Integer;
  begin
    SetLength(Weights, Length(Weights) + 1);
    n := High(Weights);
    for i := n - 1 downto 0 do
    if Weights[i].FrameWeight < Weight then
    begin
      if i + 1 < n then
      Move(Weights[i + 1], Weights[i + 2], (n - (i + 1)) * 8);
      n := i + 1;
      Break;
    end;
    Weights[n].FrameIndex := Index;
    Weights[n].FrameWeight := Weight;
  end;
var
  s, v, w: Integer;
  TotalWeight: Single;
  sv: PXSkinVertex;
begin
  for s := 0 to High(m_XSkins) do
  begin
    for v := 0 to m_XSkins[s].VertexCount - 1 do
    if m_XSkins[s].Vertices[v].FrameCount > 4 then
    begin
      sv := @m_XSkins[s].Vertices[v];
      SetLength(Weights, 0);
      for w := 0 to sv^.FrameCount - 1 do
      AddWeight(sv^.FrameWeights[w].FrameID, sv^.FrameWeights[w].FrameWeight);
      sv^.FrameCount := 4;
      SetLength(sv^.FrameWeights, sv^.FrameCount);
      TotalWeight := 0;
      for w := 0 to sv^.FrameCount - 1 do
      begin
        sv^.FrameWeights[w].FrameID := Weights[High(Weights) - w].FrameIndex;
        sv^.FrameWeights[w].FrameWeight := Weights[High(Weights) - w].FrameWeight;
        TotalWeight := TotalWeight + sv^.FrameWeights[w].FrameWeight;
      end;
      TotalWeight := 1 / TotalWeight;
      for w := 0 to sv^.FrameCount - 1 do
      sv^.FrameWeights[w].FrameWeight := sv^.FrameWeights[w].FrameWeight * TotalWeight;
    end;
  end;
end;

function TG2MeshLoaderX.GetXFrame(const Index: Integer): PXFrame;
begin
  Result := @m_XFrames[Index];
end;

function TG2MeshLoaderX.GetXFrameCount: Integer;
begin
  Result := Length(m_XFrames);
end;

function TG2MeshLoaderX.GetXMesh(const Index: Integer): PXMesh;
begin
  Result := @m_XMeshes[Index];
end;

function TG2MeshLoaderX.GetXMeshCount: Integer;
begin
  Result := Length(m_XMeshes);
end;

function TG2MeshLoaderX.GetXAnim(const Index: Integer): PXAnimSet;
begin
  Result := @m_XAnimSets[Index];
end;

function TG2MeshLoaderX.GetXAnimCount: Integer;
begin
  Result := Length(m_XAnimSets);
end;

function TG2MeshLoaderX.GetXSkin(const Index: Integer): PXSkin;
begin
  Result := @m_XSkins[Index];
end;

function TG2MeshLoaderX.GetXSkinCount: Integer;
begin
  Result := Length(m_XSkins);
end;

procedure TG2MeshLoaderX.ExportMesh(const Device: IDirect3DDevice9; const MeshData: PG2MeshData);
type
  TGeomVertex = packed record
    Position: TG2Vec3;
    Tangent: TG2Vec3;
    Binormal: TG2Vec3;
    Normal: TG2Vec3;
    TexCoords: TG2Vec2;
  end;
  PGeomVertexArray = ^TGeomVertexArray;
  TGeomVertexArray = array[Word] of TGeomVertex;
  function GetKeyAtTime(const XAnim: PXAnim; const FrameTime: DWord): TG2Mat;
  var
    k, k0: Integer;
    g: Single;
  begin
    for k := 0 to XAnim^.KeyCount - 1 do
    if XAnim^.Keys[k].Time >= FrameTime then
    begin
      if (XAnim^.Keys[k].Time = FrameTime)
      or (k = 0) then
      begin
        Result := XAnim^.Keys[k].KeyMatrix;
        Exit;
      end;
      k0 := k - 1;
      g := (FrameTime - XAnim^.Keys[k0].Time) / (XAnim^.Keys[k].Time - XAnim^.Keys[k0].Time);
      Result := G2MatSLerp(XAnim^.Keys[k0].KeyMatrix, XAnim^.Keys[k].KeyMatrix, g);
      Exit;
    end;
    Result := XAnim^.Keys[XAnim^.KeyCount - 1].KeyMatrix;
  end;
var
  i, k, f, j, b, v: Integer;
  m: TG2Mat;
  Mesh, TmpMesh: ID3DXMesh;
  VertexRemaps: array of array of DWord;
  Decl: TFVFDeclaration;
  Vertices: PGeomVertexArray;
  Ptr: Pointer;
  Adj: array of DWord;
  VertexMapping: ID3DXBuffer;
begin
  G2MeshDataClear(MeshData);
  Decl[0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
  Decl[1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
  Decl[2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
  Decl[3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
  Decl[4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
  Decl[5] := D3DDECL_END;
  MeshData^.NodeCount := Length(m_XFrames);
  SetLength(MeshData^.Nodes, MeshData^.NodeCount);
  for i := 0 to MeshData^.NodeCount - 1 do
  begin
    MeshData^.Nodes[i].OwnerID := m_XFrames[i].ParentID;
    MeshData^.Nodes[i].Name := m_XFrames[i].Name;
    MeshData^.Nodes[i].Transform := m_XFrames[i].Transform;
  end;
  MeshData^.AnimCount := Length(m_XAnimSets);
  SetLength(MeshData^.Anims, MeshData^.AnimCount);
  for i := 0 to MeshData^.AnimCount - 1 do
  begin
    MeshData^.Anims[i].Name := m_XAnimSets[i].Name;
    MeshData^.Anims[i].FrameCount := m_XAnimSets[i].FrameCount;
    MeshData^.Anims[i].NodeCount := Length(m_XAnimSets[i].XAnims);
    MeshData^.Anims[i].FrameRate := 30;
    SetLength(MeshData^.Anims[i].Nodes, MeshData^.Anims[i].NodeCount);
    for j := 0 to High(MeshData^.Anims[i].Nodes) do
    begin
      MeshData^.Anims[i].Nodes[j].NodeID := m_XAnimSets[i].XAnims[j].FrameID;
      SetLength(MeshData^.Anims[i].Nodes[j].Frames, MeshData^.Anims[i].FrameCount);
      //for k := 0 to m_XAnimSets[i].XAnims[j].KeyCount - 1 do
      //MeshData^.Anims[i].Nodes[j].Frames[k]
      //SetLength(MeshData^.Anims[i].Nodes[j].Frames, m_XAnimSets[i].XAnims[j].KeyCount);
      for k := 0 to High(MeshData^.Anims[i].Nodes[j].Frames) do
      begin
        m := GetKeyAtTime(
          @m_XAnimSets[i].XAnims[j], k
        );
        m.Decompose(
          @MeshData^.Anims[i].Nodes[j].Frames[k].Scaling,
          @MeshData^.Anims[i].Nodes[j].Frames[k].Rotation,
          @MeshData^.Anims[i].Nodes[j].Frames[k].Translation
        );
      end;
    end;
  end;
  MeshData^.GeomCount := Length(m_XMeshes);
  SetLength(VertexRemaps, MeshData^.GeomCount);
  SetLength(MeshData^.Geoms, MeshData^.GeomCount);
  for i := 0 to MeshData^.GeomCount - 1 do
  begin
    MeshData^.Geoms[i].NodeID := m_XMeshes[i].XFrameID;
    MeshData^.Geoms[i].SkinID := m_XMeshes[i].XSkinID;
    MeshData^.Geoms[i].VCount := m_XMeshes[i].VCount;
    MeshData^.Geoms[i].FCount := m_XMeshes[i].FCount;
    MeshData^.Geoms[i].TCount := 1;
    D3DXCreateMesh(
      MeshData^.Geoms[i].FCount,
      MeshData^.Geoms[i].VCount,
      D3DXMESH_SYSTEMMEM,
      @Decl[0],
      Device,
      Mesh
    );
    Mesh.LockVertexBuffer(0, Pointer(Vertices));
    for j := 0 to MeshData^.Geoms[i].VCount - 1 do
    begin
      Vertices^[j].Position := m_XMeshes[i].Vertices[j].Position;
      Vertices^[j].Tangent.SetValue(0, 0, 0);
      Vertices^[j].Binormal.SetValue(0, 0, 0);
      Vertices^[j].Normal := m_XMeshes[i].Vertices[j].Normal;
      Vertices^[j].TexCoords := m_XMeshes[i].Vertices[j].TexCoord;
    end;
    Mesh.UnlockVertexBuffer;
    Mesh.LockIndexBuffer(0, Ptr);
    for j := 0 to MeshData^.Geoms[i].FCount * 3 - 1 do
    PWordArray(Ptr)^[j] := m_XMeshes[i].Indices[j];
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
    for j := 0 to MeshData^.Geoms[i].VCount - 1 do
    begin
      MeshData^.Geoms[i].Vertices[j].Position := Vertices^[j].Position;
      MeshData^.Geoms[i].Vertices[j].Tangent := Vertices^[j].Tangent;
      MeshData^.Geoms[i].Vertices[j].Binormal := Vertices^[j].Binormal;
      MeshData^.Geoms[i].Vertices[j].Normal := Vertices^[j].Normal;
      SetLength(MeshData^.Geoms[i].Vertices[j].TexCoords, 1);
      MeshData^.Geoms[i].Vertices[j].TexCoords[0] := Vertices^[j].TexCoords;
    end;
    Mesh.UnlockVertexBuffer;
    SafeRelease(Mesh);
    SetLength(MeshData^.Geoms[i].Faces, MeshData^.Geoms[i].FCount);
    for j := 0 to MeshData^.Geoms[i].FCount - 1 do
    begin
      MeshData^.Geoms[i].Faces[j].Indices[0] := m_XMeshes[i].Indices[j * 3 + 0];
      MeshData^.Geoms[i].Faces[j].Indices[1] := m_XMeshes[i].Indices[j * 3 + 1];
      MeshData^.Geoms[i].Faces[j].Indices[2] := m_XMeshes[i].Indices[j * 3 + 2];
      MeshData^.Geoms[i].Faces[j].MaterialID := 0;
    end;
    MeshData^.Geoms[i].MCount := 1;
    SetLength(MeshData^.Geoms[i].Materials, MeshData^.Geoms[i].MCount);
    MeshData^.Geoms[i].Materials[0] := 0;
  end;
  MeshData^.SkinCount := Length(m_XSkins);
  SetLength(MeshData^.Skins, MeshData^.SkinCount);
  for i := 0 to MeshData^.SkinCount - 1 do
  begin
    MeshData^.Skins[i].GeomID := m_XSkins[i].MeshID;
    MeshData^.Skins[i].BoneCount := m_XSkins[i].FrameCount;
    SetLength(MeshData^.Skins[i].Bones, MeshData^.Skins[i].BoneCount);
    for b := 0 to MeshData^.Skins[i].BoneCount - 1 do
    begin
      MeshData^.Skins[i].Bones[b].NodeID := m_XSkins[i].Frames[b].FrameID;
      MeshData^.Skins[i].Bones[b].Bind := m_XSkins[i].Frames[b].Bind;
    end;
    SetLength(MeshData^.Skins[i].Vertices, MeshData^.Geoms[m_XSkins[i].MeshID].VCount);
    for v := 0 to High(MeshData^.Skins[i].Vertices) do
    begin
      MeshData^.Skins[i].Vertices[v].WeightCount := m_XSkins[i].Vertices[VertexRemaps[m_XSkins[i].MeshID][v]].FrameCount;
      SetLength(
        MeshData^.Skins[i].Vertices[v].Weights,
        MeshData^.Skins[i].Vertices[v].WeightCount
      );
      for j := 0 to MeshData^.Skins[i].Vertices[v].WeightCount - 1 do
      begin
        MeshData^.Skins[i].Vertices[v].Weights[j].BoneID := m_XSkins[i].Vertices[VertexRemaps[m_XSkins[i].MeshID][v]].FrameWeights[j].FrameID;
        MeshData^.Skins[i].Vertices[v].Weights[j].Weight := m_XSkins[i].Vertices[VertexRemaps[m_XSkins[i].MeshID][v]].FrameWeights[j].FrameWeight;
      end;
    end;
  end;
  MeshData^.MaterialCount := 1;
  SetLength(MeshData^.Materials, MeshData^.MaterialCount);
  MeshData^.Materials[0].ChannelCount := 1;
  SetLength(MeshData^.Materials[0].Channels, MeshData^.Materials[0].ChannelCount);
  MeshData^.Materials[0].Channels[0].Name := 'Default';
  MeshData^.RagDollCount := 0;
  MeshData^.RagDolls := nil;
end;

procedure TG2MeshLoaderX.LoadStream(const s: TStream);
var
  f: ID3DXFile;
  Enum: ID3DXFileEnumObject;
  Data: ID3DXFileData;
  ms: TMemoryStream;
  Mem: TD3DXFFileLoadMemory;
  ChildCount: DWord;
  i, j: Integer;
  AutoRootNodeID: Integer;
  XData: TXDataControl;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromStream(s);
  Mem.lpMemory := ms.Memory;
  Mem.dSize := ms.Size;
  D3DXFileCreate(f);
  f.RegisterTemplates(@XTemplates[1], Length(XTemplates));
  f.CreateEnumObject(@Mem, D3DXF_FILELOAD_FROMMEMORY, Enum);
  ms.Free;
  f.RegisterEnumTemplates(Enum);
  m_CurFrame := nil;
  Enum.GetChildren(ChildCount);

  m_TicksPerSecond := 4800;
  for i := 0 to ChildCount - 1 do
  begin
    Enum.GetChild(i, Data);
    if CheckDataType(Data, GUID_TemplateAnimTicksPerSecond) then
    begin
      XData := TXDataControl.Create;
      XData.Data := Data;
      m_TicksPerSecond := XData.ReadDWord;
      XData.Free;
    end;
  end;

  for i := 0 to ChildCount - 1 do
  begin
    Enum.GetChild(i, Data);
    if CheckDataType(Data, GUID_TemplateFrame) then
    LoadXFrame(Data);
  end;

  for i := 0 to ChildCount - 1 do
  begin
    Enum.GetChild(i, Data);
    if CheckDataType(Data, GUID_TemplateAnimationSet) then
    LoadXAnimSet(Data);
  end;

  for i := 0 to ChildCount - 1 do
  begin
    Enum.GetChild(i, Data);
    if CheckDataType(Data, GUID_TemplateMesh) then
    begin
      AutoRootNodeID := Length(m_XFrames);
      SetLength(m_XFrames, AutoRootNodeID + 1);
      m_XFrames[AutoRootNodeID].xid := GUID_NULL;
      m_XFrames[AutoRootNodeID].Name := '';
      m_XFrames[AutoRootNodeID].Index := AutoRootNodeID;
      m_XFrames[AutoRootNodeID].Transform.SetIdentity;
      m_XFrames[AutoRootNodeID].ParentID := -1;
      m_XFrames[AutoRootNodeID].MeshID := -1;
      LoadXMesh(Data, AutoRootNodeID);
    end;
  end;

  for i := 0 to High(m_XSkins) do
  for j := 0 to m_XSkins[i].FrameCount - 1 do
  m_XSkins[i].Frames[j].FrameID := GetFrameIDByName(m_XSkins[i].Frames[j].FrameName);

  {$IFDEF G2_WRITE_LOG}
  for i := 0 to ChildCount - 1 do
  begin
    Enum.GetChild(i, Data);
    if not (
      CheckDataType(Data, GUID_TemplateFrame)
      or CheckDataType(Data, GUID_TemplateAnimationSet)
      or CheckDataType(Data, GUID_TemplateMesh)
    ) then
    G2WriteLogTimed(
      '(W) Root template ignored: ' +
      GetTemplateName(Data),
      'LoaderX'
    );
  end;
  {$ENDIF}
  NormalizeSkins;
end;

class function TG2MeshLoaderX.GetDifinition: AnsiString;
begin
  Result := 'DirectX Mesh Loader';
end;

class function TG2MeshLoaderX.CanLoadStream(const s: TStream): Boolean;
var
  Def: AnsiString;
  Pos: Int64;
begin
  Pos := s.Position;
  SetLength(Def, 4);
  s.Read(Def[1], 4);
  s.Position := Pos;
  Result := UpperCase(String(Def)) = 'XOF ';
end;
//TG2MeshLoaderX END

//TXDataControl BEGIN
constructor TXDataControl.Create;
begin
  inherited Create;
  m_Locked := False;
  m_Pos := 0;
  m_LockSize := 0;
end;

destructor TXDataControl.Destroy;
begin
  if m_Locked then Unlock;
  SafeRelease(m_Data);
  inherited Destroy;
end;

procedure TXDataControl.SetData(const Value: ID3DXFileData);
begin
  Unlock;
  SafeRelease(m_Data);
  m_Pos := 0;
  m_LockSize := 0;
  m_Data := Value;
end;

procedure TXDataControl.Lock(const Pos, Size: DWord);
begin
  if not Assigned(m_Data) then Exit;
  if m_Locked then
  begin
    if (m_LockSize >= Pos + Size) then
    Exit
    else
    Unlock;
  end;
  m_LockSize := Pos + Size;
  m_Locked := Succeeded(m_Data.Lock(@m_LockSize, Pointer(m_pb)));
end;

procedure TXDataControl.Lock(const Size: DWord);
begin
  Lock(m_Pos, Size);
end;

procedure TXDataControl.Unlock;
begin
  if not Assigned(m_Data) then Exit;
  if not m_Locked then Exit;
  Data.Unlock;
  m_Locked := False;
end;

procedure TXDataControl.Skip(const Offset: Integer);
begin
  m_Pos := m_Pos + Offset;
end;

procedure TXDataControl.ReadBuffer(var i; const Size: Integer);
begin
  if not Assigned(m_Data) then Exit;
  Lock(m_Pos, Size);
  Move(m_pb^[m_Pos], i, Size);
  m_Pos := m_Pos + Size;
end;

function TXDataControl.ReadWord: Word;
begin
  ReadBuffer(Result, 2);
end;

function TXDataControl.ReadDWord: DWord;
begin
  ReadBuffer(Result, 4);
end;

function TXDataControl.ReadFloat: Single;
begin
  ReadBuffer(Result, 4);
end;

function TXDataControl.ReadString: AnsiString;
var
  c: AnsiChar;
begin
  Result := '';
  ReadBuffer(c, 1);
  while c <> #0 do
  begin
    Result := Result + c;
    ReadBuffer(c, 1);
  end;
end;

function TXDataControl.ReadVec3: TG2Vec3;
begin
  ReadBuffer(Result, 3 * 4);
end;

function TXDataControl.ReadVec2: TG2Vec2;
begin
  ReadBuffer(Result, 2 * 4);
end;

function TXDataControl.ReadQuat: TG2Quat;
begin
  Lock(16);
  ReadBuffer(Result.w, 4);
  Result.w := -Result.w;
  ReadBuffer(Result.x, 4 * 3);
end;

function TXDataControl.ReadMat: TG2Mat;
begin
  ReadBuffer(Result, 16 * 4);
end;
//TXDataControl END

initialization
  G2RegMeshLoader(TG2MeshLoaderX);

end.