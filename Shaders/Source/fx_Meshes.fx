/*--------------------------------*\
		Gen2 Mesh Shaders
\*--------------------------------*/

static const int MAX_LIGHT_COUNT_SM2_POINT = 3;
static const int MAX_LIGHT_COUNT_SM2_DIR = 2;
static const int MAX_LIGHT_COUNT = 8;
static const int MAX_ANISOTROPY = 4;
static const int MAX_BONE_COUNT = 80;

float4x3 WV;
float4x4 WVP;
float4x3 SkinPallete[MAX_BONE_COUNT];

vector LightAmbient;

uniform int LightShaderIndex;

int LightCountSpot;
float3 LightPosSpot[MAX_LIGHT_COUNT];
float3 LightDirSpot[MAX_LIGHT_COUNT];
float3 LightColorSpot[MAX_LIGHT_COUNT];
float4 LightRangeSpot[MAX_LIGHT_COUNT];

int LightCountPoint;
float3 LightPosPoint[MAX_LIGHT_COUNT];
float3 LightColorPoint[MAX_LIGHT_COUNT];
float LightRangePoint[MAX_LIGHT_COUNT];

int LightCountDirectional;
float3 LightDirDirectional[MAX_LIGHT_COUNT];
float3 LightColorDirectional[MAX_LIGHT_COUNT];

uniform int MaxBoneWeights;

texture TexDiffuse;
texture TexSpecular;
texture TexNormals;

sampler SampDiffuse = sampler_state {
  Texture = <TexDiffuse>;
  MaxAnisotropy = MAX_ANISOTROPY;
  MinFilter = Anisotropic;  
  MagFilter = Anisotropic;
  MipFilter = Anisotropic;
  AddressU  = Wrap;
  AddressV  = Wrap;
};

sampler SampSpecular = sampler_state {
  Texture = <TexSpecular>;
	MaxAnisotropy = MAX_ANISOTROPY;
  MinFilter = Anisotropic;  
  MagFilter = Anisotropic;
  MipFilter = Anisotropic;
  AddressU  = Wrap;
  AddressV  = Wrap;
};

sampler SampNormals = sampler_state {
  Texture = <TexNormals>;
	MaxAnisotropy = MAX_ANISOTROPY;
  MinFilter = Anisotropic;  
  MagFilter = Anisotropic;
  MipFilter = Anisotropic;
  AddressU  = Wrap;
  AddressV  = Wrap;
};

void VShader0SM3 (
  const in float3 InPosition0: Position0,
	const in float3 InTangent0: Tangent0,
	const in float3 InBinormal0: Binormal0,
	const in float3 InNormal0: Normal0,
	const in float2 InTexCoord0: TexCoord0,
  out float4 OutPosition0: Position0,
	out float2 OutTexCoord0: TexCoord0,
	out float3 OutWorldPosition: TexCoord1,
	out float3 OutWorldTangent: TexCoord2,
	out float3 OutWorldBinormal: TexCoord3,
	out float3 OutWorldNormal: TexCoord4
) {
	OutPosition0 = mul(float4(InPosition0, 1), WVP);
	OutTexCoord0 = (float2)InTexCoord0;
	OutWorldPosition = mul(float4(InPosition0, 1), (float4x3)WV);
	OutWorldTangent = mul(InTangent0, (float3x3)WV);
	OutWorldBinormal = mul(InBinormal0, (float3x3)WV);
	OutWorldNormal = mul(InNormal0, (float3x3)WV);
}

void VShader1SM3 (
  const in float3 InPosition0: Position0,
	const in float3 InTangent0: Tangent0,
	const in float3 InBinormal0: Binormal0,
	const in float3 InNormal0: Normal0,
	const in float2 InTexCoord0: TexCoord0,
	const in int InBIndices0: BlendIndices0, 
  out float4 OutPosition0: Position0,
	out float2 OutTexCoord0: TexCoord0,
	out float3 OutWorldPosition: TexCoord1,
	out float3 OutWorldTangent: TexCoord2,
	out float3 OutWorldBinormal: TexCoord3,
	out float3 OutWorldNormal: TexCoord4
) {
	OutTexCoord0 = InTexCoord0;
	float4x3 FinalMatrix = SkinPallete[InBIndices0];
	float4 SkinPos = float4(mul(float4(InPosition0, 1), FinalMatrix), 1);
	OutPosition0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	OutWorldPosition = mul(SkinPos, (float4x3)WV);
	OutWorldTangent = mul(InTangent0, (float3x3)FinalMatrix);
	OutWorldBinormal = mul(InBinormal0, (float3x3)FinalMatrix);
	OutWorldNormal = mul(InNormal0, (float3x3)FinalMatrix);
}

void VShader2SM3 (
  const in float3 InPosition0: Position0,
	const in float3 InTangent0: Tangent0,
	const in float3 InBinormal0: Binormal0,
	const in float3 InNormal0: Normal0,
	const in float2 InTexCoord0: TexCoord0,
	const in int2 InBIndices0: BlendIndices0,
	const in float2 InBWeights0: BlendWeight0,
  out float4 OutPosition0: Position0,
	out float2 OutTexCoord0: TexCoord0,
	out float3 OutWorldPosition: TexCoord1,
	out float3 OutWorldTangent: TexCoord2,
	out float3 OutWorldBinormal: TexCoord3,
	out float3 OutWorldNormal: TexCoord4
) {
	OutTexCoord0 = InTexCoord0;
	float4x3 FinalMatrix = (
		SkinPallete[InBIndices0[0]] * InBWeights0[0] +
		SkinPallete[InBIndices0[1]] * InBWeights0[1]
	);
	float4 SkinPos = float4(mul(float4(InPosition0, 1), FinalMatrix), 1);
	OutPosition0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	OutWorldPosition = mul(SkinPos, (float4x3)WV);
	OutWorldTangent = mul(InTangent0, (float3x3)FinalMatrix);
	OutWorldBinormal = mul(InBinormal0, (float3x3)FinalMatrix);
	OutWorldNormal = mul(InNormal0, (float3x3)FinalMatrix);
}

void VShader3SM3 (
  const in float3 InPosition0: Position0,
	const in float3 InTangent0: Tangent0,
	const in float3 InBinormal0: Binormal0,
	const in float3 InNormal0: Normal0,
	const in float2 InTexCoord0: TexCoord0,
	const in int3 InBIndices0: BlendIndices0,
	const in float3 InBWeights0: BlendWeight0,
  out float4 OutPosition0: Position0,
	out float2 OutTexCoord0: TexCoord0,
	out float3 OutWorldPosition: TexCoord1,
	out float3 OutWorldTangent: TexCoord2,
	out float3 OutWorldBinormal: TexCoord3,
	out float3 OutWorldNormal: TexCoord4
) {
	OutTexCoord0 = InTexCoord0;
	float4x3 FinalMatrix = (
		SkinPallete[InBIndices0[0]] * InBWeights0[0] +
		SkinPallete[InBIndices0[1]] * InBWeights0[1] +        
		SkinPallete[InBIndices0[2]] * InBWeights0[2]
	);
	float4 SkinPos = float4(mul(float4(InPosition0, 1), FinalMatrix), 1);
	OutPosition0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	OutWorldPosition = mul(SkinPos, (float4x3)WV);
	OutWorldTangent = mul(InTangent0, (float3x3)FinalMatrix);
	OutWorldBinormal = mul(InBinormal0, (float3x3)FinalMatrix);
	OutWorldNormal = mul(InNormal0, (float3x3)FinalMatrix);
}

void VShader4SM3 (
  const in float3 InPosition0: Position0,
	const in float3 InTangent0: Tangent0,
	const in float3 InBinormal0: Binormal0,
	const in float3 InNormal0: Normal0,
	const in float2 InTexCoord0: TexCoord0,
	const in int4 InBIndices0: BlendIndices0,
	const in float4 InBWeights0: BlendWeight0,
  out float4 OutPosition0: Position0,
	out float2 OutTexCoord0: TexCoord0,
	out float3 OutWorldPosition: TexCoord1,
	out float3 OutWorldTangent: TexCoord2,
	out float3 OutWorldBinormal: TexCoord3,
	out float3 OutWorldNormal: TexCoord4
) {
	OutTexCoord0 = InTexCoord0;
	float4x3 FinalMatrix = (
		SkinPallete[InBIndices0[0]] * InBWeights0[0] +
		SkinPallete[InBIndices0[1]] * InBWeights0[1] +
		SkinPallete[InBIndices0[2]] * InBWeights0[2] +
		SkinPallete[InBIndices0[3]] * InBWeights0[3]
	);
	float4 SkinPos = float4(mul(float4(InPosition0, 1), FinalMatrix), 1);
	OutPosition0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	OutWorldPosition = mul(SkinPos, (float4x3)WV);
	OutWorldTangent = mul(InTangent0, (float3x3)FinalMatrix);
	OutWorldBinormal = mul(InBinormal0, (float3x3)FinalMatrix);
	OutWorldNormal = mul(InNormal0, (float3x3)FinalMatrix);
}

void PShaderSM3 (
  const in float4 InPosition0: Position0,
	const in float2 InTexCoord0: TexCoord0,
	const in float3 InWorldPosition: TexCoord1,
	const in float3 InWorldTangent: TexCoord2,
	const in float3 InWorldBinormal: TexCoord3,
	const in float3 InWorldNormal: TexCoord4,
  uniform int LCPoint, 
  uniform int LCDir,
  uniform int LCSpot,
  out float4 OutColor: Color0
) {
	float3 CamDir = normalize(InWorldPosition);
	float3 WorldTangent = normalize(InWorldTangent);
	float3 WorldBinormal = normalize(InWorldBinormal);
	float3 WorldNormal = normalize(InWorldNormal);
	float3x3 TBN = float3x3(WorldTangent, WorldBinormal, WorldNormal);
	float3 Normal = normalize(mul(((float3)tex2D(SampNormals, InTexCoord0)) * 2  - 1, (float3x3) TBN));
	float3 RefDir = CamDir - 2 * Normal * dot(CamDir, Normal);
	float3 Dif = (0, 0, 0);
	float3 Spec = (0, 0, 0);
	float Att;
	float3 VertexToLight;
	for (int i = 0; i < LCPoint; i++) {
		VertexToLight = LightPosPoint[i] - InWorldPosition;
		Att = clamp(1 - dot(VertexToLight, VertexToLight) / LightRangePoint[i], 0, 1);
		VertexToLight = normalize(VertexToLight);
		Dif += saturate(dot(VertexToLight, Normal) * LightColorPoint[i]) * Att;
		Spec += saturate(dot(VertexToLight, RefDir) * LightColorPoint[i]) * Att;
	}
	for (int i = 0; i < LCDir; i++) {
		Dif += saturate(dot(LightDirDirectional[i], Normal) * LightColorDirectional[i]); 
		Spec += saturate(dot(LightDirDirectional[i], RefDir) * LightColorDirectional[i]); 
	}
  for (int i = 0; i < LCSpot; i++) {
    VertexToLight = LightPosSpot[i] - InWorldPosition;
    float VertexToLightLenSq = dot(VertexToLight, VertexToLight);
    VertexToLight = normalize(VertexToLight);
    Att = smoothstep(LightRangeSpot[i].y, LightRangeSpot[i].x, VertexToLightLenSq) * smoothstep(LightRangeSpot[i].w, LightRangeSpot[i].z, dot(-VertexToLight, LightDirSpot[i]));    
    Dif += saturate(dot(VertexToLight, Normal) * LightColorSpot[i]) * Att;
		Spec += saturate(dot(VertexToLight, RefDir) * LightColorSpot[i]) * Att;
  }
	float4 TexDiffuse = tex2D(SampDiffuse, InTexCoord0);
	float4 TexSpecular = tex2D(SampSpecular, InTexCoord0);
	OutColor = TexDiffuse * float4(saturate(Dif) + LightAmbient, 1) + float4(pow(saturate(Spec), 5), 0) * TexSpecular;
	OutColor.w = TexDiffuse.w;
}

VertexShader VShaderSkinnedSM3Arr[5] = {
  compile vs_3_0 VShader0SM3(),
	compile vs_3_0 VShader1SM3(), 
  compile vs_3_0 VShader2SM3(),
  compile vs_3_0 VShader3SM3(),
  compile vs_3_0 VShader4SM3()
};

PixelShader PShaderSM3Arr[729] = {
	compile ps_3_0 PShaderSM3(0, 0, 0),
	compile ps_3_0 PShaderSM3(0, 1, 0),
	compile ps_3_0 PShaderSM3(0, 2, 0),
	compile ps_3_0 PShaderSM3(0, 3, 0),
	compile ps_3_0 PShaderSM3(0, 4, 0),
	compile ps_3_0 PShaderSM3(0, 5, 0),
	compile ps_3_0 PShaderSM3(0, 6, 0),
	compile ps_3_0 PShaderSM3(0, 7, 0),
	compile ps_3_0 PShaderSM3(0, 8, 0),
	compile ps_3_0 PShaderSM3(1, 0, 0),
	compile ps_3_0 PShaderSM3(1, 1, 0),
	compile ps_3_0 PShaderSM3(1, 2, 0),
	compile ps_3_0 PShaderSM3(1, 3, 0),
	compile ps_3_0 PShaderSM3(1, 4, 0),
	compile ps_3_0 PShaderSM3(1, 5, 0),
	compile ps_3_0 PShaderSM3(1, 6, 0),
	compile ps_3_0 PShaderSM3(1, 7, 0),
	compile ps_3_0 PShaderSM3(1, 8, 0),
	compile ps_3_0 PShaderSM3(2, 0, 0),
	compile ps_3_0 PShaderSM3(2, 1, 0),
	compile ps_3_0 PShaderSM3(2, 2, 0),
	compile ps_3_0 PShaderSM3(2, 3, 0),
	compile ps_3_0 PShaderSM3(2, 4, 0),
	compile ps_3_0 PShaderSM3(2, 5, 0),
	compile ps_3_0 PShaderSM3(2, 6, 0),
	compile ps_3_0 PShaderSM3(2, 7, 0),
	compile ps_3_0 PShaderSM3(2, 8, 0),
	compile ps_3_0 PShaderSM3(3, 0, 0),
	compile ps_3_0 PShaderSM3(3, 1, 0),
	compile ps_3_0 PShaderSM3(3, 2, 0),
	compile ps_3_0 PShaderSM3(3, 3, 0),
	compile ps_3_0 PShaderSM3(3, 4, 0),
	compile ps_3_0 PShaderSM3(3, 5, 0),
	compile ps_3_0 PShaderSM3(3, 6, 0),
	compile ps_3_0 PShaderSM3(3, 7, 0),
	compile ps_3_0 PShaderSM3(3, 8, 0),
	compile ps_3_0 PShaderSM3(4, 0, 0),
	compile ps_3_0 PShaderSM3(4, 1, 0),
	compile ps_3_0 PShaderSM3(4, 2, 0),
	compile ps_3_0 PShaderSM3(4, 3, 0),
	compile ps_3_0 PShaderSM3(4, 4, 0),
	compile ps_3_0 PShaderSM3(4, 5, 0),
	compile ps_3_0 PShaderSM3(4, 6, 0),
	compile ps_3_0 PShaderSM3(4, 7, 0),
	compile ps_3_0 PShaderSM3(4, 8, 0),
	compile ps_3_0 PShaderSM3(5, 0, 0),
	compile ps_3_0 PShaderSM3(5, 1, 0),
	compile ps_3_0 PShaderSM3(5, 2, 0),
	compile ps_3_0 PShaderSM3(5, 3, 0),
	compile ps_3_0 PShaderSM3(5, 4, 0),
	compile ps_3_0 PShaderSM3(5, 5, 0),
	compile ps_3_0 PShaderSM3(5, 6, 0),
	compile ps_3_0 PShaderSM3(5, 7, 0),
	compile ps_3_0 PShaderSM3(5, 8, 0),
	compile ps_3_0 PShaderSM3(6, 0, 0),
	compile ps_3_0 PShaderSM3(6, 1, 0),
	compile ps_3_0 PShaderSM3(6, 2, 0),
	compile ps_3_0 PShaderSM3(6, 3, 0),
	compile ps_3_0 PShaderSM3(6, 4, 0),
	compile ps_3_0 PShaderSM3(6, 5, 0),
	compile ps_3_0 PShaderSM3(6, 6, 0),
	compile ps_3_0 PShaderSM3(6, 7, 0),
	compile ps_3_0 PShaderSM3(6, 8, 0),
	compile ps_3_0 PShaderSM3(7, 0, 0),
	compile ps_3_0 PShaderSM3(7, 1, 0),
	compile ps_3_0 PShaderSM3(7, 2, 0),
	compile ps_3_0 PShaderSM3(7, 3, 0),
	compile ps_3_0 PShaderSM3(7, 4, 0),
	compile ps_3_0 PShaderSM3(7, 5, 0),
	compile ps_3_0 PShaderSM3(7, 6, 0),
	compile ps_3_0 PShaderSM3(7, 7, 0),
	compile ps_3_0 PShaderSM3(7, 8, 0),
	compile ps_3_0 PShaderSM3(8, 0, 0),
	compile ps_3_0 PShaderSM3(8, 1, 0),
	compile ps_3_0 PShaderSM3(8, 2, 0),
	compile ps_3_0 PShaderSM3(8, 3, 0),
	compile ps_3_0 PShaderSM3(8, 4, 0),
	compile ps_3_0 PShaderSM3(8, 5, 0),
	compile ps_3_0 PShaderSM3(8, 6, 0),
	compile ps_3_0 PShaderSM3(8, 7, 0),
	compile ps_3_0 PShaderSM3(8, 8, 0),
	compile ps_3_0 PShaderSM3(0, 0, 1),
	compile ps_3_0 PShaderSM3(0, 1, 1),
	compile ps_3_0 PShaderSM3(0, 2, 1),
	compile ps_3_0 PShaderSM3(0, 3, 1),
	compile ps_3_0 PShaderSM3(0, 4, 1),
	compile ps_3_0 PShaderSM3(0, 5, 1),
	compile ps_3_0 PShaderSM3(0, 6, 1),
	compile ps_3_0 PShaderSM3(0, 7, 1),
	compile ps_3_0 PShaderSM3(0, 8, 1),
	compile ps_3_0 PShaderSM3(1, 0, 1),
	compile ps_3_0 PShaderSM3(1, 1, 1),
	compile ps_3_0 PShaderSM3(1, 2, 1),
	compile ps_3_0 PShaderSM3(1, 3, 1),
	compile ps_3_0 PShaderSM3(1, 4, 1),
	compile ps_3_0 PShaderSM3(1, 5, 1),
	compile ps_3_0 PShaderSM3(1, 6, 1),
	compile ps_3_0 PShaderSM3(1, 7, 1),
	compile ps_3_0 PShaderSM3(1, 8, 1),
	compile ps_3_0 PShaderSM3(2, 0, 1),
	compile ps_3_0 PShaderSM3(2, 1, 1),
	compile ps_3_0 PShaderSM3(2, 2, 1),
	compile ps_3_0 PShaderSM3(2, 3, 1),
	compile ps_3_0 PShaderSM3(2, 4, 1),
	compile ps_3_0 PShaderSM3(2, 5, 1),
	compile ps_3_0 PShaderSM3(2, 6, 1),
	compile ps_3_0 PShaderSM3(2, 7, 1),
	compile ps_3_0 PShaderSM3(2, 8, 1),
	compile ps_3_0 PShaderSM3(3, 0, 1),
	compile ps_3_0 PShaderSM3(3, 1, 1),
	compile ps_3_0 PShaderSM3(3, 2, 1),
	compile ps_3_0 PShaderSM3(3, 3, 1),
	compile ps_3_0 PShaderSM3(3, 4, 1),
	compile ps_3_0 PShaderSM3(3, 5, 1),
	compile ps_3_0 PShaderSM3(3, 6, 1),
	compile ps_3_0 PShaderSM3(3, 7, 1),
	compile ps_3_0 PShaderSM3(3, 8, 1),
	compile ps_3_0 PShaderSM3(4, 0, 1),
	compile ps_3_0 PShaderSM3(4, 1, 1),
	compile ps_3_0 PShaderSM3(4, 2, 1),
	compile ps_3_0 PShaderSM3(4, 3, 1),
	compile ps_3_0 PShaderSM3(4, 4, 1),
	compile ps_3_0 PShaderSM3(4, 5, 1),
	compile ps_3_0 PShaderSM3(4, 6, 1),
	compile ps_3_0 PShaderSM3(4, 7, 1),
	compile ps_3_0 PShaderSM3(4, 8, 1),
	compile ps_3_0 PShaderSM3(5, 0, 1),
	compile ps_3_0 PShaderSM3(5, 1, 1),
	compile ps_3_0 PShaderSM3(5, 2, 1),
	compile ps_3_0 PShaderSM3(5, 3, 1),
	compile ps_3_0 PShaderSM3(5, 4, 1),
	compile ps_3_0 PShaderSM3(5, 5, 1),
	compile ps_3_0 PShaderSM3(5, 6, 1),
	compile ps_3_0 PShaderSM3(5, 7, 1),
	compile ps_3_0 PShaderSM3(5, 8, 1),
	compile ps_3_0 PShaderSM3(6, 0, 1),
	compile ps_3_0 PShaderSM3(6, 1, 1),
	compile ps_3_0 PShaderSM3(6, 2, 1),
	compile ps_3_0 PShaderSM3(6, 3, 1),
	compile ps_3_0 PShaderSM3(6, 4, 1),
	compile ps_3_0 PShaderSM3(6, 5, 1),
	compile ps_3_0 PShaderSM3(6, 6, 1),
	compile ps_3_0 PShaderSM3(6, 7, 1),
	compile ps_3_0 PShaderSM3(6, 8, 1),
	compile ps_3_0 PShaderSM3(7, 0, 1),
	compile ps_3_0 PShaderSM3(7, 1, 1),
	compile ps_3_0 PShaderSM3(7, 2, 1),
	compile ps_3_0 PShaderSM3(7, 3, 1),
	compile ps_3_0 PShaderSM3(7, 4, 1),
	compile ps_3_0 PShaderSM3(7, 5, 1),
	compile ps_3_0 PShaderSM3(7, 6, 1),
	compile ps_3_0 PShaderSM3(7, 7, 1),
	compile ps_3_0 PShaderSM3(7, 8, 1),
	compile ps_3_0 PShaderSM3(8, 0, 1),
	compile ps_3_0 PShaderSM3(8, 1, 1),
	compile ps_3_0 PShaderSM3(8, 2, 1),
	compile ps_3_0 PShaderSM3(8, 3, 1),
	compile ps_3_0 PShaderSM3(8, 4, 1),
	compile ps_3_0 PShaderSM3(8, 5, 1),
	compile ps_3_0 PShaderSM3(8, 6, 1),
	compile ps_3_0 PShaderSM3(8, 7, 1),
	compile ps_3_0 PShaderSM3(8, 8, 1),
	compile ps_3_0 PShaderSM3(0, 0, 2),
	compile ps_3_0 PShaderSM3(0, 1, 2),
	compile ps_3_0 PShaderSM3(0, 2, 2),
	compile ps_3_0 PShaderSM3(0, 3, 2),
	compile ps_3_0 PShaderSM3(0, 4, 2),
	compile ps_3_0 PShaderSM3(0, 5, 2),
	compile ps_3_0 PShaderSM3(0, 6, 2),
	compile ps_3_0 PShaderSM3(0, 7, 2),
	compile ps_3_0 PShaderSM3(0, 8, 2),
	compile ps_3_0 PShaderSM3(1, 0, 2),
	compile ps_3_0 PShaderSM3(1, 1, 2),
	compile ps_3_0 PShaderSM3(1, 2, 2),
	compile ps_3_0 PShaderSM3(1, 3, 2),
	compile ps_3_0 PShaderSM3(1, 4, 2),
	compile ps_3_0 PShaderSM3(1, 5, 2),
	compile ps_3_0 PShaderSM3(1, 6, 2),
	compile ps_3_0 PShaderSM3(1, 7, 2),
	compile ps_3_0 PShaderSM3(1, 8, 2),
	compile ps_3_0 PShaderSM3(2, 0, 2),
	compile ps_3_0 PShaderSM3(2, 1, 2),
	compile ps_3_0 PShaderSM3(2, 2, 2),
	compile ps_3_0 PShaderSM3(2, 3, 2),
	compile ps_3_0 PShaderSM3(2, 4, 2),
	compile ps_3_0 PShaderSM3(2, 5, 2),
	compile ps_3_0 PShaderSM3(2, 6, 2),
	compile ps_3_0 PShaderSM3(2, 7, 2),
	compile ps_3_0 PShaderSM3(2, 8, 2),
	compile ps_3_0 PShaderSM3(3, 0, 2),
	compile ps_3_0 PShaderSM3(3, 1, 2),
	compile ps_3_0 PShaderSM3(3, 2, 2),
	compile ps_3_0 PShaderSM3(3, 3, 2),
	compile ps_3_0 PShaderSM3(3, 4, 2),
	compile ps_3_0 PShaderSM3(3, 5, 2),
	compile ps_3_0 PShaderSM3(3, 6, 2),
	compile ps_3_0 PShaderSM3(3, 7, 2),
	compile ps_3_0 PShaderSM3(3, 8, 2),
	compile ps_3_0 PShaderSM3(4, 0, 2),
	compile ps_3_0 PShaderSM3(4, 1, 2),
	compile ps_3_0 PShaderSM3(4, 2, 2),
	compile ps_3_0 PShaderSM3(4, 3, 2),
	compile ps_3_0 PShaderSM3(4, 4, 2),
	compile ps_3_0 PShaderSM3(4, 5, 2),
	compile ps_3_0 PShaderSM3(4, 6, 2),
	compile ps_3_0 PShaderSM3(4, 7, 2),
	compile ps_3_0 PShaderSM3(4, 8, 2),
	compile ps_3_0 PShaderSM3(5, 0, 2),
	compile ps_3_0 PShaderSM3(5, 1, 2),
	compile ps_3_0 PShaderSM3(5, 2, 2),
	compile ps_3_0 PShaderSM3(5, 3, 2),
	compile ps_3_0 PShaderSM3(5, 4, 2),
	compile ps_3_0 PShaderSM3(5, 5, 2),
	compile ps_3_0 PShaderSM3(5, 6, 2),
	compile ps_3_0 PShaderSM3(5, 7, 2),
	compile ps_3_0 PShaderSM3(5, 8, 2),
	compile ps_3_0 PShaderSM3(6, 0, 2),
	compile ps_3_0 PShaderSM3(6, 1, 2),
	compile ps_3_0 PShaderSM3(6, 2, 2),
	compile ps_3_0 PShaderSM3(6, 3, 2),
	compile ps_3_0 PShaderSM3(6, 4, 2),
	compile ps_3_0 PShaderSM3(6, 5, 2),
	compile ps_3_0 PShaderSM3(6, 6, 2),
	compile ps_3_0 PShaderSM3(6, 7, 2),
	compile ps_3_0 PShaderSM3(6, 8, 2),
	compile ps_3_0 PShaderSM3(7, 0, 2),
	compile ps_3_0 PShaderSM3(7, 1, 2),
	compile ps_3_0 PShaderSM3(7, 2, 2),
	compile ps_3_0 PShaderSM3(7, 3, 2),
	compile ps_3_0 PShaderSM3(7, 4, 2),
	compile ps_3_0 PShaderSM3(7, 5, 2),
	compile ps_3_0 PShaderSM3(7, 6, 2),
	compile ps_3_0 PShaderSM3(7, 7, 2),
	compile ps_3_0 PShaderSM3(7, 8, 2),
	compile ps_3_0 PShaderSM3(8, 0, 2),
	compile ps_3_0 PShaderSM3(8, 1, 2),
	compile ps_3_0 PShaderSM3(8, 2, 2),
	compile ps_3_0 PShaderSM3(8, 3, 2),
	compile ps_3_0 PShaderSM3(8, 4, 2),
	compile ps_3_0 PShaderSM3(8, 5, 2),
	compile ps_3_0 PShaderSM3(8, 6, 2),
	compile ps_3_0 PShaderSM3(8, 7, 2),
	compile ps_3_0 PShaderSM3(8, 8, 2),
	compile ps_3_0 PShaderSM3(0, 0, 3),
	compile ps_3_0 PShaderSM3(0, 1, 3),
	compile ps_3_0 PShaderSM3(0, 2, 3),
	compile ps_3_0 PShaderSM3(0, 3, 3),
	compile ps_3_0 PShaderSM3(0, 4, 3),
	compile ps_3_0 PShaderSM3(0, 5, 3),
	compile ps_3_0 PShaderSM3(0, 6, 3),
	compile ps_3_0 PShaderSM3(0, 7, 3),
	compile ps_3_0 PShaderSM3(0, 8, 3),
	compile ps_3_0 PShaderSM3(1, 0, 3),
	compile ps_3_0 PShaderSM3(1, 1, 3),
	compile ps_3_0 PShaderSM3(1, 2, 3),
	compile ps_3_0 PShaderSM3(1, 3, 3),
	compile ps_3_0 PShaderSM3(1, 4, 3),
	compile ps_3_0 PShaderSM3(1, 5, 3),
	compile ps_3_0 PShaderSM3(1, 6, 3),
	compile ps_3_0 PShaderSM3(1, 7, 3),
	compile ps_3_0 PShaderSM3(1, 8, 3),
	compile ps_3_0 PShaderSM3(2, 0, 3),
	compile ps_3_0 PShaderSM3(2, 1, 3),
	compile ps_3_0 PShaderSM3(2, 2, 3),
	compile ps_3_0 PShaderSM3(2, 3, 3),
	compile ps_3_0 PShaderSM3(2, 4, 3),
	compile ps_3_0 PShaderSM3(2, 5, 3),
	compile ps_3_0 PShaderSM3(2, 6, 3),
	compile ps_3_0 PShaderSM3(2, 7, 3),
	compile ps_3_0 PShaderSM3(2, 8, 3),
	compile ps_3_0 PShaderSM3(3, 0, 3),
	compile ps_3_0 PShaderSM3(3, 1, 3),
	compile ps_3_0 PShaderSM3(3, 2, 3),
	compile ps_3_0 PShaderSM3(3, 3, 3),
	compile ps_3_0 PShaderSM3(3, 4, 3),
	compile ps_3_0 PShaderSM3(3, 5, 3),
	compile ps_3_0 PShaderSM3(3, 6, 3),
	compile ps_3_0 PShaderSM3(3, 7, 3),
	compile ps_3_0 PShaderSM3(3, 8, 3),
	compile ps_3_0 PShaderSM3(4, 0, 3),
	compile ps_3_0 PShaderSM3(4, 1, 3),
	compile ps_3_0 PShaderSM3(4, 2, 3),
	compile ps_3_0 PShaderSM3(4, 3, 3),
	compile ps_3_0 PShaderSM3(4, 4, 3),
	compile ps_3_0 PShaderSM3(4, 5, 3),
	compile ps_3_0 PShaderSM3(4, 6, 3),
	compile ps_3_0 PShaderSM3(4, 7, 3),
	compile ps_3_0 PShaderSM3(4, 8, 3),
	compile ps_3_0 PShaderSM3(5, 0, 3),
	compile ps_3_0 PShaderSM3(5, 1, 3),
	compile ps_3_0 PShaderSM3(5, 2, 3),
	compile ps_3_0 PShaderSM3(5, 3, 3),
	compile ps_3_0 PShaderSM3(5, 4, 3),
	compile ps_3_0 PShaderSM3(5, 5, 3),
	compile ps_3_0 PShaderSM3(5, 6, 3),
	compile ps_3_0 PShaderSM3(5, 7, 3),
	compile ps_3_0 PShaderSM3(5, 8, 3),
	compile ps_3_0 PShaderSM3(6, 0, 3),
	compile ps_3_0 PShaderSM3(6, 1, 3),
	compile ps_3_0 PShaderSM3(6, 2, 3),
	compile ps_3_0 PShaderSM3(6, 3, 3),
	compile ps_3_0 PShaderSM3(6, 4, 3),
	compile ps_3_0 PShaderSM3(6, 5, 3),
	compile ps_3_0 PShaderSM3(6, 6, 3),
	compile ps_3_0 PShaderSM3(6, 7, 3),
	compile ps_3_0 PShaderSM3(6, 8, 3),
	compile ps_3_0 PShaderSM3(7, 0, 3),
	compile ps_3_0 PShaderSM3(7, 1, 3),
	compile ps_3_0 PShaderSM3(7, 2, 3),
	compile ps_3_0 PShaderSM3(7, 3, 3),
	compile ps_3_0 PShaderSM3(7, 4, 3),
	compile ps_3_0 PShaderSM3(7, 5, 3),
	compile ps_3_0 PShaderSM3(7, 6, 3),
	compile ps_3_0 PShaderSM3(7, 7, 3),
	compile ps_3_0 PShaderSM3(7, 8, 3),
	compile ps_3_0 PShaderSM3(8, 0, 3),
	compile ps_3_0 PShaderSM3(8, 1, 3),
	compile ps_3_0 PShaderSM3(8, 2, 3),
	compile ps_3_0 PShaderSM3(8, 3, 3),
	compile ps_3_0 PShaderSM3(8, 4, 3),
	compile ps_3_0 PShaderSM3(8, 5, 3),
	compile ps_3_0 PShaderSM3(8, 6, 3),
	compile ps_3_0 PShaderSM3(8, 7, 3),
	compile ps_3_0 PShaderSM3(8, 8, 3),
	compile ps_3_0 PShaderSM3(0, 0, 4),
	compile ps_3_0 PShaderSM3(0, 1, 4),
	compile ps_3_0 PShaderSM3(0, 2, 4),
	compile ps_3_0 PShaderSM3(0, 3, 4),
	compile ps_3_0 PShaderSM3(0, 4, 4),
	compile ps_3_0 PShaderSM3(0, 5, 4),
	compile ps_3_0 PShaderSM3(0, 6, 4),
	compile ps_3_0 PShaderSM3(0, 7, 4),
	compile ps_3_0 PShaderSM3(0, 8, 4),
	compile ps_3_0 PShaderSM3(1, 0, 4),
	compile ps_3_0 PShaderSM3(1, 1, 4),
	compile ps_3_0 PShaderSM3(1, 2, 4),
	compile ps_3_0 PShaderSM3(1, 3, 4),
	compile ps_3_0 PShaderSM3(1, 4, 4),
	compile ps_3_0 PShaderSM3(1, 5, 4),
	compile ps_3_0 PShaderSM3(1, 6, 4),
	compile ps_3_0 PShaderSM3(1, 7, 4),
	compile ps_3_0 PShaderSM3(1, 8, 4),
	compile ps_3_0 PShaderSM3(2, 0, 4),
	compile ps_3_0 PShaderSM3(2, 1, 4),
	compile ps_3_0 PShaderSM3(2, 2, 4),
	compile ps_3_0 PShaderSM3(2, 3, 4),
	compile ps_3_0 PShaderSM3(2, 4, 4),
	compile ps_3_0 PShaderSM3(2, 5, 4),
	compile ps_3_0 PShaderSM3(2, 6, 4),
	compile ps_3_0 PShaderSM3(2, 7, 4),
	compile ps_3_0 PShaderSM3(2, 8, 4),
	compile ps_3_0 PShaderSM3(3, 0, 4),
	compile ps_3_0 PShaderSM3(3, 1, 4),
	compile ps_3_0 PShaderSM3(3, 2, 4),
	compile ps_3_0 PShaderSM3(3, 3, 4),
	compile ps_3_0 PShaderSM3(3, 4, 4),
	compile ps_3_0 PShaderSM3(3, 5, 4),
	compile ps_3_0 PShaderSM3(3, 6, 4),
	compile ps_3_0 PShaderSM3(3, 7, 4),
	compile ps_3_0 PShaderSM3(3, 8, 4),
	compile ps_3_0 PShaderSM3(4, 0, 4),
	compile ps_3_0 PShaderSM3(4, 1, 4),
	compile ps_3_0 PShaderSM3(4, 2, 4),
	compile ps_3_0 PShaderSM3(4, 3, 4),
	compile ps_3_0 PShaderSM3(4, 4, 4),
	compile ps_3_0 PShaderSM3(4, 5, 4),
	compile ps_3_0 PShaderSM3(4, 6, 4),
	compile ps_3_0 PShaderSM3(4, 7, 4),
	compile ps_3_0 PShaderSM3(4, 8, 4),
	compile ps_3_0 PShaderSM3(5, 0, 4),
	compile ps_3_0 PShaderSM3(5, 1, 4),
	compile ps_3_0 PShaderSM3(5, 2, 4),
	compile ps_3_0 PShaderSM3(5, 3, 4),
	compile ps_3_0 PShaderSM3(5, 4, 4),
	compile ps_3_0 PShaderSM3(5, 5, 4),
	compile ps_3_0 PShaderSM3(5, 6, 4),
	compile ps_3_0 PShaderSM3(5, 7, 4),
	compile ps_3_0 PShaderSM3(5, 8, 4),
	compile ps_3_0 PShaderSM3(6, 0, 4),
	compile ps_3_0 PShaderSM3(6, 1, 4),
	compile ps_3_0 PShaderSM3(6, 2, 4),
	compile ps_3_0 PShaderSM3(6, 3, 4),
	compile ps_3_0 PShaderSM3(6, 4, 4),
	compile ps_3_0 PShaderSM3(6, 5, 4),
	compile ps_3_0 PShaderSM3(6, 6, 4),
	compile ps_3_0 PShaderSM3(6, 7, 4),
	compile ps_3_0 PShaderSM3(6, 8, 4),
	compile ps_3_0 PShaderSM3(7, 0, 4),
	compile ps_3_0 PShaderSM3(7, 1, 4),
	compile ps_3_0 PShaderSM3(7, 2, 4),
	compile ps_3_0 PShaderSM3(7, 3, 4),
	compile ps_3_0 PShaderSM3(7, 4, 4),
	compile ps_3_0 PShaderSM3(7, 5, 4),
	compile ps_3_0 PShaderSM3(7, 6, 4),
	compile ps_3_0 PShaderSM3(7, 7, 4),
	compile ps_3_0 PShaderSM3(7, 8, 4),
	compile ps_3_0 PShaderSM3(8, 0, 4),
	compile ps_3_0 PShaderSM3(8, 1, 4),
	compile ps_3_0 PShaderSM3(8, 2, 4),
	compile ps_3_0 PShaderSM3(8, 3, 4),
	compile ps_3_0 PShaderSM3(8, 4, 4),
	compile ps_3_0 PShaderSM3(8, 5, 4),
	compile ps_3_0 PShaderSM3(8, 6, 4),
	compile ps_3_0 PShaderSM3(8, 7, 4),
	compile ps_3_0 PShaderSM3(8, 8, 4),
	compile ps_3_0 PShaderSM3(0, 0, 5),
	compile ps_3_0 PShaderSM3(0, 1, 5),
	compile ps_3_0 PShaderSM3(0, 2, 5),
	compile ps_3_0 PShaderSM3(0, 3, 5),
	compile ps_3_0 PShaderSM3(0, 4, 5),
	compile ps_3_0 PShaderSM3(0, 5, 5),
	compile ps_3_0 PShaderSM3(0, 6, 5),
	compile ps_3_0 PShaderSM3(0, 7, 5),
	compile ps_3_0 PShaderSM3(0, 8, 5),
	compile ps_3_0 PShaderSM3(1, 0, 5),
	compile ps_3_0 PShaderSM3(1, 1, 5),
	compile ps_3_0 PShaderSM3(1, 2, 5),
	compile ps_3_0 PShaderSM3(1, 3, 5),
	compile ps_3_0 PShaderSM3(1, 4, 5),
	compile ps_3_0 PShaderSM3(1, 5, 5),
	compile ps_3_0 PShaderSM3(1, 6, 5),
	compile ps_3_0 PShaderSM3(1, 7, 5),
	compile ps_3_0 PShaderSM3(1, 8, 5),
	compile ps_3_0 PShaderSM3(2, 0, 5),
	compile ps_3_0 PShaderSM3(2, 1, 5),
	compile ps_3_0 PShaderSM3(2, 2, 5),
	compile ps_3_0 PShaderSM3(2, 3, 5),
	compile ps_3_0 PShaderSM3(2, 4, 5),
	compile ps_3_0 PShaderSM3(2, 5, 5),
	compile ps_3_0 PShaderSM3(2, 6, 5),
	compile ps_3_0 PShaderSM3(2, 7, 5),
	compile ps_3_0 PShaderSM3(2, 8, 5),
	compile ps_3_0 PShaderSM3(3, 0, 5),
	compile ps_3_0 PShaderSM3(3, 1, 5),
	compile ps_3_0 PShaderSM3(3, 2, 5),
	compile ps_3_0 PShaderSM3(3, 3, 5),
	compile ps_3_0 PShaderSM3(3, 4, 5),
	compile ps_3_0 PShaderSM3(3, 5, 5),
	compile ps_3_0 PShaderSM3(3, 6, 5),
	compile ps_3_0 PShaderSM3(3, 7, 5),
	compile ps_3_0 PShaderSM3(3, 8, 5),
	compile ps_3_0 PShaderSM3(4, 0, 5),
	compile ps_3_0 PShaderSM3(4, 1, 5),
	compile ps_3_0 PShaderSM3(4, 2, 5),
	compile ps_3_0 PShaderSM3(4, 3, 5),
	compile ps_3_0 PShaderSM3(4, 4, 5),
	compile ps_3_0 PShaderSM3(4, 5, 5),
	compile ps_3_0 PShaderSM3(4, 6, 5),
	compile ps_3_0 PShaderSM3(4, 7, 5),
	compile ps_3_0 PShaderSM3(4, 8, 5),
	compile ps_3_0 PShaderSM3(5, 0, 5),
	compile ps_3_0 PShaderSM3(5, 1, 5),
	compile ps_3_0 PShaderSM3(5, 2, 5),
	compile ps_3_0 PShaderSM3(5, 3, 5),
	compile ps_3_0 PShaderSM3(5, 4, 5),
	compile ps_3_0 PShaderSM3(5, 5, 5),
	compile ps_3_0 PShaderSM3(5, 6, 5),
	compile ps_3_0 PShaderSM3(5, 7, 5),
	compile ps_3_0 PShaderSM3(5, 8, 5),
	compile ps_3_0 PShaderSM3(6, 0, 5),
	compile ps_3_0 PShaderSM3(6, 1, 5),
	compile ps_3_0 PShaderSM3(6, 2, 5),
	compile ps_3_0 PShaderSM3(6, 3, 5),
	compile ps_3_0 PShaderSM3(6, 4, 5),
	compile ps_3_0 PShaderSM3(6, 5, 5),
	compile ps_3_0 PShaderSM3(6, 6, 5),
	compile ps_3_0 PShaderSM3(6, 7, 5),
	compile ps_3_0 PShaderSM3(6, 8, 5),
	compile ps_3_0 PShaderSM3(7, 0, 5),
	compile ps_3_0 PShaderSM3(7, 1, 5),
	compile ps_3_0 PShaderSM3(7, 2, 5),
	compile ps_3_0 PShaderSM3(7, 3, 5),
	compile ps_3_0 PShaderSM3(7, 4, 5),
	compile ps_3_0 PShaderSM3(7, 5, 5),
	compile ps_3_0 PShaderSM3(7, 6, 5),
	compile ps_3_0 PShaderSM3(7, 7, 5),
	compile ps_3_0 PShaderSM3(7, 8, 5),
	compile ps_3_0 PShaderSM3(8, 0, 5),
	compile ps_3_0 PShaderSM3(8, 1, 5),
	compile ps_3_0 PShaderSM3(8, 2, 5),
	compile ps_3_0 PShaderSM3(8, 3, 5),
	compile ps_3_0 PShaderSM3(8, 4, 5),
	compile ps_3_0 PShaderSM3(8, 5, 5),
	compile ps_3_0 PShaderSM3(8, 6, 5),
	compile ps_3_0 PShaderSM3(8, 7, 5),
	compile ps_3_0 PShaderSM3(8, 8, 5),
	compile ps_3_0 PShaderSM3(0, 0, 6),
	compile ps_3_0 PShaderSM3(0, 1, 6),
	compile ps_3_0 PShaderSM3(0, 2, 6),
	compile ps_3_0 PShaderSM3(0, 3, 6),
	compile ps_3_0 PShaderSM3(0, 4, 6),
	compile ps_3_0 PShaderSM3(0, 5, 6),
	compile ps_3_0 PShaderSM3(0, 6, 6),
	compile ps_3_0 PShaderSM3(0, 7, 6),
	compile ps_3_0 PShaderSM3(0, 8, 6),
	compile ps_3_0 PShaderSM3(1, 0, 6),
	compile ps_3_0 PShaderSM3(1, 1, 6),
	compile ps_3_0 PShaderSM3(1, 2, 6),
	compile ps_3_0 PShaderSM3(1, 3, 6),
	compile ps_3_0 PShaderSM3(1, 4, 6),
	compile ps_3_0 PShaderSM3(1, 5, 6),
	compile ps_3_0 PShaderSM3(1, 6, 6),
	compile ps_3_0 PShaderSM3(1, 7, 6),
	compile ps_3_0 PShaderSM3(1, 8, 6),
	compile ps_3_0 PShaderSM3(2, 0, 6),
	compile ps_3_0 PShaderSM3(2, 1, 6),
	compile ps_3_0 PShaderSM3(2, 2, 6),
	compile ps_3_0 PShaderSM3(2, 3, 6),
	compile ps_3_0 PShaderSM3(2, 4, 6),
	compile ps_3_0 PShaderSM3(2, 5, 6),
	compile ps_3_0 PShaderSM3(2, 6, 6),
	compile ps_3_0 PShaderSM3(2, 7, 6),
	compile ps_3_0 PShaderSM3(2, 8, 6),
	compile ps_3_0 PShaderSM3(3, 0, 6),
	compile ps_3_0 PShaderSM3(3, 1, 6),
	compile ps_3_0 PShaderSM3(3, 2, 6),
	compile ps_3_0 PShaderSM3(3, 3, 6),
	compile ps_3_0 PShaderSM3(3, 4, 6),
	compile ps_3_0 PShaderSM3(3, 5, 6),
	compile ps_3_0 PShaderSM3(3, 6, 6),
	compile ps_3_0 PShaderSM3(3, 7, 6),
	compile ps_3_0 PShaderSM3(3, 8, 6),
	compile ps_3_0 PShaderSM3(4, 0, 6),
	compile ps_3_0 PShaderSM3(4, 1, 6),
	compile ps_3_0 PShaderSM3(4, 2, 6),
	compile ps_3_0 PShaderSM3(4, 3, 6),
	compile ps_3_0 PShaderSM3(4, 4, 6),
	compile ps_3_0 PShaderSM3(4, 5, 6),
	compile ps_3_0 PShaderSM3(4, 6, 6),
	compile ps_3_0 PShaderSM3(4, 7, 6),
	compile ps_3_0 PShaderSM3(4, 8, 6),
	compile ps_3_0 PShaderSM3(5, 0, 6),
	compile ps_3_0 PShaderSM3(5, 1, 6),
	compile ps_3_0 PShaderSM3(5, 2, 6),
	compile ps_3_0 PShaderSM3(5, 3, 6),
	compile ps_3_0 PShaderSM3(5, 4, 6),
	compile ps_3_0 PShaderSM3(5, 5, 6),
	compile ps_3_0 PShaderSM3(5, 6, 6),
	compile ps_3_0 PShaderSM3(5, 7, 6),
	compile ps_3_0 PShaderSM3(5, 8, 6),
	compile ps_3_0 PShaderSM3(6, 0, 6),
	compile ps_3_0 PShaderSM3(6, 1, 6),
	compile ps_3_0 PShaderSM3(6, 2, 6),
	compile ps_3_0 PShaderSM3(6, 3, 6),
	compile ps_3_0 PShaderSM3(6, 4, 6),
	compile ps_3_0 PShaderSM3(6, 5, 6),
	compile ps_3_0 PShaderSM3(6, 6, 6),
	compile ps_3_0 PShaderSM3(6, 7, 6),
	compile ps_3_0 PShaderSM3(6, 8, 6),
	compile ps_3_0 PShaderSM3(7, 0, 6),
	compile ps_3_0 PShaderSM3(7, 1, 6),
	compile ps_3_0 PShaderSM3(7, 2, 6),
	compile ps_3_0 PShaderSM3(7, 3, 6),
	compile ps_3_0 PShaderSM3(7, 4, 6),
	compile ps_3_0 PShaderSM3(7, 5, 6),
	compile ps_3_0 PShaderSM3(7, 6, 6),
	compile ps_3_0 PShaderSM3(7, 7, 6),
	compile ps_3_0 PShaderSM3(7, 8, 6),
	compile ps_3_0 PShaderSM3(8, 0, 6),
	compile ps_3_0 PShaderSM3(8, 1, 6),
	compile ps_3_0 PShaderSM3(8, 2, 6),
	compile ps_3_0 PShaderSM3(8, 3, 6),
	compile ps_3_0 PShaderSM3(8, 4, 6),
	compile ps_3_0 PShaderSM3(8, 5, 6),
	compile ps_3_0 PShaderSM3(8, 6, 6),
	compile ps_3_0 PShaderSM3(8, 7, 6),
	compile ps_3_0 PShaderSM3(8, 8, 6),
	compile ps_3_0 PShaderSM3(0, 0, 7),
	compile ps_3_0 PShaderSM3(0, 1, 7),
	compile ps_3_0 PShaderSM3(0, 2, 7),
	compile ps_3_0 PShaderSM3(0, 3, 7),
	compile ps_3_0 PShaderSM3(0, 4, 7),
	compile ps_3_0 PShaderSM3(0, 5, 7),
	compile ps_3_0 PShaderSM3(0, 6, 7),
	compile ps_3_0 PShaderSM3(0, 7, 7),
	compile ps_3_0 PShaderSM3(0, 8, 7),
	compile ps_3_0 PShaderSM3(1, 0, 7),
	compile ps_3_0 PShaderSM3(1, 1, 7),
	compile ps_3_0 PShaderSM3(1, 2, 7),
	compile ps_3_0 PShaderSM3(1, 3, 7),
	compile ps_3_0 PShaderSM3(1, 4, 7),
	compile ps_3_0 PShaderSM3(1, 5, 7),
	compile ps_3_0 PShaderSM3(1, 6, 7),
	compile ps_3_0 PShaderSM3(1, 7, 7),
	compile ps_3_0 PShaderSM3(1, 8, 7),
	compile ps_3_0 PShaderSM3(2, 0, 7),
	compile ps_3_0 PShaderSM3(2, 1, 7),
	compile ps_3_0 PShaderSM3(2, 2, 7),
	compile ps_3_0 PShaderSM3(2, 3, 7),
	compile ps_3_0 PShaderSM3(2, 4, 7),
	compile ps_3_0 PShaderSM3(2, 5, 7),
	compile ps_3_0 PShaderSM3(2, 6, 7),
	compile ps_3_0 PShaderSM3(2, 7, 7),
	compile ps_3_0 PShaderSM3(2, 8, 7),
	compile ps_3_0 PShaderSM3(3, 0, 7),
	compile ps_3_0 PShaderSM3(3, 1, 7),
	compile ps_3_0 PShaderSM3(3, 2, 7),
	compile ps_3_0 PShaderSM3(3, 3, 7),
	compile ps_3_0 PShaderSM3(3, 4, 7),
	compile ps_3_0 PShaderSM3(3, 5, 7),
	compile ps_3_0 PShaderSM3(3, 6, 7),
	compile ps_3_0 PShaderSM3(3, 7, 7),
	compile ps_3_0 PShaderSM3(3, 8, 7),
	compile ps_3_0 PShaderSM3(4, 0, 7),
	compile ps_3_0 PShaderSM3(4, 1, 7),
	compile ps_3_0 PShaderSM3(4, 2, 7),
	compile ps_3_0 PShaderSM3(4, 3, 7),
	compile ps_3_0 PShaderSM3(4, 4, 7),
	compile ps_3_0 PShaderSM3(4, 5, 7),
	compile ps_3_0 PShaderSM3(4, 6, 7),
	compile ps_3_0 PShaderSM3(4, 7, 7),
	compile ps_3_0 PShaderSM3(4, 8, 7),
	compile ps_3_0 PShaderSM3(5, 0, 7),
	compile ps_3_0 PShaderSM3(5, 1, 7),
	compile ps_3_0 PShaderSM3(5, 2, 7),
	compile ps_3_0 PShaderSM3(5, 3, 7),
	compile ps_3_0 PShaderSM3(5, 4, 7),
	compile ps_3_0 PShaderSM3(5, 5, 7),
	compile ps_3_0 PShaderSM3(5, 6, 7),
	compile ps_3_0 PShaderSM3(5, 7, 7),
	compile ps_3_0 PShaderSM3(5, 8, 7),
	compile ps_3_0 PShaderSM3(6, 0, 7),
	compile ps_3_0 PShaderSM3(6, 1, 7),
	compile ps_3_0 PShaderSM3(6, 2, 7),
	compile ps_3_0 PShaderSM3(6, 3, 7),
	compile ps_3_0 PShaderSM3(6, 4, 7),
	compile ps_3_0 PShaderSM3(6, 5, 7),
	compile ps_3_0 PShaderSM3(6, 6, 7),
	compile ps_3_0 PShaderSM3(6, 7, 7),
	compile ps_3_0 PShaderSM3(6, 8, 7),
	compile ps_3_0 PShaderSM3(7, 0, 7),
	compile ps_3_0 PShaderSM3(7, 1, 7),
	compile ps_3_0 PShaderSM3(7, 2, 7),
	compile ps_3_0 PShaderSM3(7, 3, 7),
	compile ps_3_0 PShaderSM3(7, 4, 7),
	compile ps_3_0 PShaderSM3(7, 5, 7),
	compile ps_3_0 PShaderSM3(7, 6, 7),
	compile ps_3_0 PShaderSM3(7, 7, 7),
	compile ps_3_0 PShaderSM3(7, 8, 7),
	compile ps_3_0 PShaderSM3(8, 0, 7),
	compile ps_3_0 PShaderSM3(8, 1, 7),
	compile ps_3_0 PShaderSM3(8, 2, 7),
	compile ps_3_0 PShaderSM3(8, 3, 7),
	compile ps_3_0 PShaderSM3(8, 4, 7),
	compile ps_3_0 PShaderSM3(8, 5, 7),
	compile ps_3_0 PShaderSM3(8, 6, 7),
	compile ps_3_0 PShaderSM3(8, 7, 7),
	compile ps_3_0 PShaderSM3(8, 8, 7),
	compile ps_3_0 PShaderSM3(0, 0, 8),
	compile ps_3_0 PShaderSM3(0, 1, 8),
	compile ps_3_0 PShaderSM3(0, 2, 8),
	compile ps_3_0 PShaderSM3(0, 3, 8),
	compile ps_3_0 PShaderSM3(0, 4, 8),
	compile ps_3_0 PShaderSM3(0, 5, 8),
	compile ps_3_0 PShaderSM3(0, 6, 8),
	compile ps_3_0 PShaderSM3(0, 7, 8),
	compile ps_3_0 PShaderSM3(0, 8, 8),
	compile ps_3_0 PShaderSM3(1, 0, 8),
	compile ps_3_0 PShaderSM3(1, 1, 8),
	compile ps_3_0 PShaderSM3(1, 2, 8),
	compile ps_3_0 PShaderSM3(1, 3, 8),
	compile ps_3_0 PShaderSM3(1, 4, 8),
	compile ps_3_0 PShaderSM3(1, 5, 8),
	compile ps_3_0 PShaderSM3(1, 6, 8),
	compile ps_3_0 PShaderSM3(1, 7, 8),
	compile ps_3_0 PShaderSM3(1, 8, 8),
	compile ps_3_0 PShaderSM3(2, 0, 8),
	compile ps_3_0 PShaderSM3(2, 1, 8),
	compile ps_3_0 PShaderSM3(2, 2, 8),
	compile ps_3_0 PShaderSM3(2, 3, 8),
	compile ps_3_0 PShaderSM3(2, 4, 8),
	compile ps_3_0 PShaderSM3(2, 5, 8),
	compile ps_3_0 PShaderSM3(2, 6, 8),
	compile ps_3_0 PShaderSM3(2, 7, 8),
	compile ps_3_0 PShaderSM3(2, 8, 8),
	compile ps_3_0 PShaderSM3(3, 0, 8),
	compile ps_3_0 PShaderSM3(3, 1, 8),
	compile ps_3_0 PShaderSM3(3, 2, 8),
	compile ps_3_0 PShaderSM3(3, 3, 8),
	compile ps_3_0 PShaderSM3(3, 4, 8),
	compile ps_3_0 PShaderSM3(3, 5, 8),
	compile ps_3_0 PShaderSM3(3, 6, 8),
	compile ps_3_0 PShaderSM3(3, 7, 8),
	compile ps_3_0 PShaderSM3(3, 8, 8),
	compile ps_3_0 PShaderSM3(4, 0, 8),
	compile ps_3_0 PShaderSM3(4, 1, 8),
	compile ps_3_0 PShaderSM3(4, 2, 8),
	compile ps_3_0 PShaderSM3(4, 3, 8),
	compile ps_3_0 PShaderSM3(4, 4, 8),
	compile ps_3_0 PShaderSM3(4, 5, 8),
	compile ps_3_0 PShaderSM3(4, 6, 8),
	compile ps_3_0 PShaderSM3(4, 7, 8),
	compile ps_3_0 PShaderSM3(4, 8, 8),
	compile ps_3_0 PShaderSM3(5, 0, 8),
	compile ps_3_0 PShaderSM3(5, 1, 8),
	compile ps_3_0 PShaderSM3(5, 2, 8),
	compile ps_3_0 PShaderSM3(5, 3, 8),
	compile ps_3_0 PShaderSM3(5, 4, 8),
	compile ps_3_0 PShaderSM3(5, 5, 8),
	compile ps_3_0 PShaderSM3(5, 6, 8),
	compile ps_3_0 PShaderSM3(5, 7, 8),
	compile ps_3_0 PShaderSM3(5, 8, 8),
	compile ps_3_0 PShaderSM3(6, 0, 8),
	compile ps_3_0 PShaderSM3(6, 1, 8),
	compile ps_3_0 PShaderSM3(6, 2, 8),
	compile ps_3_0 PShaderSM3(6, 3, 8),
	compile ps_3_0 PShaderSM3(6, 4, 8),
	compile ps_3_0 PShaderSM3(6, 5, 8),
	compile ps_3_0 PShaderSM3(6, 6, 8),
	compile ps_3_0 PShaderSM3(6, 7, 8),
	compile ps_3_0 PShaderSM3(6, 8, 8),
	compile ps_3_0 PShaderSM3(7, 0, 8),
	compile ps_3_0 PShaderSM3(7, 1, 8),
	compile ps_3_0 PShaderSM3(7, 2, 8),
	compile ps_3_0 PShaderSM3(7, 3, 8),
	compile ps_3_0 PShaderSM3(7, 4, 8),
	compile ps_3_0 PShaderSM3(7, 5, 8),
	compile ps_3_0 PShaderSM3(7, 6, 8),
	compile ps_3_0 PShaderSM3(7, 7, 8),
	compile ps_3_0 PShaderSM3(7, 8, 8),
	compile ps_3_0 PShaderSM3(8, 0, 8),
	compile ps_3_0 PShaderSM3(8, 1, 8),
	compile ps_3_0 PShaderSM3(8, 2, 8),
	compile ps_3_0 PShaderSM3(8, 3, 8),
	compile ps_3_0 PShaderSM3(8, 4, 8),
	compile ps_3_0 PShaderSM3(8, 5, 8),
	compile ps_3_0 PShaderSM3(8, 6, 8),
	compile ps_3_0 PShaderSM3(8, 7, 8),
	compile ps_3_0 PShaderSM3(8, 8, 8)
};

technique g2sm3 {
	pass P0 {
		VertexShader = VShaderSkinnedSM3Arr[MaxBoneWeights];
		PixelShader = PShaderSM3Arr[LightShaderIndex];
	}
}