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

struct TVSInputStatic {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
};

struct TVSInputSkinned {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVSInputSkinned1SM3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int _BIndices0: BlendIndices0;
};

struct TVSInputSkinned2SM3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVSInputSkinned3SM3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVSInputSkinned4SM3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TPSInputSM2 {
	float4 _Position0: Position0;
	float4 _TexCoord0: TexCoord0;
	float4 _LightPoint[MAX_LIGHT_COUNT_SM2_POINT]: TexCoord1;
	float4 _LightDir[MAX_LIGHT_COUNT_SM2_DIR]: TexCoord4;
	float3 _CamDir: TexCoord6;
};

struct TPSInputSM3 {
	float4 _Position0: Position0;
	float2 _TexCoord0: TexCoord0;
	float3 _WorldPosition: TexCoord1;
	float3 _WorldTangent: TexCoord2;
	float3 _WorldBinormal: TexCoord3;
	float3 _WorldNormal: TexCoord4;
};

void VShaderStaticSM3 (const in TVSInputStatic Input, out TPSInputSM3 Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), WVP);
	Output._TexCoord0 = (float2)Input._TexCoord0;
	Output._WorldPosition = mul(float4(Input._Position0, 1), (float4x3)WV);
	Output._WorldTangent = mul(Input._Tangent0, (float3x3)WV);
	Output._WorldBinormal = mul(Input._Binormal0, (float3x3)WV);
	Output._WorldNormal = mul(Input._Normal0, (float3x3)WV);
}

void VShaderSkinned1SM3 (const in TVSInputSkinned1SM3 Input, out TPSInputSM3 Output) {
	Output._TexCoord0 = Input._TexCoord0;
	float4x3 FinalMatrix = SkinPallete[Input._BIndices0];
	float4 SkinPos = float4(mul(float4(Input._Position0, 1), FinalMatrix), 1);
	Output._Position0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	Output._WorldPosition = mul(SkinPos, (float4x3)WV);
	Output._WorldTangent = mul(Input._Tangent0, (float3x3)FinalMatrix);
	Output._WorldBinormal = mul(Input._Binormal0, (float3x3)FinalMatrix);
	Output._WorldNormal = mul(Input._Normal0, (float3x3)FinalMatrix);
}

void VShaderSkinned2SM3 (const in TVSInputSkinned2SM3 Input, out TPSInputSM3 Output) {
	Output._TexCoord0 = Input._TexCoord0;
	float4x3 FinalMatrix = (
		SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1]
	);
	float4 SkinPos = float4(mul(float4(Input._Position0, 1), FinalMatrix), 1);
	Output._Position0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	Output._WorldPosition = mul(SkinPos, (float4x3)WV);
	Output._WorldTangent = mul(Input._Tangent0, (float3x3)FinalMatrix);
	Output._WorldBinormal = mul(Input._Binormal0, (float3x3)FinalMatrix);
	Output._WorldNormal = mul(Input._Normal0, (float3x3)FinalMatrix);
}

void VShaderSkinned3SM3 (const in TVSInputSkinned3SM3 Input, out TPSInputSM3 Output) {
	Output._TexCoord0 = Input._TexCoord0;
	float4x3 FinalMatrix = (
		SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2]
	);
	float4 SkinPos = float4(mul(float4(Input._Position0, 1), FinalMatrix), 1);
	Output._Position0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	Output._WorldPosition = mul(SkinPos, (float4x3)WV);
	Output._WorldTangent = mul(Input._Tangent0, (float3x3)FinalMatrix);
	Output._WorldBinormal = mul(Input._Binormal0, (float3x3)FinalMatrix);
	Output._WorldNormal = mul(Input._Normal0, (float3x3)FinalMatrix);
}

void VShaderSkinned4SM3 (const in TVSInputSkinned4SM3 Input, out TPSInputSM3 Output) {
	Output._TexCoord0 = Input._TexCoord0;
	float4x3 FinalMatrix = (
		SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2] +
		SkinPallete[Input._BIndices0[3]] * Input._BWeights0[3]
	);
	float4 SkinPos = float4(mul(float4(Input._Position0, 1), FinalMatrix), 1);
	Output._Position0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	Output._WorldPosition = mul(SkinPos, (float4x3)WV);
	Output._WorldTangent = mul(Input._Tangent0, (float3x3)FinalMatrix);
	Output._WorldBinormal = mul(Input._Binormal0, (float3x3)FinalMatrix);
	Output._WorldNormal = mul(Input._Normal0, (float3x3)FinalMatrix);
}

float4 PShaderSM3 (const in TPSInputSM3 Input, uniform int LCPoint, uniform int LCDir): Color0 {
	float3 CamDir = normalize(Input._WorldPosition);
	float3 WorldTangent = normalize(Input._WorldTangent);
	float3 WorldBinormal = normalize(Input._WorldBinormal);
	float3 WorldNormal = normalize(Input._WorldNormal);
	float3x3 TBN = float3x3(WorldTangent, WorldBinormal, WorldNormal);
	float3 Normal = normalize(mul(((float3)tex2D(SampNormals, Input._TexCoord0)) * 2  - 1, (float3x3) TBN));
	float3 RefDir = CamDir - 2 * Normal * dot(CamDir, Normal);
	float3 Dif = (0, 0, 0);
	float3 Spec = (0, 0, 0);
	float Att;
	float3 VertexToLight;
	for (int i = 0; i < LCPoint; i++) {
		VertexToLight = LightPosPoint[i] - Input._WorldPosition;
		Att = clamp(1 - dot(VertexToLight, VertexToLight) / LightRangePoint[i], 0, 1);
		VertexToLight = normalize(VertexToLight);
		Dif += saturate(dot(VertexToLight, Normal) * LightColorPoint[i]) * Att;
		Spec += saturate(dot(VertexToLight, RefDir) * LightColorPoint[i]) * Att;
	}
	for (int i = 0; i < LCDir; i++) {
		Dif += saturate(dot(LightDirDirectional[i], Normal) * LightColorDirectional[i]); 
		Spec += saturate(dot(LightDirDirectional[i], RefDir) * LightColorDirectional[i]); 
	}
	float4 TexDiffuse = tex2D(SampDiffuse, Input._TexCoord0);
	float4 TexSpecular = tex2D(SampSpecular, Input._TexCoord0);
	float4 Output = TexDiffuse * float4(saturate(Dif) + LightAmbient, 1) + float4(pow(saturate(Spec), 5), 0) * TexSpecular;
	Output.w = TexDiffuse.w;
	return Output;
}


void VShaderStaticSM2 (const in TVSInputStatic Input, out TPSInputSM2 Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), WVP);
	float3 WorldPosition = mul(float4(Input._Position0, 1), (float4x3)WV);
	float3 WorldTangent = mul(Input._Tangent0, (float3x3)WV);
	float3 WorldBinormal = mul(Input._Binormal0, (float3x3)WV);
	float3 WorldNormal = cross(WorldTangent, WorldBinormal);
	float3x3 BTN = float3x3(WorldBinormal, WorldTangent, WorldNormal);
	int LPCount = min(LightCountPoint, MAX_LIGHT_COUNT_SM2_POINT);
	int LDCount = min(LightCountDirectional, MAX_LIGHT_COUNT_SM2_DIR);
	float Att;
	float3 VertexToLight;
	for (int i = 0; i < MAX_LIGHT_COUNT_SM2_POINT; i++) {
		VertexToLight = LightPosPoint[i] - WorldPosition;
		if (i >= LPCount)
		Att = 0;
		else
		Att = 1;
		Output._LightPoint[i] = float4(mul(BTN, VertexToLight), Att);
	}
	for (int i = 0; i < MAX_LIGHT_COUNT_SM2_DIR; i++) {
		if (i >= LDCount)
		Att = 0;
		else
		Att = 1;
		Output._LightDir[i] = float4(mul(BTN, LightDirDirectional[i]), 1);
	}
	Output._TexCoord0 = float4((float2)Input._TexCoord0, LPCount, LDCount);
	Output._CamDir = mul(BTN, WorldPosition);
}

void VShaderSkinnedSM2 (const in TVSInputSkinned Input, out TPSInputSM2 Output, uniform int MaxWeights) {
	float4x3 FinalMatrix = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	for (int i = 0; i < MaxWeights; i++) {
		FinalMatrix += SkinPallete[Input._BIndices0[i]] * Input._BWeights0[i];
	}
	float4 SkinPos = float4(mul(float4(Input._Position0, 1), FinalMatrix), 1);
	Output._Position0 = mul(SkinPos, WVP);
	FinalMatrix = mul(FinalMatrix, WV);
	float3 WorldPosition = mul(SkinPos, (float4x3)WV);
	float3 WorldTangent = normalize(mul(Input._Tangent0, (float3x3)FinalMatrix));
	float3 WorldBinormal = normalize(mul(Input._Binormal0, (float3x3)FinalMatrix));
	float3 WorldNormal = cross(WorldTangent, WorldBinormal);
	float3x3 BTN = float3x3(WorldBinormal, WorldTangent, WorldNormal);
	int LPCount = min(LightCountPoint, MAX_LIGHT_COUNT_SM2_POINT);
	int LDCount = min(LightCountDirectional, MAX_LIGHT_COUNT_SM2_DIR);
	float Att;
	float3 VertexToLight;
	for (int i = 0; i < MAX_LIGHT_COUNT_SM2_POINT; i++) {
		VertexToLight = LightPosPoint[i] - WorldPosition;
		if (i >= LPCount)
		Att = 0;
		else
		Att = 1;
		Output._LightPoint[i] = float4(mul(BTN, VertexToLight), Att);
	}
	for (int i = 0; i < MAX_LIGHT_COUNT_SM2_DIR; i++) {
		if (i >= LDCount)
		Att = 0;
		else
		Att = 1;
		Output._LightDir[i] = float4(mul(BTN, LightDirDirectional[i]), Att);
	}
	Output._TexCoord0 = float4((float2)Input._TexCoord0, LPCount, LDCount);
	Output._CamDir = mul(BTN, WorldPosition);
}

float4 PShaderSM2 (const in TPSInputSM2 Input): Color0 {
	float3 CamDir = normalize(Input._CamDir);
	float3 Normal = normalize((float3)tex2D(SampNormals, Input._TexCoord0) * 2 - 1);
	float3 RefDir = CamDir - 2 * Normal * dot(CamDir, Normal);
	float3 Dif = (0, 0, 0);
	float3 Spec = (0, 0, 0);
	float Att;
	float3 VertexToLight;
	for (int i = 0; i < MAX_LIGHT_COUNT_SM2_POINT; i++) {
		VertexToLight = Input._LightPoint[i];
		Att = clamp(1 - dot(VertexToLight, VertexToLight) / LightRangePoint[i], 0, 1);
		VertexToLight = normalize(VertexToLight);
		Dif += saturate(dot(VertexToLight, Normal) * LightColorPoint[i]) * Att;
		Spec += saturate(dot(VertexToLight, RefDir) * LightColorPoint[i]) * Att;
	}
	for (int i = 0; i < MAX_LIGHT_COUNT_SM2_DIR; i++) {
		Dif += saturate(dot(Input._LightDir[i], Normal) * LightColorDirectional[i]) * Input._LightDir[i].w; 
		Spec += saturate(dot(Input._LightDir[i], RefDir) * LightColorDirectional[i]) * Input._LightDir[i].w; 
	}
	float4 TexDiffuse = tex2D(SampDiffuse, Input._TexCoord0);
	float4 TexSpecular = tex2D(SampSpecular, Input._TexCoord0);
	float4 Output = TexDiffuse * float4(saturate(Dif) + LightAmbient, 1) + float4(pow(saturate(Spec), 5), 0) * TexSpecular;
	Output.w = 1;
	return Output;
}

struct TPSInputD {
	float4 _Position0: Position0;
	float4 _Diffuse0: Color0;
	float2 _TexCoord0: TexCoord0;
};

void VShaderStaticD (const in TVSInputStatic Input, out TPSInputD Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), WVP);
	Output._TexCoord0 = (float2)Input._TexCoord0;
	//float3 WorldNormal = mul(Input._Normal0, (float3x3)W);
	//Output._Diffuse0 = dot(WorldNormal, normalize(float3(-1, 1, -1)));
}

void VShaderSkinnedD (const in TVSInputSkinned Input, out TPSInputD Output) {
	float4x3 FinalBone = (
		SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2] +
		SkinPallete[Input._BIndices0[3]] * Input._BWeights0[3]
	);
	float3 SkinPos = mul(float4(Input._Position0, 1), FinalBone);
	Output._Position0 = mul(float4(SkinPos, 1), WVP);
	Output._TexCoord0 = Input._TexCoord0;
	//float3x3 FinalBoneW = mul(FinalBone, WV);
	//float3 WorldNormal = mul(Input._Normal0, (float3x3)FinalBoneW);
	//Output._Diffuse0 = dot(normalize(WorldNormal), normalize(float3(-1, 1, -1)));
	//Output._Position0 = mul(float4(Input._Position0, 1), WVP);
	//Output._TexCoord0 = Input._TexCoord0;
}

float4 PShaderD (const in TPSInputD Input): Color0 {
	return tex2D(SampDiffuse, Input._TexCoord0) * float4(Input._Diffuse0.xyz, 1);
}

VertexShader VShaderSkinnedSM3Arr[4] = {
	compile vs_3_0 VShaderSkinned1SM3(), 
    compile vs_3_0 VShaderSkinned2SM3(),
    compile vs_3_0 VShaderSkinned3SM3(),
    compile vs_3_0 VShaderSkinned4SM3()
};

VertexShader VShaderSkinnedSM2Arr[4] = {
	compile vs_2_0 VShaderSkinnedSM2(1), 
    compile vs_2_0 VShaderSkinnedSM2(2),
    compile vs_2_0 VShaderSkinnedSM2(3),
    compile vs_2_0 VShaderSkinnedSM2(4)
};

PixelShader PShaderSM3Arr[81] = {
	compile ps_3_0 PShaderSM3(0, 0),
	compile ps_3_0 PShaderSM3(0, 1),
	compile ps_3_0 PShaderSM3(0, 2),
	compile ps_3_0 PShaderSM3(0, 3),
	compile ps_3_0 PShaderSM3(0, 4),
	compile ps_3_0 PShaderSM3(0, 5),
	compile ps_3_0 PShaderSM3(0, 6),
	compile ps_3_0 PShaderSM3(0, 7),
	compile ps_3_0 PShaderSM3(0, 8),
	compile ps_3_0 PShaderSM3(1, 0),
	compile ps_3_0 PShaderSM3(1, 1),
	compile ps_3_0 PShaderSM3(1, 2),
	compile ps_3_0 PShaderSM3(1, 3),
	compile ps_3_0 PShaderSM3(1, 4),
	compile ps_3_0 PShaderSM3(1, 5),
	compile ps_3_0 PShaderSM3(1, 6),
	compile ps_3_0 PShaderSM3(1, 7),
	compile ps_3_0 PShaderSM3(1, 8),
	compile ps_3_0 PShaderSM3(2, 0),
	compile ps_3_0 PShaderSM3(2, 1),
	compile ps_3_0 PShaderSM3(2, 2),
	compile ps_3_0 PShaderSM3(2, 3),
	compile ps_3_0 PShaderSM3(2, 4),
	compile ps_3_0 PShaderSM3(2, 5),
	compile ps_3_0 PShaderSM3(2, 6),
	compile ps_3_0 PShaderSM3(2, 7),
	compile ps_3_0 PShaderSM3(2, 8),
	compile ps_3_0 PShaderSM3(3, 0),
	compile ps_3_0 PShaderSM3(3, 1),
	compile ps_3_0 PShaderSM3(3, 2),
	compile ps_3_0 PShaderSM3(3, 3),
	compile ps_3_0 PShaderSM3(3, 4),
	compile ps_3_0 PShaderSM3(3, 5),
	compile ps_3_0 PShaderSM3(3, 6),
	compile ps_3_0 PShaderSM3(3, 7),
	compile ps_3_0 PShaderSM3(3, 8),
	compile ps_3_0 PShaderSM3(4, 0),
	compile ps_3_0 PShaderSM3(4, 1),
	compile ps_3_0 PShaderSM3(4, 2),
	compile ps_3_0 PShaderSM3(4, 3),
	compile ps_3_0 PShaderSM3(4, 4),
	compile ps_3_0 PShaderSM3(4, 5),
	compile ps_3_0 PShaderSM3(4, 6),
	compile ps_3_0 PShaderSM3(4, 7),
	compile ps_3_0 PShaderSM3(4, 8),
	compile ps_3_0 PShaderSM3(5, 0),
	compile ps_3_0 PShaderSM3(5, 1),
	compile ps_3_0 PShaderSM3(5, 2),
	compile ps_3_0 PShaderSM3(5, 3),
	compile ps_3_0 PShaderSM3(5, 4),
	compile ps_3_0 PShaderSM3(5, 5),
	compile ps_3_0 PShaderSM3(5, 6),
	compile ps_3_0 PShaderSM3(5, 7),
	compile ps_3_0 PShaderSM3(5, 8),
	compile ps_3_0 PShaderSM3(6, 0),
	compile ps_3_0 PShaderSM3(6, 1),
	compile ps_3_0 PShaderSM3(6, 2),
	compile ps_3_0 PShaderSM3(6, 3),
	compile ps_3_0 PShaderSM3(6, 4),
	compile ps_3_0 PShaderSM3(6, 5),
	compile ps_3_0 PShaderSM3(6, 6),
	compile ps_3_0 PShaderSM3(6, 7),
	compile ps_3_0 PShaderSM3(6, 8),
	compile ps_3_0 PShaderSM3(7, 0),
	compile ps_3_0 PShaderSM3(7, 1),
	compile ps_3_0 PShaderSM3(7, 2),
	compile ps_3_0 PShaderSM3(7, 3),
	compile ps_3_0 PShaderSM3(7, 4),
	compile ps_3_0 PShaderSM3(7, 5),
	compile ps_3_0 PShaderSM3(7, 6),
	compile ps_3_0 PShaderSM3(7, 7),
	compile ps_3_0 PShaderSM3(7, 8),
	compile ps_3_0 PShaderSM3(8, 0),
	compile ps_3_0 PShaderSM3(8, 1),
	compile ps_3_0 PShaderSM3(8, 2),
	compile ps_3_0 PShaderSM3(8, 3),
	compile ps_3_0 PShaderSM3(8, 4),
	compile ps_3_0 PShaderSM3(8, 5),
	compile ps_3_0 PShaderSM3(8, 6),
	compile ps_3_0 PShaderSM3(8, 7),
	compile ps_3_0 PShaderSM3(8, 8)
};

technique g2sm2st {
	pass P0 {
		VertexShader = compile vs_2_0 VShaderStaticSM2();
		PixelShader = compile ps_2_0 PShaderSM2();
	}
}

technique g2sm2sk {
	pass P0 {
		VertexShader = VShaderSkinnedSM2Arr[MaxBoneWeights];
		PixelShader = compile ps_2_0 PShaderSM2();
	}
}

technique g2sm3st {
	pass P0 {
		VertexShader = compile vs_3_0 VShaderStaticSM3();
		PixelShader = PShaderSM3Arr[LightShaderIndex];
	}
}

technique g2sm3sk {
	pass P0 {
		VertexShader = VShaderSkinnedSM3Arr[MaxBoneWeights];
		PixelShader = PShaderSM3Arr[LightShaderIndex];
	}
}

technique g2m01d {
	pass P0 {
		VertexShader = compile vs_2_0 VShaderStaticD();
		PixelShader = compile ps_2_0 PShaderD();
	}
}

technique g2m02d {
	pass P0 {
		VertexShader = compile vs_3_0 VShaderSkinnedD();
		PixelShader = compile ps_3_0 PShaderD();
	}
}