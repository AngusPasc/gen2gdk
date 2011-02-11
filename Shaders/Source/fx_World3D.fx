/*--------------------------------*\
		Gen2 World3D Shaders
\*--------------------------------*/

static const int MAX_ANISOTROPY = 16;
static const int MAX_LIGHTS = 6;
static const float SHADOW_EPSILON = 0.00005f;
static const int MAX_BONE_COUNT = 80;

uniform int VS_Index;
uniform int PS_Index;
matrix g_W;
matrix g_WVP;
matrix g_LightWVP;
matrix g_LightProj;
float3 g_CamPosW;
float3 g_LightPosW;
float3 g_LightDir;
float3 g_LightColor;
float2 g_LightAtt;
float g_SpotInner;
float g_SpotOutter;
uniform int g_CubeShadowIndex;
uniform int g_ProjShadowIndex;
float g_DepthBias;
float g_SlopeScaleDepthBias;
float g_DepthScale;
float g_ShadowMapStep;
float4 g_CubeSampleMap[16];
float4 g_ProjSampleMap[16];
float4x3 g_SkinPallete[MAX_BONE_COUNT];
float2 g_BlurOffset;

float3 g_Ambient;

texture TexDiffuse;
texture TexNormals;
texture TexSpecular;
texture TexLightMap;
texture TexShadowProj;
texture TexDepth0;
texture TexDepth1;
texture TexDepth2;
texture TexDepth3;
texture TexDepth4;
texture TexDepth5;
texture TexDepth;
texture TexBlur;

sampler SampDiffuse = sampler_state {
    Texture = <TexDiffuse>;
    MinFilter = Linear;  
    MagFilter = Linear;
    MipFilter = Linear;
    AddressU  = Wrap;
    AddressV  = Wrap;
};

sampler SampNormals = sampler_state {
    Texture = <TexNormals>;
    MinFilter = Linear;  
    MagFilter = Linear;
    MipFilter = Linear;
    AddressU  = Wrap;
    AddressV  = Wrap;
};

sampler SampSpecular = sampler_state {
    Texture = <TexSpecular>;
    MinFilter = Linear;  
    MagFilter = Linear;
    MipFilter = Linear;
    AddressU  = Wrap;
    AddressV  = Wrap;
};

sampler SampLightMap = sampler_state {
    Texture = <TexLightMap>;
    MinFilter = Linear;  
    MagFilter = Linear;
    MipFilter = Linear;
    AddressU  = Clamp;
    AddressV  = Clamp;
};

sampler SampShadowProj = sampler_state {
    Texture = <TexShadowProj>;
    MinFilter = Linear;  
    MagFilter = Linear;
    MipFilter = Linear;
    AddressU  = Clamp;
    AddressV  = Clamp;
};

samplerCUBE SampDepth0 = sampler_state {
	Texture = <TexDepth0>;
	MinFilter = Linear;  
	MagFilter = Linear;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

samplerCUBE SampDepth1 = sampler_state {
	Texture = <TexDepth1>;
	MinFilter = Linear;  
	MagFilter = Linear;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

samplerCUBE SampDepth2 = sampler_state {
	Texture = <TexDepth2>;
	MinFilter = Linear;  
	MagFilter = Linear;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

samplerCUBE SampDepth3 = sampler_state {
	Texture = <TexDepth3>;
	MinFilter = Linear;  
	MagFilter = Linear;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

samplerCUBE SampDepth4 = sampler_state {
	Texture = <TexDepth4>;
	MinFilter = Linear;  
	MagFilter = Linear;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

samplerCUBE SampDepth5 = sampler_state {
	Texture = <TexDepth5>;
	MinFilter = Linear;  
	MagFilter = Linear;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

sampler SampDepth = sampler_state {
	Texture = <TexDepth>;
	MinFilter = Linear;  
	MagFilter = Linear;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

sampler SampBlur = sampler_state {
	Texture = <TexBlur>;
	MinFilter = Linear;  
	MagFilter = Linear;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

struct TPS_Input_Blur {
	float2 _TexCoord0: TexCoord0;
};

struct TPS_Output_Blur {
	float4 _Color0: Color0;
};

void PS_Blur (const in TPS_Input_Blur Input, out TPS_Output_Blur Output) {
	Output._Color0 = tex2D(SampBlur, Input._TexCoord0) * 0.3;
	Output._Color0 += tex2D(SampBlur, Input._TexCoord0 + g_BlurOffset * g_ShadowMapStep) * 0.2;
	Output._Color0 += tex2D(SampBlur, Input._TexCoord0 - g_BlurOffset * g_ShadowMapStep) * 0.2;
	Output._Color0 += tex2D(SampBlur, Input._TexCoord0 + g_BlurOffset * g_ShadowMapStep * 2) * 0.1;
	Output._Color0 += tex2D(SampBlur, Input._TexCoord0 - g_BlurOffset * g_ShadowMapStep * 2) * 0.1;
	Output._Color0 += tex2D(SampBlur, Input._TexCoord0 + g_BlurOffset * g_ShadowMapStep * 3) * 0.05;
	Output._Color0 += tex2D(SampBlur, Input._TexCoord0 - g_BlurOffset * g_ShadowMapStep * 3) * 0.05;
}

technique g2Blur {
	pass P0 {
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur();
	}
}

struct TPS_Output_DepthClear {
	float4 _Color0: Color0;
};

void PS_DepthClear (out TPS_Output_DepthClear Output) {
	Output._Color0 = float4(1, 0, 0, 1);
}

technique g2DepthClear {
	pass P0 {
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_DepthClear();
	}
}

struct TVS_Input_Depth_Static {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
};

struct TVS_Input_Depth_Skinned1 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int _BIndices0: BlendIndices0;
};

struct TVS_Input_Depth_Skinned2 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVS_Input_Depth_Skinned3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVS_Input_Depth_Skinned4 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVS_Output_Depth {
	float4 _Position0: Position0;
	float _Depth: TexCoord0;
};

struct TPS_Input_Depth {
	float _Depth: TexCoord0;
};

struct TPS_Output_Depth {
	float4 _Color0: Color0;
};

void VS_Depth_Static (const in TVS_Input_Depth_Static Input, out TVS_Output_Depth Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
	float3 PositionW = mul(float4(Input._Position0, 1), (float4x3)g_W);
	float3 NormalW = normalize(mul(Input._Normal0, (float3x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionW);
	Output._Depth = Output._Position0.z * 0.2 * g_DepthScale + (1 - abs(dot(NormalW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void VS_Depth_Skinned1 (const in TVS_Input_Depth_Skinned1 Input, out TVS_Output_Depth Output) {
	float4x3 S = g_SkinPallete[Input._BIndices0];
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	float3 PositionSW = normalize(mul(Input._Normal0, g_W));
	float3 NormalSW = normalize(mul(mul(Input._Normal0, (float3x3)S), (float3x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionSW);
	Output._Depth = Output._Position0.z * 0.2 * g_DepthScale + (1 - abs(dot(NormalSW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void VS_Depth_Skinned2 (const in TVS_Input_Depth_Skinned2 Input, out TVS_Output_Depth Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	float3 PositionSW = normalize(mul(Input._Normal0, g_W));
	float3 NormalSW = normalize(mul(mul(Input._Normal0, (float3x3)S), (float3x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionSW);
	Output._Depth = Output._Position0.z * 0.2 * g_DepthScale + (1 - abs(dot(NormalSW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void VS_Depth_Skinned3 (const in TVS_Input_Depth_Skinned3 Input, out TVS_Output_Depth Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	float3 PositionSW = normalize(mul(Input._Normal0, g_W));
	float3 NormalSW = normalize(mul(mul(Input._Normal0, (float3x3)S), (float3x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionSW);
	Output._Depth = Output._Position0.z * 0.2 * g_DepthScale + (1 - abs(dot(NormalSW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void VS_Depth_Skinned4 (const in TVS_Input_Depth_Skinned4 Input, out TVS_Output_Depth Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2] +
		g_SkinPallete[Input._BIndices0[3]] * Input._BWeights0[3]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	float3 PositionSW = normalize(mul(Input._Normal0, g_W));
	float3 NormalSW = normalize(mul(mul(Input._Normal0, (float3x3)S), (float3x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionSW);
	Output._Depth = Output._Position0.z * 0.2 * g_DepthScale + (1 - abs(dot(NormalSW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void PS_Depth (const in TPS_Input_Depth Input, out TPS_Output_Depth Output, uniform bool VSM) {
	if (VSM) {
		Output._Color0 = float4(Input._Depth, Input._Depth * Input._Depth, 0, 1);
	}
	else {
		Output._Color0 = float4(Input._Depth, 0, 0, 1);
	}
}

struct TVS_Input_DepthCube_Static {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
};

struct TVS_Input_DepthCube_Skinned1 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int _BIndices0: BlendIndices0;
};

struct TVS_Input_DepthCube_Skinned2 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVS_Input_DepthCube_Skinned3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVS_Input_DepthCube_Skinned4 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVS_Output_DepthCube {
	float4 _Position0: Position0;
	float4 _Depth: TexCoord0;
};

struct TPS_Input_DepthCube {
	float4 _Depth: TexCoord0;
};

struct TPS_Output_DepthCube {
	float4 _Color0: Color0;
};

void VS_DepthCube_Static (const in TVS_Input_DepthCube_Static Input, out TVS_Output_DepthCube Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
	float3 PositionW = mul(float4(Input._Position0, 1), (float4x3)g_W);
	float3 NormalW = normalize(mul(Input._Normal0, (float3x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionW);
	Output._Depth.xyz = (PositionW - g_LightPosW) * 0.2 * g_DepthScale;
	Output._Depth.w = (1 - abs(dot(NormalW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void VS_DepthCube_Skinned1 (const in TVS_Input_DepthCube_Skinned1 Input, out TVS_Output_DepthCube Output) {
	float4x3 S = g_SkinPallete[Input._BIndices0];
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	float3 PositionSW = mul(PositionS, (float4x3)g_W);
	float3 NormalSW = normalize(mul(mul(Input._Normal0, (float3x3)S), (float4x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionSW);
	Output._Depth.xyz = (PositionSW - g_LightPosW) * 0.2 * g_DepthScale;
	Output._Depth.w = (1 - abs(dot(NormalSW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void VS_DepthCube_Skinned2 (const in TVS_Input_DepthCube_Skinned2 Input, out TVS_Output_DepthCube Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	float3 PositionSW = mul(PositionS, (float4x3)g_W);
	float3 NormalSW = normalize(mul(mul(Input._Normal0, (float3x3)S), (float4x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionSW);
	Output._Depth.xyz = (PositionSW - g_LightPosW) * 0.2 * g_DepthScale;
	Output._Depth.w = (1 - abs(dot(NormalSW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void VS_DepthCube_Skinned3 (const in TVS_Input_DepthCube_Skinned3 Input, out TVS_Output_DepthCube Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	float3 PositionSW = mul(PositionS, (float4x3)g_W);
	float3 NormalSW = normalize(mul(mul(Input._Normal0, (float3x3)S), (float4x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionSW);
	Output._Depth.xyz = (PositionSW - g_LightPosW) * 0.2 * g_DepthScale;
	Output._Depth.w = (1 - abs(dot(NormalSW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void VS_DepthCube_Skinned4 (const in TVS_Input_DepthCube_Skinned4 Input, out TVS_Output_DepthCube Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2] +
		g_SkinPallete[Input._BIndices0[3]] * Input._BWeights0[3]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	float3 PositionSW = mul(PositionS, (float4x3)g_W);
	float3 NormalSW = normalize(mul(mul(Input._Normal0, (float3x3)S), (float4x3)g_W));
	float3 CamDirW = normalize(g_CamPosW - PositionSW);
	Output._Depth.xyz = (PositionSW - g_LightPosW) * 0.2 * g_DepthScale;
	Output._Depth.w = (1 - abs(dot(NormalSW, CamDirW))) * g_SlopeScaleDepthBias + g_DepthBias;
}

void PS_DepthCube (const in TPS_Input_DepthCube Input, out TPS_Output_DepthCube Output, uniform bool VSM) {
	float Depth = length(Input._Depth.xyz) + Input._Depth.w;
	if (VSM) {
		Output._Color0 = float4(Depth, Depth * Depth, 0, 1);
	}
	else {
		Output._Color0 = float4(Depth, 0, 0, 1);
	}
}

struct TVS_Input_Ambience_Static {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	float2 _TexCoord1: TexCoord1;
};

struct TVS_Input_Ambience_Skinned1 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int _BIndices0: BlendIndices0;
};

struct TVS_Input_Ambience_Skinned2 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVS_Input_Ambience_Skinned3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVS_Input_Ambience_Skinned4 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVS_Output_Ambience {
	float4 _Position0: Position0;
	float2 _TexCoord0: TexCoord0;
	float2 _TexCoord1: TexCoord1;
};

struct TPS_Input_Ambience {
	float2 _TexCoord0: TexCoord0;
	float2 _TexCoord1: TexCoord1;
};

struct TPS_Output_Ambience {
	float4 _Color0: Color0;
};

void VS_Ambience_Static (const in TVS_Input_Ambience_Static Input, out TVS_Output_Ambience Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
	Output._TexCoord0 = Input._TexCoord0;
	Output._TexCoord1 = Input._TexCoord1;
}

void VS_Ambience_Skinned1 (const in TVS_Input_Ambience_Skinned1 Input, out TVS_Output_Ambience Output) {
	float3 PositionS = mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0]);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._TexCoord0 = Input._TexCoord0;
	Output._TexCoord1 = float2(0, 0);
}

void VS_Ambience_Skinned2 (const in TVS_Input_Ambience_Skinned2 Input, out TVS_Output_Ambience Output) {
	float3 PositionS = (
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[0]]) * Input._BWeights0[0] + 
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[1]]) * Input._BWeights0[1]
	);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._TexCoord0 = Input._TexCoord0;
	Output._TexCoord1 = float2(0, 0);
}

void VS_Ambience_Skinned3 (const in TVS_Input_Ambience_Skinned3 Input, out TVS_Output_Ambience Output) {
	float3 PositionS = (
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[0]]) * Input._BWeights0[0] + 
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[1]]) * Input._BWeights0[1] + 
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[2]]) * Input._BWeights0[2]
	);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._TexCoord0 = Input._TexCoord0;
	Output._TexCoord1 = float2(0, 0);
}

void VS_Ambience_Skinned4 (const in TVS_Input_Ambience_Skinned4 Input, out TVS_Output_Ambience Output) {
	float3 PositionS = (
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[0]]) * Input._BWeights0[0] + 
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[1]]) * Input._BWeights0[1] + 
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[2]]) * Input._BWeights0[2] + 
		mul(float4(Input._Position0, 1), g_SkinPallete[Input._BIndices0[3]]) * Input._BWeights0[3]
	);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._TexCoord0 = Input._TexCoord0;
	Output._TexCoord1 = float2(0, 0);
}

void PS_Ambience (const in TPS_Input_Ambience Input, out TPS_Output_Ambience Output) {
	Output._Color0 = tex2D(SampDiffuse, Input._TexCoord0) * tex2D(SampLightMap, Input._TexCoord1);
	Output._Color0.w = 1;
}

struct TVS_Input_ShadowMap_Static {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
};

struct TVS_Input_ShadowMap_Skinned1 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int _BIndices0: BlendIndices0;
};

struct TVS_Input_ShadowMap_Skinned2 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVS_Input_ShadowMap_Skinned3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVS_Input_ShadowMap_Skinned4 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVS_Output_ShadowMap {
	float4 _Position0: Position0;
	float2 _TexCoord0: TexCoord0;
	float3 _TangentW: TexCoord1;
	float3 _BinormalW: TexCoord2;
	float3 _NormalW: TexCoord3;
	float4 _PositionW_Depth: TexCoord4;
	float4 _TexCoordDepth: TexCoord5;
};

struct TPS_Input_ShadowMap {
	float2 _TexCoord0: TexCoord0;
	float3 _TangentW: TexCoord1;
	float3 _BinormalW: TexCoord2;
	float3 _NormalW: TexCoord3;
	float4 _PositionW_Depth: TexCoord4;
	float4 _TexCoordDepth: TexCoord5;
};

struct TPS_Output_ShadowMap {
	float4 _Color0: Color0;
};

void VS_ShadowMap_Static (const in TVS_Input_ShadowMap_Static Input, out TVS_Output_ShadowMap Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
	Output._TangentW = mul(Input._Tangent0, (float3x3)g_W);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)g_W);
	Output._NormalW = mul(Input._Normal0, (float3x3)g_W);
	Output._TexCoord0 = Input._TexCoord0;
	float4 PosLightWVP = mul(float4(Input._Position0, 1), g_LightWVP);
	Output._PositionW_Depth.w = PosLightWVP.z * 0.2;
	Output._TexCoordDepth = mul(PosLightWVP, g_LightProj);
	Output._PositionW_Depth.xyz = mul(float4(Input._Position0, 1), g_W);
}

void VS_ShadowMap_Skinned1 (const in TVS_Input_ShadowMap_Skinned1 Input, out TVS_Output_ShadowMap Output) {
	float4x3 S = g_SkinPallete[Input._BIndices0];
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	float4x3 SW = mul(S, g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord0 = Input._TexCoord0;
	float4 PosLightWVP = mul(float4(PositionS, 1), g_LightWVP);
	Output._PositionW_Depth.w = PosLightWVP.z * 0.2;
	Output._TexCoordDepth = mul(PosLightWVP, g_LightProj);
	Output._PositionW_Depth.xyz = mul(float4(PositionS, 1), g_W);
}

void VS_ShadowMap_Skinned2 (const in TVS_Input_ShadowMap_Skinned2 Input, out TVS_Output_ShadowMap Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1]
	);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	float4x3 SW = mul(S, g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord0 = Input._TexCoord0;
	float4 PosLightWVP = mul(float4(PositionS, 1), g_LightWVP);
	Output._PositionW_Depth.w = PosLightWVP.z * 0.2;
	Output._TexCoordDepth = mul(PosLightWVP, g_LightProj);
	Output._PositionW_Depth.xyz = mul(float4(PositionS, 1), g_W);
}

void VS_ShadowMap_Skinned3 (const in TVS_Input_ShadowMap_Skinned3 Input, out TVS_Output_ShadowMap Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2]
	);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	float4x3 SW = mul(S, g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord0 = Input._TexCoord0;
	float4 PosLightWVP = mul(float4(PositionS, 1), g_LightWVP);
	Output._PositionW_Depth.w = PosLightWVP.z * 0.2;
	Output._TexCoordDepth = mul(PosLightWVP, g_LightProj);
	Output._PositionW_Depth.xyz = mul(float4(PositionS, 1), g_W);
}

void VS_ShadowMap_Skinned4 (const in TVS_Input_ShadowMap_Skinned4 Input, out TVS_Output_ShadowMap Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2] +
		g_SkinPallete[Input._BIndices0[3]] * Input._BWeights0[3]
	);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	float4x3 SW = mul(S, g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord0 = Input._TexCoord0;
	float4 PosLightWVP = mul(float4(PositionS, 1), g_LightWVP);
	Output._PositionW_Depth.w = PosLightWVP.z * 0.2;
	Output._TexCoordDepth = mul(PosLightWVP, g_LightProj);
	Output._PositionW_Depth.xyz = mul(float4(PositionS, 1), g_W);
}

void PS_ShadowMap (const in TPS_Input_ShadowMap Input, out TPS_Output_ShadowMap Output, uniform bool VSM, uniform int ShadowSmapleCount) {
	float3 LightToVertexW = Input._PositionW_Depth.xyz - g_LightPosW;
	float SpotAngle = dot(normalize(LightToVertexW), g_LightDir);
	clip(SpotAngle - g_SpotOutter);
	float3 TangentW = normalize(Input._TangentW);
	float3 BinormalW = normalize(Input._BinormalW);
	float3 NormalW = normalize(Input._NormalW);
	float3x3 TBNW = float3x3(TangentW, BinormalW, NormalW);
	float3 VertexToLightNormW = -normalize(LightToVertexW);
	float DepthP = Input._PositionW_Depth.w;
	
	float Shadow = 0;
	if (VSM) {
		float2 DepthL = tex2Dproj(SampDepth, Input._TexCoordDepth).xy;
		Shadow = (DepthP <= DepthL.x);
		float SQ1 = DepthL.y;
		float SQ2 = DepthL.x * DepthL.x;
		float Variance = min(max(SQ1 - SQ2, 0.0) + SHADOW_EPSILON, 1.0);
		float Dif = (DepthL.x - DepthP);
		float p = Variance / (Variance + Dif * Dif);
		Shadow = min(max(Shadow, p + 0.01), 1);
	}
	else {
		for (int i = 0; i < ShadowSmapleCount; i++) {
			float DepthL = tex2Dproj(SampDepth, Input._TexCoordDepth + g_ProjSampleMap[i]).x;
			if (DepthL > DepthP) Shadow += 1;
		}
		Shadow /= ShadowSmapleCount;
	}
	
	clip(Shadow - 0.01);
	float3 CameraToVertexW = Input._PositionW_Depth.xyz - g_CamPosW;
	float Att = smoothstep(g_LightAtt.y, g_LightAtt.x, DepthP) * smoothstep(g_SpotOutter, g_SpotInner, SpotAngle) * Shadow;
	float3 NormalMapW = normalize(mul(((float3)tex2D(SampNormals, Input._TexCoord0)) * 2  - 1, (float3x3) TBNW));
	float3 CamDirW = normalize(CameraToVertexW);
	float3 RefDirW = CamDirW - 2 * NormalMapW * dot(CamDirW, NormalMapW);
	float4 ColDiffuse = saturate(dot(NormalMapW, VertexToLightNormW)) * Att;
	float4 ColSpecular = pow(saturate(dot(RefDirW, VertexToLightNormW)), 40) * Att;
	Output._Color0 = tex2D(SampDiffuse, Input._TexCoord0) * ColDiffuse + ColSpecular;
}

struct TVS_Input_ShadowMapCube_Static {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
};

struct TVS_Input_ShadowMapCube_Skinned1 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int _BIndices0: BlendIndices0;
};

struct TVS_Input_ShadowMapCube_Skinned2 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVS_Input_ShadowMapCube_Skinned3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVS_Input_ShadowMapCube_Skinned4 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVS_Output_ShadowMapCube {
	float4 _Position0: Position0;
	float3 _PositionW: TexCoord0;
	float3 _TangentW: TexCoord1;
	float3 _BinormalW: TexCoord2;
	float3 _NormalW: TexCoord3;
	float2 _TexCoord0: TexCoord4;
};

struct TPS_Input_ShadowMapCube {
	float3 _PositionW: TexCoord0;
	float3 _TangentW: TexCoord1;
	float3 _BinormalW: TexCoord2;
	float3 _NormalW: TexCoord3;
	float2 _TexCoord0: TexCoord4;
};

struct TPS_Output_ShadowMapCube {
	float4 _Color0: Color0;
};

float GetDepthSample (int SamplerID, float3 TexCoord) {
	if (SamplerID == 0) return texCUBE(SampDepth0, TexCoord).x;
	if (SamplerID == 1) return texCUBE(SampDepth1, TexCoord).x;
	if (SamplerID == 2) return texCUBE(SampDepth2, TexCoord).x;
	if (SamplerID == 3) return texCUBE(SampDepth3, TexCoord).x;
	if (SamplerID == 4) return texCUBE(SampDepth4, TexCoord).x;
	if (SamplerID == 5) return texCUBE(SampDepth5, TexCoord).x;
}

float2 GetDepthSampleVSM (int SamplerID, float3 TexCoord) {
	if (SamplerID == 0) return texCUBE(SampDepth0, TexCoord).xy;
	if (SamplerID == 1) return texCUBE(SampDepth1, TexCoord).xy;
	if (SamplerID == 2) return texCUBE(SampDepth2, TexCoord).xy;
	if (SamplerID == 3) return texCUBE(SampDepth3, TexCoord).xy;
	if (SamplerID == 4) return texCUBE(SampDepth4, TexCoord).xy;
	if (SamplerID == 5) return texCUBE(SampDepth5, TexCoord).xy;
}

void VS_ShadowMapCube_Static (const in TVS_Input_ShadowMapCube_Static Input, out TVS_Output_ShadowMapCube Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
	Output._PositionW = mul(float4(Input._Position0, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)g_W);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)g_W);
	Output._NormalW = mul(Input._Normal0, (float3x3)g_W);
	Output._TexCoord0 = Input._TexCoord0;
}

void VS_ShadowMapCube_Skinned1 (const in TVS_Input_ShadowMapCube_Skinned1 Input, out TVS_Output_ShadowMapCube Output) {
	float4x3 S = g_SkinPallete[Input._BIndices0];
	float4x3 SW = mul(S, g_W);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._PositionW = mul(float4(PositionS, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord0 = Input._TexCoord0;
}

void VS_ShadowMapCube_Skinned2 (const in TVS_Input_ShadowMapCube_Skinned2 Input, out TVS_Output_ShadowMapCube Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1]
	);
	float4x3 SW = mul(S, g_W);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._PositionW = mul(float4(PositionS, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord0 = Input._TexCoord0;
}

void VS_ShadowMapCube_Skinned3 (const in TVS_Input_ShadowMapCube_Skinned3 Input, out TVS_Output_ShadowMapCube Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2]
	);
	float4x3 SW = mul(S, g_W);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._PositionW = mul(float4(PositionS, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord0 = Input._TexCoord0;
}

void VS_ShadowMapCube_Skinned4 (const in TVS_Input_ShadowMapCube_Skinned4 Input, out TVS_Output_ShadowMapCube Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2] +
		g_SkinPallete[Input._BIndices0[3]] * Input._BWeights0[3]
	);
	float4x3 SW = mul(S, g_W);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._PositionW = mul(float4(PositionS, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord0 = Input._TexCoord0;
}

void PS_ShadowMapCube (const in TPS_Input_ShadowMapCube Input, out TPS_Output_ShadowMapCube Output, uniform bool VSM, uniform int ShadowSmapleCount) {
	float3 PosToLightW = g_LightPosW - Input._PositionW;
	float Att = smoothstep(g_LightAtt.y, g_LightAtt.x, dot(PosToLightW, PosToLightW));
	clip(Att - 0.01);
	
	float3 CamDirW = normalize(Input._PositionW - g_CamPosW);
	float3 TangentW = normalize(Input._TangentW);
	float3 BinormalW = normalize(Input._BinormalW);
	float3 NormalW = normalize(Input._NormalW);
	float3x3 TBNW = float3x3(TangentW, BinormalW, NormalW);
	float3 NormalMapW = normalize(mul(((float3)tex2D(SampNormals, Input._TexCoord0)) * 2  - 1, (float3x3) TBNW));
	float3 RefDirW = CamDirW - 2 * NormalMapW * dot(CamDirW, NormalMapW);
	float4 ColDiffuse = float4(0, 0, 0, 0);
	float4 ColSpecular = float4(0, 0, 0, 0);
	
	float3 LightToPosW = -PosToLightW;
	PosToLightW = normalize(PosToLightW);
	float PDepth = length(LightToPosW) * 0.2;
	LightToPosW = normalize(LightToPosW);
	if (VSM) {
		float2 LDepth = float2(0, 0);
		float Samples = 0;
		for (int j = 0; j < 16; j++) {
			LDepth += GetDepthSampleVSM(0, LightToPosW + g_CubeSampleMap[j].xyz) * g_CubeSampleMap[j].w;
		}
		float LitFactor = (PDepth <= LDepth.x);
		float SQ1 = LDepth.y;
		float SQ2 = LDepth.x * LDepth.x;
		float Variance = min(max(SQ1 - SQ2, 0.0) + SHADOW_EPSILON, 1.0);
		float Dif = (LDepth.x - PDepth);
		float p = Variance / (Variance + Dif * Dif);
		Att *= min(max(LitFactor, p + 0.01), 1);
	}
	else {
		float LDepth;
		float Samples = 0;
		for (int j = 0; j < ShadowSmapleCount; j++) {
			LDepth = GetDepthSample(0, LightToPosW + g_CubeSampleMap[j]);
			if (LDepth > PDepth) Samples += 1;
		}
		Att *= Samples / ShadowSmapleCount;
	}
	clip(Att - 0.01);
	ColDiffuse += saturate(dot(NormalMapW, PosToLightW)) * float4(g_LightColor, 1) * Att;
	ColSpecular += pow(saturate(dot(RefDirW, PosToLightW)), 40) * float4(g_LightColor, 1) * Att;
		
	Output._Color0 = ColDiffuse * tex2D(SampDiffuse, Input._TexCoord0) + ColSpecular;
}

VertexShader VS_Depth_Arr[10] = {
	compile vs_2_0 VS_Depth_Static(),
	compile vs_2_0 VS_Depth_Skinned1(),
	compile vs_2_0 VS_Depth_Skinned2(),
	compile vs_2_0 VS_Depth_Skinned3(),
	compile vs_2_0 VS_Depth_Skinned4(),
	compile vs_2_0 VS_DepthCube_Static(),
	compile vs_2_0 VS_DepthCube_Skinned1(),
	compile vs_2_0 VS_DepthCube_Skinned2(),
	compile vs_2_0 VS_DepthCube_Skinned3(),
	compile vs_2_0 VS_DepthCube_Skinned4()
};

PixelShader PS_Depth_Arr[4] = {
	compile ps_2_0 PS_Depth(false),
	compile ps_2_0 PS_Depth(true),
	compile ps_2_0 PS_DepthCube(false),
	compile ps_2_0 PS_DepthCube(true)
};

VertexShader VS_Ambience_Arr[5] = {
	compile vs_2_0 VS_Ambience_Static(),
	compile vs_2_0 VS_Ambience_Skinned1(),
	compile vs_2_0 VS_Ambience_Skinned2(),
	compile vs_2_0 VS_Ambience_Skinned3(),
	compile vs_2_0 VS_Ambience_Skinned4()
};

PixelShader PS_Ambience_Arr[1] = {
	compile ps_2_0 PS_Ambience()
};

VertexShader VS_ShadowMap_Arr[10] = {
	compile vs_2_0 VS_ShadowMap_Static(),
	compile vs_2_0 VS_ShadowMap_Skinned1(),
	compile vs_2_0 VS_ShadowMap_Skinned2(),
	compile vs_2_0 VS_ShadowMap_Skinned3(),
	compile vs_2_0 VS_ShadowMap_Skinned4(),
	compile vs_2_0 VS_ShadowMapCube_Static(),
	compile vs_2_0 VS_ShadowMapCube_Skinned1(),
	compile vs_2_0 VS_ShadowMapCube_Skinned2(),
	compile vs_2_0 VS_ShadowMapCube_Skinned3(),
	compile vs_2_0 VS_ShadowMapCube_Skinned4(),
};

PixelShader PS_ShadowMap_Arr[10] = {
	compile ps_2_0 PS_ShadowMap(false, 1), 
    compile ps_3_0 PS_ShadowMap(false, 4), 
    compile ps_3_0 PS_ShadowMap(false, 9), 
    compile ps_3_0 PS_ShadowMap(false, 16),
	compile ps_2_0 PS_ShadowMap(true, 0),
	compile ps_2_0 PS_ShadowMapCube(false, 1), 
	compile ps_3_0 PS_ShadowMapCube(false, 4), 
	compile ps_3_0 PS_ShadowMapCube(false, 9), 
	compile ps_3_0 PS_ShadowMapCube(false, 16),
	compile ps_3_0 PS_ShadowMapCube(true, 0)
};

technique g2Depth {
	pass P0 {
		VertexShader = VS_Depth_Arr[VS_Index];
		PixelShader = PS_Depth_Arr[PS_Index];
	}
}

technique g2Ambience {
	pass P0 {
		VertexShader = VS_Ambience_Arr[VS_Index];
		PixelShader = PS_Ambience_Arr[PS_Index];
	}
}

technique g2ShadowMap {
	pass P0 {
		VertexShader = VS_ShadowMap_Arr[VS_Index];
		PixelShader = PS_ShadowMap_Arr[PS_Index];
	}
}

struct TVS_Input_LightMap {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	float2 _TexCoord1: TexCoord1;
};

struct TVS_Output_LightMap {
	float4 _Position0: Position0;
	float4 _TexCoord: TexCoord0;
	float3 _TangentW: TexCoord1;
	float3 _BinormalW: TexCoord2;
	float3 _NormalW: TexCoord3;
	float3 _PositionW: TexCoord4;
	float4 _ShadowProjTex: TexCoord5;
	float2 _Depth: TexCoord6;
};

struct TPS_Input_LightMap {
	float4 _TexCoord: TexCoord0;
	float3 _TangentW: TexCoord1;
	float3 _BinormalW: TexCoord2;
	float3 _NormalW: TexCoord3;
	float3 _PositionW: TexCoord4;
	float4 _ShadowProjTex: TexCoord5;
	float2 _Depth: TexCoord6;
};

struct TPS_Output_LightMap {
	float4 _Color0: Color0;
};

void VS_LightMap (const in TVS_Input_LightMap Input, out TVS_Output_LightMap Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
	Output._TexCoord.xy = Input._TexCoord0;
	Output._TexCoord.zw = Input._TexCoord1;
	Output._TangentW = mul(Input._Tangent0, (float3x3)g_W);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)g_W);
	Output._NormalW = mul(Input._Normal0, (float3x3)g_W);
	Output._PositionW = mul(float4(Input._Position0, 1), (float4x3)g_W);
	float4 PosShadowProjWVP = mul(float4(Input._Position0, 1), g_LightWVP);
	Output._Depth = float2(PosShadowProjWVP.z, PosShadowProjWVP.w);
	Output._ShadowProjTex = mul(PosShadowProjWVP, g_LightProj);
}

void PS_LightMap (const in TPS_Input_LightMap Input, out TPS_Output_LightMap Output) {
	float3 TangentW = normalize(Input._TangentW);
	float3 BinormalW = normalize(Input._BinormalW);
	float3 NormalW = normalize(Input._NormalW);
	float3x3 TBNW = float3x3(TangentW, BinormalW, NormalW);
	float3 NormalMapW = normalize(mul(((float3)tex2D(SampNormals, Input._TexCoord.xy)) * 2 - 1, (float3x3) TBNW));
	float3 CamDirW = normalize(Input._PositionW - g_CamPosW);
	float3 RefDirW = CamDirW - 2 * NormalMapW * dot(CamDirW, NormalMapW);
	float Shadow = 1;
	if ((Input._Depth.x / Input._Depth.y) > tex2Dproj(SampShadowProj, Input._ShadowProjTex).x) Shadow = 0;
	float3 LightMap = tex2D(SampLightMap, Input._TexCoord.zw).xyz * Shadow * g_LightColor;
	float3 Specular = pow(saturate(dot(RefDirW, g_LightDir)), 15) * LightMap;
	float3 Diffuse = saturate(dot(NormalMapW, g_LightDir) * LightMap) + g_Ambient;
	Output._Color0.xyz = tex2D(SampDiffuse, Input._TexCoord.xy).xyz * Diffuse + tex2D(SampSpecular, Input._TexCoord.xy).xyz * Specular;
	Output._Color0.w = 1;
}

technique g2LightMap {
	pass P0 {
		VertexShader = compile vs_2_0 VS_LightMap();
		PixelShader = compile ps_2_0 PS_LightMap();
	}
}

struct TVS_Input_Simple_Static {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
};

struct TVS_Input_Simple_Skinned1 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int _BIndices0: BlendIndices0;
};

struct TVS_Input_Simple_Skinned2 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVS_Input_Simple_Skinned3 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVS_Input_Simple_Skinned4 {
	float3 _Position0: Position0;
	float3 _Tangent0: Tangent0;
	float3 _Binormal0: Binormal0;
	float3 _Normal0: Normal0;
	float2 _TexCoord0: TexCoord0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVS_Output_Simple {
	float4 _Position0: Position0;
	float2 _TexCoord: TexCoord0;
	float3 _TangentW: TexCoord1;
	float3 _BinormalW: TexCoord2;
	float3 _NormalW: TexCoord3;
	float3 _PositionW: TexCoord4;
};

struct TPS_Input_Simple {
	float2 _TexCoord: TexCoord0;
	float3 _TangentW: TexCoord1;
	float3 _BinormalW: TexCoord2;
	float3 _NormalW: TexCoord3;
	float3 _PositionW: TexCoord4;
};

struct TPS_Output_Simple {
	float4 _Color0: Color0;
};

void VS_Simple_Static (const in TVS_Input_Simple_Static Input, out TVS_Output_Simple Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
	Output._PositionW = mul(float4(Input._Position0, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)g_W);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)g_W);
	Output._NormalW = mul(Input._Normal0, (float3x3)g_W);
	Output._TexCoord = Input._TexCoord0;
}

void VS_Simple_Skinned1 (const in TVS_Input_Simple_Skinned1 Input, out TVS_Output_Simple Output) {
	float4x3 S = g_SkinPallete[Input._BIndices0];
	float4x3 SW = mul(S, g_W);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._PositionW = mul(float4(PositionS, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord = Input._TexCoord0;
}

void VS_Simple_Skinned2 (const in TVS_Input_Simple_Skinned2 Input, out TVS_Output_Simple Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1]
	);
	float4x3 SW = mul(S, g_W);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._PositionW = mul(float4(PositionS, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord = Input._TexCoord0;
}

void VS_Simple_Skinned3 (const in TVS_Input_Simple_Skinned3 Input, out TVS_Output_Simple Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2]
	);
	float4x3 SW = mul(S, g_W);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._PositionW = mul(float4(PositionS, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord = Input._TexCoord0;
}

void VS_Simple_Skinned4 (const in TVS_Input_Simple_Skinned4 Input, out TVS_Output_Simple Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2] +
		g_SkinPallete[Input._BIndices0[3]] * Input._BWeights0[3]
	);
	float4x3 SW = mul(S, g_W);
	float3 PositionS = mul(float4(Input._Position0, 1), S);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
	Output._PositionW = mul(float4(PositionS, 1), (float4x3)g_W);
	Output._TangentW = mul(Input._Tangent0, (float3x3)SW);
	Output._BinormalW = mul(Input._Binormal0, (float3x3)SW);
	Output._NormalW = mul(Input._Normal0, (float3x3)SW);
	Output._TexCoord = Input._TexCoord0;
}

float g_CharAlpha = 1;

void PS_Simple (const in TPS_Input_Simple Input, out TPS_Output_Simple Output) {
	float3 TangentW = normalize(Input._TangentW);
	float3 BinormalW = normalize(Input._BinormalW);
	float3 NormalW = normalize(Input._NormalW);
	float3x3 TBNW = float3x3(TangentW, BinormalW, NormalW);
	float3 NormalMapW = normalize(mul(((float3)tex2D(SampNormals, Input._TexCoord)) * 2 - 1, (float3x3) TBNW));
	float3 CamDirW = normalize(Input._PositionW - g_CamPosW);
	float3 RefDirW = CamDirW - 2 * NormalMapW * dot(CamDirW, NormalMapW);
	float3 Specular = pow(saturate(dot(RefDirW, g_LightDir)), 15) * g_LightColor;
	float3 Diffuse = saturate(dot(NormalMapW, g_LightDir) * g_LightColor) + g_Ambient;
	Output._Color0.xyz = tex2D(SampDiffuse, Input._TexCoord).xyz * Diffuse + tex2D(SampSpecular, Input._TexCoord).xyz * Specular;
	Output._Color0.w = g_CharAlpha;
}

VertexShader VS_Simple_Arr[5] = {
	compile vs_2_0 VS_Simple_Static(),
	compile vs_2_0 VS_Simple_Skinned1(),
	compile vs_2_0 VS_Simple_Skinned2(),
	compile vs_2_0 VS_Simple_Skinned3(),
	compile vs_2_0 VS_Simple_Skinned4(),
};

technique g2Simple {
	pass P0 {
		VertexShader = VS_Simple_Arr[VS_Index];
		PixelShader = compile ps_2_0 PS_Simple();
	}
}

struct TVS_Input_ShadowProj_Static {
	float3 _Position0: Position0;
};

struct TVS_Input_ShadowProj_Skinned1 {
	float3 _Position0: Position0;
	int _BIndices0: BlendIndices0;
};

struct TVS_Input_ShadowProj_Skinned2 {
	float3 _Position0: Position0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVS_Input_ShadowProj_Skinned3 {
	float3 _Position0: Position0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVS_Input_ShadowProj_Skinned4 {
	float3 _Position0: Position0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVS_Output_ShadowProj {
	float4 _Position0: Position0;
};

struct TPS_Output_ShadowProj {
	float4 _Color0: Color0;
};

void VS_ShadowProj_Static (const in TVS_Input_ShadowProj_Static Input, out TVS_Output_ShadowProj Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
}

void VS_ShadowProj_Skinned1 (const in TVS_Input_ShadowProj_Skinned1 Input, out TVS_Output_ShadowProj Output) {
	float3 PositionS = mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0]);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
}

void VS_ShadowProj_Skinned2 (const in TVS_Input_ShadowProj_Skinned2 Input, out TVS_Output_ShadowProj Output) {
	float3 PositionS = (
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[0]]) * Input._BWeights0[0] + 
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[1]]) * Input._BWeights0[1]
	);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
}

void VS_ShadowProj_Skinned3 (const in TVS_Input_ShadowProj_Skinned3 Input, out TVS_Output_ShadowProj Output) {
	float3 PositionS = (
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[0]]) * Input._BWeights0[0] + 
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[1]]) * Input._BWeights0[1] + 
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[2]]) * Input._BWeights0[2]
	);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
}

void VS_ShadowProj_Skinned4 (const in TVS_Input_ShadowProj_Skinned4 Input, out TVS_Output_ShadowProj Output) {
	float3 PositionS = (
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[0]]) * Input._BWeights0[0] + 
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[1]]) * Input._BWeights0[1] + 
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[2]]) * Input._BWeights0[2] + 
		mul(float4(Input._Position0, 1), (float4x3)g_SkinPallete[Input._BIndices0[3]]) * Input._BWeights0[3]
	);
	Output._Position0 = mul(float4(PositionS, 1), g_WVP);
}

void PS_ShadowProj (out TPS_Output_Simple Output) {
	Output._Color0 = float4(0, 0, 0, 1);
}

VertexShader VS_ShadowProj_Arr[5] = {
	compile vs_2_0 VS_ShadowProj_Static(),
	compile vs_2_0 VS_ShadowProj_Skinned1(),
	compile vs_2_0 VS_ShadowProj_Skinned2(),
	compile vs_2_0 VS_ShadowProj_Skinned3(),
	compile vs_2_0 VS_ShadowProj_Skinned4(),
};

technique g2ShadowProj {
	pass P0 {
		VertexShader = VS_ShadowProj_Arr[VS_Index];
		PixelShader = compile ps_2_0 PS_ShadowProj();
	}
}

struct TVS_Input_ShadowDepth_Static {
	float3 _Position0: Position0;
};

struct TVS_Input_ShadowDepth_Skinned1 {
	float3 _Position0: Position0;
	int _BIndices0: BlendIndices0;
};

struct TVS_Input_ShadowDepth_Skinned2 {
	float3 _Position0: Position0;
	int2 _BIndices0: BlendIndices0;
	float2 _BWeights0: BlendWeight0;
};

struct TVS_Input_ShadowDepth_Skinned3 {
	float3 _Position0: Position0;
	int3 _BIndices0: BlendIndices0;
	float3 _BWeights0: BlendWeight0;
};

struct TVS_Input_ShadowDepth_Skinned4 {
	float3 _Position0: Position0;
	int4 _BIndices0: BlendIndices0;
	float4 _BWeights0: BlendWeight0;
};

struct TVS_Output_ShadowDepth {
	float4 _Position0: Position0;
	float2 _Depth: TexCoord0;
};

struct TPS_Input_ShadowDepth {
	float2 _Depth: TexCoord0;
};

struct TPS_Output_ShadowDepth {
	float4 _Color0: Color0;
};

void VS_ShadowDepth_Static (const in TVS_Input_ShadowDepth_Static Input, out TVS_Output_ShadowDepth Output) {
	Output._Position0 = mul(float4(Input._Position0, 1), g_WVP);
	Output._Depth = float2(Output._Position0.z, Output._Position0.w);
}

void VS_ShadowDepth_Skinned1 (const in TVS_Input_ShadowDepth_Skinned1 Input, out TVS_Output_ShadowDepth Output) {
	float4x3 S = g_SkinPallete[Input._BIndices0];
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	Output._Depth = float2(Output._Position0.z, Output._Position0.w);
}

void VS_ShadowDepth_Skinned2 (const in TVS_Input_ShadowDepth_Skinned2 Input, out TVS_Output_ShadowDepth Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	Output._Depth = float2(Output._Position0.z, Output._Position0.w);
}

void VS_ShadowDepth_Skinned3 (const in TVS_Input_ShadowDepth_Skinned3 Input, out TVS_Output_ShadowDepth Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	Output._Depth = float2(Output._Position0.z, Output._Position0.w);
}

void VS_ShadowDepth_Skinned4 (const in TVS_Input_ShadowDepth_Skinned4 Input, out TVS_Output_ShadowDepth Output) {
	float4x3 S = (
		g_SkinPallete[Input._BIndices0[0]] * Input._BWeights0[0] +
		g_SkinPallete[Input._BIndices0[1]] * Input._BWeights0[1] +
		g_SkinPallete[Input._BIndices0[2]] * Input._BWeights0[2] +
		g_SkinPallete[Input._BIndices0[3]] * Input._BWeights0[3]
	);
	float4 PositionS = float4(mul(float4(Input._Position0, 1), S), 1);
	Output._Position0 = mul(PositionS, g_WVP);
	Output._Depth = float2(Output._Position0.z, Output._Position0.w);
}

void PS_ShadowDepth (const in TPS_Input_ShadowDepth Input, out TPS_Output_ShadowDepth Output) {
	Output._Color0 = float4(Input._Depth.x / Input._Depth.y, 0, 0, 1);
}

VertexShader VS_ShadowDepth_Arr[5] = {
	compile vs_2_0 VS_ShadowDepth_Static(),
	compile vs_2_0 VS_ShadowDepth_Skinned1(),
	compile vs_2_0 VS_ShadowDepth_Skinned2(),
	compile vs_2_0 VS_ShadowDepth_Skinned3(),
	compile vs_2_0 VS_ShadowDepth_Skinned4(),
};

technique g2ShadowDepth {
	pass P0 {
		VertexShader = VS_ShadowDepth_Arr[VS_Index];
		PixelShader = compile ps_2_0 PS_ShadowDepth();
	}
}