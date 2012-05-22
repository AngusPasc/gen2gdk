/*--------------------------------*\
      Gen2 Utility Shaders
\*--------------------------------*/

static const int MAX_BONE_COUNT = 80;
uniform extern int VS_Index = 0;
uniform extern int PS_Index = 0;
uniform float4x3 g_SkinPallete[MAX_BONE_COUNT];
uniform float g_Bias = 0;

//DepthLinear BEGIN
struct TVarDepthLinear {
  float4x3 W;
  float4x4 WVP;
  float4 DepthDirW;
  float DepthMaxRcp;
};
TVarDepthLinear VarDepthLinear;

void VS_DepthLinear0 (
  const in float3 InPosition0: Position0, 
  out float4 OutPosition0: Position0,
	out float OutDepth: TexCoord0
) {
	OutPosition0 = mul(float4(InPosition0, 1), VarDepthLinear.WVP);
  OutDepth = (dot(VarDepthLinear.DepthDirW.xyz, mul(float4(InPosition0, 1), VarDepthLinear.W).xyz) - VarDepthLinear.DepthDirW.w) * VarDepthLinear.DepthMaxRcp;
}

void VS_DepthLinear1 (
  const in float3 InPosition0: Position0, 
  const in int InBIndices0: BlendIndices0,
  out float4 OutPosition0: Position0,
	out float OutDepth: TexCoord0
) {
	float3 PositionS = mul(float4(InPosition0, 1), g_SkinPallete[InBIndices0]);
	OutPosition0 = mul(float4(PositionS, 1), VarDepthLinear.WVP);
  OutDepth = (dot(VarDepthLinear.DepthDirW.xyz, mul(float4(PositionS, 1), VarDepthLinear.W).xyz) - VarDepthLinear.DepthDirW.w) * VarDepthLinear.DepthMaxRcp;
}

void VS_DepthLinear2 (
  const in float3 InPosition0: Position0, 
  const in int2 InBIndices0: BlendIndices0,
	const in float2 InBWeights0: BlendWeight0,
  out float4 OutPosition0: Position0,
	out float OutDepth: TexCoord0
) {
  float4x3 S = (
		g_SkinPallete[InBIndices0[0]] * InBWeights0[0] +
		g_SkinPallete[InBIndices0[1]] * InBWeights0[1]
	);
	float3 PositionS = mul(float4(InPosition0, 1), S);
	OutPosition0 = mul(float4(PositionS, 1), VarDepthLinear.WVP);
  OutDepth = (dot(VarDepthLinear.DepthDirW.xyz, mul(float4(PositionS, 1), VarDepthLinear.W).xyz) - VarDepthLinear.DepthDirW.w) * VarDepthLinear.DepthMaxRcp;
}

void VS_DepthLinear3 (
  const in float3 InPosition0: Position0, 
  const in int3 InBIndices0: BlendIndices0,
	const in float3 InBWeights0: BlendWeight0,
  out float4 OutPosition0: Position0,
	out float OutDepth: TexCoord0
) {
  float4x3 S = (
		g_SkinPallete[InBIndices0[0]] * InBWeights0[0] +
		g_SkinPallete[InBIndices0[1]] * InBWeights0[1] +
		g_SkinPallete[InBIndices0[2]] * InBWeights0[2]
	);
	float3 PositionS = mul(float4(InPosition0, 1), S);
	OutPosition0 = mul(float4(PositionS, 1), VarDepthLinear.WVP);
  OutDepth = (dot(VarDepthLinear.DepthDirW.xyz, mul(float4(PositionS, 1), VarDepthLinear.W).xyz) - VarDepthLinear.DepthDirW.w) * VarDepthLinear.DepthMaxRcp;
}

void VS_DepthLinear4 (
  const in float3 InPosition0: Position0, 
  const in int4 InBIndices0: BlendIndices0,
	const in float4 InBWeights0: BlendWeight0,
  out float4 OutPosition0: Position0,
	out float OutDepth: TexCoord0
) {
  float4x3 S = (
		g_SkinPallete[InBIndices0[0]] * InBWeights0[0] +
		g_SkinPallete[InBIndices0[1]] * InBWeights0[1] +
		g_SkinPallete[InBIndices0[2]] * InBWeights0[2] +
		g_SkinPallete[InBIndices0[3]] * InBWeights0[3]
	);
	float3 PositionS = mul(float4(InPosition0, 1), S);
	OutPosition0 = mul(float4(PositionS, 1), VarDepthLinear.WVP);
  OutDepth = (dot(VarDepthLinear.DepthDirW.xyz, mul(float4(PositionS, 1), VarDepthLinear.W).xyz) - VarDepthLinear.DepthDirW.w) * VarDepthLinear.DepthMaxRcp;
}

void PS_DepthLinear (
  const in float InDepth: TexCoord0, 
  out float4 OutColor0: Color0
) {
  OutColor0 = float4(saturate(InDepth + g_Bias), 0, 0, 1);
}

void PS_DepthLinearVSM (
  const in float InDepth: TexCoord0, 
  out float4 OutColor0: Color0
) {
  float d = saturate(InDepth + g_Bias); 
  OutColor0 = float4(d, d * d, 0, 1);
}

VertexShader VS_DepthLinear_Arr[5] = {
	compile vs_2_0 VS_DepthLinear0(),
  compile vs_2_0 VS_DepthLinear1(),
  compile vs_2_0 VS_DepthLinear2(),
  compile vs_2_0 VS_DepthLinear3(),
  compile vs_2_0 VS_DepthLinear4()
};

technique DepthLinear {
  pass Standard {
    VertexShader = VS_DepthLinear_Arr[VS_Index];
    PixelShader = compile ps_2_0 PS_DepthLinear();
  }
  pass VSM {
    VertexShader = VS_DepthLinear_Arr[VS_Index];
    PixelShader = compile ps_2_0 PS_DepthLinearVSM();
  }
}
//DepthLinear END

//DepthRadial BEGIN
struct TVarDepthRadial {
  float4x3 W;
  float4x4 WVP;
  float3 DepthOriginW;
  float DepthMaxRcp;
};
TVarDepthRadial VarDepthRadial;

void VS_DepthRadial (
  const in float3 InPosition0: Position0, 
  out float4 OutPosition0: Position0,
  out float3 OutDepth: TexCoord0
) {
	OutPosition0 = mul(float4(InPosition0, 1), VarDepthRadial.WVP);
  OutDepth = mul(float4(InPosition0, 1), (float4x3)VarDepthRadial.W).xyz - VarDepthRadial.DepthOriginW;
}

void PS_DepthRadial (
  const in float3 InDepth: TexCoord0, 
  out float4 OutColor0: Color0
) {
  OutColor0 = float4(saturate(length(InDepth) * VarDepthRadial.DepthMaxRcp), 0, 0, 1);
}

technique DepthRadial {
  pass P0 {
    VertexShader = compile vs_2_0 VS_DepthRadial();
    PixelShader = compile ps_2_0 PS_DepthRadial();
  }
}
//DepthRadial END
