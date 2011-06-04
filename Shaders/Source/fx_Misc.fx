/*--------------------------------*\
      Gen2 Utility Shaders
\*--------------------------------*/

uniform extern int VS_Index;
uniform extern int PS_Index;

//DepthLinear BEGIN
struct TVarDepthLinear {
  float4x3 W;
  float4x4 WVP;
  float4 DepthDirW;
  float DepthMaxRcp;
};
TVarDepthLinear VarDepthLinear;

void VS_DepthLinear (
  const in float3 InPosition0: Position0, 
  out float4 OutPosition0: Position0,
	out float OutDepth: TexCoord0
) {
	OutPosition0 = mul(float4(InPosition0, 1), VarDepthLinear.WVP);
  OutDepth = (dot(VarDepthLinear.DepthDirW.xyz, mul(float4(InPosition0, 1), VarDepthLinear.W).xyz) - VarDepthLinear.DepthDirW.w) * VarDepthLinear.DepthMaxRcp;
}

void PS_DepthLinear (
  const in float InDepth: TexCoord0, 
  out float4 OutColor0: Color0
) {
  OutColor0 = float4(saturate(InDepth), 0, 0, 1);
}

void PS_DepthLinearVSM (
  const in float InDepth: TexCoord0, 
  out float4 OutColor0: Color0
) {
  float d = saturate(InDepth);
  OutColor0 = float4(d, d * d, 0, 1);
}

technique DepthLinear {
  pass Standard {
    VertexShader = compile vs_2_0 VS_DepthLinear();
    PixelShader = compile ps_2_0 PS_DepthLinear();
  }
  pass VSM {
    VertexShader = compile vs_2_0 VS_DepthLinear();
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
