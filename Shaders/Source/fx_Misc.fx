/*--------------------------------*\
      Gen2 Fireball Shader
\*--------------------------------*/

uniform extern int VS_Index;
uniform extern int PS_Index;

uniform extern float4x3 g_W;
uniform extern float4x4 g_WVP;

uniform extern float4 g_DepthDir;
uniform extern float g_DepthMax;
uniform extern float2 g_BlurOffset;

void VS_DepthLinear (
    const in float3 InPosition0: Position0, 
    out float4 OutPosition0: Position0,
  	out float OutDepth: TexCoord0
  ) {
	OutPosition0 = mul(float4(InPosition0, 1), g_WVP);
  OutDepth = (dot(g_DepthDir.xyz, mul(float4(InPosition0, 1), (float4x3)g_W).xyz) - g_DepthDir.w) / g_DepthMax;
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

//g_W
//g_WVP
//g_DepthDir
//g_DepthMax
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

texture TexBlur;

sampler SampBlur = sampler_state {
	Texture = <TexBlur>;
	MinFilter = Point;  
	MagFilter = Point;
	MipFilter = None;
	AddressU  = Clamp;
	AddressV  = Clamp;
};

void PS_Blur (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
	OutColor0 = tex2D(SampBlur, InTexCoord0) * 0.3;
	OutColor0 += tex2D(SampBlur, InTexCoord0 + g_BlurOffset) * 0.2;
	OutColor0 += tex2D(SampBlur, InTexCoord0 - g_BlurOffset) * 0.2;
	OutColor0 += tex2D(SampBlur, InTexCoord0 + g_BlurOffset * 2) * 0.1;
	OutColor0 += tex2D(SampBlur, InTexCoord0 - g_BlurOffset * 2) * 0.1;
	OutColor0 += tex2D(SampBlur, InTexCoord0 + g_BlurOffset * 3) * 0.05;
	OutColor0 += tex2D(SampBlur, InTexCoord0 - g_BlurOffset * 3) * 0.05;
}

//g_BlurOffset
technique Blur {
	pass {
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur();
	}
}
