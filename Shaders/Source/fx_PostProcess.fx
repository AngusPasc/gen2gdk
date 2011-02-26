/*--------------------------------*\
      Gen2 Fireball Shader
\*--------------------------------*/

uniform extern int PS_Index;

sampler s0: register(s0);
sampler s1: register(s1);
sampler s2: register(s2);
sampler s3: register(s3);

//Blur BEGIN
struct TVarBlur {
  float2 Offset;
};
TVarBlur VarBlur;
void PS_Blur3x3 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.25;
  Col += tex2D(s0, InTexCoord0) * 0.5;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.25;
	OutColor0 = Col;
}
void PS_Blur5x5 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.095;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.25;
  Col += tex2D(s0, InTexCoord0) * 0.31;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.25;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.095;
	OutColor0 = Col;
}
void PS_Blur7x7 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 3 * VarBlur.Offset) * 0.049;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.138;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.2;
  Col += tex2D(s0, InTexCoord0) * 0.226;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.2;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.138;
  Col += tex2D(s0, InTexCoord0 + 3 * VarBlur.Offset) * 0.049;
	OutColor0 = Col;
}
void PS_Blur9x9 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 4 * VarBlur.Offset) * 0.03;
  Col += tex2D(s0, InTexCoord0 - 3 * VarBlur.Offset) * 0.086;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.133;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.163;
  Col += tex2D(s0, InTexCoord0) * 0.176;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.163;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.133;
  Col += tex2D(s0, InTexCoord0 + 3 * VarBlur.Offset) * 0.086;
  Col += tex2D(s0, InTexCoord0 + 4 * VarBlur.Offset) * 0.03;
	OutColor0 = Col;
}
void PS_Blur11x11 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 5 * VarBlur.Offset) * 0.02;
  Col += tex2D(s0, InTexCoord0 - 4 * VarBlur.Offset) * 0.059;
  Col += tex2D(s0, InTexCoord0 - 3 * VarBlur.Offset) * 0.093;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.119;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.136;
  Col += tex2D(s0, InTexCoord0) * 0.146;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.136;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.119;
  Col += tex2D(s0, InTexCoord0 + 3 * VarBlur.Offset) * 0.093;
  Col += tex2D(s0, InTexCoord0 + 4 * VarBlur.Offset) * 0.059;
  Col += tex2D(s0, InTexCoord0 + 5 * VarBlur.Offset) * 0.02;
	OutColor0 = Col;
}
void PS_Blur13x13 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 6 * VarBlur.Offset) * 0.014;
  Col += tex2D(s0, InTexCoord0 - 5 * VarBlur.Offset) * 0.042;
  Col += tex2D(s0, InTexCoord0 - 4 * VarBlur.Offset) * 0.068;
  Col += tex2D(s0, InTexCoord0 - 3 * VarBlur.Offset) * 0.09;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.106;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.117;
  Col += tex2D(s0, InTexCoord0) * 0.126;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.117;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.106;
  Col += tex2D(s0, InTexCoord0 + 3 * VarBlur.Offset) * 0.09;
  Col += tex2D(s0, InTexCoord0 + 4 * VarBlur.Offset) * 0.068;
  Col += tex2D(s0, InTexCoord0 + 5 * VarBlur.Offset) * 0.042;
  Col += tex2D(s0, InTexCoord0 + 6 * VarBlur.Offset) * 0.014;
	OutColor0 = Col;
}
void PS_Blur15x15 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 7 * VarBlur.Offset) * 0.01;
  Col += tex2D(s0, InTexCoord0 - 6 * VarBlur.Offset) * 0.032;
  Col += tex2D(s0, InTexCoord0 - 5 * VarBlur.Offset) * 0.052;
  Col += tex2D(s0, InTexCoord0 - 4 * VarBlur.Offset) * 0.069;
  Col += tex2D(s0, InTexCoord0 - 3 * VarBlur.Offset) * 0.084;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.095;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.102;
  Col += tex2D(s0, InTexCoord0) * 0.112;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.102;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.095;
  Col += tex2D(s0, InTexCoord0 + 3 * VarBlur.Offset) * 0.084;
  Col += tex2D(s0, InTexCoord0 + 4 * VarBlur.Offset) * 0.069;
  Col += tex2D(s0, InTexCoord0 + 5 * VarBlur.Offset) * 0.052;
  Col += tex2D(s0, InTexCoord0 + 6 * VarBlur.Offset) * 0.032;
  Col += tex2D(s0, InTexCoord0 + 7 * VarBlur.Offset) * 0.01;
	OutColor0 = Col;
}
void PS_Blur17x17 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 8 * VarBlur.Offset) * 0.008;
  Col += tex2D(s0, InTexCoord0 - 7 * VarBlur.Offset) * 0.025;
  Col += tex2D(s0, InTexCoord0 - 6 * VarBlur.Offset) * 0.041;
  Col += tex2D(s0, InTexCoord0 - 5 * VarBlur.Offset) * 0.055;
  Col += tex2D(s0, InTexCoord0 - 4 * VarBlur.Offset) * 0.068;
  Col += tex2D(s0, InTexCoord0 - 3 * VarBlur.Offset) * 0.078;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.086;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.09;
  Col += tex2D(s0, InTexCoord0) * 0.098;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.09;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.086;
  Col += tex2D(s0, InTexCoord0 + 3 * VarBlur.Offset) * 0.078;
  Col += tex2D(s0, InTexCoord0 + 4 * VarBlur.Offset) * 0.068;
  Col += tex2D(s0, InTexCoord0 + 5 * VarBlur.Offset) * 0.055;
  Col += tex2D(s0, InTexCoord0 + 6 * VarBlur.Offset) * 0.041;
  Col += tex2D(s0, InTexCoord0 + 7 * VarBlur.Offset) * 0.025;
  Col += tex2D(s0, InTexCoord0 + 8 * VarBlur.Offset) * 0.008;
	OutColor0 = Col;
}
void PS_Blur19x19 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 9 * VarBlur.Offset) * 0.006;
  Col += tex2D(s0, InTexCoord0 - 8 * VarBlur.Offset) * 0.02;
  Col += tex2D(s0, InTexCoord0 - 7 * VarBlur.Offset) * 0.033;
  Col += tex2D(s0, InTexCoord0 - 6 * VarBlur.Offset) * 0.045;
  Col += tex2D(s0, InTexCoord0 - 5 * VarBlur.Offset) * 0.055;
  Col += tex2D(s0, InTexCoord0 - 4 * VarBlur.Offset) * 0.065;
  Col += tex2D(s0, InTexCoord0 - 3 * VarBlur.Offset) * 0.072;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.078;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.081;
  Col += tex2D(s0, InTexCoord0) * 0.09;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.081;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.078;
  Col += tex2D(s0, InTexCoord0 + 3 * VarBlur.Offset) * 0.072;
  Col += tex2D(s0, InTexCoord0 + 4 * VarBlur.Offset) * 0.065;
  Col += tex2D(s0, InTexCoord0 + 5 * VarBlur.Offset) * 0.055;
  Col += tex2D(s0, InTexCoord0 + 6 * VarBlur.Offset) * 0.045;
  Col += tex2D(s0, InTexCoord0 + 7 * VarBlur.Offset) * 0.033;
  Col += tex2D(s0, InTexCoord0 + 8 * VarBlur.Offset) * 0.02;
  Col += tex2D(s0, InTexCoord0 + 9 * VarBlur.Offset) * 0.006;
	OutColor0 = Col;
}
void PS_Blur21x21 (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Col = 0;
  Col += tex2D(s0, InTexCoord0 - 10 * VarBlur.Offset) * 0.005;
  Col += tex2D(s0, InTexCoord0 - 9 * VarBlur.Offset) * 0.016;
  Col += tex2D(s0, InTexCoord0 - 8 * VarBlur.Offset) * 0.027;
  Col += tex2D(s0, InTexCoord0 - 7 * VarBlur.Offset) * 0.037;
  Col += tex2D(s0, InTexCoord0 - 6 * VarBlur.Offset) * 0.046;
  Col += tex2D(s0, InTexCoord0 - 5 * VarBlur.Offset) * 0.054;
  Col += tex2D(s0, InTexCoord0 - 4 * VarBlur.Offset) * 0.061;
  Col += tex2D(s0, InTexCoord0 - 3 * VarBlur.Offset) * 0.067;
  Col += tex2D(s0, InTexCoord0 - 2 * VarBlur.Offset) * 0.071;
  Col += tex2D(s0, InTexCoord0 - VarBlur.Offset) * 0.073;
  Col += tex2D(s0, InTexCoord0) * 0.086;
  Col += tex2D(s0, InTexCoord0 + VarBlur.Offset) * 0.073;
  Col += tex2D(s0, InTexCoord0 + 2 * VarBlur.Offset) * 0.071;
  Col += tex2D(s0, InTexCoord0 + 3 * VarBlur.Offset) * 0.067;
  Col += tex2D(s0, InTexCoord0 + 4 * VarBlur.Offset) * 0.061;
  Col += tex2D(s0, InTexCoord0 + 5 * VarBlur.Offset) * 0.054;
  Col += tex2D(s0, InTexCoord0 + 6 * VarBlur.Offset) * 0.046;
  Col += tex2D(s0, InTexCoord0 + 7 * VarBlur.Offset) * 0.037;
  Col += tex2D(s0, InTexCoord0 + 8 * VarBlur.Offset) * 0.027;
  Col += tex2D(s0, InTexCoord0 + 9 * VarBlur.Offset) * 0.016;
  Col += tex2D(s0, InTexCoord0 + 10 * VarBlur.Offset) * 0.005;
	OutColor0 = Col;
}
technique Blur {
  pass Blur3x3 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur3x3();
	}
	pass Blur5x5 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur5x5();
	}
  pass Blur7x7 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur7x7();
	}
  pass Blur9x9 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;        
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur9x9();
	}
  pass Blur11x11 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;        
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur11x11();
	}
  pass Blur13x13 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur13x13();
	}
  pass Blur15x15 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur15x15();
	}
  pass Blur17x17 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur17x17();
	}
  pass Blur19x19 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur19x19();
	}
  pass Blur21x21 {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Blur21x21();
	}
}
//Blur END

//Sharpen BEGIN
struct TVarSharpen {
  float2 PixelSize;
  float Scale;
};
TVarSharpen VarSharpen;
void PS_Sharpen (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float2 Offset = VarSharpen.PixelSize * 0.1;
  OutColor0 = tex2D(s0, InTexCoord0.xy);
  OutColor0 -= tex2D(s0, InTexCoord0.xy + Offset) * VarSharpen.Scale;
  OutColor0 += tex2D(s0, InTexCoord0.xy - Offset) * VarSharpen.Scale;
}
technique Sharpen {
	pass {
    MinFilter[0] = Linear;
    MagFilter[0] = Linear;
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Sharpen();
	}
}
//Sharpen END

//Monotone BEGIN
struct TVarMonotone {
  float Amount;
  float4 Mask;
};
TVarMonotone VarMonotone;
void PS_Monotone (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Color = tex2D(s0, InTexCoord0.xy);
  float Tone = Color.x * 0.3 + Color.y * 0.59 + Color.z * 0.11;
  float4 Monotone = float4(Tone, Tone, Tone, Color.w) * VarMonotone.Mask;
  OutColor0 = lerp(Color, Monotone, VarMonotone.Amount);
}
technique Monotone {
	pass {
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Monotone();
	}
}
//Monotone END

//Contrast BEGIN
struct TVarContrast {
  float Amount;
};
TVarContrast VarContrast;
void PS_Contrast (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Color = tex2D(s0, InTexCoord0.xy);
  OutColor0.xyz = pow(Color * 2, VarContrast.Amount) * 0.5;//pow(Color + 0.3, VarContrast.Amount) - 0.3;
  OutColor0.w = Color.w;
}
technique Contrast {
	pass {
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Contrast();
	}
}
//Contrast END

//Emboss BEGIN
struct TVarEmboss {
  float2 PixelSize;
  float Scale;
};
TVarEmboss VarEmboss;
void PS_Emboss (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float2 Offset = VarEmboss.PixelSize * 0.1;
  float4 Color = float4(0.5, 0.5, 0.5, 1);
  Color -= tex2D(s0, InTexCoord0.xy - Offset) * VarEmboss.Scale;
  Color += tex2D(s0, InTexCoord0.xy + Offset) * VarEmboss.Scale;
  OutColor0.rgb = (Color.r * 0.3 + Color.g * 0.59 + Color.b * 0.11);
  OutColor0.a = 1;
}
technique Emboss {
	pass {
    MinFilter[0] = Linear;
    MagFilter[0] = Linear;
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Emboss();
	}
}
//Emboss END

//Edge BEGIN
struct TVarEdge {
  float2 PixelSize;
  float Scale;
  float Power;
};
TVarEdge VarEdge;
void PS_Edge (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 c0 = tex2D(s0, InTexCoord0.xy);
  float4 c1 = tex2D(s0, InTexCoord0.xy + float2(VarEdge.PixelSize.x, 0));
  float4 c2 = tex2D(s0, InTexCoord0.xy + float2(0, VarEdge.PixelSize.y));
  float4 c3 = tex2D(s0, InTexCoord0.xy + VarEdge.PixelSize);
  float4 Dif1 = abs(c0 - c1);
  float4 Dif2 = abs(c0 - c2);
  float4 Dif3 = abs(c0 - c3);
  float Dif = (Dif1.r + Dif1.g + Dif1.b + Dif2.r + Dif2.g + Dif2.b + Dif3.r + Dif3.g + Dif3.b) * 0.11;
  OutColor0 = pow(float4(Dif, Dif, Dif, c0.w) * VarEdge.Scale, VarEdge.Power);
}
technique Edge {
	pass {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_Edge();
	}
}
//Emboss END

//ColorClamp BEGIN
struct TVarColorClamp {
  float ClampMin;
  float ClampMax;
};
TVarColorClamp VarColorClamp;
void PS_ColorClamp (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Color = tex2D(s0, InTexCoord0.xy);
  OutColor0 = smoothstep(VarColorClamp.ClampMin, VarColorClamp.ClampMax, Color);
}
technique ColorClamp {
	pass {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_ColorClamp();
	}
}
//ColorClamp END

//MonotoneClamp BEGIN
struct TVarMonotoneClamp {
  float ClampMin;
  float ClampMax;
};
TVarMonotoneClamp VarMonotoneClamp;
void PS_MonotoneClamp (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 Color = tex2D(s0, InTexCoord0.xy);
  float Tone = Color.x * 0.3 + Color.y * 0.59 + Color.z * 0.11;
  OutColor0.xyz = smoothstep(VarMonotoneClamp.ClampMin, VarMonotoneClamp.ClampMax, Tone);
  OutColor0.w = Color.w;
}
technique MonotoneClamp {
	pass {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_MonotoneClamp();
	}
}
//MonotoneClamp END

//DistortMap BEGIN
struct TVarDistortMap {
  float2 PixelSize;
  float2 DistortShift;
  float Amount;
};
TVarDistortMap VarDistortMap;
void PS_DistortMap (
    const in float2 InTexCoord0: TexCoord0, 
    const in float2 InTexCoord1: TexCoord1, 
    out float4 OutColor0: Color0
  ) {
  float4 nm = normalize(tex2D(s1, InTexCoord1.xy + VarDistortMap.DistortShift) * 2 - 1);
  float2 tc = InTexCoord0.xy + nm.xy * VarDistortMap.PixelSize * VarDistortMap.Amount;
  OutColor0 = tex2D(s0, tc);
}
technique DistortMap {
	pass {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
    AddressU[1] = Wrap;
    AddressV[1] = Wrap;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_DistortMap();
	}
}
//DistortMap END

//DistortMap2 BEGIN
struct TVarDistortMap2 {
  float2 PixelSize;
  float2 DistortShift0;
  float2 DistortShift1;
  float Amount;
};
TVarDistortMap2 VarDistortMap2;
void PS_DistortMap2 (
    const in float2 InTexCoord0: TexCoord0, 
    const in float2 InTexCoord1: TexCoord1, 
    const in float2 InTexCoord2: TexCoord2, 
    out float4 OutColor0: Color0
  ) {
  float4 nm0 = tex2D(s1, InTexCoord1.xy + VarDistortMap2.DistortShift0);
  float4 nm1 = tex2D(s2, InTexCoord2.xy + VarDistortMap2.DistortShift1);
  float4 nm = normalize(nm0 + nm1 - 1);
  float2 tc = InTexCoord0.xy + nm.xy * VarDistortMap2.PixelSize * VarDistortMap2.Amount;
  OutColor0 = tex2D(s0, tc);
}
technique DistortMap2 {
	pass {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
    AddressU[1] = Wrap;
    AddressV[1] = Wrap;
    AddressU[2] = Wrap;
    AddressV[2] = Wrap;
		VertexShader = NULL;
		PixelShader = compile ps_2_0 PS_DistortMap2();
	}
}
//DistortMap2 END

//Bloom BEGIN
struct TVarBloom {
  float2 PixelSize;
  float Power;
};
TVarBloom VarBloom;
void PS_Bloom (
    const in float2 InTexCoord0: TexCoord0, 
    out float4 OutColor0: Color0
  ) {
  float4 col = float4(0, 0, 0, 0);
  float4 c = tex2D(s0, InTexCoord0.xy);
  for (int i = -9; i <= 9; i++) {
    for (int j = -3; j <= 3; j++) {
      col += tex2D(s0, InTexCoord0.xy + float2(i, j) * VarBloom.PixelSize) * ((20 - abs(i)) * (20 - abs(j))) * 0.001;
    }
  }
  float br = c.x * 0.3 + c.y * 0.59 + c.z * 0.11;
  OutColor0.xyz = c + col * col * 0.0002 * VarBloom.Power; //lerp(0.0005, 0.00005, br)
  OutColor0.w = c.w;
}
technique Bloom {
	pass {
    AddressU[0] = Clamp;
    AddressV[0] = Clamp;
		VertexShader = NULL;
		PixelShader = compile ps_3_0 PS_Bloom();
	}
}
//Emboss END