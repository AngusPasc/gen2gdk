/*--------------------------------*\
		Gen2 Landscape Shaders
\*--------------------------------*/

static const int MAX_ANISOTROPY = 16;

matrix g_W;
matrix g_WV;
matrix g_WVP;
float3 g_LightDir;
float4 g_LightDiffuse;
float4 g_LightAmbient;
float4 g_LandProps;
float3 g_CamPos;
float2 g_TexRepeat;
float2 g_TexDims;
float g_SpecularPower;
float g_HeightMapScale;
int g_MipThreshold;
int g_MinSamples;
int g_MaxSamples;

texture TexNormals;
texture TexMask;
texture TexDiffuseSpecularMap;
texture TexNormalHeightMap;

sampler SampNormals = sampler_state {
    Texture = <TexNormals>;
    MinFilter = Linear;  
    MagFilter = Linear;
    MipFilter = Point;
    AddressU  = Clamp;
    AddressV  = Clamp;
};

sampler SampMask = sampler_state {
    Texture = <TexMask>;
    MinFilter = Linear;  
    MagFilter = Linear;
    MipFilter = Point;
    AddressU  = Clamp;
    AddressV  = Clamp;
};

sampler SampDiffuseSpecularMap = sampler_state {
    Texture = <TexDiffuseSpecularMap>;
    MaxAnisotropy = MAX_ANISOTROPY;
	MinFilter = Anisotropic;  
    MagFilter = Anisotropic;
    MipFilter = Anisotropic;
    AddressU  = Wrap;
    AddressV  = Wrap;
};

sampler SampNormalHeightMap = sampler_state {
    Texture = <TexNormalHeightMap>;
    MaxAnisotropy = MAX_ANISOTROPY;
	MinFilter = Anisotropic;  
    MagFilter = Anisotropic;
    MipFilter = Anisotropic;
    AddressU  = Wrap;
    AddressV  = Wrap;
};

struct TVSInput {
	float3 _Position0: Position0;
	float3 _Position1: TexCoord0;
};

struct TVSOutput {
	float4 _Position0: Position0;
	float4 _TexCoord0: TexCoord0;
	float3 _CamDir: TexCoord1;
};

struct TPSInput {
	float4 _TexCoord0: TexCoord0;
	float3 _CamDir: TexCoord1;
};

void VShader (const in TVSInput Input, out TVSOutput Output) {
	float3 dif = (g_CamPos - Input._Position0);
	float dist = dot(dif, dif);
	float Morph = pow(
		(min(max(dist, Input._Position1.y), Input._Position1.z) - Input._Position1.y) / (Input._Position1.z - Input._Position1.y),
		4
	);
	float3 Pos2 = float3(Input._Position0.x, Input._Position1.x, Input._Position0.z);
	float3 PosMS = lerp(Input._Position0, Pos2, Morph);
	Output._Position0 = mul(float4(PosMS, 1), g_WVP);
	Output._TexCoord0.xy = (Input._Position0.xz - g_LandProps.xy) * g_LandProps.zw;
	Output._TexCoord0.zw = Output._TexCoord0.xy * g_TexRepeat;
	Output._CamDir = PosMS - g_CamPos;
}

float4 PShaderPlain (const in TPSInput Input): Color0 {
	float3 CamDir = normalize(Input._CamDir);
	
	float3 Normal = normalize((float3)tex2D(SampNormals, Input._TexCoord0.xy) * 2 - 1);
	float4 DiffuseSpecularMap = tex2D(SampDiffuseSpecularMap, Input._TexCoord0.zw);
	
	float3 RefDir = CamDir - 2 * Normal * dot(CamDir, Normal);
	float DiffuseLight = saturate(dot(g_LightDir, Normal));
	float4 DiffuseCol = DiffuseSpecularMap * DiffuseLight * g_LightDiffuse;
	float4 SpecularCol = DiffuseSpecularMap.w * pow(saturate(dot(g_LightDir, RefDir)), g_SpecularPower);
	float4 Col = DiffuseCol + SpecularCol + g_LightAmbient;
	Col.w = tex2D(SampMask, Input._TexCoord0.xy).w;
	return Col;
}

float4 PShaderBump (const in TPSInput Input): Color0 {
	float3 CamDir = normalize(Input._CamDir);
	
	float3 Normal = (float3)tex2D(SampNormals, Input._TexCoord0.xy) * 2 - 1;
	float3 Binormal = normalize(cross(float3(1, 0, 0), Normal));
	float3 Tangent = normalize(cross(Normal, Binormal));
	float3x3 TBN = float3x3(Tangent, Binormal, Normal);
	
	float3 LightDir = normalize(mul(TBN, g_LightDir));
	CamDir = normalize(mul(TBN, CamDir));
	
	float3 NormalMap = (float3)tex2D(SampNormalHeightMap, Input._TexCoord0.zw) * 2 - 1;
	float4 DiffuseSpecularMap = tex2D(SampDiffuseSpecularMap, Input._TexCoord0.zw);
	
	float3 RefDir = CamDir - 2 * NormalMap * dot(CamDir, NormalMap);
	float DiffuseLight = saturate(dot(LightDir, NormalMap));
	float4 DiffuseCol = DiffuseSpecularMap * DiffuseLight * g_LightDiffuse;
	float4 SpecularCol = DiffuseSpecularMap.w * pow(saturate(dot(LightDir, RefDir)), g_SpecularPower);
	float4 Col = DiffuseCol + SpecularCol + g_LightAmbient;
	Col.w = tex2D(SampMask, Input._TexCoord0.xy).w;
	return Col;
}

float4 PShaderParallax (const in TPSInput Input): Color0 {
	float3 CamDir = normalize(Input._CamDir);
	
	float3 Normal = (float3)tex2D(SampNormals, Input._TexCoord0.xy) * 2 - 1;
	float3 Binormal = normalize(cross(float3(1, 0, 0), Normal));
	float3 Tangent = normalize(cross(Normal, Binormal));
	float3x3 TBN = float3x3(Tangent, Binormal, Normal);
	
	float3 LightDir = normalize(mul(TBN, g_LightDir));
	CamDir = normalize(mul(TBN, CamDir));
	
	float2 ParallaxDirection = normalize(CamDir.xy);
	
	float HeightMap = tex2D(SampNormalHeightMap, Input._TexCoord0.zw).w * g_HeightMapScale;
	float2 ParalaxTex0 = Input._TexCoord0.zw - HeightMap * ParallaxDirection;
	float3 NormalMap = (float3)tex2D(SampNormalHeightMap, ParalaxTex0) * 2 - 1;
	float4 DiffuseSpecularMap = tex2D(SampDiffuseSpecularMap, ParalaxTex0);
	
	float3 RefDir = CamDir - 2 * NormalMap * dot(CamDir, NormalMap);
	float DiffuseLight = saturate(dot(LightDir, NormalMap));
	float4 DiffuseCol = DiffuseSpecularMap * DiffuseLight * g_LightDiffuse;
	float4 SpecularCol = DiffuseSpecularMap.w * pow(saturate(dot(LightDir, RefDir)), g_SpecularPower);
	float4 Col = DiffuseCol + SpecularCol + g_LightAmbient;
	Col.w = tex2D(SampMask, Input._TexCoord0.xy).w;
	return Col;
}

float4 PShaderParallaxOcclusion (const in TPSInput Input): Color0 {
	float3 CamDirWS = normalize(-Input._CamDir);
	
	float3 Normal = (float3)tex2D(SampNormals, Input._TexCoord0.xy) * 2 - 1;
	float3 Binormal = normalize(cross(float3(1, 0, 0), Normal));
	float3 Tangent = normalize(cross(Normal, Binormal));
	float3x3 TBN = float3x3(Tangent, Binormal, Normal);
	
	float3 LightDirTS = normalize(mul(TBN, g_LightDir));
	float3 CamDirTS = normalize(mul(TBN, CamDirWS));
	
	float2 ParallaxDirection = normalize(CamDirTS.xy);
	float FarLength = length(ParallaxDirection);
    float ParallaxLength = sqrt(FarLength * FarLength - CamDirTS.z * CamDirTS.z) / CamDirTS.z;

	float2 ParallaxOffsetTS = ParallaxDirection * ParallaxLength * g_HeightMapScale;
	
	float2 TexCoordsPerSize = Input._TexCoord0.zw * g_TexDims;
	
	float2 dxSize, dySize;
	float2 dx, dy;

	float4(dxSize, dx) = ddx(float4(TexCoordsPerSize, Input._TexCoord0.zw));
	float4(dySize, dy) = ddy(float4(TexCoordsPerSize, Input._TexCoord0.zw));

	float  MipLevel;
	float  MipLevelInt;
	float  MipLevelFrac;

	float  MinTexCoordDelta;
	float2 TexCoords;
	
	TexCoords = dxSize * dxSize + dySize * dySize;
	
	MinTexCoordDelta = max(TexCoords.x, TexCoords.y);
	
	MipLevel = max(0.5 * log2(MinTexCoordDelta), 0);
	
	float2 TexSample = Input._TexCoord0.zw;
	
	if (MipLevel <= (float)g_MipThreshold) {

		int NumSteps = (int) lerp(g_MaxSamples, g_MinSamples, dot(CamDirWS, Normal));

		float CurrHeight = 0.0;
		float StepSize = 1.0 / (float) NumSteps;
		float PrevHeight = 1.0;
		float NextHeight = 0.0;

		int StepIndex = 0;
		bool Condition = true;

		float2 TexOffsetPerStep = StepSize * ParallaxOffsetTS;
		float2 TexCurrentOffset = Input._TexCoord0.zw;
		float CurrentBound = 1.0;
		float ParallaxAmount = 0.0;

		float2 pt1 = 0;
		float2 pt2 = 0;
       
		float2 TexOffset2 = 0;

		while (StepIndex < NumSteps) {
			TexCurrentOffset -= TexOffsetPerStep;
			
			CurrHeight = tex2Dgrad(SampNormalHeightMap, TexCurrentOffset, dx, dy).a;

			CurrentBound -= StepSize;

			if (CurrHeight > CurrentBound) {   
				pt1 = float2(CurrentBound, CurrHeight);
				pt2 = float2(CurrentBound + StepSize, PrevHeight);

				TexOffset2 = TexCurrentOffset - TexOffsetPerStep;

				StepIndex = NumSteps + 1;
				PrevHeight = CurrHeight;
			}
			else {
				StepIndex++;
				PrevHeight = CurrHeight;
			}
		}   

		float Delta2 = pt2.x - pt2.y;
		float Delta1 = pt1.x - pt1.y;

		float Denominator = Delta2 - Delta1;

		if (Denominator == 0.0f) {
			ParallaxAmount = 0.0f;
		}
		else {
			ParallaxAmount = (pt1.x * Delta2 - pt2.x * Delta1) / Denominator;
		}

		float2 ParallaxOffset = ParallaxOffsetTS * (1 - ParallaxAmount );

		float2 TexSampleBase = Input._TexCoord0.zw - ParallaxOffset;
		TexSample = TexSampleBase;

		if (MipLevel > (float)(g_MipThreshold - 1)) {
			MipLevelFrac = modf(MipLevel, MipLevelInt);

			TexSample = lerp(TexSampleBase, Input._TexCoord0.zw, MipLevelFrac);
		}    
	}
	
	float3 NormalTS = normalize(tex2D( SampNormalHeightMap, TexSample ) * 2 - 1);

	float4 BaseColor = tex2D(SampDiffuseSpecularMap, TexSample);
   
	float4 Diffuse = saturate(dot(NormalTS, LightDirTS));
   
	float3 ReflectionTS = normalize(2 * dot(CamDirTS, NormalTS) * NormalTS - CamDirTS);

	float RdotL = saturate(dot(ReflectionTS, LightDirTS));
	float4 Specular = saturate(pow(RdotL, g_SpecularPower)) * BaseColor.w;

	float4 FinalColor = (Diffuse * BaseColor * g_LightDiffuse + g_LightAmbient + Specular); 
	FinalColor.w = tex2D(SampMask, Input._TexCoord0.xy).w;

	return FinalColor;  
}

technique g2LandPlain {
	pass P0 {
		VertexShader = compile vs_2_0 VShader();
		PixelShader = compile ps_2_0 PShaderPlain();
	}
}

technique g2LandBump {
	pass P0 {
		VertexShader = compile vs_2_0 VShader();
		PixelShader = compile ps_2_0 PShaderBump();
	}
}

technique g2LandParallax {
	pass P0 {
		VertexShader = compile vs_2_0 VShader();
		PixelShader = compile ps_2_0 PShaderParallax();
	}
}

technique g2LandParallaxOcclusion {
	pass P0 {
		VertexShader = compile vs_3_0 VShader();
		PixelShader = compile ps_3_0 PShaderParallaxOcclusion();
	}
}
