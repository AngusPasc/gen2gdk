/*--------------------------------*\
		Gen2 Particles3D Shaders
\*--------------------------------*/

matrix g_WVP;

VertexShader Quad = asm {
  vs.2.0

  def c0, 2, 1, 6, 4
  //c[1]: Reserved
  //c[2..5]: WVPt (m4x4)
  //c[6..~]: Color4, Transform (m4x3)

  dcl_position0 v0
  dcl_texcoord0 v1

  mad r0.x, v0.w, c0.w, c0.z
  mova a0.x, r0.x

  mov r1.xyz, v0
  mov r1.w, c0.y

  dp4 r0.x, r1, c[a0.x + 1]
  dp4 r0.y, r1, c[a0.x + 2]
  dp4 r0.z, r1, c[a0.x + 3]
  mov r0.w, c0.y

  m4x4 oPos, r0, c2
  mov oT0, v1
  mov oD0, c[a0.x]
};

technique QuadArr {
	pass P0 {
    VertexShaderConstant[2] = g_WVP;
		VertexShader = Quad;
		PixelShader = NULL;
	}
}