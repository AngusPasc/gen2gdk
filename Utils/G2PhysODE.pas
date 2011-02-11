//G2PhysODE v1.0
unit G2PhysODE;

interface

uses
  Classes,
  DelphiODE,
  Gen2,
  G2Math;

type

  PG2ODECallBackParams = ^TG2ODECallBackParams;
  TG2ODECallBackParams = record
    World: PdxWorld;
    ContactJoints: TdJointGroupID;
  end;

//Convertion Functions BEGIN
function G2ODEConvVector3(const V: TdVector3): TG2Vec3; overload;
function G2ODEConvVector3(const V: TG2Vec3): TdVector3; overload;
function G2ODEConvVector4(const V: TdVector4): TG2Vec4; overload;
function G2ODEConvVector4(const V: TG2Vec4): TdVector4; overload;
function G2ODEConvQuaternion(const Q: TdQuaternion): TG2Quat; overload;
function G2ODEConvQuaternion(const Q: TG2Quat): TdQuaternion; overload;
function G2ODEConvMatrix3(const M: TdMatrix3): TG2Mat; overload;
function G2ODEConvMatrix3(const M: TG2Mat): TdMatrix3; overload;
function G2ODEConvMatrix4(const M: TdMatrix4): TG2Mat; overload;
function G2ODEConvMatrix4(const M: TG2Mat): TdMatrix4; overload;
//Convertion Functions END

//Misc Functions BEGIN
procedure G2ODENearCallback(Data: Pointer; o1, o2: PdxGeom); cdecl;
//Misc Functions END

implementation

//Convertion Functions BEGIN
function G2ODEConvVector3(const V: TdVector3): TG2Vec3;
begin
  Result.x := V[0];
  Result.y := V[1];
  Result.z := V[2];
end;

function G2ODEConvVector3(const V: TG2Vec3): TdVector3;
begin
  Result[0] := V.x;
  Result[1] := V.y;
  Result[2] := V.z;
end;

function G2ODEConvVector4(const V: TdVector4): TG2Vec4;
begin
  Result.x := V[0];
  Result.y := V[1];
  Result.z := V[2];
  Result.w := V[3];
end;

function G2ODEConvVector4(const V: TG2Vec4): TdVector4;
begin
  Result[0] := V.x;
  Result[1] := V.y;
  Result[2] := V.z;
  Result[3] := V.w;
end;

function G2ODEConvQuaternion(const Q: TdQuaternion): TG2Quat;
begin
  Result.x := Q[1];
  Result.y := Q[2];
  Result.z := Q[3];
  Result.w := Q[0];
end;

function G2ODEConvQuaternion(const Q: TG2Quat): TdQuaternion;
begin
  Result[0] := Q.w;
  Result[1] := Q.x;
  Result[2] := Q.y;
  Result[3] := Q.z;
end;

function G2ODEConvMatrix3(const M: TdMatrix3): TG2Mat;
begin
  Result.e00 := M[0];
  Result.e10 := M[1];
  Result.e20 := M[2];
  Result.e30 := M[3];
  Result.e01 := M[4];
  Result.e11 := M[5];
  Result.e21 := M[6];
  Result.e31 := M[7];
  Result.e02 := M[8];
  Result.e12 := M[9];
  Result.e22 := M[10];
  Result.e32 := M[11];
  Result.e03 := 0;
  Result.e13 := 0;
  Result.e23 := 0;
  Result.e33 := 1;
end;

function G2ODEConvMatrix3(const M: TG2Mat): TdMatrix3;
begin
  Result[0] := M.e00;
  Result[1] := M.e10;
  Result[2] := M.e20;
  Result[3] := M.e30;
  Result[4] := M.e01;
  Result[5] := M.e11;
  Result[6] := M.e21;
  Result[7] := M.e31;
  Result[8] := M.e02;
  Result[9] := M.e12;
  Result[10] := M.e22;
  Result[11] := M.e32;
end;

function G2ODEConvMatrix4(const M: TdMatrix4): TG2Mat;
begin
  Result.e00 := M[0];
  Result.e10 := M[1];
  Result.e20 := M[2];
  Result.e30 := M[3];
  Result.e01 := M[4];
  Result.e11 := M[5];
  Result.e21 := M[6];
  Result.e31 := M[7];
  Result.e02 := M[8];
  Result.e12 := M[9];
  Result.e22 := M[10];
  Result.e32 := M[11];
  Result.e03 := M[12];
  Result.e13 := M[13];
  Result.e23 := M[14];
  Result.e33 := M[15];
end;

function G2ODEConvMatrix4(const M: TG2Mat): TdMatrix4;
begin
  Result[0] := M.e00;
  Result[1] := M.e10;
  Result[2] := M.e20;
  Result[3] := M.e30;
  Result[4] := M.e01;
  Result[5] := M.e11;
  Result[6] := M.e21;
  Result[7] := M.e31;
  Result[8] := M.e02;
  Result[9] := M.e12;
  Result[10] := M.e22;
  Result[11] := M.e32;
  Result[12] := M.e03;
  Result[13] := M.e13;
  Result[14] := M.e23;
  Result[15] := M.e33;
end;
//Convertion Functions END

//Misc Functins BEGIN
procedure G2ODENearCallback(Data: Pointer; o1, o2: PdxGeom); cdecl;
const
  COL_MAX = 8;
var
  i : integer;
  b1, b2 : PdxBody;
  numc : integer;
  Contact : array[0..COL_MAX - 1] of TdContact;
  c : TdJointID;
begin
  if (dGeomIsSpace(o1) > 0) or (dGeomIsSpace(o2) > 0) then
  dSpaceCollide2(o1, o2, data, G2ODENearCallback)
  else
  begin
    b1 := dGeomGetBody(o1);
    b2 := dGeomGetBody(o2);
    if not (Assigned(b1) and Assigned(b2) and (dAreConnected(b1, b2) > 0)) then
    begin
      numc := dCollide(o1, o2, COL_MAX, Contact[0].geom, SizeOf(TdContact));
      if (numc > 0) then
      begin
        for i := 0 to numc - 1 do
        begin
          Contact[i].surface.mode := dContactBounce;
          Contact[i].surface.mu := 10e9;
          Contact[i].surface.mu2 := 0;
          Contact[i].surface.bounce := 0.5;
          Contact[i].surface.bounce_vel := 0.1;
        
          c := dJointCreateContact(
            PG2ODECallBackParams(data)^.World,
            PG2ODECallBackParams(data)^.ContactJoints,
            @Contact[i]
          );
          dJointAttach(c, b1, b2);
        end;
      end;
    end;
  end;
end;
//Misc Functions END

end.