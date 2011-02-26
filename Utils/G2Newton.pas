unit G2Newton;

interface

uses
  Classes,
  Types,
  Math,
  Windows,
  D3DX9,
  DXTypes,
  Direct3D9,
  G2MeshLoader,
  Gen2,
  G2Math,
  Newton;

type
//TG2RagdollSegment BEGIN
  TG2RagdollSegment = record
  public
    var NodeID: Integer;
    var NewtonBody: TNewtonBody;
    var TransformBind: TG2Mat;
    var TransformDef: TG2Mat;
    var Size: TG2Vec3;
    var Dependants: array of record
    public
      var NodeID: Integer;
      var Offset: TG2Mat;
    end;
    procedure SetTransform(const Transform: TG2Mat);
  end;
  PG2RagdollSegment = ^TG2RagdollSegment;
//TG2RagdollSegment END

//TG2RagdollGroup BEGIN
  TG2RagdollGroup = record
  public
    var NodeID: Integer;
    var Segments: array of PG2RagdollSegment;
    var BHead: PG2RagdollSegment;
    var BNeck: PG2RagdollSegment;
    var BPelvis: PG2RagdollSegment;
    var BBody: array of PG2RagdollSegment;
    var BArmR: array of PG2RagdollSegment;
    var BArmL: array of PG2RagdollSegment;
    var BLegR: array of PG2RagdollSegment;
    var BLegL: array of PG2RagdollSegment;
  end;
  PG2RagdollGroup = ^TG2RagdollGroup;
//TG2RagdollGroup END

//TG2MeshRagdoll BEGIN
  TG2MeshRagdoll = class
  strict private
    var m_World: TNewtonWorld;
    var m_Mesh: TG2Mesh;
    var m_MeshInst: TG2MeshInst;
    var m_ForceCallback: TNewtonApplyForceAndTorque;
    function CreateBoxBody(const MinV, MaxV: TG2Vec3; const Mass: Single = 1): TNewtonBody;
    function AddSegment(const RagdollGroup: PG2RagdollGroup; const RagdollObject: TG2RagdollObject; const Mass: Single = 1): PG2RagdollSegment;
    function MakePnP(const Pivot, Pin: TG2Vec3): TG2Mat;
    procedure ConnectSegmentsBall(const SegParent, SegChild: TG2RagdollSegment; const Pin: TG2Vec3; const ConeAngle, TwistMin, TwistMax: Single);
    procedure ConnectSegmentsHinge(const SegParent, SegChild: TG2RagdollSegment; const Pin: TG2Vec3; const MinAngle, MaxAngle: Single);
    procedure ConnectSegments6DOF(const SegParent, SegChild: TG2RagdollSegment; const Pin: TG2Vec3; const MinX, MinY, MinZ, MaxX, MaxY, MaxZ: Single);
  public
    var RagdollGroups: array of TG2RagdollGroup;
    constructor Create(const World: TNewtonWorld; const MeshInst: TG2MeshInst; const ForceCallback: TNewtonApplyForceAndTorque);
    destructor Destroy; override;
    procedure Render(const Prim3D: TG2Primitives3D);
    procedure Apply;
    procedure Reset;
  end;
//TG2MeshRagdoll END

implementation

//TG2RagdollSegment BEGIN
procedure TG2RagdollSegment.SetTransform(const Transform: TG2Mat);
  var m: TG2Mat;
begin
  m := TransformBind * Transform;
  NewtonBodySetMatrix(NewtonBody, @m);
end;
//TG2RagdollSegment END

//TG2MeshRagdoll BEGIN
function TG2MeshRagdoll.CreateBoxBody(const MinV, MaxV: TG2Vec3; const Mass: Single = 1): TNewtonBody;
  var Collision: TNewtonCollision;
  var Inertia: TG2Vec3;
  var m: TG2Mat;
  var Size: TG2Vec3;
  var Pos: TG2Vec3;
begin
  Size := MaxV - MinV;
  Pos := (MinV + MaxV) * 0.5;
  Collision := NewtonCreateBox(m_World, Size.x, Size.y, Size.z, SERIALIZE_ID_BOX,  nil);
  Result := NewtonCreateBody(m_World, Collision);
  NewtonReleaseCollision(m_World, Collision);
  Inertia.x := Mass * (Size.y * Size.y + Size.z * Size.z) / 12;
  Inertia.y := Mass * (Size.x * Size.x + Size.z * Size.z) / 12;
  Inertia.z := Mass * (Size.x * Size.x + Size.y * Size.y) / 12;
  NewtonBodySetMassMatrix(Result, Mass, Inertia.x, Inertia.y, Inertia.z);
  NewtonBodySetForceAndTorqueCallBack(Result, m_ForceCallBack);
  NewtonBodyGetMatrix(Result, @m);
  m[3, 0] := Pos.x;
  m[3, 1] := Pos.y;
  m[3, 2] := Pos.z;
  NewtonBodySetMatrix(Result, @m);
end;

function TG2MeshRagdoll.AddSegment(const RagdollGroup: PG2RagdollGroup; const RagdollObject: TG2RagdollObject; const Mass: Single = 1): PG2RagdollSegment;
  var i: Integer;
begin
  New(Result);
  Result^.NodeID := RagdollObject.NodeID;
  Result^.NewtonBody := CreateBoxBody(RagdollObject.MinV, RagdollObject.MaxV, Mass);
  NewtonBodyGetMatrix(Result^.NewtonBody, @Result^.TransformBind);
  Result^.TransformDef := RagdollObject.Transform;
  Result^.SetTransform(Result^.TransformDef);
  Result^.Size := RagdollObject.MaxV - RagdollObject.MinV;
  SetLength(Result.Dependants, RagdollObject.DependantCount);;
  for i := 0 to High(Result^.Dependants) do
  begin
    Result^.Dependants[i].NodeID := RagdollObject.Dependants[i].NodeID;
    Result^.Dependants[i].Offset := RagdollObject.Dependants[i].Offset;
  end;
  SetLength(RagdollGroup^.Segments, Length(RagdollGroup^.Segments) + 1);
  RagdollGroup^.Segments[High(RagdollGroup^.Segments)] := Result;
end;

function TG2MeshRagdoll.MakePnP(const Pivot, Pin: TG2Vec3): TG2Mat;
  var q: TG2Quat;
begin
  q := G2Math.G2Vec3Rotation(G2Vec3(1, 0, 0), Pin);
  Result.SetRotation(q);
  Result.Translate(Pivot);
end;

procedure TG2MeshRagdoll.ConnectSegmentsBall(const SegParent, SegChild: TG2RagdollSegment; const Pin: TG2Vec3; const ConeAngle, TwistMin, TwistMax: Single);
  var m, PnP: TG2Mat;
  var PinNorm: TG2Vec3;
  var j: TNewtonJoint;
begin
  PinNorm := Pin.Normalized;
  NewtonBodyGetMatrix(SegChild.NewtonBody, @m);
  m := SegChild.TransformBind.Inverse * m;
  PnP := MakePnP(m.GetTranslation, PinNorm);
  j := NewtonCreateCustomBallAndSocket(@PnP, SegChild.NewtonBody, SegParent.NewtonBody);
  NewtonBallAndSocketSetConeAngle(j, ConeAngle);
  NewtonBallAnsSocketSetTwistAngle(j, TwistMin, TwistMax);
end;

procedure TG2MeshRagdoll.ConnectSegmentsHinge(const SegParent, SegChild: TG2RagdollSegment; const Pin: TG2Vec3; const MinAngle, MaxAngle: Single);
  var m, PnP: TG2Mat;
  var PinNorm: TG2Vec3;
  var j: TNewtonJoint;
begin
  PinNorm := Pin.Normalized;
  NewtonBodyGetMatrix(SegChild.NewtonBody, @m);
  m := SegChild.TransformBind.Inverse * m;
  PnP := MakePnP(m.GetTranslation, PinNorm);
  j := NewtonCreateCustomHinge(@PnP, SegChild.NewtonBody, SegParent.NewtonBody);
  NewtonCustomHingeEnableLimits(j, 1);
  NewtonCustomHingeSetLimits(j, MinAngle, MaxAngle);
end;

procedure TG2MeshRagdoll.ConnectSegments6DOF(const SegParent, SegChild: TG2RagdollSegment; const Pin: TG2Vec3; const MinX, MinY, MinZ, MaxX, MaxY, MaxZ: Single);
  var m, PnP: TG2Mat;
  var PinNorm: TG2Vec3;
  var j: TNewtonJoint;
  var v4a, v4b: TG2Vec4;
begin
  PinNorm := Pin.Normalized;
  NewtonBodyGetMatrix(SegChild.NewtonBody, @m);
  m := SegChild.TransformBind.Inverse * m;
  PnP := MakePnP(m.GetTranslation, PinNorm);
  j := NewtonCreateCustomJoint6DOF(@PnP, @PnP, SegChild.NewtonBody, SegParent.NewtonBody);
  v4a.SetValue(MinX, MinY, MinZ, 0);
  v4b.SetValue(MaxX, MaxY, MaxZ, 0);
  NewtonCustom6DOF_SetAngularLimits(j, @v4a, @v4b);
end;

constructor TG2MeshRagdoll.Create(const World: TNewtonWorld; const MeshInst: TG2MeshInst; const ForceCallback: TNewtonApplyForceAndTorque);
  var i, j: Integer;
  var v: TG2Vec3;
  var m, m1: TG2Mat;
  var q: TG2Quat;
  var ArmOffsets: array of TG2Mat;
  var a: Single;
begin
  inherited Create;
  m_World := World;
  m_Mesh := MeshInst.Mesh;
  m_MeshInst := MeshInst;
  m_ForceCallback := ForceCallback;
  SetLength(RagdollGroups, m_Mesh.RagdollCount);
  for i := 0 to High(RagdollGroups) do
  with RagdollGroups[i] do
  begin
    NodeID := m_Mesh.Ragdolls[i].NodeID;
    SetLength(BBody, m_Mesh.Ragdolls[i].BodyNodeCount);
    SetLength(BArmR, m_Mesh.Ragdolls[i].ArmRNodeCount);
    SetLength(BArmL, m_Mesh.Ragdolls[i].ArmLNodeCount);
    SetLength(BLegR, m_Mesh.Ragdolls[i].LegRNodeCount);
    SetLength(BLegL, m_Mesh.Ragdolls[i].LegLNodeCount);
    BHead := AddSegment(@RagdollGroups[i], m_Mesh.Ragdolls[i].Head, 2);
    BNeck := AddSegment(@RagdollGroups[i], m_Mesh.Ragdolls[i].Neck, 1);
    BPelvis := AddSegment(@RagdollGroups[i], m_Mesh.Ragdolls[i].Pelvis, 5);
    for j := 0 to High(BBody) do
    BBody[j] := AddSegment(@RagdollGroups[i], m_Mesh.Ragdolls[i].BodyNodes[j], 3);
    for j := 0 to High(BArmR) do
    BArmR[j] := AddSegment(@RagdollGroups[i], m_Mesh.Ragdolls[i].ArmRNodes[j], 2);
    for j := 0 to High(BArmL) do
    BArmL[j] := AddSegment(@RagdollGroups[i], m_Mesh.Ragdolls[i].ArmLNodes[j], 2);
    for j := 0 to High(BLegR) do
    BLegR[j] := AddSegment(@RagdollGroups[i], m_Mesh.Ragdolls[i].LegRNodes[j], 3);
    for j := 0 to High(BLegL) do
    BLegL[j] := AddSegment(@RagdollGroups[i], m_Mesh.Ragdolls[i].LegLNodes[j], 3);

    SetLength(ArmOffsets, Length(BArmR) - 1);
    for j := 0 to High(ArmOffsets) do
    ArmOffsets[j] := BArmR[j + 1]^.TransformDef * BArmR[0]^.TransformDef.Inverse;
    NewtonBodyGetMatrix(BArmR[0]^.NewtonBody, @m);
    v.SetValue(1, 0, 0); D3DXVec3TransformNormal(PD3DXVector3(@v)^, v, m); v.Normalize;
    q := G2Vec3Rotation(v, G2Vec3(-1, 0, 0)); m.SetRotation(q);
    m := m * BArmR[0]^.TransformDef;
    m1 := BArmR[0]^.TransformBind * m;
    NewtonBodySetMatrix(BArmR[0]^.NewtonBody, @m1);
    for j := 1 to High(BArmR) do
    BArmR[j]^.SetTransform(ArmOffsets[j - 1] * m);

    SetLength(ArmOffsets, Length(BArmL) - 1);
    for j := 0 to High(ArmOffsets) do
    ArmOffsets[j] := BArmL[j + 1]^.TransformDef * BArmL[0]^.TransformDef.Inverse;
    NewtonBodyGetMatrix(BArmL[0]^.NewtonBody, @m);
    v.SetValue(1, 0, 0); D3DXVec3TransformNormal(PD3DXVector3(@v)^, v, m); v.Normalize;
    q := G2Vec3Rotation(v, G2Vec3(1, 0, 0)); m.SetRotation(q);
    m := m * BArmL[0]^.TransformDef;
    m1 := BArmL[0]^.TransformBind * m;
    NewtonBodySetMatrix(BArmL[0]^.NewtonBody, @m1);
    for j := 1 to High(BArmL) do
    BArmL[j]^.SetTransform(ArmOffsets[j - 1] * m);

    //ConnectSegmentsBall(BNeck^, BHead^, G2Vec3(0, 1, 0), Pi * 0.1, -Pi * 0.1, Pi * 0.1);
    ConnectSegments6DOF(BNeck^, BHead^, G2Vec3(1, 0, 0), -QuatPi, -QuatPi, -Pi * 0.1, Pi * 0.1, QuatPi, Pi * 0.1);
    ConnectSegmentsBall(BBody[High(BBody)]^, BNeck^, G2Vec3(0, 1, 0), Pi * 0.001, -Pi * 0.001, Pi * 0.001);
    ConnectSegmentsBall(BBody[0]^, BPelvis^, G2Vec3(0, -1, 0), Pi * 0.01, -Pi * 0.01, Pi * 0.01);
    for j := 0 to High(BBody) - 1 do
    ConnectSegmentsBall(BBody[j]^, BBody[j + 1]^, G2Vec3(0, 1, 0), Pi * 0.02, -Pi * 0.05, Pi * 0.05);
    ConnectSegments6DOF(BPelvis^, BLegR[0]^, G2Vec3(1, 0, 0), -HalfPi, 0, -Pi * 0.1, Pi * 0.1, 0, QuatPi);
    ConnectSegments6DOF(BPelvis^, BLegL[0]^, G2Vec3(1, 0, 0), -HalfPi, 0, -QuatPi, Pi * 0.1, 0, Pi * 0.1);
    ConnectSegmentsHinge(BLegR[0]^, BLegR[1]^, G2Vec3(1, 0, 0), 0, QuatPi * 3);
    ConnectSegmentsHinge(BLegL[0]^, BLegL[1]^, G2Vec3(1, 0, 0), 0, QuatPi * 3);
    ConnectSegmentsHinge(BLegR[1]^, BLegR[2]^, G2Vec3(1, 0, 0), -Pi * 0.1, Pi * 0.2);
    ConnectSegmentsHinge(BLegL[1]^, BLegL[2]^, G2Vec3(1, 0, 0), -Pi * 0.1, Pi * 0.2);
    //ConnectSegments6DOF(BBody[High(BBody)]^, BArmR[0]^, G2Vec3(1, 0, 0), -HalfPi, -Pi * 0.02, -Pi * 0.4, QuatPi, Pi * 0.02, Pi * 0.4);
    //ConnectSegments6DOF(BBody[High(BBody)]^, BArmL[0]^, G2Vec3(1, 0, 0), -HalfPi, -Pi * 0.02, -Pi * 0.4, QuatPi, Pi * 0.02, Pi * 0.4);
    ConnectSegmentsBall(BBody[High(BBody)]^, BArmR[0]^, G2Vec3(-1, 0, 0), QuatPi * 1.3, -Pi * 0.3, 0);
    ConnectSegmentsBall(BBody[High(BBody)]^, BArmL[0]^, G2Vec3(1, 0, 0), QuatPi * 1.3, -Pi * 0.3, 0);
    ConnectSegmentsHinge(BArmR[0]^, BArmR[1]^, G2Vec3(0, 1, 0), 0, QuatPi * 3);
    ConnectSegmentsHinge(BArmL[0]^, BArmL[1]^, G2Vec3(0, 1, 0), -QuatPi * 3, 0);
    ConnectSegmentsHinge(BArmR[1]^, BArmR[2]^, G2Vec3(0, 0, 1), -Pi * 0.1, Pi * 0.1);
    ConnectSegmentsHinge(BArmL[1]^, BArmL[2]^, G2Vec3(0, 0, 1), -Pi * 0.1, Pi * 0.1);

    for j := 0 to High(Segments) do
    Segments[j].SetTransform(Segments[j].TransformDef);
  end;
end;

destructor TG2MeshRagdoll.Destroy;
  var i, j: Integer;
begin
  for i := 0 to High(RagdollGroups) do
  for j := 0 to High(RagdollGroups[i].Segments) do
  begin
    NewtonDestroyBody(m_World, RagdollGroups[i].Segments[j].NewtonBody);
    Dispose(RagdollGroups[i].Segments[j]);
  end;
  inherited Destroy;
end;

procedure TG2MeshRagdoll.Render(const Prim3D: TG2Primitives3D);
  procedure RenderSegment(const s: TG2RagdollSegment);
    var m, mw: TG2Mat;
  begin
    NewtonBodyGetMatrix(s.NewtonBody, @m);
    mw.SetScaling(s.Size);
    m := mw * m;
    Prim3D.Core.Graphics.Transforms.W[0] := m;
    Prim3D.Core.Graphics.Transforms.ApplyW(0);
    Prim3D.DrawBox(-0.5, -0.5, -0.5, 1, 1, 1, $ffffffff);
  end;
  var i, j: Integer;
begin
  Prim3D.Core.Graphics.Transforms.PushW;
  for i := 0 to High(RagdollGroups) do
  for j := 0 to High(RagdollGroups[i].Segments) do
  RenderSegment(RagdollGroups[i].Segments[j]^);
  Prim3D.Core.Graphics.Transforms.PopW;
  Prim3D.Core.Graphics.Transforms.ApplyW(0);
end;

procedure TG2MeshRagdoll.Apply;
  var i, j, d: Integer;
  var m, md: TG2Mat;
begin
  for i := 0 to High(RagdollGroups) do
  for j := 0 to High(RagdollGroups[i].Segments) do
  begin
    NewtonBodyGetMatrix(RagdollGroups[i].Segments[j].NewtonBody, @m);
    m := RagdollGroups[i].Segments[j].TransformBind.Inverse * m;
    m_MeshInst.NodeTransforms[RagdollGroups[i].Segments[j].NodeID].TransformRen := m;
    for d := 0 to High(RagdollGroups[i].Segments[j].Dependants) do
    begin
      md := RagdollGroups[i].Segments[j].Dependants[d].Offset * m;
      m_MeshInst.NodeTransforms[RagdollGroups[i].Segments[j].Dependants[d].NodeID].TransformRen := md;
    end;
  end;
  m_MeshInst.ComputeSkinTransforms;
end;

procedure TG2MeshRagdoll.Reset;
  var i, j: Integer;
begin
  for i := 0 to High(RagdollGroups) do
  for j := 0 to High(RagdollGroups[i].Segments) do
  begin
    RagdollGroups[i].Segments[j].SetTransform(RagdollGroups[i].Segments[j].TransformDef);
  end;
end;
//TG2MeshRagdoll END

end.