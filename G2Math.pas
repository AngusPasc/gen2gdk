//G2Math v1.0
//Author: Dan
unit G2Math;

{$include Gen2.inc}

interface

uses
  Types,
  SysUtils,
  Math,
  DXTypes,
  Direct3D9,
  D3DX9;

type
  UInt8 = Byte;
  Int8 = ShortInt;
  UInt16 = Word;
  Int16 = SmallInt;
  UInt32 = DWord;
  Int32 = Integer;
  
  PG2MatRef = ^TG2MatRef;
  TG2MatRef = TD3DXMatrix;

  PG2MatArr = ^TG2MatArr;
  TG2MatArr = array[0..3, 0..3] of Single;

  PG2Mat2Ref = ^TG2Mat2Ref;
  TG2Mat2Ref = array[0..1, 0..1] of Single;

  PG2Vec2Ref = ^TG2Vec2Ref;
  TG2Vec2Ref = TD3DXVector2;

  PG2Vec3Ref = ^TG2Vec3Ref;
  TG2Vec3Ref = TD3DXVector3;

  PG2Vec4Ref = ^TG2Vec4Ref;
  TG2Vec4Ref = TD3DXVector4;

  PG2QuatRef = ^TG2QuatRef;
  TG2QuatRef = TD3DXQuaternion;

  PG2PlaneRef = ^TG2PlaneRef;
  TG2PlaneRef = TD3DXPlane;

  PG2Mat = ^TG2Mat;
  TG2Mat = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMat(const ix, iy: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMat(const ix, iy: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var e00, e01, e02, e03: Single;
    var e10, e11, e12, e13: Single;
    var e20, e21, e22, e23: Single;
    var e30, e31, e32, e33: Single;
    property Arr[const Index: Integer]: Single read GetArr write SetArr;
    property Mat[const ix, iy: Integer]: Single read GetMat write SetMat; default;
    class operator Explicit(const m: TG2Mat): TG2MatRef;
    class operator Explicit(const m: TG2MatRef): TG2Mat;
    class operator Implicit(const m: TG2Mat): TG2MatRef;
    class operator Implicit(const m: TG2MatRef): TG2Mat;
    class operator Equal(const m1, m2: TG2Mat): Boolean;
    class operator NotEqual(const m1, m2: TG2Mat): Boolean;
    class operator Add(const m1, m2: TG2Mat): TG2Mat;
    class operator Subtract(const m1, m2: TG2Mat): TG2Mat;
    class operator Multiply(const m1, m2: TG2Mat): TG2Mat;
    procedure SetValue(
      const m00, m10, m20, m30: Single;
      const m01, m11, m21, m31: Single;
      const m02, m12, m22, m32: Single;
      const m03, m13, m23, m33: Single
    ); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetValue(const AxisX, AxisY, AxisZ, Translation: TG2Vec3Ref); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetIdentity; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetScaling(const _X, _Y, _Z: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetScaling(const _v: TG2Vec3Ref); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetScaling(const _s: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetTranslation(const _X, _Y, _Z: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetTranslation(const _v: TG2Vec3Ref); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotationX(const _Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotationY(const _Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotationZ(const _Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotation(const _X, _Y, _Z, _Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotation(const _v: TG2Vec3Ref; const _Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotation(const _q: TG2QuatRef); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Scale(const X, Y, Z: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Scale(const v: TG2Vec3Ref); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Translate(const X, Y, Z: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Translate(const v: TG2Vec3Ref); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RotateX(const Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RotateY(const Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RotateZ(const Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Rotate(const X, Y, Z, Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Rotate(const v: TG2Vec3Ref; const Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Rotate(const q: TG2QuatRef); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreScale(const X, Y, Z: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreScale(const v: TG2Vec3Ref); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreTranslate(const X, Y, Z: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreTranslate(const v: TG2Vec3Ref); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreRotateX(const Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreRotateY(const Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreRotateZ(const Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreRotate(const X, Y, Z, Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreRotate(const v: TG2Vec3Ref; const Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreRotate(const q: TG2QuatRef); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetView(const Pos, Target, Up: TG2Vec3Ref); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetOrthogonal(const Width, Height, ZNear, ZFar: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPerspective(const FOV, Aspect, ZNear, ZFar: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Decompose(const Scaling: PG2Vec3Ref; const Rotation: PG2QuatRef; const Translation: PG2Vec3Ref); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTranslation: TG2Vec3Ref;
    function GetRotation: TG2QuatRef;
    function GetScaling: TG2Vec3Ref;
    function Determinant: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Transpose: TG2Mat;
    function Inverse: TG2Mat;
    function Mat4x3: TG2Mat;
    function Mat3x3: TG2Mat;
  end;

  PG2Mat2 = ^TG2Mat2;
  TG2Mat2 = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var e00, e01: Single;
    var e10, e11: Single;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Explicit(const m: TG2Mat2): TG2Mat2Ref;
    class operator Explicit(const m: TG2Mat2Ref): TG2Mat2;
    class operator Implicit(const m: TG2Mat2): TG2Mat2Ref;
    class operator Implicit(const m: TG2Mat2Ref): TG2Mat2;
    class operator Equal(const m1, m2: TG2Mat2): Boolean;
    class operator NotEqual(const m1, m2: TG2Mat2): Boolean;
    class operator Add(const m1, m2: TG2Mat2): TG2Mat2;
    class operator Subtract(const m1, m2: TG2Mat2): TG2Mat2;
    class operator Multiply(const m1, m2: TG2Mat2): TG2Mat2;
    class operator Multiply(const m: TG2Mat2; s: Single): TG2Mat2;
    procedure SetIdentity; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetScaling(const _X, _Y: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotation(const _Angle: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Scale(const X, Y: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Rotate(const Angle: Single);
    function Determinant: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Transpose: TG2Mat2;
    function Inverse: TG2Mat2;
    function GetRotation: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

  PG2Vec2 = ^TG2Vec2;
  TG2Vec2 = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var x, y: Single;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Negative(const v: TG2Vec2): TG2Vec2;
    class operator Explicit(const v: TG2Vec2): TG2Vec2Ref;
    class operator Explicit(const v: TG2Vec2Ref): TG2Vec2;
    class operator Explicit(const v: TPoint): TG2Vec2;
    class operator Implicit(const v: TG2Vec2): TG2Vec2Ref;
    class operator Implicit(const v: TG2Vec2Ref): TG2Vec2;
    class operator Implicit(const v: TPoint): TG2Vec2;
    class operator Equal(const v1, v2: TG2Vec2): Boolean;
    class operator NotEqual(const v1, v2: TG2Vec2): Boolean;
    class operator Add(const v1, v2: TG2Vec2): TG2Vec2;
    class operator Add(const v: TG2Vec2; const s: Single): TG2Vec2;
    class operator Subtract(const v1, v2: TG2Vec2): TG2Vec2;
    class operator Subtract(const v: TG2Vec2; const s: Single): TG2Vec2;
    class operator Multiply(const v1, v2: TG2Vec2): Single;
    class operator Multiply(const v: TG2Vec2; const s: Single): TG2Vec2;
    class operator Multiply(const v: TG2Vec2; const m: TG2Mat): TG2Vec2;
    class operator Multiply(const v: TG2Vec2; const m: TG2Mat2): TG2Vec2;
    class operator Divide(const v: TG2Vec2; const s: Single): TG2Vec2;
    procedure SetValue(const _X, _Y: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Normalize; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Normalized: TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Len: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function LenSq: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Dot(const v: TG2Vec2): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Cross(const v: TG2Vec2): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Angle(const v: TG2Vec2): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Perp: TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Reflect(const n: TG2Vec2): TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Rotate(const Angle: Single): TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV2(const Shuffle: Word): TG2Vec2Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV3(const Shuffle: Word): TG2Vec3Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV4(const Shuffle: Word): TG2Vec4Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

  PG2Vec3 = ^TG2Vec3;
  TG2Vec3 = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var x, y, z: Single;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Negative(const v: TG2Vec3): TG2Vec3; 
    class operator Explicit(const v: TG2Vec3): TG2Vec3Ref; 
    class operator Explicit(const v: TG2Vec3Ref): TG2Vec3; 
    class operator Implicit(const v: TG2Vec3): TG2Vec3Ref; 
    class operator Implicit(const v: TG2Vec3Ref): TG2Vec3; 
    class operator Equal(const v1, v2: TG2Vec3): Boolean; 
    class operator NotEqual(const v1, v2: TG2Vec3): Boolean; 
    class operator Add(const v1, v2: TG2Vec3): TG2Vec3;
    class operator Add(const v: TG2Vec3; const s: Single): TG2Vec3; 
    class operator Subtract(const v1, v2: TG2Vec3): TG2Vec3;
    class operator Subtract(const v: TG2Vec3; const s: Single): TG2Vec3; 
    class operator Multiply(const v1, v2: TG2Vec3): Single; 
    class operator Multiply(const v: TG2Vec3; const s: Single): TG2Vec3; 
    class operator Multiply(const v: TG2Vec3; const m: TG2Mat): TG2Vec3; 
    class operator Divide(const v: TG2Vec3; const s: Single): TG2Vec3;
    procedure SetValue(const _X, _Y, _Z: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetValue(const v2: TG2Vec2; const _Z: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Normalize; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Normalized: TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Transform3x3(const m: TG2Mat): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Transform4x3(const m: TG2Mat): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Transform4x4(const m: TG2Mat): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Len: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function LenSq: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Dot(const v: TG2Vec3): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Cross(const v: TG2Vec3): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Angle(const v: TG2Vec3): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function InTriangle(const v0, v1, v2: TG2Vec3): Boolean;
    function AsVec2: TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV2(const Shuffle: Word): TG2Vec2Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV3(const Shuffle: Word): TG2Vec3Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV4(const Shuffle: Word): TG2Vec4Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

  PG2Vec4 = ^TG2Vec4;
  TG2Vec4 = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var x, y, z, w: Single;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Negative(const v: TG2Vec4): TG2Vec4;
    class operator Explicit(const v: TG2Vec4): TG2Vec4Ref; 
    class operator Explicit(const v: TG2Vec4Ref): TG2Vec4;
    class operator Explicit(const v: TG2Vec3): TG2Vec4;
    class operator Explicit(const v: TG2Vec2): TG2Vec4;
    class operator Explicit(const s: Single): TG2Vec4;
    class operator Implicit(const v: TG2Vec4): TG2Vec4Ref; 
    class operator Implicit(const v: TG2Vec4Ref): TG2Vec4;
    class operator Implicit(const v: TG2Vec3): TG2Vec4;
    class operator Implicit(const v: TG2Vec2): TG2Vec4;
    class operator Implicit(const s: Single): TG2Vec4;
    class operator Equal(const v1, v2: TG2Vec4): Boolean; 
    class operator NotEqual(const v1, v2: TG2Vec4): Boolean; 
    class operator Add(const v1, v2: TG2Vec4): TG2Vec4;
    class operator Add(const v: TG2Vec4; const s: Single): TG2Vec4; 
    class operator Subtract(const v1, v2: TG2Vec4): TG2Vec4;
    class operator Subtract(const v: TG2Vec4; const s: Single): TG2Vec4; 
    class operator Multiply(const v1, v2: TG2Vec4): Single; 
    class operator Multiply(const v: TG2Vec4; const s: Single): TG2Vec4;
    class operator Multiply(const v: TG2Vec4; const m: TG2Mat): TG2Vec4; 
    class operator Divide(const v: TG2Vec4; const s: Single): TG2Vec4;
    procedure SetValue(const _X, _Y, _Z, _W: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetValue(const v3: TG2Vec3; _W: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetValue(const v2: TG2Vec2; _Z, _W: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Normalize; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Normalized: TG2Vec4; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Transform(const m: TG2Mat): TG2Vec4; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Len: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function LenSq: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Dot(const v: TG2Vec4): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Cross(const v1, v2: TG2Vec4): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Angle(const v: TG2Vec4): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function AsVec3: TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function AsVec2: TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV2(const Shuffle: Word): TG2Vec2Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV3(const Shuffle: Word): TG2Vec3Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SwizzleV4(const Shuffle: Word): TG2Vec4Ref; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

  PG2Quat = ^TG2Quat;
  TG2Quat = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var x, y, z, w: Single;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Explicit(const q: TG2Quat): TG2QuatRef;
    class operator Explicit(const q: TG2QuatRef): TG2Quat;
    class operator Implicit(const q: TG2Quat): TG2QuatRef;
    class operator Implicit(const q: TG2QuatRef): TG2Quat;
    class operator Equal(const q1, q2: TG2Quat): Boolean;
    class operator NotEqual(const q1, q2: TG2Quat): Boolean;
    class operator Add(const q1, q2: TG2Quat): TG2Quat;
    class operator Add(const q: TG2Quat; const s: Single): TG2Quat;
    class operator Subtract(const q1, q2: TG2Quat): TG2Quat;
    class operator Subtract(const q: TG2Quat; const s: Single): TG2Quat;
    class operator Multiply(const q: TG2Quat; const s: Single): TG2Quat;
    class operator Multiply(const q1, q2: TG2Quat): TG2Quat;
    procedure SetValue(const _X, _Y, _Z, _W: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF} 
    procedure SetIdentity; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotation(const Axis: TG2Vec3; const Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRotation(const m: TG2Mat); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Rotate(const Axis: TG2Vec3; const Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Rotate(const m: TG2Mat); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Normalize; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure GetRotation(var Axis: TG2Vec3; var Angle: Single); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure GetRotation(var m: TG2Mat); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Dot(const q: TG2Quat): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Conjugate: TG2Quat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Inverse: TG2Quat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF} 
  end;

  PG2AABox = ^TG2AABox;
  TG2AABox = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var MinV, MaxV: TG2Vec3;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Equal(const b1, b2: TG2AABox): Boolean;
    class operator NotEqual(const b1, b2: TG2AABox): Boolean;
    class operator Add(const b1, b2: TG2AABox): TG2AABox;
    class operator Add(const b: TG2AABox; const v: TG2Vec3): TG2AABox;
    procedure SetValue(const _MinV, _MaxV: TG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Project(const W, V, P: TG2Mat; const Viewport: TD3DViewport9): TRect;
    function Intersect(const AABox: TG2AABox): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function PointInside(const v: TG2Vec3): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

  PG2Box = ^TG2Box;
  TG2Box = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var C, vx, vy, vz: TG2Vec3;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Equal(const b1, b2: TG2Box): Boolean;
    class operator NotEqual(const b1, b2: TG2Box): Boolean;
    class operator Multiply(const b: TG2Box; const m: TG2Mat): TG2Box;
    class operator Implicit(const b: TG2AABox): TG2Box;
    procedure SetValue(const _C, _vx, _vy, _vz: TG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure GetPoints(const v: PG2Vec3);
    function AABox: TG2AABox;
  end;

  PG2Sphere = ^TG2Sphere;
  TG2Sphere = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var C: TG2Vec3;
    var R: Single;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Equal(const s1, s2: TG2Sphere): Boolean;
    class operator NotEqual(const s1, s2: TG2Sphere): Boolean;
    procedure SetValue(const _C: TG2Vec3; const _R: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

  PG2Plane = ^TG2Plane;
  TG2Plane = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetA: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetA(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetB: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetB(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetC: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetC(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var N: TG2Vec3;
    var D: Single;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    property A: Single read GetA write SetA;
    property B: Single read GetB write SetB;
    property C: Single read GetC write SetC;
    class operator Explicit(const p: TG2Plane): TG2PlaneRef;
    class operator Explicit(const p: TG2PlaneRef): TG2Plane;
    class operator Implicit(const p: TG2Plane): TG2PlaneRef;
    class operator Implicit(const p: TG2PlaneRef): TG2Plane;
    class operator Equal(const p1, p2: TG2Plane): Boolean;
    class operator NotEqual(const p1, p2: TG2Plane): Boolean;
    class operator Multiply(const p: TG2Plane; const m: TG2Mat): TG2Plane; 
    procedure SetValue(const _A, _B, _C, _D: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPlane(const _V, _N: TG2Vec3); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPlane(const _v1, _v2, _v3: TG2Vec3); overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Normalize; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DistanceToPoint(const v: TG2Vec3): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DistanceToBox(const b: TG2Box): Single;
    function Project(const v: TG2Vec3): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Reflect(const v: TG2Vec3): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectTri(const v1, v2, v3: TG2Vec3): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectAABox(const b: TG2AABox): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectSphere(const s: TG2Sphere): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

  PG2Ray = ^TG2Ray;
  TG2Ray = record
  strict private
    function GetArr(const Index: Integer): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetArr(const Index: Integer; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    var Origin: TG2Vec3;
    var Dir: TG2Vec3;
    property Arr[const Index: Integer]: Single read GetArr write SetArr; default;
    class operator Equal(const p1, p2: TG2Ray): Boolean;
    class operator NotEqual(const p1, p2: TG2Ray): Boolean;
    class operator Multiply(const r: TG2Ray; m: TG2Mat): TG2Ray;
    procedure SetValue(const _Origin, _Dir: TG2Vec3);
    procedure Normalize;
    procedure Transform(const m: TG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure TransformInverse(const m: TG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function PointAtDistance(const Distance: Single): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectTri(const v1, v2, v3: TG2Vec3; var U, V, Distance: Single): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectSphere(const s: TG2Sphere): Boolean; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectSphere(const s: TG2Sphere; var Pt: TG2Vec3): Boolean; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectAABox(const b: TG2AABox): Boolean; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectAABox(const b: TG2AABox; var Pt: TG2Vec3): Boolean; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectPlane(const Plane: TG2Plane; const BackFace: Boolean; var Distance: Single): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

  TG2FrustumCheck = (
    fcInside,
    fcIntersect,
    fcOutside
  );

  PG2Frustum = ^TG2Frustum;
  TG2Frustum = record
  strict private
    var m_Planes: array[0..5] of TG2Plane;
    var m_RefV: PG2Mat;
    var m_RefP: PG2Mat;
    function GetPlane(const Index: integer): PG2Plane; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Normalize; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property RefV: PG2Mat read m_RefV write m_RefV;
    property RefP: PG2Mat read m_RefP write m_RefP;
    property Planes[const Index: Integer]: PG2Plane read GetPlane;
    procedure Update;
    procedure ExtractPoints(const OutV: PG2Vec3);
    function PointInFrustum(const V: TG2Vec3): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function BoxInFrustum(const MinV, MaxV: TG2Vec3): Boolean; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function BoxInFrustum(const AABox: TG2AABox): Boolean; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SphereInFrustum(const C: TG2Vec3; const R: Single): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function FrustumCheckBox(const MinV, MaxV: TG2Vec3): TG2FrustumCheck; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function FrustumCheckSphere(const C: TG2Vec3; const R: Single): TG2FrustumCheck; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DistanceToPoint(const PlaneIndex: Integer; Pt: TG2Vec3): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IntersectFrustum(const Frustum: PG2Frustum): Boolean;
    function Project(const W, V, P: TG2Mat; const Viewport: TD3DViewport9): TRect;
  end;

  PFloatArray = ^TFloatArray;
  TFloatArray = array[0..0] of Single;

  PG2Vec2Array = ^TG2Vec2Array;
  TG2Vec2Array = array[0..0] of TG2Vec2;

  PG2Vec3Array = ^TG2Vec3Array;
  TG2Vec3Array = array[0..0] of TG2Vec3;

  PG2Vec4Array = ^TG2Vec4Array;
  TG2Vec4Array = array[0..0] of TG2Vec4;

  PG2MatArray = ^TG2MatArray;
  TG2MatArray = array[0..0] of TG2Mat;

  PG2MatPtrArray = ^TG2MatPtrArray;
  TG2MatPtrArray = array[0..0] of PG2Mat;

  PG2MD5 = ^TG2MD5;
  TG2MD5 = record
  public
    var b: array[0..15] of Byte;
    procedure SetValue(const Value: PByte; const Count: Integer); overload;
    procedure SetValue(const Value: AnsiString); overload;
    function ToAnsiString: AnsiString;
    class operator Equal(const md5a, md5b: TG2MD5): Boolean;
    class operator NotEqual(const md5a, md5b: TG2MD5): Boolean;
  end;

  TG2PolyTriang = record
  public
    var v: array of TG2Vec2;
    var Triangles: array of array[0..2] of Integer;
  end;
  PG2PolyTriang = ^TG2PolyTriang;

  TG2ProcMatAdd = procedure (const OutM, InM1, InM2: PG2Mat);
  TG2ProcMatSub = procedure (const OutM, InM1, InM2: PG2Mat);
  TG2ProcMatFltMul = procedure (const OutM, InM: PG2Mat; const s: Single);
  TG2ProcMatMul = procedure (const OutM, InM1, InM2: PG2Mat);
  TG2ProcMatInv = procedure (const OutM, InM: PG2Mat);
  TG2ProcVec3MatMul = procedure (const OutV, InV: PG2Vec3; const InM: PG2Mat);
  TG2ProcVec4MatMul = procedure (const OutV, InV: PG2Vec4; const InM: PG2Mat);
  TG2FuncVec3Len = function (const InV: PG2Vec3): Single;
  TG2FuncVec4Len = function (const InV: PG2Vec4): Single;
  TG2ProcVec3Norm = procedure (const OutV, InV: PG2Vec3);
  TG2ProcVec4Norm = procedure (const OutV, InV: PG2Vec4);
  TG2ProcVec3Cross = procedure (const OutV, InV1, InV2: PG2Vec3);
  TG2ProcMat2Mul = procedure (const OutM, InM1, InM2: PG2Mat2);
  TG2ProcVec2Mat2Mul = procedure (const OutV, InV: PG2Vec2; const InM: PG2Mat2);

var
  G2MatAdd: TG2ProcMatAdd;
  G2MatSub: TG2ProcMatSub;
  G2MatFltMul: TG2ProcMatFltMul;
  G2MatMul: TG2ProcMatMul;
  G2MatInv: TG2ProcMatInv;
  G2Vec3MatMul3x3: TG2ProcVec3MatMul;
  G2Vec3MatMul4x3: TG2ProcVec3MatMul;
  G2Vec3MatMul4x4: TG2ProcVec3MatMul;
  G2Vec4MatMul: TG2ProcVec4MatMul;
  G2Vec3Len: TG2FuncVec3Len;
  G2Vec4Len: TG2FuncVec4Len;
  G2Vec3Norm: TG2ProcVec3Norm;
  G2Vec4Norm: TG2ProcVec4Norm;
  G2Vec3Cross: TG2ProcVec3Cross;
  G2Mat2Mul: TG2ProcMat2Mul;
  G2Vec2Mat2Mul: TG2ProcVec2Mat2Mul;

  SysMMX: Boolean = False;
  SysSSE: Boolean = False;
  SysSSE2: Boolean = False;
  SysSSE3: Boolean = False;

function G2Vec2(const _X, _Y: Single): TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec3(const _X, _Y, _Z: Single): TG2Vec3; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec3(const _V: TG2Vec2; const _Z: Single): TG2Vec3; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec4(const _X, _Y, _Z, _W: Single): TG2Vec4; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec4(const _V: TG2Vec2; const _Z, _W: Single): TG2Vec4; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec4(const _V: TG2Vec3; const _W: Single): TG2Vec4; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Quat(const _X, _Y, _Z, _W: Single): TG2Quat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Ray(const _Origin, _Direction: TG2Vec3): TG2Ray; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Plane(const V, N: TG2Vec3): TG2Plane; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2LerpColor(const c1, c2: DWord; const s: Single): DWord;
function G2LerpFloat(const f1, f2, s: Single): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2LerpVec2(const v1, v2: TG2Vec2; const s: Single): TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2LerpVec3(const v1, v2: TG2Vec3; const s: Single): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2LerpVec4(const v1, v2: TG2Vec4; const s: Single): TG2Vec4; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2LerpQuat(const q1, q2: TG2Quat; const s: Single): TG2Quat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2NLerpQuat(const q1, q2: TG2Quat; const s: Single): TG2Quat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2SLerpQuat(const q1, q2: TG2Quat; const s: Single): TG2Quat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2CosrpFloat(const f1, f2: Single; const s: Single): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec2InRect(const v, MinV, MaxV: TG2Vec2): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec2InPoly(const v: TG2Vec2; const VArr: PG2Vec2Array; const VCount: Integer): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec2ToLine(const l1, l2, v: TG2Vec2; var VecOnLine: TG2Vec2; var InSegment: boolean): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec3ToLine(const l1, l2, v: TG2Vec3; var VecOnLine: TG2Vec3; var InSegment: boolean): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2RectInRect(const MinV1, MaxV1, MinV2, MaxV2: TG2Vec2): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec2Reflect(const v, n: TG2Vec2): TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec2AngleOX(const v1, v2: TG2Vec2): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec2AngleOY(const v1, v2: TG2Vec2): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Intersect3Planes(const p1, p2, p3: TG2Plane): TG2Vec3;
function G2Vec3Rotation(const SrcV, DstV: TG2Vec3): TG2Quat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2TriangleNormal(const v1, v2, v3: TG2Vec3): TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2FaceTBN(
  const v1, v2, v3: TG2Vec3;
  const uv1, uv2, uv3: TG2Vec2;
  var T, B, N: TG2Vec3
);
function G2MatLerp(const Mat1, Mat2: TG2Mat; const s: Single): TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2MatSLerp(const Mat1, Mat2: TG2Mat; const s: Single): TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2SinCos(const Angle: Single; var s, c: Single); assembler;
function G2MD5(const Value: PByte; const Count: DWord): TG2MD5; overload;
function G2MD5(const Value: AnsiString): TG2MD5; overload;
function G2CRC16(const Value: PByte; const Count: Integer): Word;
function G2CRC32(const Value: PByte; const Count: Integer): DWord;
function G2PolyTriangulate(const Triang: PG2PolyTriang): Boolean;

procedure G2MatAddSSE(const OutM, InM1, InM2: PG2Mat); assembler;
procedure G2MatAddStd(const OutM, InM1, InM2: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2MatSubSSE(const OutM, InM1, InM2: PG2Mat); assembler;
procedure G2MatSubStd(const OutM, InM1, InM2: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2MatMulSSE(const OutM, InM1, InM2: PG2Mat); assembler;
procedure G2MatMulStd(const OutM, InM1, InM2: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2MatInvSSE(const OutM, InM: PG2Mat); assembler;
procedure G2MatInvStd(const OutM, InM: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec2MatMul3x3Std(const OutV, InV: PG2Vec2; const InM: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec2MatMul4x3Std(const OutV, InV: PG2Vec2; const InM: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec2MatMul4x4Std(const OutV, InV: PG2Vec2; const InM: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec3MatMul3x3SSE(const OutV, InV: PG2Vec3; const InM: PG2Mat); assembler;
procedure G2Vec3MatMul3x3Std(const OutV, InV: PG2Vec3; const InM: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec3MatMul4x3SSE(const OutV, InV: PG2Vec3; const InM: PG2Mat); assembler;
procedure G2Vec3MatMul4x3Std(const OutV, InV: PG2Vec3; const InM: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec3MatMul4x4SSE(const OutV, InV: PG2Vec3; const InM: PG2Mat); assembler;
procedure G2Vec3MatMul4x4Std(const OutV, InV: PG2Vec3; const InM: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec4MatMulSSE(const OutV, InV: PG2Vec4; const InM: PG2Mat); assembler;
procedure G2Vec4MatMulStd(const OutV, InV: PG2Vec4; const InM: PG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec3LenSSE(const InV: PG2Vec3): Single; assembler;
function G2Vec3LenStd(const InV: PG2Vec3): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Vec4LenSSE(const InV: PG2Vec4): Single; assembler;
function G2Vec4LenStd(const InV: PG2Vec4): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec3NormSSE(const OutV, InV: PG2Vec3); assembler;
procedure G2Vec3NormStd(const OutV, InV: PG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec4NormSSE(const OutV, InV: PG2Vec4); assembler;
procedure G2Vec4NormStd(const OutV, InV: PG2Vec4); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec3CrossSSE(const OutV, InV1, InV2: PG2Vec3); assembler;
procedure G2Vec3CrossStd(const OutV, InV1, InV2: PG2Vec3); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Mat2MulSSE(const OutM, InM1, InM2: PG2Mat2); assembler;
procedure G2Mat2MulStd(const OutM, InM1, InM2: PG2Mat2); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2Vec2Mat2MulSSE(const OutV, InV: PG2Vec2; const InM: PG2Mat2); assembler;
procedure G2Vec2Mat2MulStd(const OutV, InV: PG2Vec2; const InM: PG2Mat2); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}

const
  TwoPi = Pi * 2;
  HalfPi = Pi * 0.5;
  QuatPi = Pi * 0.25;
  Rcp255 = 1 / 255;
  RcpTwoPi = 1 / TwoPi;
  RcpPi = 1 / Pi;
  RcpHalfPi = 1 / HalfPi;
  RcpQuatPi = 1 / QuatPi;
  G2EPS = 1E-7;
  RadToDeg = 180 / Pi;
  DegToRad = Pi / 180;

implementation

//TG2Mat BEGIN
function TG2Mat.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@e00)^[Index];
end;

procedure TG2Mat.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@e00)^[Index] := Value;
end;

function TG2Mat.GetMat(const ix, iy: Integer): Single;
begin
  Result := PG2MatArr(@e00)^[ix, iy];
end;

procedure TG2Mat.SetMat(const ix, iy: Integer; const Value: Single);
begin
  PG2MatArr(@e00)^[ix, iy] := Value;
end;

class operator TG2Mat.Explicit(const m: TG2Mat): TG2MatRef;
begin
  Result := PG2MatRef(@m)^;
end;

class operator TG2Mat.Explicit(const m: TG2MatRef): TG2Mat;       
begin
  Result := PG2Mat(@m)^;
end;

class operator TG2Mat.Implicit(const m: TG2Mat): TG2MatRef;   
begin
  Result := PG2MatRef(@m)^;
end;

class operator TG2Mat.Implicit(const m: TG2MatRef): TG2Mat; 
begin
  Result := PG2Mat(@m)^;
end;

class operator TG2Mat.Equal(const m1, m2: TG2Mat): Boolean;   
begin
  Result := CompareMem(@m1, @m2, SizeOf(TG2Mat));
end;

class operator TG2Mat.NotEqual(const m1, m2: TG2Mat): Boolean;  
begin
  Result := not CompareMem(@m1, @m2, SizeOf(TG2Mat));
end;

class operator TG2Mat.Add(const m1, m2: TG2Mat): TG2Mat;     
begin
  G2MatAdd(@Result, @m1, @m2);
end;

class operator TG2Mat.Subtract(const m1, m2: TG2Mat): TG2Mat;  
begin
  G2MatSub(@Result, @m1, @m2);
end;

class operator TG2Mat.Multiply(const m1, m2: TG2Mat): TG2Mat;       
begin
  G2MatMul(@Result, @m1, @m2);
end;

procedure TG2Mat.SetValue(
      const m00, m10, m20, m30: Single;
      const m01, m11, m21, m31: Single;
      const m02, m12, m22, m32: Single;
      const m03, m13, m23, m33: Single
    );
begin
  e00 := m00; e10 := m10; e20 := m20; e30 := m30;
  e01 := m01; e11 := m11; e21 := m21; e31 := m31;
  e02 := m02; e12 := m12; e22 := m22; e32 := m32;
  e03 := m03; e13 := m13; e23 := m23; e33 := m33;
end;

procedure TG2Mat.SetValue(const AxisX, AxisY, AxisZ, Translation: TG2Vec3Ref);
begin
  e00 := AxisX.x; e10 := AxisY.x; e20 := AxisZ.x; e30 := Translation.x;
  e01 := AxisX.y; e11 := AxisY.y; e21 := AxisZ.y; e31 := Translation.y;
  e02 := AxisX.z; e12 := AxisY.z; e22 := AxisZ.z; e32 := Translation.z;
  e03 := 0; e13 := 0; e23 := 0; e33 := 1;
end;

procedure TG2Mat.SetIdentity;
begin
  SetValue(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetScaling(const _X, _Y, _Z: Single);
begin
  SetValue(
    _X, 0, 0, 0,
    0, _Y, 0, 0,
    0, 0, _Z, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetScaling(const _v: TG2Vec3Ref);   
begin
  SetValue(
    _v.x, 0, 0, 0,
    0, _v.y, 0, 0,
    0, 0, _v.z, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetScaling(const _s: Single);
begin
  SetValue(
    _s, 0, 0, 0,
    0, _s, 0, 0,
    0, 0, _s, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetTranslation(const _X, _Y, _Z: Single);
begin
  SetValue(
    1, 0, 0, _X,
    0, 1, 0, _Y,
    0, 0, 1, _Z,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetTranslation(const _v: TG2Vec3Ref);
begin
  SetValue(
    1, 0, 0, _v.x,
    0, 1, 0, _v.y,
    0, 0, 1, _v.z,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetRotationX(const _Angle: Single);
var
  s, c: Single;
begin
  G2SinCos(_Angle, s, c);
  SetValue(
    1, 0, 0, 0,
    0, c, -s, 0,
    0, s, c, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetRotationY(const _Angle: Single);  
var
  s, c: Single;
begin
  G2SinCos(_Angle, s, c);
  SetValue(
    c, 0, s, 0,
    0, 1, 0, 0,
    -s, 0, c, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetRotationZ(const _Angle: Single);  
var
  s, c: Single;
begin
  G2SinCos(_Angle, s, c);
  SetValue(
    c, -s, 0, 0,
    s, c, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetRotation(const _X, _Y, _Z, _Angle: Single);
begin
  SetRotation(G2Vec3(_X, _Y, _Z), _Angle);
end;

procedure TG2Mat.SetRotation(const _v: TG2Vec3Ref; const _Angle: Single);
var
  vr: TG2Vec3 absolute _v;
  v: TG2Vec3;
  s, c, cr,
  xs, ys, zs,
  crxy, crxz, cryz: Single;
begin
  v := vr.Normalized;
  G2SinCos(_Angle, s, c);
  cr := 1 - c;
  xs := v.x * s;
  ys := v.y * s;
  zs := v.z * s;
  crxy := cr * v.x * v.y;
  crxz := cr * v.x * v.z;
  cryz := cr * v.y * v.z;
  SetValue(
    cr * v.x * v.x + c, -zs + crxy, ys + crxz, 0,
    zs + crxy, cr * v.y * v.y + c, -xs + cryz, 0,
    -ys + crxz, xs + cryz, cr * v.z * v.z + c, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetRotation(const _q: TG2QuatRef);
var
  xx, yy, zz,
  xy, xz, yz,
  wx, wy, wz: Single;
begin
  xx := 2 * _q.x * _q.x;
  yy := 2 * _q.y * _q.y;
  zz := 2 * _q.z * _q.z;
  xy := 2 * _q.x * _q.y;
  xz := 2 * _q.x * _q.z;
  yz := 2 * _q.y * _q.z;
  wx := 2 * _q.w * _q.x;
  wy := 2 * _q.w * _q.y;
  wz := 2 * _q.w * _q.z;
  SetValue(
    1 - yy - zz, xy - wz, xz + wy, 0,
    xy + wz, 1 - xx - zz, yz - wx, 0,
    xz - wy, yz + wx, 1 - xx - yy, 0,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.Scale(const X, Y, Z: Single);
var
  m: TG2Mat;
begin
  m.SetScaling(X, Y, Z);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.Scale(const v: TG2Vec3Ref);
var
  m: TG2Mat;
begin
  m.SetScaling(v);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.Translate(const X, Y, Z: Single);
var
  m: TG2Mat;
begin
  m.SetTranslation(X, Y, Z);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.Translate(const v: TG2Vec3Ref);
var
  m: TG2Mat;
begin
  m.SetTranslation(v);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.RotateX(const Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotationX(Angle);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.RotateY(const Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotationY(Angle);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.RotateZ(const Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotationZ(Angle);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.Rotate(const X, Y, Z, Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotation(X, Y, Z, Angle);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.Rotate(const v: TG2Vec3Ref; const Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotation(v, Angle);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.Rotate(const q: TG2QuatRef);
var
  m: TG2Mat;
begin
  m.SetRotation(q);
  G2MatMul(@Self, @Self, @m);
end;

procedure TG2Mat.PreScale(const X, Y, Z: Single);
var
  m: TG2Mat;
begin
  m.SetScaling(X, Y, Z);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreScale(const v: TG2Vec3Ref);
var
  m: TG2Mat;
begin
  m.SetScaling(v);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreTranslate(const X, Y, Z: Single);
var
  m: TG2Mat;
begin
  m.SetTranslation(X, Y, Z);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreTranslate(const v: TG2Vec3Ref);
var
  m: TG2Mat;
begin
  m.SetTranslation(v);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreRotateX(const Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotationX(Angle);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreRotateY(const Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotationY(Angle);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreRotateZ(const Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotationZ(Angle);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreRotate(const X, Y, Z, Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotation(X, Y, Z, Angle);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreRotate(const v: TG2Vec3Ref; const Angle: Single);
var
  m: TG2Mat;
begin
  m.SetRotation(v, Angle);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.PreRotate(const q: TG2QuatRef);
var
  m: TG2Mat;
begin
  m.SetRotation(q);
  G2MatMul(@Self, @m, @Self);
end;

procedure TG2Mat.SetView(const Pos, Target, Up: TG2Vec3Ref);
var
  VPos: TG2Vec3 absolute Pos;
  VTarget: TG2Vec3 absolute Target;
  VUp: TG2Vec3 absolute Up;
  VecX, VecY, VecZ: TG2Vec3;
begin
  VecZ := (VTarget - VPos).Normalized;
  VecX := VUp.Cross(VecZ).Normalized;
  VecY := VecZ.Cross(VecX).Normalized;
  SetValue(
    VecX.x, VecX.y, VecX.z, -VecX.Dot(VPos),
    VecY.x, VecY.y, VecY.z, -VecY.Dot(VPos),
    VecZ.x, VecZ.y, VecZ.z, -VecZ.Dot(VPos),
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetOrthogonal(const Width, Height, ZNear, ZFar: Single);
var
  RcpD: Single;
begin
  RcpD := 1 / (ZFar - ZNear);
  SetValue(
    2 / Width, 0, 0, 0,
    0, 2 / Height, 0, 0,
    0, 0, RcpD, -ZNear * RcpD,
    0, 0, 0, 1
  );
end;

procedure TG2Mat.SetPerspective(const FOV, Aspect, ZNear, ZFar: Single);
var
  ct, q: Single;
begin
  ct := CoTan(FOV * 0.5);
  q := ZFar / (ZFar - ZNear);
  SetValue(
    ct / Aspect, 0, 0, 0,
    0, ct, 0, 0,
    0, 0, q, -q * ZNear,
    0, 0, 1, 0
  );
end;

procedure TG2Mat.Decompose(const Scaling: PG2Vec3Ref; const Rotation: PG2QuatRef; const Translation: PG2Vec3Ref);
begin
  D3DXMatrixDecompose(
    PD3DXVector3(Scaling),
    PD3DXQuaternion(Rotation),
    PD3DXVector3(Translation),
    PG2MatRef(@Self)^
  );
end;

function TG2Mat.GetTranslation: TG2Vec3Ref;
begin
  Result.x := e30; Result.y := e31; Result.z := e32;
end;

function TG2Mat.GetRotation: TG2QuatRef;
var
  Trace, d: Single;
begin
  Trace := e00 + e11 + e22;
  with Result do
  if Trace > 0 then
  begin
    w := Sqrt(Trace + 1) * 0.5;
    d := 1 / (4 * w);
    x := (e12 - e21) * d;
    y := (e20 - e02) * d;
    z := (e01 - e10) * d;
  end
  else
  begin
    if (e00 > e11) and (e00 > e22) then
    begin
      x := Sqrt(e00 - e11 - e22 + 1) * 0.5;
      d := 1 / (4 * x);
      y := (e10 + e01) * d;
      z := (e20 + e02) * d;
      w := (e21 - e12) * d;
    end
    else if (e11 > e22) then
    begin
      y := Sqrt(e11 - e00 - e22 + 1) * 0.5;
      d := 1 / (4 * y);
      x := (e10 + e01) * d;
      z := (e21 + e12) * d;
      w := (e02 - e20) * d;
    end
    else
    begin
      z := Sqrt(e22 - e00 - e11 + 1) * 0.5;
      d := 1 / (4 * z);
      x := (e20 + e02) * d;
      y := (e21 + e12) * d;
      w := (e10 - e01) * d;
    end;
  end;
end;

function TG2Mat.GetScaling: TG2Vec3Ref;
begin
  Result.x := Sqrt(e00 * e00 + e10 * e10 + e20 * e20);
  Result.y := Sqrt(e01 * e01 + e11 * e11 + e21 * e21);
  Result.z := Sqrt(e02 * e02 + e12 * e12 + e22 * e22);
end;

function TG2Mat.Determinant: Single;
  var e_22_33_32_23: Single;
  var e_12_33_32_13: Single;
  var e_12_23_22_13: Single;
  var e_02_33_32_03: Single;
  var e_02_23_22_03: Single;
  var e_02_13_12_03: Single;
begin
  e_22_33_32_23 := e22 * e33 - e32 * e23;
  e_12_33_32_13 := e12 * e33 - e32 * e13;
  e_12_23_22_13 := e12 * e23 - e22 * e13;
  e_02_33_32_03 := e02 * e33 - e32 * e03;
  e_02_23_22_03 := e02 * e23 - e22 * e03;
  e_02_13_12_03 := e02 * e13 - e12 * e03;
  Result := (
    e00 * (e11 * e_22_33_32_23 - e21 * e_12_33_32_13 + e31 * e_12_23_22_13) -
    e10 * (e01 * e_22_33_32_23 - e21 * e_02_33_32_03 + e31 * e_02_23_22_03) +
    e20 * (e01 * e_12_33_32_13 - e11 * e_02_33_32_03 + e31 * e_02_13_12_03) -
    e30 * (e01 * e_12_23_22_13 - e11 * e_02_23_22_03 + e21 * e_02_13_12_03)
  );
end;

function TG2Mat.Transpose: TG2Mat;
var
  rm: TG2Mat;
begin
  rm.SetValue(
    e00, e01, e02, e03,
    e10, e11, e12, e13,
    e20, e21, e22, e23,
    e30, e31, e32, e33
  );
  Result := rm;
end;

function TG2Mat.Inverse: TG2Mat;
begin
  G2MatInv(@Result, @Self);
end;

function TG2Mat.Mat4x3: TG2Mat;
var
  rm: TG2Mat;
begin
  rm.SetValue(
    e00, e10, e20, e30,
    e01, e11, e21, e31,
    e02, e12, e22, e32,
    0, 0, 0, 1
  );
  Result := rm;
end;

function TG2Mat.Mat3x3: TG2Mat;
var
  rm: TG2Mat;
begin
  rm.SetValue(
    e00, e10, e20, 0,
    e01, e11, e21, 0,
    e02, e12, e22, 0,
    0, 0, 0, 1
  );
  Result := rm;
end;
//TG2Mat END

//TG2Mat2 BEGIN
function TG2Mat2.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@e00)^[Index];
end;

procedure TG2Mat2.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@e00)^[Index] := Value;
end;

class operator TG2Mat2.Explicit(const m: TG2Mat2): TG2Mat2Ref;
begin
  Result := PG2Mat2Ref(@m)^;
end;

class operator TG2Mat2.Explicit(const m: TG2Mat2Ref): TG2Mat2;
begin
  Result := PG2Mat2(@m)^;
end;

class operator TG2Mat2.Implicit(const m: TG2Mat2): TG2Mat2Ref;
begin
  Result := PG2Mat2Ref(@m)^;
end;

class operator TG2Mat2.Implicit(const m: TG2Mat2Ref): TG2Mat2;
begin
  Result := PG2Mat2(@m)^;
end;

class operator TG2Mat2.Equal(const m1, m2: TG2Mat2): Boolean;
begin
  Result := CompareMem(@m1, @m2, SizeOf(TG2Mat2));
end;

class operator TG2Mat2.NotEqual(const m1, m2: TG2Mat2): Boolean;  
begin
  Result := not CompareMem(@m1, @m2, SizeOf(TG2Mat2));
end;

class operator TG2Mat2.Add(const m1, m2: TG2Mat2): TG2Mat2;
begin
  with Result do
  begin
    e00 := m1.e00 + m2.e00; e10 := m1.e10 + m2.e10;
    e01 := m1.e01 + m2.e01; e11 := m1.e11 + m2.e11;
  end;
end;

class operator TG2Mat2.Subtract(const m1, m2: TG2Mat2): TG2Mat2;   
begin
  with Result do
  begin
    e00 := m1.e00 - m2.e00; e10 := m1.e10 - m2.e10;
    e01 := m1.e01 - m2.e01; e11 := m1.e11 - m2.e11;
  end;
end;

class operator TG2Mat2.Multiply(const m1, m2: TG2Mat2): TG2Mat2;    
begin
  G2Mat2Mul(@Result, @m1, @m2);
end;

class operator TG2Mat2.Multiply(const m: TG2Mat2; s: Single): TG2Mat2; 
begin
  Result.e00 := m.e00 * s;
  Result.e01 := m.e01 * s;
  Result.e10 := m.e10 * s;
  Result.e11 := m.e11 * s;
end;

procedure TG2Mat2.SetIdentity;
begin
  e00 := 1; e10 := 0; e01 := 0; e11 := 1;
end;

procedure TG2Mat2.SetScaling(const _X, _Y: Single); 
begin
  e00 := _X; e10 := 0; e01 := 0; e11 := _Y;
end;

procedure TG2Mat2.SetRotation(const _Angle: Single);
var
  s, c: Extended;
begin
  SinCos(_Angle, s, c);
  e00 := c;
  e10 := -s;
  e01 := s;
  e11 := c;
end;

procedure TG2Mat2.Scale(const X, Y: Single);
var
  m: TG2Mat2;  
begin
  m.SetScaling(X, Y);
  Self := Self * m;
end;

procedure TG2Mat2.Rotate(const Angle: Single); 
var
  m: TG2Mat2;  
begin
  m.SetRotation(Angle);
  Self := Self * m;
end;

function TG2Mat2.Determinant: Single;     
begin
  Result := e00 * e11 - e10 * e01;
end;

function TG2Mat2.Transpose: TG2Mat2;  
var
  rm: TG2Mat2;
begin
  rm.e10 := e01;
  rm.e01 := e10;
  Result := rm;
end;

function TG2Mat2.Inverse: TG2Mat2; 
var
  D: Single;
  rm: TG2Mat2;
begin
  D := 1 / Determinant;
  rm.e00 := e11 * D;
  rm.e01 := -e01 * D;
  rm.e10 := -e10 * D;
  rm.e11 := e00 * D;
  Result := rm;
end;

function TG2Mat2.GetRotation: Single;  
begin
  Result := ArcTan2(e01, e00);
end;
//TG2Mat2 END

//TG2Vec2 BEGIN
function TG2Vec2.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@x)^[Index];
end;

procedure TG2Vec2.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@x)^[Index] := Value;
end;

class operator TG2Vec2.Negative(const v: TG2Vec2): TG2Vec2;
begin
  Result.x := -v.x;
  Result.y := -v.y;
end;

class operator TG2Vec2.Explicit(const v: TG2Vec2): TG2Vec2Ref;  
begin
  Result := PG2Vec2Ref(@v)^;
end;

class operator TG2Vec2.Explicit(const v: TG2Vec2Ref): TG2Vec2;  
begin
  Result := PG2Vec2(@v)^;
end;

class operator TG2Vec2.Explicit(const v: TPoint): TG2Vec2;      
begin
  Result.x := v.X;
  Result.y := v.Y;
end;

class operator TG2Vec2.Implicit(const v: TG2Vec2): TG2Vec2Ref; 
begin
  Result := PG2Vec2Ref(@v)^;
end;

class operator TG2Vec2.Implicit(const v: TG2Vec2Ref): TG2Vec2;  
begin
  Result := PG2Vec2(@v)^;
end;

class operator TG2Vec2.Implicit(const v: TPoint): TG2Vec2;        
begin
  Result.x := v.X;
  Result.y := v.Y;
end;

class operator TG2Vec2.Equal(const v1, v2: TG2Vec2): Boolean;    
begin
  Result := CompareMem(@v1, @v2, SizeOf(TG2Vec2));
end;

class operator TG2Vec2.NotEqual(const v1, v2: TG2Vec2): Boolean;   
begin
  Result := not CompareMem(@v1, @v2, SizeOf(TG2Vec2));
end;

class operator TG2Vec2.Add(const v1, v2: TG2Vec2): TG2Vec2;       
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
end;

class operator TG2Vec2.Add(const v: TG2Vec2; const s: Single): TG2Vec2;
begin
  Result.x := v.x + s;
  Result.y := v.y + s;
end;

class operator TG2Vec2.Subtract(const v1, v2: TG2Vec2): TG2Vec2;      
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
end;

class operator TG2Vec2.Subtract(const v: TG2Vec2; const s: Single): TG2Vec2;   
begin
  Result.x := v.x - s;
  Result.y := v.y - s;
end;

class operator TG2Vec2.Multiply(const v1, v2: TG2Vec2): Single;  
begin
  Result := v1.x * v2.x + v1.y * v2.y;
end;

class operator TG2Vec2.Multiply(const v: TG2Vec2; const s: Single): TG2Vec2;     
begin
  Result.x := v.x * s; Result.y := v.y * s;
end;

class operator TG2Vec2.Multiply(const v: TG2Vec2; const m: TG2Mat): TG2Vec2;  
begin
  G2Vec2MatMul4x3Std(@Result, @v, @m);
end;

class operator TG2Vec2.Multiply(const v: TG2Vec2; const m: TG2Mat2): TG2Vec2;  
begin
  G2Vec2Mat2Mul(@Result, @v, @m);
end;

class operator TG2Vec2.Divide(const v: TG2Vec2; const s: Single): TG2Vec2;  
var
  RcpS: Single;
begin
  RcpS := 1 / s;
  Result.x := v.x * RcpS;
  Result.y := v.y * RcpS;
end;

procedure TG2Vec2.SetValue(const _X, _Y: Single);
begin
  x := _X; y := _Y;
end;

procedure TG2Vec2.Normalize;
begin
  D3DXVec2Normalize(PG2Vec2Ref(@Self)^, PG2Vec2Ref(@Self)^);
end;

function TG2Vec2.Normalized: TG2Vec2;
begin
  D3DXVec2Normalize(PG2Vec2Ref(@Result)^, PG2Vec2Ref(@Self)^);
end;

function TG2Vec2.Len: Single;
begin
  Result := D3DXVec2Length(PG2Vec2Ref(@Self)^);
end;

function TG2Vec2.LenSq: Single;
begin
  Result := Sqr(x) + Sqr(y);
end;

function TG2Vec2.Dot(const v: TG2Vec2): Single;
begin
  Result := x * v.x + y * v.y;
end;

function TG2Vec2.Cross(const v: TG2Vec2): Single;
begin
  Result := x * v.y - y * v.x;
end;

function TG2Vec2.Angle(const v: TG2Vec2): Single;
  var VLen: Single;
begin
  VLen := Len * v.Len;
  if VLen > 0 then
  Result := ArcCos(Dot(v) / VLen)
  else
  Result := 0;
end;

function TG2Vec2.Perp: TG2Vec2;
begin
  Result := G2Vec2(-y, x);
end;

function TG2Vec2.Reflect(const n: TG2Vec2): TG2Vec2;
  var Dot: Single;
begin
  Dot := Self.Dot(n);
  Result.SetValue(x - 2 * n.x * Dot, y - 2 * n.y * Dot);
end;

function TG2Vec2.Rotate(const Angle: Single): TG2Vec2;
  var s, c: Single;
begin
  G2SinCos(Angle, s, c);
  Result.x := x * c - y * s;
  Result.y := y * c + x * s;
end;

function TG2Vec2.SwizzleV2(const Shuffle: Word): TG2Vec2Ref;
begin
  Result.x := Arr[(Shuffle shr 8) and $f];
  Result.y := Arr[(Shuffle) and $f];
end;

function TG2Vec2.SwizzleV3(const Shuffle: Word): TG2Vec3Ref;
begin
  Result.x := Arr[(Shuffle shr 16) and $f];
  Result.y := Arr[(Shuffle shr 8) and $f];
  Result.z := Arr[(Shuffle) and $f];
end;

function TG2Vec2.SwizzleV4(const Shuffle: Word): TG2Vec4Ref;
begin
  Result.x := Arr[(Shuffle shr 24) and $f];
  Result.y := Arr[(Shuffle shr 16) and $f];
  Result.z := Arr[(Shuffle shr 8) and $f];
  Result.w := Arr[(Shuffle) and $f];
end;
//TG2Vec2 END

//TG2Vec3 BEGIN
function TG2Vec3.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@x)^[Index];
end;

procedure TG2Vec3.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@x)^[Index] := Value;
end;

class operator TG2Vec3.Negative(const v: TG2Vec3): TG2Vec3;
begin
  Result.x := -v.x;
  Result.y := -v.y;
  Result.z := -v.z;
end;

class operator TG2Vec3.Explicit(const v: TG2Vec3): TG2Vec3Ref;  
begin
  Result := PG2Vec3Ref(@v)^;
end;

class operator TG2Vec3.Explicit(const v: TG2Vec3Ref): TG2Vec3;  
begin
  Result := PG2Vec3(@v)^;
end;

class operator TG2Vec3.Implicit(const v: TG2Vec3): TG2Vec3Ref;  
begin
  Result := PG2Vec3Ref(@v)^;
end;

class operator TG2Vec3.Implicit(const v: TG2Vec3Ref): TG2Vec3;   
begin
  Result := PG2Vec3(@v)^;
end;

class operator TG2Vec3.Equal(const v1, v2: TG2Vec3): Boolean;     
begin
  Result := CompareMem(@v1, @v2, SizeOf(TG2Vec3));
end;

class operator TG2Vec3.NotEqual(const v1, v2: TG2Vec3): Boolean;    
begin
  Result := not CompareMem(@v1, @v2, SizeOf(TG2Vec3));
end;

class operator TG2Vec3.Add(const v1, v2: TG2Vec3): TG2Vec3;          
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
  Result.z := v1.z + v2.z;
end;

class operator TG2Vec3.Add(const v: TG2Vec3; const s: Single): TG2Vec3;  
begin
  Result.x := v.x + s;
  Result.y := v.y + s;
  Result.z := v.z + s;
end;

class operator TG2Vec3.Subtract(const v1, v2: TG2Vec3): TG2Vec3;     
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
  Result.z := v1.z - v2.z;
end;

class operator TG2Vec3.Subtract(const v: TG2Vec3; const s: Single): TG2Vec3; 
begin
  Result.x := v.x - s;
  Result.y := v.y - s;
  Result.z := v.z - s;
end;

class operator TG2Vec3.Multiply(const v1, v2: TG2Vec3): Single;               
begin
  Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

class operator TG2Vec3.Multiply(const v: TG2Vec3; const s: Single): TG2Vec3;   
begin
  Result.x := v.x * s;
  Result.y := v.y * s;
  Result.z := v.z * s;
end;

class operator TG2Vec3.Multiply(const v: TG2Vec3; const m: TG2Mat): TG2Vec3;  
begin
  G2Vec3MatMul4x3(@Result, @v, @m);
end;

class operator TG2Vec3.Divide(const v: TG2Vec3; const s: Single): TG2Vec3;  
var
  RcpS: Single;
begin
  RcpS := 1 / s;
  Result.x := v.x * RcpS;
  Result.y := v.y * RcpS;
  Result.z := v.z * RcpS;
end;

procedure TG2Vec3.SetValue(const _X, _Y, _Z: Single);
begin
  x := _X; y := _Y; z := _Z;
end;

procedure TG2Vec3.SetValue(const v2: TG2Vec2; const _Z: Single);
begin
  x := v2.x; y := v2.y; z := _Z;
end;

procedure TG2Vec3.Normalize;
begin
  G2Vec3Norm(@Self, @Self);
end;

function TG2Vec3.Normalized: TG2Vec3;
begin
  G2Vec3Norm(@Result, @Self);
end;

function TG2Vec3.Transform3x3(const m: TG2Mat): TG2Vec3;
begin
  G2Vec3MatMul3x3(@Result, @Self, @m);
end;

function TG2Vec3.Transform4x3(const m: TG2Mat): TG2Vec3;
begin
  G2Vec3MatMul4x3(@Result, @Self, @m);
end;

function TG2Vec3.Transform4x4(const m: TG2Mat): TG2Vec3;
begin
  G2Vec3MatMul4x4(@Result, @Self, @m);
end;

function TG2Vec3.Len: Single;
begin
  Result := G2Vec3Len(@Self);
end;

function TG2Vec3.LenSq: Single;
begin
  Result := x * x + y * y + z * z;
end;

function TG2Vec3.Dot(const v: TG2Vec3): Single;
begin
  Result := x * v.x + y * v.y + z * v.z;
end;

function TG2Vec3.Cross(const v: TG2Vec3): TG2Vec3;    
begin
  G2Vec3Cross(@Result, @Self, @v);
end;

function TG2Vec3.Angle(const v: TG2Vec3): Single;
  var VLen: Single;
begin
  VLen := Len * v.Len;
  if VLen > 0 then
  Result := ArcCos(Dot(v) / VLen)
  else
  Result := 0;
end;

function TG2Vec3.InTriangle(const v0, v1, v2: TG2Vec3): Boolean;
var
  u, v, w: TG2Vec3;
  uu, uv, vv, wu, wv, d, InvD, s, t: Single;
begin
  u := v1 - v0;
	v := v2 - v0;
	w := Self - v0;

	uu := u.Dot(u);
	uv := u.Dot(v);
	vv := v.Dot(v);
	wu := w.Dot(u);
	wv := w.Dot(v);
	d := uv * uv - uu * vv;

	InvD := 1 / d;
	s := (uv * wv - vv * wu) * InvD;
	if (s < 0) or (s > 1) then
  begin
		Result := False;
    Exit;
  end;

	t := (uv * wu - uu * wv) * InvD;
	if (t < 0) or ((s + t) > 1) then
  begin
    Result := False;
    Exit;
  end;

	Result := True;
end;

function TG2Vec3.AsVec2: TG2Vec2;
begin
  Result := PG2Vec2(@Self)^;
end;

function TG2Vec3.SwizzleV2(const Shuffle: Word): TG2Vec2Ref;
begin
  Result.x := Arr[(Shuffle shr 8) and $f];
  Result.y := Arr[(Shuffle) and $f];
end;

function TG2Vec3.SwizzleV3(const Shuffle: Word): TG2Vec3Ref;
begin
  Result.x := Arr[(Shuffle shr 16) and $f];
  Result.y := Arr[(Shuffle shr 8) and $f];
  Result.z := Arr[(Shuffle) and $f];
end;

function TG2Vec3.SwizzleV4(const Shuffle: Word): TG2Vec4Ref;
begin
  Result.x := Arr[(Shuffle shr 24) and $f];
  Result.y := Arr[(Shuffle shr 16) and $f];
  Result.z := Arr[(Shuffle shr 8) and $f];
  Result.w := Arr[(Shuffle) and $f];
end;
//TG2Vec3 END

//TG2Vec4 BEGIN
function TG2Vec4.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@x)^[Index];
end;

procedure TG2Vec4.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@x)^[Index] := Value;
end;

class operator TG2Vec4.Negative(const v: TG2Vec4): TG2Vec4;
begin
  Result.x := -v.x;
  Result.y := -v.y;
  Result.z := -v.z;
  Result.w := -v.w;
end;

class operator TG2Vec4.Explicit(const v: TG2Vec4): TG2Vec4Ref;   
begin
  Result := PG2Vec4Ref(@v)^;
end;

class operator TG2Vec4.Explicit(const v: TG2Vec4Ref): TG2Vec4;   
begin
  Result := PG2Vec4(@v)^;
end;

class operator TG2Vec4.Explicit(const v: TG2Vec3): TG2Vec4;
begin
  Result.x := v.x;
  Result.y := v.y;
  Result.z := v.z;
end;

class operator TG2Vec4.Explicit(const v: TG2Vec2): TG2Vec4;
begin
  Result.x := v.x;
  Result.y := v.y;
end;

class operator TG2Vec4.Explicit(const s: Single): TG2Vec4;
begin
  Result.x := s;
  Result.y := s;
  Result.z := s;
  Result.w := s;
end;

class operator TG2Vec4.Implicit(const v: TG2Vec4): TG2Vec4Ref;
begin
  Result := PG2Vec4Ref(@v)^;
end;

class operator TG2Vec4.Implicit(const v: TG2Vec4Ref): TG2Vec4;  
begin
  Result := PG2Vec4(@v)^;
end;

class operator TG2Vec4.Implicit(const v: TG2Vec3): TG2Vec4;
begin
  Result.x := v.x;
  Result.y := v.y;
  Result.z := v.z;
end;

class operator TG2Vec4.Implicit(const v: TG2Vec2): TG2Vec4;
begin
  Result.x := v.x;
  Result.y := v.y;
end;

class operator TG2Vec4.Implicit(const s: Single): TG2Vec4;
begin
  Result.x := s;
  Result.y := s;
  Result.z := s;
  Result.w := s;
end;

class operator TG2Vec4.Equal(const v1, v2: TG2Vec4): Boolean;
begin
  Result := CompareMem(@v1, @v2, SizeOf(TG2Vec4));
end;

class operator TG2Vec4.NotEqual(const v1, v2: TG2Vec4): Boolean;    
begin
  Result := not CompareMem(@v1, @v2, SizeOf(TG2Vec4));
end;

class operator TG2Vec4.Add(const v1, v2: TG2Vec4): TG2Vec4;      
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
  Result.z := v1.z + v2.z;
  Result.w := v1.w + v2.w;
end;

class operator TG2Vec4.Add(const v: TG2Vec4; const s: Single): TG2Vec4;    
begin
  Result.x := v.x + s;
  Result.y := v.y + s;
  Result.z := v.z + s;
  Result.w := v.w + s;
end;

class operator TG2Vec4.Subtract(const v1, v2: TG2Vec4): TG2Vec4;     
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
  Result.z := v1.z - v2.z;
  Result.w := v1.w - v2.w;
end;

class operator TG2Vec4.Subtract(const v: TG2Vec4; const s: Single): TG2Vec4;    
begin
  Result.x := v.x - s;
  Result.y := v.y - s;
  Result.z := v.z - s;
  Result.w := v.w - s;
end;

class operator TG2Vec4.Multiply(const v1, v2: TG2Vec4): Single;      
begin
  Result := D3DXVec4Dot(PG2Vec4Ref(@v1)^, PG2Vec4Ref(@v2)^);
end;

class operator TG2Vec4.Multiply(const v: TG2Vec4; const s: Single): TG2Vec4;     
begin
  Result.x := v.x * s;
  Result.y := v.y * s;
  Result.z := v.z * s;
  Result.w := v.w * s;
end;

class operator TG2Vec4.Multiply(const v: TG2Vec4; const m: TG2Mat): TG2Vec4; 
begin
  G2Vec4MatMul(@Result, @v, @m);
end;

class operator TG2Vec4.Divide(const v: TG2Vec4; const s: Single): TG2Vec4;  
var
  RcpS: Single;
begin
  RcpS := 1 / s;
  Result.x := v.x * RcpS;
  Result.y := v.y * RcpS;
  Result.z := v.z * RcpS;
  Result.w := v.w * RcpS;
end;

procedure TG2Vec4.SetValue(const _X, _Y, _Z, _W: Single);   
begin
  x := _X; y := _Y; z := _Z; w := _W;
end;

procedure TG2Vec4.SetValue(const v3: TG2Vec3; _W: Single);
begin
  x := v3.x; y := v3.y; z := v3.z; w := _W;
end;

procedure TG2Vec4.SetValue(const v2: TG2Vec2; _Z, _W: Single);
begin
  x := v2.x; y := v2.y; z := _Z; w := _W;
end;

procedure TG2Vec4.Normalize;                    
begin
  G2Vec4Norm(@Self, @Self);
end;

function TG2Vec4.Normalized: TG2Vec4;
begin
  G2Vec4Norm(@Result, @Self);
end;

function TG2Vec4.Transform(const m: TG2Mat): TG2Vec4;
begin
  G2Vec4MatMul(@Result, @Self, @m);
end;

function TG2Vec4.Len: Single;                       
begin
  Result := G2Vec4Len(@Self);
end;

function TG2Vec4.LenSq: Single;
begin
  Result := x * x + y * y + z * z + w * w;
end;

function TG2Vec4.Dot(const v: TG2Vec4): Single;       
begin
  Result := x * v.x + y * v.y + z * v.z + w * v.w;
end;

function TG2Vec4.Cross(const v1, v2: TG2Vec4): TG2Vec3;     
begin
  D3DXVec4Cross(PG2Vec4Ref(@Result)^, PG2Vec4Ref(@Self)^, PG2Vec4Ref(@v1)^, PG2Vec4Ref(@v2)^);
end;

function TG2Vec4.Angle(const v: TG2Vec4): Single;
begin
  Result := ArcCos((Self * v) / (Len * v.Len));
end;

function TG2Vec4.AsVec3: TG2Vec3;
begin
  Result := PG2Vec3(@Self)^;
end;

function TG2Vec4.AsVec2: TG2Vec2;
begin
  Result := PG2Vec2(@Self)^;
end;

function TG2Vec4.SwizzleV2(const Shuffle: Word): TG2Vec2Ref;
begin
  Result.x := Arr[(Shuffle shr 8) and $f];
  Result.y := Arr[(Shuffle) and $f];
end;

function TG2Vec4.SwizzleV3(const Shuffle: Word): TG2Vec3Ref;
begin
  Result.x := Arr[(Shuffle shr 16) and $f];
  Result.y := Arr[(Shuffle shr 8) and $f];
  Result.z := Arr[(Shuffle) and $f];
end;

function TG2Vec4.SwizzleV4(const Shuffle: Word): TG2Vec4Ref;
begin
  Result.x := Arr[(Shuffle shr 24) and $f];
  Result.y := Arr[(Shuffle shr 16) and $f];
  Result.z := Arr[(Shuffle shr 8) and $f];
  Result.w := Arr[(Shuffle) and $f];
end;
//TG2Vec4 END

//TG2Quat BEGIN
function TG2Quat.GetArr(const Index: Integer): Single;
begin
  Result := PFLoatArray(@x)^[Index];
end;

procedure TG2Quat.SetArr(const Index: Integer; const Value: Single);
begin
  PFLoatArray(@x)^[Index] := Value;
end;

class operator TG2Quat.Explicit(const q: TG2Quat): TG2QuatRef;
begin
  Result := PG2QuatRef(@q)^;
end;

class operator TG2Quat.Explicit(const q: TG2QuatRef): TG2Quat;
begin
  Result := PG2Quat(@q)^;
end;

class operator TG2Quat.Implicit(const q: TG2Quat): TG2QuatRef;     
begin
  Result := PG2QuatRef(@q)^;
end;

class operator TG2Quat.Implicit(const q: TG2QuatRef): TG2Quat;  
begin
  Result := PG2Quat(@q)^;
end;

class operator TG2Quat.Equal(const q1, q2: TG2Quat): Boolean;  
begin
  Result := CompareMem(@q1, @q2, SizeOf(TG2Quat));
end;

class operator TG2Quat.NotEqual(const q1, q2: TG2Quat): Boolean;   
begin
  Result := not CompareMem(@q1, @q2, SizeOf(TG2Quat));
end;

class operator TG2Quat.Add(const q1, q2: TG2Quat): TG2Quat;
begin
  Result.x := q1.x + q2.x;
  Result.y := q1.y + q2.y;
  Result.z := q1.z + q2.z;
  Result.w := q1.w + q2.w;
end;

class operator TG2Quat.Add(const q: TG2Quat; const s: Single): TG2Quat;
begin
  Result.x := q.x + s;
  Result.y := q.y + s;
  Result.z := q.z + s;
  Result.w := q.w + s;
end;

class operator TG2Quat.Subtract(const q1, q2: TG2Quat): TG2Quat;
begin
  Result.x := q1.x - q2.x;
  Result.y := q1.y - q2.y;
  Result.z := q1.z - q2.z;
  Result.w := q1.w - q2.w;
end;

class operator TG2Quat.Subtract(const q: TG2Quat; const s: Single): TG2Quat;
begin
  Result.x := q.x - s;
  Result.y := q.y - s;
  Result.z := q.z - s;
  Result.w := q.w - s;
end;

class operator TG2Quat.Multiply(const q: TG2Quat; const s: Single): TG2Quat;
begin
  Result.x := q.x * s;
  Result.y := q.y * s;
  Result.z := q.z * s;
  Result.w := q.w * s;
end;

class operator TG2Quat.Multiply(const q1, q2: TG2Quat): TG2Quat;  
begin
  D3DXQuaternionMultiply(PG2QuatRef(@Result)^, PG2QuatRef(@q1)^, PG2QuatRef(@q2)^);
end;

procedure TG2Quat.SetValue(const _X, _Y, _Z, _W: Single);
begin
  x := _X; y := _Y; z := _Z; w := _W;
end;

procedure TG2Quat.SetIdentity;
begin
  x := 0; y := 0; z := 0; w := 1;
end;

procedure TG2Quat.SetRotation(const Axis: TG2Vec3; const Angle: Single);
begin
  D3DXQuaternionRotationAxis(PG2QuatRef(@Self)^, Axis, Angle);
end;

procedure TG2Quat.SetRotation(const m: TG2Mat);
begin
  D3DXQuaternionRotationMatrix(PG2QuatRef(@Self)^, PG2MatRef(@m)^);
end;

procedure TG2Quat.Rotate(const Axis: TG2Vec3; const Angle: Single);
var
  q: TG2Quat;
begin
  D3DXQuaternionRotationAxis(PG2QuatRef(@q)^, PG2Vec3Ref(@Axis)^, Angle);
  D3DXQuaternionMultiply(PG2QuatRef(@Self)^, PG2QuatRef(@Self)^, PG2QuatRef(@q)^);
end;

procedure TG2Quat.Rotate(const m: TG2Mat);
var
  q: TG2Quat;
begin
  D3DXQuaternionRotationMatrix(PG2QuatRef(@q)^, PG2MatRef(@m)^);
  D3DXQuaternionMultiply(PG2QuatRef(@Self)^, PG2QuatRef(@Self)^, PG2QuatRef(@q)^);
end;

procedure TG2Quat.Normalize;
begin
  D3DXQuaternionNormalize(PG2QuatRef(@Self)^, PG2QuatRef(@Self)^);
end;

procedure TG2Quat.GetRotation(var Axis: TG2Vec3; var Angle: Single);
begin
  D3DXQuaternionToAxisAngle(PG2QuatRef(@Self)^, PG2Vec3Ref(@Axis)^, Angle);
end;

procedure TG2Quat.GetRotation(var m: TG2Mat);
begin
  D3DXMatrixRotationQuaternion(PG2MatRef(@m)^, PG2QuatRef(@Self)^);
end;

function TG2Quat.Dot(const q: TG2Quat): Single;
begin
  Result := x * q.x + y * q.y + z * q.z + w * q.w;
end;

function TG2Quat.Conjugate: TG2Quat;
begin
  D3DXQuaternionConjugate(PG2QuatRef(@Result)^, PG2QuatRef(@Self)^);
end;

function TG2Quat.Inverse: TG2Quat;
begin
  D3DXQuaternionInverse(PG2QuatRef(@Result)^, PG2QuatRef(@Self)^);
end;
//TG2Quat END

//TG2AABox BEGIN
function TG2AABox.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@MinV.x)^[Index];
end;

procedure TG2AABox.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@MinV.x)^[Index] := Value;
end;

class operator TG2AABox.Equal(const b1, b2: TG2AABox): Boolean;
begin
  Result := CompareMem(@b1, @b2, SizeOf(TG2AABox));
end;

class operator TG2AABox.NotEqual(const b1, b2: TG2AABox): Boolean;
begin
  Result := not CompareMem(@b1, @b2, SizeOf(TG2AABox));
end;

class operator TG2AABox.Add(const b1, b2: TG2AABox): TG2AABox;
begin
  if b1.MinV.x < b2.MinV.x then Result.MinV.x := b1.MinV.x else Result.MinV.x := b2.MinV.x;
  if b1.MinV.y < b2.MinV.y then Result.MinV.y := b1.MinV.y else Result.MinV.y := b2.MinV.y;
  if b1.MinV.z < b2.MinV.z then Result.MinV.z := b1.MinV.z else Result.MinV.z := b2.MinV.z;
  if b1.MaxV.x > b2.MaxV.x then Result.MaxV.x := b1.MaxV.x else Result.MaxV.x := b2.MaxV.x;
  if b1.MaxV.y > b2.MaxV.y then Result.MaxV.y := b1.MaxV.y else Result.MaxV.y := b2.MaxV.y;
  if b1.MaxV.z > b2.MaxV.z then Result.MaxV.z := b1.MaxV.z else Result.MaxV.z := b2.MaxV.z;
end;

class operator TG2AABox.Add(const b: TG2AABox; const v: TG2Vec3): TG2AABox;
begin
  if v.x < b.MinV.x then Result.MinV.x := v.x else Result.MinV.x := b.MinV.x;
  if v.y < b.MinV.y then Result.MinV.y := v.y else Result.MinV.y := b.MinV.y;
  if v.z < b.MinV.z then Result.MinV.z := v.z else Result.MinV.z := b.MinV.z;
  if v.x > b.MaxV.x then Result.MaxV.x := v.x else Result.MaxV.x := b.MaxV.x;
  if v.y > b.MaxV.y then Result.MaxV.y := v.y else Result.MaxV.y := b.MaxV.y;
  if v.z > b.MaxV.z then Result.MaxV.z := v.z else Result.MaxV.z := b.MaxV.z;
end;

procedure TG2AABox.SetValue(const _MinV, _MaxV: TG2Vec3);
begin
  MinV := _MinV; MaxV := _MaxV;
end;

function TG2AABox.Project(const W, V, P: TG2Mat; const Viewport: TD3DViewport9): TRect;
  var Points: array[0..7] of TG2Vec3;
  var i: Integer;
begin
  Points[0].SetValue(MinV.x, MinV.y, MinV.z);
  Points[1].SetValue(MaxV.x, MinV.y, MinV.z);
  Points[2].SetValue(MinV.x, MinV.y, MaxV.z);
  Points[3].SetValue(MaxV.x, MinV.y, MaxV.z);
  Points[4].SetValue(MinV.x, MaxV.y, MinV.z);
  Points[5].SetValue(MaxV.x, MaxV.y, MinV.z);
  Points[6].SetValue(MinV.x, MaxV.y, MaxV.z);
  Points[7].SetValue(MaxV.x, MaxV.y, MaxV.z);
  D3DXVec3ProjectArray(@Points, 12, @Points, 12, Viewport, P, V, W, 8);
  Result.Left := Trunc(Points[0].x);
  Result.Top := Trunc(Points[0].y);
  Result.Right := Result.Left;
  Result.Bottom := Result.Top;
  for i := 1 to 7 do
  begin
    if Points[i].x < Result.Left then Result.Left := Trunc(Points[i].x);
    if Points[i].y < Result.Top then Result.Top := Trunc(Points[i].y);
    if Points[i].x > Result.Right then Result.Right := Trunc(Points[i].x);
    if Points[i].y > Result.Bottom then Result.Bottom := Trunc(Points[i].y);
  end;
end;

function TG2AABox.Intersect(const AABox: TG2AABox): Boolean;
begin
  Result := (
    (MinV.x < AABox.MaxV.x)
    and (MinV.y < AABox.MaxV.y)
    and (MinV.z < AABox.MaxV.z)
    and (MaxV.x > AABox.MinV.x)
    and (MaxV.y > AABox.MinV.y)
    and (MaxV.z > AABox.MinV.z)
  );
end;

function TG2AABox.PointInside(const v: TG2Vec3): Boolean;
begin
  Result := (
    (v.x > MinV.x)
    and (v.y > MinV.y)
    and (v.z > MinV.z)
    and (v.x < MaxV.x)
    and (v.y < MaxV.y)
    and (v.z < MaxV.z)
  );
end;
//TG2AABox END

//TG2Box BEGIN
function TG2Box.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@C.x)^[Index];
end;

procedure TG2Box.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@C.x)^[Index] := Value;
end;

class operator TG2Box.Equal(const b1, b2: TG2Box): Boolean;
begin
  Result := CompareMem(@b1, @b2, SizeOf(TG2Box));
end;

class operator TG2Box.NotEqual(const b1, b2: TG2Box): Boolean;  
begin
  Result := not CompareMem(@b1, @b2, SizeOf(TG2Box));
end;

class operator TG2Box.Multiply(const b: TG2Box; const m: TG2Mat): TG2Box;
begin
  Result.C := b.C.Transform4x3(m);
  Result.vx := b.vx.Transform3x3(m);
  Result.vy := b.vy.Transform3x3(m);
  Result.vz := b.vz.Transform3x3(m);
end;

class operator TG2Box.Implicit(const b: TG2AABox): TG2Box;
begin
  Result.C := (b.MaxV + b.MinV) * 0.5;
  Result.vx.SetValue((b.MaxV.x - b.MinV.x) * 0.5, 0, 0);
  Result.vy.SetValue(0, (b.MaxV.y - b.MinV.y) * 0.5, 0);
  Result.vz.SetValue(0, 0, (b.MaxV.z - b.MinV.z) * 0.5);
end;

procedure TG2Box.SetValue(const _C, _vx, _vy, _vz: TG2Vec3);
begin
  C := _C; vx := _vx; vy := _vy; vz := _vz;
end;

procedure TG2Box.GetPoints(const v: PG2Vec3);
  var pv: PG2Vec3;
begin
  pv := v;
  pv^ := C + vx + vy + vz; Inc(pv);
  pv^ := C + vx + vy - vz; Inc(pv);
  pv^ := C - vx + vy - vz; Inc(pv);
  pv^ := C - vx + vy + vz; Inc(pv);
  pv^ := C + vx - vy + vz; Inc(pv);
  pv^ := C + vx - vy - vz; Inc(pv);
  pv^ := C - vx - vy - vz; Inc(pv);
  pv^ := C - vx - vy + vz;
end;

function TG2Box.AABox: TG2AABox;
var
  i: Int32;
  v: array[0..7] of TG2Vec3;
begin
  v[0] := C + vx + vy + vz;
  v[1] := C + vx + vy - vz;
  v[2] := C - vx + vy - vz;
  v[3] := C - vx + vy + vz;
  v[4] := C + vx - vy + vz;
  v[5] := C + vx - vy - vz;
  v[6] := C - vx - vy - vz;
  v[7] := C - vx - vy + vz;
  Result.MinV := v[0];
  Result.MaxV := v[0];
  for i := 1 to 7 do
  begin
    if v[i].x < Result.MinV.x then Result.MinV.x := v[i].x;
    if v[i].y < Result.MinV.y then Result.MinV.y := v[i].y;
    if v[i].z < Result.MinV.z then Result.MinV.z := v[i].z;
    if v[i].x > Result.MaxV.x then Result.MaxV.x := v[i].x;
    if v[i].y > Result.MaxV.y then Result.MaxV.y := v[i].y;
    if v[i].z > Result.MaxV.z then Result.MaxV.z := v[i].z;
  end;
end;
//TG2Box END

//TG2Sphere BEGIN
function TG2Sphere.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@C.x)^[Index];
end;

procedure TG2Sphere.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@C.x)^[Index] := Value;
end;

class operator TG2Sphere.Equal(const s1, s2: TG2Sphere): Boolean;
begin
  Result := CompareMem(@s1, @s2, SizeOf(TG2Sphere));
end;

class operator TG2Sphere.NotEqual(const s1, s2: TG2Sphere): Boolean;
begin
  Result := not CompareMem(@s1, @s2, SizeOf(TG2Sphere));
end;

procedure TG2Sphere.SetValue(const _C: TG2Vec3; const  _R: Single);
begin
  C := _C; R := _R;
end;
//TG2Sphere END

//TG2Plane BEGIN
function TG2Plane.GetArr(const Index: Integer): Single;
begin
  Result := PFLoatArray(@N.x)^[Index];
end;

procedure TG2Plane.SetArr(const Index: Integer; const Value: Single);
begin
  PFLoatArray(@N.x)^[Index] := Value;
end;

function TG2Plane.GetA: Single;
begin
  Result := N.x;
end;

procedure TG2Plane.SetA(const Value: Single);
begin
  N.x := Value;
end;

function TG2Plane.GetB: Single;
begin
  Result := N.y;
end;

procedure TG2Plane.SetB(const Value: Single);
begin
  N.y := Value;
end;

function TG2Plane.GetC: Single;
begin
  Result := N.z;
end;

procedure TG2Plane.SetC(const Value: Single);
begin
  N.z := Value;
end;

class operator TG2Plane.Explicit(const p: TG2Plane): TG2PlaneRef;
begin
  Result := PG2PlaneRef(@p)^;
end;

class operator TG2Plane.Explicit(const p: TG2PlaneRef): TG2Plane;  
begin
  Result := PG2Plane(@p)^;
end;

class operator TG2Plane.Implicit(const p: TG2Plane): TG2PlaneRef;   
begin
  Result := PG2PlaneRef(@p)^;
end;

class operator TG2Plane.Implicit(const p: TG2PlaneRef): TG2Plane;    
begin
  Result := PG2Plane(@p)^;
end;

class operator TG2Plane.Equal(const p1, p2: TG2Plane): Boolean;
begin
  Result := CompareMem(@p1, @p2, SizeOf(TG2Plane));
end;

class operator TG2Plane.NotEqual(const p1, p2: TG2Plane): Boolean;
begin
  Result := not CompareMem(@p1, @p2, SizeOf(TG2Plane));
end;

class operator TG2Plane.Multiply(const p: TG2Plane; const m: TG2Mat): TG2Plane;  
begin
  D3DXPlaneTransform(PG2PlaneRef(@Result)^, PG2PlaneRef(@p)^, PG2MatRef(@m)^);
end;

procedure TG2Plane.SetValue(const _A, _B, _C, _D: Single);
begin
  N.SetValue(_A, _B, _C);
  D := _D;
end;

procedure TG2Plane.SetPlane(const _V, _N: TG2Vec3);
begin
  D3DXPlaneFromPointNormal(PG2PlaneRef(@Self)^, PG2Vec3Ref(@_V)^, PG2Vec3Ref(@_N)^);
end;

procedure TG2Plane.SetPlane(const _v1, _v2, _v3: TG2Vec3);
begin
  D3DXPlaneFromPoints(PG2PlaneRef(@Self)^, PG2Vec3Ref(@_v1)^, PG2Vec3Ref(@_v2)^, PG2Vec3Ref(@_v3)^);
end;

procedure TG2Plane.Normalize;
begin
  D3DXPlaneNormalize(PG2PlaneRef(@Self)^, PG2PlaneRef(@Self)^);
end;

function TG2Plane.DistanceToPoint(const v: TG2Vec3): Single;
begin
  Result := N * v + D;
end;

function TG2Plane.DistanceToBox(const b: TG2Box): Single;
  var Pts: array[0..7] of TG2Vec3;
  var i: Integer;
  var d: Single;
begin
  b.GetPoints(@Pts);
  Result := DistanceToPoint(Pts[0]);
  for i := 1 to 7 do
  begin
    d := DistanceToPoint(Pts[i]);
    if d < Result then Result := d;
  end;
end;

function TG2Plane.Project(const v: TG2Vec3): TG2Vec3;
var
  Dist: Single;
begin
  Dist := DistanceToPoint(v);
  Result.SetValue(
    v.x - N.x * Dist,
		v.y - N.y * Dist,
		v.z - N.z * Dist
  );
end;

function TG2Plane.Reflect(const v: TG2Vec3): TG2Vec3;
var
  Dist: Single;
begin
  Dist := DistanceToPoint(v);
  Result := v - (N * (2 * Dist));
end;

function TG2Plane.IntersectTri(const v1, v2, v3: TG2Vec3): Boolean;
var
  b: Boolean;
begin
  b := (N * v1 > D);
  Result := not ((b = (N * v2 > D)) and (b = (N * v3 > D)));
end;

function TG2Plane.IntersectAABox(const b: TG2AABox): Boolean;
var
  MinV, MaxV: TG2Vec3;
begin
  if N.x > 0 then
  begin
    MinV.x := b.MinV.x;
    MaxV.x := b.MaxV.x;
  end
  else
  begin
    MinV.x := b.MaxV.x;
    MaxV.x := b.MinV.x;
  end;
  if N.y > 0 then
  begin
    MinV.y := b.MinV.y;
    MaxV.y := b.MaxV.y;
  end
  else
  begin
    MinV.y := b.MaxV.y;
    MaxV.y := b.MinV.y;
  end;
  if N.z > 0 then
  begin
    MinV.z := b.MinV.z;
    MaxV.z := b.MaxV.z;
  end
  else
  begin
    MinV.z := b.MaxV.z;
    MaxV.z := b.MinV.z;
  end;
  if N * MinV + D > 0 then
  begin
    Result := False;
    Exit;
  end;
  if N * MaxV + D >= 0 then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
end;

function TG2Plane.IntersectSphere(const s: TG2Sphere): Boolean;
begin
  Result := Abs(DistanceToPoint(s.C)) <= s.R;
end;
//TG2Plane END

//TG2Ray BEGIN
function TG2Ray.GetArr(const Index: Integer): Single;
begin
  Result := PFloatArray(@Origin.x)^[Index];
end;

procedure TG2Ray.SetArr(const Index: Integer; const Value: Single);
begin
  PFloatArray(@Origin.x)^[Index] := Value;
end;

class operator TG2Ray.Equal(const p1, p2: TG2Ray): Boolean;
begin
  Result := CompareMem(@p1, @p2, SizeOf(TG2Ray));
end;

class operator TG2Ray.NotEqual(const p1, p2: TG2Ray): Boolean;
begin
  Result := not CompareMem(@p1, @p2, SizeOf(TG2Ray));
end;

class operator TG2Ray.Multiply(const r: TG2Ray; m: TG2Mat): TG2Ray;
begin
  r.Origin.Transform4x3(m);
  r.Dir.Transform3x3(m);
end;

procedure TG2Ray.SetValue(const _Origin, _Dir: TG2Vec3);
begin
  Origin := _Origin;
  Dir := _Dir;
end;

procedure TG2Ray.Normalize;
begin
  Dir.Normalize;
end;

procedure TG2Ray.Transform(const m: TG2Mat);
begin
  Self.Origin.Transform4x3(m);
  Self.Dir.Transform3x3(m);
end;

procedure TG2Ray.TransformInverse(const m: TG2Mat);
begin
  Transform(m.Inverse);
end;

function TG2Ray.PointAtDistance(const Distance: Single): TG2Vec3;
begin
  Result := Origin + Dir * Distance;
end;

function TG2Ray.IntersectTri(const v1, v2, v3: TG2Vec3; var U, V, Distance: Single): Boolean;
begin
  Result := D3DXIntersectTri(
    PG2Vec3Ref(@v1)^, PG2Vec3Ref(@v2)^, PG2Vec3Ref(@v3)^,
    PG2Vec3Ref(@Origin)^,
    PG2Vec3Ref(@Dir)^,
    U, V, Distance
  );
end;

function TG2Ray.IntersectSphere(const s: TG2Sphere): Boolean;
begin
  Result := D3DXSphereBoundProbe(
    PG2Vec3Ref(@s.C)^, s.R,
    PG2Vec3Ref(@Origin)^, PG2Vec3Ref(@Dir)^
  );
end;

function TG2Ray.IntersectSphere(const s: TG2Sphere; var Pt: TG2Vec3): Boolean;
  var a, b, c, d, t0, t1: Single;
  var n: TG2Vec3;
begin
  n := Origin - s.C;
  a := Dir.LenSq;
  b := 2 * Dir.Dot(n);
  c := n.LenSq - Sqr(s.R);
  d := Sqr(b) - 4 * a * c;
  if d > 0 then
  begin
    a := 0.5 * a;
    d := Sqrt(d);
    t0 := (-b + d) * a;
    t1 := (-b - d) * a;
    if t0 < t1 then
    Pt := Origin + Dir * t0
    else
    Pt := Origin + Dir * t1;
    Result := True;
  end
  else if d = 0 then
  begin
    Pt := Origin + Dir * ((-b) * 0.5 * a);
    Result := True;
  end
  else
  Result := False;
end;

function TG2Ray.IntersectAABox(const b: TG2AABox): Boolean;
begin
  Result := D3DXBoxBoundProbe(
    PG2Vec3Ref(@b.MinV)^, PG2Vec3Ref(@b.MaxV)^,
    PG2Vec3Ref(@Origin)^, PG2Vec3Ref(@Dir)^
  );
end;

function TG2Ray.IntersectAABox(const b: TG2AABox; var Pt: TG2Vec3): Boolean;
  var d, t, t0, t1, tn, tf: Single;
  var i: Integer;
begin
  Result := False;
  tn := -1E+16;
  tf := 1E+16;
  for i := 0 to 2 do
  begin
    if Abs(Dir[i]) < G2EPS then
    begin
      if (Origin[i] < b.MinV[i])
      or (Origin[i] > b.MaxV[i]) then
      Exit;
    end;
    d := 1 / Dir[i];
    t0 := (b.MinV[i] - Origin[i]) * d;
    t1 := (b.MaxV[i] - Origin[i]) * d;
    if t0 > t1 then
    begin
      t := t1;
      t1 := t0;
      t0 := t;
    end;
    if t0 > tn then
    tn := t0;
    if t1 < tf then
    tf := t1;
    if (tn > tf) or (tf < 0) then Exit;
  end;
  if tn > 0 then
  Pt := Origin + Dir * tn
  else
  Pt := Origin + Dir * tf;
  Result := True;
end;

function TG2Ray.IntersectPlane(const Plane: TG2Plane; const BackFace: Boolean; var Distance: Single): Boolean;  
var
  vd, v0: Single;
begin
  vd := Plane.N * Dir;
  if Abs(vd) < 1E-8 then
  begin
    Result := False;
    Exit;
  end;
  if not BackFace and (vd > 0) then
  begin
    Result := False;
    Exit;
  end;
  v0 := -((Plane.N * Origin) + Plane.D);
  Distance := v0 / vd;
  if Distance < 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;
//TG2Ray END

//TG2Frustum BEGIN
function TG2Frustum.GetPlane(const Index: integer): PG2Plane;
begin
  Result := @m_Planes[Index];
end;

procedure TG2Frustum.Normalize;
var
  i: Integer;
  Rcp: Single;
begin
  for i := 0 to 5 do
  begin
    Rcp := 1 / m_Planes[i].N.Len;
    m_Planes[i].N.x := m_Planes[i].N.x * Rcp;
    m_Planes[i].N.y := m_Planes[i].N.y * Rcp;
    m_Planes[i].N.z := m_Planes[i].N.z * Rcp;
    m_Planes[i].D := m_Planes[i].D * Rcp;
  end;
end;

procedure TG2Frustum.Update;
var
  m: TG2Mat;
begin
  m := m_RefV^ * m_RefP^;
  //Left plane
  m_Planes[0].N.x := m.e03 + m.e00;
  m_Planes[0].N.y := m.e13 + m.e10;
  m_Planes[0].N.z := m.e23 + m.e20;
  m_Planes[0].D := m.e33 + m.e30;

  //Right plane
  m_Planes[1].N.x := m.e03 - m.e00;
  m_Planes[1].N.y := m.e13 - m.e10;
  m_Planes[1].N.z := m.e23 - m.e20;
  m_Planes[1].D := m.e33 - m.e30;

  //Top plane
  m_Planes[2].N.x := m.e03 - m.e01;
  m_Planes[2].N.y := m.e13 - m.e11;
  m_Planes[2].N.z := m.e23 - m.e21;
  m_Planes[2].D := m.e33 - m.e31;

  //Bottom plane
  m_Planes[3].N.x := m.e03 + m.e01;
  m_Planes[3].N.y := m.e13 + m.e11;
  m_Planes[3].N.z := m.e23 + m.e21;
  m_Planes[3].D := m.e33 + m.e31;

  //Near plane
  m_Planes[4].N.x := m.e02;
  m_Planes[4].N.y := m.e12;
  m_Planes[4].N.z := m.e22;
  m_Planes[4].D := m.e32;

  //Far plane
  m_Planes[5].N.x := m.e03 - m.e02;
  m_Planes[5].N.y := m.e13 - m.e12;
  m_Planes[5].N.z := m.e23 - m.e22;
  m_Planes[5].D := m.e33 - m.e32;

  Normalize;
end;

procedure TG2Frustum.ExtractPoints(const OutV: PG2Vec3);
  var pv: PG2Vec3;
begin
  pv := OutV;
  //0 - Left
  //1 - Right
  //2 - Top
  //3 - Bottom
  //4 - Near
  //5 - Far
  pv^ := G2Intersect3Planes(m_Planes[4], m_Planes[0], m_Planes[2]);
  Inc(pv);
  pv^ := G2Intersect3Planes(m_Planes[5], m_Planes[0], m_Planes[2]);
  Inc(pv);
  pv^ := G2Intersect3Planes(m_Planes[4], m_Planes[2], m_Planes[1]);
  Inc(pv);
  pv^ := G2Intersect3Planes(m_Planes[5], m_Planes[2], m_Planes[1]);
  Inc(pv);
  pv^ := G2Intersect3Planes(m_Planes[4], m_Planes[1], m_Planes[3]);
  Inc(pv);
  pv^ := G2Intersect3Planes(m_Planes[5], m_Planes[1], m_Planes[3]);
  Inc(pv);
  pv^ := G2Intersect3Planes(m_Planes[4], m_Planes[3], m_Planes[0]);
  Inc(pv);
  pv^ := G2Intersect3Planes(m_Planes[5], m_Planes[3], m_Planes[0]);
end;

function TG2Frustum.PointInFrustum(const V: TG2Vec3): Boolean;
var
  i: Integer;
  d: Single;
begin
  for i := 0 to 5 do
  begin
    d := DistanceToPoint(i, V);
    if d < 0 then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function TG2Frustum.BoxInFrustum(const MinV, MaxV: TG2Vec3): Boolean;
var
  fc: TG2FrustumCheck;
begin
  fc := FrustumCheckBox(MinV, MaxV);
  Result := (fc = fcInside) or (fc = fcIntersect);
end;

function TG2Frustum.BoxInFrustum(const AABox: TG2AABox): Boolean;
begin
  Result := BoxInFrustum(AABox.MinV, AABox.MaxV);
end;

function TG2Frustum.SphereInFrustum(const C: TG2Vec3; const R: Single): Boolean;
var
  fc: TG2FrustumCheck;
begin
  fc := FrustumCheckSphere(C, R);
  Result := (fc = fcInside) or (fc = fcIntersect);
end;

function TG2Frustum.FrustumCheckBox(const MinV, MaxV: TG2Vec3): TG2FrustumCheck;
var
  i: Integer;
  MaxPt, MinPt: TG2Vec3;
begin
  Result := fcInside;
  for i := 0 to 5 do
  begin
    if m_Planes[i].N.x <= 0 then
    begin
      MinPt.x := MinV.x;
      MaxPt.x := MaxV.x;
    end
    else
    begin
      MinPt.x := MaxV.x;
      MaxPt.x := MinV.x;
    end;
    if m_Planes[i].N.y <= 0 then
    begin
      MinPt.y := MinV.y;
      MaxPt.y := MaxV.y;
    end
    else
    begin
      MinPt.y := MaxV.y;
      MaxPt.y := MinV.y;
    end;
    if m_Planes[i].N.z <= 0 then
    begin
      MinPt.z := MinV.z;
      MaxPt.z := MaxV.z;
    end
    else
    begin
      MinPt.z :=MaxV.z;
      MaxPt.z :=MinV.z;
    end;
    if DistanceToPoint(i, MinPt) < 0 then
    begin
      Result := fcOutside;
      Exit;
    end;
    if DistanceToPoint(i, MaxPt) <= 0 then
    Result := fcIntersect;
  end;
end;

function TG2Frustum.FrustumCheckSphere(const C: TG2Vec3; const R: Single): TG2FrustumCheck;
var
  i: Integer;
  d: Single;
begin
  Result := fcInside;
  for i := 0 to 5 do
  begin
    d := DistanceToPoint(i, C);
    if d < -R then
    begin
      Result := fcOutside;
      Exit;
    end;
    if d < R then
    Result := fcIntersect;
  end;
end;

function TG2Frustum.DistanceToPoint(const PlaneIndex: Integer; Pt: TG2Vec3): Single;
begin
  Result := m_Planes[PlaneIndex].N.Dot(Pt) + m_Planes[PlaneIndex].D;
end;

function TG2Frustum.IntersectFrustum(const Frustum: PG2Frustum): Boolean;
  function FrustumOutside(const f1, f2: PG2Frustum): Boolean;
  var
    Points: array[0..7] of TG2Vec3;
    i, j, n: Integer;
  begin
    f2^.ExtractPoints(@Points[0]);
    for i := 0 to 5 do
    begin
      n := 0;
      for j := 0 to 7 do
      begin
        if f1^.DistanceToPoint(i, Points[j]) < 0 then
        Inc(n);
      end;
      if n >= 8 then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;
begin
  Result := (
    (not FrustumOutside(@Self, Frustum))
    and (not FrustumOutside(Frustum, @Self))
  );
end;

function TG2Frustum.Project(const W, V, P: TG2Mat; const Viewport: TD3DViewport9): TRect;
  var Points: array[0..7] of TG2Vec3;
  var i: Integer;
begin
  ExtractPoints(@Points);
  D3DXVec3ProjectArray(@Points, 12, @Points, 12, Viewport, P, V, W, 8);
  Result.Left := Trunc(Points[0].x);
  Result.Top := Trunc(Points[0].y);
  Result.Right := Result.Left;
  Result.Bottom := Result.Top;
  for i := 1 to 7 do
  begin
    if Points[i].x < Result.Left then Result.Left := Trunc(Points[i].x);
    if Points[i].y < Result.Top then Result.Top := Trunc(Points[i].y);
    if Points[i].x > Result.Right then Result.Right := Trunc(Points[i].x);
    if Points[i].y > Result.Bottom then Result.Bottom := Trunc(Points[i].y);
  end;
end;
//TG2Frustum END

//TMD5 BEGIN
procedure TG2MD5.SetValue(const Value: PByte; const Count: Integer);
begin
  Self := G2MD5(Value, Count);
end;

procedure TG2MD5.SetValue(const Value: AnsiString);
begin
  Self := G2MD5(Value);
end;

function TG2MD5.ToAnsiString: AnsiString;
  var i: Integer;
  const HexArr: array[0..15] of AnsiChar = (
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  );
begin
  SetLength(Result, 32);
  for i := 0 to 15 do
  begin
    Result[i * 2 + 1] := HexArr[(b[i] shr 4) and $0f];
    Result[i * 2 + 2] := HexArr[b[i] and $0f];
  end;
end;

class operator TG2MD5.Equal(const md5a, md5b: TG2MD5): Boolean;
  type TDWordArr4 = array[0..3] of DWord;
  var da1: TDWordArr4 absolute md5a;
  var da2: TDWordArr4 absolute md5b;
  var i: Integer;
begin
  for i := 0 to 3 do
  if da1[i] <> da2[i] then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

class operator TG2MD5.NotEqual(const md5a, md5b: TG2MD5): Boolean;
  type TDWordArr4 = array[0..3] of DWord;
  var da1: TDWordArr4 absolute md5a;
  var da2: TDWordArr4 absolute md5b;
  var i: Integer;
begin
  for i := 0 to 3 do
  if da1[i] <> da2[i] then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
end;
//TMD5 END

//Utility Functions BEGIN
function G2Vec2(const _X, _Y: Single): TG2Vec2;
begin
  Result.SetValue(_X, _Y);
end;

function G2Vec3(const _X, _Y, _Z: Single): TG2Vec3;
begin
  Result.SetValue(_X, _Y, _Z);
end;

function G2Vec3(const _V: TG2Vec2; const _Z: Single): TG2Vec3;
begin
  Result.SetValue(_V.x, _V.y, _Z);
end;

function G2Vec4(const _X, _Y, _Z, _W: Single): TG2Vec4;
begin
  Result.SetValue(_X, _Y, _Z, _W);
end;

function G2Vec4(const _V: TG2Vec2; const _Z, _W: Single): TG2Vec4;
begin
  Result.SetValue(_V.x, _V.y, _Z, _W);
end;

function G2Vec4(const _V: TG2Vec3; const _W: Single): TG2Vec4;
begin
  Result.SetValue(_V.x, _V.y, _V.z, _W);
end;

function G2Quat(const _X, _Y, _Z, _W: Single): TG2Quat;
begin
  Result.SetValue(_X, _Y, _Z, _W);
end;

function G2Ray(const _Origin, _Direction: TG2Vec3): TG2Ray;
begin
  Result.SetValue(_Origin, _Direction);
end;

function G2Plane(const V, N: TG2Vec3): TG2Plane;
begin
  Result.SetPlane(V, N);
end;

function G2LerpColor(const c1, c2: DWord; const s: Single): DWord;
var
  argb1: record b, g, r, a: Byte; end absolute c1;
  argb2: record b, g, r, a: Byte; end absolute c2;
  argb3: record b, g, r, a: Byte; end absolute Result;
begin
  argb3.b := Trunc(argb1.b + (argb2.b - argb1.b) * s);
  argb3.g := Trunc(argb1.g + (argb2.g - argb1.g) * s);
  argb3.r := Trunc(argb1.r + (argb2.r - argb1.r) * s);
  argb3.a := Trunc(argb1.a + (argb2.a - argb1.a) * s);
end;

function G2LerpFloat(const f1, f2, s: Single): Single;
begin
  Result := f1 + (f2 - f1) * s;
end;

function G2LerpVec2(const v1, v2: TG2Vec2; const s: Single): TG2Vec2;  
begin
  Result := v1 + (v2 - v1) * s;
end;

function G2LerpVec3(const v1, v2: TG2Vec3; const s: Single): TG2Vec3;   
begin
  Result := v1 + (v2 - v1) * s;
end;

function G2LerpVec4(const v1, v2: TG2Vec4; const s: Single): TG2Vec4;   
begin
  Result := v1 + (v2 - v1) * s;
end;

function G2LerpQuat(const q1, q2: TG2Quat; const s: Single): TG2Quat;
begin
  Result.x := q1.x + (q2.x - q1.x) * s;
  Result.y := q1.y + (q2.y - q1.y) * s;
  Result.z := q1.z + (q2.z - q1.z) * s;
  Result.w := q1.w + (q2.w - q1.w) * s;
end;

function G2NLerpQuat(const q1, q2: TG2Quat; const s: Single): TG2Quat;
begin
  Result.x := q1.x + (q2.x - q1.x) * s;
  Result.y := q1.y + (q2.y - q1.y) * s;
  Result.z := q1.z + (q2.z - q1.z) * s;
  Result.w := q1.w + (q2.w - q1.w) * s;
  Result.Normalize;
end;

function G2SLerpQuat(const q1, q2: TG2Quat; const s: Single): TG2Quat;
  var SinTh, CosTh, Th, ra, rb: Single;
  var qa, qb: TG2Quat;
begin
  qa := q1;
  qb := q2;
  CosTh := qa.x * qb.x + qa.y * qb.y + qa.z * qb.z + qa.w * qb.w;
  if CosTh < 0 then
  begin
    qb.x := -qb.x; qb.y := -qb.y; qb.z := -qb.z;
    CosTh := -CosTh;
  end;
	if Abs(CosTh) >= 1.0 then
  begin
		Result := qa;
		Exit;
	end;
	Th := ArcCos(CosTh);
	SinTh := Sin(Th);
	if Abs(SinTh) < 1E-4 then
  begin
		ra := 1 - s;
    rb := s;
	end
  else
  begin
    ra := Sin((1 - s) * Th) / SinTh;
    rb := Sin(s * Th) / SinTh;
  end;
	Result.x := qa.x * ra + qb.x * rb;
  Result.y := qa.y * ra + qb.y * rb;
  Result.z := qa.z * ra + qb.z * rb;
  Result.w := qa.w * ra + qb.w * rb;
end;

function G2CosrpFloat(const f1, f2: Single; const s: Single): Single;
begin
  Result := f1 + (f2 - f1) * (1.0 - Cos(s * Pi)) * 0.5;
end;

function G2Vec2InRect(const v, MinV, MaxV: TG2Vec2): Boolean;
begin
  Result := (
    (v.x >= MinV.x)
    and (v.x <= MaxV.x)
    and (v.y >= MinV.y)
    and (v.y <= MaxV.y)
  );
end;

function G2Vec2InPoly(const v: TG2Vec2; const VArr: PG2Vec2Array; const VCount: Integer): Boolean;
var
  i: Integer; 
  pi, pj: PG2Vec2; 
begin 
  Result := False; 
  if VCount < 3 then Exit;
  pj := @VArr^[VCount - 1];
  for i := 0 to VCount - 1 do
  begin 
    pi := @VArr^[i];
    if (
      ((pi^.y <= v.y) and (v.y < pj^.y))
      or
      ((pj^.y <= v.y) and (v.y < pi^.y))
    )
    and (
      v.x < (pj^.x - pi^.x) * (v.y - pi^.y) / (pj^.y - pi^.y) + pi^.x
    ) then
    Result := not Result; 
    pj := pi; 
  end; 
end;

function G2Vec2ToLine(const l1, l2, v: TG2Vec2; var VecOnLine: TG2Vec2; var InSegment: boolean): Single;
var
  u: single;
begin
  u := ((v.x - l1.x) * (l2.x - l1.x) + (v.y - l1.y) * (l2.y - l1.y)) / (Sqr(l2.x - l1.x) + Sqr(l2.y - l1.y));
  VecOnLine.SetValue(l1.x + u * (l2.x - l1.x), l1.y + u * (l2.y - l1.y));
  InSegment := (u >= 0) and (u <= 1);
  Result := (v - VecOnLine).Len;
end;

function G2Vec3ToLine(const l1, l2, v: TG2Vec3; var VecOnLine: TG2Vec3; var InSegment: boolean): Single;
var
  u: single;
begin
  u := (
    ((v.x - l1.x) * (l2.x - l1.x) + (v.y - l1.y) * (l2.y - l1.y) + (v.z - l1.z) * (l2.z - l1.z)) /
    (Sqr(l2.x - l1.x) + Sqr(l2.y - l1.y) + Sqr(l2.z - l1.z))
  );
  VecOnLine.SetValue(l1.x + u * (l2.x - l1.x), l1.y + u * (l2.y - l1.y), l1.z + u * (l2.z - l1.z));
  InSegment := (u >= 0) and (u <= 1);
  Result := (v - VecOnLine).Len;
end;

function G2RectInRect(const MinV1, MaxV1, MinV2, MaxV2: TG2Vec2): Boolean;
begin
  Result := (
    (MaxV1.x >= MinV2.x)
    and (MaxV1.y >= MinV2.y)
    and (MinV1.x <= MaxV2.x)
    and (MinV1.y <= MaxV2.y)
  );
end;

function G2Vec2Reflect(const v, n: TG2Vec2): TG2Vec2;
var
  Dot: Single;
begin
  Dot := v * n;
  Result := G2Vec2(
    (v.x - 2 * n.x * Dot),
    (v.y - 2 * n.y * Dot)
  );
end;

function G2Vec2AngleOX(const v1, v2: TG2Vec2): Single;
begin
  Result := ArcTan2(v2.y - v1.y, v2.x - v1.x);
end;

function G2Vec2AngleOY(const v1, v2: TG2Vec2): Single;
begin
  Result := ArcTan2(v2.x - v1.x, v2.y - v1.y);
end;

function G2Intersect3Planes(const p1, p2, p3: TG2Plane): TG2Vec3;
  var iDet: Single;
begin
  iDet := -p1.N.Dot(p2.N.Cross(p3.N));
  if Abs(iDet) > 1E-5 then
  begin
    iDet := 1 / iDet;
    Result := ((p2.N.Cross(p3.N) * p1.D) + (p3.N.Cross(p1.N) * p2.D) + (p1.N.Cross(p2.N) * p3.D)) * iDet;
  end;
end;

function G2Vec3Rotation(const SrcV, DstV: TG2Vec3): TG2Quat;
var
  Angle: Single;
  Axis: TG2Vec3;
begin
  Angle := SrcV.Angle(DstV);
  Axis := SrcV.Cross(DstV);
  if Axis.LenSq < 1E-5 then
  Axis.SetValue(0, 1, 0);
  Result.SetRotation(Axis, Angle);
end;

function G2TriangleNormal(const v1, v2, v3: TG2Vec3): TG2Vec3;
begin
  Result := (v2 - v1).Cross(v3 - v1);
  Result.Normalize;
end;

procedure G2FaceTBN(
      const v1, v2, v3: TG2Vec3;
      const uv1, uv2, uv3: TG2Vec2;
      var T, B, N: TG2Vec3
    );
var
  FaceNormal, Side1, Side2, cp: TG2Vec3;
begin
  FaceNormal := G2TriangleNormal(v1, v2, v3);
  Side1.SetValue(v2.x - v1.x, uv2.x - uv1.x, uv2.y - uv1.y);
  Side2.SetValue(v3.x - v1.x, uv3.x - uv1.x, uv3.y - uv1.y);
  cp := Side1.Cross(Side2);
  T.x := -cp.y / cp.x; B.x := -cp.z / cp.x;
  Side1.x := v2.y - v1.y; Side2.x := v3.y - v1.y;
  cp := Side1.Cross(Side2);
  T.y := -cp.y / cp.x; B.y := -cp.z / cp.x;
  Side1.x := v2.z - v1.z; Side2.x := v3.z - v1.z;
  cp := Side1.Cross(Side2);
  T.z := -cp.y / cp.x; B.z := -cp.z / cp.x;
  T.Normalize; B.Normalize;
  N := T.Cross(B); N.Normalize;
  if N.Dot(FaceNormal) < 0 then N := -N;
end;

function G2MatLerp(const Mat1, Mat2: TG2Mat; const s: Single): TG2Mat;
var
  i, j: Integer;
  mr: TD3DXMatrix absolute Result;
  m1: TD3DXMatrix absolute Mat1;
  m2: TD3DXMatrix absolute Mat2;
begin
  for i := 0 to 3 do
  for j := 0 to 3 do
  mr.m[i, j] := G2LerpFloat(m1.m[i, j], m2.m[i, j], s);
end;

function G2MatSLerp(const Mat1, Mat2: TG2Mat; const s: Single): TG2Mat;
var
  Scale, Scale1, Scale2: TD3DXVector3;
  Rotation, Rotation1, Rotation2: TD3DXQuaternion;
  Translation, Translation1, Translation2: TD3DXVector3;
  MatRotation, MatScaling, MatTranslation: TG2Mat;
begin
  Mat1.Decompose(@Scale1, @Rotation1, @Translation1);
  Mat2.Decompose(@Scale2, @Rotation2, @Translation2);
  D3DXVec3Lerp(Scale, Scale1, Scale2, s);
  D3DXQuaternionNormalize(Rotation1, Rotation1);
  D3DXQuaternionNormalize(Rotation2, Rotation2);
  D3DXQuaternionSlerp(Rotation, Rotation1, Rotation2, s);
  D3DXVec3Lerp(Translation, Translation1, Translation2, s);
  MatRotation.SetRotation(Rotation);
  MatScaling.SetScaling(Scale.x, Scale.y, Scale.z);
  MatTranslation.SetTranslation(Translation);
  Result := MatScaling * MatRotation * MatTranslation;
end;

procedure G2SinCos(const Angle: Single; var s, c: Single);
asm
  fld Angle
  fsincos
  fstp [edx]
  fstp [eax]
  fwait
end;

function G2MD5(const Value: PByte; const Count: DWord): TG2MD5;
  const S11 = 7;
  const S12 = 12;
  const S13 = 17;
  const S14 = 22;

  const S21 = 5;
  const S22 = 9;
  const S23 = 14;
  const S24 = 20;

  const S31 = 4;
  const S32 = 11;
  const S33 = 16;
  const S34 = 23;

  const S41 = 6;
  const S42 = 10;
  const S43 = 15;
  const S44 = 21;
  var ContextCount: array[0..1] of DWord;
  var ContextState: array[0..4] of DWord;
  var ContextBuffer: array[0..63] of Byte;
  var Padding: array[0..63] of Byte;
  procedure Encode(const Dst: PByteArray; const Src: PDWordArray; const Count: Integer);
    var i, j: Integer;
  begin
    i := 0;
    j := 0;
    while (j < Count) do
    begin
      Dst^[j] := Src^[i] and $ff;
      Dst^[j + 1] := (Src^[i] shr 8)  and $ff;
      Dst^[j + 2] := (Src^[i] shr 16) and $ff;
      Dst^[j + 3] := (Src^[i] shr 24) and $ff;
      Inc(j, 4);
      Inc(i);
    end;
  end;
  procedure Decode(const Dst: PDWordArray; const Src: PByteArray; const Count, Shift: Integer);
    var i, j: Integer;
  begin
    j := 0;
    i := 0;
    while (j < Count) do
    begin
      Dst^[i] := (
        (Src^[j + Shift] and $ff) or
        ((Src^[j + Shift + 1] and $ff) shl 8)  or
        ((Src^[j + Shift + 2] and $ff) shl 16) or
        ((Src^[j + Shift + 3] and $ff) shl 24)
      );
      Inc(j, 4);
      Inc(i);
    end;
  end;
  procedure Transform(const Block: PByteArray; const Shift: Integer);
    function F(const x, y, z: DWord): DWord;
    begin
      Result := (x and y) or ((not x) and z);
    end;
    function G(const x, y, z: DWord): DWord;
    begin
      Result := (x and z) or (y and (not z));
    end;
    function H(const x, y, z: DWord): DWord;
    begin
      Result := x xor y xor z;
    end;
    function I(const x, y, z: DWord): DWord;
    begin
      Result := y xor (x or (not z));
    end;
    procedure RL(var x: Longword; const n: Byte);
    begin
      x := (x shl n) or (x shr (32 - n));
    end;
    procedure FF(var a: Longword; const b, c, d, x: DWord; const s: Byte; const ac: DWord);
    begin
      Inc(a, F(b, c, d) + x + ac);
      RL(a, s);
      Inc(a, b);
    end;
    procedure GG(var a: Longword; const b, c, d, x: DWord; const s: Byte; const ac: DWord);
    begin
      Inc(a, G(b, c, d) + x + ac);
      RL(a, s);
      Inc(a, b);
    end;
    procedure HH(var a: Longword; const b, c, d, x: DWord; const s: Byte; const ac: DWord);
    begin
      Inc(a, H(b, c, d) + x + ac);
      RL(a, s);
      Inc(a, b);
    end;
    procedure II(var a: Longword; const b, c, d, x: DWord; const s: Byte; const ac: DWord);
    begin
      Inc(a, I(b, c, d) + x + ac);
      RL(a, s);
      Inc(a, b);
    end;
    var a, b, c, d: DWord;
    var x: array[0..15] of DWord;
  begin
    a := ContextState[0];
    b := ContextState[1];
    c := ContextState[2];
    d := ContextState[3];
    Decode(@x[0], Block, 64, Shift);
    //Round 1
    FF( a, b, c, d, x[ 0], S11, $d76aa478); { 1 }
    FF( d, a, b, c, x[ 1], S12, $e8c7b756); { 2 }
    FF( c, d, a, b, x[ 2], S13, $242070db); { 3 }
    FF( b, c, d, a, x[ 3], S14, $c1bdceee); { 4 }
    FF( a, b, c, d, x[ 4], S11, $f57c0faf); { 5 }
    FF( d, a, b, c, x[ 5], S12, $4787c62a); { 6 }
    FF( c, d, a, b, x[ 6], S13, $a8304613); { 7 }
    FF( b, c, d, a, x[ 7], S14, $fd469501); { 8 }
    FF( a, b, c, d, x[ 8], S11, $698098d8); { 9 }
    FF( d, a, b, c, x[ 9], S12, $8b44f7af); { 10 }
    FF( c, d, a, b, x[10], S13, $ffff5bb1); { 11 }
    FF( b, c, d, a, x[11], S14, $895cd7be); { 12 }
    FF( a, b, c, d, x[12], S11, $6b901122); { 13 }
    FF( d, a, b, c, x[13], S12, $fd987193); { 14 }
    FF( c, d, a, b, x[14], S13, $a679438e); { 15 }
    FF( b, c, d, a, x[15], S14, $49b40821); { 16 }
    //Round 2
    GG( a, b, c, d, x[ 1], S21, $f61e2562); { 17 }
    GG( d, a, b, c, x[ 6], S22, $c040b340); { 18 }
    GG( c, d, a, b, x[11], S23, $265e5a51); { 19 }
    GG( b, c, d, a, x[ 0], S24, $e9b6c7aa); { 20 }
    GG( a, b, c, d, x[ 5], S21, $d62f105d); { 21 }
    GG( d, a, b, c, x[10], S22,  $2441453); { 22 }
    GG( c, d, a, b, x[15], S23, $d8a1e681); { 23 }
    GG( b, c, d, a, x[ 4], S24, $e7d3fbc8); { 24 }
    GG( a, b, c, d, x[ 9], S21, $21e1cde6); { 25 }
    GG( d, a, b, c, x[14], S22, $c33707d6); { 26 }
    GG( c, d, a, b, x[ 3], S23, $f4d50d87); { 27 }
    GG( b, c, d, a, x[ 8], S24, $455a14ed); { 28 }
    GG( a, b, c, d, x[13], S21, $a9e3e905); { 29 }
    GG( d, a, b, c, x[ 2], S22, $fcefa3f8); { 30 }
    GG( c, d, a, b, x[ 7], S23, $676f02d9); { 31 }
    GG( b, c, d, a, x[12], S24, $8d2a4c8a); { 32 }
    //Round 3
    HH( a, b, c, d, x[ 5], S31, $fffa3942); { 33 }
    HH( d, a, b, c, x[ 8], S32, $8771f681); { 34 }
    HH( c, d, a, b, x[11], S33, $6d9d6122); { 35 }
    HH( b, c, d, a, x[14], S34, $fde5380c); { 36 }
    HH( a, b, c, d, x[ 1], S31, $a4beea44); { 37 }
    HH( d, a, b, c, x[ 4], S32, $4bdecfa9); { 38 }
    HH( c, d, a, b, x[ 7], S33, $f6bb4b60); { 39 }
    HH( b, c, d, a, x[10], S34, $bebfbc70); { 40 }
    HH( a, b, c, d, x[13], S31, $289b7ec6); { 41 }
    HH( d, a, b, c, x[ 0], S32, $eaa127fa); { 42 }
    HH( c, d, a, b, x[ 3], S33, $d4ef3085); { 43 }
    HH( b, c, d, a, x[ 6], S34,  $4881d05); { 44 }
    HH( a, b, c, d, x[ 9], S31, $d9d4d039); { 45 }
    HH( d, a, b, c, x[12], S32, $e6db99e5); { 46 }
    HH( c, d, a, b, x[15], S33, $1fa27cf8); { 47 }
    HH( b, c, d, a, x[ 2], S34, $c4ac5665); { 48 }
    //Round 4
    II( a, b, c, d, x[ 0], S41, $f4292244); { 49 }
    II( d, a, b, c, x[ 7], S42, $432aff97); { 50 }
    II( c, d, a, b, x[14], S43, $ab9423a7); { 51 }
    II( b, c, d, a, x[ 5], S44, $fc93a039); { 52 }
    II( a, b, c, d, x[12], S41, $655b59c3); { 53 }
    II( d, a, b, c, x[ 3], S42, $8f0ccc92); { 54 }
    II( c, d, a, b, x[10], S43, $ffeff47d); { 55 }
    II( b, c, d, a, x[ 1], S44, $85845dd1); { 56 }
    II( a, b, c, d, x[ 8], S41, $6fa87e4f); { 57 }
    II( d, a, b, c, x[15], S42, $fe2ce6e0); { 58 }
    II( c, d, a, b, x[ 6], S43, $a3014314); { 59 }
    II( b, c, d, a, x[13], S44, $4e0811a1); { 60 }
    II( a, b, c, d, x[ 4], S41, $f7537e82); { 61 }
    II( d, a, b, c, x[11], S42, $bd3af235); { 62 }
    II( c, d, a, b, x[ 2], S43, $2ad7d2bb); { 63 }
    II( b, c, d, a, x[ 9], S44, $eb86d391); { 64 }

    Inc(ContextState[0], a);
    Inc(ContextState[1], b);
    Inc(ContextState[2], c);
    Inc(ContextState[3], d);
  end;
  procedure Update(const Value: PByte; const Count: DWord);
    var i, Index, PartLen, Start: DWord;
    var pb: PByteArray;
  begin
    pb := PByteArray(Value);
    Index := (ContextCount[0] shr 3) and $3f;
    Inc(ContextCount[0], Count shl 3);
    if ContextCount[0] < (Count shl 3) then
    Inc(ContextCount[1]);
    Inc(ContextCount[1], Count shr 29);
    PartLen := 64 - Index;
    if Count >= PartLen then
    begin
      for i := 0 to PartLen - 1 do
      ContextBuffer[i + Index] := pb^[i];

      Transform(@ContextBuffer, 0);
      i := PartLen;
      while (i + 63) < Count do
      begin
        Transform(pb, i);
        Inc(i, 64);
      end;
      Index := 0;
    end
    else
    i := 0;
    if (i < Count) then
    begin
      Start := i;
      while (i < Count) do
      begin
        ContextBuffer[Index + i - Start] := pb^[i];
        Inc(I);
      end;
    end;
  end;
  var Bits: array[0..7] of Byte;
  var Index, PadLen: Integer;
begin
  FillChar(Padding, 64, 0);
  Padding[0] := $80;
  ContextCount[0] := 0;
  ContextCount[1] := 0;
  ContextState[0] := $67452301;
  ContextState[1] := $efcdab89;
  ContextState[2] := $98badcfe;
  ContextState[3] := $10325476;
  Update(Value, Count);
  Encode(@Bits, @ContextCount, 8);
  Index := (ContextCount[0] shr 3) and $3f;
  if Index < 56 then
  PadLen := 56 - Index
  else
  PadLen := 120 - Index;
  Update(@Padding, PadLen);
  Update(@bits, 8);
  Encode(@Result, @ContextState, 16);
end;

function G2MD5(const Value: AnsiString): TG2MD5;
begin
  Result := G2MD5(@Value[1], Length(Value));
end;

function G2CRC16(const Value: PByte; const Count: Integer): Word;
  const CRC16Table: array[0..255] of Word = (
    $0000, $c0c1, $c181, $0140, $c301, $03c0, $0280, $c241, $c601, $06c0, $0780,
    $c741, $0500, $c5c1, $c481, $0440, $cc01, $0cc0, $0d80, $cd41, $0f00, $cfc1,
    $ce81, $0e40, $0a00, $cac1, $cb81, $0b40, $c901, $09c0, $0880, $c841, $d801,
    $18c0, $1980, $d941, $1b00, $dbc1, $da81, $1a40, $1e00, $dec1, $df81, $1f40,
    $dd01, $1dc0, $1c80, $dc41, $1400, $d4c1, $d581, $1540, $d701, $17c0, $1680,
    $d641, $d201, $12c0, $1380, $d341, $1100, $d1c1, $d081, $1040, $f001, $30c0,
    $3180, $f141, $3300, $f3c1, $f281, $3240, $3600, $f6c1, $f781, $3740, $f501,
    $35c0, $3480, $f441, $3c00, $fcc1, $fd81, $3d40, $ff01, $3fc0, $3e80, $fe41,
    $fa01, $3ac0, $3b80, $fb41, $3900, $f9c1, $f881, $3840, $2800, $e8c1, $e981,
    $2940, $eb01, $2bc0, $2a80, $ea41, $ee01, $2ec0, $2f80, $ef41, $2d00, $edc1,
    $ec81, $2c40, $e401, $24c0, $2580, $e541, $2700, $e7c1, $e681, $2640, $2200,
    $e2c1, $e381, $2340, $e101, $21c0, $2080, $e041, $a001, $60c0, $6180, $a141,
    $6300, $a3c1, $a281, $6240, $6600, $a6c1, $a781, $6740, $a501, $65c0, $6480,
    $a441, $6c00, $acc1, $ad81, $6d40, $af01, $6fc0, $6e80, $ae41, $aa01, $6ac0,
    $6b80, $ab41, $6900, $a9c1, $a881, $6840, $7800, $b8c1, $b981, $7940, $bb01,
    $7bc0, $7a80, $ba41, $be01, $7ec0, $7f80, $bf41, $7d00, $bdc1, $bc81, $7c40,
    $b401, $74c0, $7580, $b541, $7700, $b7c1, $b681, $7640, $7200, $b2c1, $b381,
    $7340, $b101, $71c0, $7080, $b041, $5000, $90c1, $9181, $5140, $9301, $53c0,
    $5280, $9241, $9601, $56c0, $5780, $9741, $5500, $95c1, $9481, $5440, $9c01,
    $5cc0, $5d80, $9d41, $5f00, $9fc1, $9e81, $5e40, $5a00, $9ac1, $9b81, $5b40,
    $9901, $59c0, $5880, $9841, $8801, $48c0, $4980, $8941, $4b00, $8bc1, $8a81,
    $4a40, $4e00, $8ec1, $8f81, $4f40, $8d01, $4dc0, $4c80, $8c41, $4400, $84c1,
    $8581, $4540, $8701, $47c0, $4680, $8641, $8201, $42c0, $4380, $8341, $4100,
    $81c1, $8081, $4040
  );
  var i: Integer;
  var pb: PByteArray absolute Value;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  Result := (Result shr 8) xor CRC16Table[pb^[i] xor (Result and $ff)];
end;

function G2CRC32(const Value: PByte; const Count: Integer): DWord;
  const CRC32Table: array[0..255] of DWord = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535,
    $9e6495a3, $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd,
    $e7b82d07, $90bf1d91, $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d,
    $6ddde4eb, $f4d4b551, $83d385c7, $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
    $14015c4f, $63066cd9, $fa0f3d63, $8d080df5, $3b6e20c8, $4c69105e, $d56041e4,
    $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b, $35b5a8fa, $42b2986c,
    $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59, $26d930ac,
    $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab,
    $b6662d3d, $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f,
    $9fbfe4a5, $e8b8d433, $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb,
    $086d3d2d, $91646c97, $e6635c01, $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
    $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457, $65b0d9c6, $12b7e950, $8bbeb8ea,
    $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65, $4db26158, $3ab551ce,
    $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a,
    $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409,
    $ce61e49f, $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81,
    $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739,
    $9dd277af, $04db2615, $73dc1683, $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
    $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1, $f00f9344, $8708a3d2, $1e01f268,
    $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7, $fed41b76, $89d32be0,
    $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5, $d6d6a3e8,
    $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
    $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703,
    $220216b9, $5505262f, $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7,
    $b5d0cf31, $2cd99e8b, $5bdeae1d, $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
    $9c0906a9, $eb0e363f, $72076785, $05005713, $95bf4a82, $e2b87a14, $7bb12bae,
    $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242,
    $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777, $88085ae6,
    $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d,
    $3e6e77db, $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5,
    $47b2cf7f, $30b5ffe9, $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
    $cdd70693, $54de5729, $23d967bf, $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
    $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
  );
  var i: Integer;
  var pb: PByteArray absolute Value;
begin
  Result := $ffffffff;
  for i := 0 to Count - 1 do
  Result := ((Result shr 8) and $00ffffff) xor CRC32Table[(Result xor pb^[i]) and $ff];
  Result := Result xor $ffffffff;
end;

function G2PolyTriangulate(const Triang: PG2PolyTriang): Boolean;
  function InsideTriangle(const Ax, Ay, Bx, By, Cx, Cy, Px, Py: Single): Boolean;
  begin
    Result := (
      ((Cx - Bx) * (Py - By) - (Cy - By) * (Px - Bx) >= 0)
      and ((Bx - Ax) * (Py - Ay) - (By - Ay) * (Px - Ax) >= 0)
      and ((Ax - Cx) * (Py - Cy) - (Ay - Cy) * (Px - Cx) >= 0)
    );
  end;
  function Area: Single;
    var i, j: Integer;
  begin
    Result := 0;
    i := High(Triang.v);
    for j := 0 to High(Triang.v) do
    begin
      Result := Result + Triang.v[i].x * Triang.v[j].y - Triang.v[j].x * Triang.v[i].y;
      i := j;
    end;
    Result := Result * 0.5;
  end;
  function Snip(const u, v, w, n: Integer; const Ind: PIntegerArray): Boolean;
    var i: Integer;
    var Ax, Ay, Bx, By, Cx, Cy, Px, Py: Single;
  begin
    Ax := Triang.v[Ind^[u]].x;
    Ay := Triang.v[Ind^[u]].y;
    Bx := Triang.v[Ind^[v]].x;
    By := Triang.v[Ind^[v]].y;
    Cx := Triang.v[Ind^[w]].x;
    Cy := Triang.v[Ind^[w]].y;
    if 1E-5 > (((Bx - Ax) * (Cy - Ay)) - ((By - Ay) * (Cx - Ax))) then
    begin
      Result := False;
      Exit;
    end;
    for i := 0 to n - 1 do
    begin
      if (i = u) or (i = v) or (i = w) then Continue;
      Px := Triang.v[Ind^[i]].x;
      Py := Triang.v[Ind^[i]].y;
      if InsideTriangle(Ax, Ay, Bx, By, Cx, Cy, Px, Py) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
  var u, v, w, i, n, nv, c: Integer;
  var s, t: Integer;
  var Ind: array of Integer;
begin
  n := Length(Triang.v);
  if n < 3 then
  begin
    Result := False;
    Exit;
  end;
  SetLength(Triang.Triangles, n);
  SetLength(Ind, n);
  if Area > 0 then
  for i := 0 to n - 1 do Ind[i] := i
  else
  for i := 0 to n - 1 do Ind[i] := n - 1 - i;
  nv := n;
  c := nv * 2;
  i := 0;
  v := nv - 1;
  t := 0;
  while nv > 2 do
  begin
    if c - 1 <= 0 then
    begin
      SetLength(Triang.Triangles, 0);
      Result := False;
      Exit;
    end;
    u := v; if nv <= u then u := 0;
    v := u + 1; if nv <= v then v := 0;
    w := v + 1; if nv <= w then w := 0;
    if Snip(u, v, w, nv, @Ind[0]) then
    begin
      Triang.Triangles[t][0] := Ind[u];
      Triang.Triangles[t][1] := Ind[v];
      Triang.Triangles[t][2] := Ind[w];
      Inc(t);
      for s := v to nv - 2 do
      Ind[s] := Ind[s + 1];
      Dec(nv);
      c := nv * 2;
      i := 0;
    end
    else
    begin
      Inc(i);
      if (i > nv + 1) then
      begin
        SetLength(Triang.Triangles, 0);
        Result := False;
        Exit;
      end;
      v := v + 1; if nv <= v then v := 0;
    end;
  end;
  SetLength(Triang.Triangles, t);
  Result := True;
end;

procedure G2MatAddSSE(const OutM, InM1, InM2: PG2Mat);
asm
  movups xmm0, [edx]
  movups xmm1, [ecx]
  addps xmm0, xmm1
  movlps [eax], xmm0
  movhps [eax + 8], xmm0
  movups xmm0, [edx + 10h]
  movups xmm1, [ecx + 10h]
  addps xmm0, xmm1
  movlps [eax + 10h], xmm0
  movhps [eax + 18h], xmm0
  movups xmm0, [edx + 20h]
  movups xmm1, [ecx + 20h]
  addps xmm0, xmm1
  movlps [eax + 20h], xmm0
  movhps [eax + 28h], xmm0
  movups xmm0, [edx + 30h]
  movups xmm1, [ecx + 30h]
  addps xmm0, xmm1
  movlps [eax + 30h], xmm0
  movhps [eax + 38h], xmm0
end;

procedure G2MatAddStd(const OutM, InM1, InM2: PG2Mat);
begin
  with OutM^ do
  begin
    e00 := InM1^.e00 + InM2^.e00; e10 := InM1^.e10 + InM2^.e10; e20 := InM1^.e20 + InM2^.e20; e30 := InM1^.e30 + InM2^.e30;
    e01 := InM1^.e01 + InM2^.e01; e11 := InM1^.e11 + InM2^.e11; e21 := InM1^.e21 + InM2^.e21; e31 := InM1^.e31 + InM2^.e31;
    e02 := InM1^.e02 + InM2^.e02; e12 := InM1^.e12 + InM2^.e12; e22 := InM1^.e22 + InM2^.e22; e32 := InM1^.e32 + InM2^.e32;
    e03 := InM1^.e03 + InM2^.e03; e13 := InM1^.e13 + InM2^.e13; e23 := InM1^.e23 + InM2^.e23; e33 := InM1^.e33 + InM2^.e33;
  end;
end;

procedure G2MatSubSSE(const OutM, InM1, InM2: PG2Mat);
asm
  movups xmm0, [edx]
  movups xmm1, [ecx]
  subps xmm0, xmm1
  movlps [eax], xmm0
  movhps [eax + 8h], xmm0
  movups xmm0, [edx + 10h]
  movups xmm1, [ecx + 10h]
  subps xmm0, xmm1
  movlps [eax + 10h], xmm0
  movhps [eax + 18h], xmm0
  movups xmm0, [edx + 20h]
  movups xmm1, [ecx + 20h]
  subps xmm0, xmm1
  movlps [eax + 20h], xmm0
  movhps [eax + 28h], xmm0
  movups xmm0, [edx + 30h]
  movups xmm1, [ecx + 30h]
  subps xmm0, xmm1
  movlps [eax + 30h], xmm0
  movhps [eax + 38h], xmm0
end;

procedure G2MatSubStd(const OutM, InM1, InM2: PG2Mat);
begin
  with OutM^ do
  begin
    e00 := InM1^.e00 - InM2^.e00; e10 := InM1^.e10 - InM2^.e10; e20 := InM1^.e20 - InM2^.e20; e30 := InM1^.e30 - InM2^.e30;
    e01 := InM1^.e01 - InM2^.e01; e11 := InM1^.e11 - InM2^.e11; e21 := InM1^.e21 - InM2^.e21; e31 := InM1^.e31 - InM2^.e31;
    e02 := InM1^.e02 - InM2^.e02; e12 := InM1^.e12 - InM2^.e12; e22 := InM1^.e22 - InM2^.e22; e32 := InM1^.e32 - InM2^.e32;
    e03 := InM1^.e03 - InM2^.e03; e13 := InM1^.e13 - InM2^.e13; e23 := InM1^.e23 - InM2^.e23; e33 := InM1^.e33 - InM2^.e33;
  end;
end;

procedure G2MatFltMulSSE(const OutM, InM1: PG2Mat; const s: Single);
asm
  movss xmm0, [s]
  shufps xmm0, xmm0, 0
  movups xmm1, [edx]
  mulps xmm1, xmm0
  movlps [eax], xmm1
  movhps [eax + 8], xmm1
  movups xmm1, [edx + 10h]
  mulps xmm1, xmm0
  movlps [eax + 10h], xmm1
  movhps [eax + 18h], xmm1
  movups xmm1, [edx + 20h]
  mulps xmm1, xmm0
  movlps [eax + 20h], xmm1
  movhps [eax + 28h], xmm1
  movups xmm1, [edx + 30h]
  mulps xmm1, xmm0
  movlps [eax + 30h], xmm1
  movhps [eax + 38h], xmm1
end;

procedure G2MatFltMulStd(const OutM, InM: PG2Mat; const s: Single);
begin
  OutM^.e00 := InM^.e00 * s;
  OutM^.e10 := InM^.e10 * s;
  OutM^.e20 := InM^.e20 * s;
  OutM^.e30 := InM^.e30 * s;
  OutM^.e01 := InM^.e01 * s;
  OutM^.e11 := InM^.e11 * s;
  OutM^.e21 := InM^.e21 * s;
  OutM^.e31 := InM^.e31 * s;
  OutM^.e02 := InM^.e02 * s;
  OutM^.e12 := InM^.e12 * s;
  OutM^.e22 := InM^.e22 * s;
  OutM^.e32 := InM^.e32 * s;
  OutM^.e03 := InM^.e03 * s;
  OutM^.e13 := InM^.e13 * s;
  OutM^.e23 := InM^.e23 * s;
  OutM^.e33 := InM^.e33 * s;
end;

procedure G2MatMulSSE(const OutM, InM1, InM2: PG2Mat);
asm
  movss xmm0, [edx]
  movups xmm4, [ecx]
  shufps xmm0, xmm0, 0
  mulps xmm0, xmm4
  movss xmm1, [edx + 4]
  movups xmm5, [ecx + 10h]
  shufps xmm1, xmm1, 0
  mulps xmm1, xmm5
  addps xmm0, xmm1
  movss xmm1, [edx + 8]
  movups xmm6, [ecx + 20h]
  shufps xmm1, xmm1, 0
  mulps xmm1, xmm6
  addps xmm0, xmm1
  movss xmm1, [edx + 0Ch]
  movups xmm7, [ecx + 30h]
  shufps xmm1, xmm1, 0
  mulps xmm1, xmm7
  addps xmm0, xmm1
  movss xmm1, [edx + 10h]
  shufps xmm1, xmm1, 0
  mulps xmm1, xmm4
  movss xmm2, [edx + 14h]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm5
  addps xmm1, xmm2
  movss xmm2, [edx + 18h]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm6
  addps xmm1, xmm2
  movss xmm2, [edx + 1Ch]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm7
  addps xmm1, xmm2
  movss xmm2, [edx + 20h]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm4
  movss xmm3, [edx + 24h]
  shufps xmm3, xmm3, 0
  mulps xmm3, xmm5
  addps xmm2, xmm3
  movss xmm3, [edx + 28h]
  shufps xmm3, xmm3, 0
  mulps xmm3, xmm6
  addps xmm2, xmm3
  movss xmm3, [edx + 2Ch]
  shufps xmm3, xmm3, 0
  mulps xmm3, xmm7
  addps xmm2, xmm3
  movss xmm3, [edx + 30h]
  movlps [eax], xmm0
  movhps [eax + 8h], xmm0
  shufps xmm3, xmm3, 0
  mulps xmm3, xmm4
  movss xmm4, [edx + 34h]
  movlps [eax + 10h], xmm1
  movhps [eax + 18h], xmm1
  shufps xmm4, xmm4, 0
  mulps xmm4, xmm5
  addps xmm3, xmm4
  movss xmm4, [edx + 38h]
  movlps [eax + 20h], xmm2
  movhps [eax + 28h], xmm2
  shufps xmm4, xmm4, 0
  mulps xmm4, xmm6
  addps xmm3, xmm4
  movss xmm4, [edx + 3Ch]
  shufps xmm4, xmm4, 0
  mulps xmm4, xmm7
  addps xmm3, xmm4
  movlps [eax + 30h], xmm3
  movhps [eax + 38h], xmm3
end;

procedure G2MatMulStd(const OutM, InM1, InM2: PG2Mat);
var
  mr: TG2Mat;
begin
  with mr do
  begin
    e00 := InM1^.e00 * InM2^.e00 + InM1^.e01 * InM2^.e10 + InM1^.e02 * InM2^.e20 + InM1^.e03 * InM2^.e30;
    e10 := InM1^.e10 * InM2^.e00 + InM1^.e11 * InM2^.e10 + InM1^.e12 * InM2^.e20 + InM1^.e13 * InM2^.e30;
    e20 := InM1^.e20 * InM2^.e00 + InM1^.e21 * InM2^.e10 + InM1^.e22 * InM2^.e20 + InM1^.e23 * InM2^.e30;
    e30 := InM1^.e30 * InM2^.e00 + InM1^.e31 * InM2^.e10 + InM1^.e32 * InM2^.e20 + InM1^.e33 * InM2^.e30;
    e01 := InM1^.e00 * InM2^.e01 + InM1^.e01 * InM2^.e11 + InM1^.e02 * InM2^.e21 + InM1^.e03 * InM2^.e31;
    e11 := InM1^.e10 * InM2^.e01 + InM1^.e11 * InM2^.e11 + InM1^.e12 * InM2^.e21 + InM1^.e13 * InM2^.e31;
    e21 := InM1^.e20 * InM2^.e01 + InM1^.e21 * InM2^.e11 + InM1^.e22 * InM2^.e21 + InM1^.e23 * InM2^.e31;
    e31 := InM1^.e30 * InM2^.e01 + InM1^.e31 * InM2^.e11 + InM1^.e32 * InM2^.e21 + InM1^.e33 * InM2^.e31;
    e02 := InM1^.e00 * InM2^.e02 + InM1^.e01 * InM2^.e12 + InM1^.e02 * InM2^.e22 + InM1^.e03 * InM2^.e32;
    e12 := InM1^.e10 * InM2^.e02 + InM1^.e11 * InM2^.e12 + InM1^.e12 * InM2^.e22 + InM1^.e13 * InM2^.e32;
    e22 := InM1^.e20 * InM2^.e02 + InM1^.e21 * InM2^.e12 + InM1^.e22 * InM2^.e22 + InM1^.e23 * InM2^.e32;
    e32 := InM1^.e30 * InM2^.e02 + InM1^.e31 * InM2^.e12 + InM1^.e32 * InM2^.e22 + InM1^.e33 * InM2^.e32;
    e03 := InM1^.e00 * InM2^.e03 + InM1^.e01 * InM2^.e13 + InM1^.e02 * InM2^.e23 + InM1^.e03 * InM2^.e33;
    e13 := InM1^.e10 * InM2^.e03 + InM1^.e11 * InM2^.e13 + InM1^.e12 * InM2^.e23 + InM1^.e13 * InM2^.e33;
    e23 := InM1^.e20 * InM2^.e03 + InM1^.e21 * InM2^.e13 + InM1^.e22 * InM2^.e23 + InM1^.e23 * InM2^.e33;
    e33 := InM1^.e30 * InM2^.e03 + InM1^.e31 * InM2^.e13 + InM1^.e32 * InM2^.e23 + InM1^.e33 * InM2^.e33;
  end;
  OutM^ := mr;
end;

procedure G2MatInvSSE(const OutM, InM: PG2Mat);
asm
  movlps xmm0, [edx]
  movhps xmm0, [edx + 10h]
  movlps xmm1, [edx + 20h]
  movhps xmm1, [edx + 30h]
  movaps xmm2, xmm0
  shufps xmm2, xmm1, $88
  shufps xmm1, xmm0, $DD
  movlps xmm0, [edx + 8]
  movhps xmm0, [edx + 18h]
  movlps xmm3, [edx + 28h]
  movhps xmm3, [edx + 38h]
  movaps xmm4, xmm0
  shufps xmm4, xmm3, $88
  shufps xmm3, xmm0, $DD
  movaps xmm0, xmm4
  mulps xmm0, xmm3
  shufps xmm0, xmm0, $B1
  movaps xmm5, xmm1
  mulps xmm5, xmm0
  movaps xmm6, xmm2
  mulps xmm6, xmm0
  shufps xmm0, xmm0, $4E
  movaps xmm7, xmm5
  movaps xmm5, xmm1
  mulps xmm5, xmm0
  subps xmm5, xmm7
  movaps xmm7, xmm6
  movaps xmm6, xmm2
  mulps xmm6, xmm0
  subps xmm6, xmm7
  shufps xmm6, xmm6, $4E
  movlps [eax], xmm6
  movhps [eax + 8], xmm6
  movaps xmm0, xmm1
  mulps xmm0, xmm4
  shufps xmm0, xmm0, $B1
  movaps xmm7, xmm5
  movaps xmm5, xmm3
  mulps xmm5, xmm0
  addps xmm5, xmm7
  movaps xmm6, xmm2
  mulps xmm6, xmm0
  shufps xmm0, xmm0, $4E
  movaps xmm7, xmm3
  mulps xmm7, xmm0
  subps xmm5, xmm7
  movaps xmm7, xmm6
  movaps xmm6, xmm2
  mulps xmm6, xmm0
  subps xmm6, xmm7
  shufps xmm6, xmm6, $4E
  movaps xmm0, xmm1
  shufps xmm0, xmm0, $4E
  mulps xmm0, xmm3
  shufps xmm0, xmm0, $B1
  shufps xmm4, xmm4, $4E
  movaps xmm7, xmm4
  mulps xmm7, xmm0
  addps xmm5, xmm7
  movlps [eax + 10h], xmm6
  movhps [eax + 18h], xmm6
  movaps xmm6, xmm2
  mulps xmm6, xmm0
  shufps xmm0, xmm0, $4E
  movaps xmm7, xmm4
  mulps xmm7, xmm0
  subps xmm5, xmm7
  movaps xmm7, xmm6
  movaps xmm6, xmm2
  mulps xmm6, xmm0
  subps xmm6, xmm7
  shufps xmm6, xmm6, $4E
  movaps xmm0, xmm2
  mulps xmm0, xmm1
  shufps xmm0, xmm0, $B1
  movaps xmm7, xmm3
  mulps xmm7, xmm0
  addps xmm6, xmm7
  movlps [eax + 20h], xmm5
  movhps [eax + 28h], xmm5
  movups xmm7, [eax + 10h]
  movaps xmm5, xmm4
  mulps xmm5, xmm0
  subps xmm5, xmm7
  shufps xmm0, xmm0, $4E
  movaps xmm7, xmm6
  movaps xmm6, xmm3
  mulps xmm6, xmm0
  subps xmm6, xmm7
  movaps xmm7, xmm4
  mulps xmm7, xmm0
  subps xmm5, xmm7
  movaps xmm0, xmm2
  mulps xmm0, xmm3
  shufps xmm0, xmm0, $B1
  movlps [eax + 10h], xmm5
  movhps [eax + 18h], xmm5
  movups xmm5, [eax]
  movaps xmm7, xmm4
  mulps xmm7, xmm0
  subps xmm5, xmm7
  movaps xmm7, xmm1
  mulps xmm7, xmm0
  addps xmm6, xmm7
  shufps xmm0, xmm0, $4E
  movaps xmm7, xmm4
  mulps xmm7, xmm0
  addps xmm5, xmm7
  movaps xmm7, xmm1
  mulps xmm7, xmm0
  subps xmm6, xmm7
  movaps xmm0, xmm2
  mulps xmm0, xmm4
  movups xmm4, [eax + 10h]
  shufps xmm0, xmm0, $B1
  movaps xmm7, xmm3
  mulps xmm7, xmm0
  addps xmm5, xmm7
  movaps xmm7, xmm1
  mulps xmm7, xmm0
  subps xmm4, xmm7
  shufps xmm0, xmm0, $4E
  mulps xmm3, xmm0
  subps xmm5, xmm3
  mulps xmm1, xmm0
  addps xmm4, xmm1
  movups xmm1, [eax + 20h]
  mulps xmm2, xmm1
  movaps xmm7, xmm2
  shufps xmm7, xmm7, $4E
  addps xmm2, xmm7
  movaps xmm7, xmm2
  shufps xmm7, xmm7, $B1
  addss xmm2, xmm7
  rcpss xmm0, xmm2
  movss xmm7, xmm0
  mulss xmm7, xmm7
  mulss xmm2, xmm7
  addss xmm0, xmm0
  subss xmm0, xmm2
  shufps xmm0, xmm0, 0
  mulps xmm1, xmm0
  movlps [eax], xmm1
  movhps [eax + 8], xmm1
  mulps xmm5, xmm0
  movlps [eax + 10h], xmm5
  movhps [eax + 18h], xmm5
  mulps xmm6, xmm0
  movlps [eax + 20h], xmm6
  movhps [eax + 28h], xmm6
  mulps xmm4, xmm0
  movlps [eax + 30h], xmm4
  movhps [eax + 38h], xmm4
end;

procedure G2MatInvStd(const OutM, InM: PG2Mat);
var
  d, di: Single;
begin
  di := InM^.e00;
  d := 1 / di;
  with OutM^ do
  begin
    e00 := d;
    e10 := -InM^.e10 * d;
    e20 := -InM^.e20 * d;
    e30 := -InM^.e30 * d;
    e01 := InM^.e01 * d;
    e02 := InM^.e02 * d;
    e03 := InM^.e03 * d;
    e11 := InM^.e11 + e10 * e01 * di;
    e12 := InM^.e12 + e10 * e02 * di;
    e13 := InM^.e13 + e10 * e03 * di;
    e21 := InM^.e21 + e20 * e01 * di;
    e22 := InM^.e22 + e20 * e02 * di;
    e23 := InM^.e23 + e20 * e03 * di;
    e31 := InM^.e31 + e30 * e01 * di;
    e32 := InM^.e32 + e30 * e02 * di;
    e33 := InM^.e33 + e30 * e03 * di;
    di := e11;
    d := 1 / di;
    e11 := d;
    e01 := -e01 * d;
    e21 := -e21 * d;
    e31 := -e31 * d;
    e10 := e10 * d;
    e12 := e12 * d;
    e13 := e13 * d;
    e00 := e00 + e01 * e10 * di;
    e02 := e02 + e01 * e12 * di;
    e03 := e03 + e01 * e13 * di;
    e20 := e20 + e21 * e10 * di;
    e22 := e22 + e21 * e12 * di;
    e23 := e23 + e21 * e13 * di;
    e30 := e30 + e31 * e10 * di;
    e32 := e32 + e31 * e12 * di;
    e33 := e33 + e31 * e13 * di;
    di := e22;
    d := 1 / di;
    e22 := d;
    e02 := -e02 * d;
    e12 := -e12 * d;
    e32 := -e32 * d;
    e20 := e20 * d;
    e21 := e21 * d;
    e23 := e23 * d;
    e00 := e00 + e02 * e20 * di;
    e01 := e01 + e02 * e21 * di;
    e03 := e03 + e02 * e23 * di;
    e10 := e10 + e12 * e20 * di;
    e11 := e11 + e12 * e21 * di;
    e13 := e13 + e12 * e23 * di;
    e30 := e30 + e32 * e20 * di;
    e31 := e31 + e32 * e21 * di;
    e33 := e33 + e32 * e23 * di;
    di := e33;
    d := 1 / di;
    e33 := d;
    e03 := -e03 * d;
    e13 := -e13 * d;
    e23 := -e23 * d;
    e30 := e30 * d;
    e31 := e31 * d;
    e32 := e32 * d;
    e00 := e00 + e03 * e30 * di;
    e01 := e01 + e03 * e31 * di;
    e02 := e02 + e03 * e32 * di;
    e10 := e10 + e13 * e30 * di;
    e11 := e11 + e13 * e31 * di;
    e12 := e12 + e13 * e32 * di;
    e20 := e20 + e23 * e30 * di;
    e21 := e21 + e23 * e31 * di;
    e22 := e22 + e23 * e32 * di;
  end;
end;

procedure G2Vec2MatMul3x3Std(const OutV, InV: PG2Vec2; const InM: PG2Mat);
var
  vr: TG2Vec2;
begin
  vr.x := InV^.x * InM^.e00 + InV^.y * InM^.e10;
  vr.y := InV^.x * InM^.e01 + InV^.y * InM^.e11;
  OutV^ := vr;
end;

procedure G2Vec2MatMul4x3Std(const OutV, InV: PG2Vec2; const InM: PG2Mat);
var
  vr: TG2Vec2;
begin
  vr.x := InV^.x * InM^.e00 + InV^.y * InM^.e10 + InM^.e30;
  vr.y := InV^.x * InM^.e01 + InV^.y * InM^.e11 + InM^.e31;
  OutV^ := vr;
end;

procedure G2Vec2MatMul4x4Std(const OutV, InV: PG2Vec2; const InM: PG2Mat);
var
  vr: TG2Vec2;
  w: Single;
begin
  vr.x := InV^.x * InM^.e00 + InV^.y * InM^.e10 + InM^.e30;
  vr.y := InV^.x * InM^.e01 + InV^.y * InM^.e11 + InM^.e31;
  w := 1 / (InV^.x * InM^.e03 + InV^.y * InM^.e13 + InM^.e33);
  OutV^.x := vr.x * w;
  OutV^.y := vr.y * w;
end;

procedure G2Vec3MatMul3x3SSE(const OutV, InV: PG2Vec3; const InM: PG2Mat);
asm
  movups xmm0, [ecx]
  movss xmm1, [edx]
  shufps xmm1, xmm1, 0
  mulps xmm1, xmm0
  movups xmm0, [ecx + 10h]
  movss xmm2, [edx + 4]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movups xmm0, [ecx + 20h]
  movss xmm2, [edx + 8]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movlps [eax], xmm1
  movhlps xmm1, xmm1
  movss [eax + 8], xmm1
end;

procedure G2Vec3MatMul3x3Std(const OutV, InV: PG2Vec3; const InM: PG2Mat);
var
  vr: TG2Vec3;
begin
  vr.x := InV^.x * InM^.e00 + InV^.y * InM^.e10 + InV^.z * InM^.e20;
  vr.y := InV^.x * InM^.e01 + InV^.y * InM^.e11 + InV^.z * InM^.e21;
  vr.z := InV^.x * InM^.e02 + InV^.y * InM^.e12 + InV^.z * InM^.e22;
  OutV^ := vr;
end;

procedure G2Vec3MatMul4x3SSE(const OutV, InV: PG2Vec3; const InM: PG2Mat);
asm
  movups xmm0, [ecx]
  movss xmm1, [edx]
  shufps xmm1, xmm1, 0
  mulps xmm1, xmm0
  movups xmm0, [ecx + 10h]
  movss xmm2, [edx + 4]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movups xmm0, [ecx + 20h]
  movss xmm2, [edx + 8]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movups xmm0, [ecx + 30h]
  addps xmm1, xmm0
  movlps [eax], xmm1
  movhlps xmm1, xmm1
  movss [eax + 8], xmm1
end;

procedure G2Vec3MatMul4x3Std(const OutV, InV: PG2Vec3; const InM: PG2Mat);
var
  vr: TG2Vec3;
begin
  vr.x := InV^.x * InM^.e00 + InV^.y * InM^.e10 + InV^.z * InM^.e20 + InM^.e30;
  vr.y := InV^.x * InM^.e01 + InV^.y * InM^.e11 + InV^.z * InM^.e21 + InM^.e31;
  vr.z := InV^.x * InM^.e02 + InV^.y * InM^.e12 + InV^.z * InM^.e22 + InM^.e32;
  OutV^ := vr;
end;

procedure G2Vec3MatMul4x4SSE(const OutV, InV: PG2Vec3; const InM: PG2Mat);
asm
  movups xmm0, [ecx]
  movss xmm1, [edx]
  shufps xmm1, xmm1, 0
  mulps xmm1, xmm0
  movups xmm0, [ecx + 10h]
  movss xmm2, [edx + 4]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movups xmm0, [ecx + 20h]
  movss xmm2, [edx + 8]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movups xmm0, [ecx + 30h]
  addps xmm1, xmm0
  movaps xmm0, xmm1
  shufps xmm0, xmm0, $FF
  rcpps xmm0, xmm0
  mulps xmm1, xmm0
  movlps [eax], xmm1
  movhlps xmm1, xmm1
  movss [eax + 8], xmm1
end;

procedure G2Vec3MatMul4x4Std(const OutV, InV: PG2Vec3; const InM: PG2Mat);
var
  vr: TG2Vec3;
  w: Single;
begin
  vr.x := InV^.x * InM^.e00 + InV^.y * InM^.e10 + InV^.z * InM^.e20 + InM^.e30;
  vr.y := InV^.x * InM^.e01 + InV^.y * InM^.e11 + InV^.z * InM^.e21 + InM^.e31;
  vr.z := InV^.x * InM^.e02 + InV^.y * InM^.e12 + InV^.z * InM^.e22 + InM^.e32;
  w := 1 / (InV^.x * InM^.e03 + InV^.y * InM^.e13 + InV^.z * InM^.e23 + InM^.e33);
  OutV^.x := vr.x * w;
  OutV^.y := vr.y * w;
  OutV^.z := vr.z * w;
end;

procedure G2Vec4MatMulSSE(const OutV, InV: PG2Vec4; const InM: PG2Mat);
asm
  movups xmm0, [ecx]
  movss xmm1, [edx]
  shufps xmm1, xmm1, 0
  mulps xmm1, xmm0
  movups xmm0, [ecx + 10h]
  movss xmm2, [edx + 4]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movups xmm0, [ecx + 20h]
  movss xmm2, [edx + 8]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movups xmm0, [ecx + 30h]
  movss xmm2, [edx + 0Ch]
  shufps xmm2, xmm2, 0
  mulps xmm2, xmm0
  addps xmm1, xmm2
  movups [eax], xmm1
end;

procedure G2Vec4MatMulStd(const OutV, InV: PG2Vec4; const InM: PG2Mat);
var
  vr: TG2Vec4;
begin
  vr.x := InV^.x * InM^.e00 + InV^.y * InM^.e10 + InV^.z * InM^.e20 + InV^.w * InM^.e30;
  vr.y := InV^.x * InM^.e01 + InV^.y * InM^.e11 + InV^.z * InM^.e21 + InV^.w * InM^.e31;
  vr.z := InV^.x * InM^.e02 + InV^.y * InM^.e12 + InV^.z * InM^.e22 + InV^.w * InM^.e32;
  vr.w := InV^.x * InM^.e03 + InV^.y * InM^.e13 + InV^.z * InM^.e23 + InV^.w * InM^.e33;
  OutV^ := vr;
end;

function G2Vec3LenSSE(const InV: PG2Vec3): Single;
asm
  movss xmm0, [eax + 8]
  movlhps xmm0, xmm0
  movlps xmm0, [eax]
  mulps xmm0, xmm0
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $C9
  addss xmm0, xmm1
  shufps xmm1, xmm1, $C9
  addss xmm0, xmm1
  sqrtss xmm0, xmm0
  movss [Result], xmm0
end;

function G2Vec3LenStd(const InV: PG2Vec3): Single;
begin
  Result := Sqrt(InV^.x * InV^.x + InV^.y * InV^.y + InV^.z * InV^.z);
end;

function G2Vec4LenSSE(const InV: PG2Vec4): Single;
asm
  movups xmm0, [eax]
  mulps xmm0, xmm0
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $B1
  addps xmm0, xmm1
  movhlps xmm1, xmm0
  addss xmm0, xmm1
  sqrtss xmm0, xmm0
  movss [Result], xmm0
end;

function G2Vec4LenStd(const InV: PG2Vec4): Single;
begin
  Result := Sqrt(InV^.x * InV^.x + InV^.y * InV^.y + InV^.z * InV^.z + InV^.w * InV^.w);
end;

procedure G2Vec3NormSSE(const OutV, InV: PG2Vec3);
asm
  movss xmm0, [edx + 8]
  movlhps xmm0, xmm0
  movlps xmm0, [edx]
  movaps xmm1, xmm0
  mulps xmm1, xmm1
  movaps xmm2, xmm1
  shufps xmm2, xmm2, $C9
  addss xmm1, xmm2
  shufps xmm2, xmm2, $C9
  addss xmm1, xmm2
  movss [eax], xmm1
  mov ecx, eax
  cmp [eax], 0
  jbe @Bail
  rsqrtss xmm1, xmm1
  shufps xmm1, xmm1, 0
  mulps xmm0, xmm1
  movlps [ecx], xmm0
  movhlps xmm0, xmm0
  movss [ecx + 8], xmm0
  ret
@Bail:
  mov [ecx], 0
  mov [ecx + 4], 0
  mov [ecx + 8], 0
end;

procedure G2Vec3NormStd(const OutV, InV: PG2Vec3);
var
  d: Single;
begin
  d := Sqrt(InV^.x * InV^.x + InV^.y * InV^.y + InV^.z * InV^.z);
  if d > 0 then
  begin
    d := 1 / d;
    OutV^.x := InV^.x * d;
    OutV^.y := InV^.y * d;
    OutV^.z := InV^.z * d;
  end
  else
  begin
    OutV^.x := 0;
    OutV^.y := 0;
    OutV^.z := 0;
  end;
end;

procedure G2Vec4NormSSE(const OutV, InV: PG2Vec4);
asm
  movups xmm0, [edx]
  movaps xmm1, xmm0
  mulps xmm1, xmm1
  movaps xmm2, xmm1
  shufps xmm2, xmm2, $B1
  addps xmm1, xmm2
  movhlps xmm2, xmm1
  addss xmm1, xmm2
  movss [eax], xmm1
  mov ecx, eax
  cmp [eax], 0
  jbe @Bail
  rsqrtss xmm1, xmm1
  shufps xmm1, xmm1, 0
  mulps xmm0, xmm1
  movlps [ecx], xmm0
  movhps [ecx + 8], xmm0
  ret
@Bail:
  mov [ecx], 0
  mov [ecx + 4], 0
  mov [ecx + 8], 0
end;

procedure G2Vec4NormStd(const OutV, InV: PG2Vec4);
var
  d: Single;
begin
  d := Sqrt(InV^.x * InV^.x + InV^.y * InV^.y + InV^.z * InV^.z + InV^.w * InV^.w);
  if d > 0 then
  begin
    d := 1 / d;
    OutV^.x := InV^.x * d;
    OutV^.y := InV^.y * d;
    OutV^.z := InV^.z * d;
    OutV^.w := InV^.w * d;
  end
  else
  begin
    OutV^.x := 0;
    OutV^.y := 0;
    OutV^.z := 0;
    OutV^.w := 0;
  end;
end;

procedure G2Vec3CrossSSE(const OutV, InV1, InV2: PG2Vec3);
asm
  movss xmm1, [edx + 8]
  movlhps xmm1, xmm1
  movlps xmm1, [edx]
  movss xmm2, [ecx + 8]
  movlhps xmm2, xmm2
  movlps xmm2, [ecx]
  shufps xmm1, xmm1, $C9
  shufps xmm2, xmm2, $D2
  movaps xmm0, xmm1
  mulps xmm0, xmm2
  shufps xmm1, xmm1, $C9
  shufps xmm2, xmm2, $D2
  mulps xmm1, xmm2
  subps xmm0, xmm1
  movlps [eax], xmm0
  movhlps xmm0, xmm0
  movss [eax + 8], xmm0
end;

procedure G2Vec3CrossStd(const OutV, InV1, InV2: PG2Vec3);
begin
  OutV^.x := InV1^.y * InV2^.z - InV1^.z * InV2^.y;
  OutV^.y := InV1^.z * InV2^.x - InV1^.x * InV2^.z;
  OutV^.z := InV1^.x * InV2^.y - InV1^.y * InV2^.x;
end;

procedure G2Mat2MulSSE(const OutM, InM1, InM2: PG2Mat2);
asm
  movups xmm0, [edx]
  movups xmm1, [ecx]
  movaps xmm2, xmm0
  shufps xmm0, xmm0, $a0
  shufps xmm2, xmm2, $f5
  movaps xmm3, xmm1
  shufps xmm1, xmm1, $44
  shufps xmm3, xmm3, $ee
  mulps xmm0, xmm1
  mulps xmm2, xmm3
  addps xmm0, xmm2
  movlps [eax], xmm0
  movhps [eax + 8h], xmm0
end;

procedure G2Mat2MulStd(const OutM, InM1, InM2: PG2Mat2);
var
  mr: TG2Mat2;
begin
  with mr do
  begin
    e00 := InM1^.e00 * InM2^.e00 + InM1^.e01 * InM2^.e10;
    e01 := InM1^.e00 * InM2^.e01 + InM1^.e01 * InM2^.e11;
    e10 := InM1^.e10 * InM2^.e00 + InM1^.e11 * InM2^.e10;
    e11 := InM1^.e10 * InM2^.e01 + InM1^.e11 * InM2^.e11;
  end;
  OutM^ := mr;
end;

procedure G2Vec2Mat2MulSSE(const OutV, InV: PG2Vec2; const InM: PG2Mat2);
asm
  movlps xmm0, [edx]
  shufps xmm0, xmm0, $44
  movups xmm1, [ecx]
  shufps xmm1, xmm1, $D8
  mulps xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $b1
  addps xmm0, xmm1
  shufps xmm0, xmm0, $d8
  movlps [eax], xmm0
end;

procedure G2Vec2Mat2MulStd(const OutV, InV: PG2Vec2; const InM: PG2Mat2);
var
  vr: TG2Vec2;
begin
  vr.x := InV^.x * InM^.e00 + InV^.y * InM^.e10;
  vr.y := InV^.x * InM^.e01 + InV^.y * InM^.e11;
  OutV^ := vr;
end;
//Utility Functions END

//Unit Functions BEGIN
procedure CPUExtensions;
asm
  push ebx
  mov eax, 1
  cpuid
  test ecx, 00000001h
  jz @CheckSSE2
  mov [SysSSE3], 1
@CheckSSE2:
  test edx, 04000000h
  jz @CheckSSE
  mov [SysSSE2], 1
@CheckSSE:
  test edx, 02000000h
  jz @CheckMMX
  mov [SysSSE], 1
@CheckMMX:
  test edx, 00800000h
  jz @Done
  mov [SysMMX], 1
@Done:
  pop ebx
end;
//Unit Functions END

initialization
  CPUExtensions;
  if SysSSE then
  begin
    G2MatAdd := G2MatAddSSE;
    G2MatSub := G2MatSubSSE;
    G2MatFltMul := G2MatFltMulSSE;
    G2MatMul := G2MatMulSSE;
    G2MatInv := G2MatInvSSE;
    G2Vec3MatMul3x3 := G2Vec3MatMul3x3SSE;
    G2Vec3MatMul4x3 := G2Vec3MatMul4x3SSE;
    G2Vec3MatMul4x4 := G2Vec3MatMul4x4SSE;
    G2Vec4MatMul := G2Vec4MatMulSSE;
    G2Vec3Len := G2Vec3LenSSE;
    G2Vec4Len := G2Vec4LenSSE;
    G2Vec3Norm := G2Vec3NormSSE;
    G2Vec4Norm := G2Vec4NormSSE;
    G2Vec3Cross := G2Vec3CrossSSE;
    G2Mat2Mul := G2Mat2MulSSE;
    G2Vec2Mat2Mul := G2Vec2Mat2MulSSE;
  end
  else
  begin
    G2MatAdd := G2MatAddStd;
    G2MatSub := G2MatSubStd;
    G2MatFltMul := G2MatFltMulStd;
    G2MatMul := G2MatMulStd;
    G2MatInv := G2MatInvStd;
    G2Vec3MatMul3x3 := G2Vec3MatMul3x3Std;
    G2Vec3MatMul4x3 := G2Vec3MatMul4x3Std;
    G2Vec3MatMul4x4 := G2Vec3MatMul4x4Std;
    G2Vec4MatMul := G2Vec4MatMulStd;
    G2Vec3Len := G2Vec3LenStd;
    G2Vec4Len := G2Vec4LenStd;
    G2Vec3Norm := G2Vec3NormStd;
    G2Vec4Norm := G2Vec4NormStd;
    G2Vec3Cross := G2Vec3CrossStd;
    G2Mat2Mul := G2Mat2MulStd;
    G2Vec2Mat2Mul := G2Vec2Mat2MulStd;
  end;

end.