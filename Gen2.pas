//Gen2 v1.3
//Author: Dan

unit Gen2;

{$include Gen2.inc}

interface

uses
  Windows,
  Classes,
  Types,
  Controls,
  Graphics,
  SysUtils,
  SyncObjs,
  ActiveX,
  Registry,
  Forms,
  Messages,
  MMSystem,
  Math,
  ZLib,
  Dialogs,
  WinSock,
  DirectInput,
  DirectShow9,
  DirectMusic,
  DirectSound,
  Direct3D9,
  DirectDraw,
  DXTypes,
  D3DX9,
  G2AVI,
  G2Math,
  G2MeshLoader;

type
  TG2Class = class;
  TG2HighClass = class;
  TG2Core = class;
  TG2PackLinker = class;
  TG2Pack = class;
  TG2List = class;
  TG2FileRW = class;
  TG2Engine = class;
  TG2Timer = class;
  TG2Graphics = class;
  TG2GraphicsSpecs = class;
  TG2GraphicsInitParams = class;
  TG2GraphicsRunTimeParams = class;
  TG2Transforms = class;
  TG2Lights = class;
  TG2SwapChain = class;
  TG2Audio = class;
  TG2Input = class;
  TG2Network = class;
  TG2Module = class;
  TG2Plug = class;
  TG2PlugTimer = class;
  TG2PlugGraphics = class;
  TG2PlugAudio = class;
  TG2PlugInput = class;
  TG2PlugNetwork = class;
  TG2ResMgr = class;
  TG2Res = class;
  TG2MusicMgr = class;
  TG2Music = class;
  TG2SoundMgr = class;
  TG2Sound = class;
  TG2SoundInst = class;
  TG2TextureMgr = class;
  TG2TextureBase = class;
  TG2Texture2DBase = class;
  TG2Texture2D = class;
  TG2Texture2DRT = class;
  TG2Texture2DDS = class;
  TG2Texture2DVideo = class;
  TG2TextureCube = class;
  TG2TextureCubeRT = class;
  TG2TextureVolume = class;
  TG2SurfaceMgr = class;
  TG2Surface = class;
  TG2SurfaceRT = class;
  TG2SurfaceDS = class;
  TG2NetServer = class;
  TG2NetClient = class;
  TG2FontMgr = class;
  TG2Font = class;
  TG2MeshMgr = class;
  TG2Mesh = class;
  TG2MeshInst = class;
  TG2Shared = class;
  TG2VBMgr = class;
  TG2VB = class;
  TG2IBMgr = class;
  TG2IB = class;
  TG2ScriptMgr = class;
  TG2Script = class;
  TG2UI = class;
  TG2UISkin = class;
  TG2UIFrame = class;
  TG2UIPanel = class;
  TG2UIButton = class;
  TG2UILabel = class;
  TG2RenderStates = class;
  TG2SamplerStates = class;
  TG2TextureStageStates = class;
  TG2SharedVB2D = class;
  TG2Render = class;
  TG2Render2D = class;
  TG2Render3D = class;
  TG2Camera = class;
  TG2Primitives2D = class;
  TG2Primitives3D = class;
  TG2ShaderMgr = class;
  TG2Shader = class;
  TG2VertexShader = class;
  TG2PixelShader = class;
  TG2EffectMgr = class;
  TG2Effect = class;
  TG2ShaderLib = class;
  TG2ShaderBin = class;
  TG2Scene3D = class;
  TG2SkyBox = class;
  TG2PostProcess = class;
  TG2App = class;
  TG2Thread = class;

  PG2Module = ^TG2Module;
  PG2Plug = ^TG2Plug;

  TG2Proc = procedure;
  TG2ProcObj = procedure of Object;
  TG2ProcRef = procedure (const Ref: Pointer);
  TG2ProcRefObj = procedure (const Ref: Pointer) of Object;

  TG2InputKeyDown = procedure (const Key: Byte) of Object;
  TG2InputKeyUp = procedure (const Key: Byte) of Object;
  TG2InputKeyPress = procedure (const Key: AnsiChar) of Object;
  TG2InputMouseDown = procedure (const Button: Byte) of Object;
  TG2InputMouseUp = procedure (const Button: Byte) of Object;
  TG2InputMouseMove = procedure (const Shift: TPoint) of Object;
  TG2InputWheelMove = procedure (const Shift: Integer) of Object;

  CG2ModClass = class of TG2Module;
  CG2PlugClass = class of TG2Plug;
  CG2UIFrameClass = class of TG2UIFrame;

  TG2Result = (
    grOk,
    grFail,
    grConditionalOk,
    grNotInitialized,
    grInvalidCall,
    grRedundantCall
  );

  TG2VertexProcessing = (
    vpSoftware,
    vpHardware,
    vpMixed
  );

  TG2MeshMode = (
    mmSW,
    mmFF,
    mmSM2,
    mmSM3
  );

  TG2TimerMode = (
    tmAppIdle,
    tmWinTimer,
    tmManual
  );

  TG2PrimType = (
    ptPointList,
    ptLineList,
    ptLineStrip,
    ptTriangleList,
    ptTriangleStrip,
    ptTriangleFan
  );

  TG2CameraMode = (
    cmFree,
    cmHorizontal
  );

  TG2Scene2DDivType = (
    dtNoDiv,
    dtDivQ,
    dtDivH,
    dtDivV
  );

  TG2Scene2DOcclusionCheck = (
    ocNone,
    ocPartial,
    ocFull
  );

  TG2NetMesDesc = (
    mdPing,
    mdUser
  );

  TG2NetStatus = (
    nsIdle,
    nsListen,
    nsDisconnected,
    nsConnecting,
    nsConnected
  );

  PG2CompiledShader = ^TG2CompiledShader;
  TG2CompiledShader = array of Byte;

  PG2Index16 = ^TG2Index16;
  TG2Index16 = Word;
  PG2Index16Array = ^TG2Index16Array;
  TG2Index16Array = array[Word] of TG2Index16;

  TDWordArray = array[Word] of DWord;
  PDWordArray = ^TDWordArray;

  TG2Index32 = DWord;
  PG2Index32 = ^TG2Index32;

  TG2Index32Array = array[Word] of TG2Index32;
  PG2Index32Array = ^TG2Index32Array;

  TG2NetMesHeader = packed record
  public
    var Desc: TG2NetMesDesc;
    var Len: Integer;
  end;

//TG2Color BEGIN
  PG2Color = ^TG2Color;
  TG2Color = record
  public
    var b, g, r, a: Byte;
    class operator Equal(const c1, c2: TG2Color): Boolean;
    class operator NotEqual(const c1, c2: TG2Color): Boolean;
    class operator Explicit(const c: TG2Color): DWord;
    class operator Explicit(const c: DWord): TG2Color;
    class operator Explicit(const c: TG2Color): TD3DColorValue;
    class operator Explicit(const c: TD3DColorValue): TG2Color;
    class operator Explicit(const c: TG2Color): TG2Vec4;
    class operator Explicit(const v: TG2Vec4): TG2Color;
    class operator Explicit(const c: TG2Color): TG2Vec3;
    class operator Explicit(const v: TG2Vec3): TG2Color;
    class operator Implicit(const c: TG2Color): DWord;
    class operator Implicit(const c: DWord): TG2Color;
    class operator Implicit(const c: TG2Color): TD3DColorValue;
    class operator Implicit(const c: TD3DColorValue): TG2Color;
    class operator Implicit(const c: TG2Color): TG2Vec4;
    class operator Implicit(const v: TG2Vec4): TG2Color;
    class operator Implicit(const c: TG2Color): TG2Vec3;
    class operator Implicit(const v: TG2Vec3): TG2Color;
  end;
  PG2ColorArray = ^TG2ColorArray;
  TG2ColorArray = array[0..High(Word)] of TG2Color;
//TG2Color END

//TG2BlendMode BEGIN
  PG2BlendMode = ^TG2BlendMode;
  TG2BlendMode = record
  public
    var ColSrc, ColDst, AlphaSrc, AlphaDst: Byte;
    function SeparateAlpha: Boolean;
    procedure SetNormal;
    procedure SetAdd;
    class operator Equal(const bm1, bm2: TG2BlendMode): Boolean;
    class operator NotEqual(const bm1, bm2: TG2BlendMode): Boolean;
    class operator Explicit(const bm: TG2BlendMode): DWord;
    class operator Explicit(const bm: DWord): TG2BlendMode;
    class operator Implicit(const bm: TG2BlendMode): DWord;
    class operator Implicit(const bm: DWord): TG2BlendMode;
  end;
//TG2BlendMode END

//TG2Rect BEGIN
  PG2Rect = ^TG2Rect;
  TG2Rect = record
  public
    var Left: Single;
    var Top: Single;
    var Right: Single;
    var Bottom: Single;
    function Width: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Height: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function TopLeft: TG2Vec2;
    function TopRight: TG2Vec2;
    function BottomLeft: TG2Vec2;
    function BottomRight: TG2Vec2;
  end;
//TG2Rect END

//TG2Class BEGIN
  TG2Class = class (TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;
//TG2Class END

//TG2Core BEGIN
  TG2Core = class sealed (TG2Class)
  strict private
    var m_Handle: HWnd;
    var m_OwnWindow: Boolean;
    var m_Initialized: Boolean;
    var m_Timer: TG2Timer;
    var m_Graphics: TG2Graphics;
    var m_Audio: TG2Audio;
    var m_Input: TG2Input;
    var m_Network: TG2Network;
    var m_PackLinker: TG2PackLinker;
    var m_Mods: TG2List;
    procedure SetHandle(const Value: HWnd);
    function CreateWindow: HWnd;
    procedure FreeMods;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Handle: HWnd read m_Handle write SetHandle;
    property Timer: TG2Timer read m_Timer;
    property Graphics: TG2Graphics read m_Graphics;
    property Audio: TG2Audio read m_Audio;
    property Input: TG2Input read m_Input;
    property Network: TG2Network read m_Network;
    property PackLinker: TG2PackLinker read m_PackLinker;
    property Initialized: Boolean read m_Initialized;
    function Initialize: TG2Result;
    function Finalize: TG2Result;
    function RequestMod(
      const ModClass: CG2ModClass;
      const Module: PG2Module
    ): TG2Result;
    function ReleaseMod(
      const Module: PG2Module
    ): TG2Result;
    function RequestPlug(
      const PlugClass: CG2PlugClass;
      const Plug: PG2Plug
    ): TG2Result;
    function ReleasePlug(
      const Plug: PG2Plug
    ): TG2Result;
  end;
//TG2Core END

//TG2HighClass BEGIN
  TG2HighClass = class (TG2Class)
  strict private
    var m_Core: TG2Core;
  strict protected
    var m_Initialized: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Core: TG2Core read m_Core;
    property Initialized: Boolean read m_Initialized;
    function Initialize(const G2Core: TG2Core): TG2Result; virtual;
    function Finalize: TG2Result; virtual;
  end;
//TG2HighClass END

//TG2PackCRC BEGIN
  TG2PackCRC = packed record
  public
    var Pos: DWord;
    var Num: Byte;
  end;
//TG2PackCRC END

//TG2PackFile BEGIN
  PG2PackFile = ^TG2PackFile;
  TG2PackFile = record
  public
    var Name: AnsiString;
    var Loaded: Boolean;
    var Encrypted: Boolean;
    var Compressed: Boolean;
    var DataPos: DWord;
    var DataLength: DWord;
    var OriginalSize: DWord;
    var Data: array of Byte;
    var CRC: array[0..7] of TG2PackCRC;
  end;
//TG2PackFile END

//TG2PackFolder BEGIN
  PG2PackFolder = ^TG2PackFolder;
  TG2PackFolder = record
  public
    var Name: AnsiString;
    var Files: array of PG2PackFile;
  end;
//TG2PackFolder END

//TG2PackLinker BEGIN
  TG2PackLinker = class
  strict private
    var m_Packs: TList;
    function GetPack(const Index: Integer): TG2Pack; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPackCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    property Packs[const Index: Integer]: TG2Pack read GetPack;
    property PackCount: Integer read GetPackCount;
    function LinkPack(const FileName: String; const Key: AnsiString = ''): TG2Pack;
    procedure UnLoadCache;
    procedure GetFileData(
      const FolderName, FileName: AnsiString;
      var Data: Pointer;
      var DataSize: DWord
    );
  end;
//TG2PackLinker END

//TG2Pack BEGIN
  TG2Pack = class
  strict private
    var m_Folders: array of PG2PackFolder;
    var m_DataPos: DWord;
    var m_IsOpen: Boolean;
    var m_fs: TFileStream;
    var m_Key: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    property IsOpen: Boolean read m_IsOpen;
    property Key: AnsiString read m_Key write m_Key;
    procedure OpenPack(const FileName: String);
    procedure ClosePack;
    function FindFolder(const Name: AnsiString): PG2PackFolder;
    function FindFile(const fld: PG2PackFolder; const Name: AnsiString): PG2PackFile; overload;
    function FindFile(const FolderName: AnsiString; const Name: AnsiString): PG2PackFile; overload;
    function PackFileExists(const FileName: AnsiString): Boolean;
    procedure PreLoadFile(const f: PG2PackFile);
    procedure UnLoadFile(const f: PG2PackFile);
    procedure GetFileData(const f: PG2PackFile; var Data: Pointer; var DataSize: DWord);
    procedure UnLoadFiles;
  end;
//TG2Pack END

//TG2List BEGIN
  TG2List = class sealed (TList)
  public
    procedure FreeItems;
    destructor Destroy; override;
  end;
//TG2List END

//TG2QuickList BEGIN
  TG2QuickList = record
  strict private
    var m_Items: array of Pointer;
    var m_ItemCount: Integer;
    procedure SetItem(const Index: Integer; const Value: Pointer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetItem(const Index: Integer): Pointer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCapacity(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCapacity: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFirst: Pointer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLast: Pointer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read m_ItemCount;
    property Items[const Index: Integer]: Pointer read GetItem write SetItem; default;
    property First: Pointer read GetFirst;
    property Last: Pointer read GetLast;
    function Add(const Item: Pointer): Integer;
    function Pop: Pointer;
    function Insert(const Index: Integer; const Item: Pointer): Integer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Item: Pointer);
    procedure Clear;
  end;
  PG2QuickList = ^TG2QuickList;
//TG2QuickList END

//TG2QuickSortList BEGIN
  TG2QuickSortListItem = record
  public
    var Data: Pointer;
    var Order: Single;
  end;
  
  TG2QuickSortList = record
  strict private
    var m_Items: array of TG2QuickSortListItem;
    var m_ItemCount: Integer;
    procedure SetItem(const Index: Integer; const Value: Pointer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetItem(const Index: Integer): Pointer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure InsertItem(const Pos: Integer; const Item: Pointer; const Order: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCapacity(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCapacity: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFirst: Pointer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLast: Pointer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read m_ItemCount;
    property Items[const Index: Integer]: Pointer read GetItem write SetItem; default;
    property First: Pointer read GetFirst;
    property Last: Pointer read GetLast;
    function Add(const Item: Pointer; const Order: Single): Integer; overload;
    function Add(const Item: Pointer): Integer; overload;
    function Pop: Pointer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Item: Pointer);
    procedure Clear;
  end;
  PG2QuickSortList = ^TG2QuickSortList;
//TG2QuickSortList END

//TG2SortedList BEGIN
  TG2SortedList = class sealed (TG2Class)
  strict private
    type
      PSortedItem = ^TSortedItem;
      TSortedItem = record
        Item: Pointer;
        Order: Integer;
      end;
    var
      var m_List: TList;
    function GetItem(const Index: Integer): Pointer;
    function GetCount: Integer;
    function FindItemIndex(const Order: Integer): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Items[const Index: Integer]: Pointer read GetItem; default;
    property Count: Integer read GetCount;
    procedure Add(const Item: Pointer; const Order: Integer);
    procedure Clear;
  end;
//TG2SortedList END

//TG2LinkedListItem BEGIN
  TG2LinkedListItem = class
  strict private
    var m_Next: TG2LinkedListItem;
    var m_Prev: TG2LinkedListItem;
  public
    constructor Create;
    destructor Destroy; override;
    property Next: TG2LinkedListItem read m_Next write m_Next;
    property Prev: TG2LinkedListItem read m_Prev write m_Prev;
  end;
//TG2LinkedListItem END

//TG2LinkedList BEGIN
  TG2LinkedList = class
  strict private
    var m_First: TG2LinkedListItem;
    var m_Last: TG2LinkedListItem;
    var m_Cur: TG2LinkedListItem;
    var m_FuncNext: function: TG2LinkedListItem of object;
    function NextFirst: TG2LinkedListItem;
    function NextNext: TG2LinkedListItem;
  public
    constructor Create;
    destructor Destroy; override;
    property First: TG2LinkedListItem read m_First;
    property Last: TG2LinkedListItem read m_Last;
    property Cur: TG2LinkedListItem read m_Cur;
    function Next: TG2LinkedListItem;
    procedure Reset;
    procedure AddItem(const Item: TG2LinkedListItem);
    procedure RemoveItem(const Item: TG2LinkedListItem);
  end;
//TG2LinkedList END

//TG2FileRW BEGIN
  TG2FileRW = class
  strict private
    type TFileRWOp = (
      rwIdle,
      rwRead,
      rwWrite
    );
    var m_fs: TFileStream;
    var m_cds: TStream;
    var m_ws: TStream;
    var m_Open: Boolean;
    var m_Op: TFileRWOp;
    var m_Compression: Boolean;
    var m_ProcGetPosition: function (): Int64 of object;
    var m_ProcSetPosition: procedure (const Value: Int64) of object;
    var m_ProcSeek: function (const Offset: Int64; Origin: TSeekOrigin = soCurrent): Int64 of object;
    var m_ProcReadBuffer: procedure (var Buffer; const Size: Integer) of object;
    var m_ProcReadBool: function: Boolean of object;
    var m_ProcReadUInt1: function: Byte of object;
    var m_ProcReadSInt1: function: ShortInt of object;
    var m_ProcReadUInt2: function: Word of object;
    var m_ProcReadSInt2: function: SmallInt of object;
    var m_ProcReadUInt4: function: DWord of object;
    var m_ProcReadSInt4: function: Integer of object;
    var m_ProcReadSInt8: function: Int64 of object;
    var m_ProcReadFloat4: function: Single of object;
    var m_ProcReadFloat8: function: Double of object;
    var m_ProcReadStringNT: function: AnsiString of object;
    var m_ProcReadString: function: AnsiString of object;
    var m_ProcReadWideStringNT: function: WideString of object;
    var m_ProcReadWideString: function: WideString of object;
    var m_ProcReadVec2: function: TG2Vec2 of object;
    var m_ProcReadVec3: function: TG2Vec3 of object;
    var m_ProcReadVec4: function: TG2Vec4 of object;
    var m_ProcReadQuat: function: TG2Quat of object;
    var m_ProcReadMat4x4: function: TG2Mat of object;
    var m_ProcReadMat4x3: function: TG2Mat of object;
    var m_ProcReadColor: function: TG2Color of object;
    var m_ProcWriteBuffer: procedure (const Buffer; const Size: Integer) of object;
    var m_ProcWriteBool: procedure (const Value: Boolean) of object;
    var m_ProcWriteUInt1: procedure (const Value: Byte) of object;
    var m_ProcWriteSInt1: procedure (const Value: ShortInt) of object;
    var m_ProcWriteUInt2: procedure (const Value: Word) of object;
    var m_ProcWriteSInt2: procedure (const Value: SmallInt) of object;
    var m_ProcWriteUInt4: procedure (const Value: DWord) of object;
    var m_ProcWriteSInt4: procedure (const Value: Integer) of object;
    var m_ProcWriteSInt8: procedure (const Value: Int64) of object;
    var m_ProcWriteFloat4: procedure (const Value: Single) of object;
    var m_ProcWriteFloat8: procedure (const Value: Double) of object;
    var m_ProcWriteStringNT: procedure (const Value: AnsiString) of object;
    var m_ProcWriteString: procedure (const Value: AnsiString) of object;
    var m_ProcWriteWideStringNT: procedure (const Value: WideString) of object;
    var m_ProcWriteWideString: procedure (const Value: WideString) of object;
    var m_ProcWriteVec2: procedure (const Value: TG2Vec2) of object;
    var m_ProcWriteVec3: procedure (const Value: TG2Vec3) of object;
    var m_ProcWriteVec4: procedure (const Value: TG2Vec4) of object;
    var m_ProcWriteQuat: procedure (const Value: TG2Quat) of object;
    var m_ProcWriteMat4x4: procedure (const Value: TG2Mat) of object;
    var m_ProcWriteMat4x3: procedure (const Value: TG2Mat) of object;
    var m_ProcWriteColor: procedure (const Value: TG2Color) of object;
    function GetPositionDummy: Int64;
    procedure SetPositionDummy(const Value: Int64);
    function GetPositionProc: Int64;
    procedure SetPositionProc(const Value: Int64);
    function SeekDummy(const Offset: Int64; Origin: TSeekOrigin = soCurrent): Int64;
    function SeekProc(const Offset: Int64; Origin: TSeekOrigin = soCurrent): Int64;
    procedure ReadBufferDummy(var Buffer; const Size: Integer);
    procedure ReadBufferProc(var Buffer; const Size: Integer);
    function ReadBoolDummy: Boolean;
    function ReadBoolProc: Boolean;
    function ReadUInt1Dummy: Byte;
    function ReadUInt1Proc: Byte;
    function ReadSInt1Dummy: ShortInt;
    function ReadSInt1Proc: ShortInt;
    function ReadUInt2Dummy: Word;
    function ReadUInt2Proc: Word;
    function ReadSInt2Dummy: SmallInt;
    function ReadSInt2Proc: SmallInt;
    function ReadUInt4Dummy: DWord;
    function ReadUInt4Proc: DWord;
    function ReadSInt4Dummy: Integer;
    function ReadSInt4Proc: Integer;
    function ReadSInt8Dummy: Int64;
    function ReadSInt8Proc: Int64;
    function ReadFloat4Dummy: Single;
    function ReadFloat4Proc: Single;
    function ReadFloat8Dummy: Double;
    function ReadFloat8Proc: Double;
    function ReadStringNTDummy: AnsiString;
    function ReadStringNTProc: AnsiString;
    function ReadStringDummy: AnsiString;
    function ReadStringProc: AnsiString;
    function ReadWideStringNTDummy: WideString;
    function ReadWideStringNTProc: WideString;
    function ReadWideStringDummy: WideString;
    function ReadWideStringProc: WideString;
    function ReadVec2Dummy: TG2Vec2;
    function ReadVec2Proc: TG2Vec2;
    function ReadVec3Dummy: TG2Vec3;
    function ReadVec3Proc: TG2Vec3;
    function ReadVec4Dummy: TG2Vec4;
    function ReadVec4Proc: TG2Vec4;
    function ReadQuatDummy: TG2Quat;
    function ReadQuatProc: TG2Quat;
    function ReadMat4x4Dummy: TG2Mat;
    function ReadMat4x4Proc: TG2Mat;
    function ReadMat4x3Dummy: TG2Mat;
    function ReadMat4x3Proc: TG2Mat;
    function ReadColorDummy: TG2Color;
    function ReadColorProc: TG2Color;
    procedure WriteBufferDummy(const Buffer; const Size: Integer);
    procedure WriteBufferProc(const Buffer; const Size: Integer);
    procedure WriteBoolDummy(const Value: Boolean);
    procedure WriteBoolProc(const Value: Boolean);
    procedure WriteUInt1Dummy(const Value: Byte);
    procedure WriteUInt1Proc(const Value: Byte);
    procedure WriteSInt1Dummy(const Value: ShortInt);
    procedure WriteSInt1Proc(const Value: ShortInt);
    procedure WriteUInt2Dummy(const Value: Word);
    procedure WriteUInt2Proc(const Value: Word);
    procedure WriteSInt2Dummy(const Value: SmallInt);
    procedure WriteSInt2Proc(const Value: SmallInt);
    procedure WriteUInt4Dummy(const Value: DWord);
    procedure WriteUInt4Proc(const Value: DWord);
    procedure WriteSInt4Dummy(const Value: Integer);
    procedure WriteSInt4Proc(const Value: Integer);
    procedure WriteSInt8Dummy(const Value: Int64);
    procedure WriteSInt8Proc(const Value: Int64);
    procedure WriteFloat4Dummy(const Value: Single);
    procedure WriteFloat4Proc(const Value: Single);
    procedure WriteFloat8Dummy(const Value: Double);
    procedure WriteFloat8Proc(const Value: Double);
    procedure WriteStringNTDummy(const Value: AnsiString);
    procedure WriteStringNTProc(const Value: AnsiString);
    procedure WriteStringDummy(const Value: AnsiString);
    procedure WriteStringProc(const Value: AnsiString);
    procedure WriteWideStringNTDummy(const Value: WideString);
    procedure WriteWideStringNTProc(const Value: WideString);
    procedure WriteWideStringDummy(const Value: WideString);
    procedure WriteWideStringProc(const Value: WideString);
    procedure WriteVec2Dummy(const Value: TG2Vec2);
    procedure WriteVec2Proc(const Value: TG2Vec2);
    procedure WriteVec3Dummy(const Value: TG2Vec3);
    procedure WriteVec3Proc(const Value: TG2Vec3);
    procedure WriteVec4Dummy(const Value: TG2Vec4);
    procedure WriteVec4Proc(const Value: TG2Vec4);
    procedure WriteQuatDummy(const Value: TG2Quat);
    procedure WriteQuatProc(const Value: TG2Quat);
    procedure WriteMat4x4Dummy(const Value: TG2Mat);
    procedure WriteMat4x4Proc(const Value: TG2Mat);
    procedure WriteMat4x3Dummy(const Value: TG2Mat);
    procedure WriteMat4x3Proc(const Value: TG2Mat);
    procedure WriteColorDummy(const Value: TG2Color);
    procedure WriteColorProc(const Value: TG2Color);
    function GetPosition: Int64; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPosition(const Value: Int64); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCompression(const Value: Boolean);
    function GetSize: Int64; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDummy;
    procedure SetProcs;
  public
    property Stream: TStream read m_ws;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize;
    property Compression: Boolean read m_Compression write SetCompression;
    constructor Create;
    destructor Destroy; override;
    procedure OpenRead(const FileName: WideString);
    procedure OpenWrite(const FileName: WideString);
    procedure Close;
    function Seek(const Offset: Int64; Origin: TSeekOrigin = soCurrent): Int64;
    procedure ReadBuffer(var Buffer; const Size: Integer);
    function ReadBool: Boolean;
    function ReadUInt1: Byte;
    function ReadSInt1: ShortInt;
    function ReadUInt2: Word;
    function ReadSInt2: SmallInt;
    function ReadUInt4: DWord;
    function ReadSInt4: Integer;
    function ReadSInt8: Int64;
    function ReadFloat4: Single;
    function ReadFloat8: Double;
    function ReadStringNT: AnsiString;
    function ReadString: AnsiString;
    function ReadWideStringNT: WideString;
    function ReadWideString: WideString;
    function ReadVec2: TG2Vec2;
    function ReadVec3: TG2Vec3;
    function ReadVec4: TG2Vec4;
    function ReadQuat: TG2Quat;
    function ReadMat4x4: TG2Mat;
    function ReadMat4x3: TG2Mat;
    function ReadColor: TG2Color;
    procedure WriteBuffer(const Buffer; const Size: Integer);
    procedure WriteBool(const Value: Boolean);
    procedure WriteUInt1(const Value: Byte);
    procedure WriteSInt1(const Value: ShortInt);
    procedure WriteUInt2(const Value: Word);
    procedure WriteSInt2(const Value: SmallInt);
    procedure WriteUInt4(const Value: DWord);
    procedure WriteSInt4(const Value: Integer);
    procedure WriteSInt8(const Value: Int64);
    procedure WriteFloat4(const Value: Single);
    procedure WriteFloat8(const Value: Double);
    procedure WriteStringNT(const Value: AnsiString);
    procedure WriteString(const Value: AnsiString);
    procedure WriteWideStringNT(const Value: WideString);
    procedure WriteWideString(const Value: WideString);
    procedure WriteVec2(const Value: TG2Vec2);
    procedure WriteVec3(const Value: TG2Vec3);
    procedure WriteVec4(const Value: TG2Vec4);
    procedure WriteQuat(const Value: TG2Quat);
    procedure WriteMat4x4(const Value: TG2Mat);
    procedure WriteMat4x3(const Value: TG2Mat);
    procedure WriteColor(const Value: TG2Color);
    procedure Write(const Value: Boolean); overload;
    procedure Write(const Value: Byte); overload;
    procedure Write(const Value: ShortInt); overload;
    procedure Write(const Value: Word); overload;
    procedure Write(const Value: SmallInt); overload;
    procedure Write(const Value: DWord); overload;
    procedure Write(const Value: Integer); overload;
    procedure Write(const Value: Int64); overload;
    procedure Write(const Value: Single); overload;
    procedure Write(const Value: Double); overload;
    procedure Write(const Value: AnsiString); overload;
    procedure Write(const Value: WideString); overload;
    procedure Write(const Value: TG2Vec2); overload;
    procedure Write(const Value: TG2Vec3); overload;
    procedure Write(const Value: TG2Vec4); overload;
    procedure Write(const Value: TG2Quat); overload;
    procedure Write(const Value: TG2Mat); overload;
    procedure Write(const Value: TG2Color); overload;
  end;
//TG2FileRW END

//TG2Engine BEGIN
  TG2Engine = class (TG2HighClass)
  strict protected
    var m_Plugs: TG2List;
    var m_PlugClass: CG2PlugClass;
    var m_Powered: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    property PlugClass: CG2PlugClass read m_PlugClass;
    property Powered: Boolean read m_Powered write m_Powered;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function RequestPlug(
      const PlugClass: CG2PlugClass;
      const Plug: PG2Plug
    ): TG2Result;
    function ReleasePlug(
      const Plug: PG2Plug
    ): TG2Result;
  end;
//TG2Engine END

//TG2Timer BEGIN
  TG2Timer = class sealed (TG2Engine)
  strict private
    var m_Mode: TG2TimerMode;
    var m_Enabled: Boolean;
    var m_MaxFPS: Integer;
    var m_TargetUPS: Integer;
    var m_FullSpeed: Boolean;
    var m_QPCAvailable: Boolean;
    var m_FPS: Integer;
    var m_FPSUpdateTime: Int64;
    var m_FrameCount: Integer;
    var m_Frequency: Int64;
    var m_MaxRenderLag: Int64;
    var m_PrevRenderTime: Int64;
    var m_MaxUpdateLag: Int64;
    var m_PrevUpdateTime: Int64;
    var m_PrevIdle: TIdleEvent;
    var m_WndHandle: HWnd;
    var m_TotalFPS: Int64;
    var m_TotalFPSCount: Int64;
    var m_CS: TCriticalSection;
    var m_PerfCheckTime: Int64;
    var m_PerfCheckResult: Single;
    procedure Start;
    procedure Stop;
    procedure InitMode;
    procedure UnInitMode;
    procedure SetMode(const Value: TG2TimerMode);
    procedure SetEnabled(const Value: Boolean);
    procedure ResetTimer;
    function GetTime: Int64;
    function GetSpeedFactor: Single;
  private
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Mode: TG2TimerMode read m_Mode write SetMode;
    property Enabled: Boolean read m_Enabled write SetEnabled;
    property FPS: Integer read m_FPS;
    property SpeedFactor: Single read GetSpeedFactor;
    property MaxFPS: Integer read m_MaxFPS write m_MaxFPS;
    property TargetUPS: Integer read m_TargetUPS write m_TargetUPS;
    property FullSpeed: Boolean read m_FullSpeed write m_FullSpeed;
    property CS: TCriticalSection read m_CS;
    property PerfCheckResult: Single read m_PerfCheckResult;
    procedure OnTimer;
    function ThreadStart(const Proc: TG2ProcObj; const OnFinish: TG2ProcObj = nil): TG2Thread;
    procedure ThreadLock; //{$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure ThreadUnlock; //{$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PerfCheckStart;
    procedure PerfCheckEnd;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Timer END

//TG2Graphics BEGIN
  TG2Graphics = class sealed (TG2Engine)
  strict private
    var m_PlugTimer: TG2PlugTimer;
    var m_D3D9: IDirect3D9;
    var m_Device: IDirect3DDevice9;
    var m_Caps: TD3DCaps9;
    var m_Params: TG2GraphicsRunTimeParams;
    var m_Specs: TG2GraphicsSpecs;
    var m_RenderStates: TG2RenderStates;
    var m_SamplerStates: TG2SamplerStates;
    var m_TextureStageStates: TG2TextureStageStates;
    var m_Transforms: TG2Transforms;
    var m_Lights: TG2Lights;
    var m_Material: TD3DMaterial9;
    var m_ShaderLib: TG2ShaderLib;
    var m_SharedVB2D: TG2SharedVB2D;
    var m_Shared: TG2Shared;
    var m_SwapChains: TG2List;
    var m_DefaultRenderTarget: TG2SurfaceRT;
    var m_DefaultDepthStencil: TG2SurfaceDS;
    var m_DefaultSwapChain: TG2SwapChain;
    var m_DefaultViewPort: TD3DViewPort9;
    var m_DeviceLost: Boolean;
    var m_InitGamma: Integer;
    var m_RestoreGamma: Boolean;
    var m_CurSurfaceRT: TG2SurfaceRT;
    var m_CurSurfaceDS: TG2SurfaceDS;
    var m_WndPosX: Integer;
    var m_WndPosY: Integer;
    var m_WndWidth: Integer;
    var m_WndHeight: Integer;
    var m_Rec: Boolean;
    var m_RecWidth: DWord;
    var m_RecHeight: DWord;
    var m_RecSurfaceOff: IDirect3DSurface9;
    var m_RecAVIFile: PAVIFile;
    var m_RecBMPInfo: TBitmapInfoHeader;
    var m_RecBMPFile: TBitmapFileHeader;
    var m_RecAVIStream: PAVIStream;
    var m_RecAVIStreamC: PAVIStream;
    var m_RecFramesRecorded: Integer;
    var m_RecTimeStart: DWord;
    var m_RecTimerInterval: DWord;
    procedure RecCatchFrame;
    function ScreenGrab: ID3DXBuffer;
    function InitializeDevice: TG2Result;
    function FinalizeDevice: TG2Result;
    function LostDevice: TG2Result;
    function ResetDevice: TG2Result;
    procedure UpdatePresentParams;
    procedure Update;
    function GetCanRender: Boolean;
    procedure SetGamma(const Value: Integer);
    function GetGamma: Integer;
  private
    var m_InitParams: TG2GraphicsInitParams;
    var m_InitParamsActual: TG2GraphicsInitParams;
    var m_PresentParams: TD3DPresentParameters;
    var m_CurDefViewPort: PD3DViewPort9;
    var m_CurSwapChain: TG2SwapChain;
    procedure HardReset;
    procedure QuickReset;
    procedure ParamsChange;
  public
    constructor Create; override;
    destructor Destroy; override;
    property D3D9: IDirect3D9 read m_D3D9;
    property Device: IDirect3DDevice9 read m_Device;
    property InitParams: TG2GraphicsInitParams read m_InitParams;
    property Params: TG2GraphicsRunTimeParams read m_Params;
    property Caps: TD3DCaps9 read m_Caps;
    property Specs: TG2GraphicsSpecs read m_Specs;
    property RenderStates: TG2RenderStates read m_RenderStates;
    property SamplerStates: TG2SamplerStates read m_SamplerStates;
    property TextureStageStages: TG2TextureStageStates read m_TextureStageStates;
    property Transforms: TG2Transforms read m_Transforms;
    property Lights: TG2Lights read m_Lights;
    property ShaderLib: TG2ShaderLib read m_ShaderLib;
    property SharedVB2D: TG2SharedVB2D read m_SharedVB2D;
    property Shared: TG2Shared read m_Shared;
    property Gamma: Integer read GetGamma write SetGamma;
    property CanRender: Boolean read GetCanRender;
    property Recording: Boolean read m_Rec;
    function SwapChainAdd(const Handle: HWND; const SurfaceWidth, SurfaceHeight: Word): TG2SwapChain; overload;
    procedure SwapChainRemove(const SwapChain: TG2SwapChain);
    procedure SetViewPortDefault;
    procedure SetViewPort(const X, Y, Width, Height: Integer); overload;
    procedure SetViewPort(const ViewPort: TD3DViewPort9); overload;
    function GetViewPort: TD3DViewPort9;
    procedure SetMaterialDefault;
    procedure SetMaterialFromMemory;
    procedure SetMaterial(
      const Diffuse: TG2Color;
      const Ambient: TG2Color;
      const Specular: TG2Color;
      const Emissive: TG2Color;
      const SpecPower: Single
    );
    procedure SetRenderTargetSurface(const Surface: TG2SurfaceRT);
    procedure SetRenderTargetTexture2D(const Texture: TG2Texture2DRT);
    procedure SetRenderTargetSwapChain(const SwapChain: TG2SwapChain);
    procedure SetDepthStencilSurface(const Surface: TG2SurfaceDS);
    procedure SetDepthStencilTexture2D(const Texture: TG2Texture2DDS);
    procedure SetRenderTargetDefault;
    procedure SetDepthStencilDefault;
    procedure PresentToBackBuffer;
    procedure PresentToSwapChain(const SwapChain: TG2SwapChain);
    procedure SaveScreenshot(const FileName: WideString);
    procedure RecStart(const FileName: WideString; const RecWidth: Integer = 0; const RecHeight: Integer = 0; const Quality: Integer = 6000);
    procedure RecStop;
    function PointToRay(const Pt: TPoint): TG2Ray;
    function PointTo3D(const Pt: TG2Vec3): TG2Vec3;
    function PointTo2D(const Pt: TG2Vec3): TG2Vec3;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Graphics END

//TG2GraphicsSpecs BEGIN
  TG2GraphicsSpecs = class sealed
  strict private
    type TFormatScore = record
      var Format: TD3DFormat;
      var Score: DWord;
    end;
    type TFormatScoreArray = array[0..0] of TFormatScore;
    type PFormatScoreArray = ^TFormatScoreArray;
    type TResolution = record
    public
      var AspX, AspY: Integer;
      var Width: DWord;
      var Height: DWord;
      var RefreshRates: array of DWord;
    end;
    type PResolution = ^TResolution;
    var m_Resolutions: array of TResolution;
    var m_ResolutionsComp: array of TResolution;
    var m_Antialias: array of Integer;
  const
    FormatScoresTexture: array[0..8] of TFormatScore = (
      (Format: D3DFMT_DXT1; Score: 10),
      (Format: D3DFMT_DXT2; Score: 19),
      (Format: D3DFMT_DXT3; Score: 31),
      (Format: D3DFMT_DXT4; Score: 41),
      (Format: D3DFMT_DXT5; Score: 49),
      (Format: D3DFMT_A4R4G4B4; Score: 59),
      (Format: D3DFMT_A8R8G8B8; Score: 71),
      (Format: D3DFMT_X8R8G8B8; Score: 79),
      (Format: D3DFMT_R5G6B5; Score: 90)
    );
    FormatScoresRenderTarget: array[0..12] of TFormatScore = (
      (Format: D3DFMT_R3G3B2; Score: 19),
      (Format: D3DFMT_X4R4G4B4; Score: 35),
      (Format: D3DFMT_X1R5G5B5; Score: 37),
      (Format: D3DFMT_R5G6B5; Score: 40),
      (Format: D3DFMT_R8G8B8; Score: 41),
      (Format: D3DFMT_X8B8G8R8; Score: 48),
      (Format: D3DFMT_A8B8G8R8; Score: 50),
      (Format: D3DFMT_A4R4G4B4; Score: 54),
      (Format: D3DFMT_A1R5G5B5; Score: 55),
      (Format: D3DFMT_A16B16G16R16; Score: 58),
      (Format: D3DFMT_A2R10G10B10; Score: 62),
      (Format: D3DFMT_A2B10G10R10; Score: 64),
      (Format: D3DFMT_A8R3G3B2; Score: 70)
    );
    FormatScoresDepthStencil: array[0..3] of TFormatScore = (
      (Format: D3DFMT_D16; Score: 10),
      (Format: D3DFMT_D24X8; Score: 20),
      (Format: D3DFMT_D32; Score: 28),
      (Format: D3DFMT_D24S8; Score: 34)
    );
    function FindFormat(
      const Format: TD3DFormat;
      const Usage: DWord;
      const ResType: TD3DResourceType;
      const FormatArray: PFormatScoreArray;
      const FormatCount: Integer;
      const InitScore: Integer;
      const DefaultFormat: TD3DFormat
    ): TD3DFormat;
    procedure FindResolutions;
    function GetResolution(const Index: Integer): PResolution; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetResolutionCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetResolutionComp(const Index: Integer): PResolution; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetResolutionCompCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAntialiasSamples(const Index: Integer): Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAntialiasCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var m_Gfx: TG2Graphics;
    var m_List: TG2SortedList;
    var m_DisplayMode: TD3DDisplayMode;
  public
    constructor Create(const G2Graphics: TG2Graphics);
    destructor Destroy; override;
    procedure Initialize;
    property Resolutions[const Index: Integer]: PResolution read GetResolution;
    property ResolutionCount: Integer read GetResolutionCount;
    property CompatiableResolutions[const Index: Integer]: PResolution read GetResolutionComp;
    property CompatiableResolutionCount: Integer read GetResolutionCompCount;
    property AntialiasSamples[const Index: Integer]: Integer read GetAntialiasSamples;
    property AntialiasCount: Integer read GetAntialiasCount;
    function FindCompatiableTexture2DFormat(const Format: TD3DFormat): TD3DFormat;
    function FindCompatiableTexture2DRTFormat(const Format: TD3DFormat): TD3DFormat;
    function FindCompatiableTexture2DDSFormat(const Format: TD3DFormat): TD3DFormat;
    function FindCompatiableTextureCubeFormat(const Format: TD3DFormat): TD3DFormat;
    function FindCompatiableTextureCubeRTFormat(const Format: TD3DFormat): TD3DFormat;
    function FindCompatiableTextureVolumeFormat(const Format: TD3DFormat): TD3DFormat;
    function FindCompatiableSurfaceRTFormat(const Format: TD3DFormat): TD3DFormat;
    function FindCompatiableSurfaceDSFormat(const Format: TD3DFormat): TD3DFormat;
    procedure FindAsp(const Width, Height: Integer; var X, Y: Integer);
    function GetVRAM: DWord;
    function GetVRAMFree: DWord;
  end;
//TG2GRaphicsSpecs END

//TG2GraphicsInitParams BEGIN
  TG2GraphicsInitParams = class sealed (TG2Class)
  strict private
    var m_Adapter: Word;
    var m_DeviceType: TD3DDevType;
    var m_Width: Word;
    var m_Height: Word;
    var m_FormatBackBuffer: TD3DFormat;
    var m_FormatSurfaceRT: TD3DFormat;
    var m_FormatSurfaceDS: TD3DFormat;
    var m_FormatTexture2D: TD3DFormat;
    var m_FormatTexture2DRT: TD3DFormat;
    var m_FormatTexture2DDS: TD3DFormat;
    var m_FormatTextureCube: TD3DFormat;
    var m_FormatTextureCubeRT: TD3DFormat;
    var m_FormatTextureVolume: TD3DFormat;
    var m_FullScreen: Boolean;
    var m_VSync: Boolean;
    var m_Antialiasing: Boolean;
    var m_AntialiasingSampleCount: Word;
    var m_VertexProcessing: TG2VertexProcessing;
    var m_MultiThreaded: Boolean;
    var m_PureDevice: Boolean;
  private
    var m_Gfx: TG2Graphics;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Adapter: Word read m_Adapter write m_Adapter;
    property DeviceType: TD3DDevType read m_DeviceType write m_DeviceType;
    property Width: Word read m_Width write m_Width;
    property Height: Word read m_Height write m_Height;
    property FormatBackBuffer: TD3DFormat read m_FormatBackBuffer write m_FormatBackBuffer;
    property FormatSurfaceRT: TD3DFormat read m_FormatSurfaceRT write m_FormatSurfaceRT;
    property FormatSurfaceDS: TD3DFormat read m_FormatSurfaceDS write m_FormatSurfaceDS;
    property FormatTexture2D: TD3DFormat read m_FormatTexture2D write m_FormatTexture2D;
    property FormatTexture2DRT: TD3DFormat read m_FormatTexture2DRT write m_FormatTexture2DRT;
    property FormatTexture2DDS: TD3DFormat read m_FormatTexture2DDS write m_FormatTexture2DDS;
    property FormatTextureCube: TD3DFormat read m_FormatTextureCube write m_FormatTextureCube;
    property FormatTextureCubeRT: TD3DFormat read m_FormatTextureCubeRT write m_FormatTextureCubeRT;
    property FormatTextureVolume: TD3DFormat read m_FormatTextureVolume write m_FormatTextureVolume;
    property FullScreen: Boolean read m_FullScreen write m_FullScreen;
    property VSync: Boolean read m_VSync write m_VSync;
    property Antialiasing: Boolean read m_Antialiasing write m_Antialiasing;
    property AntialiasingSampleCount: Word read m_AntialiasingSampleCount write m_AntialiasingSampleCount;
    property VertexProcessing: TG2VertexProcessing read m_VertexProcessing write m_VertexProcessing;
    property MultiThreaded: Boolean read m_MultiThreaded write m_MultiThreaded;
    property PureDevice: Boolean read m_PureDevice write m_PureDevice;
    function Verify: TG2Result;
    procedure Defaults;
    procedure Clone(const Params: TG2GraphicsInitParams);
  end;
//TG2GraphicsInitParams END

//TG2GraphicsRunTimeParams BEGIN
  TG2GraphicsRunTimeParams = class sealed (TG2Class)
  strict private
    var m_Adapter: Word;
    var m_DeviceType: TD3DDevType;
    var m_Width: Word;
    var m_Height: Word;
    var m_FormatBackBuffer: TD3DFormat;
    var m_FormatSurfaceRT: TD3DFormat;
    var m_FormatSurfaceDS: TD3DFormat;
    var m_FormatTexture2D: TD3DFormat;
    var m_FormatTexture2DRT: TD3DFormat;
    var m_FormatTexture2DDS: TD3DFormat;
    var m_FormatTextureCube: TD3DFormat;
    var m_FormatTextureCubeRT: TD3DFormat;
    var m_FormatTextureVolume: TD3DFormat;
    var m_FullScreen: Boolean;
    var m_VSync: Boolean;
    var m_Antialiasing: Boolean;
    var m_AntialiasingSampleCount: Word;
    var m_VertexProcessing: TG2VertexProcessing;
    var m_MultiThreaded: Boolean;
    var m_PureDevice: Boolean;
  private
    var m_Gfx: TG2Graphics;
    procedure Assign(const Params: TG2GraphicsInitParams);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Adapter: Word read m_Adapter;
    property DeviceType: TD3DDevType read m_DeviceType;
    property Width: Word read m_Width write m_Width;
    property Height: Word read m_Height write m_Height;
    property FormatBackBuffer: TD3DFormat read m_FormatBackBuffer write m_FormatBackBuffer;
    property FormatSurfaceRT: TD3DFormat read m_FormatSurfaceRT write m_FormatSurfaceRT;
    property FormatSurfaceDS: TD3DFormat read m_FormatSurfaceDS write m_FormatSurfaceDS;
    property FormatTexture2D: TD3DFormat read m_FormatTexture2D write m_FormatTexture2D;
    property FormatTexture2DRT: TD3DFormat read m_FormatTexture2DRT write m_FormatTexture2DRT;
    property FormatTexture2DDS: TD3DFormat read m_FormatTexture2DDS write m_FormatTexture2DDS;
    property FormatTextureCube: TD3DFormat read m_FormatTextureCube write m_FormatTextureCube;
    property FormatTextureCubeRT: TD3DFormat read m_FormatTextureCubeRT write m_FormatTextureCubeRT;
    property FormatTextureVolume: TD3DFormat read m_FormatTextureVolume write m_FormatTextureVolume;
    property FullScreen: Boolean read m_FullScreen write m_FullScreen;
    property VSync: Boolean read m_VSync write m_VSync;
    property Antialiasing: Boolean read m_Antialiasing write m_Antialiasing;
    property AntialiasingSampleCount: Word read m_AntialiasingSampleCount write m_AntialiasingSampleCount;
    property VertexProcessing: TG2VertexProcessing read m_VertexProcessing;
    property MutiThreaded: Boolean read m_MultiThreaded;
    property PureDevice: Boolean read m_PureDevice;
    function Apply: TG2Result;
  end;
//TG2GraphicsRunTimeParams END

//TG2Transforms BEGIN
  TG2Transforms = class sealed (TG2HighClass)
  strict private
    var m_W: array[0..255] of TG2Mat;
    var m_V: TG2Mat;
    var m_P: TG2Mat;
    var m_T: array[0..15] of TG2Mat;
    var m_BufferW: array of TG2Mat;
    var m_BufferV: array of TG2Mat;
    var m_BufferP: array of TG2Mat;
    var m_BufferT: array of TG2Mat;
    var m_Frustum: TG2Frustum;
    function GetW(const Index: Byte): TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetW(const Index: Byte; const m: TG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetV(const m: TG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetP(const m: TG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetT(const Index: Byte): TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetT(const Index: Byte; const m: TG2Mat); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWV: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVP: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWVP: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWt: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVt: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPt: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWVt: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVPt: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWVPt: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWi: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVi: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPi: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWVi: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVPi: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWVPi: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVpos: TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVdir: TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVb: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  public
    constructor Create; override;
    destructor Destroy; override;
    property W[const Index: Byte]: TG2Mat read GetW write SetW;
    property V: TG2Mat read m_V write SetV;
    property P: TG2Mat read m_P write SetP;
    property T[const Index: Byte]: TG2Mat read GetT write SetT;
    property WV: TG2Mat read GetWV;
    property VP: TG2Mat read GetVP;
    property WVP: TG2Mat read GetWVP;
    property Wt: TG2Mat read GetWt;
    property Vt: TG2Mat read GetVt;
    property Pt: TG2Mat read GetPt;
    property WVt: TG2Mat read GetWVt;
    property VPt: TG2Mat read GetVPt;
    property WVPt: TG2Mat read GetWVPt;
    property Wi: TG2Mat read GetWi;
    property Vi: TG2Mat read GetVi;
    property Pi: TG2Mat read GetPi;
    property WVi: TG2Mat read GetWVi;
    property VPi: TG2Mat read GetVPi;
    property WVPi: TG2Mat read GetWVPi;
    property Vpos: TG2Vec3 read GetVpos;
    property Vdir: TG2Vec3 read GetVdir;
    property Vb: TG2Mat read GetVb;
    property Frustum: TG2Frustum read m_Frustum;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    procedure ApplyW(const Index: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure ApplyV; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure ApplyP; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure ApplyT(const Index: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure UpdateW(const Index: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure UpdateV; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure UpdateP; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure UpdateT(const Index: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PushW;
    procedure PushV;
    procedure PushP;
    procedure PushT;
    procedure PopW;
    procedure PopV;
    procedure PopP;
    procedure PopT;
  end;
//TG2Transforms END

//TG2Lights BEGIN
  TG2Lights = class sealed (TG2HighClass)
  strict private
    const LCount = 8;
    type TG2LightType = (
      ltAmbient,
      ltDirectional,
      ltPoint,
      ltSpot
    );
    type TG2Light = record
    private
      var m_Lights: TG2Lights;
      var m_Index: Byte;
    public
      var Light: TD3DLight9;
      var LightType: TG2LightType;
      var Enabled: Boolean;
      procedure SetAmbientLight(
        const Ambient: TG2Color
      );
      procedure SetPointLight(
        const PosX, PosY, PosZ: Single;
        const Diffuse: TG2Color;
        const Range: Single
      );
      procedure SetDirectionalLight(
        const DirX, DirY, DirZ: Single;
        const Diffuse: TG2Color
      );
      procedure SetToDevice;
    end;
    type PG2Light = ^TG2Light;
    var m_Lights: array[0..LCount - 1] of TG2Light;
    function GetLight(const Index: Byte): PG2Light;
    function GetLightCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var m_LightsBuffer: array[0..LCount - 1] of TD3DLight9;
    var m_LightsEnabled: array[0..LCount - 1] of Boolean;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Lights[const Index: Byte]: PG2Light read GetLight; default;
    property LightCount: Integer read GetLightCount;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    procedure DisableLights;
    procedure SetToEffect(const Effect: TG2Effect);
  end;
//TG2Lights END

//TG2SwapChain BEGIN
  TG2SwapChain = class sealed (TG2Class)
  strict private
    var m_SwapChain: IDirect3DSwapChain9;
    var m_RenderTarget: TG2SurfaceRT;
    var m_PresentParams: TD3DPresentParameters;
    procedure Release;
  private
    var m_Gfx: TG2Graphics;
    procedure Initialize(
      const Handle: HWND;
      const Width, Height: Word;
      const PresentParams: TD3DPresentParameters
    ); overload;
    procedure Reset(const Width, Height: Word);
    procedure OnLostDevice;
    procedure OnResetDevice;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Present;
    property SwapChain: IDirect3DSwapChain9 read m_SwapChain;
    property RenderTarget: TG2SurfaceRT read m_RenderTarget;
  end;
//TG2SwapChain END

//TG2Audio BEGIN
  TG2Audio = class sealed (TG2Engine)
  strict private
    var m_Volume: Single;
    procedure SetVolume(Value: Single);
  private
    var m_Loader: IDirectMusicLoader8;
    var m_Performance: IDirectMusicPerformance8;
    var m_MusicMgrs: TList;
    var m_SoundMgrs: TList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Volume: Single read m_Volume write SetVolume;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Audio END

//TG2Input BEGIN
  TG2Input = class sealed (TG2Engine)
  strict private
    var m_DI: IDirectInput8;
    var m_PlugTimer: TG2PlugTimer;
    var m_KeyboardAquired: Boolean;
    var m_MouseAquired: Boolean;
    var m_Keyboard: IDirectInputDevice8;
    var m_Mouse: IDirectInputDevice8;
    var m_KeyState: array[0..255] of Boolean;
    var m_MouseState: TDIMouseState;
    var m_TopHandle: HWnd;
    var m_PrevWndProc: Pointer;
    procedure Update;
    procedure KeyDown(const Key: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure KeyUp(const Key: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseDown(const Button: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseUp(const Button: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseMove(const Shift: TPoint); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure WheelMove(const Shift: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    property PrevWndProc: Pointer read m_PrevWndProc;
    procedure KeyPress(const Key: AnsiChar);
    function GetKey(const Index: Byte): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetButton(const Index: Byte): Boolean;
    function GetMousePos: TPoint; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMouseShift: TPoint; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWheelShift: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Input END

//TG2Network BEGIN
  TG2Network = class sealed (TG2Engine)
  strict private
    var m_SockData: TWSAData;
    function GetSockData: PWSAData; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property SockData: PWSAData read GetSockData;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function HTTPRequestGET(const Host, Params: AnsiString; var Response: AnsiString): TG2Result;
  end;
//TG2Network END

//TG2Module BEGIN
  TG2Module = class (TG2HighClass)
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Module END

//TG2Plug BEGIN
  TG2Plug = class (TG2HighClass)
  strict private
    var m_Engine: TG2Engine;
    var m_ParentMod: TG2Module;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Engine: TG2Engine read m_Engine write m_Engine;
    property ParentMod: TG2Module read m_ParentMod write m_ParentMod;
  end;
//TG2Plug END

//TG2PlugTimer BEGIN
  TG2PlugTimer = class sealed (TG2Plug)
  strict private
    var m_OnTimer: TG2ProcObj;
    var m_OnUpdate: TG2ProcObj;
    var m_OnRender: TG2ProcObj;
  public
    constructor Create; override;
    destructor Destroy; override;
    property OnTimer: TG2ProcObj read m_OnTimer write m_OnTimer;
    property OnUpdate: TG2ProcObj read m_OnUpdate write m_OnUpdate;
    property OnRender: TG2ProcObj read m_OnRender write m_OnRender;
  end;
//TG2PlugTimer END

//TG2PlugGraphics BEGIN
  TG2PlugGraphics = class sealed (TG2Plug)
  strict private
    var m_OnDeviceLost: TG2ProcObj;
    var m_OnDeviceReset: TG2ProcObj;
    var m_OnParamsChange: TG2ProcObj;
  public
    constructor Create; override;
    destructor Destroy; override;
    property OnDeviceLost: TG2ProcObj read m_OnDeviceLost write m_OnDeviceLost;
    property OnDeviceReset: TG2ProcObj read m_OnDeviceReset write m_OnDeviceReset;
    property OnParamsChange: TG2ProcObj read m_OnParamsChange write m_OnParamsChange;
  end;
//TG2PlugGraphics END

//TG2PlugAudio BEGIN
  TG2PlugAudio = class sealed (TG2Plug)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
//TG2PlugAudio END

//TG2PlugInput BEGIN
  TG2PlugInput = class sealed (TG2Plug)
  strict private
    var m_Input: TG2Input;
    var m_OnKeyDown: TG2InputKeyDown;
    var m_OnKeyUp: TG2InputKeyUp;
    var m_OnKeyPress: TG2InputKeyPress;
    var m_OnMouseDown: TG2InputMouseDown;
    var m_OnMouseUp: TG2InputMouseUp;
    var m_OnMouseMove: TG2InputMouseMove;
    var m_OnWheelMove: TG2InputWheelMove;
    function GetKeyDown(const Index: Byte): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMouseDown(const Index: Byte): Boolean;
    function GetMousePos: TPoint; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMouseShift: TPoint; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWheelShift: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    property OnKeyDown: TG2InputKeyDown read m_OnKeyDown write m_OnKeyDown;
    property OnKeyUp: TG2InputKeyUp read m_OnKeyUp write m_OnKeyUp;
    property OnKeyPress: TG2InputKeyPress read m_OnKeyPress write m_OnKeyPress;
    property OnMouseDown: TG2InputMouseDown read m_OnMouseDown write m_OnMouseDown;
    property OnMouseUp: TG2InputMouseUp read m_OnMouseUp write m_OnMouseUp;
    property OnMouseMove: TG2InputMouseMove read m_OnMouseMove write m_OnMouseMove;
    property OnWheelMove: TG2InputWheelMove read m_OnWheelMove write m_OnWheelMove;
    property KeyDown[const Index: Byte]: Boolean read GetKeyDown;
    property MouseDown[const Index: Byte]: Boolean read GetMouseDown;
    property MousePos: TPoint read GetMousePos;
    property MouseShift: TPoint read GetMouseShift;
    property WheelShift: Integer read GetWheelShift;
    function KeyToChar(const Key: Byte): AnsiChar;
  end;
//TG2PlugInput END

//TG2PlugNetwork BEGIN
  TG2PlugNetwork = class sealed (TG2Plug)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
//TG2PlugNetwork END

//TG2ResMgr BEGIN
  TG2ResMgr = class (TG2Module)
  strict protected
    var m_Resources: TG2QuickList;
    function GetCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  protected
    function FindResourceIndex(const NameCache: PWordArray; const Len: Integer): Integer;
    function FindResource(const Name: WideString): TG2Res;
    procedure AddResource(const Res: TG2Res); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RemoveResource(const Res: TG2Res); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure DeleteResource(const Index: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure FreeResources; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2ResMgr END

//TG2Res BEGIN
  TG2Res = class (TG2HighClass)
  strict private
    var m_Name: WideString;
    procedure SetName(const Value: WideString);
  private
    var NameCache: array of Word;
    var Mgrs: TG2QuickList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Name: WideString read m_Name write SetName;
  end;
//TG2Res END

//TG2MusicMgr BEGIN
  TG2MusicMgr = class sealed (TG2ResMgr)
  private
    procedure ResetVolume;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function FindMusic(const Name: WideString): TG2Music;
    function CreateMusic(const Name: WideString): TG2Music;
  end;
//TG2MusicMgr END

//TG2Music BEGIN
  TG2Music = class sealed (TG2Res)
  strict private
    var m_G2Audio: TG2Audio;
    var m_Graph: IGraphBuilder;
    var m_Control: IMediaControl;
    var m_Event: IMediaEventEx;
    var m_Position: IMediaPosition;
    var m_Seeking: IMediaSeeking;
    var m_Filter: IMediaFilter;
    var m_Audio: IBasicAudio;
    var m_Open: Boolean;
    var m_IsPlaying: Boolean;
    var m_PlayTime: DWord;
    procedure ClearGraph;
    procedure SetPosition(const Value: Int64);
    function GetPosition: Int64;
    function GetDuration: Int64;
    procedure SetVolume(Value: Single);
    function GetVolume: Single;
    procedure SetPan(Value: Single);
    function GetPan: Single;
    procedure SetPlayRate(Value: Single);
    function GetPlayRate: Single;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Position: Int64 read GetPosition write SetPosition;
    property Duration: Int64 read GetDuration;
    property Volume: Single read GetVolume write SetVolume;
    property Pan: Single read GetPan write SetPan;
    property PlayRate: Single read GetPlayRate write SetPlayRate;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    procedure OpenFile(const f: WideString);
    procedure Close;
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Rewind;
    function IsPlaying: Boolean;
  end;
//TG2Music END

//TG2SoundMgr BEGIN
  TG2SoundMgr = class sealed (TG2ResMgr)
  private
    procedure ResetVolume;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function FindSound(const Name: WideString): TG2Sound;
    function CreateSound(const Name: WideString): TG2Sound;
    function CreateSoundFromFile(const Name, f: WideString): TG2Sound;
  end;
//TG2SoundMgr END

//TG2Sound BEGIN
  TG2Sound = class sealed (TG2Res)
  strict private
    var m_Buffer3D: IDirectSound3DBuffer8;
    var m_Buffer: IDirectSoundBuffer;
    var m_Listener: IDirectSound3DListener8;
    var m_AudioPath: IDirectMusicAudioPath;
    var m_Volume: Single;
    var m_Frequency: DWORD;
    var m_Enable3D: Boolean;
    var m_PlayTime: DWord;
    procedure SetVolume(Value: Single);
    procedure SetSoundPosition(Value: TG2Vec3);
    function GetSoundPosition: TG2Vec3;
    procedure SetSoundVelocity(Value: TG2Vec3);
    function GetSoundVelocity: TG2Vec3;
    procedure SetSoundDirection(Value: TG2Vec3);
    function GetSoundDirection: TG2Vec3;
    procedure SetListenerPosition(Value: TG2Vec3);
    function GetListenerPosition: TG2Vec3;
    procedure SetListenerVelocity(Value: TG2Vec3);
    function GetListenerVelocity: TG2Vec3;
    procedure SetListenerDirection(Value: TG2Vec3);
    function GetListenerDirection: TG2Vec3;
    procedure SetMinDistance(Value: Single);
    function GetMinDistance: Single;
    procedure SetMaxDistance(Value: Single);
    function GetMaxDistance: Single;
    procedure SetDistanceFactor(Value: Single);
    function GetDistanceFactor: Single;
    procedure SetDopplerFactor(Value: Single);
    function GetDopplerFactor: Single;
    procedure SetRollOffFactor(Value: Single);
    function GetRollOffFactor: Single;
    procedure SetPlayRate(Value: Single);
    function GetPlayRate: Single;
    procedure SetEnable3D(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var m_G2Audio: TG2Audio;
    var m_Loaded: Boolean;
    var m_Segment: IDirectMusicSegment8;
    var m_Instances: TList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Volume: Single read m_Volume write SetVolume;
    property SoundPosition: TG2Vec3 read GetSoundPosition write SetSoundPosition;
    property SoundVelocity: TG2Vec3 read GetSoundVelocity write SetSoundVelocity;
    property SoundDirection: TG2Vec3 read GetSoundDirection write SetSoundDirection;
    property ListenerPosition: TG2Vec3 read GetListenerPosition write SetListenerPosition;
    property ListenerVelocity: TG2Vec3 read GetListenerVelocity write SetListenerVelocity;
    property ListenerDirection: TG2Vec3 read GetListenerDirection write SetListenerDirection;
    property MinDistance: Single read GetMinDistance write SetMinDistance;
    property MaxDistance: Single read GetMaxDistance write SetMaxDistance;
    property DistanceFactor: Single read GetDistanceFactor write SetDistanceFactor;
    property DopplerFactor: Single read GetDopplerFactor write SetDopplerFactor;
    property RollOffFactor: Single read GetRollOffFactor write SetRollOffFactor;
    property PlayRate: Single read GetPlayRate write SetPlayRate;
    property Enable3D: Boolean read m_Enable3D write SetEnable3D;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function LoadFile(const f: WideString): TG2Result;
    procedure UnLoad;
    procedure Play;
    procedure Stop;
    function IsPlaying: Boolean;
    function CreateInstance: TG2SoundInst;
  end;
//TG2Sound END

//TG2SoundInst BEGIN
  TG2SoundInst = class sealed (TG2Class)
  strict private
    var m_Buffer3D: IDirectSound3DBuffer8;
    var m_Buffer: IDirectSoundBuffer;
    var m_Listener: IDirectSound3DListener8;
    var m_AudioPath: IDirectMusicAudioPath;
    var m_Loaded: Boolean;
    var m_Volume: Single;
    var m_Frequency: DWORD;
    procedure SetVolume(Value: Single);
    procedure SetSoundPosition(Value: TG2Vec3);
    function GetSoundPosition: TG2Vec3;
    procedure SetSoundVelocity(Value: TG2Vec3);
    function GetSoundVelocity: TG2Vec3;
    procedure SetSoundDirection(Value: TG2Vec3);
    function GetSoundDirection: TG2Vec3;
    procedure SetListenerPosition(Value: TG2Vec3);
    function GetListenerPosition: TG2Vec3;
    procedure SetListenerVelocity(Value: TG2Vec3);
    function GetListenerVelocity: TG2Vec3;
    procedure SetListenerDirection(Value: TG2Vec3);
    function GetListenerDirection: TG2Vec3;
    procedure SetMinDistance(Value: Single);
    function GetMinDistance: Single;
    procedure SetMaxDistance(Value: Single);
    function GetMaxDistance: Single;
    procedure SetDistanceFactor(Value: Single);
    function GetDistanceFactor: Single;
    procedure SetDopplerFactor(Value: Single);
    function GetDopplerFactor: Single;
    procedure SetRollOffFactor(Value: Single);
    function GetRollOffFactor: Single;
    procedure SetPlayRate(Value: Single);
    function GetPlayRate: Single;
  private
    var m_G2Sound: TG2Sound;
    procedure Initialize;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Volume: Single read m_Volume write SetVolume;
    property SoundPosition: TG2Vec3 read GetSoundPosition write SetSoundPosition;
    property SoundVelocity: TG2Vec3 read GetSoundVelocity write SetSoundVelocity;
    property SoundDirection: TG2Vec3 read GetSoundDirection write SetSoundDirection;
    property ListenerPosition: TG2Vec3 read GetListenerPosition write SetListenerPosition;
    property ListenerVelocity: TG2Vec3 read GetListenerVelocity write SetListenerVelocity;
    property ListenerDirection: TG2Vec3 read GetListenerDirection write SetListenerDirection;
    property MinDistance: Single read GetMinDistance write SetMinDistance;
    property MaxDistance: Single read GetMaxDistance write SetMaxDistance;
    property DistanceFactor: Single read GetDistanceFactor write SetDistanceFactor;
    property DopplerFactor: Single read GetDopplerFactor write SetDopplerFactor;
    property RollOffFactor: Single read GetRollOffFactor write SetRollOffFactor;
    property PlayRate: Single read GetPlayRate write SetPlayRate;
    procedure Play;
    procedure Stop;
    function IsPlaying: Boolean;
  end;
//TG2SoundInst END

//TG2TextureMgr BEGIN
  TG2TextureMgr = class sealed (TG2ResMgr)
  strict private
    var m_PlugGraphics: TG2PlugGraphics;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
    function GetTexture(const Index: Integer): TG2TextureBase; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    property Textures[const Index: Integer]: TG2TextureBase read GetTexture; default;
    constructor Create; override;
    destructor Destroy; override;
    function CreateTexture2D(
      const Name: WideString;
      const Width, Height: Integer;
      const MipLevels: Integer = 8;
      const Usage: DWord = 0;
      const Format: TD3DFormat = D3DFMT_UNKNOWN;
      const Pool: TD3DPool = D3DPOOL_MANAGED
    ): TG2Texture2D;
    function CreateTexture2DFromFile(
      const Name: WideString;
      const f: WideString;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
    function CreateTexture2DFromStream(
      const Name: WideString;
      const s: TMemoryStream;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
    function CreateTexture2DFromBuffer(
      const Name: WideString;
      const b: Pointer;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
    function CreateTexture2DFromGraphic(
      const Name: WideString;
      const g: TGraphic;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
    function CreateTexture2DFromPack(
      const Name: WideString;
      const FolderName, FileName: AnsiString;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
    function CreateTexture2DFromTexture2D(
      const Name: WideString;
      const Tex: TG2Texture2DBase;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
    function CreateTexture2DNormalMap(
      const Name: WideString;
      const HeightMap: TG2Texture2D;
      const Amplitude: Single = 1;
      const MipLevels: Integer = 8;
      const Channel: DWord = D3DX_CHANNEL_ALPHA;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
    function CreateTexture2DRT(
      const Name: WideString;
      const Width, Height: Integer;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2DRT;
    function CreateTexture2DDS(
      const Name: WideString;
      const Width, Height: Integer
    ): TG2Texture2DDS;
    function CreateTexture2DVideo(
      const Name: WideString;
      const f: WideString
    ): TG2Texture2DVideo;
    function CreateTextureCube(
      const Name: WideString;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const Usage: DWord = 0;
      const Format: TD3DFormat = D3DFMT_UNKNOWN;
      const Pool: TD3DPool = D3DPOOL_MANAGED
    ): TG2TextureCube;
    function CreateTextureCubeFromFile(
      const Name: WideString;
      const f: WideString;
      const MipLevels: Integer = 1;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2TextureCube;
    function CreateTextureCubeFromPack(
      const Name: WideString;
      const FolderName, FileName: AnsiString;
      const MipLevels: Integer = 1;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2TextureCube;
    function CreateTextureCubeRT(
      const Name: WideString;
      const Size: Integer;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2TextureCubeRT;
    function FindTexture(const Name: WideString): TG2TextureBase; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure AddTexture(const Texture: TG2TextureBase); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure DeleteTexture(const Index: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RemoveTexture(const Texture: TG2TextureBase); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure FreeTextures; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2TextureMgr END

//TG2TextureSurface BEGIN
  TG2TextureSurface = class (TObject)
  strict private
    var m_Texture: TG2TextureBase;
    var m_Level: Integer;
    var m_Face: TD3DCubemapFaces;
    var m_Locked: Boolean;
    var m_Desc: TD3DSurfaceDesc;
    var m_Lock: TD3DLockedRect;
    var m_Width: Integer;
    var m_Height: Integer;
    var m_ProcSetPixel: procedure(const X, Y: Integer; const Value: TG2Color) of Object;
    var m_ProcGetPixel: function(const X, Y: Integer): TG2Color of Object;
    function GetPixelA16B16G16R16(const X, Y: Integer): TG2Color;
    procedure SetPixelA16B16G16R16(const X, Y: Integer; const Value: TG2Color);
    function GetPixelA2B10G10R10(const X, Y: Integer): TG2Color;
    procedure SetPixelA2B10G10R10(const X, Y: Integer; const Value: TG2Color);
    function GetPixelG16R16(const X, Y: Integer): TG2Color;
    procedure SetPixelG16R16(const X, Y: Integer; const Value: TG2Color);
    function GetPixelA2R10G10B10(const X, Y: Integer): TG2Color;
    procedure SetPixelA2R10G10B10(const X, Y: Integer; const Value: TG2Color);
    function GetPixelA8R3G3B2(const X, Y: Integer): TG2Color;
    procedure SetPixelA8R3G3B2(const X, Y: Integer; const Value: TG2Color);
    function GetPixelA8(const X, Y: Integer): TG2Color;
    procedure SetPixelA8(const X, Y: Integer; const Value: TG2Color);
    function GetPixelR3G3B2(const X, Y: Integer): TG2Color;
    procedure SetPixelR3G3B2(const X, Y: Integer; const Value: TG2Color);
    function GetPixelA1R5G5B5(const X, Y: Integer): TG2Color;
    procedure SetPixelA1R5G5B5(const X, Y: Integer; const Value: TG2Color);
    function GetPixelR5G6B5(const X, Y: Integer): TG2Color;
    procedure SetPixelR5G6B5(const X, Y: Integer; const Value: TG2Color);
    function GetPixelA4R4G4B4(const X, Y: Integer): TG2Color;
    procedure SetPixelA4R4G4B4(const X, Y: Integer; const Value: TG2Color);
    function GetPixelR8G8B8(const X, Y: Integer): TG2Color;
    procedure SetPixelR8G8B8(const X, Y: Integer; const Value: TG2Color);
    function GetPixelA8R8G8B8(const X, Y: Integer): TG2Color;
    procedure SetPixelA8R8G8B8(const X, Y: Integer; const Value: TG2Color);
    function GetPixelDummy(const X, Y: Integer): TG2Color;
    procedure SetPixelDummy(const X, Y: Integer; const Value: TG2Color);
    function GetPixel(const X, Y: Integer): TG2Color;
    procedure SetPixel(const X, Y: Integer; const Value: TG2Color);
    function GetPixelLerp(const X, Y: Single): TG2Color; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSurfaceDesc: PD3DSurfaceDesc;
    function GetLockedRect: PD3DLockedRect;
  public
    constructor Create(
      const Texture: TG2TextureBase;
      const Level: Integer = 0;
      const Face: TD3DCubemapFaces = D3DCUBEMAP_FACE_POSITIVE_X
    );
    destructor Destroy; override;
    property Locked: Boolean read m_Locked write m_Locked;
    property SurfaceDesc: PD3DSurfaceDesc read GetSurfaceDesc;
    property LockedRect: PD3DLockedRect read GetLockedRect;
    property Width: Integer read m_Width;
    property Height: Integer read m_Height;
    property Pixels[const X, Y: Integer]: TG2Color read GetPixel write SetPixel;
    property PixelsLerp[const X, Y: Single]: TG2Color read GetPixelLerp;
    function Lock(const Flags: DWord = 0): TG2Result;
    function UnLock: TG2Result;
  end;
//TG2TextureSurface END

//TG2TextureBase BEGIN
  TG2TextureBase = class (TG2Res)
  strict protected
    var m_Gfx: TG2Graphics;
    var m_Levels: Integer;
    var m_Format: TD3DFormat;
    function GetTexture: IDirect3DBaseTexture9; virtual; abstract;
    procedure SetTexture(const Value: IDirect3DBaseTexture9); virtual; abstract;
    function MaxMipLevels(const Width, Height, Depth: Integer): Integer;
  protected
    procedure OnDeviceLost; virtual;
    procedure OnDeviceReset; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Texture: IDirect3DBaseTexture9 read GetTexture write SetTexture;
    property Format: TD3DFormat read m_Format;
    property Levels: Integer read m_Levels;
    procedure Release; virtual; abstract;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2TextureBase END

//TG2Texture2DBase BEGIN
  TG2Texture2DBase = class (TG2TextureBase)
  strict private
    function GetDrawRect: PG2Rect;
  strict protected
    var m_Texture: IDirect3DTexture9;
    var m_Width: Integer;
    var m_Height: Integer;
    var m_RealWidth: Integer;
    var m_RealHeight: Integer;
    var m_DrawRect: TG2Rect;
    var m_Desc: TD3DSurfaceDesc;
    procedure SetDrawRect;
    function GetTexture: IDirect3DBaseTexture9; override;
    procedure SetTexture(const Value: IDirect3DBaseTexture9); override;
    function GetProjMatrix: TG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTexelSize: TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property Width: Integer read m_Width write m_Width;
    property Height: Integer read m_Height write m_Height;
    property RealWidth: Integer read m_RealWidth;
    property RealHeight: Integer read m_RealHeight;
    property DrawRect: PG2Rect read GetDrawRect;
    property ProjMatrix: TG2Mat read GetProjMatrix;
    property TexelSize: TG2Vec2 read GetTexelSize;
    function SaveToFile(const FileName: String): TG2Result;
    procedure Release; override;
  end;
//TG2Texture2DBase END

//TG2Texture2D BEGIN
  TG2Texture2D = class (TG2Texture2DBase)
  strict private
    var m_Surfaces: array of TG2TextureSurface;
    procedure InitLevels;
    procedure UnInitLevels;
    function GetSurface(const SurfaceLevel: Integer): TG2TextureSurface; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  strict protected
    procedure SetTexture(const Value: IDirect3DBaseTexture9); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Surfaces[const SurfaceLevel: Integer]: TG2TextureSurface read GetSurface;
    function LoadFromFile(
      const f: WideString;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function LoadFromStream(
      const s: TMemoryStream;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function LoadFromBuffer(
      const b: Pointer;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function LoadFromGraphic(
      const g: TGraphic;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function LoadFromPack(
      const FolderName, FileName: AnsiString;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function LoadFromTexture2D(
      const Tex: TG2Texture2DBase;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function LoadNormalMap(
      const HeightMap: TG2Texture2D;
      const Amplitude: Single = 1;
      const MipLevels: Integer = 8;
      const Channel: DWord = D3DX_CHANNEL_ALPHA;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function MakeTexture(
      const NewWidth: Integer;
      const NewHeight: Integer;
      const MipLevels: Integer = 8;
      const Usage: DWord = 0;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN;
      const Pool: TD3DPool = D3DPOOL_MANAGED
    ): TG2Result;
    function LoadAlpha(
      const Tex: TG2Texture2D;
      const Channel: DWord = D3DX_CHANNEL_ALPHA
    ): TG2Result;
    function ChangeFormat(
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function ChangeColor(
      const OldColor, NewColor: TG2Color
    ): TG2Result;
    procedure GenerateMipMaps;
    procedure Release; override;
  end;
//TG2Texture2D END

//TG2Texture2DRT BEGIN
  TG2Texture2DRT = class (TG2Texture2DBase)
  strict private
    var m_SurfaceRT: TG2SurfaceRT;
  protected
    procedure OnDeviceLost; override;
    procedure OnDeviceReset; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property SurfaceRT: TG2SurfaceRT read m_SurfaceRT;
    function MakeRenderTarget(
      const NewWidth, NewHeight: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    procedure Release; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Texture2DRT END

//TG2Texture2DDS BEGIN
  TG2Texture2DDS = class (TG2Texture2DBase)
  strict private
    var m_DepthStencil: TG2SurfaceDS;
  protected
    procedure OnDeviceLost; override;
    procedure OnDeviceReset; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property DepthStencil: TG2SurfaceDS read m_DepthStencil;
    function MakeDepthStencil(
      const NewWidth, NewHeight: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    procedure Release; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Texture2DDS END

//TG2Texture2DVideo BEGIN
  TG2Texture2DVideo = class (TG2Texture2DBase)
  strict private
    type TG2VMRAllocatorPresenter = class (TInterfacedObject, IVMRSurfaceAllocator9, IVMRImagePresenter9)
    private
      var m_Texture: TG2Texture2DVideo;
      var m_Surfaces: array of IDirect3DSurface9;
      var m_SurfaceAllocatorNotify: IVMRSurfaceAllocatorNotify9;
      var m_CriticalSection: TCriticalSection;
      var m_AllocInfo: TVMR9AllocationInfo;
      var m_SurfaceCount: DWord;
    protected
      function CreateSurfaces: HRESULT;
      function CreateTexture: HRESULT;
      procedure FreeSurfaces;
    public
      constructor Create(const Texture: TG2Texture2DVideo);
      destructor Destroy; override;
      function InitializeDevice(
        dwUserID: DWORD;
        lpAllocInfo: PVMR9AllocationInfo;
        var lpNumBuffers: DWORD
      ): HResult; stdcall;
      function TerminateDevice(
        dwID: DWORD
      ): HResult; stdcall;
      function GetSurface(
        dwUserID: DWORD;
        SurfaceIndex: DWORD;
        SurfaceFlags: DWORD;
        out lplpSurface: IDirect3DSurface9
      ): HResult; stdcall;
      function AdviseNotify(
        lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9
      ): HResult; stdcall;
      function StartPresenting(
        dwUserID: DWORD
      ): HResult; stdcall;
      function StopPresenting(
        dwUserID: DWORD
      ): HResult; stdcall;
      function PresentImage(
        dwUserID: DWORD;
        lpPresInfo: PVMR9PresentationInfo
      ): HResult; stdcall;
    end;
    var m_UsrID: DWORD;
    var m_Graph: IGraphBuilder;
    var m_Control: IMediaControl;
    var m_Event: IMediaEventEx;
    var m_Position: IMediaPosition;
    var m_Seeking: IMediaSeeking;
    var m_VMR: IBaseFilter;
    var m_PinIn, m_PinOut: IPin;
    var m_AllocatorPresenter: TG2VMRAllocatorPresenter;
    var m_PrevPos: Int64;
    var m_FName: WideString;
    var m_Loaded: Boolean;
    function AddFilter(
      CLSID: TGUID;
      FilterName: WideString
    ): IBaseFilter;
    procedure ClearGraph;
    procedure CompleteGraph;
    procedure SetPosition(const Value: Int64);
    function GetPosition: Int64;
    function GetDuration: Int64;
  private
    function MakeTexture(const NewWidth, NewHeight: Integer): TG2Result;
    function SetSurfaceAllocator: TG2Result;
  protected
    procedure OnDeviceLost; override;
    procedure OnDeviceReset; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Position: Int64 read GetPosition write SetPosition;
    property Duration: Int64 read GetDuration;
    property Loaded: Boolean read m_Loaded;
    function StreamFile(const f: WideString): TG2Result;
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Rewind;
    function IsPlaying: Boolean;
    procedure Release; override;
  end;
//TG2Texture2DVideo END

//TG2TextureCubeBase BEGIN
  TG2TextureCubeBase = class (TG2TextureBase)
  strict protected
    var m_Texture: IDirect3DCubeTexture9;
    var m_Width: Integer;
    var m_Height: Integer;
    var m_Depth: Integer;
    function GetTexture: IDirect3DBaseTexture9; override;
    procedure SetTexture(const Value: IDirect3DBaseTexture9); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Width: Integer read m_Width;
    property Height: Integer read m_Height;
    property Depth: Integer read m_Depth;
    function SaveToFile(const FileName: WideString): TG2Result;
    procedure Release; override;
  end;
//TG2TextureCubeBase END

//TG2TextureCube BEGIN
  TG2TextureCube = class (TG2TextureCubeBase)
  strict protected
    var m_Surfaces: array[0..5] of array of TG2TextureSurface;
    procedure InitLevels;
    procedure UnInitLevels;
    procedure SetTexture(const Value: IDirect3DBaseTexture9); override;
    function GetSurface(const Face: TD3DCubemapFaces; const SurfaceLevel: Integer): TG2TextureSurface; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property Surfaces[const Face: TD3DCubemapFaces; const SurfaceLevel: Integer]: TG2TextureSurface read GetSurface;
    function LoadFromFile(
      const f: WideString;
      const MipLevels: Integer = 1;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function LoadFromBuffer(
      const Buffer: Pointer;
      const Size: DWord;
      const MipLevels: Integer = 1;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function LoadFromPack(
      const FolderName, FileName: AnsiString;
      const MipLevels: Integer = 1;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    function MakeTexture(
      const Size: Integer;
      const MipLevels: Integer = 8;
      const Usage: DWord = 0;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN;
      const Pool: TD3DPool = D3DPOOL_MANAGED
    ): TG2Result;
    procedure Release; override;
  end;
//TG2TextureCube END

//TG2TextureCubeRT BEGIN
  TG2TextureCubeRT = class (TG2TextureCubeBase)
  strict private
    var m_SurfacesRT: array[0..5] of TG2SurfaceRT;
    procedure InitSurfaces;
    procedure UnInitSurfaces;
    function GetSurface(const Face: TD3DCubemapFaces): TG2SurfaceRT; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  strict protected
    procedure SetTexture(const Value: IDirect3DBaseTexture9); override;
  protected
    procedure OnDeviceLost; override;
    procedure OnDeviceReset; override;
  public
    property SurfacesRT[const Face: TD3DCubemapFaces]: TG2SurfaceRT read GetSurface;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function MakeRenderTarget(
      const Size: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    procedure Release; override;
  end;
//TG2TextureCubeRT END

//TG2TextureVolume BEGIN
  TG2TextureVolume = class (TG2TextureBase)
  strict private
    var m_Texture: IDirect3DVolumeTexture9;
    var m_Width: Integer;
    var m_Height: Integer;
    var m_Depth: Integer;
  strict protected
    function GetTexture: IDirect3DBaseTexture9; override;
    procedure SetTexture(const Value: IDirect3DBaseTexture9); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function LoadFromFile(
      const FileName: WideString;
      const MipLevels: Integer = 1;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    procedure Release; override;
  end;
//TG2TextureVolume END

//TG2SurfaceMgr BEGIN
  TG2SurfaceMgr = class (TG2ResMgr)
  strict private
    var m_PlugGraphics: TG2PlugGraphics;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateRenderTargetSurface(
      const Name: WideString;
      const Width, Height: Integer;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2SurfaceRT;
    function CreateDepthStencilSurface(
      const Name: WideString;
      const Width, Height: Integer;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2SurfaceDS;
    function FindSurface(const Name: WideString): TG2Surface;
    procedure DeleteSurface(const Index: Integer);
    procedure RemoveSurface(const Surface: TG2Surface);
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2SurfaceMgr END

//TG2Surface BEGIN
  TG2Surface = class (TG2Res)
  strict protected
    var m_Surface: IDirect3DSurface9;
    var m_ViewPort: TD3DViewPort9;
    var m_Format: TD3DFormat;
    var m_Width: DWord;
    var m_Height: DWord;
    var m_Enabled: Boolean;
    procedure SetSurface(const Value: IDirect3DSurface9);
    procedure SetUpViewPort;
    function GetViewPort: PD3DViewport9;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Surface: IDirect3DSurface9 read m_Surface write SetSurface;
    property Format: TD3DFormat read m_Format;
    property Width: DWord read m_Width;
    property Height: DWord read m_Height;
    property ViewPort: PD3DViewport9 read GetViewPort;
    procedure Release;
    procedure OnDeviceLost; virtual; abstract;
    procedure OnDeviceReset; virtual; abstract;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Surface END

//TG2SurfaceRT BEGIN
  TG2SurfaceRT = class (TG2Surface)
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateRenderTarget(
      const NewWidth, NewHeight: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    procedure OnDeviceLost; override;
    procedure OnDeviceReset; override;
  end;
//TG2SurfaceRT END

//TG2SurfaceDS BEGIN
  TG2SurfaceDS = class (TG2Surface)
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateDepthStencil(
      const NewWidth, NewHeight: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
    procedure OnDeviceLost; override;
    procedure OnDeviceReset; override;
  end;
//TG2SurfaceDS END

//TG2NetServer BEGIN
  TG2NetServer = class (TG2HighClass)
  strict private
    type TG2NetThread = class (TThread)
    strict private
      var m_CS: TCriticalSection;
    private
      var Server: TG2NetServer;
      property CS: TCriticalSection read m_CS;
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
    end;
    type TG2NetThreadListen = class (TG2NetThread)
    strict protected
      procedure Execute; override;
    end;
    type TG2NetThreadReceive = class (TG2NetThread)
    strict private
      var m_Buf: array of Byte;
    strict protected
      procedure Execute; override;
    private
      var ID: Integer;
    end;
    type TG2NetThreadIdle = class (TG2NetThread)
    strict protected
      procedure Execute; override;
    end;
    type TG2NetConnection = record
    private
      var Ping: DWord;
      var ThreadReceive: TG2NetThreadReceive;
      var Server: TG2NetServer;
      procedure ReceivingEnable;
      procedure ReceivingDisable;
    public
      var ID: Integer;
      var Sock: Integer;
      var Addr: TSockAddrIn;
      var UserData: Pointer;
      var Status: TG2NetStatus;
    end;
    type PG2NetConnection = ^TG2NetConnection;
    type TG2NetPacketReceive = record
    private
      var Server: TG2NetServer;
      var ID: Integer;
      var Discarded: Boolean;
    public
      var Buf: array of Byte;
      var Len: Integer;
      var UserData: Pointer;
      procedure Discard;
    end;
    type PG2NetPacketReceive = ^TG2NetPacketReceive;
    var m_Sock: Integer;
    var m_Connections: array of TG2NetConnection;
    var m_ConnectionCount: Integer;
    var m_ThreadListen: TG2NetThreadListen;
    var m_AllowConnections: Boolean;
    var m_PacketList: array of Integer;
    var m_PacketListCount: Integer;
    var m_Status: TG2NetStatus;
    function GetConnection(const Index: Integer): PG2NetConnection; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPacket(const Index: Integer): PG2NetPacketReceive; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var PacketsReceived: array of TG2NetPacketReceive;
    procedure AddConnection(const NewSock: Integer; const NewAddr: PSockAddrIn);
    procedure EnableReceivePacket(const ID: Integer);
    procedure DiscardReceivePacket(const ID: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Sock: Integer read m_Sock;
    property Connections[const Index: Integer]: PG2NetConnection read GetConnection;
    property ConnectionCount: Integer read m_ConnectionCount;
    property Packets[const Index: Integer]: PG2NetPacketReceive read GetPacket;
    property PacketCount: Integer read m_PacketListCount;
    property Status: TG2NetStatus read m_Status;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function AllowConnections(const Port: Word = 5632; const MaxRequests: Integer = 4): TG2Result;
    procedure ForbidConnections;
    function Send(const ID: Integer; const Data: Pointer; const Size: Integer): TG2Result;
  end;
//TG2NetServer END

//TG2NetClient BEGIN
  TG2NetClient = class (TG2HighClass)
  strict private
    type TG2NetThread = class (TThread)
    strict private
      var m_CS: TCriticalSection;
    private
      var Client: TG2NetClient;
      property CS: TCriticalSection read m_CS;
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
    end;
    type TG2NetThreadReceive = class (TG2NetThread)
    strict private
      var m_Buf: array of Byte;
    strict protected
      procedure Execute; override;
    end;
    type TG2NetPacketReceive = record
    private
      var Client: TG2NetClient;
      var ID: Integer;
      var Discarded: Boolean;
    public
      var Buf: array of Byte;
      var Len: Integer;
      procedure Discard;
    end;
    type PG2NetPacketReceive = ^TG2NetPacketReceive;
    var m_Sock: Integer;
    var m_Connected: Boolean;
    var m_ThreadReceive: TG2NetThreadReceive;
    var m_PacketList: array of Integer;
    var m_PacketListCount: Integer;
    function GetPacket(const Index: Integer): PG2NetPacketReceive; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var PacketsReceived: array of TG2NetPacketReceive;
    procedure EnableReceivePacket(const ID: Integer);
    procedure DiscardReceivePacket(const ID: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Sock: Integer read m_Sock;
    property Packets[const Index: Integer]: PG2NetPacketReceive read GetPacket;
    property PacketCount: Integer read m_PacketListCount;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function Connect(const Host: AnsiString; const Port: Word = 5632): TG2Result;
    function Send(const Data: Pointer; const Size: Integer): TG2Result;
    function Disconnect: TG2Result;
  end;
//TG2NetClient END

//TG2FontMgr BEGIN
  TG2FontMgr = class sealed (TG2ResMgr)
  strict private
    function GetFont(const Index: Integer): TG2Font; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property Fonts[const Index: Integer]: TG2Font read GetFont;
    function CreateFont(
      const Name: WideString;
      const FontFace: AnsiString;
      const Size: Integer
    ): TG2Font;
    function CreateFontFromFile(
      const Name: WideString;
      const FileName: WideString;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Font;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2FontMgr END

//TG2Font BEGIN
  TG2Font = class (TG2Res)
  strict private
    type TCharProps = record
      Width: Integer;
      Height: Integer;
      OffsetX: Integer;
      OffsetY: Integer;
    end;
    var m_Gfx: TG2Graphics;
    var m_Texture: TG2Texture2D;
    var m_Props: array[Byte] of TCharProps;
    var m_CharTU: Single;
    var m_CharTV: Single;
    var m_CharWidth: Integer;
    var m_CharHeight: Integer;
    var m_FontFace: AnsiString;
    var m_Size: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Texture: TG2Texture2D read m_Texture;
    property FontFace: AnsiString read m_FontFace;
    property Size: Integer read m_Size;
    function MakeFont(const NewFontFace: AnsiString; const NewSize: Integer): TG2Result;
    function LoadFont(const FileName: String; const NewFormat: TD3DFormat = D3DFMT_UNKNOWN): TG2Result;
    function Print(const X, Y: Single; const Color: TG2Color; const Text: AnsiString): TG2Result; overload;
    function Print(const X, Y, ScaleX, ScaleY: Single; const Color: TG2Color; const Text: AnsiString): TG2Result; overload;
    function GetTextWidth(const Text: AnsiString): Integer;
    function GetTextHeight(const Text: AnsiString): Integer;
    procedure Release;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Font END

//TG2MeshMgr BEGIN
  TG2MeshMgr = class (TG2ResMgr)
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateMeshFromFile(const Name: WideString; const f: WideString): TG2Mesh;
    function CreateMeshFromBuffer(const Name: WideString; const Buffer: Pointer; const Size: Integer): TG2Mesh;
    function CreateMeshFromPack(const Name: WideString; const FolderName, FileName: AnsiString): TG2Mesh;
    function FindMesh(const Name: WideString): TG2Mesh; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure AddMesh(const Mesh: TG2Mesh); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure DeleteMesh(const Index: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RemoveMesh(const Mesh: TG2Mesh); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;
//TG2MeshMgr END

//TG2MeshBoneGroup BEGIN
  TG2MeshBoneGroup = record
  private
    var Mesh: TG2Mesh;
    var Nodes: array of Integer;
    procedure IncludeBone(const NodeID: Integer; const SubBones: Boolean = False); overload;
    procedure ExcludeBone(const NodeID: Integer; const SubBones: Boolean = False); overload;
  public
    class operator Add(const g1, g2: TG2MeshBoneGroup): TG2MeshBoneGroup;
    class operator Subtract(const g1, g2: TG2MeshBoneGroup): TG2MeshBoneGroup;
    procedure IncludeBone(const NodeName: AnsiString; const SubBones: Boolean = False); overload;
    procedure ExcludeBone(const NodeName: AnsiString; const SubBones: Boolean = False); overload;
    procedure IncludeGroup(const BoneGroup: TG2MeshBoneGroup);
    procedure ExcludeGroup(const BoneGroup: TG2MeshBoneGroup);
    procedure IncludeAll;
    procedure ExcludeAll;
    procedure Assign(const BoneGroup: TG2MeshBoneGroup);
  end;
  PG2MeshBoneGroup = ^TG2MeshBoneGroup;
//TG2MeshBoneGroup END

//TG2MeshAnimBlend BEGIN
  TG2MeshAnimBlend = record
  private
    var Mesh: TG2Mesh;
    var Anims: array of record
    public
      var AnimID: Integer;
      var Weight: Single;
      var Bones: array of Boolean;
      var Frame: Single;
    end;
    var NodeTransforms: array of record
    public
      var Rotation: TG2Quat;
      var Scaling: TG2Vec3;
      var Translation: TG2Vec3;
      var TotalWeight: Single;
    end;
  public
    function AddAnim(const AnimName: AnsiString; const BoneGroup: TG2MeshBoneGroup; const Weight: Single): Integer;
    procedure SetAnimFrame(const AnimBlendID: Integer; const Frame: Single);
  end;
//TG2MeshAnimBlend END

//TG2Mesh BEGIN
  TG2MeshRenderMode = (rmFF, rmSM3);

  TG2Mesh = class (TG2Res)
  strict private
    type TG2MeshVertexStatic = packed record
      var Position: TG2Vec3;
      var Tangent: TG2Vec3;
      var Binormal: TG2Vec3;
      var Normal: TG2Vec3;
      var TexCoords: TG2Vec2;
    end;
    type TG2MeshVertexStaticArr = array[Word] of TG2MeshVertexStatic;
    type PG2MeshVertexStaticArr = ^TG2MeshVertexStaticArr;
    type TG2MeshVertexSkinned = packed record
      var Position: TG2Vec3;
      var Tangent: TG2Vec3;
      var Binormal: TG2Vec3;
      var Normal: TG2Vec3;
      var TexCoords: TG2Vec2;
      var BoneIndices: array[0..3] of Single;
      var BoneWeights: array[0..3] of Single;
    end;
    type PG2MeshVertexSkinned = ^TG2MeshVertexSkinned;
    type TG2MeshVertexSkinnedArr = array[Word] of TG2MeshVertexSkinned;
    type PG2MeshVertexSkinnedArr = ^TG2MeshVertexSkinnedArr;
    type TG2MeshVertexStaticFF = packed record
      var Position: TG2Vec3;
      var Normal: TG2Vec3;
      var TexCoords: TG2Vec2;
    end;
    type TG2MeshVertexStaticFFArr = array[Word] of TG2MeshVertexStaticFF;
    type PG2MeshVertexStaticFFArr = ^TG2MeshVertexStaticFFArr;
    type TG2MeshVertexSkinnedFF = packed record
      var Position: TG2Vec3;
      var BWeights: array[0..3] of Single;
      var BIndices: array[0..3] of Byte;
      var Normal: TG2Vec3;
      var TexCoords: TG2Vec2;
    end;
    type PG2MeshVertexSkinnedFF = ^TG2MeshVertexSkinnedFF;
    type TG2MeshVertexSkinnedFFArr = array[Word] of TG2MeshVertexSkinnedFF;
    type PG2MeshVertexSkinnedFFArr = ^TG2MeshVertexSkinnedFFArr;
    var m_Effect: TG2Effect;
  public
    class var RenderMode: TG2MeshRenderMode;
    var NodeCount: Integer;
    var GeomCount: Integer;
    var AnimCount: Integer;
    var MaterialCount: Integer;
    var RagdollCount: Integer;
    var BBox: TG2Box;
    var Nodes: array of record
    public
      OwnerID: Integer;
      Name: AnsiString;
      Transform: TG2Mat;
      SlaveID: array of Integer;
    end;
    var Geoms: array of record
    public
      var NodeID: Integer;
      var Skinned: Boolean;
      var BBox: TG2Box;
      var Technique: TD3DXHandle;
      var MaxWeights: Word;
      var BoneCount: Integer;
      var VertexStride: Word;
      var Bones: array of record
      public
        NodeID: Integer;
        Bind: TG2Mat;
        BBox: TG2Box;
        VCount: Integer;
      end;
      var MaterialCount: Integer;
      var Materials: array of Integer;
      var Mesh: ID3DXMesh;
      var Visible: Boolean;
    end;
    var Anims: array of record
    public
      var Name: AnsiString;
      var FrameRate: Integer;
      var FrameCount: Integer;
      var NodeCount: Integer;
      var Nodes: array of record
      public
        var NodeID: Integer;
        var Frames: array of record
        public
          var Scale: TG2Vec3;
          var Rotation: TG2Quat;
          var Translation: TG2Vec3;
        end;
      end;
    end;
    var Materials: array of record
    public
      var Name: AnsiString;
      var TwoSided: Boolean;
      var AmbientColor: TG2Color;
      var DiffuseColor: TG2Color;
      var SpecularColor: TG2Color;
      var SpecularColorAmount: Single;
      var SpecularPower: Single;
      var EmmissiveColor: TG2Color;
      var EmmissiveColorAmount: Single;
      var AmbientMapEnable: Boolean;
      var AmbientMap: AnsiString;
      var AmbientMapAmount: Single;
      var DiffuseMapEnable: Boolean;
      var DiffuseMap: AnsiString;
      var DiffuseMapAmount: Single;
      var SpecularMapEnable: Boolean;
      var SpecularMap: AnsiString;
      var SpecularMapAmount: Single;
      var OpacityMapEnable: Boolean;
      var OpacityMap: AnsiString;
      var OpacityMapAmount: Single;
      var LightMapEnable: Boolean;
      var LightMap: AnsiString;
      var LightMapAmount: Single;
      var NormalMapEnable: Boolean;
      var NormalMap: AnsiString;
      var NormalMapAmount: Single;
    end;
    var Ragdolls: array of record
    public
      var NodeID: Integer;
      var Head: TG2RagdollObject;
      var Neck: TG2RagdollObject;
      var Pelvis: TG2RagdollObject;
      var BodyNodeCount: Integer;
      var BodyNodes: array of TG2RagdollObject;
      var ArmRNodeCount: Integer;
      var ArmRNodes: array of TG2RagdollObject;
      var ArmLNodeCount: Integer;
      var ArmLNodes: array of TG2RagdollObject;
      var LegRNodeCount: Integer;
      var LegRNodes: array of TG2RagdollObject;
      var LegLNodeCount: Integer;
      var LegLNodes: array of TG2RagdollObject;
    end;
    property Effect: TG2Effect read m_Effect;
    function LoadData(const MeshData: TG2MeshData): TG2Result;
    function ExtractData: TG2MeshData;
    function NodeIndex(const NodeName: AnsiString): Integer;
    function GeomIndex(const NodeName: AnsiString): Integer;
    function AnimIndex(const AnimName: AnsiString): Integer;
    function RagdollIndex(const RagdollName: AnsiString): Integer;
    function InstanceCreate: TG2MeshInst;
    function BoneGroupCreate: TG2MeshBoneGroup;
    function AnimBlendCreate: TG2MeshAnimBlend;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Mesh END

//TG2MeshInst BEGIN
  TG2MeshInst = class
  strict private
    var m_Mesh: TG2Mesh;
    var m_Effect: TG2Effect;
    var m_RootNodes: array of Integer;
    var m_SkinTransforms: array of array of TG2Mat;
    var m_AutoFrustumCull: Boolean;
    var m_AutoComputeTransforms: Boolean;
    var m_TempMat: array of TG2Mat;
    var m_TempVec: array of TG2Vec3;
    procedure SetMesh(const Value: TG2Mesh); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSkinTransforms(const Index: Integer): PG2Mat; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBox: TG2Box; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetGeomBBox(const Index: Integer): TG2Box; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure RenderFF;
    procedure RenderSM3;
  public
    var NodeTransforms: array of record
    public
      var TransformDef: TG2Mat;
      var TransformCur: TG2Mat;
      var TransformRen: TG2Mat;
    end;
    var GeomProperties: array of record
    public
      var Visible: Boolean;
    end;
    var Materials: array of record
    public
      var Name: AnsiString;
      var TwoSided: Boolean;
      var MapDiffuse: TG2Texture2DBase;
      var MapSpecular: TG2Texture2DBase;
      var MapNormals: TG2Texture2DBase;
    end;
    constructor Create;
    destructor Destroy; override;
    property Mesh: TG2Mesh read m_Mesh write SetMesh;
    property SkinTransforms[const Index: Integer]: PG2Mat read GetSkinTransforms;
    property BBox: TG2Box read GetBox;
    property GeomBBox[const Index: Integer]: TG2Box read GetGeomBBox;
    property AutoFrustumCull: Boolean read m_AutoFrustumCull write m_AutoFrustumCull;
    property AutoComputeTransforms: Boolean read m_AutoComputeTransforms write m_AutoComputeTransforms;
    procedure FrameSetFast(const AnimName: AnsiString; const Frame: Integer);
    procedure FrameSet(const AnimName: AnsiString; const Frame: Single); overload;
    procedure FrameSet(const AnimName: AnsiString; const Frame: Single; const BoneGroup: TG2MeshBoneGroup); overload;
    procedure FrameBlend(const AnimBlend: TG2MeshAnimBlend);
    procedure ComputeTransforms; overload;
    procedure ComputeTransforms(const BoneGroup: TG2MeshBoneGroup); overload;
    procedure ComputeSkinTransforms;
    procedure Render;
    function Pick(
      const Ray: TG2Ray;
      const OutD: PSingle = nil;
      const OutGeomID: PInteger = nil;
      const OutFaceID: PWord = nil;
      const OutU: PSingle = nil;
      const OutV: PSingle = nil
    ): Boolean;
    function PickGeom(
      const GeomID: Integer;
      const Ray: TG2Ray;
      const OutD: PSingle = nil;
      const OutFaceID: PWord = nil;
      const OutU: PSingle = nil;
      const OutV: PSingle = nil
    ): Boolean;
  end;
//TG2MeshInst END

//TG2Shared BEGIN
  TG2Shared = class (TG2Module)
  strict private
    var m_VBMgr: TG2VBMgr;
    var m_IBMgr: TG2IBMgr;
    var m_FontMgr: TG2FontMgr;
    var m_LastVBID: DWord;
    var m_LastIBID: DWord;
    var m_LastFontID: DWord;
  public
    constructor Create; override;
    destructor Destroy; override;
    function RequestVB(
      const NewStride: DWord;
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFVF: DWord;
      const NewPool: TD3DPool
    ): TG2VB;
    function RequestIB(
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFormat: TD3DFormat;
      const NewPool: TD3DPool
    ): TG2IB;
    function RequestFont(
      const FontFace: AnsiString;
      const Size: Integer
    ): TG2Font;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Shared END

//TG2VBMgr BEGIN
  TG2VBMgr = class (TG2ResMgr)
  strict private
    var m_PlugGraphics: TG2PlugGraphics;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
    function GetVB(const Index: Integer): TG2VB; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property VertexBuffers[const Index: Integer]: TG2VB read GetVB;
    function CreateVertexBuffer(
      const Name: WideString;
      const NewStride: DWord;
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFVF: DWord;
      const NewPool: TD3DPool
    ): TG2VB;
    function FindVB(const Name: WideString): TG2VB;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2VBMgr END

//TG2VB BEGIN
  TG2VB = class (TG2Res)
  strict private
    var m_Graphics: TG2Graphics;
    var m_VB: IDirect3DVertexBuffer9;
    var m_Stride: DWord;
    var m_Count: DWord;
    var m_Usage: DWord;
    var m_FVF: DWord;
    var m_Pool: TD3DPool;
    var m_Suspended: Boolean;
    function InitializeVB: Boolean;
  private
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  public
    constructor Create; override;
    destructor Destroy; override;
    property VB: IDirect3DVertexBuffer9 read m_VB;
    property Stride: DWord read m_Stride;
    property Count: DWord read m_Count;
    property Usage: DWord read m_Usage;
    property FVF: DWord read m_FVF;
    property Pool: TD3DPool read m_Pool;
    function Verify(
      const NewStride: DWord;
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFVF: DWord;
      const NewPool: TD3DPool
    ): Boolean;
    function Lock(const LockOffset, LockSize: DWord; var Data: Pointer; const LockFlags: DWord = 0): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function UnLock: HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Release;
    procedure SetToDevice;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2VB END

//TG2IBMgr BEGIN
  TG2IBMgr = class (TG2ResMgr)
  strict private
    var m_PlugGraphics: TG2PlugGraphics;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
    function GetIB(const Index: Integer): TG2IB; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property IndexBuffers[const Index: Integer]: TG2IB read GetIB;
    function CreateIndexBuffer(
      const Name: WideString;
      const NewSize: DWord;
      const NewUsage: DWord;
      const NewFormat: TD3DFormat;
      const NewPool: TD3DPool
    ): TG2IB;
    function FindIB(const Name: WideString): TG2IB;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2IBMgr END

//TG2IB BEGIN
  TG2IB = class (TG2Res)
  strict private
    var m_Graphics: TG2Graphics;
    var m_IB: IDirect3DIndexBuffer9;
    var m_Count: DWord;
    var m_Usage: DWord;
    var m_Format: TD3DFormat;
    var m_Pool: TD3DPool;
    var m_Suspended: Boolean;
    function InitializeIB: Boolean;
  private
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  public
    constructor Create; override;
    destructor Destroy; override;
    property IB: IDirect3DIndexBuffer9 read m_IB;
    property Count: DWord read m_Count;
    property Usage: DWord read m_Usage;
    property Format: TD3DFormat read m_Format;
    property Pool: TD3DPool read m_Pool;
    function Verify(
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFormat: TD3DFormat;
      const NewPool: TD3DPool
    ): Boolean;
    function Lock(const LockOffset, LockSize: DWord; var Data: Pointer; const Flags: DWord = 0): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function UnLock: HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Release;
    procedure SetToDevice;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2IB END

//TG2ScriptMgr BEGIN
  TG2ScriptMgr = class (TG2ResMgr)
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2ScriptMgr END

//TG2Script BEGIN
  TG2Script = class (TG2Res)
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Script END

  TG2UIAlign = (alNone, alClient, alCenter, alLeft, alTopLeft, alTop, alTopRight, alRight, alBottomRight, alBottom, alBottomLeft);

//TG2UI BEGIN
  TG2UI = class sealed (TG2Module)
  strict private
    var m_Root: TG2UIFrame;
    var m_PlugInput: TG2PlugInput;
    var m_PlugGraphics: TG2PlugGraphics;
    var m_Skin: TG2UISkin;
    var m_ClipRects: TG2QuickList;
    var m_Overlay: TG2UIFrame;
    var m_Render2D: TG2Render2D;
    var m_Prim2D: TG2Primitives2D;
    procedure InitBuffers;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
    procedure OnParamsChange;
    procedure OnKeyDown(const Key: Byte);
    procedure OnKeyUp(const Key: Byte);
    procedure OnKeyPress(const Key: AnsiChar);
    procedure OnMouseDown(const Button: Byte);
    procedure OnMouseUp(const Button: Byte);
    procedure OnMouseMove(const Shift: TPoint);
    procedure OnWheelMove(const Shift: Integer);
  private
    type TVertex = record
    public
      var Pos: TG2Vec4;
      var Tex: array[0..3] of TG2Vec2;
    end;
    type TVertexArr = array[Word] of TVertex;
    type PVertexArr = ^TVertexArr;
    var VB: TG2VB;
    var IB: TG2IB;
    var Skins: TG2QuickList;
    var MouseDownPos: TPoint;
    var RenderOverlays: Boolean;
    class function ParentToClientRect(const RectParent, RectClient: TRect): TRect;
    class function ClipRect(const Rect1, Rect2: TRect): TRect;
  public
    property Root: TG2UIFrame read m_Root;
    property Skin: TG2UISkin read m_Skin write m_Skin;
    property PlugInput: TG2PlugInput read m_PlugInput;
    property Render2D: TG2Render2D read m_Render2D;
    property Prim2D: TG2Primitives2D read m_Prim2D;
    property Overlay: TG2UIFrame read m_Overlay write m_Overlay;
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    procedure Update;
    procedure Render;
    function FrameAtPoint(const Pt: TPoint): TG2UIFrame;
    procedure PushClipRect(const R: TRect);
    procedure PopClipRect;
  end;
//TG2UI END

//TG2UISkin BEGIN
  TG2UISkin = class
  strict private
    type TMapping = (mtStretch, mtTile, mtBorder);
    type TElement = record
    public
      var Name: WideString;
      var Mapping: TMapping;
      var TexCoords: TG2Rect;
      var Texture: TG2Texture2D;
    end;
    type PElement = ^TElement;
    type TTemplate = record
    public
      var Name: WideString;
      var BorderSize: Integer;
      var Layers: array[0..3] of PElement;
    end;
    type PTemplate = ^TTemplate;
    var m_Textures: array of TG2Texture2D;
    var m_Elements: array of TElement;
    var m_Templates: array of TTemplate;
  private
    var GUI: TG2UI;
  public
    constructor Create(const OwnerGUI: TG2UI);
    destructor Destroy; override;
    function LoadFromFile(const f: WideString): TG2Result;
    function FindTemplate(const Name: WideString): PTemplate;
    procedure DrawTemplate(const TemplateName: WideString; const R: TRect);
  end;
//TG2UISkin END

//TG2UIFrame BEGIN
  TG2UIFrame = class
  strict private
    var m_Parent: TG2UIFrame;
    var m_SubFrames: TG2QuickSortList;
    var m_Initialized: Boolean;
    var m_Visible: Boolean;
    var m_Order: Integer;
    procedure SetParent(const Value: TG2UIFrame); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSubFrameCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSubFrame(const Index: Integer): TG2UIFrame; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLeft: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetLeft(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTop: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetTop(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetRight: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRight(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBottom: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBottom(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetX(const Value: Integer);
    procedure SetY(const Value: Integer);
    function GetWidth: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWidth(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetHeight: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetHeight(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetClientWidth: Integer;
    function GetClientHeight: Integer;
    procedure SetOrder(const Value: Integer);
  strict protected
    var m_RectSelf: TRect;
    var m_RectClient: TRect;
    var GUI: TG2UI;
    procedure ClientRectAdjust; virtual;
    procedure ScreenRectAdjust;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function IsMouseOver: Boolean;
    function IsMouseDown: Boolean;
  private
    var RectScreen: TRect;
    var RectClip: TRect;
    procedure SubFrameAdd(const Frame: TG2UIFrame); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SubFrameRemove(const Frame: TG2UIFrame); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  protected
    var CanFocus: Boolean;
    var CanInput: Boolean;
    procedure OnUpdate; virtual;
    procedure OnRender; virtual;
    procedure OnDeviceLost; virtual;
    procedure OnDeviceReset; virtual;
    procedure OnParamsChange; virtual;
    procedure OnKeyOnwn(const Key: Byte); virtual;
    procedure OnKeyUp(const Key: Byte); virtual;
    procedure OnKeyPress(const Key: AnsiChar); virtual;
    procedure OnMouseDown(const Button: Byte); virtual;
    procedure OnMouseUp(const Button: Byte); virtual;
    procedure OnMouseMove(const Shift: TPoint); virtual;
    procedure OnWheelMove(const Shift: Integer); virtual;
  public
    property Parent: TG2UIFrame read m_Parent write SetParent;
    property SubFrameCount: Integer read GetSubFrameCount;
    property SubFrames[const Index: Integer]: TG2UIFrame read GetSubFrame;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Right: Integer read GetRight write SetRight;
    property Bottom: Integer read GetBottom write SetBottom;
    property X: Integer read GetLeft write SetX;
    property Y: Integer read GetTop write SetY;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property ClientWidth: Integer read GetClientWidth;
    property ClientHeight: Integer read GetClientHeight;
    property RectLocal: TRect read m_RectSelf;
    property RectGlobal: TRect read RectScreen;
    property RectClient: TRect read m_RectClient;
    property Visible: Boolean read m_Visible write m_Visible;
    property Order: Integer read m_Order write SetOrder;
    constructor Create(const OwnerGUI: TG2UI);
    destructor Destroy; override;
    procedure Update;
    procedure Render;
    function RectToGlobal(const R: TRect): TRect;
  end;
//TG2UIFrame END

//TG2UIPanel BEGIN
  TG2UIPanel = class (TG2UIFrame)
  strict private
    var m_Template: AnsiString;
  strict protected
    procedure Initialize; override;
    procedure OnRender; override;
  public
    property Template: AnsiString read m_Template write m_Template;
  end;
//TG2UIPanel END

//TG2UIImage BEGIN
  TG2UIImage = class (TG2UIFrame)
  strict private
    var m_Texture: TG2Texture2D;
  strict protected
    procedure Initialize; override;
    procedure OnRender; override;
  public
    property Texture: TG2Texture2D read m_Texture write m_Texture;
  end;
//TG2UIImage END

//TG2UIButton BEGIN
  TG2UIButton = class (TG2UIFrame)
  strict private
    var m_ProcOnClick: TG2ProcObj;
  strict protected
    var m_Label: TG2UILabel;
    var m_Image: TG2UIImage;
    var m_ImageAlign: TG2UIAlign;
    procedure AdjustLabel;
    procedure AdjustImage;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure ClientRectAdjust; override;
    procedure OnRender; override;
    procedure OnMouseUp(const Button: Byte); override;
    function GetImage: TG2Texture2D;
    procedure SetImage(const Value: TG2Texture2D);
    function GetImageWidth: Integer;
    procedure SetImageWidth(const Value: Integer);
    function GetImageHeight: Integer;
    procedure SetImageHeight(const Value: Integer);
    function GetCaption: AnsiString;
    procedure SetCaption(const Value: AnsiString);
  public
    property Image: TG2Texture2D read GetImage write SetImage;
    property ImageAlign: TG2UIAlign read m_ImageAlign write m_ImageAlign;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth;
    property ImageHeight: Integer read GetImageHeight write SetImageHeight;
    property Caption: AnsiString read GetCaption write SetCaption;
    property OnClick: TG2ProcObj read m_ProcOnClick write m_ProcOnClick;
  end;
//TG2UIButton END

//TG2UIButtonSwitch BEGIN
  TG2UIButtonSwitch = class (TG2UIButton)
  strict protected
    var m_SwitchGroup: Integer;
    var m_Switch: Boolean;
    var m_ProcOnSwitch: TG2ProcObj;
    procedure Initialize; override;
    procedure OnMouseDown(const Button: Byte); override;
    procedure SetSwitch(const Value: Boolean);
    procedure OnRender; override;
  public
    property SwitchGroup: Integer read m_SwitchGroup write m_SwitchGroup;
    property Switch: Boolean read m_Switch write SetSwitch;
    property OnSwitch: TG2ProcObj read m_ProcOnSwitch write m_ProcOnSwitch;
  end;
//TG2UIButtonSwitch END

//TG2UILabel BEGIN
  TG2UILabel = class (TG2UIFrame)
  strict protected
    var m_Text: AnsiString;
    var m_Font: TG2Font;
    var m_FontColor: TG2Color;
    var m_OnChange: TG2ProcObj;
    procedure SetText(const Value: AnsiString); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFont(const Value: TG2Font); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Initialize; override;
    procedure OnRender; override;
  public
    property Text: AnsiString read m_Text write SetText;
    property Font: TG2Font read m_Font write SetFont;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    property OnChange: TG2ProcObj read m_OnChange write m_OnChange;
  end;
//TG2UILabel END

//TG2UIEdit BEGIN
  TG2UIEdit = class (TG2UIFrame)
  strict protected
    var m_Text: AnsiString;
    var m_Font: TG2Font;
    procedure SetText(const Value: AnsiString); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFont(const Value: TG2Font); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Initialize; override;
    procedure OnRender; override;
  public
    property Text: AnsiString read m_Text write SetText;
    property Font: TG2Font read m_Font write SetFont;
  end;
//TG2UIEdit END

(*
//TG2GUI BEGIN
  TG2GUI = class sealed (TG2Module)
  strict private
    m_Root: TG2GUIWindow;
    m_Focus: TG2GUIWindow;
    m_PlugInput: TG2PlugInput;
    m_Render2D: TG2Render2D;
    m_Prim2D: TG2Primitives2D;
    procedure OnMouseDown(const Button: Byte);
    procedure OnMouseUp(const Button: Byte);
    procedure OnMouseMove(const Shift: TPoint);
    procedure OnMouseWheel(const Shift: Integer);
    procedure OnKeyDown(const Key: Byte);
    procedure OnKeyUp(const Key: Byte);
    procedure OnKeyPress(const Key: AnsiChar);
    procedure SetFocus(const Value: TG2GUIWindow); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    property PlugInput: TG2PlugInput read m_PlugInput;
    property Render2D: TG2Render2D read m_Render2D;
    property Prim2D: TG2Primitives2D read m_Prim2D;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Root: TG2GUIWindow read m_Root;
    property Focus: TG2GUIWindow read m_Focus write SetFocus;
    procedure WindowCreate(const WindowClass: CG2GUIWindowClass; const Parent: TG2GUIWindow; var Window);
    procedure WindowDestroy(const Window: TG2GUIWindow);
    procedure Render;
    procedure Update;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUI END

//TG2GUIWindow BEGIN
  TG2GUIWindow = class (TG2HighClass)
  strict private
  type
    TVertex0 = packed record
      x, y, z, rhw: Single;
      Col: TG2Color;
    end;
    TVertex0Array = array[Word] of TVertex0;
    PVertex0Array = ^TVertex0Array;
    TIndexArray = array[Word] of Word;
    PIndexArray = ^TIndexArray;
  const
    FVF0 = D3DFVF_XYZRHW or D3DFVF_DIFFUSE;
  var
    m_GUI: TG2GUI;
    m_Parent: TG2GUIWindow;
    m_Children: TG2List;
    m_X: Integer;
    m_Y: Integer;
    m_W: Integer;
    m_H: Integer;
    m_Visible: Boolean;
    m_MarginLeft: Integer;
    m_MarginTop: Integer;
    m_MarginRight: Integer;
    m_MarginBottom: Integer;
    m_DownInRect: Boolean;
    m_MoveInRect: Boolean;
    m_OnMouseDown: TG2InputMouseDown;
    m_OnMouseUp: TG2InputMouseUp;
    m_OnMouseClick: TG2ProcObj;
    m_OnMouseMove: TG2InputMouseMove;
    m_OnWheelMove: TG2InputWheelMove;
    m_OnKeyDown: TG2InputKeyDown;
    m_OnKeyUp: TG2InputKeyUp;
    m_OnKeyPress: TG2InputKeyPress;
    m_OnMouseEnter: TG2ProcObj;
    m_OnMouseLeave: TG2ProcObj;
    m_OnFocusReceive: TG2ProcObj;
    m_OnFocusLose: TG2ProcObj;
    m_OnResize: TG2ProcObj;
    procedure SetParent(const Value: TG2GUIWindow); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRect(const Value: TRect); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetRect: TRect; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetClientRect(const Value: TRect); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetClientRect: TRect; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDrawRect: TRect; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetClientDrawRect: TRect; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFocused: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetW(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetH(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    property Children: TG2List read m_Children;
    procedure MouseDown(const Button: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseUp(const Button: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseClick; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseMove(const Shift: TPoint); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseWheel(const Shift: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure KeyDown(const Key: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure KeyUp(const Key: Byte); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure KeyPress(const Key: AnsiChar); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseEnter; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure MouseLeave; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure FocusReceive; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure FocusLose; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Resize; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure CheckFocus;
  strict protected
    m_VB0: TG2VB;
    m_IB: TG2IB;
    property OnMouseDown: TG2InputMouseDown read m_OnMouseDown write m_OnMouseDown;
    property OnMouseUp: TG2InputMouseUp read m_OnMouseUp write m_OnMouseUp;
    property OnMouseClick: TG2ProcObj read m_OnMouseClick write m_OnMouseClick;
    property OnMouseMove: TG2InputMouseMove read m_OnMouseMove write m_OnMouseMove;
    property OnWheelMove: TG2InputWheelMove read m_OnWheelMove write m_OnWheelMove;
    property OnKeyDown: TG2InputKeyDown read m_OnKeyDown write m_OnKeyDown;
    property OnKeyUp: TG2InputKeyUp read m_OnKeyUp write m_OnKeyUp;
    property OnKeyPress: TG2InputKeyPress read m_OnKeyPress write m_OnKeyPress;
    property OnMouseEnter: TG2ProcObj read m_OnMouseEnter write m_OnMouseEnter;
    property OnMouseLeave: TG2ProcObj read m_OnMouseLeave write m_OnMouseLeave;
    property OnFocusReceive: TG2ProcObj read m_OnFocusReceive write m_OnFocusReceive;
    property OnFocusLose: TG2ProcObj read m_OnFocusLose write m_OnFocusLose;
    property OnResize: TG2ProcObj read m_OnResize write m_OnResize;
    function GetGlobalRect: TRect; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure PreRender;
    procedure Render; virtual;
    procedure PostRender;
    procedure RenderBox(
      const BoxRect: TRect;
      const Lowered: Boolean = False;
      const ColNormal: DWord = $ffcccccc;
      const ColHighlight: DWord = $ffffffff;
      const ColShadow: DWord = $ff555555
    ); overload;
    procedure RenderBox(
      const BoxRect: TRect;
      const MarginL, MarginT, MarginR, MarginB: Integer;
      const Lowered: Boolean = False;
      const ColNormal: DWord = $ffcccccc;
      const ColHighlight: DWord = $ffffffff;
      const ColShadow: DWord = $ff555555
    ); overload;
    procedure RenderTextBox(
      const BoxRect: TRect;
      const ColExt: DWord = $ff444444;
      const ColInt: DWord = $ffffffff
    );
    procedure RenderFrame(
      const BoxRect: TRect;
      const ColHighlight: DWord = $ffffffff;
      const ColShadow: DWord = $ff555555
    );
    procedure RenderArrowUp(
      const BoxRect: TRect;
      const Margin: Integer = 4;
      const Col: DWord = $ff000000
    );
    procedure RenderArrowDown(
      const BoxRect: TRect;
      const Margin: Integer = 4;
      const Col: DWord = $ff000000
    );
    procedure RenderArrowLeft(
      const BoxRect: TRect;
      const Margin: Integer = 4;
      const Col: DWord = $ff000000
    );
    procedure RenderArrowRight(
      const BoxRect: TRect;
      const Margin: Integer = 4;
      const Col: DWord = $ff000000
    );
    function IsPressed: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IsMouseOver: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function IsMouseDown: Boolean;
    procedure CustomMouseDown(const Button: Byte); virtual;
    procedure CustomMouseUp(const Button: Byte); virtual;
    procedure CustomMouseClick; virtual;
    procedure CustomMouseMove(const Shift: TPoint); virtual;
    procedure CustomMouseWheel(const Shift: Integer); virtual;
    procedure CustomKeyDown(const Key: Byte); virtual;
    procedure CustomKeyUp(const Key: Byte); virtual;
    procedure CustomKeyPress(const Key: AnsiChar); virtual;
    procedure CustomMouseEnter; virtual;
    procedure CustomMouseLeave; virtual;
    procedure CustomFocusReceive; virtual;
    procedure CustomFocusLose; virtual;
    procedure CustomResize; virtual;
    procedure CustomUpdate; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property GUI: TG2GUI read m_GUI write m_GUI;
    property Parent: TG2GUIWindow read m_Parent write SetParent;
    property X: Integer read m_X write m_X;
    property Y: Integer read m_Y write m_Y;
    property W: Integer read m_W write SetW;
    property H: Integer read m_H write SetH;
    property Visible: Boolean read m_Visible write m_Visible;
    property MarginLeft: Integer read m_MarginLeft write m_MarginLeft;
    property MarginTop: Integer read m_MarginTop write m_MarginTop;
    property MarginRight: Integer read m_MarginRight write m_MarginRight;
    property MarginBottom: Integer read m_MarginBottom write m_MarginBottom;
    property WndRect: TRect read GetRect write SetRect;
    property ClientRect: TRect read GetClientRect write SetClientRect;
    property DrawRect: TRect read GetDrawRect;
    property ClientDrawRect: TRect read GetClientDrawRect;
    property Focused: Boolean read GetFocused;
    procedure Draw; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Update; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFocus;
    procedure SetRectByGrid(const l, t, r, b: Integer);
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIWindow END

//TG2GUIPanel BEGIN
  TG2GUIPanel = class (TG2GUIWindow)
  strict protected
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIPanel END

//TG2GUIButton BEGIN
  TG2GUIButton = class (TG2GUIWindow)
  strict private
    m_Caption: AnsiString;
    m_Font: TG2Font;
    m_FontColor: TG2Color;
  strict protected
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Caption: AnsiString read m_Caption write m_Caption;
    property Font: TG2Font read m_Font write m_Font;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    property OnMouseClick;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIButton END

//TG2GUIEdit BEGIN
  TG2GUIEdit = class (TG2GUIWindow)
  strict private
    m_Text: AnsiString;
    m_Font: TG2Font;
    m_FontColor: TG2Color;
    m_CurPos: Integer;
    m_FlickerTime: DWord;
  strict protected
    procedure CustomMouseClick; override;
    procedure CustomKeyDown(const Key: Byte); override;
    procedure CustomKeyPress(const Key: AnsiChar); override;
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Text: AnsiString read m_Text write m_Text;
    property Font: TG2Font read m_Font write m_Font;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIEdit END

//TG2GUICheckBox BEGIN
  TG2GUICheckBox = class (TG2GUIWindow)
  strict private
    m_Caption: AnsiString;
    m_Font: TG2Font;
    m_FontColor: TG2Color;
    m_Checked: Boolean;
    m_OnChanged: TG2ProcObj;
  strict protected
    procedure CustomMouseDown(const Button: Byte); override;
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Caption: AnsiString read m_Caption write m_Caption;
    property Font: TG2Font read m_Font write m_Font;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    property Checked: Boolean read m_Checked write m_Checked;
    property OnChanged: TG2ProcObj read m_OnChanged write m_OnChanged;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUICheckBox END

//TG2GUIRadioButton BEGIN
  TG2GUIRadioButton = class (TG2GUIWindow)
  strict private
    m_Caption: AnsiString;
    m_Font: TG2Font;
    m_FontColor: TG2Color;
    m_Checked: Boolean;
    m_Group: Integer;
    procedure SetChecked(const Value: Boolean);
  strict protected
    procedure CustomMouseDown(const Button: Byte); override;
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Caption: AnsiString read m_Caption write m_Caption;
    property Font: TG2Font read m_Font write m_Font;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    property Checked: Boolean read m_Checked write SetChecked;
    property Group: Integer read m_Group write m_Group;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIRadioButton END

//TG2GUILabel BEGIN
  TG2GUILabel = class (TG2GUIWindow)
  strict private
    m_Caption: AnsiString;
    m_Font: TG2Font;
    m_FontColor: TG2Color;
    m_AutoSize: Boolean;
  strict protected
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Caption: AnsiString read m_Caption write m_Caption;
    property Font: TG2Font read m_Font write m_Font;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    property AutoSize: Boolean read m_AutoSize write m_AutoSize;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUILabel END

//TG2GUIMemoBox BEGIN
  TG2GUIMemoBox = class (TG2GUIWindow)
  strict private
    m_Caption: AnsiString;
    m_Font: TG2Font;
    m_FontColor: TG2Color;
    m_Text: TStringList;
    m_CurPos: TPoint;
    m_FlickerTime: DWord;
    m_ProcOnChange: TG2ProcObj;
    m_Scroll: TPoint;
    procedure LinesChanged(Sender: TObject);
  strict protected
    procedure CustomMouseClick; override;
    procedure CustomKeyDown(const Key: Byte); override;
    procedure CustomKeyPress(const Key: AnsiChar); override;
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Caption: AnsiString read m_Caption write m_Caption;
    property Font: TG2Font read m_Font write m_Font;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    property Text: TStringList read m_Text;
    property Scroll: TPoint read m_Scroll write m_Scroll;
    property CurPos: TPoint read m_CurPos write m_CurPos;
    property OnMouseClick;
    property OnWheelMove;
    property OnChange: TG2ProcObj read m_ProcOnChange write m_ProcOnChange;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIMemoBox END

//TG2GUIStringListBox BEGIN
  TG2GUIStringListBox = class (TG2GUIWindow)
  strict private
    m_Caption: AnsiString;
    m_Font: TG2Font;
    m_FontColor: TG2Color;
    m_List: TStringList;
    m_ItemIndex: Integer;
    m_Scroll: Integer;
    m_OnChange: TG2ProcObj;
    procedure OnListChange(Sender: TObject);
  strict protected
    procedure CustomMouseDown(const Button: Byte); override;
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Caption: AnsiString read m_Caption write m_Caption;
    property Font: TG2Font read m_Font write m_Font;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    property List: TStringList read m_List;
    property ItemIndex: Integer read m_ItemIndex write m_ItemIndex;
    property Scroll: Integer read m_Scroll write m_Scroll;
    property OnWheelMove;
    property OnChange: TG2ProcObj read m_OnChange write m_OnChange;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIStringListBox END

//TG2GUISlider BEGIN
  TG2GUISlider = class (TG2GUIWindow)
  strict private
    m_Font: TG2Font;
    m_FontColor: TG2Color;
    m_Progress: Single;
    m_Editable: Boolean;
    procedure AdjustSlider;
    procedure SetProgress(const Value: Single);
  strict protected
    procedure CustomMouseMove(const Shift: TPoint); override;
    procedure CustomMouseDown(const Button: Byte); override;
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Font: TG2Font read m_Font write m_Font;
    property FontColor: TG2Color read m_FontColor write m_FontColor;
    property Progress: Single read m_Progress write SetProgress;
    property Editable: Boolean read m_Editable write m_Editable;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUISlider END

//TG2GUIScrollBar BEGIN
  TG2GUIScrollBar = class (TG2GUIWindow)
  strict private
  type
    TG2GUIScrollBarOrient = (
      orVertical,
      orHorizontal
    );
  var
    m_Orient: TG2GUIScrollBarOrient;
    m_ButtonSize: Integer;
    m_Position: Single;
    m_SliderRatio: Single;
    m_Drag: Boolean;
    m_DragOffset: Integer;
    m_ScrollSpeed: Single;
    m_ScrollUp: Boolean;
    m_ScrollDown: Boolean;
    m_ProcOnScroll: TG2ProcObj;
    function SliderRect: TRect;
    function BtnUpRect(const GR: PRect): TRect;
    function BtnDownRect(const GR: PRect): TRect;
    procedure SetSlider(const Pos: Integer);
    procedure SetPosition(const Value: Single);
  strict protected
    procedure CustomMouseMove(const Shift: TPoint); override;
    procedure CustomMouseDown(const Button: Byte); override;
    procedure CustomMouseUp(const Button: Byte); override;
    procedure CustomMouseWheel(const Shift: Integer); override;
    procedure CustomUpdate; override;
    procedure Render; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Orient: TG2GUIScrollBarOrient read m_Orient write m_Orient;
    property ButtonSize: Integer read m_ButtonSize write m_ButtonSize;
    property Position: Single read m_Position write SetPosition;
    property SliderRatio: Single read m_SliderRatio write m_SliderRatio;
    property ScrollSpeed: Single read m_ScrollSpeed write m_ScrollSpeed;
    property OnScroll: TG2ProcObj read m_ProcOnScroll write m_ProcOnScroll;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    procedure Scroll(const Amount: Integer);
  end;
//TG2GUIScrollBar END

//TG2GUIMemo BEGIN
  TG2GUIMemo = class (TG2GUIWindow)
  strict private
    m_MemoBox: TG2GUIMemoBox;
    m_ScrollV: TG2GUIScrollBar;
    m_ScrollH: TG2GUIScrollBar;
    procedure AdjustScrollBars;
    procedure AdjustMemoBox;
    procedure PropMouseWheel(const Shift: Integer);
  strict protected
    procedure CustomResize; override;
    procedure Render; override;
    procedure CustomMouseWheel(const Shift: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MemoBox: TG2GUIMemoBox read m_MemoBox;
    property ScrollV: TG2GUIScrollBar read m_ScrollV;
    property ScrollH: TG2GUIScrollBar read m_ScrollH;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIMemo END

//TG2GUIStringList BEGIN
  TG2GUIStringList = class (TG2GUIWindow)
  strict private
    m_StrListBox: TG2GUIStringListBox;
    m_ScrollV: TG2GUIScrollBar;
    procedure AdjustScrollBar;
    procedure AdjustListBox;
    procedure PropMouseWheel(const Shift: Integer);
    function GetList: TStringList;
    procedure SetItemIndex(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetItemIndex: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  strict protected
    procedure CustomResize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property List: TStringList read GetList;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2GUIStringList END
*)
//TG2RenderStates BEGIN
  TG2RenderStates = class (TG2Class)
  strict private
    var m_States: array[0..255] of DWord;
    function GetBoolean(const StateType: TD3DRenderStateType): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBoolean(const StateType: TD3DRenderStateType; const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDWord(const StateType: TD3DRenderStateType): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDWord(const StateType: TD3DRenderStateType; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSingle(const StateType: TD3DRenderStateType): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSingle(const StateType: TD3DRenderStateType; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetZEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetZEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFillMode: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFillMode(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetShadeMode: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetShadeMode(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetZWriteEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetZWriteEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlphaTestEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAlphaTestEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLastPixel: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetLastPixel(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSrcBlend: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSrcBlend(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDestBlend: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDestBlend(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCullMode: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCullMode(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetZFunc: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetZFunc(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlphaRef: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAlphaRef(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlphaFunc: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAlphaFunc(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDitherEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDitherEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlphaBlendEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAlphaBlendEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFogEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFogEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSpecularEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSpecularEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFogColor: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFogColor(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFogTableMode: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFogTableMode(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFogVertexMode: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFogVertexMode(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFogStart: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFogStart(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFogEnd: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFogEnd(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFogDensity: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFogDensity(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetRangeFogEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetRangeFogEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetStencilEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetStencilEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetStencilFail: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetStencilFail(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetStencilZFail: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetStencilZFail(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetStencilPass: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetStencilPass(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetStencilFunc: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetStencilFunc(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetStencilRef: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetStencilRef(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetStencilMask: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetStencilMask(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetStencilWriteMask: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetStencilWriteMask(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTwoSidedStencilMode: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetTwoSidedStencilMode(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCCWStencilFail: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCCWStencilFail(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCCWStencilZFail: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCCWStencilZFail(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCCWStencilPass: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCCWStencilPass(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCCWStencilFunc: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCCWStencilFunc(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTextureFactor: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetTextureFactor(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap0: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap0(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap1: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap1(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap2: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap2(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap3: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap3(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap4: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap4(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap5: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap5(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap6: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap6(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap7: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap7(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap8: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap8(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap9: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap9(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap10: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap10(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap11: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap11(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap12: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap12(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap13: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap13(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap14: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap14(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetWrap15: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetWrap15(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetClipping: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetClipping(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLighting: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetLighting(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAmbient: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAmbient(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorVertex: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorVertex(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetLocalViewer: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetLocalViewer(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetNormalizeNormals: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetNormalizeNormals(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDiffuseMaterialSource: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDiffuseMaterialSource(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSpecularMaterialSource: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSpecularMaterialSource(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAmbientMaterialSource: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAmbientMaterialSource(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetEmissiveMaterialSource: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetEmissiveMaterialSource(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVertexBlend: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetVertexBlend(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetClipPlaneEnable: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetClipPlaneEnable(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPointSize: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPointSize(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPointSizeMin: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPointSizeMin(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPointSizeMax: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPointSizeMax(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPointSpriteEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPointSpriteEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPointScaleEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPointScaleEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPointScaleA: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPointScaleA(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPointScaleB: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPointScaleB(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPointScaleC: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPointScaleC(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMultisampleAntialias: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMultisampleAntialias(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMultisampleMask: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMultisampleMask(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPatchEdgeStyle: TD3DPatchEdgeStyle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPatchEdgeStyle(const Value: TD3DPatchEdgeStyle); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDebugMonitorToken: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDebugMonitorToken(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetIndexedVertexBlendEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetIndexedVertexBlendEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorWriteEnable: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorWriteEnable(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTweenFactor: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetTweenFactor(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBlendOp: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBlendOp(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPositionDegree: TD3DDegreeType; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetPositionDegree(const Value: TD3DDegreeType); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetNormalDegree: TD3DDegreeType; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetNormalDegree(const Value: TD3DDegreeType); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetScissorTestEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetScissorTestEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSlopeScaleDepthBias: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSlopeScaleDepthBias(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAntialiasedLineEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAntialiasedLineEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMinTessellationLevel: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMinTessellationLevel(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMaxTessellationLevel: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMaxTessellationLevel(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAdaptiveTessX: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAdaptiveTessX(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAdaptiveTessY: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAdaptiveTessY(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAdaptiveTessZ: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAdaptiveTessZ(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAdaptiveTessW: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAdaptiveTessW(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetEnableAdaptiveTessellation: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetEnableAdaptiveTessellation(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorWriteEnable1: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorWriteEnable1(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorWriteEnable2: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorWriteEnable2(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorWriteEnable3: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorWriteEnable3(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBlendFactor: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBlendFactor(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSRGBWriteEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSRGBWriteEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDepthBias: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDepthBias(const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSeparateAlphaBlendEnable: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSeparateAlphaBlendEnable(const Value: Boolean); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSrcBlendAlpha: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSrcBlendAlpha(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDestBlendAlpha: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDestBlendAlpha(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBlendOpAlpha: DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBlendOpAlpha(const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var m_Gfx: TG2Graphics;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ZEnable: Boolean read GetZEnable write SetZEnable;
    property FillMode: DWord read GetFillMode write SetFillMode;
    property ShadeMode: DWord read GetShadeMode write SetShadeMode;
    property ZWriteEnable: Boolean read GetZWriteEnable write SetZWriteEnable;
    property AlphaTestEnable: Boolean read GetAlphaTestEnable write SetAlphaTestEnable;
    property LastPixel: Boolean read GetLastPixel write SetLastPixel;
    property SrcBlend: DWord read GetSrcBlend write SetSrcBlend;
    property DestBlend: DWord read GetDestBlend write SetDestBlend;
    property CullMode: DWord read GetCullMode write SetCullMode;
    property ZFunc: DWord read GetZFunc write SetZFunc;
    property AlphaRef: DWord read GetAlphaRef write SetAlphaRef;
    property AlphaFunc: DWord read GetAlphaFunc write SetAlphaFunc;
    property DitherEnable: Boolean read GetDitherEnable write SetDitherEnable;
    property AlphaBlendEnable: Boolean read GetAlphaBlendEnable write SetAlphaBlendEnable;
    property FogEnable: Boolean read GetFogEnable write SetFogEnable;
    property SpecularEnable: Boolean read GetSpecularEnable write SetSpecularEnable;
    property FogColor: DWord read GetFogColor write SetFogColor;
    property FogTableMode: DWord read GetFogTableMode write SetFogTableMode;
    property FogVertexMode: DWord read GetFogVertexMode write SetFogVertexMode;
    property FogStart: Single read GetFogStart write SetFogStart;
    property FogEnd: Single read GetFogEnd write SetFogEnd;
    property FogDensity: Single read GetFogDensity write SetFogDensity;
    property RangeFogEnable: Boolean read GetRangeFogEnable write SetRangeFogEnable;
    property StencilEnable: Boolean read GetStencilEnable write SetStencilEnable;
    property StencilFail: DWord read GetStencilFail write SetStencilFail;
    property StencilZFail: DWord read GetStencilZFail write SetStencilZFail;
    property StencilPass: DWord read GetStencilPass write SetStencilPass;
    property StencilFunc: DWord read GetStencilFunc write SetStencilFunc;
    property StencilRef: DWord read GetStencilRef write SetStencilRef;
    property StencilMask: DWord read GetStencilMask write SetStencilMask;
    property StencilWriteMask: DWord read GetStencilWriteMask write SetStencilWriteMask;
    property TwoSidedStencilMode: Boolean read GetTwoSidedStencilMode write SetTwoSidedStencilMode;
    property CCWStencilFail: DWord read GetCCWStencilFail write SetCCWStencilFail;
    property CCWStencilZFail: DWord read GetCCWStencilZFail write SetCCWStencilZFail;
    property CCWStencilPass: DWord read GetCCWStencilPass write SetCCWStencilPass;
    property CCWStencilFunc: DWord read GetCCWStencilFunc write SetCCWStencilFunc;
    property TextureFactor: DWord read GetTextureFactor write SetTextureFactor;
    property Wrap0: DWord read GetWrap0 write SetWrap0;
    property Wrap1: DWord read GetWrap1 write SetWrap1;
    property Wrap2: DWord read GetWrap2 write SetWrap2;
    property Wrap3: DWord read GetWrap3 write SetWrap3;
    property Wrap4: DWord read GetWrap4 write SetWrap4;
    property Wrap5: DWord read GetWrap5 write SetWrap5;
    property Wrap6: DWord read GetWrap6 write SetWrap6;
    property Wrap7: DWord read GetWrap7 write SetWrap7;
    property Wrap8: DWord read GetWrap8 write SetWrap8;
    property Wrap9: DWord read GetWrap9 write SetWrap9;
    property Wrap10: DWord read GetWrap10 write SetWrap10;
    property Wrap11: DWord read GetWrap11 write SetWrap11;
    property Wrap12: DWord read GetWrap12 write SetWrap12;
    property Wrap13: DWord read GetWrap13 write SetWrap13;
    property Wrap14: DWord read GetWrap14 write SetWrap14;
    property Wrap15: DWord read GetWrap15 write SetWrap15;
    property Clipping: Boolean read GetClipping write SetClipping;
    property Lighting: Boolean read GetLighting write SetLighting;
    property Ambient: DWord read GetAmbient write SetAmbient;
    property ColorVertex: Boolean read GetColorVertex write SetColorVertex;
    property LocalViewer: Boolean read GetLocalViewer write SetLocalViewer;
    property NormalizeNormals: Boolean read GetNormalizeNormals write SetNormalizeNormals;
    property DiffuseMaterialSource: DWord read GetDiffuseMaterialSource write SetDiffuseMaterialSource;
    property SpecularMaterialSource: DWord read GetSpecularMaterialSource write SetSpecularMaterialSource;
    property AmbientMaterialSource: DWord read GetAmbientMaterialSource write SetAmbientMaterialSource;
    property EmissiveMaterialSource: DWord read GetEmissiveMaterialSource write SetEmissiveMaterialSource;
    property VertexBlend: DWord read GetVertexBlend write SetVertexBlend;
    property ClipPlaneEnable: DWord read GetClipPlaneEnable write SetClipPlaneEnable;
    property PointSize: Single read GetPointSize write SetPointSize;
    property PointSizeMin: Single read GetPointSizeMin write SetPointSizeMin;
    property PointSizeMax: Single read GetPointSizeMax write SetPointSizeMax;
    property PointSpriteEnable: Boolean read GetPointSpriteEnable write SetPointSpriteEnable;
    property PointScaleEnable: Boolean read GetPointScaleEnable write SetPointScaleEnable;
    property PointScaleA: Single read GetPointScaleA write SetPointScaleA;
    property PointScaleB: Single read GetPointScaleB write SetPointScaleB;
    property PointScaleC: Single read GetPointScaleC write SetPointScaleC;
    property MultisampleAntialias: Boolean read GetMultisampleAntialias write SetMultisampleAntialias;
    property MultisampleMask: DWord read GetMultisampleMask write SetMultisampleMask;
    property PatchEdgeStyle: TD3DPatchEdgeStyle read GetPatchEdgeStyle write SetPatchEdgeStyle;
    property DebugMonitorToken: DWord read GetDebugMonitorToken write SetDebugMonitorToken;
    property IndexedVertexBlendEnable: Boolean read GetIndexedVertexBlendEnable write SetIndexedVertexBlendEnable;
    property ColorWriteEnable: DWord read GetColorWriteEnable write SetColorWriteEnable;
    property TweenFactor: Single read GetTweenFactor write SetTweenFactor;
    property BlendOp: DWord read GetBlendOp write SetBlendOp;
    property PositionDegree: TD3DDegreeType read GetPositionDegree write SetPositionDegree;
    property NormalDegree: TD3DDegreeType read GetNormalDegree write SetNormalDegree;
    property ScissorTestEnable: Boolean read GetScissorTestEnable write SetScissorTestEnable;
    property SlopeScaleDepthBias: Single read GetSlopeScaleDepthBias write SetSlopeScaleDepthBias;
    property AntialiasedLineEnable: Boolean read GetAntialiasedLineEnable write SetAntialiasedLineEnable;
    property MinTessellationLevel: Single read GetMinTessellationLevel write SetMinTessellationLevel;
    property MaxTessellationLevel: Single read GetMaxTessellationLevel write SetMaxTessellationLevel;
    property AdaptiveTessX: DWord read GetAdaptiveTessX write SetAdaptiveTessX;
    property AdaptiveTessY: DWord read GetAdaptiveTessY write SetAdaptiveTessY;
    property AdaptiveTessZ: DWord read GetAdaptiveTessZ write SetAdaptiveTessZ;
    property AdaptiveTessW: DWord read GetAdaptiveTessW write SetAdaptiveTessW;
    property EnableAdaptiveTessellation: Boolean read GetEnableAdaptiveTessellation write SetEnableAdaptiveTessellation;
    property ColorWriteEnable1: DWord read GetColorWriteEnable1 write SetColorWriteEnable1;
    property ColorWriteEnable2: DWord read GetColorWriteEnable2 write SetColorWriteEnable2;
    property ColorWriteEnable3: DWord read GetColorWriteEnable3 write SetColorWriteEnable3;
    property BlendFactor: DWord read GetBlendFactor write SetBlendFactor;
    property SRGBWriteEnable: Boolean read GetSRGBWriteEnable write SetSRGBWriteEnable;
    property DepthBias: Single read GetDepthBias write SetDepthBias;
    property SeparateAlphaBlendEnable: Boolean read GetSeparateAlphaBlendEnable write SetSeparateAlphaBlendEnable;
    property SrcBlendAlpha: DWord read GetSrcBlendAlpha write SetSrcBlendAlpha;
    property DestBlendAlpha: DWord read GetDestBlendAlpha write SetDestBlendAlpha;
    property BlendOpAlpha: DWord read GetBlendOpAlpha write SetBlendOpAlpha;
    procedure SetDefaults;
    procedure SetFromMemory;
  end;
//TG2RenderStates END

//TG2SamplerStates BEGIN
  TG2SamplerStates = class (TG2Class)
  strict private
    var m_States: array[0..15, 1..13] of DWord;
    function GetDWord(const Sampler: Byte; const StateType: TD3DSamplerStateType): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDWord(const Sampler: Byte; const StateType: TD3DSamplerStateType; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSingle(const Sampler: Byte; const StateType: TD3DSamplerStateType): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSingle(const Sampler: Byte; const StateType: TD3DSamplerStateType; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAddressU(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAddressU(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAddressV(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAddressV(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAddressW(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAddressW(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBorderColor(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBorderColor(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMagFilter(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMagFilter(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMinFilter(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMinFilter(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMipFilter(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMipFilter(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMipMapLODBias(const Index: Byte): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMipMapLODBias(const Index: Byte; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMaxMipLevel(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMaxMipLevel(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMaxAnisotropy(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetMaxAnisotropy(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSRGBTexture(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSRGBTexture(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetElementIndex(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetElementIndex(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDMapOffset(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDMapOffset(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var m_Gfx: TG2Graphics;
  public
    constructor Create; override;
    destructor Destroy; override;
    property AddressU[const Index: Byte]: DWord read GetAddressU write SetAddressU;
    property AddressV[const Index: Byte]: DWord read GetAddressV write SetAddressV;
    property AddressW[const Index: Byte]: DWord read GetAddressW write SetAddressW;
    property BorderColor[const Index: Byte]: DWord read GetBorderColor write SetBorderColor;
    property MagFilter[const Index: Byte]: DWord read GetMagFilter write SetMagFilter;
    property MinFilter[const Index: Byte]: DWord read GetMinFilter write SetMinFilter;
    property MipFilter[const Index: Byte]: DWord read GetMipFilter write SetMipFilter;
    property MipMapLODBias[const Index: Byte]: Single read GetMipMapLODBias write SetMipMapLODBias;
    property MaxMipLevel[const Index: Byte]: DWord read GetMaxMipLevel write SetMaxMipLevel;
    property MaxAnisotropy[const Index: Byte]: DWord read GetMaxAnisotropy write SetMaxAnisotropy;
    property SRGBTexture[const Index: Byte]: DWord read GetSRGBTexture write SetSRGBTexture;
    property ElementIndex[const Index: Byte]: DWord read GetElementIndex write SetElementIndex;
    property DMapOffset[const Index: Byte]: DWord read GetDMapOffset write SetDMapOffset;
    procedure SetDefaults;
    procedure SetFromMemory;
  end;
//TG2SamplerStates END

//TG2TextureStageStates BEGIN
  TG2TextureStageStates = class (TG2Class)
  strict private
    var m_States: array[0..15, 1..32] of DWord;
    function GetDWord(const Index: Byte; const StageType: TD3DTextureStageStateType): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetDWord(const Index: Byte; const StageType: TD3DTextureStageStateType; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetSingle(const Index: Byte; const StageType: TD3DTextureStageStateType): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetSingle(const Index: Byte; const StageType: TD3DTextureStageStateType; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorOp(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorOp(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorArg0(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorArg0(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorArg1(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorArg1(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetColorArg2(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetColorArg2(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlphaOp(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAlphaOp(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlphaArg0(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAlphaArg0(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlphaArg1(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAlphaArg1(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAlphaArg2(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetAlphaArg2(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBumpEnvMat00(const Index: Byte): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBumpEnvMat00(const Index: Byte; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBumpEnvMat01(const Index: Byte): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBumpEnvMat01(const Index: Byte; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBumpEnvMat10(const Index: Byte): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBumpEnvMat10(const Index: Byte; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBumpEnvMat11(const Index: Byte): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBumpEnvMat11(const Index: Byte; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTexCoordIndex(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetTexCoordIndex(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBumpEnvLScale(const Index: Byte): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBumpEnvLScale(const Index: Byte; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBumpEnvLOffset(const Index: Byte): Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetBumpEnvLOffset(const Index: Byte; const Value: Single); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTextureTransformFlags(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetTextureTransformFlags(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetResultArg(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetResultArg(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetConstant(const Index: Byte): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetConstant(const Index: Byte; const Value: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  private
    var m_Gfx: TG2Graphics;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ColorOp[const Index: Byte]: DWord read GetColorOp write SetColorOp;
    property ColorArg0[const Index: Byte]: DWord read GetColorArg0 write SetColorArg0;
    property ColorArg1[const Index: Byte]: DWord read GetColorArg1 write SetColorArg1;
    property ColorArg2[const Index: Byte]: DWord read GetColorArg2 write SetColorArg2;
    property AlphaOp[const Index: Byte]: DWord read GetAlphaOp write SetAlphaOp;
    property AlphaArg0[const Index: Byte]: DWord read GetAlphaArg0 write SetAlphaArg0;
    property AlphaArg1[const Index: Byte]: DWord read GetAlphaArg1 write SetAlphaArg1;
    property AlphaArg2[const Index: Byte]: DWord read GetAlphaArg2 write SetAlphaArg2;
    property BumpEnvMat00[const Index: Byte]: Single read GetBumpEnvMat00 write SetBumpEnvMat00;
    property BumpEnvMat01[const Index: Byte]: Single read GetBumpEnvMat01 write SetBumpEnvMat01;
    property BumpEnvMat10[const Index: Byte]: Single read GetBumpEnvMat10 write SetBumpEnvMat10;
    property BumpEnvMat11[const Index: Byte]: Single read GetBumpEnvMat11 write SetBumpEnvMat11;
    property TexCoordIndex[const Index: Byte]: DWord read GetTexCoordIndex write SetTexCoordIndex;
    property BumpEnvLScale[const Index: Byte]: Single read GetBumpEnvLScale write SetBumpEnvLScale;
    property BumpEnvLOffset[const Index: Byte]: Single read GetBumpEnvLOffset write SetBumpEnvLOffset;
    property TextureTransformFlags[const Index: Byte]: DWord read GetTextureTransformFlags write SetTextureTransformFlags;
    property ResultArg[const Index: Byte]: DWord read GetResultArg write SetResultArg;
    property Constant[const Index: Byte]: DWord read GetConstant write SetConstant;
    procedure SetDefaults;
    procedure SetFromMemory;
  end;
//TG2TextureStageStates END

//TG2SharedVB2D BEGIN
  TG2SharedVertex2D = packed record
  public
    var x, y, z, rhw: Single;
    var Color: TG2Color;
    var tu, tv: Single;
  end;
  PG2SharedVertex2DArray = ^TG2SharedVertex2DArray;
  TG2SharedVertex2DArray = array[0..0] of TG2SharedVertex2D;

  TG2SharedVB2D = class (TG2Class)
  strict private
    var m_Gfx: TG2Graphics;
    var m_VB: IDirect3DVertexBuffer9;
    var m_Size: Integer;
    function InitializeBuffer(const Size: Integer): TG2Result;
  private
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  public
    constructor Create; override;
    destructor Destroy; override;
    property VB: IDirect3DVertexBuffer9 read m_VB;
    procedure Initialize(const G2Graphics: TG2Graphics);
    procedure Finalize;
    procedure SetToDevice;
    function VerifySize(const Size: Integer): TG2Result;
  end;
//TG2SharedVB2D END

//TG2Render BEGIN
  TG2Render = class (TG2Module)
  strict private
    var m_Gfx: TG2Graphics;
    function GetCanRender: Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property CanRender: Boolean read GetCanRender;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function RenderStart: TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function RenderStop: TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Present: TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Clear(
      ClearStencil: Boolean;
      ClearZBuffer: Boolean;
      ClearTarget: Boolean;
      Color: TG2Color;
      Depth: Single = 1;
      Stencil: DWord = 0
    ); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure TextureClear(const Stage: Byte = 0); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure TextureSet(const Texture: TG2TextureBase; const Stage: Byte = 0); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;
//TG2Render END

//TG2RenderModes BEGIN
  TG2RenderModes = class (TG2Module)
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure BlendModeNormal;
    procedure BlendModeAdd;
    procedure BlendModeAddColor;
    procedure BlendModeSubColor;
    procedure BlendModeMultiply;
    procedure FilteringPoint(const Stage: Byte = 0);
    procedure FilteringLinear(const Stage: Byte = 0);
    procedure FilteringAnisotropic(const Stage: Byte = 0; const MaxAnisotropy: Integer = 8);
    procedure TexAddressClamp(const Stage: Byte = 0);
    procedure TexAddressWrap(const Stage: Byte = 0);
    procedure TexAddressBorder(const Stage: Byte = 0);
    procedure TexAddressMirror(const Stage: Byte = 0);
    procedure ScissorSet(const R: TRect);
    procedure ScissorDisable;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2RenderModes END

//TG2Render2D BEGIN
  TG2Render2D = class (TG2Module)
  strict private
    type TVertex = packed record
    public
      x, y, z, rhw: Single;
      Color: TG2Color;
      tu, tv: Single;
    end;
    type TVertexArray = array[0..3] of TVertex;
    type PVertexArray = ^TVertexArray;
    type TVertex2 = packed record
    public
      var x, y, z, rhw: Single;
      var Color: TG2Color;
      var tu1, tv1: Single;
      var tu2, tv2: Single;
    end;
    type TVertex2Array = array[0..3] of TVertex2;
    type PVertex2Array = ^TVertex2Array;
    type TVertex3 = packed record
    public
      var x, y, z, rhw: Single;
      var Color: TG2Color;
      var tu1, tv1: Single;
      var tu2, tv2: Single;
      var tu3, tv3: Single;
    end;
    type TVertex3Array = array[0..3] of TVertex3;
    type PVertex3Array = ^TVertex3Array;
    type TDrawQuadFunc = function (
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result of object;
    type TDrawQuadRawFunc = function (
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result of object;
    var m_Gfx: TG2Graphics;
    var m_PlugGraphics: TG2PlugGraphics;
    var m_VB: IDirect3DVertexBuffer9;
    var m_VBSize: Integer;
    var m_VB2: IDirect3DVertexBuffer9;
    var m_VB2Size: Integer;
    var m_VB3: IDirect3DVertexBuffer9;
    var m_VB3Size: Integer;
    var m_IB: IDirect3DIndexBuffer9;
    var m_IBSize: Integer;
    var m_IBBatch: TG2IB;
    var m_DrawQuadFunc: TDrawQuadFunc;
    var m_DrawQuadRawFunc: TDrawQuadRawFunc;
    var m_MaxQuads: Integer;
    var m_CurQuad: Integer;
    var m_BatchDrawCalls: DWord;
    var m_CurTexture: TG2Texture2DBase;
    var m_Batching: Boolean;
    var m_Vertices: array of TVertex;
    var m_Drawing: Boolean;
    var m_PrimType: TG2PrimType;
    var m_PosArr: array of TG2Vec2;
    var m_ColArr: array of TG2Color;
    var m_TexArr: array of TG2Vec2;
    var m_IndArr: array of Word;
    var m_CurPos: Int64;
    var m_CurCol: Int64;
    var m_CurTex: Int64;
    var m_CurInd: Int64;
    var m_BaseVertexIndex: DWord;
    const FVF = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1;
    const FVF2 = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX2;
    const FVF3 = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX3;
    function VerifyBuffer(const Size: Integer): TG2Result;
    function VerifyBuffer2(const Size: Integer): TG2Result;
    function VerifyBuffer3(const Size: Integer): TG2Result;
    function VerifyIndices(const Size: Integer): TG2Result;
    procedure OnDeviceLost;
    procedure OnDeviceReset;
    function LoadBuffers: TG2Result;
    function DrawPointList: TG2Result;
    function DrawLineList: TG2Result;
    function DrawLineStrip: TG2Result;
    function DrawTriangleList: TG2Result;
    function DrawTriangleStrip: TG2Result;
    function DrawTriangleFan: TG2Result;
    function DrawQuadImmediate(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
    function DrawQuadBatched(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
    function DrawQuadRawImmediate(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
    function DrawQuadRawBatched(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
  public
    constructor Create; override;
    destructor Destroy; override;
    property BatchDrawCalls: DWord read m_BatchDrawCalls;
    property BaseVertexIndex: DWord read m_BaseVertexIndex write m_BaseVertexIndex;
    property CurPos: Int64 read m_CurPos;
    property CurCol: Int64 read m_CurCol;
    property CurTex: Int64 read m_CurTex;
    property CurInd: Int64 read m_CurInd;
    function DrawQuad(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect(
      const X, Y: Single;
      const Texture: TG2Texture2DBase
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect(
      const R: TRect;
      const Texture: TG2Texture2DBase
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect(
      const X, Y, Width, Height: Single;
      const TexRect: TRect;
      const Texture: TG2Texture2DBase
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect(
      const X, Y, Width, Height: Single;
      const TexRect: TRect;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase
    ): TG2Result; overload;
    function DrawRect(
      const X, Y, Width, Height: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect(
      const X, Y, Width, Height: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const Rotation: Single;
      const ScaleX, ScaleY: Single;
      const FlipLeftRight: Boolean;
      const FlipTopBottom: Boolean
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect(
      const X, Y, Width, Height: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const Rotation: Single;
      const ScaleX, ScaleY: Single;
      const FlipLeftRight: Boolean;
      const FlipTopBottom: Boolean;
      const PatternWidth: Integer;
      const PatternHeight: Integer;
      const PatternFrame: Integer
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawQuad2(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture0: TG2Texture2DBase;
      const t0c1, t0c2, t0c3, t0c4: TG2Vec2;
      const Texture1: TG2Texture2DBase;
      const t1c1, t1c2, t1c3, t1c4: TG2Vec2
    ): TG2Result;
    function DrawQuad3(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture0: TG2Texture2DBase;
      const t0c1, t0c2, t0c3, t0c4: TG2Vec2;
      const Texture1: TG2Texture2DBase;
      const t1c1, t1c2, t1c3, t1c4: TG2Vec2;
      const Texture2: TG2Texture2DBase;
      const t2c1, t2c2, t2c3, t2c4: TG2Vec2
    ): TG2Result;
    function DrawQuadRaw(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawBegin(const PrimType: TG2PrimType): TG2Result;
    procedure AddPos(const p: TG2Vec2); overload;
    procedure AddPos(const x, y: Single); overload;
    procedure AddCol(const c: TG2Color); overload;
    procedure AddCol(const c: TG2Color; const Count: Integer); overload;
    procedure AddTex(const t: TG2Vec2);
    procedure AddInd(const i: Word);
    procedure AddFace(const i0, i1, i2: Word);
    function DrawEnd: TG2Result;
    function BatchBegin(const MaxQuads: Integer = 1000): TG2Result;
    function BatchEnd: TG2Result;
    function BatchFlush: TG2Result;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Render2D END

//TG2Render3D BEGIN
  TG2Render3D = class (TG2Module)
  strict private
    type TVertex = packed record
    public
      var x, y, z: Single;
      var nx, ny, nz: Single;
      var Color: TG2Color;
      var tu, tv: Single;
    end;
    type TVertex2 = packed record
    public
      var x, y, z: Single;
      var nx, ny, nz: Single;
      var Color: TG2Color;
      var tu0, tv0: Single;
      var tu1, tv1: Single;
    end;
    type TVertexArray = array[Word] of TVertex;
    type PVertexArray = ^TVertexArray;
    type TVertex2Array = array[Word] of TVertex2;
    type PVertex2Array = ^TVertex2Array;
    type TIndex = type Word;
    type TIndexArray = array[Word] of TIndex;
    type PIndexArray = ^TIndexArray;
    const FVF = D3DFVF_XYZ or D3DFVF_DIFFUSE or D3DFVF_NORMAL or D3DFVF_TEX1;
    const FVF2 = D3DFVF_XYZ or D3DFVF_DIFFUSE or D3DFVF_NORMAL or D3DFVF_TEX2;
    var m_PlugGraphics: TG2PlugGraphics;
    var m_VB: IDirect3DVertexBuffer9;
    var m_VB2: IDirect3DVertexBuffer9;
    var m_IB: IDirect3DIndexBuffer9;
    var m_VBSize: Integer;
    var m_VB2Size: Integer;
    var m_IBSize: Integer;
    var m_CurV: Word;
    var m_CurI: DWord;
    var m_Vertices: PVertexArray;
    var m_Vertices2: PVertex2Array;
    var m_Indices: PIndexArray;
    var m_Drawing: Boolean;
    var m_PrimType: TG2PrimType;
    var m_PosArr: array of TG2Vec3;
    var m_ColArr: array of TG2Color;
    var m_TexArr: array of TG2Vec2;
    var m_NrmArr: array of TG2Vec3;
    var m_IndArr: array of Word;
    var m_CurPos: Int64;
    var m_CurCol: Int64;
    var m_CurTex: Int64;
    var m_CurNrm: Int64;
    var m_CurInd: Int64;
    var m_BaseVertexIndex: DWord;
    function VerifyVB(const Size: Integer): TG2Result;
    function VerifyVB2(const Size: Integer): TG2Result;
    function VerifyIB(const Size: Integer): TG2Result;
    function AddVertex(
      const X, Y, Z: Single;
      const Color: TG2Color;
      const NX, NY, NZ: Single;
      const TU: Single = 0;
      const TV: Single = 0
    ): Word;
    procedure AddFace(
      const v1, v2, v3: Word
    );
    procedure OnDeviceLost;
    procedure OnDeviceReset;
    function LoadBuffers: TG2Result;
    function DrawPointList: TG2Result;
    function DrawLineList: TG2Result;
    function DrawLineStrip: TG2Result;
    function DrawTriangleList: TG2Result;
    function DrawTriangleStrip: TG2Result;
    function DrawTriangleFan: TG2Result;
  public
    constructor Create; override;
    destructor Destroy; override;
    property BaseVertexIndex: DWord read m_BaseVertexIndex write m_BaseVertexIndex;
    property CurPos: Int64 read m_CurPos;
    property CurCol: Int64 read m_CurCol;
    property CurTex: Int64 read m_CurTex;
    property CurNrm: Int64 read m_CurNrm;
    property CurInd: Int64 read m_CurInd;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function DrawTriangle(
      const v1, v2, v3: TG2Vec3;
      const Color: TG2Color;
      const tc1, tc2, tc3: TG2Vec2
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawTriangle3Col(
      const v1, v2, v3: TG2Vec3;
      const c1, c2, c3: TG2Color;
      const tc1, tc2, tc3: TG2Vec2
    ): TG2Result;
    function DrawQuad(
      const v1, v2, v3, v4: TG2Vec3;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const TextureRect: PG2Rect = nil
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawQuad4Col(
      const v1, v2, v3, v4: TG2Vec3;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const TextureRect: PG2Rect = nil
    ): TG2Result;
    function DrawQuad2(
      const v1, v2, v3, v4: TG2Vec3;
      const Color: TG2Color;
      const Texture0: TG2Texture2DBase;
      const TextureRect0: PG2Rect = nil;
      const Texture1: TG2Texture2DBase = nil;
      const TextureRect1: PG2Rect = nil
    ): TG2Result;
    function DrawBox(
      const X, Y, Z: Single;
      const SizeX, SizeY, SizeZ: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase
    ): TG2Result;
    function DrawSphere(
      const X, Y, Z: Single;
      const Radius: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const VSegments: Integer = 32;
      const HSegments: Integer = 32
    ): TG2Result;
    function DrawCylinder(
      const X, Y, Z: Single;
      const Height, Radius: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const Segments: Integer = 32
    ): TG2Result;
    function DrawCapsule(
      const X, Y, Z: Single;
      const Height, Radius: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const VCapSegments: Integer = 8;
      const HSegments: Integer = 32
    ): TG2Result;
    function DrawTorus(
      const X, Y, Z: Single;
      const RadiusInt, RadiusExt: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const Sides: Integer = 16;
      const Segments: Integer = 32
    ): TG2Result;
    function DrawAxisRect(
      const Pos0, Pos1: TG2Vec3;
      const Rad0, Rad1: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const TextureRect: PG2Rect = nil
    ): TG2Result;
    function DrawBegin(const PrimType: TG2PrimType): TG2Result;
    procedure AddPos(const p: TG2Vec3); overload;
    procedure AddPos(const x, y, z: Single); overload;
    procedure AddCol(const c: TG2Color); overload;
    procedure AddCol(const c: TG2Color; const Count: Integer); overload;
    procedure AddTex(const t: TG2Vec2); overload;
    procedure AddTex(const u, v: Single); overload;
    procedure AddNrm(const n: TG2Vec3); overload;
    procedure AddNrm(const x, y, z: Single); overload;
    procedure AddInd(const i: Word);
    function DrawEnd: TG2Result;
  end;
//TG2Render3D END

//TG2Camera BEGIN
  TG2Camera = class (TG2Module)
  strict private
    type TG2CameraDragMode = (
      cdDragDir,
      cdDragPos,
      cdStrafe
    );
    var m_Src: TG2Vec3;
    var m_Dst: TG2Vec3;
    var m_Up: TG2Vec3;
    var m_FOV: Single;
    var m_Aspect: Single;
    var m_Near: Single;
    var m_Far: Single;
    var m_AngH: Single;
    var m_AngV: Single;
    var m_MinAngV: Single;
    var m_MaxAngV: Single;
    var m_Speed: Single;
    var m_Sensitivity: Single;
    var m_StrafeSpeed: Single;
    var m_UpdatePos: Boolean;
    var m_UpdateDir: Boolean;
    var m_BtnLeft: Byte;
    var m_BtnRight: Byte;
    var m_BtnForward: Byte;
    var m_BtnBackward: Byte;
    var m_Mode: TG2CameraMode;
    var m_DragMode: TG2CameraDragMode;
    var m_PlugInput: TG2PlugInput;
    function GetView: TG2Mat;
    function GetProj: TG2Mat;
    procedure AnglesFromVectors; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure VectorsFromAngles; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure OnMouseMove(const Shift: TPoint);
    function GetPos: PG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTarget: PG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetUp: PG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property View: TG2Mat read GetView;
    property Proj: TG2Mat read GetProj;
    property Pos: PG2Vec3 read GetPos;
    property Target: PG2Vec3 read GetTarget;
    property Up: PG2Vec3 read GetUp;
    property FOV: Single read m_FOV write m_FOV;
    property Aspect: Single read m_Aspect write m_Aspect;
    property ZNear: Single read m_Near write m_Near;
    property ZFar: Single read m_Far write m_Far;
    property MaxAngV: Single read m_MaxAngV write m_MaxAngV;
    property MinAngV: Single read m_MinAngV write m_MinAngV;
    property Speed: Single read m_Speed write m_Speed;
    property Sensitivity: Single read m_Sensitivity write m_Sensitivity;
    property StrafeSpeed: Single read m_StrafeSpeed write m_StrafeSpeed;
    property BtnLeft: Byte read m_BtnLeft write m_BtnLeft;
    property BtnRight: Byte read m_BtnRight write m_BtnRight;
    property BtnForward: Byte read m_BtnForward write m_BtnForward;
    property BtnBackward: Byte read m_BtnBackward write m_BtnBackward;
    property UpdatePos: Boolean read m_UpdatePos write m_UpdatePos;
    property UpdateDir: Boolean read m_UpdateDir write m_UpdateDir;
    property Mode: TG2CameraMode read m_Mode write m_Mode;
    property DragMode: TG2CameraDragMode read m_DragMode write m_DragMode;
    procedure SetPerspective(const NewFOV, NewAspect, NewNear, NewFar: Single); overload;
    procedure SetPerspective; overload;
    procedure SetView(const PosX, PosY, PosZ, TargetX, TargetY, TargetZ, UpX, UpY, UpZ: Single); overload;
    procedure SetView(const NewPos, NewTarget, NewUp: TG2Vec3); overload;
    procedure Update;
    procedure GetViewSpaceVectors(var VecX, VecY, VecZ: TG2Vec3);
    procedure MoveTo(const NewPos: TG2Vec3);
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Camera END

//TG2Primitives2D BEGIN
  TG2Primitives2D = class (TG2Module)
  strict private
    type TG2Vertex = packed record
    public
      var x, y, z, rhw: Single;
      var Color: TG2Color
    end;
    type TG2VertexArray = array[Word] of TG2Vertex;
    type PG2VertexArray = ^TG2VertexArray;
    const FVF = D3DFVF_XYZRHW or D3DFVF_DIFFUSE;
    var m_PlugGraphics: TG2PlugGraphics;
    var m_VB: IDirect3DVertexBuffer9;
    var m_IB: IDirect3DIndexBuffer9;
    var m_VBSize: Integer;
    var m_IBSize: Integer;
    function VerifyVB(const Size: Integer): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function VerifyIB(const Size: Integer): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function DrawLine(
      const v1, v2: TG2Vec2;
      const Color: TG2Color
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawLine2Col(
      const v1, v2: TG2Vec2;
      const c1, c2: TG2Color
    ): TG2Result;
    function DrawLineStrip(
      const VArr: PG2Vec2Array;
      const VCount: Word;
      const Color: TG2Color
    ): TG2Result;
    function DrawTriangle(
      const v1, v2, v3: TG2Vec2;
      const Color: TG2Color
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawTriangle3Col(
      const v1, v2, v3: TG2Vec2;
      const c1, c2, c3: TG2Color
    ): TG2Result;
    function DrawRect(
      const X, Y, Width, Height: Single;
      const Color: TG2Color
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect(
      const R: TRect;
      const Color: TG2Color
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect4Col(
      const X, Y, Width, Height: Single;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRect4Col(
      const R: TRect;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawQuad(
      const v1, v2, v3, v4: TG2Vec2;
      const Color: TG2Color
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawQuad4Col(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
    function DrawRectHollow(
      const X, Y, Width, Height: Single;
      const Color: TG2Color
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRectHollow(
      const R: TRect;
      const Color: TG2Color
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRectHollow4Col(
      const X, Y, Width, Height: Single;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawRectHollow4Col(
      const R: TRect;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawQuadHollow(
      const v1, v2, v3, v4: TG2Vec2;
      const Color: TG2Color
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawQuadHollow4Col(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
    function DrawCircle(
      const Center: TG2Vec2;
      const Radius: Single;
      const Color: TG2Color;
      const Segments: Word = 32
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawCircle2Col(
      const Center: TG2Vec2;
      const Radius: Single;
      const ColInt, ColExt: TG2Color;
      const Segments: Word = 32
    ): TG2Result;
    function DrawCircleHollow(
      const Center: TG2Vec2;
      const RadiusInt, RadiusExt: Single;
      const Color: TG2Color;
      const Segments: Word = 32
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawCircleHollow2Col(
      const Center: TG2Vec2;
      const RadiusInt, RadiusExt: Single;
      const ColInt, ColExt: TG2Color;
      const Segments: Word = 32
    ): TG2Result;
    function DrawArrow(
      const v1, v2: TG2Vec2;
      const Color: TG2Color
    ): TG2Result;
    function DrawPolyConvex(
      const VArr: PG2Vec2Array;
      const VCount: Word;
      const Color: TG2Color
    ): TG2Result;
  end;
//TG2Primitives2D END

//TG2Primitives3D BEGIN
  TG2Primitives3D = class (TG2Module)
  strict private
    type TG2Vertex = packed record
    public
      var x, y, z: Single;
      var nx, ny, nz: Single;
      var Color: TG2Color
    end;
    type TG2VertexArray = array[Word] of TG2Vertex;
    type PG2VertexArray = ^TG2VertexArray;
    type TG2Index = type Word;
    type TG2IndexArray = array[Word] of TG2Index;
    type PG2IndexArray = ^TG2IndexArray;
    const FVF = D3DFVF_XYZ or D3DFVF_DIFFUSE or D3DFVF_NORMAL;
    var m_PlugGraphics: TG2PlugGraphics;
    var m_VB: IDirect3DVertexBuffer9;
    var m_IB: IDirect3DIndexBuffer9;
    var m_VBSize: Integer;
    var m_IBSize: Integer;
    var m_CurV: Word;
    var m_CurI: DWord;
    var m_Vertices: PG2VertexArray;
    var m_Indices: PG2IndexArray;
    function VerifyVB(const Size: Integer): TG2Result;
    function VerifyIB(const Size: Integer): TG2Result;
    function AddVertex(
      const X, Y, Z: Single;
      const Color: TG2Color;
      const NX, NY, NZ: Single
    ): Word;
    procedure AddFace(
      const v1, v2, v3: Word
    );
    procedure OnDeviceLost;
    procedure OnDeviceReset;
  public
    constructor Create; override;
    destructor Destroy; override;
    function DrawLine(
      const v1, v2: TG2Vec3;
      const Color: TG2Color
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawLine2Col(
      const v1, v2: TG2Vec3;
      const c1, c2: TG2Color
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawTriangle(
      const v1, v2, v3: TG2Vec3;
      const Color: TG2Color
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawTriangle3Col(
      const v1, v2, v3: TG2Vec3;
      const c1, c2, c3: TG2Color
    ): TG2Result;
    function DrawQuad(
      const v1, v2, v3, v4: TG2Vec3;
      const Color: TG2Color
    ): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function DrawQuad4Col(
      const v1, v2, v3, v4: TG2Vec3;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
    function DrawBox(
      const X, Y, Z: Single;
      const SizeX, SizeY, SizeZ: Single;
      const Color: TG2Color
    ): TG2Result;
    function DrawSphere(
      const X, Y, Z: Single;
      const Radius: Single;
      const Color: TG2Color;
      const VSegments: Integer = 32;
      const HSegments: Integer = 32
    ): TG2Result;
    function DrawCylinder(
      const X, Y, Z: Single;
      const Height, Radius: Single;
      const Color: TG2Color;
      const Segments: Integer = 32
    ): TG2Result;
    function DrawCone(
      const X, Y, Z: Single;
      const Height, RadiusTop, RadiusBottom: Single;
      const Color: TG2Color;
      const Segments: Integer = 32
    ): TG2Result;
    function DrawCapsule(
      const X, Y, Z: Single;
      const Height, Radius: Single;
      const Color: TG2Color;
      const VCapSegments: Integer = 8;
      const HSegments: Integer = 32
    ): TG2Result;
    function DrawTorus(
      const X, Y, Z: Single;
      const RadiusInt, RadiusExt: Single;
      const Color: TG2Color;
      const Sides: Integer = 16;
      const Segments: Integer = 32
    ): TG2Result;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Primitives3D END

//TG2ShaderMgr BEGIN
  TG2ShaderMgr = class (TG2ResMgr)
  strict private
    var m_PlugGraphics: TG2PlugGraphics;
    procedure OnLostDevice;
    procedure OnResetDevice;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function CreateVertexShaderFromFile(const Name: WideString; const f: WideString): TG2VertexShader;
    function CreateVertexShaderFromFileCompiled(const Name: WideString; const f: WideString): TG2VertexShader;
    function CreateVertexShaderFromMemory(const Name: WideString; const Ptr: Pointer; const Size: DWord): TG2VertexShader;
    function CreateVertexShaderFromFunction(const Name: WideString; const Func: PDWord): TG2VertexShader;
    function CreatePixelShaderFromFile(const Name: WideString; const f: WideString): TG2PixelShader;
    function CreatePixelShaderFromFileCompiled(const Name: WideString; const f: WideString): TG2PixelShader;
    function CreatePixelShaderFromMemory(const Name: WideString; const Ptr: Pointer; const Size: DWord): TG2PixelShader;
    function CreatePixelShaderFromFunction(const Name: WideString; const Func: PDWord): TG2PixelShader;
    function FindShader(const Name: WideString): TG2Shader;
  end;
//TG2ShaderMgr END

//TG2Shader BEGIN
  TG2Shader = class (TG2Res)
  strict protected
    var m_CompiledShader: PG2CompiledShader;
    var m_ShaderFunction: PDWord;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function LoadFromFile(const f: WideString): TG2Result; virtual;
    function LoadFromFileCompiled(const f: WideString): TG2Result; virtual;
    function LoadFromMemory(const Ptr: Pointer; const Size: DWord): TG2Result; virtual;
    function LoadFromFunction(const Func: PDWord): TG2Result; virtual;
    procedure Release; virtual;
    procedure OnLostDevice; virtual;
    procedure OnResetDevice; virtual;
  end;
//TG2Shader END

//TG2VertexShader BEGIN
  TG2VertexShader = class (TG2Shader)
  strict private
    var m_Shader: IDirect3DVertexShader9;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Shader: IDirect3DVertexShader9 read m_Shader;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function LoadFromFile(const f: WideString): TG2Result; override;
    function LoadFromFileCompiled(const f: WideString): TG2Result; override;
    function LoadFromMemory(const Ptr: Pointer; const Size: DWord): TG2Result; override;
    function LoadFromFunction(const Func: PDWord): TG2Result; override;
    procedure SetConstantF(const r: DWord; const c: PSingle; const F4Count: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetConstantI(const r: DWord; const c: PInteger; const I4Count: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetConstantB(const r: DWord; const c: PBool; const BCount: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetToDevice; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Release; override;
    procedure OnLostDevice; override;
    procedure OnResetDevice; override;
  end;
//TG2VertexShader END

//TG2PixelShader BEGIN
  TG2PixelShader = class (TG2Shader)
  strict private
    var m_Shader: IDirect3DPixelShader9;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Shader: IDirect3DPixelShader9 read m_Shader;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function LoadFromFile(const f: WideString): TG2Result; override;
    function LoadFromFileCompiled(const f: WideString): TG2Result; override;
    function LoadFromMemory(const Ptr: Pointer; const Size: DWord): TG2Result; override;
    function LoadFromFunction(const Func: PDWord): TG2Result; override;
    procedure SetConstantF(const c: DWord; const Value: PSingle; const F4Count: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetConstantI(const c: DWord; const Value: PInteger; const I4Count: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetConstantB(const c: DWord; const Value: PBool; const BCount: DWord); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetToDevice; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure Release; override;
    procedure OnLostDevice; override;
    procedure OnResetDevice; override;
  end;
//TG2PixelShader END

//TG2EffectMgr BEGIN
  TG2EffectMgr = class (TG2ResMgr)
  strict private
    var m_PlugGraphics: TG2PlugGraphics;
    procedure OnLostDevice;
    procedure OnResetDevice;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function CreateEffectFromFile(const Name: WideString; const f: WideString): TG2Effect;
    function CreateEffectFromMemory(const Name: WideString; const Ptr: Pointer; const Size: Integer): TG2Effect;
    function CreateEffectFromPack(const Name: WideString; const FolderName, FileName: AnsiString): TG2Effect;
    function FindEffect(const Name: WideString): TG2Effect;
  end;
//TG2EffectMgr END

//TG2Effect BEGIN
  TG2Effect = class (TG2Res)
  strict private
    var m_Effect: ID3DXEffect;
    function GetCurTechnique: TD3DXHandle;  {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetCurTechnique(const Value: TD3DXHandle);  {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    property Effect: ID3DXEffect read m_Effect;
    property Technique: TD3DXHandle read GetCurTechnique write SetCurTechnique;
    function BeginEffect(Passes: PLongWord; const Flags: DWORD = 0): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function EndEffect: TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function BeginPass(const Pass: LongWord): TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function EndPass: TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function CommitChanges: TG2Result; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetValue(hParameter: TD3DXHandle; pData: Pointer; Bytes: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetValue(hParameter: TD3DXHandle; pData: Pointer; Bytes: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetBool(hParameter: TD3DXHandle; b: BOOL): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBool(hParameter: TD3DXHandle; out pb: BOOL): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetBoolArray(hParameter: TD3DXHandle; pb: PBOOL; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetBoolArray(hParameter: TD3DXHandle; pb: PBOOL; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetInt(hParameter: TD3DXHandle; n: Integer): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetInt(hParameter: TD3DXHandle; out pn: Integer): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetIntArray(hParameter: TD3DXHandle; pn: PInteger; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetIntArray(hParameter: TD3DXHandle; pn: PInteger; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetFloat(hParameter: TD3DXHandle; f: Single): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFloat(hParameter: TD3DXHandle; out pf: Single): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetFloatArray(hParameter: TD3DXHandle; pf: PSingle; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFloatArray(hParameter: TD3DXHandle; pf: PSingle; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetVector(hParameter: TD3DXHandle; const pVector: TD3DXVector4): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVector(hParameter: TD3DXHandle; out pVector: TD3DXVector4): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetVec2(hParameter: TD3DXHandle; const Vec2: TG2Vec2): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetVec3(hParameter: TD3DXHandle; const Vec3: TG2Vec3): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetVectorArray(hParameter: TD3DXHandle; pVector: PD3DXVector4; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVectorArray(hParameter: TD3DXHandle; pVector: PD3DXVector4; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetMatrix(hParameter: TD3DXHandle; const pMatrix: TD3DXMatrix): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMatrix(hParameter: TD3DXHandle; out pMatrix: TD3DXMatrix): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetMatrixArray(hParameter: TD3DXHandle; pMatrix: PD3DXMatrix; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMatrixArray(hParameter: TD3DXHandle; pMatrix: PD3DXMatrix; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetMatrixPointerArray(hParameter: TD3DXHandle; ppMatrix: PPD3DXMatrix; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMatrixPointerArray(hParameter: TD3DXHandle; ppMatrix: PPD3DXMatrix; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetMatrixTranspose(hParameter: TD3DXHandle; const pMatrix: TD3DXMatrix): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMatrixTranspose(hParameter: TD3DXHandle; out pMatrix: TD3DXMatrix): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetMatrixTransposeArray(hParameter: TD3DXHandle; pMatrix: PD3DXMatrix; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMatrixTransposeArray(hParameter: TD3DXHandle; pMatrix: PD3DXMatrix; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetMatrixTransposePointerArray(hParameter: TD3DXHandle; ppMatrix: PPD3DXMatrix; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetMatrixTransposePointerArray(hParameter: TD3DXHandle; ppMatrix: PPD3DXMatrix; Count: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetString(hParameter: TD3DXHandle; pString: PAnsiChar): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetString(hParameter: TD3DXHandle; out ppString: PAnsiChar): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetTexture(hParameter: TD3DXHandle; pTexture: IDirect3DBaseTexture9): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTexture(hParameter: TD3DXHandle; out ppTexture: IDirect3DBaseTexture9): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPixelShader(hParameter: TD3DXHandle; out ppPShader: IDirect3DPixelShader9): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetVertexShader(hParameter: TD3DXHandle; out ppVShader: IDirect3DVertexShader9): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function SetArrayRange(hParameter: TD3DXHandle; uStart, uEnd: LongWord): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetDesc(out pDesc: TD3DXEffectDesc): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetParameterDesc(hParameter: TD3DXHandle; out pDesc: TD3DXParameterDesc): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTechniqueDesc(hTechnique: TD3DXHandle; out pDesc: TD3DXTechniqueDesc): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPassDesc(hPass: TD3DXHandle; out pDesc: TD3DXPassDesc): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFunctionDesc(hShader: TD3DXHandle; out pDesc: TD3DXFunctionDesc): HResult; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetParameter(hParameter: TD3DXHandle; Index: LongWord): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetParameterByName(hParameter: TD3DXHandle; pName: PAnsiChar): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetParameterBySemantic(hParameter: TD3DXHandle; pSemantic: PAnsiChar): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetParameterElement(hParameter: TD3DXHandle; Index: LongWord): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTechnique(Index: LongWord): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetTechniqueByName(pName: PAnsiChar): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPass(hTechnique: TD3DXHandle; Index: LongWord): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetPassByName(hTechnique: TD3DXHandle; pName: PAnsiChar): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFunction(Index: LongWord): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetFunctionByName(pName: PAnsiChar): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAnnotation(hObject: TD3DXHandle; Index: LongWord): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetAnnotationByName(hObject: TD3DXHandle; pName: PAnsiChar): TD3DXHandle; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    function LoadFromFile(const f: WideString): TG2Result;
    function LoadFromMemory(const Ptr: Pointer; const Size: Integer): TG2Result;
    function LoadFromPack(const FolderName, FileName: AnsiString): TG2Result;
    procedure Release;  {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure OnLostDevice; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure OnResetDevice; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;
//TG2Effect END

//TG2ShaderLib BEGIN
  TG2ShaderLib = class (TG2ResMgr)
  strict private
    var m_VertexShaders: TG2ShaderMgr;
    var m_PixelShaders: TG2ShaderMgr;
    var m_Effects: TG2EffectMgr;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
    procedure CreateShaderBin(const Name: WideString; const Ptr: Pointer; const Size: DWord); overload;
    procedure CreateShaderBin(const Name: WideString; const Func: PDWord); overload;
    function RequestVertexShader(const Name: WideString): TG2VertexShader;
    function RequestPixelShader(const Name: WideString): TG2PixelShader;
    function RequestEffect(const Name: WideString): TG2Effect;
  end;
//TG2ShaderLib END

//TG2ShaderBin BEGIN
  TG2ShaderBin = class (TG2Res)
  strict private
    var m_Buffer: array of Byte;
    var m_Func: PDWord;
    function GetBuffer: Pointer; inline;
    function GetSize: Integer; inline;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Buffer: Pointer read GetBuffer;
    property Size: Integer read GetSize;
    property Func: PDWord read m_Func;
    procedure LoadBuffer(const Ptr: Pointer; const Size: DWord);
    procedure LoadFunction(const NewFunc: PDWord);
  end;
//TG2ShaderBin END

//TG2Scene2D BEGIN
  PG2Scene2DNode = ^TG2Scene2DNode;
  TG2Scene2DNode = record
  public
    var DivType: TG2Scene2DDivType;
    var Parent: PG2Scene2DNode;
    var Children: array of PG2Scene2DNode;
    var MinV, MaxV: TG2Vec2;
    var LOD: Word;
    var Items: TList;
    var Occlusion: TG2Scene2DOcclusionCheck;
  end;

  PG2Scene2DItem = ^TG2Scene2DItem;
  TG2Scene2DItem = record
  public
    var Data: Pointer;
    var MinV, MaxV: TG2Vec2;
    var FetchID: DWord;
    var Nodes: array of PG2Scene2DNode;
    var NodeCount: Word;
  end;

  TG2Scene2D = class (TG2Module)
  strict private
    var m_RootNode: PG2Scene2DNode;
    var m_LOD: Word;
    var m_MinV, m_MaxV: TG2Vec2;
    var m_Built: Boolean;
    var m_UpdateFlag: DWord;
    var m_CurFetchID: DWord;
    var m_FetchNodes: TList;
    var m_FetchItems: TList;
    function NodeInScope(const n: PG2Scene2DNode; const MinV, MaxV: TG2Vec2): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure FetchNodes(const MinV, MaxV: TG2Vec2);
    procedure ResetFetchID;
  public
    constructor Create; override;
    destructor Destroy; override;
    property InitLOD: Word read m_LOD write m_LOD;
    property InitMinV: TG2Vec2 read m_MinV write m_MinV;
    property InitMaxV: TG2Vec2 read m_MaxV write m_MaxV;
    property FetchItems: TList read m_FetchItems;
    procedure QueryItems(const MinV, MaxV: TG2Vec2);
    procedure SceneBuild;
    procedure SceneDestroy;
    function ItemCreate(const MinV, MaxV: TG2Vec2): PG2Scene2DItem;
    procedure ItemDestroy(const Item: PG2Scene2DItem);
    procedure ItemAdd(const Item: PG2Scene2DItem);
    procedure ItemRemove(const Item: PG2Scene2DItem);
    procedure ItemMove(const Item: PG2Scene2DItem; const MinV, MaxV: TG2Vec2);
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Scene2D END

//TG2Scene3D BEGIN
  PG2Scene3DNode = ^TG2Scene3DNode;
  TG2Scene3DNode = record
  public
    var DivX, DivY, DivZ, DivN: Boolean;
    var Parent: PG2Scene3DNode;
    var Children: array of array of array of PG2Scene3DNode;
    var NodeMinX, NodeMaxX: Byte;
    var NodeMinY, NodeMaxY: Byte;
    var NodeMinZ, NodeMaxZ: Byte;
    var MinV, MaxV: TG2Vec3;
    var LOD: Word;
    var FrustumCheck: TG2FrustumCheck;
    var Items: TList;
  end;

  PG2Scene3DItem = ^TG2Scene3DItem;
  TG2Scene3DItem = record
  public
    var Data: Pointer;
    var MinV, MaxV: TG2Vec3;
    var C: TG2Vec3;
    var R: Single;
    var LocType: Integer;
    var FetchID: DWord;
    var Nodes: array of PG2Scene3DNode;
    var NodeCount: Word;
  end;

  TG2Scene3D = class (TG2Module)
  strict private
    var m_RootNode: PG2Scene3DNode;
    var m_LOD: Word;
    var m_MaxLODX: Integer;
    var m_MaxLODY: Integer;
    var m_MaxLODZ: Integer;
    var m_MinV, m_MaxV: TG2Vec3;
    var m_Built: Boolean;
    var m_FetchNodes: TList;
    var m_FetchItems: TList;
    var m_RefList: TList;
    var m_CurFetchID: DWord;
    var m_ProcAddItem: array[0..1] of procedure (const Item: PG2Scene3DItem) of object;
    var m_FuncFrustumItem: array[0..1] of function (const Item: PG2Scene3DItem; const Frustum: TG2Frustum): Boolean of object;
    procedure ItemAddBox(const Item: PG2Scene3DItem);
    procedure ItemAddSphere(const Item: PG2Scene3DItem);
    function ItemFrustumBox(const Item: PG2Scene3DItem; const Frustum: TG2Frustum): Boolean;
    function ItemFrustumSphere(const Item: PG2Scene3DItem; const Frustum: TG2Frustum): Boolean;
    function NodeInScope(const n: PG2Scene3DNode; const MinV, MaxV: TG2Vec3): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetFetchList(const Value: TList);
  public
    constructor Create; override;
    destructor Destroy; override;
    property InitLOD: Word read m_LOD write m_LOD;
    property InitMaxLODX: Integer read m_MaxLODX write m_MaxLODX;
    property InitMaxLODY: Integer read m_MaxLODY write m_MaxLODY;
    property InitMaxLODZ: Integer read m_MaxLODZ write m_MaxLODZ;
    property InitMinV: TG2Vec3 read m_MinV write m_MinV;
    property InitMaxV: TG2Vec3 read m_MaxV write m_MaxV;
    property FetchItems: TList read m_RefList write SetFetchList;
    property FetchNodes: TList read m_FetchNodes;
    property CurFetchID: DWord read m_CurFetchID write m_CurFetchID;
    property RootNode: PG2Scene3DNode read m_RootNode;
    procedure ResetFetchID;
    procedure QueryItems(const MinV, MaxV: TG2Vec3); overload;
    procedure QueryItems(const Frustum: TG2Frustum); overload;
    procedure QueryNodes(const MinV, MaxV: TG2Vec3); overload;
    procedure QueryNodes(const Frustum: TG2Frustum); overload;
    procedure SceneBuild;
    procedure SceneDestroy;
    function ItemCreate(const LocType: Integer = 0): PG2Scene3DItem;
    procedure ItemDestroy(const Item: PG2Scene3DItem);
    procedure ItemAdd(const Item: PG2Scene3DItem);
    procedure ItemRemove(const Item: PG2Scene3DItem);
    procedure ItemMove(const Item: PG2Scene3DItem; const MinV, MaxV: TG2Vec3); overload;
    procedure ItemMove(const Item: PG2Scene3DItem; const C: TG2Vec3; const R: Single); overload;
    function Initialize(const G2Core: TG2Core): TG2Result; override;
    function Finalize: TG2Result; override;
  end;
//TG2Scene3D END

//TG2SkyBox BEGIN
  TG2SkyBox = class
  strict private
    var m_Core: TG2Core;
    var m_Texture: TG2TextureCubeBase;
    var m_VB: TG2VB;
    var m_IB: TG2IB;
    var m_Scale: Single;
  public
    constructor Create(const Core: TG2Core);
    destructor Destroy; override;
    property Scale: Single read m_Scale write m_Scale;
    property Texture: TG2TextureCubeBase read m_Texture write m_Texture;
    procedure Render;
  end;
//TG2SkyBox END

//TG2PostProcess BEGIN
  TG2PostProcess = class
  strict private
    type TVertex1 = record
    public
      var Pos: TG2Vec4;
      var Tex0: TG2Vec2;
    end;
    type TVertex2 = record
    public
      var Pos: TG2Vec4;
      var Tex0: TG2Vec2;
      var Tex1: TG2Vec2;
    end;
    type TVertex3 = record
    public
      var Pos: TG2Vec4;
      var Tex0: TG2Vec2;
      var Tex1: TG2Vec2;
      var Tex2: TG2Vec2;
    end;
    type TVertex4 = record
    public
      var Pos: TG2Vec4;
      var Tex0: TG2Vec2;
      var Tex1: TG2Vec2;
      var Tex2: TG2Vec2;
      var Tex3: TG2Vec2;
    end;
    type TVertex1Arr = array[Word] of TVertex1;
    type PVertex1Arr = ^TVertex1Arr;
    type TVertex2Arr = array[Word] of TVertex2;
    type PVertex2Arr = ^TVertex2Arr;
    type TVertex3Arr = array[Word] of TVertex3;
    type PVertex3Arr = ^TVertex3Arr;
    type TVertex4Arr = array[Word] of TVertex4;
    type PVertex4Arr = ^TVertex4Arr;
    type TTempSurface = record
    public
      var Surface: TG2Texture2DRT;
      var Width: Integer;
      var Height: Integer;
      var Format: TD3DFormat;
    end;
    type PTempSurface = ^TTempSurface;
    var m_Core: TG2Core;
    var m_Shaders: TG2Effect;
    var m_Surfaces: TG2QuickList;
    var m_VB: array [0..3] of TG2VB;
    procedure LoadBuffer0(const Pos, Tex0: TG2Rect);
    procedure LoadBuffer1(const Pos, Tex0, Tex1: TG2Rect);
    procedure LoadBuffer2(const Pos, Tex0, Tex1, Tex2: TG2Rect);
    procedure LoadBuffer3(const Pos, Tex0, Tex1, Tex2, Tex3: TG2Rect);
    function RequestSurface(const Width, Height: Integer; const Format: TD3DFormat = D3DFMT_UNKNOWN): TG2Texture2DRT;
  public
    constructor Create(const Core: TG2Core);
    destructor Destroy; override;
    procedure EffectBlur(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Integer = 1
    );
    procedure EffectSharpen(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 10
    );
    procedure EffectMonotone(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 1;
      const Mask: DWord = $ffffffff
    );
    procedure EffectContrast(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 1
    );
    procedure EffectEmboss(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 10
    );
    procedure EffectEdges(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 10;
      const Power: Single = 1
    );
    procedure EffectColorClamp(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const ClampMin: Single = 0;
      const ClampMax: Single = 1
    );
    procedure EffectMonotoneClamp(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const ClampMin: Single = 0;
      const ClampMax: Single = 1
    );
    procedure EffectDistortMap(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const DistortMap: TG2Texture2DBase;
      const DistortShift: TG2Vec2;
      const Amount: Single = 10
    );
    procedure EffectDistortMap2(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const DistortMap0: TG2Texture2DBase;
      const DistortMap1: TG2Texture2DBase;
      const DistortShift0: TG2Vec2;
      const DistortShift1: TG2Vec2;
      const Amount: Single = 10
    );
    procedure EffectBloom(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Power: Single = 1
    );
  end;
//TG2PostProcess END

//TG2Thread BEGIN
  TG2Thread = class (TThread)
  strict private
    var m_Proc: TG2ProcObj;
    var m_OnFinishProc: TG2ProcObj;
    var m_CS: TCriticalSection;
  strict protected
    procedure Execute; override;
    procedure OnFinish(Sender: TObject);
  public
    property Proc: TG2ProcObj read m_Proc write m_Proc;
    property ProcFinish: TG2ProcObj read m_OnFinishProc write m_OnFinishProc;
    property CS: TCriticalSection read m_CS;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;
//TG2Thread END

//TG2App BEGIN
  TG2App = class (TObject)
  strict private
    var m_g2: TG2Core;
    var m_Gfx: TG2Graphics;
    var m_Sfx: TG2Audio;
    var m_Inp: TG2Input;
    var m_Net: TG2Network;
    var m_Tmr: TG2Timer;
    var m_PlugTimer: TG2PlugTimer;
    var m_PlugGraphics: TG2PlugGraphics;
    var m_PlugInput: TG2PlugInput;
    var m_Render: TG2Render;
    var m_Render2D: TG2Render2D;
    var m_Render3D: TG2Render3D;
    var m_Prim2D: TG2Primitives2D;
    var m_Prim3D: TG2Primitives3D;
    var m_RenderModes: TG2RenderModes;
    var m_Cam: TG2Camera;
    var m_AppTime: DWord;
    var m_Pause: Boolean;
    var m_Running: Boolean;
    var m_WndClassName: AnsiString;
    var m_WindowArr: array of HWnd;
    procedure SetHandle(const Value: HWnd);
    function GetHandle: HWnd;
    procedure AppTimer;
    procedure AppRender;
    procedure AppUpdate;
    procedure AppDeviceLost;
    procedure AppDeviceReset;
    procedure AppParamsChange;
    procedure AppKeyDown(const Key: Byte);
    procedure AppKeyUp(const Key: Byte);
    procedure AppKeyPress(const Key: AnsiChar);
    procedure AppMouseDown(const Button: Byte);
    procedure AppMouseUp(const Button: Byte);
    procedure AppMouseMove(const Shift: TPoint);
    procedure AppWheelMove(const Shift: Integer);
  strict protected
    procedure OnTimer; virtual;
    procedure OnRender; virtual;
    procedure OnUpdate; virtual;
    procedure OnDeviceLost; virtual;
    procedure OnDeviceReset; virtual;
    procedure OnParamsChange; virtual;
    procedure OnKeyDown(const Key: Byte); virtual;
    procedure OnKeyUp(const Key: Byte); virtual;
    procedure OnKeyPress(const Key: AnsiChar); virtual;
    procedure OnMouseDown(const Button: Byte); virtual;
    procedure OnMouseUp(const Button: Byte); virtual;
    procedure OnMouseMove(const Shift: TPoint); virtual;
    procedure OnWheelMove(const Shift: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Handle: HWnd read GetHandle write SetHandle;
    property g2: TG2Core read m_g2;
    property Gfx: TG2Graphics read m_Gfx;
    property Sfx: TG2Audio read m_Sfx;
    property Inp: TG2Input read m_Inp;
    property Net: TG2Network read m_Net;
    property Tmr: TG2Timer read m_Tmr;
    property PlugTimer: TG2PlugTimer read m_PlugTimer;
    property PlugGraphics: TG2PlugGraphics read m_PlugGraphics;
    property PlugInput: TG2PlugInput read m_PlugInput;
    property Render: TG2Render read m_Render;
    property Render2D: TG2Render2D read m_Render2D;
    property Render3D: TG2Render3D read m_Render3D;
    property Prim2D: TG2Primitives2D read m_Prim2D;
    property Prim3D: TG2Primitives3D read m_Prim3D;
    property RenderModes: TG2RenderModes read m_RenderModes;
    property Cam: TG2Camera read m_Cam;
    property AppTime: DWord read m_AppTime;
    property Pause: Boolean read m_Pause write m_Pause;
    property Running: Boolean read m_Running write m_Running;
    function WindowCreate(
      const Width: Integer = 0;
      const Height: Integer = 0;
      const Caption: AnsiString = ''
    ): HWnd;
    procedure WindowDestroy(const WindowHandle: HWnd);
    procedure Loop; virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  end;
//TG2App END

//TG2SysInfo BEGIN
  TG2SysCPU = record
    var Name: AnsiString;
    var Identifier: AnsiString;
    var Vendor: AnsiString;
    var MHz: Integer;
  end;

  TG2SysInfo = record
    var CPUCount: Integer;
    var CPUs: array of TG2SysCPU;
    var DisplayDevice: AnsiString;
    var Memory: TMemoryStatus;
    var VideoMemoryTotal: DWord;
    var VideoMemoryFree: DWord;
    var DXVersion: String;
    var OSVersion: String;
  end;
//TG2SysInfo END

const G2Version: Word = $0101;

const D3DFMT_TRMS = TD3DFormat($434F5441);
const D3DFMT_TRSS = TD3DFormat($41415353);

const TG2ResSuccess: set of TG2Result = [grOk, grConditionalOk];

const TG2SharedVertex2DFVF = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1;

const TG2CubeFaces: array[0..5] of TD3DCubemapFaces = (
    D3DCUBEMAP_FACE_POSITIVE_X,
    D3DCUBEMAP_FACE_NEGATIVE_X,
    D3DCUBEMAP_FACE_POSITIVE_Y,
    D3DCUBEMAP_FACE_NEGATIVE_Y,
    D3DCUBEMAP_FACE_POSITIVE_Z,
    D3DCUBEMAP_FACE_NEGATIVE_Z
  );

var AppTime: DWord;
var AppPath: WideString;
var AppCompiler: AnsiString;
var G2VMRID: DWord = 0;

//Utility Functions BEGIN
function G2ResOk(const Res: TG2Result): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2ResFail(const Res: TG2Result): Boolean; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure SafeRelease(var i); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function WideStringToString(const WS: WideString; CodePage: Word = CP_ACP): AnsiString;
function StringToWideString(const S: AnsiString; CodePage: Word = CP_ACP): WideString;
function G2Param(const Str: AnsiString; const Separator: AnsiString; const Param: Integer): AnsiString;
function G2ParamCount(const Str: AnsiString; const Separator: AnsiString): Integer;
function G2StrInStr(const Str: AnsiString; SubStr: AnsiString): Integer;
function G2Color(const R, G, B: Byte; const A: Byte = 255): TG2Color; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Color(const Color: DWord): TG2Color; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Color(const v: TG2Vec3): TG2Color; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Color(const v: TG2Vec4): TG2Color; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Rect(const Left, Top, Right, Bottom: Single): TG2Rect; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2FormatSize(const Format: TD3DFormat): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2FormatToString(const Format: TD3DFormat): AnsiString;
function G2SysInfo: TG2SysInfo;
function G2PiTime(Amp: Single = 1000): Single; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2PiTime(Amp: Single; Time: DWord): Single; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2TimeInterval(Interval: DWord = 1000): Single; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2TimeInterval(Interval: DWord; Time: DWord): Single; overload; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2RandomPi: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2Random2Pi: Single; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2RandomCirclePoint: TG2Vec2; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2RandomSpherePoint: TG2Vec3; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2RandomColor(const MinBrightness: Byte = 0): DWord; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2EncDec(const Ptr: PByteArray; const Count: Integer; const Key: AnsiString);
function G2RectVsRect(const R1, R2: TRect): Boolean;
function G2RectBuild(const Pt1, Pt2: TPoint): TRect;
function DIKToChar(const Key: Byte): AnsiChar; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function D3DTS_TEXTUREMATRIX(const Index: Byte): TD3DTransformStateType; {$IFDEF G2_USE_INLINE} inline;  {$ENDIF}
function D3DVertexElement(
  Stream:     Word;
  Offset:     Word;
  _Type:      TD3DDeclType;
  Method:     TD3DDeclMethod;
  Usage:      TD3DDeclUsage;
  UsageIndex: Byte
): TD3DVertexElement9; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
function G2DefWndProc(Wnd: HWnd; Msg: UInt; wParam: WPARAM; lParam: LPARAM): LResult; stdcall;
function G2InputWndProc(Wnd: HWnd; Msg: UInt; wParam: WPARAM; lParam: LPARAM): LResult; stdcall;
function G2WSAErrorToStr(const Error: Integer): AnsiString;
procedure G2BreakPoint; overload;
//Utility Functions END

//Unit Functions BEGIN
procedure G2WriteLog(const Log: AnsiString); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
procedure G2WriteLogTimed(const Log: AnsiString; const Module: AnsiString = ''); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
//Unit Functions END

implementation

//TG2Color BEGIN
class operator TG2Color.Equal(const c1, c2: TG2Color): Boolean;
begin
  Result := PDWord(@c1)^ = PDWord(@c2)^;
end;

class operator TG2Color.NotEqual(const c1, c2: TG2Color): Boolean;
begin
  Result := PDWord(@c1)^ <> PDWord(@c2)^;
end;

class operator TG2Color.Explicit(const c: TG2Color): DWord;
begin
  Result := PDWord(@c)^;
end;

class operator TG2Color.Explicit(const c: DWord): TG2Color;
begin
  Result := PG2Color(@c)^;
end;

class operator TG2Color.Explicit(const c: TG2Color): TD3DColorValue;
const
  Rcp255 = 1 / 255;
begin
  Result.r := c.r * Rcp255;
  Result.g := c.g * Rcp255;
  Result.b := c.b * Rcp255;
  Result.a := c.a * Rcp255;
end;

class operator TG2Color.Explicit(const c: TD3DColorValue): TG2Color;
begin
  Result.r := Trunc(c.r * 255);
  Result.g := Trunc(c.g * 255);
  Result.b := Trunc(c.b * 255);
  Result.a := Trunc(c.a * 255);
end;

class operator TG2Color.Explicit(const c: TG2Color): TG2Vec4;
begin
  Result.x := c.r * Rcp255;
  Result.y := c.g * Rcp255;
  Result.z := c.b * Rcp255;
  Result.w := c.a * Rcp255;
end;

class operator TG2Color.Explicit(const v: TG2Vec4): TG2Color;
begin
  Result.r := Trunc(v.x * 255);
  Result.g := Trunc(v.y * 255);
  Result.b := Trunc(v.z * 255);
  Result.a := Trunc(v.w * 255);
end;

class operator TG2Color.Explicit(const c: TG2Color): TG2Vec3;
begin
  Result.x := c.r * Rcp255;
  Result.y := c.g * Rcp255;
  Result.z := c.b * Rcp255;
end;

class operator TG2Color.Explicit(const v: TG2Vec3): TG2Color;
begin
  Result.r := Trunc(v.x * 255);
  Result.g := Trunc(v.y * 255);
  Result.b := Trunc(v.z * 255);
  Result.a := $ff;
end;

class operator TG2Color.Implicit(const c: TG2Color): DWord;
begin
  Result := PDWord(@c)^;
end;

class operator TG2Color.Implicit(const c: DWord): TG2Color;
begin
  Result := PG2Color(@c)^;
end;

class operator TG2Color.Implicit(const c: TG2Color): TD3DColorValue;
const
  Rcp255 = 1 / 255;
begin
  Result.r := c.r * Rcp255;
  Result.g := c.g * Rcp255;
  Result.b := c.b * Rcp255;
  Result.a := c.a * Rcp255;
end;

class operator TG2Color.Implicit(const c: TD3DColorValue): TG2Color;
begin
  Result.r := Trunc(c.r * 255);
  Result.g := Trunc(c.g * 255);
  Result.b := Trunc(c.b * 255);
  Result.a := Trunc(c.a * 255);
end;

class operator TG2Color.Implicit(const c: TG2Color): TG2Vec4;
begin
  Result.x := c.r * Rcp255;
  Result.y := c.g * Rcp255;
  Result.z := c.b * Rcp255;
  Result.w := c.a * Rcp255;
end;

class operator TG2Color.Implicit(const v: TG2Vec4): TG2Color;
begin
  Result.r := Trunc(v.x * 255);
  Result.g := Trunc(v.y * 255);
  Result.b := Trunc(v.z * 255);
  Result.a := Trunc(v.w * 255);
end;

class operator TG2Color.Implicit(const c: TG2Color): TG2Vec3;
begin
  Result.x := c.r * Rcp255;
  Result.y := c.g * Rcp255;
  Result.z := c.b * Rcp255;
end;

class operator TG2Color.Implicit(const v: TG2Vec3): TG2Color;
begin
  Result.r := Trunc(v.x * 255);
  Result.g := Trunc(v.y * 255);
  Result.b := Trunc(v.z * 255);
  Result.a := $ff;
end;
//TG2Color END

//TG2BlendMode BEGIN
function TG2BlendMode.SeparateAlpha: Boolean;
begin
  Result := (ColSrc <> AlphaSrc) or (ColDst <> AlphaDst);
end;

procedure TG2BlendMode.SetNormal;
begin
  ColSrc := D3DBLEND_SRCALPHA;
  ColDst := D3DBLEND_INVSRCALPHA;
  AlphaSrc := D3DBLEND_SRCALPHA;
  AlphaDst := D3DBLEND_INVSRCALPHA;
end;

procedure TG2BlendMode.SetAdd;
begin
  ColSrc := D3DBLEND_ONE;
  ColDst := D3DBLEND_ONE;
  AlphaSrc := D3DBLEND_ONE;
  AlphaDst := D3DBLEND_ONE;
end;

class operator TG2BlendMode.Equal(const bm1, bm2: TG2BlendMode): Boolean;
begin
  Result := PDWord(@bm1)^ = PDWord(@bm2)^;
end;

class operator TG2BlendMode.NotEqual(const bm1, bm2: TG2BlendMode): Boolean;
begin
  Result := PDWord(@bm1)^ <> PDWord(@bm2)^;
end;

class operator TG2BlendMode.Explicit(const bm: TG2BlendMode): DWord;
begin
  Result := PDWord(@bm)^;
end;

class operator TG2BlendMode.Explicit(const bm: DWord): TG2BlendMode;
begin
  Result := PG2BlendMode(@bm)^;
end;

class operator TG2BlendMode.Implicit(const bm: TG2BlendMode): DWord;
begin
  Result := PDWord(@bm)^;
end;

class operator TG2BlendMode.Implicit(const bm: DWord): TG2BlendMode;
begin
  Result := PG2BlendMode(@bm)^;
end;
//TG2BlendMode END

//TG2Rect BEGIN
function TG2Rect.Width: Single;
begin
  Result := Right - Left;
end;

function TG2Rect.Height: Single;
begin
  Result := Bottom - Top;
end;

function TG2Rect.TopLeft: TG2Vec2;
begin
  Result.SetValue(Left, Top);
end;

function TG2Rect.TopRight: TG2Vec2;
begin
  Result.SetValue(Right, Top);
end;

function TG2Rect.BottomLeft: TG2Vec2;
begin
  Result.SetValue(Left, Bottom);
end;

function TG2Rect.BottomRight: TG2Vec2;
begin
  Result.SetValue(Right, Bottom);
end;
//TG2Rect END

//TG2Class BEGIN
constructor TG2Class.Create;
begin
  inherited Create;
end;

destructor TG2Class.Destroy;
begin
  inherited Destroy;
end;
//TG2Class END

//TG2Core BEGIN
constructor TG2Core.Create;
begin
  inherited Create;
  m_Handle := 0;
  m_OwnWindow := False;
  m_Initialized := False;
  m_Timer := TG2Timer.Create;
  m_Graphics := TG2Graphics.Create;
  m_Audio := TG2Audio.Create;
  m_Input := TG2Input.Create;
  m_Network := TG2Network.Create;
  m_PackLinker := TG2PackLinker.Create;
  m_Mods := TG2List.Create;
end;

destructor TG2Core.Destroy;
begin
  Finalize;
  m_Mods.Free;
  m_PackLinker.Free;
  m_Network.Free;
  m_Input.Free;
  m_Audio.Free;
  m_Graphics.Free;
  m_Timer.Free;
  inherited Destroy;
end;

procedure TG2Core.SetHandle(const Value: HWnd);
begin
  if not m_Initialized then
  m_Handle := Value;
end;

function TG2Core.CreateWindow: HWnd;
var
  WndClass: TWndClassExA;
  ClassName: AnsiString;
begin
  FillChar(WndClass, SizeOf(TWndClassEx), 0);
  WndClass.cbSize := SizeOf(TWndClassEx);
  WndClass.hIconSm := LoadIcon(0, IDI_APPLICATION);
  WndClass.hIcon := LoadIcon(0, IDI_APPLICATION);
  WndClass.hInstance := HInstance;
  WndClass.hCursor := LoadCursor(0, IDC_ARROW);
  WndClass.lpszClassName := 'Gen2';
  WndClass.style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC or CS_DBLCLKS;
  WndClass.lpfnWndProc := @G2DefWndProc;

  if RegisterClassExA(WndClass) = 0 then
  ClassName := 'Static'
  else
  ClassName := WndClass.lpszClassName;

  Result := CreateWindowExA(
    0, PAnsiChar(ClassName), nil, WS_POPUP or WS_VISIBLE or WS_EX_TOPMOST, 0, 0,
    GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN),
    0, 0, HInstance, nil
  );
end;

procedure TG2Core.FreeMods;
var
  i: Integer;
  m: TG2Module;
  ModsToFree: TList;
begin
  ModsToFree := TList.Create;
  try
    ModsToFree.Assign(m_Mods);
    for i := 0 to ModsToFree.Count - 1 do
    begin
      try
        m := TG2Module(ModsToFree[i]);
        ReleaseMod(@m);
      except
        G2WriteLogTimed(AnsiString('(E) Module destruction failed: ' + TObject(m_Mods[i]).ClassName), 'Core');
      end;
    end;
  finally
    ModsToFree.Free;
  end;
end;

function TG2Core.Initialize: TG2Result;
begin
  if m_Initialized then
  begin
    Result := grRedundantCall;
    Exit;
  end;
  G2WriteLogTimed('Initialization Started.', 'Core');
  if m_Handle = 0 then
  begin
    m_Handle := CreateWindow;
    m_OwnWindow := True;
  end;
  m_Initialized := True;
  m_Timer.Initialize(Self);
  m_Graphics.Initialize(Self);
  m_Audio.Initialize(Self);
  m_Input.Initialize(Self);
  m_Network.Initialize(Self);
  Result := grOk;
  G2WriteLogTimed('Initialization Finished.', 'Core');
end;

function TG2Core.Finalize: TG2Result;
begin
  if not m_Initialized then
  begin
    Result := grRedundantCall;
    Exit;
  end;
  G2WriteLogTimed('Finalization Started.', 'Core');
  m_Initialized := False;
  m_Timer.Enabled := False;
  m_Network.Finalize;
  m_Input.Finalize;
  m_Audio.Finalize;
  m_Graphics.Finalize;
  m_Timer.Finalize;
  if m_Mods.Count > 0 then
  begin
    G2WriteLogTimed('(W) ' + AnsiString(IntToStr(m_Mods.Count)) + ' Modules are unreleased', 'Core');
    G2WriteLogTimed('Module Cleanup Started.', 'Core');
    FreeMods;
    G2WriteLogTimed('Module Cleanup Finished.', 'Core');
  end;
  if m_OwnWindow then
  begin
    DestroyWindow(m_Handle);
    m_Handle := 0;
    m_OwnWindow := False;
  end;
  Result := grOk;
  G2WriteLogTimed('Finalization Finished.', 'Core');
end;

function TG2Core.RequestMod(
      const ModClass: CG2ModClass;
      const Module: PG2Module
    ): TG2Result;
begin
  if not m_Initialized then
  begin
    Result := grNotInitialized;
    Exit;
  end;
  Module^ := ModClass.Create;
  Result := Module^.Initialize(Self);
  if G2ResOk(Result) then
  m_Mods.Add(Module^);
end;

function TG2Core.ReleaseMod(
      const Module: PG2Module
    ): TG2Result;
begin
  m_Mods.Remove(Module^);
  if Module^.Initialized then
  Result := Module^.Finalize
  else
  Result := grOk;
  Module^.Free;
end;

function TG2Core.RequestPlug(
      const PlugClass: CG2PlugClass;
      const Plug: PG2Plug
    ): TG2Result;
begin
  if not m_Initialized then
  begin
    Result := grNotInitialized;
    Exit;
  end;
  Result := grInvalidCall;
  if m_Timer.PlugClass = PlugClass then
  Result := m_Timer.RequestPlug(PlugClass, Plug)
  else if m_Graphics.PlugClass = PlugClass then
  Result := m_Graphics.RequestPlug(PlugClass, Plug)
  else if m_Audio.PlugClass = PlugClass then
  Result := m_Audio.RequestPlug(PlugClass, Plug)
  else if m_Input.PlugClass = PlugClass then
  Result := m_Input.RequestPlug(PlugClass, Plug)
  else if m_Network.PlugClass = PlugClass then
  Result := m_Network.RequestPlug(PlugClass, Plug);
end;

function TG2Core.ReleasePlug(
      const Plug: PG2Plug
    ): TG2Result;
begin
  Result := grInvalidCall;
  if Plug^ is TG2PlugTimer then
  Result := m_Timer.ReleasePlug(Plug)
  else if Plug^ is TG2PlugGraphics then
  Result := m_Graphics.ReleasePlug(Plug)
  else if Plug^ is TG2PlugAudio then
  Result := m_Audio.ReleasePlug(Plug)
  else if Plug^ is TG2PlugInput then
  Result := m_Input.ReleasePlug(Plug)
  else if Plug^ is TG2PlugNetwork then
  Result := m_Network.ReleasePlug(Plug);
end;
//TG2Core END

//TG2HighClass BEGIN
constructor TG2HighClass.Create;
begin
  inherited Create;
  m_Core := nil;
  m_Initialized := False;
end;

destructor TG2HighClass.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

function TG2HighClass.Initialize(const G2Core: TG2Core): TG2Result;
begin
  if not m_Initialized then
  begin
    m_Core := G2Core;
    m_Initialized := True;
    Result := grOk;
  end
  else
  Result := grRedundantCall;
end;

function TG2HighClass.Finalize: TG2Result;
begin
  if m_Initialized then
  begin
    m_Initialized := False;
    Result := grOk;
  end
  else
  Result := grRedundantCall;
end;
//TG2HighClass END

//TG2PackLinker BEGIN
constructor TG2PackLinker.Create;
begin
  inherited Create;
  m_Packs := TList.Create;
end;

destructor TG2PackLinker.Destroy;
var
  i: Integer;
begin
  for i := 0 to m_Packs.Count - 1 do
  TG2Pack(m_Packs[i]).Free;
  m_Packs.Free;
  inherited Destroy;
end;

function TG2PackLinker.GetPack(const Index: Integer): TG2Pack;
begin
  Result := TG2Pack(m_Packs[Index]);
end;

function TG2PackLinker.GetPackCount: Integer;
begin
  Result := m_Packs.Count;
end;

function TG2PackLinker.LinkPack(const FileName: String; const Key: AnsiString = ''): TG2Pack;
begin
  if not FileExists(FileName) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TG2Pack.Create;
  Result.Key := Key;
  Result.OpenPack(FileName);
  if Result.IsOpen then
  m_Packs.Add(Result)
  else
  begin
    Result.Free;
    Result := nil;
  end;
end;

procedure TG2PackLinker.UnLoadCache;
var
  i: Integer;
begin
  for i := 0 to m_Packs.Count - 1 do
  TG2Pack(m_Packs[i]).UnLoadFiles;
end;

procedure TG2PackLinker.GetFileData(
      const FolderName, FileName: AnsiString;
      var Data: Pointer;
      var DataSize: DWord
    );
var
  i: Integer;
  f: PG2PackFile;
begin
  for i := 0 to m_Packs.Count - 1 do
  begin
    f := TG2Pack(m_Packs[i]).FindFile(FolderName, FileName);
    if Assigned(f) then
    begin
      TG2Pack(m_Packs[i]).GetFileData(f, Data, DataSize);
      Exit;
    end;
  end;
  Data := nil;
  DataSize := 0;
end;
//TG2PackLinker END

//TG2Pack BEGIN
constructor TG2Pack.Create;
begin
  inherited Create;
  m_IsOpen := False;
end;

destructor TG2Pack.Destroy;
begin
  ClosePack;
  inherited Destroy;
end;

procedure TG2Pack.OpenPack(const FileName: String);
type
  TPackHeader = packed record
  public
    Definition: String[4];
    Version: DWord;
    FolderCount: DWord;
    DataPos: DWord;
  end;
  function ReadStringNT: AnsiString;
  var
    Pos: Int64;
    b: Byte;
    Len: Integer;
  begin
    Pos := m_fs.Position;
    Len := 0;
    repeat
      m_fs.Read(b, 1);
      Inc(Len);
    until b = 0;
    Dec(Len);
    m_fs.Position := Pos;
    SetLength(Result, Len);
    m_fs.Read(Result[1], Len);
    m_fs.Position := m_fs.Position + 1;
  end;
  function ReadDWord: DWord;
  begin
    m_fs.Read(Result, 4);
  end;
  function ReadBool: Boolean;
  begin
    m_fs.Read(Result, 1);
  end;
  procedure Skip(const Bytes: Integer);
  begin
    m_fs.Position := m_fs.Position + Bytes;
  end;
  procedure SkipStringNT;
  var
    b: Byte;
  begin
    repeat
      m_fs.Read(b, 1);
    until b = 0;
  end;
var
  Header: TPackHeader;
  fld: PG2PackFolder;
  f: PG2PackFile;
  i, j: Integer;
begin
  if not FileExists(FileName) then Exit;
  if m_IsOpen then ClosePack;
  m_fs := TFileStream.Create(FileName, fmOpenRead);
  if m_fs.Size < SizeOf(Header) then
  begin
    m_fs.Free;
    Exit;
  end;
  try
    m_fs.Read(Header, SizeOf(Header));
    if (Header.Definition <> 'G2PK')
    or (Header.Version <> $00010000) then
    begin
      m_fs.Free;
      Exit;
    end;
    m_DataPos := Header.DataPos;
    SetLength(m_Folders, Header.FolderCount);
    for i := 0 to High(m_Folders) do
    begin
      New(m_Folders[i]);
      fld := m_Folders[i];
      fld^.Name := ReadStringNT;
      Skip(1);
      SetLength(fld^.Files, ReadDWord);
      for j := 0 to High(fld^.Files) do
      begin
        New(fld^.Files[j]);
        f := fld^.Files[j];
        f^.Name := ReadStringNT;
        SkipStringNT;
        f^.Encrypted := ReadBool;
        f^.Compressed := ReadBool;
        Skip(4);
        f^.DataPos := ReadDWord;
        f^.DataLength := ReadDWord;
        f^.OriginalSize := ReadDWord;
        m_fs.Read(f^.CRC, SizeOf(f^.CRC));
        f^.Loaded := False;
      end;
    end;
  except
    m_fs.Free;
  end;
  m_IsOpen := True;
end;

procedure TG2Pack.ClosePack;
var
  fld: PG2PackFolder;
  f: PG2PackFile;
begin
  if not m_IsOpen then Exit;
  for fld in m_Folders do
  begin
    for f in fld^.Files do
    Dispose(f);
    Dispose(fld);
  end;
  m_fs.Free;
  m_IsOpen := False;
end;

function TG2Pack.FindFolder(const Name: AnsiString): PG2PackFolder;
var
  fld: PG2PackFolder;
begin
  for fld in m_Folders do
  if LowerCase(String(fld^.Name)) = LowerCase(String(Name)) then
  begin
    Result := fld;
    Exit;
  end;
  Result := nil;
end;

function TG2Pack.FindFile(const fld: PG2PackFolder; const Name: AnsiString): PG2PackFile;
var
  f: PG2PackFile;
begin
  for f in fld^.Files do
  if LowerCase(String(f^.Name)) = LowerCase(String(Name)) then
  begin
    Result := f;
    Exit;
  end;
  Result := nil;
end;

function TG2Pack.FindFile(const FolderName: AnsiString; const Name: AnsiString): PG2PackFile;
var
  fld: PG2PackFolder;
begin
  Result := nil;
  fld := FindFolder(FolderName);
  if Assigned(fld) then
  Result := FindFile(fld, Name);
end;

function TG2Pack.PackFileExists(const FileName: AnsiString): Boolean;
  var i: Integer;
begin
  for i := 0 to High(m_Folders) do
  begin
    if FindFile(m_Folders[i], FileName) <> nil then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TG2Pack.PreLoadFile(const f: PG2PackFile);
var
  ds: TDecompressionStream;
  ms: TMemoryStream;
  i: Integer;
begin
  if f^.Loaded then Exit;
  m_fs.Position := m_DataPos + f^.DataPos;
  SetLength(f^.Data, f^.DataLength);
  m_fs.Read(f^.Data[0], f^.DataLength);
  if f^.Encrypted then
  begin
    try
      G2EncDec(@f^.Data[0], f^.DataLength, m_Key);
      for i := 0 to High(f^.CRC) do
      if f^.Data[f^.CRC[i].Pos] <> f^.CRC[i].Num then
      begin
        f^.Data := nil;
        Exit;
      end;
    except
      f^.Data := nil;
      Exit;
    end;
  end;
  if f^.Compressed then
  begin
    try
      ms := TMemoryStream.Create;
      try
        ms.Write(f^.Data[0], f^.DataLength);
        SetLength(f^.Data, f^.OriginalSize);
        ms.Position := 0;
        ds := TDecompressionStream.Create(ms);
        try
          ds.Read(f^.Data[0], f^.OriginalSize);
        finally
          ds.Free;
        end;
      finally
        ms.Free;
      end;
    except
      f^.Data := nil;
      Exit;
    end;
  end;
  f^.Loaded := True;
end;

procedure TG2Pack.UnLoadFile(const f: PG2PackFile);
begin
  f^.Data := nil;
  f^.Loaded := False;
end;

procedure TG2Pack.GetFileData(const f: PG2PackFile; var Data: Pointer; var DataSize: DWord);
begin
  if not f^.Loaded then
  begin
    PreLoadFile(f);
    if not f^.Loaded then
    begin
      Data := nil;
      DataSize := 0;
      Exit;
    end;
  end;
  Data := @f^.Data[0];
  DataSize := f^.OriginalSize;
end;

procedure TG2Pack.UnLoadFiles;
var
  fld: PG2PackFolder;
  f: PG2PackFile;
begin
  for fld in m_Folders do
  for f in fld^.Files do
  if f^.Loaded then
  UnLoadFile(f);
end;
//TG2Pack END

//TG2List BEGIN
destructor TG2List.Destroy;
begin
  FreeItems;
  inherited Destroy;
end;

procedure TG2List.FreeItems;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  TObject(Items[i]).Free;
  Clear;
end;
//TG2List END

//TG2QuickList BEGIN
procedure TG2QuickList.SetItem(const Index: Integer; const Value: Pointer);
begin
  m_Items[Index] := Value;
end;

function TG2QuickList.GetItem(const Index: Integer): Pointer;
begin
  Result := m_Items[Index];
end;

procedure TG2QuickList.SetCapacity(const Value: Integer);
begin
  SetLength(m_Items, Value);
end;

function TG2QuickList.GetCapacity: Integer;
begin
  Result := Length(m_Items);
end;

function TG2QuickList.GetFirst: Pointer;
begin
  if m_ItemCount > 0 then
  Result := m_Items[0]
  else
  Result := nil;
end;

function TG2QuickList.GetLast: Pointer;
begin
  if m_ItemCount > 0 then
  Result := m_Items[m_ItemCount - 1]
  else
  Result := nil;
end;

function TG2QuickList.Add(const Item: Pointer): Integer;
begin
  if Length(m_Items) <= m_ItemCount then
  SetLength(m_Items, Length(m_Items) + 32);
  m_Items[m_ItemCount] := Item;
  Result := m_ItemCount;
  Inc(m_ItemCount);
end;

function TG2QuickList.Pop: Pointer;
begin
  if m_ItemCount > 0 then
  begin
    Result := m_Items[m_ItemCount - 1];
    Delete(m_ItemCount - 1);
  end
  else
  Result := nil;
end;

function TG2QuickList.Insert(const Index: Integer; const Item: Pointer): Integer;
  var i: Integer;
begin
  if Length(m_Items) <= m_ItemCount then
  SetLength(m_Items, Length(m_Items) + 32);
  if Index < m_ItemCount then
  begin
    for i := m_ItemCount downto Index do
    m_Items[i + 1] := m_Items[i];
    m_Items[Index] := Item;
    Result := Index;
  end
  else
  begin
    m_Items[m_ItemCount] := Item;
    Result := m_ItemCount;
  end;
  Inc(m_ItemCount);
end;

procedure TG2QuickList.Delete(const Index: Integer);
  var i: Integer;
begin
  for i := Index to m_ItemCount - 2 do
  m_Items[i] := m_Items[i + 1];
  Dec(m_ItemCount);
end;

procedure TG2QuickList.Remove(const Item: Pointer);
  var i: Integer;
begin
  for i := 0 to m_ItemCount - 1 do
  if m_Items[i] = Item then
  begin
    Delete(i);
    Exit;
  end;
end;

procedure TG2QuickList.Clear;
begin
  m_ItemCount := 0;
end;
//TG2QuickList END

//TG2QuickSortList BEGIN
procedure TG2QuickSortList.SetItem(const Index: Integer; const Value: Pointer);
begin
  m_Items[Index].Data := Value;
end;

function TG2QuickSortList.GetItem(const Index: Integer): Pointer;
begin
  Result := m_Items[Index].Data;
end;

procedure TG2QuickSortList.InsertItem(const Pos: Integer; const Item: Pointer; const Order: Single);
  var i: Integer;
begin
  if Length(m_Items) <= m_ItemCount then
  SetLength(m_Items, Length(m_Items) + 32);
  for i := m_ItemCount downto Pos + 1 do
  begin
    m_Items[i].Data := m_Items[i - 1].Data;
    m_Items[i].Order := m_Items[i - 1].Order;
  end;
  m_Items[Pos].Data := Item;
  m_Items[Pos].Order := Order;
  Inc(m_ItemCount);
end;

procedure TG2QuickSortList.SetCapacity(const Value: Integer);
begin
  SetLength(m_Items, Value);
end;

function TG2QuickSortList.GetCapacity: Integer;
begin
  Result := Length(m_Items);
end;

function TG2QuickSortList.GetFirst: Pointer;
begin
  if m_ItemCount > 0 then
  Result := m_Items[0].Data
  else
  Result := nil;
end;

function TG2QuickSortList.GetLast: Pointer;
begin
  if m_ItemCount > 0 then
  Result := m_Items[m_ItemCount - 1].Data
  else
  Result := nil;
end;

function TG2QuickSortList.Add(const Item: Pointer; const Order: Single): Integer;
  var l, h, m: Integer;
begin
  l := 0;
  h := m_ItemCount - 1;
  while l <= h do
  begin
    m := (l + h) div 2;
    if m_Items[m].Order - Order < 0 then
    l := m + 1 else h := m - 1;
  end;
  InsertItem(l, Item, Order);
  Result := l;
end;

function TG2QuickSortList.Add(const Item: Pointer): Integer;
begin
  if Length(m_Items) <= m_ItemCount then
  SetLength(m_Items, Length(m_Items) + 32);
  m_Items[m_ItemCount].Data := Item;
  m_Items[m_ItemCount].Order := 0;
  Result := m_ItemCount;
  Inc(m_ItemCount);
end;

function TG2QuickSortList.Pop: Pointer;
begin
  if m_ItemCount > 0 then
  begin
    Result := m_Items[m_ItemCount - 1].Data;
    Delete(m_ItemCount - 1);
  end
  else
  Result := nil;
end;

procedure TG2QuickSortList.Delete(const Index: Integer);
  var i: Integer;
begin
  for i := Index to m_ItemCount - 2 do
  begin
    m_Items[i].Data := m_Items[i + 1].Data;
    m_Items[i].Order := m_Items[i + 1].Order;
  end;
  Dec(m_ItemCount);
end;

procedure TG2QuickSortList.Remove(const Item: Pointer);
  var i: Integer;
begin
  for i := 0 to m_ItemCount - 1 do
  if m_Items[i].Data = Item then
  begin
    Delete(i);
    Exit;
  end;
end;

procedure TG2QuickSortList.Clear;
begin
  m_ItemCount := 0;
end;
//TG2QuickSortList END

//TG2SortedList BEGIn
constructor TG2SortedList.Create;
begin
  inherited Create;
  m_List := TList.Create;
end;

destructor TG2SortedList.Destroy;
begin
  Clear;
  m_List.Free;
  inherited Destroy;
end;

function TG2SortedList.GetItem(const Index: Integer): Pointer;
begin
  Result := PSortedItem(m_List[Index])^.Item;
end;

function TG2SortedList.GetCount: Integer;
begin
  Result := m_List.Count;
end;

function TG2SortedList.FindItemIndex(const Order: Integer): Integer;
var
  l, h, m: Integer;
begin
  l := 0;
  h := m_List.Count - 1;
  while l <= h do
  begin
    m := (l + h) div 2;
    if PSortedItem(m_List[m])^.Order - Order < 0 then
    l := m + 1
    else
    h := m - 1;
  end;
  Result := l;
end;

procedure TG2SortedList.Add(const Item: Pointer; const Order: Integer);
var
  SortedItem: PSortedItem;
begin
  New(SortedItem);
  SortedItem^.Item := Item;
  SortedItem^.Order := Order;
  m_List.Insert(FindItemIndex(Order), SortedItem);
end;

procedure TG2SortedList.Clear;
var
  i: Integer;
begin
  for i := 0 to m_List.Count - 1 do
  Dispose(PSortedItem(m_List[i]));
  m_List.Clear;
end;
//TG2SortedList END

//TG2LinkedListItem BEGIN
constructor TG2LinkedListItem.Create;
begin
  inherited Create;
end;

destructor TG2LinkedListItem.Destroy;
begin
  inherited Destroy;
end;
//TG2LinkedListItem END

//TG2LinkedList BEGIN
constructor TG2LinkedList.Create;
begin
  inherited Create;
  m_First := nil;
  m_Last := nil;
  m_Cur := nil;
  m_FuncNext := NextFirst;
end;

destructor TG2LinkedList.Destroy;
begin
  inherited Destroy;
end;

function TG2LinkedList.NextFirst: TG2LinkedListItem;
begin
  m_Cur := m_First;
  m_FuncNext := NextNext;
  Result := m_Cur;
end;

function TG2LinkedList.NextNext: TG2LinkedListItem;
begin
  m_Cur := m_Cur.Next;
  Result := m_Cur;
end;

function TG2LinkedList.Next: TG2LinkedListItem;
begin
  Result := m_FuncNext;
end;

procedure TG2LinkedList.Reset;
begin
  m_Cur := nil;
  m_FuncNext := NextFirst;
end;

procedure TG2LinkedList.AddItem(const Item: TG2LinkedListItem);
begin
  if m_First = nil then
  begin
    Item.Next := nil;
    Item.Prev := nil;
    m_First := Item;
    m_Last := Item;
  end
  else
  begin
    Item.Next := nil;
    Item.Prev := m_Last;
    m_Last.Next := Item;
    m_Last := Item;
  end;
end;

procedure TG2LinkedList.RemoveItem(const Item: TG2LinkedListItem);
begin
  if (Item = m_First) and (Item = m_Last) then
  begin
    m_First := nil;
    m_Last := nil;
  end
  else if Item = m_First then
  begin
    m_First := Item.Next;
    Item.Next.Prev := nil;
  end
  else if Item = m_Last then
  begin
    m_Last := Item.Prev;
    Item.Prev.Next := nil;
  end
  else
  begin
    Item.Next.Prev := Item.Prev;
    Item.Prev.Next := Item.Next;
  end;
  Item.Free;
end;
//TG2LinkedList END

//TG2FileRW BEGIN
constructor TG2FileRW.Create;
begin
  inherited Create;
  m_Open := False;
  m_Compression := False;
  m_Op := rwIdle;
  SetDummy;
end;

destructor TG2FileRW.Destroy;
begin
  if m_Open then Close;
  inherited Destroy;
end;

function TG2FileRW.GetPositionDummy: Int64;
begin
  Result := 0;
end;

procedure TG2FileRW.SetPositionDummy(const Value: Int64);
begin

end;

function TG2FileRW.GetPositionProc: Int64;
begin
  Result := m_fs.Position;
end;

procedure TG2FileRW.SetPositionProc(const Value: Int64);
begin
  m_fs.Position := Value;
end;

function TG2FileRW.SeekDummy(const Offset: Int64; Origin: TSeekOrigin = soCurrent): Int64;
begin
  Result := 0;
  G2WriteLogTimed('(W) Attempt to seek an uninitialized stream', 'FileRW');
end;

function TG2FileRW.SeekProc(const Offset: Int64; Origin: TSeekOrigin = soCurrent): Int64;
begin
  Result := m_ws.Seek(Offset, Origin);
end;

procedure TG2FileRW.ReadBufferDummy(var Buffer; const Size: Integer);
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.ReadBufferProc(var Buffer; const Size: Integer);
begin
  m_ws.ReadBuffer(Buffer, Size);
end;

function TG2FileRW.ReadBoolDummy: Boolean;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := False;
end;

function TG2FileRW.ReadBoolProc: Boolean;
begin
  m_ws.ReadBuffer(Result, 1);
end;

function TG2FileRW.ReadUInt1Dummy: Byte;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadUInt1Proc: Byte;
begin
  m_ws.ReadBuffer(Result, 1);
end;

function TG2FileRW.ReadSInt1Dummy: ShortInt;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadSInt1Proc: ShortInt;
begin
  m_ws.ReadBuffer(Result, 1);
end;

function TG2FileRW.ReadUInt2Dummy: Word;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadUInt2Proc: Word;
begin
  m_ws.ReadBuffer(Result, 2);
end;

function TG2FileRW.ReadSInt2Dummy: SmallInt;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadSInt2Proc: SmallInt;
begin
  m_ws.ReadBuffer(Result, 2);
end;

function TG2FileRW.ReadUInt4Dummy: DWord;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadUInt4Proc: DWord;
begin
  m_ws.ReadBuffer(Result, 4);
end;

function TG2FileRW.ReadSInt4Dummy: Integer;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadSInt4Proc: Integer;
begin
  m_ws.ReadBuffer(Result, 4);
end;

function TG2FileRW.ReadSInt8Dummy: Int64;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadSInt8Proc: Int64;
begin
  m_ws.ReadBuffer(Result, 8);
end;

function TG2FileRW.ReadFloat4Dummy: Single;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadFloat4Proc: Single;
begin
  m_ws.ReadBuffer(Result, 4);
end;

function TG2FileRW.ReadFloat8Dummy: Double;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadFloat8Proc: Double;
begin
  m_ws.ReadBuffer(Result, 8);
end;

function TG2FileRW.ReadStringNTDummy: AnsiString;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := '';
end;

function TG2FileRW.ReadStringNTProc: AnsiString;
var
  c: AnsiChar;
begin
  Result := '';
  m_ws.ReadBuffer(c, 1);
  while c <> #0 do
  begin
    Result := Result + c;
    m_ws.ReadBuffer(c, 1);
  end;
end;

function TG2FileRW.ReadStringDummy: AnsiString;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := '';
end;

function TG2FileRW.ReadStringProc: AnsiString;
  var l: Integer;
begin
  m_ws.ReadBuffer(l, 4);
  SetLength(Result, l);
  m_ws.ReadBuffer(Result[1], l);
end;

function TG2FileRW.ReadWideStringNTDummy: WideString;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := '';
end;

function TG2FileRW.ReadWideStringNTProc: WideString;
var
  c: WideChar;
begin
  Result := '';
  m_ws.ReadBuffer(c, 2);
  while c <> #0 do
  begin
    Result := Result + c;
    m_ws.ReadBuffer(c, 2);
  end;
end;

function TG2FileRW.ReadWideStringDummy: WideString;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := '';
end;

function TG2FileRW.ReadWideStringProc: WideString;
  var l: Integer;
begin
  m_ws.ReadBuffer(l, 4);
  SetLength(Result, l);
  m_ws.ReadBuffer(Result[1], l * 2);
end;

function TG2FileRW.ReadVec2Dummy: TG2Vec2;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := G2Vec2(0, 0);
end;

function TG2FileRW.ReadVec2Proc: TG2Vec2;
begin
  m_ws.ReadBuffer(Result, SizeOf(TG2Vec2));
end;

function TG2FileRW.ReadVec3Dummy: TG2Vec3;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := G2Vec3(0, 0, 0);
end;

function TG2FileRW.ReadVec3Proc: TG2Vec3;
begin
  m_ws.ReadBuffer(Result, SizeOf(TG2Vec3));
end;

function TG2FileRW.ReadVec4Dummy: TG2Vec4;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := G2Vec4(0, 0, 0, 0);
end;

function TG2FileRW.ReadVec4Proc: TG2Vec4;
begin
  m_ws.ReadBuffer(Result, SizeOf(TG2Vec4));
end;

function TG2FileRW.ReadQuatDummy: TG2Quat;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result.SetIdentity;
end;

function TG2FileRW.ReadQuatProc: TG2Quat;
begin
  m_ws.ReadBuffer(Result, SizeOf(TG2Quat));
end;

function TG2FileRW.ReadMat4x4Dummy: TG2Mat;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result.SetIdentity;
end;

function TG2FileRW.ReadMat4x4Proc: TG2Mat;
begin
  m_ws.ReadBuffer(Result, SizeOf(TG2Mat));
end;

function TG2FileRW.ReadMat4x3Dummy: TG2Mat;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result.SetIdentity;
end;

function TG2FileRW.ReadMat4x3Proc: TG2Mat;
begin
  m_ws.ReadBuffer(Result.e00, SizeOf(TG2Vec3));
  m_ws.ReadBuffer(Result.e10, SizeOf(TG2Vec3));
  m_ws.ReadBuffer(Result.e20, SizeOf(TG2Vec3));
  m_ws.ReadBuffer(Result.e30, SizeOf(TG2Vec3));
  Result.e03 := 0;
  Result.e13 := 0;
  Result.e23 := 0;
  Result.e33 := 1;
end;

function TG2FileRW.ReadColorDummy: TG2Color;
begin
  G2WriteLogTimed('(W) Attempt to read an uninitialized stream', 'FileRW');
  Result := 0;
end;

function TG2FileRW.ReadColorProc: TG2Color;
begin
  m_ws.ReadBuffer(Result, 4);
end;

procedure TG2FileRW.WriteBufferDummy(const Buffer; const Size: Integer);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteBufferProc(const Buffer; const Size: Integer);
begin
  m_ws.WriteBuffer(Buffer, Size);
end;

procedure TG2FileRW.WriteBoolDummy(const Value: Boolean);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteBoolProc(const Value: Boolean);
begin
  m_ws.WriteBuffer(Value, 1);
end;

procedure TG2FileRW.WriteUInt1Dummy(const Value: Byte);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteUInt1Proc(const Value: Byte);
begin
  m_ws.WriteBuffer(Value, 1);
end;

procedure TG2FileRW.WriteSInt1Dummy(const Value: ShortInt);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteSInt1Proc(const Value: ShortInt);
begin
  m_ws.WriteBuffer(Value, 1);
end;

procedure TG2FileRW.WriteUInt2Dummy(const Value: Word);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteUInt2Proc(const Value: Word);
begin
  m_ws.WriteBuffer(Value, 2);
end;

procedure TG2FileRW.WriteSInt2Dummy(const Value: SmallInt);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteSInt2Proc(const Value: SmallInt);
begin
  m_ws.WriteBuffer(Value, 2);
end;

procedure TG2FileRW.WriteUInt4Dummy(const Value: DWord);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteUInt4Proc(const Value: DWord);
begin
  m_ws.WriteBuffer(Value, 4);
end;

procedure TG2FileRW.WriteSInt4Dummy(const Value: Integer);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteSInt4Proc(const Value: Integer);
begin
  m_ws.WriteBuffer(Value, 4);
end;

procedure TG2FileRW.WriteSInt8Dummy(const Value: Int64);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteSInt8Proc(const Value: Int64);
begin
  m_ws.WriteBuffer(Value, 8);
end;

procedure TG2FileRW.WriteFloat4Dummy(const Value: Single);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteFloat4Proc(const Value: Single);
begin
  m_ws.WriteBuffer(Value, 4);
end;

procedure TG2FileRW.WriteFloat8Dummy(const Value: Double);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteFloat8Proc(const Value: Double);
begin
  m_ws.WriteBuffer(Value, 8);
end;

procedure TG2FileRW.WriteStringNTDummy(const Value: AnsiString);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteStringNTProc(const Value: AnsiString);
var
  nt: Byte;
begin
  nt := 0;
  m_ws.WriteBuffer(Value[1], Length(Value));
  m_ws.WriteBuffer(nt, 1);
end;

procedure TG2FileRW.WriteStringDummy(const Value: AnsiString);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteStringProc(const Value: AnsiString);
  var l: Integer;
begin
  l := Length(Value);
  m_ws.WriteBuffer(l, 4);
  m_ws.WriteBuffer(Value[1], l);
end;

procedure TG2FileRW.WriteWideStringNTDummy(const Value: WideString);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteWideStringNTProc(const Value: WideString);
var
  nt: Byte;
begin
  nt := 0;
  m_ws.WriteBuffer(Value[1], Length(Value));
  m_ws.WriteBuffer(nt, 1);
end;

procedure TG2FileRW.WriteWideStringDummy(const Value: WideString);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteWideStringProc(const Value: WideString);
  var l: Integer;
begin
  l := Length(Value);
  m_ws.WriteBuffer(l, 4);
  m_ws.WriteBuffer(Value[1], l * 2);
end;

procedure TG2FileRW.WriteVec2Dummy(const Value: TG2Vec2);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteVec2Proc(const Value: TG2Vec2);
begin
  m_ws.WriteBuffer(Value, SizeOf(TG2Vec2));
end;

procedure TG2FileRW.WriteVec3Dummy(const Value: TG2Vec3);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteVec3Proc(const Value: TG2Vec3);
begin
  m_ws.WriteBuffer(Value, SizeOf(TG2Vec3));
end;

procedure TG2FileRW.WriteVec4Dummy(const Value: TG2Vec4);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteVec4Proc(const Value: TG2Vec4);
begin
  m_ws.WriteBuffer(Value, SizeOf(TG2Vec4));
end;

procedure TG2FileRW.WriteQuatDummy(const Value: TG2Quat);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteQuatProc(const Value: TG2Quat);
begin
  m_ws.WriteBuffer(Value, SizeOf(TG2Quat));
end;

procedure TG2FileRW.WriteMat4x4Dummy(const Value: TG2Mat);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteMat4x4Proc(const Value: TG2Mat);
begin
  m_ws.WriteBuffer(Value, SizeOf(TG2Mat));
end;

procedure TG2FileRW.WriteMat4x3Dummy(const Value: TG2Mat);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteMat4x3Proc(const Value: TG2Mat);
begin
  m_ws.WriteBuffer(Value.e00, SizeOf(TG2Vec3));
  m_ws.WriteBuffer(Value.e10, SizeOf(TG2Vec3));
  m_ws.WriteBuffer(Value.e20, SizeOf(TG2Vec3));
  m_ws.WriteBuffer(Value.e30, SizeOf(TG2Vec3));
end;

procedure TG2FileRW.WriteColorDummy(const Value: TG2Color);
begin
  G2WriteLogTimed('(W) Attempt to write an uninitialized stream', 'FileRW');
end;

procedure TG2FileRW.WriteColorProc(const Value: TG2Color);
begin
  m_ws.WriteBuffer(Value, 4);
end;

function TG2FileRW.GetPosition: Int64;
begin
  Result := m_ProcGetPosition;
end;

procedure TG2FileRW.SetPosition(const Value: Int64);
begin
  m_ProcSetPosition(Value);
end;

procedure TG2FileRW.SetCompression(const Value: Boolean);
var
  ds: TDecompressionStream;
  cs: TCompressionStream;
begin
  if m_Compression = Value then Exit;
  m_Compression := Value;
  if m_Compression then
  begin
    case m_Op of
      rwRead:
      begin
        ds := TDecompressionStream.Create(m_fs);
        m_cds := ds;
        m_ws := m_cds;
      end;
      rwWrite:
      begin
        cs := TCompressionStream.Create(clMax, m_fs);
        m_cds := cs;
        m_ws := m_cds;
      end;
      else
      begin
        m_ws := m_fs;
        m_Compression := False;
      end;
    end;
  end
  else
  begin
    m_cds.Free;
    m_ws := m_fs;
  end;
end;

function TG2FileRW.GetSize: Int64;
begin
  Result := m_fs.Size;
end;

procedure TG2FileRW.SetDummy;
begin
  m_ProcGetPosition := GetPositionDummy;
  m_ProcSetPosition := SetPositionDummy;
  m_ProcSeek := SeekDummy;
  m_ProcReadBuffer := ReadBufferDummy;
  m_ProcReadBool := ReadBoolDummy;
  m_ProcReadUInt1 := ReadUInt1Dummy;
  m_ProcReadSInt1 := ReadSInt1Dummy;
  m_ProcReadUInt2 := ReadUInt2Dummy;
  m_ProcReadSInt2 := ReadSInt2Dummy;
  m_ProcReadUInt4 := ReadUInt4Dummy;
  m_ProcReadSInt4 := ReadSInt4Dummy;
  m_ProcReadSInt8 := ReadSInt8Dummy;
  m_ProcReadFloat4 := ReadFloat4Dummy;
  m_ProcReadFloat8 := ReadFloat8Dummy;
  m_ProcReadStringNT := ReadStringNTDummy;
  m_ProcReadString := ReadStringDummy;
  m_ProcReadWideStringNT := ReadWideStringNTDummy;
  m_ProcReadWideString := ReadWideStringDummy;
  m_ProcReadVec2 := ReadVec2Dummy;
  m_ProcReadVec3 := ReadVec3Dummy;
  m_ProcReadVec4 := ReadVec4Dummy;
  m_ProcReadQuat := ReadQuatDummy;
  m_ProcReadMat4x4 := ReadMat4x4Dummy;
  m_ProcReadMat4x3 := ReadMat4x3Dummy;
  m_ProcReadColor := ReadColorDummy;
  m_ProcWriteBuffer := WriteBufferDummy;
  m_ProcWriteBool := WriteBoolDummy;
  m_ProcWriteUInt1 := WriteUInt1Dummy;
  m_ProcWriteSInt1 := WriteSInt1Dummy;
  m_ProcWriteUInt2 := WriteUInt2Dummy;
  m_ProcWriteSInt2 := WriteSInt2Dummy;
  m_ProcWriteUInt4 := WriteUInt4Dummy;
  m_ProcWriteSInt4 := WriteSInt4Dummy;
  m_ProcWriteSInt8 := WriteSInt8Dummy;
  m_ProcWriteFloat4 := WriteFloat4Dummy;
  m_ProcWriteFloat8 := WriteFloat8Dummy;
  m_ProcWriteStringNT := WriteStringNTDummy;
  m_ProcWriteString := WriteStringDummy;
  m_ProcWriteWideStringNT := WriteWideStringNTDummy;
  m_ProcWriteWideString := WriteWideStringDummy;
  m_ProcWriteVec2 := WriteVec2Dummy;
  m_ProcWriteVec3 := WriteVec3Dummy;
  m_ProcWriteVec4 := WriteVec4Dummy;
  m_ProcWriteQuat := WriteQuatDummy;
  m_ProcWriteMat4x4 := WriteMat4x4Dummy;
  m_ProcWriteMat4x3 := WriteMat4x3Dummy;
  m_ProcWriteColor := WriteColorDummy;
end;

procedure TG2FileRW.SetProcs;
begin
  m_ProcGetPosition := GetPositionProc;
  m_ProcSetPosition := SetPositionProc;
  m_ProcSeek := SeekProc;
  m_ProcReadBuffer := ReadBufferProc;
  m_ProcReadBool := ReadBoolProc;
  m_ProcReadUInt1 := ReadUInt1Proc;
  m_ProcReadSInt1 := ReadSInt1Proc;
  m_ProcReadUInt2 := ReadUInt2Proc;
  m_ProcReadSInt2 := ReadSInt2Proc;
  m_ProcReadUInt4 := ReadUInt4Proc;
  m_ProcReadSInt4 := ReadSInt4Proc;
  m_ProcReadSInt8 := ReadSInt8Proc;
  m_ProcReadFloat4 := ReadFloat4Proc;
  m_ProcReadFloat8 := ReadFloat8Proc;
  m_ProcReadStringNT := ReadStringNTProc;
  m_ProcReadString := ReadStringProc;
  m_ProcReadWideStringNT := ReadWideStringNTProc;
  m_ProcReadWideString := ReadWideStringProc;
  m_ProcReadVec2 := ReadVec2Proc;
  m_ProcReadVec3 := ReadVec3Proc;
  m_ProcReadVec4 := ReadVec4Proc;
  m_ProcReadQuat := ReadQuatProc;
  m_ProcReadMat4x4 := ReadMat4x4Proc;
  m_ProcReadMat4x3 := ReadMat4x3Proc;
  m_ProcReadColor := ReadColorProc;
  m_ProcWriteBuffer := WriteBufferProc;
  m_ProcWriteBool := WriteBoolProc;
  m_ProcWriteUInt1 := WriteUInt1Proc;
  m_ProcWriteSInt1 := WriteSInt1Proc;
  m_ProcWriteUInt2 := WriteUInt2Proc;
  m_ProcWriteSInt2 := WriteSInt2Proc;
  m_ProcWriteUInt4 := WriteUInt4Proc;
  m_ProcWriteSInt4 := WriteSInt4Proc;
  m_ProcWriteSInt8 := WriteSInt8Proc;
  m_ProcWriteFloat4 := WriteFloat4Proc;
  m_ProcWriteFloat8 := WriteFloat8Proc;
  m_ProcWriteStringNT := WriteStringNTProc;
  m_ProcWriteString := WriteStringProc;
  m_ProcWriteWideStringNT := WriteWideStringNTProc;
  m_ProcWriteWideString := WriteWideStringProc;
  m_ProcWriteVec2 := WriteVec2Proc;
  m_ProcWriteVec3 := WriteVec3Proc;
  m_ProcWriteVec4 := WriteVec4Proc;
  m_ProcWriteQuat := WriteQuatProc;
  m_ProcWriteMat4x4 := WriteMat4x4Proc;
  m_ProcWriteMat4x3 := WriteMat4x3Proc;
  m_ProcWriteColor := WriteColorProc;
end;

procedure TG2FileRW.OpenRead(const FileName: WideString);
begin
  if m_Open then Close;
  m_fs := TFileStream.Create(FileName, fmOpenRead);
  m_Compression := False;
  m_Open := True;
  m_Op := rwRead;
  m_ws := m_fs;
  SetProcs;
end;

procedure TG2FileRW.OpenWrite(const FileName: WideString);
begin
  if m_Open then Close;
  m_fs := TFileStream.Create(FileName, fmCreate);
  m_Compression := False;
  m_Open := True;
  m_Op := rwWrite;
  m_ws := m_fs;
  SetProcs;
end;

procedure TG2FileRW.Close;
begin
  if not m_Open then Exit;
  if m_Compression then
  m_cds.Free;
  m_fs.Free;
  m_Open := False;
  SetDummy;
end;

function TG2FileRW.Seek(const Offset: Int64; Origin: TSeekOrigin = soCurrent): Int64;
begin
  Result := m_ProcSeek(Offset, Origin);
end;

procedure TG2FileRW.ReadBuffer(var Buffer; const Size: Integer);
begin
  m_ProcReadBuffer(Buffer, Size);
end;

function TG2FileRW.ReadBool: Boolean;
begin
  Result := m_ProcReadBool;
end;

function TG2FileRW.ReadUInt1: Byte;
begin
  Result := m_ProcReadUInt1;
end;

function TG2FileRW.ReadSInt1: ShortInt;
begin
  Result := m_ProcReadSInt1;
end;

function TG2FileRW.ReadUInt2: Word;
begin
  Result := m_ProcReadUInt2;
end;

function TG2FileRW.ReadSInt2: SmallInt;
begin
  Result := m_ProcReadSInt2;
end;

function TG2FileRW.ReadUInt4: DWord;
begin
  Result := m_ProcReadUInt4;
end;

function TG2FileRW.ReadSInt4: Integer;
begin
  Result := m_ProcReadSInt4;
end;

function TG2FileRW.ReadSInt8: Int64;
begin
  Result := m_ProcReadSInt8;
end;

function TG2FileRW.ReadFloat4: Single;
begin
  Result := m_ProcReadFloat4;
end;

function TG2FileRW.ReadFloat8: Double;
begin
  Result := m_ProcReadFloat8;
end;

function TG2FileRW.ReadStringNT: AnsiString;
begin
  Result := m_ProcReadStringNT;
end;

function TG2FileRW.ReadString: AnsiString;
begin
  Result := m_ProcReadString;
end;

function TG2FileRW.ReadWideStringNT: WideString;
begin
  Result := m_ProcReadWideStringNT;
end;

function TG2FileRW.ReadWideString: WideString;
begin
  Result := m_ProcReadWideString;
end;

function TG2FileRW.ReadVec2: TG2Vec2;
begin
  Result := m_ProcReadVec2;
end;

function TG2FileRW.ReadVec3: TG2Vec3;
begin
  Result := m_ProcReadVec3;
end;

function TG2FileRW.ReadVec4: TG2Vec4;
begin
  Result := m_ProcReadVec4;
end;

function TG2FileRW.ReadQuat: TG2Quat;
begin
  Result := m_ProcReadQuat;
end;

function TG2FileRW.ReadMat4x4: TG2Mat;
begin
  Result := m_ProcReadMat4x4;
end;

function TG2FileRW.ReadMat4x3: TG2Mat;
begin
  Result := m_ProcReadMat4x3;
end;

function TG2FileRW.ReadColor: TG2Color;
begin
  Result := m_ProcReadColor;
end;

procedure TG2FileRW.WriteBuffer(const Buffer; const Size: Integer);
begin
  m_ProcWriteBuffer(Buffer, Size);
end;

procedure TG2FileRW.WriteBool(const Value: Boolean);
begin
  m_ProcWriteBool(Value);
end;

procedure TG2FileRW.WriteUInt1(const Value: Byte);
begin
  m_ProcWriteUInt1(Value);
end;

procedure TG2FileRW.WriteSInt1(const Value: ShortInt);
begin
  m_ProcWriteSInt1(Value);
end;

procedure TG2FileRW.WriteUInt2(const Value: Word);
begin
  m_ProcWriteUInt2(Value);
end;

procedure TG2FileRW.WriteSInt2(const Value: SmallInt);
begin
  m_ProcWriteSInt2(Value);
end;

procedure TG2FileRW.WriteUInt4(const Value: DWord);
begin
  m_ProcWriteUInt4(Value);
end;

procedure TG2FileRW.WriteSInt4(const Value: Integer);
begin
  m_ProcWriteSInt4(Value);
end;

procedure TG2FileRW.WriteSInt8(const Value: Int64);
begin
  m_ProcWriteSInt8(Value);
end;

procedure TG2FileRW.WriteFloat4(const Value: Single);
begin
  m_ProcWriteFloat4(Value);
end;

procedure TG2FileRW.WriteFloat8(const Value: Double);
begin
  m_ProcWriteFloat8(Value);
end;

procedure TG2FileRW.WriteStringNT(const Value: AnsiString);
begin
  m_ProcWriteStringNT(Value);
end;

procedure TG2FileRW.WriteString(const Value: AnsiString);
begin
  m_ProcWriteString(Value);
end;

procedure TG2FileRW.WriteWideStringNT(const Value: WideString);
begin
  m_ProcWriteWideStringNT(Value);
end;

procedure TG2FileRW.WriteWideString(const Value: WideString);
begin
  m_ProcWriteWideString(Value);
end;

procedure TG2FileRW.WriteVec2(const Value: TG2Vec2);
begin
  m_ProcWriteVec2(Value);
end;

procedure TG2FileRW.WriteVec3(const Value: TG2Vec3);
begin
  m_ProcWriteVec3(Value);
end;

procedure TG2FileRW.WriteVec4(const Value: TG2Vec4);
begin
  m_ProcWriteVec4(Value);
end;

procedure TG2FileRW.WriteQuat(const Value: TG2Quat);
begin
  m_ProcWriteQuat(Value);
end;

procedure TG2FileRW.WriteMat4x4(const Value: TG2Mat);
begin
  m_ProcWriteMat4x4(Value);
end;

procedure TG2FileRW.WriteMat4x3(const Value: TG2Mat);
begin
  m_ProcWriteMat4x3(Value);
end;

procedure TG2FileRW.WriteColor(const Value: TG2Color);
begin
  m_ProcWriteColor(Value);
end;

procedure TG2FileRW.Write(const Value: Boolean);
begin
  m_ProcWriteBool(Value);
end;

procedure TG2FileRW.Write(const Value: Byte);
begin
  m_ProcWriteUInt1(Value);
end;

procedure TG2FileRW.Write(const Value: ShortInt);
begin
  m_ProcWriteSInt1(Value);
end;

procedure TG2FileRW.Write(const Value: Word);
begin
  m_ProcWriteUInt2(Value);
end;

procedure TG2FileRW.Write(const Value: SmallInt);
begin
  m_ProcWriteSInt2(Value);
end;

procedure TG2FileRW.Write(const Value: DWord);
begin
  m_ProcWriteUInt4(Value);
end;

procedure TG2FileRW.Write(const Value: Integer);
begin
  m_ProcWriteSInt4(Value);
end;

procedure TG2FileRW.Write(const Value: Int64);
begin
  m_ProcWriteSInt8(Value);
end;

procedure TG2FileRW.Write(const Value: Single);
begin
  m_ProcWriteFloat4(Value);
end;

procedure TG2FileRW.Write(const Value: Double);
begin
  m_ProcWriteFloat8(Value);
end;

procedure TG2FileRW.Write(const Value: AnsiString);
begin
  m_ProcWriteStringNT(Value);
end;

procedure TG2FileRW.Write(const Value: WideString);
begin
  m_ProcWriteWideStringNT(Value);
end;

procedure TG2FileRW.Write(const Value: TG2Vec2);
begin
  m_ProcWriteVec2(Value);
end;

procedure TG2FileRW.Write(const Value: TG2Vec3);
begin
  m_ProcWriteVec3(Value);
end;

procedure TG2FileRW.Write(const Value: TG2Vec4);
begin
  m_ProcWriteVec4(Value);
end;

procedure TG2FileRW.Write(const Value: TG2Quat);
begin
  m_ProcWriteQuat(Value);
end;

procedure TG2FileRW.Write(const Value: TG2Mat);
begin
  m_ProcWriteMat4x4(Value);
end;

procedure TG2FileRW.Write(const Value: TG2Color);
begin
  m_ProcWriteColor(Value);
end;
//TG2FileRW END

//TG2Engine BEGIN
constructor TG2Engine.Create;
begin
  inherited Create;
  m_Plugs := TG2List.Create;
  m_PlugClass := TG2Plug;
  m_Powered := True;
end;

destructor TG2Engine.Destroy;
begin
  Finalize;
  m_Plugs.Free;
  inherited Destroy;
end;

function TG2Engine.Initialize(const G2Core: TG2Core): TG2Result;
begin
  if not m_Powered then
  begin
    Result := grInvalidCall;
    Exit;
  end;
  Result := inherited Initialize(G2Core);
end;

function TG2Engine.Finalize: TG2Result;
begin
  if not m_Powered then
  begin
    Result := grInvalidCall;
    Exit;
  end;
  Result := inherited Finalize;
end;

function TG2Engine.RequestPlug(
      const PlugClass: CG2PlugClass;
      const Plug: PG2Plug
    ): TG2Result;
begin
  if not Initialized then
  begin
    Result := grNotInitialized;
    Exit;
  end;
  if m_PlugClass <> PlugClass then
  begin
    Result := grInvalidCall;
    Exit;
  end;
  Plug^ := PlugClass.Create;
  Plug^.Engine := Self;
  Plug^.Initialize(Core);
  m_Plugs.Add(Plug^);
  Result := grOk;
end;

function TG2Engine.ReleasePlug(
      const Plug: PG2Plug
    ): TG2Result;
begin
  if not Initialized then
  begin
    Result := grNotInitialized;
    Exit;
  end;
  if not (Plug^ is m_PlugClass) then
  begin
    Result := grInvalidCall;
    Exit;
  end;
  m_Plugs.Remove(Plug^);
  Plug^.Finalize;
  Plug^.Free;
  Result := grOk;
end;
//TG2Engine END

//TG2Timer BEGIN
constructor TG2Timer.Create;
begin
  inherited Create;
  m_PlugClass := TG2PlugTimer;
  m_QPCAvailable := QueryPerformanceFrequency(m_Frequency);
  if not m_QPCAvailable then m_Frequency := 1000;
  m_MaxFPS := 0;
  m_TargetUPS := 50;
  m_FullSpeed := True;
  m_Mode := tmWinTimer;
  m_CS := TCriticalSection.Create;
end;

destructor TG2Timer.Destroy;
begin
  m_CS.Free;
  inherited Destroy;
end;

procedure TG2Timer.Start;
const
  TIME_KILL_SYNCHRONOUS = $0100;
begin
  ResetTimer;
  case m_Mode of
    tmAppIdle: Application.OnIdle := OnIdle;
    tmWinTimer: SetTimer(m_WndHandle, 1, 1, nil);
  end;
  m_TotalFPS := 0;
  m_TotalFPSCount := 0;
  G2WriteLogTimed('Timer Started.', 'Timer');
end;

procedure TG2Timer.Stop;
begin
  case m_Mode of
    tmAppIdle: Application.OnIdle := m_PrevIdle;
    tmWinTimer: KillTimer(m_WndHandle, 1);
  end;
  G2WriteLogTimed('Timer Stoped.', 'Timer');
  if m_TotalFPSCount > 0 then
  G2WriteLogTimed(AnsiString('Timer Average FPS = ' + IntToStr((m_TotalFPS div m_TotalFPSCount))), 'Timer');
end;

procedure TG2Timer.InitMode;
begin
  case m_Mode of
    tmAppIdle: m_PrevIdle := Application.OnIdle;
    tmWinTimer: m_WndHandle := Classes.AllocateHWnd(WndProc);
  end;
end;

procedure TG2Timer.UnInitMode;
begin
  case m_Mode of
    tmAppIdle: m_PrevIdle := nil;
    tmWinTimer:
    begin
      Classes.DeallocateHWnd(m_WndHandle);
      m_WndHandle := 0;
    end;
  end;
end;

procedure TG2Timer.SetMode(const Value: TG2TimerMode);
begin
  if m_Enabled then
  begin
    Enabled := False;
    UnInitMode;
    m_Mode := Value;
    InitMode;
    Enabled := True;
  end
  else
  begin
    UnInitMode;
    m_Mode := Value;
    InitMode;
  end;
end;

procedure TG2Timer.SetEnabled(const Value: Boolean);
begin
  if not Initialized
  or (Value = m_Enabled) then
  Exit;
  m_Enabled := Value;
  if m_Enabled then
  begin
    Start;
  end
  else
  begin
    Stop;
  end;
end;

procedure TG2Timer.ResetTimer;
var
  CurTime: Int64;
begin
  CurTime := GetTime;
  m_FPS := 0;
  m_FrameCount := 0;
  m_FPSUpdateTime := CurTime;
  m_PrevRenderTime := CurTime;
  m_PrevUpdateTime := CurTime;
  if m_MaxFPS = 0 then
  m_MaxRenderLag := 0
  else
  m_MaxRenderLag := m_Frequency div m_MaxFPS;
  if m_TargetUPS = 0 then
  m_MaxUpdateLag := 0
  else
  m_MaxUpdateLag := m_Frequency div m_TargetUPS;
end;

function TG2Timer.GetTime: Int64;
begin
  if m_QPCAvailable then
  QueryPerformanceCounter(Result)
  else
  Result := GetTickCount;
end;

function TG2Timer.GetSpeedFactor: Single;
begin
  if m_TargetUPS > 0 then
  Result := 1000 / m_TargetUPS
  else
  Result := 0;
end;

procedure TG2Timer.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if Assigned(m_PrevIdle) then
  m_PrevIdle(Sender, Done);
  OnTimer;
  Done := False;
end;

procedure TG2Timer.WndProc(var Msg: TMessage);
begin
  with Msg do
  if Msg = WM_TIMER then
  OnTimer
  else
  Result := DefWindowProc(m_WndHandle, Msg, wParam, lParam);
end;

procedure TG2Timer.OnTimer;
var
  i, j: Integer;
  CurTime: Int64;
  CurRenderLag: Int64;
  CurUpdateLag: Int64;
  UpdCount: Integer;
begin
  try
    CurTime := GetTime;

    if m_Enabled then
    for i := 0 to m_Plugs.Count - 1 do
    if Assigned(TG2PlugTimer(m_Plugs.Items[i]).OnTimer) then
    TG2PlugTimer(m_Plugs.Items[i]).OnTimer;

    CurUpdateLag := CurTime - m_PrevUpdateTime;
    if m_Enabled
    and (CurUpdateLag >= m_MaxUpdateLag) then
    begin
      UpdCount := CurUpdateLag div m_MaxUpdateLag;
      m_PrevUpdateTime := m_PrevUpdateTime + UpdCount * m_MaxUpdateLag;
      for j := 0 to UpdCount - 1 do
      begin
        if not m_Enabled then Break;
        for i := 0 to m_Plugs.Count - 1 do
        begin
          if not m_Enabled then Break;
          if Assigned(TG2PlugTimer(m_Plugs.Items[i]).OnUpdate) then
          TG2PlugTimer(m_Plugs.Items[i]).OnUpdate;
        end;
      end;
      m_PrevUpdateTime := CurTime - CurUpdateLag mod m_MaxUpdateLag;
    end;

    CurRenderLag := CurTime - m_PrevRenderTime;
    if m_Enabled
    and (
      (m_MaxFPS = 0)
      or (CurRenderLag >= m_MaxRenderLag)
    ) then
    begin
      for i := 0 to m_Plugs.Count - 1 do
      if Assigned(TG2PlugTimer(m_Plugs.Items[i]).OnRender) then
      TG2PlugTimer(m_Plugs.Items[i]).OnRender;
      Inc(m_FrameCount);
      m_PrevRenderTime := CurTime;
    end;

    if CurTime - m_FPSUpdateTime >= m_Frequency then
    begin
      m_FPS := m_FrameCount;
      m_TotalFPS := m_TotalFPS + m_FPS;
      Inc(m_TotalFPSCount);
      m_FrameCount := 0;
      m_FPSUpdateTime := CurTime;
    end;

    if not m_FullSpeed then Sleep(1);
  except
    on E : Exception do
    begin
      Enabled := False;
      G2WriteLogTimed(AnsiString('(E) ' + E.ClassName + ': ' + E.Message), 'Application');
      try
        ReportMemoryLeaksOnShutdown := False;
        Core.Free;
      finally
        Halt;
      end;
    end;
  end;
end;

function TG2Timer.ThreadStart(const Proc: TG2ProcObj; const OnFinish: TG2ProcObj = nil): TG2Thread;
begin
  Result := TG2Thread.Create(True);
  Result.FreeOnTerminate := True;
  Result.Proc := Proc;
  Result.ProcFinish := OnFinish;
  {$IFDEF VER210}
  Result.Start;
  {$ELSE}
  Result.Resume;
  {$ENDIF}
end;

procedure TG2Timer.ThreadLock;
begin
  m_CS.Enter;
end;

procedure TG2Timer.ThreadUnlock;
begin
  m_CS.Leave;
end;

procedure TG2Timer.PerfCheckStart;
begin
  m_PerfCheckTime := GetTime;
end;

procedure TG2Timer.PerfCheckEnd;
begin
  m_PerfCheckResult := (GetTime - m_PerfCheckTime) / m_Frequency;
end;

function TG2Timer.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  G2WriteLogTimed('Initialization Started.', 'Timer');
  m_Enabled := False;
  InitMode;
  Result := grOk;
  G2WriteLogTimed('Initialization Finished.', 'Timer');
end;

function TG2Timer.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  G2WriteLogTimed('Finalization Started.', 'Timer');
  Enabled := False;
  UnInitMode;
  Result := grOk;
  G2WriteLogTimed('Finalization Finished.', 'Timer');
end;
//TG2Timer END

//TG2Graphics BEGIN
constructor TG2Graphics.Create;
begin
  inherited Create;
  m_PlugClass := TG2PlugGraphics;
  m_D3D9 := Direct3DCreate9(D3D_SDK_VERSION);
  m_D3D9.GetDeviceCaps(0, D3DDEVTYPE_HAL, m_Caps);
  m_InitParams := TG2GraphicsInitParams.Create;
  m_InitParams.m_Gfx := Self;
  m_InitParamsActual := TG2GraphicsInitParams.Create;
  m_InitParamsActual.m_Gfx := Self;
  m_Params := TG2GraphicsRunTimeParams.Create;
  m_Params.m_Gfx := Self;
  m_Specs := TG2GraphicsSpecs.Create(Self);
  m_RenderStates := TG2RenderStates.Create;
  m_RenderStates.m_Gfx := Self;
  m_SamplerStates := TG2SamplerStates.Create;
  m_SamplerStates.m_Gfx := Self;
  m_TextureStageStates := TG2TextureStageStates.Create;
  m_TextureStageStates.m_Gfx := Self;
  m_Transforms := TG2Transforms.Create;
  m_Lights := TG2Lights.Create;
  m_SharedVB2D := TG2SharedVB2D.Create;
  m_Shared := TG2Shared.Create;
  m_SwapChains := TG2List.Create;
  m_DefaultRenderTarget := TG2SurfaceRT.Create;
  m_DefaultDepthStencil := TG2SurfaceDS.Create;
  m_DeviceLost := False;
  m_Rec := False;
  m_RecSurfaceOff := nil;
end;

destructor TG2Graphics.Destroy;
begin
  m_DefaultDepthStencil.Free;
  m_DefaultRenderTarget.Free;
  m_SwapChains.Free;
  m_Shared.Free;
  m_SharedVB2D.Free;
  m_Lights.Free;
  m_Transforms.Free;
  m_TextureStageStates.Free;
  m_SamplerStates.Free;
  m_RenderStates.Free;
  m_Specs.Free;
  m_Params.Free;
  m_InitParamsActual.Free;
  m_InitParams.Free;
  SafeRelease(m_D3D9);
  inherited Destroy;
end;

procedure TG2Graphics.RecCatchFrame;
var
  Screen: ID3DXBuffer;
  Frames: Integer;
begin
  Frames := Round(((GetTickCount - m_RecTimeStart) / 1000) * 30) - m_RecFramesRecorded;
  if Frames > 0 then
  begin
    Screen := ScreenGrab;
    if AVIStreamWrite(
      m_RecAVIStreamC,
      m_RecFramesRecorded,
      1,
      Pointer(Cardinal(Screen.GetBufferPointer) + m_RecBMPFile.bfOffBits),
      m_RecBMPInfo.biSizeImage, 0, nil, nil
    ) = S_OK then
    begin
      Inc(m_RecFramesRecorded, Frames);
    end;
    //m_PrevTimeStamp := GetTickCount;
    SafeRelease(Screen);
  end;
end;

function TG2Graphics.ScreenGrab: ID3DXBuffer;
  var SurfaceOff: IDirect3DSurface9;
  var Surface: IDirect3DSurface9;
  var SurfaceStretch: IDirect3DSurface9;
  var SurfaceDesc: TD3DSurfaceDesc;
begin
  m_Device.GetRenderTarget(0, Surface);
  Surface.GetDesc(SurfaceDesc);
  if m_Rec then
  SurfaceOff := m_RecSurfaceOff
  else
  begin
    m_Device.CreateOffscreenPlainSurface(
      m_RecWidth, m_RecHeight,
      SurfaceDesc.Format,
      D3DPOOL_SYSTEMMEM,
      SurfaceOff,
      nil
    );
  end;
  if (m_RecWidth = SurfaceDesc.Width)
  and (m_RecHeight = SurfaceDesc.Height) then
  begin
    m_Device.GetRenderTargetData(
      Surface, SurfaceOff
    );
  end
  else
  begin
    m_Device.CreateRenderTarget(
      m_RecWidth, m_RecHeight,
      SurfaceDesc.Format,
      SurfaceDesc.MultiSampleType,
      SurfaceDesc.MultiSampleQuality,
      False,
      SurfaceStretch,
      nil
    );
    m_Device.StretchRect(
      Surface, nil,
      SurfaceStretch, nil,
      D3DTEXF_NONE
    );
    m_Device.GetRenderTargetData(
      SurfaceStretch, SurfaceOff
    );
    SafeRelease(SurfaceStretch);
  end;
  D3DXSaveSurfaceToFileInMemory(
    Result,
    D3DXIFF_BMP,
    SurfaceOff,
    nil, nil
  );
  SafeRelease(SurfaceOff);
  SafeRelease(Surface);
end;

function TG2Graphics.InitializeDevice: TG2Result;
var
  BehaviorFlags: DWord;
  BackBufferRT: IDirect3DSurface9;
  R: TRect;
  hr: HResult;
begin
  UpdatePresentParams;

  BehaviorFlags := 0;
  case m_InitParamsActual.VertexProcessing of
    vpSoftware: BehaviorFlags := BehaviorFlags or D3DCREATE_SOFTWARE_VERTEXPROCESSING;
    vpHardware: BehaviorFlags := BehaviorFlags or D3DCREATE_HARDWARE_VERTEXPROCESSING;
    vpMixed: BehaviorFlags := BehaviorFlags or D3DCREATE_MIXED_VERTEXPROCESSING;
  end;

  if m_InitParamsActual.MultiThreaded then
  BehaviorFlags := BehaviorFlags or D3DCREATE_MULTITHREADED;

  if m_InitParamsActual.PureDevice then
  BehaviorFlags := BehaviorFlags or D3DCREATE_PUREDEVICE;

  hr := m_D3D9.CreateDevice(
    m_InitParamsActual.Adapter,
    m_InitParamsActual.DeviceType,
    Core.Handle,
    BehaviorFlags,
    @m_PresentParams,
    m_Device
  );

  if Succeeded(hr) then
  begin
    m_Params.Assign(m_InitParamsActual);

    m_Specs.Initialize;
    m_Device.GetDeviceCaps(m_Caps);
    m_Device.GetViewport(m_DefaultViewPort);
    m_CurDefViewPort := @m_DefaultViewPort;

    m_Device.GetRenderTarget(0, BackBufferRT);
    m_DefaultRenderTarget.Initialize(Core);
    m_DefaultRenderTarget.Surface := BackBufferRT;
    SafeRelease(BackBufferRT);

    m_DefaultDepthStencil.Initialize(Core);
    m_DefaultDepthStencil.CreateDepthStencil(
      m_DefaultViewPort.Width,
      m_DefaultViewPort.Height
    );

    m_CurSurfaceRT := nil;
    m_CurSurfaceDS := nil;
    m_CurSwapChain := nil;

    if not m_Params.FullScreen then
    begin
      m_DefaultSwapChain := SwapChainAdd(
        m_PresentParams.hDeviceWindow,
        m_PresentParams.BackBufferWidth,
        m_PresentParams.BackBufferHeight
      );
      SetRenderTargetSwapChain(m_DefaultSwapChain);
    end
    else
    SetRenderTargetDefault;

    SetDepthStencilDefault;

    PresentToBackBuffer;
    m_RenderStates.SetDefaults;
    m_SamplerStates.SetDefaults;
    m_TextureStageStates.SetDefaults;
    m_Transforms.Initialize(Core);
    m_Lights.Initialize(Core);
    m_SharedVB2D.Initialize(Self);
    m_Shared.Initialize(Core);
    SetMaterialDefault;
    Core.RequestMod(TG2ShaderLib, @m_ShaderLib);

    GetWindowRect(Core.Handle, R);
    m_WndPosX := R.Left;
    m_WndPosY := R.Top;
    m_WndWidth := R.Right - R.Left;
    m_WndHeight := R.Bottom - R.Top;

    Result := grOk;
  end
  else
  begin
    G2WriteLogTimed('(E) InitializeDevice Failed', 'Graphics');
    Result := grFail;
  end;
end;

function TG2Graphics.FinalizeDevice: TG2Result;
begin
  Core.ReleaseMod(@m_ShaderLib);
  m_DefaultDepthStencil.Finalize;
  m_DefaultRenderTarget.Finalize;
  m_Shared.Finalize;
  m_SharedVB2D.Finalize;
  m_Lights.Finalize;
  m_Transforms.Finalize;
  SwapChainRemove(m_DefaultSwapChain);
  SafeRelease(m_Device);
  Result := grOk;
end;

function TG2Graphics.LostDevice: TG2Result;
var
  i: Integer;
  R: TRect;
begin
  if m_DeviceLost then
  begin
    Result := grFail;
    Exit;
  end;

  if not m_InitParamsActual.FullScreen then
  begin
    GetWindowRect(Core.Handle, R);
    m_WndPosX := R.Left;
    m_WndPosY := R.Top;
    m_WndWidth := R.Right - R.Left;
    m_WndHeight := R.Bottom - R.Top;
  end;

  m_DefaultRenderTarget.Release;
  m_DefaultDepthStencil.Release;

  m_SharedVB2D.OnDeviceLost;
  m_Transforms.OnDeviceLost;
  m_Lights.OnDeviceLost;

  for i := 0 to m_SwapChains.Count - 1 do
  TG2SwapChain(m_SwapChains[i]).OnLostDevice;

  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugGraphics(m_Plugs[i]).OnDeviceLost) then
  TG2PlugGraphics(m_Plugs[i]).OnDeviceLost;
  m_DeviceLost := True;
  Result := grOk;
  G2WriteLogTimed('(W) Device Lost.', 'Graphics');
end;

function TG2Graphics.ResetDevice: TG2Result;
var
  i: Integer;
  hr: HResult;
  BackBufferRT: IDirect3DSurface9;
begin
  if not m_DeviceLost then LostDevice;
  m_InitParams.Clone(m_InitParamsActual);
  if G2ResFail(m_InitParamsActual.Verify) then
  begin
    Result := grFail;
    Exit;
  end;

  UpdatePresentParams;

  hr := m_Device.Reset(m_PresentParams);
  if Failed(hr) then
  begin
    Result := grFail;
    Exit;
  end;

  m_Device.GetViewport(m_DefaultViewPort);

  m_Device.GetRenderTarget(0, BackBufferRT);
  m_DefaultRenderTarget.Surface := BackBufferRT;

  m_DefaultDepthStencil.CreateDepthStencil(
    m_DefaultViewPort.Width,
    m_DefaultViewPort.Height
  );
  G2WriteLog('all ok until if fullscreen');
  if not m_Params.FullScreen then
  begin
    G2WriteLog('enter fullscreen');
    for i := 0 to m_SwapChains.Count - 1 do
    TG2SwapChain(m_SwapChains[i]).OnResetDevice;
    if Assigned(m_DefaultSwapChain) then
    m_DefaultSwapChain.Reset(m_DefaultViewPort.Width, m_DefaultViewPort.Height)
    else
    m_DefaultSwapChain := SwapChainAdd(
      m_PresentParams.hDeviceWindow,
      m_PresentParams.BackBufferWidth,
      m_PresentParams.BackBufferHeight
    );
    if Assigned(m_CurSwapChain) then
    begin
      PresentToSwapChain(m_CurSwapChain);
      SetRenderTargetSurface(m_CurSurfaceRT);
    end
    else
    begin
      PresentToBackBuffer;
      SetRenderTargetSwapChain(m_DefaultSwapChain);
    end;
    SetWindowPos(Core.Handle, GetParent(Core.Handle), m_WndPosX, m_WndPosY, m_WndWidth, m_WndHeight, 0);
  end
  else
  begin
    m_CurSwapChain := nil;
    SetRenderTargetDefault;
  end;

  SetDepthStencilSurface(m_CurSurfaceDS);

  m_TextureStageStates.SetFromMemory;
  m_SamplerStates.SetFromMemory;
  m_RenderStates.SetFromMemory;
  m_Transforms.OnDeviceReset;
  m_Lights.OnDeviceReset;
  m_SharedVB2D.OnDeviceReset;
  SetMaterialFromMemory;

  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugGraphics(m_Plugs[i]).OnDeviceReset) then
  TG2PlugGraphics(m_Plugs[i]).OnDeviceReset;

  m_DeviceLost := False;
  Result := grOk;
  G2WriteLogTimed('Device Reset.', 'Graphics');
end;

procedure TG2Graphics.UpdatePresentParams;
begin
  ZeroMemory(@m_PresentParams, SizeOf(m_PresentParams));
  m_PresentParams.BackBufferWidth := m_InitParamsActual.Width;
  m_PresentParams.BackBufferHeight := m_InitParamsActual.Height;
  m_PresentParams.BackBufferFormat := m_InitParamsActual.FormatBackBuffer;
  m_PresentParams.BackBufferCount := 1;
  m_PresentParams.MultiSampleQuality := 0;
  m_PresentParams.SwapEffect := D3DSWAPEFFECT_DISCARD;
  if m_InitParamsActual.Antialiasing then
  m_PresentParams.MultiSampleType := TD3DMultiSampleType(m_InitParamsActual.AntialiasingSampleCount)
  else
  m_PresentParams.MultiSampleType := D3DMULTISAMPLE_NONE;
  m_PresentParams.hDeviceWindow := Core.Handle;
  m_PresentParams.Windowed := not m_InitParamsActual.FullScreen;
  m_PresentParams.EnableAutoDepthStencil := False;
  if m_InitParamsActual.VSync then
  m_PresentParams.PresentationInterval := D3DPRESENT_INTERVAL_ONE
  else
  m_PresentParams.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
  m_PresentParams.Flags := 0;
end;

procedure TG2Graphics.Update;
var
  hr: HResult;
begin
  if not m_Initialized then Exit;
  hr := m_Device.TestCooperativeLevel;
  if Failed(hr) then
  begin
    case hr of
      D3DERR_DEVICELOST:
      begin
        LostDevice;
      end;
      D3DERR_DEVICENOTRESET:
      begin
        ResetDevice;
      end;
    end;
  end
  else
  begin
    if m_Rec then
    RecCatchFrame;
  end;
end;

function TG2Graphics.GetCanRender: Boolean;
begin
  Result := not m_DeviceLost;
end;

procedure TG2Graphics.SetGamma(const Value: Integer);
var
  DC: HDC;
  ramp: array [0..256 * 3 - 1] of Word;
  i: Integer;
  r: Double;
  g: Double;
begin
  g := (100 - gamma) * (2.7 - 0.23) / 100 + 0.23;
  for i := 0 to 255 do
  begin
    r := Power(i / 256, g) * 65536;
    if r < 0 then
    r := 0
    else
    if r > 65535 then
    r := 65535;
    ramp[i] := Trunc(r);
    ramp[i + 256] := Trunc(r);
    ramp[i + 512] := Trunc(r);
  end;
  DC := GetDC(Core.Handle);
  SetDeviceGammaRamp(DC, ramp);
  ReleaseDC(Core.Handle, DC);
  m_RestoreGamma := True;
end;

function TG2Graphics.GetGamma: Integer;
var
  DC: HDC;
  ramp: array [0..256 * 3 - 1] of Word;
  rgb: array [0..2] of double;
  sum: double;
  count: integer;
  min: integer;
  max: integer;
  A, B: double;
  i, j: integer;
begin
  if not m_Initialized then
  begin
    Result := -1;
    Exit;
  end;
  rgb[0] := 1;
  rgb[1] := 1;
  rgb[2] := 1;
  DC := GetDC(Core.Handle);
  GetDeviceGammaRamp(DC, ramp);
  ReleaseDC(Core.Handle, DC);
  for i := 0 to 2 do
  begin
    sum := 0;
    count := 0;
    min := 256 * i;
    max := min + 256;
    for j := min to max - 1 do
    if ramp[j] > 0 then
    begin
      B := (j mod 256) / 256;
      A := ramp[j] / 65536;
      sum := sum + ln(A) / ln(B);
      Inc(count);
    end;
    rgb[i] := sum / count;
  end;
  Result := 100 - Trunc(((rgb[0] + rgb[1] + rgb[2]) / 3 - 0.23) * 100 / (2.7 - 0.23));
end;

procedure TG2Graphics.HardReset;
begin
  ResetDevice;
end;

procedure TG2Graphics.QuickReset;
begin
  m_InitParams.Clone(m_InitParamsActual);
  if G2ResFail(m_InitParamsActual.Verify) then Exit;
  m_PresentParams.BackBufferWidth := m_InitParamsActual.Width;
  m_PresentParams.BackBufferHeight := m_InitParamsActual.Height;
  m_DefaultViewPort.X := 0;
  m_DefaultViewPort.Y := 0;
  m_DefaultViewPort.Width := m_InitParamsActual.Width;
  m_DefaultViewPort.Height := m_InitParamsActual.Height;
  m_DefaultSwapChain.Reset(
    m_InitParamsActual.Width,
    m_InitParamsActual.Height
  );
  SetRenderTargetSwapChain(m_DefaultSwapChain);
  if (m_PresentParams.BackBufferWidth > m_DefaultDepthStencil.Width)
  or (m_PresentParams.BackBufferHeight > m_DefaultDepthStencil.Height) then
  begin
    m_DefaultDepthStencil.CreateDepthStencil(
      m_PresentParams.BackBufferWidth,
      m_PresentParams.BackBufferHeight
    );
    SetDepthStencilSurface(m_CurSurfaceDS);
  end;
end;

procedure TG2Graphics.ParamsChange;
var
  i: Integer;
begin
  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugGraphics(m_Plugs[i]).OnParamsChange) then
  TG2PlugGraphics(m_Plugs[i]).OnParamsChange;
end;

function TG2Graphics.SwapChainAdd(const Handle: HWND; const SurfaceWidth, SurfaceHeight: Word): TG2SwapChain;
begin
  Result := TG2SwapChain.Create;
  Result.m_Gfx := Self;
  Result.Initialize(Handle, SurfaceWidth, SurfaceHeight, m_PresentParams);
  m_SwapChains.Add(Result);
end;

procedure TG2Graphics.SwapChainRemove(const SwapChain: TG2SwapChain);
begin
  m_SwapChains.Remove(SwapChain);
  SwapChain.Free;
end;

procedure TG2Graphics.SetViewPortDefault;
begin
  m_Device.SetViewport(m_CurDefViewPort^);
end;

procedure TG2Graphics.SetViewPort(const X, Y, Width, Height: Integer);
var
  TmpViewPort: TD3DViewPort9;
begin
  TmpViewPort := m_CurDefViewPort^;
  TmpViewPort.X := X;
  TmpViewPort.Y := Y;
  TmpViewPort.Width := Width;
  TmpViewPort.Height := Height;
  m_Device.SetViewport(TmpViewPort);
end;

procedure TG2Graphics.SetViewPort(const ViewPort: TD3DViewPort9);
begin
  m_Device.SetViewport(ViewPort);
end;

function TG2Graphics.GetViewPort: TD3DViewPort9;
begin
  m_Device.GetViewport(Result);
end;

procedure TG2Graphics.SetMaterialDefault;
begin
  SetMaterial($ffffffff, $ffffffff, $00000000, $00000000, 0);
end;

procedure TG2Graphics.SetMaterialFromMemory;
begin
  Device.SetMaterial(m_Material);
end;

procedure TG2Graphics.SetMaterial(
      const Diffuse: TG2Color;
      const Ambient: TG2Color;
      const Specular: TG2Color;
      const Emissive: TG2Color;
      const SpecPower: Single
    );
begin
  m_Material.Diffuse := Diffuse;
  m_Material.Ambient := Ambient;
  m_Material.Specular := Specular;
  m_Material.Emissive := Emissive;
  m_Material.Power := SpecPower;
  Device.SetMaterial(m_Material);
end;

procedure TG2Graphics.SetRenderTargetSurface(const Surface: TG2SurfaceRT);
begin
  m_Device.SetRenderTarget(0, Surface.Surface);
  m_CurSurfaceRT := Surface;
end;

procedure TG2Graphics.SetRenderTargetTexture2D(const Texture: TG2Texture2DRT);
begin
  SetRenderTargetSurface(Texture.SurfaceRT);
end;

procedure TG2Graphics.SetRenderTargetSwapChain(const SwapChain: TG2SwapChain);
begin
  SetRenderTargetSurface(SwapChain.RenderTarget);
end;

procedure TG2Graphics.SetDepthStencilSurface(const Surface: TG2SurfaceDS);
begin
  m_Device.SetDepthStencilSurface(Surface.Surface);
  m_CurSurfaceDS := Surface;
end;

procedure TG2Graphics.SetDepthStencilTexture2D(const Texture: TG2Texture2DDS);
begin
  SetDepthStencilSurface(Texture.DepthStencil);
end;

procedure TG2Graphics.SetRenderTargetDefault;
begin
  if m_CurSwapChain <> nil then
  SetRenderTargetSwapChain(m_DefaultSwapChain)
  else
  SetRenderTargetSurface(m_DefaultRenderTarget);
end;

procedure TG2Graphics.SetDepthStencilDefault;
begin
  SetDepthStencilSurface(m_DefaultDepthStencil);
end;

procedure TG2Graphics.PresentToBackBuffer;
begin
  m_CurSwapChain := m_DefaultSwapChain;
end;

procedure TG2Graphics.PresentToSwapChain(const SwapChain: TG2SwapChain);
begin
  m_CurSwapChain := SwapChain;
end;

procedure TG2Graphics.SaveScreenshot(const FileName: WideString);
  var Surface: IDirect3DSurface9;
  var SurfaceOff: IDirect3DSurface9;
  var SurfaceDesc: TD3DSurfaceDesc;
  var fmt: TD3DXImageFileFormat;
  var ext: String;
begin
  ext := UpperCase(ExtractFileExt(FileName));
  Delete(ext, 1, 1);
  if ext = 'BMP' then fmt := D3DXIFF_BMP
  else if (ext = 'JPG') or (ext = 'JPEG') then fmt := D3DXIFF_JPG
  else if ext = 'TGA' then fmt := D3DXIFF_TGA
  else if ext = 'PNG' then fmt := D3DXIFF_PNG
  else if ext = 'PPM' then fmt := D3DXIFF_PPM
  else if ext = 'DIB' then fmt := D3DXIFF_DIB
  else if ext = 'HDR' then fmt := D3DXIFF_HDR
  else if ext = 'PFM' then fmt := D3DXIFF_PFM
  else fmt := D3DXIFF_DDS;
  m_Device.GetRenderTarget(0, Surface);
  Surface.GetDesc(SurfaceDesc);
  m_Device.CreateOffscreenPlainSurface(
    SurfaceDesc.Width, SurfaceDesc.Height,
    SurfaceDesc.Format,
    D3DPOOL_SYSTEMMEM,
    SurfaceOff,
    nil
  );
  m_Device.GetRenderTargetData(
    Surface, SurfaceOff
  );
  SafeRelease(Surface);
  D3DXSaveSurfaceToFileW(
    PWideChar(FileName),
    fmt,
    SurfaceOff,
    nil, nil
  );
  SafeRelease(SurfaceOff);
end;

procedure TG2Graphics.RecStart(const FileName: WideString; const RecWidth: Integer = 0; const RecHeight: Integer = 0; const Quality: Integer = 6000);
  var Screen: ID3DXBuffer;
  var CompressOptions: TAVICompressOptions;
  var StreamInfo: TAVIStreamInfoA;
  var Surface: IDirect3DSurface9;
  var SurfaceDesc: TD3DSurfaceDesc;
begin
  if m_Rec then Exit;
  if (RecWidth <= 0) or (RecWidth > m_Params.Width) then m_RecWidth := m_Params.Width else m_RecWidth := RecWidth;
  if (RecHeight <= 0) or (RecHeight > m_Params.Height) then m_RecHeight := m_Params.Height else m_RecHeight := RecHeight;
  ZeroMemory(@CompressOptions, SizeOf(TAVICompressOptions));
  CompressOptions.fccType := streamtypeVIDEO;
  CompressOptions.fccHandler := MMIOStringToFOURCCA('msvc', 0);
  CompressOptions.dwQuality := Quality;
  CompressOptions.dwKeyFrameEvery := 0;
  CompressOptions.dwFlags := 0;
  DeleteFile(FileName);
  if AVIFileOpenW(m_RecAVIFile, PWideChar(FileName), OF_WRITE or OF_CREATE, nil) = S_OK then
  begin
    Screen := ScreenGrab;
    Move(Screen.GetBufferPointer^, m_RecBMPFile, SizeOf(TBitmapFileHeader));
    Move(Pointer(cardinal(Screen.GetBufferPointer) + SizeOf(TBitmapFileHeader))^, m_RecBMPInfo, SizeOf(TBitmapInfoHeader));
    SafeRelease(Screen);
    ZeroMemory(@StreamInfo, SizeOf(TAVIStreamInfoA));
    StreamInfo.fccType := streamtypeVIDEO;
    StreamInfo.fccHandler := 0;
    StreamInfo.dwScale := 1;
    StreamInfo.dwRate := 30;
    StreamInfo.dwSuggestedBufferSize := m_RecBMPInfo.biSizeImage;
    SetRect(StreamInfo.rcFrame, 0, 0, m_RecBMPInfo.biWidth, m_RecBMPInfo.biHeight);
    if AVIFileCreateStream(m_RecAVIFile, m_RecAVIStream, @StreamInfo) = S_OK then
    begin
      if AVIMakeCompressedStream(m_RecAVIStreamC, m_RecAVIStream, @CompressOptions, nil) = S_OK then
      begin
        if AVIStreamSetFormat(m_RecAVIStreamC, 0, @m_RecBMPInfo, SizeOf(m_RecBMPInfo)) = S_OK then
        begin
          m_Rec := True;
          m_RecFramesRecorded := 0;
          m_RecTimeStart := GetTickCount;
          m_RecTimerInterval := 1000 div (StreamInfo.dwRate);
          m_Device.GetRenderTarget(0, Surface);
          Surface.GetDesc(SurfaceDesc);
          SafeRelease(Surface);
          m_Device.CreateOffscreenPlainSurface(
            m_RecWidth, m_RecHeight,
            SurfaceDesc.Format,
            D3DPOOL_SYSTEMMEM,
            m_RecSurfaceOff,
            nil
          );
          RecCatchFrame;
        end
        else
        begin
          AVIStreamRelease(m_RecAVIStreamC);
          AVIStreamRelease(m_RecAVIStream);
          AVIFileRelease(m_RecAVIFile);
        end;
      end
      else
      begin
        AVIStreamRelease(m_RecAVIStream);
        AVIFileRelease(m_RecAVIFile);
      end;
    end
    else
    begin
      AVIFileRelease(m_RecAVIFile);
    end;
  end;
end;

procedure TG2Graphics.RecStop;
begin
  if m_Rec then
  begin
    SafeRelease(m_RecSurfaceOff);
    AVIStreamRelease(m_RecAVIStream);
    AVIStreamRelease(m_RecAVIStreamC);
    AVIFileRelease(m_RecAVIFile);
    m_Rec := False;
  end;
end;

function TG2Graphics.PointToRay(const Pt: TPoint): TG2Ray;
  var VPi: TG2Mat;
  var ViewPort: TD3DViewPort9;
begin
  m_Device.GetViewport(ViewPort);
  Result.Origin.x := (Pt.x / ViewPort.Width) * 2 - 1;
  Result.Origin.y := -((Pt.y / ViewPort.Height) * 2 - 1);
  Result.Origin.z := 0;
  Result.Dir.x := Result.Origin.x;
  Result.Dir.y := Result.Origin.y;
  Result.Dir.z := 1;
  VPi := m_Transforms.VPi;
  Result.Origin := Result.Origin.Transform4x4(VPi);
  Result.Dir := (Result.Dir.Transform4x4(VPi) - Result.Origin).Normalized;
end;

function TG2Graphics.PointTo3D(const Pt: TG2Vec3): TG2Vec3;
begin
  D3DXVec3Unproject(
    PG2Vec3Ref(@Result)^,
    PG2Vec3Ref(@Pt)^,
    GetViewPort,
    Transforms.P,
    Transforms.V,
    Transforms.W[0]
  );
end;

function TG2Graphics.PointTo2D(const Pt: TG2Vec3): TG2Vec3;
begin
  D3DXVec3Project(
    PG2Vec3Ref(@Result)^,
    PG2Vec3Ref(@Pt)^,
    GetViewPort,
    Transforms.P,
    Transforms.V,
    Transforms.W[0]
  );
end;

function TG2Graphics.Initialize(const G2Core: TG2Core): TG2Result;
{$IFDEF G2_WRITE_LOG}
var
  Caps: TD3DCaps9;
  AdapterIdentifier: TD3DAdapterIdentifier9;
  DisplayMode: TD3DDisplayMode;
  ParamsString: AnsiString;
const
  Sp = '                            ';
{$ENDIF}
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  G2WriteLogTimed('Initialization Started.', 'Graphics');
  m_InitParams.Clone(m_InitParamsActual);
  Result := m_InitParamsActual.Verify;
  if G2ResFail(Result) then
  begin
    G2WriteLogTimed('(E) Initialization Failed: Unacceptable InitParams.', 'Graphics');
    m_Initialized := False;
    Exit;
  end;
  {$IFDEF G2_WRITE_LOG}
  m_D3D9.GetDeviceCaps(m_InitParamsActual.Adapter, m_InitParamsActual.DeviceType, Caps);
  m_D3D9.GetAdapterIdentifier(m_InitParamsActual.Adapter, 0, AdapterIdentifier);
  m_D3D9.GetAdapterDisplayMode(m_InitParamsActual.Adapter, DisplayMode);
  G2WriteLogTimed(AnsiString('Adapter: ' + AdapterIdentifier.Description), 'Graphics');
  G2WriteLogTimed(AnsiString('Adapter Driver: ' + AdapterIdentifier.Driver), 'Graphics');
  G2WriteLogTimed(
    AnsiString('System Display Mode: ' + IntToStr(DisplayMode.Width) + 'x' + IntToStr(DisplayMode.Height) + ' ' + IntToStr(DisplayMode.RefreshRate) + 'Hz'),
    'Graphics'
  );
  ParamsString := 'Initialization Parameters:'#$D#$A#$D#$A;
  if m_InitParamsActual.FullScreen then
  ParamsString := ParamsString + Sp + 'FullScreen '
  else
  ParamsString := ParamsString + Sp + 'Windowed ';
  ParamsString := ParamsString + AnsiString(IntToStr(m_InitParamsActual.Width) + 'x' + IntToStr(m_InitParamsActual.Height)) + #$D#$A;
  if m_InitParamsActual.Antialiasing then
  ParamsString := ParamsString + Sp + AnsiString('Antialiasing: ' + IntToStr(m_InitParamsActual.AntialiasingSampleCount) + 'x') + #$D#$A;
  ParamsString := ParamsString + Sp + 'Back Buffer Format: ' + G2FormatToString(m_InitParamsActual.FormatBackBuffer) + #$D#$A;
  ParamsString := ParamsString + Sp + 'Render Target Surface Format: ' + G2FormatToString(m_InitParamsActual.FormatSurfaceRT) + #$D#$A;
  ParamsString := ParamsString + Sp + 'Depth-Stencil Surface Format: ' + G2FormatToString(m_InitParamsActual.FormatSurfaceDS) + #$D#$A;
  ParamsString := ParamsString + Sp + 'Texture2D Format: ' + G2FormatToString(m_InitParamsActual.FormatTexture2D) + #$D#$A;
  ParamsString := ParamsString + Sp + 'Render Target Texture2D Format: ' + G2FormatToString(m_InitParamsActual.FormatTexture2DRT) + #$D#$A;
  ParamsString := ParamsString + Sp + 'Depth-Stencil Texture2D Format: ' + G2FormatToString(m_InitParamsActual.FormatTexture2DDS) + #$D#$A;
  ParamsString := ParamsString + Sp + 'TextureCube Format: ' + G2FormatToString(m_InitParamsActual.FormatTextureCube) + #$D#$A;
  ParamsString := ParamsString + Sp + 'Render Target TextureCube Format: ' + G2FormatToString(m_InitParamsActual.FormatTextureCubeRT) + #$D#$A;
  ParamsString := ParamsString + Sp + 'TextureVolume Format: ' + G2FormatToString(m_InitParamsActual.FormatTextureVolume) + #$D#$A;
  ParamsString := ParamsString + Sp + 'Vertex Processing: ';
  case m_InitParamsActual.VertexProcessing of
    vpMixed: ParamsString := ParamsString + 'Mixed'#$D#$A;
    vpHardware: ParamsString := ParamsString + 'Hardware'#$D#$A;
    else
    ParamsString := ParamsString + 'Software'#$D#$A;
  end;
  ParamsString := ParamsString + Sp + 'Device Settings: ';
  if m_InitParamsActual.MultiThreaded then
  ParamsString := ParamsString + '[Multithreaded]';
  if m_InitParamsActual.PureDevice then
  ParamsString := ParamsString + '[Pure Device]';
  if m_InitParamsActual.VSync then
  ParamsString := ParamsString + '[VSync]';
  ParamsString := ParamsString + #$D#$A + Sp + 'Device Capabilities:'#$D#$A;
  ParamsString := ParamsString + Sp + AnsiString('Max Texture Size: ' + IntToStr(Caps.MaxTextureWidth) + 'x' + IntToStr(Caps.MaxTextureHeight)) + #$D#$A;
  ParamsString := ParamsString + Sp + AnsiString('Max Texture Blend Stages: ' + IntToStr(Caps.MaxTextureBlendStages)) + #$D#$A;
  ParamsString := ParamsString + Sp + AnsiString('Max Simultanious Textures: ' + IntToStr(Caps.MaxSimultaneousTextures)) + #$D#$A;
  ParamsString := ParamsString + Sp + AnsiString('VertexShader Version: ' + IntToStr(D3DSHADER_VERSION_MAJOR(Caps.VertexShaderVersion)) + '.' + IntToStr(D3DSHADER_VERSION_MINOR(Caps.VertexShaderVersion))) + #$D#$A;
  ParamsString := ParamsString + Sp + AnsiString('PixelShader Version: ' + IntToStr(D3DSHADER_VERSION_MAJOR(Caps.PixelShaderVersion)) + '.' + IntToStr(D3DSHADER_VERSION_MINOR(Caps.PixelShaderVersion))) + #$D#$A;
  G2WriteLogTimed(ParamsString, 'Graphics');
  {$ENDIF}
  Result := InitializeDevice;
  if G2ResFail(Result) then
  begin
    G2WriteLogTimed('Initialization Failed.', 'Graphics');
    m_Initialized := False;
    Exit;
  end;
  m_InitGamma := Gamma;
  m_RestoreGamma := False;
  Core.RequestPlug(TG2PlugTimer, @m_PlugTimer);
  m_PlugTimer.OnTimer := Update;
  G2WriteLogTimed('Initialization Finished.', 'Graphics');
end;

function TG2Graphics.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  G2WriteLogTimed('Finalization Started.', 'Graphics');
  if m_Rec then RecStop;
  if m_RestoreGamma
  and (m_InitGamma >= 0)
  and (Gamma <> m_InitGamma) then
  Gamma := m_InitGamma;
  Core.ReleasePlug(@m_PlugTimer);
  Result := FinalizeDevice;
  G2WriteLogTimed('Finalization Finished.', 'Graphics');
end;
//TG2Graphics END

//TG2GraphicsSpecs BEGIN
constructor TG2GraphicsSpecs.Create;
  var i: Integer;
begin
  inherited Create;
  m_List := TG2SortedList.Create;
  m_Gfx := G2Graphics;
  FindResolutions;
  m_Antialias := nil;
  for i := 2 to 16 do
  begin
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceMultiSampleType(
        0,
        D3DDEVTYPE_HAL,
        D3DFMT_X8R8G8B8,
        True,
        TD3DMultiSampleType(i),
        nil
      )
    ) then
    begin
      SetLength(m_Antialias, Length(m_Antialias) + 1);
      m_Antialias[High(m_Antialias)] := i;
    end;
  end;
end;

destructor TG2GraphicsSpecs.Destroy;
begin
  m_List.Free;
  inherited Destroy;
end;

function TG2GraphicsSpecs.FindFormat(
      const Format: TD3DFormat;
      const Usage: DWord;
      const ResType: TD3DResourceType;
      const FormatArray: PFormatScoreArray;
      const FormatCount: Integer;
      const InitScore: Integer;
      const DefaultFormat: TD3DFormat
    ): TD3DFormat;
var
  i: Integer;
  Score: DWord;
begin
  if Succeeded(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Gfx.m_InitParamsActual.Adapter,
      m_Gfx.m_InitParamsActual.DeviceType,
      m_DisplayMode.Format,
      Usage,
      ResType,
      Format
    )
  ) then
  begin
    Result := Format;
    Exit;
  end;
  Result := DefaultFormat;
  Score := InitScore;
  for i := 0 to FormatCount - 1 do
  if FormatArray^[i].Format = Format then
  begin
    Score := FormatArray^[i].Score;
    Break;
  end;
  for i := 0 to FormatCount - 1 do
  if FormatArray^[i].Format <> Format then
  m_List.Add(@FormatArray^[i].Format, FormatArray^[i].Score - Score);
  for i := 0 to m_List.Count - 1 do
  if Succeeded(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Gfx.m_InitParamsActual.Adapter,
      m_Gfx.m_InitParamsActual.DeviceType,
      m_DisplayMode.Format,
      Usage,
      ResType,
      PD3DFormat(m_List.Items[i])^
    )
  ) then
  begin
    Result := PD3DFormat(m_List.Items[i])^;
    Break;
  end;
  m_List.Clear;
end;

procedure TG2GraphicsSpecs.FindResolutions;
  var ac, i, j, t, Pos, sw, sh, sax, say, ax, ay: Integer;
  var Mode: TD3DDisplayMode;
  var r: PResolution;
  var AddMode: Boolean;
begin
  sw := GetSystemMetrics(SM_CXSCREEN);
  sh := GetSystemMetrics(SM_CYSCREEN);
  FindAsp(sw, sh, sax, say);
  ac := m_Gfx.D3D9.GetAdapterModeCount(0, D3DFMT_X8R8G8B8);
  for i := 0 to ac - 1 do
  begin
    m_Gfx.D3D9.EnumAdapterModes(0, D3DFMT_X8R8G8B8, i, Mode);
    AddMode := True;
    for j := 0 to m_List.Count - 1 do
    if (PResolution(m_List[j])^.Width = Mode.Width)
    and ((PResolution(m_List[j])^.Height = Mode.Height)) then
    begin
      Pos := 0;
      for t := 0 to High(PResolution(m_List[j])^.RefreshRates) do
      begin
        if PResolution(m_List[j])^.RefreshRates[t] = Mode.RefreshRate then
        begin
          AddMode := False;
          Break;
        end
        else
        if PResolution(m_List[j])^.RefreshRates[t] < Mode.RefreshRate then
        Pos := t + 1;
      end;
      if AddMode then
      begin
        SetLength(PResolution(m_List[j])^.RefreshRates, Length(PResolution(m_List[j])^.RefreshRates) + 1);
        for t := Pos to High(PResolution(m_List[j])^.RefreshRates) - 1 do
        PResolution(m_List[j])^.RefreshRates[t + 1] := PResolution(m_List[j])^.RefreshRates[t];
        PResolution(m_List[j])^.RefreshRates[Pos] := Mode.RefreshRate;
      end;
      AddMode := False;
      Break;
    end;
    if AddMode then
    begin
      FindAsp(Mode.Width, Mode.Height, ax, ay);
      New(r);
      r^.AspX := ax;
      r^.AspY := ay;
      r^.Width := Mode.Width;
      r^.Height := Mode.Height;
      SetLength(r^.RefreshRates, 1);
      r^.RefreshRates[0] := Mode.RefreshRate;
      m_List.Add(r, Mode.Width * Mode.Height);
    end;
  end;
  SetLength(m_Resolutions, m_List.Count);
  for i := 0 to m_List.Count - 1 do
  begin
    m_Resolutions[i] := PResolution(m_List[i])^;
    Dispose(PResolution(m_List[i]));
  end;
  m_List.Clear;
  for i := 0 to ac - 1 do
  begin
    m_Gfx.D3D9.EnumAdapterModes(0, D3DFMT_X8R8G8B8, i, Mode);
    FindAsp(Mode.Width, Mode.Height, ax, ay);
    if (ax = sax) and (ay = say) then
    begin
      AddMode := True;
      for j := 0 to m_List.Count - 1 do
      if (PResolution(m_List[j])^.Width = Mode.Width)
      and ((PResolution(m_List[j])^.Height = Mode.Height)) then
      begin
        Pos := 0;
        for t := 0 to High(PResolution(m_List[j])^.RefreshRates) do
        begin
          if PResolution(m_List[j])^.RefreshRates[t] = Mode.RefreshRate then
          begin
            AddMode := False;
            Break;
          end
          else
          if PResolution(m_List[j])^.RefreshRates[t] < Mode.RefreshRate then
          Pos := t + 1;
        end;
        if AddMode then
        begin
          SetLength(PResolution(m_List[j])^.RefreshRates, Length(PResolution(m_List[j])^.RefreshRates) + 1);
          for t := Pos to High(PResolution(m_List[j])^.RefreshRates) - 1 do
          PResolution(m_List[j])^.RefreshRates[t + 1] := PResolution(m_List[j])^.RefreshRates[t];
          PResolution(m_List[j])^.RefreshRates[Pos] := Mode.RefreshRate;
        end;
        AddMode := False;
        Break;
      end;
      if AddMode then
      begin
        New(r);
        r^.AspX := ax;
        r^.AspY := ay;
        r^.Width := Mode.Width;
        r^.Height := Mode.Height;
        SetLength(r^.RefreshRates, 1);
        r^.RefreshRates[0] := Mode.RefreshRate;
        m_List.Add(r, Mode.Width * Mode.Height);
      end;
    end;
  end;
  SetLength(m_ResolutionsComp, m_List.Count);
  for i := 0 to m_List.Count - 1 do
  begin
    m_ResolutionsComp[i] := PResolution(m_List[i])^;
    Dispose(PResolution(m_List[i]));
  end;
  m_List.Clear;
end;

function TG2GraphicsSpecs.GetResolution(const Index: Integer): PResolution;
begin
  Result := @m_Resolutions[Index];
end;

function TG2GraphicsSpecs.GetResolutionCount: Integer;
begin
  Result := Length(m_Resolutions);
end;

function TG2GraphicsSpecs.GetResolutionComp(const Index: Integer): PResolution;
begin
  Result := @m_ResolutionsComp[Index];
end;

function TG2GraphicsSpecs.GetResolutionCompCount: Integer;
begin
  Result := Length(m_ResolutionsComp);
end;

function TG2GraphicsSpecs.GetAntialiasSamples(const Index: Integer): Integer;
begin
  Result := m_Antialias[Index];
end;

function TG2GraphicsSpecs.GetAntialiasCount: Integer;
begin
  Result := Length(m_Antialias);
end;

procedure TG2GraphicsSpecs.Initialize;
begin
  m_Gfx.D3D9.GetAdapterDisplayMode(
    m_Gfx.m_InitParamsActual.Adapter,
    m_DisplayMode
  );
end;

function TG2GraphicsSpecs.FindCompatiableTexture2DFormat(const Format: TD3DFormat): TD3DFormat;
begin
  Result := FindFormat(
    Format,
    0,
    D3DRTYPE_TEXTURE,
    @FormatScoresTexture,
    Length(FormatScoresTexture),
    59,
    m_Gfx.m_InitParamsActual.FormatTexture2D
  );
end;

function TG2GraphicsSpecs.FindCompatiableTexture2DRTFormat(const Format: TD3DFormat): TD3DFormat;
begin
  Result := FindFormat(
    Format,
    D3DUSAGE_RENDERTARGET,
    D3DRTYPE_TEXTURE,
    @FormatScoresRenderTarget,
    Length(FormatScoresRenderTarget),
    50,
    m_Gfx.m_InitParamsActual.FormatTexture2DRT
  );
end;

function TG2GraphicsSpecs.FindCompatiableTexture2DDSFormat(const Format: TD3DFormat): TD3DFormat;
begin
  Result := FindFormat(
    Format,
    D3DUSAGE_DEPTHSTENCIL,
    D3DRTYPE_TEXTURE,
    @FormatScoresDepthStencil,
    Length(FormatScoresDepthStencil),
    34,
    m_Gfx.m_InitParamsActual.FormatTexture2DDS
  );
end;

function TG2GraphicsSpecs.FindCompatiableTextureCubeFormat(const Format: TD3DFormat): TD3DFormat;
begin
  Result := FindFormat(
    Format,
    0,
    D3DRTYPE_CUBETEXTURE,
    @FormatScoresTexture,
    Length(FormatScoresTexture),
    59,
    m_Gfx.m_InitParamsActual.FormatTextureCube
  );
end;

function TG2GraphicsSpecs.FindCompatiableTextureCubeRTFormat(const Format: TD3DFormat): TD3DFormat;
begin
  Result := FindFormat(
    Format,
    D3DUSAGE_RENDERTARGET,
    D3DRTYPE_CUBETEXTURE,
    @FormatScoresRenderTarget,
    Length(FormatScoresRenderTarget),
    50,
    m_Gfx.m_InitParamsActual.FormatTextureCubeRT
  );
end;

function TG2GraphicsSpecs.FindCompatiableTextureVolumeFormat(const Format: TD3DFormat): TD3DFormat;
begin
  Result := FindFormat(
    Format,
    0,
    D3DRTYPE_VOLUMETEXTURE,
    @FormatScoresTexture,
    Length(FormatScoresTexture),
    59,
    m_Gfx.m_InitParamsActual.FormatTextureVolume
  );
end;

function TG2GraphicsSpecs.FindCompatiableSurfaceRTFormat(const Format: TD3DFormat): TD3DFormat;
begin
  Result := FindFormat(
    Format,
    D3DUSAGE_RENDERTARGET,
    D3DRTYPE_SURFACE,
    @FormatScoresRenderTarget,
    Length(FormatScoresRenderTarget),
    50,
    m_Gfx.m_InitParamsActual.FormatSurfaceRT
  );
end;

function TG2GraphicsSpecs.FindCompatiableSurfaceDSFormat(const Format: TD3DFormat): TD3DFormat;
begin
  Result := FindFormat(
    Format,
    D3DUSAGE_DEPTHSTENCIL,
    D3DRTYPE_SURFACE,
    @FormatScoresDepthStencil,
    Length(FormatScoresDepthStencil),
    34,
    m_Gfx.m_InitParamsActual.FormatSurfaceDS
  );
end;

procedure TG2GraphicsSpecs.FindAsp(const Width, Height: Integer; var X, Y: Integer);
  var MaxDiv, d: Integer;
begin
  x := Width;
  y := Height;
  MaxDiv := Min(x, y);
  d := 2;
  while d <= MaxDiv do
  begin
    if (x mod d = 0) and (y mod d = 0) then
    begin
      x := x div d;
      y := y div d;
      d := 2;
      MaxDiv := Min(x, y);
    end
    else
    Inc(d);
  end;
end;

function TG2GraphicsSpecs.GetVRAM: DWord;
var
  DDraw: IDirectDraw7;
  Caps: TDDCaps;
begin
  if (
    Failed(
      DirectDrawCreateEx(
        nil,
        DDraw,
        IID_IDirectDraw7,
        nil
      )
    )
  ) then
  begin
    Result := 0;
    Exit;
  end;
  Caps.dwSize := SizeOf(TDDCaps);
  DDraw.GetCaps(@Caps, nil);
  Result := Caps.dwVidMemTotal;
  SafeRelease(DDraw);
end;

function TG2GraphicsSpecs.GetVRAMFree: DWord;
var
  DDraw: IDirectDraw7;
  Caps: TDDCaps;
begin
  if (
    Failed(
      DirectDrawCreateEx(
        nil,
        DDraw,
        IID_IDirectDraw7,
        nil
      )
    )
  ) then
  begin
    Result := 0;
    Exit;
  end;
  Caps.dwSize := SizeOf(TDDCaps);
  DDraw.GetCaps(@Caps, nil);
  Result := Caps.dwVidMemFree;
  SafeRelease(DDraw);
end;
//TG2GraphicsSpecs END

//TG2GraphicsInitParams BEGIN
constructor TG2GraphicsInitParams.Create;
begin
  inherited Create;
  Defaults;
end;

destructor TG2GraphicsInitParams.Destroy;
begin
  inherited Destroy;
end;

function TG2GraphicsInitParams.Verify: TG2Result;
type
  TModeScore = record
    Score: Single;
  end;
var
  i, j: Integer;
  ModeCount: DWord;
  CurMode: TD3DDisplayMode;
  ModeScore: array of TModeScore;
  Mode: Integer;
  ModeAccepted: Boolean;
  MaxScore: Single;
  Caps: TD3DCaps9;
  RenderFormat: TD3DFormat;
  SurfaceFormats: array of TD3DFormat;
  DisplayMode: TD3DDisplayMode;
  {$IFDEF G2_PERFHUD}AdapterIdentifier: TD3DAdapterIdentifier9;{$ENDIF}
const
  BackBufferFormats: array[0..5] of TD3DFormat = (
    D3DFMT_A8R8G8B8,
    D3DFMT_X8R8G8B8,
    D3DFMT_A2R10G10B10,
    D3DFMT_A1R5G5B5,
    D3DFMT_X1R5G5B5,
    D3DFMT_R5G6B5
  );
  RenderTargetFormats: array[0..7] of TD3DFormat = (
    D3DFMT_A8R8G8B8,
    D3DFMT_X8R8G8B8,
    D3DFMT_A16B16G16R16,
    D3DFMT_A2R10G10B10,
    D3DFMT_A2B10G10R10,
    D3DFMT_R8G8B8,
    D3DFMT_R5G6B5,
    D3DFMT_X1R5G5B5
  );
  DepthStencilFormats: array[0..5] of TD3DFormat = (
    D3DFMT_D24S8,
    D3DFMT_D24X4S4,
    D3DFMT_D15S1,
    D3DFMT_D32,
    D3DFMT_D24X8,
    D3DFMT_D16
  );
  TextureFormats: array[0..13] of TD3DFormat = (
    D3DFMT_DXT3,
    D3DFMT_DXT4,
    D3DFMT_DXT2,
    D3DFMT_DXT5,
    D3DFMT_DXT1,
    D3DFMT_A8R8G8B8,
    D3DFMT_A16B16G16R16,
    D3DFMT_A2R10G10B10,
    D3DFMT_A2B10G10R10,
    D3DFMT_A4R4G4B4,
    D3DFMT_A1R5G5B5,
    D3DFMT_X8R8G8B8,
    D3DFMT_R5G6B5,
    D3DFMT_X1R5G5B5
  );
  VertexCaps = (
    D3DVTXPCAPS_DIRECTIONALLIGHTS
    or D3DVTXPCAPS_LOCALVIEWER
    or D3DVTXPCAPS_POSITIONALLIGHTS
    or D3DVTXPCAPS_TEXGEN
    or D3DVTXPCAPS_TWEENING
  );
begin
  Result := grOk;

  {$IFDEF G2_PERFHUD}
  for i := 0 to m_Gfx.D3D9.GetAdapterCount - 1 do
  begin
    m_Gfx.D3D9.GetAdapterIdentifier(i, 0, AdapterIdentifier);
    if G2StrInStr(AdapterIdentifier.Description, 'PerfHUD') > 0 then
    begin
      m_Adapter := i;
      m_DeviceType := D3DDEVTYPE_REF;
    end;
  end;
  {$ENDIF}

  m_Gfx.D3D9.GetAdapterDisplayMode(m_Adapter, DisplayMode);

  if m_FormatTexture2DRT = D3DFMT_UNKNOWN then
  m_FormatTexture2DRT := m_FormatSurfaceRT;

  if m_FormatTexture2DDS = D3DFMT_UNKNOWN then
  m_FormatTexture2DDS := m_FormatSurfaceDS;

  if m_FormatTextureCube = D3DFMT_UNKNOWN then
  m_FormatTextureCube := m_FormatTexture2D;

  if m_FormatTextureCubeRT = D3DFMT_UNKNOWN then
  m_FormatTextureCubeRT := m_FormatTexture2DRT;

  if m_FormatTextureVolume = D3DFMT_UNKNOWN then
  m_FormatTextureVolume := m_FormatTexture2D;

  if Failed(
    m_Gfx.D3D9.CheckDeviceType(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      m_FormatBackBuffer,
      not m_FullScreen
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(BackBufferFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceType(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        BackBufferFormats[i],
        not m_FullScreen
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatBackBuffer := BackBufferFormats[i];
      Break;
    end;
    if not ModeAccepted then
    begin
      Result := grFail;
      Exit;
    end;
  end;

  if Failed(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      D3DUSAGE_RENDERTARGET,
      D3DRTYPE_SURFACE,
      m_FormatSurfaceRT
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(RenderTargetFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceFormat(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        D3DUSAGE_RENDERTARGET,
        D3DRTYPE_SURFACE,
        RenderTargetFormats[i]
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatSurfaceRT := RenderTargetFormats[i];
      Break;
    end;
    if not ModeAccepted then
    begin
      Result := grFail;
      Exit;
    end;
  end;

  if Failed(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      D3DUSAGE_DEPTHSTENCIL,
      D3DRTYPE_SURFACE,
      m_FormatSurfaceDS
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(DepthStencilFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceFormat(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        D3DUSAGE_DEPTHSTENCIL,
        D3DRTYPE_SURFACE,
        DepthStencilFormats[i]
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatSurfaceDS := DepthStencilFormats[i];
      Break;
    end;
    if not ModeAccepted then
    begin
      Result := grFail;
      Exit;
    end;
  end;

  if Failed(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      0,
      D3DRTYPE_TEXTURE,
      m_FormatTexture2D
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(TextureFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceFormat(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        0,
        D3DRTYPE_TEXTURE,
        TextureFormats[i]
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatTexture2D := TextureFormats[i];
      Break;
    end;
    if not ModeAccepted then
    begin
      Result := grFail;
      Exit;
    end;
  end;

  if Failed(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      D3DUSAGE_RENDERTARGET,
      D3DRTYPE_TEXTURE,
      m_FormatTexture2DRT
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(RenderTargetFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceFormat(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        D3DUSAGE_RENDERTARGET,
        D3DRTYPE_TEXTURE,
        RenderTargetFormats[i]
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatTexture2DRT := RenderTargetFormats[i];
      Break;
    end;
    if not ModeAccepted then
    m_FormatTexture2DRT := D3DFMT_UNKNOWN;
  end;

  if Failed(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      D3DUSAGE_DEPTHSTENCIL,
      D3DRTYPE_TEXTURE,
      m_FormatTexture2DDS
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(DepthStencilFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceFormat(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        D3DUSAGE_DEPTHSTENCIL,
        D3DRTYPE_TEXTURE,
        DepthStencilFormats[i]
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatTexture2DDS := DepthStencilFormats[i];
      Break;
    end;
    if not ModeAccepted then
    m_FormatTexture2DDS := D3DFMT_UNKNOWN;
  end;

  if Failed(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      0,
      D3DRTYPE_CUBETEXTURE,
      m_FormatTextureCube
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(TextureFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceFormat(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        0,
        D3DRTYPE_CUBETEXTURE,
        TextureFormats[i]
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatTextureCube := TextureFormats[i];
      Break;
    end;
    if not ModeAccepted then
    m_FormatTextureCube := D3DFMT_UNKNOWN;
  end;

  if Failed(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      D3DUSAGE_RENDERTARGET,
      D3DRTYPE_CUBETEXTURE,
      m_FormatTextureCubeRT
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(RenderTargetFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceFormat(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        D3DUSAGE_RENDERTARGET,
        D3DRTYPE_CUBETEXTURE,
        RenderTargetFormats[i]
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatTextureCubeRT := RenderTargetFormats[i];
      Break;
    end;
    if not ModeAccepted then
    m_FormatTextureCubeRT := D3DFMT_UNKNOWN;
  end;

  if Failed(
    m_Gfx.D3D9.CheckDeviceFormat(
      m_Adapter,
      m_DeviceType,
      DisplayMode.Format,
      0,
      D3DRTYPE_VOLUMETEXTURE,
      m_FormatTextureVolume
    )
  ) then
  begin
    Result := grConditionalOk;
    ModeAccepted := False;
    for i := 0 to High(TextureFormats) do
    if Succeeded(
      m_Gfx.D3D9.CheckDeviceFormat(
        m_Adapter,
        m_DeviceType,
        DisplayMode.Format,
        0,
        D3DRTYPE_VOLUMETEXTURE,
        TextureFormats[i]
      )
    ) then
    begin
      ModeAccepted := True;
      m_FormatTextureVolume := TextureFormats[i];
      Break;
    end;
    if not ModeAccepted then
    m_FormatTextureVolume := D3DFMT_UNKNOWN;
  end;

  m_Gfx.D3D9.GetDeviceCaps(m_Adapter, m_DeviceType, Caps);

  if m_FullScreen then
  begin
    ModeAccepted := False;
    ModeCount := m_Gfx.D3D9.GetAdapterModeCount(m_Adapter, DisplayMode.Format);
    SetLength(ModeScore, ModeCount);
    for i := 0 to ModeCount - 1 do
    begin
      m_Gfx.D3D9.EnumAdapterModes(m_Adapter, DisplayMode.Format, i, CurMode);
      if (CurMode.Format = m_FormatBackBuffer)
      and (CurMode.Width = m_Width)
      and (CurMode.Height = m_Height) then
      begin
        ModeAccepted := True;
        Break;
      end
      else
      begin
        ModeScore[i].Score := 1 - Min(
          Max(
            Abs(CurMode.Width - m_Width) * 0.001 +
            Abs(CurMode.Height - m_Height) * 0.001,
            0
          ),
          1
        );
        if CurMode.Format <> m_FormatBackBuffer then
        ModeScore[i].Score := Max(ModeScore[i].Score - 0.5, 0);
      end;
    end;
    if not ModeAccepted then
    begin
      if ModeCount = 0 then
      begin
        Result := grFail;
        Exit;
      end;
      Mode := 0;
      MaxScore := ModeScore[Mode].Score;
      for i := 1 to High(ModeScore) do
      begin
        if ModeScore[i].Score > MaxScore then
        begin
          Mode := i;
          MaxScore := ModeScore[i].Score;
        end;
      end;
      m_Gfx.D3D9.EnumAdapterModes(m_Adapter, DisplayMode.Format, Mode, CurMode);
      m_Width := CurMode.Width;
      m_Height := CurMode.Height;
    end;
  end;

  if m_AntialiasingSampleCount > 16 then
  m_AntialiasingSampleCount := 16;
  if m_AntialiasingSampleCount <= 1 then
  m_Antialiasing := False;
  if m_Antialiasing then
  begin
    while Failed(
      m_Gfx.D3D9.CheckDeviceMultiSampleType(
        m_Adapter,
        m_DeviceType,
        m_FormatBackBuffer,
        not m_Fullscreen,
        TD3DMultiSampleType(m_AntialiasingSampleCount),
        nil
      )
    ) do
    begin
      Result := grConditionalOk;
      Dec(m_AntialiasingSampleCount);
      if m_AntialiasingSampleCount <= 1 then
      begin
        m_Antialiasing := False;
        Break;
      end;
    end;
  end
  else
  m_AntialiasingSampleCount := 0;

  if m_PureDevice then
  begin
    if (Caps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT = 0)
    or (Caps.VertexProcessingCaps and VertexCaps <> VertexCaps) then
    begin
      Result := grConditionalOk;
      m_PureDevice := False;
    end;
  end;
  if (m_VertexProcessing = vpHardware)
  and (Caps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT = 0) then
  begin
    m_VertexProcessing := vpMixed;
    Result := grConditionalOk;
  end;
  if (m_VertexProcessing = vpMixed)
  and (Caps.VertexProcessingCaps = 0) then
  begin
    m_VertexProcessing := vpSoftware;
    Result := grConditionalOk;
  end;
end;

procedure TG2GraphicsInitParams.Defaults;
begin
  m_Adapter := D3DADAPTER_DEFAULT;
  m_DeviceType := D3DDEVTYPE_HAL;
  m_Width := 800;
  m_Height := 600;
  m_FormatBackBuffer := D3DFMT_X8R8G8B8;
  m_FormatSurfaceRT := D3DFMT_A8R8G8B8;
  m_FormatSurfaceDS := D3DFMT_D24S8;
  m_FormatTexture2D := D3DFMT_DXT3;
  m_FormatTexture2DRT := D3DFMT_UNKNOWN;
  m_FormatTexture2DDS := D3DFMT_UNKNOWN;
  m_FormatTextureCube := D3DFMT_UNKNOWN;
  m_FormatTextureCubeRT := D3DFMT_UNKNOWN;
  m_FormatTextureVolume := D3DFMT_UNKNOWN;
  m_FullScreen := False;
  m_VSync := False;
  m_Antialiasing := False;
  m_AntialiasingSampleCount := 0;
  m_VertexProcessing := vpHardware;
  m_MultiThreaded := True;
  m_PureDevice := False;
end;

procedure TG2GraphicsInitParams.Clone(const Params: TG2GraphicsInitParams);
begin
  Params.Adapter := m_Adapter;
  Params.DeviceType := m_DeviceType;
  Params.Width := m_Width;
  Params.Height := m_Height;
  Params.FormatBackBuffer := m_FormatBackBuffer;
  Params.FormatSurfaceRT := m_FormatSurfaceRT;
  Params.FormatSurfaceDS := m_FormatSurfaceDS;
  Params.FormatTexture2D := m_FormatTexture2D;
  Params.FormatTexture2DRT := m_FormatTexture2DRT;
  Params.FormatTexture2DDS := m_FormatTexture2DDS;
  Params.FormatTextureCube := m_FormatTextureCube;
  Params.FormatTextureCubeRT := m_FormatTextureCubeRT;
  Params.FormatTextureVolume := m_FormatTextureVolume;
  Params.FullScreen := m_FullScreen;
  Params.VSync := m_VSync;
  Params.Antialiasing := m_Antialiasing;
  Params.AntialiasingSampleCount := m_AntialiasingSampleCount;
  Params.VertexProcessing := m_VertexProcessing;
  Params.MultiThreaded := m_MultiThreaded;
  Params.PureDevice := m_PureDevice;
end;
//TG2GraphicsInitParams END

//TG2GraphicsRunTimeParams BEGIN
constructor TG2GraphicsRunTimeParams.Create;
begin
  inherited Create;
end;

destructor TG2GraphicsRunTimeParams.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GraphicsRunTimeParams.Assign(const Params: TG2GraphicsInitParams);
begin
  m_Adapter := Params.Adapter;
  m_DeviceType := Params.DeviceType;
  m_Width := Params.Width;
  m_Height := Params.Height;
  m_FormatBackBuffer := Params.FormatBackBuffer;
  m_FormatSurfaceRT := Params.FormatSurfaceRT;
  m_FormatSurfaceDS := Params.FormatSurfaceDS;
  m_FormatTexture2D := Params.FormatTexture2D;
  m_FormatTexture2DRT := Params.FormatTexture2DRT;
  m_FormatTexture2DDS := Params.FormatTexture2DDS;
  m_FormatTextureCube := Params.FormatTextureCube;
  m_FormatTextureCubeRT := Params.FormatTextureCubeRT;
  m_FormatTextureVolume := Params.FormatTextureVolume;
  m_FullScreen := Params.FullScreen;
  m_VSync := Params.VSync;
  m_Antialiasing := Params.Antialiasing;
  m_AntialiasingSampleCount := Params.AntialiasingSampleCount;
  m_VertexProcessing := Params.VertexProcessing;
  m_MultiThreaded := Params.MultiThreaded;
  m_PureDevice := Params.PureDevice;
end;

function TG2GraphicsRunTimeParams.Apply: TG2Result;
var
  ApplyChanges: Boolean;
  NeedQuickReset: Boolean;
  NeedHardReset: Boolean;
begin
  ApplyChanges := False;
  NeedQuickReset := False;
  NeedHardReset := False;
  if m_Gfx.m_InitParamsActual.FormatSurfaceRT <> m_FormatSurfaceRT then ApplyChanges := True;
  if m_Gfx.m_InitParamsActual.FormatSurfaceDS <> m_FormatSurfaceDS then ApplyChanges := True;
  if m_Gfx.m_InitParamsActual.FormatTexture2D <> m_FormatTexture2D then ApplyChanges := True;
  if m_Gfx.m_InitParamsActual.FormatTexture2DRT <> m_FormatTexture2DRT then ApplyChanges := True;
  if m_Gfx.m_InitParamsActual.FormatTexture2DDS <> m_FormatTexture2DDS then ApplyChanges := True;
  if m_Gfx.m_InitParamsActual.FormatTextureCube <> m_FormatTextureCube then ApplyChanges := True;
  if m_Gfx.m_InitParamsActual.FormatTextureCubeRT <> m_FormatTextureCubeRT then ApplyChanges := True;
  if m_Gfx.m_InitParamsActual.FormatTextureVolume <> m_FormatTextureVolume then ApplyChanges := True;
  if m_Gfx.m_InitParamsActual.FormatBackBuffer <> m_FormatBackBuffer then NeedQuickReset := True;
  if m_Gfx.m_InitParamsActual.Width <> m_Width then NeedQuickReset := True;
  if m_Gfx.m_InitParamsActual.Height <> m_Height then NeedQuickReset := True;
  if m_Gfx.m_InitParamsActual.Antialiasing <> m_Antialiasing then NeedQuickReset := True;
  if m_Gfx.m_InitParamsActual.AntialiasingSampleCount <> m_AntialiasingSampleCount then NeedQuickReset := True;
  if m_Gfx.m_InitParamsActual.FullScreen <> m_FullScreen then NeedHardReset := True;
  if m_FullScreen then NeedHardReset := True;
  ApplyChanges := ApplyChanges or NeedQuickReset or NeedHardReset;
  if NeedHardReset then NeedQuickReset := False;
  if ApplyChanges then
  begin
    m_Gfx.m_InitParams.FormatBackBuffer := m_FormatBackBuffer;
    m_Gfx.m_InitParams.FormatSurfaceRT := m_FormatSurfaceRT;
    m_Gfx.m_InitParams.FormatSurfaceDS := m_FormatSurfaceDS;
    m_Gfx.m_InitParams.FormatTexture2D := m_FormatTexture2D;
    m_Gfx.m_InitParams.FormatTexture2DRT := m_FormatTexture2DRT;
    m_Gfx.m_InitParams.FormatTexture2DDS := m_FormatTexture2DDS;
    m_Gfx.m_InitParams.FormatTextureCube := m_FormatTextureCube;
    m_Gfx.m_InitParams.FormatTextureCubeRT := m_FormatTextureCubeRT;
    m_Gfx.m_InitParams.FormatTextureVolume := m_FormatTextureVolume;
    m_Gfx.m_InitParams.Width := m_Width;
    m_Gfx.m_InitParams.Height := m_Height;
    m_Gfx.m_InitParams.Antialiasing := m_Antialiasing;
    m_Gfx.m_InitParams.AntialiasingSampleCount := m_AntialiasingSampleCount;
    m_Gfx.m_InitParams.FullScreen := m_FullScreen;
    if NeedHardReset then
    begin
      G2WriteLogTimed('New Paremeters, reset required.', 'Graphics');
      m_Gfx.HardReset;
    end;
    if NeedQuickReset then
    begin
      G2WriteLogTimed('New Paremeters, swap chain reset required.', 'Graphics');
      m_Gfx.QuickReset;
    end;
    m_Gfx.ParamsChange;
  end;
  Result := grOk;
end;
//TG2GraphicsRunTimeParams END

//TG2Transforms BEGIN
constructor TG2Transforms.Create;
begin
  inherited Create;
end;

destructor TG2Transforms.Destroy;
begin
  inherited Destroy;
end;

function TG2Transforms.GetW(const Index: Byte): TG2Mat;
begin
  Result := m_W[Index];
end;

procedure TG2Transforms.SetW(const Index: Byte; const m: TG2Mat);
begin
  m_W[Index] := m;
end;

procedure TG2Transforms.SetV(const m: TG2Mat);
begin
  m_V := m;
  m_Frustum.Update;
end;

procedure TG2Transforms.SetP(const m: TG2Mat);
begin
  m_P := m;
  m_Frustum.Update;
end;

function TG2Transforms.GetT(const Index: Byte): TG2Mat;
begin
  Result := m_T[Index];
end;

procedure TG2Transforms.SetT(const Index: Byte; const m: TG2Mat);
begin
  m_T[Index] := m;
end;

function TG2Transforms.GetWV: TG2Mat;
begin
  Result := m_W[0] * m_V;
end;

function TG2Transforms.GetVP: TG2Mat;
begin
  Result := m_V * m_P;
end;

function TG2Transforms.GetWVP: TG2Mat;
begin
  Result := m_W[0] * m_V * m_P;
end;

function TG2Transforms.GetWt: TG2Mat;
begin
  Result := m_W[0].Transpose;
end;

function TG2Transforms.GetVt: TG2Mat;
begin
  Result := m_V.Transpose;
end;

function TG2Transforms.GetPt: TG2Mat;
begin
  Result := m_P.Transpose;
end;

function TG2Transforms.GetWVt: TG2Mat;
begin
  Result := WV.Transpose;
end;

function TG2Transforms.GetVPt: TG2Mat;
begin
  Result := VP.Transpose;
end;

function TG2Transforms.GetWVPt: TG2Mat;
begin
  Result := WVP.Transpose;
end;

function TG2Transforms.GetWi: TG2Mat;
begin
  Result := m_W[0].Inverse;
end;

function TG2Transforms.GetVi: TG2Mat;
begin
  Result := m_V.Inverse;
end;

function TG2Transforms.GetPi: TG2Mat;
begin
  Result := m_P.Inverse;
end;

function TG2Transforms.GetWVi: TG2Mat;
begin
  Result := WV.Inverse;
end;

function TG2Transforms.GetVPi: TG2Mat;
begin
  Result := VP.Inverse;
end;

function TG2Transforms.GetWVPi: TG2Mat;
begin
  Result := WVP.Inverse;
end;

function TG2Transforms.GetVpos: TG2Vec3;
begin
  Result.x := -m_V.e30;
  Result.y := -m_V.e31;
  Result.z := -m_V.e32;
  Result := Result.Transform3x3(m_V.Transpose);
end;

function TG2Transforms.GetVdir: TG2Vec3;
begin
  Result := G2Vec3(0, 0, 1).Transform3x3(m_V.Transpose);
end;

function TG2Transforms.GetVb: TG2Mat;
begin
  Result := Vi;
  Result.e30 := 0;
  Result.e31 := 0;
  Result.e32 := 0;
end;

procedure TG2Transforms.OnDeviceLost;
begin

end;

procedure TG2Transforms.OnDeviceReset;
var
  i: Integer;
begin
  if not Initialized then Exit;
  for i := 0 to Core.Graphics.Caps.MaxVertexBlendMatrices - 1 do
  ApplyW(i);
  ApplyV;
  ApplyP;
  for i := 0 to Core.Graphics.Caps.MaxTextureBlendStages - 1 do
  ApplyT(i);
end;

function TG2Transforms.Initialize(const G2Core: TG2Core): TG2Result;
var
  i: Integer;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  for i := 0 to Core.Graphics.Caps.MaxVertexBlendMatrices - 1 do
  UpdateW(i);
  UpdateV;
  UpdateP;
  for i := 0 to Core.Graphics.Caps.MaxTextureBlendStages - 1 do
  UpdateT(i);
  m_Frustum.RefV := @m_V;
  m_Frustum.RefP := @m_P;
  m_Frustum.Update;
end;

function TG2Transforms.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
end;

procedure TG2Transforms.ApplyW(const Index: Byte);
begin
  Core.Graphics.Device.SetTransform(D3DTS_WORLDMATRIX(Index), m_W[Index]);
end;

procedure TG2Transforms.ApplyV;
begin
  Core.Graphics.Device.SetTransform(D3DTS_VIEW, m_V);
end;

procedure TG2Transforms.ApplyP;
begin
  Core.Graphics.Device.SetTransform(D3DTS_PROJECTION, m_P);
end;

procedure TG2Transforms.ApplyT(const Index: Byte);
begin
  Core.Graphics.Device.SetTransform(D3DTS_TEXTUREMATRIX(Index), m_T[Index]);
end;

procedure TG2Transforms.UpdateW(const Index: Byte);
begin
  Core.Graphics.Device.GetTransform(D3DTS_WORLDMATRIX(Index), PG2MatRef(@m_W[Index])^);
end;

procedure TG2Transforms.UpdateV;
begin
  Core.Graphics.Device.GetTransform(D3DTS_VIEW, PG2MatRef(@m_V)^);
end;

procedure TG2Transforms.UpdateP;
begin
  Core.Graphics.Device.GetTransform(D3DTS_PROJECTION, PG2MatRef(@m_P)^);
end;

procedure TG2Transforms.UpdateT(const Index: Byte);
begin
  Core.Graphics.Device.GetTransform(D3DTS_TEXTUREMATRIX(Index), PG2MatRef(@m_T[Index])^);
end;

procedure TG2Transforms.PushW;
begin
  SetLength(m_BufferW, Length(m_BufferW) + 1);
  m_BufferW[High(m_BufferW)] := m_W[0];
end;

procedure TG2Transforms.PushV;
begin
  SetLength(m_BufferV, Length(m_BufferV) + 1);
  m_BufferV[High(m_BufferV)] := m_V;
end;

procedure TG2Transforms.PushP;
begin
  SetLength(m_BufferP, Length(m_BufferP) + 1);
  m_BufferP[High(m_BufferP)] := m_P;
end;

procedure TG2Transforms.PushT;
begin
  SetLength(m_BufferT, Length(m_BufferT) + 1);
  m_BufferT[High(m_BufferT)] := m_T[0];
end;

procedure TG2Transforms.PopW;
begin
  m_W[0] := m_BufferW[High(m_BufferW)];
  SetLength(m_BufferW, Length(m_BufferW) - 1);
end;

procedure TG2Transforms.PopV;
begin
  m_V := m_BufferV[High(m_BufferV)];
  SetLength(m_BufferV, Length(m_BufferV) - 1);
end;

procedure TG2Transforms.PopP;
begin
  m_P := m_BufferP[High(m_BufferP)];
  SetLength(m_BufferP, Length(m_BufferP) - 1);
end;

procedure TG2Transforms.PopT;
begin
  m_T[0] := m_BufferT[High(m_BufferT)];
  SetLength(m_BufferT, Length(m_BufferT) - 1);
end;
//TG2Transforms END

//TG2Lights BEGIN
constructor TG2Lights.Create;
var
  i: Integer;
begin
  inherited Create;
  for i := 0 to High(m_Lights) do
  begin
    m_Lights[i].m_Lights := Self;
    m_Lights[i].m_Index := i;
  end;
end;

destructor TG2Lights.Destroy;
begin
  inherited Destroy;
end;

function TG2Lights.GetLight(const Index: Byte): PG2Light;
begin
  Result := @m_Lights[Index];
end;

function TG2Lights.GetLightCount: Integer;
begin
  Result := LCount;
end;

procedure TG2Lights.OnDeviceLost;
begin

end;

procedure TG2Lights.OnDeviceReset;
var
  i: Integer;
begin
  if not Initialized then Exit;
  for i := 0 to High(m_LightsBuffer) do
  begin
    Core.Graphics.Device.SetLight(i, m_LightsBuffer[i]);
    Core.Graphics.Device.LightEnable(i, m_LightsEnabled[i]);
  end;
end;

function TG2Lights.Initialize(const G2Core: TG2Core): TG2Result;
var
  i: Integer;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  ZeroMemory(@m_LightsBuffer[0], SizeOf(TD3DLight9) * Length(m_LightsBuffer));
  ZeroMemory(@m_LightsEnabled[0], SizeOf(Boolean) * Length(m_LightsEnabled));
  for i := 0 to High(m_Lights) do
  begin
    ZeroMemory(@m_Lights[i].Light, SizeOf(TD3DLight9));
    m_Lights[i].SetAmbientLight(0);
    m_Lights[i].Enabled := False;
    m_LightsBuffer[i] := m_Lights[i].Light;
    m_LightsEnabled[i] := False;
  end;
end;

function TG2Lights.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
end;

procedure TG2Lights.DisableLights;
var
  i: Integer;
begin
  for i := 0 to High(m_Lights) do
  begin
    m_Lights[i].Enabled := False;
    m_Lights[i].SetToDevice;
  end;
end;

procedure TG2Lights.SetToEffect(const Effect: TG2Effect);
var
  i: Integer;
  v3: TG2Vec3;
  LightAmbient: TD3DXColor;
  LightCountPoint: Integer;
  LightPosPoint: array[0..15] of TD3DXVector4;
  LightColorPoint: array[0..15] of TD3DXColor;
  LightRangePoint: array[0..15] of Single;
  LightCountDirectional: Integer;
  LightDirDirectional: array[0..15] of TD3DXVector4;
  LightColorDirectional: array[0..15] of TD3DXColor;
begin
  LightCountPoint := 0;
  LightCountDirectional := 0;
  LightAmbient := D3DXColorFromDWord(Core.Graphics.RenderStates.Ambient);
  with Core.Graphics do
  for i := 0 to Lights.LightCount - 1 do
  if Lights[i].Enabled then
  begin
    case Lights[i].LightType of
      ltAmbient:
      begin
        D3DXColorAdd(LightAmbient, LightAmbient, Lights[i].Light.Ambient);
      end;
      ltPoint:
      begin
        v3 := Lights[i].Light.Position;
        LightPosPoint[LightCountPoint] := D3DXVector4(
          v3 * Transforms.V, 1
        );
        LightColorPoint[LightCountPoint] := Lights[i].Light.Diffuse;
        LightRangePoint[LightCountPoint] := Sqr(Lights[i].Light.Range);
        Inc(LightCountPoint);
      end;
      ltDirectional:
      begin
        v3 := Lights[i].Light.Direction;
        D3DXVec3TransformNormal(PG2Vec3Ref(@v3)^, v3, Transforms.V);
        v3 := -v3;
        v3.Normalize;
        LightDirDirectional[LightCountDirectional] := D3DXVector4(v3, 1);
        LightColorDirectional[LightCountDirectional] := Lights[i].Light.Diffuse;
        Inc(LightCountDirectional);
      end;
    end;
  end;
  Effect.SetVector('LightAmbient', PD3DXVector4(@LightAmbient)^);
  Effect.SetInt('LightCountPoint', LightCountPoint);
  Effect.SetVectorArray('LightPosPoint', @LightPosPoint[0], LightCountPoint);
  Effect.SetVectorArray('LightColorPoint', @LightColorPoint[0], LightCountPoint);
  Effect.SetFloatArray('LightRangePoint', @LightRangePoint[0], LightCountPoint);
  Effect.SetInt('LightCountDirectional', LightCountDirectional);
  Effect.SetVectorArray('LightDirDirectional', @LightDirDirectional[0], LightCountDirectional);
  Effect.SetVectorArray('LightColorDirectional', @LightColorDirectional[0], LightCountDirectional);
end;

procedure TG2Lights.TG2Light.SetAmbientLight(
      const Ambient: TG2Color
    );
begin
  ZeroMemory(@Light, SizeOf(TD3DLight9));
  Light._Type := D3DLIGHT_DIRECTIONAL;
  Light.Ambient := Ambient;
  LightType := ltAmbient;
end;

procedure TG2Lights.TG2Light.SetPointLight(
      const PosX, PosY, PosZ: Single;
      const Diffuse: TG2Color;
      const Range: Single
    );
begin
  ZeroMemory(@Light, SizeOf(TD3DLight9));
  Light._Type := D3DLIGHT_POINT;
  Light.Position := G2Vec3(PosX, PosY, PosZ);
  Light.Diffuse := Diffuse;
  Light.Specular := Diffuse;
  Light.Range := Range;
  if Range = 0 then
  Light.Attenuation1 := 9999
  else
  Light.Attenuation1 := 5 / Range;
  LightType := ltPoint;
end;

procedure TG2Lights.TG2Light.SetDirectionalLight(
      const DirX, DirY, DirZ: Single;
      const Diffuse: TG2Color
    );
begin
  ZeroMemory(@Light, SizeOf(TD3DLight9));
  Light._Type := D3DLIGHT_DIRECTIONAL;
  Light.Direction := G2Vec3(DirX, DirY, DirZ);
  Light.Diffuse := Diffuse;
  Light.Specular := Diffuse;
  LightType := ltDirectional;
end;

procedure TG2Lights.TG2Light.SetToDevice;
begin
  m_Lights.Core.Graphics.Device.SetLight(
    m_Index, Light
  );
  m_Lights.Core.Graphics.Device.LightEnable(m_Index, Enabled);
  m_Lights.m_LightsBuffer[m_Index] := Light;
  m_Lights.m_LightsEnabled[m_Index] := Enabled;
end;
//TG2Lights END

//TG2SwapChain BEGIN
constructor TG2SwapChain.Create;
begin
  inherited Create;
  m_RenderTarget := TG2SurfaceRT.Create;
end;

destructor TG2SwapChain.Destroy;
begin
  Release;
  m_RenderTarget.Free;
  inherited Destroy;
end;

procedure TG2SwapChain.Release;
begin
  m_RenderTarget.Release;
  SafeRelease(m_SwapChain);
end;

procedure TG2SwapChain.Initialize(
      const Handle: HWND;
      const Width, Height: Word;
      const PresentParams: TD3DPresentParameters
    );
var
  BackBufferRT: IDirect3DSurface9;
begin
  ZeroMemory(@m_PresentParams, SizeOf(TD3DPresentParameters));
  m_PresentParams.BackBufferWidth := Width;
  m_PresentParams.BackBufferHeight := Height;
  m_PresentParams.BackBufferFormat := PresentParams.BackBufferFormat;
  m_PresentParams.BackBufferCount := 1;
  m_PresentParams.MultiSampleType := PresentParams.MultiSampleType;
  m_PresentParams.SwapEffect := PresentParams.SwapEffect;
  m_PresentParams.hDeviceWindow := Handle;
  m_PresentParams.Windowed := PresentParams.Windowed;
  m_PresentParams.EnableAutoDepthStencil := False;
  m_PresentParams.Flags := m_PresentParams.Flags;
  m_PresentParams.PresentationInterval := PresentParams.PresentationInterval;
  m_Gfx.Device.CreateAdditionalSwapChain(m_PresentParams, m_SwapChain);
  m_SwapChain.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, BackBufferRT);
  m_RenderTarget.Initialize(m_Gfx.Core);
  m_RenderTarget.Surface := BackBufferRT;
end;

procedure TG2SwapChain.Reset(const Width, Height: Word);
var
  BackBufferRT: IDirect3DSurface9;
begin
  Release;
  m_PresentParams.BackBufferFormat := m_Gfx.m_InitParamsActual.FormatBackBuffer;
  m_PresentParams.BackBufferWidth := Width;
  m_PresentParams.BackBufferHeight := Height;
  m_PresentParams.MultiSampleType := TD3DMultisampleType(m_Gfx.m_InitParamsActual.AntialiasingSampleCount);
  m_Gfx.Device.CreateAdditionalSwapChain(m_PresentParams, m_SwapChain);
  m_SwapChain.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, BackBufferRT);
  m_RenderTarget.Surface := BackBufferRT;
end;

procedure TG2SwapChain.OnLostDevice;
begin
  Release;
end;

procedure TG2SwapChain.OnResetDevice;
var
  BackBufferRT: IDirect3DSurface9;
begin
  m_Gfx.Device.CreateAdditionalSwapChain(m_PresentParams, m_SwapChain);
  m_SwapChain.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, BackBufferRT);
  m_RenderTarget.Surface := BackBufferRT;
end;

procedure TG2SwapChain.Present;
begin
  m_SwapChain.Present(nil, nil, 0, nil, D3DPRESENT_DONOTWAIT);
end;
//TG2SwapChain END

//TG2Audio BEGIN
constructor TG2Audio.Create;
begin
  inherited Create;
  m_PlugClass := TG2PlugAudio;
  m_Volume := 1;
  m_MusicMgrs := TList.Create;
  m_SoundMgrs := TList.Create;
end;

destructor TG2Audio.Destroy;
begin
  m_SoundMgrs.Free;
  m_MusicMgrs.Free;
  inherited Destroy;
end;

procedure TG2Audio.SetVolume(Value: Single);
var
  i: Integer;
begin
  m_Volume := Value;
  for i := 0 to m_MusicMgrs.Count - 1 do
  begin
    TG2MusicMgr(m_MusicMgrs[i]).ResetVolume;
  end;
  for i := 0 to m_SoundMgrs.Count - 1 do
  begin
    TG2SoundMgr(m_SoundMgrs[i]).ResetVolume;
  end;
end;

function TG2Audio.Initialize(const G2Core: TG2Core): TG2Result;
begin
  G2WriteLogTimed('Initialization Started.', 'Audio');
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Volume := 1;
  if Failed(
    CoCreateInstance(
      CLSID_DirectMusicLoader,
      nil,
      CLSCTX_INPROC,
      IID_IDirectMusicLoader8,
      m_Loader
    )
  ) then
  begin
    G2WriteLogTimed('(E) Failed to create loader.', 'Audio');
    Result := grFail;
    Exit;
  end;
  if Failed(
    CoCreateInstance(
      CLSID_DirectMusicPerformance,
      nil,
      CLSCTX_INPROC,
      IID_IDirectMusicPerformance8,
      m_Performance
    )
  ) then
  begin
    G2WriteLogTimed('(E) Failed to create performance.', 'Audio');
    SafeRelease(m_Loader);
    Result := grFail;
    Exit;
  end;
  if Failed(
    m_Performance.InitAudio(
      nil,
      nil,
      Core.Handle,
      DMUS_APATH_SHARED_STEREOPLUSREVERB,
      64,
      DMUS_AUDIOF_ALL,
      nil
    )
  ) then
  begin
    G2WriteLogTimed('(E) Failed to initialize audio.', 'Audio');
    SafeRelease(m_Loader);
    SafeRelease(m_Performance);
    Result := grFail;
    Exit;
  end;
  Result := grOk;
  G2WriteLogTimed('Initialization Finished.', 'Audio');
end;

function TG2Audio.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  G2WriteLogTimed('Finalization Started', 'Audio');
  SafeRelease(m_Loader);
  SafeRelease(m_Performance);
  Result := grOk;
  G2WriteLogTimed('Finalization Finished', 'Audio');
end;
//TG2Audio END

//TG2Input BEGIN
constructor TG2Input.Create;
begin
  inherited Create;
  m_PlugClass := TG2PlugInput;
end;

destructor TG2Input.Destroy;
begin
  inherited Destroy;
end;

procedure TG2Input.Update;
var
  i: Integer;
  NewKeyState: array[0..255] of Byte;
  NewMouseState: TDIMouseState;
  Res: HResult;
  function TryUpdateKeyboard: HResult;
  begin
    Result := m_Keyboard.Poll;
    if Succeeded(Result) then
    begin
      Result := m_Keyboard.GetDeviceState(
        SizeOf(NewKeyState),
        @NewKeyState
      );
    end;
  end;
  function TryUpdateMouse: HResult;
  begin
    Result := m_Mouse.Poll;
    if Succeeded(Result) then
    begin
      Result := m_Mouse.GetDeviceState(
        SizeOf(NewMouseState),
        @NewMouseState
      );
    end;
  end;
begin
  if not Initialized then Exit;
  if m_KeyboardAquired then
  begin
    Res := TryUpdateKeyboard;
    if Failed(Res) then
    begin
      if (Res = DIERR_INPUTLOST)
      or (Res = DIERR_NOTACQUIRED) then
      begin
        if Succeeded(m_Keyboard.Acquire) then
        begin
          Res := TryUpdateKeyboard;
        end;
      end;
    end;
    if Succeeded(Res) then
    begin
      for i := 0 to High(m_KeyState) do
      begin
        if m_KeyState[i] <> (NewKeyState[i] > 0) then
        begin
          if NewKeyState[i] > 0 then
          KeyDown(i)
          else
          KeyUp(i);
        end;
        m_KeyState[i] := NewKeyState[i] > 0;
      end;
    end;
  end;
  if m_MouseAquired then
  begin
    Res := TryUpdateMouse;
    if Failed(Res) then
    begin
      if (Res = DIERR_INPUTLOST)
      or (Res = DIERR_NOTACQUIRED) then
      begin
        if Succeeded(m_Mouse.Acquire) then
        begin
          Res := TryUpdateMouse;
        end;
      end;
    end;
    if Succeeded(Res) then
    begin
      for i := 0 to High(m_MouseState.rgbButtons) do
      if m_MouseState.rgbButtons[i] <> NewMouseState.rgbButtons[i] then
      begin
        if NewMouseState.rgbButtons[i] > 0 then
        MouseDown(i)
        else
        MouseUp(i);
      end;
      Move(NewMouseState, m_MouseState, SizeOf(m_MouseState));
      if (m_MouseState.lX <> 0)
      or (m_MouseState.lY <> 0) then
      MouseMove(Point(m_MouseState.lX, m_MouseState.lY));
      if m_MouseState.lZ <> 0 then
      WheelMove(m_MouseState.lZ);
    end;
  end;
end;

procedure TG2Input.KeyDown(const Key: Byte);
var
  i: Integer;
begin
  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugInput(m_Plugs.Items[i]).OnKeyDown) then
  TG2PlugInput(m_Plugs.Items[i]).OnKeyDown(Key);
end;

procedure TG2Input.KeyUp(const Key: Byte);
var
  i: Integer;
begin
  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugInput(m_Plugs.Items[i]).OnKeyUp) then
  TG2PlugInput(m_Plugs.Items[i]).OnKeyUp(Key);
end;

procedure TG2Input.MouseDown(const Button: Byte);
var
  i: Integer;
begin
  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugInput(m_Plugs.Items[i]).OnMouseDown) then
  TG2PlugInput(m_Plugs.Items[i]).OnMouseDown(Button);
end;

procedure TG2Input.MouseUp(const Button: Byte);
var
  i: Integer;
begin
  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugInput(m_Plugs.Items[i]).OnMouseUp) then
  TG2PlugInput(m_Plugs.Items[i]).OnMouseUp(Button);
end;

procedure TG2Input.MouseMove(const Shift: TPoint);
var
  i: Integer;
begin
  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugInput(m_Plugs.Items[i]).OnMouseMove) then
  TG2PlugInput(m_Plugs.Items[i]).OnMouseMove(Shift);
end;

procedure TG2Input.WheelMove(const Shift: Integer);
var
  i: Integer;
begin
  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugInput(m_Plugs.Items[i]).OnWheelMove) then
  TG2PlugInput(m_Plugs.Items[i]).OnWheelMove(Shift);
end;

procedure TG2Input.KeyPress(const Key: AnsiChar);
var
  i: Integer;
begin
  for i := 0 to m_Plugs.Count - 1 do
  if Assigned(TG2PlugInput(m_Plugs.Items[i]).OnKeyPress) then
  TG2PlugInput(m_Plugs.Items[i]).OnKeyPress(Key);
end;

function TG2Input.GetKey(const Index: Byte): Boolean;
begin
  Result := m_KeyState[Index];
end;

function TG2Input.GetButton(const Index: Byte): Boolean;
begin
  Result := m_MouseState.rgbButtons[Index] > 0;
end;

function TG2Input.GetMousePos: TPoint;
var
  cRect: TRect;
  cW, cH: Integer;
begin
  Result := Mouse.CursorPos;
  if Initialized then
  begin
    ScreenToClient(Core.Handle, Result);
    if Core.Graphics.Initialized then
    begin
      GetClientRect(Core.Handle, cRect);
      cW := cRect.Right - cRect.Left;
      cH := cRect.Bottom - cRect.Top;
      Result.X := Round((Result.X / cW) * Core.Graphics.Params.Width);
      Result.Y := Round((Result.Y / cH) * Core.Graphics.Params.Height);
    end;
  end;
end;

function TG2Input.GetMouseShift: TPoint;
begin
  Result := Point(m_MouseState.lX, m_MouseState.lY);
end;

function TG2Input.GetWheelShift: Integer;
begin
  Result := m_MouseState.lZ;
end;

function TG2Input.Initialize(const G2Core: TG2Core): TG2Result;
var
  Inst: HINST;
begin
  G2WriteLogTimed('Initialization Started.', 'Input');
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_KeyboardAquired := False;
  m_MouseAquired := False;
  m_TopHandle := Core.Handle;
  while GetParent(m_TopHandle) > 0 do
  m_TopHandle := GetParent(m_TopHandle);
  m_PrevWndProc := Pointer(GetWindowLong(m_TopHandle, GWL_WNDPROC));
  SetWindowLong(m_TopHandle, GWL_WNDPROC, DWord(@G2InputWndProc));
  SetWindowLong(m_TopHandle, GWL_USERDATA, DWord(Self));
  Inst := GetWindowLong(m_TopHandle, GWL_HINSTANCE);
  if Failed(
    DirectInput8Create(
      Inst,
      DIRECTINPUT_VERSION,
      IID_IDirectInput8,
      m_DI,
      nil
    )
  ) then
  begin
    G2WriteLogTimed('(E) Failed to initialize DirectInput8.', 'Input');
    Result := grFail;
    m_Initialized := False;
    Exit;
  end;
  if Succeeded(
    m_DI.CreateDevice(
      GUID_SysKeyboard,
      m_Keyboard,
      nil
    )
  ) then
  begin
    if Succeeded(
      m_Keyboard.SetDataFormat(c_dfDIKeyboard)
    ) then
    begin
      if Succeeded(
        m_Keyboard.SetCooperativeLevel(
          m_TopHandle,
          DISCL_FOREGROUND
          or DISCL_NONEXCLUSIVE
          or DISCL_NOWINKEY
        )
      ) then
      begin
        m_KeyboardAquired := True;
        ZeroMemory(@m_KeyState, SizeOf(m_KeyState));
      end
      else
      begin
        SafeRelease(m_Keyboard);
      end;
    end
    else
    begin
      SafeRelease(m_Keyboard);
    end;
  end;
  if not Assigned(m_Keyboard) then
  G2WriteLogTimed('(E) Failed to create keyboard device.', 'Input');
  if Succeeded(
    m_DI.CreateDevice(
      GUID_SysMouse,
      m_Mouse,
      nil
    )
  ) then
  begin
    if Succeeded(
      m_Mouse.SetDataFormat(c_dfDIMouse)
    ) then
    begin
      if Succeeded(
        m_Mouse.SetCooperativeLevel(
          m_TopHandle,
          DISCL_FOREGROUND
          or DISCL_NONEXCLUSIVE
          or DISCL_NOWINKEY
        )
      ) then
      begin
        m_MouseAquired := True;
        ZeroMemory(@m_MouseState, SizeOf(m_MouseState));
      end
      else
      begin
        SafeRelease(m_Mouse);
      end;
    end
    else
    begin
      SafeRelease(m_Mouse);
    end;
  end;
  if not Assigned(m_Mouse) then
  G2WriteLogTimed('(E) Failed to create mouse device.', 'Input');
  if G2ResFail(
    Core.RequestPlug(TG2PlugTimer, @m_PlugTimer)
  ) then
  begin
    G2WriteLogTimed('(E) Input is unable to connect to Timer.', 'Input');
    SafeRelease(m_Mouse);
    SafeRelease(m_Keyboard);
    SafeRelease(m_DI);
    m_Initialized := False;
    Result := grFail;
    Exit;
  end;
  m_PlugTimer.OnTimer := Update;
  Result := grOk;
  G2WriteLogTimed('Initialization Finished.', 'Input');
end;

function TG2Input.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugTimer);
  SafeRelease(m_Mouse);
  SafeRelease(m_Keyboard);
  SafeRelease(m_DI);
  SetWindowLong(m_TopHandle, GWL_WNDPROC, DWord(m_PrevWndProc));
  Result := grOk;
end;
//TG2Input END

//TG2Network BEGIN
function TG2Network.GetSockData: PWSAData;
begin
  Result := @m_SockData;
end;

constructor TG2Network.Create;
begin
  inherited Create;
  m_PlugClass := TG2PlugNetwork;
end;

destructor TG2Network.Destroy;
begin
  inherited Destroy;
end;

function TG2Network.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  G2WriteLogTimed('Initialization Started.', 'Network');
  if WSAStartup($0101, m_SockData) = 0 then
  Result := grOk
  else
  begin
    Result := grFail;
    m_Initialized := False;
  end;
  G2WriteLogTimed('Initialization Finished.', 'Network');
end;

function TG2Network.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  G2WriteLogTimed('Finalization Started.', 'Network');
  WSACleanup;
  Result := grOk;
  G2WriteLogTimed('Finalization Finished.', 'Network');
end;

function TG2Network.HTTPRequestGET(const Host, Params: AnsiString; var Response: AnsiString): TG2Result;
  var phe: PHostEnt;
  var Sock: Integer;
  var Addr: SockAddr_in;
  var Request: AnsiString;
  var RespBuf: array[0..1024] of AnsiChar;
  var Bytes: Integer;
begin
  Result := grOk;
  phe := GetHostByName(PAnsiChar(Host));
  if phe = nil then
  begin
    G2WriteLogTimed('(E) HTTPRequestGET error: ' + G2WSAErrorToStr(WSAGetLastError), 'Network');
    Result := grFail;
    Exit;
  end;
  Sock := Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Sock = INVALID_SOCKET then
  begin
    G2WriteLogTimed('(E) HTTPRequestGET error: ' + G2WSAErrorToStr(WSAGetLastError), 'Network');
    Result := grFail;
    Exit;
  end;
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(80);
  Addr.sin_addr := PInAddr(phe.h_addr_list^)^;
  if connect(Sock, Addr, SizeOf(addr)) = SOCKET_ERROR then
  begin
    G2WriteLogTimed('(E) HTTPRequestGET error: ' + G2WSAErrorToStr(WSAGetLastError), 'Network');
    Result := grFail;
    Exit;
  end;
  Request := 'GET ' + Params + ' HTTP/1.0' + #$D#$A;
  Request := Request + 'Host: ' + Host + #$D#$A;
  Request := Request + #$D#$A;
  Send(Sock, Request[1], Length(Request), 0);
  Response := '';
  repeat
    Bytes := Recv(Sock, RespBuf, 1024, 0);
    Response := Response + Copy(RespBuf, 0, Bytes);
  until Bytes < 1024;
  CloseSocket(Sock);
  if Length(Response) > 0 then
  Response := AnsiString(Copy(Response, Pos(#$D#$A#$D#$A, String(Response)) + 4, Length(Response)));
end;
//TG2Network END

//TG2Module BEGIN
constructor TG2Module.Create;
begin
  inherited Create;
end;

destructor TG2Module.Destroy;
begin
  inherited Destroy;
end;

function TG2Module.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
end;

function TG2Module.Finalize: TG2Result;
begin
  Result := inherited Finalize;
end;
//TG2Module END

//TG2Plug BEGIN
constructor TG2Plug.Create;
begin
  inherited Create;
  m_Engine := nil;
end;

destructor TG2Plug.Destroy;
begin
  m_Engine := nil;
  inherited Destroy;
end;
//TG2Plug END

//TG2PlugTimer BEGIN
constructor TG2PlugTimer.Create;
begin
  inherited Create;
  m_OnTimer := nil;
  m_OnUpdate := nil;
  m_OnRender := nil;
end;

destructor TG2PlugTimer.Destroy;
begin
  inherited Destroy;
end;
//TG2PlugTimer END

//TG2PlugGraphics BEGIN
constructor TG2PlugGraphics.Create;
begin
  inherited Create;
  m_OnDeviceLost := nil;
  m_OnDeviceReset := nil;
  m_OnParamsChange := nil;
end;

destructor TG2PlugGraphics.Destroy;
begin
  inherited Destroy;
end;
//TG2PlugGraphics END

//TG2PlugAudio BEGIN
constructor TG2PlugAudio.Create;
begin
  inherited Create;
end;

destructor TG2PlugAudio.Destroy;
begin
  inherited Destroy;
end;
//TG2PlugAudio END

//TG2PlugInput BEGIN
constructor TG2PlugInput.Create;
begin
  inherited Create;
  m_OnKeyDown := nil;
  m_OnKeyUp := nil;
  m_OnKeyPress := nil;
  m_OnMouseDown := nil;
  m_OnMouseUp := nil;
  m_OnMouseMove := nil;
  m_OnWheelMove := nil;
end;

destructor TG2PlugInput.Destroy;
begin
  inherited Destroy;
end;

function TG2PlugInput.GetKeyDown(const Index: Byte): Boolean;
begin
  Result := m_Input.GetKey(Index);
end;

function TG2PlugInput.GetMouseDown(const Index: Byte): Boolean;
begin
  Result := m_Input.GetButton(Index);
end;

function TG2PlugInput.GetMousePos: TPoint;
begin
  Result := m_Input.GetMousePos;
end;

function TG2PlugInput.GetMouseShift: TPoint;
begin
  Result := m_Input.GetMouseShift;
end;

function TG2PlugInput.GetWheelShift: Integer;
begin
  Result := m_Input.GetWheelShift;
end;

function TG2PlugInput.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Input := Core.Input;
  Result := grOk;
end;

function TG2PlugInput.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;

function TG2PlugInput.KeyToChar(const Key: Byte): AnsiChar;
begin
  Result := DIKToChar(Key);
end;
//TG2PlugInput END

//TG2PlugNetwork BEGIN
constructor TG2PlugNetwork.Create;
begin
  inherited Create;
end;

destructor TG2PlugNetwork.Destroy;
begin
  inherited Destroy;
end;
//TG2PlugNetwork END

//TG2ResMgr BEGIN
constructor TG2ResMgr.Create;
begin
  inherited Create;
  m_Resources.Clear;
end;

destructor TG2ResMgr.Destroy;
begin
  Finalize;
  m_Resources.Clear;
  inherited Destroy;
end;

function TG2ResMgr.GetCount: Integer;
begin
  Result := m_Resources.Count;
end;

function TG2ResMgr.FindResourceIndex(const NameCache: PWordArray; const Len: Integer): Integer;
var
  i, j, Incr, l, h, m: Integer;
begin
  l := 0;
  h := m_Resources.Count - 1;
  while l <= h do
  begin
    m := (l + h) div 2;
    j := Length(TG2Res(m_Resources[m]).NameCache);
    if j < Len then
    begin
      j := j - 1;
      Incr := 1;
    end
    else
    begin
      j := Len - 1;
      Incr := -1;
    end;
    for i := 0 to j do
    if TG2Res(m_Resources[m]).NameCache[i] < NameCache^[i] then
    begin
      Incr := 1;
      Break;
    end
    else if TG2Res(m_Resources[m]).NameCache[i] > NameCache^[i] then
    begin
      Incr := -1;
      Break;
    end;
    if Incr > 0 then l := m + 1 else h := m - 1;
  end;
  Result := l;
end;

function TG2ResMgr.FindResource(const Name: WideString): TG2Res;
  var Cache: array of Word;
  var Ind, i: Integer;
  var n: WideString;
begin
  SetLength(Cache, Length(Name));
  for i := 0 to High(Cache) do
  Cache[i] := Ord(UpperCase(Name[i + 1])[1]);
  Ind := FindResourceIndex(@Cache[0], Length(Cache));
  if (Ind < m_Resources.Count) then
  begin
    n := TG2Res(m_Resources[Ind]).Name;
    if (UpperCase(n) = UpperCase(Name)) then
    Result := TG2Res(m_Resources[Ind])
    else
    Result := nil;
  end
  else
  Result := nil;
end;

procedure TG2ResMgr.AddResource(const Res: TG2Res);
begin
  m_Resources.Insert(FindResourceIndex(@Res.NameCache[0], Length(Res.NameCache)), Res);
  Res.Mgrs.Add(Self);
end;

procedure TG2ResMgr.RemoveResource(const Res: TG2Res);
begin
  m_Resources.Remove(Res);
  Res.Mgrs.Remove(Self);
end;

procedure TG2ResMgr.DeleteResource(const Index: Integer);
begin
  TG2Res(m_Resources[Index]).Mgrs.Remove(Self);
  m_Resources.Delete(Index);
end;

procedure TG2ResMgr.FreeResources;
var
  i, j: Integer;
begin
  i := 0;
  j := m_Resources.Count - 1;
  while i <= j do
  if TG2Res(m_Resources[i]).Mgrs.Count <= 1 then
  begin
    TG2Res(m_Resources[i]).Finalize;
    TG2Res(m_Resources[i]).Free;
    Dec(j);
  end
  else
  Inc(i);
  m_Resources.Clear;
end;

function TG2ResMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;

function TG2ResMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  FreeResources;
  Result := grOk;
end;
//TG2ResMgr END

//TG2Res BEGIN
constructor TG2Res.Create;
begin
  inherited Create;
  m_Name := '';
  NameCache := nil;
  Mgrs.Clear;
end;

destructor TG2Res.Destroy;
begin
  while Mgrs.Count > 0 do
  TG2ResMgr(Mgrs[0]).RemoveResource(Self);
  Mgrs.Clear;
  Finalize;
  inherited Destroy;
end;

procedure TG2Res.SetName(const Value: WideString);
var
  i: Integer;
begin
  m_Name := Value;
  SetLength(NameCache, Length(m_Name));
  for i := 0 to High(NameCache) do
  NameCache[i] := Ord(UpperCase(m_Name[i + 1])[1]);
  for i := 0 to Mgrs.Count - 1 do
  begin
    TG2ResMgr(Mgrs[i]).RemoveResource(Self);
    TG2ResMgr(Mgrs[i]).AddResource(Self);
  end;
end;
//TG2Res END

//TG2MusicMgr BEGIN
constructor TG2MusicMgr.Create;
begin
  inherited Create;
end;

destructor TG2MusicMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TG2MusicMgr.ResetVolume;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2Music(m_Resources[i]).Volume := TG2Music(m_Resources[i]).Volume;
end;

function TG2MusicMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.Audio.m_MusicMgrs.Add(Self);
  Result := grOk;
end;

function TG2MusicMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  FreeResources;
  Core.Audio.m_MusicMgrs.Remove(Self);
  Result := grOk;
end;

function TG2MusicMgr.FindMusic(const Name: WideString): TG2Music;
begin
  Result := TG2Music(FindResource(Name));
end;

function TG2MusicMgr.CreateMusic(const Name: WideString): TG2Music;
begin
  Result := TG2Music.Create;
  Result.Initialize(Core);
  Result.Name := Name;
  AddResource(Result);
end;
//TG2MusicMgr END

//TG2Music BEGIN
constructor TG2Music.Create;
begin
  inherited Create;
  CoCreateInstance(
    CLSID_FilterGraph,
    nil,
    CLSCTX_INPROC_SERVER,
    IID_IGraphBuilder,
    m_Graph
  );
  m_Graph.QueryInterface(IID_IMediaControl, m_Control);
  m_Graph.QueryInterface(IID_IMediaEventEx, m_Event);
  m_Graph.QueryInterface(IID_IMediaPosition, m_Position);
  m_Graph.QueryInterface(IID_IMediaSeeking, m_Seeking);
  m_Graph.QueryInterface(IID_IMediaFilter, m_Filter);
  m_Graph.QueryInterface(IID_IBasicAudio, m_Audio);
  m_Open := False;
  m_IsPlaying := False;
  m_PlayTime := GetTickCount;
end;

destructor TG2Music.Destroy;
begin
  ClearGraph;
  SafeRelease(m_Audio);
  SafeRelease(m_Filter);
  SafeRelease(m_Seeking);
  SafeRelease(m_Position);
  SafeRelease(m_Event);
  SafeRelease(m_Control);
  SafeRelease(m_Graph);
  inherited Destroy;
end;

procedure TG2Music.ClearGraph;
var
  Filter: IBaseFilter;
  Enum: IEnumFilters;
begin
  if m_Open then
  begin
    m_Control.Stop;
    if SUCCEEDED(m_Graph.EnumFilters(Enum)) then
    begin
      while (S_OK = Enum.Next(1, Filter, nil)) do
      begin
        m_Graph.RemoveFilter(Filter);
        SafeRelease(Filter);
        Enum.Reset;
      end;
    end;
  end;
  m_Open := False;
end;

procedure TG2Music.SetPosition(const Value: Int64);
var
  Pos, PosStop: Int64;
begin
  if m_Open then
  begin
    Pos := Value;
    PosStop := 0;
    m_Seeking.SetPositions(
      Pos,
      AM_SEEKING_AbsolutePositioning,
      PosStop,
      AM_SEEKING_NoPositioning
    );
  end;
end;

function TG2Music.GetPosition: Int64;
begin
  if m_Open then
  m_Seeking.GetCurrentPosition(Result)
  else
  Result := 0;
end;

function TG2Music.GetDuration: Int64;
begin
  if m_Open then
  m_Seeking.GetDuration(Result)
  else
  Result := 0;
end;

procedure TG2Music.SetVolume(Value: Single);
var
  Vol: Integer;
begin
  if m_Open and Assigned(m_Audio) then
  begin
    Vol := Round((m_G2Audio.Volume * Value - 1) * 10000);
    if Vol < -10000 then Vol := -10000;
    if Vol > 0 then Vol := 0;
    m_Audio.put_Volume(Vol);
  end;
end;

function TG2Music.GetVolume: Single;
var
  Vol: Integer;
begin
  if m_Open then
  begin
    m_Audio.get_Volume(Vol);
    Result := Vol * 0.0001 + 1;
  end
  else
  Result := 0;
end;

procedure TG2Music.SetPan(Value: Single);
var
  Pan: Integer;
begin
  if m_Open then
  begin
    Pan := Round(Value * 10000);
    if Pan < -10000 then Pan := -10000;
    if Pan > 10000 then Pan := 10000;
    m_Audio.put_Balance(Pan);
  end;
end;

function TG2Music.GetPan: Single;
var
  Pan: Integer;
begin
  if m_Open then
  begin
    m_Audio.get_Balance(Pan);
    Result := Pan * 0.0001;
  end
  else
  Result := 0;
end;

procedure TG2Music.SetPlayRate(Value: Single);
begin
  if m_Open then
  m_Seeking.SetRate(Value);
end;

function TG2Music.GetPlayRate: Single;
var
  Rate: Double;
begin
  if m_Open then
  begin
    m_Seeking.GetRate(Rate);
    Result := Rate;
  end
  else
  Result := 0;
end;

function TG2Music.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_G2Audio := Core.Audio;
  G2WriteLogTimed(AnsiString('(+) Music Initialized: (' + Name + ').'));
end;

function TG2Music.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  G2WriteLogTimed(AnsiString('(-) Music Finalized: (' + Name + ').'));
end;

procedure TG2Music.OpenFile(const f: WideString);
begin
  ClearGraph;
  m_Open := Succeeded(
    m_Graph.RenderFile(PWideChar(f), nil)
  );
  if m_Open then
  begin
    Volume := 1;
    G2WriteLogTimed(AnsiString('(>) Music Loaded (' + Name + ').'));
  end;
end;

procedure TG2Music.Close;
begin
  if m_Open then
  G2WriteLogTimed(AnsiString('(<) Music Released (' + Name + ').'));
  ClearGraph;
end;

procedure TG2Music.Play;
begin
  if m_Open then
  begin
    m_Control.Run;
    m_IsPlaying := True;
    m_PlayTime := GetTickCount;
  end;
end;

procedure TG2Music.Pause;
begin
  if m_Open then
  begin
    m_Control.Pause;
  end;
end;

procedure TG2Music.Stop;
begin
  if m_Open then
  begin
    m_Control.Stop;
    Rewind;
  end;
end;

procedure TG2Music.Rewind;
begin
  if m_Open then
  begin
    m_Position.put_CurrentPosition(0);
  end;
end;

function TG2Music.IsPlaying: Boolean;
var
  State: TFilterState;
  EventCode, p1, p2: Integer;
begin
  if m_Open then
  begin
    m_Event.GetEvent(EventCode, p1, p2, 0);
    if EventCode = EC_COMPLETE then
    Stop;
    m_Control.GetState(0, State);
    Result := State = State_Running;
    if not Result
    and m_IsPlaying
    and (GetTickCount - m_PlayTime < 1000) then
    Result := True;
    m_IsPlaying := Result;
  end
  else
  Result := False;
end;
//TG2Music END

//TG2SoundMgr BEGIN
constructor TG2SoundMgr.Create;
begin
  inherited Create;
end;

destructor TG2SoundMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TG2SoundMgr.ResetVolume;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2Sound(m_Resources[i]).Volume := TG2Sound(m_Resources[i]).Volume;
end;

function TG2SoundMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.Audio.m_SoundMgrs.Add(Self);
  Result := grOk;
end;

function TG2SoundMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.Audio.m_SoundMgrs.Remove(Self);
  FreeResources;
  Result := grOk;
end;

function TG2SoundMgr.FindSound(const Name: WideString): TG2Sound;
begin
  Result := TG2Sound(FindResource(Name));
end;

function TG2SoundMgr.CreateSound(const Name: WideString): TG2Sound;
begin
  Result := TG2Sound.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  AddResource(Result);
end;

function TG2SoundMgr.CreateSoundFromFile(const Name, f: WideString): TG2Sound;
begin
  Result := TG2Sound.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFile(f)) then
  AddResource(Result) else FreeAndNil(Result);
end;
//TG2SoundMgr END

//TG2Sound BEGIN
constructor TG2Sound.Create;
begin
  inherited Create;
  m_Loaded := False;
  m_Volume := 1;
  m_Enable3D := False;
  m_Instances := TList.Create;
end;

destructor TG2Sound.Destroy;
begin
  while m_Instances.Count > 0 do
  TG2SoundInst(m_Instances[0]).Free;
  m_Instances.Free;
  Finalize;
  inherited Destroy;
end;

procedure TG2Sound.SetVolume(Value: Single);
var
  i, Vol: Integer;
begin
  m_Volume := Value;
  if m_Loaded then
  begin
    Vol := Round((m_G2Audio.Volume * Value - 1) * 4000);
    if Vol < -4000 then Vol := -4000;
    if Vol > 0 then Vol := 0;
    m_AudioPath.SetVolume(Vol, 0);
  end;
  for i := 0 to m_Instances.Count - 1 do
  TG2SoundInst(m_Instances[i]).Volume := TG2SoundInst(m_Instances[i]).Volume;
end;

procedure TG2Sound.SetSoundPosition(Value: TG2Vec3);
begin
  m_Buffer3D.SetPosition(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2Sound.GetSoundPosition: TG2Vec3;
begin
  m_Buffer3D.GetPosition(PG2Vec3Ref(@Result)^);
end;

procedure TG2Sound.SetSoundVelocity(Value: TG2Vec3);
begin
  m_Buffer3D.SetVelocity(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2Sound.GetSoundVelocity: TG2Vec3;
begin
  m_Buffer3D.GetVelocity(PG2Vec3Ref(@Result)^);
end;

procedure TG2Sound.SetSoundDirection(Value: TG2Vec3);
begin
  m_Buffer3D.SetConeOrientation(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2Sound.GetSoundDirection: TG2Vec3;
begin
  m_Buffer3D.GetConeOrientation(PG2Vec3Ref(@Result)^);
end;

procedure TG2Sound.SetListenerPosition(Value: TG2Vec3);
begin
  m_Listener.SetPosition(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2Sound.GetListenerPosition: TG2Vec3;
begin
  m_Listener.GetPosition(PG2Vec3Ref(@Result)^);
end;

procedure TG2Sound.SetListenerVelocity(Value: TG2Vec3);
begin
  m_Listener.SetVelocity(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2Sound.GetListenerVelocity: TG2Vec3;
begin
  m_Listener.GetVelocity(PG2Vec3Ref(@Result)^);
end;

procedure TG2Sound.SetListenerDirection(Value: TG2Vec3);
begin
  m_Listener.SetOrientation(Value.x, Value.y, Value.z, 0, 1, 0, DS3D_IMMEDIATE);
end;

function TG2Sound.GetListenerDirection: TG2Vec3;
var
  Dummy: TD3DXVector3;
begin
  m_Listener.GetOrientation(PG2Vec3Ref(@Result)^, Dummy);
end;

procedure TG2Sound.SetMinDistance(Value: Single);
begin
  m_Buffer3D.SetMinDistance(Value, DS3D_IMMEDIATE);
end;

function TG2Sound.GetMinDistance: Single;
begin
  m_Buffer3D.GetMinDistance(Result);
end;

procedure TG2Sound.SetMaxDistance(Value: Single);
begin
  m_Buffer3D.SetMaxDistance(Value, DS3D_IMMEDIATE);
end;

function TG2Sound.GetMaxDistance: Single;
begin
  m_Buffer3D.GetMaxDistance(Result);
end;

procedure TG2Sound.SetDistanceFactor(Value: Single);
begin
  m_Listener.SetDistanceFactor(Value, DS3D_IMMEDIATE);
end;

function TG2Sound.GetDistanceFactor: Single;
begin
  m_Listener.GetDistanceFactor(Result);
end;

procedure TG2Sound.SetDopplerFactor(Value: Single);
begin
  m_Listener.SetDopplerFactor(Value, DS3D_IMMEDIATE);
end;

function TG2Sound.GetDopplerFactor: Single;
begin
  m_Listener.GetDopplerFactor(Result);
end;

procedure TG2Sound.SetRollOffFactor(Value: Single);
begin
  m_Listener.SetRolloffFactor(Value, DS3D_IMMEDIATE);
end;

function TG2Sound.GetRollOffFactor: Single;
begin
  m_Listener.GetRolloffFactor(Result);
end;

procedure TG2Sound.SetPlayRate(Value: Single);
begin
  m_Buffer.SetFrequency(Round(m_Frequency * Value));
end;

function TG2Sound.GetPlayRate: Single;
var
  Freq: DWORD;
begin
  m_Buffer.GetFrequency(Freq);
  Result := Freq / m_Frequency;
end;

procedure TG2Sound.SetEnable3D(const Value: Boolean);
begin
  if m_Enable3D <> Value then
  begin
    m_Enable3D := Value;
    if m_Enable3D then
    m_Buffer3D.SetMode(DS3DMODE_NORMAL, DS3D_IMMEDIATE)
    else
    m_Buffer3D.SetMode(DS3DMODE_DISABLE, DS3D_IMMEDIATE);
  end;
end;

function TG2Sound.Initialize(const G2Core: TG2Core): TG2Result;
var
  BufferParams: DS3DBuffer;
  ListenerParams: DS3DListener;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_G2Audio := Core.Audio;
  m_G2Audio.m_Performance.CreateStandardAudioPath(
    DMUS_APATH_DYNAMIC_3D,
    16,
    True,
    m_AudioPath
  );
  ZeroMemory(@BufferParams, SizeOf(BufferParams));
  ZeroMemory(@ListenerParams, SizeOf(ListenerParams));
  BufferParams.dwSize := SizeOf(BufferParams);
  ListenerParams.dwSize := SizeOf(ListenerParams);
  m_AudioPath.GetObjectInPath(
    0,
    DMUS_PATH_BUFFER,
    0,
    GUID_NULL,
    0,
    IID_IDirectSoundBuffer,
    m_Buffer
  );
  m_AudioPath.GetObjectInPath(
    0,
    DMUS_PATH_BUFFER,
    0,
    GUID_NULL,
    0,
    IID_IDirectSound3DBuffer8,
    m_Buffer3D
  );
  m_AudioPath.GetObjectInPath(
    0,
    DMUS_PATH_PRIMARY_BUFFER,
    0,
    GUID_NULL,
    0,
    IID_IDirectSound3Dlistener8,
    m_Listener
  );
  m_Buffer3D.GetAllParameters(BufferParams);
  BufferParams.dwMode := DS3DMODE_DISABLE;
  BufferParams.flMinDistance := 1;
  BufferParams.flMaxDistance := 100;
  m_Buffer3D.SetAllParameters(BufferParams, DS3D_IMMEDIATE);
  m_Listener.GetAllParameters(ListenerParams);
  ListenerParams.flRolloffFactor := 0.25;
  ListenerParams.flDopplerFactor := 0;
  ListenerParams.flDistanceFactor := 0;
  ListenerParams.vPosition := D3DXVector3(0, 0, 0);
  m_Listener.SetAllParameters(ListenerParams, DS3D_IMMEDIATE);
  m_Buffer.GetFrequency(m_Frequency);
  m_PlayTime := GetTickCount - 1000;
  G2WriteLogTimed(AnsiString('(+) Sound Initialized: (' + Name + ').'), 'Sounds');
end;

function TG2Sound.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  UnLoad;
  SafeRelease(m_Listener);
  SafeRelease(m_Buffer3D);
  SafeRelease(m_Buffer);
  SafeRelease(m_AudioPath);
  G2WriteLogTimed(AnsiString('(+) Sound Finalized: (' + Name + ').'), 'Sounds');
end;

function TG2Sound.LoadFile(const f: WideString): TG2Result;
begin
  Result := grFail;
  if SUCCEEDED(
    m_G2Audio.m_Loader.LoadObjectFromFile(
      CLSID_DirectMusicSegment,
      IID_IDirectMusicSegment8,
      PWideChar(f),
      m_Segment
    )
  ) then
  begin
    if SUCCEEDED(
      m_Segment.Download(m_G2Audio.m_Performance)
    ) then
    begin
      m_Loaded := True;
      Result := grOk;
      G2WriteLogTimed(AnsiString('(>) Sound Loaded (' + Name + ').'), 'Sounds');
    end;
  end;
end;

procedure TG2Sound.UnLoad;
begin
  if m_Loaded then
  begin
    m_Segment.Unload(m_AudioPath);
    SafeRelease(m_Segment);
    m_Loaded := False;
    G2WriteLogTimed(AnsiString('(<) Sound Released (' + Name + ').'), 'Sounds');
  end;
end;

procedure TG2Sound.Play;
begin
  if m_Loaded then
  begin
    m_G2Audio.m_Performance.PlaySegmentEx(
      m_Segment,
      nil,
      nil,
      DMUS_SEGF_SECONDARY,
      0,
      nil,
      nil,
      m_AudioPath
    );
    m_PlayTime := GetTickCount;
  end;
end;

procedure TG2Sound.Stop;
begin
  if m_Loaded then
  m_G2Audio.m_Performance.StopEx(
    m_Segment,
    0,
    0
  );
end;

function TG2Sound.IsPlaying: Boolean;
begin
  Result := (
    (GetTickCount - m_PlayTime < 1000)
    or (m_G2Audio.m_Performance.IsPlaying(m_Segment, nil) = S_OK)
  );
end;

function TG2Sound.CreateInstance: TG2SoundInst;
begin
  Result := TG2SoundInst.Create;
  Result.m_G2Sound := Self;
  Result.Initialize;
  Result.Volume := 1;
  Result.SoundPosition := SoundPosition;
  Result.SoundVelocity := SoundVelocity;
  Result.SoundDirection := SoundDirection;
  Result.ListenerPosition := ListenerPosition;
  Result.ListenerVelocity := ListenerVelocity;
  Result.ListenerDirection := ListenerDirection;
  Result.MinDistance := MinDistance;
  Result.MaxDistance := MaxDistance;
  Result.DistanceFactor := DistanceFactor;
  Result.DopplerFactor := DopplerFactor;
  Result.RollOffFactor := RollOffFactor;
  Result.PlayRate := PlayRate;
  m_Instances.Add(Result);
end;
//TG2Sound END

//TG2SoundInst BEGIN
constructor TG2SoundInst.Create;
begin
  inherited Create;
  m_Loaded := False;
  m_Volume := 1;
end;

destructor TG2SoundInst.Destroy;
begin
  m_G2Sound.m_Instances.Remove(Self);
  SafeRelease(m_Listener);
  SafeRelease(m_Buffer3D);
  SafeRelease(m_Buffer);
  SafeRelease(m_AudioPath);
  inherited Destroy;
end;

procedure TG2SoundInst.SetVolume(Value: Single);
var
  Vol: Integer;
begin
  m_Volume := Value;
  if m_Loaded then
  begin
    Vol := Round((m_G2Sound.Volume * Value - 1) * 4000);
    if Vol < -4000 then Vol := -4000;
    if Vol > 0 then Vol := 0;
    m_AudioPath.SetVolume(Vol, 0);
  end;
end;

procedure TG2SoundInst.SetSoundPosition(Value: TG2Vec3);
begin
  m_Buffer3D.SetPosition(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetSoundPosition: TG2Vec3;
begin
  m_Buffer3D.GetPosition(PG2Vec3Ref(@Result)^);
end;

procedure TG2SoundInst.SetSoundVelocity(Value: TG2Vec3);
begin
  m_Buffer3D.SetVelocity(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetSoundVelocity: TG2Vec3;
begin
  m_Buffer3D.GetVelocity(PG2Vec3Ref(@Result)^);
end;

procedure TG2SoundInst.SetSoundDirection(Value: TG2Vec3);
begin
  m_Buffer3D.SetConeOrientation(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetSoundDirection: TG2Vec3;
begin
  m_Buffer3D.GetConeOrientation(PG2Vec3Ref(@Result)^);
end;

procedure TG2SoundInst.SetListenerPosition(Value: TG2Vec3);
begin
  m_Listener.SetPosition(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetListenerPosition: TG2Vec3;
begin
  m_Listener.GetPosition(PG2Vec3Ref(@Result)^);
end;

procedure TG2SoundInst.SetListenerVelocity(Value: TG2Vec3);
begin
  m_Listener.SetVelocity(Value.x, Value.y, Value.z, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetListenerVelocity: TG2Vec3;
begin
  m_Listener.GetVelocity(PG2Vec3Ref(@Result)^);
end;

procedure TG2SoundInst.SetListenerDirection(Value: TG2Vec3);
begin
  m_Listener.SetOrientation(Value.x, Value.y, Value.z, 0, 1, 0, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetListenerDirection: TG2Vec3;
var
  Dummy: TD3DXVector3;
begin
  m_Listener.GetOrientation(PG2Vec3Ref(@Result)^, Dummy);
end;

procedure TG2SoundInst.SetMinDistance(Value: Single);
begin
  m_Buffer3D.SetMinDistance(Value, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetMinDistance: Single;
begin
  m_Buffer3D.GetMinDistance(Result);
end;

procedure TG2SoundInst.SetMaxDistance(Value: Single);
begin
  m_Buffer3D.SetMaxDistance(Value, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetMaxDistance: Single;
begin
  m_Buffer3D.GetMaxDistance(Result);
end;

procedure TG2SoundInst.SetDistanceFactor(Value: Single);
begin
  m_Listener.SetDistanceFactor(Value, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetDistanceFactor: Single;
begin
  m_Listener.GetDistanceFactor(Result);
end;

procedure TG2SoundInst.SetDopplerFactor(Value: Single);
begin
  m_Listener.SetDopplerFactor(Value, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetDopplerFactor: Single;
begin
  m_Listener.GetDopplerFactor(Result);
end;

procedure TG2SoundInst.SetRollOffFactor(Value: Single);
begin
  m_Listener.SetRolloffFactor(Value, DS3D_IMMEDIATE);
end;

function TG2SoundInst.GetRollOffFactor: Single;
begin
  m_Listener.GetRolloffFactor(Result);
end;

procedure TG2SoundInst.SetPlayRate(Value: Single);
begin
  m_Buffer.SetFrequency(Round(m_Frequency * Value));
end;

function TG2SoundInst.GetPlayRate: Single;
var
  Freq: DWORD;
begin
  m_Buffer.GetFrequency(Freq);
  Result := Freq / m_Frequency;
end;

procedure TG2SoundInst.Initialize;
var
  BufferParams: DS3DBuffer;
  ListenerParams: DS3DListener;
begin
  m_G2Sound.m_G2Audio.m_Performance.CreateStandardAudioPath(
    DMUS_APATH_DYNAMIC_3D,
    64,
    True,
    m_AudioPath
  );
  ZeroMemory(@BufferParams, SizeOf(BufferParams));
  ZeroMemory(@ListenerParams, SizeOf(ListenerParams));
  BufferParams.dwSize := SizeOf(BufferParams);
  ListenerParams.dwSize := SizeOf(ListenerParams);
  m_AudioPath.GetObjectInPath(
    0,
    DMUS_PATH_BUFFER,
    0,
    GUID_NULL,
    0,
    IID_IDirectSoundBuffer,
    m_Buffer
  );
  m_AudioPath.GetObjectInPath(
    0,
    DMUS_PATH_BUFFER,
    0,
    GUID_NULL,
    0,
    IID_IDirectSound3DBuffer8,
    m_Buffer3D
  );
  m_AudioPath.GetObjectInPath(
    0,
    DMUS_PATH_PRIMARY_BUFFER,
    0,
    GUID_NULL,
    0,
    IID_IDirectSound3Dlistener8,
    m_Listener
  );
  m_Buffer3D.GetAllParameters(BufferParams);
  BufferParams.dwMode := DS3DMODE_HEADRELATIVE;
  BufferParams.flMinDistance := 0.1;
  BufferParams.flMaxDistance := 100;
  m_Buffer3D.SetAllParameters(BufferParams, DS3D_IMMEDIATE);
  m_Listener.GetAllParameters(ListenerParams);
  ListenerParams.flRolloffFactor := 0;
  ListenerParams.flDopplerFactor := 0;
  m_Listener.SetAllParameters(ListenerParams, DS3D_IMMEDIATE);
  m_Buffer.GetFrequency(m_Frequency);
end;

procedure TG2SoundInst.Play;
begin
  if m_G2Sound.m_Loaded then
  m_G2Sound.m_G2Audio.m_Performance.PlaySegmentEx(
    m_G2Sound.m_Segment,
    nil,
    nil,
    DMUS_SEGF_SECONDARY,
    0,
    nil,
    nil,
    m_AudioPath
  );
end;

procedure TG2SoundInst.Stop;
begin
  if m_G2Sound.m_Loaded then
  m_G2Sound.m_G2Audio.m_Performance.StopEx(
    m_G2Sound.m_Segment,
    0,
    0
  );
end;

function TG2SoundInst.IsPlaying: Boolean;
begin
  Result := m_G2Sound.m_G2Audio.m_Performance.IsPlaying(m_G2Sound.m_Segment, nil) = S_OK;
end;
//TG2SoundInst END

//TG2TextureMgr BEGIN
constructor TG2TextureMgr.Create;
begin
  inherited Create;
end;

destructor TG2TextureMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TG2TextureMgr.OnDeviceLost;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2TextureBase(m_Resources[i]).OnDeviceLost;
end;

procedure TG2TextureMgr.OnDeviceReset;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2TextureBase(m_Resources[i]).OnDeviceReset;
end;

function TG2TextureMgr.GetTexture(const Index: Integer): TG2TextureBase;
begin
  Result := TG2TextureBase(m_Resources[Index]);
end;

function TG2TextureMgr.CreateTexture2D(
      const Name: WideString;
      const Width, Height: Integer;
      const MipLevels: Integer = 8;
      const Usage: DWord = 0;
      const Format: TD3DFormat = D3DFMT_UNKNOWN;
      const Pool: TD3DPool = D3DPOOL_MANAGED
    ): TG2Texture2D;
begin
  Result := TG2Texture2D.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.MakeTexture(Width, Height, MipLevels, Usage, Format, Pool)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTexture2DFromFile(
      const Name: WideString;
      const f: WideString;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
begin
  if FileExists(f) then
  begin
    Result := TG2Texture2D.Create;
    Result.Name := Name;
    Result.Initialize(Core);
    if G2ResOk(Result.LoadFromFile(f, MipLevels, Format)) then
    AddResource(Result) else FreeAndNil(Result);
    Exit;
  end
  else
  G2WriteLogTimed('(W) Texture file missing: ' + AnsiString(f), 'Textures');
  Result := nil;
end;

function TG2TextureMgr.CreateTexture2DFromStream(
      const Name: WideString;
      const s: TMemoryStream;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
begin
  Result := TG2Texture2D.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromStream(s, Size, MipLevels, Format)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTexture2DFromBuffer(
      const Name: WideString;
      const b: Pointer;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
begin
  Result := TG2Texture2D.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromBuffer(b, Size, MipLevels, Format)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTexture2DFromGraphic(
      const Name: WideString;
      const g: TGraphic;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
begin
  Result := TG2Texture2D.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromGraphic(g, MipLevels, Format)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTexture2DFromPack(
      const Name: WideString;
      const FolderName, FileName: AnsiString;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
  var i: Integer;
  var CanLoad: Boolean;
begin
  CanLoad := False;
  for i := 0 to Core.PackLinker.PackCount - 1 do
  if Core.PackLinker.Packs[i].PackFileExists(FileName) then
  begin
    CanLoad := True;
    Break;
  end;
  if CanLoad then
  begin
    Result := TG2Texture2D.Create;
    Result.Name := Name;
    Result.Initialize(Core);
    if G2ResOk(Result.LoadFromPack(FolderName, FileName, MipLevels, Format)) then
    AddResource(Result) else FreeAndNil(Result);
    Exit;
  end;
  Result := nil;
end;

function TG2TextureMgr.CreateTexture2DFromTexture2D(
      const Name: WideString;
      const Tex: TG2Texture2DBase;
      const MipLevels: Integer = 8;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
begin
  Result := TG2Texture2D.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromTexture2D(Tex, MipLevels, Format)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTexture2DNormalMap(
      const Name: WideString;
      const HeightMap: TG2Texture2D;
      const Amplitude: Single = 1;
      const MipLevels: Integer = 8;
      const Channel: DWord = D3DX_CHANNEL_ALPHA;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2D;
begin
  Result := TG2Texture2D.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(
    Result.LoadNormalMap(
      HeightMap, Amplitude, MipLevels, Channel, Format
    )
  ) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTexture2DRT(
      const Name: WideString;
      const Width, Height: Integer;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Texture2DRT;
begin
  Result := TG2Texture2DRT.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.MakeRenderTarget(Width, Height, Format)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTexture2DDS(
      const Name: WideString;
      const Width, Height: Integer
    ): TG2Texture2DDS;
begin
  Result := TG2Texture2DDS.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.MakeDepthStencil(Width, Height)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTexture2DVideo(
      const Name: WideString;
      const f: WideString
    ): TG2Texture2DVideo;
begin
  Result := TG2Texture2DVideo.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.StreamFile(f)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTextureCube(
      const Name: WideString;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const Usage: DWord = 0;
      const Format: TD3DFormat = D3DFMT_UNKNOWN;
      const Pool: TD3DPool = D3DPOOL_MANAGED
    ): TG2TextureCube;
begin
  Result := TG2TextureCube.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(
    Result.MakeTexture(
      Size,
      MipLevels,
      Usage,
      Format,
      Pool
    )
  ) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTextureCubeFromFile(
      const Name: WideString;
      const f: WideString;
      const MipLevels: Integer = 1;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2TextureCube;
begin
  Result := TG2TextureCube.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromFile(f, MipLevels, Format)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTextureCubeFromPack(
      const Name: WideString;
      const FolderName, FileName: AnsiString;
      const MipLevels: Integer = 1;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2TextureCube;
begin
  Result := TG2TextureCube.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromPack(FolderName, FileName, MipLevels, Format)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.CreateTextureCubeRT(
      const Name: WideString;
      const Size: Integer;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2TextureCubeRT;
begin
  Result := TG2TextureCubeRT.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(
    Result.MakeRenderTarget(
      Size, Format
    )
  ) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2TextureMgr.FindTexture(const Name: WideString): TG2TextureBase;
begin
  Result := TG2TextureBase(FindResource(Name));
end;

procedure TG2TextureMgr.AddTexture(const Texture: TG2TextureBase);
begin
  AddResource(Texture);
end;

procedure TG2TextureMgr.DeleteTexture(const Index: Integer);
begin
  DeleteResource(Index);
end;

procedure TG2TextureMgr.RemoveTexture(const Texture: TG2TextureBase);
begin
  RemoveResource(Texture);
end;

procedure TG2TextureMgr.FreeTextures;
begin
  FreeResources;
end;

function TG2TextureMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  Result := grOk;
end;

function TG2TextureMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  FreeResources;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;
//TG2TextureMgr END

//TG2TextureSurface BEGIN
constructor TG2TextureSurface.Create(
      const Texture: TG2TextureBase;
      const Level: Integer = 0;
      const Face: TD3DCubemapFaces = D3DCUBEMAP_FACE_POSITIVE_X
    );
begin
  inherited Create;
  m_Texture := Texture;
  m_Face := Face;
  m_Level := Level;
  m_Locked := False;
  m_Desc.Width := 0;
  m_Desc.Height := 0;
  if m_Texture is TG2Texture2D then
  begin
    IDirect3DTexture9(
      m_Texture.Texture
    ).GetLevelDesc(
      m_Level,
      m_Desc
    );
  end
  else if m_Texture is TG2TextureCube then
  begin
    IDirect3DCubeTexture9(
      m_Texture.Texture
    ).GetLevelDesc(
      m_Level,
      m_Desc
    );
  end;
  m_Width := m_Desc.Width;
  m_Height := m_Desc.Height;
  m_ProcSetPixel := SetPixelDummy;
  m_ProcGetPixel := GetPixelDummy;
end;

destructor TG2TextureSurface.Destroy;
begin
  if m_Locked then UnLock;
  inherited Destroy;
end;

{$Warnings Off}
function TG2TextureSurface.GetPixelA16B16G16R16(const X, Y: Integer): TG2Color;
var
  c: PWordArray;
begin
  c := PWordArray(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 8);
  with Result do
  begin
    r := c^[0] shr 8;
    g := c^[1] shr 8;
    b := c^[2] shr 8;
    a := c^[3] shr 8;
  end;
end;

procedure TG2TextureSurface.SetPixelA16B16G16R16(const X, Y: Integer; const Value: TG2Color);
var
  c: PWordArray;
begin
  c := PWordArray(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 8);
  with Value do
  begin
    c[0] := r shl 8;
    c[1] := g shl 8;
    c[2] := b shl 8;
    c[3] := a shl 8;
  end;
end;

function TG2TextureSurface.GetPixelA2B10G10R10(const X, Y: Integer): TG2Color;
var
  c: DWord;
begin
  c := PDWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 4)^;
  with Result do
  begin
    r := (c and $3fc) shr 2;
    g := (c and $ff000) shr 12;
    b := (c and $3fc00000) shr 22;
    a := (c and $c0000000) shr 24;
  end;
end;

procedure TG2TextureSurface.SetPixelA2B10G10R10(const X, Y: Integer; const Value: TG2Color);
begin
  with Value do
  PDWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 4)^ := (
    (r shl 2) or (g shl 12) or (b shl 22) or ((a and $3) shl 24)
  );
end;

function TG2TextureSurface.GetPixelG16R16(const X, Y: Integer): TG2Color;
var
  c: DWord;
begin
  c := PDWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 4)^;
  with Result do
  begin
    b := $ff;
    g := (c and $ff00) shr 8;
    r := (c and $ff000000) shr 24;
    a := $ff;
  end;
end;

procedure TG2TextureSurface.SetPixelG16R16(const X, Y: Integer; const Value: TG2Color);
begin
  with Value do
  PDWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 4)^ := (
    (g shl 8) or (r shl 24)
  );
end;

function TG2TextureSurface.GetPixelA2R10G10B10(const X, Y: Integer): TG2Color;
var
  c: DWord;
begin
  c := PDWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 4)^;
  with Result do
  begin
    b := (c and $3fc) shr 2;
    g := (c and $ff000) shr 12;
    r := (c and $3fc00000) shr 22;
    a := (c and $c0000000) shr 24;
  end;
end;

procedure TG2TextureSurface.SetPixelA2R10G10B10(const X, Y: Integer; const Value: TG2Color);
begin
  with Value do
  PDWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 4)^ := (
    (b shl 2) or (g shl 12) or (r shl 22) or ((a and $3) shl 24)
  );
end;

function TG2TextureSurface.GetPixelA8R3G3B2(const X, Y: Integer): TG2Color;
var
  c: Word;
begin
  c := PWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 2)^;
  with Result do
  begin
    b := (c and $3) shl 7;
    g := (c and $1c) shl 3;
    r := (c and $e0);
    a := (c and $ff00) shr 8;
  end;
end;

procedure TG2TextureSurface.SetPixelA8R3G3B2(const X, Y: Integer; const Value: TG2Color);
begin
  with Value do
  PByte(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X)^ := (
    ((b and $c0) shr 7) or ((g and $e0) shr 3) or (r and $e0) or (a shl 8)
  );
end;

function TG2TextureSurface.GetPixelA8(const X, Y: Integer): TG2Color;
begin
  with Result do
  begin
    r := $ff; g := $ff; b := $ff;
    a := PByte(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X)^;
  end;
end;

procedure TG2TextureSurface.SetPixelA8(const X, Y: Integer; const Value: TG2Color);
begin
  PByte(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X)^ := Value.a;
end;

function TG2TextureSurface.GetPixelR3G3B2(const X, Y: Integer): TG2Color;
var
  c: Byte;
begin
  c := PByte(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X)^;
  with Result do
  begin
    b := (c and $3) shl 7;
    g := (c and $1c) shl 3;
    r := (c and $e0);
    a := $ff;
  end;
end;

procedure TG2TextureSurface.SetPixelR3G3B2(const X, Y: Integer; const Value: TG2Color);
begin
  with Value do
  PByte(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X)^ := (
    ((b and $c0) shr 7) or ((g and $e0) shr 3) or (r and $e0)
  );
end;

function TG2TextureSurface.GetPixelA1R5G5B5(const X, Y: Integer): TG2Color;
var
  c: Word;
begin
  c := PWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 2)^;
  with Result do
  begin
    b := (c and $1f) shl 3;
    g := (c and $3e0) shr 2;
    r := (c and $7c00) shr 7;
    a := (c and $8000) shr 8;
  end;
end;

procedure TG2TextureSurface.SetPixelA1R5G5B5(const X, Y: Integer; const Value: TG2Color);
begin
  with Value do
  PWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 2)^ := (
    ((b and $f8) shr 3) or ((g and $f8) shl 2) or ((r and $f8) shl 7) or ((a and $80) shl 8)
  );
end;

function TG2TextureSurface.GetPixelR5G6B5(const X, Y: Integer): TG2Color;
var
  c: Word;
begin
  c := PWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 2)^;
  with Result do
  begin
    b := (c and $1f) shl 3;
    g := (c and $7e0) shr 3;
    r := (c and $f800) shr 8;
    a := $ff;
  end;
end;

procedure TG2TextureSurface.SetPixelR5G6B5(const X, Y: Integer; const Value: TG2Color);
begin
  with Value do
  PWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 2)^ := (
    ((b and $f8) shr 3) or ((g and $fc) shl 3) or ((r and $f8) shl 8)
  );
end;

function TG2TextureSurface.GetPixelA4R4G4B4(const X, Y: Integer): TG2Color;
var
  c: Word;
begin
  c := PWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 2)^;
  with Result do
  begin
    b := (c shl 4) and $f0;
    g := (c and $f0);
    r := (c shr 4) and $f0;
    a := (c shr 8) and $f0;
  end;
end;

procedure TG2TextureSurface.SetPixelA4R4G4B4(const X, Y: Integer; const Value: TG2Color);
begin
  with Value do
  PWord(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 2)^ := (
    (b shr 4) or (g and $f0) or ((r and $f0) shl 4) or ((a and $f0) shl 8)
  );
end;

function TG2TextureSurface.GetPixelR8G8B8(const X, Y: Integer): TG2Color;
begin
  Move(PByte(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 3)^, Result, 3);
  Result.a := $ff;
end;

procedure TG2TextureSurface.SetPixelR8G8B8(const X, Y: Integer; const Value: TG2Color);
begin
  Move(Value, PByte(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 3)^, 3);
end;

function TG2TextureSurface.GetPixelA8R8G8B8(const X, Y: Integer): TG2Color;
begin
  Result := PG2Color(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 4)^;
end;

procedure TG2TextureSurface.SetPixelA8R8G8B8(const X, Y: Integer; const Value: TG2Color);
begin
  PG2Color(Cardinal(m_Lock.pBits) + Y * m_Lock.Pitch + X * 4)^ := Value;
end;

function TG2TextureSurface.GetPixelDummy(const X, Y: Integer): TG2Color;
begin
  Result := 0;
end;

procedure TG2TextureSurface.SetPixelDummy(const X, Y: Integer; const Value: TG2Color);
begin

end;
{$Warnings On}

function TG2TextureSurface.GetPixel(const X, Y: Integer): TG2Color;
begin
  Result := m_ProcGetPixel(X, Y);
end;

procedure TG2TextureSurface.SetPixel(const X, Y: Integer; const Value: TG2Color);
begin
  m_ProcSetPixel(X, Y, Value);
end;

function TG2TextureSurface.GetPixelLerp(const X, Y: Single): TG2Color;
var
  IntX1, IntY1, IntX2, IntY2: Integer;
  FracX, FracY: Single;
  c1, c2: TG2Color;
begin
  IntX1 := Trunc(X) mod m_Width;
  IntY1 := Trunc(Y) mod m_Height;
  IntX2 := (IntX1 + 1) mod m_Width;
  IntY2 := (IntY1 + 1) mod m_Height;
  FracX := Frac(X);
  FracY := Frac(Y);
  c1 := G2LerpColor(GetPixel(IntX1, IntY1), GetPixel(IntX2, IntY1), FracX);
  c2 := G2LerpColor(GetPixel(IntX1, IntY2), GetPixel(IntX2, IntY2), FracX);
  Result := G2LerpColor(c1, c2, FracY);
end;

function TG2TextureSurface.GetSurfaceDesc: PD3DSurfaceDesc;
begin
  Result := @m_Desc;
end;

function TG2TextureSurface.GetLockedRect: PD3DLockedRect;
begin
  Result := @m_Lock;
end;

function TG2TextureSurface.Lock(const Flags: DWord = 0): TG2Result;
begin
  Result := grFail;
  if m_Locked then Exit;
  if m_Texture is TG2Texture2D then
  begin
    if Failed(
      IDirect3DTexture9(
        m_Texture.Texture
      ).GetLevelDesc(
        m_Level,
        m_Desc
      )
    ) then
    Exit;
    if Failed(
      IDirect3DTexture9(
        m_Texture.Texture
      ).LockRect(
        m_Level,
        m_Lock,
        nil,
        Flags
      )
    ) then
    Exit;
  end
  else if m_Texture is TG2TextureCube then
  begin
    if Failed(
      IDirect3DCubeTexture9(
        m_Texture.Texture
      ).GetLevelDesc(
        m_Level,
        m_Desc
      )
    ) then
    Exit;
    if Failed(
      IDirect3DCubeTexture9(
        m_Texture.Texture
      ).LockRect(
        m_Face,
        m_Level,
        m_Lock,
        nil,
        Flags
      )
    ) then
    Exit;
  end
  else
  Exit;
  case m_Desc.Format of
    D3DFMT_A8R8G8B8, D3DFMT_X8R8G8B8:
    begin
      m_ProcGetPixel := GetPixelA8R8G8B8;
      m_ProcSetPixel := SetPixelA8R8G8B8;
    end;
    D3DFMT_R8G8B8:
    begin
      m_ProcGetPixel := GetPixelR8G8B8;
      m_ProcSetPixel := SetPixelR8G8B8;
    end;
    D3DFMT_A4R4G4B4, D3DFMT_X4R4G4B4:
    begin
      m_ProcGetPixel := GetPixelA4R4G4B4;
      m_ProcSetPixel := SetPixelA4R4G4B4;
    end;
    D3DFMT_R5G6B5:
    begin
      m_ProcGetPixel := GetPixelR5G6B5;
      m_ProcSetPixel := SetPixelR5G6B5;
    end;
    D3DFMT_A1R5G5B5, D3DFMT_X1R5G5B5:
    begin
      m_ProcGetPixel := GetPixelA1R5G5B5;
      m_ProcSetPixel := SetPixelA1R5G5B5;
    end;
    D3DFMT_R3G3B2:
    begin
      m_ProcGetPixel := GetPixelR3G3B2;
      m_ProcSetPixel := SetPixelR3G3B2;
    end;
    D3DFMT_A8:
    begin
      m_ProcGetPixel := GetPixelA8;
      m_ProcSetPixel := SetPixelA8;
    end;
    D3DFMT_A8R3G3B2:
    begin
      m_ProcGetPixel := GetPixelA8R3G3B2;
      m_ProcSetPixel := SetPixelA8R3G3B2;
    end;
    D3DFMT_A2B10G10R10:
    begin
      m_ProcGetPixel := GetPixelA2B10G10R10;
      m_ProcSetPixel := SetPixelA2B10G10R10;
    end;
    D3DFMT_G16R16:
    begin
      m_ProcGetPixel := GetPixelG16R16;
      m_ProcSetPixel := SetPixelG16R16;
    end;
    D3DFMT_A2R10G10B10:
    begin
      m_ProcGetPixel := GetPixelA2R10G10B10;
      m_ProcSetPixel := SetPixelA2R10G10B10;
    end;
    D3DFMT_A16B16G16R16:
    begin
      m_ProcGetPixel := GetPixelA16B16G16R16;
      m_ProcSetPixel := SetPixelA16B16G16R16;
    end;
    else
    begin
      m_ProcGetPixel := GetPixelDummy;
      m_ProcSetPixel := SetPixelDummy;
    end;
  end;
  m_Locked := True;
  Result := grOk;
end;

function TG2TextureSurface.UnLock: TG2Result;
begin
  Result := grFail;
  if not m_Locked then Exit;
  if m_Texture is TG2Texture2D then
  begin
    if Failed(
      IDirect3DTexture9(
        m_Texture.Texture
      ).UnlockRect(
        m_Level
      )
    ) then
    Exit;
  end
  else if m_Texture is TG2TextureCube then
  begin
    if Failed(
      IDirect3DCubeTexture9(
        m_Texture.Texture
      ).UnlockRect(
        m_Face,
        m_Level
      )
    ) then
    Exit;
  end;
  m_ProcSetPixel := SetPixelDummy;
  m_ProcGetPixel := GetPixelDummy;
  m_Locked := False;
  Result := grOk;
end;
//TG2TextureSurface END

//TG2TextureBase BEGIN
constructor TG2TextureBase.Create;
begin
  inherited Create;
end;

destructor TG2TextureBase.Destroy;
begin
  inherited Destroy;
end;

function TG2TextureBase.MaxMipLevels(const Width, Height, Depth: Integer): Integer;
var
  rw, rh, rd, mipu, mipv, mipw: Integer;
begin
  rw := 1;
  rh := 1;
  rd := 1;
  mipu := 1;
  mipv := 1;
  mipw := 1;
  while rw < Width do
  begin
    rw := rw shl 1;
    Inc(mipu);
  end;
  while rh < Height do
  begin
    rh := rh shl 1;
    Inc(mipv);
  end;
  while rd < Depth do
  begin
    rd := rd shl 1;
    Inc(mipw);
  end;
  Result := Max(Max(mipu, mipv), mipw);
end;

procedure TG2TextureBase.OnDeviceLost;
begin

end;

procedure TG2TextureBase.OnDeviceReset;
begin

end;

function TG2TextureBase.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Gfx := Core.Graphics;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(+) Texture Initialized: (' + Name + ').'), 'Textures');
end;

function TG2TextureBase.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Release;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(-) Texture Finalized: (' + Name + ').'), 'Textures');
end;
//TG2TextureBase END

//TG2Texture2DBase BEGIN
constructor TG2Texture2DBase.Create;
begin
  inherited Create;
  m_Width := 0;
  m_Height := 0;
  m_RealWidth := 0;
  m_RealHeight := 0;
  m_DrawRect := G2Rect(0, 0, 0, 0);
  m_Levels := 0;
  ZeroMemory(@m_Desc, SizeOf(m_Desc));
end;

destructor TG2Texture2DBase.Destroy;
begin
  inherited Destroy;
end;

function TG2Texture2DBase.GetDrawRect: PG2Rect;
begin
  Result := @m_DrawRect;
end;

procedure TG2Texture2DBase.SetDrawRect;
begin
  m_DrawRect := G2Rect(0, 0, m_Width / m_RealWidth, m_Height / m_RealHeight);
end;

function TG2Texture2DBase.GetTexture: IDirect3DBaseTexture9;
begin
  Result := m_Texture;
end;

procedure TG2Texture2DBase.SetTexture(const Value: IDirect3DBaseTexture9);
begin
  Release;
  m_Texture := IDirect3DTexture9(Value);
  m_Texture.GetLevelDesc(0, m_Desc);
  m_Levels := m_Texture.GetLevelCount;
  m_Format := m_Desc.Format;
  m_RealWidth := m_Desc.Width;
  m_RealHeight := m_Desc.Height;
  m_Width := m_RealWidth;
  m_Height := m_RealHeight;
  m_DrawRect := G2Rect(0, 0, 1, 1);
end;

function TG2Texture2DBase.GetProjMatrix: TG2Mat;
var
  OffsetX, OffsetY: Single;
begin
  OffsetX := 0.5 + (0.5 / m_RealWidth);
  OffsetY := 0.5 + (0.5 / m_RealHeight);
  ZeroMemory(@Result, SizeOf(TG2Mat));
  Result.e00 := 0.5;
  Result.e11 := -0.5;
  Result.e22 := 1;
  Result.e30 := OffsetX;
  Result.e31 := OffsetY;
  Result.e33 := 1.0;
end;

function TG2Texture2DBase.GetTexelSize: TG2Vec2;
begin
  Result.SetValue(1 / m_RealWidth, 1 / m_RealHeight);
end;

function TG2Texture2DBase.SaveToFile(const FileName: String): TG2Result;
var
  RTSurface: IDirect3DSurface9;
  TexSurface: IDirect3DSurface9;
  fmt: TD3DXImageFileFormat;
  ext: String;
  R: TRect;
begin
  Result := grFail;
  ext := UpperCase(ExtractFileExt(FileName));
  Delete(ext, 1, 1);
  if ext = 'BMP' then fmt := D3DXIFF_BMP
  else if (ext = 'JPG') or (ext = 'JPEG') then fmt := D3DXIFF_JPG
  else if ext = 'TGA' then fmt := D3DXIFF_TGA
  else if ext = 'PNG' then fmt := D3DXIFF_PNG
  else if ext = 'PPM' then fmt := D3DXIFF_PPM
  else if ext = 'DIB' then fmt := D3DXIFF_DIB
  else if ext = 'HDR' then fmt := D3DXIFF_HDR
  else if ext = 'PFM' then fmt := D3DXIFF_PFM
  else fmt := D3DXIFF_DDS;
  if Self is TG2Texture2DRT then
  begin
    if Failed(
      m_Texture.GetSurfaceLevel(0, RTSurface)
    ) then Exit;
    if Failed(
      m_Gfx.Device.CreateOffscreenPlainSurface(
        m_RealWidth,
        m_RealHeight,
        m_Format,
        D3DPOOL_SYSTEMMEM,
        TexSurface,
        nil
      )
    ) then
    begin
      SafeRelease(RTSurface);
      Exit;
    end;
    if Failed(
      m_Gfx.Device.GetRenderTargetData(
        RTSurface,
        TexSurface
      )
    ) then
    begin
      SafeRelease(RTSurface);
      SafeRelease(TexSurface);
      Exit;
    end;
    SafeRelease(RTSurface);
  end
  else
  begin
    if Failed(
      m_Texture.GetSurfaceLevel(0, TexSurface)
    ) then Exit;
  end;
  R := Rect(
    Round(m_DrawRect.Left * m_RealWidth),
    Round(m_DrawRect.Top * m_RealHeight),
    Round(m_DrawRect.Right * m_RealWidth),
    Round(m_DrawRect.Bottom * m_RealHeight)
  );
  if Failed(
    D3DXSaveSurfaceToFileW(
      PWideChar(WideString(FileName)),
      fmt,
      TexSurface,
      nil,
      @R
    )
  ) then
  begin
    SafeRelease(TexSurface);
    Exit;
  end;
  SafeRelease(TexSurface);

  Result := grOk;
end;

procedure TG2Texture2DBase.Release;
begin
  if Assigned(m_Texture) then
  G2WriteLogTimed(AnsiString('(<) Texture Released (' + Name + ').'), 'Textures');
  SafeRelease(m_Texture);
  m_Width := 0;
  m_Height := 0;
  m_RealWidth := 0;
  m_RealHeight := 0;
  m_DrawRect := G2Rect(0, 0, 0, 0);
  m_Levels := 0;
  m_Format := D3DFMT_UNKNOWN;
  ZeroMemory(@m_Desc, SizeOf(m_Desc));
end;
//TG2Texture2DBase END

//TG2Texture2D BEGIN
constructor TG2Texture2D.Create;
begin
  inherited Create;
end;

destructor TG2Texture2D.Destroy;
begin
  inherited Destroy;
end;

procedure TG2Texture2D.InitLevels;
var
  i: Integer;
begin
  SetLength(m_Surfaces, m_Levels);
  for i := 0 to High(m_Surfaces) do
  m_Surfaces[i] := TG2TextureSurface.Create(Self, i);
end;

procedure TG2Texture2D.UnInitLevels;
var
  i: Integer;
begin
  for i := 0 to High(m_Surfaces) do
  m_Surfaces[i].Free;
  m_Surfaces := nil;
end;

function TG2Texture2D.GetSurface(const SurfaceLevel: Integer): TG2TextureSurface;
begin
  Result := m_Surfaces[SurfaceLevel];
end;

procedure TG2Texture2D.SetTexture(const Value: IDirect3DBaseTexture9);
begin
  inherited SetTexture(Value);
  InitLevels;
end;

function TG2Texture2D.LoadFromFile(
      const f: WideString;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  ImageInfo: TD3DXImageInfo;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTexture2D
  else
  m_Format := m_Gfx.Specs.FindCompatiableTexture2DFormat(NewFormat);

  if Failed(
    D3DXCreateTextureFromFileExW(
      m_Gfx.Device,
      PWideChar(f),
      0, 0,
      MipLevels,
      0,
      m_Format,
      D3DPOOL_MANAGED,
      D3DX_FILTER_NONE,
      D3DX_FILTER_NONE,
      0,
      @ImageInfo,
      nil,
      m_Texture
    )
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;

  m_Width := ImageInfo.Width;
  m_Height := ImageInfo.Height;

  m_Texture.GetLevelDesc(0, m_Desc);
  m_RealWidth := m_Desc.Width;
  m_RealHeight := m_Desc.Height;

  if MipLevels = 0 then
  m_Levels := MaxMipLevels(Width, Height, 1)
  else
  m_Levels := Min(MipLevels, MaxMipLevels(Width, Height, 1));

  GenerateMipMaps;

  SetDrawRect;
  InitLevels;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Loaded (' + String(Name) + '): Texture2D ' + IntToStr(m_RealWidth) + 'x' + IntToStr(m_RealHeight) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

function TG2Texture2D.LoadFromStream(
      const s: TMemoryStream;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
begin
  Result := LoadFromBuffer(s.Memory, Size, MipLevels, NewFormat);
end;

function TG2Texture2D.LoadFromBuffer(
      const b: Pointer;
      const Size: Integer;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  ImageInfo: TD3DXImageInfo;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTexture2D
  else
  m_Format := m_Gfx.Specs.FindCompatiableTexture2DFormat(NewFormat);

  if Failed(
    D3DXCreateTextureFromFileInMemoryEx(
      m_Gfx.Device,
      b,
      Size,
      0, 0,
      MipLevels,
      0,
      m_Format,
      D3DPOOL_MANAGED,
      D3DX_FILTER_NONE,
      D3DX_FILTER_NONE,
      0,
      @ImageInfo,
      nil,
      m_Texture
    )
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;

  m_Width := ImageInfo.Width;
  m_Height := ImageInfo.Height;

  m_Texture.GetLevelDesc(0, m_Desc);
  m_RealWidth := m_Desc.Width;
  m_RealHeight := m_Desc.Height;

  if MipLevels = 0 then
  m_Levels := MaxMipLevels(Width, Height, 1)
  else
  m_Levels := Min(MipLevels, MaxMipLevels(Width, Height, 1));

  GenerateMipMaps;

  SetDrawRect;
  InitLevels;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Loaded (' + String(Name) + '): Texture2D ' + IntToStr(m_RealWidth) + 'x' + IntToStr(m_RealHeight) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

function TG2Texture2D.LoadFromGraphic(
      const g: TGraphic;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  s: TMemoryStream;
begin
  s := TMemoryStream.Create;
  g.SaveToStream(s);
  Result := LoadFromStream(s, s.Size, MipLevels, NewFormat);
  s.Free;
end;

function TG2Texture2D.LoadFromPack(
      const FolderName, FileName: AnsiString;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  Data: Pointer;
  DataSize: DWord;
begin
  Core.PackLinker.GetFileData(FolderName, FileName, Data, DataSize);
  if Assigned(Data) then
  Result := LoadFromBuffer(Data, DataSize, MipLevels, NewFormat)
  else
  Result := grFail;
end;

function TG2Texture2D.LoadFromTexture2D(
      const Tex: TG2Texture2DBase;
      const MipLevels: Integer = 8;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  SrcSurface: IDirect3DSurface9;
  DstSurface: IDirect3DSurface9;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTexture2D
  else
  m_Format := m_Gfx.Specs.FindCompatiableTexture2DFormat(NewFormat);

  m_Width := Tex.Width;
  m_Height := Tex.Height;
  m_RealWidth := Tex.RealWidth;
  m_RealHeight := Tex.RealHeight;

  if MipLevels = 0 then
  m_Levels := MaxMipLevels(Width, Height, 1)
  else
  m_Levels := Min(MipLevels, MaxMipLevels(Width, Height, 1));

  if Failed(
    m_Gfx.Device.CreateTexture(
      m_RealWidth, m_RealHeight,
      m_Levels,
      0,
      m_Format,
      D3DPOOL_MANAGED,
      m_Texture,
      nil
    )
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;

  if Failed(
    IDirect3DTexture9(Tex.Texture).GetSurfaceLevel(0, SrcSurface)
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;

  if Failed(
    m_Texture.GetSurfaceLevel(0, DstSurface)
  ) then
  begin
    SafeRelease(SrcSurface);
    Release;
    Result := grFail;
    Exit;
  end;

  if Failed(
    D3DXLoadSurfaceFromSurface(
      DstSurface,
      nil,
      nil,
      SrcSurface,
      nil,
      nil,
      D3DX_FILTER_TRIANGLE,
      0
    )
  ) then
  begin
    SafeRelease(SrcSurface);
    SafeRelease(DstSurface);
    Release;
    Result := grFail;
    Exit;
  end;
  SafeRelease(SrcSurface);
  SafeRelease(DstSurface);

  m_Texture.GetLevelDesc(0, m_Desc);

  GenerateMipMaps;

  SetDrawRect;
  InitLevels;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Loaded (' + String(Name) + '): Texture2D ' + IntToStr(m_RealWidth) + 'x' + IntToStr(m_RealHeight) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

function TG2Texture2D.LoadNormalMap(
      const HeightMap: TG2Texture2D;
      const Amplitude: Single = 1;
      const MipLevels: Integer = 8;
      const Channel: DWord = D3DX_CHANNEL_ALPHA;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTexture2D
  else
  m_Format := m_Gfx.Specs.FindCompatiableTexture2DFormat(NewFormat);

  m_Width := HeightMap.Width;
  m_Height := HeightMap.Height;
  m_RealWidth := HeightMap.RealWidth;
  m_RealHeight := HeightMap.RealHeight;

  if MipLevels = 0 then
  m_Levels := MaxMipLevels(m_Width, m_Height, 1)
  else
  m_Levels := Min(MipLevels, MaxMipLevels(m_Width, m_Height, 1));

  if Failed(
    m_Gfx.Device.CreateTexture(
      m_RealWidth, m_RealHeight,
      m_Levels,
      0,
      m_Format,
      D3DPOOL_MANAGED,
      m_Texture,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;

  if Failed(
    D3DXComputeNormalMap(
      m_Texture,
      IDirect3DTexture9(HeightMap.Texture),
      nil,
      0,
      Channel,
      Amplitude
    )
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;

  m_Texture.GetLevelDesc(0, m_Desc);

  GenerateMipMaps;

  SetDrawRect;
  InitLevels;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Loaded (' + String(Name) + '): Texture2D ' + IntToStr(m_RealWidth) + 'x' + IntToStr(m_RealHeight) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

function TG2Texture2D.MakeTexture(
      const NewWidth: Integer;
      const NewHeight: Integer;
      const MipLevels: Integer = 8;
      const Usage: DWord = 0;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN;
      const Pool: TD3DPool = D3DPOOL_MANAGED
    ): TG2Result;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTexture2D
  else
  m_Format := m_Gfx.Specs.FindCompatiableTexture2DFormat(NewFormat);

  m_Width := NewWidth;
  m_Height := NewHeight;
  m_RealWidth := 4; while m_RealWidth < m_Width do m_RealWidth := m_RealWidth shl 1;
  m_RealHeight := 4; while m_RealHeight < m_Height do m_RealHeight := m_RealHeight shl 1;

  if MipLevels = 0 then
  m_Levels := MaxMipLevels(Width, Height, 1)
  else
  m_Levels := Min(MipLevels, MaxMipLevels(Width, Height, 1));

  if Failed(
    m_Gfx.Device.CreateTexture(
      m_RealWidth, m_RealHeight,
      m_Levels,
      Usage,
      m_Format,
      Pool,
      m_Texture,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;

  m_Texture.GetLevelDesc(0, m_Desc);

  SetDrawRect;
  InitLevels;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Created (' + String(Name) + '): Texture2D ' + IntToStr(m_RealWidth) + 'x' + IntToStr(m_RealHeight) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

function TG2Texture2D.LoadAlpha(
      const Tex: TG2Texture2D;
      const Channel: DWord = D3DX_CHANNEL_ALPHA
    ): TG2Result;
var
  i, j: Integer;
  c1, c2: TG2Color;
  fc1, fc2: Boolean;
  fmt1, fmt2: TD3DFormat;
  CInd: Integer;
begin
  if not Assigned(m_Texture)
  or not Assigned(Tex.Texture) then
  begin
    Result := grFail;
    Exit;
  end;

  fmt1 := Format;
  fmt2 := Tex.Format;
  fc1 := False;
  fc2 := False;

  if (fmt1 = D3DFMT_DXT1)
  or (fmt1 = D3DFMT_DXT2)
  or (fmt1 = D3DFMT_DXT3)
  or (fmt1 = D3DFMT_DXT4)
  or (fmt1 = D3DFMT_DXT5) then
  begin
    fc1 := True;
    Result := ChangeFormat(D3DFMT_A8R8G8B8);
    if G2ResFail(Result) then Exit;
  end;

  if (fmt2 = D3DFMT_DXT1)
  or (fmt2 = D3DFMT_DXT2)
  or (fmt2 = D3DFMT_DXT3)
  or (fmt2 = D3DFMT_DXT4)
  or (fmt2 = D3DFMT_DXT5) then
  begin
    fc2 := True;
    Result := Tex.ChangeFormat(D3DFMT_A8R8G8B8);
    if G2ResFail(Result) then
    begin
      if fc1 then ChangeFormat(fmt1);
      Exit;
    end;
  end;

  case Channel of
    D3DX_CHANNEL_RED: CInd := 2;
    D3DX_CHANNEL_GREEN: CInd := 1;
    D3DX_CHANNEL_BLUE: CInd := 0;
    else CInd := 3;
  end;

  Surfaces[0].Lock;
  Tex.Surfaces[0].Lock;
  for j := 0 to m_Height - 1 do
  for i := 0 to m_Width - 1 do
  begin
    c1 := Surfaces[0].Pixels[i, j];
    c2 := Tex.Surfaces[0].Pixels[
      Round(i / (m_Width - 1) * (Tex.Width - 1)),
      Round(j / (m_Height - 1) * (Tex.Height - 1))
    ];
    c1.a := PByteArray(@c2)^[CInd];
    Surfaces[0].Pixels[i, j] := c1;
  end;
  Tex.Surfaces[0].UnLock;
  Surfaces[0].UnLock;

  if fc1 then ChangeFormat(fmt1)
  else
  GenerateMipMaps;
  if fc2 then ChangeFormat(fmt2);

  Result := grOk;
end;

function TG2Texture2D.ChangeFormat(
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  NewTex: IDirect3DTexture9;
  OldSurface: IDirect3DSurface9;
  NewSurface: IDirect3DSurface9;
begin
  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTexture2D
  else
  m_Format := m_Gfx.Specs.FindCompatiableTexture2DFormat(NewFormat);

  if Failed(
    m_Gfx.Device.CreateTexture(
      m_RealWidth, m_RealHeight,
      m_Levels,
      0,
      m_Format,
      D3DPOOL_MANAGED,
      NewTex,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;

  if Failed(
    m_Texture.GetSurfaceLevel(0, OldSurface)
  ) then
  begin
    SafeRelease(NewTex);
    Result := grFail;
    Exit;
  end;

  if Failed(
    NewTex.GetSurfaceLevel(0, NewSurface)
  ) then
  begin
    SafeRelease(OldSurface);
    SafeRelease(NewTex);
    Result := grFail;
    Exit;
  end;

  if Failed(
    D3DXLoadSurfaceFromSurface(
      NewSurface,
      nil,
      nil,
      OldSurface,
      nil,
      nil,
      D3DX_FILTER_TRIANGLE,
      0
    )
  ) then
  begin
    SafeRelease(NewSurface);
    SafeRelease(OldSurface);
    SafeRelease(NewTex);
    Result := grFail;
    Exit;
  end;
  SafeRelease(NewSurface);
  SafeRelease(OldSurface);
  SafeRelease(m_Texture);
  m_Texture := NewTex;
  SafeRelease(NewTex);

  m_Texture.GetLevelDesc(0, m_Desc);

  GenerateMipMaps;

  Result := grOk;
end;

function TG2Texture2D.ChangeColor(
      const OldColor, NewColor: TG2Color
    ): TG2Result;
var
  l, i, j: Integer;
begin
  Result := grOk;
  for l := 0 to m_Levels - 1 do
  begin
    if G2ResFail(m_Surfaces[l].Lock) then
    begin
      Result := grFail;
      Exit;
    end;
    for j := 0 to m_Height - 1 do
    for i := 0 to m_Width - 1 do
    if m_Surfaces[l].Pixels[i, j] = OldColor then
    m_Surfaces[l].Pixels[i, j] := NewColor;
    m_Surfaces[l].UnLock;
  end;
end;

procedure TG2Texture2D.GenerateMipMaps;
begin
  if m_Levels > 1 then
  D3DXFilterTexture(
    m_Texture,
    nil,
    0,
    D3DX_FILTER_BOX
  );
end;

procedure TG2Texture2D.Release;
begin
  UnInitLevels;
  inherited Release;
end;
//TG2Texture2D END

//TG2Texture2DRT BEGIN
constructor TG2Texture2DRT.Create;
begin
  inherited Create;
end;

destructor TG2Texture2DRT.Destroy;
begin
  inherited Destroy;
end;

procedure TG2Texture2DRT.OnDeviceLost;
begin
  m_SurfaceRT.Release;
  SafeRelease(m_Texture);
end;

procedure TG2Texture2DRT.OnDeviceReset;
var
  RenderTargetSurface: IDirect3DSurface9;
begin
  if Succeeded(
    m_Gfx.Device.CreateTexture(
      m_RealWidth,
      m_RealHeight,
      1,
      D3DUSAGE_RENDERTARGET,
      m_Format,
      D3DPOOL_DEFAULT,
      m_Texture,
      nil
    )
  ) then
  begin
    m_Texture.GetSurfaceLevel(0, RenderTargetSurface);
    m_SurfaceRT.Surface := RenderTargetSurface;
  end;
end;

function TG2Texture2DRT.MakeRenderTarget(
      const NewWidth, NewHeight: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  RenderTargetSurface: IDirect3DSurface9;
begin
  Release;

  m_Width := NewWidth;
  m_Height := NewHeight;
  m_RealWidth := 1;
  m_RealHeight := 1;
  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTexture2DRT
  else
  m_Format := m_Gfx.Specs.FindCompatiableTexture2DRTFormat(NewFormat);
  while m_RealWidth < m_Width do m_RealWidth := m_RealWidth shl 1;
  while m_RealHeight < m_Height do m_RealHeight := m_RealHeight shl 1;

  if Failed(
    m_Gfx.Device.CreateTexture(
      m_RealWidth,
      m_RealHeight,
      1,
      D3DUSAGE_RENDERTARGET,
      m_Format,
      D3DPOOL_DEFAULT,
      m_Texture,
      nil
    )
  ) then
  begin
    Result := grFail;
    Release;
    Exit;
  end;

  m_Texture.GetSurfaceLevel(0, RenderTargetSurface);
  m_SurfaceRT.Surface := RenderTargetSurface;

  m_Texture.GetLevelDesc(0, m_Desc);
  SetDrawRect;

  m_Levels := 1;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture RenderTarget (' + String(Name) + '): Texture2DRT ' + IntToStr(m_RealWidth) + 'x' + IntToStr(m_RealHeight) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

procedure TG2Texture2DRT.Release;
begin
  m_SurfaceRT.Release;
  inherited Release;
end;

function TG2Texture2DRT.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_SurfaceRT := TG2SurfaceRT.Create;
  m_SurfaceRT.Initialize(Core);
  Result := grOk;
end;

function TG2Texture2DRT.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  m_SurfaceRT.Finalize;
  m_SurfaceRT.Free;
  Result := grOk;
end;
//TG2Texture2DRT END

//TG2Texture2DDS BEGIN
constructor TG2Texture2DDS.Create;
begin
  inherited Create;
  m_DepthStencil := TG2SurfaceDS.Create;
end;

destructor TG2Texture2DDS.Destroy;
begin
  m_DepthStencil.Free;
  inherited Destroy;
end;

procedure TG2Texture2DDS.OnDeviceLost;
begin
  m_DepthStencil.Release;
  SafeRelease(m_Texture);
end;

procedure TG2Texture2DDS.OnDeviceReset;
var
  DepthStencilSurface: IDirect3DSurface9;
begin
  if Succeeded(
    m_Gfx.Device.CreateTexture(
      m_RealWidth,
      m_RealHeight,
      1,
      D3DUSAGE_DEPTHSTENCIL,
      m_Format,
      D3DPOOL_DEFAULT,
      m_Texture,
      nil
    )
  ) then
  begin
    m_Texture.GetSurfaceLevel(0, DepthStencilSurface);
    m_DepthStencil.Surface := DepthStencilSurface;
  end;
end;

function TG2Texture2DDS.MakeDepthStencil(
      const NewWidth, NewHeight: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  DepthStencilSurface: IDirect3DSurface9;
begin
  Release;

  m_Width := NewWidth;
  m_Height := NewHeight;
  m_RealWidth := 1;
  m_RealHeight := 1;
  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTexture2DDS
  else
  m_Format := m_Gfx.Specs.FindCompatiableTexture2DDSFormat(NewFormat);
  while m_RealWidth < m_Width do m_RealWidth := m_RealWidth shl 1;
  while m_RealHeight < m_Height do m_RealHeight := m_RealHeight shl 1;

  if Failed(
    m_Gfx.Device.CreateTexture(
      m_RealWidth,
      m_RealHeight,
      1,
      D3DUSAGE_DEPTHSTENCIL,
      m_Format,
      D3DPOOL_DEFAULT,
      m_Texture,
      nil
    )
  ) then
  begin
    Result := grFail;
    Release;
    Exit;
  end;

  m_Texture.GetSurfaceLevel(0, DepthStencilSurface);
  m_DepthStencil.Surface := DepthStencilSurface;

  m_Texture.GetLevelDesc(0, m_Desc);
  m_DrawRect := G2Rect(0, 0, m_Width / m_RealWidth, m_Height / m_RealHeight);

  m_Levels := 1;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture DepthStencil (' + String(Name) + '): Texture2DDS ' + IntToStr(m_RealWidth) + 'x' + IntToStr(m_RealHeight) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

procedure TG2Texture2DDS.Release;
begin
  m_DepthStencil.Release;
  inherited Release;
end;

function TG2Texture2DDS.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_DepthStencil.Initialize(Core);
  Result := grOk;
end;

function TG2Texture2DDS.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  m_DepthStencil.Finalize;
  Result := grOk;
end;
//TG2Texture2DRT END

//TG2Texture2DVideo BEGIN
constructor TG2Texture2DVideo.TG2VMRAllocatorPresenter.Create(const Texture: TG2Texture2DVideo);
begin
  inherited Create;
  m_Texture := Texture;
  m_CriticalSection := TCriticalSection.Create;
end;

destructor TG2Texture2DVideo.TG2VMRAllocatorPresenter.Destroy;
begin
  m_CriticalSection.Free;
  inherited Destroy;
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.CreateSurfaces: HResult;
begin
  FreeSurfaces;
  SetLength(m_Surfaces, m_SurfaceCount);
  Result := m_SurfaceAllocatorNotify.AllocateSurfaceHelper(
    @m_AllocInfo, m_SurfaceCount, m_Surfaces[0]
  );
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.CreateTexture: HResult;
var
  rt: IDirect3DSurface9;
  TextureSurface: IDirect3DSurface9;
begin
  Result := E_FAIL;
  if G2ResFail(
    m_Texture.MakeTexture(
      m_AllocInfo.dwWidth,
      m_AllocInfo.dwHeight
    )
  ) then Exit;
  m_Texture.DrawRect^ := G2Rect(0, 0, 1, 1);
  m_Texture.Core.Graphics.Device.GetRenderTarget(0, rt);
  IDirect3DTexture9(m_Texture.Texture).GetSurfaceLevel(0, TextureSurface);
  m_Texture.Core.Graphics.Device.SetRenderTarget(0, TextureSurface);
  m_Texture.Core.Graphics.Device.Clear(0, nil, D3DCLEAR_TARGET, $ff000000, 0, 0);
  m_Texture.Core.Graphics.Device.SetRenderTarget(0, rt);
  SafeRelease(TextureSurface);
  SafeRelease(rt);
  Result := S_OK;
end;

procedure TG2Texture2DVideo.TG2VMRAllocatorPresenter.FreeSurfaces;
var
  i: integer;
begin
  for i := 0 to High(m_Surfaces) do
  SafeRelease(m_Surfaces[i]);
  m_Surfaces := nil;
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.InitializeDevice(
      dwUserID: DWORD;
      lpAllocInfo: PVMR9AllocationInfo;
      var lpNumBuffers: DWORD
    ): HResult;
var
  Mon: HMONITOR;
begin
  m_CriticalSection.Enter;
  try
    Mon := m_Texture.Core.Graphics.D3D9.GetAdapterMonitor(D3DADAPTER_DEFAULT);
    Result := m_SurfaceAllocatorNotify.SetD3DDevice(
      m_Texture.Core.Graphics.Device,
      Mon
    );
    if Failed(Result) then Exit;
    m_SurfaceCount := lpNumBuffers;
    lpAllocInfo^.dwFlags := VMR9AllocFlag_OffscreenSurface;
    Move(lpAllocInfo^, m_AllocInfo, SizeOf(TVMRAllocationInfo));
    Result := CreateSurfaces;
    if Failed(Result) then Exit;
    Result := CreateTexture;
  finally
    m_CriticalSection.Leave;
  end;
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.TerminateDevice(
      dwID: DWORD
    ): HResult;
begin
  m_CriticalSection.Enter;
  try
    FreeSurfaces;
    Result := S_OK;
  finally
    m_CriticalSection.Leave;
  end;
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.GetSurface(
      dwUserID: DWORD;
      SurfaceIndex: DWORD;
      SurfaceFlags: DWORD;
      out lplpSurface: IDirect3DSurface9
    ): HResult;
begin
  m_CriticalSection.Enter;
  try
    try
      if m_Texture.Core.Graphics.Device.TestCooperativeLevel = S_OK then
      begin
        SafeRelease(lplpSurface);
        lplpSurface := m_Surfaces[SurfaceIndex];
        Result := S_OK;
      end
      else
      Result := E_FAIL;
    except
      Result := E_FAIL;
    end;
  finally
    m_CriticalSection.Leave;
  end;
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.AdviseNotify(
      lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9
    ): HResult;
begin
  m_CriticalSection.Enter;
  try
    m_SurfaceAllocatorNotify := lpIVMRSurfAllocNotify;
    Result := S_OK;
  finally
    m_CriticalSection.Leave;
  end;
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.StartPresenting(
      dwUserID: DWORD
    ): HResult;
begin
  Result := S_OK;
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.StopPresenting(
      dwUserID: DWORD
    ): HResult;
begin
  Result := S_OK;
end;

function TG2Texture2DVideo.TG2VMRAllocatorPresenter.PresentImage(
      dwUserID: DWORD;
      lpPresInfo: PVMR9PresentationInfo
    ): HResult;
var
  TexSurface: IDirect3DSurface9;
begin
  m_CriticalSection.Enter;
  try
    if ((lpPresInfo.dwFlags and VMR9Sample_TimeValid) = VMR9Sample_TimeValid)
    and (lpPresInfo.rtStart > 5000000)
    and (m_Texture.Core.Graphics.Device.TestCooperativeLevel = S_OK) then
    begin
      IDirect3DTexture9(m_Texture.Texture).GetSurfaceLevel(0, TexSurface);
      m_Texture.Core.Graphics.Device.StretchRect(
        lpPresInfo^.lpSurf, nil, TexSurface, nil, D3DTEXF_LINEAR
      );
      SafeRelease(TexSurface);
    end;
    Result := S_OK;
  finally
    m_CriticalSection.Leave;
  end;
end;

constructor TG2Texture2DVideo.Create;
begin
  inherited Create;
  m_Loaded := False;
end;

destructor TG2Texture2DVideo.Destroy;
begin
  Release;
  inherited Destroy;
end;

function TG2Texture2DVideo.AddFilter(
      CLSID: TGUID;
      FilterName: WideString
    ): IBaseFilter;
begin
  if Failed(
    CoCreateInstance(
      CLSID,
      nil,
      CLSCTX_INPROC_SERVER,
      IID_IBaseFilter,
      Result
    )
  ) then Exit;
  if Failed(m_Graph.AddFilter(Result, PWideChar(FilterName))) then
  SafeRelease(Result);
end;

procedure TG2Texture2DVideo.ClearGraph;
  var Filter: IBaseFilter;
  var Enum: IEnumFilters;
begin
  if Assigned(m_Control) then
  m_Control.Stop;
  if Assigned(m_Graph)
  and Succeeded(m_Graph.EnumFilters(Enum)) then
  begin
    while Enum.Next(1, Filter, nil) = S_OK do
    begin
      m_Graph.RemoveFilter(Filter);
      SafeRelease(Filter);
      Enum.Reset;
    end;
  end;
end;

procedure TG2Texture2DVideo.CompleteGraph;
  var Filter: IBaseFilter;
  var Enum: IEnumFilters;
  var Pin, PinIn: IPin;
  var EnumPins: IEnumPins;
  var PinInfo: TPinInfo;
begin
  if Assigned(m_Graph)
  and Succeeded(m_Graph.EnumFilters(Enum)) then
  begin
    while Enum.Next(1, Filter, nil) = S_OK do
    begin
      Filter.EnumPins(EnumPins);
      while EnumPins.Next(1, Pin, nil) = S_OK do
      begin
        Pin.QueryPinInfo(PinInfo);
        if PinInfo.dir = PINDIR_OUTPUT then
        begin
          Pin.ConnectedTo(PinIn);
          if not Assigned(PinIn) then
          begin
            try
              m_Graph.Render(Pin);
            finally
            end;
          end;
          SafeRelease(PinIn);
        end;
      end;
    end;
  end;
end;

procedure TG2Texture2DVideo.SetPosition(const Value: Int64);
var
  PosCur, PosStop: Int64;
begin
  PosStop := 0;
  PosCur := Value;
  m_Seeking.SetPositions(
    PosCur,
    AM_SEEKING_AbsolutePositioning,
    PosStop,
    AM_SEEKING_NoPositioning
  );
end;

function TG2Texture2DVideo.GetPosition: Int64;
begin
  m_Seeking.GetCurrentPosition(Result);
end;

function TG2Texture2DVideo.GetDuration: Int64;
begin
  m_Seeking.GetDuration(Result);
end;

function TG2Texture2DVideo.MakeTexture(const NewWidth, NewHeight: Integer): TG2Result;
begin
  SafeRelease(m_Texture);

  m_Width := NewWidth;
  m_Height := NewHeight;
  m_RealWidth := 1;
  m_RealHeight := 1;
  m_Format := D3DFMT_X8R8G8B8;
  while m_RealWidth < m_Width do m_RealWidth := m_RealWidth shl 1;
  while m_RealHeight < m_Height do m_RealHeight := m_RealHeight shl 1;

  if Failed(
    m_Gfx.Device.CreateTexture(
      m_RealWidth,
      m_RealHeight,
      1,
      D3DUSAGE_RENDERTARGET,
      m_Format,
      D3DPOOL_DEFAULT,
      m_Texture,
      nil
    )
  ) then
  begin
    Result := grFail;
    Release;
    Exit;
  end;

  m_Texture.GetLevelDesc(0, m_Desc);
  m_DrawRect := G2Rect(0, 0, m_Width / m_RealWidth, m_Height / m_RealHeight);
  m_Levels := 1;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Video(' + String(Name) + '): Texture2DVideo ' + IntToStr(m_RealWidth) + 'x' + IntToStr(m_RealHeight)), 'Textures');
end;

function TG2Texture2DVideo.SetSurfaceAllocator: TG2Result;
var
  SurfaceAllocatorNotify: IVMRSurfaceAllocatorNotify9;
  Alloc: IVMRSurfaceAllocator9;
begin
  if Failed(
    m_VMR.QueryInterface(IID_IVMRSurfaceAllocatorNotify9, SurfaceAllocatorNotify)
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;
  if Failed(
    m_AllocatorPresenter.QueryInterface(IID_IVMRSurfaceAllocator9, Alloc)
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;
  if Failed(
    SurfaceAllocatorNotify.AdviseSurfaceAllocator(m_UsrID, Alloc)
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;
  if Failed(
    Alloc.AdviseNotify(SurfaceAllocatorNotify)
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;
  SafeRelease(SurfaceAllocatorNotify);
  Result := grOk;
end;

procedure TG2Texture2DVideo.OnDeviceLost;
begin
  if m_Loaded then
  begin
    m_PrevPos := Position;
    Release;
    m_Loaded := True;
  end;
end;

procedure TG2Texture2DVideo.OnDeviceReset;
begin
  if m_Loaded then
  begin
    StreamFile(m_FName);
    Position := m_PrevPos;
    Play;
  end;
end;

function TG2Texture2DVideo.StreamFile(const f: WideString): TG2Result;
var
  FileSourceBase: IBaseFilter;
  FileSource: IFileSourceFilter;
  FilterConfig: IVMRFilterConfig9;
  CurPin: IPin;
  FileSourcePin: IPin;
  EnumPins: IEnumPins;
  PinInfo: TPinInfo;
begin
  Release;
  m_FName := f;
  if not Core.Graphics.Params.MutiThreaded then
  begin
    G2WriteLogTimed('(E) Video Textures can only work with multithreaded device.', 'Textures');
    Result := grFail;
    Exit;
  end;
  try
    m_UsrID := G2VMRID; Inc(G2VMRID);
    if Failed(
      CoCreateInstance(
        CLSID_FilterGraph,
        nil,
        CLSCTX_INPROC_SERVER,
        IID_IGraphBuilder,
        m_Graph
      )
    ) then
    begin
      Result := grFail;
      Exit;
    end;
    if Failed(
      m_Graph.QueryInterface(IID_IMediaControl, m_Control)
    ) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    if Failed(
      m_Graph.QueryInterface(IID_IMediaEventEx, m_Event)
    ) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    if Failed(
      m_Graph.QueryInterface(IID_IMediaPosition, m_Position)
    ) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    if Failed(
      m_Graph.QueryInterface(IID_IMediaSeeking, m_Seeking)
    ) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    m_AllocatorPresenter := TG2VMRAllocatorPresenter.Create(Self);
    FileSourceBase := AddFilter(CLSID_AsyncReader, 'FileSource');
    FileSourceBase.QueryInterface(IID_IFileSourceFilter, FileSource);
    if not Assigned(FileSource) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    if Failed(FileSource.Load(PWideChar(f), nil)) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    FileSourceBase.EnumPins(EnumPins);
    while EnumPins.Next(1, CurPin, nil) = S_OK do
    begin
      CurPin.QueryPinInfo(PinInfo);
      if PinInfo.dir = PINDIR_OUTPUT then
      begin
        FileSourcePin := CurPin;
        Break;
      end;
    end;
    if not Assigned(FileSourcePin) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    m_VMR := AddFilter(CLSID_VideoMixingRenderer9, 'VMR9');
    if not Assigned(m_VMR) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    if Failed(
      m_VMR.QueryInterface(IID_IVMRFilterConfig9, FilterConfig)
    ) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    if Failed(
      FilterConfig.SetRenderingMode(VMR9Mode_Renderless)
    ) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    if G2ResFail(SetSurfaceAllocator) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    m_VMR.EnumPins(EnumPins);
    while EnumPins.Next(1, CurPin, nil) = S_OK do
    begin
      CurPin.QueryPinInfo(PinInfo);
      if PinInfo.dir = PINDIR_INPUT then
      begin
        m_PinIn := CurPin;
        Break;
      end;
    end;
    if not Assigned(m_PinIn) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    if Failed(m_Graph.Connect(FileSourcePin, m_PinIn)) then
    begin
      Release;
      Result := grFail;
      Exit;
    end;
    CompleteGraph;
    m_PinIn.ConnectedTo(m_PinOut);
    m_Loaded := True;
    Result := grOk;
  except
    Release;
    Result := grFail;
    Exit;
  end;
end;

procedure TG2Texture2DVideo.Play;
begin
  if m_Loaded then
  m_Control.Run;
end;

procedure TG2Texture2DVideo.Pause;
begin
  if m_Loaded then
  m_Control.Pause;
end;

procedure TG2Texture2DVideo.Stop;
begin
  if m_Loaded then
  begin
    m_Control.Stop;
    Rewind;
  end;
end;

procedure TG2Texture2DVideo.Rewind;
begin
  if m_Loaded then
  m_Position.put_CurrentPosition(0);
end;

function TG2Texture2DVideo.IsPlaying: Boolean;
var
  State: TFilterState;
  EventCode, p1, p2: Integer;
begin
  if m_Loaded then
  begin
    m_Event.GetEvent(EventCode, p1, p2, 0);
    if EventCode = EC_COMPLETE then
    Stop;
    m_Control.GetState(0, State);
    Result := (State = State_Running);
  end
  else
  Result := False;
end;

procedure TG2Texture2DVideo.Release;
begin
  m_Loaded := False;
  SafeRelease(m_PinOut);
  SafeRelease(m_PinIn);
  ClearGraph;
  SafeRelease(m_Seeking);
  SafeRelease(m_Position);
  SafeRelease(m_Event);
  SafeRelease(m_Control);
  SafeRelease(m_Graph);
  inherited Release;
end;
//TG2Texture2DVideo END

//TG2TextureCubeBase BEGIN
constructor TG2TextureCubeBase.Create;
begin
  inherited Create;
end;

destructor TG2TextureCubeBase.Destroy;
begin
  inherited Destroy;
end;

function TG2TextureCubeBase.GetTexture: IDirect3DBaseTexture9;
begin
  Result := m_Texture;
end;

procedure TG2TextureCubeBase.SetTexture(const Value: IDirect3DBaseTexture9);
var
  Desc: TD3DSurfaceDesc;
begin
  Release;
  m_Texture := IDirect3DCubeTexture9(Value);
  m_Texture.GetLevelDesc(0, Desc);
  m_Levels := m_Texture.GetLevelCount;
  m_Format := Desc.Format;
  m_Width := Desc.Width;
  m_Height := Desc.Width;
  m_Depth := Desc.Width;
end;

function TG2TextureCubeBase.SaveToFile(const FileName: WideString): TG2Result;
begin
  if Succeeded(
    D3DXSaveTextureToFileW(
      PWideChar(FileName),
      D3DXIFF_DDS,
      m_Texture,
      nil
    )
  ) then
  Result := grOk
  else
  Result := grFail;
end;

procedure TG2TextureCubeBase.Release;
begin
  if Assigned(m_Texture) then
  G2WriteLogTimed(AnsiString('(<) Texture Released (' + Name + ').'), 'Textures');
  SafeRelease(m_Texture);
end;
//TG2TextureCubeBase END

//TG2TextureCube BEGIN
constructor TG2TextureCube.Create;
begin
  inherited Create;
end;

destructor TG2TextureCube.Destroy;
begin
  inherited Destroy;
end;

procedure TG2TextureCube.InitLevels;
var
  i, j: Integer;
begin
  for i := 0 to High(TG2CubeFaces) do
  begin
    SetLength(m_Surfaces[i], m_Levels);
    for j := 0 to m_Levels - 1 do
    m_Surfaces[i][j] := TG2TextureSurface.Create(Self, j, TG2CubeFaces[i]);
  end;
end;

procedure TG2TextureCube.UnInitLevels;
var
  i, j: Integer;
begin
  for i := 0 to High(TG2CubeFaces) do
  begin
    for j := 0 to m_Levels - 1 do
    m_Surfaces[i][j].Free;
    m_Surfaces[i] := nil;
  end;
end;

procedure TG2TextureCube.SetTexture(const Value: IDirect3DBaseTexture9);
begin
  inherited SetTexture(Value);
  InitLevels;
end;

function TG2TextureCube.GetSurface(const Face: TD3DCubemapFaces; const SurfaceLevel: Integer): TG2TextureSurface;
begin
  Result := m_Surfaces[Byte(Face)][SurfaceLevel];
end;

function TG2TextureCube.LoadFromFile(
      const f: WideString;
      const MipLevels: Integer = 1;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  ImageInfo: TD3DXImageInfo;
begin
  Release;

  if Format = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTextureCube
  else
  m_Format := m_Gfx.Specs.FindCompatiableTextureCubeFormat(NewFormat);

  if Failed(
    D3DXCreateCubeTextureFromFileExW(
      m_Gfx.Device,
      PWideChar(f),
      0,
      MipLevels,
      0,
      m_Format,
      D3DPOOL_MANAGED,
      D3DX_FILTER_NONE,
      D3DX_FILTER_NONE,
      0,
      @ImageInfo,
      nil,
      m_Texture
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;

  m_Width := ImageInfo.Width;
  m_Height := ImageInfo.Height;
  m_Depth := ImageInfo.Depth;

  m_Levels := m_Texture.GetLevelCount;
  if m_Levels > 1 then
  begin
    D3DXFilterCubeTexture(
      m_Texture,
      nil,
      0,
      D3DX_FILTER_BOX
    );
  end;

  InitLevels;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Loaded (' + String(Name) + '): TextureCube ' + IntToStr(m_Width) + 'x' + IntToStr(m_Height) + 'x' + IntToStr(m_Depth) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

function TG2TextureCube.LoadFromBuffer(
      const Buffer: Pointer;
      const Size: DWord;
      const MipLevels: Integer = 1;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  ImageInfo: TD3DXImageInfo;
begin
  Release;

  if Format = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTextureCube
  else
  m_Format := m_Gfx.Specs.FindCompatiableTextureCubeFormat(NewFormat);

  if Failed(
    D3DXCreateCubeTextureFromFileInMemoryEx(
      m_Gfx.Device,
      Buffer,
      Size,
      0,
      MipLevels,
      0,
      m_Format,
      D3DPOOL_MANAGED,
      D3DX_FILTER_NONE,
      D3DX_FILTER_NONE,
      0,
      @ImageInfo,
      nil,
      m_Texture
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;

  m_Width := ImageInfo.Width;
  m_Height := ImageInfo.Height;
  m_Depth := ImageInfo.Depth;

  m_Levels := m_Texture.GetLevelCount;
  if m_Levels > 1 then
  begin
    D3DXFilterCubeTexture(
      m_Texture,
      nil,
      0,
      D3DX_FILTER_BOX
    );
  end;

  InitLevels;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Loaded (' + String(Name) + '): TextureCube ' + IntToStr(m_Width) + 'x' + IntToStr(m_Height) + 'x' + IntToStr(m_Depth) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

function TG2TextureCube.LoadFromPack(
      const FolderName, FileName: AnsiString;
      const MipLevels: Integer = 1;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  Data: Pointer;
  DataSize: DWord;
begin
  Core.PackLinker.GetFileData(FolderName, FileName, Data, DataSize);
  if Assigned(Data) then
  Result := LoadFromBuffer(Data, DataSize, MipLevels, NewFormat)
  else
  Result := grFail;
end;

function TG2TextureCube.MakeTexture(
      const Size: Integer;
      const MipLevels: Integer = 8;
      const Usage: DWord = 0;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN;
      const Pool: TD3DPool = D3DPOOL_MANAGED
    ): TG2Result;
var
  EdgeSize: Integer;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTextureCube
  else
  m_Format := m_Gfx.Specs.FindCompatiableTextureCubeFormat(NewFormat);

  EdgeSize := 4; while EdgeSize < Size do EdgeSize := EdgeSize shl 1;
  m_Width := EdgeSize; m_Height := EdgeSize; m_Depth := EdgeSize;

  if MipLevels = 0 then
  m_Levels := MaxMipLevels(Width, Height, Depth)
  else
  m_Levels := Min(MipLevels, MaxMipLevels(Width, Height, Depth));

  if Failed(
    m_Gfx.Device.CreateCubeTexture(
      EdgeSize,
      m_Levels,
      Usage,
      m_Format,
      Pool,
      m_Texture,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;

  InitLevels;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Created (' + String(Name) + '): TextureCube ' + IntToStr(m_Width) + 'x' + IntToStr(m_Height) + 'x' + IntToStr(m_Depth) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

procedure TG2TextureCube.Release;
begin
  UnInitLevels;
  inherited Release;
end;
//TG2TextureCube END

//TG2TextureCubeRT BEGIN
procedure TG2TextureCubeRT.InitSurfaces;
var
  i: Integer;
  Surf: IDirect3DSurface9;
begin
  for i := 0 to 5 do
  begin
    m_SurfacesRT[i].Release;
    m_Texture.GetCubeMapSurface(TD3DCubemapFaces(i), 0, Surf);
    m_SurfacesRT[i].Surface := Surf;
  end;
end;

procedure TG2TextureCubeRT.UnInitSurfaces;
var
  i: Integer;
begin
  for i := 0 to 5 do
  m_SurfacesRT[i].Release;
end;

procedure TG2TextureCubeRT.SetTexture(const Value: IDirect3DBaseTexture9);
begin
  inherited SetTexture(Value);
  InitSurfaces;
end;

function TG2TextureCubeRT.GetSurface(const Face: TD3DCubemapFaces): TG2SurfaceRT;
begin
  Result := m_SurfacesRT[Byte(Face)];
end;

procedure TG2TextureCubeRT.OnDeviceLost;
begin
  Release;
end;

procedure TG2TextureCubeRT.OnDeviceReset;
begin
  if Succeeded(
    m_Gfx.Device.CreateCubeTexture(
      m_Width,
      m_Levels,
      D3DUSAGE_RENDERTARGET,
      m_Format,
      D3DPOOL_DEFAULT,
      m_Texture,
      nil
    )
  ) then
  InitSurfaces;
end;

function TG2TextureCubeRT.Initialize(const G2Core: TG2Core): TG2Result;
var
  i: Integer;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  for i := 0 to 5 do
  begin
    m_SurfacesRT[i] := TG2SurfaceRT.Create;
    m_SurfacesRT[i].Initialize(G2Core);
  end;
end;

function TG2TextureCubeRT.Finalize: TG2Result;
var
  i: Integer;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  UnInitSurfaces;
  for i := 0 to 5 do
  begin
    m_SurfacesRT[i].Finalize;
    m_SurfacesRT[i].Free;
  end;
end;

function TG2TextureCubeRT.MakeRenderTarget(
      const Size: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  EdgeSize: Integer;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTextureCubeRT
  else
  m_Format := m_Gfx.Specs.FindCompatiableTextureCubeRTFormat(NewFormat);

  EdgeSize := 4; while EdgeSize < Size do EdgeSize := EdgeSize shl 1;
  m_Width := EdgeSize; m_Height := EdgeSize; m_Depth := EdgeSize;

  m_Levels := 1;


  if Failed(
    m_Gfx.Device.CreateCubeTexture(
      EdgeSize,
      m_Levels,
      D3DUSAGE_RENDERTARGET,
      m_Format,
      D3DPOOL_DEFAULT,
      m_Texture,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;

  InitSurfaces;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Created (' + String(Name) + '): TextureCubeRT ' + IntToStr(m_Width) + 'x' + IntToStr(m_Height) + 'x' + IntToStr(m_Depth) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

procedure TG2TextureCubeRT.Release;
begin
  UnInitSurfaces;
  inherited Release;
end;
//TG2TextureCubeRT END

//TG2TextureVolume BEGIN
constructor TG2TextureVolume.Create;
begin
  inherited Create;
end;

destructor TG2TextureVolume.Destroy;
begin
  inherited Destroy;
end;

function TG2TextureVolume.GetTexture: IDirect3DBaseTexture9;
begin
  Result := m_Texture;
end;

procedure TG2TextureVolume.SetTexture(const Value: IDirect3DBaseTexture9);
var
  Desc: TD3DVolumeDesc;
begin
  Release;
  m_Texture := IDirect3DVolumeTexture9(Value);
  m_Texture.GetLevelDesc(0, Desc);
  m_Levels := m_Texture.GetLevelCount;
  m_Format := Desc.Format;
  m_Width := Desc.Width;
  m_Height := Desc.Height;
  m_Depth := Desc.Depth;
end;

function TG2TextureVolume.LoadFromFile(
      const FileName: WideString;
      const MipLevels: Integer = 1;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
var
  ImageInfo: TD3DXImageInfo;
begin
  Release;

  if Format = D3DFMT_UNKNOWN then
  m_Format := m_Gfx.m_InitParamsActual.FormatTextureVolume
  else
  m_Format := m_Gfx.Specs.FindCompatiableTextureVolumeFormat(Format);

  if Failed(
    D3DXCreateVolumeTextureFromFileExW(
      m_Gfx.Device,
      PWideChar(FileName),
      0, 0, 0,
      MipLevels,
      0,
      m_Format,
      D3DPOOL_MANAGED,
      D3DX_FILTER_NONE,
      D3DX_FILTER_NONE,
      0,
      @ImageInfo,
      nil,
      m_Texture
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;

  m_Width := ImageInfo.Width;
  m_Height := ImageInfo.Height;
  m_Depth := ImageInfo.Depth;

  m_Levels := m_Texture.GetLevelCount;
  if m_Levels > 1 then
  begin
    m_Texture.SetAutoGenFilterType(D3DX_FILTER_BOX);
    m_Texture.GenerateMipSubLevels;
  end;

  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Texture Loaded (' + String(Name) + '): TextureVolume ' + IntToStr(m_Width) + 'x' + IntToStr(m_Height) + 'x' + IntToStr(m_Depth) + ' ' + String(G2FormatToString(m_Format)) + ' ' + IntToStr(m_Levels) + 'Mip'), 'Textures');
end;

procedure TG2TextureVolume.Release;
begin
  SafeRelease(m_Texture);
end;
//TG2TextureVolume END

//TG2SurfaceMgr BEGIN
constructor TG2SurfaceMgr.Create;
begin
  inherited Create;
end;

destructor TG2SurfaceMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TG2SurfaceMgr.OnDeviceLost;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2Surface(m_Resources[i]).OnDeviceLost;
end;

procedure TG2SurfaceMgr.OnDeviceReset;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2Surface(m_Resources[i]).OnDeviceReset;
end;

function TG2SurfaceMgr.CreateRenderTargetSurface(
      const Name: WideString;
      const Width, Height: Integer;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2SurfaceRT;
begin
  Result := TG2SurfaceRT.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.CreateRenderTarget(Width, Height, Format)) then
  AddResource(Result) else Result.Free;
end;

function TG2SurfaceMgr.CreateDepthStencilSurface(
      const Name: WideString;
      const Width, Height: Integer;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2SurfaceDS;
begin
  Result := TG2SurfaceDS.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.CreateDepthStencil(Width, Height, Format)) then
  AddResource(Result) else Result.Free;
end;

function TG2SurfaceMgr.FindSurface(const Name: WideString): TG2Surface;
begin
  Result := TG2Surface(FindResource(Name));
end;

procedure TG2SurfaceMgr.DeleteSurface(const Index: Integer);
begin
  DeleteResource(Index);
end;

procedure TG2SurfaceMgr.RemoveSurface(const Surface: TG2Surface);
begin
  RemoveResource(Surface);
end;

function TG2SurfaceMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  Result := grOk;
end;

function TG2SurfaceMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;
//TG2SurfaceMgr END

//TG2Surface BEGIN
constructor TG2Surface.Create;
begin
  inherited Create;
end;

destructor TG2Surface.Destroy;
begin
  inherited Destroy;
end;

procedure TG2Surface.SetSurface(const Value: IDirect3DSurface9);
var
  Desc: TD3DSurfaceDesc;
begin
  m_Surface := Value;
  if Assigned(m_Surface) then
  begin
    m_Surface.GetDesc(Desc);
    m_Format := Desc.Format;
    m_Width := Desc.Width;
    m_Height := Desc.Height;
    SetUpViewPort;
    m_Enabled := True;
  end
  else
  Release;
end;

procedure TG2Surface.SetUpViewPort;
begin
  m_ViewPort.X := 0;
  m_ViewPort.Y := 0;
  m_ViewPort.Width := m_Width;
  m_ViewPort.Height := m_Height;
  m_ViewPort.MinZ := 0;
  m_ViewPort.MaxZ := 1;
end;

function TG2Surface.GetViewPort: PD3DViewport9;
begin
  Result := @m_ViewPort;
end;

procedure TG2Surface.Release;
begin
  SafeRelease(m_Surface);
  m_Enabled := False;
  m_Format := D3DFMT_UNKNOWN;
  m_Width := 0;
  m_Height := 0;
end;

function TG2Surface.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Release;
  Result := grOk;
end;

function TG2Surface.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Release;
  Result := grOk;
end;
//TG2Surface END

//TG2SurfaceRT BEGIN
constructor TG2SurfaceRT.Create;
begin
  inherited Create;
end;

destructor TG2SurfaceRT.Destroy;
begin
  inherited Destroy;
end;

function TG2SurfaceRT.CreateRenderTarget(
      const NewWidth, NewHeight: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := Core.Graphics.Params.FormatSurfaceRT
  else
  m_Format := Core.Graphics.Specs.FindCompatiableSurfaceRTFormat(NewFormat);

  if Failed(
    Core.Graphics.Device.CreateRenderTarget(
      NewWidth, NewHeight, m_Format,
      TD3DMultiSampleType(Core.Graphics.Params.AntialiasingSampleCount),
      0, False, m_Surface, nil
    )
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;

  m_Width := NewWidth;
  m_Height := NewHeight;
  SetUpViewPort;
  m_Enabled := True;
  Result := grOk;
end;

procedure TG2SurfaceRT.OnDeviceLost;
begin
  SafeRelease(m_Surface);
end;

procedure TG2SurfaceRT.OnDeviceReset;
begin
  if m_Enabled then
  begin
    m_Format := Core.Graphics.Specs.FindCompatiableSurfaceRTFormat(m_Format);
    if Failed(
      Core.Graphics.Device.CreateRenderTarget(
        m_Width, m_Height, m_Format,
        TD3DMultiSampleType(Core.Graphics.Params.AntialiasingSampleCount),
        0, False, m_Surface, nil
      )
    ) then
    Release;
  end;
end;
//TG2SurfaceRT END

//TG2SurfaceDS BEGIN
constructor TG2SurfaceDS.Create;
begin
  inherited Create;
end;

destructor TG2SurfaceDS.Destroy;
begin
  inherited Destroy;
end;

function TG2SurfaceDS.CreateDepthStencil(
      const NewWidth, NewHeight: Integer;
      const NewFormat: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Result;
begin
  Release;

  if NewFormat = D3DFMT_UNKNOWN then
  m_Format := Core.Graphics.Params.FormatSurfaceDS
  else
  m_Format := Core.Graphics.Specs.FindCompatiableSurfaceDSFormat(NewFormat);

  if Failed(
    Core.Graphics.Device.CreateDepthStencilSurface(
      NewWidth, NewHeight, m_Format,
      TD3DMultiSampleType(Core.Graphics.Params.AntialiasingSampleCount),
      0, True, m_Surface, nil
    )
  ) then
  begin
    Release;
    Result := grFail;
    Exit;
  end;

  m_Width := NewWidth;
  m_Height := NewHeight;
  SetUpViewPort;
  m_Enabled := True;
  Result := grOk;
end;

procedure TG2SurfaceDS.OnDeviceLost;
begin
  SafeRelease(m_Surface);
end;

procedure TG2SurfaceDS.OnDeviceReset;
begin
  if m_Enabled then
  begin
    m_Format := Core.Graphics.Specs.FindCompatiableSurfaceDSFormat(m_Format);
    if Failed(
      Core.Graphics.Device.CreateDepthStencilSurface(
        m_Width, m_Height, m_Format,
        TD3DMultiSampleType(Core.Graphics.Params.AntialiasingSampleCount),
        0, True, m_Surface, nil
      )
    ) then
    Release;
  end;
end;
//TG2SurfaceDS END

//TG2NetServer BEGIN
procedure TG2NetServer.TG2NetThread.AfterConstruction;
begin
  m_CS := TCriticalSection.Create;
end;

procedure TG2NetServer.TG2NetThread.BeforeDestruction;
begin
  m_CS.Free;
end;

procedure TG2NetServer.TG2NetThreadListen.Execute;
var
  Sock: Integer;
  Addr: TSockAddrIn;
  AddrLen: Integer;
begin
  while not Terminated do
  begin
    AddrLen := SizeOf(TSockAddrIn);
    ZeroMemory(@Addr, AddrLen);
    Sock := WinSock.Accept(Server.Sock, @Addr, @AddrLen);
    if Sock <> INVALID_SOCKET then
    begin
      CS.Enter;
      try
        Server.AddConnection(Sock, @Addr);
      finally
        CS.Leave;
      end;
    end;
  end;
end;

procedure TG2NetServer.TG2NetThreadReceive.Execute;
var
  Header: TG2NetMesHeader;
  i, n, l: Integer;
begin
  while not Terminated do
  begin
    l := WinSock.Recv(Server.Connections[ID]^.Sock, Header, SizeOf(Header), 0);
    if l = SizeOf(Header) then
    begin
      case Header.Desc of
        mdPing:
        begin
          Server.Connections[ID]^.Ping := GetTickCount;
        end;
        mdUser:
        begin
          Server.Connections[ID]^.Ping := GetTickCount;
          if Header.Len > 0 then
          begin
            if Length(m_Buf) < Header.Len then
            SetLength(m_Buf, Header.Len);
            i := 0;
            while i < Header.Len do
            begin
              l := WinSock.Recv(Server.Connections[ID]^.Sock, m_Buf[i], Header.Len - i, 0);
              if l = SOCKET_ERROR then
              Break;
              i := i + l;
            end;
            if l <> SOCKET_ERROR then
            begin
              CS.Enter;
              try
                n := -1;
                for i := 0 to High(Server.PacketsReceived) do
                if Server.PacketsReceived[i].Discarded then
                begin
                  n := i;
                  Break;
                end;
                if n = -1 then
                begin
                  n := Length(Server.PacketsReceived);
                  SetLength(Server.PacketsReceived, n + 16);
                end;
                Server.PacketsReceived[n].Server := Server;
                Server.PacketsReceived[n].ID := n;
                Server.PacketsReceived[n].UserData := Server.Connections[ID]^.UserData;
                if Length(Server.PacketsReceived[n].Buf) < Header.Len then
                SetLength(Server.PacketsReceived[n].Buf, Header.Len);
                Server.PacketsReceived[n].Len := Header.Len;
                Move(m_Buf[0], Server.PacketsReceived[n].Buf[0], Header.Len);
                Server.EnableReceivePacket(n);
              finally
                CS.Leave;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TG2NetServer.TG2NetThreadIdle.Execute;
begin
  while not Terminated do
  begin

  end;
end;

procedure TG2NetServer.TG2NetConnection.ReceivingEnable;
begin
  Status := nsConnected;
  ThreadReceive := TG2NetThreadReceive.Create(True);
  ThreadReceive.Server := Server;
  ThreadReceive.ID := ID;
  ThreadReceive.FreeOnTerminate := False;
  ThreadReceive.Priority := tpLower;
  {$IFDEF VER210} ThreadReceive.Start; {$ELSE} ThreadReceive.Resume; {$ENDIF}
end;

procedure TG2NetServer.TG2NetConnection.ReceivingDisable;
begin
  Status := nsDisconnected;
  ThreadReceive.Terminate;
  CloseSocket(Sock);
  Sock := INVALID_SOCKET;
  ThreadReceive.WaitFor;
  ThreadReceive.Free;
  ThreadReceive := nil;
end;

procedure TG2NetServer.TG2NetPacketReceive.Discard;
begin
  Server.DiscardReceivePacket(ID);
end;

function TG2NetServer.GetConnection(const Index: Integer): PG2NetConnection;
begin
  Result := @m_Connections[Index];
end;

function TG2NetServer.GetPacket(const Index: Integer): PG2NetPacketReceive;
begin
  Result := @PacketsReceived[m_PacketList[Index]];
end;

procedure TG2NetServer.AddConnection(const NewSock: Integer; const NewAddr: PSockAddrIn);
{$IFDEF G2_WRITE_LOG}
  var hadr: Integer;
{$ENDIF}
begin
  if NewSock <> INVALID_SOCKET then
  begin
    m_Connections[m_ConnectionCount].Server := Self;
    m_Connections[m_ConnectionCount].ID := m_ConnectionCount;
    m_Connections[m_ConnectionCount].Sock := NewSock;
    m_Connections[m_ConnectionCount].Addr := NewAddr^;
    m_Connections[m_ConnectionCount].Ping := GetTickCount;
    m_Connections[m_ConnectionCount].ReceivingEnable;
    Inc(m_ConnectionCount);
    if m_ConnectionCount > High(m_Connections) then
    SetLength(m_Connections, Length(m_Connections) + 16);
    {$IFDEF G2_WRITE_LOG}
    hadr := WinSock.ntohl(Integer(NewAddr^.sin_addr));
    G2WriteLogTimed(
      AnsiString(
        'TG2NetServer connection received (' +
        IntToStr((hadr shr 24) and $ff) + '.' +
        IntToStr((hadr shr 16) and $ff) + '.' +
        IntToStr((hadr shr 8) and $ff) + '.' +
        IntToStr(hadr and $ff) + ':' +
        IntToStr(ntohs(NewAddr^.sin_port)) + ')'
      ),
      'Network'
    );
    {$ENDIF}
  end;
end;

procedure TG2NetServer.EnableReceivePacket(const ID: Integer);
begin
  PacketsReceived[ID].Discarded := False;
  if m_PacketListCount >= Length(m_PacketList) then
  SetLength(m_PacketList, m_PacketListCount + 16);
  m_PacketList[m_PacketListCount] := ID;
  Inc(m_PacketListCount);
end;

procedure TG2NetServer.DiscardReceivePacket(const ID: Integer);
  var i, n: Integer;
begin
  PacketsReceived[ID].Discarded := True;
  n := -1;
  for i := 0 to m_PacketListCount - 1 do
  if m_PacketList[i] = ID then
  begin
    n := i;
    Break;
  end;
  if n > -1 then
  begin
    Move(m_PacketList[n + 1], m_PacketList[n], (m_PacketListCount - n - 1) * 4);
    Dec(m_PacketListCount);
  end;
end;

constructor TG2NetServer.Create;
var
  i: Integer;
begin
  inherited Create;
  m_Sock := INVALID_SOCKET;
  m_ConnectionCount := 0;
  SetLength(m_Connections, 16);
  SetLength(PacketsReceived, 16);
  SetLength(m_PacketList, 16);
  m_PacketListCount := 0;
  for i := 0 to High(PacketsReceived) do
  PacketsReceived[i].Discarded := True;
  m_AllowConnections := False;
end;

destructor TG2NetServer.Destroy;
begin
  inherited Destroy;
end;

function TG2NetServer.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Status := nsIdle;
end;

function TG2NetServer.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  ForbidConnections;
end;

function TG2NetServer.AllowConnections(const Port: Word = 5632; const MaxRequests: Integer = 4): TG2Result;
var
  Addr: TSockAddrIn;
begin
  Result := grFail;
  if m_AllowConnections then
  ForbidConnections;
  m_Sock := Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if m_Sock = INVALID_SOCKET then Exit;
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr := TInAddr(htonl(INADDR_ANY));
  if Bind(m_Sock, Addr, SizeOf(Addr)) = SOCKET_ERROR then Exit;
  if Listen(m_Sock, MaxRequests) = SOCKET_ERROR then Exit;
  m_AllowConnections := True;
  m_ThreadListen := TG2NetThreadListen.Create(True);
  m_ThreadListen.FreeOnTerminate := False;
  m_ThreadListen.Server := Self;
  m_ThreadListen.Priority := tpLowest;
  {$IFDEF VER210} m_ThreadListen.Start; {$ELSE} m_ThreadListen.Resume; {$ENDIF}
  m_Status := nsListen;
  Result := grOk;
end;

procedure TG2NetServer.ForbidConnections;
  var i: Integer;
begin
  if not m_AllowConnections then Exit;
  m_AllowConnections := False;
  m_Status := nsIdle;
  if m_Sock <> INVALID_SOCKET then
  begin
    CloseSocket(m_Sock);
    m_Sock := INVALID_SOCKET;
  end;
  m_ThreadListen.Terminate;
  m_ThreadListen.WaitFor;
  m_ThreadListen.Free;
  m_ThreadListen := nil;
  for i := 0 to m_ConnectionCount - 1 do
  if m_Connections[i].Sock <> INVALID_SOCKET then
  m_Connections[i].ReceivingDisable;
  m_ConnectionCount := 0;
end;

function TG2NetServer.Send(const ID: Integer; const Data: Pointer; const Size: Integer): TG2Result;
var
  Header: TG2NetMesHeader;
  i, l: Integer;
begin
  Header.Desc := mdUser;
  Header.Len := Size;
  if WinSock.Send(m_Connections[ID].Sock, Header, SizeOf(Header), 0) <> SizeOf(Header) then
  begin
    Result := grFail;
    Exit;
  end;
  i := 0;
  while i < Size do
  begin
    l := WinSock.Send(m_Connections[ID].Sock, PByteArray(Data)^[i], Size, 0);
    if l = SOCKET_ERROR then
    begin
      Result := grFail;
      Exit;
    end;
    i := i + l;
  end;
  Result := grOk;
end;
//TG2NetServer END

//TG2NetClient BEGIN
procedure TG2NetClient.TG2NetThread.AfterConstruction;
begin
  m_CS := TCriticalSection.Create;
end;

procedure TG2NetClient.TG2NetThread.BeforeDestruction;
begin
  m_CS.Free;
end;

procedure TG2NetClient.TG2NetPacketReceive.Discard;
begin
  Client.DiscardReceivePacket(ID);
end;

procedure TG2NetClient.TG2NetThreadReceive.Execute;
var
  Header: TG2NetMesHeader;
  i, n, l: Integer;
begin
  while not Terminated do
  begin
    l := WinSock.Recv(Client.Sock, Header, SizeOf(Header), 0);
    if l = SizeOf(Header) then
    begin
      case Header.Desc of
        mdUser:
        begin
          if Header.Len > 0 then
          begin
            if Length(m_Buf) < Header.Len then
            SetLength(m_Buf, Header.Len);
            i := 0;
            while i < Header.Len do
            begin
              l := WinSock.Recv(Client.Sock, m_Buf[i], Header.Len - i, 0);
              if l = SOCKET_ERROR then
              Break;
              i := i + l;
            end;
            if l <> SOCKET_ERROR then
            begin
              CS.Enter;
              try
                n := -1;
                for i := 0 to High(Client.PacketsReceived) do
                if Client.PacketsReceived[i].Discarded then
                begin
                  n := i;
                  Break;
                end;
                if n = -1 then
                begin
                  n := Length(Client.PacketsReceived);
                  SetLength(Client.PacketsReceived, n + 16);
                end;
                Client.PacketsReceived[n].Client := Client;
                Client.PacketsReceived[n].ID := n;
                if Length(Client.PacketsReceived[n].Buf) < Header.Len then
                SetLength(Client.PacketsReceived[n].Buf, Header.Len);
                Client.PacketsReceived[n].Len := Header.Len;
                Move(m_Buf[0], Client.PacketsReceived[n].Buf[0], Header.Len);
                Client.EnableReceivePacket(n);
              finally
                CS.Leave;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TG2NetClient.GetPacket(const Index: Integer): PG2NetPacketReceive;
begin
  Result := @PacketsReceived[m_PacketList[Index]];
end;

procedure TG2NetClient.EnableReceivePacket(const ID: Integer);
begin
  PacketsReceived[ID].Discarded := False;
  if m_PacketListCount >= Length(m_PacketList) then
  SetLength(m_PacketList, m_PacketListCount + 16);
  m_PacketList[m_PacketListCount] := ID;
  Inc(m_PacketListCount);
end;

procedure TG2NetClient.DiscardReceivePacket(const ID: Integer);
  var i, n: Integer;
begin
  PacketsReceived[ID].Discarded := True;
  n := -1;
  for i := 0 to m_PacketListCount - 1 do
  if m_PacketList[i] = ID then
  begin
    n := i;
    Break;
  end;
  if n > -1 then
  begin
    Move(m_PacketList[n + 1], m_PacketList[n], (m_PacketListCount - n - 1) * 4);
    Dec(m_PacketListCount);
  end;
end;

constructor TG2NetClient.Create;
begin
  inherited Create;
  SetLength(m_PacketList, 16);
  m_PacketListCount := 0;
end;

destructor TG2NetClient.Destroy;
begin
  inherited Destroy;
end;

function TG2NetClient.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Connected := False;
end;

function TG2NetClient.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Disconnect;
end;

function TG2NetClient.Connect(const Host: AnsiString; const Port: Word = 5632): TG2Result;
  var Addr: TSockAddrIn;
begin
  Result := grFail;
  m_Sock := Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if m_Sock = INVALID_SOCKET then
  begin
    G2WriteLogTimed('TG2NetClient socket creation failed - ' + G2WSAErrorToStr(WSAGetLastError), 'Network');
    Exit;
  end;
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr := TInAddr(inet_addr(PAnsiChar(Host)));
  if WinSock.Connect(m_Sock, Addr, SizeOf(Addr)) = SOCKET_ERROR then
  begin
    G2WriteLogTimed('TG2NetClient socket connection failed (' + Host + ') - ' + G2WSAErrorToStr(WSAGetLastError), 'Network');
    Exit;
  end;
  m_Connected := True;
  m_ThreadReceive := TG2NetThreadReceive.Create(True);
  m_ThreadReceive.Client := Self;
  {$IFDEF VER210} m_ThreadReceive.Start; {$ELSE} m_ThreadReceive.Resume; {$ENDIF}
  Result := grOk;
end;

function TG2NetClient.Send(const Data: Pointer; const Size: Integer): TG2Result;
var
  Header: TG2NetMesHeader;
  i, l: Integer;
begin
  Header.Desc := mdUser;
  Header.Len := Size;
  if WinSock.Send(m_Sock, Header, SizeOf(Header), 0) <> SizeOf(Header) then
  begin
    Result := grFail;
    Exit;
  end;
  i := 0;
  while i < Size do
  begin
    l := WinSock.Send(m_Sock, PByteArray(Data)^[i], Size, 0);
    if l = SOCKET_ERROR then
    begin
      Result := grFail;
      Exit;
    end;
    i := i + l;
  end;
  Result := grOk;
end;

function TG2NetClient.Disconnect: TG2Result;
begin
  if m_Connected then
  begin
    m_ThreadReceive.Terminate;
    if m_Sock <> INVALID_SOCKET then
    begin
      CloseSocket(m_Sock);
      m_Sock := INVALID_SOCKET;
    end;
    m_ThreadReceive.WaitFor;
    m_ThreadReceive.Free;
    m_ThreadReceive := nil;
    m_Connected := False;
    Result := grOk;
  end
  else
  Result := grRedundantCall;
end;
//TG2NetClient END

//TG2FontMgr BEGIN
constructor TG2FontMgr.Create;
begin
  inherited Create;
end;

destructor TG2FontMgr.Destroy;
begin
  inherited Destroy;
end;

function TG2FontMgr.GetFont(const Index: Integer): TG2Font;
begin
  Result := TG2Font(m_Resources[Index]);
end;

function TG2FontMgr.CreateFont(
      const Name: WideString;
      const FontFace: AnsiString;
      const Size: Integer
    ): TG2Font;
begin
  Result := TG2Font.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.MakeFont(FontFace, Size)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2FontMgr.CreateFontFromFile(
      const Name: WideString;
      const FileName: WideString;
      const Format: TD3DFormat = D3DFMT_UNKNOWN
    ): TG2Font;
begin
  Result := TG2Font.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFont(FileName, Format)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2FontMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;

function TG2FontMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2FontMgr END

//TG2Font BEGIN
constructor TG2Font.Create;
begin
  inherited Create;
end;

destructor TG2Font.Destroy;
begin
  inherited Destroy;
end;

function TG2Font.MakeFont(const NewFontFace: AnsiString; const NewSize: Integer): TG2Result;
type
  TBGRA = record
    b, g, r, a: Byte;
  end;
  PBGRAArray = ^TBGRAArray;
  TBGRAArray = array[Word] of TBGRA;
var
  bmp: TBitmap;
  CharWidth, CharHeight, w, h, cx, cy: Integer;
  MaxWidth, MaxHeight: Integer;
  i, j: Integer;
  pbc: PBGRAArray;
  XChar: Byte;
  LogFont: TLogFont;
begin
  Release;
  m_FontFace := NewFontFace;
  m_Size := NewSize;
  bmp := TBitmap.Create;
  MaxWidth := Min(m_Gfx.Caps.MaxTextureWidth, 2048);
  MaxHeight := Min(m_Gfx.Caps.MaxTextureHeight, 2048);
  bmp.Canvas.Font.Name := String(m_FontFace);
  bmp.Canvas.Font.Size := m_Size;
  GetObject(bmp.Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
  LogFont.lfQuality := ANTIALIASED_QUALITY;
  bmp.Canvas.Font.Handle := CreateFontIndirect(LogFont);
  bmp.Canvas.Font.Charset := RUSSIAN_CHARSET;
  CharWidth := 1;
  CharHeight := 1;
  for i := 0 to 255 do
  begin
    w := bmp.Canvas.TextWidth(Char(i));
    h := bmp.Canvas.TextHeight(Char(i));
    m_Props[i].Width := w;
    m_Props[i].Height := h;
    if w > CharWidth then CharWidth := w;
    if h > CharHeight then CharHeight := h;
  end;
  i := Min(CharWidth * 16, MaxWidth);
  j := Min(CharHeight * 16, MaxHeight);
  w := 1; h := 1;
  while w < i do w := w shl 1;
  while h < j do h := h shl 1;
  CharWidth := w div 16;
  CharHeight := h div 16;
  bmp.Monochrome := True;
  bmp.PixelFormat := pf32bit;
  bmp.SetSize(w, h);
  bmp.Canvas.Pen.Color := clBlack;
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.Rectangle(bmp.Canvas.ClipRect);
  bmp.Canvas.Brush.Style := bsClear;
  bmp.Canvas.Font.Color := clWhite;
  for j := 0 to 15 do
  for i := 0 to 15 do
  begin
    XChar := j * 16 + i;
    cx := (CharWidth - m_Props[XChar].Width) div 2;
    cy := (CharHeight - m_Props[XChar].Height) div 2;
    m_Props[XChar].OffsetX := -cx;
    m_Props[XChar].OffsetY := -cy;
    bmp.Canvas.TextOut(i * CharWidth + cx, j * CharHeight + cy, String(AnsiChar(XChar)));
  end;
  for j := 0 to h - 1 do
  begin
    pbc := bmp.ScanLine[j];
    for i := 0 to w - 1 do
    begin
      pbc^[i].a := Trunc((1 - Cos(pbc^[i].r / 255 * HalfPi)) * 255);
      pbc^[i].b := $ff;
      pbc^[i].g := $ff;
      pbc^[i].r := $ff;
    end;
  end;
  Result := m_Texture.LoadFromGraphic(bmp, 1, D3DFMT_DXT2);
  bmp.Free;
  m_CharTU := CharWidth / m_Texture.Width;
  m_CharTV := CharHeight / m_Texture.Height;
  m_CharWidth := CharWidth;
  m_CharHeight := CharHeight;
  G2WriteLogTimed(AnsiString('(>) Font Generated (' + String(Name) + '): ' + String(m_FontFace) + ' ' + IntToStr(m_Size) + ' Texture' + IntToStr(m_Texture.RealWidth) + 'x' + IntToStr(m_Texture.RealHeight)), 'Fonts');
end;

function TG2Font.LoadFont(const FileName: String; const NewFormat: TD3DFormat = D3DFMT_UNKNOWN): TG2Result;
type
  TCharProp = packed record
    Width: Byte;
    Height: Byte;
  end;
  TG2FontFile = packed record
    Definition: string[4];
    Version: DWord;
    FontFace: AnsiString;
    FontSize: Integer;
    DataSize: Int64;
    Chars: array[0..255] of TCharProp;
  end;
const
  Definition = 'G2F';
  Version = $00010000;
var
  fw: TG2FileRW;
  FontFile: TG2FontFile;
  i: Integer;
  fmt: TD3DFormat;
  Data: array of Byte;
begin
  Release;
  fw := TG2FileRW.Create;
  try
    fw.OpenRead(FileName);
    fw.Compression := True;
    fw.ReadBuffer(FontFile.Definition, 4);
    if FontFile.Definition <> Definition then
    begin
      fw.Free;
      Result := grFail;
      Exit;
    end;
    FontFile.Version := fw.ReadUInt4;
    if FontFile.Version <> Version then
    begin
      fw.Free;
      Result := grFail;
      Exit;
    end;
    FontFile.FontFace := fw.ReadStringNT;
    FontFile.FontSize := fw.ReadSInt4;
    FontFile.DataSize := fw.ReadSInt8;
    fw.ReadBuffer(FontFile.Chars, SizeOf(FontFile.Chars));
    SetLength(Data, FontFile.DataSize);
    fw.ReadBuffer(Data[0], FontFile.DataSize);
    if NewFormat = D3DFMT_UNKNOWN then
    fmt := Core.Graphics.m_InitParamsActual.FormatTexture2D
    else
    fmt := Core.Graphics.Specs.FindCompatiableTexture2DFormat(NewFormat);
    Result := m_Texture.LoadFromBuffer(@Data[0], FontFile.DataSize, 1, fmt);
    m_FontFace := FontFile.FontFace;
    m_Size := FontFile.FontSize;
    m_CharWidth := m_Texture.Width div 16;
    m_CharHeight := m_Texture.Height div 16;
    for i := 0 to 255 do
    begin
      m_Props[i].Width := FontFile.Chars[i].Width;
      m_Props[i].Height := FontFile.Chars[i].Height;
      m_Props[i].OffsetX := -(m_CharWidth - m_Props[i].Width) div 2;
      m_Props[i].OffsetY := -(m_CharHeight - m_Props[i].Height) div 2;
    end;
    m_CharTU := m_CharWidth / m_Texture.Width;
    m_CharTV := m_CharHeight / m_Texture.Height;
    Result := grOk;
  finally
    fw.Free;
  end;
  G2WriteLogTimed(AnsiString('(>) Font Loaded (' + String(Name) + '): ' + String(m_FontFace) + ' ' + IntToStr(m_Size) + ' Texture' + IntToStr(m_Texture.RealWidth) + 'x' + IntToStr(m_Texture.RealHeight)), 'Fonts');
end;

function TG2Font.Print(const X, Y: Single; const Color: TG2Color; const Text: AnsiString): TG2Result;
begin
  Result := Print(X, Y, 1, 1, Color, Text);
end;

function TG2Font.Print(const X, Y, ScaleX, ScaleY: Single; const Color: TG2Color; const Text: AnsiString): TG2Result;
var
  i, CurVertex: Integer;
  XChar: Byte;
  Width: Single;
  VBSize: Integer;
  Vertices: PG2SharedVertex2DArray;
  tu1, tv1, tu2, tv2: Single;
  x1, y1, x2, y2: Single;
begin
  VBSize := Length(Text) * 4;
  Result := m_Gfx.SharedVB2D.VerifySize(VBSize);
  if G2ResFail(Result) then Exit;
  Width := 0;
  m_Gfx.SharedVB2D.VB.Lock(0, VBSize, Pointer(Vertices), D3DLOCK_DISCARD);
  for i := 0 to Length(Text) - 1 do
  begin
    XChar := Ord(Text[i + 1]);
    CurVertex := i * 4;
    tu1 := (XChar mod 16) * m_CharTU;
    tv1 := (XChar div 16) * m_CharTV;
    tu2 := tu1 + m_CharTU;
    tv2 := tv1 + m_CharTV;
    x1 := X + Width + m_Props[XChar].OffsetX * ScaleX;
    y1 := Y + m_Props[XChar].OffsetY * ScaleY;
    x2 := x1 + m_CharWidth * ScaleX;
    y2 := y1 + m_CharHeight * ScaleY;
    Vertices^[CurVertex].x := x1;
    Vertices^[CurVertex].y := y2;
    Vertices^[CurVertex].z := 0;
    Vertices^[CurVertex].rhw := 1;
    Vertices^[CurVertex].Color := Color;
    Vertices^[CurVertex].tu := tu1;
    Vertices^[CurVertex].tv := tv2;
    Inc(CurVertex);
    Vertices^[CurVertex].x := x1;
    Vertices^[CurVertex].y := y1;
    Vertices^[CurVertex].z := 0;
    Vertices^[CurVertex].rhw := 1;
    Vertices^[CurVertex].Color := Color;
    Vertices^[CurVertex].tu := tu1;
    Vertices^[CurVertex].tv := tv1;
    Inc(CurVertex);
    Vertices^[CurVertex].x := x2;
    Vertices^[CurVertex].y := y2;
    Vertices^[CurVertex].z := 0;
    Vertices^[CurVertex].rhw := 1;
    Vertices^[CurVertex].Color := Color;
    Vertices^[CurVertex].tu := tu2;
    Vertices^[CurVertex].tv := tv2;
    Inc(CurVertex);
    Vertices^[CurVertex].x := x2;
    Vertices^[CurVertex].y := y1;
    Vertices^[CurVertex].z := 0;
    Vertices^[CurVertex].rhw := 1;
    Vertices^[CurVertex].Color := Color;
    Vertices^[CurVertex].tu := tu2;
    Vertices^[CurVertex].tv := tv1;
    Width := Width + m_Props[XChar].Width * ScaleX;
  end;
  m_Gfx.SharedVB2D.VB.Unlock;
  m_Gfx.Device.SetTexture(0, m_Texture.Texture);
  m_Gfx.SharedVB2D.SetToDevice;
  m_Gfx.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, Length(Text) * 4 - 2);
end;

function TG2Font.GetTextWidth(const Text: AnsiString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Text) do
  Result := Result + m_Props[Ord(Text[i])].Width;
end;

function TG2Font.GetTextHeight(const Text: AnsiString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Text) do
  Result := Max(Result, m_Props[Ord(Text[i])].Height);
end;

procedure TG2Font.Release;
begin
  if Assigned(m_Texture.Texture) then
  G2WriteLogTimed(AnsiString('(<) Font Released: (' + Name + ').'), 'Fonts');
  m_Texture.Release;
end;

function TG2Font.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Gfx := Core.Graphics;
  m_Texture := TG2Texture2D.Create;
  m_Texture.Name := 'Font_' + Name + '_Texture';
  m_Texture.Initialize(Core);
  m_FontFace := '';
  m_Size := 0;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(+) Font Initialized: (' + Name + ')'), 'Fonts');
end;

function TG2Font.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Release;
  m_Texture.Free;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(-) Font Finalized: (' + Name + ')'), 'Fonts');
end;
//TG2Font END


//TG2MeshMgr BEGIN
constructor TG2MeshMgr.Create;
begin
  inherited Create;
end;

destructor TG2MeshMgr.Destroy;
begin
  inherited Destroy;
end;

function TG2MeshMgr.CreateMeshFromFile(const Name: WideString; const f: WideString): TG2Mesh;
  var i: Integer;
  var LoaderClass: CG2MeshLoaderClass;
  var Loader: TG2MeshLoader;
  var MeshData: TG2MeshData;
begin
  LoaderClass := nil;
  for i := 0 to High(G2MeshLoaders) do
  if G2MeshLoaders[i].CanLoadFile(f) then
  begin
    LoaderClass := G2MeshLoaders[i];
    Break;
  end;
  if not Assigned(LoaderClass) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TG2Mesh.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  Loader := LoaderClass.Create;
  Loader.LoadFile(f);
  Loader.ExportMesh(Core.Graphics.Device, @MeshData);
  Result.LoadData(MeshData);
  Loader.Free;
  AddResource(Result);
end;

function TG2MeshMgr.CreateMeshFromBuffer(const Name: WideString; const Buffer: Pointer; const Size: Integer): TG2Mesh;
  var ms: TMemoryStream;
  var i: Integer;
  var LoaderClass: CG2MeshLoaderClass;
  var Loader: TG2MeshLoader;
  var MeshData: TG2MeshData;
begin
  ms := TMemoryStream.Create;
  ms.Write(Buffer^, Size);
  ms.Position := 0;
  LoaderClass := nil;
  for i := 0 to High(G2MeshLoaders) do
  if G2MeshLoaders[i].CanLoadStream(ms) then
  begin
    LoaderClass := G2MeshLoaders[i];
    Break;
  end;
  if not Assigned(LoaderClass) then
  begin
    Result := nil;
    ms.Free;
    Exit;
  end;
  Result := TG2Mesh.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  Loader := LoaderClass.Create;
  Loader.LoadStream(ms);
  Loader.ExportMesh(Core.Graphics.Device, @MeshData);
  Result.LoadData(MeshData);
  Loader.Free;
  AddResource(Result);
  ms.Free;
end;

function TG2MeshMgr.CreateMeshFromPack(const Name: WideString; const FolderName, FileName: AnsiString): TG2Mesh;
  var Data: Pointer;
  var DataSize: DWord;
begin
  Core.PackLinker.GetFileData(FolderName, FileName, Data, DataSize);
  if Assigned(Data) then
  Result := CreateMeshFromBuffer(Name, Data, DataSize)
  else
  Result := nil;
end;

function TG2MeshMgr.FindMesh(const Name: WideString): TG2Mesh;
begin
  Result := TG2Mesh(FindResource(Name));
end;

procedure TG2MeshMgr.AddMesh(const Mesh: TG2Mesh);
begin
  AddResource(Mesh);
end;

procedure TG2MeshMgr.DeleteMesh(const Index: Integer);
begin
  DeleteResource(Index);
end;

procedure TG2MeshMgr.RemoveMesh(const Mesh: TG2Mesh);
begin
  RemoveResource(Mesh);
end;
//TG2MeshMgr END

//TG2MeshBoneGroup BEGIN
procedure TG2MeshBoneGroup.IncludeBone(const NodeID: Integer; const SubBones: Boolean = False);
  var i: Integer;
  var AddNode: Boolean;
begin
  AddNode := True;
  for i := 0 to High(Nodes) do
  if Nodes[i] = NodeID then
  begin
    AddNode := False;
    Break;
  end;
  if AddNode then
  begin
    SetLength(Nodes, Length(Nodes) + 1);
    Nodes[High(Nodes)] := NodeID;
  end;
  if SubBones then
  for i := 0 to High(Mesh.Nodes[NodeID].SlaveID) do
  IncludeBone(Mesh.Nodes[NodeID].SlaveID[i], True);
end;

procedure TG2MeshBoneGroup.ExcludeBone(const NodeID: Integer; const SubBones: Boolean = False);
  var i, n: Integer;
begin
  n := -1;
  for i := 0 to High(Nodes) do
  if Nodes[i] = NodeID then
  begin
    n := i;
    Break;
  end;
  if n = -1 then
  Exit;
  for i := n to High(Nodes) - 1 do
  Nodes[i] := Nodes[i + 1];
  SetLength(Nodes, Length(Nodes) - 1);
  if SubBones then
  for i := 0 to High(Mesh.Nodes[NodeID].SlaveID) do
  ExcludeBone(Mesh.Nodes[NodeID].SlaveID[i], True);
end;

class operator TG2MeshBoneGroup.Add(const g1, g2: TG2MeshBoneGroup): TG2MeshBoneGroup;
  var i: Integer;
begin
  if g1.Mesh <> g2.Mesh then Exit;
  Result.Assign(g1);
  for i := 0 to High(g2.Nodes) do
  Result.IncludeBone(g2.Nodes[i]);
end;

class operator TG2MeshBoneGroup.Subtract(const g1, g2: TG2MeshBoneGroup): TG2MeshBoneGroup;
  var i: Integer;
begin
  if g1.Mesh <> g2.Mesh then Exit;
  Result.Assign(g1);
  if @Result <> @g2 then
  for i := 0 to High(g2.Nodes) do
  Result.ExcludeBone(g2.Nodes[i]);
end;

procedure TG2MeshBoneGroup.IncludeBone(const NodeName: AnsiString; const SubBones: Boolean = False);
  var i, n: Integer;
begin
  n := -1;
  for i := 0 to Mesh.NodeCount - 1 do
  if Mesh.Nodes[i].Name = NodeName then
  begin
    n := i;
    Break;
  end;
  if n = -1 then
  Exit;
  IncludeBone(n, SubBones);
end;

procedure TG2MeshBoneGroup.ExcludeBone(const NodeName: AnsiString; const SubBones: Boolean = False);
  var i, n: Integer;
begin
  n := -1;
  for i := 0 to Mesh.NodeCount - 1 do
  if Mesh.Nodes[i].Name = NodeName then
  begin
    n := i;
    Break;
  end;
  if n = -1 then
  Exit;
  ExcludeBone(n, SubBones);
end;

procedure TG2MeshBoneGroup.IncludeGroup(const BoneGroup: TG2MeshBoneGroup);
  var i: Integer;
begin
  for i := 0 to High(BoneGroup.Nodes) do
  IncludeBone(BoneGroup.Nodes[i]);
end;

procedure TG2MeshBoneGroup.ExcludeGroup(const BoneGroup: TG2MeshBoneGroup);
  var i: Integer;
begin
  for i := 0 to High(BoneGroup.Nodes) do
  ExcludeBone(BoneGroup.Nodes[i]);
end;

procedure TG2MeshBoneGroup.IncludeAll;
  var i: Integer;
begin
  SetLength(Nodes, Mesh.NodeCount);
  for i := 0 to Mesh.NodeCount - 1 do
  Nodes[i] := i;
end;

procedure TG2MeshBoneGroup.ExcludeAll;
begin
  SetLength(Nodes, 0);
end;

procedure TG2MeshBoneGroup.Assign(const BoneGroup: TG2MeshBoneGroup);
  var i: Integer;
begin
  Mesh := BoneGroup.Mesh;
  SetLength(Nodes, Length(BoneGroup.Nodes));
  for i := 0 to High(Nodes) do
  Nodes[i] := BoneGroup.Nodes[i];
end;
//TG2MeshBoneGroup END

//TG2MeshAnimBlend BEGIN
function TG2MeshAnimBlend.AddAnim(const AnimName: AnsiString; const BoneGroup: TG2MeshBoneGroup; const Weight: Single): Integer;
  var i, j: Integer;
begin
  Result := Mesh.AnimIndex(AnimName);
  if Result = -1 then
  Exit;
  i := Result;
  Result := Length(Anims);
  SetLength(Anims, Result + 1);
  Anims[Result].AnimID := i;
  Anims[Result].Weight := Weight;
  Anims[Result].Frame := 0;
  SetLength(Anims[Result].Bones, Mesh.Anims[Anims[Result].AnimID].NodeCount);
  for i := 0 to Mesh.Anims[Anims[Result].AnimID].NodeCount - 1 do
  begin
    Anims[Result].Bones[i] := False;
    for j := 0 to High(BoneGroup.Nodes) do
    if BoneGroup.Nodes[j] = Mesh.Anims[Anims[Result].AnimID].Nodes[i].NodeID then
    begin
      Anims[Result].Bones[i] := True;
      Break;
    end;
  end;
end;

procedure TG2MeshAnimBlend.SetAnimFrame(const AnimBlendID: Integer; const Frame: Single);
begin
  Anims[AnimBlendID].Frame := Frame;
end;
//TG2MeshAnimBlend END

//TG2Mesh BEGIN
function TG2Mesh.LoadData(const MeshData: TG2MeshData): TG2Result;
var
  i, j, k, b: Integer;
  Decl: TFVFDeclaration;
  VerticesSkinned: PG2MeshVertexSkinnedArr;
  VerticesStatic: PG2MeshVertexStaticArr;
  VerticesSkinnedFF: PG2MeshVertexSkinnedFFArr;
  VerticesStaticFF: PG2MeshVertexStaticFFArr;
  Indices: PG2Index16Array;
  WeightType: TD3DDeclType;
  Ptr: Pointer;
  VSkinned: PG2MeshVertexSkinned;
  PtrWeight, PtrBoneID: PFloatArray;
  Adj: array of DWord;
  MinV, MaxV: TG2Vec3;
  BMinV, BMaxV: TG2Vec3;
  BoneVertices: array of array of TG2Vec3;
  BoneVerticesCount: array of Integer;
  FVF: DWord;
begin
  NodeCount := MeshData.NodeCount;
  GeomCount := MeshData.GeomCount;
  AnimCount := MeshData.AnimCount;
  MaterialCount := MeshData.MaterialCount;
  RagdollCount := MeshData.RagDollCount;
  SetLength(Nodes, NodeCount);
  SetLength(Geoms, GeomCount);
  SetLength(Anims, AnimCount);
  SetLength(Materials, MaterialCount);
  SetLength(Ragdolls, RagdollCount);
  for i := 0 to NodeCount - 1 do
  begin
    Nodes[i].OwnerID := MeshData.Nodes[i].OwnerID;
    Nodes[i].Name := MeshData.Nodes[i].Name;
    Nodes[i].Transform := MeshData.Nodes[i].Transform;
    Nodes[i].SlaveID := nil;
  end;
  for i := 0 to NodeCount - 1 do
  if Nodes[i].OwnerID > -1 then
  begin
    SetLength(Nodes[Nodes[i].OwnerID].SlaveID, Length(Nodes[Nodes[i].OwnerID].SlaveID) + 1);
    Nodes[Nodes[i].OwnerID].SlaveID[High(Nodes[Nodes[i].OwnerID].SlaveID)] := i;
  end;
  for i := 0 to GeomCount - 1 do
  begin
    Geoms[i].NodeID := MeshData.Geoms[i].NodeID;
    Geoms[i].Skinned := MeshData.Geoms[i].SkinID > -1;
    case RenderMode of
      rmFF:
      begin
        if Geoms[i].Skinned then
        FVF := D3DFVF_XYZB5 or D3DFVF_LASTBETA_UBYTE4
        else
        FVF := D3DFVF_XYZ;
        FVF := FVF or D3DFVF_NORMAL or D3DFVF_TEX1;
        if Geoms[i].Skinned then
        begin
          Geoms[i].MaxWeights := 0;
          for j := 0 to MeshData.Geoms[i].VCount - 1 do
          Geoms[i].MaxWeights := Max(Geoms[i].MaxWeights, Min(4, MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].WeightCount));
          Geoms[i].BoneCount := MeshData.Skins[MeshData.Geoms[i].SkinID].BoneCount;
          SetLength(Geoms[i].Bones, Geoms[i].BoneCount);
          for j := 0 to Geoms[i].BoneCount - 1 do
          begin
            Geoms[i].Bones[j].NodeID := MeshData.Skins[MeshData.Geoms[i].SkinID].Bones[j].NodeID;
            Geoms[i].Bones[j].Bind := MeshData.Skins[MeshData.Geoms[i].SkinID].Bones[j].Bind;
          end;
          Geoms[i].VertexStride := SizeOf(TG2MeshVertexSkinnedFF);
        end
        else
        begin
          Geoms[i].BoneCount := 0;
          Geoms[i].VertexStride := SizeOf(TG2MeshVertexStaticFF);
        end;
        D3DXCreateMeshFVF(
          MeshData.Geoms[i].FCount,
          MeshData.Geoms[i].VCount,
          D3DXMESH_MANAGED,
          FVF,
          Core.Graphics.Device,
          Geoms[i].Mesh
        );
        MinV := MeshData.Geoms[i].Vertices[0].Position;
        MaxV := MinV;
        if Geoms[i].Skinned then
        begin
          SetLength(BoneVertices, Geoms[i].BoneCount, Geoms[i].Mesh.GetNumVertices);
          SetLength(BoneVerticesCount, Geoms[i].BoneCount);
          ZeroMemory(@BoneVerticesCount[0], Geoms[i].BoneCount * 4);
          Geoms[i].Mesh.LockVertexBuffer(0, Pointer(VerticesSkinnedFF));
          for j := 0 to Geoms[i].Mesh.GetNumVertices - 1 do
          begin
            if MeshData.Geoms[i].Vertices[j].Position.x > MaxV.x then MaxV.x := MeshData.Geoms[i].Vertices[j].Position.x;
            if MeshData.Geoms[i].Vertices[j].Position.y > MaxV.y then MaxV.y := MeshData.Geoms[i].Vertices[j].Position.y;
            if MeshData.Geoms[i].Vertices[j].Position.z > MaxV.z then MaxV.z := MeshData.Geoms[i].Vertices[j].Position.z;
            if MeshData.Geoms[i].Vertices[j].Position.x < MinV.x then MinV.x := MeshData.Geoms[i].Vertices[j].Position.x;
            if MeshData.Geoms[i].Vertices[j].Position.y < MinV.y then MinV.y := MeshData.Geoms[i].Vertices[j].Position.y;
            if MeshData.Geoms[i].Vertices[j].Position.z < MinV.z then MinV.z := MeshData.Geoms[i].Vertices[j].Position.z;
            VerticesSkinnedFF^[j].Position := MeshData.Geoms[i].Vertices[j].Position;
            VerticesSkinnedFF^[j].Normal := MeshData.Geoms[i].Vertices[j].Normal;
            if MeshData.Geoms[i].TCount > 0 then
            VerticesSkinnedFF^[j].TexCoords := MeshData.Geoms[i].Vertices[j].TexCoords[0]
            else
            VerticesSkinnedFF^[j].TexCoords.SetValue(0, 0);
            ZeroMemory(@VerticesSkinnedFF^[j].BWeights, 16);
            ZeroMemory(@VerticesSkinnedFF^[j].BIndices, 4);
            for b := 0 to Min(Geoms[i].MaxWeights - 1, MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].WeightCount - 1) do
            begin
              VerticesSkinnedFF^[j].BWeights[b] := MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].Weight;
              VerticesSkinnedFF^[j].BIndices[b] := MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].BoneID;
              BoneVertices[
                MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].BoneID,
                BoneVerticesCount[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].BoneID]
              ] := MeshData.Geoms[i].Vertices[j].Position;
              Inc(BoneVerticesCount[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].BoneID]);
            end;
          end;
          Geoms[i].Mesh.UnlockVertexBuffer;
          for b := 0 to Geoms[i].BoneCount - 1 do
          begin
            Geoms[i].Bones[b].VCount := BoneVerticesCount[b];
            if BoneVerticesCount[b] > 0 then
            begin
              BMinV := BoneVertices[b, 0];
              BMaxV := BMinV;
              for j := 1 to BoneVerticesCount[b] - 1 do
              begin
                if BoneVertices[b, j].x < BMinV.x then BMinV.x := BoneVertices[b, j].x;
                if BoneVertices[b, j].y < BMinV.y then BMinV.y := BoneVertices[b, j].y;
                if BoneVertices[b, j].z < BMinV.z then BMinV.z := BoneVertices[b, j].z;
                if BoneVertices[b, j].x > BMaxV.x then BMaxV.x := BoneVertices[b, j].x;
                if BoneVertices[b, j].y > BMaxV.y then BMaxV.y := BoneVertices[b, j].y;
                if BoneVertices[b, j].z > BMaxV.z then BMaxV.z := BoneVertices[b, j].z;
              end;
              Geoms[i].Bones[b].BBox.C := (BMinV + BMaxV) * 0.5;
              Geoms[i].Bones[b].BBox.vx.SetValue((BMaxV.x - BMinV.x) * 0.5, 0, 0);
              Geoms[i].Bones[b].BBox.vy.SetValue(0, (BMaxV.y - BMinV.y) * 0.5, 0);
              Geoms[i].Bones[b].BBox.vz.SetValue(0, 0, (BMaxV.z - BMinV.z) * 0.5);
            end;
          end;
        end
        else
        begin
          Geoms[i].Mesh.LockVertexBuffer(0, Pointer(VerticesStaticFF));
          for j := 0 to Geoms[i].Mesh.GetNumVertices - 1 do
          begin
            if MeshData.Geoms[i].Vertices[j].Position.x > MaxV.x then MaxV.x := MeshData.Geoms[i].Vertices[j].Position.x;
            if MeshData.Geoms[i].Vertices[j].Position.y > MaxV.y then MaxV.y := MeshData.Geoms[i].Vertices[j].Position.y;
            if MeshData.Geoms[i].Vertices[j].Position.z > MaxV.z then MaxV.z := MeshData.Geoms[i].Vertices[j].Position.z;
            if MeshData.Geoms[i].Vertices[j].Position.x < MinV.x then MinV.x := MeshData.Geoms[i].Vertices[j].Position.x;
            if MeshData.Geoms[i].Vertices[j].Position.y < MinV.y then MinV.y := MeshData.Geoms[i].Vertices[j].Position.y;
            if MeshData.Geoms[i].Vertices[j].Position.z < MinV.z then MinV.z := MeshData.Geoms[i].Vertices[j].Position.z;
            VerticesStaticFF^[j].Position := MeshData.Geoms[i].Vertices[j].Position;
            VerticesStaticFF^[j].Normal := MeshData.Geoms[i].Vertices[j].Normal;
            if MeshData.Geoms[i].TCount > 0 then
            VerticesStaticFF^[j].TexCoords := MeshData.Geoms[i].Vertices[j].TexCoords[0]
            else
            VerticesStaticFF^[j].TexCoords.SetValue(0, 0);
          end;
          Geoms[i].Mesh.UnlockVertexBuffer;
        end;
        Geoms[i].BBox.c := (MinV + MaxV) * 0.5;
        Geoms[i].BBox.vx.SetValue(MaxV.x - Geoms[i].BBox.c.x, 0, 0);
        Geoms[i].BBox.vy.SetValue(0, MaxV.y - Geoms[i].BBox.c.y, 0);
        Geoms[i].BBox.vz.SetValue(0, 0, MaxV.z - Geoms[i].BBox.c.z);
        Geoms[i].Mesh.LockIndexBuffer(0, Pointer(Indices));
        for j := 0 to Geoms[i].Mesh.GetNumFaces - 1 do
        begin
          for k := 0 to 2 do
          Indices^[j * 3 + k] := MeshData.Geoms[i].Faces[j].Indices[k];
        end;
        Geoms[i].Mesh.UnlockIndexBuffer;
        Geoms[i].MaterialCount := MeshData.Geoms[i].MCount;
        SetLength(Geoms[i].Materials, Geoms[i].MaterialCount);
        for j := 0 to Geoms[i].MaterialCount - 1 do
        Geoms[i].Materials[j] := MeshData.Geoms[i].Materials[j];
        if Geoms[i].MaterialCount > 0 then
        begin
          Geoms[i].Mesh.LockAttributeBuffer(0, PDWord(Ptr));
          for j := 0 to Geoms[i].Mesh.GetNumFaces - 1 do
          PDWordArray(Ptr)^[j] := MeshData.Geoms[i].Faces[j].MaterialID;
          Geoms[i].Mesh.UnlockAttributeBuffer;
        end;
        SetLength(Adj, Geoms[i].Mesh.GetNumFaces * 3);
        Geoms[i].Mesh.GenerateAdjacency(G2EPS, @Adj[0]);
        Geoms[i].Mesh.OptimizeInplace(
          D3DXMESHOPT_COMPACT
          or D3DXMESHOPT_VERTEXCACHE
          or D3DXMESHOPT_ATTRSORT,
          @Adj[0], nil, nil, nil
        );
        Geoms[i].Visible := True;
      end;
      rmSM3:
      begin
        Decl[0] := D3DVertexElement(0, 4 * 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0);
        Decl[1] := D3DVertexElement(0, 4 * 3, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0);
        Decl[2] := D3DVertexElement(0, 4 * 6, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0);
        Decl[3] := D3DVertexElement(0, 4 * 9, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0);
        Decl[4] := D3DVertexElement(0, 4 * 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0);
        if Geoms[i].Skinned then
        begin
          Geoms[i].Technique := m_Effect.GetTechniqueByName('g2sm3sk');
          Geoms[i].MaxWeights := 0;
          for j := 0 to MeshData.Geoms[i].VCount - 1 do
          Geoms[i].MaxWeights := Max(Geoms[i].MaxWeights, Min(4, MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].WeightCount));
          case Geoms[i].MaxWeights of
            0: WeightType := D3DDECLTYPE_UNUSED;
            1: WeightType := D3DDECLTYPE_FLOAT1;
            2: WeightType := D3DDECLTYPE_FLOAT2;
            3: WeightType := D3DDECLTYPE_FLOAT3;
            else WeightType := D3DDECLTYPE_FLOAT4;
          end;
          Geoms[i].BoneCount := MeshData.Skins[MeshData.Geoms[i].SkinID].BoneCount;
          SetLength(Geoms[i].Bones, Geoms[i].BoneCount);
          for j := 0 to Geoms[i].BoneCount - 1 do
          begin
            Geoms[i].Bones[j].NodeID := MeshData.Skins[MeshData.Geoms[i].SkinID].Bones[j].NodeID;
            Geoms[i].Bones[j].Bind := MeshData.Skins[MeshData.Geoms[i].SkinID].Bones[j].Bind;
          end;
          if Geoms[i].MaxWeights = 1 then
          begin
            Decl[5] := D3DVertexElement(0, 4 * 14, WeightType, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDINDICES, 0);
            Decl[6] := D3DDECL_END;
            Geoms[i].VertexStride := 60;
          end
          else
          begin
            Decl[5] := D3DVertexElement(0, 4 * 14, WeightType, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDINDICES, 0);
            Decl[6] := D3DVertexElement(0, 4 * (14 + Geoms[i].MaxWeights), WeightType, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDWEIGHT, 0);
            Decl[7] := D3DDECL_END;
            Geoms[i].VertexStride := 56 + Geoms[i].MaxWeights * 8;
          end;
        end
        else
        begin
          Geoms[i].Technique := m_Effect.GetTechniqueByName('g2sm3st');
          Geoms[i].BoneCount := 0;
          Decl[5] := D3DDECL_END;
          Geoms[i].VertexStride := 56;
        end;
        D3DXCreateMesh(
          MeshData.Geoms[i].FCount,
          MeshData.Geoms[i].VCount,
          D3DXMESH_MANAGED,
          @Decl[0],
          Core.Graphics.Device,
          Geoms[i].Mesh
        );
        MinV := MeshData.Geoms[i].Vertices[0].Position;
        MaxV := MinV;
        if Geoms[i].Skinned then
        begin
          SetLength(BoneVertices, Geoms[i].BoneCount, Geoms[i].Mesh.GetNumVertices);
          SetLength(BoneVerticesCount, Geoms[i].BoneCount);
          ZeroMemory(@BoneVerticesCount[0], Geoms[i].BoneCount * 4);
          Geoms[i].Mesh.LockVertexBuffer(0, Ptr);
          for j := 0 to Geoms[i].Mesh.GetNumVertices - 1 do
          begin
            if MeshData.Geoms[i].Vertices[j].Position.x > MaxV.x then MaxV.x := MeshData.Geoms[i].Vertices[j].Position.x;
            if MeshData.Geoms[i].Vertices[j].Position.y > MaxV.y then MaxV.y := MeshData.Geoms[i].Vertices[j].Position.y;
            if MeshData.Geoms[i].Vertices[j].Position.z > MaxV.z then MaxV.z := MeshData.Geoms[i].Vertices[j].Position.z;
            if MeshData.Geoms[i].Vertices[j].Position.x < MinV.x then MinV.x := MeshData.Geoms[i].Vertices[j].Position.x;
            if MeshData.Geoms[i].Vertices[j].Position.y < MinV.y then MinV.y := MeshData.Geoms[i].Vertices[j].Position.y;
            if MeshData.Geoms[i].Vertices[j].Position.z < MinV.z then MinV.z := MeshData.Geoms[i].Vertices[j].Position.z;
            if Geoms[i].MaxWeights = 1 then
            VSkinned := PG2MeshVertexSkinned(Integer(Ptr) + (SizeOf(TG2MeshVertexStatic) + 4) * j)
            else
            VSkinned := PG2MeshVertexSkinned(Integer(Ptr) + (SizeOf(TG2MeshVertexStatic) + Geoms[i].MaxWeights * 8) * j);
            PtrBoneID := PFloatArray(Integer(VSkinned) + SizeOf(TG2MeshVertexStatic));
            PtrWeight := PFloatArray(Integer(PtrBoneID) + Geoms[i].MaxWeights * 4);
            VSkinned^.Position := MeshData.Geoms[i].Vertices[j].Position;
            VSkinned^.Tangent := MeshData.Geoms[i].Vertices[j].Tangent;
            VSkinned^.Binormal := MeshData.Geoms[i].Vertices[j].Binormal;
            VSkinned^.Normal := MeshData.Geoms[i].Vertices[j].Normal;
            if MeshData.Geoms[i].TCount > 0 then
            VSkinned^.TexCoords := MeshData.Geoms[i].Vertices[j].TexCoords[0]
            else
            VSkinned^.TexCoords.SetValue(0, 0);
            if Geoms[i].MaxWeights = 1 then
            begin
              ZeroMemory(@PtrBoneID^[0], Geoms[i].MaxWeights * 4);
              PtrBoneID^[0] := MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[0].BoneID;
              BoneVertices[
                MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[0].BoneID,
                BoneVerticesCount[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[0].BoneID]
              ] := MeshData.Geoms[i].Vertices[j].Position;
              Inc(BoneVerticesCount[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[0].BoneID]);
            end
            else
            begin
              ZeroMemory(@PtrBoneID^[0], Geoms[i].MaxWeights * 8);
              for b := 0 to Min(Geoms[i].MaxWeights - 1, MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].WeightCount - 1) do
              begin
                PtrBoneID^[b] := MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].BoneID;
                PtrWeight^[b] := MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].Weight;
                BoneVertices[
                  MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].BoneID,
                  BoneVerticesCount[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].BoneID]
                ] := MeshData.Geoms[i].Vertices[j].Position;
                Inc(BoneVerticesCount[MeshData.Skins[MeshData.Geoms[i].SkinID].Vertices[j].Weights[b].BoneID]);
              end;
            end;
          end;
          Geoms[i].Mesh.UnlockVertexBuffer;
          for b := 0 to Geoms[i].BoneCount - 1 do
          begin
            Geoms[i].Bones[b].VCount := BoneVerticesCount[b];
            if BoneVerticesCount[b] > 0 then
            begin
              BMinV := BoneVertices[b, 0];
              BMaxV := BMinV;
              for j := 1 to BoneVerticesCount[b] - 1 do
              begin
                if BoneVertices[b, j].x < BMinV.x then BMinV.x := BoneVertices[b, j].x;
                if BoneVertices[b, j].y < BMinV.y then BMinV.y := BoneVertices[b, j].y;
                if BoneVertices[b, j].z < BMinV.z then BMinV.z := BoneVertices[b, j].z;
                if BoneVertices[b, j].x > BMaxV.x then BMaxV.x := BoneVertices[b, j].x;
                if BoneVertices[b, j].y > BMaxV.y then BMaxV.y := BoneVertices[b, j].y;
                if BoneVertices[b, j].z > BMaxV.z then BMaxV.z := BoneVertices[b, j].z;
              end;
              Geoms[i].Bones[b].BBox.C := (BMinV + BMaxV) * 0.5;
              Geoms[i].Bones[b].BBox.vx.SetValue((BMaxV.x - BMinV.x) * 0.5, 0, 0);
              Geoms[i].Bones[b].BBox.vy.SetValue(0, (BMaxV.y - BMinV.y) * 0.5, 0);
              Geoms[i].Bones[b].BBox.vz.SetValue(0, 0, (BMaxV.z - BMinV.z) * 0.5);
            end;
          end;
        end
        else
        begin
          Geoms[i].Mesh.LockVertexBuffer(0, Pointer(VerticesStatic));
          for j := 0 to Geoms[i].Mesh.GetNumVertices - 1 do
          begin
            if MeshData.Geoms[i].Vertices[j].Position.x > MaxV.x then MaxV.x := MeshData.Geoms[i].Vertices[j].Position.x;
            if MeshData.Geoms[i].Vertices[j].Position.y > MaxV.y then MaxV.y := MeshData.Geoms[i].Vertices[j].Position.y;
            if MeshData.Geoms[i].Vertices[j].Position.z > MaxV.z then MaxV.z := MeshData.Geoms[i].Vertices[j].Position.z;
            if MeshData.Geoms[i].Vertices[j].Position.x < MinV.x then MinV.x := MeshData.Geoms[i].Vertices[j].Position.x;
            if MeshData.Geoms[i].Vertices[j].Position.y < MinV.y then MinV.y := MeshData.Geoms[i].Vertices[j].Position.y;
            if MeshData.Geoms[i].Vertices[j].Position.z < MinV.z then MinV.z := MeshData.Geoms[i].Vertices[j].Position.z;
            VerticesStatic^[j].Position := MeshData.Geoms[i].Vertices[j].Position;
            VerticesStatic^[j].Tangent := MeshData.Geoms[i].Vertices[j].Tangent;
            VerticesStatic^[j].Binormal := MeshData.Geoms[i].Vertices[j].Binormal;
            VerticesStatic^[j].Normal := MeshData.Geoms[i].Vertices[j].Normal;
            if MeshData.Geoms[i].TCount > 0 then
            VerticesStatic^[j].TexCoords := MeshData.Geoms[i].Vertices[j].TexCoords[0]
            else
            VerticesStatic^[j].TexCoords.SetValue(0, 0);
          end;
          Geoms[i].Mesh.UnlockVertexBuffer;
        end;
        Geoms[i].BBox.c := (MinV + MaxV) * 0.5;
        Geoms[i].BBox.vx.SetValue(MaxV.x - Geoms[i].BBox.c.x, 0, 0);
        Geoms[i].BBox.vy.SetValue(0, MaxV.y - Geoms[i].BBox.c.y, 0);
        Geoms[i].BBox.vz.SetValue(0, 0, MaxV.z - Geoms[i].BBox.c.z);
        Geoms[i].Mesh.LockIndexBuffer(0, Pointer(Indices));
        for j := 0 to Geoms[i].Mesh.GetNumFaces - 1 do
        begin
          for k := 0 to 2 do
          Indices^[j * 3 + k] := MeshData.Geoms[i].Faces[j].Indices[k];
        end;
        Geoms[i].Mesh.UnlockIndexBuffer;
        Geoms[i].MaterialCount := MeshData.Geoms[i].MCount;
        SetLength(Geoms[i].Materials, Geoms[i].MaterialCount);
        for j := 0 to Geoms[i].MaterialCount - 1 do
        Geoms[i].Materials[j] := MeshData.Geoms[i].Materials[j];
        if Geoms[i].MaterialCount > 0 then
        begin
          Geoms[i].Mesh.LockAttributeBuffer(0, PDWord(Ptr));
          for j := 0 to Geoms[i].Mesh.GetNumFaces - 1 do
          PDWordArray(Ptr)^[j] := MeshData.Geoms[i].Faces[j].MaterialID;
          Geoms[i].Mesh.UnlockAttributeBuffer;
        end;
        SetLength(Adj, Geoms[i].Mesh.GetNumFaces * 3);
        Geoms[i].Mesh.GenerateAdjacency(G2EPS, @Adj[0]);
        Geoms[i].Mesh.OptimizeInplace(
          D3DXMESHOPT_COMPACT
          or D3DXMESHOPT_VERTEXCACHE
          or D3DXMESHOPT_ATTRSORT,
          @Adj[0], nil, nil, nil
        );
        Geoms[i].Visible := True;
      end;
    end;
  end;
  for i := 0 to AnimCount - 1 do
  begin
    Anims[i].Name := MeshData.Anims[i].Name;
    Anims[i].FrameRate := MeshData.Anims[i].FrameRate;
    Anims[i].FrameCount := MeshData.Anims[i].FrameCount;
    Anims[i].NodeCount := MeshData.Anims[i].NodeCount;
    SetLength(Anims[i].Nodes, Anims[i].NodeCount);
    for j := 0 to Anims[i].NodeCount - 1 do
    begin
      Anims[i].Nodes[j].NodeID := MeshData.Anims[i].Nodes[j].NodeID;
      SetLength(Anims[i].Nodes[j].Frames, Anims[i].FrameCount);
      for k := 0 to Anims[i].FrameCount - 1 do
      begin
        Anims[i].Nodes[j].Frames[k].Scale := MeshData.Anims[i].Nodes[j].Frames[k].Scaling;
        Anims[i].Nodes[j].Frames[k].Rotation := MeshData.Anims[i].Nodes[j].Frames[k].Rotation;
        Anims[i].Nodes[j].Frames[k].Translation := MeshData.Anims[i].Nodes[j].Frames[k].Translation;
      end;
    end;
  end;
  for i := 0 to MaterialCount - 1 do
  begin
    Materials[i].Name := MeshData.Materials[i].Channels[0].Name;
    Materials[i].TwoSided := MeshData.Materials[i].Channels[0].TwoSided;
    Materials[i].AmbientColor := MeshData.Materials[i].Channels[0].AmbientColor;
    Materials[i].DiffuseColor := MeshData.Materials[i].Channels[0].DiffuseColor;
    Materials[i].SpecularColor := MeshData.Materials[i].Channels[0].SpecularColor;
    Materials[i].SpecularColorAmount := MeshData.Materials[i].Channels[0].SpecularColorAmount;
    Materials[i].SpecularPower := MeshData.Materials[i].Channels[0].SpecularPower;
    Materials[i].EmmissiveColor := MeshData.Materials[i].Channels[0].EmmissiveColor;
    Materials[i].EmmissiveColorAmount := MeshData.Materials[i].Channels[0].EmmissiveColorAmount;
    Materials[i].AmbientMapEnable := MeshData.Materials[i].Channels[0].AmbientMapEnable;
    Materials[i].AmbientMap := MeshData.Materials[i].Channels[0].AmbientMap;
    Materials[i].AmbientMapAmount := MeshData.Materials[i].Channels[0].AmbientMapAmount;
    Materials[i].DiffuseMapEnable := MeshData.Materials[i].Channels[0].DiffuseMapEnable;
    Materials[i].DiffuseMap := MeshData.Materials[i].Channels[0].DiffuseMap;
    Materials[i].DiffuseMapAmount := MeshData.Materials[i].Channels[0].DiffuseMapAmount;
    Materials[i].SpecularMapEnable := MeshData.Materials[i].Channels[0].SpecularMapEnable;
    Materials[i].SpecularMap := MeshData.Materials[i].Channels[0].SpecularMap;
    Materials[i].SpecularMapAmount := MeshData.Materials[i].Channels[0].SpecularMapAmount;
    Materials[i].OpacityMapEnable := MeshData.Materials[i].Channels[0].OpacityMapEnable;
    Materials[i].OpacityMap := MeshData.Materials[i].Channels[0].OpacityMap;
    Materials[i].OpacityMapAmount := MeshData.Materials[i].Channels[0].OpacityMapAmount;
    Materials[i].LightMapEnable := MeshData.Materials[i].Channels[0].LightMapEnable;
    Materials[i].LightMap := MeshData.Materials[i].Channels[0].LightMap;
    Materials[i].LightMapAmount := MeshData.Materials[i].Channels[0].LightMapAmount;
    Materials[i].NormalMapEnable := MeshData.Materials[i].Channels[0].NormalMapEnable;
    Materials[i].NormalMap := MeshData.Materials[i].Channels[0].NormalMap;
    Materials[i].NormalMapAmount := MeshData.Materials[i].Channels[0].NormalMapAmount;
  end;
  for i := 0 to RagdollCount - 1 do
  begin
    Ragdolls[i].NodeID := MeshData.RagDolls[i].NodeID;
    Ragdolls[i].BodyNodeCount := MeshData.RagDolls[i].BodyNodeCount;
    Ragdolls[i].ArmRNodeCount := MeshData.RagDolls[i].ArmRNodeCount;
    Ragdolls[i].ArmLNodeCount := MeshData.RagDolls[i].ArmLNodeCount;
    Ragdolls[i].LegRNodeCount := MeshData.RagDolls[i].LegRNodeCount;
    Ragdolls[i].LegLNodeCount := MeshData.RagDolls[i].LegLNodeCount;
    SetLength(Ragdolls[i].BodyNodes, Ragdolls[i].BodyNodeCount);
    SetLength(Ragdolls[i].ArmRNodes, Ragdolls[i].ArmRNodeCount);
    SetLength(Ragdolls[i].ArmLNodes, Ragdolls[i].ArmLNodeCount);
    SetLength(Ragdolls[i].LegRNodes, Ragdolls[i].LegRNodeCount);
    SetLength(Ragdolls[i].LegLNodes, Ragdolls[i].LegLNodeCount);
    G2MeshRagdollObjectCopy(@MeshData.RagDolls[i].Head, @Ragdolls[i].Head);
    G2MeshRagdollObjectCopy(@MeshData.RagDolls[i].Neck, @Ragdolls[i].Neck);
    G2MeshRagdollObjectCopy(@MeshData.RagDolls[i].Pelvis, @Ragdolls[i].Pelvis);
    for j := 0 to Ragdolls[i].BodyNodeCount - 1 do
    G2MeshRagdollObjectCopy(@MeshData.RagDolls[i].BodyNodes[j], @Ragdolls[i].BodyNodes[j]);
    for j := 0 to Ragdolls[i].ArmRNodeCount - 1 do
    G2MeshRagdollObjectCopy(@MeshData.RagDolls[i].ArmRNodes[j], @Ragdolls[i].ArmRNodes[j]);
    for j := 0 to Ragdolls[i].ArmLNodeCount - 1 do
    G2MeshRagdollObjectCopy(@MeshData.RagDolls[i].ArmLNodes[j], @Ragdolls[i].ArmLNodes[j]);
    for j := 0 to Ragdolls[i].LegRNodeCount - 1 do
    G2MeshRagdollObjectCopy(@MeshData.RagDolls[i].LegRNodes[j], @Ragdolls[i].LegRNodes[j]);
    for j := 0 to Ragdolls[i].LegLNodeCount - 1 do
    G2MeshRagdollObjectCopy(@MeshData.RagDolls[i].LegLNodes[j], @Ragdolls[i].LegLNodes[j]);
  end;
end;

function TG2Mesh.ExtractData: TG2MeshData;
  type TSkin = record
    var SkinID: Integer;
    var GeomID: Integer;
  end;
  var Skins: array of TSkin;
  function AddSkin(const GeomID: Integer): Integer;
  begin
    SetLength(Skins, Length(Skins) + 1);
    Skins[High(Skins)].SkinID := High(Skins);
    Skins[High(Skins)].GeomID := GeomID;
    Result := High(Skins);
  end;
  var i, j, n: Integer;
  var VStatic: PG2MeshVertexStaticArr;
  var VSkinned: PG2MeshVertexSkinnedArr;
  var VStaticFF: PG2MeshVertexStaticFFArr;
  var VSkinnedFF: PG2MeshVertexSkinnedFFArr;
  var Ind: PG2Index16Array;
  var Att: PDWordArray;
begin
  Result.NodeCount := NodeCount;
  SetLength(Result.Nodes, Result.NodeCount);
  for i := 0 to Result.NodeCount - 1 do
  begin
    Result.Nodes[i].OwnerID := Nodes[i].OwnerID;
    Result.Nodes[i].Name := Nodes[i].Name;
    Result.Nodes[i].Transform := Nodes[i].Transform;
  end;
  Result.GeomCount := GeomCount;
  SetLength(Result.Geoms, Result.GeomCount);
  for i := 0 to Result.GeomCount - 1 do
  begin
    Result.Geoms[i].NodeID := Geoms[i].NodeID;
    Result.Geoms[i].VCount := Geoms[i].Mesh.GetNumVertices;
    Result.Geoms[i].FCount := Geoms[i].Mesh.GetNumFaces;
    Result.Geoms[i].TCount := 1;
    Result.Geoms[i].MCount := Geoms[i].MaterialCount;
    SetLength(Result.Geoms[i].Vertices, Result.Geoms[i].VCount);
    if TG2Mesh.RenderMode = rmFF then
    begin
      if Geoms[i].Skinned then
      begin
        Result.Geoms[i].SkinID := AddSkin(i);
        Geoms[i].Mesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(VSkinnedFF));
        for j := 0 to Result.Geoms[i].VCount - 1 do
        begin
          Result.Geoms[i].Vertices[j].Position := VSkinnedFF^[j].Position;
          Result.Geoms[i].Vertices[j].Tangent.SetValue(0, 0, 0);
          Result.Geoms[i].Vertices[j].Binormal.SetValue(0, 0, 0);
          Result.Geoms[i].Vertices[j].Normal := VSkinnedFF^[j].Normal;
          SetLength(Result.Geoms[i].Vertices[j].TexCoords, 1);
          Result.Geoms[i].Vertices[j].TexCoords[0] := VSkinnedFF^[j].TexCoords;
          Result.Geoms[i].Vertices[j].Color := $ffffffff;
        end;
        Geoms[i].Mesh.UnlockVertexBuffer;
      end
      else
      begin
        Result.Geoms[i].SkinID := -1;
        Geoms[i].Mesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(VStaticFF));
        for j := 0 to Result.Geoms[i].VCount - 1 do
        begin
          Result.Geoms[i].Vertices[j].Position := VStaticFF^[j].Position;
          Result.Geoms[i].Vertices[j].Tangent.SetValue(0, 0, 0);
          Result.Geoms[i].Vertices[j].Binormal.SetValue(0, 0, 0);
          Result.Geoms[i].Vertices[j].Normal := VStaticFF^[j].Normal;
          SetLength(Result.Geoms[i].Vertices[j].TexCoords, 1);
          Result.Geoms[i].Vertices[j].TexCoords[0] := VStaticFF^[j].TexCoords;
          Result.Geoms[i].Vertices[j].Color := $ffffffff;
        end;
        Geoms[i].Mesh.UnlockVertexBuffer;
      end;
    end
    else
    begin
      if Geoms[i].Skinned then
      begin
        Result.Geoms[i].SkinID := AddSkin(i);
        Geoms[i].Mesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(VSkinned));
        for j := 0 to Result.Geoms[i].VCount - 1 do
        begin
          Result.Geoms[i].Vertices[j].Position := VSkinned^[j].Position;
          Result.Geoms[i].Vertices[j].Tangent := VSkinned^[j].Tangent;
          Result.Geoms[i].Vertices[j].Binormal := VSkinned^[j].Binormal;
          Result.Geoms[i].Vertices[j].Normal := VSkinned^[j].Normal;
          SetLength(Result.Geoms[i].Vertices[j].TexCoords, 1);
          Result.Geoms[i].Vertices[j].TexCoords[0] := VSkinned^[j].TexCoords;
          Result.Geoms[i].Vertices[j].Color := $ffffffff;
        end;
        Geoms[i].Mesh.UnlockVertexBuffer;
      end
      else
      begin
        Result.Geoms[i].SkinID := -1;
        Geoms[i].Mesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(VStatic));
        for j := 0 to Result.Geoms[i].VCount - 1 do
        begin
          Result.Geoms[i].Vertices[j].Position := VStatic^[j].Position;
          Result.Geoms[i].Vertices[j].Tangent := VStatic^[j].Tangent;
          Result.Geoms[i].Vertices[j].Binormal := VStatic^[j].Binormal;
          Result.Geoms[i].Vertices[j].Normal := VStatic^[j].Normal;
          SetLength(Result.Geoms[i].Vertices[j].TexCoords, 1);
          Result.Geoms[i].Vertices[j].TexCoords[0] := VStatic^[j].TexCoords;
          Result.Geoms[i].Vertices[j].Color := $ffffffff;
        end;
        Geoms[i].Mesh.UnlockVertexBuffer;
      end;
    end;
    SetLength(Result.Geoms[i].Faces, Result.Geoms[i].FCount);
    Geoms[i].Mesh.LockIndexBuffer(D3DLOCK_READONLY, Pointer(Ind));
    Geoms[i].Mesh.LockAttributeBuffer(D3DLOCK_READONLY, PDword(Att));
    for j := 0 to Result.Geoms[i].FCount - 1 do
    begin
      Result.Geoms[i].Faces[j].Indices[0] := Ind^[j * 3 + 0];
      Result.Geoms[i].Faces[j].Indices[1] := Ind^[j * 3 + 1];
      Result.Geoms[i].Faces[j].Indices[2] := Ind^[j * 3 + 2];
      Result.Geoms[i].Faces[j].MaterialID := Att^[j];
    end;
    Geoms[i].Mesh.UnlockIndexBuffer;
    Geoms[i].Mesh.UnlockAttributeBuffer;
    SetLength(Result.Geoms[i].Materials, Result.Geoms[i].MCount);
    for j := 0 to Result.Geoms[i].MCount - 1 do
    Result.Geoms[i].Materials[j] := Geoms[i].Materials[j];
  end;
  Result.SkinCount := Length(Skins);
  SetLength(Result.Skins, Result.SkinCount);
  for i := 0 to Result.SkinCount - 1 do
  begin
    Result.Skins[i].GeomID := Skins[i].GeomID;
    Result.Skins[i].MaxWeights := Geoms[Skins[i].GeomID].MaxWeights;
    Result.Skins[i].BoneCount := Geoms[Skins[i].GeomID].BoneCount;
    SetLength(Result.Skins[i].Bones, Result.Skins[i].BoneCount);
    for j := 0 to Result.Skins[i].BoneCount - 1 do
    begin
      Result.Skins[i].Bones[j].NodeID := Geoms[Skins[i].GeomID].Bones[j].NodeID;
      Result.Skins[i].Bones[j].Bind := Geoms[Skins[i].GeomID].Bones[j].Bind;
    end;
    SetLength(Result.Skins[i].Vertices, Result.Geoms[Skins[i].GeomID].VCount);
    if TG2Mesh.RenderMode = rmFF then
    begin
      Geoms[Skins[i].GeomID].Mesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(VSkinnedFF));
      for j := 0 to Result.Geoms[Skins[i].GeomID].VCount - 1 do
      begin
        Result.Skins[i].Vertices[j].WeightCount := Result.Skins[i].MaxWeights;
        for n := 0 to Result.Skins[i].MaxWeights - 1 do
        begin
          Result.Skins[i].Vertices[j].Weights[n].BoneID := Round(VSkinnedFF^[j].BIndices[n]);
          Result.Skins[i].Vertices[j].Weights[n].Weight := VSkinnedFF^[j].BWeights[n];
        end;
      end;
      Geoms[Skins[i].GeomID].Mesh.UnlockVertexBuffer;
    end
    else
    begin
      Geoms[Skins[i].GeomID].Mesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(VSkinned));
      for j := 0 to Result.Geoms[Skins[i].GeomID].VCount - 1 do
      begin
        Result.Skins[i].Vertices[j].WeightCount := Result.Skins[i].MaxWeights;
        for n := 0 to Result.Skins[i].MaxWeights - 1 do
        begin
          Result.Skins[i].Vertices[j].Weights[n].BoneID := Round(VSkinned^[j].BoneIndices[n]);
          Result.Skins[i].Vertices[j].Weights[n].Weight := VSkinned^[j].BoneWeights[n];
        end;
      end;
      Geoms[Skins[i].GeomID].Mesh.UnlockVertexBuffer;
    end;
  end;
  Result.AnimCount := AnimCount;
  SetLength(Result.Anims, Result.AnimCount);
  for i := 0 to Result.AnimCount - 1 do
  begin
    Result.Anims[i].Name := Anims[i].Name;
    Result.Anims[i].FrameRate := Anims[i].FrameRate;
    Result.Anims[i].FrameCount := Anims[i].FrameCount;
    Result.Anims[i].NodeCount := Anims[i].NodeCount;
    SetLength(Result.Anims[i].Nodes, Result.Anims[i].NodeCount);
    for j := 0 to Result.Anims[i].NodeCount - 1 do
    begin
      Result.Anims[i].Nodes[j].NodeID := Anims[i].Nodes[j].NodeID;
      SetLength(Result.Anims[i].Nodes[j].Frames, Result.Anims[i].FrameCount);
      for n := 0 to Result.Anims[i].FrameCount - 1 do
      begin
        Result.Anims[i].Nodes[j].Frames[n].Scaling := Anims[i].Nodes[j].Frames[n].Scale;
        Result.Anims[i].Nodes[j].Frames[n].Rotation := Anims[i].Nodes[j].Frames[n].Rotation;
        Result.Anims[i].Nodes[j].Frames[n].Translation := Anims[i].Nodes[j].Frames[n].Translation;
      end;
    end;
  end;
  Result.MaterialCount := MaterialCount;
  SetLength(Result.Materials, Result.MaterialCount);
  for i := 0 to Result.MaterialCount - 1 do
  begin
    Result.Materials[i].ChannelCount := 1;
    SetLength(Result.Materials[i].Channels, 1);
    Result.Materials[i].Channels[0].Name := Materials[i].Name;
    Result.Materials[i].Channels[0].TwoSided := Materials[i].TwoSided;
    Result.Materials[i].Channels[0].AmbientColor := Materials[i].AmbientColor;
    Result.Materials[i].Channels[0].DiffuseColor := Materials[i].DiffuseColor;
    Result.Materials[i].Channels[0].SpecularColor := Materials[i].SpecularColor;
    Result.Materials[i].Channels[0].SpecularColorAmount := Materials[i].SpecularColorAmount;
    Result.Materials[i].Channels[0].SpecularPower := Materials[i].SpecularPower;
    Result.Materials[i].Channels[0].EmmissiveColor := Materials[i].EmmissiveColor;
    Result.Materials[i].Channels[0].EmmissiveColorAmount := Materials[i].EmmissiveColorAmount;
    Result.Materials[i].Channels[0].AmbientMapEnable := Materials[i].AmbientMapEnable;
    Result.Materials[i].Channels[0].AmbientMap := Materials[i].AmbientMap;
    Result.Materials[i].Channels[0].AmbientMapAmount := Materials[i].AmbientMapAmount;
    Result.Materials[i].Channels[0].DiffuseMapEnable := Materials[i].DiffuseMapEnable;
    Result.Materials[i].Channels[0].DiffuseMap := Materials[i].DiffuseMap;
    Result.Materials[i].Channels[0].DiffuseMapAmount := Materials[i].DiffuseMapAmount;
    Result.Materials[i].Channels[0].SpecularMapEnable := Materials[i].SpecularMapEnable;
    Result.Materials[i].Channels[0].SpecularMap := Materials[i].SpecularMap;
    Result.Materials[i].Channels[0].SpecularMapAmount := Materials[i].SpecularMapAmount;
    Result.Materials[i].Channels[0].OpacityMapEnable := Materials[i].OpacityMapEnable;
    Result.Materials[i].Channels[0].OpacityMap := Materials[i].OpacityMap;
    Result.Materials[i].Channels[0].OpacityMapAmount := Materials[i].OpacityMapAmount;
    Result.Materials[i].Channels[0].LightMapEnable := Materials[i].LightMapEnable;
    Result.Materials[i].Channels[0].LightMap := Materials[i].LightMap;
    Result.Materials[i].Channels[0].LightMapAmount := Materials[i].LightMapAmount;
    Result.Materials[i].Channels[0].NormalMapEnable := Materials[i].NormalMapEnable;
    Result.Materials[i].Channels[0].NormalMap := Materials[i].NormalMap;
    Result.Materials[i].Channels[0].NormalMapAmount := Materials[i].NormalMapAmount;
  end;
  Result.RagDollCount := RagdollCount;
  SetLength(Result.RagDolls, Result.RagDollCount);
  for i := 0 to Result.RagDollCount - 1 do
  begin
    Result.RagDolls[i].NodeID := Ragdolls[i].NodeID;
    Result.RagDolls[i].BodyNodeCount := Ragdolls[i].BodyNodeCount;
    Result.RagDolls[i].ArmRNodeCount := Ragdolls[i].ArmRNodeCount;
    Result.RagDolls[i].ArmLNodeCount := Ragdolls[i].ArmLNodeCount;
    Result.RagDolls[i].LegRNodeCount := Ragdolls[i].LegRNodeCount;
    Result.RagDolls[i].LegLNodeCount := Ragdolls[i].LegLNodeCount;
    SetLength(Result.Ragdolls[i].BodyNodes, Result.Ragdolls[i].BodyNodeCount);
    SetLength(Result.Ragdolls[i].ArmRNodes, Result.Ragdolls[i].ArmRNodeCount);
    SetLength(Result.Ragdolls[i].ArmLNodes, Result.Ragdolls[i].ArmLNodeCount);
    SetLength(Result.Ragdolls[i].LegRNodes, Result.Ragdolls[i].LegRNodeCount);
    SetLength(Result.Ragdolls[i].LegLNodes, Result.Ragdolls[i].LegLNodeCount);
    G2MeshRagdollObjectCopy(@RagDolls[i].Head, @Result.Ragdolls[i].Head);
    G2MeshRagdollObjectCopy(@RagDolls[i].Neck, @Result.Ragdolls[i].Neck);
    G2MeshRagdollObjectCopy(@RagDolls[i].Pelvis, @Result.Ragdolls[i].Pelvis);
    for j := 0 to Result.Ragdolls[i].BodyNodeCount - 1 do
    G2MeshRagdollObjectCopy(@RagDolls[i].BodyNodes[j], @Result.Ragdolls[i].BodyNodes[j]);
    for j := 0 to Result.Ragdolls[i].ArmRNodeCount - 1 do
    G2MeshRagdollObjectCopy(@RagDolls[i].ArmRNodes[j], @Result.Ragdolls[i].ArmRNodes[j]);
    for j := 0 to Result.Ragdolls[i].ArmLNodeCount - 1 do
    G2MeshRagdollObjectCopy(@RagDolls[i].ArmLNodes[j], @Result.Ragdolls[i].ArmLNodes[j]);
    for j := 0 to Result.Ragdolls[i].LegRNodeCount - 1 do
    G2MeshRagdollObjectCopy(@RagDolls[i].LegRNodes[j], @Result.Ragdolls[i].LegRNodes[j]);
    for j := 0 to Result.Ragdolls[i].LegLNodeCount - 1 do
    G2MeshRagdollObjectCopy(@RagDolls[i].LegLNodes[j], @Result.Ragdolls[i].LegLNodes[j]);
  end;
end;

function TG2Mesh.NodeIndex(const NodeName: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to NodeCount - 1 do
  if Nodes[i].Name = NodeName then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TG2Mesh.GeomIndex(const NodeName: AnsiString): Integer;
  var i, ni: Integer;
begin
  ni := NodeIndex(NodeName);
  for i := 0 to GeomCount - 1 do
  if Geoms[i].NodeID = ni then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TG2Mesh.AnimIndex(const AnimName: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to AnimCount - 1 do
  if Anims[i].Name = AnimName then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TG2Mesh.RagdollIndex(const RagdollName: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to RagdollCount - 1 do
  if Nodes[Ragdolls[i].NodeID].Name = RagdollName then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TG2Mesh.InstanceCreate: TG2MeshInst;
begin
  Result := TG2MeshInst.Create;
  Result.Mesh := Self;
end;

function TG2Mesh.BoneGroupCreate: TG2MeshBoneGroup;
begin
  Result.Mesh := Self;
end;

function TG2Mesh.AnimBlendCreate: TG2MeshAnimBlend;
begin
  Result.Mesh := Self;
  SetLength(Result.NodeTransforms, NodeCount);
end;

function TG2Mesh.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Effect := Core.Graphics.ShaderLib.RequestEffect('fx_Meshes');
end;

function TG2Mesh.Finalize: TG2Result;
begin
  Result := inherited Finalize;
end;
//TG2Mesh END

//TG2MeshInst BEGIN
constructor TG2MeshInst.Create;
begin
  inherited Create;
  m_AutoFrustumCull := True;
  m_AutoComputeTransforms := True;
end;

destructor TG2MeshInst.Destroy;
begin
  inherited Destroy;
end;

procedure TG2MeshInst.SetMesh(const Value: TG2Mesh);
var
  i, r: Integer;
begin
  if Value <> m_Mesh then
  begin
    m_Mesh := Value;
    m_Effect := m_Mesh.Effect;
    SetLength(Materials, m_Mesh.MaterialCount);
    for i := 0 to m_Mesh.MaterialCount - 1 do
    begin
      Materials[i].Name := m_Mesh.Materials[i].Name;
      Materials[i].TwoSided := m_Mesh.Materials[i].TwoSided;
    end;
    SetLength(GeomProperties, m_Mesh.GeomCount);
    for i := 0 to m_Mesh.GeomCount - 1 do
    begin
      GeomProperties[i].Visible := m_Mesh.Geoms[i].Visible;
    end;
    r := 0;
    SetLength(m_RootNodes, m_Mesh.NodeCount);
    SetLength(NodeTransforms, m_Mesh.NodeCount);
    SetLength(m_TempMat, m_Mesh.NodeCount);
    for i := 0 to m_Mesh.NodeCount - 1 do
    begin
      if m_Mesh.Nodes[i].OwnerID = -1 then
      begin
        m_RootNodes[r] := i;
        Inc(r);
      end;
      NodeTransforms[i].TransformDef := m_Mesh.Nodes[i].Transform;
      NodeTransforms[i].TransformCur := NodeTransforms[i].TransformDef;
      NodeTransforms[i].TransformRen.SetIdentity;
    end;
    SetLength(m_RootNodes, r);
    SetLength(m_SkinTransforms, m_Mesh.GeomCount);
    for i := 0 to m_Mesh.GeomCount - 1 do
    if m_Mesh.Geoms[i].Skinned then
    SetLength(m_SkinTransforms[i], m_Mesh.Geoms[i].BoneCount);
    ComputeTransforms;
  end;
end;

function TG2MeshInst.GetSkinTransforms(const Index: Integer): PG2Mat;
begin
  Result := @m_SkinTransforms[Index][0];
end;

function TG2MeshInst.GetBox: TG2Box;
  var i, bi, b: Integer;
  var AABox: TG2AABox;
  var MinV, MaxV: TG2Vec3;
begin
  for i := 0 to m_Mesh.GeomCount - 1 do
  begin
    if m_Mesh.Geoms[i].Skinned and (m_Mesh.Geoms[i].BoneCount > 0) then
    begin
      for bi := 0 to m_Mesh.Geoms[i].BoneCount - 1 do
      if m_Mesh.Geoms[i].Bones[bi].VCount > 0 then
      begin
        AABox := (m_Mesh.Geoms[i].Bones[bi].BBox * m_SkinTransforms[i, 0]).AABox;
        Break;
      end;
      for b := bi to m_Mesh.Geoms[i].BoneCount - 1 do
      if m_Mesh.Geoms[i].Bones[b].VCount > 0 then
      AABox := AABox + (m_Mesh.Geoms[i].Bones[b].BBox * m_SkinTransforms[i, b]).AABox;
    end
    else
    AABox := (m_Mesh.Geoms[i].BBox * NodeTransforms[m_Mesh.Geoms[i].NodeID].TransformRen).AABox;
    if i = 0 then
    begin
      MinV := AABox.MinV;
      MaxV := AABox.MaxV;
    end
    else
    begin
      if AABox.MinV.x < MinV.x then MinV.x := AABox.MinV.x;
      if AABox.MinV.y < MinV.y then MinV.y := AABox.MinV.y;
      if AABox.MinV.z < MinV.z then MinV.z := AABox.MinV.z;
      if AABox.MaxV.x > MaxV.x then MaxV.x := AABox.MaxV.x;
      if AABox.MaxV.y > MaxV.y then MaxV.y := AABox.MaxV.y;
      if AABox.MaxV.z > MaxV.z then MaxV.z := AABox.MaxV.z;
    end;
  end;
  Result.C := (MinV + MaxV) * 0.5;
  Result.vx.SetValue((MaxV.x - MinV.x) * 0.5, 0, 0);
  Result.vy.SetValue(0, (MaxV.y - MinV.y) * 0.5, 0);
  Result.vz.SetValue(0, 0, (MaxV.z - MinV.z) * 0.5);
end;

function TG2MeshInst.GetGeomBBox(const Index: Integer): TG2Box;
  var AABox: TG2AABox;
  var i, b: Integer;
begin
  if m_Mesh.Geoms[Index].Skinned and (m_Mesh.Geoms[Index].BoneCount > 0) then
  begin
    for i := 0 to m_Mesh.Geoms[Index].BoneCount - 1 do
    if m_Mesh.Geoms[Index].Bones[i].VCount > 0 then
    begin
      AABox := (m_Mesh.Geoms[Index].Bones[0].BBox * m_SkinTransforms[Index, 0]).AABox;
      Break;
    end;
    for b := i + 1 to m_Mesh.Geoms[Index].BoneCount - 1 do
    if m_Mesh.Geoms[Index].Bones[b].VCount > 0 then
    AABox := AABox + (m_Mesh.Geoms[Index].Bones[b].BBox * m_SkinTransforms[Index, b]).AABox;
    Result.C := (AABox.MinV + AABox.MaxV) * 0.5;
    Result.vx.SetValue((AABox.MaxV.x - AABox.MinV.x) * 0.5, 0, 0);
    Result.vy.SetValue(0, (AABox.MaxV.y - AABox.MinV.y) * 0.5, 0);
    Result.vz.SetValue(0, 0, (AABox.MaxV.z - AABox.MinV.z) * 0.5);
  end
  else
  Result := m_Mesh.Geoms[Index].BBox * NodeTransforms[m_Mesh.Geoms[Index].NodeID].TransformRen;
end;

procedure TG2MeshInst.RenderFF;
  var i, j: Integer;
  var BBox: TG2Box;
  var PrevIndexVertexVertexBlendEnable: Boolean;
  var PrevVertexBlend: DWord;
  var PrevCullMode: DWord;
  var W: TG2Mat;
begin
  PrevIndexVertexVertexBlendEnable := m_Mesh.Core.Graphics.RenderStates.IndexedVertexBlendEnable;
  PrevVertexBlend := m_Mesh.Core.Graphics.RenderStates.VertexBlend;
  PrevCullMode := m_Mesh.Core.Graphics.RenderStates.CullMode;
  W := m_Mesh.Core.Graphics.Transforms.W[0];
  m_Mesh.Core.Graphics.Transforms.PushW;
  for i := 0 to m_Mesh.GeomCount - 1 do
  if GeomProperties[i].Visible then
  begin
    if m_AutoFrustumCull then
    begin
      BBox := GetGeomBBox(i) * W;
      if not m_Mesh.Core.Graphics.Transforms.Frustum.BoxInFrustum(BBox.AABox) then Continue;
    end;
    if m_Mesh.Geoms[i].Skinned then
    begin
      m_Mesh.Core.Graphics.RenderStates.IndexedVertexBlendEnable := True;
      m_Mesh.Core.Graphics.RenderStates.VertexBlend := 4;
      for j := 0 to m_Mesh.Geoms[i].BoneCount - 1 do
      begin
        m_Mesh.Core.Graphics.Transforms.W[j] := m_SkinTransforms[i][j] * W;
        m_Mesh.Core.Graphics.Transforms.ApplyW(j);
      end;
      if m_Mesh.Core.Graphics.Params.VertexProcessing = vpMixed then
      m_Mesh.Core.Graphics.Device.SetSoftwareVertexProcessing(True);
    end
    else
    begin
      m_Mesh.Core.Graphics.Transforms.W[0] := NodeTransforms[m_Mesh.Geoms[i].NodeID].TransformRen * W;
      m_Mesh.Core.Graphics.Transforms.ApplyW(0);
    end;
    if m_Mesh.Geoms[i].MaterialCount > 0 then
    for j := 0 to m_Mesh.Geoms[i].MaterialCount - 1 do
    begin
      if Materials[m_Mesh.Geoms[i].Materials[j]].TwoSided then
      m_Mesh.Core.Graphics.RenderStates.CullMode := D3DCULL_NONE
      else
      m_Mesh.Core.Graphics.RenderStates.CullMode := D3DCULL_CCW;
      if Assigned(Materials[m_Mesh.Geoms[i].Materials[j]].MapDiffuse) then
      m_Mesh.Core.Graphics.Device.SetTexture(0, Materials[m_Mesh.Geoms[i].Materials[j]].MapDiffuse.Texture)
      else
      m_Mesh.Core.Graphics.Device.SetTexture(0, nil);
      m_Mesh.Geoms[i].Mesh.DrawSubset(j);
    end
    else
    begin
      m_Mesh.Core.Graphics.RenderStates.CullMode := D3DCULL_CCW;
      m_Mesh.Core.Graphics.Device.SetTexture(0, nil);
      m_Mesh.Geoms[i].Mesh.DrawSubset(0);
    end;
    if m_Mesh.Geoms[i].Skinned then
    begin
      if m_Mesh.Core.Graphics.Params.VertexProcessing = vpMixed then
      m_Mesh.Core.Graphics.Device.SetSoftwareVertexProcessing(False);
      m_Mesh.Core.Graphics.RenderStates.IndexedVertexBlendEnable := False;
      m_Mesh.Core.Graphics.RenderStates.VertexBlend := 0;
    end;
  end;
  m_Mesh.Core.Graphics.RenderStates.CullMode := PrevCullMode;
  m_Mesh.Core.Graphics.RenderStates.VertexBlend := PrevVertexBlend;
  m_Mesh.Core.Graphics.RenderStates.IndexedVertexBlendEnable := PrevIndexVertexVertexBlendEnable;
  m_Mesh.Core.Graphics.Transforms.PopW;
  m_Mesh.Core.Graphics.Transforms.ApplyW(0);
end;

procedure TG2MeshInst.RenderSM3;
  var i, j: Integer;
  var WVo, P, WV, WVP: TG2Mat;
  var v3: TG2Vec3;
  var LightCountPoint: Integer;
  var LightPosPoint: array[0..15] of TD3DXVector4;
  var LightColorPoint: array[0..15] of TD3DXColor;
  var LightRangePoint: array[0..15] of Single;
  var LightCountDirectional: Integer;
  var LightDirDirectional: array[0..15] of TD3DXVector4;
  var LightColorDirectional: array[0..15] of TD3DXColor;
  var LightAmbient: TD3DXColor;
  var CurTechnique: TD3DXHandle;
  var PassOpen: Boolean;
  var BBox: TG2Box;
  var PrevCullMode: DWord;
begin
  PrevCullMode := m_Mesh.Core.Graphics.RenderStates.CullMode;
  WVo := m_Mesh.Core.Graphics.Transforms.WV;
  P := m_Mesh.Core.Graphics.Transforms.P;
  LightCountPoint := 0;
  LightCountDirectional := 0;
  LightAmbient := D3DXColorFromDWord(m_Mesh.Core.Graphics.RenderStates.Ambient);
  for i := 0 to m_Mesh.Core.Graphics.Lights.LightCount - 1 do
  if m_Mesh.Core.Graphics.Lights[i].Enabled then
  begin
    case m_Mesh.Core.Graphics.Lights[i].LightType of
      ltAmbient:
      begin
        D3DXColorAdd(LightAmbient, LightAmbient, m_Mesh.Core.Graphics.Lights[i].Light.Ambient);
      end;
      ltPoint:
      begin
        v3 := m_Mesh.Core.Graphics.Lights[i].Light.Position;
        LightPosPoint[LightCountPoint] := D3DXVector4(
          v3 * m_Mesh.Core.Graphics.Transforms.V, 1
        );
        LightColorPoint[LightCountPoint] := m_Mesh.Core.Graphics.Lights[i].Light.Diffuse;
        LightRangePoint[LightCountPoint] := Sqr(m_Mesh.Core.Graphics.Lights[i].Light.Range);
        Inc(LightCountPoint);
      end;
      ltDirectional:
      begin
        v3 := m_Mesh.Core.Graphics.Lights[i].Light.Direction;
        D3DXVec3TransformNormal(PG2Vec3Ref(@v3)^, v3, m_Mesh.Core.Graphics.Transforms.V);
        v3 := -v3;
        v3.Normalize;
        LightDirDirectional[LightCountDirectional] := D3DXVector4(v3, 1);
        LightColorDirectional[LightCountDirectional] := m_Mesh.Core.Graphics.Lights[i].Light.Diffuse;
        Inc(LightCountDirectional);
      end;
    end;
  end;
  m_Effect.SetVector('LightAmbient', PD3DXVector4(@LightAmbient)^);
  m_Effect.SetInt('LightCountPoint', LightCountPoint);
  m_Effect.SetVectorArray('LightPosPoint', @LightPosPoint[0], LightCountPoint);
  m_Effect.SetVectorArray('LightColorPoint', @LightColorPoint[0], LightCountPoint);
  m_Effect.SetFloatArray('LightRangePoint', @LightRangePoint[0], LightCountPoint);
  m_Effect.SetInt('LightCountDirectional', LightCountDirectional);
  m_Effect.SetVectorArray('LightDirDirectional', @LightDirDirectional[0], LightCountDirectional);
  m_Effect.SetVectorArray('LightColorDirectional', @LightColorDirectional[0], LightCountDirectional);
  m_Effect.SetInt('LightShaderIndex', LightCountPoint * 9 + LightCountDirectional);
  CurTechnique := nil;
  PassOpen := False;
  for i := 0 to m_Mesh.GeomCount - 1 do
  if GeomProperties[i].Visible then
  begin
    if m_AutoFrustumCull then
    begin
      BBox := GetGeomBBox(i) * m_Mesh.Core.Graphics.Transforms.W[0];
      if not m_Mesh.Core.Graphics.Transforms.Frustum.BoxInFrustum(BBox.AABox) then Continue;
    end;
    if m_Mesh.Geoms[i].Technique <> CurTechnique then
    begin
      CurTechnique := m_Mesh.Geoms[i].Technique;
      if PassOpen then
      begin
        m_Effect.EndPass;
        m_Effect.EndEffect;
      end;
      m_Effect.Technique := CurTechnique;
      m_Effect.BeginEffect(nil, 0); //D3DXFX_DONOTSAVESTATE
      m_Effect.BeginPass(0);
      PassOpen := True;
    end;
    if m_Mesh.Geoms[i].Skinned then
    begin
      m_Effect.SetInt('MaxBoneWeights', m_Mesh.Geoms[i].MaxWeights - 1);
      m_Effect.SetMatrixArray('SkinPallete', @m_SkinTransforms[i][0], m_Mesh.Geoms[i].BoneCount);
      WV := WVo;
    end
    else
    WV := NodeTransforms[m_Mesh.Geoms[i].NodeID].TransformRen * WVo;
    WVP := WV * P;
    m_Effect.SetMatrix('WV', WV);
    m_Effect.SetMatrix('WVP', WVP);
    if m_Mesh.Geoms[i].MaterialCount > 0 then
    for j := 0 to m_Mesh.Geoms[i].MaterialCount - 1 do
    begin
      if Assigned(Materials[m_Mesh.Geoms[i].Materials[j]].MapDiffuse) then
      m_Effect.SetTexture('TexDiffuse', Materials[m_Mesh.Geoms[i].Materials[j]].MapDiffuse.Texture)
      else
      m_Effect.SetTexture('TexDiffuse', nil);
      if Assigned(Materials[m_Mesh.Geoms[i].Materials[j]].MapSpecular) then
      m_Effect.SetTexture('TexSpecular', Materials[m_Mesh.Geoms[i].Materials[j]].MapSpecular.Texture)
      else
      m_Effect.SetTexture('TexSpecular', nil);
      if Assigned(Materials[m_Mesh.Geoms[i].Materials[j]].MapNormals) then
      m_Effect.SetTexture('TexNormals', Materials[m_Mesh.Geoms[i].Materials[j]].MapNormals.Texture)
      else
      m_Effect.SetTexture('TexNormals', nil);
      m_Effect.CommitChanges;
      if Materials[m_Mesh.Geoms[i].Materials[j]].TwoSided then
      m_Mesh.Core.Graphics.RenderStates.CullMode := D3DCULL_NONE
      else
      m_Mesh.Core.Graphics.RenderStates.CullMode := D3DCULL_CCW;
      m_Mesh.Geoms[i].Mesh.DrawSubset(j);
    end
    else
    begin
      m_Mesh.Core.Graphics.RenderStates.CullMode := D3DCULL_CCW;
      m_Effect.SetTexture('TexDiffuse', nil);
      m_Effect.SetTexture('TexSpecular', nil);
      m_Effect.SetTexture('TexNormals', nil);
      m_Effect.CommitChanges;
      m_Mesh.Geoms[i].Mesh.DrawSubset(0);
    end;
  end;
  if PassOpen then
  begin
    m_Effect.EndPass;
    m_Effect.EndEffect;
  end;
  m_Mesh.Core.Graphics.RenderStates.CullMode := PrevCullMode;
end;

procedure TG2MeshInst.FrameSetFast(const AnimName: AnsiString; const Frame: Integer);
var
  AnimIndex, i: Integer;
  f0: Integer;
begin
  AnimIndex := m_Mesh.AnimIndex(AnimName);
  if AnimIndex > -1 then
  begin
    if Frame < 0 then
    f0 := m_Mesh.Anims[AnimIndex].FrameCount - (Abs(Frame) mod m_Mesh.Anims[AnimIndex].FrameCount)
    else
    f0 := Frame mod m_Mesh.Anims[AnimIndex].FrameCount;
    for i := 0 to m_Mesh.Anims[AnimIndex].NodeCount - 1 do
    with NodeTransforms[m_Mesh.Anims[AnimIndex].Nodes[i].NodeID].TransformCur do
    begin
      SetScaling(m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f0].Scale);
      Rotate(m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f0].Rotation);
      Translate(m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f0].Translation);
    end;
    if m_AutoComputeTransforms then ComputeTransforms;
  end;
end;

procedure TG2MeshInst.FrameSet(const AnimName: AnsiString; const Frame: Single);
var
  AnimIndex, i: Integer;
  f0, f1: Integer;
  f: Single;
  s0: TG2Vec3;
  r0: TG2Quat;
  t0: TG2Vec3;
begin
  AnimIndex := m_Mesh.AnimIndex(AnimName);
  if AnimIndex > -1 then
  begin
    if Frame < 0 then
    f := m_Mesh.Anims[AnimIndex].FrameCount - (Trunc(Abs(Frame)) mod m_Mesh.Anims[AnimIndex].FrameCount) + Frac(Frame)
    else
    f := Frame;
    f0 := Trunc(f) mod m_Mesh.Anims[AnimIndex].FrameCount;
    f1 := (f0 + 1) mod m_Mesh.Anims[AnimIndex].FrameCount;
    f := Frac(f);
    for i := 0 to m_Mesh.Anims[AnimIndex].NodeCount - 1 do
    with NodeTransforms[m_Mesh.Anims[AnimIndex].Nodes[i].NodeID].TransformCur do
    begin
      s0 := G2LerpVec3(
        m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f0].Scale,
        m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f1].Scale,
        f
      );
      D3DXQuaternionSlerp(
        PG2QuatRef(@r0)^,
        m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f0].Rotation,
        m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f1].Rotation,
        f
      );
      t0 := G2LerpVec3(
        m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f0].Translation,
        m_Mesh.Anims[AnimIndex].Nodes[i].Frames[f1].Translation,
        f
      );
      SetScaling(s0);
      Rotate(r0);
      Translate(t0);
    end;
    if m_AutoComputeTransforms then ComputeTransforms;
  end;
end;

procedure TG2MeshInst.FrameSet(const AnimName: AnsiString; const Frame: Single; const BoneGroup: TG2MeshBoneGroup);
var
  AnimIndex, i, j, n: Integer;
  f0, f1: Integer;
  f: Single;
  s0: TG2Vec3;
  r0: TG2Quat;
  t0: TG2Vec3;
begin
  AnimIndex := m_Mesh.AnimIndex(AnimName);
  if AnimIndex > -1 then
  begin
    if Frame < 0 then
    f := m_Mesh.Anims[AnimIndex].FrameCount - (Trunc(Abs(Frame)) mod m_Mesh.Anims[AnimIndex].FrameCount) + Frac(Frame)
    else
    f := Frame;
    f0 := Trunc(f) mod m_Mesh.Anims[AnimIndex].FrameCount;
    f1 := (f0 + 1) mod m_Mesh.Anims[AnimIndex].FrameCount;
    f := Frac(f);
    for i := 0 to High(BoneGroup.Nodes) do
    begin
      n := -1;
      for j := 0 to m_Mesh.Anims[AnimIndex].NodeCount - 1 do
      if m_Mesh.Anims[AnimIndex].Nodes[j].NodeID = BoneGroup.Nodes[i] then
      begin
        n := j;
        Break;
      end;
      if n > -1 then
      with NodeTransforms[BoneGroup.Nodes[i]].TransformCur do
      begin
        s0 := G2LerpVec3(
          m_Mesh.Anims[AnimIndex].Nodes[n].Frames[f0].Scale,
          m_Mesh.Anims[AnimIndex].Nodes[n].Frames[f1].Scale,
          f
        );
        D3DXQuaternionSlerp(
          PG2QuatRef(@r0)^,
          m_Mesh.Anims[AnimIndex].Nodes[n].Frames[f0].Rotation,
          m_Mesh.Anims[AnimIndex].Nodes[n].Frames[f1].Rotation,
          f
        );
        t0 := G2LerpVec3(
          m_Mesh.Anims[AnimIndex].Nodes[n].Frames[f0].Translation,
          m_Mesh.Anims[AnimIndex].Nodes[n].Frames[f1].Translation,
          f
        );
        SetScaling(s0);
        Rotate(r0);
        Translate(t0);
      end;
    end;
    if m_AutoComputeTransforms then ComputeTransforms(BoneGroup);
  end;
end;

procedure TG2MeshInst.FrameBlend(const AnimBlend: TG2MeshAnimBlend);
  var i, b: Integer;
  var f0, f1: Integer;
  var f: Single;
  var s0: TG2Vec3;
  var r0: TG2Quat;
  var t0: TG2Vec3;
  var RcpW: Single;
begin
  for i := 0 to Mesh.NodeCount - 1 do
  begin
    AnimBlend.NodeTransforms[i].Rotation.SetValue(0, 0, 0, 0);
    AnimBlend.NodeTransforms[i].Scaling.SetValue(0, 0, 0);
    AnimBlend.NodeTransforms[i].Translation.SetValue(0, 0, 0);
    AnimBlend.NodeTransforms[i].TotalWeight := 0;
  end;
  for i := 0 to High(AnimBlend.Anims) do
  for b := 0 to High(AnimBlend.Anims[i].Bones) do
  if AnimBlend.Anims[i].Bones[b] then
  begin
    if AnimBlend.Anims[i].Frame < 0 then
    f := (
      m_Mesh.Anims[AnimBlend.Anims[i].AnimID].FrameCount - (
        Trunc(Abs(AnimBlend.Anims[i].Frame)) mod m_Mesh.Anims[AnimBlend.Anims[i].AnimID].FrameCount
      ) + Frac(AnimBlend.Anims[i].Frame)
    )
    else
    f := AnimBlend.Anims[i].Frame;
    f0 := Trunc(f) mod m_Mesh.Anims[AnimBlend.Anims[i].AnimID].FrameCount;
    f1 := (f0 + 1) mod m_Mesh.Anims[AnimBlend.Anims[i].AnimID].FrameCount;
    f := Frac(f);
    s0 := G2LerpVec3(
      Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].Frames[f0].Scale,
      Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].Frames[f1].Scale,
      f
    ) * AnimBlend.Anims[i].Weight;
    t0 := G2LerpVec3(
      Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].Frames[f0].Translation,
      Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].Frames[f1].Translation,
      f
    ) * AnimBlend.Anims[i].Weight;
    D3DXQuaternionSlerp(
      PG2QuatRef(@r0)^,
      Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].Frames[f0].Rotation,
      Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].Frames[f1].Rotation,
      f
    );
    r0 := r0 * AnimBlend.Anims[i].Weight;
    AnimBlend.NodeTransforms[Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].NodeID].Scaling := (
      AnimBlend.NodeTransforms[Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].NodeID].Scaling + s0
    );
    AnimBlend.NodeTransforms[Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].NodeID].Translation := (
      AnimBlend.NodeTransforms[Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].NodeID].Translation + t0
    );
    AnimBlend.NodeTransforms[Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].NodeID].Rotation := (
      AnimBlend.NodeTransforms[Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].NodeID].Rotation + r0
    );
    AnimBlend.NodeTransforms[Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].NodeID].TotalWeight := (
      AnimBlend.NodeTransforms[Mesh.Anims[AnimBlend.Anims[i].AnimID].Nodes[b].NodeID].TotalWeight +
      AnimBlend.Anims[i].Weight
    );
  end;
  for i := 0 to Mesh.NodeCount - 1 do
  if AnimBlend.NodeTransforms[i].TotalWeight > 1E-5 then
  begin
    RcpW := 1 / AnimBlend.NodeTransforms[i].TotalWeight;
    s0 := AnimBlend.NodeTransforms[i].Scaling * RcpW;
    t0 := AnimBlend.NodeTransforms[i].Translation * RcpW;
    r0 := AnimBlend.NodeTransforms[i].Rotation * RcpW;
    NodeTransforms[i].TransformCur.SetScaling(s0);
    NodeTransforms[i].TransformCur.Rotate(r0);
    NodeTransforms[i].TransformCur.Translate(t0);
  end;
  if m_AutoComputeTransforms then ComputeTransforms;
end;

procedure TG2MeshInst.ComputeTransforms;
  procedure ComputeNode(const NodeID: Integer);
  var
    i: Integer;
  begin
    if m_Mesh.Nodes[NodeID].OwnerID > -1 then
    NodeTransforms[NodeID].TransformRen := NodeTransforms[NodeID].TransformCur * NodeTransforms[m_Mesh.Nodes[NodeID].OwnerID].TransformRen
    else
    NodeTransforms[NodeID].TransformRen := NodeTransforms[NodeID].TransformCur;
    for i := 0 to High(m_Mesh.Nodes[NodeID].SlaveID) do
    ComputeNode(m_Mesh.Nodes[NodeID].SlaveID[i]);
  end;
var
  i: Integer;
begin
  for i := 0 to High(m_RootNodes) do
  ComputeNode(m_RootNodes[i]);
  ComputeSkinTransforms;
end;

procedure TG2MeshInst.ComputeTransforms(const BoneGroup: TG2MeshBoneGroup);
  procedure ComputeNode(const NodeID: Integer);
  var
    i: Integer;
  begin
    if m_Mesh.Nodes[NodeID].OwnerID > -1 then
    m_TempMat[NodeID] := NodeTransforms[NodeID].TransformCur * m_TempMat[m_Mesh.Nodes[NodeID].OwnerID]
    else
    m_TempMat[NodeID] := NodeTransforms[NodeID].TransformCur;
    for i := 0 to High(BoneGroup.Nodes) do
    if BoneGroup.Nodes[i] = NodeID then
    begin
      NodeTransforms[NodeID].TransformRen := m_TempMat[NodeID];
      Break;
    end;
    for i := 0 to High(m_Mesh.Nodes[NodeID].SlaveID) do
    ComputeNode(m_Mesh.Nodes[NodeID].SlaveID[i]);
  end;
  var i: Integer;
begin
  for i := 0 to High(m_RootNodes) do
  ComputeNode(m_RootNodes[i]);
  ComputeSkinTransforms;
end;

procedure TG2MeshInst.ComputeSkinTransforms;
  var i, j: Integer;
begin
  for i := 0 to m_Mesh.GeomCount - 1 do
  if m_Mesh.Geoms[i].Skinned then
  for j := 0 to m_Mesh.Geoms[i].BoneCount - 1 do
  begin
    m_SkinTransforms[i][j] := (
      m_Mesh.Geoms[i].Bones[j].Bind * NodeTransforms[m_Mesh.Geoms[i].Bones[j].NodeID].TransformRen
    );
  end;
end;

procedure TG2MeshInst.Render;
begin
  case m_Mesh.RenderMode of
    rmFF: RenderFF;
    rmSM3: RenderSM3;
  end;
end;

function TG2MeshInst.Pick(
  const Ray: TG2Ray;
  const OutD: PSingle = nil;
  const OutGeomID: PInteger = nil;
  const OutFaceID: PWord = nil;
  const OutU: PSingle = nil;
  const OutV: PSingle = nil
): Boolean;
  var i: Integer;
  var CurD, CurU, CurV: Single;
  var FinD, FinU, FinV: Single;
  var CurFaceID: Word;
  var FinFaceID: Word;
  var FinGeomID: Integer;
  var FirstHit: Boolean;
begin
  Result := False;
  FinU := 0; FinV := 0; FinD := 0; FinFaceID := 0; FinGeomID := 0;
  FirstHit := True;
  for i := 0 to m_Mesh.GeomCount - 1 do
  if PickGeom(i, Ray, @CurD, @CurFaceID, @CurU, @CurV) then
  begin
    if FirstHit or (CurD < FinD) then
    begin
      FirstHit := False;
      FinU := CurU;
      FinV := CurV;
      FinD := CurD;
      FinGeomID := i;
      FinFaceID := CurFaceID;
      Result := True;
    end;
  end;
  if Result then
  begin
    if Assigned(OutD) then OutD^ := FinD;
    if Assigned(OutGeomID) then OutGeomID^ := FinGeomID;
    if Assigned(OutFaceID) then OutFaceID^ := FinFaceID;
    if Assigned(OutU) then OutU^ := FinU;
    if Assigned(OutV) then OutV^ := FinV;
  end;
end;

function TG2MeshInst.PickGeom(
      const GeomID: Integer;
      const Ray: TG2Ray;
      const OutD: PSingle = nil;
      const OutFaceID: PWord = nil;
      const OutU: PSingle = nil;
      const OutV: PSingle = nil
    ): Boolean;
  var VCount, i: Integer;
  var t: Word;
  var W, m: TG2Mat;
  var Ptr: Pointer;
  var Ind: PG2Index16Array;
  var CurD, CurU, CurV: Single;
  var FinD, FinU, FinV: Single;
  var FinFaceID: Word;
  var FirstHit: Boolean;
  var v: PG2Vec3;
  var tmpv: TG2Vec3;
  var bi: Integer;
  var bw: Single;
begin
  Result := False;
  FinU := 0; FinV := 0; FinD := 0; FinFaceID := 0;
  if Ray.IntersectAABox((GetGeomBBox(GeomID) * m_Mesh.Core.Graphics.Transforms.W[0]).AABox) then
  begin
    VCount := m_Mesh.Geoms[GeomID].Mesh.GetNumVertices;
    if Length(m_TempVec) < VCount then
    SetLength(m_TempVec, VCount);
    m_Mesh.Geoms[GeomID].Mesh.LockVertexBuffer(D3DLOCK_READONLY or D3DLOCK_NOSYSLOCK, Ptr);
    if m_Mesh.Geoms[GeomID].Skinned then
    begin
      W := m_Mesh.Core.Graphics.Transforms.W[0];
      if m_Mesh.Geoms[GeomID].MaxWeights = 1 then
      for i := 0 to VCount - 1 do
      begin
        v := Ptr;
        bi := Round(PSingle(DWord(Ptr) + 56)^);
        Ptr := Pointer(DWord(Ptr) + m_Mesh.Geoms[GeomID].VertexStride);
        m := m_SkinTransforms[GeomID, bi] * W;
        D3DXVec3TransformCoord(PD3DXVector3(@m_TempVec[i])^, v^, m);
      end
      else
      for i := 0 to VCount - 1 do
      begin
        v := Ptr;
        m_TempVec[i].SetValue(0, 0, 0);
        for t := 0 to m_Mesh.Geoms[GeomID].MaxWeights - 1 do
        begin
          bi := Round(PSingle(DWord(Ptr) + 56 + t * 4)^);
          bw := PSingle(DWord(Ptr) + 56 + m_Mesh.Geoms[GeomID].MaxWeights * 4 + t * 4)^;
          m := m_SkinTransforms[GeomID, bi] * W;
          D3DXVec3TransformCoord(PD3DXVector3(@tmpv)^, v^, m);
          tmpv := tmpv * bw;
          m_TempVec[i] := m_TempVec[i] + tmpv;
        end;
        Ptr := Pointer(DWord(Ptr) + m_Mesh.Geoms[GeomID].VertexStride);
      end;
    end
    else
    begin
      W := NodeTransforms[m_Mesh.Geoms[GeomID].NodeID].TransformRen * m_Mesh.Core.Graphics.Transforms.W[0];
      D3DXVec3TransformCoordArray(
        @m_TempVec[0], 12, Ptr, m_Mesh.Geoms[GeomID].VertexStride, W, VCount
      );
    end;
    m_Mesh.Geoms[GeomID].Mesh.UnlockVertexBuffer;
    m_Mesh.Geoms[GeomID].Mesh.LockIndexBuffer(D3DLOCK_READONLY or D3DLOCK_NOSYSLOCK, Pointer(Ind));
    FirstHit := True;
    for i := 0 to m_Mesh.Geoms[GeomID].Mesh.GetNumFaces - 1 do
    begin
      if D3DXIntersectTri(
        m_TempVec[Ind^[i * 3 + 0]],
        m_TempVec[Ind^[i * 3 + 1]],
        m_TempVec[Ind^[i * 3 + 2]],
        Ray.Origin, Ray.Dir,
        CurU, CurV, CurD
      ) then
      begin
        if FirstHit or (CurD < FinD) then
        begin
          FirstHit := False;
          FinU := CurU;
          FinV := CurV;
          FinD := CurD;
          FinFaceID := i;
          Result := True;
        end;
      end;
    end;
    m_Mesh.Geoms[GeomID].Mesh.UnlockIndexBuffer;
  end;
  if Result then
  begin
    if Assigned(OutD) then OutD^ := FinD;
    if Assigned(OutFaceID) then OutFaceID^ := FinFaceID;
    if Assigned(OutU) then OutU^ := FinU;
    if Assigned(OutV) then OutV^ := FinV;
  end;
end;
//TG2MeshInst END

//TG2Shared BEGIN
constructor TG2Shared.Create;
begin
  inherited Create;
end;

destructor TG2Shared.Destroy;
begin
  inherited Destroy;
end;

function TG2Shared.RequestVB(
      const NewStride: DWord;
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFVF: DWord;
      const NewPool: TD3DPool
    ): TG2VB;
var
  i: Integer;
begin
  for i := 0 to m_VBMgr.Count - 1 do
  if (m_VBMgr.VertexBuffers[i].Stride = NewStride)
  and (m_VBMgr.VertexBuffers[i].Usage = NewUsage)
  and (m_VBMgr.VertexBuffers[i].FVF = NewFVF)
  and (m_VBMgr.VertexBuffers[i].Pool = NewPool) then
  begin
    if m_VBMgr.VertexBuffers[i].Count < NewCount then
    m_VBMgr.VertexBuffers[i].Verify(
      NewStride, NewCount, NewUsage, NewFVF, NewPool
    );
    Result := m_VBMgr.VertexBuffers[i];
    Exit;
  end;
  Result := m_VBMgr.CreateVertexBuffer(
    'SharedVertexBuffer' + IntToStr(m_LastVBID),
    NewStride,
    NewCount,
    NewUsage,
    NewFVF,
    NewPool
  );
  Inc(m_LastVBID);
end;

function TG2Shared.RequestIB(
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFormat: TD3DFormat;
      const NewPool: TD3DPool
    ): TG2IB;
var
  i: Integer;
begin
  for i := 0 to m_IBMgr.Count - 1 do
  if (m_IBMgr.IndexBuffers[i].Usage = NewUsage)
  and (m_IBMgr.IndexBuffers[i].Format = NewFormat)
  and (m_IBMgr.IndexBuffers[i].Pool = NewPool) then
  begin
    if m_IBMgr.IndexBuffers[i].Count < NewCount then
    m_IBMgr.IndexBuffers[i].Verify(
      NewCount, NewUsage, NewFormat, NewPool
    );
    Result := m_IBMgr.IndexBuffers[i];
    Exit;
  end;
  Result := m_IBMgr.CreateIndexBuffer(
    'SharedIndexBuffer' + IntToStr(m_LastIBID),
    NewCount,
    NewUsage,
    NewFormat,
    NewPool
  );
  Inc(m_LastIBID);
end;

function TG2Shared.RequestFont(
      const FontFace: AnsiString;
      const Size: Integer
    ): TG2Font;
var
  i: Integer;
begin
  for i := 0 to m_FontMgr.Count - 1 do
  if (m_FontMgr.Fonts[i].FontFace = FontFace)
  and (m_FontMgr.Fonts[i].Size = Size) then
  begin
    Result := m_FontMgr.Fonts[i];
    Exit;
  end;
  Result := m_FontMgr.CreateFont(
    'SharedFont' + IntToStr(m_LastFontID),
    FontFace, Size
  );
  Inc(m_LastFontID);
end;

function TG2Shared.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestMod(TG2VBMgr, @m_VBMgr);
  Core.RequestMod(TG2IBMgr, @m_IBMgr);
  Core.RequestMod(TG2FontMgr, @m_FontMgr);
  m_LastVBID := 0;
  m_LastIBID := 0;
  m_LastFontID := 0;
  Result := grOk;
end;

function TG2Shared.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleaseMod(@m_FontMgr);
  Core.ReleaseMod(@m_VBMgr);
  Core.ReleaseMod(@m_IBMgr);
  Result := grOk;
end;
//TG2Shared END

//TG2VBMgr BEGIN
constructor TG2VBMgr.Create;
begin
  inherited Create;
end;

destructor TG2VBMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TG2VBMgr.OnDeviceLost;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2VB(m_Resources[i]).OnDeviceLost;
end;

procedure TG2VBMgr.OnDeviceReset;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2VB(m_Resources[i]).OnDeviceReset;
end;

function TG2VBMgr.GetVB(const Index: Integer): TG2VB;
begin
  Result := TG2VB(m_Resources[Index]);
end;

function TG2VBMgr.CreateVertexBuffer(
      const Name: WideString;
      const NewStride: DWord;
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFVF: DWord;
      const NewPool: TD3DPool
    ): TG2VB;
begin
  Result := TG2VB.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if Result.Verify(
    NewStride,
    NewCount,
    NewUsage,
    NewFVF,
    NewPool
  ) then
  AddResource(Result)
  else
  Result.Free;
end;

function TG2VBMgr.FindVB(const Name: WideString): TG2VB;
begin
  Result := TG2VB(FindResource(Name));
end;

function TG2VBMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  Result := grOk;
end;

function TG2VBMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;
//TG2VBMgr END

//TG2VB BEGIN
constructor TG2VB.Create;
begin
  inherited Create;
  m_Stride := 0;
  m_Count := 0;
  m_Usage := D3DUSAGE_WRITEONLY;
  m_FVF := 0;
  m_Pool := D3DPOOL_DEFAULT;
  m_Suspended := False;
end;

destructor TG2VB.Destroy;
begin
  inherited Destroy;
end;

function TG2VB.InitializeVB: Boolean;
begin
  Release;
  Result := Succeeded(
    m_Graphics.Device.CreateVertexBuffer(
      m_Stride * m_Count,
      m_Usage,
      m_FVF,
      m_Pool,
      m_VB,
      nil
    )
  );
end;

procedure TG2VB.OnDeviceLost;
begin
  if m_Pool = D3DPOOL_DEFAULT then
  begin
    Release;
    m_Suspended := True;
  end;
end;

procedure TG2VB.OnDeviceReset;
begin
  if m_Suspended then
  InitializeVB;
end;

function TG2VB.Verify(
      const NewStride: DWord;
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFVF: DWord;
      const NewPool: TD3DPool
    ): Boolean;
begin
  if (m_VB = nil)
  or (m_Stride <> NewStride)
  or (m_Count < NewCount)
  or (m_Usage <> NewUsage)
  or (m_FVF <> NewFVF)
  or (NewPool <> NewPool) then
  begin
    m_Stride := NewStride;
    m_Count := NewCount;
    m_Usage := NewUsage;
    m_FVF := NewFVF;
    m_Pool := NewPool;
    Result := InitializeVB;
  end
  else
  Result := True;
end;

function TG2VB.Lock(const LockOffset, LockSize: DWord; var Data: Pointer; const LockFlags: DWord = 0): HResult;
begin
  Result := m_VB.Lock(LockOffset, LockSize, Data, LockFlags);
end;

function TG2VB.UnLock: HResult;
begin
  Result := m_VB.Unlock;
end;

procedure TG2VB.Release;
begin
  SafeRelease(m_VB);
end;

procedure TG2VB.SetToDevice;
begin
  m_Graphics.Device.SetStreamSource(0, m_VB, 0, m_Stride);
  if m_FVF > 0 then
  m_Graphics.Device.SetFVF(m_FVF);
end;

function TG2VB.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Graphics := Core.Graphics;
  Result := grOk;
end;

function TG2VB.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Release;
  Result := grOk;
end;
//TG2VB END

//TG2IBMgr BEGIN
constructor TG2IBMgr.Create;
begin
  inherited Create;
end;

destructor TG2IBMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TG2IBMgr.OnDeviceLost;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2IB(m_Resources[i]).OnDeviceLost;
end;

procedure TG2IBMgr.OnDeviceReset;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2IB(m_Resources[i]).OnDeviceReset;
end;

function TG2IBMgr.GetIB(const Index: Integer): TG2IB;
begin
  Result := TG2IB(m_Resources[Index]);
end;

function TG2IBMgr.CreateIndexBuffer(
      const Name: WideString;
      const NewSize: DWord;
      const NewUsage: DWord;
      const NewFormat: TD3DFormat;
      const NewPool: TD3DPool
    ): TG2IB;
begin
  Result := TG2IB.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if Result.Verify(
    NewSize,
    NewUsage,
    NewFormat,
    NewPool
  ) then
  AddResource(Result)
  else
  Result.Free;
end;

function TG2IBMgr.FindIB(const Name: WideString): TG2IB;
begin
  Result := TG2IB(FindResource(Name));
end;

function TG2IBMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  Result := grOk;
end;

function TG2IBMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;
//TG2IBMgr END

//TG2IB BEGIN
constructor TG2IB.Create;
begin
  inherited Create;
  m_Count := 0;
  m_Usage := D3DUSAGE_WRITEONLY;
  m_Format := D3DFMT_INDEX16;
  m_Pool := D3DPOOL_DEFAULT;
end;

destructor TG2IB.Destroy;
begin
  inherited Destroy;
end;

function TG2IB.InitializeIB: Boolean;
var
  Stride: DWord;
begin
  case m_Format of
    D3DFMT_INDEX16: Stride := 2;
    D3DFMT_INDEX32: Stride := 4;
    else Stride := 0;
  end;
  Result := Succeeded(
    m_Graphics.Device.CreateIndexBuffer(
      m_Count * Stride,
      m_Usage,
      m_Format,
      m_Pool,
      m_IB,
      nil
    )
  );
end;

procedure TG2IB.OnDeviceLost;
begin
  if m_Pool = D3DPOOL_DEFAULT then
  begin
    Release;
    m_Suspended := True;
  end;
end;

procedure TG2IB.OnDeviceReset;
begin
  if m_Suspended then
  InitializeIB;
end;

function TG2IB.Verify(
      const NewCount: DWord;
      const NewUsage: DWord;
      const NewFormat: TD3DFormat;
      const NewPool: TD3DPool
    ): Boolean;
begin
  if (m_IB = nil)
  or (m_Count < NewCount)
  or (m_Usage <> NewUsage)
  or (m_Format <> NewFormat)
  or (m_Pool <> NewPool) then
  begin
    m_Count := NewCount;
    m_Usage := NewUsage;
    m_Format := NewFormat;
    m_Pool := NewPool;
    Result := InitializeIB;
  end
  else
  Result := True;
end;

function TG2IB.Lock(const LockOffset, LockSize: DWord; var Data: Pointer; const Flags: DWord = 0): HResult;
begin
  Result := m_IB.Lock(LockOffset, LockSize, Data, Flags);
end;

function TG2IB.UnLock: HResult;
begin
  Result := m_IB.Unlock;
end;

procedure TG2IB.Release;
begin
  SafeRelease(m_IB);
end;

procedure TG2IB.SetToDevice;
begin
  m_Graphics.Device.SetIndices(m_IB);
end;

function TG2IB.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Graphics := Core.Graphics;
  Result := grOk;
end;

function TG2IB.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2IB END

//TG2ScriptMgr BEGIN
constructor TG2ScriptMgr.Create;
begin
  inherited Create;
end;

destructor TG2ScriptMgr.Destroy;
begin
  inherited Destroy;
end;

function TG2ScriptMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;

function TG2ScriptMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2ScriptMgr END

//TG2Script BEGIN
constructor TG2Script.Create;
begin
  inherited Create;
end;

destructor TG2Script.Destroy;
begin
  inherited Destroy;
end;

function TG2Script.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;

function TG2Script.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2Script END

//TG2UI BEGIN
procedure TG2UI.InitBuffers;
  var Indices: PG2Index16Array;
  var CurInd: Integer;
  procedure AddIndQuad(const i0, i1, i2, i3: Word);
  begin
    Indices^[CurInd + 0] := i0;
    Indices^[CurInd + 1] := i1;
    Indices^[CurInd + 2] := i2;
    Indices^[CurInd + 3] := i2;
    Indices^[CurInd + 4] := i1;
    Indices^[CurInd + 5] := i3;
    Inc(CurInd, 6);
  end;
begin
  VB := TG2VB.Create;
  VB.Initialize(Core);
  IB := TG2IB.Create;
  IB.Initialize(Core);
  VB.Verify(SizeOf(TVertex), 16, D3DUSAGE_WRITEONLY, D3DFVF_XYZRHW or D3DFVF_TEX4, D3DPOOL_MANAGED);
  IB.Verify(54, D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_MANAGED);
  IB.Lock(0, 54 * 2, Pointer(Indices), D3DLOCK_DISCARD);
  CurInd := 0;
  AddIndQuad(0, 1, 4, 5);
  AddIndQuad(1, 2, 5, 6);
  AddIndQuad(2, 3, 6, 7);
  AddIndQuad(4, 5, 8, 9);
  AddIndQuad(5, 6, 9, 10);
  AddIndQuad(6, 7, 10, 11);
  AddIndQuad(8, 9, 12, 13);
  AddIndQuad(9, 10, 13, 14);
  AddIndQuad(10, 11, 14, 15);
  IB.UnLock;
end;

procedure TG2UI.OnDeviceLost;
begin

end;

procedure TG2UI.OnDeviceReset;
begin

end;

procedure TG2UI.OnParamsChange;
begin
  m_Root.Left := 0;
  m_Root.Top := 0;
  m_Root.Width := Core.Graphics.Params.Width;
  m_Root.Height := Core.Graphics.Params.Height;
end;

procedure TG2UI.OnKeyDown(const Key: Byte);
begin

end;

procedure TG2UI.OnKeyUp(const Key: Byte);
begin

end;

procedure TG2UI.OnKeyPress(const Key: AnsiChar);
begin

end;

procedure TG2UI.OnMouseDown(const Button: Byte);
  var Frame: TG2UIFrame;
begin
  MouseDownPos := PlugInput.MousePos;
  Frame := FrameAtPoint(MouseDownPos);
  if Frame <> nil then
  Frame.OnMouseDown(Button);
  if m_Overlay <> nil then
  m_Overlay.OnMouseDown(Button);
end;

procedure TG2UI.OnMouseUp(const Button: Byte);
  var Frame: TG2UIFrame;
begin
  Frame := FrameAtPoint(MouseDownPos);
  if Frame <> nil then
  Frame.OnMouseUp(Button);
  if m_Overlay <> nil then
  m_Overlay.OnMouseUp(Button);
end;

procedure TG2UI.OnMouseMove(const Shift: TPoint);
begin

end;

procedure TG2UI.OnWheelMove(const Shift: Integer);
begin

end;

class function TG2UI.ParentToClientRect(const RectParent, RectClient: TRect): TRect;
begin
  Result.Left := RectParent.Left + RectClient.Left;
  Result.Top := RectParent.Top + RectClient.Top;
  Result.Right := RectParent.Left + RectClient.Right;
  Result.Bottom := RectParent.Top + RectClient.Bottom;
end;

class function TG2UI.ClipRect(const Rect1, Rect2: TRect): TRect;
begin
  if Rect1.Left > Rect2.Left then Result.Left := Rect1.Left else Result.Left := Rect2.Left;
  if Rect1.Top > Rect2.Top then Result.Top := Rect1.Top else Result.Top := Rect2.Top;
  if Rect1.Right < Rect2.Right then Result.Right := Rect1.Right else Result.Right := Rect2.Right;
  if Rect1.Bottom < Rect2.Bottom then Result.Bottom := Rect1.Bottom else Result.Bottom := Rect2.Bottom;
end;

constructor TG2UI.Create;
begin
  inherited Create;
  m_Root := nil;
end;

destructor TG2UI.Destroy;
begin
  inherited Destroy;
end;

function TG2UI.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
  InitBuffers;
  m_Render2D := TG2Render2D.Create;
  m_Render2D.Initialize(Core);
  m_Prim2D := TG2Primitives2D.Create;
  m_Prim2D.Initialize(Core);
  Core.RequestPlug(TG2PlugInput, @m_PlugInput);
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_Root := TG2UIFrame.Create(Self);
  m_Root.Left := 0;
  m_Root.Top := 0;
  m_Root.Width := Core.Graphics.Params.Width;
  m_Root.Height := Core.Graphics.Params.Height;
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  m_PlugGraphics.OnParamsChange := OnParamsChange;
  m_PlugInput.OnKeyDown := OnKeyDown;
  m_PlugInput.OnKeyUp := OnKeyUp;
  m_PlugInput.OnKeyPress := OnKeyPress;
  m_PlugInput.OnMouseDown := OnMouseDown;
  m_PlugInput.OnMouseUp := OnMouseUp;
  m_PlugInput.OnMouseMove := OnMouseMove;
  m_PlugInput.OnWheelMove := OnWheelMove;
  Skins.Clear;
  m_Skin := nil;
end;

function TG2UI.Finalize: TG2Result;
  var i: Integer;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
  Core.ReleasePlug(@m_PlugGraphics);
  Core.ReleasePlug(@m_PlugInput);
  m_Root.Free;
  m_Root := nil;
  for i := 0 to Skins.Count - 1 do
  TG2UISkin(Skins[i]).Free;
  m_Prim2D.Finalize;
  m_Prim2D.Free;
  m_Render2D.Finalize;
  m_Render2D.Free;
  IB.Free;
  VB.Free;
end;

procedure TG2UI.Update;
begin
  m_Root.Update;
end;

procedure TG2UI.Render;
begin
  Core.Graphics.RenderStates.ScissorTestEnable := True;
  RenderOverlays := False;
  if m_Skin <> nil then
  m_Root.Render;
  RenderOverlays := True;
  if m_Overlay <> nil then
  m_Overlay.Render;
  Core.Graphics.RenderStates.ScissorTestEnable := False;
end;

function TG2UI.FrameAtPoint(const Pt: TPoint): TG2UIFrame;
  var TargetFrame: TG2UIFrame;
  procedure CheckFrame(const Frame: TG2UIFrame);
    var i: Integer;
    var R: TRect;
  begin
    if (not Frame.Visible)
    or (Frame = m_Overlay) then Exit;
    R := ClipRect(Frame.RectScreen, Frame.RectClip);
    if PtInRect(R, Pt) then
    begin
      TargetFrame := Frame;
      for i := 0 to Frame.SubFrameCount - 1 do
      if Frame.SubFrames[i].CanInput then
      CheckFrame(Frame.SubFrames[i]);
    end;
  end;
begin
  TargetFrame := nil;
  CheckFrame(m_Root);
  Result := TargetFrame;
end;

procedure TG2UI.PushClipRect(const R: TRect);
  var ClipRectPtr: PRect;
begin
  New(ClipRectPtr);
  ClipRectPtr^ := R;
  m_ClipRects.Add(ClipRectPtr);
  Core.Graphics.Device.SetScissorRect(ClipRectPtr);
end;

procedure TG2UI.PopClipRect;
  var ClipRectPtr: PRect;
begin
  if m_ClipRects.Count > 0 then
  begin
    ClipRectPtr := m_ClipRects.Pop;
    Dispose(ClipRectPtr);
    if m_ClipRects.Count > 0 then
    begin
      ClipRectPtr := m_ClipRects.Last;
      Core.Graphics.Device.SetScissorRect(ClipRectPtr);
    end;
  end;
end;
//TG2UI END

//TG2UISkin BEGIN
constructor TG2UISkin.Create(const OwnerGUI: TG2UI);
begin
  inherited Create;
  GUI := OwnerGUI;
  GUI.Skins.Add(Self);
end;

destructor TG2UISkin.Destroy;
  var i: Integer;
begin
  GUI.Skins.Remove(Self);
  for i := 0 to High(m_Textures) do
  m_Textures[i].Free;
  m_Textures := nil;
  m_Elements := nil;
  m_Templates := nil;
  inherited Destroy;
end;

function TG2UISkin.LoadFromFile(const f: WideString): TG2Result;
  var Header: array[0..3] of AnsiChar;
  var fs: TG2FileRW;
  var i, j, t, TexSize: Integer;
  var Data: array of Byte;
begin
  Result := grOk;
  fs := TG2FileRW.Create;
  fs.OpenRead(f);
  try
    fs.ReadBuffer(Header, 4);
    if Header = 'G2UI' then
    begin
      SetLength(m_Textures, fs.ReadSInt4);
      for i := 0 to High(m_Textures) do
      begin
        m_Textures[i] := TG2Texture2D.Create;
        m_Textures[i].Initialize(GUI.Core);
        m_Textures[i].Name := fs.ReadWideString;
        TexSize := fs.ReadSInt4;
        if Length(Data) < TexSize then
        SetLength(Data, TexSize);
        fs.ReadBuffer(Data[0], TexSize);
        Result := m_Textures[i].LoadFromBuffer(@Data[0], TexSize, 1);
        if G2ResFail(Result) then
        begin
          fs.Free;
          Exit;
        end;
      end;
      SetLength(m_Elements, fs.ReadSInt4);
      for i := 0 to High(m_Elements) do
      begin
        m_Elements[i].Name := fs.ReadWideString;
        m_Elements[i].Mapping := TMapping(fs.ReadUInt1);
        fs.ReadBuffer(m_Elements[i].TexCoords, SizeOf(TG2Rect));
        t := fs.ReadSInt4;
        if t > -1 then
        m_Elements[i].Texture := m_Textures[t]
        else
        m_Elements[i].Texture := nil;
      end;
      SetLength(m_Templates, fs.ReadSInt4);
      for i := 0 to High(m_Templates) do
      begin
        m_Templates[i].Name := fs.ReadWideString;
        for j := 0 to 3 do
        begin
          t := fs.ReadSInt4;
          if t > -1 then
          m_Templates[i].Layers[j] := @m_Elements[t]
          else
          m_Templates[i].Layers[j] := nil;
        end;
        m_Templates[i].BorderSize := fs.ReadUInt1;
      end;
    end
    else
    Result := grFail;
  finally
    fs.Free;
  end;
end;

function TG2UISkin.FindTemplate(const Name: WideString): PTemplate;
  var i: Integer;
begin
  for i := 0 to High(m_Templates) do
  if m_Templates[i].Name = Name then
  begin
    Result := @m_Templates[i];
    Exit;
  end;
  Result := nil;
end;

procedure TG2UISkin.DrawTemplate(const TemplateName: WideString; const R: TRect);
  var Vertices: TG2UI.PVertexArr;
  var RectWidth, RectHeight, bgw, bgh: Single;
  var CurrentStage: Integer;
  procedure SetPositions(const bs: Single);
    var x0, x1, x2, x3, y0, y1, y2, y3: Single;
    const POS_BIAS = 0.51;
  begin
    x0 := R.Left - POS_BIAS;
    x1 := R.Left + bs - POS_BIAS;
    x2 := R.Right - bs + POS_BIAS;
    x3 := R.Right + POS_BIAS;
    y0 := R.Top - POS_BIAS;
    y1 := R.Top + bs - POS_BIAS;
    y2 := R.Bottom - bs + POS_BIAS;
    y3 := R.Bottom + POS_BIAS;
    Vertices^[0].Pos.SetValue(x0, y0, 0, 1);
    Vertices^[1].Pos.SetValue(x1, y0, 0, 1);
    Vertices^[2].Pos.SetValue(x2, y0, 0, 1);
    Vertices^[3].Pos.SetValue(x3, y0, 0, 1);
    Vertices^[4].Pos.SetValue(x0, y1, 0, 1);
    Vertices^[5].Pos.SetValue(x1, y1, 0, 1);
    Vertices^[6].Pos.SetValue(x2, y1, 0, 1);
    Vertices^[7].Pos.SetValue(x3, y1, 0, 1);
    Vertices^[8].Pos.SetValue(x0, y2, 0, 1);
    Vertices^[9].Pos.SetValue(x1, y2, 0, 1);
    Vertices^[10].Pos.SetValue(x2, y2, 0, 1);
    Vertices^[11].Pos.SetValue(x3, y2, 0, 1);
    Vertices^[12].Pos.SetValue(x0, y3, 0, 1);
    Vertices^[13].Pos.SetValue(x1, y3, 0, 1);
    Vertices^[14].Pos.SetValue(x2, y3, 0, 1);
    Vertices^[15].Pos.SetValue(x3, y3, 0, 1);
  end;
  procedure SetTexCoordsStrectch(const Element: PElement);
    var x0, x1, x2, x3, y0, y1, y2, y3: Single;
  begin
    x0 := Element^.TexCoords.Left;
    x3 := Element^.TexCoords.Right;
    x1 := G2LerpFloat(x0, x3, bgw);
    x2 := G2LerpFloat(x0, x3, 1 - bgw);
    y0 := Element^.TexCoords.Top;
    y3 := Element^.TexCoords.Bottom;
    y1 := G2LerpFloat(y0, y3, bgh);
    y2 := G2LerpFloat(y0, y3, 1 - bgh);
    Vertices^[0].Tex[CurrentStage].SetValue(x0, y0);
    Vertices^[1].Tex[CurrentStage].SetValue(x1, y0);
    Vertices^[2].Tex[CurrentStage].SetValue(x2, y0);
    Vertices^[3].Tex[CurrentStage].SetValue(x3, y0);
    Vertices^[4].Tex[CurrentStage].SetValue(x0, y1);
    Vertices^[5].Tex[CurrentStage].SetValue(x1, y1);
    Vertices^[6].Tex[CurrentStage].SetValue(x2, y1);
    Vertices^[7].Tex[CurrentStage].SetValue(x3, y1);
    Vertices^[8].Tex[CurrentStage].SetValue(x0, y2);
    Vertices^[9].Tex[CurrentStage].SetValue(x1, y2);
    Vertices^[10].Tex[CurrentStage].SetValue(x2, y2);
    Vertices^[11].Tex[CurrentStage].SetValue(x3, y2);
    Vertices^[12].Tex[CurrentStage].SetValue(x0, y3);
    Vertices^[13].Tex[CurrentStage].SetValue(x1, y3);
    Vertices^[14].Tex[CurrentStage].SetValue(x2, y3);
    Vertices^[15].Tex[CurrentStage].SetValue(x3, y3);
  end;
  procedure SetTexCoordsTile(const Element: PElement);
    var TileCountX, TileCountY: Single;
    var x0, x1, x2, x3, y0, y1, y2, y3: Single;
  begin
    TileCountX := RectWidth / Element^.Texture.RealWidth;
    TileCountY := RectHeight / Element^.Texture.RealHeight;
    x0 := 0;
    x3 := TileCountX;
    x1 := G2LerpFloat(x0, x3, bgw);
    x2 := G2LerpFloat(x0, x3, 1 - bgw);
    y0 := 0;
    y3 := TileCountY;
    y1 := G2LerpFloat(y0, y3, bgh);
    y2 := G2LerpFloat(y0, y3, 1 - bgh);
    Vertices^[0].Tex[CurrentStage].SetValue(x0, y0);
    Vertices^[1].Tex[CurrentStage].SetValue(x1, y0);
    Vertices^[2].Tex[CurrentStage].SetValue(x2, y0);
    Vertices^[3].Tex[CurrentStage].SetValue(x3, y0);
    Vertices^[4].Tex[CurrentStage].SetValue(x0, y1);
    Vertices^[5].Tex[CurrentStage].SetValue(x1, y1);
    Vertices^[6].Tex[CurrentStage].SetValue(x2, y1);
    Vertices^[7].Tex[CurrentStage].SetValue(x3, y1);
    Vertices^[8].Tex[CurrentStage].SetValue(x0, y2);
    Vertices^[9].Tex[CurrentStage].SetValue(x1, y2);
    Vertices^[10].Tex[CurrentStage].SetValue(x2, y2);
    Vertices^[11].Tex[CurrentStage].SetValue(x3, y2);
    Vertices^[12].Tex[CurrentStage].SetValue(x0, y3);
    Vertices^[13].Tex[CurrentStage].SetValue(x1, y3);
    Vertices^[14].Tex[CurrentStage].SetValue(x2, y3);
    Vertices^[15].Tex[CurrentStage].SetValue(x3, y3);
  end;
  procedure SetTexCoordsBorder(const Element: PElement);
    var x0, x1, x2, y0, y1, y2: Single;
  begin
    x0 := Element^.TexCoords.Left;
    x1 := (Element^.TexCoords.Left + Element^.TexCoords.Right) * 0.5;
    x2 := Element^.TexCoords.Right;
    y0 := Element^.TexCoords.Top;
    y1 := (Element^.TexCoords.Top + Element^.TexCoords.Bottom) * 0.5;
    y2 := Element^.TexCoords.Bottom;
    Vertices^[0].Tex[CurrentStage].SetValue(x0, y0);
    Vertices^[1].Tex[CurrentStage].SetValue(x1, y0);
    Vertices^[2].Tex[CurrentStage].SetValue(x1, y0);
    Vertices^[3].Tex[CurrentStage].SetValue(x2, y0);
    Vertices^[4].Tex[CurrentStage].SetValue(x0, y1);
    Vertices^[5].Tex[CurrentStage].SetValue(x1, y1);
    Vertices^[6].Tex[CurrentStage].SetValue(x1, y1);
    Vertices^[7].Tex[CurrentStage].SetValue(x2, y1);
    Vertices^[8].Tex[CurrentStage].SetValue(x0, y1);
    Vertices^[9].Tex[CurrentStage].SetValue(x1, y1);
    Vertices^[10].Tex[CurrentStage].SetValue(x1, y1);
    Vertices^[11].Tex[CurrentStage].SetValue(x2, y1);
    Vertices^[12].Tex[CurrentStage].SetValue(x0, y2);
    Vertices^[13].Tex[CurrentStage].SetValue(x1, y2);
    Vertices^[14].Tex[CurrentStage].SetValue(x1, y2);
    Vertices^[15].Tex[CurrentStage].SetValue(x2, y2);
  end;
  var t: PTemplate;
  var e: PElement;
  var i: Integer;
begin
  t := FindTemplate(TemplateName);
  if t = nil then Exit;
  RectWidth := (R.Right - R.Left + 1);
  RectHeight := (R.Bottom - R.Top + 1);
  bgw := t^.BorderSize / RectWidth;
  bgh := t^.BorderSize / RectHeight;
  GUI.VB.Lock(0, GUI.VB.Count * GUI.VB.Stride, Pointer(Vertices), D3DLOCK_DISCARD);
  SetPositions(t^.BorderSize);
  CurrentStage := 0;
  for i := 0 to 3 do
  if t^.Layers[i] <> nil then
  begin
    e := t^.Layers[i];
    if e^.Texture <> nil then
    begin
      case e^.Mapping of
        mtStretch: SetTexCoordsStrectch(e);
        mtTile: SetTexCoordsTile(e);
        mtBorder: SetTexCoordsBorder(e);
      end;
      GUI.Core.Graphics.Device.SetTexture(CurrentStage, e^.Texture.Texture);
      if CurrentStage = 0 then
      begin
        GUI.Core.Graphics.TextureStageStages.AlphaArg1[CurrentStage] := D3DTA_TEXTURE;
        GUI.Core.Graphics.TextureStageStages.AlphaOp[CurrentStage] := D3DTOP_SELECTARG1;
      end
      else
      begin
        if i = 3 then
        begin
          GUI.Core.Graphics.TextureStageStages.ColorArg1[CurrentStage] := D3DTA_CURRENT;
          GUI.Core.Graphics.TextureStageStages.ColorOp[CurrentStage] := D3DTOP_SELECTARG1;
          GUI.Core.Graphics.TextureStageStages.AlphaArg1[CurrentStage] := D3DTA_TEXTURE;
          GUI.Core.Graphics.TextureStageStages.AlphaOp[CurrentStage] := D3DTOP_SELECTARG1;
        end
        else
        begin
          GUI.Core.Graphics.TextureStageStages.ColorArg1[CurrentStage] := D3DTA_CURRENT;
          GUI.Core.Graphics.TextureStageStages.ColorArg2[CurrentStage] := D3DTA_TEXTURE;
          GUI.Core.Graphics.TextureStageStages.ColorOp[CurrentStage] := D3DTOP_BLENDCURRENTALPHA;
          GUI.Core.Graphics.TextureStageStages.AlphaArg1[CurrentStage] := D3DTA_TEXTURE;
          GUI.Core.Graphics.TextureStageStages.AlphaArg2[CurrentStage] := D3DTA_CURRENT;
          GUI.Core.Graphics.TextureStageStages.AlphaOp[CurrentStage] := D3DTOP_ADD;
        end;
      end;
      Inc(CurrentStage);
    end;
  end;
  GUI.VB.UnLock;
  if CurrentStage > 0 then
  begin
    GUI.IB.SetToDevice;
    GUI.VB.SetToDevice;
    GUI.Core.Graphics.Device.DrawIndexedPrimitive(
      D3DPT_TRIANGLELIST,
      0, 0, GUI.VB.Count,
      0, GUI.IB.Count div 3
    );
    GUI.Core.Graphics.TextureStageStages.SetDefaults;
  end;
end;
//TG2UISkin END

//TG2UIFrame BEGIN
procedure TG2UIFrame.SetParent(const Value: TG2UIFrame);
begin
  if m_Parent <> nil then
  m_Parent.SubFrameRemove(Self);
  m_Parent := Value;
  if m_Parent <> nil then
  m_Parent.SubFrameAdd(Self);
  if m_Initialized then
  begin
    ClientRectAdjust;
    ScreenRectAdjust;
  end;
end;

function TG2UIFrame.GetSubFrameCount: Integer;
begin
  Result := m_SubFrames.Count;
end;

function TG2UIFrame.GetSubFrame(const Index: Integer): TG2UIFrame;
begin
  Result := TG2UIFrame(m_SubFrames[Index]);
end;

function TG2UIFrame.GetLeft: Integer;
begin
  Result := m_RectSelf.Left;
end;

procedure TG2UIFrame.SetLeft(const Value: Integer);
begin
  m_RectSelf.Left := Value;
  if m_RectSelf.Right < m_RectSelf.Left then
  m_RectSelf.Right := m_RectSelf.Left;
  ClientRectAdjust;
  ScreenRectAdjust;
end;

function TG2UIFrame.GetTop: Integer;
begin
  Result := m_RectSelf.Top;
end;

procedure TG2UIFrame.SetTop(const Value: Integer);
begin
  m_RectSelf.Top := Value;
  if m_RectSelf.Bottom < m_RectSelf.Top then
  m_RectSelf.Bottom := m_RectSelf.Top;
  ClientRectAdjust;
  ScreenRectAdjust;
end;

function TG2UIFrame.GetRight: Integer;
begin
  Result := m_RectSelf.Right;
end;

procedure TG2UIFrame.SetRight(const Value: Integer);
begin
  m_RectSelf.Right := Value;
  if m_RectSelf.Left > m_RectSelf.Right then
  m_RectSelf.Left := m_RectSelf.Right;
  ClientRectAdjust;
  ScreenRectAdjust;
end;

function TG2UIFrame.GetBottom: Integer;
begin
  Result := m_RectSelf.Bottom;
end;

procedure TG2UIFrame.SetBottom(const Value: Integer);
begin
  m_RectSelf.Bottom := Value;
  if m_RectSelf.Top > m_RectSelf.Bottom then
  m_RectSelf.Top := m_RectSelf.Bottom;
  ClientRectAdjust;
  ScreenRectAdjust;
end;

procedure TG2UIFrame.SetX(const Value: Integer);
  var AdjustRight: Integer;
begin
  AdjustRight := m_RectSelf.Right - m_RectSelf.Left;
  m_RectSelf.Left := Value;
  m_RectSelf.Right := m_RectSelf.Left + AdjustRight;
  ClientRectAdjust;
  ScreenRectAdjust;
end;

procedure TG2UIFrame.SetY(const Value: Integer);
  var AdjustBottom: Integer;
begin
  AdjustBottom := m_RectSelf.Bottom - m_RectSelf.Top;
  m_RectSelf.Top := Value;
  m_RectSelf.Bottom := m_RectSelf.Top + AdjustBottom;
  ClientRectAdjust;
  ScreenRectAdjust;
end;

function TG2UIFrame.GetWidth: Integer;
begin
  Result := m_RectSelf.Right - m_RectSelf.Left + 1;
end;

procedure TG2UIFrame.SetWidth(const Value: Integer);
begin
  if Value < 0 then Exit;
  m_RectSelf.Right := m_RectSelf.Left + (Value - 1);
  ClientRectAdjust;
  ScreenRectAdjust;
end;

function TG2UIFrame.GetHeight: Integer;
begin
  Result := m_RectSelf.Bottom - m_RectSelf.Top + 1;
end;

procedure TG2UIFrame.SetHeight(const Value: Integer);
begin
  if Value < 0 then Exit;
  m_RectSelf.Bottom := m_RectSelf.Top + (Value - 1);
  ClientRectAdjust;
  ScreenRectAdjust;
end;

function TG2UIFrame.GetClientWidth: Integer;
begin
  Result := m_RectClient.Right - m_RectClient.Left + 1;
end;

function TG2UIFrame.GetClientHeight: Integer;
begin
  Result := m_RectClient.Bottom - m_RectClient.Top + 1;
end;

procedure TG2UIFrame.SetOrder(const Value: Integer);
begin
  if m_Order = Value then Exit;
  m_Order := Value;
  if m_Parent <> nil then
  m_Parent.SubFrameRemove(Self);
  m_Parent.SubFrameAdd(Self);
end;

procedure TG2UIFrame.ClientRectAdjust;
begin
  m_RectClient.Left := 0;
  m_RectClient.Top := 0;
  m_RectClient.Right := Width - 1;
  m_RectClient.Bottom := Height - 1;
end;

procedure TG2UIFrame.ScreenRectAdjust;
  var r: TRect;
  var i: Integer;
begin
  if Parent = nil then
  begin
    RectScreen := RectLocal;
    RectClip := RectLocal;
    RectClip.Right := RectClip.Right + 1;
    RectClip.Bottom := RectClip.Bottom + 1;
  end
  else
  begin
    r := GUI.ParentToClientRect(Parent.RectScreen, Parent.RectClient);
    RectScreen := GUI.ParentToClientRect(r, RectLocal);
    r.Right := r.Right + 1;
    r.Bottom := r.Bottom + 1;
    RectClip := GUI.ClipRect(r, Parent.RectClip);
  end;
  for i := 0 to m_SubFrames.Count - 1 do
  TG2UIFrame(m_SubFrames[i]).ScreenRectAdjust;
end;

procedure TG2UIFrame.Initialize;
begin

end;

procedure TG2UIFrame.Finalize;
begin

end;

procedure TG2UIFrame.OnUpdate;
begin

end;

procedure TG2UIFrame.OnRender;
begin

end;

procedure TG2UIFrame.OnDeviceLost;
begin

end;

procedure TG2UIFrame.OnDeviceReset;
begin

end;

procedure TG2UIFrame.OnParamsChange;
begin

end;

procedure TG2UIFrame.OnKeyOnwn(const Key: Byte);
begin

end;

procedure TG2UIFrame.OnKeyUp(const Key: Byte);
begin

end;

procedure TG2UIFrame.OnKeyPress(const Key: AnsiChar);
begin

end;

procedure TG2UIFrame.OnMouseDown(const Button: Byte);
begin

end;

procedure TG2UIFrame.OnMouseUp(const Button: Byte);
begin

end;

procedure TG2UIFrame.OnMouseMove(const Shift: TPoint);
begin

end;

procedure TG2UIFrame.OnWheelMove(const Shift: Integer);
begin

end;

function TG2UIFrame.IsMouseOver: Boolean;
begin
  Result := PtInRect(
    GUI.ClipRect(RectClip, RectScreen),
    GUI.PlugInput.MousePos
  );
end;

function TG2UIFrame.IsMouseDown: Boolean;
  var R: TRect;
begin
  if GUI.PlugInput.MouseDown[0] then
  begin
    R := GUI.ClipRect(RectClip, RectScreen);
    Result := (
      PtInRect(R, GUI.PlugInput.MousePos)
      and PtInRect(R, GUI.MouseDownPos)
    );
  end
  else
  Result := False;
end;

procedure TG2UIFrame.SubFrameAdd(const Frame: TG2UIFrame);
begin
  m_SubFrames.Add(Frame, Frame.Order);
end;

procedure TG2UIFrame.SubFrameRemove(const Frame: TG2UIFrame);
begin
  m_SubFrames.Remove(Frame);
end;

constructor TG2UIFrame.Create(const OwnerGUI: TG2UI);
begin
  inherited Create;
  m_Initialized := False;
  m_Visible := True;
  m_Order := 0;
  GUI := OwnerGUI;
  m_SubFrames.Capacity := 16;
  m_SubFrames.Clear;
  Parent := GUI.Root;
  m_RectSelf := Rect(0, 0, 63, 63);
  CanFocus := False;
  CanInput := True;
  Initialize;
  m_Initialized := True;
  ClientRectAdjust;
  ScreenRectAdjust;
end;

destructor TG2UIFrame.Destroy;
  var i: Integer;
begin
  Finalize;
  for i := m_SubFrames.Count - 1 downto 0 do
  TG2UIFrame(m_SubFrames[i]).Free;
  Parent := nil;
  inherited Destroy;
end;

procedure TG2UIFrame.Update;
  var i: Integer;
begin
  OnUpdate;
  for i := 0 to m_SubFrames.Count - 1 do
  TG2UIFrame(m_SubFrames[i]).Update;
end;

procedure TG2UIFrame.Render;
  var i: Integer;
begin
  if not m_Visible
  or ((Self = GUI.Overlay) and not GUI.RenderOverlays) then Exit;
  GUI.PushClipRect(RectClip);
  OnRender;
  for i := 0 to m_SubFrames.Count - 1 do
  TG2UIFrame(m_SubFrames[i]).Render;
  GUI.PopClipRect;
end;

function TG2UIFrame.RectToGlobal(const R: TRect): TRect;
begin
  Result := GUI.ParentToClientRect(RectScreen, R);
end;
//TG2UIFrame END

//TG2UIPanel BEGIN
procedure TG2UIPanel.Initialize;
begin
  m_Template := 'Panel';
end;

procedure TG2UIPanel.OnRender;
begin
  GUI.Skin.DrawTemplate(WideString(m_Template), RectGlobal);
end;
//TG2UIPanel END

//TG2UIImage BEGIN
procedure TG2UIImage.Initialize;
begin
  m_Texture := nil;
end;

procedure TG2UIImage.OnRender;
begin
  if m_Texture <> nil then
  GUI.Render2D.DrawRect(
    RectGlobal.Left, RectGlobal.Top,
    RectGlobal.Right - RectGlobal.Left + 1, RectGlobal.Bottom - RectGlobal.Top + 1,
    $ffffffff, m_Texture
  );
end;
//TG2UIImage END

//TG2UIButton BEGIN
procedure TG2UIButton.AdjustLabel;
begin
  m_Label.X := (m_RectClient.Right - m_RectClient.Left + 1 - m_Label.Width) div 2;
  m_Label.Y := (m_RectClient.Bottom - m_RectClient.Top + 1 - m_Label.Height) div 2;
end;

procedure TG2UIButton.AdjustImage;
begin
  if m_Image.Texture <> nil then
  begin
    m_Image.X := (ClientWidth - m_Image.Width) div 2;
    m_Image.Y := (ClientHeight - m_Image.Height) div 2;
  end;
end;

procedure TG2UIButton.Initialize;
begin
  m_Image := TG2UIImage.Create(GUI);
  m_Image.Parent := Self;
  m_Image.CanInput := False;
  m_Label := TG2UILabel.Create(GUI);
  m_Label.Parent := Self;
  m_Label.OnChange := AdjustLabel;
  m_Label.Text := 'Button';
  m_Label.CanInput := False;
end;

procedure TG2UIButton.Finalize;
begin
  m_Label.Free;
  m_Image.Free;
end;

procedure TG2UIButton.ClientRectAdjust;
begin
  m_RectClient.Left := 4;
  m_RectClient.Top := 4;
  m_RectClient.Right := Width - 1 - 4;
  m_RectClient.Bottom := Height - 1 - 4;
  AdjustLabel;
  AdjustImage;
end;

procedure TG2UIButton.OnRender;
begin
  if IsMouseDown then
  GUI.Skin.DrawTemplate('ButtonDown', RectGlobal)
  else if IsMouseOver then
  GUI.Skin.DrawTemplate('ButtonOver', RectGlobal)
  else
  GUI.Skin.DrawTemplate('Button', RectGlobal);
end;

procedure TG2UIButton.OnMouseUp(const Button: Byte);
begin
  if IsMouseDown
  and Assigned(m_ProcOnClick) then
  m_ProcOnClick();
end;

function TG2UIButton.GetImage: TG2Texture2D;
begin
  Result := m_Image.Texture;
end;

procedure TG2UIButton.SetImage(const Value: TG2Texture2D);
begin
  m_Image.Texture := Value;
  AdjustImage;
end;

function TG2UIButton.GetImageWidth: Integer;
begin
  Result := m_Image.Width;
end;

procedure TG2UIButton.SetImageWidth(const Value: Integer);
begin
  m_Image.Width := Value;
  AdjustImage;
end;

function TG2UIButton.GetImageHeight: Integer;
begin
  Result := m_Image.Height;
end;

procedure TG2UIButton.SetImageHeight(const Value: Integer);
begin
  m_Image.Height := Value;
  AdjustImage;
end;

function TG2UIButton.GetCaption: AnsiString;
begin
  Result := m_Label.Text;
end;

procedure TG2UIButton.SetCaption(const Value: AnsiString);
begin
  m_Label.Text := Value;
end;
//TG2UIButton END

//TG2UIButtonSwitch BEGIN
procedure TG2UIButtonSwitch.Initialize;
begin
  inherited Initialize;
  m_SwitchGroup := 0;
  m_Switch := False;
end;

procedure TG2UIButtonSwitch.OnMouseDown(const Button: Byte);
begin
  Switch := True;
end;

procedure TG2UIButtonSwitch.SetSwitch(const Value: Boolean);
  var i: Integer;
begin
  if m_Switch <> Value then
  begin
    m_Switch := Value;
    if Assigned(m_ProcOnSwitch) then
    m_ProcOnSwitch();
    if m_Switch
    and (m_SwitchGroup > 0)
    and (Parent <> nil) then
    for i := 0 to Parent.SubFrameCount - 1 do
    begin
      if (Parent.SubFrames[i] is TG2UIButtonSwitch)
      and (Parent.SubFrames[i] <> Self)
      and (TG2UIButtonSwitch(Parent.SubFrames[i]).SwitchGroup = m_SwitchGroup) then
      TG2UIButtonSwitch(Parent.SubFrames[i]).Switch := False;
    end;
  end;
end;

procedure TG2UIButtonSwitch.OnRender;
begin
  if Switch then
  GUI.Skin.DrawTemplate('ButtonDown', RectGlobal)
  else if IsMouseOver then
  GUI.Skin.DrawTemplate('ButtonOver', RectGlobal)
  else
  GUI.Skin.DrawTemplate('Button', RectGlobal);
end;
//TG2UIButtonSwitch END

//TG2UILabel BEGIN
procedure TG2UILabel.SetText(const Value: AnsiString);
begin
  m_Text := Value;
  Width := m_Font.GetTextWidth(m_Text);
  Height := m_Font.GetTextHeight(m_Text);
  if Assigned(m_OnChange) then m_OnChange;
end;

procedure TG2UILabel.SetFont(const Value: TG2Font);
begin
  m_Font := Value;
  SetText(m_Text);
end;

procedure TG2UILabel.Initialize;
begin
  m_Font := GUI.Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  Text := 'Label';
  m_OnChange := nil;
end;

procedure TG2UILabel.OnRender;
begin
  m_Font.Print(
    RectGlobal.Left, RectGlobal.Top,
    m_FontColor, m_Text
  );
end;
//TG2UILabel END

//TG2UIEdit BEGIN
procedure TG2UIEdit.SetText(const Value: AnsiString);
begin
  m_Text := Value;
end;

procedure TG2UIEdit.SetFont(const Value: TG2Font);
begin
  m_Font := Value;
end;

procedure TG2UIEdit.Initialize;
begin
  m_Font := GUI.Core.Graphics.Shared.RequestFont('Arial', 12);
  m_Text := '';
end;

procedure TG2UIEdit.OnRender;
begin
  GUI.Skin.DrawTemplate('Edit', RectGlobal);
end;
//TG2UIEdit END

(*
//TG2GUI BEGIN
constructor TG2GUI.Create;
begin
  inherited Create;
end;

destructor TG2GUI.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUI.OnMouseDown(const Button: Byte);
begin
  m_Root.MouseDown(Button);
  if Assigned(m_Focus) then
  m_Focus.CheckFocus;
end;

procedure TG2GUI.OnMouseUp(const Button: Byte);
begin
  m_Root.MouseUp(Button);
end;

procedure TG2GUI.OnMouseMove(const Shift: TPoint);
begin
  m_Root.MouseMove(Shift);
end;

procedure TG2GUI.OnMouseWheel(const Shift: Integer);
begin
  m_Root.MouseWheel(Shift);
end;

procedure TG2GUI.OnKeyDown(const Key: Byte);
begin
  m_Root.KeyDown(Key);
end;

procedure TG2GUI.OnKeyUp(const Key: Byte);
begin
  m_Root.KeyUp(Key);
end;

procedure TG2GUI.OnKeyPress(const Key: AnsiChar);
begin
  m_Root.KeyPress(Key);
end;

procedure TG2GUI.SetFocus(const Value: TG2GUIWindow);
begin
  if m_Focus = Value then Exit;
  if Assigned(m_Focus) then
  m_Focus.FocusLose;
  if Assigned(Value) then
  m_Focus := Value
  else
  m_Focus := m_Root;
  if Assigned(m_Focus) then
  m_Focus.FocusReceive;
end;

procedure TG2GUI.WindowCreate(const WindowClass: CG2GUIWindowClass; const Parent: TG2GUIWindow; var Window);
var
  Instance: TG2GUIWindow;
begin
  Instance := TG2GUIWindow(WindowClass.NewInstance);
  TG2GUIWindow(Window) := Instance;
  Instance.Create;
  Instance.GUI := Self;
  Instance.Initialize(Core);
  if Assigned(Parent) then
  Instance.Parent := Parent
  else
  Instance.Parent := m_Root;
end;

procedure TG2GUI.WindowDestroy(const Window: TG2GUIWindow);
begin
  if Window <> m_Root then
  begin
    if m_Focus = Window then
    m_Focus := nil;
    if Assigned(Window.Parent) then
    Window.Parent.Children.Remove(Window);
    Window.Free;
  end;
end;

procedure TG2GUI.Render;
begin
  Core.Graphics.RenderStates.ScissorTestEnable := True;
  m_Root.Draw;
  Core.Graphics.RenderStates.ScissorTestEnable := False;
end;

procedure TG2GUI.Update;
begin
  m_Root.Update;
end;

function TG2GUI.Initialize(const G2Core: TG2Core): TG2Result;
var
  ViewPort: TD3DViewPort9;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Root := TG2GUIWindow.Create;
  m_Root.Initialize(Core);
  m_Root.GUI := Self;
  m_Focus := m_Root;
  ViewPort := Core.Graphics.GetViewPort;
  m_Root.X := ViewPort.X;
  m_Root.Y := ViewPort.Y;
  m_Root.W := ViewPort.Width;
  m_Root.H := ViewPort.Height;
  Core.RequestPlug(TG2PlugInput, @m_PlugInput);
  m_PlugInput.OnMouseDown := OnMouseDown;
  m_PlugInput.OnMouseUp := OnMouseUp;
  m_PlugInput.OnMouseMove := OnMouseMove;
  m_PlugInput.OnWheelMove := OnMouseWheel;
  m_PlugInput.OnKeyDown := OnKeyDown;
  m_PlugInput.OnKeyUp := OnKeyUp;
  m_PlugInput.OnKeyPress := OnKeyPress;
  m_Render2D := TG2Render2D.Create;
  m_Render2D.Initialize(Core);
  m_Prim2D := TG2Primitives2D.Create;
  m_Prim2D.Initialize(Core);
  Result := grOk;
end;

function TG2GUI.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  m_Prim2D.Finalize;
  m_Prim2D.Free;
  m_Render2D.Finalize;
  m_Render2D.Free;
  Core.ReleasePlug(@m_PlugInput);
  m_Root.Finalize;
  m_Root.Free;
  Result := grOk;
end;
//TG2GUI END

//TG2GUIWindow BEGIN
constructor TG2GUIWindow.Create;
begin
  inherited Create;
end;

destructor TG2GUIWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUIWindow.SetParent(const Value: TG2GUIWindow);
begin
  if Assigned(m_Parent) then m_Parent.m_Children.Remove(Self);
  m_Parent := Value;
  if Assigned(m_Parent) then m_Parent.m_Children.Add(Self);
end;

procedure TG2GUIWindow.SetRect(const Value: TRect);
begin
  m_X := Value.Left;
  m_Y := Value.Top;
  m_W := Value.Right - Value.Left;
  m_H := Value.Bottom - Value.Top;
end;

function TG2GUIWindow.GetRect: TRect;
begin
  Result.Left := m_X;
  Result.Top := m_Y;
  Result.Right := m_X + m_W;
  Result.Bottom := m_Y + m_H;
end;

procedure TG2GUIWindow.SetClientRect(const Value: TRect);
begin
  m_MarginLeft := Max(Value.Left, 0);
  m_MarginTop := Max(Value.Top, 0);
  m_MarginRight := Min(m_W - Value.Left - Value.Right, 0);
  m_MarginBottom := Min(m_H - Value.Top - Value.Bottom, 0);
end;

function TG2GUIWindow.GetClientRect: TRect;
begin
  Result.Left := m_MarginLeft;
  Result.Top := m_MarginTop;
  Result.Right := m_W - m_MarginRight;
  Result.Bottom := m_H - m_MarginBottom;
end;

function TG2GUIWindow.GetDrawRect: TRect;
var
  ParentRect: TRect;
begin
  Result := GetRect;
  if Assigned(m_Parent) then
  begin
    ParentRect := m_Parent.ClientDrawRect;
    Result.Left := Max(Result.Left + m_Parent.X + m_Parent.MarginLeft, ParentRect.Left);
    Result.Top := Max(Result.Top + m_Parent.Y + m_Parent.MarginTop, ParentRect.Top);
    Result.Right := Min(Result.Right + m_Parent.X + m_Parent.MarginLeft, ParentRect.Right);
    Result.Bottom := Min(Result.Bottom + m_Parent.Y + m_Parent.MarginTop, ParentRect.Bottom);
  end;
end;

function TG2GUIWindow.GetClientDrawRect: TRect;
var
  ParentRect: TRect;
begin
  Result := GetClientRect;
  Result.Left := Result.Left + m_X;
  Result.Top := Result.Top + m_Y;
  Result.Right := Result.Right + m_X;
  Result.Bottom := Result.Bottom + m_Y;
  if Assigned(m_Parent) then
  begin
    ParentRect := m_Parent.ClientDrawRect;
    Result.Left := Max(Result.Left + m_Parent.X, ParentRect.Left);
    Result.Top := Max(Result.Top + m_Parent.Y, ParentRect.Top);
    Result.Right := Min(Result.Right + m_Parent.X, ParentRect.Right);
    Result.Bottom := Min(Result.Bottom + m_Parent.Y, ParentRect.Bottom);
  end;
end;

function TG2GUIWindow.GetFocused: Boolean;
begin
  Result := m_GUI.Focus = Self;
end;

procedure TG2GUIWindow.SetW(const Value: Integer);
begin
  m_W := Value;
  Resize;
end;

procedure TG2GUIWindow.SetH(const Value: Integer);
begin
  m_H := Value;
  Resize;
end;

procedure TG2GUIWindow.MouseDown(const Button: Byte);
var
  i: Integer;
  ProcessSelf: Boolean;
begin
  if not m_Visible then Exit;
  ProcessSelf := True;
  for i := m_Children.Count - 1 downto 0 do
  if TG2GUIWindow(m_Children[i]).IsMouseOver then
  begin
    ProcessSelf := False;
    TG2GUIWindow(m_Children[i]).MouseDown(Button);
  end;
  if ProcessSelf then
  begin
    if IsMouseOver and (Button = 0) then
    m_DownInRect := True;
    CustomMouseDown(Button);
    if Assigned(m_OnMouseDown) then
    m_OnMouseDown(Button);
  end;
end;

procedure TG2GUIWindow.MouseUp(const Button: Byte);
var
  i: Integer;
  ProcessSelf: Boolean;
begin
  if not m_Visible then Exit;
  ProcessSelf := True;
  for i := m_Children.Count - 1 downto 0 do
  if TG2GUIWindow(m_Children[i]).IsMouseOver then
  begin
    ProcessSelf := False;
    TG2GUIWindow(m_Children[i]).MouseUp(Button);
  end;
  if ProcessSelf then
  begin
    if Button = 0 then
    begin
      if IsPressed then
      MouseClick;
      m_DownInRect := False;
    end;
    CustomMouseUp(Button);
    if Assigned(m_OnMouseUp) then
    m_OnMouseUp(Button);
  end;
end;

procedure TG2GUIWindow.MouseClick;
begin
  if not m_Visible then Exit;
  SetFocus;
  CustomMouseClick;
  if Assigned(m_OnMouseClick) then
  m_OnMouseClick;
end;

procedure TG2GUIWindow.MouseMove(const Shift: TPoint);
var
  i: Integer;
  MOver: Boolean;
begin
  if not m_Visible then Exit;
  for i := m_Children.Count - 1 downto 0 do
  TG2GUIWindow(m_Children[i]).MouseMove(Shift);
  CustomMouseMove(Shift);
  if Assigned(m_OnMouseMove) then
  m_OnMouseMove(Shift);
  MOver := IsMouseOver;
  if MOver <> m_MoveInRect then
  begin
    m_MoveInRect := MOver;
    if MOver then
    MouseEnter
    else
    MouseLeave;
  end;
end;

procedure TG2GUIWindow.MouseWheel(const Shift: Integer);
var
  i: Integer;
begin
  if not m_Visible then Exit;
  for i := m_Children.Count - 1 downto 0 do
  TG2GUIWindow(m_Children[i]).MouseWheel(Shift);
  if Focused then
  begin
    CustomMouseWheel(Shift);
    if Assigned(m_OnWheelMove) then
    m_OnWheelMove(Shift);
  end;
end;

procedure TG2GUIWindow.KeyDown(const Key: Byte);
var
  i: Integer;
begin
  if not m_Visible then Exit;
  for i := m_Children.Count - 1 downto 0 do
  TG2GUIWindow(m_Children[i]).KeyDown(Key);
  if Focused then
  begin
    CustomKeyDown(Key);
    if Assigned(m_OnKeyDown) then
    m_OnKeyDown(Key);
  end;
end;

procedure TG2GUIWindow.KeyUp(const Key: Byte);
var
  i: Integer;
begin
  if not m_Visible then Exit;
  for i := m_Children.Count - 1 downto 0 do
  TG2GUIWindow(m_Children[i]).KeyUp(Key);
  if Focused then
  begin
    CustomKeyUp(Key);
    if Assigned(m_OnKeyUp) then
    m_OnKeyUp(Key);
  end;
end;

procedure TG2GUIWindow.KeyPress(const Key: AnsiChar);
var
  i: Integer;
begin
  if not m_Visible then Exit;
  for i := m_Children.Count - 1 downto 0 do
  TG2GUIWindow(m_Children[i]).KeyPress(Key);
  if Focused then
  begin
    CustomKeyPress(Key);
    if Assigned(m_OnKeyPress) then
    m_OnKeyPress(Key);
  end;
end;

procedure TG2GUIWindow.MouseEnter;
begin
  if not m_Visible then Exit;
  CustomMouseEnter;
  if Assigned(m_OnMouseEnter) then
  m_OnMouseEnter;
end;

procedure TG2GUIWindow.MouseLeave;
begin
  if not m_Visible then Exit;
  CustomMouseLeave;
  if Assigned(m_OnMouseLeave) then
  m_OnMouseLeave;
end;

procedure TG2GUIWindow.FocusReceive;
begin
  if not m_Visible then Exit;
  CustomFocusReceive;
  if Assigned(m_OnFocusReceive) then
  m_OnFocusReceive;
end;

procedure TG2GUIWindow.FocusLose;
begin
  if not m_Visible then Exit;
  CustomFocusLose;
  if Assigned(m_OnFocusLose) then
  m_OnFocusLose;
end;

procedure TG2GUIWindow.Resize;
begin
  if not m_Visible then Exit;
  CustomResize;
  if Assigned(m_OnResize) then
  m_OnResize;
end;

procedure TG2GUIWindow.CheckFocus;
begin
  if not Focused then Exit;
  if not IsMouseOver then
  GUI.Focus := nil;
end;

function TG2GUIWindow.GetGlobalRect: TRect;
var
  ParentRect: TRect;
begin
  if Assigned(m_Parent) then
  begin
    ParentRect := m_Parent.GetGlobalRect;
    Result.Left := ParentRect.Left + m_X + m_Parent.MarginLeft;
    Result.Top := ParentRect.Top + m_Y + m_Parent.MarginTop;
    Result.Right := Result.Left + m_W;
    Result.Bottom := Result.Top + m_H;
  end
  else
  Result := GetRect;
end;

procedure TG2GUIWindow.PreRender;
var
  R: TRect;
begin
  Core.Graphics.Device.SetTexture(0, nil);
  R := GetDrawRect;
  Core.Graphics.Device.SetScissorRect(@R);
end;

procedure TG2GUIWindow.Render;
begin

end;

procedure TG2GUIWindow.PostRender;
var
  i: Integer;
begin
  for i := 0 to m_Children.Count - 1 do
  TG2GUIWindow(m_Children[i]).Draw;
end;

procedure TG2GUIWindow.Draw;
begin
  if m_Visible then
  begin
    PreRender;
    Render;
    PostRender;
  end;
end;

procedure TG2GUIWindow.Update;
var
  i: Integer;
begin
  CustomUpdate;
  for i := 0 to m_Children.Count - 1 do
  TG2GUIWindow(m_Children[i]).Update;
end;

procedure TG2GUIWindow.SetFocus;
begin
  m_GUI.Focus := Self;
end;

procedure TG2GUIWindow.SetRectByGrid(const l, t, r, b: Integer);
begin
  X := l * 8;
  Y := t * 8;
  W := (r - l) * 8;
  H := (b - t) * 8;
end;

procedure TG2GUIWindow.RenderBox(
      const BoxRect: TRect;
      const Lowered: Boolean = False;
      const ColNormal: DWord = $ffcccccc;
      const ColHighlight: DWord = $ffffffff;
      const ColShadow: DWord = $ff555555
    );
begin
  RenderBox(
    BoxRect,
    MarginLeft, MarginTop, MarginRight, MarginBottom,
    Lowered, ColNormal, ColHighlight, ColShadow
  );
end;

procedure TG2GUIWindow.RenderBox(
      const BoxRect: TRect;
      const MarginL, MarginT, MarginR, MarginB: Integer;
      const Lowered: Boolean = False;
      const ColNormal: DWord = $ffcccccc;
      const ColHighlight: DWord = $ffffffff;
      const ColShadow: DWord = $ff555555
    );
var
  ColTL, ColBR: TG2Color;
begin
  if Lowered then
  begin
    ColTL := ColShadow;
    ColBR := ColHighlight;
  end
  else
  begin
    ColTL := ColHighlight;
    ColBR := ColShadow;
  end;
  with GUI.Render2D do
  begin
    DrawBegin(ptTriangleList);

    AddPos(G2Vec2(BoxRect.Left, BoxRect.Top));

    AddPos(G2Vec2(BoxRect.Right, BoxRect.Top));
    AddPos(G2Vec2(BoxRect.Right, BoxRect.Top));

    AddPos(G2Vec2(BoxRect.Right, BoxRect.Bottom));

    AddPos(G2Vec2(BoxRect.Left, BoxRect.Bottom));
    AddPos(G2Vec2(BoxRect.Left, BoxRect.Bottom));

    AddPos(G2Vec2(BoxRect.Left + MarginL, BoxRect.Top + MarginT));
    AddPos(G2Vec2(BoxRect.Right - MarginR, BoxRect.Top + MarginT));
    AddPos(G2Vec2(BoxRect.Right - MarginR, BoxRect.Bottom - MarginB));
    AddPos(G2Vec2(BoxRect.Left + MarginL, BoxRect.Bottom - MarginB));

    AddCol(ColTL);

    AddCol(ColTL);
    AddCol(ColBR);

    AddCol(ColBR);

    AddCol(ColBR);
    AddCol(ColTL);

    AddCol(ColNormal, 4);

    AddFace(0, 1, 6); AddFace(6, 1, 7);
    AddFace(2, 3, 7); AddFace(7, 3, 8);
    AddFace(3, 4, 8); AddFace(8, 4, 9);
    AddFace(5, 0, 9); AddFace(9, 0, 6);

    AddFace(6, 7, 8); AddFace(8, 9, 6);

    DrawEnd;
  end;
end;

procedure TG2GUIWindow.RenderTextBox(
      const BoxRect: TRect;
      const ColExt: DWord = $ff444444;
      const ColInt: DWord = $ffffffff
    );
begin
  with GUI.Render2D, BoxRect do
  begin
    DrawBegin(ptTriangleList);

    AddPos(G2Vec2(Left, Top));
    AddPos(G2Vec2(Right, Top));
    AddPos(G2Vec2(Right, Bottom));
    AddPos(G2Vec2(Left, Bottom));

    AddPos(G2Vec2(Left + MarginLeft, Top + MarginTop));
    AddPos(G2Vec2(Right - MarginRight, Top + MarginTop));
    AddPos(G2Vec2(Right - MarginRight, Bottom - MarginBottom));
    AddPos(G2Vec2(Left + MarginLeft, Bottom - MarginBottom));

    AddCol($ff444444, 4); AddCol($ffffffff, 4);

    AddFace(0, 1, 4); AddFace(4, 1, 5);
    AddFace(5, 1, 2); AddFace(5, 2, 6);
    AddFace(6, 2, 3); AddFace(6, 3, 7);
    AddFace(3, 0, 7); AddFace(7, 0, 4);
    AddFace(4, 5, 6); AddFace(6, 7, 4);

    DrawEnd;
  end;
end;

procedure TG2GUIWindow.RenderFrame(
      const BoxRect: TRect;
      const ColHighlight: DWord = $ffffffff;
      const ColShadow: DWord = $ff555555
    );
begin
  with GUI.Render2D, BoxRect do
  begin
    DrawBegin(ptTriangleStrip);

    AddPos(G2Vec2(Left + MarginLeft, Top + MarginTop));
    AddPos(G2Vec2(Left, Top));

    AddPos(G2Vec2(Right - MarginRight, Top + MarginTop));
    AddPos(G2Vec2(Right, Top));
    AddPos(G2Vec2(Right - MarginRight, Top + MarginTop));
    AddPos(G2Vec2(Right, Top));

    AddPos(G2Vec2(Right - MarginRight, Bottom - MarginBottom));
    AddPos(G2Vec2(Right, Bottom));

    AddPos(G2Vec2(Left + MarginLeft, Bottom - MarginBottom));
    AddPos(G2Vec2(Left, Bottom));
    AddPos(G2Vec2(Left + MarginLeft, Bottom - MarginBottom));
    AddPos(G2Vec2(Left, Bottom));

    AddPos(G2Vec2(Left + MarginLeft, Top + MarginTop));
    AddPos(G2Vec2(Left, Top));

    AddCol(ColShadow); AddCol(ColHighlight);
    AddCol(ColShadow); AddCol(ColHighlight);
    AddCol(ColHighlight); AddCol(ColShadow);
    AddCol(ColHighlight); AddCol(ColShadow);
    AddCol(ColHighlight); AddCol(ColShadow);
    AddCol(ColShadow); AddCol(ColHighlight);
    AddCol(ColShadow); AddCol(ColHighlight);

    DrawEnd;
  end;
end;

procedure TG2GUIWindow.RenderArrowUp(
      const BoxRect: TRect;
      const Margin: Integer = 4;
      const Col: DWord = $ff000000
    );
begin
  GUI.Prim2D.DrawTriangle(
    G2Vec2((BoxRect.Left + BoxRect.Right) * 0.5, BoxRect.Top + Margin),
    G2Vec2(BoxRect.Right - Margin, BoxRect.Bottom - Margin),
    G2Vec2(BoxRect.Left + Margin, BoxRect.Bottom - Margin),
    Col
  );
end;

procedure TG2GUIWindow.RenderArrowDown(
      const BoxRect: TRect;
      const Margin: Integer = 4;
      const Col: DWord = $ff000000
    );
begin
  GUI.Prim2D.DrawTriangle(
    G2Vec2((BoxRect.Left + BoxRect.Right) * 0.5, BoxRect.Bottom - Margin),
    G2Vec2(BoxRect.Left + Margin, BoxRect.Top + Margin),
    G2Vec2(BoxRect.Right - Margin, BoxRect.Top + Margin),
    Col
  );
end;

procedure TG2GUIWindow.RenderArrowLeft(
      const BoxRect: TRect;
      const Margin: Integer = 4;
      const Col: DWord = $ff000000
    );
begin
  GUI.Prim2D.DrawTriangle(
    G2Vec2(BoxRect.Left + Margin, (BoxRect.Top + BoxRect.Bottom) * 0.5),
    G2Vec2(BoxRect.Right - Margin, BoxRect.Top + Margin),
    G2Vec2(BoxRect.Right - Margin, BoxRect.Bottom - Margin),
    Col
  );
end;

procedure TG2GUIWindow.RenderArrowRight(
      const BoxRect: TRect;
      const Margin: Integer = 4;
      const Col: DWord = $ff000000
    );
begin
  GUI.Prim2D.DrawTriangle(
    G2Vec2(BoxRect.Right - Margin, (BoxRect.Top + BoxRect.Bottom) * 0.5),
    G2Vec2(BoxRect.Left + Margin, BoxRect.Bottom - Margin),
    G2Vec2(BoxRect.Left + Margin, BoxRect.Top + Margin),
    Col
  );
end;

function TG2GUIWindow.IsPressed: Boolean;
begin
  Result := m_DownInRect and IsMouseOver and IsMouseDown;
end;

function TG2GUIWindow.IsMouseOver: Boolean;
begin
  Result := PtInRect(
    GetDrawRect,
    m_GUI.PlugInput.MousePos
  );
end;

function TG2GUIWindow.IsMouseDown: Boolean;
begin
  Result := m_GUI.PlugInput.MouseDown[0];
end;

procedure TG2GUIWindow.CustomMouseDown(const Button: Byte);
begin

end;

procedure TG2GUIWindow.CustomMouseUp(const Button: Byte);
begin

end;

procedure TG2GUIWindow.CustomMouseClick;
begin

end;

procedure TG2GUIWindow.CustomMouseMove(const Shift: TPoint);
begin

end;

procedure TG2GUIWindow.CustomMouseWheel(const Shift: Integer);
begin

end;

procedure TG2GUIWindow.CustomKeyDown(const Key: Byte);
begin

end;

procedure TG2GUIWindow.CustomKeyUp(const Key: Byte);
begin

end;

procedure TG2GUIWindow.CustomKeyPress(const Key: AnsiChar);
begin

end;

procedure TG2GUIWindow.CustomMouseEnter;
begin

end;

procedure TG2GUIWindow.CustomMouseLeave;
begin

end;

procedure TG2GUIWindow.CustomFocusReceive;
begin

end;

procedure TG2GUIWindow.CustomFocusLose;
begin

end;

procedure TG2GUIWindow.CustomResize;
begin

end;

procedure TG2GUIWindow.CustomUpdate;
begin

end;

function TG2GUIWindow.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Parent := nil;
  m_Children := TG2List.Create;
  m_X := 0;
  m_Y := 0;
  m_W := 64;
  m_H := 64;
  m_Visible := True;
  m_MarginLeft := 0;
  m_MarginTop := 0;
  m_MarginLeft := 0;
  m_MarginBottom := 0;
  m_DownInRect := False;
  m_MoveInRect := False;
  m_VB0 := Core.Graphics.Shared.RequestVB(
    SizeOf(TVertex0),
    10,
    D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
    FVF0,
    D3DPOOL_DEFAULT
  );
  m_IB := Core.Graphics.Shared.RequestIB(
    30,
    D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
    D3DFMT_INDEX16,
    D3DPOOL_DEFAULT
  );
  Result := grOk;
end;

function TG2GUIWindow.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  if GUI.Focus = Self then
  GUI.Focus := nil;
  m_Children.Free;
  Result := grOk;
end;
//TG2GUIWindow END

//TG2GUIPanel BEGIN
constructor TG2GUIPanel.Create;
begin
  inherited Create;
end;

destructor TG2GUIPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUIPanel.Render;
begin
  RenderBox(GetGlobalRect);
end;

function TG2GUIPanel.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  MarginLeft := 3;
  MarginTop := 3;
  MarginRight := 3;
  MarginBottom := 3;
  Result := grOk;
end;

function TG2GUIPanel.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUIPanel END

//TG2GUIButton BEGIN
constructor TG2GUIButton.Create;
begin
  inherited Create;
end;

destructor TG2GUIButton.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUIButton.Render;
var
  R: TRect;
  fx, fy: Single;
  Lowered: Boolean;
  PrevMinFilter: DWord;
  PrevMagFilter: DWord;
begin
  Lowered := IsPressed;
  RenderBox(GetGlobalRect, Lowered);
  R := ClientDrawRect;
  Core.Graphics.Device.SetScissorRect(@R);
  fx := R.Left + ((R.Right - R.Left) - m_Font.GetTextWidth(m_Caption)) * 0.5;
  fy := R.Top + ((R.Bottom - R.Top) - m_Font.GetTextHeight(m_Caption)) * 0.5;
  if Lowered then fy := fy + 2;
  PrevMinFilter := Core.Graphics.SamplerStates.MinFilter[0];
  PrevMagFilter := Core.Graphics.SamplerStates.MagFilter[0];
  Core.Graphics.SamplerStates.MinFilter[0] := D3DTEXF_POINT;
  Core.Graphics.SamplerStates.MagFilter[0] := D3DTEXF_POINT;
  m_Font.Print(
    fx, fy, FontColor, m_Caption
  );
  Core.Graphics.SamplerStates.MinFilter[0] := PrevMinFilter;
  Core.Graphics.SamplerStates.MagFilter[0] := PrevMagFilter;
end;

function TG2GUIButton.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  MarginLeft := 6;
  MarginTop := 6;
  MarginRight := 6;
  MarginBottom := 6;
  m_Caption := 'Button';
  m_Font := Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  Result := grOk;
end;

function TG2GUIButton.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUIButton END

//TG2GUIEdit BEGIN
constructor TG2GUIEdit.Create;
begin
  inherited Create;
end;

destructor TG2GUIEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUIEdit.CustomMouseClick;
var
  i, fx, w, cx: Integer;
begin
  fx := ClientDrawRect.Left;
  cx := GUI.PlugInput.MousePos.x;
  for i := 1 to Length(m_Text) do
  begin
    w := m_Font.GetTextWidth(m_Text[i]);
    if fx + w * 0.5 >= cx then
    begin
      m_CurPos := i - 1;
      Exit;
    end;
    fx := fx + w;
  end;
  m_CurPos := Length(m_Text);
end;

procedure TG2GUIEdit.CustomKeyDown(const Key: Byte);
var
  s1, s2: AnsiString;
begin
  if (Key = DIK_LEFT)
  and (m_CurPos > 0) then
  begin
    Dec(m_CurPos);
    m_FlickerTime := GetTickCount;
  end;
  if (Key = DIK_RIGHT)
  and (m_CurPos < Length(m_Text)) then
  begin
    Inc(m_CurPos);
    m_FlickerTime := GetTickCount;
  end;
  if (Key = DIK_HOME) then
  m_CurPos := 0;
  if (Key = DIK_END) then
  m_CurPos := Length(m_Text);
  if (Key = DIK_BACK)
  and (m_CurPos > 0) then
  begin
    SetLength(s1, m_CurPos - 1);
    SetLength(s2, Length(m_Text) - m_CurPos);
    Move(m_Text[1], s1[1], m_CurPos - 1);
    Move(m_Text[m_CurPos + 1], s2[1], Length(s2));
    m_Text := s1 + s2;
    Dec(m_CurPos);
    m_FlickerTime := GetTickCount;
  end;
  if (Key = DIK_DELETE)
  and (m_CurPos < Length(m_Text)) then
  begin
    SetLength(s1, m_CurPos);
    SetLength(s2, Length(m_Text) - m_CurPos - 1);
    Move(m_Text[1], s1[1], m_CurPos);
    Move(m_Text[m_CurPos + 2], s2[1], Length(s2));
    m_Text := s1 + s2;
    m_FlickerTime := GetTickCount;
  end;
end;

procedure TG2GUIEdit.CustomKeyPress(const Key: AnsiChar);
var
  s1, s2: AnsiString;
begin
  if not (Ord(Key) < 32) then
  begin
    SetLength(s1, m_CurPos);
    SetLength(s2, Length(m_Text) - m_CurPos);
    Move(m_Text[1], s1[1], m_CurPos);
    Move(m_Text[m_CurPos + 1], s2[1], Length(s2));
    m_Text := s1 + Key + s2;
    Inc(m_CurPos);
  end;
end;

procedure TG2GUIEdit.Render;
var
  i: Integer;
  R: TRect;
  fx, fy: Single;
  PrevMinFilter: DWord;
  PrevMagFilter: DWord;
  c: TG2Color;
begin
  RenderTextBox(GetGlobalRect);
  R := ClientDrawRect;
  Core.Graphics.Device.SetScissorRect(@R);
  fx := R.Left;
  fy := R.Top + ((R.Bottom - R.Top) - m_Font.GetTextHeight(m_Text)) * 0.5;
  PrevMinFilter := Core.Graphics.SamplerStates.MinFilter[0];
  PrevMagFilter := Core.Graphics.SamplerStates.MagFilter[0];
  Core.Graphics.SamplerStates.MinFilter[0] := D3DTEXF_POINT;
  Core.Graphics.SamplerStates.MagFilter[0] := D3DTEXF_POINT;
  m_Font.Print(
    fx, fy, FontColor, m_Text
  );
  if Focused then
  begin
    fx := R.Left;
    for i := 1 to m_CurPos do
    fx := fx + m_Font.GetTextWidth(m_Text[i]);
    c := G2LerpColor($ff000000, $0, Sin(G2PiTime(100, GetTickCount - m_FlickerTime + 200)) * 0.5 + 0.5);
    with GUI do
    begin
      Core.Graphics.Device.SetTexture(0, nil);
      Prim2D.DrawLine(G2Vec2(fx, R.Top), G2Vec2(fx, R.Bottom), c);
    end;
  end;
  Core.Graphics.SamplerStates.MinFilter[0] := PrevMinFilter;
  Core.Graphics.SamplerStates.MagFilter[0] := PrevMagFilter;
end;

function TG2GUIEdit.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  MarginLeft := 5;
  MarginTop := 5;
  MarginRight := 5;
  MarginBottom := 5;
  m_Text := 'Edit';
  m_Font := Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  m_CurPos := 0;
  m_FlickerTime := GetTickCount;
  Result := grOk;
end;

function TG2GUIEdit.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUIEdit END

//TG2GUICheckBox BEGIN
constructor TG2GUICheckBox.Create;
begin
  inherited Create;
end;

destructor TG2GUICheckBox.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUICheckBox.CustomMouseDown(const Button: Byte);
begin
  if Button = 0 then
  begin
    m_Checked := not m_Checked;
    if Assigned(m_OnChanged) then
    m_OnChanged;
  end;
end;

procedure TG2GUICheckBox.Render;
var
  R: TRect;
  c: TPoint;
  fx, fy: Single;
  PrevMinFilter: DWord;
  PrevMagFilter: DWord;
const
  HSize = 8;
begin
  R := GetGlobalRect;
  c := Point(R.Left + HSize, (R.Top + R.Bottom) div 2);
  Core.Graphics.Device.SetTexture(0, nil);
  with GUI.Render2D do
  begin
    DrawBegin(ptTriangleFan);
    AddPos(c);
    AddPos(G2Vec2(c.x - HSize, c.y - HSize));
    AddPos(G2Vec2(c.x + HSize, c.y - HSize));
    AddPos(G2Vec2(c.x + HSize, c.y + HSize));
    AddPos(G2Vec2(c.x - HSize, c.y + HSize));
    AddPos(G2Vec2(c.x - HSize, c.y - HSize));
    AddCol($ffbbbbbb); AddCol($ff444444, 5);
    DrawEnd;
    if m_Checked then
    begin
      DrawBegin(ptTriangleFan);
      AddPos(c);
      AddPos(G2Vec2(c.x - HSize, c.y - HSize));
      AddPos(G2Vec2(c.x + HSize, c.y - HSize));
      AddPos(G2Vec2(c.x + HSize, c.y + HSize));
      AddPos(G2Vec2(c.x - HSize, c.y + HSize));
      AddPos(G2Vec2(c.x - HSize, c.y - HSize));
      AddCol($ffffbb00); AddCol($00ffbb00, 5);
      DrawEnd;
    end;
  end;
  fx := R.Left + HSize * 2 + 4;
  fy := c.Y - m_Font.GetTextHeight(m_Caption) * 0.5;
  PrevMinFilter := Core.Graphics.SamplerStates.MinFilter[0];
  PrevMagFilter := Core.Graphics.SamplerStates.MagFilter[0];
  Core.Graphics.SamplerStates.MinFilter[0] := D3DTEXF_POINT;
  Core.Graphics.SamplerStates.MagFilter[0] := D3DTEXF_POINT;
  m_Font.Print(fx, fy, m_FontColor, m_Caption);
  Core.Graphics.SamplerStates.MinFilter[0] := PrevMinFilter;
  Core.Graphics.SamplerStates.MagFilter[0] := PrevMagFilter;
end;

function TG2GUICheckBox.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  W := 128;
  H := 16;
  m_Caption := 'Check Box';
  m_Font := Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  m_Checked := False;
  Result := grOk;
end;

function TG2GUICheckBox.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUICheckBox END

//TG2GUIRadioButton BEGIN
constructor TG2GUIRadioButton.Create;
begin
  inherited Create;
end;

destructor TG2GUIRadioButton.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUIRadioButton.CustomMouseDown(const Button: Byte);
begin
  if Button = 0 then
  Checked := True;
end;

procedure TG2GUIRadioButton.SetChecked(const Value: Boolean);
var
  i: Integer;
begin
  m_Checked := Value;
  if m_Checked and (m_Group <> 0) and Assigned(Parent) then
  for i := 0 to Parent.Children.Count - 1 do
  if (Parent.Children[i] <> Self)
  and (TG2GUIWindow(Parent.Children[i]) is TG2GUIRadioButton) then
  TG2GUIRadioButton(Parent.Children[i]).Checked := False;
end;

procedure TG2GUIRadioButton.Render;
var
  R: TRect;
  c: TPoint;
  fx, fy: Single;
  PrevMinFilter: DWord;
  PrevMagFilter: DWord;
const
  HSize = 8;
begin
  R := GetGlobalRect;
  c := Point(R.Left + HSize, (R.Top + R.Bottom) div 2);
  Core.Graphics.Device.SetTexture(0, nil);
  GUI.Prim2D.DrawCircle2Col(c, HSize, $ffbbbbbb, $ff444444, 12);
  if m_Checked then
  GUI.Prim2D.DrawCircle2Col(c, HSize, $ffffbb00, $00ffbb00, 12);
  fx := R.Left + HSize * 2 + 4;
  fy := c.Y - m_Font.GetTextHeight(m_Caption) * 0.5;
  PrevMinFilter := Core.Graphics.SamplerStates.MinFilter[0];
  PrevMagFilter := Core.Graphics.SamplerStates.MagFilter[0];
  Core.Graphics.SamplerStates.MinFilter[0] := D3DTEXF_POINT;
  Core.Graphics.SamplerStates.MagFilter[0] := D3DTEXF_POINT;
  m_Font.Print(fx, fy, m_FontColor, m_Caption);
  Core.Graphics.SamplerStates.MinFilter[0] := PrevMinFilter;
  Core.Graphics.SamplerStates.MagFilter[0] := PrevMagFilter;
end;

function TG2GUIRadioButton.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  W := 128;
  H := 16;
  m_Caption := 'Radio Button';
  m_Font := Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  m_Checked := False;
  m_Group := 0;
  Result := grOk;
end;

function TG2GUIRadioButton.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUIRadioButton END

//TG2GUILabel BEGIN
constructor TG2GUILabel.Create;
begin
  inherited Create;
end;

destructor TG2GUILabel.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUILabel.Render;
var
  R: TRect;
  fx, fy: Single;
begin
  if m_AutoSize then
  begin
    W := m_Font.GetTextWidth(m_Caption);
    H := m_Font.GetTextHeight(m_Caption);
  end;
  R := GetGlobalRect;
  fx := R.Left;
  fy := (R.Top + R.Bottom - m_Font.GetTextHeight(m_Caption)) * 0.5;
  m_Font.Print(fx, fy, m_FontColor, m_Caption);
end;

function TG2GUILabel.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  W := 128;
  H := 16;
  m_Caption := 'Label';
  m_Font := Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  m_AutoSize := True;
  Result := grOk;
end;

function TG2GUILabel.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUILabel END

//TG2GUIMemoBox BEGIN
constructor TG2GUIMemoBox.Create;
begin
  inherited Create;
  m_Text := TStringList.Create;
end;

destructor TG2GUIMemoBox.Destroy;
begin
  m_Text.Free;
  inherited Destroy;
end;

procedure TG2GUIMemoBox.LinesChanged(Sender: TObject);
begin
  if m_Text.Count = 0 then
  m_Text.Append('');
  if Assigned(m_ProcOnChange) then
  m_ProcOnChange;
end;

procedure TG2GUIMemoBox.CustomMouseClick;
var
  mc: TPoint;
  R: TRect;
  th, l, i, w, fx: Integer;
begin
  if m_CurPos.Y < 0 then Exit;
  R := ClientDrawRect;
  R.Left := R.Left - m_Scroll.x;
  R.Top := R.Top - m_Scroll.y;
  R.Right := R.Right - m_Scroll.x;
  R.Bottom := R.Bottom - m_Scroll.y;
  mc := GUI.PlugInput.MousePos;
  th := m_Font.GetTextHeight('A');
  l := (mc.Y - R.Top) div th;
  if l < m_Text.Count then
  begin
    fx := R.Left;
    for i := 1 to Length(m_Text[l]) do
    begin
      w := m_Font.GetTextWidth(AnsiString(m_Text[l])[i]);
      if fx + w * 0.5 >= mc.x then
      begin
        m_CurPos := Point(i - 1, l);
        Exit;
      end;
      fx := fx + w;
    end;
    m_CurPos := Point(Length(m_Text[l]), l);
  end
  else
  begin
    l := m_Text.Count - 1;
    m_CurPos := Point(Length(m_Text[l]), l);
  end;
end;

procedure TG2GUIMemoBox.CustomKeyDown(const Key: Byte);
var
  s1, s2: AnsiString;
  fx, cw, w, i: Integer;
begin
  if (Key = DIK_LEFT)
  and (m_CurPos.x > 0) then
  begin
    Dec(m_CurPos.x);
    m_FlickerTime := GetTickCount;
  end;
  if (Key = DIK_RIGHT)
  and (m_CurPos.x < Length(m_Text[m_CurPos.y])) then
  begin
    Inc(m_CurPos.x);
    m_FlickerTime := GetTickCount;
  end;
  if (Key = DIK_UP)
  and (m_CurPos.y > 0) then
  begin
    fx := 0;
    for i := 1 to m_CurPos.x do
    fx := fx + m_Font.GetTextWidth(AnsiString(m_Text[m_CurPos.y])[i]);
    Dec(m_CurPos.y);
    w := 0;
    m_CurPos.x := Length(m_Text[m_CurPos.y]);
    for i := 1 to Length(m_Text[m_CurPos.y]) do
    begin
      cw := m_Font.GetTextWidth(AnsiString(m_Text[m_CurPos.y])[i]);
      if w + cw * 0.5 >= fx then
      begin
        m_CurPos.x := i - 1;
        Break;
      end;
      w := w + cw;
    end;
    if m_CurPos.x > Length(m_Text[m_CurPos.y]) then
    m_CurPos.x := Length(m_Text[m_CurPos.y]);
    m_FlickerTime := GetTickCount;
  end;
  if (Key = DIK_HOME) then
  begin
    m_CurPos.x := 0;
    if Assigned(m_ProcOnChange) then
    m_ProcOnChange;
  end;
  if (Key = DIK_END) then
  begin
    m_CurPos.x := Length(m_Text[m_CurPos.y]);
  end;
  if (Key = DIK_DOWN)
  and (m_CurPos.y < m_Text.Count - 1) then
  begin
    fx := 0;
    for i := 1 to m_CurPos.x do
    fx := fx + m_Font.GetTextWidth(AnsiString(m_Text[m_CurPos.y])[i]);
    Inc(m_CurPos.y);
    w := 0;
    m_CurPos.x := Length(m_Text[m_CurPos.y]);
    for i := 1 to Length(m_Text[m_CurPos.y]) do
    begin
      cw := m_Font.GetTextWidth(AnsiString(m_Text[m_CurPos.y])[i]);
      if w + cw * 0.5 > fx then
      begin
        m_CurPos.x := i - 1;
        Break;
      end;
      w := w + cw;
    end;
    if m_CurPos.x > Length(m_Text[m_CurPos.y]) then
    m_CurPos.x := Length(m_Text[m_CurPos.y]);
    m_FlickerTime := GetTickCount;
  end;
  if (Key = DIK_BACK) then
  begin
    if (m_CurPos.x = 0) and (m_CurPos.y > 0) then
    begin
      s1 := AnsiString(m_Text[m_CurPos.y]);
      m_Text.Delete(m_CurPos.y);
      Dec(m_CurPos.y);
      m_CurPos.x := Length(m_Text[m_CurPos.y]);
      m_Text[m_CurPos.y] := m_Text[m_CurPos.y] + String(s1);
    end
    else if (m_CurPos.x > 0) then
    begin
      SetLength(s1, m_CurPos.x - 1);
      SetLength(s2, Length(m_Text[m_CurPos.y]) - m_CurPos.x);
      s1 := AnsiString(Copy(m_Text[m_CurPos.y], 1, m_CurPos.x - 1));
      s2 := AnsiString(Copy(m_Text[m_CurPos.y], m_CurPos.x + 1, Length(s2)));
      m_Text[m_CurPos.y] := String(s1 + s2);
      Dec(m_CurPos.x);
    end;
    m_FlickerTime := GetTickCount;
  end;
  if (Key = DIK_DELETE) then
  begin
    if (m_CurPos.x < Length(m_Text[m_CurPos.y])) then
    begin
      SetLength(s1, m_CurPos.x);
      SetLength(s2, Length(m_Text[m_CurPos.y]) - m_CurPos.x - 1);
      s1 := AnsiString(Copy(m_Text[m_CurPos.y], 1, m_CurPos.x));
      s2 := AnsiString(Copy(m_Text[m_CurPos.y], m_CurPos.x + 2, Length(s2)));
      m_Text[m_CurPos.y] := String(s1 + s2);
      m_FlickerTime := GetTickCount;
    end
    else if (m_CurPos.y < m_Text.Count - 1) then
    begin
      m_Text[m_CurPos.y] := m_Text[m_CurPos.y] + m_Text[m_CurPos.y + 1];
      m_Text.Delete(m_CurPos.Y + 1);
    end;
  end;
  if (Key = DIK_RETURN) then
  begin
    SetLength(s1, m_CurPos.x);
    SetLength(s2, Length(m_Text[m_CurPos.y]) - m_CurPos.x);
    s1 := AnsiString(Copy(m_Text[m_CurPos.y], 1, m_CurPos.x));
    s2 := AnsiString(Copy(m_Text[m_CurPos.y], m_CurPos.x + 1, Length(s2)));
    m_Text[m_CurPos.y] := String(s1);
    m_Text.Insert(m_CurPos.y + 1, String(s2));
    Inc(m_CurPos.y);
    m_CurPos.x := 0;
    m_FlickerTime := GetTickCount;
  end;
end;

procedure TG2GUIMemoBox.CustomKeyPress(const Key: AnsiChar);
var
  s1, s2: AnsiString;
begin
  if not (Ord(Key) < 32) then
  begin
    SetLength(s1, m_CurPos.x);
    SetLength(s2, Length(m_Text[m_CurPos.y]) - m_CurPos.x);
    s1 := AnsiString(Copy(m_Text[m_CurPos.y], 1, m_CurPos.x));
    s2 := AnsiString(Copy(m_Text[m_CurPos.y], m_CurPos.x + 1, Length(s2)));
    m_Text[m_CurPos.y] := String(s1 + Key + s2);
    Inc(m_CurPos.x);
  end;
end;

procedure TG2GUIMemoBox.Render;
var
  R: TRect;
  th, i: Integer;
  fx, fy: Single;
  c: TG2Color;
begin
  RenderTextBox(GetGlobalRect);
  R := ClientDrawRect;
  Core.Graphics.Device.SetScissorRect(@R);
  R.Left := R.Left - m_Scroll.x;
  R.Top := R.Top - m_Scroll.y;
  R.Right := R.Right - m_Scroll.x;
  R.Bottom := R.Bottom - m_Scroll.y;
  th := m_Font.GetTextHeight('A');
  for i := 0 to m_Text.Count - 1 do
  begin
    fx := R.Left;
    fy := R.Top + th * i;
    m_Font.Print(fx, fy, m_FontColor, AnsiString(m_Text[i]));
  end;
  if Focused then
  begin
    fx := R.Left;
    for i := 1 to m_CurPos.x do
    fx := fx + m_Font.GetTextWidth(AnsiString(m_Text[m_CurPos.y])[i]);
    c := G2LerpColor($ff000000, $0, Sin(G2PiTime(100, GetTickCount - m_FlickerTime + 200)) * 0.5 + 0.5);
    with GUI do
    begin
      Core.Graphics.Device.SetTexture(0, nil);
      Prim2D.DrawLine(G2Vec2(fx, R.Top + m_CurPos.y * th), G2Vec2(fx, R.Top + m_CurPos.y * th + th), c);
    end;
  end;
end;

function TG2GUIMemoBox.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  MarginLeft := 5;
  MarginTop := 5;
  MarginRight := 5;
  MarginBottom := 5;
  W := 256;
  H := 128;
  m_Font := Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  m_CurPos := Point(0, 0);
  m_Text.Text := 'Memo';
  m_Text.OnChange := LinesChanged;
  m_FlickerTime := GetTickCount;
  m_ProcOnChange := nil;
  Result := grOk;
end;

function TG2GUIMemoBox.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUIMemoBox END

//TG2GUStringIList BEGIN
constructor TG2GUIStringListBox.Create;
begin
  inherited Create;
  m_List := TStringList.Create;
  m_List.OnChange := OnListChange;
end;

destructor TG2GUIStringListBox.Destroy;
begin
  m_List.Free;
  inherited Destroy;
end;

procedure TG2GUIStringListBox.OnListChange(Sender: TObject);
begin
  if Assigned(m_OnChange) then
  m_OnChange();
end;

procedure TG2GUIStringListBox.CustomMouseDown(const Button: Byte);
var
  R: TRect;
  i: Integer;
begin
  R := ClientDrawRect;
  if (Button = 0)
  and PtInRect(R, GUI.PlugInput.MousePos) then
  begin
    i := (GUI.PlugInput.MousePos.y + m_Scroll - R.Top) div m_Font.GetTextHeight('A');
    if (i >= 0) then
    begin
      if (i >= m_List.Count) then
      m_ItemIndex := m_List.Count - 1
      else
      m_ItemIndex := i;
    end;
  end;
end;

procedure TG2GUIStringListBox.Render;
var
  R: TRect;
  th, i: Integer;
  fx, fy: Single;
begin
  RenderTextBox(GetGlobalRect);
  R := ClientDrawRect;
  Core.Graphics.Device.SetScissorRect(@R);
  th := m_Font.GetTextHeight('A');
  Core.Graphics.Device.SetTexture(0, nil);
  if (m_ItemIndex >= 0) and (m_ItemIndex < m_List.Count) then
  GUI.Prim2D.DrawRect(R.Left, R.Top + th * m_ItemIndex - m_Scroll, R.Right - R.Left, th, $ffccccff);
  for i := 0 to m_List.Count - 1 do
  begin
    fx := R.Left;
    fy := R.Top + th * i - m_Scroll;
    m_Font.Print(fx, fy, m_FontColor, AnsiString(m_List[i]));
  end;
end;

function TG2GUIStringListBox.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  MarginLeft := 5;
  MarginTop := 5;
  MarginRight := 5;
  MarginBottom := 5;
  W := 256;
  H := 128;
  m_Font := Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  m_ItemIndex := -1;
  m_List.Clear;
  m_List.Add('List');
  Result := grOk;
end;
function TG2GUIStringListBox.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUIStringListBox END

//TG2GUISlider BEGIN
constructor TG2GUISlider.Create;
begin
  inherited Create;
end;

destructor TG2GUISlider.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUISlider.AdjustSlider;
var
  mc: TPoint;
  R: TRect;
  CursorPos, SliderWidth: Integer;
begin
  if not m_Editable then Exit;
  R := ClientDrawRect;
  mc := GUI.PlugInput.MousePos;
  CursorPos := mc.X - R.Left;
  SliderWidth := R.Right - R.Left;
  m_Progress := Min(Max(CursorPos / SliderWidth, 0), 1) * 100;
end;

procedure TG2GUISlider.SetProgress(const Value: Single);
begin
  m_Progress := Min(Max(Value, 0), 100);
end;

procedure TG2GUISlider.CustomMouseMove(const Shift: TPoint);
begin
  if IsPressed then AdjustSlider;
end;

procedure TG2GUISlider.CustomMouseDown(const Button: Byte);
begin
  if Button = 0 then AdjustSlider;
end;

procedure TG2GUISlider.Render;
var
  R: TRect;
  g, w, fx, fy: Single;
  Text: AnsiString;
begin
  RenderFrame(GetGlobalRect);
  R := ClientDrawRect;
  GUI.Core.Graphics.Device.SetScissorRect(@R);
  g := Min(Max(m_Progress / 100, 0), 1);
  w := (R.Right - R.Left) * g;
  with GUI.Render2D do
  begin
    DrawBegin(ptTriangleList);

    AddPos(G2Vec2(R.Left, R.Top));
    AddPos(G2Vec2(R.Left + w - 1, R.Top));
    AddPos(G2Vec2(R.Left, R.Bottom));
    AddPos(G2Vec2(R.Left + w - 1, R.Bottom));

    AddFace(0, 1, 2); AddFace(2, 1, 3);

    AddCol($ff00ff00, 2); AddCol($ff008800, 2);

    BaseVertexIndex := CurPos;

    AddPos(G2Vec2(R.Left + w - 1, R.Top));
    AddPos(G2Vec2(R.Left + w + 1, R.Top));
    AddPos(G2Vec2(R.Left + w - 1, R.Bottom));
    AddPos(G2Vec2(R.Left + w + 1, R.Bottom));

    AddFace(0, 1, 2); AddFace(2, 1, 3);

    AddCol($ffff0000, 2); AddCol($ff880000, 2);

    BaseVertexIndex := CurPos;

    AddPos(G2Vec2(R.Left + w + 1, R.Top));
    AddPos(G2Vec2(R.Right, R.Top));
    AddPos(G2Vec2(R.Left + w + 1, R.Bottom));
    AddPos(G2Vec2(R.Right, R.Bottom));

    AddFace(0, 1, 2); AddFace(2, 1, 3);

    AddCol($ffcccccc, 4);

    DrawEnd;

    BaseVertexIndex := 0;
  end;
  Text := AnsiString(IntToStr(Round(m_Progress)) + '%');
  fx := (R.Right + R.Left - m_Font.GetTextWidth(Text)) * 0.5;
  fy := (R.Bottom + R.Top - m_Font.GetTextHeight(Text)) * 0.5;
  m_Font.Print(fx, fy, m_FontColor, Text);
end;

function TG2GUISlider.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  MarginLeft := 4;
  MarginTop := 4;
  MarginRight := 4;
  MarginBottom := 4;
  W := 192;
  H := 28;
  m_Font := Core.Graphics.Shared.RequestFont('Arial', 12);
  m_FontColor := $ff000000;
  m_Progress := 0;
  m_Editable := True;
  Result := grOk;
end;

function TG2GUISlider.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUISlider END

//TG2GUIScrollBar BEGIN
constructor TG2GUIScrollBar.Create;
begin
  inherited Create;
end;

destructor TG2GUIScrollBar.Destroy;
begin
  inherited Destroy;
end;

function TG2GUIScrollBar.SliderRect: TRect;
var
  R: TRect;
  ScrollSize, SliderSize: Integer;
  Pos, Pos1, Pos2: Single;
begin
  R := GetGlobalRect;
  case m_Orient of
    orVertical:
    begin
      ScrollSize := R.Bottom - R.Top - m_ButtonSize * 2;
      SliderSize := Round(Min(Max(ScrollSize * m_SliderRatio, 8), ScrollSize));
      Pos1 := R.Top + m_ButtonSize;
      Pos2 := Pos1 + ScrollSize - SliderSize;
      Pos := G2LerpFloat(Pos1, Pos2, m_Position);
      Result := Rect(R.Left, Round(Pos), R.Right, Round(Pos + SliderSize));
    end;
    orHorizontal:
    begin
      ScrollSize := R.Right - R.Left - m_ButtonSize * 2;
      SliderSize := Round(Min(Max(ScrollSize * m_SliderRatio, 8), ScrollSize));
      Pos1 := R.Left + m_ButtonSize;
      Pos2 := Pos1 + ScrollSize - SliderSize;
      Pos := G2LerpFloat(Pos1, Pos2, m_Position);
      Result := Rect(Round(Pos), R.Top, Round(Pos + SliderSize), R.Bottom);
    end;
  end;
end;

function TG2GUIScrollBar.BtnUpRect(const GR: PRect): TRect;
begin
  case m_Orient of
    orVertical: Result := Rect(GR^.Left, GR^.Top, GR^.Right, GR^.Top + m_ButtonSize);
    orHorizontal: Result := Rect(GR^.Left, GR^.Top, GR^.Left + m_ButtonSize, GR^.Bottom);
  end;
end;

function TG2GUIScrollBar.BtnDownRect(const GR: PRect): TRect;
begin
  case m_Orient of
    orVertical: Result := Rect(GR^.Left, GR^.Bottom - m_ButtonSize, GR^.Right, GR^.Bottom);
    orHorizontal: Result := Rect(GR^.Right - m_ButtonSize, GR^.Top, GR^.Right, GR^.Bottom);
  end;
end;

procedure TG2GUIScrollBar.SetSlider(const Pos: Integer);
var
  R: TRect;
  ScrollSize, SliderSize: Integer;
begin
  R := GetGlobalRect;
  case m_Orient of
    orVertical:
    begin
      ScrollSize := R.Bottom - R.Top - m_ButtonSize * 2;
      SliderSize := Round(Min(Max(ScrollSize * m_SliderRatio, 8), ScrollSize));
      if SliderSize < ScrollSize then
      Position := Min(Max((Pos - (R.Top + m_ButtonSize)) / (ScrollSize - SliderSize), 0), 1)
      else
      Position := 0;
    end;
    orHorizontal:
    begin
      ScrollSize := R.Right - R.Left - m_ButtonSize * 2;
      SliderSize := Round(Min(Max(ScrollSize * m_SliderRatio, 8), ScrollSize));
      if SliderSize < ScrollSize then
      Position := Min(Max((Pos - (R.Left + m_ButtonSize)) / (ScrollSize - SliderSize), 0), 1)
      else
      Position := 0;
    end;
  end;
end;

procedure TG2GUIScrollBar.SetPosition(const Value: Single);
begin
  m_Position := Min(Max(Value, 0), 1);
  if Assigned(m_ProcOnScroll) then
  m_ProcOnScroll;
end;

procedure TG2GUIScrollBar.CustomMouseMove(const Shift: TPoint);
begin
  if m_Drag then
  begin
    if GUI.PlugInput.MouseDown[0] then
    begin
      case m_Orient of
        orVertical:
        begin
          SetSlider(GUI.PlugInput.MousePos.y - m_DragOffset);
        end;
        orHorizontal:
        begin
          SetSlider(GUI.PlugInput.MousePos.x - m_DragOffset);
        end;
      end;
    end
    else
    begin
      m_Drag := False;
    end;
  end;
end;

procedure TG2GUIScrollBar.CustomMouseDown(const Button: Byte);
var
  GR, R, Rhi, Rlo: TRect;
  mc: TPoint;
begin
  R := SliderRect;
  mc := GUI.PlugInput.MousePos;
  if PtInRect(R, mc) then
  begin
    m_Drag := True;
    case m_Orient of
      orVertical: m_DragOffset := mc.Y - R.Top;
      orHorizontal: m_DragOffset := mc.X - R.Left;
    end;
  end
  else
  begin
    GR := GetGlobalRect;
    Rhi := BtnUpRect(@GR);
    Rlo := BtnDownRect(@GR);
    if PtInRect(Rhi, mc) then
    m_ScrollUp := True
    else if PtInRect(Rlo, mc) then
    m_ScrollDown := True
    else
    begin
      case m_Orient of
        orVertical:
        begin
          Rhi := Rect(GR.Left, GR.Top + m_ButtonSize, GR.Right, R.Top);
          Rlo := Rect(GR.Left, R.Bottom, GR.Right, GR.Bottom - m_ButtonSize);
        end;
        orHorizontal:
        begin
          Rhi := Rect(GR.Left + m_ButtonSize, GR.Top, R.Left, GR.Bottom);
          Rlo := Rect(R.Right, GR.Top, GR.Right - m_ButtonSize, GR.Bottom);
        end;
      end;
      if PtInRect(Rhi, mc) then
      begin
        Position := Min(Max(m_Position - m_SliderRatio, 0), 1);
      end
      else if PtInRect(Rlo, mc) then
      begin
        Position := Min(Max(m_Position + m_SliderRatio, 0), 1);
      end;
    end;
  end;
end;

procedure TG2GUIScrollBar.CustomMouseUp(const Button: Byte);
begin
  m_Drag := False;
end;

procedure TG2GUIScrollBar.CustomMouseWheel(const Shift: Integer);
begin
  Scroll(Shift);
end;

procedure TG2GUIScrollBar.CustomUpdate;
var
  R, Rhi, Rlo: TRect;
  mc: TPoint;
begin
  if not GUI.PlugInput.MouseDown[0] then
  begin
    m_ScrollUp := False;
    m_ScrollDown := False;
    m_Drag := False;
  end
  else
  begin
    R := GetGlobalRect;
    mc := GUI.PlugInput.MousePos;
    Rhi := BtnUpRect(@R);
    Rlo := BtnDownRect(@R);
    if m_ScrollUp
    and PtInRect(Rhi, mc) then
    Position := Min(Max(m_Position - m_ScrollSpeed * m_SliderRatio, 0), 1)
    else if m_ScrollDown
    and PtInRect(Rlo, mc) then
    Position := Min(Max(m_Position + m_ScrollSpeed * m_SliderRatio, 0), 1);
  end;
end;

procedure TG2GUIScrollBar.Render;
var
  R, btnr, Rhi, Rlo: TRect;
  mc: TPoint;
  Lowered: Boolean;
begin
  R := GetGlobalRect;
  mc := GUI.PlugInput.MousePos;
  Rhi := BtnUpRect(@R);
  Rlo := BtnDownRect(@R);
  case m_Orient of
    orVertical:
    begin
      GUI.Prim2D.DrawRect(
        R.Left,
        R.Top + m_ButtonSize,
        R.Right - R.Left,
        R.Bottom - R.Top - m_ButtonSize * 2,
        $ffaaaaaa
      );
      btnr := Rect(R.Left, R.Top, R.Right, R.Top + m_ButtonSize);
      Lowered := m_ScrollUp and PtInRect(Rhi, mc);
      RenderBox(btnr, 2, 2, 2, 2, Lowered);
      RenderArrowUp(btnr);
      btnr := Rect(R.Left, R.Bottom - m_ButtonSize, R.Right, R.Bottom);
      Lowered := m_ScrollDown and PtInRect(Rlo, mc);
      RenderBox(btnr, 2, 2, 2, 2, Lowered);
      RenderArrowDown(btnr);
      btnr := SliderRect;
      RenderBox(btnr, 2, 2, 2, 2);
    end;
    orHorizontal:
    begin
      GUI.Prim2D.DrawRect(
        R.Left + m_ButtonSize,
        R.Top,
        R.Right - R.Left - m_ButtonSize * 2,
        R.Bottom - R.Top,
        $ffaaaaaa
      );
      btnr := Rect(R.Left, R.Top, R.Left + m_ButtonSize, R.Bottom);
      Lowered := m_ScrollUp and PtInRect(Rhi, mc);
      RenderBox(btnr, 2, 2, 2, 2, Lowered);
      RenderArrowLeft(btnr);
      btnr := Rect(R.Right - m_ButtonSize, R.Top, R.Right, R.Bottom);
      Lowered := m_ScrollDown and PtInRect(Rlo, mc);
      RenderBox(btnr, 2, 2, 2, 2, Lowered);
      RenderArrowRight(btnr);
      btnr := SliderRect;
      RenderBox(btnr, 2, 2, 2, 2);
    end;
  end;
end;

function TG2GUIScrollBar.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  MarginLeft := 0;
  MarginTop := 0;
  MarginRight := 0;
  MarginBottom := 0;
  W := 16;
  H := 16;
  m_ButtonSize := 16;
  m_Orient := orVertical;
  m_Position := 0;
  m_SliderRatio := 0.2;
  m_Drag := False;
  m_DragOffset := 0;
  m_ScrollSpeed := 0.01;
  m_ScrollUp := False;
  m_ScrollDown := False;
  Result := grOk;
end;

function TG2GUIScrollBar.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;

procedure TG2GUIScrollBar.Scroll(const Amount: Integer);
begin
  Position := Position - Amount * 0.1 * m_ScrollSpeed;
end;
//TG2GUIScrollBar END

//TG2GUIMemo BEGIN
constructor TG2GUIMemo.Create;
begin
  inherited Create;
end;

destructor TG2GUIMemo.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUIMemo.AdjustScrollBars;
var
  R, MR: TRect;
  TextSize: TPoint;
  th, i: Integer;
  cr: TPoint;
begin
  R := ClientRect;
  MR := Rect(
    R.Left + m_MemoBox.MarginLeft,
    R.Top + m_MemoBox.MarginTop,
    R.Right - m_MemoBox.MarginRight,
    R.Bottom - m_MemoBox.MarginBottom
  );
  if m_ScrollV.Visible then
  MR.Right := MR.Right - 16;
  if m_ScrollH.Visible then
  MR.Bottom := MR.Bottom - 16;
  th := m_MemoBox.Font.GetTextHeight('A');
  TextSize.x := 1;
  for i := 0 to m_MemoBox.Text.Count - 1 do
  TextSize.x := Max(TextSize.x, m_MemoBox.Font.GetTextWidth(AnsiString(m_MemoBox.Text[i])) + 1);
  TextSize.y := m_MemoBox.Text.Count * th;
  m_ScrollH.Visible := TextSize.x > MR.Right - MR.Left;
  m_ScrollV.Visible := TextSize.y > MR.Bottom - MR.Top;
  m_MemoBox.X := 0;
  m_MemoBox.Y := 0;
  m_ScrollV.X := R.Right - 16;
  m_ScrollV.Y := 0;
  m_ScrollV.W := 16;
  m_ScrollH.X := 0;
  m_ScrollH.Y := R.Bottom - 16;
  m_ScrollH.H := 16;
  if m_ScrollH.Visible then
  begin
    m_ScrollH.SliderRatio := (MR.Right - MR.Left) / TextSize.x;
    m_MemoBox.H := R.Bottom - R.Top - 16;
    m_ScrollV.H := R.Bottom - R.Top - 16;
  end
  else
  begin
    m_ScrollH.Position := 0;
    m_MemoBox.H := R.Bottom - R.Top;
    m_ScrollV.H := R.Bottom - R.Top;
  end;
  if m_ScrollV.Visible then
  begin
    m_ScrollV.SliderRatio := (MR.Bottom - MR.Top) / TextSize.y;
    m_MemoBox.W := R.Right - R.Left - 16;
    m_ScrollH.W := R.Right - R.Left - 16;
  end
  else
  begin
    m_ScrollV.Position := 0;
    m_MemoBox.W := R.Right - R.Left;
    m_ScrollH.W := R.Right - R.Left;
  end;
  with m_MemoBox do
  begin
    cr.x := 0;
    for i := 1 to CurPos.x do
    cr.x := cr.x + Font.GetTextWidth(AnsiString(Text[CurPos.y])[i]);
    cr.y := th * CurPos.y;
  end;
  if (cr.x - m_MemoBox.Scroll.x < 0)
  or (cr.x - m_MemoBox.Scroll.x >= (MR.Right - MR.Left) - 1)  then
  begin
    if (cr.x - m_MemoBox.Scroll.x < 0) then
    cr.x := cr.x - Round((MR.Right - MR.Left) * 0.5)
    else
    cr.x := cr.x + Round((MR.Right - MR.Left) * 0.5);
    if TextSize.x > (MR.Right - MR.Left) then
    m_ScrollH.Position := cr.x / (TextSize.x - (MR.Right - MR.Left));
  end;
  if (cr.y - m_MemoBox.Scroll.y < 0)
  or (cr.y + th - m_MemoBox.Scroll.y >= (MR.Bottom - MR.Top) - 1) then
  begin
    if (cr.y - m_MemoBox.Scroll.y < 0) then
    cr.y := cr.y - Round((MR.Bottom - MR.Top) * 0.5)
    else
    cr.y := cr.y + Round((MR.Bottom - MR.Top) * 0.5);
    if TextSize.y > (MR.Bottom - MR.Top) then
    m_ScrollV.Position := cr.y / (TextSize.y - (MR.Bottom - MR.Top));
  end;
end;

procedure TG2GUIMemo.AdjustMemoBox;
var
  MR: TRect;
  TextSize, Scr: TPoint;
  i: Integer;
begin
  MR := m_MemoBox.ClientRect;
  TextSize.x := 1;
  for i := 0 to m_MemoBox.Text.Count - 1 do
  TextSize.x := Max(TextSize.x, m_MemoBox.Font.GetTextWidth(AnsiString(m_MemoBox.Text[i])) + 1);
  TextSize.y := m_MemoBox.Text.Count * m_MemoBox.Font.GetTextHeight('A');
  Scr := m_MemoBox.Scroll;
  if m_ScrollH.Visible then
  begin
    Scr.x := Round((TextSize.x - (MR.Right - MR.Left)) * m_ScrollH.Position);
  end;
  if m_ScrollV.Visible then
  begin
    Scr.y := Round((TextSize.y - (MR.Bottom - MR.Top)) * m_ScrollV.Position);
  end;
  m_MemoBox.Scroll := Scr;
end;

procedure TG2GUIMemo.PropMouseWheel(const Shift: Integer);
begin
  if m_ScrollV.Visible then
  m_ScrollV.Position := m_ScrollV.Position - Shift * 0.1 * m_ScrollV.ScrollSpeed
  else if m_ScrollH.Visible then
  m_ScrollH.Position := m_ScrollH.Position - Shift * 0.1 * m_ScrollH.ScrollSpeed;
end;

procedure TG2GUIMemo.CustomResize;
begin
  AdjustScrollBars;
end;

procedure TG2GUIMemo.Render;
var
  R: TRect;
begin
  if m_ScrollV.Visible
  and m_ScrollH.Visible then
  begin
    R := ClientDrawRect;
    GUI.Prim2D.DrawRect(R.Right - 16, R.Bottom - 16, 16, 16, $ffcccccc);
  end;
end;

procedure TG2GUIMemo.CustomMouseWheel(const Shift: Integer);
begin
  if m_ScrollV.Visible then
  m_ScrollV.Scroll(Shift)
  else if m_ScrollH.Visible then
  m_ScrollH.Scroll(Shift);
end;

function TG2GUIMemo.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  GUI.WindowCreate(TG2GUIMemoBox, Self, m_MemoBox);
  GUI.WindowCreate(TG2GUIScrollBar, Self, m_ScrollV);
  GUI.WindowCreate(TG2GUIScrollBar, Self, m_ScrollH);
  MarginLeft := 0;
  MarginTop := 0;
  MarginRight := 0;
  MarginBottom := 0;
  W := 192;
  H := 192;
  m_ScrollV.Orient := orVertical;
  m_ScrollH.Orient := orHorizontal;
  m_MemoBox.OnChange := AdjustScrollBars;
  m_MemoBox.OnWheelMove := PropMouseWheel;
  m_ScrollV.OnScroll := AdjustMemoBox;
  m_ScrollH.OnScroll := AdjustMemoBox;
  AdjustScrollBars;
  Result := grOk;
end;

function TG2GUIMemo.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUIMemo END

//TG2GUIStringList BEGIN
constructor TG2GUIStringList.Create;
begin
  inherited Create;
end;

destructor TG2GUIStringList.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GUIStringList.AdjustScrollBar;
var
  R, MR: TRect;
  TextSize: TPoint;
  th, i: Integer;
begin
  R := ClientRect;
  MR := Rect(
    R.Left + m_StrListBox.MarginLeft,
    R.Top + m_StrListBox.MarginTop,
    R.Right - m_StrListBox.MarginRight,
    R.Bottom - m_StrListBox.MarginBottom
  );
  if m_ScrollV.Visible then
  MR.Right := MR.Right - 16;
  th := m_StrListBox.Font.GetTextHeight('A');
  TextSize.x := 1;
  for i := 0 to m_StrListBox.List.Count - 1 do
  TextSize.x := Max(TextSize.x, m_StrListBox.Font.GetTextWidth(AnsiString(m_StrListBox.List[i])) + 1);
  TextSize.y := m_StrListBox.List.Count * th;
  m_ScrollV.Visible := TextSize.y > MR.Bottom - MR.Top;
  m_StrListBox.X := 0;
  m_StrListBox.Y := 0;
  m_StrListBox.H := R.Bottom - R.Top;
  m_ScrollV.X := R.Right - 16;
  m_ScrollV.Y := 0;
  m_ScrollV.W := 16;
  m_ScrollV.H := R.Bottom - R.Top;
  if m_ScrollV.Visible then
  begin
    m_ScrollV.SliderRatio := (MR.Bottom - MR.Top) / TextSize.y;
    m_StrListBox.W := R.Right - R.Left - 16;
  end
  else
  begin
    m_ScrollV.Position := 0;
    m_StrListBox.W := R.Right - R.Left;
  end;
end;

procedure TG2GUIStringList.AdjustListBox;
var
  MR: TRect;
  TextSize: TPoint;
  Scr, i: Integer;
begin
  MR := m_StrListBox.ClientRect;
  TextSize.x := 1;
  for i := 0 to m_StrListBox.List.Count - 1 do
  TextSize.x := Max(TextSize.x, m_StrListBox.Font.GetTextWidth(AnsiString(m_StrListBox.List[i])) + 1);
  TextSize.y := m_StrListBox.List.Count * m_StrListBox.Font.GetTextHeight('A');
  Scr := m_StrListBox.Scroll;
  if m_ScrollV.Visible then
  begin
    Scr := Round((TextSize.y - (MR.Bottom - MR.Top)) * m_ScrollV.Position);
  end;
  m_StrListBox.Scroll := Scr;
end;

procedure TG2GUIStringList.PropMouseWheel(const Shift: Integer);
begin
  if m_ScrollV.Visible then
  m_ScrollV.Position := m_ScrollV.Position - Shift * 0.1 * m_ScrollV.ScrollSpeed;
end;

function TG2GUIStringList.GetList: TStringList;
begin
  Result := m_StrListBox.List;
end;

procedure TG2GUIStringList.SetItemIndex(const Value: Integer);
begin
  m_StrListBox.ItemIndex := Value;
end;

function TG2GUIStringList.GetItemIndex: Integer;
begin
  Result := m_StrListBox.ItemIndex;
end;

procedure TG2GUIStringList.CustomResize;
begin
  AdjustScrollBar;
end;

function TG2GUIStringList.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  GUI.WindowCreate(TG2GUIStringListBox, Self, m_StrListBox);
  GUI.WindowCreate(TG2GUIScrollBar, Self, m_ScrollV);
  MarginLeft := 0;
  MarginTop := 0;
  MarginRight := 0;
  MarginBottom := 0;
  W := 192;
  H := 192;
  m_ScrollV.Orient := orVertical;
  m_StrListBox.OnWheelMove := PropMouseWheel;
  m_StrListBox.OnChange := AdjustScrollBar;
  m_ScrollV.OnScroll := AdjustListBox;
  AdjustScrollBar;
  Result := grOk;
end;

function TG2GUIStringList.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2GUIStringList END
*)
//TG2RenderStates BEGIN
constructor TG2RenderStates.Create;
begin
  inherited Create;
end;

destructor TG2RenderStates.Destroy;
begin
  inherited Destroy;
end;

function TG2RenderStates.GetBoolean(const StateType: TD3DRenderStateType): Boolean;
var
  Value: DWord;
begin
  m_Gfx.Device.GetRenderState(StateType, Value);
  Result := Value > 0;
end;

procedure TG2RenderStates.SetBoolean(const StateType: TD3DRenderStateType; const Value: Boolean);
begin
  m_States[Byte(StateType)] := Byte(Value);
  m_Gfx.Device.SetRenderState(StateType, Byte(Value));
end;

function TG2RenderStates.GetDWord(const StateType: TD3DRenderStateType): DWord;
begin
  m_Gfx.Device.GetRenderState(StateType, Result);
end;

procedure TG2RenderStates.SetDWord(const StateType: TD3DRenderStateType; const Value: DWord);
begin
  m_States[Byte(StateType)] := Value;
  m_Gfx.Device.SetRenderState(StateType, Value);
end;

function TG2RenderStates.GetSingle(const StateType: TD3DRenderStateType): Single;
var
  Value: DWord;
begin
  m_Gfx.Device.GetRenderState(StateType, Value);
  Result := PSingle(@Value)^;
end;

procedure TG2RenderStates.SetSingle(const StateType: TD3DRenderStateType; const Value: Single);
begin
  m_States[Byte(StateType)] := PDWord(@Value)^;
  m_Gfx.Device.SetRenderState(StateType, PDWord(@Value)^);
end;

function TG2RenderStates.GetZEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_ZENABLE);
end;

procedure TG2RenderStates.SetZEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_ZENABLE, Value);
end;

function TG2RenderStates.GetFillMode: DWord;
begin
  Result := GetDWord(D3DRS_FILLMODE);
end;

procedure TG2RenderStates.SetFillMode(const Value: DWord);
begin
  SetDWord(D3DRS_FILLMODE, Value);
end;

function TG2RenderStates.GetShadeMode: DWord;
begin
  Result := GetDWord(D3DRS_SHADEMODE);
end;

procedure TG2RenderStates.SetShadeMode(const Value: DWord);
begin
  SetDWord(D3DRS_SHADEMODE, Value);
end;

function TG2RenderStates.GetZWriteEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_ZWRITEENABLE);
end;

procedure TG2RenderStates.SetZWriteEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_ZWRITEENABLE, Value);
end;

function TG2RenderStates.GetAlphaTestEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_ALPHATESTENABLE);
end;

procedure TG2RenderStates.SetAlphaTestEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_ALPHATESTENABLE, Value);
end;

function TG2RenderStates.GetLastPixel: Boolean;
begin
  Result := GetBoolean(D3DRS_LASTPIXEL);
end;

procedure TG2RenderStates.SetLastPixel(const Value: Boolean);
begin
  SetBoolean(D3DRS_LASTPIXEL, Value);
end;

function TG2RenderStates.GetSrcBlend: DWord;
begin
  Result := GetDWord(D3DRS_SRCBLEND);
end;

procedure TG2RenderStates.SetSrcBlend(const Value: DWord);
begin
  SetDWord(D3DRS_SRCBLEND, Value);
end;

function TG2RenderStates.GetDestBlend: DWord;
begin
  Result := GetDWord(D3DRS_DESTBLEND);
end;

procedure TG2RenderStates.SetDestBlend(const Value: DWord);
begin
  SetDWord(D3DRS_DESTBLEND, Value);
end;

function TG2RenderStates.GetCullMode: DWord;
begin
  Result := GetDWord(D3DRS_CULLMODE);
end;

procedure TG2RenderStates.SetCullMode(const Value: DWord);
begin
  SetDWord(D3DRS_CULLMODE, Value);
end;

function TG2RenderStates.GetZFunc: DWord;
begin
  Result := GetDWord(D3DRS_ZFUNC);
end;

procedure TG2RenderStates.SetZFunc(const Value: DWord);
begin
  SetDWord(D3DRS_ZFUNC, Value);
end;

function TG2RenderStates.GetAlphaRef: DWord;
begin
  Result := GetDWord(D3DRS_ALPHAREF);
end;

procedure TG2RenderStates.SetAlphaRef(const Value: DWord);
begin
  SetDWord(D3DRS_ALPHAREF, Value);
end;

function TG2RenderStates.GetAlphaFunc: DWord;
begin
  Result := GetDWord(D3DRS_ALPHAFUNC);
end;

procedure TG2RenderStates.SetAlphaFunc(const Value: DWord);
begin
  SetDWord(D3DRS_ALPHAFUNC, Value);
end;

function TG2RenderStates.GetDitherEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_DITHERENABLE);
end;

procedure TG2RenderStates.SetDitherEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_DITHERENABLE, Value);
end;

function TG2RenderStates.GetAlphaBlendEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_ALPHABLENDENABLE);
end;

procedure TG2RenderStates.SetAlphaBlendEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_ALPHABLENDENABLE, Value);
end;

function TG2RenderStates.GetFogEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_FOGENABLE);
end;

procedure TG2RenderStates.SetFogEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_FOGENABLE, Value);
end;

function TG2RenderStates.GetSpecularEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_SPECULARENABLE);
end;

procedure TG2RenderStates.SetSpecularEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_SPECULARENABLE, Value);
end;

function TG2RenderStates.GetFogColor: DWord;
begin
  Result := GetDWord(D3DRS_FOGCOLOR);
end;

procedure TG2RenderStates.SetFogColor(const Value: DWord);
begin
  SetDWord(D3DRS_FOGCOLOR, Value);
end;

function TG2RenderStates.GetFogTableMode: DWord;
begin
  Result := GetDWord(D3DRS_FOGTABLEMODE);
end;

procedure TG2RenderStates.SetFogTableMode(const Value: DWord);
begin
  SetDWord(D3DRS_FOGTABLEMODE, Value);
end;

function TG2RenderStates.GetFogVertexMode: DWord;
begin
  Result := GetDWord(D3DRS_FOGVERTEXMODE);
end;

procedure TG2RenderStates.SetFogVertexMode(const Value: DWord);
begin
  SetDWord(D3DRS_FOGVERTEXMODE, Value);
end;

function TG2RenderStates.GetFogStart: Single;
begin
  Result := GetSingle(D3DRS_FOGSTART);
end;

procedure TG2RenderStates.SetFogStart(const Value: Single);
begin
  SetSingle(D3DRS_FOGSTART, Value);
end;

function TG2RenderStates.GetFogEnd: Single;
begin
  Result := GetSingle(D3DRS_FOGEND);
end;

procedure TG2RenderStates.SetFogEnd(const Value: Single);
begin
  SetSingle(D3DRS_FOGEND, Value);
end;

function TG2RenderStates.GetFogDensity: Single;
begin
  Result := GetSingle(D3DRS_FOGDENSITY);
end;

procedure TG2RenderStates.SetFogDensity(const Value: Single);
begin
  SetSingle(D3DRS_FOGDENSITY, Value);
end;

function TG2RenderStates.GetRangeFogEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_RANGEFOGENABLE);
end;

procedure TG2RenderStates.SetRangeFogEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_RANGEFOGENABLE, Value);
end;

function TG2RenderStates.GetStencilEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_STENCILENABLE);
end;

procedure TG2RenderStates.SetStencilEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_STENCILENABLE, Value);
end;

function TG2RenderStates.GetStencilFail: DWord;
begin
  Result := GetDWord(D3DRS_STENCILFAIL);
end;

procedure TG2RenderStates.SetStencilFail(const Value: DWord);
begin
  SetDWord(D3DRS_STENCILFAIL, Value);
end;

function TG2RenderStates.GetStencilZFail: DWord;
begin
  Result := GetDWord(D3DRS_STENCILZFAIL);
end;

procedure TG2RenderStates.SetStencilZFail(const Value: DWord);
begin
  SetDWord(D3DRS_STENCILZFAIL, Value);
end;

function TG2RenderStates.GetStencilPass: DWord;
begin
  Result := GetDWord(D3DRS_STENCILPASS);
end;

procedure TG2RenderStates.SetStencilPass(const Value: DWord);
begin
  SetDWord(D3DRS_STENCILPASS, Value);
end;

function TG2RenderStates.GetStencilFunc: DWord;
begin
  Result := GetDWord(D3DRS_STENCILFUNC);
end;

procedure TG2RenderStates.SetStencilFunc(const Value: DWord);
begin
  SetDWord(D3DRS_STENCILFUNC, Value);
end;

function TG2RenderStates.GetStencilRef: DWord;
begin
  Result := GetDWord(D3DRS_STENCILREF);
end;

procedure TG2RenderStates.SetStencilRef(const Value: DWord);
begin
  SetDWord(D3DRS_STENCILREF, Value);
end;

function TG2RenderStates.GetStencilMask: DWord;
begin
  Result := GetDWord(D3DRS_STENCILMASK);
end;

procedure TG2RenderStates.SetStencilMask(const Value: DWord);
begin
  SetDWord(D3DRS_STENCILMASK, Value);
end;

function TG2RenderStates.GetStencilWriteMask: DWord;
begin
  Result := GetDWord(D3DRS_STENCILWRITEMASK);
end;

procedure TG2RenderStates.SetStencilWriteMask(const Value: DWord);
begin
  SetDWord(D3DRS_STENCILWRITEMASK, Value);
end;

function TG2RenderStates.GetTwoSidedStencilMode: Boolean;
begin
  Result := GetBoolean(D3DRS_TWOSIDEDSTENCILMODE);
end;

procedure TG2RenderStates.SetTwoSidedStencilMode(const Value: Boolean);
begin
  SetBoolean(D3DRS_TWOSIDEDSTENCILMODE, Value);
end;

function TG2RenderStates.GetCCWStencilFail: DWord;
begin
  Result := GetDWord(D3DRS_CCW_STENCILFAIL);
end;

procedure TG2RenderStates.SetCCWStencilFail(const Value: DWord);
begin
  SetDWord(D3DRS_CCW_STENCILFAIL, Value);
end;

function TG2RenderStates.GetCCWStencilZFail: DWord;
begin
  Result := GetDWord(D3DRS_CCW_STENCILZFAIL);
end;

procedure TG2RenderStates.SetCCWStencilZFail(const Value: DWord);
begin
  SetDWord(D3DRS_CCW_STENCILZFAIL, Value);
end;

function TG2RenderStates.GetCCWStencilPass: DWord;
begin
  Result := GetDWord(D3DRS_CCW_STENCILPASS);
end;

procedure TG2RenderStates.SetCCWStencilPass(const Value: DWord);
begin
  SetDWord(D3DRS_CCW_STENCILPASS, Value);
end;

function TG2RenderStates.GetCCWStencilFunc: DWord;
begin
  Result := GetDWord(D3DRS_CCW_STENCILFUNC);
end;

procedure TG2RenderStates.SetCCWStencilFunc(const Value: DWord);
begin
  SetDWord(D3DRS_CCW_STENCILFUNC, Value);
end;

function TG2RenderStates.GetTextureFactor: DWord;
begin
  Result := GetDWord(D3DRS_TEXTUREFACTOR);
end;

procedure TG2RenderStates.SetTextureFactor(const Value: DWord);
begin
  SetDWord(D3DRS_TEXTUREFACTOR, Value);
end;

function TG2RenderStates.GetWrap0: DWord;
begin
  Result := GetDWord(D3DRS_WRAP0);
end;

procedure TG2RenderStates.SetWrap0(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP0, Value);
end;

function TG2RenderStates.GetWrap1: DWord;
begin
  Result := GetDWord(D3DRS_WRAP1);
end;

procedure TG2RenderStates.SetWrap1(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP1, Value);
end;

function TG2RenderStates.GetWrap2: DWord;
begin
  Result := GetDWord(D3DRS_WRAP2);
end;

procedure TG2RenderStates.SetWrap2(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP2, Value);
end;

function TG2RenderStates.GetWrap3: DWord;
begin
  Result := GetDWord(D3DRS_WRAP3);
end;

procedure TG2RenderStates.SetWrap3(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP3, Value);
end;

function TG2RenderStates.GetWrap4: DWord;
begin
  Result := GetDWord(D3DRS_WRAP4);
end;

procedure TG2RenderStates.SetWrap4(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP4, Value);
end;

function TG2RenderStates.GetWrap5: DWord;
begin
  Result := GetDWord(D3DRS_WRAP5);
end;

procedure TG2RenderStates.SetWrap5(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP5, Value);
end;

function TG2RenderStates.GetWrap6: DWord;
begin
  Result := GetDWord(D3DRS_WRAP6);
end;

procedure TG2RenderStates.SetWrap6(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP6, Value);
end;

function TG2RenderStates.GetWrap7: DWord;
begin
  Result := GetDWord(D3DRS_WRAP7);
end;

procedure TG2RenderStates.SetWrap7(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP7, Value);
end;

function TG2RenderStates.GetWrap8: DWord;
begin
  Result := GetDWord(D3DRS_WRAP8);
end;

procedure TG2RenderStates.SetWrap8(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP8, Value);
end;

function TG2RenderStates.GetWrap9: DWord;
begin
  Result := GetDWord(D3DRS_WRAP9);
end;

procedure TG2RenderStates.SetWrap9(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP9, Value);
end;

function TG2RenderStates.GetWrap10: DWord;
begin
  Result := GetDWord(D3DRS_WRAP10);
end;

procedure TG2RenderStates.SetWrap10(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP10, Value);
end;

function TG2RenderStates.GetWrap11: DWord;
begin
  Result := GetDWord(D3DRS_WRAP11);
end;

procedure TG2RenderStates.SetWrap11(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP11, Value);
end;

function TG2RenderStates.GetWrap12: DWord;
begin
  Result := GetDWord(D3DRS_WRAP12);
end;

procedure TG2RenderStates.SetWrap12(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP12, Value);
end;

function TG2RenderStates.GetWrap13: DWord;
begin
  Result := GetDWord(D3DRS_WRAP13);
end;

procedure TG2RenderStates.SetWrap13(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP13, Value);
end;

function TG2RenderStates.GetWrap14: DWord;
begin
  Result := GetDWord(D3DRS_WRAP14);
end;

procedure TG2RenderStates.SetWrap14(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP14, Value);
end;

function TG2RenderStates.GetWrap15: DWord;
begin
  Result := GetDWord(D3DRS_WRAP15);
end;

procedure TG2RenderStates.SetWrap15(const Value: DWord);
begin
  SetDWord(D3DRS_WRAP15, Value);
end;

function TG2RenderStates.GetClipping: Boolean;
begin
  Result := GetBoolean(D3DRS_CLIPPING);
end;

procedure TG2RenderStates.SetClipping(const Value: Boolean);
begin
  SetBoolean(D3DRS_CLIPPING, Value);
end;

function TG2RenderStates.GetLighting: Boolean;
begin
  Result := GetBoolean(D3DRS_LIGHTING);
end;

procedure TG2RenderStates.SetLighting(const Value: Boolean);
begin
  SetBoolean(D3DRS_LIGHTING, Value);
end;

function TG2RenderStates.GetAmbient: DWord;
begin
  Result := GetDWord(D3DRS_AMBIENT);
end;

procedure TG2RenderStates.SetAmbient(const Value: DWord);
begin
  SetDWord(D3DRS_AMBIENT, Value);
end;

function TG2RenderStates.GetColorVertex: Boolean;
begin
  Result := GetBoolean(D3DRS_COLORVERTEX);
end;

procedure TG2RenderStates.SetColorVertex(const Value: Boolean);
begin
  SetBoolean(D3DRS_COLORVERTEX, Value);
end;

function TG2RenderStates.GetLocalViewer: Boolean;
begin
  Result := GetBoolean(D3DRS_LOCALVIEWER);
end;

procedure TG2RenderStates.SetLocalViewer(const Value: Boolean);
begin
  SetBoolean(D3DRS_LOCALVIEWER, Value);
end;

function TG2RenderStates.GetNormalizeNormals: Boolean;
begin
  Result := GetBoolean(D3DRS_NORMALIZENORMALS);
end;

procedure TG2RenderStates.SetNormalizeNormals(const Value: Boolean);
begin
  SetBoolean(D3DRS_NORMALIZENORMALS, Value);
end;

function TG2RenderStates.GetDiffuseMaterialSource: DWord;
begin
  Result := GetDWord(D3DRS_DIFFUSEMATERIALSOURCE);
end;

procedure TG2RenderStates.SetDiffuseMaterialSource(const Value: DWord);
begin
  SetDWord(D3DRS_DIFFUSEMATERIALSOURCE, Value);
end;

function TG2RenderStates.GetSpecularMaterialSource: DWord;
begin
  Result := GetDWord(D3DRS_SPECULARMATERIALSOURCE);
end;

procedure TG2RenderStates.SetSpecularMaterialSource(const Value: DWord);
begin
  SetDWord(D3DRS_SPECULARMATERIALSOURCE, Value);
end;

function TG2RenderStates.GetAmbientMaterialSource: DWord;
begin
  Result := GetDWord(D3DRS_AMBIENTMATERIALSOURCE);
end;

procedure TG2RenderStates.SetAmbientMaterialSource(const Value: DWord);
begin
  SetDWord(D3DRS_AMBIENTMATERIALSOURCE, Value);
end;

function TG2RenderStates.GetEmissiveMaterialSource: DWord;
begin
  Result := GetDWord(D3DRS_EMISSIVEMATERIALSOURCE);
end;

procedure TG2RenderStates.SetEmissiveMaterialSource(const Value: DWord);
begin
  SetDWord(D3DRS_EMISSIVEMATERIALSOURCE, Value);
end;

function TG2RenderStates.GetVertexBlend: DWord;
begin
  Result := GetDWord(D3DRS_VERTEXBLEND);
end;

procedure TG2RenderStates.SetVertexBlend(const Value: DWord);
begin
  SetDWord(D3DRS_VERTEXBLEND, Value);
end;

function TG2RenderStates.GetClipPlaneEnable: DWord;
begin
  Result := GetDWord(D3DRS_CLIPPLANEENABLE);
end;

procedure TG2RenderStates.SetClipPlaneEnable(const Value: DWord);
begin
  SetDWord(D3DRS_CLIPPLANEENABLE, Value);
end;

function TG2RenderStates.GetPointSize: Single;
begin
  Result := GetSingle(D3DRS_POINTSIZE);
end;

procedure TG2RenderStates.SetPointSize(const Value: Single);
begin
  SetSingle(D3DRS_POINTSIZE, Value);
end;

function TG2RenderStates.GetPointSizeMin: Single;
begin
  Result := GetSingle(D3DRS_POINTSIZE_MIN);
end;

procedure TG2RenderStates.SetPointSizeMin(const Value: Single);
begin
  SetSingle(D3DRS_POINTSIZE_MIN, Value);
end;

function TG2RenderStates.GetPointSizeMax: Single;
begin
  Result := GetSingle(D3DRS_POINTSIZE_MAX);
end;

procedure TG2RenderStates.SetPointSizeMax(const Value: Single);
begin
  SetSingle(D3DRS_POINTSIZE_MAX, Value);
end;

function TG2RenderStates.GetPointSpriteEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_POINTSPRITEENABLE);
end;

procedure TG2RenderStates.SetPointSpriteEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_POINTSPRITEENABLE, Value);
end;

function TG2RenderStates.GetPointScaleEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_POINTSCALEENABLE);
end;

procedure TG2RenderStates.SetPointScaleEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_POINTSCALEENABLE, Value);
end;

function TG2RenderStates.GetPointScaleA: Single;
begin
  Result := GetSingle(D3DRS_POINTSCALE_A);
end;

procedure TG2RenderStates.SetPointScaleA(const Value: Single);
begin
  SetSingle(D3DRS_POINTSCALE_A, Value);
end;

function TG2RenderStates.GetPointScaleB: Single;
begin
  Result := GetSingle(D3DRS_POINTSCALE_B);
end;

procedure TG2RenderStates.SetPointScaleB(const Value: Single);
begin
  SetSingle(D3DRS_POINTSCALE_B, Value)
end;

function TG2RenderStates.GetPointScaleC: Single;
begin
  Result := GetSingle(D3DRS_POINTSCALE_C);
end;

procedure TG2RenderStates.SetPointScaleC(const Value: Single);
begin
  SetSingle(D3DRS_POINTSCALE_C, Value);
end;

function TG2RenderStates.GetMultisampleAntialias: Boolean;
begin
  Result := GetBoolean(D3DRS_MULTISAMPLEANTIALIAS);
end;

procedure TG2RenderStates.SetMultisampleAntialias(const Value: Boolean);
begin
  SetBoolean(D3DRS_MULTISAMPLEANTIALIAS, Value);
end;

function TG2RenderStates.GetMultisampleMask: DWord;
begin
  Result := GetDWord(D3DRS_MULTISAMPLEMASK);
end;

procedure TG2RenderStates.SetMultisampleMask(const Value: DWord);
begin
  SetDWord(D3DRS_MULTISAMPLEMASK, Value);
end;

function TG2RenderStates.GetPatchEdgeStyle: TD3DPatchEdgeStyle;
begin
  Result := TD3DPatchEdgeStyle(GetDWord(D3DRS_PATCHEDGESTYLE));
end;

procedure TG2RenderStates.SetPatchEdgeStyle(const Value: TD3DPatchEdgeStyle);
begin
  SetDWord(D3DRS_PATCHEDGESTYLE, DWord(Value));
end;

function TG2RenderStates.GetDebugMonitorToken: DWord;
begin
  Result := GetDWord(D3DRS_DEBUGMONITORTOKEN);
end;

procedure TG2RenderStates.SetDebugMonitorToken(const Value: DWord);
begin
  SetDWord(D3DRS_DEBUGMONITORTOKEN, Value);
end;

function TG2RenderStates.GetIndexedVertexBlendEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_INDEXEDVERTEXBLENDENABLE);
end;

procedure TG2RenderStates.SetIndexedVertexBlendEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_INDEXEDVERTEXBLENDENABLE, Value);
end;

function TG2RenderStates.GetColorWriteEnable: DWord;
begin
  Result := GetDWord(D3DRS_COLORWRITEENABLE);
end;

procedure TG2RenderStates.SetColorWriteEnable(const Value: DWord);
begin
  SetDWord(D3DRS_COLORWRITEENABLE, Value);
end;

function TG2RenderStates.GetTweenFactor: Single;
begin
  Result := GetSingle(D3DRS_TWEENFACTOR);
end;

procedure TG2RenderStates.SetTweenFactor(const Value: Single);
begin
  SetSingle(D3DRS_TWEENFACTOR, Value);
end;

function TG2RenderStates.GetBlendOp: DWord;
begin
  Result := GetDWord(D3DRS_BLENDOP);
end;

procedure TG2RenderStates.SetBlendOp(const Value: DWord);
begin
  SetDWord(D3DRS_BLENDOP, Value);
end;

function TG2RenderStates.GetPositionDegree: TD3DDegreeType;
begin
  Result := TD3DDegreeType(GetDWord(D3DRS_POSITIONDEGREE));
end;

procedure TG2RenderStates.SetPositionDegree(const Value: TD3DDegreeType);
begin
  SetDWord(D3DRS_POSITIONDEGREE, DWord(Value));
end;

function TG2RenderStates.GetNormalDegree: TD3DDegreeType;
begin
  Result := TD3DDegreeType(GetDWord(D3DRS_NORMALDEGREE));
end;

procedure TG2RenderStates.SetNormalDegree(const Value: TD3DDegreeType);
begin
  SetDWord(D3DRS_NORMALDEGREE, DWord(Value));
end;

function TG2RenderStates.GetScissorTestEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_SCISSORTESTENABLE);
end;

procedure TG2RenderStates.SetScissorTestEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_SCISSORTESTENABLE, Value);
end;

function TG2RenderStates.GetSlopeScaleDepthBias: Single;
begin
  Result := GetSingle(D3DRS_SLOPESCALEDEPTHBIAS);
end;

procedure TG2RenderStates.SetSlopeScaleDepthBias(const Value: Single);
begin
  SetSingle(D3DRS_SLOPESCALEDEPTHBIAS, Value);
end;

function TG2RenderStates.GetAntialiasedLineEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_ANTIALIASEDLINEENABLE);
end;

procedure TG2RenderStates.SetAntialiasedLineEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_ANTIALIASEDLINEENABLE, Value)
end;

function TG2RenderStates.GetMinTessellationLevel: Single;
begin
  Result := GetSingle(D3DRS_MINTESSELLATIONLEVEL);
end;

procedure TG2RenderStates.SetMinTessellationLevel(const Value: Single);
begin
  SetSingle(D3DRS_MINTESSELLATIONLEVEL, Value);
end;

function TG2RenderStates.GetMaxTessellationLevel: Single;
begin
  Result := GetSingle(D3DRS_MAXTESSELLATIONLEVEL);
end;

procedure TG2RenderStates.SetMaxTessellationLevel(const Value: Single);
begin
  SetSingle(D3DRS_MAXTESSELLATIONLEVEL, Value);
end;

function TG2RenderStates.GetAdaptiveTessX: DWord;
begin
  Result := GetDWord(D3DRS_ADAPTIVETESS_X);
end;

procedure TG2RenderStates.SetAdaptiveTessX(const Value: DWord);
begin
  SetDWord(D3DRS_ADAPTIVETESS_X, Value);
end;

function TG2RenderStates.GetAdaptiveTessY: DWord;
begin
  Result := GetDWord(D3DRS_ADAPTIVETESS_Y);
end;

procedure TG2RenderStates.SetAdaptiveTessY(const Value: DWord);
begin
  SetDWord(D3DRS_ADAPTIVETESS_Y, Value);
end;

function TG2RenderStates.GetAdaptiveTessZ: DWord;
begin
  Result := GetDWord(D3DRS_ADAPTIVETESS_Z);
end;

procedure TG2RenderStates.SetAdaptiveTessZ(const Value: DWord);
begin
  SetDWord(D3DRS_ADAPTIVETESS_Z, Value);
end;

function TG2RenderStates.GetAdaptiveTessW: DWord;
begin
  Result := GetDWord(D3DRS_ADAPTIVETESS_W);
end;

procedure TG2RenderStates.SetAdaptiveTessW(const Value: DWord);
begin
  SetDWord(D3DRS_ADAPTIVETESS_W, Value);
end;

function TG2RenderStates.GetEnableAdaptiveTessellation: Boolean;
begin
  Result := GetBoolean(D3DRS_ENABLEADAPTIVETESSELLATION);
end;

procedure TG2RenderStates.SetEnableAdaptiveTessellation(const Value: Boolean);
begin
  SetBoolean(D3DRS_ENABLEADAPTIVETESSELLATION, Value);
end;

function TG2RenderStates.GetColorWriteEnable1: DWord;
begin
  Result := GetDWord(D3DRS_COLORWRITEENABLE1);
end;

procedure TG2RenderStates.SetColorWriteEnable1(const Value: DWord);
begin
  SetDWord(D3DRS_COLORWRITEENABLE1, Value);
end;

function TG2RenderStates.GetColorWriteEnable2: DWord;
begin
  Result := GetDWord(D3DRS_COLORWRITEENABLE2);
end;

procedure TG2RenderStates.SetColorWriteEnable2(const Value: DWord);
begin
  SetDWord(D3DRS_COLORWRITEENABLE2, Value)
end;

function TG2RenderStates.GetColorWriteEnable3: DWord;
begin
  Result := GetDWord(D3DRS_COLORWRITEENABLE3);
end;

procedure TG2RenderStates.SetColorWriteEnable3(const Value: DWord);
begin
  SetDWord(D3DRS_COLORWRITEENABLE3, Value);
end;

function TG2RenderStates.GetBlendFactor: DWord;
begin
  Result := GetDWord(D3DRS_BLENDFACTOR);
end;

procedure TG2RenderStates.SetBlendFactor(const Value: DWord);
begin
  SetDWord(D3DRS_BLENDFACTOR, Value);
end;

function TG2RenderStates.GetSRGBWriteEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_SRGBWRITEENABLE);
end;

procedure TG2RenderStates.SetSRGBWriteEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_SRGBWRITEENABLE, Value);
end;

function TG2RenderStates.GetDepthBias: Single;
begin
  Result := GetSingle(D3DRS_DEPTHBIAS);
end;

procedure TG2RenderStates.SetDepthBias(const Value: Single);
begin
  SetSingle(D3DRS_DEPTHBIAS, Value);
end;

function TG2RenderStates.GetSeparateAlphaBlendEnable: Boolean;
begin
  Result := GetBoolean(D3DRS_SEPARATEALPHABLENDENABLE);
end;

procedure TG2RenderStates.SetSeparateAlphaBlendEnable(const Value: Boolean);
begin
  SetBoolean(D3DRS_SEPARATEALPHABLENDENABLE, Value);
end;

function TG2RenderStates.GetSrcBlendAlpha: DWord;
begin
  Result := GetDWord(D3DRS_SRCBLENDALPHA);
end;

procedure TG2RenderStates.SetSrcBlendAlpha(const Value: DWord);
begin
  SetDWord(D3DRS_SRCBLENDALPHA, Value);
end;

function TG2RenderStates.GetDestBlendAlpha: DWord;
begin
  Result := GetDWord(D3DRS_DESTBLENDALPHA);
end;

procedure TG2RenderStates.SetDestBlendAlpha(const Value: DWord);
begin
  SetDWord(D3DRS_DESTBLENDALPHA, Value);
end;

function TG2RenderStates.GetBlendOpAlpha: DWord;
begin
  Result := GetDWord(D3DRS_BLENDOPALPHA);
end;

procedure TG2RenderStates.SetBlendOpAlpha(const Value: DWord);
begin
  SetDWord(D3DRS_BLENDOPALPHA, Value);
end;

procedure TG2RenderStates.SetDefaults;
begin
  ZEnable := False;
  FillMode := D3DFILL_SOLID;
  ShadeMode := D3DSHADE_GOURAUD;
  ZWriteEnable := True;
  AlphaTestEnable := True;
  LastPixel := True;
  SrcBlend := D3DBLEND_SRCALPHA;
  DestBlend := D3DBLEND_INVSRCALPHA;
  BlendOp := D3DBLENDOP_ADD;
  CullMode := D3DCULL_CCW;
  ZFunc := D3DCMP_LESSEQUAL;
  AlphaRef := 16;
  AlphaFunc := D3DCMP_GREATEREQUAL;
  DitherEnable := False;
  AlphaBlendEnable := True;
  FogEnable := False;
  SpecularEnable := False;
  FogColor := 0;
  FogTableMode := D3DFOG_NONE;
  FogVertexMode := D3DFOG_NONE;
  FogStart := 0;
  FogEnd := 1;
  FogDensity := 1;
  RangeFogEnable := False;
  StencilEnable := False;
  StencilFail := D3DSTENCILOP_KEEP;
  StencilZFail := D3DSTENCILOP_KEEP;
  StencilPass := D3DSTENCILOP_KEEP;
  StencilFunc := D3DCMP_ALWAYS;
  StencilRef := 0;
  StencilMask := $ffffffff;
  StencilWriteMask := $ffffffff;
  TextureFactor := $ffffffff;
  Wrap0 := 0;
  Wrap1 := 0;
  Wrap2 := 0;
  Wrap3 := 0;
  Wrap4 := 0;
  Wrap5 := 0;
  Wrap6 := 0;
  Wrap7 := 0;
  Wrap8 := 0;
  Wrap9 := 0;
  Wrap10 := 0;
  Wrap11 := 0;
  Wrap12 := 0;
  Wrap13 := 0;
  Wrap14 := 0;
  Wrap15 := 0;
  Clipping := True;
  Lighting := False;
  Ambient := 0;
  ColorVertex := True;
  LocalViewer := True;
  NormalizeNormals := True;
  DiffuseMaterialSource := D3DMCS_COLOR1;
  SpecularMaterialSource := D3DMCS_COLOR2;
  AmbientMaterialSource := D3DMCS_MATERIAL;
  EmissiveMaterialSource := D3DMCS_MATERIAL;
  VertexBlend := D3DVBF_DISABLE;
  ClipPlaneEnable := 0;
  PointSize := 4;
  PointSizeMin := 1;
  PointSpriteEnable := False;
  PointScaleEnable := False;
  PointScaleA := 0;
  PointScaleB := 0;
  PointScaleC := 0;
  MultisampleAntialias := True;
  MultisampleMask := $ffffffff;
  PatchEdgeStyle := D3DPATCHEDGE_DISCRETE;
  DebugMonitorToken := D3DDMT_DISABLE;
  IndexedVertexBlendEnable := False;
  ColorWriteEnable := $f;
  TweenFactor := 0;
  PositionDegree := D3DDEGREE_CUBIC;
  NormalDegree := D3DDEGREE_LINEAR;
  ScissorTestEnable := False;
  SlopeScaleDepthBias := 0;
  AntialiasedLineEnable := False;
  MinTessellationLevel := 1;
  MaxTessellationLevel := 1;
  AdaptiveTessX := 0;
  AdaptiveTessY := 0;
  AdaptiveTessZ := 1;
  AdaptiveTessW := 0;
  EnableAdaptiveTessellation := False;
  TwoSidedStencilMode := False;
  CCWStencilFail := D3DSTENCILOP_KEEP;
  CCWStencilZFail := D3DSTENCILOP_KEEP;
  CCWStencilPass := D3DSTENCILOP_KEEP;
  CCWStencilFunc := D3DCMP_ALWAYS;
  ColorWriteEnable1 := $f;
  ColorWriteEnable2 := $f;
  ColorWriteEnable3 := $f;
  BlendFactor := $ffffffff;
  SRGBWriteEnable := False;
  DepthBias := 0;
  SeparateAlphaBlendEnable := True;
  SrcBlendAlpha := D3DBLEND_ONE;
  DestBlendAlpha := D3DBLEND_ONE;
  BlendopAlpha := D3DBLENDOP_ADD;
end;

procedure TG2RenderStates.SetFromMemory;
  procedure ResetState(const State: TD3DRenderStateType);
  begin
    SetDWord(State, m_States[Byte(State)]);
  end;
begin
  ResetState(D3DRS_ZENABLE);
  ResetState(D3DRS_FILLMODE);
  ResetState(D3DRS_SHADEMODE);
  ResetState(D3DRS_ZWRITEENABLE);
  ResetState(D3DRS_ALPHATESTENABLE);
  ResetState(D3DRS_LASTPIXEL);
  ResetState(D3DRS_SRCBLEND);
  ResetState(D3DRS_DESTBLEND);
  ResetState(D3DRS_CULLMODE);
  ResetState(D3DRS_ZFUNC);
  ResetState(D3DRS_ALPHAREF);
  ResetState(D3DRS_ALPHAFUNC);
  ResetState(D3DRS_DITHERENABLE);
  ResetState(D3DRS_ALPHABLENDENABLE);
  ResetState(D3DRS_FOGENABLE);
  ResetState(D3DRS_SPECULARENABLE);
  ResetState(D3DRS_FOGCOLOR);
  ResetState(D3DRS_FOGTABLEMODE);
  ResetState(D3DRS_FOGSTART);
  ResetState(D3DRS_FOGEND);
  ResetState(D3DRS_FOGDENSITY);
  ResetState(D3DRS_RANGEFOGENABLE);
  ResetState(D3DRS_STENCILENABLE);
  ResetState(D3DRS_STENCILFAIL);
  ResetState(D3DRS_STENCILZFAIL);
  ResetState(D3DRS_STENCILPASS);
  ResetState(D3DRS_STENCILFUNC);
  ResetState(D3DRS_STENCILREF);
  ResetState(D3DRS_STENCILMASK);
  ResetState(D3DRS_STENCILWRITEMASK);
  ResetState(D3DRS_TEXTUREFACTOR);
  ResetState(D3DRS_WRAP0);
  ResetState(D3DRS_WRAP1);
  ResetState(D3DRS_WRAP2);
  ResetState(D3DRS_WRAP3);
  ResetState(D3DRS_WRAP4);
  ResetState(D3DRS_WRAP5);
  ResetState(D3DRS_WRAP6);
  ResetState(D3DRS_WRAP7);
  ResetState(D3DRS_CLIPPING);
  ResetState(D3DRS_LIGHTING);
  ResetState(D3DRS_AMBIENT);
  ResetState(D3DRS_FOGVERTEXMODE);
  ResetState(D3DRS_COLORVERTEX);
  ResetState(D3DRS_LOCALVIEWER);
  ResetState(D3DRS_NORMALIZENORMALS);
  ResetState(D3DRS_DIFFUSEMATERIALSOURCE);
  ResetState(D3DRS_SPECULARMATERIALSOURCE);
  ResetState(D3DRS_AMBIENTMATERIALSOURCE);
  ResetState(D3DRS_EMISSIVEMATERIALSOURCE);
  ResetState(D3DRS_VERTEXBLEND);
  ResetState(D3DRS_CLIPPLANEENABLE);
  ResetState(D3DRS_POINTSIZE);
  ResetState(D3DRS_POINTSIZE_MIN);
  ResetState(D3DRS_POINTSPRITEENABLE);
  ResetState(D3DRS_POINTSCALEENABLE);
  ResetState(D3DRS_POINTSCALE_A);
  ResetState(D3DRS_POINTSCALE_B);
  ResetState(D3DRS_POINTSCALE_C);
  ResetState(D3DRS_MULTISAMPLEANTIALIAS);
  ResetState(D3DRS_MULTISAMPLEMASK);
  ResetState(D3DRS_PATCHEDGESTYLE);
  ResetState(D3DRS_DEBUGMONITORTOKEN);
  ResetState(D3DRS_POINTSIZE_MAX);
  ResetState(D3DRS_INDEXEDVERTEXBLENDENABLE);
  ResetState(D3DRS_COLORWRITEENABLE);
  ResetState(D3DRS_TWEENFACTOR);
  ResetState(D3DRS_BLENDOP);
  ResetState(D3DRS_POSITIONDEGREE);
  ResetState(D3DRS_NORMALDEGREE);
  ResetState(D3DRS_SCISSORTESTENABLE);
  ResetState(D3DRS_SLOPESCALEDEPTHBIAS);
  ResetState(D3DRS_ANTIALIASEDLINEENABLE);
  ResetState(D3DRS_MINTESSELLATIONLEVEL);
  ResetState(D3DRS_MAXTESSELLATIONLEVEL);
  ResetState(D3DRS_ADAPTIVETESS_X);
  ResetState(D3DRS_ADAPTIVETESS_Y);
  ResetState(D3DRS_ADAPTIVETESS_Z);
  ResetState(D3DRS_ADAPTIVETESS_W);
  ResetState(D3DRS_ENABLEADAPTIVETESSELLATION);
  ResetState(D3DRS_TWOSIDEDSTENCILMODE);
  ResetState(D3DRS_CCW_STENCILFAIL);
  ResetState(D3DRS_CCW_STENCILZFAIL);
  ResetState(D3DRS_CCW_STENCILPASS);
  ResetState(D3DRS_CCW_STENCILFUNC);
  ResetState(D3DRS_COLORWRITEENABLE1);
  ResetState(D3DRS_COLORWRITEENABLE2);
  ResetState(D3DRS_COLORWRITEENABLE3);
  ResetState(D3DRS_BLENDFACTOR);
  ResetState(D3DRS_SRGBWRITEENABLE);
  ResetState(D3DRS_DEPTHBIAS);
  ResetState(D3DRS_WRAP8);
  ResetState(D3DRS_WRAP9);
  ResetState(D3DRS_WRAP10);
  ResetState(D3DRS_WRAP11);
  ResetState(D3DRS_WRAP12);
  ResetState(D3DRS_WRAP13);
  ResetState(D3DRS_WRAP14);
  ResetState(D3DRS_WRAP15);
  ResetState(D3DRS_SEPARATEALPHABLENDENABLE);
  ResetState(D3DRS_SRCBLENDALPHA);
  ResetState(D3DRS_DESTBLENDALPHA);
  ResetState(D3DRS_BLENDOPALPHA);
end;
//TG2RenderStates END

//TG2SamplerStates BEGIN
constructor TG2SamplerStates.Create;
begin
  inherited Create;
end;

destructor TG2SamplerStates.Destroy;
begin
  inherited Destroy;
end;

function TG2SamplerStates.GetDWord(const Sampler: Byte; const StateType: TD3DSamplerStateType): DWord;
begin
  Result := m_Gfx.Device.GetSamplerState(Sampler, StateType, Result);
end;

procedure TG2SamplerStates.SetDWord(const Sampler: Byte; const StateType: TD3DSamplerStateType; const Value: DWord);
begin
  m_States[Sampler, Byte(StateType)] := Value;
  m_Gfx.Device.SetSamplerState(Sampler, StateType, Value);
end;

function TG2SamplerStates.GetSingle(const Sampler: Byte; const StateType: TD3DSamplerStateType): Single;
var
  Value: DWord;
begin
  m_Gfx.Device.GetSamplerState(Sampler, StateType, Value);
  Result := PSingle(@Value)^;
end;

procedure TG2SamplerStates.SetSingle(const Sampler: Byte; const StateType: TD3DSamplerStateType; const Value: Single);
begin
  m_States[Sampler, Byte(StateType)] := PDWord(@Value)^;
  m_Gfx.Device.SetSamplerState(Sampler, StateType, PDWord(@Value)^);
end;

function TG2SamplerStates.GetAddressU(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_ADDRESSU);
end;

procedure TG2SamplerStates.SetAddressU(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_ADDRESSU, Value);
end;

function TG2SamplerStates.GetAddressV(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_ADDRESSV);
end;

procedure TG2SamplerStates.SetAddressV(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_ADDRESSV, Value);
end;

function TG2SamplerStates.GetAddressW(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_ADDRESSW);
end;

procedure TG2SamplerStates.SetAddressW(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_ADDRESSW, Value);
end;

function TG2SamplerStates.GetBorderColor(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_BORDERCOLOR);
end;

procedure TG2SamplerStates.SetBorderColor(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_BORDERCOLOR, Value);
end;

function TG2SamplerStates.GetMagFilter(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_MAGFILTER);
end;

procedure TG2SamplerStates.SetMagFilter(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_MAGFILTER, Value);
end;

function TG2SamplerStates.GetMinFilter(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_MINFILTER);
end;

procedure TG2SamplerStates.SetMinFilter(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_MINFILTER, Value);
end;

function TG2SamplerStates.GetMipFilter(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_MIPFILTER);
end;

procedure TG2SamplerStates.SetMipFilter(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_MIPFILTER, Value);
end;

function TG2SamplerStates.GetMipMapLODBias(const Index: Byte): Single;
begin
  Result := GetSingle(Index, D3DSAMP_MIPMAPLODBIAS);
end;

procedure TG2SamplerStates.SetMipMapLODBias(const Index: Byte; const Value: Single);
begin
  SetSingle(Index, D3DSAMP_MIPMAPLODBIAS, Value);
end;

function TG2SamplerStates.GetMaxMipLevel(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_MAXMIPLEVEL);
end;

procedure TG2SamplerStates.SetMaxMipLevel(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_MAXMIPLEVEL, Value);
end;

function TG2SamplerStates.GetMaxAnisotropy(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_MAXANISOTROPY);
end;

procedure TG2SamplerStates.SetMaxAnisotropy(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_MAXANISOTROPY, Value);
end;

function TG2SamplerStates.GetSRGBTexture(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_SRGBTEXTURE);
end;

procedure TG2SamplerStates.SetSRGBTexture(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_SRGBTEXTURE, Value);
end;

function TG2SamplerStates.GetElementIndex(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_ELEMENTINDEX);
end;

procedure TG2SamplerStates.SetElementIndex(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_ELEMENTINDEX, Value);
end;

function TG2SamplerStates.GetDMapOffset(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DSAMP_DMAPOFFSET);
end;

procedure TG2SamplerStates.SetDMapOffset(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DSAMP_DMAPOFFSET, Value);
end;

procedure TG2SamplerStates.SetDefaults;
var
  i: Integer;
  MaxSamplers: Integer;
begin
  MaxSamplers := m_Gfx.Caps.MaxTextureBlendStages;
  for i := 0 to MaxSamplers - 1 do
  begin
    AddressU[i] := D3DTADDRESS_WRAP;
    AddressV[i] := D3DTADDRESS_WRAP;
    AddressW[i] := D3DTADDRESS_WRAP;
    BorderColor[i] := 0;
    MagFilter[i] := D3DTEXF_POINT;
    MinFilter[i] := D3DTEXF_POINT;
    MipFilter[i] := D3DTEXF_NONE;
    MipMapLODBias[i] := 0;
    MaxMipLevel[i] := 0;
    MaxAnisotropy[i] := 1;
    SRGBTexture[i] := 0;
    ElementIndex[i] := 0;
    DMapOffset[i] := 0;
  end;
end;

procedure TG2SamplerStates.SetFromMemory;
var
  i, j: Integer;
  MaxSamplers: Integer;
begin
  MaxSamplers := m_Gfx.Caps.MaxTextureBlendStages;
  for i := 0 to MaxSamplers - 1 do
  for j := 1 to 13 do
  SetDWord(i, TD3DSamplerStateType(j), m_States[i, j]);
end;
//TG2SamplerStates END

//TG2TextureStageStates BEGIN
constructor TG2TextureStageStates.Create;
begin
  inherited Create;
end;

destructor TG2TextureStageStates.Destroy;
begin
  inherited Destroy;
end;

function TG2TextureStageStates.GetDWord(const Index: Byte; const StageType: TD3DTextureStageStateType): DWord;
begin
  m_Gfx.Device.GetTextureStageState(Index, StageType, Result);
end;

procedure TG2TextureStageStates.SetDWord(const Index: Byte; const StageType: TD3DTextureStageStateType; const Value: DWord);
begin
  m_States[Index, Byte(StageType)] := Value;
  m_Gfx.Device.SetTextureStageState(Index, StageType, Value);
end;

function TG2TextureStageStates.GetSingle(const Index: Byte; const StageType: TD3DTextureStageStateType): Single;
var
  Value: DWord;
begin
  m_Gfx.Device.GetTextureStageState(Index, StageType, Value);
  Result := PSingle(@Value)^;
end;

procedure TG2TextureStageStates.SetSingle(const Index: Byte; const StageType: TD3DTextureStageStateType; const Value: Single);
begin
  m_States[Index, Byte(StageType)] := PDWord(@Value)^;
  m_Gfx.Device.SetTextureStageState(Index, StageType, PDWord(@Value)^);
end;

function TG2TextureStageStates.GetColorOp(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_COLOROP);
end;

procedure TG2TextureStageStates.SetColorOp(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_COLOROP, Value);
end;

function TG2TextureStageStates.GetColorArg0(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_COLORARG0);
end;

procedure TG2TextureStageStates.SetColorArg0(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_COLORARG0, Value);
end;

function TG2TextureStageStates.GetColorArg1(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_COLORARG1);
end;

procedure TG2TextureStageStates.SetColorArg1(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_COLORARG1, Value);
end;

function TG2TextureStageStates.GetColorArg2(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_COLORARG2);
end;

procedure TG2TextureStageStates.SetColorArg2(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_COLORARG2, Value);
end;

function TG2TextureStageStates.GetAlphaOp(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_ALPHAOP);
end;

procedure TG2TextureStageStates.SetAlphaOp(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_ALPHAOP, Value);
end;

function TG2TextureStageStates.GetAlphaArg0(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_ALPHAARG0);
end;

procedure TG2TextureStageStates.SetAlphaArg0(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_ALPHAARG0, Value);
end;

function TG2TextureStageStates.GetAlphaArg1(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_ALPHAARG1);
end;

procedure TG2TextureStageStates.SetAlphaArg1(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_ALPHAARG1, Value);
end;

function TG2TextureStageStates.GetAlphaArg2(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_ALPHAARG2);
end;

procedure TG2TextureStageStates.SetAlphaArg2(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_ALPHAARG2, Value);
end;

function TG2TextureStageStates.GetBumpEnvMat00(const Index: Byte): Single;
begin
  Result := GetSingle(Index, D3DTSS_BUMPENVMAT00);
end;

procedure TG2TextureStageStates.SetBumpEnvMat00(const Index: Byte; const Value: Single);
begin
  SetSingle(Index, D3DTSS_BUMPENVMAT00, Value);
end;

function TG2TextureStageStates.GetBumpEnvMat01(const Index: Byte): Single;
begin
  Result := GetSingle(Index, D3DTSS_BUMPENVMAT01);
end;

procedure TG2TextureStageStates.SetBumpEnvMat01(const Index: Byte; const Value: Single);
begin
  SetSingle(Index, D3DTSS_BUMPENVMAT01, Value);
end;

function TG2TextureStageStates.GetBumpEnvMat10(const Index: Byte): Single;
begin
  Result := GetSingle(Index, D3DTSS_BUMPENVMAT10);
end;

procedure TG2TextureStageStates.SetBumpEnvMat10(const Index: Byte; const Value: Single);
begin
  SetSingle(Index, D3DTSS_BUMPENVMAT10, Value);
end;

function TG2TextureStageStates.GetBumpEnvMat11(const Index: Byte): Single;
begin
  Result := GetSingle(Index, D3DTSS_BUMPENVMAT11);
end;

procedure TG2TextureStageStates.SetBumpEnvMat11(const Index: Byte; const Value: Single);
begin
  SetSingle(Index, D3DTSS_BUMPENVMAT11, Value);
end;

function TG2TextureStageStates.GetTexCoordIndex(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_TEXCOORDINDEX);
end;

procedure TG2TextureStageStates.SetTexCoordIndex(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_TEXCOORDINDEX, Value);
end;

function TG2TextureStageStates.GetBumpEnvLScale(const Index: Byte): Single;
begin
  Result := GetSingle(Index, D3DTSS_BUMPENVLSCALE);
end;

procedure TG2TextureStageStates.SetBumpEnvLScale(const Index: Byte; const Value: Single);
begin
  SetSingle(Index, D3DTSS_BUMPENVLSCALE, Value);
end;

function TG2TextureStageStates.GetBumpEnvLOffset(const Index: Byte): Single;
begin
  Result := GetSingle(Index, D3DTSS_BUMPENVLOFFSET);
end;

procedure TG2TextureStageStates.SetBumpEnvLOffset(const Index: Byte; const Value: Single);
begin
  SetSingle(Index, D3DTSS_BUMPENVLOFFSET, Value);
end;

function TG2TextureStageStates.GetTextureTransformFlags(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_TEXTURETRANSFORMFLAGS);
end;

procedure TG2TextureStageStates.SetTextureTransformFlags(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_TEXTURETRANSFORMFLAGS, Value);
end;

function TG2TextureStageStates.GetResultArg(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_RESULTARG);
end;

procedure TG2TextureStageStates.SetResultArg(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_RESULTARG, Value);
end;

function TG2TextureStageStates.GetConstant(const Index: Byte): DWord;
begin
  Result := GetDWord(Index, D3DTSS_CONSTANT);
end;

procedure TG2TextureStageStates.SetConstant(const Index: Byte; const Value: DWord);
begin
  SetDWord(Index, D3DTSS_CONSTANT, Value);
end;

procedure TG2TextureStageStates.SetDefaults;
var
  i: Integer;
  MaxStages: Integer;
begin
  MaxStages := m_Gfx.Caps.MaxTextureBlendStages;
  ColorOp[0] := D3DTOP_MODULATE;
  ColorArg0[0] := D3DTA_CURRENT;
  ColorArg1[0] := D3DTA_TEXTURE;
  ColorArg2[0] := D3DTA_DIFFUSE;
  AlphaOp[0] := D3DTOP_MODULATE;
  AlphaArg0[0] := D3DTA_CURRENT;
  AlphaArg1[0] := D3DTA_TEXTURE;
  AlphaArg2[0] := D3DTA_DIFFUSE;
  BumpEnvMat00[0] := 0;
  BumpEnvMat01[0] := 0;
  BumpEnvMat10[0] := 0;
  BumpEnvMat11[0] := 0;
  TexCoordIndex[0] := 0;
  BumpEnvLScale[0] := 0;
  BumpEnvLOffset[0] := 0;
  TextureTransformFlags[0] := D3DTTFF_DISABLE;
  ResultArg[0] := D3DTA_CURRENT;
  Constant[0] := 0;
  for i := 1 to MaxStages - 1 do
  begin
    ColorOp[i] := D3DTOP_DISABLE;
    ColorArg0[i] := D3DTA_CURRENT;
    ColorArg1[i] := D3DTA_TEXTURE;
    ColorArg2[i] := D3DTA_CURRENT;
    AlphaOp[i] := D3DTOP_DISABLE;
    AlphaArg0[i] := D3DTA_CURRENT;
    AlphaArg1[i] := D3DTA_DIFFUSE;
    AlphaArg2[i] := D3DTA_CURRENT;
    BumpEnvMat00[i] := 0;
    BumpEnvMat01[i] := 0;
    BumpEnvMat10[i] := 0;
    BumpEnvMat11[i] := 0;
    TexCoordIndex[i] := i;
    BumpEnvLScale[i] := 0;
    BumpEnvLOffset[i] := 0;
    TextureTransformFlags[i] := D3DTTFF_DISABLE;
    ResultArg[i] := D3DTA_CURRENT;
    Constant[i] := 0;
  end;
end;

procedure TG2TextureStageStates.SetFromMemory;
  procedure ResetState(const Index: Byte; const StageType: TD3DTextureStageStateType);
  begin
    SetDWord(Index, StageType, m_States[Index, Byte(StageType)]);
  end;
var
  i: Integer;
  MaxStages: Integer;
begin
  MaxStages := m_Gfx.Caps.MaxTextureBlendStages;
  for i := 0 to MaxStages - 1 do
  begin
    ResetState(i, D3DTSS_COLOROP);
    ResetState(i, D3DTSS_COLORARG1);
    ResetState(i, D3DTSS_COLORARG2);
    ResetState(i, D3DTSS_ALPHAOP);
    ResetState(i, D3DTSS_ALPHAARG1);
    ResetState(i, D3DTSS_ALPHAARG2);
    ResetState(i, D3DTSS_BUMPENVMAT00);
    ResetState(i, D3DTSS_BUMPENVMAT01);
    ResetState(i, D3DTSS_BUMPENVMAT10);
    ResetState(i, D3DTSS_BUMPENVMAT11);
    ResetState(i, D3DTSS_TEXCOORDINDEX);
    ResetState(i, D3DTSS_BUMPENVLSCALE);
    ResetState(i, D3DTSS_BUMPENVLOFFSET);
    ResetState(i, D3DTSS_TEXTURETRANSFORMFLAGS);
    ResetState(i, D3DTSS_COLORARG0);
    ResetState(i, D3DTSS_ALPHAARG0);
    ResetState(i, D3DTSS_RESULTARG);
    ResetState(i, D3DTSS_CONSTANT);
  end;
end;
//TG2TextureStageStates END

//TG2SharedVB2D BEGIN
constructor TG2SharedVB2D.Create;
begin
  inherited Create;
  m_Size := 0;
end;

destructor TG2SharedVB2D.Destroy;
begin
  SafeRelease(m_VB);
  inherited Destroy;
end;

function TG2SharedVB2D.InitializeBuffer(const Size: Integer): TG2Result;
begin
  SafeRelease(m_VB);
  m_Size := Size;
  if Failed(
    m_Gfx.Device.CreateVertexBuffer(
      SizeOf(TG2SharedVertex2D) * m_Size,
      D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
      TG2SharedVertex2DFVF,
      D3DPOOL_DEFAULT,
      m_VB,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;
  Result := grOk;
end;

procedure TG2SharedVB2D.OnDeviceLost;
begin
  SafeRelease(m_VB);
end;

procedure TG2SharedVB2D.OnDeviceReset;
begin
  InitializeBuffer(m_Size);
end;

procedure TG2SharedVB2D.Initialize(const G2Graphics: TG2Graphics);
begin
  m_Gfx := G2Graphics;
  InitializeBuffer(256);
end;

procedure TG2SharedVB2D.Finalize;
begin
  SafeRelease(m_VB);
end;

procedure TG2SharedVB2D.SetToDevice;
begin
  m_Gfx.Device.SetFVF(TG2SharedVertex2DFVF);
  m_Gfx.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2SharedVertex2D));
end;

function TG2SharedVB2D.VerifySize(const Size: Integer): TG2Result;
begin
  if m_Size < Size then
  Result := InitializeBuffer(Size)
  else
  Result := grOk;
end;
//TG2SharedVB2D END

//TG2Render BEGIN
constructor TG2Render.Create;
begin
  inherited Create;
end;

destructor TG2Render.Destroy;
begin
  inherited Destroy;
end;

function TG2Render.GetCanRender: Boolean;
begin
  Result := m_Gfx.CanRender;
end;

function TG2Render.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Gfx := Core.Graphics;
  Result := grOk;
end;

function TG2Render.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;

function TG2Render.RenderStart: TG2Result;
begin
  if Succeeded(m_Gfx.Device.BeginScene) then
  Result := grOk
  else
  Result := grFail;
end;

function TG2Render.RenderStop: TG2Result;
begin
  if Succeeded(m_Gfx.Device.EndScene) then
  Result := grOk
  else
  Result := grFail;
end;

function TG2Render.Present: TG2Result;
var
  hr: HResult;
begin
  if m_Gfx.m_CurSwapChain = nil then
  hr := m_Gfx.Device.Present(nil, nil, 0, nil)
  else
  hr := m_Gfx.m_CurSwapChain.SwapChain.Present(nil, nil, 0, nil, D3DPRESENT_DONOTWAIT);
  if Succeeded(hr) then
  Result := grOk
  else
  Result := grFail;
end;

procedure TG2Render.Clear(
      ClearStencil: Boolean;
      ClearZBuffer: Boolean;
      ClearTarget: Boolean;
      Color: TG2Color;
      Depth: Single = 1;
      Stencil: DWord = 0
    );
var
  ClearObject: byte;
begin
  ClearObject := 0;
  if ClearStencil then
  ClearObject := ClearObject or D3DCLEAR_STENCIL;
  if ClearZBuffer then
  ClearObject := ClearObject or D3DCLEAR_ZBUFFER;
  if ClearTarget then
  ClearObject := ClearObject or D3DCLEAR_TARGET;
  m_Gfx.Device.Clear(
    0,
    nil,
    ClearObject,
    Color,
    Depth,
    Stencil
  );
end;

procedure TG2Render.TextureClear(const Stage: Byte = 0);
begin
  m_Gfx.Device.SetTexture(Stage, nil);
end;

procedure TG2Render.TextureSet(const Texture: TG2TextureBase; const Stage: Byte = 0);
begin
  m_Gfx.Device.SetTexture(Stage, Texture.Texture);
end;
//TG2Render END

//TG2RenderModes BEGIN
constructor TG2RenderModes.Create;
begin
  inherited Create;
end;

destructor TG2RenderModes.Destroy;
begin
  inherited Destroy;
end;

procedure TG2RenderModes.BlendModeNormal;
begin
  with Core.Graphics do
  begin
    RenderStates.SrcBlend := D3DBLEND_SRCALPHA;
    RenderStates.DestBlend := D3DBLEND_INVSRCALPHA;
  end;
end;

procedure TG2RenderModes.BlendModeAdd;
begin
  with Core.Graphics do
  begin
    RenderStates.SrcBlend := D3DBLEND_SRCALPHA;
    RenderStates.DestBlend := D3DBLEND_DESTALPHA;
  end;
end;

procedure TG2RenderModes.BlendModeAddColor;
begin
  with Core.Graphics do
  begin
    RenderStates.SrcBlend := D3DBLEND_ONE;
    RenderStates.DestBlend := D3DBLEND_ONE;
  end;
end;

procedure TG2RenderModes.BlendModeSubColor;
begin
  with Core.Graphics do
  begin
    RenderStates.SrcBlend := D3DBLEND_ZERO;
    RenderStates.DestBlend := D3DBLEND_INVSRCCOLOR;
  end;
end;

procedure TG2RenderModes.BlendModeMultiply;
begin
  with Core.Graphics do
  begin
    RenderStates.SrcBlend := D3DBLEND_ZERO;
    RenderStates.DestBlend := D3DBLEND_SRCCOLOR;
  end;
end;

procedure TG2RenderModes.FilteringPoint(const Stage: Byte = 0);
begin
  with Core.Graphics do
  begin
    SamplerStates.MagFilter[Stage] := D3DTEXF_POINT;
    SamplerStates.MinFilter[Stage] := D3DTEXF_POINT;
    SamplerStates.MipFilter[Stage] := D3DTEXF_POINT;
  end;
end;

procedure TG2RenderModes.FilteringLinear(const Stage: Byte = 0);
begin
  with Core.Graphics do
  begin
    SamplerStates.MagFilter[Stage] := D3DTEXF_LINEAR;
    SamplerStates.MinFilter[Stage] := D3DTEXF_LINEAR;
    SamplerStates.MipFilter[Stage] := D3DTEXF_LINEAR;
  end;
end;

procedure TG2RenderModes.FilteringAnisotropic(const Stage: Byte = 0; const MaxAnisotropy: Integer = 8);
begin
  with Core.Graphics do
  begin
    SamplerStates.MagFilter[Stage] := D3DTEXF_ANISOTROPIC;
    SamplerStates.MinFilter[Stage] := D3DTEXF_ANISOTROPIC;
    SamplerStates.MipFilter[Stage] := D3DTEXF_LINEAR;
    SamplerStates.MaxAnisotropy[Stage] := MaxAnisotropy;
  end;
end;

procedure TG2RenderModes.TexAddressClamp(const Stage: Byte = 0);
begin
  with Core.Graphics do
  begin
    SamplerStates.AddressU[Stage] := D3DTADDRESS_CLAMP;
    SamplerStates.AddressV[Stage] := D3DTADDRESS_CLAMP;
    SamplerStates.AddressW[Stage] := D3DTADDRESS_CLAMP;
  end;
end;

procedure TG2RenderModes.TexAddressWrap(const Stage: Byte = 0);
begin
  with Core.Graphics do
  begin
    SamplerStates.AddressU[Stage] := D3DTADDRESS_WRAP;
    SamplerStates.AddressV[Stage] := D3DTADDRESS_WRAP;
    SamplerStates.AddressW[Stage] := D3DTADDRESS_WRAP;
  end;
end;

procedure TG2RenderModes.TexAddressBorder(const Stage: Byte = 0);
begin
  with Core.Graphics do
  begin
    SamplerStates.AddressU[Stage] := D3DTADDRESS_BORDER;
    SamplerStates.AddressV[Stage] := D3DTADDRESS_BORDER;
    SamplerStates.AddressW[Stage] := D3DTADDRESS_BORDER;
  end;
end;

procedure TG2RenderModes.TexAddressMirror(const Stage: Byte = 0);
begin
  with Core.Graphics do
  begin
    SamplerStates.AddressU[Stage] := D3DTADDRESS_MIRROR;
    SamplerStates.AddressV[Stage] := D3DTADDRESS_MIRROR;
    SamplerStates.AddressW[Stage] := D3DTADDRESS_MIRROR;
  end;
end;

procedure TG2RenderModes.ScissorSet(const R: TRect);
begin
  with Core.Graphics do
  begin
    Device.SetScissorRect(@R);
    RenderStates.ScissorTestEnable := True;
  end;
end;

procedure TG2RenderModes.ScissorDisable;
begin
  with Core.Graphics do
  begin
    RenderStates.ScissorTestEnable := False;
  end;
end;

function TG2RenderModes.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;

function TG2RenderModes.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2RenderModes END

//TG2Render2D BEGIN
constructor TG2Render2D.Create;
begin
  inherited Create;
  m_VBSize := 0;
end;

destructor TG2Render2D.Destroy;
begin
  inherited Destroy;
end;

function TG2Render2D.VerifyBuffer(const Size: Integer): TG2Result;
begin
  Result := grOk;
  if (Size > m_VBSize)
  or (m_VB = nil) then
  begin
    SafeRelease(m_VB);
    if Succeeded(
      m_Gfx.Device.CreateVertexBuffer(
        Size * SizeOf(TVertex),
        D3DUSAGE_WRITEONLY,
        FVF,
        D3DPOOL_MANAGED,
        m_VB,
        nil
      )
    ) then
    begin
      m_VBSize := Size;
    end
    else
    Result := grFail;
  end;
end;

function TG2Render2D.VerifyBuffer2(const Size: Integer): TG2Result;
begin
  Result := grOk;
  if (Size > m_VB2Size)
  or (m_VB2 = nil) then
  begin
    SafeRelease(m_VB2);
    if Succeeded(
      m_Gfx.Device.CreateVertexBuffer(
        Size * SizeOf(TVertex2),
        D3DUSAGE_WRITEONLY,
        FVF2,
        D3DPOOL_MANAGED,
        m_VB2,
        nil
      )
    ) then
    begin
      m_VB2Size := Size;
    end
    else
    Result := grFail;
  end;
end;

function TG2Render2D.VerifyBuffer3(const Size: Integer): TG2Result;
begin
  Result := grOk;
  if (Size > m_VB3Size)
  or (m_VB3 = nil) then
  begin
    SafeRelease(m_VB3);
    if Succeeded(
      m_Gfx.Device.CreateVertexBuffer(
        Size * SizeOf(TVertex3),
        D3DUSAGE_WRITEONLY,
        FVF3,
        D3DPOOL_MANAGED,
        m_VB3,
        nil
      )
    ) then
    begin
      m_VB3Size := Size;
    end
    else
    Result := grFail;
  end;
end;

function TG2Render2D.VerifyIndices(const Size: Integer): TG2Result;
begin
  Result := grOk;
  if (Size > m_IBSize)
  or (m_IB = nil) then
  begin
    SafeRelease(m_IB);
    if Succeeded(
      m_Gfx.Device.CreateIndexBuffer(
        Size * SizeOf(Word),
        D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
        D3DFMT_INDEX16,
        D3DPOOL_DEFAULT,
        m_IB,
        nil
      )
    ) then
    begin
      m_IBSize := Size;
    end
    else
    Result := grFail;
  end;
end;

procedure TG2Render2D.OnDeviceLost;
begin
  SafeRelease(m_VB);
  SafeRelease(m_VB2);
  SafeRelease(m_IB);
end;

procedure TG2Render2D.OnDeviceReset;
begin
  VerifyBuffer(m_VBSize);
  VerifyBuffer2(m_VB2Size);
  VerifyIndices(m_IBSize);
end;

function TG2Render2D.LoadBuffers: TG2Result;
var
  i: Integer;
  Vertices: PVertexArray;
  Indices: PG2Index16Array;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyBuffer(m_CurPos)) then Exit;
  if (m_CurTex < m_CurPos) then
  begin
    if Length(m_TexArr) < Length(m_PosArr) then
    SetLength(m_TexArr, Length(m_PosArr));
    FillChar(m_TexArr[m_CurTex], (m_CurPos - m_CurTex) * 8, 0);
  end;
  if (m_CurCol < m_CurPos) then
  begin
    if Length(m_ColArr) < Length(m_PosArr) then
    SetLength(m_ColArr, Length(m_PosArr));
    FillChar(m_ColArr[m_CurCol], (m_CurPos - m_CurCol) * 4, $ff);
  end;

  hr := m_VB.Lock(0, SizeOf(TVertex) * m_CurPos, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  try
    for i := 0 to m_CurPos - 1 do
    begin
      Move(m_PosArr[i].x, Vertices^[i].x, 8);
      Vertices^[i].z := 0;
      Vertices^[i].rhw := 1;
      Vertices^[i].Color := m_ColArr[i];
      Move(m_TexArr[i].x, Vertices^[i].tu, 8);
    end;
  finally
    hr := m_VB.Unlock;
  end;
  if Failed(hr) then Exit;

  if m_CurInd > 0 then
  begin
    if G2ResFail(VerifyIndices(m_CurInd)) then Exit;
    hr := m_IB.Lock(0, m_CurInd, Pointer(Indices), D3DLOCK_DISCARD);
    if Failed(hr) then Exit;
    try
      Move(m_IndArr[0], Indices^[0], m_CurInd * 2);
      m_Gfx.Device.SetIndices(m_IB);
    finally
      hr := m_IB.Unlock;
    end;
    if Failed(hr) then Exit;
  end;

  hr := m_Gfx.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render2D.DrawPointList: TG2Result;
var
  hr: HResult;
begin
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_POINTLIST, 0, m_CurPos);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render2D.DrawLineList: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := m_Gfx.Device.DrawIndexedPrimitive(D3DPT_LINELIST, 0, 0, m_CurPos, 0, m_CurInd div 2)
  else
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_LINELIST, 0, m_CurPos div 2);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render2D.DrawLineStrip: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := m_Gfx.Device.DrawIndexedPrimitive(D3DPT_LINESTRIP, 0, 0, m_CurPos, 0, m_CurInd - 1)
  else
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_LINESTRIP, 0, m_CurPos - 1);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render2D.DrawTriangleList: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := m_Gfx.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, m_CurPos, 0, m_CurInd div 3)
  else
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_TRIANGLELIST, 0, m_CurPos div 3);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render2D.DrawTriangleStrip: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := m_Gfx.Device.DrawIndexedPrimitive(D3DPT_TRIANGLESTRIP, 0, 0, m_CurPos, 0, m_CurInd - 2)
  else
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, m_CurPos - 2);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render2D.DrawTriangleFan: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := m_Gfx.Device.DrawIndexedPrimitive(D3DPT_TRIANGLEFAN, 0, 0, m_CurPos, 0, m_CurInd - 2)
  else
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_TRIANGLEFAN, 0, m_CurPos - 2);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render2D.DrawQuadImmediate(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
var
  Vertices: PVertexArray;
  hr: HResult;
  PrevTexture: IDirect3DBaseTexture9;
begin
  Result := grFail;
  if G2ResFail(VerifyBuffer(4)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  Vertices^[0].x := v1.X; Vertices^[0].y := v1.Y; Vertices^[0].z := 0;
  Vertices^[0].rhw := 1; Vertices^[0].Color := c1;
  Vertices^[0].tu := tc1.X; Vertices^[0].tv := tc1.Y;

  Vertices^[1].x := v2.X; Vertices^[1].y := v2.Y; Vertices^[1].z := 0;
  Vertices^[1].rhw := 1; Vertices^[1].Color := c2;
  Vertices^[1].tu := tc2.X; Vertices^[1].tv := tc2.Y;

  Vertices^[2].x := v3.X; Vertices^[2].y := v3.Y; Vertices^[2].z := 0;
  Vertices^[2].rhw := 1; Vertices^[2].Color := c3;
  Vertices^[2].tu := tc3.X; Vertices^[2].tv := tc3.Y;

  Vertices^[3].x := v4.X; Vertices^[3].y := v4.Y; Vertices^[3].z := 0;
  Vertices^[3].rhw := 1; Vertices^[3].Color := c4;
  Vertices^[3].tu := tc4.X; Vertices^[3].tv := tc4.Y;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;

  m_Gfx.Device.GetTexture(0, PrevTexture);
  if Assigned(Texture) then
  begin
    if PrevTexture <> Texture.Texture then
    hr := m_Gfx.Device.SetTexture(0, Texture.Texture);
  end
  else
  hr := m_Gfx.Device.SetTexture(0, nil);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render2D.DrawQuadBatched(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
var
  q, v: Integer;
begin
  if (m_CurTexture <> Texture) then
  begin
    BatchFlush;
    if Assigned(Texture) then
    m_Gfx.Device.SetTexture(0, Texture.Texture)
    else
    m_Gfx.Device.SetTexture(0, nil);
    m_CurTexture := Texture;
  end;

  q := m_CurQuad * 4;
  v := q;
  m_Vertices[v].x := v1.X; m_Vertices[v].y := v1.Y; m_Vertices[v].z := 0;
  m_Vertices[v].rhw := 1; m_Vertices[v].Color := c1;
  m_Vertices[v].tu := tc1.X; m_Vertices[v].tv := tc1.Y;
  v := q + 1;
  m_Vertices[v].x := v2.X; m_Vertices[v].y := v2.Y; m_Vertices[v].z := 0;
  m_Vertices[v].rhw := 1; m_Vertices[v].Color := c2;
  m_Vertices[v].tu := tc2.X; m_Vertices[v].tv := tc2.Y;
  v := q + 2;
  m_Vertices[v].x := v3.X; m_Vertices[v].y := v3.Y; m_Vertices[v].z := 0;
  m_Vertices[v].rhw := 1; m_Vertices[v].Color := c3;
  m_Vertices[v].tu := tc3.X; m_Vertices[v].tv := tc3.Y;
  v := q + 3;
  m_Vertices[v].x := v4.X; m_Vertices[v].y := v4.Y; m_Vertices[v].z := 0;
  m_Vertices[v].rhw := 1; m_Vertices[v].Color := c4;
  m_Vertices[v].tu := tc4.X; m_Vertices[v].tv := tc4.Y;

  Inc(m_CurQuad);
  if (m_CurQuad >= m_MaxQuads) then
  BatchFlush;
  Result := grOk;
end;

function TG2Render2D.DrawQuadRawImmediate(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
var
  Vertices: PVertexArray;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyBuffer(4)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  Vertices^[0].x := v1.X; Vertices^[0].y := v1.Y; Vertices^[0].z := 0;
  Vertices^[0].rhw := 1; Vertices^[0].Color := c1;
  Vertices^[0].tu := tc1.X; Vertices^[0].tv := tc1.Y;

  Vertices^[1].x := v2.X; Vertices^[1].y := v2.Y; Vertices^[1].z := 0;
  Vertices^[1].rhw := 1; Vertices^[1].Color := c2;
  Vertices^[1].tu := tc2.X; Vertices^[1].tv := tc2.Y;

  Vertices^[2].x := v3.X; Vertices^[2].y := v3.Y; Vertices^[2].z := 0;
  Vertices^[2].rhw := 1; Vertices^[2].Color := c3;
  Vertices^[2].tu := tc3.X; Vertices^[2].tv := tc3.Y;

  Vertices^[3].x := v4.X; Vertices^[3].y := v4.Y; Vertices^[3].z := 0;
  Vertices^[3].rhw := 1; Vertices^[3].Color := c4;
  Vertices^[3].tu := tc4.X; Vertices^[3].tv := tc4.Y;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render2D.DrawQuadRawBatched(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
var
  q, v: Integer;
begin
  q := m_CurQuad * 4;
  v := q;
  m_Vertices[v].x := v1.X; m_Vertices[v].y := v1.Y; m_Vertices[v].z := 0;
  m_Vertices[v].rhw := 1; m_Vertices[v].Color := c1;
  m_Vertices[v].tu := tc1.X; m_Vertices[v].tv := tc1.Y;
  v := q + 1;
  m_Vertices[v].x := v2.X; m_Vertices[v].y := v2.Y; m_Vertices[v].z := 0;
  m_Vertices[v].rhw := 1; m_Vertices[v].Color := c2;
  m_Vertices[v].tu := tc2.X; m_Vertices[v].tv := tc2.Y;
  v := q + 2;
  m_Vertices[v].x := v3.X; m_Vertices[v].y := v3.Y; m_Vertices[v].z := 0;
  m_Vertices[v].rhw := 1; m_Vertices[v].Color := c3;
  m_Vertices[v].tu := tc3.X; m_Vertices[v].tv := tc3.Y;
  v := q + 3;
  m_Vertices[v].x := v4.X; m_Vertices[v].y := v4.Y; m_Vertices[v].z := 0;
  m_Vertices[v].rhw := 1; m_Vertices[v].Color := c4;
  m_Vertices[v].tu := tc4.X; m_Vertices[v].tv := tc4.Y;

  Inc(m_CurQuad);
  if (m_CurQuad >= m_MaxQuads) then
  BatchFlush;
  Result := grOk;
end;

function TG2Render2D.DrawQuad(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
begin
  Result := m_DrawQuadFunc(
    v1, v2, v3, v4,
    c1, c2, c3, c4,
    Texture,
    tc1, tc2, tc3, tc4
  );
end;

function TG2Render2D.DrawRect(
      const X, Y: Single;
      const Texture: TG2Texture2DBase
    ): TG2Result;
  var x1, y1, x2, y2: Single;
begin
  x1 := x - 0.5;
  y1 := y - 0.5;
  x2 := x + Texture.Width - 1 + 0.5;
  y2 := y + Texture.Height - 1 + 0.5;
  Result := DrawQuad(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    $ffffffff, $ffffffff, $ffffffff, $ffffffff,
    Texture,
    G2Vec2(Texture.DrawRect^.Left, Texture.DrawRect^.Top),
    G2Vec2(Texture.DrawRect^.Right, Texture.DrawRect^.Top),
    G2Vec2(Texture.DrawRect^.Left, Texture.DrawRect^.Bottom),
    G2Vec2(Texture.DrawRect^.Right, Texture.DrawRect^.Bottom)
  );
end;

function TG2Render2D.DrawRect(
      const R: TRect;
      const Texture: TG2Texture2DBase
    ): TG2Result;
  var x1, y1, x2, y2: Single;
begin
  x1 := R.Left - 0.5;
  y1 := R.Top - 0.5;
  x2 := R.Right + 0.5;
  y2 := R.Bottom + 0.5;
  Result := DrawQuad(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    $ffffffff, $ffffffff, $ffffffff, $ffffffff,
    Texture,
    G2Vec2(Texture.DrawRect^.Left, Texture.DrawRect^.Top),
    G2Vec2(Texture.DrawRect^.Right, Texture.DrawRect^.Top),
    G2Vec2(Texture.DrawRect^.Left, Texture.DrawRect^.Bottom),
    G2Vec2(Texture.DrawRect^.Right, Texture.DrawRect^.Bottom)
  );
end;

function TG2Render2D.DrawRect(
      const X, Y, Width, Height: Single;
      const TexRect: TRect;
      const Texture: TG2Texture2DBase
    ): TG2Result;
  var r: TG2Rect;
  var x1, y1, x2, y2: Single;
begin
  x1 := x - 0.5;
  y1 := y - 0.5;
  x2 := x + Width - 1 + 0.5;
  y2 := y + Height - 1 + 0.5;
  r.Left := (TexRect.Left) / Texture.RealWidth;
  r.Top := (TexRect.Top) / Texture.RealHeight;
  r.Right := (TexRect.Right) / Texture.RealWidth;
  r.Bottom := (TexRect.Bottom) / Texture.RealHeight;
  Result := DrawQuad(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    $ffffffff, $ffffffff, $ffffffff, $ffffffff,
    Texture,
    G2Vec2(r.Left, r.Top),
    G2Vec2(r.Right, r.Top),
    G2Vec2(r.Left, r.Bottom),
    G2Vec2(r.Right, r.Bottom)
  );
end;

function TG2Render2D.DrawRect(
      const X, Y, Width, Height: Single;
      const TexRect: TRect;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase
    ): TG2Result;
  var r: TG2Rect;
  var x1, y1, x2, y2: Single;
begin
  x1 := x - 0.5;
  y1 := y - 0.5;
  x2 := x + Width - 1 + 0.5;
  y2 := y + Height - 1 + 0.5;
  r.Left := (TexRect.Left) / Texture.RealWidth;
  r.Top := (TexRect.Top) / Texture.RealHeight;
  r.Right := (TexRect.Right) / Texture.RealWidth;
  r.Bottom := (TexRect.Bottom) / Texture.RealHeight;
  Result := DrawQuad(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    Color, Color, Color, Color,
    Texture,
    G2Vec2(r.Left, r.Top),
    G2Vec2(r.Right, r.Top),
    G2Vec2(r.Left, r.Bottom),
    G2Vec2(r.Right, r.Bottom)
  );
end;

function TG2Render2D.DrawRect(
      const X, Y, Width, Height: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase
    ): TG2Result;
  var x1, y1, x2, y2: Single;
begin
  x1 := x - 0.5;
  y1 := y - 0.5;
  x2 := x + Width - 1 + 0.5;
  y2 := y + Height - 1 + 0.5;
  Result := DrawQuad(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    Color, Color, Color, Color,
    Texture,
    G2Vec2(Texture.DrawRect^.Left, Texture.DrawRect^.Top),
    G2Vec2(Texture.DrawRect^.Right, Texture.DrawRect^.Top),
    G2Vec2(Texture.DrawRect^.Left, Texture.DrawRect^.Bottom),
    G2Vec2(Texture.DrawRect^.Right, Texture.DrawRect^.Bottom)
  );
end;

function TG2Render2D.DrawRect(
      const X, Y, Width, Height: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const Rotation: Single;
      const ScaleX, ScaleY: Single;
      const FlipLeftRight: Boolean;
      const FlipTopBottom: Boolean
    ): TG2Result;
var
  v1, v2, v3, v4: TG2Vec2;
  tc1, tc2, tc3, tc4: TG2Vec2;
  m, mr, ms: TG2Mat2;
  hw, hh: Single;
  VShift: TG2Vec2;
begin
  ms.SetScaling(ScaleX, ScaleY);
  mr.SetRotation(Rotation);
  m := ms * mr;
  hw := (Width - 0.5) * 0.5;
  hh := (Height - 0.5) * 0.5;
  VShift := G2Vec2(hw + X - 0.25, hh + Y - 0.25);
  v1 := G2Vec2(-hw, -hh) * m + VShift;
  v2 := G2Vec2(hw, -hh) * m + VShift;
  v3 := G2Vec2(-hw, hh) * m + VShift;
  v4 := G2Vec2(hw, hh) * m + VShift;
  if FlipLeftRight then
  begin
    tc1.X := Texture.DrawRect^.Right; tc2.X := Texture.DrawRect^.Left;
    tc3.X := Texture.DrawRect^.Right; tc4.X := Texture.DrawRect^.Left;
  end
  else
  begin
    tc1.X := Texture.DrawRect^.Left; tc2.X := Texture.DrawRect^.Right;
    tc3.X := Texture.DrawRect^.Left; tc4.X := Texture.DrawRect^.Right;
  end;
  if FlipTopBottom then
  begin
    tc1.Y := Texture.DrawRect^.Bottom; tc3.Y := Texture.DrawRect^.Top;
    tc2.Y := Texture.DrawRect^.Bottom; tc4.Y := Texture.DrawRect^.Top;
  end
  else
  begin
    tc1.Y := Texture.DrawRect^.Top; tc3.Y := Texture.DrawRect^.Bottom;
    tc2.Y := Texture.DrawRect^.Top; tc4.Y := Texture.DrawRect^.Bottom;
  end;
  Result := DrawQuad(
    v1, v2, v3, v4,
    Color, Color, Color, Color,
    Texture,
    tc1, tc2, tc3, tc4
  );
end;

function TG2Render2D.DrawRect(
      const X, Y, Width, Height: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const Rotation: Single;
      const ScaleX, ScaleY: Single;
      const FlipLeftRight: Boolean;
      const FlipTopBottom: Boolean;
      const PatternWidth: Integer;
      const PatternHeight: Integer;
      const PatternFrame: Integer
    ): TG2Result;
var
  v1, v2, v3, v4: TG2Vec2;
  tc1, tc2, tc3, tc4: TG2Vec2;
  m, mr, ms: TG2Mat2;
  hw, hh, tu, tv: Single;
  VShift: TG2Vec2;
  TexRect: TG2Rect;
  pc, py, px: Integer;
begin
  ms.SetScaling(ScaleX, ScaleY);
  mr.SetRotation(Rotation);
  m := ms * mr;
  hw := (Width) * 0.5;
  hh := (Height) * 0.5;
  VShift := G2Vec2(hw + X - 0.5, hh + Y - 0.5);
  v1 := G2Vec2(-hw, -hh) * m + VShift;
  v2 := G2Vec2(hw, -hh) * m + VShift;
  v3 := G2Vec2(-hw, hh) * m + VShift;
  v4 := G2Vec2(hw, hh) * m + VShift;
  tu := PatternWidth / Texture.RealWidth;
  tv := PatternHeight / Texture.RealHeight;
  pc := Texture.Width div PatternWidth;
  px := PatternFrame mod pc;
  py := PatternFrame div pc;
  TexRect := G2Rect(px * tu, py * tv, px * tu + tu, py * tv + tv);
  if FlipLeftRight then
  begin
    tc1.X := TexRect.Right; tc2.X := TexRect.Left;
    tc3.X := TexRect.Right; tc4.X := TexRect.Left;
  end
  else
  begin
    tc1.X := TexRect.Left; tc2.X := TexRect.Right;
    tc3.X := TexRect.Left; tc4.X := TexRect.Right;
  end;
  if FlipTopBottom then
  begin
    tc1.Y := TexRect.Bottom; tc3.Y := TexRect.Top;
    tc2.Y := TexRect.Bottom; tc4.Y := TexRect.Top;
  end
  else
  begin
    tc1.Y := TexRect.Top; tc3.Y := TexRect.Bottom;
    tc2.Y := TexRect.Top; tc4.Y := TexRect.Bottom;
  end;
  Result := DrawQuad(
    v1, v2, v3, v4,
    Color, Color, Color, Color,
    Texture,
    tc1, tc2, tc3, tc4
  );
end;

function TG2Render2D.DrawQuad2(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture0: TG2Texture2DBase;
      const t0c1, t0c2, t0c3, t0c4: TG2Vec2;
      const Texture1: TG2Texture2DBase;
      const t1c1, t1c2, t1c3, t1c4: TG2Vec2
    ): TG2Result;
var
  Vertices: PVertex2Array;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyBuffer2(4)) then Exit;
  hr := m_VB2.Lock(0, SizeOf(TVertex2) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  Vertices^[0].x := v1.X; Vertices^[0].y := v1.Y; Vertices^[0].z := 0;
  Vertices^[0].rhw := 1; Vertices^[0].Color := c1;
  Vertices^[0].tu1 := t0c1.X; Vertices^[0].tv1 := t0c1.Y;
  Vertices^[0].tu2 := t1c1.X; Vertices^[0].tv2 := t1c1.Y;

  Vertices^[1].x := v2.X; Vertices^[1].y := v2.Y; Vertices^[0].z := 0;
  Vertices^[1].rhw := 1; Vertices^[1].Color := c2;
  Vertices^[1].tu1 := t0c2.X; Vertices^[1].tv1 := t0c2.Y;
  Vertices^[1].tu2 := t1c2.X; Vertices^[1].tv2 := t1c2.Y;

  Vertices^[2].x := v3.X; Vertices^[2].y := v3.Y; Vertices^[0].z := 0;
  Vertices^[2].rhw := 1; Vertices^[2].Color := c3;
  Vertices^[2].tu1 := t0c3.X; Vertices^[2].tv1 := t0c3.Y;
  Vertices^[2].tu2 := t1c3.X; Vertices^[2].tv2 := t1c3.Y;

  Vertices^[3].x := v4.X; Vertices^[3].y := v4.Y; Vertices^[0].z := 0;
  Vertices^[3].rhw := 1; Vertices^[3].Color := c4;
  Vertices^[3].tu1 := t0c4.X; Vertices^[3].tv1 := t0c4.Y;
  Vertices^[3].tu2 := t1c4.X; Vertices^[3].tv2 := t1c4.Y;
  hr := m_VB2.Unlock;
  if Failed(hr) then Exit;

  hr := m_Gfx.Device.SetTexture(0, Texture0.Texture);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetTexture(1, Texture1.Texture);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetFVF(FVF2);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetStreamSource(0, m_VB2, 0, SizeOf(TVertex2));
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render2D.DrawQuad3(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const Texture0: TG2Texture2DBase;
      const t0c1, t0c2, t0c3, t0c4: TG2Vec2;
      const Texture1: TG2Texture2DBase;
      const t1c1, t1c2, t1c3, t1c4: TG2Vec2;
      const Texture2: TG2Texture2DBase;
      const t2c1, t2c2, t2c3, t2c4: TG2Vec2
    ): TG2Result;
var
  Vertices: PVertex3Array;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyBuffer3(4)) then Exit;
  hr := m_VB3.Lock(0, SizeOf(TVertex3) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  Vertices^[0].x := v1.X; Vertices^[0].y := v1.Y; Vertices^[0].z := 0;
  Vertices^[0].rhw := 1; Vertices^[0].Color := c1;
  Vertices^[0].tu1 := t0c1.X; Vertices^[0].tv1 := t0c1.Y;
  Vertices^[0].tu2 := t1c1.X; Vertices^[0].tv2 := t1c1.Y;

  Vertices^[1].x := v2.X; Vertices^[1].y := v2.Y; Vertices^[0].z := 0;
  Vertices^[1].rhw := 1; Vertices^[1].Color := c2;
  Vertices^[1].tu1 := t0c2.X; Vertices^[1].tv1 := t0c2.Y;
  Vertices^[1].tu2 := t1c2.X; Vertices^[1].tv2 := t1c2.Y;

  Vertices^[2].x := v3.X; Vertices^[2].y := v3.Y; Vertices^[0].z := 0;
  Vertices^[2].rhw := 1; Vertices^[2].Color := c3;
  Vertices^[2].tu1 := t0c3.X; Vertices^[2].tv1 := t0c3.Y;
  Vertices^[2].tu2 := t1c3.X; Vertices^[2].tv2 := t1c3.Y;

  Vertices^[3].x := v4.X; Vertices^[3].y := v4.Y; Vertices^[0].z := 0;
  Vertices^[3].rhw := 1; Vertices^[3].Color := c4;
  Vertices^[3].tu1 := t0c4.X; Vertices^[3].tv1 := t0c4.Y;
  Vertices^[3].tu2 := t1c4.X; Vertices^[3].tv2 := t1c4.Y;
  hr := m_VB3.Unlock;
  if Failed(hr) then Exit;

  hr := m_Gfx.Device.SetTexture(0, Texture0.Texture);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetTexture(1, Texture1.Texture);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetTexture(2, Texture2.Texture);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetFVF(FVF3);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetStreamSource(0, m_VB3, 0, SizeOf(TVertex3));
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render2D.DrawQuadRaw(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color;
      const tc1, tc2, tc3, tc4: TG2Vec2
    ): TG2Result;
begin
  Result := m_DrawQuadRawFunc(
    v1, v2, v3, v4,
    c1, c2, c3, c4,
    tc1, tc2, tc3, tc4
  );
end;

function TG2Render2D.DrawBegin(const PrimType: TG2PrimType): TG2Result;
begin
  if m_Drawing then
  begin
    Result := grInvalidCall;
    Exit;
  end;
  m_CurPos := 0;
  m_CurCol := 0;
  m_CurTex := 0;
  m_CurInd := 0;
  m_Drawing := True;
  m_PrimType := PrimType;
  Result := grOk;
end;

procedure TG2Render2D.AddPos(const p: TG2Vec2);
begin
  if High(m_PosArr) < m_CurPos then
  SetLength(m_PosArr, Length(m_PosArr) * 2);
  m_PosArr[m_CurPos] := p;
  Inc(m_CurPos);
end;

procedure TG2Render2D.AddPos(const x, y: Single);
begin
  if High(m_PosArr) < m_CurPos then
  SetLength(m_PosArr, Length(m_PosArr) * 2);
  m_PosArr[m_CurPos].SetValue(x, y);
  Inc(m_CurPos);
end;

procedure TG2Render2D.AddCol(const c: TG2Color);
begin
  if High(m_ColArr) < m_CurCol then
  SetLength(m_ColArr, Length(m_ColArr) * 2);
  m_ColArr[m_CurCol] := c;
  Inc(m_CurCol);
end;

procedure TG2Render2D.AddCol(const c: TG2Color; const Count: Integer);
var
  i: Integer;
  cnt: Integer;
begin
  if Count = 0 then
  cnt := m_CurPos - m_CurCol
  else
  cnt := Count;
  if cnt <= 0 then Exit;
  while High(m_ColArr) < m_CurCol + (cnt - 1) do
  SetLength(m_ColArr, Length(m_ColArr) * 2);
  for i := 0 to cnt - 1 do
  m_ColArr[m_CurCol + i] := c;
  Inc(m_CurCol, cnt);
end;

procedure TG2Render2D.AddTex(const t: TG2Vec2);
begin
  if High(m_TexArr) < m_CurTex then
  SetLength(m_TexArr, Length(m_TexArr) * 2);
  m_TexArr[m_CurTex] := t;
  Inc(m_CurTex);
end;

procedure TG2Render2D.AddInd(const i: Word);
begin
  if High(m_IndArr) < m_CurInd then
  SetLength(m_IndArr, Length(m_IndArr) * 2);
  m_IndArr[m_CurInd] := i + m_BaseVertexIndex;
  Inc(m_CurInd);
end;

procedure TG2Render2D.AddFace(const i0, i1, i2: Word);
begin
  while High(m_IndArr) < m_CurInd + 2 do
  SetLength(m_IndArr, Length(m_IndArr) * 2);
  m_IndArr[m_CurInd] := i0 + m_BaseVertexIndex;
  m_IndArr[m_CurInd + 1] := i1 + m_BaseVertexIndex;
  m_IndArr[m_CurInd + 2] := i2 + m_BaseVertexIndex;
  Inc(m_CurInd, 3);
end;

function TG2Render2D.DrawEnd: TG2Result;
begin
  if not m_Drawing then
  begin
    Result := grInvalidCall;
    Exit;
  end;

  m_Drawing := False;

  Result := LoadBuffers;
  if G2ResOk(Result) then
  case m_PrimType of
    ptPointList: Result := DrawPointList;
    ptLineList: Result := DrawLineList;
    ptLineStrip: Result := DrawLineStrip;
    ptTriangleList: Result := DrawTriangleList;
    ptTriangleStrip: Result := DrawTriangleStrip;
    ptTriangleFan: Result := DrawTriangleFan;
  end;
end;

function TG2Render2D.BatchBegin(const MaxQuads: Integer = 1000): TG2Result;
var
  i, Ind, Ver: Integer;
  Indices: PWordArray;
  hr: HResult;
begin
  if m_Batching or (MaxQuads <= 0) then
  begin
    Result := grInvalidCall;
    Exit;
  end;
  Result := VerifyBuffer(MaxQuads * 4);
  if G2ResFail(Result) then Exit;
  if Length(m_Vertices) < MaxQuads * 4 then
  SetLength(m_Vertices, MaxQuads * 4);

  if MaxQuads > m_MaxQuads then
  begin
    m_IBBatch.Verify(MaxQuads * 6, D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_MANAGED);
    m_IBBatch.Lock(0, MaxQuads * 6 * SizeOf(Word), Pointer(Indices), 0);
    for i := m_MaxQuads to MaxQuads - 1 do
    begin
      Ind := i * 6;
      Ver := i * 4;
      Indices^[Ind + 0] := Ver + 0;
      Indices^[Ind + 1] := Ver + 1;
      Indices^[Ind + 2] := Ver + 2;
      Indices^[Ind + 3] := Ver + 2;
      Indices^[Ind + 4] := Ver + 1;
      Indices^[Ind + 5] := Ver + 3;
    end;
    m_IBBatch.UnLock;
  end;

  m_IBBatch.SetToDevice;
  hr := m_Gfx.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := m_Gfx.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;

  m_CurQuad := 0;
  m_BatchDrawCalls := 0;
  m_CurTexture := nil;
  m_MaxQuads := MaxQuads;

  m_DrawQuadFunc := DrawQuadBatched;
  m_DrawQuadRawFunc := DrawQuadRawBatched;
  m_Batching := True;
  Result := grOk;
end;

function TG2Render2D.BatchEnd: TG2Result;
begin
  if not m_Batching then
  begin
    Result := grInvalidCall;
    Exit;
  end;

  BatchFlush;
  m_DrawQuadFunc := DrawQuadImmediate;
  m_DrawQuadRawFunc := DrawQuadRawImmediate;
  m_Gfx.Device.SetIndices(nil);
  m_Batching := False;
  Result := grOk;
end;

function TG2Render2D.BatchFlush: TG2Result;
var
  VPtr: Pointer;
  VCount: DWord;
begin
  if not m_Batching then
  begin
    Result := grInvalidCall;
    Exit;
  end;
  if m_CurQuad <= 0 then
  begin
    Result := grRedundantCall;
    Exit;
  end;

  VCount := m_CurQuad * 4;
  m_VB.Lock(0, VCount * SizeOf(TVertex), VPtr, D3DLOCK_DISCARD);
  Move(m_Vertices[0], VPtr^, VCount * SizeOf(TVertex));
  m_VB.Unlock;
  m_Gfx.Device.DrawIndexedPrimitive(
    D3DPT_TRIANGLELIST,
    0,
    0, VCount,
    0, m_CurQuad * 2
  );
  m_CurQuad := 0;
  Inc(m_BatchDrawCalls);
  Result := grOk;
end;

function TG2Render2D.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Gfx := Core.Graphics;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  VerifyBuffer(4);
  VerifyBuffer2(4);
  VerifyBuffer3(4);
  VerifyIndices(6);
  SetLength(m_PosArr, 4);
  SetLength(m_ColArr, 4);
  SetLength(m_TexArr, 4);
  SetLength(m_IndArr, 4);
  m_MaxQuads := 0;
  m_CurQuad := 0;
  m_BatchDrawCalls := 0;
  m_CurTexture := nil;
  m_DrawQuadFunc := DrawQuadImmediate;
  m_DrawQuadRawFunc := DrawQuadRawImmediate;
  m_IBBatch := TG2IB.Create;
  m_IBBatch.Initialize(Core);
  m_Batching := False;
  m_BaseVertexIndex := 0;
  Result := grOk;
end;

function TG2Render2D.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  m_IBBatch.Free;
  SafeRelease(m_VB3);
  SafeRelease(m_VB2);
  SafeRelease(m_VB);
  Result := grOk;
end;
//TG2Render2D END

//TG2Render3D BEGIN
constructor TG2Render3D.Create;
begin
  inherited Create;
  m_VBSize := 0;
  m_IBSize := 0;
end;

destructor TG2Render3D.Destroy;
begin
  inherited Destroy;
end;

function TG2Render3D.VerifyVB(const Size: Integer): TG2Result;
begin
  if (Size <= m_VBSize)
  and Assigned(m_VB) then
  begin
    Result := grOk;
    Exit;
  end;
  m_VBSize := Size;
  SafeRelease(m_VB);
  if Failed(
    Core.Graphics.Device.CreateVertexBuffer(
      m_VBSize * SizeOf(TVertex),
      D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
      FVF,
      D3DPOOL_DEFAULT,
      m_VB,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;
  Result := grOk;
end;

function TG2Render3D.VerifyVB2(const Size: Integer): TG2Result;
begin
  if (Size <= m_VB2Size)
  and Assigned(m_VB2) then
  begin
    Result := grOk;
    Exit;
  end;
  m_VB2Size := Size;
  SafeRelease(m_VB2);
  if Failed(
    Core.Graphics.Device.CreateVertexBuffer(
      m_VB2Size * SizeOf(TVertex2),
      D3DUSAGE_WRITEONLY,
      FVF2,
      D3DPOOL_DEFAULT,
      m_VB2,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;
  Result := grOk;
end;

function TG2Render3D.VerifyIB(const Size: Integer): TG2Result;
begin
  if (Size <= m_IBSize)
  and Assigned(m_IB) then
  begin
    Result := grOk;
    Exit;
  end;
  m_IBSize := Size;
  SafeRelease(m_IB);
  if Failed(
    Core.Graphics.Device.CreateIndexBuffer(
      m_IBSize * SizeOf(Word),
      D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
      D3DFMT_INDEX16,
      D3DPOOL_DEFAULT,
      m_IB,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;
  Result := grOk;
end;

function TG2Render3D.AddVertex(
      const X, Y, Z: Single;
      const Color: TG2Color;
      const NX, NY, NZ: Single;
      const TU: Single = 0;
      const TV: Single = 0
    ): Word;
begin
  m_Vertices^[m_CurV].x := X;
  m_Vertices^[m_CurV].y := Y;
  m_Vertices^[m_CurV].z := Z;
  m_Vertices^[m_CurV].Color := Color;
  m_Vertices^[m_CurV].nx := NX;
  m_Vertices^[m_CurV].ny := NY;
  m_Vertices^[m_CurV].nz := NZ;
  m_Vertices^[m_CurV].tu := TU;
  m_Vertices^[m_CurV].tv := TV;
  Result := m_CurV;
  Inc(m_CurV);
end;

procedure TG2Render3D.AddFace(
      const v1, v2, v3: Word
    );
begin
  m_Indices^[m_CurI] := v1;
  m_Indices^[m_CurI + 1] := v2;
  m_Indices^[m_CurI + 2] := v3;
  Inc(m_CurI, 3);
end;

procedure TG2Render3D.OnDeviceLost;
begin
  SafeRelease(m_VB);
  SafeRelease(m_VB2);
  SafeRelease(m_IB);
end;

procedure TG2Render3D.OnDeviceReset;
begin
  VerifyVB(m_VBSize);
  VerifyVB2(m_VB2Size);
  VerifyIB(m_IBSize);
end;

function TG2Render3D.LoadBuffers: TG2Result;
var
  i: Integer;
  Vertices: PVertexArray;
  Indices: PG2Index16Array;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(m_CurPos)) then Exit;
  if (m_CurTex < m_CurPos) then
  begin
    if Length(m_TexArr) < Length(m_PosArr) then
    SetLength(m_TexArr, Length(m_PosArr));
    FillChar(m_TexArr[m_CurTex], (m_CurPos - m_CurTex) * 8, 0);
  end;
  if (m_CurCol < m_CurPos) then
  begin
    if Length(m_ColArr) < Length(m_PosArr) then
    SetLength(m_ColArr, Length(m_PosArr));
    FillChar(m_ColArr[m_CurCol], (m_CurPos - m_CurCol) * 4, $ff);
  end;
  if (m_CurNrm < m_CurPos) then
  begin
    if Length(m_NrmArr) < Length(m_PosArr) then
    SetLength(m_NrmArr, Length(m_PosArr));
    FillChar(m_NrmArr[m_CurNrm], (m_CurPos - m_CurNrm) * 12, 0);
  end;

  hr := m_VB.Lock(0, SizeOf(TVertex) * m_CurPos, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  try
    for i := 0 to m_CurPos - 1 do
    begin
      Move(m_PosArr[i].x, Vertices^[i].x, 12);
      Vertices^[i].Color := m_ColArr[i];
      Move(m_TexArr[i].x, Vertices^[i].tu, 8);
      Move(m_NrmArr[i].x, Vertices^[i].nx, 12);
    end;
  finally
    hr := m_VB.Unlock;
  end;
  if Failed(hr) then Exit;

  if m_CurInd > 0 then
  begin
    if G2ResFail(VerifyIB(m_CurInd)) then Exit;
    hr := m_IB.Lock(0, m_CurInd, Pointer(Indices), D3DLOCK_DISCARD);
    if Failed(hr) then Exit;
    try
      Move(m_IndArr[0], Indices^[0], m_CurInd * 2);
      Core.Graphics.Device.SetIndices(m_IB);
    finally
      hr := m_IB.Unlock;
    end;
    if Failed(hr) then Exit;
  end;

  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawPointList: TG2Result;
var
  hr: HResult;
begin
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_POINTLIST, 0, m_CurPos);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render3D.DrawLineList: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_LINELIST, 0, 0, m_CurPos, 0, m_CurInd div 2)
  else
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_LINELIST, 0, m_CurPos div 2);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render3D.DrawLineStrip: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_LINESTRIP, 0, 0, m_CurPos, 0, m_CurInd - 1)
  else
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_LINESTRIP, 0, m_CurPos - 1);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render3D.DrawTriangleList: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, m_CurPos, 0, m_CurInd div 3)
  else
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLELIST, 0, m_CurPos div 3);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render3D.DrawTriangleStrip: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLESTRIP, 0, 0, m_CurPos, 0, m_CurInd - 2)
  else
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, m_CurPos - 2);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render3D.DrawTriangleFan: TG2Result;
var
  hr: HResult;
begin
  if m_CurInd > 0 then
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLEFAN, 0, 0, m_CurPos, 0, m_CurInd - 2)
  else
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLEFAN, 0, m_CurPos - 2);
  if Failed(hr) then Result := grFail else Result := grOk;
end;

function TG2Render3D.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  VerifyVB(256);
  VerifyIB(1024);
  SetLength(m_PosArr, 4);
  SetLength(m_ColArr, 4);
  SetLength(m_TexArr, 4);
  SetLength(m_NrmArr, 4);
  SetLength(m_IndArr, 4);
  m_BaseVertexIndex := 0;
  Result := grOk;
end;

function TG2Render3D.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;

function TG2Render3D.DrawTriangle(
      const v1, v2, v3: TG2Vec3;
      const Color: TG2Color;
      const tc1, tc2, tc3: TG2Vec2
    ): TG2Result;
begin
  Result := DrawTriangle3Col(
    v1, v2, v3, Color, Color, Color, tc1, tc2, tc3
  );
end;

function TG2Render3D.DrawTriangle3Col(
      const v1, v2, v3: TG2Vec3;
      const c1, c2, c3: TG2Color;
      const tc1, tc2, tc3: TG2Vec2
    ): TG2Result;
var
  hr: HResult;
  n: TG2Vec3;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(3)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * 3, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  n := G2TriangleNormal(v1, v2, v3);

  m_Vertices^[0].x := v1.X; m_Vertices^[0].y := v1.Y; m_Vertices^[0].z := v1.Z;
  m_Vertices^[0].nx := n.X; m_Vertices^[0].ny := n.Y; m_Vertices^[0].nz := n.Z;
  m_Vertices^[0].tu := tc1.X; m_Vertices^[0].tv := tc1.Y;
  m_Vertices^[0].Color := c1;

  m_Vertices^[1].x := v2.X; m_Vertices^[1].y := v2.Y; m_Vertices^[1].z := v2.Z;
  m_Vertices^[1].nx := n.X; m_Vertices^[1].ny := n.Y; m_Vertices^[1].nz := n.Z;
  m_Vertices^[1].tu := tc2.X; m_Vertices^[1].tv := tc2.Y;
  m_Vertices^[1].Color := c2;

  m_Vertices^[2].x := v3.X; m_Vertices^[2].y := v3.Y; m_Vertices^[2].z := v3.Z;
  m_Vertices^[2].nx := n.X; m_Vertices^[2].ny := n.Y; m_Vertices^[2].nz := n.Z;
  m_Vertices^[2].tu := tc3.X; m_Vertices^[2].tv := tc3.Y;
  m_Vertices^[2].Color := c3;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLELIST, 0, 1);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawQuad(
      const v1, v2, v3, v4: TG2Vec3;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const TextureRect: PG2Rect = nil
    ): TG2Result;
begin
  Result := DrawQuad4Col(
    v1, v2, v3, v4,
    Color, Color, Color, Color,
    Texture, TextureRect
  );
end;

function TG2Render3D.DrawQuad4Col(
      const v1, v2, v3, v4: TG2Vec3;
      const c1, c2, c3, c4: TG2Color;
      const Texture: TG2Texture2DBase;
      const TextureRect: PG2Rect = nil
    ): TG2Result;
var
  hr: HResult;
  n1, n2, n3: TG2Vec3;
  tu0, tv0, tu1, tv1: Single;
  tr: PG2Rect;
  r: TG2Rect;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(4)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * 4, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  if Assigned(Texture) then
  Core.Graphics.Device.SetTexture(0, Texture.Texture);
  if Assigned(TextureRect) then
  tr := TextureRect
  else
  begin
    if Assigned(Texture) then
    tr := Texture.DrawRect
    else
    begin
      r := G2Rect(0, 0, 1, 1);
      tr := @r;
    end;
  end;

  n1 := G2TriangleNormal(v1, v2, v3);
  n2 := G2TriangleNormal(v3, v2, v4);
  n3 := (n1 + n2);
  n3.Normalize;

  tu0 := tr^.Left;
  tv0 := tr^.Top;
  tu1 := tr^.Right;
  tv1 := tr^.Bottom;

  m_Vertices^[0].x := v1.X; m_Vertices^[0].y := v1.Y; m_Vertices^[0].z := v1.Z;
  m_Vertices^[0].nx := n1.X; m_Vertices^[0].ny := n1.Y; m_Vertices^[0].nz := n1.Z;
  m_Vertices^[0].tu := tu0; m_Vertices^[0].tv := tv0;
  m_Vertices^[0].Color := c1;

  m_Vertices^[1].x := v2.X; m_Vertices^[1].y := v2.Y; m_Vertices^[1].z := v2.Z;
  m_Vertices^[1].nx := n3.X; m_Vertices^[1].ny := n3.Y; m_Vertices^[1].nz := n3.Z;
  m_Vertices^[1].tu := tu1; m_Vertices^[1].tv := tv0;
  m_Vertices^[1].Color := c2;

  m_Vertices^[2].x := v3.X; m_Vertices^[2].y := v3.Y; m_Vertices^[2].z := v3.Z;
  m_Vertices^[2].nx := n3.X; m_Vertices^[2].ny := n3.Y; m_Vertices^[2].nz := n3.Z;
  m_Vertices^[2].tu := tu0; m_Vertices^[2].tv := tv1;
  m_Vertices^[2].Color := c3;

  m_Vertices^[3].x := v4.X; m_Vertices^[3].y := v4.Y; m_Vertices^[3].z := v4.Z;
  m_Vertices^[3].nx := n2.X; m_Vertices^[3].ny := n2.Y; m_Vertices^[3].nz := n2.Z;
  m_Vertices^[3].tu := tu1; m_Vertices^[3].tv := tv1;
  m_Vertices^[3].Color := c4;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawQuad2(
      const v1, v2, v3, v4: TG2Vec3;
      const Color: TG2Color;
      const Texture0: TG2Texture2DBase;
      const TextureRect0: PG2Rect = nil;
      const Texture1: TG2Texture2DBase = nil;
      const TextureRect1: PG2Rect = nil
    ): TG2Result;
var
  hr: HResult;
  n1, n2, n3: TG2Vec3;
  t0u0, t0v0, t0u1, t0v1: Single;
  t1u0, t1v0, t1u1, t1v1: Single;
  tr0, tr1: PG2Rect;
  r: TG2Rect;
begin
  Result := grFail;
  if G2ResFail(VerifyVB2(4)) then Exit;
  hr := m_VB2.Lock(0, SizeOf(TVertex2) * 4, Pointer(m_Vertices2), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  if Assigned(Texture0) then
  Core.Graphics.Device.SetTexture(0, Texture0.Texture);
  if Assigned(Texture1) then
  Core.Graphics.Device.SetTexture(1, Texture1.Texture);
  r := G2Rect(0, 0, 1, 1);
  if Assigned(TextureRect0) then
  tr0 := TextureRect0
  else
  begin
    if Assigned(Texture0) then
    tr0 := Texture0.DrawRect
    else
    tr0 := @r;
  end;
  if Assigned(TextureRect1) then
  tr1 := TextureRect1
  else
  begin
    if Assigned(Texture1) then
    tr1 := Texture1.DrawRect
    else
    tr1 := @r;
  end;

  n1 := G2TriangleNormal(v1, v2, v3);
  n2 := G2TriangleNormal(v3, v2, v4);
  n3 := (n1 + n2);
  n3.Normalize;

  t0u0 := tr0^.Left;
  t0v0 := tr0^.Top;
  t0u1 := tr0^.Right;
  t0v1 := tr0^.Bottom;

  t1u0 := tr1^.Left;
  t1v0 := tr1^.Top;
  t1u1 := tr1^.Right;
  t1v1 := tr1^.Bottom;

  m_Vertices2^[0].x := v1.X; m_Vertices2^[0].y := v1.Y; m_Vertices2^[0].z := v1.Z;
  m_Vertices2^[0].nx := n1.X; m_Vertices2^[0].ny := n1.Y; m_Vertices2^[0].nz := n1.Z;
  m_Vertices2^[0].tu0 := t0u0; m_Vertices2^[0].tv0 := t0v0;
  m_Vertices2^[0].tu1 := t1u0; m_Vertices2^[0].tv1 := t1v0;
  m_Vertices2^[0].Color := Color;

  m_Vertices2^[1].x := v2.X; m_Vertices2^[1].y := v2.Y; m_Vertices2^[1].z := v2.Z;
  m_Vertices2^[1].nx := n3.X; m_Vertices2^[1].ny := n3.Y; m_Vertices2^[1].nz := n3.Z;
  m_Vertices2^[1].tu0 := t0u1; m_Vertices2^[1].tv0 := t0v0;
  m_Vertices2^[1].tu1 := t1u1; m_Vertices2^[1].tv1 := t1v0;
  m_Vertices2^[1].Color := Color;

  m_Vertices2^[2].x := v3.X; m_Vertices2^[2].y := v3.Y; m_Vertices2^[2].z := v3.Z;
  m_Vertices2^[2].nx := n3.X; m_Vertices2^[2].ny := n3.Y; m_Vertices2^[2].nz := n3.Z;
  m_Vertices2^[2].tu0 := t0u0; m_Vertices2^[2].tv0 := t0v1;
  m_Vertices2^[2].tu1 := t1u0; m_Vertices2^[2].tv1 := t1v1;
  m_Vertices2^[2].Color := Color;

  m_Vertices2^[3].x := v4.X; m_Vertices2^[3].y := v4.Y; m_Vertices2^[3].z := v4.Z;
  m_Vertices2^[3].nx := n2.X; m_Vertices2^[3].ny := n2.Y; m_Vertices2^[3].nz := n2.Z;
  m_Vertices2^[3].tu0 := t0u1; m_Vertices2^[3].tv0 := t0v1;
  m_Vertices2^[3].tu1 := t1u1; m_Vertices2^[3].tv1 := t1v1;
  m_Vertices2^[3].Color := Color;

  hr := m_VB2.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF2);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB2, 0, SizeOf(TVertex2));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawBox(
      const X, Y, Z: Single;
      const SizeX, SizeY, SizeZ: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase
    ): TG2Result;
var
  hr: HResult;
  MaxV: TG2Vec3;
  tu0, tv0, tu1, tv1: Single;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(24)) then Exit;
  if G2ResFail(VerifyIB(36)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * 24, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * 36, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  MaxV := G2Vec3(X + SizeX, Y + SizeY, Z + SizeZ);

  tu0 := Texture.DrawRect^.Left;
  tv0 := Texture.DrawRect^.Top;
  tu1 := Texture.DrawRect^.Right;
  tv1 := Texture.DrawRect^.Bottom;

  m_CurV := 0;
  m_CurI := 0;

  AddVertex(X, Y, Z, Color, 0, -1, 0, tu0, tv0);
  AddVertex(MaxV.X, Y, Z, Color, 0, -1, 0, tu1, tv0);
  AddVertex(X, Y, MaxV.Z, Color, 0, -1, 0, tu0, tv1);
  AddVertex(MaxV.X, Y, MaxV.Z, Color, 0, -1, 0, tu1, tv1);
  AddFace(0, 1, 2); AddFace(2, 1, 3);

  AddVertex(X, MaxV.Y, MaxV.Z, Color, 0, 1, 0, tu0, tv0);
  AddVertex(MaxV.X, MaxV.Y, MaxV.Z, Color, 0, 1, 0, tu1, tv0);
  AddVertex(X, MaxV.Y, Z, Color, 0, 1, 0, tu0, tv1);
  AddVertex(MaxV.X, MaxV.Y, Z, Color, 0, 1, 0, tu1, tv1);
  AddFace(4, 5, 6); AddFace(6, 5, 7);

  AddVertex(X, MaxV.Y, Z, Color, 0, 0, -1, tu0, tv0);
  AddVertex(MaxV.X, MaxV.Y, Z, Color, 0, 0, -1, tu1, tv0);
  AddVertex(X, Y, Z, Color, 0, 0, -1, tu0, tv1);
  AddVertex(MaxV.X, Y, Z, Color, 0, 0, -1, tu1, tv1);
  AddFace(8, 9, 10); AddFace(10, 9, 11);

  AddVertex(MaxV.X, MaxV.Y, MaxV.Z, Color, 0, 0, 1, tu0, tv0);
  AddVertex(X, MaxV.Y, MaxV.Z, Color, 0, 0, 1, tu1, tv0);
  AddVertex(MaxV.X, Y, MaxV.Z, Color, 0, 0, 1, tu0, tv1);
  AddVertex(X, Y, MaxV.Z, Color, 0, 0, 1, tu1, tv1);
  AddFace(12, 13, 14); AddFace(14, 13, 15);

  AddVertex(X, MaxV.Y, MaxV.Z, Color, -1, 0, 0, tu0, tv0);
  AddVertex(X, MaxV.Y, Z, Color, -1, 0, 0, tu1, tv0);
  AddVertex(X, Y, MaxV.Z, Color, -1, 0, 0, tu0, tv1);
  AddVertex(X, Y, Z, Color, -1, 0, 0, tu1, tv1);
  AddFace(16, 17, 18); AddFace(18, 17, 19);

  AddVertex(MaxV.X, MaxV.Y, Z, Color, 1, 0, 0, tu0, tv0);
  AddVertex(MaxV.X, MaxV.Y, MaxV.Z, Color, 1, 0, 0, tu1, tv0);
  AddVertex(MaxV.X, Y, Z, Color, 1, 0, 0, tu0, tv1);
  AddVertex(MaxV.X, Y, MaxV.Z, Color, 1, 0, 0, tu1, tv1);
  AddFace(20, 21, 22); AddFace(22, 21, 23);

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, 24, 0, 12);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawSphere(
      const X, Y, Z: Single;
      const Radius: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const VSegments: Integer = 32;
      const HSegments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i, j: Integer;
  g1, g2, gv, gh: Extended;
  v: Single;
  s, c: Extended;
  LastV: Word;
  VCount: Integer;
  ICount: Integer;
  v1, v2, v3, v4: Word;
  n: TG2Vec3;
  tu0, tv0, tu1, tv1: Single;
begin
  Result := grFail;
  VCount := (VSegments + 1) * (HSegments);
  ICount := VSegments * (HSegments - 1) * 6;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  tu0 := Texture.DrawRect^.Left;
  tv0 := Texture.DrawRect^.Top;
  tu1 := Texture.DrawRect^.Right;
  tv1 := Texture.DrawRect^.Bottom;

  m_CurV := 0;
  m_CurI := 0;

  for j := 0 to VSegments - 1 do
  begin
    g1 := j / (VSegments - 1);
    SinCos(g1 * Pi, gh, gv);
    for i := 0 to HSegments do
    begin
      g2 := i / HSegments;
      SinCos(g2 * TwoPi, s, c);
      AddVertex(
        X + s * Radius * gh,
        Y + Radius * gv,
        Z + c * Radius * gh,
        Color,
        s * gh, gv, c * gh,
        G2LerpFloat(tu0, tu1, g2),
        G2LerpFloat(tv0, tv1, g1)
      );
    end;
  end;
  for j := 0 to VSegments - 2 do
  for i := 0 to HSegments - 1 do
  begin
    v1 := (j + 0) * (HSegments + 1) + (i + 0);
    v2 := (j + 0) * (HSegments + 1) + (i + 1);
    v3 := (j + 1) * (HSegments + 1) + (i + 0);
    v4 := (j + 1) * (HSegments + 1) + (i + 1);
    AddFace(v1, v3, v2); AddFace(v3, v4, v2);
  end;

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, m_CurV, 0, m_CurI div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawCylinder(
      const X, Y, Z: Single;
      const Height, Radius: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const Segments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i, j: Integer;
  g, gv, gh: Extended;
  v: Single;
  s, c: Extended;
  TopV, BottomV: Word;
  VCount: Integer;
  ICount: Integer;
  v1, v2, v3, v4: Word;
  n: TG2Vec3;
  vx, vz: Single;
  tur, tvr, tum, tvm: Single;
  tu0, tv0, tu1, tv1: Single;
begin
  Result := grFail;
  if Segments < 3 then Exit;
  VCount := (Segments + 1) * 4 + 2;
  ICount := Segments * 12;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  m_CurV := 0;
  m_CurI := 0;

  tu0 := Texture.DrawRect^.Left;
  tv0 := Texture.DrawRect^.Top;
  tu1 := Texture.DrawRect^.Right;
  tv1 := Texture.DrawRect^.Bottom;
  tur := (tu1 - tu0) * 0.5;
  tvr := (tv1 - tv0) * 0.5;
  tum := tu0 + Texture.DrawRect^.Left;
  tvm := tv0 + Texture.DrawRect^.Top;

  for i := 0 to Segments do
  begin
    g := i / Segments;
    SinCos(g * TwoPi, s, c);
    vx := X + s * Radius;
    vz := Z + c * Radius;
    AddVertex(vx, Y + Height, vz, Color, 0, 1, 0, tum + c * tur, tvm + s * tvr);
    AddVertex(vx, Y + Height, vz, Color, s, 0, c, G2LerpFloat(tu0, tu1, g), tv0);
    AddVertex(vx, Y, vz, Color, s, 0, c, G2LerpFloat(tu0, tu1, g), tv1);
    AddVertex(vx, Y, vz, Color, 0, -1, 0, tum + s * tur, tvm + c * tvr);
  end;
  TopV := AddVertex(X, Y + Height, Z, Color, 0, 1, 0, tum, tvm);
  BottomV := AddVertex(X, Y, Z, Color, 0, -1, 0, tum, tvm);
  for i := 0 to Segments - 1 do
  begin
    AddFace(TopV, ((i + 0) * 4 + 0), ((i + 1) * 4 + 0));
    AddFace(((i + 0) * 4 + 1), ((i + 0) * 4 + 2), ((i + 1) * 4 + 1));
    AddFace(((i + 0) * 4 + 2), ((i + 1) * 4 + 2), ((i + 1) * 4 + 1));
    AddFace(BottomV, ((i + 1) * 4 + 3), ((i + 0) * 4 + 3));
  end;

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, m_CurV, 0, m_CurI div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawCapsule(
      const X, Y, Z: Single;
      const Height, Radius: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const VCapSegments: Integer = 8;
      const HSegments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i, j, v: Integer;
  g1, g2, gv, gh: Extended;
  s, c: Extended;
  TopV, BottomV: Word;
  VCount: Integer;
  ICount: Integer;
  n: TG2Vec3;
  vx, vz: Single;
  HalfHeight: Single;
  TotalHeight: Single;
  tu0, tv0, tu1, tv1: Single;
  tvc0, tvc1: Single;
begin
  Result := grFail;
  if (VCapSegments < 1) or (HSegments < 3) then Exit;
  VCount := (VCapSegments) * (HSegments + 1) * 2;
  ICount := 6 * HSegments * (VCapSegments * 2 - 1);
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  tu0 := Texture.DrawRect^.Left;
  tv0 := Texture.DrawRect^.Top;
  tu1 := Texture.DrawRect^.Right;
  tv1 := Texture.DrawRect^.Bottom;

  m_CurV := 0;
  m_CurI := 0;

  HalfHeight := Height * 0.5;
  TotalHeight := Height + Radius * 2;
  tvc0 := Radius / TotalHeight;
  tvc1 := (Radius + Height) / TotalHeight;

  for i := 0 to HSegments do
  begin
    g1 := i / HSegments;
    SinCos(g1 * TwoPi, s, c);
    for j := 0 to VCapSegments - 1 do
    begin
      g2 := j / (VCapSegments - 1);
      SinCos(g2 * HalfPi, gh, gv);
      vx := X + s * Radius * gh;
      vz := Z + c * Radius * gh;
      n := G2Vec3(s * gh, gv, c * gh);
      AddVertex(
        vx,
        Y + HalfHeight + Radius * gv,
        vz,
        Color,
        n.x, n.y, n.z,
        G2LerpFloat(tu0, tu1, g1),
        G2LerpFloat(tv0, tvc0, g2)
      );
      AddVertex(
        vx,
        Y - HalfHeight - Radius * gv,
        vz,
        Color,
        n.x, -n.y, n.z,
        G2LerpFloat(tu0, tu1, g1),
        G2LerpFloat(tv1, tvc1, g2)
      );
    end;
  end;
  v := (VCapSegments) * 2;
  for i := 0 to HSegments - 1 do
  begin
    AddFace(i * v + v - 2, i * v + v - 1, (i + 1) * v + v - 2);
    AddFace((i + 1) * v + v - 1, (i + 2) * v - 2, i * v + v - 1);
    for j := 0 to VCapSegments - 2 do
    begin
      AddFace(i * v + j * 2, (i + 1) * v + (j + 1) * 2, (i + 1) * v + j * 2);
      AddFace((i + 1) * v + (j + 1) * 2, i * v + j * 2, i * v + (j + 1) * 2);
      AddFace(i * v + j * 2 + 1, (i + 1) * v + j * 2 + 1, (i + 1) * v + (j + 1) * 2 + 1);
      AddFace((i + 1) * v + (j + 1) * 2 + 1, i * v + (j + 1) * 2 + 1, i * v + j * 2 + 1);
    end;
  end;

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, m_CurV, 0, m_CurI div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawTorus(
      const X, Y, Z: Single;
      const RadiusInt, RadiusExt: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const Sides: Integer = 16;
      const Segments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i, j: Integer;
  g1, g2, gv, gh: Extended;
  s, c, s1, c1: Extended;
  TopV, BottomV: Word;
  VCount: Integer;
  ICount: Integer;
  n: TG2Vec3;
  vx, vz: Single;
  v, v1, v2: TG2Vec3;
  fv1, fv2, fv3, fv4: Word;
  HalfHeight: Single;
  Radius1, Radius2: Single;
  tu0, tv0, tu1, tv1: Single;
begin
  Result := grFail;
  if (Sides < 3) or (Segments < 3) then Exit;
  VCount := (Segments + 1) * (Sides + 1);
  ICount := Sides * Segments * 6;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  tu0 := Texture.DrawRect^.Left;
  tv0 := Texture.DrawRect^.Top;
  tu1 := Texture.DrawRect^.Right;
  tv1 := Texture.DrawRect^.Bottom;

  m_CurV := 0;
  m_CurI := 0;

  Radius1 := (RadiusExt - RadiusInt) * 0.5;
  Radius2 := RadiusInt + Radius1;

  for i := 0 to Segments do
  begin
    g1 := i / Segments;
    SinCos(g1 * TwoPi, s, c);
    v := G2Vec3(X + s * Radius2, Y, Z + c * Radius2);
    for j := 0 to Sides do
    begin
      g2 := j / Sides;
      SinCos(g2 * TwoPi, s1, c1);
      n := G2Vec3(s * c1, s1, c * c1);
      v1 := v + n * Radius1;
      AddVertex(v1.x, v1.Y, v1.z, Color, n.x, n.y, n.z, G2LerpFloat(tu0, tu1, g1), G2LerpFloat(tv0, tv1, g2));
    end;
  end;
  for i := 0 to Segments - 1 do
  for j := 0 to Sides - 1 do
  begin
    fv1 := i * (Sides + 1) + j;
    fv2 := (i + 1) * (Sides + 1) + j;
    fv3 := i * (Sides + 1) + (j + 1);
    fv4 := (i + 1) * (Sides + 1) + (j + 1);
    AddFace(fv1, fv2, fv3);
    AddFace(fv3, fv2, fv4);
  end;

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, m_CurV, 0, m_CurI div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawAxisRect(
      const Pos0, Pos1: TG2Vec3;
      const Rad0, Rad1: Single;
      const Color: TG2Color;
      const Texture: TG2Texture2DBase;
      const TextureRect: PG2Rect = nil
    ): TG2Result;
var
  hr: HResult;
  c, n0, n1, n: TG2Vec3;
  v0, v1, v2, v3: TG2Vec3;
  tu0, tv0, tu1, tv1: Single;
  tr: PG2Rect;
  r: TG2Rect;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(4)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TVertex) * 4, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  if Assigned(Texture) then
  Core.Graphics.Device.SetTexture(0, Texture.Texture);
  if Assigned(TextureRect) then
  tr := TextureRect
  else
  begin
    if Assigned(Texture) then
    tr := Texture.DrawRect
    else
    begin
      r := G2Rect(0, 0, 1, 1);
      tr := @r;
    end;
  end;
  tu0 := tr^.Left;
  tv0 := tr^.Top;
  tu1 := tr^.Right;
  tv1 := tr^.Bottom;

  n := Pos0 - Pos1;
  c := Core.Graphics.Transforms.Vpos;
  n0 := (c - Pos0).Cross(n).Normalized;
  n := n.Cross(n0).Normalized;
  n1 := n0 * Rad1;
  n0 := n0 * Rad0;
  v0 := Pos0 - n0;
  v1 := Pos0 + n0;
  v2 := Pos1 - n1;
  v3 := Pos1 + n1;

  m_Vertices^[0].x := v0.X; m_Vertices^[0].y := v0.Y; m_Vertices^[0].z := v0.Z;
  m_Vertices^[0].nx := n.X; m_Vertices^[0].ny := n.Y; m_Vertices^[0].nz := n.Z;
  m_Vertices^[0].tu := tu0; m_Vertices^[0].tv := tv0;
  m_Vertices^[0].Color := Color;

  m_Vertices^[1].x := v1.X; m_Vertices^[1].y := v1.Y; m_Vertices^[1].z := v1.Z;
  m_Vertices^[1].nx := n.X; m_Vertices^[1].ny := n.Y; m_Vertices^[1].nz := n.Z;
  m_Vertices^[1].tu := tu1; m_Vertices^[1].tv := tv0;
  m_Vertices^[1].Color := Color;

  m_Vertices^[2].x := v2.X; m_Vertices^[2].y := v2.Y; m_Vertices^[2].z := v2.Z;
  m_Vertices^[2].nx := n.X; m_Vertices^[2].ny := n.Y; m_Vertices^[2].nz := n.Z;
  m_Vertices^[2].tu := tu0; m_Vertices^[2].tv := tv1;
  m_Vertices^[2].Color := Color;

  m_Vertices^[3].x := v3.X; m_Vertices^[3].y := v3.Y; m_Vertices^[3].z := v3.Z;
  m_Vertices^[3].nx := n.X; m_Vertices^[3].ny := n.Y; m_Vertices^[3].nz := n.Z;
  m_Vertices^[3].tu := tu1; m_Vertices^[3].tv := tv1;
  m_Vertices^[3].Color := Color;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TVertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Render3D.DrawBegin(const PrimType: TG2PrimType): TG2Result;
begin
  if m_Drawing then
  begin
    Result := grInvalidCall;
    Exit;
  end;
  m_CurPos := 0;
  m_CurCol := 0;
  m_CurTex := 0;
  m_CurNrm := 0;
  m_CurInd := 0;
  m_BaseVertexIndex := 0;
  m_Drawing := True;
  m_PrimType := PrimType;
  Result := grOk;
end;

procedure TG2Render3D.AddPos(const p: TG2Vec3);
begin
  if High(m_PosArr) < m_CurPos then
  SetLength(m_PosArr, Length(m_PosArr) * 2);
  m_PosArr[m_CurPos] := p;
  Inc(m_CurPos);
end;

procedure TG2Render3D.AddPos(const x, y, z: Single);
begin
  if High(m_PosArr) < m_CurPos then
  SetLength(m_PosArr, Length(m_PosArr) * 2);
  m_PosArr[m_CurPos].x := x;
  m_PosArr[m_CurPos].y := y;
  m_PosArr[m_CurPos].z := z;
  Inc(m_CurPos);
end;

procedure TG2Render3D.AddCol(const c: TG2Color);
begin
  if High(m_ColArr) < m_CurCol then
  SetLength(m_ColArr, Length(m_ColArr) * 2);
  m_ColArr[m_CurCol] := c;
  Inc(m_CurCol);
end;

procedure TG2Render3D.AddCol(const c: TG2Color; const Count: Integer);
var
  i: Integer;
begin
  while High(m_ColArr) < m_CurCol + (Count - 1) do
  SetLength(m_ColArr, Length(m_ColArr) * 2);
  for i := 0 to Count - 1 do
  m_ColArr[m_CurCol + i] := c;
  Inc(m_CurCol, Count);
end;

procedure TG2Render3D.AddTex(const t: TG2Vec2);
begin
  if High(m_TexArr) < m_CurTex then
  SetLength(m_TexArr, Length(m_TexArr) * 2);
  m_TexArr[m_CurTex] := t;
  Inc(m_CurTex);
end;

procedure TG2Render3D.AddTex(const u, v: Single);
begin
  if High(m_TexArr) < m_CurTex then
  SetLength(m_TexArr, Length(m_TexArr) * 2);
  m_TexArr[m_CurTex].x := u;
  m_TexArr[m_CurTex].y := v;
  Inc(m_CurTex);
end;

procedure TG2Render3D.AddNrm(const n: TG2Vec3);
begin
  if High(m_NrmArr) < m_CurNrm then
  SetLength(m_NrmArr, Length(m_NrmArr) * 2);
  m_NrmArr[m_CurNrm] := n;
  Inc(m_CurNrm);
end;

procedure TG2Render3D.AddNrm(const x, y, z: Single);
begin
  if High(m_NrmArr) < m_CurNrm then
  SetLength(m_NrmArr, Length(m_NrmArr) * 2);
  m_NrmArr[m_CurNrm].x := x;
  m_NrmArr[m_CurNrm].y := y;
  m_NrmArr[m_CurNrm].z := z;
  Inc(m_CurNrm);
end;

procedure TG2Render3D.AddInd(const i: Word);
begin
  if High(m_IndArr) < m_CurInd then
  SetLength(m_IndArr, Length(m_IndArr) * 2);
  m_IndArr[m_CurInd] := i + m_BaseVertexIndex;
  Inc(m_CurInd);
end;

function TG2Render3D.DrawEnd: TG2Result;
begin
  if not m_Drawing then
  begin
    Result := grInvalidCall;
    Exit;
  end;

  m_Drawing := False;

  Result := LoadBuffers;
  if G2ResOk(Result) then
  case m_PrimType of
    ptPointList: Result := DrawPointList;
    ptLineList: Result := DrawLineList;
    ptLineStrip: Result := DrawLineStrip;
    ptTriangleList: Result := DrawTriangleList;
    ptTriangleStrip: Result := DrawTriangleStrip;
    ptTriangleFan: Result := DrawTriangleFan;
  end;
end;
//TG2Render3D END

//TG2Camera BEGIN
constructor TG2Camera.Create;
begin
  inherited Create;
end;

destructor TG2Camera.Destroy;
begin
  inherited Destroy;
end;

function TG2Camera.GetView: TG2Mat;
begin
  Result.SetView(
    m_Src, m_Dst, m_Up
  );
end;

function TG2Camera.GetProj: TG2Mat;
begin
  Result.SetPerspective(
    m_FOV, m_Aspect, m_Near, m_Far
  );
end;

procedure TG2Camera.AnglesFromVectors;
var
  Dir: TG2Vec3;
begin
  Dir := m_Dst - m_Src;
  Dir.Normalize;
  m_AngH := G2Vec2AngleOX(G2Vec2(0, 0), G2Vec2(Dir.x, Dir.z));
  m_AngV := ArcSin(Dir.y);
end;

procedure TG2Camera.VectorsFromAngles;
var
  Dir: TG2Vec3;
  sh, ch, sv, cv, sv2, cv2: Extended;
  l: Single;
begin
  SinCos(m_AngH, sh, ch);
  SinCos(m_AngV, sv, cv);
  SinCos(m_AngV + HalfPi, sv2, cv2);
  l := (m_Dst - m_Src).Len;
  Dir.x := ch * cv; Dir.y := sv; Dir.z := sh * cv;
  if m_DragMode = cdDragDir then
  m_Dst := m_Src + Dir * l
  else
  m_Src := m_Dst - Dir * l;
  m_Up.x := ch * cv2;
  m_Up.y := sv2;
  m_Up.z := sh * cv2;
end;

procedure TG2Camera.OnMouseMove(const Shift: TPoint);
  const Threshold = 0.1;
  const MaxAng = HalfPi - Threshold;
  var CamSpaceX, CamSpaceY, CamSpaceZ: TG2Vec3;
  var CamShift: TG2Vec3;
  var MinA, MaxA: Single;
begin
  if m_UpdateDir then
  begin
    AnglesFromVectors;
    m_AngH := m_AngH - Shift.x * m_Sensitivity * 0.01;
    m_AngV := m_AngV - Shift.y * m_Sensitivity * 0.01;
    MinA := Max(-MaxAng, m_MinAngV);
    MaxA := Min(MaxAng, m_MaxAngV);
    if m_AngV < MinA then m_AngV := MinA;
    if m_AngV > MaxA then m_AngV := MaxA;
    VectorsFromAngles;
  end;
  if m_DragMode = cdStrafe then
  begin
    CamSpaceY := m_Up.Normalized;
    CamSpaceZ := (m_Dst - m_Src).Normalized;
    CamSpaceX := CamSpaceZ.Cross(CamSpaceY).Normalized;
    CamSpaceY := CamSpaceX.Cross(CamSpaceZ).Normalized;
    CamShift := CamSpaceY * (Shift.Y * m_StrafeSpeed) + CamSpaceX * (Shift.X * m_StrafeSpeed);
    m_Src := m_Src + CamShift;
    m_Dst := m_Dst + CamShift;
  end;
end;

function TG2Camera.GetPos: PG2Vec3;
begin
  Result := @m_Src;
end;

function TG2Camera.GetTarget: PG2Vec3;
begin
  Result := @m_Dst;
end;

function TG2Camera.GetUp: PG2Vec3;
begin
  Result := @m_Up;
end;

procedure TG2Camera.SetPerspective(const NewFOV, NewAspect, NewNear, NewFar: Single);
begin
  m_FOV := NewFOV;
  m_Aspect := NewAspect;
  m_Near := NewNear;
  m_Far := NewFar;
end;

procedure TG2Camera.SetPerspective;
begin
  m_FOV := QuatPi;
  m_Aspect := Core.Graphics.Params.Width / Core.Graphics.Params.Height;
  m_Near := 0.1;
  m_Far := 1000;
end;

procedure TG2Camera.SetView(const PosX, PosY, PosZ, TargetX, TargetY, TargetZ, UpX, UpY, UpZ: Single);
begin
  m_Src := G2Vec3(PosX, PosY, PosZ);
  m_Dst := G2Vec3(TargetX, TargetY, TargetZ);
  m_Up := G2Vec3(UpX, UpY, UpZ);
end;

procedure TG2Camera.SetView(const NewPos, NewTarget, NewUp: TG2Vec3);
begin
  m_Src := NewPos;
  m_Dst := NewTarget;
  m_Up := NewUp;
end;

procedure TG2Camera.Update;
var
  Dir: TG2Vec3;
  sh, sv, ch, cv: Extended;
begin
  if m_UpdatePos then
  begin
    AnglesFromVectors;
    Dir.SetValue(0, 0, 0);
    if m_PlugInput.KeyDown[m_BtnLeft] then
    begin
      SinCos(m_AngH + HalfPi, sh, ch);
      Dir.x := Dir.x + ch;
      Dir.z := Dir.z + sh;
    end;
    if m_PlugInput.KeyDown[m_BtnRight] then
    begin
      SinCos(m_AngH - HalfPi, sh, ch);
      Dir.x := Dir.x + ch;
      Dir.z := Dir.z + sh;
    end;
    if m_PlugInput.KeyDown[m_BtnForward] then
    begin
      if m_Mode = cmFree then
      begin
        SinCos(m_AngH, sh, ch);
        SinCos(m_AngV, sv, cv);
        Dir.x := Dir.x + ch * cv;
        Dir.y := Dir.y + sv;
        Dir.z := Dir.z + sh * cv;
      end
      else
      begin
        SinCos(m_AngH, sh, ch);
        Dir.x := Dir.x + ch;
        Dir.z := Dir.z + sh;
      end;
    end;
    if m_PlugInput.KeyDown[m_BtnBackward] then
    begin
      if m_Mode = cmFree then
      begin
        SinCos(m_AngH, sh, ch);
        SinCos(m_AngV + Pi, sv, cv);
        Dir.x := Dir.x + ch * cv;
        Dir.y := Dir.y + sv;
        Dir.z := Dir.z + sh * cv;
      end
      else
      begin
        SinCos(m_AngH + Pi, sh, ch);
        Dir.x := Dir.x + ch;
        Dir.z := Dir.z + sh;
      end
    end;
    Dir := Dir.Normalized * m_Speed;
    m_Src := m_Src + Dir;
    m_Dst := m_Dst + Dir;
  end;
end;

procedure TG2Camera.GetViewSpaceVectors(var VecX, VecY, VecZ: TG2Vec3);
begin
  VecY := m_Up.Normalized;
  VecZ := (m_Dst - m_Src).Normalized;
  VecX := VecZ.Cross(VecY).Normalized;
  VecY := VecX.Cross(VecZ).Normalized;
end;

procedure TG2Camera.MoveTo(const NewPos: TG2Vec3);
  var ShiftVec: TG2Vec3;
begin
  ShiftVec := NewPos - m_Src;
  m_Src := NewPos;
  m_Dst := m_Dst + ShiftVec;
end;

function TG2Camera.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugInput, @m_PlugInput);
  m_PlugInput.OnMouseMove := OnMouseMove;
  m_Speed := 1;
  m_Sensitivity := 1;
  m_StrafeSpeed := 0.001;
  m_BtnLeft := DIK_A;
  m_BtnRight := DIK_D;
  m_BtnForward := DIK_W;
  m_BtnBackward := DIK_S;
  m_AngH := 0;
  m_AngV := 0;
  m_UpdatePos := False;
  m_UpdateDir := False;
  m_Mode := cmFree;
  m_DragMode := cdDragDir;
  m_MaxAngV := HalfPi - 0.04;
  m_MinAngV := -m_MaxAngV;
  Result := grOk;
end;

function TG2Camera.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugInput);
  Result := grOk;
end;
//TG2Camera END

//TG2Primitives2D BEGIN
constructor TG2Primitives2D.Create;
begin
  inherited Create;
  m_VBSize := 0;
  m_IBSize := 0;
end;

destructor TG2Primitives2D.Destroy;
begin
  inherited Destroy;
end;

function TG2Primitives2D.VerifyIB(const Size: Integer): TG2Result;
begin
  if (Size <= m_IBSize)
  and Assigned(m_IB) then
  begin
    Result := grOk;
    Exit;
  end;
  m_IBSize := Size;
  SafeRelease(m_IB);
  if Failed(
    Core.Graphics.Device.CreateIndexBuffer(
      m_IBSize * 2,
      D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
      D3DFMT_INDEX16,
      D3DPOOL_DEFAULT,
      m_IB,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;
  Result := grOk;
end;

function TG2Primitives2D.VerifyVB(const Size: Integer): TG2Result;
begin
  if (Size <= m_VBSize)
  and Assigned(m_VB) then
  begin
    Result := grOk;
    Exit;
  end;
  m_VBSize := Size;
  SafeRelease(m_VB);
  if Failed(
    Core.Graphics.Device.CreateVertexBuffer(
      m_VBSize * SizeOf(TG2Vertex),
      D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
      FVF,
      D3DPOOL_DEFAULT,
      m_VB,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;
  Result := grOk;
end;

procedure TG2Primitives2D.OnDeviceLost;
begin
  SafeRelease(m_VB);
  SafeRelease(m_IB);
end;

procedure TG2Primitives2D.OnDeviceReset;
begin
  VerifyVB(m_VBSize);
  VerifyIB(m_IBSize);
end;

function TG2Primitives2D.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  VerifyVB(256);
  VerifyIB(1024);
  Result := grOk;
end;

function TG2Primitives2D.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;

function TG2Primitives2D.DrawLine(
      const v1, v2: TG2Vec2;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawLine2Col(v1, v2, Color, Color);
end;

function TG2Primitives2D.DrawLine2Col(
      const v1, v2: TG2Vec2;
      const c1, c2: TG2Color
    ): TG2Result;
var
  Vertices: PG2VertexArray;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(2)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 2, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  Vertices^[0].x := v1.X; Vertices^[0].y := v1.Y; Vertices^[0].z := 0;
  Vertices^[0].rhw := 1; Vertices^[0].Color := c1;

  Vertices^[1].x := v2.X; Vertices^[1].y := v2.Y; Vertices^[1].z := 0;
  Vertices^[1].rhw := 1; Vertices^[1].Color := c2;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_LINESTRIP, 0, 1);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives2D.DrawLineStrip(
      const VArr: PG2Vec2Array;
      const VCount: Word;
      const Color: TG2Color
    ): TG2Result;
var
  Vertices: PG2VertexArray;
  i: Integer;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  for i := 0 to VCount - 1 do
  begin
    Vertices^[i].x := VArr^[i].X; Vertices^[i].y := VArr^[i].Y; Vertices^[i].z := 0;
    Vertices^[i].rhw := 1; Vertices^[i].Color := Color;
  end;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_LINESTRIP, 0, VCount - 1);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives2D.DrawTriangle(
      const v1, v2, v3: TG2Vec2;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawTriangle3Col(
    v1, v2, v3,
    Color, Color, Color
  );
end;

function TG2Primitives2D.DrawTriangle3Col(
      const v1, v2, v3: TG2Vec2;
      const c1, c2, c3: TG2Color
    ): TG2Result;
var
  Vertices: PG2VertexArray;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(3)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 3, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  Vertices^[0].x := v1.X; Vertices^[0].y := v1.Y; Vertices^[0].z := 0;
  Vertices^[0].rhw := 1; Vertices^[0].Color := c1;

  Vertices^[1].x := v2.X; Vertices^[1].y := v2.Y; Vertices^[1].z := 0;
  Vertices^[1].rhw := 1; Vertices^[1].Color := c2;

  Vertices^[2].x := v3.X; Vertices^[2].y := v3.Y; Vertices^[2].z := 0;
  Vertices^[2].rhw := 1; Vertices^[2].Color := c3;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLELIST, 0, 1);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives2D.DrawRect(
      const X, Y, Width, Height: Single;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawRect4Col(
    X, Y, Width, Height,
    Color, Color, Color, Color
  );
end;

function TG2Primitives2D.DrawRect(
      const R: TRect;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawRect4Col(
    R,
    Color, Color, Color, Color
  );
end;

function TG2Primitives2D.DrawRect4Col(
      const X, Y, Width, Height: Single;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
var
  x1, y1, x2, y2: Single;
begin
  x1 := x - 0.5;
  y1 := y - 0.5;
  x2 := X + Width - 1 + 0.5;
  y2 := Y + Height - 1 + 0.5;
  Result := DrawQuad4Col(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    c1, c2, c3, c4
  );
end;

function TG2Primitives2D.DrawRect4Col(
      const R: TRect;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
  var x1, y1, x2, y2: Single;
begin
  x1 := R.Left - 0.5;
  y1 := R.Top - 0.5;
  x2 := R.Right + 0.5;
  y2 := R.Bottom + 0.5;
  Result := DrawQuad4Col(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    c1, c2, c3, c4
  );
end;

function TG2Primitives2D.DrawQuad(
      const v1, v2, v3, v4: TG2Vec2;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawQuad4Col(v1, v2, v3, v4, Color, Color, Color, Color);
end;

function TG2Primitives2D.DrawQuad4Col(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
var
  Vertices: PG2VertexArray;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(4)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  Vertices^[0].x := v1.X; Vertices^[0].y := v1.Y; Vertices^[0].z := 0;
  Vertices^[0].rhw := 0; Vertices^[0].Color := c1;

  Vertices^[1].x := v2.X; Vertices^[1].y := v2.Y; Vertices^[1].z := 0;
  Vertices^[1].rhw := 0; Vertices^[1].Color := c2;

  Vertices^[2].x := v3.X; Vertices^[2].y := v3.Y; Vertices^[2].z := 0;
  Vertices^[2].rhw := 0; Vertices^[2].Color := c3;

  Vertices^[3].x := v4.X; Vertices^[3].y := v4.Y; Vertices^[3].z := 0;
  Vertices^[3].rhw := 0; Vertices^[3].Color := c4;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives2D.DrawRectHollow(
      const X, Y, Width, Height: Single;
      const Color: TG2Color
    ): TG2Result;
  var x1, y1, x2, y2: Single;
begin
  x1 := x - 0.5;
  y1 := y - 0.5;
  x2 := x + Width - 1 + 0.5;
  y2 := y + Height - 1 + 0.5;
  Result := DrawQuadHollow(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    Color
  );
end;

function TG2Primitives2D.DrawRectHollow(
      const R: TRect;
      const Color: TG2Color
    ): TG2Result;
  var x1, y1, x2, y2: Single;
begin
  x1 := R.Left - 0.5;
  y1 := R.Top - 0.5;
  x2 := R.Right + 0.5;
  y2 := R.Bottom + 0.5;
  Result := DrawQuadHollow(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    Color
  );
end;

function TG2Primitives2D.DrawRectHollow4Col(
      const X, Y, Width, Height: Single;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
  var x1, y1, x2, y2: Single;
begin
  x1 := x - 0.5;
  y1 := y - 0.5;
  x2 := x + Width - 1 + 0.5;
  y2 := y + Height - 1 + 0.5;
  Result := DrawQuadHollow4Col(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    c1, c2, c3, c4
  );
end;

function TG2Primitives2D.DrawRectHollow4Col(
      const R: TRect;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
  var x1, y1, x2, y2: Single;
begin
  x1 := R.Left - 0.5;
  y1 := R.Top - 0.5;
  x2 := R.Right + 0.5;
  y2 := R.Bottom + 0.5;
  Result := DrawQuadHollow4Col(
    G2Vec2(x1, y1), G2Vec2(x2, y1),
    G2Vec2(x1, y2), G2Vec2(x2, y2),
    c1, c2, c3, c4
  );
end;

function TG2Primitives2D.DrawQuadHollow(
      const v1, v2, v3, v4: TG2Vec2;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawQuadHollow4Col(
    v1, v2, v3, v4,
    Color, Color, Color, Color
  );
end;

function TG2Primitives2D.DrawQuadHollow4Col(
      const v1, v2, v3, v4: TG2Vec2;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
var
  Vertices: PG2VertexArray;
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(5)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 5, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  Vertices^[0].x := v1.X; Vertices^[0].y := v1.Y; Vertices^[0].z := 0;
  Vertices^[0].rhw := 0; Vertices^[0].Color := c1;

  Vertices^[1].x := v2.X; Vertices^[1].y := v2.Y; Vertices^[1].z := 0;
  Vertices^[1].rhw := 0; Vertices^[1].Color := c2;

  Vertices^[2].x := v4.X; Vertices^[2].y := v4.Y; Vertices^[2].z := 0;
  Vertices^[2].rhw := 0; Vertices^[2].Color := c4;

  Vertices^[3].x := v3.X; Vertices^[3].y := v3.Y; Vertices^[3].z := 0;
  Vertices^[3].rhw := 0; Vertices^[3].Color := c3;

  Vertices^[4].x := v1.X; Vertices^[4].y := v1.Y; Vertices^[4].z := 0;
  Vertices^[4].rhw := 0; Vertices^[4].Color := c1;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_LINESTRIP, 0, 4);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives2D.DrawCircle(
      const Center: TG2Vec2;
      const Radius: Single;
      const Color: TG2Color;
      const Segments: Word = 32
    ): TG2Result;
begin
  Result := DrawCircle2Col(Center, Radius, Color, Color, Segments);
end;

function TG2Primitives2D.DrawCircle2Col(
      const Center: TG2Vec2;
      const Radius: Single;
      const ColInt, ColExt: TG2Color;
      const Segments: Word = 32
    ): TG2Result;
var
  Vertices: PG2VertexArray;
  i, v: Integer;
  s, c: Extended;
  hr: HResult;
  VCount: Integer;
begin
  Result := grFail;
  if Segments < 3 then Exit;
  VCount := Segments + 2;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  Vertices^[0].x := Center.X; Vertices^[0].y := Center.Y;
  Vertices^[0].z := 0; Vertices^[0].rhw := 1; Vertices^[0].Color := ColInt;
  for i := 0 to Segments do
  begin
    v := i + 1;
    SinCos((i / (Segments)) * TwoPi, s, c);
    Vertices^[v].x := c * Radius + Center.X; Vertices^[v].y := s * Radius + Center.Y;
    Vertices^[v].z := 0; Vertices^[v].rhw := 1; Vertices^[v].Color := ColExt;
  end;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLEFAN, 0, Segments);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives2D.DrawCircleHollow(
      const Center: TG2Vec2;
      const RadiusInt, RadiusExt: Single;
      const Color: TG2Color;
      const Segments: Word = 32
    ): TG2Result;
begin
  Result := DrawCircleHollow2Col(Center, RadiusInt, RadiusExt, Color, Color, Segments);
end;

function TG2Primitives2D.DrawCircleHollow2Col(
      const Center: TG2Vec2;
      const RadiusInt, RadiusExt: Single;
      const ColInt, ColExt: TG2Color;
      const Segments: Word = 32
    ): TG2Result;
var
  Vertices: PG2VertexArray;
  i, v1, v2: Integer;
  s, c: Extended;
  hr: HResult;
  VCount: Integer;
begin
  Result := grFail;
  if Segments < 3 then Exit;
  VCount := (Segments + 1) * 2;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  for i := 0 to Segments do
  begin
    v1 := i * 2;
    v2 := v1 + 1;
    SinCos((i / (Segments)) * TwoPi, s, c);
    Vertices[v1].x := c * RadiusInt + Center.X; Vertices[v1].y := s * RadiusInt + Center.Y;
    Vertices[v1].z := 0; Vertices[v1].rhw := 1; Vertices[v1].Color := ColInt;
    Vertices[v2].x := c * RadiusExt + Center.X; Vertices[v2].y := s * RadiusExt + Center.Y;
    Vertices[v2].z := 0; Vertices[v2].rhw := 1; Vertices[v2].Color := ColExt;
  end;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, Segments * 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives2D.DrawArrow(
      const v1, v2: TG2Vec2;
      const Color: TG2Color
    ): TG2Result;
var
  hr: HResult;
  Vertices: PG2VertexArray;
  Indices: PG2Index16Array;
  dir, dirs, p: TG2Vec2;
  l: Single;
  Color2: TG2Color;
  PtMid: TG2Vec2;
const
  BORDER_SIZE = 3;
  ARROW_SIZE = 6;
begin
  Result := grFail;
  dir := v2 - v1;
  l := dir.Len;
  if l < (ARROW_SIZE + BORDER_SIZE) * 2 then Exit;
  if G2ResFail(VerifyVB(14)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 14, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  dir := dir / l;
  dirs := dir.Perp;
  Color2 := Color;
  PtMid := v2 - dir * (ARROW_SIZE * 2 + BORDER_SIZE);
  Color2.a := 0;
  Vertices[0].x := v2.x; Vertices[0].y := v2.y; Vertices[0].z := 1;
  Vertices[0].rhw := 1; Vertices[0].Color := Color2;
  p := PtMid + dir * (ARROW_SIZE * 2 - BORDER_SIZE * 0.5);
  Vertices[1].x := p.x; Vertices[1].y := p.y; Vertices[1].z := 1;
  Vertices[1].rhw := 1; Vertices[1].Color := Color;
  p := PtMid - dirs * ARROW_SIZE;
  Vertices[2].x := p.x; Vertices[2].y := p.y; Vertices[2].z := 1;
  Vertices[2].rhw := 1; Vertices[2].Color := Color;
  p := PtMid - dirs;
  Vertices[3].x := p.x; Vertices[3].y := p.y; Vertices[3].z := 1;
  Vertices[3].rhw := 1; Vertices[3].Color := Color;
  p := PtMid + dirs;
  Vertices[4].x := p.x; Vertices[4].y := p.y; Vertices[4].z := 1;
  Vertices[4].rhw := 1; Vertices[4].Color := Color;
  p := PtMid + dirs * ARROW_SIZE;
  Vertices[5].x := p.x; Vertices[5].y := p.y; Vertices[5].z := 1;
  Vertices[5].rhw := 1; Vertices[5].Color := Color;
  PtMid := PtMid - dir * BORDER_SIZE;
  p := PtMid - dirs * (ARROW_SIZE + BORDER_SIZE * 1.5);
  Vertices[6].x := p.x; Vertices[6].y := p.y; Vertices[6].z := 1;
  Vertices[6].rhw := 1; Vertices[6].Color := Color2;
  p := PtMid - dirs * (BORDER_SIZE + 1);
  Vertices[7].x := p.x; Vertices[7].y := p.y; Vertices[7].z := 1;
  Vertices[7].rhw := 1; Vertices[7].Color := Color2;
  p := PtMid + dirs * (BORDER_SIZE + 1);
  Vertices[8].x := p.x; Vertices[8].y := p.y; Vertices[8].z := 1;
  Vertices[8].rhw := 1; Vertices[8].Color := Color2;
  p := PtMid + dirs * (ARROW_SIZE + BORDER_SIZE * 1.5);
  Vertices[9].x := p.x; Vertices[9].y := p.y; Vertices[9].z := 1;
  Vertices[9].rhw := 1; Vertices[9].Color := Color2;
  p := v1 - dirs * (BORDER_SIZE + 1);
  Vertices[10].x := p.x; Vertices[10].y := p.y; Vertices[10].z := 1;
  Vertices[10].rhw := 1; Vertices[10].Color := Color2;
  p := v1 + dir * BORDER_SIZE - dirs;
  Vertices[11].x := p.x; Vertices[11].y := p.y; Vertices[11].z := 1;
  Vertices[11].rhw := 1; Vertices[11].Color := Color;
  p := v1 + dir * BORDER_SIZE + dirs;
  Vertices[12].x := p.x; Vertices[12].y := p.y; Vertices[12].z := 1;
  Vertices[12].rhw := 1; Vertices[12].Color := Color;
  p := v1 + dirs * (BORDER_SIZE + 1);
  Vertices[13].x := p.x; Vertices[13].y := p.y; Vertices[13].z := 1;
  Vertices[13].rhw := 1; Vertices[13].Color := Color2;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;

  if G2ResFail(VerifyIB(57)) then Exit;
  hr := m_IB.Lock(0, 57 * 2, Pointer(Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  Indices^[0] := 0; Indices^[1] := 9; Indices^[2] := 5;
  Indices^[3] := 5; Indices^[4] := 1; Indices^[5] := 0;
  Indices^[6] := 0; Indices^[7] := 1; Indices^[8] := 2;
  Indices^[9] := 2; Indices^[10] := 6; Indices^[11] := 0;
  Indices^[12] := 1; Indices^[13] := 3; Indices^[14] := 2;
  Indices^[15] := 1; Indices^[16] := 4; Indices^[17] := 3;
  Indices^[18] := 1; Indices^[19] := 5; Indices^[20] := 4;
  Indices^[21] := 5; Indices^[22] := 9; Indices^[23] := 8;
  Indices^[24] := 8; Indices^[25] := 4; Indices^[26] := 5;
  Indices^[27] := 3; Indices^[28] := 7; Indices^[29] := 2;
  Indices^[30] := 2; Indices^[31] := 7; Indices^[32] := 6;
  Indices^[33] := 4; Indices^[34] := 8; Indices^[35] := 12;
  Indices^[36] := 12; Indices^[37] := 8; Indices^[38] := 13;
  Indices^[39] := 12; Indices^[40] := 13; Indices^[41] := 11;
  Indices^[42] := 11; Indices^[43] := 13; Indices^[44] := 10;
  Indices^[45] := 11; Indices^[46] := 10; Indices^[47] := 7;
  Indices^[48] := 7; Indices^[49] := 3; Indices^[50] := 11;
  Indices^[51] := 3; Indices^[52] := 4; Indices^[53] := 12;
  Indices^[54] := 12; Indices^[55] := 11; Indices^[56] := 3;
  hr := m_IB.Unlock;
  if Failed(hr) then Exit;

  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, 14, 0, 19);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives2D.DrawPolyConvex(
      const VArr: PG2Vec2Array;
      const VCount: Word;
      const Color: TG2Color
    ): TG2Result;
var
  Vertices: PG2VertexArray;
  i: Integer;
  hr: HResult;
begin
  Result := grFail;
  if VCount < 2 then Exit;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  for i := 0 to VCount - 1 do
  begin
    Vertices^[i].x := VArr^[i].x; Vertices^[i].y := VArr^[i].y;
    Vertices^[i].z := 0; Vertices^[i].rhw := 1; Vertices^[i].Color := Color;
  end;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLEFAN, 0, VCount - 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;
//TG2Primitives2D END

//TG2Primitives3D BEGIN
constructor TG2Primitives3D.Create;
begin
  inherited Create;
  m_VBSize := 0;
  m_IBSize := 0;
end;

destructor TG2Primitives3D.Destroy;
begin
  inherited Destroy;
end;

function TG2Primitives3D.VerifyVB(const Size: Integer): TG2Result;
begin
  if (Size <= m_VBSize)
  and Assigned(m_VB) then
  begin
    Result := grOk;
    Exit;
  end;
  m_VBSize := Size;
  SafeRelease(m_VB);
  if Failed(
    Core.Graphics.Device.CreateVertexBuffer(
      m_VBSize * SizeOf(TG2Vertex),
      D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
      FVF,
      D3DPOOL_DEFAULT,
      m_VB,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;
  Result := grOk;
end;

function TG2Primitives3D.VerifyIB(const Size: Integer): TG2Result;
begin
  if (Size <= m_IBSize)
  and Assigned(m_IB) then
  begin
    Result := grOk;
    Exit;
  end;
  m_IBSize := Size;
  SafeRelease(m_IB);
  if Failed(
    Core.Graphics.Device.CreateIndexBuffer(
      m_IBSize * SizeOf(Word),
      D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
      D3DFMT_INDEX16,
      D3DPOOL_DEFAULT,
      m_IB,
      nil
    )
  ) then
  begin
    Result := grFail;
    Exit;
  end;
  Result := grOk;
end;

function TG2Primitives3D.AddVertex(
      const X, Y, Z: Single;
      const Color: TG2Color;
      const NX, NY, NZ: Single
    ): Word;
begin
  m_Vertices^[m_CurV].x := X;
  m_Vertices^[m_CurV].y := Y;
  m_Vertices^[m_CurV].z := Z;
  m_Vertices^[m_CurV].Color := Color;
  m_Vertices^[m_CurV].nx := nx;
  m_Vertices^[m_CurV].ny := ny;
  m_Vertices^[m_CurV].nz := nz;
  Result := m_CurV;
  Inc(m_CurV);
end;

procedure TG2Primitives3D.AddFace(
      const v1, v2, v3: Word
    );
begin
  m_Indices^[m_CurI] := v1;
  m_Indices^[m_CurI + 1] := v2;
  m_Indices^[m_CurI + 2] := v3;
  Inc(m_CurI, 3);
end;

procedure TG2Primitives3D.OnDeviceLost;
begin
  SafeRelease(m_VB);
  SafeRelease(m_IB);
end;

procedure TG2Primitives3D.OnDeviceReset;
begin
  VerifyVB(m_VBSize);
  VerifyIB(m_IBSize);
end;

function TG2Primitives3D.DrawLine(
      const v1, v2: TG2Vec3;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawLine2Col(v1, v2, Color, Color);
end;

function TG2Primitives3D.DrawLine2Col(
      const v1, v2: TG2Vec3;
      const c1, c2: TG2Color
    ): TG2Result;
var
  hr: HResult;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(2)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 2, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  m_Vertices^[0].x := v1.X; m_Vertices^[0].y := v1.Y; m_Vertices^[0].z := v1.Z;
  m_Vertices^[0].nx := 0; m_Vertices^[0].ny := 0; m_Vertices^[0].nz := 0;
  m_Vertices^[0].Color := c1;

  m_Vertices^[1].x := v2.X; m_Vertices^[1].y := v2.Y; m_Vertices^[1].z := v2.Z;
  m_Vertices^[1].nx := 0; m_Vertices^[1].ny := 0; m_Vertices^[1].nz := 0;
  m_Vertices^[1].Color := c2;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_LINELIST, 0, 1);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.DrawTriangle(
      const v1, v2, v3: TG2Vec3;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawTriangle3Col(
    v1, v2, v3, Color, Color, Color
  );
end;

function TG2Primitives3D.DrawTriangle3Col(
      const v1, v2, v3: TG2Vec3;
      const c1, c2, c3: TG2Color
    ): TG2Result;
var
  hr: HResult;
  n: TG2Vec3;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(3)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 3, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  n := G2TriangleNormal(v1, v2, v3);

  m_Vertices^[0].x := v1.X; m_Vertices^[0].y := v1.Y; m_Vertices^[0].z := v1.Z;
  m_Vertices^[0].nx := n.X; m_Vertices^[0].ny := n.Y; m_Vertices^[0].nz := n.Z;
  m_Vertices^[0].Color := c1;

  m_Vertices^[1].x := v2.X; m_Vertices^[1].y := v2.Y; m_Vertices^[1].z := v2.Z;
  m_Vertices^[1].nx := n.X; m_Vertices^[1].ny := n.Y; m_Vertices^[1].nz := n.Z;
  m_Vertices^[1].Color := c2;

  m_Vertices^[2].x := v3.X; m_Vertices^[2].y := v3.Y; m_Vertices^[2].z := v3.Z;
  m_Vertices^[2].nx := n.X; m_Vertices^[2].ny := n.Y; m_Vertices^[2].nz := n.Z;
  m_Vertices^[2].Color := c3;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLELIST, 0, 1);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.DrawQuad(
      const v1, v2, v3, v4: TG2Vec3;
      const Color: TG2Color
    ): TG2Result;
begin
  Result := DrawQuad4Col(
    v1, v2, v3, v4,
    Color, Color, Color, Color
  );
end;

function TG2Primitives3D.DrawQuad4Col(
      const v1, v2, v3, v4: TG2Vec3;
      const c1, c2, c3, c4: TG2Color
    ): TG2Result;
var
  hr: HResult;
  n1, n2, n3: TG2Vec3;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(4)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 4, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  n1 := G2TriangleNormal(v1, v2, v3);
  n2 := G2TriangleNormal(v3, v2, v4);
  n3 := (n1 + n2);
  n3.Normalize;

  m_Vertices^[0].x := v1.X; m_Vertices^[0].y := v1.Y; m_Vertices^[0].z := v1.Z;
  m_Vertices^[0].nx := n1.X; m_Vertices^[0].ny := n1.Y; m_Vertices^[0].nz := n1.Z;
  m_Vertices^[0].Color := c1;

  m_Vertices^[1].x := v2.X; m_Vertices^[1].y := v2.Y; m_Vertices^[1].z := v2.Z;
  m_Vertices^[1].nx := n3.X; m_Vertices^[1].ny := n3.Y; m_Vertices^[1].nz := n3.Z;
  m_Vertices^[1].Color := c2;

  m_Vertices^[2].x := v3.X; m_Vertices^[2].y := v3.Y; m_Vertices^[2].z := v3.Z;
  m_Vertices^[2].nx := n3.X; m_Vertices^[2].ny := n3.Y; m_Vertices^[2].nz := n3.Z;
  m_Vertices^[2].Color := c3;

  m_Vertices^[3].x := v4.X; m_Vertices^[3].y := v4.Y; m_Vertices^[3].z := v4.Z;
  m_Vertices^[3].nx := n2.X; m_Vertices^[3].ny := n2.Y; m_Vertices^[3].nz := n2.Z;
  m_Vertices^[3].Color := c4;

  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.DrawBox(
      const X, Y, Z: Single;
      const SizeX, SizeY, SizeZ: Single;
      const Color: TG2Color
    ): TG2Result;
var
  hr: HResult;
  MaxV: TG2Vec3;
begin
  Result := grFail;
  if G2ResFail(VerifyVB(24)) then Exit;
  if G2ResFail(VerifyIB(36)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * 24, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * 36, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  MaxV := G2Vec3(X + SizeX, Y + SizeY, Z + SizeZ);

  m_CurV := 0;
  m_CurI := 0;

  //Bottom
  AddVertex(X, Y, Z, Color, 0, -1, 0);
  AddVertex(MaxV.X, Y, Z, Color, 0, -1, 0);
  AddVertex(X, Y, MaxV.Z, Color, 0, -1, 0);
  AddVertex(MaxV.X, Y, MaxV.Z, Color, 0, -1, 0);
  AddFace(0, 1, 2); AddFace(2, 1, 3);

  //Top
  AddVertex(X, MaxV.Y, MaxV.Z, Color, 0, 1, 0);
  AddVertex(MaxV.X, MaxV.Y, MaxV.Z, Color, 0, 1, 0);
  AddVertex(X, MaxV.Y, Z, Color, 0, 1, 0);
  AddVertex(MaxV.X, MaxV.Y, Z, Color, 0, 1, 0);
  AddFace(4, 5, 6); AddFace(6, 5, 7);

  //Front
  AddVertex(X, MaxV.Y, Z, Color, 0, 0, -1);
  AddVertex(MaxV.X, MaxV.Y, Z, Color, 0, 0, -1);
  AddVertex(X, Y, Z, Color, 0, 0, -1);
  AddVertex(MaxV.X, Y, Z, Color, 0, 0, -1);
  AddFace(8, 9, 10); AddFace(10, 9, 11);

  //Back
  AddVertex(MaxV.X, MaxV.Y, MaxV.Z, Color, 0, 0, 1);
  AddVertex(X, MaxV.Y, MaxV.Z, Color, 0, 0, 1);
  AddVertex(MaxV.X, Y, MaxV.Z, Color, 0, 0, 1);
  AddVertex(X, Y, MaxV.Z, Color, 0, 0, 1);
  AddFace(12, 13, 14); AddFace(14, 13, 15);

  //Left
  AddVertex(X, MaxV.Y, MaxV.Z, Color, -1, 0, 0);
  AddVertex(X, MaxV.Y, Z, Color, -1, 0, 0);
  AddVertex(X, Y, MaxV.Z, Color, -1, 0, 0);
  AddVertex(X, Y, Z, Color, -1, 0, 0);
  AddFace(16, 17, 18); AddFace(18, 17, 19);

  //Right
  AddVertex(MaxV.X, MaxV.Y, Z, Color, 1, 0, 0);
  AddVertex(MaxV.X, MaxV.Y, MaxV.Z, Color, 1, 0, 0);
  AddVertex(MaxV.X, Y, Z, Color, 1, 0, 0);
  AddVertex(MaxV.X, Y, MaxV.Z, Color, 1, 0, 0);
  AddFace(20, 21, 22); AddFace(22, 21, 23);

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, 24, 0, 12);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.DrawSphere(
      const X, Y, Z: Single;
      const Radius: Single;
      const Color: TG2Color;
      const VSegments: Integer = 32;
      const HSegments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i, j: Integer;
  gv, gh: Extended;
  v: Single;
  s, c: Extended;
  LastV: Word;
  VCount: Integer;
  ICount: Integer;
  v1, v2, v3, v4: Word;
  n: TG2Vec3;
begin
  Result := grFail;
  VCount := (VSegments - 1) * HSegments + 2;
  ICount := (VSegments - 2) * HSegments * 6 + HSegments * 6;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  m_CurV := 0;
  m_CurI := 0;

  AddVertex(X, Y + Radius, Z, Color, 0, 1, 0);
  for j := 1 to VSegments - 1 do
  begin
    SinCos((j / (VSegments)) * Pi, gh, gv);
    v := gv * Radius + Y;
    for i := 0 to HSegments - 1 do
    begin
      SinCos((i / (HSegments)) * TwoPi, s, c);
      n := G2Vec3(s * gh, gv, c * gh);
      AddVertex(X + s * gh * Radius, v, Z + c * gh * Radius, Color, n.X, n.Y, n.Z);
    end;
  end;
  LastV := AddVertex(X, Y - Radius, Z, Color, 0, -1, 0);
  for i := 1 to HSegments - 1 do
  AddFace(0, i, i + 1);
  AddFace(0, HSegments, 1);
  for j := 0 to VSegments - 3 do
  begin
    for i := 1 to HSegments - 1 do
    begin
      v1 := (j + 0) * HSegments + (i + 0);
      v2 := (j + 1) * HSegments + (i + 0);
      v3 := (j + 0) * HSegments + (i + 1);
      v4 := (j + 1) * HSegments + (i + 1);
      AddFace(v1, v2, v3); AddFace(v3, v2, v4);
    end;
    i := HSegments;
    v1 := (j + 0) * HSegments + (i);
    v2 := (j + 1) * HSegments + (i);
    v3 := (j + 0) * HSegments + (1);
    v4 := (j + 1) * HSegments + (1);
    AddFace(v1, v2, v3); AddFace(v3, v2, v4);
  end;
  j := (VSegments - 2) * HSegments;
  for i := 1 to HSegments - 1 do
  AddFace(LastV, j + i + 1, j + i);
  AddFace(LastV, j + 1, j + HSegments);

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, VCount, 0, ICount div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.DrawCylinder(
      const X, Y, Z: Single;
      const Height, Radius: Single;
      const Color: TG2Color;
      const Segments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i: Integer;
  s, c: Extended;
  TopV, BottomV: Word;
  VCount: Integer;
  ICount: Integer;
  vx, vz: Single;
begin
  Result := grFail;
  if Segments < 3 then Exit;
  VCount := Segments * 4 + 2;
  ICount := Segments * 12;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  m_CurV := 0;
  m_CurI := 0;

  for i := 0 to Segments - 1 do
  begin
    SinCos((i / Segments) * TwoPi, s, c);
    vx := X + s * Radius;
    vz := Z + c * Radius;
    AddVertex(vx, Y + Height, vz, Color, 0, 1, 0);
    AddVertex(vx, Y + Height, vz, Color, s, 0, c);
    AddVertex(vx, Y, vz, Color, s, 0, c);
    AddVertex(vx, Y, vz, Color, 0, -1, 0);
  end;
  TopV := AddVertex(X, Y + Height, Z, Color, 0, 1, 0);
  BottomV := AddVertex(X, Y, Z, Color, 0, -1, 0);
  for i := 0 to Segments - 2 do
  begin
    AddFace(TopV, ((i + 0) * 4 + 0), ((i + 1) * 4 + 0));
    AddFace(((i + 0) * 4 + 1), ((i + 0) * 4 + 2), ((i + 1) * 4 + 1));
    AddFace(((i + 0) * 4 + 2), ((i + 1) * 4 + 2), ((i + 1) * 4 + 1));
    AddFace(BottomV, ((i + 1) * 4 + 3), ((i + 0) * 4 + 3));
  end;
  i := (Segments - 1) * 4;
  AddFace(TopV, i, 0);
  AddFace((i + 1), (i + 2), 1);
  AddFace((i + 2), 2, 1);
  AddFace(BottomV, 3, i + 3);

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, VCount, 0, ICount div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.DrawCone(
      const X, Y, Z: Single;
      const Height, RadiusTop, RadiusBottom: Single;
      const Color: TG2Color;
      const Segments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i: Integer;
  s, c: Extended;
  TopV, BottomV: Word;
  VCount: Integer;
  ICount: Integer;
  vxt, vzt, vxb, vzb: Single;
begin
  Result := grFail;
  if Segments < 3 then Exit;
  VCount := Segments * 4 + 2;
  ICount := Segments * 12;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  m_CurV := 0;
  m_CurI := 0;

  for i := 0 to Segments - 1 do
  begin
    SinCos((i / Segments) * TwoPi, s, c);
    vxt := X + s * RadiusTop;
    vzt := Z + c * RadiusTop;
    vxb := X + s * RadiusBottom;
    vzb := Z + c * RadiusBottom;
    AddVertex(vxt, Y + Height, vzt, Color, 0, 1, 0);
    AddVertex(vxt, Y + Height, vzt, Color, s, 0, c);
    AddVertex(vxb, Y, vzb, Color, s, 0, c);
    AddVertex(vxb, Y, vzb, Color, 0, -1, 0);
  end;
  TopV := AddVertex(X, Y + Height, Z, Color, 0, 1, 0);
  BottomV := AddVertex(X, Y, Z, Color, 0, -1, 0);
  for i := 0 to Segments - 2 do
  begin
    AddFace(TopV, ((i + 0) * 4 + 0), ((i + 1) * 4 + 0));
    AddFace(((i + 0) * 4 + 1), ((i + 0) * 4 + 2), ((i + 1) * 4 + 1));
    AddFace(((i + 0) * 4 + 2), ((i + 1) * 4 + 2), ((i + 1) * 4 + 1));
    AddFace(BottomV, ((i + 1) * 4 + 3), ((i + 0) * 4 + 3));
  end;
  i := (Segments - 1) * 4;
  AddFace(TopV, i, 0);
  AddFace((i + 1), (i + 2), 1);
  AddFace((i + 2), 2, 1);
  AddFace(BottomV, 3, i + 3);

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, VCount, 0, ICount div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.DrawCapsule(
      const X, Y, Z: Single;
      const Height, Radius: Single;
      const Color: TG2Color;
      const VCapSegments: Integer = 8;
      const HSegments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i, j, v: Integer;
  gv, gh: Extended;
  s, c: Extended;
  TopV, BottomV: Word;
  VCount: Integer;
  ICount: Integer;
  n: TG2Vec3;
  vx, vz: Single;
  HalfHeight: Single;
begin
  Result := grFail;
  if (VCapSegments < 1) or (HSegments < 3) then Exit;
  VCount := VCapSegments * HSegments * 2 + 2;
  ICount := 12 * (VCapSegments * HSegments);
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  m_CurV := 0;
  m_CurI := 0;

  HalfHeight := Height * 0.5;

  for i := 0 to HSegments - 1 do
  begin
    SinCos((i / HSegments) * TwoPi, s, c);
    for j := 1 to VCapSegments do
    begin
      SinCos((j / (VCapSegments)) * HalfPi, gh, gv);
      vx := X + s * Radius * gh;
      vz := Z + c * Radius * gh;
      n := G2Vec3(s * gh, gv, c * gh);
      AddVertex(
        vx,
        Y + HalfHeight + Radius * gv,
        vz,
        Color,
        n.x, n.y, n.z
      );
      AddVertex(
        vx,
        Y - HalfHeight - Radius * gv,
        vz,
        Color,
        n.x, -n.y, n.z
      );
    end;
  end;
  TopV := AddVertex(X, Y + HalfHeight + Radius, Z, Color, 0, 1, 0);
  BottomV := AddVertex(X, Y - HalfHeight - Radius, Z, Color, 0, -1, 0);
  v := (VCapSegments) * 2;
  for i := 0 to HSegments - 2 do
  begin
    AddFace(TopV, i * v, (i + 1) * v);
    AddFace(BottomV, (i + 1) * v + 1, i * v + 1);
    AddFace(i * v + v - 2, i * v + v - 1, (i + 1) * v + v - 2);
    AddFace((i + 1) * v + v - 1, (i + 2) * v - 2, i * v + v - 1);
    for j := 0 to VCapSegments - 2 do
    begin
      AddFace(i * v + j * 2, (i + 1) * v + (j + 1) * 2, (i + 1) * v + j * 2);
      AddFace((i + 1) * v + (j + 1) * 2, i * v + j * 2, i * v + (j + 1) * 2);
      AddFace(i * v + j * 2 + 1, (i + 1) * v + j * 2 + 1, (i + 1) * v + (j + 1) * 2 + 1);
      AddFace((i + 1) * v + (j + 1) * 2 + 1, i * v + (j + 1) * 2 + 1, i * v + j * 2 + 1);
    end;
  end;
  i := (HSegments - 1);
  AddFace(TopV, i * v, 0);
  AddFace(BottomV, 1, i * v + 1);
  AddFace(i * v + v - 2, i * v + v - 1, v - 2);
  AddFace(v - 1, v - 2, i * v + v - 1);
  for j := 0 to VCapSegments - 2 do
  begin
    AddFace(i * v + j * 2, (j + 1) * 2, j * 2);
    AddFace((j + 1) * 2, i * v + j * 2, i * v + (j + 1) * 2);
    AddFace(i * v + j * 2 + 1, j * 2 + 1, (j + 1) * 2 + 1);
    AddFace((j + 1) * 2 + 1, i * v + (j + 1) * 2 + 1, i * v + j * 2 + 1);
  end;

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, m_CurV, 0, m_CurI div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.DrawTorus(
      const X, Y, Z: Single;
      const RadiusInt, RadiusExt: Single;
      const Color: TG2Color;
      const Sides: Integer = 16;
      const Segments: Integer = 32
    ): TG2Result;
var
  hr: HResult;
  i, j: Integer;
  gv, gh: Extended;
  s, c, s1, c1: Extended;
  TopV, BottomV: Word;
  VCount: Integer;
  ICount: Integer;
  n: TG2Vec3;
  vx, vz: Single;
  v, v1, v2: TG2Vec3;
  fv1, fv2, fv3, fv4: Word;
  HalfHeight: Single;
  Radius1, Radius2: Single;
begin
  Result := grFail;
  if (Sides < 3) or (Segments < 3) then Exit;
  VCount := Segments * Sides;
  ICount := Sides * Segments * 6;
  if G2ResFail(VerifyVB(VCount)) then Exit;
  if G2ResFail(VerifyIB(ICount)) then Exit;
  hr := m_VB.Lock(0, SizeOf(TG2Vertex) * VCount, Pointer(m_Vertices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;
  hr := m_IB.Lock(0, SizeOf(Word) * ICount, Pointer(m_Indices), D3DLOCK_DISCARD);
  if Failed(hr) then Exit;

  m_CurV := 0;
  m_CurI := 0;

  Radius1 := (RadiusExt - RadiusInt) * 0.5;
  Radius2 := RadiusInt + Radius1;

  for i := 0 to Segments - 1 do
  begin
    SinCos((i / Segments) * TwoPi, s, c);
    v := G2Vec3(X + s * Radius2, Y, Z + c * Radius2);
    for j := 0 to Sides - 1 do
    begin
      SinCos((j / Sides) * TwoPi, s1, c1);
      n := G2Vec3(s * c1, s1, c * c1);
      v1 := v + n * Radius1;
      AddVertex(v1.x, v1.Y, v1.z, Color, n.x, n.y, n.z);
    end;
  end;
  for i := 0 to Segments - 2 do
  begin
    for j := 0 to Sides - 2 do
    begin
      fv1 := i * Sides + j;
      fv2 := (i + 1) * Sides + j;
      fv3 := i * Sides + (j + 1);
      fv4 := (i + 1) * Sides + (j + 1);
      AddFace(fv1, fv2, fv3);
      AddFace(fv3, fv2, fv4);
    end;
    j := Sides - 1;
    fv1 := i * Sides + j;
    fv2 := (i + 1) * Sides + j;
    fv3 := i * Sides;
    fv4 := (i + 1) * Sides;
    AddFace(fv1, fv2, fv3);
    AddFace(fv3, fv2, fv4);
  end;
  i := Segments - 1;
  for j := 0 to Sides - 2 do
  begin
    fv1 := i * Sides + j;
    fv2 := j;
    fv3 := i * Sides + (j + 1);
    fv4 := (j + 1);
    AddFace(fv1, fv2, fv3);
    AddFace(fv3, fv2, fv4);
  end;
  j := Sides - 1;
  fv1 := i * Sides + j;
  fv2 := j;
  fv3 := i * Sides;
  fv4 := 0;
  AddFace(fv1, fv2, fv3);
  AddFace(fv3, fv2, fv4);

  hr := m_IB.Unlock;
  if Failed(hr) then Exit;
  hr := m_VB.Unlock;
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetFVF(FVF);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetIndices(m_IB);
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.SetStreamSource(0, m_VB, 0, SizeOf(TG2Vertex));
  if Failed(hr) then Exit;
  hr := Core.Graphics.Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, m_CurV, 0, m_CurI div 3);
  if Failed(hr) then Exit;
  Result := grOk;
end;

function TG2Primitives3D.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnDeviceLost;
  m_PlugGraphics.OnDeviceReset := OnDeviceReset;
  VerifyVB(256);
  VerifyIB(1024);
  Result := grOk;
end;

function TG2Primitives3D.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;
//TG2Primitives3D END

//TG2ShaderMgr BEGIN
constructor TG2ShaderMgr.Create;
begin
  inherited Create;
end;

destructor TG2ShaderMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TG2ShaderMgr.OnLostDevice;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2Shader(m_Resources[i]).OnLostDevice;
end;

procedure TG2ShaderMgr.OnResetDevice;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2Shader(m_Resources[i]).OnResetDevice;
end;

function TG2ShaderMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnLostDevice;
  m_PlugGraphics.OnDeviceReset := OnResetDevice;
  Result := grOk;
end;

function TG2ShaderMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;

function TG2ShaderMgr.CreateVertexShaderFromFile(const Name: WideString; const f: WideString): TG2VertexShader;
begin
  Result := TG2VertexShader.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromFile(f)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2ShaderMgr.CreateVertexShaderFromFileCompiled(const Name: WideString; const f: WideString): TG2VertexShader;
begin
  Result := TG2VertexShader.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromFileCompiled(f)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2ShaderMgr.CreateVertexShaderFromMemory(const Name: WideString; const Ptr: Pointer; const Size: DWord): TG2VertexShader;
begin
  Result := TG2VertexShader.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromMemory(Ptr, Size)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2ShaderMgr.CreateVertexShaderFromFunction(const Name: WideString; const Func: PDWord): TG2VertexShader;
begin
  Result := TG2VertexShader.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromFunction(Func)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2ShaderMgr.CreatePixelShaderFromFile(const Name: WideString; const f: WideString): TG2PixelShader;
begin
  Result := TG2PixelShader.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromFile(f)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2ShaderMgr.CreatePixelShaderFromFileCompiled(const Name: WideString; const f: WideString): TG2PixelShader;
begin
  Result := TG2PixelShader.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromFileCompiled(f)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2ShaderMgr.CreatePixelShaderFromMemory(const Name: WideString; const Ptr: Pointer; const Size: DWord): TG2PixelShader;
begin
  Result := TG2PixelShader.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromMemory(Ptr, Size)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2ShaderMgr.CreatePixelShaderFromFunction(const Name: WideString; const Func: PDWord): TG2PixelShader;
begin
  Result := TG2PixelShader.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromFunction(Func)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2ShaderMgr.FindShader(const Name: WideString): TG2Shader;
begin
  Result := TG2Shader(FindResource(Name));
end;
//TG2ShaderMgr END

//TG2Shader BEGIN
constructor TG2Shader.Create;
begin
  inherited Create;
end;

destructor TG2Shader.Destroy;
begin
  inherited Destroy;
end;

function TG2Shader.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_ShaderFunction := nil;
end;

function TG2Shader.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Release;
end;

function TG2Shader.LoadFromFile(const f: WideString): TG2Result;
var
  ShaderBuffer: ID3DXBuffer;
  ErrorBuffer: ID3DXBuffer;
  ErrorLog: AnsiString;
begin
  Release;
  if Failed(
    D3DXAssembleShaderFromFileW(
      PWideChar(f),
      nil,
      nil,
      0,
      @ShaderBuffer,
      @ErrorBuffer
    )
  ) then
  begin
    Result := grFail;
    SetLength(ErrorLog, ErrorBuffer.GetBufferSize);
    Move(PChar(ErrorBuffer.GetBufferPointer)^, ErrorLog[1], ErrorBuffer.GetBufferSize);
    SafeRelease(ErrorBuffer);
    G2WriteLogTimed(AnsiString('(E) Shader compilation failed (' + Name + '): '#$D#$A + String(ErrorLog)), 'Shaders');
    Exit;
  end;
  New(m_CompiledShader);
  SetLength(m_CompiledShader^, ShaderBuffer.GetBufferSize);
  Move(PByte(ShaderBuffer.GetBufferPointer)^, m_CompiledShader^[0], ShaderBuffer.GetBufferSize);
  m_ShaderFunction := @m_CompiledShader^[0];
  SafeRelease(ShaderBuffer);
  Result := grOk;
end;

function TG2Shader.LoadFromFileCompiled(const f: WideString): TG2Result;
var
  fs: TFileStream;
begin
  Result := grOk;
  Release;
  New(m_CompiledShader);
  try
    fs := TFileStream.Create(f, fmOpenRead);
    try
      SetLength(m_CompiledShader^, fs.Size);
      fs.Position := 0;
      fs.ReadBuffer(m_CompiledShader^[0], fs.Size);
      m_ShaderFunction := @m_CompiledShader^[0];
    finally
      fs.Free;
    end;
  except
    Result := grFail;
  end;
end;

function TG2Shader.LoadFromMemory(const Ptr: Pointer; const Size: DWord): TG2Result;
begin
  Release;
  New(m_CompiledShader);
  SetLength(m_CompiledShader^, Size);
  Move(PByte(Ptr)^, m_CompiledShader^[0], Size);
  Result := grOk;
end;

function TG2Shader.LoadFromFunction(const Func: PDWord): TG2Result;
begin
  Release;
  m_ShaderFunction := Func;
  Result := grOk;
end;

procedure TG2Shader.Release;
begin
  if m_CompiledShader <> nil then
  begin
    SetLength(m_CompiledShader^, 0);
    Dispose(m_CompiledShader);
    m_CompiledShader := nil;
  end;
end;

procedure TG2Shader.OnLostDevice;
begin

end;

procedure TG2Shader.OnResetDevice;
begin

end;
//TG2Shader END

//TG2VertexShader BEGIN
constructor TG2VertexShader.Create;
begin
  inherited Create;
end;

destructor TG2VertexShader.Destroy;
begin
  inherited Destroy;
end;

function TG2VertexShader.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(+) VertexShader Initialized: (' + Name + ').'), 'Shaders');
end;

function TG2VertexShader.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(-) VertexShader Finalized: (' + Name + ').'), 'Shaders');
end;

function TG2VertexShader.LoadFromFile(const f: WideString): TG2Result;
begin
  SafeRelease(m_Shader);
  Result := inherited LoadFromFile(f);
  if G2ResFail(Result) then Exit;
  if Failed(
    Core.Graphics.Device.CreateVertexShader(
      @(m_CompiledShader^[0]),
      m_Shader
    )
  ) then
  Result := grFail
  else
  G2WriteLogTimed(AnsiString('(>) VertexShader Loaded (' + Name + ').'), 'Shaders');
end;

function TG2VertexShader.LoadFromFileCompiled(const f: WideString): TG2Result;
begin
  SafeRelease(m_Shader);
  Result := inherited LoadFromFileCompiled(f);
  if G2ResFail(Result) then Exit;
  if Failed(
    Core.Graphics.Device.CreateVertexShader(
      @(m_CompiledShader^[0]),
      m_Shader
    )
  ) then
  Result := grFail
  else
  G2WriteLogTimed(AnsiString('(>) VertexShader Loaded (' + Name + ').'), 'Shaders');
end;

function TG2VertexShader.LoadFromMemory(const Ptr: Pointer; const Size: DWord): TG2Result;
begin
  SafeRelease(m_Shader);
  Result := inherited LoadFromMemory(Ptr, Size);
  if G2ResFail(Result) then Exit;
  if Failed(
    Core.Graphics.Device.CreateVertexShader(
      @(m_CompiledShader^[0]),
      m_Shader
    )
  ) then
  Result := grFail
  else
  G2WriteLogTimed(AnsiString('(>) VertexShader Loaded (' + Name + ').'), 'Shaders');
end;

function TG2VertexShader.LoadFromFunction(const Func: PDWord): TG2Result;
begin
  SafeRelease(m_Shader);
  Result := inherited LoadFromFunction(Func);
  if G2ResFail(Result) then Exit;
  if Failed(
    Core.Graphics.Device.CreateVertexShader(
      m_ShaderFunction,
      m_Shader
    )
  ) then
  Result := grFail
  else
  G2WriteLogTimed(AnsiString('(>) VertexShader Loaded (' + Name + ').'), 'Shaders');
end;

procedure TG2VertexShader.SetConstantF(const r: DWord; const c: PSingle; const F4Count: DWord);
begin
  Core.Graphics.Device.SetVertexShaderConstantF(r, c, F4Count);
end;

procedure TG2VertexShader.SetConstantI(const r: DWord; const c: PInteger; const I4Count: DWord);
begin
  Core.Graphics.Device.SetVertexShaderConstantI(r, c, I4Count);
end;

procedure TG2VertexShader.SetConstantB(const r: DWord; const c: PBool; const BCount: DWord);
begin
  Core.Graphics.Device.SetVertexShaderConstantB(r, c, BCount);
end;

procedure TG2VertexShader.SetToDevice;
begin
  Core.Graphics.Device.SetVertexShader(m_Shader);
end;

procedure TG2VertexShader.Release;
begin
  if Assigned(m_Shader) then
  G2WriteLogTimed(AnsiString('(<) VertexShader Released: (' + Name + ').'), 'Shaders');
  SafeRelease(m_Shader);
  inherited Release;
end;

procedure TG2VertexShader.OnLostDevice;
begin
  SafeRelease(m_Shader);
end;

procedure TG2VertexShader.OnResetDevice;
begin
  if Assigned(m_ShaderFunction) then
  Core.Graphics.Device.CreateVertexShader(
    m_ShaderFunction,
    m_Shader
  )
  else if Assigned(m_CompiledShader) then
  Core.Graphics.Device.CreateVertexShader(
    @(m_CompiledShader^[0]),
    m_Shader
  );
end;
//TG2VertexShader END

//TG2PixelShader BEGIN
constructor TG2PixelShader.Create;
begin
  inherited Create;
end;

destructor TG2PixelShader.Destroy;
begin
  inherited Destroy;
end;

function TG2PixelShader.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(+) PixelShader Initialized: (' + Name + ').'), 'Shaders');
end;

function TG2PixelShader.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(+) PixelShader Finalized: (' + Name + ').'), 'Shaders');
end;

function TG2PixelShader.LoadFromFile(const f: WideString): TG2Result;
begin
  SafeRelease(m_Shader);
  Result := inherited LoadFromFile(f);
  if G2ResFail(Result) then Exit;
  if Failed(
    Core.Graphics.Device.CreatePixelShader(
      @(m_CompiledShader^[0]),
      m_Shader
    )
  ) then
  Result := grFail
  else
  G2WriteLogTimed(AnsiString('(>) PixelShader Loaded (' + Name + ').'), 'Shaders');
end;

function TG2PixelShader.LoadFromFileCompiled(const f: WideString): TG2Result;
begin
  SafeRelease(m_Shader);
  Result := inherited LoadFromFileCompiled(f);
  if G2ResFail(Result) then Exit;
  if Failed(
    Core.Graphics.Device.CreatePixelShader(
      @(m_CompiledShader^[0]),
      m_Shader
    )
  ) then
  Result := grFail
  else
  G2WriteLogTimed(AnsiString('(>) PixelShader Loaded (' + Name +').'), 'Shaders');
end;

function TG2PixelShader.LoadFromMemory(const Ptr: Pointer; const Size: DWord): TG2Result;
begin
  SafeRelease(m_Shader);
  Result := inherited LoadFromMemory(Ptr, Size);
  if G2ResFail(Result) then Exit;
  if Failed(
    Core.Graphics.Device.CreatePixelShader(
      @(m_CompiledShader^[0]),
      m_Shader
    )
  ) then
  Result := grFail
  else
  G2WriteLogTimed(AnsiString('(>) PixelShader Loaded (' + Name + ').'), 'Shaders');
end;

function TG2PixelShader.LoadFromFunction(const Func: PDWord): TG2Result;
begin
  SafeRelease(m_Shader);
  Result := inherited LoadFromFunction(Func);
  if G2ResFail(Result) then Exit;
  if Failed(
    Core.Graphics.Device.CreatePixelShader(
      m_ShaderFunction,
      m_Shader
    )
  ) then
  Result := grFail
  else
  G2WriteLogTimed(AnsiString('(>) PixelShader Loaded (' + Name + ').'), 'Shaders');
end;

procedure TG2PixelShader.SetConstantF(const c: DWord; const Value: PSingle; const F4Count: DWord);
begin
  Core.Graphics.Device.SetPixelShaderConstantF(c, Value, F4Count);
end;

procedure TG2PixelShader.SetConstantI(const c: DWord; const Value: PInteger; const I4Count: DWord);
begin
  Core.Graphics.Device.SetPixelShaderConstantI(c, Value, I4Count);
end;

procedure TG2PixelShader.SetConstantB(const c: DWord; const Value: PBool; const BCount: DWord);
begin
  Core.Graphics.Device.SetPixelShaderConstantB(c, Value, BCount);
end;

procedure TG2PixelShader.SetToDevice;
begin
  Core.Graphics.Device.SetPixelShader(m_Shader);
end;

procedure TG2PixelShader.Release;
begin
  if Assigned(m_Shader) then
  G2WriteLogTimed(AnsiString('(<) PixelShader Released (' + Name + ').'), 'Shaders');
  SafeRelease(m_Shader);
  inherited Release;
end;

procedure TG2PixelShader.OnLostDevice;
begin
  SafeRelease(m_Shader);
end;

procedure TG2PixelShader.OnResetDevice;
begin
  if Assigned(m_ShaderFunction) then
  Core.Graphics.Device.CreatePixelShader(
    m_ShaderFunction,
    m_Shader
  )
  else if m_CompiledShader <> nil then
  Core.Graphics.Device.CreatePixelShader(
    @(m_CompiledShader^[0]),
    m_Shader
  );
end;
//TG2PixelShader END

//TG2EffectMgr BEGIN
constructor TG2EffectMgr.Create;
begin
  inherited Create;
end;

destructor TG2EffectMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TG2EffectMgr.OnLostDevice;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2Effect(m_Resources[i]).OnLostDevice;
end;

procedure TG2EffectMgr.OnResetDevice;
var
  i: Integer;
begin
  for i := 0 to m_Resources.Count - 1 do
  TG2Effect(m_Resources[i]).OnResetDevice;
end;

function TG2EffectMgr.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Core.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := OnLostDevice;
  m_PlugGraphics.OnDeviceReset := OnResetDevice;
  Result := grOk;
end;

function TG2EffectMgr.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleasePlug(@m_PlugGraphics);
  Result := grOk;
end;

function TG2EffectMgr.CreateEffectFromFile(const Name: WideString; const f: WideString): TG2Effect;
begin
  Result := TG2Effect.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if FileExists(f) then
  if G2ResOk(Result.LoadFromFile(f)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2EffectMgr.CreateEffectFromMemory(const Name: WideString; const Ptr: Pointer; const Size: Integer): TG2Effect;
begin
  Result := TG2Effect.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromMemory(Ptr, Size)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2EffectMgr.CreateEffectFromPack(const Name: WideString; const FolderName, FileName: AnsiString): TG2Effect;
begin
  Result := TG2Effect.Create;
  Result.Name := Name;
  Result.Initialize(Core);
  if G2ResOk(Result.LoadFromPack(FolderName, FileName)) then
  AddResource(Result) else FreeAndNil(Result);
end;

function TG2EffectMgr.FindEffect(const Name: WideString): TG2Effect;
begin
  Result := TG2Effect(FindResource(Name));
end;
//TG2EffectMgr END

//TG2Effect BEGIN
constructor TG2Effect.Create;
begin
  inherited Create;
end;

destructor TG2Effect.Destroy;
begin
  inherited Destroy;
end;

function TG2Effect.GetCurTechnique: TD3DXHandle;
begin
  Result := m_Effect.GetCurrentTechnique;
end;

procedure TG2Effect.SetCurTechnique(const Value: TD3DXHandle);
begin
  m_Effect.SetTechnique(Value);
end;

function TG2Effect.BeginEffect(Passes: PLongWord; const Flags: DWORD = 0): TG2Result;
begin
  if Succeeded(m_Effect._Begin(Passes, Flags)) then
  Result := grOk else Result := grFail;
end;

function TG2Effect.EndEffect: TG2Result;
begin
  if Succeeded(m_Effect._End) then
  Result := grOk else Result := grFail;
end;

function TG2Effect.BeginPass(const Pass: LongWord): TG2Result;
begin
  if Succeeded(m_Effect.BeginPass(Pass)) then
  Result := grOk else Result := grFail;
end;

function TG2Effect.EndPass: TG2Result;
begin
  if Succeeded(m_Effect.EndPass) then
  Result := grOk else Result := grFail;
end;

function TG2Effect.CommitChanges: TG2Result;
begin
  if Succeeded(m_Effect.CommitChanges) then
  Result := grOk else Result := grFail;
end;

function TG2Effect.SetValue(hParameter: TD3DXHandle; pData: Pointer; Bytes: LongWord): HResult;
begin
  Result := m_Effect.SetValue(hParameter, pData, Bytes);
end;

function TG2Effect.GetValue(hParameter: TD3DXHandle; pData: Pointer; Bytes: LongWord): HResult;
begin
  Result := m_Effect.GetValue(hParameter, pData, Bytes);
end;

function TG2Effect.SetBool(hParameter: TD3DXHandle; b: BOOL): HResult;
begin
  Result := m_Effect.SetBool(hParameter, b);
end;

function TG2Effect.GetBool(hParameter: TD3DXHandle; out pb: BOOL): HResult;
begin
  Result := m_Effect.GetBool(hParameter, pb);
end;

function TG2Effect.SetBoolArray(hParameter: TD3DXHandle; pb: PBOOL; Count: LongWord): HResult;
begin
  Result := m_Effect.SetBoolArray(hParameter, pb, Count);
end;

function TG2Effect.GetBoolArray(hParameter: TD3DXHandle; pb: PBOOL; Count: LongWord): HResult;
begin
  Result := m_Effect.GetBoolArray(hParameter, pb, Count);
end;

function TG2Effect.SetInt(hParameter: TD3DXHandle; n: Integer): HResult;
begin
  Result := m_Effect.SetInt(hParameter, n);
end;

function TG2Effect.GetInt(hParameter: TD3DXHandle; out pn: Integer): HResult;
begin
  Result := m_Effect.GetInt(hParameter, pn);
end;

function TG2Effect.SetIntArray(hParameter: TD3DXHandle; pn: PInteger; Count: LongWord): HResult;
begin
  Result := m_Effect.SetIntArray(hParameter, pn, Count);
end;

function TG2Effect.GetIntArray(hParameter: TD3DXHandle; pn: PInteger; Count: LongWord): HResult;
begin
  Result := m_Effect.GetIntArray(hParameter, pn, Count);
end;

function TG2Effect.SetFloat(hParameter: TD3DXHandle; f: Single): HResult;
begin
  Result := m_Effect.SetFloat(hParameter, f);
end;

function TG2Effect.GetFloat(hParameter: TD3DXHandle; out pf: Single): HResult;
begin
  Result := m_Effect.GetFloat(hParameter, pf);
end;

function TG2Effect.SetFloatArray(hParameter: TD3DXHandle; pf: PSingle; Count: LongWord): HResult;
begin
  Result := m_Effect.SetFloatArray(hParameter, pf, Count);
end;

function TG2Effect.GetFloatArray(hParameter: TD3DXHandle; pf: PSingle; Count: LongWord): HResult;
begin
  Result := m_Effect.GetFloatArray(hParameter, pf, Count);
end;

function TG2Effect.SetVector(hParameter: TD3DXHandle; const pVector: TD3DXVector4): HResult;
begin
  Result := m_Effect.SetVector(hParameter, pVector);
end;

function TG2Effect.GetVector(hParameter: TD3DXHandle; out pVector: TD3DXVector4): HResult;
begin
  Result := m_Effect.GetVector(hParameter, pVector);
end;

function TG2Effect.SetVec2(hParameter: TD3DXHandle; const Vec2: TG2Vec2): HResult;
begin
  Result := m_Effect.SetValue(hParameter, @Vec2, SizeOf(TG2Vec2));
end;

function TG2Effect.SetVec3(hParameter: TD3DXHandle; const Vec3: TG2Vec3): HResult;
begin
  Result := m_Effect.SetValue(hParameter, @Vec3, SizeOf(TG2Vec3));
end;

function TG2Effect.SetVectorArray(hParameter: TD3DXHandle; pVector: PD3DXVector4; Count: LongWord): HResult;
begin
  Result := m_Effect.SetVectorArray(hParameter, pVector, Count);
end;

function TG2Effect.GetVectorArray(hParameter: TD3DXHandle; pVector: PD3DXVector4; Count: LongWord): HResult;
begin
  Result := m_Effect.GetVectorArray(hParameter, pVector, Count);
end;

function TG2Effect.SetMatrix(hParameter: TD3DXHandle; const pMatrix: TD3DXMatrix): HResult;
begin
  Result := m_Effect.SetMatrix(hParameter, pMatrix);
end;

function TG2Effect.GetMatrix(hParameter: TD3DXHandle; out pMatrix: TD3DXMatrix): HResult;
begin
  Result := m_Effect.GetMatrix(hParameter, pMatrix);
end;

function TG2Effect.SetMatrixArray(hParameter: TD3DXHandle; pMatrix: PD3DXMatrix; Count: LongWord): HResult;
begin
  Result := m_Effect.SetMatrixArray(hParameter, pMatrix, Count);
end;

function TG2Effect.GetMatrixArray(hParameter: TD3DXHandle; pMatrix: PD3DXMatrix; Count: LongWord): HResult;
begin
  Result := m_Effect.GetMatrixArray(hParameter, pMatrix, Count);
end;

function TG2Effect.SetMatrixPointerArray(hParameter: TD3DXHandle; ppMatrix: PPD3DXMatrix; Count: LongWord): HResult;
begin
  Result := m_Effect.SetMatrixPointerArray(hParameter, ppMatrix, Count);
end;

function TG2Effect.GetMatrixPointerArray(hParameter: TD3DXHandle; ppMatrix: PPD3DXMatrix; Count: LongWord): HResult;
begin
  Result := m_Effect.GetMatrixPointerArray(hParameter, ppMatrix, Count);
end;

function TG2Effect.SetMatrixTranspose(hParameter: TD3DXHandle; const pMatrix: TD3DXMatrix): HResult;
begin
  Result := m_Effect.SetMatrixTranspose(hParameter, pMatrix);
end;

function TG2Effect.GetMatrixTranspose(hParameter: TD3DXHandle; out pMatrix: TD3DXMatrix): HResult;
begin
  Result := m_Effect.GetMatrixTranspose(hParameter, pMatrix);
end;

function TG2Effect.SetMatrixTransposeArray(hParameter: TD3DXHandle; pMatrix: PD3DXMatrix; Count: LongWord): HResult;
begin
  Result := m_Effect.SetMatrixTransposeArray(hParameter, pMatrix, Count);
end;

function TG2Effect.GetMatrixTransposeArray(hParameter: TD3DXHandle; pMatrix: PD3DXMatrix; Count: LongWord): HResult;
begin
  Result := m_Effect.GetMatrixTransposeArray(hParameter, pMatrix, Count);
end;

function TG2Effect.SetMatrixTransposePointerArray(hParameter: TD3DXHandle; ppMatrix: PPD3DXMatrix; Count: LongWord): HResult;
begin
  Result := m_Effect.SetMatrixTransposePointerArray(hParameter, ppMatrix, Count);
end;

function TG2Effect.GetMatrixTransposePointerArray(hParameter: TD3DXHandle; ppMatrix: PPD3DXMatrix; Count: LongWord): HResult;
begin
  Result := m_Effect.GetMatrixTransposePointerArray(hParameter, ppMatrix, Count);
end;

function TG2Effect.SetString(hParameter: TD3DXHandle; pString: PAnsiChar): HResult;
begin
  Result := m_Effect.SetString(hParameter, pString);
end;

function TG2Effect.GetString(hParameter: TD3DXHandle; out ppString: PAnsiChar): HResult;
begin
  Result := m_Effect.GetString(hParameter, ppString);
end;

function TG2Effect.SetTexture(hParameter: TD3DXHandle; pTexture: IDirect3DBaseTexture9): HResult;
begin
  Result := m_Effect.SetTexture(hParameter, pTexture);
end;

function TG2Effect.GetTexture(hParameter: TD3DXHandle; out ppTexture: IDirect3DBaseTexture9): HResult;
begin
  Result := m_Effect.GetTexture(hParameter, ppTexture);
end;

function TG2Effect.GetPixelShader(hParameter: TD3DXHandle; out ppPShader: IDirect3DPixelShader9): HResult;
begin
  Result := m_Effect.GetPixelShader(hParameter, ppPShader);
end;

function TG2Effect.GetVertexShader(hParameter: TD3DXHandle; out ppVShader: IDirect3DVertexShader9): HResult;
begin
  Result := m_Effect.GetVertexShader(hParameter, ppVShader);
end;

function TG2Effect.SetArrayRange(hParameter: TD3DXHandle; uStart, uEnd: LongWord): HResult;
begin
  Result := m_Effect.SetArrayRange(hParameter, uStart, uEnd);
end;

function TG2Effect.GetDesc(out pDesc: TD3DXEffectDesc): HResult;
begin
  Result := m_Effect.GetDesc(pDesc);
end;

function TG2Effect.GetParameterDesc(hParameter: TD3DXHandle; out pDesc: TD3DXParameterDesc): HResult;
begin
  Result := m_Effect.GetParameterDesc(hParameter, pDesc);
end;

function TG2Effect.GetTechniqueDesc(hTechnique: TD3DXHandle; out pDesc: TD3DXTechniqueDesc): HResult;
begin
  Result := m_Effect.GetTechniqueDesc(hTechnique, pDesc);
end;

function TG2Effect.GetPassDesc(hPass: TD3DXHandle; out pDesc: TD3DXPassDesc): HResult;
begin
  Result := m_Effect.GetPassDesc(hPass, pDesc);
end;

function TG2Effect.GetFunctionDesc(hShader: TD3DXHandle; out pDesc: TD3DXFunctionDesc): HResult;
begin
  Result := m_Effect.GetFunctionDesc(hShader, pDesc);
end;

function TG2Effect.GetParameter(hParameter: TD3DXHandle; Index: LongWord): TD3DXHandle;
begin
  Result := m_Effect.GetParameter(hParameter, Index);
end;

function TG2Effect.GetParameterByName(hParameter: TD3DXHandle; pName: PAnsiChar): TD3DXHandle;
begin
  Result := m_Effect.GetParameterByName(hParameter, pName);
end;

function TG2Effect.GetParameterBySemantic(hParameter: TD3DXHandle; pSemantic: PAnsiChar): TD3DXHandle;
begin
  Result := m_Effect.GetParameterBySemantic(hParameter, pSemantic);
end;

function TG2Effect.GetParameterElement(hParameter: TD3DXHandle; Index: LongWord): TD3DXHandle;
begin
  Result := m_Effect.GetParameterElement(hParameter, Index);
end;

function TG2Effect.GetTechnique(Index: LongWord): TD3DXHandle;
begin
  Result := m_Effect.GetTechnique(Index);
end;

function TG2Effect.GetTechniqueByName(pName: PAnsiChar): TD3DXHandle;
begin
  Result := m_Effect.GetTechniqueByName(pName);
end;

function TG2Effect.GetPass(hTechnique: TD3DXHandle; Index: LongWord): TD3DXHandle;
begin
  Result := m_Effect.GetPass(hTechnique, Index);
end;

function TG2Effect.GetPassByName(hTechnique: TD3DXHandle; pName: PAnsiChar): TD3DXHandle;
begin
  Result := m_Effect.GetPassByName(hTechnique, pName);
end;

function TG2Effect.GetFunction(Index: LongWord): TD3DXHandle;
begin
  Result := m_Effect.GetFunction(Index);
end;

function TG2Effect.GetFunctionByName(pName: PAnsiChar): TD3DXHandle;
begin
  Result := m_Effect.GetFunctionByName(pName);
end;

function TG2Effect.GetAnnotation(hObject: TD3DXHandle; Index: LongWord): TD3DXHandle;
begin
  Result := m_Effect.GetAnnotation(hObject, Index);
end;

function TG2Effect.GetAnnotationByName(hObject: TD3DXHandle; pName: PAnsiChar): TD3DXHandle;
begin
  Result := m_Effect.GetAnnotationByName(hObject, pName);
end;

function TG2Effect.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(+) Effect Initialized: (' + Name + ').'), 'Shaders');
end;

function TG2Effect.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Release;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(-) Effect Finalized: (' + Name + ').'), 'Shaders');
end;

function TG2Effect.LoadFromFile(const f: WideString): TG2Result;
var
  ErrorBuffer: ID3DXBuffer;
  ErrorStr: AnsiString;
begin
  Release;
  if FAILED(
    D3DXCreateEffectFromFileW(
      Core.Graphics.Device,
      PWideChar(f),
      nil,
      nil,
      0,
      nil,
      m_Effect,
      @ErrorBuffer
    )
  ) then
  begin
    SetLength(ErrorStr, ErrorBuffer.GetBufferSize);
    Move(ErrorBuffer.GetBufferPointer^, ErrorStr[1], ErrorBuffer.GetBufferSize);
    SafeRelease(ErrorBuffer);
    G2WriteLogTimed(AnsiString('(E) Failed to load effect(' + Name + '): '#$D#$A + String(ErrorStr)), 'Shaders');
    Result := grFail;
    Exit;
  end;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Effect Loaded (' + Name + ').'), 'Shaders');
end;

function TG2Effect.LoadFromMemory(const Ptr: Pointer; const Size: Integer): TG2Result;
var
  ErrorBuffer: ID3DXBuffer;
  ErrorStr: AnsiString;
begin
  Release;
  if Failed(
    D3DXCreateEffect(
      Core.Graphics.Device,
      Ptr,
      Size,
      nil,
      nil,
      0,
      nil,
      m_Effect,
      @ErrorBuffer
    )
  ) then
  begin
    SetLength(ErrorStr, ErrorBuffer.GetBufferSize);
    Move(PChar(ErrorBuffer.GetBufferPointer)^, ErrorStr[1], ErrorBuffer.GetBufferSize);
    SafeRelease(ErrorBuffer);
    G2WriteLogTimed(AnsiString('(E) Faield to load effect(' + Name + '): '#$D#$A + String(ErrorStr)), 'Shaders');
    Result := grFail;
    Exit;
  end;
  Result := grOk;
  G2WriteLogTimed(AnsiString('(>) Effect Loaded (' + Name + ').'), 'Shaders');
end;

function TG2Effect.LoadFromPack(const FolderName, FileName: AnsiString): TG2Result;
  var Data: Pointer;
  var DataSize: DWord;
begin
  Core.PackLinker.GetFileData(FolderName, FileName, Data, DataSize);
  if Data = nil then
  begin
    Result := grFail;
    Exit;
  end;
  Result := LoadFromMemory(Data, DataSize);
end;

procedure TG2Effect.Release;
begin
  if Assigned(m_Effect) then
  G2WriteLogTimed(AnsiString('(<) Effect Released (' + Name + ').'), 'Shaders');
  SafeRelease(m_Effect);
end;

procedure TG2Effect.OnLostDevice;
begin
  m_Effect.OnLostDevice;
end;

procedure TG2Effect.OnResetDevice;
begin
  m_Effect.OnResetDevice;
end;
//TG2Effect END

//TG2ShaderLib BEGIN
constructor TG2ShaderLib.Create;
begin
  inherited Create;
end;

destructor TG2ShaderLib.Destroy;
begin
  inherited Destroy;
end;

function TG2ShaderLib.Initialize(const G2Core: TG2Core): TG2Result;
{$include Gen2ShaderLibH.inc}
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  {$include Gen2ShaderLibB.inc}
  Core.RequestMod(TG2ShaderMgr, @m_VertexShaders);
  Core.RequestMod(TG2ShaderMgr, @m_PixelShaders);
  Core.RequestMod(TG2EffectMgr, @m_Effects);
  Result := grOk;
end;

function TG2ShaderLib.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Core.ReleaseMod(@m_Effects);
  Core.ReleaseMod(@m_PixelShaders);
  Core.ReleaseMod(@m_VertexShaders);
  Result := grOk;
end;

procedure TG2ShaderLib.CreateShaderBin(const Name: WideString; const Ptr: Pointer; const Size: DWord);
var
  ShaderBin: TG2ShaderBin;
begin
  if Assigned(FindResource(Name)) then Exit;
  ShaderBin := TG2ShaderBin.Create;
  ShaderBin.Name := Name;
  ShaderBin.LoadBuffer(Ptr, Size);
  AddResource(ShaderBin);
  G2WriteLogTimed(AnsiString('Binary Shader Added: ' + Name), 'ShaderLib');
end;

procedure TG2ShaderLib.CreateShaderBin(const Name: WideString; const Func: PDWord);
var
  ShaderBin: TG2ShaderBin;
begin
  if Assigned(FindResource(Name)) then Exit;
  ShaderBin := TG2ShaderBin.Create;
  ShaderBin.Name := Name;
  ShaderBin.LoadFunction(Func);
  AddResource(ShaderBin);
  G2WriteLogTimed(AnsiString('Binary Shader Added: ' + Name), 'ShaderLib');
end;

function TG2ShaderLib.RequestVertexShader(const Name: WideString): TG2VertexShader;
var
  ShaderBin: TG2ShaderBin;
begin
  Result := TG2VertexShader(m_VertexShaders.FindShader(Name));
  if not Assigned(Result) then
  begin
    ShaderBin := TG2ShaderBin(FindResource(Name));
    if Assigned(ShaderBin) then
    begin
      if Assigned(ShaderBin.Func) then
      Result := m_VertexShaders.CreateVertexShaderFromFunction(
        Name,
        ShaderBin.Func
      )
      else
      Result := m_VertexShaders.CreateVertexShaderFromMemory(
        Name,
        ShaderBin.Buffer,
        ShaderBin.Size
      );
    end
    else
    G2WriteLogTimed(AnsiString('(E)' + Name + ' is not in the shader library.'), 'ShaderLib');
  end;
end;

function TG2ShaderLib.RequestPixelShader(const Name: WideString): TG2PixelShader;
var
  ShaderBin: TG2ShaderBin;
begin
  Result := TG2PixelShader(m_PixelShaders.FindShader(Name));
  if not Assigned(Result) then
  begin
    ShaderBin := TG2ShaderBin(FindResource(Name));
    if Assigned(ShaderBin) then
    begin
      if Assigned(ShaderBin.Func) then
      Result := m_PixelShaders.CreatePixelShaderFromFunction(
        Name,
        ShaderBin.Func
      )
      else
      Result := m_PixelShaders.CreatePixelShaderFromMemory(
        Name,
        ShaderBin.Buffer,
        ShaderBin.Size
      )
    end
    else
    G2WriteLogTimed(AnsiString('(E)' + Name + ' is not in the shader library.'), 'ShaderLib');
  end;
end;

function TG2ShaderLib.RequestEffect(const Name: WideString): TG2Effect;
var
  ShaderBin: TG2ShaderBin;
begin
  Result := TG2Effect(m_Effects.FindEffect(Name));
  if not Assigned(Result) then
  begin
    ShaderBin := TG2ShaderBin(FindResource(Name));
    if Assigned(ShaderBin) then
    Result := m_Effects.CreateEffectFromMemory(
      Name,
      ShaderBin.Buffer,
      ShaderBin.Size
    )
    else
    G2WriteLogTimed(AnsiString('(E)' + Name + ' is not in the shader library.'), 'ShaderLib');
  end;
end;
//TG2ShaderLib END

//TG2ShaderBin BEGIN
constructor TG2ShaderBin.Create;
begin
  inherited Create;
end;

destructor TG2ShaderBin.Destroy;
begin
  inherited Destroy;
end;

function TG2ShaderBin.GetBuffer: Pointer;
begin
  Result := @m_Buffer[0];
end;

function TG2ShaderBin.GetSize: Integer;
begin
  Result := Length(m_Buffer);
end;

procedure TG2ShaderBin.LoadBuffer(const Ptr: Pointer; const Size: DWord);
begin
  SetLength(m_Buffer, Size);
  Move(Ptr^, m_Buffer[0], Size);
  m_Func := @m_Buffer[0];
end;

procedure TG2ShaderBin.LoadFunction(const NewFunc: PDWord);
begin
  m_Func := NewFunc;
end;
//TG2ShaderBin END

//TG2Scene2D BEGIN
constructor TG2Scene2D.Create;
begin
  inherited Create;
  m_FetchNodes := TList.Create;
  m_FetchItems := TList.Create;
end;

destructor TG2Scene2D.Destroy;
begin
  m_FetchItems.Free;
  m_FetchNodes.Free;
  inherited Destroy;
end;

function TG2Scene2D.NodeInScope(const n: PG2Scene2DNode; const MinV, MaxV: TG2Vec2): Boolean;
begin
  Result := (
    (n^.MinV.x <= MaxV.x)
    and (n.MaxV.x >= MinV.x)
    and (n.MinV.y <= MaxV.y)
    and (n.MaxV.y >= MinV.y)
  );
end;

procedure TG2Scene2D.FetchNodes(const MinV, MaxV: TG2Vec2);
  procedure CheckNode(const n: PG2Scene2DNode);
  var
    i: Integer;
  begin
    if NodeInScope(n, MinV, MaxV) then
    begin
      if (n^.MinV.x >= MinV.x)
      and (n^.MinV.y >= MinV.y)
      and (n^.MaxV.x <= MaxV.x)
      and (n^.MaxV.y <= MaxV.y) then
      n^.Occlusion := ocFull
      else
      n^.Occlusion := ocPartial;
      if n^.DivType = dtNoDiv then
      m_FetchNodes.Add(n)
      else
      for i := 0 to High(n^.Children) do
      CheckNode(n^.Children[i]);
    end;
  end;
begin
  m_FetchNodes.Clear;
  CheckNode(m_RootNode);
end;

procedure TG2Scene2D.ResetFetchID;
  procedure ResetNode(const n: PG2Scene2DNode);
  var
    i: Integer;
  begin
    if n^.DivType = dtNoDiv then
    for i := 0 to n^.Items.Count - 1 do
    PG2Scene2DItem(n^.Items[i])^.FetchID := 0
    else
    for i := 0 to High(n^.Children) do
    ResetNode(n^.Children[i]);
  end;
begin
  ResetNode(m_RootNode);
  m_CurFetchID := 0;
end;

procedure TG2Scene2D.QueryItems(const MinV, MaxV: TG2Vec2);
var
  i, j: Integer;
begin
  m_FetchItems.Clear;
  if m_CurFetchID >= High(DWord) - 1 then
  ResetFetchID;
  Inc(m_CurFetchID);
  FetchNodes(MinV, MaxV);
  for i := 0 to m_FetchNodes.Count - 1 do
  for j := 0 to PG2Scene2DNode(m_FetchNodes[i])^.Items.Count - 1 do
  if PG2Scene2DItem(PG2Scene2DNode(m_FetchNodes[i])^.Items[j])^.FetchID < m_CurFetchID then
  begin
    PG2Scene2DItem(PG2Scene2DNode(m_FetchNodes[i])^.Items[j])^.FetchID := m_CurFetchID;
    m_FetchItems.Add(PG2Scene2DItem(PG2Scene2DNode(m_FetchNodes[i])^.Items[j]));
  end;
end;

procedure TG2Scene2D.SceneBuild;
  procedure BuildNode(const Node: PG2Scene2DNode);
    procedure SetUpChildNodes;
    var
      i: Integer;
    begin
      for i := 0 to High(Node^.Children) do
      begin
        New(Node^.Children[i]);
        Node^.Children[i]^.DivType := dtNoDiv;
        Node^.Children[i]^.Parent := Node;
        if Node^.DivType = dtDivQ then
        Node^.Children[i]^.LOD := Node^.LOD + 1
        else
        Node^.Children[i]^.LOD := Node^.LOD;
      end;
    end;
  var
    i: Integer;
    Asp: Single;
    MidX, MidY: Single;
  begin
    MidX := (Node^.MaxV.x + Node^.MinV.x) * 0.5;
    MidY := (Node^.MaxV.y + Node^.MinV.y) * 0.5;
    Asp := (Node^.MaxV.x - Node^.MinV.x) / (Node^.MaxV.y - Node^.MinV.y);
    if Asp >= 2 then
    begin
      Node^.DivType := dtDivH;
      SetLength(Node^.Children, 2);
      SetUpChildNodes;
      Node^.Children[0]^.MinV := Node^.MinV;
      Node^.Children[0]^.MaxV := G2Vec2(MidX, Node^.MaxV.y);
      Node^.Children[1]^.MinV := G2Vec2(MidX, Node^.MinV.y);
      Node^.Children[1]^.MaxV := Node^.MaxV;
    end
    else if Asp <= 0.5 then
    begin
      Node^.DivType := dtDivV;
      SetLength(Node^.Children, 2);
      SetUpChildNodes;
      Node^.Children[0]^.MinV := Node^.MinV;
      Node^.Children[0]^.MaxV := G2Vec2(Node^.MaxV.x, MidY);
      Node^.Children[1]^.MinV := G2Vec2(Node^.MinV.x, MidY);
      Node^.Children[1]^.MaxV := Node^.MaxV;
    end
    else
    begin
      Node^.DivType := dtDivQ;
      SetLength(Node^.Children, 4);
      SetUpChildNodes;
      Node^.Children[0]^.MinV := Node^.MinV;
      Node^.Children[0]^.MaxV := G2Vec2(MidX, MidY);
      Node^.Children[1]^.MinV := G2Vec2(MidX, Node^.MinV.y);
      Node^.Children[1]^.MaxV := G2Vec2(Node^.MaxV.x, MidY);
      Node^.Children[2]^.MinV := G2Vec2(MidX, MidY);
      Node^.Children[2]^.MaxV := Node^.MaxV;
      Node^.Children[3]^.MinV := G2Vec2(Node^.MinV.x, MidY);
      Node^.Children[3]^.MaxV := G2Vec2(MidX, Node^.MaxV.y);
    end;
    for i := 0 to High(Node^.Children) do
    if Node^.Children[i]^.LOD < m_LOD then
    BuildNode(Node^.Children[i]);
    if Node^.DivType = dtNoDiv then
    Node^.Items := TList.Create;
  end;
begin
  if m_Built then Exit;
  New(m_RootNode);
  m_RootNode^.DivType := dtNoDiv;
  m_RootNode^.MinV := m_MinV;
  m_RootNode^.MaxV := m_MaxV;
  m_RootNode^.Parent := nil;
  m_RootNode^.LOD := 0;
  if m_LOD > 0 then
  BuildNode(m_RootNode);
  m_UpdateFlag := 1;
end;

procedure TG2Scene2D.SceneDestroy;
  procedure DestroyNode(const Node: PG2Scene2DNode);
  var
    i, j: Integer;
  begin
    if Node^.DivType <> dtNoDiv then
    for i := 0 to High(Node^.Children) do
    DestroyNode(Node^.Children[i])
    else
    begin
      for j := 0 to Node^.Items.Count - 1 do
      PG2Scene2DItem(Node^.Items[j])^.NodeCount := 0;
      Node^.Items.Free;
    end;
    Dispose(Node);
  end;
begin
  if not m_Built then Exit;
  DestroyNode(m_RootNode);
end;

function TG2Scene2D.ItemCreate(const MinV, MaxV: TG2Vec2): PG2Scene2DItem;
begin
  New(Result);
  Result^.MinV := MinV;
  Result^.MaxV := MaxV;
  Result^.FetchID := 0;
  Result^.NodeCount := 0;
end;

procedure TG2Scene2D.ItemDestroy(const Item: PG2Scene2DItem);
begin
  if Item^.NodeCount > 0 then
  ItemRemove(Item);
  Dispose(Item);
end;

procedure TG2Scene2D.ItemAdd(const Item: PG2Scene2DItem);
var
  i: Integer;
begin
  Item^.FetchID := 0;
  FetchNodes(Item^.MinV, Item^.MaxV);
  if Length(Item^.Nodes) < m_FetchNodes.Count then
  SetLength(Item^.Nodes, m_FetchNodes.Count);
  Item^.NodeCount := m_FetchNodes.Count;
  for i := 0 to m_FetchNodes.Count - 1 do
  begin
    PG2Scene2DNode(m_FetchNodes[i])^.Items.Add(Item);
    Item^.Nodes[i] := PG2Scene2DNode(m_FetchNodes[i]);
  end;
end;

procedure TG2Scene2D.ItemRemove(const Item: PG2Scene2DItem);
var
  i: Integer;
begin
  for i := 0 to Item^.NodeCount - 1 do
  Item^.Nodes[i]^.Items.Remove(Item);
  Item^.NodeCount := 0;
end;

procedure TG2Scene2D.ItemMove(const Item: PG2Scene2DItem; const MinV, MaxV: TG2Vec2);
begin
  Item^.MinV := MinV;
  Item^.MaxV := MaxV;
  ItemRemove(Item);
  ItemAdd(Item);
end;

function TG2Scene2D.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Built := False;
  m_CurFetchID := 0;
  Result := grOk;
end;

function TG2Scene2D.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  Result := grOk;
end;
//TG2Scene2D END

//TG2Scene3D BEGIN
constructor TG2Scene3D.Create;
begin
  inherited Create;
  m_FetchNodes := TList.Create;
  m_FetchItems := TList.Create;
  m_ProcAddItem[0] := ItemAddBox;
  m_ProcAddItem[1] := ItemAddSphere;
  m_FuncFrustumItem[0] := ItemFrustumBox;
  m_FuncFrustumItem[1] := ItemFrustumSphere;
end;

destructor TG2Scene3D.Destroy;
begin
  m_FetchItems.Free;
  m_FetchNodes.Free;
  inherited Destroy;
end;

procedure TG2Scene3D.ItemAddBox(const Item: PG2Scene3DItem);
var
  i: Integer;
begin
  Item^.FetchID := 0;
  QueryNodes(Item^.MinV, Item^.MaxV);
  if Length(Item^.Nodes) < m_FetchNodes.Count then
  SetLength(Item^.Nodes, m_FetchNodes.Count);
  Item^.NodeCount := m_FetchNodes.Count;
  for i := 0 to m_FetchNodes.Count - 1 do
  begin
    PG2Scene3DNode(m_FetchNodes[i])^.Items.Add(Item);
    Item^.Nodes[i] := PG2Scene3DNode(m_FetchNodes[i]);
  end;
end;

procedure TG2Scene3D.ItemAddSphere(const Item: PG2Scene3DItem);
begin
  Item^.MinV := Item^.C - Item^.R;
  Item^.MaxV := Item^.C + Item^.R;
  ItemAddBox(Item);
end;

function TG2Scene3D.ItemFrustumBox(const Item: PG2Scene3DItem; const Frustum: TG2Frustum): Boolean;
begin
  Result := Frustum.BoxInFrustum(Item^.MinV, Item^.MaxV);
end;

function TG2Scene3D.ItemFrustumSphere(const Item: PG2Scene3DItem; const Frustum: TG2Frustum): Boolean;
begin
  Result := Frustum.SphereInFrustum(Item^.C, Item^.R);
end;

function TG2Scene3D.NodeInScope(const n: PG2Scene3DNode; const MinV, MaxV: TG2Vec3): Boolean;
begin
  Result := (
    (n^.MinV.x <= MaxV.x)
    and (n^.MinV.y <= MaxV.y)
    and (n^.MinV.z <= MaxV.z)
    and (n^.MaxV.x >= MinV.x)
    and (n^.MaxV.y >= MinV.y)
    and (n^.MaxV.z >= MinV.z)
  );
end;

procedure TG2Scene3D.SetFetchList(const Value: TList);
begin
  if Assigned(Value) then
  m_RefList := Value
  else
  m_RefList := m_FetchItems;
end;

procedure TG2Scene3D.ResetFetchID;
  procedure ResetNode(const n: PG2Scene3DNode);
  var
    nx, ny, nz, i: Integer;
  begin
    if n^.DivN then
    for i := 0 to n^.Items.Count - 1 do
    PG2Scene3DItem(n^.Items[i])^.FetchID := 0
    else
    for nx := n^.NodeMinX to n^.NodeMaxX do
    for ny := n^.NodeMinY to n^.NodeMaxY do
    for nz := n^.NodeMinZ to n^.NodeMaxZ do
    ResetNode(n^.Children[nx, ny, nz]);
  end;
begin
  ResetNode(m_RootNode);
  m_CurFetchID := 0;
end;

procedure TG2Scene3D.QueryItems(const MinV, MaxV: TG2Vec3);
var
  i, j: Integer;
begin
  m_RefList.Clear;
  if m_CurFetchID >= High(DWord) - 1 then
  ResetFetchID;
  Inc(m_CurFetchID);
  QueryNodes(MinV, MaxV);
  for i := 0 to m_FetchNodes.Count - 1 do
  for j := 0 to PG2Scene3DNode(m_FetchNodes[i])^.Items.Count - 1 do
  if PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j])^.FetchID < m_CurFetchID then
  begin
    PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j])^.FetchID := m_CurFetchID;
    m_RefList.Add(PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j])^.Data);
  end;
end;

procedure TG2Scene3D.QueryItems(const Frustum: TG2Frustum);
var
  i, j: Integer;
begin
  m_RefList.Clear;
  if m_CurFetchID >= High(DWord) - 1 then
  ResetFetchID;
  Inc(m_CurFetchID);
  QueryNodes(Frustum);
  for i := 0 to m_FetchNodes.Count - 1 do
  for j := 0 to PG2Scene3DNode(m_FetchNodes[i])^.Items.Count - 1 do
  if PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j])^.FetchID < m_CurFetchID then
  begin
    PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j])^.FetchID := m_CurFetchID;
    if PG2Scene3DNode(m_FetchNodes[i])^.FrustumCheck = fcIntersect then
    begin
      if m_FuncFrustumItem[
        PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j])^.LocType
      ](PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j]), Frustum) then
      m_RefList.Add(PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j])^.Data);
    end
    else
    m_RefList.Add(PG2Scene3DItem(PG2Scene3DNode(m_FetchNodes[i])^.Items[j])^.Data);
  end;
end;

procedure TG2Scene3D.QueryNodes(const MinV, MaxV: TG2Vec3);
  procedure CheckNode(const n: PG2Scene3DNode);
  var
    nx, ny, nz: Integer;
  begin
    if NodeInScope(n, MinV, MaxV) then
    begin
      if n^.DivN then
      m_FetchNodes.Add(n)
      else
      for nx := n^.NodeMinX to n^.NodeMaxX do
      for ny := n^.NodeMinY to n^.NodeMaxY do
      for nz := n^.NodeMinZ to n^.NodeMaxZ do
      CheckNode(n^.Children[nx, ny, nz]);
    end;
  end;
begin
  m_FetchNodes.Clear;
  CheckNode(m_RootNode);
end;

procedure TG2Scene3D.QueryNodes(const Frustum: TG2Frustum);
  procedure CheckNode(const n: PG2Scene3DNode);
  var
    nx, ny, nz: Integer;
  begin
    n^.FrustumCheck := Frustum.FrustumCheckBox(n^.MinV, n^.MaxV);
    if (n^.FrustumCheck = fcInside)
    or (n^.FrustumCheck = fcIntersect) then
    begin
      if n^.DivN then
      m_FetchNodes.Add(n)
      else
      for nx := n^.NodeMinX to n^.NodeMaxX do
      for ny := n^.NodeMinY to n^.NodeMaxY do
      for nz := n^.NodeMinZ to n^.NodeMaxZ do
      CheckNode(n^.Children[nx, ny, nz]);
    end;
  end;
begin
  m_FetchNodes.Clear;
  CheckNode(m_RootNode);
end;

procedure TG2Scene3D.SceneBuild;
  procedure BuildNode(const n: PG2Scene3DNode);
  var
    gx, gy, gz, sx, sy, sz: Single;
    nx, ny, nz: Integer;
  begin
    sx := n^.MaxV.x - n^.MinV.x;
    sy := n^.MaxV.y - n^.MinV.y;
    sz := n^.MaxV.z - n^.MinV.z;
    gx := Min(sx / sy, sx / sz);
    gy := Min(sy / sx, sy / sz);
    gz := Min(sz / sx, sz / sy);
    n^.DivX := (not (gx < 0.5)) and ((m_MaxLODX = -1) or (n^.LOD < m_MaxLODX));
    n^.DivY := (not (gy < 0.5)) and ((m_MaxLODY = -1) or (n^.LOD < m_MaxLODY));
    n^.DivZ := (not (gz < 0.5)) and ((m_MaxLODZ = -1) or (n^.LOD < m_MaxLODZ));
    n^.DivN := not (n^.DivX or n^.DivY or n^.DivZ);
    n^.NodeMinX := 0; n^.NodeMaxX := 0;
    n^.NodeMinY := 0; n^.NodeMaxY := 0;
    n^.NodeMinZ := 0; n^.NodeMaxZ := 0;
    if n^.DivX then
    begin
      n^.NodeMaxX := 1;
      sx := sx * 0.5;
    end;
    if n^.DivY then
    begin
      n^.NodeMaxY := 1;
      sy := sy * 0.5;
    end;
    if n^.DivZ then
    begin
      n^.NodeMaxZ := 1;
      sz := sz * 0.5;
    end;
    if n^.DivN then
    begin
      n^.NodeMinX := 1;
      n^.NodeMinY := 1;
      n^.NodeMinZ := 1;
      n^.NodeMaxX := 0;
      n^.NodeMaxY := 0;
      n^.NodeMaxZ := 0;
      n.Items := TList.Create;
    end
    else
    begin
      SetLength(n^.Children, n^.NodeMaxX + 1, n^.NodeMaxY + 1, n^.NodeMaxZ + 1);
      for nx := n^.NodeMinX to n^.NodeMaxX do
      for ny := n^.NodeMinY to n^.NodeMaxY do
      for nz := n^.NodeMinZ to n^.NodeMaxZ do
      begin
        New(n^.Children[nx, ny, nz]);
        n^.Children[nx, ny, nz]^.Parent := n;
        n^.Children[nx, ny, nz]^.LOD := n^.LOD + 1;
        n^.Children[nx, ny, nz]^.DivX := False;
        n^.Children[nx, ny, nz]^.DivY := False;
        n^.Children[nx, ny, nz]^.DivZ := False;
        n^.Children[nx, ny, nz]^.DivN := True;
        n^.Children[nx, ny, nz]^.NodeMinX := 0;
        n^.Children[nx, ny, nz]^.NodeMaxX := 0;
        n^.Children[nx, ny, nz]^.NodeMinY := 0;
        n^.Children[nx, ny, nz]^.NodeMaxY := 0;
        n^.Children[nx, ny, nz]^.NodeMinZ := 0;
        n^.Children[nx, ny, nz]^.NodeMaxZ := 0;
        n^.Children[nx, ny, nz]^.MinV := n^.MinV + G2Vec3(sx * nx, sy * ny, sz * nz);
        n^.Children[nx, ny, nz]^.MaxV := n^.MinV + G2Vec3(sx * (nx + 1), sy * (ny + 1), sz * (nz + 1));
        if n^.Children[nx, ny, nz]^.LOD < m_LOD then
        BuildNode(n^.Children[nx, ny, nz])
        else
        n^.Children[nx, ny, nz]^.Items := TList.Create;
      end;
    end;
  end;
begin
  if m_Built then Exit;
  New(m_RootNode);
  m_RootNode^.DivX := False;
  m_RootNode^.DivY := False;
  m_RootNode^.DivZ := False;
  m_RootNode^.DivN := True;
  m_RootNode^.MinV := m_MinV;
  m_RootNode^.MaxV := m_MaxV;
  m_RootNode^.Parent := nil;
  m_RootNode^.LOD := 0;
  if m_LOD > 0 then
  BuildNode(m_RootNode)
  else
  m_RootNode^.Items := TList.Create;
  m_CurFetchID := 0;
  m_Built := True;
end;

procedure TG2Scene3D.SceneDestroy;
  procedure DestroyNode(const n: PG2Scene3DNode);
  var
    nx, ny, nz, i: Integer;
  begin
    if n^.DivN then
    begin
      for i := 0 to n^.Items.Count - 1 do
      PG2Scene3DItem(n^.Items[i])^.NodeCount := 0;
      n^.Items.Free;
    end
    else
    for nx := n^.NodeMinX to n^.NodeMaxX do
    for ny := n^.NodeMinY to n^.NodeMaxY do
    for nz := n^.NodeMinZ to n^.NodeMaxZ do
    DestroyNode(n^.Children[nx, ny, nz]);
    Dispose(n);
  end;
begin
  if not m_Built then Exit;
  DestroyNode(m_RootNode);
  m_Built := False;
end;

function TG2Scene3D.ItemCreate(const LocType: Integer = 0): PG2Scene3DItem;
begin
  New(Result);
  Result^.LocType := LocType;
  Result^.FetchID := 0;
  Result^.NodeCount := 0;
end;

procedure TG2Scene3D.ItemDestroy(const Item: PG2Scene3DItem);
begin
  if Item^.NodeCount > 0 then
  ItemRemove(Item);
  Dispose(Item);
end;

procedure TG2Scene3D.ItemAdd(const Item: PG2Scene3DItem);
begin
  m_ProcAddItem[Item^.LocType](Item);
end;

procedure TG2Scene3D.ItemRemove(const Item: PG2Scene3DItem);
var
  i: Integer;
begin
  for i := 0 to Item^.NodeCount - 1 do
  Item^.Nodes[i]^.Items.Remove(Item);
  Item^.NodeCount := 0;
end;

procedure TG2Scene3D.ItemMove(const Item: PG2Scene3DItem; const MinV, MaxV: TG2Vec3);
begin
  ItemRemove(Item);
  Item^.MinV := MinV;
  Item^.MaxV := MaxV;
  ItemAdd(Item);
end;

procedure TG2Scene3D.ItemMove(const Item: PG2Scene3DItem; const C: TG2Vec3; const R: Single);
begin
  ItemRemove(Item);
  Item^.C := C;
  Item^.R := R;
  ItemAdd(Item);
end;

function TG2Scene3D.Initialize(const G2Core: TG2Core): TG2Result;
begin
  Result := inherited Initialize(G2Core);
  if G2ResFail(Result) then Exit;
  m_Built := False;
  m_RefList := m_FetchItems;
  m_MaxLODX := -1;
  m_MaxLODY := -1;
  m_MaxLODZ := -1;
  Result := grOk;
end;

function TG2Scene3D.Finalize: TG2Result;
begin
  Result := inherited Finalize;
  if G2ResFail(Result) then Exit;
  if m_Built then SceneDestroy;
  Result := grOk;
end;
//TG2Scene3D END

//TG2SkyBox BEGIN
constructor TG2SkyBox.Create(const Core: TG2Core);
  type TVertex = record
  public
    var Position: TG2Vec3;
    var TexCoord: TG2Vec3;
  end;
  type TVertexArr = array[Word] of TVertex;
  type PVertexArr = ^TVertexArr;
  var Vertices: PVertexArr;
  var Indices: PG2Index16Array;
  var CurIndex: Integer;
  procedure AddIndexFace(const i0, i1, i2, i3: Integer);
  begin
    Indices^[CurIndex + 0] := i0;
    Indices^[CurIndex + 1] := i1;
    Indices^[CurIndex + 2] := i2;
    Indices^[CurIndex + 3] := i0;
    Indices^[CurIndex + 4] := i2;
    Indices^[CurIndex + 5] := i3;
    Inc(CurIndex, 6);
  end;
begin
  inherited Create;
  m_Core := Core;
  m_VB := TG2VB.Create;
  m_VB.Initialize(m_Core);
  m_VB.Verify(
    SizeOf(TVertex),
    8,
    0,
    D3DFVF_XYZ or D3DFVF_TEX1 or D3DFVF_TEXCOORDSIZE3(0),
    D3DPOOL_MANAGED
  );
  m_IB := TG2IB.Create;
  m_IB.Initialize(m_Core);
  m_IB.Verify(
    36,
    0,
    D3DFMT_INDEX16,
    D3DPOOL_MANAGED
  );
  m_VB.Lock(0, m_VB.Count * m_VB.Stride, Pointer(Vertices), D3DLOCK_DISCARD);
  Vertices^[0].Position.SetValue(-0.5, 0.5, -0.5); Vertices^[0].TexCoord := Vertices^[0].Position.Normalized;
  Vertices^[1].Position.SetValue(-0.5, 0.5, 0.5); Vertices^[1].TexCoord := Vertices^[1].Position.Normalized;
  Vertices^[2].Position.SetValue(0.5, 0.5, 0.5); Vertices^[2].TexCoord := Vertices^[2].Position.Normalized;
  Vertices^[3].Position.SetValue(0.5, 0.5, -0.5); Vertices^[3].TexCoord := Vertices^[3].Position.Normalized;
  Vertices^[4].Position.SetValue(-0.5, -0.5, -0.5); Vertices^[4].TexCoord := Vertices^[4].Position.Normalized;
  Vertices^[5].Position.SetValue(-0.5, -0.5, 0.5); Vertices^[5].TexCoord := Vertices^[5].Position.Normalized;
  Vertices^[6].Position.SetValue(0.5, -0.5, 0.5); Vertices^[6].TexCoord := Vertices^[6].Position.Normalized;
  Vertices^[7].Position.SetValue(0.5, -0.5, -0.5); Vertices^[7].TexCoord := Vertices^[7].Position.Normalized;
  m_VB.UnLock;
  m_IB.Lock(0, m_IB.Count * 2, Pointer(Indices));
  CurIndex := 0;
  AddIndexFace(4, 5, 6, 7);
  AddIndexFace(5, 1, 2, 6);
  AddIndexFace(1, 0, 3, 2);
  AddIndexFace(3, 0, 4, 7);
  AddIndexFace(4, 0, 1, 5);
  AddIndexFace(6, 2, 3, 7);
  m_IB.UnLock;
  m_Scale := 10;
end;

destructor TG2SkyBox.Destroy;
begin
  m_IB.Finalize;
  m_IB.Free;
  m_VB.Finalize;
  m_VB.Free;
  inherited Destroy;
end;

procedure TG2SkyBox.Render;
  var m: TG2Mat;
begin
  if m_Texture <> nil then
  begin
    m_Core.Graphics.Transforms.PushW;
    m.SetScaling(m_Scale);
    m_Core.Graphics.Transforms.W[0] := m;
    m_Core.Graphics.Transforms.ApplyW(0);
    m_Core.Graphics.Transforms.PushV;
    m := m_Core.Graphics.Transforms.V;
    m.e30 := 0; m.e31 := 0; m.e32 := 0;
    m_Core.Graphics.Transforms.V := m;
    m_Core.Graphics.Transforms.ApplyV;
    m_VB.SetToDevice;
    m_IB.SetToDevice;
    m_Core.Graphics.Device.SetTexture(0, m_Texture.Texture);
    m_Core.Graphics.Device.DrawIndexedPrimitive(
      D3DPT_TRIANGLELIST,
      0, 0, 8, 0, 12
    );
    m_Core.Graphics.Transforms.PopW;
    m_Core.Graphics.Transforms.ApplyW(0);
    m_Core.Graphics.Transforms.PopV;
    m_Core.Graphics.Transforms.ApplyV;
  end;
end;
//TG2SkyBox END

//TG2PostProcess BEGIN
procedure TG2PostProcess.LoadBuffer0(const Pos, Tex0: TG2Rect);
  var Vertices: PVertex1Arr;
begin
  m_VB[0].Lock(0, SizeOf(TVertex1) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  Vertices^[0].Pos.x := Pos.Left - 0.5; Vertices^[0].Pos.y := Pos.Top - 0.5; Vertices^[0].Pos.z := 0; Vertices^[0].Pos.w := 1;
  Vertices^[1].Pos.x := Pos.Right + 0.5; Vertices^[1].Pos.y := Pos.Top - 0.5; Vertices^[1].Pos.z := 0; Vertices^[1].Pos.w := 1;
  Vertices^[2].Pos.x := Pos.Left - 0.5; Vertices^[2].Pos.y := Pos.Bottom + 0.5; Vertices^[2].Pos.z := 0; Vertices^[2].Pos.w := 1;
  Vertices^[3].Pos.x := Pos.Right + 0.5; Vertices^[3].Pos.y := Pos.Bottom + 0.5; Vertices^[3].Pos.z := 0; Vertices^[3].Pos.w := 1;
  Vertices^[0].Tex0.SetValue(Tex0.Left, Tex0.Top);
  Vertices^[1].Tex0.SetValue(Tex0.Right, Tex0.Top);
  Vertices^[2].Tex0.SetValue(Tex0.Left, Tex0.Bottom);
  Vertices^[3].Tex0.SetValue(Tex0.Right, Tex0.Bottom);
  m_VB[0].UnLock;
end;

procedure TG2PostProcess.LoadBuffer1(const Pos, Tex0, Tex1: TG2Rect);
  var Vertices: PVertex2Arr;
begin
  m_VB[1].Lock(0, SizeOf(TVertex2) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  Vertices^[0].Pos.x := Pos.Left - 0.5; Vertices^[0].Pos.y := Pos.Top - 0.5; Vertices^[0].Pos.z := 0; Vertices^[0].Pos.w := 1;
  Vertices^[1].Pos.x := Pos.Right + 0.5; Vertices^[1].Pos.y := Pos.Top - 0.5; Vertices^[1].Pos.z := 0; Vertices^[1].Pos.w := 1;
  Vertices^[2].Pos.x := Pos.Left - 0.5; Vertices^[2].Pos.y := Pos.Bottom + 0.5; Vertices^[2].Pos.z := 0; Vertices^[2].Pos.w := 1;
  Vertices^[3].Pos.x := Pos.Right + 0.5; Vertices^[3].Pos.y := Pos.Bottom + 0.5; Vertices^[3].Pos.z := 0; Vertices^[3].Pos.w := 1;
  Vertices^[0].Tex0.SetValue(Tex0.Left, Tex0.Top);
  Vertices^[1].Tex0.SetValue(Tex0.Right, Tex0.Top);
  Vertices^[2].Tex0.SetValue(Tex0.Left, Tex0.Bottom);
  Vertices^[3].Tex0.SetValue(Tex0.Right, Tex0.Bottom);
  Vertices^[0].Tex1.SetValue(Tex1.Left, Tex1.Top);
  Vertices^[1].Tex1.SetValue(Tex1.Right, Tex1.Top);
  Vertices^[2].Tex1.SetValue(Tex1.Left, Tex1.Bottom);
  Vertices^[3].Tex1.SetValue(Tex1.Right, Tex1.Bottom);
  m_VB[1].UnLock;
end;

procedure TG2PostProcess.LoadBuffer2(const Pos, Tex0, Tex1, Tex2: TG2Rect);
  var Vertices: PVertex3Arr;
begin
  m_VB[2].Lock(0, SizeOf(TVertex3) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  Vertices^[0].Pos.x := Pos.Left - 0.5; Vertices^[0].Pos.y := Pos.Top - 0.5; Vertices^[0].Pos.z := 0; Vertices^[0].Pos.w := 1;
  Vertices^[1].Pos.x := Pos.Right + 0.5; Vertices^[1].Pos.y := Pos.Top - 0.5; Vertices^[1].Pos.z := 0; Vertices^[1].Pos.w := 1;
  Vertices^[2].Pos.x := Pos.Left - 0.5; Vertices^[2].Pos.y := Pos.Bottom + 0.5; Vertices^[2].Pos.z := 0; Vertices^[2].Pos.w := 1;
  Vertices^[3].Pos.x := Pos.Right + 0.5; Vertices^[3].Pos.y := Pos.Bottom + 0.5; Vertices^[3].Pos.z := 0; Vertices^[3].Pos.w := 1;
  Vertices^[0].Tex0.SetValue(Tex0.Left, Tex0.Top);
  Vertices^[1].Tex0.SetValue(Tex0.Right, Tex0.Top);
  Vertices^[2].Tex0.SetValue(Tex0.Left, Tex0.Bottom);
  Vertices^[3].Tex0.SetValue(Tex0.Right, Tex0.Bottom);
  Vertices^[0].Tex1.SetValue(Tex1.Left, Tex1.Top);
  Vertices^[1].Tex1.SetValue(Tex1.Right, Tex1.Top);
  Vertices^[2].Tex1.SetValue(Tex1.Left, Tex1.Bottom);
  Vertices^[3].Tex1.SetValue(Tex1.Right, Tex1.Bottom);
  Vertices^[0].Tex2.SetValue(Tex2.Left, Tex2.Top);
  Vertices^[1].Tex2.SetValue(Tex2.Right, Tex2.Top);
  Vertices^[2].Tex2.SetValue(Tex2.Left, Tex2.Bottom);
  Vertices^[3].Tex2.SetValue(Tex2.Right, Tex2.Bottom);
  m_VB[2].UnLock;
end;

procedure TG2PostProcess.LoadBuffer3(const Pos, Tex0, Tex1, Tex2, Tex3: TG2Rect);
  var Vertices: PVertex4Arr;
begin
  m_VB[3].Lock(0, SizeOf(TVertex4) * 4, Pointer(Vertices), D3DLOCK_DISCARD);
  Vertices^[0].Pos.x := Pos.Left - 0.5; Vertices^[0].Pos.y := Pos.Top - 0.5; Vertices^[0].Pos.z := 0; Vertices^[0].Pos.w := 1;
  Vertices^[1].Pos.x := Pos.Right + 0.5; Vertices^[1].Pos.y := Pos.Top - 0.5; Vertices^[1].Pos.z := 0; Vertices^[1].Pos.w := 1;
  Vertices^[2].Pos.x := Pos.Left - 0.5; Vertices^[2].Pos.y := Pos.Bottom + 0.5; Vertices^[2].Pos.z := 0; Vertices^[2].Pos.w := 1;
  Vertices^[3].Pos.x := Pos.Right + 0.5; Vertices^[3].Pos.y := Pos.Bottom + 0.5; Vertices^[3].Pos.z := 0; Vertices^[3].Pos.w := 1;
  Vertices^[0].Tex0.SetValue(Tex0.Left, Tex0.Top);
  Vertices^[1].Tex0.SetValue(Tex0.Right, Tex0.Top);
  Vertices^[2].Tex0.SetValue(Tex0.Left, Tex0.Bottom);
  Vertices^[3].Tex0.SetValue(Tex0.Right, Tex0.Bottom);
  Vertices^[0].Tex1.SetValue(Tex1.Left, Tex1.Top);
  Vertices^[1].Tex1.SetValue(Tex1.Right, Tex1.Top);
  Vertices^[2].Tex1.SetValue(Tex1.Left, Tex1.Bottom);
  Vertices^[3].Tex1.SetValue(Tex1.Right, Tex1.Bottom);
  Vertices^[0].Tex2.SetValue(Tex2.Left, Tex2.Top);
  Vertices^[1].Tex2.SetValue(Tex2.Right, Tex2.Top);
  Vertices^[2].Tex2.SetValue(Tex2.Left, Tex2.Bottom);
  Vertices^[3].Tex2.SetValue(Tex2.Right, Tex2.Bottom);
  Vertices^[0].Tex3.SetValue(Tex3.Left, Tex3.Top);
  Vertices^[1].Tex3.SetValue(Tex3.Right, Tex3.Top);
  Vertices^[2].Tex3.SetValue(Tex3.Left, Tex3.Bottom);
  Vertices^[3].Tex3.SetValue(Tex3.Right, Tex3.Bottom);
  m_VB[3].UnLock;
end;

function TG2PostProcess.RequestSurface(const Width, Height: Integer; const Format: TD3DFormat = D3DFMT_UNKNOWN): TG2Texture2DRT;
  var i: Integer;
  var TmpSurf: PTempSurface;
begin
  for i := 0 to m_Surfaces.Count - 1 do
  if (
    (PTempSurface(m_Surfaces[i])^.Width = Width)
    and (PTempSurface(m_Surfaces[i])^.Height = Height)
    and (PTempSurface(m_Surfaces[i])^.Format = Format)
  ) or (
    (PTempSurface(m_Surfaces[i])^.Surface.Width = Width)
    and (PTempSurface(m_Surfaces[i])^.Surface.Height = Height)
    and (PTempSurface(m_Surfaces[i])^.Surface.Format = Format)
  ) then
  begin
    Result := PTempSurface(m_Surfaces[i])^.Surface;
    Exit;
  end;
  Result := TG2Texture2DRT.Create;
  Result.Initialize(m_Core);
  Result.MakeRenderTarget(Width, Height, Format);
  New(TmpSurf);
  TmpSurf^.Surface := Result;
  TmpSurf^.Width := Width;
  TmpSurf^.Height := Height;
  TmpSurf^.Format := Format;
  m_Surfaces.Add(TmpSurf);
end;

constructor TG2PostProcess.Create(const Core: TG2Core);
  var i: Integer;
begin
  inherited Create;
  m_Core := Core;
  m_Shaders := m_Core.Graphics.ShaderLib.RequestEffect('fx_PostProcess');
  for i := 0 to 3 do
  begin
    m_VB[i] := TG2VB.Create;
    m_VB[i].Initialize(m_Core);
  end;
  m_VB[0].Verify(SizeOf(TVertex1), 4, D3DUSAGE_WRITEONLY, D3DFVF_XYZRHW or D3DFVF_TEX1, D3DPOOL_MANAGED);
  m_VB[1].Verify(SizeOf(TVertex2), 4, D3DUSAGE_WRITEONLY, D3DFVF_XYZRHW or D3DFVF_TEX2, D3DPOOL_MANAGED);
  m_VB[2].Verify(SizeOf(TVertex3), 4, D3DUSAGE_WRITEONLY, D3DFVF_XYZRHW or D3DFVF_TEX3, D3DPOOL_MANAGED);
  m_VB[3].Verify(SizeOf(TVertex4), 4, D3DUSAGE_WRITEONLY, D3DFVF_XYZRHW or D3DFVF_TEX4, D3DPOOL_MANAGED);
end;

destructor TG2PostProcess.Destroy;
  var i: Integer;
begin
  for i := 0 to 3 do
  begin
    m_VB[i].Finalize;
    m_VB[i].Free;
  end;
  for i := 0 to m_Surfaces.Count - 1 do
  begin
    PTempSurface(m_Surfaces[i])^.Surface.Finalize;
    PTempSurface(m_Surfaces[i])^.Surface.Free;
    Dispose(PTempSurface(m_Surfaces[i]));
  end;
  m_Surfaces.Clear;
  m_Shaders.Finalize;
  m_Shaders.Free;
  inherited Destroy;
end;

procedure TG2PostProcess.EffectBlur(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Integer = 1
    );
  var Tmp01: TG2Texture2DRT;
  var ShaderVar: record
    var Offset: TG2Vec2;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
  var PassID: Integer;
begin
  if Amount < 1 then
  PassID := 0
  else
  if Amount > 10 then
  PassID := 9
  else
  case Amount of
    1: PassID := 0;//3x3
    2: PassID := 1;//5x5
    3: PassID := 2;//7x7
    4: PassID := 3;//9x9
    5: PassID := 4;//11x11
    6: PassID := 5;//13x13
    7: PassID := 6;//15x15
    8: PassID := 7;//17x17
    9: PassID := 8;//19x19
    10: PassID := 9;//21x21
    else PassID := 0;
  end;
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  Tmp01 := RequestSurface(Input.Width, Input.Height);
  m_Shaders.Technique := 'Blur';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.Offset.SetValue(1 / Input.RealWidth, 0);
  m_Shaders.SetValue('VarBlur', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(PassID);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
  LoadBuffer0(
    G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
    Input.DrawRect^
  );
  ShaderVar.Offset.SetValue(0, 1 / Input.RealHeight);
  m_Shaders.SetValue('VarBlur', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.CommitChanges;
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Tmp01.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectSharpen(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 10
    );
  var ShaderVar: record
    var PixelSize: TG2Vec2;
    var Scale: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'Sharpen';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.PixelSize := Input.TexelSize;
  ShaderVar.Scale := Amount;
  m_Shaders.SetValue('VarSharpen', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectMonotone(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 1;
      const Mask: DWord = $ffffffff
    );
  var ShaderVar: record
    var Amount: Single;
    var Mask: TG2Vec4;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'Monotone';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.Amount := Amount;
  ShaderVar.Mask.SetValue(((Mask shr 16) and $ff) * Rcp255, ((Mask shr 8) and $ff) * Rcp255, (Mask and $ff) * Rcp255, ((Mask shr 24) and $ff) * Rcp255);
  m_Shaders.SetValue('VarMonotone', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectContrast(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 1
    );
  var ShaderVar: record
    var Amount: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'Contrast';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.Amount := Amount;
  m_Shaders.SetValue('VarContrast', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectEmboss(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 10
    );
  var ShaderVar: record
    var PixelSize: TG2Vec2;
    var Scale: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'Emboss';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.PixelSize := Input.TexelSize;
  ShaderVar.Scale := Amount;
  m_Shaders.SetValue('VarEmboss', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectEdges(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Amount: Single = 10;
      const Power: Single = 1
    );
  var ShaderVar: record
    var PixelSize: TG2Vec2;
    var Scale: Single;
    var Power: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'Edge';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.PixelSize := Input.TexelSize;
  ShaderVar.Scale := Amount;
  ShaderVar.Power := Power;
  m_Shaders.SetValue('VarEdge', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectColorClamp(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const ClampMin: Single = 0;
      const ClampMax: Single = 1
    );
  var ShaderVar: record
    var ClampMin: Single;
    var ClampMax: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'ColorClamp';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.ClampMin := ClampMin;
  ShaderVar.ClampMax := ClampMax;
  m_Shaders.SetValue('VarColorClamp', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectMonotoneClamp(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const ClampMin: Single = 0;
      const ClampMax: Single = 1
    );
  var ShaderVar: record
    var ClampMin: Single;
    var ClampMax: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'MonotoneClamp';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.ClampMin := ClampMin;
  ShaderVar.ClampMax := ClampMax;
  m_Shaders.SetValue('VarMonotoneClamp', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectDistortMap(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const DistortMap: TG2Texture2DBase;
      const DistortShift: TG2Vec2;
      const Amount: Single = 10
    );
  var ShaderVar: record
    var PixelSize: TG2Vec2;
    var DistortShift: TG2Vec2;
    var Amount: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'DistortMap';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.PixelSize := Input.TexelSize;
  ShaderVar.DistortShift := DistortShift;
  ShaderVar.Amount := Amount;
  m_Shaders.SetValue('VarDistortMap', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[1].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.SetTexture(1, DistortMap.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectDistortMap2(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const DistortMap0: TG2Texture2DBase;
      const DistortMap1: TG2Texture2DBase;
      const DistortShift0: TG2Vec2;
      const DistortShift1: TG2Vec2;
      const Amount: Single = 10
    );
  var ShaderVar: record
    var PixelSize: TG2Vec2;
    var DistortShift0: TG2Vec2;
    var DistortShift1: TG2Vec2;
    var Amount: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'DistortMap2';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.PixelSize := Input.TexelSize;
  ShaderVar.DistortShift0 := DistortShift0;
  ShaderVar.DistortShift1 := DistortShift1;
  ShaderVar.Amount := Amount;
  m_Shaders.SetValue('VarDistortMap2', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[2].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.SetTexture(1, DistortMap0.Texture);
  m_Core.Graphics.Device.SetTexture(2, DistortMap1.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;

procedure TG2PostProcess.EffectBloom(
      const Output: TG2Texture2DRT;
      const Input: TG2Texture2DBase;
      const Power: Single = 1
    );
  var ShaderVar: record
    var PixelSize: TG2Vec2;
    var Power: Single;
  end;
  var PrevRT: IDirect3DSurface9;
  var PrevDS: IDirect3DSurface9;
  var PrevZEnable: Boolean;
begin
  PrevZEnable := m_Core.Graphics.RenderStates.ZEnable;
  m_Core.Graphics.RenderStates.ZEnable := False;
  m_Core.Graphics.Device.GetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.GetDepthStencilSurface(PrevDS);
  m_Core.Graphics.Device.SetDepthStencilSurface(nil);
  m_Shaders.Technique := 'Bloom';
  if Output <> nil then
  begin
    m_Core.Graphics.Device.SetRenderTarget(0, Output.SurfaceRT.Surface);
    LoadBuffer0(
      G2Rect(0, 0, Output.Width - 1, Output.Height - 1),
      Input.DrawRect^
    );
  end
  else
  begin
    m_Core.Graphics.SetRenderTargetDefault;
    LoadBuffer0(
      G2Rect(0, 0, m_Core.Graphics.Params.Width - 1, m_Core.Graphics.Params.Height - 1),
      Input.DrawRect^
    );
  end;
  ShaderVar.PixelSize := Input.TexelSize;
  ShaderVar.Power := Power;
  m_Shaders.SetValue('VarBloom', @ShaderVar, SizeOf(ShaderVar));
  m_Shaders.BeginEffect(nil);
  m_Shaders.BeginPass(0);
  m_VB[0].SetToDevice;
  m_Core.Graphics.Device.SetTexture(0, Input.Texture);
  m_Core.Graphics.Device.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
  m_Shaders.EndPass;
  m_Shaders.EndEffect;
  m_Core.Graphics.Device.SetRenderTarget(0, PrevRT);
  m_Core.Graphics.Device.SetDepthStencilSurface(PrevDS);
  SafeRelease(PrevRT);
  SafeRelease(PrevDS);
  m_Core.Graphics.RenderStates.ZEnable := PrevZEnable;
end;
//TG2PostProcess END

//TG2Thread BEGIN
procedure TG2Thread.Execute;
begin
  if Assigned(m_Proc) then m_Proc();
end;

procedure TG2Thread.OnFinish(Sender: TObject);
begin
  if Assigned(m_OnFinishProc) then
  begin
    m_CS.Enter;
    m_OnFinishProc();
    m_CS.Leave;
  end;
end;

procedure TG2Thread.AfterConstruction;
begin
  m_CS := TCriticalSection.Create;
  OnTerminate := OnFinish;
  inherited AfterConstruction;
end;

procedure TG2Thread.BeforeDestruction;
begin
  m_CS.Free;
end;
//TG2Thread END

//TG2App BEGIN
constructor TG2App.Create;
var
  WndClass: TWndClassExA;
begin
  inherited Create;
  m_g2 := TG2Core.Create;
  m_Gfx := m_g2.Graphics;
  m_Sfx := m_g2.Audio;
  m_Inp := m_g2.Input;
  m_Net := m_g2.Network;
  m_Tmr := m_g2.Timer;
  m_WndClassName := 'Gen2Wnd';
  FillChar(WndClass, SizeOf(TWndClassEx), 0);
  WndClass.cbSize := SizeOf(TWndClassEx);
  WndClass.hIconSm := LoadIcon(MainInstance, 'MAINICON');
  WndClass.hIcon := LoadIcon(MainInstance, 'MAINICON');
  WndClass.hInstance := HInstance;
  WndClass.hCursor := LoadCursor(0, IDC_ARROW);
  WndClass.lpszClassName := PAnsiChar(m_WndClassName);
  WndClass.style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC or CS_DBLCLKS;
  WndClass.lpfnWndProc := @G2DefWndProc;
  if RegisterClassExA(WndClass) = 0 then
  m_WndClassName := 'Static';
  m_Pause := False;
  m_AppTime := 0;
end;

destructor TG2App.Destroy;
var
  i: Integer;
begin
  if m_g2.Initialized then
  begin
    Tmr.Enabled := False;
    Finalize;
  end;
  m_g2.Free;
  for i := 0 to High(m_WindowArr) do
  DestroyWindow(m_WindowArr[i]);
  m_WindowArr := nil;
  inherited Destroy;
end;

function TG2App.WindowCreate(
      const Width: Integer = 0;
      const Height: Integer = 0;
      const Caption: AnsiString = ''
    ): HWnd;
var
  w, h: Integer;
  R: TRect;
  WndStyle: DWord;
begin
  if Width = 0 then w := GetSystemMetrics(SM_CXSCREEN) else w := Width;
  if Height = 0 then h := GetSystemMetrics(SM_CYSCREEN) else h := Height;
  if (Width = 0) and (Height = 0) then
  Result := CreateWindowExA(
    0, PAnsiChar(m_WndClassName), PAnsiChar(Caption),
    WS_POPUP or
    WS_VISIBLE or
    WS_EX_TOPMOST,
    (GetSystemMetrics(SM_CXSCREEN) - w) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - h) div 2,
    w, h, 0, 0, HInstance, nil
  )
  else
  begin
    WndStyle := (
      WS_CAPTION or
      WS_POPUP or
      WS_VISIBLE or
      WS_EX_TOPMOST or
      WS_MINIMIZEBOX or
      WS_MAXIMIZEBOX or
      WS_SYSMENU
    );
    R.Left := (GetSystemMetrics(SM_CXSCREEN) - w) div 2;
    R.Right := R.Left + w;
    R.Top := (GetSystemMetrics(SM_CYSCREEN) - h) div 2;
    R.Bottom := R.Top + h;
    AdjustWindowRect(R, WndStyle, False);
    Result := CreateWindowExA(
      0, PAnsiChar(m_WndClassName), PAnsiChar(Caption),
      WndStyle,
      R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
      0, 0, HInstance, nil
    );
    SetLength(m_WindowArr, Length(m_WindowArr) + 1);
    m_WindowArr[High(m_WindowArr)] := Result;
  end;
end;

procedure TG2App.WindowDestroy(const WindowHandle: HWnd);
var
  i, j: Integer;
begin
  j := Length(m_WindowArr);
  i := 0;
  while i < j do
  begin
    if m_WindowArr[i] = WindowHandle then
    begin
      if i < j - 1 then
      Move(m_WindowArr[i + 1], m_WindowArr[i], SizeOf(WindowHandle) * (i - j - 1));
      SetLength(m_WindowArr, Length(m_WindowArr) - 1);
    end;
  end;
  DestroyWindow(WindowHandle);
end;

procedure TG2App.Loop;
var
  msg: TMsg;
begin
  FillChar(msg, SizeOf(msg), 0);
  while m_Running
  and (msg.message <> WM_QUIT)
  and (msg.message <> WM_DESTROY)
  and (msg.message <> WM_CLOSE) do
  begin
    if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end
    else
    if Tmr.Mode = tmManual then
    Tmr.OnTimer;
  end;
  ExitCode := 0;
end;

procedure TG2App.Initialize;
begin
  m_g2.Initialize;
  m_g2.RequestPlug(TG2PlugTimer, @m_PlugTimer);
  m_PlugTimer.OnTimer := AppTimer;
  m_PlugTimer.OnUpdate := AppUpdate;
  m_PlugTimer.OnRender := AppRender;
  m_g2.RequestPlug(TG2PlugGraphics, @m_PlugGraphics);
  m_PlugGraphics.OnDeviceLost := AppDeviceLost;
  m_PlugGraphics.OnDeviceReset := AppDeviceReset;
  m_PlugGraphics.OnParamsChange := AppParamsChange;
  m_g2.RequestPlug(TG2PlugInput, @m_PlugInput);
  m_PlugInput.OnKeyDown := AppKeyDown;
  m_PlugInput.OnKeyUp := AppKeyUp;
  m_PlugInput.OnMouseDown := AppMouseDown;
  m_PlugInput.OnMouseUp := AppMouseUp;
  m_PlugInput.OnMouseMove := AppMouseMove;
  m_PlugInput.OnWheelMove := AppWheelMove;
  m_PlugInput.OnKeyPress := AppKeyPress;
  m_g2.RequestMod(TG2Render, @m_Render);
  m_g2.RequestMod(TG2Render2D, @m_Render2D);
  m_g2.RequestMod(TG2Render3D, @m_Render3D);
  m_g2.RequestMod(TG2Primitives2D, @m_Prim2D);
  m_g2.RequestMod(TG2Primitives3D, @m_Prim3D);
  m_g2.RequestMod(TG2RenderModes, @m_RenderModes);
  m_g2.RequestMod(TG2Camera, @m_Cam);
  m_Cam.SetPerspective;
  m_Running := True;
end;

procedure TG2App.Finalize;
begin
  m_Tmr.Enabled := False;
  m_g2.ReleaseMod(@m_Cam);
  m_g2.ReleaseMod(@m_RenderModes);
  m_g2.ReleaseMod(@m_Prim3D);
  m_g2.ReleaseMod(@m_Prim2D);
  m_g2.ReleaseMod(@m_Render3D);
  m_g2.ReleaseMod(@m_Render2D);
  m_g2.ReleaseMod(@m_Render);
  m_g2.ReleasePlug(@m_PlugInput);
  m_g2.ReleasePlug(@m_PlugGraphics);
  m_g2.ReleasePlug(@m_PlugTimer);
  m_g2.Finalize;
end;

procedure TG2App.SetHandle(const Value: HWnd);
begin
  m_g2.Handle := Value;
end;

function TG2App.GetHandle: HWnd;
begin
  Result := m_g2.Handle;
end;

procedure TG2App.AppTimer;
begin
  OnTimer;
end;

procedure TG2App.AppRender;
begin
  if m_Render.CanRender then
  OnRender;
end;

procedure TG2App.AppUpdate;
begin
  if not m_Pause then
  m_AppTime := m_AppTime + Round(1000 / m_g2.Timer.TargetUPS);
  OnUpdate;
end;

procedure TG2App.AppDeviceLost;
begin
  OnDeviceLost;
end;

procedure TG2App.AppDeviceReset;
begin
  OnDeviceReset;
end;

procedure TG2App.AppParamsChange;
  var i: Integer;
  var WndStyle: DWord;
  var R: TRect;
begin
  for i := 0 to High(m_WindowArr) do
  if Handle = m_WindowArr[i] then
  begin
    WndStyle := (
      WS_CAPTION or
      WS_POPUP or
      WS_VISIBLE or
      WS_EX_TOPMOST or
      WS_MINIMIZEBOX or
      WS_MAXIMIZEBOX or
      WS_SYSMENU
    );
    R.Left := (GetSystemMetrics(SM_CXSCREEN) - Gfx.Params.Width) div 2;
    R.Right := R.Left + Gfx.Params.Width;
    R.Top := (GetSystemMetrics(SM_CYSCREEN) - Gfx.Params.Height) div 2;
    R.Bottom := R.Top + Gfx.Params.Height;
    AdjustWindowRect(R, WndStyle, False);
    SetWindowPos(Handle, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, 0);
    Break;
  end;
  OnParamsChange;
end;

procedure TG2App.AppKeyDown(const Key: Byte);
begin
  OnKeyDown(Key);
end;

procedure TG2App.AppKeyUp(const Key: Byte);
begin
  OnKeyUp(Key);
end;

procedure TG2App.AppKeyPress(const Key: AnsiChar);
begin
  OnKeyPress(Key);
end;

procedure TG2App.AppMouseDown(const Button: Byte);
begin
  OnMouseDown(Button);
end;

procedure TG2App.AppMouseUp(const Button: Byte);
begin
  OnMouseUp(Button);
end;

procedure TG2App.AppMouseMove(const Shift: TPoint);
begin
  OnMouseMove(Shift);
end;

procedure TG2App.AppWheelMove(const Shift: Integer);
begin
  OnWheelMove(Shift);
end;

procedure TG2App.OnTimer;
begin

end;

procedure TG2App.OnRender;
begin

end;

procedure TG2App.OnUpdate;
begin

end;

procedure TG2App.OnDeviceLost;
begin

end;

procedure TG2App.OnDeviceReset;
begin

end;

procedure TG2App.OnParamsChange;
begin

end;

procedure TG2App.OnKeyDown(const Key: Byte);
begin

end;

procedure TG2App.OnKeyUp(const Key: Byte);
begin

end;

procedure TG2App.OnKeyPress(const Key: AnsiChar);
begin

end;

procedure TG2App.OnMouseDown(const Button: Byte);
begin

end;

procedure TG2App.OnMouseUp(const Button: Byte);
begin

end;

procedure TG2App.OnMouseMove(const Shift: TPoint);
begin

end;

procedure TG2App.OnWheelMove(const Shift: Integer);
begin

end;
//TG2App END

//Uyility Functions BEGIN
function G2ResOk(const Res: TG2Result): Boolean;
begin
  Result := Res in TG2ResSuccess;
end;

function G2ResFail(const Res: TG2Result): Boolean;
begin
  Result := not (Res in TG2ResSuccess);
end;

procedure SafeRelease(var i);
begin
  if IUnknown(i) <> nil then IUnknown(i) := nil;
end;

function WideStringToString(const WS: WideString; codePage: Word = CP_ACP): AnsiString;
var
  l: integer;
begin
  if WS = '' then
  Result := ''
  else
  begin
    l := WideCharToMultiByte(
      CodePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @WS[1], - 1, nil, 0, nil, nil
    );
    SetLength(Result, l - 1);
    if l > 1 then
    WideCharToMultiByte(
      CodePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @WS[1], - 1, @Result[1], l - 1, nil, nil
    );
  end;
end;

function StringToWideString(const S: AnsiString; codePage: Word = CP_ACP): WideString;
var
  l: integer;
begin
  if s = '' then
  Result := ''
  else
  begin
    l := MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PAnsiChar(@s[1]), - 1, nil, 0);
    SetLength(Result, l - 1);
    if l > 1 then
    MultiByteToWideChar(
      CodePage,
      MB_PRECOMPOSED,
      PAnsiChar(@S[1]),
      - 1,
      PWideChar(@Result[1]),
      l - 1
    );
  end;
end;

function G2Param(const Str: AnsiString; const Separator: AnsiString; const Param: Integer): AnsiString;
var
  i: Integer;
  j: Integer;
  CurParam: Integer;
  PrevParamIndex: Integer;
  b: Boolean;
begin
  Result := '';
  b := False;
  CurParam := 0;
  PrevParamIndex := 1;
  for i := 1 to Length(Str) do
  begin
    for j := 1 to Length(Separator) do
    begin
      if Str[i + j] <> Separator[j] then
      Break
      else
      begin
        if j = Length(Separator) then
        begin
          if CurParam = Param then
          begin
            b := True;
            SetLength(Result, i - PrevParamIndex + 1);
            move(Str[PrevParamIndex], Result[1], Length(Result));
          end
          else
          begin
            CurParam := CurParam + 1;
            PrevParamIndex := i + Length(Separator) + 1;
          end;
        end;
      end;
    end;
    if b then Break;
    if (i = Length(Str)) and (CurParam = Param) then
    begin
      SetLength(Result, i - PrevParamIndex + 1);
      Move(Str[PrevParamIndex], Result[1], Length(Result));
    end;
  end;
end;

function G2ParamCount(const Str: AnsiString; const Separator: AnsiString): Integer;
var
  i, j: Integer;
  DoInc: Boolean;
begin
  if Length(Str) <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  Result := 1;
  for i := 1 to Length(Str) - Length(Separator) do
  begin
    DoInc := True;
    for j := 1 to Length(Separator) do
    if Str[i + j - 1] <> Separator[j] then
    begin
      DoInc := False;
      Break;
    end;
    if DoInc then
    Result := Result + 1;
  end;
end;

function G2StrInStr(const Str: AnsiString; SubStr: AnsiString): Integer;
var
  i: Integer;
begin
  if Length(SubStr) > 0 then
  begin
    for i := 1 to Length(Str) + 1 - Length(SubStr) do
    if CompareMem(@Str[i], @SubStr[1], Length(SubStr)) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := 0;
end;

function G2Color(const R, G, B: Byte; const A: Byte = 255): TG2Color;
begin
  Result.b := B;
  Result.g := G;
  Result.r := R;
  Result.a := A;
end;

function G2Color(const Color: DWord): TG2Color;
begin
  Result := PG2Color(@Color)^;
end;

function G2Color(const v: TG2Vec3): TG2Color;
begin
  Result := G2Color(
    Trunc((v.x * 0.5 + 0.5) * 255),
    Trunc((v.y * 0.5 + 0.5) * 255),
    Trunc((v.z * 0.5 + 0.5) * 255)
  );
end;

function G2Color(const v: TG2Vec4): TG2Color;
begin
  Result := G2Color(
    Trunc(v.x * 255),
    Trunc(v.y * 255),
    Trunc(v.z * 255),
    Trunc(v.w * 255)
  );
end;

function G2Rect(const Left, Top, Right, Bottom: Single): TG2Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function G2FormatSize(const Format: TD3DFormat): DWord;
begin
  case Format of
    D3DFMT_R8G8B8: Result := 3;
    D3DFMT_A8R8G8B8: Result := 4;
    D3DFMT_X8R8G8B8: Result := 4;
    D3DFMT_R5G6B5: Result := 2;
    D3DFMT_X1R5G5B5: Result := 2;
    D3DFMT_A1R5G5B5: Result := 2;
    D3DFMT_A4R4G4B4: Result := 2;
    D3DFMT_R3G3B2: Result := 1;
    D3DFMT_A8: Result := 1;
    D3DFMT_A8R3G3B2: Result := 2;
    D3DFMT_X4R4G4B4: Result := 2;
    D3DFMT_A2B10G10R10: Result := 4;
    D3DFMT_A8B8G8R8: Result := 4;
    D3DFMT_X8B8G8R8: Result := 4;
    D3DFMT_G16R16: Result := 4;
    D3DFMT_A2R10G10B10: Result := 4;
    D3DFMT_A16B16G16R16: Result := 8;
    D3DFMT_A8P8: Result := 2;
    D3DFMT_P8: Result := 1;
    D3DFMT_L8: Result := 1;
    D3DFMT_A8L8: Result := 2;
    D3DFMT_A4L4: Result := 1;
    D3DFMT_V8U8: Result := 2;
    D3DFMT_L6V5U5: Result := 2;
    D3DFMT_X8L8V8U8: Result := 4;
    D3DFMT_Q8W8V8U8: Result := 4;
    D3DFMT_V16U16: Result := 4;
    D3DFMT_A2W10V10U10: Result := 4;
    D3DFMT_D16_LOCKABLE: Result := 2;
    D3DFMT_D32: Result := 4;
    D3DFMT_D15S1: Result := 2;
    D3DFMT_D24S8: Result := 4;
    D3DFMT_D24X8: Result := 4;
    D3DFMT_D24X4S4: Result := 4;
    D3DFMT_D16: Result := 2;
    D3DFMT_D32F_LOCKABLE: Result := 4;
    D3DFMT_D24FS8: Result := 4;
    D3DFMT_L16: Result := 2;
    D3DFMT_INDEX16: Result := 2;
    D3DFMT_INDEX32: Result := 4;
    D3DFMT_Q16W16V16U16: Result := 8;
    D3DFMT_R16F: Result := 2;
    D3DFMT_G16R16F: Result := 4;
    D3DFMT_A16B16G16R16F: Result := 8;
    D3DFMT_R32F: Result := 4;
    D3DFMT_G32R32F: Result := 8;
    D3DFMT_A32B32G32R32F: Result := 16;
  else
    Result := 0;
  end;
end;

function G2FormatToString(const Format: TD3DFormat): AnsiString;
begin
  case Format of
    D3DFMT_R8G8B8: Result := 'R8G8B8';
    D3DFMT_A8R8G8B8: Result := 'A8R8G8B8';
    D3DFMT_X8R8G8B8: Result := 'X8R8G8B8';
    D3DFMT_R5G6B5: Result := 'R5G6B5';
    D3DFMT_X1R5G5B5: Result := 'X1R5G5B5';
    D3DFMT_A1R5G5B5: Result := 'A1R5G5B5';
    D3DFMT_A4R4G4B4: Result := 'A4R4G4B4';
    D3DFMT_R3G3B2: Result := 'R3G3B2';
    D3DFMT_A8: Result := 'A8';
    D3DFMT_A8R3G3B2: Result := 'A8R3G3B2';
    D3DFMT_X4R4G4B4: Result := 'X4R4G4B4';
    D3DFMT_A2B10G10R10: Result := 'A2B10G10R10';
    D3DFMT_A8B8G8R8: Result := 'A8B8G8R8';
    D3DFMT_X8B8G8R8: Result := 'X8B8G8R8';
    D3DFMT_G16R16: Result := 'G16R16';
    D3DFMT_A2R10G10B10: Result := 'A2R10G10B10';
    D3DFMT_A16B16G16R16: Result := 'A16B16G16R16';
    D3DFMT_A8P8: Result := 'A8P8';
    D3DFMT_P8: Result := 'P8';
    D3DFMT_L8: Result := 'L8';
    D3DFMT_A8L8: Result := 'A8L8';
    D3DFMT_A4L4: Result := 'A4L4';
    D3DFMT_V8U8: Result := 'V8U8';
    D3DFMT_L6V5U5: Result := 'L6V5U5';
    D3DFMT_X8L8V8U8: Result := 'X8L8V8U8';
    D3DFMT_Q8W8V8U8: Result := 'Q8W8V8U8';
    D3DFMT_V16U16: Result := 'V16U16';
    D3DFMT_A2W10V10U10: Result := 'A2W10V10U10';
    D3DFMT_A8X8V8U8: Result := 'A8X8V8U8';
    D3DFMT_L8X8V8U8: Result := 'L8X8V8U8';
    D3DFMT_UYVY: Result := 'UYVY';
    D3DFMT_RGBG: Result := 'RGBG';
    D3DFMT_YUY2: Result := 'YUY2';
    D3DFMT_GRGB: Result := 'GRGB';
    D3DFMT_DXT1: Result := 'DXT1';
    D3DFMT_DXT2: Result := 'DXT2';
    D3DFMT_DXT3: Result := 'DXT3';
    D3DFMT_DXT4: Result := 'DXT4';
    D3DFMT_DXT5: Result := 'DXT5';
    D3DFMT_D16_LOCKABLE: Result := 'D16_LOCKABLE';
    D3DFMT_D32: Result := 'D32';
    D3DFMT_D15S1: Result := 'D15S1';
    D3DFMT_D24S8: Result := 'D24S8';
    D3DFMT_D24X8: Result := 'D24X8';
    D3DFMT_D24X4S4: Result := 'D24X4S4';
    D3DFMT_D16: Result := 'D16';
    D3DFMT_D32F_LOCKABLE: Result := 'D32F_LOCKABLE';
    D3DFMT_D24FS8: Result := 'D24FS8';
    D3DFMT_L16: Result := 'L16';
    D3DFMT_VERTEXDATA: Result := 'VERTEXDATA';
    D3DFMT_INDEX16: Result := 'INDEX16';
    D3DFMT_INDEX32: Result := 'INDEX32';
    D3DFMT_Q16W16V16U16: Result := 'Q16W16V16U16';
    D3DFMT_MULTI2_ARGB8: Result := 'MULTI2_ARGB8';
    D3DFMT_R16F: Result := 'R16F';
    D3DFMT_G16R16F: Result := 'G16R16F';
    D3DFMT_A16B16G16R16F: Result := 'A16B16G16R16F';
    D3DFMT_R32F: Result := 'R32F';
    D3DFMT_G32R32F: Result := 'G32R32F';
    D3DFMT_A32B32G32R32F: Result := 'A32B32G32R32F';
    D3DFMT_CxV8U8: Result := 'CxV8U8';
    D3DFMT_FORCE_DWORD: Result := 'FORCE_DWORD';
  else
    Result := 'Unknown';
  end;
end;

function G2SysInfo: TG2SysInfo;
  {$IFNDEF IDE_D2009_UP}
  type TOSVersionInfoExA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved:BYTE;
  end;
  const SM_SERVERR2 = 89;
  {$ENDIF}
  var Reg: TRegistry;
  var dd: TDisplayDeviceA;
  var i: Integer;
  var DDraw: IDirectDraw7;
  var DDCaps: TDDCaps;
  var WinInfo: TOSVersionInfoExA;
  var SysInfo: TSystemInfo;
  const VER_SERVER_NT = $80000000;
  const VER_WORKSTATION_NT = $40000000;
  const VER_SUITE_SMALLBUSINESS = $00000001;
  const VER_SUITE_ENTERPRISE = $00000002;
  const VER_SUITE_BACKOFFICE = $00000004;
  const VER_SUITE_COMMUNICATIONS = $00000008;
  const VER_SUITE_TERMINAL = $00000010;
  const VER_SUITE_SMALLBUSINESS_RESTRICTED = $00000020;
  const VER_SUITE_EMBEDDEDNT = $00000040;
  const VER_SUITE_DATACENTER = $00000080;
  const VER_SUITE_SINGLEUSERTS = $00000100;
  const VER_SUITE_PERSONAL = $00000200;
  const VER_SUITE_BLADE = $00000400;
  const VER_SUITE_EMBEDDED_RESTRICTED = $00000800;
  const VER_SUITE_SECURITY_APPLIANCE = $00001000;
  const VER_SUITE_STORAGE_SERVER = $00002000;
  const VER_SUITE_COMPUTE_SERVER = $00004000;
  const VER_SUITE_WH_SERVER = $00008000;
  const VER_NT_WORKSTATION = $0000001;
  const VER_NT_DOMAIN_CONTROLLER = $0000002;
  const VER_NT_SERVER = $0000003;
  const PROCESSOR_ARCHITECTURE_INTEL = 0;
  const PROCESSOR_ARCHITECTURE_IA64 = 7;
  const PROCESSOR_ARCHITECTURE_AMD64 = 9;
begin
  Result.CPUCount := 0;
  SetLength(Result.CPUs, 0);
  ZeroMemory(@Result.Memory, SizeOf(Result.Memory));
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    i := 0;
    while Reg.OpenKey('\Hardware\Description\System\CentralProcessor\' + IntToStr(i), False) do
    begin
      SetLength(Result.CPUs, i + 1);
      Result.CPUs[i].Name := AnsiString(Reg.ReadString('ProcessorNameString'));
      Result.CPUs[i].Identifier := AnsiString(Reg.ReadString('Identifier'));
      Result.CPUs[i].Vendor := AnsiString(Reg.ReadString('VendorIdentifier'));
      Result.CPUs[i].MHz := Reg.ReadInteger('~MHz');
      Inc(i);
    end;
    Result.CPUCount := Length(Result.CPUs);
    Result.Memory.dwLength := SizeOf(Result.Memory);
    GlobalMemoryStatus(Result.Memory);
    if Reg.OpenKey('\Software\Microsoft\DirectX\', False) then
    Result.DXVersion := Reg.ReadString('Version')
    else
    Result.DXVersion := 'Unspecified';
    i := 0;
    dd.cb := SizeOf(dd);
    while EnumDisplayDevicesA(nil, i, dd, 0) do
    begin
      if dd.StateFlags and DISPLAY_DEVICE_PRIMARY_DEVICE > 0 then
      Result.DisplayDevice := dd.DeviceString;
      Inc(i);
    end;
  finally
    Reg.Free;
  end;
  if Succeeded(
    DirectDrawCreateEx(
      nil,
      DDraw,
      IID_IDirectDraw7,
      nil
    )
  ) then
  begin
    DDCaps.dwSize := SizeOf(TDDCaps);
    DDraw.GetCaps(@DDCaps, nil);
    Result.VideoMemoryTotal := DDCaps.dwVidMemTotal;
    Result.VideoMemoryFree := DDCaps.dwVidMemFree;
  end
  else
  begin
    Result.VideoMemoryTotal := 0;
    Result.VideoMemoryFree := 0;
  end;
  Result.OSVersion := '';
  ZeroMemory(@WinInfo, SizeOf(WinInfo));
  WinInfo.dwOSVersionInfoSize := SizeOf(WinInfo);
  ZerOMemory(@SysInfo, SizeOf(SysInfo));
  Result.OSVersion := 'Unknown';
  if GetVersionExA(POSVersionInfoA(@WinInfo)^) then
  begin
    GetSystemInfo(SysInfo);
    if (WinInfo.dwPlatformId = VER_PLATFORM_WIN32_NT)
    and (WinInfo.dwMajorVersion > 4) then
    begin
      Result.OSVersion := 'Microsoft ';
      if WinInfo.dwMajorVersion = 6 then
      begin
        if WinInfo.dwMinorVersion = 0 then
        begin
          if WinInfo.wProductType = VER_NT_WORKSTATION then
          Result.OSVersion := Result.OSVersion + 'Windows Vista'
          else
          Result.OSVersion := Result.OSVersion + 'Windows Server 2008';
        end
        else if WinInfo.dwMinorVersion = 1 then
        begin
          if WinInfo.wProductType = VER_NT_WORKSTATION then
          Result.OSVersion := Result.OSVersion + 'Windows 7'
          else
          Result.OSVersion := Result.OSVersion + 'Windows Sercer 2008 R2';
        end;
      end
      else if (WinInfo.dwMajorVersion = 5)
      and (WinInfo.dwMinorVersion = 2) then
      begin
        if (GetSystemMetrics(SM_SERVERR2) > 0) then
        Result.OSVersion := Result.OSVersion + 'Windows Server 2003 R2'
        else if (WinInfo.wSuiteMask and VER_SUITE_STORAGE_SERVER > 0) then
        Result.OSVersion := Result.OSVersion + 'Windows Storage Server 2003'
        else if (WinInfo.wSuiteMask and VER_SUITE_WH_SERVER > 0) then
        Result.OSVersion := Result.OSVersion + 'Windows Home Server'
        else if (WinInfo.wProductType = VER_NT_WORKSTATION)
        and (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
        Result.OSVersion := Result.OSVersion + 'Windows XP Proffessional x64'
        else Result.OSVersion := Result.OSVersion + 'Windows Server 2003';
        if (WinInfo.wProductType <> VER_NT_WORKSTATION) then
        begin
          if (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64) then
          begin
            if (WinInfo.wSuiteMask and VER_SUITE_DATACENTER > 0) then
            Result.OSVersion := Result.OSVersion + ' Datacenter Edition for Itanium-based Systems'
            else if (WinInfo.wSuiteMask and VER_SUITE_ENTERPRISE > 0) then
            Result.OSVersion := Result.OSVersion + ' Enterprise Edition for Itanium-based Systems'
            else if (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
            begin
              if (WinInfo.wSuiteMask and VER_SUITE_DATACENTER > 0) then
              Result.OSVersion := Result.OSVersion + ' Datacenter x64 Edition'
              else if (WinInfo.wSuiteMask and VER_SUITE_ENTERPRISE > 0) then
              Result.OSVersion := Result.OSVersion + ' Enterprise x64 Edition'
              else Result.OSVersion := Result.OSVersion + ' Standard x64 Edition';
            end
            else
            begin
              if (WinInfo.wSuiteMask and VER_SUITE_COMPUTE_SERVER > 0) then
              Result.OSVersion := Result.OSVersion + ' Compute Cluster Edition'
              else if (WinInfo.wSuiteMask and VER_SUITE_DATACENTER > 0) then
              Result.OSVersion := Result.OSVersion + ' Datacenter Edition'
              else if (WinInfo.wSuiteMask and VER_SUITE_ENTERPRISE > 0) then
              Result.OSVersion := Result.OSVersion + ' Enterprise Edition'
              else if (WinInfo.wSuiteMask and VER_SUITE_BLADE > 0) then
              Result.OSVersion := Result.OSVersion + ' Web Edition'
              else Result.OSVersion := Result.OSVersion + ' Standard Edition';
            end;
          end;
        end;
      end
      else if (WinInfo.dwMajorVersion = 5)
      and (WinInfo.dwMinorVersion = 1) then
      begin
        Result.OSVersion := Result.OSVersion + 'Windows XP';
        if (WinInfo.wSuiteMask and VER_SUITE_PERSONAL > 0) then
        Result.OSVersion := Result.OSVersion + ' Home Edition'
        else Result.OSVersion := Result.OSVersion + ' Professional';
      end
      else if (WinInfo.dwMajorVersion = 5)
      and (WinInfo.dwMinorVersion = 0) then
      begin
        Result.OSVersion := Result.OSVersion + 'Windows 2000';
        if (WinInfo.wProductType = VER_NT_WORKSTATION) then
        Result.OSVersion := Result.OSVersion + ' Professional'
        else
        begin
          if (WinInfo.wSuiteMask and VER_SUITE_DATACENTER > 0) then
          Result.OSVersion := Result.OSVersion + ' Datacenter Server'
          else if (WinInfo.wSuiteMask and VER_SUITE_ENTERPRISE > 0) then
          Result.OSVersion := Result.OSVersion + ' Advanced Server'
          else Result.OSVersion := Result.OSVersion + ' Server';
        end;
      end;
      Result.OSVersion := Result.OSVersion + ' ' + String(WinInfo.szCSDVersion);
      if (WinInfo.dwMajorVersion >= 6) then
      begin
        if (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
        Result.OSVersion := Result.OSVersion + ', 64-bit'
        else if (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_INTEL) then
        Result.OSVersion := Result.OSVersion + ', 32-bit';
      end;
    end
    else if WinInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    begin
      Result.OSVersion := 'Microsoft ';
      if (WinInfo.dwMajorVersion = 4)
      and (WinInfo.dwMinorVersion = 0) then
      Result.OSVersion := 'Windows 95'
      else if (WinInfo.dwMajorVersion = 4)
      and (WinInfo.dwMinorVersion = 10) then
      Result.OSVersion := 'Windows 98'
      else if (WinInfo.dwMajorVersion = 4)
      and (WinInfo.dwMinorVersion = 90) then
      Result.OSVersion := 'Windows ME';
    end;
  end;
end;

function G2PiTime(Amp: Single = 1000): Single;
begin
  Result := (GetTickCount mod Round(TwoPi * Amp)) / (Amp);
end;

function G2PiTime(Amp: Single; Time: DWord): Single;
begin
  Result := (Time mod Round(TwoPi * Amp)) / (Amp);
end;

function G2TimeInterval(Interval: DWord = 1000): Single;
begin
  Result := (GetTickCount mod Interval) / Interval;
end;

function G2TimeInterval(Interval: DWord; Time: DWord): Single;
begin
  Result := (Time mod Interval) / Interval;
end;

function G2RandomPi: Single;
begin
  Result := Random(Round(Pi * 1000)) / 1000;
end;

function G2Random2Pi: Single;
begin
  Result := Random(Round(TwoPi * 1000)) / 1000;
end;

function G2RandomCirclePoint: TG2Vec2;
  var a: Single;
begin
  a := G2Random2Pi;
  G2SinCos(a, Result.y, Result.x);
end;

function G2RandomSpherePoint: TG2Vec3;
  var a1, a2, s1, s2, c1, c2: Single;
begin
  a1 := G2Random2Pi;
  a2 := G2Random2Pi;
  G2SinCos(a1, s1, c1);
  G2SinCos(a2, s2, c2);
  Result.SetValue(c1 * c2, s2, s1 * c2);
end;

function G2RandomColor(const MinBrightness: Byte = 0): DWord;
var
  bgra: record b, g, r, a: Byte; end absolute Result;
  r: Byte;
begin
  r := 255 - MinBrightness;
  bgra.b := MinBrightness + Random(r);
  bgra.g := MinBrightness + Random(r);
  bgra.r := MinBrightness + Random(r);
  bgra.a := 255;
end;

procedure G2EncDec(const Ptr: PByteArray; const Count: Integer; const Key: AnsiString);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  Ptr^[i] := Ptr^[i] xor Ord(Key[(i mod Length(Key)) + 1]);
end;

function G2RectVsRect(const R1, R2: TRect): Boolean;
begin
  Result := (
    (R1.Left <= R2.Right)
    and (R1.Right >= R2.Left)
    and (R1.Top <= R2.Bottom)
    and (R1.Bottom >= R2.Top)
  );
end;

function G2RectBuild(const Pt1, Pt2: TPoint): TRect;
begin
  if Pt1.X < Pt2.X then
  begin
    Result.Left := Pt1.X;
    Result.Right := Pt2.X;
  end
  else
  begin
    Result.Left := Pt2.X;
    Result.Right := Pt1.X;
  end;
  if Pt1.Y < Pt2.Y then
  begin
    Result.Top := Pt1.Y;
    Result.Bottom := Pt2.Y;
  end
  else
  begin
    Result.Top := Pt2.Y;
    Result.Bottom := Pt1.Y;
  end;
end;

function DIKToChar(const Key: Byte): AnsiChar;
var
  Layout: HKL;
begin
  Layout := GetKeyboardLayout(0);
  Result := AnsiChar(MapVirtualKeyEx(Key, 1, Layout));
end;

function D3DTS_TEXTUREMATRIX(const Index: Byte): TD3DTransformStateType;
begin
  Result := TD3DTransformStateType(16 + Index);
end;

function D3DVertexElement(
    Stream:     Word;
    Offset:     Word;
    _Type:      TD3DDeclType;
    Method:     TD3DDeclMethod;
    Usage:      TD3DDeclUsage;
    UsageIndex: Byte
  ): TD3DVertexElement9;
begin
  Result.Stream := Stream;
  Result.Offset := Offset;
  Result._Type := _Type;
  Result.Method := Method;
  Result.Usage := Usage;
  Result.UsageIndex := UsageIndex;
end;

function G2DefWndProc(Wnd: HWnd; Msg: UInt; wParam: WPARAM; lParam: LPARAM): LResult; stdcall;
begin
  case Msg of
    WM_DESTROY, WM_QUIT, WM_CLOSE:
    begin
      PostQuitMessage(0);
      Result := 0;
      Exit;
    end;
  end;
  Result := DefWindowProcA(Wnd, Msg, wParam, lParam);
end;

function G2InputWndProc(Wnd: HWnd; Msg: UInt; wParam: WPARAM; lParam: LPARAM): LResult; stdcall;
var
  Input: TG2Input;
begin
  Input := TG2Input(GetWindowLong(Wnd, GWL_USERDATA));
  if IsWindowUnicode(Wnd) then
  begin
    case Msg of
      WM_CHAR:
      begin
        Input.KeyPress(WideStringToString(WideChar(wParam))[1]);
      end;
    end;
    Result := CallWindowProcW(Input.PrevWndProc, Wnd, Msg, wParam, lParam);
  end
  else
  begin
    case Msg of
      WM_CHAR:
      begin
        Input.KeyPress(AnsiChar(wParam));
      end;
    end;
    Result := CallWindowProcA(Input.PrevWndProc, Wnd, Msg, wParam, lParam);
  end;
end;

function G2WSAErrorToStr(const Error: Integer): AnsiString;
begin
  case Error of
    0: Result := 'No error';
    10004: Result := 'Interrupted function call';
    10013: Result := 'Permission denied';
    10014: Result := 'Bad address';
    10022: Result := 'Invalid argument';
    10024: Result := 'Too many open files';
    10035: Result := 'Resource temporarily unavailable';
    10036: Result := 'Operation now in progress';
    10037: Result := 'Operation already in progress';
    10038: Result := 'Socket operation on non-socket';
    10039: Result := 'Destination address required';
    10040: Result := 'Message too long';
    10041: Result := 'Protocol wrong type for socket';
    10042: Result := 'Bad protocol option';
    10043: Result := 'Protocol not supported';
    10044: Result := 'Socket type not supported';
    10045: Result := 'Operation not supported';
    10046: Result := 'Protocol family not supported';
    10047: Result := 'Address family not supported by protocol family';
    10048: Result := 'Address already in use';
    10049: Result := 'Cannot assign requested address';
    10050: Result := 'Network is down';
    10051: Result := 'Network is unreachable';
    10052: Result := 'Network dropped connection on reset';
    10053: Result := 'Software caused connection abort';
    10054: Result := 'Connection reset by peer';
    10055: Result := 'No buffer space available';
    10056: Result := 'Socket is already connected';
    10057: Result := 'Socket is not connected';
    10058: Result := 'Cannot send after socket shutdown';
    10060: Result := 'Connection timed out';
    10061: Result := 'Connection refused';
    10064: Result := 'Host is down';
    10065: Result := 'No route to host';
    10067: Result := 'Too many processes';
    10091: Result := 'Network subsystem is unavailable';
    10092: Result := 'WINSOCK.DLL version out of range';
    10093: Result := 'Successful WSAStartup not yet performed';
    10094: Result := 'Graceful shutdown in progress';
    11001: Result := 'Host not found';
    11002: Result := 'Non-authoritative host not found';
    11003: Result := 'This is a non-recoverable error';
    11004: Result := 'Valid name, no data record of requested type';
    else Result := 'Unknown error';
  end;
end;

procedure G2BreakPoint;
asm
  int 3;
end;
//Utility Functions END

//Unit Functions BEGIN
procedure G2WriteLog(const Log: AnsiString);
{$IFDEF G2_WRITE_LOG}
var
  LogHandle: HFile;
  BytesWritten: DWord;
  LogStr: AnsiString;
const
  FILE_APPEND_DATA = $00000004;
{$ENDIF}
begin
  {$IFDEF G2_WRITE_LOG}
  LogStr := Log + #$D#$A;
  LogHandle := CreateFileW(
    PWideChar(AppPath + 'G2Log.txt'),
    FILE_APPEND_DATA,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil,
    OPEN_ALWAYS,
    FILE_FLAG_RANDOM_ACCESS,
    0
  );
  if LogHandle = INVALID_HANDLE_VALUE then Exit;
  try
    if WriteFile(
      LogHandle,
      LogStr[1],
      Length(LogStr),
      BytesWritten,
      nil
    ) then
    WaitForSingleObject(LogHandle, INFINITE);
  finally
    CloseHandle(LogHandle);
  end;
  {$ENDIF}
end;

procedure G2WriteLogTimed(const Log: AnsiString; const Module: AnsiString = '');
{$IFDEF G2_WRITE_LOG}
var
  t, h, m, s: DWord;
  LogStr, ModStr, sh, sm, ss: AnsiString;
const
  TSec = 1000;
  TMin = TSec * 60;
  THrs = TMin * 60;
  ModuleStringSize = 12;
{$ENDIF}
begin
  {$IFDEF G2_WRITE_LOG}
  {$Warnings off}
  t := GetTickCount - AppTime;
  h := t div THrs;
  m := (t - (h * THrs)) div TMin;
  s := (t - (h * THrs) - (m * TMin)) div TSec;
  if s < 10 then ss := '0' + IntToStr(s) else ss := IntToStr(s);
  if m < 10 then sm := '0' + IntToStr(m) else sm := IntToStr(m);
  if h < 10 then sh := '0' + IntToStr(h) else sh := IntToStr(h);
  LogStr := '[' + sh + ':' + sm + ':' + ss + '] ';
  if Length(Module) > 0 then
  begin
    SetLength(ModStr, ModuleStringSize);
    FillChar(ModStr[1], ModuleStringSize, Ord('='));
    Move(Module[1], ModStr[(ModuleStringSize - Length(Module)) div 2 + 1], Min(Length(Module), ModuleStringSize));
    LogStr := LogStr + '[' + ModStr + '] - ';
  end;
  LogStr := LogStr + Log;
  G2WriteLog(LogStr);
  {$Warnings on}
  {$ENDIF}
end;

procedure LogStart;
{$IFDEF G2_WRITE_LOG}
var
  SysInfo: TG2SysInfo;
  s: String;
{$ENDIF}
begin
  {$Warnings off}
  DeleteFile(AppPath + 'G2Log.txt');
  G2WriteLog('Gen2 v' + IntToStr((G2Version shr 8) and $ff) + '.' + IntToStr(G2Version and $ff) + ' Log');
  G2WriteLog('Application Compiler: ' + AppCompiler);
  G2WriteLog('Gen2 Config Options:');
  {$IFDEF G2_WRITE_LOG}
  G2WriteLog('  Write Log - ON');
  {$ELSE}
  G2WriteLog('  Write Log - OFF');
  {$ENDIF}
  {$IFDEF G2_REPORT_LEAKS}
  G2WriteLog('  Report Memory Leaks - ON');
  {$ELSE}
  G2WriteLog('  Report Memory Leaks - OFF');
  {$ENDIF}
  {$IFDEF G2_PERFHUD}
  G2WriteLog('  Enable PerfHUD - ON');
  {$ELSE}
  G2WriteLog('  Enable PerfHUD - OFF');
  {$ENDIF}
  {$IFDEF G2_USE_INLINE}
  G2WriteLog('  Use Inline - ON');
  {$ELSE}
  G2WriteLog('  Use Inline - OFF');
  {$ENDIF}
  {$IFDEF G2_WRITE_LOG}
  SysInfo := G2SysInfo;
  G2WriteLog('System Specs:');
  G2WriteLog('  OS: ' + SysInfo.OSVersion);
  G2WriteLog('  CPU: ');
  if Length(SysInfo.CPUs) > 0 then
  begin
    G2WriteLog('    Name: ' + SysInfo.CPUs[0].Name);
    G2WriteLog('    Identifier: ' + SysInfo.CPUs[0].Identifier);
    G2WriteLog('    Vendor: ' + SysInfo.CPUs[0].Vendor);
    G2WriteLog('    Speed: ' + IntToStr(SysInfo.CPUs[0].MHz) + 'MHz');
    G2WriteLog('    Core Count: ' + IntToStr(SysInfo.CPUCount));
  end
  else
  G2WriteLog('    Access Denied');
  if SysMMX or SysSSE or SysSSE2 or SysSSE3 then
  begin
    s := '    Extensions: ';
    if SysMMX then
    s := s + '[MMX]';
    if SysSSE then
    s := s + '[SSE]';
    if SysSSE2 then
    s := s + '[SSE2]';
    if SysSSE3 then
    s := s + '[SSE3]';
    G2WriteLog(s);
  end;
  G2WriteLog('  Memory:');
  G2WriteLog('    Total: ' + IntToStr(SysInfo.Memory.dwTotalPhys div 1048576) + 'Mb');
  G2WriteLog('    Available: ' + IntToStr(SysInfo.Memory.dwAvailPhys div 1048576) + 'Mb');
  G2WriteLog('    ' + IntToStr(SysInfo.Memory.dwMemoryLoad) + '% in use');
  G2WriteLog('  Video Memory:');
  G2WriteLog('    Total: ' + IntToStr(SysInfo.VideoMemoryTotal div 1048576) + 'Mb');
  G2WriteLog('    Available: ' + IntToStr(SysInfo.VideoMemoryFree div 1048576) + 'Mb');
  G2WriteLog('  Display Device: ' + SysInfo.DisplayDevice);
  G2WriteLog('DirectX Info:');
  G2WriteLog('  D3DX SDK Version: ' + IntToStr(D3DX_SDK_VERSION));
  G2WriteLog('  D3DX Lib: ' + d3dx9dll);
  G2WriteLog('  DirectX Installed Version: ' + SysInfo.DXVersion);
  G2WriteLogTimed('Application started.');
  {$ENDIF}
  {$Warnings on}
end;

procedure LogStop;
{$IFDEF G2_WRITE_LOG}
var
  SysInfo: TG2SysInfo;
  PrevVRAMFree: DWord;
{$ENDIF}
begin
  {$IFDEF G2_WRITE_LOG}
  PrevVRAMFree := SysInfo.VideoMemoryFree;
  SysInfo := G2SysInfo;
  if PrevVRAMFree > SysInfo.VideoMemoryFree then
  G2WriteLog(AnsiString('VRAM Leak: ' + IntToStr((PrevVRAMFree - SysInfo.VideoMemoryFree) div 1048576) + 'Mb'));
  {$ENDIF}
  G2WriteLogTimed('Application closed.');
end;
//Unit Functions END

initialization
begin
  {$Warnings off}
  AppTime := GetTickCount;
  AppPath := ExtractFilePath(ParamStr(0));
  AppCompiler := 'Unidentified.';
  {$IFDEF VER80} AppCompiler := 'Delphi 1'; {$ENDIF}
  {$IFDEF VER90} AppCompiler := 'Delphi 2'; {$ENDIF}
  {$IFDEF VER100} AppCompiler := 'Delphi 3'; {$ENDIF}
  {$IFDEF VER120} AppCompiler := 'Delphi 4'; {$ENDIF}
  {$IFDEF VER130} AppCompiler := 'Delphi 5'; {$ENDIF}
  {$IFDEF VER140} AppCompiler := 'Delphi 6'; {$ENDIF}
  {$IFDEF VER150} AppCompiler := 'Delphi 7'; {$ENDIF}
  {$IFDEF VER160} AppCompiler := 'Delphi 8'; {$ENDIF}
  {$IFDEF VER170} AppCompiler := 'Delphi 2005'; {$ENDIF}
  {$IFDEF VER180} AppCompiler := 'Delphi 2006'; {$ENDIF}
  {$IFDEF VER185} AppCompiler := 'Delphi 2007'; {$ENDIF}
  {$IFDEF VER200} AppCompiler := 'Delphi 2009'; {$ENDIF}
  {$IFDEF VER210} AppCompiler := 'Delphi 2010'; {$ENDIF}
  {$IFDEF G2_REPORT_LEAKS}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  {$IFDEF G2_WRITE_LOG}
  LogStart;
  {$ENDIF}
  CoInitialize(nil);
  TG2Mesh.RenderMode := rmSM3;
  {$Warnings on}
end;

finalization
begin
  CoUnInitialize;
  {$IFDEF G2_WRITE_LOG}
  LogStop;
  {$ENDIF}
end;

end.
