unit G2AVI;

interface

uses
  Windows,
  Types,
  ActiveX;

type
  PAVIStreamInfoW = ^TAVIStreamInfoW;
  TAVIStreamInfoW = record
  public
    var fccType: DWord;
    var fccHandler: DWord;
    var dwFlags: DWord;
    var dwCaps: DWord;
    var wPriority: Word;
    var wLanguage: Word;
    var dwScale: DWord;
    var dwRate: DWord;
    var dwStart: DWord;
    var dwLength: DWord;
    var dwInitialFrames: DWord;
    var dwSuggestedBufferSize: DWord;
    var dwQuality: DWord;
    var dwSampleSize: DWord;
    var rcFrame: TRECT;
    var dwEditCount: DWord;
    var dwFormatChangeCount: DWord;
    var szName: array[0..63] of WideChar;
  end;

  PAVIStreamInfoA = ^TAVIStreamInfoA;
  TAVIStreamInfoA = record
  public
    var fccType: DWord;
    var fccHandler: DWord;
    var dwFlags: DWord;
    var dwCaps: DWord;
    var wPriority: Word;
    var wLanguage: Word;
    var dwScale: DWord;
    var dwRate: DWord;
    var dwStart: DWord;
    var dwLength: DWord;
    var dwInitialFrames: DWord;
    var dwSuggestedBufferSize: DWord;
    var dwQuality: DWord;
    var dwSampleSize: DWord;
    var rcFrame: TRECT;
    var dwEditCount: DWord;
    var dwFormatChangeCount: DWord;
    var szName: array[0..63] of AnsiChar;
  end;

  PAVIFileInfoW = ^TAVIFileInfoW;
  TAVIFileInfoW = record
  public
    var dwMaxBytesPerSec: DWord;
    var dwFlags: DWord;
    var dwCaps: DWord;
    var dwStreams: DWord;
    var dwSuggestedBufferSize: DWord;
    var dwWidth: DWord;
    var dwHeight: DWord;
    var dwScale: DWord;
    var dwRate: DWord;
    var dwLength: DWord;
    var dwEditCount: DWord;
    var szFileType: array[0..63] of WideChar;
  end;

  PAVICompressOptions = ^TAVICompressOptions;
  TAVICompressOptions = record
  public
    var fccType: DWord;
    var fccHandler: DWord;
    var dwKeyFrameEvery: DWord;
    var dwQuality: DWord;
    var dwBytesPerSecond: DWord;
    var dwFlags: DWord;
    var lpFormat: Pointer;
    var cbFormat: DWord;
    var lpParms: Pointer;
    var cbParms: DWord;
    var dwInterleaveEvery: DWord;
  end;

  PAVIStream = ^IAVIStream;
  IAVIStream = interface(IUnknown)
    function Create(lParam1, lParam2: LPARAM): HResult; stdcall;
    function Info(psi: PAVIStreamInfoW; lSize: DWord): HResult; stdcall;
    function FindSample(lPos: DWord; lFlags: DWord): DWord; stdcall;
    function ReadFormat(lPos: DWord; lpFormat: Pointer; lpcbFormat: PDWord): HResult; stdcall;
    function SetFormat(lPos: DWord; lpFormat: Pointer; cbFormat: DWord): HResult; stdcall;
    function Read(lStart: DWord; lSamples: DWord; lpBuffer: Pointer; cbBuffer: DWord; plBytes, plSamples: PDWord): HResult; stdcall;
    function Write(lStart: DWord; lSamples: DWord; lpBuffer: Pointer; cbBuffer: DWord; dwFlags: DWord; plSampWritten, plBytesWritten: PDWord): HResult; stdcall;
    function Delete(lStart: DWord; lSamples: DWord): HResult; stdcall;
    function ReadData(fcc: DWord; lp: Pointer; lpcb: PDWord): HResult; stdcall;
    function WriteData(fcc: DWord; lp: Pointer; cb: DWord): HResult; stdcall;
    function SetInfo(lpInfo: PAVIStreamInfoW; cbInfo: DWord): HResult; stdcall;
  end;

  PAVIFile = ^IAVIFile;
  IAVIFile = interface(IUnknown)
    function Info(pfi: PAVIFileInfoW; lSize: DWord): HResult; stdcall;
    function GetStream(var ppStream: PAVIStream; fccType: DWord; lParam: DWord): HResult; stdcall;
    function CreateStream(var ppStream: PAVIStream; psi: PAVIStreamInfoW): HResult; stdcall;
    function WriteData(ckid: DWord; lpData: Pointer; cbData: DWord): HResult; stdcall;
    function ReadData(ckid: DWord; lpData: Pointer; lpcbData: PDWord): HResult; stdcall;
    function EndRecord: HResult; stdcall;
    function DeleteStream(fccType: DWord; lParam: DWord): HResult; stdcall;
  end;

  var AVIFILAvailable: Boolean = False;
  var AVIFILModule: DWord;

  const AVIFIL_DLL = 'AVIFIL32.DLL';

  var AVIFileOpen: function (var ppfile: PAVIFile; szFile: LPCSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;
  var AVIFileOpenA: function (var ppfile: PAVIFile; szFile: LPCSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;
  var AVIFileOpenW: function (var ppfile: PAVIFile; szFile: LPCWSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;

  var AVIFileCreateStream: function (pfile: PAVIFile; var ppavi: PAVIStream; psi: PAVIStreamInfoA): HResult; stdcall;
  var AVIFileCreateStreamA: function (pfile: PAVIFile; var ppavi: PAVIStream; psi: PAVIStreamInfoA): HResult; stdcall;
  var AVIFileCreateStreamW: function (pfile: PAVIFile; var ppavi: PAVIStream; psi: PAVIStreamInfoW): HResult; stdcall;

  var AVIFileRelease: function (pfile: PAVIFile): DWord; stdcall;

  var AVIMakeCompressedStream: function (var ppsCompressed: PAVIStream; ppsSource: PAVIStream; lpOptions: PAVICompressOptions; pclsidHandler: PCLSID): HResult; stdcall;

  var AVIStreamSetFormat: function (pavi: PAVIStream; lPos: DWord; lpFormat: Pointer; cbFormat: DWord): HResult; stdcall;
  var AVIStreamWrite: function (pavi: PAVIStream; lStart, lSamples: DWord; lpBuffer: Pointer; cbBuffer: DWord; dwFlags: DWord; plSampWritten: PDWord; plBytesWritten: PDWord): HResult; stdcall;
  var AVIStreamRelease: function (pavi: PAVIStream): DWord; stdcall;

implementation

initialization
  AVIFILModule := LoadLibraryW(AVIFIL_DLL);
  AVIFILAvailable := AVIFILModule > 0;
  if AVIFILAvailable then
  begin
    AVIFileOpenA := GetProcAddress(AVIFILModule, 'AVIFileOpenA');
    AVIFileOpenW := GetProcAddress(AVIFILModule, 'AVIFileOpenW');
    AVIFileOpen := GetProcAddress(AVIFILModule, 'AVIFileOpenA');
    AVIFileCreateStreamA := GetProcAddress(AVIFILModule, 'AVIFileCreateStreamA');
    AVIFileCreateStreamW := GetProcAddress(AVIFILModule, 'AVIFileCreateStreamW');
    AVIFileCreateStream := GetProcAddress(AVIFILModule, 'AVIFileCreateStreamA');
    AVIMakeCompressedStream := GetProcAddress(AVIFILModule, 'AVIMakeCompressedStream');
    AVIStreamSetFormat := GetProcAddress(AVIFILModule, 'AVIStreamSetFormat');
    AVIStreamWrite := GetProcAddress(AVIFILModule, 'AVIStreamWrite');
    AVIStreamRelease := GetProcAddress(AVIFILModule, 'AVIStreamRelease');
    AVIFileRelease := GetProcAddress(AVIFILModule, 'AVIFileRelease');
  end;

finalization
  if AVIFILAvailable then FreeLibrary(AVIFILModule);

end.