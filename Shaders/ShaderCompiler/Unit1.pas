unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Direct3D9, D3DX9;

type
  TForm1 = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  g_Device: IDirect3DDevice9;

implementation

{$R *.dfm}

procedure SafeRelease(var i); inline;
begin
  if IUnknown(i) <> nil then IUnknown(i):= nil;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SafeRelease(g_Device);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  D3D9: IDirect3D9;
  pp: TD3DPresentParameters;
begin
  D3D9 := Direct3DCreate9(D3D_SDK_VERSION);
  ZeroMemory(@pp, SizeOf(TD3DPresentParameters));
  pp.BackBufferWidth := 0;
  pp.BackBufferHeight := 0;
  pp.BackBufferFormat := D3DFMT_X8R8G8B8;
  pp.BackBufferCount := 0;
  pp.MultiSampleType := D3DMULTISAMPLE_NONE;
  pp.MultiSampleQuality := 0;
  pp.SwapEffect := D3DSWAPEFFECT_DISCARD;
  pp.hDeviceWindow := Form1.Handle;
  pp.Windowed := True;
  pp.EnableAutoDepthStencil := False;
  pp.AutoDepthStencilFormat := D3DFMT_UNKNOWN;
  pp.Flags := 0;
  pp.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
  D3D9.CreateDevice(
    D3DADAPTER_DEFAULT,
    D3DDEVTYPE_HAL,
    Form1.Handle,
    D3DCREATE_SOFTWARE_VERTEXPROCESSING,
    @pp,
    g_Device
  );
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i, j: integer;
  SR: TSearchRec;
  fs: TFileStream;
  FileNames: array of string;
  CurShader: array of Byte;
  ShaderText: AnsiString;
  procedure FindExt(const Ext: String);
  begin
    if FindFirst('*.' + Ext, 0, SR) = 0 then
    repeat
      begin
        SetLength(FileNames, length(FileNames) + 1);
        FileNames[high(FileNames)] := SR.Name;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  procedure DeleteExt(const Ext: String);
  begin
    if FindFirst('..\*.' + Ext, 0, SR) = 0 then
    repeat
      begin
        DeleteFile('..\' + SR.Name);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  procedure SaveBuffer(
    const Buffer: ID3DXBuffer;
    const SaveFile: String;
    const ShaderName: AnsiString;
    const Comments: AnsiString = ''
    );
  var
    i: Integer;
    pb: PByteArray;
    Size: Integer;
    s: String;
    CommentList: TStringList;
  begin
    Size := Buffer.GetBufferSize;
    pb := Buffer.GetBufferPointer;
    s := ShaderName + ': array[0..' + IntToStr(Size - 1) + '] of Byte = (';
    for i := 0 to Size - 1 do
    begin
      if i mod 16 = 0 then
      s := s + #$D#$A'  ';
      s := s + '$' + IntToHex(pb^[i], 2);
      if i < Size - 1 then
      s := s + ',';
    end;
    s := s + #$D#$A');';
    CommentList := TStringList.Create;
    CommentList.Text := Comments;
    for i := 0 to CommentList.Count - 1 do
    if Length(CommentList[i]) > 0 then
    CommentList[i] := '//' + CommentList[i];
    CommentList.Text := CommentList.Text + #$D#$A + s;
    CommentList.SaveToFile('..\' + SaveFile);
    CommentList.Free;
  end;
  function CompileShader(const ShaderFile: String): Boolean;
  var
    FXCompiler: ID3DXEffectCompiler;
    Ext: AnsiString;
    CompileTarget: AnsiString;
    ShaderBuffer: ID3DXBuffer;
    DisAsmBuffer: ID3DXBuffer;
    DisAsm: AnsiString;
    ShaderName: AnsiString;
    FileName: AnsiString;
    Effect: ID3DXEffect;
    c: Integer;
  begin
    fs := TFileStream.Create(ShaderFile, fmOpenRead);
    SetLength(CurShader, fs.Size);
    fs.Read(CurShader[0], fs.Size);
    fs.Free;
    for c := 0 to High(CurShader) do
    if CurShader[c] = Ord(#$D) then
    CurShader[c] := Ord(#$A);
    Ext := ExtractFileExt(ShaderFile);
    ShaderName := ExtractFileName(ShaderFile);
    FileName := ShaderName;
    Delete(
      ShaderName,
      Length(ShaderName) - Length(ExtractFileExt(ShaderName)) + 1,
      Length(ExtractFileExt(ShaderName))
    );
    if ExtractFileExt(FileNames[i]) = '.vsh' then FileName := ShaderName + '.cvs';
    if ExtractFileExt(FileNames[i]) = '.psh' then FileName := ShaderName + '.cps';
    if ExtractFileExt(FileNames[i]) = '.vsa' then FileName := ShaderName + '.cvs';
    if ExtractFileExt(FileNames[i]) = '.psa' then FileName := ShaderName + '.cps';
    if ExtractFileExt(FileNames[i]) = '.fx' then FileName := ShaderName + '.cfx';
    if (LowerCase(Ext) = '.vsa')
    or (LowerCase(Ext) = '.psa') then
    begin
      if Succeeded(
        D3DXAssembleShader(
          PAnsiChar(@CurShader[0]),
          Length(CurShader),
          nil,
          nil,
          0,
          @ShaderBuffer,
          nil
        )
      ) then
      begin
        if Succeeded(
          D3DXDisassembleShader(
            PDWord(ShaderBuffer.GetBufferPointer),
            False, nil, DisAsmBuffer
          )
        ) then
        begin
          SetLength(DisAsm, DisAsmBuffer.GetBufferSize);
          Move(DisAsmBuffer.GetBufferPointer^, DisAsm[1], DisAsmBuffer.GetBufferSize);
          SafeRelease(DisAsmBuffer);
          DisAsm := '//' + ShaderName + #$D#$A + DisAsm;
        end
        else
        DisAsm := '//' + ShaderName + #$D#$A;
        SaveBuffer(ShaderBuffer, FileName, ShaderName, DisAsm);
        SafeRelease(ShaderBuffer);
      end;
    end;
    if (LowerCase(Ext) = '.vsh')
    or (LowerCase(Ext) = '.psh') then
    begin
      if LowerCase(Ext) = '.vsh' then
      CompileTarget := 'vs_3_0'
      else
      CompileTarget := 'ps_3_0';
      if Succeeded(
        D3DXCompileShader(
          PAnsiChar(@CurShader[0]),
          Length(CurShader),
          nil,
          nil,
          'main',
          PAnsiChar(CompileTarget),
          D3DXSHADER_OPTIMIZATION_LEVEL3 or
          D3DXSHADER_PARTIALPRECISION,
          @ShaderBuffer,
          nil,
          nil
        )
      ) then
      begin
        if Succeeded(
          D3DXDisassembleShader(
            PDWord(ShaderBuffer.GetBufferPointer),
            False, nil, DisAsmBuffer
          )
        ) then
        begin
          SetLength(DisAsm, DisAsmBuffer.GetBufferSize);
          Move(DisAsmBuffer.GetBufferPointer^, DisAsm[1], DisAsmBuffer.GetBufferSize);
          SafeRelease(DisAsmBuffer);
          DisAsm := '//' + ShaderName + #$D#$A + DisAsm;
        end
        else
        DisAsm := '//' + ShaderName + #$D#$A;
        SaveBuffer(ShaderBuffer, FileName, ShaderName, DisAsm);
        SafeRelease(ShaderBuffer);
      end;
    end;
    if (LowerCase(Ext) = '.fx') then
    begin
      if Succeeded(
        D3DXCreateEffectCompiler(
          @CurShader[0],
          Length(CurShader),
          nil,
          nil,
          D3DXSHADER_AVOID_FLOW_CONTROL or
          D3DXSHADER_OPTIMIZATION_LEVEL3,
          FXCompiler,
          nil
        )
      ) then
      begin
        if Succeeded(
          FXCompiler.CompileEffect(
            D3DXSHADER_AVOID_FLOW_CONTROL or
            D3DXSHADER_OPTIMIZATION_LEVEL3,
            @ShaderBuffer,
            nil
          )
        ) then
        begin
          if Succeeded(
            D3DXCreateEffect(
              g_Device,
              ShaderBuffer.GetBufferPointer,
              ShaderBuffer.GetBufferSize,
              nil,
              nil,
              0,
              nil,
              Effect,
              nil
            )
          ) then
          begin
            if Succeeded(
              D3DXDisassembleEffect(
                Effect, False, DisAsmBuffer
              )
            ) then
            begin
              SetLength(DisAsm, DisAsmBuffer.GetBufferSize);
              Move(DisAsmBuffer.GetBufferPointer^, DisAsm[1], DisAsmBuffer.GetBufferSize);
              SafeRelease(DisAsmBuffer);
              DisAsm := '//' + ShaderName + #$D#$A + DisAsm;
            end
            else
            DisAsm := '//' + ShaderName + #$D#$A;
            SafeRelease(Effect);
          end
          else
          DisAsm := '//' + ShaderName + #$D#$A;
          SaveBuffer(ShaderBuffer, FileName, ShaderName, DisAsm);
          SafeRelease(ShaderBuffer);
        end;
        SafeRelease(FXCompiler);
      end;
    end;
  end;
begin
  Timer1.Enabled := false;
  DeleteExt('cvs');
  DeleteExt('cps');
  DeleteExt('cfx');
  FindExt('vsh');
  FindExt('psh');
  FindExt('vsa');
  FindExt('psa');
  FindExt('fx');
  if length(FileNames) > 0 then
  begin
    for i := 0 to high(FileNames) do
    begin
      Label2.Caption := 'Current Shader: ' + FileNames[i];
      ProgressBar1.Position := trunc((i / length(FileNames)) * 100);
      Application.ProcessMessages;
      CompileShader(FileNames[i]);
    end;
  end;
  Close;
end;

end.
