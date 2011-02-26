unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  SynEditHighlighter,
  SynHighlighterGeneralEx,
  SynHighlighterHLSL,
  SynEdit,
  SynMemo,
  ExtCtrls,
  StdCtrls,
  ImgList,
  ComCtrls,
  ToolWin,
  Menus,
  DXTypes,
  Direct3D9,
  D3DX9,
  Tabs,
  DockTabSet,
  ButtonGroup,
  SynEditMiscClasses,
  SynEditSearch,
  SynEditRegexSearch;

type
  TStrArr = array of AnsiString;

  TProjTab = record
  public
    var TabID: Integer;
    var TabStrID: Integer;
    var CaretPos: TPoint;
    var Saved: Boolean;
    var TabName: String;
    var FileName: String;
    var Code: String;
    procedure MakeNew;
    procedure Compile;
    procedure Kill;
    procedure Save(const ForceSaveAs: Boolean = False);
    procedure Open(const f: String);
  end;

  TProject = record
  public
    var CurTab: Integer;
    var Tabs: array of TProjTab;
    function AddNewTab(const NewName: String): Integer;
    procedure SetTab(const TabStrID: Integer);
    procedure KillTab(const TabStrID: Integer);
    procedure OpenNewTab;
  end;

  TForm1 = class(TForm)
    GridPanel1: TGridPanel;
    SynHLSLSyn1: TSynHLSLSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ImageList1: TImageList;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Save1: TMenuItem;
    Load1: TMenuItem;
    Exit1: TMenuItem;
    ToolButton2: TToolButton;
    ToolButton5: TToolButton;
    Project1: TMenuItem;
    Compile1: TMenuItem;
    SaveAs1: TMenuItem;
    Output: TListBox;
    GridPanel3: TGridPanel;
    sm: TSynMemo;
    TabSet1: TTabSet;
    Panel1: TPanel;
    ButtonGroup1: TButtonGroup;
    sd1: TSaveDialog;
    od1: TOpenDialog;
    Close1: TMenuItem;
    Options1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Compile1Click(Sender: TObject);
    procedure smChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ButtonGroup1Items0Click(Sender: TObject);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure ButtonGroup1Items1Click(Sender: TObject);
    procedure TabSet1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolButton3Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure smKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Close1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Options1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Project: TProject;
  g_Device: IDirect3DDevice9;

const DefTabName = 'Unnamed';

implementation

uses Unit2, Unit3;

{$R *.dfm}

function TabNameFromFile(const FileName: String): String;
  var Ext: String;
begin
  Ext := ExtractFileExt(FileName);
  Result := ExtractFileName(FileName);
  Delete(Result, Length(Result) - Length(Ext) + 1, Length(Ext));
end;

function StrExplode(const Str: AnsiString; const Separator: AnsiString): TStrArr;
var
  i: Integer;
  j: Integer;
  CurElement: Integer;
  PrevParamIndex: Integer;
  b: Boolean;
begin
  SetLength(Result, Length(Str));
  CurElement := 0;
  PrevParamIndex := 1;
  for i := 1 to Length(Str) do
  begin
    b := True;
    for j := 0 to Length(Separator) - 1 do
    begin
      if Separator[j + 1] <> Str[i + j] then
      begin
        b := False;
        Break;
      end;
    end;
    if b then
    begin
      SetLength(Result[CurElement], i - PrevParamIndex);
      Move(Str[PrevParamIndex], Result[CurElement][1], i - PrevParamIndex);
      PrevParamIndex := i + Length(Separator);
      Inc(CurElement);
    end;
  end;
  if Length(Str) > PrevParamIndex then
  begin
    SetLength(Result[CurElement], Length(Str) - PrevParamIndex);
    Move(Str[PrevParamIndex], Result[CurElement][1], Length(Str) - PrevParamIndex);
    Inc(CurElement);
  end;
  SetLength(Result, CurElement);
end;

//TProjTab BEGIN
procedure TProjTab.MakeNew;
begin
  Saved := True;
  Code := '';
  Code := Code + '/*--------------------------------*\'#$D#$A;
	Code := Code + '      Gen2 Shader'#$D#$A;
  Code := Code + '\*--------------------------------*/'#$D#$A;
  Code := Code + #$D#$A;
  Code := Code + 'technique T0 {'#$D#$A;
  Code := Code + '  pass P0 {'#$D#$A;
  Code := Code + '    VertexShader = NULL;'#$D#$A;
  Code := Code + '    PixelShader = NULL;'#$D#$A;
  Code := Code + '  }'#$D#$A;
  Code := Code + '}';
  CaretPos := Point(0, 0);
  Project.SetTab(TabStrID);
end;

procedure TProjTab.Compile;
  var CodeAnsi: AnsiString;
  var CodeUni, CodeUni2: String;
  var Compiler: ID3DXEffectCompiler;
  var EffectBuffer: ID3DXBuffer;
  var Effect: ID3DXEffect;
  var Errors: ID3DXBuffer;
  var DisAsm: ID3DXBuffer;
  procedure PrintErrors;
    var ErrorsAnsi: AnsiString;
    var ErrorLines: TStrArr;
    var i: Integer;
  begin
    Form1.Output.Items.Clear;
    if Assigned(Errors) then
    begin
      SetLength(ErrorsAnsi, Errors.GetBufferSize);
      Move(Errors.GetBufferPointer^, ErrorsAnsi[1], Errors.GetBufferSize);
      ErrorsAnsi := AnsiString(StringReplace(String(ErrorsAnsi), #$D#$A, #$D, [rfReplaceAll]));
      ErrorsAnsi := AnsiString(StringReplace(String(ErrorsAnsi), #$A, #$D, [rfReplaceAll]));
      ErrorLines := StrExplode(ErrorsAnsi, #$D);
      for i := 0 to High(ErrorLines) do
      begin
        ErrorLines[i] := AnsiString(StringReplace(String(ErrorLines[i]), 'ID3DXEffectCompiler: ', '', [rfReplaceAll]));
        Form1.Output.Items.Append(String(ErrorLines[i]));
      end;
    end
    else
    Form1.Output.Items.Text := 'Undefined error.';
  end;
  var i: Integer;
  var Options: DWord;
begin
  Form1.Output.Items.Text := 'Compiling: ' + TabName;
  Application.ProcessMessages;
  CodeAnsi := AnsiString(Code);
  Options := 0;
  case Form3.cbb1.ItemIndex of
    1: Options := Options or D3DXSHADER_OPTIMIZATION_LEVEL0;
    2: Options := Options or D3DXSHADER_OPTIMIZATION_LEVEL1;
    3: Options := Options or D3DXSHADER_OPTIMIZATION_LEVEL2;
    4: Options := Options or D3DXSHADER_OPTIMIZATION_LEVEL3;
  end;
  case Form3.cbb2.ItemIndex of
    1: Options := Options or D3DXSHADER_PREFER_FLOW_CONTROL;
    2: Options := Options or D3DXSHADER_AVOID_FLOW_CONTROL;
  end;
  if Form3.chk1.Checked then Options := Options or D3DXSHADER_DEBUG;
  if Form3.chk2.Checked then Options := Options or D3DXSHADER_SKIPOPTIMIZATION;
  if Form3.chk3.Checked then Options := Options or D3DXSHADER_PARTIALPRECISION;
  if Form3.chk4.Checked then Options := Options or D3DXSHADER_NO_PRESHADER;
  if Succeeded(
    D3DXCreateEffectCompiler(
      @CodeAnsi[1], Length(CodeAnsi),
      nil, nil,
      Options,
      Compiler,
      @Errors
    )
  ) then
  begin
    if Succeeded(
      Compiler.CompileEffect(
        Options,
        @EffectBuffer,
        @Errors
      )
    ) then
    begin
      if Succeeded(
        D3DXCreateEffect(
          g_Device,
          EffectBuffer.GetBufferPointer,
          EffectBuffer.GetBufferSize,
          nil,
          nil,
          0,
          nil,
          Effect,
          @Errors
        )
      ) then
      begin
        if Succeeded(
          D3DXDisassembleEffect(
            Effect, False, DisAsm
          )
        ) then
        begin
          SetLength(CodeAnsi, DisAsm.GetBufferSize);
          Move(DisAsm.GetBufferPointer^, CodeAnsi[1], DisAsm.GetBufferSize);
          CodeUni := String(CodeAnsi);
          CodeUni := StringReplace(CodeUni, #$D#$A, #$D, [rfReplaceAll]);
          CodeUni := StringReplace(CodeUni, #$A, #$D, [rfReplaceAll]);
          CodeUni := StringReplace(CodeUni, #$D, #$D#$A, [rfReplaceAll]);
          Form2.MemoAssembly.Text := CodeUni;
        end;
        SetLength(CodeAnsi, EffectBuffer.GetBufferSize);
        Move(EffectBuffer.GetBufferPointer^, CodeAnsi[1], EffectBuffer.GetBufferSize);
        CodeUni := '';
        CodeUni2 := '';
        for i := 1 to Length(CodeAnsi) do
        begin
          if (i > 1)
          and ((i - 1) mod 32 = 0)
          and (i < Length(CodeAnsi)) then
          CodeUni := CodeUni + #$D#$A;
          if ((i - 1) mod 16 = 0) then
          CodeUni2 := CodeUni2 + #$D#$A'    ';
          CodeUni2 := CodeUni2 + '$' + IntToHex(Ord(CodeAnsi[i]), 2);
          if (i < Length(CodeAnsi)) then
          CodeUni2 := CodeUni2 + ', ';
          CodeUni := CodeUni + IntToHex(Ord(CodeAnsi[i]), 2) + ' ';
        end;
        Form2.MemoInline.Lines.BeginUpdate;
        CodeUni2 := '  ' + TabName + ': array[0..' + IntToStr(Length(CodeAnsi) - 1) + '] of Byte = (' + CodeUni2 + #$D#$A'  );';
        Form2.MemoInline.Lines.Text := CodeUni2;
        Form2.MemoInline.Lines.EndUpdate;
        Form2.MemoBinary.Lines.BeginUpdate;
        Form2.MemoBinary.Lines.Text := CodeUni;
        Form2.MemoBinary.Lines.EndUpdate;
        SetLength(Unit2.CompileBinary, EffectBuffer.GetBufferSize);
        Move(EffectBuffer.GetBufferPointer^, Unit2.CompileBinary[0], EffectBuffer.GetBufferSize);
        Form2.PageControl1.TabIndex := 0;
        Form1.Output.Items.Text := 'Compilation successful.';
        Form2.ShowModal;
      end
      else
      begin
        PrintErrors;
      end;
    end
    else
    begin
      PrintErrors;
    end;
  end
  else
  begin
    PrintErrors;
  end;
  if Errors <> nil then Errors := nil;
  if Effect <> nil then Effect := nil;
  if EffectBuffer <> nil then EffectBuffer := nil;
  if Compiler <> nil then Compiler := nil;
end;

procedure TProjTab.Kill;
  var i: Integer;
begin
  if not Saved then
  begin
    i := MessageDlg(
      TabName + ' has not been saved.'#$D#$A'Any changes made to this tab will be lost!'#$D#$A'Would you like to save it now?',
      mtInformation, [mbYes, mbNo], 0, mbNo
    );
    if i = mrYes then Save();
  end;
  Form1.TabSet1.Tabs.Delete(TabStrID);
  for i := TabID to High(Project.Tabs) - 1 do
  begin
    Project.Tabs[i].TabID := i;
    Project.Tabs[i].TabStrID := Project.Tabs[i + 1].TabStrID - 1;
    Project.Tabs[i].TabName := Project.Tabs[i + 1].TabName;
    Project.Tabs[i].CaretPos := Project.Tabs[i + 1].CaretPos;
    Project.Tabs[i].Code := Project.Tabs[i + 1].Code;
  end;
  SetLength(Project.Tabs, Length(Project.Tabs) - 1);
  if Project.CurTab > 0 then
  Project.SetTab(Project.Tabs[Project.CurTab - 1].TabStrID)
  else if Length(Project.Tabs) > 0 then
  Project.SetTab(Project.Tabs[Project.CurTab].TabStrID);
end;

procedure TProjTab.Save(const ForceSaveAs: Boolean = False);
  var fs: TFileStream;
begin
  if ForceSaveAs
  or not FileExists(FileName) then
  begin
    if Form1.sd1.Execute() then
    FileName := Form1.sd1.FileName;
  end;
  if Length(FileName) > 0 then
  begin
    fs := TFileStream.Create(FileName, fmCreate);
    try
      TabName := TabNameFromFile(FileName);
      Form1.TabSet1.Tabs[TabStrID] := TabName;
      fs.Write(AnsiString(Code)[1], Length(Code));
      Saved := True;
    finally
      fs.Free;
    end;
  end;
end;

procedure TProjTab.Open(const f: String);
  var CodeAnsi: AnsiString;
  var fs: TFileStream;
begin
  if FileExists(f) then
  begin
    FileName := f;
    TabName := TabNameFromFile(f);
    CaretPos := Point(0, 0);
    fs := TFileStream.Create(FileName, fmOpenRead);
    SetLength(CodeAnsi, fs.Size);
    fs.Read(CodeAnsi[1], fs.Size);
    fs.Free;
    Code := String(CodeAnsi);
    Form1.TabSet1.Tabs[TabStrID] := TabName;
  end;
end;
//TProjTab END

//TProject BEGIN
function TProject.AddNewTab(const NewName: String): Integer;
  var i, j: Integer;
  var TabName: String;
  var NameUnique: Boolean;
begin
  SetLength(Tabs, Length(Tabs) + 1);
  Tabs[High(Tabs)].TabID := High(Tabs);
  Tabs[High(Tabs)].TabStrID := Form1.TabSet1.Tabs.Count;
  TabName := NewName;
  NameUnique := False;
  j := 0;
  while not NameUnique do
  begin
    NameUnique := True;
    for i := 0 to High(Tabs) do
    if Tabs[i].TabName = TabName then
    begin
      TabName := NewName + IntToStr(j);
      Inc(j);
      NameUnique := False;
      Break;
    end;
  end;
  Tabs[High(Tabs)].TabName := TabName;
  Tabs[High(Tabs)].CaretPos := Point(0, 0);
  Form1.TabSet1.Tabs.Add(TabName);
  Tabs[High(Tabs)].MakeNew;
  SetTab(Tabs[High(Tabs)].TabStrID);
  Result := High(Tabs);
end;

procedure TProject.SetTab(const TabStrID: Integer);
  var i: Integer;
  var OnChange: TTabChangeEvent;
begin
  if CurTab > -1 then
  begin
    Tabs[CurTab].CaretPos := Point(Form1.sm.CaretX, Form1.sm.CaretY);
  end;
  OnChange := Form1.TabSet1.OnChange;
  Form1.TabSet1.OnChange := nil;
  if Form1.TabSet1.TabIndex <> TabStrID then
  Form1.TabSet1.TabIndex := TabStrID;
  for i := 0 to High(Tabs) do
  if Tabs[i].TabStrID = TabStrID then
  begin
    CurTab := Tabs[i].TabID;
    Form1.sm.Text := Tabs[i].Code;
    Form1.sm.CaretX := Tabs[i].CaretPos.X;
    Form1.sm.CaretY := Tabs[i].CaretPos.Y;
    Break;
  end;
  Form1.TabSet1.OnChange := OnChange;
end;

procedure TProject.KillTab(const TabStrID: Integer);
  var i: Integer;
begin
  for i := 0 to High(Tabs) do
  if Tabs[i].TabStrID = TabStrID then
  begin
    Tabs[i].Kill;
    Break;
  end;
  if Length(Tabs) = 0 then
  AddNewTab(DefTabName);
end;

procedure TProject.OpenNewTab;
  var TabID: Integer;
begin
  if Form1.od1.Execute() then
  begin
    TabID := AddNewTab(DefTabName);
    Tabs[TabID].Open(Form1.od1.FileName);
    SetTab(Tabs[TabID].TabStrID);
  end;
end;
//TProject END

procedure TForm1.ButtonGroup1Items0Click(Sender: TObject);
begin
  Project.AddNewTab(DefTabName);
end;

procedure TForm1.ButtonGroup1Items1Click(Sender: TObject);
begin
  if Project.CurTab > -1 then
  begin
    Project.Tabs[Project.CurTab].Kill;
    if Length(Project.Tabs) = 0 then
    Project.AddNewTab(DefTabName);
  end;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  if Project.CurTab > -1 then
  begin
    Project.Tabs[Project.CurTab].Kill;
    if Length(Project.Tabs) = 0 then
    Project.AddNewTab(DefTabName);
  end;
end;

procedure TForm1.Compile1Click(Sender: TObject);
begin
  if Project.CurTab > -1 then
  Project.Tabs[Project.CurTab].Compile;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if g_Device <> nil then
  g_Device := nil;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  var i: Integer;
begin
  for i := High(Project.Tabs) downto 0 do
  Project.KillTab(Project.Tabs[i].TabStrID);
  CanClose := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
  var D3D9: IDirect3D9;
  var pp: TD3DPresentParameters;
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
  if FileExists(ParamStr(1)) then
  begin
    Project.AddNewTab(DefTabName);
    Project.Tabs[0].Open(ParamStr(1));
    Project.SetTab(0);
  end
  else
  begin
    Project.AddNewTab(DefTabName);
    Project.SetTab(0);
  end;
end;

procedure TForm1.Load1Click(Sender: TObject);
begin
  Project.OpenNewTab;
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  Project.AddNewTab(DefTabName);
end;

procedure TForm1.Options1Click(Sender: TObject);
begin
  Form3.ShowModal;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if Project.CurTab > -1 then
  Project.Tabs[Project.CurTab].Save;
end;

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if Project.CurTab > -1 then
  Project.Tabs[Project.CurTab].Save(True);
end;

procedure TForm1.smChange(Sender: TObject);
begin
  if Project.CurTab > -1 then
  begin
    Project.Tabs[Project.CurTab].Code := sm.Text;
    Project.Tabs[Project.CurTab].Saved := False;
  end;
end;

procedure TForm1.smKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F9 then
  if Project.CurTab > -1 then
  Project.Tabs[Project.CurTab].Compile;
  if ssCtrl in Shift then
  begin
    if Key = Ord('N') then
    begin
      Project.AddNewTab(DefTabName);
    end;
    if Key = Ord('D') then
    begin
      Project.OpenNewTab;
    end;
    if Key = Ord('S') then
    begin
      if Project.CurTab > -1 then
      Project.Tabs[Project.CurTab].Save;
    end;
  end;
end;

procedure TForm1.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  Project.SetTab(NewTab);
end;

procedure TForm1.TabSet1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var i: Integer;
  var mc: TPoint;
begin
  if Button = mbMiddle then
  begin
    mc := TabSet1.ScreenToClient(Mouse.CursorPos);
    for i := 0 to TabSet1.Tabs.Count - 1 do
    if PtInRect(
      TabSet1.ItemRect(i),
      mc
    ) then
    begin
      Project.KillTab(i);
      Exit;
    end;
  end;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  Project.AddNewTab(DefTabName);
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  if Project.CurTab > -1 then
  Project.Tabs[Project.CurTab].Save;
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
  Project.OpenNewTab;
end;

procedure TForm1.ToolButton5Click(Sender: TObject);
begin
  if Project.CurTab > -1 then
  Project.Tabs[Project.CurTab].Compile;
end;

end.
