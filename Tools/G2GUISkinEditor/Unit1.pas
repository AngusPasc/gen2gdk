//GUI Format:
//  Header: AnsiChar[4] = 'GUIP'
//  ElementCount: Int4
//  TextureCount: Int4
//  TemplateCount: Int4
//  Elements[ElementCount]:
//    Name: WideString
//    MappingType: Byte
//    TexCoords: TRect
//    Texture: Int4
//  Textures[TextureCount]:
//    Name: WideString
//    Path: WideString
//  Templates[TemplateCount]:
//    Name: WideString
//    Layers[4]: Int4
//    BorderSize: UInt1

//G2GUI Format:
//  Header: AnsiChar[4] = 'G2UI'
//  TextureCount: Int4
//  Textures[TextureCount]:
//    Name: WideString
//    DataSize: Int4
//    TextureData: Buffer[DataSize]
//  ElementCount: Int4
//  Elements[ElementCount]:
//    Name: WideString
//    MappingType: Byte (0 - Stretch; 1 - Tile, 2 - Border)
//    TexCoords:
//      Left: Float4
//      Top: Float4
//      Right: Float4
//      Bottom: Float4
//    Texture: Int4
//  TemplateCount: Int4
//  Templates[TemplateCount]:
//    Name: WideString;
//    Layers[4]: Int4
//    BorderSize: UInt1

unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  PNGImage,
  Controls,
  Types,
  Math,
  Forms,
  Dialogs,
  DirectInput,
  Direct3D9,
  D3DX9,
  DXTypes,
  Gen2,
  G2Math,
  G2Group,
  G2Landscape,
  G2PerlinNoise, sSkinManager, ExtCtrls, sPanel, StdCtrls,
  sButton, sComboBox, sListBox, sEdit, sSpinEdit, sCheckBox, sRadioButton,
  sGroupBox, sDialogs, ComCtrls, sTabControl, sPageControl, sGauge, sTrackBar,
  sLabel;

type
  TForm1 = class(TForm)
    sSkinManager1: TsSkinManager;
    GridPanel1: TGridPanel;
    GridPanel2: TGridPanel;
    pnl1: TPanel;
    sPanel2: TsPanel;
    sButton2: TsButton;
    btn2: TsButton;
    btn3: TsButton;
    btn4: TsButton;
    sd1: TsSaveDialog;
    od1: TsOpenDialog;
    sd2: TsSaveDialog;
    spnl1: TsPanel;
    sPageControl1: TsPageControl;
    sTabSheet1: TsTabSheet;
    spnl5: TsPanel;
    spnl2: TsPanel;
    sListBox1: TsListBox;
    btn1: TsButton;
    btn5: TsButton;
    spnl3: TsPanel;
    spnl4: TsPanel;
    sEdit1: TsEdit;
    cbb1: TsComboBox;
    sRadioGroup1: TsRadioGroup;
    rb1: TsRadioButton;
    rb2: TsRadioButton;
    rb3: TsRadioButton;
    sTabSheet2: TsTabSheet;
    spnl6: TsPanel;
    spnl7: TsPanel;
    sListBox2: TsListBox;
    btn6: TsButton;
    btn7: TsButton;
    spnl8: TsPanel;
    spnl9: TsPanel;
    spnl10: TsPanel;
    cbb2: TsComboBox;
    cbb3: TsComboBox;
    sEdit2: TsEdit;
    edt1: TsSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure sListBox1Click(Sender: TObject);
    procedure sEdit1Change(Sender: TObject);
    procedure sButton2Click(Sender: TObject);
    procedure cbb1Change(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure rb1Click(Sender: TObject);
    procedure rb2Click(Sender: TObject);
    procedure rb3Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure sListBox2Click(Sender: TObject);
    procedure cbb2Change(Sender: TObject);
    procedure sEdit2Change(Sender: TObject);
    procedure cbb3Change(Sender: TObject);
    procedure edt1Change(Sender: TObject);
    procedure sPageControl1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TMapType = (mtStretch, mtTile, mtBorder);

  TMapElement = record
  public
    var Name: WideString;
    var MapType: TMapType;
    var TexCoords: TRect;
    var Texture: Integer;
  end;

  TTexture = record
  public
    var Tex: TG2Texture2D;
    var Name: WideString;
    var Path: WideString;
  end;

  TTemplate = record
  public
    var Name: WideString;
    var Layers: array[0..3] of Integer;
    var BorderSize: Byte;
  end;

  TEditPoint = (epNone, epLT, epRT, epLB, epRB);

  TVertex = record
  public
    var Pos: TG2Vec4;
    var Tex: array[0..3] of TG2Vec2;
  end;
  TVertexArr = array[Word] of TVertex;
  PVertexArr = ^TVertexArr;

  TMyApp = class (TG2App)
  public
    var AppClose: Boolean;
    var MgrTextures: TG2TextureMgr;
    var Font: TG2Font;
    var TexGrid: TG2Texture2D;
    var TexPos: TG2Vec2;
    var TexScale: Single;
    var EditPoint: TEditPoint;
    var Elements: array of TMapElement;
    var Textures: array of TTexture;
    var Templates: array of TTemplate;
    var VB: TG2VB;
    var IB: TG2IB;
    procedure OnRender; override;
    procedure OnUpdate; override;
    procedure OnKeyDown(const Key: Byte); override;
    procedure OnKeyUp(const Key: Byte); override;
    procedure OnMouseDown(const Button: Byte); override;
    procedure OnMouseUp(const Button: Byte); override;
    procedure OnMouseMove(const Shift: TPoint); override;
    procedure OnWheelMove(const Shift: Integer); override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure RenderElements;
    procedure RenderTemplates;
    procedure UpdateElementList;
    procedure AddElement;
    procedure RemoveElement;
    procedure SelectElement;
    function FindElementByName(const Name: WideString): Integer;
    procedure AddTextures;
    procedure RemoveTextures;
    procedure UpdateTextureList;
    function FindTextureByPath(const Path: WideString): Integer;
    function FindTextureByName(const Name: WideString): Integer;
    procedure AddTemplate;
    procedure RemoveTemplate;
    procedure UpdateTemplateList;
    procedure SelectTemplate;
    function FindTemplateByName(const Name: WideString): Integer;
    function GetTexRect: TG2Rect;
    procedure SaveProject(const f: WideString);
    procedure LoadProject(const f: WideString);
    procedure ExportProject(const f: WideString);
  end;

var
  Form1: TForm1;
  App: TMyApp;

implementation

uses Unit2;

{$R *.dfm}

//TMyApp BEGIN
procedure TMyApp.OnRender;
begin
  Render.RenderStart;
  Render.Clear(False, True, True, $ff888888);

  Render2D.DrawRect(0, 0, Gfx.Params.Width, Gfx.Params.Height, Rect(0, 0, Gfx.Params.Width, Gfx.Params.Height), TexGrid);

  case Form1.sPageControl1.TabIndex of
    0: RenderElements;
    1: RenderTemplates;
  end;

  Font.Print(10, 10, $ff0000ff, 'FPS: ' + IntToStr(Tmr.FPS));

  Render.RenderStop;
  Render.Present;
end;

procedure TMyApp.OnUpdate;
  var Ind: Integer;
  var Tex: TG2Texture2D;
  var NewPoint: TPoint;
  var ImgPos, ImgSize: TG2Vec2;
  var mc: TPoint;
begin
  if AppClose then
  begin
    Tmr.Enabled := False;
    Form1.Close;
  end
  else
  begin
    if EditPoint <> epNone then
    begin
      Ind := Form1.sListBox1.ItemIndex;
      if Ind > -1 then
      begin
        if Elements[Ind].Texture > -1 then
        begin
          Tex := Textures[Elements[Ind].Texture].Tex;
          ImgPos.x := (Gfx.Params.Width - Tex.Width * TexScale) * 0.5 + TexPos.x;
          ImgPos.y := (Gfx.Params.Height - Tex.Height * TexScale) * 0.5 + TexPos.y;
          ImgSize.x := Tex.Width * TexScale;
          ImgSize.y := Tex.Height * TexScale;
          mc := PlugInput.MousePos;
          NewPoint.X := Round(((mc.x - ImgPos.x) / TexScale));
          NewPoint.Y := Round(((mc.y - ImgPos.y) / TexScale));
          case EditPoint of
            epLT:
            begin
              Elements[Ind].TexCoords.Left := NewPoint.X;
              Elements[Ind].TexCoords.Top := NewPoint.Y;
            end;
            epRT:
            begin
              Elements[Ind].TexCoords.Right := NewPoint.X;
              Elements[Ind].TexCoords.Top := NewPoint.Y;
            end;
            epLB:
            begin
              Elements[Ind].TexCoords.Left := NewPoint.X;
              Elements[Ind].TexCoords.Bottom := NewPoint.Y;
            end;
            epRB:
            begin
              Elements[Ind].TexCoords.Right := NewPoint.X;
              Elements[Ind].TexCoords.Bottom := NewPoint.Y;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMyApp.OnKeyDown(const Key: Byte);
begin
  if Key = DIK_SPACE then
  begin
    Gfx.Params.FullScreen := not Gfx.Params.FullScreen;
    Gfx.Params.Apply;
  end;
  if Key = DIK_ESCAPE then
  AppClose := True;
end;

procedure TMyApp.OnKeyUp(const Key: Byte);
begin

end;

procedure TMyApp.OnMouseDown(const Button: Byte);
  var Ind: Integer;
  var TexRect: TG2Rect;
begin
  case Button of
    0:
    begin
      EditPoint := epNone;
      Ind := Form1.sListBox1.ItemIndex;
      if Ind > -1 then
      begin
        if Elements[Ind].Texture > -1 then
        begin
          TexRect := GetTexRect;
          if PtInRect(Rect(Round(TexRect.Left - 4), Round(TexRect.Top - 4), Round(TexRect.Left + 4), Round(TexRect.Top + 4)),PlugInput.MousePos) then EditPoint := epLT;
          if PtInRect(Rect(Round(TexRect.Right - 4), Round(TexRect.Top - 4), Round(TexRect.Right + 4), Round(TexRect.Top + 4)),PlugInput.MousePos) then EditPoint := epRT;
          if PtInRect(Rect(Round(TexRect.Left - 4), Round(TexRect.Bottom - 4), Round(TexRect.Left + 4), Round(TexRect.Bottom + 4)),PlugInput.MousePos) then EditPoint := epLB;
          if PtInRect(Rect(Round(TexRect.Right - 4), Round(TexRect.Bottom - 4), Round(TexRect.Right + 4), Round(TexRect.Bottom + 4)),PlugInput.MousePos) then EditPoint := epRB;
        end;
      end;
    end;
  end;
end;

procedure TMyApp.OnMouseUp(const Button: Byte);
begin
  EditPoint := epNone;
end;

procedure TMyApp.OnMouseMove(const Shift: TPoint);
begin
  if PlugInput.MouseDown[1] then
  begin
    TexPos := TexPos + Shift;
  end;
end;

procedure TMyApp.OnWheelMove(const Shift: Integer);
  var Ind: Integer;
  var Tex: TG2Texture2D;
  var CurPos: TG2Vec2;
  var CurToTex: TG2Vec2;
begin
  Ind := Form1.sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    if Elements[Ind].Texture > -1 then
    begin
      Tex := Textures[Elements[Ind].Texture].Tex;
      CurPos := PlugInput.MousePos - G2Vec2(Gfx.Params.Width * 0.5, Gfx.Params.Height * 0.5);
      CurToTex := TexPos - CurPos;
      if Shift > 0 then
      begin
        TexScale := TexScale * 1.1;
        CurToTex := CurToTex * 1.1;
      end
      else
      begin
        TexScale := TexScale * 0.9;
        CurToTex := CurToTex * 0.9;
      end;
      TexPos := CurPos + CurToTex;
    end;
  end;
end;

procedure TMyApp.Initialize;
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
  AppClose := False;
  Tmr.MaxFPS := 0;
  Tmr.Mode := tmWinTimer;
  Gfx.InitParams.FormatTexture2D := D3DFMT_A8R8G8B8;
  inherited Initialize;
  g2.RequestMod(TG2TextureMgr, @MgrTextures);
  Font := Gfx.Shared.RequestFont('Arial', 16);
  TexGrid := MgrTextures.CreateTexture2DFromFile('Gris', 'gfx\Grid.png', 1);
  EditPoint := epNone;
  VB := TG2VB.Create;
  VB.Initialize(g2);
  IB := TG2IB.Create;
  IB.Initialize(g2);
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
  Tmr.Enabled := True;
end;

procedure TMyApp.Finalize;
begin
  IB.Free;
  VB.Free;
  inherited Finalize;
end;

procedure TMyApp.RenderElements;
  var Ind: Integer;
  var Tex: TG2Texture2D;
  var sc: TG2Vec2;
  var TexRect: TG2Rect;
  var c: TG2Color;
begin
  Ind := Form1.sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    if Elements[Ind].Texture > -1 then
    begin
      Tex := Textures[Elements[Ind].Texture].Tex;
      sc.SetValue((Gfx.Params.Width - Tex.Width * TexScale) * 0.5, (Gfx.Params.Height - Tex.Height * TexScale) * 0.5);
      if (Elements[Ind].TexCoords.Left = -1)
      and (Elements[Ind].TexCoords.Top = -1)
      and (Elements[Ind].TexCoords.Right = -1)
      and (Elements[Ind].TexCoords.Bottom = -1) then
      Elements[Ind].TexCoords := Rect(0, 0, Tex.Width, Tex.Height);
      Render2D.DrawRect(
        sc.x + TexPos.x,
        sc.y + TexPos.y,
        Tex.Width * TexScale,
        Tex.Height * TexScale,
        $ffffffff,
        Tex
      );
      Render.TextureClear();
      TexRect := GetTexRect;
      Prim2D.DrawRectHollow(
        TexRect.Left, TexRect.Top, TexRect.Right - TexRect.Left, TexRect.Bottom - TexRect.Top,
        $ffff0000
      );
      if PtInRect(Rect(Round(TexRect.Left - 4), Round(TexRect.Top - 4), Round(TexRect.Left + 4), Round(TexRect.Top + 4)),PlugInput.MousePos) then c := $ff00ff00 else c := $ffffff00;
      Prim2D.DrawRect(TexRect.Left - 4, TexRect.Top - 4, 8, 8, c);
      Prim2D.DrawRectHollow(TexRect.Left - 4, TexRect.Top - 4, 8, 8, $ffff0000);
      if PtInRect(Rect(Round(TexRect.Right - 4), Round(TexRect.Top - 4), Round(TexRect.Right + 4), Round(TexRect.Top + 4)),PlugInput.MousePos) then c := $ff00ff00 else c := $ffffff00;
      Prim2D.DrawRect(TexRect.Right - 4, TexRect.Top - 4, 8, 8, c);
      Prim2D.DrawRectHollow(TexRect.Right - 4, TexRect.Top - 4, 8, 8, $ffff0000);
      if PtInRect(Rect(Round(TexRect.Left - 4), Round(TexRect.Bottom - 4), Round(TexRect.Left + 4), Round(TexRect.Bottom + 4)),PlugInput.MousePos) then c := $ff00ff00 else c := $ffffff00;
      Prim2D.DrawRect(TexRect.Left - 4, TexRect.Bottom - 4, 8, 8, c);
      Prim2D.DrawRectHollow(TexRect.Left - 4, TexRect.Bottom - 4, 8, 8, $ffff0000);
      if PtInRect(Rect(Round(TexRect.Right - 4), Round(TexRect.Bottom - 4), Round(TexRect.Right + 4), Round(TexRect.Bottom + 4)),PlugInput.MousePos) then c := $ff00ff00 else c := $ffffff00;
      Prim2D.DrawRect(TexRect.Right - 4, TexRect.Bottom - 4, 8, 8, c);
      Prim2D.DrawRectHollow(TexRect.Right - 4, TexRect.Bottom - 4, 8, 8, $ffff0000);
    end;
  end;
end;

procedure TMyApp.RenderTemplates;
  var Vertices: PVertexArr;
  var t, l, e, ts: Integer;
  var RenderRect: TRect;
  var bw, bh, rbs: Single;
  var tsx, tsy: Single;
  var Tex: TG2Texture2D;
  procedure SetPositions;
    var x0, x1, x2, x3, y0, y1, y2, y3: Single;
  begin
    x0 := RenderRect.Left - 0.25;
    x1 := RenderRect.Left + rbs - 0.25;
    x2 := RenderRect.Right - rbs + 0.25;
    x3 := RenderRect.Right + 0.25;
    y0 := RenderRect.Top - 0.25;
    y1 := RenderRect.Top + rbs - 0.25;
    y2 := RenderRect.Bottom - rbs + 0.25;
    y3 := RenderRect.Bottom + 0.25;
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
  procedure SetTexCoordsBorder;
    var tcxl, tcxm, tcxh, tcyl, tcym, tcyh: Single;
  begin
    tcxl := Elements[e].TexCoords.Left * tsx;
    tcxm := (Elements[e].TexCoords.Right + Elements[e].TexCoords.Left) * 0.5 * tsx;
    tcxh := Elements[e].TexCoords.Right * tsx;
    tcyl := Elements[e].TexCoords.Top * tsy;
    tcym := (Elements[e].TexCoords.Bottom + Elements[e].TexCoords.Top) * 0.5 * tsy;
    tcyh := Elements[e].TexCoords.Bottom * tsy;
    Vertices^[0].Tex[ts].SetValue(tcxl, tcyl);
    Vertices^[1].Tex[ts].SetValue(tcxm, tcyl);
    Vertices^[2].Tex[ts].SetValue(tcxm, tcyl);
    Vertices^[3].Tex[ts].SetValue(tcxh, tcyl);
    Vertices^[4].Tex[ts].SetValue(tcxl, tcym);
    Vertices^[5].Tex[ts].SetValue(tcxm, tcym);
    Vertices^[6].Tex[ts].SetValue(tcxm, tcym);
    Vertices^[7].Tex[ts].SetValue(tcxh, tcym);
    Vertices^[8].Tex[ts].SetValue(tcxl, tcym);
    Vertices^[9].Tex[ts].SetValue(tcxm, tcym);
    Vertices^[10].Tex[ts].SetValue(tcxm, tcym);
    Vertices^[11].Tex[ts].SetValue(tcxh, tcym);
    Vertices^[12].Tex[ts].SetValue(tcxl, tcyh);
    Vertices^[13].Tex[ts].SetValue(tcxm, tcyh);
    Vertices^[14].Tex[ts].SetValue(tcxm, tcyh);
    Vertices^[15].Tex[ts].SetValue(tcxh, tcyh);
  end;
  procedure SetTexCoordsStretch;
    var tcx0, tcx1, tcx2, tcx3, tcy0, tcy1, tcy2, tcy3: Single;
    var tbw, tbh: Single;
  begin
    tbw := rbs * ((Elements[e].TexCoords.Right - Elements[e].TexCoords.Left) / (RenderRect.Right - RenderRect.Left + 1));
    tbh := rbs * ((Elements[e].TexCoords.Bottom - Elements[e].TexCoords.Top) / (RenderRect.Bottom - RenderRect.Top + 1));
    tcx0 := Elements[e].TexCoords.Left * tsx;
    tcx1 := (Elements[e].TexCoords.Left + tbw) * tsx;
    tcx2 := (Elements[e].TexCoords.Right - tbw) * tsx;
    tcx3 := Elements[e].TexCoords.Right * tsx;
    tcy0 := Elements[e].TexCoords.Top * tsy;
    tcy1 := (Elements[e].TexCoords.Top + tbh) * tsy;
    tcy2 := (Elements[e].TexCoords.Bottom - tbh) * tsy;
    tcy3 := Elements[e].TexCoords.Bottom * tsy;
    Vertices^[0].Tex[ts].SetValue(tcx0, tcy0);
    Vertices^[1].Tex[ts].SetValue(tcx1, tcy0);
    Vertices^[2].Tex[ts].SetValue(tcx2, tcy0);
    Vertices^[3].Tex[ts].SetValue(tcx3, tcy0);
    Vertices^[4].Tex[ts].SetValue(tcx0, tcy1);
    Vertices^[5].Tex[ts].SetValue(tcx1, tcy1);
    Vertices^[6].Tex[ts].SetValue(tcx2, tcy1);
    Vertices^[7].Tex[ts].SetValue(tcx3, tcy1);
    Vertices^[8].Tex[ts].SetValue(tcx0, tcy2);
    Vertices^[9].Tex[ts].SetValue(tcx1, tcy2);
    Vertices^[10].Tex[ts].SetValue(tcx2, tcy2);
    Vertices^[11].Tex[ts].SetValue(tcx3, tcy2);
    Vertices^[12].Tex[ts].SetValue(tcx0, tcy3);
    Vertices^[13].Tex[ts].SetValue(tcx1, tcy3);
    Vertices^[14].Tex[ts].SetValue(tcx2, tcy3);
    Vertices^[15].Tex[ts].SetValue(tcx3, tcy3);
  end;
  procedure SetTexCoordsTile;
    var TileCountX: Single;
    var TileCountY: Single;
    var bwg, bhg: Single;
    var rw, rh: Single;
    var tcx0, tcx1, tcx2, tcx3, tcy0, tcy1, tcy2, tcy3: Single;
  begin
    rw := (RenderRect.Right - RenderRect.Left + 1);
    rh := (RenderRect.Bottom - RenderRect.Top + 1);
    TileCountX := rw / Tex.RealWidth;
    TileCountY := rh / Tex.RealHeight;
    bwg := (rbs) / rw;
    bhg := (rbs) / rh;
    tcx0 := 0;
    tcx1 := TileCountX * bwg;
    tcx2 := TileCountX * (1 - bwg);
    tcx3 := TileCountX;
    tcy0 := 0;
    tcy1 := TileCountY * bhg;
    tcy2 := TileCountY * (1 - bhg);
    tcy3 := TileCountY;
    Vertices^[0].Tex[ts].SetValue(tcx0, tcy0);
    Vertices^[1].Tex[ts].SetValue(tcx1, tcy0);
    Vertices^[2].Tex[ts].SetValue(tcx2, tcy0);
    Vertices^[3].Tex[ts].SetValue(tcx3, tcy0);
    Vertices^[4].Tex[ts].SetValue(tcx0, tcy1);
    Vertices^[5].Tex[ts].SetValue(tcx1, tcy1);
    Vertices^[6].Tex[ts].SetValue(tcx2, tcy1);
    Vertices^[7].Tex[ts].SetValue(tcx3, tcy1);
    Vertices^[8].Tex[ts].SetValue(tcx0, tcy2);
    Vertices^[9].Tex[ts].SetValue(tcx1, tcy2);
    Vertices^[10].Tex[ts].SetValue(tcx2, tcy2);
    Vertices^[11].Tex[ts].SetValue(tcx3, tcy2);
    Vertices^[12].Tex[ts].SetValue(tcx0, tcy3);
    Vertices^[13].Tex[ts].SetValue(tcx1, tcy3);
    Vertices^[14].Tex[ts].SetValue(tcx2, tcy3);
    Vertices^[15].Tex[ts].SetValue(tcx3, tcy3);
  end;
begin
  RenderRect.Left := Gfx.Params.Width div 2 - 128;
  RenderRect.Top := Gfx.Params.Height div 2 - 128;
  RenderRect.Right := RenderRect.Left + 256;
  RenderRect.Bottom := RenderRect.Top + 256;
  t := Form1.sListBox2.ItemIndex;
  if t > -1 then
  begin
    rbs := Templates[t].BorderSize + 0.1;
    VB.Lock(0, VB.Count * VB.Stride, Pointer(Vertices), D3DLOCK_DISCARD);
    SetPositions;
    ts := 0;
    for l := 0 to 3 do
    begin
      e := Templates[t].Layers[l];
      if e > -1 then
      begin
        if Elements[e].Texture > -1 then
        begin
          Tex := Textures[Elements[e].Texture].Tex;
          tsx := 1 / Tex.RealWidth;
          tsy := 1 / Tex.RealHeight;
          bw := (Elements[e].TexCoords.Right - Elements[e].TexCoords.Left) * 0.5;
          bh := (Elements[e].TexCoords.Bottom - Elements[e].TexCoords.Top) * 0.5;
          case Elements[e].MapType of
            mtStretch: SetTexCoordsStretch;
            mtTile: SetTexCoordsTile;
            mtBorder: SetTexCoordsBorder;
          end;
          Render.TextureSet(Tex, ts);
          if ts = 0 then
          begin
            Gfx.TextureStageStages.AlphaArg1[ts] := D3DTA_TEXTURE;
            Gfx.TextureStageStages.AlphaOp[ts] := D3DTOP_SELECTARG1;
          end
          else
          begin
            if l = 3 then
            begin
              Gfx.TextureStageStages.ColorArg1[ts] := D3DTA_CURRENT;
              Gfx.TextureStageStages.ColorOp[ts] := D3DTOP_SELECTARG1;
              Gfx.TextureStageStages.AlphaArg1[ts] := D3DTA_TEXTURE;
              Gfx.TextureStageStages.AlphaOp[ts] := D3DTOP_SELECTARG1;
            end
            else
            begin
              Gfx.TextureStageStages.ColorArg1[ts] := D3DTA_CURRENT;
              Gfx.TextureStageStages.ColorArg2[ts] := D3DTA_TEXTURE;
              Gfx.TextureStageStages.ColorOp[ts] := D3DTOP_BLENDCURRENTALPHA;
              Gfx.TextureStageStages.AlphaArg1[ts] := D3DTA_TEXTURE;
              Gfx.TextureStageStages.AlphaArg2[ts] := D3DTA_CURRENT;
              Gfx.TextureStageStages.AlphaOp[ts] := D3DTOP_ADD;
            end;
          end;
          Inc(ts);
        end;
      end;
    end;
    VB.UnLock;
    if ts > 0 then
    begin
      IB.SetToDevice;
      VB.SetToDevice;
      Gfx.Device.DrawIndexedPrimitive(
        D3DPT_TRIANGLELIST,
        0, 0, VB.Count,
        0, IB.Count div 3
      );
      Gfx.TextureStageStages.SetDefaults;
    end;
  end;
end;

procedure TMyApp.UpdateElementList;
  var i: Integer;
begin
  Form1.sListBox1.Clear;
  Form1.cbb3.Clear;
  Form1.sListBox1.Items.BeginUpdate;
  Form1.cbb3.Items.BeginUpdate;
  for i := 0 to High(Elements) do
  begin
    Form1.sListBox1.Items.Append(Elements[i].Name);
    Form1.cbb3.Items.Append(Elements[i].Name);
  end;
  Form1.sListBox1.Items.EndUpdate;
  Form1.cbb3.Items.EndUpdate;
end;

procedure TMyApp.AddElement;
  var i: Integer;
  var BaseName, NewName: WideString;
begin
  BaseName := 'NewElement';
  NewName := BaseName;
  i := 0;
  while FindElementByName(NewName) > -1 do
  begin
    NewName := BaseName + IntToStr(i);
    Inc(i);
  end;
  SetLength(Elements, Length(Elements) + 1);
  Elements[High(Elements)].Name := NewName;
  Elements[High(Elements)].MapType := mtTile;
  Elements[High(Elements)].TexCoords := Rect(-1, -1, -1, -1);
  Elements[High(Elements)].Texture := -1;
  Form1.sListBox1.ItemIndex := High(Elements);
  UpdateElementList;
  SelectElement;
end;

procedure TMyApp.RemoveElement;
  var i, j, Ind: Integer;
begin
  Ind := Form1.sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    for i := 0 to High(Templates) do
    for j := 0 to 3 do
    if Templates[i].Layers[j] = Ind then
    Templates[i].Layers[j] := -1;
    for i := Ind to High(Elements) - 1 do
    begin
      Elements[i].Name := Elements[i + 1].Name;
      Elements[i].MapType := Elements[i + 1].MapType;
      Elements[i].TexCoords := Elements[i + 1].TexCoords;
      Elements[i].Texture := Elements[i + 1].Texture;
    end;
    SetLength(Elements, Length(Elements) - 1);
  end;
  UpdateElementList;
  Form1.spnl3.Visible := False;
end;

procedure TMyApp.SelectElement;
  var Ind: Integer;
begin
  Ind := Form1.sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    Form1.sEdit1.Text := Elements[Ind].Name;
    Form1.rb1.Checked := Elements[Ind].MapType = mtStretch;
    Form1.rb2.Checked := Elements[Ind].MapType = mtTile;
    Form1.rb3.Checked := Elements[Ind].MapType = mtBorder;
    Form1.cbb1.ItemIndex := Elements[Ind].Texture;
    Form1.spnl3.Visible := True;
    TexPos.SetValue(0, 0);
    TexScale := 1;
  end
  else
  begin
    Form1.spnl3.Visible := False;
  end;
end;

function TMyApp.FindElementByName(const Name: WideString): Integer;
  var i: Integer;
begin
  for i := 0 to High(Elements) do
  if LowerCase(Elements[i].Name) = LowerCase(Name) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

procedure TMyApp.AddTextures;
  procedure LoadTexture(const Path: WideString);
    var Ind, i: Integer;
    var BaseName, NewName, Ext: AnsiString;
  begin
    Ind := FindTextureByPath(Path);
    if Ind = -1 then
    begin
      BaseName := ExtractFileName(Path);
      Ext := ExtractFileExt(Path);
      Delete(BaseName, Length(BaseName) + 1 - Length(Ext), Length(Ext));
      NewName := BaseName;
      i := 0;
      while FindTextureByName(NewName) > -1 do
      begin
        NewName := BaseName + IntToStr(i);
        Inc(i);
      end;
      SetLength(Textures, Length(Textures) + 1);
      Textures[High(Textures)].Name := NewName;
      Textures[High(Textures)].Path := Path;
      Textures[High(Textures)].Tex := MgrTextures.CreateTexture2DFromFile(AnsiString(NewName), Path, 1);
    end;
  end;
  var i: Integer;
begin
  if Form2.opd1.Execute() then
  begin
    Form2.sGauge1.Progress := 0;
    for i := 0 to Form2.opd1.Files.Count - 1 do
    begin
      LoadTexture(LowerCase(Form2.opd1.Files[i]));
      Form2.sGauge1.Progress := Round(i / (Form2.opd1.Files.Count - 1) * 100);
      Application.ProcessMessages;
    end;
  end;
  UpdateTextureList;
  Form2.sGauge1.Progress := 0;
end;

procedure TMyApp.RemoveTextures;
  procedure RemoveTexture(const Ind: Integer);
    var i, j: Integer;
  begin
    for i := 0 to High(Elements) do
    if Elements[i].Texture = Ind then
    Elements[i].Texture := -1;
    Textures[Ind].Tex.Free;
    for i := Ind to High(Textures) - 1 do
    begin
      for j := 0 to High(Elements) do
      if Elements[j].Texture = i + 1 then
      Elements[j].Texture := i;
      Textures[i].Tex := Textures[i + 1].Tex;
      Textures[i].Name := Textures[i + 1].Name;
      Textures[i].Path := Textures[i + 1].Path;
    end;
    SetLength(Textures, Length(Textures) - 1);
  end;
  var i: Integer;
  var SelArr: array of Integer;
begin
  for i := High(Textures) downto 0 do
  begin
    if Form2.sListBox1.Selected[i] then
    begin
      SetLength(SelArr, Length(SelArr) + 1);
      SelArr[High(SelArr)] := i;
    end;
  end;
  for i := 0 to High(SelArr) do
  RemoveTexture(i);
  UpdateTextureList;
  SelectElement;
end;

procedure TMyApp.UpdateTextureList;
  var i: Integer;
begin
  Form2.sListBox1.Clear;
  Form1.cbb1.Clear;
  Form2.sListBox1.Items.BeginUpdate;
  Form1.cbb1.Items.BeginUpdate;
  for i := 0 to High(Textures) do
  begin
    Form2.sListBox1.Items.Append(Textures[i].Name + ' (' + Textures[i].Path + ')');
    Form1.cbb1.Items.Append(Textures[i].Name);
  end;
  Form2.sListBox1.Items.EndUpdate;
  Form1.cbb1.Items.EndUpdate;
end;

function TMyApp.FindTextureByPath(const Path: WideString): Integer;
  var i: Integer;
begin
  for i := 0 to High(Textures) do
  if Textures[i].Path = Path then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TMyApp.FindTextureByName(const Name: WideString): Integer;
  var i: Integer;
begin
  for i := 0 to High(Textures) do
  if LowerCase(Textures[i].Name) = LowerCase(Name) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

procedure TMyApp.AddTemplate;
  var BaseName: WideString;
  var NewName: WideString;
  var i: Integer;
begin
  BaseName := 'NewTemplate';
  NewName := BaseName;
  i := 0;
  while FindTemplateByName(NewName) > -1 do
  begin
    NewName := BaseName + IntToStr(i);
    Inc(i);
  end;
  SetLength(Templates, Length(Templates) + 1);
  Templates[High(Templates)].Name := NewName;
  for i := 0 to High(Templates[High(Templates)].Layers) do
  begin
    Templates[High(Templates)].Layers[i] := -1;
    Templates[High(Templates)].BorderSize := 8;
  end;
  UpdateTemplateList;
end;

procedure TMyApp.RemoveTemplate;
  var Ind, i, j: Integer;
begin
  Ind := Form1.sListBox2.ItemIndex;
  if Ind > -1 then
  begin
    for i := Ind to High(Templates) - 1 do
    begin
      Templates[i].Name := Templates[i + 1].Name;
      for j := 0 to 3 do
      begin
        Templates[i].Layers[j] := Templates[i + 1].Layers[j];
        Templates[i].BorderSize := Templates[i + 1].BorderSize;
      end;
    end;
    SetLength(Templates, Length(Templates) - 1);
    UpdateTemplateList;
    Form1.spnl8.Visible := False;
  end;
end;

procedure TMyApp.UpdateTemplateList;
  var i: Integer;
begin
  Form1.sListBox2.Clear;
  Form1.sListBox2.Items.BeginUpdate;
  for i := 0 to High(Templates) do
  Form1.sListBox2.Items.Append(Templates[i].Name);
  Form1.sListBox2.Items.EndUpdate;
end;

procedure TMyApp.SelectTemplate;
  var Ind, i: Integer;
begin
  Ind := Form1.sListBox2.ItemIndex;
  if Ind > -1 then
  begin
    Form1.sEdit2.Text := Templates[Ind].Name;
    Form1.cbb2.ItemIndex := i;
    Form1.edt1.Value := Templates[Ind].BorderSize;
    Form1.cbb2Change(Form1);
    Form1.spnl8.Visible := True;
  end
  else
  begin
    Form1.spnl8.Visible := False;
  end;
end;

function TMyApp.FindTemplateByName(const Name: WideString): Integer;
  var i: Integer;
begin
  for i := 0 to High(Templates) do
  if LowerCase(Templates[i].Name) = LowerCase(Name) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TMyApp.GetTexRect: TG2Rect;
  var Ind: Integer;
  var Tex: TG2Texture2D;
  var sc: TG2Vec2;
begin
  Result := G2Rect(0, 0, 0, 0);
  Ind := Form1.sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    if Elements[Ind].Texture > -1 then
    begin
      Tex := Textures[Elements[Ind].Texture].Tex;
      sc.SetValue((Gfx.Params.Width - Tex.Width * TexScale) * 0.5, (Gfx.Params.Height - Tex.Height * TexScale) * 0.5);
      Result.Left := sc.x + TexPos.x + Elements[Ind].TexCoords.Left * TexScale;
      Result.Top := sc.y + TexPos.y + Elements[Ind].TexCoords.Top * TexScale;
      Result.Right := sc.x + TexPos.x + Elements[Ind].TexCoords.Right * TexScale;
      Result.Bottom := sc.y + TexPos.y + Elements[Ind].TexCoords.Bottom * TexScale;
    end;
  end;
end;

procedure TMyApp.SaveProject(const f: WideString);
  const Header: array[0..3] of AnsiChar = 'GUIP';
  var fs: TG2FileRW;
  var i: Integer;
begin
  fs := TG2FileRW.Create;
  fs.OpenWrite(f);
  try
    fs.WriteBuffer(Header, 4);
    fs.WriteSInt4(Length(Elements));
    fs.WriteSInt4(Length(Textures));
    fs.WriteSInt4(Length(Templates));
    for i := 0 to High(Elements) do
    begin
      fs.WriteWideString(Elements[i].Name);
      fs.WriteBuffer(Elements[i].MapType, 1);
      fs.WriteBuffer(Elements[i].TexCoords, SizeOf(TRect));
      fs.WriteSInt4(Elements[i].Texture);
    end;
    for i := 0 to High(Textures) do
    begin
      fs.WriteWideString(Textures[i].Name);
      fs.WriteWideString(Textures[i].Path);
    end;
    for i := 0 to High(Templates) do
    begin
      fs.WriteWideString(Templates[i].Name);
      fs.WriteBuffer(Templates[i].Layers[0], 16);
      fs.WriteUInt1(Templates[i].BorderSize);
    end;
  finally
    fs.Close;
    fs.Free;
  end;
end;

procedure TMyApp.LoadProject(const f: WideString);
  var Header: array[0..3] of AnsiChar;
  var fs: TG2FileRW;
  var i: Integer;
begin
  fs := TG2FileRW.Create;
  fs.OpenRead(f);
  try
    fs.ReadBuffer(Header, 4);
    if Header = 'GUIP' then
    begin
      i := fs.ReadSInt4;
      SetLength(Elements, i);
      i := fs.ReadSInt4;
      SetLength(Textures, i);
      i := fs.ReadSInt4;
      SetLength(Templates, i);
      for i := 0 to High(Elements) do
      begin
        Elements[i].Name := fs.ReadWideString;
        Elements[i].MapType := TMapType(fs.ReadUInt1);
        fs.ReadBuffer(Elements[i].TexCoords, SizeOf(TRect));
        Elements[i].Texture := fs.ReadSInt4;
      end;
      for i := 0 to High(Textures) do
      begin
        Textures[i].Name := fs.ReadWideString;
        Textures[i].Path := fs.ReadWideString;
        Textures[i].Tex := MgrTextures.CreateTexture2DFromFile(AnsiString(Textures[i].Name), Textures[i].Path, 1);
      end;
      for i := 0 to High(Templates) do
      begin
        Templates[i].Name := fs.ReadWideString;
        fs.ReadBuffer(Templates[i].Layers, 16);
        Templates[i].BorderSize := fs.ReadUInt1;
      end;
      UpdateElementList;
      UpdateTextureList;
      UpdateTemplateList;
    end;
  finally
    fs.Close;
    fs.Free;
  end;
end;

procedure TMyApp.ExportProject(const f: WideString);
  const Header: array[0..3] of AnsiChar = 'G2UI';
  var fs: TG2FileRW;
  var i, j, t, x, y: Integer;
  var p1, p2: Int64;
  var b: Boolean;
  var TextureRemap: array of Integer;
  var png: TPNGImage;
  var bmp: TBitmap;
  var ca: PG2ColorArray;
  var pb: PByteArray;
begin
  fs := TG2FileRW.Create;
  fs.OpenWrite(f);
  try
    t := 0;
    SetLength(TextureRemap, Length(Textures));
    for i := 0 to High(Textures) do
    begin
      b := False;
      for j := 0 to High(Elements) do
      if Elements[j].Texture = i then
      begin
        b := True;
        Break;
      end;
      if b then
      begin
        TextureRemap[i] := t;
        Inc(t);
      end
      else
      TextureRemap[i] := -1;
    end;
    fs.WriteBuffer(Header, 4);
    fs.WriteSInt4(t);
    png := TPNGImage.Create;
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf32bit;
    for i := 0 to High(Textures) do
    if TextureRemap[i] > -1 then
    begin
      bmp.Width := Textures[i].Tex.Width;
      bmp.Height := Textures[i].Tex.Height;
      Textures[i].Tex.Surfaces[0].Lock();
      for y := 0 to bmp.Height - 1 do
      begin
        ca := bmp.ScanLine[y];
        for x := 0 to bmp.Width - 1 do
        ca^[x] := Textures[i].Tex.Surfaces[0].Pixels[x, y];
      end;
      Textures[i].Tex.Surfaces[0].UnLock;
      png.Assign(bmp);
      png.CreateAlpha;
      for y := 0 to bmp.Height - 1 do
      begin
        ca := bmp.ScanLine[y];
        pb := png.AlphaScanline[y];
        for x := 0 to bmp.Width - 1 do
        pb^[x] := ca^[x].a;
      end;
      fs.WriteWideString(Textures[i].Name);
      p1 := fs.Position;
      t := 0;
      fs.WriteSInt4(t);
      png.SaveToStream(fs.Stream);
      p2 := fs.Position;
      t := p2 - (p1 + 4);
      fs.Position := p1;
      fs.WriteSInt4(t);
      fs.Position := p2;
    end;
    bmp.Free;
    png.Free;
    fs.WriteSInt4(Length(Elements));
    for i := 0 to High(Elements) do
    begin
      fs.WriteWideString(Elements[i].Name);
      fs.WriteUInt1(Byte(Elements[i].MapType));
      fs.WriteFloat4(Elements[i].TexCoords.Left / Textures[TextureRemap[Elements[i].Texture]].Tex.RealWidth);
      fs.WriteFloat4(Elements[i].TexCoords.Top / Textures[TextureRemap[Elements[i].Texture]].Tex.RealHeight);
      fs.WriteFloat4(Elements[i].TexCoords.Right / Textures[TextureRemap[Elements[i].Texture]].Tex.RealWidth);
      fs.WriteFloat4(Elements[i].TexCoords.Bottom / Textures[TextureRemap[Elements[i].Texture]].Tex.RealHeight);
      fs.WriteSInt4(TextureRemap[Elements[i].Texture]);
    end;
    fs.WriteSInt4(Length(Templates));
    for i := 0 to High(Templates) do
    begin
      fs.WriteWideString(Templates[i].Name);
      fs.WriteBuffer(Templates[i].Layers[0], 16);
      fs.WriteUInt1(Templates[i].BorderSize);
    end;
  finally
    fs.Close;
    fs.Free;
  end;
end;
//TMyApp END

procedure TForm1.btn1Click(Sender: TObject);
begin
  App.AddElement;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  if sd1.Execute() then
  App.SaveProject(sd1.FileName);
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  if od1.Execute() then
  App.LoadProject(od1.FileName);
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  if sd2.Execute() then
  App.ExportProject(sd2.FileName);
end;

procedure TForm1.btn5Click(Sender: TObject);
begin
  App.RemoveElement;
end;

procedure TForm1.btn6Click(Sender: TObject);
begin
  App.AddTemplate;
end;

procedure TForm1.btn7Click(Sender: TObject);
begin
  App.RemoveTemplate;
end;

procedure TForm1.cbb1Change(Sender: TObject);
  var Ind: Integer;
begin
  Ind := sListBox1.ItemIndex;
  if Ind > -1 then
  App.Elements[Ind].Texture := cbb1.ItemIndex;
end;

procedure TForm1.cbb2Change(Sender: TObject);
  var Ind, i: Integer;
begin
  Ind := sListBox2.ItemIndex;
  if Ind > -1 then
  begin
    i := cbb2.ItemIndex;
    cbb3.ItemIndex := App.Templates[Ind].Layers[i];
  end;
end;

procedure TForm1.cbb3Change(Sender: TObject);
  var Ind, i: Integer;
begin
  Ind := sListBox2.ItemIndex;
  if Ind > -1 then
  begin
    i := cbb2.ItemIndex;
    if i > -1 then
    begin
      App.Templates[Ind].Layers[i] := cbb3.ItemIndex;
    end;
  end;
end;

procedure TForm1.edt1Change(Sender: TObject);
  var Ind, i: Integer;
begin
  Ind := sListBox2.ItemIndex;
  if Ind > -1 then
  begin
    App.Templates[Ind].BorderSize := edt1.Value;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  App.Free;
  App := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Constraints.MinWidth := Form1.Width;
  Form1.Constraints.MinHeight := Form1.Height;
  App := TMyApp.Create;
  App.Handle := pnl1.Handle;
  App.Gfx.InitParams.Width := pnl1.ClientWidth;
  App.Gfx.InitParams.Height := pnl1.ClientHeight;
  App.Initialize;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if (App <> nil)
  and (App.g2.Initialized) then
  with App do
  begin
    Gfx.Params.Width := pnl1.ClientWidth;
    Gfx.Params.Height := pnl1.ClientHeight;
    Gfx.Params.Apply;
  end;
end;

procedure TForm1.rb1Click(Sender: TObject);
  var Ind: Integer;
begin
  Ind := sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    App.Elements[Ind].MapType := mtStretch;
  end;
end;

procedure TForm1.rb2Click(Sender: TObject);
  var Ind: Integer;
begin
  Ind := sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    App.Elements[Ind].MapType := mtTile;
  end;
end;

procedure TForm1.rb3Click(Sender: TObject);
  var Ind: Integer;
begin
  Ind := sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    App.Elements[Ind].MapType := mtBorder;
  end;
end;

procedure TForm1.sButton2Click(Sender: TObject);
begin
  Form2.ShowModal;
end;

procedure TForm1.sEdit1Change(Sender: TObject);
  var Ind: Integer;
begin
  Ind := sListBox1.ItemIndex;
  if Ind > -1 then
  begin
    App.Elements[Ind].Name := sEdit1.Text;
    sListBox1.Items[Ind] := App.Elements[Ind].Name;
    cbb3.Items[Ind] := App.Elements[Ind].Name;
  end;
end;

procedure TForm1.sEdit2Change(Sender: TObject);
  var Ind: Integer;
begin
  Ind := sListBox2.ItemIndex;
  if Ind > -1 then
  begin
    App.Templates[Ind].Name := sEdit2.Text;
    sListBox2.Items[Ind] := App.Templates[Ind].Name;
  end;
end;

procedure TForm1.sListBox1Click(Sender: TObject);
begin
  App.SelectElement;
end;

procedure TForm1.sListBox2Click(Sender: TObject);
begin
  App.SelectTemplate;
end;

procedure TForm1.sPageControl1Change(Sender: TObject);
begin
  case sPageControl1.TabIndex of
    0: App.SelectElement;
    1: App.SelectTemplate;
  end;
end;

end.
