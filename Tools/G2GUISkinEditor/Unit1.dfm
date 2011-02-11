object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Gen2 GUI Skin Editor'
  ClientHeight = 526
  ClientWidth = 893
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 893
    Height = 526
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = GridPanel2
        Row = 0
      end
      item
        Column = 0
        Control = sPanel2
        Row = 1
      end>
    Ctl3D = False
    ParentCtl3D = False
    RowCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 40.000000000000000000
      end>
    TabOrder = 0
    object GridPanel2: TGridPanel
      Left = 0
      Top = 0
      Width = 893
      Height = 486
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 100.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 203.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = pnl1
          Row = 0
        end
        item
          Column = 1
          Control = spnl1
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 690
        Height = 486
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
      end
      object spnl1: TsPanel
        Left = 690
        Top = 0
        Width = 203
        Height = 486
        Align = alClient
        Caption = 'spnl1'
        TabOrder = 1
        SkinData.SkinSection = 'PANEL'
        object sPageControl1: TsPageControl
          Left = 1
          Top = 1
          Width = 201
          Height = 484
          ActivePage = sTabSheet1
          Align = alClient
          TabOrder = 0
          OnChange = sPageControl1Change
          SkinData.SkinSection = 'PAGECONTROL'
          object sTabSheet1: TsTabSheet
            Caption = 'Elements'
            SkinData.CustomColor = False
            SkinData.CustomFont = False
            object spnl5: TsPanel
              Left = 0
              Top = 0
              Width = 193
              Height = 257
              Caption = 'spnl1'
              ShowCaption = False
              TabOrder = 0
              SkinData.SkinSection = 'GROUPBOX'
              object spnl2: TsPanel
                Left = 0
                Top = 0
                Width = 193
                Height = 25
                Caption = 'Elements'
                TabOrder = 0
                SkinData.SkinSection = 'PANEL'
              end
              object sListBox1: TsListBox
                Left = 0
                Top = 24
                Width = 193
                Height = 185
                Color = 16776441
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                OnClick = sListBox1Click
                BoundLabel.Indent = 0
                BoundLabel.Font.Charset = DEFAULT_CHARSET
                BoundLabel.Font.Color = clWindowText
                BoundLabel.Font.Height = -11
                BoundLabel.Font.Name = 'Tahoma'
                BoundLabel.Font.Style = []
                BoundLabel.Layout = sclLeft
                BoundLabel.MaxWidth = 0
                BoundLabel.UseSkinColor = True
                SkinData.SkinSection = 'EDIT'
              end
              object btn1: TsButton
                Left = 0
                Top = 208
                Width = 193
                Height = 25
                Caption = 'Add Element'
                TabOrder = 2
                OnClick = btn1Click
                SkinData.SkinSection = 'TOOLBAR'
              end
              object btn5: TsButton
                Left = 0
                Top = 232
                Width = 193
                Height = 25
                Caption = 'Remove Element'
                TabOrder = 3
                OnClick = btn5Click
                SkinData.SkinSection = 'TOOLBAR'
              end
            end
            object spnl3: TsPanel
              Left = 0
              Top = 264
              Width = 193
              Height = 177
              Caption = 'spnl3'
              ShowCaption = False
              TabOrder = 1
              Visible = False
              SkinData.SkinSection = 'GROUPBOX'
              object spnl4: TsPanel
                Left = 0
                Top = 0
                Width = 193
                Height = 25
                Caption = 'Properties'
                TabOrder = 0
                SkinData.SkinSection = 'PANEL'
              end
              object sEdit1: TsEdit
                Left = 8
                Top = 40
                Width = 177
                Height = 19
                Color = 16776441
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                OnChange = sEdit1Change
                SkinData.SkinSection = 'EDIT'
                BoundLabel.Active = True
                BoundLabel.Caption = 'Name'
                BoundLabel.Indent = 0
                BoundLabel.Font.Charset = DEFAULT_CHARSET
                BoundLabel.Font.Color = clBlack
                BoundLabel.Font.Height = -11
                BoundLabel.Font.Name = 'Tahoma'
                BoundLabel.Font.Style = []
                BoundLabel.Layout = sclTopLeft
                BoundLabel.MaxWidth = 0
                BoundLabel.UseSkinColor = True
              end
              object cbb1: TsComboBox
                Left = 8
                Top = 72
                Width = 177
                Height = 22
                AutoDropDown = True
                Alignment = taLeftJustify
                BoundLabel.Active = True
                BoundLabel.Caption = 'Texture'
                BoundLabel.Indent = 0
                BoundLabel.Font.Charset = DEFAULT_CHARSET
                BoundLabel.Font.Color = clBlack
                BoundLabel.Font.Height = -11
                BoundLabel.Font.Name = 'Tahoma'
                BoundLabel.Font.Style = []
                BoundLabel.Layout = sclTopLeft
                BoundLabel.MaxWidth = 0
                BoundLabel.UseSkinColor = True
                SkinData.SkinSection = 'COMBOBOX'
                Style = csDropDownList
                Color = 16776441
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ItemIndex = -1
                ParentFont = False
                TabOrder = 2
                OnChange = cbb1Change
              end
              object sRadioGroup1: TsRadioGroup
                Left = 8
                Top = 96
                Width = 177
                Height = 73
                Caption = 'Mapping'
                ParentBackground = False
                TabOrder = 3
                SkinData.SkinSection = 'GROUPBOX'
              end
              object rb1: TsRadioButton
                Left = 16
                Top = 112
                Width = 63
                Height = 19
                Caption = 'Stretch'
                TabOrder = 4
                OnClick = rb1Click
                SkinData.SkinSection = 'RADIOBUTTON'
              end
              object rb2: TsRadioButton
                Left = 16
                Top = 128
                Width = 44
                Height = 19
                Caption = 'Tile'
                TabOrder = 5
                OnClick = rb2Click
                SkinData.SkinSection = 'RADIOBUTTON'
              end
              object rb3: TsRadioButton
                Left = 16
                Top = 144
                Width = 60
                Height = 19
                Caption = 'Border'
                TabOrder = 6
                OnClick = rb3Click
                SkinData.SkinSection = 'RADIOBUTTON'
              end
            end
          end
          object sTabSheet2: TsTabSheet
            Caption = 'Templates'
            SkinData.CustomColor = False
            SkinData.CustomFont = False
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object spnl6: TsPanel
              Left = 0
              Top = 0
              Width = 193
              Height = 257
              Caption = 'spnl6'
              ShowCaption = False
              TabOrder = 0
              SkinData.SkinSection = 'GROUPBOX'
              object spnl7: TsPanel
                Left = 0
                Top = 0
                Width = 193
                Height = 25
                Caption = 'Templates'
                TabOrder = 0
                SkinData.SkinSection = 'PANEL'
              end
              object sListBox2: TsListBox
                Left = 0
                Top = 24
                Width = 193
                Height = 185
                Color = 16776441
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                OnClick = sListBox2Click
                BoundLabel.Indent = 0
                BoundLabel.Font.Charset = DEFAULT_CHARSET
                BoundLabel.Font.Color = clWindowText
                BoundLabel.Font.Height = -11
                BoundLabel.Font.Name = 'Tahoma'
                BoundLabel.Font.Style = []
                BoundLabel.Layout = sclLeft
                BoundLabel.MaxWidth = 0
                BoundLabel.UseSkinColor = True
                SkinData.SkinSection = 'EDIT'
              end
              object btn6: TsButton
                Left = 0
                Top = 208
                Width = 193
                Height = 25
                Caption = 'Add Template'
                TabOrder = 2
                OnClick = btn6Click
                SkinData.SkinSection = 'TOOLBAR'
              end
              object btn7: TsButton
                Left = 0
                Top = 232
                Width = 193
                Height = 25
                Caption = 'Remove Template'
                TabOrder = 3
                OnClick = btn7Click
                SkinData.SkinSection = 'TOOLBAR'
              end
            end
            object spnl8: TsPanel
              Left = 0
              Top = 264
              Width = 193
              Height = 185
              Caption = 'spnl8'
              ShowCaption = False
              TabOrder = 1
              Visible = False
              SkinData.SkinSection = 'GROUPBOX'
              object spnl9: TsPanel
                Left = 0
                Top = 0
                Width = 193
                Height = 25
                Caption = 'Properties'
                TabOrder = 0
                SkinData.SkinSection = 'PANEL'
              end
              object spnl10: TsPanel
                Left = 8
                Top = 72
                Width = 177
                Height = 65
                Caption = 'spnl10'
                ShowCaption = False
                TabOrder = 1
                SkinData.SkinSection = 'GROUPBOX'
                object cbb3: TsComboBox
                  Left = 8
                  Top = 32
                  Width = 161
                  Height = 22
                  Alignment = taLeftJustify
                  BoundLabel.Active = True
                  BoundLabel.Caption = 'Element'
                  BoundLabel.Indent = 0
                  BoundLabel.Font.Charset = DEFAULT_CHARSET
                  BoundLabel.Font.Color = clBlack
                  BoundLabel.Font.Height = -11
                  BoundLabel.Font.Name = 'Tahoma'
                  BoundLabel.Font.Style = []
                  BoundLabel.Layout = sclTopLeft
                  BoundLabel.MaxWidth = 0
                  BoundLabel.UseSkinColor = True
                  SkinData.SkinSection = 'COMBOBOX'
                  Style = csDropDownList
                  Color = 16776441
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clBlack
                  Font.Height = -11
                  Font.Name = 'Tahoma'
                  Font.Style = []
                  ItemIndex = -1
                  ParentFont = False
                  TabOrder = 0
                  OnChange = cbb3Change
                end
              end
              object cbb2: TsComboBox
                Left = 16
                Top = 64
                Width = 145
                Height = 22
                Alignment = taLeftJustify
                BoundLabel.Indent = 0
                BoundLabel.Font.Charset = DEFAULT_CHARSET
                BoundLabel.Font.Color = clWindowText
                BoundLabel.Font.Height = -11
                BoundLabel.Font.Name = 'Tahoma'
                BoundLabel.Font.Style = []
                BoundLabel.Layout = sclLeft
                BoundLabel.MaxWidth = 0
                BoundLabel.UseSkinColor = True
                SkinData.SkinSection = 'COMBOBOX'
                Style = csDropDownList
                Color = 16776441
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ItemIndex = 0
                ParentFont = False
                TabOrder = 2
                Text = 'Layer 0'
                OnChange = cbb2Change
                Items.Strings = (
                  'Layer 0'
                  'Layer 1'
                  'Layer 2'
                  'Alpha')
              end
              object sEdit2: TsEdit
                Left = 8
                Top = 40
                Width = 177
                Height = 19
                BevelInner = bvLowered
                BevelOuter = bvNone
                BevelWidth = 2
                Color = 16776441
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 3
                Text = 'sEdit2'
                OnChange = sEdit2Change
                SkinData.SkinSection = 'EDIT'
                BoundLabel.Active = True
                BoundLabel.Caption = 'Name'
                BoundLabel.Indent = 0
                BoundLabel.Font.Charset = DEFAULT_CHARSET
                BoundLabel.Font.Color = clBlack
                BoundLabel.Font.Height = -11
                BoundLabel.Font.Name = 'Tahoma'
                BoundLabel.Font.Style = []
                BoundLabel.Layout = sclTopLeft
                BoundLabel.MaxWidth = 0
                BoundLabel.UseSkinColor = True
              end
              object edt1: TsSpinEdit
                Left = 8
                Top = 152
                Width = 65
                Height = 19
                BevelInner = bvSpace
                BevelKind = bkSoft
                BiDiMode = bdRightToLeft
                BorderStyle = bsNone
                Color = 16776441
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentBiDiMode = False
                ParentFont = False
                TabOrder = 4
                OnChange = edt1Change
                SkinData.SkinSection = 'EDIT'
                BoundLabel.Active = True
                BoundLabel.Caption = 'Border Size'
                BoundLabel.Indent = 0
                BoundLabel.Font.Charset = DEFAULT_CHARSET
                BoundLabel.Font.Color = clBlack
                BoundLabel.Font.Height = -11
                BoundLabel.Font.Name = 'Tahoma'
                BoundLabel.Font.Style = []
                BoundLabel.Layout = sclTopLeft
                BoundLabel.MaxWidth = 0
                BoundLabel.UseSkinColor = True
                MaxValue = 0
                MinValue = 0
                Value = 0
              end
            end
          end
        end
      end
    end
    object sPanel2: TsPanel
      Left = 0
      Top = 486
      Width = 893
      Height = 40
      Align = alClient
      TabOrder = 1
      SkinData.SkinSection = 'PANEL'
      object sButton2: TsButton
        Left = 817
        Top = 1
        Width = 75
        Height = 38
        Align = alRight
        Caption = 'Textures'
        TabOrder = 0
        OnClick = sButton2Click
        SkinData.SkinSection = 'BUTTON'
      end
      object btn2: TsButton
        Left = 1
        Top = 1
        Width = 75
        Height = 38
        Align = alLeft
        Caption = 'Save'
        TabOrder = 1
        OnClick = btn2Click
        SkinData.SkinSection = 'BUTTON'
      end
      object btn3: TsButton
        Left = 76
        Top = 1
        Width = 75
        Height = 38
        Align = alLeft
        Caption = 'Load'
        TabOrder = 2
        OnClick = btn3Click
        SkinData.SkinSection = 'BUTTON'
      end
      object btn4: TsButton
        Left = 151
        Top = 1
        Width = 75
        Height = 38
        Align = alLeft
        Caption = 'Export'
        TabOrder = 3
        OnClick = btn4Click
        SkinData.SkinSection = 'BUTTON'
      end
    end
  end
  object sSkinManager1: TsSkinManager
    InternalSkins = <
      item
        Name = 'Office12Style (internal)'
        Shadow1Color = clBlack
        Shadow1Offset = 0
        Shadow1Transparency = 0
        Data = {
          41537A66030000000B0000004F7074696F6E732E64617444370000789CED1BDB
          6EDB38F63D40FE21F981AE781129C13010D9566CA1B2654872DAEE201E14458B
          0D5A4C8BE9EE6BBE7DCE2145EA46C972E3A2B3D8451085A48E489E0BCF95F96D
          9D668B284D76F7D9E3F5D5227BBBDCC4CBD7F16A7EE7112EEF3C8FC283843EB4
          38BF63779E023AEC6A302F0C0C18170DB0659666F99C30C9B9E7F3EBAB4D5656
          437E48424F06D757DBA828E37C9194DB683FD79D578BEDFEFA2A8F5649D65882
          87D5125E281B4B28B0D65E3CCF00F2A001586CA3346D6E19C00825F0F0F0C1E8
          1DB160CDF9F46B05486803303A941BC0E4F5D72FDFDE7FFEF0AFA79BE255F1EA
          FAEA21CE8B24DBCDE52B0F805671B1CC937D8923DBE226FBF4E9E9C3C71B426F
          BE3C7DFE78F3FDF3D31F379FBEFE79F3FDE39F4F5FFFF3FDE6FDB76F5F9E3EBC
          FFF7D3D73FBEC35CC52159CD19BDBE5AC31CC9365AC7B81F8264961CB7822804
          52EF67912CB3DD32CD0A0514E25601758FFAC03722FC8A6F08B489D33DC2D010
          1FAC015DC36CA3B7761A04D1D3B441929D9DC5A30DE01A6497E55B3B0DC2E869
          0C4C999469BC38946556CF847BD6F066CFF7D9AE921AFC6499C6F7E59C082925
          25BE92C4751EBDD33C0D14D71597505AFDB062FD265A656F48767F5FC4F0AD1D
          58A4877CCE6C572F227CC904959524D8B935F39518289219315864F92ACE2BA9
          E6F0C3A440EC8B7DB48C71AD45A276AC489227EB8D6A5E5FFD566659AA717F6C
          1E0C8B180C9579B48379F278B77C37F7EBF304305C70908B16004181531D2052
          5414F3621FC72BBD845AB1D8A74909E7EB7110EC7785B302DEC6BB03A884F8B1
          BF13B5507F44ADD099EAD105E89C6F8D0719F6B40762C21F249292F8461FBE73
          40C16817D040ADA2329AEFFE11B53EAD07F711526367215A7DCD5740A2783D3F
          A25239A2323CA2DE387AD4C39647BB0F762447FF48FA8C32FC8E90F8AD9D101E
          324F7032239C4A4244380372CCF0A71E300D35DE27558FF1FB6817A75D142881
          8D83CA8516E3473C8E155AED0739E22F515B5E250FC94AC94B7B2A5041473C01
          F0A04817C92D0DA49D4AE23438D980ACAC9228CDD61D59BC0785D125108333E5
          737F86D4A47E20673E9D11A48FE95B081EE817F60BD3F0705CADAACC45F3D84D
          946C650F40D79CF551BC4ACA1EF13C8F21F5FD236A2A142A3ECE875A9404E7A8
          BBCAA88C8BB9E6D01E041FF46C99836CC1B6B26D7F399457CD2BC5B5D1E56857
          8EECEC5DA6F8848A30405A87DC0B4930F32BA1357D25D784CA99A884B9EA2A44
          4840678A2D5EDDB78D4ACADBD829CDF92B91234EE4981EEF2327AC8876913392
          D8D0950E197999BA99604E86A5160641D495A632CE9A0CE12881B3760F54DAAD
          E35DB448C12896F9213663C90E54E7033A9173E67507B34339F7ED208C45E810
          15737FFA4E8B1435D17213ED40B5A10AA2EAEC086CF9D80A9124A4FB40CE2B0A
          E9EF15798D2F8B2E22A581F613B59FA0801E0C94F221A4F26C102A305E4B9F8C
          C9F235F882FFC48FA4F228958B865F124FBB090802AE6109209413E30D2967C7
          631A04656209D2982ECADDC06186398FE8181DF1536805A153DE9991F775FA6E
          BF31E8F84223A13C2B68857AD9B6E8575C98C109088444B364AC911D308DEA9C
          764D6B3D8590428682D553D801D3E84FD1B269CEC12699D0B3BA2C9190379A48
          2AF209699B48CEED8D508E0C508E9CA45CC0A5ACA7B003A6D19F6290486ED5F9
          222AA993A1A9441A549A260A644014C8EC02347552630A95CA6C7F6149928646
          FAE05DE8B89D85E0B88451EAB5CEA61E308DA619069DDB23CE59FE24B5B684C3
          F242B808A10DA531A5C69FB0A6D5D85ED1F1338CB136FE8435DEA6314638F4EC
          B2ED22D39E5D8B7F5A49C20310D5FA5A69FFA60B81FE1D468FD99BFB6C79282A
          83E874F7F85477AFE5DEADF3ECB0D77BBB2CF1DD1E791EADB5DD6F85DC1D4671
          503F0C24C8F7434FD0DAF1337D1BD0F83503557CE3CB405226D037D21F547DDB
          1863D4CBD06FA1EB88405F126C9E1F5AA2DA012E1F8AA17050C9BB3D0864E020
          D0CE41300DDE3D21A6319DBEE72ABE2A5F00AE9DC367615E35979AB5F26559F7
          416126E60E16DD7CE8B8EC46987848250DC386FED07DC2591880733C63D578D5
          05B90D0246782D9655DF364E382980FB9817EFA9032F10CB504929EF3E980A94
          89535B832AF69491842317F066CCA4FAC4A73CA002CD45B579D35710BE9C71D6
          F8C2977563A2EB556D84055412FCBA6A706335CC0B9F809009500B81D1C255DF
          34381D7831269593E30D47BEA54A432D92F58B19D3D4F9269FE658314AF79BC8
          1DF38778806888712AAD2CC140B4D289F96D48D41D9AC0351FF4B172218CD50F
          A51506E305E846C58396ED6945800C940FE3F2A539A5097AD889D76532813F90
          6AFB21E50E94BB80CBD412031278E2EF66C6A6E80DEB2C18F56C083E64FC817C
          6FE28555A81393C7B5BF4220E2063BA373208D61816C0D06344594AB4AC4493D
          2182713358AF277CE993807776E1394FF28F71B53B743EABA6A9FEAA50373345
          BC9A8576C0344E2604264B81834998197EFC1FAF84B992844A8784365DD8974E
          AAA4934F1222A73C9CA1B48D9A0D4588B978417D2F90A44ED8577D0B6112F6F6
          0BD318CA90FC94A95BD8E671018E45BC5A64591A47BBCAE9A804D0AD262837F4
          661C35847AA8047FE3419587ABD47AAD10020CC83C879AE81D208F4A56DB6B1E
          DAF4B235E40A028205F3C27EA11A15D693AC6D8FEA7505868040067E4DF5AA3F
          B9023392A838A5F3EA644D94934D2F6EB27E253AB802FC61130098BE85B001EB
          399E683FE0B8D87A4334E98451CE84F768F9CE928BFE9F5CE7908B3C4C2017B3
          07ADBD7D2EF50BC7F627252887D62343EB85E3EB4D23D70F14876BE9FA99E49A
          40990B4C7D41CA0CE5085444F07B9ABDE97B992A1A3CE965DA6456ED42F672A9
          237E9CAB8A6E0E60AF8AEE0DE870CF6543DA85DA4BA52A7B81EE2FCD13FFBD22
          AF36C59D45B0732BE3A7A94C9C54F63B85F10695C90095D5E11BACC463013A5A
          38AB310C1D291F6548A00C49B720E9ACCDB44371C9E0C6D2ACAEA8B82F368C24
          124B4C74B94A9A2A018BA85322C651F78FD3B02403589EAC4CD6588EDE7038B7
          1007A80F15BD15B77D9B87BE00DF2DDE963436223503A631C2778BB7264D83EF
          CEECDA38F2EE2AAD66B7623C676730FE5CDCC900EE83D2D0C7BD179054B84FAB
          53EBAB179B8B9660412361188D7136C1875F95F0CE12CD099A7F803E4251012C
          B4D77476555F65F62523336EC6759708C12478ADB61861FAB6D13152D57A9487
          019F621459C7284EAD1975F8F470E1EB0458200F55A95CEA0BD7E7F3691213C8
          0013C81013C800135C7715BA4C309EF9499BD96082FB457D696C1D1DD6A3794A
          4A4E7990C47957F467398BE8AAE4D93A8F8BA22F34DADF55FB0EB8ADC3A83456
          D068119338194F3B1995E3039F080DCD9E4D970A6084F4CC8E4DD7FC3D714B60
          1B253B2C71F6716036F726089E8340677B3A8F66E494808BAB6F564FBB5684F7
          3EEBE8253C11D6E0BDD3B740A9B396D04ADBC89BA9138D6803CCC36E9C097AE3
          A93EA3E7F98C59D9678CC69FF1D6DD33F19E8933B5B349762EEBF763B71C4480
          A2D5AC026EFAC7E656BFC237F80F27B7EC96DC7AB7C04FDA7BB82B802E8DD3AE
          CA1A3ADAAAAC69B8B578B796EA0FD55259389E2271A4CFABABDC2EC51D549425
          81A271AD3BC8C9129E0D6EAB23DEB7DFE71EF133C4F545E77CD0C3BDD4AC5D05
          A28AC38DBB46FFFD05E2224EE3255EE4EDCB948F7937A9EE7DE0ED272907B161
          2EA63BF33EBFEC8287B904CD28173468D2DAFE5FDBF5D55FE076E6290E000000
          427574746F6E487567652E626D7036DC0000789CEDDD7B5053D91D07F01F1B41
          9E12C4A0184505051151510C11050CB0A211165FB0A009B03E5130111F808822
          6A40081144FE68DD425D743BBB4E9DB1D6BAD5BA5D3AB35A1FD351579D5AA7EA
          6A1FBA76AD76D6C738BE9A078F049293D7BD71CE9DDFD7C93022F339F7FECEC9
          0DE03DBF246788FE0EFA88FA018CD57E1CE906D0A1FDE8069E867FB80110E863
          78683FA9CF3BED9F173F3D851FBEBF070FEEDC85BF5DF80BFCF4DFC7F0CD17BF
          86EFFEF42DFCE3C64D38D1FA19FCEE1707E0F8A7BF84DFEE6F83633F6F85AFDA
          DAE1DCF1AFE0FC899370F1F77F803BDF5D83FFFDE74778FCC34368AB5641BBAA
          0E3EAFD3C0170D4D70B8711F7CB9A719BEFED59770B2FD73E8387C04AE9F3D07
          37CE5F84BF9EBB008FFE7D1F7EF3B34FE1E46787E0D2D7DFC0A53F76C0F5337F
          866BDF9E851FFFF92FF8FEDA7578F7EE1DBC7DFB16DEBC7903AF5FBF8657AF5E
          C1CB972FE1C58B17F0FCF97378F6EC193C7DFA149E3C79028F1F3F86478F1EC1
          C3870FE1C1830770FFFE7DB877EF1EDCBD7B17EEDCB903B76FDF865BB76EC1CD
          9B37E1EAD5AB70E5CA15B87CF9325CBA74092E5EBC08172E5C80F3E7CFC3D9B3
          67E1CC9933D0D1D101A74F9F8653A74EC1891327E0F8F1E370ECD831387AF428
          1C3972040E1F3E0C870E1D8283070F427B7B3B1C387000DADADAA0B5B515F6EF
          DF0F2D2D2DD0DCDC0CF5F5F5505757072A950AAAABABA1B2B2122A2A2AA0BCBC
          1CCACACAA0A4A404944A251417174351511114161642414101E4E7E7835C2E07
          994C06B9B9B99093930359595990919101E9E9E920954A21353515241209CC9C
          391392929220212101E2E3E3412C16435C5C1C88442288898981499326417474
          348C1F3F1EA2A2A260DCB8711019190911111110161606A1A1A110121202C386
          0D03A15008C1C1C110141404028100F87C3EF8FBFB839F9F1FF8F8F880B7B737
          78797981A7A72778787880BBBBBB7E4DE94CF38F167D2D6C7B5832480F9A7D7B
          6C47C6306F8CB398163B6259E1A66F8FEDC81854FB8EE0F60C41B7EFB86EDB08
          74FBCEE9D647A0DC779E270F40B7DFEB0B559B37DA94CD2A1B4770B1DFDCA869
          505B4D83A6B1D936BFD7B12B0BE6CDB129F30A94BDCEE1FDFBCD8D0DF5353BAB
          B7565ACDD6EA9D35F50DA635B2C6ABD66B8F3D2D31C6D244992626314D7B0EEB
          55560670A1DFA4DEBD6B5BC586758AA2D58556B2BA48B16E43C5B65DBBD54D64
          DFE4E8336C3DF69E73C8303983F7E937A96BB6979514AF5A2A5F9C939D6525D9
          398BE54B571597946DAF31AE9065DE91A3377306EFCDDFABAEADDAA4589197BB
          30533A3B3525D94A5252674B3317E6E6AD506CAAAA55EFB5E4777FBE6A994347
          DF7906CBAA2C0CE0327F4FDD8E52C5F2258BD2D32449F162D1D4582B992A12C7
          2749D2D2172D59AE28DD51B7C7BCDF73F89F7CE8E0D1EBCFE0C34FCC9F80CB7C
          4DED16E54AD902694A823876F2C4E828EB6654F4C4C9B1E28414E902D94AE596
          5A0DC95797E63973F8BA13C82B55BF3F7F9F4655BE363F7B6EF27451CC041B6A
          D395A80931A2E9C973B3F3D796AB34FBFAFA5D252BFDD8D1B5DF7D02891F97F6
          9D0057F91A55D91AF9FC5933ECAB8E2EBA0ACD98355FBEA64CA5E9E3772FFEBC
          44E78E5E97C4BCAADE03B8CAD796A75096299936C5DEEAE8123561CA3449A6AC
          D04C8118B93674C5E81AE1627F4F6DF91AD94733E3268D774C1E3F296EE647B2
          35E5B57B4CFDCEBFA9963171F8BA1358A63219C045FEDEBA2D6BE5998E97C750
          A04CF9DA2D757BCDF9EB3318397CED0964AC7F1FBE7A87327FBEC489F2E80B24
          999FAFDC61F212D035BD050C5C1C0C492C309E6017F94DB5A52BB3674D73A63C
          BA024D9B95BDB2B4B6A9AFCFD8F4F69E6017F9EA2A856CEE8C294E95475BA029
          33E6CA1455EA3E3E83D36B34C1AEF39B6A362D5F902C9AE02C3D4194BC60F9A6
          9AA66E9FF9E9359D6017F9EAED8A25D2E9310EBCB09B262A66BA748962BBBA97
          CFE8F49A4CB06BFCE6DD652B16A538BF7C740B2865D18AB2DDCDA6FEE6790C4E
          AF7682E76D76ADDFB8AB242F3DC1F9E5A35B4009E97925BB1A4DFD8D739C978D
          3367A36BFD866DC5B9696206968F760189D3728BB73574FA9DCF33651A13744F
          D2942D2661DBAFAF58B55012CBC0F2D12EA058C9C25515F5263CC39707E36F51
          5CE237D76C589A993499197B7252E6D20D3526BF9066F8F2607481708DDFB873
          9D5C1A3F91197B62BC54BE6E67A3B1CFF4E5A1E702E11A5F53AD583C5B1CCD0C
          1D2D9EBD5851ADE1527D1AB616E5A48A18B9FC682F40A2D49CA2AD0D5CAA8FBA
          727576CA54A6ECA929D9AB2BD5DCAA4F6156722C53766C725621D6C772B03EE4
          607DC8C1FA9083F52107EB430ED6871CAC0F39581F72B03EE4607DC8C1FA9083
          F52107EB430ED6871CAC0F39581F72B03EE4607DC8C1FA9083F52107EB430ED6
          871CAC0F39581F72B03EE4607DC8C1FA9083F52107EB430ED6871C2ED607EF8F
          22F9787F1DD967FBFE4CBCBFD72866EEEFC5FBC38D62E6FE70DC5FD013D3FD05
          B83FA5774CF7A7E0FEA65E31BFBF09F7C775C5FCFE38DC5FD9993EFB2B717FAE
          49FAECCFC5FDDDC6B1B8BF1BFB03E862AE3F00F697E88ED9FE12D89FA42B16FA
          93607F1B432CF5B7C1FE48BA10FA23617F2D727F2DECCF66A53F1BF6F7B3D2DF
          0FFB4392FB43627F512BFD45B13FAD95FEB4D8DFD84A7F63EC8F6DCDC7FEEA56
          7CDAFBE7B3EF33300091A7DE7776046B3AFDBE3323D8A2D3EF3B3884CD38077C
          7BC7B0D7A6C7078BE1F5F7B731FD7996156EFABCFE1E3E5E01E13626C0CBC7C3
          BE31A8F6797CAD1D11EEE966EB4A76F30C8FD08EC1B77108BAFD48A117DF0EDB
          680CBE973092EB3E4FE81B603FDE354480AFD0CA1C50EE8F19E0B0DE39C28031
          DCF57961A123BBF5C081A387FB0E0D12101324081AEA3B7CF4C0C0EE11468686
          599C02CAFD113DC51F14221C4C964D33581832A8670A4670D28FF0EE2A3E8F1F
          6C0F6E48309FD73505DE111CF4BBF9510143ECD7751912308A3000E57E37DFCF
          CF315D17BF7E1607A0DC1FD1C9BBF31DD775E1BB770E3082533E6F80811FEBE1
          1C2F10788C350C3080C7253F2C40CF7F30CC595E2018F6817E8080300EF96342
          3DF5D56780D70EA09F01CFD0319CF13B57A7BBD38BD3100FF75E2B94765F6858
          9D4E5EDA7AC237AC502147FC485FFDEAECC7142F10E85F263D7D23B9E11BCA3F
          CA89EF1B7AC76F94F10450EEF3BCF4E50F608E170802F413E0C5E382CFE7EBCA
          CF73F09B72F319A2FB59C68DCFE782EF13CEE8C5CD10FD252EDC8703BE61790E
          72E0275E528207752D50DAFDFEFAAB5B08B3BC4010A2BFC2F5A7DFF788D02D25
          21D3BE50A74678D0EFEB9FBE8176FDB6CD960C0EEC7C0253EE1B9EBE0399E605
          8281862730EDBEE1E93B9A797FB4E1094CBBEFAF7F751CCEBC3F5CFF0AE9CF0D
          DF9779DF974BFE50E6FDA15CF2ADFC379A2309E292CF3C2F10A08F3EFAE8A38F
          3EFAE8A38F3EFAE8A38F3EFAE8A38F3EFAE8A38F3EFAE8A38F3EFAE8A38F3EFA
          E8A38F3EFAAEF169BF7F896D9FF6FBDFD8F669BF7F926D9FF6FB6FD9F369BF7F
          9B6D9FF6FBFFD9F669DF3FC2BA4FFBFE23B67DDAF7AFB1EDD3BEFF916D9FF6FD
          B3ACFBB4EFBF66DBA77DFF3EDB3EEDFD1F58F769EF1FC2B64F7BFF19D67DDAFB
          17B1EDD3DEFF8A759FF6FE69ACFBB4F7DF63DBA7BD7F23EB3EEDFD3F59F769EF
          1FCBBA4F7BFF61D67DDAFB57B3EED3DEFF9C759FF6FEF9ECFBB4BFFF02EB3EED
          EFDFC1BE4FFBFBBFB0EF03E5EF1FE402DF3006BDEF3FC59CFF7FB93FD1E60A00
          00004D61737465722E626D709A1D0100789CED9D094014579AC73F1ADCCD26BB
          1BB2C98C9B6414A69325C2CE66B24E1267C78999C16334C66442565113A52511
          8130232451D35E51079368346D4C942828A2080154E49CE041505031A8A01CE2
          81683C51C45B231ECC7BAFEEABBBABE9EA6EA0FE4D77BFF755FDE9AA1F8FD7AF
          DE57D51D3C6C790F2FC07AD107A0177A7E1B552BD1B3173C40E2EF3EEE058F3E
          04E40ED4AA446D6D6D806FF7EFDF87BBE8E6856E4F3EF124B4DD6F8343BBCAA1
          AE6C071CADD8038D9555B07F6B31546EDE02D525DBC080BC13870E859A6DDBE0
          5079391CDD83D6A9AA8223E8B9F9F871B880EE2DA74EC1FDBB7751B911B6E4E6
          C0D6827CB8D1721136E5E5C0D50B4D70EED81178676830A4CC8883826516D8BA
          663934EEDF03276AF743D1C6F570E77233DCBB7115AE349D819B975B60E1F8E1
          10F7C600081FD417E68E1F094BE2DE81FD695F43E1B22FE0FC0F5B61474E069C
          AEDC09B5A55BA06E47315CFFF108DC38D500895F2E84C6EA7D70BBF92C34FF78
          0C66BCF52A247E180E8BFFF236CC9F100A9F458C804FD1EFFE38ECCF30F1CD41
          3061D81F21EC4FBF8759A63760E594F190FC5104A4CE88819AAC65B07EE14CD8
          B86836E47E3517F2977E06794B3E85A2158BA0227B0D34EFDB0665EB52A0346B
          15ECCA49877D9B72606FD146284C4D826B470FC09903BBA17A5B11ECFFFEEF50
          5BB6056A4A37437DF93638B8AB04EA776F87433F94C2EDA61FE1E8BE7268BD70
          0A1A0FEC8163FB2BA0B5F90CDC3A7F0A4ED657C3A94335B0B5300FEEDDBC0617
          CF9C84E6D33F427AD8EF203FEE75D835EF3DF8266638BCD2E759581A3B16C60D
          7909BE79DF043188D917EF8D86C9A386C27BA83CFED53F20EE2FC3DAD9B170F9
          FB34B8B1633D64CD9F0AEB3E9F0677F614C0C62FE7C0BDFD5B10D7CFA170F902
          F82EE90B5836670A6C4A5E0C47B76E802DAB97C0F6CC95B02D630594AD5F0D27
          CAB7C0CE8D6BA1A2701DFC509005BBF33361CF771BE078C536A8DC9207FB36E7
          425571019CA9A9809B270F43DDCE62B48FBBE0C8DE9D7078CF0E3877B81AEDE3
          69B879EE04DCB9D404678E1E84D347EAE0D4E15A388FFE5E4D271AE0EEF5CB70
          E9EC49F8BEA810EEDDBA0E37AF5E86EBA84D5CBB74116EDFBA095F4F781DBE8C
          780D964487C0CA4963E1999E8F435361221C2C4C856325D970AFB112F1DF05E7
          EAD173433D14E56E40EC4EC0DCA91FC0BCD9D3E1EAC5F370B676371CAF2C83A2
          F56BA1E9F861F8E9C63598F6D777E18B4FE7A0FD5D0AD7CE35C2D5B3C7E03AFA
          DB5C457FA72F177C069BB256C0B5B347E0CEED5B70F74E2BB4DEFE094ABE2F86
          AD68FD1B67EBA1BE7207DC3A5707757BB743C9BAAF60DDAA45505FB5130EA1B6
          5057550E55E55BA1EDA71668440C8E1EAC84130D7570A8760F9C3ED90017CEA3
          767FF3067C9398007B6B6AA0F9CA15C858BF1EAEDDB80125E8FF71C3F61D507D
          EC04ECBAD206B9A7DB20F5681B24D6B5C1C7A56D70FE4233C46C6C833713EEC3
          BD7BF7A0BEBE1EEAEAEAA0A4A40466CF9E0D53A74E052FAB827648D1EDE565E0
          CBDE9716D93A8034640BF27F3814B6630B2466B1AD03C816DBF6705734DADE02
          19B3C8D601A4215B65A7ED2D9033EB6C75B6AC74B6DA4967AB9D74B6DA4967AB
          9D74B6DA4967AB9DDCC2963C5ADD029DADC36CEF81CE5623B6F7FEE99ECE562B
          B608ADCE56EF6FDB21B7B0BDC543ABB3752EDBFBF775B6DAB1BD658BADDCFCED
          F32A354B419D9A2D7EB0B105727987E7A365F5BC527CD61859756EB6F66C81CC
          AF7B3E5A6E45CC563E3E6B8C5CBCD3B3C5ED96BECBAE6230EC10CB60D0D9DAC3
          F6E2452FF62EBB8A142D82ABB3B58FAD81BDCBAE228376C70E9DAD4DB8F6E479
          75B6D6C9DA3A3FC15A7FABB3B5D16A15179057B6DADFEA6CADCA60B8242FFA95
          F5FED67119588A4289C33A5BF552402B84DBA9FBDB9E56F59475FD684D4A6811
          5CB6D4B9FBDB9EB2BB46ABE753FBACE8A9A7BEB1223BD976E63EC116DB324539
          832D96CED6A96CBDF8E729EB6C75B6EAE516B68273C075B63A5BF5D2D96A279D
          AD76D2D96A270F65AB9077E858B9484F652B9B2FEB603974F7B395D58810B116
          190C217F1BF69A505F686377923C93ED88904CB14216FD795684580A74DA6977
          963C936D88BFBFA893F4F70F19922CD56B5AD89D250F659BB64CACB4909753A4
          5260DB3EBBB3E4A16C6346891513627E572A05B6EDB33B4B1ECA366009D6D225
          9C0242CCE3A42270C256AD5A6542CF26D4124D8C9D04918F04193B09A2671264
          EC9AC953D986867E154ADFBF0AC5372B6C933FFAE82304D794829E53183B150C
          A5828C9D0A8EA3825D956DE4F0E123468C183E823C0EC795C810731FA9E8763B
          68D0A055A614F448B75B64170419BB20C8D8359387B28D5A24565488D9241505
          C7B46AE0C081F84EA1A5ECBC20CFCE0539BB56D290AD729ED7CBD6E7A3C8B395
          397EA5E198564DC7A2D1D27636C8B7B341BE5D2369C7D6CAF909B6D9C6468915
          1B624E948A81635A3560C000062D63A783423B1D14DAB591866C95CFAB713A5B
          D3AAFE586CBB8DA5D0D241BE9D0D7670B68E4B9EED48A998FE76DAB469F8CEF4
          B7B1042D1BE4ECBC2067D74A5AB2352ADC6C2B244E72EC1F17624E908A1E274C
          993205BDFBA3477A9C80EC822063170419BB66D2906DB45178DB4CDF0C86CBB6
          A6C1424A2487A72521E6AFA5A2C6B766B3998C6FD1730A63A782295490B153C1
          AFA92063EF6A6C332447091921E6A552291C9765480EC1283B09A2671264EC9A
          E9AF36D83A7ECE922DB66FBEF9A693D83ADBEE2CB5EB5C3BABB2C9F6252B6C33
          C3C4CA0C31874AA5C0B67D76672AD81EE1157D9022232325EFE142A135F08AED
          6AB76969A2C1565A5A88F94BA9FEA685DD790A0EEE1BF06D71C6BAD2ECDC9A56
          B16A72B34BD765147F1BD017D3F5F189C9F4F3EBE1ADA0D69ADADCF2EC8D3DFC
          FC3263305BD16D337DB3A3BF1D11922E56C8A24FCCC3C59A354F0BBBD3141C6E
          AF820D3E3171F62AC6C7CA6B62B6CC89FBF2924D787D621669C81BDAD89D2555
          6C33ED669BE97341E1B0ECD2852B0EB2ED68B9C8E0C8F0706B430556E1E191C1
          E3FCE2E2EC5A392ECE6FDC79255DBC60936DE7C8450607D8CD362038BCD46EB6
          A5E1E71F57D0F94B36D9768E5CA4C7B0C5E92B0E0E9D4CC441493211073B442E
          3238DD6EB6E9C12B73EC669BB3522DDB8103796CE964220E4A938928D8217291
          6E637BA10E8DDA9A94D8D2B9481C94E4227150902F1BC7EB425732764190B5F3
          839AE7CB54B1EDA3826D1F1B6C2F5E686E6E6EE1B115F409742E120725B9481C
          14B08DF888530463170419BB20D885D80ADA2D9D8BC441492E120705B9C88841
          6C631C14C1D80541C62E086A9E8B54C5364C05DB3007D8B270997C19098A7391
          24C8CF45460C8CA07D7409DB5191CEB49112654745DA48953C88AD49055B9315
          B6F885A57D020F2ECB9683CBE522A9202F1719319D653B9D633B9D653B3D82B1
          A322FD4BA85227657BA50E1D46F0DECB0470D99C0E1514E5CBA82097F08A18C0
          B2A54AD88E8AF4EF2025CA8E8AF4EFA04A1EC4768C0AB663148FCBCE5F91DF14
          1E5C2E5FC6C2E5E72239B814DBFE6C2FDA9F63CB0FB26CF9C10ECCD6684B02AC
          18EABBECFB19618B83345C2E998883F81105D96462C4344E1C5B7E90B10B829A
          E72255B11DAB82ED5863AB375569351A5B784566FA52C096B4581E5B9C8B24C1
          0812E47291249840826C2E32620AA708D6CE0F32764150F35CA4B66CC9C78678
          63A0541C17E9771831DB81E4808B618B7391BC20978BE405D95C64046FDE3082
          B10B828C5D10D43C17A9255BFCD11688276AAC0C5A5C94658B610D1CC8F5B719
          E418810D72092F5E904D78498FCB32248760945D10D43C5FA68AED32156C9719
          BDC98C8197778BD1487709B8C8B06DE1B365E0B2E3848C711C5CEA288C4926B2
          41CFCF45AA62BB5C05DBE5C6D66EE44B12A9FE96A2DBAAD02750EF66DC512F9D
          4CA483C264221DEC00B9480DD9B674F3229F7843FA5B02D75BA9BF95C0E914B9
          48556C47AA603B12BD97614137DCC9D2C94AA5FE56ACCE918B54C576940AB6A3
          8CD450CBBB1BEE646929F6B7523A9D2017A9659FD04ADF5047C094F9456B9BD5
          397291DA8D13541C9749D9748A5CA466E3DBF66C5527C9457A26DB4E715DA487
          B2ED14D7456A370FA66223DEED8B1519F90E13C0C94436C8CF45B2417EBE6C25
          EE3BF1BF781F9E9D0BF2EC5C9067D74C1EC1B6EF204A914C002713D9203F17C9
          06657391C93C3B17E4D9B9A0C7B155957750B1117DE9F9178E6DE4F0E16C909F
          8B6483925C246A8C839279762EC8B373419E5D337906DBE99438B6518B16B141
          7E2E920DCAE422930716D3DD022F17490545B9482AE859B94855795E151B21ED
          6F111C36C8BF2E920DF2AF8BA43390C9D3877EFC713263A773915450948BA482
          9E958B54757E427BB64ADDB57B1103A8FFF401C5C9539319BB20C8CB457241CF
          CA97792ADBFEB49293A7736CF9412E17C90B7A165B55E783B567ABD45D17C966
          18939307736CF941492E12073D2B17E93AB6AAAE8B44A3D6E2D78B916292FF98
          CCD80541C62E086A9F8BD4ECFCDBF66C95BAEB229122FE402B9967E7823C3B17
          D43E17E9996CD526BCD8046E32CFCE0579762EA87DBE4CB3EB1DDAB355AAD9AE
          8CA067B6FAF0EC5C9067E7829AB33504AFD4E83A9DF66C5427B92E32B88FEBAF
          2FB3A9CE918BD4F0BAC876A873E422B134BA9EB73DEA14B9C80EA17F13CAD7AA
          3E5790BB77C28DA2F393C2275AFF2A94EF0DBE7C7D6FF2E5FBF97FCBAA0BB3A5
          3EC69A79DA4CC42E7CE4DF911E7904FD90922DB672A3C2AECCD66ABBFD17A17C
          AF23DD4037F2A0B3B525ABEDF63F881E78E001EA91B065A5B3B5296BEDF6C147
          7FF6B3071F7DF0D1471F7C103F89D9FEC497CE5622ABEDF6B1C71E7B08DDE031
          AC871EF2BD76FD1AFA79183D22316CC16EB693DDB07F6E95B5761B40040054C1
          973045553E5B54B593EDB3CF3EEB8E1D749FACB6DBF4F480F47472167AFAB701
          E9DFFAD26829B888ED6D8216C1BDFDD36D9B6CFF07CB2DFBE891F247A2BFE113
          177DAF5DBDF630554525D26E1FA66447BBED47C9159B2DB71D2A24FB0DA78E6C
          C7AF98C22F6516767FA27BF7EE3FEF4EF4C4CFBBFB5EBDC6BBF9FADEE6CB06DB
          5F317288963AC9D19145261F046813CB41B6744B9A2EC7F63F85F2BDCA973AB6
          AE9497F49BA3C14B2178472CD9EF3C77942DE9037F2DCB3648A8F6B0756DBBF5
          18B6E8DDFB39095B325EF2134A6FB7EA84D94E1E4AD8FE929181192FF5104A67
          AB4EFC764B9FF36560C74B3ADBF6B2C54F549FF06B2C03375E12B3B53E7FABB3
          1549384E78EEB9E70C8EBEEFE86CC5128D6F873ABE4736D92AA5FB1C91D52DF1
          10B64F8BAE8FEAAD2EA8B3B5226374D96FF822C88C81A9BFE58B0A2615FD2F5F
          12B83A5B31DB5E32708D1B64E01A57DB80ABB315B3CDEAB5E62D4665345C63F5
          86C0DF337A8F866B4C5B9DB490950C5C9BB9C82EC8367A02A3F12BFE9FC0C56C
          B358F5FA3F02D7E8BF3AE96D465B16BF2A816B3387DE05D956F0455A2E622B08
          92968BD80A82A4E5AA7AAD2EC8966BB7A8E19649DB6D56AF5469BB7D617191CC
          DB99CE56C036306B2DF76DDB6B105BD2DF564F0CE484D8E2E0AED55B933821B6
          2AD17645B6BDD6B28A1E5F46860435D51B26B20ACC4A25C14CFFD55B5925BD50
          A4166D9764CB6AEDDA35D450B6BA7A03A78981547097FF6A4E4949AAD17639B6
          4E3C2ED3D98A44FF1E2A79242C7889934AD2022F24937512AFABB3E5159C2C17
          B2954A552ED2156C59C6B205D1EAC26D11FD3A2F97B26D9F641B86B3D9DA2AF0
          908AFFD63CA46CC4656CE5BEC0ACDD419D2D8556A631F66C675067AB9EADB4BB
          75225B2B9DAA15B61EDDDF7A0E5B655963EB80BA1A5B2B83583B86BE1E3ABEF5
          10B6A3BD5B5A6EDDBAE53D9A57205F58E03DDA309A9241B9305A5A505EE2BAE3
          320F62DBDCDC4CB1A50B345B54C21AAD5CC04571417181780D17B0F5C6376FEA
          E3957B2A04DDD36E118BA696166F61012F610A48D20225B48EB4E072B618A44C
          BB1504DDD46E5104495060173DCE20151718B8A3A50537B0E5588A89B9A8DD22
          A80C40295B85451D89ADB8DD0A829AB355689C569A749764DBC017AA4FE25430
          49502FC075AB9DAAB5AE9841EAD9FDAD93D9D6D45423E522D510B673E6CC993D
          FB3B22C2F60356EF17E0C114D954D9C180CD2184A78F139CCFF6DCB9D8B3674F
          9F3C75BC9AB09DBB69133E59A3A8A888B02DF8B0B0B0704AA1392F2FEF7D5C57
          1EC4DA31F4ED62E3DB86DA73CF506A6CA2D9D2758AEDA40F0B7F4729EF834956
          B7D0E9EA846C178AD84ED2D93ACCF6ACCE5673B6C714D89A75B68EB2CD3D4DB3
          3C44B1FD5467EB3CB627456C8B446C0B58B6052E00CA5327607B8A6679A45CC4
          7636C536CF99EDB637770E887C9111DEEC6E921DB147EA5DDAF5B7671A1B1B8F
          1D3A72E4442ECD9611C3362F2F3F1F3F38816D6FE36F188CBD8DBFE58AD2F3F0
          00EEAA16DE55475C5AE5221B6AC831596E53137D5CF629D2DCB973F1E119C596
          123E306B375B8496BE4402A3A52F8CC068A5E78FE20CC43FCB6CB4359166E480
          4BAB1CBA95F984499364EAED464BC1256829B804AD04AE4BD9767CF5369695D1
          9748F436A6A6A652707B1B51F72385ABB35525E39AF12B5654904B24C8A9A615
          E4C20863D20B8B1757BC2A3E63DFD56CA31698679A1744B1AF2FAE1B0CC3CC0B
          F81B285D01D505FBCB37D833DD5850C04E37DA3D3F595050909F87EAC6B5D16B
          C657AC206C2706066655F4A2D82625BD50B1D8BD6CFD43670E4BCC0E1F3633D4
          9FBC3AAAF78F9F17DF9FA91B0C833FB124589E1EC66C1E3684977306838136B0
          3B307888C562F984A16D6DBA110F6D45D38D94A1D69A613653FBEEBB7CCC766D
          F4848AF1F81209E38689886DD67B98EDEAAD495B2AB62C742BDB507362797975
          4D6BB83994BC7A28028515DF9FAA1B86C45B2C090909964FE8CDC3866A9EC1C0
          1A0C9CC1C2196C4D374E124D37F20DB9B28639CCF223B86EEC85D94E788B63FB
          7B9AEDDB156FBB956DD4CCC49C6C0AEE4CFC5F1E351391316598102B5237A07A
          424A6C246245B55C6440EBE7B20603CF60E01B2C16AAE536D49E2B2FCFEE511A
          14C44E37D275762AB7AE6EFD2FBEDFB9933E8C20867C91219F3320B674FD9067
          B35D600EF2A3E10EC37DE48221090919B1B1B119F3E6F5277D667F0BAA0705C5
          262450ED7081D92F679DA59FA5076D302003A923838136E07A36D37011AAECEC
          1E3DFC822AD92931BACE4E2DAC5FFF8B279FE1B3CDCECFCFE71936D175962DB3
          1CF7119ECBD61C5AC2C00DC7EDCC3C1F353A2CD3BC785C1F1C6F492941D5A014
          0BD570CDA1417E967EFD665A780652E719507D3A7A8C1F46A13A9B4350EE61D9
          F6A0EAEC74E39304ED7E96EDD902C42E9F67C8A7EA0C5B76B967B39D1993C9C0
          CDC6FFD3334DA4D992868BEB93E3E3C322FD71DD62194CFEE58B4B825E79E5E5
          7EFDAAABCB1903A9B3064BD89497719D3620547E7E7E4188253BDD48D7D976FB
          CC3308AD806D811F82C719E83A6D98CB2CF7F0FED61C5ACCC04DA49AA12992DF
          6E67F48F9F1F1655EC1F9B42D59121A364CCB057FA59CAABC31903A933ED7648
          FC7C0B626D610C0DB9A78310C9CABD07D8E946BACEB09DB2136B3FCB36F77401
          5A01C1630D749D61CB2EF76CB60BCCE90C5CBABF3599487F6BA1FBDB19F1F14B
          C3C2A2222D7477BAC09C999139E615CBBAF272BABF3599489D5901199EB64CB1
          B08686DC93EB2A2BF7EC3D50CF4E3752F5C30CDB3C4276FF51962D65E0D816D1
          75862DB3DCC3DB6DD4CC501A6E223D4E586A324566A42458E871C2E4FEF1F1F3
          975A2CF3E2FF44B60E193233C81F23911E277006839CA121F7D45E44F240FD61
          76BA91AEB3536208ECC1FD078FD253B9AC215F64C8A70D73D9E59ECD160D5729
          B889CCF8D68C58A1F12C3BBE45BD02D10C7AF39081C0650C06D6609035D835DD
          48CD3732FDAD0DC35CB4FC1859EEE16CD16196393426D4CC3B2E1B32DF347F08
          77D8F57704ABFF0CF6137989A19833A0964B19D81D982C30A89E6EB469983B97
          AECDA6D822B815D16BCA305B04B722303015B345702B92928A3C7D3E61F2E419
          FC0DB43D9F3063F260B6AC7ABA51A5C188AFFF478A2E3332570007A61A8DFED4
          75BF49456E65DBC1650C0C241FB3D02B1AB1255317D51B028DC65D99BBFCFDFD
          8B572719F92BEB6C5589BB7AFA69F9227F657C95844A4AD4C5120EB8DC05C45D
          C28D501D25A02F4451EF72F7BEBA5A78AFD5666C2956EA5DEEDE57570B735247
          896989EA5DEEDE57576BB7431AE590DCBDAFAED6EE74E5FFEE6E0A0BD2778FCC
          51743DACE4CA19E9EE7D75B576A7A9EF39D3772F5766ABA82EC8B6D811B6CB74
          B676C85A9FA0A434C7D82E73F7BEBA5ADB1C609BBE6DAC236CC7BA7B5F5D2D9D
          AD76D2D96A27C7D88E7184ED1877EFABABB5DD11B6DB75B6F6C831B62647D89A
          DCBDAFAE96CE563BFDE008DB1F5E7484ED8BEEDE5757CB31B6618EB00D73F7BE
          BA5A3A5BEDE418DB3E8EB0EDE3EE7D75B5AA1C615BA5B3B5475501EA29055485
          97AA779586BB7B5F5D2D9DAD76AA8A544F29B26A9C9F7A97DF3877EFABCB356A
          A55A4A2B47197C32E3D4BAE2327DDCBDAB2ED7A23EE1EA2885F75964F089898B
          55E78A8D8BE97A6C0D9651917D0362BE4D2BCE7C2936A8746376796E6D4D2BAB
          9AEADCF2EC8DA541B12F65FAA7FD2526A06FDF511683C1C72726137F7B6D8F5C
          2F858F216DADA9A9CDCDCDCECEDE98D3A314AD9919E3D305D91A0CE48AADCF6C
          8BAC370F3B7C90FE2B122BCAA6C86A91D8F10F35D7B6FA}
      end>
    MenuSupport.IcoLineSkin = 'ICOLINE'
    MenuSupport.ExtraLineFont.Charset = DEFAULT_CHARSET
    MenuSupport.ExtraLineFont.Color = clWindowText
    MenuSupport.ExtraLineFont.Height = -11
    MenuSupport.ExtraLineFont.Name = 'Tahoma'
    MenuSupport.ExtraLineFont.Style = []
    SkinDirectory = 'c:\Skins'
    SkinName = 'Office12Style (internal)'
    SkinInfo = '7'
    ThirdParty.ThirdEdits = ' '
    ThirdParty.ThirdButtons = 'TButton'
    ThirdParty.ThirdBitBtns = ' '
    ThirdParty.ThirdCheckBoxes = ' '
    ThirdParty.ThirdGroupBoxes = ' '
    ThirdParty.ThirdListViews = ' '
    ThirdParty.ThirdPanels = ' '
    ThirdParty.ThirdGrids = ' '
    ThirdParty.ThirdTreeViews = ' '
    ThirdParty.ThirdComboBoxes = ' '
    ThirdParty.ThirdWWEdits = ' '
    ThirdParty.ThirdVirtualTrees = ' '
    ThirdParty.ThirdGridEh = ' '
    ThirdParty.ThirdPageControl = ' '
    ThirdParty.ThirdTabControl = ' '
    ThirdParty.ThirdToolBar = ' '
    ThirdParty.ThirdStatusBar = ' '
    ThirdParty.ThirdSpeedButton = ' '
  end
  object sd1: TsSaveDialog
    DefaultExt = 'gui'
    Left = 32
  end
  object od1: TsOpenDialog
    DefaultExt = 'gui'
    Left = 64
  end
  object sd2: TsSaveDialog
    DefaultExt = 'g2gui'
    Left = 96
  end
end
