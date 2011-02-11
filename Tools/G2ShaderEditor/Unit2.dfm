object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Compile Result'
  ClientHeight = 569
  ClientWidth = 816
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 816
    Height = 569
    ActivePage = TabSheet2
    Align = alClient
    MultiLine = True
    TabOrder = 0
    TabWidth = 128
    object TabSheet1: TTabSheet
      Caption = 'Binary'
      object MemoBinary: TMemo
        Left = 3
        Top = 3
        Width = 802
        Height = 502
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'MemoBinary')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Button1: TButton
        Left = 3
        Top = 511
        Width = 118
        Height = 25
        Caption = 'Save'
        TabOrder = 1
        OnClick = Button1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Inline'
      ImageIndex = 1
      object MemoInline: TMemo
        Left = 3
        Top = 3
        Width = 802
        Height = 504
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'MemoInline')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Button2: TButton
        Left = 3
        Top = 513
        Width = 118
        Height = 25
        Caption = 'Copy to Clipboard'
        TabOrder = 1
        OnClick = Button2Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Assembly'
      ImageIndex = 2
      object MemoAssembly: TMemo
        Left = 3
        Top = 3
        Width = 802
        Height = 502
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'MemoAssembly')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object sdbinary: TSaveDialog
    DefaultExt = '.cfx'
    Filter = 'Compiled Effect (*.cfx)|*.cfx|All Files (*.*)|*.*'
    FilterIndex = 0
    Left = 128
    Top = 528
  end
end
