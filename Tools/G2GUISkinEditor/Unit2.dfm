object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Textures'
  ClientHeight = 290
  ClientWidth = 458
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
  object sGauge1: TsGauge
    Left = 8
    Top = 264
    Width = 441
    Height = 17
    SkinData.SkinSection = 'GAUGE'
    ForeColor = clBlack
    Progress = 0
    Suffix = '%'
  end
  object sListBox1: TsListBox
    Left = 8
    Top = 8
    Width = 441
    Height = 217
    Color = 16776441
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MultiSelect = True
    ParentFont = False
    TabOrder = 0
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
  object sButton1: TsButton
    Left = 8
    Top = 232
    Width = 217
    Height = 25
    Caption = 'Add Textures'
    TabOrder = 1
    OnClick = sButton1Click
    SkinData.SkinSection = 'BUTTON'
  end
  object sButton2: TsButton
    Left = 232
    Top = 232
    Width = 217
    Height = 25
    Caption = 'RemoveTextures'
    TabOrder = 2
    OnClick = sButton2Click
    SkinData.SkinSection = 'BUTTON'
  end
  object opd1: TsOpenPictureDialog
    Filter = 
      'All (*.png;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.wmf)|*.png;*.' +
      'png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.wmf|Portable network graph' +
      'ics (AlphaControls) (*.png)|*.png|Portable Network Graphics (*.p' +
      'ng)|*.png|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)' +
      '|*.jpeg|Bitmaps (*.bmp)|*.bmp|TIFF Images (*.tif)|*.tif|TIFF Ima' +
      'ges (*.tiff)|*.tiff|Metafiles (*.wmf)|*.wmf'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 8
  end
end
