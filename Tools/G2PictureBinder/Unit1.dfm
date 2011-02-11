object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Gen2'
  ClientHeight = 403
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object od1: TOpenDialog
    Filter = 
      'All (*.*)|*.*|png (*.png)|*.png|tga (*.tga)|*.tga|bmp (*.bmp)|*.' +
      'bmp|jpeg (*.jpg)|*.jpg, *.jpeg|dds (*.dds)|*.dds|ppm (*.ppm)|*.p' +
      'pm|dib (*.dib)|*.dib|hdr (*.hdr)|*.hdr|pfm (*.pfm)|*.pfm'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
  end
  object sd1: TSaveDialog
    Filter = 
      'png (*.png)|*.png|tga (*.tga)|*.tga|bmp (*.bmp)|*.bmp|jpeg (*.jp' +
      'g)|*.jpg, *.jpeg|dds (*.dds)|*.dds|ppm (*.ppm)|*.ppm|dib (*.dib)' +
      '|*.dib|hdr (*.hdr)|*.hdr|pfm (*.pfm)|*.pfm'
    Left = 32
  end
end
