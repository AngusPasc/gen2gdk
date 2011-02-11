object Form1: TForm1
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 73
  ClientWidth = 405
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
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 405
    Height = 73
    Style = bsRaised
  end
  object Label1: TLabel
    Left = 8
    Top = 5
    Width = 103
    Height = 13
    Caption = 'Shader Compiler v1.0'
  end
  object Label2: TLabel
    Left = 8
    Top = 46
    Width = 81
    Height = 13
    Caption = 'Current Shader: '
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 24
    Width = 389
    Height = 16
    TabOrder = 0
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 368
    Top = 40
  end
end
