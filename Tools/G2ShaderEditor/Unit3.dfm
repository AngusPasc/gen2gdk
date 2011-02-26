object Form3: TForm3
  Left = 0
  Top = 0
  AutoSize = True
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Compiler'
  ClientHeight = 137
  ClientWidth = 233
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object grp1: TGroupBox
    Left = 0
    Top = 0
    Width = 233
    Height = 137
    Caption = 'Compiler options'
    TabOrder = 0
    object lbl1: TLabel
      Left = 8
      Top = 64
      Width = 84
      Height = 13
      Caption = 'Optimization level'
    end
    object lbl2: TLabel
      Left = 120
      Top = 64
      Width = 58
      Height = 13
      Caption = 'Flow control'
    end
    object cbb1: TComboBox
      Left = 8
      Top = 80
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemIndex = 4
      TabOrder = 0
      Text = 'Level 3'
      Items.Strings = (
        'Default'
        'Level 0'
        'Level 1'
        'Level 2'
        'Level 3')
    end
    object chk1: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Debug'
      TabOrder = 1
    end
    object chk2: TCheckBox
      Left = 8
      Top = 17
      Width = 103
      Height = 17
      Caption = 'Skip Optimization'
      TabOrder = 2
    end
    object chk4: TCheckBox
      Left = 128
      Top = 16
      Width = 97
      Height = 17
      Caption = 'No preshader'
      TabOrder = 3
    end
    object chk3: TCheckBox
      Left = 128
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Partial precision'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object btn1: TButton
      Left = 8
      Top = 104
      Width = 217
      Height = 25
      Caption = 'OK'
      TabOrder = 5
      OnClick = btn1Click
    end
    object cbb2: TComboBox
      Left = 120
      Top = 80
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 6
      Text = 'Avoided'
      Items.Strings = (
        'Default'
        'Prefered'
        'Avoided')
    end
  end
end
