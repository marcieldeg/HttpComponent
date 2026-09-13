object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 438
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 635
    Height = 438
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'GET'
      object Button1: TButton
        Left = 3
        Top = 30
        Width = 75
        Height = 25
        Caption = 'GET'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Edit1: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 621
        Height = 21
        Align = alTop
        TabOrder = 1
        Text = 
          'https://raw.githubusercontent.com/mxw/grmr/master/src/finaltests' +
          '/bible.txt'
        TextHint = 'Enter a URL'
      end
      object ProgressBar1: TProgressBar
        Left = 3
        Top = 61
        Width = 621
        Height = 17
        TabOrder = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'POST'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Edit2: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 621
        Height = 21
        Align = alTop
        TabOrder = 0
        Text = 'https://httpbin.org/post'
        TextHint = 'Enter a URL'
      end
      object Button2: TButton
        Left = 3
        Top = 172
        Width = 75
        Height = 25
        Caption = 'POST'
        TabOrder = 1
        OnClick = Button2Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'PUT'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Edit3: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 621
        Height = 21
        Align = alTop
        TabOrder = 0
        Text = 'https://httpbin.org/put'
        TextHint = 'Enter a URL'
      end
      object Button3: TButton
        Left = 3
        Top = 172
        Width = 75
        Height = 25
        Caption = 'PUT'
        TabOrder = 1
        OnClick = Button3Click
      end
      object Memo1: TMemo
        Left = 3
        Top = 30
        Width = 185
        Height = 89
        Lines.Strings = (
          'Memo1')
        TabOrder = 2
      end
    end
    object DELETE: TTabSheet
      Caption = 'DELETE'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Edit4: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 621
        Height = 21
        Align = alTop
        TabOrder = 0
        Text = 'https://httpbin.org/delete'
        TextHint = 'Enter a URL'
      end
      object Button5: TButton
        Left = 3
        Top = 30
        Width = 75
        Height = 25
        Caption = 'DELETE'
        TabOrder = 1
        OnClick = Button4Click
      end
    end
  end
  object HttpRequest1: THttpRequest
    SecurityOptions = [soSecure, soSsl, soSsl3, soPct, soPct4, soIetfssl4, so40bit, so128bit, so56bit, soUnknownbit, soIgnoreRevication, soIgnoreUnknownCA, soIgnoreWrongUsage, soIgnoreCertCNInvalid, soIgnoreCertDateInvalid, soIgnoreRedirectHttps, soIgnoreRedirectHttp]
    UserAgent = 'Mozilla/5.0 (compatible, HttpComponent)'
    OnBeforeRequest = HttpRequest1BeforeRequest
    OnAfterResponse = HttpRequest1AfterResponse
    OnProgress = HttpRequest1Progress
    Left = 256
    Top = 176
  end
end
