object FHeadersEditor: TFHeadersEditor
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Headers'
  ClientHeight = 245
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 466
    Height = 216
    Align = alClient
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'NAME'
        PickList.Strings = (
          'A-IM'
          'Accept'
          'Accept-Charset'
          'Accept-Encoding'
          'Accept-Language'
          'Accept-Datetime'
          'Access-Control-Request-Method'
          'Access-Control-Request-Headers'
          'Authorization'
          'Cache-Control'
          'Connection'
          'Content-Length'
          'Content-Type'
          'Cookie'
          'Date'
          'Expect'
          'Forwarded'
          'From'
          'Host'
          'If-Match'
          'If-Modified-Since'
          'If-None-Match'
          'If-Range'
          'If-Unmodified-Since'
          'Max-Forwards'
          'Origin'
          'Pragma'
          'Proxy-Authorization'
          'Range'
          'Referer'
          'TE'
          'User-Agent'
          'Upgrade'
          'Via'
          'Warning')
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'VALUE'
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 216
    Width = 466
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object DBNavigator1: TDBNavigator
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 54
      Height = 23
      Margins.Right = 0
      DataSource = DataSource
      VisibleButtons = [nbInsert, nbDelete]
      Align = alLeft
      ConfirmDelete = False
      TabOrder = 0
    end
    object BCancel: TButton
      AlignWithMargins = True
      Left = 388
      Top = 3
      Width = 75
      Height = 23
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object BOk: TButton
      AlignWithMargins = True
      Left = 307
      Top = 3
      Width = 75
      Height = 23
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 57
      Top = 3
      Width = 40
      Height = 23
      Margins.Left = 0
      Align = alLeft
      Caption = 'Clear'
      TabOrder = 3
      TabStop = False
      OnClick = Button1Click
    end
  end
  object ClientDataSet: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    Left = 280
    Top = 48
    Data = {
      4D0000009619E0BD0100000018000000020000000000030000004D00044E414D
      4501004900000001000557494454480200020032000556414C55450200490000
      00010005574944544802000200A00F0000}
    object ClientDataSetNAME: TStringField
      DisplayLabel = 'Header Name'
      DisplayWidth = 20
      FieldName = 'NAME'
      Size = 50
    end
    object ClientDataSetVALUE: TStringField
      DisplayLabel = 'Header Value'
      DisplayWidth = 50
      FieldName = 'VALUE'
      Size = 4000
    end
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 144
    Top = 48
  end
end
