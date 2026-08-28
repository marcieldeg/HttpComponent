unit UfrmHeadersEditor;

interface

uses
  Forms, ExtCtrls, Classes, Controls, DB, DBClient, Grids, DBGrids, DBCtrls, HttpClasses, StdCtrls;

type
  TFHeadersEditor = class(TForm)
    DBGrid1: TDBGrid;
    ClientDataSet: TClientDataSet;
    DataSource: TDataSource;
    ClientDataSetVALUE: TStringField;
    ClientDataSetNAME: TStringField;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    BCancel: TButton;
    BOk: TButton;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure ClearHeaders;
  public
    procedure LoadHeaders(AHeaders: THeaders);
    procedure SaveHeaders(AHeaders: THeaders);
  end;

var
  FHeadersEditor: TFHeadersEditor;

implementation

{$R *.dfm}
{TFHeadersEditor}

procedure TFHeadersEditor.Button1Click(Sender: TObject);
begin
  ClearHeaders;
end;

procedure TFHeadersEditor.ClearHeaders;
begin
  ClientDataSet.Close;
  ClientDataSet.CreateDataSet;
end;

procedure TFHeadersEditor.LoadHeaders(AHeaders: THeaders);
var
  HeaderIndex: Integer;
begin
  ClearHeaders;
  for HeaderIndex := 0 to AHeaders.Count - 1 do
    ClientDataSet.InsertRecord([AHeaders.Names[HeaderIndex], AHeaders.ValueFromIndex[HeaderIndex]]);
end;

procedure TFHeadersEditor.SaveHeaders(AHeaders: THeaders);
var
  HeaderName: String;
  HeaderValue: String;
begin
  AHeaders.Clear;
  ClientDataSet.First;
  while not ClientDataSet.Eof do
  begin
    HeaderName := ClientDataSetNAME.AsString;
    HeaderValue := ClientDataSetVALUE.AsString;
    if HeaderName <> '' then
      AHeaders.AddHeader(HeaderName, HeaderValue);
    ClientDataSet.Next;
  end;
end;

end.
