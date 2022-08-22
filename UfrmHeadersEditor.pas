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
    {Private declarations}
  public
    {Public declarations}
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
  ClientDataSet.Close;
  ClientDataSet.CreateDataSet;
end;

procedure TFHeadersEditor.LoadHeaders(AHeaders: THeaders);
var
  i: Integer;
begin
  for i := 0 to AHeaders.Count - 1 do
    ClientDataSet.InsertRecord([AHeaders.Names[i], AHeaders.ValueFromIndex[i]]);
end;

procedure TFHeadersEditor.SaveHeaders(AHeaders: THeaders);
begin
  AHeaders.Clear;
  ClientDataSet.First;
  while not ClientDataSet.Eof do
  begin
    AHeaders.AddHeader(ClientDataSet.FieldByName('NAME').Text, ClientDataSet.FieldByName('VALUE').Text);
    ClientDataSet.Next;
  end;
end;

end.
