unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HttpClasses, ComCtrls;

type
  TForm1 = class(TForm)
    HttpRequest1: THttpRequest;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    Edit1: TEdit;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    DELETE: TTabSheet;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure HttpRequest1Progress(Sender: TObject; ABytesRead, ABytesTotal: Integer);
    procedure HttpRequest1BeforeRequest(Sender: TObject; AMethod, AUrl: string);
    procedure HttpRequest1AfterResponse(Sender: TObject);
  private
    {Private declarations}
  public
    {Public declarations}
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if HttpRequest1.Get(Edit1.Text) then
  begin
    ShowMessage('ContentLenght = ' + IntToStr(HttpRequest1.Response.ContentLength));
    if HttpRequest1.UseCookies and (HttpRequest1.Cookies.Count > 0) then
      ShowMessage(HttpRequest1.Cookies[0].ToRequestValue);
    HttpRequest1.Response.SaveToFile('C:\Users\Marciel\Desktop\teste.txt');
  end
  else
    ShowMessage('ERROR ' + IntToStr(Integer(HttpRequest1.Response.StatusCode)));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Body: TMultipartFormBody;
begin
  Body := TMultipartFormBody.Create;
  Body.ReleaseAfterSend := True;
  Body.Add('codigo', '2');
  Body.Add('acentos', 'уrзгo');
  Body.AddFromFile('imagemTeste', 'C:\Users\Marciel\Desktop\Aaa.bmp');
  HttpRequest1.Post(Edit2.Text, Body);
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  HttpRequest1.Put(Edit3.Text, 'teste PUT');
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  HttpRequest1.DELETE(Edit4.Text, 'teste DELETE');
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Body: TUrlEncodedFormBody;
begin
  Body := TUrlEncodedFormBody.Create;
  Body.ReleaseAfterSend := True;
  Body.Add('codigo', '2');
  Body.Add('acentos', 'уrзгo');

  HttpRequest1.Post('https://httpbin.org/post', Body);
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;

procedure TForm1.HttpRequest1AfterResponse(Sender: TObject);
begin
  ShowMessage('AfterResponse');
end;

procedure TForm1.HttpRequest1BeforeRequest(Sender: TObject; AMethod, AUrl: string);
begin
  ShowMessage(Format('BeforeRequest: %s - %s', [AMethod, AUrl]));
end;

procedure TForm1.HttpRequest1Progress(Sender: TObject; ABytesRead, ABytesTotal: Integer);
begin
  ProgressBar1.Max := ABytesTotal div 1000;
  ProgressBar1.Position := ABytesRead div 1000;
  Application.ProcessMessages;
end;

end.
