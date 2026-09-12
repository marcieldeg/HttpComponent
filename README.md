# HttpComponent
Delphi component wrapper for WinInet library. Written in Delphi 2010.

## Packages

The component is split into two packages:

- **`HttpClientRT.dpk`** — runtime package (`THttpRequest` and all supporting classes). Only requires `rtl`; this is the package your compiled applications need at runtime.
- **`HttpClient.dpk`** — design-time package (property editor and visual headers editor shown in the IDE). Requires `HttpClientRT` plus the IDE design packages (`DesignIDE`, `dbrtl`, `dsnap`, `vcldb`).

Install both packages in the IDE (`HttpClientRT.dpk` first) to get the component on the palette with its custom Headers editor; deploy only `HttpClientRT` with your compiled application.

## Session and keep-alive

`THttpRequest` keeps a WinINet session (`InternetOpen`) and, while the host/port/credentials stay the same, a connection (`InternetConnect`) for the life of the component. Requests set `INTERNET_FLAG_KEEP_CONNECTION` so HTTP/1.1 connections can be reused. Changing `UserAgent` reopens the session; changing `Username` or `Password`, or calling a different host/port, reconnects.

After each completed request, `LastUrl` is the URL WinINet reports for the handle (the target after redirects when `AutoRedirect` is True). If that query fails, `LastUrl` is the URL you passed in.

The component is **not thread-safe**. Use one `THttpRequest` per thread, or serialize all calls from a single thread (typically the VCL main thread).

## How to use

### GET
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  if HttpRequest1.Get('https://httpbin.org/get') then
    ShowMessage(HttpRequest1.Response.ContentAsString)
  else
    ShowMessage('ERROR ' + IntToStr(HttpRequest1.Response.StatusCode));
end;
```
### POST
Posting a simple text:
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  if HttpRequest1.Post('https://httpbin.org/post', 'testing a POST') then
    ShowMessage(HttpRequest1.Response.ContentAsString)
  else
    ShowMessage('ERROR ' + IntToStr(HttpRequest1.Response.StatusCode));
end;
```
Posting a file with a Multipart form:
```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  Body: TMultipartFormBody;
begin
  Body := TMultipartFormBody.Create;
  Body.ReleaseAfterSend := True;
  Body.Add('code', '2');
  Body.AddFromFile('image', 'C:\Users\User\Desktop\image.png');

  HttpRequest1.Post('https://httpbin.org/post', Body);
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;
```
Posting a url encoded form:
```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  Body: TUrlEncodedFormBody;
begin
  Body := TUrlEncodedFormBody.Create;
  Body.ReleaseAfterSend := True;
  Body.Add('code', '1');
  Body.Add('name', 'John');

  HttpRequest1.Post('https://httpbin.org/post', Body);
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;
```

## Body ownership

`TBody` instances (`TStringBody`, `TBytesBody`, `TUrlEncodedFormBody`, `TMultipartFormBody`) are plain objects, not owned by `THttpRequest` by default. You choose one of two strategies:

1. **Caller-managed** (default): create the body, pass it to `Get`/`Post`/`Put`/... and free it yourself after the call, typically with `try/finally`.
2. **Component-managed**: set `Body.ReleaseAfterSend := True` before calling the request method. `THttpRequest` will free the body object automatically right after the request has been sent. Do not access or free the body object again after this point.

Never mix both strategies for the same instance, that leads to a double free.

## Security options

`TSecurityOptions` (`soIgnoreUnknownCA`, `soIgnoreCertCNInvalid`, `soIgnoreCertDateInvalid`, `soIgnoreRedirectHttps`, `soIgnoreRedirectHttp`, ...) instruct WinINet to bypass SSL/TLS certificate validation. **They are dangerous and disabled by default.** Only enable them for controlled scenarios (e.g. talking to a known internal server with a self-signed certificate); enabling them against untrusted networks exposes the connection to man-in-the-middle attacks.

Similarly, embedding credentials directly in the URL (`https://user:pass@host/...`) sends them in clear text and risks leaking them into logs. Prefer the `Username`/`Password` properties of `THttpRequest` instead.


