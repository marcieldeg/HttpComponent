# HttpComponent
Delphi component wrapper for WinInet library. Written in Delphi 2010.

**Full documentation:** [DOCUMENTATION.md](DOCUMENTATION.md)

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
2. **Component-managed**: set `Body.ReleaseAfterSend := True` before calling the request method. `THttpRequest` takes ownership of the body and frees it automatically after the request has been sent — **including when the send fails** (exception raised). After the call (success or failure), do not access or free the body object again.

Never mix both strategies for the same instance, that leads to a double free.

## Retry, abort e eventos

### MaxRetries (retry automático)

`MaxRetries` (default `0`) controla quantas vezes um request é repetido automaticamente. A política é conservadora:

- **Somente métodos idempotentes** (`GET`, `HEAD`, `OPTIONS`, `DELETE`, `TRACE`, `PUT`) são repetidos — `POST`/`PATCH` nunca são reenviados automaticamente.
- **Somente códigos retryáveis**: `408` (Request Timeout) e todos os `5xx`.
- **`Retry-After` é honrado**: se o servidor enviar `Retry-After: <segundos>` ou uma data HTTP, o componente espera esse intervalo antes da nova tentativa.
- **Não retry se o body já foi liberado** (`ReleaseAfterSend := True`): a tentativa seguinte não pode reenviar o mesmo corpo, então o retry é desabilitado.

### Abort

`Abort` cancela o request em andamento de qualquer thread (o componente não é thread-safe, mas `Abort` pode ser chamado de outro thread para interromper uma operação bloqueante). Ele:

1. Seta a flag interna `Aborted` (propriedade `Aborted` de leitura).
2. Fecha o handle WinINet ativo (`InternetCloseHandle`), o que faz as chamadas bloqueantes (`HttpSendRequest`, `InternetReadFile`) retornarem imediatamente.
3. O `Request` lança uma exceção `'Request aborted by the user'`.

`Abort` também é chamado automaticamente no `Destroy` do componente. Durante o download, a flag é verificada a cada chunk (em `ReadResponseBody`), garantindo resposta rápida ao cancelamento.

### Eventos

- **`OnBeforeRequest(Sender, AMethod, AUrl)`** — disparado antes de cada request (inclusive antes de cada tentativa de retry), permitindo logging, alteração de estado ou rejeição.
- **`OnAfterResponse(Sender)`** — disparado após a conclusão (sucesso ou erro) de cada request, permitindo logging e medição de tempo.

## Cookies

Cookie management is done exclusively by the component when `UseCookies` is `True`: the WinINet built-in cookie jar is always disabled (`INTERNET_FLAG_NO_COOKIES`), so cookies never mix with the IE/Windows cache. `Set-Cookie` response headers are parsed into `TCookie` objects and sent back as a single `Cookie:` header for matching requests.

### Atributos suportados pelo parser de `Set-Cookie`

| Atributo   | Comportamento                                                                                     |
|------------|---------------------------------------------------------------------------------------------------|
| `Domain`   | Define o escopo de domínio; cookies de domínio são enviados para subdomínios                      |
| `Path`     | Define o escopo de caminho                                                                        |
| `Expires`  | Data de expiração (RFC 1123), convertida para hora local antes da comparação com `Now`            |
| `Max-Age`  | Segundos de vida a partir da criação; **tem precedência sobre `Expires`** (RFC 6265 §5.3)          |
| `Secure`   | Cookie é enviado **somente via HTTPS**                                                            |
| `HttpOnly` | Armazenado e exposto (propriedade `HttpOnly`); não impede envio em `Cookie:` (limitação HTTP)      |
| `SameSite` | `None`, `Lax` ou `Strict` (aproximação — não aplica política de cross-site)                       |

`Max-Age=0` (ou negativo) remove o cookie imediatamente. Cookies `Secure` recebidos em HTTPS não são enviados em requisições HTTP subsequentes.

### Compartilhando o cookie jar entre instâncias

`TCookies.Assign` copia o conteúdo de um `TCookies` para outro (deep copy). Para compartilhar a sessão entre dois `THttpRequest`:

```pascal
HttpRequest2.Cookies.Assign(HttpRequest1.Cookies);
```

## Security options

`TSecurityOptions` (`soIgnoreUnknownCA`, `soIgnoreCertCNInvalid`, `soIgnoreCertDateInvalid`, `soIgnoreRedirectHttps`, `soIgnoreRedirectHttp`, ...) instruct WinINet to bypass SSL/TLS certificate validation. **They are dangerous and disabled by default.** Only enable them for controlled scenarios (e.g. talking to a known internal server with a self-signed certificate); enabling them against untrusted networks exposes the connection to man-in-the-middle attacks.

Similarly, embedding credentials directly in the URL (`https://user:pass@host/...`) sends them in clear text and risks leaking them into logs. Prefer the `Username`/`Password` properties of `THttpRequest` instead.


