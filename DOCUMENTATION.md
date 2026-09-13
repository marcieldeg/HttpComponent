# HttpComponent — User Documentation

A Delphi VCL component that wraps WinINet (`wininet.dll`) for HTTP/HTTPS requests. Written in **Delphi 2010** (Win32/x86).

---

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Published Properties](#published-properties)
4. [Public Methods](#public-methods)
5. [Bodies (Request Body)](#bodies-request-body)
6. [Response](#response)
7. [Cookies](#cookies)
8. [Timeouts](#timeouts)
9. [Proxy](#proxy)
10. [Authentication](#authentication)
11. [Redirect](#redirect)
12. [Cache](#cache)
13. [Abort (Cancellation)](#abort-cancellation)
14. [Retry (Automatic Retries)](#retry-automatic-retries)
15. [Events](#events)
16. [Security](#security)
17. [Complete Examples](#complete-examples)
18. [Thread-Safety](#thread-safety)
19. [Compatibility](#compatibility)

---

## Installation

The component is split into two packages:

| Package | Type | Dependencies |
|---------|------|--------------|
| `HttpClientRT.dpk` | Runtime | `rtl` only |
| `HttpClient.dpk` | Design-time | `rtl`, `HttpClientRT`, `DesignIDE`, `dbrtl`, `dsnap`, `vcldb` |

**To install in the IDE:**
1. Open `HttpClientRT.dpk` and compile/install
2. Open `HttpClient.dpk` and compile/install
3. The `THttpRequest` component will appear on the `HttpClient` palette

**To distribute with your application:** include only `HttpClientRT` (runtime).

---

## Quick Start

### Simple GET

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  if HttpRequest1.Get('https://httpbin.org/get') then
    ShowMessage(HttpRequest1.Response.ContentAsString)
  else
    ShowMessage('ERROR ' + IntToStr(HttpRequest1.Response.StatusCode));
end;
```

### POST with text

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  HttpRequest1.Post('https://httpbin.org/post', 'testing POST');
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;
```

### POST with URL-encoded form

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  Body: TUrlEncodedFormBody;
begin
  Body := TUrlEncodedFormBody.Create;
  Body.ReleaseAfterSend := True;
  Body.Add('username', 'john');
  Body.Add('password', '123456');

  HttpRequest1.Post('https://httpbin.org/post', Body);
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;
```

### POST with multipart (file upload)

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  Body: TMultipartFormBody;
begin
  Body := TMultipartFormBody.Create;
  Body.ReleaseAfterSend := True;
  Body.Add('field', 'value');
  Body.AddFromFile('file', 'C:\Users\User\Desktop\file.pdf');

  HttpRequest1.Post('https://httpbin.org/post', Body);
  ShowMessage(HttpRequest1.Response.ContentAsString);
end;
```

---

## Published Properties

### `Headers: THeaders`

Custom HTTP headers sent with every request.

```pascal
HttpRequest1.Headers.AddHeader('X-Custom-Header', 'value');
HttpRequest1.Headers.AddHeader('Accept', 'application/json');
```

> **Note:** Use `AddHeader`, not `Values[]` directly, since values may contain `:`.

### `UserAgent: String`

Client identification (default: `'Mozilla/5.0 (compatible, HttpClient)'`). Changes invalidate the WinINet session.

### `HttpVersion: THttpVersion`

HTTP version: `hv1_0` or `hv1_1` (default).

### `UseCookies: Boolean`

Enables cookie management (default: `False`). When disabled, clears all stored cookies.

### `AutoRedirect: Boolean`

Follows redirects automatically via WinINet (default: `True`). When `False`, redirects are handled manually by the component (respecting `MaxRedirects`).

### `SecurityOptions: TSecurityOptions`

Bypasses TLS/SSL validation. **Dangerous — disabled by default.**

```pascal
type
  TSecurityOption = (
    soIgnoreUnknownCA,      // Ignore unknown CA
    soIgnoreCertCNInvalid,   // Ignore invalid CN
    soIgnoreCertDateInvalid, // Ignore expired certificate
    soIgnoreRedirectHttps,   // Ignore HTTPS->HTTP redirect
    soIgnoreRedirectHttp     // Ignore HTTP->HTTPS redirect
  );
```

### `Username` / `Password`

Credentials for authentication. Changes invalidate the connection (not the session).

### `Timeout: THttpTimeouts`

Timeouts in milliseconds (0 = system default):

| Property | Description |
|----------|-------------|
| `ConnectTimeout` | Connection timeout |
| `SendTimeout` | Send timeout |
| `ReceiveTimeout` | Receive timeout |

### `MaxRetries: Integer`

Number of automatic retries for idempotent methods on 408 or 5xx errors (default: 0).

### `MaxContentLength: Int64`

Maximum response body size in bytes (0 = no limit). Raises exception if exceeded.

### `AcceptCompressed: Boolean`

Sends `Accept-Encoding: gzip, deflate` (default: `True`).

### `ProxyMode: THttpProxyMode`

| Value | Description |
|-------|-------------|
| `pmSystem` | Use system configuration (default) |
| `pmDirect` | Direct connection, no proxy |
| `pmExplicit` | Use `ProxyUrl` |

### `ProxyUrl: String`

Proxy URL (e.g.: `'http://proxy:8080'`). Used when `ProxyMode = pmExplicit`.

### `AuthScheme: THttpAuthScheme`

| Value | Description |
|-------|-------------|
| `asNone` | No authentication (default) |
| `asBasic` | Basic (uses `Username`/`Password`) |
| `asBearer` | Bearer token (uses `AuthToken`) |
| `asNegotiate` | Negotiate/Kerberos (not implemented) |

### `AuthToken: String`

Token for Bearer authentication.

### `MaxRedirects: Integer`

Maximum manual redirects when `AutoRedirect = False` (default: 10).

### `CachePolicy: THttpCachePolicy`

| Value | Description |
|-------|-------------|
| `cpDefault` | Default behavior (default) |
| `cpReload` | Force reload from server |
| `cpNoStore` | Do not store in cache |

---

## Public Methods

### HTTP Methods

| Method | Signature |
|--------|-----------|
| `Get` | `function Get(AUrl: String): Boolean;` |
| `Head` | `function Head(AUrl: String): Boolean;` |
| `Options` | `function Options(AUrl: String): Boolean;` |
| `Trace` | `function Trace(AUrl: String): Boolean;` |
| `Delete` | `function Delete(AUrl: String): Boolean; overload;` |
| `Delete` | `function Delete(AUrl: String; ABody: TBody): Boolean; overload;` |
| `Delete` | `function Delete(AUrl: String; ABody: String): Boolean; overload;` |
| `Post` | `function Post(AUrl: String): Boolean; overload;` |
| `Post` | `function Post(AUrl: String; ABody: TBody): Boolean; overload;` |
| `Post` | `function Post(AUrl: String; ABody: String): Boolean; overload;` |
| `Put` | `function Put(AUrl: String): Boolean; overload;` |
| `Put` | `function Put(AUrl: String; ABody: TBody): Boolean; overload;` |
| `Put` | `function Put(AUrl: String; ABody: String): Boolean; overload;` |
| `Patch` | `function Patch(AUrl: String): Boolean; overload;` |
| `Patch` | `function Patch(AUrl: String; ABody: TBody): Boolean; overload;` |
| `Patch` | `function Patch(AUrl: String; ABody: String): Boolean; overload;` |

**Returns:** `True` if status 2xx, `False` otherwise.

### Other Methods

| Method | Description |
|--------|-------------|
| `Abort` | Cancels in-progress request |

### Read-Only Properties

| Property | Type | Description |
|----------|------|-------------|
| `Response` | `THttpResponse` | Response from last request |
| `Cookies` | `TCookies` | Cookie jar |
| `LastUrl` | `String` | Effective URL after redirects |
| `Aborted` | `Boolean` | Whether the request was aborted |

---

## Bodies (Request Body)

### Hierarchy

```
TBody (abstract)
├── TBytesBody
│   └── TStringBody
├── TUrlEncodedFormBody
└── TMultipartFormBody
```

### TBody (base class)

Properties:
- `ContentType: String` — MIME type of the body
- `Stream: TMemoryStream` — stream with data (freed by caller)
- `ReleaseAfterSend: Boolean` — if `True`, the request frees the body automatically

### TStringBody

```pascal
Body := TStringBody.Create('text', 'text/plain; charset=utf-8');
// or
Body := TStringBody.Create('text'); // uses text/plain; charset=utf-8
```

### TBytesBody

```pascal
// From TBytes
Body := TBytesBody.Create(Bytes, 'application/octet-stream');

// From file
Body := TBytesBody.Create('C:\path\file.bin');
```

### TUrlEncodedFormBody

```pascal
Body := TUrlEncodedFormBody.Create;
Body.ReleaseAfterSend := True;
Body.Add('field1', 'value1');
Body.Add('field2', 'value2');
// Generates: field1=value1&field2=value2 (URL-encoded)
```

### TMultipartFormBody

```pascal
Body := TMultipartFormBody.Create;
Body.ReleaseAfterSend := True;
Body.Add('text_field', 'value');
Body.AddFromFile('file_field', 'C:\path\file.pdf');
```

### Body Ownership

**Default (caller frees):**
```pascal
Body := TStringBody.Create('test');
try
  HttpRequest1.Post('https://example.com', Body);
finally
  Body.Free;
end;
```

**Component frees (`ReleaseAfterSend := True`):**
```pascal
Body := TStringBody.Create('test');
Body.ReleaseAfterSend := True;
HttpRequest1.Post('https://example.com', Body);
// Do NOT free Body manually — the request already freed it (even on exception)
```

> **Never mix both strategies on the same instance** — this causes a double-free.

---

## Response

### THttpResponse

| Property | Type | Description |
|----------|------|-------------|
| `StatusCode` | `Integer` | HTTP status code (200, 404, etc.) |
| `Content` | `TBytes` | Response body as bytes |
| `ContentAsString` | `String` | Body converted to string (detects charset from Content-Type) |
| `ContentType` | `String` | Content-Type header |
| `ContentLength` | `Integer` | Content-Length header (-1 if absent) |
| `Headers` | `THeaders` | All response headers |

### Methods

| Method | Signature |
|--------|-----------|
| `SaveToFile` | `procedure SaveToFile(AFileName: String);` |
| `SaveToStream` | `procedure SaveToStream(AStream: TStream);` |
| `Clear` | `procedure Clear;` |

### Usage Example

```pascal
if HttpRequest1.Get('https://httpbin.org/json') then
begin
  Memo1.Lines.Add('Status: ' + IntToStr(HttpRequest1.Response.StatusCode));
  Memo1.Lines.Add('Content-Type: ' + HttpRequest1.Response.ContentType);
  Memo1.Lines.Add('Size: ' + IntToStr(HttpRequest1.Response.ContentLength));
  Memo1.Lines.Add('Body:');
  Memo1.Lines.Add(HttpRequest1.Response.ContentAsString);
end;
```

### Accessing Response Headers

```pascal
// First value of a header
ContentType := HttpRequest1.Response.Headers.GetFirst('Content-Type');

// All values of a header (e.g.: Set-Cookie)
CookieList := HttpRequest1.Response.Headers.GetAll('Set-Cookie');
try
  for Cookie in CookieList do
    Memo1.Lines.Add(Cookie);
finally
  CookieList.Free;
end;
```

---

## Cookies

Enabled by `UseCookies := True`. The component manages cookies per RFC 6265.

### Supported Attributes

| Attribute | Behavior |
|-----------|----------|
| `Domain` | Domain scope (sent to subdomains) |
| `Path` | Path scope |
| `Expires` | Expiration date (RFC 1123) |
| `Max-Age` | Lifetime in seconds (takes precedence over Expires) |
| `Secure` | Sent only via HTTPS |
| `HttpOnly` | Stored and exposed (property `HttpOnly`) |
| `SameSite` | `None`, `Lax`, or `Strict` |

### Sharing Cookies Between Instances

```pascal
HttpRequest2.Cookies.AssignFrom(HttpRequest1.Cookies);
```

### Example

```pascal
HttpRequest1.UseCookies := True;

// Login
HttpRequest1.Post('https://example.com/login', 'user=john&pass=123');

// Next request sends cookies automatically
HttpRequest1.Get('https://example.com/profile');
```

---

## Timeouts

```pascal
HttpRequest1.Timeout.ConnectTimeout := 5000;   // 5 seconds
HttpRequest1.Timeout.SendTimeout := 10000;     // 10 seconds
HttpRequest1.Timeout.ReceiveTimeout := 30000;  // 30 seconds
```

> **Note:** 0 = WinINet system default.

---

## Proxy

### Use system proxy (default)

```pascal
HttpRequest1.ProxyMode := pmSystem;
```

### Direct connection (no proxy)

```pascal
HttpRequest1.ProxyMode := pmDirect;
```

### Explicit proxy

```pascal
HttpRequest1.ProxyMode := pmExplicit;
HttpRequest1.ProxyUrl := 'http://proxy.company.com:8080';
```

---

## Authentication

### Basic (username/password)

```pascal
HttpRequest1.AuthScheme := asBasic;
HttpRequest1.Username := 'user';
HttpRequest1.Password := 'pass';
HttpRequest1.Get('https://example.com/protected');
```

### Bearer token

```pascal
HttpRequest1.AuthScheme := asBearer;
HttpRequest1.AuthToken := 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...';
HttpRequest1.Get('https://example.com/api/data');
```

---

## Redirect

### Automatic (default)

```pascal
HttpRequest1.AutoRedirect := True; // WinINet follows redirects
```

### Manual

```pascal
HttpRequest1.AutoRedirect := False;
HttpRequest1.MaxRedirects := 5;
HttpRequest1.Get('https://example.com/redirect');
// LastUrl contains the final URL after redirects
```

> **Note:** Manual redirects respect `MaxRedirects` and raise an exception if exceeded.

---

## Cache

```pascal
HttpRequest1.CachePolicy := cpReload;   // Force reload
HttpRequest1.CachePolicy := cpNoStore;  // Do not store in cache
HttpRequest1.CachePolicy := cpDefault;  // Default behavior
```

---

## Abort (Cancellation)

Cancels the in-progress request from any thread:

```pascal
procedure TForm1.btnCancelClick(Sender: TObject);
begin
  HttpRequest1.Abort;
end;
```

`Abort`:
1. Sets the internal `Aborted` flag
2. Closes the active WinINet handle (interrupts blocking calls)
3. Raises exception `'Request aborted by the user'`

During download, the flag is checked on every chunk (in `ReadResponseBody`), ensuring fast response to cancellation.

---

## Retry (Automatic Retries)

Configured via `MaxRetries`:

```pascal
HttpRequest1.MaxRetries := 3; // Up to 3 attempts
```

**Conservative policy:**
- **Only idempotent methods:** GET, HEAD, OPTIONS, DELETE, TRACE, PUT
- **Only retryable codes:** 408 (Request Timeout) and all 5xx
- **Honors `Retry-After`:** waits the interval specified by the server
- **Disabled if body was released** (`ReleaseAfterSend := True`)

---

## Events

### `OnBeforeRequest`

Fired before each request (including before each retry attempt):

```pascal
procedure TForm1.HttpRequest1BeforeRequest(Sender: TObject; AMethod, AUrl: String);
begin
  Memo1.Lines.Add(Format('[%s %s]', [AMethod, AUrl]));
end;
```

### `OnAfterResponse`

Fired after completion (success or error) of each request:

```pascal
procedure TForm1.HttpRequest1AfterResponse(Sender: TObject);
begin
  Memo1.Lines.Add(Format('Status: %d', [HttpRequest1.Response.StatusCode]));
end;
```

### `OnProgress`

Fired during body download:

```pascal
procedure TForm1.HttpRequest1Progress(Sender: TObject; ABytesRead, ABytesTotal: Integer);
begin
  ProgressBar1.Max := ABytesTotal;
  ProgressBar1.Position := ABytesRead;
  Application.ProcessMessages;
end;
```

---

## Security

### ⚠️ SecurityOptions

Security options bypass TLS/SSL validation. **They are dangerous and disabled by default.**

```pascal
// ⚠️ ONLY for known servers with self-signed certificates!
HttpRequest1.SecurityOptions := [soIgnoreUnknownCA, soIgnoreCertDateInvalid];
HttpRequest1.Get('https://internal-server.local/api');
```

**Risk:** enabling on untrusted networks exposes the connection to man-in-the-middle attacks.

### Credentials in URL

Avoid embedding credentials in the URL (`https://user:pass@host/...`) — they are sent in plain text and may leak into logs. Prefer the `Username`/`Password` properties.

---

## Complete Examples

### Login with cookies and session

```pascal
procedure TForm1.btnLoginClick(Sender: TObject);
var
  Body: TUrlEncodedFormBody;
begin
  HttpRequest1.UseCookies := True;

  Body := TUrlEncodedFormBody.Create;
  Body.ReleaseAfterSend := True;
  Body.Add('username', edtUser.Text);
  Body.Add('password', edtPass.Text);

  if HttpRequest1.Post('https://example.com/api/login', Body) then
  begin
    ShowMessage('Login OK!');
    // Session cookies are sent automatically on subsequent requests
    HttpRequest1.Get('https://example.com/api/profile');
    Memo1.Lines.Text := HttpRequest1.Response.ContentAsString;
  end
  else
    ShowMessage('Login failed: ' + IntToStr(HttpRequest1.Response.StatusCode));
end;
```

### File upload with progress

```pascal
procedure TForm1.btnUploadClick(Sender: TObject);
var
  Body: TMultipartFormBody;
begin
  Body := TMultipartFormBody.Create;
  Body.ReleaseAfterSend := True;
  Body.Add('description', edtDesc.Text);
  Body.AddFromFile('file', edtFile.Text);

  HttpRequest1.OnProgress := HttpRequest1Progress;

  if HttpRequest1.Post('https://example.com/api/upload', Body) then
    ShowMessage('Upload complete!')
  else
    ShowMessage('Upload error: ' + IntToStr(HttpRequest1.Response.StatusCode));

  HttpRequest1.OnProgress := nil;
end;

procedure TForm1.HttpRequest1Progress(Sender: TObject; ABytesRead, ABytesTotal: Integer);
begin
  if ABytesTotal > 0 then
  begin
    ProgressBar1.Max := ABytesTotal;
    ProgressBar1.Position := ABytesRead;
    Application.ProcessMessages;
  end;
end;
```

### Request with Bearer authentication

```pascal
procedure TForm1.btnSearchClick(Sender: TObject);
begin
  HttpRequest1.AuthScheme := asBearer;
  HttpRequest1.AuthToken := edtToken.Text;
  HttpRequest1.Headers.AddHeader('Accept', 'application/json');

  if HttpRequest1.Get('https://api.example.com/data') then
    Memo1.Lines.Text := HttpRequest1.Response.ContentAsString
  else
    ShowMessage('Error: ' + IntToStr(HttpRequest1.Response.StatusCode));
end;
```

### Request with retry

```pascal
procedure TForm1.btnRequestClick(Sender: TObject);
begin
  HttpRequest1.MaxRetries := 3;
  HttpRequest1.Timeout.ConnectTimeout := 5000;

  if HttpRequest1.Get('https://api.example.com/data') then
    ProcessResponse
  else
    ShowMessage('Failed after retries: ' + IntToStr(HttpRequest1.Response.StatusCode));
end;
```

### Request cancellation

```pascal
procedure TForm1.btnStartClick(Sender: TObject);
begin
  HttpRequest1.Get('https://example.com/long-operation');
end;

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  HttpRequest1.Abort; // Interrupts the in-progress request
end;
```

---

## Thread-Safety

**The component is NOT thread-safe.** Use one of the following approaches:

1. **One `THttpRequest` per thread** (recommended)
2. **Serialize all calls** on a single thread (usually the VCL main thread)

`Abort` can be called from another thread to interrupt a blocking operation.

---

## Compatibility

- **Delphi:** 2010 (Unicode)
- **Platform:** Win32 / x86
- **API:** WinINet (`wininet.dll`)
- **Does not use:** FMX, 64-bit, `System.Net.HttpClient`

### Known Limitations

- `InternetTimeToSystemTime` is not available in WinINet for D2010 (HTTP dates use custom parser)
- `SameSite` is stored but does not apply full cross-site policy
- `Negotiate`/`Kerberos` not implemented (declaration only)

---

## API Reference

### THttpStatus (HTTP status code enumeration)

```pascal
THttpStatus = (
  SC_CONTINUE = 100,
  SC_OK = 200,
  SC_CREATED = 201,
  SC_BAD_REQUEST = 400,
  SC_UNAUTHORIZED = 401,
  SC_FORBIDDEN = 403,
  SC_NOT_FOUND = 404,
  SC_INTERNAL_SERVER_ERROR = 500,
  SC_BAD_GATEWAY = 502,
  SC_SERVICE_UNAVAILABLE = 503,
  // ... see HttpClasses.pas for complete list
);
```

### THttpURI (URL parser)

```pascal
URI := THttpURI.Create('https://user:pass@host:8080/path?query#fragment');
WriteLn(URI.Protocol);   // 'https'
WriteLn(URI.Host);       // 'host'
WriteLn(URI.Port);       // 8080
WriteLn(URI.Path);       // '/path'
WriteLn(URI.Params);     // 'query'
WriteLn(URI.Username);   // 'user'
WriteLn(URI.Password);   // 'pass'
```

---

## Troubleshooting

| Problem | Solution |
|---------|----------|
| SSL certificate error | Check certificate expiration date; use `SecurityOptions` only on known servers |
| Timeout | Adjust `Timeout.ConnectTimeout`, `Timeout.SendTimeout`, `Timeout.ReceiveTimeout` |
| Redirect loop | Check `MaxRedirects` and `AutoRedirect` |
| Body too large | Adjust `MaxContentLength` or use `Response.SaveToFile`/`Response.SaveToStream` |
| Cookies not sent | Check if `UseCookies := True` and if domain/path match |
| Proxy error | Check `ProxyMode` and `ProxyUrl` |

---

## License

See the `LICENSE` file in the project repository.