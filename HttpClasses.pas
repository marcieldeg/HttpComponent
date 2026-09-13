unit HttpClasses;

interface

uses
  Classes, SysUtils, Generics.Collections, WinInet;

type
  THeaders = class(TStringList)
  private
    procedure FromRawString(AStringHeaders: String);
  public
    constructor Create;
    procedure AddHeader(AName, AValue: String);
    function GetAll(AName: String): TStrings;
    function GetFirst(AName: String): String;
    function ToString: String; override;
  end;

  TBody = class abstract
  private
    FContentType: String;
    FNeedLength: Boolean;
    FReleaseAfterSend: Boolean;
    FEncoding: TEncoding;
    function GetStream: TMemoryStream; virtual; abstract;
  public
    constructor Create(AContentType: String; ANeedLength: Boolean);
    function ToString: String; override;
    property ContentType: String read FContentType;
    property Stream: TMemoryStream read GetStream;
    property ReleaseAfterSend: Boolean read FReleaseAfterSend write FReleaseAfterSend;
  end;

  TBytesBody = class(TBody)
  private
    FData: TBytes;
    function GetStream: TMemoryStream; override;
  public
    constructor Create(AData: TBytes); overload;
    constructor Create(AData: TBytes; AContentType: String = 'application/octet-stream'); overload;
    constructor Create(AFileName: String); overload;
  end;

  TStringBody = class(TBytesBody)
  public
    constructor Create(AData: String; AContentType: String = 'text/plain; charset=utf-8'); reintroduce; overload;
    constructor Create(AData: UTF8String; AContentType: String = 'text/plain; charset=utf-8'); reintroduce; overload;
  end;

  TUrlEncodedFormBody = class(TBody)
  private
    FParts: TStringList;
    function GetStream: TMemoryStream; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AName, AValue: String); overload;
  end;

  TPart = class
  private
    FName: String;
    FFilename: String;
    FData: TBytes;
    FContentType: String;
  public
    constructor Create(AName: String; AData: TBytes; AContentType: String); overload;
    constructor CreateFromFile(AName, AFileName: String);
    property Name: String read FName;
    property Filename: String read FFilename;
    property Data: TBytes read FData;
    property ContentType: String read FContentType;
  end;

  TMultipartFormBody = class(TBody)
  private
    FBoundary: String;
    FParts: TObjectList<TPart>;
    function GetStream: TMemoryStream; override;
    function GetPart(AIndex: Integer): TPart;
    function GetPartCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AName, AData: String; AContentType: String = 'text/plain'); overload;
    procedure Add(AName: String; AData: UTF8String; AContentType: String = 'text/plain; charset=utf-8'); overload;
    procedure Add(AName: String; AData: TBytes; AContentType: String = 'application/octet-stream'); overload;
    procedure AddFromFile(AName, AFileName: String);
    property Part[AIndex: Integer]: TPart read GetPart;
    property PartCount: Integer read GetPartCount;
  end;

  THttpStatus = (
    SC_CONTINUE = 100,
    SC_SWITCHING_PROTOCOLS = 101,
    SC_PROCESSING = 102,
    SC_EARLY_HINTS = 103,
    SC_OK = 200,
    SC_CREATED = 201,
    SC_ACCEPTED = 202,
    SC_NON_AUTHORITATIVE_INFORMATION = 203,
    SC_NO_CONTENT = 204,
    SC_RESET_CONTENT = 205,
    SC_PARTIAL_CONTENT = 206,
    SC_MULTI_STATUS = 207,
    SC_IM_USED = 226,
    SC_MULTIPLE_CHOICES = 300,
    SC_MOVED_PERMANENTLY = 301,
    SC_MOVED_TEMPORARILY = 302,
    SC_SEE_OTHER = 303,
    SC_NOT_MODIFIED = 304,
    SC_USE_PROXY = 305,
    SC_TEMPORARY_REDIRECT = 307,
    SC_PERMANENT_REDIRECT = 308,
    SC_BAD_REQUEST = 400,
    SC_UNAUTHORIZED = 401,
    SC_PAYMENT_REQUIRED = 402,
    SC_FORBIDDEN = 403,
    SC_NOT_FOUND = 404,
    SC_METHOD_NOT_ALLOWED = 405,
    SC_NOT_ACCEPTABLE = 406,
    SC_PROXY_AUTHENTICATION_REQUIRED = 407,
    SC_REQUEST_TIMEOUT = 408,
    SC_CONFLICT = 409,
    SC_GONE = 410,
    SC_LENGTH_REQUIRED = 411,
    SC_PRECONDITION_FAILED = 412,
    SC_REQUEST_TOO_LONG = 413,
    SC_REQUEST_URI_TOO_LONG = 414,
    SC_UNSUPPORTED_MEDIA_TYPE = 415,
    SC_REQUESTED_RANGE_NOT_SATISFIABLE = 416,
    SC_EXPECTATION_FAILED = 417,
    SC_IM_A_TEAPOT = 418,
    SC_INSUFFICIENT_SPACE_ON_RESOURCE = 419,
    SC_METHOD_FAILURE = 420,
    SC_MISDIRECT_REQUEST = 421,
    SC_UNPROCESSABLE_ENTITY = 422,
    SC_LOCKED = 423,
    SC_FAILED_DEPENDENCY = 424,
    SC_TOO_EARLY = 425,
    SC_UPGRADE_REQUIRED = 426,
    SC_PRECONDITION_REQUIRED = 428,
    SC_TOO_MANY_REQUESTS = 429,
    SC_REQUEST_HEADER_FIELDS_TOO_LARGE = 431,
    SC_UNAVAILABLE_FOR_LEGAL_REASONS = 451,
    SC_INTERNAL_SERVER_ERROR = 500,
    SC_NOT_IMPLEMENTED = 501,
    SC_BAD_GATEWAY = 502,
    SC_SERVICE_UNAVAILABLE = 503,
    SC_GATEWAY_TIMEOUT = 504,
    SC_HTTP_VERSION_NOT_SUPPORTED = 505,
    SC_INSUFFICIENT_STORAGE = 507,
    SC_LOOP_DETECTED = 508,
    SC_NOT_EXTENDED = 510,
    SC_NETWORK_AUTHENTICATION_REQUIRED = 511);

  THttpResponse = class(TComponent)
  private
    FStatusCode: Integer;
    FHeaders: THeaders;
    FData: TBytes;
    function GetContentLength: Integer;
    function GetContentType: String;
    function GetContentAsString: String;
    function GetEncodingForCharset: TEncoding;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile(AFileName: String);
    procedure SaveToStream(AStream: TStream);
    property StatusCode: Integer read FStatusCode;
    property Content: TBytes read FData;
    property ContentAsString: String read GetContentAsString;
    property ContentType: String read GetContentType;
    property ContentLength: Integer read GetContentLength;
    property Headers: THeaders read FHeaders;
  end;

  TCookieSameSite = (csNone, csLax, csStrict);

  TCookie = class
  private
    FName: String;
    FValue: String;
    FDomain: String;
    FPath: String;
    FExpires: TDateTime;
    FHasExpires: Boolean;
    FHostOnly: Boolean;
    FSecure: Boolean;
    FHttpOnly: Boolean;
    FSameSite: TCookieSameSite;
    FMaxAge: Integer;
    FHasMaxAge: Boolean;
    FCreatedAt: TDateTime;
    function GetDefaultPath(const ARequestPath: String): String;
    function TryParseExpires(const AValue: String; out AExpires: TDateTime): Boolean;
  public
    constructor Create(const ASetCookie, ADefaultDomain, ADefaultPath: String);
    procedure AssignFrom(ASource: TCookie);
    function IsExpired: Boolean;
    function Matches(const AHost, APath, AScheme: String): Boolean;
    function ToRequestValue: String;
    property Name: String read FName;
    property Value: String read FValue;
    property Domain: String read FDomain;
    property Path: String read FPath;
    property Expires: TDateTime read FExpires;
    property HasExpires: Boolean read FHasExpires;
    property HostOnly: Boolean read FHostOnly;
    property Secure: Boolean read FSecure;
    property HttpOnly: Boolean read FHttpOnly;
    property SameSite: TCookieSameSite read FSameSite;
    property MaxAge: Integer read FMaxAge;
    property HasMaxAge: Boolean read FHasMaxAge;
    property CreatedAt: TDateTime read FCreatedAt;
  end;

  TCookies = class(TObjectList<TCookie>)
  private
    procedure RemoveExpired;
  public
    constructor Create;
    procedure AddFromSetCookie(const ASetCookie, ADefaultDomain, ADefaultPath: String);
    function ToRequestHeaders(const AHost, APath, AScheme: String): String;
    procedure AssignFrom(ASource: TCookies);
  end;

  TSecurityOption = (soSecure, soSsl, soSsl3, soPct, soPct4, soIetfssl4, so40bit, so128bit, so56bit, soUnknownbit,
    soIgnoreRevication, soIgnoreUnknownCA, soIgnoreWrongUsage, soIgnoreCertCNInvalid, soIgnoreCertDateInvalid,
    soIgnoreRedirectHttps, soIgnoreRedirectHttp);

  TSecurityOptions = set of TSecurityOption;

  THttpVersion = (hv1_0, hv1_1);

  THttpOnProgress = procedure(Sender: TObject; ABytesRead, ABytesTotal: Integer) of object;

  THttpOnBeforeRequest = procedure(Sender: TObject; AMethod, AUrl: String) of object;
  THttpOnAfterResponse = procedure(Sender: TObject) of object;

  THttpProxyMode = (pmSystem, pmDirect, pmExplicit);
  THttpAuthScheme = (asNone, asBasic, asBearer, asNegotiate);
  THttpCachePolicy = (cpDefault, cpReload, cpNoStore);

  THttpURI = class;

  THttpTimeouts = class(TPersistent)
  private
    FConnectTimeout: Cardinal;
    FSendTimeout: Cardinal;
    FReceiveTimeout: Cardinal;
    FOnChange: TNotifyEvent;
    procedure SetConnectTimeout(Value: Cardinal);
    procedure SetSendTimeout(Value: Cardinal);
    procedure SetReceiveTimeout(Value: Cardinal);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ConnectTimeout: Cardinal read FConnectTimeout write SetConnectTimeout default 0;
    property SendTimeout: Cardinal read FSendTimeout write SetSendTimeout default 0;
    property ReceiveTimeout: Cardinal read FReceiveTimeout write SetReceiveTimeout default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THttpRequest = class(TComponent)
  private
    FUserAgent: String;
    FHeaders: THeaders;
    FUseCookies: Boolean;
    FCookies: TCookies;
    FSecurityOptions: TSecurityOptions;
    FResponse: THttpResponse;
    FHttpVersion: THttpVersion;
    FUsername: String;
    FPassword: String;
    FAutoRedirect: Boolean;
    FOnProgress: THttpOnProgress;
    FTimeout: THttpTimeouts;
    FhInet: HINTERNET;
    FhConnect: HINTERNET;
    FSessionUserAgent: String;
    FSessionProxyMode: THttpProxyMode;
    FSessionProxyUrl: String;
    FConnectHost: String;
    FConnectPort: Integer;
    FConnectUser: String;
    FConnectPassword: String;
    FLastUrl: String;
    FMaxRetries: Integer;
    FAbortRequested: Boolean;
    FhRequestActive: HINTERNET;
    FOnBeforeRequest: THttpOnBeforeRequest;
    FOnAfterResponse: THttpOnAfterResponse;
    FMaxContentLength: Int64;
    FAcceptCompressed: Boolean;
    FProxyMode: THttpProxyMode;
    FProxyUrl: String;
    FAuthScheme: THttpAuthScheme;
    FAuthToken: String;
    FMaxRedirects: Integer;
    FCachePolicy: THttpCachePolicy;
    procedure SetUseCookies(AValue: Boolean);
    procedure SetAcceptCompressed(AValue: Boolean);
    procedure SetUserAgent(const AValue: String);
    procedure SetUsername(const AValue: String);
    procedure SetPassword(const AValue: String);
    procedure SetProxyMode(AValue: THttpProxyMode);
    procedure SetProxyUrl(const AValue: String);
    procedure SetAuthScheme(AValue: THttpAuthScheme);
    procedure SetMaxRedirects(AValue: Integer);
    procedure SetCachePolicy(AValue: THttpCachePolicy);
    procedure CloseInternetHandle(var AHandle: HINTERNET);
    procedure CloseConnect;
    procedure CloseSession;
    procedure EnsureSession;
    procedure EnsureConnect(const AURI: THttpURI);
    procedure CaptureLastUrl(ARequest: HINTERNET; const AFallback: String);
    function Request(AMethod, AUrl: String; ABody: TBody): Boolean;
    function BuildOpenRequestFlags(const AURI: THttpURI): Cardinal;
    procedure ApplyTimeouts(ARequest: HINTERNET);
    procedure ApplySecurityFlags(ARequest: HINTERNET);
    function BuildRequestHeaders(const AURI: THttpURI; ABody: TBody; ABodyStream: TMemoryStream): String;
    procedure SendRequestToServer(ARequest: HINTERNET; const AHeaders: String; ABodyStream: TMemoryStream);
    function ReadResponseStatusCode(ARequest: HINTERNET): Integer;
    function ReadRawHeaders(ARequest: HINTERNET): String;
    procedure ReadResponseBody(ARequest: HINTERNET);
    procedure ExtractCookiesFromResponse(const AHost, APath, AScheme: String);
    function IsIdempotentMethod(const AMethod: String): Boolean;
    function GetRetryDelaySeconds(ARequest: HINTERNET): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Delete(AUrl: String): Boolean; overload;
    function Delete(AUrl: String; ABody: TBody): Boolean; overload;
    function Delete(AUrl: String; ABody: String): Boolean; overload;
    function Get(AUrl: String): Boolean;
    function Head(AUrl: String): Boolean;
    function Options(AUrl: String): Boolean;
    function Patch(AUrl: String): Boolean; overload;
    function Patch(AUrl: String; ABody: TBody): Boolean; overload;
    function Patch(AUrl: String; ABody: String): Boolean; overload;
    function Post(AUrl: String): Boolean; overload;
    function Post(AUrl: String; ABody: TBody): Boolean; overload;
    function Post(AUrl: String; ABody: String): Boolean; overload;
    function Put(AUrl: String): Boolean; overload;
    function Put(AUrl: String; ABody: TBody): Boolean; overload;
    function Put(AUrl: String; ABody: String): Boolean; overload;
    function Trace(AUrl: String): Boolean;
    procedure Abort;
    property Cookies: TCookies read FCookies;
    property Response: THttpResponse read FResponse;
    property LastUrl: String read FLastUrl;
    property Aborted: Boolean read FAbortRequested;
  published
    property Headers: THeaders read FHeaders write FHeaders;
    property UseCookies: Boolean read FUseCookies write SetUseCookies default False;
    property AutoRedirect: Boolean read FAutoRedirect write FAutoRedirect default True;
    property SecurityOptions: TSecurityOptions read FSecurityOptions write FSecurityOptions;
    property UserAgent: String read FUserAgent write SetUserAgent;
    property HttpVersion: THttpVersion read FHttpVersion write FHttpVersion default hv1_1;
    property Username: String read FUsername write SetUsername;
    property Password: String read FPassword write SetPassword;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries default 0;
    property MaxContentLength: Int64 read FMaxContentLength write FMaxContentLength default 0;
    property AcceptCompressed: Boolean read FAcceptCompressed write SetAcceptCompressed default True;
    property ProxyMode: THttpProxyMode read FProxyMode write SetProxyMode default pmSystem;
    property ProxyUrl: String read FProxyUrl write SetProxyUrl;
    property AuthScheme: THttpAuthScheme read FAuthScheme write SetAuthScheme default asNone;
    property AuthToken: String read FAuthToken write FAuthToken;
    property MaxRedirects: Integer read FMaxRedirects write SetMaxRedirects default 10;
    property CachePolicy: THttpCachePolicy read FCachePolicy write SetCachePolicy default cpDefault;
    property OnBeforeRequest: THttpOnBeforeRequest read FOnBeforeRequest write FOnBeforeRequest;
    property OnAfterResponse: THttpOnAfterResponse read FOnAfterResponse write FOnAfterResponse;
    property OnProgress: THttpOnProgress read FOnProgress write FOnProgress;
    property Timeout: THttpTimeouts read FTimeout write FTimeout;
  end;

  TIPVersion = (ivIP4, ivIP6);
  TURIOptionalFields = (ofAuthInfo, ofBookmark);
  TURIOptionalFieldsSet = set of TURIOptionalFields;

  THttpURI = class
  private
    FDocument: string;
    FProtocol: string;
    FURI: String;
    FPort: Integer;
    FPath: string;
    FHost: string;
    FBookmark: string;
    FUsername: string;
    FPassword: string;
    FParams: string;
    FIPVersion: TIPVersion;
    procedure SetURI(const Value: String);
    function GetURI: String;
    procedure SetProtocol(const Value: String);
  public
    constructor Create(const AURI: String = '');
    function GetFullURI(const AOptionalFields: TURIOptionalFieldsSet = [ofAuthInfo, ofBookmark]): String;
    function GetPathAndParams: String;
    property Bookmark: String read FBookmark write FBookmark;
    property Document: String read FDocument write FDocument;
    property Host: String read FHost write FHost;
    property Password: String read FPassword write FPassword;
    property Path: String read FPath write FPath;
    property Params: String read FParams write FParams;
    property Port: Integer read FPort write FPort;
    property Protocol: String read FProtocol write SetProtocol;
    property URI: String read GetURI write SetURI;
    property Username: String read FUsername write FUsername;
    property IPVersion: TIPVersion read FIPVersion write FIPVersion;
  end;

procedure Register;

implementation

uses
  Windows, IOUtils, UrlMon, DateUtils, Math, EncdDecd, HttpUtils;

procedure Register;
begin
  Classes.RegisterComponents('HttpClient', [THttpRequest]);
end;

{THeaders}

procedure THeaders.AddHeader(AName, AValue: String);
begin
  Add(AName + NameValueSeparator + AValue);
end;

constructor THeaders.Create;
begin
  inherited;
  Delimiter := #10;
  NameValueSeparator := ':';
  StrictDelimiter := True;
end;

procedure THeaders.FromRawString(AStringHeaders: String);
var
  i: Integer;
  Name, Value: String;
begin
  with TStringList.Create do
    try
      Delimiter := #$D;
      NameValueSeparator := ':';
      StrictDelimiter := True;
      DelimitedText := StringReplace(AStringHeaders, #$D#$A, #$D, [rfReplaceAll]);
      for i := 0 to Count - 1 do
      begin
        Name := Trim(Names[i]);
        Value := Trim(ValueFromIndex[i]);
        if (Name <> '') and (Value <> '') then
          AddHeader(Name, Value);
      end;
    finally
      Free;
    end;
end;

function THeaders.GetAll(AName: String): TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;

  for i := 0 to Count - 1 do
    if SameText(Names[i], AName) then
      Result.Add(ValueFromIndex[i]);
end;

function THeaders.GetFirst(AName: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    if SameText(Names[i], AName) then
    begin
      Result := ValueFromIndex[i];
      Exit;
    end;
end;

function THeaders.ToString: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if i > 0 then
      Result := Result + sLineBreak;
    Result := Result + Strings[i];
  end;
end;

{TBody}

constructor TBody.Create(AContentType: String; ANeedLength: Boolean);
begin
  inherited Create;
  FContentType := AContentType;
  FNeedLength := ANeedLength;
  FEncoding := TEncoding.Default;
end;

function TBody.ToString: String;
var
  Stream: TMemoryStream;
  Bytes: TBytes;
begin
  Stream := GetStream;
  try
    SetLength(Bytes, Stream.Size);
    if Stream.Size > 0 then
    begin
      Stream.Position := 0;
      Stream.ReadBuffer(Bytes[0], Stream.Size);
    end;
    if Assigned(FEncoding) then
      Result := FEncoding.GetString(Bytes)
    else
      Result := TEncoding.Default.GetString(Bytes);
  finally
    Stream.Free;
  end;
end;

{TBytesBody}

constructor TBytesBody.Create(AData: TBytes);
begin
  inherited Create(GetMimeType(Pointer(AData), Length(AData)), True);
  FData := AData;
end;

constructor TBytesBody.Create(AData: TBytes; AContentType: String);
begin
  inherited Create(AContentType, True);
  FData := AData;
end;

constructor TBytesBody.Create(AFileName: String);
var
  FileStream: TBytesStream;
begin
  FileStream := TBytesStream.Create;
  try
    FileStream.LoadFromFile(AFileName);
    FileStream.Position := 0;
    inherited Create(GetMimeType(FileStream.Memory, FileStream.Size), True);
    FData := Copy(FileStream.Bytes, 0, FileStream.Size);
  finally
    FileStream.Free;
  end;
end;

function TBytesBody.GetStream: TMemoryStream;
begin
  Result := TBytesStream.Create(FData);
end;

{TStringBody}

constructor TStringBody.Create(AData, AContentType: String);
begin
  inherited Create(AContentType, True);
  FData := TEncoding.UTF8.GetBytes(AData);
  FEncoding := TEncoding.UTF8;
end;

constructor TStringBody.Create(AData: UTF8String; AContentType: String);
begin
  inherited Create(AContentType, True);
  FData := TEncoding.UTF8.GetBytes(String(AData));
  FEncoding := TEncoding.UTF8;
end;

{TUrlEncodedFormBody}

procedure TUrlEncodedFormBody.Add(AName, AValue: String);
begin
  FParts.Add(AName + '=' + AValue);
end;

constructor TUrlEncodedFormBody.Create;
begin
  inherited Create('application/x-www-form-urlencoded', True);
  FParts := TStringList.Create;
end;

destructor TUrlEncodedFormBody.Destroy;
begin
  FParts.Free;
  inherited;
end;

function TUrlEncodedFormBody.GetStream: TMemoryStream;
var
  Text: String;
  i: Integer;
  Stream: TMemoryStream;
begin
  Text := '';
  for i := 0 to FParts.Count - 1 do
  begin
    if i > 0 then
      Text := Text + '&';
    Text := Text + UrlEncode(FParts.Names[i]) + '=' + UrlEncode(FParts.ValueFromIndex[i]);
  end;
  Stream := TBytesStream.Create;
  WriteString(Stream, Text, FEncoding);
  Result := Stream;
end;

{TPart}

constructor TPart.Create(AName: String; AData: TBytes; AContentType: String);
begin
  FName := AName;
  FData := AData;
  FFilename := '';
  FContentType := AContentType;
end;

constructor TPart.CreateFromFile(AName, AFileName: String);
begin
  FName := AName;
  FFilename := ExtractFileName(AFileName);
  with TBytesStream.Create do
    try
      LoadFromFile(AFileName);
      Position := 0;
      FContentType := GetMimeType(Memory, Size);
      FData := Copy(Bytes, 0, Size);
    finally
      Free;
    end;
end;

{TMultipartFormBody}

procedure TMultipartFormBody.Add(AName, AData, AContentType: String);
begin
  FParts.Add(TPart.Create(AName, TEncoding.UTF8.GetBytes(AData), AContentType));
end;

procedure TMultipartFormBody.Add(AName: String; AData: TBytes; AContentType: String);
begin
  FParts.Add(TPart.Create(AName, AData, AContentType));
end;

procedure TMultipartFormBody.Add(AName: String; AData: UTF8String; AContentType: String);
begin
  FParts.Add(TPart.Create(AName, TEncoding.UTF8.GetBytes(String(AData)), AContentType));
end;

procedure TMultipartFormBody.AddFromFile(AName, AFileName: String);
begin
  FParts.Add(TPart.CreateFromFile(AName, AFileName));
end;

constructor TMultipartFormBody.Create;
var
  Boundary: String;
begin
  Boundary := '--------------------HttpClient' + RandomHex;
  inherited Create('multipart/form-data; boundary=' + Boundary, False);
  FBoundary := Boundary;
  FParts := TObjectList<TPart>.Create;
end;

destructor TMultipartFormBody.Destroy;
begin
  FParts.Free;
  inherited;
end;

function TMultipartFormBody.GetPart(AIndex: Integer): TPart;
begin
  Result := FParts[AIndex];
end;

function TMultipartFormBody.GetPartCount: Integer;
begin
  Result := FParts.Count;
end;

function TMultipartFormBody.GetStream: TMemoryStream;
var
  Part: TPart;
  Stream: TMemoryStream;
begin
  Stream := TBytesStream.Create;

  for Part in FParts do
  begin
    WriteString(Stream, '--' + FBoundary + sLineBreak);
    WriteString(Stream, 'Content-Disposition: form-data; name="' + Part.FName + '"');
    if (Part.FFilename <> '') then
      WriteString(Stream, '; filename="' + Part.FFilename + '"');
    WriteString(Stream, sLineBreak);
    if Part.FContentType <> '' then
      WriteString(Stream, 'Content-Type: ' + Part.FContentType + sLineBreak);
    WriteString(Stream, sLineBreak);
    WriteBytes(Stream, Part.FData);
    WriteString(Stream, sLineBreak);
  end;
  WriteString(Stream, '--' + FBoundary + '--' + sLineBreak);

  Result := Stream;
end;

{THttpResponse}

constructor THttpResponse.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaders := THeaders.Create;
end;

destructor THttpResponse.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure THttpResponse.Clear;
begin
  FStatusCode := 0;
  FHeaders.Clear;
  SetLength(FData, 0);
end;

function THttpResponse.GetEncodingForCharset: TEncoding;
var
  ContentTypeHeader, Charset: String;
  CharsetPos: Integer;
begin
  Result := TEncoding.UTF8;
  ContentTypeHeader := LowerCase(GetContentType);
  CharsetPos := Pos('charset=', ContentTypeHeader);
  if CharsetPos = 0 then
    Exit;
  Charset := Copy(ContentTypeHeader, CharsetPos + Length('charset='), MaxInt);
  CharsetPos := Pos(';', Charset);
  if CharsetPos > 0 then
    Charset := Copy(Charset, 1, CharsetPos - 1);
  Charset := Trim(Charset);
  Charset := StringReplace(Charset, '"', '', [rfReplaceAll]);
  Charset := StringReplace(Charset, '''', '', [rfReplaceAll]);
  if (Charset = 'utf-8') or (Charset = 'utf8') then
    Result := TEncoding.UTF8
  else if (Charset = 'us-ascii') or (Charset = 'ascii') then
    Result := TEncoding.ASCII
  else if (Charset = 'iso-8859-1') or (Charset = 'latin1') or (Charset = 'windows-1252') then
    Result := TEncoding.GetEncoding(1252)
  else if (Charset = 'utf-16') or (Charset = 'unicode') then
    Result := TEncoding.Unicode;
end;

function THttpResponse.GetContentAsString: String;
begin
  Result := GetEncodingForCharset.GetString(FData);
end;

function THttpResponse.GetContentLength: Integer;
begin
  Result := StrToIntDef(FHeaders.GetFirst('Content-Length'), -1);
end;

function THttpResponse.GetContentType: String;
begin
  Result := FHeaders.GetFirst('Content-Type');
end;

procedure THttpResponse.SaveToFile(AFileName: String);
begin
  with TBytesStream.Create(FData) do
    try
      SaveToFile(AFileName);
    finally
      Free;
    end;
end;

procedure THttpResponse.SaveToStream(AStream: TStream);
begin
  if Length(FData) > 0 then
    AStream.WriteBuffer(FData[0], Length(FData));
end;

{TCookie}

constructor TCookie.Create(const ASetCookie, ADefaultDomain, ADefaultPath: String);
var
  Attribute: String;
  AttributeName: String;
  AttributeValue: String;
  CookieData: String;
begin
  inherited Create;
  CookieData := ASetCookie;
  FName := Trim(Fetch(CookieData, '='));
  FValue := Trim(Fetch(CookieData, ';'));
  FDomain := LowerCase(ADefaultDomain);
  FPath := GetDefaultPath(ADefaultPath);
  FHostOnly := True;
  FSecure := False;
  FHttpOnly := False;
  FSameSite := csNone;
  FMaxAge := -1;
  FHasMaxAge := False;
  FCreatedAt := Now;

  while CookieData <> '' do
  begin
    Attribute := Trim(Fetch(CookieData, ';'));
    AttributeName := Trim(Fetch(Attribute, '='));
    AttributeValue := Trim(Attribute);
    if SameText(AttributeName, 'Domain') then
    begin
      FDomain := LowerCase(AttributeValue);
      FHostOnly := False;
    end
    else if SameText(AttributeName, 'Path') then
      FPath := AttributeValue
    else if SameText(AttributeName, 'Expires') then
      FHasExpires := TryParseExpires(AttributeValue, FExpires)
    else if SameText(AttributeName, 'Secure') then
      FSecure := True
    else if SameText(AttributeName, 'HttpOnly') then
      FHttpOnly := True
    else if SameText(AttributeName, 'SameSite') then
    begin
      if SameText(AttributeValue, 'Lax') then
        FSameSite := csLax
      else if SameText(AttributeValue, 'Strict') then
        FSameSite := csStrict
      else
        FSameSite := csNone;
    end
    else if SameText(AttributeName, 'Max-Age') then
    begin
      FMaxAge := StrToIntDef(AttributeValue, -1);
      FHasMaxAge := FMaxAge >= 0;
    end;
  end;

  if (FDomain <> '') and (FDomain[1] = '.') then
    Delete(FDomain, 1, 1);
  if (FPath = '') or (FPath[1] <> '/') then
    FPath := '/';
end;

procedure TCookie.AssignFrom(ASource: TCookie);
begin
  FName := ASource.FName;
  FValue := ASource.FValue;
  FDomain := ASource.FDomain;
  FPath := ASource.FPath;
  FExpires := ASource.FExpires;
  FHasExpires := ASource.FHasExpires;
  FHostOnly := ASource.FHostOnly;
  FSecure := ASource.FSecure;
  FHttpOnly := ASource.FHttpOnly;
  FSameSite := ASource.FSameSite;
  FMaxAge := ASource.FMaxAge;
  FHasMaxAge := ASource.FHasMaxAge;
  FCreatedAt := ASource.FCreatedAt;
end;

function TCookie.GetDefaultPath(const ARequestPath: String): String;
var
  PathEnd: Integer;
  RequestPath: String;
begin
  RequestPath := ARequestPath;
  Fetch(RequestPath, '?');
  PathEnd := RPos('/', RequestPath);
  if PathEnd <= 1 then
    Result := '/'
  else
    Result := Copy(RequestPath, 1, PathEnd - 1);
end;

function TCookie.TryParseExpires(const AValue: String; out AExpires: TDateTime): Boolean;
const
  MonthNames: array [1 .. 12] of String = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov',
    'Dec');
var
  DateValue: String;
  Day: Integer;
  Month: Integer;
  Year: Integer;
  Hour: Integer;
  Minute: Integer;
  Second: Integer;
  TimeValue: String;
  TimePart: String;
  MonthIndex: Integer;
  UtcSystemTime: TSystemTime;
  LocalSystemTime: TSystemTime;
begin
  Result := False;
  DateValue := Trim(AValue);
  if Pos(',', DateValue) > 0 then
  begin
    Fetch(DateValue, ',');
    DateValue := Trim(DateValue);
  end;
  Day := StrToIntDef(Fetch(DateValue, ' '), 0);
  TimePart := Fetch(DateValue, ' ');
  Month := 0;
  for MonthIndex := Low(MonthNames) to High(MonthNames) do
    if SameText(TimePart, MonthNames[MonthIndex]) then
    begin
      Month := MonthIndex;
      Break;
    end;
  Year := StrToIntDef(Fetch(DateValue, ' '), 0);
  TimeValue := Fetch(DateValue, ' ');
  Hour := StrToIntDef(Fetch(TimeValue, ':'), -1);
  Minute := StrToIntDef(Fetch(TimeValue, ':'), -1);
  Second := StrToIntDef(TimeValue, -1);
  if not TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0, AExpires) then
    Exit;

  DateTimeToSystemTime(AExpires, UtcSystemTime);
  if SystemTimeToTzSpecificLocalTime(nil, UtcSystemTime, LocalSystemTime) then
    AExpires := SystemTimeToDateTime(LocalSystemTime);
  Result := True;
end;

function TCookie.IsExpired: Boolean;
begin
  if FHasMaxAge then
    Result := (FCreatedAt + FMaxAge / 86400) <= Now
  else
    Result := FHasExpires and (FExpires <= Now);
end;

function TCookie.Matches(const AHost, APath, AScheme: String): Boolean;
var
  Host: String;
  RequestPath: String;
  DomainMatches: Boolean;
  PathMatches: Boolean;
begin
  if FSecure and not SameText(AScheme, 'HTTPS') then
  begin
    Result := False;
    Exit;
  end;

  Host := LowerCase(AHost);
  RequestPath := APath;
  if RequestPath = '' then
    RequestPath := '/';

  if FHostOnly then
    DomainMatches := SameText(Host, FDomain)
  else
    DomainMatches := SameText(Host, FDomain) or ((Length(Host) > Length(FDomain)) and (Copy(Host,
          Length(Host) - Length(FDomain) + 1, Length(FDomain)) = FDomain) and
        (Host[Length(Host) - Length(FDomain)] = '.'));

  PathMatches := (FPath = '/') or SameText(RequestPath, FPath);
  if (not PathMatches) and (Length(RequestPath) > Length(FPath)) then
    PathMatches := (Copy(RequestPath, 1, Length(FPath)) = FPath) and (RequestPath[Length(FPath) + 1] = '/');

  Result := DomainMatches and PathMatches;
end;

function TCookie.ToRequestValue: String;
begin
  Result := FName + '=' + FValue;
end;

{TCookies}

procedure TCookies.AddFromSetCookie(const ASetCookie, ADefaultDomain, ADefaultPath: String);
var
  Cookie: TCookie;
  CookieIndex: Integer;
begin
  Cookie := TCookie.Create(ASetCookie, ADefaultDomain, ADefaultPath);
  try
    if Cookie.Name = '' then
      Exit;

    for CookieIndex := Count - 1 downto 0 do
      if SameText(Items[CookieIndex].Name, Cookie.Name) and SameText(Items[CookieIndex].Domain, Cookie.Domain)
        and SameText(Items[CookieIndex].Path, Cookie.Path) then
        Delete(CookieIndex);

    if not Cookie.IsExpired then
    begin
      Add(Cookie);
      Cookie := nil;
    end;
  finally
    Cookie.Free;
  end;
end;

constructor TCookies.Create;
begin
  inherited Create(True);
end;

procedure TCookies.RemoveExpired;
var
  CookieIndex: Integer;
begin
  for CookieIndex := Count - 1 downto 0 do
    if Items[CookieIndex].IsExpired then
      Delete(CookieIndex);
end;

function TCookies.ToRequestHeaders(const AHost, APath, AScheme: String): String;
var
  Cookie: TCookie;
begin
  RemoveExpired;
  Result := '';
  for Cookie in Self do
    if Cookie.Matches(AHost, APath, AScheme) then
    begin
      if Result <> '' then
        Result := Result + '; ';
      Result := Result + Cookie.ToRequestValue;
    end;
end;

procedure TCookies.AssignFrom(ASource: TCookies);
var
  i: Integer;
begin
  Clear;
  for i := 0 to ASource.Count - 1 do
  begin
    Add(TCookie.Create('', '', ''));
    Items[Count - 1].AssignFrom(ASource.Items[i]);
  end;
end;

{THttpRequest}

constructor THttpRequest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaders := THeaders.Create;
  FCookies := TCookies.Create;
  FTimeout := THttpTimeouts.Create;
  FResponse := THttpResponse.Create(Self);
  FHttpVersion := hv1_1;
  FUserAgent := 'Mozilla/5.0 (compatible, HttpClient)';
  FAutoRedirect := True;
  FMaxRetries := 0;
  FAbortRequested := False;
  FhRequestActive := nil;
  FMaxContentLength := 0;
  FAcceptCompressed := True;
  FProxyMode := pmSystem;
  FProxyUrl := '';
  FAuthScheme := asNone;
  FAuthToken := '';
  FMaxRedirects := 10;
  FCachePolicy := cpDefault;
end;

function THttpRequest.Delete(AUrl: String; ABody: TBody): Boolean;
begin
  Result := Request('DELETE', AUrl, ABody);
end;

function THttpRequest.Delete(AUrl, ABody: String): Boolean;
var
  Body: TBody;
begin
  Body := TStringBody.Create(ABody);
  try
    Result := Request('DELETE', AUrl, Body);
  finally
    Body.Free;
  end;
end;

function THttpRequest.Delete(AUrl: String): Boolean;
begin
  Result := Request('DELETE', AUrl, nil);
end;

destructor THttpRequest.Destroy;
begin
  Abort;
  CloseSession;
  FResponse.Free;
  FHeaders.Free;
  FCookies.Free;
  FTimeout.Free;
  inherited;
end;

function THttpRequest.Get(AUrl: String): Boolean;
begin
  Result := Request('GET', AUrl, nil);
end;

function THttpRequest.Head(AUrl: String): Boolean;
begin
  Result := Request('HEAD', AUrl, nil);
end;

function THttpRequest.Options(AUrl: String): Boolean;
begin
  Result := Request('OPTIONS', AUrl, nil);
end;

function THttpRequest.Patch(AUrl: String; ABody: TBody): Boolean;
begin
  Result := Request('PATCH', AUrl, ABody);
end;

function THttpRequest.Post(AUrl: String; ABody: TBody): Boolean;
begin
  Result := Request('POST', AUrl, ABody);
end;

function THttpRequest.Put(AUrl: String; ABody: TBody): Boolean;
begin
  Result := Request('PUT', AUrl, ABody);
end;

function THttpRequest.BuildOpenRequestFlags(const AURI: THttpURI): Cardinal;
begin
  Result := INTERNET_FLAG_KEEP_CONNECTION;
  if SameText(AURI.Protocol, 'HTTPS') then
    Result := Result or INTERNET_FLAG_SECURE;
  Result := Result or INTERNET_FLAG_NO_COOKIES;
  if not FAutoRedirect then
    Result := Result or INTERNET_FLAG_NO_AUTO_REDIRECT;
  case FCachePolicy of
    cpReload:
      Result := Result or INTERNET_FLAG_RELOAD;
    cpNoStore:
      Result := Result or INTERNET_FLAG_NO_CACHE_WRITE;
  end;
end;

procedure THttpRequest.ApplyTimeouts(ARequest: HINTERNET);
begin
  if FTimeout.FConnectTimeout > 0 then
    InternetSetOption(ARequest, INTERNET_OPTION_CONNECT_TIMEOUT, @FTimeout.FConnectTimeout,
      SizeOf(FTimeout.ConnectTimeout));
  if FTimeout.FSendTimeout > 0 then
    InternetSetOption(ARequest, INTERNET_OPTION_SEND_TIMEOUT, @FTimeout.FSendTimeout, SizeOf(FTimeout.SendTimeout));
  if FTimeout.FReceiveTimeout > 0 then
    InternetSetOption(ARequest, INTERNET_OPTION_RECEIVE_TIMEOUT, @FTimeout.FReceiveTimeout,
      SizeOf(FTimeout.ReceiveTimeout));
end;

procedure THttpRequest.ApplySecurityFlags(ARequest: HINTERNET);
var
  SecurityFlags, dwBuffLen: Cardinal;
  SecurityOption: TSecurityOption;
begin
  if FSecurityOptions = [] then
    Exit;

  dwBuffLen := SizeOf(SecurityFlags);
  if not InternetQueryOption(ARequest, INTERNET_OPTION_SECURITY_FLAGS, @SecurityFlags, dwBuffLen) then
    raise Exception.Create(GetErrorDescription(GetLastError));

  for SecurityOption in FSecurityOptions do
    case SecurityOption of
      soSecure:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_SECURE;
      soSsl:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_SSL;
      soSsl3:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_SSL3;
      soPct:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_PCT;
      soPct4:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_PCT4;
      soIetfssl4:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_IETFSSL4;
      so40bit:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_40BIT;
      so128bit:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_128BIT;
      so56bit:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_56BIT;
      soUnknownbit:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_UNKNOWNBIT;
      soIgnoreRevication:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_IGNORE_REVOCATION;
      soIgnoreUnknownCA:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_IGNORE_UNKNOWN_CA;
      soIgnoreWrongUsage:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_IGNORE_WRONG_USAGE;
      soIgnoreCertCNInvalid:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_IGNORE_CERT_CN_INVALID;
      soIgnoreCertDateInvalid:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;
      soIgnoreRedirectHttps:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS;
      soIgnoreRedirectHttp:
        SecurityFlags := SecurityFlags or SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP;
    end;

  if not InternetSetOption(ARequest, INTERNET_OPTION_SECURITY_FLAGS, @SecurityFlags, SizeOf(SecurityFlags)) then
    raise Exception.Create(GetErrorDescription(GetLastError));
end;

function THttpRequest.BuildRequestHeaders(const AURI: THttpURI; ABody: TBody; ABodyStream: TMemoryStream): String;
var
  TmpHead: TStringBuilder;
  CookieHeaders: String;
  HostHeader: String;
  AuthHeader: String;
begin
  TmpHead := TStringBuilder.Create;
  try
    HostHeader := AURI.Host;
    if AURI.IPVersion = ivIP6 then
      HostHeader := '[' + HostHeader + ']';
    if (AURI.Port <> 0) and not ((SameText(AURI.Protocol, 'HTTP') and (AURI.Port = 80)) or
      (SameText(AURI.Protocol, 'HTTPS') and (AURI.Port = 443))) then
      HostHeader := HostHeader + ':' + IntToStr(AURI.Port);
    TmpHead.Append('Host: ' + HostHeader + sLineBreak);

    if FAcceptCompressed then
      TmpHead.Append('Accept-Encoding: gzip, deflate' + sLineBreak);

    case FAuthScheme of
      asBearer:
        if FAuthToken <> '' then
          TmpHead.Append('Authorization: Bearer ' + FAuthToken + sLineBreak);
      asBasic:
        if (FUsername <> '') or (FPassword <> '') then
        begin
          AuthHeader := EncodeString(FUsername + ':' + FPassword);
          TmpHead.Append('Authorization: Basic ' + AuthHeader + sLineBreak);
        end;
    end;

    if FUseCookies then
    begin
      CookieHeaders := FCookies.ToRequestHeaders(AURI.Host, AURI.GetPathAndParams, AURI.Protocol);
      if CookieHeaders <> '' then
        TmpHead.Append('Cookie: ' + CookieHeaders + sLineBreak);
    end;

    if Assigned(ABody) then
    begin
      TmpHead.Append('Content-Type: ' + ABody.ContentType + sLineBreak);
      if ABody.FNeedLength then
        TmpHead.Append('Content-Length: ' + IntToStr(ABodyStream.Size) + sLineBreak);
    end;

    TmpHead.Append(FHeaders.ToString);

    Result := TmpHead.ToString;
  finally
    TmpHead.Free;
  end;
end;

procedure THttpRequest.SendRequestToServer(ARequest: HINTERNET; const AHeaders: String; ABodyStream: TMemoryStream);
begin
  if not HttpSendRequest(ARequest, PChar(AHeaders), Length(AHeaders), ABodyStream.Memory, ABodyStream.Size) then
    raise Exception.Create(GetErrorDescription(GetLastError));
end;

function THttpRequest.ReadResponseStatusCode(ARequest: HINTERNET): Integer;
var
  Buffer, BufferLength, Reserved: Cardinal;
begin
  BufferLength := SizeOf(Buffer);
  Reserved := 0;
  if not HttpQueryInfo(ARequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @Buffer, BufferLength, Reserved) then
    raise Exception.Create(GetErrorDescription(GetLastError));
  Result := Integer(Buffer);
end;

function THttpRequest.ReadRawHeaders(ARequest: HINTERNET): String;
var
  BufferLength, Reserved: Cardinal;
  Buffer: PChar;
begin
  BufferLength := 2048;
  Reserved := 0;
  repeat
    Buffer := StrAlloc(BufferLength div SizeOf(Char) + 1);
    try
      if HttpQueryInfo(ARequest, HTTP_QUERY_RAW_HEADERS_CRLF, Buffer, BufferLength, Reserved) then
      begin
        Result := Buffer;
        Exit;
      end;
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        raise Exception.Create(GetErrorDescription(GetLastError));
    finally
      StrDispose(Buffer);
    end;
  until False;
end;

procedure THttpRequest.ReadResponseBody(ARequest: HINTERNET);
var
  BytesAvailable, BytesRead, TotalRead: Cardinal;
  Buffer: TBytes;
  ContentLength: Integer;
  Stream: TMemoryStream;
begin
  ContentLength := FResponse.GetContentLength;

  if Assigned(FOnProgress) then
    FOnProgress(Self, 0, ContentLength);

  Stream := TMemoryStream.Create;
  try
    TotalRead := 0;
    if not InternetQueryDataAvailable(ARequest, BytesAvailable, 0, 0) then
      raise Exception.Create(GetErrorDescription(GetLastError));

    while BytesAvailable > 0 do
    begin
      if FAbortRequested then
        raise Exception.Create('Request aborted by the user');

      SetLength(Buffer, BytesAvailable);
      if not InternetReadFile(ARequest, @Buffer[0], BytesAvailable, BytesRead) then
        raise Exception.Create(GetErrorDescription(GetLastError));

      if BytesRead = 0 then
        Break;

      if (FMaxContentLength > 0) and (TotalRead + BytesRead > FMaxContentLength) then
        raise Exception.Create('Response body exceeds MaxContentLength (' +
          IntToStr(FMaxContentLength) + ' bytes)');

      Stream.WriteBuffer(Buffer[0], BytesRead);
      TotalRead := TotalRead + BytesRead;

      if Assigned(FOnProgress) then
        FOnProgress(Self, TotalRead, ContentLength);

      if not InternetQueryDataAvailable(ARequest, BytesAvailable, 0, 0) then
        raise Exception.Create(GetErrorDescription(GetLastError));
    end;

    SetLength(FResponse.FData, Stream.Size);
    if Stream.Size > 0 then
    begin
      Stream.Position := 0;
      Stream.ReadBuffer(FResponse.FData[0], Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

procedure THttpRequest.ExtractCookiesFromResponse(const AHost, APath, AScheme: String);
var
  SetCookie: String;
  CookieList: TStrings;
begin
  if not FUseCookies then
    Exit;

  CookieList := FResponse.FHeaders.GetAll('Set-Cookie');
  try
    for SetCookie in CookieList do
      FCookies.AddFromSetCookie(SetCookie, AHost, APath);
  finally
    CookieList.Free;
  end;
end;

procedure THttpRequest.CloseInternetHandle(var AHandle: HINTERNET);
begin
  if AHandle <> nil then
  begin
    InternetCloseHandle(AHandle);
    AHandle := nil;
  end;
end;

procedure THttpRequest.CloseConnect;
begin
  CloseInternetHandle(FhConnect);
  FConnectHost := '';
  FConnectPort := 0;
  FConnectUser := '';
  FConnectPassword := '';
end;

procedure THttpRequest.CloseSession;
begin
  CloseConnect;
  CloseInternetHandle(FhInet);
  FSessionUserAgent := '';
  FSessionProxyMode := pmSystem;
  FSessionProxyUrl := '';
end;

procedure THttpRequest.EnsureSession;
begin
  if (FhInet <> nil) and (FSessionUserAgent = FUserAgent) and (FSessionProxyMode = FProxyMode) and
    (FSessionProxyUrl = FProxyUrl) then
    Exit;

  CloseSession;
  case FProxyMode of
    pmSystem:
      FhInet := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    pmDirect:
      FhInet := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
    pmExplicit:
      if FProxyUrl <> '' then
        FhInet := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PROXY, PChar(FProxyUrl), nil, 0)
      else
        FhInet := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  end;
  if FhInet = nil then
    raise Exception.Create(GetErrorDescription(GetLastError));
  FSessionUserAgent := FUserAgent;
  FSessionProxyMode := FProxyMode;
  FSessionProxyUrl := FProxyUrl;
end;

procedure THttpRequest.EnsureConnect(const AURI: THttpURI);
begin
  if (FhConnect <> nil) and SameText(FConnectHost, AURI.Host) and (FConnectPort = AURI.Port) and
    (FConnectUser = FUsername) and (FConnectPassword = FPassword) then
    Exit;

  CloseConnect;
  FhConnect := InternetConnect(FhInet, PChar(AURI.Host), AURI.Port, PChar(FUsername), PChar(FPassword),
    INTERNET_SERVICE_HTTP, 0, 0);
  if FhConnect = nil then
    raise Exception.Create(GetErrorDescription(GetLastError));

  FConnectHost := AURI.Host;
  FConnectPort := AURI.Port;
  FConnectUser := FUsername;
  FConnectPassword := FPassword;
end;

procedure THttpRequest.CaptureLastUrl(ARequest: HINTERNET; const AFallback: String);
var
  Buffer: array [0 .. 2083] of Char;
  BufferLength: Cardinal;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  BufferLength := SizeOf(Buffer);
  if InternetQueryOption(ARequest, INTERNET_OPTION_URL, @Buffer[0], BufferLength) and (Buffer[0] <> #0) then
    FLastUrl := PChar(@Buffer[0])
  else
    FLastUrl := AFallback;
end;

function THttpRequest.Request(AMethod, AUrl: String; ABody: TBody): Boolean;
var
  hRequest: HINTERNET;
  IdURI: THttpURI;
  BodyStream: TMemoryStream;
  RequestHeaders: String;
  StatusCode: Integer;
  OpenRequestFlags: Cardinal;
  Attempt: Integer;
  RetryDelay: Integer;
  BodyReleased: Boolean;
  RedirectCount: Integer;
  CurrentUrl: String;
  RedirectUrl: String;
const
  HTTP_VERSION: array [THttpVersion] of PChar = ('HTTP/1.0', 'HTTP/1.1');

begin
  Result := False;
  BodyReleased := False;
  RedirectCount := 0;
  CurrentUrl := AUrl;

  FResponse.Clear;
  FLastUrl := AUrl;
  FAbortRequested := False;

  if Assigned(FOnBeforeRequest) then
    FOnBeforeRequest(Self, AMethod, AUrl);

  while True do
  begin
    if FAbortRequested then
      raise Exception.Create('Request aborted by the user');

    IdURI := THttpURI.Create(CurrentUrl);
    try
      if not(SameText(IdURI.Protocol, 'HTTP') or SameText(IdURI.Protocol, 'HTTPS')) then
        raise Exception.Create('Only HTTP and HTTPS URLs are supported');
      if IdURI.Host = '' then
        raise Exception.Create('URL host is empty');

      EnsureSession;
      EnsureConnect(IdURI);

      OpenRequestFlags := BuildOpenRequestFlags(IdURI);

      Attempt := 0;
      while True do
      begin
        Inc(Attempt);

        if FAbortRequested then
          raise Exception.Create('Request aborted by the user');

        hRequest := HttpOpenRequest(FhConnect, PChar(AMethod), PChar(IdURI.GetPathAndParams),
          HTTP_VERSION[FHttpVersion], '', nil, OpenRequestFlags, 0);
        if hRequest = nil then
        begin
          CloseConnect;
          raise Exception.Create(GetErrorDescription(GetLastError));
        end;
        FhRequestActive := hRequest;

        try
          ApplyTimeouts(hRequest);
          ApplySecurityFlags(hRequest);

          if Assigned(ABody) then
            BodyStream := ABody.GetStream
          else
            BodyStream := TMemoryStream.Create;
          try
            RequestHeaders := BuildRequestHeaders(IdURI, ABody, BodyStream);
            try
              SendRequestToServer(hRequest, RequestHeaders, BodyStream);
            except
              if Assigned(ABody) and ABody.ReleaseAfterSend then
              begin
                ABody.Free;
                BodyReleased := True;
              end;
              raise;
            end;
          finally
            BodyStream.Free;
          end;

          if Assigned(ABody) and ABody.ReleaseAfterSend and (not BodyReleased) then
          begin
            ABody.Free;
            BodyReleased := True;
          end;

          StatusCode := ReadResponseStatusCode(hRequest);
          FResponse.FStatusCode := StatusCode;
          FResponse.FHeaders.FromRawString(ReadRawHeaders(hRequest));

          if (Attempt <= FMaxRetries) and IsIdempotentMethod(AMethod) and (not BodyReleased) and
            ((StatusCode >= 500) or (StatusCode = 408)) then
          begin
            RetryDelay := GetRetryDelaySeconds(hRequest);
            FResponse.Clear;
            if RetryDelay > 0 then
              Sleep(RetryDelay * 1000);
            Continue;
          end;

          if not FAutoRedirect and (StatusCode >= 300) and (StatusCode < 400) then
          begin
            Inc(RedirectCount);
            if RedirectCount > FMaxRedirects then
              raise Exception.Create('Too many redirects (max ' + IntToStr(FMaxRedirects) + ')');

            RedirectUrl := FResponse.FHeaders.GetFirst('Location');
            if RedirectUrl = '' then
              raise Exception.Create('Redirect (' + IntToStr(StatusCode) + ') without Location header');

            if (Pos('://', RedirectUrl) = 0) and not SameText(Copy(RedirectUrl, 1, 2), '//') then
            begin
              if Copy(RedirectUrl, 1, 1) = '/' then
                RedirectUrl := IdURI.Protocol + '://' + IdURI.Host + RedirectUrl
              else
                RedirectUrl := IdURI.Protocol + '://' + IdURI.Host + '/' + RedirectUrl;
            end;

            if (StatusCode = 303) and not SameText(AMethod, 'GET') then
              AMethod := 'GET';

            CurrentUrl := RedirectUrl;
            FLastUrl := RedirectUrl;
            Break;
          end;

          Result := (StatusCode >= 200) and (StatusCode < 300);

          ReadResponseBody(hRequest);

          ExtractCookiesFromResponse(IdURI.Host, IdURI.GetPathAndParams, IdURI.Protocol);
          CaptureLastUrl(hRequest, CurrentUrl);
          Break;
        except
          InternetCloseHandle(hRequest);
          FhRequestActive := nil;
          CloseConnect;
          raise;
        end;
        InternetCloseHandle(hRequest);
        FhRequestActive := nil;
      end;

      if not FAutoRedirect and (FResponse.FStatusCode >= 300) and (FResponse.FStatusCode < 400) then
        Continue;

      Break;
    finally
      IdURI.Free;
    end;
  end;

  if Assigned(FOnAfterResponse) then
    FOnAfterResponse(Self);
end;

procedure THttpRequest.SetAcceptCompressed(AValue: Boolean);
begin
  FAcceptCompressed := AValue;
end;

procedure THttpRequest.SetUseCookies(AValue: Boolean);
begin
  FUseCookies := AValue;
  if not AValue then
    FCookies.Clear;
end;

procedure THttpRequest.SetUserAgent(const AValue: String);
begin
  if FUserAgent = AValue then
    Exit;
  FUserAgent := AValue;
  CloseSession;
end;

procedure THttpRequest.SetUsername(const AValue: String);
begin
  if FUsername = AValue then
    Exit;
  FUsername := AValue;
  CloseConnect;
end;

procedure THttpRequest.SetPassword(const AValue: String);
begin
  if FPassword = AValue then
    Exit;
  FPassword := AValue;
  CloseConnect;
end;

procedure THttpRequest.SetProxyMode(AValue: THttpProxyMode);
begin
  if FProxyMode = AValue then
    Exit;
  FProxyMode := AValue;
  CloseSession;
end;

procedure THttpRequest.SetProxyUrl(const AValue: String);
begin
  if FProxyUrl = AValue then
    Exit;
  FProxyUrl := AValue;
  CloseSession;
end;

procedure THttpRequest.SetAuthScheme(AValue: THttpAuthScheme);
begin
  FAuthScheme := AValue;
end;

procedure THttpRequest.SetMaxRedirects(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  FMaxRedirects := AValue;
end;

procedure THttpRequest.SetCachePolicy(AValue: THttpCachePolicy);
begin
  FCachePolicy := AValue;
end;

function THttpRequest.Patch(AUrl, ABody: String): Boolean;
var
  Body: TBody;
begin
  Body := TStringBody.Create(ABody);
  try
    Result := Patch(AUrl, Body);
  finally
    Body.Free;
  end;
end;

function THttpRequest.Patch(AUrl: String): Boolean;
begin
  Result := Patch(AUrl, nil);
end;

function THttpRequest.Post(AUrl, ABody: String): Boolean;
var
  Body: TBody;
begin
  Body := TStringBody.Create(ABody);
  try
    Result := Post(AUrl, Body);
  finally
    Body.Free;
  end;
end;

function THttpRequest.Post(AUrl: String): Boolean;
begin
  Result := Post(AUrl, nil);
end;

function THttpRequest.Put(AUrl: String): Boolean;
begin
  Result := Put(AUrl, nil);
end;

function THttpRequest.Put(AUrl, ABody: String): Boolean;
var
  Body: TBody;
begin
  Body := TStringBody.Create(ABody);
  try
    Result := Put(AUrl, Body);
  finally
    Body.Free;
  end;
end;

procedure THttpRequest.Abort;
begin
  FAbortRequested := True;
  if FhRequestActive <> nil then
  begin
    InternetCloseHandle(FhRequestActive);
    FhRequestActive := nil;
  end;
end;

function THttpRequest.IsIdempotentMethod(const AMethod: String): Boolean;
begin
  Result := SameText(AMethod, 'GET') or SameText(AMethod, 'HEAD') or SameText(AMethod, 'OPTIONS')
    or SameText(AMethod, 'DELETE') or SameText(AMethod, 'TRACE') or SameText(AMethod, 'PUT');
end;

function THttpRequest.GetRetryDelaySeconds(ARequest: HINTERNET): Integer;
var
  Buffer: array [0 .. 255] of Char;
  BufferLength: Cardinal;
  RetryAfter: String;
  Delay: Integer;
  RetryDate: TDateTime;
  UtcNow: TDateTime;
  SystemTime: TSystemTime;
  Reserved: Cardinal;
begin
  Result := 0;
  FillChar(Buffer, SizeOf(Buffer), 0);
  BufferLength := SizeOf(Buffer);
  if HttpQueryInfo(ARequest, HTTP_QUERY_RETRY_AFTER, @Buffer[0], BufferLength, Reserved) then
  begin
    RetryAfter := Trim(PChar(@Buffer[0]));
    if RetryAfter <> '' then
    begin
      Delay := StrToIntDef(RetryAfter, -1);
      if Delay >= 0 then
        Result := Delay
      else if ParseHttpDate(RetryAfter, RetryDate) then
      begin
        // RetryDate is UTC; compute UTC now without TTimeZone (D2010 compatible).
        GetSystemTime(SystemTime);
        UtcNow := SystemTimeToDateTime(SystemTime);
        Result := Max(0, SecondsBetween(RetryDate, UtcNow));
      end;
    end;
  end;
end;

function THttpRequest.Trace(AUrl: String): Boolean;
begin
  Result := Request('TRACE', AUrl, nil);
end;

{THttpURI}

constructor THttpURI.Create(const AURI: String = ''); {Do not Localize}
begin
  inherited Create;
  if Length(AURI) > 0 then
  begin
    URI := AURI;
  end;
end;

procedure THttpURI.SetProtocol(const Value: String);
begin
  FProtocol := Value;
  if FPort = 0 then
  begin
    if SameText(FProtocol, 'HTTP') then
      FPort := 80
    else if SameText(FProtocol, 'HTTPS') then
      FPort := 443
    else if SameText(FProtocol, 'FTP') then
      FPort := 21;
  end;
end;

procedure THttpURI.SetURI(const Value: String);
var
  LBuffer: String;
  LTokenPos, Port: Integer;
  LURI: String;
  AuthInfo: String;
begin
  FURI := Value;
  FURI := StringReplace(FURI, '\', '/', [rfReplaceAll]);
  LURI := FURI;
  FHost := '';
  FProtocol := '';
  FPath := '';
  FDocument := '';
  FPort := 0;
  FBookmark := '';
  FUsername := '';
  FPassword := '';
  FParams := '';
  FIPVersion := ivIP4;

  LTokenPos := Pos('://', LURI);
  if LTokenPos > 0 then
  begin
    SetProtocol(Copy(LURI, 1, LTokenPos - 1));
    Delete(LURI, 1, LTokenPos + 2);
    LTokenPos := Pos('?', LURI);
    if LTokenPos > 0 then
    begin
      FParams := Copy(LURI, LTokenPos + 1, MaxInt);
      LURI := Copy(LURI, 1, LTokenPos - 1);
    end;
    LBuffer := Fetch(LURI, '/');
    LTokenPos := Pos('@', LBuffer);
    if LTokenPos > 0 then
    begin
      AuthInfo := Copy(LBuffer, 1, LTokenPos - 1);
      Delete(LBuffer, 1, LTokenPos);
      FUsername := Fetch(AuthInfo, ':');
      if Length(FUsername) = 0 then
        FPassword := ''
      else
        FPassword := AuthInfo;
    end;
    if (Pos('[', LBuffer) > 0) and (Pos(']', LBuffer) > Pos('[', LBuffer)) then
    begin
      FHost := Fetch(LBuffer, ']');
      Fetch(FHost, '[');
      Fetch(LBuffer, ':');
      FIPVersion := ivIP6;
    end
    else
    begin
      FHost := Fetch(LBuffer, ':');
    end;
    if TryStrToInt(LBuffer, Port) then
      FPort := Port;
    LTokenPos := RPos('/', LURI, -1);
    if LTokenPos > 0 then
    begin
      FPath := Copy(LURI, 1, LTokenPos);
      Delete(LURI, 1, LTokenPos);
    end
    else
    begin
      FPath := '/';
    end;
  end
  else
  begin
    LTokenPos := Pos('?', LURI);
    if LTokenPos > 0 then
    begin
      FParams := Copy(LURI, LTokenPos + 1, MaxInt);
      LURI := Copy(LURI, 1, LTokenPos - 1);
    end;
    LTokenPos := RPos('/', LURI, -1);
    if LTokenPos > 0 then
    begin
      FPath := Copy(LURI, 1, LTokenPos);
      Delete(LURI, 1, LTokenPos);
    end;
  end;
  FDocument := LURI;
  FBookmark := FDocument;
  FDocument := Fetch(FBookmark, '#');
end;

function THttpURI.GetURI: String;
begin
  Result := GetFullURI([]);
end;

function THttpURI.GetFullURI(const AOptionalFields: TURIOptionalFieldsSet): String;
var
  LURI: String;
begin
  if FProtocol = '' then
    raise Exception.Create('Protocol field is empty');

  if FHost = '' then
    raise Exception.Create('Host field is empty');

  LURI := FProtocol + '://';

  if (FUsername <> '') and (ofAuthInfo in AOptionalFields) then
  begin
    LURI := LURI + FUsername;
    if FPassword <> '' then
    begin
      LURI := LURI + ':' + FPassword;
    end;
    LURI := LURI + '@';
  end;

  if FIPVersion = ivIP6 then
    LURI := LURI + '[' + FHost + ']'
  else
    LURI := LURI + FHost;

  if FPort <> 0 then
  begin
    if SameText(FProtocol, 'HTTP') then
    begin
      if FPort <> 80 then
        LURI := LURI + ':' + IntToStr(FPort);
    end
    else if SameText(FProtocol, 'HTTPS') then
    begin
      if FPort <> 443 then
        LURI := LURI + ':' + IntToStr(FPort);
    end
    else if SameText(FProtocol, 'FTP') then
    begin
      if FPort <> 21 then
        LURI := LURI + ':' + IntToStr(FPort);
    end
    else
      LURI := LURI + ':' + IntToStr(FPort);
  end;

  LURI := LURI + GetPathAndParams;

  if (FBookmark <> '') and (ofBookmark in AOptionalFields) then
    LURI := LURI + '#' + FBookmark;

  Result := LURI;
end;

function THttpURI.GetPathAndParams: String;
begin
  Result := FPath + FDocument;
  if FParams <> '' then
    Result := Result + '?' + FParams;
end;

{THttpTimeouts}

constructor THttpTimeouts.Create;
begin
  inherited;
  FConnectTimeout := 0;
  FSendTimeout := 0;
  FReceiveTimeout := 0;
end;

procedure THttpTimeouts.Assign(Source: TPersistent);
begin
  if Source is THttpTimeouts then
  begin
    FConnectTimeout := THttpTimeouts(Source).ConnectTimeout;
    FSendTimeout := THttpTimeouts(Source).SendTimeout;
    FReceiveTimeout := THttpTimeouts(Source).ReceiveTimeout;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure THttpTimeouts.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THttpTimeouts.SetConnectTimeout(Value: Cardinal);
begin
  if FConnectTimeout <> Value then
  begin
    FConnectTimeout := Value;
    Changed;
  end;
end;

procedure THttpTimeouts.SetReceiveTimeout(Value: Cardinal);
begin
  if FReceiveTimeout <> Value then
  begin
    FReceiveTimeout := Value;
    Changed;
  end;
end;

procedure THttpTimeouts.SetSendTimeout(Value: Cardinal);
begin
  if FSendTimeout <> Value then
  begin
    FSendTimeout := Value;
    Changed;
  end;
end;

end.