unit HttpClasses;

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  THeader = class
  private
    FName: String;
    FValue: String;
  public
    constructor Create(AName, AValue: String);
    property Name: String read FName;
    property Value: String read FValue;
  end;

  THeaders = class(TObjectList<THeader>)
  private
    procedure FromRawString(AStringHeaders: String);
  public
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
    constructor Create(AData: String; AContentType: String = 'text/plain'); reintroduce; overload;
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
    FParts: TList<TPart>;
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

  THttpResponse = class(TComponent)
  private
    FStatusCode: Cardinal;
    FHeaders: THeaders;
    FData: TBytes;
    function GetContentLenght: Integer;
    function GetContentType: String;
    function GetContentAsString: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile(AFileName: String);
    property StatusCode: Cardinal read FStatusCode;
    property Content: TBytes read FData;
    property ContentAsString: String read GetContentAsString;
    property ContentType: String read GetContentType;
    property ContentLenght: Integer read GetContentLenght;
    property Headers: THeaders read FHeaders;
  end;

  // reserved for future implementation
  TCookies = class(TStringList)
  end;

  TSecurityOption = (soSecure, soSsl, soSsl3, soPct, soPct4, soIetfssl4, so40bit, so128bit, so56bit, soUnknownbit,
    soIgnoreRevication, soIgnoreUnknownCA, soIgnoreWrongUsage, soIgnoreCertCNInvalid, soIgnoreCertDateInvalid,
    soIgnoreRedirectHttps, soIgnoreRedirectHttp);

  TSecurityOptions = set of TSecurityOption;

  THttpVersion = (hv1_0, hv1_1);

  THttpRequest = class(TComponent)
  private
    FUserAgent: String;
    FHeaders: THeaders;
    FUseCookies: Boolean;
    FCookies: TCookies;
    FSecurityOptions: TSecurityOptions;
    FResponse: THttpResponse;
    FHttpVersion: THttpVersion;
    procedure SetUseCookies(AValue: Boolean);
    function Request(AMethod, AUrl: String; ABody: TBody): Boolean;
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
    property Cookies: TCookies read FCookies;
    property Response: THttpResponse read FResponse;
  published
    property Headers: THeaders read FHeaders write FHeaders;
    property UseCookies: Boolean read FUseCookies write SetUseCookies default False;
    property SecurityOptions: TSecurityOptions read FSecurityOptions write FSecurityOptions;
    property UserAgent: String read FUserAgent write FUserAgent;
    property HttpVersion: THttpVersion read FHttpVersion write FHttpVersion default hv1_1;
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
    FUserName: string;
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
    //
    property Bookmark: String read FBookmark write FBookmark;
    property Document: String read FDocument write FDocument;
    property Host: String read FHost write FHost;
    property Password: String read FPassword write FPassword;
    property Path: String read FPath write FPath;
    property Params: String read FParams write FParams;
    property Port: Integer read FPort write FPort;
    property Protocol: String read FProtocol write SetProtocol;
    property URI: String read GetURI write SetURI;
    property Username: String read FUserName write FUserName;
    property IPVersion: TIPVersion read FIPVersion write FIPVersion;
  end;

procedure Register;

implementation

uses
  Windows, IOUtils, UrlMon, WinInet, DateUtils, HttpUtils;

procedure Register;
begin
  Classes.RegisterComponents('HttpClient', [THttpRequest]);
end;

{THeader}

constructor THeader.Create(AName, AValue: String);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
end;

{THeaders}

procedure THeaders.AddHeader(AName, AValue: String);
begin
  Add(THeader.Create(AName, AValue));
end;

procedure THeaders.FromRawString(AStringHeaders: String);
var
  i: Integer;
  Name, Value: String;
begin
  with TStringList.Create do
    try
      Delimiter := #10;
      NameValueSeparator := ':';
      StrictDelimiter := True;
      DelimitedText := AStringHeaders;
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
  Header: THeader;
begin
  Result := TStringList.Create;

  for Header in Self do
    if Header.Name = AName then
      Result.Add(Header.Value);
end;

function THeaders.GetFirst(AName: String): String;
var
  Header: THeader;
begin
  for Header in Self do
    if Header.Name = AName then
    begin
      Result := Header.Value;
      Exit;
    end;
end;

function THeaders.ToString: String;
var
  Header: THeader;
begin
  Result := '';
  for Header in Self do
    Result := Result + Header.Name + ': ' + Header.Value + sLineBreak;
  Result := Trim(Result);
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
  Stream: TStream;
begin
  Stream := GetStream;
  with TStringStream.Create do
    try
      CopyFrom(Stream, Stream.Size);
      Result := DataString;
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
begin
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

function TBytesBody.GetStream: TMemoryStream;
begin
  Result := TBytesStream.Create(FData);
end;

{TStringBody}

constructor TStringBody.Create(AData, AContentType: String);
begin
  inherited Create(AContentType, True);
  FData := TEncoding.ASCII.GetBytes(AData);
end;

constructor TStringBody.Create(AData: UTF8String; AContentType: String);
begin
  inherited Create(AContentType, True);
  FData := TEncoding.UTF8.GetBytes(String(AData));
end;

{TUrlEncodedFormBody}

procedure TUrlEncodedFormBody.Add(AName, AValue: String);
begin
  FParts.Values[AName] := AValue;
end;

constructor TUrlEncodedFormBody.Create;
begin
  inherited Create('application/x-www-form-urlencoded', True);
  FParts := TStringList.Create;
  FParts.Delimiter := '&';
  FParts.NameValueSeparator := '=';
  FParts.StrictDelimiter := True;
end;

destructor TUrlEncodedFormBody.Destroy;
begin
  FParts.Free;
  inherited;
end;

function TUrlEncodedFormBody.GetStream: TMemoryStream;
var
  Body: TBytes;
  Stream: TMemoryStream;
begin
  Stream := TBytesStream.Create;
  Body := FEncoding.GetBytes(PathEncode(FParts.DelimitedText));
  WriteBytes(Stream, Body);
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
  FParts.Add(TPart.Create(AName, TEncoding.ASCII.GetBytes(AData), AContentType));
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
  WriteString(Stream, '--' + FBoundary + '--');

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

function THttpResponse.GetContentAsString: String;
begin
  Result := TEncoding.ASCII.GetString(FData);
end;

function THttpResponse.GetContentLenght: Integer;
begin
  Result := StrToIntDef(FHeaders.GetFirst('Content-Lenght'), -1);
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

{THttpRequest}

constructor THttpRequest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaders := THeaders.Create;
  FCookies := TCookies.Create;
  FHttpVersion := hv1_1;
  FUserAgent := 'Mozilla/5.0 (compatible, HttpClient)';
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
  FHeaders.Free;
  FCookies.Free;
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

function THttpRequest.Request(AMethod, AUrl: String; ABody: TBody): Boolean;
var
  hInet: HINTERNET;
  hConnect: HINTERNET;
  hRequest: HINTERNET;
  lpdwBufferLength: Cardinal;
  lpdwReserved: Cardinal;
  IdURI: THttpURI;
  lpdwNumberOfBytesAvailable: Cardinal;
  dwBytesRead: Cardinal;
  Response: TBytes;
  Headers: PChar;
  Body: TMemoryStream;
  Cookie: String;
  dwFlags, dwBuffLen: Cardinal;
  SecurityOption: TSecurityOption;
  TmpHead: TStringBuilder;
const
  HTTP_VERSION: array [THttpVersion] of PChar = ('HTTP/1.0', 'HTTP/1.1');

begin
  Result := False;

  FResponse := THttpResponse.Create(Self);

  hInet := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  IdURI := THttpURI.Create(AUrl);
  try
    hConnect := InternetConnect(hInet, PChar(IdURI.Host), IdURI.Port, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
    try
      hRequest := HttpOpenRequest(hConnect, PChar(AMethod), PChar(IdURI.GetPathAndParams), HTTP_VERSION[FHttpVersion],
        '', nil, INTERNET_FLAG_SECURE, 0);

      dwBuffLen := SizeOf(dwFlags);
      if FSecurityOptions <> [] then
        if InternetQueryOption(hRequest, INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, dwBuffLen) then
        begin
          dwFlags := 0;
          for SecurityOption in FSecurityOptions do
            case SecurityOption of
              soSecure:
                dwFlags := dwFlags or SECURITY_FLAG_SECURE;
              soSsl:
                dwFlags := dwFlags or SECURITY_FLAG_SSL;
              soSsl3:
                dwFlags := dwFlags or SECURITY_FLAG_SSL3;
              soPct:
                dwFlags := dwFlags or SECURITY_FLAG_PCT;
              soPct4:
                dwFlags := dwFlags or SECURITY_FLAG_PCT4;
              soIetfssl4:
                dwFlags := dwFlags or SECURITY_FLAG_IETFSSL4;
              so40bit:
                dwFlags := dwFlags or SECURITY_FLAG_40BIT;
              so128bit:
                dwFlags := dwFlags or SECURITY_FLAG_128BIT;
              so56bit:
                dwFlags := dwFlags or SECURITY_FLAG_56BIT;
              soUnknownbit:
                dwFlags := dwFlags or SECURITY_FLAG_UNKNOWNBIT;
              soIgnoreRevication:
                dwFlags := dwFlags or SECURITY_FLAG_IGNORE_REVOCATION;
              soIgnoreUnknownCA:
                dwFlags := dwFlags or SECURITY_FLAG_IGNORE_UNKNOWN_CA;
              soIgnoreWrongUsage:
                dwFlags := dwFlags or SECURITY_FLAG_IGNORE_WRONG_USAGE;
              soIgnoreCertCNInvalid:
                dwFlags := dwFlags or SECURITY_FLAG_IGNORE_CERT_CN_INVALID;
              soIgnoreCertDateInvalid:
                dwFlags := dwFlags or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;
              soIgnoreRedirectHttps:
                dwFlags := dwFlags or SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS;
              soIgnoreRedirectHttp:
                dwFlags := dwFlags or SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP;
            end;
          InternetSetOption(hRequest, INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, SizeOf(dwFlags));
        end
        else
          raise Exception.Create(GetErrorDescription(GetLastError));

      TmpHead := TStringBuilder.Create;
      try
        TmpHead.Append('Host: ' + IdURI.Host + sLineBreak);

        if FUseCookies then
          for Cookie in FCookies do
            TmpHead.Append('Cookie: ' + Cookie + sLineBreak);

        if Assigned(ABody) then
        begin
          Body := ABody.GetStream;

          TmpHead.Append('Content-Type: ' + ABody.ContentType + sLineBreak);
          if ABody.FNeedLength then
            TmpHead.Append('Content-Length: ' + IntToStr(Body.Size) + sLineBreak);
        end
        else
          Body := TMemoryStream.Create;

        TmpHead.Append(FHeaders.ToString);

        Headers := PChar(TmpHead.ToString);

        MessageBox(0, Headers, 'Headers', 0);
        try
          if not HttpSendRequest(hRequest, Headers, Length(Headers), Body.Memory, Body.Size) then
            raise Exception.Create(GetErrorDescription(GetLastError));
        finally
          Body.Free;
        end;

        if Assigned(ABody) and ABody.FReleaseAfterSend then
          ABody.Free;

        lpdwReserved := 0;

        lpdwBufferLength := SizeOf(FResponse.FStatusCode);
        HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @FResponse.FStatusCode,
          lpdwBufferLength, lpdwReserved);

        Result := FResponse.FStatusCode = 200;

        lpdwBufferLength := 2048;
        Headers := StrAlloc(lpdwBufferLength);
        try
          HttpQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Headers, lpdwBufferLength, lpdwReserved);
          FResponse.FHeaders.FromRawString(Headers);
        finally
          StrDispose(Headers);
        end;

        repeat
          InternetQueryDataAvailable(hRequest, lpdwNumberOfBytesAvailable, 0, 0);
          SetLength(Response, lpdwNumberOfBytesAvailable);
          InternetReadFile(hRequest, @Response[0], lpdwNumberOfBytesAvailable, dwBytesRead);
          FResponse.FData := AppendBytes(FResponse.FData, Response);
        until dwBytesRead >= lpdwNumberOfBytesAvailable;

        if FUseCookies then
          for Cookie in FResponse.FHeaders.GetAll('Set-Cookie') do
            FCookies.Add(Cookie);
      finally
        TmpHead.Free;
        InternetCloseHandle(hRequest);
      end;
    finally
      InternetCloseHandle(hConnect);
    end;
  finally
    InternetCloseHandle(hInet);
    IdURI.Free;
  end;
end;

procedure THttpRequest.SetUseCookies(AValue: Boolean);
begin
  FUseCookies := AValue;
  if not AValue then
    FCookies.Clear;
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
  FUserName := '';
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
      FPassword := Copy(LBuffer, 1, LTokenPos - 1);
      Delete(LBuffer, 1, LTokenPos);
      FUserName := Fetch(FPassword, ':');
      if Length(FUserName) = 0 then
      begin
        FPassword := '';
      end;
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
    // Get the path
    LTokenPos := RPos('/', LURI, -1);
    if LTokenPos > 0 then
    begin
      FPath := '/' + Copy(LURI, 1, LTokenPos);
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
  FURI := GetFullURI;
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

  if (FUserName <> '') and (ofAuthInfo in AOptionalFields) then
  begin
    LURI := LURI + FUserName;
    if FPassword <> '' then
    begin
      LURI := LURI + ':' + FPassword;
    end;
    LURI := LURI + '@';
  end;

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

end.
