unit HttpUtils;

interface

uses
  Classes, SysUtils;

function PathEncode(const ASrc: String): String;
function RPos(const ASub, AIn: String; AStart: Integer = -1): Integer;
function Fetch(var AInput: String; const ADelim: String): String;
function GetErrorDescription(AErrorCode: Integer): String;
function AppendBytes(ABytes1, ABytes2: TBytes): TBytes;
function RandomHex: String;
procedure WriteBytes(const AStream: TStream; const ABytes: TBytes);
procedure WriteString(const AStream: TStream; const AString: String; const AEncoding: TEncoding = nil);
function GetMimeType(const AMemory: Pointer; const ASize: Integer): String;

implementation

uses
  Windows, WinInet, UrlMon;

function PathEncode(const ASrc: String): String;
const
  UnsafeChars = ['*', '#', '%', '<', '>', '+', ' ', '[', ']'];
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(ASrc) do
  begin
    if CharInSet(ASrc[i], UnsafeChars) or (not CharInSet(ASrc[i], [#32 .. #127])) then
    begin
      Result := Result + '%' + IntToHex(Ord(ASrc[i]), 2);
      Inc(i);
    end
    else
    begin
      Result := Result + ASrc[i];
      Inc(i);
    end;
  end;
end;

function RPos(const ASub, AIn: String; AStart: Integer = -1): Integer;
var
  i: Integer;
  LStartPos: Integer;
  LTokenLen: Integer;
begin
  Result := 0;
  LTokenLen := Length(ASub);
  // Get starting position
  if AStart < 0 then
  begin
    AStart := Length(AIn);
  end;
  if AStart < (Length(AIn) - LTokenLen + 1) then
  begin
    LStartPos := AStart;
  end
  else
  begin
    LStartPos := (Length(AIn) - LTokenLen + 1);
  end;
  // Search for the String
  for i := LStartPos downto 1 do
  begin
    if SameText(Copy(AIn, i, LTokenLen), ASub) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function Fetch(var AInput: String; const ADelim: String): String;
var
  LPos: Integer;
begin
  LPos := Pos(ADelim, AInput);
  if LPos = 0 then
  begin
    Result := AInput;
    AInput := ''; {Do not Localize}
  end
  else
  begin
    Result := Copy(AInput, 1, LPos - 1);
    AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
  end;
end;

// from https://docs.microsoft.com/en-us/windows/win32/wininet/wininet-errors
function GetErrorDescription(AErrorCode: Integer): String;
// addictional values
const
  ERROR_INTERNET_DECODING_FAILED = 12175;
  ERROR_INTERNET_FORTEZZA_LOGIN_NEEDED = 12054;
  ERROR_INTERNET_LOGIN_FAILURE_DISPLAY_ENTITY_BODY = 12174;
  ERROR_INTERNET_NEED_MSN_SSPI_PKG = 12173;
  ERROR_INTERNET_NEED_UI = 12034;
  ERROR_INTERNET_NOT_INITIALIZED = 12172;
  ERROR_INTERNET_SEC_CERT_ERRORS = 12055;
  ERROR_INTERNET_SEC_CERT_NO_REV = 12056;
begin
  case AErrorCode of
    ERROR_FTP_DROPPED:
      Result := 'The FTP operation was not completed because the session was aborted.';
    ERROR_FTP_NO_PASSIVE_MODE:
      Result := 'Passive mode is not available on the server.';
    ERROR_FTP_TRANSFER_IN_PROGRESS:
      Result :=
        'The requested operation cannot be made on the FTP session handle because an operation is already in progress.';
    ERROR_GOPHER_ATTRIBUTE_NOT_FOUND:
      Result := 'The requested attribute could not be located.';
    ERROR_GOPHER_DATA_ERROR:
      Result := 'An error was detected while receiving data from the Gopher server.';
    ERROR_GOPHER_END_OF_DATA:
      Result := 'The end of the data has been reached.';
    ERROR_GOPHER_INCORRECT_LOCATOR_TYPE:
      Result := 'The type of the locator is not correct for this operation.';
    ERROR_GOPHER_INVALID_LOCATOR:
      Result := 'The supplied locator is not valid.';
    ERROR_GOPHER_NOT_FILE:
      Result := 'The request must be made for a file locator.';
    ERROR_GOPHER_NOT_GOPHER_PLUS:
      Result :=
        'The requested operation can be made only against a Gopher+ server, or with a locator that specifies a Gopher+ operation.';
    ERROR_GOPHER_PROTOCOL_ERROR:
      Result := 'An error was detected while parsing data returned from the Gopher server.';
    ERROR_GOPHER_UNKNOWN_LOCATOR:
      Result := 'The locator type is unknown.';
    ERROR_HTTP_COOKIE_DECLINED:
      Result := 'The HTTP cookie was declined by the server.';
    ERROR_HTTP_COOKIE_NEEDS_CONFIRMATION:
      Result := 'The HTTP cookie requires confirmation.';
    ERROR_HTTP_DOWNLEVEL_SERVER:
      Result := 'The server did not return any headers.';
    ERROR_HTTP_HEADER_ALREADY_EXISTS:
      Result := 'The header could not be added because it already exists.';
    ERROR_HTTP_HEADER_NOT_FOUND:
      Result := 'The requested header could not be located.';
    ERROR_HTTP_INVALID_HEADER:
      Result := 'The supplied header is invalid.';
    ERROR_HTTP_INVALID_QUERY_REQUEST:
      Result := 'The request made to HttpQueryInfo is invalid.';
    ERROR_HTTP_INVALID_SERVER_RESPONSE:
      Result := 'The server response could not be parsed.';
    ERROR_HTTP_NOT_REDIRECTED:
      Result := 'The HTTP request was not redirected.';
    ERROR_HTTP_REDIRECT_FAILED:
      Result :=
        'The redirection failed because either the scheme changed (for example, HTTP to FTP) or all attempts made to redirect failed (default is five attempts).';
    ERROR_HTTP_REDIRECT_NEEDS_CONFIRMATION:
      Result := 'The redirection requires user confirmation.';
    ERROR_INTERNET_ASYNC_THREAD_FAILED:
      Result := 'The application could not start an asynchronous thread.';
    ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT:
      Result := 'There was an error in the automatic proxy configuration script.';
    ERROR_INTERNET_BAD_OPTION_LENGTH:
      Result :=
        'The length of an option supplied to InternetQueryOption or InternetSetOption is incorrect for the type of option specified.';
    ERROR_INTERNET_BAD_REGISTRY_PARAMETER:
      Result := 'A required registry value was located but is an incorrect type or has an invalid value.';
    ERROR_INTERNET_CANNOT_CONNECT:
      Result := 'The attempt to connect to the server failed.';
    ERROR_INTERNET_CHG_POST_IS_NON_SECURE:
      Result :=
        'The application is posting and attempting to change multiple lines of text on a server that is not secure.';
    ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED:
      Result := 'The server is requesting client authentication.';
    ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP:
      Result := 'Client authorization is not set up on this computer.';
    ERROR_INTERNET_CONNECTION_ABORTED:
      Result := 'The connection with the server has been terminated.';
    ERROR_INTERNET_CONNECTION_RESET:
      Result := 'The connection with the server has been reset.';
    ERROR_INTERNET_DECODING_FAILED:
      Result :=
        'WinINet failed to perform content decoding on the response. For more information, see the Content Encoding topic.';
    ERROR_INTERNET_DIALOG_PENDING:
      Result := 'Another thread has a password dialog box in progress.';
    ERROR_INTERNET_DISCONNECTED:
      Result := 'The Internet connection has been lost.';
    ERROR_INTERNET_EXTENDED_ERROR:
      Result :=
        'An extended error was returned from the server. This is typically a string or buffer containing a verbose error message. Call InternetGetLastResponseInfo to retrieve the error text.';
    ERROR_INTERNET_FAILED_DUETOSECURITYCHECK:
      Result := 'The function failed due to a security check.';
    ERROR_INTERNET_FORCE_RETRY:
      Result := 'The function needs to redo the request.';
    ERROR_INTERNET_FORTEZZA_LOGIN_NEEDED:
      Result := 'The requested resource requires Fortezza authentication.';
    ERROR_INTERNET_HANDLE_EXISTS:
      Result := 'The request failed because the handle already exists.';
    ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR:
      Result := 'The application is moving from a non-SSL to an SSL connection because of a redirect.';
    ERROR_INTERNET_HTTPS_HTTP_SUBMIT_REDIR:
      Result := 'The data being submitted to an SSL connection is being redirected to a non-SSL connection.';
    ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR:
      Result := 'The application is moving from an SSL to an non-SSL connection because of a redirect.';
    ERROR_INTERNET_INCORRECT_FORMAT:
      Result := 'The format of the request is invalid.';
    ERROR_INTERNET_INCORRECT_HANDLE_STATE:
      Result :=
        'The requested operation cannot be carried out because the handle supplied is not in the correct state.';
    ERROR_INTERNET_INCORRECT_HANDLE_TYPE:
      Result := 'The type of handle supplied is incorrect for this operation.';
    ERROR_INTERNET_INCORRECT_PASSWORD:
      Result :=
        'The request to connect and log on to an FTP server could not be completed because the supplied password is incorrect.';
    ERROR_INTERNET_INCORRECT_USER_NAME:
      Result :=
        'The request to connect and log on to an FTP server could not be completed because the supplied user name is incorrect.';
    ERROR_INTERNET_INSERT_CDROM:
      Result := 'The request requires a CD-ROM to be inserted in the CD-ROM drive to locate the resource requested.';
    ERROR_INTERNET_INTERNAL_ERROR:
      Result := 'An internal error has occurred.';
    ERROR_INTERNET_INVALID_CA:
      Result := 'The function is unfamiliar with the Certificate Authority that generated the server''s certificate.';
    ERROR_INTERNET_INVALID_OPERATION:
      Result := 'The requested operation is invalid.';
    ERROR_INTERNET_INVALID_OPTION:
      Result := 'A request to InternetQueryOption or InternetSetOption specified an invalid option value.';
    ERROR_INTERNET_INVALID_PROXY_REQUEST:
      Result := 'The request to the proxy was invalid.';
    ERROR_INTERNET_INVALID_URL:
      Result := 'The URL is invalid.';
    ERROR_INTERNET_ITEM_NOT_FOUND:
      Result := 'The requested item could not be located.';
    ERROR_INTERNET_LOGIN_FAILURE:
      Result := 'The request to connect and log on to an FTP server failed.';
    ERROR_INTERNET_LOGIN_FAILURE_DISPLAY_ENTITY_BODY:
      Result :=
        'The MS-Logoff digest header has been returned from the website. This header specifically instructs the digest package to purge credentials for the associated realm.';
    // This error will only be returned if INTERNET_ERROR_MASK_LOGIN_FAILURE_DISPLAY_ENTITY_BODY option has been set; otherwise, ERROR_INTERNET_LOGIN_FAILURE is returned.';
    ERROR_INTERNET_MIXED_SECURITY:
      Result :=
        'The content is not entirely secure. Some of the content being viewed may have come from unsecured servers.';
    ERROR_INTERNET_NAME_NOT_RESOLVED:
      Result := 'The server name could not be resolved.';
    ERROR_INTERNET_NEED_MSN_SSPI_PKG:
      Result := 'Not currently implemented.';
    ERROR_INTERNET_NEED_UI:
      Result := 'A user interface or other blocking operation has been requested.';
    ERROR_INTERNET_NO_CALLBACK:
      Result := 'An asynchronous request could not be made because a callback function has not been set.';
    ERROR_INTERNET_NO_CONTEXT:
      Result := 'An asynchronous request could not be made because a zero context value was supplied.';
    ERROR_INTERNET_NO_DIRECT_ACCESS:
      Result := 'Direct network access cannot be made at this time.';
    ERROR_INTERNET_NOT_INITIALIZED:
      Result :=
        'Initialization of the WinINet API has not occurred. Indicates that a higher-level function, such as InternetOpen, has not been called yet.';
    ERROR_INTERNET_NOT_PROXY_REQUEST:
      Result := 'The request cannot be made via a proxy.';
    ERROR_INTERNET_OPERATION_CANCELLED:
      Result :=
        'The operation was canceled, usually because the handle on which the request was operating was closed before the operation completed.';
    ERROR_INTERNET_OPTION_NOT_SETTABLE:
      Result := 'The requested option cannot be set, only queried.';
    ERROR_INTERNET_OUT_OF_HANDLES:
      Result := 'No more handles could be generated at this time.';
    ERROR_INTERNET_POST_IS_NON_SECURE:
      Result := 'The application is posting data to a server that is not secure.';
    ERROR_INTERNET_PROTOCOL_NOT_FOUND:
      Result := 'The requested protocol could not be located.';
    ERROR_INTERNET_PROXY_SERVER_UNREACHABLE:
      Result := 'The designated proxy server cannot be reached.';
    ERROR_INTERNET_REDIRECT_SCHEME_CHANGE:
      Result := 'The function could not handle the redirection, because the scheme changed (for example, HTTP to FTP).';
    ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND:
      Result := 'A required registry value could not be located.';
    ERROR_INTERNET_REQUEST_PENDING:
      Result := 'The required operation could not be completed because one or more requests are pending.';
    ERROR_INTERNET_RETRY_DIALOG:
      Result := 'The dialog box should be retried.';
    ERROR_INTERNET_SEC_CERT_CN_INVALID:
      Result :=
        'SSL certificate common name (host name field) is incorrect for example, if you entered www.server.com and the common name on the certificate says www.different.com.';
    ERROR_INTERNET_SEC_CERT_DATE_INVALID:
      Result := 'SSL certificate date that was received from the server is bad. The certificate is expired.';
    ERROR_INTERNET_SEC_CERT_ERRORS:
      Result := 'The SSL certificate contains errors.';
    ERROR_INTERNET_SEC_CERT_NO_REV:
      Result := 'The SSL certificate was not revoked.';
    ERROR_INTERNET_SEC_CERT_REV_FAILED:
      Result := 'Revocation of the SSL certificate failed.';
    ERROR_INTERNET_SEC_CERT_REVOKED:
      Result := 'The SSL certificate was revoked.';
    ERROR_INTERNET_SEC_INVALID_CERT:
      Result := 'The SSL certificate is invalid.';
    ERROR_INTERNET_SECURITY_CHANNEL_ERROR:
      Result := 'The application experienced an internal error loading the SSL libraries.';
    ERROR_INTERNET_SERVER_UNREACHABLE:
      Result := 'The website or server indicated is unreachable.';
    ERROR_INTERNET_SHUTDOWN:
      Result := 'WinINet support is being shut down or unloaded.';
    ERROR_INTERNET_TCPIP_NOT_INSTALLED:
      Result := 'The required protocol stack is not loaded and the application cannot start WinSock.';
    ERROR_INTERNET_TIMEOUT:
      Result := 'The request has timed out.';
    ERROR_INTERNET_UNABLE_TO_CACHE_FILE:
      Result := 'The function was unable to cache the file.';
    ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT:
      Result :=
        'The automatic proxy configuration script could not be downloaded. The INTERNET_FLAG_MUST_CACHE_REQUEST flag was set.';
    ERROR_INTERNET_UNRECOGNIZED_SCHEME:
      Result := 'The URL scheme could not be recognized, or is not supported.';
    ERROR_INVALID_HANDLE:
      Result := 'The handle that was passed to the API has been either invalidated or closed.';
    ERROR_MORE_DATA:
      Result := 'More data is available.';
    ERROR_NO_MORE_FILES:
      Result := 'No more files have been found.';
    ERROR_NO_MORE_ITEMS:
      Result := 'No more items have been found.';
  else
    Result := SysErrorMessage(AErrorCode);
  end;
end;

function AppendBytes(ABytes1, ABytes2: TBytes): TBytes;
begin
  SetLength(Result, Length(ABytes1) + Length(ABytes2));
  if ABytes1 <> nil then
    Move(ABytes1[0], Result[0], Length(ABytes1));
  if ABytes2 <> nil then
    Move(ABytes2[0], Result[Length(ABytes1)], Length(ABytes2));
end;

function RandomHex: String;
var
  i: Integer;
const
  Chars = '0123456789abcdef';
begin
  RandSeed := Trunc(Now * SecsPerDay * 1000.0);
  Result := '';
  for i := 1 to 20 do
    Result := Result + Chars[Random(16) + 1];
end;

procedure WriteBytes(const AStream: TStream; const ABytes: TBytes);
begin
  AStream.Write(ABytes[0], Length(ABytes));
end;

procedure WriteString(const AStream: TStream; const AString: string; const AEncoding: TEncoding);
var
  LBytes: TBytes;
begin
  if AEncoding = nil then
    LBytes := TEncoding.ASCII.GetBytes(AString)
  else
    LBytes := AEncoding.GetBytes(AString);
  WriteBytes(AStream, LBytes);
end;

function GetMimeType(const AMemory: Pointer; const ASize: Integer): String;
var
  Mimetype: PChar;
begin
  FindMimeFromData(nil, nil, AMemory, ASize, nil, 0, Mimetype, 0);
  Result := String(Mimetype);
end;

end.
