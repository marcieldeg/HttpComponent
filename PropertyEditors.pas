unit PropertyEditors;

interface

uses
  DesignIntf, DesignEditors;

type
  THeadersProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

uses
  Forms, SysUtils, Controls, HttpClasses, UfrmHeadersEditor, TypInfo;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(THeaders), THttpRequest, 'Headers', THeadersProperty);
end;

{THeadersProperty}

// Object-typed properties are exposed by the IDE's property inspector as an
// ordinal (the object reference reinterpreted as an integer/pointer-sized
// value). This is the standard, documented way TClassProperty descendants
// read/write object properties in the Delphi 2010 design-time API; there is
// no safer alternative available in that IDE version. GetOrdValue never
// returns nil here because THttpRequest.Headers is always assigned in its
// constructor, but the check below keeps this method defensive regardless.
procedure THeadersProperty.Edit;
var
  Headers: THeaders;
begin
  Headers := THeaders(Pointer(GetOrdValue));
  if not Assigned(Headers) then
    Exit;
  with TFHeadersEditor.Create(Application) do
    try
      LoadHeaders(Headers);
      if ShowModal = mrOk then
      begin
        SaveHeaders(Headers);
        SetOrdValue(Integer(Headers));
      end;
    finally
      Free;
    end;
end;

function THeadersProperty.GetAttributes: TPropertyAttributes;
begin
  Result := {inherited GetAttributes +} [paDialog];
end;

function THeadersProperty.GetValue: string;
begin
  FmtStr(Result, '<%s>', [GetTypeName(GetPropType)]);
end;

end.
