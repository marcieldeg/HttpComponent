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

procedure THeadersProperty.Edit;
var
  Headers: THeaders;
begin
  Headers := THeaders(Pointer(GetOrdValue));
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
