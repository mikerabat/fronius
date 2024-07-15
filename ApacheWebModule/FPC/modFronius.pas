unit modFronius;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;

type
  TwmFronius = class(TFPWebModule)
  private

  public

  end;

var
  wmFronius: TwmFronius;

implementation

{$R *.frm}

initialization
  RegisterHTTPModule('TFPWebModule1', TwmFronius);
end.

