// ###########################################
// #### Base for action handlers
// ###########################################
unit Fronius.ActionsBase;

interface

uses SysUtils, Classes, {$IFDEF FPC} HTTPDefs {$ELSE} Web.HTTPApp {$ENDIF};

{$IFDEF FPC}
type
  TWebRequest = TRequest;
  TWebResponse = TResponse;
{$ENDIF}

type
  TWebActionBase = class(TObject)
  public
    function getStringParam( Request : TWebRequest; name, def : string ) : string;
    function GetIntParam( Request : TWebRequest; Name : string; def : integer = 0) : integer;
    procedure InitResponse( Response : TWebResponse );
  end;

implementation

uses Fronius.Globals {$IFDEF FPC}, httpprotocol {$ENDIF};

{ TWebActionBase }

function TWebActionBase.GetIntParam(Request: TWebRequest; Name: string;
  def: integer): integer;
var s : string;
begin
     s := getStringParam(Request, Name, def.ToString);

     if not TryStrToInt( s, Result ) then
        Result := def;
end;


function TWebActionBase.getStringParam(Request: TWebRequest; Name,
  def: string): string;
begin
     if Request.QueryFields.IndexOfName(name) >= 0
     then
         Result := Request.QueryFields.Values[name]
     else if Request.ContentFields.IndexOfName(name) >= 0
     then
         Result := Request.ContentFields.Values[name]
     else
         Result := def;
end;

procedure TWebActionBase.InitResponse(Response: TWebResponse);
begin
     // all elements return json data that should not be cached
     {$IFDEF FPC}
     Response.ContentType := 'application/json';
     Response.SetHeader(hhPragma, 'no-cache');
     Response.SetHeader(hhCacheControl, 'no-store, no-cache, must-revalidate');
     Response.SetHeader(hhAllow, froniusConf.HTTPHost);
     Response.SetHeader(hhContentEncoding, 'UTF-8');
     {$ELSE}
     Response.SetCustomHeader('Pragma','no-cache');
     Response.SetCustomHeader('Cache-Control','no-store, no-cache, must-revalidate');
     Response.ContentType := 'application/json; charset=UTF-8';
     {$ENDIF}
     Response.SetCustomHeader('Access-Control-Allow-Credentials', froniusConf.HTTPHost);
     Response.SetCustomHeader('Access-Control-Allow-Credentials', 'true');
end;

end.
