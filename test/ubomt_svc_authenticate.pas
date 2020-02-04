unit ubomt_svc_authenticate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , ubomt;


type

  { TBomt_Auth_Reader }

  TBomt_Auth_Reader = class(TBomt_Reader)
  public
    procedure LoadItem(const ASid: TBomt_SID); override;
    procedure LoadAll; override;

  end;


  { TBomt_Auth_Service }

  TBomt_Auth_Service = Class(TBomt_Service)
  public
    const ServiceName = 'login';
  public

    function DoExecute: boolean; override;

  end;



implementation

{ TBomt_Auth_Service }


function TBomt_Auth_Service.DoExecute: boolean;
var m: TBomt_SystemObjectManager;
    r: TBomt_Auth_Reader;
begin

   Response.StatusCod:=0;
   Response.StatusDes:='';
   Response.Handled:=false;


   m:=TBomt_SystemObjectManager.Create('login01', Config, Logger);
   try
     r:=TBomt_Auth_Reader.Create(Config, Logger);
     r.Items:=m.Items;

     m.Reader:=r;

     m.Reader.LoadItem(Request.Params.Values['usr']);

     if true then begin
        Response.StatusCod:=200;
        Response.StatusDes:='200 (OK)';
        Logger.Send('!!! executed !!!');
     end else begin

     end;


   finally
     FreeAndNil(r);
     FreeAndNil(m);
   end;

   result :=Response.Handled;

end;

{ TBomt_Auth_Reader }

procedure TBomt_Auth_Reader.LoadItem(const ASid: TBomt_SID);
begin

end;

procedure TBomt_Auth_Reader.LoadAll;
begin

end;


initialization
    ServiceList[TBomt_Auth_Service.ServiceName]:= TBomt_Auth_Service;


end.

