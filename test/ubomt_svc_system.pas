unit ubomt_svc_system;

{$mode objfpc}{$H+}{$modeswitch prefixedattributes}

interface

// -s sistema/connessione -m put -p "tok=d67fdc3f6e6510182e993fe0b0a55611;host=10.4.4.172;dbname=AHR80_DDNRT;user=sa;password=@dp55SQL"
// -s sistema/connessione -m options -p "tok=d67fdc3f6e6510182e993fe0b0a55611;host=10.4.4.172;dbname=AHR80_DDNRT;user=sa;password=@dp55SQL"
uses
  Classes, SysUtils
  , ubomt
  , fpjson
  ;

{ **
  In questa unit, relativamente alla gestione del sistema, dovrebbero essere presenti:
  - managers
  - reader
  - writers
  **
}

type

  { TBomt_SysConn_Reader }

  TBomt_SysConn_Reader = class(TBomt_Reader)
  public
    constructor Create(const AConfig: TBomt_AppConfig; const ALogger: TBomt_Logger); override;
    destructor Destroy; override;

    function LoadItem(const ASid: TBomt_SID): integer; override;
    procedure LoadAll; override;

  end;


  { TBomt_SysConn_Service }

  [TBomt_Service('sistema', 'Servizio per gestione delle connessioni')]
  TBomt_SysConn_Service = Class(TBomt_Service)
  private
    function GetConnessioneUpdate: boolean;
    function GetNuovo: boolean;
    function GetTest: boolean;
  published

    [TBomt_Method([brkDataGet, brkDataQuery])]
    property Test: boolean read GetTest;

    [TBomt_Method([brkDataPost,brkDataPut])]
    property Connessione: boolean read GetNuovo;

    [TBomt_Method([brkDataPatch])]
    property ConnessioneUpdate: boolean read GetConnessioneUpdate;

  end;



implementation


{ TBomt_Auth_Service }

function TBomt_SysConn_Service.GetConnessioneUpdate: boolean;
begin
   Logger.EnterIn('TBomt_SysConn_Service.GetConnessioneUpdate');

   result := False;
   Response.StatusCod:=200;
   Response.StatusDes:='200 (OK)';
   Response.ResponseData:=TJSONObject.Create;
   Response.ResponseData.Add('GetConnessioneUpdate', 'OK');
   Response.Handled:=True;
   result :=Response.Handled;

end;

function TBomt_SysConn_Service.GetNuovo: boolean;
{
var mng: TBomt_SystemObjectManager;     // manager
    reader: TBomt_SysConn_Reader;          // reader
    WUser: TBomtSoUser;                 // classi bomt

    sUser, sPass, sToken, sEnv: string; // variabili
    i: integer;
}
begin
   Logger.EnterIn('TBomt_SysConn_Service.GetNuovo');

   result := False;
   Response.StatusCod:=200;
   Response.StatusDes:='200 (OK)';
   Response.ResponseData:=TJSONObject.Create;
   Response.ResponseData.Add('GetNuovo', 'OK');
   Response.Handled:=True;

   {
   mng:=TBomt_SystemObjectManager.Create('manag01', Config, Logger);
   try
     reader:=TBomt_SysConn_Reader.Create(Config, Logger);
     reader.Items:=mng.Items;

     mng.Reader:=reader;
     i:=mng.Reader.LoadItem(sUser);

     if i>= 0 then begin
        WUser := TBomtSoUser(mng.Items[i]);
        if WUser.Password = sPass then begin
           try
             Response.ResponseData:=TJSONObject.Create;
             sToken:=CreateToken(WUser.UserName, sEnv, WUser.Role);
             Response.ResponseData.Add('token', sToken);
             Logger.Send(Format('login %s, role %s', [sUser, WUser.Role]));
             Logger.Watch('Session life', WUser.SessionLife);
             Response.StatusCod:=200;
             Response.StatusDes:='200 (OK)';
           except
             on e: exception do begin
                Logger.SendError(e.Message);
                Response.StatusCod:=404;
                Response.StatusDes:='404 (Not Found)';
             end;
           end;

        end else begin
           Response.StatusCod:=204;
           Response.StatusDes:='204 (No Content)';
           Logger.Send(Format('login attempt %s, invalid password', [sUser]));
        end;
     end else begin
        Response.StatusCod:=404;
        Response.StatusDes:='404 (Not Found)';
        Logger.Send(Format('user %s not found!', [sUser]));
     end;

   finally
     FreeAndNil(reader);
     FreeAndNil(mng);
     Logger.ExitFrom('TBomt_Sys_Service.GetLogin');
   end;
   }

   result :=Response.Handled;

end;

function TBomt_SysConn_Service.GetTest: boolean;
{
var mng: TBomt_SystemObjectManager;     // manager
    reader: TBomt_SysConn_Reader;          // reader
    WUser: TBomtSoUser;                 // classi bomt

    sUser, sPass, sToken, sEnv: string; // variabili
    i: integer;
}
begin
   Logger.EnterIn('TBomt_SysConn_Service.GetTest');

   result := False;
   Response.StatusCod:=200;
   Response.StatusDes:='200 (OK)';
   Response.ResponseData:=TJSONObject.Create;
   Response.ResponseData.Add('Test', 'OK');
   Response.Handled:=True;

   {
   mng:=TBomt_SystemObjectManager.Create('manag01', Config, Logger);
   try
     reader:=TBomt_SysConn_Reader.Create(Config, Logger);
     reader.Items:=mng.Items;

     mng.Reader:=reader;
     i:=mng.Reader.LoadItem(sUser);

     if i>= 0 then begin
        WUser := TBomtSoUser(mng.Items[i]);
        if WUser.Password = sPass then begin
           try
             Response.ResponseData:=TJSONObject.Create;
             sToken:=CreateToken(WUser.UserName, sEnv, WUser.Role);
             Response.ResponseData.Add('token', sToken);
             Logger.Send(Format('login %s, role %s', [sUser, WUser.Role]));
             Logger.Watch('Session life', WUser.SessionLife);
             Response.StatusCod:=200;
             Response.StatusDes:='200 (OK)';
           except
             on e: exception do begin
                Logger.SendError(e.Message);
                Response.StatusCod:=404;
                Response.StatusDes:='404 (Not Found)';
             end;
           end;

        end else begin
           Response.StatusCod:=204;
           Response.StatusDes:='204 (No Content)';
           Logger.Send(Format('login attempt %s, invalid password', [sUser]));
        end;
     end else begin
        Response.StatusCod:=404;
        Response.StatusDes:='404 (Not Found)';
        Logger.Send(Format('user %s not found!', [sUser]));
     end;

   finally
     FreeAndNil(reader);
     FreeAndNil(mng);
     Logger.ExitFrom('TBomt_Sys_Service.GetLogin');
   end;
   }

   result :=Response.Handled;

end;


{ TBomt_SysConn_Reader }

constructor TBomt_SysConn_Reader.Create(const AConfig: TBomt_AppConfig;
  const ALogger: TBomt_Logger);
var sPar: string;
begin
  ALogger.Send('TBomt_Sys_Reader.Create');

  inherited Create(AConfig, ALogger);

  // sPar:=Config.Options['Sys'].Values['sysfolder'];
  // ALogger.Watch('parameter', sPar);

end;

destructor TBomt_SysConn_Reader.Destroy;
begin
  // free resources

  inherited Destroy;
end;

function TBomt_SysConn_Reader.LoadItem(const ASid: TBomt_SID): integer;
{
var s: string;
    Wuser: TBomtSoUser;
}
begin
  result := -1;
{
  s:=FUserDataFile.ReadString(ASid, 'password', '');
  if s <> '' then begin
     Wuser:=TBomtSoUser.Create(ASid);
     Wuser.UserName:=ASid;
     Wuser.Password:=s;
     Wuser.Role:=FUserDataFile.ReadString(ASid, 'role', '');
     Wuser.SessionLife:=FUserDataFile.ReadInt64(ASid, 'sessionlife', 60);
     result := Items.Add(Wuser);
  end;
}
end;

procedure TBomt_SysConn_Reader.LoadAll;
begin
   raise Exception.Create('Operation not supported');
end;


initialization

   Bomt_RegisterService(TBomt_SysConn_Service);

end.

