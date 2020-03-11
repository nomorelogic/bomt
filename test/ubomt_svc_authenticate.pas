unit ubomt_svc_authenticate;

{$mode objfpc}{$H+}{$modeswitch prefixedattributes}

interface

// -s autenticazione/login -m get -p "usr=marcello;pwd=pippo555;env=DDNRT"


uses
  Classes, SysUtils
  , ubomt
  , IniFiles
  , md5
  , fpjson
  , typinfo
  ;


type

  { TBomt_Auth_Reader }

  TBomt_Auth_Reader = class(TBomt_Reader)
  private
    FUserDataFile: TIniFile;
  public
    constructor Create(const AConfig: TBomt_AppConfig; const ALogger: TBomt_Logger); override;
    destructor Destroy; override;


    function LoadItem(const ASid: TBomt_SID): integer; override;
    procedure LoadAll; override;

  end;


  { TBomt_Auth_Service }

  [TBomt_Service('autenticazione', 'Servizio per autenticazione utenti e validazione tokens')]
  TBomt_Auth_Service = Class(TBomt_Service)
  private
    function CreateToken(const AUser, AEnvironment, ARole: string): string;
    function GetLogin: boolean;
    function EnvironmentIsValid(const AEnv: string; out AMsg: string): boolean;
    // function CreateSession(const AUser, ARole, AToken: string): string; // sessione non necessaria
  published

    [TBomt_Method([brkDataGet, brkDataQuery])]
    property Login: boolean read GetLogin;

  end;



implementation

uses variants;

{ TBomt_Auth_Service }


{
function TBomt_Auth_Service.DoExecute: boolean;
var m: TBomt_SystemObjectManager;
    r: TBomt_Auth_Reader;
    sUser, sPass, sToken: string;
    WUser: TBomtSoUser;
    i: integer;
begin
   // writeln('... TBomt_Auth_Service.DoExecute');
   // ReadServiceMethods;
   // DumpTypeInfo(self);
   // writeln('... test get');
   // TestGet(self);
   // writeln('... TBomt_Auth_Service.DoExecute');

   Response.StatusCod:=0;
   Response.StatusDes:='';
   Response.Handled:=false;


   m:=TBomt_SystemObjectManager.Create('manag01', Config, Logger);
   try
     r:=TBomt_Auth_Reader.Create(Config, Logger);
     r.Items:=m.Items;

     m.Reader:=r;

     sUser:=Request.Params.Values['usr'];
     sPass:=Request.Params.Values['pwd'];
     i:=m.Reader.LoadItem(sUser);

     if i>= 0 then begin
        WUser := TBomtSoUser(m.Items[i]);
        if WUser.Password = sPass then begin
           Response.StatusCod:=200;
           Response.StatusDes:='200 (OK)';

           Response.ResponseData:=TJSONObject.Create;
           sToken:=CreateToken(WUser.UserName, WUser.Role);
           Response.ResponseData.Add('token', sToken);
           // Response.ResponseData.Add('session', CreateSession(WUser.UserName, WUser.Role, sToken));

           Logger.Send(Format('login %s, role %s', [sUser, WUser.Role]));
           Logger.Watch('Session life', WUser.SessionLife);
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
     FreeAndNil(r);
     FreeAndNil(m);
   end;

   result :=Response.Handled;

end;

procedure TBomt_Auth_Service.ReadServiceMethods;
begin

end;
}


function TBomt_Auth_Service.CreateToken(const AUser, AEnvironment, ARole: string): string;
var s, sfilename: string;
    sl:TStringList;
begin
   sl:=TStringList.Create;
   try
     s:='';
     sl.Values['user']:=AUser;
     sl.Values['role']:=ARole;
     sl.Values['environment']:=AEnvironment;
     sl.Values['datetime.create']:=FormatDateTime('yyyy-mm-dd_hhnnss[zzz]', Now);
     sl.Values['datetime.renew']:='';
     sl.Values['datetime.expire']:=FormatDateTime('yyyy-mm-dd_hhnnss[zzz]', Now + 100); // 100 gg

     s:=MD5Print(MD5String(sl.Text));
     sfilename:=Config.Options['Sys'].Values['tokenfolder'];
     sfilename:=sfilename + PathDelim + s + '.dat';
     sl.SaveToFile(sfilename);
     result:=s;
   finally
     FreeAndNil(sl);
   end;
end;

function TBomt_Auth_Service.GetLogin: boolean;
var mng: TBomt_SystemObjectManager;
    reader: TBomt_Auth_Reader;
    sUser, sPass, sToken, sEnv, sMsg: string;
    WUser: TBomtSoUser;
    i: integer;
begin
   // writeln('... TBomt_Auth_Service.DoExecute');
   // ReadServiceMethods;
   // DumpTypeInfo(self);
   // writeln('... test get');
   // TestGet(self);
   // writeln('... TBomt_Auth_Service.DoExecute');
   Logger.EnterIn('TBomt_Auth_Service.GetLogin');

   result := False;
   Response.StatusCod:=0;
   Response.StatusDes:='';
   Response.Handled:=True;


   mng:=TBomt_SystemObjectManager.Create('manag01', Config, Logger);
   try
     reader:=TBomt_Auth_Reader.Create(Config, Logger);
     reader.Items:=mng.Items;

     mng.Reader:=reader;

     sUser:=Request.Params.Values['usr'];
     sPass:=Request.Params.Values['pwd'];
     sEnv:=Request.Params.Values['env'];
     i:=mng.Reader.LoadItem(sUser);

     if i>= 0 then begin
        WUser := TBomtSoUser(mng.Items[i]);
        if WUser.Password = sPass then begin
           try
             Response.ResponseData:=TJSONObject.Create;

             // environment
             if not EnvironmentIsValid(sEnv, sMsg) then begin
                sEnv:='';
                Response.AddMessage(brtWarn, sMsg);
             end;

             sToken:=CreateToken(WUser.UserName, sEnv, WUser.Role);
             Response.ResponseData.Add('token', sToken);
             // Response.ResponseData.Add('session', CreateSession(WUser.UserName, WUser.Role, sToken));
             Logger.Send(Format('login %s, role %s', [sUser, WUser.Role]));
             Logger.Watch('Session life', WUser.SessionLife);
             Response.StatusCod:=200;
             Response.StatusDes:='200 (OK)';
           except
             on e: exception do begin
                Logger.SendError(e.Message);
                Response.StatusCod:=404;
                Response.StatusDes:='404 (Not Found)';
                // se debug user... fornire Logger.FileName nel response
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
     Logger.ExitFrom('TBomt_Auth_Service.GetLogin');
   end;

   result :=Response.Handled;

end;

function TBomt_Auth_Service.EnvironmentIsValid(const AEnv: string; out
  AMsg: string): boolean;
var s: string;
begin
  result :=False;
  AMsg:='';

  s:=Config.Options['Sys'].Values['sysfolder'];
  if DirectoryExists(s) then begin
     s:=s+PathDelim+Format('env[%s].ini', [AEnv]);
     result:=FileExists(s);
     if not Result then
        AMsg:='environment not found';
  end else begin
     AMsg:='sys folder not found';
  end;

  if AMsg<>'' then
     Logger.Warning(AMsg);

end;



{
function TBomt_Auth_Service.CreateSession(const AUser, ARole, AToken: string): string;
var s, sfilename: string;
    sl:TStringList;
begin
   s:=FormatDateTime('yyyy-mm-dd_hhnnss[zzz]', Now);
   sl:=TStringList.Create;
   try
     sl.Values['user']:=AUser;
     sl.Values['role']:=ARole;
     sl.Values['token']:=AToken;
     sl.Values['datetime.create']:=s;
     sl.Values['datetime.renew']:='';

     s:=FormatDateTime('yyyy-mm-dd_hhnnss[zzz]', Now + 0.2);
     sl.Values['datetime.expire']:=s;

     s:=MD5Print(MD5String(sl.Text));
     sfilename:=Config.Options['Sys'].Values['sessionfolder'] + PathDelim + s + '.dat';
     sl.SaveToFile(sfilename);
   finally
     FreeAndNil(sl);
   end;
   result:=s;
end;
}

{ TBomt_Auth_Reader }

constructor TBomt_Auth_Reader.Create(const AConfig: TBomt_AppConfig;
  const ALogger: TBomt_Logger);
var udir, ufil: string;
begin
  ALogger.Send('TBomt_Auth_Reader.Create');

  inherited Create(AConfig, ALogger);



  udir:=Config.Options['Sys'].Values['sysfolder'];
  ufil:=Config.Options['Sys'].Values['users'];

  ALogger.Watch('folder', udir);
  ALogger.Watch('file name', ufil);


  FUserDataFile:=TIniFile.Create(udir + PathDelim + ufil);


end;

destructor TBomt_Auth_Reader.Destroy;
begin
  FreeAndNil(FUserDataFile);

  inherited Destroy;
end;

function TBomt_Auth_Reader.LoadItem(const ASid: TBomt_SID): integer;
var s: string;
    Wuser: TBomtSoUser;
begin
  result := -1;
  s:=FUserDataFile.ReadString(ASid, 'password', '');
  if s <> '' then begin
     // todo: validare la password senza metterla nell'istanza?
     Wuser:=TBomtSoUser.Create(ASid);
     Wuser.UserName:=ASid;
     Wuser.Password:=s;
     Wuser.Role:=FUserDataFile.ReadString(ASid, 'role', '');
     Wuser.SessionLife:=FUserDataFile.ReadInt64(ASid, 'sessionlife', 60);
     result := Items.Add(Wuser);
  end;

end;

procedure TBomt_Auth_Reader.LoadAll;
begin
   raise Exception.Create('Operation not supported');
end;


initialization
    // ServiceList[TBomt_Auth_Service.ServiceName]:= TBomt_Auth_Service;
    Bomt_RegisterService(TBomt_Auth_Service);

end.

