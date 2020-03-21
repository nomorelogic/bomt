unit ubomt_svc_system;

{$mode objfpc}{$H+}{$modeswitch prefixedattributes}

interface

// -s sistema/connessione -m get -p "tok=d67fdc3f6e6510182e993fe0b0a55611;host=127.0.0.1;name=DdnRt_MsSql"
// -s sistema/connessione -m post -p "tok=d67fdc3f6e6510182e993fe0b0a55611;name=DdnRt_MsSql;HostName=127.0.0.1;ConnectionType=MsSql;DatabaseName=AHRDDNRETAIL;UserName=sa;Password=@dp55SQL"
// -s sistema/connessione -m patch -p "tok=d67fdc3f6e6510182e993fe0b0a55611;name=DdnRt_MsSql;HostName=sconosciuto0;name=DdnRt_MsSql;ConnectionType=sconosciuto1;DatabaseName=sconosciuto2;UserName=sconosciuto3;Password=sconosciuto4"
// -s sistema/connessione -m patch -p "tok=d67fdc3f6e6510182e993fe0b0a55611;name=DdnRt_MsSql;HostName=127.0.0.1;ConnectionType=MsSql;DatabaseName=AHRDDNRETAIL;UserName=sa;Password=@dp55SQL"

uses
  Classes, SysUtils
  , ubomt
  , fpjson
  , IniFiles
  ;

{ **
  In questa unit, relativamente alla gestione del sistema, dovrebbero essere presenti:
  - managers
  - reader
  - writers
  **
}


const
  CONNECTION_PATH_MASK = '%s/%s.con';  // folder/sid


type

  { TBomt_SysConn_Reader }

  TBomt_SysConn_Reader = class(TBomt_Reader)
  private
    FConnFolder: string;

  public
    constructor Create(const AConfig: TBomt_AppConfig;
                       const ALogger: TBomt_Logger;
                       const AManager: TBomt_SystemObjectManager); override;
    destructor Destroy; override;

    function LoadItem(const ASid: TBomt_SID): integer; override;
    procedure LoadAll; override;

  end;

  { TBomt_SysConn_Writer }

  TBomt_SysConn_Writer = class(TBomt_Writer)
  public
    constructor Create(const AConfig: TBomt_AppConfig; const ALogger: TBomt_Logger;
                       const AManager: TBomt_SystemObjectManager); override;
    destructor Destroy; override;

    procedure SaveItem; override;
    procedure SaveAll; override;

  end;

  { TBomt_SysConn_Service }

  [TBomt_Service('sistema', 'Servizio per gestione delle connessioni a database')]
  TBomt_SysConn_Service = Class(TBomt_Service)
  private
    function GetConnessione_Update: boolean;
    function GetConnessione_New: boolean;
    function GetConnessione_Read: boolean;
    function GetTest: boolean;
  published

    [TBomt_Method('test', [brkDataGet, brkDataQuery], 'endpoint di test')]
    property Test: boolean read GetTest;

    [TBomt_Method('connessione', [brkDataGet], 'Interrogazione connessione')]
    property ConnessioneRead: boolean read GetConnessione_Read;

    [TBomt_Method('connessione', [brkDataPost], 'Creazione nuova connessione')]
    property Connessione: boolean read GetConnessione_New;

    [TBomt_Method('connessione', [brkDataPatch], 'Aggiornamento connessione esistente')]
    property ConnessioneUpdate: boolean read GetConnessione_Update;

  end;



implementation

{ TBomt_SysConn_Writer }

constructor TBomt_SysConn_Writer.Create(const AConfig: TBomt_AppConfig;
  const ALogger: TBomt_Logger; const AManager: TBomt_SystemObjectManager);
begin
  ALogger.Send('TBomt_SysConn_Writer.Create');

  inherited Create(AConfig, ALogger, AManager);

end;

destructor TBomt_SysConn_Writer.Destroy;
begin
  // free resources


  inherited Destroy;
end;

procedure TBomt_SysConn_Writer.SaveItem;
var sFolder, sName: string;
    ConnDef: TIniFile;
    WConn: TBomtSoConnection;
begin
   try
     sFolder:=Config.Options['Sys'].Values['sysfolder'];
     if not DirectoryExists(sFolder) then
        raise Exception.CreateFmt('folder %s doesn''t exists', [sFolder]);

     if not Assigned(Manager.Data) then
        raise Exception.Create('object to save not found');

     WConn:=TBomtSoConnection(Manager.Data);
     sName:=sFolder+PathDelim+Manager.Data.Name+'.con';
     ConnDef:=TIniFile.Create(sName);
     try
       // todo: serialization
       ConnDef.WriteString('Connection', 'ConnectionType', WConn.ConnectionType);
       ConnDef.WriteString('Connection', 'HostName',       WConn.HostName);
       ConnDef.WriteString('Connection', 'DatabaseName',   WConn.DatabaseName);
       ConnDef.WriteString('Connection', 'UserName',       WConn.UserName);
       ConnDef.WriteString('Connection', 'Password',       WConn.Password);
     finally
       FreeAndNil(ConnDef);
     end;

   except
     on e: exception do begin
        Logger.SendError(e.Message);
        raise;
     end;
   end;
end;

procedure TBomt_SysConn_Writer.SaveAll;
begin
  raise Exception.Create('Operation not supported');
end;


{ TBomt_Auth_Service }

function TBomt_SysConn_Service.GetConnessione_Update: boolean;
var mng: TBomt_SystemObjectManager;     // manager
    reader: TBomt_SysConn_Reader;       // reader
    writer: TBomt_SysConn_Writer;       // writer
    WConn {, WConnUpd}: TBomtSoConnection;           // classi bomt
    sConnName, sHost, sDb, sUser, sPass, sType: string; // variabili
    i: integer;
begin
   Logger.EnterIn('TBomt_SysConn_Service.GetConnessione_Update');

   result := False;

   mng:=TBomt_SystemObjectManager.Create('manager', Config, Logger);
   try
     // reader / writer
     reader:=TBomt_SysConn_Reader.Create(Config, Logger, mng);
     writer:=TBomt_SysConn_Writer.Create(Config, Logger, mng);

     // parameters
     sConnName:=Request.Params.Values['name'];
     sType:=Request.Params.Values['ConnectionType'];
     sHost:=Request.Params.Values['HostName'];
     sDb  :=Request.Params.Values['DatabaseName'];
     sUser:=Request.Params.Values['UserName'];
     sPass:=Request.Params.Values['Password'];

     // test parameters
     if sConnName='' then
        raise exception.Create('connection name required');

     // read
     i:=mng.Reader.LoadItem(sConnName);

     if i >= 0 then begin
        try
          WConn := TBomtSoConnection(mng.Data);

          // WConnUpd := TBomtSoConnection.Create(WConn.Name+'_Clone', Logger);
          // WConn.CloneValuesIn(WConnUpd);

          try

            if sType<>'' then
               WConn.ConnectionType:=sType;
            if sHost<>'' then
               WConn.HostName:=sHost;
            if sDb<>'' then
               WConn.DatabaseName:=sDb;
            if sUser<>'' then
               WConn.UserName:=sUser;
            if sPass<>'' then
               WConn.Password:=sPass;

            // validate

            // save
            mng.Writer.SaveItem;

            // build OK response
            Response.StatusCod:=200;
            Response.StatusDes:='200 (OK)';

          finally
            // FreeAndNil(WConnUpd);
          end;

        except
          on e: exception do begin
             Logger.SendError(e.Message);
             // build 500 response
             Response.StatusCod:=500;
             Response.StatusDes:='500 (Internal Server Error)';
          end;
        end;

     end else begin // patch error: not found
          Response.StatusCod:=404;
          Response.StatusDes:='404 (Not Found)';
          Logger.Send(Format('404 (Not Found)', [sConnName]));
     end;

   finally
     FreeAndNil(reader);
     FreeAndNil(writer);

     FreeAndNil(mng);
     Logger.ExitFrom('GetConnessione_Update.GetNuovo');
   end;


   result :=Response.Handled;

end;

function TBomt_SysConn_Service.GetConnessione_New: boolean;
var mng: TBomt_SystemObjectManager;     // manager
    reader: TBomt_SysConn_Reader;       // reader
    writer: TBomt_SysConn_Writer;       // writer
    WConn: TBomtSoConnection;           // classi bomt
    sConnName, sHost, sDb, sUser, sPass, sType: string; // variabili
    i: integer;
begin
   Logger.EnterIn('TBomt_SysConn_Service.GetNuovo');

   result := False;

   mng:=TBomt_SystemObjectManager.Create('manager', Config, Logger);
   try
     // reader / writer
     reader:=TBomt_SysConn_Reader.Create(Config, Logger, mng);
     writer:=TBomt_SysConn_Writer.Create(Config, Logger, mng);

     // parameters
     sConnName:=Request.Params.Values['name'];
     sType:=Request.Params.Values['ConnectionType'];
     sHost:=Request.Params.Values['HostName'];
     sDb  :=Request.Params.Values['DatabaseName'];
     sUser:=Request.Params.Values['UserName'];
     sPass:=Request.Params.Values['Password'];

     // test parameters
     if sConnName='' then
        raise exception.Create('connection name required');

     // read
     i:=mng.Reader.LoadItem(sConnName);

     if i < 0 then begin
        try
          // Response.ResponseData:=TJSONObject.Create;

          WConn := TBomtSoConnection.Create(sConnName, Logger);
          try
            mng.Add(WConn);

            WConn.ConnectionType :=sType;
            WConn.HostName       :=sHost;
            WConn.DatabaseName   :=sDb;
            WConn.UserName       :=sUser;
            WConn.Password       :=sPass;

            // validate

            // save
            mng.Writer.SaveItem;

            // build OK response
            Response.StatusCod:=200;
            Response.StatusDes:='200 (OK)';

          finally
            // FreeAndNil(WConn);
          end;

        except
          on e: exception do begin
             Logger.SendError(e.Message);
             // build 500 response
             Response.StatusCod:=500;
             Response.StatusDes:='500 (Internal Server Error)';
          end;
        end;

     end else begin // post error: ID exists
        Response.StatusCod:=404;
        Response.StatusDes:='404 (Connection already exists)';
        Logger.Send(Format('404 - Connection %s exists!', [sConnName]));
     end;

   finally
     FreeAndNil(reader);
     FreeAndNil(writer);

     FreeAndNil(mng);
     Logger.ExitFrom('TBomt_SysConn_Service.GetNuovo');
   end;


   result :=Response.Handled;

end;

function TBomt_SysConn_Service.GetConnessione_Read: boolean;
var mng: TBomt_SystemObjectManager;     // manager
    reader: TBomt_SysConn_Reader;       // reader
    WConn: TBomtSoConnection;           // classi bomt
    sConnName: string; // variabili
    i: integer;
begin
   Logger.EnterIn('TBomt_SysConn_Service.GetConnessione_Read');

   result := False;
   Response.StatusCod:=0;
   Response.StatusDes:='';
   Response.Handled:=True;


   mng:=TBomt_SystemObjectManager.Create('manager', Config, Logger);
   try
     // reader
     reader:=TBomt_SysConn_Reader.Create(Config, Logger, mng);
     // reader.Items:=mng.Items;  // mettere nel setter
     // mng.Reader:=reader;

     // parameters
     sConnName:=Request.Params.Values['name'];
     i:=mng.Reader.LoadItem(sConnName);

     if i >= 0 then begin
        try

          WConn := TBomtSoConnection(mng.Data);
          Response.ResponseData:=TJSONObject.Create;
          try
            Response.ResponseData.Add('Name', WConn.Name);
            Response.ResponseData.Add('ConnectionType', WConn.ConnectionType);
            Response.ResponseData.Add('HostName', WConn.HostName);
            Response.ResponseData.Add('DatabaseName', WConn.DatabaseName);
            Response.ResponseData.Add('UserName', WConn.UserName);

            // validate

            // save

            // build OK response
            Response.StatusCod:=200;
            Response.StatusDes:='200 (OK)';

          finally
            // FreeAndNil(WConn);
          end;

        except
          on e: exception do begin
             Logger.SendError(e.Message);
             // build 500 response
             Response.StatusCod:=500;
             Response.StatusDes:='500 (Internal Server Error)';
          end;
        end;

     end else begin // post error: ID exists
        Response.StatusCod:=404;
        Response.StatusDes:='404 (Not Found)';
        Logger.Send(Format('404 (Not Found)', [sConnName]));
     end;

   finally
     FreeAndNil(reader);

     FreeAndNil(mng);
     Logger.ExitFrom('TBomt_SysConn_Service.GetConnessione_Read');
   end;

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
  const ALogger: TBomt_Logger; const AManager: TBomt_SystemObjectManager);
begin
  ALogger.Send('TBomt_Sys_Reader.Create');

  inherited Create(AConfig, ALogger, AManager);

  FConnFolder:=Config.Options['Sys'].Values['sysfolder'];
  ALogger.Watch('folder', FConnFolder);

end;

destructor TBomt_SysConn_Reader.Destroy;
begin
  // free resources

  inherited Destroy;
end;

function TBomt_SysConn_Reader.LoadItem(const ASid: TBomt_SID): integer;
var s: string;
    WConn: TBomtSoConnection;
    ConnDef: TIniFile;
begin
  result := -1;

  if ASid <> '' then begin
     // s:=FConnFolder + PathDelim + ASid;
     s:=Format(CONNECTION_PATH_MASK, [FConnFolder, ASid]);
     if FileExists(s) then begin
        WConn:=TBomtSoConnection.Create(ASid,Logger);
        try
          ConnDef:=TIniFile.Create(s);
          try
            WConn.ConnectionType :=ConnDef.ReadString('Connection', 'ConnectionType', '');
            WConn.HostName       :=ConnDef.ReadString('Connection', 'HostName', '');
            WConn.DatabaseName   :=ConnDef.ReadString('Connection', 'DatabaseName', '');
            WConn.UserName       :=ConnDef.ReadString('Connection', 'UserName', '');
            WConn.Password       :=ConnDef.ReadString('Connection', 'Password', '');
            // result := Items.Add(WConn);
            result := Manager.Add(WConn);

          finally
            FreeAndNil(ConnDef);
          end;
        except
          on e: exception do begin
             Logger.SendError(e.Message);
          end;
        end;
     end;
  end;

end;

procedure TBomt_SysConn_Reader.LoadAll;
begin
   raise Exception.Create('Operation not supported');
end;


initialization

   Bomt_RegisterService(TBomt_SysConn_Service);

end.

