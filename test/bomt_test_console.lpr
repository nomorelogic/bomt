program bomt_test_console;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  , ubomt
  // , ubomt_soggetto
  // , ubomt_soggetto_rdbms_ahr
  // , ubomt_persistence
  // , ubomt_so_connection
  // , ubomt_rest
  , ubomt_svc_authenticate
  , fpjson
  ;

type

  { TBomtTestApplication }

  TBomtTestApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    FMethod: TBomt_RequestMethod;
    FParams: TStringList;
    FServiceName: string;
    FSession: string;
    FToken: string;
    procedure ExecuteService(const AConfig: TBomt_AppConfig; const ALogger: TBomt_Logger);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property ServiceName: string read FServiceName write FServiceName;
    property Method: TBomt_RequestMethod read FMethod write FMethod;
    property Token: string read FToken write FToken;
    property Session: string read FSession write FSession;
    property Params: TStringList read FParams write FParams;

  end;

{ TBomtTestApplication }

procedure TBomtTestApplication.DoRun;
var
  ErrorMsg: String;
  cnf: TBomt_AppConfig;
  blog: TBomt_Logger;
  sPar: string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hsmtxp', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('s', 'service') then
     FServiceName:=GetOptionValue('s');

  if HasOption('m', 'method') then begin
     case UpperCase(GetOptionValue('m')) of
        'GET': FMethod:=brkDataGet;
        'PUT': FMethod:=brkDataPut;
     else
       FMethod:=brkOptions;
     end;
  end;

  if HasOption('t', 'token') then
     FToken:=GetOptionValue('t');

  if HasOption('x', 'session') then
     FSession:=GetOptionValue('x');

  if HasOption('p', 'params') then begin
     sPar:=GetOptionValue('p');
     FParams.Text:=StringReplace(sPar, ';', #$D#$A, [rfReplaceAll]);
  end;

  // config
  cnf:=TBomt_AppConfig.Create('/media/dati/dev/llab2/bomt/test/bomt.ini');

  // logger
  blog:=TBomt_Logger.Create(cnf);

  try
    blog.Send('service ' + ServiceName);

    case FMethod of
       brkDataGet: blog.Send('method brkDataGet');
       brkDataPut: blog.Send('method brkDataPut');
       brkOptions: blog.Send('method brkOptions');
    else
       blog.Send('method unknown');
    end;
    blog.Watch('token', Token);
    blog.Watch('session', Session);
    blog.Watch('params', sPar);

    ExecuteService(cnf, blog);
  finally
    FreeAndNil(cnf);
    FreeAndNil(blog);
  end;




  // stop program loop
  Terminate;
end;

procedure TBomtTestApplication.ExecuteService(const AConfig: TBomt_AppConfig;
  const ALogger: TBomt_Logger);
var req: TBomt_Request;
    res: TBomt_Response;
    svc: TBomt_Service; // TBomt_Auth_Service;
    jResp, jo: TJSONObject;
begin

  ALogger.Send('Main execute: BEGIN');

  // richiesta
  req:=TBomt_Request.Create;
  req.AuthToken   := Token;
  req.Session     := Session;
  req.RequestKind := Method;
  req.EndPoint    := ServiceName;
  req.Params.Text := Params.Text;

  // response
  res:=TBomt_Response.Create;

  // servizio
  // svc := TBomt_Auth_Service.Create(req, res, cnf, blog);
  if ServiceList.IndexOf(ServiceName) >= 0 then begin
     ALogger.Send('Main execute - Found: ' + ServiceName);
     ALogger.Send('Main execute - Class: ' + ServiceList[ServiceName].ClassName);
  end else begin
     ALogger.Send('Main execute - NO SERVICE FOUND!');
  end;

  svc := ServiceList[ServiceName].Create(req, res, AConfig, ALogger);
  jResp:=TJSONObject.Create;
  try

    try
      ALogger.Send('Main execute - run service');
      svc.Execute;
      ALogger.Send('Main execute - end of run (service)');
      jo:=TJSONObject.Create;
      jo.Add('Code', res.StatusCod);
      jo.Add('Description', res.StatusDes);
      jResp.Add('Status', jo);

      if Assigned(res.ResponseData) then
         jResp.Add('Data', res.ResponseData);

      if Assigned(res.ResponseUtils) then
         jResp.Add('Utils', res.ResponseUtils);

      if Assigned(res.DocTech) then
         jResp.Add('DocTech', res.DocTech);

      if Assigned(res.DocUser) then
         jResp.Add('DocUser', res.DocUser);

      ALogger.Send(jResp.AsJSON);
      writeln(jResp.AsJSON);

    except
      on e: Exception do begin
        ALogger.Send('*** ERROR ***');
        ALogger.Send(e.Message);
      end;
    end;


  finally
    FreeAndNil(jResp);
    FreeAndNil(svc);
    FreeAndNil(req);
    FreeAndNil(res);
  end;

  ALogger.Send('Main execute: END');
end;

constructor TBomtTestApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FParams:=TStringList.Create;
end;

destructor TBomtTestApplication.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TBomtTestApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TBomtTestApplication;
begin
  Application:=TBomtTestApplication.Create(nil);
  Application.Title:='Bomt Test Application';
  Application.Run;
  Application.Free;
end.

