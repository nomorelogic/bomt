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
  , ubomt_svc_authenticate;

type

  { TBomtTestApplication }

  TBomtTestApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    FMethod: TBomt_RequestKind;
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
    property Method: TBomt_RequestKind read FMethod write FMethod;
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

  if HasOption('p', 'params') then
     FParams.Text:=GetOptionValue('p');

  { add your program here }

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
    blog.Send('token   ' + Token);
    blog.Send('session ' + Session);
    blog.Send('params  ' + Params.Text);

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
begin

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
  svc := ServiceList[ServiceName].Create(req, res, AConfig, ALogger);
  try

    try
      svc.Execute;

    except
      on e: Exception do begin
        ALogger.Send('*** ERROR ***');
        ALogger.Send(e.Message);
      end;
    end;


  finally
    FreeAndNil(svc);
    FreeAndNil(req);
    FreeAndNil(res);
  end;

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

