program bomt_test_console;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  , ubomt, ubomt_soggetto, ubomt_soggetto_rdbms_ahr, ubomt_persistence,
  ubomt_so_connection, ubomt_rest;

type

  { TBomtTestApplication }

  TBomtTestApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TBomtTestApplication }

procedure TBomtTestApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
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

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TBomtTestApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TBomtTestApplication.Destroy;
begin
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

