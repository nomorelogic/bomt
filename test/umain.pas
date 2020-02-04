unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls
  , ubomt
  , ubomt_so_connection;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    edCfg: TEdit;
    lbSystemRoot: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConnessioni: TBomtSoConnectionManager;
    FAppConfig: TBomt_AppConfig;
  public
    procedure ReadConfig;
    procedure WriteConfig;

    procedure CreateConnectionManager;
    procedure CreateConnessioneBomt;
    procedure ValidaConnessione;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  lbSystemRoot.Caption:=ExtractFilePath(Application.ExeName);
  memo1.Clear;
  FConnessioni:=nil;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CreateConnectionManager;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CreateConnessioneBomt;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ValidaConnessione;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ReadConfig;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  WriteConfig;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FConnessioni.Free;
  FreeAndNil(FAppConfig);
end;

procedure TForm1.ReadConfig;
begin
  if Assigned(FAppConfig) then
     FreeAndNil(FAppConfig);

  FAppConfig:=TBomt_AppConfig.Create(edCfg.Text);

  memo1.Lines.AddStrings(FAppConfig.Options['AHR']);

end;

procedure TForm1.WriteConfig;
begin
  if not Assigned(FAppConfig) then begin
     ShowMessage('unable to use config');
     exit;
  end;

  FAppConfig.AppName:='bomt';
  {
  FAppConfig.OptionsConnection.Values['AHR.TYPE'] := 'MsSql';
  FAppConfig.OptionsConnection.Values['AHR.HOST'] := '10.4.4.72';
  FAppConfig.OptionsConnection.Values['AHR.DB'] := 'AHRTOMA';
  FAppConfig.OptionsConnection.Values['AHR.USER'] := 'sa';
  FAppConfig.OptionsConnection.Values['AHR.PASS'] := '@dp55SQL';
  FAppConfig.Save;
  }
end;

procedure TForm1.CreateConnectionManager;
begin
  FConnessioni:=TBomtSoConnectionManager.Create('Connections', '');
end;

procedure TForm1.CreateConnessioneBomt;
var i:int64;
begin
  memo1.Lines.Add('crea connessione bomt');
  i:=FConnessioni.New('bomt');
  memo1.Lines.Add('  creata connessione ' + IntToStr(i));

end;

procedure TForm1.ValidaConnessione;
begin
  memo1.Lines.Add('validazione connessione ' + FConnessioni.Data.Name);
  FConnessioni.ValidateItem;
  memo1.Lines.Add('- hints');
  memo1.Lines.AddStrings(FConnessioni.Data.Hints);
  memo1.Lines.Add('- warnings');
  memo1.Lines.AddStrings(FConnessioni.Data.Warnings);
  memo1.Lines.Add('- errors');
  memo1.Lines.AddStrings(FConnessioni.Data.Errors);
end;

end.

