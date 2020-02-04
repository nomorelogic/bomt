unit ubomt_so_connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , ubomt
  , sqldb, IBConnection, mssqlconn;

// definizione di oggetto dati minimo + manager
// nomenclatura
// _so_ = system object
// _bo_ = business object
// esempio so connessioni


type

  { TBomtSoConnection }

  TBomtSoConnection = class(TBomt_SystemObject)
  private
    FDatabaseName: string;
    FHostName: string;
    FPassword: string;
    FPort: integer;
    FServerType: string;
    FUserName: string;
  published
     property ServerType: string read FServerType write FServerType;
     property HostName: string read FHostName write FHostName;
     property DatabaseName: string read FDatabaseName write FDatabaseName;
     property Port: integer read FPort write FPort;
     property UserName: string read FUserName write FUserName;
     property Password: string read FPassword write FPassword;
  end;


  { TBomtSoConnectionManager }

  TBomtSoConnectionManager = class(TBomt_SystemObjectManager)
  private
    function DoNew(const AName: string): Int64; virtual; reintroduce;
    function DoAppend(const AName: string): Int64; virtual;
    procedure DoSaveItem; virtual;
    procedure DoValidateItem; virtual;
  end;

var
  x: TBomtSoConnectionManager;


implementation



{ TBomtSoConnectionManager }

function TBomtSoConnectionManager.DoNew(const AName: string): Int64;
begin
   // inherited DoNew(AName);

   ItemIndex := Items.Add( TBomtSoConnection.Create(AName) );
   result:= ItemIndex;
end;

function TBomtSoConnectionManager.DoAppend(const AName: string): Int64;
begin
   result := -1;
end;

procedure TBomtSoConnectionManager.DoSaveItem;
begin

end;

procedure TBomtSoConnectionManager.DoValidateItem;
var CurrData: TBomtSoConnection;
begin
   // -----------------------------------------------------------
   // source notes
   // -----------------------------------------------------------
   // Popolare Hints, Warning ed Errors
   // Validazione OK se (Errors.Count = 0)
   // NB: property Name è validata in TBomt_SystemObjectManager
   // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   // validate warnings
   CurrData:=TBomtSoConnection(Data);

   if CurrData.ServerType = '' then
      CurrData.Warnings.Add('Attenzione: tipo server non specificato');
   if CurrData.HostName = '' then
      CurrData.Warnings.Add('Attenzione: hostname non specificato');
   if CurrData.DatabaseName = '' then
      CurrData.Warnings.Add('Attenzione: database non specificato');
   if CurrData.UserName = '' then
      CurrData.Warnings.Add('Attenzione: username non specificato');
   if CurrData.Password = '' then
      CurrData.Warnings.Add('Attenzione: password non specificata');

   // result
   //result := CurrData.Errors.Count = 0;
end;

initialization
    // with TBomtSoConnectionManager.Create do Free;

end.

