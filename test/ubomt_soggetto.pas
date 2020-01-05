unit ubomt_soggetto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , fgl
  , ubomt
  , ubomt_persistence;


type
  { Tipi dato Soggetto }

  TBomt_Soggetto_Codice = TBomt_SID;
  TBomt_Soggetto_Tipo   = (bomt_ts_NonDefinito, bomt_ts_Fisico, bomt_ts_Giuridico);
  TBomt_Soggetto_TipoRapporto = (bomt_tr_NonDefinito, bomt_tr_Cliente, bomt_tr_Fornitore);

  { classi }

  TBomt_Soggetto_Indirizzo      = class;
  TBomt_Soggetto_ListaIndirizzi = specialize TFPGObjectList<TBomt_Soggetto_Indirizzo>;


  { TBomt_Soggetto }

  TBomt_Soggetto = class(TBomt_Object)
  private
    function DoGetSedeFatturazione: TBomt_Soggetto_Indirizzo;
  private
    FCodice        : TBomt_Soggetto_Codice;
    FIndirizzi     : TBomt_Soggetto_ListaIndirizzi;
    FRagioneSociale: TBomt_Descrizione;
    FSedeFatturazione: TBomt_Soggetto_Indirizzo;
    FTipo          : TBomt_Soggetto_Tipo;
    FTipoRapporto: TBomt_Soggetto_TipoRapporto;

    procedure Clear;
    function GetSedeFatturazione: TBomt_Soggetto_Indirizzo;
  public
    constructor Create(AOwner: TComponent); override;

    // procedure Load
  published
    property Codice        : TBomt_Soggetto_Codice read FCodice write FCodice;
    property RagioneSociale: TBomt_Descrizione read FRagioneSociale write FRagioneSociale;
    property Tipo          : TBomt_Soggetto_Tipo read FTipo write FTipo;
    property TipoRapporto  : TBomt_Soggetto_TipoRapporto read FTipoRapporto write FTipoRapporto;
    property ListaIndirizzi: TBomt_Soggetto_ListaIndirizzi read FIndirizzi;
    property SedeFatturazione: TBomt_Soggetto_Indirizzo read GetSedeFatturazione;
  end;

  { TBomt_Soggetto_Indirizzo }

  TBomt_Soggetto_Indirizzo = class(TBomt_Indirizzo)
  private
    FCodice       : TBomt_Indirizzo_Codice;
    FDataValido: TDate;
    FDataObsoleto: TDate;
    FDatiIndirizzo: TBomt_Indirizzo;
    FDescrizione  : TBomt_Descrizione;
    FTipo         : TBomt_Indirizzo_Tipo;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Codice       : TBomt_Indirizzo_Codice read FCodice write FCodice;
    property Descrizione  : TBomt_Descrizione read FDescrizione write FDescrizione;
    property Tipo         : TBomt_Indirizzo_Tipo read FTipo write FTipo;
    property DataValido   : TDate read FDataValido write FDataValido;
    property DataObsoleto : TDate read FDataObsoleto write FDataObsoleto;
    property DatiIndirizzo: TBomt_Indirizzo read FDatiIndirizzo write FDatiIndirizzo;
  end;





implementation

{ TBomt_Soggetto_Indirizzo }

constructor TBomt_Soggetto_Indirizzo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

{ TBomt_Soggetto }

function TBomt_Soggetto.DoGetSedeFatturazione: TBomt_Soggetto_Indirizzo;
begin
  // legge da DB
  result := nil;
end;

procedure TBomt_Soggetto.Clear;
begin
   FCodice := '';
   FRagioneSociale:='';
   FTipo:=bomt_ts_NonDefinito;
end;

function TBomt_Soggetto.GetSedeFatturazione: TBomt_Soggetto_Indirizzo;
begin
   // cerca indirizzo fatturazione valido

   if FSedeFatturazione = nil then
      FSedeFatturazione := DoGetSedeFatturazione;

   result := FSedeFatturazione;
end;

constructor TBomt_Soggetto.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with PersistenceList[ PersistenceList.Add( TBomt_Persistence.Create(AOwner) ) ] do
    begin
       Name:='AHR';
       PersistenceType:=bomt_per_AhrMsSql;
       CanRead:=True;
    end;

  with PersistenceList[ PersistenceList.Add( TBomt_Persistence.Create(AOwner) ) ] do
    begin
       Name:='FB01';
       PersistenceType:=bomt_per_Firebird;
       CanUpdateSchema:=True;
       CanCreate:=True;
       CanRead:=True;
       CanUpdate:=True;
       CanDelete:=True;
    end;

  with PersistenceList[ PersistenceList.Add( TBomt_Persistence.Create(AOwner) ) ] do
    begin
       Name:='MY01';
       PersistenceType:=bomt_per_MySql;
       CanUpdateSchema:=True;
       CanCreate:=True;
       CanRead:=True;
       CanUpdate:=True;
       CanDelete:=True;
    end;

  Clear;
end;

end.

