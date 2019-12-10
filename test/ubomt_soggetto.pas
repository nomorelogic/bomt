unit ubomt_soggetto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , fgl
  , ubomt;


type
  { Tipi dato Soggetto }

  TBomt_Soggetto_Codice = TBomt_SID;
  TBomt_Soggetto_Tipo   = (bomt_ts_NonDefinito, bomt_ts_Fisico, bomt_ts_Giuridico);

  { classi }

  TBomt_Soggetto_Indirizzo      = class;
  TBomt_Soggetto_ListaIndirizzi = specialize TFPGObjectList<TBomt_Soggetto_Indirizzo>;


  { TBomt_Soggetto }

  TBomt_Soggetto = class(TBomt_Object)
  private
    FCodice        : TBomt_Soggetto_Codice;
    FIndirizzi     : TBomt_Soggetto_ListaIndirizzi;
    FRagioneSociale: TBomt_Descrizione;
    FTipo          : TBomt_Soggetto_Tipo;

    procedure Clear;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Codice        : TBomt_Soggetto_Codice read FCodice write FCodice;
    property RagioneSociale: TBomt_Descrizione read FRagioneSociale write FRagioneSociale;
    property Tipo          : TBomt_Soggetto_Tipo read FTipo write FTipo;
    property ListaIndirizzi: TBomt_Soggetto_ListaIndirizzi read FIndirizzi;
  end;

  { TBomt_Soggetto_Indirizzo }

  TBomt_Soggetto_Indirizzo = class(TBomt_Indirizzo)
  private
    FCodice       : TBomt_Indirizzo_Codice;
    FDataFineVal  : TDate;
    FDataInizioVal: TDate;
    FDatiIndirizzo: TBomt_Indirizzo;
    FDescrizione  : TBomt_Descrizione;
    FTipo         : TBomt_Indirizzo_Tipo;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Codice       : TBomt_Indirizzo_Codice read FCodice write FCodice;
    property Descrizione  : TBomt_Descrizione read FDescrizione write FDescrizione;
    property Tipo         : TBomt_Indirizzo_Tipo read FTipo write FTipo;
    property DataInizioVal: TDate read FDataInizioVal write FDataInizioVal;
    property DataFineVal  : TDate read FDataFineVal write FDataFineVal;
    property DatiIndirizzo: TBomt_Indirizzo read FDatiIndirizzo write FDatiIndirizzo;
  end;





implementation

{ TBomt_Soggetto_Indirizzo }

constructor TBomt_Soggetto_Indirizzo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  IsRdbmsMapped:=True;
end;

{ TBomt_Soggetto }

procedure TBomt_Soggetto.Clear;
begin
   FCodice := '';
   FRagioneSociale:='';
   FTipo:=bomt_ts_NonDefinito;
end;

constructor TBomt_Soggetto.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  IsRdbmsMapped:=True;

  Clear;
end;

end.

