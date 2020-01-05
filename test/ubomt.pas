unit ubomt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  ,ubomt_persistence
  ;


type

  { Tipi dato base }

  TBomt_SID = string[30];
  TBomt_SID2 = string[2];
  TBomt_SID3 = string[3];
  TBomt_NID3 = string[3];
  TBomt_SID5 = string[5];
  TBomt_SID10 = string[10];

  { Tipi dato generici }

  TBomt_Descrizione = string[64];

  { Tipi dato GEO Nazione }

  TBomt_Geo_Nazione_Codice = TBomt_SID5;
  TBomt_Geo_Nazione_IsoAlfa3 = TBomt_SID3;
  TBomt_Geo_Nazione_IsoAlfa2 = TBomt_SID2;
  TBomt_Geo_Nazione_IsoNumerico3 = TBomt_NID3;
  TBomt_Geo_Nazione_IsoLocali = TBomt_SID10;

  { Tipi dato Indirizzo }

  TBomt_Indirizzo_Codice = TBomt_SID5;
  TBomt_Indirizzo_Tipo = (bomt_ti_Generico, bomt_ti_Consegna, bomt_ti_Fatturazione);


  { TBomt_Object }

  TBomt_Object = class(TComponent)
  private
    FDateQuery: TDate;
    FHasLog: boolean;
    FHasVersion: boolean;
    FPersistenceList: TBomt_Persistence_List;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const ADateQuery: TDate); overload;
    destructor Destroy; override;
  published
    property HasVersion: boolean read FHasVersion write FHasVersion;
    property HasLog: boolean read FHasLog write FHasLog;
    property PersistenceList: TBomt_Persistence_List read FPersistenceList;
    property DateQuery: TDate read FDateQuery write FDateQuery;
  end;


  { TBomt_Nazione }

  TBomt_Nazione = class(TBomt_Object)
  private
    FCodice: TBomt_SID5;
    FDescrizione: string;
    FIsoAlfa2: TBomt_SID2;
    FIsoAlfa3: TBomt_SID3;
    FIsoLocali: TBomt_SID10;
    FNumerico: TBomt_NID3;
  public
  published
    property Codice: TBomt_Geo_Nazione_Codice read FCodice write FCodice;
    property Descrizione: string read FDescrizione write FDescrizione;
    property Numerico: TBomt_Geo_Nazione_IsoNumerico3 read FNumerico write FNumerico;
    property IsoAlfa3: TBomt_Geo_Nazione_IsoAlfa3 read FIsoAlfa3 write FIsoAlfa3;
    property IsoAlfa2: TBomt_Geo_Nazione_IsoAlfa2 read FIsoAlfa2 write FIsoAlfa2;
    property IsoLocali: TBomt_Geo_Nazione_IsoLocali read FIsoLocali write FIsoLocali;
  end;


  { TBomt_Indirizzo }

  TBomt_Indirizzo = class(TBomt_Object)
  private
    FCAP: string;
    FIndirizzo: string;
    FLocalita: string;
    FNazione: TBomt_Geo_Nazione_Codice;
    FProvincia: string;
  public
  published
    property Indirizzo: string read FIndirizzo write FIndirizzo;
    property CAP: string read FCAP write FCAP;
    property Localita: string read FLocalita write FLocalita;
    property Provincia: string read FProvincia write FProvincia;
    property Nazione: TBomt_Geo_Nazione_Codice read FNazione write FNazione;
  end;



implementation

{ TBomt_Object }

constructor TBomt_Object.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHasLog:=False;
  FHasVersion:=False;
  FPersistenceList:=TBomt_Persistence_List.Create;
  FDateQuery:=0;
end;

constructor TBomt_Object.Create(AOwner: TComponent; const ADateQuery: TDate);
begin
  Create(AOwner);
  FDateQuery:=ADateQuery;
end;

destructor TBomt_Object.Destroy;
begin
  FPersistenceList.Clear;
  FreeAndNil(FPersistenceList);

  inherited Destroy;
end;

end.

