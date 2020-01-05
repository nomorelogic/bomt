unit ubomt_soggetto_rdbms_ahr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , ubomt_soggetto;


type

  { TBomt_Mapping_rdbms }

  TBomt_Mapping_rdbms = Class(TComponent)
  published
  end;

  { TBomt_Soggetto_rdbms_ahr }

  TBomt_Soggetto_rdbms_ahr = Class(TBomt_Mapping_rdbms)

  private
    FBusinessObject: TBomt_Soggetto;
    FMapClass: TStringList;
    FMapSelectFields: TStringList;
    FName: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property BusinessObject: TBomt_Soggetto read FBusinessObject write FBusinessObject;
    property MapClass: TStringList read FMapClass;
    property MapSelectFields: TStringList read FMapSelectFields;

    // property
    // property ANTIPCON
    // property ANCODCON
    // property DES_DIVE

  end;




implementation

{ TBomt_Soggetto_rdbms_ahr }

constructor TBomt_Soggetto_rdbms_ahr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMapClass:=TStringList.Create;
  FMapSelectFields:=TStringList.Create;

  MapClass.Values['TBomt_Soggetto'] := 'CONTI';
    MapSelectFields.Values['TBomt_Soggetto.TipoRapporto'] := 'ANTIPCON';
    MapSelectFields.Values['TBomt_Soggetto.Codice'] := 'ANCODICE';
    MapSelectFields.Values['TBomt_Soggetto.RagioneSociale'] := 'ANDESCRI';

  MapClass.Values['TBomt_Soggetto.IndirizzoFatturazione'] := 'CONTI';
    MapSelectFields.Values['TBomt_Soggetto.IndirizzoFatturazione.DatiIndirizzo.Indirizzo'] := 'ANINDIRI';
    MapSelectFields.Values['TBomt_Soggetto.IndirizzoFatturazione.DatiIndirizzo.CAP'] := 'AN___CAP';
    MapSelectFields.Values['TBomt_Soggetto.IndirizzoFatturazione.DatiIndirizzo.Localita'] := 'ANLOCALI';
    MapSelectFields.Values['TBomt_Soggetto.IndirizzoFatturazione.DatiIndirizzo.Provincia'] := 'ANPROVIN';
    MapSelectFields.Values['TBomt_Soggetto.IndirizzoFatturazione.DatiIndirizzo.Nazione'] := 'ANCODNAZ';

  MapClass.Values['TBomt_Soggetto.ListaIndirizzi'] := 'DES_DIVE';
    MapSelectFields.Values['TBomt_Soggetto.TipoRapporto'] := 'DDTIPCON';
    MapSelectFields.Values['TBomt_Soggetto.Codice'] := 'DDCODCON';
    MapSelectFields.Values['TBomt_Soggetto.ListaIndirizzi.Item.DataValido'] := 'DDDATINI';
    MapSelectFields.Values['TBomt_Soggetto.ListaIndirizzi.Item.DataObsoleto'] := 'DDDATFIN';
    MapSelectFields.Values['TBomt_Soggetto.ListaIndirizzi.Item.DatiIndirizzo.Indirizzo'] := 'DDINDIRI';
    MapSelectFields.Values['TBomt_Soggetto.ListaIndirizzi.Item.DatiIndirizzo.CAP'] := 'DD___CAP';
    MapSelectFields.Values['TBomt_Soggetto.ListaIndirizzi.Item.DatiIndirizzo.Localita'] := 'DDLOCALI';
    MapSelectFields.Values['TBomt_Soggetto.ListaIndirizzi.Item.DatiIndirizzo.Provincia'] := 'DDPROVIN';
    MapSelectFields.Values['TBomt_Soggetto.ListaIndirizzi.Item.DatiIndirizzo.Nazione'] := 'DDCODNAZ';



end;

destructor TBomt_Soggetto_rdbms_ahr.Destroy;
begin
  FreeAndNil(FMapSelectFields);
  FreeAndNil(FMapClass);

  inherited Destroy;
end;

end.

