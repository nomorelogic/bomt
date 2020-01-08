unit ubomt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl
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


  { TBomt_SystemObject }
  TBomt_SystemObject = class(TPersistent)

  private
    FErrors: TStringList;
    FHints: TStringList;
    FIsModified: boolean;
    FIsValid: boolean;
    FName: string;
    FWarnings: TStringList;
  public
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property IsModified: boolean read FIsModified write FIsModified;
    property IsValid: boolean read FIsValid write FIsValid;
    property Hints: TStringList read FHints write FHints;
    property Warnings: TStringList read FWarnings write FWarnings;
    property Errors: TStringList read FErrors write FErrors;
  end;


  TBomt_SystemObject_List = specialize TFPGObjectList<TBomt_SystemObject>;


  { TBomt_SystemObjectManager }

  TBomt_SystemObjectManager = class(TPersistent)
  private
    FItemIndex: UInt64;
    FItems: TBomt_SystemObject_List;
    procedure Clear;
    function GetData: TBomt_SystemObject;
  private
    function CanValidate: boolean;

    // abstract
    function DoNew(const AId: string): Int64; virtual; abstract;
    function DoLoadById(const AId: string): Int64; virtual; abstract;
    procedure DoSaveItem; virtual; abstract;
    function DoValidateItem: boolean; virtual; abstract;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Bof: boolean; virtual;
    function Eof: boolean; virtual;

    // abstract
    function New(const AId: string): Int64; virtual;
    function LoadById(const AId: string): Int64; virtual;
    procedure SaveItem; virtual;
    function ValidateItem: boolean; virtual;

    procedure First; virtual;
    procedure Prior; virtual;
    procedure Next; virtual;
    procedure Last; virtual;

  public // property
    property ItemIndex: UInt64 read FItemIndex write FItemIndex;
    property Items: TBomt_SystemObject_List read FItems;
    property Data: TBomt_SystemObject read GetData;
  end;


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

{ TBomt_SystemObject }

constructor TBomt_SystemObject.Create(const AName: string);
begin
  FName:=AName;

  FHints:=nil;
  FWarnings:=nil;
  FErrors:=nil;
end;

destructor TBomt_SystemObject.Destroy;
begin
  FreeAndNil(FErrors);
  FreeAndNil(FWarnings);
  FreeAndNil(FHints);

  inherited Destroy;
end;

{ TBomt_SystemObjectManager }

procedure TBomt_SystemObjectManager.Clear;
begin
  if Assigned(FItems) then begin
     FItems.Clear;
     FreeAndNil(FItems);
  end;
end;

function TBomt_SystemObjectManager.GetData: TBomt_SystemObject;
begin
  result := Items[FItemIndex];
end;

function TBomt_SystemObjectManager.CanValidate: boolean;
begin

  with Data do begin
    if Assigned(FHints) then
       FHints.Clear
    else
       FHints:=TStringList.Create;

    if Assigned(FWarnings) then
       FWarnings.Clear
    else
       FWarnings:=TStringList.Create;

    if Assigned(FErrors) then
       FErrors.Clear
    else
       FErrors:=TStringList.Create;
  end;

  result := true;
end;

constructor TBomt_SystemObjectManager.Create;
begin
  FItems:=TBomt_SystemObject_List.Create;
end;

destructor TBomt_SystemObjectManager.Destroy;
begin
  Clear;

  inherited Destroy;
end;

function TBomt_SystemObjectManager.Eof: boolean;
begin
  result := FItemIndex = (FItems.Count - 1);
end;

function TBomt_SystemObjectManager.New(const AId: string): Int64;
begin
  result := DoNew(AId);
end;

function TBomt_SystemObjectManager.LoadById(const AId: string): Int64;
begin
   result := DoLoadById(AId);
end;

procedure TBomt_SystemObjectManager.SaveItem;
begin
  DoSaveItem;
end;

function TBomt_SystemObjectManager.ValidateItem: boolean;
begin

  if not CanValidate then
     exit;

  result := DoValidateItem;

end;

procedure TBomt_SystemObjectManager.First;
begin
   ItemIndex:=0;
end;

procedure TBomt_SystemObjectManager.Prior;
begin
  dec(FItemIndex);
end;

procedure TBomt_SystemObjectManager.Next;
begin
  inc(FItemIndex);
end;

procedure TBomt_SystemObjectManager.Last;
begin
  FItemIndex:=fitems.Count - 1;
end;



function TBomt_SystemObjectManager.Bof: boolean;
begin
  result := FItemIndex = 0;
end;

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

