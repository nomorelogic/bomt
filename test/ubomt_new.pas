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


  TBomt_HashStringList = specialize TFPGMap<string,TStringList>;


  { TBomt_AppConfig }

  TBomt_AppConfig = class(TPersistent)

  private
    FAppName: string;
    FOptions: TBomt_HashStringList;
    FFileName: string;
  public
    constructor Create(const AFileName: string); virtual;
    destructor Destroy; override;
    procedure Load;
    procedure Save;

    property AppName: string read FAppName write FAppName;
    property Options: TBomt_HashStringList read FOptions;
  end;


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
    FName: string;
    FData: TBomt_SystemObject;
    FEof: boolean;
    FItemIndex: Int64;
    FItems: TBomt_SystemObject_List;
    function CanValidate: boolean;

    // abstract
    function DoNew(const AName: string): Int64; virtual;
    function DoAppend(const AName: string): Int64; virtual; abstract;
    procedure DoSaveItem; virtual; abstract;
    procedure DoValidateItem; virtual; abstract;
    function GetEof: boolean;

  public
    constructor Create(AName: string; AConfig: string = ''); virtual;
    destructor Destroy; override;

    function New(const AName: string): Int64; virtual;
    function Load(const AName: string): Int64; virtual;
    function Append(const AName: string): Int64; virtual;

    procedure SaveItem; virtual;
    function ValidateItem: boolean; virtual;

    procedure Clear;
    procedure First; virtual;
    procedure Prior; virtual;
    procedure Next; virtual;
    procedure Last; virtual;

  public // property
    property Name: string read FName;
    property ItemIndex: Int64 read FItemIndex write FItemIndex;
    property Items: TBomt_SystemObject_List read FItems;
    property Data: TBomt_SystemObject read FData;
    property Eof: boolean read GetEof;

    // injectable
    //property Config

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


uses IniFiles;

{ TBomt_AppConfig }

constructor TBomt_AppConfig.Create(const AFileName: string);
begin
  FOptions:=TBomt_HashStringList.Create;
  FFileName:=AFileName;
  Load;
end;

destructor TBomt_AppConfig.Destroy;
begin
  FOptions.Clear;
  FreeAndNil(FOptions);

  inherited Destroy;
end;

procedure TBomt_AppConfig.Load;
var cfg: TIniFile;
    sl: TStringList;

    procedure _ReadSection(const ASection: string);
    var i: integer;
    begin
      sl.Clear;
      if ASection='Application' then
         FAppName:=cfg.ReadString('Application', 'Name', 'unknown');

      cfg.ReadSectionRaw(ASection, sl);
      for i:=0 to sl.Count - 1 do
          FOptions[ASection].Values[ sl.Names[i] ] := sl.Values[ sl.Names[i] ];

    end; // _ReadSection;

    procedure _ReadConnections;
    begin

    end; // _ReadConnections

begin
  sl:=TStringList.Create;
  cfg:=TIniFile.Create(FFileName);
  try
    // Application
    _ReadSection('Application');

    // Connections
    _ReadSection('Connections');
    _ReadConnections;


  finally
    FreeAndNil(cfg);
  end;
end;

procedure TBomt_AppConfig.Save;
var cfg: TIniFile;
    i: integer;
begin
  cfg:=TIniFile.Create(FFileName);
  try
    cfg.WriteString('Application', 'Name', AppName);
    {
    for i := 0 to FOptionsConnection.Count - 1 do begin
        cfg.WriteString('Connections', FOptionsConnection.Names[i],
                                       FOptionsConnection.Values[FOptionsConnection.Names[i]]);
    end;
    }
  finally
    FreeAndNil(cfg);
  end;
end;

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
  end;
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

function TBomt_SystemObjectManager.DoNew(const AName: string): Int64;
begin
  result:= -1;
end;



function TBomt_SystemObjectManager.GetEof: boolean;
begin
  if FItems.Count = 0 then
     raise exception.Create('Items is empty!');

  result := FEof;
end;


constructor TBomt_SystemObjectManager.Create(AName: string; AConfig: string);
begin
  FName:=AName;
  FItems := TBomt_SystemObject_List.Create;
  FData := nil;
  FItemIndex := -1;
end;

destructor TBomt_SystemObjectManager.Destroy;
begin
  Clear;

  FreeAndNil(FItems);

  inherited Destroy;
end;


function TBomt_SystemObjectManager.New(const AName: string): Int64;
begin
  FItemIndex := DoNew(AName);
  FData:=FItems[FItemIndex];
  result := FItemIndex;
end;

function TBomt_SystemObjectManager.Load(const AName: string): Int64;
begin
   Clear;
   result := Append(AName);
end;

function TBomt_SystemObjectManager.Append(const AName: string): Int64;
begin
  FItemIndex := DoAppend(AName);
  FData:=FItems[FItemIndex];
  result := FItemIndex;
end;

procedure TBomt_SystemObjectManager.SaveItem;
begin
  DoSaveItem;
end;

function TBomt_SystemObjectManager.ValidateItem: boolean;
begin

  if not Assigned(FData) then
     raise exception.Create('No Item to validate referenced by FData!') ;

  if not CanValidate then
     exit;


  // validate errors
  if FData.Name = '' then
     FData.Errors.Add('Nome non specificato');


  DoValidateItem;

  result := Data.Errors.Count = 0;

end;

procedure TBomt_SystemObjectManager.First;
begin
   if FItems.Count = 0 then begin
      FItemIndex:= -1;
      FData:=nil;
   end else begin
      FEof:=False;
      FItemIndex:=0;
      FData:=FItems[FItemIndex];
   end;
end;

procedure TBomt_SystemObjectManager.Prior;
begin
   if FItems.Count = 0 then begin
      FItemIndex:= -1;
      FData:=nil;
   end else begin
      FEof:=False;
      if FItemIndex > 0 then
         dec(FItemIndex);
      FData:=FItems[FItemIndex];
   end;
end;

procedure TBomt_SystemObjectManager.Next;
begin
   if FItems.Count = 0 then begin
      FItemIndex:= -1;
      FData:=nil;
   end else begin
      FEof := FItemIndex = (FItems.Count - 1);
      if FItemIndex < (FItems.Count - 1) then
         inc(FItemIndex);
      FData:=FItems[FItemIndex];
   end;
end;

procedure TBomt_SystemObjectManager.Last;
begin
   if FItems.Count = 0 then begin
      FItemIndex:= -1;
      FData:=nil;
   end else begin
      FItemIndex:=FItems.Count - 1;
      FEof:=True;
      FData:=FItems[FItemIndex];
   end;
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

