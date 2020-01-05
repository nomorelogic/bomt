unit ubomt_persistence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , fgl
  ;

type
  { Tipo di persistenza }
  TBomt_Persistence_Type = (bomt_per_None, bomt_per_Firebird, bomt_per_MySql, bomt_per_AhrMsSql);

  { TBomt_Persistence }

  TBomt_Persistence = class(TComponent)
  private
    FCanCreate: boolean;
    FCanDelete: boolean;
    FCanRead: boolean;
    FCanUpdate: boolean;
    FCanUpdateSchema: boolean;
    FName: string;
    FPersistenceType: TBomt_Persistence_Type;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property PersistenceType: TBomt_Persistence_Type read FPersistenceType write FPersistenceType;
    property CanUpdateSchema: boolean read FCanUpdateSchema write FCanUpdateSchema;
    property CanCreate: boolean read FCanCreate write FCanCreate;
    property CanRead: boolean read FCanRead write FCanRead;
    property CanUpdate: boolean read FCanUpdate write FCanUpdate;
    property CanDelete: boolean read FCanDelete write FCanDelete;
  end;


  { Lista tipo persistenza }
  TBomt_Persistence_List = specialize TFPGObjectList<TBomt_Persistence>;


implementation

{ TBomt_Persistence }

constructor TBomt_Persistence.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPersistenceType:=bomt_per_None;
  FCanUpdateSchema:=False;

  FCanCreate:=False;
  FCanDelete:=False;
  FCanRead:=False;
  FCanUpdate:=False;
end;

destructor TBomt_Persistence.Destroy;
begin
  inherited Destroy;
end;

end.

