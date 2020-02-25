unit ubomt;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

interface

uses
  Classes, SysUtils, fgl
  , ubomt_persistence
  , fpjson
  , typinfo
  , rtti
  ;


  

Const TypeNames : Array [TTypeKind] of string[15] =
                    ('Unknown','Integer','Char','Enumeration',
                     'Float','Set','Method','ShortString','LongString',
                     'AnsiString','WideString','Variant','Array','Record',
                     'Interface','Class','Object','WideChar','Bool','Int64','QWord',
                     'DynamicArray','RawInterface','ProcVar','UnicodeString','UnicodeChar',
                     'Helper','File','ClassRef','Pointer');

Const OrdinalTypes = [tkInteger,tkChar,tkENumeration,tkbool];


type

  { Tipi dato base }

  TBomt_SID = string[38];  // I:ID    I=punto di inserimento   ID=identificativo
  TBomt_SID2 = string[2];
  TBomt_SID3 = string[3];
  TBomt_NID3 = string[3];
  TBomt_SID5 = string[5];
  TBomt_SID10 = string[10];

  TBomt_ArrayOf_SID = array of TBomt_SID;

  TBomt_RequestMethod =
                      (brkDataGet,     // SELECT
                                       // Collection: http://www.example.com/bomt/company/clienti
                                       //   success - 200 (OK)
                                       //   fail    - 204 (No Content)
                                       // Item      : http://www.example.com/bomt/company/clienti/1234
                                       //   success - 200 (OK)
                                       //   fail    - 404 (Not Found)

                       brkDataQuery,   // = get

                       brkDataPost,    // INSERT
                                       // Collection: http://www.example.com/bomt/company/clienti
                                       //   success - 200 (OK)
                                       //   fail    - 500 (Internal Server Error)
                                       // Item      : http://www.example.com/bomt/company/cliente/1234
                                       //   success - 200 (OK)
                                       //   fail    - 404 (Not Found)

                       brkDataPatch,   // UPDATE
                                       // Collection: http://www.example.com/bomt/company/clienti
                                       //   success - 200 (OK)
                                       //   fail    - 405 (Method Not Allowed)
                                       // Item      : http://www.example.com/bomt/company/cliente/1234
                                       //   success - 200 (OK)
                                       //   fail    - 404 (Not Found)

                       brkDataPut,     // INSERT or UPDATE
                                       // Collection: http://www.example.com/bomt/company/clienti
                                       //   success - 201 (Created) or 200 (OK)
                                       //   fail    - 500 (Internal Server Error)
                                       // Item      : http://www.example.com/bomt/company/cliente/1234
                                       //   success - 201 (Created) or 200 (OK)
                                       //   fail    - 500 (Internal Server Error)

                       brkDataDelete,  // DELETE
                                       // Collection: http://www.example.com/bomt/company/clienti
                                       //   success - 200 (OK)
                                       //   fail    - 204 (No Content)
                                       // Item      : http://www.example.com/bomt/company/clienti/1234
                                       //   success - 200 (OK)
                                       //   fail    - 404 (Not Found)

                       brkInfoTech,    // TECH INFO
                                       // Collection: http://www.example.com/bomt/company/clienti/techinfo
                                       //   success - 200 (OK)
                                       //   fail    - 204 (No Content)
                                       // Item      : http://www.example.com/bomt/company/clienti/1234/techinfo
                                       //   405 (Method Not Allowed)


                       brkInfoUser,    // TECH INFO
                                       // Collection: http://www.example.com/bomt/company/clienti/userinfo
                                       //   success - 200 (OK)
                                       //   fail    - 204 (No Content)
                                       // Item      : http://www.example.com/bomt/company/clienti/userinfo/1234
                                       //   405 (Method Not Allowed)

                       brkOptions      // Collection: http://www.example.com/bomt/company/clienti
                                       //   success - 200 (OK)
                                       //   fail    - 204 (No Content)
                       );


  TBomt_RequestMethod_Set = set of TBomt_RequestMethod;

  TBomt_ResponseFormat = (brfUndefined, brfJson, brfOdf, brfCsv, brfPdf);

  TBomt_ResponseFormat_Set = set of TBomt_ResponseFormat;



  // - business object -------------

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


  { TBomt_Logger }

  TBomt_Logger = class
  private
    procedure DoSend(const AMsg: string; const AppendEOL: boolean=True);

  private
    FConfig: TBomt_AppConfig;
    FLogStream: TFileStream;

    FAppending: boolean;
  public
    constructor Create(const AConfig: TBomt_AppConfig); virtual;
    destructor Destroy; override;

    procedure Send(const AMsg: string);
    procedure Append(const AMsg: string);
    procedure SendEol;
    procedure Watch(const AKey, AVal: string);
    procedure Watch(const AKey: string; const AVal: Int64);
    // procedure Send(ARequest: TBomt_Request);

    property Config: TBomt_AppConfig read FConfig write FConfig;
  end;


  { TBomt_Request }

  TBomt_Request = class
  private
    FAuthToken: TBomt_SID;
    FEndPoint: string;
    FParams: TStringList;
    FRequestKind: TBomt_RequestMethod;
    FSession: TBomt_SID;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property RequestKind: TBomt_RequestMethod read FRequestKind write FRequestKind;
    property AuthToken: TBomt_SID read FAuthToken write FAuthToken;
    property Session: TBomt_SID read FSession write FSession;
    property EndPoint: string read FEndPoint write FEndPoint;
    property Params: TStringList read FParams;
  end;


  { TBomt_Response }

  TBomt_Response = class
  private
    FDocTech: TJSONObject;
    FDocUser: TJSONObject;
    FHandled: boolean;
    FResponseData: TJSONObject;
    FResponseUtils: TJSONObject;
    FStatusCod: integer;
    FStatusDes: string;
  public
    constructor Create; virtual;

    property StatusCod: integer read FStatusCod write FStatusCod;
    property StatusDes: string read FStatusDes write FStatusDes;
    property Handled: boolean read FHandled write FHandled;

    property ResponseData: TJSONObject read FResponseData write FResponseData;
    property ResponseUtils: TJSONObject read FResponseUtils write FResponseUtils;
    property DocTech: TJSONObject read FDocTech write FDocTech;
    property DocUser: TJSONObject read FDocUser write FDocUser;
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


  { TBomt_Reader }

  TBomt_Reader = class

  private
    FConfig: TBomt_AppConfig;
    FItems: TBomt_SystemObject_List;
    FLogger: TBomt_Logger;
  public
    constructor Create; virtual;
    constructor Create(const AConfig: TBomt_AppConfig; const ALogger: TBomt_Logger); virtual; overload;


    function LoadItem(const ASid: TBomt_SID): integer; virtual; abstract;
    procedure LoadAll; virtual; abstract;


    // injectable
    property Config: TBomt_AppConfig read FConfig write FConfig;
    property Logger: TBomt_Logger read FLogger write FLogger;
    property Items: TBomt_SystemObject_List read FItems write FItems;

  end;

  { TBomt_Writer }

  TBomt_Writer = class

  private
    FConfig: TBomt_AppConfig;
    FItems: TBomt_SystemObject_List;
  public
    constructor Create;

    procedure SaveItem; virtual; abstract;
    procedure SaveAll; virtual; abstract;

    // injectable
    property Config: TBomt_AppConfig read FConfig write FConfig;
    property Items: TBomt_SystemObject_List read FItems write FItems;

  end;


  { TBomt_SystemObjectManager }

  TBomt_SystemObjectManager = class(TPersistent)
  private
    FName: string;
    FData: TBomt_SystemObject;
    FEof: boolean;
    FItemIndex: Int64;
    FItems: TBomt_SystemObject_List;
    function GetData: TBomt_SystemObject;
  private
    FConfig: TBomt_AppConfig;
    FLogger: TBomt_Logger;
    FReader: TBomt_Reader;
    FWriter: TBomt_Writer;
    function CanValidate: boolean;

    // abstract
    // procedure DoValidateItem; virtual; abstract;

    function DoNew(const AName: string): Int64; virtual;
    function GetEof: boolean;
    procedure SetLogger(AValue: TBomt_Logger);
    procedure SetReader(AValue: TBomt_Reader);
    procedure SetWriter(AValue: TBomt_Writer);

  public
    constructor Create(const AName: string; const AConfig: TBomt_AppConfig; const ALogger: TBomt_Logger); virtual; overload;
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    function New(const AName: string): Int64; virtual;

    // abstract
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
    property Config: TBomt_AppConfig read FConfig write FConfig;
    property Logger: TBomt_Logger read FLogger write SetLogger;
    property Reader: TBomt_Reader read FReader write SetReader;
    property Writer: TBomt_Writer read FWriter write SetWriter;
    // validator

  end;


  { TBomt_MethodAttribute }

  TBomt_MethodAttribute = class(TCustomAttribute)
  private
    FDefaultResponseformat: TBomt_ResponseFormat;
    FMethodsHandled: TBomt_RequestMethod_Set;
    FResponseFormats: TBomt_ResponseFormat_Set;
  public
    constructor Create(const AMethodsHandled: TBomt_RequestMethod_Set);overload;
  published
    property MethodsHandled: TBomt_RequestMethod_Set read FMethodsHandled write FMethodsHandled;
    property ResponseFormats: TBomt_ResponseFormat_Set read FResponseFormats write FResponseFormats;
    property DefaultResponseformat: TBomt_ResponseFormat read FDefaultResponseformat write FDefaultResponseformat;
  end;


  { TBomt_ServiceAttribute }

  TBomt_ServiceAttribute = class(TCustomAttribute)
  private
    FDescription: string;
    FEndPoint: string;
  public
    constructor Create(const AEndPoint, ADescription: string);overload;
  published
    property EndPoint: string read FEndPoint write FEndPoint;
    property Description: string read FDescription write FDescription;
  end;


  { TBomt_Service }

  [TBomt_Service('xxx', 'Servizio per autenticazione utenti e validazione tokens')]
  TBomt_Service = class
  private
    FConfig: TBomt_AppConfig;
    FLogger: TBomt_Logger;
    FRequest: TBomt_Request;
    FResponse: TBomt_Response;
  public
    // abstract
    function DoExecute: boolean; virtual; abstract;
  public
    constructor Create; virtual;
    constructor Create(const ARequest: TBomt_Request; const AResponse: TBomt_Response;
                       const AConfig: TBomt_AppConfig; const ALogger: TBomt_Logger); overload;

    destructor Destroy; override;

    function Execute: boolean; virtual;

    property Request: TBomt_Request read FRequest write FRequest;
    property Response: TBomt_Response read FResponse write FResponse;
    property Config: TBomt_AppConfig read FConfig write FConfig;
    property Logger: TBomt_Logger read FLogger write FLogger;
  end;


  TServiceClass = class of TBomt_Service;

  { TBomt_HashServiceList }
  TBomt_HashServiceList = specialize TFPGMap<string,TServiceClass>;


  // -----------
  // bomt system objscts
  // -----------


  { TBomtSoUser }

  TBomtSoUser = class(TBomt_SystemObject)
  private
    FPassword: string;
    FRole: string;
    FSessionLife: Int64;
    FUserName: string;
  published
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Role: string read FRole write FRole;
    property SessionLife: Int64 read FSessionLife write FSessionLife;
  end;



  // -----------
  // sotto: esperimenti, da non considerare
  // -----------

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



var
  ServiceList: TBomt_HashServiceList;


  procedure Bomt_RegisterService(const AClassService: TServiceClass);
  Procedure DumpTypeInfo (O : TBomt_Service);
  Procedure TestGet (O : TBomt_Service; const AName: string = '');



implementation


uses IniFiles;


// functions and procedures

procedure Bomt_RegisterService(const AClassService: TServiceClass);
var Context : TRttiContext;
    AType : TRttiType;
    Attribute : TCustomAttribute;
begin
  // here- mettere come class procedure?

  Context := TRttiContext.Create;
  try
    // AType := Context.GetType(typeinfo(AClassService)); // error
    AType := Context.GetType(AClassService);
    for Attribute in  AType.GetAttributes do begin
      // if Attribute is TBomt_ServiceAttribute then
      //    writeln('attr: ', TBomt_ServiceAttribute(Attribute).EndPoint);
      ServiceList[TBomt_ServiceAttribute(Attribute).EndPoint]:= AClassService;
    end;
  finally
    Context.Free
  end;

end;


Function ProcType (PP : Byte) : String;

begin
  Case PP and 3 of
    ptfield   : Result:='from Field';
    ptstatic  : Result:='with static method';
    ptVirtual : Result:='with virtual method';
    ptconst   : Result:='with Const';
   end;
end;



Procedure DumpTypeInfo (O : TBomt_Service);

Var
    PT : PTypeData;
    PI : PTypeInfo;
    I  : Longint;
    PP : PPropList;

begin
  PI:=O.ClassInfo;
  Writeln ('Type kind : ',TypeNames[PI^.Kind]);
  Writeln ('Type name : ',PI^.Name);
  PT:=GetTypeData(PI);
  //DumpMem(PByte(PI));
  If PT^.ParentInfo=Nil then
    Writeln ('Object has no parent info')
  else
    Writeln ('Object has parent info');
  Writeln ('Property Count : ',PT^.PropCount);
  Writeln ('Unit name      : ',PT^.UnitName);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  For I:=0 to PT^.PropCount-1 do
  If PP^[i]<>Nil then
    With PP^[I]^ do
      begin
      Writeln ('Property name : ',Name);
      Writeln (' Type kind: ',TypeNames[PropType^.Kind]);
      Writeln (' Type Name: ',PropType^.Name);
      If GetProc=Nil then Write ('No');
      Writeln (' Getproc available');
      If SetProc=Nil then Write ('No');
      Writeln (' Setproc available');
      If StoredProc=Nil then Write ('No');
      Writeln (' Storedproc available');
      Writeln (' Get property ',proctype(Propprocs));
      Writeln (' Set Property ',proctype(propprocs shr 2));
      Writeln (' Stored Property ',proctype(propprocs shr 4));
      Writeln (' Default : ',Default,' Index : ',Index);
      Writeln (' NameIndex : ',NameIndex);
      end;
    FreeMem (PP);
end;


Procedure TestGet (O : TBomt_Service; const AName: string = '');

Var
    PT : PTypeData;
    PI : PTypeInfo;
    I,J : Longint;
    PP : PPropList;
    prI : PPropInfo;
    Intf : IInterface;
begin
  PI:=O.ClassInfo;
  Writeln ('Type kind : ',TypeNames[PI^.Kind]);
  Writeln ('Type name : ',PI^.Name);
  PT:=GetTypeData(PI);
  If PT^.ParentInfo=Nil then
    Writeln ('Object has no parent info')
  else
    Writeln ('Object has parent info');
  Writeln ('Property Count : ',PT^.PropCount);
  Writeln ('Unit name      : ',PT^.UnitName);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  For I:=0 to PT^.PropCount-1 do
    begin
    pri:=PP^[i];

    if (AName<>'') and (AName<>Pri^.Name) then
       continue;

    With Pri^ do
      begin
      Write ('(Examining ',name,' : Type : ',TypeNames[PropType^.Kind],', ');
      If (Proptype^.kind in Ordinaltypes) Then
        begin
        J:=GetOrdProp(O,pri);
        Write ('Value : ',j);
        If PropType^.Kind=tkenumeration then
          Write ('(=',GetEnumName(Proptype,J),')')
        end
      else
        Case pri^.proptype^.kind of
          tkfloat :  begin
                     Write ('Value : ');
                     Flush(output);
                     Write(GetFloatProp(O,pri))
                     end;
        tkAstring : begin
                    Write ('value : ');
                    flush (output);
                    Write(GetStrProp(O,Pri));
                    end;
        tkInterface : begin
                       Write ('value : ');
                       flush (output);
                       Write(PtrUInt(GetInterfaceProp(O,Pri)));
                       { play a little bit with the interface to test SetInterfaceProp }
                       SetInterfaceProp(O,Pri,TInterfacedObject.Create);
                     end;
        tkClass   : begin
                       Write ('value : ');
                       flush (output);
                       Write(PtrUInt(GetObjectProp(O,Pri)));
                     end;
        else
          Write ('Untested type:',ord(pri^.proptype^.kind));
        end;
          Writeln (')');
      end;
    end;
  FreeMem (PP);
end;

// functions and procedures



{ TBomt_ServiceAttribute }

constructor TBomt_ServiceAttribute.Create(const AEndPoint, ADescription: string);
begin
  FEndPoint:=AEndPoint;
  FDescription:=ADescription;
end;

{ TBomt_MethodAttribute }

constructor TBomt_MethodAttribute.Create(const AMethodsHandled: TBomt_RequestMethod_Set);
begin
  FMethodsHandled := AMethodsHandled;
  FDefaultResponseformat:= brfJson;
  FResponseFormats:= [brfJson, brfOdf, brfCsv, brfPdf];
end;


{ TBomt_Logger }

procedure TBomt_Logger.DoSend(const AMsg: string; const AppendEOL: boolean);
var n:longint;
    s:string;
begin
  s:=AMsg;
  if AppendEOL then
     s+=#$A;
  n := Length(s);
  FLogStream.Write(s[1], n);
end;

constructor TBomt_Logger.Create(const AConfig: TBomt_AppConfig);
var s: string;
begin
   Config:=AConfig;

   s:=Config.Options['Log'].Values['Folder'];
   if not DirectoryExists(s)
      then ForceDirectories(s);

   s:=s+PathDelim+Format('%s.log', [FormatDateTime('yyyy-mm-dd_hhnnss_zzz', now)]);
   FLogStream:=TFileStream.Create(s, fmCreate);
   FLogStream.Position := 0;
   FAppending:=False;
end;

destructor TBomt_Logger.Destroy;
begin
   FLogStream.Free;
end;

procedure TBomt_Logger.Send(const AMsg: string);
begin
   DoSend( Format('%s %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AMsg ]));
   FAppending:=False;
end;

procedure TBomt_Logger.Append(const AMsg: string);
var s: string;
begin
  s:='';
  if not FAppending then begin
     FAppending:=True;
     s:=Format('%s %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AMsg ])
  end else begin
     s:= ' - ' + AMsg;
  end;
  DoSend(s, False);
end;

procedure TBomt_Logger.SendEol;
begin
  DoSend('');
end;

procedure TBomt_Logger.Watch(const AKey, AVal: string);
begin
  Send(AKey + ' = ' + AVal);
end;

procedure TBomt_Logger.Watch(const AKey: string; const AVal: Int64);
begin
  Send(AKey + ' = ' + IntToStr(AVal));
end;

{ TBomt_Service }

constructor TBomt_Service.Create;
begin
  FConfig:=nil;
  FRequest:=nil;
  FResponse:=nil;
end;

constructor TBomt_Service.Create(const ARequest: TBomt_Request;
  const AResponse: TBomt_Response; const AConfig: TBomt_AppConfig;
  const ALogger: TBomt_Logger);
begin
  inherited Create;

  Request:=ARequest;
  Response:=AResponse;
  Config:=AConfig;
  Logger:=ALogger;

  Logger.Send(ClassName);

end;

destructor TBomt_Service.Destroy;
begin
  // by default NOT owns request and response
  // FreeAndNil(FRequest);
  // FreeAndNil(FResponse);

  if Assigned(FLogger) then
     FLogger.Send('service stop');

  inherited;
end;

function TBomt_Service.Execute: boolean;
Var PT : PTypeData;
    PI : PTypeInfo;
    I  : Longint;
    PP : PPropList;
    s: string;
    AT: PAttributeTable;
    AE: TAttributeEntry;
    Attribute : TCustomAttribute;
    AttrEntry: TAttributeEntry;
    scan: integer;
    rm: TBomt_RequestMethod;
begin
  Logger.Send('=== Begin execute');
  PI:=self.ClassInfo;


  PT:=GetTypeData(PI);
  //DumpMem(PByte(PI));
  If PT^.ParentInfo=Nil then
    Logger.Send('Object has no parent info')
  else
    Logger.Send('Object has parent info');

  s:=Format('%s %s (unit %s) / %d method(s)',
            [TypeNames[PI^.Kind],PI^.Name,PT^.UnitName,PT^.PropCount]);
  Logger.Send(s);

  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  For I:=0 to PT^.PropCount-1 do
  If PP^[i]<>Nil then
    With PP^[I]^ do
      begin
        Logger.Watch('Property name', Name);
        Logger.Watch('Type kind', TypeNames[PropType^.Kind]);
        Logger.Watch('Type Name', PropType^.Name);
        Logger.Watch('Attr. count', PP^[I]^.AttributeTable^.AttributeCount);

      end;
      AT:=PP^[I]^.AttributeTable;
      if PP^[I]^.AttributeTable^.AttributeCount > 0 then
         for scan:=0 to PP^[I]^.AttributeTable^.AttributeCount - 1 do begin
             Attribute := GetAttribute(AT,scan);
             Logger.Watch(Format('attr[%d]', [scan]), TCustomAttribute( Attribute ).ClassName);
             if Attribute is TBomt_MethodAttribute then begin
                Logger.Append('methods: ');
                for rm in TBomt_MethodAttribute( Attribute ).MethodsHandled do begin
                    Logger.Append(GetEnumName(TypeInfo(TBomt_RequestMethod), Ord(rm)) );
                end;
                Logger.SendEol;
             end;
             Attribute.Free;
             Logger.Send('--- calling service');
             // TestGet(self, PP^[I]^.Name);
             Logger.Send('--- EOF service');
         end;
    FreeMem (PP);

  result := DoExecute;
  Logger.Send('=== End execute');
end;

{ TBomt_Response }

constructor TBomt_Response.Create;
begin
  FDocTech:=nil;
  FDocUser:=nil;
  FResponseData:=nil;
  FResponseUtils:=nil
end;


{ TBomt_Request }

constructor TBomt_Request.Create;
begin
  FParams:=TStringList.Create;
end;

destructor TBomt_Request.Destroy;
begin
  FreeAndNil(FParams);

  inherited Destroy;
end;

{ TBomt_Writer }

constructor TBomt_Writer.Create;
begin
  FItems:=nil;
end;

{ TBomt_Reader }

constructor TBomt_Reader.Create;
begin
  FItems:=nil;
  FConfig:=nil;
  FLogger:=nil;
end;

constructor TBomt_Reader.Create(const AConfig: TBomt_AppConfig;
  const ALogger: TBomt_Logger);
begin
  Create;

  FConfig:=AConfig;
  FLogger:=ALogger;
end;

{ TBomt_AppConfig }

constructor TBomt_AppConfig.Create(const AFileName: string);
begin
  FOptions:=TBomt_HashStringList.Create;
  FFileName:=AFileName;
  Load;
end;

destructor TBomt_AppConfig.Destroy;
var i: integer;
begin
  // remove all instances
  for i:=0 to FOptions.Count-1 do
      TStringList(FOptions.Data[i]).Free;

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
      if FOptions.IndexOf(ASection) < 0 then
         FOptions[ASection]:=TStringList.Create;

      for i:=0 to sl.Count - 1 do begin
          FOptions[ASection].Values[ sl.Names[i] ] := sl.Values[ sl.Names[i] ];
      end;

    end; // _ReadSection;


begin
  sl:=TStringList.Create;
  cfg:=TIniFile.Create(FFileName);
  try
    // Application
    _ReadSection('Application');

    // Connections
    _ReadSection('Connections');

    _ReadSection('Sys');

    _ReadSection('Log');

  finally
    FreeAndNil(cfg);
    FreeAndNil(sl);
  end;
end;

procedure TBomt_AppConfig.Save;
var cfg: TIniFile;
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

procedure TBomt_SystemObjectManager.SetLogger(AValue: TBomt_Logger);
begin
  if FLogger=AValue then Exit;
  FLogger:=AValue;
end;

procedure TBomt_SystemObjectManager.SetReader(AValue: TBomt_Reader);
begin
  if FReader=AValue then Exit;
  FReader:=AValue;
  FReader.Items:=FItems;
end;

procedure TBomt_SystemObjectManager.SetWriter(AValue: TBomt_Writer);
begin
  if FWriter=AValue then Exit;
  FWriter:=AValue;
  FWriter.Items:=FItems;
end;

constructor TBomt_SystemObjectManager.Create(const AName: string;
  const AConfig: TBomt_AppConfig; const ALogger: TBomt_Logger);
begin
  Create(AName);

  Config:=AConfig;
  Logger:=ALogger;
end;


constructor TBomt_SystemObjectManager.Create(const AName: string);
begin
  FName:=AName;
  FItems := TBomt_SystemObject_List.Create;
  FData := nil;
  FItemIndex := -1;
  FConfig:=nil;
  FLogger:=nil;
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

{
function TBomt_SystemObjectManager.Append(const AName: string): Int64;
begin
  FItemIndex := DoAppend(AName);
  FData:=FItems[FItemIndex];
  result := FItemIndex;
end;
}


function TBomt_SystemObjectManager.ValidateItem: boolean;
begin

  if not Assigned(FData) then
     raise exception.Create('No Item to validate referenced by FData!') ;

  if not CanValidate then
     exit;


  // validate errors
  if FData.Name = '' then
     FData.Errors.Add('Nome non specificato');


  // DoValidateItem;

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


initialization
   ServiceList:=TBomt_HashServiceList.Create;

   // Bomt_RegisterService(TBomt_Service);

finalization

   //for i:=0 to ServiceList.Count-1 do
   //    TBomt_Service( ServiceList.Data[i] ).Free;

   ServiceList.Clear;
   FreeAndNil(ServiceList);

end.

