unit ubomt_rest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , fpjson
  // , fpjsontopas
  , ubomt
  ;


type
                       // 400 (BAD REQUEST)
  TBomt_RequestKind = (brkDataGet,     // SELECT
                                       // Collection: http://www.example.com/bomt/company/clienti
                                       //   success - 200 (OK)
                                       //   fail    - 204 (No Content)
                                       // Item      : http://www.example.com/bomt/company/clienti/1234
                                       //   success - 200 (OK)
                                       //   fail    - 404 (Not Found)

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

  // TBomt_RequestKind_Set = set of TBomt_RequestKind;


  { TBomt_Request }

  TBomt_Request = class
  private
    FAuthToken: TBomt_SID;
    FDataId: TBomt_ArrayOf_SID;
    FRequestKind: TBomt_RequestKind;
    FSession: TBomt_SID;
  published
    property RequestKind: TBomt_RequestKind read FRequestKind write FRequestKind;
    property AuthToken: TBomt_SID read FAuthToken write FAuthToken;
    property Session: TBomt_SID read FSession write FSession;
    property DataId: TBomt_ArrayOf_SID read FDataId write FDataId;
  end;



  // TBomt_RestResponseFactory = class
  // end;
  // TBomt_RestAuthentication = class
  // end;



implementation

end.

