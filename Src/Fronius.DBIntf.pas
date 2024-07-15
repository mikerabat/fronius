unit Fronius.DBIntf;

interface

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

uses SysUtils;

type
  TLogEvent = procedure(Sender : TObject; level : integer; const msg : string) of Object;
  IFroniusDBAdapter = Interface
  ['{C2311489-1BCA-4785-B33B-562DF485B294}']
    procedure SetSQLText( const sql : string );
    procedure SetParam( param : string; val : integer ); overload;
    procedure SetParamDT( param : string; val : TDateTime );
    procedure SetParam( param : string; val : double ); overload;
    procedure SetParam( param : string; val : string ); overload;

    function GetResDT( field : string; defValue : TDateTime ) : TDateTime; overload;
    function GetRes( field : string; defValue : string) : string; overload;
    function GetRes( field : string; defValue : double) : double; overload;
    function GetRes( field : String; defValue : integer) : integer; overload;

    function Eof : boolean;
    procedure Next;
    procedure ExecQuery;
    procedure Commit;
    procedure Rollback;
    procedure Close;

    function TickCnt : int64;

    procedure SetLogEvt( evt : TLogEvent );
    function ConnectToDB(out ErrMsg : string; DBName : string = '') : boolean;

    function IsConnected: boolean;
  end;


implementation

end.
