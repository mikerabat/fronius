unit Fronius.Linux.Log;

interface

uses
  Classes, SysUtils, syncobjs;

type

    { TFroniusLogFile }

    TFroniusLogFile = class(TObject)
    private
      fLogFile : TFileStream;
      fCs : TCriticalSection;
      fLogLevel : integer;
    public
      procedure LogEvt( Sender : TObject; level : integer; const msg : string );

      procedure LogMsg( level : integer; msg : string ); overload;
      procedure LogMsg( msg : string ); overload;

      constructor Create;
      destructor Destroy; override;
    end;

implementation

uses Fronius.Consts;

{ TFroniusLogFile }

procedure TFroniusLogFile.LogEvt(Sender: TObject; level: integer;
  const msg: string);
begin
     LogMsg( level, msg );
end;

procedure TFroniusLogFile.LogMsg(level: integer; msg: string);
begin
     if level <= fLogLevel then
        LogMsg(msg);
end;

procedure TFroniusLogFile.LogMsg(msg: string);
var s : UTF8String;
begin
     s := UTF8String(msg) + slineBreak;
     fCs.Enter;
     try
        fLogFile.WriteBuffer(s[1], Length(s));
     finally
            fCs.Leave;
     end;
end;

constructor TFroniusLogFile.Create;
var conf : IFroniusConf;
    fn : string;
const cUTF8BOM : Array[0..2] of byte = ($EF, $BB, $BF);
begin
     inherited Create;

     fCS := TCriticalSection.Create;
     conf := FroniusConfig;
     fLogLevel := conf.LogLevel;

     fn := conf.LogFile;

     fLogFile := TFileStream.Create( fn, fmCreate or fmOpenWrite or fmShareDenyNone);
     fLogFile.WriteBuffer( cUTF8BOM, sizeof( cUTF8BOM ) );
end;

destructor TFroniusLogFile.Destroy;
begin
     fCs.Free;
     fLogFile.Free;
     inherited Destroy;
end;

end.

