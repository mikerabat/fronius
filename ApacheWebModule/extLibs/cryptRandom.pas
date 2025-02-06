// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2024, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit cryptRandom;

interface

// interface to the CryptGenRandom windows API

// ###########################################
// #### rnd object interface
// ###########################################

type
  IRndEngine = interface
   ['{DA274E1F-B493-4E78-8B37-2EEB2D093E58}']
    procedure Init(seed : LongWord);
    function Random : byte;
  end;


function CreateRndObj : IRndEngine;

implementation

uses SysUtils,
     {$IFDEF FPC} {$IFDEF LINUX} Classes, {$ELSE} Windows, {$ENDIF} syncobjs
     {$ELSE}
     {$IF CompilerVersion >= 23.0}System.SyncObjs, Winapi.Windows
     {$ELSE} Windows, SyncObjs, {$IFEND}
     {$ENDIF};

var cs : TCriticalSection;

{$IFDEF LINUX}

type
  TLinuxOSRndEngine = class(TInterfacedObject, IRndEngine)
  private
    const cNumPreCalc = 1000;
  private
    fBuf : Array[0..cNumPreCalc-1] of byte;  // precalculated buffer
    fBufIdx : integer;
  public
    procedure Init(seed : LongWord);
    function Random : byte;

    constructor Create;
    destructor Destroy; override;
  end;

function CreateRndObj : IRndEngine;
begin
     Result := TLinuxOSRndEngine.Create;
end;

procedure TLinuxOSRndEngine.Init(seed : LongWord);
var randomFIle : File of Byte;
begin
     fBufIdx := 0;

     cs.enter;
     try
        assignFile(randomFile, '/dev/urandom');
        try
           reset(randomFile);
           blockRead(randomFile, fBuf[0], length(fBuf));
        finally
               closeFile(randomFile);
        end;
     finally
            cs.Leave;
     end;
end;

function TLinuxOSRndEngine.Random : byte;
begin
     if fBufIdx = cNumPreCalc then
        Init(0);

     Result := fBuf[fBufIdx];
     inc(fBufIdx);
end;

constructor TLinuxOSRndEngine.Create;
begin
     inherited Create;

     fBufIdx := cNumPreCalc;
end;

destructor TLinuxOSRndEngine.Destroy;
begin
     inherited Destroy;
end;

{$ELSE}

// ###########################################
// #### Function definitions
// ###########################################

type
  THCRYPTROV = pointer;

const PROV_RSA_FULL = $00000001;
      PROV_DSS = $00000003;
      PROV_RSA_AES = $00000018;
      PROV_DSS_DH = $0000000D;
      PROV_DH_SCHANNEL = $00000012;
      PROV_RSA_SCHANNEL = $0000000C;
      PROV_MS_EXCHANGE = $00000005;

      CRYPT_VERIFYCONTEXT = $F0000000;
      CRYPT_NEWKEYSET = 8;

      BCRYPT_RNG_USE_ENTROPY_IN_BUFFER = $00000001;  //
      BCRYPT_USE_SYSTEM_PREFERRED_RNG = $00000002;   // hAlgorithm needs to be null then

      STATUS_SUCCESS = 0;

// ###########################################
// #### Delayed loading
// ###########################################

type
  TCryptAcquireContext = function ( out phProv : THCRYPTROV; pszContainer : PChar; pszProvider : PChar;
                              dwProvType : DWord; dwFlags : DWord) : boolean; stdcall;
  TCryptReleaseContext = function ( hProv : THCRYPTROV; dwFlags : DWord) : boolean; stdcall;
  TCryptGenRandom = function ( hProv : THCRYPTROV; dwLen : DWORD; pbBuffer : PByte) : boolean; stdcall;

  BCrypt_ALG_HANDLE = Pointer;

  // newer BCrypt.h API
  TBCryptGenRandom = function (hAlgorith : BCRYPT_ALG_HANDLE; pbBuffer : PByte;
                               cbBuffer : ULong; dwFlags : ULong ) : Longint; stdcall;

var locADVAPIHdl : HMODULE = 0;
    locBCryptHdl : HMODULE = 0;

    locCryptAcquireContext : TCryptAcquireContext = nil;
    locCryptReleaseContext : TCryptReleaseContext = nil;
    locCryptGenRandom : TCryptGenRandom = nil;

    locBCrytGenRandom : TBCryptGenRandom = nil;

procedure InitADVAPI;
begin
     if locADVAPIHdl = 0 then
     begin
          locADVAPIHdl := LoadLibrary('Advapi32.dll');
          if locADVAPIHdl = 0 then
             RaiseLastOSError;

          locCryptAcquireContext := TCryptAcquireContext( GetProcAddress(locADVAPIHdl,  'CryptAcquireContextW') );
          locCryptReleaseContext := TCryptReleaseContext( GetProcAddress(locADVAPIHdl,  'CryptReleaseContext') );
          locCryptGenRandom := TCryptGenRandom( GetProcAddress(locADVAPIHdl,  'CryptGenRandom') );
     end;
end;

procedure InitBCrypt;
begin
     if locBCryptHdl = 0 then
     begin
          locBCryptHdl := LoadLibrary('BCrypt.dll');
          if locBCryptHdl = 0 then
             exit;

          locBCrytGenRandom := TBCryptGenRandom( GetProcAddress(locBCryptHdl, 'BCryptGenRandom') );
     end;
end;

function CryptAcquireContext( out phProv : THCRYPTROV; pszContainer : PChar; pszProvider : PChar;
                              dwProvType : DWord; dwFlags : DWord) : boolean; stdcall;
begin
     Assert(locADVAPIHdl <> 0, 'Error - call InitRandLib first');

     Result := locCryptAcquireContext(phProv, pszContainer, pszProvider, dwProvType, dwFlags);
end;

function CryptReleaseContext( hProv : THCRYPTROV; dwFlags : DWord) : boolean; stdcall;
begin
     Assert(locADVAPIHdl <> 0, 'Error - call InitRandLib first');

     Result := locCryptReleaseContext(hProv, dwFlags);
end;

function CryptGenRandom( hProv : THCRYPTROV; dwLen : DWORD; pbBuffer : PByte) : boolean; stdcall;
begin
     Assert(locADVAPIHdl <> 0, 'Error - call InitRandLib first');

     Result := locCryptGenRandom(hProv, dwLen, pbBuffer);
end;

function BCryptGenRandom(hAlgorith : BCRYPT_ALG_HANDLE; pbBuffer : PByte;
  cbBuffer : ULong; dwFlags : ULong ) : Longint;
begin
     assert(locBCryptHdl <> 0, 'Error - call InitBCrypt first');

     Result := locBCrytGenRandom( hAlgorith, pbBuffer, cbBuffer, dwFlags );
end;
// ###########################################
// #### Simple class that returns random bytes based on windows CryptGenRandom api
// ###########################################
type
  TWinRndEngine = class(TInterfacedObject, IRndEngine)
  private
    const cNumPreCalc = 1000;
  private
    fBuf : Array[0..cNumPreCalc-1] of byte;  // precalculated buffer
    fBufIdx : integer;
    fphProv : THCRYPTROV;
  public
    procedure Init(seed : LongWord);
    function Random : byte;

    constructor Create;
    destructor Destroy; override;
  end;

function CreateRndObj : IRndEngine;
begin
     Result := TWinRndEngine.Create;
end;

{ TWinRndEngine }

constructor TWinRndEngine.Create;
begin
     inherited Create;

     fBufIdx := cNumPreCalc;

     if (locBCryptHdl = 0) and (locADVAPIHdl = 0) then
     begin
          cs.Enter;
          try
             if locBCryptHdl = 0 then
                InitBCrypt;

             if locBCryptHdl = 0 then
             begin
                  InitADVAPI;

                  // create a random object context with default parameters
                  fBufIdx := cNumPreCalc;
                  if not CryptAcquireContext(fPhProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
                  begin
                       if GetLastError = LongWord( NTE_BAD_KEYSET ) then
                       begin
                            if not CryptAcquireContext(fPhProv, nil, nil, PROV_RSA_FULL, CRYPT_NEWKEYSET or CRYPT_VERIFYCONTEXT) then
                               RaiseLastOSError;
                       end
                       else
                           RaiseLastOSError;
                  end;
             end;
          finally
                 cs.Leave;
          end;

     end;
end;

destructor TWinRndEngine.Destroy;
begin
     if fphProv <> nil then
        CryptReleaseContext(fphProv, 0); 
        
     inherited;
end;

procedure TWinRndEngine.Init(seed: LongWord);
begin
     if locBCryptHdl <> 0 then
     begin
          fBufIdx := 0;
          // use default system RNG
          if BCryptGenRandom(nil, @fBuf[0], cNumPreCalc, BCRYPT_USE_SYSTEM_PREFERRED_RNG) <> STATUS_SUCCESS then
             RaiseLastOSError;
     end
     else
     begin
          // fetch new values
          fBufIdx := 0;
          if not CryptGenRandom(fphProv, cNumPreCalc*sizeof(LongWord), @fBuf[0]) then
             RaiseLastOSError;
     end;
end;

function TWinRndEngine.Random: byte;
begin
     if fBufIdx = cNumPreCalc then
        Init(0);

     // return from a set of precalculated values
     Result := fBuf[fBufIdx];
     inc(fBufIdx);
end;

{$ENDIF}

initialization
  cs := TCriticalSection.Create;
finalization
  FreeAndNil(cs);

end.
