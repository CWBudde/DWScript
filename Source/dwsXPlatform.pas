{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsXPlatform;

{$I dws.inc}

//
// This unit should concentrate all non-UI cross-platform aspects,
// cross-Delphi versions, ifdefs and other conditionals
//
// no ifdefs in the main code.

{$WARN SYMBOL_PLATFORM OFF}

{-$DEFINE WindowsCaseConvert}

{$IFDEF FPC}
   {$DEFINE VER200}  // FPC compatibility = D2009
{$ENDIF}

interface

uses
   Classes, SysUtils, Types, Masks, SyncObjs, Variants, StrUtils,
   {$IFDEF FPC}
      {$IFDEF Windows}
         Windows
      {$ELSE}
         LCLIntf
      {$ENDIF}
   {$ELSE}
      {$IFDEF POSIX}
      Posix.Time, Posix.SysTime, Posix.SysTypes, Posix.Dirent, Posix.Fcntl,
      Posix.Stdio, Posix.Pthread, DateUtils
      {$IFDEF MACOS}
      , Macapi.CoreFoundation, Macapi.Foundation, Macapi.Helpers
      {$ENDIF}
      {$ELSE}
      Windows
      {$ENDIF}
      {$IFNDEF VER200}, IOUtils{$ENDIF}
   {$ENDIF}
   ;

const
{$IFDEF POSIX}
   cLineTerminator  = #10;
{$ELSE}
   cLineTerminator  = #13#10;
{$ENDIF}

   // following is missing from D2010
   INVALID_HANDLE_VALUE = NativeUInt(-1);

   {$ifdef FPC}
   // FreePascal RTL declares this constant, but does not support it,
   // so it just leads to runtime crashes, this attempts to trigger compile-time crashes instead
   varUString = 'varUString is not supported by FreePascal';
   {$endif}

type

   // see http://delphitools.info/2011/11/30/fixing-tcriticalsection/
   {$HINTS OFF}
   {$ifdef POSIX}
   TdwsCriticalSection = class (TCriticalSection);
   {$else}
   TdwsCriticalSection = class
      private
         FDummy : array [0..95-SizeOf(TRTLCRiticalSection)-2*SizeOf(Pointer)] of Byte;
         FCS : TRTLCriticalSection;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Enter;
         procedure Leave;

         function TryEnter : Boolean;
   end;
   {$endif}

   IMultiReadSingleWrite = interface
      procedure BeginRead;
      function  TryBeginRead : Boolean;
      procedure EndRead;

      procedure BeginWrite;
      function  TryBeginWrite : Boolean;
      procedure EndWrite;
   end;

   TMultiReadSingleWriteState = (mrswUnlocked, mrswReadLock, mrswWriteLock);

   {$ifdef POSIX}{$define SRW_FALLBACK}{$endif}

   TMultiReadSingleWrite = class (TInterfacedObject, IMultiReadSingleWrite)
      private
         {$ifndef SRW_FALLBACK}
         FSRWLock : Pointer;
         FDummy : array [0..95-4*SizeOf(Pointer)] of Byte; // padding
         {$else}
         FLock : TdwsCriticalSection;
         {$endif}

      public
         {$ifdef SRW_FALLBACK}
         constructor Create;
         destructor Destroy; override;
         {$endif}

         procedure BeginRead; inline;
         function  TryBeginRead : Boolean; inline;
         procedure EndRead; inline;

         procedure BeginWrite; inline;
         function  TryBeginWrite : Boolean; inline;
         procedure EndWrite; inline;

         // use for diagnostic only
         function State : TMultiReadSingleWriteState;
   end;

   {$HINTS ON}

procedure SetDecimalSeparator(c : Char);
function GetDecimalSeparator : Char;

type
   TCollectFileProgressEvent = procedure (const directory : TFileName; var skipScan : Boolean) of object;

procedure CollectFiles(const directory, fileMask : TFileName;
                       list : TStrings; recurseSubdirectories: Boolean = False;
                       onProgress : TCollectFileProgressEvent = nil);
procedure CollectSubDirs(const directory : TFileName; list : TStrings);

type
   {$IFNDEF FPC}
   {$IF CompilerVersion<22.0}
   // NativeUInt broken in D2009, and PNativeInt is missing in D2010
   // http://qc.embarcadero.com/wc/qcmain.aspx?d=71292
   NativeInt = Integer;
   PNativeInt = ^NativeInt;
   NativeUInt = Cardinal;
   PNativeUInt = ^NativeUInt;
   {$IFEND}
   {$ENDIF}

   {$IFDEF FPC}
   TBytes = array of Byte;

   RawByteString = String;

   PNativeInt = ^NativeInt;
   PUInt64 = ^UInt64;
   {$ENDIF}

   TPath = class
      class function GetTempPath : String; static;
      class function GetTempFileName : String; static;
   end;

   TFile = class
      class function ReadAllBytes(const filename : String) : TBytes; static;
   end;

   TdwsThread = class (TThread)
      {$IFNDEF FPC}
      {$IFDEF VER200}
      procedure Start;
      {$ENDIF}
      {$ENDIF}
   end;

// 64bit system clock reference in milliseconds since boot
function GetSystemMilliseconds : Int64;
function UTCDateTime : TDateTime;
function UnixTime : Int64;

function LocalDateTimeToUTCDateTime(t : TDateTime) : TDateTime;
function UTCDateTimeToLocalDateTime(t : TDateTime) : TDateTime;

function SystemMillisecondsToUnixTime(t : Int64) : Int64;
function UnixTimeToSystemMilliseconds(ut : Int64) : Int64;

procedure SystemSleep(msec : Integer);

function FirstWideCharOfString(const s : String; const default : WideChar = #0) : WideChar; inline;
procedure CodePointToUnicodeString(c : Integer; var result : UnicodeString);
procedure CodePointToString(const c : Integer; var result : String); inline;

{$ifndef FPC}
function UnicodeCompareStr(const S1, S2 : String) : Integer; inline;
function UnicodeStringReplace(const s, oldPattern, newPattern: String; flags: TReplaceFlags) : String; inline;
{$endif}

function UnicodeCompareP(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer; overload;
function UnicodeCompareP(p1, p2 : PWideChar; n : Integer) : Integer; overload;

function UnicodeLowerCase(const s : UnicodeString) : UnicodeString; overload;
function UnicodeUpperCase(const s : UnicodeString) : UnicodeString; overload;

{$ifdef FPC}
function UnicodeLowerCase(const s : String) : String; overload;
function UnicodeUpperCase(const s : String) : String; overload;
{$endif}

function ASCIICompareText(const s1, s2 : String) : Integer; inline;
function ASCIISameText(const s1, s2 : String) : Boolean; inline;

function NormalizeString(const s, form : String) : String;
function StripAccents(const s : String) : String;

function InterlockedIncrement(var val : Integer) : Integer; overload; {$IFDEF PUREPASCAL} inline; {$endif}
function InterlockedDecrement(var val : Integer) : Integer; {$IFDEF PUREPASCAL} inline; {$endif}

procedure FastInterlockedIncrement(var val : Integer); {$IFDEF PUREPASCAL} inline; {$endif}
procedure FastInterlockedDecrement(var val : Integer); {$IFDEF PUREPASCAL} inline; {$endif}

function InterlockedExchangePointer(var target : Pointer; val : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}

function InterlockedCompareExchangePointer(var destination : Pointer; exchange, comparand : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}

procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));

procedure OutputDebugString(const msg : String);

procedure WriteToOSEventLog(const logName, logCaption, logDetails : String;
                            const logRawData : RawByteString = ''); overload;

{$ifdef FPC}
procedure VarCopy(out dest : Variant; const src : Variant); inline;
{$else}
function VarToUnicodeStr(const v : Variant) : String; inline;
{$endif}

{$ifdef FPC}
function Utf8ToUnicodeString(const buf : RawByteString) : UnicodeString; inline;
{$endif}

function RawByteStringToBytes(const buf : RawByteString) : TBytes;
function BytesToRawByteString(const buf : TBytes; startIndex : Integer = 0) : RawByteString; overload;
function BytesToRawByteString(p : Pointer; size : Integer) : RawByteString; overload;

{$ifdef MSWindows}
function LoadDataFromFile(const fileName : TFileName) : TBytes;
procedure SaveDataToFile(const fileName : TFileName; const data : TBytes);

function LoadRawBytesFromFile(const fileName : TFileName) : RawByteString;
function SaveRawBytesToFile(const fileName : TFileName; const data : RawByteString) : Integer;

procedure LoadRawBytesAsScriptStringFromFile(const fileName : TFileName; var result : String);

function LoadTextFromBuffer(const buf : TBytes) : UnicodeString;
function LoadTextFromRawBytes(const buf : RawByteString) : UnicodeString;
{$endif}
function LoadTextFromStream(aStream : TStream) : UnicodeString;
function LoadTextFromFile(const fileName : TFileName) : UnicodeString;
procedure SaveTextToUTF8File(const fileName : TFileName; const text : UTF8String);
procedure AppendTextToUTF8File(const fileName : TFileName; const text : UTF8String);
function OpenFileForSequentialReadOnly(const fileName : TFileName) : THandle;
function OpenFileForSequentialWriteOnly(const fileName : TFileName) : THandle;
procedure CloseFileHandle(hFile : THandle);
{$ifdef MSWindows}
function FileWrite(hFile : THandle; buffer : Pointer; byteCount : Integer) : Cardinal;
function FileFlushBuffers(hFile : THandle) : Boolean;
function FileCopy(const existing, new : TFileName; failIfExists : Boolean) : Boolean;
function FileMove(const existing, new : TFileName) : Boolean;
function FileDelete(const fileName : TFileName) : Boolean;
function FileRename(const oldName, newName : TFileName) : Boolean;
function FileSize(const name : TFileName) : Int64;
function FileDateTime(const name : TFileName) : TDateTime;
procedure FileSetDateTime(hFile : THandle; aDateTime : TDateTime);
{$endif}
function DeleteDirectory(const path : String) : Boolean;

function DirectSet8087CW(newValue : Word) : Word; register;
function DirectSetMXCSR(newValue : Word) : Word; register;

function SwapBytes(v : Cardinal) : Cardinal;
procedure SwapInt64(src, dest : PInt64);

function RDTSC : UInt64;

function GetCurrentUserName : String;

{$ifndef FPC}
// Generics helper functions to handle Delphi 2009 issues - HV
function TtoObject(const T): TObject; inline;
function TtoPointer(const T): Pointer; inline;
procedure GetMemForT(var T; Size: integer); inline;
{$endif}

procedure InitializeWithDefaultFormatSettings(var fmt : TFormatSettings);

{$ifdef MSWindows}
type
   TTimerEvent = procedure of object;

   ITimer = interface
      procedure Cancel;
   end;

   TTimerTimeout = class (TInterfacedObject, ITimer)
      private
         FTimer : THandle;
         FOnTimer : TTimerEvent;

      public
         class function Create(delayMSec : Cardinal; onTimer : TTimerEvent) : ITimer;
         destructor Destroy; override;

         procedure Cancel;
   end;
{$endif}

{$ifndef SRW_FALLBACK}
procedure AcquireSRWLockExclusive(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
function TryAcquireSRWLockExclusive(var SRWLock : Pointer) : BOOL; stdcall; external 'kernel32.dll';
procedure ReleaseSRWLockExclusive(var SRWLock : Pointer); stdcall; external 'kernel32.dll';

procedure AcquireSRWLockShared(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
function TryAcquireSRWLockShared(var SRWLock : Pointer) : BOOL; stdcall; external 'kernel32.dll';
procedure ReleaseSRWLockShared(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
{$endif}

type
   TModuleVersion = record
      Major, Minor : Word;
      Release, Build : Word;
      function AsString : String;
   end;

{$IFDEF MSWINDOWS}
function GetModuleVersion(instance : THandle; var version : TModuleVersion) : Boolean;
function GetApplicationVersion(var version : TModuleVersion) : Boolean;
{$ENDIF}
function ApplicationVersion : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$ifdef FPC}
type
   TFindExInfoLevels = FINDEX_INFO_LEVELS;
{$endif}

// GetSystemTimeMilliseconds
//
function GetSystemTimeMilliseconds : Int64; stdcall;
{$IFDEF WINDOWS}
var
   fileTime : TFileTime;
begin
   GetSystemTimeAsFileTime(fileTime);
   Result:=Round(PInt64(@fileTime)^*1e-4); // 181
{$ELSE}
var
   fileTime : timeval;
begin
   gettimeofday(fileTime, nil);
   Result := 1000 * fileTime.tv_sec + fileTime.tv_usec;
{$ENDIF}
end;

// GetSystemMilliseconds
//
var
   vGetSystemMilliseconds : function : Int64; stdcall;
function GetSystemMilliseconds : Int64;
{$ifdef WIN32_ASM}
asm
   jmp [vGetSystemMilliseconds]
{$else}
begin
   Result:=vGetSystemMilliseconds;
{$endif}
end;

// InitializeGetSystemMilliseconds
//
procedure InitializeGetSystemMilliseconds;
var
   h : THandle;
begin
   {$IFDEF WINDOWS}
   h:=LoadLibrary('kernel32.dll');
   vGetSystemMilliseconds:=GetProcAddress(h, 'GetTickCount64');
   {$ENDIF}
   if not Assigned(vGetSystemMilliseconds) then
      vGetSystemMilliseconds:=@GetSystemTimeMilliseconds;
end;

// UTCDateTime
//
function UTCDateTime : TDateTime;
{$IFDEF Windows}
var
   systemTime : TSystemTime;
begin
   FillChar(systemTime, SizeOf(systemTime), 0);
   GetSystemTime(systemTime);
   with systemTime do
      Result:= EncodeDate(wYear, wMonth, wDay)
              +EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
var
   systemTime : CFGregorianDate;
begin
   systemTime := CFAbsoluteTimeGetGregorianDate(CFAbsoluteTimeGetCurrent, nil);
   with systemTime do
      Result := EncodeDate(year, month, day)
               +EncodeTime(hour, minute, Trunc(second), Round((second - Trunc(second)) * MSecsPerSec));
{$ELSE}
var
   systemTime : ptm;
   t : time_t;
begin
   t := time(nil);
   systemTime := gmtime(t);
   if systemTime = nil then
     raise Exception.Create('Error calling gmtime');

   with systemTime^ do
      Result := EncodeDate(1900 + tm_year, 1 + tm_mon, tm_mday)
               +EncodeTime(tm_hour, tm_min, tm_sec, 0);
{$ENDIF}
{$ENDIF}
end;

// UnixTime
//
function UnixTime : Int64;
begin
{$IFDEF POSIX}
   Result:=time(nil);
{$ELSE}
   Result:=Trunc(UTCDateTime*86400)-Int64(25569)*86400;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
type
   TDynamicTimeZoneInformation = record
      Bias : Longint;
      StandardName : array[0..31] of WCHAR;
      StandardDate : TSystemTime;
      StandardBias : Longint;
      DaylightName : array[0..31] of WCHAR;
      DaylightDate : TSystemTime;
      DaylightBias : Longint;
      TimeZoneKeyName : array[0..127] of WCHAR;
      DynamicDaylightTimeDisabled : Boolean;
   end;
   PDynamicTimeZoneInformation = ^TDynamicTimeZoneInformation;

function GetDynamicTimeZoneInformation(
      var pTimeZoneInformation: TDynamicTimeZoneInformation): DWORD; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};
function GetTimeZoneInformationForYear(wYear: USHORT; lpDynamicTimeZoneInformation: PDynamicTimeZoneInformation;
      var lpTimeZoneInformation: TTimeZoneInformation): BOOL; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation;
      var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};
{$ENDIF}

// LocalDateTimeToUTCDateTime
//
function LocalDateTimeToUTCDateTime(t : TDateTime) : TDateTime;
{$IFDEF MSWINDOWS}
var
   localSystemTime, universalSystemTime : TSystemTime;
   tzDynInfo : TDynamicTimeZoneInformation;
   tzInfo : TTimeZoneInformation;
begin
   DateTimeToSystemTime(t, localSystemTime);
   if GetDynamicTimeZoneInformation(tzDynInfo) = TIME_ZONE_ID_INVALID then
      RaiseLastOSError;
   if not GetTimeZoneInformationForYear(localSystemTime.wYear, @tzDynInfo, tzInfo) then
      RaiseLastOSError;
   if not TzSpecificLocalTimeToSystemTime(@tzInfo, localSystemTime, universalSystemTime) then
      RaiseLastOSError;
   Result := SystemTimeToDateTime(universalSystemTime);
{$ELSE}
begin
   Result := TTimeZone.Local.ToUniversalTime(t);
{$ENDIF}
end;

// UTCDateTimeToLocalDateTime
//
function UTCDateTimeToLocalDateTime(t : TDateTime) : TDateTime;
{$IFDEF MSWINDOWS}
var
   tzDynInfo : TDynamicTimeZoneInformation;
   tzInfo : TTimeZoneInformation;
   localSystemTime, universalSystemTime : TSystemTime;
begin
   DateTimeToSystemTime(t, universalSystemTime);
   if GetDynamicTimeZoneInformation(tzDynInfo) = TIME_ZONE_ID_INVALID then
      RaiseLastOSError;
   if not GetTimeZoneInformationForYear(localSystemTime.wYear, @tzDynInfo, tzInfo) then
      RaiseLastOSError;
   if not SystemTimeToTzSpecificLocalTime(@tzInfo, universalSystemTime, localSystemTime) then
      RaiseLastOSError;
   Result := SystemTimeToDateTime(localSystemTime);
{$ELSE}
begin
   Result := TTimeZone.Local.ToLocalTime(t);
{$ENDIF}
end;

// SystemMillisecondsToUnixTime
//
function SystemMillisecondsToUnixTime(t : Int64) : Int64;
begin
   Result := UnixTime - (GetSystemTimeMilliseconds-t) div 1000;
end;

// UnixTimeToSystemMilliseconds
//
function UnixTimeToSystemMilliseconds(ut : Int64) : Int64;
begin
   Result := GetSystemTimeMilliseconds - (UnixTime-ut)*1000;
end;

// SystemSleep
//
procedure SystemSleep(msec : Integer);
{$IFDEF MSWINDOWS}
begin
   if msec>=0 then
      Windows.Sleep(msec);
{$ENDIF}
{$IFDEF POSIX}
var
  tim: timespec;
begin
   if msec<0 then
      Exit;

   tim.tv_sec := Trunc(msec * 0.001);
   tim.tv_nsec := (msec - tim.tv_sec * 1000) * 1000000;
   nanosleep(tim, nil);
{$ENDIF}
end;

// FirstWideCharOfString
//
function FirstWideCharOfString(const s : String; const default : WideChar = #0) : WideChar;
begin
   {$ifdef FPC}
   if s <> '' then
      Result := PWideChar(String(s))^
   else Result := default;
   {$else}
   if s <> '' then
      Result := PWideChar(Pointer(s))^
   else Result := default;
   {$endif}
end;

// CodePointToUnicodeString
//
procedure CodePointToUnicodeString(c : Integer; var result : UnicodeString);
begin
   case c of
      0..$FFFF :
         Result := WideChar(c);
      $10000..$10FFFF : begin
         c := c-$10000;
         Result := WideChar($D800+(c shr 10))+WideChar($DC00+(c and $3FF));
      end;
   else
      raise EConvertError.CreateFmt('Invalid codepoint: %d', [c]);
   end;
end;

// CodePointToString
//
procedure CodePointToString(const c : Integer; var result : String); inline;
{$ifdef FPC}
var
   buf : UnicodeString;
begin
   CodePointToUnicodeString(c, buf);
   result := String(buf);
{$else}
begin
   CodePointToUnicodeString(c, result);
{$endif}
end;

// UnicodeCompareStr
//
{$ifndef FPC}
function UnicodeCompareStr(const S1, S2 : String) : Integer;
begin
   Result:=CompareStr(S1, S2);
end;
{$endif}

// UnicodeStringReplace
//
function UnicodeStringReplace(const s, oldPattern, newPattern: String; flags: TReplaceFlags) : String;
begin
   Result := SysUtils.StringReplace(s, oldPattern, newPattern, flags);
end;

function CompareStringEx(
   lpLocaleName: LPCWSTR; dwCmpFlags: DWORD;
   lpString1: LPCWSTR; cchCount1: Integer;
   lpString2: LPCWSTR; cchCount2: Integer;
   lpVersionInformation: Pointer; lpReserved: LPVOID;
   lParam: LPARAM): Integer; stdcall; external 'kernel32.dll';

// UnicodeCompareP
//
function UnicodeCompareP(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer;
{$IFDEF MACOS}
begin
   Result := StrToNSSTR(p1).localizedCaseInsensitiveCompare(StrToNSSTR(p2));
{$ELSE}
const
   CSTR_EQUAL = 2;
begin
{$IFDEF WINDOWS_XP}
   Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n1, p2, n2)-CSTR_EQUAL;
{$ELSE}
   Result := CompareStringEx(nil, NORM_IGNORECASE, p1, n1, p2, n2, nil, nil, 0)-CSTR_EQUAL;
{$ENDIF}
{$ENDIF}
end;

// UnicodeCompareP
//
function UnicodeCompareP(p1, p2 : PWideChar; n : Integer) : Integer; overload;
{$IFDEF MACOS}
begin
   Result := StrToNSSTR(p1).localizedCaseInsensitiveCompare(StrToNSSTR(p2));
{$ELSE}
const
   CSTR_EQUAL = 2;
begin
{$IFDEF WINDOWS_XP}
   Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n, p2, n)-CSTR_EQUAL;
{$ELSE}
   Result := CompareStringEx(nil, NORM_IGNORECASE, p1, n, p2, n, nil, nil, 0)-CSTR_EQUAL;
{$ENDIF}
{$ENDIF}
end;

{$ifndef WindowsCaseConvert}
const
   CCaseMapLower: array [0 .. 4012] of Word = (
      (* index *)
      $1bf, $2bf, $3bf, $44f, $54f, $64f, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $6af, $100, $100, $77d, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $87d, $97c, $100, $a79, $100, $100,
      $afd, $100, $100, $100, $100, $100, $100, $100, $bfd, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $cf0, $dce,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $ead,
      (* defaults *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $41 .. $ff *)
      32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
      32, 32, 32, 32, 32, 32, 32, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 32, 32, 32, 32, 32,
      32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 0,
      32, 32, 32, 32, 32, 32, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $100 .. $1ff *)
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      $ff39, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      0, $ff87, 1, 0, 1, 0, 1, 0, 0, 0, $d2, 1, 0, 1, 0, $ce, 1, 0, $cd, $cd,
      1, 0, 0, $4f, $ca, $cb, 1, 0, $cd, $cf, 0, $d3, $d1, 1, 0, 0, 0, $d3,
      $d5, 0, $d6, 1, 0, 1, 0, 1, 0, $da, 1, 0, $da, 0, 0, 1, 0, $da, 1, 0,
      $d9, $d9, 1, 0, 1, 0, $db, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0,
      2, 1, 0, 2, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, $2, 1, 0, 1, 0,
      $ff9f, $ffc8, 1, 0, 1, 0, 1, 0, 1, 0,
      (* $200 .. $2ff *)
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, $ff7e, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, $2a2b, 1, 0, $ff5d, $2a28, 0, 0, 1, 0,
      $ff3d, $45, $47, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $370 .. $3ff *)
      1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, $74, 0, 0, 0, 0, 0, 0, $26,
      0, $25, $25, $25, 0, $40, 0, $3f, $3f, 0, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, 0, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 0, 0, 0, 0, $ffc4, 0, 0, 1, 0, $fff9, 1, 0, 0, $ff7e,
      $ff7e, $ff7e,
      (* $400 .. $4ff *)
      80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 32, 32,
      32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
      32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, $f, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0,
      (* $500 .. $5ff *)
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      0, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
      48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
      48, 48, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      (* $10a0 .. $10ff *)
      $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60,
      $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60,
      $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60,
      $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, 0, $1c60, 0, 0,
      0, 0, 0, $1c60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      (* $1332 .. $13ff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $97d0, $97d0, $97d0, $97d0,
      $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0,
      $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0,
      $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0,
      $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0,
      $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0,
      $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0,
      $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, $97d0,
      $97d0, $97d0, $97d0, $97d0, $97d0, $97d0, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      (* $1e00 .. $1eff *)
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, $e241, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      (* $1f01 .. $1fff *)
      0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8,
      $fff8, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8,
      $fff8, $fff8, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8,
      $fff8, $fff8, $fff8, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8,
      $fff8, $fff8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, 0, $fff8, 0, $fff8,
      0, $fff8, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8,
      $fff8, $fff8, $fff8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8,
      $fff8, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8,
      $fff8, $fff8, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8,
      $fff8, $fff8, $fff8, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $ffb6, $ffb6,
      $fff7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffaa, $ffaa, $ffaa, $ffaa, $fff7,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $ff9c, $ff9c, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $ff90, $ff90, $fff9, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, $ff80, $ff80, $ff82, $ff82, $fff7, 0, 0, 0,
      (* $2103 .. $21ff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $e2a3, 0, 0, 0, $df41, $dfba, 0, 0, 0,
      0, 0, 0, $1c, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $247c .. $24ff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $2c00 .. $2cff *)
      48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
      48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
      48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, $d609, $f11a,
      $d619, 0, 0, 1, 0, 1, 0, 1, 0, $d5e4, $d603, $d5e1, $d5e2, 0, 1, 0, 0,
      1, 0, 0, 0, 0, 0, 0, 0, 0, $d5c1, $d5c1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
      0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $a60d .. $a6ff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0,
      (* $a722 .. $a7ff *)
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, $75fc, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, $5ad8, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, $5abc, $5ab1, $5ab5,
      $5abf, 0, 0, $5aee, $5ad6, $5aeb, $3a0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $ff21 .. $ffff *)
      32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
      32, 32, 32, 32, 32, 32, 32, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
 );


const
   CCaseMapUpper: array [0 .. 4569] of Word = (
      (* index *)
      $19f, $29f, $39f, $45a, $556, $656, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $6dd, $100, $100, $100, $100,
      $100, $100, $100, $100, $7db, $864, $963, $a63, $100, $b57, $100, $100,
      $bdc, $100, $100, $100, $100, $100, $100, $100, $cc6, $dc6, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $e85, $f62,
      $100, $100, $100, $101a, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100, $100,
      $100, $100, $100, $10da,
      (* defaults *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $61 .. $ff *)
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      $2e7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, 0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $79,
      (* $100 .. $1ff *)
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ff18, 0, $ffff, 0, $ffff, 0, $ffff,
      0, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, 0, $ffff, 0, $ffff, 0,
      $ffff, $fed4, $c3, 0, 0, $ffff, 0, $ffff, 0, 0, $ffff, 0, 0, 0, $ffff,
      0, 0, 0, 0, 0, $ffff, 0, 0, $61, 0, 0, 0, $ffff, $a3, 0, 0, 0, $82, 0,
      0, $ffff, 0, $ffff, 0, $ffff, 0, 0, $ffff, 0, 0, 0, 0, $ffff, 0, 0,
      $ffff, 0, 0, 0, $ffff, 0, $ffff, 0, 0, $ffff, 0, 0, 0, $ffff, 0, $38,
      0, 0, 0, 0, 0, $ffff, $fffe, 0, $ffff, $fffe, 0, $ffff, $fffe, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      $ffb1, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, 0, $ffff, $fffe, 0, $ffff, 0, 0, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff,
      (* $200 .. $2ff *)
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, 0, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, 0, 0, 0, 0, 0, 0, 0,
      $ffff, 0, 0, $2a3f, $2a3f, 0, $ffff, 0, 0, 0, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, $2a1f, $2a1c, $2a1e, $ff2e, $ff32, 0,
      $ff33, $ff33, 0, $ff36, 0, $ff35, $a54f, 0, 0, 0, $ff33, $a54b, 0,
      $ff31, 0, $a528, $a544, 0, $ff2f, $ff2d, $a544, $29f7, $a541, 0, 0,
      $ff2d, 0, $29fd, $ff2b, 0, 0, $ff2a, 0, 0, 0, 0, 0, 0, 0, $29e7, 0, 0,
      $ff26, 0, 0, $ff26, 0, 0, 0, $a52a, $ff26, $ffbb, $ff27, $ff27, $ffb9,
      0, 0, 0, 0, 0, $ff25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $a515, $a512, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $345 .. $3ff *)
      84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffff, 0,
      $ffff, 0, 0, 0, $ffff, 0, 0, 0, $82, $82, $82, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffda, $ffdb, $ffdb, $ffdb, 0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe1, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffc0, $ffc1, $ffc1,
      0, $ffc2, $ffc7, 0, 0, 0, $ffd1, $ffca, $fff8, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, $ffaa, $ffb0, $7, $ff8c, 0, $ffa0, 0, 0,
      $ffff, 0, 0, $ffff, 0, 0, 0, 0,
      (* $404 .. $4ff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0,
      $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, 0, 0, 0, 0, 0, 0, 0, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, $fff1, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      (* $500 .. $5ff *)
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffd0, $ffd0, $ffd0, $ffd0,
      $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
      $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
      $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
      $ffd0, $ffd0, $ffd0, $ffd0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $1379 .. $13ff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, 0, 0,
      (* $1c02 .. $1cff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, $e792, $e793, $e79c, $e79e, $e79e, $e79d, $e7a4, $e7db,
      $89c2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $1d77 .. $1dff *)
      0, 0, $8a04, 0, 0, 0, $ee6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $1e01 .. $1eff *)
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, 0, 0, 0, 0, $ffc5,
      0, 0, 0, 0, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      (* $1f00 .. $1fff *)
      8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0,
      8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 8, 0, 8, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0,
      8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 74, 74, 86, 86, 86, 86,
      $64, $64, $80, $80, $70, $70, $7e, $7e, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 0,
      0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8,
      8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 9, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, $e3db, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 0, 7, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $210c .. $21ff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffe4, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, $fff0, $fff0, $fff0, $fff0, $fff0, $fff0, $fff0, $fff0,
      $fff0, $fff0, $fff0, $fff0, $fff0, $fff0, $fff0, $fff0, 0, 0, 0, 0, $ffff,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0,
      (* $247b .. $24ff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6,
      $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6,
      $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6,
      $ffe6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $2c16 .. $2cff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
      $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
      $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
      $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
      $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, 0, 0, $ffff,
      0, 0, 0, $d5d5, $d5d8, 0, $ffff, 0, $ffff, 0, $ffff, 0, 0, 0, 0, 0,
      0, $ffff, 0, 0, $ffff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, 0, 0, 0,
      0, 0, 0, 0, $ffff, 0, $ffff, 0, 0, 0, 0, $ffff, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0,
      (* $2d00 .. $2dff *)
      $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0,
      $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0,
      $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0,
      $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, 0, $e3a0, 0, 0,
      0, 0, 0, $e3a0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $a641 .. $a6ff *)
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      (* $a723 .. $a7ff *)
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, 0,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0,
      $ffff, 0, $ffff, 0, $ffff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $ffff, 0, $ffff,
      0, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, 0, 0, 0, $ffff,
      0, 0, 0, 0, $ffff, 0, $ffff, 0, 0, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff,
      0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, $ffff, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, $ffff, 0, $ffff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $ab48 .. $abff *)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $fc60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $6830, $6830,
      $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830,
      $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830,
      $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830,
      $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830,
      $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830,
      $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830,
      $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830,
      $6830, $6830, $6830, $6830, $6830, $6830, $6830, $6830, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      (* $ff40 .. $ffff *)
      0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
      $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
   );

{$ifndef WIN32_ASM}
function ToLowerW(Ch: Word): Word; inline
begin
   Result := ch + CCaseMapLower[CCaseMapLower[ch shr 8] + (ch and $ff)];
end;

function ToUpperW(Ch: Word): Word; inline;
begin
   Result := ch + CCaseMapUpper[CCaseMapUpper[ch shr 8] + (ch and $ff)];
end;
{$endif}

procedure CharLowerBuffW(Data: PWord; Count: Cardinal);
{$ifndef WIN32_ASM}
var
   Index: Integer;
begin
   Index := Count;
   while Index > 0 do
   begin
      Data^ := ToLowerW(Data^);
      Inc(Data);
      Dec(Index);
   end;
{$else}
asm
        TEST    EDX,EDX
        JS      @2

        PUSH    EBX
        PUSH    EDI

        MOV     ECX,EDX             // ECX <- Count
        MOV     EDI,EAX             // EDI <- Data

@1:     MOV     AX,WORD PTR [EDI]
        MOV     DX,AX
        SHR     DX,8
        MOVZX   EDX, DX
        MOVZX   EDX, WORD PTR [CCaseMapLower + 2 * EDX]

        MOV     BX,AX
        AND     BX,$FF
        ADD     DX,BX
        MOVZX   EDX, DX
        MOVZX   EDX, WORD PTR [CCaseMapLower + 2 * EDX]
        ADD     AX, DX
        MOV     [EDI],AX

        ADD     EDI,2
        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     EBX

@2:
{$endif}
end;

procedure CharUpperBuffW(Data: PWord; Count: Cardinal);
{$ifndef WIN32_ASM}
var
   Index: Integer;
begin
   Index := Count;
   while Index > 0 do
   begin
      Data^ := ToUpperW(Data^);
      Inc(Data);
      Dec(Index);
   end;
{$else}
asm
        TEST    EDX,EDX
        JS      @2

        PUSH    EBX
        PUSH    EDI

        MOV     ECX,EDX
        MOV     EDI,EAX

@1:     MOV     AX,WORD PTR [EDI]
        MOV     DX,AX
        SHR     DX,8
        MOVZX   EDX, DX
        MOVZX   EDX, WORD PTR [CCaseMapUpper + 2 * EDX]

        MOV     BX,AX
        AND     BX,$FF
        ADD     DX,BX
        MOVZX   EDX, DX
        MOVZX   EDX, WORD PTR [CCaseMapUpper + 2 * EDX]
        ADD     AX, DX
        MOV     [EDI],AX

        ADD     EDI,2
        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     EBX

@2:
{$endif}
end;
{$endif}

// UnicodeLowerCase
//
function UnicodeLowerCase(const s : UnicodeString) : UnicodeString;
begin
   if s<>'' then begin
      Result:=s;
      UniqueString(Result);
      {$IFDEF WindowsCaseConvert}
      Windows.CharLowerBuffW(PWideChar(Pointer(Result)), Length(Result));
      {$ELSE}
      CharLowerBuffW(@Result[1], Length(Result));
      {$ENDIF}
   end else Result:=s;
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s : UnicodeString) : UnicodeString;
begin
   if s<>'' then begin
      Result:=s;
      UniqueString(Result);
      {$IFDEF WindowsCaseConvert}
      Windows.CharUpperBuffW(PWideChar(Pointer(Result)), Length(Result));
      {$ELSE}
      CharUpperBuffW(@Result[1], Length(Result));
      {$ENDIF}
   end else Result:=s;
end;

{$ifdef FPC}
// UnicodeLowerCase
//
function UnicodeLowerCase(const s : String) : String;
begin
   Result := String(UnicodeLowerCase(UnicodeString(s)));
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s : String) : String;
begin
   Result := String(UnicodeUpperCase(UnicodeString(s)));
end;
{$endif}

// ASCIICompareText
//
function ASCIICompareText(const s1, s2 : String) : Integer; inline;
begin
   {$ifdef FPC}
   Result:=CompareText(UTF8Encode(s1), UTF8Encode(s2));
   {$else}
   Result:=CompareText(s1, s2);
   {$endif}
end;

// ASCIISameText
//
function ASCIISameText(const s1, s2 : String) : Boolean; inline;
begin
   {$ifdef FPC}
   Result:=(ASCIICompareText(s1, s2)=0);
   {$else}
   Result:=SameText(s1, s2);
   {$endif}
end;

// NormalizeString
//
{$IFDEF MSWINDOWS}
function APINormalizeString(normForm : Integer; lpSrcString : LPCWSTR; cwSrcLength : Integer;
                            lpDstString : LPWSTR; cwDstLength : Integer) : Integer;
                            stdcall; external 'Normaliz.dll' name 'NormalizeString' {$ifndef FPC}delayed{$endif};
{$endif}

function NormalizeString(const s, form : String) : String;
{$ifdef MSWindows}
var
   nf, len, n : Integer;
begin
   if s = '' then Exit('');
   if (form = '') or (form = 'NFC') then
      nf := 1
   else if form = 'NFD' then
      nf := 2
   else if form = 'NFKC' then
      nf := 5
   else if form = 'NFKD' then
      nf := 6
   else raise Exception.CreateFmt('Unsupported normalization form "%s"', [form]);
   n := 10;
   len := APINormalizeString(nf, Pointer(s), Length(s), nil, 0);
   repeat
      SetLength(Result, len);
      len := APINormalizeString(nf, PWideChar(s), Length(s), Pointer(Result), len);
      if len <= 0 then begin
         if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
            RaiseLastOSError;
         Dec(n);
         if n <= 0 then
            RaiseLastOSError;
         len := -len;
         len := len + (len div 4); // extra margin since estimation failed
         continue;
      end;
   until True;
   SetLength(Result, len);
{$else}
var
   str : CFStringRef;
   mstr : CFMutableStringRef;
   nf : Integer;
begin
   str := CFStringCreateWithCharacters(nil, PWideChar(s), length(S));
   if (form = '') or (form = 'NFC') then
      nf := kCFStringNormalizationFormC
   else if form = 'NFD' then
      nf := kCFStringNormalizationFormD
   else if form = 'NFKC' then
      nf := kCFStringNormalizationFormKC
   else if form = 'NFKD' then
      nf := kCFStringNormalizationFormKD
   else raise Exception.CreateFmt('Unsupported normalization form "%s"', [form]);
   mstr := CFStringCreateMutableCopy(nil, 0, str);
   CFStringNormalize(mstr, nf);
   Result := CFStringRefToStr(CFStringRef(mstr));
{$endif}
end;

// StripAccents
//
function StripAccents(const s : String) : String;
var
   i : Integer;
   pSrc, pDest : PWideChar;
begin
   Result := NormalizeString(s, 'NFD');
   pSrc := Pointer(Result);
   pDest := pSrc;
   for i := 1 to Length(Result) do begin
      case Ord(pSrc^) of
         $300..$36F : ; // diacritic range
      else
         pDest^ := pSrc^;
         Inc(pDest);
      end;
      Inc(pSrc);
   end;
   SetLength(Result, (NativeUInt(pDest)-NativeUInt(Pointer(Result))) div 2);
end;

// InterlockedIncrement
//
function InterlockedIncrement(var val : Integer) : Integer;
{$ifdef MACOS}
begin
   Result:=System.AtomicIncrement(val);
{$else}
{$ifndef WIN32_ASM}
begin
   Result:=Windows.InterlockedIncrement(val);
{$else}
asm
   mov   ecx,  eax
   mov   eax,  1
   lock  xadd [ecx], eax
   inc   eax
{$endif}
{$endif}
end;

// InterlockedDecrement
//
function InterlockedDecrement(var val : Integer) : Integer;
{$ifdef MACOS}
begin
   Result:=System.AtomicDecrement(val);
{$else}
{$ifndef WIN32_ASM}
begin
   Result:=Windows.InterlockedDecrement(val);
{$else}
asm
   mov   ecx,  eax
   mov   eax,  -1
   lock  xadd [ecx], eax
   dec   eax
{$endif}
{$endif}
end;

// FastInterlockedIncrement
//
procedure FastInterlockedIncrement(var val : Integer);
{$ifndef WIN32_ASM}
begin
   InterlockedIncrement(val);
{$else}
asm
   lock  inc [eax]
{$endif}
end;

// FastInterlockedDecrement
//
procedure FastInterlockedDecrement(var val : Integer);
{$ifndef WIN32_ASM}
begin
   InterlockedDecrement(val);
{$else}
asm
   lock  dec [eax]
{$endif}
end;

// InterlockedExchangePointer
//
function InterlockedExchangePointer(var target : Pointer; val : Pointer) : Pointer;
{$ifdef MACOS}
begin
   Result:=System.AtomicExchange(target, val);
{$else}
{$ifndef WIN32_ASM}
begin
   {$ifdef FPC}
   Result:=System.InterLockedExchange(target, val);
   {$else}
   Result:=Windows.InterlockedExchangePointer(target, val);
   {$endif}
{$else}
asm
   lock  xchg dword ptr [eax], edx
   mov   eax, edx
{$endif}
{$endif}
end;

// InterlockedCompareExchangePointer
//
function InterlockedCompareExchangePointer(var destination : Pointer; exchange, comparand : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}
begin
   {$ifdef FPC}
      {$ifdef CPU64}
      Result := Pointer(System.InterlockedCompareExchange64(QWord(destination), QWord(exchange), QWord(comparand)));
      {$else}
      Result:=System.InterLockedCompareExchange(destination, exchange, comparand);
      {$endif}
   {$else}
   {$ifdef MSWINDOWS}
   Result:=Windows.InterlockedCompareExchangePointer(destination, exchange, comparand);
   {$else}
   Result:=System.AtomicCmpExchange(destination, exchange, comparand);
   {$endif}
   {$endif}
end;

// SetThreadName
//
{$IFDEF MSWINDOWS}
function IsDebuggerPresent : BOOL; stdcall; external kernel32 name 'IsDebuggerPresent';
{$ENDIF}
{$IFDEF MACOS}
// eventually add an implementation of IsDebuggerPresent here, see:
// https://developer.apple.com/library/content/qa/qa1361/_index.html
// or https://gist.github.com/rais38/4758465
// yet missing the definition of kinfo_proc though... (could be found in FPC code)
{$ENDIF}

procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));
// http://www.codeproject.com/Articles/8549/Name-your-threads-in-the-VC-debugger-thread-list
{$IFDEF MSWINDOWS}
type
   TThreadNameInfo = record
      dwType : Cardinal;      // must be 0x1000
      szName : PAnsiChar;     // pointer to name (in user addr space)
      dwThreadID : Cardinal;  // thread ID (-1=caller thread)
      dwFlags : Cardinal;     // reserved for future use, must be zero
   end;
var
   info : TThreadNameInfo;
begin
   if not IsDebuggerPresent then Exit;

   info.dwType:=$1000;
   info.szName:=threadName;
   info.dwThreadID:=threadID;
   info.dwFlags:=0;
   {$ifndef FPC}
   try
      RaiseException($406D1388, 0, SizeOf(info) div SizeOf(Cardinal), @info);
   except
   end;
   {$endif}
{$ELSE}
begin
  pthread_setname_np(threadName);
{$ENDIF}
end;

// OutputDebugString
//
procedure OutputDebugString(const msg : String);
begin
{$IFDEF MACOS}
   WriteLn(msg);
{$ELSE}
   Windows.OutputDebugStringW(PWideChar(msg));
{$ENDIF}
end;

// WriteToOSEventLog
//
procedure WriteToOSEventLog(const logName, logCaption, logDetails : String;
                            const logRawData : RawByteString = '');
{$IFDEF MACOS}
begin
   // yet todo, eventually append text to a file in /Library/Logs
   WriteLn(logName, logCaption, logDetails);
{$ELSE}
var
  eventSource : THandle;
  detailsPtr : array [0..1] of PWideChar;
begin
   if logName<>'' then
      eventSource:=RegisterEventSourceW(nil, PWideChar(logName))
   else eventSource:=RegisterEventSourceW(nil, PWideChar(ChangeFileExt(ExtractFileName(ParamStr(0)), '')));
   if eventSource>0 then begin
      try
         detailsPtr[0]:=PWideChar(logCaption);
         detailsPtr[1]:=PWideChar(logDetails);
         ReportEventW(eventSource, EVENTLOG_INFORMATION_TYPE, 0, 0, nil,
                      2, Length(logRawData),
                      @detailsPtr, Pointer(logRawData));
      finally
         DeregisterEventSource(eventSource);
      end;
   end;
{$ENDIF}
end;

// SetDecimalSeparator
//
procedure SetDecimalSeparator(c : Char);
begin
   {$IFDEF FPC}
      FormatSettings.DecimalSeparator:=c;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      FormatSettings.DecimalSeparator:=c;
      {$ELSE}
      DecimalSeparator:=c;
      {$IFEND}
   {$ENDIF}
end;

// GetDecimalSeparator
//
function GetDecimalSeparator : Char;
begin
   {$IFDEF FPC}
      Result:=FormatSettings.DecimalSeparator;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      Result:=FormatSettings.DecimalSeparator;
      {$ELSE}
      Result:=DecimalSeparator;
      {$IFEND}
   {$ENDIF}
end;

// CollectFiles
//
type
   TMasks = array of TMask;

{$IFDEF MSWINDOWS}
   TFindDataRec = record
      Handle : THandle;
      Data : TWin32FindDataW;
   end;
{$ENDIF}

// CollectFilesMasked
//
procedure CollectFilesMasked(const directory : TFileName;
                             const masks : TMasks; list : TStrings;
                             recurseSubdirectories: Boolean = False;
                             onProgress : TCollectFileProgressEvent = nil);
{$IFDEF MSWINDOWS}
const
   // contant defined in Windows.pas is incorrect
   FindExInfoBasic = 1;
var
   searchRec : TFindDataRec;
   infoLevel : TFindexInfoLevels;
   fileName : TFileName;
   skipScan, addToList : Boolean;
   i : Integer;
begin
   // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
   if ((Win32MajorVersion shl 8) or Win32MinorVersion)>=$601 then
      infoLevel:=TFindexInfoLevels(FindExInfoBasic)
   else infoLevel:=FindExInfoStandard;

   if Assigned(onProgress) then begin
      skipScan:=False;
      onProgress(directory, skipScan);
      if skipScan then exit;
   end;

   fileName:=directory+'*';
   searchRec.Handle:=FindFirstFileEx(PChar(fileName), infoLevel,
                                     @searchRec.Data, FINDEX_SEARCH_OPS.FindExSearchNameMatch,
                                     nil, 0);
   if searchRec.Handle<>INVALID_HANDLE_VALUE then begin
      repeat
         if (searchRec.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then begin
            // check file against mask
            fileName:=searchRec.Data.cFileName;
            addToList := True;
            for i := 0 to High(masks) do begin
               addToList := masks[i].Matches(fileName);
               if addToList then Break;
            end;
            if addToList then begin
               fileName:=directory+fileName;
               list.Add(fileName);
            end;
         end else if recurseSubdirectories then begin
            // dive in subdirectory
            if searchRec.Data.cFileName[0]='.' then begin
               if searchRec.Data.cFileName[1]='.' then begin
                  if searchRec.Data.cFileName[2]=#0 then continue;
               end else if searchRec.Data.cFileName[1]=#0 then continue;
            end;
            // decomposed cast and concatenation to avoid implicit string variable
            fileName:=searchRec.Data.cFileName;
            fileName:=directory+fileName+PathDelim;
            CollectFilesMasked(fileName, masks, list, True, onProgress);
         end;
      until not FindNextFileW(searchRec.Handle, searchRec.Data);
      Windows.FindClose(searchRec.Handle);
   end;
{$ELSE}
var
   searchRec : TSearchRec;
   fileName : TFileName;
   skipScan, addToList : Boolean;
   Attr, Done, i : Integer;
begin
   if Assigned(onProgress) then begin
      skipScan:=False;
      onProgress(directory, skipScan);
      if skipScan then exit;
   end;

   Attr := faAnyFile;
   Done := FindFirst(directory + '*.*', Attr, SearchRec);
   while Done = 0 do
   begin
      if SearchRec.Attr <> faDirectory then
      begin
         // check file against mask
         fileName:=searchRec.Name;
         addToList := True;
         for i := 0 to High(masks) do begin
            addToList := masks[i].Matches(fileName);
            if addToList then Break;
         end;
         if addToList then begin
            fileName:=directory+fileName;
            list.Add(fileName);
         end;
      end else if recurseSubdirectories then begin
          // dive in subdirectory
          if searchRec.Name[1]='.' then begin
             if searchRec.Name[2]='.' then begin
                if searchRec.Name[3]=#0 then continue;
             end else if searchRec.Name[2]=#0 then continue;
          end;

          // decomposed cast and concatenation to avoid implicit string variable
          fileName:=directory+searchRec.Name+PathDelim;
          CollectFilesMasked(fileName, masks, list, True, onProgress);
       end;

      Done := FindNext(SearchRec);
   end;
   FindClose(SearchRec);
{$ENDIF}
end;

// CollectFiles
//
procedure CollectFiles(const directory, fileMask : TFileName; list : TStrings;
                       recurseSubdirectories: Boolean = False;
                       onProgress : TCollectFileProgressEvent = nil);
var
   masks : TMasks;
   p, pNext : Integer;
begin
   if fileMask <> '' then begin
      p := 1;
      repeat
         pNext := PosEx(';', fileMask, p);
         if pNext < p then begin
            SetLength(masks, Length(masks)+1);
            masks[High(masks)] := TMask.Create(Copy(fileMask, p));
            break;
         end;
         if pNext > p then begin
            SetLength(masks, Length(masks)+1);
            masks[High(masks)] := TMask.Create(Copy(fileMask, p, pNext-p));
         end;
         p := pNext + 1;
      until p > Length(fileMask);
   end;
   // Windows can match 3 character filters with old DOS filenames
   // Mask confirmation is necessary
   try
      CollectFilesMasked(IncludeTrailingPathDelimiter(directory), masks,
                         list, recurseSubdirectories, onProgress);
   finally
      for p := 0 to High(masks) do
         masks[p].Free;
   end;
end;

// CollectSubDirs
//
procedure CollectSubDirs(const directory : TFileName; list : TStrings);
{$IFDEF MSWINDOWS}
const
   // contant defined in Windows.pas is incorrect
   FindExInfoBasic = 1;
var
   searchRec : TFindDataRec;
   infoLevel : TFindexInfoLevels;
   fileName : TFileName;
begin
   // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
   if ((Win32MajorVersion shl 8) or Win32MinorVersion)>=$601 then
      infoLevel:=TFindexInfoLevels(FindExInfoBasic)
   else infoLevel:=FindExInfoStandard;

   fileName := directory+'*';
   searchRec.Handle:=FindFirstFileEx(PChar(fileName), infoLevel,
                                     @searchRec.Data, FINDEX_SEARCH_OPS.FindExSearchLimitToDirectories,
                                     nil, 0);
   if searchRec.Handle<>INVALID_HANDLE_VALUE then begin
      repeat
         if (searchRec.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0 then begin
            if searchRec.Data.cFileName[0]='.' then begin
               if searchRec.Data.cFileName[1]='.' then begin
                  if searchRec.Data.cFileName[2]=#0 then continue;
               end else if searchRec.Data.cFileName[1]=#0 then continue;
            end;
            // decomposed cast and concatenation to avoid implicit string variable
            fileName := searchRec.Data.cFileName;
            list.Add(fileName);
         end;
      until not FindNextFileW(searchRec.Handle, searchRec.Data);
      Windows.FindClose(searchRec.Handle);
   end;
{$ELSE}
var
   searchRec : TSearchRec;
   Done : Integer;
begin
   Done := SysUtils.FindFirst(directory + '*', faAnyFile, SearchRec);
   while Done = 0 do
   begin
      if (faDirectory and searchRec.Attr) <> 0 then
         if searchRec.Name[1]<>'.' then
            list.Add(searchRec.Name);

      Done := SysUtils.FindNext(SearchRec);
   end;
   SysUtils.FindClose(SearchRec);
{$ENDIF}
end;

{$ifdef FPC}
// VarCopy
//
procedure VarCopy(out dest : Variant; const src : Variant);
begin
   dest:=src;
end;
{$else}
// VarToUnicodeStr
//
function VarToUnicodeStr(const v : Variant) : String; inline;
begin
   Result := VarToStr(v);
end;
{$endif FPC}

{$ifdef FPC}
// Utf8ToUnicodeString
//
function Utf8ToUnicodeString(const buf : RawByteString) : UnicodeString; inline;
begin
   Result := UTF8Decode(buf);
end;
{$endif}

// RawByteStringToBytes
//
function RawByteStringToBytes(const buf : RawByteString) : TBytes;
var
   n : Integer;
begin
   n:=Length(buf);
   SetLength(Result, n);
   if n>0 then
      System.Move(buf[1], Result[0], n);
end;

// BytesToRawByteString
//
function BytesToRawByteString(const buf : TBytes; startIndex : Integer = 0) : RawByteString;
var
   n : Integer;
begin
   n:=Length(buf)-startIndex;
   if n<=0 then
      Result:=''
   else begin
      SetLength(Result, n);
      System.Move(buf[startIndex], Pointer(Result)^, n);
   end;
end;

// BytesToRawByteString
//
function BytesToRawByteString(p : Pointer; size : Integer) : RawByteString;
begin
   SetLength(Result, size);
   System.Move(p^, Pointer(Result)^, size);
end;

// TryTextToFloat
//
function TryTextToFloat(const s : PChar; var value : Extended; const formatSettings : TFormatSettings) : Boolean;
{$ifdef FPC}
var
   cw : Word;
begin
   cw:=Get8087CW;
   Set8087CW($133F);
   if TryStrToFloat(s, value, formatSettings) then
      Result:=(value>-1.7e308) and (value<1.7e308);
   if not Result then
      value:=0;
   asm fclex end;
   Set8087CW(cw);
{$else}
begin
//   Result:=TextToFloat(s, value, fvExtended, formatSettings);
   Result := TryStrToFloat(s, value, formatSettings);
//   Result := StrToFloat(s, formatSettings);
{$endif}
end;

// TryTextToFloatW
//
function TryTextToFloatW(const s : PWideChar; var value : Extended;
                        const formatSettings : TFormatSettings) : Boolean;
{$ifdef FPC}
var
   bufU : UnicodeString;
   buf : String;
begin
   bufU := s;
   buf := String(bufU);
   Result := TryTextToFloat(PChar(buf), value, formatSettings);
{$else}
begin
   Result:=TextToFloat(s, value, fvExtended, formatSettings)
{$endif}
end;

{$IFDEF MSWINDOWS}

// LoadTextFromBuffer
//
function LoadTextFromBuffer(const buf : TBytes) : UnicodeString;
var
   n, sourceLen, len : Integer;
   encoding : TEncoding;
begin
   if buf=nil then
      Result:=''
   else begin
      encoding:=nil;
      n:=TEncoding.GetBufferEncoding(buf, encoding);
      if n=0 then
         encoding:=TEncoding.UTF8;
      if encoding=TEncoding.UTF8 then begin
         // handle UTF-8 directly, encoding.GetString returns an empty string
         // whenever a non-utf-8 character is detected, the implementation below
         // will return a '?' for non-utf8 characters instead
         sourceLen := Length(buf)-n;
         SetLength(Result, sourceLen);
         len := Utf8ToUnicode(Pointer(Result), sourceLen+1, PAnsiChar(buf)+n, sourceLen)-1;
         if len>0 then begin
            if len<>sourceLen then
               SetLength(Result, len);
         end else Result:=''
      end else begin
         Result:=encoding.GetString(buf, n, Length(buf)-n);
      end;
   end;
end;

// LoadTextFromRawBytes
//
function LoadTextFromRawBytes(const buf : RawByteString) : UnicodeString;
var
   b : TBytes;
begin
   if buf='' then Exit('');
   SetLength(b, Length(buf));
   System.Move(buf[1], b[0], Length(buf));
   Result:=LoadTextFromBuffer(b);
end;

// ReadFileChunked
//
function ReadFileChunked(hFile : THandle; const buffer; size : Integer) : Integer;
const
   CHUNK_SIZE = 16384;
var
   p : PByte;
   nRemaining : Integer;
   nRead : Cardinal;
begin
   p := @buffer;
   nRemaining := size;
   repeat
      if nRemaining > CHUNK_SIZE then
         nRead := CHUNK_SIZE
      else nRead := nRemaining;
      if not ReadFile(hFile, p^, nRead, nRead, nil) then
         RaiseLastOSError
      else if nRead = 0 then begin
         // file got trimmed while we were reading
         Exit(size-nRemaining);
      end;
      Dec(nRemaining, nRead);
      Inc(p, nRead);
   until nRemaining <= 0;
   Result := size;
end;

// LoadDataFromFile
//
function LoadDataFromFile(const fileName : TFileName) : TBytes;
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, nRead : Cardinal;
begin
   if fileName='' then Exit(nil);
   hFile:=OpenFileForSequentialReadOnly(fileName);
   if hFile=INVALID_HANDLE_VALUE then Exit(nil);
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         nRead := ReadFileChunked(hFile, Result[0], n);
         if nRead < n then
            SetLength(Result, nRead);
      end else Result:=nil;
   finally
      CloseHandle(hFile);
   end;
end;

// SaveDataToFile
//
procedure SaveDataToFile(const fileName : TFileName; const data : TBytes);
var
   hFile : THandle;
   n, nWrite : DWORD;
begin
   hFile:=OpenFileForSequentialWriteOnly(fileName);
   try
      n:=Length(data);
      if n>0 then
         if not WriteFile(hFile, data[0], n, nWrite, nil) then
            RaiseLastOSError;
   finally
      CloseHandle(hFile);
   end;
end;

// LoadRawBytesFromFile
//
function LoadRawBytesFromFile(const fileName : TFileName) : RawByteString;
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, nRead : Cardinal;
begin
   if fileName='' then Exit;
   hFile := OpenFileForSequentialReadOnly(fileName);
   if hFile = INVALID_HANDLE_VALUE then Exit;
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         nRead := ReadFileChunked(hFile, Pointer(Result)^, n);
         if nRead < n then
            SetLength(Result, nRead);
      end;
   finally
      CloseHandle(hFile);
   end;
end;

// SaveRawBytesToFile
//
function SaveRawBytesToFile(const fileName : TFileName; const data : RawByteString) : Integer;
var
   hFile : THandle;
   nWrite : DWORD;
begin
   Result:=0;
   hFile:=OpenFileForSequentialWriteOnly(fileName);
   try
      if data<>'' then begin
         Result:=Length(data);
         if not WriteFile(hFile, data[1], Result, nWrite, nil) then
            RaiseLastOSError;
      end;
   finally
      CloseHandle(hFile);
   end;
end;

// LoadRawBytesAsScriptStringFromFile
//
procedure LoadRawBytesAsScriptStringFromFile(const fileName : TFileName; var result : String);
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, i, nRead : Cardinal;
   pDest : PWord;
   buffer : array [0..16383] of Byte;
begin
   if fileName='' then Exit;
   hFile:=OpenFileForSequentialReadOnly(fileName);
   if hFile=INVALID_HANDLE_VALUE then Exit;
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         pDest := Pointer(Result);
         repeat
            if n > SizeOf(Buffer) then
               nRead := SizeOf(Buffer)
            else nRead := n;
            if not ReadFile(hFile, buffer, nRead, nRead, nil) then
               RaiseLastOSError
            else if nRead = 0 then begin
               // file got trimmed while we were reading
               SetLength(Result, Length(Result)-Integer(n));
               Break;
            end;
            for i := 1 to nRead do begin
               pDest^ := buffer[i-1];
               Inc(pDest);
            end;
            Dec(n, nRead);
         until n <= 0;
      end;
   finally
      CloseHandle(hFile);
   end;
end;
{$endif}

// LoadTextFromStream
//
function LoadTextFromStream(aStream : TStream) : UnicodeString;
{$ifdef MSWINDOWS}
var
   n : Integer;
   buf : TBytes;
begin
   n := aStream.Size-aStream.Position;
   SetLength(buf, n);
   aStream.Read(buf[0], n);
   Result:=LoadTextFromBuffer(buf);
{$else}
var
   StringList: TStringList;
begin
   StringList := TStringList.Create;
   try
     StringList.LoadFromStream(aStream);
     Result := StringList.Text;
   finally
     StringList.Free;
   end;
{$endif}
end;

// LoadTextFromFile
//
function LoadTextFromFile(const fileName : TFileName) : UnicodeString;
{$ifdef MSWINDOWS}
var
   buf : TBytes;
begin
   buf:=LoadDataFromFile(fileName);
   Result:=LoadTextFromBuffer(buf);
{$else}
var
   StringList: TStringList;
begin
   StringList := TStringList.Create;
   try
     StringList.LoadFromFile(fileName);
     Result := StringList.Text;
   finally
     StringList.Free;
   end;
{$endif}
end;

// SaveTextToUTF8File
//
procedure SaveTextToUTF8File(const fileName : TFileName; const text : UTF8String);
begin
{$IFDEF MACOS}
   StrToNSStr(string(text)).writeToFile(StrToNSStr(fileName), False);
{$ENDIF}
{$IFDEF MSWINDOWS}
   SaveRawBytesToFile(fileName, UTF8Encode(text));
{$ENDIF}
end;

// AppendTextToUTF8File
//
procedure AppendTextToUTF8File(const fileName : TFileName; const text : UTF8String);
var
   fs : TFileStream;
begin
   if text='' then Exit;
   if FileExists(fileName) then
      fs:=TFileStream.Create(fileName, fmOpenWrite or fmShareDenyNone)
   else fs:=TFileStream.Create(fileName, fmCreate);
   try
      fs.Seek(0, soFromEnd);
      fs.Write(text[1], Length(text));
   finally
      fs.Free;
   end;
end;

// OpenFileForSequentialReadOnly
//
function OpenFileForSequentialReadOnly(const fileName : TFileName) : THandle;
begin
{$IFDEF POSIX}
   Result := open(PAnsiChar(AnsiString(fileName)), 0, O_RDONLY);
{$ENDIF}
{$IFDEF MSWINDOWS}
   Result:=CreateFile(PChar(fileName), GENERIC_READ, FILE_SHARE_READ+FILE_SHARE_WRITE,
                      nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if Result=INVALID_HANDLE_VALUE then begin
      if GetLastError<>ERROR_FILE_NOT_FOUND then
         RaiseLastOSError;
   end;
{$ENDIF}
end;

// OpenFileForSequentialWriteOnly
//
function OpenFileForSequentialWriteOnly(const fileName : TFileName) : THandle;
begin
{$IFDEF POSIX}
   Result := open(PAnsiChar(AnsiString(fileName)), 0, O_WRONLY);
{$ENDIF}
{$IFDEF MSWINDOWS}
   Result:=CreateFile(PChar(fileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                      FILE_ATTRIBUTE_NORMAL+FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if Result=INVALID_HANDLE_VALUE then
      RaiseLastOSError;
{$ENDIF}
end;

// CloseFileHandle
//
procedure CloseFileHandle(hFile : THandle);
begin
{$IFDEF POSIX}
  fcntl(hFile, FD_CLOEXEC)
{$ENDIF}
{$IFDEF MSWINDOWS}
   CloseHandle(hFile);
{$ENDIF}
end;

{$ifdef MSWINDOWS}
// FileWrite
//
function FileWrite(hFile : THandle; buffer : Pointer; byteCount : Integer) : Cardinal;
begin
   if not WriteFile(hFile, buffer^, byteCount, Result, nil) then
      RaiseLastOSError;
end;

// FileFlushBuffers
//
function FlushFileBuffers(hFile : THandle) : BOOL; stdcall; external 'kernel32.dll';
function FileFlushBuffers(hFile : THandle) : Boolean;
begin
   Result := FlushFileBuffers(hFile);
end;

// FileCopy
//
function FileCopy(const existing, new : TFileName; failIfExists : Boolean) : Boolean;
begin
   Result:=Windows.CopyFileW(PWideChar(existing), PWideChar(new), failIfExists);
end;

// FileMove
//
function FileMove(const existing, new : TFileName) : Boolean;
begin
   Result:=Windows.MoveFileW(PWideChar(existing), PWideChar(new));
end;

// FileDelete
//
function FileDelete(const fileName : TFileName) : Boolean;
begin
   Result:=SysUtils.DeleteFile(fileName);
end;

// FileRename
//
function FileRename(const oldName, newName : TFileName) : Boolean;
begin
   Result:=RenameFile(oldName, newName);
end;

// FileSize
//
function FileSize(const name : TFileName) : Int64;
var
   info : TWin32FileAttributeData;
begin
   if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info) then
      Result:=info.nFileSizeLow or (Int64(info.nFileSizeHigh) shl 32)
   else Result:=-1;
end;

// FileDateTime
//
function FileDateTime(const name : TFileName) : TDateTime;
var
   info : TWin32FileAttributeData;
   localTime : TFileTime;
   systemTime : TSystemTime;
begin
   if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info) then begin
      FileTimeToLocalFileTime(info.ftLastWriteTime, localTime);
      FileTimeToSystemTime(localTime, systemTime);
      Result:=SystemTimeToDateTime(systemTime);
   end else Result:=0;
end;

// FileSetDateTime
//
procedure FileSetDateTime(hFile : THandle; aDateTime : TDateTime);
begin
   FileSetDate(hFile, DateTimeToFileDate(aDateTime));
end;
{$ENDIF}

// DeleteDirectory
//
function DeleteDirectory(const path : String) : Boolean;
begin
   {$ifdef FPC}
   Result := RemoveDir(path);
   {$else}
   try
      TDirectory.Delete(path, True);
   except
      Exit(False);
   end;
   Result := not TDirectory.Exists(path);
   {$endif}
end;

// DirectSet8087CW
//
function DirectSet8087CW(newValue: Word): Word; register;
{$IFNDEF WIN32_ASM}
begin
   Result:=newValue;
{$else}
asm
   push    eax
   push    eax
   fnstcw  [esp]
   fnclex
   pop     eax
   fldcw   [esp]
   pop     edx
{$endif}
end;

// DirectSetMXCSR
//
function DirectSetMXCSR(newValue : Word) : Word; register;
{$ifdef WIN32_ASM}
asm
   and      eax, $FFC0
   push     eax
   push     eax
   stmxcsr  [esp+4]
   ldmxcsr  [esp]
   pop eax
   pop eax
{$else}
begin
   Result:=newValue;
{$endif}
end;

// SwapBytes
//
function SwapBytes(v : Cardinal) : Cardinal;
{$ifdef WIN32_ASM}
asm
   bswap eax
{$else}
type
   TCardinalBytes = array [0..3] of Byte;
begin
   TCardinalBytes(Result)[0] := TCardinalBytes(v)[3];
   TCardinalBytes(Result)[1] := TCardinalBytes(v)[2];
   TCardinalBytes(Result)[2] := TCardinalBytes(v)[1];
   TCardinalBytes(Result)[3] := TCardinalBytes(v)[0];
{$endif}
end;

// SwapInt64
//
procedure SwapInt64(src, dest : PInt64);
{$ifdef WIN32_ASM}
asm
   mov   ecx, [eax]
   mov   eax, [eax+4]
   bswap ecx
   bswap eax
   mov   [edx+4], ecx
   mov   [edx], eax
{$else}
begin
   PByteArray(dest)[0] := PByteArray(src)[7];
   PByteArray(dest)[1] := PByteArray(src)[6];
   PByteArray(dest)[2] := PByteArray(src)[5];
   PByteArray(dest)[3] := PByteArray(src)[4];
   PByteArray(dest)[4] := PByteArray(src)[3];
   PByteArray(dest)[5] := PByteArray(src)[2];
   PByteArray(dest)[6] := PByteArray(src)[1];
   PByteArray(dest)[7] := PByteArray(src)[0];
{$endif}
end;

// RDTSC
//
function RDTSC : UInt64;
asm
   RDTSC
end;

// GetCurrentUserName
//
function GetCurrentUserName : String;
{$IFDEF MSWINDOWS}
var
   len : Cardinal;
begin
   len:=255;
   SetLength(Result, len);
   Windows.GetUserName(PChar(Result), len);
   SetLength(Result, len-1);
{$ENDIF}
{$IFDEF MACOS}
begin
   Result := UnicodeString(TNSString.Wrap(NSUserName).UTF8String);
{$ENDIF}
end;

{$ifndef FPC}
// Delphi 2009 is not able to cast a generic T instance to TObject or Pointer
function TtoObject(const T): TObject;
begin
// Manually inlining the code would require the IF-defs
//{$IF Compilerversion >= 21}
   Result := TObject(T);
//{$ELSE}
//   Result := PObject(@T)^;
//{$IFEND}
end;

function TtoPointer(const T): Pointer;
begin
// Manually inlining the code would require the IF-defs
//{$IF Compilerversion >= 21}
   Result := Pointer(T);
//{$ELSE}
//   Result := PPointer(@T)^;
//{$IFEND}
end;

procedure GetMemForT(var T; Size: integer); inline;
begin
  GetMem(Pointer(T), Size);
end;
{$endif}

// InitializeWithDefaultFormatSettings
//
procedure InitializeWithDefaultFormatSettings(var fmt : TFormatSettings);
begin
   {$ifdef DELPHI_XE_PLUS}
   fmt:=SysUtils.FormatSettings;
   {$else}
   fmt:=SysUtils.TFormatSettings((@CurrencyString{%H-})^);
   {$endif}
end;

// AsString
//
function TModuleVersion.AsString : String;
begin
   Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

{$IFDEF MSWindows}
// Adapted from Ian Boyd code published in
// http://stackoverflow.com/questions/10854958/how-to-get-version-of-running-executable
function GetModuleVersion(instance : THandle; var version : TModuleVersion) : Boolean;
var
   fileInformation : PVSFIXEDFILEINFO;
   verlen : Cardinal;
   rs : TResourceStream;
   m : TMemoryStream;
   resource : HRSRC;
begin
   Result:=False;

   // Workaround bug in Delphi if resource doesn't exist
   resource:=FindResource(instance, PChar(1), RT_VERSION);
   if resource=0 then Exit;

   m:=TMemoryStream.Create;
   try
      rs:=TResourceStream.CreateFromID(instance, 1, RT_VERSION);
      try
         m.CopyFrom(rs, rs.Size);
      finally
         rs.Free;
      end;

      m.Position:=0;
      if VerQueryValue(m.Memory, '\', Pointer(fileInformation), verlen) then begin
         version.Major := fileInformation.dwFileVersionMS shr 16;
         version.Minor := fileInformation.dwFileVersionMS and $FFFF;
         version.Release := fileInformation.dwFileVersionLS shr 16;
         version.Build := fileInformation.dwFileVersionLS and $FFFF;
         Result := True;
      end;
   finally
      m.Free;
   end;
end;

// GetApplicationVersion
//
var
   vApplicationVersion : TModuleVersion;
   vApplicationVersionRetrieved : Integer;
function GetApplicationVersion(var version : TModuleVersion) : Boolean;
begin
   if vApplicationVersionRetrieved = 0 then begin
      if GetModuleVersion(HInstance, vApplicationVersion) then
         vApplicationVersionRetrieved := 1
      else vApplicationVersionRetrieved := -1;
   end;
   Result := (vApplicationVersionRetrieved = 1);
   if Result then
      version := vApplicationVersion;
end;

// ApplicationVersion
//
function ApplicationVersion : String;
var
   version : TModuleVersion;
begin
   if GetApplicationVersion(version) then
      Result := version.AsString
   else Result := '?.?.?.?';
end;
{$ENDIF}

{$IFDEF MACOS}
function ApplicationVersion : String;
var
   CFStr: CFStringRef;
   Range: CFRange;
begin
   CFStr := CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle,
      kCFBundleVersionKey);

   if CFStr = nil then
     Exit('unknown');

   Range.location := 0;
   Range.length := CFStringGetLength(CFStr);
   SetLength(Result, Range.length);
   CFStringGetCharacters(CFStr, Range, PChar(Result));
end;
{$ENDIF}

{$ifndef POSIX}
// ------------------
// ------------------ TdwsCriticalSection ------------------
// ------------------

// Create
//
constructor TdwsCriticalSection.Create;
begin
   InitializeCriticalSection(FCS);
end;

// Destroy
//
destructor TdwsCriticalSection.Destroy;
begin
   DeleteCriticalSection(FCS);
end;

// Enter
//
procedure TdwsCriticalSection.Enter;
begin
   EnterCriticalSection(FCS);
end;

// Leave
//
procedure TdwsCriticalSection.Leave;
begin
   LeaveCriticalSection(FCS);
end;

// TryEnter
//
function TdwsCriticalSection.TryEnter : Boolean;
begin
   Result:=TryEnterCriticalSection(FCS);
end;
{$endif}

// ------------------
// ------------------ TPath ------------------
// ------------------

// GetTempPath
//
class function TPath.GetTempPath : String;
{$IFDEF WINDOWS}
var
   tempPath : array [0..MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
   if Windows.GetTempPath(MAX_PATH, @tempPath[0])=0 then begin
      tempPath[1]:='.'; // Current directory
      tempPath[2]:=#0;
   end;
   Result:=tempPath;
{$ELSE}
begin
   Result:=IOUTils.TPath.GetTempPath;
{$ENDIF}
end;

// GetTempFileName
//
class function TPath.GetTempFileName : String;
{$IFDEF WINDOWS}
var
   tempPath, tempFileName : array [0..MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
   if Windows.GetTempPath(MAX_PATH, @tempPath[0])=0 then begin
      tempPath[1]:='.'; // Current directory
      tempPath[2]:=#0;
   end;
   if Windows.GetTempFileNameW(@tempPath[0], 'DWS', 0, tempFileName)=0 then
      RaiseLastOSError; // should never happen
   Result:=tempFileName;
{$ELSE}
begin
   Result:=IOUTils.TPath.GetTempFileName;
{$ENDIF}
end;

// ------------------
// ------------------ TFile ------------------
// ------------------

// ReadAllBytes
//
class function TFile.ReadAllBytes(const filename : String) : TBytes;
{$IFDEF VER200} // Delphi 2009
var
   fileStream : TFileStream;
   n : Integer;
begin
   fileStream:=TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
   try
      n:=fileStream.Size;
      SetLength(Result, n);
      if n>0 then
         fileStream.ReadBuffer(Result[0], n);
   finally
      fileStream.Free;
   end;
{$ELSE}
begin
   Result:=IOUTils.TFile.ReadAllBytes(filename);
{$ENDIF}
end;

// ------------------
// ------------------ TdwsThread ------------------
// ------------------

{$IFNDEF FPC}
{$IFDEF VER200}

// Start
//
procedure TdwsThread.Start;
begin
   Resume;
end;

{$ENDIF}
{$ENDIF}

// ------------------
// ------------------ TMultiReadSingleWrite ------------------
// ------------------

{$ifndef SRW_FALLBACK}
procedure TMultiReadSingleWrite.BeginRead;
begin
   AcquireSRWLockShared(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginRead : Boolean;
begin
   Result:=TryAcquireSRWLockShared(FSRWLock);
end;

procedure TMultiReadSingleWrite.EndRead;
begin
   ReleaseSRWLockShared(FSRWLock)
end;

procedure TMultiReadSingleWrite.BeginWrite;
begin
   AcquireSRWLockExclusive(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginWrite : Boolean;
begin
   Result:=TryAcquireSRWLockExclusive(FSRWLock);
end;

procedure TMultiReadSingleWrite.EndWrite;
begin
   ReleaseSRWLockExclusive(FSRWLock)
end;

function TMultiReadSingleWrite.State : TMultiReadSingleWriteState;
begin
   // Attempt to guess the state of the lock without making assumptions
   // about implementation details
   // This is only for diagnosing locking issues
   if TryBeginWrite then begin
      EndWrite;
      Result:=mrswUnlocked;
   end else if TryBeginRead then begin
      EndRead;
      Result:=mrswReadLock;
   end else begin
      Result:=mrswWriteLock;
   end;
end;
{$else} // SRW_FALLBACK
constructor TMultiReadSingleWrite.Create;
begin
   FLock := TdwsCriticalSection.Create;
end;

destructor TMultiReadSingleWrite.Destroy;
begin
   FLock.Free;
end;

procedure TMultiReadSingleWrite.BeginRead;
begin
   FLock.Enter;
end;

function TMultiReadSingleWrite.TryBeginRead : Boolean;
begin
   Result:=FLock.TryEnter;
end;

procedure TMultiReadSingleWrite.EndRead;
begin
   FLock.Leave;
end;

procedure TMultiReadSingleWrite.BeginWrite;
begin
   FLock.Enter;
end;

function TMultiReadSingleWrite.TryBeginWrite : Boolean;
begin
   Result:=FLock.TryEnter;
end;

procedure TMultiReadSingleWrite.EndWrite;
begin
   FLock.Leave;
end;

function TMultiReadSingleWrite.State : TMultiReadSingleWriteState;
begin
   if FLock.TryEnter then begin
      FLock.Leave;
      Result := mrswUnlocked;
   end else Result := mrswWriteLock;
end;

{$endif}

// ------------------
// ------------------ TTimerTimeout ------------------
// ------------------

{$ifdef FPC}
type TWaitOrTimerCallback = procedure (Context: Pointer; Success: Boolean); stdcall;
function CreateTimerQueueTimer(out phNewTimer: THandle;
   TimerQueue: THandle; CallBack: TWaitOrTimerCallback;
   Parameter: Pointer; DueTime: DWORD; Period: DWORD; Flags: ULONG): BOOL; stdcall; external 'kernel32.dll';
function DeleteTimerQueueTimer(TimerQueue: THandle;
   Timer: THandle; CompletionEvent: THandle): BOOL; stdcall; external 'kernel32.dll';
const
   WT_EXECUTEDEFAULT       = ULONG($00000000);
   WT_EXECUTEONLYONCE      = ULONG($00000008);
   WT_EXECUTELONGFUNCTION  = ULONG($00000010);
{$endif}

{$ifdef MSWINDOWS}
procedure TTimerTimeoutCallBack(Context: Pointer; {%H-}Success: Boolean); stdcall;
var
   tt : TTimerTimeout;
   event : TTimerEvent;
begin
   tt := TTimerTimeout(Context);
   tt._AddRef;
   try
      event := tt.FOnTimer;
      if Assigned(event) then
         event();
      DeleteTimerQueueTimer(0, tt.FTimer, 0);
      tt.FTimer := 0;
   finally
      tt._Release;
   end;
end;

// Create
//
class function TTimerTimeout.Create(delayMSec : Cardinal; onTimer : TTimerEvent) : ITimer;
var
   obj : TTimerTimeout;
begin
   obj := TTimerTimeout(inherited Create);
   Result := obj;
   obj.FOnTimer := onTimer;
   CreateTimerQueueTimer(obj.FTimer, 0, TTimerTimeoutCallBack, obj,
                         delayMSec, 0,
                         WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION or WT_EXECUTEONLYONCE);
end;

// Destroy
//
destructor TTimerTimeout.Destroy;
begin
   Cancel;
   inherited;
end;

// Cancel
//
procedure TTimerTimeout.Cancel;
begin
   FOnTimer := nil;
   if FTimer = 0 then Exit;
   DeleteTimerQueueTimer(0, FTimer, INVALID_HANDLE_VALUE);
   FTimer:=0;
end;
{$endif}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeGetSystemMilliseconds;

end.
