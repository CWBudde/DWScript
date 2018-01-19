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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit UdwsXPlatformTests;

interface

uses Classes, SysUtils, Math, Variants, Types, SynCommons,
   dwsXPlatformTests, dwsUtils,
   dwsXPlatform, dwsWebUtils, dwsTokenStore, dwsCryptoXPlatform,
   dwsEncodingLibModule, dwsGlobalVars, dwsEncoding, dwsDataContext,
   dwsXXHash, dwsURLRewriter, dwsJSON;

type

   TdwsXPlatformTests = class (TTestCase)
      private
      protected
         procedure SetUp; override;
         procedure TearDown; override;
      published
         procedure DateTimeConversionTest;
         procedure MillisecondsConversionTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsXPlatformTests ------------------
// ------------------


// SetUp
//
procedure TdwsXPlatformTests.SetUp;
begin

end;

// TearDown
//
procedure TdwsXPlatformTests.TearDown;
begin

end;

// DateTimeConversionTest
//
procedure TdwsXPlatformTests.DateTimeConversionTest;
var
   CurrentDateTime: TDateTime;
begin
   CurrentDateTime := UTCDateTime;
   CheckEquals(CurrentDateTime, UTCDateTimeToLocalDateTime(LocalDateTimeToUTCDateTime(Now)));
end;


// MillisecondsConversionTest
//
procedure TdwsXPlatformTests.MillisecondsConversionTest;
var
   CurrentMilliseconds: Int64;
begin
   CurrentMilliseconds := UnixTime;
   CheckEquals(CurrentMilliseconds, UnixTimeToSystemMilliseconds(SystemMillisecondsToUnixTime(CurrentMilliseconds)));
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('XPlatformTests', TdwsXPlatformTests);

end.
