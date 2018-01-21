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
      protected
         procedure SetUp; override;
         procedure TearDown; override;
      published
         procedure DecimalPointTest;
         procedure CollectFilesTest;
         procedure DateTimeConversionTest;
         procedure MillisecondsConversionTest;
         procedure UnicodeLowerAndUpperCaseTest;
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
procedure TdwsXPlatformTests.CollectFilesTest;
var
   Files : TStringList;
begin
   Files:=TStringList.Create;
   try
      CollectFiles(ExtractFilePath(ParamStr(0))+'Data'+PathDelim, '*.txt', Files);
      CheckEquals(3, Files.Count);
   finally
      Files.Free;
   end;
end;

procedure TdwsXPlatformTests.DateTimeConversionTest;
var
   CurrentDateTime : TDateTime;
begin
   CurrentDateTime := UTCDateTime;
   CheckEquals(CurrentDateTime, UTCDateTimeToLocalDateTime(LocalDateTimeToUTCDateTime(Now)));
end;


// MillisecondsConversionTest
//
procedure TdwsXPlatformTests.DecimalPointTest;
var
  OldDecimalSeparator : Char;
begin
  OldDecimalSeparator := GetDecimalSeparator;
  SetDecimalSeparator(',');
  CheckEquals(',', GetDecimalSeparator);
  SetDecimalSeparator('.');
  CheckEquals('.', GetDecimalSeparator);
  SetDecimalSeparator(OldDecimalSeparator);
end;

procedure TdwsXPlatformTests.MillisecondsConversionTest;
var
   CurrentMilliseconds : Int64;
begin
   CurrentMilliseconds := UnixTime;
   CheckEquals(CurrentMilliseconds, UnixTimeToSystemMilliseconds(SystemMillisecondsToUnixTime(CurrentMilliseconds)));
end;


// UnicodeLowerAndUpperCaseTest
//
procedure TdwsXPlatformTests.UnicodeLowerAndUpperCaseTest;
const
  TestStringUpperCaseBasic = '0123456789<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  TestStringUpperCaseSupplement = 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ';
  TestStringUpperCaseExtendedA =
    'ĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŹŻŽ';
  TestStringUpperCaseExtendedB = 'ƇƉƑƓƔƘǄǇǊǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮ';
  TestStringUpperCaseGreek = 'ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫϢ';
  TestStringUpperCaseCyrillic = 'ЁЂЃЄЅІЇЈЉЊЋЌЎЏАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';
  TestStringUpperCaseArmenian = 'ԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿՀՁՂՃՄՅՆՇՈՉՊՋՌՍՎՏՐՑՒՓՔՕ';
  TestStringUpperCaseExtendedAdditional =
    'ḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮḰḲḴḶḸḺḼḾṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾ';
  TestStringUpperCaseGreekExtended =
    'ἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹἺἻἼἽἾἿὈὉὊὋὌὍὙὛὝὟὨὩὪὫὬὭὮὯᾈᾉᾊᾋᾌᾍ';
  TestStringUpperCaseFullWidth =
    'ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ';
  TestStringLowerCaseBasic = '0123456789<=>abcdefghijklmnopqrstuvwxyz';
  TestStringLowerCaseSupplement = 'àáâãäåæçèéêëìíîïðñòóôõö';
  TestStringLowerCaseExtendedA =
    'āăąćĉċčďđēĕėęěĝğġģĥħĩīĭįĳĵķĺļľŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷźżž';
  TestStringLowerCaseExtendedB = 'ƈɖƒɠɣƙǆǉǌǎǐǒǔǖǘǚǜǟǡǣǥǧǩǫǭǯ';
  TestStringLowerCaseGreek = 'αβγδεζηθικλμνξοπρστυφχψωϊϋϣ';
  TestStringLowerCaseCyrillic = 'ёђѓєѕіїјљњћќўџабвгдежзийклмнопрстуфхцчшщъыьэюя';
  TestStringLowerCaseArmenian = 'աբգդեզէըթժիլխծկհձղճմյնշոչպջռսվտրցւփքօ';
  TestStringLowerCaseExtendedAdditional =
    'ḁḃḅḇḉḋḍḏḑḓḕḗḙḛḝḟḡḣḥḧḩḫḭḯḱḳḵḷḹḻḽḿṁṃṅṇṉṋṍṏṑṓṕṗṙṛṝṟṡṣṥṧṩṫṭṯṱṳṵṷṹṻṽṿ';
  TestStringLowerCaseGreekExtended =
    'ἀἁἂἃἄἅἆἇἐἑἒἓἔἕἠἡἢἣἤἥἦἧἰἱἲἳἴἵἶἷὀὁὂὃὄὅὑὓὕὗὠὡὢὣὤὥὦὧᾀᾁᾂᾃᾄᾅ';
  TestStringLowerCaseFullWidth =
    'ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ';
var
   Test: String;
begin
   CheckEquals(TestStringLowerCaseBasic, UnicodeLowerCase(TestStringUpperCaseBasic));
   CheckEquals(TestStringLowerCaseSupplement, UnicodeLowerCase(TestStringUpperCaseSupplement));
   CheckEquals(TestStringLowerCaseExtendedA, UnicodeLowerCase(TestStringUpperCaseExtendedA));
   CheckEquals(TestStringLowerCaseExtendedB, UnicodeLowerCase(TestStringUpperCaseExtendedB));
   CheckEquals(TestStringLowerCaseGreek, UnicodeLowerCase(TestStringUpperCaseGreek));
   CheckEquals(TestStringLowerCaseCyrillic, UnicodeLowerCase(TestStringUpperCaseCyrillic));
   CheckEquals(TestStringLowerCaseArmenian, UnicodeLowerCase(TestStringUpperCaseArmenian));
   CheckEquals(TestStringLowerCaseExtendedAdditional, UnicodeLowerCase(TestStringUpperCaseExtendedAdditional));
   CheckEquals(TestStringLowerCaseGreekExtended, UnicodeLowerCase(TestStringUpperCaseGreekExtended));
   CheckEquals(TestStringLowerCaseFullWidth, UnicodeLowerCase(TestStringUpperCaseFullWidth));
   CheckEquals(TestStringUpperCaseBasic, UnicodeUpperCase(TestStringLowerCaseBasic));
   CheckEquals(TestStringUpperCaseSupplement, UnicodeUpperCase(TestStringLowerCaseSupplement));
   CheckEquals(TestStringUpperCaseExtendedA, UnicodeUpperCase(TestStringLowerCaseExtendedA));
   CheckEquals(TestStringUpperCaseExtendedB, UnicodeUpperCase(TestStringLowerCaseExtendedB));
   CheckEquals(TestStringUpperCaseGreek, UnicodeUpperCase(TestStringLowerCaseGreek));
   CheckEquals(TestStringUpperCaseCyrillic, UnicodeUpperCase(TestStringLowerCaseCyrillic));
   CheckEquals(TestStringUpperCaseArmenian, UnicodeUpperCase(TestStringLowerCaseArmenian));
   CheckEquals(TestStringUpperCaseExtendedAdditional, UnicodeUpperCase(TestStringLowerCaseExtendedAdditional));
   CheckEquals(TestStringUpperCaseGreekExtended, UnicodeUpperCase(TestStringLowerCaseGreekExtended));
   CheckEquals(TestStringUpperCaseFullWidth, UnicodeUpperCase(TestStringLowerCaseFullWidth));
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
