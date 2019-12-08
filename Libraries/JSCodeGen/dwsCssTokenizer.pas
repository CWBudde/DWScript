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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsCssTokenizer;

{$I dws.inc}

interface

uses
  dwsTokenizer, dwsStrings;

type
   TCssTokenizerStateRules = class(TTokenizerRules)
      private
         sStart, sSlashComment, sSlashComment0 : TState;
         sNameF, sUriF: TState;
         sHexF, sIntS, sIntF: TState;
         sStringSingle, sStringSingleF : TState;
         sStringDouble, sStringDoubleF : TState;
         sBlockCommentSlash, sBlockCommentSlash1 : TState;
         sIntPoint, sIntPointF : TState;
         sImportant, sPercentage: TState;
         sIntUnit, sMinus: TState;
      protected
         function StartState : TState; override;

      public
         constructor Create; override;

   end;

const
   cCssSymbolTokens : TTokenTypes = [
      ttStrVal, ttIntVal, ttFloatVal, ttCARET, ttAT, ttDOLLAR, ttPIPE, ttSEMI,
      ttCOMMA, ttCOLON, ttPERCENT, ttTIMES, ttPLUS, ttGTR, ttEXCLAMATION,
      ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttCRIGHT, ttCLEFT];

   cCssReservedNames : TTokenTypes = [
      ttCONST, ttFALSE, ttTRUE
   ];

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cOPS = ['='];
   cSPACE = [' ', #9, #10, #12, #13, #0];
   cSPEC = ['(', ')', ',', '.', ':', ';', '[', ']', '>', '|', '~', '#', '{', '}'];
   cSTOP = cSPEC + cOPS + cSPACE;
   cANYCHAR = [#0..#255];
   cNAM = ['A'..'Z', 'a'..'z', '_', #127, #240..#255];
   cINT = ['0'..'9'];
   cALPHA = cNAM + cINT;
   cHEX = cINT + ['A'..'F', 'a'..'f', '_'];

// ------------------
// ------------------ TCssTokenizerStateRules ------------------
// ------------------

// Create
//
constructor TCssTokenizerStateRules.Create;
begin
   inherited;

   CaseSensitive:=tcsHintCaseMismatch;

   SymbolTokens:=cCssSymbolTokens;
   ReservedNames:=cCssReservedNames;

   sStart:=CreateState;
   sSlashComment0:=CreateState;
   sSlashComment:=CreateState;
   sBlockCommentSlash:=CreateState;
   sBlockCommentSlash1:=CreateState;
   sHexF:=CreateState;
   sImportant:=CreateState;
   sIntS:=CreateState;
   sIntF:=CreateState;
   sIntPoint:=CreateState;
   sIntPointF:=CreateState;
   sNameF:=CreateState;
   sUriF:=CreateState;
   sStringSingle:=CreateState;
   sStringSingleF:=CreateState;
   sStringDouble:=CreateState;
   sStringDoubleF:=CreateState;
   sIntUnit:=CreateState;
   sPercentage:=CreateState;
   sMinus:=CreateState;

   sStart.AddTransition(cSPACE, TSeekTransition.Create(sStart, [], caNone));
   sStart.AddTransition(cNAM, TConsumeTransition.Create(sNameF, [toStart], caNone));
   sStart.AddTransition(['#'], TConsumeTransition.Create(sHexF, [toStart], caNone));
   sStart.AddTransition(cInt, TConsumeTransition.Create(sIntS, [toStart], caNone));
   sStart.AddTransition(['-'], TConsumeTransition.Create(sMinus, [toStart], caNone));
   sStart.AddTransition([''''], TSeekTransition.Create(sStringSingle, [toStart], caNone));
   sStart.AddTransition(['"'], TSeekTransition.Create(sStringDouble, [toStart], caNone));
   sStart.AddTransition(['/'], TConsumeTransition.Create(sSlashComment0, [toStart], caNone));
   sStart.AddTransition(cSPEC + cOPS, TConsumeTransition.Create(sStart, [toStart, toFinal], caName));
   sStart.AddTransition(['+', '*', '%', '@', '|'], TConsumeTransition.Create(sStart, [toStart, toFinal], caName));
   sStart.AddTransition(['!'], TConsumeTransition.Create(sImportant, [toStart], caNone));

   sStart.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sSlashComment0.AddTransition(['/'], TSeekTransition.Create(sSlashComment, [], caNone));
   sSlashComment0.AddTransition(['*'], TConsumeTransition.Create(sBlockCommentSlash, [], caNone));
   sSlashComment0.SetElse(TCheckTransition.Create(sStart, [toFinal], caName));

   sBlockCommentSlash.AddTransition(['*'], TSeekTransition.Create(sBlockCommentSlash1, [], caNone));
   sBlockCommentSlash.SetElse(TSeekTransition.Create(sBlockCommentSlash, [], caNone));

   sBlockCommentSlash1.AddTransition(['/'], TSeekTransition.Create(sStart, [], caClear));
   sBlockCommentSlash1.AddTransition(['*'], TSeekTransition.Create(sBlockCommentSlash1, [], caNone));
   sBlockCommentSlash1.SetElse(TSeekTransition.Create(sBlockCommentSlash, [], caNone));

   sSlashComment.AddTransition([#10], TSeekTransition.Create(sStart, [], caClear));
   sSlashComment.SetElse(TSeekTransition.Create(sSlashComment, [], caNone));

   sMinus.AddTransition(cNAM, TConsumeTransition.Create(sNameF, [], caNone));
   sMinus.AddTransition(cINT, TConsumeTransition.Create(sIntS, [], caNone));
   sMinus.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caNone));

   sNameF.AddTransition(cALPHA + ['-'], TConsumeTransition.Create(sNameF, [], caNone));
   sNameF.AddTransition(['/', '+'], TConsumeTransition.Create(sUriF, [], caNone));
   sNameF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sNameF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sUriF.AddTransition(cALPHA + ['-'], TConsumeTransition.Create(sUriF, [], caNone));
   sUriF.AddTransition(['/', '+'], TConsumeTransition.Create(sUriF, [], caNone));
   sUriF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sUriF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sHexF.AddTransition(cHEX, TConsumeTransition.Create(sHexF, [], caNone));
   sHexF.AddTransition(cNAM, TCheckTransition.Create(sHexF, [toFinal], caName));
   sHexF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caHex));
   sHexF.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sImportant.AddTransition(cNAM, TConsumeTransition.Create(sImportant, [], caNone));
   sImportant.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));

   sIntS.AddTransition(cINT, TConsumeTransition.Create(sIntF, [], caNone));
   sIntS.AddTransition(['.'], TConsumeTransition.Create(sIntPoint, [], caNone));
   sIntS.AddTransition(['%'], TConsumeTransition.Create(sPercentage, [], caNone));
   sIntS.AddTransition(cNAM + ['-'], TConsumeTransition.Create(sIntUnit, [], caNone));
   sIntS.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caInteger));
   sIntS.SetElse(TErrorTransition.Create(TOK_NumberPointExponentExpected));

   sIntF.AddTransition(cINT, TConsumeTransition.Create(sIntF, [], caNone));
   sIntF.AddTransition(['.'], TConsumeTransition.Create(sIntPoint, [], caNone));
   sIntF.AddTransition(['%'], TConsumeTransition.Create(sPercentage, [], caNone));
   sIntF.AddTransition(cNAM + ['-'], TConsumeTransition.Create(sIntUnit, [], caNone));
   sIntF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caInteger));
   sIntF.SetElse(TErrorTransition.Create(TOK_NumberPointExponentExpected));

   sIntPoint.AddTransition(cINT, TConsumeTransition.Create(sIntPointF, [], caNone));
   sIntPoint.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sIntPointF.AddTransition(cINT, TConsumeTransition.Create(sIntPointF, [], caNone));
   sIntPointF.AddTransition(['%'], TConsumeTransition.Create(sPercentage, [], caNone));
   sIntPointF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caFloat));
   sIntPointF.SetElse(TErrorTransition.Create(TOK_NumberExponentExpected));

   sIntUnit.AddTransition(cINT, TConsumeTransition.Create(sIntUnit, [], caNone));
   sIntUnit.AddTransition(cNAM + ['-'], TConsumeTransition.Create(sIntUnit, [], caNone));
   sIntUnit.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));

   sPercentage.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));

   sStringDouble.AddTransition(cANYCHAR - ['"', #0], TConsumeTransition.Create(sStringDouble, [], caNone));
   sStringDouble.AddTransition(['"'], TSeekTransition.Create(sStringDoubleF, [], caNone));
   sStringDouble.AddTransition([#0], TErrorTransition.Create(TOK_HereDocTerminationError));

   sStringDoubleF.AddTransition(['"'], TConsumeTransition.Create(sStringDouble, [], caNone));
   sStringDoubleF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caString));
   sStringDoubleF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caString));
   sStringDoubleF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sStringSingle.AddTransition(cANYCHAR - ['''', #0], TConsumeTransition.Create(sStringSingle, [], caNone));
   sStringSingle.AddTransition([''''], TSeekTransition.Create(sStringSingleF, [], caNone));
   sStringSingle.AddTransition([#0], TErrorTransition.Create(TOK_HereDocTerminationError));

   sStringSingleF.AddTransition([''''], TConsumeTransition.Create(sStringSingle, [], caNone));
   sStringSingleF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caString));
   sStringSingleF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caString));
   sStringSingleF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   PrepareStates;
end;

// StartState
//
function TCssTokenizerStateRules.StartState : TState;
begin
   Result := sStart;
end;

end.
