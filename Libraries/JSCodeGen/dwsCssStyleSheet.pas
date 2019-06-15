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
unit dwsCssStyleSheet;

interface

uses
   System.SysUtils, System.Generics.Collections, dwsErrors, dwsTokenizer,
   dwsUtils, dwsCssTokenizer, dwsScriptSource;

type
   TCssSimpleSelector = class;
   TCssExpression = class;

   TCssElement = class(TRefCountedObject)
   protected
      FStartPos: TScriptPos;
      constructor Create(StartPos: TScriptPos);
   public
      property StartPos: TScriptPos read FStartPos;
   end;

   TCssSelector = class(TCssElement)
   type
      TSelectorCombinator = (scDescendent, scChild, scAdjacentSibling,
         scGeneralSibling);
   private
      FCombinator: TSelectorCombinator;
      FSimpleSelector: TCssSimpleSelector;
      FSelector: TCssSelector;
      function GetName: string;
   public
      constructor Create(StartPos: TScriptPos; SimpleSelector: TCssSimpleSelector);
      destructor Destroy; override;

      property Combinator: TSelectorCombinator read FCombinator write FCombinator;
      property Selector: TCssSelector read FSelector write FSelector;
      property SimpleSelector: TCssSimpleSelector read FSimpleSelector;
      property Name: string read GetName;
   end;

   TCssSelectorList = TObjectList<TCssSelector>;

   TCssSimpleSelectorSupplement = class(TCssElement)
   private
      FName: string;
   protected
      function GetFullName: string; virtual; abstract;
   public
      constructor Create(StartPos: TScriptPos; const Name: string);

      property FullName: string read GetFullName;
      property Name: string read FName;
   end;

   TCssSimpleSelectorSupplementHash = class(TCssSimpleSelectorSupplement)
   protected
      function GetFullName: string; override;
   end;

   TCssSimpleSelectorSupplementClass = class(TCssSimpleSelectorSupplement)
   protected
      function GetFullName: string; override;
   end;

   TCssSimpleSelectorSupplementAttribute = class(TCssSimpleSelectorSupplement)
   protected
      function GetFullName: string; override;
   end;

   TCssSimpleSelectorSupplementAttributeFilter = class(TCssSimpleSelectorSupplementAttribute)
   type
      TCssFilterOperator = (foEquals, foContainsWord, foBase, foBegins, foEnds,
         foContains);
   private
      FFilter: string;
      FOperator: TCssFilterOperator;
   protected
      function GetFullName: string; override;
   public
      constructor Create(StartPos: TScriptPos; const Name: string;
        FilterOperator: TCssFilterOperator; const Filter: string); reintroduce;
   end;

   TCssSimpleSelectorSupplementPseudo = class(TCssSimpleSelectorSupplement)
   protected
      function GetFullName: string; override;
   end;

   TCssSimpleSelectorSupplementList = TObjectList<TCssSimpleSelectorSupplement>;

   TCssSimpleSelector = class(TCssElement)
   private
      FElementName: string;
      FSupplements: TCssSimpleSelectorSupplementList;
      function GetName: string;
   public
      constructor Create(StartPos: TScriptPos; ElementName: string);
      destructor Destroy; override;

      property ElementName: string read FElementName;
      property Supplements: TCssSimpleSelectorSupplementList read FSupplements;
      property Name: string read GetName;
   end;

   TCssSimpleSelectorList = TObjectList<TCssSimpleSelector>;

   TCssTerm = class(TCssElement)
   protected
      function GetAsString: string; virtual; abstract;
   public
      property AsString: string read GetAsString;
   end;

   TCssTextTerm = class(TCssTerm)
   private
      FText: string;
   protected
      function GetAsString: string; override;
   public
      constructor Create(StartPos: TScriptPos; Text: string);
   end;

   TCssStringTerm = class(TCssTextTerm)
   protected
      function GetAsString: string; override;
   end;

   TCssNumberTerm = class(TCssTerm)
   private
      FNumber: Double;
      FUnit: string;
   protected
      function GetAsString: string; override;
   public
      constructor Create(StartPos: TScriptPos; ANumber: Double; AUnit: string = '');
      property Number: Double read FNumber;
      property &Unit: string read FUnit;
   end;

   TCssFunctionTerm = class(TCssTerm)
   private
      FName: string;
      FExpression: TCssExpression;
   protected
      function GetAsString: string; override;
   public
      constructor Create(StartPos: TScriptPos; Name: string; Expression: TCssExpression);
      destructor Destroy; override;

      property Name: string read FName;
      property Expression: TCssExpression read FExpression;
   end;

   TCssTermList = TObjectList<TCssTerm>;

   TCssExpression = class(TCssElement)
   private
      FTermList: TCssTermList;
      function GetAsString: string;
   public
      constructor Create(StartPos: TScriptPos);
      destructor Destroy; override;

      property TermList: TCssTermList read FTermList;
      property AsString: string read GetAsString;
   end;

   TCssDeclaration = class(TCssElement)
   private
      FPropertyName: string;
      FExpression: TCssExpression;
      FImportant: Boolean;
   public
      constructor Create(StartPos: TScriptPos; PropertyName: string;
         Expression: TCssExpression; Important: Boolean = False);
      destructor Destroy; override;

      property PropertyName: string read FPropertyName;
      property Expression: TCssExpression read FExpression;
      property Important: Boolean read FImportant;
   end;

   TCssDeclarations = TObjectList<TCssDeclaration>;

   TCssRule = class(TCssElement)
   private
      FSelectors: TCssSelectorList;
      FDeclarations: TCssDeclarations;
      FEndPos: TScriptPos;
   public
      constructor Create(StartPos, EndPos: TScriptPos;
         Selectors: TCssSelectorList; Declarations: TCssDeclarations);
      destructor Destroy; override;

      property Selectors: TCssSelectorList read FSelectors;
      property Declarations: TCssDeclarations read FDeclarations;
      property EndPos: TScriptPos read FEndPos;
   end;

   TCssRuleList = TObjectList<TCssRule>;

   TCssStyleSheet = class(TCssElement)
   private
      FMsgs: TdwsCompileMessageList;
      FTokenizerStateRules: TCssTokenizerStateRules;
      FTokenizer : TTokenizer;
      FRuleList: TCssRuleList;
      function ReadClass: TCssSimpleSelectorSupplementClass;
      function ReadHash: TCssSimpleSelectorSupplementHash;
      function ReadPseudo: TCssSimpleSelectorSupplementPseudo;
      function ReadPropertyName: string;
      function ReadExpression: TCssExpression;
      function ReadPriority: Boolean;
      function ReadTerm: TCssTerm;
      function ReadIdentifier: string;
      function ReadString: string;
      function ReadFloat: Double;
      function ReadInteger: Double;
      function ReadNameTerm: TCssTerm;
      procedure ReadAtRule;
   protected
      function ReadRule: TCssRule;
      procedure ReadRules(const RuleList: TCssRuleList);
      function ReadSelector: TCssSelector;
      function ReadSelectors: TCssSelectorList;
      function ReadElementName: string;
      function ReadSimpleSelector: TCssSimpleSelector;
      function ReadAttribute: TCssSimpleSelectorSupplementAttribute;
      function ReadDeclaration: TCssDeclaration;
      function ReadDeclarations: TCssDeclarations;
   public
      constructor Create;
      destructor Destroy; override;

      procedure Parse(const Text: string; const Name: string = '');

      property RuleList: TCssRuleList read FRuleList;
      property Messages: TdwsCompileMessageList read FMsgs;
   end;

implementation

resourcestring
   RStrTokenExpected = '''%s'' expected';
   RStrIdentifierExpected = 'Identifier expected';

{ TCssElement }

constructor TCssElement.Create(StartPos: TScriptPos);
begin
   FStartPos := StartPos;
end;


{ TCssSelector }

constructor TCssSelector.Create(StartPos: TScriptPos; SimpleSelector: TCssSimpleSelector);
begin
   inherited Create(StartPos);
   FCombinator := scDescendent;
   FSimpleSelector := SimpleSelector;
   FSelector := nil;
end;

destructor TCssSelector.Destroy;
begin
  FSelector.Free;
  FSimpleSelector.Free;

  inherited;
end;

function TCssSelector.GetName: string;
begin
   Result := FSimpleSelector.Name;

   if Assigned(FSelector) then
   begin
      case FCombinator of
         scDescendent:
            Result := Result + ' ';
         scChild:
            Result := Result + '>';
         scAdjacentSibling:
            Result := Result + '+';
         scGeneralSibling:
            Result := Result + '~';
      end;
      Result := Result + FSelector.Name;
   end;
end;


{ TCssRule.TCssSimpleSelector }

constructor TCssSimpleSelector.Create(StartPos: TScriptPos; ElementName: string);
begin
   inherited Create(StartPos);
   FElementName := ElementName;
   FSupplements := TCssSimpleSelectorSupplementList.Create;
end;

destructor TCssSimpleSelector.Destroy;
begin
   FSupplements.Free;
   inherited;
end;

function TCssSimpleSelector.GetName: string;
var
   Index: Integer;
begin
   Result := FElementName;
   for Index := 0 to FSupplements.Count - 1 do
      Result := Result + FSupplements[Index].FullName;
end;


{ TCssSimpleSelectorSupplement }

constructor TCssSimpleSelectorSupplement.Create(StartPos: TScriptPos;
   const Name: string);
begin
   inherited Create(StartPos);
   FName := Name;
end;


{ TCssSimpleSelectorSupplementHash }

function TCssSimpleSelectorSupplementHash.GetFullName: string;
begin
   Result := '#' + FName;
end;


{ TCssSimpleSelectorSupplementClass }

function TCssSimpleSelectorSupplementClass.GetFullName: string;
begin
   Result := '.' + FName;
end;


{ TCssSimpleSelectorSupplementAttribute }

function TCssSimpleSelectorSupplementAttribute.GetFullName: string;
begin
   Result := '[' + FName + ']';
end;


{ TCssSimpleSelectorSupplementAttributeFilter }

constructor TCssSimpleSelectorSupplementAttributeFilter.Create(
  StartPos: TScriptPos; const Name: string; FilterOperator: TCssFilterOperator;
  const Filter: string);
begin
  inherited Create(StartPos, Name);

  FFilter := Filter;
  FOperator := FilterOperator;
end;

function TCssSimpleSelectorSupplementAttributeFilter.GetFullName: string;
begin
   Result := '[' + FName;

   case FOperator of
      foEquals:
         Result := Result + '=';
      foContainsWord:
         Result := Result + '~=';
      foBase:
         Result := Result + '|=';
      foBegins:
         Result := Result + '^=';
      foEnds:
         Result := Result + '$=';
      foContains:
         Result := Result + '*=';
   end;

   Result := Result + FFilter + ']';
end;


{ TCssSimpleSelectorSupplementPseudo }

function TCssSimpleSelectorSupplementPseudo.GetFullName: string;
begin
   Result := ':' + FName;
end;


{ TCssTextTerm }

constructor TCssTextTerm.Create(StartPos: TScriptPos; Text: string);
begin
   inherited Create(StartPos);
   FText := Text;
end;

function TCssTextTerm.GetAsString: string;
begin
   Result := FText;
end;


{ TCssStringTerm }

function TCssStringTerm.GetAsString: string;
begin
  Result := FText;
end;


{ TCssNumberTerm }

constructor TCssNumberTerm.Create(StartPos: TScriptPos; ANumber: Double;
   AUnit: string = '');
begin
   inherited Create(StartPos);
   FNumber := ANumber;
   FUnit := AUnit;
end;

function TCssNumberTerm.GetAsString: string;
begin
   Result := FloatToStr(FNumber) + FUnit;
end;


{ TCssFunctionTerm }

constructor TCssFunctionTerm.Create(StartPos: TScriptPos; Name: string;
   Expression: TCssExpression);
begin
   inherited Create(StartPos);
   FName := Name;
   FExpression := Expression;
end;

destructor TCssFunctionTerm.Destroy;
begin
   FExpression.Free;
   inherited;
end;

function TCssFunctionTerm.GetAsString: string;
begin
   Result := FName + '(' + FExpression.AsString + ')';
end;


{ TCssExpression }

constructor TCssExpression.Create;
begin
   FTermList := TCssTermList.Create;
end;

destructor TCssExpression.Destroy;
begin
   FTermList.Free;
   inherited;
end;

function TCssExpression.GetAsString: string;
var
   Index: Integer;
begin
   Result := FTermList[0].AsString;
   for Index := 1 to FTermList.Count - 1 do
      Result := Result + ' ' + FTermList[Index].AsString;
end;


{ TCssDeclaration }

constructor TCssDeclaration.Create(StartPos: TScriptPos; PropertyName: string;
   Expression: TCssExpression; Important: Boolean = False);
begin
   inherited Create(StartPos);
   FPropertyName := PropertyName;
   FExpression := Expression;
   FImportant := Important;
end;

destructor TCssDeclaration.Destroy;
begin
  FExpression.Free;
  inherited;
end;


{ TCssRule }

constructor TCssRule.Create(StartPos, EndPos: TScriptPos;
  Selectors: TCssSelectorList; Declarations: TCssDeclarations);
begin
   inherited Create(StartPos);
   FSelectors := Selectors;
   FDeclarations := Declarations;
   FEndPos := EndPos;
end;

destructor TCssRule.Destroy;
begin
   FSelectors.Free;
   FDeclarations.Free;
   inherited;
end;


{ TCssStyleSheet }

constructor TCssStyleSheet.Create;
begin
   FTokenizerStateRules := TCssTokenizerStateRules.Create;
   FMsgs := TdwsCompileMessageList.Create;
   {$IFDEF RELEASE}
   FMsgs.HintsLevel := hlDisabled;
   {$ENDIF}
   FTokenizer := TTokenizer.Create(FTokenizerStateRules, FMsgs);

   FRuleList := TCssRuleList.Create;
end;

destructor TCssStyleSheet.Destroy;
begin
   FRuleList.Free;

   FTokenizer.Free;
   FMsgs.Free;
   FTokenizerStateRules.Free;

   inherited;
end;

procedure TCssStyleSheet.Parse(const Text: string; const Name: string);
var
   SourceFile: TSourceFile;
begin
   FMsgs.Clear;
   FRuleList.Clear;

   SourceFile := TSourceFile.Create;
   try
      SourceFile.Code := Text;
      SourceFile.Name := Name;
      FTokenizer.BeginSourceFile(SourceFile);
      try
         try
            ReadRules(FRuleList);
         except
            Exit;
         end;
      finally
         FTokenizer.EndSourceFile;
      end;
   finally
      SourceFile.Free;
   end;
end;

function TCssStyleSheet.ReadHash: TCssSimpleSelectorSupplementHash;
var
   LocalStartPos: TScriptPos;
begin
   // get current position
   LocalStartPos := FTokenizer.HotPos;

   // ensure the next token is a name
   if not FTokenizer.TestAnyName then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, RStrIdentifierExpected);

   // create supplement class
   Result := TCssSimpleSelectorSupplementHash.Create(LocalStartPos,
      FTokenizer.GetToken.AsString);
end;

function TCssStyleSheet.ReadClass: TCssSimpleSelectorSupplementClass;
var
   LocalStartPos: TScriptPos;
begin
   // get current position
   LocalStartPos := FTokenizer.HotPos;

   // ensure the next token is a name
   if not FTokenizer.TestAnyName then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, RStrIdentifierExpected);

   // create supplement class
   Result := TCssSimpleSelectorSupplementClass.Create(LocalStartPos,
      FTokenizer.GetToken.AsString);
end;

function TCssStyleSheet.ReadAttribute: TCssSimpleSelectorSupplementAttribute;
var
   LocalStartPos: TScriptPos;
   Name, Filter: string;
   FilterOperator: TCssSimpleSelectorSupplementAttributeFilter.TCssFilterOperator;
begin
   // get current position
   LocalStartPos := FTokenizer.HotPos;

   // ensure the next token is a name
   if not FTokenizer.TestAnyName then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, RStrIdentifierExpected);

   Name := FTokenizer.GetToken.AsString;

   FTokenizer.KillToken;
   if not FTokenizer.HasTokens then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');

   if not FTokenizer.TestDelete(ttARIGHT) then
   begin
      if FTokenizer.TestDelete(ttEQ) then
         FilterOperator := foEquals
      else
      if FTokenizer.TestDelete(ttCARET) then
         FilterOperator := foBegins
      else
      if FTokenizer.TestDelete(ttPIPE) then
         FilterOperator := foBase
      else
      if FTokenizer.TestDelete(ttDOLLAR) then
         FilterOperator := foEnds
      else
      if FTokenizer.TestDelete(ttTIMES) then
         FilterOperator := foContains
      else
      begin
         // check for '~=' operator
         if FTokenizer.TestName then
            if FTokenizer.GetToken.AsString = '~' then
            begin
               FTokenizer.KillToken;
               if not FTokenizer.HasTokens then
                  FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');

            end
            else
               FMsgs.AddCompilerStop(FTokenizer.HotPos, 'Operator expected');

         FilterOperator := foBegins;
      end;

      if not FTokenizer.TestDelete(ttEQ) then
         FMsgs.AddCompilerStopFmt(FTokenizer.HotPos, RStrTokenExpected, ['=']);

      if not (FTokenizer.TestName or FTokenizer.Test(ttStrVal)) then
         FMsgs.AddCompilerStop(FTokenizer.HotPos, RStrIdentifierExpected);

      Filter := FTokenizer.GetToken.AsString;
      FTokenizer.KillToken;
      if not FTokenizer.HasTokens then
         FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');

      if not FTokenizer.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStopFmt(FTokenizer.HotPos, RStrTokenExpected, [']']);

      Result := TCssSimpleSelectorSupplementAttributeFilter.Create(
         LocalStartPos, Name, FilterOperator, Filter);
   end
   else
      // create supplement class
      Result := TCssSimpleSelectorSupplementAttribute.Create(LocalStartPos,
         Name);
end;

function TCssStyleSheet.ReadPseudo: TCssSimpleSelectorSupplementPseudo;
var
   LocalStartPos: TScriptPos;
begin
   // get current position
   LocalStartPos := FTokenizer.HotPos;

   // ensure the next token is a name
   if not FTokenizer.TestAnyName then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, RStrIdentifierExpected);

   // create supplement class
   Result := TCssSimpleSelectorSupplementPseudo.Create(LocalStartPos,
      FTokenizer.GetToken.AsString);
end;

function TCssStyleSheet.ReadElementName: string;
begin
   // check for element name
   if not (FTokenizer.TestAnyName or FTokenizer.Test(ttTimes)) then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, RStrIdentifierExpected);

   if not FTokenizer.Test(ttDOT) then
   begin
      Result := FTokenizer.GetToken.AsString;
      FTokenizer.KillToken;
      if not FTokenizer.HasTokens then
         FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');
   end
   else
      Result := '';
end;

function TCssStyleSheet.ReadSimpleSelector: TCssSimpleSelector;
var
   LocalStartPos: TScriptPos;
   ElementName: string;
   Supplement: TCssSimpleSelectorSupplement;
begin
   // get current position
   LocalStartPos := FTokenizer.HotPos;

   ElementName := ReadElementName;
   Result := TCssSimpleSelector.Create(LocalStartPos, ElementName);

   while FTokenizer.TestAny([{ADD TEST FOR # HERE!} ttDOT, ttALEFT, ttCOLON]) <> ttNone do
   begin
      Supplement := nil;
(*
      if FTokenizer.TestDelete(#) then
         Supplement := ReadHash
*)
      if FTokenizer.TestDelete(ttDOT) then
         Supplement := ReadClass
      else if FTokenizer.TestDelete(ttALEFT) then
         Supplement := ReadAttribute
      else if FTokenizer.TestDelete(ttCOLON) then
         Supplement := ReadPseudo;

      if not Assigned(Supplement) then
         FMsgs.AddCompilerStop(FTokenizer.HotPos, 'Supplement is nil!');

      // add string as supplement
      Result.Supplements.Add(Supplement);

      // kill token and test for end of file
      FTokenizer.KillToken;
      if not FTokenizer.HasTokens then
         FMsgs.AddCompilerStop(FTokenizer.HotPos, 'No more tokens are available!');
   end;
end;

function TCssStyleSheet.ReadSelector: TCssSelector;
var
   LocalStartPos: TScriptPos;
   SimpleSelector: TCssSimpleSelector;
   SelectorRequired: Boolean;
begin
   // get current position
   LocalStartPos := FTokenizer.HotPos;

   // read simple selector
   SimpleSelector := ReadSimpleSelector;
   Result := TCssSelector.Create(LocalStartPos, SimpleSelector);

   SelectorRequired := False;
   if FTokenizer.TestDelete(ttGTR) then
   begin
      Result.Combinator := scChild;
      SelectorRequired := True;
   end
   else if FTokenizer.TestDelete(ttPLUS) then
   begin
      Result.Combinator := scAdjacentSibling;
      SelectorRequired := True;
(*   else if FTokenizer.TestDelete(ttTilde) then
   end
   begin
      Result.Combinator := scGeneralSibling;
      SelectorRequired := True;
*)
   end;

   // check if any selector follows
   if FTokenizer.TestAnyName then
      Result.Selector := ReadSelector
   else if SelectorRequired then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, 'Selector required');
end;

function TCssStyleSheet.ReadSelectors: TCssSelectorList;
begin
   Result := TCssSelectorList.Create;
   repeat
      Result.Add(ReadSelector);
   until not (FTokenizer.TestDelete(ttCOMMA));
end;

function TCssStyleSheet.ReadPropertyName: string;
begin
   // check for element name
   if not (FTokenizer.TestAnyName or FTokenizer.Test(ttTimes)) then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, RStrIdentifierExpected);

   Result := FTokenizer.GetToken.AsString;

   // now kill token and check whether further tokens are available
   FTokenizer.KillToken;
   if not FTokenizer.HasTokens then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');
end;

function TCssStyleSheet.ReadIdentifier: string;
begin
   Result := FTokenizer.GetToken.AsString;

   // now kill token and check whether further tokens are available
   FTokenizer.KillToken;
   if not FTokenizer.HasTokens then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');
end;

function TCssStyleSheet.ReadString: string;
begin
   Result := FTokenizer.GetToken.AsString;

   // now kill token and check whether further tokens are available
   FTokenizer.KillToken;
   if not FTokenizer.HasTokens then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');
end;

function TCssStyleSheet.ReadInteger: Double;
begin
   Result := FTokenizer.GetToken.FInteger;

   // now kill token and check whether further tokens are available
   FTokenizer.KillToken;
   if not FTokenizer.HasTokens then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');
end;

function TCssStyleSheet.ReadFloat: Double;
begin
   Result := FTokenizer.GetToken.FFloat;

   // now kill token and check whether further tokens are available
   FTokenizer.KillToken;
   if not FTokenizer.HasTokens then
      FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');
end;

function TCssStyleSheet.ReadNameTerm: TCssTerm;
var
   LocalStartPos: TScriptPos;
   Text, Uri, Number: string;
   Index: Integer;
   Expr: TCssExpression;
   Term: TCssTextTerm;
begin
   // get current position
   LocalStartPos := FTokenizer.HotPos;

   // read identifier
   Text := ReadIdentifier;
   Assert(Text <> '');

   // check for function
   if FTokenizer.TestDelete(ttBLEFT) then
   begin
      if FTokenizer.Test(ttBRIGHT) then
         FMsgs.AddCompilerStop(FTokenizer.HotPos, 'Expression expected');

      if SameText(Text, 'url') then
      begin
         Expr := TCssExpression.Create(FTokenizer.HotPos);

         Uri := '';
         while (not FTokenizer.Test(ttBRIGHT)) and FTokenizer.HasTokens do
         begin
            Uri := Uri + FTokenizer.GetToken.AsString;
            FTokenizer.KillToken;
         end;

         Term := TCssTextTerm.Create(FTokenizer.HotPos, Uri);
         Expr.TermList.Add(Term);
      end
      else
         // read function here
         Expr := ReadExpression;

      if not FTokenizer.TestDelete(ttBRIGHT) then
      begin
         FMsgs.AddCompilerErrorFmt(FTokenizer.HotPos, RStrTokenExpected, [')']);
         FTokenizer.SkipTo(ttBRIGHT);
      end;

      Exit(TCssFunctionTerm.Create(LocalStartPos, Text, Expr))
   end;

   // check for number value (with unit!)
   if CharInSet(Text[1], ['0'..'9']) then
   begin
      Index := 2;
      while (Index <= Length(Text)) and CharInSet(Text[Index], ['0'..'9', '.']) do
         Inc(Index);

      Number := Copy(Text, 1, Index - 1);
      Delete(Text, 1, Index - 1);
      Result := TCssNumberTerm.Create(LocalStartPos, StrToFloat(Number), Text);
   end
   else
   begin
      Result := TCssTextTerm.Create(LocalStartPos, Text);

      if FTokenizer.TestDelete(ttCOLON) then
      begin
         FMsgs.AddCompilerWarning(FTokenizer.HotPos, 'Token '':'' not expected here!');

         // skip to next semi colon or closing curly bracket
         while FTokenizer.HasTokens and (FTokenizer.TestAny([ttSEMI, ttCRIGHT]) = ttNone) do
            FTokenizer.KillToken;
      end;
   end;
end;

function TCssStyleSheet.ReadTerm: TCssTerm;
var
   LocalStartPos: TScriptPos;
begin
   Result := nil;

   // check for element name
   if FTokenizer.TestAnyName then
      Result := ReadNameTerm
   else
   begin
      // get current position
      LocalStartPos := FTokenizer.HotPos;

      if FTokenizer.Test(ttStrVal) then
         Result := TCssStringTerm.Create(LocalStartPos, ReadString)
      else if FTokenizer.Test(ttIntVal) then
         Result := TCssNumberTerm.Create(LocalStartPos, ReadInteger)
      else if FTokenizer.Test(ttFloatVal) then
         Result := TCssNumberTerm.Create(LocalStartPos, ReadFloat);
   end;
end;

function TCssStyleSheet.ReadExpression: TCssExpression;
var
   Term: TCssTerm;
   RequiresValue: Boolean;
begin
   Result := TCssExpression.Create(FTokenizer.HotPos);

   RequiresValue := False;
   repeat
      Term := ReadTerm;
      if Term = nil then
         if RequiresValue then
            FMsgs.AddCompilerStopFmt(FTokenizer.HotPos, RStrTokenExpected, ['Term'])
         else
            Break
      else
         Result.TermList.Add(Term);

      RequiresValue := FTokenizer.TestDelete(ttCOMMA);
   until not FTokenizer.HasTokens;
end;

function TCssStyleSheet.ReadPriority: Boolean;
begin
   Result := FTokenizer.TestDelete(ttEXCLAMATION);
   if Result then
   begin
      // check for element name
      if not FTokenizer.TestAnyName then
         FMsgs.AddCompilerStop(FTokenizer.HotPos, RStrIdentifierExpected);

      if not SameText(FTokenizer.GetToken.AsString, 'important') then
         FMsgs.AddCompilerStopFmt(FTokenizer.HotPos, RStrTokenExpected, [QuotedStr('important')]);

      // now kill token and check whether further tokens are available
      FTokenizer.KillToken;
      if not FTokenizer.HasTokens then
         FMsgs.AddCompilerStop(FTokenizer.HotPos, 'EOF');
   end;
end;

function TCssStyleSheet.ReadDeclaration: TCssDeclaration;
var
   LocalStartPos: TScriptPos;
   PropertyName: string;
   Expression: TCssExpression;
   Important: Boolean;
begin
   // get current position
   LocalStartPos := FTokenizer.HotPos;

   PropertyName := ReadPropertyName;

   // check for colon
   if not FTokenizer.TestDelete(ttCOLON) then
      FMsgs.AddCompilerStopFmt(FTokenizer.HotPos, RStrTokenExpected, [cTokenStrings[ttCOLON]]);

   Expression := ReadExpression;
   Important := ReadPriority;

   Result := TCssDeclaration.Create(LocalStartPos, PropertyName, Expression, Important);
end;

function TCssStyleSheet.ReadDeclarations: TCssDeclarations;
begin
   Result := TCssDeclarations.Create;

   // check curly bracket
   if not FTokenizer.TestDelete(ttCLEFT) then
      FMsgs.AddCompilerStopFmt(FTokenizer.HotPos, RStrTokenExpected, [cTokenStrings[ttCLEFT]]);

   while not FTokenizer.TestDelete(ttCRIGHT) do
   begin
      Result.Add(ReadDeclaration);

      // eventualy
      if not FTokenizer.TestDelete(ttSEMI) then
      begin
         // no semicolon -> check for closing bracket
         if not FTokenizer.TestDelete(ttCRIGHT) then
            FMsgs.AddCompilerStopFmt(FTokenizer.HotPos, RStrTokenExpected, [cTokenStrings[ttCRIGHT]]);

         Break;
      end;

//   FTokenizer.SkipTo(ttCRIGHT);
   end;
end;

function TCssStyleSheet.ReadRule: TCssRule;
var
   Declarations: TCssDeclarations;
   SelectorList: TCssSelectorList;
   LocalStartPos, LocalEndPos: TScriptPos;
begin
   LocalStartPos := FTokenizer.HotPos;
   SelectorList := ReadSelectors;
   Declarations := ReadDeclarations;
   LocalEndPos := FTokenizer.HotPos;
   Result := TCssRule.Create(LocalStartPos, LocalEndPos, SelectorList, Declarations);
end;

procedure TCssStyleSheet.ReadAtRule;
var
   Level: Integer;
   LocalStartPos, LocalEndPos: TScriptPos;
begin
   LocalStartPos := FTokenizer.HotPos;

   // ignore the content
   FTokenizer.SkipTo(ttCLEFT);
   if FTokenizer.TestDelete(ttCLEFT) then
   begin
      Level := 1;

      while FTokenizer.HasTokens do
      begin
         if FTokenizer.TestDelete(ttCRIGHT) then
         begin
            Dec(Level);
            if Level = 0 then
               Break;
         end else
         if FTokenizer.TestDelete(ttCLEFT) then
            Inc(Level)
         else
            FTokenizer.KillToken;
      end;
   end;

   LocalEndPos := FTokenizer.HotPos;
end;

procedure TCssStyleSheet.ReadRules(const RuleList: TCssRuleList);
begin
   while FTokenizer.HasTokens do
   begin
      // check at-rule
      if FTokenizer.TestDelete(ttAT) then
      begin
         ReadAtRule;
         FMsgs.AddCompilerWarning(FTokenizer.HotPos, 'At-rule not supported yet!');
      end
      else
         RuleList.Add(ReadRule);
   end;
end;

end.
