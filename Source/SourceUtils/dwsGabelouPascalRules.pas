unit dwsGabelouPascalRules;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils, Character,
   dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsCoreExprs, dwsTokenizer,
   dwsStrings, dwsUnitSymbols, dwsGabelou, dwsGabelouStrings,
   dwsSymbolDictionary;

type
   TGR_InfixCapsParameters = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_InfixCapsLocalVariables = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_InfixCapsFunctions = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_InfixCapsProperties = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_InfixCapsTypes = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_PrefixedFields = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_PrefixedClasses = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_ConstantNamingRules = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_NoUnderscores = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

resourcestring
   GAB_HintMessage = 'Gabelou: %s';

   GAB_PrefixedFields_Name = 'Prefixed fields';
   GAB_PrefixedFields_Description = 'Private or protected fields should start with "F" followed by InfixCaps. Public fields should follow InfixCaps.';

   GAB_PrefixedClasses_Name = 'Prefixed classes';
   GAB_PrefixedClasses_Description = 'Classes should start with "T", "J" or "E" followed by InfixCaps or named "Exception".';

   GAB_ConstsNamingRules_Name = 'Constant naming rules';
   GAB_ConstsNamingRules_Description = 'Constant names should either start with a "C" followed by InfixCaps or be ALL_CAPS up to the first "_"';

   GAB_InfixCapsParameters_Name = 'InfixCaps parameters';
   GAB_InfixCapsParameters_Description = 'Parameter names should follow InfixCaps and start with an upper-case character';

   GAB_InfixCapsLocalVariables_Name = 'InfixCaps local variables';
   GAB_InfixCapsLocalVariables_Description = 'Local variables names should follow InfixCaps and start with an upper-case character';

   GAB_InfixCapsFunctions_Name = 'InfixCaps functions';
   GAB_InfixCapsFunctions_Description = 'Function names should follow InfixCaps and start with an upper-case character';

   GAB_InfixCapsProperties_Name = 'InfixCaps properties';
   GAB_InfixCapsProperties_Description = 'Property names should follow InfixCaps and start with an upper-case character';

   GAB_InfixCapsTypes_Name = 'InfixCaps types';
   GAB_InfixCapsTypes_Description = 'Type names should follow InfixCaps and start with an upper-case character';

   GAB_NoUnderscores_Name = 'No Underscores';
   GAB_NoUnderscores_Description = 'Identifiers should not contain an underscore';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGR_InfixCapsParameters ------------------
// ------------------

// Create
//
constructor TGR_InfixCapsParameters.Create;
begin
   Name:=GAB_InfixCapsParameters_Name;
   Description:=GAB_InfixCapsParameters_Description;
end;

// EvaluateSymbol
//
procedure TGR_InfixCapsParameters.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if not (aSymbolList.Symbol is TParamSymbol) then Exit;
   if aSymbolList.Symbol.Name='Self' then Exit;

   if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_InfixCapsLocalVariables ------------------
// ------------------

// Create
//
constructor TGR_InfixCapsLocalVariables.Create;
begin
   Name:=GAB_InfixCapsLocalVariables_Name;
   Description:=GAB_InfixCapsLocalVariables_Description;
end;

// EvaluateSymbol
//
procedure TGR_InfixCapsLocalVariables.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if    (aSymbolList.Symbol.ClassType<>TDataSymbol)
      or (TDataSymbol(aSymbolList.Symbol).Level<=0) then Exit;

   if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_InfixCapsFunctions ------------------
// ------------------

// Create
//
constructor TGR_InfixCapsFunctions.Create;
begin
   Name:=GAB_InfixCapsFunctions_Name;
   Description:=GAB_InfixCapsFunctions_Description;
end;

// EvaluateSymbol
//
procedure TGR_InfixCapsFunctions.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if not (aSymbolList.Symbol is TFuncSymbol) then Exit;

   if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_InfixCapsProperties ------------------
// ------------------

// Create
//
constructor TGR_InfixCapsProperties.Create;
begin
   Name:=GAB_InfixCapsProperties_Name;
   Description:=GAB_InfixCapsProperties_Description;
end;

// EvaluateSymbol
//
procedure TGR_InfixCapsProperties.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if aSymbolList.Symbol.ClassType<>TPropertySymbol then Exit;

   if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_InfixCapsTypes ------------------
// ------------------

// Create
//
constructor TGR_InfixCapsTypes.Create;
begin
   Name:=GAB_InfixCapsTypes_Name;
   Description:=GAB_InfixCapsTypes_Description;
end;

// EvaluateSymbol
//
procedure TGR_InfixCapsTypes.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if not (aSymbolList.Symbol is TTypeSymbol) then Exit;
   if aSymbolList.Symbol is TFuncSymbol then Exit;
   if aSymbolList.Symbol is TUnitSymbol then Exit;

   if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_ConstantNamingRules ------------------
// ------------------

// Create
//
constructor TGR_ConstantNamingRules.Create;
begin
   Name:=GAB_ConstsNamingRules_Name;
   Description:=GAB_ConstsNamingRules_Description;
end;

// EvaluateSymbol
//
procedure TGR_ConstantNamingRules.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);

   function ChecksCPrefix(const s : String) : Boolean;
   begin
      Result:=(Length(s)>=2) and (s[1]='C') and TCharacter.IsUpper(s[2]);
   end;

   function ChecksAllCapsUpToUnderscore(const s : String) : Boolean;
   var
      c : Char;
      i : Integer;
   begin
      for i:=1 to Length(s) do begin
         c:=s[i];
         if c='_' then
            break
         else if TCharacter.IsLower(c) then
            Exit(False);
      end;
      Result:=True;
   end;

begin
   if not (aSymbolList.Symbol is TConstSymbol) then Exit;
   if aSymbolList.Symbol is TElementSymbol then Exit;
   if aSymbolList.Symbol.Name='Null' then Exit;

   if not (ChecksCPrefix(aSymbolList.Symbol.Name) or ChecksAllCapsUpToUnderscore(aSymbolList.Symbol.Name)) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_PrefixedFields ------------------
// ------------------

// Create
//
constructor TGR_PrefixedFields.Create;
begin
   Name:=GAB_PrefixedFields_Name;
   Description:=GAB_PrefixedFields_Description;
end;

// EvaluateSymbol
//
procedure TGR_PrefixedFields.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if aSymbolList.Symbol.ClassType<>TFieldSymbol then Exit;

   if not (TFieldSymbol(aSymbolList.Symbol).Visibility in [cvPublic, cvPublished]) then begin
      if    (Length(aSymbolList.Symbol.Name)<2)
         or (aSymbolList.Symbol.Name[1]<>'F') or TCharacter.IsLower(aSymbolList.Symbol.Name[2]) then
         TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
   end else begin
      if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
         TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
   end;
end;

// ------------------
// ------------------ TGR_PrefixedClasses ------------------
// ------------------

// Create
//
constructor TGR_PrefixedClasses.Create;
begin
   Name:=GAB_PrefixedClasses_Name;
   Description:=GAB_PrefixedClasses_Description;
end;

// EvaluateSymbol
//
procedure TGR_PrefixedClasses.EvaluateSymbol(
  const aSymbolList: TSymbolPositionList; msgs: TdwsMessageList);
begin
   if aSymbolList.Symbol.ClassType<>TClassSymbol then Exit;

   if (Length(aSymbolList.Symbol.Name)<2)
      or not ((CharInSet(aSymbolList.Symbol.Name[1], ['T', 'J'])
      and TCharacter.IsUpper(aSymbolList.Symbol.Name[2]))
      or ((aSymbolList.Symbol.Name[1]='E')
      and (TCharacter.IsUpper(aSymbolList.Symbol.Name[2])
      or (aSymbolList.Symbol.Name = 'Exception')))) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_NoUnderscores ------------------
// ------------------

constructor TGR_NoUnderscores.Create;
begin
   Name:=GAB_NoUnderscores_Name;
   Description:=GAB_NoUnderscores_Description;
end;

procedure TGR_NoUnderscores.EvaluateSymbol(
  const aSymbolList: TSymbolPositionList; msgs: TdwsMessageList);
const
   CSymbolList: array [0 .. 2] of TSymbolClass = (TDataSymbol, TTypeSymbol,
     TPropertySymbol);
begin
   if not ((aSymbolList.Symbol is TDataSymbol) or (aSymbolList.Symbol is
      TTypeSymbol) or (aSymbolList.Symbol is TPropertySymbol)) then
      Exit;

   if Pos('_', aSymbolList.Symbol.Name) > 0 then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsGabelou.RegisterRuleClasses([
      TGR_InfixCapsParameters, TGR_InfixCapsLocalVariables, TGR_InfixCapsTypes,
      TGR_InfixCapsFunctions, TGR_InfixCapsProperties,

      TGR_ConstantNamingRules,

      TGR_PrefixedFields, TGR_PrefixedClasses,

      TGR_NoUnderscores]);

end.
