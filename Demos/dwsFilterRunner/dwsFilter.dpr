program dwsFilter;

{$SetPEFlags $0001}

{$IFNDEF VER200} // delphi 2009
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
{$APPTYPE CONSOLE}

{$r *.dres}

uses
  Windows,
  Classes,
  SysUtils,
  dwsXPlatform,
  dwsComp,
  dwsCompiler,
  dwsExprs,
  dwsUtils,
  dwsFunctions,
  SynZip,
  dwsErrors,
  dwsMathFunctions,
  dwsStringFunctions,
  dwsTimeFunctions,
  dwsVariantFunctions,
  dwsFileFunctions,
  dwsClassesLibModule,
  dwsZipLibModule,
  dwsEncodingLibModule,
  dwsCryptoLibModule,
  dwsWebLibModule,
  dwsDatabaseLibModule,
  dwsComConnector,
  dwsJSONConnector,
  dwsSynSQLiteDatabase,
  dwsHtmlFilter;

function CreateScript : TDelphiWebScript;
begin
   Result := TDelphiWebScript.Create(nil);

   TdwsComConnector.Create(Result).Script:=Result;
   TdwsJSONLibModule.Create(Result).Script:=Result;
   TdwsClassesLib.Create(Result).dwsUnit.Script:=Result;
   TdwsEncodingLib.Create(Result).dwsEncoding.Script:=Result;
   TdwsCryptoLib.Create(Result).dwsCrypto.Script:=Result;
   TdwsZipLib.Create(Result).dwsZip.Script:=Result;
   TdwsWebLib.Create(Result).dwsWeb.Script:=Result;
   TdwsDatabaseLib.Create(Result).dwsDatabase.Script:=Result;
end;

procedure WriteHeader;
begin
   Writeln('dws Runner - sample code runner for DWScript');
   Writeln('');
end;

var
  InputFileName, OutputFileName: string;
  Text: string;
  Filter: TdwsHtmlFilter;
  Messages: TdwsMessageList;
  Script: TDelphiWebScript;
  Prog: IdwsProgram;
  Exec: IdwsProgramExecution;
  Params: array of Variant;
  Index: Integer;
begin
  if ParamCount < 2 then
  begin
    WriteHeader;
    Writeln('Run a DWS Filter on a text file:');
    Writeln('   dwsFilter <inputfile> <outputfile> [param1] [param2] ... [paramN]');
    Writeln('');
    Exit;
  end;

  InputFileName := ParamStr(1);
  OutputFileName := ParamStr(2);
  if not FileExists(InputFileName) then
  begin
    Writeln('File "', InputFileName, '" not found.');
    Exit;
  end;

  Text := LoadTextFromFile(InputFileName);
  try
    Filter := TdwsHtmlFilter.Create(nil);
    Messages := TdwsMessageList.Create;
    Text := Filter.Process(Text, Messages);
    if Messages.Count > 0 then
      if Messages.HasErrors then
      begin
        Writeln(Messages.AsInfo);
        Exit;
      end;

    Script := CreateScript;
    try
      Prog := Script.Compile(Text);
      if Prog.Msgs.Count > 0 then
        if Prog.Msgs.HasErrors then
        begin
          Writeln(Prog.Msgs.AsInfo);
          Exit;
        end;

      SetLength(Params, ParamCount - 2);
      for Index := 3 to ParamCount do
        Params[Index - 3] := ParamStr(Index);

      Exec := Prog.ExecuteParam(Params);
      Text := Exec.Result.ToString;

      SaveTextToUTF8File(OutputFileName, Text);
    finally
      Script.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
