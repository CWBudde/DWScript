{$ifdef JS_CODEGEN}
for var i := 1 to 50 step 5 do
    PrintLn(ByteSizeToStr(Round(IntPower(2, i))));
{$else}
for var i := 1 to 50 step 5 do
    PrintLn(ByteSizeToStr(1 shl i));
{$endif}