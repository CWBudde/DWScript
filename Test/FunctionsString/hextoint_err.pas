﻿var s : String = 'JKL';
var i : Integer;

try
   i:=HexToInt(s);
   {$ifdef INLINE_MAGICS}
   asm
      if (!isFinite(@i)) PrintLn('Except', @i=0);
   end;
   {$endif}
except
   PrintLn('Except');
end;
PrintLn(i);