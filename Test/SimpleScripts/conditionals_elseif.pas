{$IFDEF Foo}
   PrintLn('Foo');
{$ELSEIFDEF Bar}
   PrintLn('Bar');
{$ENDIF}

{$DEFINE Foo}

{$IFDEF Foo}
   PrintLn('Foo');
{$ELSEIFDEF Bar}
   PrintLn('Bar');
{$ENDIF}

{$DEFINE Bar}

{$IFDEF Foo}
   PrintLn('Foo');
{$ELSEIFDEF Bar}
   PrintLn('Bar');
{$ENDIF}
