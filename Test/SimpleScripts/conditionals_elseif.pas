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

{$UNDEF Foo}
{$IFDEF Foo}
   PrintLn('Foo');
{$ELSEIFDEF Bar}
   PrintLn('Bar');
{$ENDIF}

{$UNDEF Bar}
{$DEFINE FooBar}
{$IFDEF Foo}
   PrintLn('Foo');
{$ELSEIFDEF Bar}
   PrintLn('Bar');
{$ELSEIFDEF FooBar}
   PrintLn('FooBar');
{$ENDIF}