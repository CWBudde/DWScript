var a, b : String;

PrintLn(StrFind(a, b));

a:='banana';
PrintLn(StrFind(a, b));

b:='ana';
PrintLn(StrFind(a, b));
PrintLn(StrFind(a, b, 3));

b:='zzz';
PrintLn(StrFind(a, b));

a:='';
PrintLn(StrFind(a, b));
PrintLn(a.IndexOf(''));

a:='banana';
b:='';
PrintLn(a.IndexOf(b));

b:='nan';
PrintLn(a.IndexOf(b));

PrintLn('hello'.IndexOf('world'));
PrintLn('hellohello'.IndexOf('hell'));
PrintLn('hellohello'.IndexOf('hell', 2));

