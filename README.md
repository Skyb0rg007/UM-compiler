# um-compiler

This program is designed to compile my DSL into Comp 40 UM bytecode

While this is initially meant for writing tests, I want to extend my
DSL to have extra features, such as functions

Here is a sample from the DSL:

    $r[0] = 20;       // Note $r[0] is the same as R0
    $r[1] = 100;
    $r[8] = 400;
    $r[4] = 2;
    $r[2] = alloc $r[8];
    $m[$r[2]][$r[4]] = $r[0];
    $r[1] = $r[1] ~&~ $r[4];   // ~&~ is 'nand'
    print $r[1];
    free $r[2];
