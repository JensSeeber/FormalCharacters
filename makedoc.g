#
# FormalCharacters: This package blabla
#
# This file is a script which compiles the package manual.
#
if fail = LoadPackage("AutoDoc", ">= 2014.03.27") then
    Error("AutoDoc version 2014.03.27 is required.");
fi;

AutoDoc( "FormalCharacters" :
    scaffold := true, 
    autodoc := rec( scan_dirs := [ "examples", "lib" ] ,
    files := ["gap/FormalRoots.gd", "gap/FormalCharacters.gd"] ),
    maketest := true);

QUIT;
