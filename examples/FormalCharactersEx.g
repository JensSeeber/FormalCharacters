#! @BeginChunk RationalCharactersEx
#! @BeginExample
G := SymmetricGroup(5); StructureDescription(G);
#! Sym( [ 1 .. 5 ] )
#! "S5"
CT := CharacterTable(G);; FCT := FormalCharacterTable(CT);
#! FormalCharacterTable( S5 )
i := Irr(FCT);          
#! [ [ 1, -1 + ~2, 1, 1, -1 + ~2, -1 + ~2, 1 ],
#! [ 4, -2 + 3*~2, 2*~2, 1 + ~3, 1 - ~3 + ~6, ~4, -1 + ~5 ], 
#! [ 5, -1 + 3*~2, 1 + 2*~2, -1 + 2*~3, -1 + ~6, 1 + ~4, ~5 ], 
#! [ 6, 3*~2, -2 + 4*~2, 2*~3, ~6, -~2 + 2*~4, 1 + ~5 ], 
#! [ 5, 1 + 2*~2, 1 + 2*~2, -1 + 2*~3, 1 - ~2 + ~6, -1 + ~2 + ~4, ~5 ], 
#! [ 4, 2 + ~2, 2*~2, 1 + ~3, -1 + ~2 + ~3, ~4, -1 + ~5 ], 
#! [ 1, 1, 1, 1, 1, 1, 1 ] ]
r := RationalCharacters(FCT);
#! [ [ 1, -1 + ~2, 1, 1, -1 + ~2, -1 + ~2, 1 ], 
#! [ 4, -2 + 3*~2, 2*~2, 1 + ~3, 1 - ~3 + ~6, ~4, -1 + ~5 ], 
#! [ 5, -1 + 3*~2, 1 + 2*~2, -1 + 2*~3, -1 + ~6, 1 + ~4, ~5 ], 
#! [ 6, 3*~2, -2 + 4*~2, 2*~3, ~6, -~2 + 2*~4, 1 + ~5 ], 
#! [ 5, 1 + 2*~2, 1 + 2*~2, -1 + 2*~3, 1 - ~2 + ~6, -1 + ~2 + ~4, ~5 ], 
#! [ 4, 2 + ~2, 2*~2, 1 + ~3, -1 + ~2 + ~3, ~4, -1 + ~5 ], 
#! [ 1, 1, 1, 1, 1, 1, 1 ] ]
i = r;
#! true
#! @EndExample
#! @EndChunk





#! @BeginChunk PolyaSubstitutionEx
#! @BeginExample
G := AlternatingGroup(5); StructureDescription(G);
#! Alt( [ 1 .. 5 ] )
#! "A5"
CT := CharacterTable(G); FCT := FormalCharacterTable(CT); 
#! CharacterTable( A5 )
#! FormalCharacterTable( A5 )
p := PermutationCharacter(G,Group((1,2,3,4,5)));
#! Character( CharacterTable( A5 ), [ 12, 0, 0, 2, 2 ] )
fp := FormalCharacter(FCT,p);
#! [ 12, 6*~2, 4*~3, 2 + 2*~5, 2 + 2*~5 ]
PolyaSubstitution(fp,n->Indeterminate(Rationals,Concatenation("x",String(n))));
#! 1/60*x1^12+1/4*x2^6+2/5*x1^2*x5^2+1/3*x3^4
x := Indeterminate(Rationals,"x");
#! x
y := Indeterminate(Rationals,"y");
#! y
PolyaSubstitution(fp,n->x^n+y^n);
#! x^12+x^11*y+3*x^10*y^2+5*x^9*y^3+
#! 12*x^8*y^4+14*x^7*y^5+24*x^6*y^6+
#! 14*x^5*y^7+12*x^4*y^8+5*x^3*y^9+
#! 3*x^2*y^10+x*y^11+y^12
#! @EndExample
#! @EndChunk





#! @BeginChunk IrrEx
#! @BeginExample
Q := QuaternionGroup(8); StructureDescription(Q);
#! <pc group of size 8 with 3 generators>
#! "Q8"
CT := CharacterTable(Q); FCT := FormalCharacterTable(CT);
#! CharacterTable( Q8 )
#! FormalCharacterTable( Q8 )
Irr(FCT);
#! [ [ 1, 1, 1, 1, 1 ], 
#! [ 1, -1 + ~2, -1 + ~2, 1, 1 ], 
#! [ 1, -1 + ~2, 1, 1, -1 + ~2 ], 
#! [ 1, 1, -1 + ~2, 1, -1 + ~2 ], 
#! [ 2, -~2 + ~4, -~2 + ~4, -2 + 2*~2, -~2 + ~4 ] ]
#! @EndExample
#! @EndChunk
