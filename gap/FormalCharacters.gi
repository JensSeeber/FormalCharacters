#################################################################################
#################################################################################
###                                                                           ###
###   FormalCharacters - a gap-package for calculation and manipulation       ###
###                      of formal characters                                 ###
###                                                                           ###
###   Copyright (C) 2014 Jens Seeber                                          ###
###                                                                           ###
###   This program is free software: you can redistribute it and/or modify    ###
###   it under the terms of the GNU General Public License as published by    ###
###   the Free Software Foundation, either version 3 of the License, or       ###
###   (at your option) any later version.                                     ###
###                                                                           ###
###   This program is distributed in the hope that it will be useful,         ###
###   but WITHOUT ANY WARRANTY; without even the implied warranty of          ###
###   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ###
###   GNU General Public License for more details.                            ###
###                                                                           ###
###   You should have received a copy of the GNU General Public License       ###
###   along with this program.  If not, see <http://www.gnu.org/licenses/>.   ###
###                                                                           ###
#################################################################################
#################################################################################



DeclareRepresentation( "IsFormalCharacterRep",
         IsFormalCharacter and IsAttributeStoringRep,
         []
);

DeclareRepresentation( "IsFormalCharacterTableRep",
         IsFormalCharacterTable and IsAttributeStoringRep,
         []
);

BindGlobal("TheFamilyOfFormalCharacterTables",
            NewFamily("TheFamilyOfFormalCharacterTables", IsFormalCharacterTable));

BindGlobal("TheFamilyOfFormalCharacters",
            NewFamily("TheFamilyOfFormalCharacters", IsFormalCharacter));

##############################################################
##
## Attributes
##
##############################################################

InstallMethod(UnderlyingGroup,
                [IsFormalCharacterTable],
            function(fct)
                local ct;
                ct := UnderlyingCharacterTable(fct);
                if IsBrauerTable(ct) then
                    return UnderlyingGroup(OrdinaryCharacterTable(ct));
                else
                    return UnderlyingGroup(ct);
                fi;
            end
);

InstallMethod(Irr,
                [IsFormalCharacterTable],
            function(fct)
                return List(Irr(UnderlyingCharacterTable(fct)),c -> FormalCharacter(fct,c));
            end
);

InstallMethod(RationalCharacters,
                [IsFormalCharacterTable],
            function(fct)
                local I,res,n,G,O,t;
                I := Irr(fct);
                n := Exponent(fct);
                G := Units(ZmodnZ(n));
                O := Orbits(G,I);
                res := List(O,Sum);
                for t in res do
                    List(UnderlyingList(t),PermSum);
                od;
                return res;
            end
);

InstallMethod(Exponent,
                [IsFormalCharacterTable],
            function(fct)
                return Exponent(UnderlyingCharacterTable(fct));
            end
);

InstallMethod(ConjugacyClasses,
                [IsFormalCharacterTable],
            function(fct)
                if HasUnderlyingCharacterTable(fct) then
                    return ConjugacyClasses(UnderlyingCharacterTable(fct));
                else
                    Error("Not yet implemented.");
                fi;
            end
);
                
InstallMethod(UnderlyingFCT,
                [IsFormalCharacter],
            function(fc)
                if HasUnderlyingFCT(fc) then
                    return UnderlyingCharacterTable(fc);
                else
                    Error("Formal character has no UnderlyingFCT. That's a problem.");
                fi;
            end
);

InstallMethod(UnderlyingGroup,
                [IsFormalCharacter],
            function(fc)
                return UnderlyingGroup(UnderlyingFCT(fc));
            end
);

InstallMethod(UnderlyingList,
                [IsFormalCharacter],
            function(fc)
                return List(UnderlyingClassFunction(fc));
            end
);

InstallMethod(ViewObj,
                [IsFormalCharacterTable],
            function(fct)
                local ct;
                ct := UnderlyingCharacterTable(fct);
                if HasUnderlyingGroup(ct) then
                    Print("FormalCharacterTable( ");ViewObj(UnderlyingGroup(fct));Print(" )");
                else
                    Print("FCT( "); ViewObj(UnderlyingCharacterTable(fct)); Print(" )");
                fi;
            end
);

InstallMethod(ViewObj,
                [IsFormalCharacter],
            function(fc)
                ViewObj(UnderlyingList(fc));
            end
);
                

######################################################################
##
## Copy Constructors
##
######################################################################

InstallMethod(FormalCharacterTable,
                "copy constructor",
                [IsFormalCharacterTable],
                IdFunc
);

InstallMethod(FormalCharacter,
                "copy constructor",
                [IsFormalCharacter],
                IdFunc
);

InstallMethod(ScalarProduct,
                " for formal Characters",
                [IsFormalCharacter, IsFormalCharacter],
            function(a,b)
                 return Indicator(a * Conjugate(b));
            end
);

######################################################################
##
## Methods
##
######################################################################

InstallMethod( \[\],
                [IsFormalCharacter, IsInt],
            function(fc,n)
                return UnderlyingClassFunction(fc)[n];
            end
);

InstallMethod(\=,
                [IsFormalCharacterTable, IsFormalCharacterTable],
                function(t1, t2)
                    return UnderlyingCharacterTable(t1) = UnderlyingCharacterTable(t2);
                end
);

InstallMethod(Specialize,
                [IsFormalCharacter, IsInt],
                function(fc,s)
                    return VirtualCharacter(UnderlyingCharacterTable(UnderlyingFCT(fc)),List(UnderlyingList(fc),x->Specialize(x,s)));
                end
);

InstallMethod(\^,
                [IsMultiplicativeElement, IsFormalCharacter],
            function(g, fc)
                return g ^ UnderlyingClassFunction(fc);
            end
);

InstallMethod(\+,
                [IsFormalCharacter, IsFormalCharacter],
            function(a,b)
                local res;
                if not UnderlyingFCT(a) = UnderlyingFCT(b) then
                    Error("FormalCharacterTables don't match.");
                fi;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res, UnderlyingFCT(a));
                SetUnderlyingClassFunction(res, UnderlyingClassFunction(a) + UnderlyingClassFunction(b));
                return res;
            end
);

InstallMethod(\-,
                [IsFormalCharacter, IsFormalCharacter],
            function(a,b)
                return a + (-1)*b;
            end
);

InstallMethod(\=,
                [IsFormalCharacter, IsFormalCharacter],
            function(a,b)
                local temp;
                temp := a - b;
                return ForAll(UnderlyingList(temp),a->a=\$(0));
            end
);

InstallMethod(\*,
                [IsFormalCharacter, IsFormalCharacter],
            function(a,b)
                local res;
                if not UnderlyingFCT(a) = UnderlyingFCT(b) then
                    Error("FormalCharacterTables don't match.");
                fi;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res, UnderlyingFCT(a));
                SetUnderlyingClassFunction(res, UnderlyingClassFunction(a) * UnderlyingClassFunction(b));
                return res;
            end
);

InstallMethod(\*,
                "multiply formal rootsum and formal character",
                [IsFormalRootSum, IsFormalCharacter],
            function(a,fc)
                local res,ct;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res,UnderlyingFCT(fc));
                ct := UnderlyingCharacterTable(UnderlyingFCT(fc));
                SetUnderlyingClassFunction(res,ClassFunction(ct,List(UnderlyingList(fc),x->a*x)));
                return res;
            end
);

InstallMethod(\*,
                "multiply rational and formal character",
                [IsRat, IsFormalCharacter],
            function(a,fc)
                local res;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res,UnderlyingFCT(fc));
                SetUnderlyingClassFunction(res, a * UnderlyingClassFunction(fc));
                return res;
            end
);

InstallMethod(\*,
                "multiply rational and formal character",
                [IsFormalCharacter,IsRat],
            function(fc,a)
                return a*fc;
            end
);

InstallMethod(\^,
                "take a power of a formal character",
                [IsFormalCharacter, IsInt],
            function(fc,z)
                local res,ct;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res,UnderlyingFCT(fc));
                ct := UnderlyingCharacterTable(UnderlyingFCT(fc));
                SetUnderlyingClassFunction(res,ClassFunction(ct,List(UnderlyingList(fc),x->x^z)));
                return res;
            end
);

InstallMethod( Power,
                "apply a morphism to a formal character",
                [IsFormalCharacter, IsInt],
            function(fc,z)
                local res,ct;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res,UnderlyingFCT(fc));
                ct := UnderlyingCharacterTable(UnderlyingFCT(fc));
                SetUnderlyingClassFunction(res,ClassFunction(ct,List(UnderlyingList(fc),x->Power(x,z))));
                return res;
            end
);

InstallMethod(\^,
                "apply a morphism to a formal character",
                [IsFormalCharacter, IsZmodnZObj],
            function(fc,z)
                local res,ct;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res,UnderlyingFCT(fc));
                ct := UnderlyingCharacterTable(UnderlyingFCT(fc));
                SetUnderlyingClassFunction(res,ClassFunction(ct,List(UnderlyingList(fc),x->x^z)));
                return res;
            end
);

InstallMethod(\^,
                "apply a morphism to a formal character",
                [IsFormalCharacter, IsFFE],
            function(fc,z)
                local res,ct;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res,UnderlyingFCT(fc));
                ct := UnderlyingCharacterTable(UnderlyingFCT(fc));
                SetUnderlyingClassFunction(res,ClassFunction(ct,List(UnderlyingList(fc),x->x^z)));
                return res;
            end
);

InstallMethod(Conjugate,
                "conjugate formal character",
                [IsFormalCharacter],
            function(fc)
                local res,ct;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res,UnderlyingFCT(fc));
                ct := UnderlyingCharacterTable(UnderlyingFCT(fc));
                SetUnderlyingClassFunction(res,ClassFunction(ct,List(UnderlyingList(fc),Conjugate)));
                return res;
            end
);

InstallMethod(Indicator,
                [IsFormalCharacter],
            function(fc)
                local G,centOrder,res,CT;
                CT := UnderlyingCharacterTable(UnderlyingFCT(fc));
                centOrder := ClassFunction(CT,SizesConjugacyClasses(CT));
                res := (1/Size(CT))*Sum(List(UnderlyingClassFunction(fc)*centOrder)); #\frac{1}{|G|} \sum_{g \in G} fc(g)
                return res;
            end
);

InstallMethod(GroupIndicator,
                [IsFormalCharacterTable],
            function(fct)
                local list,res;
                list := List(Irr(fct),Indicator);
                res := Sum(List([1..Length(list)],i->list[i]*Irr(fct)[i]));
                return res;
            end
);

InstallMethod(VirtualPowerCharacter,
                [IsCharacter, IsInt],
            function(chi, n)
                local CT;
                CT := UnderlyingCharacterTable(chi);
                return VirtualCharacter(CT,CompositionMaps(List(chi),PowerMap(CT,n)));
            end
);

InstallMethod(SymCharacter,
                [IsCharacter],
            function(chi)
                return (chi^2 + VirtualPowerCharacter(chi,2))/2;
            end
);

InstallMethod(AltCharacter,
                [IsCharacter],
            function(chi)
                return (chi^2 - VirtualPowerCharacter(chi,2))/2;
            end
);

InstallMethod(PolyaSubstitution,
                [IsFormalCharacter, IsFunction],
            function(fc, f)
                local G,c,centOrder,CT;
                CT := UnderlyingCharacterTable(UnderlyingFCT(fc));
                c := ClassFunction(CT,List(List(UnderlyingList(fc),PermSum),S->Product(S,p->f(p[2])^p[1])));
                centOrder := ClassFunction(CT,SizesConjugacyClasses(CT));
                return 1/Size(CT) * Sum(List(c*centOrder));
            end
);

InstallMethod(FormalCharacterByValues,
                "Creates formal character by list",
                [IsFormalCharacterTable, IsList],
            function(fct, list)
                local res,ct;
                if IsAttributeStoringRep(list) then #catch the case, where list is a class function
                    TryNextMethod();
                fi;
                if not ForAll(list, IsFormalRootSum) then
                    Error("The listelements have to be formal rootsums");
                fi;
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingFCT(res, fct);
                ct := UnderlyingCharacterTable(fct);
                SetUnderlyingClassFunction(res, ClassFunction(ct,list));
                return res;
            end
);

InstallMethod(FormalCharacter,
                "Creates formal character by ordinary one",
                [IsClassFunction],
            function(char)
                local CT, FCT;
                CT := UnderlyingCharacterTable(char);
                FCT := FormalCharacterTable(CT);
                return FormalCharacter(FCT,char);
            end
);

InstallMethod(FormalCharacter,
                "Creates formal character by ordinary one",
                [IsFormalCharacterTable, IsClassFunction],
            function(ftable,char)
                local CT, res,r,formal,restricted,cycsubtbl,L,k,temp,cyc,CharList,OCR,orders,fusion,possible,gencyc,o;
                CT:=UnderlyingCharacterTable(char);
                if not CT = UnderlyingCharacterTable(ftable) then
                    Error("Character table of formal character table and character do not match.");
                fi;
                L := [];
                CharList:=List(char);
                if ForAll(CharList,IsRat) then #rational Character!
                    OCR := OrdersClassRepresentatives(CT);
                    for r in [1..Length(CharList)] do
                        Add(L, Sum(DivisorsInt(OCR[r]),d->(1/d*(Sum(DivisorsInt(d),k->MoebiusMu(d/k)*CharList[PowerMap(CT,k)[r]])))*\$(d)));
                    od;
                    res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                    SetUnderlyingClassFunction(res, ClassFunction(CT,L));
                    SetUnderlyingFCT(res,ftable);
                    return res;
                fi;
                #no rational Character
                orders := Set(OrdersClassRepresentatives(CT)); #all orders of cyclic subgroups
                for o in orders do #for all possible orders
                    cyc := CyclicGroup(o);
                    if IsEmpty(GeneratorsOfGroup(cyc)) then  #trivial group
                        gencyc := One(cyc);
                    else
                        gencyc := GeneratorsOfGroup(cyc)[1];
                    fi;
                    cycsubtbl := CharacterTable(cyc);
                    possible := PossibleClassFusions(cycsubtbl,CT); #all cyclic subgroups of order o and the way they are seen in the character table
                    for fusion in possible do
                        StoreFusion(cycsubtbl,fusion,CT); #Chose one possible cyclic subgroup
                        restricted := Restricted(char,cycsubtbl); #restrict character to cyclic subgroup
                        formal := Sum(List(Irr(cycsubtbl),i->ScalarProduct(restricted,i)*FormalRoot(gencyc^i)));
                        if Length(fusion) = 1 then
                            L[1] := formal; #We hit the trivial subgroup
                        else
                            L[fusion[2]] := formal; #fusion[2] tells us, which conjugacy class is regarded as containing cycgen
                        fi;
                        cyc := CyclicGroup(o);   #delete the stored class fusion by defining a fresh cyclic group
                        if IsEmpty(GeneratorsOfGroup(cyc)) then  #trivial group
                            gencyc := One(cyc);
                        else
                            gencyc := GeneratorsOfGroup(cyc)[1];
                        fi;
                        cycsubtbl := CharacterTable(cyc); 
                    od;
                    #if Order(r) = 1 then #Spezialfall: Konjugiertenklasse der 1
                    #    Add(L,char[1]*FormalRoot(0,1));
                    #else
                    #fi;
                od; 
                res := Objectify( NewType( TheFamilyOfFormalCharacters, IsFormalCharacterRep), rec());
                SetUnderlyingClassFunction(res,ClassFunction(CT,L));
                SetUnderlyingFCT(res,ftable);
                return res;
            end
);

InstallMethod(FormalCharacterTable,
                "Creates a formal character table from an ordinary one.",
                [IsCharacterTable],
            function(ct)
                local res;
                res := Objectify( NewType( TheFamilyOfFormalCharacterTables, IsFormalCharacterTableRep), rec());
                SetUnderlyingCharacterTable(res,ct);
                return res;
            end
);

InstallMethod(Display,
                [IsFormalCharacterTable],
                function(fct)
                    local array, arr,  max,  l,  k,j;
                    array := List(Irr(fct),UnderlyingList);
                    PrintMat(array);
                end
);

InstallMethod( Display, 
        "Display a list of formal characters",
        [IsList],
        function(L)
            local array;
            if ForAll(L,IsFormalCharacter) then
                array := List(L,UnderlyingList);
                PrintMat(array);
            else
                TryNextMethod();
            fi;
        end
);

InstallMethod( PrintMat,
        "Displays a matrix more compact than PrintArray",
        [IsList],
        function(array)
            local arr,max,j,l,k;
            if Length( array ) = 0  then
                Print( "[ ]\n" );
            elif array = [[]]  then
                Print( "[ [ ] ]\n" );
            else
                if not ForAll(array, IsList) then
                    Error("The matrix to be printed is no matrix");
                fi;
                if not ForAll(array,x->Length(x)=Length(array[1])) then
                    Error("A matrix is rectangular.");
                fi;
                arr := List( array, x -> List( x, String ) ); #arr is a list of lists of strings
                max := [];
                for j in [1..Length(arr[1])] do
                    Add(max,Maximum(List([1..Length(arr)],i->Length(arr[i][j])))); #for every column we take the maximum width of an entry
                od;

                #Print( "[ " );
                for l in [ 1 .. Length( arr ) ] do
                    Print( "[ " );
                    if Length(arr[ l ]) = 0 then
                        Print("  ]" );
                    else
                        for k  in [ 1 .. Length( arr[ l ] ) ]  do
                            Print( String( arr[ l ][ k ], max[k] + 1 ) );
                            if k = Length( arr[ l ] )  then
                                Print( " ]" );
                            else
                                Print( ", " );
                            fi;
                        od;
                    fi;
                    if l = Length( arr )  then
                        #Print( " ]\n" );
                    else
                        Print( ",\n" );
                    fi;
                od;
            fi;
        end
);
