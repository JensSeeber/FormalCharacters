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

####################################
##
## Representations
##
####################################

DeclareRepresentation( "IsFormalRootSumRep",
        IsFormalRootSum and IsAttributeStoringRep,
        []
);

DeclareRepresentation( "IsPermSumRep",
        IsFormalRootSum and IsAttributeStoringRep and IsFormalRootSumRep,
        []
);

DeclareRepresentation( "IsGaloisSumRep",
        IsFormalRootSum and IsAttributeStoringRep and IsFormalRootSumRep,
        []
);

DeclareRepresentation( "IsFormalRootGalRep",
        IsFormalRoot and IsAttributeStoringRep and IsGaloisSumRep,
        []
);

DeclareRepresentation( "IsFormalRootRep",
        IsFormalRoot and IsAttributeStoringRep and IsFormalRootSumRep,
        []
);

####################################
##
## Types and Families
##
####################################

BindGlobal( "TheFamilyOfFormalRoots",
        NewFamily( "TheFamilyOfFormalRoots", IsFormalRoot)
);

BindGlobal( "TheFamilyOfFormalRootSums",
        NewFamily( "TheFamilyOfFormalRootSums", IsFormalRootSum)
);

####################################
##
## Attributes
##
####################################

InstallMethod( Zero,
                [IsFormalRootSum],
            function(frs)
                return \$(0);
            end
);

InstallMethod( ZeroMutable,
                [IsFormalRootSum],
            function(frs)
                return \$(0); #note, that this is NOT mutable, but there is no such thing as a mutable formal root sum.
            end
);

InstallMethod( DegreeRoot,
                [IsFormalRoot],
            function(fr)
                return DenominatorRat(Frac(fr));
            end
);

InstallMethod( Terms,
                [IsFormalRootSum],
            function(fr)
                local res,terms,t,G,level;
                if IsFormalRoot(fr) then
                    return List([[1,fr]]); #a formal root just has one summand - itself
                else
                    if HasPermSum(fr) then
                        #Info(InfoWarning,1,"I have a formal root sum in perm form and start calculating it's coeff' list. That's probably a bad idea.");
                            #Debug warning for efficiency
                        #ViewObj(fr);
                        res := RegularGalois(0); #RegularGalois enforces galois representation, so this is suitible for the second part of a mixed sum
                        terms := PermSum(fr);
                        for t in terms do
                            res := res + t[1]*Sum([1..t[2]],i-> FormalRootGal(i,t[2]));
                        od;
                        return Terms(res);
                    elif HasGaloisSum(fr) then
                        level := InfoLevel(InfoWarning); #Sometimes calls of ZmodnZObj generates peculiar warnings, which are not interesting at all
                        SetInfoLevel(InfoWarning, 0); #Therefore we temporary disable warnings
                        res := RegularGalois(0);
                        for t in GaloisSum(fr) do
                            if t[3] = '/' then
                                G := Units(ZmodnZ(DegreeRoot(t[2])));
                            else
                                G := Group(t[3],One(ZmodnZ(DegreeRoot(t[2]))));
                            fi;
                            res := res + t[1] * Sum(Orbit(G,t[2])); #convert GaloisSum (given by orbits) in a full list of terms
                        od;
                        SetInfoLevel(InfoWarning, level);
                        return Terms(res);
                    elif HasMixedSum(fr) then
                        Info(InfoWarning,1,"I have a formal root sum in mixed form and start calculating it's coeff' list. That should not be necessary.");
                        return Union(Terms(PermPart(fr)),Terms(GaloisPart(fr)));
                    fi;
                fi;
            end
);

InstallMethod( AdditiveInverseAttr,
                [IsFormalRootSum],
            function(frs)
                local res,terms;
                if IsPermSumRep(frs) then
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsPermSumRep), rec());
                    terms:=List(PermSum(frs),t->[-t[1],t[2]]);
                    SetPermSum(res,terms);
                elif IsGaloisSumRep(frs) then
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsGaloisSumRep), rec());
                    terms:=List(Terms(frs),t->[-t[1],t[2]]);
                    SetTerms(res,terms);
                else
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsFormalRootSumRep), rec());
                    terms := List(MixedSum(frs),x -> -x);
                    SetMixedSum(res,terms);
                fi;
                return res;
            end
);

## ByDegree sorts the formal root sum by degree of formal roots.

InstallMethod( ByDegree,
                [IsFormalRootSum],
            function(frs)
                local res,terms,a,block;
                terms := Terms(frs);
                res := [];
                while not terms = [] do
                    a := terms[1];
                    block := Filtered(terms,t->DegreeRoot(t[2])=DegreeRoot(a[2]));
                    terms:=Difference(terms,block);
                    Add(res,block);
                od;
                return res;
            end
);

## Computes a composition in Galois-orbits as a list of [m,r,g] where m is a coefficient, r is a representative of an orbit and g generates the Galois-group

InstallMethod( GaloisSum,
                [IsFormalRootSum],
            function(frs)
                local res,byDeg,l,list,roots,o,m,G,S,part,term,level;
                byDeg := ByDegree(frs);
                if byDeg = [] then
                    return [];
                fi;
                res := [];
                level := InfoLevel(InfoWarning);
                SetInfoLevel(InfoWarning, 0); #disable annoying warnings
                for l in byDeg do
                    list := l; # all terms in list have the same degree
                    while not list = [] do
                        roots := Set(list, t->t[2]);
                        G:=Units(ZmodnZ(DegreeRoot(roots[1])));
                        S:=Stabilizer(G,roots,OnSets); #roots is an S-set
                        for o in Orbits(S,roots) do #try to identify as many and as huge orbits as possible
                            part := Filtered(list,t->t[2] in o);
                            m:=Minimum(List(part,t->t[1]));
                            if S = G then
                                term:=[m,o[1],'/']; # '/' represents the whole group
                            else                   
                                term:=[m,o[1],GeneratorsOfGroup(S)];
                            fi;
                            Add(res, term);
                            list := List(list, function(t) if t[2] in o then return [t[1]-m,t[2]]; else return t; fi; end);
                            list := Filtered(list, t-> not t[1] = 0);
                        od;
                    od;
                od;
                SetInfoLevel(InfoWarning, level); #reenable warnings
                return res;
            end
);

InstallMethod( PermPart,
                [IsFormalRootSum],
            function(frs)
                return MixedSum(frs)[1];
            end
);

InstallMethod( GaloisPart,
                [IsFormalRootSum],
            function(frs)
                return MixedSum(frs)[2];
            end
);


InstallMethod( MixedSum,
                [IsFormalRootSum],
            function(frs)
                local reg,irr,terms,t,G,n;
                if IsFormalRootRep(frs) then
                    irr := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootGalRep), rec());
                    SetFrac(irr,Frac(frs));
                    return [\$(0),irr];
                fi;
                if IsPermSumRep(frs) then
                    return [frs, RegularGalois(0)];
                fi;
                terms := Filtered(GaloisSum(frs),t->t[3] = '/'); #those parts can be rewritten in terms of ~n
                reg := \$(0);
                for t in terms do
                    n := DegreeRoot(t[2]);
                    reg := reg + t[1]*Sum(DivisorsInt(n),k->MoebiusMu(n/k) * \$(k));
                od;
                terms := Filtered(GaloisSum(frs), t -> not t[3] = '/');
                irr := Objectify( NewType( TheFamilyOfFormalRootSums, IsGaloisSumRep), rec());
                SetGaloisSum(irr,terms);
                return [reg, irr];
            end
);

InstallMethod( PermSum,
                [IsFormalRootSum],
            function(frs)
                if GaloisPart(frs) = RegularGalois(0) then
                    return PermSum(PermPart(frs));
                else
                    Error("This formal root doesn't have a permutation representation. Try MixedSum instead.");
                fi;
            end
);

InstallMethod( MixedString,
                "Gives the string representation as a mixed sum. Is printed if DisplayMode is set to mixed.",
                [IsFormalRootSum],
                function(frs)
                    local ms,res;
                    if GaloisPart(frs) = RegularGalois(0) then
                        return PermString(PermPart(frs));
                    elif PermPart(frs) = \$(0) then
                        return GaloisString(GaloisPart(frs));
                    else
                        res := List(PermString(PermPart(frs)));
                        Append(res, " + ");
                        Append(res, GaloisString(GaloisPart(frs)));
                        return res;
                    fi;
                end
);

InstallMethod( PermString,
                "Gives the string representation as a permsum. Is printed if DisplayMode is set to perm.",
                [IsFormalRootSum],
                function(frs)
                    local a,terms,frac,res,coeff;
                    res := "";
                    terms := PermSum(frs);

                    if terms = [] then
                        Add(res,'0');
                        return res;
                    fi;
                    if terms[1][2] = 1 then
                        Append(res, String(terms[1][1])); #don't print coeff 1
                    else
                        if not terms[1][1] = 1 then
                            if IsInt(terms[1][1]) then
                                if terms[1][1] = -1 then
                                    Add(res, '-'); #write -~n rather than -1*~n
                                else
                                    Append(res,String(terms[1][1]));
                                    Add(res, '*');
                                fi;
                            else
                                Add(res,'(');
                                Append(res,String(terms[1][1])); #a rational as a coeff has to be put in brackets
                                Add(res,')');
                                Add(res, '*');
                            fi;
                                
                        fi;
                        Add(res, '~');
                        Append(res,String(terms[1][2]));
                    fi;

                    terms:=DifferenceLists(terms,[terms[1]]);
                    for a in terms do
                        if a[1] < 0 then
                            Append(res," - "); #write - a*~n rather than + (-a)*~n
                            coeff := -a[1];
                        else
                            Append(res," + ");
                            coeff := a[1];
                        fi;
                        if a[2] = 1 then
                            Append(res,String(a[1])); #don't print coeff 1
                        else
                            if not coeff = 1 then
                                if IsInt(coeff) then
                                    Append(res,String(coeff));
                                else
                                    Add(res,'(');
                                    Append(res,String(coeff)); #put rational coeff's in brackets
                                    Add(res,')');
                                fi;
                                Add(res, '*');
                            fi;
                            Add(res, '~');
                            Append(res,String(a[2]));
                        fi;
                    od;
                    return res;
                end
);

InstallMethod( GaloisString,
                "Gives the string representation as a permsum. Is printed if DisplayMode is set to perm.",
                [IsFormalRootSum],
                function(frs)
                    local a,terms,frac,res,coeff;
                    res := "";
                    if IsFormalRoot(frs) then
                        frac := Frac(frs);
                        Add(res, '\(');
                        Append(res,String(frac));
                        Add(res,')' );
                    else
                        terms := GaloisSum(frs);

                        if terms = [] then
                            Add(res,'0');
                            return res;
                        fi;
                        if terms[1][2] = FormalRootGal(0,1) then
                            Append(res,String(terms[1][1]));
                        else
                            if not terms[1][1] = 1 then
                                if IsInt(terms[1][1]) then
                                    if terms[1][1] = -1 then
                                        Add(res, '-'); #write -/n rather than -1*/n
                                    else
                                        Append(res,String(terms[1][1]));
                                        Add(res, '*');
                                    fi;
                                else
                                    Add(res,'(');
                                    Append(res,String(terms[1][1])); #rational coeff's in brackets
                                    Add(res,')');
                                    Add(res, '*');
                                fi;
                                    
                            fi;
                            if not terms[1][3] = [] then
                                if terms[1][3] = '/' then
                                    Add(res, '/');
                                    Append(res,String(DenominatorRat(Frac(terms[1][2]))));
                                elif IsList(terms[1][3]) then
                                    Append(res,String(terms[1][2]));
                                    Add(res,'^');
                                    Append(res,String(List(terms[1][3],Int)));
                                else
                                    Error("This should not happen. Only '/' and generators of a group allowed in a GaloisSum.");
                                fi;
                            else
                                Append(res,String(terms[1][2])); 
                            fi;
                        fi;
                        terms:=DifferenceLists(terms,[terms[1]]);
                        for a in terms do
                            if a[1] < 0 then
                                Append(res," - "); #write - a * x rather than + (-a) * x
                                coeff := -a[1];
                            else
                                Append(res," + ");
                                coeff := a[1];
                            fi;
                            if a[2] = FormalRootGal(0,1) then
                                Append(res,String(a[1]));
                            else
                                if not coeff = 1 then
                                    if IsInt(coeff) then
                                        Append(res,String(coeff));
                                    else
                                        Add(res,'(');
                                        Append(res,String(coeff));
                                        Add(res,')');
                                    fi;
                                    Add(res, '*');
                                fi;
                                if not a[3] = [] then
                                    if a[3] = '/' then
                                        Add(res, '/');
                                        Append(res,String(DenominatorRat(Frac(a[2]))));
                                    elif IsList(a[3]) then #List of FFE
                                        Append(res,String(a[2]));
                                        Add(res,'^');
                                        Append(res,String(List(a[3],Int)));
                                    else
                                        Error("This should not happen. Only / and generators of a group allowed in a GaloisSum.");
                                    fi;
                                else
                                    Append(res,String(a[2])); 
                                fi;
                            fi;
                        od;
                    fi;
                    return res;
                end
);

InstallMethod( Exponent,
                [IsFormalRootSum],
            function(frs)
                local res;
                if IsFormalRoot(frs) then
                    return DegreeRoot(frs);
                elif IsPermSumRep(frs) then
                    if frs = \$(0) then
                        return 1;
                    fi;
                    return Lcm(List(PermSum(frs),x->x[2])); 
                elif IsGaloisSumRep(frs) then
                    if Terms(frs) = [] then
                        return 1;
                    fi;
                    return Lcm(List(Terms(frs),x->DegreeRoot(x[2])));
                else
                    return Lcm(Exponent(GaloisPart(frs)),Exponent(PermPart(frs)));
                fi;
            end
);

####################################
##
## Methods
##
####################################

InstallMethod( LT,
                [IsFormalRoot,IsFormalRoot],
            function(a,b)
                return LT(Frac(a), Frac(b));
            end
);

InstallMethod( EQ,
                [IsFormalRoot,IsFormalRoot],
            function(a,b)
                return EQ(Frac(a), Frac(b));
            end
);

InstallMethod( \=,
                [IsFormalRootSum, IsFormalRootSum],
            function(a,b)
                if IsPermSumRep(a) and IsPermSumRep(b) then
                    return ForAll(PermSum(a-b),l->l[1]=0);
                elif IsGaloisSumRep(a) and IsGaloisSumRep(b) then
                    return ForAll(Terms(a-b),l->l[1]=0);
                else
                    return MixedSum(a - b) = MixedSum(\$(0));
                fi;
            end
);

InstallMethod( \-,
                [IsFormalRootSum, IsFormalRootSum],
            function(a,b)
                return a + (-b); 
            end
);
InstallMethod( \+,
                [IsFormalRootSum, IsFormalRootSum],
            function(a,b)
                local roots,aa,bb,res,list,m1,m2,coeff,term,x;
                list:= [];
                if IsPermSumRep(a) and IsPermSumRep(b) then
                    roots := Union(List(PermSum(a),t->t[2]),List(PermSum(b),t->t[2])); #all appearing roots
                    for x in roots do
                        coeff := 0;
                        term:=Filtered(PermSum(a),t->t[2]=x);
                        if not IsEmpty(term) then
                            coeff:=coeff + term[1][1];
                        fi;
                        term:=Filtered(PermSum(b),t->t[2]=x);
                        if not IsEmpty(term) then
                            coeff:=coeff + term[1][1];
                        fi;
                        if not IsZero(coeff) then
                            Add(list,[coeff,x]);
                        fi;
                    od;
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsPermSumRep), rec());
                    SetPermSum(res,list);
                    return res;
                elif IsGaloisSumRep(a) and IsGaloisSumRep(b) then
                    roots := Union(List(Terms(a),t->t[2]),List(Terms(b),t->t[2])); #all appearing terms
                    for x in roots do
                        coeff := 0;
                        term:=Filtered(Terms(a),t->t[2]=x);
                        if not IsEmpty(term) then
                            coeff:=coeff + term[1][1];
                        fi;
                        term:=Filtered(Terms(b),t->t[2]=x);
                        if not IsEmpty(term) then
                            coeff:=coeff + term[1][1];
                        fi;
                        if not IsZero(coeff) then
                            Add(list,[coeff,x]);
                        fi;
                    od;
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsGaloisSumRep), rec());
                    SetTerms(res,list);
                    return res;
     
                else
                    m1 := PermPart(a) + PermPart(b); 
                    m2 := MixedSum(GaloisPart(a) + GaloisPart(b)); #adding two galois reps could have a non trivial perm part
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsFormalRootSumRep), rec());
                    SetMixedSum(res, [m1 + m2[1], m2[2]]);
                    return res;
                fi;
                
            end
);

InstallMethod( \+,
                [IsFormalRootSum, IsRat],
            function(a,r)
                return a + r*\$(1);
            end
);

InstallMethod( \-,
                [IsFormalRootSum, IsRat],
            function(a,r)
                return a - r*\$(1);
            end
);

InstallMethod( \+,
                [IsRat, IsFormalRootSum],
            function(r,a)
                return a + r*\$(1);
            end
);

InstallMethod( \-,
                [IsRat, IsFormalRootSum],
            function(r,a)
                return r*\$(1) - a;
            end
);

InstallMethod( \*,
                [IsFormalRootSum, IsFormalRootSum],
            function(a,b)
                local res,x,y,f,m;
                if IsFormalRoot(a) and IsFormalRoot(b) then
                    f:=Frac(a)+Frac(b);
                    if IsFormalRootRep(a) and IsFormalRootRep(b) then
                        res := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootRep), rec());
                    else
                        res := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootGalRep), rec());
                    fi;
                    SetFrac(res, f);
                    return Reduce(res);
                else
                    if IsPermSumRep(a) and IsPermSumRep(b) then
                        res := \$(0);
                        for x in PermSum(a) do
                            for y in PermSum(b) do
                                res := res + (x[1] * y[1] * Gcd(x[2],y[2])) * \$( Lcm(x[2],y[2]) ); #this is a general rule for ~k
                            od;
                        od;
                        return res;
                    elif IsGaloisSumRep(a) or IsGaloisSumRep(b) then
                        res := RegularGalois(0);
                        for x in Terms(a) do
                            for y in Terms(b) do
                                res := res + (x[1]*y[1])*(x[2]*y[2]);
                            od;
                        od;
                        return res;
                    else
                        x := GaloisPart(a) * GaloisPart(b);
                        if not GaloisPart(a) = RegularGalois(0) then
                            x := x + GaloisPart(a) * PermPart(b);
                        fi;
                        if not GaloisPart(b) = RegularGalois(0) then
                            x := x + GaloisPart(b) * PermPart(a);
                        fi;
                        m := MixedSum(x);
                        res := (PermPart(a) * PermPart(b) + m[1]) + m[2];
                        return res;
                    fi;
                fi;
            end
);

InstallMethod( \*,
                [IsRat, IsFormalRootSum],
            function(n,frs)
                local res,ms;
                if IsPermSumRep(frs) then
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsPermSumRep), rec());
                    SetPermSum(res,List(PermSum(frs),x->[n*x[1],x[2]]));
                elif IsGaloisSumRep(frs) then
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsGaloisSumRep), rec());
                    SetTerms(res,List(Terms(frs),x->[n*x[1],x[2]]));
                else
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsFormalRootSumRep), rec());
                    ms := MixedSum(frs);
                    SetMixedSum(res, [n * ms[1], n * ms[2]]);
                fi;
                return res;
            end
);

InstallMethod( \*,
                [IsFormalRootSum,IsRat],
            function(frs,n)
                return n*frs;
            end
);

InstallMethod( \^,
                [IsFormalRootSum, IsInt],
            function(a,n)
                local temp;
                if n = 1 then #square and multiply
                    return a;
                elif n = 0 then
                    return \$(1);
                elif n < 0 then
                    Error("The ring of formal root sums is not a field");
                fi;
                if n mod 2 = 1 then
                    return a * (a^(n-1));
                else
                    temp := a^(n/2);
                    return temp*temp;
                fi;
            end
);


InstallMethod( \^,
                [IsFormalRootSum, IsZmodnZObj],
            function(a,n)
                local e,s;
                e := Exponent(a);
                s := Size(DefaultRing(n));
                if s mod e = 0 then
                    return Power(a, Int(n));
                else
                    Error("Degree and ideal of ZmodnZObj don't match.");
                fi;
            end
);

InstallMethod( \^,
                [IsFormalRootSum, IS_FFE],
            function(a,n)
                local e,s;
                e := Exponent(a);
                s := Size(DefaultRing(n));
                if s mod e = 0 then
                    return Power(a, Int(n));
                else
                    Error("Degree and ideal of FFE don't match.");
                fi;
            end
);

InstallMethod( Power,
                [IsFormalRootSum, IsRat],
            function(a, i)
                local res,f,t,x,m;
                if IsFormalRoot(a) then
                    f := Frac(a)*i;
                    if IsFormalRootRep(a) then
                        res := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootRep), rec());
                    else
                        res := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootGalRep), rec());
                    fi;
                    SetFrac(res, f);
                    return Reduce(res);
                else
                    if IsPermSumRep(a) then
                        res := \$(0);
                        if IsInt(i) then
                            for t in PermSum(a) do
                                res := res + (t[1] * Gcd(i,t[2])) * \$(t[2]/Gcd(i,t[2])); #general rule. See bachelor thesis
                            od;
                        else
                            for t in Terms(a) do
                                res := res + t[1]*Power(t[2],i);
                            od;
                        fi;
                    elif IsGaloisSumRep(a) then
                        res := RegularGalois(0);
                        for t in Terms(a) do
                            res := res + t[1] * Power(t[2],i);
                        od;
                    else
                        x := Power(GaloisPart(a),i);
                        m := MixedSum(x);
                        res := (Power(PermPart(a),i) + m[1]) + m[2];
                    fi;
                    return res;
                fi;
            end
);


InstallMethod( Reduce,
              [IsFormalRoot],
          function(fr)
              local res,frac;
              frac := Frac(fr);
              if IsFormalRootRep(fr) then
                  res := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootRep), rec());
              else
                  res := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootGalRep), rec());
              fi;
              SetFrac(res, (NumeratorRat(frac) mod DenominatorRat(frac)) / DenominatorRat(frac));
              return res;
          end
);

InstallMethod( Conjugate,
            [IsFormalRootSum],
        function(frs)
            return Power(frs,-1);
        end
);

InstallMethod( Specialize,
                [IsFormalRootSum, IsRat],
            function(frs, i)
                local terms,res,t,f,p,q;
                if i = 1 then
                    if HasPermSum(frs) then
                        terms := PermSum(frs);
                        f := Filtered(terms,x->x[2]=1);
                        if f = [] then
                            return 0;
                        else
                            return f[1][1];
                        fi;
                    elif HasMixedSum(frs) then
                        res := Specialize(PermPart(frs),i) + Specialize(GaloisPart(frs),i);
                    else
                        terms := Terms(frs);
                        res := 0;
                        for t in terms do
                            f := Frac(t[2]);
                            p := NumeratorRat(f);
                            q := DenominatorRat(f);
                            res := res + t[1] * E(q)^p; #Substitute FormalRoot(p,q) by E(q)^(p*i)
                        od;
                    fi;
                else
                    return Specialize(Power(frs,i),1);
                fi;
                return res;
            end
);

####################################
##
## Constructors
##
####################################

InstallMethod( FormalRootSum,
                "copy constructor",
                [ IsFormalRootSum ],

 IdFunc

);

InstallMethod(FormalRoot,
                [IsCyc],
            function(c)
                local r,n,i,formal;
                if Order(c) = infinity then
                    Error("<c> must have finite order.");
                fi;
                n := Order(c);
                r := E(n);
                for i in [1..n] do      #this is not the most elegant solution
                    if r^i = c then
                        return FormalRoot(i,n);
                    fi;
                od;
            end
);

InstallMethod(FormalRootByRat,
                [IsRat],
            function(r)
                local formal;
                formal := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootRep), rec());
                SetFrac(formal, r);
                return Reduce(formal);
            end
);

InstallMethod(FormalRootGal,
                [IsInt,IsInt],
            function(p,n)
                local formal;
                formal := Objectify( NewType( TheFamilyOfFormalRoots, IsFormalRootGalRep), rec()); #uses the GaloisRep. Is only for internal usage
                SetFrac(formal, p/n);
                return Reduce(formal);
            end
);

InstallMethod( \$,
                "returns ~n",
                [IsInt],
                function(n)
                    local res;
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsPermSumRep), rec());
                    if n = 0 then
                        SetPermSum(res, []);
                    else
                        SetPermSum(res, [[1,n]]); #returns ~n
                    fi;
                    return res;
                end
);

InstallMethod( \/,
                "returns /n",
                [IsInt],
                function(n)
                    local res;
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsFormalRootSumRep), rec());
                    if n = 0 then
                        SetTerms(res, []);
                    else
                        SetGaloisSum(res, [[1,FormalRoot(1,n),'/']]); #returns /n
                    fi;
                    return res;
                end
);

InstallMethod( RegularGalois,
                "returns /n",
                [IsInt],
                function(n)
                    local res;
                    res := Objectify( NewType( TheFamilyOfFormalRootSums, IsGaloisSumRep), rec()); #uses the galois rep. Again, just for internal usage
                    if n = 0 then
                        SetTerms(res, []);
                    else
                        SetGaloisSum(res, [[1,FormalRootGal(1,n),'/']]);
                    fi;
                    return res;
                end
);

InstallMethod( FormalRoot,
                "Creates a FormalRoot Object by a pair of Integers",
                [ IsInt, IsInt ],
                
        function( p, n )
                if n <= 0 then
                    Error("You will not want me to create a nonpositive root of unity.");
                fi;
                return FormalRootByRat(p/n);

        end
);

InstallMethod( Monomial,
                [IsInt, IsFormalRoot],
            function(n, a)
                return \$(n) * Power(a,1/n);
            end
);

####################################
##
## Display Methods
##
####################################

InstallMethod( String,
                "for FormalRoots",
                [IsFormalRoot],
            function(fr)
                local res,frac;
                res := "";
                frac := Frac(fr);
                Add(res, '\(');
                Append(res,String(frac));
                Add(res,')' );
                return res;
            end
);

InstallMethod( String,
                "for FormalRootSum",
                [IsFormalRootSum],
                function(frs)
                    if IsPermSumRep(frs) then
                        return PermString(frs);
                    elif IsGaloisSumRep(frs) then
                        return GaloisString(frs);
                    else
                        return MixedString(frs);
                    fi;
                end
);
                
InstallMethod( ViewObj,
                "for FormalRootSums",
                [ IsFormalRootSum ],
                        function(frs)
                            local res;
                            if IsPermSumRep(frs) then
                                res := PermString(frs);
                            elif IsGaloisSumRep(frs) then
                                res := GaloisString(frs);
                            else
                                res := MixedString(frs);
                            fi;
                            Print(res);
                        end
);

InstallMethod(Display,
                [IsFormalRootSum],
                ViewObj
);

