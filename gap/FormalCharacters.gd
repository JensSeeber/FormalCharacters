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

#The strange comments you find here are AutoDoc comments. They are used for
#documentation. See https://github.com/homalg-project/AutoDoc.


#! @Chapter Formal Characters
#! @Section Categories
#! The categories describing formal character tables and formal characters.

#! @Description The category of formal character tables.
#! @Arguments fct
DeclareCategory("IsFormalCharacterTable",
                IsObject );

#! @Description The category of formal characters.
#! @Arguments fc
DeclareCategory("IsFormalCharacter",
                IsObject );
                
####################################
##
## Attributes
##
####################################
#! @EndSection
#! @Section Attributes
#! This section describes attributes, that are saved about a formal character (table).

#! @Description Computes the irreducible formal characters of formal character table.
#! @Arguments fct
#! @InsertChunk IrrEx
DeclareAttribute( "Irr",
                IsFormalCharacterTable );

#! @Description Computes formal rational characters of the given formal character table.
#! @Arguments fct
#! @InsertChunk RationalCharactersEx
DeclareAttribute( "RationalCharacters",
                IsFormalCharacterTable );

#! @Returns the character table, the formal character table is built upon.
#! @Arguments fct
DeclareAttribute( "UnderlyingCharacterTable",
                IsFormalCharacterTable );

#! @Returns the exponent of the group.
#! @Arguments fct
DeclareAttribute( "Exponent",
                IsFormalCharacterTable );

#! @Returns the underlying group if known.
#! @Arguments fct
DeclareAttribute( "UnderlyingGroup",
                IsFormalCharacterTable );

#! @Returns the underlying group if known.
#! @Arguments fc
DeclareAttribute( "UnderlyingGroup",
                IsFormalCharacter);

#! @Returns the formal character converted into a class function.
#! @Arguments fc
DeclareAttribute( "UnderlyingClassFunction",
                IsFormalCharacter);

#! @Returns the list of values of fc.
#! @Arguments fc
DeclareAttribute( "UnderlyingList",
                IsFormalCharacter);

#! @Returns the formal character table, that fc belongs to.
#! @Arguments fc
DeclareAttribute( "UnderlyingFCT",
                IsFormalCharacter);

#! @Returns the conjugacy classes of the underlying group - if known.
#! @Arguments fct
DeclareAttribute( "ConjugacyClasses",
                IsFormalCharacterTable);

#! @Returns the indicator of fc
#! @Arguments fc
DeclareAttribute( "Indicator",
                IsFormalCharacter);

#! @Returns the sum of all indicators of Irr(fct).
#! @Arguments fct
DeclareAttribute( "GroupIndicator",
                IsFormalCharacterTable);

####################################
##
## Methods
##
####################################

#! @EndSection
#! @Section Methods
#! This section describes methods applicable to formal characters and formal character tables.

#! @Returns the value at the $i$-th class.
#! @Arguments fc, i
DeclareOperation( "\[\]",
                [IsFormalCharacter, IsInt]);

#! @Returns the scalar product of $\chi$ and $\psi$.
#! @Arguments \chi, \psi
DeclareOperation( "ScalarProduct",
                [IsFormalCharacter, IsFormalCharacter]);

#! @Returns the virtual character obtained by applying Specialize(r,i) to every value $r$.
#! @Arguments fc, i
DeclareOperation( "Specialize",
                [IsFormalCharacter, IsInt]);

#! @Returns the evaluation of fc at the group element g
#! @Arguments g, fc
DeclareOperation( "^",
                [IsMultiplicativeElement, IsFormalCharacter]);

#! @Description Addition of formal characters.
#! @Arguments a b
DeclareOperation( "+",
                [IsFormalCharacter, IsFormalCharacter]);

#! @Description Subtraction of formal characters.
#! @Arguments a b
DeclareOperation( "-",
                [IsFormalCharacter, IsFormalCharacter]);

#! @Description Multiplication of formal characters.
#! @Arguments a b
DeclareOperation( "*",
                [IsFormalCharacter, IsFormalCharacter]);

#! @Description Multiplies every value of fc by r.
#! @Arguments r,fc
DeclareOperation( "*",
                [IsFormalRootSum, IsFormalCharacter]);

#! @Description Multiplies every value of fc by r.
#! @Arguments r,fc
DeclareOperation( "*",
                [IsRat, IsFormalCharacter]);

#! @Description Multiplies every value of fc by r.
#! @Arguments fc,r
DeclareOperation( "*",
                [IsFormalCharacter, IsRat]);

#! @Description Takes the $i$-th power of fc.
#! @Arguments fc, i
DeclareOperation( "^",
                [IsFormalCharacter, IsInt]);

#! @Description Applies Power(r,i) to every value r of fc
#! @Arguments fc, i
DeclareOperation( "Power",
                [IsFormalCharacter, IsInt]);

#! @Description Applies r^i to every value r of fc
#! @Arguments fc, i
DeclareOperation( "^",
                [IsFormalCharacter, IsFFE]);

#! @Description Applies r^i to every value r of fc
#! @Arguments fc, i
DeclareOperation( "^",
                [IsFormalCharacter, IsZmodnZObj]);

#! @Description Applies conjugation to every value of fc
#! @Arguments fc, i
DeclareOperation( "Conjugate",
                [IsFormalCharacter]);

#! @Returns Virtual character $\chi(g^n)$
#! @Arguments \chi, n
DeclareOperation( "VirtualPowerCharacter",
                [IsCharacter, IsInt]);

#! @Returns the character corresponding to the Representation on $M \odot M$, where $\chi$ is the character on $M$.
#! @Arguments \chi
DeclareOperation( "SymCharacter",
                [IsCharacter]);

#! @Returns the character corresponding to the Representation on $M \wedge M$, where $\chi$ is the character on $M$.
#! @Arguments \chi
DeclareOperation( "AltCharacter",
                [IsCharacter]);

#! @Description This function aims to calculate the cycle counting polynomial of a formal permutation character, but it can be used
#! to do a lot more. $\phi$ has to be a function that takes an Integer as it's argument and returns anything in any ring.
#! If fc is a rational formal character, then we define a class function c by $c(g) = \prod_{i} \phi(i)^{a_i}$, where $fc(g) = \sum_{i} a_i \widetilde{i}$.
#! Then the polya substitution is $\frac{1}{|G|} \sum_{g \in G} c(g)$.
#! @Arguments fc, \phi
#! @InsertChunk PolyaSubstitutionEx
DeclareOperation( "PolyaSubstitution",
                [IsFormalCharacter, IsFunction]);
####################################
##
## Constructors
##
####################################

#! @EndSection
#! @Section Constructors
#! Methods for construction of formal characters and formal character tables.

DeclareOperation( "FormalCharacterTable",
                [ IsFormalCharacterTable ] );
                
DeclareOperation( "FormalCharacter",
                [ IsFormalCharacter ] );

#! @Description Constructs the formal character table with underlying character table ct.
#! This takes nearly no time, because no calculation is done yet.
#! @Arguments ct
DeclareOperation( "FormalCharacterTable",
                [ IsCharacterTable ] );

#! @Description Constructs the formal character out of the ordinary character c.
#! @Arguments c
DeclareOperation( "FormalCharacter",
                [ IsClassFunction ]);

#! @Description Constructs the formal character out of the ordinary character c.
#! Sets the formal character table of the result to fct.
#! @Arguments fct, c
DeclareOperation( "FormalCharacter",
                [ IsFormalCharacterTable, IsClassFunction ]);

#! @Description Constructs the formal character, that has list of values L.
#! @Arguments fct, L
DeclareOperation( "FormalCharacterByValues",
                [ IsFormalCharacterTable, IsList ]);

#! @Description Method for pretty printing a matrix.
DeclareOperation( "PrintMat",
                [IsList]);

#! @EndSection
