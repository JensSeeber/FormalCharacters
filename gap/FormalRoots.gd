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

#! @Chapter Formal Roots
#! @Section Categories
#! The categories for formal roots and formal root sums.

#! @Description
#! The category of formal root sums.
#! @Arguments frs
DeclareCategory("IsFormalRootSum",
                IsObject);

#! @Description
#! The category of formal roots.
#! @Arguments fr
DeclareCategory("IsFormalRoot",
                IsFormalRootSum );

####################################
##
## Attributes
##
####################################

#! @EndSection
#! @Section Attributes
#! Here are the attributes a formal root or formal root sum can have.

#! @Returns underlying fraction of a formal root.
#! @Arguments fr
DeclareAttribute( "Frac",
                IsFormalRoot );

#! @Returns the degree of a formal root.
#! @Arguments fr
DeclareAttribute( "DegreeRoot",
                IsFormalRoot );

#! @Returns the list of terms of a formal root sum.
#! @Arguments frs
DeclareAttribute( "Terms",
                IsFormalRootSum);

#! @Returns the additive Inverse of a formal root sum.
#! @Arguments frs
DeclareAttribute( "AdditiveInverseAttr",
                IsFormalRootSum);

#! @Returns the empty formal root sum.
#! @Arguments frs
DeclareAttribute( "Zero",
                IsFormalRootSum);

DeclareOperation( "ZeroMutable",
                [IsFormalRootSum]);

#! @Returns the list of terms of a formal root sum sorted by degree.
#! @Arguments frs
DeclareAttribute( "ByDegree",
                IsFormalRootSum);

#! @Returns the galois sum of frs as a list of tuples, where [a,r,g] represents $a \sum_{x \in r^g} x$ and g is a list of generators of the group acting on r.
#! @Description Mostly for internal usage
#! @Arguments frs
DeclareAttribute( "GaloisSum",
                IsFormalRootSum);

#! @Returns the permutation sum of frs as a list of tuples, where [a, n] represents $a \widetilde{n}$.
#! @Arguments frs
DeclareAttribute( "PermSum",
                IsFormalRootSum);

#! @Returns the string used for printing most formal root sums.
#! @Arguments frs
DeclareAttribute( "MixedString",
                IsFormalRootSum);

#! @Returns a tuple with two entries. The first
#! is guaranteed to be in permutation representation, the second is guaranteed to be in galois representation.
#! @Arguments frs
DeclareAttribute( "MixedSum",
                IsFormalRootSum);

#! @Returns the string used for printing, if frs is a GaloisSumRep.
#! @Arguments frs
DeclareAttribute( "GaloisString",
                IsFormalRootSum);

#! @Returns the string used for printing, if frs is a PermSumRep.
#! @Arguments frs
DeclareAttribute( "PermString",
                IsFormalRootSum);

#! @Returns the permutation part of frs as a formal root sum. Is guaranteed to be in PermSumRep.
#! @Arguments frs
DeclareAttribute( "PermPart",
                IsFormalRootSum);
                
#! @Returns the galois part of frs as a formal root sum. Is guaranteed to be in GaloisSumRep.
#! @Arguments frs
DeclareAttribute( "GaloisPart",
                IsFormalRootSum);
                
#! @Returns the exponent of a formal root sum.
#! @Arguments frs
DeclareAttribute( "Exponent",
                IsFormalRootSum);

####################################
##
## Methods
##
####################################

#! @EndSection
#! @Section Methods
#! This section provides methods for manipulation of formal root sums.

#! @Returns the sum of two formal root sums.
#! @Arguments a, b
DeclareOperation( "\+",
                [IsFormalRootSum, IsFormalRootSum]);

#! @Returns the sum of two formal root sums, where b is interpreted as a formal root sum first.
#! @Arguments a, b
DeclareOperation( "\+",
                [IsFormalRootSum, IsRat]);

#! @Returns the sum of two formal root sums, where a is interpreted as a formal root sum first.
#! @Arguments a, b
DeclareOperation( "\+",
                [IsRat, IsFormalRootSum]);

#! @Returns the difference of two formal root sums.
#! @Arguments a, b
DeclareOperation( "\-",
                [IsFormalRootSum,IsFormalRootSum]);

#! @Returns the difference of two formal root sums, where a is interpreted as a formal root sum first.
#! @Arguments a, b
DeclareOperation( "\-",
                [IsRat, IsFormalRootSum]);

#! @Returns the difference of two formal root sums, where b is interpreted as a formal root sum first.
#! @Arguments a, b
DeclareOperation( "\-",
                [IsFormalRootSum,IsRat]);

#! @Returns the product of two formal roots.
#! @Arguments a, b
DeclareOperation( "\*",
                [IsFormalRoot, IsFormalRoot] );

#! @Returns the scalar product of a rational number and a formal root sum.
#! @Arguments r, a
DeclareOperation( "\*",
                [IsRat, IsFormalRootSum] );

#! @Returns the scalar product of a rational number and a formal root sum.
#! @Arguments a, r
DeclareOperation( "\*",
                [IsFormalRootSum,IsRat] );

#! @Returns the product of two formal root sums by distributivity.
#! @Arguments a, r
DeclareOperation( "\*",
                [IsFormalRootSum,IsFormalRootSum] );

#! @Returns the n-th power of the formal root sum a.
#! @Arguments a, n
DeclareOperation( "\^",
                [IsFormalRootSum, IsInt]);

#! @Returns the formal root sum generated by applying the morphism in g to a.
#! @Arguments a, g
DeclareOperation( "\^",
                [IsFormalRootSum, IsZmodnZObj]);

#! @Description Does the same as the function before. It is just needed, because gap regards a finite field element different from ZmodnZObj.
#! @Returns the formal root sum generated by applying the morphism in g to a.
#! @Arguments a, g
DeclareOperation( "\^",
                [IsFormalRootSum, IS_FFE]);

#! @Description Reduces a formal root by just normalizing the underlying fraction.
#! @Arguments fr
DeclareOperation( "Reduce",
                [IsFormalRoot]);

#! @Returns the algebraic conjugate for the formal root sum. This is just inversion for the formal roots extended by linearity.
#! @Arguments frs
DeclareOperation( "Conjugate",
                [IsFormalRootSum]);

#! @Returns the formal root sum, that is created by substituting the n-th root of unity (1/n) in frs by $E(n)^i$.
#! @Arguments frs,i
DeclareOperation( "Specialize",
                [IsFormalRootSum, IsRat]);

#! @Returns the formal root sum, that is created by substituting the n-th root of unity (1/n) in frs by the formal root (i/n).
#! @Arguments frs,i
DeclareOperation( "Power",
                [IsFormalRootSum, IsRat]);

####################################
##
## Constructors
##
####################################

#! @EndSection
#! @Section Constructors
#! The following methods can be used to construct a formal root or formal root sum.

DeclareOperation( "FormalRoot",
                [ IsFormalRoot ] );
                
DeclareOperation( "FormalRootSum",
                [ IsFormalRootSum ] );

#! @Description converts an element of finite order into a formal root
#! @Returns IsFormalRoot
#! @Arguments fr
DeclareOperation( "FormalRoot",
                [ IsCyc]);

#! @Returns the formal root, that represents the root of unity $e^{2 \pi i a}$.
#! @Arguments a
DeclareOperation( "FormalRootByRat",
                [ IsRat ]);

#! @InsertChunk FormalRootEx
#! @Returns the formal root, that represents the root of unity $e^{\frac{2 \pi i p}{q}}$.
#! @Arguments p,q
DeclareOperation( "FormalRoot",
                [ IsInt, IsInt ] );

#! @Description If you use this constructor, Gap will use the FormalRootGal representation, which Gap will NEVER try to convert into a perm representation. This is usually not recommended.
#! @Returns the formal root, that represents the root of unity $e^{\frac{2 \pi i p}{q}}$.
#! @Arguments p,q
DeclareOperation( "FormalRootGal",
                [ IsInt, IsInt ] );

#! @Returns ~n := $\sum_{i=1}^{n} FormalRoot(i,n)$.
#! @Arguments n
DeclareOperation( "\$",
                [IsInt]);

#! @Returns /n := $\sum_{i=1, gcd(i,n) = 1}^{n} FormalRoot(i,n)$.
#! @Arguments n
DeclareOperation( "\/",
                [IsInt]);

#! @Description If you use this constructor, Gap will use the FormalRootGal representation, which Gap will NEVER try to convert into a perm representation. This is usually not recommended.
#! @Returns /n := $\sum_{i=1, gcd(i,n) = 1}^{n} FormalRoot(i,n)$.
#! @Arguments n
DeclareOperation( "RegularGalois",
                [IsInt]);

#! @Returns the formal trace of the monomial matrix, which has an n-cycle as underlying permutation and the upper right entry is changed to a.
#! @Arguments n,a
DeclareOperation( "Monomial",
                [IsInt, IsFormalRoot]);

#! @EndSection
