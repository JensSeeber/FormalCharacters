#
# FormalCharacters: This package blabla
#
# This file contains package meta data.
#
SetPackageInfo( rec(

PackageName := "FormalCharacters",
Subtitle := "This package provides functionality for computing and manipulating formal characters.",
Version := "1.0",
Date := "26/09/2014", # dd/mm/yyyy format

Persons := [
  rec(
    IsAuthor := true,
    IsMaintainer := true,
    FirstNames := "Jens",
    LastName := "Seeber",
    WWWHome := "https://github.com/JensSeeber",
    Email := "jens.seeber@rwth-aachen.de",
    #PostalAddress := "",
    #Place := "",
    Institution := "RWTH Aachen",
  ),
],

PackageWWWHome := "https://github.com/JensSeeber/FormalCharacters",

ArchiveURL     := Concatenation( ~.PackageWWWHome, "FormalCharacters-", ~.Version ),
README_URL     := Concatenation( ~.PackageWWWHome, "README" ),
PackageInfoURL := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),

ArchiveFormats := ".tar.gz",

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "submitted"     for packages submitted for the refereeing
##    "deposited"     for packages for which the GAP developers agreed
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages
##    "other"         for all other packages
##
Status := "dev",

AbstractHTML :=  "",

PackageDoc := rec(
  BookName  := "FormalCharacters",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "A package for computation and manipulation of formal characters.",
),

Dependencies := rec(
  GAP := ">= 4.6",
  NeededOtherPackages := [ [ "GAPDoc", ">= 1.5" ] ],
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ],
),

AvailabilityTest := function()
        return true;
    end,

TestFile := "tst/testall.g",

#Keywords := [ "TODO" ],

));


