#!/bin/bash -e

flexcat locale/AmiTube.cd Catalogs/deutsch.ct NEWCTFILE Catalogs/deutsch.ct
flexcat locale/AmiTube.cd Catalogs/deutsch.ct CATALOG Catalogs/deutsch/AmiTube.catalog
flexcat locale/AmiTube.cd AmiTubelocale.pas=locale/FPCUnit.sd

cp locale/AmiTube.cd pack/AmiTube/Catalogs
cp Catalogs/deutsch/AmiTube.catalog pack/AmiTube/Catalogs/deutsch

rm -f ./lib/m68k-amiga/*
fpc4amiga000.sh -B -FU./lib/m68k-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTube/AmiTube.000

rm -f ./lib/m68k-amiga/*
fpc4amiga.sh -B -FU./lib/m68k-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTube/AmiTube

