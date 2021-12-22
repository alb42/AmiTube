#!/bin/bash -e

flexcat locale/AmiTube.cd AmiTubelocale.pas=locale/FPCUnit.sd
# languages
flexcat locale/AmiTube.cd locale/deutsch.ct NEWCTFILE locale/deutsch.ct
flexcat locale/AmiTube.cd locale/deutsch.ct CATALOG Catalogs/deutsch/AmiTube.catalog

# copy locales to package
cp locale/AmiTube.cd pack/AmiTube/Catalogs
cp -r Catalogs/* pack/AmiTube/Catalogs/

rm -f ./lib/m68k-amiga/*
fpc4amiga000.sh -B -FU./lib/m68k-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTube/AmiTube.000

rm -f ./lib/m68k-amiga/*
fpc4amiga.sh -B -FU./lib/m68k-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTube/AmiTube

