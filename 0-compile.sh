#!/bin/bash -e

flexcat locale/AmiTube.cd AmiTubelocale.pas=locale/FPCUnit.sd
# languages
flexcat locale/AmiTube.cd locale/deutsch.ct NEWCTFILE locale/deutsch.ct
flexcat locale/AmiTube.cd locale/deutsch.ct CATALOG Catalogs/deutsch/AmiTube.catalog

# copy locales to package
cp locale/AmiTube.cd pack/AmiTube/Catalogs
cp -r Catalogs/* pack/AmiTube/Catalogs/

# copy locales to package
cp locale/AmiTube.cd pack/AmiTubeAROS/Catalogs
cp -r Catalogs/* pack/AmiTubeAROS/Catalogs/

rm -f ./lib/m68k-amiga/*
fpc4amiga000.sh -B -FU./lib/m68k-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTube/AmiTube.000

rm -f ./lib/m68k-amiga/*
fpc4amiga.sh -B -FU./lib/m68k-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTube/AmiTube

mkdir -p lib/i386-aros
rm -f ./lib/i386-aros/*
fpc4aros.sh -B -FU./lib/i386-aros -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTubeAROS/AmiTube.i386

mkdir -p lib/arm-aros
rm -f ./lib/arm-aros/*
fpc4arosarm.sh -B -FU./lib/arm-aros -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTubeAROS/AmiTube.arm



