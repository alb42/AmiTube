#!/bin/bash -e

flexcat locale/AmiTube.cd AmiTubelocale.pas=locale/FPCUnit.sd
# languages
flexcat locale/AmiTube.cd locale/deutsch.ct NEWCTFILE locale/deutsch.ct
flexcat locale/AmiTube.cd locale/deutsch.ct CATALOG Catalogs/deutsch/AmiTube.catalog

# copy locales to package
cp locale/AmiTube.cd pack/AmiTube/Catalogs
cp -r Catalogs/* pack/AmiTube/Catalogs/
# 
cp locale/AmiTube.cd pack/AmiTubeAROS/Catalogs
cp -r Catalogs/* pack/AmiTubeAROS/Catalogs/
#
cp locale/AmiTube.cd pack/AmiTubeMorphOS/Catalogs
cp -r Catalogs/* pack/AmiTubeMorphOS/Catalogs/
#
cp locale/AmiTube.cd pack/AmiTubeOS4/Catalogs
cp -r Catalogs/* pack/AmiTubeOS4/Catalogs/

#
mkdir -p lib/m68k-amiga
rm -f ./lib/m68k-amiga/*
fpc4amiga000.sh -B -XX CX -FU./lib/m68k-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTube/AmiTube.000

rm -f ./lib/m68k-amiga/*
fpc4amiga.sh -B -XX -CX -FU./lib/m68k-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTube/AmiTube

mkdir -p lib/i386-aros
rm -f ./lib/i386-aros/*
fpc4aros.sh -B -O2 -XX -CX -FU./lib/i386-aros -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTubeAROS/AmiTube.i386

mkdir -p lib/arm-aros
rm -f ./lib/arm-aros/*
fpc4arosarm.sh -B -O2 -XX -CX -FU./lib/arm-aros -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTubeAROS/AmiTube.arm

mkdir -p lib/x86_64-aros
rm -f ./lib/x86_64-aros/*
fpc4aros64.sh -B -O2 -XX -CX -FU./lib/x86_64-aros -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTubeAROS/AmiTube.x64

mkdir -p lib/powerpc-morphos
rm -f ./lib/powerpc-morphos/*
fpc4mos.sh -B -O2 -XX -CX -FU./lib/powerpc-morphos -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTubeMorphOS/AmiTube

mkdir -p lib/powerpc-amiga
rm -f ./lib/powerpc-amiga/*
fpc4os4.sh -B -O2 -Xs -XX -CX -FU./lib/powerpc-amiga -Fu../MUIClass/src AmiTube.pas -o./pack/AmiTubeOS4/AmiTube
powerpc-amiga-strip ./pack/AmiTubeOS4/AmiTube

cd pack
./packme.sh
cd ..