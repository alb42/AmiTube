#!/bin/bash
rm -f AmiTube.lha
rm -f AmiTubeAROS.lha
rm -f AmiTubeMorphOS.lha
rm -f AmiTubeOS4.lha
rm -f *.lha.uaem
rm -f ../rexx/*.uaem
#
rm -f AmiTube/*.uaem
rm -f AmiTube/*.ini
rm -rf AmiTube/Icons
rm -rf AmiTube/rexx
cp ../AmiTube.guide AmiTube/AmiTube.guide
cp -ar ../Icons AmiTube/
cp -ar ../rexx AmiTube/
#
rm -f AmiTubeAROS/*.uaem
rm -f AmiTubeAROS/*.ini
rm -rf AmiTubeAROS/Icons
rm -rf AmiTubeAROS/rexx
cp ../AmiTube.guide AmiTubeAROS/AmiTube.guide
cp -ar ../Icons AmiTubeAROS/
cp -ar ../rexx AmiTubeAROS/
#
rm -f AmiTubeMorphOS/*.uaem
rm -f AmiTubeMorphOS/*.ini
rm -rf AmiTubeMorphOS/Icons
rm -rf AmiTubeMorphOS/rexx
cp ../AmiTube.guide AmiTubeMorphOS/AmiTube.guide
cp -ar ../Icons AmiTubeMorphOS/
cp -ar ../rexx AmiTubeMorphOS/
#
rm -f AmiTubeOS4/*.uaem
rm -f AmiTubeOS4/*.ini
rm -rf AmiTubeOS4/Icons
rm -rf AmiTubeOS4/rexx
cp ../AmiTube.guide AmiTubeOS4/AmiTube.guide
cp -ar ../Icons AmiTubeOS4/
cp -ar ../rexx AmiTubeOS4/

echo "packme.sh done"

#lha ao5 AmiTube.lha AmiTube AmiTube.info
#lha ao5 AmiTubeAROS.lha AmiTubeAROS AmiTubeAROS.info
