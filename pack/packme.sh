#!/bin/bash
rm -f AmiTube.lha
rm -f AmiTubeAROS.lha
rm -f AmiTubeMorphOS.lha
rm -f AmiTubeOS4.lha
rm -f *.lha.uaem
#
rm -f AmiTube/*.uaem
rm -f AmiTube/*.ini
rm -rf AmiTube/Icons
cp ../AmiTube.guide AmiTube/AmiTube.guide
cp -ar ../Icons AmiTube/
#
rm -f AmiTubeAROS/*.uaem
rm -f AmiTubeAROS/*.ini
rm -rf AmiTubeAROS/Icons
cp ../AmiTube.guide AmiTubeAROS/AmiTube.guide
cp -ar ../Icons AmiTubeAROS/
#
rm -f AmiTubeMorphOS/*.uaem
rm -f AmiTubeMorphOS/*.ini
rm -rf AmiTubeMorphOS/Icons
cp ../AmiTube.guide AmiTubeMorphOS/AmiTube.guide
cp -ar ../Icons AmiTubeMorphOS/
#
rm -f AmiTubeOS4/*.uaem
rm -f AmiTubeOS4/*.ini
rm -rf AmiTubeOS4/Icons
cp ../AmiTube.guide AmiTubeOS4/AmiTube.guide
cp -ar ../Icons AmiTubeOS4/

echo "packme.sh done"

#lha ao5 AmiTube.lha AmiTube AmiTube.info
#lha ao5 AmiTubeAROS.lha AmiTubeAROS AmiTubeAROS.info
