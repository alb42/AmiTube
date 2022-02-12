#!/bin/bash
rm -f AmiTube.lha
rm -f AmiTubeAROS.lha
rm -f *.lha.uaem
rm -f AmiTube/*.uaem
rm -f AmiTube/*.ini
rm -f AmiTubeAROS/*.uaem
rm -rf AmiTube/Icons
rm -rf AmiTubeAROS/Icons

cp ../AmiTube.guide AmiTube/AmiTube.guide
cp -ar ../Icons AmiTube/

cp ../AmiTube.guide AmiTubeAROS/AmiTube.guide
cp -ar ../Icons AmiTubeAROS/

#lha ao5 AmiTube.lha AmiTube AmiTube.info
#lha ao5 AmiTubeAROS.lha AmiTubeAROS AmiTubeAROS.info
