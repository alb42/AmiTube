#!/bin/bash
rm -f AmiTube.lha
rm -f AmiTubeAROS.lha
rm -f AmiTube/*.uaem
rm -f AmiTube/*.ini
rm -f AmiTubeAROS/*.uaem

cp AmiTube.Readme AmiTube/AmiTube.Readme
cp AmiTube.Readme AmiTubeAROS/AmiTube.Readme

#lha ao5 AmiTube.lha AmiTube AmiTube.info
#lha ao5 AmiTubeAROS.lha AmiTubeAROS AmiTubeAROS.info
