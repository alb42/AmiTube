#!/bin/bash
rm -f AmiTube.lha
rm -f AmiTubeAROS.lha
rm -f AmiTube/*.uaem

lha ao5 AmiTube.lha AmiTube AmiTube.info
lha ao5 AmiTubeAROS.lha AmiTubeAROS AmiTubeAROS.info
