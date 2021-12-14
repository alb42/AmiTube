#!/bin/bash
rm -f AmiTube.lha
rm -f AmiTube/*.uaem

lha ao5 AmiTube.lha AmiTube AmiTube.info
