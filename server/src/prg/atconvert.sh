#!/bin/bash

cd /var/www/html/cdxlstore

cwdir=$(pwd);
tdir=$2
tdir=$tdir"temp"

format=$3

echo "makedir $tdir"
mkdir -p "$tdir"

cd "$tdir"

echo "current dir:"$(pwd)

echo downloading!

echo converting with format -$format-!

if [ $format == 0 ];
then
  /usr/local/bin/yt-dlp -f "worst[width>=320]" -o $tdir"/test.webm" -- "$1"
  ls -lasi .
  echo "start agaconv OSC5 at "$tdir
  agaconv 2>&1 test.webm test.cdxl --fps=12 --color-mode=ocs5 --width=160 --audio-mode=mono --frequency=11025 --std-cdxl
  mv test.cdxl $2
fi

if [ $format == 1 ];
then
  /usr/local/bin/yt-dlp -f "worst[width>=320]" -o test.webm -- "$1"
  echo "start agaconv AGA8"
  agaconv 2>&1 test.webm test.cdxl --fps=12 --color-mode=aga8 --width=160 --audio-mode=mono --frequency=11025 --std-cdxl
  mv test.cdxl $2
fi

if [ $format == 2 ];
then
  /usr/local/bin/yt-dlp -f "worst[width>=320]" -o test.webm -- "$1"
  echo "start ffmpeg"
  rm -f $2
  ffmpeg -i test.webm -target pal-vcd $2
fi

if [ $format == 3 ];
then
  /usr/local/bin/yt-dlp -f "worst[width>=320]" -o test.webm -- "$1"
  echo "start agaconv AGA8 Large"
  agaconv 2>&1 test.webm test.cdxl --fps=12 --color-mode=aga8 --width=320 --audio-mode=mono --frequency=11025 --std-cdxl
  mv test.cdxl $2
fi

echo docker done, finish

cd ..
rm -rf $tdir
