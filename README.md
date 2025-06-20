# AmiTube
A YouTube Client for Amiga (with server component, CDXL conversation)

Did you every dream about to watch ordinary YouTube Videos on your classic Amiga? Now it is possible, with AmiTube. You can search by keywords or youtube URLs for your favorite short films then the Server will download them convert to CDXL(32 for OCS Amigas or 255 colors for AGA or RTG Amigas) or MPEG1 (for fast RTG Amigas) then the movie is downloaded to your Amiga where you can use any CDXL/MPEG Player to watch them.

The original Idea for this program is by [Michal Bergseth](https://amitopia.com), he wrote an [dedicated article](https://amitopia.com/what-is-amitube-all-about/) about what AmiTube is about.

I suggest you use [AGABlaster](http://home.alb42.de/files/agablaster_0_9_83.lha) for CDXL Movies and [RiVA](http://aminet.net/gfx/show/RiVA-0.54.lha) for MPEG movies.

## How to compile

Cross compilation is strongly, natively compiling on Amiga is possible but need a lot of RAM (128 MB+).
Check the [FPC Amiga Wiki](https://fpcamigawiki.alb42.de/index.php?title=Installation_Classic#Crosscompile_from_Linux) how to create an Amiga Cross compiler on your Linux System. You will need [MUIClass](https://github.com/alb42/MUIClass) to compile the project. for example to compile for Classic Amiga 68020:
```
fpc -Tamiga -Pm68k -Avasm -XV -Fu/path/to/MUIClass/src AmiTube.pas
```

## Your very own converter Server (via docker container)
All you need to get it, is to start:
```
docker pull alb42/amitubeserver
```
on your linux box with docker installed and then to start the actual docker container:
```
docker run -p 1234:80 alb42/amitubeserver
```
here the 1234 is just as example the port you want to have the server running on (of 80 is free of course you can also use -p 80:80). Then go to your AmiTube icon and add the SERVERURL= parameter for example
```
SERVERURL=http://192.168.0.234:1234
```
