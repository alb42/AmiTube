/* AmiTube example script */
at="AMITUBE.1"

ADDRESS VALUE at

/* Convert the given Youtube URL,
  when no format number is given default from prefs is used
  Do not forget the "" when posting the URL or AREXX will convert it
  to all upper case */
CONVERT "https://youtu.be/sVtJGobSZKM"

/* Format Options
  0 = CDXL OCS
  1 = CDXL AGA
  2 = MPEG1 VCD
  3 = CDXL AGA+
*/

/* alternative: convert and play after finished downloading,
  here for example as MPEG
  and with the 2nd valid URL schema  */
/*
PLAY "https://www.youtube.com/watch?v=sVtJGobSZKM" 2
*/




