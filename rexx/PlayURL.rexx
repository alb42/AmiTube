/* Start AmiTube and play the given Youtube URL */
PARSE ARG url

/* Define the path to AmiTube here: */
amitubepath="dh1:SourcesAROS/AmiTube/AmiTube"

/* check if it runs already */
IF show('PORTS','AMITUBE.1') == '0' THEN
DO
  ADDRESS 'COMMAND'
  "run <>NIL:" amitubepath
  "waitforport AMITUBE.1"
END

/* Download and Play it */
ADDRESS AMITUBE.1
PLAY url





