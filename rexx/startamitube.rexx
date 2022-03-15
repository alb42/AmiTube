/* start AmiTube and wait for become available */

/* Define the path to AmiTube here: */
amitubepath="dh1:SourcesAROS/AmiTube/AmiTube"

/* check if it runs already */
IF show('PORTS','AMITUBE.1') == '0' THEN
DO
  ADDRESS 'COMMAND'
  "run <>NIL:" amitubepath
  "waitforport AMITUBE.1"
  IF RC == '0' THEN
    say('AmiTube is started')
  ELSE
    say('AmiTube cannot be started')
END
ELSE
  say('AmiTube is already running')
