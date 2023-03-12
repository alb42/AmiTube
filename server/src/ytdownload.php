<?php
require_once('functions.php');
dieIfRequestIsNotValid();

$videoId = $_GET['id'];
$id = escapeshellcmd($videoId);
$format = escapeshellcmd($_GET['format']);
$tempFilename = tempnam(getcwd() . '/tmp/', 'at_' . $videoId);
$logFile = '/var/log/amitube/download.log';
unlink($tempFilename);

// @todo: exec calls return should be checked
logToFile($logFile, 'Requested format ' . $format);
if ($format === 'mp3') {
    exec('/usr/local/bin/yt-dlp -f wa[vcodec=none] -o '.$tempFilename.'.mp4 -- "'.$id.'"', $msg, $returnCode);
    exec('/usr/bin/ffmpeg -i '.$tempFilename.'.mp4 -q:a 5 '.$tempFilename.'.mp3', $msg, $returnCode);
    unlink($tempFilename.'.mp4');
    rename($tempFilename.'.mp3', $tempFilename);
} elseif ($format === '8svx') {
    exec('/usr/local/bin/yt-dlp -f ba[vcodec=none] --audio-format mp3 -o '.$tempFilename.'.mp3 -- "'.$id.'"', $msg, $returnCode);
    exec('/usr/bin/ffmpeg -i '.$tempFilename.'.mp3 '.$tempFilename.'.wav', $msg, $returnCode);
    unlink($tempFilename.'.mp3');
    exec('/usr/bin/sox '.$tempFilename.'.wav -b 8  --endian big -r 22050 -c 2 '.$tempFilename.'.8svx', $msg, $returnCode);
    unlink($tempFilename.'.wav');
    rename($tempFilename.'.8svx', $tempFilename);
} else {
    exec("/usr/local/bin/yt-dlp -f '".$format."' -o ".$tempFilename." -- \"".$id."\"", $msg, $returnCode);
}

if ($returnCode === 0 && file_exists($tempFilename)) {
    downloadBaseHeaders(filesize($tempFilename));
    ob_end_clean();//required here or large files will not work

    $file = fopen($tempFilename, 'rb');
    fpassthru($file);
    fclose($file);
    logToFile($logFile, 'ID "' . $videoId . '" send file ' . $tempFilename . ' with size ' . filesize($tempFilename));

    logToFile($logFile, 'ID "' . $videoId . '" send done ' . $tempFilename);
    unlink($tempFilename);
} else {
    logToFile($logFile, 'ID "' . $videoId . '" failed or no temp file found. Command output was: ');
    foreach($msg as $line) {
        logToFile($logFile, $line);
    }

    http_response_code(404);
}
