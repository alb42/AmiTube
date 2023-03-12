<?php
require_once('functions.php');
dieIfRequestIsNotValid();

$videoId = $_GET['id'];
$tempFilename = tempnam(getcwd() . '/tmp/', 'at_' . $videoId);
$logFile = '/var/log/amitube/convert.log';
$format = min(
    max((int) $_GET['format'], 0),
    3
); // 0 = old OSC5, 1 = AGA8, 2 = MPEG1 3= AGAL

logToFile($logFile, 'ID "' . $videoId . '" start atconvert.sh');
exec(
    sprintf(
        '%s/prg/atconvert.sh %s %s %s',
        getcwd(),
        escapeshellcmd($videoId),
        $tempFilename,
        escapeshellcmd($format)
    ),
    $commandOutput,
    $returnCode
);
logToFile($logFile, 'ID "' . $videoId . '" Finished atconvert.sh with return ' . $returnCode);

$txt = implode("\n", $commandOutput);
logToFile($logFile, 'ID "' . $videoId . '" output ' . $txt);

if ($returnCode === 0) {
    downloadBaseHeaders(filesize($tempFilename));
    
    if ($format === 2) {
        header('Content-Type: video/MP1S');
        header('Content-Disposition: filename="' . $videoId . '.mpeg"');
    } else {
        header('Content-Type: application/octet-stream');
        header('Content-Disposition: filename="' . $videoId . '.cdxl"');
    }
    ob_end_clean(); // force a flush and end buffering, makes sure client receives early-output to prevent timeouts

    logToFile($logFile, 'ID "' . $videoId . '" send file ' . $tempFilename);
    $fileHandle = fopen($tempFilename, 'rb');
    fpassthru($fileHandle);
    fclose($fileHandle);
    logToFile($logFile, 'ID "' . $videoId . '" send done ' . $tempFilename);
} else {
    logToFile($logFile, 'ID "' . $videoId . '" error 410');
    http_response_code(410);
    echo 'ERROR ' . $returnCode . "\n" . $commandOutput[array_key_last($commandOutput)];
}

logToFile($logFile, 'ID "' . $videoId . '" everything done, delete file ' . $tempFilename);
unlink($tempFilename);
