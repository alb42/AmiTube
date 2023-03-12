<?php
require_once('functions.php');
dieIfRequestIsNotValid();

$videoId = $_GET['id'];
$logFile = '/var/log/amitube/icon.log';

$url = 'https://img.youtube.com/vi/' . $videoId . '/sddefault.jpg';
$tempFilename = tempnam(getcwd() . '/tmp/', 'icon_' . $videoId);
$downloaded = downloadFile($url, $tempFilename);
if (!$downloaded) {
    logToFile($logFile, 'ID "' . $videoId . '" initial download failed, trying alternative thumbnails');
    for ($thumbnailId = 0; $thumbnailId <= 3; ++$thumbnailId) {
        $downloaded = downloadFile(
            'https://img.youtube.com/vi/' . $videoId . '/' . $thumbnailId . '.jpg',
            $tempFilename
        );

        if ($downloaded) {
            logToFile($logFile, 'ID "' . $videoId . '" download success for thumbnailId ' . $thumbnailId);
            break;
        }
    }
}

if (!file_exists($tempFilename) || filesize($tempFilename) === 0) {
    logToFile($logFile, 'ID "' . $videoId . '" icon download failed.');
    http_response_code(404);

    return;
}

$output = [];
exec(
    "convert -resize 160 '" . $tempFilename . "' '" . $tempFilename . "'",
    $output,
    $returnCode
);

if ($returnCode === 0) {
    logToFile($logFile, 'ID "' . $videoId . '" done, serving icon');
    downloadBaseHeaders(filesize($tempFilename));
    header("Content-Type: " . mime_content_type($tempFilename));
    ob_end_clean(); // should not be required here, but better safe then sorry

    $fileHandle = fopen($tempFilename, 'rb');
    fpassthru($fileHandle);
    unlink($tempFilename);
} else {
    logToFile($logFile, 'ID "' . $videoId . '" icon processing failed. Output was:');
    foreach($output as $line) {
        logToFile($logFile, $line);
    }

    if (file_exists($tempFilename)) {
        unlink($tempFilename);
    }

    http_response_code(404);
}
