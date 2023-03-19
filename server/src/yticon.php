<?php
require_once('functions.php');
dieIfRequestIsNotValid();

$videoId = $_GET['id'];
$logFile = '/var/log/amitube/icon.log';

$url = 'https://img.youtube.com/vi/' . $videoId . '/sddefault.jpg';
$tempFilename = tempnam(getcwd() . '/tmp/', 'icon_' . $videoId);
$tempFilenameSmall = $tempFilename.'small';

$downloaded = downloadImageFile($url, $tempFilename);
if (!$downloaded || !isValidJpegThumbnail($tempFilename)) {
    logToFile($logFile, 'ID "' . $videoId . '" initial download failed, trying alternative thumbnails');
    for ($thumbnailId = 0; $thumbnailId <= 3; ++$thumbnailId) {
        logToFile($logFile, 'ID "' . $videoId . '" initial download failed, trying alternative thumbnail ' . $thumbnailId);
        $downloaded = downloadImageFile(
            'https://img.youtube.com/vi/' . $videoId . '/' . $thumbnailId . '.jpg',
            $tempFilename
        );

        if ($downloaded && isValidJpegThumbnail($tempFilename)) {
            logToFile($logFile, 'ID "' . $videoId . '" download success for thumbnailId ' . $thumbnailId);
            break;
        }
    }
    if (!file_exists($tempFilename) || filesize($tempFilename) === 0) {
      // if we reached this point, no try was successful. Let's try the hd thumb as last resort
      logToFile($logFile, 'ID "' . $videoId . '" still failed, trying 720p thumbnail');
      $downloaded = downloadImageFile(
        'https://img.youtube.com/vi/' . $videoId . '/hq720.jpg',
        $tempFilename
      );
    }
}

if (!file_exists($tempFilename) || filesize($tempFilename) === 0) {
    logToFile($logFile, 'ID "' . $videoId . '" icon download failed.');
    http_response_code(404);

    return;
}

logToFile($logFile, 'ID "' . $videoId . '" got file with size ' . filesize($tempFilename));

$output = [];
exec(
    "convert -resize 160 '" . $tempFilename . "' '" . $tempFilenameSmall . "'",
    $output,
    $returnCode
);

if ($returnCode === 0) {
    downloadBaseHeaders($tempFilenameSmall);
    logToFile($logFile, 'ID "' . $videoId . '" done, serving icon with size ' . filesize($tempFilenameSmall) . ' and mime ' . mime_content_type($tempFilenameSmall));
    header("Content-Type: " . mime_content_type($tempFilenameSmall));

    $fileHandle = fopen($tempFilenameSmall, 'rb');
    ob_end_clean();
    fpassthru($fileHandle);
    ob_end_clean();
    fclose($fileHandle);
    unlink($tempFilename);
    unlink($tempFilenameSmall);
} else {
    logToFile($logFile, 'ID "' . $videoId . '" icon processing failed. Output was:');
    foreach($output as $line) {
        logToFile($logFile, $line);
    }

    if (file_exists($tempFilename)) {
        unlink($tempFilename);
    }
    if (file_exists($tempFilenameSmall)) {
        unlink($tempFilenameSmall);
    }

    http_response_code(404);
}
