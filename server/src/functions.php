<?php
set_time_limit(0);

function logToFile(string $logFile, string $logMessage): void
{
    file_put_contents(
        $logFile,
        sprintf(
            "%s - %s - %s - %s\n",
            date("r"),
            $_SERVER['REMOTE_ADDR'],
            $_SERVER['HTTP_USER_AGENT'],
            $logMessage
        ),
        FILE_APPEND,
    );
}

function downloadFile(string $url, string $targetFile): bool
{
    $fileHandle = fopen($targetFile, "wb");
    $curlHandle = curl_init();
    curl_setopt($curlHandle, CURLOPT_URL, $url);
    curl_setopt($curlHandle, CURLOPT_FILE, $fileHandle);
    $downloadSuccess = curl_exec($curlHandle);
    curl_close($curlHandle);
    fclose($fileHandle);

    return $downloadSuccess;
}

function dieIfRequestIsNotValid(): void
{
    if ($_SERVER['REQUEST_METHOD'] !== 'GET' || !str_contains($_SERVER['HTTP_USER_AGENT'], 'AmiTube')) {
        die('OK');
    }
}

function downloadBaseHeaders(int $filesize): void
{
    header('Content-Length: ' . $filesize);
    header('Content-Transfer-Encoding: binary');
    header('Cache-Control: no-cache, must-revalidate');
    header('Pragma: no-cache');
}

function setBasicAttributesOfResultChild(object $resultData, SimpleXMLElement $base): void
{
    if (property_exists($resultData, 'id')) {
        $base->addAttribute('id', $resultData->{'id'});
    }
    if (property_exists($resultData, 'fulltitle')) {
        $base->addAttribute('fulltitle', htmlspecialchars($resultData->{'fulltitle'}, ENT_XML1, 'UTF-8'));
    }
    if (property_exists($resultData, 'duration')) {
        $base->addAttribute('duration', $resultData->{'duration'});
    }
    if (property_exists($resultData, 'uploader')) {
        $base->addAttribute('uploader', $resultData->{'uploader'});
    }
    if (property_exists($resultData, 'like_count')) {
        $base->addAttribute('like_count', $resultData->{'like_count'});
    }
    if (property_exists($resultData, 'view_count')) {
        $base->addAttribute('view_count', $resultData->{'view_count'});
    }
    if (property_exists($resultData, 'license')) {
        $base->addAttribute('license', $resultData->{'license'});
    }
    if (property_exists($resultData, 'description')) {
        $base->addChild('description', htmlspecialchars($resultData->{'description'}, ENT_XML1, 'UTF-8'));
    }
}
