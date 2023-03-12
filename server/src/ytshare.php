<?php
require_once('functions.php');
dieIfRequestIsNotValid();

$videoId = $_GET['id'];
$xmlFile = 'ytshares.xml';
$logFile = '/var/log/amitube/share.log';

$xml = file_exists($xmlFile) ? simplexml_load_string(file_get_contents($xmlFile)) : new SimpleXMLElement('<results/>');
if ($xml->hasChildren()) {
    foreach ($xml->children() as $result) {
        $arr = $result->attributes();
        if ($arr['id'] === $videoId) {
            die('already in the list');
        }
    }
}

//@todo: check exec ret
exec("/usr/local/bin/yt-dlp -j '" . escapeshellcmd($videoId) . "'", $searchResults, $ret);
foreach ($searchResults as $rawResult) {
    $resultElement = $xml->addChild('result');
    $videoDetails = json_decode($rawResult, false);
    setBasicAttributesOfResultChild($videoDetails, $resultElement);
}
$xml->asXML($xmlFile);

echo("added to list\n");
logToFile($logFile, 'share "' . $videoId . '" added');
