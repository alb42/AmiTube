<?php
require_once('functions.php');
dieIfRequestIsNotValid();

$searchTerm = '';
$videoId = isset($_GET['id']) ? $videoId = $_GET['id'] : '';
$num = isset($_GET['num']) ? (int) $_GET['num'] : 10;
$logFile = '/var/log/amitube/search.log';

if (isset($_GET['q'])) {
    $searchTerm = $_GET['q'];

    if (str_contains($searchTerm, 'https://www.youtube.') || str_contains($searchTerm, 'https://www.youtu.be')) {
        $videoId = $searchTerm;
    }
}

$xml = new SimpleXMLElement('<results/>');
// @todo exec return should be checked
if (empty($videoId)) {
    exec("/usr/local/bin/yt-dlp -j \"ytsearch" . escapeshellcmd($num) . ":" . escapeshellcmd($searchTerm) . "\"", $searchResults, $ret);
} else {
    exec("/usr/local/bin/yt-dlp -j --max-downloads " . escapeshellcmd($videoId) . " -- \"" . escapeshellcmd($videoId) . "\"", $searchResults, $ret);
}
//logToFile('/tmp/ytresult.log', var_export($searchResults, true));

$xml->addAttribute('ret', $ret);
$results = 0;
foreach ($searchResults as $rawResult) {
    $resultElement = $xml->addChild('result');
    $videoDetails = json_decode($rawResult, false);
    setBasicAttributesOfResultChild($videoDetails, $resultElement);
    if (property_exists($videoDetails, 'thumbnail')) {
        $resultElement->addAttribute('icon', base64_encode($videoDetails->{'thumbnail'}));
    }

    if (property_exists($videoDetails, 'formats')) {
        $formatsXML = $resultElement->addChild('formats');
        foreach ($videoDetails->{'formats'} as $format) {
            if (property_exists($format, 'resolution')) {
                //logToFile('/tmp/ytdebug.log', "format: ".$format->{'resolution'}."\n"."  ".$acodec." ".$vcodec." ".$formatid);
                $acodec = "none";
                if (property_exists($format, 'acodec')) {
                    $acodec = $format->{'acodec'};

                    if ($acodec !== "none") {
                        $formatXML = $formatsXML->addChild('format');
                        if (property_exists($format, 'format_id')) {
                            $formatXML->addAttribute('format_id', $format->{'format_id'});
                        }
                        if (property_exists($format, 'vcodec')) {
                            $formatXML->addAttribute('vcodec', $format->{'vcodec'});
                        }
                        if (property_exists($format, 'acodec')) {
                            $formatXML->addAttribute('acodec', $format->{'acodec'});
                        }
                        if (property_exists($format, 'format')) {
                            $formatXML->addAttribute('title', $format->{'format'});
                        }
                        if (property_exists($format, 'url')) {
                            $formatXML->addAttribute('url', $format->{'url'});
                        }
                        if (property_exists($format, 'ext')) {
                            $formatXML->addAttribute('ext', $format->{'ext'});
                        }
                    }
                }
            }
        }
    }
    ++$results;
}

print($xml->asXML() . "\n");

$message = empty($videoId) ? ' search term "' . $searchTerm . '" results: ' : ' ID "' . $videoId . '" results: ';
logToFile($logFile, $message . $results);
