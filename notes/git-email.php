<?
$r = 'This is an automatically generated message notifying you that someone pushed new commits to the github repo <project>.' . " Don't forget to do git pull before your editing.\r\n\r\nEdit summary: \r\n";
$payload = $_REQUEST['payload'];
preg_match_all('@"name":"([\w ]*?)","email@', $payload, $matches);
$authors = $matches[1];
preg_match_all('@"message":"([^"]*)"@', $payload, $matches);
$messages = $matches[1];
preg_match_all('@"url":"([^"]*)"@', $payload, $matches);
$links= $matches[1];
for ($i = 0; $i < count($messages) - 1; ++$i)
    $r = $r . $authors[$i] . ' committed with message "' . $messages[$i] . '" at ' . $links[$i] . "\r\n";
mail('<receipts>', 'New commits pushed to <project>', $r, 'From: <email>' . "\r\n" . 'Reply-to: <email>' . "\r\n");
echo $r;
/?>
