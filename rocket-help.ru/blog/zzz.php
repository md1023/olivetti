<?php
$head = <<<EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" 
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Hotkeys database</title>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
    <link rel="stylesheet" type="text/css" href="../css/common.css"/>
    <script type="text/javascript" src="../js_scripts/jquery/jquery.js"></script>
    <script type="text/javascript" src="../js_scripts/arm.js"></script>
  </head>
EOF;

$body = "The page is empty.";
$file_name = $_GET["name"];
if ($file_name) {
  $body = include $file_name . ".html";
}
echo $head . $body . "</html>";
?>