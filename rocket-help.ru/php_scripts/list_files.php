<?php
if ($handle = opendir('../blog/')) {
  require("../php_scripts/phpQuery.php");
  $excluded_files = array(".", "..", "bio.html", "index.html", "hate.html");
  $pages = array();
  while (false !== ($entry = readdir($handle))) {
    if ( !in_array($entry, $excluded_files) && preg_match("/[a-z0-9]+\.html/", $entry)) {
      $html = file_get_contents("http://rocket-help.ru/blog/".$entry);
      $doc = phpQuery::newDocumentHTML($html);
      $h = $doc->find("h3:first")->text();
      if (empty($h)) {
        $h = $entry;
      };
      $pages[$entry] = $h;
    }
  }
  echo json_encode($pages);
  closedir($handle);
}
?>