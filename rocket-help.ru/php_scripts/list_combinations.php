<?php
mysql_connect("localhost", "rockethelp_scr", "hL2CoL9O") or die(mysql_error());
mysql_select_db("rockethelp_hotkeys") or die(mysql_error());

$tags_query = "SELECT name, count(name) from tags group by name order by name asc";
$tags = mysql_query($tags_query) or die(mysql_error());

echo "<div id=\"tag_list\"><ul>";
echo "<li><a href=\"?showall\">clear</a></li>";
$i = 0;
$tag_list = array();
while ($tag = mysql_fetch_row( $tags )) {
  ++$i;
  array_push($tag_list, $tag[0]);
  echo "<li><a href=\"?tag=", $tag[0], "\">", $tag[0],"</a></li>";
}
echo "</ul>";
echo "<div id=\"tag_cloud\">
      <canvas width=\"350\" height=\"250\" id=\"tag_canvas\">
      <p>Please, install browser with canvas support.</p>
      </canvas></div>";
echo "</div>";

if (isset($_GET['tag'])) {
  $filter = $_GET['tag'];
  if (in_array($filter, $tag_list)) {
    $filter = "JOIN tags_combination tc ON tc.combinations_id = ca.id\n"
      . "JOIN tags t ON t.id = tc.tag_id\n"
      . "WHERE t.name = \"" . $filter . "\"\n";
  } else {
    $filter = "";
  };
};
  
$sql = "SELECT ca.id, kc.combination, ka.action\n"
  . "FROM combination_action ca\n"
  . "JOIN keys_action ka ON ka.id = ca.key_id\n"
  . "JOIN keys_combination kc ON kc.id = ca.action_id\n"
  . $filter
  . "LIMIT 0 , 200";
$result = mysql_query($sql) or die(mysql_error());

echo "<table class=\"hotkeys\">";
while ($row = mysql_fetch_assoc( $result )) {
  echo "<tr><td class=\"monotype combination\">", $row['combination'],
    "</td><td class=\"action\">", $row['action'], "</td></tr>";
}
echo "</table>";
?>