<?php

echo '<form id="save_combination" method="post" action="">
      <p class="icon note">
        <span>Enter hotkey combination, action and tag:</span><br/>
          <input id="combination" type="text" name="combination"/>
          <label for="combination">Combination
            <span>Type in keys as pressed</span>
          </label>
          <input id="action" type="textarea" name="action"/>
          <label for="action">Description
            <span>What combination does</span>
          </label>
          <input id="tags" type="text" name="tags"/>
          <label for="tags">Tags
            <span>Where can be used</span>
          </label>
        <input type="submit" value="Save"/>
      </p>
    </form>';

if (!empty($_POST['combination']) &&
    !empty($_POST['action']) &&
    !empty($_POST['tags'])) {
  check_combination(
		    $_POST['combination'],
		    $_POST['action'],
		    $_POST['tags']
		    );
};

function check_combination($combination, $action, $tags) {
  mysql_connect("localhost", "rockethelp_src", "hL2CoL9O") or die(mysql_error());
  mysql_select_db("rockethelp_hotkeys") or die(mysql_error());
		
  $sql_add_action = sprintf("INSERT INTO keys_action (action)\n"
			    . "VALUES ('%s')", mysql_real_escape_string($action));
  mysql_query($sql_add_action) or die(mysql_error());
  $action_id = mysql_insert_id();
		
  $sql_add_combination = sprintf("INSERT INTO keys_combination (combination)\n"
				 . "VALUES ('%s')", mysql_real_escape_string($combination));
  mysql_query($sql_add_combination) or die(mysql_error());
  $key_id = mysql_insert_id();

  $sql_add_tags = sprintf("INSERT INTO tags (name)\n"
			  . "VALUES ('%s')", mysql_real_escape_string($tags));
  mysql_query($sql_add_tags) or die(mysql_error());
  $tags_id = mysql_insert_id();

  $sql = "INSERT INTO tags_combination (combinations_id, tag_id)\n"
    . "VALUES (" . $key_id . ", " . $tags_id . ")";
  mysql_query($sql) or die(mysql_error());

  $sql = "INSERT INTO combination_action (key_id, action_id)\n"
    . "VALUES (" . $key_id . ", " . $action_id . ")";
  mysql_qery($sql) or die(mysql_error());
};

function remove_combination() {
  mysql_connect("localhost", "rockethelp_scr", "hL2CoL9O") or die(mysql_error());
  mysql_select_db("rockethelp_hotkeys") or die(mysql_error());
  $sql_add_action = sprintf("", mysql_real_escape_string($action));
  mysql_query($sql_add_action) or die(mysql_error());
};

?>