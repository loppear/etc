require("advice.js");


define_variable("bh_log_file", "/tmp/bh.log", "Path to file to log browser history events");


function log_event(buffer, msg) {
  var cmd = "echo \"" + shell_quote(msg) + "\" >> " + bh_log_file;
  dumpln(msg);
  co_call(shell_command(cmd, $cwd=buffer.cwd, $fds={}));
}

function make_logger(txt) {
  return function(context) {
    if (context.buffer !== undefined) {
      context = context.buffer;
    }
    if (context.current_URI) {
      log_event(context, txt + " " + context.current_URI.spec);
    }
  };
}

log_follow = make_logger("follow from");
log_find = make_logger("find from");


add_hook("buffer_description_change_hook", make_logger("going to"));

command_advice("follow", log_follow, $type="after");
command_advice("follow-new-buffer", log_follow, $type="after");
command_advice("follow-new-buffer-background", log_follow, $type="after");
command_advice("find-url", log_find, $type="after");
command_advice("find-url-new-buffer", log_find, $type="after");

