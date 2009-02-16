require("advice.js");

function follow_logger(follow_func)
{
  return function(I, target) {
    yield follow_func(arguments);
  };
}

function follow_after(I) {
  dumpln("follow "+I.buffer.current_URI.spec);
}

function find_after(I) {
  dumpln("find "+I.buffer.current_URI.spec);
}

command_advice("follow", follow_after, $type="after");
command_advice("follow-new-buffer", follow_after, $type="after");
command_advice("follow-new-buffer-background", follow_after, $type="after");
command_advice("find-url", find_after, $type="after");
command_advice("find-url-new-buffer", find_after, $type="after");


function make_watch(txt) {
  return function watch_location_change(buffer) {
    var url = buffer.current_URI && buffer.current_URI.spec || "()"
    dumpln(txt+" to "+url);
  };
}

add_hook("current_content_buffer_location_change_hook",
         make_watch("change"));
add_hook("content_buffer_finished_loading_hook",
         make_watch("open"));
