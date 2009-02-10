require("advice.js");

function follow_logger(follow_func)
{
  return function(I, target) {
    dumpln("log"+I+target);
    yield follow_func(arguments);
    dumpln("aft");
  };
}

function follow_before(I) {
  dumpln("follow-before "+I.buffer.current_URI.spec);
}

function find_before(I) {
  dumpln("find-before "+I.buffer.current_URI.spec);
}

command_advice("follow", follow_before, $type="after");
command_advice("follow-new-buffer", follow_before, $type="after");
command_advice("find-url", find_before, $type="after");
