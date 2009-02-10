
define_keywords("$type");
function command_advice(command, func)
{
  keywords(arguments);
  var wrapper = {
    "before": before_advice,
    "after": after_advice,
    "around": around_advice
  }[arguments.$type || "before"];
  dumpln(wrapper);
  var wrapped = wrapper(func, get_handler(command));
  set_handler(command, wrapped);
}

function before_advice(func, next)
{
  return function() {
    func.apply(func, arguments);
    yield next.apply(next, arguments);
  };
}

function after_advice(func, next)
{
  return function() {
    yield next.apply(next, arguments);
    func.apply(func, arguments);
  };
}

function around_advice(func, next)
{
  return func( function(args)
               {
                 yield next.apply(next, args);
               }
             );
}

