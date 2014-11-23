"use strict";


var closeParens = function(str) {
  var stack = [], el;
  _.map(str, function(c) {
    if (c === '{') {
      stack.push('}');
    } else if (c === '[') {
      stack.push(']');
    } else if (c === '}' || c === ']') {
      stack.pop();
    }
  });
  while (el = stack.pop()) {
    str += el;
  }
  return str;
};

var jsonliteEscape = function(str) {
  if (str.match(/[",\[\]{}:]/)) {
    return '"' + str.replace('"', '\\"') + '"';
  } else {
    return str;
  }
}


var condense = function(val, cb) {
  var parsed;

  if (!val || val[0] === ":") return cb(null, val);

  try {
    parsed = JSON.parse(val);
  } catch(e) {
    cb("Could not condense '" + val + "'. Failed to parse as JSON.");
    return;
  }

  var modifier = "";
  if (val[0] === "!" || val[0] === "?") {
    modifier = val[0];
    val = val.slice(1, val.length);
  }
  piBase.Property.ids.done(function(ids) {
    var condenser = function(f) {
      if (f.and) {
        return "{and: [" + _.map(f.and, condenser).join(", ") + "]}";
      } else if (f.or) {
        return "{or: [" + _.map(f.or, condenser).join(", ") + "]}";
      } else {
        var kv = _.pairs(f)[0]
          , prefix = kv[1] ? "" : "~"
          , dName = ids[""+kv[0]];
        return jsonliteEscape(prefix + dName);
      }
    }

    cb(null, modifier + condenser(parsed));
  });
}


var expand = function(f, cb) {
  if (f[0] === ":") return cb(null, f);

  var modifier = "";
  if (f[0] === "!" || f[0] == "?") {
    modifier = f[0];
    f = f.slice(1, f.length);
  };

  piBase.Property.names.done(function (names) {
    var unknowns = []
      , expander = function(f) {
          if (f.and) {
            return {and: _.map(f.and, expander)};
          } else if (f.or) {
            return {or: _.map(f.or, expander)};
          } else {
            // TODO: fuzzy match, make suggestions if missing
            var name, val, prop, atom;

            if (f[0] === "~" || f[0] === "-" || f[0] === "¬") {
              name = f.slice(1, f.length);
              val = false;
            } else {
              name = f;
              val = true;
            };

            prop = names[piBase.Property.canonize(name)];
            if (! prop) {
              unknowns.push(name);
            }

            atom = {}
            atom[prop] = val;
            return atom;
          }
        };

    var formula;
    try {
      formula = jsonlite.parse(closeParens(f));
    } catch(e) {
      return cb("Could not parse formula. Please be sure it is well-formed.");
    }

    try {
      var result = expander(formula);
      if (unknowns.length > 0) {
        cb("Unrecognized properties: " + unknowns.join(", "));
      } else {
        cb(null, modifier + JSON.stringify(result));
      }
    } catch(e) {
      piBase.error(["Failed to expand formula:", e]);
    };
  });
};

var displayError = function($field, err) {
  var val = $field.val();

  $field.popover({
    placement: "bottom",
    html: true,
    content: err,
    trigger: "manual"
  }).popover("show").blur(function() {
    if ($field.val() !== val) {
      $field.popover("hide");
    }
  });
}


$(function() {
  $(".formula").each(function() {
    var $field = $(this);

    condense($field.val(), function(err, formatted) {
      if (err) return piBase.error(["Can't reformat formula field:", err]);
      $field.val(formatted);
    });

    piBase.formulaTypeahead($field);
  });

  $("form:has(.formula)").submit(function(e) {
    e.preventDefault();

    var form = this
      , fs = $(form).find(".formula:not(.tt-hint)") // typeahead js creates these phantoms
      , done = _.after(fs.length, function() {
        // Adjust the submitted values without redrawing the fields
        fs.each(function() {
          var $field = $(this)
            , name = $field.attr("name")
            , val = $field.data("expanded");
          $field.attr("name","");
          $field.append( $('<input type="hidden" name="' + name + '">').val(val) );
        });

        form.submit();
      });

    fs.each(function() {
      var $field = $(this);
      expand($field.val(), function(err, val) {
        if (err) {
          displayError($field, err);
        } else {
          $field.data("expanded", val);
          done();
        }
      });
    });

    return false;
  });

  $("a.search-help").click(function(e) {
    e.preventDefault();
    $('#search-help').modal();
    return false;
  });
});
