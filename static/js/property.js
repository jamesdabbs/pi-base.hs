window.piBase.Property = (function() {
  var canonize = function(name) {
    return name.latinize().toLowerCase().replace(/[${}\(\)\\\s]/g, '');
  };
  var display = function(name) {
    return name.toLowerCase().replace(/[$]/g, '');
  }

  var nameQ = $.Deferred() // canonical name => id
    , idQ   = $.Deferred() // id => display name
    , suggestionsQ = $.Deferred();
  localCache.fetch("propertyNames", "/properties/names", function(data) {
    var names = {}
      , ids   = {}
      , suggestions = [];

    _.each(data, function(id, name) {
      var cName = canonize(name);
      if (names[cName]) {
        throw "Duplicate canonical name: " + cName;
      }
      names[cName] = id;

      var dName = display(name);
      ids[id] = ids[id] || dName;

      suggestions.push({key: cName, value: dName});
    });

    nameQ.resolve(names);
    idQ.resolve(ids);
    suggestionsQ.resolve(suggestions);
  });

  return {
    canonize:    canonize,
    names:       nameQ.promise(),
    ids:         idQ.promise(),
    suggestions: suggestionsQ.promise()
  }
} ());
