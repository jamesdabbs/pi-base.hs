"use strict";


String.prototype.regexIndexOf = function( pattern, startIndex ) {
  startIndex = startIndex || 0;
  var searchResult = this.substr( startIndex ).search( pattern );
  return ( -1 === searchResult ) ? -1 : searchResult + startIndex;
}
String.prototype.regexLastIndexOf = function( pattern, startIndex ) {
  startIndex = startIndex === undefined ? this.length : startIndex;
  var searchResult = this.substr( 0, startIndex ).reverse().regexIndexOf( pattern, 0 );
  return ( -1 === searchResult ) ? -1 : this.length - ++searchResult;
}
String.prototype.reverse = function() {
  return this.split('').reverse().join('');
}


piBase.formulaTypeahead = function($field) {
  $field.typeahead({
    minLength: 2,
    highlight: false
  }, {
    name: "formula-typeahead",
    displayKey: function(obj) { return obj.prefix + obj.value; },
    source: function(query, cb) {
      // Handle special query types
      if (query[0] == ":") return;

      var prefix = ""
      if (query[0] == "?" || query[0] == "!") {
        prefix = query[0];
        query = query.slice(1, query.length);
      }

      var fragment, sep = query.regexLastIndexOf(/[,\[]/);
      if (sep == -1) {
        fragment = query;
      } else {
        prefix = prefix + query.slice(0, sep + 1);
        fragment = query.slice(sep + 1, query.length);
      }

      // Include whitespace in the prefix
      var s = /(\s*~?\s*)(.*)/.exec(fragment);
      prefix += s[1];
      fragment = s[2];

      if (fragment.length < 2 || fragment[0] === "{") {
        cb([]);
      } else {
        // TODO: fuzzy-find suggestions
        piBase.Property.suggestions.done(function (suggestions) {
          var hits = _.select(suggestions, function(s) {
            return s.key.indexOf(piBase.Property.canonize(fragment)) === 0;
          });
          cb(_.map(hits, function(h) {
            h.prefix = prefix;
            return h;
          }));
        });
      }
    }
  });
}
