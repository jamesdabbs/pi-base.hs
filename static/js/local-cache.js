window.localCache = {}

// Cache the lookup in localStorage and prefetch on future calls
window.localCache.fetch = function(key, url, cb) {
  if (typeof(Storage) === "undefined") {
    $.get(url, cb);
  } else {
    var key = "localCache:" + key
      , value = localStorage.getItem(key);
    if (value) {
      cb(JSON.parse(value));
      $.get(url, function(data) { localStorage.setItem(key, JSON.stringify(data)); });
    } else {
      $.get(url, function(data) {
        cb(data);
        localStorage.setItem(key, JSON.stringify(data));
      });
    }
  }
}
