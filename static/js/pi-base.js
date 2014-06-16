window.piBase = {
  error: function(args) {
    if (typeof(Rollbar) === "undefined") {
      console.error(args);
    } else {
      Rollbar.error.apply(null, args);
    }
  }
};
