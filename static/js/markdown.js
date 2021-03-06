"use strict";

$(function() {
  var converter = new Markdown.Converter();

  // Kind of a gross kludge, but we need to keep the markdown converter from clobbering
  //   anything inside MathJax tags (like X^*)
  var markdown = function(text) {
    text = text.replace(/\$(.*?)\$/g, '<mathjax data-contents="$1"></mathjax>');
    text = converter.makeHtml(text);
    text = text.replace(/<mathjax data-contents="(.*?)"><\/mathjax>/g, function(match, jax) {
      return '$' + jax + '$';
    });
    return text;
  }

  $(".markdown").each(function(i, el) {
    var $el = $(el);
    $el.html(markdown($el.html()));
  });

  // TODO: I'd really like a better way of specifiying this
  //   on a field-by-field basis, but Yesod.Bootstrap3 doesn't seem to expose this
  $(".form-markdown textarea").each(function(i, el) {
    var $el = $(el)
      , $preview = $("<div class='well'></div>");
    $el.attr("rows", 10).after($preview);

    var renderPreview = function() {
      $preview.html(markdown($el.val()));
      MathJax.Hub.Queue(["Typeset", MathJax.Hub, $preview.get(0)]);
    };
    renderPreview();

    $el.keyup(_.debounce(renderPreview, 500));
  });
});
