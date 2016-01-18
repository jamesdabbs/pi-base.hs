
function getstatus(onSuccess, onError)
{
  $.ajax(
    { url: '/status'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function getsearch(q, type, onSuccess, onError)
{
  $.ajax(
    { url: '/search' + '?q=' + encodeURIComponent(q) + '&type=' + encodeURIComponent(type)
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function getproperties(onSuccess, onError)
{
  $.ajax(
    { url: '/properties'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
