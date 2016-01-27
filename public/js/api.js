
function getstatus(onSuccess, onError)
{
  $.ajax(
    { url: '/status'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function getsearch(q, type, mode, onSuccess, onError)
{
  $.ajax(
    { url: '/search' + '?q=' + encodeURIComponent(q) + '&type=' + encodeURIComponent(type) + '&mode=' + encodeURIComponent(mode)
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

function postpropertiesspaces(space_id, property_id, value, description, onSuccess, onError)
{
  $.ajax(
    { url: '/spaces/' + encodeURIComponent(space_id) + '/properties/' + encodeURIComponent(property_id) + '' + '?value=' + encodeURIComponent(value) + '&description=' + encodeURIComponent(description)
    , success: onSuccess
    , error: onError
    , type: 'POST'
    });
}

function gettraits(trait_id, onSuccess, onError)
{
  $.ajax(
    { url: '/traits/' + encodeURIComponent(trait_id) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
