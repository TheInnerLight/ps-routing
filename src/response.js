exports._setHeader = function (key)  {
    return function (value)  {
        return function (resp) {
            return function () {
                resp.set(key, value);
            }
        }
    }
}

exports._setStatus = function (status)  {
    return function (resp) {
        return function () {
            resp.status(status);
        }
    }
}

exports._sendText = function (text) {
    return function (resp) {
        return function () {
            resp.send(text)
        }
    }
}
    
