exports._setHeader = function (key)  {
    return function (value)  {
        return function (resp) {
            return function () {
                resp.set(key, value);
            }
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
    
