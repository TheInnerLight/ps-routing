exports._makeApp = function() {
    var express = require('express');
    return express();
}

exports._listenHttp = function(app) {
    return function(port) {
        return function(onError, onSuccess) {
            var http = require('http');
            var server = http.createServer(app);
            server.listen(port, function(e) {
                console.log(port)
                return onSuccess(e);
            });
            return function (cancelError, cancelerError, cancelerSuccess) {
                req.cancel(); // cancel the request
                cancelerSuccess(); // invoke the success callback for the canceler
            };
        }
    }
}

exports._getPath = function (req) {
    return function () {
        return req.path;
    };
};

exports._get = function (app) {
    return function (handler)  {
        return function () {
            return app.get("*", function(req, resp) {
                return handler(req)(resp)();
            });
        }
    }
}