exports._getPath = function (req) {
    return function () {
        return req.path;
    };
};

exports._get = function (app) {
    return function (handler)  {
        return function () {
            return app.get("*", handler)
        }
    }
}