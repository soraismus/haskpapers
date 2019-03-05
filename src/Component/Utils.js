"use strict";

function afterDuration(int) {
  return function (effect) {
    return function () {
      var id = setTimeout(effect, int);
      return function () {
        clearTimeout(id);
      };
    };
  };
}

exports.afterDuration = afterDuration;
