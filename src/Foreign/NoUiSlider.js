"use strict";

var _exports = (function () {
  var created = false;
  var slider = null;

  function createNoUiSliderEffect(spec) {
    return function () {
      if (!created) {
        created = true;

        requestAnimationFrame(function () {
          slider = document.getElementById(spec.id);

          var Data_Maybe = require('../Data.Maybe/index.js');
          var noUiSlider = require('nouislider');
          var wNumb      = require('wnumb');

          var nothing = Data_Maybe.Nothing.value;

          var fromMaybe = function (maybe) {
            return maybe === nothing ? undefined : maybe;
          }

          var decimalFormat = wNumb({ decimals: 0 });

          noUiSlider.create(
            slider,
            {
              start       : spec.start,
              margin      : fromMaybe(spec.margin),
              limit       : fromMaybe(spec.limit),
              connect     : fromMaybe(spec.connect),
              direction   : fromMaybe(spec.direction),
              orientation : fromMaybe(spec.orientation),
              behavior    : fromMaybe(spec.behavior),
              step        : fromMaybe(spec.step),
              range       : fromMaybe(spec.range),

              tooltips: [decimalFormat, decimalFormat],
              pips: {
                mode    : 'positions',
                values  : [0, 25, 50, 75, 100],
                density : 5
              }
            }
          );
        });
      }

      return {};
    };
  }

  function updateNoUiSliderEffect(ints) {
    return function () {
      if (slider != null) {
        var updateEvent = 'update';

        slider.noUiSlider.off(updateEvent);

        slider.noUiSlider.on(
          updateEvent,
          function (values, handle, unencoded, tap, positions) {
            values.map(function (x) {
              return parseInt(x, 10);
            });
          });
      }

      return {};
    };
  }

  return {
    createNoUiSliderEffect: createNoUiSliderEffect,
    updateNoUiSliderEffect: updateNoUiSliderEffect
  };
})();

exports.createNoUiSliderEffect = _exports.createNoUiSliderEffect;
exports.updateNoUiSliderEffect = _exports.updateNoUiSliderEffect;
