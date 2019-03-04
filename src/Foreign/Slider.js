"use strict";
var Data_Maybe = require('../Data.Maybe/index.js');
var slider = require('nouislider');
var wNumb = require('wnumb');

var _exports = (function () {
  var created = false;
  var sliderElement = null;

  function createSlider(spec) {
    return function () {
      if (!created) {
        created = true;

        requestAnimationFrame(function () {
          sliderElement = document.getElementById(spec.id);

          var fromMaybe = function (maybe) {
            return (maybe instanceof Data_Maybe.Nothing)
              ? undefined
              : maybe.value0;
          }

          var decimalFormat = wNumb({ decimals: 0 });

          slider.create(
            sliderElement,
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

  function onSliderUpdate(consumeInts) {
    return function () {
      if (sliderElement === null) {
        return function () {};
      } else {
        var updateEvent = 'update';

        sliderElement.slider.off(updateEvent);

        sliderElement.slider.on(
          updateEvent,
          function (values, handle, unencoded, tap, positions) {
            consumeInts(values.map(function (x) {
              return parseInt(x, 10);
            }))();
          });

        return function () {
          sliderElement.slider.off(updateEvent);
        };
      }
    };
  }

  return {
    createSlider: createSlider,
    onSliderUpdate: onSliderUpdate
  };
})();

exports.createSlider = _exports.createSlider;
exports.onSliderUpdate = _exports.onSliderUpdate;
