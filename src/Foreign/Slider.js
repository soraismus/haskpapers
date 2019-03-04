"use strict";

function configureSlider(sliderElement, spec, consumeInts) {
  if (sliderElement.noUiSlider == null) {
    initializeSlider(sliderElement, spec);
  }

  var Data_Tuple = require('../Data.Tuple/index.js');

  var createTuple = Data_Tuple.Tuple.create;
  var updateEvent = 'update';

  var toTuple = function (array) {
    if (array.length === 2) {
      return createTuple(array[0])(array[1]);
    } else {
      throw new Error("Invalid number of NoUiSlider updated values.");j:w
    }
  }

  sliderElement.noUiSlider.on(
    updateEvent,
    function (stringValues, handle, unencoded, tap, positions) {
      var effect = consumeInts(toTuple(stringValues.map(function (x) {
        return parseInt(x, 10);
      })));

      effect();
    });

  return function () {
    sliderElement.noUiSlider.off(updateEvent);
  };
}

function initializeSlider(sliderElement, spec) {
  var Data_Maybe = require('../Data.Maybe/index.js');
  var slider = require('nouislider');
  var wNumb = require('wnumb');

  var Nothing = Data_Maybe.Nothing;
  var decimalFormat = wNumb({ decimals: 0 });

  var fromMaybe = function (maybe) {
    return (maybe instanceof Nothing)
      ? undefined
      : maybe.value0;
  }

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
}

function onSliderUpdate(spec) {
  return function (consumeInts) {
    return function () {
      requestAnimationFrame(function () {
        var sliderElement = document.getElementById(spec.id);
        return sliderElement == null
          ? function () {}
          : configureSlider(sliderElement, spec, consumeInts);
      });
    };
  }
}

exports.onSliderUpdate = onSliderUpdate;
