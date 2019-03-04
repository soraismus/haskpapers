"use strict";
var Data_Maybe = require('../Data.Maybe/index.js');
// var slider = require('nouislider');
// var wNumb = require('wnumb');

function configureSlider(sliderElement, spec, consumeInts) {
  if (sliderElement.noUiSlider == null) {
    initializeSlider(sliderElement, spec);
  }

  var updateEvent = 'update';

  // sliderElement.noUiSlider.off(updateEvent);

  sliderElement.noUiSlider.on(
    updateEvent,
    function (values, handle, unencoded, tap, positions) {
      consumeInts(values.map(function (x) {
        return parseInt(x, 10);
      }))();
    });

  return function () {
    sliderElement.noUiSlider.off(updateEvent);
  };
}

function initializeSlider(sliderElement, spec) {
  var slider = require('nouislider');
  var wNumb = require('wnumb');

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
}

function fromMaybe(maybe) {
  return (maybe instanceof Data_Maybe.Nothing)
    ? undefined
    : maybe.value0;
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
