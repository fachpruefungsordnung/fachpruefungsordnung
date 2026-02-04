"use strict";

export const selectElement = function (el) {
  return function () {
    if (el && typeof el.select === "function") {
      el.select();
    } else if (el && typeof el.setSelectionRange === "function") {
      el.setSelectionRange(0, el.value.length);
    }
  };
};
