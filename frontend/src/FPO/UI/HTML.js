export const setInnerHtml = function(element) {
  return function(innerHtml) {
    return function() {
      element.innerHTML = innerHtml;
    };
  };
};
