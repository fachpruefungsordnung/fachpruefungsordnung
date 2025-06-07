export const driver = {
  newXHR: function () {
    return new XMLHttpRequest();
  },
  fixupUrl: function (url) {
    return url || "/";
  },
};

/**
 * Liest den XSRF-Token aus den Cookies aus
 * @param {string} [tokenName='XSRF-TOKEN'] - Der Name des Tokens in den Cookies
 * @returns {string} Der XSRF-Token oder ein leerer String, wenn kein Token gefunden wurde
 */
export function getCookieEff(name) {
  return function() {  // This returns a thunk that will be executed by PureScript's Effect system
    const cookies = document.cookie.split(';');
    for (const cookie of cookies) {
      const trimmedCookie = cookie.trim();
      if (trimmedCookie.indexOf(`${name}=`) === 0) {
        return decodeURIComponent(
          trimmedCookie.substring(name.length + 1)
        );
      }
    }
    return '';
  };
}
