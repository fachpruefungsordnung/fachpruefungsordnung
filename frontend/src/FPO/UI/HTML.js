export function setInnerHtml(element) {
    return function(html) {
        return function() {  // This returns a thunk that will be executed by PureScript's Effect system
            element.innerHTML = html;
        };
    };
}