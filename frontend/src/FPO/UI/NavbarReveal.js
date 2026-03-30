// NavbarReveal — hides the navbar until the user scrolls past a threshold.
//
// Because the app uses a flex container with `overflow-y: auto` (not the
// window) as its scroll surface, we listen on `nav.parentElement` instead
// of `window`.

let _cleanup = null;

export const setupNavbarRevealImpl = (threshold) => () => {
  // Tear down any previous instance first (defensive).
  if (_cleanup) {
    _cleanup();
    _cleanup = null;
  }

  const nav = document.querySelector('.navbar');
  if (!nav) return;

  const scrollContainer = nav.parentElement;
  if (!scrollContainer) return;

  // Measure the actual navbar height so CSS can collapse the gap.
  const navHeight = nav.offsetHeight;
  nav.style.setProperty('--navbar-h', navHeight + 'px');
  nav.classList.add('navbar--auto-hide');

  const onScroll = () => {
    if (scrollContainer.scrollTop > threshold) {
      nav.classList.add('navbar--revealed');
    } else {
      nav.classList.remove('navbar--revealed');
    }
  };

  scrollContainer.addEventListener('scroll', onScroll, { passive: true });

  // Run once so the initial state is correct (e.g. if the page is
  // already scrolled after a hot-reload).
  onScroll();

  _cleanup = () => {
    scrollContainer.removeEventListener('scroll', onScroll);
    nav.classList.remove('navbar--auto-hide', 'navbar--revealed');
    nav.style.removeProperty('--navbar-h');
  };
};

export const teardownNavbarRevealImpl = () => {
  if (_cleanup) {
    _cleanup();
    _cleanup = null;
  }
};
