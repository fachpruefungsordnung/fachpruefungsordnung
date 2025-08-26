export const handleMouseEnterImpl = (event) => () => {
  const el = event.target;
  if (!el.classList?.contains("truncate-hover-title")) {
    return;
  }
  if (el.offsetWidth < el.scrollWidth) {
    el.setAttribute("title", el.innerText);
    return;
  }
  el.removeAttribute("title");
};
