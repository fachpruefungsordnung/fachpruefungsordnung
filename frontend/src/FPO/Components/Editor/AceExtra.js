// src/FPO/Components/Editor/AceExtra.mjs
"use strict";

// Mauskoordinaten (clientX, clientY) → Textposition (row, column)
export const screenToText = (editor) => (clientX) => (clientY) => () => {
  const r = editor.renderer.screenToTextCoordinates(clientX, clientY);
  return { row: r.row, column: r.column };
};

// Textposition (row, col) → Bildschirmkoordinaten (pageX, pageY)
export const textToScreen = (editor) => (row) => (col) => () => {
  const p = editor.renderer.textToScreenCoordinates(row, col);
  return { pageX: p.pageX, pageY: p.pageY };
};

// Anchor neu setzen (z. B. beim Draggen eines Handles)
export const setAnchorPosition = (anchor) => (row) => (col) => () => {
  anchor.setPosition(row, col, true);
};

export const addClass = (el) => (cls) => () => {
  el.classList.add(cls);
};

export const removeClass = (el) => (cls) => () => {
  el.classList.remove(cls);
};

// Löscht den momentan selektierten Text
export const clearSelection = (editor) => () => {
  editor.clearSelection();
};