# FPO-Frontend

Dieses Projekt bildet das Frontend der Anwendung ab, geschrieben in PureScript mit dem Halogen Package als Basis.

## Setup

### Voraussetzungen

- `npm` (bestenfalls version 10.9.x)
- `node` (bestenfalls version v22.15.x)
- `spago` (erhält man via `npm install -g spago@next`)
- `esbuild` (erhält man via `npm install -g esbuild`)
- `purs` (erhält man via `npm install -g purescript`)

All dies ist benötigt, um das Projekt vollständig bauen zu können.

### Frontend starten

Hier ist die komplette Einrichtung von Grund auf beschrieben. Wenn du Schritt 1 und 2 einmal gemacht hast, braucht man nur noch Schritt 3 und 4 (solange keine neuen Packages dazukommen).

1. Rufe `npm install` auf. Damit sollten die benötigten externen Module heruntergeladen werden.
2. Rufe `spago install` auf. Damit sollten alle benötigten purescript-Module heruntergeladen werden.
3. Rufe `spago build` auf. Damit sollte eine Datei `index.js` erstellt werden.
4. Rufe `spago bundle --bundle-type app` auf. Damit sollte eine Datei `index.js` erstellt werden.
5. Öffne die Datei `index.html` im Browser deiner Wahl und erstaune das Endprodukt

## Libraries/ Frameworks

## Halogen

Dies ist das Frontend-Framework, was dem Code zu Grunde liegt. Für eine gute Dokumentation schaue [diesen Guide](https://purescript-halogen.github.io/purescript-halogen/) an.

## Bootstrap

Als Styling Framwork legen wir Bootstrap 5 (https://getbootstrap.com/docs/5.3/getting-started/introduction/) zu Grunde, da es sehr viel Funktionalität liefert, einheitliches Design ermöglicht und es eine gute Library für Halogen gibt (https://github.com/tonicebrian/purescript-halogen-bootstrap5).

## Affjax

Um HTTP-Anfragen asynchron zu bearbeiten, verwenden wir `Ajax` und das dazugehörige Purescript-Modul `Affjax-Web` (https://github.com/purescript-contrib/purescript-affjax-web).
