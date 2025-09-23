find src -name "*.hs" \
  | sed 's|src/||;s|/|.|g;s|.hs$||' \
  | sed 's/^/  - /'
