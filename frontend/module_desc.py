import sys
from pathlib import Path

if len(sys.argv) < 3:
    print("Usage: python3 module_docs.py <input.md> <output.purs>")
    sys.exit(1)

input_path = Path(sys.argv[1])
output_path = Path(sys.argv[2])
name = output_path.stem

with input_path.open("r", encoding="utf-8") as f:
    markdown = f.read()

def doc(line: str) -> str:
    return f"-- | {line.replace('```hs', '```purescript')}"

def docs(*lines: str) -> list[str]:
    return [doc(line) for line in lines]

def unlines(*lines: str) -> str:
    return "\n".join(lines)

output = unlines(
    *docs(*markdown.splitlines()),
    f"module {name} where",
    "-- dummy file to generate a index for the module documentation."
)

with output_path.open("w", encoding="utf-8") as f:
    _ = f.write(output)
