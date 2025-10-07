#!/usr/bin/env python3
from pathlib import Path
import re
import subprocess
import sys
import datetime

if len(sys.argv) < 3:
    print("Usage: python3 pdf_docs.py <input/path> <output.pdf> [template.tex]")
    sys.exit(1)

input_path = Path(sys.argv[1])
output_path = Path(sys.argv[2])
template_path = Path(sys.argv[3]) if len(sys.argv) > 3 else None
name = output_path.stem

def sort_key(p: Path):
    parent_name = p.parent.name
    priority = 0 if p.stem == parent_name else 1
    return (str(p.parent), priority, p.name.lower())

BASE_DIR = input_path.resolve()
markdown_files = sorted(BASE_DIR.rglob("*.md"), key=sort_key)
merged_content = []

def fix_paths(text: str, md_file: Path) -> str:
    """Korrigiere Bild- und Dateipfade relativ zum Hauptverzeichnis."""
    parent = md_file.parent

    text = re.sub(
        r"!\[([^\]]*)\]\(([^)]+)\)",
        lambda m: f"![{m.group(1)}]({(parent / m.group(2)).resolve().relative_to(BASE_DIR)})",
        text
    )

    text = re.sub(
        r"\[([^\]]+)\]\(([^)]+)(#[^)]+)?\)",
        lambda m: f"[{m.group(1)}](#{Path(m.group(2)).stem}{m.group(3) or ''})",
        text
    )

    return text


for md_file in markdown_files:
    rel_path = md_file.relative_to(BASE_DIR)
    text = md_file.read_text(encoding="utf-8")

    text = fix_paths(text, md_file)

    merged_content.append(f"\n\n<!-- BEGIN FILE: {rel_path} -->\n\n")
    merged_content.append(text)
    merged_content.append(f"\n\n<!-- END FILE: {rel_path} -->\n\n")

merged_path = BASE_DIR / "merged.md"
merged_path.write_text("\n".join(merged_content), encoding="utf-8")

subprocess.run([
    x for x in [
        "pandoc",
        "--standalone",
        "--file-scope",
        "--toc",
        "--number-sections",
        "--pdf-engine=xelatex",
        "--metadata", "title=FPO-Editor Documentation",
        "--metadata", "subtitle=Documentation for the Fachprüfungsordnung Editor",
        "--metadata", f"date={datetime.date.today()}",
        f"--template={template_path}" if template_path else None,
        str(merged_path),
        "-o", str(output_path)
    ] if x is not None
], check=True)
