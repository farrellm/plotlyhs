"""Extract plotly.py's colour scales as JSON, without importing plotly.

Usage: python3 -I extract-plotly-colors.py COLORS_DIR OUT_JSON

COLORS_DIR is _plotly_utils/colors from a plotly wheel. Each public module
(qualitative, sequential, diverging, cyclical) becomes a list of
[name, [colour, ...]] pairs in source order. A scale is a top-level
assignment of a list of strings, or a name imported from a sibling module.
Reversed scales (``*_r``) are left out: reverse the list instead.
"""

import ast
import json
import os
import sys

MODULES = ["qualitative", "sequential", "diverging", "cyclical"]


def scales(colors_dir, module, seen=None):
    seen = (seen or set()) | {module}
    with open(os.path.join(colors_dir, module + ".py"), encoding="utf-8") as f:
        tree = ast.parse(f.read())
    out = []
    for stmt in tree.body:
        if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1:
            target, value = stmt.targets[0], stmt.value
            if (
                isinstance(target, ast.Name)
                and isinstance(value, ast.List)
                and value.elts
                and all(isinstance(e, ast.Constant) and isinstance(e.value, str) for e in value.elts)
            ):
                out.append((target.id, [e.value for e in value.elts]))
        elif isinstance(stmt, ast.ImportFrom) and stmt.level == 1 and stmt.module and not stmt.module.startswith("_"):
            if stmt.module in seen:
                sys.exit(f"error: import cycle through {stmt.module}")
            source = dict(scales(colors_dir, stmt.module, seen))
            for alias in stmt.names:
                if alias.name in source:
                    out.append((alias.asname or alias.name, source[alias.name]))
                elif not alias.name.endswith("_r"):
                    sys.exit(f"error: {module} imports {alias.name}, which {stmt.module} does not define as a scale")
    return [(n, cs) for n, cs in out if not n.startswith("_") and not n.endswith("_r")]


def main():
    if len(sys.argv) != 3:
        sys.exit("usage: extract-plotly-colors.py COLORS_DIR OUT_JSON")
    colors_dir, out_path = sys.argv[1:]
    result = {}
    for module in MODULES:
        found = scales(colors_dir, module)
        names = [n for n, _ in found]
        if len(set(names)) != len(names):
            sys.exit(f"error: {module} defines a scale twice")
        if not found:
            sys.exit(f"error: no scales found in {module}")
        result[module] = [[n, cs] for n, cs in found]
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=1)
        f.write("\n")


if __name__ == "__main__":
    main()
