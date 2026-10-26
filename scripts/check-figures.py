"""Check the figure helpers' goldens against plotly.py.

Usage (from the repository root, with the vendored plotly.py version):

    uv run --no-project --with plotly==$(cat vendor/plotly.py/VERSION) \
        python -I scripts/check-figures.py

Each figure in test/FigureTests.hs's subplotFigures and shapeFigures is made
again here with plotly.py, and compared with its golden in
test/golden/figures/ as JSON, numbers to within 1e-9. The grids printed by
the printGrid tests are printed here too, to compare by eye.
"""

import json
import math
import sys

import plotly.graph_objects as go
import plotly.io as pio
from plotly.subplots import make_subplots

pio.templates.default = None


def ys(vs, **kw):
    return go.Scatter(y=vs, **kw)


def figures():
    out = {}
    out["subplots-basic"] = make_subplots(rows=2, cols=2)
    out["subplots-shared-x"] = make_subplots(rows=3, cols=2, shared_xaxes="columns")
    out["subplots-shared-y"] = make_subplots(rows=2, cols=3, shared_yaxes="rows")
    out["subplots-shared-all"] = make_subplots(rows=2, cols=2, shared_xaxes="all", shared_yaxes="all")
    out["subplots-shared-bottom-left"] = make_subplots(
        rows=3, cols=2, shared_xaxes="columns", shared_yaxes="all", start_cell="bottom-left"
    )
    f = make_subplots(
        rows=2,
        cols=2,
        specs=[[{"secondary_y": True}, {}], [{}, {"secondary_y": True}]],
        shared_yaxes="all",
    )
    f.add_trace(ys([1, 2]), row=1, col=1)
    f.add_trace(ys([20, 10]), row=1, col=1, secondary_y=True)
    f.add_trace(ys([3, 4]), row=2, col=2, secondary_y=True)
    out["subplots-secondary-y"] = f
    f = make_subplots(
        rows=2,
        cols=3,
        specs=[
            [{"type": "xy"}, {"type": "polar"}, {"type": "pie"}],
            [{"type": "scene"}, {"type": "geo"}, {"type": "ternary"}],
        ],
        subplot_titles=["xy", "polar", "", "scene"],
    )
    f.add_trace(ys([1, 3, 2]), row=1, col=1)
    f.add_trace(go.Scatterpolar(r=[1, 2]), row=1, col=2)
    f.add_trace(go.Pie(values=[1, 2]), row=1, col=3)
    f.add_trace(go.Scatter3d(z=[1, 2]), row=2, col=1)
    f.add_trace(go.Scattergeo(lat=[1, 2]), row=2, col=2)
    f.add_trace(go.Scatterternary(a=[1, 2]), row=2, col=3)
    out["subplots-mixed"] = f
    out["subplots-spans"] = make_subplots(
        rows=3,
        cols=3,
        specs=[
            [{"colspan": 2}, None, {"rowspan": 3}],
            [{"rowspan": 2, "l": 0.02, "t": 0.01}, {}, None],
            [None, None, None],
        ],
        shared_xaxes="columns",
        start_cell="bottom-left",
    )
    out["subplots-titles"] = make_subplots(
        rows=2,
        cols=2,
        subplot_titles=["a", "b", "c", "d"],
        column_titles=["left", "right"],
        row_titles=["top", "bottom"],
        x_title="x",
        y_title="y",
        column_widths=[1, 2],
        row_heights=[2, 1],
        horizontal_spacing=0.1,
    )
    out["subplots-heights-bottom-left"] = make_subplots(
        rows=2, cols=1, row_heights=[3, 1], start_cell="bottom-left", vertical_spacing=0.05, column_titles=["only"]
    )
    f = make_subplots(rows=2, cols=2)
    f.add_trace(ys([1, 2]), row=1, col=1)
    f.add_trace(ys([2, 1]), row=2, col=2)
    f.add_hline(y=1.5, annotation_text="limit")
    f.add_vrect(x0=0, x1=1, annotation_text="zone", annotation_position="outside top", fillcolor="green")
    f.add_vline(x=0.5, col=2, row="all", exclude_empty_subplots=False, annotation_text="now", annotation_position="bottom left")
    out["shapes-grid"] = f
    f = go.Figure([ys([1, 2])])
    f.add_hrect(y0=1, y1=2, annotation_text="band", annotation_position="inside left")
    f.add_vline(x="2024-01-02", annotation_text="day")
    f.add_vrect(x0="2024-01-01", x1="2024-01-08", annotation_text="week", annotation_position="top")
    out["shapes-single"] = f
    return out


def same(a, b, path):
    if isinstance(a, dict) and isinstance(b, dict):
        ok = True
        for k in sorted(set(a) | set(b)):
            if k not in a or k not in b:
                print(f"  {path}.{k}: only in {'plotly.py' if k in a else 'plotlyhs'}: {a.get(k, b.get(k))!r}")
                ok = False
            else:
                ok = same(a[k], b[k], f"{path}.{k}") and ok
        return ok
    if isinstance(a, list) and isinstance(b, list):
        if len(a) != len(b):
            print(f"  {path}: {len(a)} items in plotly.py, {len(b)} in plotlyhs")
            return False
        return all([same(x, y, f"{path}[{i}]") for i, (x, y) in enumerate(zip(a, b))])
    if isinstance(a, (int, float)) and isinstance(b, (int, float)) and not isinstance(a, bool) and not isinstance(b, bool):
        if math.isclose(a, b, rel_tol=0, abs_tol=1e-9):
            return True
    elif a == b:
        return True
    print(f"  {path}: plotly.py {a!r}, plotlyhs {b!r}")
    return False


def main():
    failed = []
    for name, fig in figures().items():
        expected = json.loads(pio.to_json(fig))
        expected.pop("config", None)
        with open(f"test/golden/figures/{name}.json", encoding="utf-8") as f:
            actual = json.load(f)
        print(name)
        if not same(expected, actual, ""):
            failed.append(name)
    print()
    make_subplots(rows=2, cols=2, print_grid=True)
    make_subplots(
        rows=3,
        cols=2,
        specs=[[{"colspan": 2, "secondary_y": True}, None], [{"type": "scene", "rowspan": 2}, {"type": "pie"}], [None, None]],
        print_grid=True,
    )
    if failed:
        sys.exit(f"differ from plotly.py: {', '.join(failed)}")
    print("all figures match plotly.py")


if __name__ == "__main__":
    main()
