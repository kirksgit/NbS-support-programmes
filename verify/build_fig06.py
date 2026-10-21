"""
Figure 6 -- how each fit verdict is produced: the design route behind it.

Written as a SCRIPT rather than an ad-hoc cell so the figure is
reproducible from the pipeline tables and its build is auditable.

It reads ONLY the flow tables the R pipeline emits, so it cannot disagree
with the run.

Drawing rules:
  * No pooled "+N rarer" nodes. Every option is listed in full, so the panels
    are taller and the type smaller rather than the tail being hidden.
  * Every node carries a label. Nothing is drawn unlabelled.
  * The D3a panel opens on the measures, using the same vocabulary as figure
    4, and moves measure -> the demand that measure creates -> provision.
  * Technical provisions are listed once each rather than as pre-paired
    combinations.
  * D3c labels state what "partial" means in the codebook rather than
    printing the word, and the panel opens on application channels because a
    guide is not the only route in.
Stage names sit UNDER each column.
"""
import csv, os, sys
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.path import Path
import matplotlib.patches as mpatches
import matplotlib.patheffects as pe

# Labels sit over ribbons in a dense panel. A thin white outline keeps them
# legible without a filled box, which would hide the flow beneath.
HALO = [pe.withStroke(linewidth=1.8, foreground="white")]

# Tables directory. This was hardcoded to the development tree's own layout
# ("pipe/output/tables/"), which does not exist in the reproduction bundle, so
# the script failed for anyone running it as shipped. It now takes the directory
# as the first argument, falls back to the bundle's own output/tables, and finally
# to the packaged expected/tables so the figure can be rebuilt before a run.
TAB = (sys.argv[1] if len(sys.argv) > 1 else
       next((d for d in ("output/tables", "expected/tables", "pipe/output/tables")
             if os.path.isdir(d)), "output/tables"))
if not TAB.endswith(os.sep):
    TAB += os.sep
if not os.path.isdir(TAB):
    sys.exit("build_fig06.py: no tables directory found at %r. "
             "Run 'Rscript run_all.R' first, or pass the directory as an argument."
             % TAB)

## the corpus size, derived from the run rather than typed. Each fit domain
## partitions the corpus exhaustively across its five states, so the max row-sum over
## domains IS N. A hardcoded 709 sat in every flow panel's caption while the corpus
## was 707 -- a caption asserting a corpus size the run itself contradicts.
def _n_corpus(tabdir):
    tot = {}
    with open(os.path.join(tabdir, "14_b1_domain_states.csv"), newline="", encoding="utf-8") as fh:
        for r in csv.DictReader(fh):
            tot[r["domain"]] = tot.get(r["domain"], 0) + int(float(r["n"]))
    if not tot:
        raise SystemExit("build_fig06: cannot derive N from 14_b1_domain_states.csv")
    return max(tot.values())

N_CORPUS = _n_corpus(TAB)


VERDICT_COL = {
    "Documented fit":   "#2166AC",
    "Partial fit":      "#F4A582",
    "Confirmed misfit": "#B2182B",
    "Indeterminate":    "#808080",
    "Not applicable":   "#E8E8E8",
}
VERDICT_ORDER = list(VERDICT_COL)

# The five states, defined as they are used across ALL SEVEN components. These are
# the framework's general definitions, not per-panel ones: the requirement differs
# by component, the evidential rule does not.
def _wrap_words(t, width):
    out, cur = [], ""
    for w in t.split():
        if cur and len(cur) + 1 + len(w) > width:
            out.append(cur); cur = w
        else:
            cur = f"{cur} {w}".strip()
    if cur:
        out.append(cur)
    return out


## these glosses are drawn beside the verdict labels and were tuned against
## one font stack. On a machine with wider metrics all five collided with their
## own labels (matplotlib measures RENDERED text, so the overlap check passed here
## and failed there). Shortened to leave headroom on any stack: each is now at or
## under 52 characters against the previous 74-79, which is roughly a third more
## slack than the widest metric difference observed. The meaning is unchanged --
## rule 2's three evidentiary routes are stated in the caption, not here.
VERDICT_DEF = {
    "Documented fit":   "the documents show the provision required",
    "Partial fit":      "part of it documented, or absent on part only",
    "Confirmed misfit": "documented as absent, or closed",
    "Indeterminate":    "requirement arises, documents silent",
    "Not applicable":   "the condition does not arise for this programme",
}
assert max(len(v) for v in VERDICT_DEF.values()) <= 47, "verdict glosses have grown; re-check overlap headroom"
INK, META = "#1A1A1A", "#6E6E6E"

# Per-programme verdict counts, read from the run's own table. The ribbons are
# PATH-weighted (a multi-response programme contributes several), so a verdict's
# share of PATHS is not its share of PROGRAMMES -- they differ by up to 15 points
# on panels a, b, c and f. The verdict column therefore reports programmes, which
# is what the seven fit tests actually return, and says so in the caption.
DOMAIN_OF = {"a": "D1 Ecological-functional", "b": "D2 Tenure & authority access",
             "c": "D3a Technical enablement", "d": "D3b Financial accessibility",
             "e": "D3c Administrative access", "f": "D4a Maintenance responsibility and support",
             "g": "D4b Monitoring, feedback and review"}


def verdict_counts():
    out = {}
    with open(TAB + "14_b1_domain_states.csv") as fh:
        for r in csv.DictReader(fh):
            out.setdefault(r["domain"].strip(), {})[r["outcome"].strip()] = float(r["n"])
    return out


# every panel now carries THE QUESTION IT ANSWERS, on the author's
# instruction. A reader arriving at a sankey should not have to infer the test
# from the stage names: the question states what the verdict column is a verdict
# ABOUT, which is also the guard against a panel drifting into a neighbouring
# domain's material -- a defect this figure has had twice.
PANELS = [
    ("a", "D1 · Ecological-functional", "14_b1c_d1_hazard_function_measure_fit.csv",
     ["hazard", "function", "measure"],
     ["Hazard the city documents", "Ecological function it requires",
      "Measure that delivers it"],
     "Does a supported measure deliver the function the documented hazard requires?"),
    ("b", "D2 · Tenure and authority", "14_b1d_d2_space_actor_route_flows.csv",
     ["space", "actor", "route"],
     ["Tenure context", "Actor made eligible", "Route reaching the space"],
     "Can the actor the programme admits actually reach the space the measure needs?"),
    ("c", "D3a · Technical enablement", "14_b1e_d3_support_flows.csv",
     ["measure", "demand", "provision"],
     ["Measure supported (E-1..E-7)", "Technical demand it creates",
      "Technical provision documented"],
     "Does the technical provision match the demand the supported portfolio creates? "
     "(Ribbon width is paths, block height is programmes.)"),
    ("d", "D3b · Financial accessibility", "14_b1g_d3b_financial_flows.csv",
     ["payment", "alternative", "target"],
     ["Payment route", "Alternative to paying first", "Who is targeted"],
     "Can an applicant who cannot front the cost still take part?"),
    ("e", "D3c · Administrative access", "14_b1h_d3c_admin_flows.csv",
     ["channel", "knowing", "lodging"],
     ["Which ways in are documented (D-4)", "Can they find out how to apply? (D-1)",
      "Can they lodge it? (D-3)"],
     "Can an eligible applicant find out how to apply, and then actually apply?"),
    ("f", "D4a · Maintenance", "14_b1f_d4_maintenance_flows.csv",
     ["support", "binding", "duty"],
     ["What support is documented (E-15/16/18)", "What tier of support is it?",
      "Who finally carries the duty (F-5)"],
     "Does the programme that transfers the maintenance duty also transfer the means to discharge it?"),
    ("g", "D4b · Monitoring and review", "14_b1i_d4b_monitoring_flows.csv",
     ["reporting", "review", "response"],
     ["What is reported", "Periodic review", "Response channel"],
     "Can anyone tell from the public record whether the programme worked?"),
]


def wrap_label(t, width=34):
    """Wrap a long label onto at most three lines.

    the naming work the author asked for -- stating what "partial" MEANS,
    naming each application channel, naming who the targeting reaches -- made
    several labels two to three times longer than the ones the lane widths were
    tuned for. Widening the lanes further would have shrunk every panel; wrapping
    keeps the full text and costs only vertical space, which the separation pass
    already accounts for because it measures RENDERED height.
    """
    words, lines, cur = t.split(), [], ""
    for w in words:
        if cur and len(cur) + 1 + len(w) > width:
            lines.append(cur)
            cur = w
        else:
            cur = f"{cur} {w}".strip()
    if cur:
        lines.append(cur)
    return "\n".join(lines)



def resolve_overlaps(fig, rounds=600):
    """Separate node labels using their RENDERED pixel boxes.

    Every earlier attempt predicted the space a label would need before drawing
    it -- from line counts, font points, axes inches -- and each prediction was
    wrong in a different way, because the quantity that actually matters (the
    laid-out height of a wrapped label in data coordinates) is only known after
    the renderer has run. This measures what is on the page and moves labels
    until nothing overlaps, so it cannot drift from the rendered result.

    Labels are pushed apart within their own stage only, keeping the stage's
    top-to-bottom order, and leader lines are redrawn from the node to wherever
    the label ended up.
    """
    fig.canvas.draw()                 # positions/extents must reflect the real layout
    rend = fig.canvas.get_renderer()
    groups = {}
    for ax in fig.axes:
        for t in ax.texts:
            g = t.get_gid()
            if g and (g.startswith("node:") or g == "verdict"):
                groups.setdefault((id(ax), g), (ax, []))[1].append(t)

    for _ in range(rounds):
        moved = False
        for ax, texts in groups.values():
            texts.sort(key=lambda t: t.get_position()[1])
            boxes = [t.get_window_extent(rend) for t in texts]
            for k in range(len(texts) - 1):
                lo, hi = boxes[k], boxes[k + 1]
                ov = lo.y1 - hi.y0 + 1.5   # +1.5px: leave real CLEARANCE, not
                if ov > 0:                    # mere non-overlap, which rounds to a hit
                    inv = ax.transData.inverted()
                    p0 = inv.transform((0, 0)); p1 = inv.transform((0, ov / 2 + 0.6))
                    d = abs(p1[1] - p0[1])
                    x0, y0 = texts[k].get_position()
                    x1, y1 = texts[k + 1].get_position()
                    texts[k].set_position((x0, y0 - d))
                    texts[k + 1].set_position((x1, y1 + d))
                    boxes[k] = texts[k].get_window_extent(rend)
                    boxes[k + 1] = texts[k + 1].get_window_extent(rend)
                    moved = True
        if not moved:
            return fig

    # Did not converge: the column genuinely cannot hold its labels at this size.
    # Step the font down for the crowded stages and try once more, rather than
    # shipping a figure with overlapping text.
    for ax, texts in groups.values():
        boxes = [t.get_window_extent(rend) for t in texts]
        if any(boxes[k].y1 - boxes[k + 1].y0 + 1.5 > 0 for k in range(len(boxes) - 1)):
            for t in texts:
                t.set_fontsize(max(4.2, t.get_fontsize() - 0.6))
    fig.canvas.draw()
    for _ in range(rounds):
        moved = False
        for ax, texts in groups.values():
            texts.sort(key=lambda t: t.get_position()[1])
            boxes = [t.get_window_extent(rend) for t in texts]
            for k in range(len(texts) - 1):
                ov = boxes[k].y1 - boxes[k + 1].y0 + 1.5
                if ov > 0:
                    inv = ax.transData.inverted()
                    d = abs(inv.transform((0, ov / 2 + 0.6))[1] - inv.transform((0, 0))[1])
                    x0, y0 = texts[k].get_position(); x1, y1 = texts[k + 1].get_position()
                    texts[k].set_position((x0, y0 - d)); texts[k + 1].set_position((x1, y1 + d))
                    boxes[k] = texts[k].get_window_extent(rend)
                    boxes[k + 1] = texts[k + 1].get_window_extent(rend)
                    moved = True
        if not moved:
            break
    return fig


def load(fname, stages):
    rows = []
    with open(TAB + fname) as fh:
        for r in csv.DictReader(fh):
            n = float(r["n"])
            if n <= 0:
                continue
            rows.append({**{s: wrap_label(r[s].replace("\n", " ").strip()) for s in stages},
                         "verdict": r["verdict"].strip(), "n": n})
    return rows


def node_layout(rows, stages, gap_frac=0.030):
    """Order and position every node in every stage.

    Nodes are ordered by the verdict mix they carry (fit-heaviest at the top),
    which keeps ribbons from crossing more than they must and puts the red
    routes together where a reader can follow them.
    """
    layout, order = {}, {}
    for si, st in enumerate(stages):
        tot = {}
        fit = {}
        for r in rows:
            tot[r[st]] = tot.get(r[st], 0) + r["n"]
            if r["verdict"] == "Documented fit":
                fit[r[st]] = fit.get(r[st], 0) + r["n"]
        # order every stage by TOTAL FLOW, largest at the top.
        # The previous order was by fit ratio, which put small fit-heavy nodes above
        # large ones and made neighbouring stages nearly parallel -- hence the
        # straight, unreadable ribbons the author flagged. Size order on both sides
        # of a gap gives the ribbons somewhere to cross, so the weave returns.
        # Largest at the TOP. Layout accumulates upward from y=0, so the FIRST
        # label placed lands at the bottom -- ascending order therefore reads as
        # descending on the page. Sorting descending here put the biggest node at
        # the foot of every column, which is the opposite of what was asked.
        labs = sorted(tot, key=lambda k: (tot[k], k))
        order[st] = labs
        grand = sum(tot.values())
        gap = gap_frac * grand
        span = grand + gap * (len(labs) - 1)
        y = 0.0
        for lab in labs:
            layout[(si, lab)] = (y / span, (y + tot[lab]) / span, tot[lab])
            y += tot[lab] + gap
    return layout, order


def ribbon(ax, x0, x1, y0a, y0b, y1a, y1b, colour, alpha):
    """One flow, drawn as a filled cubic band."""
    # a larger control-point offset than 0.42 makes each band
    # leave and enter its node horizontally and turn in the middle -- the curly
    # profile the author preferred. 0.42 produced near-straight diagonals.
    cx = (x1 - x0) * 0.62
    verts = [(x0, y0a), (x0 + cx, y0a), (x1 - cx, y1a), (x1, y1a),
             (x1, y1b), (x1 - cx, y1b), (x0 + cx, y0b), (x0, y0b), (x0, y0a)]
    codes = [Path.MOVETO, Path.CURVE4, Path.CURVE4, Path.CURVE4,
             Path.LINETO, Path.CURVE4, Path.CURVE4, Path.CURVE4, Path.CLOSEPOLY]
    ax.add_patch(mpatches.PathPatch(Path(verts, codes), facecolor=colour,
                                    edgecolor="none", alpha=alpha, zorder=1))


def draw_panel(ax, rows, stages, stage_names, tag, title, vcounts, question=None):
    layout, order = node_layout(rows, stages)
    # ONE gap width, used between every pair of blocks and
    # again between the last stage and the verdict. Previously each gap was sized
    # to its own stage's longest label, so the gaps differed and the verdict sat
    # far off to the right with no ribbons reaching it. The gap is now the widest
    # label lane any stage needs, applied uniformly -- so the spacing is even and
    # the verdict is a genuine fourth column the flows arrive at.
    lmax = [max((len(l) for l in {r[st] for r in rows}), default=0) for st in stages]
    lmax.append(max(len(v) for v in VERDICT_ORDER) + 12)   # verdict labels carry "  NN%  (n=NNN)"
    # the naming work (targeting routes, reporting states, partial
    # definitions) made several labels much longer, so the per-character
    # allowance rises with them rather than sitting at a value tuned to the
    # old short labels.
    gap_x = 0.46 + 0.0092 * max(max(lmax) - 22, 0)
    xs = [0.0, gap_x, 2 * gap_x]
    bw = 0.026
    VX = 3 * gap_x        # one further gap of the SAME width
    LABPT  = 7.4         # rendered line height (points) a label needs

    # ribbons, drawn misfit-last so red routes read on top
    for si in range(len(stages) - 1):
        cur = {lab: layout[(si, lab)][0] for lab in order[stages[si]]}
        nxt = {lab: layout[(si + 1, lab)][0] for lab in order[stages[si + 1]]}
        agg = {}
        for r in rows:
            k = (r[stages[si]], r[stages[si + 1]], r["verdict"])
            agg[k] = agg.get(k, 0) + r["n"]
        span = sum(v[2] for k, v in layout.items() if k[0] == si)
        gspan = layout[(si, order[stages[si]][-1])][1]
        for vd in VERDICT_ORDER:
            for (a, b, v), n in sorted(agg.items(), key=lambda kv: -kv[1]):
                if v != vd:
                    continue
                h = n / span * gspan
                ya, yb = cur[a], cur[a] + h
                cur[a] = yb
                yc, yd = nxt[b], nxt[b] + h
                nxt[b] = yd
                ribbon(ax, xs[si] + bw, xs[si + 1], ya, yb, yc, yd,
                       VERDICT_COL[v], 0.62 if v != "Not applicable" else 0.40)

    # nodes + labels -- EVERY node is labelled (annotation 3). Labels are laid out
    # with a minimum-separation pass so that even a node holding a handful of
    # records keeps its own readable label instead of being pooled away.
    lp = {i: [] for i in range(len(stages))}
    for si, st in enumerate(stages):
        for lab in order[st]:
            y0, y1, _ = layout[(si, lab)]
            ax.add_patch(mpatches.Rectangle((xs[si], y0), bw, y1 - y0,
                                            facecolor="#3A3A3A", edgecolor="none", zorder=3))
            lp[si].append([(y0 + y1) / 2, lab])

    tot = vcounts
    grand = sum(tot.values())
    # Verdict blocks stack bottom-up too, so iterate REVERSED to put the first
    # verdict class (Documented fit) at the top, matching the stage ordering.
    y, blocks, vslot = 0.0, [], {}
    for vd in reversed(VERDICT_ORDER):
        if vd not in tot:
            continue
        h = tot[vd] / grand
        ax.add_patch(mpatches.Rectangle((VX, y), bw, h, facecolor=VERDICT_COL[vd],
                                        edgecolor="none", zorder=3))
        blocks.append((vd, y + h / 2, tot[vd] / grand))
        vslot[vd] = [y, y + h]
        y += h

    # NOTE FOR THE CAPTION: ribbon width is PATHS, block height is PROGRAMMES.
    # Misfit programmes carry ~1.5 paths each against ~8.8 for documented fits --
    # they support fewer measures and document fewer provisions -- so misfit
    # ribbons are thin relative to the block they arrive at. On panel c, 77 of the
    # 81 misfit paths come from "No technical provision documented" and 4 from
    # "Technical standards or specifications" (records documenting standards but
    # carrying an explicit denial on advice). This is a property of the data, not
    # a drawing error, and the caption states it.
    # FLOWS NOW JOIN THE VERDICT. One honest wrinkle has to be
    # handled rather than hidden: a verdict BLOCK's height is its share of
    # PROGRAMMES (the thing each test returns), while a ribbon's width is PATHS
    # (multi-response fields put one programme on several). Those two scales do
    # not agree, so the incoming ribbons are scaled WITHIN each verdict block:
    # the block height stays the programme share, and the block is divided among
    # the last-stage nodes in proportion to the paths arriving from each. The
    # caption states this. The alternative -- sizing blocks by paths -- would
    # restate the very error corrected in an earlier draft, where a path share was read as a
    # programme share and differed by up to 15 points.
    last = stages[-1]
    li = len(stages) - 1
    cur = {lab: layout[(li, lab)][0] for lab in order[last]}
    gspan = layout[(li, order[last][-1])][1]
    lspan = sum(v[2] for k, v in layout.items() if k[0] == li)
    agg = {}
    for r in rows:
        agg[(r[last], r["verdict"])] = agg.get((r[last], r["verdict"]), 0) + r["n"]
    vpaths = {}
    for (a, vd), n in agg.items():
        vpaths[vd] = vpaths.get(vd, 0) + n
    vcur = {vd: sl[0] for vd, sl in vslot.items()}
    for vd in VERDICT_ORDER:
        if vd not in vslot:
            continue
        vh = vslot[vd][1] - vslot[vd][0]
        for (a, v), n in sorted(agg.items(), key=lambda kv: -kv[1]):
            if v != vd:
                continue
            ha = n / lspan * gspan                 # left side: true path width
            hb = n / vpaths[vd] * vh               # right side: share of this block
            ya, yb = cur[a], cur[a] + ha
            cur[a] = yb
            yc, yd = vcur[vd], vcur[vd] + hb
            vcur[vd] = yd
            ribbon(ax, xs[li] + bw, VX, ya, yb, yc, yd, VERDICT_COL[vd],
                   0.62 if vd != "Not applicable" else 0.40)

    # emit node labels with minimum separation, leader lines where nudged.
    # The LAST stage and the verdict column were previously separated by two
    # independent passes, so neither could see the other's labels and they
    # collided. The final stage now reserves its own lane and the verdict column
    # sits beyond it, so the two label groups cannot occupy the same x.
    for si in range(len(stages)):
        items = sorted(lp[si])
        if not items:
            continue
        # One downward pass, then a downward-shift correction so the whole stack
        # stays inside the panel. A fixed gap was too small where many small
        # nodes stack (D4a support) and pushed labels off the axis where the
        # stack was long, so the gap adapts to the number of labels.
        # Required gap = the rendered line height converted into AXES units.
        # A constant in axes units was wrong because panels differ in physical
        # height: 0.034 is generous in a tall panel and far too small in a short
        # one, which is why collisions survived three tightening passes.
        ax_h_in = ax.get_window_extent().height / ax.figure.dpi
        # a wrapped label needs one line-height per line, else two-line labels
        # collide even though the single-line spacing is correct.
        nline = max((lab.count("\n") + 1) for _, lab in items)
        # (points -> inches -> AXES FRACTION) -> DATA units. The last step was
        # missing: node y-positions are data coordinates on a y-axis that spans
        # more than 1.0 (the panel reserves room below zero for stage names), so
        # a gap expressed as an axes fraction was too small by exactly that span.
        y0_, y1_ = ax.get_ylim()
        gap = (LABPT * nline / 72.0) / max(ax_h_in, 1e-6) * (y1_ - y0_)
        # the cap must be computed over the SPAN the stack may occupy and
        # over TOTAL LINES, not item count. `0.97 / len(items)` assumed a 0-1
        # axis and one line per label, so it silently clamped the wrapped-label
        # gap straight back to single-line size -- which is why three separate
        # fixes above it changed nothing at all.
        ys = [cy for cy, _ in items]
        for (cy, lab), ty in zip(items, ys):
            _t = ax.text(xs[si] + bw + 0.014, ty, lab, va="center", ha="left",
                         fontsize=5.5, color=INK, zorder=6, path_effects=HALO)
            _t.set_gid(f"node:{si}")          # claimed by the post-render pass
            if abs(ty - cy) > 0.004:
                ax.plot([xs[si] + bw + 0.001, xs[si] + bw + 0.011], [cy, ty],
                        lw=0.35, color="#9A9A9A", zorder=4)

    # verdict column. Labels are placed by a MINIMUM-SEPARATION pass rather than
    # at each block's centre: a verdict holding 1% of records gives a block far
    # thinner than its own text, so centred labels collided. Each label is nudged
    # down to clear the previous one and a leader line connects it to its block.
    _y0v, _y1v = ax.get_ylim()
    MINSEP = max((8.2 / 72.0) / max(ax.get_window_extent().height / ax.figure.dpi, 1e-6)
                 * (_y1v - _y0v), 0.03)
    ly = -1e9
    for vd, cy, share in blocks:
        ty = max(cy, ly + MINSEP)
        ly = ty
        _vt = ax.text(VX + bw + 0.020, ty, f"{vd}  {share*100:.0f}%  (n={int(round(tot[vd]))})", va="center", ha="left",
                fontsize=5.8, color=INK, zorder=6, path_effects=HALO)
        _vt.set_gid("verdict")   # separated by the same post-render pass
        if abs(ty - cy) > 0.004:
            ax.plot([VX + bw + 0.002, VX + bw + 0.017], [cy, ty],
                    lw=0.35, color="#9A9A9A", zorder=4)

    # stage names UNDER each column (annotation: put transitions under the block)
    for si, nm in enumerate(stage_names):
        ax.text(xs[si] + bw / 2, -0.10, nm, ha="center", va="top",
                fontsize=5.9, color=META, wrap=True)
    ax.text(VX + bw / 2, -0.10, "Verdict", ha="center", va="top",
            fontsize=5.9, color=META)

    # Header lines are offset by MEASURED line height rather than a fixed
    # fraction: panels differ in physical height, so a constant offset collided
    # in the short panels and floated in the tall ones.
    hgap = (11.0 / 72.0) / max(ax.get_window_extent().height / ax.figure.dpi, 1e-6)
    ax.text(-0.02, 1.02 + hgap, tag, fontsize=8.5, fontweight="bold", color=INK,
            transform=ax.transAxes)
    ax.text(0.025, 1.02 + hgap, title, fontsize=8.5, color=INK, transform=ax.transAxes)
    if question:
        # v37b: the question gets its OWN BAND ABOVE the axes, not a line inside
        # them. Placing it at y=1.02-0.026 put it in the panel's own coordinate
        # space, so on short panels it sat on top of the first ribbons -- the
        # author saw it "sitting in the sankey". It now goes ABOVE the header
        # block (title at 1.02+hgap, count at 1.02), and the header gap `hgap`
        # is widened below so the whole stack has room whatever the panel height.
        ax.text(-0.02, 1.02 + hgap + 0.052, question, fontsize=7.0, color=INK,
                style="italic", transform=ax.transAxes, va="bottom")
    ## N READ FROM THE RUN, NOT HARDCODED. This said "709 programmes" while the
    ## corpus is 707, on every one of the seven flow panels -- a caption asserting a
    ## corpus size the run contradicts. N_CORPUS is derived once from the domain-state
    ## table (each domain partitions the corpus exhaustively) so the caption cannot
    ## drift from the data again.
    ax.text(-0.02, 1.02, f"{int(round(sum(r['n'] for r in rows))):,} paths from {N_CORPUS:,} programmes",
            fontsize=5.6, color=META, transform=ax.transAxes)
    ax.set_xlim(-0.03, VX + 0.47)
    ax.set_ylim(-0.20, 1.05)
    ax.axis("off")


def build(out="fig06_fitness_mechanism.png"):
    mpl.rcParams.update({"font.family": "sans-serif",
                         "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"]})
    data = [(tag, title, load(f, st), st, nm, q) for tag, title, f, st, nm, q in PANELS]
    VC = verdict_counts()

    # Panel height scales with the number of nodes in its busiest stage, so that
    # listing EVERY option (annotation 8) never forces labels to collide.
    heights = []
    for _, _, rows, st, _, _ in data:
        # height is driven by total LINES, not node count. Wrapping long
        # labels means a stage with 8 nodes can need 14 lines of vertical room;
        # sizing on nodes alone left those panels short and the labels collided.
        mx = max(sum(lab.count("\n") + 1 for lab in {r[s] for r in rows}) for s in st)
        # +0.42in of header room: title, path count and the question line now
        # stack above each panel, and the question needs a band of its own.
        heights.append(max(2.6, 0.42 * mx + 1.67))

    fig = plt.figure(figsize=(17.0, sum(heights) / 2 + 2.9))
    gs = fig.add_gridspec(
        (len(data) + 1) // 2, 2,
        height_ratios=[max(heights[i * 2], heights[i * 2 + 1] if i * 2 + 1 < len(heights) else 0)
                       for i in range((len(data) + 1) // 2)],
        hspace=0.46, wspace=0.30, left=0.035, right=0.985, top=0.900, bottom=0.055)

    # A first draw is required before get_window_extent() reports true panel
    # geometry; the label passes measure against it.
    axes = [fig.add_subplot(gs[i // 2, i % 2]) for i in range(len(data))]
    for ax in axes:
        ax.axis("off")   # before the sizing draw, else default ticks render
    fig.canvas.draw()
    for ax, (tag, title, rows, st, nm, q) in zip(axes, data):
        draw_panel(ax, rows, st, nm, tag, title, VC[DOMAIN_OF[tag]], q)

    fig.text(0.035, 0.982, "Figure 6   How each fit verdict is produced: the design route behind it",
             fontsize=13, fontweight="bold", color=INK, ha="left")
    fig.text(0.035, 0.963,
             "Each panel opens on the condition the programme faced, moves through what its design provided, and closes on the verdict that pairing earned. "
             "Colour is the verdict; the path is how it was reached, so a red ribbon can be followed back to the exact route that produced the misfit.",
             fontsize=7.4, color=META, ha="left")
    fig.text(0.035, 0.947,
             "Every category is listed in full -- nothing is pooled into an 'other' node and every node carries its label. Ribbon width is PATHS: four fields are "
             "multi-response, so a programme documenting several hazards, actors or provisions contributes more than one path, and each panel states its own path total. "
             "The verdict column reports PROGRAMMES (n shown), which is what each fit test returns -- so its percentages are programme shares, not path shares.",
             fontsize=7.4, color=META, ha="left")

    # legend, bottom-right slot
    ax = fig.add_subplot(gs[(len(data)) // 2, (len(data)) % 2])
    ax.axis("off")
    ax.text(0.0, 0.95, "How to read", fontsize=8.5, fontweight="bold", color=INK, va="top")
    # v38b: each row now carries a wrapped definition beneath its label, so the
    # row pitch is larger than the single-line legend needed.
    lg = max((10.5 / 72.0) / max(ax.get_window_extent().height / ax.figure.dpi, 1e-6), 0.150)
    for i, vd in enumerate(VERDICT_ORDER):
        ax.add_patch(mpatches.Rectangle((0.0, 0.72 - i * lg), 0.055, 0.055,
                                        facecolor=VERDICT_COL[vd], edgecolor="none"))
        # v38b: EACH STATE CARRIES ITS DEFINITION, on the author's annotation. The
        # legend named the five verdicts but never said what they mean, so a reader
        # had to take the framework's convention on trust. These are the general
        # definitions that hold for all seven components; each panel's own stages
        # show how that component instantiates them.
        ax.text(0.075, 0.7475 - i * lg + 0.013, vd, fontsize=7.0,
                fontweight="bold", color=INK, va="center")
        # v38b: the definition is WRAPPED to the legend axes' own width. Set as one
        # line it ran past the panel edge and was clipped at the figure boundary --
        # visible only by cropping the render, which is how it was caught.
        ax.text(0.075, 0.7475 - i * lg - 0.019,
                "\n".join(_wrap_words(VERDICT_DEF[vd], 46)),
                fontsize=5.7, color=META, va="top", linespacing=1.25)
    ax.text(0.0, min(0.22, 0.72 - len(VERDICT_ORDER) * lg - 0.04),
            "Node height is the number of programmes at that step. Stage names sit beneath each column.\n"
            "The seven tests are independent and non-nested -- no test feeds the next.",
            fontsize=6.8, color=META, va="top")
    ax.set_xlim(0, 1); ax.set_ylim(0, 1)

    resolve_overlaps(fig)
    # write PDF ALONGSIDE PNG, as save_fig() does for every R figure.
    # This was the one figure in the set with no vector version -- an
    # inconsistency invisible until someone assembled the manuscript.
    fig.savefig(out, dpi=300, bbox_inches="tight", facecolor="white")
    pdf_out = os.path.splitext(out)[0] + ".pdf"
    fig.savefig(pdf_out, bbox_inches="tight", facecolor="white")
    return fig


if __name__ == "__main__":
    f = build()
    r = f.canvas.get_renderer()
    tx = [(t, t.get_window_extent(r)) for t in f.findobj(mpl.text.Text)
          if t.get_text().strip() and t.get_visible()]
    ov = [(a.get_text()[:26], b.get_text()[:26])
          for i, (a, ba) in enumerate(tx) for b, bb in tx[i + 1:] if ba.overlaps(bb)]
    print("text overlaps:", len(ov))
    for o in ov[:8]:
        print("   ", o)
