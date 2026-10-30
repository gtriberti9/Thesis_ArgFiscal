"""
9. Empirical claims report (PDF).

Builds `empirical_claims_report.pdf`: a guided walk through the claims tested in
notebooks `8. empirical_claims.ipynb` (target r* = +2%) and
`8b. empirical_claims_negative_r.ipynb` (zero and negative targets).

Inputs (all written by notebooks 8 and 8b; run them first):
  data/interest_rates/*.csv            result tables
  data/interest_rates/figures/*.png    figures

Every number in the text is read from those CSVs, except the few values marked
"transcribed" below, which the notebooks print but do not save.

Requires pandas and reportlab (pip install reportlab).
"""
import os
import numpy as np
import pandas as pd
from reportlab.lib import colors
from reportlab.lib.enums import TA_LEFT
from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import ParagraphStyle
from reportlab.lib.units import inch
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.platypus import (Image, KeepTogether, PageBreak, Paragraph, SimpleDocTemplate, Spacer, Table,
                                TableStyle)

DATA = os.path.join("data", "interest_rates")
FIG = os.path.join(DATA, "figures")
OUT = "empirical_claims_report.pdf"

# ---------------------------------------------------------------- fonts and styles
FONT_DIR = os.path.join(os.environ.get("WINDIR", "C:/Windows"), "Fonts")
for name, f in [("Body", "calibri.ttf"), ("Body-Bold", "calibrib.ttf"), ("Body-Italic", "calibrii.ttf"),
                ("Body-BoldItalic", "calibriz.ttf")]:
    pdfmetrics.registerFont(TTFont(name, os.path.join(FONT_DIR, f)))
pdfmetrics.registerFontFamily("Body", normal="Body", bold="Body-Bold", italic="Body-Italic", boldItalic="Body-BoldItalic")

INK, INK2, MUTED = colors.HexColor("#0b0b0b"), colors.HexColor("#52514e"), colors.HexColor("#898781")
RULE, SHADE, BLUE, ORANGE = colors.HexColor("#c3c2b7"), colors.HexColor("#f0efec"), colors.HexColor("#2a78d6"), colors.HexColor("#eb6834")
ASSUME_BG, ASSUME_EDGE = colors.HexColor("#fdf6e3"), colors.HexColor("#eda100")
VERDICT_STYLE = {"supported": (colors.HexColor("#006300"), colors.white, "SUPPORTED"),
                 "not": (colors.HexColor("#b03030"), colors.white, "NOT SUPPORTED"),
                 "mixed": (colors.HexColor("#fab219"), INK, "DEPENDS ON THE TARGET"),
                 "fact": (colors.HexColor("#52514e"), colors.white, "FINDING")}

ST = {
    "title": ParagraphStyle("title", fontName="Body-Bold", fontSize=22, leading=27, textColor=INK, spaceAfter=6),
    "subtitle": ParagraphStyle("subtitle", fontName="Body", fontSize=12.5, leading=16, textColor=INK2, spaceAfter=4),
    "h1": ParagraphStyle("h1", fontName="Body-Bold", fontSize=16, leading=20, textColor=INK, spaceBefore=6, spaceAfter=8, keepWithNext=1),
    "h2": ParagraphStyle("h2", fontName="Body-Bold", fontSize=12.5, leading=16, textColor=INK, spaceBefore=10, spaceAfter=4, keepWithNext=1),
    "body": ParagraphStyle("body", fontName="Body", fontSize=10.5, leading=14.2, textColor=INK, spaceAfter=5),
    "bullet": ParagraphStyle("bullet", fontName="Body", fontSize=10.5, leading=14, textColor=INK, leftIndent=14,
                             bulletIndent=3, spaceAfter=2.5),
    "small": ParagraphStyle("small", fontName="Body", fontSize=9, leading=11.5, textColor=INK2, spaceAfter=3),
    "caption": ParagraphStyle("caption", fontName="Body", fontSize=9.2, leading=12, textColor=INK2, spaceBefore=3, spaceAfter=10),
    "cell": ParagraphStyle("cell", fontName="Body", fontSize=8.8, leading=10.8, textColor=INK, alignment=TA_LEFT),
    "cellb": ParagraphStyle("cellb", fontName="Body-Bold", fontSize=8.8, leading=10.8, textColor=INK),
    "claim": ParagraphStyle("claim", fontName="Body-Bold", fontSize=11, leading=14.5, textColor=INK),
    "claimlab": ParagraphStyle("claimlab", fontName="Body-Bold", fontSize=8.5, leading=10, textColor=MUTED),
    "verdict": ParagraphStyle("verdict", fontName="Body-Bold", fontSize=8.5, leading=10, textColor=colors.white),
    "assume": ParagraphStyle("assume", fontName="Body", fontSize=9.2, leading=12, textColor=INK),
}
W = 6.8 * inch                       # text width


def P(text, style="body"):
    return Paragraph(text, ST[style])


def bullets(items):
    return [Paragraph(t, ST["bullet"], bulletText="•") for t in items]


def claim_box(label, statement, verdict):
    bg, fg, word = VERDICT_STYLE[verdict]
    chip = Table([[Paragraph(word, ParagraphStyle("v", parent=ST["verdict"], textColor=fg))]],
                 style=[("BACKGROUND", (0, 0), (-1, -1), bg), ("LEFTPADDING", (0, 0), (-1, -1), 5),
                        ("RIGHTPADDING", (0, 0), (-1, -1), 5), ("TOPPADDING", (0, 0), (-1, -1), 2),
                        ("BOTTOMPADDING", (0, 0), (-1, -1), 3)])
    head = Table([[P(label.upper(), "claimlab"), chip]], colWidths=[W - 1.85 * inch, 1.6 * inch],
                 style=[("ALIGN", (1, 0), (1, 0), "RIGHT"), ("VALIGN", (0, 0), (-1, -1), "MIDDLE"),
                        ("LEFTPADDING", (0, 0), (-1, -1), 0), ("RIGHTPADDING", (0, 0), (-1, -1), 0)])
    t = Table([[head], [P(statement, "claim")]], colWidths=[W - 0.1 * inch])
    t.setStyle(TableStyle([("BACKGROUND", (0, 0), (-1, -1), SHADE), ("LINEBEFORE", (0, 0), (0, -1), 3, INK2),
                           ("LEFTPADDING", (0, 0), (-1, -1), 10), ("RIGHTPADDING", (0, 0), (-1, -1), 8),
                           ("TOPPADDING", (0, 0), (-1, -1), 5), ("BOTTOMPADDING", (0, -1), (-1, -1), 8)]))
    gap = Spacer(1, 6)
    t.keepWithNext = gap.keepWithNext = True       # never strand a claim at the foot of a page
    return [Spacer(1, 4), t, gap]


def assume_box(text):
    t = Table([[Paragraph("<b>Assumptions used here.</b> " + text, ST["assume"])]], colWidths=[W - 0.1 * inch])
    t.setStyle(TableStyle([("BACKGROUND", (0, 0), (-1, -1), ASSUME_BG), ("LINEBEFORE", (0, 0), (0, -1), 3, ASSUME_EDGE),
                           ("LEFTPADDING", (0, 0), (-1, -1), 10), ("RIGHTPADDING", (0, 0), (-1, -1), 8),
                           ("TOPPADDING", (0, 0), (-1, -1), 5), ("BOTTOMPADDING", (0, 0), (-1, -1), 6)]))
    return [Spacer(1, 2), t, Spacer(1, 8)]


FIG_N = [0]


def figure(name, caption, how, width=W):
    FIG_N[0] += 1
    path = os.path.join(FIG, f"{name}.png")
    from reportlab.lib.utils import ImageReader
    iw, ih = ImageReader(path).getSize()
    img = Image(path, width=width, height=width * ih / iw)
    cap = P(f"<b>Figure {FIG_N[0]}.</b> {caption} <i>How to read it:</i> {how}", "caption")
    return [KeepTogether([img, cap])]


TAB_N = [0]


def table(header, rows, widths, caption=None, num_cols=None, highlight_rows=()):
    TAB_N[0] += 1
    data = [[Paragraph(f"<b>{h}</b>", ST["cell"]) for h in header]]
    data += [[Paragraph(str(c), ST["cell"]) for c in r] for r in rows]
    t = Table(data, colWidths=widths, repeatRows=1)
    style = [("LINEABOVE", (0, 0), (-1, 0), 0.8, INK), ("LINEBELOW", (0, 0), (-1, 0), 0.6, INK2),
             ("LINEBELOW", (0, -1), (-1, -1), 0.8, INK), ("VALIGN", (0, 0), (-1, -1), "MIDDLE"),
             ("TOPPADDING", (0, 0), (-1, -1), 2.5), ("BOTTOMPADDING", (0, 0), (-1, -1), 3),
             ("LEFTPADDING", (0, 0), (-1, -1), 4), ("RIGHTPADDING", (0, 0), (-1, -1), 4)]
    for i in range(1, len(data)):
        if i % 2 == 0:
            style.append(("BACKGROUND", (0, i), (-1, i), colors.HexColor("#f7f7f5")))
    for i in highlight_rows:
        style.append(("BACKGROUND", (0, i + 1), (-1, i + 1), colors.HexColor("#fdf0e8")))
    t.setStyle(TableStyle(style))
    out = [t]
    if caption:
        out.append(P(f"<b>Table {TAB_N[0]}.</b> {caption}", "caption"))
    return [KeepTogether(out)] if len(rows) <= 12 else out      # long tables may split (header repeats)


def f(x, d=3, sign=False):
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return "–"
    s = f"{x:+.{d}f}" if sign else f"{x:.{d}f}"
    return s.replace("-", "−")


def pct(x, d=1, sign=False):
    return f(x, d, sign) + "%"


# ---------------------------------------------------------------- load results
def rd(name, **kw):
    return pd.read_csv(os.path.join(DATA, name), **kw)


rates_sum = rd("argentina_real_rate_summary.csv")
rho2 = rd("calibration_rho_rstar2.csv", index_col=0)
rho_tg = rd("calibration_rho_by_target.csv", index_col=0)
mom = rd("calibrated_moments_eta042.csv", index_col=0)
cf = rd("counterfactual_eta_calibrated.csv")
mech = rd("mechanism_bonds_by_state.csv")
LAM4 = pd.read_csv(os.path.join("data", "lambda_matrix_4state_datadriven.csv"), index_col=0)   # 4-state generator, eta = 0.42
scan = rd("bond_demand_scan_rstar2.csv")
free2 = rd("calibration_amin_theta_rstar2.csv")
neg = rd("calibration_rho_negative_targets.csv")
gap = rd("rho_and_gap_by_target.csv", index_col=0)
unt = rd("untargeted_moments_by_target.csv")
free_all = rd("calibration_amin_theta_by_target.csv")
low = rd("bond_demand_low_rho_median_1993_2025.csv")
bdebt = rd("benchmark_debt_income.csv", index_col="year")          # notebook 8, Step 11b
bprem = rd("benchmark_informal_premium.csv")
bfacts = pd.read_csv(os.path.join(DATA, "benchmarks", "bcra_household_credit_facts.csv"), index_col="item")["value"]


def win(w, series="short-term rate"):
    return rates_sum[(rates_sum["window"] == w) & (rates_sum["series"] == series)].iloc[0]


R50, R93, R91, R02 = win("1976-2025"), win("1993-2025"), win("1991-2001"), win("2002-2025")
MED93, MED50 = R93["real median %"], R50["real median %"]

RHO2S, RHO4S = rho2.loc["2-state", "calibrated rho"], rho2.loc["4-state", "calibrated rho"]
BETA2S, BETA4S = rho2.loc["2-state", "annual discount factor beta = exp(-rho)"], rho2.loc["4-state", "annual discount factor beta = exp(-rho)"]
QB4S = rho2.loc["4-state", "quarterly beta"]
GAP_RHO = (rho_tg["4-state"] - rho_tg["2-state"])

# Transcribed from notebook outputs (printed, not saved to CSV); see notebook and step in brackets
R_AT_RHO05 = {"2-state": 1.587, "4-state": -0.764}            # [nb 8, Step 3]
RHO_GAMMA027, RHO2S_ETA20, RHO4S_ETA20 = 0.0498, 0.0393, 0.0600   # [nb 8, Step 8]
REPLICATION = [("2-state, Section 2, γ<sub>I</sub> = 0.27, η = 0.42", 0.020229, 4.9e-7),
               ("2-state, Section 2, γ<sub>I</sub> = 0.27, η = 0.20", 0.033046, 2.6e-7),
               ("4-state, Section 1 (common τ, γ, θ = 0), η = 0.42", 0.018977, 1.1e-7),
               ("4-state, Section 2, η = 0.20", 0.010061, 1.5e-7)]   # [nb 8, Step 2]
GRID_CHECK = [("+2%", "2-state", 20, -0.1271, -0.1274), ("+2%", "4-state", 44, -0.2860, -0.2846),
              ("0%", "2-state", 6, -0.0377, -0.0390), ("0%", "4-state", 21, -0.1296, -0.1294),
              ("−1%", "2-state", 4, -0.0191, -0.0206), ("−1%", "4-state", 15, -0.0913, -0.0915),
              ("−2%", "4-state", 11, -0.0653, -0.0655), ("−3.56%", "4-state", 7, -0.0388, -0.0392),
              ("−5.12%", "4-state", 4, -0.0227, -0.0235)]               # [nb 8b, Step 5 resolution check]

cf_d = (cf[cf.eta == 0.20].set_index("model")[["r* %", "borrowers mass", "mass at limit"]]
        - cf[cf.eta == 0.42].set_index("model")[["r* %", "borrowers mass", "mass at limit"]])

m2 = mech.set_index(["model", "state"])
S_TOT = mech.groupby("model")["net bonds (contribution to S)"].sum()

fr2 = free2.set_index(["model", "free parameter"])
GAP2S = gap["2-state: rho - r*"].mean()
GAP4S = gap["4-state: rho - r*"].mean()

# Verdict logic: same pass rules as notebooks 8/8b (RHO_RANGE, theta >= 0, a_min attainable and cleared)
RHO_RANGE = (0.02, 0.12)
calib = pd.concat([neg.assign(test="rho").rename(columns={"rho": "value"}),
                   free_all.assign(test=free_all["free parameter"]).rename(columns={"calibrated value": "value"})],
                  ignore_index=True)


def passes(test, x):
    if x["status"] != "ok" or str(x["cleared"]) != "True":
        return False
    if test == "rho":
        return RHO_RANGE[0] <= x["value"] <= RHO_RANGE[1]
    if test == "theta_I":
        return x["value"] >= 0
    return True


def verdict(test, t):
    d = calib[(calib["test"] == test) & np.isclose(calib["r* target %"], t)].set_index("model")
    p2, p4 = (passes(test, d.loc[m]) if m in d.index else False for m in ("2-state", "4-state"))
    return {(True, True): "both pass", (False, True): "<b>4-state only</b>", (True, False): "<b>2-state only</b>",
            (False, False): "neither"}[(p2, p4)]


TARGETS = sorted(calib["r* target %"].unique(), reverse=True)
TLAB = {2.0: "+2% (advisor)", 0.0: "0%", -1.0: "−1%", -2.0: "−2%"}
tlab = lambda t: TLAB.get(round(t, 6), f"{f(t, 2)}% ({'1993–2025' if np.isclose(t, MED93) else '1976–2025'} median)")


def fa(model, t, which, col="calibrated value"):
    r = free_all[(free_all["model"] == model) & np.isclose(free_all["r* target %"], t) & (free_all["free parameter"] == which)].iloc[0]
    return r if col is None else r[col]


def rho_at(model, t):
    r = neg[(neg["model"] == model) & np.isclose(neg["r* target %"], t)].iloc[0]
    return r["rho"]


# Data benchmarks: same central values and ranges as notebook 8, Step 11b
DI = bdebt["debt / income"]
DI_MED, DI_LO, DI_HI = DI.median(), DI.min(), bdebt["debt / income incl. non-bank lenders"].max()
DI_HI_BANK, DI_LAST, DI_LAST_Y = DI.max(), DI.iloc[-1], DI.index[-1]
DI_CONV = DI.loc[1994:2001]
PR_ALL = bprem[bprem["group"] == "all"].set_index("date")
PR_MED, PR_LO, PR_HI = PR_ALL["real premium pp"].median(), bprem["real premium pp"].min(), PR_ALL["real premium pp"].max()
PR_G = bprem[bprem["group"] != "all"].set_index("group")["real premium pp"]
NONBANK = bfacts["non-bank (PNFC) credit to individuals / bank (EEFF) credit to individuals"]
MONTH = {"diciembre": "Dec", "noviembre": "Nov", "junio": "Jun"}
report_of = lambda src, page: "Report of " + " ".join(
    MONTH.get(w, w) for w in src.split("Crédito, ")[1].split(" (")[0].split()) + f", p. {page}"


def realistic(test, t):
    # J4: a requirement is realistic if the calibration passes and the implied value lies in the observed range
    out = []
    for m in ("2-state", "4-state"):
        r = fa(m, t, test, None)
        if not (r["status"] == "ok" and str(r["cleared"]) == "True"):
            out.append(False); continue
        v = r["gross debt / income"] if test == "a_min" else 100 * r["calibrated value"]
        lo, hi = (DI_LO, DI_HI) if test == "a_min" else (PR_LO, PR_HI)
        out.append(lo <= v <= hi)
    return {(True, True): "both", (False, True): "<b>4-state only</b>", (True, False): "<b>2-state only</b>"}.get(
        tuple(out), "neither")


# ---------------------------------------------------------------- page decoration
def on_page(canvas, doc):
    canvas.saveState()
    canvas.setFont("Body", 8.5); canvas.setFillColor(MUTED)
    canvas.drawString(0.85 * inch, 0.55 * inch, "From +2% to negative real rates: empirical claims of notebooks 8 and 8b")
    canvas.drawRightString(letter[0] - 0.85 * inch, 0.55 * inch, f"{doc.page}")
    canvas.setStrokeColor(RULE); canvas.setLineWidth(0.5)
    canvas.line(0.85 * inch, 0.7 * inch, letter[0] - 0.85 * inch, 0.7 * inch)
    canvas.restoreState()


S = []          # the story

# ================================================================ title + summary
S += [P("From +2% to negative real rates", "title"),
      P("What calibrating the 2-state and 4-state models to Argentina's interest rate tells us", "subtitle"),
      P("A guide to the claims and findings of notebooks <i>8. empirical_claims</i> and "
        "<i>8b. empirical_claims_negative_r</i>. Giuliana Triberti · draft of October 30, 2026", "small"),
      Spacer(1, 10)]

S += [P("The question in one paragraph", "h2"),
      P("The thesis argues that treating the informal sector as one low-income group (the <b>2-state</b> model: informal, "
        "formal) gives a distorted picture compared with splitting it by income (the <b>4-state</b> model: IH, IL, FH, FL). "
        "This report asks a simple empirical question: <i>if both models are forced to reproduce the same observed real "
        "interest rate r*, does one of them need strange parameter values, or make strange predictions?</i> The test starts at "
        "the advisor's suggested target of r* = +2% and then walks the target down through 0%, −1% and −2% to the medians "
        "of Argentina's actual ex-post real rate (" + pct(MED93, 2) + " for 1993–2025 and " + pct(MED50, 2) + " for 1976–2025)."),
      P("How to read this report", "h2"),
      P("Each section states one <b>claim</b> in a grey box with a verdict chip, then gives the evidence: a figure or table, "
        "how to read it, and the numbers. Yellow boxes list the <b>assumptions</b> each result depends on; their IDs "
        "(D = data, C = calibration, N = numerical, J = judgement rule, R = made for this report) point to the full "
        "register in Section 6, which also says how each assumption was checked.")]

S += [P("The findings at a glance", "h2")]
S += bullets([
    f"<b>The data do not give +2%.</b> Argentina's 50-year ex-post real short-term rate is negative (median {pct(MED50)}, "
    f"mean {pct(R50['real mean %'])}). Only the Convertibility years 1991–2001 average about +2% "
    f"({pct(R91['real mean %'], 2)}). So +2% is a calibration choice, not a data moment.",
    f"<b>At +2%, the hypothesis that the 2-state model needs a strange parameter is not supported.</b> Both models reach "
    f"+2% with ordinary discount rates: ρ = {f(RHO2S)} (2-state) and ρ = {f(RHO4S)} (4-state). It is the 4-state model that "
    f"needs more impatience, and, if credit is the free parameter instead, a much looser borrowing limit "
    f"({f(fr2.loc[('4-state','a_min'),'|a_min| in months of mean after-tax income'],1)} vs "
    f"{f(fr2.loc[('2-state','a_min'),'|a_min| in months of mean after-tax income'],1)} months of income).",
    "<b>The reason is precautionary saving by formal high-income (FH) workers</b>, who risk falling to the low-income formal "
    "state. It is not the informal high-income (IH) group the thesis emphasises.",
    f"<b>What calibration pins down is the impatience gap ρ − r*</b> (≈ {f(GAP2S)} in the 2-state, ≈ {f(GAP4S)} in the "
    "4-state), and not ρ or r* separately. Household behaviour barely changes when the target moves, so matching r* alone is a weak test.",
    f"<b>Toward negative rates, the ranking flips.</b> Below −2% the 2-state model breaks down: at the data medians it would "
    f"need a negative discount rate, and no borrowing limit works. The 4-state model reaches {pct(MED93, 2)} with "
    f"ρ = {f(rho_at('4-state', MED93))} and {pct(MED50, 2)} with ρ = {f(rho_at('4-state', MED50))}.",
    "<b>The formalization counterfactual is robust in sign, not in size.</b> Both models, each at its own calibration, "
    "predict that r* rises and fewer households borrow when informality falls. The fall in the borrowers mass in the 4-state "
    "model is about one-third of the 2-state fall. The thesis's sign reversal does not survive.",
    f"<b>Checked against data, the 4-state requirements at +2% are not realistic, and the thesis premium is far too low.</b> "
    f"Argentina's household debt has been {pct(100*DI_LO)}–{pct(100*DI_HI_BANK)} of income since 1994 (BIS), against the "
    f"{pct(100*fr2.loc[('4-state','a_min'),'gross debt / income'])} the 4-state borrowing limit implies at +2%. Non-bank lenders charge "
    f"a real premium of {f(PR_LO,0)}–{f(PR_HI,0)} pp over banks (BCRA), against θ<sub>I</sub> = 2 pp in the thesis. Premia of that size "
    "are exactly what the models need to produce the negative data medians.",
    "<b>The lesson.</b> Which model looks 'closer to reality' is decided by the choice of the data counterpart for r*, "
    "not by the models. If r* should be a positive real return (the advisor's +2%, or a dollar return), the 4-state model is "
    "the one under strain. If it should be the negative ex-post peso rate, only the 4-state model fits. The informal-premium data "
    "lean toward the negative reading; the household-debt data favour targets between 0% and −2%.",
])

# verdict-at-a-glance table
rows = []
for t in TARGETS:
    rows.append([tlab(t), verdict("rho", t), verdict("a_min", t), verdict("theta_I", t)])
S += [Spacer(1, 4)]
S += table(["Target r*", "ρ free (in [0.02, 0.12])", "a<sub>min</sub> free (ρ = 0.05)", "θ<sub>I</sub> free (ρ = 0.05, θ<sub>I</sub> ≥ 0)"],
           rows, [1.9 * inch, 1.6 * inch, 1.6 * inch, 1.7 * inch],
           caption="Which model can be calibrated to each target with an admissible parameter value. "
                   "\"4-state only\" supports the thesis hypothesis that the 4-state model is closer to the data; "
                   "\"2-state only\" contradicts it. Pass rules: assumptions J1–J3.")
# ================================================================ 1. setup
S += [P("1. The test design", "h1"),
      P("Both models are the continuous-time Huggett economies of the thesis (notebooks 4 and 5), with the full Argentine "
        "calibration of Section 2 at η = 0.42 (informal share). Households save or borrow in a single risk-free bond in zero "
        "net supply, and r* is the rate that clears that market. Table 2 lists the inputs, copied verbatim from the thesis."),
      ]
S += table(["", "2-state (I, F)", "4-state (IH, IL, FH, FL)"],
           [["Income y", "0.418, 1", "0.830, 0.220, 1, 0.321"],
            ["Tax τ", "0, 0.19", "0, 0, 0.19, 0.05"],
            ["Informal borrowing premium θ", "0.02, 0", "0.02, 0.02, 0, 0"],
            ["Risk aversion γ", "0.30, 0.15", "0.30, 0.30, 0.15, 0.15"],
            ["Transition rates", "EPH total hazard 0.914, split to give η", "EPH generator (thesis Tables 2–3)"],
            ["Borrowing limit a<sub>min</sub>; grid", "−0.099; 800 points on [−0.099, 5]", "same"],
            ["Thesis discount rate ρ", "0.05", "0.05"]],
           [2.0 * inch, 2.2 * inch, 2.6 * inch],
           caption="Model inputs (thesis Section 2, η = 0.42).")
S += [P("The test has three rules:"),
      *bullets(["<b>One free parameter at a time.</b> To hit a target r*, free exactly one parameter and keep every other "
                "parameter at its thesis value. The main free parameter is the discount rate ρ, which conventionally pins "
                "down r* in a Huggett economy. Two parameters that can be checked in data are then freed instead, with ρ "
                "held at 0.05: the borrowing limit a<sub>min</sub> and the informal borrowing premium θ<sub>I</sub>.",
                "<b>Verify every calibration.</b> After solving S(r*; parameter) = 0, the full equilibrium is re-solved with "
                "the usual bisection over r, and it must return the target.",
                "<b>Judge two things:</b> whether the required parameter value is plausible (assumptions J1–J3), and whether the "
                "untargeted predictions look reasonable."]),
      P("<b>Replication check.</b> Before it is used, the solver reproduces four equilibria printed in the thesis notebooks "
        "to within 5 × 10<super>−7</super> (Table 3), so the results below come from the thesis model itself.")]
S += table(["Case (ρ = 0.05)", "r* in thesis notebook", "|difference|"],
           [[c, f(r, 6), f"{d:.1e}"] for c, r, d in REPLICATION], [3.8 * inch, 1.5 * inch, 1.5 * inch],
           caption="The solver reproduces the thesis notebooks (transcribed from notebook 8, Step 2).")
S += assume_box("C1 (only one parameter freed), C2 (thesis inputs, including γ<sub>I</sub> = 0.30 in the 2-state), "
                "C4 (r* is the return on the only risk-free asset, in steady state), N1 (same solver and grid as the thesis).")
S += [PageBreak()]

# ================================================================ PART I
S += [P("Part I. The advisor's target: r* = +2%", "h1")]

# ---- Claim 1
S += claim_box("Claim 1 · notebook 8, Step 1",
               "Argentina's 50-year average real interest rate is about +2%.", "not")
S += figure("real_rate_history",
            "Ex-post real short-term interest rate in Argentina, 1976–2025 (Global Macro Database, release 2026_09).",
            "blue bars are years with a positive real rate and red bars years with a negative one. The solid black line is the "
            "+2% target, and the dashed and dotted lines are the medians of the two windows. The grey band marks Convertibility, "
            "the only period that averages about +2%.")
rows = []
for w, lab in [("1976-2025", "1976–2025 (50 years)"), ("1976-1990", "1976–1990 (high inflation)"), ("1991-2001", "1991–2001 (Convertibility)"),
               ("2002-2025", "2002–2025"), ("1993-2025", "1993–2025")]:
    r = win(w)
    rows.append([lab, int(r["n_years"]), pct(r["nominal median %"]), pct(r["real mean %"]), pct(r["real median %"]),
                 pct(r["real geometric mean %"]), f"{100*r['share of years real > 0']:.0f}%"])
pr = win("1993-2025", "policy rate (BIS)")
rows.append(["Policy rate (BIS), 1993–2025", int(pr["n_years"]), pct(pr["nominal median %"]), pct(pr["real mean %"]),
             pct(pr["real median %"]), pct(pr["real geometric mean %"]), f"{100*pr['share of years real > 0']:.0f}%"])
S += table(["Window (short-term rate)", "Years", "Nominal median", "Real mean", "Real median", "Real geom. mean", "Years real > 0"],
           rows, [2.05 * inch, 0.5 * inch, 0.85 * inch, 0.8 * inch, 0.85 * inch, 0.95 * inch, 0.8 * inch],
           caption="Summary of the ex-post real rate by window. Real rates use the exact Fisher relation (1 + i)/(1 + π) − 1.",
           highlight_rows=(2,))
S += [P("What we find", "h2")]
S += bullets([f"Every 50-year summary is negative: mean {pct(R50['real mean %'])}, median {pct(MED50)}, geometric mean "
              f"{pct(R50['real geometric mean %'])}. The real rate was positive in only {100*R50['share of years real > 0']:.0f}% of years.",
              f"Only Convertibility (1991–2001) averages near the target: mean {pct(R91['real mean %'], 2)}, median {pct(R91['real median %'])}.",
              f"Since 2002 the rate is negative again (median {pct(R02['real median %'])}). The real policy rate, available "
              "only from 1993, has a negative median too.",
              "<b>Implication.</b> +2% is an external calibration choice that matches the Convertibility average, not the 50-year "
              "average, and the thesis should say so. Everything in Part I is conditional on it; Part II tests the "
              "alternatives the data suggest."])
S += assume_box("D1 (ex-post real rate from realised inflation), D2 (spliced short-term rate as the 50-year series, "
                "because Argentina has no continuous policy rate), D3 (GMD release 2026_09, forecasts dropped), "
                "C3 (+2% is an external target).")

# ---- Claim 2
S += claim_box("Claim 2 · notebook 8, Steps 3–4",
               "At the thesis discount rate ρ = 0.05, the two models already disagree about r*: "
               f"the 2-state model gives {pct(R_AT_RHO05['2-state'], 2, True)} and the 4-state model {pct(R_AT_RHO05['4-state'], 2, True)}.",
               "fact")
S += figure("bond_demand_and_rstar_thesis_rho",
            "Left: net bond demand S at r = 2% as the discount rate ρ varies. Right: the equilibrium r* each model "
            "produces at the thesis ρ = 0.05.",
            "on the left, S > 0 means households want to save more than others want to borrow at 2%, so r* must be lower. "
            "Each curve crosses zero at the ρ that makes 2% the equilibrium rate (large dots). The orange curve lies above the blue "
            "one everywhere: at any ρ, the 4-state economy wants to hold more bonds.")
s05 = scan[np.isclose(scan["rho"], 0.05)].set_index("model")["S(r=2%)"]
S += bullets([f"At ρ = 0.05 and r = 2%, the 4-state economy has excess bond demand {f(s05['4-state'])}, against "
              f"{f(s05['2-state'])} in the 2-state model, about {s05['4-state']/s05['2-state']:.0f} times as large.",
              "So with the thesis parameters the 4-state model needs a much lower rate to clear the market. It clears "
              f"<b>below zero</b> ({pct(R_AT_RHO05['4-state'], 2, True)}). This is the first hint that the 4-state model fits "
              "low or negative rates more naturally."])
S += assume_box("C2 (thesis inputs), N1 (solver). The lower bound on r used in the thesis notebooks was removed "
                "here so that the true market-clearing rate is reported (see Claim 6 for why this matters).")

# ---- Claim 3
S += claim_box("Claim 3 · notebook 8, Steps 4–5 and 8",
               "The 2-state model needs a strange (implausible) discount rate to produce r* = +2%, while the 4-state model does not.",
               "not")
rows = [[m, f(rho2.loc[m, "calibrated rho"], 4), f(rho2.loc[m, "annual discount factor beta = exp(-rho)"], 3),
         f(rho2.loc[m, "quarterly beta"], 4), f(rho2.loc[m, "rho - r* (impatience gap)"], 4)] for m in ("2-state", "4-state")]
S += table(["Model", "Calibrated ρ", "Annual β = e<super>−ρ</super>", "Quarterly β", "Impatience gap ρ − r*"], rows,
           [1.2 * inch, 1.2 * inch, 1.4 * inch, 1.2 * inch, 1.8 * inch],
           caption="Discount rate that makes r* = +2% in each model (verified: the re-solved equilibrium returns 2.0000% in both).")
rows = [[pct(100 * t, 1), f(rho_tg.loc[t, "2-state"], 4), f(rho_tg.loc[t, "4-state"], 4), f(GAP_RHO.loc[t], 4)] for t in rho_tg.index]
rows += [["+2%, 2-state with γ<sub>I</sub> = 0.27", f(RHO_GAMMA027, 4), "", ""],
         ["+2%, calibrated at η = 0.20", f(RHO2S_ETA20, 4), f(RHO4S_ETA20, 4), f(RHO4S_ETA20 - RHO2S_ETA20, 4)]]
S += table(["Target r* (robustness)", "ρ, 2-state", "ρ, 4-state", "ρ<sub>4s</sub> − ρ<sub>2s</sub>"], rows,
           [2.6 * inch, 1.3 * inch, 1.3 * inch, 1.5 * inch],
           caption="The gap between the models does not depend on the 2% choice, on γ<sub>I</sub>, or on the size of the "
                   "informal sector (last two rows transcribed from notebook 8, Step 8).")
S += [P("What we find", "h2")]
S += bullets([f"2-state: ρ = {f(RHO2S)} (annual β = {f(BETA2S)}). That is a textbook value; at the thesis ρ = 0.05 it already gives "
              f"r* = {pct(R_AT_RHO05['2-state'], 1)}.",
              f"4-state: ρ = {f(RHO4S)} (annual β = {f(BETA4S)}, quarterly {f(QB4S)}). This is more impatient, but it is the standard value "
              "in emerging-market business-cycle models (quarterly β ≈ 0.98, as in Aguiar and Gopinath 2007 and Neumeyer and Perri 2005).",
              f"The gap ρ<sub>4s</sub> − ρ<sub>2s</sub> = {f(GAP_RHO.mean())} is the same for every target from 1% to 2.5%.",
              "<b>Verdict.</b> Neither value is strange, so neither model is ruled out on parameter grounds. If anything, the "
              "4-state model is the one that needs the less conventional value."])
S += assume_box("J1 (a value of ρ is 'plausible' if it lies in [0.02, 0.12] per year), C2 (γ<sub>I</sub> = 0.30; the "
                "notebook-4 code uses 0.27, tested in Table 6), C1.")

# ---- Claim 4
fh, f2 = m2.loc[("4-state", "FH")], m2.loc[("2-state", "F")]
ih = m2.loc[("4-state", "IH")]
S += claim_box("Claim 4 · notebook 8, Step 9",
               "The 4-state model saves more because of the informal high-income (IH) group.", "not")
S += figure("mechanism_bonds_by_state",
            "Each group's contribution to aggregate bond holdings S at a common ρ = 0.05 and r = 2%.",
            "the bar is the group's mass times its mean wealth, and the bars of a model add up to its aggregate S (in the "
            "title). Bars to the right of zero are net savers. The longest bar, formal high-income (FH) workers in the "
            "4-state model, is where the extra saving comes from.", width=6.2 * inch)
rows = [[m, s, f(r["after-tax income"]), f(r["mass"]), f(r["mean wealth"]), f(r["net bonds (contribution to S)"], 4),
         f(r["exit rate (1/expected duration)"])] for (m, s), r in m2.iterrows()]
S += table(["Model", "State", "After-tax income", "Mass", "Mean wealth", "Contribution to S", "Exit rate per year"], rows,
           [0.9 * inch, 0.6 * inch, 1.1 * inch, 0.8 * inch, 1.0 * inch, 1.2 * inch, 1.2 * inch],
           caption="Decomposition behind Figure 3 (ρ = 0.05, r = 2%).", highlight_rows=(4,))
S += bullets([f"FH workers hold {f(fh['net bonds (contribution to S)'])} of net bonds, against {f(f2['net bonds (contribution to S)'])} "
              f"for the whole formal group in the 2-state model. IH workers contribute only {f(ih['net bonds (contribution to S)'])}.",
              f"FH workers leave their state at a rate of {f(fh['exit rate (1/expected duration)'], 2)} per year, and "
              f"{f(LAM4.loc['FH', 'FL'], 2)} of that ({LAM4.loc['FH', 'FL'] / -LAM4.loc['FH', 'FH']:.0%}) is a fall to low-income formal work "
              "(income 0.32 instead of 1). Facing a large, frequent income drop, they build "
              "precautionary savings. The 2-state model averages this risk away inside a single formal state.",
              "<b>Implication.</b> The thesis should not claim that IH workers are what depresses r*. The income risk "
              "<i>within</i> the formal sector, which the 4-state model also captures, is the driver."])
S += assume_box("R3 (reading the exit rates as income risk is an interpretation of the generator, not a separate estimate), C2.")

# ---- Claim 5
S += claim_box("Claim 5 · notebook 8, Step 6",
               "Once both models are calibrated to +2%, one of them makes implausible predictions.", "not")
S += figure("borrowing_share_by_state",
            "Share of each group in debt (a &lt; 0), with each model at its own calibrated ρ and r* = +2%.",
            "bars are grouped by model. In both models informal workers borrow more than formal workers. The 4-state model "
            "adds that low-income formal workers (FL) mostly borrow, while IH workers mostly save.", width=6.2 * inch)
keys = [("borrowers mass (a<0)", "Borrowers mass (share with a &lt; 0)"), ("mass at borrowing limit (first grid pt)", "Mass at the borrowing limit"),
        ("informal: share borrowing", "Informal: share borrowing"), ("formal: share borrowing", "Formal: share borrowing"),
        ("informal mean wealth", "Informal mean wealth"), ("formal mean wealth", "Formal mean wealth"),
        ("gross debt / mean after-tax income", "Gross debt / mean after-tax income")]
S += table(["Prediction (η = 0.42, r* = 2%)", "2-state", "4-state"],
           [[lab, f(mom.loc[k, "2-state"]), f(mom.loc[k, "4-state"])] for k, lab in keys],
           [3.6 * inch, 1.5 * inch, 1.5 * inch], caption="Untargeted moments at the common calibration.")
S += bullets(["The aggregate predictions are close: about half the population borrows in both models, and just over a fifth "
              "sits at the borrowing limit.",
              "Neither model predicts anything implausible. Wealth and borrowing are not observed in the EPH, so these "
              "moments cannot tell the models apart with the thesis data. They are the natural place to bring household-finance "
              "data later (Section 5)."])

# ---- Claim 6
S += claim_box("Claim 6 · notebook 8, Step 7 (thesis Section 4.4)",
               "Formalization moves the borrowers mass in opposite directions in the two models (the thesis's 'sign reversal').",
               "not")
S += figure("counterfactual_formalization",
            "Effect of lowering informality from η = 0.42 to η = 0.20, with each model at its own calibrated ρ.",
            "each panel is one outcome and the bars show the change in each model. Bars on the same side of zero mean that "
            "the models agree on the direction; different heights mean that they disagree on the size.")
rows = []
for _, r in cf.iterrows():
    rows.append([r["model"], f(r["rho"], 4), f(r["eta"], 2), pct(r["r* %"], 2), f(r["borrowers mass"]), f(r["mass at limit"]),
                 f(r["informal share borrowing"]), f(r["formal share borrowing"])])
S += table(["Model", "ρ", "η", "r*", "Borrowers mass", "Mass at limit", "Informal borrowing", "Formal borrowing"], rows,
           [0.75 * inch, 0.65 * inch, 0.5 * inch, 0.7 * inch, 1.0 * inch, 0.95 * inch, 1.1 * inch, 1.1 * inch],
           caption="Formalization counterfactual; every equilibrium cleared.")
S += bullets([f"<b>Same signs.</b> r* rises ({f(cf_d.loc['2-state','r* %'],2,True)} pp in the 2-state, {f(cf_d.loc['4-state','r* %'],2,True)} pp "
              "in the 4-state) and the borrowers mass falls in both.",
              f"<b>Different sizes.</b> The borrowers mass falls by {f(cf_d.loc['2-state','borrowers mass'],3,True)} in the 2-state model but only "
              f"{f(cf_d.loc['4-state','borrowers mass'],3,True)} in the 4-state, about {cf_d.loc['4-state','borrowers mass']/cf_d.loc['2-state','borrowers mass']:.0%} "
              f"of it. The mass at the constraint falls by {f(cf_d.loc['2-state','mass at limit'],3,True)} against {f(cf_d.loc['4-state','mass at limit'],3,True)}.",
              "<b>Why the thesis found a reversal.</b> In thesis notebook 5, the 4-state equilibrium at η = 0.42 and ρ = 0.05 never "
              "cleared: the bisection stopped at its lower bound r = 0.0005 with S = +0.03. That point is not an equilibrium. "
              "Claim 2 shows the true r* there is negative.",
              "<b>What survives.</b> The 4-state model implies a much weaker response of financial constraints to formalization. "
              "That difference in size is policy-relevant, and it does not depend on how r* is calibrated."])
S += assume_box("C1, C2; ρ held at each model's own calibrated value while η changes (the counterfactual keeps "
                "preferences fixed). Only η = 0.20 is tested.")

# ---- Claim 7
a2, a4 = fr2.loc[("2-state", "a_min")], fr2.loc[("4-state", "a_min")]
t2 = fr2.loc[("2-state", "theta_I")]
S += claim_box("Claim 7 · notebook 8, Steps 11–12",
               "With parameters that can be checked in data (credit limit, informal premium) instead of ρ, the 4-state "
               "model matches +2% more easily.", "not")
S += figure("requirements_rstar2",
            "What each model needs to produce r* = +2% when ρ is held at 0.05 and one observable parameter is freed, "
            "with the pass/fail verdict.",
            "dots show the value each model needs. Grey bands and thin lines mark the literature range or the thesis "
            "value. 'Not attainable' means that no value of the parameter clears the market at 2%. The lower panel "
            "applies the pass rules.")
S += figure("scan_free_params_rstar2",
            "Supporting evidence: net bond demand at r = 2% and ρ = 0.05 as a<sub>min</sub> (left) or θ<sub>I</sub> (right) varies.",
            "a calibration exists where a curve crosses zero. The 4-state curve on the right stays above zero for every "
            "premium from −5% to +100%.", width=6.0 * inch)
S += bullets([f"<b>Borrowing limit.</b> 2-state: a<sub>min</sub> = {f(a2['calibrated value'])} "
              f"({f(a2['|a_min| in months of mean after-tax income'],1)} months of income; thesis value about 1.8 months), debt/income "
              f"{f(a2['gross debt / income'])}. 4-state: a<sub>min</sub> = {f(a4['calibrated value'])} "
              f"({f(a4['|a_min| in months of mean after-tax income'],1)} months), debt/income {f(a4['gross debt / income'])}, "
              f"{a4['gross debt / income']/a2['gross debt / income']:.1f} times as much.",
              f"<b>Informal premium.</b> 2-state: θ<sub>I</sub> = {f(t2['calibrated value'])}, a <i>negative</i> premium, so informal "
              "households would borrow below the saving rate; that is not economically meaningful. 4-state: not attainable. "
              "Even nearly free informal credit cannot absorb the excess saving, because that saving comes from FH households "
              "(Claim 4), whom θ<sub>I</sub> does not reach.",
              "<b>Verdict at +2%.</b> No test is passed by the 4-state model and failed by the 2-state model. Through credit, "
              f"the 4-state model needs household debt of {pct(100*a4['gross debt / income'])} of income. Part III shows that Argentina's has been "
              f"{pct(100*DI_LO)}–{pct(100*DI_HI_BANK)} since 1994, so the 2-state requirement ({pct(100*a2['gross debt / income'])}) is the "
              "realistic one (green bands in Figure 6)."])
S += assume_box("C5 (thesis a<sub>min</sub> = −0.099 has no stated source; θ<sub>I</sub> = 0.02), J2 (θ<sub>I</sub> &lt; 0 fails), "
                "J3 (the pass rule only asks that a<sub>min</sub> exists and clears; its realism is judged separately in Part III), "
                "N4 ('not attainable' is defined on the scan grid), D5–D8 (data benchmarks drawn in the figure).")
S += [PageBreak()]

# ================================================================ PART II
S += [P("Part II. Walking the target down to the data", "h1"),
      P("Part I took +2% as given. But Claim 1 showed that Argentina's real rate is negative, and Claim 2 showed that the "
        "4-state model clears below zero at the thesis parameters. Notebook 8b therefore repeats every test for targets of "
        f"0%, −1%, −2%, the 1993–2025 median ({pct(MED93, 2)}) and the 1976–2025 median ({pct(MED50, 2)}).")]

# ---- Claim 8
S += claim_box("Claim 8 · notebook 8b, Steps 2–4",
               "Matching r* identifies the discount rate.", "not")
S += figure("rho_and_gap_by_target",
            "Left: the ρ needed to hit each target. Right: the impatience gap ρ − r* at each calibration.",
            "on the left, the grey band is the literature range for ρ and the vertical lines are the two data medians. "
            "Crosses mark targets where the 2-state model cannot be calibrated with ρ > 0; the dotted line extrapolates its "
            "constant gap. On the right, both lines are flat: each model needs the same ρ − r* whatever the target.")
rows = []
for t, r in gap.iterrows():
    rows.append([tlab(t), f(r["2-state"], 4), f(r["4-state"], 4), f(r["2-state: rho - r*"], 4), f(r["4-state: rho - r*"], 4)])
S += table(["Target r*", "ρ, 2-state", "ρ, 4-state", "ρ − r*, 2-state", "ρ − r*, 4-state"], rows,
           [2.3 * inch, 1.05 * inch, 1.05 * inch, 1.2 * inch, 1.2 * inch],
           caption="Calibrated ρ and impatience gap by target ('–' means no ρ > 0 reaches the target).")
S += figure("untargeted_moments_by_target",
            "Untargeted predictions at each calibration, with ρ re-calibrated for every target.",
            "every y-axis starts at zero, so a flat line means the prediction does not depend on the target. All four "
            "predictions are essentially flat in both models.")
S += bullets([f"The calibration pins down the gap ρ − r* (about {f(GAP2S)} in the 2-state and {f(GAP4S)} in the 4-state model), "
              "not the level of either. Shifting r* and ρ together leaves household behaviour almost unchanged.",
              "<b>Implication.</b> Hitting a target r* is a weak test of a model: any target can be matched by moving ρ in step, "
              "and the predictions barely move. What discriminates is whether the required ρ is admissible (positive, inside "
              "the literature range) and what the observable parameters must be."])
S += assume_box("C4, N2 (for r ≤ 0 the solver starts from a different initial guess and uses continuation; the fixed point "
                "is unchanged, checked at r = 2% to 3 × 10<super>−10</super>), R2 (the dotted extrapolation assumes the 2-state gap "
                "stays constant).")

# ---- Claim 9
lo2 = low[(low["model"] == "2-state") & low["valid"]]
S += claim_box("Claim 9 · notebook 8b, Steps 2 and 7",
               "At Argentina's ex-post real-rate medians, only the 4-state model can be calibrated with a positive discount rate.",
               "supported")
S += figure("bond_demand_low_rho_median",
            f"Net bond demand at r = {pct(MED93, 2)} (the 1993–2025 median) as ρ falls toward zero.",
            "a calibration exists where a curve crosses zero. The 4-state curve crosses at ρ ≈ 0.02. The 2-state curve stays "
            "below zero all the way down to ρ = 0.0025: even very patient households want to borrow more than others want "
            "to lend. At higher ρ it sits at the corner S = a<sub>min</sub>, where everyone is at the borrowing limit.", width=6.2 * inch)
S += bullets([f"<b>4-state:</b> reaches {pct(MED93, 2)} with ρ = {f(rho_at('4-state', MED93), 4)} (annual β = "
              f"{f(np.exp(-rho_at('4-state', MED93)))}), a textbook value, and {pct(MED50, 2)} with ρ = {f(rho_at('4-state', MED50), 4)}, "
              "which is very patient but positive.",
              f"<b>2-state:</b> S &lt; 0 at every ρ tried, down to {f(lo2['rho'].min(), 4)}. Extrapolating its constant gap, it would need "
              f"ρ ≈ {pct(100*(MED93/100 + GAP2S), 1)} and {pct(100*(MED50/100 + GAP2S), 1)}: households that value the future "
              "<i>more</i> than the present, which is not a standard parameter.",
              "<b>Why.</b> The 4-state model carries more precautionary saving (Claims 2 and 4). At the same ρ it wants to hold more "
              "bonds, so only a low or negative rate clears the market. That same feature made it strain at +2%."])
S += assume_box("D4 (the medians, not the means, are the data targets: the means are dominated by the 1975–1991 high "
                "inflation), J1, R2 (the negative-ρ figures are extrapolations, not solves).")

# ---- Claim 10
S += claim_box("Claim 10 · notebook 8b, Steps 5–6",
               "The ranking of the models on observable parameters also flips as the target turns negative.", "supported")
S += figure("requirements_by_target",
            "What each model needs to hit each target: ρ (top left), and, with ρ = 0.05, the borrowing limit (top right), "
            "the implied household debt (bottom left) and the informal premium (bottom right).",
            "lines show the value needed, and crosses in the grey strip at the bottom mark targets where no value works. "
            "Follow each line from right (+2%) to left (the data medians). The blue 2-state line ends early in three of the four panels.")
S += figure("verdict_by_target", "Pass/fail verdict for each test and target (same pass rules as Table 1).",
            "orange cells (4-state only) support the hypothesis that the 4-state model is closer to the data; blue cells "
            "contradict it. They concentrate at the negative targets.")
rows = []
for t in TARGETS:
    a2_, a4_ = fa("2-state", t, "a_min", None), fa("4-state", t, "a_min", None)
    th2, th4 = fa("2-state", t, "theta_I", None), fa("4-state", t, "theta_I", None)
    ok = lambda r: r["status"] == "ok" and str(r["cleared"]) == "True"
    cell_a = lambda r: f"{f(r['calibrated value'])} ({f(r['|a_min| in months of mean after-tax income'],1)} mo)" if ok(r) else "not attainable"
    cell_t = lambda r: (f"{f(100*r['calibrated value'],1)} pp" if ok(r) else ("no robust eq." if r["status"] == "ok" else "not attainable"))
    rows.append([tlab(t), cell_a(a2_), cell_a(a4_), cell_t(th2), cell_t(th4)])
S += table(["Target r*", "a<sub>min</sub>, 2-state", "a<sub>min</sub>, 4-state", "θ<sub>I</sub>, 2-state", "θ<sub>I</sub>, 4-state"], rows,
           [2.0 * inch, 1.3 * inch, 1.3 * inch, 1.1 * inch, 1.1 * inch],
           caption="Observable parameter needed at ρ = 0.05 (a<sub>min</sub> also in months of mean after-tax income; θ<sub>I</sub> in "
                   "percentage points over r*).")
th4m = fa("4-state", MED93, "theta_I", None)
S += bullets(["<b>Positive targets:</b> the 4-state model is the one needing an implausible observable parameter: a very loose "
              "borrowing limit, and no informal premium works at all at 0% or +2%.",
              "<b>Negative targets:</b> the 2-state model cannot reach −2% or below through the borrowing limit. Even at "
              "a<sub>min</sub> = −0.01 almost everyone sits at the limit and S stays negative. Through θ<sub>I</sub> it needs high "
              "premia, and at the 1976–2025 median the fixed-r root does not survive re-solving (no robust equilibrium).",
              f"The 4-state model reaches every negative target with a tight but positive borrowing limit (about "
              f"{f(fa('4-state', MED93, 'a_min', '|a_min| in months of mean after-tax income'),1)} months of income at {pct(MED93, 2)}), or with a "
              f"real informal borrowing rate r + θ<sub>I</sub> of about {f(th4m['informal borrowing rate r+theta %'],0)}%.",
              "These a<sub>min</sub> values are not grid artefacts: re-calibrating on a grid three times finer moves them by less "
              "than 4%, except the 2-state model at −1% (8%, with only 4 grid points below zero). See Table 12."])
rows = [[t, m, n, f(a, 4), f(b, 4), f"{100*(b/a-1):+.1f}%".replace("-", "−")] for t, m, n, a, b in GRID_CHECK]
S += table(["Target", "Model", "Grid points below 0 (I = 800)", "a<sub>min</sub>, I = 800", "a<sub>min</sub>, I = 2400", "Change"], rows,
           [0.8 * inch, 0.8 * inch, 1.8 * inch, 1.15 * inch, 1.15 * inch, 0.9 * inch],
           caption="Grid-resolution check for tight borrowing limits (transcribed from notebook 8b, Step 5).")
S += assume_box("J2, J3, N4, N5 (grid check), N6 (the 2-state θ<sub>I</sub> root at −5.12% fails to clear when re-solved "
                "and is treated as not attainable).")
S += [PageBreak()]

# ================================================================ PART III (data benchmarks)
S += [P("Part III. Are the required parameters realistic? A check against data", "h1"),
      P("The pass rules of Parts I and II only ask whether a calibration exists and is economically meaningful. This part asks "
        "whether the borrowing limit and the informal premium each model needs are the size we observe in Argentina. "
        "Notebook 8, Step 11b, builds two benchmarks from public data (Table 13). The raw files and their sources are in "
        "<i>data/interest_rates/benchmarks/</i>.")]
S += bullets([f"<b>Household gross debt / income</b>, the benchmark for a<sub>min</sub> through the debt it implies: BIS credit to households "
              "(% of GDP) divided by household consumption (% of GDP, World Bank). In the model, income is after-tax income, and with bonds "
              "in zero net supply aggregate consumption equals it, so consumption is the model-consistent denominator. An upper series adds "
              f"non-bank lenders, which the BCRA puts at {NONBANK:.0%} of bank credit to individuals.",
              "<b>Informal borrowing premium</b>, the benchmark for θ<sub>I</sub>. In the model, formal households borrow at r and informal "
              "ones at r + θ<sub>I</sub>, so θ<sub>I</sub> is the gap in borrowing cost between the two groups. The data counterpart is "
              "the personal-loan rate of non-bank credit providers (fintech, retailers, cooperatives), who lend to people without access "
              "to bank credit, over the bank personal-loan rate. The real spread is (1 + i<sub>non-bank</sub>)/(1 + i<sub>bank</sub>) − 1, "
              "which does not depend on the inflation measure."])
S += table(["Benchmark", "Central value", "Observed range", "Source"],
           [["Household debt / income", f"{pct(100*DI_MED)} (1994–{DI_LAST_Y} median)",
             f"{pct(100*DI_LO)}–{pct(100*DI_HI_BANK)} (BIS); up to {pct(100*DI_HI)} with non-bank lenders",
             "BIS total credit statistics (Q.AR.H.A.M.770.A); World Bank WDI (NE.CON.PRVT.ZS); BCRA (2026)"],
            ["Latest year", f"{pct(100*DI_LAST)} ({DI_LAST_Y})", "", ""],
            ["Convertibility, 1994–2001 (the +2% years)", f"{pct(100*DI_CONV.mean())} (mean)", f"peak {pct(100*DI_CONV.max())} ({DI_CONV.idxmax()})", ""],
            ["Informal premium θ<sub>I</sub>", f"{f(PR_MED, 0)} pp (median of 3 dates)",
             f"{f(PR_LO, 0)} pp (cooperatives) to {f(PR_HI, 0)} pp (all providers, Feb 2026)",
             "BCRA, <i>Informe de Proveedores No Financieros de Crédito</i>, Dec 2023, Nov 2025, Jun 2026"],
            ["Thesis value of θ<sub>I</sub>", "2 pp", "", "Thesis Section 2"]],
           [1.75 * inch, 1.35 * inch, 1.9 * inch, 1.8 * inch],
           caption="Data benchmarks (notebook 8, Step 11b). The green bands in the figures are the observed ranges.")
S += [P("Table 14 summarises the result of Claims 11 and 12 below. Feasibility (Table 1) asks whether a value exists; realism asks "
        "whether that value is the size observed in Argentina.")]
rows = [[tlab(t), realistic("a_min", t), realistic("theta_I", t)] for t in TARGETS]
S += table(["Target r*", "a<sub>min</sub> route: implied debt realistic", "θ<sub>I</sub> route: premium realistic"], rows,
           [2.3 * inch, 2.25 * inch, 2.25 * inch],
           caption="Realism at a glance: which model's requirement lies inside the observed range (J4). Compare with the "
                   "feasibility verdict of Table 1.")
S += figure("data_benchmarks",
            "Left: Argentina's household debt / income, 1994–2025, against the debt each model's borrowing limit implies at "
            "r* = +2% (ρ = 0.05). Right: real premium of non-bank over bank personal-loan rates, against the thesis value and "
            "what each model needs at +2%.",
            "on the left, the black line is the data and the dashed line adds non-bank lenders. The orange line (4-state "
            "requirement) lies above the data in every year, including the Convertibility peak; the blue line (2-state) runs through "
            "the data. On the right, the bars are the all-provider premium at three dates and the open dots are provider groups in "
            "February 2026. Both the thesis 2 pp and the premia the models need at +2% sit at the bottom of the chart.")
S += figure("data_check_by_target",
            "What each model needs at each target, against the observed range (green band) and median (black line): household "
            "debt implied by the a<sub>min</sub> calibration (left) and informal premium (right), ρ = 0.05.",
            "follow each line from right (+2%) to left (the data medians) and check when it is inside the green band. Crosses at "
            "the bottom mark targets where no value works. The left panel favours moderate targets; the right panel favours negative ones.")

# ---- Claim 11
a2_, a4_ = fr2.loc[("2-state", "a_min")], fr2.loc[("4-state", "a_min")]
inc = m2["after-tax income"]
d4m = [fa("4-state", t, "a_min", "gross debt / income") for t in (MED93, MED50)]
box = claim_box("Claim 11 · notebook 8, Steps 11–12 and 11b; notebook 8b, Step 6",
               "The household debt implied by the 4-state model's borrowing limit is consistent with Argentina's.", "mixed")
rows = []
for lab, r, poor, inc_poor in [("2-state", a2_, "I", inc[("2-state", "I")]), ("4-state", a4_, "IL", inc[("4-state", "IL")])]:
    rows.append([lab, f(r["calibrated value"]), f(r["|a_min| in months of mean after-tax income"], 1),
                 f"{f(12 * -r['calibrated value'] / inc_poor, 1)} ({poor})", pct(100 * r["borrowers mass"], 0),
                 pct(100 * r["mass at borrowing limit"], 0),
                 f(12 * r["gross debt / income"] / r["borrowers mass"], 1), pct(100 * r["gross debt / income"])])
rows.append(["Data", "", "", "", "", "", "", f"{pct(100*DI_LO)}–{pct(100*DI_HI)}"])
S += [KeepTogether(box + table(["Model at r* = +2%", "a<sub>min</sub>", "Limit, months of mean income", "Limit, months of own income (poorest group)",
            "Share in debt", "Share at the limit", "Debt per borrower, months of mean income", "Debt / income"], rows,
           [0.85 * inch, 0.55 * inch, 0.85 * inch, 1.05 * inch, 0.65 * inch, 0.7 * inch, 1.15 * inch, 1.0 * inch],
           caption="What the borrowing limit needed at +2% means for households (a<sub>min</sub> calibrated, ρ = 0.05). The limit is the most "
                   "anyone may owe, not what people owe: debt per borrower is total debt divided by the share in debt.",
           highlight_rows=(1,)))]
rows = []
for t in TARGETS:
    cells = []
    for m in ("2-state", "4-state"):
        r = fa(m, t, "a_min", None)
        cells.append(pct(100 * r["gross debt / income"]) if r["status"] == "ok" and str(r["cleared"]) == "True" else "not attainable")
    rows.append([tlab(t), *cells, realistic("a_min", t)])
S += table(["Target r*", "Debt / income, 2-state", "Debt / income, 4-state", f"Inside the data range ({pct(100*DI_LO)}–{pct(100*DI_HI)})"], rows,
           [2.0 * inch, 1.4 * inch, 1.4 * inch, 2.0 * inch],
           caption="Household debt implied by the a<sub>min</sub> calibration at each target, against the data.")
S += bullets([f"<b>What the 6.6 months means.</b> a<sub>min</sub> is the most a household may owe. At +2% the 4-state limit is "
              f"{f(a4_['|a_min| in months of mean after-tax income'], 1)} months of <i>average</i> income, which is "
              f"{f(12 * -a4_['calibrated value'] / inc[('4-state', 'IL')], 1)} months of a low-income informal worker's own income. Only "
              f"{pct(100*a4_['mass at borrowing limit'], 0)} of households are at the limit; the average borrower owes about "
              f"{f(12 * a4_['gross debt / income'] / a4_['borrowers mass'], 1)} months of average income.",
              f"<b>At +2% the 4-state requirement is not realistic.</b> It implies debt of {pct(100*a4_['gross debt / income'])} of income: "
              f"{a4_['gross debt / income'] / DI_MED:.1f} times the 1994–{DI_LAST_Y} median, above every year on record (bank peak "
              f"{pct(100*DI_HI_BANK)}), and above even the upper series with non-bank lenders ({pct(100*DI_HI)}). The 2-state requirement "
              f"({pct(100*a2_['gross debt / income'])}) is close to the median.",
              "<b>Between 0% and −2%, only the 4-state requirement is realistic.</b> The 2-state limit has to become so tight that "
              "debt falls to 1–2% of income, and below −1% no limit works.",
              f"<b>At the data medians, the 4-state requirement is too tight.</b> It implies debt of {pct(100*d4m[0])} and {pct(100*d4m[1])} "
              f"of income, below the lowest year on record ({pct(100*DI_LO)}, {DI.idxmin()}). On this check, the a<sub>min</sub> route "
              "fits the medians in neither model; the ρ route of Claim 9 is the one that works there."])
S += assume_box("D5 (BIS household credit as the debt numerator), D6 (household consumption as the income denominator), "
                "J4 (a requirement is 'realistic' if it lies inside the observed range), C1 (ρ = 0.05 and all other parameters at "
                "thesis values while a<sub>min</sub> moves).")

# ---- Claim 12
th = {t: {m: fa(m, t, "theta_I", None) for m in ("2-state", "4-state")} for t in TARGETS}
box = claim_box("Claim 12 · notebook 8, Step 11b; notebook 8b, Steps 5–6",
               "The informal borrowing premium observed in Argentina is compatible with r* = +2%.", "not")
rows = []
for d, r in PR_ALL.iterrows():
    rows.append([pd.Timestamp(d).strftime("%b %Y"), "all non-bank providers", pct(r["tna %"], 0), pct(r["bank tna %"], 0),
                 f"{f(r['real premium pp'], 1)} pp", report_of(r["source"], r["report page"])])
for g, r in bprem[bprem["group"] != "all"].set_index("group").iterrows():
    rows.append([pd.Timestamp(r["date"]).strftime("%b %Y"), g, pct(r["tna %"], 0), pct(r["bank tna %"], 0),
                 f"{f(r['real premium pp'], 1)} pp", report_of(r["source"], r["report page"])])
S += [KeepTogether(box + table(["Date", "Lender", "Non-bank rate (TNA)", "Bank rate (TNA)", "Real premium", "BCRA source"], rows,
           [0.7 * inch, 1.45 * inch, 1.0 * inch, 0.9 * inch, 0.85 * inch, 1.9 * inch],
           caption="Personal-loan rates of non-bank credit providers and banks (nominal annual rates), and the implied real premium "
                   "(1 + i<sub>non-bank</sub>)/(1 + i<sub>bank</sub>) − 1. Source: BCRA, <i>Informe de Proveedores No Financieros de Crédito</i>."))]
rows = []
for t in TARGETS:
    cell = lambda r: (f"{f(100*r['calibrated value'],1)} pp" if r["status"] == "ok" and str(r["cleared"]) == "True"
                      else ("no robust eq." if r["status"] == "ok" else "not attainable"))
    rows.append([tlab(t), cell(th[t]["2-state"]), cell(th[t]["4-state"]), realistic("theta_I", t)])
S += table(["Target r*", "θ<sub>I</sub> needed, 2-state", "θ<sub>I</sub> needed, 4-state", f"Inside the data range ({f(PR_LO,0)}–{f(PR_HI,0)} pp)"], rows,
           [2.0 * inch, 1.4 * inch, 1.4 * inch, 2.0 * inch],
           caption="Informal premium needed at each target (ρ = 0.05), against the data.")
S += bullets([f"<b>The observed premium is large.</b> Non-bank lenders charge a real premium of {f(PR_ALL['real premium pp'].min(), 0)}–"
              f"{f(PR_HI, 0)} pp over banks, and {f(PR_LO, 0)} pp for the cheapest group (cooperatives and mutuals). The thesis value of 2 pp "
              "is far below every observation.",
              "<b>Papers agree on the order of magnitude.</b> In its survey of informal credit markets, Batini, Kim, Levine and Lotti (2010, "
              "IMF WP/10/42) report that informal lenders' rates are on average much higher than formal rates, range from 0 to 200% a year, "
              "and often exceed 75% a year (citing Besley 1994 and Hoff and Stiglitz 1990). The BCRA links rate differences to "
              f"business models, funding costs and client risk; non-bank clients are riskier: {bfacts['irregular share of non-bank personal loans']:.0%} of their personal loans were "
              f"irregular in February 2026, against {bfacts['irregular share of bank personal loans']:.0%} at banks.",
              "<b>A premium of that size is incompatible with +2% or 0%.</b> At +2% the 2-state model needs a negative premium and the "
              f"4-state model none works; at 0% the 4-state model still has no solution and the 2-state needs "
              f"{f(100*th[0.0]['2-state']['calibrated value'], 0)} pp, below the range.",
              f"<b>It is the size that produces a negative r*.</b> At the 1993–2025 median both models need about "
              f"{f(100*th[MED93]['4-state']['calibrated value'], 0)} pp, and at the 1976–2025 median the 4-state model needs "
              f"{f(100*th[MED50]['4-state']['calibrated value'], 0)} pp. Both are inside the observed range. This is the one data check "
              "that favours negative targets whatever one thinks of the ex-post peso rate as a measure of r*.",
              "<b>Caveat.</b> The premium check does not separate the models at the 1993–2025 median (both pass); it separates them only "
              "at the 1976–2025 median, where the 2-state root does not survive re-solving."])
S += assume_box("D7 (non-bank over bank rates as the informal premium), D8 (ratio of nominal rates as the real premium; rates "
                "are TNA, not effective; the spread includes default compensation), J4, C1 (ρ = 0.05 and thesis a<sub>min</sub> while "
                "θ<sub>I</sub> moves).")

S += [PageBreak()]

# ================================================================ PART IV
S += [P("Part IV. What we learn", "h1"),
      P("1. The +2% target does not favour the 4-state model, and the data confirm it", "h2"),
      P(f"At +2% both models calibrate with admissible discount rates, but the 4-state model needs more impatience (ρ = {f(RHO4S)} vs "
        f"{f(RHO2S)}), a borrowing limit of about {f(a4['|a_min| in months of mean after-tax income'],1)} months of income, and "
        f"household debt of {pct(100*a4['gross debt / income'])} of income, above every year in the BIS data (Claim 11). No informal "
        "premium can make it clear at 2%, and the premia observed in Argentina are far larger than any value compatible with +2% "
        "(Claim 12). The 2-state model needs only modest changes. On this target, the claim that the 2-state model needs strange "
        "parameters is not supported."),
      P("2. Matching r* is a weak test, because only ρ − r* is pinned down", "h2"),
      P("Moving the target moves ρ one-for-one and leaves every household prediction unchanged (Claim 8). A model cannot "
        "be validated or rejected by matching r* alone. The informative content is (a) whether the required ρ is admissible and "
        "(b) what the observable parameters, borrowing limits and premia, must be to fit. Those can be checked against data."),
      P("3. The 4-state model's extra precautionary saving is what lets it fit negative rates", "h2"),
      P("Income risk inside the formal sector (FH ↔ FL) makes 4-state households want to save more at any rate (Claims 2 "
        "and 4). That is a weakness at +2% and a strength at negative rates. At the medians of the actual ex-post real rate, "
        "only the 4-state model can be calibrated with a positive discount rate and an admissible borrowing limit (Claims 9 and 10)."),
      P("4. So the decisive question is what r* corresponds to in Argentina", "h2"),
      P("In the model, r* is the equilibrium return on the only risk-free saving vehicle, traded among households. "
        "Argentina's negative ex-post peso rates mostly reflect surprise inflation, rate caps and financial repression, and "
        "households save largely in dollars and real estate rather than peso deposits. A target based on dollar returns would "
        "likely lie near 0–1%, and in that range Part I's conclusions apply. The thesis should state this choice explicitly and show both readings."),
      P("5. The informal premium is the piece of data that points to negative rates", "h2"),
      P(f"Non-bank lenders in Argentina charge a real premium of {f(PR_LO,0)}–{f(PR_HI,0)} pp over banks, in line with the informal-credit "
        "literature. Holding ρ = 0.05, a premium of that size is what both models need to produce the 1993–2025 median, and what the "
        "4-state model needs for the 1976–2025 median (Claim 12). It is not compatible with +2% or 0%. This argues for a negative target "
        "through a channel that does not rely on the ex-post peso rate. It does not by itself separate the models, and household "
        "debt points the other way: at the medians, the a<sub>min</sub> route implies less debt than Argentina has ever had (Claim 11)."),
      P("6. The policy-relevant difference survives every calibration", "h2"),
      P("Each model calibrated to the same r*, the formalization counterfactual moves in the same direction in both, but the "
        "4-state model predicts that financial constraints respond about one-third as much (Claim 6). That size disagreement, not "
        "a sign reversal, is the defensible contribution for policy evaluation.")]

S += [KeepTogether([P("What the thesis can and cannot claim after these tests", "h2")] + table(["Claim", "Status", "Evidence"],
           [["The 2-state model needs implausible parameters to match the observed real rate", "Not supported at r* ≥ 0; supported only at the negative ex-post medians", "Claims 3, 7, 9, 10"],
            ["The 2-state model 'overestimates' r*", "Not supported: levels are equal by construction once both are calibrated", "Claim 3"],
            ["IH workers are what depresses r*", "Not supported: FH precautionary saving is the driver", "Claim 4"],
            ["Formalization reverses the sign of the borrowers-mass response", "Not supported: it came from a non-equilibrium point", "Claim 6"],
            ["The two models imply different impatience at the same r*", "Supported (gap ≈ 0.024, robust)", "Claims 3, 8"],
            ["The 4-state model implies a much smaller response of credit constraints to formalization", "Supported (about one-third)", "Claim 6"],
            ["Only the 4-state model can rationalise a negative real rate with ρ > 0", "Supported, conditional on the ex-post peso rate being the right counterpart", "Claims 9, 10"],
            ["The 4-state model's credit requirements at +2% are realistic", f"Not supported: implied debt {pct(100*a4['gross debt / income'])} vs {pct(100*DI_LO)}–{pct(100*DI_HI)} observed", "Claim 11"],
            ["The thesis informal premium θ<sub>I</sub> = 2 pp is realistic", f"Not supported: {f(PR_LO,0)}–{f(PR_HI,0)} pp observed", "Claim 12"],
            ["Realistic informal premia are consistent with a negative r*", "Supported (both models at the 1993–2025 median, 4-state at the 1976–2025 median)", "Claim 12"]],
           [2.9 * inch, 2.5 * inch, 1.4 * inch], caption="Summary of claims."))]

S += [P("Next steps: data that would settle the question", "h2")]
S += bullets([f"<b>Calibrate with the observed premium.</b> Set θ<sub>I</sub> to the data (about {f(PR_MED,0)} pp) instead of 2 pp, and "
              "re-calibrate ρ to each target. Part III freed one parameter at a time; this joint calibration would show which ρ each model "
              "needs once the premium is realistic.",
              "<b>Direct data on informal borrowing rates.</b> Non-bank lenders are a proxy: some of their clients are formal, and moneylenders "
              "and store credit outside any registry are missing. A household survey with loan rates by employment status (ENGHo, the "
              "BCRA financial-inclusion survey) would measure θ<sub>I</sub> directly.",
              "<b>Borrower shares</b>, formal and informal (Global Findex, ENGHo), to compare with the untargeted moments in "
              "Table 8 and Figure 9.",
              "<b>A dollar-based real return</b> for Argentine savers, to build a defensible positive r* target.",
              "<b>Re-run the Case A/B/C reform comparison</b> (thesis Section 4.6) with each model at its own calibrated ρ: the "
              "welfare and revenue gaps in the thesis were computed at ρ = 0.05, where the 4-state economy at η = 0.42 did not clear."])
S += [PageBreak()]

# ================================================================ 6. assumptions register
S += [P("6. Assumptions register", "h1"),
      P("Every assumption the results depend on, where it enters, and how it was checked. 'Report' assumptions (R) were made "
        "in writing this document; all others come from the notebooks.")]
REG = [
    ("D1", "Real rate is ex post: r = (1 + i)/(1 + π) − 1 with realised CPI inflation, not expected inflation.", "Claim 1; Part II targets", "Not checked against ex-ante measures; surprise inflation makes ex-post rates more negative."),
    ("D2", "The 50-year series is the GMD short-term rate (CS2, Bordo, ECLAC, OECD, IMF IFS spliced), because Argentina has had no continuous policy rate.", "Claim 1", "BIS policy rate reported for 1993–2025 as a cross-check; also negative on average after 2002."),
    ("D3", "Global Macro Database release 2026_09; forecast rows dropped.", "Claim 1", "–"),
    ("D4", "Data targets use medians, because the means are dominated by 1975–1991 high inflation.", "Claims 9, 10", "The 50-year mean (−11.1%) is far below the median. The 1993–2025 mean (−2.8%) is slightly above its median (−3.6%); it lies between the −2% and −3.56% cases, where the 4-state model already passes alone on ρ and a<sub>min</sub> (Table 4, Figure 12)."),
    ("D5", "Household gross debt is BIS credit to households and NPISHs from all sectors (% of GDP, adjusted for breaks).", "Claims 7, 11", f"An upper series scales it by 1 + {NONBANK:.2f} (BCRA: non-bank credit to individuals relative to bank credit, Feb 2026), in case BIS misses non-bank lenders."),
    ("D6", "Model income (mean after-tax income) is measured by household consumption, % of GDP (World Bank). With bonds in zero net supply, model consumption equals income.", "Claims 7, 11", "Consumption was 62–70% of GDP over 1994–2025; using GDP instead would lower every ratio by about a third and strengthen the Claim 11 conclusion at +2%."),
    ("D7", "The informal premium θ<sub>I</sub> is measured by the personal-loan rate of non-bank credit providers over the bank rate.", "Claim 12", "Biased down: some non-bank clients are formal, and unregistered moneylenders are missing. Literature (Batini et al. 2010) reports informal rates often above 75% a year."),
    ("D8", "Real premium = (1 + i<sub>non-bank</sub>)/(1 + i<sub>bank</sub>) − 1, using nominal annual rates (TNA) from three BCRA reports.", "Claim 12", "TNA understate effective rates under monthly compounding, so the effective spread is larger. The spread includes default compensation (non-bank irregularity 34% vs 14% at banks, Feb 2026), which the model has no counterpart for."),
    ("C1", "Exactly one parameter is freed per calibration; all others stay at thesis values (Section 2, η = 0.42).", "All claims", "Three alternative free parameters tested (ρ, a<sub>min</sub>, θ<sub>I</sub>)."),
    ("C2", "Thesis inputs used verbatim; 2-state γ<sub>I</sub> = 0.30 (thesis text and Table 1) rather than 0.27 (notebook-4 code).", "All claims", "γ<sub>I</sub> = 0.27 gives ρ = 0.0498 instead of 0.0542 (Table 6); the conclusions are unchanged."),
    ("C3", "r* = +2% is an external choice (advisor), not a data moment.", "Part I", "Targets 1–2.5% (Table 6) and 0% to −5.12% (Part II)."),
    ("C4", "r* is the steady-state return on the only risk-free asset, in zero net supply among households, in a closed economy.", "All claims", "Not testable inside the model; it is the key caveat of Part IV, point 4."),
    ("C5", "Thesis borrowing limit a<sub>min</sub> = −0.099 (grid value, no stated source) and θ<sub>I</sub> = 0.02.", "Claims 7, 10", "Both freed in turn."),
    ("N1", "Same solver as thesis notebooks 4 and 5 (implicit upwind finite differences, Δ = 1000, grid I = 800 on [−0.099, 5]).", "All claims", "Reproduces four thesis equilibria to &lt; 5 × 10<super>−7</super> (Table 3)."),
    ("N2", "For r ≤ 0: initial guess V<sub>0</sub> = u(y + ρa)/ρ, warm starts, and a smaller step (Δ = 50) if needed.", "Part II", "Same fixed point: at r = 2% the value functions differ by 3 × 10<super>−10</super>."),
    ("N3", "A solve is invalid if the HJB does not converge or the density has negative mass; invalid points are never used.", "All claims", "Flagged in every scan."),
    ("N4", "'Not attainable' means no sign change of S on the scan grid (a<sub>min</sub> ∈ [−2, −0.01], θ<sub>I</sub> ∈ [−0.05, 1]).", "Claims 7, 10", "Values outside these grids were not tested."),
    ("N5", "Tight borrowing limits are resolved with few grid points below zero.", "Claim 10", "Re-calibrated at I = 2400: changes under 4% except one case at 8% (Table 12)."),
    ("N6", "A fixed-r root that does not clear when re-solved is treated as not attainable (2-state, θ<sub>I</sub>, −5.12%).", "Claim 10", "S(r) is flat at −0.003 over r ∈ [−6.4%, −5.4%]; a knife-edge point."),
    ("N7", "This report's figures come from a re-run of notebooks 8 and 8b on October 30, 2026, with MKL_THREADING_LAYER=SEQUENTIAL "
           "(the threaded MKL layer would not load in the execution environment).", "All figures", "Re-run CSVs compared with the previously committed CSVs; see the note below this table."),
    ("J1", "ρ is plausible if it lies in [0.02, 0.12] per year (advanced-economy models use 0.02–0.06; emerging-market models use quarterly β ≈ 0.97–0.985).", "Claims 3, 9, 10", "The thresholds are editable (RHO_RANGE) in both notebooks."),
    ("J2", "A negative informal premium θ<sub>I</sub> &lt; 0 is not economically meaningful and fails.", "Claims 7, 10", "–"),
    ("J3", "A borrowing limit passes the feasibility rule whenever it exists and clears; its realism is judged separately.", "Claims 7, 10", "Judged against household-debt data in Part III (J4)."),
    ("J4", "A required a<sub>min</sub> or θ<sub>I</sub> is 'realistic' if the implied debt / income or the premium lies inside the observed range.", "Claims 11, 12", "The ranges are wide on purpose (lowest bank-only year to highest year with non-bank lenders; cheapest provider group to highest all-provider average). Two values sit near an edge: 2-state θ<sub>I</sub> at −1% (15.2 pp vs 16 pp) and 4-state debt at −2% (5.0% vs 4.0%)."),
    ("R1", "Numbers in the text are read from the notebooks' result CSVs; a few printed-only values (Tables 3 and 12, r* at ρ = 0.05, the γ<sub>I</sub> and η robustness) are transcribed.", "Report", "Transcribed values are marked in the captions."),
    ("R2", "The negative ρ the 2-state model 'would need' at the medians extrapolates its constant impatience gap; it is not a solve.", "Claims 8, 9", "Figure 10 confirms S &lt; 0 for every ρ ≥ 0.0025."),
    ("R3", "Reading the FH exit rate as a frequent, large income drop interprets the estimated generator.", "Claim 4", "–"),
    ("R4", "That a dollar-based target would be near 0–1% is stated in notebook 8b but not computed from data in the repository.", "Part IV, point 4", "Unverified: flagged as a next step."),
    ("R5", "Argentina's household credit is low.", "Claim 7", f"Verified in Part III: {pct(100*DI_LO)}–{pct(100*DI_HI_BANK)} of income since 1994 (BIS, D5–D6)."),
]
S += table(["ID", "Assumption", "Where used", "How checked / sensitivity"],
           [[i, a, w, c] for i, a, w, c in REG], [0.4 * inch, 3.1 * inch, 1.1 * inch, 2.2 * inch])
REPRO_NOTE = "REPRO_NOTE_PLACEHOLDER"
S += [P(REPRO_NOTE, "small")]

S += [P("Files", "h2"),
      P("Notebooks: <i>8. empirical_claims.ipynb</i> (Part I), <i>8b. empirical_claims_negative_r.ipynb</i> (Part II). Result "
        "tables: <i>data/interest_rates/*.csv</i>. Data benchmarks (raw files and sources): <i>data/interest_rates/benchmarks/</i>. "
        "Figures: <i>data/interest_rates/figures/</i>. This PDF is built by "
        "<i>9. empirical_claims_report.py</i>.", "small"),
      P("References", "h2"),
      P("Aguiar, M. and Gopinath, G. (2007). Emerging market business cycles: the cycle is the trend. <i>Journal of Political Economy</i> 115(1). "
        "· Banco Central de la República Argentina. <i>Informe de Proveedores No Financieros de Crédito</i>, December 2023, November 2025 and June 2026. "
        "· Bank for International Settlements. Total credit to the non-financial sector, series Q.AR.H.A.M.770.A (downloaded October 30, 2026). "
        "· Batini, N., Kim, Y.-B., Levine, P. and Lotti, E. (2010). Informal labour and credit markets: a survey. IMF Working Paper WP/10/42. "
        "· Besley, T. (1994). How do market failures justify interventions in rural credit markets? <i>World Bank Research Observer</i> 9(1). "
        "· Hoff, K. and Stiglitz, J. E. (1990). Imperfect information and rural credit markets: puzzles and policy perspectives. <i>World Bank Economic Review</i> 4(3). "
        "· Müller, K., Xu, C., Lehbib, M. and Chen, Z. (2025). The Global Macro Database. "
        "· Neumeyer, P. A. and Perri, F. (2005). Business cycles in emerging economies: the role of interest rates. <i>Journal of Monetary Economics</i> 52(2). "
        "· World Bank. World Development Indicators, series NE.CON.PRVT.ZS (downloaded October 30, 2026).", "small")]

if __name__ == "__main__":
    note_path = os.path.join(DATA, "figures", "reproducibility_note.txt")
    note = open(note_path, encoding="utf-8").read().strip() if os.path.exists(note_path) else ""
    S = [P(note, "small") if (isinstance(x, Paragraph) and x.text == REPRO_NOTE) else x for x in S]
    doc = SimpleDocTemplate(OUT, pagesize=letter, leftMargin=0.85 * inch, rightMargin=0.85 * inch, topMargin=0.75 * inch,
                            bottomMargin=0.9 * inch, title="From +2% to negative real rates",
                            author="Giuliana Triberti", subject="Empirical claims of notebooks 8 and 8b")
    doc.build(S, onFirstPage=on_page, onLaterPages=on_page)
    print("wrote", OUT)
