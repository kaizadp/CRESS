# -*- coding: utf-8 -*-
"""
Created on Fri Mar  6 11:29:50 2026

@author: keww938
"""

import pandas as pd
import os
from tqdm import tqdm

datadir = r'1-data/FTICR_Kew/ProcData/'

meta = pd.read_csv(datadir + "combined_stats_2.csv")

# remove QC and blanks
meta_samples = meta[
    (meta["is_qc"] == False) &
    (~meta["sample_name"].isin(["Blank","ProcessBlank","SPE_Blank"]))
].copy()

print(meta_samples.shape)

records = []

for _, row in tqdm(meta_samples.iterrows(), total=len(meta_samples)):

    fname = row["datafile"].replace(".d",".csv")
    fpath = os.path.join(datadir, fname)

    if not os.path.exists(fpath):
        continue

    df = pd.read_csv(fpath)

    # filter features
    df = df[
        (df["Heteroatom Class"] != "unassigned") &
        (df["Is Isotopologue"] == 0)
    ].copy()

    # add metadata
    df["sample_name"] = row["sample_name"]
    df["solution"] = row["solution"]
    df["sample_rep"] = row["sample_rep"]
    df["measure_rep"] = row["measure_rep"]
    df["datafile"] = row["datafile"]

    records.append(df)

features_long = pd.concat(records, ignore_index=True)

print(features_long.shape)


features_long["feature_id"] = (
    features_long["Molecular Formula"] +
    "_" +
    features_long["Ion Type"]
)

det = features_long[
    ["feature_id","sample_name","solution","sample_rep","measure_rep"]
].drop_duplicates()


measure_counts = (
    det
    .groupby(["feature_id","sample_name","solution","sample_rep"])
    ["measure_rep"]
    .nunique()
    .reset_index(name="n_measure_rep")
)
measure_pass = measure_counts[measure_counts["n_measure_rep"] == 2]

sample_counts = (
    measure_pass
    .groupby(["feature_id","sample_name","solution"])
    ["sample_rep"]
    .nunique()
    .reset_index(name="n_sample_rep")
)
consensus_features = sample_counts[sample_counts["n_sample_rep"] >= 2]


filtered = features_long.merge(
    consensus_features[
        ["feature_id","sample_name","solution"]
    ],
    on=["feature_id","sample_name","solution"]
)

feature_summary = (
    filtered
    .groupby(["sample_name","solution","feature_id"])
    .agg({
        "O/C":"mean",
        "H/C":"mean",
        "Peak Height":"mean",
        "Peak Area":"mean",
        "m/z":"mean",
        "Molecular Formula":"first",
        "Ion Type":"first"
    })
    .reset_index()
)


import seaborn as sns
import matplotlib.pyplot as plt

plt.figure(figsize=(8,7))

sns.scatterplot(
    data=feature_summary,
    x="O/C",
    y="H/C",
    hue="solution",
    alpha=0.6,
    s=20
)

plt.xlabel("O/C")
plt.ylabel("H/C")
plt.title("Van Krevelen diagram (consensus features)")
plt.xlim(0,1)
plt.ylim(0,2)

plt.show()


plt.figure(figsize=(8,5))

sns.histplot(
    data=feature_summary,
    x="H/C",
    hue="solution",
    bins=40,
    element="step"
)

plt.title("H/C distribution")
plt.show()


plt.figure(figsize=(8,5))

sns.histplot(
    data=feature_summary,
    x="O/C",
    hue="solution",
    bins=40,
    element="step"
)

plt.title("O/C distribution")
plt.show()


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import math

def _as_series(df, col):
    if col in df.columns:
        return pd.to_numeric(df[col], errors="coerce")
    return pd.Series(np.nan, index=df.index, dtype=float)

def _valid_formula_mask(C):
    return C.notna() & (C > 0)

def AIModCalc(df):
    """Calculate modified aromaticity index (AI_mod)."""
    C = _as_series(df, 'C')
    H = _as_series(df, 'H')
    O = _as_series(df, 'O')
    N = _as_series(df, 'N')
    S = _as_series(df, 'S')
    P = _as_series(df, 'P')

    valid = _valid_formula_mask(C)

    N = N.where(valid, np.nan).fillna(0).where(valid, np.nan)
    S = S.where(valid, np.nan).fillna(0).where(valid, np.nan)
    P = P.where(valid, np.nan).fillna(0).where(valid, np.nan)

    C = C.where(valid, np.nan)
    H = H.where(valid, np.nan)
    O = O.where(valid, np.nan)

    numerator = 1 + C - 0.5 * O - S - 0.5 * (N + P + H)
    denominator = C - 0.5 * O - N - S - P

    with np.errstate(divide='ignore', invalid='ignore'):
        ai = numerator / denominator

    ai = ai.replace([np.inf, -np.inf], np.nan)
    ai = ai.mask(~valid)
    ai = ai.mask(ai < 0, 0)
    return ai


def NOSCcalc(df):
    """Calculate nominal oxidation state of carbon (NOSC)."""
    C = _as_series(df, 'C')
    H = _as_series(df, 'H')
    O = _as_series(df, 'O')
    N = _as_series(df, 'N')
    S = _as_series(df, 'S')
    P = _as_series(df, 'P')

    valid = _valid_formula_mask(C)

    N = N.where(valid, np.nan).fillna(0).where(valid, np.nan)
    S = S.where(valid, np.nan).fillna(0).where(valid, np.nan)
    P = P.where(valid, np.nan).fillna(0).where(valid, np.nan)

    C = C.where(valid, np.nan)
    H = H.where(valid, np.nan)
    O = O.where(valid, np.nan)

    with np.errstate(divide='ignore', invalid='ignore'):
        nosc = 4 - ((4 * C + H - 2 * O - 3 * N + 5 * P - 2 * S) / C)

    nosc = nosc.replace([np.inf, -np.inf], np.nan)
    nosc = nosc.mask(~valid)
    return nosc

feature_summary = (
    filtered
    .groupby(["sample_name", "solution", "feature_id"])
    .agg({
        "Molecular Formula": "first",
        "Ion Type": "first",
        "m/z": "mean",
        "Peak Height": "mean",
        "Peak Area": "mean",
        "O/C": "mean",
        "H/C": "mean",
        "C": "first",
        "H": "first",
        "O": "first",
        "N": "first",
        "P": "first",
        "S": "first",
    })
    .reset_index()
)

feature_summary["AIMod"] = AIModCalc(feature_summary)
feature_summary["NOSC"] = NOSCcalc(feature_summary)

print(feature_summary[["AIMod", "NOSC"]].describe())

print("AIMod missing:", feature_summary["AIMod"].isna().sum())
print("NOSC missing:", feature_summary["NOSC"].isna().sum())


presence = (
    feature_summary
    .groupby(["sample_name", "solution", "feature_id"])
    .size()
    .reset_index(name="present")
)

presence_wide = (
    presence
    .pivot_table(
        index=["sample_name", "feature_id"],
        columns="solution",
        values="present",
        fill_value=0
    )
    .reset_index()
)

presence_wide["vk_group"] = np.select(
    [
        (presence_wide.get("H2O", 0) > 0) & (presence_wide.get("0.5M HCl", 0) > 0),
        (presence_wide.get("H2O", 0) > 0) & (presence_wide.get("0.5M HCl", 0) == 0),
        (presence_wide.get("H2O", 0) == 0) & (presence_wide.get("0.5M HCl", 0) > 0),
    ],
    [
        "Common",
        "H2O only",
        "0.5M HCl only",
    ],
    default="Other"
)

print(presence_wide["vk_group"].value_counts())

feature_summary = feature_summary.merge(
    presence_wide[["sample_name", "feature_id", "vk_group"]],
    on=["sample_name", "feature_id"],
    how="left"
)

sample_order = sorted(feature_summary["sample_name"].unique())

n = len(sample_order)
ncols = 3
nrows = math.ceil(n / ncols)

fig, axes = plt.subplots(nrows, ncols, figsize=(6 * ncols, 5 * nrows), sharex=True, sharey=True)
axes = np.array(axes).reshape(-1)

palette = {
    "Common": "gray",
    "H2O only": "#ff7f0e",
    "0.5M HCl only": "#1f77b4",
}

for ax, sample in zip(axes, sample_order):
    sub = feature_summary[feature_summary["sample_name"] == sample].copy()

    sns.scatterplot(
        data=sub,
        x="O/C",
        y="H/C",
        hue="vk_group",
        hue_order=["Common", "H2O only", "0.5M HCl only"],
        palette=palette,
        s=18,
        alpha=0.7,
        linewidth=0,
        ax=ax,
        legend=False
    )

    ax.set_title(sample)
    ax.set_xlim(0, 1.0)
    ax.set_ylim(0, 2.2)
    ax.grid(alpha=0.2)

for ax in axes[n:]:
    ax.axis("off")

handles, labels = axes[0].get_legend_handles_labels()
fig.legend(handles, labels, loc="upper right", frameon=True)
fig.suptitle("Van Krevelen by sample: common vs solution-unique consensus features", y=0.995)
plt.tight_layout()
plt.show()

sample_order = sorted(feature_summary["sample_name"].unique())

n = len(sample_order)
ncols = 3
nrows = math.ceil(n / ncols)

fig, axes = plt.subplots(nrows, ncols, figsize=(6 * ncols, 5 * nrows), sharex=True, sharey=True)
axes = np.array(axes).reshape(-1)

for ax, sample in zip(axes, sample_order):
    sub = feature_summary[feature_summary["sample_name"] == sample].copy()

    sns.scatterplot(
        data=sub,
        x="O/C",
        y="H/C",
        hue="vk_group",
        size="Peak Area",
        sizes=(8, 60),
        hue_order=["Common", "H2O only", "0.5M HCl only"],
        palette=palette,
        alpha=0.65,
        linewidth=0,
        ax=ax,
        legend=False
    )

    ax.set_title(sample)
    ax.set_xlim(0, 1.0)
    ax.set_ylim(0, 2.2)
    ax.grid(alpha=0.2)

for ax in axes[n:]:
    ax.axis("off")

fig.suptitle("Van Krevelen by sample with solution overlap and abundance", y=0.995)
plt.tight_layout()
plt.show()

vk_counts = (
    feature_summary[["sample_name", "feature_id", "vk_group"]]
    .drop_duplicates()
    .groupby(["sample_name", "vk_group"])
    .size()
    .unstack(fill_value=0)
)

print(vk_counts)


fig, axes = plt.subplots(1, 2, figsize=(12, 5))

sns.histplot(
    data=feature_summary,
    x="AIMod",
    hue="solution",
    bins=50,
    element="step",
    stat="count",
    common_norm=False,
    ax=axes[0]
)
axes[0].set_title("AIMod distribution")

sns.histplot(
    data=feature_summary,
    x="NOSC",
    hue="solution",
    bins=50,
    element="step",
    stat="count",
    common_norm=False,
    ax=axes[1]
)
axes[1].set_title("NOSC distribution")

plt.tight_layout()
plt.show()


sns.kdeplot(
    data=feature_summary,
    x="AIMod",
    hue="vk_group",
    hue_order=["Common", "H2O only", "0.5M HCl only"],
    palette=palette,
    common_norm=False,
    ax=axes[0]
)
axes[0].set_title("AIMod by overlap class")

sns.kdeplot(
    data=feature_summary,
    x="NOSC",
    hue="vk_group",
    hue_order=["Common", "H2O only", "0.5M HCl only"],
    palette=palette,
    common_norm=False,
    ax=axes[1]
)
axes[1].set_title("NOSC by overlap class")

plt.tight_layout()
plt.show()



import math
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

sample_order = sorted(feature_summary["sample_name"].unique())

n = len(sample_order)
ncols = 3
nrows = math.ceil(n / ncols)

fig, axes = plt.subplots(
    nrows, ncols,
    figsize=(5.5 * ncols, 4.5 * nrows),
    sharex=True, sharey=True
)
axes = np.array(axes).reshape(-1)

palette = {
    "Common": "#bdbdbd",
    "H2O only": "#e68613",
    "0.5M HCl only": "#2b83ba",
}

for ax, sample in zip(axes, sample_order):
    sub = feature_summary[feature_summary["sample_name"] == sample].copy()

    # plot common first as faint background
    common = sub[sub["vk_group"] == "Common"]
    h2o_only = sub[sub["vk_group"] == "H2O only"]
    hcl_only = sub[sub["vk_group"] == "0.5M HCl only"]

    ax.scatter(
        common["O/C"], common["H/C"],
        s=8, c=palette["Common"], alpha=0.25, linewidths=0
    )

    ax.scatter(
        h2o_only["O/C"], h2o_only["H/C"],
        s=10, c=palette["H2O only"], alpha=0.65, linewidths=0
    )

    ax.scatter(
        hcl_only["O/C"], hcl_only["H/C"],
        s=10, c=palette["0.5M HCl only"], alpha=0.65, linewidths=0
    )

    ax.set_title(sample, fontsize=11)
    ax.set_xlim(0, 1.0)
    ax.set_ylim(0.2, 2.1)
    ax.grid(alpha=0.15)

for ax in axes[n:]:
    ax.axis("off")

for ax in axes[::ncols]:
    ax.set_ylabel("H/C")
for ax in axes[-ncols:]:
    ax.set_xlabel("O/C")

from matplotlib.lines import Line2D
legend_handles = [
    Line2D([0], [0], marker='o', color='w', label='Common',
           markerfacecolor=palette["Common"], markersize=7, alpha=0.7),
    Line2D([0], [0], marker='o', color='w', label='H2O only',
           markerfacecolor=palette["H2O only"], markersize=7, alpha=0.9),
    Line2D([0], [0], marker='o', color='w', label='0.5M HCl only',
           markerfacecolor=palette["0.5M HCl only"], markersize=7, alpha=0.9),
]

fig.legend(handles=legend_handles, loc="upper right", frameon=True)
fig.suptitle("Van Krevelen by sample", y=0.995, fontsize=16)
plt.tight_layout()
plt.show()



sample_order = sorted(feature_summary["sample_name"].unique())

n = len(sample_order)
ncols = 3
nrows = math.ceil(n / ncols)

fig, axes = plt.subplots(
    nrows, ncols,
    figsize=(5.5 * ncols, 4.5 * nrows),
    sharex=True, sharey=True
)
axes = np.array(axes).reshape(-1)

for ax, sample in zip(axes, sample_order):
    sub = feature_summary[feature_summary["sample_name"] == sample].copy()

    ax.scatter(
        sub["O/C"], sub["H/C"],
        s=8, c="lightgray", alpha=0.20, linewidths=0
    )

    h2o_only = sub[sub["vk_group"] == "H2O only"]
    hcl_only = sub[sub["vk_group"] == "0.5M HCl only"]

    ax.scatter(
        h2o_only["O/C"], h2o_only["H/C"],
        s=12, c="#e68613", alpha=0.75, linewidths=0
    )

    ax.scatter(
        hcl_only["O/C"], hcl_only["H/C"],
        s=12, c="#2b83ba", alpha=0.75, linewidths=0
    )

    ax.set_title(sample, fontsize=11)
    ax.set_xlim(0, 1.0)
    ax.set_ylim(0.2, 2.1)
    ax.grid(alpha=0.15)

for ax in axes[n:]:
    ax.axis("off")

for ax in axes[::ncols]:
    ax.set_ylabel("H/C")
for ax in axes[-ncols:]:
    ax.set_xlabel("O/C")

legend_handles = [
    Line2D([0], [0], marker='o', color='w', label='All consensus features',
           markerfacecolor='lightgray', markersize=7, alpha=0.8),
    Line2D([0], [0], marker='o', color='w', label='H2O only',
           markerfacecolor="#e68613", markersize=7, alpha=0.9),
    Line2D([0], [0], marker='o', color='w', label='0.5M HCl only',
           markerfacecolor="#2b83ba", markersize=7, alpha=0.9),
]

fig.legend(handles=legend_handles, loc="upper right", frameon=True)
fig.suptitle("Van Krevelen by sample: solution-unique features highlighted", y=0.995, fontsize=16)
plt.tight_layout()
plt.show()


sample_order = sorted(feature_summary["sample_name"].unique())

n = len(sample_order)
ncols = 3
nrows = math.ceil(n / ncols)

fig, axes = plt.subplots(
    nrows, ncols,
    figsize=(5.5 * ncols, 4.5 * nrows),
    sharex=True, sharey=True
)
axes = np.array(axes).reshape(-1)

for ax, sample in zip(axes, sample_order):
    sub = feature_summary[feature_summary["sample_name"] == sample].copy()

    h2o = sub[sub["solution"] == "H2O"]
    hcl = sub[sub["solution"] == "0.5M HCl"]

    sns.kdeplot(
        data=h2o, x="O/C", y="H/C",
        fill=True, levels=6, thresh=0.05,
        alpha=0.45, color="#e68613", ax=ax
    )

    sns.kdeplot(
        data=hcl, x="O/C", y="H/C",
        fill=True, levels=6, thresh=0.05,
        alpha=0.45, color="#2b83ba", ax=ax
    )

    ax.set_title(sample, fontsize=11)
    ax.set_xlim(0, 1.0)
    ax.set_ylim(0.2, 2.1)
    ax.grid(alpha=0.15)

for ax in axes[n:]:
    ax.axis("off")

for ax in axes[::ncols]:
    ax.set_ylabel("H/C")
for ax in axes[-ncols:]:
    ax.set_xlabel("O/C")

legend_handles = [
    Line2D([0], [0], color="#e68613", lw=8, alpha=0.6, label="H2O"),
    Line2D([0], [0], color="#2b83ba", lw=8, alpha=0.6, label="0.5M HCl"),
]
fig.legend(handles=legend_handles, loc="upper right", frameon=True)

fig.suptitle("Van Krevelen density by sample and solution", y=0.995, fontsize=16)
plt.tight_layout()
plt.show()


vk_counts_plot = (
    vk_counts[["Common", "H2O only", "0.5M HCl only"]]
    .copy()
)

ax = vk_counts_plot.plot(
    kind="bar",
    stacked=True,
    figsize=(10, 6),
    color=["#bdbdbd", "#e68613", "#2b83ba"]
)

plt.ylabel("Number of consensus features")
plt.xlabel("Sample")
plt.title("Feature overlap by sample")
plt.xticks(rotation=45, ha="right")
plt.tight_layout()
plt.show()

vk_frac_plot = vk_counts_plot.div(vk_counts_plot.sum(axis=1), axis=0)

ax = vk_frac_plot.plot(
    kind="bar",
    stacked=True,
    figsize=(10, 6),
    color=["#bdbdbd", "#e68613", "#2b83ba"]
)

plt.ylabel("Fraction of consensus features")
plt.xlabel("Sample")
plt.title("Relative overlap by sample")
plt.xticks(rotation=45, ha="right")
plt.tight_layout()
plt.show()
