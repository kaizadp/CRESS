# -*- coding: utf-8 -*-
"""
Created on Fri Mar  6 10:45:44 2026

@author: keww938
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_context("talk")
import os
from tqdm import tqdm


datadir = r'E:\ICR_Data\62037_Kaizad/ProcData/'
figure_dir = datadir+'/Figures/'

files_to_ignore = ['failed_samples.csv','combined_stats.csv','processing.log',
                   'combined_stats_2.csv']


df = pd.read_csv(datadir+'combined_stats_2.csv')


qc_df = df[df["is_qc"]].copy()

srfa_df = df[df["sample_name"] == "SRFA"]
spe_blank_df = df[df["sample_name"] == "SPE_Blank"]
blank_df = df[df["sample_name"] == "Blank"]
process_blank_df = df[df["sample_name"] == "ProcessBlank"]

srfa_metrics = [
    "tic",
    "peaks",
    "qc_total_assign",
    "qc_total_CHO",
    "qc_SN_median",
    "qc_wa_OC",
    "qc_wa_HC",
    "qc_wa_mz",
    "qc_wa_AIMod",
    "qc_wa_NOSC",
    "cal_rms",
    "raw_error_median"
]

plt.figure(figsize=(8,5))
sns.stripplot(data=srfa_df, x="measure_rep", y="tic", jitter=True)
sns.boxplot(data=srfa_df, x="measure_rep", y="tic", showcaps=False, boxprops={'facecolor':'None'})
plt.title("SRFA TIC stability")
plt.show()

plt.figure(figsize=(8,5))
sns.stripplot(data=srfa_df, x="measure_rep", y="qc_total_assign", jitter=True)
plt.title("SRFA formula assignments")
plt.show()

fig, ax = plt.subplots(1,3, figsize=(15,4))

sns.boxplot(data=srfa_df, y="qc_wa_OC", ax=ax[0])
sns.boxplot(data=srfa_df, y="qc_wa_HC", ax=ax[1])
sns.boxplot(data=srfa_df, y="qc_wa_AIMod", ax=ax[2])

ax[0].set_title("SRFA O/C")
ax[1].set_title("SRFA H/C")
ax[2].set_title("SRFA AI_mod")

plt.tight_layout()
plt.show()


fig, ax = plt.subplots(1,2, figsize=(10,4))

sns.histplot(srfa_df["cal_rms"], bins=10, ax=ax[0])
ax[0].set_title("Calibration RMS")

sns.histplot(srfa_df["raw_error_median"], bins=10, ax=ax[1])
ax[1].set_title("Median mass error")

plt.show()

blank_metrics = ["tic", "peaks", "qc_total_assign"]

blank_summary = df[df["sample_name"].isin(
    ["SPE_Blank","Blank","ProcessBlank"]
)].groupby("sample_name")[blank_metrics].describe()

print(blank_summary)

compare_df = df[df["sample_name"].isin(
    ["SRFA","SPE_Blank","Blank","ProcessBlank"]
)]

plt.figure(figsize=(8,6))
sns.boxplot(data=compare_df, x="sample_name", y="peaks")
plt.title("Peaks in QC samples")
plt.show()

plt.figure(figsize=(8,6))
sns.boxplot(data=compare_df, x="sample_name", y="tic")
plt.title("TIC in QC samples")
plt.show()

qc_variability = srfa_df[srfa_metrics].agg(["mean","std","min","max"])

print(qc_variability.T)


df["plot_group"] = "Sample_" + df["solution"].astype(str)

df.loc[df["sample_name"] == "SRFA", "plot_group"] = "QC_SRFA"
df.loc[df["sample_name"] == "SPE_Blank", "plot_group"] = "QC_SPE_Blank"
df.loc[df["sample_name"] == "Blank", "plot_group"] = "QC_Blank"
df.loc[df["sample_name"] == "ProcessBlank", "plot_group"] = "QC_ProcessBlank"

print(df["plot_group"].value_counts())

plt.figure(figsize=(10,6))

sns.boxplot(
    data=df,
    x="plot_group",
    y="tic",
    showfliers=False
)

sns.stripplot(
    data=df,
    x="plot_group",
    y="tic",
    color="black",
    alpha=0.6,
    jitter=True
)

plt.xticks(rotation=45)
plt.ylabel("TIC")
plt.xlabel("")
plt.title("TIC distribution across samples and QC")
plt.tight_layout()
plt.show()

plt.figure(figsize=(10,6))

sns.scatterplot(
    data=df,
    x="hystarid",
    y="tic",
    hue="plot_group",
    s=80
)

plt.title("TIC vs injection order")
plt.xlabel("Injection ID")
plt.ylabel("TIC")
plt.tight_layout()
plt.show()

plt.figure(figsize=(10,6))

sns.scatterplot(
    data=df,
    x="tic",
    y="peaks",
    hue="plot_group",
    s=80
)

plt.title("Spectral richness vs TIC")
plt.xscale("log")
plt.tight_layout()
plt.show()

###################################
sample_df = df[
    (df["is_qc"] == False) |
    (df["sample_name"] == "SRFA")
].copy()
sample_df = sample_df[
    ~sample_df["sample_name"].isin(["Blank","ProcessBlank","SPE_Blank"])
]

sample_df["plot_group"] = sample_df["solution"]

sample_df.loc[sample_df["sample_name"] == "SRFA", "plot_group"] = "SRFA"

print(sample_df["plot_group"].value_counts())


plt.figure(figsize=(8,6))

sns.boxplot(
    data=sample_df,
    x="plot_group",
    y="tic",
    showfliers=False
)

sns.stripplot(
    data=sample_df,
    x="plot_group",
    y="tic",
    color="black",
    alpha=0.6,
    jitter=True
)

plt.ylabel("TIC")
plt.xlabel("")
plt.title("TIC comparison (Samples vs SRFA)")
plt.tight_layout()
plt.show()

plt.figure(figsize=(8,6))

sns.boxplot(
    data=sample_df,
    x="plot_group",
    y="peaks",
    showfliers=False
)

sns.stripplot(
    data=sample_df,
    x="plot_group",
    y="peaks",
    color="black",
    alpha=0.6,
    jitter=True
)

plt.ylabel("Peak count")
plt.xlabel("")
plt.title("Spectral richness")
plt.tight_layout()
plt.show()


plt.figure(figsize=(8,6))

sns.boxplot(
    data=sample_df,
    x="plot_group",
    y="qc_total_assign",
    showfliers=False
)

sns.stripplot(
    data=sample_df,
    x="plot_group",
    y="qc_total_assign",
    color="black",
    alpha=0.6,
    jitter=True
)

plt.ylabel("Assigned formulas")
plt.xlabel("")
plt.title("Formula assignments")
plt.tight_layout()
plt.show()

plt.figure(figsize=(8,6))

sns.scatterplot(
    data=sample_df,
    x="peaks",
    y="qc_total_assign",
    hue="plot_group",
    s=80
)

plt.title("Peak count vs assignments")
plt.tight_layout()
plt.show()


samples = df[df["is_qc"] == False].copy()
samples = samples[samples["sample_name"] != "ProcessBlank"]

samples["sample_name"].value_counts()


plt.figure(figsize=(8,6))

sns.boxplot(
    data=samples,
    x="solution",
    y="peaks"
)

sns.stripplot(
    data=samples,
    x="solution",
    y="peaks",
    color="black",
    jitter=True,
    alpha=0.6
)

plt.title("Peak counts by extraction solution")
plt.ylabel("Peaks")
plt.show()

plt.figure(figsize=(8,6))

sns.boxplot(
    data=samples,
    x="solution",
    y="qc_total_assign"
)

sns.stripplot(
    data=samples,
    x="solution",
    y="qc_total_assign",
    color="black",
    jitter=True,
    alpha=0.6
)

plt.title("Formula assignments by extraction")
plt.show()

plt.figure(figsize=(12,6))

sns.boxplot(
    data=samples,
    x="sample_name",
    y="peaks",
    hue="solution"
)

plt.xticks(rotation=45)
plt.title("Peak counts by site and extraction")
plt.tight_layout()
plt.show()


plt.figure(figsize=(12,6))

sns.boxplot(
    data=samples,
    x="sample_name",
    y="qc_total_assign",
    hue="solution"
)

plt.xticks(rotation=45)
plt.tight_layout()
plt.show()


metrics = [
    "qc_wa_OC",
    "qc_wa_HC",
    "qc_wa_AIMod",
    "qc_wa_NOSC"
]

for m in metrics:
    
    plt.figure(figsize=(8,6))
    
    sns.boxplot(
        data=samples,
        x="solution",
        y=m
    )
    
    sns.stripplot(
        data=samples,
        x="solution",
        y=m,
        color="black",
        jitter=True,
        alpha=0.6
    )
    
    plt.title(m)
    plt.show()
    
    
plt.figure(figsize=(8,6))

sns.boxplot(
    data=samples,
    x="solution",
    y="qc_mass_median"
)

sns.stripplot(
    data=samples,
    x="solution",
    y="qc_mass_median",
    color="black",
    jitter=True
)

plt.title("Median mass by extraction")
plt.show()


plt.figure(figsize=(8,6))

sns.scatterplot(
    data=samples,
    x="tic",
    y="qc_total_assign",
    hue="solution",
    style="sample_name",
    s=90
)

plt.xscale("log")
plt.title("Signal vs assignments")
plt.show()



samples = df[df["is_qc"] == False].copy()

samples = samples[
    ~samples["sample_name"].isin(["Blank","ProcessBlank","SPE_Blank"])
]


plt.figure(figsize=(10,7))

sns.scatterplot(
    data=samples,
    x="qc_wa_OC",
    y="qc_wa_HC",
    hue="solution",
    style="sample_name",
    s=120
)

plt.xlabel("Weighted Average O/C")
plt.ylabel("Weighted Average H/C")
plt.title("Pseudo Van Krevelen (sample weighted averages)")

plt.xlim(0.2, 1.0)
plt.ylim(0.5, 2.0)

plt.grid(alpha=0.3)

plt.legend(bbox_to_anchor=(1.05,1), loc="upper left")

plt.tight_layout()
plt.show()


sns.scatterplot(
    data=samples,
    x="qc_wa_OC",
    y="qc_wa_HC",
    hue="solution",
    style="sample_name",
    size="qc_total_assign",
    sizes=(60,250)
)


plot_df = df.copy()

plot_df = plot_df[
    ~plot_df["sample_name"].isin(["Blank", "ProcessBlank", "SPE_Blank"])
]


plt.figure(figsize=(15,8))

sns.boxplot(
    data=plot_df,
    x="sample_name",
    y="organic_ratio",
    hue="solution"
)

sns.stripplot(
    data=plot_df,
    x="sample_name",
    y="organic_ratio",
    hue="solution",
    dodge=True,
    color="black",
    alpha=0.5
)

plt.xticks(rotation=45)
plt.title("Organic peak ratio")
plt.ylabel("Organic / Inorganic")
plt.tight_layout()
plt.show()

plt.figure(figsize=(15,8))

sns.boxplot(
    data=plot_df,
    x="sample_name",
    y="c13_ratio",
    hue="solution"
)

sns.stripplot(
    data=plot_df,
    x="sample_name",
    y="c13_ratio",
    hue="solution",
    dodge=True,
    color="black",
    alpha=0.5
)

plt.xticks(rotation=45)
plt.title("13C isotope ratio")
plt.ylabel("13C peak ratio")
plt.tight_layout()
plt.show()


plt.figure(figsize=(15,8))

sns.boxplot(
    data=plot_df,
    x="sample_name",
    y="qc_pc_assign",
    hue="solution"
)

sns.stripplot(
    data=plot_df,
    x="sample_name",
    y="qc_pc_assign",
    hue="solution",
    dodge=True,
    color="black",
    alpha=0.5
)

plt.xticks(rotation=45)
plt.title("Percent of peaks assigned formulas")
plt.ylabel("% assigned")
plt.tight_layout()
plt.show()