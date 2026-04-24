#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on February 5th 2026

@author: keww938

This script processes the MONET FY24 WEOM Data
It uses the same rules as FY23 WEOM for consistency

Requires CoreMS 3.9.x on Python 3.11. Activate the CoreMS
environment before running, e.g. on Windows PowerShell:
    C:\Tools\activate_corems.ps1

Data processed February 5th 2026
"""

##############################################################################
# IMPORTS & CONFIGURATION
##############################################################################

# Standard library imports
from pathlib import Path
from collections import defaultdict
from datetime import datetime
from typing import Optional, Dict, Any, List
from dataclasses import dataclass, asdict
import os
import re
import sys
import threading
import multiprocessing
import logging
import logging.handlers
import concurrent.futures

# Third-party imports
import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')  # non-interactive backend for multiprocessing safety
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import seaborn as sns
from tqdm import tqdm

# CoreMS imports
from corems.encapsulation.factory.parameters import MSParameters
from corems.transient.input.brukerSolarix import ReadBrukerSolarix
from corems.mass_spectrum.calc.Calibration import MzDomainCalibration
from corems.molecular_id.search.molecularFormulaSearch import SearchMolecularFormulas
from corems.mass_spectrum.calc.MeanResolvingPowerFilter import MeanResolvingPowerFilter
from corems.encapsulation.factory.parameters import hush_output

##############################################################################
# GLOBAL CONFIGURATION
##############################################################################

TEST_MODE = False  # Set to True to only validate filenames without processing
multithread = True
max_workers = 32
hush = True
sns.set_context('talk')

# Data directory locations
DATA_DIR = Path(r'E:\ICR_Data\62037_Kaizad\RawData')
OUTPUT_DIR = Path(r'E:\ICR_Data\62037_Kaizad\ProcData')
FIG_OUTPUT = OUTPUT_DIR / 'Figures'
REFMASLIST_DIR = Path(r'E:\ICR_Data\refmasslists/')
REFMASSLIST = REFMASLIST_DIR / 'Hawkes_neg.ref'

if hush:
    hush_output()


##############################################################################
# PARAMETER & SETTINGS FUNCTIONS
# Modify these to adjust processing behavior
##############################################################################

def set_mf_search_settings(mass_spectrum, docker=True):
    """Configure molecular formula search parameters."""
    if docker:
        mass_spectrum.molecular_search_settings.url_database = "postgresql+psycopg2://coremsappdb:coremsapppnnl@localhost:5432/coremsapp"
    else:
        mass_spectrum.molecular_search_settings.url_database = None
    
    mass_spectrum.molecular_search_settings.db_chunk_size = 1000
    mass_spectrum.molecular_search_settings.error_method = 'None'
    mass_spectrum.molecular_search_settings.score_method = 'prob_score'

    mass_spectrum.molecular_search_settings.min_ppm_error  = -0.25
    mass_spectrum.molecular_search_settings.max_ppm_error = 0.25

    mass_spectrum.molecular_search_settings.min_dbe = 0
    mass_spectrum.molecular_search_settings.max_dbe = 40
    
    mass_spectrum.molecular_search_settings.min_hc_filter = 0.2

    mass_spectrum.molecular_search_settings.use_isotopologue_filter = False
    mass_spectrum.molecular_search_settings.min_abun_error = -30
    mass_spectrum.molecular_search_settings.max_abun_error = 70
    
    mass_spectrum.molecular_search_settings.use_min_peaks_filter = True
    mass_spectrum.molecular_search_settings.min_peaks_per_class = 10

    mass_spectrum.molecular_search_settings.usedAtoms['C'] = (1, 90)
    mass_spectrum.molecular_search_settings.usedAtoms['H'] = (4, 200)
    mass_spectrum.molecular_search_settings.usedAtoms['O'] = (1, 23)
    mass_spectrum.molecular_search_settings.usedAtoms['N'] = (0, 3)
    mass_spectrum.molecular_search_settings.usedAtoms['S'] = (0, 2)
    mass_spectrum.molecular_search_settings.usedAtoms['Cl'] = (0, 0)
    mass_spectrum.molecular_search_settings.usedAtoms['Br'] = (0, 0)
    mass_spectrum.molecular_search_settings.usedAtoms['P'] = (0, 1)
    mass_spectrum.molecular_search_settings.usedAtoms['Na'] = (0, 0)
    mass_spectrum.molecular_search_settings.adduct_atoms_neg = ['Cl']

    mass_spectrum.molecular_search_settings.isProtonated = True
    mass_spectrum.molecular_search_settings.isRadical = False
    mass_spectrum.molecular_search_settings.isAdduct = False

    
def set_other_params():
    """Configure other CoreMS processing parameters."""
    MSParameters.mass_spectrum.noise_threshold_method = 'log' 
    MSParameters.mass_spectrum.noise_threshold_log_nsigma = 8
    MSParameters.ms_peak.legacy_resolving_power = False
    MSParameters.mass_spectrum.picking_point_extrapolate = 0 
    MSParameters.ms_peak.peak_min_prominence_percent = 0.001
    MSParameters.mass_spectrum.noise_min_mz = 150
    MSParameters.mass_spectrum.noise_max_mz = 800
    MSParameters.mass_spectrum.min_picking_mz = 150
    MSParameters.mass_spectrum.max_picking_mz = 800
    MSParameters.transient.number_of_zero_fills = 1
    
    # Mass recalibration parameters
    MSParameters.mass_spectrum.calib_pol_order = 2
    MSParameters.mass_spectrum.calib_sn_threshold = 12
    MSParameters.mass_spectrum.calibration_ref_match_method = 'merged'
    MSParameters.mass_spectrum.calibration_ref_match_tolerance = 0.001


##############################################################################
# UTILITY & HELPER FUNCTIONS
# These provide low-level utilities for other functions
##############################################################################

def _as_series(df, col):
    """Return column as Series; NaN if missing, index-aligned."""
    if col in df.columns:
        return pd.to_numeric(df[col], errors="coerce")
    return pd.Series(np.nan, index=df.index, dtype="float64")


def _valid_formula_mask(C):
    """Create mask for valid formulas (C > 0 and integral)."""
    is_num = np.isfinite(C.values)
    Cv = C.values.copy()
    ok = np.zeros_like(is_num, dtype=bool)
    ok[is_num] = (Cv[is_num] > 0) & (np.floor(Cv[is_num]) == Cv[is_num])
    return pd.Series(ok, index=C.index)


def _parse_token(tok):
    """
    Parse a single formula token like 'C8', 'H', 'O3', or '13C1'.
    Returns (element_symbol, count) ignoring isotope mass numbers.
    """
    tok = re.sub(r'^\d+', '', tok)  # Remove leading isotope mass numbers
    m = re.match(r'^([A-Z][a-z]?)(\d*)$', tok)
    if not m:
        return None, 0
    el, cnt = m.group(1), m.group(2)
    return el, int(cnt) if cnt else 1


def _formula_to_base_counts(formula):
    """Convert formula string like 'C7 H8 O3 13C1' -> {'C': 8, 'H': 8, 'O': 3}."""
    if not isinstance(formula, str) or not formula.strip():
        return None
    counts = defaultdict(int)
    for tok in formula.split():
        el, n = _parse_token(tok)
        if el:
            counts[el] += n
    return dict(counts) if counts else None


def _counts_to_hill_string(counts):
    """Convert element-count dict to Hill-style string 'C8 H8 O3 ...'."""
    if not counts:
        return None
    parts = []
    if 'C' in counts:
        parts.append(f"C{counts['C']}" if counts['C'] != 1 else "C")
    if 'H' in counts:
        parts.append(f"H{counts['H']}" if counts['H'] != 1 else "H")
    for el in sorted(k for k in counts.keys() if k not in ('C', 'H')):
        v = counts[el]
        parts.append(f"{el}{v}" if v != 1 else el)
    return " ".join(parts)


def _normalize_base_formula(formula):
    """Normalize a molecular formula to Hill system."""
    counts = _formula_to_base_counts(formula)
    return _counts_to_hill_string(counts) if counts else None


##############################################################################
# QC & METRIC CALCULATION FUNCTIONS
# Calculate quantitative metrics from mass spectra data
##############################################################################

def AIModCalc(df):
    """Calculate aromaticity index modified (AIMod)."""
    C = _as_series(df, 'C')
    H = _as_series(df, 'H')
    O = _as_series(df, 'O')
    N = _as_series(df, 'N')
    S = _as_series(df, 'S')
    P = _as_series(df, 'P')

    valid = _valid_formula_mask(C)

    # For valid rows: fill missing N/S/P with 0; for invalid rows: force NaN
    N = N.where(valid, np.nan).fillna(0).where(valid, np.nan)
    S = S.where(valid, np.nan).fillna(0).where(valid, np.nan)
    P = P.where(valid, np.nan).fillna(0).where(valid, np.nan)

    # C, H, O must exist for a meaningful calculation
    C = C.where(valid, np.nan)
    H = H.where(valid, np.nan)
    O = O.where(valid, np.nan)

    numerator   = 1 + C - 0.5*O - S - 0.5*(N + P + H)
    denominator = C - 0.5*O - N - S - P

    with np.errstate(divide='ignore', invalid='ignore'):
        ai = numerator / denominator

    # invalid or denom==0 -> NaN; clamp negatives to 0
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

    # Zero-fill only N/S/P when valid; otherwise NaN
    N = N.where(valid, np.nan).fillna(0).where(valid, np.nan)
    S = S.where(valid, np.nan).fillna(0).where(valid, np.nan)
    P = P.where(valid, np.nan).fillna(0).where(valid, np.nan)

    # Keep C,H,O as-is; force NaN on invalid rows
    C = C.where(valid, np.nan)
    H = H.where(valid, np.nan)
    O = O.where(valid, np.nan)

    with np.errstate(divide='ignore', invalid='ignore'):
        nosc = 4 - ((4*C + H - 2*O - 3*N + 5*P - 2*S) / C)

    nosc = nosc.replace([np.inf, -np.inf], np.nan)
    nosc = nosc.mask(~valid)
    return nosc


def calc_QC_metrics(df):
    """Calculate quality control metrics for a mass spectrum."""
    key_elements = ['C', 'H', 'O', 'N', 'S', 'P']
    for ele in key_elements:
        if ele not in df.columns:
            df[ele] = 0
            
    df = df.fillna(0)
    df['AIMod'] = AIModCalc(df)
    df['NOSC'] = NOSCcalc(df)

    df_assign = df[df['Heteroatom Class'] != 'unassigned'].copy()
    df_CHO = df[(df['C'] > 0) & (df['H'] > 0) & (df['O'] > 0) & 
                (df['N'] == 0) & (df['S'] == 0) & (df['P'] == 0)]

    # Interlab study metrics (10.1002/lom3.10364)
    df_assign['Norm_PeakHeight'] = (df_assign['Peak Height'] / df_assign['Peak Height'].max()) * 10_000
    df_assign['Weighted_OC'] = df_assign['O/C'] * df_assign['Norm_PeakHeight']
    df_assign['Weighted_HC'] = df_assign['H/C'] * df_assign['Norm_PeakHeight']
    df_assign['Weighted_mz'] = df_assign['m/z'] * df_assign['Norm_PeakHeight']
    df_assign['Weighted_AIMod'] = df_assign['AIMod'] * df_assign['Norm_PeakHeight']
    df_assign['Weighted_NOSC'] = df_assign['NOSC'] * df_assign['Norm_PeakHeight']

    qc_metrics = {}
    qc_metrics['qc_assign_rms'] = (df_assign['m/z Error (ppm)'] ** 2).mean() ** 0.5
    qc_metrics['qc_total_assign'] = len(df_assign)
    qc_metrics['qc_total_CHO'] = len(df_CHO)
    qc_metrics['qc_pc_assign'] = (len(df_assign) / len(df)) * 100
    qc_metrics['qc_pc_CHO'] = (len(df_CHO) / len(df)) * 100
    qc_metrics['qc_mass_median'] = df_assign['m/z'].median()
    qc_metrics['qc_mass_skew'] = df_assign['m/z'].skew()
    qc_metrics['qc_mass_kurt'] = df_assign['m/z'].kurtosis()
    qc_metrics['qc_SN_median'] = df_assign['S/N'].median()
    qc_metrics['qc_wa_OC'] = df_assign['Weighted_OC'].sum() / df_assign['Norm_PeakHeight'].sum()
    qc_metrics['qc_wa_HC'] = df_assign['Weighted_HC'].sum() / df_assign['Norm_PeakHeight'].sum()
    qc_metrics['qc_wa_mz'] = df_assign['Weighted_mz'].sum() / df_assign['Norm_PeakHeight'].sum()
    qc_metrics['qc_wa_AIMod'] = df_assign['Weighted_AIMod'].sum() / df_assign['Norm_PeakHeight'].sum()
    qc_metrics['qc_wa_NOSC'] = df_assign['Weighted_NOSC'].sum() / df_assign['Norm_PeakHeight'].sum()

    return qc_metrics


def calc_isotope_delta_error_stats(df):
    """Calculate isotopologue mass error statistics."""
    iso = df.loc[df['Is Isotopologue'].fillna(0).astype(int) == 1].copy()
    iso = iso[iso['Mono Isotopic Index'].notna()].copy()
    iso['Mono Isotopic Index'] = iso['Mono Isotopic Index'].astype(int)

    if iso.empty:
        return {
            'isotope_delta_error_mean': np.nan,
            'isotope_delta_error_std': np.nan,
            'n_iso_used': 0,
            'n_missing_parents': 0
        }

    # Monoisotopic rows (the parents) keyed by peak Index
    mono = df.loc[df['Is Isotopologue'].fillna(0).astype(int) == 0,
                  ['Index', 'Calibrated m/z', 'Calculated m/z']].copy()
    mono = mono.rename(columns={
        'Calibrated m/z': 'Calibrated m/z_mono',
        'Calculated m/z': 'Calculated m/z_mono'
    })

    # Join isotopologues to their parents via peak id
    merged = iso.merge(mono, left_on='Mono Isotopic Index', right_on='Index', how='left')

    # Count and drop isotopologues whose parent isn't in this dataframe
    missing_parents = merged['Calibrated m/z_mono'].isna().sum()
    merged = merged.dropna(subset=['Calibrated m/z_mono', 'Calculated m/z_mono'])

    if merged.empty:
        return {
            'isotope_delta_error_mean': np.nan,
            'isotope_delta_error_std': np.nan,
            'n_iso_used': 0,
            'n_missing_parents': int(missing_parents)
        }

    merged['obs_delta']  = merged['Calibrated m/z'] - merged['Calibrated m/z_mono']
    merged['theo_delta'] = merged['Calculated m/z'] - merged['Calculated m/z_mono']
    merged['delta_error'] = merged['obs_delta'] - merged['theo_delta']

    return {
        'isotope_delta_error_mean': merged['delta_error'].mean(),
        'isotope_delta_error_std': merged['delta_error'].std(),
        'n_iso_used': int(len(merged)),
        'n_missing_parents': int(missing_parents)
    }


def analyze_mass_spectrum_composition(df):
    """
    Analyze mass spectrum composition based on mass defects and isotopologue patterns.
    
    Calculates:
    - Organic vs inorganic compound counts based on mass defect
    - C13 and Cl37 isotopologue detection
    
    Returns:
        Dictionary with keys: organic_count, inorganic_count, organic_ratio,
                              c13_count, cl37_count, c13_ratio
    """
    if df.empty:
        return {
            'organic_count': 0,
            'inorganic_count': 0,
            'organic_ratio': np.nan,
            'c13_count': 0,
            'cl37_count': 0,
            'c13_ratio': np.nan
        }
    
    # Analyze mass defects
    mass_defects = df['Calibrated m/z'] % 1
    organic_count = np.sum((mass_defects >= 0.0) & (mass_defects <= 0.4))
    inorganic_count = np.sum((mass_defects >= 0.6) & (mass_defects < 1.0))
    organic_ratio = organic_count / inorganic_count if inorganic_count > 0 else np.nan

    # Detect isotopologues using efficient sorted sliding window approach
    calibrated_mz = np.sort(df['Calibrated m/z'].values)
    
    c13_count = 0
    cl37_count = 0
    max_delta = 2.5  # Safety margin for isotopologue detection
    
    for i in range(len(calibrated_mz)):
        j = i + 1
        while j < len(calibrated_mz) and (calibrated_mz[j] - calibrated_mz[i]) <= max_delta:
            delta = calibrated_mz[j] - calibrated_mz[i]
            
            # Check for C13 isotopologue (1.003355 ± 0.0005)
            if abs(delta - 1.003355) <= 0.0005:
                c13_count += 1
            
            # Check for Cl37 isotopologue (1.99705 ± 0.0005)
            if abs(delta - 1.99705) <= 0.0005:
                cl37_count += 1
            
            j += 1
    
    c13_ratio = c13_count / cl37_count if cl37_count > 0 else np.nan

    return {
        'organic_count': int(organic_count),
        'inorganic_count': int(inorganic_count),
        'organic_ratio': organic_ratio,
        'c13_count': int(c13_count),
        'cl37_count': int(cl37_count),
        'c13_ratio': c13_ratio
    }


##############################################################################
# DATA CLEANING & DEDUPLICATION FUNCTIONS
# Remove or consolidate conflicting assignments
##############################################################################

def dedup_by_mz_keep_best(ms_df, mz_col='m/z', score_col='Confidence Score'):
    """Keep best scoring assignment per m/z value."""
    tmp = ms_df.assign(_score=ms_df[score_col].astype(float).fillna(-np.inf))
    idx = tmp.groupby(mz_col, sort=False)['_score'].idxmax()
    out = ms_df.loc[idx].copy()
    out.sort_index(inplace=True)
    return out


def dedup_keep_best_and_prune_orphans(
    ms_df,
    peak_idx_col='Index',
    score_col='Confidence Score',
    is_iso_col='Is Isotopologue',
    mono_idx_col='Mono Isotopic Index',
    enforce_formula_consistency=False,
    formula_col='Molecular Formula'
):
    """
    Deduplicate assignments per peak and remove orphaned isotopologues.
    Optionally enforce base formula consistency between isotopologues and parents.
    """
    # 1) Dedup per peak (Index), not per m/z
    tmp = ms_df.copy()
    tmp['_score'] = pd.to_numeric(tmp[score_col], errors='coerce').fillna(-np.inf)
    best_row_idx = tmp.groupby(peak_idx_col, sort=False)['_score'].idxmax()
    out = ms_df.loc[best_row_idx].copy()
    out.sort_index(inplace=True)

    # 2) Build set of surviving monoisotopic peak IDs
    mono_survivors = set(out.loc[out[is_iso_col].fillna(0).astype(int) == 0, peak_idx_col])

    # 3) Drop isotopologues whose parent peak didn't survive
    is_iso = out[is_iso_col].fillna(0).astype(int) == 1
    parent_idx = pd.to_numeric(out[mono_idx_col], errors='coerce').astype('Int64')
    keep_iso_has_parent = parent_idx.isin(mono_survivors)
    mask_keep = (~is_iso) | (is_iso & keep_iso_has_parent)
    out = out.loc[mask_keep].copy()

    if enforce_formula_consistency:
        # 4) Optional: ensure isotopologue base formula matches kept mono's base formula
        mono_base = out.loc[~is_iso, [peak_idx_col, formula_col]].copy()
        mono_base['__base'] = mono_base[formula_col].map(_normalize_base_formula)
        base_map = dict(zip(mono_base[peak_idx_col], mono_base['__base']))

        iso_rows = out.loc[is_iso].copy()
        iso_rows['__iso_base'] = iso_rows[formula_col].map(_normalize_base_formula)
        iso_rows['__parent_base'] = iso_rows[mono_idx_col].map(base_map)
        consistent = iso_rows['__iso_base'].notna() & (iso_rows['__iso_base'] == iso_rows['__parent_base'])

        out = pd.concat([
            out.loc[~is_iso],
            iso_rows.loc[consistent]
        ]).sort_index()

    # Clean up temp cols
    for c in ('_score', '__iso_base', '__parent_base'):
        if c in out.columns:
            out.drop(columns=c, inplace=True, errors='ignore')

    return out


def remove_assignments(ms_df):
    """Remove unwanted formula assignments based on ion type and heteroatom content."""
    cols_to_clear = [
        'Calculated m/z', 'm/z Error (ppm)', 'm/z Error Score',
        'Isotopologue Similarity', 'Confidence Score', 'DBE', 'O/C', 'H/C',
        'Heteroatom Class', 'Ion Type', 'Adduct', 'Is Isotopologue',
        'Mono Isotopic Index', 'Molecular Formula', 'C', 'H', 'O', 'N', 'S',
        '13C', '15N', '18O', '33S', '34S', 'Formula Class'
    ]
    
    # Bad rows: Ion Type == 'adduct' AND (S > 0 OR N > 0)
    ion_is_adduct = (ms_df['Ion Type'].astype(str).str.lower() == 'adduct')
    bad_formula_mask = ion_is_adduct & ((ms_df['S'] > 0) | (ms_df['N'] > 0))

    # Count occurrences of each Index
    index_counts = ms_df['Index'].map(ms_df['Index'].value_counts())

    # Among bad rows, split by uniqueness
    drop_mask = bad_formula_mask & (index_counts > 1)      # Drop these
    nullify_mask = bad_formula_mask & (index_counts == 1)  # Nullify these

    # Drop only the matching bad rows (keep non-bad rows with same Index)
    ms_df = ms_df.loc[~drop_mask].copy()

    # Nullify assignment columns for unique bad rows
    ms_df.loc[nullify_mask, cols_to_clear] = np.nan
    
    return ms_df


##############################################################################
# VISUALIZATION FUNCTIONS
# Create publication-quality figures
##############################################################################

def create_qc_figure(msobj, msdf, title='QC Plot', figsize=(24, 10), nrows=2, ncols=4, hspace=0.22, wspace=0.22):
    """
    Create a comprehensive QC figure with mass spectrum, errors, Van Krevelen, and metrics.
    Thread-safe: returns a new figure and axes on each call.
    """
    msdf = msdf.copy()
    key_elements = ['C', 'H', 'O', 'N', 'S', 'P']
    for ele in key_elements:
        if ele not in msdf.columns:
            msdf[ele] = 0
    msdf[key_elements] = msdf[key_elements].fillna(0)
    
    mz = msobj.mz_cal_profile
    abu = msobj.abundance_profile
    
    def subset_mz(mz_array, abu_array, mz_min, mz_max):
        idx = (mz_array >= mz_min) & (mz_array <= mz_max)
        return mz_array[idx], abu_array[idx]
    
    fig = plt.figure(figsize=figsize)
    gs = gridspec.GridSpec(nrows, ncols, figure=fig, hspace=hspace, wspace=wspace)
    axes = [fig.add_subplot(gs[row, col]) for row in range(nrows) for col in range(ncols)]
    
    # Mass Spectrum
    axes[0].plot(mz, abu, lw=1, c='k')
    
    # Zoom 1 (200-800 m/z)
    mz_zoom1, abu_zoom1 = subset_mz(mz, abu, 200, 800)
    axes[1].plot(mz_zoom1, abu_zoom1, lw=1, c='k')
    axes[1].set_ylim(0, 0.1*max(abu))
    axes[1].set_xlim(200, 800)
    
    # Zoom 2 (282.95-283.2 m/z)
    mz_zoom2, abu_zoom2 = subset_mz(mz, abu, 282.95, 283.2)
    axes[4].plot(mz_zoom2, abu_zoom2, lw=1, c='k')
    
    # Zoom 3 (571.0-571.25 m/z)
    mz_zoom3, abu_zoom3 = subset_mz(mz, abu, 571.0, 571.25)
    axes[5].plot(mz_zoom3, abu_zoom3, lw=1, c='k')

    intensity_label = 'Intensity (a.u.)'
    mass_label = '$m/z$'
    for ax in [axes[0], axes[1], axes[4], axes[5]]:
        ax.set_ylabel(intensity_label)
        ax.set_xlabel(mass_label)

    # Error Plot
    axes[2].scatter(msdf['m/z'], msdf['m/z Error (ppm)'], s=msdf['S/N']*0.01, c='k', alpha=0.5)
    axes[2].set_ylabel('$m/z$ Error (ppm)')
    axes[2].set_xlabel(mass_label)

    # Van Krevelen
    axes[3].scatter(msdf['O/C'], msdf['H/C'], s=msdf['S/N']*0.01, c='k', alpha=0.5)
    axes[3].set_ylabel('H/C')
    axes[3].set_xlabel('O/C')
    axes[3].set_xlim(0, 1.25)
    axes[3].set_ylim(0.25, 2.25)
    
    # Heteroatom Countplot
    df_filtered = msdf[
        (msdf['Heteroatom Class'] != 'unassigned') &
        (msdf['Is Isotopologue'] == 0)
    ].copy()
    
    def group_hetero(row):
        if row['S'] > 0:
            return 'S > 0'
        elif row['N'] > 0:
            return 'N > 0'
        else:
            return 'No S/N'
       
    df_filtered['Hetero Group'] = df_filtered.apply(group_hetero, axis=1)
    df_filtered['O'] = pd.to_numeric(df_filtered['O'], errors='coerce').fillna(0).astype(int)
    
    group_order = ['No S/N', 'S > 0', 'N > 0']

    sns.countplot(
        data=df_filtered,
        x='O',
        hue='Hetero Group',
        order=sorted(df_filtered['O'].unique()),
        hue_order=group_order,
        ax=axes[6]
    )
    
    axes[6].tick_params(axis='x', labelsize=10, rotation=45)
    axes[6].legend(
        title='Hetero Group',
        fontsize=8,
        title_fontsize=9,
        loc='best',
        frameon=True,
        borderpad=0.3,
        handletextpad=0.3,
        borderaxespad=0.3,
        labelspacing=0.3,
        handlelength=1
    )

    # NOSC KDE
    msdf['NOSC'] = NOSCcalc(msdf)
    sns.kdeplot(data=msdf, x='NOSC', ax=axes[7], c='k')
    axes[7].set_xlim(-2.5, 2.5)
    
    fig.suptitle(title, fontsize=24, y=0.95)
    
    return fig, axes


##############################################################################
# FILENAME PARSING FUNCTIONS
# Extract metadata from raw data filenames
##############################################################################

def parse_filename(name: str) -> Dict[str, Any]:
    """
    Simple filename parsing: extract hystarid, is_qc, and is_srfa flags.
    
    Returns:
        Dict with keys:
        - hystarid: Last 5-digit segment before .d extension (or None)
        - is_qc: True if "QC" appears in filename
        - is_srfa: True if both "QC" and "SRFA" appear in filename
    """
    name = name.strip()
    
    # Extract hystarid: last underscore-separated part as 5-digit number
    hystarid = None
    if name.endswith('.d'):
        name_no_ext = name[:-2]
        parts = name_no_ext.split('_')
        if parts and parts[-1].isdigit() and len(parts[-1]) == 5:
            try:
                hystarid = int(parts[-1])
            except ValueError:
                pass
    
    # Check for QC and SRFA flags (case-insensitive)
    name_upper = name.upper()
    is_qc = 'QC' in name_upper
    is_srfa = is_qc and 'SRFA' in name_upper
    
    return {
        'hystarid': hystarid,
        'is_qc': is_qc,
        'is_srfa': is_srfa,
    }


def parse_many(names):
    """Convenience function for parsing multiple filenames (for pandas .apply)."""
    return [parse_filename(n) for n in names]


##############################################################################
# CORE PROCESSING FUNCTIONS
# Main data processing and readback extraction
##############################################################################

def get_readbacks(bruker_reader, result):
    """Extract capillary and shield voltage readback statistics."""
    def describe_to_dict(series, suffix):
        return (
            series
            .describe()
            .rename(lambda x: f"{x}_{suffix}")
            .to_dict()
        )
    sqlite_data = bruker_reader.parse_sqlite()
    #cap_readback = pd.DataFrame(sqlite_data['Capillary'])
    shield_readback = pd.DataFrame(sqlite_data['Shield'])
    
    result.update(describe_to_dict(shield_readback['values'], 'shield'))
    #result.update(describe_to_dict(cap_readback['values'], 'capillary'))

    return result
    

def process_datafile(datafile_path: Path, parsed_name: Optional[Dict[str, Any]], refmasslist, outputdir: Path, figoutput: Path):
    """
    Main processing pipeline for a single raw data file.
    
    Steps:
    1. Parse filename metadata
    2. Read Bruker raw data
    3. Extract calibration and instrument parameters
    4. Peak picking and resolving power filtering
    5. Mass calibration
    6. Molecular formula assignment
    7. Calculate QC metrics
    8. Generate output files (CSV, PNG)
    
    Returns:
        Dictionary with results, or None on failure
    """
    result = {}
    try:
        file_location = str(datafile_path)
        
        # Add basic filename info
        result['datafile'] = datafile_path.name
        outname = datafile_path.stem
        
        # Add parsed filename metadata
        parsed = parsed_name if parsed_name is not None else parse_filename(datafile_path.name)
        result.update(parsed)
        
        # Read in Bruker Raw Data
        bruker_reader = ReadBrukerSolarix(file_location)

        # Get readbacks and add to result dict
        result = get_readbacks(bruker_reader, result)

        # Extract the ion accumulation time from the parameters
        acq_pars = bruker_reader.parse_parameters(bruker_reader.locate_file(Path(file_location)))
        result['IAT'] = float(acq_pars['D_10'])
        
        # Set the peak picking/noise threshold
        set_other_params()
    
        # Read the mass spectrum
        mass_spectrum = bruker_reader.get_transient().get_mass_spectrum(plot_result=False, auto_process=True)
        
        # Remove features with resolving powers outside of 3 stdevs of the mean
        ids_to_remove = MeanResolvingPowerFilter(mass_spectrum, plot=False, ndeviations=3).main()
        mass_spectrum.filter_by_index(ids_to_remove)
    
        # Calibrate the mass spectrum
        MzDomainCalibration(mass_spectrum, refmasslist).run()
        
        baseline_noise = mass_spectrum.baseline_noise
        baseline_noise_std = mass_spectrum.baseline_noise_std
    
        # Set MF search settings
        set_mf_search_settings(mass_spectrum)
        
        # Search MFs
        SearchMolecularFormulas(mass_spectrum, first_hit=False).run_worker_mass_spectrum()
      
        # Convert to DataFrame
        ms_df = mass_spectrum.to_dataframe()
        
        ms_df.to_csv(str(outputdir / f"{outname}.csv"))
        
        result.update(calc_isotope_delta_error_stats(ms_df))
        result.update(analyze_mass_spectrum_composition(ms_df))

        # Generate QC figure
        figtitle = outname
        qc_fig, qc_axes = create_qc_figure(mass_spectrum, ms_df, title=figtitle, hspace=0.25, wspace=0.35)
        qc_fig.savefig(str(figoutput / f"{figtitle}.png"), dpi=100, bbox_inches='tight')
        plt.close(qc_fig)
        plt.close('all')
        
        # Calculate and store QC metrics
        qc_metrics = calc_QC_metrics(ms_df)
        result |= qc_metrics

        # Store additional metrics from mass spectrum
        result['tic'] = mass_spectrum.tic
        result['baseline_noise'] = baseline_noise
        result['baseline_noise_std'] = baseline_noise_std
        result['peaks'] = len(mass_spectrum)
        result['cal_points'] = mass_spectrum.calibration_points
        result['cal_rms'] = mass_spectrum.calibration_RMS
        result['raw_error_median'] = mass_spectrum.calibration_raw_error_median
        result['raw_error_stdev'] = mass_spectrum.calibration_raw_error_stdev
        result['original_masscalcoef_A'] = mass_spectrum.Aterm
        result['original_masscalcoef_B'] = mass_spectrum.Bterm
        result['original_masscalcoef_C'] = mass_spectrum.Cterm
        result['resolving_power_peaks_removed'] = len(ids_to_remove)
        
    except Exception as e:
        logging.error(f"Failed to process {datafile_path.name}: {e}", exc_info=True)
        result = None
        
    return result


##############################################################################
# LOGGING CONFIGURATION FUNCTIONS
# Set up logging for multiprocessing environment
##############################################################################

def _worker_configure_logger(log_queue):
    """Configure worker process to push logs into a multiprocessing queue."""
    qh = logging.handlers.QueueHandler(log_queue)
    root = logging.getLogger()
    root.handlers.clear()
    root.setLevel(logging.ERROR)
    root.addHandler(qh)


def _configure_main_logger(log_queue, logfile: Path):
    """Configure main process logging to use the same queue + file listener."""
    file_handler = logging.FileHandler(logfile)
    file_handler.setLevel(logging.ERROR)
    formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
    file_handler.setFormatter(formatter)

    listener = logging.handlers.QueueListener(log_queue, file_handler, respect_handler_level=True)
    listener.start()

    root = logging.getLogger()
    root.handlers.clear()
    root.setLevel(logging.ERROR)
    root.addHandler(logging.handlers.QueueHandler(log_queue))

    return listener


##############################################################################
# MAIN EXECUTION
# Script entry point - change settings and run
##############################################################################

if __name__ == '__main__':

    # Ensure CoreMS is available before proceeding (helpful if env not activated)
    try:
        import corems  # noqa: F401
    except ImportError as e:
        print("CoreMS not available. Activate the CoreMS environment (e.g., C:\\Tools\\activate_corems.ps1) and retry.")
        raise SystemExit(1) from e

    if not REFMASSLIST.exists():
        print(f"Reference mass list not found: {REFMASSLIST}")
        raise SystemExit(1)

    # Ensure output directories exist
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    FIG_OUTPUT.mkdir(parents=True, exist_ok=True)

    datafiles = [p for p in DATA_DIR.iterdir() if p.is_dir() and p.suffix.lower() == '.d']

    # Configure logging early so validation errors are captured (multiprocess-safe)
    log_queue = multiprocessing.Queue()
    log_listener = _configure_main_logger(log_queue, OUTPUT_DIR / 'processing.log')

    # Parse all filenames and build lookup; skip any that fail to parse
    parsed_lookup = {}
    skipped_files = []
    for datafile in datafiles:
        try:
            parsed = parse_filename(datafile.name)
            parsed_lookup[datafile.name] = parsed
        except Exception as exc:
            logging.error(f"Failed to parse {datafile.name}: {exc}", exc_info=True)
            skipped_files.append({"datafile": datafile.name, "error": str(exc)})
    
    # Remove skipped files from processing list
    datafiles = [p for p in datafiles if p.name in parsed_lookup]
    
    if skipped_files:
        err_df = pd.DataFrame(skipped_files)
        err_path = OUTPUT_DIR / 'parse_errors.csv'
        err_df.to_csv(err_path, index=False)
        print(f"Warning: {len(skipped_files)} file(s) failed to parse. See {err_path} for details.")

    if TEST_MODE:
        print(f"✓ TEST MODE: Successfully parsed {len(datafiles)} filenames.")
        log_listener.stop()
        raise SystemExit(0)

    df_stats = pd.DataFrame(index=datafiles, columns=['peaks', 'assignments', 'rms_cal', 'rms_assignments', 'pc_assign', 'tic',
                                                      'baseline_noise', 'baseline_noise_std'])

    all_results = []
    failed_results = []

    try:
        if multithread:
            with concurrent.futures.ProcessPoolExecutor(max_workers=max_workers, initializer=_worker_configure_logger, initargs=(log_queue,)) as executor:
                future_to_datafile = {executor.submit(process_datafile, datafile, 
                                                      parsed_lookup.get(datafile.name), 
                                                      str(REFMASSLIST), OUTPUT_DIR, FIG_OUTPUT): datafile for datafile in datafiles}
                for future in tqdm(concurrent.futures.as_completed(future_to_datafile), total=len(datafiles)):
                    datafile = future_to_datafile[future]
                    try:
                        result = future.result()
                        if result:
                            all_results.append(result)
                        else:
                            failed_results.append({"datafile": datafile.name, "error": "returned None"})
                    except Exception as exc:
                        logging.error(f'{datafile} generated an exception: {exc}', exc_info=True)
                        failed_results.append({"datafile": datafile.name, "error": str(exc)})
        else:
            for datafile in datafiles:
                result = process_datafile(datafile, parsed_lookup.get(datafile.name), str(REFMASSLIST), OUTPUT_DIR, FIG_OUTPUT)
                if result:
                    all_results.append(result)
                else:
                    failed_results.append({"datafile": datafile.name, "error": "returned None"})

        if all_results:
            df_stats = pd.DataFrame(all_results)
            df_stats.to_csv(str(OUTPUT_DIR / 'combined_stats.csv'), index=False)

        if failed_results:
            df_failed = pd.DataFrame(failed_results)
            df_failed.to_csv(str(OUTPUT_DIR / 'failed_samples.csv'), index=False)

    finally:
        # Stop the logging listener even on early exits
        log_listener.stop()

# end
