# BunAG

## Overview

ED UMC's anion gap research in 2026

## Study Objectives

### Objective 1: Survival Analysis

Evaluate the association between Anion Gap and mortality outcomes using:

* Kaplan–Meier survival analysis
* Cox proportional hazards regression
* Firth penalized regression (if needed)
* Bootstrap validation

### Objective 2: SOFA Performance Analysis

Assess and compare the prognostic performance of:

* SOFA score
* Albumin-corrected Anion Gap
* Other candidate predictors

Methods may include:

* ROC analysis
* Calibration assessment
* Model comparison
* Predictive performance evaluation

---

## Project Structure

```text
BunAG/
├── data/
│   ├── raw/
│   └── clean/
├── scripts/
│   ├── common/
│   ├── survival/
│   └── sofa/
├── figures/
├── output/
├── manuscript/
└── README.md
```

## Data Management

* Raw patient-level data are not stored in this repository.
* Only de-identified outputs, scripts, and study materials should be uploaded.

## Statistical Software

* R
* R packages: survival, rms, survminer, timeROC, pROC, logistf, boot

## Status

Project initiated: June 2026

Current phase:

* Data cleaning
* Variable derivation
* Survival analysis development
