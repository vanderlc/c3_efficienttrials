# C3 Subtest Modeling for Trial Enrichment in Preclinical Alzheimer's Disease

## Overview

This repository contains analytic code evaluating whether individual subtests within the Computerized Cognitive Composite (C3) can improve efficiency in secondary prevention trials for Alzheimer's disease.

The primary objective is to determine whether specific C3 components provide stronger prognostic signal than the composite score alone, enabling:

- Earlier detection of subtle cognitive decline
- Improved identification of high-risk individuals
- Reduced sample sizes
- Shorter and more efficient prevention studies

This work focuses on cognitively unimpaired older adults and models longitudinal decline and risk of progression using subtest-level performance.

---

## Scientific Rationale

Preclinical Alzheimer's disease trials require large sample sizes and long follow-up periods due to slow rates of cognitive decline.

The C3 composite has demonstrated sensitivity to early change. However, the relative contribution of individual subtests remains unclear. If specific subtests capture early decline more efficiently, they may:

- Improve trial enrichment strategies
- Increase statistical power
- Reduce required follow-up duration
- Enable more scalable and lower-burden cognitive endpoints

This repository evaluates those possibilities using subtest-level modeling and validation frameworks.

---

## Repository Structure

code/ - R scripts for preprocessing, modeling, validation, and plotting
data/ - datasets
graphs/ - Generated figures, ROC curves, longitudinal plots, and summary visualizations