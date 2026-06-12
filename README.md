# Healthcare Readmissions Analytics - CMS HRRP

Analysis of the CMS Hospital Readmissions Reduction Program (HRRP) data examining
what structural hospital characteristics drive excess readmission ratios and payment
penalties across 3,085 US hospitals and 6 conditions.

---

## The Problem

CMS penalizes hospitals up to 3% of total Medicare payments for excess readmissions.
In the most recent data release, 78.6% of hospitals (2,342 of 2,980) received a
payment reduction. The central question: which structural hospital factors actually
predict readmission performance, and how much of the variation can facility-level
data explain?

---

## Datasets

Three CMS public datasets, merged at the hospital level:

| Dataset | Rows | What it contains |
|---|---|---|
| HRRP | 18,510 | Condition-level excess readmission ratios (6 conditions × 3,085 hospitals) |
| Hospital General Information | 5,381 | Star ratings, ownership type, quality measure counts |
| HRRP Supplemental File | 3,048 | Payment penalties, dual-eligible proportions, peer group assignments |

Source: [CMS.gov](https://data.cms.gov) - all datasets are public domain.

---

## Tools

- **Language:** R
- **Libraries:** tidyverse, ggplot2, broom, car, here
- **Methods:** OLS regression, logistic regression, ANOVA, state fixed effects

---

## Key Findings

**Readmission quality profile outperforms star ratings as a predictor.**
Adding a hospital's net readmission performance score, the count of readmission
measures rated better minus worse than the national average to the baseline
predictors pushes adjusted R² from 8.3% to 14.0%. This single addition accounts
for the largest improvement across all 7 models tested, outperforming state fixed
effects, log transformation, and interaction terms.

**Structural factors explain 15.3% of variance in the best model.**
Adding state fixed effects alongside the readmission quality composite (Model 5)
achieves an adjusted R² of 15.3%. The remaining ~85% reflects patient-level factors -
comorbidities, socioeconomic status, discharge support that facility-level CMS data
does not capture. This is expected and consistent with the nature of aggregate hospital
reporting.

**78.6% of hospitals were penalized.**
Of 2,980 hospitals with valid penalty data, 2,342 received payment reductions.
Mean penalty: 0.322%. Maximum: 3.0%.

**Condition-specific models significantly outperform the pooled model.**
Heart Failure (adj R² = 30.3%) and Pneumonia (29.2%) models explain approximately twice
the variance of the pooled model (14.0%) using identical predictors. This points to
condition-specific quality drivers that get masked when all six conditions are pooled
into a single model.

**Dual-eligible proportion does not independently predict penalty.**
After controlling for peer group and ownership, dual proportion is not a statistically
significant predictor of penalty status (p = 0.886). Peer group assignment already
stratifies hospitals by dual-eligible burden, absorbing that effect. Ownership type
and peer group are the dominant penalty predictors.

---

## Model Comparison

All 7 models run on the same sample (n = 11,120 hospital-condition observations).
AIC values are directly comparable across all models.

| Model | Adj R² | Notes |
|---|---|---|
| Model 1: Full Baseline | 8.26% | Star rating + ER + dual_prop + peer group + ownership + condition |
| Model 2: Refined Baseline | 8.27% | Star rating + peer group + ownership + condition |
| Model 3: + Readm Net | 14.0% | Adds hospital-level readmission quality composite |
| Model 4: + State FE | 11.0% | Adds state fixed effects |
| **Model 5: Readm Net + State FE** | **15.3%** | **Best model - lowest AIC (-26,357)** |
| Model 6: Log Outcome | 8.22% | Log-transformed excess ratio - no improvement |
| Model 7: Interaction | 8.29% | rating_num × peer_group - interaction not significant |

---

## Condition-Specific Models

Model: excess_ratio ~ star rating + peer group + ownership + readm_net (per condition)

| Condition | Adj R² | n |
|---|---|---|
| Heart Failure | 30.3% | 2,424 |
| Pneumonia | 29.2% | 2,423 |
| AMI | 21.0% | 1,722 |
| CABG | 10.4% | 869 |
| COPD | 10.4% | 2,229 |
| Hip/Knee | 6.95% | 1,453 |

Heart Failure and Pneumonia show the strongest structural signal. CABG and Hip/Knee
are largely driven by surgical volume and patient selection factors not captured here.

---

## Payment Penalty Analysis

Logistic regression on penalty_binary (n = 2,944 hospitals):
penalty_binary ~ peer_group + ownership + dual_prop

**Significant predictors (p < 0.001):**
- Peer groups 2–4 are significantly more likely to be penalized than peer group 1
- Proprietary, Voluntary non-profit, and Government-owned hospitals all face
  significantly higher penalty odds than Federal hospitals

**Not significant:**
- Dual proportion (p = 0.886) -  peer group assignment already accounts for this

---

## How to Run

1. Open `Healthcare_Analytics.Rproj` in RStudio - sets the working directory via `here()`
2. Install packages if needed:
   ```r
   install.packages(c("tidyverse", "broom", "car", "here"))
   ```
3. Run `Analytics_Project_v2.R`

---

## Project Structure

```
Healthcare_Readmissions_Analytics/
│
├── Analytics_Project_v2.R
├── Healthcare_Analytics.Rproj
├── FY_2025_Hospital_Readmissions_Reduction_Program_Hospital 1.csv
├── Hospital_General_Information.csv
├── Copy_of_FY2025_Final_Rule_Supplemental_File 1.csv
└── README.md
```

---

## Full Portfolio

Detailed methodology, visualizations, and findings:
**[Notion Portfolio](your-notion-link-here)**
