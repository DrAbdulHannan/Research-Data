# Prediction of factors contributing to Pain Intensity among low back pain patients: A comparative Machine Learning Frameworks (Random Forest versus XGBoost)

[![R Version](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://img.shields.io/badge/DOI-10.xxxx/xxxxxx-red.svg)](https://doi.org/10.xxxx/xxxxxx)

## 📌 What the project does

This project compares two powerful machine learning algorithms—**Random Forest** and **XGBoost**—to predict pain intensity levels in patients with low back pain (LBP). It identifies key clinical, demographic, and lifestyle factors contributing to pain severity using real-world patient data.

### Key Features

- Data preprocessing and exploratory data analysis (EDA)
- Feature selection and engineering
- Training and tuning Random Forest and XGBoost models
- Model evaluation (accuracy, RMSE, R², confusion matrix, ROC curves)
- SHAP analysis for model interpretability (feature importance)
- Comparative visualization of results

### Variables Analyzed

| Category | Variables |
|----------|-----------|
| Demographic | Age, Sex, Marital Status, Qualification, Nationality |
| Lifestyle | Smoking status, Technology use, Social media use, AI use |
| Clinical | Chronic illness, Previous surgery, Medication for anxiety |
| Occupational | Type of occupation, Nature of occupation, Years of experience, Working hours |
| Outcomes | Pain Intensity (ordinal: Mild, Moderate, Severe, Very Severe) |

---

## 🎯 Why the project is useful

### Clinical Relevance
Helps clinicians understand which factors (age, BMI, pain duration, smoking status, medication use, technology use) most influence pain intensity in LBP patients.

### Comparative Insights
Provides a head-to-head comparison of Random Forest and XGBoost, helping researchers choose the best model for ordinal classification tasks.

### Interpretability
Uses SHAP (SHapley Additive exPlanations) to explain individual predictions, making machine learning transparent and trustworthy for medical applications.

### Reproducible Research
All R code, sample data structures, and results are provided for full reproducibility.

### Educational Value
Serves as a template for applying ML in healthcare research, including data splitting, cross-validation, hyperparameter tuning, and model interpretation.

---

## 🚀 How users can get started with the project

### Prerequisites

- R version 4.0 or higher
- RStudio (recommended)

### Installation

1. **Clone the repository**

```bash
git clone https://github.com/yourusername/lbp-pain-intensity-ml.git
cd lbp-pain-intensity-ml