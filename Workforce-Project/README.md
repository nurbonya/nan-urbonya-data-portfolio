(project_banner.jpeg)

# WIOA Repeat Participant Analysis Project

## Overview
This project analyzes differences between Single and Repeat participants in Missouri’s Workforce Innovation and Opportunity Act (WIOA) Title I training programs. The analysis examines how participant characteristics, training patterns, socioeconomic barriers, and instructional program pathways relate to employment and credential outcomes.  

The goal is to provide **data-driven insights** that strengthen workforce services and improve training and employment opportunities statewide.

📄 **Full Research Report:**  
`data/raw/Research Report.docx`

---

## Key Research Questions
- How do participant characteristics differ between Single-enrollment and Repeat-enrollment cohorts?  
- What barriers (low income, single parent, ex-offender, etc.) appear more often among Repeat participants?  
- How do employment and credential outcomes compare across participant groups?  
- Which training programs (CIP codes) show the strongest concentration of barriers?  
- How have training enrollments shifted across PY2018–PY2024?

---

## Key Insights
- Repeat participants tend to be **younger** and face **higher concentrations of barriers**.  
- Employment and credential attainment outcomes are **lower among Repeat participants**.  
- CIP 48 (Precision Production) and CIP 49 (Transportation & Materials Moving) show **higher-than-average barrier concentrations**.  
- Enrollment trends display a decline after PY2019, stabilizing in recent years.  

These findings highlight where targeted supports, flexible training structures, and enhanced case management may improve participant outcomes.

---

## Data Sources
- **PIRL Records (PY2018–PY2024)**  
- **Local Area Unemployment Statistics (LAUS)**  
- WIOA Title I Adult, Dislocated Worker, and Youth Programs  
- Wagner-Peyser co-enrollment data  

---

## Methods
This project uses a combination of:  
- Descriptive statistics  
- Longitudinal trend analysis  
- **Generalized Linear Mixed Models (GLMMs)** for outcome prediction  
- Natural spline terms for nonlinear relationships (e.g., age, program length)  
- County-level random intercepts using LAUS data  

This modeling approach accounts for demographic, programmatic, and geographic variation.

---

## Skills Demonstrated
- Workforce development analytics  
- Statistical modeling (GLMM + splines)  
- R programming (tidyverse, lme4)  
- Data visualization  
- Report writing and interpretation  
- Portfolio-quality documentation  

---

## How to Use This Repository
1. Clone or download the repository:  
   `git clone https://github.com/nurbonya/nan-urbonya-data-portfolio/Workforce-Project/WIOA-Repeat-Participant-Analysis.git`
2. Review the full report in **data/raw**.  
3. Explore the **scripts** folder for modeling and visualization code.  
4. View exported figures and tables in **outputs/**.

---

## Contact
If you'd like support customizing this project, structuring additional analysis, or creating more professional visuals, feel free to reach out anytime.
