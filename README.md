# ap_Indices_Surv_HNSCC
Public repository of R code to reproduce analyses and figures in "Adherence to a priori-defined Diet Quality Indices Throughout the Early Disease Course is Associated with Survival in Head and Neck Cancer Survivors: An Application Involving Marginal Structural Models"

`diet_data_preproccess_score_calculations.R`: Code for conversion of raw dietary intake data to servings per day, computation of food groups, and, finally, computation of index scores utilized in the analyses.
`imputations.R`: Code for imputing those with missing data. Imputed and non-imputed datasets are retained.
`AC_mortality_analysis.R`: Main analysis with marginal structural models for all-cause mortality.
`CA_mortality_analysis.R`: Main analysis with marginal structural models for cancer-specific mortality.
`AC_interaction_effectMod_analysis.R`: Effect-modification analysis (all-cause mortality).
`CA_interaction_effectMod_analysis.R`: Effect-modification analysis (cancer-specific mortality).
`AC_sensitivity_analysis.R`: Sensitivity analyses (all-cause mortality).
`CA_sensitivity_analysis.R`: Sensitivity analyses (cancer-specific mortality).
`baseline_index__only_models.R`: Analyses using time-invariant (with baseline fixed diet index scores) Cox Proportional Hazards Models (all-cause and cancer-specific mortality).
`alluvial_plots.R`: Code for generating alluvial plots in manuscript.
`spline_plots.R`: Code for conducting restricted cubic splines analyses and generating the plots in the manuscript.
`table_1_epi_characteristics.R`: Code for generating Table 1 in the manuscript.
`quantfunction.R`: Source code for function used in various analysis files (above).
`surv_miner_bugfix_826`: Bug-fix for `survminer` package (this is required and used in `AC_mortality_analysis.R` and `CA_mortality_analysis.R` to generate adjusted survival curves.
