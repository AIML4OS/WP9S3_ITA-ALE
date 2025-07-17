# WP9S3_ITA-ALE
Random forest implementation to predict categorical variable Attained Level of Education (ALE) beyond the sample.
- 01_input_sperim3.R : EDA, Feature Engeneering. 
Variables selection, NA treatment, variables' coding, train/test splitting.
- 02_tuning.R: tuning mtry (number of variables for the splitting) and min.node.tree (leaves' dimension) using tuneRanger
- 02_implementation_sperim3.R: implementation of Random Forest with ranger package. 
- 03_Predictions_sperim3.R: evaluate predictions on test set (beyond the sample units). 
macro-level comparison. i.e. comparison between obtained distribution and benchmark distribution.
