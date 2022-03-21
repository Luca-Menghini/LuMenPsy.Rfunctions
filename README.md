# R functions for data processing and data analysis
This repository includes a set of R functions for self-report, physiological, and behavioural data management and data analysis, and especially for dealing with multilevel data.

## R functions

### Descriptives

- `itemsICC` computes and prints the intraclass correlation coefficient for one or more time-varying variables.

- `multidesc` computes and prints descriptive statistics (mean and SD) and intraclass correlation of a multilevel dataset.

- `multicorr` computes the correlation matrix of a multilevel dataset.

### Psychometrics

- `fit.ind` computes and prints fit indices from mono- and multi-level confirmatory factor analysis models.

- `MCFArel` computes and prints composite reliability index (Chronbach's alpha and McDondald's omega) from a multilevel confirmatory factor analysis model.

- `GTHEORYrel` computes and prints variance components and reliability indices from a multilevel dataset, as described by Cranford, J. A., Shrout, P. E., Iida, M., Rafaeli, E., Yip, T., & Bolger, N. (2006). A Procedure for Evaluating Sensitivity to Within-Person Change: Can Mood Measures in Diary Studies Detect Change Reliably? Personality and Social Psychology Bulletin, 32(7), 917–929. https://doi.org/10.1177/0146167206287721

### Multilevel modeling

- `glmerAn` fits generalized linear (mixed-effects) regression models and prints core numerical and graphical ouputs.

- `key.resPlot` visualizes the key results (i.e., Akaike weight, likelihood ratio test, and *t* value) obtained with the `glmerAn` function.

### Influential analysis

- `sample.fluct` recoursively compute and save parameter estimates from multilevel CFA estimates by removing participants one-to-one.

- `plot.infl` visualizes the results of the `sample.fluct` function.

## R packages

The functions are based on the following R packages:

- `car`

- `ggplot2`

- `Hmisc`

- `knitr`

- `lavaan`

- `lme4`

- `ordinal`

- `MuMIn`

- `Rmisc`

- `sjPlot`

- `tcltk`

- `XML`
