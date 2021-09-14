# Student Project - Leonard Kunz

## Abstract

This project draws on the results obtained by [Paul Niehaus and Sandip Sukhtankar (2013)](https://www.aeaweb.org/articles?id=10.1257/pol.5.4.230). 
Niehaus and Sukthankar study an effect they call "golden goose effect". Said effect means that if officials expect an increase in future illicit rents, they will curb their corrupt behavior in the present to extract rents in the future.

For this purpose Niehaus and Sukhtankar use data from the Indian welfare program National Rural Employment Guarantee Scheme (NREGS). They exploit a wage hike for NREGS recipients in the state of Orissa in 2007 to test for a "golden goose effect" and find that the permanent wage increased theft by 64% less than in case of a temporary wage increase.

The project is split into two parts. The first part addresses the replication of the results published by Niehaus and Sukhtankar and the underlying causal graphs, the second part adresses flaws in their regression design. The replication part is entirely written in *Python 3.6.9*. For the second part I use the statistical software *R 3.5*.

## Replication and Causal Graphs [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/HumanCapitalAnalysis/student-project-LeonardMK/master?filepath=Replication%20and%20Causal%20Graphs/Replication.ipynb) [![nbviewer](https://github.com/jupyter/design/blob/master/logos/Badges/nbviewer_badge.svg)](https://nbviewer.jupyter.org/github/HumanCapitalAnalysis/student-project-LeonardMK/blob/master/Replication%20and%20Causal%20Graphs/Replication.ipynb)
This section briefly summarizes the main findings by Niehaus and Sukhtankar (2013) and presents the assumed underlying causal graphs of their regression models. Their main finding is that there are in fact "golden goose" effects.

The *Python*-Code used to estimate the models in the Robustness Checks part can be found here: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/HumanCapitalAnalysis/student-project-LeonardMK/master?filepath=Replication_Program.ipynb) [![nbviewer](https://github.com/jupyter/design/blob/master/logos/Badges/nbviewer_badge.svg)](https://nbviewer.jupyter.org/github/HumanCapitalAnalysis/student-project-LeonardMK/blob/master/Replication_Program.ipynb)

## Robustness Checks [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/HumanCapitalAnalysis/student-project-LeonardMK/master?filepath=Robustness/Robustness.ipynb) [![nbviewer](https://github.com/jupyter/design/blob/master/logos/Badges/nbviewer_badge.svg)](https://nbviewer.jupyter.org/github/HumanCapitalAnalysis/student-project-LeonardMK/blob/master/Robustness/Robustness.ipynb)
In this section I assess the quality of Niehaus and Sukhtankar's (2013) results and try to further improve the accuracy of their regression models. I find first, that all of their models suffer from severe multicollinearity and second most of the sample observations are censored etiher from above, below or both. This kind of censoring leads to downward biased effect estimates. To account for this I use the Tobit-model developed by James Tobin (1958). Further Niehaus and Sukthankar corrected their standard errors on the panchayat and day level. This assumes that observations are either correlated on the panchayat and/or on the day-level. However, controlls are performed by district or block officials (administrative units). To check for the robustness of my findings I compare singificance between clusters on the block and the district level.

Most of my findings support the hypothesis stated by Niehaus and Sukthankar, however, my estimates imply stronger effects as estimated in the original paper. Contrary to Niehaus and Sukthankar I find that corruption changed differentially between groups. Also, I find that in some cases past corruption opportunities predict corruption.

The *R*-Code used to estimate the models in the Robustness Checks part can be found here: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/HumanCapitalAnalysis/student-project-LeonardMK/master?filepath=Robustness_checks.ipynb) [![nbviewer](https://github.com/jupyter/design/blob/master/logos/Badges/nbviewer_badge.svg)](https://nbviewer.jupyter.org/github/HumanCapitalAnalysis/student-project-LeonardMK/blob/master/Robustness_checks.ipynb)

## References
***Law Enforcement, Malfeasance, and Compensation of Enforcers***, Gary S. Becker and George J. Stigler; (*Journal of Legal Studies*, 1974, 3(1); 1-18).

***A Practitioner's Guide to Cluster-Robust Inference***, A. Colin Cameron and Douglas L. Miller; (*Journal of Human Resources*, Spring 2015, 50(2); 317-373).

***Electoral Accountability and Corruption: Evidence from the Audits of Local Governments***, Claudio Ferraz and Frederico Finan; (*American Economic Review*, 2011, 101(4); 1274-1311).

***Econometric Analysis, 5th edition***, William H. Greene; (*Upper Saddle River, NJ Prentice Hall*, 2003).

***Corruption Dynamics: The Golden Goose Effect***, Paul Niehaus and Sandip Sukhtankar; (*American Economic Journal: Economic Policy*, 2013, 5(4); 230-269).

***Monitoring Corruption: Evidence from a Field Experiment in Indonesia***, Benjamin A. Olken; *Journal of Political Economy*, 2007, 115(2); 200-249).

***Estimation of Relationships for Limited Dependent Variables*** James Tobin; (*Econometrica*, 1958, 26; 24-36).


[![Build Status](https://api.travis-ci.org/HumanCapitalAnalysis/student-project-LeonardMK.svg?branch=master)](https://travis-ci.org/HumanCapitalAnalysis/student-project-LeonardMK) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](HumanCapitalAnalysis/student-project-LeonardMK/blob/master/LICENSE)