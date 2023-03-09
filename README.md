## A repository for the analysis and visualization code used in *How to enhance learning in less than one second* Reilly W.B., Antony. J.W., & Ranganath, C. (in prep)

### Project Description

The testing effect–the retention benefit of practicing retrieval compared to studying–and the pretesting effect–incorrectly guessing a target before learning compared to studying–demonstrate the advantages of retrieval-based learning, but no extant theories have accounted for both of these effects. Error-driven learning is the algorithm underlying the success of neural networks in which robust learning takes place by comparing a predicted pattern to a correct pattern. Here, we argue that retrieval involves the same kind of computation that takes place in neural networks.

According to our error-driven learning account, retrieval-based learning serves to “stress test” the human memory system, allowing it to learn to better predict a target from a cue, whereas in studying, there is no opportunity for the system to form a prediction because the target is already present. An alternative account holds that retrieval benefits learning because it allows learning from making errors. Here, we present results from four experiments designed to adjudicate between these two accounts. 

Participants learned Swahili-English translations in two types of conditions. In the “gap” conditions, the cue was presented before the target was presented, creating a gap during which a retrieval attempt could take place. In the “no gap” condition, the cue and the target were presented simultaneously. 

In three experiments, we found that a gap of only 600 ms was sufficient to enhance retention of word pairs compared to the no gap condition one day after learning. Due to the fact that participants made very few errors during learning, these results are consistent with the error-driven learning account and are inconsistent with the error-correction account.

The educational applications of these results are two-fold. These results show that participants do not receive marginal benefits from longer retrieval attempts than around one second, therefore formative assessment products may provide feedback relatively quickly, potentially reducing learner discomfort. A knock-on benefit of faster feedback presentation is that more learning trials may fit into the same learning block.  

### Contents

This repository includes the scripts used to munge, analyze, and visualize the data and the resulting figures. It also includes html files that contain the code and output from all statistical analyses.

### Key scripts

`preprocess_plot.py` Run interactively. Combines the two experimental phases, engineers features, identifies bad data, creates plots, outputs clean dataframes.

`stats.Rmd` Statistical analyses of learning data and cued-recall performance on the final test. Cued-recall was modeled with mixed-effects logistic regression to account for random variation among items and participants. Inference was conducted with a parametric bootstrap approach.

`stats.md` Same as above--but capable of rendering on GitHub.

`tfidf_check.py` tf-idf natural language processing technique used to identify spurious corrleations in incorrect final test performance to help identify individuals who used multiple accounts, i.e. bad data. 

### Figure 1. 
<p align="center">
	<img src = "figures/edl_design.png" width = "70%">
</p>
Experimental Design. There were two types of experimental condition. In "gap" conditions, there was a temporal gap between Swahili presentation and English word presentation. In "no gap" conditions, the Swahili and English words were presented simultaneously.

### Figure 2.
<p align="center"> 
	<img align ="center" src="figures/exp1.png" width = "60%">
</p>
Experiment 1 Cued-recall Performance. Mean proportion correct on the final cued-recall test in Experiment 1. Error bars represent standard error of the mean.

### Figure 3. 
<p align="center">
<img src = "figures/exp2.png" width = "60%">
</p>

### Figure 4. 
<p align="center">
	<img src = "figures/exp3.png" width = "60%">
</p>

### Figure 5. 
<p align="center">
	<img src = "figures/exp4_30s.png" width = "60%">
</p>

### Figure 6. 
<p align="center">
	<img src = "figures/exp4_24h.png" width = "60%">
</p>

### Tools

[![](https://img.shields.io/badge/python-3.7.9-blue)](https://www.python.org/)
[![](https://img.shields.io/badge/spyder-4.1.5-blue)](https://www.spyder-ide.org/)
[![](https://img.shields.io/badge/pandas-1.1.3-blue)](https://pandas.pydata.org/)
[![](https://img.shields.io/badge/seaborn-0.11.1-blue)](https://seaborn.pydata.org/)
[![](https://img.shields.io/badge/matplotlib-3.3.2-blue)](https://matplotlib.org/)

[![](https://img.shields.io/badge/R-4.2.2-blue)](https://www.r-project.org/)
[![](https://img.shields.io/badge/RStudio-2022.07.2-blue)](https://posit.co/products/open-source/rstudio/)
[![](https://img.shields.io/badge/tidyverse-1.3.2-blue)](https://www.tidyverse.org/)
[![](https://img.shields.io/badge/afex-1.2.0-blue)](https://cran.r-project.org/web/packages/afex/index.html)
[![](https://img.shields.io/badge/emmeans-1.8.2-blue)](https://cran.r-project.org/web/packages/emmeans/index.html)
[![](https://img.shields.io/badge/pbkrtest-0.5.1-blue)](https://cran.r-project.org/web/packages/pbkrtest/index.html)
[![](https://img.shields.io/badge/lme4-1.1.31-blue)](https://cran.r-project.org/web/packages/lme4/index.html)
[![](https://img.shields.io/badge/gt-0.8.0-blue)](https://cloud.r-project.org/web/packages/gt/index.html)
[![](https://img.shields.io/badge/rmarkdown-2.18.0-blue)](https://rmarkdown.rstudio.com/)










