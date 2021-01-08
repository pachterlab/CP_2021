# The Split Senate

Code for reproducing the results in "The Split Senate" preprint.

![Alt text](https://github.com/pachterlab/CP_2021/blob/main/116senate.png?raw=true "The Split Senate")


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

R (>= 3.6.1) is necessary for analysis:

R can be downloaded [here](https://cran.r-project.org/mirrors.html)

The RStudio IDE can be downloaded [here](https://rstudio.com/products/rstudio/download/)

### Initial Setup

1. Clone github repo

2. Move to main scripts
```
cd CP_2021/scripts
```

3. Run gendata.R
```
rscript gendata.R
```
* This will install all necessary packages, create the annotated dataset for analysis, and set the working directory.


## Running Analysis

1. senateVotes.R
	* Code to generate splits graphs for the 116th Senate
	* Analysis of apparent 'coalitions' of Democratic Senators
	* Statistical analysis of vote contribution to coalition splits
	
2. splitDistCorr.R
	* Concordance and correlation of split weight distances and L1 input distances for the 116th Senate
	
3. senateOverTime.R
	* Analysis of agreement distributions ('center' distances) across Senates from the last 30 years

4. missingSenators.R
	* Creation of distance matrices for splits graphs of senators not present for all votes in the 116th Senate

5. exampleforFig1.R
	* Code to generate example splits graph in Figure 1

All scripts utilize functions from distFuncs.R



## Authors

* Tara Chari

