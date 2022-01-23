# fb-privacy-modeling
R-Targets Analysis Pipeline for Inferential Modeling of Hand-Coded Data of Facebook Posts.

Please refer to [this repository](https://github.com/student-privacy/download-baseline-data) 
for more information on the downloading process of the CrowdTangle data
and this [repository](https://github.com/student-privacy/baseline-data) 
for the data cleaning and aggregation process.

Folder structure:

```
.
├── _targets.R		                        # targets pipeline instruction script
├── R/functions.R		                      # file containing all R code to used in the targets pipeline
├── rds/             	                    # data folder (please contact authors for access)
│   ├── 400-aggregated-posts.rds        	# Hand-coded Facebook posts with NCES and FB variables
│   └── accounts-n_posts-hashmap.rds			# Dictionary of FB-Account: Number of Posts in full Data Set
├── analysis.Rmd                    	# RMarkdown Notebook to Summarize Results of Analysis
├── irr.Rmd		                        # RMarkdown Notebook to Summarize Hand-Coding Reliability
└── README.md
```
