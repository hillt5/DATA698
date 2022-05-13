# DATA698: Analytics Master's Research Project
Final project: ADVERSE DRUG EVENTS IN NEW YORK HOSPITALS: PREDICTION AND ASSOCIATED COSTS

**DISCLAIMER**

This paper's datasets are not available to unlicensed users as the AHRQ and HCUP Central Distributor require registration, training, and payment prior to access to national and state data. If you are already trained or have questions related to HCUP files, please message me to the email attached to this Github and I will try to help.

Instead, please access descriptions of the HCUP process using the resources below:

SID Homepage: https://www.hcup-us.ahrq.gov/sidoverview.jsp

Summary statistics are available here: 
Core file: https://www.hcup-us.ahrq.gov/db/state/sidc/tools/cdstats/NY_SIDC_2018_SummaryStats_CORE.PDF
Column descriptions: https://www.hcup-us.ahrq.gov/db/state/siddist/siddistvarnote2018.jsp
Charge file: https://www.hcup-us.ahrq.gov/db/state/sidc/tools/cdstats/NY_SIDC_2018_SummaryStats_CHGS.PDF

Individual queries are available here: https://hcupnet.ahrq.gov/

SPSS Load Programs for SID are available here: https://www.hcup-us.ahrq.gov/spssload/spssload_search.jsp

**END DISCLAIMER**

SQL CODE HCUP is a combination of all queries I used while organizing the original .csv's. The two most useful queries are loadin the CORE files, which required assigning approximately 200 datatypes, and the CORE-icd joined file, which contained all observations with at least one ADE.

There are several R markdown files that I spread the work across.

feature engineering.rmd is the file where I performed all exporatory analysis and feature engineering on the CORE files. 

machine learning.rmd contains preprocessing and machine learning code. This includes splitting to training and test data, over/undersampling by ROSE, then four calls to the _caret_ package to train, predict, and generate confusion matrices for each algorithm.

final project paper.rmd contains the r-markdown file I used for the submitted paper, and graphics for the presentation.

Finally here are descriptions of local .csv's.

i10_procedure: contains descriptions of each ICD-10 procedure code
icd_csv: contains all ICD-10 diagnoses related to adverse drug events
models_performance: contains raw confusion matrix information
NY_Municipalties_and_County_FIPS_codes: all FIPS codes in New York state along with their corresponding location details
revcodes: A list of revenue codes for understanding each cost center in the CHARGES file. This was adapted from a table available here <https://www.health.ny.gov/statistics/sparcs/sysdoc/appi.htm>
