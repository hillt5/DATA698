---
title: Short Paper
author:
  - name: Alice Anonymous
    email: alice@example.com
    affiliation: Some Institute of Technology
    correspondingauthor: true
    footnote: 1
  - name: Bob Security
    email: bob@example.com
    affiliation: Another University
  - name: Cat Memes
    email: cat@example.com
    affiliation: Another University
    footnote: 2
  - name: Derek Zoolander
    email: derek@example.com
    affiliation: Some Institute of Technology
    footnote: 2
address:
  - code: Some Institute of Technology
    address: Department, Street, City, State, Zip
  - code: Another University
    address: Department, Street, City, State, Zip
footnote:
  - code: 1
    text: "This is the first author footnote."
  - code: 2
    text: "Another author footnote."
abstract: |
  This is the abstract.

  It consists of two paragraphs.
keywords: 
  - keyword1
  - keyword2
journal: "An awesome journal"
date: "2022-05-15"
classoption: preprint, 3p, authoryear
bibliography: mybibfile.bib
linenumbers: false
numbersections: true
# Use a CSL with `citation_package = "default"`
# csl: https://www.zotero.org/styles/elsevier-harvard
output: 
  rticles::elsevier_article:
    keep_tex: true
    citation_package: natbib
---

## Abstract 

Adverse drug events are a serious consequence of medication therapy. Previous research has associated adverse events with poorer outcomes, and suggested some patients are at higher risks for side effects. Using hospital admission records from New York State in 2018, the most common adverse events were obtained and the overall impact of an adverse drug event was estimated. A model for predicting prospective risk based on patient characteristics was developed. 

## Background 

Adverse drug event (ADE) is the comprehensive term for the negative effects related to medication therapy. ADE’s may be unforeseen or known, and vary in severity from untoward changes in laboratory values to injury and death. Some medications have therapeutic dosing close to their corresponding toxic dose, which is a characteristic described as narrow therapeutic index. In these situations, prospective monitoring may be warranted.  

In the United States, the Food and Drug Administration manages the publicly available FDA Adverse Event Reporting System to collect voluntary reports from consumers and healthcare practitioners. In contrast, New York State mandates that adverse events occurring in hospitals and diagnostic centers are reported. 

The US government also has a program offering access to detailed hospital admissions data through its Agency for Healthcare Research and Quality (AHRQ). It combines hospital and admission characteristics as well as information about charges. The research done using these data is extensive, including applications to monitoring ADE’s. 

## Dataset 

The dataset is available through AHRQ’s healthcare cost and utilization project (HCUP), which offers several options for obtaining admissions information. There are annual reports spanning the entire United States, as well as more detailed reports for each state. Datasets for each year are further broken down into pediatric and adult, as well as inpatient, emergency, ambulatory, and readmissions data. The most recent dataset available at the time is 2018, and while the variables reported are mostly uniform there are additional admission characteristics that are reported in some states.  

Data are available in two separate files: CORE and CHARGE files. The CORE file represents patient characteristics, timing of admission and length of stay; it also includes diagnoses and procedures as described by the International Classification of Diseases (ICD-10) as well as diagnosis-related groups (DRG’s) used extensively by Medicare. The CHARGE file contains dollar and unit amounts of services rendered during each individual admission. There is also a Healthcare Common Procedure Coding System (HCPCS) revenue code as an additional descriptor of the hospital cost center where the charge originated. Each observation in both files has a unique admission number designated by the KEY attribute. In total, there were ### unique admissions with over ### charges in 2018. 

These data are also available for free via HCUPnet, a data query tool accessible on the AHRQ website. No API is available for repeated or programmable querying. HCUPnet returns counts and rates for selected diagnoses by year and region, but remains useful to researchers to confirm the validity of their own findings. 

## Methods 

The two main files were first loaded into SPSS to be translated using HCUP’s provided loader program. These were then converted to .csv format and loaded into a local SQL database. The CORE file for New York state has ### features, many of which are used to hold ICD codes and dates for ICD procedure codes. The ICD has three generic codes used for ADE’s, seen in Table ###. Based on previous research, there are also ### additional codes located in described in other ICD categories that are also correlated with the absence of an event. The strength of associations is described in Table ###. Admissions were determined by filtering ICD codes where an ADE was likely present.  

There ### additional databases I used for labeling purposes. I used a .csv with all available ICD diagnosis codes and subcodes to interpret diagnoses. There was also a corresponding list of all ICD procedure codes. A ny.gov list of FIPS codes was used to translate patient location into county and then region information. Finally, for the CHARGES file, I reformatted a table available on New York’s Department of Health website for descriptions of each revenue code. 

### Data Wrangling 

The dataset required extensive transformation to better visualize and explore each feature. ICD diagnosis codes are located in 35 columns, with an additional 35 columns indicating whether the diagnosis was present upon admission. Similarly, each ICD procedure code filled one of 25 columns, plus 50 additional columns specifying the date and year. The columns containing the codes were converted to one-hot encoded columns for the most common diagnoses. Diagnoses were sometimes specific, e.g. ‘G40’ for epileptic seizure, or more general by searching for all codes starting with ‘G’ indicating diseases of the nervous system. Procedures were handled identically, with the added step that multiple identical procedures were counted more than once. For example, if the patient received three infusions of platelets, the value would be ‘3’ rather than ‘1’.  

#### Missing Values 

For blood pressure and heart rate, values were available for a minority of observations. Instead of ignoring these data, each was refactored according to clinical categories, like Stage I Hypertension or bradycardia. In the case that patient location was unavailable via FIPS code, the hospital NPI was used as an approximate location by manually replacing with the local code. 










```
## Warning in recode.numeric(.x, !!!values, .default = .default, .missing
## = .missing): NAs introduced by coercion

## Warning in recode.numeric(.x, !!!values, .default = .default, .missing
## = .missing): NAs introduced by coercion
```








```r
##-Average LOS 


ade_cost <- ade_flag %>% select(has_ade, LOS_X, TOTCHG_X) 
ade_cost$LOS_X[ade_cost$LOS_X == 0] <- 1 #change 'zero' length of stay to '1'

ade_cost %>%
	summarize(AVG_LOS = mean(LOS_X, na.rm = TRUE))

ade_cost %>%
  filter(has_ade ==1) %>%
	summarize(AVG_LOS = mean(LOS_X, na.rm = TRUE))

ade_cost %>%
	summarize(med_los = median(LOS_X, na.rm = TRUE))

ade_cost %>%
  filter(has_ade ==1) %>%
summarize(med_los = median(LOS_X, na.rm = TRUE))
```






```
##   avg_charge
## 1   56839.97
```

```
##   avg_charge
## 1     104979
```

```
##   avg_charge
## 1   32149.41
```

```
##   avg_charge
## 1   51858.46
```




```r
stats_table <- read.csv('descriptive_statistics.csv')
colnames(stats_table) <- c('Characteristics', 'Total Population', 'Target Population')

stats_table[1,2] <- prettyNum(big.mark = ',',stats_table[1,2]) 
stats_table[1,3] <- prettyNum(big.mark = ',',stats_table[1,3]) 

stats_table %>%
replace(is.na(.), ' ') %>%
   gt() %>%
  tab_header("2018 New York Hospital Admissions, Baseline Characteristics")
```

\captionsetup[table]{labelformat=empty,skip=1pt}
\begin{longtable}{lll}
\caption*{
{\large 2018 New York Hospital Admissions, Baseline Characteristics}
} \\ 
\toprule
Characteristics & Total Population & Target Population \\ 
\midrule
n & 2,018,259 & 42,829 \\ 
Age (years) & 58.3 & 62.2 \\ 
Female (\%) & 56.2 & 48.9 \\ 
Race (\%) &   &   \\ 
     White & 54.6 & 59.7 \\ 
     Black & 17.1 & 13 \\ 
     Hispanic & 13.8 & 13.1 \\ 
     AAPI & 4.6 & 4.1 \\ 
     Native American & 0.2 & 0.7 \\ 
     Other/Missing & 9.7 & 9.7 \\ 
Hispanic (\%) &   &   \\ 
     Not Hispanic & 81.6 & 83 \\ 
     Hispanic, White & 3.2 & 2.8 \\ 
     Hispanic, Black & 0.7 & 0.6 \\ 
     Hispanic, Other Race & 9.8 & 9.8 \\ 
     Missing & 4.6 & 3.9 \\ 
ED Admission (\%) & 67 & 43.8 \\ 
Died (\%) & 2.4 & 4.8 \\ 
Length of Stay (days) & 5.7 & 9.2 \\ 
Median Length of Stay (days) & 3 & 6 \\ 
Number of Diagnoses & 12.1 & 18 \\ 
Number of Procedures & 2.1 & 2.9 \\ 
Charge (\$1K) & 56.8 & 105 \\ 
Median Charge ( \$1K) & 32.1 & 58.9 \\ 
Charge per Day (\$1K) & 13.4 & 12.1 \\ 
Median Charge per Day (\$1K) & 9.4 & 9.9 \\ 
\bottomrule
\end{longtable}


```r
ggplot(ade_flag, aes(x = TOTCHG_X, fill = has_ade)) +
  geom_density(alpha = 0.4) +
  labs(x = 'Total Average of Hospital Admission, USD', y = 'Proportion of Each Population', title = 'Average Cost Per Inpatient Visit', fill = 'Presence of ADE') +
  scale_x_continuous(trans = 'log10', labels = scales::dollar_format()) +
  theme(legend.position = c(0.25,0.75))
```

![](final-project-paper_files/figure-latex/charge-plot-1.pdf)<!-- --> 


```r
ggplot(ade_cost, aes(cost_per_day, fill = has_ade)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(trans = 'log10', labels = scales::dollar_format()) +
  labs(x = 'Average Cost per Day of Hospital Admission, USD', y = 'Proportion of Each Population', title = 'Average Cost per day of Inpatient Visit', fill = 'Presence of ADE') +
  theme(legend.position = c(0.25,0.75))
```

![](final-project-paper_files/figure-latex/cost-per-day-plot-1.pdf)<!-- --> 

## Results


### Admitting Diagnosis


```r
ade_admitting <- ade_flag %>% #most common ADEs, admitting only
  dplyr::select(I10_DX_Admitting) %>%
  group_by(I10_DX_Admitting) %>%
  summarize(n_patients = n()) %>%
  arrange(desc(n_patients)) %>%
  head(10) %>%
  left_join(icd, c('I10_DX_Admitting' = 'V3')) %>%
  select(V5, n_patients)

ggplot(ade_admitting, aes(x = fct_reorder(V5, n_patients), y = n_patients/1000)) +
  geom_col() +
  scale_x_discrete(name = 'ICD Description', labels = function(x) str_wrap(str_replace_all(x, "foo" , " "), width = 40)) +
  labs(y = "Cases, in thousands", title = "Top 10 Admitting Diagnoses, New York Hospitals 2018") +
  theme(plot.title = element_text(hjust = 0.25))+
  coord_flip()
```

![](final-project-paper_files/figure-latex/dx-admitting-1.pdf)<!-- --> 

#### Total Number of Diagnoses


```r
ggplot(ade_flag, aes(I10_NDX)) + #anomaly in the number of diagnoses
  geom_bar() +
    scale_y_continuous(name = 'Cases, in thousands', labels = function(y) y /1000) +
  labs(title='Anomaly in the Number of Diagnoses', x = 'Number of ICD-10 Diagnoses') 
```

![](final-project-paper_files/figure-latex/n-dx-plot-1.pdf)<!-- --> 



### Most Common Procedures


```r
ggplot(ade_flag, aes(I10_NPR)) +
  geom_bar() +
  scale_y_continuous(name = 'Cases, in thousands', labels = function(y) y /1000) +
  labs(title='Number of ICD-10 Procedures', x = 'Number of ICD-10 Diagnoses') +
    theme(plot.title = element_text(hjust = 0.25))
```

![](final-project-paper_files/figure-latex/n-procedures-1.pdf)<!-- --> 


```r
i10_procedure <- read.csv('i10_procedure.csv')


proc_counts <- ade_flag %>% #use similar code to determine most common procedures
  dplyr:: select(I10_PR1:I10_PR25) %>%
  gather(dx_num, Code, I10_PR1:I10_PR25,factor_key = TRUE ) %>%
  mutate(Code = as.factor(Code)) %>%
  group_by(Code) %>%
  summarize(n_patients = n()) %>%
  mutate(Code = na_if(Code, ' ')) %>%
  drop_na() %>%
  arrange(desc(n_patients)) %>%
  inner_join(i10_procedure, by = c('Code' = 'ICD.10.PCS.CODE'))  %>% #join with i10 procedure text csv
  dplyr :: select(ICD.10.PCS.CODE.DESCRIPTION, n_patients, Code)


proc_counts %>%
  arrange(desc(n_patients)) %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(ICD.10.PCS.CODE.DESCRIPTION, n_patients))) +
  geom_col(aes(y = n_patients/1000)) + 
  scale_x_discrete(name = 'ICD Description', labels = function(x) str_wrap(str_replace_all(x, "foo" , " "), width = 55)) +
  theme(legend.position = c(0.75,0.25), plot.title = element_text(hjust = 0.25)) +
  labs(title = 'Top 10 Most Common Procedures') +
  scale_y_continuous(name = 'ICD-10 Procedure Claims, in thousands') +
  coord_flip() 
```

![](final-project-paper_files/figure-latex/top-10-procedures-1.pdf)<!-- --> 

### Top Adverse Drug Events


```r
rm(ade_cost)


ade_counts <- ade_flag %>%
  filter(has_ade == 1) %>%
  group_by(Code, Description) %>%
  summarize(n_icd = n()) %>%
  left_join(icd, c('Code' = 'V3')) %>%
  arrange(desc(n_icd)) %>%
  mutate(Description = fct_relevel(Description, c('Category A1','Category A2', 'Category B2', 'Category C')))
```

```
## `summarise()` has grouped output by 'Code'. You can override using the
## `.groups` argument.
```

```r
ade_counts %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(V5, n_icd))) +
  geom_col(aes(y = n_icd/1000, fill = Description)) + 
  scale_x_discrete(name = 'ICD Description', labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                 width = 40)) +
  theme(legend.position = c(0.75,0.25), plot.title = element_text(hjust = 0.25)) +
  labs(title = 'Top 10 Most Common ADE\'s, 2018') +
  scale_y_continuous(name = 'Cases, in thousands') +
  coord_flip() 
```

![](final-project-paper_files/figure-latex/top-10-ade-1.pdf)<!-- --> 


```r
ade_counts2 <- ade_flag %>%
  filter(has_ade == 1) %>%
  filter(Description != 'Category C') %>%
  group_by(Code, Description) %>%
  summarize(n_icd = n()) %>%
  left_join(icd, c('Code' = 'V3')) %>%
  arrange(desc(n_icd)) %>%
  mutate(Description = fct_relevel(Description, c('Category A1','Category A2', 'Category B2', 'Category C')))
```

```
## `summarise()` has grouped output by 'Code'. You can override using the
## `.groups` argument.
```

```r
ade_counts2 %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(V5, n_icd))) +
  geom_col(aes(y = n_icd/1000, fill = Description)) + 
  scale_x_discrete(name = 'ICD Description', labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                 width = 40)) +
  theme(legend.position = c(0.75,0.25), plot.title = element_text(hjust = 0.25)) +
  labs(title = 'Most Common Category \'A\' drug reactions') +
  scale_y_continuous(name = 'Cases, in thousands') +
  coord_flip() 
```

![](final-project-paper_files/figure-latex/top-10-ade-cat-a-1.pdf)<!-- --> 


```r
ade_counts2 %>%
  filter(Description == 'Category B2') %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(V5, n_icd))) +
  geom_col(aes(y = n_icd)) + 
  scale_x_discrete(name = 'ICD Description', labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                 width = 35)) +
  labs(title = 'Most Common Category \'B\' drug reactions') +
  scale_y_continuous(name = 'Cases') +
  coord_flip() 
```

![](final-project-paper_files/figure-latex/ade-cat-B-1.pdf)<!-- --> 


```r
ade_counts %>%
  filter(Description == 'Category C') %>%
  head(3) %>%
  ggplot(aes(x = fct_reorder(V5, n_icd))) +
  geom_col(aes(y = n_icd/1000)) + 
  scale_x_discrete(name = 'ICD Description', labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                 width = 35)) +
  labs(title = 'Most Common Category \'C\' drug reactions') +
  scale_y_continuous(name = 'Cases, in thousands') +
  coord_flip() 
```

![](final-project-paper_files/figure-latex/ade-cat-c-1.pdf)<!-- --> 

### Target Prediction 

#### Techniques 

#### Model Performance 



```r
performance_df <- read.csv('models_performance.csv')

performance_df <- performance_df %>%
  mutate(Accuracy = 100*round((TP+TN)/(TP+TN+FP+FN),3)) %>%
  mutate(Precision = 100*round(TP/(TP + FP),3)) %>%
  mutate(Recall = 100*round(TP/(TP + FN),3))

performance_df <- performance_df %>%
  mutate(F1 = round(2*Precision*Recall/(Precision + Recall)/100,2)) %>%
  mutate(TPR = 100*round(TP/(TP+FN),3)) %>%
  select(-TN,-TP,-FN,-FP)


performance_df %>%
  gt() %>%
  tab_header("Selected Model Performance for the Detection of Adverse Drug Events")
```

\captionsetup[table]{labelformat=empty,skip=1pt}
\begin{longtable}{lrrrrr}
\caption*{
{\large Selected Model Performance for the Detection of Adverse Drug Events}
} \\ 
\toprule
Model & Accuracy & Precision & Recall & F1 & TPR \\ 
\midrule
GLM & 77.6 & 6.7 & 71.9 & 0.12 & 71.9 \\ 
SGB & 77.2 & 7.4 & 82.4 & 0.14 & 82.4 \\ 
Random Forest & 2.2 & 5.6 & 2.2 & 0.03 & 2.2 \\ 
Neural Net & 67.9 & 5.6 & 85.6 & 0.11 & 85.6 \\ 
\bottomrule
\end{longtable}





## Conclusions and Future Directions 
