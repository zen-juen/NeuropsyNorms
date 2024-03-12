# NeuropsyNorms
The `NeuropsyNorms` package aims to facilitate and expedite the computation of normative data for neuropsychological assessments. It is an open-source software that is still in its preliminary stages. In the spirit of open science, this project is open to contributors from different disciplines (e.g., neuropsychologists, neuroscientists, psychologists, statisticians). If that sounds like you, feel free to open an issue on this repository or drop an email!


## Functionality
`NeuropsyNorms` computes norms for:

- Trail Making Test (TMT)
- Colour Trails Test (CTT)
- Symbol Digit Modalities Test (SDMT)
- Controlled Oral Word Association Test (COWAT)
- Brief Visuospatial Memory Test-Revised (BVMT-R)
- Rey Auditory Verbal Learning Test (RAVLT)
- California Verbal Learning Test (CVLT) - *short form only for now*
- Repeatable Battery for the Assessment of Neuropsychological Status (RBANS)


## Normative Samples

A secondary aim is to increase awareness about the nature of the normative samples that neuropsychological assessments are based on. Hence, every time you run a line of code to compute normative data, information about the reference group (i.e., sample size, language of test administration, age range, education) is printed.

A compilation of the normative studies used in this package can be found [here](https://docs.google.com/spreadsheets/d/1Cd1jCAim4IS-qomW3ujmqJ1pAh5Nk7Rsi45yh1WDcOY/edit?usp=sharing).


## Demonstration

First, you'll need to have the following downloaded:
- R (https://cran.r-project.org/bin/windows/base/)
- R Studio (https://rstudio.com/products/rstudio/download/)

Then, run the following commands in your R console: 
```
# Install and load necessary packages
install.packages("devtools")
library(devtools)
library(NeuropsyNorms)

# Compute norms
tmt_norms(education=10, age=55, trailsA=30, trailsB=40, source="Tombaugh2004_Canada")
```

This gives the following output:
```
[1] "Tombaugh (2004) study ``N`` = 911 in English-speaking individuals residing in Canada aged between 18 - 89, of education (in years) of 0-12 and >12. Tests were administered in English. Scores are age-adjusted for persons between ages 18 - 54, and both age- and education-adjusted for ages 55-89."
        Raw Scores  Z-scores    Descriptor
TrailsA         30 0.4661792       Average
TrailsB         40 2.0345731 Very Superior
```




