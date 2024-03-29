
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ACC AHA BP guideline groups

This repository contains all of the code necessary to reproduce the
manuscript *Predicted cardiovascular risk for US adults with diabetes,
chronic kidney disease, and ≥ 65 years of age*. The abstract and one
central illustration for the paper are below.

# ABSTRACT

**Background:** The 2017 American College of Cardiology/American Heart
Association blood pressure (BP) guideline recommends using 10-year
predicted atherosclerotic cardiovascular disease (ASCVD) risk to guide
decisions to initiate antihypertensive medication.

**Objectives:** Determine if the majority of US adults in subgroups
defined by diabetes, chronic kidney disease (CKD), and age ≥65 years
have high ASCVD risk (i.e., 10-year predicted ASCVD risk ≥10% or
clinical CVD), and estimate the age-adjusted probability of having high
ASCVD risk.

**Methods:** Adults aged 40-79 years from the National Health and
Nutrition Examination Survey 2013-2018 were included (n=8,803). We
predicted 10-year ASCVD risk using the Pooled Cohort risk equations and
clinical CVD was self-reported. Analyses were conducted overall and
among those with stage 1 hypertension, defined by systolic BP of 130-139
mm Hg or diastolic BP of 80-89 mm Hg.

**Results:** Among US adults, an estimated 72.3%, 64.5%, and 83.9% of
those with diabetes, CKD, and age ≥65 years had high ASCVD risk,
respectively. Among US adults with stage 1 hypertension, an estimated
55.0%, 36.7%, and 72.6% of those with diabetes, CKD, and age ≥65 years
had high ASCVD risk, respectively. The probability of having high ASCVD
risk increased with age and exceeded 50% for US adults with diabetes and
CKD at ages 52 and 57 years, respectively. For those with stage 1
hypertension, these ages were 55 and 64, respectively.

**Conclusions:** Most US adults with diabetes, CKD, or age ≥65 years had
high ASCVD risk. However, many with stage 1 hypertension did not.

## Central Illustration

Distribution of US adults by blood pressure and antihypertensive
medication use categories. In text, the proportion of adults with stage
1 hypertension at high atherosclerotic cardiovascular disease risk in
subgroups defined by diabetes, chronic kidney disease and age ≥ 65
years.

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

High atherosclerotic cardiovascular disease risk was defined by a
10-year predicted risk for atherosclerotic cardiovascular disease ≥ 10%
or clinical cardiovascular disease. Data are from the National Health
and Nutrition Examination Survey, 2013-2018.

ASCVD = atherosclerotic cardiovascular disease; CKD = chronic kidney
disease

*Note:* Currently under review at *Journal of Hypertension*.

# INSTRUCTIONS FOR REPRODUCTION

To reproduce this manuscript, you should first obtain all of the code in
this repository. To do this, fork or download its contents. Once the
repository is locally available and its contents unzipped, find and open
the `ACC AHA BP guideline groups.Rproj` file. This should trigger
Rstudio to open the project for you in a local R session.

To reproduce the manuscript as-is, follow the steps below:

1.  Open `packages.R` and run all of the code in the file.

    -   The `pacman` package must be installed on your machine to run
        this code

    -   Two packages need to be installed from GitHub: `table.glue` and
        `PooledCohort`. If you asked whether you want to update
        dependencies - say ‘no’. If `pacman` is not able to do this for
        you, you can use `remotes::install_github('bcjaeger/table.glue)`
        and `remotes::install_github('bcjaeger/PooledCohort)` to get
        these packages manually.

    -   It may take more than one try to successfully download and/or
        update the packages and their dependencies. If any installations
        fail, try restarting your R session and using
        `install.packages()` to install any packages that are causing
        issues for `pacman`.

2.  This project is built around `drake`, a workflow package. To produce
    every target in the drake workflow, run `drake::r_make()` in your
    console.

3.  Allow Rstudio to access the internet if prompted, as
    `drake::r_make()` will be downloading NHANES data from the NHANES
    website while it produces the manuscript.

4.  `drake::r_make()` will build every target, ending with the
    manuscript and finally the `README.md` file. Simply open the file
    ‘doc/ACCAHA\_BP\_groups.docx’ to see the manuscript you built.

*Note:* If an error occurs while the project is being made and you have
successfully downloaded all of the packages in `packages.R`, please file
an issue in this repository.

## Make a different manuscript

*Note:* Not everything in the paper will update when you change
parameters. For example, the exclusion figure was made using InkScape
and the paper’s central illustration was made using PowerPoint, so they
won’t update if you change parameters in the `plan.R` file. However, the
remaining figures and text are.

These instructions assume you have successfully completed the steps
above. If you would like to change some initial parameters for the
manuscript, you can do so by following these instructions:

1.  Open `R/plan.R`.

2.  Notice that `exams = c(2013, 2015, 2017)` is the first argument in
    `drake_plan()`. Change this to `exams = c(2011, 2013, 2015, 2017)`
    to include a fourth NHANES exam.

3.  Save your changes to the `R/plan.R` file and then run
    `drake::r_make()` in your console.

4.  Enjoy reading your fascinating new paper!

There are a few other parameters that can be changed in `R/plan.R`:

-   `fasted_hrs_lower = 8`,
-   `fasted_hrs_upper = 24`,
-   `gluc_cutpoint_fasted = 126`,
-   `gluc_cutpoint_fed = 200`,
-   `hba1c_cutpoint = 6.5`,
-   `egfr_cutpoint = 60`,
-   `acr_cutpoint = 30`

These values define variables, manuscript variable definitions, and
table footnotes.
