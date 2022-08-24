# Summary
Quality-Adjusted Life Expectancy (QALE) is an intuitive summary measure of 
population health, which combines information on mortality with information on 
health. In contrast to Healthy Life Expectancy (HLE), QALE moves away from a 
binary perspective on health (healthy at age X vs. unhealthy at age X+1) by 
capturing health on a more granular scale across the entire life course. The 
described research pipeline estimates QALE for Local Authority Districts in 
Scotland, England, and Wales based on publicly available data. This README file 
provides general guidance on how to run and control the underlying research 
pipeline. 

# Pre-Requisites
In order to run the research pipeline in its entity from start to finish, there 
are three important pre-requisites:

**1. Software:** Ensure you have installed a recent version of R (Version 4.2 
or higher) and R Studio (Build 485 or higher). You do not need to install and 
load packages yourself. The program will examine this and react - if required - 
on the fly.

**2. Survey Data:** Register with [UK Data Service](https://ukdataservice.ac.uk/)
and download 
[Understanding Society - General License Version](https://beta.ukdataservice.ac.uk/datacatalogue/doi/?id=6614#!#16).
The data set is free and publicly available. Once you have registered with UK 
data service and agreed to the terms and conditions of using Understanding Society 
data, you can download all waves. Store the downloaded *X_indresp_dta* files 
of waves *e* to *j* in the program folder *RData/UnderstandingSociety/*. For 
example for wave *e* the final path would be: *RData/UnderstandingSociety/e_indresp_dta*. 
You can also skip this stage, in which case you answer "no" in the prompted dialog 
when running the pipeline. In this case, EQ-5D utility scores derived from wave *j* 
of Understanding Society are loaded by default, limiting the cross-sectional 
analysis to the year of 2019.

**3. Mortality Data:**  Create a Human Mortality Database (HMD) Account for data 
queries using the following link to the 
[HMD website - former version](https://former.mortality.org/). We are using the 
former version of the HMD, as the new version does not proide an API (yet). When 
running the program you will be prompted to enter your username and password for 
this website. 

# Running the Pipeline

Once these pre-requisites have been fulfilled, you can open the project file 
*QALE_Exemplar.Rproj* in the main program folder. This will prompt a new RStudio 
Session. You can then open and run the main control file of the program *01_main.R* 
(CTRL + A then CTRL + ENTER). All results will be stored in the folder *ROutput/*.
You are free to change parameters in the definitions section of the main control 
file *01_main.R* , for example change the year of the cross-sectional analysis 
[Line 75 - Line 95]. The pipeline might break the very first time you run it - 
likely due to the fact that the RSession ist not able to immediately fetch from 
online sources - just re-run everything again and it should work.

# Bug Reports 

Please direct all bug reports to andreas.hoehn@glasgow.ac.uk

# Key References 

De Beer, Joop. "Smoothing and projecting age-specific probabilities of death by TOPALS." Demographic Research 27 (2012): 543-592.

Jagger, C., Cox, B., Le Roy, S., Clavel, A., Robine, J. M., Romieu, I., & Van Oyen, H. (1999). Health expectancy calculation by the Sullivan method: a practical guide.

Lawrence, W. F., & Fleishman, J. A. (2004). Predicting EuroQoL EQ-5D preference scores from the SF-12 Health Survey in a nationally representative sample. Medical Decision Making, 24(2), 160-169.

Modig, K., Rau, R., & Ahlbom, A. (2020). Life expectancy: what does it measure?. BMJ open, 10(7), e035932.

Rau, R., & Schmertmann, C. P. (2020). District-level life expectancy in Germany. Deutsches Ärzteblatt International, 117(29-30), 493.

Schmertmann, C. P. (2019). Fitting a TOPALS mortality model with age-grouped data, by Penalized Iteratively Weighted Least Squares (PIRLS). https://github.com/schmert/TOPALS/blob/master/TOPALS_fitting_with_grouped_data.pdf

# About

**Title:** Estimating Quality-Adjusted Life Expectancy (QALE) for Local Authority Districts in the UK

**Author:** Andreas Höhn

**Version:** Beta 0.9

**Updated:** 2022-08-24



