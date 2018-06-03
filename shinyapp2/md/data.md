## Data

### Main data set
The data that was used consisted of 46 samples from malaria patients and 27 variables (see figure 1 for a preview [INSERT LINK HERE]). As various values were missing (see figure 2 [INSERT LINK HERE]), we only took the samples that were complete. However, only 21 samples were complete on all 27 variables. Therefore, we only discarded the non-complete samples after variable selection (see chapter _Methods_ [INSERT LINK HERE]), ie we discarded the samples that were not complete on the variables that we identified as most explaining. This complete data set containing 40 samples and for 6 important variables (Parasitemia percentage, parasitemia density, total white blood cell count, lymphocyte percentage, monocyte percentage, and neutrophil percentage) and the repsonse variable (percentage of reads that map to the pathogen).

### Two more data sets were available:
One consisted of RNA Seq data from mice containing 33 samples. However, parasitemia percentage was the only variable available for prediction. Additionally, it contained many zero values where it was not clear whether this was due to measurement errors or actual non-infected mice. 
Here, the logit transformation proved to be useful as the residuals of a linear model showed highly non-normal behaviour. A logit regression model has been developed and is available, please contact [INSERT EMAIL ADRESS/ MAILTO].

The other data set analysed captured the SPVs (surrogate proportion variables) for each parasite developmental stage. The measurements were from different time points in the life cycle (0hrs, 24hrs and 48 hrs) progressing from most immature to most mature stages. It also contained the level of gametocytes (Gam5) which are the sexual stage of the parasite.

Significant relationships between the developmental stage and some variables, ie increase in total white cell count, PfHRP2 concentration, and lactate as well as a decrease in total red blood cell count, hemoglobin concentraion and  parasite clone could be found. 

However, the developmental stages were not directly correlated to our response variable, and did not explain the outliers found in our dataset. Therefore, we did not incorporate them in the model presented.
Nontheless, they might be relevant for more complex models and once more data has become available.

[INSERT FIGURE 1]
[INSERT FIGURE 2]
