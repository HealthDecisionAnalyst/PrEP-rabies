# Project Title

Rabies pre-exposure prophylaxis is an underused intervention that can be cost effective across a range of scenarios.


## Authors

Adam John Ritchiea, Aronrag Meeyai, Caroline Trotter, Alexander D Douglas

## Documentation

[Documentation](https://linkto paper) 

## Decision tree model
Comparing the PEP-only to the PrEP plus PEP scenarios. 
<img width="452" alt="image" src="https://github.com/user-attachments/assets/85d75e2b-8fba-4b8e-bc6a-b57fa4d876e6">


## Required packages
ggplot2/ ggthemes/ dplyr/ plotly
## Using this model
step1) clone this repository to your computer and then run the "Results Generatiobn.R" file.

The easiest way to reproduce the results of our paper is to clone this repository to your computer and then run the "Results Generation.R" file.

A major aim of these models is that they be used to simulate One Health AMR interventions in agriculture in other real-life contexts. In order to do this, researchers and policymakers can alter the inputs to reflect the context of interest. Because it may be difficult to determine all of the necessary parameters for the context of interest, we recommend the following:

1) choose the income category scenario that is most appropriate (low-income (LIC), middle income (MIC), or high income(HIC));
2) in the input file for the general version of the model, 'inputs - general model.csv', edit the parameters in the column of the chosen income category where they are known, and leave the remaining parameters unchanged;
3) go to the general version of the model and select the relevant income category by editing the line
scenario_income <- "..... ;
4) run the model and obtain results!

the more accurately the user is able to parameterise the model to the context of the interest, the more accurate the results. An example of how to adapt the model to a country-specific context is included in the file "Example.R".




## Data
all data sources can be found in ...
## Inputs
Table 1, present input paramaters and its sources
## Outputs 
there are ...
## Model parameters and calculations
there are spredsheets contain sources for the model parameters for the model

## Results
To generate the results for this work, there are ...

## Figures
Main figures for this work including: 
1) Tornado plot for Indremental Cost-Effectiveness Ratio (ICER)
2) Individual plot of the impact of varying each key parameter in a one-way sensitivity analysis
3) Contour plot of the ICER across a range of variables

## Acknowledgements

 - [Awesome Readme Templates](https://awesomeopensource.com/project/elangosundar/awesome-README-templates)
 - [Awesome README](https://github.com/matiassingers/awesome-readme)
 - [How to write a Good readme](https://bulldogjob.com/news/449-how-to-write-a-good-readme-for-your-github-project)


## Appendix
Supplementary figures for this work including sensitive analysis for Main figures with: 0%, 1.5%, and 3% discount rates









