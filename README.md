# Associations Between Health and Economic Factors Across American Counties
R was used to explore publicly available data to identify correlations between county-level economic and health factors.  

## Tools

R and GoogleSheets.

## Data

 The 2014 US County Health Rankings data from the Robert Wood Johnson Foundation was used: 
- Retrieved from https://public.tableau.com/s/sites/default/files/media/County_Health_Rankings.csv

## Statistics

The data was explored numerically and visually using code in RobinKingPortfolio.R


## Findings 
The corelations below all had p-value <=6 .15E-150. Alpha was set to p-value <0.01. 
* There were significant positive correlations between **childhood poverty** and:
  * Uninsured rates (r-squared=0.246)
  * Adult obesity (r-squared=0.216)
  * Physical inactivity among adults (r-squared=0.304)
  * Unemployment (r-squared=0.239)
  * Preventable hospitalization rates (r-squared=0.159)

* There were significant positive correlations between **adult obesity** and:
  * Unemployment rates (r-squared=0.149)
  * Physical inactivity among adults (r-squared=0.458)

* There were significant positive correlations between **premature death** and:
  * Preventable hospitalization rates (r-squared=0.304)
  * Violent crime rates (r-squared=0.127)

There were significant differences across U.S. regions for several measures. Divisions in the **South** region in particular had less healthy values across several measures compared with other U.S. regions.

See *RobinKing_HealthAndEconomicFactors.pdf* for more details about the study.
