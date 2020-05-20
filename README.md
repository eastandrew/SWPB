# SWPB =  Stagewise Probit Model and Dose Analysis
Fit probit model to entered data and then choose new doses in between 10th and 90th percentiles on log scale.  

The model refreshes realtime/automatically every time new data is type into the side panel.

## Enter Data
Type in doses, then number of animals responding/moribund, and then number of animals in each treatment.

Lastly, enter the number of doses you would like in your n-th stage including the "current" LC10 and LC90 as probit based brackets.  


## Tabs  

* Data Table shows the data YOU entered for QAQC purposes.
* Plot shows the fitted probit model and the mean response for each treatment.
* Summary shows the details of the fitted model from the drc package.  
* LC Values shows estimate, lower CI, and upper CI doses for selected proportional responses.  
* New Dose Chooser shows the doses bracketed by the LC10 and LC90 and the estimated response given the current model.  


# Shiny App 
To run shiny app:

`library(shiny)`

`runGitHub("doseresponseplanner", "eastandrew")`
