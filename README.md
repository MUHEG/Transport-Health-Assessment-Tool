# Transport Health Assessment Tool for Melbourne and Brisbane

## Citation

Zapata-Diomedi Belen, Ali Abbas,  Alan Both and Steve Pemberton (2023). Transport Health Assessment Tool for Melbourne and Brisbane. R code.

## Introduction 

The Transport Health Assessment Tool quantifies physical activity-related health impacts arising from a range of scenarios where short car trips are replaced by walking, cycling or a combination of both, based on data from metropolitan Melbourne.

The Transport Health Assessment Tool models scenarios for replacing short car trips with walking, cycling or a combination of both, accommodating options for trip purpose; age groups; and sex. The model always replaces car trips. The maximum possible distance to be replaced with walking is 2 kilometres and 5 kilometres for cycling. Note that distances replaced correspond to stages of a trip, for example, a trip to work may include multiple stages including walking to a train station, train ride itself, and walking from the station to the place of employment. Trip purposes were grouped into: 1) all trips, which includes work-related, education, leisure, shopping, pick-up or drop-off someone/something, personal business, other, accompanying someone, and at or going home; 2) commuting, which includes work-related and education trips.

For details of the methods reflected in the tool, please refer to the paper at https://doi.org/10.1016/j.jth.2023.101628 with an application for Melbourne.  Please refer to the Australian Urban Observatory at https://auo.org.au/that-brisbane/ for the Brisbane results.  An older version for Melbourne is at https://auo.org.au/that-melbourne/, and updated results for Melbourne are at https://belenzapata-diomedi.shinyapps.io/presentation_dashboard_MD/.


## Run the model

See below for steps for running the model and generating outputs

1) model_script_trips_replace.R: runs the model with two options, probabilistic and deterministic
2) presentation_paper: functions to visualise outcomes

## Data

The data to run the model is available in the data folder. 
