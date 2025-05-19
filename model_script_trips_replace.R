rm (list = ls())
# remotes::install_github("meta-analyses/drpa")

# ---- Get libraries ----
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(srvyr)
library(stringr)
library(doParallel)
library(ParallelLogger)
library(caret)
library(devtools)
library(drpa)
# library(magrittr)


# ---- Create directories -----
# city <- "melbourne"
city <- "brisbane"

# Working directory
scenarioLocation <- paste0("./scenarios/", city, "-scenarios")
scenarioTripsLocation <- paste0(scenarioLocation, "/scenarioTrips")
finalLocation     <- paste0("output/", city, "-outputs")

# Local path to result folder
source("Private/Paths.R")
# local_dir_path <- "C:/home/"

# Local drive-results (large files)

outputLocation       <- paste0(local_dir_path, "results/scenarioTripsReplace/", city, "-outputs-raw")
combinedLocationDisease     <- paste0(local_dir_path, "results/scenarioTripsReplace/", city, "-outputs-combined/disease")
combinedLocationLifeYears <- paste0(local_dir_path, "results/scenarioTripsReplace/", city, "-outputs-combined/LifeYears")
combineLocationOutputAgg <- paste0(local_dir_path, "results/scenarioTripsReplace/", city, "-outputs-combined/OutputAgg")


# Create directories, in case not created yet
dir.create(outputLocation , recursive=TRUE, showWarnings=FALSE)
dir.create(finalLocation, recursive=TRUE, showWarnings=FALSE)
dir.create(combinedLocationDisease, recursive=TRUE, showWarnings=FALSE)
dir.create(combinedLocationLifeYears, recursive=TRUE, showWarnings=FALSE)
dir.create(combineLocationOutputAgg, recursive=TRUE, showWarnings=FALSE)


# ---- Get scenarios names and data -----

maxDistanceWalk <- c(0,1,2)
maxDistanceCycle <- c(0,2,5,10)

tripPurpose <- c("commuting", "all")

tripPurposeFull <- c("Work,Education",
                     "Leisure,Shopping,Work,Education,Other")

tripPurposeDF <- data.frame(purpose=tripPurpose,
                            purpose_full=tripPurposeFull,
                            stringsAsFactors=FALSE)


scenarios_ShortTrips <- crossing(data.frame(max_walk=maxDistanceWalk),
                                 data.frame(max_cycle=maxDistanceCycle),
                                 data.frame(purpose=tripPurpose)) %>%
  filter(max_walk!=max_cycle) %>%
  inner_join(tripPurposeDF) %>%
  mutate(scenario=paste0(purpose,"_",max_walk,"_",max_cycle))%>%
  mutate(scenario_location=paste0(scenarioLocation,"/",scenario,".csv")) %>%
  mutate(trips_location=paste0(scenarioTripsLocation,"/",scenario,".csv")) %>%
  mutate(output_location=paste0(outputLocation,"/",scenario))


# ---- Functions to run model ----
source("Scripts/data_prep/mmet_pp.R")
source("Scripts/run_model.R")
source("Scripts/data_prep/population_prep.R")

# --- Fixed inputs ---

mslt_general="Data/processed/mslt/mslt_df.csv"
death_rate_periodic="Data/processed/mslt/deaths_periodic.csv"
death_rates_projections="Data/processed/mslt/deaths_projections.csv"
population_estimates = "Data/original/abs/population_estimates_age_sex_2001_to_2021.xlsx"
census_population_before = "Data/original/abs/population_census_GCCSA_2016.xlsx"
census_population_after = "Data/original/abs/population_census_GCCSA_2021.xlsx"
disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv"

## Victoria/Queensland specific
## (see 'data_prep/death rate projection checks.R' for notes on location_assumption)
if (city == "melbourne") {
  location_deaths_periodic="Victoria"
  location_deaths_projections="Victoria"
  location_population="Greater Melbourne"
  location_assumption="medium"
} else if (city == "brisbane") {
  location_deaths_periodic="Queensland"
  location_deaths_projections="Queensland"
  location_population="Greater Brisbane"
  location_assumption="high"
}

MSLT_DF <- read.csv(mslt_general,as.is=T,fileEncoding="UTF-8-BOM")

death_rate_periodic <- read.csv(death_rate_periodic,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == location_deaths_periodic) %>%
  dplyr::select("sex_age_cat", "mx")
MSLT_DF <- left_join(MSLT_DF, death_rate_periodic)

death_projections <- read.csv(death_rates_projections,as.is=T,fileEncoding="UTF-8-BOM") %>% 
  dplyr::filter(location == location_deaths_projections, assumption == location_assumption)

population <- GetPopulation(
  population_estimates = population_estimates,
  census_population_before = census_population_before,
  census_population_after = census_population_after,
  location = location_population)
MSLT_DF <<- left_join(MSLT_DF, population)
MSLT_DF$age <- as.numeric(MSLT_DF$age)


# Age and sex cohorts to model (always run all, results can be viewed by age and sex)
i_age_cohort <<- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)
i_sex <<- c("male", "female")

## DATA FILES FOR MODEL  

DISEASE_SHORT_NAMES <<- read.csv("Data/processed/mslt/disease_names.csv",as.is=T,fileEncoding="UTF-8-BOM")

include <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM") %>% 
  dplyr::filter(physical_activity == 1)

### Exclude diseases with no effect based on https://shiny.mrc-epid.cam.ac.uk/meta-analyses-physical-activity/
### Note 'alzheimer's disease' is not excluded, as it is an acronym for 'Alzheimers disease and other 
###   dementias', and the 'other dementias' should not be excluded
### DRFs for fatal and non-fatal
DISEASE_SHORT_NAMES <<- DISEASE_SHORT_NAMES %>%
  dplyr::filter(acronym %in% include$acronym) %>%
  dplyr::filter(!acronym %in% c("bladder-cancer", "esophageal-cancer", "kidney-cancer", "diabetes",
                                "prostate-cancer", "rectum-cancer", "parkinson's-disease"))

DISEASE_INVENTORY <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
  dplyr::filter(acronym %in% DISEASE_SHORT_NAMES$acronym)

SCEN_SHORT_NAME <- c("base", "scen1")

# Incidence and mortality trends
incidence_trends <- bind_rows(
  read.csv("Data/processed/mslt/incidence_trends_m.csv",as.is=T,fileEncoding="UTF-8-BOM"),
  read.csv("Data/processed/mslt/incidence_trends_f.csv",as.is=T,fileEncoding="UTF-8-BOM")
)

mortality_trends <- bind_rows(
  read.csv("Data/processed/mslt/mortality_trends_m.csv",as.is=T,fileEncoding="UTF-8-BOM"),
  read.csv("Data/processed/mslt/mortality_trends_f.csv",as.is=T,fileEncoding="UTF-8-BOM")
)


# --- Parameters ----

# To produce deterministic results, set NSAMPLES <- 1 and UNCERTAINTY <- F
NSAMPLES <- 10# 00
UNCERTAINTY <- T

### MSLT & PIFs options

#### 1) Include pifs for all-cause mortality impacting on all cause mortality instead of individual diseases
# accumulated change in mortalities
all_cause <- FALSE ## Choose true for all-cause-mortality pifs modifying all cause mortality instead of the 
## summation of changes from individual diseases
#### 2) Use all_cancer pif for individual cancers instead of individual diseases pifs, use disease specific mortality changes
# changes in individual diseases mortality

cancers_all <- FALSE

### 3) Use all_cancer pif for individual cancers insted of individual pifs, and all-cause mortality pif instead of disease specific changes mortality
### combine 1 and 2


parameters  <-   GetParameters(
  DIABETES_IHD_RR_F= c(2.82, 2.35, 3.38), 
  DIABETES_STROKE_RR_F= c(2.28, 1.93, 2.69),
  DIABETES_IHD_RR_M= c(2.16, 1.82, 2.56),
  DIABETES_STROKE_RR_M= c(1.83, 1.60, 2.08))


### VSLY and health cost parameters
# Office of Best Practice Regulation value of statistical life year in 2022 $
# https://oia.pmc.gov.au/sites/default/files/2022-09/value-statistical-life-guidance-note.pdf
VSLY.2022 <- 227000

# Victoria/Queensland wage price index 2019 and 2022
# 6345.0 Wage Price Index, Australia, Table 2a. Total Hourly Rates of Pay
# Excluding Bonuses: All Sectors by State, Original (Financial Year Index Numbers 
# for year ended June quarter), 
# https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/wage-price-index-australia/latest-release#data-downloads
melb.WPI <- c(131.9, 140.3)
bris.WPI <- c(130.8, 138.7)

if (city == "melbourne") {
  VSLY <- VSLY.2022 * (melb.WPI[1] / melb.WPI[2])
} else if (city == "brisbane") {
  VSLY <- VSLY.2022 * (bris.WPI[1] / bris.WPI[2])
}

discount.rates <- c(3, 5, 7)

disease.costs <- read.csv("./Data/processed/disease costs.csv")


# ---- Run model ----

print(paste0("iterating through ",nrow(scenarios_ShortTrips)," scenarios at ",Sys.time()))
# number_cores <- max(1,floor(as.integer(detectCores())*0.8))
number_cores <- detectCores()
cl <- makeCluster(number_cores)
cat(paste0("About to start processing results in parallel, using ",number_cores," cores\n"))
seeds <-1:NSAMPLES
registerDoParallel(cl)
start_time = Sys.time()

results <-  foreach::foreach(seed_current=seeds,.export=ls(globalenv())) %:%
  
  foreach::foreach(i=1:nrow(scenarios_ShortTrips), # Try 10 scenarios at the time
                   .combine=rbind,
                   .verbose=F,
                   .packages=c("dplyr","tidyr","stringr","readr","readxl","data.table","srvyr")
                   
  ) %dopar% {
    for(p in 1:length(parameters))
      assign(names(parameters)[p],parameters[[p]][[seed_current]],pos=1)
    
    if (file.exists(scenarios_ShortTrips[i,]$scenario_location))
      CalculationModel(output_location=scenarios_ShortTrips[i,]$output_location,
                       persons_matched= read.csv(scenarios_ShortTrips[i,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM"))
    
    end_time = Sys.time()
    end_time - start_time
    stopCluster(cl)
    
    cat(paste0("\n scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
    
    # TESTING - this should flush memory without removing environmental variables
    # (still to be trialled on run of more than 80)
    .rs.restartR()
  }


# ---- Summarize results ---------------

# ---- Health outcomes ----
# Combine outputs and save 
# Diseases
for (i in 1:nrow(scenarios_ShortTrips)){
  combineOutputs(paste0(scenarios_ShortTrips[i,]$output_location,'/disease/'), 
                 paste0(combinedLocationDisease,"/",scenarios_ShortTrips[i,]$scenario, ".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

# Life years
for (i in 1:nrow(scenarios_ShortTrips)){
  combineOutputs(paste0(scenarios_ShortTrips[i,]$output_location,'/life_years/'), 
                 paste0(combinedLocationLifeYears,"/",scenarios_ShortTrips[i,]$scenario, ".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}

# Over life course 
for (i in 1:nrow(scenarios_ShortTrips)){
  combineOutputs(paste0(scenarios_ShortTrips[i,]$output_location,'/output_df_agg/'), 
                 paste0(combineLocationOutputAgg,"/",scenarios_ShortTrips[i,]$scenario, ".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n"))
}


# Calculate statistics outputs
output_diseases_change <- CalculateDisease(inputDirectory=paste0(local_dir_path, "results/scenarioTripsReplace/", city, "-outputs-combined/disease"))
output_life_years_change <- CalculateLifeYears(inputDirectory=paste0(local_dir_path, "results/scenarioTripsReplace/", city, "-outputs-combined/LifeYears")) 

## Do list and then append list
index <- 1
list_output_agg <- list()
for (i in 1:nrow(scenarios_ShortTrips)) {
  list_output_agg[[index]] <- CalculateOutputAgg(paste0(local_dir_path, 
                                                        "results/scenarioTripsReplace/", city, "-outputs-combined/OutputAgg/", scenarios_ShortTrips[i,]$scenario, ".rds"))
  index <- index + 1
}

output_df_agg <- do.call(rbind.data.frame, list_output_agg)

# Save results 

saveRDS(output_diseases_change,paste0(finalLocation,"/output_diseases_change.rds"))
saveRDS(output_life_years_change,paste0(finalLocation,"/output_life_years_change.rds"))
saveRDS(output_df_agg, paste0(finalLocation,"/output_df_agg.rds"))


# ---- Transport -----

print(paste0("summarising transport modes ",nrow(scenarios_ShortTrips)," scenario outputs at ",Sys.time()))
scenarioTrips<-NULL
for (i in 1:nrow(scenarios_ShortTrips)){
  scenarioTripsCurrent<-summariseTransport(scenarios_ShortTrips[i,]$trips_location,
                                           scenarios_ShortTrips[i,]$scenario)
  scenarioTrips<-bind_rows(scenarioTrips,scenarioTripsCurrent) %>%
    dplyr::filter(participant_wt!=0) ## Some weights had 0 value
  cat(paste0("\n combined transport scenario ",i,"/",nrow(scenarios_ShortTrips)," complete at ",Sys.time(),"\n")) }

scenarioTrips <- transportOucomes(scenarioTrips)

saveRDS(scenarioTrips,paste0(finalLocation,"/output_transport_modes.rds"))


# ----- Physical activity -----
PA <- PAOutcomes(scenarioLocation)

saveRDS(PA[["PAall"]],paste0(finalLocation,"/PAall.rds"))
saveRDS(PA[["PAallGuide"]],paste0(finalLocation,"/PAallGuide.rds"))

