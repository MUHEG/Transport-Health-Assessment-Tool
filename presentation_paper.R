######################################### SCRIPTS TO PRESENT RESULTS ###############################################
rm (list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# city <- "melbourne"
city <- "brisbane"

source("./Scripts/functions_presentation.R")

options(scipen=999)

# ---- Functions' inputs -----

# Combinations scenarios for graphs and results

scenariosDF <- crossing(data.frame(max_walk=c(0,1,2)),
                        data.frame(max_cycle=c(0,2,5,10)),
                        data.frame(purpose=c("commuting", "all"))) %>%
  filter(max_walk!=max_cycle) %>%
  mutate(scen=paste0(purpose,"_",max_walk,"_",max_cycle)) %>%
  mutate(title1=paste0(ifelse(max_walk>0, max_walk, max_cycle), "km \u2265 ",
                       ifelse(max_walk>0, "walking", "cycling"), ifelse(max_walk>0 & max_cycle>0, paste(" ;", max_walk, "km",
         "<", "cycling \u2264", max_cycle, "km"), ""))) %>%
  mutate(scen_order=case_when(title1 == "1km \u2265 walking" ~ 1,
                              title1 ==   "2km \u2265 walking" ~ 2,
                              title1 ==  "2km \u2265 cycling" ~ 3, 
                              title1 ==   "5km \u2265 cycling" ~ 4, 
                              title1 ==   "10km \u2265 cycling" ~ 5,
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 2 km" ~ 6, 
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 5 km" ~ 7,
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 10 km" ~ 8, 
                              title1 ==   "2km \u2265 walking ; 2 km < cycling \u2264 5 km" ~ 9,
                              title1 ==   "2km \u2265 walking ; 2 km < cycling \u2264 10 km" ~ 10))

# Combinations age and sex for graphs and results
age_sex_cohorts <- crossing(data.frame(age=c("15-19", "20-39", "40-64", "65plus", "all")),
                            data.frame(sex=c('male', 'female', 'all'))) %>%
  dplyr::mutate(cohort=paste0(age,"_",sex))


# Load data ---------------------------------------------------------------

finalLocation <- paste0("./output/", city, "-outputs")



output_df_agg_all <- readRDS(paste0(finalLocation,"/output_df_agg.rds")) %>% ## add titles
  left_join(scenariosDF)
output_diseases_change <- readRDS(paste0(finalLocation,"/output_diseases_change.rds")) %>% ## add titles
  left_join(scenariosDF)
output_life_years_change <- readRDS(paste0(finalLocation,"/output_life_years_change.rds")) %>% ## add titles
  left_join(scenariosDF)
PAall<-readRDS(paste0(finalLocation,"/PAall.rds")) %>% ## add titles
left_join(scenariosDF)
PAallGuide<-readRDS(paste0(finalLocation,"/PAallGuide.rds")) %>% ## add titles
left_join(scenariosDF)
output_transport_modes<-readRDS(paste0(finalLocation,"/output_transport_modes.rds")) %>% ## add titles
  left_join(scenariosDF)

# #Outputs location (seems redundant, delete)
graphsLocations <- paste0("./results/", city, "/graphs/")
dir.create(graphsLocations, recursive=TRUE, showWarnings=FALSE)

graphsLocationDiseases <- paste0("./results/", city, "/graphs/disease/")
dir.create(graphsLocationDiseases, recursive=TRUE, showWarnings=FALSE)



tablesLocations <- paste0("./results/", city, "/tables/")
dir.create(tablesLocations, recursive=TRUE, showWarnings=FALSE)

# Function inputs select


age_val<-c('15-19','20-39', '40-64', '65plus', 'all')
sex_val<- c('all', 'female', 'male')
purp_val<- unique(scenariosDF$purpose)
disease_val <- c("ishd","strk",
                 "brsc","carc","tbalc","utrc","mltm", "chml","stmc", "lvrc","hanc",
                 "cancers",
                 "dmt2", 
                 "dprd", 
                 "adaod", "cancers")



## Transport mode share

for (PURP in purp_val){
  for(AGE in age_val){
    for (SEX in sex_val){

 # AGE="all"
 # SEX="all"
 # PURP="all"

### Create output location
      
graphsLocation <- paste0(graphsLocations, AGE, SEX, PURP)
dir.create(graphsLocation, recursive=TRUE, showWarnings=FALSE)

modes <- GraphsMode(
  age_val= AGE,
  sex_val= SEX,
  purpose_val=PURP
)
ggsave(paste0(graphsLocation, "/mode_share", ".png"),width=12,height=18)

    } 
  }
}
  
## PA tables
for (PURP in purp_val){
  for(AGE in age_val){
    for (SEX in sex_val){
       
      # AGE="all"
      # SEX="all"
      # PURP="all"
      
### Create output location
      
tablesLocation <- paste0(tablesLocations, AGE, SEX, PURP)
dir.create(tablesLocation, recursive=TRUE, showWarnings=FALSE)

### PA minutes
minutesT <- minutesTable(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

write.csv(minutesT, paste0(tablesLocation, "/minutesTable.csv"))

### PA minutes guidelines
minutesG <- minutesGuide(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

write.csv(minutesG, paste0(tablesLocation, "/miuntesGTable.csv"))

    }
  }
}

## Diseases graphs

for (PURP in purp_val){
  for(AGE in age_val){
    for (SEX in sex_val){
      
      # AGE="all"
      # SEX="all"
      # PURP="all"
      
### Create output location
graphsLocation <- paste0(graphsLocations, AGE, SEX, PURP)

### Overall percentage change in incidence over life course
diseasesChangeIncidencePerc(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

ggsave(paste0(graphsLocation, "/disease_change_perc.png"),width=12,height=18)

## Overall percentage change in mortaility over life course
diseasesChangeDeathsPerc(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

ggsave(paste0(graphsLocation, "/deaths_change_perc.png"),width=12,height=18)

diseasesChangeIncidenceNumbers(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

ggsave(paste0(graphsLocation, "/disease_change_num.png"),width=12,height=18)

## Overall percentage change in mortality over life course
diseasesChangeDeathsNumbers(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/deaths_change_num.png"),width=12,height=18)

    }
  }
}

## Life years

for (PURP in purp_val){
  for(AGE in age_val){
    for (SEX in sex_val){
      
      # AGE="all"
      # SEX="all"
      # PURP="all"
      
### Create output location
graphsLocation <- paste0(graphsLocations, AGE, SEX, PURP)

### Healh Adjusted life years change per simulation year
halyGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/halys.png"),width=12,height=18)

### Life years change per simulation year

lyGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/lys.png"),width=12,height=18)


      }
    }
  }


### Graphs for diseases incidence and deaths over time

for (PURP in purp_val){
  for(AGE in age_val){
    for (SEX in sex_val){
        
### Create output location
graphsLocation <- paste0(graphsLocations, AGE, SEX, PURP)

### Incidence diseases change per simulation year
incidenceDiseasesGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/incidence.png"),width=12,height=18)

### Mortality diseases change per simulation year
mortalityDiseasesGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/deaths.png"),width=12,height=18)

    }
  }
}

