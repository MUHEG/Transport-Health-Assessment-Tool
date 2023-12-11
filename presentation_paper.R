######################################### SCRIPTS TO PRESENT RESULTS ###############################################
rm (list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)



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
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 2 km" ~ 5, 
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 5 km" ~ 6,
                              title1 ==   "1km \u2265 walking ; 1 km < cycling \u2264 10 km" ~ 8, 
                              title1 ==   "2km \u2265 walking ; 2 km < cycling \u2264 5 km" ~ 7,
                              title1 ==   "2km \u2265 walking ; 2 km < cycling \u2264 10 km" ~ 10))

# Combinations age and sex for graphs and results
age_sex_cohorts <- crossing(data.frame(age=c("15-19", "20-39", "40-64", "65plus", "all")),
                            data.frame(sex=c('male', 'female', 'all'))) %>%
  dplyr::mutate(cohort=paste0(age,"_",sex))


# Load data ---------------------------------------------------------------

finalLocation <- "./output/brisbane-outputs"



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

#Outputs location
graphsLocation <- "./results/paper/graphs"
dir.create(graphsLocation, recursive=TRUE, showWarnings=FALSE)

graphsLocationDisease <- "./results/paper/graphs/disease"
dir.create(graphsLocationDisease, recursive=TRUE, showWarnings=FALSE)


tablesLocation <- "./results/paper/tables"
dir.create(tablesLocation, recursive=TRUE, showWarnings=FALSE)
# Function inputs select


age_val<-c('15-19','20-39', '40-64', '65plus', 'all')
sex_val<- c('all', 'female', 'male')
purp_val<- unique(scenariosDF$purpose)
disease_val <- c("ishd","strk",
                 "brsc","carc","tbalc","utrc","mltm", "chml","stmc", "lvrc","hanc",
                 "cancers",
                 "dmt2", 
                 "dprd", 
                 "adaod")




modes_list <- list()
index <- 1

  for(AGE in age_val){
    for (SEX in sex_val){
  # 
# AGE="all"
# SEX="all"
# PURP="all"
      
## Create output location for each scenario, age and sex combination
      
graphsLocation <- paste0( "./results/paper/graphs/", AGE, SEX)
dir.create(paste0(graphsLocation), recursive=TRUE, showWarnings=FALSE)
      
      
      
tablesLocation <- paste0("./results/paper/tables/", AGE, SEX)
dir.create(tablesLocation, recursive=TRUE, showWarnings=FALSE)
      # Function inputs select


# Transport graph ----------------------------------------------

modes_list[[index]] <- GraphsMode(
  age_val= AGE,
  sex_val= SEX,
  purpose_val=PURP
)
ggsave(paste0(graphsLocation, "/mode_share", ".tiff"),width=12,height=18)
index <- index + 1
  }
}
  
### PA and health outcomes graphs
for (PURP in purp_val){
  for(AGE in age_val){
    for (SEX in sex_val){
      # 
      # AGE="all"
      # SEX="all"
      # PURP="all"
      
      ## Create output location for each scenario, age and sex combination
      
      graphsLocation <- paste0( "./results/paper/graphs/", AGE, SEX)
      dir.create(paste0(graphsLocation), recursive=TRUE, showWarnings=FALSE)
      
      
      
      tablesLocation <- paste0("./results/paper/tables/", AGE, SEX)
      dir.create(tablesLocation, recursive=TRUE, showWarnings=FALSE)
      # Function inputs select
      
      

# PA minutes --------------------------------------------------
minutesT <- minutesTable(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

write.csv(minutesT, paste0(tablesLocation, "/", paste0(PURP, ".miuntesTable.csv")))

# PA minutes guidelines ---------------------------------------
minutesG <- minutesGuide(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)

write.csv(minutesG, paste0(tablesLocation, "/", paste0(PURP, ".miuntesGTable.csv")))


# Diseases graphs ---------------------------------------------
## Overall percentage change in incidence over life course
diseasesChangeIncidencePerc(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "disease_change_perc", PURP, ".png"),width=12,height=18)

## Overall percentage change in mortaility over life course
diseasesChangeDeathsPerc(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "deaths_change_perc", PURP, ".png"),width=12,height=18)

diseasesChangeIncidenceNumbers(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "disease_change_num", PURP, ".png"),width=12,height=18)

## Overall percentage change in mortality over life course
diseasesChangeDeathsNumbers(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "deaths_change_num", PURP, ".png"),width=12,height=18)


# Diseases table -------------------------------------------------------------

diseases <- diseasesTable(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
write.csv(diseases,paste0(tablesLocation, "/", paste0("disease", PURP,".csv")))
# Life years -----------------------------------------------------------------
## Healh Adjusted life years change per simulation year
halyGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "halys", PURP, ".png"),width=12,height=18)

## Healh Adjusted life years change per simulation year
lyGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
ggsave(paste0(graphsLocation, "/", "lys", PURP, ".png"),width=12,height=18)
# HALYs table ---------------------------------------------------------------

HALYsLYs <- HALYsTable(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP
)
write.csv(HALYsLYs,paste0(tablesLocation, "/", paste0("HALYsLYs", PURP,".csv")))
index <- index + 1

      }
    }
  }


# Graphs for diseases incidence and deaths over time

index <- 1

for (PURP in purp_val){
  for(AGE in age_val){
    for (SEX in sex_val){
      for (DISEASE in disease_val) {

### Incidence diseases change per simulation year
incidenceDiseasesGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP,
  disease_val= DISEASE
)
ggsave(paste0(graphsLocationDisease, "/", "incidence", AGE, "_", SEX, "_", PURP, "_", DISEASE, ".png"),width=12,height=18)

### Mortality diseases change per simulation year
mortalityDiseasesGraph(
  age_val= AGE,
  sex_val= SEX,
  purpose_val= PURP,
  disease_val= DISEASE
)
ggsave(paste0(graphsLocationDisease, "/", "deaths", AGE, "_", SEX, "_", PURP, "_", DISEASE, ".png"),width=12,height=18)
 
index <- index + 1      
      }
    }
  }
}

# ---- Analysis by age and sex groups ----

# HALYs and life years (only selected scenario to show differene in gains per 100,000 as this is comparable)

### Data

  dataFiltered <- output_life_years_change %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    mutate(across(mean:percentile975, round, digits=4)) %>%
    mutate(per_100000 = median*100000/population) %>%
    dplyr::select(population,measure,mean,median,percentile025,percentile975, scen, title1, per_100000) %>%
    filter(!str_detect(measure, "baseline|difference")) %>%
    filter(scen %in% c("all_2_5", "commuting_2_5")) %>%
    mutate(scen_name=ifelse(scen=="all_2_5", "All trips 2km \u2265 walking ; 2 km < cycling \u2264 5 km",
                            "Commute trips 2km \u2265 walking ; 2 km < cycling \u2264 5 km")) %>%
    mutate(sex=ifelse(sex=="male", "males", sex), 
         sex=ifelse(sex=="female", "females", sex)) %>%
  mutate(age_sex_cohorts=paste0(sex, " ", age)) %>%
  filter(sex %in% c("females", "males")) %>%
  filter(age != "all")

### Graph
ggplot(dataFiltered, aes(x=per_100000, y=measure, fill=scen_name)) +
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_text(aes(label=scales::comma(round(per_100000))),
            position=position_dodge(width=0.9),
            hjust=0.60, vjust=0.5, size=3) +
  labs(x="HALYs and life years gained per 100,000 population") +
  scale_x_continuous(labels = scales::comma, 
                     breaks = waiver(),
                     minor_breaks = NULL,
                     n.breaks = 10) +
  theme_classic() +
  facet_wrap(~age_sex_cohorts,  nrow = 5, ncol = 2) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        axis.title.y=element_blank(),
        legend.position = "bottom",
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle=90),
        legend.key = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(size=10))


ggsave(paste0(graphsLocation, "/", "halys_ly_per_100000",".png"),width=8,height=11)

# Diseases (only selected scenario to show differene in gains per 100,000 as this is comparable)
diseaseLevels <- c("ishd","strk",
                   "brsc","carc","tbalc","utrc","mltm", "chml","stmc", "lvrc","hanc",
                   "dmt2", 
                   "dprd", 
                   "adaod")
diseaseLabels <- c("Ischemic heart disease","Stroke",
                   "Breast cancer","Colon cancer","Lung cancer","Uterine cancer","Multiple myeloma", 
                   "Chronic myeloid leukemia", "Stomach cancer", "Liver cancer","Head and neck cancer",
                   "Diabetes type 2",
                   "Depression", 
                   "Alzheimers disease and other dementias")
### Data

dataFiltered <- output_diseases_change %>%
  mutate(purpose = case_when(grepl("all", scen) ~ "all",
                             grepl("commuting", scen) ~ "commuting")) %>%
  mutate(across(mean:percentile975, round, digits=4)) %>%
  filter(disease %in% c("ishd","strk",  "dmt2","adaod" )) %>%
 mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
  mutate(per_100000 = -1*median*100000/population) %>%
  dplyr::select(population,measure,disease,mean,median,percentile025,percentile975, scen, title1, per_100000) %>%
  filter(str_detect(measure, "mx.num|inc.num")) %>%
  filter(scen %in% c("all_2_5", "commuting_2_5")) %>%
  mutate(scen_name=ifelse(scen=="all_2_5", "All trips 2km \u2265 walking ; 2 km < cycling \u2264 5 km",
                          "Commute trips 2km \u2265 walking ; 2 km < cycling \u2264 5 km")) %>%
  mutate(age=ifelse(age=="all", "all age groups", age),
         sex=ifelse(sex=="all", "females and males", sex), 
         sex=ifelse(sex=="male", "males", sex), 
         sex=ifelse(sex=="female", "females", sex)) %>%
  mutate(age_sex_cohorts=paste0(sex, " ", age)) %>%
  filter(sex %in% c("females", "males"))%>%
  filter(sex %in% c("females", "males")) %>%
  filter(age != "all")

### Graph diseases for the most the highest results
diseases <- dataFiltered %>% filter(measure == "inc.num")
ggplot(diseases, aes(x=per_100000, y=disease, fill=scen_name)) +
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_text(aes(label=scales::comma(round(per_100000))),
            position=position_dodge(width=0.9),
            hjust=0.60, vjust=0.5, size=3) +
  labs(x="Diseases prevented per 100,000 population") +
  scale_x_continuous(labels = scales::comma, 
                     breaks = waiver(),
                     minor_breaks = NULL,
                     n.breaks = 10) +
  theme_classic() +
  facet_wrap(~age_sex_cohorts,  nrow = 5, ncol = 2) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        axis.title.y=element_blank(),
        legend.position = "bottom",
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle=90),
        legend.key = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(size=10))


ggsave(paste0(graphsLocation, "/", "diseases_per_100000",".png"),width=8,height=13)

### Graph deaths

### Graph diseases for the most the highest results
deaths <- dataFiltered %>% filter(measure == "mx.num") %>%
            filter(disease != "Diabetes type 2")
ggplot(diseases, aes(x=per_100000, y=disease, fill=scen_name)) +
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_text(aes(label=scales::comma(round(per_100000))),
            position=position_dodge(width=0.9),
            hjust=0.60, vjust=0.5, size=3) +
  labs(x="Deaths prevented per 100,000 population") +
  scale_x_continuous(labels = scales::comma, 
                     breaks = waiver(),
                     minor_breaks = NULL,
                     n.breaks = 10) +
  theme_classic() +
  facet_wrap(~age_sex_cohorts,  nrow = 5, ncol = 2) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
  theme(legend.position = "none") +
  theme(plot.title=element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        axis.title.y=element_blank(),
        legend.position = "bottom",
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle=90),
        legend.key = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(size=10))


ggsave(paste0(graphsLocation, "/", "deaths_per_100000",".png"),width=8,height=13)

### Compare rates of diseases

### Incidence

# Read data

mslt_general="Data/processed/mslt/mslt_df.csv"

mslt_data <- read_csv(mslt_general) %>%
  select(sex, age, starts_with("incidence"))

incidenceLevels <- c("incidence_ishd","incidence_strk",
                     "incidence_dmt2",
                     "incidence_adaod")
incidenceLabels <- c("Ischemic heart disease","Stroke",
                     "Diabetes type 2",
                     "Alzheimers disease and other dementias")

### Plot incidence rates by sex and age groups
mslt <- mslt_data %>% 
  select(c("age", "sex", "incidence_adaod", "incidence_dmt2", "incidence_ishd", "incidence_strk"))

# To long to plot all disease incidence rates

mslt_inc_long <- mslt %>%
  pivot_longer(incidence_adaod:incidence_strk, names_to = "variable", values_to = "value") %>%
  mutate(variable=factor(variable, levels=incidenceLevels, labels=incidenceLabels))  # convert to long format

# Plot

ggplot(data=mslt_inc_long,
       aes(x=age, y=value, colour=variable)) +
  geom_line() +
  facet_wrap(~sex) +
  theme_classic() +
  labs(x="Age", y="Incidence rate per one") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

ggsave(paste0(graphsLocation, "/", "inc_selected",".png"),width=8,height=13)
  

### Deaths

# Read data

mslt_general="Data/processed/mslt/mslt_df.csv"

mslt_data <- read_csv(mslt_general) %>%
  select(sex, age, starts_with("death"))

deathsLevels <- c("deaths_rate_ishd","deaths_rate_strk",
                     "deaths_rate_adaod")
deathsLabels <- c("Ischemic heart disease","Stroke",
                     "Alzheimers disease and other dementias")

### Plot deaths rates by sex and age groups
mslt <- mslt_data %>% 
  select(c("age", "sex", "deaths_rate_adaod",
           "deaths_rate_ishd", "deaths_rate_strk"))

# To long to plot all disease incidence rates

mslt_deaths_long <- mslt %>%
  pivot_longer(deaths_rate_adaod:deaths_rate_strk, names_to = "variable", values_to = "value") %>%
  mutate(variable=factor(variable, levels=deathsLevels, labels=deathsLabels))  # convert to long format

# Plot

ggplot(data=mslt_deaths_long,
       aes(x=age, y=value, colour=variable)) +
  geom_line() +
  facet_wrap(~sex) +
  theme_classic() +
  labs(x="Age", y="Deaths rate per one") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

ggsave(paste0(graphsLocation, "/", "deaths_selected",".png"),width=8,height=13)


