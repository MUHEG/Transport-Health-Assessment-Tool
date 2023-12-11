######################################### FUNCTIONS TO PRESENT MODEL OUTPUTS ############################################

library(ggplot2)
library(dplyr) # for manipulating data
library(tidyr) # for pivoting data
library(scales) # for reordering factor for graphs
library(zoo) # for calculating rolling mean
library(ggpubr)
library(forcats)


# Graphs general inputs
diseaseLevels <- c("brsc","carc","dmt2","ishd","strk","tbalc","utrc", "dprd", "mltm", "chml",
                   "stmc", "lvrc", "prkd", "adaod", "hanc", "cancers")
diseaseLabels <- c("Breast cancer","Colon cancer","Diabetes type 2",
                   "Ischemic heart disease","Stroke","Lung cancer",
                   "Uterine cancer", "Depression", "Multiple myeloma", "Chronic myeloid leukemia",
                   "Stomach cancer", "Liver cancer", "Parkinson's disease", 
                   "Alzheimers disease and other dementias", "Head and neck cancer", "Cancers")

scen.lab <- c("1km \u2265 walking","2km \u2265 walking","2km \u2265 cycling", "5km \u2265 cycling","10km \u2265 cycling",
              "1km \u2265 walking ; 1 km < cycling \u2264 2 km","1km \u2265 walking ; 1 km < cycling \u2264 5 km",
              "1km \u2265 walking ; 1 km < cycling \u2264 10 km", "2km \u2265 walking ; 2 km < cycling \u2264 5 km",
              "2km \u2265 walking ; 2 km < cycling \u2264 10 km")
names(scen.lab) <- c(1,2,3,4,5,6,7,8,9,10)

auo_theme <- theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        legend.key = element_blank(),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle=0, hjust=0),
        strip.background = element_rect(fill="white",size=0),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.8, "lines")) # use this to change the spacing between the different disease plots

# ----- Change in mode of transport -----
GraphsMode <- function(age_val,sex_val,purpose_val) {
   # age_val='all'; sex_val='all'; purpose_val='commuting'


  dataFiltered <- output_transport_modes %>% 
    # dplyr::filter(age==age_val,sex==sex_val, purpose==purpose_val) %>%
    mutate(scenario=factor(scenario, levels=c("bl","sc"), labels=c("Base case", "Scenario")))%>%
    mutate(mode=factor(mode,
                       levels=c("walking","bicycle","public.transport","car","other"),
                       labels=c("Walking","Cycling","Public transport","Driving","Other"))) %>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex)) %>% 
    mutate(scen=as.factor(scen)) %>%
    arrange(scen_order)
                                      
  
 ggplot(dataFiltered, aes(x=prop, y=mode, fill=scenario)) +
    geom_bar(stat="identity", position="dodge") + 
    # AUO teal and pink
    scale_fill_manual(values=c("#24C9AC","#EC4497")) +
    labs(x="Proportion of all trips") +
    geom_text(aes(label=paste0(round(prop*100,1),"%")),
              position=position_dodge(width=0.9),
              hjust=-0.05, vjust=0.5, size=4) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 5L),limits=c(0,.8)) +
    theme_classic() +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    ggtitle(paste0(ifelse(dataFiltered$purpose=='all', "All", "Commuting"), " trips for ", unique(dataFiltered$age), " and ", unique(dataFiltered$sex))) +
    theme(plot.title=element_text(size=16, hjust = 0.5),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=14),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=14))

    }


# ----- Change in minutes walking ----
minutesTable <- function(age_val,sex_val,purpose_val) {
  # age_val='65plus'; sex_val='all'; purpose_val='commuting'

  
  dataFiltered <- PAall %>% 
    dplyr::filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                                      grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    dplyr::select(age, sex, walk_base, walk_scen, cycle_base, cycle_scen, scen, purpose)
  }
     
# ----- Meets guidelines minutes -----

minutesGuide <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'

  dataFiltered <- PAallGuide %>% 
    mutate_if(is.numeric, round, digits = 4) %>%
    dplyr::filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    mutate(meets_base=paste0(meets_base*100, "%"), 
           meets_scen=paste0(meets_scen*100, "%"))
}

# ---- Disease tables -----
diseasesTable <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'
  
  dataFiltered <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    mutate(measure=case_when(measure=='inc.num' ~ 'Incidence',
                             measure=='mx.num'  ~ 'Deaths')) %>%
    filter(measure%in% c("Incidence", "Deaths")) %>%
    mutate(across(mean:percentile975, round, digits=1)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    dplyr::select(population,measure,disease,mean,median,percentile025,percentile975, scen, title1)
}

# ---- Disease tables -----
HALYsTable <- function(age_val,sex_val,purpose_val) {
  # age_val='65plus'; sex_val='all'; purpose_val='commuting'
  dataFiltered <- output_life_years_change %>%
    filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    mutate(across(mean:percentile975, round, digits=4)) %>%
    dplyr::select(population,measure,mean,median,percentile025,percentile975, title1)
}

# ---- Disease change incidence table -----
diseasesChangeIncidenceTable <- function(age_val,sex_val,purp_val) {
  # age_val='65plus'; sex_val='all'; purpose_val='commuting'
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    filter(measure=="inc.percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    mutate(across(median:percentile975, round, digits=3)) %>%
    dplyr::select(disease,median,percentile025,percentile975, title1, age, sex, purpose)
}

# ---- Disease change incidence graph -----
diseasesChangeIncidencePerc <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'

  #### ADD decimals
  
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    filter(measure=="inc.percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    mutate(across(median:percentile975, round, digits=3)) %>%
    dplyr::select(disease,median,percentile025,percentile975, title1, age, sex, purpose, max_walk, max_cycle, scen_order, population)%>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex)) %>%
          arrange(scen_order)
  
    
  ggplot(tmpPlot, aes(x=median, y=disease)) +
    geom_bar(stat="identity", color=NA, fill="#EC4497", # AUO pink
             position=position_dodge()) +
    geom_errorbar(aes(xmin=percentile025, xmax=percentile975), width=.2) +
    scale_x_continuous(labels = scales::percent) +
    auo_theme +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    theme(legend.position = "none") +
   labs(x="Percentage change incidence", 
         title = paste0(ifelse(tmpPlot$purpose=='all', "All", "Commuting"), " trips for ", unique(tmpPlot$age), " and ", unique(tmpPlot$sex)), 
         subtitle = paste("(population=", unique(tmpPlot$population), ")"))+
    theme(plot.title=element_text(size=16, hjust = 0.5),
          plot.subtitle = element_text(size=14, hjust = 0.5),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=12))
  
}

diseasesChangeIncidenceNumbers <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'
  
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    filter(measure=="inc.num" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    mutate(across(median:percentile975, round, digits=3)) %>%
    dplyr::select(disease,median,percentile025,percentile975, title1, age, sex, purpose, max_walk, max_cycle, scen_order)%>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex)) %>%
    arrange(scen_order)
  
  
  ggplot(tmpPlot, aes(x=median, y=disease)) +
    geom_bar(stat="identity", color=NA, fill="#EC4497", # AUO pink
             position=position_dodge()) +
    labs(x="Change incidence") +
    geom_errorbar(aes(xmin=percentile025, xmax=percentile975), width=.2) +
    scale_x_continuous(labels = scales::comma, 
                       breaks = waiver(),
                       minor_breaks = NULL,
                       n.breaks = 10) +
    auo_theme +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    theme(legend.position = "none") +
    ggtitle(paste0(ifelse(tmpPlot$purpose=='all', "All", "Commuting"), " trips for ", unique(tmpPlot$age), " and ", unique(tmpPlot$sex))) +
    theme(plot.title=element_text(size=16, hjust = 0.5),
          plot.subtitle = element_text(size=14, hjust = 0.5),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=12))
}
# ---- Disease change deaths graph -----
diseasesChangeDeathsPerc <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'

  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    filter(measure=="mx.percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    mutate(across(median:percentile975, round, digits=3)) %>%
    dplyr::select(disease,median,percentile025,percentile975, title1, age, sex, purpose, max_walk, max_cycle, scen_order, population)%>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex)) %>%
    arrange(scen_order)
  
  
  ggplot(tmpPlot, aes(x=median, y=disease)) +
    geom_bar(stat="identity", color=NA, fill="#EC4497", # AUO pink
             position=position_dodge()) +
    geom_errorbar(aes(xmin=percentile025, xmax=percentile975), width=.2) +
    scale_x_continuous(labels = scales::percent) +
    auo_theme +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    theme(legend.position = "none") +
    labs(x="Percentage change deaths", 
         title = paste0(ifelse(tmpPlot$purpose=='all', "All", "Commuting"), " trips for ", unique(tmpPlot$age), " and ", unique(tmpPlot$sex)), 
         subtitle = paste("(population=", unique(tmpPlot$population), ")"))+
    theme(plot.title=element_text(size=16, hjust = 0.5),
          plot.subtitle = element_text(size=14, hjust = 0.5),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=12))
  
}

diseasesChangeDeathsNumbers <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'
  
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    filter(measure=="mx.num" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    mutate(across(median:percentile975, round, digits=3)) %>%
    dplyr::select(disease,median,percentile025,percentile975, title1, age, sex, purpose, max_walk, max_cycle, scen_order, population)%>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex)) %>%
    arrange(scen_order)
  
  
  ggplot(tmpPlot, aes(x=median, y=disease)) +
    geom_bar(stat="identity", color=NA, fill="#EC4497", # AUO pink
             position=position_dodge()) +
    labs(x="Prevented deaths") +
    geom_errorbar(aes(xmin=percentile025, xmax=percentile975), width=.2) +
    scale_x_continuous(labels = scales::comma, 
                       breaks = waiver(),
                       minor_breaks = NULL,
                       n.breaks = 10) +
    auo_theme +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    theme(legend.position = "none") +
    labs(x="Deaths prevented",  
         title = paste0(ifelse(tmpPlot$purpose=='all', "All", "Commuting"), " trips for ", unique(tmpPlot$age), " and ", unique(tmpPlot$sex)), 
          subtitle = paste("(population=", unique(tmpPlot$population), ")"))+
    theme(plot.title=element_text(size=16, hjust = 0.5),
          plot.subtitle = element_text(size=14, hjust = 0.5),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=12))
}

# ---- Disease change incidence over time graph -----

### Change colors, darker and thicker

incidenceDiseasesGraph <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'
  
 
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val) %>% #, disease==disease_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    filter(measure=="inc.num" & scenario== "diff") %>%
    dplyr::select(year,disease,median,percentile025,percentile975, title1, age, sex, purpose, scen_order) %>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex)) %>%
    arrange(disease,year) %>%
    group_by(disease) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    ungroup() %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    arrange(scen_order)
  
  ggplot(tmpPlot, aes(x=year, colour = disease)) +
    # geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal, removed UI to graph all diseases in one graph
    geom_smooth(aes(y=median)) +
    # facet_grid(rows=vars(disease), scales = "free_y") +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 5,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Incidence", 
         title = paste0(ifelse(tmpPlot$purpose=='all', "All", "Commuting"), " trips for ", unique(tmpPlot$age), " and ", unique(tmpPlot$sex)), 
         subtitle = paste0("Change in incidence over time")) +
    auo_theme +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    theme(legend.position = "none") +
    theme(plot.title=element_text(size=16, hjust = 0.5),
          plot.subtitle = element_text(size=14, hjust = 0.5),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=12))
}

# ---- Disease change mortality over time graph -----
mortalityDiseasesGraph <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'

  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val) %>% #, disease==disease_val) %>%
    mutate(purpose = case_when(grepl("all", scen) ~ "all",
                               grepl("commuting", scen) ~ "commuting")) %>%
    dplyr::filter(purpose==purpose_val) %>%
    filter(measure=="mx.num" & scenario== "diff") %>%
    dplyr::select(year,disease,median,percentile025,percentile975, title1, age, sex, purpose, scen_order) %>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex)) %>%
    arrange(disease,year) %>%
    group_by(disease) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    ungroup() %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    arrange(scen_order)
  
  ggplot(tmpPlot, aes(x=year, colour = disease)) +
    # geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal, removed UI to graph all diseases in one graph
    geom_smooth(aes(y=median)) +
    # facet_grid(rows=vars(disease), scales = "free_y") +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 5,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0))+
    labs(x="Years since scenario commenced", y="Incidence", 
         title = paste0(ifelse(tmpPlot$purpose=='all', "All", "Commuting"), " trips for ", unique(tmpPlot$age), " and ", unique(tmpPlot$sex)), 
         subtitle = paste0("Change in incidence over time"))+
    auo_theme +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    theme(legend.position = "none") +
    theme(plot.title=element_text(size=16, hjust = 0.5),
          plot.subtitle = element_text(size=14, hjust = 0.5),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=12))
}


# ---- Life years change over time graph -----
halyGraph <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,purpose==purpose_val) %>%
    filter(measure=="Lwx" & scenario== "diff") %>%
    dplyr::select(year,median,percentile025,percentile975, title1, age, sex, purpose, scen_order) %>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex))  %>%
    arrange(year) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    # year 84 (the last year, has a weird uptick, removing for now)
    filter(year<=83) %>% 
    arrange(scen_order)
  
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 5,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Health-adjusted life years") +
    # subtitle =  paste0(unique(tmpPlot$population)) +
    auo_theme +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    theme(legend.position = "none") +
    labs(x="Years since scenario commenced", y="Incidence", 
         title = paste0(ifelse(tmpPlot$purpose=='all', "All", "Commuting"), " trips for ", unique(tmpPlot$age), " and ", unique(tmpPlot$sex)), 
         subtitle = paste0("Change in Health-adjusted life years"))+
    theme(plot.title=element_text(size=16, hjust = 0.5),
          plot.subtitle = element_text(size=14, hjust = 0.5),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=12))
}


lyGraph <- function(age_val,sex_val,purpose_val) {
  # age_val='all'; sex_val='all'; purpose_val='all'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,purpose==purpose_val) %>%
    filter(measure=="Lx" & scenario== "diff") %>%
    dplyr::select(year,median,percentile025,percentile975, title1, age, sex, purpose, scen_order) %>%
    mutate(age=ifelse(age=="all", "all age groups", age),
           sex=ifelse(sex=="all", "females and males", sex), 
           sex=ifelse(sex=="male", "males", sex), 
           sex=ifelse(sex=="female", "females", sex))  %>%
    arrange(year) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    # year 84 (the last year, has a weird uptick, removing for now)
    filter(year<=83) %>% 
    arrange(scen_order)
  
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 5,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Life years") +
    # subtitle =  paste0(unique(tmpPlot$population)) +
    auo_theme +
    facet_wrap(~ scen_order , nrow = 5, ncol = 2, labeller= labeller(scen_order= scen.lab)) + 
    theme(legend.position = "none") +
    labs(x="Years since scenario commenced", y="Life years", 
         title = paste0(ifelse(tmpPlot$purpose=='all', "All", "Commuting"), " trips for ", unique(tmpPlot$age), " and ", unique(tmpPlot$sex)), 
         subtitle = paste0("Change in Life years"))+
    theme(plot.title=element_text(size=16, hjust = 0.5),
          plot.subtitle = element_text(size=14, hjust = 0.5),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.title.y=element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=12))
}