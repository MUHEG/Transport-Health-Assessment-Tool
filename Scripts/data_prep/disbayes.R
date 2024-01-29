#install.packages("disbayes")

# From Chris J https://github.com/chjackson/disbayes/blob/master/metahit/gbd_process_2019.Rmd
## First load required packages and specify local file paths 

library(dplyr)
library(tidyverse)
library(progress)
library(disbayes)
library(tempdisagg)
library(parallel)
library(tidyr)
library(readxl)
setwd(getwd())


# Steps: 1) Prepare GBD data, 2) Run Disbayes

# 1. Prepare GBD data ----
# -----------------------------------------------------------------------------#
## 1.1 Read in and combine data ----
## -------------------------------------#
gbd <- rbind(read.csv("./Data/original/gbd/gbd_2019_deaths.csv"),
             read.csv("./Data/original/gbd/gbd_2019_incidence.csv"),
             read.csv("./Data/original/gbd/gbd_2019_prevalence.csv"),
             read.csv("./Data/original/gbd/gbd_2019_YLDs.csv"))

names(gbd) <- gsub(pattern = "_name", replacement = "", x = names(gbd))
gbd_temp <- gbd %>%
  select(-contains("id")) %>%
  mutate(cause = tolower(cause)) %>% filter(location=="Australia") %>%
  filter(!str_detect(cause, "injuries")) %>%
  filter(!cause %in% "Lower respiratory infections") %>% 
  pivot_wider(names_from="metric", values_from=c("val", "upper", "lower")) %>%
  mutate(pop = val_Number*100000/val_Rate) %>%
  pivot_longer(cols = val_Number:lower_Rate,
               names_to = "metric",
               values_to = "val") %>%
  separate(metric, c("metric2", "metric")) %>%
  pivot_wider(names_from = "metric2", values_from = "val")


## 1.2 Determining "effective sample sizes" behind estimates ----
## -------------------------------------#
gbdp <- 
  gbd_temp %>%
  dplyr::filter(metric == "Rate") %>% 
  dplyr::filter(!cause %in% c("all causes", "road injuries", "pedestrian road injuries", "cyclist road     injuries", "motorcyclist road injuries", "motor vechicle road injuries", "other road injuries", "motor vehicle road injuries", "hypertensive heart disease")) %>%
  dplyr::mutate(val = val/100000, lower = lower/100000, upper = upper/100000) %>%
  dplyr::mutate(lower = if_else(lower<=0, pmin(val/2, 0.00001), lower)) %>% 
  dplyr::mutate(upper = if_else(upper>=1, pmax((1+val)/2, 0.99999), upper))

# The function `ci2num` in the `disbayes` package is then used to calculate the 
#effective sample sizes.  This is computationally intensive. 
library(foreach)
library(doParallel)
registerDoParallel(3) 
nest <- nrow(gbdp)
numdenom <- foreach (i=1:nest, .combine=rbind) %dopar% {
  if (gbdp$val[i] < gbdp$upper[i] &
      gbdp$val[i] > gbdp$lower[i]) { 
    counts <- disbayes:::ci2num(gbdp$val[i], gbdp$lower[i], gbdp$upper[i], denom0=gbdp$pop[i])
    c(rowid = i, num=counts$num, denom=counts$denom)
  }
}
numdenom <- as.data.frame(numdenom)
rownames(numdenom) <- NULL

gbdp <- gbdp %>% 
  mutate(rowid = row_number()) %>%
  left_join(numdenom, by="rowid") %>% 
  mutate(rowid= NULL)

# The estimates are very close to the implicit numerator divided by the implicit 
# denominator in all cases - so those implicit counts can be used in place of the 
# point estimates. 

summary(gbdp$num/gbdp$denom - gbdp$val, na.rm=TRUE)


# The remaining counts still to be filled in are those where the point estimate 
# is exactly 0 or 1, which is incompatible with a beta distribution.

# There are also many estimates for which the implicit denominator is implausibly large.  
# These correspond to disease events which are very rare in particular subgroups, 
# thus the estimates of the effective sample size are unstable.


gbdp %>% dplyr::select(val, num, denom) %>% dplyr::arrange(desc(denom)) %>% head


# If the point estimate is 0 or 1, or if the denominator obtained from 
# `ci2num` is larger than the actual population size, 
# we will simply use the actual population size of the subgroup as the denominator, 
# which will be closer to the amount of information contributing the estimate.


## 1.3 Determining actual population sizes ----
## -------------------------------------#
# Therefore we reconstruct the actual population sizes of each subgroup, 
# assumed to be equal to the estimates published by GBD as the estimated 
# "Number" of cases, divided by the estimated "rates" per person.

gbdnum <- gbd_temp %>%
  dplyr::filter(metric=="Number") %>%
  dplyr::select(measure, location, sex, age, cause, Number=val)

gbdp <- gbdp %>%
  left_join(gbdnum, by=c("measure","location","sex","age","cause")) %>%
  dplyr::mutate(pop = Number / val)

# We can then use these to fill in the effective sample sizes "d"  that were missing 
# or implausible, and deduce the effective numerators "n" by multipling "d" by 
# the point estimate of the proportion.

# Use an arbitrary plausible number (5000) where the population size was indeterminate


gbdp <- gbdp %>%
  mutate(pop = if_else(is.na(pop), 5000, pop)) %>% 
  dplyr::mutate(nodenom = is.na(denom) | (denom > pop),
                denom = if_else(is.na(denom), pop, denom),
                denom = if_else(denom > pop, pop, denom),
                num = ifelse(nodenom, round(val*denom), num))
summary(gbdp$num/gbdp$denom - gbdp$val, na.rm=TRUE)


## 1.4 Disaggregating by age groups ----
## -------------------------------------#
# Working with the data that has one row per five-year age group, 
# first construct 1-year counts as an extra column. 
gbdp <- gbdp %>%
  # some ages have 'years' (eg 5-9 years), while others don't (eg 80-84); omit 'years'
  mutate(age = gsub(" years", "", age)) %>%
  tidyr::extract(age, c("from_age", "to_age"), "(.+)-(.+)", remove=FALSE, convert=TRUE) %>%
  mutate(from_age = case_when(age=="95+"  ~  95L,
                              age=="<5"  ~  0L,
                              TRUE  ~  from_age),
         to_age = case_when(age=="95+"  ~  99L,
                            age=="<5"  ~  4L,
                            TRUE  ~  to_age),
         agediff = to_age - from_age + 1,  # this will equal 5 
         num1yr = round(num/agediff),
         denom1yr = round(denom/agediff)) %>%
  rename(agegroup = age)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(gbdp), gbdp$agediff)
gbdpyrd5 <- gbdp[index,] %>%
  mutate(ageyr = from_age + sequence(gbdp$agediff) - 1)
gbdpyrd5 <- gbdpyrd5 %>% 
  select(measure, location, ageyr, sex, agegroup, from_age, to_age, cause, year, num1yr, denom1yr) 

## More advanced method for smooth disaggregation 
## using the tempdisagg package 
# tmp <- gbdp %>% group_by(measure, location, sex, cause) %>% filter(cur_group_id()==1)
gbdp_grp <- gbdp %>% 
  group_by(measure, location, sex, cause) %>%  # filter(cur_group_id() %in% 1:3) %>%
  arrange(measure, location, sex, cause, from_age)
disagg <- function(dat, key){ 
  res <- with(dat, { 
    data.frame( 
      ageyr = rep(from_age,agediff) + sequence(agediff) - 1,
      num = predict(td(num ~ 1, to=5, method="fast")),
      denom = predict(td(denom ~ 1, to=5, method="fast"))
    ) } )
  res 
}
# This takes about a minute 
library(tempdisagg)
gbdpyr <- group_modify(gbdp_grp, disagg) %>% 
  ungroup %>%
  left_join(gbdpyrd5, by=c("measure","location","ageyr","sex","cause"))

# Sometimes the results are negative.  
# Revert to dividing by 5 for all agegroups that contain a negative result 
neg_num <- gbdpyr %>%
  filter(num<0) %>%
  select("measure","location","sex","cause","agegroup") %>%
  distinct() %>%
  mutate(zeronum = TRUE)
neg_denom <- gbdpyr %>%
  filter(denom<0) %>%
  select("measure","location","sex","cause","agegroup") %>%
  distinct() %>%
  mutate(zerodenom = TRUE)
gbdpyr <- gbdpyr %>% 
  left_join(neg_num, by=c("measure", "location","sex","cause","agegroup")) %>%
  left_join(neg_denom, by=c("measure", "location","sex","cause","agegroup")) %>%
  mutate(zeronum = replace_na(zeronum, FALSE)) %>%
  mutate(zerodenom = replace_na(zerodenom, FALSE)) %>%
  mutate(num = if_else(zeronum, num1yr, round(num)),
         denom = if_else(zerodenom, denom1yr, round(denom)))

gbddb <- gbdpyr %>%
  filter(measure %in% c("Deaths","Incidence", "Prevalence")) %>%
  select(measure, sex, cause, ageyr, num, denom) %>%
  mutate(measure = fct_recode(measure, mort="Deaths", inc="Incidence", prev="Prevalence")) %>%
  pivot_wider(names_from=measure,
              values_from=c("num","denom"),
              values_fill = 0) %>% 
  mutate(disease = str_to_sentence(cause)) %>%
  rename(age = ageyr,
         gender = sex,
         inc_num = num_inc, inc_denom = denom_inc,
         prev_num = num_prev, prev_denom = denom_prev,
         mort_num = num_mort, mort_denom = denom_mort
  ) %>% 
  select(age, gender, disease, inc_num, inc_denom, prev_num, prev_denom, mort_num, mort_denom) %>%
  arrange(disease, gender, age)


## 1.5 Merge the head and neck cancers ----
## -------------------------------------#
# see https://github.com/chjackson/disbayes/blob/master/metahit/gbd_process_2019.Rmd , line 266

hncancers <- c("Larynx cancer", "Lip and oral cavity cancer", "Nasopharynx cancer", "Other pharynx cancer")

gbd_hanc <- gbddb %>% 
  filter(disease %in% hncancers) %>% 
  group_by(age, gender) %>%
  summarise(mort_num=sum(mort_num), inc_num=sum(inc_num), 
            prev_num=sum(prev_num), 
            mort_denom=sum(mort_denom), inc_denom=sum(inc_denom), 
            prev_denom=sum(prev_denom),
            .groups = "drop") %>%
  mutate(disease = "Head and neck cancer")

gbddb <- gbddb %>%
  filter(!(disease %in% hncancers)) %>% 
  full_join(gbd_hanc)


## 1.6 Extract colon from colorectal cancer ----
## -------------------------------------#
# Revise numerators for colon cancer, using AIHW incidence and mortality to 
# determine percentage of colon-and-rectum cancer which is colon cancer
# (and, as there are no equivalent AIHW prevalence figures, omit prevalence)

# Read in AIHW incidence and mortality data
aihw_colon <- rbind(
  # load incidence and mortality
  readxl::read_xlsx("Data/original/aihw/CDiA-2022-Book-1a-Cancer-incidence-age-standardised-rates-5-year-age-groups.xlsx",
                    sheet = "Table S1a.1") %>%
    # column names from row 4; omit non-data first column and rows at beginning and end; add 'measure'
    setNames(., .[4, ]) %>%
    .[5:(nrow(.)-19), ] %>%
    mutate(measure = "inc"),
  readxl::read_xlsx("Data/original/aihw/CDiA-2022-Book-1a-Cancer-incidence-age-standardised-rates-5-year-age-groups.xlsx",
                    sheet = "Table S1a.1") %>%
    # column names from row 4; omit non-data first column and rows at beginning and end; add 'measure'
    setNames(., .[4, ]) %>%
    .[5:(nrow(.)-16), ] %>%
    mutate(measure = "mort")) %>%
  
  #  filter to relevant cancers and 2019 only 
  # (note that for incidence, 2019 figures are projections (actuals up to 2018 only))
  filter(`Cancer group/site` %in% c("Colon cancer", "Rectal cancer", "Colorectal cancer"),
         Year == "2019", `Age group (years)`!= "All ages combined", Sex != "Persons") %>%
  
  # select relevant columns and pivot wider
  dplyr::select(disease = `Cancer group/site`, sex = Sex, age = `Age group (years)`, 
                count = Count, measure) %>%
  pivot_wider(names_from = disease, values_from = count) %>%
  
  # calculate colon.pct
  mutate(colon.pct = as.numeric(`Colon cancer`) / as.numeric(`Colorectal cancer`) * 100) %>%
  
  # extract from-age and to-age
  tidyr::extract(age, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE, convert=TRUE) %>%
  mutate(from_age = case_when(age=="90+"  ~  90L,
                              TRUE  ~  from_age),
         to_age = case_when(age=="90+"  ~  99L,
                            TRUE  ~  to_age),
         agediff = to_age - from_age + 1) %>%
  rename(agegroup = age) 

# using index, make data frame with 1 row per year of age 
index <- rep(1:nrow(aihw_colon), aihw_colon$agediff)
aihw_colon_yr <- aihw_colon[index,] %>%
  mutate(age = from_age + sequence(aihw_colon$agediff) - 1) %>%
  dplyr::select(age, gender = sex, measure, colon.pct) %>%
  # convert any NAs (no colorectal cases) to 100% (ie all colon cancer, more likely than none)
  mutate(colon.pct = case_when(is.na(colon.pct)  ~ 100,
                               TRUE              ~ colon.pct)) %>%
  # pivot so measure (inc or mort) is in column names
  pivot_wider(names_from = measure, values_from = colon.pct, 
              names_prefix = "pct.") %>%
  # tidy gender names and NAs
  mutate(gender = case_when(gender == "Females" ~ "Female",
                            gender == "Males" ~ "Male"))

# multiply inc and mort numerators by relevant colon percentages  
gbd_carc <- gbddb %>% 
  filter(disease == "Colon and rectum cancer") %>%
  # join the AIHW data
  left_join(aihw_colon_yr, by = c("age", "gender")) %>%
  # update incidence and mortality by AIHW percentages
  mutate(inc_num = round(inc_num * pct.inc / 100),
         mort_num = round(mort_num * pct.inc / 100)) %>%
  # set prevalence to NA
  mutate(prev_num = NA, prev_denom = NA) %>%
  # remove the AIHW columns
  dplyr::select(-c(pct.inc, pct.mort))

gbddb <- gbddb %>%
  filter(!(disease == "Colon and rectum cancer")) %>% 
  full_join(gbd_carc)

# Note - 'disease' is still called 'colon and rectum cancer', even though it is
# now only colon cancer.  This is to align with other uses of abbreviation 'carc'.


## 1.7 Save result ----
## -------------------------------------#
saveRDS(gbddb, file=file.path("./Data/disbayes/inputs/", "disbayes_input_gbd.rds"))


# 2. Run disbayes ----
# -----------------------------------------------------------------------------#
## 2.0 Testing on a single disease (uncomment to use) ----
## -------------------------------------#
# selected.disease = "Colon and rectum cancer"
# selected.gender = "Female"
# 
# data_disease_gender <- readRDS(file=file.path("./Data/disbayes/inputs/", "disbayes_input_gbd.rds")) %>%
#   filter(disease == selected.disease, gender == selected.gender)
# 
# # # mcmc method
# dbres <- disbayes(data = data_disease_gender, age = "age",
#                   inc_num = "inc_num", inc_denom = "inc_denom",
#                   # prev_num = "prev_num", prev_denom = "prev_denom", # comment out for carc
#                   mort_num = "mort_num", mort_denom = "mort_denom",
#                   method="mcmc", chains=2, iter=1000, 
#                   eqage = 0)
# 
# # Extract data from results
# summ <- tidy(dbres) 
# 
# disbayes_output_1 <- summ %>% 
#   filter(var=="cf", between(age,0,100)) %>%
#   select(age, `50%`) %>%
#   mutate(sex_age_cat = paste0(tolower(selected.gender), "_", age)) %>%
#   rename(case_fatality = `50%`)  # if joining, would need to add disease, eg 'case_fatality_brsc'
# 
# disbayes_output_2 <- summ %>% 
#   filter(var=="inc", between(age,0,100)) %>%
#   select(age, `50%`) %>%
#   mutate(sex_age_cat = paste0(tolower(selected.gender), "_", age)) %>%
#   rename(incidence  = `50%`)  # if joining, would need to add disease, eg 'case_fatality_brsc'
# 
# library(ggplot2)
# ## case fatality & incidence
# plot(dbres, variable = "cf") +  ylab("Case fatality") + xlab("Age") 
# plot(dbres, variable = "inc") +  ylab("Incidence") + xlab("Age")
# 
# # ggsave("./SP_working/GBD_[insert]_cf.png",
# ggsave("./SP_working/GBD_carc_female_cf.png",
#        plot = plot(dbres, variable = "cf") +  ylab("Case fatality") + xlab("Age"),
#        width = 14, height = 14, units = "cm")
# 
# # ggsave("./SP_working/GBD_ [insert]_inc.png",
# ggsave("./SP_working/GBD_carc_female_inc.png",
#        plot = plot(dbres, variable = "inc") +  ylab("Incidence") + xlab("Age"),
#        width = 14, height = 14, units = "cm")


## 2.1 Load gbd data (prepared in section 1) ----
## -------------------------------------#
gbddb <- readRDS(file=file.path("./Data/disbayes/inputs/", "disbayes_input_gbd.rds")) %>%
  # convert to lower case and add short names
  mutate(gender = tolower(gender), 
         disease = tolower(disease), 
         disease = gsub("'", '', disease), # removing apostrophes
         sname = abbreviate(disease))

diseases <- unique(gbddb$sname) %>% sort()
genders <- unique(gbddb$gender) %>% sort()

combinations = crossing(diseases, genders) %>%
  mutate(combination = paste0(diseases, "_", genders)) %>%
  .$combination


## 2.2 Setup for parallel processing ----
## -------------------------------------#
# using 'doSNOW' instead of 'doParallel' as it provides a progress bar
# library(doParallel)
library(doSNOW)
library(parallel)
library(foreach)
# Detect the number of available cores and create cluster
cores <- detectCores()
# cluster <- parallel::makeCluster(cores)
# Option with output file - may capture some (but not all console messages/warnings)
cluster <- parallel::makeCluster(cores, outfile = "./output.txt")
# Activate cluster for foreach library
doSNOW::registerDoSNOW(cluster)
# Report
print(paste("Parallel processing with", cores, "cores"))

## set up progress reporting
# https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
pb <- txtProgressBar(max = length(combinations), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


## 2.3 Parallel processing disbayes loop ----
## -------------------------------------#
# Produces 44 dataframes (or error messages), one for each disease/gender
# combination.  Result is: (1) if successful, a list of (a) outputs and (b) 'dbres'
# (from which plots can be produced for the disease/gender combination, or
# (2) if unsuccessful, a text error message
disbayes_output_list <- foreach(i=1:length(combinations),
                                .packages = c("dplyr", "disbayes"),
                                .errorhandling = "pass",
                                # .verbose = TRUE,
                                .options.snow = opts) %dopar% {
  
    result <- tryCatch(
      expr = {
        
        selected.disease <- unlist(strsplit(combinations[i], "_"))[1]
        selected.gender <- unlist(strsplit(combinations[i], "_"))[2]
        
        data = gbddb %>%
          filter(sname == selected.disease & gender == selected.gender)
        
        # proceed if data exists (may not, eg no male data for uterine cancer)
        # note 'prevalence' is not available for 'carc', see section 1.6)
        if (nrow(data) > 0) {
          
          # # optimisation method
          # # adjust parameters as required, if using optimisation instead of mcmc
          # dbres <- disbayes(data = data, age = "age",
          #                   inc_num = "inc_num", inc_denom = "inc_denom",
          #                   prev_num = "prev_num", prev_denom = "prev_denom",
          #                   mort_num = "mort_num", mort_denom = "mort_denom",
          #                   eqage = 40)
          
          #  mcmc method 
          if (selected.disease == "carc") {
            dbres <- disbayes(data = data, age = "age",
                              inc_num = "inc_num", inc_denom = "inc_denom",
                              mort_num = "mort_num", mort_denom = "mort_denom",
                              method="mcmc", chains=4, iter=10000, 
                              eqage = 0)
            
          } else {
            dbres <- disbayes(data = data, age = "age",
                              inc_num = "inc_num", inc_denom = "inc_denom",
                              prev_num = "prev_num", prev_denom = "prev_denom",
                              mort_num = "mort_num", mort_denom = "mort_denom",
                              method="mcmc", chains=4, iter=10000, 
                              eqage = 0)
          }
          
          # convert list output to dataframe
          summ <- disbayes::tidy(dbres)
          
          # extract case fatality and incidence outputs, and combine
          cf_col <- paste0("case_fatality_", selected.disease)
          inc_col <- paste0("incidence_", selected.disease)
          rem_col <- paste0("remission_", selected.disease)
          
          # case fatality
          disbayes_output_cf <- summ %>% 
           filter(var=="cf", between(age, 0, 100)) %>% 
            select(age, `50%`) %>%
            mutate(sex_age_cat = paste0(selected.gender, "_", age)) %>%
            rename_with(~cf_col, `50%`)
          
          # incidence
          disbayes_output_inc <- summ %>%
            filter(var=="inc", between(age, 0, 100)) %>% 
            select(age, `50%`) %>%
            mutate(sex_age_cat = paste0(selected.gender, "_", age)) %>%
            rename_with(~inc_col, `50%`)
          
          # combined case fatality, incidence and remission (zero)
          disease_gender_df <- disbayes_output_cf %>%
            # combine case fatality and incidence columns
            inner_join(., disbayes_output_inc, by = "sex_age_cat") %>%
            dplyr::select(sex_age_cat, all_of(c(cf_col, inc_col))) %>%
            # add remission column (set to zero)
            mutate(rem_col = 0) %>%
            rename_with(~rem_col, rem_col)
          
          return(list(disease_gender_df, dbres))
        } else {
          return(paste(selected.disease, "|", selected.gender, " | no data"))
        }
      },

      # full error output will not appear when parallel processing,
      # but can be seen in individual step-through
      error = function(e){
        print(paste(selected.disease, "|", selected.gender, "| Error:", e))
        return(paste(selected.disease, "|", selected.gender, "| Error:", e))
      }
    )

return(result)
}

close(pb)
stopCluster(cluster)


## 2.4 Assemble results into output dataframe and plots, and write ----
## -------------------------------------#
# data frame for results, by sex_age_cat
sex_age_cat <- c()
for (i in c("female_", "male_")) {
  for (j in 0:100) {
    sex_age_cat <- c(sex_age_cat, paste0(i, j))
  }
}

disbayes_output_female <- 
  data.frame(sex_age_cat = sex_age_cat[str_detect(sex_age_cat, "female")])
disbayes_output_male <- 
  data.frame(sex_age_cat = sex_age_cat[!str_detect(sex_age_cat, "female")])

# data frame to hold error outputs
error_output <- data.frame()

# assemble parallel processing list outputs into dataframe of results, and plots
for (i in 1:length(disbayes_output_list)) {
  
  # find the output for [i] - if it's a list, then it's a successful output
  # consisting of (1) the desired outputs, plus (2) the dbres object (used
  # for plots); it it's not a list, then it's an error message
  selected.output <- disbayes_output_list[[i]]

  # successful results (lists) are joined to disbayes_output for gender,
  # and write plot
  if (is.list(selected.output)) {
    # get the disease name (from second column of selected.output[[1]])
    disease <- gsub("case_fatality_", "", names(selected.output[[1]])[2])
    
    if (str_detect(selected.output[[1]][1, "sex_age_cat"], "female")){
      # female
      disbayes_output_female <- disbayes_output_female %>%
        left_join(selected.output[[1]],
                  by = "sex_age_cat")
      
      ggsave(paste0("./disbayes_plots/", disease, "_female_cf.png"),
             plot = plot(selected.output[[2]], variable="cf") +  
               ylab("Case fatality") + xlab("Age"),
             width = 14, height = 14, units = "cm")
      
      ggsave(paste0("./disbayes_plots/", disease, "_female_inc.png"),
             plot = plot(selected.output[[2]], variable="inc") +  
               ylab("Incidence") + xlab("Age"),
             width = 14, height = 14, units = "cm")

    } else {
      #male
      disbayes_output_male <- disbayes_output_male %>%
        left_join(selected.output[[1]],
                  by = "sex_age_cat")
    
      ggsave(paste0("./disbayes_plots/", disease, "_male_cf.png"),
             plot = plot(selected.output[[2]], variable="cf") +  
               ylab("Case fatality") + xlab("Age"),
             width = 14, height = 14, units = "cm")
      

      ggsave(paste0("./disbayes_plots/", disease, "_male_inc.png"),
             plot = plot(selected.output[[2]], variable="inc") +  
               ylab("Incidence") + xlab("Age"),
             width = 14, height = 14, units = "cm")
      
    }
  } 
  # unsuccessful results (not lists) are added to error_output
  else {
    error_output <- rbind(error_output,
                          disbayes_output_list[[i]])
  }
}

# combine male and female results and write result
disbayes_output <- bind_rows(disbayes_output_female,
                              disbayes_output_male) %>%
  mutate(case_fatality_dprd = 0)

write.csv(disbayes_output, 
          "./data/processed/mslt/disbayes_output.csv",
          row.names = FALSE)

write.csv(error_output, "./disbayes_plots/error_output.csv")



