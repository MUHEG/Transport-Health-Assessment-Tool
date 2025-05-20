#################################################### FUNCTIONS TO RUN_MODEL ###############################################
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(srvyr)
library(stringr)
library(drpa)


# ----- Potential Impact Fraction ----
# Generate PIF per person
# Uses modify version from ithimr to account for participant weights 
# allows to calculate PIFs for physical activity and air pollution

## Brisbane output from this function differs from Melbourne output as follows.
## - The Brisbane input has age in 5 year bands, whereas the Melbourne input has
##     age in individual years.  For Melbourne, pifs are calculated as the mean
##     for each year of age.  Whereas for Brisbane, pifs are initially calculated
##     as the mean for each 5 year band, and then the pifs for the band are 
##     imputed to each year within that band (so the pifs for each year within a
##     5 year band are the same as each other).
## - The lowest age for Brisbane is 15 (from the 15-19 age band), whereas the 
##     lowest age for Melbourne is 18.


health_burden_2 <- function(ind_ap_pa_location,demographic_location,combined_AP_PA=T,calculate_AP=T){
  # ind_ap_pa_location=mmets_pp
  # demographic_location="Data/processed/DEMO.csv"
  # combined_AP_PA=F
  # calculate_AP=F

  
  # if ind_ap_pa_location is a file location, read the csv. If not, then use it as a dataframe.
  ind_ap_pa <- NULL
  if(is.character(ind_ap_pa_location)) {
    ind_ap_pa <- read.csv(ind_ap_pa_location,as.is=T, fileEncoding="UTF-8-BOM")
  }
  if(!is.character(ind_ap_pa_location)) {
    ind_ap_pa <- ind_ap_pa_location 
  }
  DISEASE_INVENTORY <- DISEASE_INVENTORY
  
  # filtering down to columns with 'mmet' in their name
  SCEN_SHORT_NAME <- colnames(ind_ap_pa)[grep("mmet",colnames(ind_ap_pa))]
  # removing '_mmet' to find the base scenario and scenario names
  SCEN_SHORT_NAME <- gsub("_mmet","",SCEN_SHORT_NAME)
  
  
  # select appropriate age field ('age' for Melbourne; 'age_band' for Brisbane)
  if ("age" %in% colnames(ind_ap_pa)) {
    age_field <- "age"
  } else if ("age_band" %in% colnames(ind_ap_pa)) {
    age_field <- "age_band"
  }
  
  # pif_scen <- pop_details
  pif_scen_2 <- ind_ap_pa_location %>% dplyr::select(any_of(c("dem_index", "participant_wt", "sex", "age_group_2", age_field)))
  # set up reference (scen1)
  reference_scenario <- SCEN_SHORT_NAME[1]
  scen_names <- SCEN_SHORT_NAME[SCEN_SHORT_NAME!=reference_scenario]
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_INVENTORY)){
    # j=2
    # Disease acronym and full name
    ac <- as.character(DISEASE_INVENTORY$acronym[j])
    gbd_dn <- as.character(DISEASE_INVENTORY$GBD_name[j])
    # calculating health outcome, or independent pathways?
    pathways_to_calculate <- ifelse(combined_AP_PA,1,DISEASE_INVENTORY$physical_activity[j]+DISEASE_INVENTORY$air_pollution[j])
    for(path in 1:pathways_to_calculate){
      # set up column names
      if(combined_AP_PA){
        middle_bit <-
          paste0(
            ifelse(DISEASE_INVENTORY$physical_activity[j] == 1, 'pa_', ''),
            ifelse(DISEASE_INVENTORY$air_pollution[j] == 1, 'ap_', '')
          )
        middle_bit_plus <-
          paste0(
            ifelse(DISEASE_INVENTORY$physical_activity[j] == 1, 'pa_', ''),
            ifelse(DISEASE_INVENTORY$air_pollution[j] == 1, 'ap_', ''),
            ifelse(DISEASE_INVENTORY$noise[j] == 1, 'noise_', ''),
            ifelse(DISEASE_INVENTORY$nitrogen_dioxide[j] == 1, 'no2_', '')
          )
      }else{
        # if independent, choose which one
        val <- c('pa_','ap_')[which(c(DISEASE_INVENTORY$physical_activity[j],DISEASE_INVENTORY$air_pollution[j])==1)]
        if(pathways_to_calculate>1) val=val[path]
        middle_bit <- middle_bit_plus <- val
      }
      if(calculate_AP==T | middle_bit!="ap_") {
        base_var <- paste0('RR_', middle_bit, reference_scenario, '_', ac)
        scen_vars <- paste0('RR_', middle_bit, scen_names, '_', ac)
        # set up pif tables
        # dies here if you don't have air pollution
        pif_table <- setDT(ind_ap_pa[,colnames(ind_ap_pa)%in%c(base_var,'dem_index', 'participant_wt', age_field, 'sex')])
        setnames(pif_table,base_var,'outcome') #problem here
        # pif_ref <- pif_table[,.(sum(outcome)),by='dem_index']  
        pif_ref_2 <- pif_table[,.(outcome)] 
        ## sort pif_ref
        # setorder(pif_ref,dem_index)
        for (index in 1:length(scen_vars)){
          # set up naming conventions
          scen <- scen_names[index]
          scen_var <- scen_vars[index]
          pif_name <- paste0('pif_',ac)
          # Calculate PIFs for selected scenario
          pif_table <- setDT(ind_ap_pa[,colnames(ind_ap_pa)%in%c(scen_var,'dem_index', 'participant_wt', age_field, 'sex')])
          setnames(pif_table,scen_var,'outcome', skip_absent = TRUE)
          # pif_temp <- pif_table[,.(sum(outcome)),by='dem_index']
          pif_temp_2 <- pif_table[,.(outcome)] 
          
          
          ## sort pif_temp
          # setorder(pif_temp,dem_index)
          # pif_scen[[pif_name]] <- (pif_ref[,V1] - pif_temp[,V1]) / pif_ref[,V1] 
          pif_scen_2[[pif_name]] <- (pif_ref_2[,outcome]  - pif_temp_2[,outcome]) / pif_ref_2[,outcome]
          pif <- pif_scen_2 %>%
            group_by(sex, across(any_of(age_field))) %>%
            dplyr::summarise_at(vars(starts_with("pif"), "age_group_2"), mean)
          
          
          ### Declare survey as weighted
          if("participant_wt" %in% colnames(pif_scen_2)) {
            pif_wt <- pif_scen_2 %>%
              srvyr::as_survey_design(weights = participant_wt)
            ### Calculate mean pifs by age and sex
            pif <-  pif_wt  %>%
              group_by(sex, across(any_of(age_field))) %>%
              dplyr::summarise_at(vars(starts_with("pif"),"age_group_2"), survey_mean)}
           
          
        }
      }
    }
  }
  
  # if participants only have an age band rather than a specific age (Brisbane),
  # impute the pifs for the band to each year in the band
  if (!("age" %in% colnames(pif)) & "age_band" %in% colnames(pif)) {
    pif <- pif %>%
      # convert age_band to a sequence (eg '15-19 years' to '15:19')
      rowwise() %>%
      mutate(age_band= str_split(age_band, "\\D")) %>%  # split at non-digits
      mutate(age_band = list(unlist(age_band[age_band != ""]))) %>%  # remove the empty elements
      mutate(age_band = list(seq(min(age_band), max(age_band))))%>%   # convert to sequence
      
      # create one row for each element of age_band, numbered sequentially as 'id'
      # (id is 1 to 5, except for age band 100+ where there is only 1)
      uncount(., weights = length(age_band), .id = "id") %>%
      
      # complete age as the id'th element of age_band
      rowwise() %>%
      mutate(age = age_band[id]) %>%
      
      # drop the age_band and 'i' columns
      dplyr::select(-age_band, -id) %>%
      ungroup()
      
  }
  
  pif
}


# ----- Proportional multi-state life table ----


### Age and sex life table for baseline and scenario.
# Names references

# mx:        mortality
# pyld_rate: person-years lived with a disability rate
# qx:        probability of dying
# lx:        number of survivors year 1 simulation
# dx:        number died in year 1 simulation
# Lx:        number of persons lived by cohort to age x + 1/2 (average people)
# ex:        life expectancy
# Lwx:       health adjusted life years
# ewx:       health adjusted life expectancy



RunLifeTable <- function(in_idata, in_sex, in_mid_age, death_rates=NA) {
  # in_idata=MSLT_DF
  # in_sex='male'
  # in_mid_age=17
  
  # Create a life table starting data frame from input data. 
  lf_df <- in_idata %>%
    dplyr::filter(age >= in_mid_age & sex == in_sex) %>%
    dplyr::select('sex', 'age', 'pyld_rate', 'mx')
  
  # are we using modified mortality rates?
  if(is.data.frame(death_rates)) {
    # filter to only this cohort's death rates
    cohort_death_rates <- death_rates %>%
      dplyr::filter(age_cohort == in_mid_age & sex == in_sex) %>%
      dplyr::select(age,sex,rate)
    # join to lf_df and replace mx with the new mortality data
    lf_df <- lf_df %>%
      dplyr::inner_join(cohort_death_rates, by=c('age','sex')) %>%
      dplyr::select(sex, age, pyld_rate, mx=rate)
  }
  
  
  # Create list life table variables. First as vector and then added to the data frame at the end.
  ## We model up to 100, that is the reason for the age limit in the function
  
  # probability of dying
  
  qx <-  ifelse(lf_df$age < 100, 1 - exp(-1 * lf_df$mx), 1)
  
  # number of survivors year 1 simulation
  
  num_row <- nrow(lf_df)
  lx <- rep(0,num_row)### BZ: growth rate here ??
  lx[1] <- as.numeric(in_idata$population[in_idata$age == in_mid_age & in_idata$sex == in_sex]) 
  
  # number died in year 1 simulation
  
  dx <- rep(0,num_row)
  dx[1] <- lx[1] * qx[1]
  
  # number of survivors and who die from year 2 onwards. 
  
  for (i in 2:num_row){
    lx[i] <- lx[i - 1] - dx[i - 1]
    dx[i] <- lx[i] * qx[i]
  }
  
  # number of persons lived by cohort to age x + 1/2 (average people)
  
  Lx <- rep(0,num_row)
  
  # for years up to 99
  
  for (i in 1:(num_row-1))
    Lx[i] <- (lx[i] + lx[i + 1]) / 2
  
  # for year 100, cohort dies at 100 if anyone left
  
  Lx[num_row] <- lx[num_row] / lf_df$mx[num_row]
  
  
  # create life expectancy variable
  ex <- rep(0,num_row)
  for (i in 1:num_row){
    ex[i] <- sum(Lx[i:num_row]) / lx[i]
  }
  
  # create health adjusted life years variable 
  
  Lwx <- Lx * (1 - lf_df$pyld_rate)
  
  # create health adjusted life expectancy variable
  ewx <- rep(0,num_row)
  for (i in 1:num_row){
    ewx[i] <- sum(Lwx[i:num_row]) / lx[i]
  }
  
  lf_df$qx <- qx
  lf_df$lx <- lx
  lf_df$dx <- dx
  lf_df$Lx <- Lx
  lf_df$ex <- ex
  lf_df$Lwx <- Lwx
  lf_df$ewx <- ewx
  lf_df
}

### Age and sex disease process for baseline and scenario
## Remission is not modeled.

RunDisease <- function(in_idata, in_sex, in_mid_age, in_disease, incidence_trends=NA, mortality_trends=NA) {
  # in_idata=MSLT_DF
  # in_sex='male'
  # in_mid_age=17
  # in_disease='dmt2'
  
  # create disease variable for the disease life table function 
  dw_disease <- paste("dw_adj", in_disease, sep = "_")
  incidence_disease <- paste("incidence", in_disease, sep = "_")
  case_fatality_disease <- paste("case_fatality", in_disease, sep = "_")
  
  ## add generic variable names to the source data frame (in_idata)
  in_idata$dw_disease <- in_idata[[dw_disease]]
  in_idata$incidence_disease <- in_idata[[incidence_disease]]
  in_idata$case_fatality_disease <- in_idata[[case_fatality_disease]]
  
  # Select columns for lifetable calculations
  
  dlt_df <- in_idata %>%
    dplyr::filter(age >= in_mid_age & sex == in_sex) %>% 
    dplyr::select('sex', 'age', dw_disease, incidence_disease, case_fatality_disease)
  
  
  dlt_df$disease <- in_disease
  
  # are we using modified mortality trends?
  if(is.data.frame(mortality_trends)) {
    # filter to only this cohort's incidence trends
    cohort_mortality_trends <- mortality_trends %>%
      dplyr::filter(sex == in_sex) %>%
      dplyr::select('year',mortality_trend=in_disease) %>%
      arrange(year) %>% # SP: needed, because (at least when I tested it) otherwise
      # rows are in year order 0, 1, 10, 100, 11, 12 ... 19, 2, 20 ...
      dplyr::mutate(row_num=row_number())
    
    dlt_df <- dlt_df %>%
      dplyr::mutate(row_num=row_number()) %>%
      dplyr::inner_join(cohort_mortality_trends, by=c('row_num')) %>%
      dplyr::mutate(case_fatality_disease=case_fatality_disease*mortality_trend) %>%
      dplyr::select('sex', 'age', 'dw_disease', 'incidence_disease', 'case_fatality_disease', 'disease')
  }
  
  # are we using modified incidence trends?
  if(is.data.frame(incidence_trends)) {
    # filter to only this cohort's incidence trends
    cohort_incidence_trends <- incidence_trends %>%
      dplyr::filter(sex == in_sex) %>%
      dplyr::select('year',incidence_trend=in_disease) %>%
      arrange(year) %>%
      dplyr::mutate(row_num=row_number())
    
    dlt_df <- dlt_df %>%
      dplyr::mutate(row_num=row_number()) %>%
      dplyr::inner_join(cohort_incidence_trends, by=c('row_num')) %>%
      dplyr::mutate(incidence_disease=incidence_disease*incidence_trend) %>%
      dplyr::select('sex', 'age', 'dw_disease', 'incidence_disease', 'case_fatality_disease', 'disease')
  }
  
  # create list of life table variables. Created as vectors and then added to dataframe. 
  # See see methods in: 1) Concept and original calculations: Barendregt, J. J., et al. (1998). "Coping with multiple morbidity in a life table." Math Popul Stud 7(1): 29-49. 
  # and 2) Latest version, variables below calculated from it: Barendregt, J. J., et al. (2003). "A generic model for the assessment of disease epidemiology: the computational basis of DisMod II." Popul Health Metr 1(1): 4-4.
  
  
  ### lx, qx, wx and vx are intermediate variables, 
  
  lx <- dlt_df$incidence_disease + dlt_df$case_fatality_disease
  qx <-  sqrt((dlt_df$incidence_disease - dlt_df$case_fatality_disease) * (dlt_df$incidence_disease - dlt_df$case_fatality_disease))
  wx <- exp(-1*(lx+qx)/2)
  vx <- exp(-1*(lx-qx)/2)
  
  ## Healthy (Sx), Disease (Cx) and Death (Dx), total (Tx) (control check, has to be 1000), total alive (Ax)
  ## persons years live at risk (PYx), prevalence rate (px), mortality rate (mx)
  ## Remission and mortality from other causes were replaced by zero in the formulas (as we assume no remission and independence of disease mortality with total mortlaity). 
  
  ### First create empty variables
  
  number_of_ages <- nrow(dlt_df)
  Sx <- Cx <- Dx <- Tx  <- Ax <- PYx <- px <- mx <- rep(0,number_of_ages)
  cfds <- dlt_df$case_fatality_disease
  ages <- dlt_df$age
  
  #### Starts with 1000 healthy (Sx) and total (Ax) people. 
  
  Sx[1] <- Ax[1] <- 1000
  
  ##### start with variables without calculation exceptions
  
  ##### variables without exceptions (else includes exception for year one of the simulation)  
  for (i in 2:(number_of_ages-1)){ ##!! this can go to "number_of_ages" now (?)
    if(qx[i-1] > 0){
      
      ### The following five variables are created to simplify Sx, Cx and Dx calculations, and do not form part of the disease life tables.
      vxmwx <- vx[i-1] - wx[i-1]
      SxpCx <- Sx[i-1]+Cx[i-1]
      dqx <- 2 * qx[i-1]
      qxmlx <- qx[i-1] - lx[i-1]
      qxplx <- qx[i-1] + lx[i-1]
      
      ### Healthy (Sx), Diseases (Cx) and Death from the Disease (Dx)
      
      Sx[i] <- Sx[i-1] * (2*vxmwx * cfds[i-1]  + (vx[i-1] * qxmlx + wx[i-1] * qxplx)) / dqx
      Cx[i] <- -1*(vxmwx*(2*(cfds[i-1]  * SxpCx - lx[i-1] * Sx[i-1]) - Cx[i-1] * lx[i-1]) - Cx[i-1] * qx[i-1] * (vx[i-1]+wx[i-1])) / dqx
      Dx[i] <- (vxmwx * (2 * cfds[i-1] * Cx[i-1] - lx[i-1]*SxpCx)- qx[i-1] * SxpCx*(vx[i-1]+wx[i-1]) + dqx * (SxpCx+Dx[i-1]) ) / dqx
    }else{
      Sx[i] <- Sx[i - 1] 
      Cx[i] <- Cx[i - 1]
      Dx[i] <- Dx[i - 1]
    }
  }
  
  
  Tx   <- Sx + Cx + Dx 
  Ax <- Sx + Cx
  
  first_indices <- 1:(number_of_ages-1)
  last_indices <- 2:number_of_ages
  PYx <- (Ax[first_indices] + Ax[last_indices])/2
  mx[first_indices] <- (Dx[last_indices] - Dx[first_indices])/PYx[first_indices]
  mx[mx<0] <- 0
  px[first_indices] <- (Cx[last_indices] + Cx[first_indices])/2/PYx[first_indices]
  
  dlt_df$Tx <- Tx
  dlt_df$mx <- mx
  dlt_df$px <- px
  dlt_df
}

# ---- Parameters with uncertainty ----

# Get parameters to run PMSLT
# CAPITAL LETTERS ARE GLOBAL INPUTS
# lower_case are location specific (Melbourne here)

### Get stDev to calculated corrected RR
GetStDevRR <- function(RR, LB, UB){
  
  SE=(log(UB)-log(LB))/3.92
  return(SE)
}

### Get modified parameters for lognormal distribution for RRs

GetShape <- function(mean, sd) {
  Shape=sqrt(log(sd ^ 2 + 
                   exp(2 * log(mean))) - 
               2 * log(mean))
  return(Shape)
}

GetLocation <- function(mean, shape) {
  Location = log(mean) - .5 * shape^2
  return(Location)
}


GetParameters <- function(DIABETES_IHD_RR_F = 2.82, 
                          DIABETES_STROKE_RR_F = 2.28,
                          DIABETES_IHD_RR_M = 2.16, 
                          DIABETES_STROKE_RR_M = 1.83){
  ## To test function
  
  # MMET_CYCLING = 4.63
  # MMET_WALKING = 2.53
  # DIABETES_IHD_RR_F = c(2.82, 2.35, 3.38)
  # DIABETES_STROKE_RR_F = c(2.28, 1.93, 2.69)
  # DIABETES_IHD_RR_M = c(2.16, 1.82, 2.56)
  # DIABETES_STROKE_RR_M = c(1.83, 1.60, 2.08)
  # UNCERTAINTY = T
  
  
  parameters <- list()
  
  
  ### Relative risks diabetes for ihd and stroke
  
  normVariablesRR <- c("DIABETES_IHD_RR_F",
                       "DIABETES_STROKE_RR_F",
                       "DIABETES_IHD_RR_M",
                       "DIABETES_STROKE_RR_M"
  )
  
  
  for (i in 1:length(normVariablesRR)) {
    name <- normVariablesRR[i]
    val <- get(normVariablesRR[i])
    if (length(val) == 1) {
      parameters[[name]] <- val[1]
    } else {
      parameters[[name]] <-
        rlnorm(NSAMPLES, GetLocation(val[1], GetStDevRR(val[1], val[2],val[3])),
               GetShape(val[1], GetStDevRR(val[1], val[2],val[3])))
    }
  }
  
  
  ### Relative risks physical activity
  
  if(UNCERTAINTY == T ) {
    
    parameters[["QUANTILE"]] <- runif(NSAMPLES,0,1)
    
  }
  
  else {
    parameters[["QUANTILE"]] <- 0.5
    parameters[["DIABETES_IHD_RR_M"]] <- 2.16   
    parameters[["DIABETES_IHD_RR_F"]] <-  2.82
    parameters[["DIABETES_STROKE_RR_M"]] <- 1.83
    parameters[["DIABETES_STROKE_RR_F"]] <- 2.28
  }
  parameters 
}


# ---- Calculate model ----

# Includes above developed functions to run the model




CalculationModel <- function(output_location="modelOutput",
                             persons_matched  ### ADD options for what rrs to use
){
  
  # persons_matched <- read.csv("./scenarios/melbourne-scenarios/all_0_2.csv")
  # persons_matched <- read.csv("./scenarios/brisbane-scenarios/all_0_2.csv")
  
  ## list parameters to global env when checking the code below (comment out 
  # otherwise - required for 'quantile' in the calculation of relative risks, 
  # and for 'disease relative risks' in the scenario life table); 'seed_current' also required
  # list2env(parameters,globalenv())
  # seed_current <- 1
  
  
  # if persons_matched is a file location, read the csv. If not, then
  # use it as a dataframe.
  persons_matched_df <- NULL
  if(is.character(persons_matched)) {
    persons_matched_df <- read.csv(persons_matched,as.is=T, fileEncoding="UTF-8-BOM")
  }
  if(!is.character(persons_matched)) {
    persons_matched_df <- persons_matched
  }
  persons_matched <- persons_matched_df
  cat(paste0("loaded persons_matched, with ", nrow(persons_matched)," rows\n"))
  
  
  # Generate marginal mets for matched population, then used to derive RRs per person. Total defaults is FALSE, do not include work mmets.
  mmets_pp <- calculateMMETSperPerson(
    matched_pop_location = persons_matched,
    MMET_CYCLING = MMET_CYCLING, 
    MMET_WALKING = MMET_WALKING,
    TOTAL = F
  )
  cat(paste0("have run calculateMMETSperPerson\n"))
  
  # Create age groups for easier presentation changes and convert variables to factors for summaries
  # Note Brisbane cannot use same bands as Melbourne, because Brisbane QTS ages are in 5 year bands
  # [SP query - perhaps Melbourne should use same bands as below for Brisbane - compare current
  #  bands at https://auo.org.au/transport-health-assessment/ which are 16-19, 20-39, 40-64, 65+]
  # For Melbourne, groups are based on 'age'; for Brisbane, based on 'age_band'
  if ("age" %in% colnames(mmets_pp)) {  # Melbourne has 'age'
    mmets_pp <- mmets_pp %>%
      dplyr::filter(age != 0) %>%
      dplyr::mutate(age_group = as.factor(case_when(
        age <  18             ~ "0 to 17" ,
        age >= 18 & age <= 40 ~ "18 to 40",
        age >= 41 & age <= 65 ~ "41 to 65",  ## SP note: perhaps '65' should be '64'
        age >= 65             ~ "65 plus"))) %>%
      mutate(sex=as.factor(sex)) 
  } else if ("age_band" %in% colnames(mmets_pp)) {  # Brisbane has 'age band'
    mmets_pp <- mmets_pp %>%
      dplyr::mutate(age_group = as.factor(case_when(
        age_band == "15-19 years"                      ~ "0 to 19" ,
        age_band %in% c("20-24 years", "25-29 years",
                        "30-34 years", "35-39 years")  ~ "20 to 39",
        age_band %in% c("40-44 years", "45-49 years",
                        "50-54 years", "55-59 years",
                        "60-64 years")                 ~ "40 to 64",
        age_band %in% c("65-69 years", "70-74 years",
                        "75-79 years", "80-84 years",
                        "85-89 years", "90-94 years",
                        "95-99 years", "100 years and over") ~ "65 plus"))) %>%
      mutate(sex=as.factor(sex)) 
    
  }
  
  
  # Generate relative risks per person (need to use abbreviated names for RRs)
  # Check that sometimes it throws an error if function not ran prior to running code in model_script...
  
  for (s in SCEN_SHORT_NAME) {
    for (i in 1:nrow(DISEASE_SHORT_NAMES)) {
      # s="scen1"
      # i=6
      # o="fatal-and-non-fatal"
      
      
      mmets_pp[,paste("RR_pa", s, DISEASE_SHORT_NAMES$acronym[i], sep = "_")] <- drpa::dose_response(cause = DISEASE_SHORT_NAMES$acronym[i],
                            outcome_type = ifelse(DISEASE_SHORT_NAMES$acronym[i] == "diabetes", "non-fatal",'fatal-and-non-fatal'), 
                            #"fatal-and-non-fatal", #
                            dose = mmets_pp[,paste0(s, "_mmet")],
                            quantile = get(paste("QUANTILE"), envir = .GlobalEnv),
                            confidence_intervals = F)
    }
  }
  mmets_pp
  
  cat(paste0("have generated relative risks\n"))
  
  # Calculate PIFs by age and sex groups
  # See note in 'health_burden_2' function regarding difference 
  # between Melbourne and Brisbane in available age details
  pif <- health_burden_2(
    ind_ap_pa_location= mmets_pp,
    demographic_location="Data/processed/DEMO.csv",
    combined_AP_PA=F,
    calculate_AP=F
  ) %>%
    dplyr::filter(age > 16) %>%
    replace(is.na(.), 0)
  
  if(city=="melbourne") {
  pif <- pif %>% 
    group_by(sex) %>%
    do(data.frame(age=seq(1:100))) %>%
    ungroup() %>%
    left_join(pif) %>% 
    dplyr::filter(age > 16) %>%
    replace(is.na(.), 0) 
   
    }
  
  # Calculate PMSLT

  ## Inputs
  
  cat(paste0("have run health_burden_2\n"))
  
  ### Steps 
  # 1) Run general life table baseline
  # 2) Run disease life tables baseline
  # 3) Run scenario life tables (where incidence is mofified by pif)
  # 4) Collect changes in mx and pylds from differences between baseline and sceanrio disease life tables
  # 5) Recalculate general life table with mx and totalpylds modified by 4
  
  
  
  # 1) Run general life table baseline
  
  # browser()
  
  general_life_table_list_bl <- list()
  
  # dataframe of the age and sex cohorts (crossing just does a cross product)
  age_sex_cohorts <- crossing(data.frame(age=i_age_cohort),
                              data.frame(sex=c('male', 'female'))) %>%
    dplyr::mutate(cohort=paste0(age,"_",sex))
  
  for (i in 1:nrow(age_sex_cohorts)){
    suppressWarnings(
      general_life_table_list_bl[[i]] <- RunLifeTable(
        in_idata    = MSLT_DF,
        in_sex      = age_sex_cohorts$sex[i],
        in_mid_age  = age_sex_cohorts$age[i],
        death_rates = death_projections ## Belen, add option to run static
      ))
    names(general_life_table_list_bl)[i] <- age_sex_cohorts$cohort[i]
  }
  
  # convert the list of dataframes to single dataframes
  general_life_table_bl <- bind_rows(general_life_table_list_bl, .id = "age_group") %>%
    mutate(age_group = as.numeric(gsub("_.*","",age_group)))
  
  cat(paste0("have run baseline life table"))
  
  # 2) Run disease life tables baseline
  
  ### Change order in disease short_names to start with diabetes. This is important when calculating the scenario disease life tables as diabetes is calculated first to then 
  ### impact on cardiovascular disease calculations. 
  
  
  disease_cohorts <- DISEASE_SHORT_NAMES %>%
    # Exclude non-diseases, road injuries, and diseases with no pif
    dplyr::filter(is_not_dis == 0 & acronym != 'no_pif' & acronym != 'other') %>%
    dplyr::select(sname,acronym,males,females)
  
  # adding the age and sex cohorts:
  age_sex_disease_cohorts <- crossing(age_sex_cohorts,disease_cohorts) %>%
    mutate(cohort=paste0(age,'_',sex,'_',sname)) %>%
    # Exclude non-male diseases (and non-female if there were any)
    filter( (sex=='male' & males==1) | (sex=='female' & females==1)) %>%
    dplyr::select(age,sex,sname,acronym,cohort) %>%
    # ishd and strk have the prerequisite disease dmt2
    mutate(prerequsite=ifelse(sname %in% c("ishd","strk"),paste0(age,"_",sex,"_dmt2"),0)) %>%
    # if all-cause cancer true
    mutate(cancers=ifelse(str_detect(acronym, 'cancer') | 
                            acronym == "myeloid-leukemia" |
                            acronym == "myeloma", 
                          "all-cause-cancer", 0)) %>%
    # ensuring prequisites are calculated first
    arrange(age,sex,prerequsite, cancers, sname)
  
  
  disease_life_table_list_bl <- list()
  
  for (i in 1:nrow(age_sex_disease_cohorts)){
    disease_life_table_list_bl[[i]] <- RunDisease(
      in_idata         = MSLT_DF,
      in_mid_age       = age_sex_disease_cohorts$age[i],
      in_sex           = age_sex_disease_cohorts$sex[i],
      in_disease       = age_sex_disease_cohorts$sname[i],
      incidence_trends = incidence_trends,
      mortality_trends = mortality_trends
    )
    names(disease_life_table_list_bl)[i] <- age_sex_disease_cohorts$cohort[i]
    
  }
  cat(paste0("have run baseline disease life table"))
  
  # 3) Run scenario life tables (where incidence is modified by pif)
  
  ### Read disease inventory and only include PA related diseases
  
  ## note when checking the code with UNCERTAINTY=F that parameters need to be listed
  ## to global env for the adjustment for diabetes effect on ihsd and stroke below 
  ## to work - there is a line at the start of 'CalculationModel' for this, or 
  ## uncomment line below do it here
  # list2env(parameters,globalenv())
  
  disease_relative_risks <- tribble(
    ~sex    , ~prerequsite, ~disease , ~relative_risk       ,
    "male"  ,  "dmt2"     ,  "ishd"  ,  DIABETES_IHD_RR_M   ,
    "female",  "dmt2"     ,  "ishd"  ,  DIABETES_IHD_RR_F   ,
    "male"  ,  "dmt2"     ,  "strk"  ,  DIABETES_STROKE_RR_M,
    "female",  "dmt2"     ,  "strk"  ,  DIABETES_STROKE_RR_F
  )
  
  disease_life_table_list_sc <- list()
  
  for (i in 1:nrow(age_sex_disease_cohorts)){
    
    # i=2L
    td1_age_sex <- MSLT_DF %>%
      filter(age >= age_sex_disease_cohorts$age[i] & sex == age_sex_disease_cohorts$sex[i])

   {if(age_sex_disease_cohorts$cancers[i] != 0 && cancers_all == T){
      
      pif_colname <- paste0('pif_all-cause-cancer')
      
      pif_disease <- pif %>%
        filter(age >= age_sex_disease_cohorts$age[i] & sex == age_sex_disease_cohorts$sex[i]) %>%
        dplyr::select(age,sex,pif_colname)
    }else{
    pif_colname <- paste0('pif_',age_sex_disease_cohorts$acronym[i])
    
    pif_disease <- pif %>%
      filter(age >= age_sex_disease_cohorts$age[i] & sex == age_sex_disease_cohorts$sex[i]) %>%
      dplyr::select(age,sex,pif_colname)
    }
      }
    
    # adjustment for diabetes effect on ihd and stroke
    if(age_sex_disease_cohorts$prerequsite[i] != 0){
      # get name for pif column
      target_disease <- paste0("pif_",age_sex_disease_cohorts$acronym[i])
      # get prerequisite disease cohort name (i.e., age_sex_dmt2 for diabetes)
      dia_col <- age_sex_disease_cohorts$prerequsite[i]
      # select relative risk of disease given diabetes (depends on sex, not age)
      relative_risk <- disease_relative_risks %>%
        filter(sex == age_sex_disease_cohorts$sex[i] &
                 disease == age_sex_disease_cohorts$sname[i]) %>%
        pull(relative_risk)
    
      scenario_prevalence <- disease_life_table_list_sc[[dia_col]]$px
      baseline_prevalence <- disease_life_table_list_bl[[dia_col]]$px
      pif_dia <- -(scenario_prevalence - baseline_prevalence)*(relative_risk-1)/
        (baseline_prevalence * (relative_risk-1) + 1)
      # modify pif for target disease: new pif =  (1 - old pif) * (1 - diabetes pif)
      pif_disease[[target_disease]] <- 1- (1-pif_disease[[target_disease]]) * (1-pif_dia)
      # print(sum(old_pif-pif_disease[[target_disease]]))
      
      
    }


        incidence_colname <- paste0('incidence_', age_sex_disease_cohorts$sname[i])
        new_col <- td1_age_sex%>%pull(incidence_colname) * (1 - (pif_disease%>%pull(pif_colname)))
        new_col[is.na(new_col)] <- 0
        td1_age_sex[[incidence_colname]] <- new_col
    
    
    
    ## Instead of idata, feed td to run scenarios. Now all diseases are run again, with the effect of diabetes
    ## on cardiovascular diseases taken into account. 
  
    disease_life_table_list_sc[[i]] <- RunDisease(
      in_idata         = td1_age_sex,
      in_sex           = age_sex_disease_cohorts$sex[i],
      in_mid_age       = age_sex_disease_cohorts$age[i],
      in_disease       = age_sex_disease_cohorts$sname[i],
      incidence_trends = incidence_trends,
      mortality_trends = mortality_trends
    )
    names(disease_life_table_list_sc)[i] <- age_sex_disease_cohorts$cohort[i]
    
  }
  cat(paste0("have run scenario disease life table"))
  
  for (cohort in age_sex_disease_cohorts$cohort) {
    disease_life_table_list_sc[[cohort]]$diff_inc_disease <-
      disease_life_table_list_sc[[cohort]]$incidence_disease - disease_life_table_list_bl[[cohort]]$incidence_disease
    
    disease_life_table_list_sc[[cohort]]$diff_prev_disease <-
      disease_life_table_list_sc[[cohort]]$px - disease_life_table_list_bl[[cohort]]$px
    
    disease_life_table_list_sc[[cohort]]$diff_mort_disease <-
      disease_life_table_list_sc[[cohort]]$mx - disease_life_table_list_bl[[cohort]]$mx
    
    disease_life_table_list_sc[[cohort]]$diff_pylds_disease <-
      (disease_life_table_list_sc[[cohort]]$px - disease_life_table_list_bl[[cohort]]$px) * 
      (disease_life_table_list_bl[[cohort]]$dw_disease)
  }
  
  
  # convert the list of dataframes to single dataframes
  disease_life_table_bl <- bind_rows(disease_life_table_list_bl, .id = "age_sex_disease_cohort") %>%
    dplyr::mutate(age_sex_disease_cohort = as.numeric(gsub("_.*","",age_sex_disease_cohort))) %>%
    dplyr::rename(age_group=age_sex_disease_cohort) %>% 
    mutate(mx = ifelse(disease == "dprd", 0, mx)) # depression has no mx, but some left from original data
  
  disease_life_table_sc <- bind_rows(disease_life_table_list_sc, .id = "age_sex_disease_cohort") %>%
    dplyr::mutate(age_sex_disease_cohort = as.numeric(gsub("_.*","",age_sex_disease_cohort))) %>%
    dplyr::rename(age_group=age_sex_disease_cohort) %>% 
    mutate(mx = ifelse(disease == "dprd", 0, mx)) # d
  
  
  # 4) Collect changes in mx and pylds from differences between baseline and sceanrio disease life tables
  
  ### Sum mortality rate and pylds change scenarios
  mx_pylds_sc_total_disease_df <- disease_life_table_sc %>%
    dplyr::group_by(age_group, sex, age) %>%
    dplyr::summarise(mortality_sum = sum(diff_mort_disease,na.rm=T),
                     pylds_sum=sum(diff_pylds_disease,na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(age_sex_cohort=paste0(age_group,'_',sex))
  
  
  # 5) Recalculate general life table with mx and totalpylds modified by 4
  
  ## Calculate general life tables with modified mortality and pylds total
  ## Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
  ## Original pyld rate is modified by the change in each disease pylds
  
  general_life_table_list_sc <- list()
  
  for (i in 1:nrow(age_sex_cohorts)){
    # modify idata's mortality and pyld total for the said scenario
    mx_pylds_sc_total_disease_df_cohort <- mx_pylds_sc_total_disease_df %>%
      filter(age_sex_cohort==age_sex_cohorts$cohort[i]) %>%
      dplyr::select(age,mortality_sum,pylds_sum)
    
 
    ### Modify rates in static MSLT  (pylds are always static, mx can include future trends)

    ### removing the impact from the accumulated mortality from the diseases
    td1 <- MSLT_DF %>%
      filter(sex==age_sex_cohorts$sex[i]) %>%
      # filter(sex == age_sex_cohorts$sex[i] & age >= age_sex_cohorts$age[i]) %>%  # SP: probably don't need
      left_join(mx_pylds_sc_total_disease_df_cohort,by="age") %>%
      mutate(mx=mx+replace_na(mortality_sum,0),
             pyld_rate=pyld_rate+replace_na(pylds_sum,0)) %>%
      dplyr::select(-pylds_sum)
  
    ### case when we use all cause instead of disease specific mortality
    if(all_cause == T) {
      pif_all_cause <- pif %>% dplyr::select(age, sex, 'pif_all-cause-mortality')
      

     td2 <- td1 %>%
       left_join(pif_all_cause, by=c("age", "sex")) %>%
       dplyr::mutate(mx=(mx-replace_na(mortality_sum,0))*(1-`pif_all-cause-mortality`)) %>%
       dplyr::select(-mortality_sum)
    } else {
      td2=td1
      }

    
    ### Modify death rates with future trends
    pif_all_cause <- pif %>% dplyr::select(age, sex, 'pif_all-cause-mortality')
    td3 <- death_projections %>%
      mutate(cohort=paste(age_cohort, sex, sep = "_")) %>% # variable to match change in mortality rates df
      filter(cohort==age_sex_cohorts$cohort[i]) %>%
      left_join(mx_pylds_sc_total_disease_df_cohort) %>%
      left_join(pif_all_cause)
      ## takes into account the option of using all-cause mortlaity instead of mortality sum from individual diseases
       if (all_cause == T) {
         td3 <- td3 %>%
      mutate(rate=rate*(1-`pif_all-cause-mortality`))   
      
  } else {
      td3 <- td3 %>%
      mutate(rate=rate+replace_na(mortality_sum,0))%>%
      dplyr::select(-mortality_sum,-pylds_sum)   
      }

    suppressWarnings(
      general_life_table_list_sc[[i]] <- RunLifeTable(
        in_idata    = td2,
        in_sex      = age_sex_cohorts$sex[i],
        in_mid_age  = age_sex_cohorts$age[i],
        death_rates = td3
      ))
    names(general_life_table_list_sc)[i] <- age_sex_cohorts$cohort[i]
  }
  
  # convert the list of dataframes to single dataframes
  general_life_table_sc <- bind_rows(general_life_table_list_sc, .id = "age_group") %>%
    mutate(age_group = as.numeric(gsub("_.*","",age_group)))
  
  
  # 6) Generate outputs dataframe
  
  # In the following list 'output_life_table', 34 data frames are nested per age and sex cohort
  # Outputs are generated following the index order of disease life tables baseline and scenarios where diabetes is first calculated as it impacts on cardiovascular diseases. 
  # In the following list 'output_life_table', 34 data frames are nested per age and sex cohort
  # Outputs are generated following the index order of disease life tables baseline and scenarios where diabetes is first calculated as it impacts on cardiovascular diseases. 
  
  dia_index <- which(DISEASE_SHORT_NAMES$sname=='dmt2')
  dia_order <- c(dia_index,c(1:nrow(DISEASE_SHORT_NAMES))[-dia_index])
  
  disease_sc <- inner_join(disease_life_table_sc %>%
                             dplyr::select(age_group,sex,age,disease,incidence_disease,mx,px),
                           general_life_table_sc %>%
                             dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx),
                           by=c("age","sex","age_group")) %>%
    mutate(intervention="sc")
  
  disease_bl <- inner_join(disease_life_table_bl %>%
                             dplyr::select(age_group,sex,age,disease,incidence_disease,mx,px),
                           general_life_table_bl %>%
                             dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx),
                           by=c("age","sex","age_group")) %>%
    mutate(intervention="bl")
  
  disease_combined <- bind_rows(disease_sc,disease_bl) %>%
    pivot_wider(names_from  = intervention,
                values_from = c(incidence_disease,mx,px,Lx,ex,Lwx,ewx)) %>%
    mutate(inc_num_bl   = incidence_disease_bl*(1-px_bl)*Lx_bl,
           inc_num_sc   = incidence_disease_sc*(1-px_sc)*Lx_sc,
           inc_num_diff = inc_num_sc-inc_num_bl,
           mx_num_bl    = mx_bl*Lx_bl,
           mx_num_sc    = mx_sc*Lx_sc,
           mx_num_diff  = mx_num_sc-mx_num_bl,
           px_num_bl    = px_bl*Lx_bl,  
           px_num_sc    = px_sc*Lx_sc,
           px_num_diff  = px_num_sc-px_num_bl) %>%  # px_num_diff is used to calculate disease costs
    # join disease costs per case
    left_join(., disease.costs %>%
                dplyr::select(sex, disease = sname, cost.per.case = cost), 
              by = c("sex", "disease")) %>%
    pivot_wider(names_from  = disease,
                values_from = -c("age_group", "sex", "age", "disease"))
  
  general_lf <- bind_rows(
    general_life_table_sc %>%
      dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx) %>%
      mutate(intervention="sc"),
    general_life_table_bl %>%
      dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx) %>%
      mutate(intervention="bl")) %>%
    pivot_wider(names_from  = intervention,
                values_from = c(Lx,ex,Lwx,ewx)) %>%
    mutate(Lx_diff  = Lx_sc-Lx_bl,
           Lwx_diff = Lwx_sc-Lwx_bl,
           ex_diff  = ex_sc-ex_bl,
           ewx_diff = ewx_sc-ewx_bl)
  
  
  ## Dataframe with all outputs by age and sex cohort over the simulation years (years of the cohort)
  output_df <- inner_join(disease_combined,
                          general_lf,
                          by=c("age","sex","age_group"))
  
  
  
  ### Create age groups variable, easier to read
  
  output_df <- output_df %>%
    mutate(age_group_2 = case_when(
      age_group == 17 ~ "16-19",
      age_group == 22 ~ "20-24",
      age_group == 27 ~ "25-29",
      age_group == 32 ~ "30-34",
      age_group == 37 ~ "35-39",
      age_group == 42 ~ "40-44",
      age_group == 47 ~ "45-49",
      age_group == 52 ~ "50-54",
      age_group == 57 ~ "55-59",
      age_group == 62 ~ "60-64",
      age_group == 67 ~ "65-69",
      age_group == 72 ~ "70-74",
      age_group == 77 ~ "75-79",
      age_group == 82 ~ "80-84",
      age_group == 87 ~ "85-89",
      age_group == 92 ~ "90-94",
      age_group == 97 ~ "95 plus")) %>%
    mutate(cohort=paste(sex, age_group, sep = "_"))
  
  ### Get population to add to tables
  
  population2 <- population %>%
    dplyr::rename(cohort = sex_age_cat) %>%
    dplyr::filter(cohort %in% unique(output_df$cohort)) %>%
    separate(cohort,into=c('sex','age'),sep='_') %>%
    dplyr::mutate(age_group_final=case_when(
      age==17                        ~'15-19',
      age%in%c(22,27,32,37)          ~'20-39',
      age%in%c(42,47,52,57,62)       ~'40-64',
      age%in%c(67,72,77,82,87,92,97) ~'65plus'))
  
  populationLargeCohort <- bind_rows(
    population2,
    population2%>%mutate(age_group_final='all'),
    population2%>%mutate(sex='all'),
    population2%>%mutate(age_group_final='all',sex='all')
  ) %>%
    mutate(population = as.numeric(population)) %>%
    group_by(sex,age_group_final) %>%
    dplyr::summarise(population=sum(population,na.rm=T)) %>%
    ungroup()
  rm(population2)
  
  ### Add age and sex all
  
  output_df_year <- output_df  %>% # Create a simulation year columns 
    group_by(age_group, sex, .add=TRUE) %>%
    dplyr::mutate(year = row_number()) %>%
    ungroup() %>%
    dplyr::mutate(age_group_final=case_when(
      age_group_2 == '16-19'                                                        ~'15-19',
      age_group_2 %in% c('20-24','25-29','30-34','35-39')                           ~'20-39',
      age_group_2 %in% c('40-44','45-49','50-54','55-59','60-64')                   ~'40-64',
      age_group_2 %in% c('65-69','70-74','75-79','80-84','85-89','90-94','95 plus') ~'65plus'))
  rm(output_df)
  
  ### Add value of statistical life years calculations
  for (i in 1:length(discount.rates)) {
    # constants for the relevant discount rate
    rate <- discount.rates[i]
    val.col <- paste0("VSLY.", rate)
    
    # create value of health-adjusted life years columns
    output_df_year <- output_df_year %>% 
      mutate(!!val.col := (Lwx_diff * VSLY) / ((1 + (rate / 100)) ^ year))
  }
  
  ### Add health care system costs
  # Vector of the names of diseases that have cost columns (should be all)
  diseases.with.costs <- output_df_year %>% 
    dplyr::select(contains("cost.per.case")) %>% names(.) %>%
    str_split(., "_") %>% unlist() %>% unique() %>% 
    .[!(. == "cost.per.case")]
  
  # Add cost column for each disease and discount rate
  for (i in 1:length(discount.rates)) {
    rate <- discount.rates[i]
    
    for (j in 1:length(diseases.with.costs)) {
      disease <- diseases.with.costs[j]
      cost.per.case.col <- paste0("cost.per.case_", disease)
      px.col <- paste0("px_num_diff_", disease)
      cost.col <- paste0("costs.saved.", rate, "_", disease)
      
      output_df_year <- output_df_year %>%
        # multiply cost by -1 so it is a positive figure for savings
        mutate(!!cost.col := (get(cost.per.case.col) * get(px.col)) / 
                 ((1 + (rate / 100)) ^ year) * -1)
    }
  }
  

  ### Add columns for all cancers
  cancers <- c("brsc", "carc", "chml", "hanc", "lvrc", "mltm", "stmc", "tbalc", "utrc")
  output_df_year <- output_df_year %>%
    mutate(inc_num_diff_cancers = 
             select(., contains(cancers) & contains("inc_num_diff")) %>%
             rowSums(na.rm = TRUE),
           mx_num_diff_cancers = 
             select(., contains(cancers) & contains("mx_num_diff")) %>%
             rowSums(na.rm = TRUE),
           inc_num_bl_cancers = 
             select(., contains(cancers) & contains("inc_num_bl")) %>%
             rowSums(na.rm = TRUE),
           mx_num_bl_cancers = 
             select(., contains(cancers) & contains("mx_num_bl")) %>%
             rowSums(na.rm = TRUE))
  
  # Sum costs saved for cancers for each discount rate
  for (i in 1:length(discount.rates)) {
    costs.string <- paste0("costs.saved.", discount.rates[i])
    costs.cancer.col <- paste0("costs.saved.", discount.rates[i], 
                                  "_cancers")
    
    output_df_year <- output_df_year %>%
      mutate(!!costs.cancer.col := 
               select(., contains(cancers) & contains(costs.string)) %>%
               rowSums(na.rm = TRUE))
  }
  
  
  dataAll <- bind_rows( ### add age group all
    output_df_year,
    output_df_year %>% mutate(age_group_final='all'),
    output_df_year %>% mutate(sex='all'),
    output_df_year %>% mutate(age_group_final='all',sex='all')
  ) %>%
    dplyr::mutate(cohort=paste(sex, age_group, sep = "_"))
  

  # Life years and health adjusted life years ----
  
  # seed_current=1 #use when checking code, if not comment out
  options(scipen = 100)
  output_life_years_change <- dataAll %>%
    group_by(sex, age_group_final) %>%
    dplyr::select(age_group_final, sex, Lx_diff, Lwx_diff, Lx_bl, Lwx_bl,
                  contains("VSLY")) %>%
    summarise_if(is.numeric, funs(sum)) %>%
    dplyr::mutate(percent_diff_Lx=Lx_diff/Lx_bl,
                  percent_diff_Lwx=Lwx_diff/Lwx_bl) %>%
    ungroup() %>%
    # add columns that will become LY/HALY numbers per 1000
    mutate(Lx_diff_1000 = Lx_diff,
           Lwx_diff_1000 = Lwx_diff) %>%
    pivot_longer(cols=-c("sex", "age_group_final"),
                 names_to = "measure") %>%
    # join population, and calculate relevant measures by 1000
    left_join(populationLargeCohort, by=c('age_group_final', 'sex')) %>%
    relocate(population, .after = sex) %>%
    dplyr::mutate(value = if_else(
      measure %in% c("Lx_diff_1000", "Lwx_diff_1000") |
        startsWith(measure, "VSLY"),
      value / (population / 1000),
      value)) %>% 
    dplyr::mutate(measure=case_when(
      measure=="Lx_diff" ~ "Life years (LYs)",
      measure=="Lx_diff_1000" ~ "Life years (LYs), per 1000",
      measure=="Lwx_diff" ~ "Health adjusted life years (HALYs)",
      measure=="Lwx_diff_1000" ~ "Health adjusted life years (HALYs), per 1000",
      measure=="percent_diff_Lx" ~ "Life years % difference", 
      measure=="percent_diff_Lwx" ~ "Health adjusted life years % difference", 
      measure=="Lx_bl" ~ "Life years baseline",
      measure=="Lwx_bl" ~ "Health adjusted life years baseline",
      str_detect(measure, "VSLY") ~ 
        gsub("VSLY.(\\d)", # capture VSLY. plus digit (\\d), then '\\1' represents the captured group
             "Value of HALYs (discount rate \\1%), per 1000",
             measure),  
      TRUE ~ measure
    )) %>%
    dplyr::mutate(run=seed_current) %>%
    dplyr::rename(age=age_group_final)
  
  # Diseases deaths and incidence
  
  output_diseases_change <- dataAll %>% 
    dplyr::select(sex, age_group_final, contains(c("inc_num_diff", "mx_num_diff",
                                                  "inc_num_bl", "mx_num_bl",
                                                  "costs.saved")))  %>% 
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>% 
    mutate_all(function(x) ifelse(is.na(x), 0, x))  %>%  
    group_by(sex, age_group_final) %>%
    summarise_if(is.numeric, funs(sum), na.rm = TRUE) %>% 
    ungroup()
  
  # calculate percentages
  disease.snames <- c(DISEASE_SHORT_NAMES %>%
                       filter(is_not_dis == 0) %>%
                       .$sname,
                     "cancers")
  
  for(i in 1:length(disease.snames)) {
    diff_inc=paste0("inc_percent_diff_", disease.snames[i])
    inc_num=paste0("inc_num_diff_", disease.snames[i])
    inc_baseline=paste0("inc_num_bl_", disease.snames[i])
    
    diff_mx=paste0("mx_percent_diff_", disease.snames[i])
    mx_num=paste0("mx_num_diff_", disease.snames[i])
    mx_baseline=paste0("mx_num_bl_", disease.snames[i])
    
    output_diseases_change[, diff_inc] <- 
      output_diseases_change[,inc_num]/output_diseases_change[,inc_baseline]
    
    output_diseases_change[, diff_mx] <- 
      output_diseases_change[,mx_num]/output_diseases_change[,mx_baseline]
  }
    
  output_diseases_change <- output_diseases_change %>%
    dplyr::rename_with(~ gsub("inc_num", "inc.num", .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("mx_num", "mx.num", .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("inc_percent", "inc.percent", .x, fixed = TRUE)) %>% 
    dplyr::rename_with(~ gsub("mx_percent", "mx.percent", .x, fixed = TRUE)) %>%
    setNames(gsub("costs.saved.(\\d)_","costs.saved.per.1000.\\1%.disc_diff_",names(.))) %>%
    dplyr::select(-contains("bl")) %>%
    pivot_longer(cols=-c(sex,age_group_final),
                 names_to = c("measure","scenario","disease"),
                 names_sep="_")  %>%
    # join population, and calculate costs per 1000
    left_join(populationLargeCohort, by=c('age_group_final', 'sex')) %>%
    relocate(population, .after = sex) %>%
    mutate(value = if_else(
      startsWith(measure, "costs.saved"),
      value / (population / 1000),
      value)) %>%
    mutate(run=seed_current) %>%
    dplyr::rename(age=age_group_final) %>% 
    mutate_all(function(x) ifelse(is.na(x), 0, x)) 
  
  # Aggregate results over years
  output_df_agg <- dataAll %>%
    dplyr::select(sex, age_group_final, year, Lx_bl, Lx_sc, Lx_diff,
                  Lwx_bl, Lwx_sc, Lwx_diff, contains("num"), 
                  contains("VSLY"), contains("costs.saved")) %>%
    dplyr::select(-contains("px")) %>%
    group_by(year, sex, age_group_final) %>%
    summarise_if(is.numeric, sum, na.rm=T) %>%
    ungroup() %>%
    setNames(gsub("VSLY.(\\d)$", "VSLY.\\1_diff", names(.))) %>% 
    setNames(gsub("costs.saved.(\\d)_","costs.saved.per.1000.\\1%.disc_diff_",names(.))) %>%
    setNames(gsub("_sc$","_sc_all",names(.))) %>%
    setNames(gsub("_bl$","_bl_all",names(.))) %>%
    setNames(gsub("_diff$","_diff_all",names(.))) %>%
    rename_with(~ gsub("inc_num", "inc.num", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("mx_num", "mx.num", .x, fixed = TRUE)) %>%
    pivot_longer(cols=-c("year", "sex", "age_group_final"),
                 names_to = c("measure","scenario","disease"),
                 names_sep="_") %>%
    # join population, and calculate costs per 1000
    left_join(populationLargeCohort, by=c('age_group_final', 'sex')) %>%
    mutate(value = if_else(
      startsWith(measure, "costs.saved"),
      value / (population / 1000),
      value)) %>%
    dplyr::select(-population) %>%
    dplyr::mutate(run=seed_current) %>%
    dplyr::rename(age=age_group_final)
  
  
  ### Create directories
  
  # outputDir <- paste0(output_location,"/output_df/")
  # pifsDir <- paste0(output_location,"/pifs/")
  mmetsDir <- paste0(output_location,"/mmets/")
  outputDFaggDir <- paste0(output_location,"/output_df_agg/")
  lifeYearsDir <- paste0(output_location,"/life_years/")
  diseaseDir <- paste0(output_location,"/disease/")
  
  
  # dir.create(outputDir, recursive=TRUE, showWarnings=FALSE)
  # dir.create(pifsDir, recursive=TRUE, showWarnings=FALSE)
  # dir.create(mmetsDir, recursive=TRUE, showWarnings=FALSE)
  dir.create(outputDFaggDir, recursive=TRUE, showWarnings=FALSE)
  dir.create(lifeYearsDir, recursive=TRUE, showWarnings=FALSE)
  dir.create(diseaseDir, recursive=TRUE, showWarnings=FALSE)
  
  # write.csv(output_df, file=paste0(outputDir, seed_current, ".csv"), row.names=FALSE)
  # write.csv(pif[[2]], file=paste0(pifsDir, seed_current,".csv"), row.names=FALSE)
  # write.csv(mmets_pp, file=paste0(mmetsDir, seed_current,".csv"), row.names=FALSE)
  write.csv(output_df_agg, file=paste0(outputDFaggDir, seed_current,".csv"), row.names=FALSE)
  write.csv(output_life_years_change, file=paste0(lifeYearsDir, seed_current,".csv"), row.names=FALSE)
  write.csv(output_diseases_change, file=paste0(diseaseDir, seed_current,".csv"), row.names=FALSE)
  
  return(seed_current)
}





# ---- Prepare outputs  -------

# Function to combine outputs (combines excel files for each output and run)
combineOutputs <- function(inputDirectory, outputFile) {
  output_df_files<-list.files(inputDirectory,pattern="*.csv",full.names=T)
  output_df<-lapply(output_df_files,read.csv,header=T) %>%
    bind_rows(.id="run") %>%
    mutate(run=as.integer(run)) %>% 
    mutate(scen=scenarios_ShortTrips[i,]$scenario)
  saveRDS(output_df, file=outputFile)
}

# Functions to calculate summary statistics for diseases change, life year change and results over the life course

## Diseases

CalculateDisease <- function(inputDirectory) {
  # inputDirectory="C:/home/results/scenarioTripsReplace/melbourne-outputs-combined/disease"
  
  output_df_files <- list.files(inputDirectory,pattern="*.csv",full.names=T)
  
  file_location <- list.files(inputDirectory, pattern = "*.rds", full.names=T,recursive=T)
  
  combinedOutput <- ldply(file_location, readRDS)
  
  ## Calculate mean, median, sd, quantiles
  
  combinedOutputStats <- combinedOutput  %>%
    group_by(sex, age, measure, scenario, scen, disease) %>%
    dplyr::summarise(mean=mean(value,na.rm=T),sd=sd(value,na.rm=T),median=median(value,na.rm=T),
                     percentile025=quantile(value,probs=0.025, na.rm=T),
                     percentile975=quantile(value,probs=0.975, na.rm=T),
                     population=mean(population)) %>% 
    replace(is.na(.), 0)
  
}

## Life years

CalculateLifeYears <- function(inputDirectory) {
  # inputDirectory="C:/home/results/scenarioTripsReplace/melbourne-outputs-combined/LifeYears"
  
  output_df_files <- list.files(inputDirectory,pattern="*.csv",full.names=T)
  
  file_location <- list.files(inputDirectory, pattern = "*.rds", full.names=T,recursive=T)
  
  combinedOutput <- ldply(file_location, readRDS)
  
  ## Calculate mean, median, sd, quantiles
  
  combinedOutputStats <- combinedOutput  %>%
    group_by(sex, age, measure,scen) %>%
    dplyr::summarise(mean=mean(value,na.rm=T),sd=sd(value,na.rm=T),median=median(value,na.rm=T),
                     percentile025=quantile(value,probs=0.025, na.rm=T),
                     percentile975=quantile(value,probs=0.975, na.rm=T),
                     population=mean(population))
}

library(fst)

# Aggregate output
CalculateOutputAgg <- function(inputDirectory) {


  data <- readRDS(inputDirectory)
  combinedOutput <- data %>%
    dplyr::group_by(sex, age, measure, scenario, scen, disease, year) %>%
    dplyr::summarise(mean=mean(value,na.rm=T),sd=sd(value,na.rm=T),median=median(value,na.rm=T),
                     percentile025=quantile(value,probs=0.025, na.rm=T),
                     percentile975=quantile(value,probs=0.975, na.rm=T))
  
}


# Transport
summariseTransport <- function(inputFile,scenario_name="default") {
  # inputFile=scenarios_Melb[i,]$trips_location
  
  data <- read.csv(inputFile,as.is=T, fileEncoding="UTF-8-BOM")
  
  # select appropriate age field ('age' for Melbourne; 'age_band' for Brisbane)
  if ("age" %in% colnames(data)) {
    age_field <- "age"
  } else if ("age_band" %in% colnames(data)) {
    age_field <- "age_band"
  }
  
  data <- data %>%
    dplyr::select(participant_wt,all_of(age_field),sex,trip_mode_base,trip_mode_scen)
  
  if ("age" %in% colnames(data)) {  # Melbourne has 'age'
    data <- data %>%
      mutate(agegroup= case_when(
        age>=15 & age<=19 ~'15-19',
        age>=20 & age<=39 ~'20-39',
        age>=40 & age<=64 ~'40-64',
        age>=65           ~'65plus'))
  } else if ("age_band" %in% colnames(data))  {  # Brisbane has 'age_band'
    data <- data %>%
      mutate(agegroup = as.factor(case_when(
        age_band == "15-19 years"                      ~ "15-19" ,
        age_band %in% c("20-24 years", "25-29 years",
                        "30-34 years", "35-39 years")  ~ "20-39",
        age_band %in% c("40-44 years", "45-49 years",
                        "50-54 years", "55-59 years",
                        "60-64 years")                 ~ "40-64",
        age_band %in% c("65-69 years", "70-74 years",
                        "75-79 years", "80-84 years",
                        "85-89 years", "90-94 years",
                        "95-99 years", "100 years and over") ~ "65plus")))
  }
  
  data <- data %>%
    dplyr::select(age=agegroup,sex,participant_wt,bl=trip_mode_base,sc=trip_mode_scen)
  
  
  dataAll <- bind_rows(
    data,
    data%>%mutate(age='all'),
    data%>%mutate(sex='all'),
    data%>%mutate(age='all',sex='all')
  ) %>% pivot_longer(cols = c("bl", "sc"),
                     names_to = "scenario",
                     values_to = "mode")%>%
    mutate(scen=scenario_name) %>%
    as.data.frame()
  
  return(dataAll)
}

transportOucomes <- function(inputFile) {
  # inputFile=scenarioTrips  
  
  scenario_trips_weighted <- list()
  iage <- c(unique(scenarioTrips$age))
  isex <- c(unique(scenarioTrips$sex))
  iscenario <- c(unique(scenarioTrips$scenario))
  iscen <- c(unique(scenarioTrips$scen))
  index <- 1
  for (a in iage){
    for (s in isex){
      for (sc in iscenario){
        for (scena in iscen) {
          data <- scenarioTrips %>%
            dplyr::filter(age==a, sex==s, scenario==sc, scen==scena)
          mode_share <- data %>%
            srvyr::as_survey_design(weights = participant_wt) %>%
            group_by(mode) %>%
            dplyr::summarize(prop= srvyr::survey_mean(na.rm = T),
                             trips = srvyr::survey_total(na.rm = T),
                             surveyed = srvyr::unweighted(dplyr::n())) %>%
            ungroup() %>%
            dplyr::mutate(age=a,
                          sex=s,
                          scenario=sc,
                          scen=scena) %>%
            dplyr::select(age, sex, scenario, scen, prop, mode)
          scenario_trips_weighted[[index]] <- mode_share
          index <- index +1}}}}
  
  scenarioTrips<- bind_rows(scenario_trips_weighted) 
}

# Physical activity

## Physical activity summarised outputs

PAOutcomes <- function(inputFile) {
  # inputFile=scenarioLocation
  PA_files<-list.files(inputFile,pattern="*.csv",full.names=T)
  data<-lapply(PA_files,read.csv,header=T) %>%
    bind_rows() 
  
  # select appropriate age field ('age' for Melbourne; 'age_band' for Brisbane)
  if ("age" %in% colnames(data)) {
    age_field <- "age"
  } else if ("age_band" %in% colnames(data)) {
    age_field <- "age_band"
  }
  
  data <- data %>%
    dplyr::select(participant_wt,all_of(age_field),sex,ses,mod_total_hr:scen)
  
  if ("age" %in% colnames(data)) {  # Melbourne has 'age'
    data <- data %>%
      mutate(agegroup= case_when(
        age>=15 & age<=19 ~'15-19',
        age>=20 & age<=39 ~'20-39',
        age>=40 & age<=64 ~'40-64',
        age>=65           ~'65plus'))
  } else if ("age_band" %in% colnames(data))  {  # Brisbane has 'age_band'
    data <- data %>%
      mutate(agegroup = as.factor(case_when(
        age_band == "15-19 years"                      ~ "15-19" ,
        age_band %in% c("20-24 years", "25-29 years",
                        "30-34 years", "35-39 years")  ~ "20-39",
        age_band %in% c("40-44 years", "45-49 years",
                        "50-54 years", "55-59 years",
                        "60-64 years")                 ~ "40-64",
        age_band %in% c("65-69 years", "70-74 years",
                        "75-79 years", "80-84 years",
                        "85-89 years", "90-94 years",
                        "95-99 years", "100 years and over") ~ "65plus")))
  }
  
  data <- data %>%  
    dplyr::select(participant_wt,age=agegroup,sex,ses,mod_total_hr:scen)
  
  dataAll <- bind_rows(
    data,
    data%>%mutate(age='all'),
    data%>%mutate(sex='all'),
    data%>%mutate(age='all',sex='all')) 
  
  ## Create weighted stats
  PAall_weighted  <- dataAll %>%
   dplyr::filter(participant_wt!=0)  %>%## Some weights had 0 value
    srvyr::as_survey_design(weights = participant_wt)%>%
    group_by(age, sex, scen) %>%
    dplyr::summarize(walk_base= srvyr::survey_mean(time_base_walking*60),
                     walk_scen = srvyr::survey_mean(time_scen_walking*60),
                     cycle_base= srvyr::survey_mean(time_base_bicycle*60),
                     cycle_scen = srvyr::survey_mean(time_scen_bicycle*60))
  

  
  #### Meets guidelines
  PA_weighted  <- dataAll %>% 
    dplyr::mutate(meets_pa_base=ifelse((time_base_walking + walk_rc + time_base_bicycle*2 + mod_leis_hr + vig_leis_hr*2)*60 >= 150, 1, 0), 
                  meets_pa_scen= ifelse((time_scen_walking + walk_rc + time_scen_bicycle*2 + mod_leis_hr + vig_leis_hr*2)*60>= 150, 1, 0))
  
  PA_guide_weighted  <- PA_weighted %>%
    dplyr::filter(participant_wt!=0)  %>%## Some weights had 0 value
    srvyr::as_survey_design(weights = participant_wt)%>%
    group_by(age, sex, scen) %>%
    dplyr::summarize(meets_base= srvyr::survey_mean(meets_pa_base, na.rm = T), 
                     meets_scen = srvyr::survey_mean(meets_pa_scen, na.rm = T))
  

  
  return(list(PAall=PAall_weighted, PAallGuide=PA_guide_weighted))
  }