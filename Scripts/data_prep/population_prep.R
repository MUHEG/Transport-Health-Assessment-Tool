### Estimated population is from ABS regional population estimates for 2019, in
### 5 year age bands.  However, the top age band in the estimates is 85+, without
### further breakdown.  Census data from 2016 and 2021 is used to estimate
### breakdown of the 85+ band.

GetPopulation <- function(population_estimates,
                          census_population_before,
                          census_population_after,
                          location) {
  
  # population_estimates = "Data/original/abs/population_estimates_age_sex_2001_to_2021.xlsx"
  # census_population_before = "Data/original/abs/population_census_GCCSA_2016.xlsx"
  # census_population_after = "Data/original/abs/population_census_GCCSA_2021.xlsx"
  # location = "Greater Brisbane"  # or other area of interest - use "Total" for whole of Australia
  
  # 1. From 2016 and 2021 census populations, get breakdown of 85+ agegroup
  # in 5 year groups, so breakdown can be applied to 2019 estimates
  # ---------------------------------------------------------------------------#
  # function to extract proportions in each group within 85+ from census
  census_proportions_85_and_over <- function(census) {
    return(
      readxl::read_xlsx(census, 
                        sheet = "Data Sheet 0") %>%
        # column names from rows 8 & 7; omit non-data first column and rows at beginning and end
        setNames(., c(.[8, 1:3], .[7, 4:ncol(.)])) %>%
        .[9:(nrow(.)-7), -1] %>%
        
        # select age, sex, location
        dplyr::rename(age = 1, sex = 2) %>%
        dplyr::select(age, sex, population = all_of(location)) %>%
        
        # every second row has missing age group
        mutate(age = ifelse(is.na(age), lag(age), age)) %>%
        
        # for groups 85+, find proportion of total 85+ in each group
        filter(age %in% c("85-89 years", "90-94 years",
                          "95-99 years", "100 years and over")) %>%
        group_by(sex) %>%
        mutate(proportion = as.numeric(population) / sum(as.numeric(population))) %>%
        ungroup()
    )
  }
  
  # extract the proprtions for the 2016 (before) and 2021 (after) censues
  census_proportions_before <- census_proportions_85_and_over(census_population_before)
  census_proportions_after <- census_proportions_85_and_over(census_population_after)
  
  # find mean proportions
  census_proportions_mean <- census_proportions_before %>%
    left_join(census_proportions_after,
              by = c("age", "sex"),
              suffix = c(".before", ".after")) %>%
    rowwise() %>%
    mutate(proportion.mean = (proportion.before + proportion.after) / 2) %>%
    dplyr::select(age, sex, proportion.mean) %>%
    mutate(sex = tolower(sex))
  
  
  # 2. Read in 2019 estimates, apportion 85+ group into 5 year groups based
  # on 2016 and 2021 census proportions, and produce output table
  # ---------------------------------------------------------------------------#
  # numeric column names in population estimates
  age.cols <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                "70-74", "75-79", "80-84", "85 and over")
  
  # function to read and extract population by sex and age from estimates
  getEstimates <- function(sex) {
    if (sex == "male") {
      sheet = "Table 1"
    } else if (sex == "female") {
      sheet = "Table 2"
    }
    # read and extract sex and age from estimates
    estimates <- readxl::read_xlsx(population_estimates, sheet = sheet) %>%
      # column names from rows 7 & 6; fix dash problems in age columns by 
      # inserting manually; omit non-data rows at beginning and end
      setNames(., c(.[7, 1:11], 
                    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                    "70-74", "75-79", "80-84", "85 and over",
                    .[6, 30])) %>%
      .[9:(nrow(.)-5),]
    
    # select relevant year and location
    if (location == "Total") {
      estimates <- estimates %>%
        filter(Year == "2019")
    } else {
      estimates <- estimates %>%
        filter(Year == "2019" & `GCCSA name` == location)
    }
    
    # sum ages
    estimates <- estimates %>%
      dplyr::select(any_of(age.cols)) %>%
      dplyr::summarise(across(everything(), ~sum(as.numeric(.)))) %>%
      
      # transpose, with ages as a column
      rbind(., names(.)) %>%
      t() %>%
      as.data.frame(row.names = FALSE) %>%
      dplyr::select(age_group = V2, population = V1)
    
    # apportion 85+ population
    ## total population 85+ for sex
    age_85_and_over_pop <- unlist(estimates[estimates$age_group == "85 and over",
                                            "population"]) %>%
      as.numeric()
    ## proportions for each 5 year band within 85+ group for sex
    age_bands_85_and_over <- census_proportions_mean %>%
      filter(sex == !!sex) %>%
      mutate(population = age_85_and_over_pop * proportion.mean) %>%
      # omit 100+ (not used)
      filter(age != "100 years and over") %>%
      # select relevant columns
      dplyr::select(age_group = age, population)
    
    # finalise output, replacing 85+ with 5 year bands
    estimates <- estimates %>%
      filter(age_group != "85 and over") %>%
      rbind(., age_bands_85_and_over) %>%
      # add sex column
      mutate(sex = sex)
    
    return(estimates)
    
  }
  
  estimates_male <- getEstimates("male")
  estimates_female <- getEstimates("female")
  
  population <- rbind(estimates_male, estimates_female) %>%
    # take first year of age_group and add 2
    # (\d+ is one or more digits (str_extract is first occurrence only, str_extract_all is all))
    mutate(from_age = as.numeric(str_extract(age_group, "\\d+")),
           age_cat = from_age + 2,
           sex_age_cat = paste(tolower(sex), age_cat, sep="_")) %>%
    dplyr::select(sex_age_cat, population)
  
  # 3. Return the output population
  # ---------------------------------------------------------------------------#
  return(population)
  
}

