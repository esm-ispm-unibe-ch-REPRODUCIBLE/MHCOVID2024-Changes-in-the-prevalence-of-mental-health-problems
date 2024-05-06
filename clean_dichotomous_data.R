#-------------------------------------------------------------------------------
# File  : clean_dichotomous_data.R
#
# Perform processing and data cleaning of dichotomous outcomes from MHCOVID
# 
#-------------------------------------------------------------------------------


#read data--------

outcomes1<- read_excel("in/outcomes.xlsx", na = "NA" )#data until March 2021
metadata1 <- read_excel("in/metadata.xlsx", na = "NA" )#data until March 2021
outcomes2 <- read_excel("in/outcomes_wave2.xlsx", na = "NA" )#data from March 2021 to August 2021
metadata2 <- read_excel("in/metadata_wave2.xlsx", na = "NA" )#data from March 2021 to August 2021
metadata2<-metadata2 %>% rename(author_1=first_author)


# the RoB in the second waved is measured at each timepoint and needs harmonisation before merging
rob<-metadata2 %>% select(starts_with("rob"))  %>%
  mutate_all(~(replace(., .== "Low risk",  0)))%>% 
  mutate_all(~(replace(., .== "Unclear risk",  1)))%>% 
  mutate_all(~(replace(., .== "High risk",  2)))
rob$rob_is_target_pop<-apply(rob %>% select(starts_with("rob2")), 1, max, na.rm=T)
rob$rob_non_bias<-apply(rob %>% select(starts_with("rob2")), 1, max, na.rm=T)
rob$rob_info_bias<-apply(rob %>% select(starts_with("rob3")), 1, max, na.rm=T)
rob<- rob %>%
        select(starts_with("rob_")) %>%
        mutate_all(~(replace(., .== 0,  "Low risk")))%>% 
        mutate_all(~(replace(., .== 1,  "Unclear risk")))%>% 
        mutate_all(~(replace(., .== 2,  "High risk")))

metadata2<- metadata2 %>% select(!starts_with("rob")) 
metadata2<-cbind.data.frame(metadata2,rob)
rm(rob)


# Merging!
outcomes1$timepoint<- as.Date(outcomes1$timepoint)
outcomes2$timepoint<- as.Date(outcomes2$timepoint)
outcomes1<-outcomes1 %>% select(!first_infection)%>% select(!delta)
outcomes=full_join(outcomes1,outcomes2)
metadata1<-metadata1 %>% select(!date_end)%>% select(!date_begin) %>% select(!starts_with("comment"))
metadata=full_join(select(metadata1, -pop_ethnicity),select(metadata2, -c(pop_ethnicity,n_timepoints)))
print(paste("Number of unique studies in the file outcomes.csv:", length(unique(outcomes$record_id))))


#Recoding, cleaning and creating variables in the merged data--------

#recode design
metadata<-metadata %>% mutate(study_design=recode(study_design, '0'="Cross-sectional at one timepoint",'1'="Cross-sectional at multiple timepoints",'2'="Longitudinal"))

# binary indicator: is the study population the same as the analysed population (as opposed to subgroup)
# e.g. study_population == "Children" & population == "Men"
outcomes$stpop <- as.numeric(outcomes$study_population == outcomes$population)

# clean study and subgroup population names
outcomes <- outcomes %>%
  mutate(study_population = replace(study_population, study_population == "Women,Adults", "Adults")) %>%
  mutate(population = replace(population, population == "Women,Adults", "Women"))

#get correct direction of the events
outcomes$diagnosed[outcomes$effect_direction==-1]<-outcomes$sample_size[outcomes$effect_direction==-1]-outcomes$diagnosed[outcomes$effect_direction==-1]
outcomes$effect_direction[outcomes$effect_direction==-1]<-1
outcomes$gini_2019[outcomes$country=="Hong Kong"]<-51.8  

# # Check how many studies for each population
# a<-as.data.frame(outcomes %>%
#                 group_by(population) %>%
#                 mutate(count = n_distinct(record_id)) %>%
#                 select(c(population, count)) %>%
#                 arrange(desc(count)) %>%
#                 distinct()) 
# kable(a,format = "markdown", digits = 3,
#       caption = "Available Studies/timepoints per condition in outcomes.csv",col.names = c("Condition", "Nr of Studies"))

outcomes <- outcomes %>%
  mutate(out_pop = ifelse(stpop == 0 & (population == "Men" | population == "Women"),
                          paste(population, study_population),
                          population))

# Verify that all `stpop` fields are non-NA before continuing
stopifnot(sum(is.na(outcomes$stpop)) == 0)

# Create a sex variable
outcomes$sex <- "Mixed"
outcomes$sex[grepl("Women", outcomes$out_pop)] <- "Women"
outcomes$sex[grepl("Men", outcomes$out_pop)] <- "Men"


#Select only studies with dichotomous outcome and longitudinal--------

# Get only the dichotomous outcomes
data.dich <- outcomes %>%
  filter(!is.na(diagnosed) & !is.na(sample_size)) %>%
  mutate(main_analysis = population == study_population)  # binary indicating rows for main/subgroup analysis
print(paste("Number of unique studies with dichotomous data:", length(unique(data.dich$record_id))))

# Add binary indicator for truly longitudinal studies
data.longi <- data.dich %>%
  group_by(record_id) %>% 
  mutate(is_longi = n_distinct(timepoint) > 1) 

data.dichlongi <-data.longi %>% filter(main_analysis == 1) %>% filter(is_longi) 
print(paste("Number of unique studies with dichotomous longitudinal data:", length(unique(data.dichlongi$record_id))))

# Join metadata to the dichotomous dataset
metadata <- metadata %>%
  select(-c(country,  num_invited, num_assessed, response_rate))
data.dichlongi <- data.dichlongi %>% select(!starts_with("rob_"))%>% 
  left_join(metadata, by = c("record_id"), keep = FALSE) %>%
  arrange(record_id)

# Further data wrangling -----

#select some variables only
data.dichlongi <- data.dichlongi %>% 
  select(record_id, doi,author_1,year,population, study_population,
         country, condition, scale, timepoint, n_timepoints, is_longi,
         is_prepandemic, diagnosed, sample_size, threshold, diag_group,
         stringency, days_after_first, days_after_pandemic, confirmed_cumulative,
         deaths_cumulative, confirmed_avg, deaths_avg, cumulative_stringency,
         country_population_2019,confirmed_per_100000, deaths_per_100000,
         confirmed_avg_per_100000, deaths_avg_per_100000, gdp_per_capita_2019,
         gini_2019, effect_direction, num_invited, num_assessed,
         response_rate,  stpop, out_pop, sex,
         main_analysis, pop0_name,tag_num_timepoints, study_design,
         method_recruitment, method_collection, multi_country, pop0_central_age,
         pop0_sample_size, pop0_mean_med, pop0_sd, pop0_min_age, pop0_max_age,
         pop0_percent_female, pop_percent_phys_con, pop_percent_psych_con,
         pop_percent_covid19, starts_with("rob_"))

data.dichlongi$condition[data.dichlongi$condition=="Other (specify)"]<-"ADHD"

# Augment the dataset further
anxiety.conditions <- c("Social anxiety","Generalized Anxiety Disorder", "Panic/somatic symptoms", "Panic disorder")
data <- data.dichlongi %>%
  mutate(exact_days_after_first = days_after_first) %>%
  mutate(days_after_first = ifelse(days_after_first < 0, 0, days_after_first)) %>%
  mutate(condition = ifelse(condition %in% anxiety.conditions, "Anxiety", condition)) %>%
  mutate(gini_minus_min = gini_2019 - min(gini_2019)) %>%
  mutate(gdp_div_10000_minus_min = gdp_per_capita_2019 / 10000 - min(gdp_per_capita_2019 / 10000)) %>%
  mutate(log_cumulative_stringency = log(cumulative_stringency + 1)) %>%
  mutate(sqrt_cumulative_stringency = sqrt(cumulative_stringency)) %>%
  mutate(logconfirmed_cumulative100k = log(confirmed_cumulative / country_population_2019 * 100000 + 1)) %>%
  mutate(logdeaths_cumulative100k = log(deaths_cumulative / country_population_2019 * 100000 + 1)) %>%
  mutate(sqrtconfirmed_cumulative100k = sqrt(confirmed_cumulative / country_population_2019 * 100000)) %>%
  mutate(sqrtdeaths_cumulative100k = sqrt(deaths_cumulative / country_population_2019 * 100000)) %>%
  mutate(days_after_pandemic = ifelse(is.na(days_after_pandemic), days_after_first, days_after_pandemic))

# Re-encode RoB responses as numerics
rob.map <- data.frame(value = c(0:2), row.names = c("Low risk", "Unclear risk", "High risk"))
rob <- data %>%
  select(c(record_id, condition, sample_size, rob_info_bias, rob_is_target_pop, rob_non_bias)) %>%
  mutate(rob_info_bias = rob.map[rob_info_bias, "value"]) %>%
  mutate(rob_is_target_pop = rob.map[rob_is_target_pop, "value"]) %>%
  mutate(rob_non_bias = rob.map[rob_non_bias, "value"])
# set the risk of bias as the maximum entered for any condition within a study
rob <- ungroup(rob) %>%
  group_by(record_id, condition) %>%
  group_modify(~ mutate(.x, rob_info_bias = max(rob_info_bias, na.rm =T))) %>%
  group_modify(~ mutate(.x, rob_non_bias = max(rob_non_bias, na.rm=T))) %>%
  group_modify(~ mutate(.x, rob_is_target_pop = max(rob_is_target_pop, na.rm=T)))


rob<-rob %>% mutate(rob_info_bias=recode(rob_info_bias, 
                                     `1`="Unclear risk",
                                     `2`="High risk",
                                     `0`="Low risk"),
               rob_non_bias=recode(rob_non_bias, 
                                    `1`="Unclear risk",
                                    `2`="High risk",
                                    `0`="Low risk"),
               rob_is_target_pop=recode(rob_is_target_pop, 
                                    `1`="Unclear risk",
                                    `2`="High risk",
                                    `0`="Low risk")
               )

data=bind_cols(
          select(data,-rob_info_bias,-rob_is_target_pop,-rob_non_bias), 
          select(ungroup(rob),rob_info_bias,rob_is_target_pop,rob_non_bias)) %>%
          group_by(record_id,condition)


# Add country-specific binary indicators
data$USA   <- as.numeric(data$country == "United States")
data$CHINA <- as.numeric(data$country == "China")

data$country[data$country=="United States, France, Germany, Italy, Spain, United Kingdom"]<-"Many countries"
data$country[data$country=="Iran, Islamic Republic of"]<-"Iran"
data$country[data$country=="Taiwan, Province of China"]<-"Taiwan"

# Clean some incorrectly formatted characters from excel
data$author_1[data$author_1=="Br√§scher"]<-"Braescher"
data$author_1[data$author_1=="Gim√©nez-Das√≠"]<-"Gimenez-Dasi"
data$author_1[data$author_1=="Sch√§fer"]<-"Schaefer"
data$author_1[data$author_1=="Sch√ºtzwohl"]<-"Schuetzwohl"

# Add column with Author Year format
data$authoryear <- paste(data$author_1,data$year)
data <- data %>%
  mutate(description_of_diagnosis = coalesce(as.character(threshold), diag_group))

# Within each study ID, age and sex group (population), timepoint and condition, we select the minimum threshold. If missing, we keep it.
data <- data %>%
  group_by(record_id,condition, population, timepoint) %>%
  group_modify(~ mutate(.x, min_thresh = min(threshold))) %>%
  filter(min_thresh == threshold | is.na(min_thresh))


data$condition<-as.character(data$condition)
data$condition[data$condition == "generalized anxiety disorder"] <-"anxiety"
data$condition[data$condition == "panic/somatic symptom"]<-"panic disorder"
data$condition[data$condition == "paranoid ideation"]<-"psychotic symptoms"
data$condition[data$condition == "psychoticism"]<-"psychotic symptoms"
data$condition[data$condition == "strengths and difficulties"]<-"psychological distress"
data$condition[data$condition == "somatization"]<-"somatoform disorder"
data$condition<-toTitleCase(data$condition)

#harmonize the names of the scales
data$scale[data$scale == "Alcohol Use Disorders Identification Test, Consumption Subscale (AUDIT-C)"]<-"Alcohol Use Disorder Identification Test Consumption (AUDIT-C)"
data$scale[data$scale == "CAGE-AID" ]<-"CAGE-AID Substance Abuse Screening Tool"
data$scale[data$scale == "Center for Epidemiologic Studies Depression Scale, 10 items (CES-D-10)"]   <-  "Center for Epidemiologic Studies Depression Scale (CES-D)"                
data$scale[data$scale == "GAD-2 (Generalized Anxiety Disorder Scale, 2 items)"]<-"Generalized Anxiety Disorder Scale, 2 items (GAD-2)"
data$scale[data$scale == "GAD-7 and PHQ-9"] <- "GAD-7/PHQ-9"                                                              
data$scale[data$scale == "MHI-5"]  <-"Mental Health Inventory-5 (MHI-5)"
data$scale[data$scale == "PHQ"]<-"Patient Health Questionnaire, 9 items (PHQ-9)"                                                                       
data$scale[data$scale == "PHQ-4 (Patient Health Questionnaire, 4 items)"] <-"Patient Health Questionnaire, 4 items (PHQ-4)" 
data$scale[data$scale == "the 5-item MHI-5 index"]<-"Mental Health Inventory-5 (MHI-5)"                                                   


data <- data %>%
  group_by(record_id,condition,population) %>% 
  group_modify(~ mutate(.x,nobs=n())) %>%
  group_modify(~ mutate(.x,is_longi2=n_distinct(timepoint)>1))
data <- data %>% filter(nobs>1 & is_longi2==T) %>% select(-nobs, -is_longi2)

# The record_id "46238" measures Depression in one timepoint with "Patient Health Questionnaire, 9 items (PHQ-9)" 
# and in another timepoint with "Patient Health Questionnaire, 8 items (PHQ-8)". I harmonised this otherwise there are problems in analysis. 
data<-data%>% filter(record_id!=46238)#data measured with difference scales
data<-data%>% filter(record_id!=62122)#data entry mistake of a single timepoint study

# Add a column of having included the data in the AIM paper
contMHCOVID <-read_excel("in/MHCOVIDcont.xlsx")
dich<-sort(unique(data$record_id))
cont<-sort(unique(contMHCOVID$ID))
in.both<-dich[!is.na(match(dich,cont))]
data$previously.included <- data$record_id %in% in.both

#There is a study with multiple entries for the same timepoint and we need to group them
a<-data %>% filter(record_id==136418) 
a2<-a %>% group_by(timepoint) %>% group_modify(~ mutate(.x,sum_diag=sum(diagnosed))) %>% group_modify(~ mutate(.x,sum_n=sum(sample_size))) %>% group_modify(~ mutate(.x,n_timepoint_group=row_number()))
a$diagnosed<-a2$sum_diag
a$sample_size<-a2$sum_n
a<-a %>% filter(a2$n_timepoint_group==1)
data<-rbind.data.frame(data %>% filter(record_id!=136418),a)

#correct data entry mistake
data$diagnosed[data$record_id==95873 & data$timepoint=="2020-05-15" ]<-4960


#--------------------Write the complete "dichotomous dataset with longitudinal data------------------------------
# Write the complete "dichotomous dataset with longitudinal data
out.file <- paste0("out/", Sys.Date(), "_dichotomous_longit_outcomes.csv")
print(paste("Writing dichotomous longitudinal data to:", out.file))
write.csv(data, out.file, row.names = FALSE)
print(paste("Final Number of unique studies with dichotomous longitudinal data:", length(unique(data$record_id))))


sink("out/Number of studies and timepoints.txt")
TableStudiesTime.fun(data,mycaption="Number of longitudinal studies and timepoints for the dichotomous outcome")
sink()


