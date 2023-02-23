library(tidyverse)
library(readxl)
library(rprojroot)

setwd("~/enforcement/")

make_path <- is_git_root$make_fix_file()
out_path <- make_path("analysis/build/")

# need to read clean data
df <- read_xlsx("data/data_clean_company_jan_27.xlsx")

################################ ask claire ########################################

# *** Over one-third of the total number of observations (37.3%) are high-risk offenses

# Each group’s average fine is significantly different from the average fine of the overall dataset at a 10% confidence level
# 90% confidence level?

# In AB, 61% of non-individual offenders receive warnings; in ON, 84% are convicted in court; 
# and in QC, 90% receive administrative penalties
company_prop_prov_penatly <- df %>% filter(offender_type != "individual") %>% 
  group_by(province, penalty_type) %>% 
  summarise(count = n()) %>% 
  mutate(sum = sum(count)) %>% 
  mutate(percent = count / sum)
############ numbers are a bit different here 

############################ still need to check ####################################

# The majority of the observations (67%) occurred from 2012-2020

# The overwhelming majority, 92.7%, of the observations come from four provinces: BC, QC, AB, and PE. 

# Forty pollutant types were observed, with varying proportions between provinces, but the most abundant are smoke, dust, particulate matter, and combinations of pollutants. 
# All others each comprised less than 1% of observations

# AB, FD, NB, ON, and QC enforced violations mostly committed by companies (50%+ of observations), while BC, NS, and PE enforced violations committed mostly by individuals (75%+ of observations). 
# The majority of enforcement actions in SK involved violations committed by municipalities (>75% of observations)

################################## checked #########################################

# This process yielded a dataset of 2,217 observations
nrow(df)
# good

# Overall landscape of observations across provinces is dominated by burning offenses (74.3%, measured by keyword) 
# and individual offenders (62.8%, measured by offender type)
df %>% group_by(keyword) %>% summarise(count = n())
1647 / 2217
df %>% group_by(offender_type) %>% summarise(count = n())
1392 / 2217
# good

# CACs collectively make up 4.87% of the observations, not including emissions that may contain CACs, like smoke or dust.
# good

# Almost all observations (92%) represent single violations.
df %>% filter(number_of_violations > 1) %>% nrow()
167 / 2217
# good

# The remaining 8% represent multiple violations, where more than one violation is bundled into a single enforcement action
# Half of these are concentrated in Alberta, and half are dispersed among other provinces. 
df %>% filter(number_of_violations > 1, province == "AB") %>% nrow()
81 / 167
# good 

# The sum of all fines in the database totals $16,277,516 CAD and there were no jail sentences issued. 
df_fines %>% 
  filter(fine_imposed != "NA") %>% 
  mutate(fine_imposed = as.numeric(fine_imposed)) %>% 
  summarise(sum = sum(fine_imposed))
# good

# Over half of the observations come from BC, although most of these enforcement actions are tickets for burning offenses 
df %>% group_by(province) %>% summarise(count = n())
# good

# Ontario has surprisingly few observations (69)
# good

# PE has a seemingly high number of observations (204)
# good

# In BC, NS, PE, and SK, the most common offense is burning, while the federal offense (one observation) is an emissions offense
prov_key <- df %>% group_by(province, keyword) %>% summarise(count = n())
# good

# All observations in QC, NS, and FD involve a monetary penalty. 
prov_penalty <- df %>% group_by(province, penalty_type) %>% summarise(count = n())
# good

# Over half of AB’s enforcement actions are warnings (52%). 
# good

# ON and FD have the highest proportions of court convictions. 
prov_penatly_prop <- prov_penalty %>% 
  group_by(province) %>% 
  mutate(sum = sum(count)) %>% 
  mutate(percent = count / sum)
# good

# In AB, ON, and QC, >50% of observations concern non-individual offenders.
prop_off_prov <- df %>% group_by(province, offender_type) %>% 
  summarise(count = n()) %>% 
  mutate(sum = sum(count)) %>% 
  mutate(percent = count / sum)
# good

# When enforcement agencies do levy fines, the median fine varies dramatically, ranging from $200 in PE up to $62,774 in ON (Figure 5). Most provinces’ median fine is less than $3000. The minimum fine, $45, comes from an individual ticketed for burning in BC, while the maximum fine, $5.3 million, comes from a high-profile gas explosion in ON. 
# good see graph

# BC has the largest proportion of burn observations and consistently tickets $345 for those burns, which is the median shown
# good see graph 

# Most observations involve individual offenders (62.8%) and burns (74.3%)
df %>% filter(offender_type == "individual") %>% nrow()
1392 / 2217
df %>% filter(keyword == "Burn") %>% nrow()
1647 / 2217
# good

# PE and SK also use letters in higher proportions (~23%) than other provinces, although much less so than AB. 
# good