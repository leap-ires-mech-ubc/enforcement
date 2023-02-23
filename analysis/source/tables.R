library(tidyverse)
library(readxl)
library(rprojroot)

setwd("~/enforcement/")

make_path <- is_git_root$make_fix_file()
out_path <- make_path("analysis/build/")

# need to read clean data
df <- read_xlsx("data/data_clean_company_jan_27.xlsx")

fines_standard <- function(df_fines) {
  df_fines %>% 
    filter(fine_imposed != "NA") %>% 
    mutate(fine_imposed = as.numeric(fine_imposed))
}

df_fines <- fines_standard(df) 
y <- df_fines$fine_imposed

# CACs
criteria_air_contaminats <- c("nitrogen oxide",
                              "sulphur dioxide",
                              "carbon monoxide",
                              "ammonia",
                              "particulate matter",
                              "VOC")
cac <- df %>% filter(pollutant_type %in% criteria_air_contaminats)

nrow(cac)
# 108
108 / 2217
# 49%
length(unique(cac$offender_name))
# 86

table_5_summaries <- function(df_summary) {
  df_summary %>% 
    filter(fine_imposed != "NA") %>% 
    mutate(fine_imposed = as.numeric(fine_imposed)) %>% 
    summarise(median(fine_imposed),
              mean(fine_imposed))
}

table_5_summaries(cac)
# 10,000
# 16,869 
cac_fines <- fines_standard(cac)
x <- cac_fines$fine_imposed
t.test(x, y, conf.level = .1)
# should i do the t tests?

# repeat offenders
repeat_off <- df %>% 
  filter(offender_type == "company") %>%
  filter(offender_name != "LOGGING",
         offender_name != "Non-Incorporated Entity",
         offender_name != "Unpublished") %>% 
  group_by(offender_name) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
sum(repeat_off$count)
# 288
288 / 2217
# 13%
nrow(repeat_off)
# 95

repeat_off_names <- unique(repeat_off$offender_name)
df %>% 
  filter(offender_name %in% repeat_off_names) %>% 
  table_5_summaries(.)
# 3,500
# 32,893

# across provincial
# double check this math
across_prov1 <- df %>% 
  filter(offender_type == "company") %>% 
  group_by(offender_name, province) %>% 
  summarise(count = n()) 
across_prov2 <- across_prov1 %>% 
  group_by(offender_name) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
across_prov_names <- unique(across_prov2$offender_name)
across_prov3 <- across_prov1 %>% 
  filter(offender_name %in% across_prov_names)
nrow(across_prov2)
#not good (13 me, 10 claire)
sum(across_prov3$count)
#not good (28 me, 50 claire)
57 / 2217
df %>% 
  filter(offender_name %in% across_prov_names) %>% 
  table_5_summaries(.)
# 5,000
# 82,753

#NPRRI
npri <- df %>% filter(report_to_NPRI == "yes")
nrow(npri)
# 272
length(unique(npri$offender_name))
# 154
272 / 2217
# 12.3
table_5_summaries(npri)
# 8,500
# 44,68

#NPRI top 20
npri_20 <- df %>% filter(NPRI_top_20 == "yes")
nrow(npri_20)
# 50
50 / 2217
# 2.3%
length(unique(npri_20$offender_name))
# 24
table_5_summaries(npri_20)
# 4,000
# 53,183

# non-individuals
non_ind <- df %>% 
  filter(offender_type != "individual")
nrow(non_ind)
# 825
825 / 2217
# 37.2%
length(unique(non_ind$offender_name))
# 612
table_5_summaries(non_ind)

df_coded <- df %>% 
  mutate(cac = ifelse(pollutant_type %in% criteria_air_contaminats,1,0)) %>% 
  mutate(repeat_off = ifelse(offender_name %in% repeat_off_names,1,0)) %>% 
  mutate(npri = ifelse(report_to_NPRI == "yes", 1, 0)) %>% 
  mutate(non_ind = ifelse(offender_type != "individual",1,0))
high_risk <- df_coded %>% 
  filter(cac == 1 | repeat_off == 1 | npri == 1 | non_ind == 1)
nrow(high_risk)
# 825
829 / 2217
# 37.4%
length(unique(high_risk$offender_name))
# 612
table_5_summaries(high_risk)
# 3,500
# 28,334
