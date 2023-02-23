library(tidyverse)
library(readxl)
library(rprojroot)

setwd("~/enforcement/")

make_path <- is_git_root$make_fix_file()
out_path <- make_path("analysis/build/")

df <- read_xlsx("data/data_clean_company_jan_27.xlsx")

province_totals <- df %>% group_by(province) %>% summarise(count = n())

ggplot(df, aes(province))+
  geom_bar(aes(fill=as.factor(keyword)), position="fill") +
  labs(title = "Percentage of observations by keyword",
       subtitle = "All jurisdictions (BC, AB, QC, ON, SK, NB, PE, NS, FD)",
       x = "Provience",
       y = "Percentage of observations",
       fill = "Keyword") +
  annotate(geom="text", x=1, y=1.03, label=province_totals[1,2],
           color="black") +
  annotate(geom="text", x=2, y=1.03, label=province_totals[2,2],
           color="black") +
  annotate(geom="text", x=3, y=1.03, label=province_totals[3,2],
           color="black") +
  annotate(geom="text", x=4, y=1.03, label=province_totals[4,2],
           color="black") +
  annotate(geom="text", x=5, y=1.03, label=province_totals[5,2],
           color="black") +
  annotate(geom="text", x=6, y=1.03, label=province_totals[6,2],
           color="black") +
  annotate(geom="text", x=7, y=1.03, label=province_totals[7,2],
           color="black") +
  annotate(geom="text", x=8, y=1.03, label=province_totals[8,2],
           color="black") +
  annotate(geom="text", x=9, y=1.03, label=province_totals[9,2],
           color="black") +
  annotate(geom="text", x=11.3, y=1.03, label= "Number of observations",
           color="black") +
  coord_cartesian(xlim = c(1, 9),
                  ylim = c(0, 1),
                  clip = 'off') +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 10),
        legend.key.size = unit(1, "lines")) +
  scale_y_continuous(labels = scales::percent)
ggsave(str_c(out_path, "/percent_keyword_by_province.png"),
       width = 7, height = 4
)

ggplot(df, aes(province))+
  geom_bar(aes(fill=as.factor(offender_type)), position="fill") +
  labs(title = "Percentage of observations by offender type",
       subtitle = "All jurisdictions (BC, AB, QC, ON, SK, NB, PE, NS, FD)",
       x = "Provience",
       y = "Percentage of observations",
       fill = "Offender type") +
  scale_y_continuous(labels = scales::percent)

ggsave(str_c(out_path, "/percent_offender_by_province.png"),
       width = 7, height = 4
)

df %>% 
  mutate(penalty_type = ifelse(penalty_type == "multiple", "Multiple", penalty_type)) %>% 
  mutate(penalty_type = factor(penalty_type, levels = c(
    "Administrative Penalty", "Fine", "Ticket", "Order", "Enforcement Order",
    "Environmental Protection Order", "Information Order", "Pollution Abatement Order",
    "Pollution Prevention Order", "Court Conviction", "Open Court Proceeding",
    "Restorative Justice", "Warning","Long-form Information", "Multiple"))) %>% 
  ggplot(aes(province))+
  geom_bar(aes(fill=as.factor(penalty_type)), position="fill",
           colour="black") +
  labs(title = "Percentage of observations by penatly type",
       subtitle = "All jurisdictions (BC, AB, QC, ON, SK, NB, PE, NS, FD)",
       x = "Provience",
       y = "Percentage of observations",
       fill = "Penatly type") +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 10),
        legend.key.size = unit(.7, "lines")) +
  scale_y_continuous(labels = scales::percent)
ggsave(str_c(out_path, "/percent_penatly_by_province.png"),
       width = 7, height = 4
)

df %>%
  filter(fine_imposed != "NA") %>% 
  mutate(fine_imposed = as.numeric(fine_imposed)) %>%
  mutate(fine_imposed = log(fine_imposed)) %>% 
  ggplot(aes(province, fine_imposed)) +
  geom_boxplot(alpha = 0.3) +
  scale_y_continuous(breaks = log(c(500,3000,10000,50000,100000,250000,500000,6000000)), 
                     labels = c("$500","$3,000","$10,000","$50,000","$100,000","$250,000","$500,000", "$6,000,000")) +
  labs(title = "Fines by province",
       subtitle = "All jurisdictions (BC, AB, QC, ON, SK, NB, PE, NS, FD)",
       x = "Provience",
       y = "Fines imposed (CAD)") 
ggsave(str_c(out_path, "/fines_by_province.png"),
       width = 7, height = 4
)

df %>%
  filter(fine_imposed != "NA") %>% 
  filter(keyword != "Order") %>% 
  mutate(fine_imposed = as.numeric(fine_imposed)) %>%
  mutate(fine_imposed = log(fine_imposed)) %>%
  ggplot(aes(keyword, fine_imposed)) +
  geom_boxplot(alpha = 0.3) +
  scale_y_continuous(breaks = log(c(500,3000,10000,50000,100000,250000,500000,6000000)), 
                     labels = c("$500","$3,000","$10,000","$50,000","$100,000","$250,000","$500,000", "$6,000,000")) +
  labs(title = "Fines by keyword",
       subtitle = "All jurisdictions (BC, AB, QC, ON, SK, NB, PE, NS, FD)",
       x = "Keyword",
       y = "Fines imposed (CAD)") +
  theme(axis.text = element_text(size = 8))
ggsave(str_c(out_path, "/fines_by_keyword.png"),
       width = 7, height = 4
)
#unknown looks a little off - also had to remove "Order"?