# ============================================
# Digital Payments & Financial Inclusion
# Economic Impact Study — India 2024
# Data Source - AI Agents From Scratch
# Author: Soumya Thamke
# ============================================

# Load libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(writexl)

# --- STEP 1 : LOAD THE DATA ---
m=read_excel(file.choose())
my_file_path<-file.choose()
excel_sheets(my_file_path) # SHOWS ALL THE SHEETS IN EXCEL FILE

sheet1<-read_excel(my_file_path,sheet = 1)
sheet2<-read_excel(my_file_path,sheet=2)
sheet3<-read_excel(my_file_path,sheet=3)
sheet4<-read_excel(my_file_path,sheet=4)
sheet5<-read_excel(my_file_path,sheet=5)
sheet6<-read_excel(my_file_path,sheet=6)

# --- STEP 2 : CLEAN THE DATA ---
# CLEANING ACCORDING TO DATA QUALITY LOG SHEET MADE IN EXCEL FILE
colnames(sheet1)
#REMOVED THE COLUMNS THAT WERE NOT NECESSARY
sheet1<-select(sheet1,-data_entry_person,-recorded_date)
#FOUND THE NUMBERS IN , AND NOT NUMERIC
sheet2$transaction_amount_inr
#REPLACED , AND CHANGED TO NUMERIC
sheet2$transaction_amount_inr<-as.numeric(gsub(",","",sheet2$transaction_amount_inr))
sheet2$transaction_amount_inr

# IF IN CASE THE NUMBERS SHOW IN +9 FORM , USE "options(scipen = 999)"  
is.na(sheet3$digital_adoption_pct)
#NA VALUES FOUND
avg_digi<-mean(sheet3$digital_adoption_pct,na.rm=TRUE)
avg_digi

#NA VALUES REPLACED BY MEAN
sheet3$digital_adoption_pct[is.na(sheet3$digital_adoption_pct)]<-avg_digi
sheet3$digital_adoption_pct
sheet4$transaction_category

#STANDARDISED TRANSACTION CATEGORY
sheet4$transaction_category<-tools::toTitleCase(tolower(sheet4$transaction_category))
sheet4$transaction_category
colnames(sheet5)
colnames(is.na(sheet5))
names(sheet5)[sapply(sheet5,anyNA)]
sheet5$population_density_per_sqkm
sheet5$digital_adoption_pct
sheet5$null_flag

#NULL COLUMN REMOVED (null_flag)
sheet5<-sheet5 %>% select(-null_flag)
colnames(sheet5)
sheet5<-sheet5[!is.na(sheet5$population_density_per_sqkm),]
sheet5
names(sheet5)[sapply(sheet5, anyNA)]

#STANDARDISE THE DISTRICT NAMES
sheet6$district
sheet6$district<-tools::toTitleCase(tolower(sheet6$district))
sheet6$district

sheet2$state
#STANDARDISE THE STATE NAMES
sheet2$state<-tools::toTitleCase(tolower(sheet2$state))
sheet2$state

#NULL COLUMN (null_flag) REMOVED
sheet1<- sheet1 %>% select(-null_flag)
colnames(sheet1)

sheet1<- sheet1[sheet1$transaction_volume!=0,]
sheet1$transaction_volume

#FINDING THE MEAN
avg_vol<-mean(sheet3$transaction_volume)
sheet3$transaction_volume
#REPLACING THE NA VALUES WITH MEAN
sheet3[is.na(sheet3$transaction_volume),]
names(sheet3)[sapply(sheet3, anyNA)]
avg_vol<-mean(sheet3$transaction_volume,na.rm = TRUE)
avg_vol
sheet3$transaction_volume[is.na(sheet3$transaction_volume)]<-avg_vol
sheet3$transaction_volume

#REMOVED NULL COLUMN (null_flag)
names(sheet3)[sapply(sheet3,anyNA)]
sheet3<- sheet3 %>% select(-null_flag)
colnames(sheet3)
colnames(sheet5)
colnames(sheet6)

#REMOVED NULL COLUMN (null_flag)
sheet6<-sheet6 %>% select(-null_flag)
colnames(sheet6)
sheet6[is.na(sheet6$transaction_id),]

sum(is.na(sheet1))
sum(is.na(sheet2))
names(sheet2)[sapply(sheet2,anyNA)]

#REMOVED NULL COLUMN (null_flag)
sheet2<-sheet2 %>% select(-null_flag)
sum(is.na(sheet2))

sum(is.na(sheet3))
sum(is.na(sheet4))
names(sheet4)[sapply(sheet4,anyNA)]
sheet4$null_flag
sheet4$data_quality_flag
sum(is.na(sheet6))
sheet4<- sheet4 %>% select(-null_flag,-data_quality_flag)
colnames(sheet4)
sum(is.na(sheet5))

# --- STEP 3 : JOIN THE SHEETS 
df_combined<-bind_rows(sheet1,sheet2,sheet3,sheet4,sheet5,sheet6)
nrow(df_combined)
colnames(df_combined)
df_combined$state
table(df_combined$state)
table(df_combined$quarter)
unique(df_combined$state)

#TO STANDARDISE THE STATE NAMES
df_combined$state<-tools::toTitleCase(tolower(df_combined$state))
df_combined$state
unique(df_combined$state)

df_combined$district
unique(df_combined$district)

#TO STANDARDISE THE DISTRICT NAMES
df_combined$district<-tools::toTitleCase(tolower(df_combined$district))
df_combined$district
unique(df_combined$district)

names(df_combined)[sapply(df_combined, anyNA)]

# TO SAFELY USE THE SHEETS FOR LATER 
write_xlsx(df_combined,"df_combined.xlsx")
getwd()
install.packages("tidyverse")
install.packages(writexl)
write_xlsx(sheet1,"sheet1.xlsx")
write_xlsx(sheet2,"sheet2.xlsx")
write_xlsx(sheet3,"sheet3.xlsx")
write_xlsx(sheet4,"sheet4.xlsx")
write_xlsx(sheet5,"sheet5.xlsx")
write_xlsx(sheet6,"sheet6.xlsx")
getwd()

df_clean<-df_combined %>% select(-quarter_label,-region,-source_system)
sum(is.na(df_clean))
write_xlsx(df_clean,"df_clean.xlsx")
getwd()


# ---- STEP 4: Analysis ----
# Total volume and avg adoption by state

state_summary <- df_clean %>%
  group_by(state) %>%
  summarise(total_volume = sum(transaction_volume),
            avg_adoption = mean(digital_adoption_pct)) %>%
  arrange(desc(total_volume))

print(state_summary)

colnames(df_combined)
colnames(df_clean)
max(df_clean$digital_adoption_pct)
min(df_clean$digital_adoption_pct)

df_clean[which.max(df_clean$digital_adoption_pct),]
df_clean[which.min(df_clean$digital_adoption_pct),]
df_clean[which.min(df_clean$population_density_per_sqkm),]
df_clean[which.max(df_clean$population_density_per_sqkm),]

min(df_clean$population_density_per_sqkm)
max(df_clean$population_density_per_sqkm)

# High population low adoption districts
high_pop_low_adoption <- df_clean %>%
  filter(population_density_per_sqkm > 500,
         digital_adoption_pct < 20) %>%
  select(state, district, population_density_per_sqkm,
         digital_adoption_pct, transaction_volume) %>%
  arrange(digital_adoption_pct)

high_pop_low_adoption


# ---- STEP 5: Visualise ----
# Scatter — population density vs adoption
library(ggplot2)
ggplot(df_clean, aes(x = population_density_per_sqkm,
                     y = digital_adoption_pct,
                     color = state,
                     label = district)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(data = high_pop_low_adoption,
            aes(label = district),
            size = 3, vjust = -1, color = "red") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "red") +
  labs(title = "High Population, Low Adoption Districts — Priority Targets",
       x = "Population Density (per sq km)",
       y = "Digital Adoption (%)") +
theme_minimal()

# Bar chart — avg adoption by state
df_clean %>%
  group_by(state) %>%
  summarise(avg_adoption = mean(digital_adoption_pct)) %>%
  ggplot(aes(x = reorder(state, avg_adoption), 
             y = avg_adoption, fill = state)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Digital Adoption % by State",
       x = "State", y = "Adoption (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- STEP 6: Export ----

write_xlsx(
  list(
    "Cleaned Data"          = df_clean,
    "High Pop Low Adoption" = high_pop_low_adoption
  ),
  "Digital_Payments_Cleaned_Final.xlsx"
)
getwd()