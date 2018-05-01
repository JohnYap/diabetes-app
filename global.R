## global.R ##

### READ DATASET
library(dplyr)

df = read.csv("./data/diabetes_n.csv")

df = df %>%
  mutate(., study_type = ifelse(study_type == "Interventional", "Clinical Trial", "Observational Study"))

### SIDEBAR MENU ITEMS
choice_map = c("All","US Fed", "NIH", "Industry", "Other")
choice_study_info = c("Sponsor Type", "Condition Type", "Intervention Type", "Status", "Enrollment", "Phase (Clinical Trial)", "Duration")
choice_sponsor = c("sponsor_type", "sponsor", "sponsor_num")
choice_time = c("Studies by Start Year", "Studies by Completion Year")

#### SPONSOR TABLE

# duration
df_duration = df %>% 
  filter(., !is.na(duration)) %>%
  group_by(., sponsor) %>% 
  summarise(., 
            duration.mean = round(mean(duration), 1), 
            duration.min = round(min(duration), 1), 
            duration.max = round(max(duration), 1)) 

df_enrollment = df %>%
  filter(., !is.na(enrollment)) %>%
  group_by(., sponsor) %>% 
  summarise(.,   
            enrollment.total = sum(enrollment), 
            enrollment.mean = round(mean(enrollment), 0), 
            enrollment.min = min(enrollment), 
            enrollment.max = max(enrollment))
            
df_num_studies = df %>% 
  group_by(., sponsor) %>% 
  summarise(., studies.total = n()) 

df_1 = inner_join(df_num_studies, df_enrollment, by = "sponsor")
df_sponsor = inner_join(df_1, df_duration, by = "sponsor")

