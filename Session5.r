# Further data manipulation

library(tidyverse)

metabric <- read_csv("data/metabric_clinical_and_expression_data.csv")

# Chaining

patients <- filter(metabric,
                   Survival_status == "LIVING" & Survival_time > 120)
patients <- select(patients, 
                   Patient_ID,
                   Survival_time,
                   Tumour_stage,
                   Nottingham_prognostic_index)
patients <- arrange(patients, desc(Survival_time))

# The pipe : %>%

patients <- filter(metabric,
                   Survival_status == "LIVING" & Survival_time > 120) %>%
  select(Patient_ID,
         Survival_time,
         Tumour_stage,
         Nottingham_prognostic_index) %>%
  arrange(desc(Survival_time))

# Creating new columns

# mutate 

metabric <- mutate(metabric,
                   Deceased = Survival_status == "DECEASED")

# Adjust an existing column

metabric <- mutate(metabric,
                   Tumour_size = Tumour_size / 10)

# Adjust/Mutate multiple columns at once

metabric %>%
  mutate(across(c(Age_at_diagnosis, Survival_time), round)) %>%
  select(Patient_ID, Age_at_diagnosis, Survival_time)
    
metabric %>%
  select(ends_with("_status")) %>%
  mutate(across(ends_with("_status"), as.factor))

# Summarise

mean(metabric$ESR1)

summarise(metabric, mean(ESR1))

summarise(metabric, ESR1_mean = mean(ESR1))

summarise(metabric,
          ESR1_mean = mean(ESR1),
          ESR1_sd = sd(ESR1))                

# Applying grouping to summarise

metabric %>%
  group_by(ER_status) %>%
  summarise(ESR1_mean = mean(ESR1))

metabric %>%
  summarise(ESR1_mean = mean(ESR1), .by = ER_status)

# Summarise by multiple columns

metabric %>%
  summarise(ESR1_mean = mean(ESR1), .by = c(ER_status, PR_status))

# top_n to get the top observations with a given value

metabric %>%
  summarise(ESR1_mean = mean(ESR1), .by = Cancer_type) %>%
  top_n(n = 2, ESR1_mean)

# getting the count of observations with n()

metabric %>%
  summarise(ESR1_mean = mean(ESR1),
            Number_of_Samples = n(),
            .by = Cancer_type) 

# Counting with count

metabric %>%
  count(Cancer_type)

metabric %>%
  count(Cancer_type, ER_status)

metabric %>%
  count(Cancer_type, ER_status, name = "Number_of_Samples")

# Saving the table

myTable <- metabric %>%
  filter(Survival_status == "LIVING" & Survival_time > 120) %>%
  select(Patient_ID, ER_status, Cancer_type, Survival_time) %>%
  arrange(Survival_time)

write_csv(myTable, "My_new_table.csv")

# Facetting with ggplot

ggplot(data = metabric,
       mapping = aes(x = GATA3, y = ESR1, colour = PR_status)) +
  geom_point(size = 0.7) +
  facet_wrap(vars(PR_status))

# Facet on 3-gene_classifier

ggplot(data = metabric,
       mapping = aes(x = GATA3,
                     ESR1,
                     colour = `3-gene_classifier`)) +
  geom_point(size = 0.7) +
  facet_wrap(vars(`3-gene_classifier`), nrow = 1)

ggplot(data = metabric,
       mapping = aes(x = GATA3,
                     ESR1,
                     colour = ER_status)) +
  geom_point(size = 0.7) +
  facet_grid(rows = vars(Neoplasm_histologic_grade),
             cols = vars(HER2_status))
