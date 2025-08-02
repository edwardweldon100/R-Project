library(dplyr)
library(stringr)
data_salaries = read.csv('salaries.csv')
head(data_salaries)
data_salaries[!complete.cases(data_salaries), ]
names(data_salaries)[colSums(is.na(data_salaries)) > 0]
colnames(data_salaries) = colnames(data_salaries) |>
     str_replace_all("_", " ") |>
     str_to_title() |> 
     str_replace_all(" ", "_")
colnames(data_salaries)[which(names(data_salaries) == "Salary")] = "Salary_LOC"
colnames(data_salaries)[which(names(data_salaries) == "Salary_In_Usd")] = "Salary_USD"
colnames(data_salaries)[which(names(data_salaries) == "Employee_Residence")] = "Employee_Residence_Code"
colnames(data_salaries)[which(names(data_salaries) == "Company_Location")] = "Company_Location_Code"
data_salaries = data_salaries %>%
  mutate(
    Work_Arrangement = factor(
      ifelse(Remote_Ratio == 100, 'Remote',
             ifelse(Remote_Ratio == 50, 'Hybrid', 'In-Person')),
      levels = c('In-Person', 'Hybrid', 'Remote')
    )
  )
country_codes_legend = read.csv('continents2.csv')
country_name_lookup = function(code) {
  code_match = match(code, country_codes_legend$alpha.2)
  country_codes_legend$name[code_match]
}
data_salaries = data_salaries %>%
  mutate(
    Employee_Residence_Name = country_name_lookup(Employee_Residence_Code),
    Company_Location_Name   = country_name_lookup(Company_Location_Code)
  )
data_salaries =data_salaries %>% 
  select(
    Work_Year,
    Job_Title,
    Experience_Level,
    Employment_Type,
    Remote_Ratio,
    Work_Arrangement,
    Company_Size,
    Company_Location_Code,
    Company_Location_Name,
    Employee_Residence_Code,
    Employee_Residence_Name,
    Salary_Currency,
    Salary_LOC,
    Salary_USD
  )
yearly_job_count = data_salaries %>% 
     count(Work_Year, name = "Count")
yearly_job_count
data_salaries_2024 = data_salaries %>% filter(Work_Year == 2024)
FX_Rates_2024 <- data_salaries_2024 %>%
  group_by(Salary_Currency) %>%
  summarize(
    Avg_Salary_LOC = mean(Salary_LOC, na.rm = TRUE),
    Avg_Salary_USD = mean(Salary_USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(FX_Rate = Avg_Salary_LOC / Avg_Salary_USD)

