library(dplyr)
data_salaries = read.csv('salaries.csv')
head(data_salaries)
data_salaries[!complete.cases(data_salaries), ]
names(data_salaries)[colSums(is.na(data_salaries)) > 0]
library(stringr)
colnames(data_salaries) = colnames(data_salaries) |>
     str_replace_all("_", " ") |>
     str_to_title() |> 
     str_replace_all(" ", "_")
colnames(data_salaries)[which(names(data_salaries) == "Salary_In_Usd")] = "Salary_USD"
mutate(
  Work_Arrangement = factor(
    ifelse(Remote_Ratio == 100, 'Remote',
           ifelse(Remote_Ratio == 50, 'Hybrid', 'In-Person')),
    levels = c('In-Person', 'Hybrid', 'Remote')
  )
)