library(dplyr)
library(stringr)
library(formattable)
library(scales)
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
colnames(data_salaries)[which(names(data_salaries) == "Experience_Level")] = "Experience_Level_Code"
data_salaries = data_salaries %>%
  mutate(
    Experience_Level = factor(
      ifelse(Experience_Level_Code == 'EN', 'Entry',
             ifelse(Experience_Level_Code == 'MI', 'Mid', 
                    ifelse(Experience_Level_Code == 'SE', 'Senior', 'Executive'))),
      levels = c('Entry', 'Mid', 'Senior', 'Executive')),
    Work_Time_Arrangement = factor(
      ifelse(Employment_Type == 'PT', 'Part-time',
             ifelse(Employment_Type == 'FT', 'Full-time', 
                    ifelse(Employment_Type == 'CT', 'Contract', 'Freelance'))),
      levels = c('Part-time', 'Full-time', 'Contract', 'Freelance')),
    Work_Office_Arrangement = factor(
      ifelse(Remote_Ratio == 100, 'Remote',
             ifelse(Remote_Ratio == 50, 'Hybrid', 'In-Person')),
      levels = c('In-Person', 'Hybrid', 'Remote')),
    International = factor(
      ifelse(Company_Location_Code != Employee_Residence_Code, 'International', 'Domestic'),
      levels = c('Domestic', 'International'))
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
    Experience_Level_Code,
    Experience_Level,
    Employment_Type,
    Work_Time_Arrangement,
    Remote_Ratio,
    Work_Office_Arrangement,
    Company_Size,
    Company_Location_Code,
    Company_Location_Name,
    Employee_Residence_Code,
    Employee_Residence_Name,
    International,
    Salary_Currency,
    Salary_LOC,
    Salary_USD
  )
yearly_job_count = data_salaries %>% 
     count(Work_Year, name = "Count")
yearly_job_count
data_salaries_2024 = data_salaries %>% filter(Work_Year == 2024) %>%
  arrange(desc(Salary_USD))
data_salaries_2024_select = data_salaries_2024 %>% select(Job_Title, Experience_Level, Work_Time_Arrangement, Work_Office_Arrangement, Company_Size, Company_Location_Name, International, Salary_USD)
data_salaries_2024_select_asc = data_salaries_2024_select %>% arrange(Salary_USD)
Total_Count_2024 = nrow(data_salaries_2024_select)
FX_Rates_2024 = data_salaries_2024 %>%
  group_by(Salary_Currency) %>%
  summarise(
    Avg_Salary_LOC = mean(Salary_LOC),
    Avg_Salary_USD = mean(Salary_USD), 
    Count = n(), .groups = 'drop'
  ) %>%
  mutate(
    Percentage = round(Count / Total_Count_2024 * 100,0),
    FX_Rate = Avg_Salary_LOC / Avg_Salary_USD
  ) %>%
  arrange(Salary_Currency)
Job_Title = data_salaries_2024_select %>%
  group_by(Job_Title) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Experience_Level = data_salaries_2024_select %>%
  group_by(Experience_Level) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Work_Time_Arrangement = data_salaries_2024_select %>%
  group_by(Work_Time_Arrangement) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Work_Office_Arrangement = data_salaries_2024_select %>%
  group_by(Work_Office_Arrangement) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Company_Size = data_salaries_2024_select %>%
  group_by(Company_Size) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Company_Location_Name = data_salaries_2024_select %>%
  group_by(Company_Location_Name) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
International = data_salaries_2024_select %>%
  group_by(International) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Title_Experience = data_salaries_2024_select %>%
  group_by(Job_Title, Experience_Level) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Title_Time_Arrangement = data_salaries_2024_select %>%
  group_by(Job_Title, Work_Time_Arrangement) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Experience_Time_Arrangement = data_salaries_2024_select %>%
  group_by(Experience_Level, Work_Time_Arrangement) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Office_Time_Arrangement = data_salaries_2024_select %>%
  group_by(Work_Office_Arrangement, Work_Time_Arrangement) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
Company_Location_Size = data_salaries_2024_select %>%
  group_by(Company_Location_Name, Company_Size) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(desc(Avg_Salary_USD))
International_by_Location = data_salaries_2024 %>%
  group_by(International, Company_Location_Name, Employee_Residence_Name) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  filter(International == 'International') %>%
  arrange(desc(Avg_Salary_USD))
Job_Title_asc = data_salaries_2024_select %>%
  group_by(Job_Title) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(Avg_Salary_USD)
Company_Location_Name_asc = data_salaries_2024_select %>%
  group_by(Company_Location_Name) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(Avg_Salary_USD)
Title_Experience_asc = data_salaries_2024_select %>%
  group_by(Job_Title, Experience_Level) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(Avg_Salary_USD)
Title_Time_Arrangement_asc = data_salaries_2024_select %>%
  group_by(Job_Title, Work_Time_Arrangement) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(Avg_Salary_USD)
Company_Location_Size_asc = data_salaries_2024_select %>%
  group_by(Company_Location_Name, Company_Size) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  arrange(Avg_Salary_USD)
International_by_Location_asc = data_salaries_2024 %>%
  group_by(International, Company_Location_Name, Employee_Residence_Name) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 0)) %>%
  filter(International == 'International') %>%
  arrange(Avg_Salary_USD)
  
formattable(
  data_salaries_2024_select,
  list(
    Salary_USD = formatter("span",
                           style = x ~ style(
                             display = "block",
                             padding = "0 4px",
                             `border-radius` = "4px",
                             `background-color` = csscolor(gradient(as.numeric(x), "white", "lightblue"))
                           ),
                           x ~ comma(x, accuracy = 1)
    )
  )
)
formattable(
  data_salaries_2024_select_asc,
  list(
    Salary_USD = formatter("span",
                           style = x ~ style(
                             display = "block",
                             padding = "0 4px",
                             `border-radius` = "4px",
                             `background-color` = csscolor(gradient(as.numeric(x), "white", "lightblue"))
                           ),
                           x ~ comma(x, accuracy = 1)
    )
  )
)
Pay_Summary_by = function(df) {
  formattable(
    df,
    list(
      Avg_Salary_USD = formatter(
        "span",
        style = x ~ style(
          display = "block",
          padding = "0 4px",
          `border-radius` = "4px",
          `background-color` = csscolor(
            gradient(as.numeric(x), "white", "lightblue")
          )
        ),
        x ~ comma(x, accuracy = 1)
      ),
      Count = formatter(
        "span",
        style = x ~ style(
          display = "block",
          padding = "0 4px"
        ),
        x ~ comma(x, accuracy = 1)
      ),
      Percentage = formatter(
        "span",
        style = x ~ style(
          display = "block",
          padding = "0 4px"
        ),
        x ~ percent(x / 100, accuracy = 0.01)
      )
    )
  )
}
Top10_Pay_Summary_by = function(df) {
 Pay_Summary_by(head(df, 10))
}
Pay_Summary_by(Experience_Level)
Pay_Summary_by(Work_Time_Arrangement)
Pay_Summary_by(Company_Size)
Pay_Summary_by(International)
Pay_Summary_by(Experience_Time_Arrangement)
Pay_Summary_by(Office_Time_Arrangement)
Top10_Pay_Summary_by(Job_Title)
Top10_Pay_Summary_by(Job_Title_asc)
Top10_Pay_Summary_by(Company_Location_Name)
Top10_Pay_Summary_by(Company_Location_Name_asc)
Top10_Pay_Summary_by(Title_Experience)
Top10_Pay_Summary_by(Title_Experience_asc)
Top10_Pay_Summary_by(Title_Time_Arrangement)
Top10_Pay_Summary_by(Title_Time_Arrangement_asc)
Top10_Pay_Summary_by(Company_Location_Size)
Top10_Pay_Summary_by(Company_Location_Size_asc)
Top10_Pay_Summary_by(International_by_Location)
Top10_Pay_Summary_by(International_by_Location_asc)