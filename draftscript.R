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
country_codes_legend = read.csv('continents2.csv')
country_name_lookup = function(code) {
  code_match = match(code, country_codes_legend$alpha.2)
  country_codes_legend$name[code_match]
}
country_code3_lookup = function(code2) {
  code_match = match(code2, country_codes_legend$alpha.2)
  country_codes_legend$alpha.3[code_match]
}
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
      levels = c('Domestic', 'International')),
    Employee_Residence_Name = country_name_lookup(Employee_Residence_Code),
    Company_Location_Name = country_name_lookup(Company_Location_Code),
    Company_Location_Code3 = country_code3_lookup(Company_Location_Code)
  )
data_salaries = data_salaries %>% 
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
    Company_Location_Code3,
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
data_salaries_2024_select = data_salaries_2024 %>% filter(Work_Time_Arrangement == 'Full-time') %>% 
  select(Job_Title, Experience_Level, Work_Time_Arrangement, Work_Office_Arrangement, Company_Size, Company_Location_Code3, Company_Location_Name, International, Salary_USD)
data_salaries_2024_select_asc = data_salaries_2024_select %>% arrange(Salary_USD)
Total_Count_2024 = nrow(data_salaries_2024)
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
Summarise_Salary = function(df, group_cols) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      Avg_Salary_USD = round(mean(Salary_USD), 0),
      Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      Percentage = round(Count / Total_Count_2024 * 100, 2)
    ) %>%
    arrange(desc(Avg_Salary_USD))
}
Job_Title_Summary = Summarise_Salary(data_salaries_2024_select, 'Job_Title')
Experience_Level_Summary = Summarise_Salary(data_salaries_2024_select, 'Experience_Level')
Work_Time_Arrangement_Summary = Summarise_Salary(data_salaries_2024, 'Work_Time_Arrangement')
Work_Office_Arrangement_Summary = Summarise_Salary(data_salaries_2024_select, 'Work_Office_Arrangement')
Company_Size_Summary = Summarise_Salary(data_salaries_2024_select, 'Company_Size')
Company_Location_Name_Summary = Summarise_Salary(data_salaries_2024_select, c('Company_Location_Name','Company_Location_Code3'))
Title_Experience_Summary = Summarise_Salary(data_salaries_2024_select, c('Job_Title', 'Experience_Level'))
Title_Time_Arrangement_Summary = Summarise_Salary(data_salaries_2024, c('Job_Title', 'Work_Time_Arrangement'))
Experience_Time_Arrangement_Summary = Summarise_Salary(data_salaries_2024, c('Experience_Level', 'Work_Time_Arrangement'))
Office_Time_Arrangement_Summary = Summarise_Salary(data_salaries_2024, c('Work_Office_Arrangement', 'Work_Time_Arrangement'))
Company_Location_Size_Summary = Summarise_Salary(data_salaries_2024_select, c('Company_Location_Name', 'Company_Size'))
International_Summary = data_salaries_2024 %>%
  filter(Work_Time_Arrangement == 'Full-time') %>%
  group_by(International) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 2)) %>%
  arrange(desc(Avg_Salary_USD))
International_by_Location_Summary = data_salaries_2024 %>%
  filter(International == 'International', Work_Time_Arrangement == 'Full-time') %>%
  group_by(International, Company_Location_Name, Employee_Residence_Name) %>%
  summarise(
    Avg_Salary_USD = round(mean(Salary_USD),0), Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / Total_Count_2024 * 100, 2)) %>%
  arrange(desc(Avg_Salary_USD))
Job_Title_Summary_asc = Job_Title_Summary %>% arrange(Avg_Salary_USD)
Company_Location_Name_Summary_asc = Company_Location_Name_Summary %>% arrange(Avg_Salary_USD)
Title_Experience_Summary_asc = Title_Experience_Summary %>% arrange(Avg_Salary_USD)
Title_Time_Arrangement_Summary_asc = Title_Time_Arrangement_Summary %>% arrange(Avg_Salary_USD)
Company_Location_Size_Summary_asc = Company_Location_Size_Summary %>% arrange(Avg_Salary_USD)
International_by_Location_Summary_asc = International_by_Location_Summary %>% arrange(Avg_Salary_USD)
formattable(
  head(data_salaries_2024_select,10),
  list(
    Salary_USD = formatter("span",
                           style = x ~ list(
                             display = "block",
                             padding = "0 4px",
                             `border-radius` = "4px",
                             `background-color` = csscolor(gradient(as.numeric(x), "white", "lightblue"))
                           ),
                           format = x ~ comma(as.numeric(x), accuracy = 1)
    )
  )
)
formattable(
  head(data_salaries_2024_select_asc,10),
  list(
    Salary_USD = formatter("span",
                           style = x ~ list(
                             display = "block",
                             padding = "0 4px",
                             `border-radius` = "4px",
                             `background-color` = csscolor(gradient(as.numeric(x), "white", "lightblue"))
                           ),
                           format = x ~ comma(as.numeric(x), accuracy = 1)
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
Pay_Summary_by(Experience_Level_Summary)
Pay_Summary_by(Work_Time_Arrangement_Summary)
Pay_Summary_by(Company_Size_Summary)
Pay_Summary_by(International_Summary)
Pay_Summary_by(Experience_Time_Arrangement_Summary)
Pay_Summary_by(Office_Time_Arrangement_Summary)
Top10_Pay_Summary_by(Job_Title_Summary)
Top10_Pay_Summary_by(Job_Title_Summary_asc)
Top10_Pay_Summary_by(Company_Location_Name_Summary)
Top10_Pay_Summary_by(Company_Location_Name_Summary_asc)
Top10_Pay_Summary_by(Title_Experience_Summary)
Top10_Pay_Summary_by(Title_Experience_Summary_asc)
Top10_Pay_Summary_by(Title_Time_Arrangement_Summary)
Top10_Pay_Summary_by(Title_Time_Arrangement_Summary_asc)
Top10_Pay_Summary_by(Company_Location_Size_Summary)
Top10_Pay_Summary_by(Company_Location_Size_Summary_asc)
Top10_Pay_Summary_by(International_by_Location_Summary)
Top10_Pay_Summary_by(International_by_Location_Summary_asc)
library(plotly)
World_Map_by_Salary = plot_ly(
  data = Company_Location_Name_Summary,
  type = 'choropleth',
  locations = ~Company_Location_Code3,
  z = ~Avg_Salary_USD,
  text = ~Company_Location_Name,
  colorscale = "Blues",
  reversescale = TRUE,
  locationmode = 'ISO-3'
) %>%
  layout(
    title = 'Avg Salary by Company Location (USD)',
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'equirectangular')
    )
  )
World_Map_by_Salary