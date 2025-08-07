library(dplyr)
library(stringr)
library(formattable)
library(scales)
library(countrycode)
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
data_salaries$Company_Size = factor(data_salaries$Company_Size, 
                    levels = c('S', 'M', 'L'))
country_codes_legend = read.csv('continents2.csv')
field_title_legend = read.csv('data_field_title_legend.csv')
country_name_lookup = function(code) {
  code_match = match(code, country_codes_legend$alpha.2)
  country_codes_legend$name[code_match]
}
country_code3_lookup = function(code2) {
  code_match = match(code2, country_codes_legend$alpha.2)
  country_codes_legend$alpha.3[code_match]
}
field_lookup = function(title) {
  field_title_legend$Field[match(title, field_title_legend$Title)]
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
    Field = factor(field_lookup(Job_Title), levels = c('Data Engineering', 'Data Science', 'Data Analysis', 'Business Intelligence', 'Machine Learning', 'Artificial Intelligence   (general)', 'Software Development   (general)', 'Research & Development   (general)')),
    Company_Location_Name = country_name_lookup(Company_Location_Code),
    Company_Location_Code3 = country_code3_lookup(Company_Location_Code),
    Continent = factor(countrycode(sourcevar = Company_Location_Code3, origin = 'iso3c', destination = 'continent'),
                       levels = c('Americas', 'Europe', 'Oceania', 'Asia', 'Africa')),
    USA = factor(
      ifelse(Company_Location_Code3 == 'USA', 'USA', 'Outside_USA'),
      levels = c('USA', 'Outside_USA'))
  )
data_salaries = data_salaries %>% 
  select(
    Work_Year,
    Field,
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
    Continent,
    USA,
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
data_salaries_2024_FTonly = data_salaries_2024 %>% filter(Work_Time_Arrangement == 'Full-time')
data_salaries_2024_select = data_salaries_2024 %>% filter(Work_Time_Arrangement == 'Full-time') %>% 
  select(Field, Job_Title, Experience_Level, Work_Time_Arrangement, Work_Office_Arrangement, Company_Size, USA, Continent, Company_Location_Name, Company_Location_Code3, International, Salary_USD)
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
Field_Summary = Summarise_Salary(data_salaries_2024_FTonly, 'Field')
Field_Title_Summary = Summarise_Salary(data_salaries_2024_FTonly, c('Field', 'Job_Title'))
Experience_Level_Summary = Summarise_Salary(data_salaries_2024_FTonly, 'Experience_Level')
Work_Time_Arrangement_Summary = Summarise_Salary(data_salaries_2024, 'Work_Time_Arrangement')
Work_Office_Arrangement_Summary = Summarise_Salary(data_salaries_2024_FTonly, 'Work_Office_Arrangement')
Company_Size_Summary = Summarise_Salary(data_salaries_2024_FTonly, 'Company_Size')
Company_Location_Name_Summary = Summarise_Salary(data_salaries_2024_FTonly, c('Company_Location_Name','Company_Location_Code3'))
Continent_Summary = Summarise_Salary(data_salaries_2024_FTonly, 'Continent')
USA_Summary = Summarise_Salary(data_salaries_2024_FTonly, 'USA')
Field_Experience_Summary = Summarise_Salary(data_salaries_2024_FTonly, c('Field', 'Experience_Level'))
Field_Time_Arrangement_Summary = Summarise_Salary(data_salaries_2024, c('Field', 'Work_Time_Arrangement'))
Experience_Time_Arrangement_Summary = Summarise_Salary(data_salaries_2024, c('Experience_Level', 'Work_Time_Arrangement'))
Office_Time_Arrangement_Summary = Summarise_Salary(data_salaries_2024, c('Work_Office_Arrangement', 'Work_Time_Arrangement'))
Company_USA_Size_Summary = Summarise_Salary(data_salaries_2024_FTonly, c('USA', 'Company_Size'))
Company_Continent_Size_Summary = Summarise_Salary(data_salaries_2024_FTonly, c('Continent', 'Company_Size'))
International_Summary = Summarise_Salary(data_salaries_2024_FTonly, 'International')
International_by_Location_Summary = Summarise_Salary(data_salaries_2024_FTonly, c('International', 'Company_Location_Name', 'Employee_Residence_Name'))
Field_Summary_asc = Field_Summary %>% arrange(Avg_Salary_USD)
Field_Title_Summary_asc = Field_Title_Summary %>% arrange(Avg_Salary_USD)
Field_Experience_Summary_asc = Field_Experience_Summary %>% arrange(Avg_Salary_USD)
Field_Time_Arrangement_Summary_asc = Field_Time_Arrangement_Summary %>% arrange(Avg_Salary_USD)
Company_Location_Name_Summary_asc = Company_Location_Name_Summary %>% arrange(Avg_Salary_USD)
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
Pay_Summary_by(Continent_Summary)
Pay_Summary_by(USA_Summary)
Pay_Summary_by(International_Summary)
Pay_Summary_by(Experience_Time_Arrangement_Summary)
Pay_Summary_by(Office_Time_Arrangement_Summary)
Pay_Summary_by(Field_Summary)
Pay_Summary_by(Field_Summary_asc)
Pay_Summary_by(Field_Experience_Summary)
Pay_Summary_by(Field_Time_Arrangement_Summary)
Top10_Pay_Summary_by(Company_Location_Name_Summary)
Top10_Pay_Summary_by(Company_Location_Name_Summary_asc)
Top10_Pay_Summary_by(Field_Title_Summary)
Top10_Pay_Summary_by(Field_Title_Summary_asc)
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
World_Map_by_Job_Count = plot_ly(
  data = Company_Location_Name_Summary,
  type = 'choropleth',
  locations = ~Company_Location_Code3,
  z = ~Count,
  text = ~Company_Location_Name,
  colorscale = "Oranges",
  reversescale = FALSE,
  locationmode = 'ISO-3'
) %>%
  layout(
    title = 'Total Job Count by Company Location',
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'equirectangular')
    )
  )
World_Map_by_Job_Count
Data_Salary_Overall_Histogram = plot_ly(data_salaries_2024_select, x = ~Salary_USD) %>%
  add_histogram(name = "Overall FT Salary Distribution", nbinsx = 50, opacity = 1) %>%
  add_trace(
    x = ~sort(Salary_USD),
    y = ~ecdf(Salary_USD)(sort(Salary_USD)),
    type = 'scatter',
    mode = 'lines',
    name = 'ECDF',
    yaxis = 'y2'
  ) %>%
  layout(
    title = "FT Salary Distribution with ECDF",
    xaxis = list(title = "Salary (USD)"),
    yaxis = list(title = "Count"),
    yaxis2 = list(
      title = "ECDF",
      overlaying = "y",
      side = "right",
      range = c(0,1)
    ),
    bargap = 0.01
  )
Data_Salary_Overall_Histogram
Stacked_Histogram = function(df, catcol, numcol = 'Salary_USD') {
  plot_ly(
    data = df,
    x = ~.data[[numcol]],
    color = ~.data[[catcol]],
    type = "histogram",
    opacity = .7
  ) %>%
    layout(
      title = paste(catcol, 'Salary Distribution'),
      xaxis = list(title = 'Salary (USD)'),
      yaxis = list(title = 'Count'),
      barmode = "overlay"
    )
}
Box_1Lvl = function(df, lvl, numcol = 'Salary_USD') {
  plot_ly(
    data = df,
    y = ~get(numcol),
    color = ~get(lvl),
    type = "box"
  )
}
Box_2Lvl = function(df, lvl, subLvl, numcol = 'Salary_USD') {
  plot_ly(
    data = df,
    x = ~get(lvl),
    y = ~get(numcol),
    color = ~get(subLvl),
    type = "box"
  ) %>%
    layout(boxmode = "group")  # Side-by-side grouping
}
Violin_1Lvl = function(df, lvl, numcol = 'Salary_USD') {
  plot_ly(
    data = df,
    x = ~get(lvl),
    y = ~get(numcol),
    split = ~get(lvl),
    type = 'violin',
    box = list(visible = TRUE),
    meanline = list(visible = TRUE)
  ) %>%
    layout(
      xaxis = list(title = lvl),
      yaxis = list(title = numcol, zeroline = FALSE)
    )
}
Violin_2Lvl = function(df, lvl, subLvl, numcol = 'Salary_USD') {
  plot_ly(
    data = df,
    x = ~get(lvl),
    y = ~get(numcol),
    color = ~get(subLvl),
    type = "violin",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE)
  ) %>%
    layout(
      xaxis = list(title = lvl),
      yaxis = list(title = numcol, zeroline = FALSE),
      violinmode = "group"
    )
}
Bar_1Lvl = function(df, lvl, numcol = 'Salary_USD', aggfun = mean) {
  df_summary = df %>%
    group_by(across(all_of(lvl))) %>%
    summarise(Value = aggfun(.data[[numcol]]), .groups = "drop")
  plot_ly(
    data = df_summary,
    x = ~get(lvl),
    y = ~Value,
    color = ~get(lvl),
    type = "bar"
  ) %>%
    layout(
      xaxis = list(title = lvl),
      yaxis = list(title = numcol)
    )
}
Bar_2Lvl = function(df, lvl, subLvl, numcol = 'Salary_USD', aggfun = mean) {
  df_summary = df %>%
    group_by(across(all_of(c(lvl, subLvl)))) %>%
    summarise(Value = aggfun(.data[[numcol]]), .groups = "drop")
  plot_ly(
    data = df_summary,
    x = ~get(lvl),
    y = ~Value,
    color = ~get(subLvl),
    type = "bar"
  ) %>%
    layout(
      xaxis = list(title = lvl),
      yaxis = list(title = numcol),
      barmode = 'group')
}
Stacked_Histogram(data_salaries_2024_FTonly, 'Field')
Stacked_Histogram(data_salaries_2024_FTonly, 'Experience_Level')
Stacked_Histogram(data_salaries_2024, 'Work_Time_Arrangement')
Stacked_Histogram(data_salaries_2024_FTonly, 'Company_Size')
Stacked_Histogram(data_salaries_2024_FTonly, 'USA')
Stacked_Histogram(data_salaries_2024_FTonly, 'Continent')
Stacked_Histogram(data_salaries_2024_FTonly, 'International')
Box_1Lvl(data_salaries_2024_FTonly, 'Field')
Box_1Lvl(data_salaries_2024_FTonly, 'Experience_Level')
Box_1Lvl(data_salaries_2024, 'Work_Time_Arrangement')
Box_1Lvl(data_salaries_2024_FTonly, 'Company_Size')
Box_1Lvl(data_salaries_2024_FTonly, 'USA')
Box_1Lvl(data_salaries_2024_FTonly, 'Continent')
Box_1Lvl(data_salaries_2024_FTonly, 'International')
Box_2Lvl(data_salaries_2024_FTonly, 'Field', 'Experience_Level')
Box_2Lvl(data_salaries_2024, 'Field', 'Work_Time_Arrangement')
Box_2Lvl(data_salaries_2024, 'Experience_Level', 'Work_Time_Arrangement')
Box_2Lvl(data_salaries_2024, 'Work_Office_Arrangement', 'Work_Time_Arrangement')
Box_2Lvl(data_salaries_2024_FTonly, 'Continent', 'Company_Size')
Box_2Lvl(data_salaries_2024_FTonly, 'USA', 'Company_Size')
Violin_1Lvl(data_salaries_2024_FTonly, 'Field')
Violin_1Lvl(data_salaries_2024_FTonly, 'Experience_Level')
Violin_1Lvl(data_salaries_2024, 'Work_Time_Arrangement')
Violin_1Lvl(data_salaries_2024_FTonly, 'Company_Size')
Violin_1Lvl(data_salaries_2024_FTonly, 'USA')
Violin_1Lvl(data_salaries_2024_FTonly, 'Continent')
Violin_1Lvl(data_salaries_2024_FTonly, 'International')
Violin_2Lvl(data_salaries_2024_FTonly, 'Field', 'Experience_Level')
Violin_2Lvl(data_salaries_2024, 'Field', 'Work_Time_Arrangement')
Violin_2Lvl(data_salaries_2024, 'Experience_Level', 'Work_Time_Arrangement')
Violin_2Lvl(data_salaries_2024, 'Work_Office_Arrangement', 'Work_Time_Arrangement')
Violin_2Lvl(data_salaries_2024_FTonly, 'Continent', 'Company_Size')
Violin_2Lvl(data_salaries_2024_FTonly, 'USA', 'Company_Size')
Bar_1Lvl(data_salaries_2024_FTonly, 'Field')
Bar_1Lvl(data_salaries_2024_FTonly, 'Experience_Level')
Bar_1Lvl(data_salaries_2024, 'Work_Time_Arrangement')
Bar_1Lvl(data_salaries_2024_FTonly, 'Company_Size')
Bar_1Lvl(data_salaries_2024_FTonly, 'USA')
Bar_1Lvl(data_salaries_2024_FTonly, 'Continent')
Bar_1Lvl(data_salaries_2024_FTonly, 'International')
Bar_2Lvl(data_salaries_2024_FTonly, 'Field', 'Experience_Level')
Bar_2Lvl(data_salaries_2024, 'Field', 'Work_Time_Arrangement')
Bar_2Lvl(data_salaries_2024, 'Experience_Level', 'Work_Time_Arrangement')
Bar_2Lvl(data_salaries_2024, 'Work_Office_Arrangement', 'Work_Time_Arrangement')
Bar_2Lvl(data_salaries_2024_FTonly, 'Continent', 'Company_Size')
Bar_2Lvl(data_salaries_2024_FTonly, 'USA', 'Company_Size')