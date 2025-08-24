library(shiny)
<<<<<<< HEAD
library(shinythemes)
library(dplyr)
library(plotly)

data = read.csv('data/Data Salaries- Shiny.csv', stringsAsFactors = FALSE)

#Factor mutation function
factor_mutation = function(df) {
  df %>% mutate(
    Field = factor(Field,
                   levels = c('Data Engineering', 'Data Science', 'Data Analysis', 'Business Intelligence', 
                              'Machine Learning', 'Artificial Intelligence   (general)', 
                              'Software Development   (general)', 'Research & Development   (general)')),
    Experience_Level = factor(Experience_Level, levels = c('Entry', 'Mid', 'Senior', 'Executive')),
    Work_Time_Arrangement = factor(Work_Time_Arrangement, levels = c('Full-time', 'Part-time', 'Contract', 'Freelance')),
    Work_Office_Arrangement = factor(Work_Office_Arrangement, levels = c('In-Person', 'Hybrid', 'Remote')),
    Continent = factor(Continent, levels = c('Americas', 'Europe', 'Oceania', 'Asia', 'Africa')),
    Company_Size = factor(Company_Size, levels = c('S', 'M', 'L'))
  )
}
####
data = factor_mutation(data)
data_2024 = data %>% filter(Work_Year == 2024)

fields = levels(data$Field)
exp_lvls = levels(data$Experience_Level)
titles = sort(unique(data$Job_Title))
times = levels(data$Work_Time_Arrangement)
office_arrangements = levels(data$Work_Office_Arrangement)
continents = levels(data$Continent)
countries = sort(unique(data$Country))
sizes = levels(data$Company_Size)

#Job Count by Year
yearly_job_count = data %>%
  count(Work_Year, name = 'Count') %>%
  mutate(Count = format(Count, big.mark = ','))


#Plot Functions
Histogram_ECDF = function(df) {
  if (nrow(df) == 0) return(NULL)
  
  plot_ly(df, x = ~Salary_USD) %>%
    add_histogram(name = 'Salary Distribution', nbinsx = 50, opacity = 0.7) %>%
    add_trace(
      x = ~sort(Salary_USD),
      y = ~ecdf(Salary_USD)(sort(Salary_USD)),
      type = 'scatter',
      mode = 'lines',
      name = 'ECDF',
      yaxis = 'y2'
    ) %>%
    layout(
      title = 'Salary Distribution with ECDF',
      xaxis = list(title = 'Salary (USD)'),
      yaxis = list(title = 'Count'),
      yaxis2 = list(
        title = 'ECDF',
        overlaying = 'y',
        side = 'right',
        range = c(0, 1)
      ),
      bargap = 0.01
    )
}

Violin_2lvl = function(df, lvl, subLvl, numcol = 'Salary_USD') {
  if (is.null(subLvl) || subLvl == 'NULL') {
    plt = plot_ly(
      data = df,
      x = ~get(numcol),
      y = ~get(lvl),
      color = ~get(lvl),
      type = 'violin',
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      orientation = 'h'
    )
  } else {
    plt = plot_ly(
      data = df,
      x = ~get(numcol),
      y = ~get(lvl),
      color = ~get(subLvl),
      type = 'violin',
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      orientation = 'h'
    )
  }
  
  plt %>% layout(
    xaxis = list(title = 'Salary USD', zeroline = TRUE, range = c(0, max(df[[numcol]], na.rm = TRUE))),
    yaxis = list(title = lvl),
    violinmode = 'group'
  )
}

Summarise_Salary = function(df, group_cols) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      Avg_Salary_USD = round(mean(Salary_USD), 0),
      Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      Percentage = round(Count / sum(Count) * 100, 2)
    ) %>%
    arrange(desc(Avg_Salary_USD))
}


Bar_2lvl = function(df, lvl, subLvl, numcol = 'Salary_USD') {
  if (is.null(subLvl) || subLvl == 'NULL') {
    plt = plot_ly(
      data = df %>% 
        group_by(across(all_of(lvl))) %>%
        summarise(Value = mean(.data[[numcol]], na.rm = TRUE), .groups = 'drop'),
      x = ~get(lvl),
      y = ~Value,
      color = ~get(lvl),
      type = 'bar'
    )
  } else {
    plt = plot_ly(
      data = df %>% 
        group_by(across(all_of(c(lvl, subLvl)))) %>%
        summarise(Value = mean(.data[[numcol]], na.rm = TRUE), .groups = 'drop'),
      x = ~get(lvl),
      y = ~Value,
      color = ~get(subLvl),
      type = 'bar'
    )
  }
  plt %>% layout(
    xaxis = list(title = lvl),
    yaxis = list(title = 'Salary USD (avg)', zeroline = FALSE),
    barmode = 'group'
  )
}

Pie_1lvl = function(df_summary, lvl, subLvl = 'NULL') {
  plot_ly(
    data = df_summary,
    labels = ~get(lvl),
    values = ~Count,
    type = 'pie'
  ) %>%
    layout(
      title = paste('% of Jobs by', lvl)
    )
}

Sunburst_2lvl = function(df_summary, lvl, subLvl, numcol = 'Percentage') {
  df_summary = df_summary %>%
    rename(Level = all_of(lvl),
           SubLevel = all_of(subLvl),
           Value = all_of(numcol)) %>%
    mutate(
      Level = as.character(Level),
      SubLevel = as.character(SubLevel)
    )
  root_node = tibble(
    labels = 'Total',
    parents = '',
    values = sum(df_summary$Value, na.rm = TRUE)
  )
  df_lvl = df_summary %>%
    group_by(Level) %>%
    summarise(values = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    transmute(
      labels = Level,
      parents = 'Total',
      values
    )
  df_subLvl = df_summary %>%
    transmute(
      labels = SubLevel,
      parents = Level,
      values = Value
    )
  df_sunburst = bind_rows(root_node, df_lvl, df_subLvl)
  plot_ly(
    data = df_sunburst,
    labels = ~labels,
    parents = ~parents,
    values = ~values,
    type = 'sunburst',
    branchvalues = 'total'
  ) %>% layout(title = paste('% of rows by', lvl, subLvl))
}

Pie_Sunburst = function(df, lvl, subLvl) {
  if (is.null(subLvl) || subLvl == 'NULL') {
    Pie_1lvl(Summarise_Salary(df, lvl), lvl)}
  else {Sunburst_2lvl(Summarise_Salary(df, c(lvl, subLvl)), lvl, subLvl)}
}

Bar_Trend_Salary = function(df, numcol = 'Salary_USD', aggfun = mean) {
  df_summary = df %>%
    group_by(Work_Year) %>%
    summarise(Value = aggfun(.data[[numcol]], na.rm = TRUE), .groups = 'drop')
  
  plot_ly(
    data = df_summary,
    x = ~Work_Year,
    y = ~Value,
    color = ~as.factor(Work_Year),
    type = 'bar'
  ) %>%
    layout(
      xaxis = list(title = 'Pay Trend'),
      yaxis = list(title = 'Salary USD (avg)'),
      barmode = 'group'
    )
}

Bar_Trend_Salary_Sub = function(df, subLvl, numcol = 'Salary_USD', aggfun = mean) {
  df_summary = df %>%
    group_by(across(all_of(c('Work_Year', subLvl)))) %>%
    summarise(Value = aggfun(.data[[numcol]], na.rm = TRUE), .groups = 'drop')
=======
library(dplyr)
library(plotly)

# ==== Load data ====
data <- read.csv("data/Data Salaries- Shiny.csv", stringsAsFactors = FALSE)

# ==== Unique values for filters ====
fields <- sort(unique(data$Field))
exp_lvls <- sort(unique(data$Experience_Level))
titles <- sort(unique(data$Job_Title))
times <- sort(unique(data$Work_Time_Arrangement))
office_arrangements <- sort(unique(data$Work_Office_Arrangement))
continents <- sort(unique(data$Continent))
countries <- sort(unique(data$Company_Location_Name))
sizes <- sort(unique(data$Company_Size))

# ==== Functions ====
Bar_Trend_Salary <- function(df, subLvl, numcol = 'Salary_USD', aggfun = mean) {
  df_summary <- df %>%
    group_by(across(all_of(c('Work_Year', subLvl)))) %>%
    summarise(Value = aggfun(.data[[numcol]]), .groups = "drop")
>>>>>>> c9839f3abc33fa5a631bce80224ac3a4ed2b2b2a
  
  plot_ly(
    data = df_summary,
    x = ~Work_Year,
    y = ~Value,
    color = ~get(subLvl),
<<<<<<< HEAD
    type = 'bar'
  ) %>%
    layout(
      xaxis = list(title = paste('Pay Trend by', subLvl)),
      yaxis = list(title = 'Salary USD (avg)'),
=======
    type = "bar"
  ) %>%
    layout(
      xaxis = list(title = paste('Pay Trend by', subLvl)),
      yaxis = list(title = numcol),
>>>>>>> c9839f3abc33fa5a631bce80224ac3a4ed2b2b2a
      barmode = 'group'
    )
}

<<<<<<< HEAD
World_Map_by_Salary = function(df) {
=======
World_Map_by_Salary <- function(df) {
>>>>>>> c9839f3abc33fa5a631bce80224ac3a4ed2b2b2a
  plot_ly(
    data = df,
    type = 'choropleth',
    locations = ~Company_Location_Code3,
    z = ~Avg_Salary_USD,
<<<<<<< HEAD
    text = ~Country,
    colorscale = 'Blues',
=======
    text = ~Company_Location_Name,
    colorscale = "Blues",
>>>>>>> c9839f3abc33fa5a631bce80224ac3a4ed2b2b2a
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
}

<<<<<<< HEAD

ui = fluidPage(theme = shinytheme('darkly'),
               navbarPage('Where are Data Professionals needed the most?',
                          tabPanel('App', 
                                   h2('This Shiny App allows you to analyze salaries in the Data space by',
                                      style = 'text-align:left; font-weight: bold;'),
                                   h3('Field,  Job Title,  Experience Level,  Time Arrangement,  Office Arrangement,  Company Size,  Continent,  & Country', 
                                      style = 'text-align:left'),
                          )
               ),	
               navbarPage('Manual', 
                          tags$div(style = 'text-align:left; margin:auto;',
                                   tags$h4('Dataset Overview'),
                                   tags$ul(
                                     tags$li('Source: ',
                                             tags$a(
                                               href = 'https://www.kaggle.com/datasets/lorenzovzquez/data-jobs-salaries', 
                                               'Kaggle – Data Jobs Salaries',
                                               target = '_blank')),
                                     tags$li('Content: Global salary information for AI, ML, and Data Science jobs'),
                                     tags$li('Years Covered: 2020–2024 (data collection increased over time)'),
                                     tableOutput('yearly_count_table'),
                                     tags$li('Country Coverage: ~80 countries represented; 49 countries have 2024 data; missing China, Russia, Colombia, Peru, and most of Africa')
                                   ),
                                   tags$h4('Modifications'),
                                   tags$ul(
                                     tags$li('Column names and factor levels standardized for clarity'),
                                     tags$li('Added Field and Continent columns to group job titles and countries')
                                   ),
                                   tags$h4('App Usage'),
                                   tags$ul(
                                     tags$li('Main Filters (Histogram tab): Apply to ALL Plots; includes Select All & Clear All buttons; filters are interdependent (choosing one option limits others).'),
                                     tags$li('Example: Choosing AI in Field limits Job Title to AI roles.'),
                                     tags$li('Additional Filters (other tabs): Apply only to the current tab; allow grouping and sub-grouping.'),
                                     tags$li('Tip: Selecting Field or Continent first simplifies navigation; using Select All and then removing unwanted options is fastest.'),
                                     tags$li('Note: Part-time salaries can slightly skew averages but make up a small portion of the data.')
                                   ),
                                   tags$h4('Characteristics / Filter Variables'),
                                   tags$ol(
                                     tags$li('Field – 8 custom categories covering 155 job titles (2024 subset)'),
                                     tags$li('Experience Level – Entry, Mid, Senior, Executive'),
                                     tags$li('Job Title – 155 job titles; best to choose Field first'),
                                     tags$li('Work Time Arrangement – Full-time, Part-time, Contract, Freelance'),
                                     tags$li('Work Office Arrangement – In-Person (<20% remote), Hybrid (20–80% remote), Remote (>80% remote)'),
                                     tags$li('Company Size – S: <50, M: 50–250, L: >250 employees'),
                                     tags$li('Continent – Americas, Europe, Oceania, Asia, Africa'),
                                     tags$li('Country – 49 countries; best to choose Continent first')
                                   ),
                                   tags$h4('Plots'),
                                   tags$ol(
                                     tags$li('Histogram with ECDF – Average salary distribution (2024 only) with ECDF line.'),
                                     tags$li('Violin – Frequency distribution by grouping and sub-grouping.'),
                                     tags$li('Bar – Average salary by grouping and sub-grouping.'),
                                     tags$li('Pie-Sunburst – Percentages by grouping and sub-grouping; switches to Sunburst when sub-group is chosen.'),
                                     tags$li('Annual Trend – Salary trends over 2020–2024.'),
                                     tags$li('Annual Trend Breakdown – Salary trends broken down by grouping.'),
                                     tags$li('World Map – Average salary by country (2024 only); darker blue = higher salaries; white = no data.')
                                   )
                          )
               ),
               navbarPage('Explore', 
                          div('Data Salaries Dashboard', class = 'my-title-panel'),
                          tabsetPanel(
                            tabPanel('Histogram',
                                     sidebarLayout(
                                       sidebarPanel(
                                         div(strong('Field'),
                                             fluidRow(column(6, actionButton('select_all_Field', 'Select All')),
                                                      column(6, actionButton('clear_all_Field', 'Clear All'))),
                                             selectizeInput('Field', NULL, choices = fields, multiple = TRUE, options = list(plugins = list('remove_button')))),
                                         
                                         div(strong('Job Title'),
                                             fluidRow(column(6, actionButton('select_all_Job_Title', 'Select All')),
                                                      column(6, actionButton('clear_all_Job_Title', 'Clear All'))),
                                             selectizeInput('Job_Title', NULL, choices = titles, multiple = TRUE, options = list(plugins = list('remove_button')))),
                                         
                                         div(strong('Experience Level'),
                                             fluidRow(column(6, actionButton('select_all_Experience_Level', 'Select All')),
                                                      column(6, actionButton('clear_all_Experience_Level', 'Clear All'))),
                                             selectizeInput('Experience_Level', NULL, choices = exp_lvls, multiple = TRUE, options = list(plugins = list('remove_button')))),
                                         
                                         div(strong('Work Time Arrangement'),
                                             fluidRow(column(6, actionButton('select_all_Work_Time_Arrangement', 'Select All')),
                                                      column(6, actionButton('clear_all_Work_Time_Arrangement', 'Clear All'))),
                                             selectizeInput('Work_Time_Arrangement', NULL, choices = times, multiple = TRUE, options = list(plugins = list('remove_button')))),
                                         
                                         div(strong('Work Office Arrangement'),
                                             fluidRow(column(6, actionButton('select_all_Work_Office_Arrangement', 'Select All')),
                                                      column(6, actionButton('clear_all_Work_Office_Arrangement', 'Clear All'))),
                                             selectizeInput('Work_Office_Arrangement', NULL, choices = office_arrangements, multiple = TRUE, options = list(plugins = list('remove_button')))),
                                         
                                         div(strong('Company Size'),
                                             fluidRow(column(6, actionButton('select_all_Company_Size', 'Select All')),
                                                      column(6, actionButton('clear_all_Company_Size', 'Clear All'))),
                                             selectizeInput('Company_Size', NULL, choices = sizes, multiple = TRUE, options = list(plugins = list('remove_button')))),
                                         
                                         div(strong('Continent'),
                                             fluidRow(column(6, actionButton('select_all_Continent', 'Select All')),
                                                      column(6, actionButton('clear_all_Continent', 'Clear All'))),
                                             selectizeInput('Continent', NULL, choices = continents, multiple = TRUE, options = list(plugins = list('remove_button')))),
                                         
                                         div(strong('Country'),
                                             fluidRow(column(6, actionButton('select_all_Country', 'Select All')),
                                                      column(6, actionButton('clear_all_Country', 'Clear All'))),
                                             selectizeInput('Country', NULL, choices = countries, multiple = TRUE, options = list(plugins = list('remove_button'))))
                                       ),
                                       mainPanel(plotlyOutput('salary_plot', height = '800px'))
                                     )
                            ),
                            
                            tabPanel('Violin',
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput('violin_lvl', 'Grouping', 
                                                     choices = c('Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                                                 'Work_Office_Arrangement', 'Company_Size', 'Continent', 'Country'), 
                                                     selected = 'Field'),
                                         selectInput('violin_subLvl', 'Sub-Grouping', 
                                                     choices = c('NULL', 'Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                                                 'Work_Office_Arrangement', 'Company_Size', 'Continent', 'Country'), 
                                                     selected = 'NULL')
                                       ),
                                       mainPanel(plotlyOutput('violin_plot', height = '800px'))
                                     )
                            ),
                            
                            tabPanel('Bar',
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput('bar_lvl', 'Grouping', 
                                                     choices = c('Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                                                 'Work_Office_Arrangement', 'Company_Size', 'Continent', 'Country'), 
                                                     selected = 'Field'),
                                         selectInput('bar_subLvl', 'Sub-Grouping', 
                                                     choices = c('NULL', 'Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                                                 'Work_Office_Arrangement', 'Company_Size', 'Continent', 'Country'), 
                                                     selected = 'NULL')
                                       ),
                                       mainPanel(plotlyOutput('bar_plot', height = '800px'))
                                     )
                            ),
                            
                            tabPanel('Pie-Sunburst',
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput('pie_lvl', 'Grouping', 
                                                     choices = c('Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                                                 'Work_Office_Arrangement', 'Company_Size', 'Continent', 'Country'), 
                                                     selected = 'Field'),
                                         selectInput('sunburst_subLvl', 'Sub-Grouping', 
                                                     choices = c('NULL', 'Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                                                 'Work_Office_Arrangement', 'Company_Size', 'Continent', 'Country'), 
                                                     selected = 'NULL')
                                       ),
                                       mainPanel(plotlyOutput('pie_sunburst', height = '800px'))
                                     )
                            ),
                            
                            tabPanel('Annual Trend',
                                     mainPanel(plotlyOutput('annual_trend_plot', height = '800px'))
                            ),
                            
                            tabPanel('Annual Trend- Breakdown',
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput('trend_subLvl', 'Grouping', 
                                                     choices = c('Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                                                 'Work_Office_Arrangement', 'Company_Size', 'Continent', 'Country'), 
                                                     selected = 'Field')
                                       ),
                                       mainPanel(plotlyOutput('annual_trend_plot_sub', height = '800px'))
                                     )
                            ),
                            
                            tabPanel('World Map',
                                     mainPanel(plotlyOutput('world_map_plot', height = '1200px'))
                            )
                          )
               )
)



server = function(input, output, session) {
  
  output$yearly_count_table = renderTable({
    yearly_job_count
  }, striped = TRUE, bordered = TRUE)
  
  #Reactive filtered data
  filtered_data = reactive({
    df = data
    if (length(input$Field)) df = df %>% filter(Field %in% input$Field)
    if (length(input$Job_Title)) df = df %>% filter(Job_Title %in% input$Job_Title)
    if (length(input$Experience_Level)) df = df %>% filter(Experience_Level %in% input$Experience_Level)
    if (length(input$Work_Time_Arrangement)) df = df %>% filter(Work_Time_Arrangement %in% input$Work_Time_Arrangement)
    if (length(input$Work_Office_Arrangement)) df = df %>% filter(Work_Office_Arrangement %in% input$Work_Office_Arrangement)
    if (length(input$Company_Size)) df = df %>% filter(Company_Size %in% input$Company_Size)
    if (length(input$Continent)) df = df %>% filter(Continent %in% input$Continent)
    if (length(input$Country)) df = df %>% filter(Country %in% input$Country)
    df
  })
  
  filtered_data_2024 = reactive({ filtered_data() %>% filter(Work_Year == 2024) })
  
  #Dynamically updated choices, Select & Clear all
  update_choices = function(inputId, column) {
    observe({
      df = filtered_data()
      valid_choices = sort(unique(df[[column]]))
      updateSelectizeInput(session, inputId,
                           choices = valid_choices,
                           selected = input[[inputId]][input[[inputId]] %in% valid_choices])
    })
    
    observeEvent(input[[paste0('select_all_', inputId)]], {
      updateSelectizeInput(session, inputId, selected = sort(unique(filtered_data()[[column]])))
    })
    observeEvent(input[[paste0('clear_all_', inputId)]], {
      updateSelectizeInput(session, inputId, selected = character(0))
    })
  }
  
  update_choices('Field', 'Field')
  update_choices('Job_Title', 'Job_Title')
  update_choices('Experience_Level', 'Experience_Level')
  update_choices('Work_Time_Arrangement', 'Work_Time_Arrangement')
  update_choices('Work_Office_Arrangement', 'Work_Office_Arrangement')
  update_choices('Company_Size', 'Company_Size')
  update_choices('Continent', 'Continent')
  update_choices('Country', 'Country')
  
  
  #Plot Output
  
  output$salary_plot = renderPlotly({
    Histogram_ECDF(filtered_data_2024())
  })
  
  output$violin_plot = renderPlotly({
    Violin_2lvl(filtered_data_2024(), input$violin_lvl, input$violin_subLvl)
  })
  
  output$bar_plot = renderPlotly({
    Bar_2lvl(filtered_data_2024(), input$bar_lvl, input$bar_subLvl)
  })
  
  output$pie_sunburst = renderPlotly({
    Pie_Sunburst(filtered_data_2024(), input$pie_lvl, input$sunburst_subLvl)
  })
  
  output$annual_trend_plot = renderPlotly({
    Bar_Trend_Salary(filtered_data())
  })
  
  output$annual_trend_plot_sub = renderPlotly({
    Bar_Trend_Salary_Sub(filtered_data(), input$trend_subLvl)
  })
  
  output$world_map_plot = renderPlotly({
    df = filtered_data_2024()
    if (nrow(df) == 0) return(NULL)
    df_summary = df %>%
      group_by(Country, Company_Location_Code3) %>%
      summarise(Avg_Salary_USD = mean(Salary_USD, na.rm = TRUE), .groups = 'drop')
=======
# ==== UI ====
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      /* Title panel bold and blue */
      .my-title-panel {
        background-color: blue
        font-weight: bold;
        color: white;
        padding: 15px;
        text-align: left;
        font-size: 40px;
      }

      /* Tabs */
      .nav-tabs > li > a {
        background-color: orange !important;
        color: black !important;
        font-weight: bold;
      }
      .nav-tabs > li[class=active] > a {
        background-color: orange !important;
        color: black !important;
      }

      /* Sidebar */
      .well {
        background-color: orange !important;
        color: black !important;
      }
      .well label {
        font-weight: bold;
      }

      /* Plotly full height */
      .plotly {
        height: calc(100vh - 200px) !important;
      }

      /* Body background blue to match title panel */
      body {
        background-color: blue;
      }
    "))
  ),
  
  div("Data Salaries Dashboard", class = "my-title-panel"),
  
  tabsetPanel(
    # Tab 1: Salary Histogram
    tabPanel("Salary Histogram",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(6, actionButton("select_all_Field", "Select All")),
                          column(6, actionButton("clear_all_Field", "Clear All"))),
                 selectInput("Field", "Field", choices = fields, multiple = TRUE),
                 
                 fluidRow(column(6, actionButton("select_all_Title", "Select All")),
                          column(6, actionButton("clear_all_Title", "Clear All"))),
                 selectInput("Job_Title", "Job Title", choices = titles, multiple = TRUE),
                 
                 fluidRow(column(6, actionButton("select_all_Exp", "Select All")),
                          column(6, actionButton("clear_all_Exp", "Clear All"))),
                 selectInput("Experience_Level", "Experience Level", choices = exp_lvls, multiple = TRUE),
                 
                 fluidRow(column(6, actionButton("select_all_Time", "Select All")),
                          column(6, actionButton("clear_all_Time", "Clear All"))),
                 selectInput("Work_Time_Arrangement", "Work Time Arrangement", choices = times, multiple = TRUE),
                 
                 fluidRow(column(6, actionButton("select_all_Office", "Select All")),
                          column(6, actionButton("clear_all_Office", "Clear All"))),
                 selectInput("Work_Office_Arrangement", "Work Office Arrangement", choices = office_arrangements, multiple = TRUE),
                 
                 fluidRow(column(6, actionButton("select_all_Continent", "Select All")),
                          column(6, actionButton("clear_all_Continent", "Clear All"))),
                 selectInput("Continent", "Continent", choices = continents, multiple = TRUE),
                 
                 fluidRow(column(6, actionButton("select_all_Country", "Select All")),
                          column(6, actionButton("clear_all_Country", "Clear All"))),
                 selectInput("Company_Location_Name", "Country", choices = countries, multiple = TRUE),
                 
                 fluidRow(column(6, actionButton("select_all_Size", "Select All")),
                          column(6, actionButton("clear_all_Size", "Clear All"))),
                 selectInput("Company_Size", "Company Size", choices = sizes, multiple = TRUE)
               ),
               mainPanel(plotlyOutput("salary_plot", height = "800px"))
             )
    ),
    
    # Tab 2: Annual Trend
    tabPanel("Annual Trend",
             sidebarLayout(
               sidebarPanel(
                 selectInput("trend_subLvl", "Color By", 
                             choices = c("Field", "Job_Title", "Experience_Level", "Company_Size"), 
                             selected = "Job_Title")
               ),
               mainPanel(plotlyOutput("annual_trend_plot", height = "800px"))
             )
    ),
    
    # Tab 3: World Map
    tabPanel("World Map",
             mainPanel(plotlyOutput("world_map_plot", height = "1200px"))
    )
  )
)

# ==== Server ====
server <- function(input, output, session) {
  
  # --- Select All / Clear All ---
  observeEvent(input$select_all_Field, updateSelectInput(session, "Field", selected = fields))
  observeEvent(input$clear_all_Field, updateSelectInput(session, "Field", selected = character(0)))
  
  observeEvent(input$select_all_Title, updateSelectInput(session, "Job_Title", selected = titles))
  observeEvent(input$clear_all_Title, updateSelectInput(session, "Job_Title", selected = character(0)))
  
  observeEvent(input$select_all_Exp, updateSelectInput(session, "Experience_Level", selected = exp_lvls))
  observeEvent(input$clear_all_Exp, updateSelectInput(session, "Experience_Level", selected = character(0)))
  
  observeEvent(input$select_all_Time, updateSelectInput(session, "Work_Time_Arrangement", selected = times))
  observeEvent(input$clear_all_Time, updateSelectInput(session, "Work_Time_Arrangement", selected = character(0)))
  
  observeEvent(input$select_all_Office, updateSelectInput(session, "Work_Office_Arrangement", selected = office_arrangements))
  observeEvent(input$clear_all_Office, updateSelectInput(session, "Work_Office_Arrangement", selected = character(0)))
  
  observeEvent(input$select_all_Continent, updateSelectInput(session, "Continent", selected = continents))
  observeEvent(input$clear_all_Continent, updateSelectInput(session, "Continent", selected = character(0)))
  
  observeEvent(input$select_all_Country, updateSelectInput(session, "Company_Location_Name", selected = countries))
  observeEvent(input$clear_all_Country, updateSelectInput(session, "Company_Location_Name", selected = character(0)))
  
  observeEvent(input$select_all_Size, updateSelectInput(session, "Company_Size", selected = sizes))
  observeEvent(input$clear_all_Size, updateSelectInput(session, "Company_Size", selected = character(0)))
  
  # --- Filtered data ---
  filtered_data <- reactive({
    df <- data
    if (!is.null(input$Field) && length(input$Field) > 0) df <- df %>% filter(Field %in% input$Field)
    if (!is.null(input$Job_Title) && length(input$Job_Title) > 0) df <- df %>% filter(Job_Title %in% input$Job_Title)
    if (!is.null(input$Experience_Level) && length(input$Experience_Level) > 0) df <- df %>% filter(Experience_Level %in% input$Experience_Level)
    if (!is.null(input$Work_Time_Arrangement) && length(input$Work_Time_Arrangement) > 0) df <- df %>% filter(Work_Time_Arrangement %in% input$Work_Time_Arrangement)
    if (!is.null(input$Work_Office_Arrangement) && length(input$Work_Office_Arrangement) > 0) df <- df %>% filter(Work_Office_Arrangement %in% input$Work_Office_Arrangement)
    if (!is.null(input$Continent) && length(input$Continent) > 0) df <- df %>% filter(Continent %in% input$Continent)
    if (!is.null(input$Company_Location_Name) && length(input$Company_Location_Name) > 0) df <- df %>% filter(Company_Location_Name %in% input$Company_Location_Name)
    if (!is.null(input$Company_Size) && length(input$Company_Size) > 0) df <- df %>% filter(Company_Size %in% input$Company_Size)
    df
  })
  
  # --- Salary Histogram ---
  output$salary_plot <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    plot_ly(df, x = ~Salary_USD) %>%
      add_histogram(name = "Salary Distribution", nbinsx = 50, opacity = 0.7) %>%
      add_trace(
        x = ~sort(Salary_USD),
        y = ~ecdf(Salary_USD)(sort(Salary_USD)),
        type = 'scatter',
        mode = 'lines',
        name = 'ECDF',
        yaxis = 'y2'
      ) %>%
      layout(
        title = "Salary Distribution with ECDF",
        xaxis = list(title = "Salary (USD)"),
        yaxis = list(title = "Count"),
        yaxis2 = list(
          title = "ECDF",
          overlaying = "y",
          side = "right",
          range = c(0, 1)
        ),
        bargap = 0.01
      )
  })
  
  # --- Annual Trend ---
  output$annual_trend_plot <- renderPlotly({
    df <- filtered_data()
    Bar_Trend_Salary(df, input$trend_subLvl)
  })
  
  # --- World Map ---
  output$world_map_plot <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    df_summary <- df %>%
      group_by(Company_Location_Name, Company_Location_Code3) %>%
      summarise(Avg_Salary_USD = mean(Salary_USD, na.rm = TRUE), .groups = "drop")
    
>>>>>>> c9839f3abc33fa5a631bce80224ac3a4ed2b2b2a
    World_Map_by_Salary(df_summary)
  })
}

<<<<<<< HEAD
=======
# ==== Run App ====
>>>>>>> c9839f3abc33fa5a631bce80224ac3a4ed2b2b2a
shinyApp(ui, server)
