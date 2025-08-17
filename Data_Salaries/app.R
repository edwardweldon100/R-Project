library(shiny)
library(dplyr)
library(plotly)

data = read.csv('data/Data Salaries- Shiny.csv', stringsAsFactors = FALSE)
data_2024 = data %>% filter(Work_Year == 2024)

factor_mutation = function(df) {
  df %>% mutate(
    Field = factor(Field,
                   levels = c('Data Engineering', 'Data Science', 'Data Analysis', 'Business Intelligence', 'Machine Learning', 
                              'Artificial Intelligence   (general)', 'Software Development   (general)', 'Research & Development   (general)')),
    Experience_Level = factor(Experience_Level,
                              levels = c('Entry', 'Mid', 'Senior', 'Executive')),
    Work_Time_Arrangement = factor(Work_Time_Arrangement,
                                   levels = c('Full-time', 'Part-time', 'Contract', 'Freelance')),
    Work_Office_Arrangement = factor(Work_Office_Arrangement,
                                     levels = c('In-Person', 'Hybrid', 'Remote')),
    Continent = factor(Continent,
                       levels = c('Americas', 'Europe', 'Oceania', 'Asia', 'Africa')),
    Company_Size = factor(Company_Size,
                          levels = c('S', 'M', 'L'))
  )
}

data_2024 = factor_mutation(data_2024)

fields = levels(data_2024$Field)
exp_lvls = levels(data_2024$Experience_Level)
titles = sort(unique(data_2024$Job_Title))
times = levels(data_2024$Work_Time_Arrangement)
office_arrangements = levels(data_2024$Work_Office_Arrangement)
continents = levels(data_2024$Continent)
countries = sort(unique(data_2024$Company_Location_Name))
sizes = levels(data_2024$Company_Size)

#Plot Functions
World_Map_by_Salary = function(df) {
  plot_ly(
    data = df,
    type = 'choropleth',
    locations = ~Company_Location_Code3,
    z = ~Avg_Salary_USD,
    text = ~Company_Location_Name,
    colorscale = 'Blues',
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
      yaxis = list(title = numcol),
      barmode = 'group'
    )
}
Bar_Trend_Salary_Sub = function(df, subLvl, numcol = 'Salary_USD', aggfun = mean) {
  df_summary = df %>%
    group_by(across(all_of(c('Work_Year', subLvl)))) %>%
    summarise(Value = aggfun(.data[[numcol]]), .groups = 'drop')
  
  plot_ly(
    data = df_summary,
    x = ~Work_Year,
    y = ~Value,
    color = ~get(subLvl),
    type = 'bar'
  ) %>%
    layout(
      xaxis = list(title = paste('Pay Trend by', subLvl)),
      yaxis = list(title = numcol),
      barmode = 'group'
    )
}
ui = fluidPage(
  
  tags$head(
    tags$style(HTML('
      /* Title panel bold and blue */
      .my-title-panel {
        background-color: darkblue;
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
        background-color: darkorange !important;
        color: black !important;
      }

      /* Sidebar */
      .well {
        background-color: darkorange !important;
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
        background-color: darkblue;
      }
    '))
  ),
  
  div('Data Salaries Dashboard', class = 'my-title-panel'),
  
  tabsetPanel(
    #Tab 1- Histogram
    tabPanel('Histogram',
             sidebarLayout(
               sidebarPanel(
                 div(
                   strong('Field'),
                   fluidRow(
                     column(6, actionButton('select_all_Field', 'Select All')),
                     column(6, actionButton('clear_all_Field', 'Clear All'))
                   ),
                   selectInput('Field', NULL, choices = fields, multiple = TRUE)
                 ),
                 div(
                   strong('Job Title'),
                   fluidRow(
                     column(6, actionButton('select_all_Title', 'Select All')),
                     column(6, actionButton('clear_all_Title', 'Clear All'))
                   ),
                   selectInput('Job_Title', NULL, choices = titles, multiple = TRUE)
                 ),
                 div(
                   strong('Experience Level'),
                   fluidRow(
                     column(6, actionButton('select_all_Exp', 'Select All')),
                     column(6, actionButton('clear_all_Exp', 'Clear All'))
                   ),
                   selectInput('Experience_Level', NULL, choices = exp_lvls, multiple = TRUE)
                 ),
                 div(
                   strong('Work Time Arrangement'),
                   fluidRow(
                     column(6, actionButton('select_all_Time', 'Select All')),
                     column(6, actionButton('clear_all_Time', 'Clear All'))
                   ),
                   selectInput('Work_Time_Arrangement', NULL, choices = times, multiple = TRUE)
                 ),
                 div(
                   strong('Work Office Arrangement'),
                   fluidRow(
                     column(6, actionButton('select_all_Office', 'Select All')),
                     column(6, actionButton('clear_all_Office', 'Clear All'))
                   ),
                   selectInput('Work_Office_Arrangement', NULL, choices = office_arrangements, multiple = TRUE)
                 ),
                 div(
                   strong('Continent'),
                   fluidRow(
                     column(6, actionButton('select_all_Continent', 'Select All')),
                     column(6, actionButton('clear_all_Continent', 'Clear All'))
                   ),
                   selectInput('Continent', NULL, choices = continents, multiple = TRUE)
                 ),
                 div(
                   strong('Country'),
                   fluidRow(
                     column(6, actionButton('select_all_Country', 'Select All')),
                     column(6, actionButton('clear_all_Country', 'Clear All'))
                   ),
                   selectInput('Company_Location_Name', NULL, choices = countries, multiple = TRUE)
                 ),
                 div(
                   strong('Company Size'),
                   fluidRow(
                     column(6, actionButton('select_all_Size', 'Select All')),
                     column(6, actionButton('clear_all_Size', 'Clear All'))
                   ),
                   selectInput('Company_Size', NULL, choices = sizes, multiple = TRUE)
                 )
               ),
               mainPanel(plotlyOutput('salary_plot', height = '800px'))
             )
    ),
    
    #Tab 2- World Map
    tabPanel('World Map',
             mainPanel(plotlyOutput('world_map_plot', height = '1200px'))
    ),
    
    #Tab 3- Annual Trend
    tabPanel('Annual Trend',
             mainPanel(plotlyOutput('annual_trend_plot', height = '800px'))
    ),
    
    #Tab 4- Annual Trend- Breakdown
    tabPanel('Annual Trend- Breakdown',
             sidebarLayout(
               sidebarPanel(
                 selectInput('trend_subLvl', 'Color By', 
                             choices = c('Field', 'Job_Title', 'Experience_Level', 'Company_Size'), 
                             selected = 'Job_Title')
               ),
               mainPanel(plotlyOutput('annual_trend_plot_sub', height = '800px'))
             )
    )
  )
)

server = function(input, output, session) {
  
  #Select & Clear All Options
  observeEvent(input$select_all_Field, updateSelectInput(session, 'Field', selected = fields))
  observeEvent(input$clear_all_Field, updateSelectInput(session, 'Field', selected = character(0)))
  
  observeEvent(input$select_all_Title, updateSelectInput(session, 'Job_Title', selected = titles))
  observeEvent(input$clear_all_Title, updateSelectInput(session, 'Job_Title', selected = character(0)))
  
  observeEvent(input$select_all_Exp, updateSelectInput(session, 'Experience_Level', selected = exp_lvls))
  observeEvent(input$clear_all_Exp, updateSelectInput(session, 'Experience_Level', selected = character(0)))
  
  observeEvent(input$select_all_Time, updateSelectInput(session, 'Work_Time_Arrangement', selected = times))
  observeEvent(input$clear_all_Time, updateSelectInput(session, 'Work_Time_Arrangement', selected = character(0)))
  
  observeEvent(input$select_all_Office, updateSelectInput(session, 'Work_Office_Arrangement', selected = office_arrangements))
  observeEvent(input$clear_all_Office, updateSelectInput(session, 'Work_Office_Arrangement', selected = character(0)))
  
  observeEvent(input$select_all_Continent, updateSelectInput(session, 'Continent', selected = continents))
  observeEvent(input$clear_all_Continent, updateSelectInput(session, 'Continent', selected = character(0)))
  
  observeEvent(input$select_all_Country, updateSelectInput(session, 'Company_Location_Name', selected = countries))
  observeEvent(input$clear_all_Country, updateSelectInput(session, 'Company_Location_Name', selected = character(0)))
  
  observeEvent(input$select_all_Size, updateSelectInput(session, 'Company_Size', selected = sizes))
  observeEvent(input$clear_all_Size, updateSelectInput(session, 'Company_Size', selected = character(0)))
  
  #Data Filters
  filtered_data = reactive({
    df = factor_mutation(data)
    if (!is.null(input$Field) && length(input$Field) > 0) df = df %>% filter(Field %in% input$Field)
    if (!is.null(input$Job_Title) && length(input$Job_Title) > 0) df = df %>% filter(Job_Title %in% input$Job_Title)
    if (!is.null(input$Experience_Level) && length(input$Experience_Level) > 0) df = df %>% filter(Experience_Level %in% input$Experience_Level)
    if (!is.null(input$Work_Time_Arrangement) && length(input$Work_Time_Arrangement) > 0) df = df %>% filter(Work_Time_Arrangement %in% input$Work_Time_Arrangement)
    if (!is.null(input$Work_Office_Arrangement) && length(input$Work_Office_Arrangement) > 0) df = df %>% filter(Work_Office_Arrangement %in% input$Work_Office_Arrangement)
    if (!is.null(input$Continent) && length(input$Continent) > 0) df = df %>% filter(Continent %in% input$Continent)
    if (!is.null(input$Company_Location_Name) && length(input$Company_Location_Name) > 0) df = df %>% filter(Company_Location_Name %in% input$Company_Location_Name)
    if (!is.null(input$Company_Size) && length(input$Company_Size) > 0) df = df %>% filter(Company_Size %in% input$Company_Size)
    df
  })
  filtered_data_2024 = reactive({filtered_data() %>% filter(Work_Year == 2024)})
  
  #Histogram
  output$salary_plot = renderPlotly({
    df = filtered_data_2024()
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
  })
  
  #Annual Trend
  output$annual_trend_plot = renderPlotly({
    df = filtered_data()
    Bar_Trend_Salary(df)
  })
  
  
  #Annual Trend- Sub Plot
  output$annual_trend_plot_sub = renderPlotly({
    df = filtered_data()
    Bar_Trend_Salary_Sub(df, input$trend_subLvl)
  })
  
  #World Map
  output$world_map_plot = renderPlotly({
    df = filtered_data_2024()
    if (nrow(df) == 0) return(NULL)
    
    df_summary = df %>%
      group_by(Company_Location_Name, Company_Location_Code3) %>%
      summarise(Avg_Salary_USD = mean(Salary_USD, na.rm = TRUE), .groups = 'drop')
    
    World_Map_by_Salary(df_summary)
  })
}

shinyApp(ui, server)
