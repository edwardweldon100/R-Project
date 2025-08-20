library(shiny)
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

data = factor_mutation(data)
data_2024 = data %>% filter(Work_Year == 2024)

fields = levels(data$Field)
exp_lvls = levels(data$Experience_Level)
titles = sort(unique(data$Job_Title))
times = levels(data$Work_Time_Arrangement)
office_arrangements = levels(data$Work_Office_Arrangement)
continents = levels(data$Continent)
countries = sort(unique(data$Company_Location_Name))
sizes = levels(data$Company_Size)


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

Pie_1lvl = function(df_summary, lvl, subLvl = 'NULL') {
  plot_ly(
    data = df_summary,
    labels = ~get(lvl),
    values = ~Count,
    type = "pie"
  ) %>%
    layout(
      title = paste("% of Jobs by", lvl)
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
  ) %>% layout(title = paste("% of rows by", lvl, subLvl))
}

Pie_Sunburst = function(df, lvl, subLvl) {
  if (is.null(subLvl) || subLvl == 'NULL') {
    Pie_1lvl(Summarise_Salary(df, lvl), lvl)}
  else {Sunburst_2lvl(Summarise_Salary(df, c(lvl, subLvl)), lvl, subLvl)}
}

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
  
  plot_ly(
    data = df_summary,
    x = ~Work_Year,
    y = ~Value,
    color = ~get(subLvl),
    type = 'bar'
  ) %>%
    layout(
      xaxis = list(title = paste('Pay Trend by', subLvl)),
      yaxis = list(title = 'Salary USD (avg)'),
      barmode = 'group'
    )
}


ui = fluidPage(
  tags$head(
    tags$style(HTML('
      .my-title-panel {
        background-color: darkblue;
        font-weight: bold;
        color: white;
        padding: 15px;
        text-align: left;
        font-size: 40px;
      }
      .nav-tabs > li > a {
        background-color: orange !important;
        color: black !important;
        font-weight: bold;
      }
      .nav-tabs > li[class=active] > a {
        background-color: darkorange !important;
        color: black !important;
      }
      .well {
        background-color: darkorange !important;
        color: black !important;
      }
      .well label { font-weight: bold; }
      body { background-color: darkblue; }
    '))
  ),
  
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
                     fluidRow(column(6, actionButton('select_all_Title', 'Select All')),
                              column(6, actionButton('clear_all_Title', 'Clear All'))),
                     selectizeInput('Job_Title', NULL, choices = titles, multiple = TRUE, options = list(plugins = list('remove_button')))),
                 
                 div(strong('Experience Level'),
                     fluidRow(column(6, actionButton('select_all_Exp', 'Select All')),
                              column(6, actionButton('clear_all_Exp', 'Clear All'))),
                     selectizeInput('Experience_Level', NULL, choices = exp_lvls, multiple = TRUE, options = list(plugins = list('remove_button')))),
                 
                 div(strong('Work Time Arrangement'),
                     fluidRow(column(6, actionButton('select_all_Time', 'Select All')),
                              column(6, actionButton('clear_all_Time', 'Clear All'))),
                     selectizeInput('Work_Time_Arrangement', NULL, choices = times, multiple = TRUE, options = list(plugins = list('remove_button')))),
                 
                 div(strong('Work Office Arrangement'),
                     fluidRow(column(6, actionButton('select_all_Office', 'Select All')),
                              column(6, actionButton('clear_all_Office', 'Clear All'))),
                     selectizeInput('Work_Office_Arrangement', NULL, choices = office_arrangements, multiple = TRUE, options = list(plugins = list('remove_button')))),
                 
                 div(strong('Continent'),
                     fluidRow(column(6, actionButton('select_all_Continent', 'Select All')),
                              column(6, actionButton('clear_all_Continent', 'Clear All'))),
                     selectizeInput('Continent', NULL, choices = continents, multiple = TRUE, options = list(plugins = list('remove_button')))),
                 
                 div(strong('Country'),
                     fluidRow(column(6, actionButton('select_all_Country', 'Select All')),
                              column(6, actionButton('clear_all_Country', 'Clear All'))),
                     selectizeInput('Company_Location_Name', NULL, choices = countries, multiple = TRUE, options = list(plugins = list('remove_button')))),
                 
                 div(strong('Company Size'),
                     fluidRow(column(6, actionButton('select_all_Size', 'Select All')),
                              column(6, actionButton('clear_all_Size', 'Clear All'))),
                     selectizeInput('Company_Size', NULL, choices = sizes, multiple = TRUE, options = list(plugins = list('remove_button'))))
               ),
               mainPanel(plotlyOutput('salary_plot', height = '800px'))
             )
    ),
    
    tabPanel('Violin',
             sidebarLayout(
               sidebarPanel(
                 selectInput('violin_lvl', 'Grouping', 
                             choices = c('Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                         'Work_Office_Arrangement', 'Continent', 'Company_Location_Name', 'Company_Size'), 
                             selected = 'Field'),
                 selectInput('violin_subLvl', 'Sub-Grouping', 
                             choices = c('NULL', 'Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                         'Work_Office_Arrangement', 'Continent', 'Company_Location_Name', 'Company_Size'), 
                             selected = 'NULL')
               ),
               mainPanel(plotlyOutput('violin_plot', height = '800px'))
             )
    ),
    
    tabPanel('Pie-Sunburst',
             sidebarLayout(
               sidebarPanel(
                 selectInput('pie_lvl', 'Grouping', 
                             choices = c('Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                         'Work_Office_Arrangement', 'Continent', 'Company_Location_Name', 'Company_Size'), 
                             selected = 'Field'),
                 selectInput('sunburst_subLvl', 'Sub-Grouping', 
                             choices = c('NULL', 'Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                         'Work_Office_Arrangement', 'Continent', 'Company_Location_Name', 'Company_Size'), 
                             selected = 'NULL')
               ),
               mainPanel(plotlyOutput('pie_sunburst', height = '800px'))
             )
    ),
    
    tabPanel('World Map',
             mainPanel(plotlyOutput('world_map_plot', height = '1200px'))
    ),
    
    tabPanel('Bar',
             sidebarLayout(
               sidebarPanel(
                 selectInput('bar_lvl', 'Grouping', 
                             choices = c('Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                         'Work_Office_Arrangement', 'Continent', 'Company_Location_Name', 'Company_Size'), 
                             selected = 'Field'),
                 selectInput('bar_subLvl', 'Sub-Grouping', 
                             choices = c('NULL', 'Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                         'Work_Office_Arrangement', 'Continent', 'Company_Location_Name', 'Company_Size'), 
                             selected = 'NULL')
               ),
               mainPanel(plotlyOutput('bar_plot', height = '800px'))
             )
    ),
    tabPanel('Annual Trend',
             mainPanel(plotlyOutput('annual_trend_plot', height = '800px'))
    ),
    
    tabPanel('Annual Trend- Breakdown',
             sidebarLayout(
               sidebarPanel(
                 selectInput('trend_subLvl', 'Color By', 
                             choices = c('Field', 'Job_Title', 'Experience_Level', 'Work_Time_Arrangement',
                                         'Work_Office_Arrangement', 'Continent', 'Company_Location_Name', 'Company_Size'), 
                             selected = 'Field')
               ),
               mainPanel(plotlyOutput('annual_trend_plot_sub', height = '800px'))
             )
    )
  )
)



server = function(input, output, session) {
  
  #Reactive filtered data
  filtered_data = reactive({
    df = data
    if (length(input$Field)) df = df %>% filter(Field %in% input$Field)
    if (length(input$Job_Title)) df = df %>% filter(Job_Title %in% input$Job_Title)
    if (length(input$Experience_Level)) df = df %>% filter(Experience_Level %in% input$Experience_Level)
    if (length(input$Work_Time_Arrangement)) df = df %>% filter(Work_Time_Arrangement %in% input$Work_Time_Arrangement)
    if (length(input$Work_Office_Arrangement)) df = df %>% filter(Work_Office_Arrangement %in% input$Work_Office_Arrangement)
    if (length(input$Continent)) df = df %>% filter(Continent %in% input$Continent)
    if (length(input$Company_Location_Name)) df = df %>% filter(Company_Location_Name %in% input$Company_Location_Name)
    if (length(input$Company_Size)) df = df %>% filter(Company_Size %in% input$Company_Size)
    df
  })
  
  filtered_data_2024 = reactive({ filtered_data() %>% filter(Work_Year == 2024) })
  
  #Dynamically updated choices, Select & Clear all
  update_choices = function(inputId, column) {
    observe({
      df = filtered_data()
      valid_choices = sort(unique(df[[column]]))
      updateSelectInput(session, inputId,
                        choices = valid_choices,
                        selected = input[[inputId]][input[[inputId]] %in% valid_choices])
    })
    
    observeEvent(input[[paste0('select_all_', inputId)]], {
      updateSelectInput(session, inputId, selected = sort(unique(filtered_data()[[column]])))
    })
    observeEvent(input[[paste0('clear_all_', inputId)]], {
      updateSelectInput(session, inputId, selected = character(0))
    })
  }
  
  update_choices('Field', 'Field')
  update_choices('Job_Title', 'Job_Title')
  update_choices('Experience_Level', 'Experience_Level')
  update_choices('Work_Time_Arrangement', 'Work_Time_Arrangement')
  update_choices('Work_Office_Arrangement', 'Work_Office_Arrangement')
  update_choices('Continent', 'Continent')
  update_choices('Company_Location_Name', 'Company_Location_Name')
  update_choices('Company_Size', 'Company_Size')
  
  
  #Plot Output
  
  output$salary_plot = renderPlotly({
    Histogram_ECDF(filtered_data_2024())
  })
  
  output$world_map_plot = renderPlotly({
    df = filtered_data_2024()
    if (nrow(df) == 0) return(NULL)
    df_summary = df %>%
      group_by(Company_Location_Name, Company_Location_Code3) %>%
      summarise(Avg_Salary_USD = mean(Salary_USD, na.rm = TRUE), .groups = 'drop')
    World_Map_by_Salary(df_summary)
  })
  
  output$violin_plot = renderPlotly({
    Violin_2lvl(filtered_data_2024(), input$violin_lvl, input$violin_subLvl)
  })
  
  output$pie_sunburst = renderPlotly({
    Pie_Sunburst(filtered_data_2024(), input$pie_lvl, input$sunburst_subLvl)
  })
  
  output$bar_plot = renderPlotly({
    Bar_2lvl(filtered_data_2024(), input$bar_lvl, input$bar_subLvl)
  })
  
  output$annual_trend_plot = renderPlotly({
    Bar_Trend_Salary(filtered_data())
  })
  
  output$annual_trend_plot_sub = renderPlotly({
    Bar_Trend_Salary_Sub(filtered_data(), input$trend_subLvl)
  })
}

shinyApp(ui, server)
