library(shiny)
library(dplyr)
library(plotly)

data = read.csv("data/Data Salaries- Shiny.csv", stringsAsFactors = FALSE)

fields = sort(unique(data$Field))
exp_lvls = sort(unique(data$Experience_Level))
titles = sort(unique(data$Job_Title))
times = sort(unique(data$Work_Time_Arrangement))
office_arrangements = sort(unique(data$Work_Office_Arrangement))
continents = sort(unique(data$Continent))
countries = sort(unique(data$Company_Location_Name))
sizes = sort(unique(data$Company_Size))

#Plot Functions
Bar_Trend_Salary = function(df, subLvl, numcol = 'Salary_USD', aggfun = mean) {
  df_summary = df %>%
    group_by(across(all_of(c('Work_Year', subLvl)))) %>%
    summarise(Value = aggfun(.data[[numcol]]), .groups = "drop")
  
  plot_ly(
    data = df_summary,
    x = ~Work_Year,
    y = ~Value,
    color = ~get(subLvl),
    type = "bar"
  ) %>%
    layout(
      xaxis = list(title = paste('Pay Trend by', subLvl)),
      yaxis = list(title = numcol),
      barmode = 'group'
    )
}

World_Map_by_Salary = function(df) {
  plot_ly(
    data = df,
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
}

ui = fluidPage(
  
  tags$head(
    tags$style(HTML("
#Title Panel
      .my-title-panel {
        background-color: blue
        font-weight: bold;
        color: white;
        padding: 15px;
        text-align: left;
        font-size: 40px;
      }
#Tabs
      .nav-tabs > li > a {
        background-color: orange !important;
        color: black !important;
        font-weight: bold;
      }
      .nav-tabs > li[class=active] > a {
        background-color: orange !important;
        color: black !important;
      }

#SideBar
      .well {
        background-color: orange !important;
        color: black !important;
      }
      .well label {
        font-weight: bold;
      }

#Plotly height adjustments
      .plotly {
        height: calc(100vh - 200px) !important;
      }

#Body Background
      body {
        background-color: blue;
      }
    "))
  ),
  
  div("Data Salaries Dashboard", class = "my-title-panel"),
  
  tabsetPanel(
    #Tab 1- Histogram
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
    
    #Tab 2- Annual Trend
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
    
    #Tab 3- World Map
    tabPanel("World Map",
             mainPanel(plotlyOutput("world_map_plot", height = "1200px"))
    )
  )
)

server = function(input, output, session) {
  
  #Select & Clear All Options
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
  
  #Data Filters
  filtered_data = reactive({
    df = data
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
  
  #Histogram
  output$salary_plot = renderPlotly({
    df = filtered_data()
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
  
  #Annual Trend
  output$annual_trend_plot = renderPlotly({
    df = filtered_data()
    Bar_Trend_Salary(df, input$trend_subLvl)
  })
  
  #World Map
  output$world_map_plot = renderPlotly({
    df = filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    df_summary = df %>%
      group_by(Company_Location_Name, Company_Location_Code3) %>%
      summarise(Avg_Salary_USD = mean(Salary_USD, na.rm = TRUE), .groups = "drop")
    
    World_Map_by_Salary(df_summary)
  })
}

shinyApp(ui, server)
