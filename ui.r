
#shiny::runApp("//scan01/Users/yanzh/IJE/2020/code/shiny", host = "0.0.0.0", port=1300)

library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(DT)

ui<-fluidPage(
  includeCSS('www/style.css'),
  
  tags$h1("Inter-Jurisdictional Employment (IJE) Dashboard",align="center"),
  tabsetPanel(
 
  tabPanel("Using This App",
           mainPanel(
             p("Text goes here")
           )),
    
  tabPanel("National", 
           sidebarLayout(
             sidebarPanel( 
               selectInput("YearInput", "Select Year",
                           choices = c(beginy:endy),
                           selected =c(endy)),
               
               radioButtons("SeriesInput", "Select Series",
                            choices = c("Employees","Income"),
                            selected=c("Employees")),
               
               radioButtons("TypeInput", tags$div(HTML(paste0("Select Employment Type",tags$sup('3')))),
                            choices = c("Incoming","Outgoing"),
                            selected =c("Incoming")),   
               
               radioButtons("GenderInput", "Select Gender",
                         choices = c("Both","Male","Female"),
                         selected =c("Both")),
               
               radioButtons("IncomeSource", tags$div(HTML(paste0("Select Income Source",tags$sup('4')))),
                           choices = c("Inside the Jurisdiction","Outside the Jurisdiction"),
                           selected =c("Inside the Jurisdiction")),
               
               radioButtons("SourceInput", tags$div(HTML(paste0("Data Source",tags$sup('2')))),
                            choices = c("T1 Personal Master File", "T1 Historical File"),
                            selected = c("T1 Personal Master File")),
               
               selectInput("tableN", "Choose a table to download: ",
                           choices = c("Filtered National Table","Full National Table")),
               downloadButton("downloadNtable", "Download") 
       
             ),
             mainPanel(
               
               ## render object occurs here
               # leafletOutput("PRcount",height=600),
               plotlyOutput("mainmap",height=600
                          # ,click='map_click'
                          ),
               
               # br(),
               hr(),
               # plotOutput("PRbar"),
               # br(),
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. Data from T1 Personal Master File are derived from the T1 and T4 tax files. Data from T1 Historical File are derived from 
                        the T1, T4 and the T1 historical personal master file. The T1 historical file includes late and re-assessed tax filers. 
                        Because the T1 historical file is only available up to 2015, the last two years is forecasted using a 5-year average from both 
                        the T4-T1 and the T4-T1-T1 historical file series."),
               helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
                        of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
                        received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
                        which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
                        true when 'Outgoing' is selected."),
               helpText("4. IJEs can make income inside/outside their jurisdiction from different income sources. Selection of income source inside/outside 
                        the jurisdiction has no effect on data maps when ‘Employees’ is selected as the data series."),
               helpText("5. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may also be 
                        suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to 
                        the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
             )
             
             
           )
           
       ),
  
  
  tabPanel("Jurisdiction",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProvinceInput", "Select Jurisdiction",
                           choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"),
                           
                           selected =c("Newfoundland and Labrador")),
               
               sliderInput("YRInput", "Year Range", 
                           beginy, endy, c(beginy, endy),
                           sep=''),
               
               radioButtons("GDInput", "Select Gender",
                           choices = c("Both","Male","Female"),
                           selected =c("Both")),
               
               radioButtons("DSInput", tags$div(HTML(paste0("Data Source",tags$sup('2')))),
                            choices = c("T1 Personal Master File", "T1 Historical File"),
                            selected = c("T1 Personal Master File")),
               
               selectInput("tableIn", "Choose a table to download: ",
                           choices = c("Filtered Table","Full Table")
                           ),
               downloadButton("downloadPtable", "Download")       
             ),
             mainPanel(
               plotlyOutput("PRtrend",height=525),
               br(),
               # plotlyOutput("PRtrend2",height=450),
               # br(),
               plotlyOutput("PRInctrend",height=525),
               # br(),
               hr(),
               ## what does this do??
               
               ### added a line of code here too `.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover `###
             #   tags$style(HTML("
             #        .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
             #        color: #ffffff;
             #        }
             #        ### ADD THIS HERE ###
             #        .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
             # 
             #        ###To change text and background color of the `Select` box ###
             #        .dataTables_length select {
             #               color: #0E334A;
             #               background-color: #0E334A
             #               }
             # 
             #        ###To change text and background color of the `Search` box ###
             #        .dataTables_filter input {
             #                color: #0E334A;
             #                background-color: #0E334A
             #               }
             # 
             #        thead {
             #        color: #ffffff;
             #        }
             # 
             #         tbody {
             #        color: #000000;
             #        }
             # 
             #       "
             # 
             #   )
             # ),
             # 
             #   ## what does this do?
             # DT::dataTableOutput("PRtable"),
             
             #br(),
             helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
             helpText("2. Data from T1 Personal Master File are derived from the T1 and T4 tax files. Data from T1 Historical File are derived from
                      the T1, T4 and the T1 historical personal master file. The T1 historical file includes late and re-assessed tax filers. Because
                      the T1 historical file is only available up to 2015, the last two years is forecasted using a 5-year average from both the T4-T1 
                      and the T4-T1-T1 historical file series."),
             helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
                        of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
                        received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
                        which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
                        true when 'Outgoing' is selected."),
             helpText("4. Incoming IJE's earnings are defined as their T4 earnings received from the selected jurisdiction. Outgoing IJE's earnings are 
                      defined as their T4 earnings received from non-residential jurisdictions."),
             helpText("5. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may also be suppressed 
                      if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
             helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
             
             
             )
           )
      ),
  
  
  
  tabPanel("Industry",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProIndInput", "Select Jurisdiction",
                           choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"),
                           selected =c("Newfoundland and Labrador")),
               
               sliderInput("YRInd", "Year Range", 
                           beginy, endy, c(beginy, endy),
                           sep=''),
               
               radioButtons('IncOutIndustry',tags$div(HTML(paste0("Select Employment Type",tags$sup('3')))),
                            choices = c('Incoming','Outgoing'),
                            selected = c('Incoming')),
               
               checkboxGroupInput("IndustryInput", tags$div(HTML(paste0("Select Industry",tags$sup('3')," (Limit 12)"))),
                                  choices = indList,
                                  selected = indList[1:12]),
               
               checkboxInput("selectAllInd","Select/Deselect All"),
               
               selectInput("tableInd", "Choose a table to download: ",
                           choices = c("Filtered Industry Table","Full Industry Table")),
               
               downloadButton("downloadItable", "Download")   
             ),
             mainPanel(
               plotlyOutput("IndCount",height=600),
               br(),
               plotlyOutput("IndIncome",height=600),
               # br(),
               hr(),
               ### add your style inline css values here
               
               ### added a line of code here too `.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover `###
               # tags$style(HTML("
               #                 .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
               #                 color: #ffffff;
               #                 }
               #                 ### ADD THIS HERE ###
               #                 .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
               #                 
               #                 ###To change text and background color of the `Select` box ###
               #                 .dataTables_length select {
               #                 color: #0E334A;
               #                 background-color: #0E334A
               #                 }
               #                 
               #                 ###To change text and background color of the `Search` box ###
               #                 .dataTables_filter input {
               #                 color: #0E334A;
               #                 background-color: #0E334A
               #                 }
               #                 
               #                 thead {
               #                 color: #ffffff;
               #                 }
               #                 
               #                 tbody {
               #                 color: #000000;
               #                 }
               #                 
               #                 "
               #                 
               # )
               # ),
               # 
               # 
               # DT::dataTableOutput("Indtable"),
               
               #br(),
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. These estimates are derived from the T1 and T4 tax files and do not include late and re-assessed 
                        taxfilers from the T1 Historical personal master file."),
               helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
                        of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
                        received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
                        which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
                        true when 'Outgoing' is selected."),
               helpText("4. When an employee has T4 earnings in more than one industry, they will be counted in the industry of the job in which the 
                        employee has the highest T4 earnings."),
               helpText("5. Industry categories are derived from the North American Industry Classification System (NAICS). ‘Other services’ include: 
                        Administrative and support; Waste management and remediation services; Entertainment and recreation; Other services etc."),
               helpText("6. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may also be 
                        suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded 
                        to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           ),
  
  tabPanel("Target Jurisdiction",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProOPInput", "Select Base Jurisdiction",
                           choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"),
                           selected =c("Newfoundland and Labrador")),
               
               sliderInput("YRTP", "Year Range", 
                           beginy, endy, c(beginy, endy),
                           sep=''),
               
               radioButtons('IncOutTgtJuris',tags$div(HTML(paste0("Select Employment Type",tags$sup('3')))),
                            choices = c('Incoming','Outgoing'),
                            selected = c('Incoming')),
               
               checkboxGroupInput('ProTPInput','Select Target Jurisdiction',
                                  choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                              "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                              "Yukon", "Northwest Territories","Nunavut"),
                                  selected = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                               "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                               "Yukon", "Northwest Territories","Nunavut")),
             
               selectInput("tableTP", "Choose a table to download: ",
                           choices = c("Filtered TP Table","Full TP Table")),
               
               downloadButton("downloadTPtable", "Download") 
               
             ),
             mainPanel(
               plotlyOutput("TPcount",height=525),
               br(),
               plotlyOutput("TPincome",height=525),
               # br(),
               hr(),
               
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. These estimates are derived from the T1 and T4 tax files and do not include late and re-assessed 
                        taxfilers from the T1 Historical personal master file."),
               helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
                        of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
                        received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
                        which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
                        true when 'Outgoing' is selected."),
               helpText("4. When an outgoing IJE has T4 earnings in more than one jurisdiction, he/she will be counted in the jurisdiction of 
                        the job in which the employee has the highest T4 earnings."),
               helpText("5. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may 
                        also be suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings 
                        are rounded to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           
           ),
           
           
  tabPanel("Age Group",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProAgeInput", "Select Jurisdiction",
                           choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"),
                           selected =c("Newfoundland and Labrador")),
               
               sliderInput("YRAge", "Year Range",
                           beginy, endy, c(beginy, endy),
                           sep=''),
               
               radioButtons("TAgeInput", tags$div(HTML(paste0("Select Employment Type",tags$sup('3')))),
                            choices = c("Incoming","Outgoing", "Resident"),
                            selected =c("Incoming")),
               
               checkboxGroupInput( "AgeInput", "Select Age Groups",
                           choices = c("18 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years",
                                       "55 to 64 years", "65 years and older"),
                           selected = c("18 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years",
                                        "55 to 64 years", "65 years and older")),
               
               radioButtons("GDAgeInput", "Select Gender",
                           choices = c("Both","Male","Female"),
                           selected =c("Both")),
               
               selectInput("tableAge", "Choose a table to download: ",
                           choices = c("Filtered Age Table","Full Age Table")),
               
               downloadButton("downloadAtable", "Download")  
               
             ),
             mainPanel(
               plotlyOutput("Agetrend",height=525),
               br(),
               plotlyOutput("Agechange",height=525),
               # br(),
               hr(),
               
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. These estimates are derived from the T1 and T4 tax files and do not include late and re-assessed 
                        taxfilers from the T1 Historical personal master file."),
               helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
                        of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
                        received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
                        which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
                        true when 'Outgoing' is selected."),
               helpText("4. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may also be suppressed 
                        if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           
          )#,
  
  # tabPanel("Reference",
  #          navlistPanel(
  #            tabPanel("Inter-jurisdictional Employees",
  #                     tags$head(
  #                       tags$style("label{font-family: 'Lobster', cursive;}")), 
  #                     h3( "Inter-jurisdictional employees are paid employment of individuals who maintain a permanent residence in a given province/territory while reporting
  #                         earnings from a different province/territory"))
  #            
  #                    ),
  #          navlistPanel(
  #            tabPanel("Data Source(s)",
  #                     tags$head(
  #                       tags$style("label{font-family: 'Lobster', cursive;}")), 
  #                     h3( "The data source for this project is the Canadian Employer-Employee Dynamics Database
  #                       (CEEDD)"))
  #          ),
  #          
  #          navlistPanel(
  #            tabPanel("User Guide",
  #                     tags$head(
  #                       tags$style("label{font-family: 'Lobster', cursive;}")),
  #                     h3("Click", downloadLink('downloadGuide','Here'), "to Download user Guide"
  #                       ))
  #          ),
  #          navlistPanel(
  #            tabPanel("Datasets",
  #                     tags$head(
  #                       tags$style("label{font-family: 'Lobster', cursive;}")),
  #                     h3("Datasets are avaiable for download from", downloadLink('downloadVintage','Here')
  #                     ))
  # 
  #          )
  #          
  # )
  

  ) 
)
