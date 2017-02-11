library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
library(ggvis)
require(datasets)
library(DT)



dashboardPage(
  dashboardHeader(title = 'MYULab'),
  dashboardSidebar(
    sidebarUserPanel("Jean Marie Cimula",
                     image = 'http://education-japan.org/africa/search/img/miyagi_logo.gif'),
    sidebarMenu(
      selectizeInput("selected", "Project Sectors",
                     choices = list('Agriculture','Economic', 'Water','Education','Energy',
                                    'Environment', 'Transport','Health','Information',
                                    'Infrastructure','Gender','Human')),
      menuItem("Historical Appraisal Repor", tabName = 'teamAttendance', icon = icon('bar-chart-o')),
      menuItem("Hierarchical Issue Tracking", tabName = 'DOW', icon = icon('calendar')),
      menuItem('Removing blindfold', tabName = 'season', icon = icon('bar-chart-o')),
      sliderInput("sldinput", "About the number of Projects", min = 1, max = 5, value = 1),
      menuItem("Data", tabName = 'data', icon = icon('database'))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName= 'teamAttendance',
              fluidRow(
                h4(box(width = 12, height = 50, status = 'info', solidHeader = TRUE, collapsible = FALSE,
                    textOutput('text1'))),
                
                valueBoxOutput('stapp', width = 2),
                valueBoxOutput('ston', width = 2),
                valueBoxOutput('stlend', width = 2),
                valueBoxOutput('stpipe', width = 2),
                valueBoxOutput('sttotal', width = 2),
                
                box(title = 'Historical Appraisal Report', width = 12, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plot1', click = 'plot_click'),
                    verbatimTextOutput('info'),
                    DT::dataTableOutput('plot_clicked_points'))
              )),
      tabItem(tabName = 'DOW',
            fluidRow(
              h4(box(width = 12, height = 50, status = 'info', solidHeader = TRUE, collapsible = FALSE,
                     textOutput('text2'))),
              
              valueBoxOutput('stapp2', width = 2),
              valueBoxOutput('ston2', width = 2),
              valueBoxOutput('stlend2', width = 2),
              valueBoxOutput('stpipe2', width = 2),
              valueBoxOutput('sttotal2', width = 2),
              
              box(title = 'Bubbles View', width = 6, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                  dataTableOutput('bbbles')
                  ),
              box(title = 'Decision Tree', width = 6, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                  plotOutput('plotMonth'))
            )),
      tabItem(tabName = 'season',
              fluidRow(
                h4(box(width = 12, height = 50, status = 'info', solidHeader = TRUE, collapsible = FALSE,
                       textOutput('text3'))),
                
                valueBoxOutput('stapp3', width = 2),
                valueBoxOutput('ston3', width = 2),
                valueBoxOutput('stlend3', width = 2),
                valueBoxOutput('stpipe3', width = 2),
                valueBoxOutput('sttotal3', width = 2),
                
                box(title = 'Project Tracker Dashboard', width = 12, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plotSeason'))
              )),
      tabItem(tabName = 'data', 
              fluidRow(
                box(htmlOutput('table2'), width = 20, height = 400)
              ))
      )
  )
)

