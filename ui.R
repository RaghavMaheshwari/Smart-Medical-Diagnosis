library(shiny)
library(shinyjs)
library(shinystan)
library(shinythemes)

shinyUI
(
  fluidPage
  ( theme = shinytheme("journal"),
    headerPanel("Smart EHR - Diabetes"),
    sidebarLayout
    (
      sidebarPanel
      (
        wellPanel
        (
          helpText(HTML("<b>Welcome</b>")),
          HTML(""),
            submitButton("Submit")
        ),
        
        wellPanel
        (
          helpText(HTML("<b>Preg</b>")),
          selectInput("preg","Select times.",
                      choices="")
          #helpText("Examples: BATTERY, THEFT etc."),
         
        ),
        
        wellPanel
        (
          helpText(HTML("<b>Pg</b>")),
          selectInput("pg","Select times.",
                      choices="")
         # helpText("Examples: BATTERY, THEFT etc."), 
        ),
        
        wellPanel
        (
          helpText(HTML("<b>DBP</b>")),
          selectInput("dbp","Select times.",
                      choices="")
          #helpText("Examples: BATTERY, THEFT etc."), 
        ),
        
        wellPanel
        (
          helpText(HTML("<b>Triceps_Skin</b>")),
          selectInput("ts","Select times.",
                      choices="")
         # helpText("Examples: BATTERY, THEFT etc."), 
        ),
        
        wellPanel
        (
          helpText(HTML("<b>SI</b>")),
          selectInput("si","Select times.",
                      choices="")
         # helpText("Examples: BATTERY, THEFT etc."), 
        ),
        
        wellPanel
        (
          helpText(HTML("<b>BMI</b>")),
          selectInput("bmi","Select times.",
                      choices="")
          #helpText("Examples: BATTERY, THEFT etc."), 
        ),
        
        wellPanel
        (
          helpText(HTML("<b>DPF</b>")),
          selectInput("dpf","Select times.",
                      choices="")
         # helpText("Examples: BATTERY, THEFT etc."), 
        ),
        
        wellPanel
        (
          helpText(HTML("<b>AGE</b>")),
          selectInput("age","Select times.",
                      choices="")
         # helpText("Examples: BATTERY, THEFT etc."), 
        )
        
      ),
      
      mainPanel
      (
        tabsetPanel(
          type = "tab",
         # tabPanel("Introduction",includeMarkdown("docs/introduction.md")),
          tabPanel("diab_pg",plotOutput("diab__pg")),
         tabPanel("diab_si",plotOutput("diab__si")),
         tabPanel("bp_bmi",plotOutput("bp__bmi")),
         tabPanel("pregnancy",plotOutput("pregnancy__")),
         tabPanel("relation",plotOutput("relation")),
         tabPanel("neural",plotOutput("neural"))
         
         #tabPanel("diab_si",dataTableOutput("diab_si"))
          #tabPanel("Map",plotOutput("map",height = 600,width = 600)),
          #tabPanel("Temp",tableOutput('temp')),
          #tabPanel("Basic Stats",showOutput("analysis","highcharts")),
          #tabPanel("Plots", plotOutput("plots")),
          #tabPanel("HeatMaps", plotOutput("heatMaps"))
          
          
        )
      )
    ) 
    
    
  )
)

