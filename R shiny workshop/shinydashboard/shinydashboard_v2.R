
# Load packages -----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(haven)
library(DT)
library(plotly)
library(tidyverse)
library(rlang)

# Load data ---------------------------------------------------------------

adae <- read_xpt(url("https://github.com/cdisc-org/sdtm-adam-pilot-project/blob/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adae.xpt?raw=true"))
adsl <- read_xpt(url("https://github.com/cdisc-org/sdtm-adam-pilot-project/blob/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adsl.xpt?raw=true"))
advs <- read_xpt(url("https://github.com/cdisc-org/sdtm-adam-pilot-project/blob/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/advs.xpt?raw=true"))

# Setup UI ----------------------------------------------------------------------

ui <- dashboardPage(  skin="purple",
  
  ## Header -----------------------------------------------------------------
  
  dashboardHeader(
    title = "Shinydashboard",
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "ADaM's are updated",
                   icon = icon("users")
                 )
    )
  ),
  
  
  ## SideBar -----------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      id = "sideMenu", # id of the sidebarMenu used to link using input.sideMenu
      
      ##* Tabs -----------------------------------------------------------------
      menuItem( tags$div("Demographics and", tags$br(), "baseline characteristics", style = "display: inline-block; vertical-align: middle;"), tabName = "demographics", icon = icon("th")),
      menuItem("Adverse events", tabName = "adverseEvents", icon = icon("stethoscope"),
               menuSubItem("Adverse events plot", tabName = "adverseEventsPlot", icon = icon("bar-chart")),
               menuSubItem("Adverse events data", tabName = "adverseEventsData", icon = icon("database"))),
      menuItem("Vital signs", tabName = "vitalSigns", icon = icon("heartbeat")),
      hr(),
      
      ##* Conditional panels with options for each tab -----------------------------------------------------------------
      
      # Demographics and baseline characteristics options visible when the demographics menuItem is selected
      conditionalPanel(
        "input.sideMenu == 'demographics'", # link to the demographics menuItem
        radioButtons("popvar", "Choose population variable", choices=names(adsl[1, ] %>% select(SAFFL, ITTFL, EFFFL, COMP8FL, COMP16FL, COMP24FL)),selected = "SAFFL"),
        radioButtons("demovar", "Choose demographic variable", choices=names(adsl[1, ] %>% select(AGEGR1, SEX, RACE, ETHNIC, BMIBLGR1))),
        varSelectInput("basecharvar", "Choose baseline characteristic variable", 
                       adsl[1, ] %>% select(AGE, HEIGHTBL, WEIGHTBL, BMIBL, CUMDOSE, AVGDD, DURDIS))
      ),
      
      # Adverse events options visible when the adverseEvents menuItem is selected
      conditionalPanel(
        "input.sideMenu == 'adverseEventsPlot' | input.sideMenu == 'adverseEventsData'", # link to the adverseEventsPlot and adverseEventsData menuSubItems
        selectInput("trtemfl", "Treatment emergent event",  unique(adae$TRTEMFL), multiple = T, selected = sort(unique(adae$TRTEMFL))),
        selectInput("aeser", "Serious event",  unique(adae$AESER), multiple = T, selected = sort(unique(adae$AESER))),
        checkboxGroupInput("aerel", "Causality",  c("NONE", "POSSIBLE", "PROBABLE", "REMOTE"), selected = c("NONE", "POSSIBLE", "PROBABLE", "REMOTE"))
      ),
      
      # Vital signs options visible when the vitalSigns menuItem is selected
      conditionalPanel(
        "input.sideMenu == 'vitalSigns'", # link to the vitalSigns menuItem
        selectInput("param", "Select param", advs %>% filter(PARAM != "Height (cm)") %>% select(PARAM) %>% unique())
      )
    )
  ),
  
  
  ## Body ------------------------------------------------------------------
  
  dashboardBody(
    tabItems(
      
      ##* Demographic ------------------------------------------------------------------
      
      # Body when the demographics menuItem is selected
      tabItem(
        tabName = "demographics", # link to the demographics menuItem
        fluidRow(
          tabBox(
            title="Demographics and Baseline characteristics Plots",id="tabset1",width=10,height=500,
            tabPanel("Demographics bar plot",plotOutput("plot_demog", height = 400)),
            tabPanel("Baseline characteristics box plot",plotOutput("plot_base", height = 400))
          )
        ),
        fluidRow(
          box(
            title = "Demographics data", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            DTOutput("dt_demog", height = 250) # UI linked to output$dt_demog generated in the server
          )
        )
      ),
      
      ##* Adverse Events Plot ------------------------------------------------------------------
      
      # Body when the adverseEventsPlot menuSubItem is selected
      tabItem(
        tabName = "adverseEventsPlot", # link to the adverseEventsPlot menuSubItem
        fluidRow(
          box(
            title = "Adverse event plot", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12, height = "100%",
            plotlyOutput("plot_ae", height = "100%") # UI linked to output$plot_ae generated in the server
          )
        ),
        fluidRow(
          box(
            title = "Table output for Adverse event plot", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            DTOutput("dt_tae", height = 250) # UI linked to output$dt_demog generated in the server
          )
        )
      ),
      
      ##* Adverse Events Data ------------------------------------------------------------------
      
      # Body when the adverseEventsData menuSubItem is selected
      tabItem(
        tabName = "adverseEventsData", # link to the adverseEventsData menuSubItem
        fluidRow(
          box(
            title = "Adverse event data", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            DTOutput("dt_ae", height = 250) # UI linked to output$dt_ae generated in the server
          )
        )
      ),
      
      ##* Vital Signs ------------------------------------------------------------------
      
      # Body when the vitalSigns menuItem is selected
      tabItem(
        tabName = "vitalSigns", # link to the vitalSigns menuItem
        fluidRow(
          box(
            title = "Vital signs plot", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            plotlyOutput("plot_vs", height = 250) # UI linked to output$plot_vs generated in the server
          )
        ),
        fluidRow(
          box(
            title = "Vital signs data", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            DTOutput("dt_vs", height = 250) # UI linked to output$dt_vs generated in the server
          )
        )
      )
    )
  )
)



# Setup server ------------------------------------------------------------


server <- function(input, output) {
  
  # Demographic -------------------------------------------------
  
  # Reactive function - Subset ADSL
  subadsl <- reactive({
    adsl %>%
      filter(!!sym(input$popvar) == "Y")
  })
  
  ## Bar plot ---------------------------------------------------------------
  
  # linked to the UI plot_demog
  output$plot_demog <- renderPlot({
    subadsl() %>% 
      ggplot(aes(x = !!sym(input$demovar), fill = TRT01A)) + geom_bar()
  })
  
  ## Box plot ---------------------------------------------------------------
  
  # linked to the UI plot_base
  output$plot_base <- renderPlot({
    subadsl() %>% 
      ggplot(aes(x = !!input$demovar, y = !!input$basecharvar, fill = TRT01A)) + geom_boxplot()
  })
  
  ## Table -----------------------------------------------------------------
  
  # linked to the UI dt_demog
  output$dt_demog <- renderDT({
    datatable(subadsl() %>% 
                select(USUBJID, TRT01A, !!input$demovar, !!input$basecharvar, SAFFL, ITTFL, EFFFL, COMP8FL, COMP16FL, COMP24FL),
              options = list(scrollX = T))
  })
  
  # Adverse Events -------------------------------------------------
  
  # Reactive function - Subset ADAE
  subadae <- reactive({
    adae %>%
      mutate(AEREL = ifelse(is.na(AEREL), "NONE", AEREL)) %>% 
      filter(SAFFL == "Y" & AEREL != "" & 
               TRTEMFL %in% c(input$trtemfl) & 
               AESER %in% c(input$aeser) & 
               AEREL %in% c(input$aerel))
  })
  
  ## Bar plot ---------------------------------------------------------------
  
  # linked to the UI plot_ae
  output$plot_ae <- renderPlotly({
    aep1 <- subadae() %>% 
      ggplot(aes(y = AESOC)) + geom_bar()
    
    ggplotly(aep1)
  })
  
  # linked to the UI dt_tae
  output$dt_tae <- renderDT({
    datatable(subadae() %>% 
                count(AESOC,TRTEMFL,AESER,AEREL) %>%
                rename("COUNT"="n"),
              options = list(scrollX = T))
  })
  
  ## Table -----------------------------------------------------------------
  
  # linked to the UI dt_ae
  output$dt_ae <- renderDT({
    datatable(subadae(), options = list(scrollX = T))
  })
  
  # Vital Signs -------------------------------------------------
  
  # Reactive function - Subset and create descriptive summary of ADVS
  dsumvs <- reactive({
    advs %>%
      filter(SAFFL == "Y" & ANL01FL == "Y" & PARAM == input$param) %>% 
      mutate(VISIT = str_to_title(VISIT)) %>% 
      group_by(PARAMCD, PARAM, VISITNUM, VISIT, TRTAN, TRTA) %>% 
      summarise(N = n(),
                MEAN = mean(AVAL, na.rm = T), 
                MEDIAN = median(AVAL, na.rm = T),
                SD = sd(AVAL, na.rm = T),
                MIN = min(AVAL, na.rm = T),
                MAX = max(AVAL, na.rm = T)
      )
  })
  
  ## Mean plot ---------------------------------------------------------------
  
  # linked to the UI plot_vs
  output$plot_vs <- renderPlotly({
    vsp1 <- dsumvs() %>% 
      ggplot(aes(x = VISIT, y = MEAN, group = TRTA, color = TRTA)) + 
      geom_line() +
      geom_point() +
      geom_crossbar(aes(ymin = MEAN - SD, ymax = MEAN + SD), width = .2, position = position_dodge(0.05))
    
    ggplotly(vsp1)
  })
  
  ## Table -----------------------------------------------------------------
  
  # linked to the UI dt_vs
  output$dt_vs <- renderDT({
    datatable(dsumvs(), options = list(scrollX = T))
  })
  
}


# Run app -----------------------------------------------------------------

shinyApp(ui, server)
