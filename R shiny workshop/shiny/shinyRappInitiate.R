
# Load packages -----------------------------------------------------------

library(shiny)
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

ui <- fluidPage(
  
  # Application title
  titlePanel("Dashboard based on Shiny"),
  
  # Defining overall layout scheme
  sidebarLayout(
    
    #* SideBar Panel --------------------------------------------------------------
    
    # Adding sidebar with options
    sidebarPanel(width = 3,
                 
                 # For demographic tab
                 
                 conditionalPanel(condition = "input.tabselected == 'Demographics'",
                                  
                                  selectInput(inputId  = "popvar",
                                              label    = "Choose population variable",
                                              choices  = c("SAFFL", "ITTFL", "EFFFL", "COMP8FL", "COMP16FL", "COMP24FL")
                                  ),
                                  
                                  selectInput(inputId  = "demovar",
                                              label    = "",
                                              choices  = c("AGEGR1", "SEX", "RACE", "ETHNIC", "BMIBLGR1")
                                  ),
                                  
                                  selectInput(inputId  = "basecharvar",
                                              label    = "Choose baseline characteristic variable",
                                              choices  = c("AGE", "HEIGHTBL", "WEIGHTBL", "BMIBL", "CUMDOSE", "AVGDD", "DURDIS")
                                  )
                 ),
                 
                 
                 # For Adverse Event tab
                 
                 conditionalPanel(condition = "input.tabselected == 'Adverse Events'",
                                  selectInput(inputId  = "trtemfl",
                                              label    = "Treatment emergent event",
                                              choices  = unique(adae$TRTEMFL),
                                              multiple = TRUE,
                                              selected = sort(unique(adae$TRTEMFL))
                                  ),
                                  
                                  
                                  
                                  
                                  checkboxGroupInput(inputId  = "aerel",
                                                     label    = "Causality",
                                                     choices  = c("NONE", "POSSIBLE", "PROBABLE", "REMOTE"),
                                                     selected = c("NONE", "POSSIBLE", "PROBABLE", "REMOTE")
                                  )
                 ),
                 
                 # For Vital Signs tab
                 
                 selectInput(inputId  = "param",
                             label    = "Select param",
                             choices  = advs %>% filter(PARAM != "Height (cm)") %>% select(PARAM) %>% unique()
                             
                 )
                 
    ),
    
    # * Main Panel ------------------------------------------------------------------
    
    # Main panel with a tab structure
    mainPanel(width = 9,
              tabsetPanel(
                
                tabPanel("Demographics",
                         fluidRow(
                           column(5, plotOutput(outputId = "plot_demog", height = 250)),
                           
                         ),
                         br(),
                         fluidRow(
                           column(10, DTOutput("dt_demog", height = 250))
                         )
                ), 
                
                tabPanel("Adverse Events",
                         fluidRow(
                           column(10, plotlyOutput("plot_ae", height = 500))
                         ),
                         hr(),  # Separate with line
                         
                ),
                
                tabPanel("Vital Signs",
                         fluidRow(
                           plotlyOutput("plot_vs", height = 250)
                         ),
                         br(),
                         fluidRow(
                           DTOutput("dt_vs", height = 250)
                         )
                )
                , id = "tabselected"         # For use in conditionalPanel calls
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
      filter( !!as.name(input$popvar) == "Y")
  })  
  
  #* Bar plot ---------------------------------------------------------------
  
  # linked to the UI plot_demog
  output$plot_demog <- renderPlot({
    subadsl() %>% 
      ggplot(aes(x = !!as.name(input$demovar),  fill = TRT01A)) + geom_bar()
  })
  
  #* Box plot ---------------------------------------------------------------
  
  # linked to the UI plot_base
  output$plot_base <- renderPlot({
    subadsl() %>% 
      ggplot(aes(x = !!as.name(input$demovar), y = !!as.name(input$basecharvar), fill = TRT01A)) + geom_boxplot()
  })
  
  #* Table -----------------------------------------------------------------
  
  # linked to the UI dt_demog
  output$dt_demog <- renderDT({
    datatable(subadsl() %>% 
                select(USUBJID, TRT01A, SAFFL, ITTFL, EFFFL, COMP8FL, COMP16FL, COMP24FL))
  })
  
  
  # Adverse Events -------------------------------------------------
  
  # Reactive function - Subset ADAE
  subadae <- reactive({
    adae %>%
      mutate(AEREL = ifelse(is.na(AEREL), "NONE", AEREL)) %>% 
      filter(
        SAFFL == "Y" &
          AEREL != "" & 
          TRTEMFL %in% c(input$trtemfl) & 
          AEREL   %in% c(input$aerel  )
      )
  })
  
  #* Bar plot ---------------------------------------------------------------
  
  # linked to the UI plot_ae
  output$plot_ae <- renderPlotly({
    aep1 <- subadae() %>% 
      ggplot(aes(y = AESOC)) + geom_bar()
    
    ggplotly(aep1)
  })
  
  #* Table -----------------------------------------------------------------
  
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
  
  #* Mean plot ---------------------------------------------------------------
  
  # linked to the UI plot_vs
  output$plot_vs <- renderPlotly({
    vsp1 <- dsumvs() %>% 
      ggplot(aes(x = VISIT, y = MEAN, group = TRTA, color = TRTA)) + 
      geom_line() +
      geom_point() +
      geom_crossbar(aes(ymin = MEAN - SD, ymax = MEAN + SD), width = .2, position = position_dodge(0.05))
    
    ggplotly(vsp1)
  })
  
  #* Table -----------------------------------------------------------------
  
  # linked to the UI dt_vs
  dt_vs <- renderDT({
    datatable(dsumvs(), options = list(scrollX = T))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
