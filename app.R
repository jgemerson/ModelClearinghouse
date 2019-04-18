##################################################################
#                             Jojo Emerson                       #
#                        Model Clearinghouse                     #
#                   Main directory: Shiny app file               #
#                     Code changes: February 4, 2019             #
#                    GitHub Upload: April 18, 2019               #
#                             Version 1.0.5                      #
##################################################################


############              REQUIRED FILES              ############

#Main directory: 
# - app.R: R shiny app (current file)
# - cleaning.R: data cleaning file
#Data directory: 
# - 02 04 2019 Update.xlsx: model data from SQL server 
#rsconnect directory:
# - connection to shinyapps.io account for deployment (removed for GitHub)
#www directory:
# - BMGF.png: BMGF logo for footer
# - cevr.png: CEVR logo for footer

#call libraries
library(shiny)
library(shinydashboard)
library(DT)

#call cleaning datafile
source("cleaning.R")

####UI####
ui <- dashboardPage(
  
  #header
  dashboardHeader(title = "Open-Source Model Clearinghouse", titleWidth  = 350),
  
  #sidebar
  dashboardSidebar(
    #keep sidebar collapsed
    collapsed = TRUE,
    
    sidebarMenu(
      #model search tab
      menuItem("Model Search", tabName = "search", icon = icon("search")),
      #explore code tab
      menuItem("Explore code", tabName = "code", icon = icon("code")),
      
      #footer with contact and logos
      tags$div(style = "position:fixed; bottom: 0;padding:25px;",
               tags$br(),
               "Developer:",
               tags$br(),
               tags$a("Joanna Emerson", href="mailto:jemerson@tuftsmedicalcenter.org"),
               tags$br(), tags$br(),
               tags$a(tags$img(src ="cevr.png",  width = "120px", height = "60px"), href="http://healtheconomics.tuftsmedicalcenter.org/orchard", target = "_blank"),
               tags$br(), tags$br(),
               tags$a(tags$img(src = "BMGF.png",width = "140px", height = "90px"), href="https://www.gatesfoundation.org", target = "_blank"))
      
    ) #close sidebar menu
  ), #close dashboard header
  
  
  
  #body
  dashboardBody(
  tabItems(
    
    #data table tab
    tabItem(tabName = "search",
            
      #change font
      tags$head(tags$style(HTML('
        * {
        font-family: Arial, sans-serif; !important
          }
        '))),
            
                    
        fluidRow(
          
          #general info statement
          htmlOutput('generalinfo'),
        
          #filters
            #disease
            column(width = 4,
              selectizeInput('disease', label = "Disease:", choices = c("Cardiovascular", "Maternal disorders", "HIV/AIDs and Tuberculosis", "Mental and behavioral disorders", "Common infectious diseases: diarrhea, lower respiratory infections, meningitis",
                                                                          "Diabetes, urogenital, blood, and endocrine disorders", "Other communicable, maternal, neonatal, and nutritional disorders", "Nutritional deficiencies",
                                                                          "Neurological disorders", "Injury: unintentional or transport", "Neonatal disorders", "Digestive diseases", "Chronic respiratory disease", "Violence: self harm or interpersonal",
                                                                          "Musculoskeletal disorders", "Other non-communicable disease", "Neoplasms/cancer"), 
                multiple = TRUE, options = list(placeholder = 'Begin typing disease...'))
            ), #close column 1
            
            #country
            column(width = 4,
              selectizeInput('country', label = "Country:", choices = models_raw$model_country, multiple = TRUE, options = list(placeholder = 'Begin typing country...'))
            ), #close column 2
            
            #intervention
            column(width = 4,
              selectizeInput('intervention', label = "Intervention:", choices = c("Care delivery", "Diagnostic", "Health education/behavior", "Immunization","Medical device", "Medical procedure", "Pharmaceutical", "Screening", "Surgical", "Nutrition", "Maternal/neonatal ", "Environmental", "Legislation", "Other", "None"),
                              multiple = TRUE, options = list(placeholder = 'Begin typing intervention...'))
            )#close column 3
        ),  #close row 1
          
        fluidRow(
        
          #model type
          column(width = 4,
            selectizeInput('type', label = "Model type:", choices = c("Decision tree",	"Markov/Transition model",	"Discrete event",	"Agent-based",	"Microsimulation",	"Other", "Not reported"),
                           multiple = TRUE, options = list(placeholder = 'Begin typing model type...'))
          ), #close column 1
          
          #software
          column(width = 4,
            selectizeInput('software', label = "Software type:", choices = models_raw$model_software, multiple = TRUE, options = list(placeholder = 'Begin typing software...'))
          ),#close column 2
          
          #sponsor
          column(width = 4,
            selectizeInput('sponsor', label = "Model/project sponsor:", choices = c("Government", "Intergovernmental org", "Foundation", "Pharmaceutical/Medical device co", "Healthcare Org", "Academic", "Prof Membership Org", "None", "Other", "Not reported"),
                           multiple = TRUE, options = list(placeholder = 'Begin typing sponsor...'))
          )#close column 3
        
        ), #close row 2
      
        fluidRow(
        
          #author last name
          column(width = 4,
                selectizeInput('author', label = "Author last name:", choices = models_raw$author1_last, multiple = TRUE, options = list(placeholder = 'Begin typing author...'))
          ), #close column 1
        
          #year
          column(width = 4,
            sliderInput("year", "Last update year:", min = 2009, max = 2019, value = c(2009, 2019), step = 1, dragRange = TRUE, sep = "", ticks = FALSE)
          )#close column 2
        
        ), #close row 3
      
      #output table      
      fluidRow(
          DT::dataTableOutput("output_table")
      ) #close row 4
    ), #close tab 1
    
    #code tab
    tabItem(tabName = 'code',
      fluidRow(
        
        #Under construction caption
        h2("Under construction"),
        
        #embedded iframe
        htmlOutput(outputId = "iframe")
        
      ) #close row 1
    )#close tab 2
  ) #close tab items
 ) #close dashboard body
) #close UI




####SERVER####
server <- function(input, output) {

#TAB 1 - SEARCH CONTENT#

  #general info statement
  output$generalinfo<-renderText({paste('<center>',"Use the menus below to filter data. Multiple selections are permitted:", '<br>', '<br>')})

  #output table
  output_table<-reactive({

    models_subset<-models_raw
    
      #disease
      if(!is.null(input$disease)){
         models_subset<-subset(models_subset, disease_s_cardiovascular %in% input$disease | disease_s_maternal  %in% input$disease | disease_s_HIVTB  %in% input$disease
                                | disease_s_mentalbehavioral %in% input$disease | disease_s_commoninfectious %in% input$disease | disease_s_diabetes  %in% input$disease
                                | disease_s_otherNCD %in% input$disease | disease_s_nutritional %in% input$disease | disease_s_neurological %in% input$disease
                                | disease_s_injury %in% input$disease |   disease_s_neonatal  %in% input$disease | disease_s_digestive  %in% input$disease
                                | disease_s_neoplasms %in% input$disease | disease_s_respiratory  %in% input$disease | disease_s_violence  %in% input$disease
                                | disease_s_musculoskeletal %in% input$disease | disease_s_musculoskeletal %in% input$disease | disease_s_othercommunicable %in% input$disease)
      }

      #country
      if(!is.null(input$country)){
         models_subset<-subset(models_subset, model_country %in% input$country)
      }

      #intervention
      if(!is.null(input$intervention)){
        models_subset<-subset(models_subset,intervention_s_caredelivery %in% input$intervention | intervention_s_diagnostic %in% input$intervention 
                              | intervention_s_edubehavior %in% input$intervention | intervention_s_immunization %in% input$intervention 
                              | intervention_s_meddevice %in% input$intervention | intervention_s_medprocedure %in% input$intervention 
                              | intervention_s_pharma %in% input$intervention | intervention_s_screening %in% input$intervention | intervention_s_surgical %in% input$intervention
                              | intervention_s_nutrition %in% input$intervention | intervention_s_maternalneonatal %in% input$intervention 
                              | intervention_s_environmental %in% input$intervention | intervention_s_legislation %in% input$intervention | intervention_s_none %in% input$intervention 
                              | intervention_s_other %in% input$intervention| intervention_s_notreported %in% input$intervention)
      }

      #model type
      if(!is.null(input$type)){
        models_subset<-subset(models_subset, type_s_tree %in% input$type | type_s_markov %in% input$type 
                              |type_s_discreteevent %in% input$type | type_s_agentbased %in% input$type 
                              |type_s_microsimulation %in% input$type | type_s_other%in% input$type| type_s_notreported %in% input$intervention)
      }

      #software
      if(!is.null(input$software)){
        models_subset<-subset(models_subset, model_software %in% input$software)
      }

      #sponsor
      if(!is.null(input$sponsor)){
        models_subset<-subset(models_subset, sponsor_s_government %in% input$sponsor| sponsor_s_intergovernmental %in% input$sponsor | sponsor_s_foundation %in% input$sponsor
                              | sponsor_s_pharmadevice %in% input$sponsor | sponsor_s_healthcare %in% input$sponsor| sponsor_s_academic %in% input$sponsor
                              | sponsor_s_profmemborg %in% input$sponsor| sponsor_s_other %in% input$sponsor| sponsor_s_notreported %in% input$sponsor)
      }

      #year
      if(!is.null(input$year)){
        models_subset<-subset(models_subset, (year >= input$year[1])&(year <= input$year[2]))
      }
      
      #author
      if(!is.null(input$author)){
        models_subset<-subset(models_subset, author1_last %in% input$author)
      }

    #create final datatable structure
    models<-models_subset[,c("title","PrimaryAuthor", "intervention_display","disease_display", "model_country", "type_display", "model_software", "sponsor_display", "model_lastupdate", "url")]
    colnames(models)<-c("Model Title", "Primary Author",  "Intervention", "Disease", "Country", "Model type", "Software", "Model sponsor", "Date of last update", "Link to source code")

    return(models)
  })
  
  #render output table
  output$output_table = DT::renderDataTable({
    
    #create table with DT with active hyperlinks
    DT::datatable(output_table(),
                  options = list(
                    #no searching
                    searching = FALSE,
                    #show 10 per page
                    pageLength = 10
                   ),
                  #table caption
                  caption = "Open-Source Models: Click OSF link to see abstract and source code",
                  #only allow one to be selected
                  selection = 'single',
                  #don't show rownames
                  rownames = FALSE,
                  #allow links to be clickable
                  escape = FALSE)
  })
  
#TAB 2 (CODE) CONTENT  
  #iframe
  output$iframe <- renderUI({
    tags$iframe(name ="iframe", src="https://osf.io/mdk2h/", seamless = TRUE, width = "100%", height = "900")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

