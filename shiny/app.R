library(shiny)
library(tidyverse)
library(BerginskiRMisc)

scan_data = read.csv('data/LINCS_KINOMEscan_percent.csv')

scan_data_derived = scan_data %>%
  mutate(name_concen = paste0(Small.Molecule.Name,' ',Assay.compound.conc, Conc.unit),
         concen_units = paste0(Assay.compound.conc, Conc.unit))

dark_kinase_list = read.csv('data/Dark Kinase List.csv')

dark_kinase_overlap = intersect(dark_kinase_list$Kinase,unique(scan_data$Protein.Name))

ui <- fluidPage(
  titlePanel('KINOMEscan Data Visualization',"KINOMEscan"),
  tabsetPanel(
    tabPanel("Protein",fluidRow(
      column(width=2,
             selectInput("protein_name","Protein Name",sort(unique(scan_data$Protein.Name)),'YES'),
             sliderInput("percent_control_protein",
                         "Percent Control Range",
                         0,100,value=c(0,100)),
             checkboxGroupInput("compound_concen_protein",
                                "Compound Concentration",
                                unique(scan_data_derived$concen_units),
                                unique(scan_data_derived$concen_units)),
             h4("Filtering Options"),
             checkboxInput("only_dark_kinases_protein",
                                "Only Show Dark Kinases")),
      column(width=10,plotOutput('protein_plot',height = 800))
    ),
    hr(),
    fluidRow(column(width=12,  
                    dataTableOutput('protein_table'),
                    downloadButton('protein_download')
    ))),
    tabPanel("Compound",fluidRow(
      column(width=2,
             selectInput("compound_name","Compound Name",sort(unique(scan_data$Small.Molecule.Name))),
             sliderInput("percent_control_compound",
                         "Percent Control Range",
                         0,100,value=c(0,100)),
             checkboxGroupInput("compound_concen_compound",
                                "Compound Concentration",
                                unique(scan_data_derived$concen_units),
                                unique(scan_data_derived$concen_units)),
             h4("Filtering Options"),
             checkboxInput("only_dark_kinases_compound",
                           "Only Show Dark Kinases")),
      column(width=10,plotOutput('compound_plot',height = 800))
    ),
    hr(),
    fluidRow(column(width=12,  
                    dataTableOutput('compound_table'),
                    downloadButton('compound_download')
    ))
    
    )    
  ) 
)

server <- function(input,output,session) {
  
  #############################################################################
  #Protein Functions
  #############################################################################
  selected_protein <- reactive({
    req(input$protein_name)
    protein_data <- scan_data %>% 
      mutate(concen_units = paste0(Assay.compound.conc, Conc.unit)) %>%
      filter(Protein.Name == input$protein_name, 
             Percent.Control >= input$percent_control_protein[1],
             Percent.Control <= input$percent_control_protein[2],
             concen_units %in% input$compound_concen_protein)
  })
  
  observe({
    # Can also set the label and select items
    if (input$only_dark_kinases_protein) {
      updateSelectInput(session, "protein_name",
                        label = "Protein Name",
                        choices = dark_kinase_overlap,
                        selected = dark_kinase_overlap[1]
      )
    } else {
      updateSelectInput(session, "protein_name",
                        label = "Protein Name",
                        choices = sort(unique(scan_data$Protein.Name)),
                        selected = "YES"
      )
      
    }
  })
  
  selected_protein_name <- reactive({
    req(input$protein_name)
  })
  
  output$protein_plot <- renderPlot({
    this_protein = selected_protein() %>%
      mutate(name_concen = paste0(Small.Molecule.Name,' ',Assay.compound.conc, Conc.unit)) %>%
      arrange(desc(Percent.Control))
    
    ggplot(this_protein,aes(y=Percent.Control,x=reorder(name_concen,Percent.Control))) + 
      geom_bar(stat='identity') + 
      labs(x="Small Molecule Name",y="Percent Control") + 
      theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ,size=10)) +
      theme_berginski()
  })

  output$protein_table <- renderDataTable(selected_protein(),
                                          options=list(pageLength=10))
  
  output$protein_download <- downloadHandler(
    filename = function () {
      paste0(selected_protein_name(),'_percent.csv')
    },
    content = function(file) {
      write.csv(selected_protein(),file,row.names = F)
    })
  
  #############################################################################
  #Compound Functions
  #############################################################################
  selected_compound <- reactive({
    req(input$compound_name)
    compound_data <- scan_data %>% 
      filter(Small.Molecule.Name == input$compound_name) %>%
      mutate(concen_units = paste0(Assay.compound.conc, Conc.unit)) %>%
      filter(Small.Molecule.Name == input$compound_name,
             Percent.Control >= input$percent_control_compound[1],
             Percent.Control <= input$percent_control_compound[2],
             concen_units %in% input$compound_concen_compound)
    
    if (input$only_dark_kinases_compound) {
      compound_data <- compound_data %>%
        filter(Protein.Name %in% dark_kinase_overlap)
    }
    
    compound_data
  })
  
  selected_compound_name <- reactive({
    req(input$compound_name)
    input$compound_name
  })
  
  output$compound_plot <- renderPlot({
    this_compound = selected_compound() %>%
      mutate(name_concen = paste0(Protein.Name,' ',Assay.compound.conc, Conc.unit))
    
    ggplot(this_compound,aes(y=Percent.Control,x=reorder(name_concen,Percent.Control))) + 
      geom_bar(stat='identity') + 
      labs(x="Protein Name",y="Percent Control") + 
      theme(text = element_text(size=20),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ,size=10)) +
      theme_berginski()
  })
  
  output$compound_table <- renderDataTable(selected_compound(),
                                          options=list(pageLength=10))
  
  output$compound_download <- downloadHandler(
    filename = function() {
      paste0(selected_compound_name(),'.csv')
    },
    content = function(file) {
      write.csv(selected_compound(),file,row.names = F)
    })

}

shinyApp(ui = ui, server = server)