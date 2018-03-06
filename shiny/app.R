library(shiny)
library(tidyverse)
library(BerginskiRMisc)

scan_data = read.csv('data/LINCS_KINOMEscan_percent.csv')

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Protein",fluidRow(
      column(width=2,selectInput("protein_name","Protein Name",sort(unique(scan_data$Protein.Name)),'YES')),
      column(width=10,plotOutput('protein_plot',height = 800))
    ),
    hr(),
    fluidRow(column(width=12,  
                    dataTableOutput('protein_table'),
                    downloadButton('protein_download')
    ))),
    tabPanel("Compound",fluidRow(
      column(width=2,selectInput("compound_name","Compound Name",unique(scan_data$Small.Molecule.Name))),
      column(width=10,plotOutput('compound_plot',height = 800))
    )
    
    )    
  ) 
)

server <- function(input, output) {
  
  #############################################################################
  #Protein Functions
  #############################################################################
  selected_protein <- reactive({
    req(input$protein_name)
    protein_data <- scan_data %>% filter(Protein.Name == input$protein_name)
  })
  
  selected_protein_name <- reactive({
    req(input$protein_name)
    input$protein_name
  })
  
  output$protein_plot <- renderPlot({
    this_protein = selected_protein() %>%
      mutate(name_concen = paste0(Small.Molecule.Name,' ',Assay.compound.conc, Conc.unit)) %>%
      arrange(desc(X..Control))
    
    ggplot(this_protein,aes(y=X..Control,x=reorder(name_concen,X..Control))) + 
      geom_bar(stat='identity') + 
      labs(x="Small Molecule Name",y="Percent Control") + 
      theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ,size=10)) +
      theme_berginski()
  })

  output$protein_table <- renderDataTable(selected_protein(),
                                          options=list(pageLength=10))
  
  output$protein_download <- downloadHandler(
    filename = paste0(selected_protein_name(),'_Kd.csv'),
    content = function(file) {
      write.csv(selected_protein(),file,row.names = F)
    })
  
    
  #Compound Functions
  selected_compound <- reactive({
    req(input$compound_name)
    compound_data <- scan_data %>% filter(Small.Molecule.Name == input$compound_name)
  })
  
  selected_compound <- reactive({
    req(input$compound_name)
    compound_data <- scan_data %>% filter(Small.Molecule.Name == input$compound_name)
  })
  
  output$compound_plot <- renderPlot({
    this_compound = selected_compound() %>%
      mutate(name_concen = paste0(Small.Molecule.Name,' ',Assay.compound.conc, Conc.unit)) %>%
      arrange(desc(X..Control))
    
    ggplot(this_protein,aes(y=X..Control,x=reorder(name_concen,X..Control))) + 
      geom_bar(stat='identity') + 
      labs(x="Small Molecule Name",y="Percent Control") + 
      theme(text = element_text(size=20),
            axis.text.x = element_text(angle = 90, hjust = 1,size=20)) +
      theme_berginski()
  })
  
  
}

shinyApp(ui = ui, server = server)
