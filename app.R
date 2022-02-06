#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid PCR Mapper v1"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Copy the line below to make a select box 
            selectInput("select", label = h3("Selecione o Kit PCR"), 
                        choices = list("Allplex (SEEGENE)" = 1, "IBMP (2021)" = 2), 
                        selected = 1),
            
            textInput("placa", "ID da placa de extração", value = "", width = NULL, placeholder = NULL),
            
            textInput("termociclador", "Termociclador", value = "", width = NULL, placeholder = NULL),
            
            textInput("lotepcr", "Lote do kit do RT-qPCR", value = "", width = NULL, placeholder = NULL),
            
            textInput("data", "Data do RT-qPCR", value = "", width = NULL, placeholder = NULL),
            
            fileInput('target_upload', 'Escolha o arquivo para upload',
                      accept = c(
                          #'text/csv', '.xls'
                          #'text/comma-separated-values',
                          #'.csv'
                          '.xls'
                      )),
            DT::dataTableOutput("sample_table"),
            
            downloadButton('download',"Baixe os dados processados"),
            
            hr(),
            fluidRow(column(1, verbatimTextOutput("value")))
            
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DT::DTOutput(outputId = "tablePlot"),
            hr(),
            print("Esse software esta disponivel no GitHub CovidMap (https://github.com/fgtorres/) | 2021")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    df_cases_upload <- reactive({
        inFile <- input$target_upload
        
        
        if (is.null(inFile))
            return(NULL)
        #df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
        df <- read_excel(inFile$datapath)
        
        
        #Processing of data PCR kit
        folder_path <- getwd()
        if (input$select == "1"){
            
            #Filtering the header Excel File
            linhas <- c(1,2,3,4,5,6)
            df<- df[-linhas,]
            names(df) <- df[1,]
            df<- df[-1,]
            
            #Spliting in sub datasets
            i<-0
            gene<-1
            
            #DIAGNOSTICO DO KIT SEEGENE
            kitpcr = "Allplex"
            diagnostico = data.frame(ID = "",
                                     diagnostico = "",
                                     Placa = input$placa,
                                     Data = input$data,
                                     Termociclador = input$termociclador,
                                     KitPCR = "",
                                     LotePCR = "",
                                     Galeria = "",
                                     Ct_gene1 = "",
                                     Ct_gene2 = "",
                                     Ct_gene3 = "",
                                     Ct_gene4 = "", 
                                     RetestePCR = "0" )
            
            
            diagnostico<- diagnostico[-1,]
            
            while(i< nrow(df)){
                i<-i+1
                amostra<- df[i,2]
                reteste<-0
                well<- df[i,1]
                diagcovid = ""
                
                if(gene==1){
                    gene1<- df[i,7]
                    gene <- gene + 1
                }else if (gene==2){
                    gene2<- df[i,7]
                    gene <- gene + 1
                }else if (gene==3){
                    gene3<- df[i,7]
                    gene <- gene + 1
                } else if(gene==4){
                    gene4<- df[i,7]
                    gene<-1
                    
                    # DIAGNOSTICANDO
                    # Teste genes negativos
                    testegene1 = 1
                    testegene2 = 1
                    testegene3 = 1
                    testegene4 = 1
                    
                    if (gene1  == "Undetermined" || gene1 > 40){
                        testegene1 = 0
                    }
                    
                    if (gene2  == "Undetermined" || gene2 > 40){
                        testegene2 = 0
                    }
                    
                    if (gene3  == "Undetermined" || gene3 > 40){
                        testegene3 = 0
                    }
                    
                    if (gene4  == "Undetermined" || gene4 > 40){
                        testegene4 = 0
                    }
                    
                    #Diagnosticando....
                    # Invalidos...
                    if (testegene1==0 && testegene2==0 && testegene3==0 && testegene4==0){
                        diagcovid = "Invalido"
                        reteste = 1
                    }
                    
                    #Nao detectados
                    if (testegene1==1 && testegene2==0 && testegene3==0 && testegene4==0){
                        diagcovid = "Nao detectado"
                    }
                    
                    #Detectados
                    if ((testegene2==1 && testegene3==1 && testegene4==1) || (testegene2==1 && testegene3==0 && testegene4==1) ||
                        (testegene2==1 && testegene3==1 && testegene4==0) || (testegene2==0 && testegene3==1 && testegene4==1)){
                        diagcovid = "Detectado"
                    }
                    
                    #Inconclusivo
                    if ((testegene2==1 && testegene3==0 && testegene4==0) || (testegene2==0 && testegene3==1 && testegene4==0)
                        || (testegene2==0 && testegene3==0 && testegene4==1) ){
                        diagcovid = "Inconclusivo"
                        reteste = 1
                    }
                    
                    if (diagcovid==""){
                        diagcovid = "Inconclusivo"
                        reteste = 1
                    }
                    
                    amostra_data <- data.frame(ID = as.character(amostra),
                                               diagnostico = diagcovid,
                                               Placa = as.character(input$placa),
                                               Data = as.character(input$data),
                                               Termociclador = as.character(input$termociclador),
                                               KitPCR = kitpcr,
                                               LotePCR = as.character(input$lotepcr),
                                               Galeria = as.character(well),
                                               Ct_gene1 = as.character(gene1),
                                               Ct_gene2 = as.character(gene2),
                                               Ct_gene3 = as.character(gene3),
                                               Ct_gene4 = as.character(gene4),
                                               RetestePCR = as.character(reteste))
                    
                    diagnostico =  rbind(diagnostico, amostra_data)
                }
            }
            
            write.csv(diagnostico,
                      paste(as.character(folder_path),
                            "/",
                            "diagnosticos_",
                            format(Sys.time(),
                                   "%a %b %d %X %Y"),
                            ".csv",
                            sep = ""),
                      sep = ",",
                      quote = F)  
            
        ######################################################################################################################
        # KIT IBMP
            
        }else if (input$select == "2"){
            
            
            #Filtering the header Excel File
            linhas <- c(1,2,3,4,5,6)
            df<- df[-linhas,]
            names(df) <- df[1,]
            df<- df[-1,]
            
            #Spliting in sub datasets
            i<-0
            gene<-1
            
            #DIAGNOSTICO DO KIT SEEGENE
            kitpcr = "IBMP"
            diagnostico = data.frame(ID = "",
                                     diagnostico = "",
                                     Placa = input$placa,
                                     Data = input$data,
                                     Termociclador = input$termociclador,
                                     KitPCR = "",
                                     LotePCR = "",
                                     Galeria = "",
                                     Ct_gene1 = "",
                                     Ct_gene2 = "",
                                     Ct_gene3 = "",
                                     Ct_gene4 = "", 
                                     RetestePCR = "0" )
            
            
            diagnostico<- diagnostico[-1,]
            
            while(i< nrow(df)){
                i<-i+1
                amostra<- df[i,2]
                reteste<-0
                well<- df[i,1]
                diagcovid = ""
                
                if(gene==1){
                    gene1<- df[i,7]
                    gene <- gene + 1
                }else if (gene==2){
                    gene2<- df[i,7]
                    gene <- gene + 1
                }else if (gene==3){
                    gene3<- ""
                    gene4<- df[i,7]
                    gene <- 1
                    
                    # DIAGNOSTICANDO
                    # Teste genes negativos
                    testegene1 = 1
                    testegene2 = 1
                    testegene3 = 1
                    testegene4 = 1
                    
                    if (gene1  == "Undetermined" || gene1 > 40){
                        testegene1 = 0
                    }
                    
                    if (gene2  == "Undetermined" || gene2 > 40){
                        testegene2 = 0
                    }
                    
                    if (gene3  == "Undetermined" || gene3 > 40){
                        testegene3 = 0
                    }
                    
                    if (gene4  == "Undetermined" || gene4 > 40){
                        testegene4 = 0
                    }
                    
                    #Diagnosticando....
                    # Invalidos...
                    if (testegene1==0 && testegene2==0 && testegene3==0 && testegene4==0){
                        diagcovid = "Invalido"
                        reteste = 1
                    }
                    
                    #Nao detectavel
                    if (testegene1==0 && testegene2==0 && testegene4==1){
                        diagcovid = "Nao detectado"
                    }
                    
                    #Detectavel
                    
                    if (testegene1==1 && testegene2==1 && testegene4==1){
                        diagcovid = "Detectado"
                    }else if (testegene1==1 && testegene2==1 && testegene4==0){
                        diagcovid = "Detectado"
                    }else if (testegene1==1 && testegene2==1 && testegene4==0){
                        diagcovid = "Detectado"
                    }else if (testegene1==1 && testegene2==1 && testegene4==0){
                        diagcovid = "Detectado"
                    }else if (testegene1==1 && testegene2==0 && testegene4==1){
                        diagcovid = "Detectado"
                    }else if (testegene1==1 && testegene2==0 && testegene4==0){
                        diagcovid = "Detectado"
                    }else if (testegene1==0 && testegene2==1 && testegene4==1){
                        diagcovid = "Detectado"
                    }else if (testegene1==0 && testegene2==1 && testegene4==0){
                        diagcovid = "Detectado"
                    }
                    
                    if (diagcovid==""){
                        diagcovid = "Inconclusivo"
                        reteste = 1
                    }
                    
                    amostra_data <- data.frame(ID = as.character(amostra),
                                               diagnostico = diagcovid,
                                               Placa = as.character(""),
                                               Data = as.character(""),
                                               Termociclador = as.character(""),
                                               KitPCR = kitpcr,
                                               LotePCR = as.character(""),
                                               Galeria = as.character(well),
                                               Ct_gene1 = as.character(gene1),
                                               Ct_gene2 = as.character(gene2),
                                               Ct_gene3 = as.character(gene3),
                                               Ct_gene4 = as.character(gene4),
                                               RetestePCR = as.character(reteste))
                    
                    diagnostico =  rbind(diagnostico, amostra_data)
                }
            }
            
            write.csv(diagnostico,
                      paste(as.character(folder_path),
                            "/",
                            "diagnosticos_",
                            format(Sys.time(),
                                   "%a %b %d %X %Y"),
                            ".csv",
                            sep = ""),
                      sep = ",",
                      quote = F)  
            
        }
        
        return(diagnostico)
    })
    
    output$tablePlot<- DT::renderDataTable({
        diagnostico <- df_cases_upload()
        DT::datatable(diagnostico)
    })
    
    output$download <- downloadHandler(
        filename = function(){"diagnostico.csv"}, 
        content = function(fname){
            write.csv(df_cases_upload(), fname)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)