
library(readxl)

setwd("C:/Users/Felipe Torres/Desktop/")

folder_path = "C:/Users/Felipe Torres/Desktop/"
  #df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
  df <- read_excel("data.xlsx")
  
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
                           Placa = "",
                           Data = "",
                           Termociclador = "",
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
  