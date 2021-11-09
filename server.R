library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(dplyr)
library(data.table)
library(shinyjs)
library(plotly)
library(htmltools)
library(magrittr)
library(D3partitionR)
library(jsTreeR)

server <-  function(input, output,seesion) {

###############################################.
## About ----
###############################################.
  fungimap <- read.csv("//Users/pei-yu/Dropbox/2019_Fungal_BGC/FunCore/data/fungimap.txt",sep = "\t")[,1:3]
  

  output$map <- renderLeaflet({  
    leaflet(data = fungimap) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 120.9605, lat = 23.6978, zoom = 7) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMiniMap(toggleDisplay = TRUE,position = "bottomleft")%>% 
      addMarkers(~decimalLongitude, ~decimalLatitude, 
                 clusterOptions = markerClusterOptions(),
                 popup = paste("<b><a href='http://www.samurainoodle.com'>",fungimap$species,"</a></b>"), 
                 label = ~as.character(species))
      
                   
  })
  
############################################.
## Browse ----
###############################################.
  browse <- fread("/Users/pei-yu/Dropbox/2019_Fungal_BGC/FunCore/data/ortho_predict2.txt",header = T)[,c(1,12,3,2,14)]
  colnames(browse) <- c("Subnet ID","# Species","Known EGs","Probability(GLM)","Probability(RF)")
  
  species <- fread("/Users/pei-yu/Dropbox/2019_Fungal_BGC/FunCore/data/bp_function.txt",header = F)[,c(2,1,4,3)]
  colnames(species) <- c("Subnet ID","Gene ID","GO Functions","Species")
  
  fun <- fread("/Users/pei-yu/Dropbox/2019_Fungal_BGC/FunCore/data/subnet_function.txt",header = F)
  
  taxonomy <- fread("/Users/pei-yu/Dropbox/2019_Fungal_BGC/FunCore/data/specie_names_491.txt",header = T)

  output$Subnets <- renderValueBox({
    valueBox(dim(browse)[1],"# Subnets",icon = icon("fire"),color = "red")
  })
  
  getdeps <- function() {
    htmltools::attachDependencies(
      htmltools::tagList(),
      c(
        htmlwidgets:::getDependency("datatables","DT")
      )
    )
  }

  output$page1 <- DT::renderDataTable(browse, rownames = FALSE,
                                      callback=DT::JS(
                                        'table.on("click.dt", "tr", function() {

    tabs = $(".tabbable .nav.nav-tabs li a");
    var data=table.row(this).data();
    document.getElementById("Subnet ID").value=data[0];
    Shiny.onInputChange("Subnet ID",data[0]);
    $(tabs[1]).click();
    table.row(this).deselect();
    })'                     
                                      ))
  

  output$table2 <- DT::renderDataTable(species %>% filter(`Subnet ID`==input$`Subnet ID`) %>% select(-`Subnet ID`), rownames = FALSE)
  
  output$page2 <- renderText({
    print(input$`Subnet ID`)
    paste0("Detailed for Subnet #",input$`Subnet ID`,":")
  })
  
  output$functionp = renderPlotly({
    fun %>% filter(V1==input$`Subnet ID`) %>%
    plot_ly(
       x = ~V2, y = ~V3,
      name = "Functions",type = "bar",mode = 'markers',
      marker = list(color = '#361E14',width = 1)
    )%>% 
      layout(title = "Top 5 GO Functions",
                xaxis = list(title = "Biological Process"),
                yaxis = list(title = "# Genes"))
  })
 
  
  
  
  nested_string <- apply(taxonomy[,c(10,9,7,5,4,3,2)],1,paste,collapse='/')
  output$taxonomy_tree <- renderJsTree({
    jsTree(nested_string) 
    })

  ##Agregating data to have unique sequence for the 4 variables
  var_names=rev(colnames(taxonomy)[-c(1,2,3,4,5,8,6)])
  taxonomy_plot=taxonomy[,.N,by=var_names]
  taxonomy_plot$K <- as.numeric(as.factor(taxonomy_plot$Phylum))
  #taxonomy_plot[,(var_names):=lapply(var_names,function(x){taxonomy_plot[[x]]=paste0(x,' ',taxonomy_plot[[x]])})]

  output$taxonomy_graph <- renderD3partitionR({
    D3partitionR() %>%
      add_data(taxonomy_plot,count = 'N',steps=var_names) %>%
      set_legend_parameters(visible=F)%>%
      set_labels_parameters(visible=T,cut_off=5,style='fill:white;')%>%
      #add_title('Titanic') %>%
      plot()                                            
  })
    
    

      
###############################################.
## Query ----
###############################################.  
  
  
  
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert("Oops!", "Something went wrong.", type = "error")})
  
  
  
  
  output$contents <- eventReactive(input$predict, { 
  #   renderTable({
  # 
  #   # input$file1 will be NULL initially. After the user selects
  #   # and uploads a file, head of that data file by default,
  #   # or all rows if selected, will be shown.
  # 
  #   req(input$file1)
  # 
  #   # when reading semicolon separated files,
  #   # having a comma separator causes `read.csv` to error
  #   tryCatch(
  #     {
         df <- read.csv(input$file1$datapath,
                        header = T)
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  # 
  #   if(input$disp == "head") {
  #     return(head(df))
  #   }
  #   else {
  #     return(df)
  #   }
  # 
  # })
    DT::renderDataTable(DT::datatable(
      df,
      options = list(rowCallback = DT::JS(
        'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[1]) >= 5.0)
          $("td:eq(1)", row).css("font-weight", "bold");
      }'
      ))
    ))
  })
  


  

}
