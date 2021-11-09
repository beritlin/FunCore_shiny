library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(dplyr)
library(shinyjs)
library(shinycssloaders)
library(leaflet)
library(jsTree)


#library(shinydashboardPlus)


# Use a fluid Bootstrap layout
ui <- fluidPage(    
  theme = shinytheme("sandstone"),
  
  # Give the page a title
  # titlePanel("Telephones by region"),
  tags$head(
    tags$style(HTML(
          ".navbar .navbar-nav {float: right}"
        ))
  ),
  navbarPage("FunCore",
             
###############################################.
## Home ----
###############################################.

    tabPanel("Home",
             icon = icon("home"),
             

             fluidRow(
               div("FunCore",
                    style="font-size:100%;padding:12px;color:white; background-color:#3E3F3A;text-center")
                   ),
             
             br(),
            
            fluidRow(
               column(width = 3,
                      
                      style="font-size:100%;padding:12px;color:white; background-color:#C6BBAA;text-center",
                      
                      
                      valueBox(491," ",color = "black"),
                     infoBox(
                       "Fungi",
                       width = 1,
                       fill = T
                     )
               ),
               
               column(width = 1,
                      style="font-size:100%;padding:12px;color:white; background-color:white;text-center",

               ),
               
               column(width = 3,
                      style="font-size:100%;padding:12px;color:white; background-color:#C6BBAA;text-center",
                      valueBox(60000," ",color = "black"),
                      box(
                        " ",
                        width = 1
                      ),
                      infoBox(
                        "Core genes",
                        width = 1,
                        fill = T,
                        icon = icon("dna")
                      )
               ),
               
             ),


             mainPanel(width = 12,
               fluidRow(
                       h4("About", style = "color:black;"),
                       p("The database for fungal core genes."),
                       p("FunCore provides users to browse the database by the ranking of possibility of being essential genes,
                         giving an exploratory overview of the fungal core gene predictions. Selecting a cluster for closer 
                         inspection will offer the functions and visualized 3D network graph of the network."),
                       p("It is able to query for specific fungal species and sequence.")),
                       
             br())
             
             ),


###############################################.
## About ----
###############################################.



    tabPanel("About",
             icon = icon("info-circle"),
             
             fluidRow(
               div("FunCore",
                   style="font-size:100%;padding:12px;color:white; background-color:#3E3F3A;text-center")
             ),
             
             mainPanel(width = 12,
                       h4("About", style = "color:black;"),
                       #htmlOutput("map")%>% withSpinner(color = "#C6BBAA")
                       leafletOutput("map") %>% withSpinner(color = "#C6BBAA")
             )
             
    ),

###############################################.
## Browse ----
###############################################.


navbarMenu("Browse",
    tabPanel("Subnets",
             icon = icon("list-alt"),
             
             fluidRow(
               div("Browse",
                   style="font-size:100%;padding:12px;color:white; background-color:#3E3F3A;text-center")
               ),
             
             mainPanel(width = 12,
                       br(),
               tabsetPanel(
                 #type ="hidden",
                 tabPanel("Core gene",
                          
                          valueBoxOutput("Subnets"),
                          DT::dataTableOutput("page1") %>% withSpinner(color = "#C6BBAA")
                          
                 ),
                 
                 tabPanel("Sqeuences",
                          inputPanel(
                            numericInput("Subnet ID", label = "Subnet ID:", 0, min(species$`Subnet ID`), max(species$`Subnet ID`))
                          ),
                          textOutput("page2"), 
                          
                          column(width = 5,plotlyOutput("functionp")),
                          column (width = 5,  includeHTML("/Users/pei-yu/Dropbox/2019_Fungal_BGC/FunCore/data/1.html")),
                          
                          DT::dataTableOutput("table2"),getdeps())
                       
                 )))

                         
              
      ,tabPanel("Taxonomy",
                icon = icon("list-alt"),
                   fluidRow(
                     div("Browse",
                         style="font-size:100%;padding:12px;color:white; background-color:#3E3F3A;text-center"),
                     
                   ),
                   
                mainPanel(width = 12,
                
                 jsTreeOutput("taxonomy_tree",width = "200%",) %>% withSpinner(color = "#C6BBAA")  
                # D3partitionROutput("taxonomy_graph") %>% withSpinner(color = "#C6BBAA")    
      
        ))
             
    ),

###############################################.
## Query ----
###############################################.

    tabPanel("Query",
             icon = icon("search"),
             
             
             fluidRow(
               div("FunCore",
                   style="font-size:100%;padding:12px;color:white; background-color:#3E3F3A;text-center")
             ),
             
             mainPanel(width = 12,
                       br(),
                       wellPanel(
                          textInput(inputId="text1", 
                                             label="Input your sequences", 
                                             value="", width=NULL, placeholder="ATCG..."), 
  
                         
                         fileInput("file1", "or choose fasta File",
                                   multiple = FALSE,
                                   accept = c(".fasta",
                                              ".fa",
                                              ".fq")),
  
                         # Input: Select separator ----
                         prettyRadioButtons(
                           inputId = "cluster",
                           label = "Methods", 
                           choices = c("GLM", "Rabdom Forest"),
                           selected = "GLM",
                           status = "info",
                           icon = icon("check"),
                           animation = "jelly"
                         ),
                         # vutton ----
                         actionButton("predict","Predict"),
                         useShinyalert(),  # Set up shinyalert
                         actionButton("preview", "Preview")
                         
                         )
            ),
            br(),
            
            fluidRow( tabPanel('Function callback',  DT::dataTableOutput('contents')))

            
            
    ),






    tabPanel("Help",
             icon = icon("question-circle")
    )),
             


  # navbarPage("© 2021 Copyright: Institute of Plant and Microbial Biology, Academia Sinica",position = "fixed-bottom",
  #            tags$style(HTML(
  #              ".navbar-header {float: centre}"
  #            ))
  # )
  
  # fluidRow(
  #   div("third box",
  #       style="font-size:100%;padding:12px;color:white; background-color:#3E3F3A;text-center",
  #       class="page-footer")
  # )
  
  navbarPage("If you have found the FunCore database useful, please cite us.",
    tags$footer(HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-small blue'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'>© 2021 Copyright:
                           <a href='https://ipmb.sinica.edu.tw/en'> Institute of Plant and Microbial Biology, Academia Sinica</a>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->
                     "))
    )


             
)


