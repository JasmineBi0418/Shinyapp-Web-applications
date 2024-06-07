library(shiny)
library(readr)
library(data.table)
library(dplyr)
library(DT)
library(rsconnect)
library(readxl)
library(htmltools)
library(shinymanager)
library(shinythemes)
library(openxlsx)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(stringr)
library(shinyalert)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
# library(googlesheets4)
# library(googledrive)
#library(shinydashboardPlus)
#drive_auth(cache = ".secrets")
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)
#drive_auth(cache = ".secrets", email = TRUE)
#gs4_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
#gs4_auth(cache = ".secrets", email = TRUE)
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 10000000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 10000000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


callback_js <- JS(
  "table.on('click', 'tr.dtrg-group', function () {",
  "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
  "  $(rowsCollapse).toggleClass('hidden');",
  "});"
)

callback2 <- c(
  "var id = $(table.table().node()).closest('.datatables').attr('id');",
  "$.contextMenu({",
  "  selector: '#' + id + ' td.factor input[type=text]',",
  "  trigger: 'hover',",
  "  build: function($trigger, e){",
  "    var levels = $trigger.parent().data('levels');",
  "    if(levels === undefined){",
  "      var colindex = table.cell($trigger.parent()[0]).index().column;",
  "      levels = table.column(colindex).data().unique();",
  "    }",
  "    var options = levels.reduce(function(result, item, index, array){",
  "      result[index] = item;",
  "      return result;",
  "    }, {});",
  "    return {",
  "      autoHide: true,",
  "      items: {",
  "        dropdown: {",
  "          name: 'Edit',",
  "          type: 'select',",
  "          options: options,",
  "          selected: 0",
  "        }",
  "      },",
  "      events: {",
  "        show: function(opts){",
  "          opts.$trigger.off('blur');",
  "        },",
  "        hide: function(opts){",
  "          var $this = this;",
  "          var data = $.contextMenu.getInputValues(opts, $this.data());",
  "          var $input = opts.$trigger;",
  "          $input.val(options[data.dropdown]);",
  "          $input.trigger('change');",
  "        }",
  "      }",
  "    };",
  "  }",
  "});"
)

createdCell <- function(levels){
  if(missing(levels)){
    return("function(td, cellData, rowData, rowIndex, colIndex){}")
  }
  quotedLevels <- toString(sprintf("\"%s\"", levels))
  c(
    "function(td, cellData, rowData, rowIndex, colIndex){",
    sprintf("  $(td).attr('data-levels', '[%s]');", quotedLevels),
    "}"
  )
}

# data.frame with credentials info
credentials <- as.data.frame(read_excel('www/credentials.xlsx'))
authorized_user<-credentials%>%filter(company=='mirimus')


ui <- secure_app(head_auth = tags$script(inactivity),
                 
                 dashboardPage(
                   dashboardHeader(title = "Tools for RT-qPCR",
                                   # Dropdown menu for messages
                                   dropdownMenu(type = "messages", badgeStatus = "success",
                                                messageItem("Support Team",
                                                            "Please email bi@mirimus.com"
                                                          
                                                            
                                                ),
                                                messageItem("Updates",
                                                            "Multiple themes added for fun, enjoy!:)",
                                                            time = "Today"
                                                )
                                   )
                                   ),skin = "purple",
                   
                   dashboardSidebar(
                     #tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
                     shinythemes::themeSelector(),
                     # The dynamically-generated user panel
                     #uiOutput("userpanel"),
                    
                
                     
                     sidebarMenu(
                      # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                
                       menuItem("PCR Results Analyzer",  icon = icon("stats",lib = "glyphicon"),
                                startExpanded = TRUE,
                                menuSubItem("Results",
                                            tabName = "upload1"),
                                fileInput("file1", h6(" Import file"),accept = c(".csv",'.xls')),
                                selectInput("usageType", "Select Experiment Type:",
                                            choices = c("Regular", "R&D Purpose"))),
                       menuItem("PCR Template Generator", tabName = "upload2", icon = icon("table"))
                       #menuItem("Insufficiency&empty",  icon = icon("tasks", lib = "glyphicon"),tabName = "upload3"),
                       #menuItem("PCR Database",  icon =icon("modal-window",lib = "glyphicon"),tabName = "upload5"),
                       #menuItem("DOH reporting tool",  icon = icon("list-alt", lib = "glyphicon"),tabName = "upload4")
                       #menuItem("Feedback",  icon = icon("fa-solid fa-comment-lines", lib = "font-awesome"),tabName = "upload6")
                       
                     )
                   ),
                   dashboardBody(
                     tabItems(
                       # tabItem(tabName = "dashboard",
                       #         fluidRow(
                       #           
                       #           infoBoxOutput("progressBox",width = 6),
                       #           infoBoxOutput("approvalBox",width = 6),
                       #           br(),
                       #           br(),
                       #           br(),
                       #           box(
                       #             title = 'Data Overview',status = 'info',solidHeader = FALSE,
                       #             collapsible = TRUE,width = 12,height = 700,
                       #             plotlyOutput("plot_data")%>%withSpinner(type = 5)
                       #             #downloadButton('downloadPlot', 'Download Plot')
                       #             
                       #           )
                       #         )
                       # ),
                       # First tab content
                       tabItem(tabName = "upload1",
                               fluidRow(
                                 # box(
                                 #   title = "Import result csv file from QS7Pro", status = "info", solidHeader = TRUE,
                                 #   fileInput("file1", h6(" Import file 1"),accept = c(".csv",'.xls')),background = "black",
                                 # ),
                                 # box(
                                 #   title = "Import print summary from Reporter", status = "info", solidHeader = TRUE,
                                 #   fileInput("file1_1", h6(" Import file 2"),accept = c(".xlsx")),background = "black",
                                 # ),
                                 box(
                                   title = "Overall Results Stats", status = "info", solidHeader = TRUE,
                                   collapsible = TRUE,width = 12,collapsed = TRUE,
                                   plotOutput("plot3", height = 250)
                                 ),
                                 # box(
                                 #   title = "Reflex summary", status = "warning", solidHeader = TRUE,
                                 #   collapsible = TRUE,
                                 #   plotOutput("plot4", height = 250)
                                 # ),
                                 
                                 box(
                                   title = "Result Table", status = "success", solidHeader = TRUE,width = 12,
                                   
                                   collapsible = TRUE,
                                   tags$a(href="https://apps.thermofisher.com/apps/spa/#/dataconnect",target='_blank', "Go to ThermoFisher DataConnect!"),
                                   br(),
                                   dataTableOutput('contents1'))
                               )
                       ),
                       
                       #2nd tab content
                       tabItem(tabName = "upload2",
                               fluidRow(
                                 box(title = "Step1-Parameter Input", width = 12, solidHeader = T, status = "primary", 
                                     fluidRow(
                                       
                                       column(3,radioButtons("Software", 
                                                             p(strong("Software"),style="color:black"),
                                                             
                                                             choices = list("QuantStudio 7 Pro system" , 
                                                                            "Viia 7 system" , 
                                                                            "CFX96"),
                                                             selected = 'QuantStudio 7 Pro system')),
                                       column(3,radioButtons("Method", 
                                                             p(strong("Test Method"),style="color:black"),
                                                             choices = list("SalivaClear" , 
                                                                            "SalivaDirect" ,
                                                                            "Eurofins"
                                                             ),
                                                             selected = 'SalivaClear')),
                                       column(3,radioButtons("Reaction", 
                                                             p(strong("Reaction Replicate"),style="color:black"),
                                                             choices = list("Triplicate" = 3, 
                                                                            "Duplicate" = 2, 
                                                                            "Single" = 1,
                                                                            "Triplicate & Duplicate" = 'MIX'),
                                                             selected = 3)),
                                       column(3,radioButtons("Plate", 
                                                             p(strong("Plate Type"),style="color:black"),
                                                             choices = list("384 Well" = 384, 
                                                                            "96 Well" = 96 
                                                             ),
                                                             selected = 384))),
                                     column(3,textInput("PC", h6("Add Positive Control Well Position"), value = "B23")),
                                     column(3,selectInput('startpos',h6('Choose Plate A column start#'),c(seq(1,23,2)))),
                                     column(2,textInput("run", h6("Input Template Run#"), value = '1')),
                                     column(2,textInput("Pool", h6("Pool Tube Prefix"), value = "EQ")),
                                     column(2,h6("Confirm your selections"), actionButton("do", "Confirm"))),
                                 box(title = "Step2-Import PoolRack files", width = 12, solidHeader = T, status = "primary", 
                                     column(3,fileInput("temp_file1", h6(" Import PoolRack1 file"),accept = c(".csv"))),
                                     column(3,fileInput("temp_file2", h6(" Import PoolRack2 file"),accept = c(".csv"))),
                                     column(3,fileInput("temp_file3", h6(" Import PoolRack3 file"),accept = c(".csv"))),
                                     column(3,fileInput("temp_file4", h6(" Import PoolRack4 file"),accept = c(".csv")))
                                 ),
                                 box(
                                   title = "Step3-Parameters Input Preview", status = "primary", solidHeader = TRUE,width = 12,
                                   collapsible = TRUE, dataTableOutput('contents4')),
                                 box(
                                   title = "Template Preview", status = "success", solidHeader = TRUE,width = 12,
                                   collapsible = TRUE,
                                   br(),
                                   downloadButton("download", "Download.txt"),
                                   br(),
                                   br(),
                                   dataTableOutput('contents5')))
                       ),
                       #3rd tab content
                       tabItem(tabName = "upload3",
                               
                               fluidRow(
                                 box(
                                   title = "Import Barcode scan file .csv", status = "warning", solidHeader = TRUE,
                                   fileInput("file3_Ins", h6(" Import file 1"),accept = c(".csv")),background = "black",
                                 ),
                                 box(
                                   title = "Import Result_auto.xlsx", status = "warning", solidHeader = TRUE,
                                   fileInput("file4_Ins", h6(" Import file 2"),accept = c(".xlsx")),background = "black",
                                 ),
                                 box(
                                   title = "Missing tubes", status = "warning", solidHeader = TRUE,width = 12,
                                   background = "red",collapsible = TRUE,textOutput('missing')
                                 ),
                                 box(
                                   title = "Result", status = "success", solidHeader = TRUE,width = 12,
                                   collapsible = TRUE,dataTableOutput('contents2_Ins'))
                               )
                       )
                       # #4th tab content
                       # ,tabItem(tabName = "upload4",
                       #         
                       #         fluidPage(tabsetPanel(
                       #           tabPanel('Reporting Tool Guidance',
                       #                    br(),
                       #                    br(),
                       #                    p(strong(h3('SARS-COV2 DOH Reporting Tool Guidance'))),
                       #                    br(),
                       #                    p(h4('1. This tool currently can convert Mirimus NPI to New York and the New Jersy States, and please choose the state you need to report to DOH after uploading the PHI; Please note that Illinois, Vermont, and California will need manual entry;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('2. Please upload patient information with EXACT column names as follows(NO need to track the order): Last Name, First Name, Middle Name, Date of Birth, Race, Ethnicity (Hispanic or Non-Hispanic), Sex, Address, City, State, Zip, Phone Number, sample_type;
                       #                    company_short, tube_ids, date_of_receipt, results_time, results;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('3. Please upload patient information with FULL ADDRESS, including address, city(not short), state, and zip; otherwise, we cannot report to DOH;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('4. The correct zip code format you uploaded should be five digits(#####); if not, please do so before uploading the file. Please note this tool will automatically add leading zero if the zip missed that;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('5. Please change the format of date_of_receipt,results_time to short date before uploading the file;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('6. Acceptable patient sex formats of NY state are female, male, transsexual, hermaphrodite/undetermined, unknown, and other;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('7. Acceptable patient race formats of NY state are Asian or Pacific Islander, Black, Native American or Alaskan Native, Multiracial, White, unknown and other;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('8. Acceptable patient ethnicity formats of NY state are Hispanic and non-Hispanic;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('9. The acceptable file format for uploading PHI is xlsx;
                       #                 ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    p(h4('10. For more detail of the Guidance, including the account name and password, please go to the Google Doc! ',tags$a(href="https://docs.google.com/document/d/1wxVGijLygLImD2yir9DjsZToccEzSnncNqycedv8yyY/edit",target='_blank', "State DOH Reporting Accounts/Reporting Guide"),
                       #                         style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       #                    
                       #                    br(),
                       #                    p(em('Any questions please contact bi@mirimus.com.
                       #                 ',style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"))
                       #                    
                       #           ),
                       #           tabPanel('Batch file output',
                       #                    column(br(),
                       #                           fileInput("file_doh", p(code("Import patient information")),accept = c(".xlsx")),width=4),
                       #                    column(br(),selectInput('state',p(code('Choose DOH reporting state')),c("New York",'New Jersey')),width=4),
                       #                    column(br(),downloadButton("download_doh", "Download tsv file format for NY state"),
                       #                           br(),
                       #                           tags$a(href="https://commerce.health.state.ny.us/hcs/index.html",target='_blank', "Go to New York DOH reporting link!"),
                       #                           br(),
                       #                           downloadButton("download_doh2", "Download csv file format for NJ state"),
                       #                           br(),
                       #                           tags$a(href="https://njgov.moveitcloud.com/human.aspx?r=391490559&arg12=signon",target='_blank', "Go to New Jersey DOH reporting link!"),
                       #                           width=4),
                       #                    br(),
                       #                    br(),
                       #                    br(),
                       #                    #h3(p(em("Output Preview "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                       #                    dataTableOutput('contents_D'))
                       #         )
                       #         
                       #         
                       #         )
                       # ),
                       #the 5th tab
                       #3rd tab content
                       # tabItem(tabName = "upload5",
                       #         
                       #         fluidRow(
                       #           
                       #           tabBox(
                       #             #title = tagList(shiny::icon("gear"), "PCR results Database"),title = "PCR results Database",
                       #             width=12,
                       #             id='tabset1',height='250px',
                       #             tabPanel("Search single barcode", dataTableOutput('contents_pcr1')%>%withSpinner(type = 1)),
                       #             tabPanel("Search list of barcodes", fileInput("file_pcr", p(code("Import list of barcodes")),
                       #                                                           accept = c(".xlsx")),width=12,dataTableOutput('contents_pcr2')%>%withSpinner(type = 4))
                       #           )
                       #           
                       #         )
                       # )
                       # 
                       
                     )
                   ))
)
#reactiveValuesToList(result_auth)[[1]]
server <- function(input, output,session) {
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  # data_sum<-reactive({
  #   con_t<-read_sheet('https://docs.google.com/spreadsheets/d/1P98_i5S0e_QzAZQlCqIVHBefOaxZ6nukDAxtM4s2Hdw/edit#gid=0')
  #   colnames(con_t)[4]<-'Testing_date'
  #   con_t$Testing_date<-as.Date(con_t$Testing_date)
  #   con_t
  # })
  #shinyalert('Welcome',"Successfully logged in!", type = "success")
  # output$progressBox <- renderInfoBox({
  #   print(reactiveValuesToList(result_auth)[[1]])
  #   t<-Sys.time()-14000
  #   print(t)
  #   con_t<-data_sum()
  #   con2<-con_t%>%summarise(pool=sum(Pool_number),sample=sum(Individual_number))
  #   info_value<-con2$pool
  #   infoBox(
  #     "Total Pool(Data as of 01/01/2021)", h2(info_value,style = "color:white"), icon = icon("list"),fill = TRUE,
  #     color = "purple"
  #   )
  # })
  # output$approvalBox <- renderInfoBox({
  #   con_t<-data_sum()
  #   con2<-con_t%>%summarise(pool=sum(Pool_number),sample=sum(Individual_number))
  #   info_value<-con2$sample
  #   infoBox(
  #     "Total Sample(Data as of 01/01/2021)",h2(info_value,style = "color:white"), icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE,
  #     color = "yellow"
  #   )
  # })
  #add plot for dashboard
  # fig1 <- reactive({
  #   con_t<-data_sum()
  #   con_t$Testing_date<-as.Date(con_t$Testing_date)
  #   p<-con_t%>%group_by(Testing_date)%>%mutate(total=sum(Individual_number))%>%unique()%>%
  #     ggplot(aes(x = Testing_date,y=total,fill=Company_short)) +
  #     geom_bar(position = 'stack',stat = 'identity',show.legend = FALSE,alpha=1)+
  #     scale_x_date(date_breaks = "14 days", date_labels = "%b\n%d")+
  #     labs(x = "Testing Date",y = "Daily Testing Number")+
  #     theme(text = element_text(size=8))
  # })
  # output$plot_data<-renderPlotly({
  #   fig1<-ggplotly(fig1())
  # })
  
  # output$downloadPlot <- downloadHandler(
  #   filename = function() {"Data overview.pdf"},
  #   content = function(file) {
  #     temp_dir <- tempdir()
  #     tempImage <- file.path(temp_dir, 'out.png')
  #     file.copy('out.png', tempImage, overwrite = TRUE)
  #     png(file, width = 1200, height = 800, units = "px", pointsize = 12, bg = "white", res = NA)
  #     dev.off()
  #   }
  # )
  #result
  data_1<-reactive({
    inFile1 <- input$file1
    filename1<-substr(inFile1$name,1,nchar(inFile1$name)-44)
    if (is.null(inFile1))
      return(NULL)
    fileformat<-substr(inFile1$name,nchar(inFile1$name)-3,nchar(inFile1$name))
    if (fileformat!='.csv'){
      result_welltable <- read_excel(inFile1$datapath,sheet = "Results")
      #User<-as.character(result_welltable[44,2])
      User<-paste('Login User Name: ',reactiveValuesToList(result_auth)[[1]],sep = '')
      RunStartDate<-as.character(result_welltable[28,2])
      RunEndDate<-as.character(result_welltable[33,2])
      result<-result_welltable[-c(1:46),]
      colnames(result)<-result_welltable[46,]
      #Choose interested columns
      result<-as.data.frame(result,stringsAsFactors = FALSE)
      result<-result%>%select(`Well Position`,`Sample Name`,`Target Name`,`Amp Status`,CT,`Cq Conf`,`Ct Mean`,`Ct SD`)
      colnames(result)<-c('Well.Position','Sample','Target','Amp.Status','Cq','Cq.Confidence','Cq.Mean','Cq.SD')
      result[,c(5:8)]<-suppressWarnings(lapply(result[,c(5:8)],as.numeric))
    }else{
      result_welltable <- read.table(inFile1$datapath,header = TRUE, sep = ',',stringsAsFactors = FALSE)
      #Choose interested columns
      result<-result_welltable
      result<-result%>%select(`Well.Position`,Sample,Target,`Amp.Status`,Cq,`Cq.Confidence`,`Cq.Mean`,`Cq.SD`)
      #str(result)
      result[,c(5:8)]<-suppressWarnings(lapply(result[,c(5:8)],as.numeric))
      review1<-result%>%filter(Target!='MS2')%>%filter(Target!='RP')%>%filter(`Cq.Mean`>32)%>%filter(!Sample%in%c('PC','HBSS','EMPTY','BLANKS','H2O'))%>%select(Sample)%>%unique()
      review2<-result%>%filter(Target!='MS2')%>%filter(Target!='RP')%>%filter(!is.na(`Cq.Mean`))%>%filter(`Cq.SD`>1|is.na(`Cq.SD`))%>%filter(!Sample%in%c('PC','HBSS','EMPTY','BLANKS','H2O'))%>%select(Sample)%>%unique()
      review3<-result%>%filter(Target!='MS2')%>%filter(Target=='RP')%>%filter(!is.na(`Cq.Mean`))%>%filter(Sample%in%c('PC','HBSS','EMPTY','BLANKS','H2O'))%>%filter(`Cq.Mean`<36)%>%select(Sample)%>%unique()
      negative_sample<-result%>%filter(Target!='MS2')%>%filter(Target!='RP')%>%filter(`Cq.Mean`>35)%>%filter(!Sample%in%c('PC','HBSS','EMPTY','BLANKS','H2O'))%>%select(Sample)%>%unique()
      review<-rbind(review1,review2,review3)
      review<-review%>%unique()
      
      test<-read.csv(inFile1$datapath,header = F,sep=",")
      # User<-as.character(test$V1[3])
      # User<-substr(User,2,nchar(User))
      User<-paste('Login User Name: ',reactiveValuesToList(result_auth)[[1]],sep = '')
      RunStartDate<-as.character(test$V1[11])
      RunStartDate<-substr(RunStartDate,2,nchar(RunStartDate))
      RunEndDate<-as.character(test$V1[12])
      RunEndDate<-substr(RunEndDate,2,nchar(RunEndDate))
    }
    result1<-result%>%filter(Sample!="NA")%>%filter(!is.na(Sample))
    Incon_s<-result1%>%filter(Target!='MS2')%>%
      filter(`Amp.Status`=='Inconclusive')%>%
      select(Sample,`Well.Position`)%>%
      unique()
    wellp_in<-data.frame(stringsAsFactors = FALSE)
    if(nrow(Incon_s)!=0){
      for (i in unique(Incon_s$Sample)){
        pos<-c(Incon_s[Incon_s$Sample==i,2])
        poss<-paste(pos,collapse = ';')
        well<-cbind(i,poss)
        wellp_in<-rbind(wellp_in,well)
      }
      colnames(wellp_in)<-c('Sample','Inconclusive_Well')
      Incon_s<-wellp_in
    }
    lowconf_s <- result1 %>%
      filter(Target != 'MS2') %>%
      filter(Cq.Confidence < 0.7, Cq.Confidence > 0) %>%
      select(Sample, Target, `Well.Position`) %>%
      group_by(Sample) %>%
      summarize(Count = n())
    
    # for (i in 1:nrow(result1)){
    #   if (result1[i,4]=='Inconclusive') {
    #     result1[i,5]=NA
    #   }
    # }
    colnames(lowconf_s)[2]<-'Low confidence_Count'
    Well_pos<-result1%>%
      select(Sample,`Well.Position`)%>%arrange(Sample)%>%unique()
    Well_n<-result1%>%as.data.frame()%>%
      select(Sample, `Well.Position`) %>%
      arrange(Sample) %>%
      unique() %>%
      group_by(Sample)%>%
      summarize(Count = n())
    # for (i in 1:nrow(result1)){
    #   if (is.na(result1[i,4])==FALSE & result1[i,4] >35) {
    #     result1[i,4]=NA
    #   }
    # }
    result_RP_2<-result1 %>%
      filter(Target=='RP')%>%arrange(Sample)%>%
      group_by(Sample) %>%
      summarise(Cq_mean=mean(Cq,na.rm=TRUE),Cq_sd=sd(Cq,na.rm=TRUE))
    Invalid_RP<- result_RP_2%>%
      filter(Cq_mean>30.1)%>%
      select(Sample)%>%group_by(Sample)
    
    Target_num<-length(unique(result1$Target))
    Targets<-unique(result1$Target)
    test<-result1%>%group_by(Sample,Target)%>%
      summarise(Cq_mean=mean(Cq,na.rm=TRUE),Cq_sd=sd(Cq,na.rm=TRUE))
    test2<-test%>%mutate(Cq=paste(round(Cq_mean,2),'(',round(Cq_sd,2),')',sep = ''))%>%
      select(Sample,Target,Cq)%>%
      spread(Target,Cq)
    test2<-as.data.frame(test2,stringsAsFactors = FALSE)
    rownames(test2)<-test2$Sample
    wellp<-data.frame(stringsAsFactors = FALSE)
    for (i in test2$Sample){
      pos<-c(Well_pos[Well_pos$Sample==i,2])
      poss<-paste(pos,collapse = ';')
      well<-cbind(i,poss)
      wellp<-rbind(wellp,well)
    }
    
    wellp<-as.data.frame(wellp,stringsAsFactors = FALSE)
    colnames(wellp)<-c('Sample',"Well_position")
    test3<-as.data.frame(left_join(test2,wellp,by="Sample"),stringsAsFactors = FALSE)
    #rownames(data)<-data$SampleID\
    if (input$usageType == "R&D Purpose") {
      # Logic to process and return the final results of test3
      data<-test3  # Replace with your actual code to get test3 final results
    }else{
      if(Target_num==5 && Targets==c("MS2","N gene","ORF1ab","S gene","RP")){
        test4<-test3%>%select(Sample,Well_position,MS2,`N gene`,ORF1ab,`S gene`,RP)
        test4$Well_position<-as.character(test4$Well_position)
        data<-test4
        result_a<-data.frame(stringsAsFactors = FALSE)
        for (i in 1:nrow(data)){ 
          a <-length(which(data[i,c(4:6)]=='NaN(NA)'))
          #print(a)
          if (a == 3){
            b<-'Not Detected'
          }else if(a == 2){
            b<-'For Review'#inconclusive
          }
          else{
            b<-"Detected"
          }
          re<-cbind(data[i,1],b)
          result_a<-rbind(result_a,re)
        }
        result_a<-as.data.frame(result_a,stringsAsFactors = FALSE)
        colnames(result_a)<-c('Sample','Analysis_Results')
        data<-left_join(data,result_a,by='Sample')
        data<-left_join(data,Incon_s,by='Sample')
        data<-left_join(data,lowconf_s,by='Sample')
        data<-as.data.frame(unique(data),stringsAsFactors = FALSE)
        rownames(data)<-unique(data$Sample)
        data$Analysis_Results<-as.character(data$Analysis_Results)
        data[Invalid_RP$Sample,8]<-'Invalid'
        for  (i in 1:nrow(data)){
          a<-data[i,7]=='NaN(NA)'
          if(!is.na(a)){
            if (a==TRUE){
              data[i,8]<-'Invalid'
            }
          }
        }
        colnames(data)[9]<-'Inconclusive_Well'
      }else if(Target_num==4 && Targets==c("N gene","ORF1ab","S gene","RP")){
        test4<-test3%>%select(Sample,Well_position,`N gene`,ORF1ab,`S gene`,RP)
        data<-test4
        result_a<-data.frame(stringsAsFactors = FALSE)
        for (i in 1:nrow(data)){ 
          a <-length(which(data[i,c(3:5)]=='NaN(NA)'))
          #print(a)
          if (a == 3){
            b<-'Not Detected'
          }else if(a == 2){
            b<-'For Review'#inconclusive
          }
          else{
            b<-"Detected"
          }
          re<-cbind(data[i,1],b)
          result_a<-rbind(result_a,re)
        }
        result_a<-as.data.frame(result_a,stringsAsFactors = FALSE)
        colnames(result_a)<-c('Sample','Analysis_Results')
        data<-left_join(data,result_a,by='Sample')
        data<-left_join(data,Incon_s,by='Sample')
        data<-left_join(data,lowconf_s,by='Sample')
        data<-as.data.frame(unique(data),stringsAsFactors = FALSE)
        rownames(data)<-unique(data$Sample)
        data[Invalid_RP$Sample,7]<-'Invalid'
        for  (i in 1:nrow(data)){
          a<-data[i,6]=='NaN(NA)'
          if(!is.na(a)){
            if (a==TRUE){
              data[i,7]<-'Invalid'
            }
          }
        }
        colnames(data)[8]<-'Inconclusive_Well'
      }else if(Target_num==3){
        data<-test3
        result_a<-data.frame(stringsAsFactors = FALSE)
        for (i in 1:nrow(data)){ 
          a <-length(which(data[i,c(2,3)]=='NaN(NA)'))
          #print(a)
          if (a == 0){
            b<-'Detected'
          }else if (a==1){
            b<-'For Review'#inconclusive
          }else{
            b<-"Not Detected"
          }
          re<-cbind(data[i,1],b)
          result_a<-rbind(result_a,re)
        }
        result_a<-as.data.frame(result_a,stringsAsFactors = FALSE)
        colnames(result_a)<-c('Sample','Analysis_Results')
        data<-left_join(data,result_a,by='Sample')
        data<-left_join(data,Incon_s,by='Sample')
        data<-left_join(data,lowconf_s,by='Sample')
        data<-as.data.frame(unique(data),stringsAsFactors = FALSE)
        rownames(data)<-unique(data$Sample)
        data[Invalid_RP$Sample,6]<-'Invalid'
        for  (i in 1:nrow(data)){
          a<-data[i,4]=='NaN(NA)'
          if(!is.na(a)){
            if (a==TRUE){
              data[i,6]<-'Invalid'
            }
          }
        }
        colnames(data)[7]<-'Inconclusive_Well'
        
      }else{
        data<-test3
        result_a<-data.frame(stringsAsFactors = FALSE)
        for (i in 1:nrow(data)){ 
          a <-length(which(data[i,2]=='NaN(NA)'))
          #print(a)
          if (a == 1){
            b<-'Not Detected'
          }
          else{
            b<-"Detected"
          }
          re<-cbind(data[i,1],b)
          result_a<-rbind(result_a,re)
        }
        result_a<-as.data.frame(result_a,stringsAsFactors = FALSE)
        colnames(result_a)<-c('Sample','Analysis_Results')
        data<-left_join(data,result_a,by='Sample')
        data<-left_join(data,Incon_s,by='Sample')
        data<-left_join(data,lowconf_s,by='Sample')
        data<-as.data.frame(unique(data),stringsAsFactors = FALSE)
        rownames(data)<-unique(data$Sample)
        data[Invalid_RP$Sample,'Analysis_Results']<-'Invalid'
        for  (i in 1:nrow(data)){
          a<-data[i,'RP']=='NaN(NA)'
          if(!is.na(a)){
            if (a==TRUE){
              data[i,'Analysis_Results']<-'Invalid'
            }
          }
        }
        #colnames(data)[6]<-'Inconclusive_Well'
      }
      data<-within(data, Analysis_Results[Sample %in% review$Sample] <- 'For Review')
      data<-within(data, Analysis_Results[Sample %in% negative_sample$Sample] <- 'For Review')
      #remove NA values
      data[data=="NaN(NA)"]<-''
      data<-replace(data, is.na(data),'')
      colnames(Well_n)<-c('Sample','Number_of_replicate')
      data<-left_join(data,Well_n,by = "Sample")
      colnames(data)[1]<-'SampleID'
      
      inFile1_1 <- input$file1_1
      
      if (is.null(inFile1_1)){
        data<-data
      }else{
        f2<-read_excel(inFile1_1$datapath)
        #f2<-read_excel('www/20210821_Run2_print_summary_example.xlsx')
        f2<-f2[,c(1:13)]
        #f3<-f2[,c(12:13)]
        colnames(f2)[5]<-'SampleID'
        #data2<-as.data.frame(read_excel('www/Book5.xlsx'))
        library(dplyr)
        data2<-left_join(data,f2,by = 'SampleID')
        if(ncol(data)==11){
          data2<-cbind(data2[,c(12:21)],data2[,c(1:11)],data2[,c(22:23)])
          data2<-cbind(data2[,c(1:4)],data2[,11],data2[,c(5:10,12:23)])
          colnames(data2)[5]<-'SampleID'
          Non_NasalSwab<-data2%>%filter(`Sample Type`!='SWAB'|is.na(`Company Name`))
          NasalSwab<-data2%>%filter(`Sample Type`=='SWAB')
          NasalSwab<-replace(NasalSwab, is.na(NasalSwab),'')
          NasalSwab2<-NasalSwab%>%separate(RP, c('RP','sd'),'[()]')%>%select(!sd)
          if (nrow(NasalSwab2)>0){
            for ( i in 1:nrow(NasalSwab)){
              if(NasalSwab[i,17]!=''){
                a<-length(which(NasalSwab[i,c(14:16)]==''))
                if (a==0 &&  as.numeric(NasalSwab2[i,17])<=33 ){
                  NasalSwab[i,18]<-'Detected'
                }else if(a==2 && as.numeric(NasalSwab2[i,17])<=33 ){
                  NasalSwab[i,18]<-'For Review'#inconclusive
                }else if(a==3 && as.numeric(NasalSwab2[i,17])<=33 ){
                  NasalSwab[i,18]<-'Not Detected'
                }else if(a==1 && as.numeric(NasalSwab2[i,17])<=33 ){
                  NasalSwab[i,18]<-'Detected'
                }
              }
            }
            data2<-rbind(NasalSwab,Non_NasalSwab)
          }
          data2$Results<-data2$Analysis_Results
          data<-data2
        }else if(ncol(data)==10){
          data2<-cbind(data2[,c(12:20)],data2[,c(1:11)])
          data2<-cbind(data2[,20],data2[,11],data2[,c(1:19)])
          colnames(data2)[1]<-'Index'
          data2$Results<-data2$Analysis_Results
          data<-data2
        }else if(ncol(data)==8){
          data2<-cbind(data2[,c(9:18)],data2[,c(1:8)],data2[,c(19:20)])
          data2<-cbind(data2[,c(1:4)],data2[,11],data2[,c(5:10,12:20)])
          colnames(data2)[5]<-'SampleID'
          #data<-read_excel('www/20210813 COVID19 RUN 3 SalivaDirect QS3 AA-SalivaDirect-PCR result analysis.xlsx')
          SD_data<-replace(data2, is.na(data2),'')
          SD_data2<-SD_data%>%separate(RP, c('RP','sd'),'[()]')%>%select(!sd)
          for ( i in 1:nrow(SD_data)){
            if (SD_data[i,13]!=''){
              a<-length(which(SD_data[i,12]!=''))
              if (a==1  && as.numeric(SD_data2[i,13])<=30 ){
                SD_data[i,c(4,15)]<-'Detected'
              }else if(a==0  && as.numeric(SD_data2[i,13])<=30 ){
                SD_data[i,c(4,15)]<-'Not Detected'
              }
            }
          }
          data2<-SD_data
          data2$Results<-data2$Analysis_Results
          data<-data2
        }else if(ncol(data)==9){
          data2<-cbind(data2[,c(10:19)],data2[,c(1:9)],data2[,c(20:21)])
          data2<-cbind(data2[,c(1:4)],data2[,11],data2[,c(5:10,12:21)])
          colnames(data2)[5]<-'SampleID'
          Non_NasalSwab<-data2%>%filter(`Sample Type`!='SWAB'|is.na(`Company Name`))
          NasalSwab<-data2%>%filter(`Sample Type`=='SWAB')
          NasalSwab<-replace(NasalSwab, is.na(NasalSwab),'')
          NasalSwab2<-NasalSwab%>%separate(RP, c('RP','sd'),'[()]')%>%select(!sd)
          if (nrow(NasalSwab2)>0){
            for ( i in 1:nrow(NasalSwab)){
              if(NasalSwab[i,14]!=''){
                a<-length(which(NasalSwab[i,c(12:13)]==''))
                if (a==0 &&  as.numeric(NasalSwab2[i,14])<=33 ){
                  NasalSwab[i,16]<-'Detected'
                }else if(a==1 && as.numeric(NasalSwab2[i,14])<=33 ){
                  NasalSwab[i,16]<-'For Review'#'Inconclusive
                }else if(a==2 && as.numeric(NasalSwab2[i,14])<=33 ){
                  NasalSwab[i,16]<-'Not Detected'
                }
              }
            }
            data2<-rbind(NasalSwab,Non_NasalSwab)
          }
          data2$Results<-data2$Analysis_Results
          data<-data2
        }
      }
      data<-data%>%arrange(Analysis_Results)
    }
  })
  output$contents1 <- DT::renderDataTable({
    inFile1 <- input$file1
    filename1<-substr(inFile1$name,1,nchar(inFile1$name)-44)
    
    
    if (is.null(inFile1))
      return(NULL)
    fileformat<-substr(inFile1$name,nchar(inFile1$name)-3,nchar(inFile1$name))
    if (fileformat!='.csv'){
      result_welltable <- read_excel(inFile1$datapath,sheet = "Results")
      User<-as.character(result_welltable[44,2])
      RunStartDate<-as.character(result_welltable[28,2])
      RunEndDate<-as.character(result_welltable[33,2])
      result<-result_welltable[-c(1:46),]
      colnames(result)<-result_welltable[46,]
      #Choose interested columns
      result<-as.data.frame(result,stringsAsFactors = FALSE)
      result<-result%>%select(`Well Position`,`Sample Name`,`Target Name`,`Amp Status`,CT,`Cq Conf`,`Ct Mean`,`Ct SD`)
      colnames(result)<-c('Well.Position','Sample','Target','Amp.Status','Cq','Cq.Confidence','Cq.Mean','Cq.SD')
      result[,c(5:8)]<-suppressWarnings(lapply(result[,c(5:8)],as.numeric))
    }else{
      result_welltable <- read.table(inFile1$datapath,header = TRUE, sep = ',',stringsAsFactors = FALSE)
      #Choose interested columns
      result<-as.data.frame(result_welltable,stringsAsFactors = FALSE)
      result<-result%>%select(`Well.Position`,Sample,Target,`Amp.Status`,Cq,`Cq.Confidence`,`Cq.Mean`,`Cq.SD`)
      #str(result)
      result[,c(5:8)]<-suppressWarnings(lapply(result[,c(5:8)],as.numeric))
      test<-read.csv(inFile1$datapath,header = F,sep=",")
      # User<-as.character(test$V1[3])
      # User<-substr(User,2,nchar(User))
      User<-paste('Login User Name: ',reactiveValuesToList(result_auth)[[1]],sep = '')
      RunStartDate<-as.character(test$V1[11])
      RunStartDate<-substr(RunStartDate,2,nchar(RunStartDate))
      RunEndDate<-as.character(test$V1[12])
      RunEndDate<-substr(RunEndDate,2,nchar(RunEndDate))
    }
    data<-data_1()
    if (ncol(data)==20){
      filename1<-paste(filename1,'-SalivaDirect',sep="")
    }else if(ncol(data)==23){
      filename1<-paste(filename1,'-SalivaClear',sep = '')
    }else if (ncol(data)==21){
      filename1<-paste(filename1,'-Eurofins',sep = '')
    }
    if (input$usageType == "R&D Purpose") {
      data <- data %>%
        mutate(across(everything(), ~ifelse(. == "NaN(NA)", "", .)))
      
      datatable(data = data
                , caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left',
                                                    HTML(filename1,'<br/>',User,'<br/>',RunStartDate,'<br/>',RunEndDate))
                ,extensions=c("Buttons",'Scroller')
                ,rownames=F
                ,editable = "cell"
                ,options = list(searchHighlight = TRUE
                                ,dom = "Blfrtip"
                                ,scrollY = 1000
                                ,scroller = TRUE
                                ,scrollX=TRUE
                                
                                ,buttons = 
                                  list("copy", 'print',list(
                                    extend = "collection"
                                    , buttons = list(list(extend="excel",title = filename1,messageBottom=Sys.time()
                                                          , customize=JS("function (xlsx) {
                                                                    var sheet = xlsx.xl.worksheets['sheet1.xml'];
                                                                    var col = $('col', sheet);
                                                                    col.each(function () {
                                                                      $(this).attr('width', 10);
                                                                    });
                                                                  }")
                                                          ,filename=paste(filename1,'PCR result analysis',sep = '-')),
                                                     list(extend="pdf"
                                                          ,exportOptions = list(stripHtml = FALSE,
                                                                                columns = ':visible'),
                                                          orientation = 'landscape',
                                                          customize = JS("function(doc){console.dir(doc);}")
                                                          , filename=paste(filename1,'PCR result analysis',sep = '-')))
                                    , text = "Download"
                                  ) ) # end of buttons customization
                                
                                # customize the length menu
                                , lengthMenu = list(c(50,100, -1) # declare values
                                                    , c(50,100, "All") # declare titles
                                ) # end of lengthMenu customization
                                , pageLength = -1
                ) # end of options
                
      )
    }else {
    
      datatable(data = data
              , caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left',
                                                  HTML(filename1,'<br/>',User,'<br/>',RunStartDate,'<br/>',RunEndDate))
              ,extensions=c("Buttons",'Scroller')
              ,rownames=F
              ,editable = "cell"
              ,options = list(searchHighlight = TRUE
                               ,dom = "Blfrtip"
                               ,scrollY = 1000
                               ,scroller = TRUE
                               ,scrollX=TRUE
                               
                               ,buttons = 
                                 list("copy", 'print',list(
                                   extend = "collection"
                                   , buttons = list(list(extend="excel",title = filename1,messageBottom=Sys.time()
                                                         , customize=JS("function (xlsx) {
                                                                    var sheet = xlsx.xl.worksheets['sheet1.xml'];
                                                                    var col = $('col', sheet);
                                                                    col.each(function () {
                                                                      $(this).attr('width', 10);
                                                                    });
                                                                  }")
                                                         ,filename=paste(filename1,'PCR result analysis',sep = '-')),
                                                    list(extend="pdf"
                                                         ,exportOptions = list(stripHtml = FALSE,
                                                                               columns = ':visible'),
                                                         orientation = 'landscape',
                                                         customize = JS("function(doc){console.dir(doc);}")
                                                         , filename=paste(filename1,'PCR result analysis',sep = '-')))
                                   , text = "Download"
                                 ) ) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list(c(50,100, -1) # declare values
                                                   , c(50,100, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = -1
              ) # end of options
              
    )%>%formatStyle(
      'Analysis_Results',
      target = 'row',
      backgroundColor = styleEqual(c('Detected','Invalid','Inconclusive','For Review'), c('#ffe4e4','#FDBB9F','#fccf3e','#ed553b')))
    }
    
  })
  #plot
  output$plot3<-renderPlot({
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    data<-data_1()
    DATA<-data%>%filter(SampleID!='BLANKS')%>%filter(SampleID!='EMPTY')%>%filter(SampleID!='H2O')%>%filter(SampleID!='HBSS')%>%filter(SampleID!='PC')%>%filter(SampleID!='H20')
    p<-ggplot(DATA, aes(x=Analysis_Results)) + 
      geom_bar(aes(color=Analysis_Results,fill=Analysis_Results))+
      geom_text(stat='count', aes(label=..count..),size=5,vjust=1)
    p
  })
  output$plot4<-renderPlot({
    inFile1_1 <- input$file1_1
    if (is.null(inFile1_1))
      return(NULL)
    data<-data_1()
    d4<-data%>%filter((!is.na(`Company Name`)))%>%filter((is.na(`Pool Name`)))%>%filter(str_detect(`Company Name`, 'Individual'))
    d5<-d4%>%separate(`Company Name`, c('SampleType', 'CompanyShort','PoolName','PoolBarcode'), sep="--")
    d6<-d5%>%group_by(CompanyShort,PoolName,Analysis_Results)%>%count()
    d6$CompanyShort<-do.call(paste, c(d6[c('CompanyShort','PoolName')], sep="-"))   
    p2<-ggplot(data=d6, aes(x=CompanyShort,y=n,fill=Analysis_Results,label=n)) +
      geom_bar(stat="identity")+
      geom_text(size = 3, position = position_stack(vjust = 0.5))+
      theme(axis.text.x = element_text(face = "bold", angle = -70))+
      ylab('Count')+
      xlab('CompanyShort-PoolName')+
      scale_fill_hue(c=45, l=80)
    if (is.null(inFile1_1)){
      return(NULL)
    }else{
      p2
    }
  })
  # data_DOH<-reactive({
  #   inFile_doh <- input$file_doh
  #   if (is.null(inFile_doh))
  #     return(NULL)
  #   if (input$state=='New York'){
  #     base<-as.data.frame(read_excel('www/BASE-NY.xlsx'))
  #     base2<-base[1,]
  #     base<-base[-c(1,2),]
  #     patient<-as.data.frame(read_excel(inFile_doh$datapath))
  #     NPI<-read_excel('www/NPI-Bi.xlsx')
  #     com<-left_join(patient,NPI,by='company_short')
  #     base$'10'<-as.Date(base$'10')
  #     base$'10'<-format(base$'10', "%m/%d/%Y")
  #     base$'61'<-as.Date(base$'61')
  #     base$'61'<-format(base$'61', "%m/%d/%Y")
  #     base$'64'<-as.Date(base$'64')
  #     base$'64'<-format(base$'64', "%m/%d/%Y")
  #     patient$`Date of Birth`<-format(patient$`Date of Birth`, "%m/%d/%Y")
  #     com$date_of_receipt<-format(com$date_of_receipt, "%m/%d/%Y")
  #     com$results_time<-format(com$results_time, "%m/%d/%Y")
  #     np<-nrow(patient)+1
  #     patient$`Middle Name`<-substring(patient$`Middle Name`, 1, 1)
  #     
  #     patient_info<-as.data.frame(patient%>%select(`Last Name`,`First Name`,`Middle Name`,`Date of Birth`,Race,`Ethnicity (Hispanic or Non-Hispanic)`,Sex,Address,City,State,Zip,`Phone Number`,sample_type))
  #     
  #     base[c(2:np),c(6,7,8,10,15:20,22,24,63)]<-patient_info
  #     customer_info<-as.data.frame(com%>%select(`Employer Name or School Dist. Name`,`Employer Street Address or School Name`,
  #                                               `Employer or School City`,`Employer or School State`,`Employer or School Zip Code`,
  #                                               `Employer or School Phone`,`Job title or Student`))
  #     base[c(2:np),c(27,30,31,32,34,35,38)]<-customer_info
  #     # head(base)
  #     test_info<-as.data.frame(com%>%select(tube_ids,date_of_receipt,results_time,results))
  #     base[c(2:np),c(59,61,64,75)]<-test_info
  #     base1<-base
  #     
  #     common_info<-c('33D2185028','EMP','33D2185028','Diagnostic Immunology Virology','F','Mirimus Lab','33D2185028','ECLRS','!!',
  #                    'Mirimus Clinical Labs','710 Parkside Avenue','Brooklyn','NY','11226','5164224079')
  #     base1[2,c(4,26,69,70,73,89,90,92,93,50:55)]<-common_info
  #     base1[c(2:np),c(4,26,69,70,73,89,90,92,93,50:55)]<-base1[2,c(4,26,69,70,73,89,90,92,93,50:55)]
  #     
  #     Saliva<-c('Saliva','94845-5','SARS-CoV-2 RNA [Presence] in Saliva (oral fluid) by NAA with probe detection')
  #     Swab<-c('Nasopharyngeal','94759-8','SARS-CoV-2 (COVID-19) RNA [Presence] in Nasopharynx by NAA with probe detection')
  #     #should based on sample type
  #     #base1[2,c(63,67,68)]<-Saliva
  #     for ( i in 2:nrow(base1)){
  #       if (base1[i,63]=="SWAB"){
  #         base1[i,c(63,67,68)]<-Swab 
  #       }else if(base1[i,63]=="SALIVA"){
  #         base1[i,c(63,67,68)]<-Saliva
  #       }
  #     }
  #     
  #     base1[1,]<-base2
  #     base1[2,1]<-'COV'
  #     Provider_info<-com%>%select(`Provider Last Name`,`Provider First name`,`Provider Middle Initial`,`Provider Name Suffix`,`Provider Street Address`,
  #                                 `Provider City`,`Provider State`,`Provider ZIP Code`,`Provider Phone Number`)
  #     
  #     base1[c(2:np),c(41:49)]<-Provider_info
  #     
  #     base1[c(2:np),3]<-gsub('/','',base1[c(2:np),10])
  #     data_new <- data.frame(lapply(base1,      # Convert data with to upper function
  #                                   function(variables) {
  #                                     if (is.character(variables)) {
  #                                       return(toupper(variables))
  #                                     } else {
  #                                       return(variables)
  #                                     }
  #                                   }),
  #                            stringsAsFactors = FALSE)
  #     
  #     data_new$X15<-data_new$X15%>%replace_na("U")
  #     data_new$X16<-data_new$X16%>%replace_na("U")
  #     data_new$X17<-data_new$X17%>%replace_na("U")
  #     data_new$X22<-data_new$X22%>%replace_na("0")
  #     #formatting sex
  #     # F = Female
  #     # H = Hermaphrodite / Undertermined
  #     # M = Male
  #     # O = Other
  #     # T = Transsexual
  #     # U = Unknown
  #     for ( i in 2:np){
  #       if (data_new[i,17]=='FEMALE'){
  #         data_new[i,17]<-'F'
  #       }else if(data_new[i,17]=='MALE'){
  #         data_new[i,17]<-'M'
  #       }else if(data_new[i,17]=='OTHER'){
  #         data_new[i,17]<-'U'
  #       }else if(data_new[i,17]=='TRANSSEXUAL'){
  #         data_new[i,17]<-'T'
  #       }else if(data_new[i,17]=='HERMAPHRODITE'){
  #         data_new[i,17]<-'H'
  #       }
  #     }
  #     #formatting race15
  #     # A = Asian or Pacific Islander
  #     # B = Black
  #     # I = Native American or Alaskan Native
  #     # M = Multiracial
  #     # O = Other
  #     # U = Unknown
  #     # W = White
  #     for ( i in 2:np){
  #       if (data_new[i,15]=='WHITE'){
  #         data_new[i,15]<-'W'
  #       }else if(data_new[i,15]=='BLACK'){
  #         data_new[i,15]<-'B'
  #       }else if(data_new[i,15]=='ASIAN OR PACIFIC ISLANDER'){
  #         data_new[i,15]<-'A'
  #       }else if(data_new[i,15]=='OTHER'){
  #         data_new[i,15]<-'O'
  #       }else if(data_new[i,15]=='MULTIRACIAL'){
  #         data_new[i,15]<-'M'
  #       }else if(data_new[i,15]=='NATIVE AMERICAN OR ALASKAN NATIVE'){
  #         data_new[i,15]<-'I'
  #       }
  #     }
  #     #formatting ethnicity 16
  #     for ( i in 2:np){
  #       if (data_new[i,16]=='HISPANIC'){
  #         data_new[i,16]<-'H'
  #       }else if(data_new[i,16]=='NON-HISPANIC'){
  #         data_new[i,16]<-'N'
  #       }
  #     }
  #     
  #     #
  #     base1<-data_new
  #     #add provider_info
  #     for (c in 2:np){
  #       if(base1[c,19]=='NY'){
  #         base1[c,19]<-'NEW YORK'
  #       }else if(base1[c,19]=='LA'){
  #         base1[c,19]<-'LOS ANGELES '
  #       }
  #     }
  #     base1<-base1%>%filter(!is.na(X6))
  #     base_output<-base1
  #     base_output[,91]<-format(Sys.time(),"%m/%d/%Y %H:%M")
  #     base_output<-replace(base_output, is.na(base_output),'')
  #     base_output<-base_output%>%filter(X75!='INVALID')
  #     base_output$'X24'<-str_replace(base_output$'X24', '([) ])', '-')
  #     base_output$'X24'<-str_replace(base_output$'X24', '([()])', '')
  #     base_output$'X24'<-str_replace(base_output$'X24', '([ ])', '')
  #     #base_output<-base_output%>%filter(X20=='NY')
  #     #colnames(base_output)<-base2
  #     for (i in 2:(np-1)){
  #       l=nchar(base_output[i,22])
  #       if(l==4){
  #         base_output[i,22]<-paste("0",base_output[i,22],sep = '')
  #       }
  #     }
  #     base_output
  #   }else if (input$state=='New Jersey'){
  #     base_NJ<-as.data.frame(read_excel('www/Base_NJ.xlsx'))
  #     colnames(base_NJ)
  #     NPI<-read_excel('www/NPI-Bi.xlsx')
  #     patient_NJ<-as.data.frame(read_excel(inFile_doh$datapath))
  #     #keep leading zero for zip
  #     patient_NJ$Zip<-as.character(patient_NJ$Zip)
  #     #patient_NJ<-patient_NJ%>%filter(State=='NJ')
  #     com_NJ<-left_join(patient_NJ,NPI,by='company_short')
  #     #str(com_NJ)
  #     
  #     #format date
  #     com_NJ$`Date of Birth`<-format(com_NJ$`Date of Birth`,"%Y/%m/%d")
  #     com_NJ$date_of_receipt<-format(com_NJ$date_of_receipt, "%Y/%m/%d")
  #     com_NJ$results_time<-format(com_NJ$results_time, "%Y/%m/%d")
  #     
  #     com_NJ$date_of_receipt<-gsub('/','',com_NJ$date_of_receipt)
  #     com_NJ$`Date of Birth`<-gsub('/','',com_NJ$`Date of Birth`)
  #     com_NJ$results_time<-gsub('/','',com_NJ$results_time)
  #     
  #     NJ_Patient_info<-as.data.frame(com_NJ%>%select(`Last Name`,`First Name`,`Date of Birth`,Sex,Race,`Ethnicity (Hispanic or Non-Hispanic)`,
  #                                                    Address,City,State,Zip,`Phone Number`))
  #     np<-nrow(com_NJ)
  #     base_NJ[c(1:np),c(1:7,9:12)]<-NJ_Patient_info
  #     #provider info
  #     NJ_Provider_info<-as.data.frame(com_NJ%>%select(`Provider Last Name`,`Provider First name`,`Provider Street Address`,
  #                                                     `Provider City`,`Provider State`,`Provider ZIP Code`,`Provider Phone Number`))
  #     base_NJ[c(1:np),c(28:30,32:35)]<-NJ_Provider_info
  #     
  #     
  #     common_info<-com_NJ%>%select(`Ordering Facility Name`,`Ordering Facility Street`,`Ordering Facility City`,`Ordering Facility State`,
  #                                  `Ordering Facility ZIP`,`Ordering Facility Phone Number`)
  #     base_NJ[c(1:np),c(21:22,24:27)]<-common_info
  #     base_NJ[c(1:np),c(17)]<-'RT-PCR'
  #     base_NJ[c(1:np),c(14)]<-'Mirimus Clinical Labs'
  #     base_NJ[c(1:np),c(15)]<-'33D2185028'
  #     #testing info
  #     test_info<-as.data.frame(com_NJ%>%select(tube_ids,date_of_receipt,results,sample_type))
  #     base_NJ[c(1:np),c(13,16,19,20)]<-test_info
  #     #sample type
  #     Saliva<-c('94845-5','Saliva')
  #     Swab<-c('94759-8','Nasopharyngeal')
  #     
  #     for ( i in 1:nrow(base_NJ)){
  #       if (base_NJ[i,20]=='SWAB'){
  #         base_NJ[i,c(18,20)]<-Swab
  #       }else if(base_NJ[i,20]=='SALIVA'){
  #         base_NJ[i,c(18,20)]<-Saliva
  #       }
  #     }
  #     
  #     
  #     data_NJ <- data.frame(lapply(base_NJ,      # Convert data with to upper function
  #                                  function(variables) {
  #                                    if (is.character(variables)) {
  #                                      return(toupper(variables))
  #                                    } else {
  #                                      return(variables)
  #                                    }
  #                                  }),
  #                           stringsAsFactors = FALSE)
  #     
  #     #PATIENT_SEX Acceptable Values
  #     #F-Female
  #     #M-Male
  #     #U-Unknown
  #     data_NJ$PATIENT_SEX<-data_NJ$PATIENT_SEX%>%replace_na("U")
  #     for ( i in 1:np){
  #       if (data_NJ[i,4]=='FEMALE'){
  #         data_NJ[i,4]<-'F'
  #       }else if(data_NJ[i,4]=='MALE'){
  #         data_NJ[i,4]<-'M'
  #       }else if(data_NJ[i,4]=='UNKNOWN'){
  #         data_NJ[i,4]<-'U'}
  #       
  #     }
  #     #PATIENT_RACE Acceptable Values
  #     #1002-5,American Indian or Alaska Native
  #     #2028-9,Asian
  #     #2054-5,Black or African American
  #     #2076-8,Native Hawaiian or Other Pacific Islander
  #     #2131-1,Other Race
  #     #2106-3,White
  #     data_NJ$PATIENT_RACE<-data_NJ$PATIENT_RACE%>%replace_na("2131-1")
  #     for ( i in 1:np){
  #       if (data_NJ[i,5]=='WHITE'){
  #         data_NJ[i,5]<-'2106-3'
  #       }else if(data_NJ[i,5]=='BLACK'){
  #         data_NJ[i,5]<-'2054-5'
  #       }else if(data_NJ[i,5]=='ASIAN OR PACIFIC ISLANDER'){
  #         data_NJ[i,5]<-'2028-9'
  #       }else if(data_NJ[i,5]=='OTHER'){
  #         data_NJ[i,5]<-'2131-1'
  #       }else if(data_NJ[i,5]=='NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER'){
  #         data_NJ[i,5]<-'2076-8'
  #       }else if(data_NJ[i,5]=='NATIVE AMERICAN OR ALASKAN NATIVE'){
  #         data_NJ[i,5]<-'1002-5'
  #       }
  #     }
  #     #PATIENT_ETHNICITY Acceptable Values
  #     #H,Hispanic or Latino
  #     #N,Not Hispanic or Latino
  #     #U,Unknown
  #     data_NJ$PATIENT_ETHNICITY<-data_NJ$PATIENT_ETHNICITY%>%replace_na("U")
  #     #formatting ethnicity 16
  #     for ( i in 1:np){
  #       if (data_NJ[i,6]=='HISPANIC'){
  #         data_NJ[i,6]<-'H'
  #       }else if(data_NJ[i,6]=='NON-HISPANIC'){
  #         data_NJ[i,6]<-'N'
  #       }
  #     }
  #     
  #     library(tidyverse)
  #     data_NJ$PATIENT_PHONE_10DIGIT<-str_replace(data_NJ$PATIENT_PHONE_10DIGIT, '([) ])', '-')
  #     data_NJ$PATIENT_PHONE_10DIGIT<-str_replace(data_NJ$PATIENT_PHONE_10DIGIT, '([()])', '')
  #     data_NJ$PATIENT_PHONE_10DIGIT<-str_replace(data_NJ$PATIENT_PHONE_10DIGIT, '([ ])', '')
  #     # remove NAs
  #     data_NJ<-replace(data_NJ,is.na(data_NJ),'')
  #     #export to csv file
  #     str(data_NJ)
  #     for (i in 1:np){
  #       l=nchar(data_NJ[i,11])
  #       if(l==4){
  #         data_NJ[i,11]<-paste("0",data_NJ[i,11],sep = '')
  #         print(data_NJ[i,11])
  #       }
  #     }
  #     data_NJ
  #   }
  #   
  # })
  # data_ins<-reactive({
  #   inFile3_Ins <- input$file3_Ins
  #   inFile4_Ins <- input$file4_Ins
  #   if (is.null(inFile4_Ins))
  #     return(NULL)
  #   invalid<-read.csv(inFile3_Ins$datapath,header = F,sep=",")
  #   invalid<-invalid%>%filter(V5!="")
  #   inval<-invalid$V5
  #   result_final<-read_excel(inFile4_Ins$datapath)
  #   result_final<-result_final%>%filter(!str_detect(pool_num, 'INSUFFICIENT'))
  #   Insuff2<-data.frame()
  #   for ( i in rownames(result_final)){
  #     a<-result_final[i,'tube_ids']
  #     a<-unlist(strsplit(as.character(a), ";"))
  #     for ( r in inval){
  #       if (r %in% a){
  #         b<-result_final[i,]
  #         b$tube_ids<-r
  #         b$pool_size<-1
  #         b$results<-'Invalid'
  #         b$pool_num<-paste(b$pool_num,'_INSUFFICIENT',sep = '')
  #         b$pool_id<-paste(b$pool_id,'-IS1',sep='')
  #         Insuff2<-rbind(Insuff2,b)
  #       }
  #     }
  #   }
  #   if (nrow(Insuff2)!=0){
  #     dup<-Insuff2%>%group_by(pool_id)%>%count()%>%filter(n!=1)
  #     dup_data<-Insuff2[duplicated(Insuff2$pool_id),]
  #     non_dup_data<-Insuff2[!duplicated(Insuff2$pool_id),]
  #     dt1<-data.frame()
  #     for ( i in dup$pool_id){
  #       old<-substr(i,1,13)
  #       num<-dup[dup$pool_id==i,'n']
  #       for (num1 in  c(2:num$n)){
  #         new<-paste(old,num1,sep = '')
  #         dup2<-cbind(dup[dup$pool_id==i,],new)
  #         dt1<-rbind(dt1,dup2)
  #       }
  #     }
  #     dt1<-dt1%>%unique()
  #     if (nrow(dt1)!=0){
  #       dt1 <-dt1%>%arrange(pool_id)
  #     }
  #     
  #     dup_data<-dup_data%>%arrange(pool_id)
  #     dup_data$pool_id<-dt1$...3
  #     insuff_final<-rbind(non_dup_data,dup_data)
  #     insuff_final
  #   }
  #   
  # })
  # filename_invalid<-reactive({
  #   inFile3_Ins <- input$file3_Ins
  #   if (is.null(inFile3_Ins)){
  #     filename_invalid<-NULL
  #   }else{
  #     filename_invalid<-substr(inFile3_Ins$name,1,nchar(inFile3_Ins$name)-4)
  #   }
  #   filename_invalid
  # })
  # output$contents2_Ins<- DT::renderDataTable({
  #   datatable(data = data_ins()
  #             ,extensions=c("Buttons",'Scroller')
  #             ,rownames=F
  #             , options = list(searchHighlight = TRUE
  #                              ,scrollY = 1000
  #                              ,scroller = TRUE
  #                              ,scrollX=TRUE
  #                              ,columnDefs=list(list(width='10%',targets="_all"))
  #                              ,dom = "Blfrtip"
  #                              ,autoWidth = TRUE
  #                              , buttons = 
  #                                list("copy", 'print',list(
  #                                  extend = "collection"
  #                                  , buttons = list(list(extend="csv"
  #                                                        , filename=paste(filename_invalid(),'INSUFFICIENT',sep = '-')),
  #                                                   list(extend="excel",title = NULL
  #                                                        ,customize=JS("function (xlsx) {
  #                                                                   var sheet = xlsx.xl.worksheets['sheet1.xml'];
  #                                                                   var col = $('col', sheet);
  #                                                                   col.each(function () {
  #                                                                     $(this).attr('width', 10);
  #                                                                   });
  #                                                                 }")
  #                                                        ,
  #                                                        filename=paste(filename_invalid(),'INSUFFICIENT',sep = '-')))
  #                                  , text = "Download"
  #                                ) ) # end of buttons customization
  #                              
  #                              # customize the length menu
  #                              , lengthMenu = list(c(50,100, -1) # declare values
  #                                                  , c(50,100, "All") # declare titles
  #                              ) # end of lengthMenu customization
  #                              , pageLength = -1
  #             ) # end of options
  #             
  #   )  
  # })
  # output$contents_D<- DT::renderDataTable({
  #   datatable(data = data_DOH()
  #             ,rownames=FALSE
  #             , options = list(searchHighlight = TRUE
  #                              ,scrollY = 1000
  #                              ,scroller = TRUE
  #                              ,scrollX=TRUE
  #                              ,columnDefs=list(list(width='10%',targets="_all"))
  #                              ,dom = "Blfrtip"
  #                              ,autoWidth = TRUE
  #                              , lengthMenu = list(c(50,100, -1) # declare values
  #                                                  , c(50,100, "All") # declare titles
  #                              ) # end of lengthMenu customization
  #                              , pageLength = -1
  #             ) # end of options
  #             
  #   )  
  # })
  # output$missing<-renderText({
  #   inFile3_Ins <- input$file3_Ins
  #   inFile4_Ins <- input$file4_Ins
  #   if (is.null(inFile4_Ins))
  #     return(NULL)
  #   invalid<-read.csv(inFile3_Ins$datapath,header = F,sep=",")
  #   invalid<-invalid%>%filter(V5!="")
  #   inval<-invalid$V5
  #   insuff_final<-data_ins()
  #   missing_tubes<-c()
  #   for (v in inval){
  #     if (!(v %in% insuff_final$tube_ids)){
  #       missing_tubes<-c(missing_tubes,v)
  #     }
  #   }
  #   if (length(missing_tubes)>0){
  #     missing_tubes<-paste("MISSING TUBE----", missing_tubes,sep = '\n')
  #   }else{
  #     missing_tubes<-"All TUBES FOUND IN THIS RUN!"
  #   }
  #   
  # })
  observeEvent(input$do,{ 
    if(input$Software=="CFX96"){
      updateRadioButtons(session, "Reaction",
                         selected = '1'
      )
      updateRadioButtons(session, "Plate",
                         selected = '96'
      )
      updateTextInput(session, "PC",
                      value = 'A12'
      )
      shinyalert('You selected CFX96',"so for this system, only single reaction, 96 well plate for SalivaDirect and SalivaClear are avaliable currently! ", type = "warning")}
    
  })
  observeEvent(input$do,{ 
    if(input$Software=="QuantStudio 7 Pro system"){
      updateRadioButtons(session, "Plate",
                         selected = '384'
      )
      shinyalert('You selected QuantStudio 7 Pro system',"so for this system, no 96 well plate for SalivaDirect and SalivaClear are avaliable yet! ", type = "warning")}
    
    
  })
  observeEvent(input$do,{ 
    if(input$Software=="Viia 7 system"){
      updateRadioButtons(session, "Plate",
                         selected = '384'
      )
      shinyalert('You selected Viia 7 Pro system',"so for this system, no 96 well plate for SalivaDirect and SalivaClear are avaliable yet! ", type = "warning")}
    
    
  })
  observeEvent(input$do,{ 
    if(input$Reaction==2){
      updateTextInput(session, "PC",
                      value = 'A22'
      )
    }
  })
  observeEvent(input$do,{ 
    if(input$Reaction==3){
      updateTextInput(session, "PC",
                      value = 'B23'
      )
    }
  })
  observeEvent(input$do,{ 
    if(input$Reaction==1){
      updateTextInput(session, "PC",
                      value = 'A12'
      )
    }
  })
  sel <- reactive({
    Parameters<-c('Software','Reaction','Method','Plate','PC','Startpos','Run#')
    Input<-c(input$Software,input$Reaction,input$Method,input$Plate,input$PC,input$startpos,input$run)
    final<-as.data.frame(cbind(Parameters,Input))
    final
  })
  data_temp<-reactive({
    final<-sel()
    if (final[2,2]==3){
      #triplicate can only upload up to 2 files
      #need an alert for users
      inFile_temp1 <- input$temp_file1
      if (is.null(inFile_temp1))
        return(NULL)
      inFile_temp2 <- input$temp_file2
      if (is.null(inFile_temp2)){
        poolbarcode1<-read.csv(inFile_temp1$datapath,header = FALSE)
        poolbarcode2<-rep(NA,each=48)
        barcodes<-c(poolbarcode1$V5,poolbarcode2)
      }else{
        poolbarcode1<-read.csv(inFile_temp1$datapath,header = FALSE)
        poolbarcode2<-read.csv(inFile_temp2$datapath,header = FALSE)
        barcodes<-c(poolbarcode1$V5,poolbarcode2$V5)
      }
      for (i in 1:3){
        if (barcodes[i]==''){
          barcodes[i]<-'BLANKS'
        }
      }
      start_pos<-as.numeric(input$startpos)
      w1<-c()
      Task<-'UNKNOWN'
      col_names<-c("Well","Well Position","Sample Name","Sample Color","Biogroup Name","Biogroup Color",
                   "Target Name", "Target Color", "Task","Reporter","Quencher", "Quantity","Comments" )
      #samples position
      for(j in seq(0,11,1)){
        pos<-start_pos+2*j
        if(pos<=23){
          well1<-paste(LETTERS[seq(1,16,2)],pos,sep='')
          well2<-paste(LETTERS[seq(1,16,2)],pos+1,sep='')
          well3<-paste(LETTERS[seq(2,16,2)],pos+1,sep='')
          well<-cbind(c(well1,well2,well3))
          w1<-c(w1,well)
        }
      }
      
      if (input$PC=='B23'){
        w1<-c(w1,'B23')
      }
      #find no sample(the rest) position-w2
      w1<-as.data.frame(w1)
      colnames(w1)<-c('Well Position')
      well_seq<-as.data.frame(cbind(paste(rep(LETTERS[1:16],each=24),rep(1:24,times=16),sep = ''),1:384))
      colnames(well_seq)<-c('Well Position','Well')
      rownames(well_seq)<-well_seq$`Well Position`
      w2<-anti_join(well_seq,w1)
      w2<-w2$`Well Position`
      
      #reverse 8 samples as a unit to load the samples to the templates
      
      Barcodes<-c()
      for (i in 1:12){
        j=as.numeric(8*i)
        s=as.numeric(8*(i-1)+1)
        m<-rep(rev(barcodes[s:j]),times=3)
        Barcodes<-c(Barcodes,m)
      }
      
      df2<-cbind(w1,Barcodes[1:length(w1$`Well Position`)])
      if (input$PC=='B23'){
        df2[nrow(df2),2]<-'PC'
      }
      if (final[3,2]=="SalivaClear"){
        df2<-df2[rep(seq_len(nrow(df2)), each = 5), ]
        Target<-c('MS2','N gene','S gene','ORF1ab','RP')
        Reportor<-c('JUN','VIC','ABY','FAM','CY5')
        
      }else if (final[3,2]=="SalivaDirect"){
        df2<-df2[rep(seq_len(nrow(df2)), each = 2), ]
        Target<-c('N1','RP')
        Reportor<-c('FAM','CY5')
      }else if (final[3,2]=="Eurofins"){
        df2<-df2[rep(seq_len(nrow(df2)), each = 3), ]
        Target<-c('N1','ORF10','RP')
        Reportor<-c('FAM','VIC','CY5')
      }
      Task<-'UNKNOWN'
      df2<-as.data.frame(cbind('',df2,'','','',Target,'',Task,Reportor,'','',''))
      df2_2<-as.data.frame(cbind('',w2,'','','','','','','','','','',''))
      colnames(df2_2)<-col_names
      colnames(df2)<-col_names
      df2<-replace(df2, is.na(df2),'')
      
      
      df3<-df2%>%filter(`Sample Name`=="")
      if (nrow(df3[,c(7,9,10)])>0){
        df3[,c(7,9,10)]<-""
      }
      
      df4<-df2%>%filter(`Sample Name`!="")
      df5<-rbind(df4,df3)
      pcr_run<-rbind(df5,df2_2)
      
      
      for (i in rownames(well_seq)) {
        for( j in 1:nrow(pcr_run)){
          if (pcr_run[j,2]==i){
            pcr_run[j,1]<-well_seq[i,2]
          }
        }
      }
      pcr_run$Well<-as.numeric(pcr_run$Well)
      pcr_run<-pcr_run[order(pcr_run$Well),]
      
      if (final[2,2]=='MIX'){
        pcr_run_t<-pcr_run%>%
          filter(!(str_starts(`Sample Name`,input$Pool)))%>%
          filter(!str_detect(`Well Position`,LETTERS[1]))%>%
          filter(!str_detect(`Well Position`,LETTERS[3]))%>%
          filter(!str_detect(`Well Position`,LETTERS[5]))%>%
          filter(!str_detect(`Well Position`,LETTERS[7]))%>%
          filter(!str_detect(`Well Position`,LETTERS[9]))%>%
          filter(!str_detect(`Well Position`,LETTERS[11]))%>%
          filter(!str_detect(`Well Position`,LETTERS[13]))%>%
          filter(!str_detect(`Well Position`,LETTERS[15]))
        
        pcr_run_t[,c(3,7,9,10)]<-''
        pcr_run_t<-pcr_run_t%>%unique()
        pcr_run_l1<-pcr_run%>%
          filter(str_starts(`Sample Name`,input$Pool))
        pcr_run_l2<-pcr_run%>%
          filter(!str_detect(`Well Position`,LETTERS[2]))%>%
          filter(!str_detect(`Well Position`,LETTERS[4]))%>%
          filter(!str_detect(`Well Position`,LETTERS[6]))%>%
          filter(!str_detect(`Well Position`,LETTERS[8]))%>%
          filter(!str_detect(`Well Position`,LETTERS[10]))%>%
          filter(!str_detect(`Well Position`,LETTERS[12]))%>%
          filter(!str_detect(`Well Position`,LETTERS[14]))%>%
          filter(!str_detect(`Well Position`,LETTERS[16]))
        
        
        pcr_run_new<-rbind(pcr_run_l1,pcr_run_t,pcr_run_l2)
        pcr_run_new$Well<-as.numeric(pcr_run_new$Well)
        pcr_run_new<-pcr_run_new[order(pcr_run_new$Well),]
        pcr_run<-pcr_run_new%>%unique()
      }
      if (final[1,2]=="Viia 7 system"){
        h<-as.data.frame(rbind('* Block Type = 384-Well Block','* Chemistry = OTHER','*Experiment Barcode = NA','* Experiment File Name = Template1.edt' ,
                               '* Experiment File Name = Template ','* Experiment Run End Time = Not Started', '* Experiment Type = Standard Curve',
                               '* Experiment User Name = NA','* Instrument Type = ViiA 7','* Passive Reference =  ','* Quantification Cycle Method = Ct',
                               '* Signal Smoothing On = true','* Stage/ Cycle where Analysis is performed = Stage 2, Step 2',
                               '[Sample Setup]',col_names))
        h[-c(15),-c(1)]<-''
        colnames(h)<-col_names
        pcr_run1<-rbind(h,pcr_run)
      }else if(final[1,2]=="QuantStudio 7 Pro system"){
        t<-paste('* Date Created =',(Sys.time()-14000),sep = "")
        
        h<-as.data.frame(rbind('* Block Type = 384-Well Block',t, 
                               '* Passive Reference =','', '[Sample Setup]  ',col_names))
        h[-c(6),-c(1)]<-''
        colnames(h)<-col_names
        pcr_run1<-rbind(h,pcr_run)
      }
    }
    else if(final[2,2]==2){
      #duplicate can upload up to 4 files
      inFile_temp1 <- input$temp_file1
      if (is.null(inFile_temp1)){
        return(NULL)
      }else{
        inFile_temp2 <- input$temp_file2
        if (is.null(inFile_temp2)){
          poolbarcode1<-read.csv(inFile_temp1$datapath,header = FALSE)
          poolbarcode2<--rep(NA,each=48)
          barcodes1<-c(poolbarcode1$V5,poolbarcode2)
        }else{
          poolbarcode1<-read.csv(inFile_temp1$datapath,header = FALSE)
          poolbarcode2<-read.csv(inFile_temp2$datapath,header = FALSE)
          barcodes1<-c(poolbarcode1$V5,poolbarcode2$V5)
        }
      }
      inFile_temp3 <- input$temp_file3
      inFile_temp4 <- input$temp_file4
      if (is.null(inFile_temp3)&is.null(inFile_temp4)){
        barcodes2<-rep(NA,each=96)
      }else{
        if (is.null(inFile_temp4)){
          poolbarcode1<-read.csv(inFile_temp3$datapath,header = FALSE)
          poolbarcode2<-rep(NA,each=48)
          barcodes2<-c(poolbarcode1$V5,poolbarcode2)
        }else{
          poolbarcode1<-read.csv(inFile_temp3$datapath,header = FALSE)
          poolbarcode2<-read.csv(inFile_temp4$datapath,header = FALSE)
          barcodes2<-c(poolbarcode1$V5,poolbarcode2$V5)
        }
      }
      start_pos<-1
      #start_pos<-as.numeric(input$startpos)
      Task<-'UNKNOWN'
      col_names<-c("Well","Well Position","Sample Name","Sample Color","Biogroup Name","Biogroup Color",
                   "Target Name", "Target Color", "Task","Reporter","Quencher", "Quantity","Comments" )
      
      w1<-c()
      for(j in seq(0,11,1)){
        pos<-start_pos+2*j
        if(pos<=23){
          well1<-paste(LETTERS[seq(2,16,2)],pos,sep='')
          w1<-c(w1,well1)
        }
      }
      
      w2<-c()
      
      #samples position
      for(j in seq(0,11,1)){
        pos<-start_pos+2*j
        if(pos<=23){
          well2<-paste(LETTERS[seq(2,16,2)],pos+1,sep='')
          w2<-c(w2,well2)
        }
      }
      #w3,w4 position for Poolrun2 duplicates samples
      w3<-c()
      for(j in seq(0,11,1)){
        pos<-start_pos+2*j
        if(pos<=23){
          well1<-paste(LETTERS[seq(1,16,2)],pos,sep='')
          w3<-c(w3,well1)
        }
      }
      w4<-c()
      for(j in seq(0,11,1)){
        pos<-start_pos+2*j
        if(pos<=23){
          well1<-paste(LETTERS[seq(1,16,2)],pos+1,sep='')
          w4<-c(w4,well1)
        }
      }
      #make them to be data.frame, and name the columns
      w1<-as.data.frame(w1)
      w2<-as.data.frame(w2)
      w3<-as.data.frame(w3)
      w4<-as.data.frame(w4)
      
      colnames(w1)<-c('Well Position')
      colnames(w2)<-c('Well Position')
      colnames(w3)<-c('Well Position')
      colnames(w4)<-c('Well Position')
      # construct a small dataframe for well Position and assign them a specific well number 
      well_seq<-as.data.frame(cbind(paste(rep(LETTERS[1:16],each=24),rep(1:24,times=16),sep = ''),1:384))
      colnames(well_seq)<-c('Well Position','Well')
      rownames(well_seq)<-well_seq$`Well Position`
      
      
      # w<-rbind(w1,w2,w3,w4)
      # w_rest<-anti_join(well_seq,w)
      # w_rest<-w_rest$`Well Position`
      # df_rest<-as.data.frame(cbind('',w_rest,'','','','','','','','','','',''))
      # colnames(df_rest)<-col_names
      # df_rest$`Sample Name`<-'Used Wells'
      #reverse 8 samples as a unit to load the samples to the templates
      
      barcodes1[is.na(barcodes1)]<-'EMPTY'
      barcodes1[barcodes1==""]<-'EMPTY'
      Barcodes1<-c()
      for (i in 1:12){
        j=as.numeric(8*i)
        s=as.numeric(8*(i-1)+1)
        m<-rev(barcodes1[s:j])
        Barcodes1<-c(Barcodes1,m)
      }
      barcodes2[barcodes2==""]<-'EMPTY'
      barcodes2[is.na(barcodes2)]<-'EMPTY'
      Barcodes2<-c()
      for (i in 1:12){
        j=as.numeric(8*i)
        s=as.numeric(8*(i-1)+1)
        m<-rev(barcodes2[s:j])
        Barcodes2<-c(Barcodes2,m)
      }
      
      df1<-cbind(w1,Barcodes1[1:length(w1$`Well Position`)])
      d1<-cbind(w2,Barcodes1[1:length(w1$`Well Position`)])
      df2<-cbind(w3,Barcodes2[1:length(w1$`Well Position`)])
      d2<-cbind(w4,Barcodes2[1:length(w1$`Well Position`)])
      if (input$PC=='A22'){
        d2[81,2]<-'PC'
        df2[81,2]<-'H2O'
      }
      df1<-rbind(df1,d1)
      df2<-rbind(df2,d2)
      if (final[3,2]=="SalivaClear"){
        df1<-df1[rep(seq_len(nrow(df1)), each = 5), ]
        df2<-df2[rep(seq_len(nrow(df2)), each = 5), ]
        Target<-c('MS2','N gene','S gene','ORF1ab','RP')
        Reportor<-c('JUN','VIC','ABY','FAM','CY5')
        
      }else if (final[3,2]=="SalivaDirect"){
        df1<-df1[rep(seq_len(nrow(df1)), each = 2), ]
        df2<-df2[rep(seq_len(nrow(df2)), each = 2), ]
        Target<-c('N1','RP')
        Reportor<-c('FAM','CY5')
      }else if (final[3,2]=="Eurofins"){
        df1<-df1[rep(seq_len(nrow(df1)), each = 3), ]
        df2<-df2[rep(seq_len(nrow(df2)), each = 3), ]
        Target<-c('N1','ORF10','RP')
        Reportor<-c('FAM','VIC','CY5')
        
      }
      
      df1<-as.data.frame(cbind('',df1,'','','',Target,'',Task,Reportor,'','',''))
      df2<-as.data.frame(cbind('',df2,'','','',Target,'',Task,Reportor,'','',''))
      
      colnames(df1)<-col_names
      colnames(df2)<-col_names
      
      df1<-replace(df1, is.na(df1),'')
      df2<-replace(df2, is.na(df2),'')
      
      df1_1<-df1%>%filter(`Sample Name`=="")
      if (nrow(df1_1[,c(7,9,10)])>0){
        df1_1[,c(7,9,10)]<-""
      }
      
      df2_1<-df2%>%filter(`Sample Name`=="")
      if (nrow(df2_1[,c(7,9,10)])>0){
        df2_1[,c(7,9,10)]<-""
      }
      
      df1<-df1%>%filter(`Sample Name`!="")
      df2<-df2%>%filter(`Sample Name`!="")
      
      pcr_run<-rbind(df1,df2,df1_1,df2_1)
      
      
      for (i in rownames(well_seq)) {
        for( j in 1:nrow(pcr_run)){
          if (pcr_run[j,2]==i){
            pcr_run[j,1]<-well_seq[i,2]
          }
        }
      }
      
      pcr_run$Well<-as.numeric(pcr_run$Well)
      pcr_run<-pcr_run[order(pcr_run$Well),]
      #add required information for templates, date,block type, passive reference, and sample setup
      if (final[1,2]=="Viia 7 system"){
        h<-as.data.frame(rbind('* Block Type = 384-Well Block','* Chemistry = OTHER','*Experiment Barcode = NA','* Experiment File Name = Template1.edt' ,
                               '* Experiment File Name = Template ','* Experiment Run End Time = Not Started', '* Experiment Type = Standard Curve',
                               '* Experiment User Name = NA','* Instrument Type = ViiA 7','* Passive Reference =  ','* Quantification Cycle Method = Ct',
                               '* Signal Smoothing On = true','* Stage/ Cycle where Analysis is performed = Stage 2, Step 2',
                               '[Sample Setup]',col_names))
        h[-c(15),-c(1)]<-''
        colnames(h)<-col_names
        pcr_run4<-rbind(h,pcr_run)
      }else if(final[1,2]=="QuantStudio 7 Pro system"){
        t<-paste('* Date Created =',(Sys.time()-14000),sep = "")
        
        h<-as.data.frame(rbind('* Block Type = 384-Well Block',t, 
                               '* Passive Reference =','', '[Sample Setup]  ',col_names))
        h[-c(6),-c(1)]<-''
        colnames(h)<-col_names
        pcr_run4<-rbind(h,pcr_run)
      }
      
      blanks<-c('P1','P2','N1','N2','L1','L2',"O3","O4","O5","O6","O7","O8")
      pcr_run1<-pcr_run4%>% 
        mutate(`Sample Name`= replace(`Sample Name`, `Well Position` %in% blanks&`Sample Name`=='EMPTY', 'BLANKS'))
      
    }
    else if(final[2,2]=='MIX'){
      #triplicate can only upload up to 2 files
      #need an alert for users
      inFile_temp1 <- input$temp_file1
      if (is.null(inFile_temp1))
        return(NULL)
      inFile_temp2 <- input$temp_file2
      if (is.null(inFile_temp2)){
        poolbarcode1<-read.csv(inFile_temp1$datapath,header = FALSE)
        poolbarcode2<-rep(NA,each=48)
        barcodes<-c(poolbarcode1$V5,poolbarcode2)
      }else{
        poolbarcode1<-read.csv(inFile_temp1$datapath,header = FALSE)
        poolbarcode2<-read.csv(inFile_temp2$datapath,header = FALSE)
        barcodes<-c(poolbarcode1$V5,poolbarcode2$V5)
      }
      for (i in 1:3){
        if (barcodes[i]==''){
          barcodes[i]<-'BLANKS'
        }
      }
      start_pos<-as.numeric(input$startpos)
      w1<-c()
      Task<-'UNKNOWN'
      col_names<-c("Well","Well Position","Sample Name","Sample Color","Biogroup Name","Biogroup Color",
                   "Target Name", "Target Color", "Task","Reporter","Quencher", "Quantity","Comments" )
      #samples position
      for(j in seq(0,11,1)){
        pos<-start_pos+2*j
        if(pos<=23){
          well1<-paste(LETTERS[seq(1,16,2)],pos,sep='')
          well2<-paste(LETTERS[seq(1,16,2)],pos+1,sep='')
          well3<-paste(LETTERS[seq(2,16,2)],pos+1,sep='')
          well<-cbind(c(well1,well2,well3))
          w1<-c(w1,well)
        }
      }
      
      if (input$PC=='B23'){
        w1<-c(w1,'B23')
      }
      #find no sample(the rest) position-w2
      w1<-as.data.frame(w1)
      colnames(w1)<-c('Well Position')
      well_seq<-as.data.frame(cbind(paste(rep(LETTERS[1:16],each=24),rep(1:24,times=16),sep = ''),1:384))
      colnames(well_seq)<-c('Well Position','Well')
      rownames(well_seq)<-well_seq$`Well Position`
      w2<-anti_join(well_seq,w1)
      w2<-w2$`Well Position`
      
      #reverse 8 samples as a unit to load the samples to the templates
      
      Barcodes<-c()
      for (i in 1:12){
        j=as.numeric(8*i)
        s=as.numeric(8*(i-1)+1)
        m<-rep(rev(barcodes[s:j]),times=3)
        Barcodes<-c(Barcodes,m)
      }
      
      df2<-cbind(w1,Barcodes[1:length(w1$`Well Position`)])
      if (input$PC=='B23'){
        df2[nrow(df2),2]<-'PC'
      }
      if (final[3,2]=="SalivaClear"){
        df2<-df2[rep(seq_len(nrow(df2)), each = 5), ]
        Target<-c('MS2','N gene','S gene','ORF1ab','RP')
        Reportor<-c('JUN','VIC','ABY','FAM','CY5')
        
      }else if (final[3,2]=="SalivaDirect"){
        df2<-df2[rep(seq_len(nrow(df2)), each = 2), ]
        Target<-c('N1','RP')
        Reportor<-c('FAM','CY5')
      }else if (final[3,2]=="Eurofins"){
        df2<-df2[rep(seq_len(nrow(df2)), each = 3), ]
        Target<-c('N1','ORF10','RP')
        Reportor<-c('FAM','VIC','CY5')
      }
      Task<-'UNKNOWN'
      df2<-as.data.frame(cbind('',df2,'','','',Target,'',Task,Reportor,'','',''))
      df2_2<-as.data.frame(cbind('',w2,'','','','','','','','','','',''))
      colnames(df2_2)<-col_names
      colnames(df2)<-col_names
      df2<-replace(df2, is.na(df2),'')
      
      
      df3<-df2%>%filter(`Sample Name`=="")
      if (nrow(df3[,c(7,9,10)])>0){
        df3[,c(7,9,10)]<-""
      }
      
      df4<-df2%>%filter(`Sample Name`!="")
      df5<-rbind(df4,df3)
      pcr_run<-rbind(df5,df2_2)
      
      
      for (i in rownames(well_seq)) {
        for( j in 1:nrow(pcr_run)){
          if (pcr_run[j,2]==i){
            pcr_run[j,1]<-well_seq[i,2]
          }
        }
      }
      pcr_run$Well<-as.numeric(pcr_run$Well)
      pcr_run<-pcr_run[order(pcr_run$Well),]
      pcr_run_t<-pcr_run%>%
        filter(!(str_starts(`Sample Name`,input$Pool)))%>%
        filter(!str_detect(`Well Position`,LETTERS[1]))%>%
        filter(!str_detect(`Well Position`,LETTERS[3]))%>%
        filter(!str_detect(`Well Position`,LETTERS[5]))%>%
        filter(!str_detect(`Well Position`,LETTERS[7]))%>%
        filter(!str_detect(`Well Position`,LETTERS[9]))%>%
        filter(!str_detect(`Well Position`,LETTERS[11]))%>%
        filter(!str_detect(`Well Position`,LETTERS[13]))%>%
        filter(!str_detect(`Well Position`,LETTERS[15]))
      
      pcr_run_t[,c(3,7,9,10)]<-''
      pcr_run_t<-pcr_run_t%>%unique()
      pcr_run_l1<-pcr_run%>%
        filter(str_starts(`Sample Name`,input$Pool))
      pcr_run_l2<-pcr_run%>%
        filter(!str_detect(`Well Position`,LETTERS[2]))%>%
        filter(!str_detect(`Well Position`,LETTERS[4]))%>%
        filter(!str_detect(`Well Position`,LETTERS[6]))%>%
        filter(!str_detect(`Well Position`,LETTERS[8]))%>%
        filter(!str_detect(`Well Position`,LETTERS[10]))%>%
        filter(!str_detect(`Well Position`,LETTERS[12]))%>%
        filter(!str_detect(`Well Position`,LETTERS[14]))%>%
        filter(!str_detect(`Well Position`,LETTERS[16]))
      
      
      pcr_run_new<-rbind(pcr_run_l1,pcr_run_t,pcr_run_l2)
      pcr_run_new$Well<-as.numeric(pcr_run_new$Well)
      pcr_run_new<-pcr_run_new[order(pcr_run_new$Well),]
      pcr_run<-pcr_run_new%>%unique()
      
      
      if (final[1,2]=="Viia 7 system"){
        h<-as.data.frame(rbind('* Block Type = 384-Well Block','* Chemistry = OTHER','*Experiment Barcode = NA','* Experiment File Name = Template1.edt' ,
                               '* Experiment File Name = Template ','* Experiment Run End Time = Not Started', '* Experiment Type = Standard Curve',
                               '* Experiment User Name = NA','* Instrument Type = ViiA 7','* Passive Reference =  ','* Quantification Cycle Method = Ct',
                               '* Signal Smoothing On = true','* Stage/ Cycle where Analysis is performed = Stage 2, Step 2',
                               '[Sample Setup]',col_names))
        h[-c(15),-c(1)]<-''
        colnames(h)<-col_names
        pcr_run1<-rbind(h,pcr_run)
      }else if(final[1,2]=="QuantStudio 7 Pro system"){
        t<-paste('* Date Created =',(Sys.time()-14000),sep = "")
        
        h<-as.data.frame(rbind('* Block Type = 384-Well Block',t, 
                               '* Passive Reference =','', '[Sample Setup]  ',col_names))
        h[-c(6),-c(1)]<-''
        colnames(h)<-col_names
        pcr_run1<-rbind(h,pcr_run)
      }
    }
    else if(final[1,2]=='CFX96'){
      final[2,2]<-1
      final[4,2]<-96
      
      #print('CFX96 can only has 96 well single reaction plate set up')
      start_pos<-1
      # find tripliacte samples positions-w1
      col_names<-c("Well","Ch1 Dye","Ch2 Dye","Ch3 Dye","Ch4 Dye","Ch5 Dye","FRET","Sample Type","Sample Name","Ch1 Target Name","Ch2 Target Name","Ch3 Target Name","Ch4 Target Name","Ch5 Target Name","FRET Target Name",
                   "Biological Set Name","Replicate","Ch1 Quantity","Ch2 Quantity","Ch3 Quantity","Ch4 Quantity","Ch5 Quantity","FRET Quantity","Well Note","Ch1 Well Color","Ch2 Well Color","Ch3 Well Color","Ch4 Well Color","Ch5 Well Color","FRET Well Color" )
      inFile_temp1 <- input$temp_file1
      if (is.null(inFile_temp1))
        return(NULL)
      inFile_temp2 <- input$temp_file2
      if (is.null(inFile_temp2)){
        poolbarcode1<-read.csv(inFile_temp1$datapath,header = FALSE)
        poolbarcode2<-rep(NA,each=48)
        barcodes<-c(poolbarcode1$V5,poolbarcode2)
      }else{
        poolbarcode1<-read.csv(inFile_temp1$datapath,header = FALSE)
        poolbarcode2<-read.csv(inFile_temp2$datapath,header = FALSE)
        barcodes<-c(poolbarcode1$V5,poolbarcode2$V5)
      }
      w1<-c()
      #samples position
      for(j in seq(0,11,1)){
        pos<-sprintf("%02d", start_pos+1*j)
        if(pos<=12){
          well1<-paste(LETTERS[seq(1,8,1)],pos,sep='')
          w1<-c(w1,well1)
        }
      }
      
      #reverse 8 samples as a unit to load the samples to the templates
      
      Barcodes<-c()
      for (i in 1:12){
        j=as.numeric(8*i)
        s=as.numeric(8*(i-1)+1)
        m<-rep(rev(barcodes[s:j]),times=1)
        Barcodes<-c(Barcodes,m)
      }
      #up to 5 channels for 96 well plate.
      #setup the channels for CFX
      if (final[3,2]=="SalivaClear"){
        ch1_target<-'MS2'
        ch1_dye<-'JUN'
        ch2_target<-'N gene'
        ch2_dye<-'VIC'
        ch3_target<-'S gene'
        ch3_dye<-'ABY'
        ch4_target<-'ORF1ab'
        ch4_dye<-'FAM'
        ch5_target<-'RP'
        ch5_dye<-'CY5'
        Sampletype<-'UNKNOWN' 
        df<-as.data.frame(cbind(w1,ch1_dye,ch2_dye,ch3_dye,ch4_dye,ch5_dye,'',Sampletype,Barcodes,ch1_target,ch2_target,ch3_target,ch4_target,ch5_target,'','','','','','','','','','','','','','','',''))
      }else if (final[3,2]=="SalivaDirect"){
        ch1_dye<-'FAM'
        ch2_dye<-'CY5'
        ch1_target<-'N1'
        ch2_target<-'RP'
        Sampletype<-'UNKNOWN'
        df<-as.data.frame(cbind(w1,ch1_dye,ch2_dye,'','','','',Sampletype,Barcodes,ch1_target,ch2_target,'','','','','','','','','','','','','','','','','','',''))
      }
      colnames(df)<-col_names
      #rownames(df)<-as.character(df[,1])
      if ( input$PC=='A12'){
        df[89,9]<-'PC'
      }
      #
      #df['B12',9]<-'NC'
      
      
      
      #add required information for templates, plate header
      Data<-c('','Data','1','96','BR White','All Channels','copy number','','','','','','','')
      
      plate<-as.data.frame(rbind('Plate Header','Filed','Version','Plate Size','Plate Type','Scan Mode',
                                 'Units','Run ID','Run Notes','Run Protocol','Data File','TBD','','Plate Data',col_names))
      # append header to template
      plate[-c(15),-c(1)]<-''
      plate[1:14,2]<-Data
      colnames(plate)<-col_names
      pcr_run1<-as.data.frame(rbind(plate,df))
      
      #write.table(pcr_run, 'CFX-text3.csv',sep=",",row.names = FALSE, col.names=FALSE)
    }
    pcr_run1
  })
  #template
  observeEvent(input$do, {
    output$contents4 <- DT::renderDataTable(DT::datatable(sel()))
  })
  
  observeEvent(input$do, {
    output$contents5 <- DT::renderDataTable(
      datatable(data = data_temp()
                ,rownames=FALSE
                #, colnames = rep("", ncol(data_temp()))
                , extensions = 'Buttons'
                , options = list(searchHighlight = TRUE
                                 ,scrollY = 1000
                                 ,scroller = TRUE
                                 ,scrollX=TRUE
                                 ,columnDefs=list(list(width='10%',targets="_all"))
                                 ,dom = "t"
                                 ,autoWidth = TRUE
                                 # , buttons = list(list(extend = "collection"
                                 #                       ,exportOptions = list(header = "")
                                 #                       , buttons = list(extend="csv"
                                 #                                        , filename=paste(gsub("-", "", Sys.Date()),'-',input$Software,'-',input$Method,'-',input$Reaction,"x Reactions-Run",input$run,sep = ''))
                                 #                       , text = "Download csv"
                                 # ) ) # end of buttons customization
                                 
                                 # customize the length menu
                                 , lengthMenu = list(c(50,100, -1) # declare values
                                                     , c(50,100, "All") # declare titles
                                 ) # end of lengthMenu customization
                                 , pageLength = -1
                ) # end of options
                
      ) )
  })
  observeEvent(input$do, {
    output$download <- downloadHandler(
      filename = function() {
        paste0(paste(gsub("-", "", Sys.Date()),'-',input$Software,'-',input$Method,'-',input$Reaction,"x Reactions-Run",input$run,sep = ''),".txt")
      },
      content = function(file) {
        final<-sel()
        write.table(data_temp(),file,sep="\t",quote = FALSE,col.names=TRUE,row.names = FALSE)
      }
    )
  })
  
  # output$download_doh <- downloadHandler(
  #   filename = function() {
  #     paste0(paste('MirimusClinicalLabs_',gsub("-", "", Sys.Date()),'_',input$state,sep = ''),".tsv")
  #   },
  #   content = function(file) {
  #     write.table(data_DOH()[-1,],file,sep = "|",row.names = FALSE,col.names = FALSE,quote = FALSE)
  #   }
  # )
  # output$download_doh2 <- downloadHandler(
  #   filename = function() {
  #     paste0(paste('MirimusClinicalLabs_',gsub("-", "", Sys.Date()),'_',input$state,sep = ''),".csv")
  #   },
  #   content = function(file) {
  #     write.csv(data_DOH(),file,row.names = FALSE)
  #   }
  # )
  # data_pcr<-reactive({
  #   #drive_auth(email = "bi@mirimus.com")
  #   #gs4_auth(email = "bi@mirimus.com")
  #   pcr_data<-read_sheet("https://docs.google.com/spreadsheets/d/1z7l4UxYdZSlhyaLLj-8uouT0oudWxsg_lv96cR7E-AI/edit#gid=0")
  #   
  # })
  output$contents_pcr1<-DT::renderDataTable({
    datatable(data = data_pcr()
              ,rownames=FALSE
              , options = list(searchHighlight = TRUE
                               ,scrollY = 1000
                               ,scroller = TRUE
                               ,scrollX=TRUE
                               ,columnDefs=list(list(width='10%',targets="_all"))
                               ,dom = "Blfrtip"
                               ,autoWidth = TRUE
                               , buttons = 
                                 list("copy", 'print',list(
                                   extend = "collection"
                                   , buttons = list(list(extend="csv"
                                                         , filename="RT-qPCR results data"),
                                                    list(extend="excel",title = NULL
                                                         ,customize=JS("function (xlsx) {
                                                                    var sheet = xlsx.xl.worksheets['sheet1.xml'];
                                                                    var col = $('col', sheet);
                                                                    col.each(function () {
                                                                      $(this).attr('width', 10);
                                                                    });
                                                                  }")
                                                         ,
                                                         filename="RT-qPCR results data"))
                                   , text = "Download"
                                 ) ) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list(c(50,100, -1) # declare values
                                                   , c(50,100, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = -1
              )
              
    )
  })
  output$contents_pcr2<-DT::renderDataTable({
    inFile_pcr <- input$file_pcr
    filename_pcr<-inFile_pcr$name
    ext <- tools::file_ext(inFile_pcr$datapath)
    validate(need(ext == "xlsx", "Please upload a xlsx file, column name of 'SampleID' must be in the your list!!"))
    if (is.null(inFile_pcr))
      return(NULL)
    data_pcr1<-data_pcr()
    data_input<-read_excel(inFile_pcr$datapath)
    data_merge<-left_join(data_input,data_pcr1,by="SampleID")
    datatable(data = data_merge
              ,extensions=c("Buttons",'Scroller')
              ,rownames=F
              , options = list(searchHighlight = TRUE
                               ,scrollY = 1000
                               ,scroller = TRUE
                               ,scrollX=TRUE
                               ,columnDefs=list(list(width='10%',targets="_all"))
                               ,dom = "Blfrtip"
                               ,autoWidth = TRUE
                               , buttons = 
                                 list("copy", 'print',list(
                                   extend = "collection"
                                   , buttons = list(list(extend="csv"
                                                         , filename=paste(filename_pcr,'CT values',sep = '-')),
                                                    list(extend="excel",title = NULL
                                                         ,customize=JS("function (xlsx) {
                                                                    var sheet = xlsx.xl.worksheets['sheet1.xml'];
                                                                    var col = $('col', sheet);
                                                                    col.each(function () {
                                                                      $(this).attr('width', 10);
                                                                    });
                                                                  }")
                                                         ,
                                                         filename=paste(filename_pcr,'CT values',sep = '-')))
                                   , text = "Download"
                                 ) ) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list(c(50,100, -1) # declare values
                                                   , c(50,100, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = -1
              ) # end of options
              
    )  
  })
}

shinyApp(ui, server)

