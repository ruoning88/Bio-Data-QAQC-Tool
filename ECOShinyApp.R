library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(devtools)
library(readxl)
library(shinyalert)
library(writexl)
library(ggiraph)
library(plotly)
library(lubridate)
library(shinycssloaders)
library(xts)

options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize=40*1024^2) 

qualif <- c("Out of Bound"="OutofBound","Exceed Max Rate of Change"="ExceedChange")
## The User Interface:
ui<- fluidPage(
  h1(id="big-heading", "Biological Data QAQC Module"),
  
## css - Custom Styles
  tags$style(HTML("#big-heading{color: #f49f02;font-size: 60px;
           font-weight: bold;font-style: italic;text-decoration: underline overline;
                  font-family:georgia; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;}")),
  
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #b8cef2;}
    .tabbable > .nav > li > a[data-value='Qualify'] {background-color: #f79a88;}
    .tabbable > .nav > li > a[data-value='world'] {background-color: #fce99f;}
    .tabbable > .nav > li[class=active]    > a {background-color: #f2da91;}
  ")),
  

tabsetPanel (
  
## Tab 1: Data Qualifier
    tabPanel(h4(id="tg","Qualify"),
             tags$style(HTML("#tg{color: #f49f02;font-size: 30px;
           font-weight: bold;font-style: italic;font-family:georgia;
           text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;")),
             
             fluid=TRUE, 
             setBackgroundImage("https://i.ibb.co/PmfJ02C/lakebouy.jpg"),
             titlePanel(title=" "),
             
             tags$head(tags$style(
               HTML('
                    #sidebar {
                    background-color: #f2da91;
                    }
                    
                    body, label, input, button, select { 
                    font-family: "georgia";
                    color: #ce6808;
                    }')
  )),
  
             sidebarLayout(
               sidebarPanel(id="sidebar",
                            
                 tags$head(tags$style(".progress-bar{background-color:#f4b642;}")),           
                 fileInput('file1', 'Input your Data',
                           accept = c(".csv")),
                 fileInput('file2', 'Input the Rules',
                           accept = c(".xlsx")),
                 selectInput("dtst", "Which Dataset ? ", choices=c("None"), selected="None",multiple = FALSE),
                 selectInput("grphst", "Which Time Interval ? ", choices=c("All Time","Noon","MidNight"), multiple = FALSE),
                 selectInput("colnm", "Select columns to display",choices = "ALL", multiple = FALSE,selected="ALL"),
                 selectInput("ytdsp", "Select Year to Display", choices="None", multiple = FALSE),
                 selectInput("prblm", "Select Means of Qualification", choices=qualif, multiple = FALSE),
                 
                 downloadButton('download1',"Download Qualified Data"),
                 
                 actionButton("button", "Qualify Data and Show Graph"),
                 
                 
                 useShinyalert(), 
                 tags$head(
                   tags$style(HTML("
                        #Dialog1{
                        display:block;
                        height: 45px;
                        width: 45px;
                        border-radius: 47%;
                        border: 2px solid #efae21;
                        }
                        "))
                 ),
                 actionButton("Dialog1", "?", style='padding:4px; font-size:170%;
                            color:yellow; background-color:#f49f02; margin-left: 320px;'
                 )
               ),
               
               mainPanel(

                 withSpinner(uiOutput("plot"),color="#f7b336")
                 
               )
             )),


## Tab 2: Data Ovrview -- Monthly and Yearly Data   
   tabPanel(
     h4(id="tg2","Overview"),
     tags$style(HTML("#tg2{color: #f49f02;font-size: 30px;
                     font-weight: bold;font-style: italic;font-family:georgia;
                     text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;")),
     fluid=TRUE, 
     setBackgroundImage("https://i.ibb.co/PmfJ02C/lakebouy.jpg"),
     titlePanel(title=" "),
     
     tags$head(tags$style(
       HTML('
            #sidebarov {
            background-color: #f2da91;
            }
            
            body, label, input, button, select { 
            font-family: "georgia";
            color: #ce6808;
            }')
  )),
  sidebarLayout(
    sidebarPanel(id="sidebarov",
                 tags$head(tags$style(".progress-bar{background-color:#f4b642;}")),           
                 fileInput('file3', 'Input your Data',
                           accept = c(".csv")),
                 selectInput("Intv", "Which Time Interval? ", choices=c("Yearly","Monthly"), selected="Yearly",multiple = FALSE),
                 selectInput("varnm", "Select columns to display",choices = "ALL", multiple = FALSE,selected="ALL"),
                 actionButton("ovbutton", "Show Data Overview Graph"),
                 
                 useShinyalert(), 
                 tags$head(
                   tags$style(HTML("
                                   #Dialog2{
                                   display:block;
                                   height: 45px;
                                   width: 45px;
                                   border-radius: 47%;
                                   border: 2px solid #efae21;
                                   }
                                   "))
                   ),
                 actionButton("Dialog2", "?", style='padding:4px; font-size:170%;
                              color:yellow; background-color:#f49f02; margin-left: 320px;'
                 )
    ),
    
    mainPanel(
      withSpinner(plotlyOutput(outputId="zplot",height="500px",width="100%"),color="#f7b336")
    )
  )
  
    )
  )
)




## The Server: 
server <- function (input,output,session) {

## Tab 1:
## Read in Dataset: First Column must be Time/Date 
   data <- reactive ({
    if(is.null(input$file1)) return ("You Have Not Import Data Yet")
    inFile <- input$file1
    platform <- read.csv(inFile$datapath,header = FALSE)
    header <- platform[2,-1]
    header2 <- unlist(header,use.names=F)
    platform <- platform[-c(1,2,3),-1]
    colnames(platform) <- header2
    platform
  })
 
## Populate the variable selector
  observeEvent(input$file1,{
    updateSelectInput(session,
                      inputId = "colnm",
                      choices = colnames(data())[2:length(colnames(data()))])
   
  }
  )
  
## Populate Available Years in the dataset 
  observeEvent(input$colnm,
  if (input$colnm != "ALL") {
  dt <- data()
  time <- dt[,c(1,which(colnames(dt)==input$colnm))]
  colnames(time) <- c("Time","Var")
  time$Time <- as.POSIXct(time$Time,format="%m/%d/%y %H:%M",tz=Sys.timezone())
  time$Time <- as.character(time$Time)
  time$Year <- substring(time$Time ,1,4)
  time$Time <- as.POSIXct(time$Time )
  time$Year <- as.factor(time$Year)
  time$Var <- as.numeric(as.character(time$Var))
  time <- time[-which(is.na(time$Time)),]
  
  updateSelectInput(session,
                    inputId = "ytdsp",
                    choices = as.numeric(as.character(time$Year)))
  }
  )
  
  
## Data Cleaning 
  cleandata <- reactive ({
    if (input$colnm != "ALL") {
    dt <- data()
    time <- dt[,c(1,which(colnames(dt)==input$colnm))]
    colnames(time) <- c("Time","Var")
    time$Time <- as.POSIXct(time$Time,format="%m/%d/%y %H:%M",tz=Sys.timezone())
    time$Time <- as.character(time$Time)
    time$Year <- substring(time$Time ,1,4)
    time$Time <- as.POSIXct(time$Time )
    time$Year <- as.factor(time$Year)
    time$Var <- as.numeric(as.character(time$Var))
    time <- time[-which(is.na(time$Time)),]
    time
    }
  }) 
 
  
## Read in the Rules dataset
rulesint <- reactive ({
    if(is.null(input$file2)) return ("You Have Not Import Data Yet")
    inFile2 <- input$file2
    rules <- read_excel(inFile2$datapath,range="A1:E49")
    rules
  })


rules <- reactive({
  rules <- rulesint()
  rulessp <- rules[which(rules$Variable==input$colnm & rules$Source==input$dtst),]
  rulessp <- rulessp %>% select (-Source)
  rulessp
})


## Populate the Dataset Selector
  observeEvent(input$file2,
    {updateSelectInput(session,
                      inputId = "dtst",
                      choices = unique(rulesint()$Source))
    }
  )  
  
  
##  Data Qaulification Step
plotdata <- reactive ({
    if (input$grphst=="All Time") {
    batterytime <- cleandata()
    rulessp <- rules()
    batterytimesp <- batterytime[which(batterytime$Year==input$ytdsp),]
    bt <- batterytimesp[which(batterytimesp$Year==input$ytdsp),]
    ma <- stats::filter(bt[,2],sides=1,filter=rep(1/3,3))
    ma <- c(ma[2:length(ma)],NA)
    bt$MA <- ma
    
    min <- as.numeric(rulessp$ValidMin)
    max <- as.numeric(rulessp$ValidMax)
    rate <- as.numeric(rulessp$MaxChangeRate)
    
    bt$OutofBound <- "Qualified"
    bt$OutofBound[which(bt[,2]<min)] <- "Out of Lower Bound"
    bt$OutofBound[which(bt[,2]>max)] <- "Out of Upper Bound"
    bt$OutofBound <- as.factor(bt$OutofBound)
    bt$OutofBound <- relevel(bt$OutofBound, ref="Qualified")
    
    bt$Diff <- c(NA,diff(bt[,2]))
    bt$Diff <- abs(bt$Diff)
    bt$ExceedChange <- "Qualified"
    bt$ExceedChange[which(bt$Diff>rate)] <- "Exceed Max Rate of Change"
    bt$ExceedChange <- as.factor(bt$ExceedChange)
    bt$ExceedChange <- relevel(bt$ExceedChange, ref="Qualified")
    
    is.na.rle <- rle(is.na(bt[, 2]))
    is.na.rle$values <- is.na.rle$values & is.na.rle$lengths >= 5
    bt$ConsecutiveNA <- "Qualified"
    bt$ConsecutiveNA[inverse.rle(is.na.rle)] <- "Over 5 NA in a Row"
    
    for (i in 1:nrow(bt)) {
      key <- as.character(bt$ExceedChange[i])
      if(key=="Exceed Max Rate of Change"){
        bt$ExceedChange[i-1] <- "Exceed Max Rate of Change"
      }
    }
    
    bt
    } else if (input$grphst=="Noon") {
      noon <- cleandata()
      rulessp <- rules()
      noon <- noon[which(noon$Year==input$ytdsp),]
      noon$datetime <-  substring(as.character(noon$Time), 12)
      noon$datetime <- hms(noon$datetime)
      noon <- noon[which(noon$datetime >="11H 30M 0S" & noon$datetime <="12H 30M 0S"),]
      noon$date <- substring(as.character(noon$Time), 1,10)
      noongroup <- noon %>% group_by(date) %>% summarize(Var=round(mean(Var,na.rm=TRUE),2),Year=Year[1])
      noongroup$Var[is.nan(noongroup$Var)] <- NA
      ma_noon <- stats::filter(noongroup[,2],sides=1,filter=rep(1/3,3))
      ma_noon <- c(ma_noon[2:length(ma_noon)],NA)
      noongroup$MA <- ma_noon
      noongroup <- as.data.frame(noongroup)
      
      min <- as.numeric(rulessp$ValidMin)
      max <- as.numeric(rulessp$ValidMax)
      rate <- as.numeric(rulessp$MaxChangeRate)
      
      noongroup$OutofBound <- "Qualified"
      noongroup$OutofBound[which(noongroup[,2]<min)] <- "Out of Lower Bound"
      noongroup$OutofBound[which(noongroup[,2]>max)] <- "Out of Upper Bound"
      noongroup$OutofBound <- as.factor(noongroup$OutofBound)
      noongroup$OutofBound <- relevel(noongroup$OutofBound, ref="Qualified")
      
      noongroup$Diff <- c(NA,diff(noongroup$Var))
      noongroup$Diff <- abs(noongroup$Diff)
      noongroup$ExceedChange <- "Qualified"
      noongroup$ExceedChange[which(noongroup$Diff>rate)] <- "Exceed Max Rate of Change"
      noongroup$ExceedChange <- as.factor(noongroup$ExceedChange)
      noongroup$ExceedChange <- relevel(noongroup$ExceedChange, ref="Qualified")
      
      is.na.rle2 <- rle(is.na(noongroup$Var))
      is.na.rle2$values <- is.na.rle2$values & is.na.rle2$lengths >= 3
      noongroup$ConsecutiveNA <- "Qualified"
      noongroup$ConsecutiveNA[inverse.rle(is.na.rle2)] <- "Over 3 NA in a Row"
      noongroup$date <-as.Date(noongroup$date)
      
      for (i in 1:nrow(noongroup)) {
        key <- as.character(noongroup$ExceedChange[i])
        if(key=="Exceed Max Rate of Change"){
          noongroup$ExceedChange[i-1] <- "Exceed Max Rate of Change"
        }
      }
      
      noongroup
    } else if (input$grphst=="MidNight") {
      night <- cleandata()
      rulessp <- rules()
      night <- night[which(night$Year==input$ytdsp),]
      night$datetime <-  substring(as.character(night$Time), 12)
      night$datetime <- hms(night$datetime)
      night <- night[which(night$datetime >="23H 30M 0S" | night$datetime <="0H 30M 0S"),]
      night$date <- substring(as.character(night$Time), 1,10)
      
      nightgroup <- night %>% group_by(date) %>% summarize(Var=round(mean(Var,na.rm=TRUE),2),Year=Year[1])
      nightgroup$Var[is.nan(nightgroup$Var)] <- NA
      ma_night <- stats::filter(nightgroup[,2],sides=1,filter=rep(1/3,3))
      ma_night <- c(ma_night[2:length(ma_night)],NA)
      nightgroup$MA <- ma_night
      nightgroup <- as.data.frame(nightgroup)
      
      min <- as.numeric(rulessp$ValidMin)
      max <- as.numeric(rulessp$ValidMax)
      rate <- as.numeric(rulessp$MaxChangeRate)
      
      nightgroup$OutofBound <- "Qualified"
      nightgroup$OutofBound[which(nightgroup[,2]<min)] <- "Out of Lower Bound"
      nightgroup$OutofBound[which(nightgroup[,2]>max)] <- "Out of Upper Bound"
      nightgroup$OutofBound <- as.factor(nightgroup$OutofBound)
      nightgroup$OutofBound <- relevel(nightgroup$OutofBound, ref="Qualified")
      
      nightgroup$Diff <- c(NA,diff(nightgroup$Var))
      nightgroup$Diff <- abs(nightgroup$Diff)
      nightgroup$ExceedChange <- "Qualified"
      nightgroup$ExceedChange[which(nightgroup$Diff>rate)] <- "Exceed Max Rate of Change"
      nightgroup$ExceedChange <- as.factor(nightgroup$ExceedChange)
      nightgroup$ExceedChange <- relevel(nightgroup$ExceedChange, ref="Qualified")
      
      is.na.rle2 <- rle(is.na(nightgroup$Var))
      is.na.rle2$values <- is.na.rle2$values & is.na.rle2$lengths >= 3
      nightgroup$ConsecutiveNA <- "Qualified"
      nightgroup$ConsecutiveNA[inverse.rle(is.na.rle2)] <- "Over 3 NA in a Row"
      nightgroup$date <-as.Date(nightgroup$date)
      
      for (i in 1:nrow(nightgroup)) {
        key <- as.character(nightgroup$ExceedChange[i])
        if(key=="Exceed Max Rate of Change"){
          nightgroup$ExceedChange[i-1] <- "Exceed Max Rate of Change"
        }
      }
      
      nightgroup
    }
  }) 


## Update the Plot Output Format: If all datapoints are qualified, use the Plotly Module. 
#If not all dataset are qualified (i.e: Some data values are out of the upper bound), then use the ggiraph Module.
observeEvent(input$button, {
output$plot<- renderUI({
  plotdata <- plotdata()
  levs <- as.numeric(length(unique(plotdata[,which(colnames(plotdata)==input$prblm)])))
    if (levs==1) {
      plotlyOutput(outputId="rplot",height="500px",width="100%")
    } else if (levs>1) {
      ggiraphOutput(outputId="qplot",height="500px",width="100%")
    } 
  })   
})


## Generate the Qualification Plots (Tab 1) -- Plotly Module
observeEvent(input$button, {
  output$rplot <- renderPlotly({
    if (!is.null(input$file2)) {
      if(input$grphst=="All Time") {
        bt <- plotdata()
        ind <- which(bt$ConsecutiveNA == "Over 5 NA in a Row") 
        
        pq <- ggplot() + geom_line(aes_string(x='Time',y='Var', color= input$prblm,group=1),data=bt)+
          scale_color_manual(values=c("grey75",'red',"blue")) + xlab("Date") +ylab(input$colnm)+ theme_bw()+
          geom_rug(data=bt[which(bt$ConsecutiveNA != "Qualified" ),c(1,2)],aes(x=Time),color='orange')+
          ggtitle("Lake Data Qualification Plot -- All Time") +
          theme(axis.text.x = element_text(size=10), axis.title=element_text(size=15,face="bold"),
                panel.grid.minor = element_blank(), legend.position = "top",
                plot.title = element_text(size=10, hjust=.5))
        
        if(length(ind) > 0) {
          indmin <- ind[[1]]
          indmax <- ind[[length(ind)]]
          pq_b <- pq + geom_smooth(aes(x=Time,y=MA),data=bt[1:indmin+1,],color="blue",method="loess",span=0.08,se=FALSE)+
            geom_smooth(aes(x=Time,y=MA),data=bt[indmax:nrow(bt),],color="blue",method="loess",span=0.08,se=FALSE)
          
          bt_na <- c(rep("Yes",nrow(bt)))
          t <- list(
            family = "sans serif",
            size = 11,
            color = 'darkgreen')
          
          pltygr <- ggplotly(pq_b) %>% layout(
            title="Lake Data Qualification Plot",
            legend = list(orientation = "h",xanchor = "center", x = 0.5,y=-0.1),
            font=t)  %>%
            style(hovertext = paste("Time:",bt$Time,
                                    "<br> Value:", bt$Var,
                                    "<br> Out of Bound ?: ", bt$OutofBound,
                                    "<br> Exceed Max Change Rate?:", bt$ExceedChange),
                  textposition = "auto",traces=1) %>%
            style(hoverinfo = "none", traces = 3:10) %>%
            style(hovertext = paste("Time:",bt$Time,
                                    "<br> Consecutive NA ?:", bt_na),
                  textposition = "auto",traces=2) 
          
          
        }  else {
          pq_b <- pq + geom_smooth(aes(x=Time,y=MA),data=bt,color="blue",method="loess",span=0.08,se=FALSE)
          
          t <- list(
            family = "sans serif",
            size = 11,
            color = 'darkgreen')
          
          pltygr <- ggplotly(pq_b) %>% layout(
            title="Lake Data Qualification Plot",
            legend = list(orientation = "h",xanchor = "center", x = 0.5,y=-0.1),
            font=t)  %>%
            style(hovertext = paste("Time:",bt$Time,
                                    "<br> Value:", bt$Var,
                                    "<br> Out of Bound ?: ", bt$OutofBound,
                                    "<br> Exceed Max Change Rate?:", bt$ExceedChange),
                  textposition = "auto",traces=1) %>%
            style(hoverinfo = "none", traces = 2:10)
          
        }
        
      }  else if (input$grphst=="Noon") {
      noon <- plotdata()
      indnoon <- which(noon$ConsecutiveNA == "Over 3 NA in a Row") 
        
       noonplt <- ggplot() + geom_line(aes_string(x='date',y='Var', color= input$prblm, group=1),data=noon)+
          scale_color_manual(values=c("grey75",'red',"blue")) + xlab("Date") +ylab(paste("Average Noon (12pm)",input$colnm))+ theme_bw()+
          geom_rug(data=noon[which(noon$ConsecutiveNA != "Qualified" ),c(1,2)],aes(x=date),color='orange')+
          ggtitle("Lake Data Qualification Plot --- Daily Average Noon Data") +
          theme(axis.text.x = element_text(size=10), axis.title=element_text(size=15,face="bold"),
                panel.grid.minor = element_blank(), legend.position = "top",
                plot.title = element_text(size=10, hjust=.5))
        
       if(length(indnoon) > 0) {
         indmin_n <- indnoon[[1]]
         indmax_n <- indnoon[[length(indnoon)]]
        
         noon_na <- c(rep("Yes",nrow(noon)))
         t <- list(
           family = "sans serif",
           size = 11,
           color = 'darkgreen')
         
         pltygr <- ggplotly(noonplt) %>% layout(
           title="Lake Data Qualification Plot --- Daily Average Noon Data",
           legend = list(orientation = "h",xanchor = "center", x = 0.5,y=-0.1),
           font=t)  %>%
           style(hovertext = paste("Date:",noon$date,
                                   "<br> Value:", noon$Var,
                                   "<br> Out of Bound ?: ", noon$OutofBound,
                                   "<br> Exceed Max Change Rate?:", noon$ExceedChange),
                 textposition = "auto",traces=1) %>%
           style(hoverinfo = "none", traces = 3:10) %>%
           style(hovertext = paste("Date:",noon$date,
                                   "<br> Consecutive NA ?:", noon_na),
                 textposition = "auto",traces=2) 
       }  else {
         
         t <- list(
           family = "sans serif",
           size = 11,
           color = 'darkgreen')
         
         pltygr <- ggplotly(noonplt) %>% layout(
           title="Lake Data Qualification Plot --- Daily Average Noon Data",
           legend = list(orientation = "h",xanchor = "center", x = 0.5,y=-0.1),
           font=t)  %>%
           style(hovertext = paste("Date:",noon$date,
                                   "<br> Value:", noon$Var,
                                   "<br> Out of Bound ?: ", noon$OutofBound,
                                   "<br> Exceed Max Change Rate?:", noon$ExceedChange),
                 textposition = "auto",traces=1) %>%
           style(hoverinfo = "none", traces = 2:10)
       }
       
        
      }   else if (input$grphst=="MidNight") {
        night <- plotdata()
        indnight <- which(night$ConsecutiveNA == "Over 3 NA in a Row") 
        
        nightplt <- ggplot() + geom_line(aes_string(x='date',y='Var', color= input$prblm, group=1),data=night)+
          scale_color_manual(values=c("grey75",'red',"blue")) + xlab("Date") +ylab(paste("Average Midnight (0am)",input$colnm))+ theme_bw()+
          geom_rug(data=night[which(night$ConsecutiveNA != "Qualified" ),c(1,2)],aes(x=date),color='orange')+
          ggtitle("Lake Data Qualification Plot --- Daily Average MidNight Data") +
          theme(axis.text.x = element_text(size=10), axis.title=element_text(size=15,face="bold"),
                panel.grid.minor = element_blank(), legend.position = "top",
                plot.title = element_text(size=10, hjust=.5))
        
        if(length(indnight) > 0) {
          indmin_ng <- indnight[[1]]
          indmax_ng <- indnight[[length(indnight)]]
          
          night_na <- c(rep("Yes",nrow(night)))
          t <- list(
            family = "sans serif",
            size = 11,
            color = 'darkgreen')
          
          pltygr <- ggplotly(nightplt) %>% layout(
            title="Lake Data Qualification Plot --- Daily Average MidNight Data",
            legend = list(orientation = "h",xanchor = "center", x = 0.5,y=-0.1),
            font=t)  %>%
            style(hovertext = paste("Date:",night$date,
                                    "<br> Value:", night$Var,
                                    "<br> Out of Bound ?: ", night$OutofBound,
                                    "<br> Exceed Max Change Rate?:", night$ExceedChange),
                  textposition = "auto",traces=1) %>%
            style(hoverinfo = "none", traces = 3:10) %>%
            style(hovertext = paste("Date:",night$date,
                                    "<br> Consecutive NA ?:", night_na),
                  textposition = "auto",traces=2) 
        }  else {
          
          t <- list(
            family = "sans serif",
            size = 11,
            color = 'darkgreen')
          
          pltygr <- ggplotly(nightplt) %>% layout(
            title="Lake Data Qualification Plot --- Daily Average MidNight Data",
            legend = list(orientation = "h",xanchor = "center", x = 0.5,y=-0.1),
            font=t)  %>%
            style(hovertext = paste("Date:",night$date,
                                    "<br> Value:", night$Var,
                                    "<br> Out of Bound ?: ", night$OutofBound,
                                    "<br> Exceed Max Change Rate?:", night$ExceedChange),
                  textposition = "auto",traces=1) %>%
            style(hoverinfo = "none", traces = 2:10)
        }
        
        
      }
      
      pltygr
    }   
  })
})


## Generate the Qualification Plots (Tab 1) -- ggirpah Module
observeEvent(input$button, {
  output$qplot <- renderggiraph({
    if (!is.null(input$file2)) {
    if(input$grphst=="All Time") {
      bt <- plotdata()
      ind <- which(bt$ConsecutiveNA == "Over 5 NA in a Row") 
      
      pq <- ggplot() + geom_line(aes_string(x='Time',y='Var', color= input$prblm,group=1),data=bt)+
        geom_point_interactive(aes(x=Time,y=Var,tooltip=paste("Time:",bt$Time,
                                                             "<br> Value:", bt$Var,
                                                             "<br> Out of Bound ?: ", bt$OutofBound,
                                                             "<br> Exceed Max Change Rate?:", bt$ExceedChange),color=get(input$prblm)),size=0.1,data=bt)+
        scale_color_manual(values=c("grey75",'red',"blue")) + xlab("Date") +ylab(input$colnm)+ theme_bw()+
        geom_rug(data=bt[which(bt$ConsecutiveNA != "Qualified" ),c(1,2)],aes(x=Time),color='orange')+
        ggtitle("Lake Data Qualification Plot -- All Time") +
        theme(axis.text.x = element_text(size=7), axis.title=element_text(size=15,face="bold"),
              panel.grid.minor = element_blank(), legend.position = "top",
              plot.title = element_text(size=10, hjust=.5))

      
      if(length(ind) > 0) {
        indmin <- ind[[1]]
        indmax <- ind[[length(ind)]]
        finalplt <- pq + geom_smooth(aes(x=Time,y=MA),data=bt[1:indmin+1,],color="blue",method="loess",span=0.08,se=FALSE)+
          geom_smooth(aes(x=Time,y=MA),data=bt[indmax:nrow(bt),],color="blue",method="loess",span=0.08,se=FALSE)
        
      }  else {
        finalplt <- pq + geom_smooth(aes(x=Time,y=MA),data=bt,color="blue",method="loess",span=0.08,se=FALSE)
        
      }
      
    }  else if (input$grphst=="Noon") {
      noon <- plotdata()
      indnoon <- which(noon$ConsecutiveNA == "Over 3 NA in a Row") 
      
      finalplt <- ggplot() + geom_line(aes_string(x='date',y='Var', color= input$prblm, group=1),data=noon)+
        geom_point_interactive(aes(x=date,y=Var,tooltip=paste("Date:",noon$date,
                                                             "<br> Value:", noon$Var,
                                                             "<br> Out of Bound ?: ", noon$OutofBound,
                                                             "<br> Exceed Max Change Rate?:", noon$ExceedChange),color=get(input$prblm)),size=0.1,data=noon)+
        scale_color_manual(values=c("grey75",'red',"blue")) + xlab("Date") +ylab(paste("Average Noon (12pm)",input$colnm))+ theme_bw()+
        geom_rug(data=noon[which(noon$ConsecutiveNA != "Qualified" ),c(1,2)],aes(x=date),color='orange')+
        ggtitle("Lake Data Qualification Plot --- Daily Average Noon Data") +
        theme(axis.text.x = element_text(size=10), axis.title=element_text(size=15,face="bold"),
              panel.grid.minor = element_blank(), legend.position = "top",
              plot.title = element_text(size=10, hjust=.5))
      
    } else if (input$grphst=="MidNight") {
      night <- plotdata()
      indnight<- which(night$ConsecutiveNA == "Over 3 NA in a Row") 
      
      finalplt <- ggplot() + geom_line(aes_string(x='date',y='Var', color= input$prblm, group=1),data=night)+
        geom_point_interactive(aes(x=date,y=Var,tooltip=paste("Date:",night$date,
                                                              "<br> Value:", night$Var,
                                                              "<br> Out of Bound ?: ", night$OutofBound,
                                                              "<br> Exceed Max Change Rate?:", night$ExceedChange),color=get(input$prblm)),size=0.1,data=night)+
        scale_color_manual(values=c("grey75",'red',"blue")) + xlab("Date") +ylab(paste("Average MidNight (0am)",input$colnm))+ theme_bw()+
        geom_rug(data=night[which(night$ConsecutiveNA != "Qualified" ),c(1,2)],aes(x=date),color='orange')+
        ggtitle("Lake Data Qualification Plot --- Daily Average MidNight Data") +
        theme(axis.text.x = element_text(size=10), axis.title=element_text(size=15,face="bold"),
              panel.grid.minor = element_blank(), legend.position = "top",
              plot.title = element_text(size=10, hjust=.5))
      
    }
      irgr <- girafe(code=print(finalplt))
      girafe_options(irgr,opts_tooltip(use_fill = TRUE),opts_zoom(max=5))
    }   
})
})

## Downloaded Qualified Data
downdata <- reactive({
  pltdata <- plotdata()
  pltdata <- pltdata %>% select(-c(Year,MA,Diff))
  colnames(pltdata)[[2]] <- input$colnm
  pltdata
})

output$download1 <- downloadHandler(
    filename = function() {
      paste("QualifiedData-",input$colnm, Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(downdata(), file=file)
    }
  )

## Info Button
observeEvent(input$Dialog1, {
  shinyalert("Information Regarding this Tab", "This Tab Qualifies the dataset  \nbased on the criterion provided:
               \n There are some points to pay attention to when using the app.
               \n•The first column of the dataset(.csv) has to be in date time format (month/day/Year Hour:Minutes)
               \n•The first row has to be the source of the dataset (i.e. If your dataset records lake statistics about lake Lacawac, then the first row should all be filled in with Lake Lacawac')
               \n•The second row of the dataset has to be the varaiable names (i.e. Temerature, PH, etc.)
               \n•The Third row of the dataset has to be the unit of the variable recorded (i.e. Seconds, Celcius, etc.)
               \n•You can simply leave it blank if you do not know the units.
               \n•You Should use the Excel Template (.xlsx) to input the QAQC Rules.
               \n•You can input anything you like in the Source Column of the Rules Template.
               \n•This Tab contains two different Graphic Modules (Plotly & ggiraph). When all datapoints are qualified, the plotly module will be used. When some datapoints are not qualified (i.e. Out of Bound), then the ggiraph module will be used.
               \n•No matter which module is used, the graphics should be interactive. When you click on the datapoints,hover texts will display."
               , type = "info",closeOnClickOutside = TRUE)
})
  
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#  
## Tab 2: Overview
## Read in Dataset: First Column must be Time/Date 
dataov <- reactive ({
  if(is.null(input$file3)) return ("You Have Not Import Data Yet")
  inFile <- input$file3
  platform <- read.csv(inFile$datapath,header = FALSE)
  header <- platform[2,-1]
  header2 <- unlist(header,use.names=F)
  platform <- platform[-c(1,2,3),-1]
  colnames(platform) <- header2
  platform
})

## Populate the variable selector
observeEvent(input$file3,{
  updateSelectInput(session,
                    inputId = "varnm",
                    choices = colnames(dataov())[2:length(colnames(dataov()))])
  
}
)


## Data Cleaning
cleandataov <- reactive ({
  if (input$varnm != "ALL") {
    dt <- dataov()
    time <- dt[,c(1,which(colnames(dt)==input$varnm))]
    colnames(time) <- c("Time","Var")
    time$Time <- as.POSIXct(time$Time,format="%m/%d/%y %H:%M",tz=Sys.timezone())
    time$Time <- as.character(time$Time)
    time$Year <- substring(time$Time ,1,4)
    time$Time <- as.POSIXct(time$Time )
    time$Year <- as.factor(time$Year)
    time$Var <- as.numeric(as.character(time$Var))
    time <- time[-which(is.na(time$Time)),]
    
    time<- time[-which(is.na(time$Var)),]
    for (i in 1:nrow(time)){
      value <- time$Var[i]
      value <- abs(value/1000)
      if(value>=1) {
        time$Var[i] <- NA
      }
    }
    time
  }
}) 


yeardata <- reactive ({
  data <- cleandataov()
  yrgrp <- data %>% group_by(Year) %>% summarize(Var=mean(Var,na.rm=TRUE))
  yrgrp
})

monthdata <- reactive ({
  data <- cleandataov()
  data<- data[which(!is.na(data$Var)),]
  data$Time <- as.Date(data$Time)
  stdate <- as.character(data$Time[1])
  t<- nrow(data)
  eddate <- as.character(data$Time[t])
  ms <- xts(x=data[,-3],order.by=data$Time)
  ms <- merge(ms,zoo(NULL,seq(ymd(stdate),
                              ymd(eddate),by="days")),all=TRUE)
  ms$Var[which(is.na(ms$Var))] <-0
  ep <- endpoints(ms, on = "months")
  ms <- period.apply(ms[,"Var"],INDEX=ep,mean)
  cr<- coredata(ms)
  index <- index(ms)
  ms <- data.frame(date=index,Var=cr)
  ms$Var[which(ms$Var==0)] <- NA
  ms
})
 

## Generate Overview Plots (Tab 2) -- Plotly Module
observeEvent(input$ovbutton, {
  output$zplot <- renderPlotly({
    if (!is.null(input$file3)) {
      if(input$Intv=="Yearly") {
        yeardt <- yeardata()
       
        pq <- ggplot() + geom_line(aes_string(x='Year',y='Var',group=1),color="blue",data=yeardt)+
          xlab("Year") +ylab(input$varnm)+ theme_bw()+
          ggtitle(paste("Data Overview Plot -- Yearly",input$varnm,sep=" ")) +
          theme(axis.text.x = element_text(size=10,angle=90), axis.title=element_text(size=15,face="bold"),
                panel.grid.minor = element_blank(), legend.position = "top",
                plot.title = element_text(size=10, hjust=.5))
        
          t <- list(
            family = "sans serif",
            size = 11,
            color = 'darkgreen')
          
          pltygr <- ggplotly(pq) %>% layout(
            title=paste("Data Overview Plot -- Yearly",input$varnm,sep=" "),
            legend = list(orientation = "h",xanchor = "center", x = 0.5,y=-0.1),
            font=t)  %>%
            style(hovertext = paste("Time:",yeardt$Year,
                                    "<br> Average:", yeardt$Var),
                  textposition = "auto",traces=1) 
          
          
         
        
      }  else if (input$Intv=="Monthly") {
        monthdt <- monthdata()
        
        pq <- ggplot() + geom_line(aes_string(x='date',y='Var'),color="blue",data=monthdt)+
          xlab("Months") +ylab(input$varnm)+ theme_bw()+
          scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y")+
          ggtitle(paste("Data Overview Plot -- Monthly",input$varnm,sep=" ")) +
          theme(axis.text.x = element_text(size=10,angle=90), axis.title=element_text(size=15,face="bold"),
                panel.grid.minor = element_blank(), legend.position = "top",
                plot.title = element_text(size=10, hjust=.5))
        
        t <- list(
          family = "sans serif",
          size = 11,
          color = 'darkgreen')
        
        pltygr <- ggplotly(pq) %>% layout(
          title=paste("Data Overview Plot -- Monthly",input$varnm,sep=" "),
          legend = list(orientation = "h",xanchor = "center", x = 0.5,y=-0.1),
          font=t)  %>%
          style(hovertext = paste("Time:",monthdt$date,
                                  "<br> Average:", monthdt$Var),
                textposition = "auto",traces=1) 
        
      }  
      
      pltygr
    }   
  })
})

## Info Button
observeEvent(input$Dialog2, {
  shinyalert("Information Regarding this Tab", "This Tab Summarizes the datase provided:
               \n•Users may choose between Yearly Summary and Monthly Summary.
               \n•When using yearly summary, the graph will display the average value of the variable over all years available.
               \n•When using Monthly summary, the graph will display the average value of the variable over all months available.
               \n•Extreme values that have more than 4 digits are eliminated."
             , type = "info",closeOnClickOutside = TRUE)
})  
}

shinyApp(ui=ui, server=server)  
