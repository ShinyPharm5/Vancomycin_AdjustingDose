#Second part of tutorial, 9/9/2019
#see begining part of video for list of inputfunctions, outpufunctions, and renderfunctions
#OK, actually this is a rough draft 
#Possible themses here https://rstudio.github.io/shinythemes/

#Download the necessary libraries
library(shiny)
library(shinythemes)
library(mrgsolve)
library(ggplot2)
library(pracma)
library(rsconnect)

library(devtools)
Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))


#install.packages('rsconnect')

#rsconnect::setAccountInfo(name='trialshinyapp',
#token='5C4A81CFA124E16791084D24357BA039',
#secret='Bj0LuCSWJbMM4JEkgrEk1hGr6vQvGPj9DYFC39GV')

#library(rsconnect)
#rsconnect::deployApp('C:/Users/kcdeback/Desktop/Vancomycin_ShinyApp/Vancomycin')



#Set up the UI
ui <- navbarPage(title = "Vancomycin Dosing", theme = shinytheme("readable"),
      
    #First Panel for dossage Calculation
      
     tabPanel(title = "Initial Vancomycin Dose Regimen",
              titlePanel("Initial Vancomycin Dose Regimen"),
                sidebarLayout(
                  sidebarPanel(   
                    sliderInput(inputId = "age", label =  "Age (yrs)", 
                                                 value = 54, min = 19, max = 88),
                    sliderInput(inputId = "serum", label =  "Serum Creatinine (mg/dl)", 
                          value = 1.4, min = .3, max = 2.5),
                    selectInput(inputId = "gender", label =  "Gender", c("Male" = "m","Female" = "f")),
                    sliderInput(inputId = "weight", label =  "Body Weight (kg)", 
                          value = 185, min = 70, max = 293),
                    actionButton(class = "btn btn-primary", inputId = "calcer", label = "Calculate Regimen", style = "margin-bottom:25px;"),
                    uiOutput("tab1")),
                  mainPanel(
                tableOutput("result"),
                tableOutput("result4"),
                tableOutput("result44"),
                tableOutput("result444"),
                uiOutput("tab"),
                uiOutput("tab22"),
                uiOutput("tab55"))
                
              )
        ),
    tabPanel(title = "User-Determined Vancomycin Dose Regimen Graph",
             titlePanel("User-Determined Vancomycin Dose Regimen Graph"),
             sidebarLayout(
               sidebarPanel(
                 uiOutput("tab56"),
                 tags$div(style = "height:9px"),
                 fluidRow(
                 tags$div( style = "background: rgba(65,105,225, 0.15); border-radius: 5px; padding-left:.75vw; padding-right:.75vw;",
                 fluidRow(column(12,numericInput(inputId = "initial_dose", "Loading Dose (mg)", 0, min = 1, max = 6000))),
                 fluidRow(column(5,numericInput(inputId = "mait_dose1", "Mnt. Dose 1 (mg)", 0, min = 1, max = 6000)),column(7,numericInput(inputId = "mait_time1", "Time Since Ldg. Dose (hr)", 0, min = 11, max = 13))))),
                 fluidRow(column(5,numericInput(inputId = "mait_dose2", "Mnt. Dose 2 (mg)", 0, min = 1, max = 6000)),column(7,numericInput(inputId = "mait_time2", "Time Since Mnt. Dose 1 (hr)", 0, min = 11, max = 13))),
                 fluidRow(column(5,numericInput(inputId = "mait_dose3", "Mnt. Dose 3 (mg)", 0, min = 1, max = 6000)),column(7,numericInput(inputId = "mait_time3", "Time Since Mnt. Dose 2 (hr)", 0, min = 11, max = 13))),
                 fluidRow(column(5,numericInput(inputId = "mait_dose4", "Mnt. Dose 4 (mg)", 0, min = 1, max = 6000)),column(7,numericInput(inputId = "mait_time4", "Time Since Mnt. Dose 3 (hr)", 0, min = 11, max = 13))),
                 fluidRow(column(5,numericInput(inputId = "mait_dose5", "Mnt. Dose 5 (mg)", 0, min = 1, max = 6000)),column(7,numericInput(inputId = "mait_time5", "Time Since Mnt. Dose 4 (hr)", 0, min = 11, max = 13))),
                 actionButton(class = "btn btn-primary", inputId = "make_graph", label = "Make Model", style = "margin-bottom:25px;"),
                 sliderInput(inputId = "clear1", label =  "Vancomycin Clearence (L/hr)", 
                             value = 5.5, min = 1, max = 10),
                 sliderInput(inputId = "vol1", label =  "Volume of Distribution (V)", 
                             value = 85, min = 20, max = 150),
                 fluidRow(
                   column(6,align = "center",numericInput(inputId = "dose_conc", "Conentration", 0, min = 1, max = 96)),
                   column(6, align = "center", numericInput(inputId = "dose_time", "Time Since Initial Dose", 0, min = 1, max = 96))),
                 actionButton(class = "btn btn-primary", inputId = "calcer1", label = "Enter Observation Information", style = "margin-bottom:25px;"),
                 tableOutput("AUC15"),
                 actionButton(class = "btn btn-primary", inputId = "calcer2", label = "Reset Observation Information", style = "margin-bottom:25px;"),
                 uiOutput("tab3")
               ), mainPanel(tags$div(style = "height:70px"),plotOutput("plot55", height = 800),
                            fluidRow(
                              column(6,align = "center", tags$div( style = "background: rgba(0,0,225, 0.05); border-radius: 5px; padding-left:.5vw; padding-right:.75vw;",tableOutput("AUC5"))),
                              column(6,align = "center", tags$div( style = "background: rgba(251,236,93, 0.2); border-radius: 5px; padding-left:.5vw; padding-right:.75vw;",tableOutput("AUC55")))

                              )
                            )
                            )
             )
      )
    

#Servor Side Setup

server <- function(input, output, session) {
  
  #Setting the Calculate Button for Tab 2
  observeEvent(c(input$clear1,input$vol1), {
    if(var$calc >1 && var$calc < 10 && run$run1 == 1 && just_on_make$jom == 0){
      have_toggled$toggle <- 1
      proceed <- 1
      correct_dose <- var55$MD
      correct_time <- toString(as.integer(var55$MD)/1000)
      if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0 && input$mait_time2 !=0 && input$mait_dose2 !=0 && input$mait_time3 !=0 && input$mait_dose3 !=0 && input$mait_time4 !=0 && input$mait_dose4 !=0 && input$mait_time5 !=0 && input$mait_dose5 !=0){
        ok_togo$OK = 1
        amount1 <- input$initial_dose/1000
        amount2 <- input$mait_dose1/1000
        amount3 <- input$mait_dose2/1000
        amount4 <- input$mait_dose3/1000
        amount5 <- input$mait_dose4/1000
        amount6 <- input$mait_dose5/1000
        frequency1 <- var555$FR
        if(frequency1 == "12"){
          time55 <- "1"
        }else{
          time55 <- "0"
        }
        statement <- paste(input$initial_dose," over ",amount1,"h q ",input$mait_time1, "x1 then ", input$mait_dose1," over ", amount2,"h q", input$mait_time2, "x1 then ", input$mait_dose2," over ", amount3,"h q ", input$mait_time3, "x1 then ", input$mait_dose3," over ", amount4,"h q", input$mait_time4, "x1 then ", input$mait_dose4," over ", amount5,"h q", input$mait_time5, "x1 then ",input$mait_dose5," over ", amount6) 
        
      }else if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0 && input$mait_time2 !=0 && input$mait_dose2 !=0 && input$mait_time3 !=0 && input$mait_dose3 !=0 && input$mait_time4 !=0 && input$mait_dose4 !=0){
        ok_togo$OK = 1
        amount1 <- input$initial_dose/1000
        amount2 <- input$mait_dose1/1000
        amount3 <- input$mait_dose2/1000
        amount4 <- input$mait_dose3/1000
        amount5 <- input$mait_dose4/1000
        frequency1 <- var555$FR
        if(frequency1 == "12"){
          time55 <- "2"
        }else{
          time55 <- "1"
        }
        statement <- paste(input$initial_dose," over ",amount1,"h q ", input$mait_time1, "x1 then ", input$mait_dose1," over ", amount2,"h q", input$mait_time2, "x1 then ", input$mait_dose2," over ", amount3,"h q", input$mait_time3, "x1 then ", input$mait_dose3," over ", amount4,"h q", input$mait_time4, "x1 then ", input$mait_dose4," over ", amount5)
        
      }else if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0 && input$mait_time2 !=0 && input$mait_dose2 !=0 && input$mait_time3 !=0 && input$mait_dose3 !=0){
        ok_togo$OK = 1
        amount1 <- input$initial_dose/1000
        amount2 <- input$mait_dose1/1000
        amount3 <- input$mait_dose2/1000
        amount4 <- input$mait_dose3/1000
        frequency1 <- var555$FR
        if(frequency1 == "12"){
          time55 <- "3"
        }else{
          time55 <- "1"
        }
        statement <- paste(input$initial_dose," over ",amount1,"h q", input$mait_time1,  "x1 then", input$mait_dose1," over ", amount2,"h q", input$mait_time2, "x1 then ", input$mait_dose2," over ", amount3,"h q", input$mait_time3, "x1 then ", input$mait_dose3," over ", amount4)
        
      }else if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0 && input$mait_time2 !=0 && input$mait_dose2 !=0){
        ok_togo$OK = 1
        amount1 <- input$initial_dose/1000
        amount2 <- input$mait_dose1/1000
        amount3 <- input$mait_dose2/1000
        frequency1 <- var555$FR
        if(frequency1 == "12"){
          time55 <- "4"
        }else{
          time55 <- "2"
        }
        statement <- paste(input$initial_dose," over ",amount1,"h q", input$mait_time1, "x 1 then ", input$mait_dose1," over ", amount2,"h q", input$mait_time2,"x 1 then ", input$mait_dose2," over ", amount3)
      }else if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0){
        ok_togo$OK = 1
        amount1 <- input$initial_dose/1000
        amount2 <- input$mait_dose1/1000
        frequency1 <- var555$FR
        if(frequency1 == "12"){
          time55 <- "4"
        }else{
          time55 <- "2"
        }
        statement <- paste(input$initial_dose," over ",amount1,"h q", input$mait_time1, "x 1 then ", input$mait_dose1," over ", amount2, sep = "")
      }
      else{
        proceed <- 0
      }
      if(proceed == 1){
        code<- paste('
                     $PARAM @annotated
                     CL   :  ', toString(input$clear1) ,': Clearance (volume/time)
                     V    : ', toString(input$vol1) ,' : Central volume (volume)
                     KA   :  1 : Absorption rate constant (1/time)
                     
                     $CMT  @annotated
                     EV   : Extravascular compartment
                     CENT : Central compartment
                     
                     $GLOBAL
                     #define CP (CENT/V)
                     
                     $PKMODEL ncmt = 1, depot = TRUE
                     
                     $CAPTURE @annotated
                     CP : Plasma concentration (mass/volume)
                     ')
        
        output$plot55 <- renderPlot({
          mod <- mcode("modified1", code)
          
          out <- mod %>% 
            ev_rx(statement) %>%
            mrgsim(end = 96, delta = 0.1) 
          df2 <- as.data.frame(out)
          data_frame552$DF <- df2
          time_data <- as.vector(df2$time)
          cp_data <- as.vector(df2$CP)
          counter24<- 0
          counter48 <- 0
          counter72 <- 0
          counter96 <-0
          counter <- 1
          
          
          while(time_data[counter] <= 24){
            counter <- counter + 1
          }
          counter24 <- counter
          while(time_data[counter] <= 48){
            counter <- counter + 1
            
          }
          counter48 <- counter
          while(time_data[counter] <= 72){
            counter <- counter + 1
            
          }
          counter72 <- counter
          while(time_data[counter] < 96){
            counter <- counter + 1
            
          }
          counter96 <- counter
          
          AUC2425$a <- trapz(time_data[0:counter24], cp_data[0:counter24])
          AUC4825$a <- trapz(time_data[counter24:counter48], cp_data[counter24:counter48])
          AUC7225$a <- trapz(time_data[counter48:counter72], cp_data[counter48:counter72])
          AUC9625$a <- trapz(time_data[counter72:counter96], cp_data[counter72:counter96])
          if(length(list1$list15)>0){
            df22 <- data.frame(matrix(unlist(list1$list15), nrow=length(list1$list15), byrow=T),stringsAsFactors=FALSE)
            colnames(df22)<- c("CP","time")
            g1 <- df22
            ggplot() + geom_line(data_frame552$DF, mapping = aes(x = time, y = CP), size = 1, color = "#F3C849") + geom_line(data_frame55$DF, mapping = aes(x = time, y = CP), linetype="dotted", size = 1, color = "blue") + geom_point(data=g1,aes(x = time, y = CP), colour="red", size = 5)+ theme_bw() + ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))  +
              xlab(paste("Time (hr)",sep=""))+
              ylab("Plasma Concentration (mg/L)") +
              theme(axis.title.y = element_text(size = rel(1.4)))+theme(axis.text = element_text(size = rel(1.21))) +theme(axis.title.x = element_text(size = rel(1.4)))+ scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96)) 
          }
          else{
            ggplot() + geom_line(data_frame552$DF, mapping = aes(x = time, y = CP), size = 1, color = "#F3C849") + geom_line(data_frame55$DF, mapping = aes(x = time, y = CP), linetype="dotted", size = 1, color = "blue") +  theme_bw() + ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))  +
              xlab(paste("Time (hr)",sep=""))+
              ylab("Plasma Concentration (mg/L)") +
              theme(axis.title.y = element_text(size = rel(1.4)))+theme(axis.text = element_text(size = rel(1.21))) +theme(axis.title.x = element_text(size = rel(1.4)))+ scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96)) 
            
          }
          
        })
        
        
        output$AUC55 <- renderTable( align = "r",{
          data = inputdata()
          resultTable = data.frame(
            "Proscribed Vancomycin Clearance (L/h)" = toString(round(AUC2425$a,digits = 0)),
            "Proscribed Vancomycin Clearance (L/h)" = toString(round(AUC4825$a,digits = 0)),
            "Proscribed Vancomycin Clearance (L/h)" = toString(round(AUC7225$a,digits = 0)),
            "Proscribed Vancomycin Clearance (L/h)" = toString(round(AUC9625$a,digits = 0))
          )
          colnames(resultTable) <- c("AUC (hr*mg/L) for 0-24 hrs","24-48 hrs","48-72 hrs","72-96 hrs")
          format(resultTable, justify = "centre")
          resultTable
        })
        
      }
    }
    
    if(just_on_make$jom == 1){
      just_on_make$jom = 0
    }
})
  
  
  
  
  
  
  
  
  
  
  
  #Make Reactive Values for Second Tab
  dose5 <- reactiveValues(do = 0)
  cp5 <- reactiveValues(cp = 0)
  ok_2 <- reactiveValues(ok=0)
  AUC242 <- reactiveValues(a = 0)
  AUC482 <- reactiveValues(a = 0)
  AUC722<- reactiveValues(a = 0)
  AUC962 <- reactiveValues(a = 0)
  AUC2425 <- reactiveValues(a = 0)
  AUC4825 <- reactiveValues(a = 0)
  AUC7225<- reactiveValues(a = 0)
  AUC9625 <- reactiveValues(a = 0)
  list1 <- reactiveValues(list15 = list())
  run <- reactiveValues(run1 = 0)
  have_toggled <- reactiveValues(toggle = 0)
  

  #When Button is Pressed on the second panel
  #var535 <- list(c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1))
  list11 <- list(c("",""))
  df22 <- data.frame(matrix(unlist(list11), nrow=length(list11), byrow=T),stringsAsFactors=FALSE)
  colnames(df22)<- c("Concentration (micromoles)","Time (hours)")
  output$AUC15 <- renderTable(df22)
  
  observeEvent(input$calcer1, {
    if(run$run1==1){
    temp <- list(c(input$dose_conc, input$dose_time))
    list1$list15 <- c(list1$list15, temp)
    df22 <- data.frame(matrix(unlist(list1$list15), nrow=length(list1$list15), byrow=T),stringsAsFactors=FALSE)
    colnames(df22)<- c("Concentration (micromoles)","Time (hours)")
    output$AUC15 <- renderTable(df22)
    output$plot55 <- renderPlot({
      colnames(df22)<- c("CP","time")
      g1 <- df22
      
      if(data_frame552$DF == 0 || have_toggled$toggle == 0){
      ggplot(data_frame55$DF, aes(x = time, y = CP, color = "blue"), size = 5) + theme_bw() + ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ geom_line(color = "blue", size = 1,linetype="dotted") +
        xlab(paste("Time (hr)",sep=""))+geom_point(data=g1,aes(x = time, y = CP), colour="red", size = 5)+theme(axis.text = element_text(size = rel(1.21)))+theme(axis.title.y = element_text(size = rel(1.4))) +theme(axis.title.x = element_text(size = rel(1.4)))+ 
        ylab("Plasma Concentration (mg/L)") +
        scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96))
      }
      else{
        ggplot() + geom_line(data_frame552$DF, mapping = aes(x = time, y = CP), size = 1, color = "#F3C849") + geom_line(data_frame55$DF, mapping = aes(x = time, y = CP), linetype="dotted", size = 1, color = "blue") + geom_point(data=g1,aes(x = time, y = CP), colour="red", size = 5)+ theme_bw() + ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))  +
          xlab(paste("Time (hr)",sep=""))+
          ylab("Plasma Concentration (mg/L)") +
          theme(axis.title.y = element_text(size = rel(1.4)))+theme(axis.text = element_text(size = rel(1.21))) +theme(axis.title.x = element_text(size = rel(1.4)))+ scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96)) 
      }
    })
  }})
  
  observeEvent(input$calcer2, {
    if(run$run1 == 1){
    list1$list15 <- list()
    temp2 <- list(c("",""))
    df22 <- data.frame(matrix(unlist(temp2), nrow=length(temp2), byrow=T),stringsAsFactors=FALSE)
    colnames(df22)<- c("Concentration (micromoles)","Time (hours)")
    output$AUC15 <- renderTable(df22)
    output$plot55 <- renderPlot({
    if(data_frame552$DF == 0 || have_toggled$toggle == 0){
      ggplot(data_frame55$DF, aes(x = time, y = CP, color = "blue"), size = 5) + theme_bw() + ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ geom_line(color = "blue", size = 1,linetype="dotted") +
        xlab(paste("Time (hr)",sep=""))+theme(axis.text = element_text(size = rel(1.21)))+theme(axis.title.y = element_text(size = rel(1.4))) +theme(axis.title.x = element_text(size = rel(1.4)))+ 
        ylab("Plasma Concentration (mg/L)") +
        scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96))
    }
    else{
      ggplot() + geom_line(data_frame552$DF, mapping = aes(x = time, y = CP), size = 1, color = "#F3C849") + geom_line(data_frame55$DF, mapping = aes(x = time, y = CP), linetype="dotted", size = 1, color = "blue") +  theme_bw() + ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))  +
        xlab(paste("Time (hr)",sep=""))+
        ylab("Plasma Concentration (mg/L)") +
        theme(axis.title.y = element_text(size = rel(1.4)))+theme(axis.text = element_text(size = rel(1.21))) +theme(axis.title.x = element_text(size = rel(1.4)))+ scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96)) 
    }
    })
      
    }})
  #First Panel Server Side Setup
  
  #italics under input
  output$tab1 <- renderUI({
    tag('i',"The parameters for the variables shown are in accordance with the paraemeters of the test group in the referenced paper. Please note that this regimen calculator is only applicable for patients with Vancomycin clearance values greater than 1 and less than 10.")
  })
  
  output$tab56 <- renderUI({
    tag('i',"Fill in at least 2 doses for model computation.")
  })
  
  output$tab3 <- renderUI({
    tag('i',"All doses are given at 1000mg/hr.")
  })
  
  output$tab22 <- renderUI({
    tag('i',"Disclaimer: This application is for educational purposes and should not replace sound clinical judgement for dosage selection.")
  })
  
  
  #URL for referencing publication
  url <- a("Dosing vancomycin in the super obese: less is more", href="https://academic.oup.com/jac/article/73/11/3081/5091849", target="_blank")
  output$tab <- renderUI({
    tagList("Reference:", url)
  })
  
  url2 <- a("here", href="https://github.com/ShinyPharm5/Vancomycin/tree/master", target="_blank")
  output$tab55 <- renderUI({
    tagList("The source code for this application is available ", url2)
  })
  
  #Setting changing variables as user adjusts
  var <- reactiveValues(calc = 0)
  var5 <- reactiveValues(LD = 0)
  var55 <- reactiveValues(MD = 0)
  var555 <- reactiveValues(FR = 0)
  data_frame5 <- reactiveValues(DF = 0)
  data_frame55 <- reactiveValues(DF=0)
  data_frame552 <- reactiveValues(DF=0)
  ok_togo <- reactiveValues(OK = 0)
  dose55 <- reactiveValues(do=0)
  just_on_make <- reactiveValues(jom = 1)
  
  
  #Make calculation for tab 2
  
  
  
  #Setting the Calculate Button for Tab 2
  observeEvent(input$make_graph, {
    if(run$run1 == 1){
    just_on_make$jom <- 1
    }
    updateSliderInput(session, "clear1", value = var$calc, step = .001)
    updateSliderInput(session, "vol1", value = 85, step = 1)
    run$run1 <- 1
    if(var$calc >1 && var$calc < 10){
    proceed <- 1
    correct_dose <- var55$MD
    correct_time <- toString(as.integer(var55$MD)/1000)
    if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0 && input$mait_time2 !=0 && input$mait_dose2 !=0 && input$mait_time3 !=0 && input$mait_dose3 !=0 && input$mait_time4 !=0 && input$mait_dose4 !=0 && input$mait_time5 !=0 && input$mait_dose5 !=0){
      ok_togo$OK = 1
      amount1 <- input$initial_dose/1000
      amount2 <- input$mait_dose1/1000
      amount3 <- input$mait_dose2/1000
      amount4 <- input$mait_dose3/1000
      amount5 <- input$mait_dose4/1000
      amount6 <- input$mait_dose5/1000
      frequency1 <- var555$FR
      if(frequency1 == "12"){
        time55 <- "1"
      }else{
        time55 <- "0"
      }
      statement <- paste(input$initial_dose," over ",amount1,"h q ",input$mait_time1, "x1 then ", input$mait_dose1," over ", amount2,"h q", input$mait_time2, "x1 then ", input$mait_dose2," over ", amount3,"h q ", input$mait_time3, "x1 then ", input$mait_dose3," over ", amount4,"h q", input$mait_time4, "x1 then ", input$mait_dose4," over ", amount5,"h q", input$mait_time5, "x1 then ",input$mait_dose5," over ", amount6) 

      }else if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0 && input$mait_time2 !=0 && input$mait_dose2 !=0 && input$mait_time3 !=0 && input$mait_dose3 !=0 && input$mait_time4 !=0 && input$mait_dose4 !=0){
      ok_togo$OK = 1
      amount1 <- input$initial_dose/1000
      amount2 <- input$mait_dose1/1000
      amount3 <- input$mait_dose2/1000
      amount4 <- input$mait_dose3/1000
      amount5 <- input$mait_dose4/1000
      frequency1 <- var555$FR
      if(frequency1 == "12"){
        time55 <- "2"
      }else{
        time55 <- "1"
      }
      statement <- paste(input$initial_dose," over ",amount1,"h q ", input$mait_time1, "x1 then ", input$mait_dose1," over ", amount2,"h q", input$mait_time2, "x1 then ", input$mait_dose2," over ", amount3,"h q", input$mait_time3, "x1 then ", input$mait_dose3," over ", amount4,"h q", input$mait_time4, "x1 then ", input$mait_dose4," over ", amount5)

    }else if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0 && input$mait_time2 !=0 && input$mait_dose2 !=0 && input$mait_time3 !=0 && input$mait_dose3 !=0){
      ok_togo$OK = 1
      amount1 <- input$initial_dose/1000
      amount2 <- input$mait_dose1/1000
      amount3 <- input$mait_dose2/1000
      amount4 <- input$mait_dose3/1000
      frequency1 <- var555$FR
      if(frequency1 == "12"){
        time55 <- "3"
      }else{
        time55 <- "1"
      }
      statement <- paste(input$initial_dose," over ",amount1,"h q", input$mait_time1,  "x1 then", input$mait_dose1," over ", amount2,"h q", input$mait_time2, "x1 then ", input$mait_dose2," over ", amount3,"h q", input$mait_time3, "x1 then ", input$mait_dose3," over ", amount4)

    }else if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0 && input$mait_time2 !=0 && input$mait_dose2 !=0){
      ok_togo$OK = 1
      amount1 <- input$initial_dose/1000
      amount2 <- input$mait_dose1/1000
      amount3 <- input$mait_dose2/1000
      frequency1 <- var555$FR
      if(frequency1 == "12"){
        time55 <- "4"
      }else{
        time55 <- "2"
      }
      statement <- paste(input$initial_dose," over ",amount1,"h q", input$mait_time1, "x 1 then ", input$mait_dose1," over ", amount2,"h q", input$mait_time2,"x 1 then ", input$mait_dose2," over ", amount3)
    }else if(input$initial_dose != 0 && input$mait_dose1 != 0 && input$mait_time1 != 0){
      ok_togo$OK = 1
      amount1 <- input$initial_dose/1000
      amount2 <- input$mait_dose1/1000
      frequency1 <- var555$FR
      if(frequency1 == "12"){
        time55 <- "4"
      }else{
        time55 <- "2"
      }
      statement <- paste(input$initial_dose," over ",amount1,"h q", input$mait_time1, "x 1 then ", input$mait_dose1," over ", amount2, sep = "")
    }
    else{
      proceed <- 0
    }
    if(proceed == 1){
      code<- paste('
      $PARAM @annotated
      CL   :  ', toString(var$calc) ,': Clearance (volume/time)
      V    : 75 : Central volume (volume)
      KA   :  1 : Absorption rate constant (1/time)
      
      $CMT  @annotated
      EV   : Extravascular compartment
      CENT : Central compartment
      
      $GLOBAL
      #define CP (CENT/V)
      
      $PKMODEL ncmt = 1, depot = TRUE
      
      $CAPTURE @annotated
      CP : Plasma concentration (mass/volume)
      ')

    output$plot55 <- renderPlot({
      mod <- mcode("modified1", code)
      
      out <- mod %>% 
        ev_rx(statement) %>%
        mrgsim(end = 96, delta = 0.1) 
      df <- as.data.frame(out)
      data_frame55$DF = df
      time_data <- as.vector(data_frame55$DF$time)
      cp_data <- as.vector(data_frame55$DF$CP)
      counter24<- 0
      counter48 <- 0
      counter72 <- 0
      counter96 <-0
      counter <- 1
      
      
      while(time_data[counter] <= 24){
        counter <- counter + 1
      }
      counter24 <- counter
      while(time_data[counter] <= 48){
        counter <- counter + 1
        
      }
      counter48 <- counter
      while(time_data[counter] <= 72){
        counter <- counter + 1
        
      }
      counter72 <- counter
      while(time_data[counter] < 96){
        counter <- counter + 1
        
      }
      counter96 <- counter
      
      AUC242$a <- trapz(time_data[0:counter24], cp_data[0:counter24])
      AUC482$a <- trapz(time_data[counter24:counter48], cp_data[counter24:counter48])
      AUC722$a <- trapz(time_data[counter48:counter72], cp_data[counter48:counter72])
      AUC962$a <- trapz(time_data[counter72:counter96], cp_data[counter72:counter96])
      if(length(list1$list15)>0){
        df22 <- data.frame(matrix(unlist(list1$list15), nrow=length(list1$list15), byrow=T),stringsAsFactors=FALSE)
        colnames(df22)<- c("CP","time")
        g1 <- df22
        ggplot(df, aes(x = time, y = CP, color = "blue"), size = 5) + theme_bw() + ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ geom_line(color = "blue", size = 1,linetype="dotted")  +geom_point(data=g1,aes(x = time, y = CP), colour="red", size = 5)+
        xlab(paste("Time (hr)",sep=""))+
        ylab("Plasma Concentration (mg/L)") +
        theme(axis.title.y = element_text(size = rel(1.4)))+theme(axis.text = element_text(size = rel(1.21))) +theme(axis.title.x = element_text(size = rel(1.4)))+ scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96)) 
      }
      else{
        ggplot(df, aes(x = time, y = CP, color = "blue"), size = 5) + theme_bw() + ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))+ geom_line(color = "blue", size = 1,linetype="dotted")  +
          xlab(paste("Time (hr)",sep=""))+
          ylab("Plasma Concentration (mg/L)") +
          theme(axis.title.y = element_text(size = rel(1.4)))+theme(axis.text = element_text(size = rel(1.21))) +theme(axis.title.x = element_text(size = rel(1.4)))+ scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96)) 
        
      }
      
      
    })
    
    
    output$AUC5 <- renderTable( align = "r",{
      data = inputdata()
      resultTable = data.frame(
        "Proscribed Vancomycin Clearance (L/h)" = toString(round(AUC242$a,digits = 0)),
        "Proscribed Vancomycin Clearance (L/h)" = toString(round(AUC482$a,digits = 0)),
        "Proscribed Vancomycin Clearance (L/h)" = toString(round(AUC722$a,digits = 0)),
        "Proscribed Vancomycin Clearance (L/h)" = toString(round(AUC962$a,digits = 0))
      )
      colnames(resultTable) <- c("AUC (hr*mg/L) for 0-24 hrs","24-48 hrs","48-72 hrs","72-96 hrs")
      format(resultTable, justify = "centre")
      resultTable
    })
    output$AUC55 <- renderTable( align = "r",{
      data = inputdata()
      resultTable = data.frame(
        "Proscribed Vancomycin Clearance (L/h)" = "N/A",
        "Proscribed Vancomycin Clearance (L/h)" = "N/A",
        "Proscribed Vancomycin Clearance (L/h)" = "N/A",
        "Proscribed Vancomycin Clearance (L/h)" = "N/A"
      )
      colnames(resultTable) <- c("AUC (hr*mg/L) for 0-24 hrs","24-48 hrs","48-72 hrs","72-96 hrs")
      format(resultTable, justify = "centre")
      resultTable
    })
    
    }
    }
  })
  
  #Sensing when user submits via the "Calculate" Button
  observeEvent(input$calcer, {
    output$AUC55 <- renderTable( align = "r",{
      data = inputdata()
      resultTable = data.frame(
        "Proscribed Vancomycin Clearance (L/h)" = "N/A",
        "Proscribed Vancomycin Clearance (L/h)" = "N/A",
        "Proscribed Vancomycin Clearance (L/h)" = "N/A",
        "Proscribed Vancomycin Clearance (L/h)" = "N/A"
      )
      colnames(resultTable) <- c("AUC (hr*mg/L) for 0-24 hrs","24-48 hrs","48-72 hrs","72-96 hrs")
      format(resultTable, justify = "centre")
      resultTable
    })
    
    have_toggled$toggle <- 0
    list11 <- list(c("",""))
    df22 <- data.frame(matrix(unlist(list11), nrow=length(list11), byrow=T),stringsAsFactors=FALSE)
    colnames(df22)<- c("Concentration (micromoles)","Time (hours)")
    output$AUC15 <- renderTable(df22)
    list1$list15 <- list()
    run$run1 <-0
    just_on_make$jom <- 0 
    ok_2$ok = 1
    if(input$gender == "m"){
      var2 = 1
    }else{
      var2 = 0
    }
    var5 <- signif((9.656 - (0.078 * input$age) - (2.009 * input$serum) + (1.09 * var2) + (0.04 * (input$weight)^.75)),4)
    var$calc <-var5
    updateSliderInput(session, "clear1", value = var$calc, step = .001)
    updateSliderInput(session, "vol1", value = 85, step = 1)
    output$plot55 <- renderPlot({
      ggplot() + theme_bw()+  ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))+
        xlab(paste("Time (hr)",sep=""))+
        ylab("Plasma Concentration (mg/L)") +
        theme(axis.title.y = element_text(size = rel(1.4)))+theme(axis.text = element_text(size = rel(1.21))) +theme(axis.title.x = element_text(size = rel(1.4)))+ scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96)) 
    })
  })
  
  #Setting up dataframe for table with no call from button
  inputdata <- reactive(
    {
      data <- data.frame(
        CV = var$calc
      )
      data
    }
  )
  
  
  #Rendering table using data frame with no call from button
  output$result <- renderTable( align = "l",{
    data = inputdata()
    if(data$CV == 0 || data$CV < 0.5){
      var5$LD="N/A"
      var55$MD="N/A"
      var555$FR = "N/A"
    }
    else if(data$CV >.5 && data$CV < 1.5){
      var5$LD = 2500
      var55$MD = 500
    }
    else if(data$CV >=1.5 && data$CV < 2.5){
      var5$LD = 2500
      var55$MD = 1000
    }
    else if(data$CV >=2.5 && data$CV < 3.5){
      var5$LD = 2500
      var55$MD = 1500
    }
    else if( data$CV>=3.5 && data$CV < 4.5){
      var5$LD = 2500
      var55$MD = 1000
    }
    else if( data$CV>=4.5 && data$CV < 5.5){
      var5$LD = 2500
      var55$MD = 1250
    }
    else if(data$CV >=5.5 && data$CV < 6.5){
      var5$LD = 2500
      var55$MD = 1500
    }
    else if(data$CV >=6.5 && data$CV < 7.5){
      var5$LD = 2500
      var55$MD = 1750
    }
    else if(data$CV >=7.5 && data$CV < 8.5){
      var5$LD = 3000
      var55$MD = 2000
    }
    else if(data$CV >=8.5 && data$CV < 9.5){
      var5$LD = 3000
      var55$MD = 2250
    }
    else if(data$CV >=9.5 ){
      var5$LD = 3000
      var55$MD = 2250
    }
    resultTable = data.frame(
      "Vancomycin Clearance (L/h)" = "N/A"
    )
    colnames(resultTable) <- c("Vancomycin Clearance (L/hr)")
    format(resultTable, justify = "centre")
    resultTable
  })
  
  output$result4 <- renderTable( align = "l",{
    data = inputdata()
    resultTable = data.frame(
      "Loading Dose (mg)" = "N/A"
    )
    colnames(resultTable) <- c("Loading Dose (mg)")
    format(resultTable, justify = "centre")
    resultTable
  })
  
  output$result44 <- renderTable( align = "l",{
    data = inputdata()
    resultTable = data.frame(
      "Maintanance Dose (mg)" = "N/A"
    )
    colnames(resultTable) <- c("Maintenance Dose (mg)")
    format(resultTable, justify = "centre")
    resultTable
  })
  
  output$result444 <- renderTable( align = "l",{
    data = inputdata()
    resultTable = data.frame(
      "Maintenance Dose Frequency (hr)" = "N/A"
    )
    colnames(resultTable) <- c("Maintenance Dose Frequency (hr)")
    format(resultTable, justify = "centre")
    resultTable
  })
  

  output$AUC5 <- renderTable( align = "r",{
    data = inputdata()
    resultTable = data.frame(
      "Proscribed Vancomycin Clearance (L/h)" = "N/A",
      "Proscribed Vancomycin Clearance (L/h)" = "N/A",
      "Proscribed Vancomycin Clearance (L/h)" = "N/A",
      "Proscribed Vancomycin Clearance (L/h)" = "N/A"
    )
    colnames(resultTable) <- c("AUC (hr*mg/L) for 0-24 hrs","24-48 hrs","48-72 hrs","72-96 hrs")
    format(resultTable, justify = "centre")
    resultTable
  })
  
  output$AUC55 <- renderTable( align = "r",{
    data = inputdata()
    resultTable = data.frame(
      "Proscribed Vancomycin Clearance (L/h)" = "N/A",
      "Proscribed Vancomycin Clearance (L/h)" = "N/A",
      "Proscribed Vancomycin Clearance (L/h)" = "N/A",
      "Proscribed Vancomycin Clearance (L/h)" = "N/A"
    )
    colnames(resultTable) <- c("AUC (hr*mg/L) for 0-24 hrs","24-48 hrs","48-72 hrs","72-96 hrs")
    format(resultTable, justify = "centre")
    resultTable
  })
  
  make_it <- reactiveValues(make = 1)
  #Setting up datframe when button call happens
  observeEvent(input$calcer, {
    
    
    inputdata <- reactive(
      {
        data <- data.frame(
          CV = var$calc
        )
        data
      }
    )
    
  #Rendering table when button call happens
  output$result <- renderTable(align = "l",{
    data = inputdata()
    if(data$CV == 0 || data$CV < 1){
      var5$LD="N/A"
      var55$MD="N/A"
      var555$FR = "N/A"
      make_it$make = 0
    }
    else if(data$CV >=1 && data$CV < 1.5){
      var5$LD = "2500"
      var55$MD = "500"
      var555$FR = "24"
      make_it$make = 1
    }
    else if(data$CV >=1.5 && data$CV < 2.5){
      var5$LD = "2500"
      var55$MD = "1000"
      var555$FR = "24"
      make_it$make = 1
    }
    else if(data$CV >=2.5 && data$CV < 3.5){
      var5$LD = "2500"
      var55$MD = "1500"
      var555$FR = "24"
      make_it$make = 1
    }
    else if( data$CV>=3.5 && data$CV < 4.5){
      var5$LD = "2500"
      var55$MD = "1000"
      var555$FR = "12"
      make_it$make = 1
    }
    else if( data$CV>=4.5 && data$CV < 5.5){
      var5$LD = "2500"
      var55$MD = "1250"
      var555$FR = "12"
      make_it$make = 1
    }
    else if(data$CV >=5.5 && data$CV < 6.5){
      var5$LD = "2500"
      var55$MD = "1500"
      var555$FR = "12"
      make_it$make = 1
    }
    else if(data$CV >=6.5 && data$CV < 7.5){
      var5$LD = "2500"
      var55$MD = "1750"
      var555$FR = "12"
      make_it$make = 1
    }
    else if(data$CV >=7.5 && data$CV < 8.5){
      var5$LD = "3000"
      var55$MD = "2000"
      var555$FR = "12"
      make_it$make = 1
    }
    else if(data$CV >=8.5 && data$CV < 9.5){
      var5$LD = "3000"
      var55$MD = "2250"
      var555$FR = "12"
      make_it$make = 1
    }
    else if(data$CV >=9.5 && data$CV < 10){
      var5$LD = "3000"
      var55$MD = "2250"
      var555$FR = "12"
      make_it$make = 1
    }
    else if(data$CV >=10){
      var5$LD = "3000"
      var55$MD = "2250"
      var555$FR = "N/A"
      make_it$make = 0
    }
    resultTable = data.frame(
      "Vancomycin Clearance (L/h)"= toString(data$CV)
    )
    colnames(resultTable) <- c("Vancomycin Clearance (L/hr)")
    format(resultTable, justify = "right")
    resultTable
  })
  
  output$result4 <- renderTable( align = "l",{
    data = inputdata()
    resultTable = data.frame(
      "Loading Dose (mg)" = var5$LD
    )
    colnames(resultTable) <- c("Loading Dose (mg)")
    format(resultTable, justify = "centre")
    resultTable
  })
  
  output$result44 <- renderTable( align = "l",{
    data = inputdata()
    resultTable = data.frame(
      "Loading Dose (mg)" = var55$MD
    )
    colnames(resultTable) <- c("Maintenance Dose (mg)")
    format(resultTable, justify = "centre")
    resultTable
  })
  
  output$result444 <- renderTable( align = "l",{
    data = inputdata()
    resultTable = data.frame(
      "Loading Dose (mg)" = var555$FR
    )
    colnames(resultTable) <- c("Maintenance Dose Frequency (hr)")
    format(resultTable, justify = "centre")
    resultTable
  })
  
  #Render Plot with Button Call

  
  
  })
  

  
  #Render the plot using mrgsolve independemt of click
  
  output$plot55 <- renderPlot({
    ggplot() + theme_bw()+  ggtitle("Plasma Concentration-Time Curve") + theme(plot.title = element_text(size = rel(2), hjust = 0.5))+
      xlab(paste("Time (hr)",sep=""))+
      ylab("Plasma Concentration (mg/L)") +
      theme(axis.title.y = element_text(size = rel(1.4)))+theme(axis.text = element_text(size = rel(1.21))) +theme(axis.title.x = element_text(size = rel(1.4)))+ scale_x_continuous(breaks=c(0,12,24,36,48,60,72,84,96)) 
  })
  
}

#Putting App Together



shinyApp(ui = ui, server = server)