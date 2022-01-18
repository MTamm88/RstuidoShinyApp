library(readxl)
library(tidyverse)
library(zoo)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(gridExtra)
library(shiny)
library(plotly)
library(rsconnect)

Activities <- read_excel("Activities.xlsx", 
                         col_types = c("date", "text", "numeric", 
                                       "numeric", "skip", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "skip", "numeric", "skip", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "skip", "numeric", 
                                       "skip", "numeric", "numeric"))

select(Activities)

Activities$time_m <- signif(Activities$time_m*24*60, digits = 2)
Activities$avg_pace_m <- signif(Activities$avg_pace_m*24*60, digits = 2)
Activities$bestpace_m <- signif(Activities$bestpace_m*24, digits = 2)
Activities$sleep_m <- signif(Activities$sleep_m*24*60, digits = 2)

Activities <- mutate(Activities, 
                     training_type = ifelse((distance_km>=13 & avg_pace_m>4.1 & AVG_hr>149 & AVG_hr<167), "Aerobic", 
                                            ifelse ((MAX_hr>=180 & AVG_hr>155 & max_cadence>=190 & distance_km+1<lap_nr), "Interval",
                                                    ifelse ((avg_pace_m>=6.1), "BodyTraining",
                                                            ifelse ((MAX_hr>=180 & AVG_hr>155 & max_cadence>=190), "Competition", "Recovery")))))

Activities <- mutate(Activities, 
                     HR_Group = ifelse((AVG_hr<=145), "130-145", 
                                       ifelse ((AVG_hr>145 & AVG_hr<=150), "146-150",
                                               ifelse ((AVG_hr>150 & AVG_hr<=155), "151-155", "156-180"))))

Activities <- mutate(Activities,
                     date_sum = format(as.Date(Activities$date,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y'))

Activities$date_sum <- as.Date(Activities$date_sum, "%m/%d/%Y")

Activities$date <- as.yearmon(Activities$date, "%m/%y")


Activities <-Activities %>%  
  mutate(
    vo2maxprevious = lag(vo2max, order_by = date_sum)) %>% 
  arrange(date_sum)

Activities <- mutate(Activities,
                     vo2maxefect = ifelse(vo2max >= vo2maxprevious & vo2max >= 58, "Good", "Bad"))


Activities <-Activities %>%  
  mutate(
    trainingbefore = lag(date_sum, order_by = date_sum)) %>% 
  arrange(date_sum)

Activities <- mutate(Activities,
                     restdays = as.numeric(date_sum-trainingbefore))

Activities$restdays <- as.numeric(Activities$restdays)

Activities <- select (Activities, training_type, HR_Group, everything())

example_data_summary <-
  Activities %>%
  group_by(date) %>%
  drop_na() %>%
  summarize(heart_rate = mean(AVG_hr), 
            pace = mean(avg_pace_m), 
            vo2max = mean(vo2max), 
            sleep = mean(sleep_m), 
            elev_gain = sum(evel_gain), 
            cadence = mean(avg_cadence),
            distance = sum(distance_km),
            time = sum(time_m)) %>%
  ungroup() 


Activity_summary <- summary(example_data_summary)

RecoveryAct <- Activities %>% filter(training_type == "Recovery")

example_data_recovery <-
  RecoveryAct %>%
  group_by(date) %>%
  drop_na() %>%
  summarize(heart_rate = mean(AVG_hr), 
            pace = mean(avg_pace_m), 
            vo2max = mean(vo2max), 
            sleep = mean(sleep_m), 
            elev_gain = sum(evel_gain), 
            cadence = mean(avg_cadence),
            distance = sum(distance_km),
            time = sum(time_m)) %>%
  ungroup() 

regression_results <- lm(vo2max ~ AVG_hr+distance_km+avg_pace_m+avg_cadence + aerobicTE + sleep_m + restdays, data = RecoveryAct)

summary(regression_results)


classification_tree <- rpart(vo2maxefect ~ AVG_hr+distance_km+avg_pace_m+avg_cadence + aerobicTE + time_m + MAX_hr + sleep_m + restdays ,data = RecoveryAct,
                             control = rpart.control(maxdepth = 4, minsplit=5), method = "class")

classificationPLOT <- prp(classification_tree, type = 1, extra = 100, split.font = 2, varlen = 20)

predict_unseen <-predict(classification_tree, RecoveryAct, type = 'class')

table_mat <- table(RecoveryAct$vo2maxefect, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))


Activity_summary <- summary(example_data_recovery)


ui <- fluidPage(titlePanel("Running trainings analyze", windowTitle = "RUN2021"),
                helpText("Eelnevate treeningute analuus tulevaste treeningute prognoosimiseks"),
                br(),
                tabsetPanel(
                  tabPanel("Rakenduse kirjeldus",
                           sidebarLayout(
                             sidebarPanel(img(height = "100%", width = "100%", src = "images.png")),
                             mainPanel(
                               h1("Rakenduse kirjeldus"),
                               p("Antud too eesmargiks on prognoosida maksimaalse hapniku tarbimisvoime (VO2max) mojutajaid ja leida ules head ja halvad jooksutreeningud. Antud masinoppe algoritmi eesmargiks on tulevikus luua rakendus, mis suudab eelnevate treeningute pohjal anda jargmise treeningu jaoks vajaliku sisendi sudameloogisageduse ja/voi kiiruse naol, et koormus oleks kehale voimalikult efektiivne. Eesmark on tootada valja algoritm ennustamaks jargmise treeningule sobivat koormust - sudameloogisagedust, kiirust ja treeningu tuupi. Analuusi jaoks kasutati r-studio rakenduses lineaarse regressioonimudelit, otsustuspuu mudelit ning segadus maatriksit toesuse kontrolliks."),
                               br(),
                               p("Tab 1: Andmete kirjeldus"),
                               p("Tab 2: Soltuvusdiagrammid - andmete visualiseerimise ja soltuvuse leidmiseks"),
                               p("Tab 3: Regressioonmudel - treeninguid enam mojutavate parameetrite leidmiseks"),
                               p("Tab 4: Otsustuspuu - heade trennide parameetrite leidmiseks"),
                               p("Tab 5: Confusion maatriks - tulemuste toesuse kontrolliks")
                             ) )),
                  tabPanel("Andmed",
                           mainPanel(h1("Andmed"),
                                     strong("Date"), span("- treeningu kuupaev"), br(),
                                     strong("TimeOfDay"), span("- treeningu kellaaeg"), br(),
                                     strong("distance_km"), span("- distants kilomeetrites"), br(),
                                     strong("Calories"), span("- kulutatud kalorite hulk"), br(),
                                     strong("time_m"), span("- treeningu kestvus minutites"), br(),
                                     strong("AVG_hr"), span("- treeningu keskmine pulsisagedus"), br(),
                                     strong("MAX_hr"), span("- treeningu maksimaalne pulsisagedus"), br(),
                                     strong("aerobic TE"), span("- treeningu aeroobne efektiivsus"), br(),
                                     strong("avg_cadence"), span("- keskmine jooksurutm"), br(),
                                     strong("max_cadence"), span("- maksimaalne jooksurutm"), br(),
                                     strong("avg_pace_m"), span("- treeningu keskmine kiirus minutites"), br(),
                                     strong("bestpace_m"), span("- treeningu maksimaalne kiirus minutites"), br(),
                                     strong("evel_gain"), span("- treeningu tousumeetrid"), br(),
                                     strong("elev_loss"), span("- treeningu langusmeetrid"), br(),
                                     strong("avg_steplenght"), span("- keskmine sammupikkus"), br(),
                                     strong("lap_nr"), span("- ringide arv treeningutel (tavaliselt 1km = 1 ring)"), br(),
                                     strong("sleep_m"), span("- uneaeg minutites"), br(),
                                     strong("vo2max"), span("- maksimaalne hapnikutarbimisevoime"), br(),
                                     dataTableOutput("tabel"))),
                  tabPanel("Soltuvusdiagrammid",
                           sidebarLayout(
                             sidebarPanel(selectInput("var",
                                                      label = "Vali treeningandmete soltuvus",
                                                      choices = names(Activities)[5:19]) ),
                             mainPanel(plotlyOutput("distPlot", width = "100%",height="auto"))      
                           )),
                  tabPanel("Regressiooni mudel",
                           mainPanel(
                             h1("Regressiooni mudel"),
                             p("Regressioonanaluus on tanapaevase andmeanaluusi valtimatu osa. Selle meetodi poole poordub uurija siis, kui on vaja vaadelda mitut tunnust korraga ja arvestada jareldustes nende omavahelisi seoseid. Viimane rida naitab meile, et mudel on statistiliselt oluline (p-vaartus on <2e-16 ehk <0.0000000000000016 ehk vaiksem kui 0.05, mida peetakse kokkuleppeliselt statistilise olulisuse lavendiks). p-vaartus naitab, et toenaosus, et saaksime sama aarmusliku statistiku vaartuse (mudeli puhul kasutatakse siin F-statistikut, tunnuste puhul t-statistikut), kui meie uuritava tunnuse ja uhegi seletava tunnuse vahel ei oleks populatsioonis seost ja koik oleks juhuslik, on vaiksem kui 0.0000000000000016 ehk 0.00000000000016 protsenti. Nonda lukkab see umber 0-hupoteesi, mille kohaselt tunnuste vahel ei ole seost, ja lubab meil vaita, et sona pikkus aitab ennustada aega, mis kulub sona aratundmiseks, ka valjaspool meie valimit"),
                             verbatimTextOutput("summary"))      
                  ),
                  tabPanel("Otsustuspuu mudel",
                           mainPanel(h1("Classification tree"),
                                     p("Otsusepuu tarvis genereerisin andmetesse dummy muutujad. Muutujatest taheldub, et kui vo2max vaartus on 58 voi korgem ning korgem voi vordne vorreldes eelmise treeninguga, siis treening on 'Good', kui alla selle vaartuse, siis 'Bad'"), br(), 
                                    p("Allpool oleval joonisel on naha, et vaga palju mojutab vo2max vaartust aeroobses tsoonis jooksmine, kiirus aeglasemalt voi vordselt 5,1min/km. Aeroobse treeningu intensiivsus peaks olema suurem voi vordne 2,6."),
                             img(height = "100%", width = "100%", src = "Rplot.png"),
                                     verbatimTextOutput("classificationPlot"))
                           ),
                  tabPanel("Segadus maatriks",
                           sidebarLayout(
                             sidebarPanel(img(src = "Picture1.png", height = 139, width = 324)),
                             mainPanel(h1("Segadus maatriks"),
                                       p("Et leida mudeli tapsust tuleb arvutada mudeli f-score, mis on harmooniline keskmine.Positiivne ennustatav vaartus voi tapsus arvutatakse jargnevalt:"),
                                       code("precision (tapsus)=TP/(TP+FP)"),
                                       p("Tundlikkus (recall) on oigesti klassifitseeritud juhtumite (toeliste positiivsete) maar koigi prognoositud klasside tegelike esinemiste suhtes. Tagasikutsumise arvutamine toimub valemiga jargmise valemiga:"),
                                       code("recall=TP/(TP+FN)"),
                                       p("Fscore on harmooniline keskmine tapsuse ja tundlikkuse vahel ja seda kalkuleeritakse jargmiselt:"), 
                                       code("precision=2TP/(2TP+FN+FP)"),
                                       p("Tulemuste esitamisel kasutatakse uhte f-skoori vaartust, mis on molema klassi f-skoori vaartuste keskmine."),
                                       verbatimTextOutput("summary2"),
                                       span("Prognoosi precision vaartus on"), strong("83,7 %"),br(),
                                       span("Prognoosi recall vaartus on"), strong(" 97,7 %"),br(),
                                       span("Prognoosi f-score vaartus on"), strong(" 90,1 %"))      
                           ))
                ) )

server <- function(input, output) {
  output$tabel <- renderDataTable(Activities,
                                  options =list(searching = TRUE,ordering=F, lengthMenu = c(50,100,200),
                                                pageLength = 20,scrollX = TRUE))  
  
  output$distPlot <- renderPlotly({
    p <- ggplot(Activities, aes_string(x = "date", y =input$var,
                                       colour = "training_type",nimi="vo2max")) +
      geom_smooth(data=Activities[,-(11:12)],aes_string(x = "date", y =input$var,nimi="vo2max"),
                  colour = "grey")+geom_point()+
      facet_wrap(~ vo2max)+
      theme(legend.position = "none")+
      scale_y_log10()+scale_x_log10()+
      scale_color_manual(values=c("#8B4513","#009FE3","#00983A","#FFDE00","#E30613","#82368C"))+
      xlab("vo2max")
    ggplotly(p,tooltip = c("vo2max",input$var,"training_type"))
  })
  
  output$summary <- renderPrint({
    summary(regression_results)
  })
  
  output$summary2 <- renderPrint({
    table_mat
  })
}

shinyApp(ui = ui, server = server)
