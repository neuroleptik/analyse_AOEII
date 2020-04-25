#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("tibble")
library("jsonlite")
library("xml2")
library("rvest")
library(rmarkdown)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(fmsb)

#Chargement du fichier -
data_f <- read_csv("df.csv")

#Conversion en tibble 
data_f <- as_tibble(data_f)

# Conversion character -> facteur #
data_f$Expansion <- as.factor(data_f$Expansion)
data_f$Age <- as.factor(data_f$Age)
data_f$Type_atk <- as.factor(data_f$Type_atk)

#Copie du dataset mais seulement avec les colonnes statistiques des unités
stats <- select(data_f,Name, Attack_delay, Accuracy,HP, Attack,Range , Line_of_Sight, Movement_rate, MeleeArmor,PierceArmor,Reload_time )

# vecteurs avec le minimum par stat ( utile pour le spiderplot)
mins <- c( min(data_f$Attack_delay,na.rm= TRUE),
           min(data_f$Accuracy,na.rm= TRUE),
           min(data_f$HP,na.rm= TRUE),
           min(data_f$Attack,na.rm= TRUE),
           min(data_f$Range,na.rm= TRUE),
           min(data_f$Line_of_Sight,na.rm= TRUE),
           min(data_f$Movement_rate,na.rm= TRUE),
           min(data_f$MeleeArmor,na.rm= FALSE),
           min(data_f$PierceArmor,na.rm= FALSE),
           min(data_f$Reload_time,na.rm= TRUE)
) 

# vecteurs avec le maximum par stat ( utile pour le spiderplot)
#saisie des max manuellement car les valeurs sont bcp trop dispersé ( on passe de 8 & 190 pour le pierce armor par exemple)
maxs <- c( max(data_f$Attack_delay,na.rm= TRUE),
           max(data_f$Accuracy,na.rm= TRUE),
           200,
           25,
           max(data_f$Range,na.rm= TRUE),
           max(data_f$Line_of_Sight,na.rm= TRUE),
           max(data_f$Movement_rate,na.rm= TRUE),
           6,
           8,
           max(data_f$Reload_time,na.rm= TRUE)
) 


# Define server logic required to draw graph
shinyServer(function(input, output) {
    
    #output 1 : infoBox pour afficher le nom
    output$nameBox <- renderInfoBox({
        select_row <- filter(data_f, Name==input$unit)
        infoBox(
            "Name", select_row$Name, icon = icon("user"),
            color = "red", fill = TRUE
        )
    })
    
   #output 2 : infoBox pour afficher l'expansion
    output$expansionBox <- renderInfoBox({
        select_row <- filter(data_f, Name==input$unit)
        infoBox(
            "Expansion", select_row$Expansion, icon = icon("star"),
            color = "orange", fill = TRUE
        )
    })
    
    #output 3 : infoBox pour afficher l'age
    output$ageBox <- renderInfoBox({
        select_row <- filter(data_f, Name==input$unit)
        infoBox(
            "Age", select_row$Age, icon = icon("chess"),
            color = "yellow", fill = TRUE
        )
    })
    
    #output 3 : infoBox pour afficher le type
    output$typeBox <- renderInfoBox({
        select_row <- filter(data_f, Name==input$unit)
        infoBox(
            "Type Attack", select_row$Type_atk, icon = icon("expand-arrows-alt"),
            color = "blue", fill = TRUE
        )
    })
    
    #output 4 : infoBox pour afficher la description
    output$descriptionBox <-renderUI({
        select_row <- filter(data_f, Name==input$unit)
        infoBox(
            "Description", select_row$Description, icon = icon("list"),
            color = "green", fill = TRUE, width = 8
        )
    })
    
    #output 5 : spiderplot pour afficher les statistiques des unités
    output$spiderplot <- renderPlot({
        
        #selection de l'unité choisi par l'user
        stat <- filter(stats, Name==input$unit)
        stat <- select(stat, -Name)
        
        #creation d'un dataframe avec le min max des colonnes et les stats de l'unité
        s_data <- rbind(maxs,mins, stat)
        
        #boucle pour baisser les stats trop elevée 
        #certains unités ont des stas trop élevés comparé a la moyenne, c'est pourquoi je les baisse au maximum du grph
        cpt = 1
        for(i in select(s_data, -Reload_time)){
          if(!is.na(i[3])){
            if(i[3] > i[1]){
              s_data[3,cpt] <- i[1]
            }
          }
          cpt <- cpt + 1
        }
        
        #création de la legende 
        #LOW rouge, DECENT orange, GOOD jaune, HIGH vert, BEST vert foncé
        comment <- c() 
        colors <- c()
        for(i in select(s_data, -Reload_time)){
          if(!is.na(i[3])){
            
            #max
            if(i[3] == i[1] ){
              comment = c(comment,"( EXCELLENT )")
              colors = c(colors,"#b29600")
            }
            #min
            else if(i[3] == i[2]){
              comment = c(comment,"( LOW )")
              colors = c(colors,"#e56565")
            }
            
            else{
              pourcentage = (i[3] * 100) / i[1]
              
              if( pourcentage < 30){
                comment = c(comment,"( DECENT ) ")
                colors = c(colors,"#f8bc4c")
              }
              else if( pourcentage < 70){
                comment = c(comment,"( GOOD )")
                colors = c(colors,"#ddd75a")
              }
              else{
                comment = c(comment,"( HIGH )")
                colors = c(colors,"#75b200")
              }
              
            }
            
          }else{
            #si c'est NA
            comment = c(comment," ")
            colors = c(colors,"black")
          }
         
        }
        
        #RELOAD TIME A FAIRE UN RELOAD TIME FAIBLE EST BIEN ET INVERSEMENT
        #BAD / GOOD 
        if(!is.na(s_data$Reload_time[3])){
          
          if(s_data$Reload_time[3] < s_data$Reload_time[1] / 2){
            comment = c(comment,"( GOOD )")
            colors = c(colors,"#ddd75a")
          }else{
            comment = c(comment,"( DECENT )")
            colors = c(colors,"#f8bc4c")
          }
          
        }else{
          #si c'est NA
          comment = c(comment," ")
          colors = c(colors,"black")
        }
        
        
        l1 <- c(paste(str_replace_all(colnames(select(stats, -Name)), "_"," ")  , stat, sep= " : "))
        
        legends <- c(paste(l1," ",comment, "") )
        
        #plot 
        radarchart(as.data.frame(s_data), 
                   title = "Battle statistics", 
                   #custom polygon
                   pcol="black" , 
                   pfcol="red" , 
                   plwd=4 , 
                   #custom the grid
                   cglcol="blue", 
                   cglty=1, 
                   axislabcol="blue", 
                   caxislabels=seq(0,20,5), 
                   cglwd=0.8,)
        
        #ajout de la légende
        
        legend(x=-5, y=1, legend = legends, bty = "n", pch=20  , text.col = colors, title = "Unit's stats")
  })
    
    ##output 6 : bar pour afficher le coût en ressource de l'unité
    output$costplot <- renderPlot({
        
        #selection de l'unité choisi par l'user
        t <- filter(data_f, Name == input$unit)
        
        #création d'un dataframe avec les noms des ressourcse et leurs valeurs
        data <- data.frame(
            name=c("Wood","Gold","Food","Built_time") ,  
            value=c(t$Cost_wood,t$Cost_gold,t$Cost_food,t$Build_time)
        )
        
        #plot du diagramme en bar
        ggplot(data, aes(x=name, y=value, fill = name)) + 
            geom_bar(stat = "identity", width=1,show.legend = FALSE) + 
            #mettre en horizontale les bars
            coord_flip() + 
            #choix des limites du graphes 
            scale_y_continuous( limits=c(0,250) ) + 
            #colors
            scale_fill_manual(values = c("blue", "red", "yellow","brown") ) + 
            #customiz title
            theme(legend.position="none",
                  plot.title = element_text(color="black", size=14, face="bold")
                  ) +
            #ajout d'un theme pour changer le fond
            theme_bw() + 
            #labels 
            labs(y = "", x = "Ressource", title = "Unit's cost") 
        
    })
    

})
