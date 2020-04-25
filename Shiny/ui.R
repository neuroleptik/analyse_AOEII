#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library("tibble")
library("jsonlite")
library("xml2")
library("rvest")
library(rmarkdown)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

##### Lecture du fichier #####
data_f <- read_csv("df.csv")

##### Conversion character -> facteur #####
data_f$Expansion <- as.factor(data_f$Expansion)
data_f$Age <- as.factor(data_f$Age)
data_f <- as_tibble(data_f)
data_f$Type_atk <- as.factor(data_f$Type_atk)


shinyUI(
    
    # creation d'un dashboard 
    dashboardPage(
        
        #en-tête 
        dashboardHeader(title = "Unit Presentation", titleWidth = 230),
        
        #sidebar / menu 
        dashboardSidebar(
            sidebarMenu(
                
                #permet à la liste de rester visible sur la page même apres le scroll
                style = "position:fixed;width:220px;",
                
                #ajout de la liste des unités 
                selectInput(
                    inputId = "unit",
                    label = "Unit: ",
                    choices = data_f$Name,
                    size = 13,
                    selectize = FALSE
                )
            ) 
        ),
        dashboardBody(

            # 3 infoBox sur la première ligne
            fluidRow(
                # A static infoBox
                infoBoxOutput("nameBox"),
                # Dynamic infoBoxes
                infoBoxOutput("expansionBox"),
                infoBoxOutput("ageBox")
            ),
            
            # 2e ligne : une box pour la description + type
            fluidRow(
                uiOutput("descriptionBox"),
                infoBoxOutput("typeBox")
               
            ),
            
            # 3e ligne : le type d'attaque
            fluidRow(
               # infoBoxOutput("typeBox")
            ),
            
            # 4e ligne : graphe "en arraigné" des statistiques
            fluidRow(
                plotOutput("spiderplot")
            ),
            
            # 5e lgine : graphe en bar des coûts
            fluidRow(
                plotOutput("costplot")
            )
            
            
            
        )
        
    )
    
)
