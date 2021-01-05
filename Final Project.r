######### Final Project #########
###### Dien Bao Tran Thai #######
### Naveen Narayana Peddyreddy ##
#### Ashish Salunkhe ############
######## Arvind Pawar ###########
####### Abhishek Nilajagi #######

#install.packages("partykit")
library(shiny) 
library(ggplot2)
library(dplyr)
library(directlabels)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(party)
library(partykit)
#setwd("C:/Users/navee/OneDrive/Desktop/communication visualization/week 4/week 5")
data <- read.csv("data.csv")


sumt <- data %>% group_by(AdoptionSpeed, Type) %>%  summarise(sum = sum(Quantity))
sumv <- data %>% group_by(AdoptionSpeed, Vaccinated) %>%  summarise(sum = sum(Quantity))
sums <- data %>% group_by(AdoptionSpeed, Sterilized) %>%  summarise(sum = sum(Quantity))
sumd <- data %>% group_by(AdoptionSpeed, Dewormed) %>%  summarise(sum = sum(Quantity))
fee <- data %>% group_by(AdoptionSpeed, Type) %>%  summarise(n = n(), mean.Fee = mean(Fee))

set.seed(1234)

#Splitting the trainset and testset
train_ind<-sample(2, nrow(data),replace = TRUE,prob = c(0.7,0.3))
trainset<-data[train_ind==1,] 
testset<-data[train_ind==2,] 

variables<-Type~Vaccinated+Sterilized+Health+AdoptionSpeed

#tree using train data

PetTree<-ctree(variables, data= trainset)
table(predict(PetTree),trainset$Type)

#tree using test data
PetTreeTest<-ctree(variables,data=testset)

ui<-dashboardPage(skin = "purple",
                  dashboardHeader(title = "Adopt a Pet today!"),
                  
                  
                  dashboardSidebar(disable = TRUE),
                   
                  
                  dashboardBody(
                    fluidPage(
                      fluidRow(
                        
                        mainPanel(width = 7,
                          tabsetPanel( 
                            tabPanel("Average Fee as per adoption speed", plotOutput("feePlot1")), 
                            tabPanel("The number of pet as per adoption speed", plotOutput("typePlot"))
                          )),
                      column(width = 5,
                             fluidRow(
                               hr(),
                               valueBox("FEE?", subtitle = tags$p("Generally dogs with high fee are adopted the fastest and cats with high fee are adopted the slowest", style = "font-size: 125%;"), width = 15, color = "blue")),
                             fluidRow(
                                hr(),
                               valueBox("Quantity of Adoption", subtitle = tags$p("It can be said that within the first month of listing, cats are adopted more and after the first month the quantity of the dog adoption increases", style = "font-size: 125%;"), width = 15, color = "green")
                             ))),
                      hr(),
                      
                      fluidRow(
                        sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons(inputId = "outputType",
                                       label = "Medical Condition of Pet",
                                       choices = list("Vaccination" = "Vaccinated",
                                                      "Sterilized" = "Sterilized",
                                                      "Dewormed" = "Dewormed")
                                       )
                        ),
                        mainPanel(width = 8, plotOutput("threeGraph"))
                        )),
                      hr(),
                      column(width = 12,
                      fluidRow(
                        
                        mainPanel(width = 16, plotOutput("a"))
                        ),
                      hr(),
                      fluidRow(
                        mainPanel(width = 16, plotOutput("b"))
                      )
                    ))
))                  



 


server <- function(input, output) {

    output$threeGraph <- renderPlot({
      
      
      if(input$outputType == "Vaccinated"){
        theGraph <- ggplot() +
          geom_line(data = sumv,aes(x = AdoptionSpeed, y = sum, group = Vaccinated, colour = Vaccinated), size = 2) +  
          geom_text(data=subset(sumv, AdoptionSpeed =="4"), aes(label=Vaccinated, colour=Vaccinated, x=AdoptionSpeed, y=sum), hjust = 2, vjust = 0.1, size = 8)+
          ylab("Quantity")
       
       
      }
        
        
      if(input$outputType == "Sterilized"){
        
        theGraph <- ggplot() +
          geom_line(data = sums,aes(x = AdoptionSpeed, y = sum, group = Sterilized, colour = Sterilized), size = 2) +
        geom_text(data=subset(sums, AdoptionSpeed =="4"), aes(label=Sterilized, colour=Sterilized, x=AdoptionSpeed, y=sum), hjust = -0.1)+
        ylab("Quantity") 
      }
      if(input$outputType == "Dewormed"){
        
        theGraph <- ggplot() +
          geom_line(data = sumd,aes(x = AdoptionSpeed, y = sum, group = Dewormed, colour = Dewormed), size = 2) +
        geom_text(data=subset(sumd, AdoptionSpeed =="4"), aes(label=Dewormed, colour=Dewormed, x=AdoptionSpeed, y=sum), hjust=1, vjust=-0.1, size = 8)+
        ylab("Quantity")
      }
      direct.label(theGraph,list("last.points", hjust =0.7, vjust =1))
      print(theGraph+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "None"))
    })
    
    
    output$feePlot1 <- renderPlot({
      
    feeGraph <- ggplot(data=fee, aes(x= AdoptionSpeed, y= mean.Fee,fill=Type),xlab = "Type", ylab ="Average Fee") + 
        geom_bar(stat="identity", position=position_dodge())
    feeGraph + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))
    })
    output$typePlot <- renderPlot({
      typeGraph <- ggplot() +
        geom_line(data = sumt,aes(x = AdoptionSpeed, y = sum, group = Type, colour = Type),size=2) +  
        geom_text(data=subset(sumt, AdoptionSpeed =="4"), aes(label=Type, colour=Type, x=AdoptionSpeed, y=sum), hjust=-0.1, size = 8)+
        ylab("Quantity")
      typeGraph + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "None")
      
    })
    output$a <- renderPlot({
      a <- plot(PetTree, main="Adoption Speed tree from Train Set")
    })
    output$b <- renderPlot({
      b <- plot(PetTreeTest,main="Adoption Speed tree from Test Set")
    })
    
}


shinyApp(ui = ui, server = server)
    