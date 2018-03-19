library(shiny)
library(gridExtra)

#EYC_Data_Filepath<-"../Outputs/SFR_EYFSP_SFR29_2017_subset.csv"
#EYFSP_Data_Filepath<-"../Outputs/SFR_EYFSP_SFR60_2017_subset.csv"


#EYC_Data <- read_csv(EYC_Data_Filepath)
#EYFSP_Data <- read_csv(EYFSP_Data_Filepath)

 # Define UI for random distribution app ----
source("global.R")
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      ##added
      # Combine the selected variables into a new data frame
      selectInput("Local_Authority", "LA:", 
                  choices=EYFSP_Data$LA_name),
      hr(),
      helpText("Data from DfE."),
      ## end of added
      hr(),
      
      
      # Input: Select the random distribution type ----
      radioButtons("graph_choice", "Distribution type:",
                   c("Within LA Gap Between FSM and All Other" = "rwithin_LA",
                     "GLD Gap with FSM in LA and FSM Nat Av" = "rFSM_Gap_with_Nat",
                     "GLD Gap with All Other in LA and All Other" = "rAll_Other_Gap_with_Nat",
                     "Percentage Take Up for Two Year olds" = "rTwo_YO_Take_Up"
                     ,"Percentage Take Up for Three and Four Year olds" = "rThree_and_Four_YO_Take_Up"
                     )
                   
                   ),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Number of observations:",
                  value = 8,
                  min = 1,
                  max = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Plot Take Up", plotOutput("plot_take_up")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  
  #this part will be replaced by stats neighbours
  random_sample<- reactive({
  n <- input$n 
  random_sample <- sample(152,input$n) %>% as.integer() #hope to replace 7 with n
  #selected_LA_name <- "Bristol, City Of" #input$choices
  #random_sample<-c(selected_LA_FSM_GAP_Rank,random_sample()) %>% sort()
   random_sample
  })
  d <- reactive({
    graph_choice <- input$graph_choice
      
                 
    
    selected_LA_name <- input$choices#"Bristol, City Of" #
   # n <- input$n 
    #random_sample <- sample(152,input$n) %>% as.integer() #hope to replace 7 with n
    
    #renderPrint(selected_LA_name)
    
    #ifelse(graph_choice=="rFSM_Gap_with_Nat") 
   # {
      selected_LA_FSM_GAP_Rank <-  filter(EYFSP_Data,LA_name==input$Local_Authority) %>%
      select(rank_FSM_Nat_Av_Gap) %>% as.integer()
      random_sample<- c(selected_LA_FSM_GAP_Rank,random_sample()) %>% sort()
      #EYFSP_selection <- 
        EYFSP_Data %>% filter(rank_FSM_Nat_Av_Gap%in%random_sample) %>% arrange(rank_FSM_Nat_Av_Gap)
        EYFSP_selection <- EYFSP_Data %>% filter(rank_FSM_Nat_Av_Gap%in%random_sample) %>% arrange(rank_FSM_Nat_Av_Gap)
        EYFSP_selection<-EYFSP_selection[order(EYFSP_selection$FSM_Nat_Av_Gap), ]  # sort
        EYFSP_selection$LA_name<-factor(EYFSP_selection$LA_name, levels = EYFSP_selection$LA_name) #convert to factor to preserve sort order
        EYFSP_selection<- EYFSP_selection %>% mutate(to_highlight= ifelse( LA_name==input$Local_Authority, "yes", "no" ) )
        EYFSP_selection<- EYFSP_selection %>% mutate(Flag_FSM_Nat_Av_Gap=ifelse(LA_name==input$Local_Authority&Flag_FSM_Nat_Av_Gap=="above","selected LA above",
                                                                                ifelse(LA_name==input$Local_Authority&Flag_FSM_Nat_Av_Gap=="below","selected LA below",Flag_FSM_Nat_Av_Gap)))
        EYFSP_selection
   })
  
  all_other_plot <- reactive({
    graph_choice <- input$graph_choice
    
    
     
    selected_LA_All_Other_GAP_Rank <-  filter(EYFSP_Data,LA_name==input$Local_Authority) %>%
                                        select(rank_All_Other_Nat_Av_Gap) %>% as.integer()
    random_sample<- c(selected_LA_All_Other_GAP_Rank,random_sample()) %>% sort()
    #EYFSP_selection <- 
    EYFSP_Data %>% filter(rank_All_Other_Nat_Av_Gap%in%random_sample) %>% arrange(rank_All_Other_Nat_Av_Gap)
    EYFSP_selection <- EYFSP_Data %>% filter(rank_All_Other_Nat_Av_Gap%in%random_sample) %>% arrange(rank_All_Other_Nat_Av_Gap)
    EYFSP_selection<-EYFSP_selection[order(EYFSP_selection$All_Other_Nat_Av_Gap), ]  # sort
    EYFSP_selection$LA_name<-factor(EYFSP_selection$LA_name, levels = EYFSP_selection$LA_name) #convert to factor to preserve sort order
    EYFSP_selection<- EYFSP_selection %>% mutate(to_highlight= ifelse( LA_name==input$Local_Authority, "yes", "no" ) )

    EYFSP_selection 
  })
  
  selected_LA_only <- reactive({
    graph_choice <- input$graph_choice
    EYFSP_selected_LA <-  filter(EYFSP_Data,LA_name==input$Local_Authority) 
    EYFSP_selected_LA 
  })
  
  
  
  selected_LA_2YO_Takeup <- reactive({
    graph_choice <- input$graph_choice
    EYFSP_selected_LA <-  filter(SFR_EYFSP_Percent_2YOs_subset,LAName==input$Local_Authority) 
    EYFSP_selected_LA 
  })
  
  #grid.arrange(
  
  output$plot <- renderPlot({
    
    
    p1<-ggplot(d(), aes(x=LA_name, y=FSM_Nat_Av_Gap, label=FSM_Nat_Av_Gap)) +
      geom_bar(stat='identity', aes(fill=Flag_FSM_Nat_Av_Gap), width=.5)  +
      scale_fill_manual(name="FSM_Gap_with_National_average",
                        labels = c("Above Average", "Below Average","Selected LA"),
                        values = c("above"="#00ba38", "below"="#f8766d","selected LA above"="green4","selected LA below"="red4")) +
      labs(subtitle="Gap of Local Authority FSM with National Average FSM",
           title= "Diverging Bars") +
      coord_flip()
      
    
     # p1<- p1+
     #   geom_bar(data=selected_LA_only(),stat='identity', aes(fill=Flag_FSM_Nat_Av_Gap), width=.5,alpha=1,size=1,color="black")  +
     #   scale_fill_manual(name="FSM_Gap_with_National_average",
     #                     labels = c("Above Average", "Below Average"),
     #                     values = c("above"="#00ba38", "below"="#f8766d")) +
     #   labs(subtitle="Gap of Local Authority FSM with National Average FSM",
     #        title= "Diverging Bars") +
     #   coord_flip()

    
    p2<- ggplot(all_other_plot(), aes(x=LA_name, y=All_Other_Nat_Av_Gap, label=All_Other_Nat_Av_Gap)) +
      geom_bar(stat='identity', aes(fill=Flag_All_Other_Nat_Av_Gap), width=.5)  +
      scale_fill_manual(name="All_Other_Gap_with_National_average",
                        labels = c("Above Average", "Below Average"),
                        values = c("above"="#00ba38", "below"="#f8766d","selected LA above"="green3","selected LA below"="red")) +
      labs(subtitle="Gap of Local Authority All Other with National Average All Other",
           title= "Diverging Bars") +
      coord_flip()
    
    # p2<- p2+
    #   geom_bar(data=selected_LA_only(),stat='identity', aes(fill=Flag_All_Other_Nat_Av_Gap), width=.5,alpha=1,size=1,color="black")  +
    #   scale_fill_manual(name="All_Other_Gap_with_National_average",
    #                     labels = c("Above Average", "Below Average"),
    #                     values = c("above"="#00ba38", "below"="#f8766d","selected LA above"="green3","selected LA below"="red")) +
    #   labs(subtitle="Gap of Local Authority FSM with National Average FSM",
    #        title= "Diverging Bars") +
    #   coord_flip()
    
    grid.arrange(p1,p2, ncol=1,nrow=2,heights = c(2,2))
  }) 
  
  #, #end of firt plot in grid arrangge
  
  output$plot_take_up <- renderPlot(
    ggplot(selected_LA_2YO_Takeup(),aes(x=LAName,y=`2YOPercentInFundedEd2017`, label="2 YO Take Up"))+geom_bar(stat='identity', width=0.1)
    
  )
  
#  )# end of second plot in grid.arrange
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
