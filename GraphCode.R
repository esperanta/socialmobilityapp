# INFORMATION -------------------------------------------------------------
# Description:        Create Graphs to be used in Shiny
# Author:             Teresa Loftus
# Date created:       04/03/2017       
# Date last modified: 

library(tidyverse)
--install.packages("readxl")
--install.packages("data.table")
library(readxl)
library(data.table)
-
  
  install.packages("gridExtra")
  
  EYC_Data_Filepath<-"./Outputs/SFR_EYFSP_SFR29_2017_subset.csv"
  EYFSP_Data_Filepath<-"./Outputs/SFR_EYFSP_SFR60_2017_subset.csv"
  
  EYC_Data <- read_csv(EYC_Data_Filepath)
  EYFSP_Data <- read_csv(EYFSP_Data_Filepath)
  
  
  #simulate a list of 8 national ranks
  
    random_sample <- sample(152,7) %>% as.integer()
    
    #up to here in GlobalFile
    
   # Bristol <- "E06000023"
    #Bristol_FSM_GAP_Rank <-  filter(EYFSP_Data,LA_code==Bristol) %>%
                                 select(rank_FSM_Nat_Av_Gap) %>% as.integer()
   
    #random_sample<- c(Bristol_FSM_GAP_Rank,random_sample) %>% sort()
    
    
    #test start
    selected_LA_name <- "Bristol, City of"
    selected_LA_FSM_GAP_Rank <-  filter(EYFSP_Data,LA_name==Bristolname) %>%
      select(rank_FSM_Nat_Av_Gap) %>% as.integer()
    
    random_sample<- c(selected_LA_FSM_GAP_Rank,random_sample) %>% sort()
      
    
    #test end
    
  
  EYFSP_selection <- EYFSP_Data %>% filter(rank_FSM_Nat_Av_Gap%in%random_sample) %>% arrange(rank_FSM_Nat_Av_Gap)
  
  EYFSP_selection <- EYFSP_selection[order(EYFSP_selection$FSM_Nat_Av_Gap), ]  # sort
  
  EYFSP_selection$LA_name <- factor(EYFSP_selection$LA_name, levels = EYFSP_selection$LA_name) #convert to factor to preserve sort order
    # Diverging Graph for FSM
    ggplot(EYFSP_selection, aes(x=LA_name, y=FSM_Nat_Av_Gap, label=FSM_Nat_Av_Gap)) + 
      geom_bar(stat='identity', aes(fill=Flag_FSM_Nat_Av_Gap), width=.5)  +
      scale_fill_manual(name="FSM_Gap_with_National_average", 
                        labels = c("Above Average", "Below Average"), 
                        values = c("above"="#00ba38", "below"="#f8766d")) + 
      labs(subtitle="Gap of Local Authority FSM with National Average FSM", 
           title= "Diverging Bars") + 
      coord_flip()
    
  EYFSP_selection <- EYFSP_selection[order(EYFSP_selection$All_Other_Nat_Av_Gap), ]  # sort
  EYFSP_selection$LA_name <- factor(EYFSP_selection$LA_name, levels = EYFSP_selection$LA_name) #convert to factor to preserve sort order
  # Diverging Graph for FSM
  
  ggplot(EYFSP_selection, aes(x=LA_name, y=All_Other_Nat_Av_Gap, label=All_Other_Nat_Av_Gap)) + 
    geom_bar(stat='identity', aes(fill=Flag_All_Other_Nat_Av_Gap), width=.5)  +
    scale_fill_manual(name="All_Other_Gap_with_National_average", 
                      labels = c("Above Average", "Below Average"), 
                      values = c("above"="#00ba38", "below"="#f8766d")) + 
    labs(subtitle="Gap of Local Authority All Other with National Average All Other", 
         title= "Diverging Bars") + 
    coord_flip()
  
  #Within LA Gap
  EYFSP_Selection_Within_LA<-EYFSP_selection%>% select(LA_name,'%FSM_GLD',`%All_Other_GLD`,`within_LA_GAP`)
  
  EYFSP_selection_Nat<- EYFSP_selection %>% select(`%All_Other_GLD_Nat`,`%FSM_GLD_Nat`) %>%
                                                        mutate(within_LA_Gap_Nat=`%All_Other_GLD_Nat` - `%FSM_GLD_Nat`)
                                                        
  EYFSP_selection_Nat<- EYFSP_selection_Nat*100 
  EYFSP_selection_Nat<-EYFSP_selection_Nat[1,] %>% as.tibble() %>% mutate(LA_Name="ENGLAND") 
  EYFSP_selection_Nat<-EYFSP_selection_Nat[,c(4,2,1,3)]
  colnames(EYFSP_selection_Nat)<-c("LA_name","%FSM_GLD","%All_Other_GLD","within_LA_GAP")
 
  
  EYFSP_Selection_Within_LA<-rbind(EYFSP_Selection_Within_LA,EYFSP_selection_Nat)
  unwanted_column <- "%All_Other_GLD"
  EYFSP_Selection_Within_LA<-EYFSP_Selection_Within_LA[,-3]
  
  EYFSP_Selection_Within_LA_graph_step1<-  EYFSP_Selection_Within_LA%>% group_by(LA_name) %>% slice(1)
  
  EYFSP_Selection_Within_LA_graph_step2<- gather(EYFSP_Selection_Within_LA_graph_step1
                                                 ,key=All_other,value = "Percent GLD",-LA_name)
  
  #delete in a min
  EYFSP_Selection_Within_LA_graph_step2 <- EYFSP_Selection_Within_LA_graph_step2[order( EYFSP_Selection_Within_LA_graph_step2$`Percent GLD`,EYFSP_Selection_Within_LA_graph_step2$`All_other`,decreasing = FALSE), ]  # sort
  #EYFSP_Selection_Within_LA_graph_step2
  EYFSP_Selection_Within_LA_graph_step2$LA_name <- factor(EYFSP_Selection_Within_LA_graph_step2$LA_name, levels = EYFSP_Selection_Within_LA_graph_step2$LA_name) 
  #end delete in a min
  theme_set(theme_light())
  
  # Histogram on a Categorical variable
    g <- ggplot(EYFSP_Selection_Within_LA_graph_step2
                ,aes(x=LA_name,y=`Percent GLD`)
                ,fill=All_other) +
          geom_col(aes(fill=All_other),width = 0.8)#
  g + coord_flip()
  
   # Take Up Graphs
  
  EYC_Data_Subset<- EYC_Data%>% filter(LA==801|is.na(LA)) #only NA value is England
  
  #Take up graph 2 yo
  
  EYC_Data_Subset_2YOs<- EYC_Data_Subset[c(2,4:6)]
  
  EYC_Data_Subset_2YOs_Graph<-
    EYC_Data_Subset_2YOs %>%
     group_by(LAName) %>%
      slice(1)
  EYC_Data_Subset_2YOs_Graph_step2<- gather(EYC_Data_Subset_2YOs_Graph,key=year,value=take_up,-LAName)
  EYC_Data_Subset_2YOs_Graph_step3<-mutate(EYC_Data_Subset_2YOs_Graph_step2,year=right(year,4))
  
  ggplot(data=EYC_Data_Subset_2YOs_Graph_step3, aes(y=take_up, x=year, group=LAName, colour=LAName)) +
    geom_line() +
    geom_point()
  
  
  right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
  }
  
  
  #Take up graph 3 and 4 yo 
  
  EYC_Data_Subset_3and4YOs<- EYC_Data_Subset[c(2,8:10)]
  
  EYC_Data_Subset_3and4YOs_Graph<-
    EYC_Data_Subset_3and4YOs %>%
    group_by(LAName) %>%
    slice(1)
  EYC_Data_Subset_3and4YOs_Graph_step2<- gather(EYC_Data_Subset_3and4YOs_Graph,key=year,value=take_up,-LAName)
  EYC_Data_Subset_3and4YOs_Graph_step3<-mutate(EYC_Data_Subset_3and4YOs_Graph_step2,year=right(year,4))
  
  ggplot(data=EYC_Data_Subset_3and4YOs_Graph_step3, aes(y=take_up, x=year, group=LAName, colour=LAName)) +
    geom_line() +
    geom_point()
  
  
  right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
  }
  