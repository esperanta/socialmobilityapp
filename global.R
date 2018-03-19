

# INFORMATION -------------------------------------------------------------
# Description:        Gather 2 Year Old Data for Publication
# Author:             Teresa Loftus
# Date created:       27/02/2017       
# Date last modified: 

library(tidyverse)
#--install.packages("readxl")
#--install.packages("data.table")
library(readxl)
library(data.table)
 
  
 # setwd("C:/Users/lucia/Documents/Work/R/LAScorecards/R/SocialMobilityApp")
  #This file creates the excerpt used for publication and makes it reproducible, by recording in excel
  
  #data_cut<- read_excel("./Data/SFR29-2017_MainTables.xlsx") 
  relative_fp <- "data/SFR29-2017_MainTables.xlsx" #internally this works
  #relative_fp <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/684240/SFR29-2017_MainTables.xlsx" #externally this works
  #fp <- "//lonnetapp01/asddata/EY-Schools-SEND//EYARU/EY Policy-Quality outcomes and providers/Social Mobility/LA Scorecards/SocialMobDBPub/Data/SFR29-2017_MainTables.xlsx"
  #data_cut <- read_excel(fp)
  SFR_EYFSP_Number_2YOs <- read_excel(relative_fp, sheet="Table 1LA", range = "B8:Q182")
  SFR_EYFSP_Percent_2YOs <- read_excel(relative_fp, sheet="Table 5LA", range = "B8:F182")
  
  
  
  
  SFR_EYFSP_Number_3YOand4YOs <- read_excel(relative_fp, sheet="Table 2LA", range = "B8:Q182")
  SFR_EYFSP_Percent_3YOand4YOs <- read_excel(relative_fp, sheet="Table 5LA", range = "B8:AD182")
  
  
  
  #Two Year Olds
  SFR_EYFSP_Number_2YOs_subset<-SFR_EYFSP_Number_2YOs[,c(1,2,16)]  
  colnames(SFR_EYFSP_Number_2YOs_subset) <- c("LA","LAName","Number2YOInFundedEduc")
  
  #Note, we filter out where the LA is NULL, this also removed region and England result, but not needed at teh moment
  SFR_EYFSP_Number_2YOs_subset<-SFR_EYFSP_Number_2YOs_subset %>% filter(!is.na(LA)|LAName=="ENGLAND")
  
  SFR_EYFSP_Percent_2YOs_subset <- SFR_EYFSP_Percent_2YOs[,c(1:5)]
  
  colnames(SFR_EYFSP_Percent_2YOs_subset) <- c("LA","LAName","2YOPercentInFundedEd2015","2YOPercentInFundedEd2016","2YOPercentInFundedEd2017")
  #Note, we filter out where the LA is NULL, this also removed region and England result, but not needed at the moment
  SFR_EYFSP_Percent_2YOs_subset<-SFR_EYFSP_Percent_2YOs_subset %>% filter(!is.na(LA)|LAName=="ENGLAND")
  
  #Three and Four Year Olds
  SFR_EYFSP_Number_3YOand4YOs_subset<-SFR_EYFSP_Number_3YOand4YOs[,c(1:2,16)]  
  colnames(SFR_EYFSP_Number_3YOand4YOs_subset) <- c("LA","LAName","Number3and4YearOldsInFundedEduc")
  
  #Note, we filter out where the LA is NULL, this also removed region and England result, but not needed at teh moment
  SFR_EYFSP_Number_3YOand4YOs_subset<-SFR_EYFSP_Number_3YOand4YOs_subset %>% filter(!is.na(LA)|LAName=="ENGLAND")
  
  SFR_EYFSP_Percent_3YOand4YOs_subset <- SFR_EYFSP_Percent_3YOand4YOs[,c(1:2,27:29)]
  colnames(SFR_EYFSP_Percent_3YOand4YOs_subset) <- c("LA","LAName","3and4PercentInFundedEd2015","3and4PercentInFundedEd2016","3and4PercentInFundedEd2017")
  SFR_EYFSP_Percent_3YOand4YOs_subset<-SFR_EYFSP_Percent_3YOand4YOs_subset %>% filter(!is.na(LA)|LAName=="ENGLAND")
  
  
  
  
  
  
  SFR_EYFSP_SFR29_2017<-left_join(SFR_EYFSP_Number_2YOs_subset,SFR_EYFSP_Percent_2YOs_subset, by="LAName")%>% 
    left_join(SFR_EYFSP_Number_3YOand4YOs_subset,by="LAName")%>%
    left_join(SFR_EYFSP_Percent_3YOand4YOs_subset,by="LAName")%>%
    arrange(LAName)
  drop_redundant_cols <- c("LA.y","LA.x.x","LA.y.y")
  SFR_EYFSP_SFR29_2017<- SFR_EYFSP_SFR29_2017[,!names(SFR_EYFSP_SFR29_2017)%in%drop_redundant_cols] %>%
    plyr::rename(c("LA.x"="LA"))
  
  fwrite(SFR_EYFSP_SFR29_2017,"data/SFR_EYFSP_SFR29_2017_subset.csv")
  
  ##***************************************************************************************************
  #Part Two
  #***************************************************************************************************
  
  #compile data from the EYFSP results 2017.
  #Data required here will be
  #--FSM
  #--Non FSM % GLD
  #-- Gap between FSM and Non FSM
  #-- Gap Between FSM and National Average
  #-- Gap between non FSM and non FSM National Average
  # National equivalent of all the above measures
  # Rank for all measures
  
  
  EYFSP_filepath <- "data/SFR60_2017_UD_Additional_Tables/SFR60_2017_UD_LA_additional tables.csv"
  
  EYFSP_Underlying_Data <- read_csv(EYFSP_filepath, skip = 0)
  
  EYFSP_Dataset_step1 <- EYFSP_Underlying_Data %>%
    select(Country_code,	Country_name,Region_code,	Region_name,	LA_code,	LA_name, ELIG_all_17, GOODLEV_all_17,
           ELIG_all_FSM_17, GOODLEV_all_FSM_17, ELIG_all_Allother_17, GOODLEV_all_Allother_17)
  
  # Make sure R realises that these are numbers- could have done this more efficiently with mutate_if
  EYFSP_Dataset_step1$GOODLEV_all_17          <- as.numeric(EYFSP_Dataset_step1$GOODLEV_all_17)
  EYFSP_Dataset_step1$ELIG_all_17             <- as.numeric(EYFSP_Dataset_step1$ELIG_all_17)
  EYFSP_Dataset_step1$GOODLEV_all_FSM_17      <- as.numeric(EYFSP_Dataset_step1$GOODLEV_all_FSM_17)
  EYFSP_Dataset_step1$ELIG_all_FSM_17         <- as.numeric(EYFSP_Dataset_step1$ELIG_all_FSM_17) 
  EYFSP_Dataset_step1$GOODLEV_all_Allother_17 <- as.numeric(EYFSP_Dataset_step1$GOODLEV_all_Allother_17)
  EYFSP_Dataset_step1$ELIG_all_Allother_17     <-as.numeric(EYFSP_Dataset_step1$ELIG_all_Allother_17)
  
  
  EYFSP_Dataset_step2 <-EYFSP_Dataset_step1 %>% mutate("%All_GLD"=GOODLEV_all_17/ELIG_all_17
                                                       ,"%FSM_GLD"=GOODLEV_all_FSM_17/ELIG_all_FSM_17
                                                       ,"%All_Other_GLD"=GOODLEV_all_Allother_17/ELIG_all_Allother_17)
  
  #use plyr as this allows renaming a few columns, not all
  EYFSP_Dataset_National <- filter(EYFSP_Dataset_step2,is.na(Region_code)) %>%
    plyr::rename(c("%All_GLD"="%All_GLD_Nat","%FSM_GLD"="%FSM_GLD_Nat", "%All_Other_GLD" ="%All_Other_GLD_Nat"))
  
  #Add national columns for ease of further calculations
  EYFSP_Dataset_step3 <- left_join(EYFSP_Dataset_step2,select(EYFSP_Dataset_National,c("Country_code","%All_GLD_Nat","%FSM_GLD_Nat","%All_Other_GLD_Nat")))
  
  
  EYFSP_Dataset_LA_Step1 <- filter(EYFSP_Dataset_step3,!is.na(LA_code)) %>% 
    mutate(within_LA_GAP = `%All_Other_GLD` - `%FSM_GLD`
           ,FSM_Nat_Av_Gap =`%FSM_GLD`-`%FSM_GLD_Nat`
           ,All_Other_Nat_Av_Gap = `%All_Other_GLD` -`%All_Other_GLD_Nat`)%>%#note to self- never start a variable name with a percentage ever again!
    #the next step is for diverging graphs, want to have below and above in separate columns   
    mutate("Below_FSM_Nat_Av_Gap"=ifelse((FSM_Nat_Av_Gap<0),FSM_Nat_Av_Gap,0)
           ,"Above_FSM_Nat_Av_Gap"=ifelse(FSM_Nat_Av_Gap>0,FSM_Nat_Av_Gap,0)
           ,"Below_All_Other_Nat_Av_Gap"=ifelse((All_Other_Nat_Av_Gap<0),All_Other_Nat_Av_Gap,0)
           ,"Above_All_Other_Nat_Av_Gap"=ifelse(All_Other_Nat_Av_Gap>0,All_Other_Nat_Av_Gap,0)
    )  %>%
    #Flags for above and below average
    mutate("Flag_FSM_Nat_Av_Gap"=ifelse((FSM_Nat_Av_Gap<0),"below","above")
           ,"Flag_All_Other_Nat_Av_Gap"=ifelse((All_Other_Nat_Av_Gap<0),"below","above")
    )  %>%
    #the next step adds national ranks for all variables of interest
    mutate( rank_FSM= frank(`%FSM_GLD`) #want descending order
            ,rank_within_LA_GAP=frankv(within_LA_GAP,order=-1L) #the smallest gap is the best
            ,rank_FSM_Nat_Av_Gap=frankv(FSM_Nat_Av_Gap,order=-1L) #want descending order
            ,rank_All_Other_Nat_Av_Gap=frankv(All_Other_Nat_Av_Gap,order=-1L)#want descending order
    ) 
  
  #Multiply some columns by 100 to get meaningful percentages
  
  EYFSP_Dataset_LA_Step1$`%All_GLD`   <- EYFSP_Dataset_LA_Step1$`%All_GLD`*100
  EYFSP_Dataset_LA_Step1$`%FSM_GLD`   <- EYFSP_Dataset_LA_Step1$`%FSM_GLD`*100
  EYFSP_Dataset_LA_Step1$`%All_Other_GLD`<- EYFSP_Dataset_LA_Step1$`%All_Other_GLD`*100
  EYFSP_Dataset_LA_Step1$within_LA_GAP  <- EYFSP_Dataset_LA_Step1$within_LA_GAP*100
  EYFSP_Dataset_LA_Step1$FSM_Nat_Av_Gap <- EYFSP_Dataset_LA_Step1$FSM_Nat_Av_Gap *100
  EYFSP_Dataset_LA_Step1$All_Other_Nat_Av_Gap <- EYFSP_Dataset_LA_Step1$All_Other_Nat_Av_Gap *100
  
  #need to add LA lookup for old to new codes for map
  
  fwrite(EYFSP_Dataset_LA_Step1,"data/SFR_EYFSP_SFR60_2017_subset.csv")
  
  #copy from graphs file
  EYC_Data_Filepath<-"data/SFR_EYFSP_SFR29_2017_subset.csv"
  EYFSP_Data_Filepath<-"data/SFR_EYFSP_SFR60_2017_subset.csv"
  
  EYC_Data <- read_csv(EYC_Data_Filepath)
  EYFSP_Data <- read_csv(EYFSP_Data_Filepath)
  
  EYFSP_Data <- EYFSP_Data%>%arrange(LA_name)
  
  #simulate a list of 8 national ranks
  
  #random_sample <- sample(152,7) %>% as.integer()