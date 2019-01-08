# Need to be in the path:
setwd("~/GitHub/ADV/")

# Ensure libraries are installed before loading
packages <- c("dplyr", "zoo", "tidyr", "data.table", "stringr", "ineq", "ggplot2")
purrr::walk(packages, library, character.only = TRUE)
rm( list=ls() ) #Removes any variables in the environment (wipe memory)


Main <- function()
{
  
  eraPath = "./Data/ADV/FORM ADV COMPLETE ERA Jan 2001 to Dec 31, 2016"
  riaPath = "./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016"
  
  all_ERA_Files <- list.files(path = eraPath, ignore.case=TRUE)
  all_RIA_Files <- list.files(path = riaPath, ignore.case=TRUE)
  fileListTracker <- c(all_ERA_Files, all_RIA_Files)
  length(fileListTracker)
  
  
  # Step 1
  #load("./Data/Generated/df.step1.Rda")
  fileListTracker <- MatchingAcrossERAandRIA(fileListTracker)
  rm( list=ls(pattern = "df."))
  #riaStillLeft <- fileListTracker[fileListTracker %in% all_RIA_Files]
  
  # Step 2a
  fileListTracker <- JustFromRIA(fileListTracker)
  rm( list=ls(pattern = "df."))
  # Step 2b
  fileListTracker <- JustFromERA(fileListTracker)
  rm( list=ls(pattern = "df."))
  
  # Step 3a: Merge stubs in ERA that have name incongruencies
  fileListTracker <- CombineERANameIncongruencies(fileListTracker)
  rm( list=ls(pattern = "df."))
  # Step 3b: Merge stubs in RIA that have name incongruencies
  fileListTracker <- CombineRIANameIncongruencies(fileListTracker)
  rm( list=ls(pattern = "df."))
  
  # Step 4: Merge "Base" stub from ERA to "Base_A" stub from RIA
  fileListTracker <- CombineERAandRIANameIncongruencies(fileListTracker)
  rm( list=ls(pattern = "df."))
  
  # Step 5: [To be done]
  # Step 5a: Schedule_D_7B1
  fileListTracker <- SCHEDULE_D_7B1(fileListTracker)
  rm( list=ls(pattern = "df."))
  # Stubs to be addressed: c("Schedule_D_Misc","Schedule_D_7B1A7d", "Schedule_D_2A_Exemptive_Order_", "Schedule_D_7A_")
  # These stubs have issues with bind_rows()
}


# TODO: DEAL WITH THE HISTORICAL!
SCHEDULE_D_7B1 <- function(fileListTracker) {
  patternSet <- c("Schedule_D_7B1_")
  
  setOfPattersERA <- patternSet
  setOfPattersRIA <- patternSet
  
  eraPath = "./Data/ADV/FORM ADV COMPLETE ERA Jan 2001 to Dec 31, 2016/"
  riaPath = "./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016/"
  
  # ERA
  for(i in 1:length(setOfPattersERA))
  {
    # Create list of files to parse
    fileList_ERA    <- list.files(pattern=setOfPattersERA[i], path = eraPath, full.names=TRUE, ignore.case=TRUE)
    
    # Parse Files and create dataframe called df.era.X
    assign(paste0("df.era.", setOfPattersERA[i]), fileList_ERA %>% lapply(read.csv, header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersERA[i], path = eraPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  
  
  # RIA
  for(i in 1:length(setOfPattersRIA))
  {
    # Create list of files to parse
    fileList_RIA <- list.files(pattern=setOfPattersRIA[i], path = riaPath, full.names=TRUE, ignore.case=TRUE)
    
    # Parse Files and create dataframe called df.ria.X
    assign(paste0("df.ria.", setOfPattersRIA[i]), fileList_RIA %>% lapply(read.csv , header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)  
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersRIA[i], path = riaPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  
  # You want to iterate and bind!
  for(i in 1:length(patternSet))
  {
    stubList <- ls(pattern=patternSet[i]) 
    assign(paste0("df.",patternSet[i]), bind_rows(get(stubList[1]), get(stubList[2])))
    rm( list=c(stubList[1], stubList[2]) ) #Removes any variables in the environment (wipe memory)
  }

    
  # Manual merge of historical data
  Schedule_D_7B_20150315 <- read.csv("./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016/Schedule_D_7B_20150315.csv", stringsAsFactors=FALSE)
  
  Schedule_D_7B_20150315 <- Schedule_D_7B_20150315  %>% group_by(FilingID, LP.LLC.Fund.Name) %>% arrange(desc(Value)) %>% filter(row_number() == 1) # deals with multiple GP
  Schedule_D_7B_20150315 <- Schedule_D_7B_20150315  %>% rename(Fund.Name = LP.LLC.Fund.Name)
  df.Schedule_D_7B1_ <- bind_rows(df.Schedule_D_7B1_, Schedule_D_7B_20150315)
  
  # clean-up
  rm(Schedule_D_7B_20150315)
  fileListTracker <- fileListTracker[!(fileListTracker %in% "Schedule_D_7B_20150315.csv")]
  
  
  saveList = ls(pattern = "df.")
  saveList = c(saveList, "fileListTracker")
  save(list = saveList, file="./Data/Generated/df.step5a.Rda")
  
  return(fileListTracker)
  
  
}

MatchingAcrossERAandRIA <- function(fileListTracker) {
  
  
  patternSet <- c("Website", "Schedule_D_7B1A22", "Schedule_D_7B1A17b", "Schedule_D_7B1A7f", "Schedule_D_7B1A7_"
                   , "Schedule_D_7B1A6b", "Schedule_D_7B1A5", "Schedule_D_7B1A3", "Schedule_D_7A10b_", "Schedule_D_6B3_"
                   , "Schedule_D_6B2_", "Schedule_D_6A_", "Schedule_D_1I_", "Schedule_D_1F_", "Schedule_D_1B_", "Schedule_D_10B_", "Schedule_D_10A_"
                   , "Schedule_D_7B2_", "Schedule_A_B", "DRP_Regulatory", "DRP_Criminal", "DRP_Civil", "DRP_Advisory", "Filing_Types", "Schedule_D_7B1A28_2"
                   , "Schedule_D_7B1A26","Schedule_D_7B1A25", "Schedule_D_7B1A24", "Schedule_D_7B1A23"
                   , "Schedule_D_7B1A18b"
                   , "Schedule_D_5I2_", "Schedule_D_5G3_", "Schedule_D_4_", "Schedule_D_2A_Newly_Formed_"
                   , "Schedule_D_Foreign_Regulatory", "Schedule_D_Books_and_Records", "Schedule_D_9C_")

    
#c("Website", "Filing_Types", "DRP_Advisory", "DRP_Civil", "DRP_Criminal", "DRP_Regulatory",
#                 "Schedule_D_6A_", "Schedule_D_6B2_", "Schedule_D_6B3_",
#                "Schedule_D_1B_", "Schedule_D_1F_", "Schedule_D_1I_", 
#                  "Schedule_D_10A_", "Schedule_D_10B_",
#                  "Schedule_A_B", "Schedule_D_7B1_", "Schedule_D_7B1A3_", "Schedule_D_7B1A5_", "Schedule_D_7B1A6b_", 
#                  "Schedule_D_7B1A7f", "Schedule_D_7B1A22",
#                  "Schedule_D_7A10b_", "Schedule_D_7B1A7_", "Schedule_D_7B1A17b", 
#                  "Schedule_D_7B2_")
  
  ##ISSUE WITH! Schedule_D_7A_, Schedule_D_7B1A7d, "Schedule_D_Misc", "Schedule_D_2A_Exemptive_Order_"
  
  #patternSet <- c("Books_and_Records")
  setOfPattersERA <- patternSet
  setOfPattersRIA <- patternSet
  
  # PATHS
  eraPath = "./Data/ADV/FORM ADV COMPLETE ERA Jan 2001 to Dec 31, 2016/"
  riaPath = "./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016/"
  
  
  
   ## Test pattern
  patternToMatch <- c("Base")
  list.files(pattern=patternToMatch, path = eraPath, ignore.case=TRUE)
  list.files(pattern=patternToMatch, path = riaPath, ignore.case=TRUE)
  
  
  # ERA
  for(i in 1:length(setOfPattersERA))
  {
    # Create list of files to parse
    fileList_ERA    <- list.files(pattern=setOfPattersERA[i], path = eraPath, full.names=TRUE, ignore.case=TRUE)
    
    # Parse Files and create dataframe called df.era.X
    assign(paste0("df.era.", setOfPattersERA[i]), fileList_ERA %>% lapply(read.csv, header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersERA[i], path = eraPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  
  
  # RIA
  for(i in 1:length(setOfPattersRIA))
  {
    # Create list of files to parse
    fileList_RIA <- list.files(pattern=setOfPattersRIA[i], path = riaPath, full.names=TRUE, ignore.case=TRUE)
    
    # Parse Files and create dataframe called df.ria.X
    assign(paste0("df.ria.", setOfPattersRIA[i]), fileList_RIA %>% lapply(read.csv , header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)  
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersRIA[i], path = riaPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  
  # You want to iterate and bind!
  for(i in 1:length(patternSet))
  {
    stubList <- ls(pattern=patternSet[i]) 
    assign(paste0("df.",patternSet[i]), bind_rows(get(stubList[1]), get(stubList[2])))
    rm( list=c(stubList[1], stubList[2]) ) #Removes any variables in the environment (wipe memory)
  }
  
  saveList = ls(pattern = "df.")
  saveList = c(saveList, "fileListTracker")
  save(list = saveList, file="./Data/Generated/df.step1.Rda")
  
  return(fileListTracker)
}

JustFromRIA <- function(fileListTracker) {
  
  
  riaPath         <- "./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016"
  
  setOfPattersRIA      <- c("Schedule_D_5I2_", "Schedule_D_5G3_", "Schedule_D_4_", "Schedule_D_2A_Newly_Formed_",
                            "Schedule_D_9C_", "Base_B") 
  
  # ISSUE WITH Exemptive_Order
  patternToMatch <- c("ADV_Base_")
  list.files(pattern=patternToMatch, path = eraPath, ignore.case=TRUE)
  list.files(pattern=patternToMatch, path = riaPath, ignore.case=TRUE)
  
  
  
  for(i in 1:length(setOfPattersRIA))
  {
    # Create list of files to parse
    fileList_RIA <- list.files(pattern=setOfPattersRIA[i], path = riaPath, full.names=TRUE, ignore.case=TRUE)
    
    # Parse Files and create dataframe called df.ria.X
    assign(paste0("df.ria.", setOfPattersRIA[i]), fileList_RIA %>% lapply(read.csv , header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)  
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersRIA[i], path = riaPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  
  #temp1 <- read.csv("./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016/ADV_Base_A_20150315.csv")
  #temp2 <- read.csv("./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016/ADV_Base_B_20150315.csv")
  
  saveList = ls(pattern = "df.")
  saveList = c(saveList, "fileListTracker")
  save(list = saveList, file="./Data/Generated/df.step2a.Rda")
  return(fileListTracker)
  
}

JustFromERA <- function(fileListTracker) { 
  
  
  eraPath = "./Data/ADV/FORM ADV COMPLETE ERA Jan 2001 to Dec 31, 2016/"
  
  setOfPattersERA      <- c("Schedule_D_1M_", "Schedule_D_1L_")
  
  #patternToMatch <- c("ADV_Base_")
  #list.files(pattern=patternToMatch, path = eraPath, ignore.case=TRUE)
  #list.files(pattern=patternToMatch, path = riaPath, ignore.case=TRUE)
  

  for(i in 1:length(setOfPattersERA))
  {
    # Create list of files to parse
    fileList_ERA <- list.files(pattern=setOfPattersERA[i], path = eraPath, full.names=TRUE, ignore.case=TRUE)
    
    # Parse Files and create dataframe called df.era.X
    assign(paste0("df.era.", setOfPattersERA[i]), fileList_ERA %>% lapply(read.csv , header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)  
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersERA[i], path = eraPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  
  #temp1 <- read.csv("./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016/ADV_Base_A_20150315.csv")
  #temp2 <- read.csv("./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016/ADV_Base_B_20150315.csv")
  
  saveList = ls(pattern = "df.")
  saveList = c(saveList, "fileListTracker")
  save(list = saveList, file="./Data/Generated/df.step2b.Rda")
  return(fileListTracker)
  
}

CombineERANameIncongruencies <- function(fileListTracker) {
  setOfPattersERA1 <- c("Schedule_D_7B1A28_2","Schedule_D_7B1A26","Schedule_D_7B1A25","Schedule_D_7B1A24","Schedule_D_7B1A23","Schedule_D_7B1A18b")
  setOfPattersERA2 <- c("Schedule_D_7B1B28_2","Schedule_D_7B1B26","Schedule_D_7B1B25","Schedule_D_7B1B24","Schedule_D_7B1B23","Schedule_D_7B1A1b")
  eraPath = "./Data/ADV/FORM ADV COMPLETE ERA Jan 2001 to Dec 31, 2016/"
  
  # ERA
  for(i in 1:length(setOfPattersERA1))
  {
    # Create list of files to parse
    fileList_ERA    <- list.files(pattern=setOfPattersERA1[i], path = eraPath, full.names=TRUE, ignore.case=TRUE)
    fileList_ERAtemp <- list.files(pattern=setOfPattersERA2[i], path = eraPath, full.names=TRUE, ignore.case=TRUE)
    fileList_ETA <- c(fileList_ERA, fileList_ERAtemp)
    
    # Parse Files and create dataframe called df.era.X
    assign(paste0("df.era.", setOfPattersERA1[i]), fileList_ERA %>% lapply(read.csv, header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersERA1[i], path = eraPath, full.names=FALSE, ignore.case=TRUE))]
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersERA2[i], path = eraPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  saveList = ls(pattern = "df.")
  saveList = c(saveList, "fileListTracker")
  save(list = saveList, file="./Data/Generated/df.step3a.Rda")
  
  return(fileListTracker)
  
}

CombineRIANameIncongruencies <- function(fileListTracker) {
  setOfPattersRIA1 <- c("Schedule_D_2A_Related_","Schedule_D_2A_Multistate_")
  setOfPattersRIA2 <- c("chedule_D_2A_Related_","Schedule_D_2A_2")
  riaPath <- "./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016"
  
  # ERA
  for(i in 1:length(setOfPattersRIA1))
  {
    # Create list of files to parse
    fileList_RIA    <- list.files(pattern=setOfPattersRIA1[i], path = riaPath, full.names=TRUE, ignore.case=TRUE)
    fileList_RIAtemp <- list.files(pattern=setOfPattersRIA2[i], path = riaPath, full.names=TRUE, ignore.case=TRUE)
    fileList_RIA <- c(fileList_RIA, fileList_RIAtemp)
    
    # Parse Files and create dataframe called df.era.X
    assign(paste0("df.ria.", setOfPattersRIA1[i]), fileList_RIA %>% lapply(read.csv, header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersRIA1[i], path = riaPath, full.names=FALSE, ignore.case=TRUE))]
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersRIA2[i], path = riaPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  saveList = ls(pattern = "df.")
  saveList = c(saveList, "fileListTracker")
  save(list = saveList, file="./Data/Generated/df.step3b.Rda")
  
  return(fileListTracker)
  
}

CombineERAandRIANameIncongruencies <- function(fileListTracker) {
  #patternSet <- c(ERAStub,RIAStub)
  setOfPattersERA <- c("Base")
  setOfPattersRIA <- c("Base_A")
  ERAStub <- "Base"
  RIAStub <- "Base_A"
  
  # PATHS
  eraPath = "./Data/ADV/FORM ADV COMPLETE ERA Jan 2001 to Dec 31, 2016/"
  riaPath = "./Data/ADV/FORM ADV COMPLETE RIA Jan 2001 to Dec 31, 2016/"
  
  # ERA
  for(i in 1:length(setOfPattersERA))
  {
    # Create list of files to parse
    fileList_ERA    <- list.files(pattern=setOfPattersERA[i], path = eraPath, full.names=TRUE, ignore.case=TRUE)
    
    # Parse Files and create dataframe called df.era.X
    assign(paste0("df.era.", setOfPattersERA[i]), fileList_ERA %>% lapply(read.csv, header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersERA[i], path = eraPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  # RIA
  for(i in 1:length(setOfPattersRIA))
  {
    # Create list of files to parse
    fileList_RIA <- list.files(pattern=setOfPattersRIA[i], path = riaPath, full.names=TRUE, ignore.case=TRUE)
    
    # Parse Files and create dataframe called df.ria.X
    assign(paste0("df.ria.", setOfPattersRIA[i]), fileList_RIA %>% lapply(read.csv , header = TRUE, stringsAsFactors = FALSE) %>% bind_rows)  
    
    # Trim File List
    fileListTracker <- fileListTracker[!(fileListTracker %in% list.files(pattern=setOfPattersRIA[i], path = riaPath, full.names=FALSE, ignore.case=TRUE))]
  }
  
  # You want to bind
  assign(paste0("df.",ERAStub), bind_rows(get(paste0("df.era.",ERAStub)), get(paste0("df.ria.",RIAStub))))
  rm( list=c(paste0("df.era.",ERAStub), paste0("df.ria.",RIAStub)) ) #Removes any variables in the environment (wipe memory)
  
  
  saveList = ls(pattern = "df.")
  saveList = c(saveList, "fileListTracker")
  save(list = saveList, file="./Data/Generated/df.step4.Rda")
  
  return(fileListTracker)
}


