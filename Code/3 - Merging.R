setwd("C:/Users/byx2/Google Drive/Skin or Skim, Online")
#install.packages("purrr")
#setwd("C:/Users/Billy Xu/Google Drive/Skin or Skim, Online")
packages <- c("dplyr", "zoo", "tidyr", "data.table", "stringr", "ineq", "ggplot2")
purrr::walk(packages, library, character.only = TRUE)
rm( list=ls() ) #Removes any variables in the environment (wipe memory)

load("./Data/ReadyForMerging/df.Base.Rda")
load("./Data/ReadyForMerging/df.Schedule_D_7B1_.Rda")

Main <- function ()
{
  # Merge 7b1 with base
  df.base.7b1 <- MERGE_BASE_7B1(df.Schedule_D_7B1_, df.Base)
  
  saveList = ls(pattern = "df.")
  save(list = saveList, file="./Data/Merged/df.base.7b1.Rda")
}


MERGE_BASE_7B1 <- function(Schedule_D_7B1_, Base) {
  
  # focus on inner join
  df <- inner_join(Schedule_D_7B1_, Base, by=c("FilingID"))
  return(df)

}