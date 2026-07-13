# ------------------------------------------------------------------ #
library("foreign")
Dataset <- read.csv(file = "CICIDS2017_Dataset/Friday-WorkingHours-Afternoon-DDos.pcap_ISCX.csv")

DatasetClass <<- split(Dataset, Dataset$Label)
DatasetClass1 <<-DatasetClass$BENIGN
DatasetClass2 <<-DatasetClass$DDoS

DatasetClass1[,79] <- as.integer(DatasetClass1[,79])
DatasetClass2[,79] <- as.integer(DatasetClass2[,79])

for(i in 1:nrow(DatasetClass1))
{
  DatasetClass1[i,79] <- 0
}

for(i in 1:nrow(DatasetClass2))
{
  DatasetClass2[i,79] <- 1
}

CVFoldList1 <- list()
FoldSize <- 2
CVFoldList1 <- split(DatasetClass1, sample(1:FoldSize, nrow(DatasetClass1), replace = TRUE))

CVFoldList2 <- list()
FoldSize <- 2
CVFoldList2 <- split(DatasetClass2, sample(1:FoldSize, nrow(DatasetClass2), replace = TRUE))

TrainDatasetOrg <- rbind(CVFoldList1[[1]],CVFoldList2[[1]])
TestDatasetOrg <- rbind(CVFoldList1[[2]],CVFoldList2[[2]])

TrainDataset <- TrainDatasetOrg
TestDataset <- TestDatasetOrg

save(TrainDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017/CICIDS2017_Dataset/CICIDS2017_Trainingset.Rdata")
save(TestDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017/CICIDS2017_Dataset/CICIDS2017_Testingset.Rdata")
# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #
library("dplyr")
TrainDataset %>% count(Label)
TestDataset %>% count(Label)
# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

for(i in 1:nrow(TrainDataset))
{
  for(j in 1:ncol(TrainDataset))
  {
    if((is.na(TrainDataset[i,j]) == TRUE) || (is.nan(TrainDataset[i,j]) == TRUE) || (is.infinite(TrainDataset[i,j]) == TRUE))
    {
      if(is.na(TrainDataset[i,j]) == TRUE)
        cat("NA found in row", i, "and col", j , "\n")
      else if(is.nan(TrainDataset[i,j]) == TRUE)
        cat("NAN found in row", i, "and col", j , "\n")
      else if(is.infinite(TrainDataset[i,j]) == TRUE)
        cat("INF found in row", i, "and col", j , "\n")
    }
  }
}

TrnrowList <- c(33,946,4014,6562,7249,26636,35985,37781,37904,42963,42974,43273,110244)
TrainDatasetTemp <- TrainDataset[-TrnrowList, ]
TrainDataset <- TrainDatasetTemp

for(i in 1:nrow(TestDataset))
{
  for(j in 1:ncol(TestDataset))
  {
    if((is.na(TestDataset[i,j]) == TRUE) || (is.nan(TestDataset[i,j]) == TRUE) || (is.infinite(TestDataset[i,j]) == TRUE))
    {
      if(is.na(TestDataset[i,j]) == TRUE)
        cat("NA found in row", i, "and col", j , "\n")
      else if(is.nan(TestDataset[i,j]) == TRUE)
        cat("NAN found in row", i, "and col", j , "\n")
      else if(is.infinite(TestDataset[i,j]) == TRUE)
        cat("INF found in row", i, "and col", j , "\n")
    }
  }
}

TestrowList <- c(887, 1689, 3415, 4202, 6953, 7653, 9244, 11439, 15698, 18766, 24519, 30387, 30397, 33895, 36785, 38510, 40741, 41103, 43041, 44425, 62440)
TestDatasetTemp <- TestDataset[-TestrowList, ]
TestDataset <- TestDatasetTemp

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #
library("dplyr")
TrainDataset %>% count(Label)
TestDataset %>% count(Label)
# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

for(i in 1:ncol(TrainDataset))
{
  cat("Feature:",i, "MaxValue:",max(TrainDataset[,i]),"\n")
}

for(i in 1:ncol(TrainDataset))
{
  cat("Feature:",i, "MinValue:",min(TrainDataset[,i]),"\n")
}

for(i in 1:ncol(TestDataset))
{
  cat("Feature:",i, "MaxValue:",max(TestDataset[,i]),"\n")
}

for(i in 1:ncol(TestDataset))
{
  cat("Feature:",i, "MinValue:",min(TestDataset[,i]),"\n")
}

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

for(i in 2:2)
{
  for(j in 1:nrow(TrainDataset))
  {
    if(TrainDataset[j,i] == -1)
    {
      TrainDataset[j,i] = 0
    }
  }
}

for(i in 2:2)
{
  for(j in 1:nrow(TestDataset))
  {
    if(TestDataset[j,i] == -1)
    {
      TestDataset[j,i] = 0
    }
  }
}

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

save(TrainDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017/CICIDS2017_Dataset/CICIDS2017_Trainingset.Rdata")
save(TestDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017/CICIDS2017_Dataset/CICIDS2017_Testingset.Rdata")

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

for(i in 1:ncol(TrainDataset))
{
  cat("Feature:",i, "MaxValue:",max(TrainDataset[,i]),"\n")
}

for(i in 1:ncol(TrainDataset))
{
  cat("Feature:",i, "MinValue:",min(TrainDataset[,i]),"\n")
}

for(i in 1:ncol(TestDataset))
{
  cat("Feature:",i, "MaxValue:",max(TestDataset[,i]),"\n")
}

for(i in 1:ncol(TestDataset))
{
  cat("Feature:",i, "MinValue:",min(TestDataset[,i]),"\n")
}

#Train Dataset Negative Valued Features
# Feature #15, #16, #17, #19, #25 #67, #68

#Test Dataset Negative Valued Features
# Feature #15, #16, #17, #19, #20, #25 #67, #68

ZeroValFTList <- c(32,33,34,50,57,58,59,60,61,62)

TrainDataset <- TrainDataset[,-ZeroValFTList]
TestDataset <- TestDataset[,-ZeroValFTList]

##### Z-Score Normalization ####################################################################################
# ------------------------------------------------------------------------------------------------------------ #
TrainDatasetNorm <- TrainDataset
TrainDatasetNoNorm <- TrainDataset
TestDatasetNorm <- TestDataset
TrainDatasetMeanList <- vector(length = (ncol(TrainDataset)-1))
TrainDatasetSdList <- vector(length = (ncol(TrainDataset)-1))

for(j in 1:ncol(TrainDataset))
{
  TrainDatasetMeanList[j] <- mean(TrainDatasetNoNorm[,j])
  TrainDatasetSdList[j] <- sd(TrainDatasetNoNorm[,j])
  
  for(i in 1:nrow(TrainDataset))
  {
    TrainDatasetNorm[i,j] <- ((TrainDataset[i,j] - TrainDatasetMeanList[j]) / (TrainDatasetSdList[j]))
  }
}
# ------------------------------------------------------------------------------------------------------------ #
for(j in 1:ncol(TestDataset))
{
  for(i in 1:nrow(TestDataset))
  {
    TestDatasetNorm[i,j] <- ((TestDataset[i,j] - TrainDatasetMeanList[j]) / (TrainDatasetSdList[j]))
  }
}
# --------------------------------------------------------------------- #

TrainDatasetNorm[,69] <- TrainDataset[,69]
TestDatasetNorm[,69] <- TestDataset[,69]

save(TrainDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TrainingsetOrg.Rdata")
save(TestDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TestingsetOrg.Rdata")
save(TrainDatasetNorm, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TrainingsetZscore.Rdata")
save(TestDatasetNorm, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TestingsetZscore.Rdata")

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #
#### Min-Max Normalization ##################################################
# ---------------------------------------------------------------------------- #
TrainDatasetNorm <- TrainDataset
TrainDatasetNoNorm <- TrainDataset
TestDatasetNorm <- TestDataset
TrainDatasetMinList <- vector(length = (ncol(TrainDataset)-1))
TrainDatasetMaxList <- vector(length = (ncol(TrainDataset)-1))

for(j in 1:ncol(TrainDataset))
{
  TrainDatasetMinList[j] <- min(TrainDatasetNoNorm[,j])
  TrainDatasetMaxList[j] <- max(TrainDatasetNoNorm[,j])
  
  for(i in 1:nrow(TrainDataset))
  {
    TrainDatasetNorm[i,j] <- ((TrainDataset[i,j] - TrainDatasetMinList[j]) / (TrainDatasetMaxList[j] - TrainDatasetMinList[j]))
  }
}
# ---------------------------------------------------------------------------- #
for(j in 1:ncol(TestDataset))
{
  for(i in 1:nrow(TestDataset))
  {
    TestDatasetNorm[i,j] <- ((TestDataset[i,j] - TrainDatasetMinList[j]) / (TrainDatasetMaxList[j] - TrainDatasetMinList[j]))
  }
}
# ---------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------ #

TrainDatasetNorm[,69] <- TrainDataset[,69]
TestDatasetNorm[,69] <- TestDataset[,69]

save(TrainDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TrainingsetOrg.Rdata")
save(TestDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TestingsetOrg.Rdata")
save(TrainDatasetNorm, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TrainingsetZscore.Rdata")
save(TestDatasetNorm, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TestingsetZscore.Rdata")

# --------------------------------------------------------------------- #
#### Log Scaling ##############################################################
# ---------------------------------------------------------------------------- #
TrainDatasetNorm <- TrainDataset
TrainDatasetNoNorm <- TrainDataset
TestDatasetNorm <- TestDataset

for(j in 1:ncol(TrainDataset))
{
  for(i in 1:nrow(TrainDataset))
  {
    TrainDatasetNorm[i,j] <- log(TrainDataset[i,j] + 1)
  }
}
# ---------------------------------------------------------------------------- #
for(j in 1:ncol(TestDataset))
{
  for(i in 1:nrow(TestDataset))
  {
    TestDatasetNorm[i,j] <- log(TestDataset[i,j] + 1)
  }
}
# ---------------------------------------------------------------------------- #
TrainDatasetNorm[,69] <- TrainDataset[,69]
TestDatasetNorm[,69] <- TestDataset[,69]

save(TrainDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TrainingsetOrg.Rdata")
save(TestDataset, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TestingsetOrg.Rdata")
save(TrainDatasetNorm, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TrainingsetZscore.Rdata")
save(TestDatasetNorm, file = "c:\\RProjects//NIDS_v1.2.1_GridSearch_FinalVer_CICIDS2017-ZScore//CICIDS2017_Dataset/CICIDS2017_TestingsetZscore.Rdata")
# --------------------------------------------------------------------- #