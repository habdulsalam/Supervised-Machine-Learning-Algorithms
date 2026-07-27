###### Begining of the Program #################################################################################
# ------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------ #
# ############################################################################################################ #

##### Library and Function Imports #############################################################################
# Libraries -------------------------------------------------------------------------------------------------- #
library("e1071")                                                   #SVM Library                                #
library("caret")                                                   #General Library                            #
library("xlsx")                                                    #Export to Excel Library                    #
# ------------------------------------------------------------------------------------------------------------ #
# Working Directory Path Select ------------------------------------------------------------------------------ #
sourcepath <<- getwd()                                             #Get working directory as sourcepath var    #
setwd(sourcepath)                                                  #Set sourcepath as working directory        #
# ------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------ #
# Import Functions ------------------------------------------------------------------------------------------- #
source(paste(sourcepath,"\\DatasetListFn.R",sep=""))                       # Import Dataset List Function      #
source(paste(sourcepath,"\\ImportDataset.R",sep=""))                       # Import Dataset List Function      #
# ------------------------------------------------------------------------------------------------------------ #
##### End of Library and Function Imports ######################################################################

##### Program Body - Run Program Dataset by Dataset ############################################################
# ------------------------------------------------------------------------------------------------------------ #

# Call Dataset List Function --------------------------------------------------------------------------------- #
DatasetListFn()                                                                                                #
ImportDataset()                                                                                               #

# ------------------------------------------------------------------------------------------------------------ #
load(TrainDataSet_PATH)
colnames(TrainDataset)[ClassColumn] <- "ClassColumn"

load(TestDataSet_PATH)
colnames(TestDataset)[ClassColumn] <- "ClassColumn"
# ------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------ #
##### External CV ##############################################################################################
# ------------------------------------------------------------------------------------------------------------ #
CVFoldList <- list()
TrnDatasetFoldList <- list()
TrainFoldList <- list()
ValFoldList <- list()

FoldSize <- 11
CVFoldList <- split(TrainDataset, sample(1:FoldSize, nrow(TrainDataset), replace = TRUE))
# ------------------------------------------------------------------------------------------------------------ #

##### Optimization FS and Classifier ###########################################################################
# ------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------ #
RankList <- list()
RankListNew <- list()
TopFtList <- c(1,2,3,4,8,16,32,64,68)
TopFtListOpt <- c(2,3,4,8)

KernelList <- c("linear","polynomial","sigmoid","radial")
CostValList <- c(10^6,10^5,10^4,10^3,10^2,10^1,10^0,10^-1,10^-2,10^-3)
GammaValList <- c(10^3,10^2,10^1,10^0,10^-1,10^-2,10^-3,10^-4,10^-5,10^-6)

EvalACCList <- as.data.frame(matrix(c(0),nrow = (FoldSize-1) * length(CostValList) * length(GammaValList), ncol = (4+length(TopFtListOpt)), byrow = TRUE))
names(EvalACCList)[1:4] <- c("Fold", "Kernel","CostVal", "GammaVal")
for(i in 1:length(TopFtListOpt))
{
  names(EvalACCList)[i+4] <- paste("TopFt_",TopFtListOpt[i],sep="")
}
# ------------------------------------------------------------------------------------------------------------ #
OptResList <- list()
TrainResList <- list()
TestResList <- list()
TestNo21List <- list()
# ------------------------------------------------------------------------------------------------------------ #
SVMTrainList <- list()
SVMTrainTrainList <- list()
SVMTrainTestList <- list()
SVMTrainTestNo21List <- list()
# ------------------------------------------------------------------------------------------------------------ #

cntr <- 1
TempBakFileNames <- vector(length = (length(CostValList) * length(GammaValList)))

for(ix in 4:length(KernelList))
{
  cat("Kernel: ",KernelList[ix],"\n",sep="")
  
  for(iy in 1:length(CostValList))
  {
    cat("Cost: ",CostValList[iy],"\n",sep="")
    
    for(iz in 1:length(GammaValList))
    {
      cat("Gamma: ",GammaValList[iz],"\n",sep="")
      
      for(xcnt in 1:(FoldSize-1)) #
      {
        cat("Fold #", xcnt,"\n",sep="")
        
        TrnDatasetTrain <- CVFoldList[[xcnt]]
        TrnDatasetEval <- CVFoldList[[11]]
        
        SVMRankTrain <- svm(x = TrnDatasetTrain[,-ClassColumn], y = TrnDatasetTrain[,ClassColumn], type = "C-classification", kernel = KernelList[ix], cost = CostValList[iy], gamma = GammaValList[iz], scale = FALSE, cachesize = 256)
        w <- crossprod(SVMRankTrain$coefs,SVMRankTrain$SV)
        rankingCriteria <- w * w
        ranking <- sort(rankingCriteria, index.return = TRUE, decreasing = TRUE)$ix
        RankList[[1]] <- ranking
        
        EvalACCList[cntr,1] <- xcnt
        EvalACCList[cntr,2] <- 4
        EvalACCList[cntr,3] <- CostValList[iy]
        EvalACCList[cntr,4] <- GammaValList[iz]
        
        for(j in 1:length(TopFtListOpt))
        {
          SelectedFeatureList <- ranking[1:TopFtListOpt[j]]
          
          SVMTrain <- svm(x = TrnDatasetTrain[,SelectedFeatureList], y = TrnDatasetTrain[,ClassColumn], type = "C-classification", kernel = KernelList[ix], cost = CostValList[iy], gamma = GammaValList[iz], scale = FALSE, cachesize = 256)
          SVMTest <- predict(SVMTrain,TrnDatasetEval[,SelectedFeatureList])
          ConfMatTable <- table(SVMTest,TrnDatasetEval[,ClassColumn])
          
          ClsfPerACCTest <- (ConfMatTable[1] + ConfMatTable[4]) / nrow(TrnDatasetEval)
          
          OptResList[[cntr]] <- paste("Feature Size:",length(SelectedFeatureList),"Accuracy:",(ClsfPerACCTest * 100),"Features:",SelectedFeatureList,sep="--")
          EvalACCList[cntr,4+j] <- ClsfPerACCTest
        }
        
        cntr <- cntr + 1
      }
      
      BakPath <- paste(sourcepath,"/Workspace/WorkspaceOptTempBak_",cntr,".RData",sep = "")
      save.image(BakPath)
      
      TempBakFileNames[(iz + (length(GammaValList) * (iy - 1)))] <- BakPath
    }
  }
}

for(i in 1:length(TempBakFileNames))
{
  file.remove(TempBakFileNames[i])
}

# ------------------------------------------------------------------------------------------------------------ #
BakPath <- paste(sourcepath,"/Workspace/Workspace_AfterOpt.RData",sep="")
save.image(BakPath)
# ------------------------------------------------------------------------------------------------------------ #

CostTotIterCnt <- length(CostValList)
GammaTotIterCnt <- length(GammaValList)
CGTotIterCnt <- length(CostValList) * length(GammaValList)

AvgEvalACCList <- as.data.frame(matrix(c(0),nrow = length(CostValList) * length(GammaValList), ncol = (4+length(TopFtListOpt)), byrow = TRUE))
names(AvgEvalACCList)[1:4] <- c("Fold", "Kernel","CostVal", "GammaVal")
for(i in 1:length(TopFtListOpt))
{
  names(AvgEvalACCList)[i+4] <- paste("TopFt_",TopFtListOpt[i],sep="")
}

cnti = 1

for(i in 1:CostTotIterCnt)
{
  for(k in 1:GammaTotIterCnt)
  {
    AvgEvalACCList[cnti,1] <- cnti
    AvgEvalACCList[cnti,2] <- 4
    AvgEvalACCList[cnti,3] <- CostValList[i]
    AvgEvalACCList[cnti,4] <- GammaValList[k]
    
    for(j in 1:length(TopFtListOpt))
    {
      AvgEvalACCList[cnti,4+j] <- mean(EvalACCList[(1+((cnti-1)*10)):(10+((cnti-1)*10)),4+j])  #10 Iter
      # AvgEvalACCList[cnti,4+j] <- mean(EvalACCList[(1+((cnti-1)*10)):(5+((cnti-1)*10)),4+j]) #5 Iter
      # AvgEvalACCList[cnti,4+j] <- (EvalACCList[(1+((cnti-1)*10)):(1+((cnti-1)*10)),4+j])     #1 Iter
    }
    
    cnti <- cnti + 1
  }
}

#### Model Selection using Average ############################################################################
# ------------------------------------------------------------------------------------------------------------ #
AvgAvgEvalACCList <- as.data.frame(matrix(c(0),nrow = length(CostValList) * length(GammaValList), ncol = 5, byrow = TRUE))
names(AvgAvgEvalACCList)[1:5] <- c("Fold", "Kernel","CostVal", "GammaVal","Average")

for(ik in 1:(length(CostValList) * length(GammaValList)))
{
  AvgAvgEvalACCList[ik,1] <- AvgEvalACCList[ik,1]
  AvgAvgEvalACCList[ik,2] <- AvgEvalACCList[ik,2]
  AvgAvgEvalACCList[ik,3] <- AvgEvalACCList[ik,3]
  AvgAvgEvalACCList[ik,4] <- AvgEvalACCList[ik,4]
  AvgAvgEvalACCList[ik,5] <- mean(as.numeric(AvgEvalACCList[ik,5:8])) #:Top 2, 3, 4, 8 Features
}

MaxAvgAccVal <- max(AvgAvgEvalACCList[,5])
MaxAvgAccValPos <- which.max(AvgAvgEvalACCList[,5])
ChosenKernel_v1 <- AvgAvgEvalACCList[which.max(AvgAvgEvalACCList[,5]),2]
ChosenCost_v1 <- AvgAvgEvalACCList[which.max(AvgAvgEvalACCList[,5]),3]
ChosenGamma_v1 <- AvgAvgEvalACCList[which.max(AvgAvgEvalACCList[,5]),4]
print(ChosenCost_v1)
print(ChosenGamma_v1)
# ------------------------------------------------------------------------------------------------------------ #

##### Single Model Tests Ranked Feature  List ###################################################################
# ------------------------------------------------------------------------------------------------------------ #
SingleModelsRankedList <- as.data.frame(matrix(c(0),nrow = 1, ncol = (4+FeatureSize), byrow = TRUE))
names(SingleModelsRankedList)[1:4] <- c("Model#", "Kernel","CostVal", "GammaVal")
for(i in 1:FeatureSize)
{
  names(SingleModelsRankedList)[i+4] <- paste("Rank-",i,sep="")
}
# ------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------ #
FinalACCResult_TrainDt <- as.data.frame(matrix(c(0),nrow = 1, ncol = (4+length(TopFtList)), byrow = TRUE))
names(FinalACCResult_TrainDt)[1:4] <- c("Model#", "Kernel","CostVal", "GammaVal")
for(i in 1:length(TopFtList))
{
  names(FinalACCResult_TrainDt)[i+4] <- paste("TopFt_",TopFtList[i],sep="")
}

FinalACCResult_TestDt <- FinalACCResult_TrainDt
# ------------------------------------------------------------------------------------------------------------ #

##### SingleModelTests - Test for each pair of values in the list ##############################################
# ------------------------------------------------------------------------------------------------------------ #
for(cntcg in 1:1) #nrow(MaxValACCList)
{
  ##### Assign selected values as chosen values for parameters ###################################################
  # ------------------------------------------------------------------------------------------------------------ #
  cat("Print chosen cost and gamma values: \n")
  ChosenCost <- ChosenCost_v1
  ChosenGamma <- ChosenGamma_v1
  cat("Cost: ", ChosenCost,"\n")
  cat("Gamma: ", ChosenGamma,"\n")
  # ------------------------------------------------------------------------------------------------------------ #
  
  BakPath <- paste(sourcepath,"/Workspace/Workspace_AfterParamSelect.RData",sep="")
  save.image(BakPath)
  
  ##### Feature Ranking with Optimized parameters ################################################################
  # ------------------------------------------------------------------------------------------------------------ #
  SVMRankTrainFinal <- svm(x = TrainDataset[,-ClassColumn], y = TrainDataset[,ClassColumn], type = "C-classification", kernel = "radial", cost = ChosenCost, gamma = ChosenGamma, scale = FALSE, cachesize = 256)
  wlist <- crossprod(SVMRankTrainFinal$coefs,SVMRankTrainFinal$SV)
  rankingCriteria <- wlist * wlist
  rankingList <- sort(rankingCriteria, index.return = TRUE, decreasing = TRUE)$ix
  RankListNew[[1]] <- rankingList
  # ------------------------------------------------------------------------------------------------------------ #
  
  SingleModelsRankedList[cntcg,1] <- cntcg
  SingleModelsRankedList[cntcg,2] <- 4
  SingleModelsRankedList[cntcg,3] <- ChosenCost
  SingleModelsRankedList[cntcg,4] <- ChosenGamma
  SingleModelsRankedList[cntcg,5:(FeatureSize+4)] <- rankingList
  
  ##### Export Ranked List to a Excel File and Save Workspace ####################################################
  # ------------------------------------------------------------------------------------------------------------ #
  write.xlsx(SingleModelsRankedList, paste(sourcepath,"\\Results\\","SingleModels_RankedFeatureList.xlsx",sep=""))
  # ------------------------------------------------------------------------------------------------------------ #
  BakPath <- paste(sourcepath,"/Workspace/WorkspaceAfterFS.RData",sep="")
  save.image(BakPath)
  # ------------------------------------------------------------------------------------------------------------ #
  # cat("Print ranked feature list: \n")
  # print(RankListNew[[1]])
  # ------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------ #
  
  # ------------------------------------------------------------------------------------------------------------ #
  # Training with Optimized Parameters # ----------------------------------------------------------------------- #
  cat("SVM Training... \n")
  
  for(j in 1:length(TopFtList))
  {
    cat("Feature Selector No:",j,sep=" ","\n")
    SelectedFeatureList <- RankListNew[[1]][1:TopFtList[j]]
    SVMTrainList[[j]] <- svm(x = TrainDataset[,SelectedFeatureList], y = TrainDataset[,ClassColumn], type = "C-classification", kernel = "radial", cost = ChosenCost, gamma = ChosenGamma, scale = FALSE, cachesize = 256)
  }
  # ------------------------------------------------------------------------------------------------------------ #
    BakPath <- paste(sourcepath,"/Workspace/WorkspaceAfterTraining.RData",sep="")
    save.image(BakPath)
  # ------------------------------------------------------------------------------------------------------------ #
  # Testing - Train Dataset # ---------------------------------------------------------------------------------- #
  # ------------------------------------------------------------------------------------------------------------ #
    cat("SVM Training and Evaluation for Train Dataset... \n")
   
    FinalACCResult_TrainDt[cntcg,1] <- cntcg
    FinalACCResult_TrainDt[cntcg,2] <- 4
    FinalACCResult_TrainDt[cntcg,3] <- ChosenCost
    FinalACCResult_TrainDt[cntcg,4] <- ChosenGamma
    
    for(j in 1:length(TopFtList))
    {
      cat("Feature Selector No:",j,sep=" ","\n")
      SelectedFeatureList <- RankListNew[[1]][1:TopFtList[j]]
    
      SVMTrainTrainList[[j]] <- predict(SVMTrainList[[j]],TrainDataset[,SelectedFeatureList])
      ConfMatTable1 <- table(SVMTrainTrainList[[j]],TrainDataset[,ClassColumn])
      ClsfPerACCTest1 <- (ConfMatTable1[1] + ConfMatTable1[4]) / nrow(TrainDataset)
      # cat("Accuracy:",(ClsfPerACCTest1 * 100),"\n")
  
      TrainResList[[j]] <- paste("Feature Size:",length(SelectedFeatureList),"Accuracy:",(ClsfPerACCTest1 * 100),"Features:",SelectedFeatureList,sep = "--")
      # cat("Feature Size:",length(SelectedFeatureList),"--Accuracy:",(ClsfPerACCTest1 * 100),"--Features:",SelectedFeatureList,"--\n")
      
      FinalACCResult_TrainDt[cntcg,4+j] <- ClsfPerACCTest1
    }
  # ------------------------------------------------------------------------------------------------------------ #
    BakPath <- paste(sourcepath,"/Workspace/Workspace_AfterTestTrnDataset.RData",sep="")
    save.image(BakPath)
  # ------------------------------------------------------------------------------------------------------------ #
  # Testing - Test Dataset # ----------------------------------------------------------------------------------- #
  # ------------------------------------------------------------------------------------------------------------ #
    cat("SVM Training and Evaluation for Test Dataset... \n")
  
    FinalACCResult_TestDt[cntcg,1] <- cntcg
    FinalACCResult_TestDt[cntcg,2] <- 4
    FinalACCResult_TestDt[cntcg,3] <- ChosenCost
    FinalACCResult_TestDt[cntcg,4] <- ChosenGamma
    
    for(j in 1:length(TopFtList))
    {
      cat("Feature Selector No:",j,sep=" ","\n")
      SelectedFeatureList <- RankListNew[[1]][1:TopFtList[j]]
  
      SVMTrainTestList[[j]] <- predict(SVMTrainList[[j]],TestDataset[,SelectedFeatureList])
      ConfMatTable2 <- table(SVMTrainTestList[[j]],TestDataset[,ClassColumn])
      ClsfPerACCTest2 <- (ConfMatTable2[1] + ConfMatTable2[4]) / nrow(TestDataset)
      # cat("Accuracy:",(ClsfPerACCTest2 * 100),"\n")
      
      TestResList[[j]] <- paste("Feature Size:",length(SelectedFeatureList),"Accuracy:",(ClsfPerACCTest2 * 100),"Features:",SelectedFeatureList,sep="--")
      # cat("Feature Size:",length(SelectedFeatureList),"--Accuracy:",(ClsfPerACCTest2 * 100),"--Features:",SelectedFeatureList,"--\n")
      
      FinalACCResult_TestDt[cntcg,4+j] <- ClsfPerACCTest2
    }
  # ------------------------------------------------------------------------------------------------------------ #
    BakPath <- paste(sourcepath,"/Workspace/Workspace_AfterTestTestDataset.RData",sep="")
    save.image(BakPath)
  # ------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------ #

  ##### Export Classification Results List to a Excel File #######################################################
  # ------------------------------------------------------------------------------------------------------------ #
  write.xlsx(FinalACCResult_TrainDt, paste(sourcepath,"\\Results\\","FinalACCResult_TrainDt.xlsx",sep=""))
  write.xlsx(FinalACCResult_TestDt, paste(sourcepath,"\\Results\\","FinalACCResult_TestDt.xlsx",sep=""))
  # ------------------------------------------------------------------------------------------------------------ #
}