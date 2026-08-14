# library("torch")
# library("coro")
# library("xlsx")
# # ----------------------------------------------------------------------------------------------------------------------------------- #
# 
# # ----------------------------------------------------------------------------------------------------------------------------------- #
# dataset <- dataset(
#   initialize = function(x, y) {
#     self$x <- x
#     self$y <- y
#   },
#   .getitem = function(i) {
#     list(x = self$x[i,,], y = self$y[i])
#   },
#   .length = function() {
#     self$x$size()[1]
#   }
# )
# # -------------------------------------------------- #
# create_sequences <- function(x, y, timesteps) {
#   n <- x$size()[1] - timesteps
# 
#   x_seq <- torch_zeros(n, timesteps, x$size()[2])
#   y_seq <- torch_zeros(n)
# 
#   for (i in 1:n) {
#     x_seq[i,,] <- x[i:(i+timesteps-1), ]
#     y_seq[i]   <- y[i+timesteps]
#   }
# 
#   list(x_seq, y_seq)
# }
# # ----------------------------------------------------------------------------------------------------------------------------------- #

# 1. UNSW-NB15 ########################################################################################################################
# ----------------------------------------------------------------------------------------------------------------------------------- #
# load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/2.UNSWNB15/UNSWNB15-Encoded_LogScaled_RDA/UNSWNB15Trainingset.Rdata")
# TrainingDataset <- Dataset
# rm(Dataset)
# load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/2.UNSWNB15/UNSWNB15-Encoded_LogScaled_RDA/UNSWNB15Testingset.Rdata")
# TestDataset <- Dataset
# rm(Dataset)
# ClassColumn <- 195
# ----------------------------------------------------------------------------------------------------------------------------------- #

# 2.CICIDS2017-Wednesday ##############################################################################################################
# ----------------------------------------------------------------------------------------------------------------------------------- #
# load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/3.CICIDS2017/2.CICIDS2017-ZScore-Wednesday_DoS-RData/CICIDS2017_Trainingset.Rdata")
# TrainingDataset <- TrainDataset
# rm(TrainDataset)
# load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/3.CICIDS2017/2.CICIDS2017-ZScore-Wednesday_DoS-RData/CICIDS2017_Testingset.Rdata")
# ClassColumn <- 69
# ----------------------------------------------------------------------------------------------------------------------------------- #

# 3.CICIDS2017-All ####################################################################################################################
# ----------------------------------------------------------------------------------------------------------------------------------- #
load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/3.CICIDS2017/7.CICIDS2017-ZScore-All_All-RData/CICIDS2017Trainingset.Rdata")
TrainingDataset <- TrainDataset
rm(TrainDataset)
load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/3.CICIDS2017/7.CICIDS2017-ZScore-All_All-RData/CICIDS2017Testingset.Rdata")
ClassColumn <- 71
# ----------------------------------------------------------------------------------------------------------------------------------- #

# 4.CICIDS2018-All ####################################################################################################################
# ----------------------------------------------------------------------------------------------------------------------------------- #
# load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/4.CSE-CICIDS2018/1.CICIDS2018-ZScore-All-RData/TrainingDataset.RData")
# TrainingDataset <- TrainingDatasetNorm
# load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/4.CSE-CICIDS2018/1.CICIDS2018-ZScore-All-RData/TestDataset.RData")
# TestDataset <- TestDatasetNorm
# rm(TrainingDatasetNorm,TestDatasetNorm)
# ClassColumn <- 71
# ----------------------------------------------------------------------------------------------------------------------------------- #

# 5.CICIoT2023-All ####################################################################################################################
# ----------------------------------------------------------------------------------------------------------------------------------- #
# load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/5.CICIoT2023/1.CICIoT2023-ZScore-All-RData/TrainingDatasetNorm.RData")
# TrainingDataset <- TrainingDatasetNorm
# load(file = "/Users/oslo/Documents/AcademicStudies/2.Datasets/3.IDSDatasets/5.CICIoT2023/1.CICIoT2023-ZScore-All-RData/TestDatasetNorm.RData")
# TestDataset <- TestDatasetNorm
# rm(TrainingDatasetNorm,TestDatasetNorm)
# ClassColumn <- 40
# ----------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------- #
colnames(TrainingDataset)[ClassColumn] <- "ClassColumn"
colnames(TestDataset)[ClassColumn] <- "ClassColumn"

TrainingDatasetX <- TrainingDataset[,-ClassColumn]
TrainingDatasetY <- TrainingDataset[,ClassColumn]

TestDatasetX <- TestDataset[,-ClassColumn]
TestDatasetY <- TestDataset[,ClassColumn]

rm(TrainingDataset, TestDataset)
# ----------------------------------------------------------------------------------------------------------------------------------- #

# Training and Evaluation #############################################################################################################
# ----------------------------------------------------------------------------------------------------------------------------------- #
TrainingDatasetXT <- torch_tensor(as.matrix(TrainingDatasetX), dtype = torch_float())
TrainingDatasetYT <- torch_tensor(TrainingDatasetY, dtype = torch_float())

TestDatasetXT <- torch_tensor(as.matrix(TestDatasetX), dtype = torch_float())
TestDatasetYT <- torch_tensor(TestDatasetY, dtype = torch_float())

train_seq <- create_sequences(TrainingDatasetXT, TrainingDatasetYT, timesteps = 20)
test_seq  <- create_sequences(TestDatasetXT,  TestDatasetYT,  timesteps = 20)
# ------------------------------------------------- #
x_train_seq <- train_seq[[1]]
y_train_seq <- train_seq[[2]]

x_test_seq <- test_seq[[1]]
y_test_seq <- test_seq[[2]]
# ------------------------------------------------- #
set.seed(123)

n <- x_train_seq$size()[1]
val_size <- floor(0.2 * n)

indices <- sample(1:n)

val_idx <- indices[1:val_size]
train_idx <- indices[(val_size + 1):n]

train_ds <- dataset(x_train_seq[train_idx,,], y_train_seq[train_idx])
val_ds   <- dataset(x_train_seq[val_idx,,],   y_train_seq[val_idx])
test_ds  <- dataset(x_test_seq,               y_test_seq)

train_dl <- dataloader(train_ds, batch_size = 64, shuffle = TRUE)
val_dl   <- dataloader(val_ds,   batch_size = 64)
test_dl  <- dataloader(test_ds,  batch_size = 64)
# ------------------------------------------------- #

# ------------------------------------------------- #
model <- nn_module("IDS_Deep_RNN",
                  
   initialize = function(input_size) {
     
     self$rnn1 <- nn_rnn(input_size, 64, batch_first = TRUE)
     self$rnn2 <- nn_rnn(64, 48, batch_first = TRUE)
     self$rnn3 <- nn_rnn(48, 32, batch_first = TRUE)
     self$rnn4 <- nn_rnn(32, 24, batch_first = TRUE)
     self$rnn5 <- nn_rnn(24, 16, batch_first = TRUE)
     self$rnn6 <- nn_rnn(16, 8, batch_first = TRUE)
     self$rnn7 <- nn_rnn(8, 4, batch_first = TRUE)
     self$rnn8 <- nn_rnn(4, 2, batch_first = TRUE)
     
     self$dropout <- nn_dropout(0.1)
     
     self$fc <- nn_linear(2, 1)
   },
   
   forward = function(x) {
     
     # Layer 1
     out1 <- self$rnn1(x)[[1]]
     out1 <- self$dropout(out1)

     # Layer 2
     out2 <- self$rnn2(out1)[[1]]
     out2 <- self$dropout(out2)

     # Layer 3
     out3 <- self$rnn3(out2)[[1]]
     out3 <- self$dropout(out3)

     # Layer 4
     out4 <- self$rnn4(out3)[[1]]
     out4 <- self$dropout(out4)

     # Layer 5
     out5 <- self$rnn5(out4)[[1]]
     out5 <- self$dropout(out5)

     # Layer 6
     out6 <- self$rnn6(out5)[[1]]
     out6 <- self$dropout(out6)

     # Layer 7
     out7 <- self$rnn7(out6)[[1]]
     out7 <- self$dropout(out7)

     # Layer 8
     out8 <- self$rnn8(out7)[[1]]

     # Last timestep
     last <- out8[, dim(out8)[2], ]

     # Fully connected layers
     x <- self$fc(last)
     
     x
   }
)

input_size <- x_train_seq$size()[3]
net <- model(input_size)
criterion <- nn_bce_with_logits_loss()

optimizer <- optim_adam(net$parameters, lr = 0.001)
scheduler <- lr_reduce_on_plateau(
  optimizer,
  mode = "min",        # minimize validation loss
  factor = 0.1,        # reduce LR by half
  patience = 2,        # wait 2 epochs
  min_lr=1e-6,
  verbose = TRUE
)

patience <- 3
best_val_loss <- Inf
counter <- 0

epochs <- 200

TrainingTimeStart <- Sys.time()

for (epoch in 1:epochs) {
  
  # ----- TRAIN -----
  net$train()
  train_loss <- 0
  batches <- 0
  
  coro::loop(for (b in train_dl) {
    
    optimizer$zero_grad()
    
    outputs <- net(b$x)
    targets <- b$y$unsqueeze(2)
    
    loss <- criterion(outputs, targets)
    
    loss$backward()
    optimizer$step()
    
    train_loss <- train_loss + loss$item()
    batches <- batches + 1
  })
  
  train_loss <- train_loss / batches

  # ----- VALIDATION -----
  net$eval()
  val_loss <- 0
  val_batches <- 0
  
  with_no_grad({
    coro::loop(for (b in val_dl) {
      
      outputs <- net(b$x)
      targets <- b$y$unsqueeze(2)
      
      loss <- criterion(outputs, targets)
      
      val_loss <- val_loss + loss$item()
      val_batches <- val_batches + 1
    })
  })
  
  val_loss <- val_loss / val_batches
  
  cat("Epoch:", epoch,
      " - Train Loss:", round(train_loss, 4),
      "Val Loss:", round(val_loss, 4), "\n")
  
  scheduler$step(val_loss)
  
  # ----- EARLY STOPPING -----
  if (val_loss < best_val_loss) {
    
    best_val_loss <- val_loss
    counter <- 0
    
    # ✅ Save BEST model weights
    torch_save(net$state_dict(), "best_model.pt")
    
  } else {
    counter <- counter + 1
  }
  
  if (counter >= patience) {
    cat("\n✅ Early stopping triggered at epoch", epoch, "\n")
    break
  }
}

TrainingTimeElapsed <- Sys.time() - TrainingTimeStart
print(TrainingTimeElapsed)

# ------------------------------------------------- #

RunningTimeStart <- Sys.time()

net <- model(input_size)
net$load_state_dict(torch_load("best_model.pt"))
net$eval()

cat("\n✅ Best model loaded successfully\n")

all_preds <- c()
all_labels <- c()

with_no_grad({
  coro::loop(for (b in test_dl) {
    
    # probs <- net(b$x)
    logits <- net(b$x)
    
    probs <- torch_sigmoid(logits)
    preds <- (probs > 0.5)$to(dtype = torch_int())
    
    all_preds <- c(all_preds, as.numeric(preds))
    all_labels <- c(all_labels, as.numeric(b$y))
  })
})

RunningTimeElapsed <- Sys.time() - RunningTimeStart
print(RunningTimeElapsed)

table(all_preds, all_labels)

TP <- sum((all_labels == 1) & (all_preds == 1))
TN <- sum((all_labels == 0) & (all_preds == 0))
FP <- sum((all_labels == 0) & (all_preds == 1))
FN <- sum((all_labels == 1) & (all_preds == 0))

cat("\nConfusion Matrix:\n")
cat("TP:", TP, " FP:", FP, "\n")
cat("FN:", FN, " TN:", TN, "\n")

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
detection_rate <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
false_alarm_rate <- ifelse((FP + TN) == 0, 0, FP / (FP + TN))

cat("\nEvaluation Metrics:\n")
cat("Accuracy:", round(accuracy * 100, 2), "% \n",sep = "")
cat("Precision:", round(precision * 100, 2), "% \n",sep = "")
cat("Detection Rate:", round(detection_rate * 100, 2), "% \n",sep = "")
cat("False Alarm Rate:", round(false_alarm_rate * 100, 2), "% \n",sep = "")

# ----------------------------------------------------------------------------------------------------------------------------------- #
gc()
# rm(list = ls())
# # ----------------------------------------------------------------------------------------------------------------------------------- #