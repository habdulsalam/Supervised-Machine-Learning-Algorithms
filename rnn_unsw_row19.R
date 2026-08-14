
## ============================================================
## UNSW-NB15 Preprocessing: Log Scaling + Categorical Encoding
## Run this BEFORE rnn_unsw_nb15_row19.R
## Assumes train_df (175341 x 45) and test_df (82332 x 45) are
## already loaded, with column 45 = "label" (0/1 binary target).
## Standard UNSW-NB15 raw column layout:
##   id, dur, proto, service, state, ... , attack_cat (col 44), label (col 45)
##   attack_cat = normal/attack (or multi-class) categorical version
##   of the target, dropped entirely — "label" (0/1) is the target used.
## ============================================================

## ------------------------------------------------------------
## 0. Sanity check column names before doing anything
## ------------------------------------------------------------
stopifnot("label" %in% names(train_df))
stopifnot("label" %in% names(test_df))
stopifnot(all(c("proto", "service", "state") %in% names(train_df)))
stopifnot(all(c("proto", "service", "state") %in% names(test_df)))

## ------------------------------------------------------------
## 1. Set aside "label" (col 45), then drop non-feature columns:
##    id, attack_cat (col 44 — categorical version of the target,
##    not used, since we keep numeric "label" instead)
##    label is stored separately and re-attached at the very end,
##    so it never gets touched by log scaling / encoding.
## ------------------------------------------------------------
train_label <- train_df[["label"]]
test_label  <- test_df[["label"]]

drop_cols <- c("id", "attack_cat")
train_df <- train_df[, !(names(train_df) %in% c(drop_cols, "label"))]
test_df  <- test_df[,  !(names(test_df)  %in% c(drop_cols, "label"))]

## ------------------------------------------------------------
## 2. Log scaling: log(x + 1) on the specified numeric columns
## ------------------------------------------------------------
columns_to_log <- c(
  "dur", "spkts", "dpkts", "sbytes", "dbytes", "rate", "sttl", "dttl",
  "sload", "dload", "sloss", "dloss", "sinpkt", "dinpkt", "sjit", "djit",
  "swin", "stcpb", "dtcpb", "dwin", "tcprtt", "synack", "ackdat", "smean",
  "dmean", "trans_depth", "response_body_len", "ct_srv_src", "ct_state_ttl",
  "ct_dst_ltm", "ct_src_dport_ltm", "ct_dst_sport_ltm", "ct_dst_src_ltm",
  "is_ftp_login", "ct_ftp_cmd", "ct_flw_http_mthd", "ct_src_ltm", "ct_srv_dst"
)

## Only touch columns that actually exist, in case your file
## uses slightly different naming
log_cols_train <- intersect(columns_to_log, names(train_df))
log_cols_test  <- intersect(columns_to_log, names(test_df))

for (col in log_cols_train) train_df[[col]] <- log(train_df[[col]] + 1)
for (col in log_cols_test)  test_df[[col]]  <- log(test_df[[col]]  + 1)

message(sprintf("Log-scaled %d columns in train_df, %d columns in test_df.",
                length(log_cols_train), length(log_cols_test)))

## ------------------------------------------------------------
## 3. One-hot encode proto / service / state
##    Using FIXED canonical category lists (not derived from the
##    data) so train_df and test_df always end up with the exact
##    same set of dummy columns, in the same order.
## ------------------------------------------------------------
proto_levels <- c(
  "tcp","udp","arp","ospf","icmp","igmp","rtp","ddp","ipv6-frag","cftp",
  "wsn","pvp","wb-expak","mtp","pri-enc","sat-mon","cphb","sun-nd","iso-ip","xtp",
  "il","unas","mfe-nsp","3pc","ipv6-route","idrp","bna","swipe","kryptolan","cpnx",
  "rsvp","wb-mon","vmtp","ib","dgp","eigrp","ax.25","gmtp","pnni","sep",
  "pgm","idpr-cmtp","zero","rvd","mobile","narp","fc","pipe","ipcomp","ipv6-no",
  "sat-expak","ipv6-opts","snp","ipcv","br-sat-mon","ttp","tcf","nsfnet-igp","sprite-rpc","aes-sp3-d",
  "sccopmce","sctp","qnx","scps","etherip","aris","pim","compaq-peer","vrrp","iatp",
  "stp","l2tp","srp","sm","isis","smp","fire","ptp","crtp","sps",
  "merit-inp","idpr","skip","any","larp","ipip","micp","encap","ifmp","tp++",
  "a/n","ipv6","i-nlsp","ipx-n-ip","sdrp","tlsp","gre","mhrp","ddx","ippc",
  "visa","secure-vmtp","uti","vines","crudp","iplt","ggp","ip","ipnip","st2",
  "argus","bbn-rcc","egp","emcon","igp","nvp","pup","xnet","chaos","mux",
  "dcn","hmp","prm","trunk-1","xns-idp","leaf-1","leaf-2","rdp","irtp","iso-tp4",
  "netblt","trunk-2","cbt"
)

service_levels <- c("-","ftp","smtp","snmp","http","ftp-data","dns","ssh","radius","pop3","dhcp","ssl","irc")

state_levels <- c("FIN","INT","CON","ECO","REQ","RST","PAR","URN","no","ACC","CLO")

one_hot_encode <- function(df, col, levels_vec, prefix) {
  for (lvl in levels_vec) {
    new_col <- paste0(prefix, "_", make.names(lvl))
    df[[new_col]] <- ifelse(df[[col]] == lvl, 1, 0)
  }
  df
}

train_df <- one_hot_encode(train_df, "proto",   proto_levels,   "proto")
train_df <- one_hot_encode(train_df, "service", service_levels, "service")
train_df <- one_hot_encode(train_df, "state",   state_levels,   "state")

test_df  <- one_hot_encode(test_df,  "proto",   proto_levels,   "proto")
test_df  <- one_hot_encode(test_df,  "service", service_levels, "service")
test_df  <- one_hot_encode(test_df,  "state",   state_levels,   "state")

## Drop the original text categorical columns now that they're encoded
train_df <- train_df[, !(names(train_df) %in% c("proto", "service", "state"))]
test_df  <- test_df[,  !(names(test_df)  %in% c("proto", "service", "state"))]

## ------------------------------------------------------------
## 4. Align columns between train_df and test_df
##    (guards against any mismatch, e.g. a category present in
##    one set's data types but not the other)
## ------------------------------------------------------------
common_cols <- intersect(names(train_df), names(test_df))
missing_in_test  <- setdiff(names(train_df), names(test_df))
missing_in_train <- setdiff(names(test_df), names(train_df))

if (length(missing_in_test) > 0) {
  message("Columns in train_df not in test_df (this shouldn't happen with fixed levels): ",
          paste(missing_in_test, collapse = ", "))
}
if (length(missing_in_train) > 0) {
  message("Columns in test_df not in train_df (this shouldn't happen with fixed levels): ",
          paste(missing_in_train, collapse = ", "))
}

## ------------------------------------------------------------
## 5. Align feature columns between train_df and test_df, then
##    re-attach the untouched "label" column as the last column
## ------------------------------------------------------------
feature_cols <- intersect(names(train_df), names(test_df))
train_df <- train_df[, feature_cols]
test_df  <- test_df[,  feature_cols]

train_df$label <- train_label
test_df$label  <- test_label

## ------------------------------------------------------------
## 6. Sanity checks before handing off to the RNN script
## ------------------------------------------------------------
stopifnot(identical(names(train_df), names(test_df)))
stopifnot(all(sapply(train_df, is.numeric)))
stopifnot(all(sapply(test_df, is.numeric)))

## ------------------------------------------------------------
## 7. Final named outputs
## ------------------------------------------------------------
TraindatasetNORM <- train_df
TestdatasetNORM  <- test_df

cat(sprintf("TraindatasetNORM: %d rows, %d columns (incl. label)\n",
            nrow(TraindatasetNORM), ncol(TraindatasetNORM)))
cat(sprintf("TestdatasetNORM:  %d rows, %d columns (incl. label)\n",
            nrow(TestdatasetNORM), ncol(TestdatasetNORM)))
cat("Label distribution (TraindatasetNORM):\n"); print(table(TraindatasetNORM$label))
cat("Label distribution (TestdatasetNORM):\n");  print(table(TestdatasetNORM$label))

## TraindatasetNORM and TestdatasetNORM are now ready to feed
## directly into rnn_unsw_nb15_row19.R (rename train_df/test_df
## there, or just point the RNN script at these two objects)


















































# ============================================================
# RNN (SimpleRNN) for Binary Classification on UNSW-NB15
# Complete Feature Dataset
# Reproduces Table 1, Row 19 configuration:
#   Hidden layers        : 3
#   Node sizes            : 64, 16, 2
#   Time steps             : 10
#   Epochs                 : 19
#   Learning rate          : 0.001
#   Batch size              : 64
#   Dropout                 : 0.10
# ============================================================

# ---- 0. Libraries -------------------------------------------------
 install.packages("keras3")   # if not already installed
 keras3::install_keras()      # installs TensorFlow backend, once
library(keras3)
library(dplyr)

# ---- 1. Reproducibility -------------------------------------------
SEED <- 42
set.seed(SEED)
tensorflow::set_random_seed(SEED)

train_df <- TraindatasetNORM
test_df  <- TestdatasetNORM

# ---- 2. Hyperparameters (Row 19) -----------------------------------
TIME_STEPS   <- 10
HIDDEN_SIZES <- c(64, 16, 2)   # 3 hidden (recurrent) layers, sizes per Table 1
EPOCHS       <- 19
LR           <- 0.001
BATCH_SIZE   <- 64
DROPOUT      <- 0.10

# ---- 3. Load YOUR train/test data ----------------------------------
# Replace these two lines with your actual loading code.
# Each should be a data.frame: feature columns + a binary "label" column
# (0 = normal, 1 = attack), already using the COMPLETE feature set
# (i.e. no feature selection applied).
#
# train_df <- read.csv("UNSW_NB15_training-set.csv")
# test_df  <- read.csv("UNSW_NB15_testing-set.csv")

#test_df <- read.csv("C:\\data\\UNSWNB15Testingset.csv") # <-- point to your file
#train_df <- read.csv("C:\\data\\UNSWNB15Trainingset (1).csv")    # <-- point to your file

label_col <- "label"   # <-- change if your binary target column is named differently

# ---- 4. Separate features / labels ---------------------------------
X_train_raw <- train_df %>% select(-all_of(label_col)) %>% as.matrix()
y_train_raw <- train_df[[label_col]]

X_test_raw  <- test_df %>% select(-all_of(label_col)) %>% as.matrix()
y_test_raw  <- test_df[[label_col]]

# Make sure everything is numeric (categorical cols must already be
# one-hot / label encoded exactly as in the paper's preprocessing).
storage.mode(X_train_raw) <- "double"
storage.mode(X_test_raw)  <- "double"

# ---- 5. Scale features (fit on train, apply to test) ----------------
train_mean <- apply(X_train_raw, 2, mean)
train_sd   <- apply(X_train_raw, 2, sd)
train_sd[train_sd == 0] <- 1  # avoid divide-by-zero on constant columns

X_train_scaled <- scale(X_train_raw, center = train_mean, scale = train_sd)
X_test_scaled  <- scale(X_test_raw,  center = train_mean, scale = train_sd)

# ---- 6. Reshape into (samples, time_steps, features_per_step) -------
# RNNs need 3D input. The common approach for tabular IDS data is to
# reshape the flat feature vector of length F into (TIME_STEPS, F/TIME_STEPS).
# This REQUIRES the number of features to be divisible by TIME_STEPS.
n_features <- ncol(X_train_scaled)

if (n_features %% TIME_STEPS != 0) {
  stop(sprintf(
    paste0("Number of features (%d) is not divisible by TIME_STEPS (%d).\n",
           "Either pad/trim your feature set to a multiple of %d, or adjust ",
           "TIME_STEPS to a divisor of %d before reshaping."),
    n_features, TIME_STEPS, TIME_STEPS, n_features
  ))
}

features_per_step <- n_features / TIME_STEPS

reshape_to_sequence <- function(X, time_steps, features_per_step) {
  array_reshape(X, dim = c(nrow(X), time_steps, features_per_step))
}

X_train <- reshape_to_sequence(X_train_scaled, TIME_STEPS, features_per_step)
X_test  <- reshape_to_sequence(X_test_scaled,  TIME_STEPS, features_per_step)

# ---- 7. One-hot encode labels (binary -> 2 output units, matches
#         the "2" in the Row 19 node sizes = softmax output layer) -----
y_train <- to_categorical(y_train_raw, num_classes = 2)
y_test  <- to_categorical(y_test_raw,  num_classes = 2)

# ---- 8. Build the model ---------------------------------------------
# 3 hidden layers: SimpleRNN(64) -> SimpleRNN(16) -> Dense(2, softmax)
# Dropout (0.10) applied after each recurrent layer.
model <- keras_model_sequential(input_shape = c(TIME_STEPS, features_per_step)) %>%
  layer_simple_rnn(units = HIDDEN_SIZES[1], return_sequences = TRUE) %>%
  layer_dropout(rate = DROPOUT) %>%
  layer_simple_rnn(units = HIDDEN_SIZES[2], return_sequences = FALSE) %>%
  layer_dropout(rate = DROPOUT) %>%
  layer_dense(units = HIDDEN_SIZES[3], activation = "softmax")  # units = 2 (binary)

model %>% compile(
  optimizer = optimizer_adam(learning_rate = LR),
  loss      = "categorical_crossentropy",
  metrics   = c("accuracy")
)

summary(model)

# ---- 9. Train ----------------------------------------------------
start_train <- Sys.time()

history <- model %>% fit(
  X_train, y_train,
  epochs          = EPOCHS,
  batch_size      = BATCH_SIZE,
  validation_split = 0.1,
  verbose         = 1
)

training_time <- Sys.time() - start_train
cat(sprintf("Training time: %s\n", format(training_time)))

# ---- 10. Predict / evaluate on test set ----------------------------
start_run <- Sys.time()

pred_probs <- model %>% predict(X_test, batch_size = BATCH_SIZE)
pred_class <- apply(pred_probs, 1, which.max) - 1   # 0/1 predicted labels
true_class <- y_test_raw

running_time <- Sys.time() - start_run
cat(sprintf("Running (inference) time: %s\n", format(running_time)))

# ---- 11. Confusion matrix + metrics (ACC, PREC, DR, FAR) ------------
TP <- sum(pred_class == 1 & true_class == 1)
TN <- sum(pred_class == 0 & true_class == 0)
FP <- sum(pred_class == 1 & true_class == 0)
FN <- sum(pred_class == 0 & true_class == 1)

ACC  <- (TP + TN) / (TP + TN + FP + FN)
PREC <- TP / (TP + FP)
DR   <- TP / (TP + FN)          # Detection Rate = Recall/Sensitivity
FAR  <- FP / (FP + TN)          # False Alarm Rate

cat("\n===== Results =====\n")
cat(sprintf("ACC  : %.2f%%\n", ACC  * 100))
cat(sprintf("PREC : %.2f%%\n", PREC * 100))
cat(sprintf("DR   : %.2f%%\n", DR   * 100))
cat(sprintf("FAR  : %.2f%%\n", FAR  * 100))

confusion_matrix <- matrix(c(TN, FP, FN, TP), nrow = 2, byrow = TRUE,
                            dimnames = list(Actual = c("Normal(0)", "Attack(1)"),
                                             Predicted = c("Normal(0)", "Attack(1)")))
print(confusion_matrix)
