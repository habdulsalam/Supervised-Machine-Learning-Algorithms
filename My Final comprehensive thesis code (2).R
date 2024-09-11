library("foreign")
library(dplyr)

library(class)
library (gmodels)
library(C50)
TrainDatasetNorm<- TrainDatasetNorm[rowSums(is.na(TrainDatasetNorm)) == 0, ]
TestDatasetNorm<- TestDatasetNorm[rowSums(is.na(TestDatasetNorm)) == 0, ]


TrainDataset <- read.arff(file ="C:\\KDDTrain+.arff" )

TestDataset <- read.arff(file = "C:\\KDDTest+.arff")

 
TrainDataset <- read.csv(file ="C:\\UNSWNB15Trainingset (1).csv" )
TestDataset <- read.csv(file = "C:\\UNSWNB15Testingset.csv")



TrainDataset =TrainDataset[,c(-1,-45)]

TestDataset =TestDataset[,c(-1,-45)]


TrainDataset =TrainDataset[,c(-4)]

TestDataset =TestDataset[,c(-4)]


FsTrain =FsTrain[,c(-17)]

FsTest =FsTest[,c(-17)]













#--------------------------------------------------------------ZSCORE NORMALIZATION FOR CICIDS2017

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

#---------------------------------------------- To Include class column



save(TrainDatasetNorm, file = "CICISFRI2017TRAINzscore.Rda")

save(TestDatasetNorm, file = "CICIDSFRI2017TESTzscore.Rda")


#-------------------------------------------------------------------------------------UNSWNB15 DATASET. LOG SCALLING
columns_to_log2 <- c( "dur","spkts","dpkts","sbytes","dbytes","rate","sttl","dttl","sload","dload","sloss",
                     "dloss" , "sinpkt","dinpkt","sjit","djit","swin","stcpb",
                     "dtcpb","dwin","tcprtt","synack","ackdat","smean", "dmean",
                     "trans_depth", "response_body_len", "ct_srv_src","ct_state_ttl","ct_dst_ltm","ct_src_dport_ltm","ct_dst_sport_ltm",
                     "ct_dst_src_ltm","is_ftp_login","ct_ftp_cmd","ct_flw_http_mthd","ct_src_ltm","ct_srv_dst"
)

for (column in columns_to_log2) {
  TestDataset[[column]] <- log(TestDataset[[column]] + 1)
}


columns_to_log1 <- c( "dur","spkts","dpkts","sbytes","dbytes","rate","sttl","dttl","sload","dload","sloss",
                     "dloss" , "sinpkt","dinpkt","sjit","djit","swin","stcpb",
                     "dtcpb","dwin","tcprtt","synack","ackdat","smean", "dmean",
                     "trans_depth", "response_body_len", "ct_srv_src","ct_state_ttl","ct_dst_ltm","ct_src_dport_ltm","ct_dst_sport_ltm",
                     "ct_dst_src_ltm","is_ftp_login","ct_ftp_cmd","ct_flw_http_mthd","ct_src_ltm","ct_srv_dst"
)

for (column in columns_to_log1) {
  TrainDataset[[column]] <- log(TrainDataset[[column]] + 1)
}



#---------------------------------------------------ENCODING FOR CATEGORICAL FEATURES UNSWNB15(SERVICE,STATE AND PROTO)


proto <- c("tcp", "udp", "arp", "ospf", "icmp", "igmp", "rtp", "ddp", "ipv6-frag", "cftp",
    "wsn", "pvp", "wb-expak", "mtp", "pri-enc", "sat-mon", "cphb", "sun-nd", "iso-ip", "xtp",
    "il", "unas", "mfe-nsp", "3pc", "ipv6-route", "idrp", "bna", "swipe", "kryptolan", "cpnx",
    "rsvp", "wb-mon", "vmtp", "ib", "dgp", "eigrp", "ax.25", "gmtp", "pnni", "sep",
    "pgm", "idpr-cmtp", "zero", "rvd", "mobile", "narp", "fc", "pipe", "ipcomp", "ipv6-no",
    "sat-expak", "ipv6-opts", "snp", "ipcv", "br-sat-mon", "ttp", "tcf", "nsfnet-igp", "sprite-rpc", "aes-sp3-d",
    "sccopmce", "sctp", "qnx", "scps", "etherip", "aris", "pim", "compaq-peer", "vrrp", "iatp",
    "stp", "l2tp", "srp", "sm", "isis", "smp", "fire", "ptp", "crtp", "sps",
    "merit-inp", "idpr", "skip", "any", "larp", "ipip", "micp", "encap", "ifmp", "tp++",
    "a/n", "ipv6", "i-nlsp", "ipx-n-ip", "sdrp", "tlsp", "gre", "mhrp", "ddx", "ippc",
    "visa", "secure-vmtp", "uti", "vines", "crudp", "iplt", "ggp", "ip", "ipnip", "st2",
    "argus", "bbn-rcc", "egp", "emcon", "igp", "nvp", "pup", "xnet", "chaos", "mux",
    "dcn", "hmp", "prm", "trunk-1", "xns-idp", "leaf-1", "leaf-2", "rdp", "irtp", "iso-tp4",
    "netblt", "trunk-2", "cbt"

)

for (proto in proto) {
  FsTrain[[proto]] <- ifelse(FsTrain$proto == proto, 1, 0)
}

service <- c( "-", "ftp", "smtp", "snmp", "http", "ftp-data", "dns", "ssh", "radius", "pop3", "dhcp", "ssl", "irc")

for (service in service) {
  FsTrain[[service]] <- ifelse( FsTrain$service == service, 1, 0)
}
state<- c("FIN", "INT", "CON", "ECO", "REQ", "RST", "PAR", "URN", "no","ACC", "CLO")

for (state in state) {
  TrainDataset[[state]] <- ifelse( TrainDataset$state == state, 1, 0)
}



proto <- c(
  "udp", "arp", "tcp", "igmp", "ospf", "sctp", "gre", "ggp", "ip", "ipnip",
  "st2", "argus", "chaos", "egp", "emcon", "nvp", "pup", "xnet", "mux", "dcn",
  "hmp", "prm", "trunk-1", "trunk-2", "xns-idp", "leaf-1", "leaf-2", "irtp", "rdp", "netblt",
  "mfe-nsp", "merit-inp", "3pc", "idpr", "ddp", "idpr-cmtp", "tp++", "ipv6", "sdrp", "ipv6-frag",
  "ipv6-route", "idrp", "mhrp", "i-nlsp", "rvd", "mobile", "narp", "skip", "tlsp", "ipv6-no",
  "any", "ipv6-opts", "cftp", "sat-expak", "ippc", "kryptolan", "sat-mon", "cpnx", "wsn", "pvp",
  "br-sat-mon", "sun-nd", "wb-mon", "vmtp", "ttp", "vines", "nsfnet-igp", "dgp", "eigrp", "tcf",
  "sprite-rpc", "larp", "mtp", "ax.25", "ipip", "aes-sp3-d", "micp", "encap", "pri-enc", "gmtp",
  "ifmp", "pnni", "qnx", "scps", "cbt", "bbn-rcc", "igp", "bna", "swipe", "visa",
  "ipcv", "cphb", "iso-tp4", "wb-expak", "sep", "secure-vmtp", "xtp", "il", "rsvp", "unas",
  "fc", "iso-ip", "etherip", "pim", "aris", "a/n", "ipcomp", "snp", "compaq-peer", "ipx-n-ip",
  "pgm", "vrrp", "l2tp", "zero", "ddx", "iatp", "stp", "srp", "uti", "sm",
  "smp", "isis", "ptp", "fire", "crtp", "crudp", "sccopmce", "iplt", "pipe", "sps",
  "ib","icmp", "rtp"
)

for (proto in proto) {
  FsTest[[proto]] <- ifelse(FsTest$proto == proto, 1, 0)
}

service <- c( "-", "http", "ftp", "ftp-data", "smtp", "pop3", "dns", "snmp", "ssl", "dhcp", "irc", "radius", "ssh")

for (service in service) {
  FsTest[[service]] <- ifelse(FsTest$service == service, 1, 0)
}
state<- c("INT", "FIN", "REQ", "ACC", "CON", "RST", "CLO","ECO", "PAR", "URN", "no" )

for (state in state) {
  TestDataset[[state]] <- ifelse(TestDataset$state == state, 1, 0)
}


#Removing the categorıcal feature, proto and service.

TrainDataset = TrainDataset[,-(2:4)]
TestDataset = TestDataset[,-(2:4)]


TrainDataset = TrainDataset[,-97]
TestDataset = TestDataset[,-97]



#---------------------------------------------------ENCODING FOR CATEGORICAL FEATURES NSLKDD (SERVICE,flag AND PROTOcol_type)


services <- c( "private", "ftp_data", "eco_i", "telnet", "http", "smtp", "ftp", "ldap", "pop_3", "courier", "discard", "ecr_i",
               "imap4", "domain_u", "mtp", "systat", "iso_tsap", "other", "csnet_ns", "finger", "uucp", "whois", "netbios_ns", "link",
               "Z39_50", "sunrpc", "auth", "netbios_dgm", "uucp_path", "vmnet", "domain", "name", "pop_2", "http_443", "urp_i", "login",
               "gopher", "exec", "time", "remote_job", "ssh", "kshell", "sql_net", "shell", "hostnames", "echo", "daytime", "pm_dump",
               "IRC", "netstat", "ctf", "nntp", "netbios_ssn", "tim_i", "supdup", "bgp", "nnsp", "rje", "printer", "efs",
               "X11", "ntp_u", "klogin", "tftp_u","red_i" ,    "urh_i" ,    "http_8001", "aol"  ,     "http_2784" ,"harvest"  )

for (service in services) {
  TestDataset[[service]] <- ifelse(TestDataset$service == service, 1, 0)
}

flag <- c("REJ", "SF", "RSTO", "S0", "RSTR", "SH", "S3", "S2", "S1", "RSTOS0", "OTH" )

for (flag in flag) {
  TestDataset[[flag]] <- ifelse(TestDataset$flag == flag, 1, 0)
}

protocol_type <- c("tcp", "udp", "icmp")

for (protocol_type in protocol_type) {
  TestDataset[[protocol_type]] <- ifelse( TestDataset$protocol_type == protocol_type, 1, 0)
}




services <- c("ftp_data", "other", "private", "http", "remote_job", "name", "netbios_ns", "eco_i", "mtp", "telnet", "finger", "domain_u",
              "supdup", "uucp_path", "Z39_50", "smtp", "csnet_ns", "uucp", "netbios_dgm", "urp_i", "auth", "domain", "ftp", "bgp",
              "ldap", "ecr_i", "gopher", "vmnet", "systat", "http_443", "efs", "whois", "imap4", "iso_tsap", "echo", "klogin",
              "link", "sunrpc", "login", "kshell", "sql_net", "time", "hostnames", "exec", "ntp_u", "discard", "nntp", "courier",
              "ctf", "ssh", "daytime", "shell", "netstat", "pop_3", "nnsp", "IRC", "pop_2", "printer", "tim_i", "pm_dump",
              "red_i", "netbios_ssn", "rje", "X11", "urh_i", "http_8001", "aol", "http_2784", "tftp_u", "harvest")

for (service in services) {
  TrainDataset[[service]] <- ifelse(TrainDataset$service == service, 1, 0)
}

flag <- c("SF", "S0", "REJ", "RSTR", "SH", "RSTO", "S1", "RSTOS0", "S3", "S2", "OTH")

for (flag in flag) {
  TrainDataset[[flag]] <- ifelse(TrainDataset$flag == flag, 1, 0)
}

protocol_type <- c("tcp", "udp", "icmp")

for (protocol_type in protocol_type) {
  TrainDataset[[protocol_type]] <- ifelse( TrainDataset$protocol_type == protocol_type, 1, 0)
}

TestDataset = TestDataset[,-(2:4)]
TrainDataset = TrainDataset[,-(2:4)]



#-------------------------------------------------------------------------------------NSLKDD DATASET. LOG SCALLING

Test<- c("duration", "src_bytes", "dst_bytes", "wrong_fragment",
                    "urgent", "hot", "num_failed_logins", "num_compromised",
                    "su_attempted", "num_root", "num_file_creations",
                    "num_shells", "num_access_files",
                    "count", "srv_count", "serror_rate", "srv_serror_rate",
                    "rerror_rate", "srv_rerror_rate", "same_srv_rate", "diff_srv_rate",
                    "srv_diff_host_rate", "dst_host_count", "dst_host_srv_count",
                    "dst_host_same_srv_rate", "dst_host_diff_srv_rate",
                    "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate",
                    "dst_host_serror_rate", "dst_host_srv_serror_rate",
                    "dst_host_rerror_rate", "dst_host_srv_rerror_rate")

for (column in Test) {
  TestDataset[[column]] <- log(TestDataset[[column]] + 1)
}


columns_to_log1<- c("duration", "src_bytes", "dst_bytes", "wrong_fragment",
                    "urgent", "hot", "num_failed_logins", "num_compromised",
                    "su_attempted", "num_root", "num_file_creations",
                    "num_shells", "num_access_files",
                    "count", "srv_count", "serror_rate", "srv_serror_rate",
                    "rerror_rate", "srv_rerror_rate", "same_srv_rate", "diff_srv_rate",
                    "srv_diff_host_rate", "dst_host_count", "dst_host_srv_count",
                    "dst_host_same_srv_rate", "dst_host_diff_srv_rate",
                    "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate",
                    "dst_host_serror_rate", "dst_host_srv_serror_rate",
                    "dst_host_rerror_rate", "dst_host_srv_rerror_rate")

for (column in columns_to_log1) {
  TrainDataset[[column]] <- log(TrainDataset[[column]] + 1)
}


save(TestDataset, file = "UNSWNB15NEWTestLog.Rda")

save(TrainDataset, file = "UNSWNB15NEWTrainLog.Rda")





#-----------------------------------------------------------------------feature selected FROM SVM CICID2017 Friday

FsTrain = TrainDatasetNorm[, c("Bwd.Packet.Length.Mean","Avg.Bwd.Segment.Size","Idle.Mean","Idle.Max","Flow.IAT.Std","Flow.IAT.Max","Bwd.Packet.Length.Std","Bwd.Packet.Length.Max","ClassColumn")]

FsTest= TestDatasetNorm[, c("Bwd.Packet.Length.Mean","Avg.Bwd.Segment.Size","Idle.Mean","Idle.Max","Flow.IAT.Std","Flow.IAT.Max","Bwd.Packet.Length.Std","Bwd.Packet.Length.Max","ClassColumn")]


FsTrain = TrainDatasetNorm[, c("Destination.Port", "Total.Length.of.Bwd.Packets", "Bwd.Packet.Length.Mean","Flow.Bytes.s", "Flow.IAT.Mean","Bwd.Header.Length","Min.Packet.Length","Down.Up.Ratio","Subflow.Bwd.Bytes","Init_Win_bytes_forward",
                               "Init_Win_bytes_backward","Idle.Std","Idle.Max","ClassColumn")]

FsTest= TestDatasetNorm[, c("Destination.Port", "Total.Length.of.Bwd.Packets", "Bwd.Packet.Length.Mean","Flow.Bytes.s", "Flow.IAT.Mean","Bwd.Header.Length","Min.Packet.Length","Down.Up.Ratio","Subflow.Bwd.Bytes","Init_Win_bytes_forward",
                            "Init_Win_bytes_backward","Idle.Std","Idle.Max","ClassColumn")]

FsTrain = TrainDatasetNorm[, c("Destination.Port","Total.Length.of.Bwd.Packets","Bwd.Packet.Length.Mean","Flow.Bytes.s","Flow.IAT.Mean","Bwd.Header.Length",
                               "Min.Packet.Length","Down.Up.Ratio","Subflow.Bwd.Bytes","Init_Win_bytes_forward","Init_Win_bytes_backward","Idle.Std","Idle.Max","Label")]

FsTest= TestDatasetNorm[,c("Destination.Port","Total.Length.of.Bwd.Packets","Bwd.Packet.Length.Mean","Flow.Bytes.s","Flow.IAT.Mean","Bwd.Header.Length",
                           "Min.Packet.Length","Down.Up.Ratio","Subflow.Bwd.Bytes","Init_Win_bytes_forward","Init_Win_bytes_backward","Idle.Std","Idle.Max","Label")]


FsTrain = TrainDatasetNorm[, c("Total.Length.of.Fwd.Packets","Subflow.Fwd.Bytes","Active.Max","Flow.IAT.Min","min_seg_size_forward","Init_Win_bytes_forward","Destination.Port",
                               "Fwd.Packet.Length.Max","Flow.IAT.Mean","Active.Std","Fwd.Packet.Length.Mean","Avg.Fwd.Segment.Size","Active.Mean","Fwd.Packet.Length.Std","Down.Up.Ratio",
                               "URG.Flag.Count","Label")]

FsTest= TestDatasetNorm[, c("Total.Length.of.Fwd.Packets","Subflow.Fwd.Bytes","Active.Max","Flow.IAT.Min","min_seg_size_forward","Init_Win_bytes_forward","Destination.Port",
                            "Fwd.Packet.Length.Max","Flow.IAT.Mean","Active.Std","Fwd.Packet.Length.Mean","Avg.Fwd.Segment.Size","Active.Mean","Fwd.Packet.Length.Std","Down.Up.Ratio",
                            "URG.Flag.Count","Label")]



FsTrain = TrainDatasetNorm[, c("Bwd.Packet.Length.Std","Bwd.Packet.Length.Max","Packet.Length.Std","Packet.Length.Variance",
                                "Avg.Bwd.Segment.Size","Bwd.Packet.Length.Mean", "Packet.Length.Mean", "Average.Packet.Size", "Avg.Fwd.Segment.Size",
                                "Fwd.Packet.Length.Mean","Flow.Bytes.s","Fwd.Packet.Length.Max","Destination.Port","Init_Win_bytes_backward","Bwd.Packets.s","Fwd.IAT.Mean",
                                "Fwd.IAT.Max","Fwd.IAT.Total","Total.Fwd.Packets","Fwd.IAT.Std","Flow.IAT.Std","Label")]

FsTest= TestDatasetNorm[, c("Bwd.Packet.Length.Std","Bwd.Packet.Length.Max","Packet.Length.Std","Packet.Length.Variance",
                            "Avg.Bwd.Segment.Size","Bwd.Packet.Length.Mean", "Packet.Length.Mean", "Average.Packet.Size", "Avg.Fwd.Segment.Size",
                            "Fwd.Packet.Length.Mean","Flow.Bytes.s","Fwd.Packet.Length.Max","Destination.Port","Init_Win_bytes_backward","Bwd.Packets.s","Fwd.IAT.Mean",
                            "Fwd.IAT.Max","Fwd.IAT.Total","Total.Fwd.Packets","Fwd.IAT.Std","Flow.IAT.Std","Label")]





FsTrain = TrainDatasetNorm[, c("Bwd.Packet.Length.Std","Bwd.Packet.Length.Max","Packet.Length.Std","Packet.Length.Variance",
                               "Avg.Bwd.Segment.Size","Bwd.Packet.Length.Mean", "Packet.Length.Mean", "Average.Packet.Size", "Avg.Fwd.Segment.Size",
                               "Fwd.Packet.Length.Mean","Flow.Bytes.s","Fwd.Packet.Length.Max","Destination.Port","Init_Win_bytes_backward","Bwd.Packets.s","Fwd.IAT.Mean",
                               "Fwd.IAT.Max","Fwd.IAT.Total","Total.Fwd.Packets","Fwd.IAT.Std","Flow.IAT.Std","Label")]


FsTrain = TrainDatasetNorm[, c("Fwd.Packet.Length.Max", "Total.Length.of.Fwd.Packets", "Destination.Port",
                               "Bwd.Packet.Length.Min","Flow.IAT.Mean", "FIN.Flag.Count", "Flow.IAT.Min", "Bwd.Packets.s", "Fwd.Packet.Length.Std",
                               "Bwd.Header.Length","Flow.Bytes.s","Total.Backward.Packets","Init_Win_bytes_forward","Init_Win_bytes_backward","ACK.Flag.Count","ClassColumn")]

FsTest= TestDatasetNorm[, c("Fwd.Packet.Length.Max", "Total.Length.of.Fwd.Packets", "Destination.Port",
                               "Bwd.Packet.Length.Min","Flow.IAT.Mean", "FIN.Flag.Count", "Flow.IAT.Min", "Bwd.Packets.s", "Fwd.Packet.Length.Std",
                               "Bwd.Header.Length","Flow.Bytes.s","Total.Backward.Packets","Init_Win_bytes_forward","Init_Win_bytes_backward","ACK.Flag.Count","ClassColumn")]


FsTrain = TrainDatasetNorm[, c("Bwd.Packet.Length.Std","Packet.Length.Mean", "Flow.IAT.Std","Fwd.IAT.Min","Total.Backward.Packets","Fwd.IAT.Std","Bwd.IAT.Std","Idle.Min","Label")]

FsTest= TestDatasetNorm[, c("Bwd.Packet.Length.Mean","Avg.Bwd.Segment.Size","Bwd.Packet.Length.Std","Bwd.Packet.Length.Max","Max.Packet.Length",
                            "Packet.Length.Mean","Average.Packet.Size","Flow.IAT.Max","Fwd.IAT.Max","Idle.Max",
                            "Fwd.IAT.Std","Idle.Mean","URG.Flag.Count","Packet.Length.Std","Packet.Length.Variance","Idle.Min","ClassColumn")]
#-----------------------------------------------------------------------feature selected FROM SVM UNBW15

FsTrain = TrainDataset [, c("dttl","dtcpb","sttl","smean","stcpb","ct_state_ttl","sjit","sbytes","dload","ct_srv_dst","sload","igmp","ct_srv_src","dloss",
                            "ct_flw_http_mthd","dbytes","dmean","dns","rate","ct_dst_sport_ltm","dwin","arp","dinpkt","ct_dst_ltm","spkts","-","ackdat",
                            "smtp","ftp","icmp","djit","response_body_len","label")]

FsTest = TestDataset [,c("dttl","dtcpb","sttl","smean","stcpb","ct_state_ttl","sjit","sbytes","dload","ct_srv_dst","sload","igmp","ct_srv_src","dloss",
                         "ct_flw_http_mthd","dbytes","dmean","dns","rate","ct_dst_sport_ltm","dwin","arp","dinpkt","ct_dst_ltm","spkts","-","ackdat",
                         "smtp","ftp","icmp","djit","response_body_len","label")]


FsTrain = TrainDataset [, c("sttl","ct_state_ttl","dload","dmean","dbytes","dpkts","label")]

FsTest = TestDataset [,c("sttl","ct_state_ttl","dload","dmean","dbytes","dpkts","label")]





FsTrain = TrainDataset [, c("dttl","dtcpb","sttl","smean","stcpb","ct_state_ttl","sjit","sbytes","dload","ct_srv_dst",
                            "sload","igmp","ct_srv_src","dloss","ct_flw_http_mthd","dbytes","dmean","dns","rate","ct_dst_sport_ltm",
                            "dwin","arp","dinpkt","ct_dst_ltm","spkts","-","ackdat","smtp","ftp","icmp","djit","response_body_len","attack_cat")]

FsTest = TestDataset [,c("dttl","dtcpb","sttl","smean","stcpb","ct_state_ttl","sjit","sbytes","dload","ct_srv_dst",
                         "sload","igmp","ct_srv_src","dloss","ct_flw_http_mthd","dbytes","dmean","dns","rate","ct_dst_sport_ltm",
                         "dwin","arp","dinpkt","ct_dst_ltm","spkts","-","ackdat","smtp","ftp","icmp","djit","response_body_len","attack_cat")]



#-----------------------------------------------------------------------feature selected FROM svm NSLKDD
str(Train)

FsTrain = TrainDataset[, c("service","src_bytes","dst_bytes","land","urgent","num_failed_logins","num_compromised","num_root",
                           "num_access_files","is_guest_login","rerror_rate","srv_rerror_rate","same_srv_rate",
                           "diff_srv_rate","dst_host_srv_diff_host_rate","dst_host_srv_serror_rate","dst_host_rerror_rate","class")]


FsTest = TestDataset[, c("service","src_bytes","dst_bytes","land","urgent","num_failed_logins","num_compromised","num_root",
                         "num_access_files","is_guest_login","rerror_rate","srv_rerror_rate","same_srv_rate",
                         "diff_srv_rate","dst_host_srv_diff_host_rate","dst_host_srv_serror_rate","dst_host_rerror_rate","class")]



FsTrain = TrainDataset[, c("src_bytes","flag","dst_bytes","root_shell","srv_serror_rate","same_srv_rate","diff_srv_rate",
                           "dst_host_srv_diff_host_rate","dst_host_srv_serror_rate","class")]


FsTest = TestDataset[, c("src_bytes","flag","dst_bytes","root_shell","srv_serror_rate","same_srv_rate","diff_srv_rate",
                         "dst_host_srv_diff_host_rate","dst_host_srv_serror_rate","class")]


FsTrain = TraindatasetNorm[, c("src_bytes_86","class_123" )]


FsTest = TestdatasetNorm[, c("src_bytes_86","ClassColumn" )]

FsTrain = TrainDatasetNorm[, c("syn_cnt","pkt_len_min","pkt_len_max","down_up_ratio","fl_dur","bw_win_byt","l_bw_pkt","Label" )]


FsTest = TestDatasetNorm[, c("syn_cnt","pkt_len_min","pkt_len_max","down_up_ratio","fl_dur","bw_win_byt","l_bw_pkt","Label" )]

FsTrain = TrainDataset[, c("service", "src_bytes","flag","dst_host_srv_diff_host_rate","num_compromised","num_failed_logins","dst_host_same_src_port_rate",
  "hot","root_shell","dst_bytes","rerror_rate","land","dst_host_srv_serror_rate","dst_host_count","count","dst_host_same_srv_rate",
  "duration","dst_host_diff_srv_rate","same_srv_rate","dst_host_rerror_rate","srv_count","num_root","dst_host_srv_count","dst_host_serror_rate",
  "protocol_type", "num_file_creations", "serror_rate","class")]


FsTest = TestDataset[, c("service", "src_bytes","flag","dst_host_srv_diff_host_rate","num_compromised","num_failed_logins","dst_host_same_src_port_rate",
                          "hot","root_shell","dst_bytes","rerror_rate","land","dst_host_srv_serror_rate","dst_host_count","count","dst_host_same_srv_rate",
                          "duration","dst_host_diff_srv_rate","same_srv_rate","dst_host_rerror_rate","srv_count","num_root","dst_host_srv_count","dst_host_serror_rate",
                          "protocol_type", "num_file_creations", "serror_rate","class")]

#------------------------------------------------ KNN IMPLEMENTATION

FsTrain$label   = as.factor(FsTrain$label)

start.time <- Sys.time()

pred <- knn(FsTrain[, -26],FsTest[,-26],FsTrain$label , k=13)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken



table(pred = pred,FsTest$label)

#-----------------------------------------------------------------------------decision tree implementation
library(C50)

FsTrain$ClassColumn  = as.factor(FsTrain$ClassColumn  )
start.time <- Sys.time()
Train_model <- C5.0(ClassColumn   ~., data =FsTrain)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


start.time <- Sys.time()

predit_model <- predict(Train_model, FsTest)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

 table(pred = predit_model, actual = FsTest$ClassColumn )





# Count the occurrences of each attack category
attack_counts <- table(FsTest$attack_cat)

# Print the result
print(attack_counts)






#-------------------------------------------------------MUlticlass classification


# Example confusion matrix
conf_matrix <- matrix(c(6, 6, 98, 108, 23, 0, 494, 11, 0, 0,
                        77, 74, 661, 646, 167, 3, 6, 81, 1, 0,
                        0, 3, 442, 262, 71, 44, 123, 9, 7, 0,
                        578, 466, 2662, 9489, 1539, 381, 907, 499, 50, 11,
                        0, 2, 46, 127, 2725, 15, 7756, 18, 26, 3,
                        2, 6, 41, 59, 33, 18372, 20, 2, 1, 2,
                        14, 17, 69, 155, 1033, 24, 27417, 32, 7, 1,
                        0, 1, 26, 160, 24, 2, 97, 2798, 4, 0,
                        0, 8, 42, 107, 447, 26, 179, 43, 282, 0,
                        0, 0, 2, 19, 0, 4, 1, 3, 0, 27), nrow = 10, byrow = TRUE)

# Attack types
attack_types <- c("Analysis", "Backdoor", "DOS", "Exploits", "Fuzzers", "Generic", "Normal", "Reconnaissance", "Shellcode", "Warms")

# Initialize vectors to store TP, FP, FN, TN for each attack type
TP <- numeric(length(attack_types))
FP <- numeric(length(attack_types))
FN <- numeric(length(attack_types))
TN <- numeric(length(attack_types))

# Loop through each attack type
for (i in seq_along(attack_types)) {
  # Extract TP, FP, FN, TN for each attack type
  TP[i] <- conf_matrix[i, i]
  FP[i] <- sum(conf_matrix[, i]) - TP[i]
  FN[i] <- sum(conf_matrix[i, ]) - TP[i]
  TN[i] <- sum(conf_matrix) - TP[i] - FP[i] - FN[i]
  
  # Print results for each attack type
  cat("Attack Type:", attack_types[i], "\n")
  cat("True Positives (TP):", TP[i], "\n")
  cat("False Positives (FP):", FP[i], "\n")
  cat("False Negatives (FN):", FN[i], "\n")
  cat("True Negatives (TN):", TN[i], "\n\n")
}




#-----------------------------------------------------C5.0 Ensemble
TrainDatasetNorm$Label   = as.factor(TrainDatasetNorm$Label )

start.time <- Sys.time()

Train_model <- C5.0(Label ~., data =TrainDatasetNorm, trials =6 )

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

start.time <- Sys.time()

predit_model <- predict(Train_model, TestDatasetNorm)


end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
table(pred = predit_model,TestDatasetNorm$Label)



#-----------------------------------------------------------------------CART iplementation
library(rpart)

TrainDatasetNorm$label  = as.factor(TrainDatasetNorm$label   )

start.time <- Sys.time()
Train_Cart = rpart( label  ~ ., data=TrainDatasetNorm, method = "class" )

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

start.time <- Sys.time()

predicted.classes <- Train_Cart %>%
  predict(TestDatasetNorm, type = "class")

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

table(pred = predicted.classes,TestDatasetNorm$label   )



summary(Train_Cart)


#-----------------------------------------------------------------------CART tunning
library(caret)

FsTrain$class   = as.factor(FsTrain$class  )

model2 <- train(
  class  ~., data = FsTrain, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 4
)

summary(model2)
model2$bestTune
plot(model2)


model2 <- train(
  x = FsTrain[,-33], y = FsTrain[,33], method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 6
)


#-----------------------------------------------------RandomForest
library(randomForest)
library(ranger)#table(FsTest$class, predictions(predit_model))

TrainDataset$class <- as.factor(TrainDataset$class)
start.time <- Sys.time()
rf <- randomForest(class ~., data = TrainDataset)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


start.time <- Sys.time()

predit_model <- predict(rf, TestDataset)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

table(pred = predit_model,TestDataset$label)

#-----------------------------------------------------Logistic Regression
library(glm)
TrainDatasetNorm$Label<- as.factor(TrainDatasetNorm$Label)

start.time <- Sys.time()
logreg_model <- glm(Label ~ ., data = TrainDatasetNorm, family = "binomial")

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

start.time <- Sys.time()

predicted_values <- predict(logreg_model, newdata = TestDatasetNorm, type = "response")
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

predicted_classes <- ifelse(predicted_values > 0.5, 1, 0)


conf_matrix <- table(observed =TestDatasetNorm$Label, predicted = predicted_classes)
conf_matrix


#---------------------------------------------------------------------------------------------SVM

library(e1071)
svm_model <- svm(x = FsTrain[,-"class_123"], y = TraindatasetNorm[,"src_bytes_86"], type = "C-classification", kernel = "radial", cost = 1000000, gamma = 10)

start.time <- Sys.time()

svm_model <- svm(x = subset(FsTrain, select = -class_123),y = FsTrain$class_123,type = "C-classification",kernel = "radial",cost = 1000000,gamma = 10
)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

#-----------------------------------------------------------svm Prediction on Train
start.time <- Sys.time()


predictions <- predict(svm_model, subset(FsTest, select = -ClassColumn))

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

pred_train <- predict(svm_model,FsTest)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


table(pred = predictions , FsTest$ClassColumn)


####   --------------------------------------------------------------------------------svm
library(e1071)

start.time <- Sys.time()
svm_model <- svm(x = FsTrain[,-2], y = FsTrain[,2], type = "C-classification", kernel = "radial", cost = 1000000, gamma = 10)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

#-----------------------------------------------------------svm Prediction on Train
start.time <- Sys.time()

pred_train <- predict(svm_model,FsTest[,-2])

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


table(pred = pred_train , FsTest[,2])

#-------------------------------------------------------------zscore normalization

TrainDatasetNorm <- Traindataset
TrainDatasetNoNorm <- Traindataset
TestDatasetNorm <- Testdataset
TrainDatasetMeanList <- vector(length = (ncol(Traindataset)))
TrainDatasetSdList <- vector(length = (ncol(Traindataset)))

for(j in 1:ncol(Traindataset))
{
  TrainDatasetMeanList[j] <- mean(TrainDatasetNoNorm[,j])
  TrainDatasetSdList[j] <- sd(TrainDatasetNoNorm[,j])

  for(i in 1:nrow(Traindataset))
  {
    TrainDatasetNorm[i,j] <- ((Traindataset[i,j] - TrainDatasetMeanList[j]) / (TrainDatasetSdList[j]))
  }
}
# ------------------------------------------------------------------------------------------------------------ #
for(j in 1:ncol(Testdataset))
{
  for(i in 1:nrow(Testdataset))
  {
    TestDatasetNorm[i,j] <- ((Testdataset[i,j] - TrainDatasetMeanList[j]) / (TrainDatasetSdList[j]))
  }
}



save(testing_data, file = "SateliteTest.Rda")

save(training_data, file = "SateliteTrain.Rda")

# ------------------------------------------------------------------------------------------------------------ #

TrainDatasetNorm[,69] <- TrainDataset[,69]
TestDatasetNorm[,69] <- TestDataset[,69]






   


# -------------------------------------------------------------------------------------------C5.0 Bagging
library(C50)



# Number of trees to grow
num_trees <- 5

# Initialize an empty list to store the models
models <- list()

# Loop to create and train multiple models
for (i in 1:num_trees) {
  # Sample with replacement from the dataset
  sampled_data <- FsTrain[sample(nrow(FsTrain), replace = TRUE), ]
  
  # Train C5.0 model on the sampled data
  FsTrain$label   = as.factor(FsTrain$label )
  
  start.time <- Sys.time()
  
  model <- C5.0(label ~ ., FsTrain)
  
  # Store the model
  models[[i]] <- model
}

# Function to predict using the bagged models
bagged_predict <- function(models, newdata) {
  # Get predictions from each model
  predictions <- sapply(models, function(model) {
    predict(model, newdata)
  })
  
  # Take majority vote as final prediction
  final_prediction <- apply(predictions, 1, function(x) {
    names(sort(table(x), decreasing = TRUE)[1])
  })
  
  return(final_prediction)
}

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

# Predict using bagged models

start.time <- Sys.time()
test_predictions <- bagged_predict(models, newdata = FsTest)


end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

table(pred = test_predictions,FsTest$label)


