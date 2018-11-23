library(neuralnet)
nids<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWTrainet.csv", header=T, sep=";")
nids1<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWTestet.csv", header=T, sep=";")
train <- nids
test <- nids1
ann <- neuralnet(as.numeric(nids$attack_cat)~
                   nids$dur+ nids$spkts+ nids$dpkts+
                   nids$sbytes+ nids$dbytes+ nids$rate+
                   nids$sttl+  nids$dttl+nids$sload+
                   nids$dload+nids$sloss+ nids$dloss+
                   nids$sinpkt+ nids$dinpkt+ nids$sjit+
                   nids$djit+ nids$swin+nids$stcpb+
                   nids$dtcpb+ nids$dwin+ nids$tcprtt+
                   nids$synack+ nids$ackdat+ nids$smean+
                   nids$dmean+ nids$trans_depth+ nids$response_body_len+
                   nids$ct_srv_src+ nids$ct_state_ttl+  nids$ct_dst_ltm+
                   nids$ct_src_dport_ltm+  nids$ct_dst_sport_ltm+ nids$ct_dst_src_ltm+
                   nids$is_ftp_login+  nids$ct_ftp_cmd+ nids$ct_flw_http_mthd+
                   nids$ct_src_ltm+ nids$ct_srv_dst+ nids$is_sm_ips_ports
                 , train,hidden=1,stepmax=1e6)
plot(ann, rep = "best")
output <- compute(ann, test[ , c("dur","spkts",
                                 "dpkts","sbytes",
                                 "dbytes","rate",
                                 "sttl","dttl",
                                 "sload","dload",
                                 "sloss","dloss",
                                 "sinpkt","dinpkt",
                                 "sjit","djit",
                                 "swin","stcpb",
                                 "dtcpb",  "dwin",
                                 "tcprtt", "synack",
                                 "ackdat","smean",
                                 "dmean", "trans_depth",
                                 "response_body_len", "ct_srv_src",
                                 "ct_state_ttl","ct_dst_ltm",
                                 "ct_src_dport_ltm", "ct_dst_sport_ltm",
                                 "ct_dst_src_ltm","is_ftp_login",
                                 "ct_ftp_cmd","ct_flw_http_mthd",
                                 "ct_src_ltm","ct_srv_dst",
                                 "is_sm_ips_ports")])
result <- data.frame(
  Real = test$attack_cat,
  Predicted = levels(nids$attack_cat)[round(output$net.result)])
result
#matriz de confusion y resultados
library(caret)
xtab <- table(result$Predicted,result$Real)
print(confusionMatrix(xtab[2:1,2:1]))

