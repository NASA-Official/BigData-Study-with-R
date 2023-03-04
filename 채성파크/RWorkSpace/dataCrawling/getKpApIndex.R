kp_ap_df <- read.table(file="https://www-app3.gfz-potsdam.de/kp_index/Kp_ap_since_1932.txt", header= FALSE, sep="")

head(kp_ap_df)

library(dplyr)
kp_ap_df <- filter(.data = kp_ap_df, YEAR>2000) 

colnames(kp_ap_df) <- c("YEAR", "MONTH", "DAY", "START_TIME", "MID_TIME", "DAYS", "MID_OF_DAYS", "KP", "AP", "VERACITY")
write.csv(kp_ap_df, file="kp_ap_data.csv", row.names = FALSE)

