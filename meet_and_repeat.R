
setwd("/Users/konradsopyllo/Desktop/IODS-project")

library(tidyverse)
library(usethis)
install.packages(tidyr)
library(tidyr)
library(readr)


url <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt"
rats_wide <- read.table(url, header = TRUE, sep ="\t")
rats_wide

rats_long <- gather(rats_wide, key = "WD", value = "weight", -ID, -Group)
rats_long$time <- as.numeric(gsub("WD","", rats_long$WD))

rats_long$ID <- factor(rats_long$ID)
rats_long$Group <- factor(rats_long$Group)
rats_long$WD <- factor(rats_long$WD)
rats_long

write_csv(rats_long, "data/rats.txt")


bprs_url <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt"
bprs_wide <- read.table(bprs_url, header = TRUE)
bprs_wide

bprs_long <- gather(bprs_wide, key = "week", value = "dose", -subject, -treatment)
bprs_long$week <- as.numeric(gsub("week","", bprs_long$week))

bprs_long

write_csv(bprs_long, "data/bprs.txt")