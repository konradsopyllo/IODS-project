
library(readr)
BPRS <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt")
RATS <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt")

write_csv(BPRS, "data/BPRS.csv")
write_csv(RATS, "data/RATS.csv")

head(BPRS)
summary(BPRS)
str(BPRS)
names(BPRS)
dim(BPRS)

head(RATS)
summary(RATS)
str(RATS)
names(RATS)
dim(RATS)


cat("\nSummary of BPRS Variables:\n")
print(summary(BPRS))

cat("\nSummary of RATS Variables:\n")
print(summary(RATS))

categorical_vars_BPRS <- sapply(BPRS, function(x) is.factor(x) | is.character(x))
BPRS[categorical_vars_BPRS] <- lapply(BPRS[categorical_vars_BPRS], as.factor)


categorical_vars_RATS <- sapply(RATS, function(x) is.factor(x) | is.character(x))
RATS[categorical_vars_RATS] <- lapply(RATS[categorical_vars_RATS], as.factor)

install.packages("tidyr")
library(tidyr)

install.packages(c("tidyr", "dplyr"))
library(tidyr)
library(dplyr)


BPRS_long <- BPRS %>%
  pivot_longer(cols = -c("Subject"), names_to = "Week", values_to = "Value") %>%
  mutate(Week = as.numeric(gsub("Week", "", Week)))

RATS_long <- RATS %>%
  pivot_longer(cols = -c("Subject"), names_to = "Time", values_to = "Value") %>%
  mutate(Time = as.numeric(gsub("Time", "", Time)))


##wide form
cat("BPRS Wide Form Variable Names:\n")
print(names(BPRS))

# View the first few rows 
cat("\nFirst Few Rows of BPRS Wide Form:\n")
print(head(BPRS))

# Summarizing the variables
cat("\nSummary of BPRS Wide Form Variables:\n")
print(summary(BPRS))

# Display variable names
cat("RATS Wide Form Variable Names:\n")
print(names(RATS))

# View the first few rows of the data
cat("\nFirst Few Rows of RATS Wide Form:\n")
print(head(RATS))

# Summarize the variables
cat("\nSummary of RATS Wide Form Variables:\n")
print(summary(RATS))

##long form

cat("BPRS Long Form Variable Names:\n")
print(names(BPRS_long))

# View the first few rows
cat("\nFirst Few Rows of BPRS Long Form:\n")
print(head(BPRS_long))

# Summarizing the variables
cat("\nSummary of BPRS Long Form Variables:\n")
print(summary(BPRS_long))
  
# Assuming RATS_long is the long form dataset
# Display variable names
cat("RATS Long Form Variable Names:\n")
print(names(RATS_long))

# View the first few rows 
cat("\nFirst Few Rows of RATS Long Form:\n")
print(head(RATS_long))

# Summarizing the variables
cat("\nSummary of RATS Long Form Variables:\n")
print(summary(RATS_long))
