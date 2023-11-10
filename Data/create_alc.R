

##Assignment 3

library(dplyr)

student_por = read.csv("data/student-por.csv", sep = ";", head = TRUE)
student_mat = read.csv("data/student-mat.csv", sep = ";", head = TRUE)

summary(student_por)
dim(student_por)

library(dplyr)

summary(student_mat)
dim(student_mat)


#Joining data
identifiers <- setdiff(names(student_por), c("failures", "paid", "absences", "G1", "G2", "G3"))
joined_data <- merge(student_por, student_mat, by = identifiers)
colnames(joined_data)
dim(joined_data)

distinct_joined_data <- distinct(joined_data)
colnames(distinct_joined_data)
dim(distinct_joined_data)

#Average of weekday and weekend alcohol consumption
joined_data <- joined_data %>%
  mutate(alc_use = (Dalc + Walc) / 2)

#high use of alcohol
joined_data <- joined_data %>%
  mutate(high_use = alc_use > 2)

str(joined_data)
dim(joined_data)

library(readr)
write_csv(joined_data, "data/create_alc.csv")

url = "https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/alc.csv"
check <- read.csv(url)
colnames(check)
