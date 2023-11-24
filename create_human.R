


library(readr)
hd <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv")
gii <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv", na = "..")

#Human development dataset
str(hd)
dim(hd)
summary(hd)
colnames(hd)

# Gender inequality dataset
str(gii)
dim(gii)
summary(gii)
colnames(gii)

#Renaming into more descriptive names

library(dplyr)

# Renaming the "Human development" 
library(dplyr)

# Rename variables in the "Human development" dataset
hd <- hd %>%
  rename(
    HDI_Rank = `HDI_Rank`,
    Country = `Country`,
    HDI = `Human_Development_Index`,
    Life.Exp = `Life_Expectancy_at_Birth`,
    Edu.Exp = `Expected_Years_of_Education`,
    Mean.Edu = `Mean_Years_of_Education`,
    GNI.percapita = `Gross_National_Income_per_Capita`,
    GNI_HDI = `GNI_per_Capita_Rank_Minus_HDI_Rank`
  )

# Rename variables in the "Gender inequality" dataset
gii <- gii %>%
  rename(
    Country = `Country`,
    GII = `Gender Inequality Index (GII)`,
    Mat.Mor = `Maternal Mortality Ratio`,
    Ado.Birth = `Adolescent Birth Rate`,
    Parli.F = `Percent Representation in Parliament`,
    Edu2.F = `Population with Secondary Education (Female)`,
    Edu2.M = `Population with Secondary Education (Male)`,
    Labo.F = `Labour Force Participation Rate (Female)`,
    Labo.M = `Labour Force Participation Rate (Male)`,
  )

head(hd)
head(gii)

#Adding two new variables. Edu2.FM = Edu2.F / Edu2.M, Labo.FM = Labo.F / Labo.M

gender_data <- gii %>%
  mutate(
    Edu2.FM = Edu2.F / Edu2.M,
    Labo.FM = Labo.F / Labo.M
  )

head(gender_data)

#Joining the data together

human <- inner_join(hd, gii, by = "Country")

head(human)
#saving the data
write_csv(human, "data/human.csv")

#Assignment 5 continuation


# Excluding unneeded variables from the file above. 

library(readr)
human <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human1.csv")

library(dplyr)


# Select only the wanted columns
human <- human %>%
  select(
    Country, Edu2.FM, Labo.FM, Edu.Exp, Life.Exp, GNI, Mat.Mor, Ado.Birth, Parli.F
  )

head(human)

# exctracting missing values

human <- na.omit(human)

#new data
head(human)


#excracting region
human <- human %>%
  filter(!grepl("Region", Country, ignore.case = TRUE))

head(human)

ncol(human)
nrow(human)

#there is some issue, the data doesn't contain "Region". 