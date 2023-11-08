"**Introduction to Open Data Science, Exercise Set 2**"

"**Regression and model validation**"

output: 
  html_document:
  theme: flatly
highlight: haddock
toc: true
toc_depth: 2
number_section: false

#Konrad Sopyllo, 6.11.2023


##Data wrangling
install.packages("tidyverse")
library(tidyverse)


url<-"http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"
data=read_tsv(url)
view(data)

#Create an analysis dataset with the variables gender, age, attitude, deep, stra, surf and points 
#by combining questions in the learning2014 data,

collumns <- c("gender", "Age", "Attitude", "Points")

analysis_dataset <- data[collumns]

#Stra =  st_os+st_tm = ST01+ST09+ST17+ST25+ST04+ST12+ST20+ST28
stra_collumns = unlist(str_split(c("ST01+ST09+ST17+ST25+ST04+ST12+ST20+ST28"), "\\+"))

#surf = su_lp+su_um+su_sb = SU02+SU10+SU18+SU26 + SU05+SU13+SU21+SU29 + SU08+SU16+SU24+SU32
surf_collumns = unlist(str_split(c("SU02+SU10+SU18+SU26+SU05+SU13+SU21+SU29+SU08+SU16+SU24+SU32"), "\\+"))

#deep = d_sm+d_ri+d_ue = D03+D11+D19+D27 + D07+D14+D22+D30 + D06+D15+D23+D31
deep_collumns = unlist(str_split(c("D03+D11+D19+D27+D07+D14+D22+D30+D06+D15+D23+D31"), "\\+"))

# create scaled variables 

analysis_dataset["stra"] = rowMeans(select(data, one_of(stra_collumns)))
analysis_dataset["deep"] = rowMeans(select(data, one_of(deep_collumns)))
analysis_dataset["surf"] = rowMeans(select(data, one_of(surf_collumns)))

# filter out rows where Points variable = 0
analysis_dataset = analysis_dataset[analysis_dataset$Points != 0,]
dim(analysis_dataset)

##Data analysis

#correlation matrix
install.packages("corrplot")
install.packages("Hmisc")
install.packages("ggplot2")
library("Hmisc")
library("corrplot")
library(ggplot2)

gender_graph <- ggplot(data.frame(analysis_dataset), aes(x = gender)) +
  geom_bar(fill = "lightgreen")
print(gender_graph)

points_dist <- ggplot(data.frame(analysis_dataset), aes(x =Points)) +
  geom_density(fill = "lightgreen")
print(points_dist)

age_dist <- points_dist <- ggplot(data.frame(analysis_dataset), aes(x = Age)) +
  geom_density(fill = "lightgreen")
print(points_dist)

deep_dist <- ggplot(data.frame(analysis_dataset), aes(x = deep)) +
  geom_density(fill = "lightgreen")
print(deep_dist)

surf_dist <- ggplot(data.frame(analysis_dataset), aes(x = surf)) +
  geom_density(fill = "lightgreen")
print(surf_dist)

stra_dist <- ggplot(data.frame(analysis_dataset), aes(x = stra)) +
  geom_density(fill = "lightgreen")
print(stra_dist)




#Correlation (pearson)
correlations = c("Attitude","deep","stra","surf","Points")
analysis_cor = cor(analysis_dataset[correlations])
corrplot(analysis_cor)


plot(analysis_dataset)

# By having a look at the correlation analysis we can identify the factors with the highest correspondance


##Attitude vs Points
p1 <- ggplot(data.frame(analysis_dataset), aes(x = Attitude, y = Points))
p2 <- p1 + geom_point()
p3 <- p2 + geom_smooth(method = "lm")
p4 <- p3+ggtitle("Attitude vs Points")
print(p4)


model <- lm(analysis_dataset$Points~analysis_dataset$Attitude)
summary(model)

#p-value being lowest in this correlation graph

# Other way of doing the same thing

library(ggplot2)
qplot(Attitude, Points, data = analysis_dataset) + geom_smooth(method = "lm")
my_model <- lm(Points ~ Attitude, data = analysis_dataset)

##surf vs deep 
p1 <- ggplot(data.frame(analysis_dataset), aes(x = deep, y = surf))
p2 <- p1 + geom_point()
p3 <- p2 + geom_smooth(method = "lm")
p4 <- p3+ggtitle("deep vs surf")
print(p4)

model <- lm(analysis_dataset$surf~analysis_dataset$deep)
summary(model)
# p-value being second lowest in this comparison

##Attitude vs surf

p1 <- ggplot(data.frame(analysis_dataset), aes(x = Attitude, y = surf))
p2 <- p1 + geom_point()
p3 <- p2 + geom_smooth(method = "lm")
p4 <- p3+ggtitle("Attitude vs surf")
print(p4)

model <- lm(analysis_dataset$surf~analysis_dataset$Attitude)
summary(model)

#p-value being third lowest in this comparison.

#multiple variable model

library(GGally)
library(ggplot2)
ggpairs(analysis_dataset, lower = list(combo = wrap("facethist", bins = 20)))
my_model2 <- lm(Points ~ Attitude + stra, data = analysis_dataset)
summary(my_model2)


my_model2 <- lm(Points ~ Attitude + stra, data = analysis_dataset)
# Linear model with multiple variables
lm(my_model2)

par(mfrow = c(2, 2))
# Residuals vs Fitted values (which = 1)
plot(my_model2, which = 1)
# Normal QQ-plot (which = 2)
plot(my_model2, which = 2)
# Residuals vs Leverage (which = 5)
plot(my_model2, which = 5)


#Multiple explanatory variables all in one
plot(my_model2, which = c(1, 2, 5))

#Making predictions

m <- lm(Points ~ Attitude, data = analysis_dataset)
summary(m)
#New observations
new_attitudes <- c("Mia" = 3.8, "Mike"= 4.4, "Riikka" = 2.2, "Pekka" = 2.9)
new_data <- data.frame(Attitude = new_attitudes)
print(new_data)

##Predictions
predictions <- predict(m, newdata = new_data)
print(predictions)

#following the liner model the predicted values are Adjusted R-squared:  0.09939. This means the model doens not quite well predict the values.





