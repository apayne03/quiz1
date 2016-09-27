library(tidyverse)
raw_data <- read_csv(file="lab_quiz_week2_data.csv")
str(raw_data)

View(raw_data)

raw_data <- read_csv(file = "lab_quiz_week2_data.csv",na=c("","NA","-999"))

categorical_variables <- select(raw_data, univ, prog_year)

categorical_variables$univ <- as.factor(categorical_variables$univ)
levels(categorical_variables$univ) <- list("Waterloo"=1,"Guelph"=2)

categorical_variables$prog_year <- as.factor(categorical_variables$prog_year)
levels(categorical_variables$prog_year) <- list("First Year"=1,"Second Year"=2,"Third Year"=3,"Forth Year"=4,"Grad School"=5)

pos_affect <- select (raw_data, PA1, PA2, PA3, PA4, PA5)
dep <- select (raw_data, D1, D2, D3, D4, D5)
prog_sat <- select (raw_data, PS1, PS2, PS3, PS4, PS5)

install.packages("psych",dep=T)

psych::describe(pos_affect)

pos_affect

is_bad_value <- pos_affect<1 | pos_affect>7

View(is_bad_value)

pos_affect[is_bad_value] <- NA
View(pos_affect)

psych::describe(dep)

dep

is_bad_value <- dep<1 | dep>4
View(is_bad_value)

dep[is_bad_value] <- NA
View(dep)

psych::describe(prog_sat)

is_bad_value <- prog_sat<1 | prog_sat>6
View(is_bad_value)

prog_sat[is_bad_value] <- NA
View(prog_sat)

pos_affect <- mutate(pos_affect,PA1=8-PA1)

dep <- mutate(dep,D4=5-D4)
dep <- mutate(dep,D5=5-D5)

prog_sat <- mutate(prog_sat,PS1=7-PS1)
prog_sat <- mutate(prog_sat,PS2=7-PS2)

pos_affect <- psych::alpha(as.data.frame(pos_affect),check.keys=FALSE)$scores
dep <- psych::alpha(as.data.frame(dep),check.keys = FALSE)$scores
prog_sat <- psych::alpha(as.data.frame(prog_sat),check.keys = FALSE)$scores

analytic_data <- cbind(categorical_variables,pos_affect,dep,prog_sat)

write_csv(analytic_data,path="quiz1_analytic_data_payne.csv")
