#program: lab1.R 
#project: applied big data science
#author: Josh Taylor
#last edited: 1/25/16

library(data.table)
library(ggplot2)


y = list(1,7,'banana', 'apple')
y[[2]]*2
x = c(1,7,3,4)
x[2]
x[2:4]
x[c(1,3,4)]
x[-2]

y = list(c(1,6,3,8), c('apple', 'banana', 'cheese'))
a = y[[1]]

##### Part 1: Using data.tables ######
x = data.table(myvariable = c(5,4,2,8,9), myothervariable = c(2,8,5,1,0))
setwd("/Volumes/Seagate Data Drive/Duke Classes/Applied Big Data Science/CER Subsample/")
files = list.files()
file.info(files)
cer_consump = fread('cer_consump.csv')
cer_assignment = fread('cer_assignment.csv')
cer_survey = fread('cer_survey.csv')

cer_survey[, list(id, n_hhsize, n_rooms)]
cer_survey[, .(id, n_hhsize, n_rooms)]
dim(cer_survey)


cer_survey[n_adults < 2, .(id, n_rooms)]

cer_survey[, d_elec_house := (d_elec_heat == 1 & d_elec_cook == 1)]

cer_survey[, mean (n_rooms, na.rm  = T), by=d_emp]

cer_survey[,.(Mean = mean(n_rooms, na.rm = T), StdDev = sd(n_rooms, na.rm = T))
           , by= .(d_emp, d_ret_care)]

dt = merge(cer_assignment, cer_consump, by= 'id')

mean_cons = dt[, .(kwh = mean(kwh)), by= .(id, tar_stim)]

dt_sum = mean_cons[,.(Mean = mean(kwh), SD = sd(kwh)), by = tar_stim]

t.test(mean_cons[tar_stim =="EE", kwh], mean_cons[tar_stim == "B2", kwh])

### Functions

pythag = function(a,b){
  print("Hello world!")
  c = sqrt(a^2 + b^2)
  return(c)
}

result = pythag(3,4)
result


sefunction = function(x){
  sqrt(var(x))/sqrt(length(x))
}

# looking at the data 
summary(cer_consump)
cer_consump[, day := (date_cer - (date_cer%%100))]
unique(cer_consump[, .(day)])

avg_daily_cons = cer_consump[, .(AvgCons = mean(kwh)), by = day]

plot(avg_daily_cons[, day], avg_daily_cons[, AvgCons], type = 'l', ylim = c(0, .5))

avg_hourly_cons[, day:= (date_cer - (date_cer%%100))]

avg_hourly_cons = cer_consump[, .(AvgCons = mean(kwh)), by = date_cer]

plot(avg_hourly_cons[day == 36000, date_cer], avg_hourly_cons[day == 36000, AvgCons], type = 'l',
     ylim = c(0,1))

ggplot(data = avg_daily_cons, aes(day, AvgCons)) + geom_line()
