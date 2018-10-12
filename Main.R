##### Read Data #####
library(Matrix)
library(stats)
library(MASS)
library("ggplot2", lib.loc = "/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library(ggplot2)
library(msm)
library(expm)
load("RawData.RData")
colnames(Data)[1] <- "ID"
colnames(Data)[2] <- "CD4count"
colnames(Data)[5] <- "ARTstartdate"
colnames(Data)[6] <- "Gender"
colnames(Data)[7] <- "PostNum"
colnames(Data)[8] <- "PrimaryCauseOfDeath"
colnames(Data)[9] <- "LivingProvince"
colnames(Data)[10] <- "DateOfBirth"
colnames(Data)[11] <- "DateOfDeath"
colnames(Data)[12] <- "CD4date"
colnames(Data)[13] <- "CD4Year"
Data[Data == "\xc4\xd0"] <- "male"
Data[Data == "Ů"] <- "female"
# mark which position is the start of a new ID
mark <- 1
change <- c(1)
for (i in 2:nrow(Data)) {
  if (Data$ID[i] != Data$ID[mark]) {
    change <- c(change, i)
    mark <- i
  }
}
change <- c(change, nrow(Data))
# change ID to numeric ordered numbers
count <- 1
for (i in 2:length(change)) {
  Data$ID[change[i - 1]:change[i]] <- count
  count <- count + 1
}

# rearrange data by individual ID 
Data$ID <- as.numeric(Data$ID)
Data[which(as.numeric(Data$CD4count) > 5000),] 

### Recording the number of observations for each individual
### for some reason Data$ID seems not to be sorted so using table does not work appropriately 
indiv.id <- table(Data$ID) #
N = length(indiv.id)
maxT = max(indiv.id) 

# Put date in the same format
Data$CD4date = as.Date(Data$CD4date, "%m/%d/%Y")
Data$DateOfDeath = as.Date(Data$DateOfDeath, "%m/%d/%Y")
Data$DateOfBirth = as.Date(Data$DateOfBirth, "%m/%d/%Y")  # when the date format is different
Data$ARTstartdate[Data$ARTstartdate == " "] = "12/31/2999"  # when there are " " entries
Data$ARTstartdate = as.Date(Data$ARTstartdate, "%m/%d/%Y")
Data$ARTAge = Data$ARTstartdate - Data$DateOfBirth
Data$ARTAge = as.double(Data$ARTAge)/365.5

######### Delete empty/NA rows #########
# There are 4816620 observations originally
Data <- Data[Data$CD4count != " ", ] # now there are 2405687 observations
# I find that for empty gender rows, the date of birth rows are also NA, so I leave a backup in case needed later
Data.bkup <- Data
Data <- Data[Data$Gender != " ", ] # now there are 2383171 observations
Data <- Data[!is.na(Data$DateOfBirth), ] # now there are 2383168 observations
Data <- Data[!is.na(Data$CD4date), ] # now there are 2383149 observations

# order each individual by CD4 date
Data <- Data[order(Data$ID, Data$CD4date),]

#### separate CD4date gap that is greater than 36 months
indiv.id <- table(Data$ID) #
N.pre = length(indiv.id)
Data$ID <- Data$ID*10
time2 <- Data$CD4date[2:nrow(Data)]
time1 <- Data$CD4date[1:nrow(Data) - 1]
gap <- time2 - time1
pos <- which(gap > 36*30.5) # can change 36 to any number of months
for (i in 1:length(pos)) {
  if (Data$ID[pos[i]] == Data$ID[pos[i] + 1]) {
    last <- tail(which(Data$ID == Data$ID[pos[i]]), n = 1)
    Data$ID[(pos[i] + 1):last] <- Data$ID[pos[i]] + 1
  }
}
indiv.id <- table(Data$ID) #
N.post = length(indiv.id)

# recording birth date, death date, ART start date, and gender as vectors, 
# recording CD4count, CD4date, period between two states, and whether previously on ART as matrices.
indiv.id <- table(Data$ID) #
N = length(indiv.id)
maxT = max(indiv.id) 
ID.last = cumsum(indiv.id) # last measurement position
ID.birth = Data$DateOfBirth[ID.last]
ID.death = Data$DateOfDeath[ID.last]
ID.ARTstart = Data$ARTstartdate[ID.last]
ID.ARTAge = Data$ARTAge[ID.last]
ID.age = matrix(NA,N,maxT)
ID.ARTdate = matrix(NA,N,maxT)
ID.gender = Data$Gender[ID.last]
ID.CD4count = ID.CD4date = matrix(NA, N, maxT) # missing for NA
ID.period = ID.art = matrix(NA, N, maxT + 1) 
ID.first = 1

for (i in 1:N) {
  ID.CD4count[i,1:indiv.id[i]] = as.numeric(Data$CD4count[ID.first:ID.last[i]])
  ID.CD4date[i,1:indiv.id[i]] = Data$CD4date[ID.first:ID.last[i]]
  ID.ARTdate[i,1:indiv.id[i]] = Data$CD4date[ID.first:ID.last[i]] - ID.ARTstart[i]
  data.i = c(ID.birth[i], ID.CD4date[i,1:indiv.id[i]], ID.death[i]) # time vector
  ID.period[i,1:(length(data.i) - 1)] = data.i[-1] - data.i[-length(data.i)] # calculate the difference between time
  ID.age[i,1:indiv.id[i]] = Data$CD4date[ID.first:ID.last[i]] - ID.birth[i]
  ID.art[i,1:(length(data.i) - 1)] = data.i[-1] > ID.ARTstart[i] # treatment start or not (include death date)
  ID.first = ID.last[i] + 1
}
ID.agem = round(ID.age/30.5,2) # change age to month
ID.age = round(ID.age/365.5,2) # change age to year
# Add death state in the proper position
l = length(ID.death)
for (i in 1:l) {
  if (!is.na(ID.death[i])) {
    ID.CD4date[i,sum(!is.na(ID.CD4date[i,])) + 1] = ID.death[i]
    ID.age[i,sum(!is.na(ID.age[i,])) + 1] = round(as.double(ID.death[i] - ID.birth[i])/365.5,2)
    ID.agem[i,sum(!is.na(ID.agem[i,])) + 1] = round(as.double(ID.death[i] - ID.birth[i])/30.5,2)
  }
}

# Calculate the difference of time between CD4 measurement(include death date) and ART start date
for (i in 1:N) {
  row.d <- ID.CD4date[i,]
  row.d <- row.d[!is.na(row.d)]
  for (j in length(row.d)) {
    ID.ARTdate[i,j] = ID.CD4date[i,j] - as.double(ID.ARTstart[i])
  }
}

ID.state = ID.CD4count # Create ID.state, which is the observed state
ID.state[ID.CD4count > 500] = 1
ID.state[ID.CD4count >= 350 & ID.CD4count <= 500] = 2
ID.state[ID.CD4count >= 250 & ID.CD4count < 350] = 3
ID.state[ID.CD4count >= 200 & ID.CD4count < 250] = 4
ID.state[ID.CD4count >= 100 & ID.CD4count < 200] = 5
ID.state[ID.CD4count >= 50 & ID.CD4count < 100] = 6
ID.state[ID.CD4count < 50] = 7

### Modify ID.age and ID.ARTAge to be categorized ####
ID.age[ID.age < 15] = 0
ID.age[ID.age >= 15 & ID.age < 35] = 1
ID.age[ID.age >= 35 & ID.age < 45] = 2
ID.age[ID.age >= 45 & ID.age < 55] = 3
ID.age[ID.age >= 55] = 4

ID.ARTAge[ID.ARTAge < 15] = 4 # This is 4 because we use 4 here to imply that the patients either do not take ART treatment or not in the age range that we want to investigate
ID.ARTAge[ID.ARTAge >= 15 & ID.ARTAge < 35] = 1
ID.ARTAge[ID.ARTAge >= 35 & ID.ARTAge < 45] = 2
ID.ARTAge[ID.ARTAge >= 45 & ID.ARTAge < 55] = 3
ID.ARTAge[ID.ARTAge >= 55] = 4

# Add death state 8 into state data frame and CD4 count data frame as 0
for (i in 1:length(ID.death)) {
  if (!is.na(ID.death[i])) {
    ID.state[i,sum(!is.na(ID.state[i,])) + 1] = 8
    ID.CD4count[i,sum(!is.na(ID.CD4count[i,])) + 1] = -1
  }
}

# find the row No. s.t. just one observation in a row or there is negative or 0 in period
del.num = 0
count = 1
for (i in 1:nrow(ID.state)) {
  row.state = ID.state[i,]
  row.state <- row.state[!is.na(row.state)]
  l = length(row.state)
  for (j in 2:l) {
    if (l == 1) {
      del.num[count] = i
      count = count + 1
      break
    }
  }
  row.period = ID.period[i,]
  row.period <- row.period[!is.na(row.period)] # get the i_th row of ID.period
  m = length(row.period)
  for (j in 1:m) {
    # print(i)
    # print(row.period[j])
    if (row.period[j] <= 0) {
      del.num[count] = i
      count = count + 1
      break
    }
  }
}

if (del.num != 0) {
  ID.death <- ID.death[-del.num]
  ID.birth <- ID.birth[-del.num]
  ID.state <- ID.state[-del.num,]
  ID.CD4count <- ID.CD4count[-del.num,]
  ID.gender <- ID.gender[-del.num]
  ID.period <- ID.period[-del.num,]
  ID.art <- ID.art[-del.num,]
  ID.ARTstart <- ID.ARTstart[-del.num]
  ID.ARTdate <- ID.ARTdate[-del.num,]
  ID.ARTAge <- ID.ARTAge[-del.num]
  ID.age <- ID.age[-del.num,]
  ID.agem <- ID.agem[-del.num,]
}

ID.periodm = ceiling(ID.period/30.5)

del.num = 0
count = 1
# delete the rows that the state changes in a unrealistic short period.
for (i in 1:nrow(ID.periodm)) {
  row.p = ID.periodm[i,]
  row.s = ID.state[i,]
  row.p <- row.p[!is.na(row.p)] # get the i_th row of ID.period
  row.s <- row.s[!is.na(row.s)] # get the i_th row of ID.S
  l = length(row.s)
  for (j in 2:l) {
    if (row.p[j - 1] < (row.s[j] - row.s[j - 1]) || row.p[j - 1] < (row.s[j - 1] - row.s[j])) {
      del.num[count] = i
      count = count + 1
      break
    }
  }
}
if (del.num != 0) {
  ID.state <- ID.state[-del.num,]
  ID.CD4count <- ID.CD4count[-del.num,]
  ID.birth <- ID.birth[-del.num]
  ID.periodm <- ID.periodm[-del.num,]
  ID.gender <- ID.gender[-del.num]
  ID.art <- ID.art[-del.num,]
  ID.ARTstart <- ID.ARTstart[-del.num]
  ID.ARTdate <- ID.ARTdate[-del.num,]
  ID.ARTAge <- ID.ARTAge[-del.num]
  ID.age <- ID.age[-del.num,]
  ID.agem <- ID.agem[-del.num,]
}

#### Delete the individuals that are under 15 years old or older than 55 years old ####
del.num = 0
count = 1
for (i in 1:nrow(ID.age)) {
  row.age = ID.age[i,]
  row.age <- row.age[!is.na(row.age)]
  l = length(row.age)
  # find the individuals that start with under 15 years old and end with under 15 years old and greater than 55. Use l-1 and 2 since after deleted has only one observation
  if ( row.age[l - 1] == 0 || row.age[2] == 4) { 
    del.num[count] = i
    count = count + 1
  }
}

length(del.num) # 39793
if (del.num != 0) {
  ID.death <- ID.death[-del.num]
  ID.birth <- ID.birth[-del.num]
  ID.state <- ID.state[-del.num,]
  ID.CD4count <- ID.CD4count[-del.num,]
  ID.gender <- ID.gender[-del.num]
  ID.periodm <- ID.periodm[-del.num,]
  ID.periodm <- ID.periodm[,-1]
  ID.art <- ID.art[-del.num,]
  ID.ARTstart <- ID.ARTstart[-del.num]
  ID.ARTdate <- ID.ARTdate[-del.num,]
  ID.ARTAge <- ID.ARTAge[-del.num]
  ID.age <- ID.age[-del.num,]
  ID.agem <- ID.agem[-del.num,]
}

#check if the age is in increasing order
del.num = 0
count = 1
for (i in 1:nrow(ID.age)) {
  row.age = ID.age[i,]
  row.age <- row.age[!is.na(row.age)]
  l = length(row.age)
  for (j in 2:l) {
    if (row.age[j - 1] > row.age[j]) {
      del.num[count] = i
      count = count + 1
    }
  }
}
del.num # no such individual
if (del.num != 0) {
  ID.death <- ID.death[-del.num]
  ID.birth <- ID.birth[-del.num]
  ID.state <- ID.state[-del.num,]
  ID.CD4count <- ID.CD4count[-del.num,]
  ID.gender <- ID.gender[-del.num]
  ID.periodm <- ID.periodm[-del.num,]
  ID.periodm <- ID.periodm[,-1]
  ID.art <- ID.art[-del.num,]
  ID.ARTstart <- ID.ARTstart[-del.num]
  ID.ARTdate <- ID.ARTdate[-del.num,]
  ID.ARTAge <- ID.ARTAge[-del.num]
  ID.age <- ID.age[-del.num,]
  ID.agem <- ID.agem[-del.num,]
}

##### The result is that there is no decreasing age order

count = 0
# delete the observations that are 0 or 4 in part of the individual's data
for (i in 1:nrow(ID.age)) {
  row.age = ID.age[i,]
  row.age <- row.age[!is.na(row.age)]
  l = length(row.age)
  # the for loop change 4s to be NA
  for (j in 1:l) {
    if (row.age[j] == 4) {
      ID.age[i,j] = NA
      ID.agem[i,j] = NA
      ID.state[i,j] = NA
      ID.CD4count[i,j] = NA
      ID.periodm[i,j - 1] = NA
      ID.art[i,j] = NA
      ID.ARTdate[i,j] = NA
      count = count + 1
    }
  }
  # manipulate on the row with part of 0s
  if (row.age[1] == 0) {
    count0 = 0 # count the number of 0 in a row
    # the for loop change 0 to NA, and count the number of them
    for (j in 1:l) {
      if (row.age[j] == 0) {
        ID.age[i,j] = NA
        ID.agem[i,j] = NA
        ID.state[i,j] = NA
        ID.CD4count[i,j] = NA
        ID.periodm[i,j] = NA
        ID.art[i,j] = NA
        ID.ARTdate[i,j] = NA
        count0 = count0 + 1
        count = count + 1
      }
    }
    # the for loop move the whole array to the front
    for (j in 1:(l - count0)) {
      ID.age[i,j] = ID.age[i,j + count0]
      ID.agem[i,j] = ID.agem[i,j + count0]
      ID.state[i,j] = ID.state[i,j + count0]
      ID.CD4count[i,j] = ID.CD4count[i,j + count0]
      ID.periodm[i,j] = ID.periodm[i,j + count0]
      ID.art[i,j] = ID.art[i,j + count0]
      ID.ARTdate[i,j] = ID.ARTdate[i,j + count0]
      ID.age[i,j + count0] = NA
      ID.agem[i,j + count0] = NA
      ID.state[i,j + count0] = NA
      ID.CD4count[i,j + count0] = NA
      ID.periodm[i,j + count0] = NA
      ID.art[i,j + count0] = NA
      ID.ARTdate[i,j + count0] = NA
    }
  }
}

count # there are 55444 observations got deleted

# Take the invidual indexes which are male and female 
male.num = which(ID.gender == "male")
female.num = which(ID.gender == "female")

# get the data grouped by gender
# 2 groups: male and female
ID.state.male = ID.state[male.num,]
ID.state.female = ID.state[female.num,]
ID.CD4count.male = ID.CD4count[male.num,]
ID.CD4count.female = ID.CD4count[female.num,]
ID.periodm.male = ID.periodm[male.num,]
ID.periodm.female = ID.periodm[female.num,]
ID.art.male = ID.art[male.num,]
ID.art.female = ID.art[female.num,] 
ID.ARTdate.male = ID.ARTdate[male.num,]
ID.ARTdate.female = ID.ARTdate[female.num,]
ID.ARTAge.male = ID.ARTAge[male.num]
ID.ARTAge.female = ID.ARTAge[female.num]
ID.age.male = ID.age[male.num,]
ID.age.female = ID.age[female.num,]
ID.agem.male = ID.agem[male.num,]
ID.agem.female = ID.agem[female.num,]

##### separate by ART treatment #####
# Make treatment on art be 1 and no art be 0
ID.art.male[ID.art.male == "TRUE"] <- 1
ID.art.male[ID.art.male == "FALSE"] <- 0
ID.art.female[ID.art.female == "TRUE"] <- 1
ID.art.female[ID.art.female == "FALSE"] <- 0

# Change ID.ARTdate to monthly
ID.ARTdate.male = ceiling(ID.ARTdate.male/30.5)
ID.ARTdate.female = ceiling(ID.ARTdate.female/30.5)

# separate male data into art and no art
separation <- separate.art(ID.art = ID.art.male, ID.state = ID.state.male, ID.CD4count = ID.CD4count.male, ID.periodm = ID.periodm.male, ID.age = ID.age.male, ID.agem = ID.agem.male, ID.ARTAge = ID.ARTAge.male, ID.ARTdate = ID.ARTdate.male)

ID.state.male.art <- separation$ID.state.art
ID.state.male.noart <- separation$ID.state.noart
ID.CD4count.male.art <- separation$ID.CD4count.art
ID.CD4count.male.noart <- separation$ID.CD4count.noart
ID.periodm.male.art <- separation$ID.periodm.art
ID.periodm.male.noart <- separation$ID.periodm.noart
ID.age.male.art <- separation$ID.age.art
ID.age.male.noart <- separation$ID.age.noart
ID.agem.male.art <- separation$ID.agem.art
ID.agem.male.noart <- separation$ID.agem.noart
ID.ARTAge.male.art <- separation$ID.ARTAge.art
ID.ARTAge.male.noart <- separation$ID.ARTAge.noart

# separate female into art and no art
separation <- separate.art(ID.art = ID.art.female, ID.state = ID.state.female, ID.CD4count = ID.CD4count.female, ID.periodm = ID.periodm.female, ID.age = ID.age.female, ID.agem = ID.agem.female, ID.ARTAge = ID.ARTAge.female, ID.ARTdate = ID.ARTdate.female)

ID.state.female.art <- separation$ID.state.art
ID.state.female.noart <- separation$ID.state.noart
ID.CD4count.female.art <- separation$ID.CD4count.art
ID.CD4count.female.noart <- separation$ID.CD4count.noart
ID.periodm.female.art <- separation$ID.periodm.art
ID.periodm.female.noart <- separation$ID.periodm.noart
ID.age.female.art <- separation$ID.age.art
ID.age.female.noart <- separation$ID.age.noart
ID.agem.female.art <- separation$ID.agem.art
ID.agem.female.noart <- separation$ID.agem.noart
ID.ARTAge.female.art <- separation$ID.ARTAge.art
ID.ARTAge.female.noart <- separation$ID.ARTAge.noart

####### Separate the group by age categories 1, 2, and 3 ###########
# separate male.art into age category 1, 2 and 3
separation <- separate.age(ID.state = ID.state.male.art, ID.CD4count = ID.CD4count.male.art, ID.periodm = ID.periodm.male.art, ID.age = ID.age.male.art, ID.agem = ID.agem.male.art)

ID.state.male.art.age1 <- separation$ID.state.age1
ID.state.male.art.age2 <- separation$ID.state.age2
ID.state.male.art.age3 <- separation$ID.state.age3
ID.CD4count.male.art.age1 <- separation$ID.CD4count.age1
ID.CD4count.male.art.age2 <- separation$ID.CD4count.age2
ID.CD4count.male.art.age3 <- separation$ID.CD4count.age3
ID.periodm.male.art.age1 <- separation$ID.periodm.age1
ID.periodm.male.art.age2 <- separation$ID.periodm.age2
ID.periodm.male.art.age3 <- separation$ID.periodm.age3

# separate male.noart into age category 1, 2 and 3
separation <- separate.age(ID.state = ID.state.male.noart, ID.CD4count = ID.CD4count.male.noart, ID.periodm = ID.periodm.male.noart, ID.age = ID.age.male.noart, ID.agem = ID.agem.male.noart)

ID.state.male.noart.age1 <- separation$ID.state.age1
ID.state.male.noart.age2 <- separation$ID.state.age2
ID.state.male.noart.age3 <- separation$ID.state.age3
ID.CD4count.male.noart.age1 <- separation$ID.CD4count.age1
ID.CD4count.male.noart.age2 <- separation$ID.CD4count.age2
ID.CD4count.male.noart.age3 <- separation$ID.CD4count.age3
ID.periodm.male.noart.age1 <- separation$ID.periodm.age1
ID.periodm.male.noart.age2 <- separation$ID.periodm.age2
ID.periodm.male.noart.age3 <- separation$ID.periodm.age3

# separate female.art into age category 1, 2 and 3
separation <- separate.age(ID.state = ID.state.female.art, ID.CD4count = ID.CD4count.female.art, ID.periodm = ID.periodm.female.art, ID.age = ID.age.female.art, ID.agem = ID.agem.female.art)

ID.state.female.art.age1 <- separation$ID.state.age1
ID.state.female.art.age2 <- separation$ID.state.age2
ID.state.female.art.age3 <- separation$ID.state.age3
ID.CD4count.female.art.age1 <- separation$ID.CD4count.age1
ID.CD4count.female.art.age2 <- separation$ID.CD4count.age2
ID.CD4count.female.art.age3 <- separation$ID.CD4count.age3
ID.periodm.female.art.age1 <- separation$ID.periodm.age1
ID.periodm.female.art.age2 <- separation$ID.periodm.age2
ID.periodm.female.art.age3 <- separation$ID.periodm.age3

# separate female.noart into age category 1, 2 and 3
separation <- separate.age(ID.state = ID.state.female.noart, ID.CD4count = ID.CD4count.female.noart, ID.periodm = ID.periodm.female.noart, ID.age = ID.age.female.noart, ID.agem = ID.agem.female.noart)

ID.state.female.noart.age1 <- separation$ID.state.age1
ID.state.female.noart.age2 <- separation$ID.state.age2
ID.state.female.noart.age3 <- separation$ID.state.age3
ID.CD4count.female.noart.age1 <- separation$ID.CD4count.age1
ID.CD4count.female.noart.age2 <- separation$ID.CD4count.age2
ID.CD4count.female.noart.age3 <- separation$ID.CD4count.age3
ID.periodm.female.noart.age1 <- separation$ID.periodm.age1
ID.periodm.female.noart.age2 <- separation$ID.periodm.age2
ID.periodm.female.noart.age3 <- separation$ID.periodm.age3

##### calculate the position vector and row.num for subgroups #####
result.male.art.age1 <- position.calc(ID.state = ID.state.male.art.age1, ID.periodm = ID.periodm.male.art.age1)
ID.pos.male.art.age1 <- result.male.art.age1$ID.pos
row.num.male.art.age1 <- result.male.art.age1$row.num

result.male.art.age2 <- position.calc(ID.state = ID.state.male.art.age2, ID.periodm = ID.periodm.male.art.age2)
ID.pos.male.art.age2 <- result.male.art.age2$ID.pos
row.num.male.art.age2 <- result.male.art.age2$row.num

result.male.art.age3 <- position.calc(ID.state = ID.state.male.art.age3, ID.periodm = ID.periodm.male.art.age3)
ID.pos.male.art.age3 <- result.male.art.age3$ID.pos
row.num.male.art.age3 <- result.male.art.age3$row.num

result.male.noart.age1 <- position.calc(ID.state = ID.state.male.noart.age1, ID.periodm = ID.periodm.male.noart.age1)
ID.pos.male.noart.age1 <- result.male.noart.age1$ID.pos
row.num.male.noart.age1 <- result.male.noart.age1$row.num

result.male.noart.age2 <- position.calc(ID.state = ID.state.male.noart.age2, ID.periodm = ID.periodm.male.noart.age2)
ID.pos.male.noart.age2 <- result.male.noart.age2$ID.pos
row.num.male.noart.age2 <- result.male.noart.age2$row.num

result.male.noart.age3 <- position.calc(ID.state = ID.state.male.noart.age3, ID.periodm = ID.periodm.male.noart.age3)
ID.pos.male.noart.age3 <- result.male.noart.age3$ID.pos
row.num.male.noart.age3 <- result.male.noart.age3$row.num

result.female.art.age1 <- position.calc(ID.state = ID.state.female.art.age1, ID.periodm = ID.periodm.female.art.age1)
ID.pos.female.art.age1 <- result.female.art.age1$ID.pos
row.num.female.art.age1 <- result.female.art.age1$row.num

result.female.art.age2 <- position.calc(ID.state = ID.state.female.art.age2, ID.periodm = ID.periodm.female.art.age2)
ID.pos.female.art.age2 <- result.female.art.age2$ID.pos
row.num.female.art.age2 <- result.female.art.age2$row.num

result.female.art.age3 <- position.calc(ID.state = ID.state.female.art.age3, ID.periodm = ID.periodm.female.art.age3)
ID.pos.female.art.age3 <- result.female.art.age3$ID.pos
row.num.female.art.age3 <- result.female.art.age3$row.num

result.female.noart.age1 <- position.calc(ID.state = ID.state.female.noart.age1, ID.periodm = ID.periodm.female.noart.age1)
ID.pos.female.noart.age1 <- result.female.noart.age1$ID.pos
row.num.female.noart.age1 <- result.female.noart.age1$row.num

result.female.noart.age2 <- position.calc(ID.state = ID.state.female.noart.age2, ID.periodm = ID.periodm.female.noart.age2)
ID.pos.female.noart.age2 <- result.female.noart.age2$ID.pos
row.num.female.noart.age2 <- result.female.noart.age2$row.num

result.female.noart.age3 <- position.calc(ID.state = ID.state.female.noart.age3, ID.periodm = ID.periodm.female.noart.age3)
ID.pos.female.noart.age3 <- result.female.noart.age3$ID.pos
row.num.female.noart.age3 <- result.female.noart.age3$row.num

################ Estimating p-matrix given s progression status using the data set without large month gap separation #############
time.start = proc.time()
opt.male.art.age1 = optim(rep(0,19), ID.pos = ID.pos.male.art.age1, row.num = row.num.male.art.age1, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.art.age1 = time.end - time.start
ind.pmatrix.male.art.age1 = round(returnMatrix(opt.male.art.age1$par),5)

time.start = proc.time()
opt.male.art.age2 = optim(rep(0,19), ID.pos = ID.pos.male.art.age2, row.num = row.num.male.art.age2, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.art.age2 = time.end - time.start
ind.pmatrix.male.art.age2 = round(returnMatrix(opt.male.art.age2$par),5)

time.start = proc.time()
opt.male.art.age3 = optim(rep(0,19), ID.pos = ID.pos.male.art.age3, row.num = row.num.male.art.age3, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.art.age3 = time.end - time.start
ind.pmatrix.male.art.age3 = round(returnMatrix(opt.male.art.age3$par),5)

time.start = proc.time()
opt.male.noart.age1 = optim(rep(0,19), ID.pos = ID.pos.male.noart.age1, row.num = row.num.male.noart.age1, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.noart.age1 = time.end - time.start
ind.pmatrix.male.noart.age1 = round(returnMatrix(opt.male.noart.age1$par),5)

time.start = proc.time()
opt.male.noart.age2 = optim(rep(0,19), ID.pos = ID.pos.male.noart.age2, row.num = row.num.male.noart.age2, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.noart.age2 = time.end - time.start
ind.pmatrix.male.noart.age2 = round(returnMatrix(opt.male.noart.age2$par),5)

time.start = proc.time()
opt.male.noart.age3 = optim(rep(0,19), ID.pos = ID.pos.male.noart.age3, row.num = row.num.male.noart.age3, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.noart.age3 = time.end - time.start
ind.pmatrix.male.noart.age3 = round(returnMatrix(opt.male.noart.age3$par),5)

time.start = proc.time()
opt.female.art.age1 = optim(rep(0,19), ID.pos = ID.pos.female.art.age1, row.num = row.num.female.art.age1, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.art.age1 = time.end - time.start
ind.pmatrix.female.art.age1 = round(returnMatrix(opt.female.art.age1$par),5)

time.start = proc.time()
opt.female.art.age2 = optim(rep(0,19), ID.pos = ID.pos.female.art.age2, row.num = row.num.female.art.age2, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.art.age2 = time.end - time.start
ind.pmatrix.female.art.age2 = round(returnMatrix(opt.female.art.age2$par),5)

time.start = proc.time()
opt.female.art.age3 = optim(rep(0,19), ID.pos = ID.pos.female.art.age3, row.num = row.num.female.art.age3, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.art.age3 = time.end - time.start
ind.pmatrix.female.art.age3 = round(returnMatrix(opt.female.art.age3$par),5)

time.start = proc.time()
opt.female.noart.age1 = optim(rep(0,19), ID.pos = ID.pos.female.noart.age1, row.num = row.num.female.noart.age1, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.noart.age1 = time.end - time.start
ind.pmatrix.female.noart.age1 = round(returnMatrix(opt.female.noart.age1$par),5)

time.start = proc.time()
opt.female.noart.age2 = optim(rep(0,19), ID.pos = ID.pos.female.noart.age2, row.num = row.num.female.noart.age2, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.noart.age2 = time.end - time.start
ind.pmatrix.female.noart.age2 = round(returnMatrix(opt.female.noart.age2$par),5)

time.start = proc.time()
opt.female.noart.age3 = optim(rep(0,19), ID.pos = ID.pos.female.noart.age3, row.num = row.num.female.noart.age3, negloglikelihood, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.noart.age3 = time.end - time.start
ind.pmatrix.female.noart.age3 = round(returnMatrix(opt.female.noart.age3$par),5)

################ Estimating p-matrix given s progression status using the data set with large month gap separation #############
time.start = proc.time()
opt.male.art.age1 = optim(rep(0,19), ID.gap = ID.gap.male.art.age1, ID.I = ID.I.male.art.age1, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.art.age1 = time.end - time.start
ind.pmatrix.male.art.age1 = round(returnMatrix(opt.male.art.age1$par),5)

time.start = proc.time()
opt.male.art.age2 = optim(rep(0,19), ID.gap = ID.gap.male.art.age2, ID.I = ID.I.male.art.age2, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.art.age2 = time.end - time.start
ind.pmatrix.male.art.age2 = round(returnMatrix(opt.male.art.age2$par),5)

time.start = proc.time()
opt.male.art.age3 = optim(rep(0,19), ID.gap = ID.gap.male.art.age3, ID.I = ID.I.male.art.age3, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.art.age3 = time.end - time.start
ind.pmatrix.male.art.age3 = round(returnMatrix(opt.male.art.age3$par),5)

time.start = proc.time()
opt.female.art.age1 = optim(rep(0,19), ID.gap = ID.gap.female.art.age1, ID.I = ID.I.female.art.age1, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.art.age1 = time.end - time.start
ind.pmatrix.female.art.age1 = round(returnMatrix(opt.female.art.age1$par),5)

time.start = proc.time()
opt.female.art.age2 = optim(rep(0,19), ID.gap = ID.gap.female.art.age2, ID.I = ID.I.female.art.age2, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.art.age2 = time.end - time.start
ind.pmatrix.female.art.age2 = round(returnMatrix(opt.female.art.age2$par),5)

time.start = proc.time()
opt.female.art.age3 = optim(rep(0,19), ID.gap = ID.gap.female.art.age3, ID.I = ID.I.female.art.age3, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.art.age3 = time.end - time.start
ind.pmatrix.female.art.age3 = round(returnMatrix(opt.female.art.age3$par),5)

time.start = proc.time()
opt.male.noart.age1 = optim(rep(0,19), ID.gap = ID.gap.male.noart.age1, ID.I = ID.I.male.noart.age1, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.noart.age1 = time.end - time.start
ind.pmatrix.male.noart.age1 = round(returnMatrix(opt.male.noart.age1$par),5)

time.start = proc.time()
opt.male.noart.age2 = optim(rep(0,19), ID.gap = ID.gap.male.noart.age2, ID.I = ID.I.male.noart.age2, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.noart.age2 = time.end - time.start
ind.pmatrix.male.noart.age2 = round(returnMatrix(opt.male.noart.age2$par),5)

time.start = proc.time()
opt.male.noart.age3 = optim(rep(0,19), ID.gap = ID.gap.male.noart.age3, ID.I = ID.I.male.noart.age3, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.male.noart.age3 = time.end - time.start
ind.pmatrix.male.noart.age3 = round(returnMatrix(opt.male.noart.age3$par),5)

time.start = proc.time()
opt.female.noart.age1 = optim(rep(0,19), ID.gap = ID.gap.female.noart.age1, ID.I = ID.I.female.noart.age1, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.noart.age1 = time.end - time.start
ind.pmatrix.female.noart.age1 = round(returnMatrix(opt.female.noart.age1$par),5)

time.start = proc.time()
opt.female.noart.age2 = optim(rep(0,19), ID.gap = ID.gap.female.noart.age2, ID.I = ID.I.female.noart.age2, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.noart.age2 = time.end - time.start
ind.pmatrix.female.noart.age2 = round(returnMatrix(opt.female.noart.age2$par),5)

time.start = proc.time()
opt.female.noart.age3 = optim(rep(0,19), ID.gap = ID.gap.female.noart.age3, ID.I = ID.I.female.noart.age3, gap.max = 36, negloglikelihood.monthGap, method = "L-BFGS-B", lower = rep(-Inf,19), upper = rep(Inf,19), hessian = TRUE)
time.end = proc.time()
runtime.female.noart.age3 = time.end - time.start
ind.pmatrix.female.noart.age3 = round(returnMatrix(opt.female.noart.age3$par),5)


##### Compare old data set individual fit result to new data set result #####
load("~/Documents/research Bao/NewDataSet/IndividualFitResult_NoGap.RData")
new.logit.ind.better <- ind.logit.calculate(1)
new.logit.ind.worse <- ind.logit.calculate(2)
new.logit.ind.stay <- ind.logit.calculate(3)
new.logit.ind.death <- ind.logit.calculate(4)
load("~/Documents/research Bao/Code and RData/1729new.RData")
old.logit.ind.better <- ind.logit.calculate(1)
old.logit.ind.worse <- ind.logit.calculate(2)
old.logit.ind.stay <- ind.logit.calculate(3)
old.logit.ind.death <- ind.logit.calculate(4)

new.logit.ind <- c(new.logit.ind.better, new.logit.ind.death, new.logit.ind.stay, new.logit.ind.worse)
old.logit.ind <- c(old.logit.ind.better, old.logit.ind.death, old.logit.ind.stay, old.logit.ind.worse)
b <- rep('better', length(new.logit.ind.better))
d <- rep('death', length(new.logit.ind.death))
s <- rep('stay', length(new.logit.ind.stay))
w <- rep('worse', length(new.logit.ind.worse))
type <- c(b,d,s,w)
scatter.data <- data.frame(new.logit.ind, old.logit.ind, type)
p <- ggplot(scatter.data, aes(x = new.logit.ind, y = old.logit.ind, color = type, shape = type)) + 
  geom_point(size = 3, alpha = 1) 
p + geom_abline(intercept = 0, slope = 1)

##### calculate 95% confidence interval for thetas #####
CI.male.art.age1 <- CIcalculate(opt.male.art.age1$par, solve(opt.male.art.age1$hessian))
CI.male.art.age2 <- CIcalculate(opt.male.art.age2$par, solve(opt.male.art.age2$hessian))
CI.male.art.age3 <- CIcalculate(opt.male.art.age3$par, solve(opt.male.art.age3$hessian))
CI.female.art.age1 <- CIcalculate(opt.female.art.age1$par, solve(opt.female.art.age1$hessian))
CI.female.art.age2 <- CIcalculate(opt.female.art.age2$par, solve(opt.female.art.age2$hessian))
CI.female.art.age3 <- CIcalculate(opt.female.art.age3$par, solve(opt.female.art.age3$hessian))
CI.male.noart.age1 <- CIcalculate(opt.male.noart.age1$par, solve(opt.male.noart.age1$hessian))
CI.male.noart.age2 <- CIcalculate(opt.male.noart.age2$par, solve(opt.male.noart.age2$hessian))
CI.male.noart.age3 <- CIcalculate(opt.male.noart.age3$par, solve(opt.male.noart.age3$hessian))
CI.female.noart.age1 <- CIcalculate(opt.female.noart.age1$par, solve(opt.female.noart.age1$hessian))
CI.female.noart.age2 <- CIcalculate(opt.female.noart.age2$par, solve(opt.female.noart.age2$hessian))
CI.female.noart.age3 <- CIcalculate(opt.female.noart.age3$par, solve(opt.female.noart.age3$hessian))

##### Combined Fit Estimation #####
time.start = proc.time()
opt.beta = optim(rep(0,95), likelihood.estimation, method = "L-BFGS-B", lower = rep(-Inf,95), upper =  rep(Inf,95), hessian = FALSE)
time.end = proc.time()
runtime.opt = time.end - time.start

beta0 <- opt.beta$par[1:19]
beta1 <- opt.beta$par[20:38]
beta2 <- opt.beta$par[39:57]
beta3 <- opt.beta$par[58:76]
beta4 <- opt.beta$par[77:95]

comb.pmatrix.male.noart.age1 <- round(returnMatrix(beta0),5)

comb.pmatrix.male.noart.age2 <- round(returnMatrix(beta0 + beta2),5)

comb.pmatrix.male.noart.age3 <- round(returnMatrix(beta0 + beta3),5)

comb.pmatrix.male.art.age1 <- round(returnMatrix(beta0 + beta4),5)

comb.pmatrix.male.art.age2 <- round(returnMatrix(beta0 + beta2 + beta4),5)

comb.pmatrix.male.art.age3 <- round(returnMatrix(beta0 + beta3 + beta4),5)

comb.pmatrix.female.noart.age1 <- round(returnMatrix(beta0 + beta1),5)

comb.pmatrix.female.noart.age2 <- round(returnMatrix(beta0 + beta1 + beta2),5)

comb.pmatrix.female.noart.age3 <- round(returnMatrix(beta0 + beta1 + beta3),5)

comb.pmatrix.female.art.age1 <- round(returnMatrix(beta0 + beta1 + beta4),5)

comb.pmatrix.female.art.age2 <- round(returnMatrix(beta0 + beta1 + beta2 + beta4),5)

comb.pmatrix.female.art.age3 <- round(returnMatrix(beta0 + beta1 + beta3 + beta4),5)

##### Create individual fit and combined fit logit comparison plot ####
logit.ind.better <- ind.logit.calculate(1)
logit.ind.worse <- ind.logit.calculate(2)
logit.ind.stay <- ind.logit.calculate(3)
logit.ind.death <- ind.logit.calculate(4)
logit.ind.stay.p11 <- ind.logit.calculate(5)
logit.ind.stay.else <- ind.logit.calculate(6)

logit.comb.better <- comb.logit.calculate(1)
logit.comb.worse <- comb.logit.calculate(2)
logit.comb.stay <- comb.logit.calculate(3)
logit.comb.death <- comb.logit.calculate(4)
logit.comb.stay.p11 <- comb.logit.calculate(5)
logit.comb.stay.else <- comb.logit.calculate(6)

logit.ind <- c(logit.ind.better, logit.ind.death, logit.ind.stay, logit.ind.worse)
logit.comb <- c(logit.comb.better, logit.comb.death, logit.comb.stay, logit.comb.worse)
b <- rep('better', length(logit.comb.better))
d <- rep('death', length(logit.comb.death))
s <- rep('stay', length(logit.comb.stay))
w <- rep('worse', length(logit.comb.worse))
type <- c(b,d,s,w)
scatter.data <- data.frame(logit.ind, logit.comb, type)
p <- ggplot(scatter.data, aes(x = logit.ind, y = logit.comb, color = type, shape = type)) + 
  geom_point(size = 3, alpha = 1) 
p + geom_abline(intercept = 0, slope = 1)

logit.ind <- c(logit.ind.better, logit.ind.death, logit.ind.stay.p11, logit.ind.stay.else, logit.ind.worse)
logit.comb <- c(logit.comb.better, logit.comb.death, logit.comb.stay.p11, logit.comb.stay.else, logit.comb.worse)
b <- rep('better', length(logit.comb.better))
d <- rep('death', length(logit.comb.death))
s.p11 <- rep('stay.p11', length(logit.comb.stay.p11))
s.else <- rep('stay.else', length(logit.comb.stay.else))
w <- rep('worse', length(logit.comb.worse))
type <- c(b,d,s.p11,s.else,w)
scatter.data <- data.frame(logit.ind, logit.comb, type)
p <- ggplot(scatter.data, aes(x = logit.ind, y = logit.comb, color = type, shape = type)) + 
  geom_point(size = 3, alpha = 1) 
p + geom_abline(intercept = 0, slope = 1)

bmatrix.comb <- matrix(round(c(beta0,beta1,beta2,beta3,beta4), 4), nrow = 19, ncol = 5)
# male => I.gender = 0
# female => I.gender = 1
# age1 => I.age2 = 0, I.age3 = 0
# age2 => I.age2 = 1, I.age3 = 0
# age3 => I.age2 = 0, I.age3 = 1
# ART => I.ART = 1
# no ART => I.ART = 0
beta0.individual <- opt.male.noart.age1$par

beta1.individual1 <- c(opt.female.art.age1$par - opt.male.art.age1$par)
beta1.individual2 <- c(opt.female.art.age2$par - opt.male.art.age2$par)
beta1.individual3 <- c(opt.female.art.age3$par - opt.male.art.age3$par)
beta1.individual4 <- c(opt.female.noart.age1$par - opt.male.noart.age1$par)
beta1.individual5 <- c(opt.female.noart.age2$par - opt.male.noart.age2$par) 
beta1.individual6 <- c(opt.female.noart.age3$par - opt.male.noart.age3$par)
beta1.individual <- (beta1.individual1 + beta1.individual2 + beta1.individual3 + beta1.individual4 + beta1.individual5 + beta1.individual6)/6
beta1.individual.sd <- sqrt(((beta1.individual1 - beta1.individual)^2 + (beta1.individual2 - beta1.individual)^2 + (beta1.individual3 - beta1.individual)^2 + (beta1.individual4 - beta1.individual)^2 + (beta1.individual5 - beta1.individual)^2 + (beta1.individual6 - beta1.individual)^2)/5)

beta2.individual1 <- c(opt.male.art.age2$par - opt.male.art.age1$par)
beta2.individual2 <- c(opt.male.noart.age2$par - opt.male.noart.age1$par)
beta2.individual3 <- c(opt.female.art.age2$par - opt.female.art.age1$par)
beta2.individual4 <- c(opt.female.noart.age2$par - opt.female.noart.age1$par)
beta2.individual <- (beta2.individual1 + beta2.individual2 + beta2.individual3 + beta2.individual4)/4
beta2.individual.sd <- sqrt(((beta2.individual1 - beta2.individual)^2 + (beta2.individual2 - beta2.individual)^2 + (beta2.individual3 - beta2.individual)^2 + (beta2.individual4 - beta2.individual)^2)/3)

beta3.individual1 <- c(opt.male.art.age3$par - opt.male.art.age1$par)
beta3.individual2 <- c(opt.male.noart.age3$par - opt.male.noart.age1$par)
beta3.individual3 <- c(opt.female.art.age3$par - opt.female.art.age1$par)
beta3.individual4 <- c(opt.female.noart.age3$par - opt.female.noart.age1$par)
beta3.individual <- (beta3.individual1 + beta3.individual2 + beta3.individual3 + beta3.individual4)/4
beta3.individual.sd <- sqrt(((beta3.individual1 - beta3.individual)^2 + (beta3.individual2 - beta3.individual)^2 + (beta3.individual3 - beta3.individual)^2 + (beta3.individual4 - beta3.individual)^2)/3)

beta4.individual1 <- c(opt.male.art.age1$par - opt.male.noart.age1$par)
beta4.individual2 <- c(opt.male.art.age2$par - opt.male.noart.age2$par)
beta4.individual3 <- c(opt.male.art.age3$par - opt.male.noart.age3$par)
beta4.individual4 <- c(opt.female.art.age1$par - opt.female.noart.age1$par)
beta4.individual5 <- c(opt.female.art.age2$par - opt.female.noart.age2$par)
beta4.individual6 <- c(opt.female.art.age3$par - opt.female.noart.age3$par)
beta4.individual <- (beta4.individual1 + beta4.individual2 + beta4.individual3 + beta4.individual4 + beta4.individual5 + beta4.individual6)/6
beta4.individual.sd <- sqrt(((beta4.individual1 - beta4.individual)^2 + (beta4.individual2 - beta4.individual)^2 + (beta4.individual3 - beta4.individual)^2 + (beta4.individual4 - beta4.individual)^2 + (beta4.individual5 - beta4.individual)^2 + (beta4.individual6 - beta4.individual)^2)/5)

bmatrix.ind <- matrix(round(c(beta0.individual,beta1.individual,beta2.individual,beta3.individual,beta4.individual),4), nrow = 19, ncol = 5)

















