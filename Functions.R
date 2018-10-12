##### separate male data into art and no art #####
# used after gender separation. May do modification for different use.
separate.art <- function(ID.art, ID.state, ID.CD4count, ID.periodm, ID.age, ID.agem, ID.ARTAge, ID.ARTdate) {
  ID.state.art <- data.frame(matrix(NA,nrow = nrow(ID.state), ncol = ncol(ID.state)))
  ID.state.noart <- data.frame(matrix(NA,nrow = nrow(ID.state), ncol = ncol(ID.state)))
  ID.CD4count.art <- data.frame(matrix(NA,nrow = nrow(ID.CD4count), ncol = ncol(ID.CD4count)))
  ID.CD4count.noart <- data.frame(matrix(NA,nrow = nrow(ID.CD4count), ncol = ncol(ID.CD4count)))
  ID.periodm.art <- data.frame(matrix(NA,nrow = nrow(ID.periodm), ncol = ncol(ID.periodm)))
  ID.periodm.noart <- data.frame(matrix(NA,nrow = nrow(ID.periodm), ncol = ncol(ID.periodm)))
  ID.age.art <- data.frame(matrix(NA,nrow = nrow(ID.age), ncol = ncol(ID.age)))
  ID.age.noart <- data.frame(matrix(NA,nrow = nrow(ID.age), ncol = ncol(ID.age)))
  ID.agem.art <- data.frame(matrix(NA,nrow = nrow(ID.agem), ncol = ncol(ID.agem)))
  ID.agem.noart <- data.frame(matrix(NA,nrow = nrow(ID.agem), ncol = ncol(ID.agem)))
  ID.ARTAge.art <- c(rep(NA,length(ID.ARTAge)))
  ID.ARTAge.noart <- c(rep(NA,length(ID.ARTAge)))
  
  for (i in 1:nrow(ID.art)) {
    row.art <- ID.art[i,]
    row.art <- row.art[!is.na(row.art)] # get the i_th row of ID.art
    l = length(row.art)
    for (j in 1:l) { # check each position in the row
      mark = 0 # set mark to be 0
      if (row.art[j] == 1) { # If the individual got ART
        mark = 1
        if (j != 1 && ID.ARTdate[i,(j - 1)] <= 0 && ID.ARTdate[i,(j - 1)] >= -1) {
          if (j > 2) {
            for (m in 1:(j - 1)) {
              ID.state.noart[i,m] = ID.state[i,m]
              ID.CD4count.noart[i,m] = ID.CD4count[i,m]
              ID.age.noart[i,m] = ID.age[i,m]
              ID.agem.noart[i,m] = ID.agem[i,m]
              ID.ARTAge.noart[i] = 4
            }
            for (m in 1:(j - 2)) {
              ID.periodm.noart[i,m] = ID.periodm[i,m]
            }
          }
          for (m in (j - 1):l) {
            ID.state.art[i,m - j + 2] = ID.state[i,m]
            ID.CD4count.art[i,m - j + 2] = ID.CD4count[i,m]
            ID.age.art[i,m - j + 2] = ID.age[i,m]
            ID.agem.art[i,m - j + 2] = ID.agem[i,m]
            ID.ARTAge.art[i] = ID.ARTAge[i]
          }
          for (m in (j - 1):(l - 1)) {
            ID.periodm.art[i,m - j + 2] = ID.periodm[i,m]
          }
        }
        else if (ID.ARTdate[i,j] <= 1 && ID.ARTdate[i,j] >= 0) {
          if (j != 1) {
            for (m in 1:j) {
              ID.state.noart[i,m] = ID.state[i,m]
              ID.CD4count.noart[i,m] = ID.CD4count[i,m]
              ID.age.noart[i,m] = ID.age[i,m]
              ID.agem.noart[i,m] = ID.agem[i,m]
              ID.ARTAge.noart[i] = 4
            }
            for (m in 1:(j - 1)) {
              ID.periodm.noart[i,m] = ID.periodm[i,m]
            }
          }
          if (j != l) {
            for (m in j:l) {
              ID.state.art[i,m - j + 1] = ID.state[i,m]
              ID.CD4count.art[i,m - j + 1] = ID.CD4count[i,m]
              ID.age.art[i,m - j + 1] = ID.age[i,m]
              ID.agem.art[i,m - j + 1] = ID.agem[i,m]
              ID.ARTAge.art[i] = ID.ARTAge[i]
            }
            for (m in j:(l - 1)) {
              ID.periodm.art[i,m - j + 1] = ID.periodm[i,m]
            }
          }
        }
        else {
          if (j > 2) {
            for (m in 1:(j - 1)) {
              ID.state.noart[i,m] = ID.state[i,m]
              ID.CD4count.noart[i,m] = ID.CD4count[i,m]
              ID.age.noart[i,m] = ID.age[i,m]
              ID.agem.noart[i,m] = ID.agem[i,m]
              ID.ARTAge.noart[i] = 4
            }
            for (m in 1:(j - 2)) {
              ID.periodm.noart[i,m] = ID.periodm[i,m]
            }
          }
          if (j != l) {
            for (m in j:l) {
              ID.state.art[i,m - j + 1] = ID.state[i,m]
              ID.CD4count.art[i,m - j + 1] = ID.CD4count[i,m]
              ID.age.art[i,m - j + 1] = ID.age[i,m]
              ID.agem.art[i,m - j + 1] = ID.agem[i,m]
              ID.ARTAge.art[i] = ID.ARTAge[i]
            }
            for (m in j:(l - 1)) {
              ID.periodm.art[i,m - j + 1] = ID.periodm[i,m]
            }
          }
        }
        break
      }
    }
    if (mark == 0) {
      ID.state.noart[i,] = ID.state[i,]
      ID.CD4count.noart[i,] = ID.CD4count[i,]
      ID.age.noart[i,] = ID.age[i,]
      ID.agem.noart[i,] = ID.agem[i,]
      ID.periodm.noart[i,] = ID.periodm[i,]
      ID.ARTAge.noart[i] = 4
    }
  }
  
  ID.state.art <- ID.state.art[rowSums(is.na(ID.state.art)) != ncol(ID.state.art), ]
  ID.state.noart <- ID.state.noart[rowSums(is.na(ID.state.noart)) != ncol(ID.state.noart), ]
  ID.CD4count.art <- ID.CD4count.art[rowSums(is.na(ID.CD4count.art)) != ncol(ID.CD4count.art), ]
  ID.CD4count.noart <- ID.CD4count.noart[rowSums(is.na(ID.CD4count.noart)) != ncol(ID.CD4count.noart), ]
  ID.periodm.art <- ID.periodm.art[rowSums(is.na(ID.periodm.art)) != ncol(ID.periodm.art), ]
  ID.periodm.noart <- ID.periodm.noart[rowSums(is.na(ID.periodm.noart)) != ncol(ID.periodm.noart), ]
  ID.age.art <- ID.age.art[rowSums(is.na(ID.age.art)) != ncol(ID.age.art), ]
  ID.age.noart <- ID.age.noart[rowSums(is.na(ID.age.noart)) != ncol(ID.age.noart), ]
  ID.agem.art <- ID.agem.art[rowSums(is.na(ID.agem.art)) != ncol(ID.agem.art), ]
  ID.agem.noart <- ID.agem.noart[rowSums(is.na(ID.agem.noart)) != ncol(ID.agem.noart), ]
  ID.ARTAge.art <- ID.ARTAge.art[!is.na(ID.ARTAge.art)]
  ID.ARTAge.noart <- ID.ARTAge.noart[!is.na(ID.ARTAge.noart)]
  
  results <- list()
  results$ID.state.art <- ID.state.art
  results$ID.state.noart <- ID.state.noart
  results$ID.CD4count.art <- ID.CD4count.art
  results$ID.CD4count.noart <- ID.CD4count.noart
  results$ID.periodm.art <- ID.periodm.art
  results$ID.periodm.noart <- ID.periodm.noart
  results$ID.age.art <- ID.age.art
  results$ID.age.noart <- ID.age.noart
  results$ID.agem.art <- ID.agem.art
  results$ID.agem.noart <- ID.agem.noart
  results$ID.ARTAge.art <- ID.ARTAge.art
  results$ID.ARTAge.noart <- ID.ARTAge.noart
  
  return(results)
}

####### Separate the group by age categories 1, 2, and 3 ########
separate.age <- function(ID.state, ID.CD4count, ID.periodm, ID.age, ID.agem) {
  ID.state.age1 <- data.frame(matrix(NA,nrow = nrow(ID.state), ncol = ncol(ID.state)))
  ID.state.age2 <- data.frame(matrix(NA,nrow = nrow(ID.state), ncol = ncol(ID.state)))
  ID.state.age3 <- data.frame(matrix(NA,nrow = nrow(ID.state), ncol = ncol(ID.state)))
  ID.CD4count.age1 <- data.frame(matrix(NA,nrow = nrow(ID.CD4count), ncol = ncol(ID.CD4count)))
  ID.CD4count.age2 <- data.frame(matrix(NA,nrow = nrow(ID.CD4count), ncol = ncol(ID.CD4count)))
  ID.CD4count.age3 <- data.frame(matrix(NA,nrow = nrow(ID.CD4count), ncol = ncol(ID.CD4count)))
  ID.periodm.age1 <- data.frame(matrix(NA,nrow = nrow(ID.periodm), ncol = ncol(ID.periodm)))
  ID.periodm.age2 <- data.frame(matrix(NA,nrow = nrow(ID.periodm), ncol = ncol(ID.periodm)))
  ID.periodm.age3 <- data.frame(matrix(NA,nrow = nrow(ID.periodm), ncol = ncol(ID.periodm)))
  
  for (i in 1:nrow(ID.age)) {
    row.age = ID.age[i,]
    row.age <- row.age[!is.na(row.age)] # get the i_th row of ID.age
    l = length(row.age)
    for (j in 1:l) { # check each position in the row
      if (row.age[j] == 1) { # If the individual is in age category 1
        ID.state.age1[i,j] = ID.state[i,j]
        ID.CD4count.age1[i,j] = ID.CD4count[i,j]
        if (j > 1) { # period is different from state
          ID.periodm.age1[i,j - 1] = ID.periodm[i,j - 1]
        }
      }
      else if (row.age[j] == 2) {
        transition = 0 # mark if a transition happens
        if (j != 1 && row.age[j - 1] == 1) { # if previous age category is 1
          transition = 1 # mark as a transition happens
          if (12*35 - ID.agem[i,j - 1] <= 1) { 
            ID.state.age2[i,1] = ID.state[i,j - 1]
            ID.state.age2[i,2] = ID.state[i,j]
            ID.CD4count.age2[i,1] = ID.CD4count[i,j - 1]
            ID.CD4count.age2[i,2] = ID.CD4count[i,j]
            ID.periodm.age2[i,1] = ID.periodm[i,j - 1]
          }
          else if (ID.agem[i,j] - 12*35 <= 1) { 
            ID.state.age1[i,j] = ID.state[i,j]
            ID.CD4count.age1[i,j] = ID.CD4count[i,j]
            ID.periodm.age1[i,j - 1] = ID.periodm[i,j - 1]
            ID.state.age2[i,1] = ID.state[i,j]
            ID.CD4count.age2[i,1] = ID.CD4count[i,j]
          }
          else {
            ID.state.age2[i,1] = ID.state[i,j]
            ID.CD4count.age2[i,1] = ID.CD4count[i,j]
          }
        }
        else if (transition == 0) {
          ID.state.age2[i,sum(!is.na(ID.state.age2[i,])) + 1] = ID.state[i,j]
          ID.CD4count.age2[i,sum(!is.na(ID.CD4count.age2[i,])) + 1] = ID.CD4count[i,j]
          if (j > 1) {
            ID.periodm.age2[i,sum(!is.na(ID.periodm.age2[i,])) + 1] = ID.periodm[i,j - 1]
          }
        }
        else if (transition == 1) {
          ID.state.age2[i,sum(!is.na(ID.state.age2[i,])) + 1] = ID.state[i,j]
          ID.CD4count.age2[i,sum(!is.na(ID.CD4count.age2[i,])) + 1] = ID.CD4count[i,j]
          ID.periodm.age2[i,sum(!is.na(ID.periodm.age2[i,])) + 1] = ID.periodm[i,j - 1]
        }
      }
      else if (row.age[j] == 3) {
        transition = 0 # mark if a transition happens
        if (j != 1 && row.age[j - 1] == 2) { # if previous age category is 2
          transition = 1 # mark as a transition happens
          if (12*45 - ID.agem[i,j - 1] <= 1) { 
            ID.state.age3[i,1] = ID.state[i,j - 1]
            ID.state.age3[i,2] = ID.state[i,j]
            ID.CD4count.age3[i,1] = ID.CD4count[i,j - 1]
            ID.CD4count.age3[i,2] = ID.CD4count[i,j]
            ID.periodm.age3[i,1] = ID.periodm[i,j - 1]
          }
          else if (ID.agem[i,j] - 12*45 <= 1) { 
            ID.state.age2[i,sum(!is.na(ID.state.age2[i,])) + 1] = ID.state[i,j]
            ID.CD4count.age2[i,sum(!is.na(ID.CD4count.age2[i,])) + 1] = ID.CD4count[i,j]
            ID.periodm.age2[i,sum(!is.na(ID.periodm.age2[i,])) + 1] = ID.periodm[i,j - 1]
            ID.state.age3[i,1] = ID.state[i,j]
            ID.CD4count.age3[i,1] = ID.CD4count[i,j]
          }
          else {
            ID.state.age3[i,1] = ID.state[i,j]
            ID.CD4count.age3[i,1] = ID.CD4count[i,j]
          }
        }
        else if (j != 1 && row.age[j - 1] == 1) { # if previous age category is 1
          ID.state.age3[i,1] = ID.state[i,j]
          ID.CD4count.age3[i,1] = ID.CD4count[i,j]
        }
        else if (transition == 0) {
          ID.state.age3[i,sum(!is.na(ID.state.age3[i,])) + 1] = ID.state[i,j]
          ID.CD4count.age3[i,sum(!is.na(ID.CD4count.age3[i,])) + 1] = ID.CD4count[i,j]
          if (j > 1) {
            ID.periodm.age3[i,sum(!is.na(ID.periodm.age3[i,])) + 1] = ID.periodm[i,j - 1]
          }
        }
        else if (transition == 1) {
          ID.state.age3[i,sum(!is.na(ID.state.age3[i,])) + 1] = ID.state[i,j]
          ID.CD4count.age3[i,sum(!is.na(ID.CD4count.age3[i,])) + 1] = ID.CD4count[i,j]
          ID.periodm.age3[i,sum(!is.na(ID.periodm.age3[i,])) + 1] = ID.periodm[i,j - 1]
        }
      }
    }
  }
  
  ID.state.age1 <- ID.state.age1[rowSums(is.na(ID.state.age1)) != ncol(ID.state.age1), ]
  ID.state.age2 <- ID.state.age2[rowSums(is.na(ID.state.age2)) != ncol(ID.state.age2), ]
  ID.state.age3 <- ID.state.age3[rowSums(is.na(ID.state.age3)) != ncol(ID.state.age3), ]
  ID.CD4count.age1 <- ID.CD4count.age1[rowSums(is.na(ID.CD4count.age1)) != ncol(ID.CD4count.age1), ]
  ID.CD4count.age2 <- ID.CD4count.age2[rowSums(is.na(ID.CD4count.age2)) != ncol(ID.CD4count.age2), ]
  ID.CD4count.age3 <- ID.CD4count.age3[rowSums(is.na(ID.CD4count.age3)) != ncol(ID.CD4count.age3), ]
  ID.periodm.age1 <- ID.periodm.age1[rowSums(is.na(ID.periodm.age1)) != ncol(ID.periodm.age1), ]
  ID.periodm.age2 <- ID.periodm.age2[rowSums(is.na(ID.periodm.age2)) != ncol(ID.periodm.age2), ]
  ID.periodm.age3 <- ID.periodm.age3[rowSums(is.na(ID.periodm.age3)) != ncol(ID.periodm.age3), ]
  
  # delete the row with just one observation in state
  del.num = 0
  count = 1
  for (i in 1:nrow(ID.state.age1)) {
    row.state.age1 = ID.state.age1[i,]
    row.state.age1 <- row.state.age1[!is.na(row.state.age1)] # get the i_th row of ID.age
    l = length(row.state.age1)
    if (l == 1) {
      del.num[count] = i
      count = count + 1
    }
  }
  
  if (del.num != 0) {
    ID.state.age1 = ID.state.age1[-del.num,]
    ID.CD4count.age1 = ID.CD4count.age1[-del.num,]
  }
  
  del.num = 0
  count = 1
  for (i in 1:nrow(ID.state.age2)) {
    row.state.age2 = ID.state.age2[i,]
    row.state.age2 <- row.state.age2[!is.na(row.state.age2)] # get the i_th row of ID.age
    l = length(row.state.age2)
    if (l == 1) {
      del.num[count] = i
      count = count + 1
    }
  }
  
  if (del.num != 0) {
    ID.state.age2 = ID.state.age2[-del.num,]
    ID.CD4count.age2 = ID.CD4count.age2[-del.num,]
  }
  
  del.num = 0
  count = 1
  for (i in 1:nrow(ID.state.age3)) {
    row.state.age3 = ID.state.age3[i,]
    row.state.age3 <- row.state.age3[!is.na(row.state.age3)] # get the i_th row of ID.age
    l = length(row.state.age3)
    if (l == 1) {
      del.num[count] = i
      count = count + 1
    }
  }
  
  if (del.num != 0) {
    ID.state.age3 = ID.state.age3[-del.num,]
    ID.CD4count.age3 = ID.CD4count.age3[-del.num,]
  }
  
  results <- list()
  results$ID.state.age1 <- ID.state.age1
  results$ID.state.age2 <- ID.state.age2
  results$ID.state.age3 <- ID.state.age3
  results$ID.CD4count.age1 <- ID.CD4count.age1
  results$ID.CD4count.age2 <- ID.CD4count.age2
  results$ID.CD4count.age3 <- ID.CD4count.age3
  results$ID.periodm.age1 <- ID.periodm.age1
  results$ID.periodm.age2 <- ID.periodm.age2
  results$ID.periodm.age3 <- ID.periodm.age3
  
  return(results)
}

position.calc <- function(ID.state, ID.periodm) {
  # calculate sum(n_i)
  N = nrow(ID.periodm)
  l = 0
  for (i in 1:N) {
    row = ID.periodm[i,]
    row <- row[!is.na(row)]
    l = l + length(row)
  }
  
  # Define the transition indicator matrix
  ID.I = matrix(0,l,64)
  ID.gap = rep(0,l)
  counter = 1
  for (j in 1:nrow(ID.state)) {
    row = ID.state[j,]
    row <- row[!is.na(row)]
    for (k in 2:length(row)) {
      indicator = matrix(0,8,8)
      indicator[row[k - 1],row[k]] = 1
      ID.I[counter,] = as.vector(indicator)
      ID.gap[counter] = ID.periodm[j,k - 1]
      counter = counter + 1
    }
  }
  
  row.num <- max(ID.gap)
  ID.pos <- c()
  for (i in 1:length(ID.gap)) {
    ID.pos[i] <- (ID.gap[i] - 1) * 64 + which(ID.I[i,] == 1)
  }
  
  results <- list()
  results$ID.pos <- ID.pos
  results$row.num <- row.num
  
  return(results)
}

##### Calculate likelihood function ######
# p-matrix is the following 
# theta[1]  theta[2]     0         0         0         0          0          1-theta[1]-theta[2]
# theta[3]  theta[4]  theta[5]     0         0         0          0       1-theta[3]-theta[4]-theta[5]
#    0      theta[6]  theta[7]  theta[8]     0         0          0       1-theta[6]-theta[7]-theta[8]
#    0         0      theta[9]  theta[10] theta[11]    0          0      1-theta[9]-theta[10]-theta[11]
#    0         0         0      theta[12] theta[13] theta[14]     0      1-theta[12]-theta[13]-theta[14]
#    0         0         0         0      theta[15] theta[16]  theta[17] 1-theta[15]-theta[16]-theta[17]
#    0         0         0         0         0      theta[18]  theta[19]     1-theta[18]-theta[19]
#    0         0         0         0         0         0          0                 1
# The order of the as.vector command is the following: 
# > m
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6
# [3,]    7    8    9
# > as.vector(m)
# [1] 1 4 7 2 5 8 3 6 9
negloglikelihood <- function(alpha, ID.pos, row.num){
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1  
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH = -sum(final.matrix.vector[ID.pos])
  
  return(neg.lgLH)
}

negloglikelihood.monthGap <- function(alpha, ID.gap, ID.I, gap.max){
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap[which(ID.gap > gap.max)])[order(unique(ID.gap[which(ID.gap > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  powered <-  p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:gap.max) {
    powered <-  powered %*% p.matrix
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap[ID.gap == exceed[i]] <- gap.max + i
  }
  
  result = powered.matrix.vector[ID.gap,] * ID.I
  result[result == 0] <- 1
  logresult = log(result)
  
  neg.lgLH = -sum(logresult)
  
  return(neg.lgLH)
}

###### Create the matrix given the alpha vector ########
returnMatrix <- function(alpha){
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  return(p.matrix)
}

##### Calculate the confidence interval 
CIcalculate <- function(alpha, cov) {
  sim <- mvrnorm(n = 10000, alpha, cov)
  theta <- matrix(NA, 10000, 26)
  for (i in 1:10000) {
    M <- returnMatrix(sim[i,])
    theta[i,] <- c(M[1,1], M[1,2], M[1,8], M[2,1], M[2,2], M[2,3], M[2,8], M[3,2], M[3,3], M[3,4], M[3,8], M[4,3], M[4,4], M[4,5], M[4,8], M[5,4], M[5,5], M[5,6], M[5,8], M[6,5], M[6,6], M[6,7], M[6,8], M[7,6], M[7,7], M[7,8])
  }
  CI <- matrix(NA,26,3)
  for (i in 1:26) {
    CI[i,c(1,3)] <- quantile(theta[,i], c(0.025,0.975))
    CI[i,2] <- mean(theta[,i])
  }
  CI <- round(CI, digits = 5)
  return(CI)
}

##### Calculate likelihood function separating by different combinations with Month Gap ######
likelihood.estimation.monthGap <- function(parameter, gap.max){
  beta0 <- parameter[1:19]
  beta1 <- parameter[20:38]
  beta2 <- parameter[39:57]
  beta3 <- parameter[58:76]
  beta4 <- parameter[77:95]
  # male => I.gender = 0
  # female => I.gender = 1
  # age1 => I.age2 = 0, I.age3 = 0
  # age2 => I.age2 = 1, I.age3 = 0
  # age3 => I.age2 = 0, I.age3 = 1
  # ART => I.ART = 1
  # no ART => I.ART = 0
  alpha.cal <- function(I.gender, I.age2, I.age3, I.ART) {
    alpha.val <- beta0 + beta1*I.gender + beta2*I.age2 + beta3*I.age3 + beta4*I.ART
    return(alpha.val)
  }
  
  alpha <- alpha.cal(0,0,0,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.male.art.age1[which(ID.gap.male.art.age1 > gap.max)])[order(unique(ID.gap.male.art.age1[which(ID.gap.male.art.age1 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.male.art.age1[ID.gap.male.art.age1 == exceed[i]] <- gap.max + i
  }
  
  result.male.art.age1 = powered.matrix.vector[ID.gap.male.art.age1,] * ID.I.male.art.age1
  result.male.art.age1[result.male.art.age1 == 0] <- 1
  logresult.male.art.age1 = log(result.male.art.age1)
  neg.lgLH.male.art.age1 = -sum(logresult.male.art.age1)
  
  alpha <- alpha.cal(0,1,0,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.male.art.age2[which(ID.gap.male.art.age2 > gap.max)])[order(unique(ID.gap.male.art.age2[which(ID.gap.male.art.age2 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.male.art.age2[ID.gap.male.art.age2 == exceed[i]] <- gap.max + i
  }
  
  result.male.art.age2 = powered.matrix.vector[ID.gap.male.art.age2,] * ID.I.male.art.age2
  result.male.art.age2[result.male.art.age2 == 0] <- 1
  logresult.male.art.age2 = log(result.male.art.age2)
  
  neg.lgLH.male.art.age2 = -sum(logresult.male.art.age2)
  
  alpha <- alpha.cal(0,0,1,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.male.art.age3[which(ID.gap.male.art.age3 > gap.max)])[order(unique(ID.gap.male.art.age3[which(ID.gap.male.art.age3 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.male.art.age3[ID.gap.male.art.age3 == exceed[i]] <- gap.max + i
  }
  
  result.male.art.age3 = powered.matrix.vector[ID.gap.male.art.age3,] * ID.I.male.art.age3
  result.male.art.age3[result.male.art.age3 == 0] <- 1
  logresult.male.art.age3 = log(result.male.art.age3)
  
  neg.lgLH.male.art.age3 = -sum(logresult.male.art.age3)
  
  alpha <- alpha.cal(0,0,0,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.male.noart.age1[which(ID.gap.male.noart.age1 > gap.max)])[order(unique(ID.gap.male.noart.age1[which(ID.gap.male.noart.age1 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.male.noart.age1[ID.gap.male.noart.age1 == exceed[i]] <- gap.max + i
  }
  
  result.male.noart.age1 = powered.matrix.vector[ID.gap.male.noart.age1,] * ID.I.male.noart.age1
  result.male.noart.age1[result.male.noart.age1 == 0] <- 1
  logresult.male.noart.age1 = log(result.male.noart.age1)
  
  neg.lgLH.male.noart.age1 = -sum(logresult.male.noart.age1)
  
  alpha <- alpha.cal(0,1,0,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.male.noart.age2[which(ID.gap.male.noart.age2 > gap.max)])[order(unique(ID.gap.male.noart.age2[which(ID.gap.male.noart.age2 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.male.noart.age2[ID.gap.male.noart.age2 == exceed[i]] <- gap.max + i
  }
  
  result.male.noart.age2 = powered.matrix.vector[ID.gap.male.noart.age2,] * ID.I.male.noart.age2
  result.male.noart.age2[result.male.noart.age2 == 0] <- 1
  logresult.male.noart.age2 = log(result.male.noart.age2)
  
  neg.lgLH.male.noart.age2 = -sum(logresult.male.noart.age2)
  
  alpha <- alpha.cal(0,0,1,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.male.noart.age3[which(ID.gap.male.noart.age3 > gap.max)])[order(unique(ID.gap.male.noart.age3[which(ID.gap.male.noart.age3 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.male.noart.age3[ID.gap.male.noart.age3 == exceed[i]] <- gap.max + i
  }
  
  result.male.noart.age3 = powered.matrix.vector[ID.gap.male.noart.age3,] * ID.I.male.noart.age3
  result.male.noart.age3[result.male.noart.age3 == 0] <- 1
  logresult.male.noart.age3 = log(result.male.noart.age3)
  
  neg.lgLH.male.noart.age3 = -sum(logresult.male.noart.age3)
  
  alpha <- alpha.cal(1,0,0,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.female.art.age1[which(ID.gap.female.art.age1 > gap.max)])[order(unique(ID.gap.female.art.age1[which(ID.gap.female.art.age1 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.female.art.age1[ID.gap.female.art.age1 == exceed[i]] <- gap.max + i
  }
  
  result.female.art.age1 = powered.matrix.vector[ID.gap.female.art.age1,] * ID.I.female.art.age1
  result.female.art.age1[result.female.art.age1 == 0] <- 1
  logresult.female.art.age1 = log(result.female.art.age1)
  
  neg.lgLH.female.art.age1 = -sum(logresult.female.art.age1)
  
  alpha <- alpha.cal(1,1,0,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.female.art.age2[which(ID.gap.female.art.age2 > gap.max)])[order(unique(ID.gap.female.art.age2[which(ID.gap.female.art.age2 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.female.art.age2[ID.gap.female.art.age2 == exceed[i]] <- gap.max + i
  }
  
  result.female.art.age2 = powered.matrix.vector[ID.gap.female.art.age2,] * ID.I.female.art.age2
  result.female.art.age2[result.female.art.age2 == 0] <- 1
  logresult.female.art.age2 = log(result.female.art.age2)
  
  neg.lgLH.female.art.age2 = -sum(logresult.female.art.age2)
  
  alpha <- alpha.cal(1,0,1,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.female.art.age3[which(ID.gap.female.art.age3 > gap.max)])[order(unique(ID.gap.female.art.age3[which(ID.gap.female.art.age3 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.female.art.age3[ID.gap.female.art.age3 == exceed[i]] <- gap.max + i
  }
  
  result.female.art.age3 = powered.matrix.vector[ID.gap.female.art.age3,] * ID.I.female.art.age3
  result.female.art.age3[result.female.art.age3 == 0] <- 1
  logresult.female.art.age3 = log(result.female.art.age3)
  
  neg.lgLH.female.art.age3 = -sum(logresult.female.art.age3)
  
  alpha <- alpha.cal(1,0,0,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.female.noart.age1[which(ID.gap.female.noart.age1 > gap.max)])[order(unique(ID.gap.female.noart.age1[which(ID.gap.female.noart.age1 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.female.noart.age1[ID.gap.female.noart.age1 == exceed[i]] <- gap.max + i
  }
  
  result.female.noart.age1 = powered.matrix.vector[ID.gap.female.noart.age1,] * ID.I.female.noart.age1
  result.female.noart.age1[result.female.noart.age1 == 0] <- 1
  logresult.female.noart.age1 = log(result.female.noart.age1)
  
  neg.lgLH.female.noart.age1 = -sum(logresult.female.noart.age1)
  
  alpha <- alpha.cal(1,1,0,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.female.noart.age2[which(ID.gap.female.noart.age2 > gap.max)])[order(unique(ID.gap.female.noart.age2[which(ID.gap.female.noart.age2 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.female.noart.age2[ID.gap.female.noart.age2 == exceed[i]] <- gap.max + i
  }
  
  result.female.noart.age2 = powered.matrix.vector[ID.gap.female.noart.age2,] * ID.I.female.noart.age2
  result.female.noart.age2[result.female.noart.age2 == 0] <- 1
  logresult.female.noart.age2 = log(result.female.noart.age2)
  
  neg.lgLH.female.noart.age2 = -sum(logresult.female.noart.age2)
  
  alpha <- alpha.cal(1,0,1,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  exceed <- unique(ID.gap.female.noart.age3[which(ID.gap.female.noart.age3 > gap.max)])[order(unique(ID.gap.female.noart.age3[which(ID.gap.female.noart.age3 > gap.max)]))]
  row.num <- gap.max + length(exceed)
  
  powered.matrix.vector = matrix(0, row.num, 64)
  # store each powered matrix in one row as a vector 
  for (i in 1:gap.max) {
    powered <-  p.matrix %^% i
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  
  for (i in 1:length(exceed)) {
    powered <-  p.matrix %^% exceed[i]
    powered.matrix.vector[gap.max + i,] <- as.vector(powered)
    ID.gap.female.noart.age3[ID.gap.female.noart.age3 == exceed[i]] <- gap.max + i
  }
  
  result.female.noart.age3 = powered.matrix.vector[ID.gap.female.noart.age3,] * ID.I.female.noart.age3
  result.female.noart.age3[result.female.noart.age3 == 0] <- 1
  logresult.female.noart.age3 = log(result.female.noart.age3)
  
  neg.lgLH.female.noart.age3 = -sum(logresult.female.noart.age3)
  
  return(neg.lgLH.male.art.age1 + neg.lgLH.male.art.age2 + neg.lgLH.male.art.age3 + neg.lgLH.male.noart.age1 + neg.lgLH.male.noart.age2 + neg.lgLH.male.noart.age3 + neg.lgLH.female.art.age1 + neg.lgLH.female.art.age2 + neg.lgLH.female.art.age3 + neg.lgLH.female.noart.age1 + neg.lgLH.female.noart.age2 + neg.lgLH.female.noart.age3)
}

##### Calculate likelihood function separating by different combinations with no Month Gap ######
likelihood.estimation <- function(parameter){
  beta0 <- parameter[1:19]
  beta1 <- parameter[20:38]
  beta2 <- parameter[39:57]
  beta3 <- parameter[58:76]
  beta4 <- parameter[77:95]
  # male => I.gender = 0
  # female => I.gender = 1
  # age1 => I.age2 = 0, I.age3 = 0
  # age2 => I.age2 = 1, I.age3 = 0
  # age3 => I.age2 = 0, I.age3 = 1
  # ART => I.ART = 1
  # no ART => I.ART = 0
  alpha.cal <- function(I.gender, I.age2, I.age3, I.ART) {
    alpha.val <- beta0 + beta1*I.gender + beta2*I.age2 + beta3*I.age3 + beta4*I.ART
    return(alpha.val)
  }
  
  alpha <- alpha.cal(0,0,0,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.male.art.age1, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.male.art.age1) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.male.art.age1 = -sum(final.matrix.vector[ID.pos.male.art.age1])
  
  alpha <- alpha.cal(0,1,0,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.male.art.age2, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.male.art.age2) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.male.art.age2 = -sum(final.matrix.vector[ID.pos.male.art.age2])
  
  alpha <- alpha.cal(0,0,1,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.male.art.age3, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.male.art.age3) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.male.art.age3 = -sum(final.matrix.vector[ID.pos.male.art.age3])
  
  alpha <- alpha.cal(0,0,0,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.male.noart.age1, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.male.noart.age1) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.male.noart.age1 = -sum(final.matrix.vector[ID.pos.male.noart.age1])
  
  alpha <- alpha.cal(0,1,0,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.male.noart.age2, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.male.noart.age2) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.male.noart.age2 = -sum(final.matrix.vector[ID.pos.male.noart.age2])
  
  alpha <- alpha.cal(0,0,1,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.male.noart.age3, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.male.noart.age3) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.male.noart.age3 = -sum(final.matrix.vector[ID.pos.male.noart.age3])
  
  alpha <- alpha.cal(1,0,0,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.female.art.age1, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.female.art.age1) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.female.art.age1 = -sum(final.matrix.vector[ID.pos.female.art.age1])
  
  alpha <- alpha.cal(1,1,0,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.female.art.age2, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.female.art.age2) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.female.art.age2 = -sum(final.matrix.vector[ID.pos.female.art.age2])
  
  alpha <- alpha.cal(1,0,1,1)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.female.art.age3, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.female.art.age3) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.female.art.age3 = -sum(final.matrix.vector[ID.pos.female.art.age3])
  
  alpha <- alpha.cal(1,0,0,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.female.noart.age1, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.female.noart.age1) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.female.noart.age1 = -sum(final.matrix.vector[ID.pos.female.noart.age1])
  
  alpha <- alpha.cal(1,1,0,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.female.noart.age2, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.female.noart.age2) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.female.noart.age2 = -sum(final.matrix.vector[ID.pos.female.noart.age2])
  
  alpha <- alpha.cal(1,0,1,0)
  p.matrix = matrix(0, 8, 8)
  p.matrix[1,1] = 1/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,2] = exp(alpha[1])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[1,8] = exp(alpha[2])/(1 + exp(alpha[1]) + exp(alpha[2]))
  p.matrix[2,1] = exp(alpha[3])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,2] = 1/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,3] = exp(alpha[4])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[2,8] = exp(alpha[5])/(1 + exp(alpha[3]) + exp(alpha[4]) + exp(alpha[5]))
  p.matrix[3,2] = exp(alpha[6])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,3] = 1/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,4] = exp(alpha[7])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[3,8] = exp(alpha[8])/(1 + exp(alpha[6]) + exp(alpha[7]) + exp(alpha[8]))
  p.matrix[4,3] = exp(alpha[9])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,4] = 1/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,5] = exp(alpha[10])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[4,8] = exp(alpha[11])/(1 + exp(alpha[9]) + exp(alpha[10]) + exp(alpha[11]))
  p.matrix[5,4] = exp(alpha[12])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,5] = 1/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,6] = exp(alpha[13])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[5,8] = exp(alpha[14])/(1 + exp(alpha[12]) + exp(alpha[13]) + exp(alpha[14]))
  p.matrix[6,5] = exp(alpha[15])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,6] = 1/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,7] = exp(alpha[16])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[6,8] = exp(alpha[17])/(1 + exp(alpha[15]) + exp(alpha[16]) + exp(alpha[17]))
  p.matrix[7,6] = exp(alpha[18])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,7] = 1/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[7,8] = exp(alpha[19])/(1 + exp(alpha[18]) + exp(alpha[19]))
  p.matrix[8,8] = 1
  
  powered.matrix.vector = matrix(0, row.num.female.noart.age3, 64)
  # store each powered matrix in one row as a vector 
  powered <- p.matrix
  powered.matrix.vector[1,] <- as.vector(powered)
  for (i in 2:row.num.female.noart.age3) {
    powered <- powered %*% p.matrix  
    powered.matrix.vector[i,] <- as.vector(powered)
  }
  powered.matrix.vector[powered.matrix.vector == 0] <- 1  
  
  log.powered.matrix.vector <- log(powered.matrix.vector)
  final.matrix.vector <- as.vector(t(log.powered.matrix.vector))
  
  neg.lgLH.female.noart.age3 = -sum(final.matrix.vector[ID.pos.female.noart.age3])
  
  return(neg.lgLH.male.art.age1 + neg.lgLH.male.art.age2 + neg.lgLH.male.art.age3 + neg.lgLH.male.noart.age1 + neg.lgLH.male.noart.age2 + neg.lgLH.male.noart.age3 + neg.lgLH.female.art.age1 + neg.lgLH.female.art.age2 + neg.lgLH.female.art.age3 + neg.lgLH.female.noart.age1 + neg.lgLH.female.noart.age2 + neg.lgLH.female.noart.age3)
}

##### Calculate individual fit logit for better/worse/stay/death #####
# better = 1, worse = 2, stay = 3, death = 4, stay.p11 = 5, stay.else = 6
ind.logit.calculate <- function(state) {
  if (state == 1) {
    logit <- c(ind.pmatrix.male.art.age1[2,1], ind.pmatrix.male.art.age1[3,2], ind.pmatrix.male.art.age1[4,3],   ind.pmatrix.male.art.age1[5,4], ind.pmatrix.male.art.age1[6,5], ind.pmatrix.male.art.age1[7,6],
              ind.pmatrix.male.art.age2[2,1], ind.pmatrix.male.art.age2[3,2], ind.pmatrix.male.art.age2[4,3],   ind.pmatrix.male.art.age2[5,4], ind.pmatrix.male.art.age2[6,5], ind.pmatrix.male.art.age2[7,6],
              ind.pmatrix.male.art.age3[2,1], ind.pmatrix.male.art.age3[3,2], ind.pmatrix.male.art.age3[4,3],   ind.pmatrix.male.art.age3[5,4], ind.pmatrix.male.art.age3[6,5], ind.pmatrix.male.art.age3[7,6],
              ind.pmatrix.male.noart.age1[2,1], ind.pmatrix.male.noart.age1[3,2], ind.pmatrix.male.noart.age1[4,3],   ind.pmatrix.male.noart.age1[5,4], ind.pmatrix.male.noart.age1[6,5], ind.pmatrix.male.noart.age1[7,6],
              ind.pmatrix.male.noart.age2[2,1], ind.pmatrix.male.noart.age2[3,2], ind.pmatrix.male.noart.age2[4,3],   ind.pmatrix.male.noart.age2[5,4], ind.pmatrix.male.noart.age2[6,5], ind.pmatrix.male.noart.age2[7,6],
              ind.pmatrix.male.noart.age3[2,1], ind.pmatrix.male.noart.age3[3,2], ind.pmatrix.male.noart.age3[4,3],   ind.pmatrix.male.noart.age3[5,4], ind.pmatrix.male.noart.age3[6,5], ind.pmatrix.male.noart.age3[7,6],
              ind.pmatrix.female.art.age1[2,1], ind.pmatrix.female.art.age1[3,2], ind.pmatrix.female.art.age1[4,3],   ind.pmatrix.female.art.age1[5,4], ind.pmatrix.female.art.age1[6,5], ind.pmatrix.female.art.age1[7,6],
              ind.pmatrix.female.art.age2[2,1], ind.pmatrix.female.art.age2[3,2], ind.pmatrix.female.art.age2[4,3],   ind.pmatrix.female.art.age2[5,4], ind.pmatrix.female.art.age2[6,5], ind.pmatrix.female.art.age2[7,6],
              ind.pmatrix.female.art.age3[2,1], ind.pmatrix.female.art.age3[3,2], ind.pmatrix.female.art.age3[4,3],   ind.pmatrix.female.art.age3[5,4], ind.pmatrix.female.art.age3[6,5], ind.pmatrix.female.art.age3[7,6],
              ind.pmatrix.female.noart.age1[2,1], ind.pmatrix.female.noart.age1[3,2], ind.pmatrix.female.noart.age1[4,3],   ind.pmatrix.female.noart.age1[5,4], ind.pmatrix.female.noart.age1[6,5], ind.pmatrix.female.noart.age1[7,6],
              ind.pmatrix.female.noart.age2[2,1], ind.pmatrix.female.noart.age2[3,2], ind.pmatrix.female.noart.age2[4,3],   ind.pmatrix.female.noart.age2[5,4], ind.pmatrix.female.noart.age2[6,5], ind.pmatrix.female.noart.age2[7,6],
              ind.pmatrix.female.noart.age3[2,1], ind.pmatrix.female.noart.age3[3,2], ind.pmatrix.female.noart.age3[4,3],   ind.pmatrix.female.noart.age3[5,4], ind.pmatrix.female.noart.age3[6,5], ind.pmatrix.female.noart.age3[7,6])
  }
  if (state == 2) {
    logit <- c(ind.pmatrix.male.art.age1[1,2], ind.pmatrix.male.art.age1[2,3], ind.pmatrix.male.art.age1[3,4], ind.pmatrix.male.art.age1[4,5], ind.pmatrix.male.art.age1[5,6], ind.pmatrix.male.art.age1[6,7],
               ind.pmatrix.male.art.age2[1,2], ind.pmatrix.male.art.age2[2,3], ind.pmatrix.male.art.age2[3,4], ind.pmatrix.male.art.age2[4,5], ind.pmatrix.male.art.age2[5,6], ind.pmatrix.male.art.age2[6,7],
               ind.pmatrix.male.art.age3[1,2], ind.pmatrix.male.art.age3[2,3], ind.pmatrix.male.art.age3[3,4], ind.pmatrix.male.art.age3[4,5], ind.pmatrix.male.art.age3[5,6], ind.pmatrix.male.art.age3[6,7],
               ind.pmatrix.male.noart.age1[1,2], ind.pmatrix.male.noart.age1[2,3], ind.pmatrix.male.noart.age1[3,4], ind.pmatrix.male.noart.age1[4,5], ind.pmatrix.male.noart.age1[5,6], ind.pmatrix.male.noart.age1[6,7],
               ind.pmatrix.male.noart.age2[1,2], ind.pmatrix.male.noart.age2[2,3], ind.pmatrix.male.noart.age2[3,4], ind.pmatrix.male.noart.age2[4,5], ind.pmatrix.male.noart.age2[5,6], ind.pmatrix.male.noart.age2[6,7],
               ind.pmatrix.male.noart.age3[1,2], ind.pmatrix.male.noart.age3[2,3], ind.pmatrix.male.noart.age3[3,4], ind.pmatrix.male.noart.age3[4,5], ind.pmatrix.male.noart.age3[5,6], ind.pmatrix.male.noart.age3[6,7],
               ind.pmatrix.female.art.age1[1,2], ind.pmatrix.female.art.age1[2,3], ind.pmatrix.female.art.age1[3,4], ind.pmatrix.female.art.age1[4,5], ind.pmatrix.female.art.age1[5,6], ind.pmatrix.female.art.age1[6,7],
               ind.pmatrix.female.art.age2[1,2], ind.pmatrix.female.art.age2[2,3], ind.pmatrix.female.art.age2[3,4], ind.pmatrix.female.art.age2[4,5], ind.pmatrix.female.art.age2[5,6], ind.pmatrix.female.art.age2[6,7],
               ind.pmatrix.female.art.age3[1,2], ind.pmatrix.female.art.age3[2,3], ind.pmatrix.female.art.age3[3,4], ind.pmatrix.female.art.age3[4,5], ind.pmatrix.female.art.age3[5,6], ind.pmatrix.female.art.age3[6,7],
               ind.pmatrix.female.noart.age1[1,2], ind.pmatrix.female.noart.age1[2,3], ind.pmatrix.female.noart.age1[3,4], ind.pmatrix.female.noart.age1[4,5], ind.pmatrix.female.noart.age1[5,6], ind.pmatrix.female.noart.age1[6,7],
               ind.pmatrix.female.noart.age2[1,2], ind.pmatrix.female.noart.age2[2,3], ind.pmatrix.female.noart.age2[3,4], ind.pmatrix.female.noart.age2[4,5], ind.pmatrix.female.noart.age2[5,6], ind.pmatrix.female.noart.age2[6,7],
               ind.pmatrix.female.noart.age3[1,2], ind.pmatrix.female.noart.age3[2,3], ind.pmatrix.female.noart.age3[3,4], ind.pmatrix.female.noart.age3[4,5], ind.pmatrix.female.noart.age3[5,6], ind.pmatrix.female.noart.age3[6,7])
  }
  if (state == 3) {
    logit <- c(ind.pmatrix.male.art.age1[1,1], ind.pmatrix.male.art.age1[2,2], ind.pmatrix.male.art.age1[3,3], ind.pmatrix.male.art.age1[4,4], ind.pmatrix.male.art.age1[5,5], ind.pmatrix.male.art.age1[6,6], ind.pmatrix.male.art.age1[7,7],
               ind.pmatrix.male.art.age2[1,1], ind.pmatrix.male.art.age2[2,2], ind.pmatrix.male.art.age2[3,3], ind.pmatrix.male.art.age2[4,4], ind.pmatrix.male.art.age2[5,5], ind.pmatrix.male.art.age2[6,6], ind.pmatrix.male.art.age2[7,7],
               ind.pmatrix.male.art.age3[1,1], ind.pmatrix.male.art.age3[2,2], ind.pmatrix.male.art.age3[3,3], ind.pmatrix.male.art.age3[4,4], ind.pmatrix.male.art.age3[5,5], ind.pmatrix.male.art.age3[6,6], ind.pmatrix.male.art.age3[7,7],
               ind.pmatrix.male.noart.age1[1,1], ind.pmatrix.male.noart.age1[2,2], ind.pmatrix.male.noart.age1[3,3], ind.pmatrix.male.noart.age1[4,4], ind.pmatrix.male.noart.age1[5,5], ind.pmatrix.male.noart.age1[6,6], ind.pmatrix.male.noart.age1[7,7],
               ind.pmatrix.male.noart.age2[1,1], ind.pmatrix.male.noart.age2[2,2], ind.pmatrix.male.noart.age2[3,3], ind.pmatrix.male.noart.age2[4,4], ind.pmatrix.male.noart.age2[5,5], ind.pmatrix.male.noart.age2[6,6], ind.pmatrix.male.noart.age2[7,7],
               ind.pmatrix.male.noart.age3[1,1], ind.pmatrix.male.noart.age3[2,2], ind.pmatrix.male.noart.age3[3,3], ind.pmatrix.male.noart.age3[4,4], ind.pmatrix.male.noart.age3[5,5], ind.pmatrix.male.noart.age3[6,6], ind.pmatrix.male.noart.age3[7,7],
               ind.pmatrix.female.art.age1[1,1], ind.pmatrix.female.art.age1[2,2], ind.pmatrix.female.art.age1[3,3], ind.pmatrix.female.art.age1[4,4], ind.pmatrix.female.art.age1[5,5], ind.pmatrix.female.art.age1[6,6], ind.pmatrix.female.art.age1[7,7],
               ind.pmatrix.female.art.age2[1,1], ind.pmatrix.female.art.age2[2,2], ind.pmatrix.female.art.age2[3,3], ind.pmatrix.female.art.age2[4,4], ind.pmatrix.female.art.age2[5,5], ind.pmatrix.female.art.age2[6,6], ind.pmatrix.female.art.age2[7,7],
               ind.pmatrix.female.art.age3[1,1], ind.pmatrix.female.art.age3[2,2], ind.pmatrix.female.art.age3[3,3], ind.pmatrix.female.art.age3[4,4], ind.pmatrix.female.art.age3[5,5], ind.pmatrix.female.art.age3[6,6], ind.pmatrix.female.art.age3[7,7],
               ind.pmatrix.female.noart.age1[1,1], ind.pmatrix.female.noart.age1[2,2], ind.pmatrix.female.noart.age1[3,3], ind.pmatrix.female.noart.age1[4,4], ind.pmatrix.female.noart.age1[5,5], ind.pmatrix.female.noart.age1[6,6], ind.pmatrix.female.noart.age1[7,7],
               ind.pmatrix.female.noart.age2[1,1], ind.pmatrix.female.noart.age2[2,2], ind.pmatrix.female.noart.age2[3,3], ind.pmatrix.female.noart.age2[4,4], ind.pmatrix.female.noart.age2[5,5], ind.pmatrix.female.noart.age2[6,6], ind.pmatrix.female.noart.age2[7,7],
               ind.pmatrix.female.noart.age3[1,1], ind.pmatrix.female.noart.age3[2,2], ind.pmatrix.female.noart.age3[3,3], ind.pmatrix.female.noart.age3[4,4], ind.pmatrix.female.noart.age3[5,5], ind.pmatrix.female.noart.age3[6,6], ind.pmatrix.female.noart.age3[7,7])
  }
  if (state == 4) {
    logit <- c(ind.pmatrix.male.art.age1[1,8], ind.pmatrix.male.art.age1[2,8], ind.pmatrix.male.art.age1[3,8], ind.pmatrix.male.art.age1[4,8], ind.pmatrix.male.art.age1[5,8], ind.pmatrix.male.art.age1[6,8], ind.pmatrix.male.art.age1[7,8],
               ind.pmatrix.male.art.age2[1,8], ind.pmatrix.male.art.age2[2,8], ind.pmatrix.male.art.age2[3,8], ind.pmatrix.male.art.age2[4,8], ind.pmatrix.male.art.age2[5,8], ind.pmatrix.male.art.age2[6,8], ind.pmatrix.male.art.age2[7,8],
               ind.pmatrix.male.art.age3[1,8], ind.pmatrix.male.art.age3[2,8], ind.pmatrix.male.art.age3[3,8], ind.pmatrix.male.art.age3[4,8], ind.pmatrix.male.art.age3[5,8], ind.pmatrix.male.art.age3[6,8], ind.pmatrix.male.art.age3[7,8],
               ind.pmatrix.male.noart.age1[1,8], ind.pmatrix.male.noart.age1[2,8], ind.pmatrix.male.noart.age1[3,8], ind.pmatrix.male.noart.age1[4,8], ind.pmatrix.male.noart.age1[5,8], ind.pmatrix.male.noart.age1[6,8], ind.pmatrix.male.noart.age1[7,8],
               ind.pmatrix.male.noart.age2[1,8], ind.pmatrix.male.noart.age2[2,8], ind.pmatrix.male.noart.age2[3,8], ind.pmatrix.male.noart.age2[4,8], ind.pmatrix.male.noart.age2[5,8], ind.pmatrix.male.noart.age2[6,8], ind.pmatrix.male.noart.age2[7,8],
               ind.pmatrix.male.noart.age3[1,8], ind.pmatrix.male.noart.age3[2,8], ind.pmatrix.male.noart.age3[3,8], ind.pmatrix.male.noart.age3[4,8], ind.pmatrix.male.noart.age3[5,8], ind.pmatrix.male.noart.age3[6,8], ind.pmatrix.male.noart.age3[7,8],
               ind.pmatrix.female.art.age1[1,8], ind.pmatrix.female.art.age1[2,8], ind.pmatrix.female.art.age1[3,8], ind.pmatrix.female.art.age1[4,8], ind.pmatrix.female.art.age1[5,8], ind.pmatrix.female.art.age1[6,8], ind.pmatrix.female.art.age1[7,8],
               ind.pmatrix.female.art.age2[1,8], ind.pmatrix.female.art.age2[2,8], ind.pmatrix.female.art.age2[3,8], ind.pmatrix.female.art.age2[4,8], ind.pmatrix.female.art.age2[5,8], ind.pmatrix.female.art.age2[6,8], ind.pmatrix.female.art.age2[7,8],
               ind.pmatrix.female.art.age3[1,8], ind.pmatrix.female.art.age3[2,8], ind.pmatrix.female.art.age3[3,8], ind.pmatrix.female.art.age3[4,8], ind.pmatrix.female.art.age3[5,8], ind.pmatrix.female.art.age3[6,8], ind.pmatrix.female.art.age3[7,8],
               ind.pmatrix.female.noart.age1[1,8], ind.pmatrix.female.noart.age1[2,8], ind.pmatrix.female.noart.age1[3,8], ind.pmatrix.female.noart.age1[4,8], ind.pmatrix.female.noart.age1[5,8], ind.pmatrix.female.noart.age1[6,8], ind.pmatrix.female.noart.age1[7,8],
               ind.pmatrix.female.noart.age2[1,8], ind.pmatrix.female.noart.age2[2,8], ind.pmatrix.female.noart.age2[3,8], ind.pmatrix.female.noart.age2[4,8], ind.pmatrix.female.noart.age2[5,8], ind.pmatrix.female.noart.age2[6,8], ind.pmatrix.female.noart.age2[7,8],
               ind.pmatrix.female.noart.age3[1,8], ind.pmatrix.female.noart.age3[2,8], ind.pmatrix.female.noart.age3[3,8], ind.pmatrix.female.noart.age3[4,8], ind.pmatrix.female.noart.age3[5,8], ind.pmatrix.female.noart.age3[6,8], ind.pmatrix.female.noart.age3[7,8])
  }
  if (state == 5) {
    logit <- c(ind.pmatrix.male.art.age1[1,1], ind.pmatrix.male.art.age2[1,1], ind.pmatrix.male.art.age3[1,1], ind.pmatrix.male.noart.age1[1,1], ind.pmatrix.male.noart.age2[1,1], ind.pmatrix.male.noart.age3[1,1], ind.pmatrix.female.art.age1[1,1], ind.pmatrix.female.art.age2[1,1], ind.pmatrix.female.art.age3[1,1], ind.pmatrix.female.noart.age1[1,1], ind.pmatrix.female.noart.age2[1,1], ind.pmatrix.female.noart.age3[1,1])
  }
  if (state == 6) {
    logit <- c(ind.pmatrix.male.art.age1[2,2], ind.pmatrix.male.art.age1[3,3], ind.pmatrix.male.art.age1[4,4], ind.pmatrix.male.art.age1[5,5], ind.pmatrix.male.art.age1[6,6], ind.pmatrix.male.art.age1[7,7],
               ind.pmatrix.male.art.age2[2,2], ind.pmatrix.male.art.age2[3,3], ind.pmatrix.male.art.age2[4,4], ind.pmatrix.male.art.age2[5,5], ind.pmatrix.male.art.age2[6,6], ind.pmatrix.male.art.age2[7,7],
               ind.pmatrix.male.art.age3[2,2], ind.pmatrix.male.art.age3[3,3], ind.pmatrix.male.art.age3[4,4], ind.pmatrix.male.art.age3[5,5], ind.pmatrix.male.art.age3[6,6], ind.pmatrix.male.art.age3[7,7],
               ind.pmatrix.male.noart.age1[2,2], ind.pmatrix.male.noart.age1[3,3], ind.pmatrix.male.noart.age1[4,4], ind.pmatrix.male.noart.age1[5,5], ind.pmatrix.male.noart.age1[6,6], ind.pmatrix.male.noart.age1[7,7],
               ind.pmatrix.male.noart.age2[2,2], ind.pmatrix.male.noart.age2[3,3], ind.pmatrix.male.noart.age2[4,4], ind.pmatrix.male.noart.age2[5,5], ind.pmatrix.male.noart.age2[6,6], ind.pmatrix.male.noart.age2[7,7],
               ind.pmatrix.male.noart.age3[2,2], ind.pmatrix.male.noart.age3[3,3], ind.pmatrix.male.noart.age3[4,4], ind.pmatrix.male.noart.age3[5,5], ind.pmatrix.male.noart.age3[6,6], ind.pmatrix.male.noart.age3[7,7],
               ind.pmatrix.female.art.age1[2,2], ind.pmatrix.female.art.age1[3,3], ind.pmatrix.female.art.age1[4,4], ind.pmatrix.female.art.age1[5,5], ind.pmatrix.female.art.age1[6,6], ind.pmatrix.female.art.age1[7,7],
               ind.pmatrix.female.art.age2[2,2], ind.pmatrix.female.art.age2[3,3], ind.pmatrix.female.art.age2[4,4], ind.pmatrix.female.art.age2[5,5], ind.pmatrix.female.art.age2[6,6], ind.pmatrix.female.art.age2[7,7],
               ind.pmatrix.female.art.age3[2,2], ind.pmatrix.female.art.age3[3,3], ind.pmatrix.female.art.age3[4,4], ind.pmatrix.female.art.age3[5,5], ind.pmatrix.female.art.age3[6,6], ind.pmatrix.female.art.age3[7,7],
               ind.pmatrix.female.noart.age1[2,2], ind.pmatrix.female.noart.age1[3,3], ind.pmatrix.female.noart.age1[4,4], ind.pmatrix.female.noart.age1[5,5], ind.pmatrix.female.noart.age1[6,6], ind.pmatrix.female.noart.age1[7,7],
               ind.pmatrix.female.noart.age2[2,2], ind.pmatrix.female.noart.age2[3,3], ind.pmatrix.female.noart.age2[4,4], ind.pmatrix.female.noart.age2[5,5], ind.pmatrix.female.noart.age2[6,6], ind.pmatrix.female.noart.age2[7,7],
               ind.pmatrix.female.noart.age3[2,2], ind.pmatrix.female.noart.age3[3,3], ind.pmatrix.female.noart.age3[4,4], ind.pmatrix.female.noart.age3[5,5], ind.pmatrix.female.noart.age3[6,6], ind.pmatrix.female.noart.age3[7,7])
  }
  return(log(logit/(1 - logit)))
}

##### Calculate combined fit logit for better/worse/stay/death #####
# better = 1, worse = 2, stay = 3, death = 4, stay.p11 = 5, stay.else = 6
comb.logit.calculate <- function(state) {
  if (state == 1) {
    logit <- c(comb.pmatrix.male.art.age1[2,1], comb.pmatrix.male.art.age1[3,2], comb.pmatrix.male.art.age1[4,3],   comb.pmatrix.male.art.age1[5,4], comb.pmatrix.male.art.age1[6,5], comb.pmatrix.male.art.age1[7,6],
               comb.pmatrix.male.art.age2[2,1], comb.pmatrix.male.art.age2[3,2], comb.pmatrix.male.art.age2[4,3],   comb.pmatrix.male.art.age2[5,4], comb.pmatrix.male.art.age2[6,5], comb.pmatrix.male.art.age2[7,6],
               comb.pmatrix.male.art.age3[2,1], comb.pmatrix.male.art.age3[3,2], comb.pmatrix.male.art.age3[4,3],   comb.pmatrix.male.art.age3[5,4], comb.pmatrix.male.art.age3[6,5], comb.pmatrix.male.art.age3[7,6],
               comb.pmatrix.male.noart.age1[2,1], comb.pmatrix.male.noart.age1[3,2], comb.pmatrix.male.noart.age1[4,3],   comb.pmatrix.male.noart.age1[5,4], comb.pmatrix.male.noart.age1[6,5], comb.pmatrix.male.noart.age1[7,6],
               comb.pmatrix.male.noart.age2[2,1], comb.pmatrix.male.noart.age2[3,2], comb.pmatrix.male.noart.age2[4,3],   comb.pmatrix.male.noart.age2[5,4], comb.pmatrix.male.noart.age2[6,5], comb.pmatrix.male.noart.age2[7,6],
               comb.pmatrix.male.noart.age3[2,1], comb.pmatrix.male.noart.age3[3,2], comb.pmatrix.male.noart.age3[4,3],   comb.pmatrix.male.noart.age3[5,4], comb.pmatrix.male.noart.age3[6,5], comb.pmatrix.male.noart.age3[7,6],
               comb.pmatrix.female.art.age1[2,1], comb.pmatrix.female.art.age1[3,2], comb.pmatrix.female.art.age1[4,3],   comb.pmatrix.female.art.age1[5,4], comb.pmatrix.female.art.age1[6,5], comb.pmatrix.female.art.age1[7,6],
               comb.pmatrix.female.art.age2[2,1], comb.pmatrix.female.art.age2[3,2], comb.pmatrix.female.art.age2[4,3],   comb.pmatrix.female.art.age2[5,4], comb.pmatrix.female.art.age2[6,5], comb.pmatrix.female.art.age2[7,6],
               comb.pmatrix.female.art.age3[2,1], comb.pmatrix.female.art.age3[3,2], comb.pmatrix.female.art.age3[4,3],   comb.pmatrix.female.art.age3[5,4], comb.pmatrix.female.art.age3[6,5], comb.pmatrix.female.art.age3[7,6],
               comb.pmatrix.female.noart.age1[2,1], comb.pmatrix.female.noart.age1[3,2], comb.pmatrix.female.noart.age1[4,3],   comb.pmatrix.female.noart.age1[5,4], comb.pmatrix.female.noart.age1[6,5], comb.pmatrix.female.noart.age1[7,6],
               comb.pmatrix.female.noart.age2[2,1], comb.pmatrix.female.noart.age2[3,2], comb.pmatrix.female.noart.age2[4,3],   comb.pmatrix.female.noart.age2[5,4], comb.pmatrix.female.noart.age2[6,5], comb.pmatrix.female.noart.age2[7,6],
               comb.pmatrix.female.noart.age3[2,1], comb.pmatrix.female.noart.age3[3,2], comb.pmatrix.female.noart.age3[4,3],   comb.pmatrix.female.noart.age3[5,4], comb.pmatrix.female.noart.age3[6,5], comb.pmatrix.female.noart.age3[7,6])
  }
  if (state == 2) {
    logit <- c(comb.pmatrix.male.art.age1[1,2], comb.pmatrix.male.art.age1[2,3], comb.pmatrix.male.art.age1[3,4], comb.pmatrix.male.art.age1[4,5], comb.pmatrix.male.art.age1[5,6], comb.pmatrix.male.art.age1[6,7],
               comb.pmatrix.male.art.age2[1,2], comb.pmatrix.male.art.age2[2,3], comb.pmatrix.male.art.age2[3,4], comb.pmatrix.male.art.age2[4,5], comb.pmatrix.male.art.age2[5,6], comb.pmatrix.male.art.age2[6,7],
               comb.pmatrix.male.art.age3[1,2], comb.pmatrix.male.art.age3[2,3], comb.pmatrix.male.art.age3[3,4], comb.pmatrix.male.art.age3[4,5], comb.pmatrix.male.art.age3[5,6], comb.pmatrix.male.art.age3[6,7],
               comb.pmatrix.male.noart.age1[1,2], comb.pmatrix.male.noart.age1[2,3], comb.pmatrix.male.noart.age1[3,4], comb.pmatrix.male.noart.age1[4,5], comb.pmatrix.male.noart.age1[5,6], comb.pmatrix.male.noart.age1[6,7],
               comb.pmatrix.male.noart.age2[1,2], comb.pmatrix.male.noart.age2[2,3], comb.pmatrix.male.noart.age2[3,4], comb.pmatrix.male.noart.age2[4,5], comb.pmatrix.male.noart.age2[5,6], comb.pmatrix.male.noart.age2[6,7],
               comb.pmatrix.male.noart.age3[1,2], comb.pmatrix.male.noart.age3[2,3], comb.pmatrix.male.noart.age3[3,4], comb.pmatrix.male.noart.age3[4,5], comb.pmatrix.male.noart.age3[5,6], comb.pmatrix.male.noart.age3[6,7],
               comb.pmatrix.female.art.age1[1,2], comb.pmatrix.female.art.age1[2,3], comb.pmatrix.female.art.age1[3,4], comb.pmatrix.female.art.age1[4,5], comb.pmatrix.female.art.age1[5,6], comb.pmatrix.female.art.age1[6,7],
               comb.pmatrix.female.art.age2[1,2], comb.pmatrix.female.art.age2[2,3], comb.pmatrix.female.art.age2[3,4], comb.pmatrix.female.art.age2[4,5], comb.pmatrix.female.art.age2[5,6], comb.pmatrix.female.art.age2[6,7],
               comb.pmatrix.female.art.age3[1,2], comb.pmatrix.female.art.age3[2,3], comb.pmatrix.female.art.age3[3,4], comb.pmatrix.female.art.age3[4,5], comb.pmatrix.female.art.age3[5,6], comb.pmatrix.female.art.age3[6,7],
               comb.pmatrix.female.noart.age1[1,2], comb.pmatrix.female.noart.age1[2,3], comb.pmatrix.female.noart.age1[3,4], comb.pmatrix.female.noart.age1[4,5], comb.pmatrix.female.noart.age1[5,6], comb.pmatrix.female.noart.age1[6,7],
               comb.pmatrix.female.noart.age2[1,2], comb.pmatrix.female.noart.age2[2,3], comb.pmatrix.female.noart.age2[3,4], comb.pmatrix.female.noart.age2[4,5], comb.pmatrix.female.noart.age2[5,6], comb.pmatrix.female.noart.age2[6,7],
               comb.pmatrix.female.noart.age3[1,2], comb.pmatrix.female.noart.age3[2,3], comb.pmatrix.female.noart.age3[3,4], comb.pmatrix.female.noart.age3[4,5], comb.pmatrix.female.noart.age3[5,6], comb.pmatrix.female.noart.age3[6,7])
  }
  if (state == 3) {
    logit <- c(comb.pmatrix.male.art.age1[1,1], comb.pmatrix.male.art.age1[2,2], comb.pmatrix.male.art.age1[3,3], comb.pmatrix.male.art.age1[4,4], comb.pmatrix.male.art.age1[5,5], comb.pmatrix.male.art.age1[6,6], comb.pmatrix.male.art.age1[7,7],
               comb.pmatrix.male.art.age2[1,1], comb.pmatrix.male.art.age2[2,2], comb.pmatrix.male.art.age2[3,3], comb.pmatrix.male.art.age2[4,4], comb.pmatrix.male.art.age2[5,5], comb.pmatrix.male.art.age2[6,6], comb.pmatrix.male.art.age2[7,7],
               comb.pmatrix.male.art.age3[1,1], comb.pmatrix.male.art.age3[2,2], comb.pmatrix.male.art.age3[3,3], comb.pmatrix.male.art.age3[4,4], comb.pmatrix.male.art.age3[5,5], comb.pmatrix.male.art.age3[6,6], comb.pmatrix.male.art.age3[7,7],
               comb.pmatrix.male.noart.age1[1,1], comb.pmatrix.male.noart.age1[2,2], comb.pmatrix.male.noart.age1[3,3], comb.pmatrix.male.noart.age1[4,4], comb.pmatrix.male.noart.age1[5,5], comb.pmatrix.male.noart.age1[6,6], comb.pmatrix.male.noart.age1[7,7],
               comb.pmatrix.male.noart.age2[1,1], comb.pmatrix.male.noart.age2[2,2], comb.pmatrix.male.noart.age2[3,3], comb.pmatrix.male.noart.age2[4,4], comb.pmatrix.male.noart.age2[5,5], comb.pmatrix.male.noart.age2[6,6], comb.pmatrix.male.noart.age2[7,7],
               comb.pmatrix.male.noart.age3[1,1], comb.pmatrix.male.noart.age3[2,2], comb.pmatrix.male.noart.age3[3,3], comb.pmatrix.male.noart.age3[4,4], comb.pmatrix.male.noart.age3[5,5], comb.pmatrix.male.noart.age3[6,6], comb.pmatrix.male.noart.age3[7,7],
               comb.pmatrix.female.art.age1[1,1], comb.pmatrix.female.art.age1[2,2], comb.pmatrix.female.art.age1[3,3], comb.pmatrix.female.art.age1[4,4], comb.pmatrix.female.art.age1[5,5], comb.pmatrix.female.art.age1[6,6], comb.pmatrix.female.art.age1[7,7],
               comb.pmatrix.female.art.age2[1,1], comb.pmatrix.female.art.age2[2,2], comb.pmatrix.female.art.age2[3,3], comb.pmatrix.female.art.age2[4,4], comb.pmatrix.female.art.age2[5,5], comb.pmatrix.female.art.age2[6,6], comb.pmatrix.female.art.age2[7,7],
               comb.pmatrix.female.art.age3[1,1], comb.pmatrix.female.art.age3[2,2], comb.pmatrix.female.art.age3[3,3], comb.pmatrix.female.art.age3[4,4], comb.pmatrix.female.art.age3[5,5], comb.pmatrix.female.art.age3[6,6], comb.pmatrix.female.art.age3[7,7],
               comb.pmatrix.female.noart.age1[1,1], comb.pmatrix.female.noart.age1[2,2], comb.pmatrix.female.noart.age1[3,3], comb.pmatrix.female.noart.age1[4,4], comb.pmatrix.female.noart.age1[5,5], comb.pmatrix.female.noart.age1[6,6], comb.pmatrix.female.noart.age1[7,7],
               comb.pmatrix.female.noart.age2[1,1], comb.pmatrix.female.noart.age2[2,2], comb.pmatrix.female.noart.age2[3,3], comb.pmatrix.female.noart.age2[4,4], comb.pmatrix.female.noart.age2[5,5], comb.pmatrix.female.noart.age2[6,6], comb.pmatrix.female.noart.age2[7,7],
               comb.pmatrix.female.noart.age3[1,1], comb.pmatrix.female.noart.age3[2,2], comb.pmatrix.female.noart.age3[3,3], comb.pmatrix.female.noart.age3[4,4], comb.pmatrix.female.noart.age3[5,5], comb.pmatrix.female.noart.age3[6,6], comb.pmatrix.female.noart.age3[7,7])
  }
  if (state == 4) {
    logit <- c(comb.pmatrix.male.art.age1[1,8], comb.pmatrix.male.art.age1[2,8], comb.pmatrix.male.art.age1[3,8], comb.pmatrix.male.art.age1[4,8], comb.pmatrix.male.art.age1[5,8], comb.pmatrix.male.art.age1[6,8], comb.pmatrix.male.art.age1[7,8],
               comb.pmatrix.male.art.age2[1,8], comb.pmatrix.male.art.age2[2,8], comb.pmatrix.male.art.age2[3,8], comb.pmatrix.male.art.age2[4,8], comb.pmatrix.male.art.age2[5,8], comb.pmatrix.male.art.age2[6,8], comb.pmatrix.male.art.age2[7,8],
               comb.pmatrix.male.art.age3[1,8], comb.pmatrix.male.art.age3[2,8], comb.pmatrix.male.art.age3[3,8], comb.pmatrix.male.art.age3[4,8], comb.pmatrix.male.art.age3[5,8], comb.pmatrix.male.art.age3[6,8], comb.pmatrix.male.art.age3[7,8],
               comb.pmatrix.male.noart.age1[1,8], comb.pmatrix.male.noart.age1[2,8], comb.pmatrix.male.noart.age1[3,8], comb.pmatrix.male.noart.age1[4,8], comb.pmatrix.male.noart.age1[5,8], comb.pmatrix.male.noart.age1[6,8], comb.pmatrix.male.noart.age1[7,8],
               comb.pmatrix.male.noart.age2[1,8], comb.pmatrix.male.noart.age2[2,8], comb.pmatrix.male.noart.age2[3,8], comb.pmatrix.male.noart.age2[4,8], comb.pmatrix.male.noart.age2[5,8], comb.pmatrix.male.noart.age2[6,8], comb.pmatrix.male.noart.age2[7,8],
               comb.pmatrix.male.noart.age3[1,8], comb.pmatrix.male.noart.age3[2,8], comb.pmatrix.male.noart.age3[3,8], comb.pmatrix.male.noart.age3[4,8], comb.pmatrix.male.noart.age3[5,8], comb.pmatrix.male.noart.age3[6,8], comb.pmatrix.male.noart.age3[7,8],
               comb.pmatrix.female.art.age1[1,8], comb.pmatrix.female.art.age1[2,8], comb.pmatrix.female.art.age1[3,8], comb.pmatrix.female.art.age1[4,8], comb.pmatrix.female.art.age1[5,8], comb.pmatrix.female.art.age1[6,8], comb.pmatrix.female.art.age1[7,8],
               comb.pmatrix.female.art.age2[1,8], comb.pmatrix.female.art.age2[2,8], comb.pmatrix.female.art.age2[3,8], comb.pmatrix.female.art.age2[4,8], comb.pmatrix.female.art.age2[5,8], comb.pmatrix.female.art.age2[6,8], comb.pmatrix.female.art.age2[7,8],
               comb.pmatrix.female.art.age3[1,8], comb.pmatrix.female.art.age3[2,8], comb.pmatrix.female.art.age3[3,8], comb.pmatrix.female.art.age3[4,8], comb.pmatrix.female.art.age3[5,8], comb.pmatrix.female.art.age3[6,8], comb.pmatrix.female.art.age3[7,8],
               comb.pmatrix.female.noart.age1[1,8], comb.pmatrix.female.noart.age1[2,8], comb.pmatrix.female.noart.age1[3,8], comb.pmatrix.female.noart.age1[4,8], comb.pmatrix.female.noart.age1[5,8], comb.pmatrix.female.noart.age1[6,8], comb.pmatrix.female.noart.age1[7,8],
               comb.pmatrix.female.noart.age2[1,8], comb.pmatrix.female.noart.age2[2,8], comb.pmatrix.female.noart.age2[3,8], comb.pmatrix.female.noart.age2[4,8], comb.pmatrix.female.noart.age2[5,8], comb.pmatrix.female.noart.age2[6,8], comb.pmatrix.female.noart.age2[7,8],
               comb.pmatrix.female.noart.age3[1,8], comb.pmatrix.female.noart.age3[2,8], comb.pmatrix.female.noart.age3[3,8], comb.pmatrix.female.noart.age3[4,8], comb.pmatrix.female.noart.age3[5,8], comb.pmatrix.female.noart.age3[6,8], comb.pmatrix.female.noart.age3[7,8])
  }
  if (state == 5) {
    logit <- c(comb.pmatrix.male.art.age1[1,1], comb.pmatrix.male.art.age2[1,1], comb.pmatrix.male.art.age3[1,1], comb.pmatrix.male.noart.age1[1,1], comb.pmatrix.male.noart.age2[1,1], comb.pmatrix.male.noart.age3[1,1], comb.pmatrix.female.art.age1[1,1], comb.pmatrix.female.art.age2[1,1], comb.pmatrix.female.art.age3[1,1], comb.pmatrix.female.noart.age1[1,1], comb.pmatrix.female.noart.age2[1,1], comb.pmatrix.female.noart.age3[1,1])
  }
  if (state == 6) {
    logit <- c(comb.pmatrix.male.art.age1[2,2], comb.pmatrix.male.art.age1[3,3], comb.pmatrix.male.art.age1[4,4], comb.pmatrix.male.art.age1[5,5], comb.pmatrix.male.art.age1[6,6], comb.pmatrix.male.art.age1[7,7],
      comb.pmatrix.male.art.age2[2,2], comb.pmatrix.male.art.age2[3,3], comb.pmatrix.male.art.age2[4,4], comb.pmatrix.male.art.age2[5,5], comb.pmatrix.male.art.age2[6,6], comb.pmatrix.male.art.age2[7,7],
      comb.pmatrix.male.art.age3[2,2], comb.pmatrix.male.art.age3[3,3], comb.pmatrix.male.art.age3[4,4], comb.pmatrix.male.art.age3[5,5], comb.pmatrix.male.art.age3[6,6], comb.pmatrix.male.art.age3[7,7],
      comb.pmatrix.male.noart.age1[2,2], comb.pmatrix.male.noart.age1[3,3], comb.pmatrix.male.noart.age1[4,4], comb.pmatrix.male.noart.age1[5,5], comb.pmatrix.male.noart.age1[6,6], comb.pmatrix.male.noart.age1[7,7],
      comb.pmatrix.male.noart.age2[2,2], comb.pmatrix.male.noart.age2[3,3], comb.pmatrix.male.noart.age2[4,4], comb.pmatrix.male.noart.age2[5,5], comb.pmatrix.male.noart.age2[6,6], comb.pmatrix.male.noart.age2[7,7],
      comb.pmatrix.male.noart.age3[2,2], comb.pmatrix.male.noart.age3[3,3], comb.pmatrix.male.noart.age3[4,4], comb.pmatrix.male.noart.age3[5,5], comb.pmatrix.male.noart.age3[6,6], comb.pmatrix.male.noart.age3[7,7],
      comb.pmatrix.female.art.age1[2,2], comb.pmatrix.female.art.age1[3,3], comb.pmatrix.female.art.age1[4,4], comb.pmatrix.female.art.age1[5,5], comb.pmatrix.female.art.age1[6,6], comb.pmatrix.female.art.age1[7,7],
      comb.pmatrix.female.art.age2[2,2], comb.pmatrix.female.art.age2[3,3], comb.pmatrix.female.art.age2[4,4], comb.pmatrix.female.art.age2[5,5], comb.pmatrix.female.art.age2[6,6], comb.pmatrix.female.art.age2[7,7],
      comb.pmatrix.female.art.age3[2,2], comb.pmatrix.female.art.age3[3,3], comb.pmatrix.female.art.age3[4,4], comb.pmatrix.female.art.age3[5,5], comb.pmatrix.female.art.age3[6,6], comb.pmatrix.female.art.age3[7,7],
      comb.pmatrix.female.noart.age1[2,2], comb.pmatrix.female.noart.age1[3,3], comb.pmatrix.female.noart.age1[4,4], comb.pmatrix.female.noart.age1[5,5], comb.pmatrix.female.noart.age1[6,6], comb.pmatrix.female.noart.age1[7,7],
      comb.pmatrix.female.noart.age2[2,2], comb.pmatrix.female.noart.age2[3,3], comb.pmatrix.female.noart.age2[4,4], comb.pmatrix.female.noart.age2[5,5], comb.pmatrix.female.noart.age2[6,6], comb.pmatrix.female.noart.age2[7,7],
      comb.pmatrix.female.noart.age3[2,2], comb.pmatrix.female.noart.age3[3,3], comb.pmatrix.female.noart.age3[4,4], comb.pmatrix.female.noart.age3[5,5], comb.pmatrix.female.noart.age3[6,6], comb.pmatrix.female.noart.age3[7,7])
  }
  return(log(logit/(1 - logit)))
}




















