{\rtf1\ansi\ansicpg1252\cocoartf1561\cocoasubrtf200
{\fonttbl\f0\fnil\fcharset0 Menlo-Regular;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\ri0\partightenfactor0

\f0\fs24 \cf0 LargeData.R includes all the executive code.\
Functions.R includes all the functions that are called in LargeData.R.\
\
The following are the usage of functions:\
1. separate.art\
This function is used to separate the data into ART treatment group and no ART treatment group after separated by gender.\
Input:\
ID.art\
ID.state \
ID.CD4count \
ID.periodm \
ID.age\
ID.agem \
ID.ARTAge\
ID.ARTdate\
\
Return: \
ID.state.art\
ID.state.noart\
ID.CD4count.art\
ID.CD4count.noart\
ID.periodm.art\
ID.periodm.noart\
ID.age.art\
ID.age.noart\
ID.agem.art\
ID.agem.noart\
ID.ARTAge.art\
ID.ARTAge.noart\
\
\
2. separate.age\
This function is used to separate age category groups, given gender and ART treatment separated.\
Input:\
ID.state \
ID.CD4count \
ID.periodm \
ID.age\
ID.agem\
\
Return:\
ID.state.age1\
ID.state.age2\
ID.state.age3\
ID.CD4count.age1\
ID.CD4count.age2\
ID.CD4count.age3\
ID.periodm.age1\
ID.periodm.age2\
ID.periodm.age3\
\
3. position.calc\
This function is used to calculate the position vector.\
Input:\
ID.state\
ID.periodm\
\
Return:\
row.num: the max time period that happened between two consecutive states.\
ID.pos: the position for the powered transition matrix vector.\
\
4. negloglikelihood (can use for all data)\
This function is used to calculate the negative log-likelihood of the transition probability matrix. We can use optim function to estimate the alpha vector. \
Input:\
alpha: the alpha vector\
ID.gap: the gap vector\
ID.I: the indicator matrix\
\
Return: \
The value of negative log-likelihood given CD4 states and period (in month).\
\
5. negloglikelihood.monthGap (may use for month separation data)\
This function is used to calculate the negative log-likelihood of the transition probability matrix. We can use optim function to estimate the alpha vector. \
Input:\
alpha: the alpha vector\
ID.gap: the gap vector\
ID.I: the indicator matrix\
gap.max: the gap that was used at the beginning to separate the data set. \
\
Return: \
The value of negative log-likelihood given CD4 states and period (in month).\
\
\
6. returnMatrix\
This function takes the alpha vector and return the corresponding transition probability matrix. \
\
Input: \
alpha: the alpha vector\
\
Return:\
7 by 8 matrix\
\
7. CIcalculate\
This function is used to calculate the 95% confidence intervals of thetas in the transition probability matrix. \
Input:\
alpha: the alpha vector\
cov: the covariance matrix of alpha\
\
Return:\
26 by 3 matrix\
Each row represents a theta in the transition probability matrix in the order of p11, p12, p18, p21, \'85, p78\
Column represents lower bound, mean, and upper bound, respectively. \
\
8. LatexCI\
This function is used to transfer the confidence interval into the comparison of theta in a scale that can be entered into Latex. \
Input:\
picnum: range from 1 to 26. Represents p11, p12, p18, p21, \'85, p78, respectively.\
death: TRUE or FALSE, if the transition probability that want to calculate with is death state.  CI.male.art.age1, CI.male.art.age2, CI.male.art.age3, CI.female.art.age1, CI.female.art.age2, CI.female.art.age3,CI.male.noart.age1, CI.male.noart.age2, CI.male.noart.age3, CI.female.noart.age1, CI.female.noart.age2, CI.female.noart.age3: the confidence intervals for different groups.\
\
Return: \
13 by 3 matrix\
The first row represents the minimum of the lower bounds, mean (NA), the maximum of the upper bounds.\
For the rest 12 rows, each row represents a group in the order of male.art.age1, male.art.age2, male.art.age3, female.art.age1, female.art.age2, female.art.age3, male.noart.age1, male.noart.age2, male.noart.age3, female.noart.age1, female.noart.age2, and female.noart.age3, \
\
9. likelihood.estimation.monthGap (may use for month separation data)\
This function is used to calculate the negative log-likelihood in the form of combined fit. \
\
Input: \
parameter: a vector of length 95, 1 to 19 represents beta0, 20 to 38 represents beta1, 39 to 57 represents beta2, 58 to 76 represents beta3, and 77 to 95 represents beta4.\
gap.max: the gap that was used at the beginning to separate the data set. \
gap.max: the gap that was used at the beginning to separate the data set. \
\
Return: \
The value of negative log-likelihood given CD4 states and period (in month).\
\
10. likelihood.estimation\
This function is used to calculate the negative log-likelihood in the form of combined fit. \
\
Input: \
parameter: a vector of length 95, 1 to 19 represents beta0, 20 to 38 represents beta1, 39 to 57 represents beta2, 58 to 76 represents beta3, and 77 to 95 represents beta4.\
gap.max: the gap that was used at the beginning to separate the data set. \
\
Return: \
The value of negative log-likelihood given CD4 states and period (in month).\
\
11. ARTEffectTable\
This function is used to calculate the ART Effect Table.\
\
Input:\
None\
\
Return:\
ART Effect Table\
\
12. ind.logit.calculate\
This function is used to calculate individual fit logit for different transition properties\
\
Input:\
state: represent the transition property\
better = 1, worse = 2, stay = 3, death = 4, stay.p11 = 5, stay.else = 6\
\
13. comb.logit.calculate\
This function is used to calculate combined fit logit for different transition properties\
\
Input:\
state: represent the transition property\
better = 1, worse = 2, stay = 3, death = 4, stay.p11 = 5, stay.else = 6\
\
}