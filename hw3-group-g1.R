# Homework 3 by Sebastian Höffner and Andrea Suckro

data = read.table("refelxive1.txt", heade=T)
data.match = subset(data, cond == "match")
data.mismatch = subset(data, cond == "mismatch")

se <- function(x) sd(x)/sqrt(length(x))

##########################################################################################
# HYPOTHESIS TESTING!

#1) Hypothesis_null: mean of match condition comes from the same distribution as the mean of the non_match condition
#2) Hypothesis_alt: mean of the match condition comes from another distribution
match_mean = mean(data.match$TFT)
mismatch_mean = mean(data.mismatch$TFT)
#3) Deciding on the alpha-level 0,05 since we only care about the difference and not a certain direction
alpha = 0.05

#4) obtain the p-value
standError = sqrt((se(data.match$TFT))^2 + (se(data.mismatch$TFT))^2)

t = (match_mean-mismatch_mean)/standError
print(t)
p_val = pt(t,df=min(length(data.match$TFT), length(data.mismatch$TFT))-1)
print(p_val)
#5) decide upon the alpha level if the 0 hypothesis is obsolete
if(p_val < alpha){
  print('rejecting the 0 hypothesis - sample is from another population!')
}else{
  print('can not reject the 0 hypothesis')
  print('not enough evidence to conclude anything')
}
