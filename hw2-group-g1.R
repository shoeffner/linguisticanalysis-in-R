# Homework 2 by Sebastian Höffner and Andrea Suckro
get.statistics <- function(population) {
  sample.size = 100 # size of single sample
  nsamp = 1000      # no. of samples

  prob1 = 0
  prob2 = 0
  prob3 = 0

  #----- code for calculating prob1, prob2 and prob3 -----#
  real_std = sd(population)
  real_mean = mean(population)
  real_se = real_std/sqrt(sample.size)

  hit = 0
  for (i in 1:nsamp) {
    samp = sample(population, sample.size)
    sample_mean = mean(samp)
    sample_std = sd(samp)
    sample_se = sample_std/sqrt(sample.size)

    if(real_mean > sample_mean - 2*sample_se & real_mean < sample_mean + 2*sample_se){
      hit = hit +1
    }
  }
  prob1 = hit/nsamp

  hit = 0
  for (i in 1:nsamp) {
    samp = sample(population, sample.size)
    samp_mean = mean(samp)
    # using now the population standard error instead of the standard deviation of the
    # distribution of means according to CLT
    if(real_mean < samp_mean + 2*real_se & real_mean > samp_mean - 2*real_se){
      hit = hit + 1
    }
  }

  prob2 = hit/nsamp

  hit = 0
  for (i in 1:nsamp) {
    samp = sample(population, sample.size)
    samp_mean = mean(samp)
    samp_sd = sd(samp)
    # using the same trick again
    if(samp_mean < real_mean + 2*real_se & samp_mean > real_mean - 2*real_se){
      hit = hit + 1
    }
  }

  prob3 = hit/nsamp

  #-------------------------------------------------------#
  print(paste("1. ", prob1))
  print(paste("2. ", prob2))
  print(paste("3. ", prob3))
}
