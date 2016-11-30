# Homework 2 by Sebastian Höffner and Andrea Suckro

in_ci <- function(val, mu, ci) {
  # Determines if a value lies within a given confidence interval ci around mu.
  # It evaluates whether val is bigger than mu - ci and smaller than mu + ci.
  #
  # Args:
  #   val: The value to check.
  #   mu:  The mean of the distribution.
  #   ci:  The radius around the mean.
  #
  # Returns:
  #   True if val lies within the radius ci around mu.

  val > mu - ci & val < mu + ci
}

get.statistics <- function(population) {
  # Simulates the process of repeated sampling from the given population
  # to answer the following questions:
  #
  # 1. What is the probability that the population mean lies within the
  #    interval of two standard errors around the sample mean?
  #    Known: real mean. Unknown: real standard deviation
  #
  # 2. What is the probability that the population mean lies within the
  #    interval of two standard deviations of the sampling distribution of the
  #    sample mean around the sample mean?
  #    Known: real mean, real standard deviation.
  #    After central limit theorem the standard deviation of the sampling
  #    distribution of the sample mean is equal to the standard error of the
  #    population, thus we can use the real standard error.
  #
  # 3. What is the probability that the mean of a sample lies within the
  #    interval of two standard deviations of the sampling distribution
  #    of the sample mean around the population mean?
  #    Known: real mean, real standard deviation.
  #    Similar to 2., but we exchange the means, i.e. we change the perspective.
  #
  # There will be 1000 samples of size 100 evaluated. For each question
  # the same sample are used to keep the results comparable.
  #
  # This method prints out the answers.

  sample.size = 100 # size of single sample
  nsamp = 1000      # no. of samples

  prob1 = 0
  prob2 = 0
  prob3 = 0

  #----- code for calculating prob1, prob2 and prob3 -----#

  real_mean <- mean(population)
  real_se <- sd(population) / sqrt(sample.size)

  hits <- rep(0, 3)
  for (i in 1:nsamp) {
    samp <- sample(population, sample.size)

    sample_mean <- mean(samp)
    sample_se <- sd(samp) / sqrt(sample.size)

    hits <- hits + c(in_ci(real_mean, sample_mean, 2 * sample_se),
                     in_ci(real_mean, sample_mean, 2 * real_se),
                     in_ci(sample_mean, real_mean, 2 * real_se))
  }

  prob1 <- hits[1] / nsamp
  prob2 <- hits[2] / nsamp
  prob3 <- hits[3] / nsamp

  #-------------------------------------------------------#
  print(paste("1. ", prob1))
  print(paste("2. ", prob2))
  print(paste("3. ", prob3))
}
