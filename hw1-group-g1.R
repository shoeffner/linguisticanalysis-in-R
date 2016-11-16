zScore <- function(x, mu=0, sigma=1) {
  # Compute the z-score of x.
  #
  # Args:
  #   x: the value x to find the z-score for
  #   mu: the mean of the normal distribution
  #   sigma: the standard deviation of the normal distribution
  #
  # Returns:
  #   The z-score of x.
  (x - mu) / sigma
}

quant <- function(zScore, mu=0, sigma=1) {
  # Computes the probability for a value to be more extreme than values with
  # the given given z-score.
  #
  # "Extreme" means that if the z-score is closer to the left tail,
  # i.e. zScore < 0, the probability of the values left of the z-score is
  # calculated, while if the probability is closer to the right tail
  # (zScore > 0), the probability of the values right of the z-score is
  # calculated.
  #
  # Args:
  #   zScore: the z-score boundary
  #   mu: the mean of the normal distribution
  #   sigma: the standard deviation of the normal distribution
  #
  # Returns:
  #   The propability for values being more extreme than the value
  #   with the given z-score in percent.
  pnorm(zScore, mu, sigma, zScore < 0) * 100
}
