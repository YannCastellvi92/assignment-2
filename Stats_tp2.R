
my_mean <- function(x) {
  result <- NULL
  
  result <- sum_divided_by(x, length(x))
  
  return(result)
}

my_sum <- function(x) {
  result <- NULL
  
  result <- 0
  for (n in x) {
    result <- result + n
  }
  
  return(result)
}
sum_divided_by <- function(x, k) {
  result <- NULL
  
  result <- my_sum(x)/k
  
  return(result)
}

difference_in_means <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- my_mean(d_1[[var]]) - my_mean(d_2[[var]])
  return(result)
}
randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]])
  return(d)
}

permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    d_perm <- randomize(d, var)
    permutation_statistics[i] <- statistic(d_perm, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}
