Multiarm_Bandit <- setRefClass("Multiarm_Bandit",
    fields = list(num_arms = "integer",  # number of arms 
                  num_iter = "integer",  # number of desired iterations
                  epsilon = "numeric",   # p(exploring another arm)
                  Q_Star = "vector",     # true expected means
                  Q_Known = "vector",    # known means
                  max_index = "integer", # maximum of known means
                  obtained_reward = "vector", # running track of rewards
                  mean_reward = "vector",    # running track of mean rewards
                  times_explored = "vector")  # how many times arm k was explored
)

Multiarm_Bandit$methods(
    initialize = function(true_means, epsilon=0.0, num_iter=1000L) {
       Q_Star <<- true_means
       Q_Known <<- rnorm(true_means)
       num_arms <<- length(.self$Q_Star)
       num_iter <<- num_iter
       epsilon <<- epsilon       
       max_index <<- which(.self$Q_Known == max(.self$Q_Known))
       times_explored <<- rep(0, .self$num_arms)
       obtained_reward <<- max(.self$Q_Known)
       mean_reward <<- max(.self$Q_Known)
    })

Multiarm_Bandit$methods(
    update_max = function() {
       max_index <<- which(.self$Q_Known == max(.self$Q_Known))
    }
)

Multiarm_Bandit$methods(
    # Calculates mean incrementally. Given n observations,
    # and then now observing the (n+1)th observation,
    # calculate the mean of all (n+1) observations.
    calculate_mean = function(new_point, current_mean, n) {
       current_mean + (new_point - current_mean)/(n+1)
    }
)

Multiarm_Bandit$methods(
    # Reinforcement learning algorithm.
    learn = function () {      
      for (i in 1:.self$num_iter) {

        # With probability epsilon, explore another arm...
        if (rbinom(1,1,epsilon) == 1) {

          # Choose an arm to explore, with equal probability
          arm <- sample(1:.self$num_arms, 1, replace = TRUE)
        } else {  # If don't explore, just stay greedy.
          arm <- .self$max_index
        }
      
      new_reward <- rnorm(1, mean = .self$Q_Star[arm])
      .self$Q_Known[arm] <- .self$calculate_mean(new_reward, .self$Q_Known[arm], .self$times_explored[arm])
      .self$times_explored[arm] <- .self$times_explored[arm] + 1
      .self$update_max()

      obtained_reward <<- c(obtained_reward, new_reward)
      mean_reward <<- c(.self$mean_reward, 
                        .self$calculate_mean(new_reward, .self$mean_reward[i-1], i-1))      
    }
  }
)

"""
Example usage...

Q_Star <- rnorm(10)
none <- Multiarm_Bandit(true_means=Q_Star)
little <- Multiarm_Bandit(true_means=Q_Star, epsilon=0.01)
some <- Multiarm_Bandit(true_means=Q_Star, epsilon=0.1)
none$learn()
little$learn()
some$learn()

library(ggplot2)
means <- data.frame(none=none$mean_reward, little=little$mean_reward, some=some$mean_reward)
means$indices <- as.integer(rownames(means))
q <- ggplot(means, aes(x=indices))
q <- q + geom_line(aes(y=none, colour="none"))
q <- q + geom_line(aes(y=little, colour='little'))
q <- q + geom_line(aes(y=some, colour='some'))
q
"""

