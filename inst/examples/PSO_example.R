library(particle.swarm.optimisation)
set.seed(42)
# This exemple use the PSO to solve the following equation:
# Y = a * x + b * x^2 + 10
# we want to find the value a and b where Y = 25 when X = 2.


fitness_function <- function(values){
  a <- values[1]
  b <- values[2]
  difference <- 25 - (a * 2 + b * 4 + 10)
  fitness <- 1 - abs(difference)
  return(fitness)
}

values_ranges <- list(c(-100,100),c(-100,100))

swarm <- ParticleSwarm$new(pop_size = 50,
                           values_names = c('a',"b"),
                           fitness_function = fitness_function,
                           max_it = 50,
                           acceleration_coefficient_range = list(c(0.5,1),c(0.5,1)),
                           inertia = 0.5,
                           range_of_values = values_ranges)
if(interactive()){
  swarm$run(verbose = FALSE,
            plot = TRUE,
            save_file = FALSE)
} else {
  swarm$run(verbose = FALSE,
            plot = FALSE,
            save_file = FALSE)
}

print(swarm)
print("solution: ")
print(swarm$swarm_best_values)


