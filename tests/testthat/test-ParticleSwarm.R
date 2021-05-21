set.seed(42)
fitness_function <- function(values){
  return(values[1]+values[2]+values[3])
}

ranges_of_values <- list(c(0,10),c(0,10),c(0,10))

test_that("ParticleSwarm new", {
  expect_error(ParticleSwarm$new())
  expect_error(ParticleSwarm$new(pop_size = "hey", # the pop is not a numeric
                                  ranges_of_values = ranges_of_values,
                                  fitness_function = fitness_function,
                                  max_it = 10,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = c(1,2,3,5), # range of values is not a list
                                  fitness_function = fitness_function,
                                  max_it = 10,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = ranges_of_values,
                                  fitness_function = "fitness_function", # fitness function is not a function
                                  max_it = 10,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = ranges_of_values,
                                  fitness_function = fitness_function,
                                  max_it = "hey",  # max_it is not a numeric
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = ranges_of_values,
                                  fitness_function = fitness_function,
                                  max_it = 10,
                                  acceleration_coefficient_range = c(c(0,1),c(0,1)), # coefficient range is not a list
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = ranges_of_values,
                                  fitness_function = fitness_function,
                                  max_it = 10,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = "hey")) # inertia is not a numeric
  expect_error(ParticleSwarm$new(ranges_of_values = ranges_of_values,
                                  fitness_function = fitness_function,
                                  max_it = 10,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  fitness_function = fitness_function,
                                  max_it = 10,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = ranges_of_values,
                                  max_it = 10,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = ranges_of_values,
                                  fitness_function = fitness_function,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = ranges_of_values,
                                  fitness_function = fitness_function,
                                  max_it = 10,
                                  inertia = 0.4))
  expect_error(ParticleSwarm$new(pop_size = 10,
                                  ranges_of_values = ranges_of_values,
                                  fitness_function = fitness_function,
                                  max_it = 10,
                                  acceleration_coefficient_range = list(c(0,1),c(0,1))))
  expect_failure(expect_error(ParticleSwarm$new(pop_size = 10,
                                               ranges_of_values = ranges_of_values,
                                               fitness_function = fitness_function,
                                               max_it = 10,
                                               acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                               inertia = 0.4)))
  })

swarm_test <- ParticleSwarm$new(pop_size = 10,
                   ranges_of_values = ranges_of_values,
                   fitness_function = fitness_function,
                   max_it = 10,
                   acceleration_coefficient_range = list(c(0,1),c(0,1)),
                   inertia = 0.4)



test_that('ParticleSwarm private fields',{
  expect_error(swarm_test$acceleration_coefficient_range <- 0)
  expect_error(swarm_test$fitness_function <- 0)
  expect_error(swarm_test$inertia <- 0)
  expect_error(swarm_test$list_fitness <- 0)
  expect_failure(expect_error(swarm_test$max_it <- 0)) #expect the failure because the max_it can be changed
  expect_error(swarm_test$pop <- 0)
  expect_error(swarm_test$pop_size <- 0)
  expect_error(swarm_test$ranges_of_values <- 0)
  expect_error(swarm_test$swarm_best_fitness <- 0)
  expect_error(swarm_test$swarm_best_values <- 0)
  expect_failure(expect_error(swarm_test$acceleration_coefficient_range))
  expect_failure(expect_error(swarm_test$fitness_function))
  expect_failure(expect_error(swarm_test$inertia))
  expect_failure(expect_error(swarm_test$list_fitness))
  expect_failure(expect_error(swarm_test$max_it))
  expect_failure(expect_error(swarm_test$pop))
  expect_failure(expect_error(swarm_test$pop_size))
  expect_failure(expect_error(swarm_test$ranges_of_values))
  expect_failure(expect_error(swarm_test$swarm_best_fitness))
  expect_failure(expect_error(swarm_test$swarm_best_values))
})

test_that('ParticleSwarm generate_pop',{
  expect_failure(expect_output(swarm_test$generate_pop(verbose = FALSE)))
  expect_equal(length(swarm_test$pop),swarm_test$pop_size)
  expect_false(is.na(swarm_test$swarm_best_fitness))
})

test_that('ParticleSwarm move_the_swarm',{
  expect_output(swarm_test$move_the_swarm(verbose = TRUE))
  expect_failure(expect_output(swarm_test$move_the_swarm(verbose = FALSE)))
})

test_that('ParticleSwarm Save_pop',{
  expect_error(swarm_test$save_pop())
  swarm_test$save_pop(nb_it = 1,dir_name=tempdir())
})

test_that('ParticleSarm Print',{
  expect_output(print(swarm_test))
})

test_that('ParticleSwarm run',{
  old_best_fitness <- swarm_test$swarm_best_fitness
  expect_failure(expect_output(swarm_test$run(verbose = FALSE,plot = FALSE,save_file = FALSE)))
  expect_output(swarm_test$run(verbose = TRUE,plot = FALSE,save_file = FALSE))
  expect_failure(expect_equal(swarm_test$swarm_best_fitness,old_best_fitness))
})
