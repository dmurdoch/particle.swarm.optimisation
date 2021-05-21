set.seed(42)
fitness_function <- function(values){
  return(values[1]+values[2]+values[3])
}

test_that("Particle new work", {
  expect_error(Particle$new())
  expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),   #length(values) != length(values_ranges)
                             values = c(10,10,10,10),
                             fitness_function = fitness_function,
                             acceleration_coefficient = c(0.5,0.5),
                             inertia = 1))
  expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),   # no values
                             fitness_function = fitness_function,
                             acceleration_coefficient = c(0.5,0.5),
                             inertia = 1))
  expect_error(Particle$new(values = c(1,8,5),   # no values_ranges
                             fitness_function = fitness_function,
                             acceleration_coefficient = c(0.5,0.5),
                             inertia = 1))
  expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),  # no fitness function
                             values = c(1,8,5),
                             acceleration_coefficient = c(0.5,0.5),
                             inertia = 1))
  expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)), # fitness_function is not a function
                             values = c(1,8,5),
                             fitness_function = 2,
                             acceleration_coefficient = c(0.5,0.5),
                             inertia = 1))
  expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),
                             values = c(1,8,5),
                             fitness_function = fitness_function,  # no acceleration coefficient
                             inertia = 1))
  expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),
                             values = c(1,8,5),
                             fitness_function = fitness_function,
                             acceleration_coefficient = c(1), # there is only one acceleration coefficient
                             inertia = 1))
  expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),
                             values = c(1,8,5),
                             fitness_function = fitness_function,
                             acceleration_coefficient = c(0.5,0.5))) # there is no inertia
  expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),
                            values = c(1,8,5),
                            fitness_function = fitness_function,
                            acceleration_coefficient = c(0.5,0.5),
                            inertia = "hey")) # inertia is not a numeric
  expect_failure(expect_error(Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),
                            values = c(1,8,5),
                            fitness_function = fitness_function,
                            acceleration_coefficient = c(0.5,0.5),
                            inertia = 1)))
})


particle_test <- Particle$new(values_ranges = list(c(0,10),c(0,10),c(0,10)),
                                values = c(1,8,5),
                                fitness_function = fitness_function,
                                acceleration_coefficient = c(0.5,0.5),
                                inertia = 1)

test_that("Particle private field access", {
  expect_error(particle_test$fitness <- 0)
  expect_error(particle_test$acceleration_coefficient <- 0)
  expect_error(particle_test$fitness_function <- 0)
  expect_error(particle_test$inertia <- 0)
  expect_error(particle_test$personal_best_fitness <- 0)
  expect_error(particle_test$personal_best_values <- 0)
  expect_error(particle_test$values <- 0)
  expect_error(particle_test$velocity <- 0)
  expect_error(particle_test$values_ranges <- 0)
  expect_failure(expect_error(particle_test$acceleration_coefficient))
  expect_failure(expect_error(particle_test$fitness))
  expect_failure(expect_error(particle_test$fitness_function))
  expect_failure(expect_error(particle_test$inertia))
  expect_failure(expect_error(particle_test$personal_best_fitness))
  expect_failure(expect_error(particle_test$personal_best_values))
  expect_failure(expect_error(particle_test$values))
  expect_failure(expect_error(particle_test$values_ranges))
  expect_failure(expect_error(particle_test$velocity))
  })

test_that("Particle get_fitness", {
  expect_error(particle_test$get_fitness(122))
  expect_equal(particle_test$get_fitness()$fitness,14)
})

test_that("Particle update",{
  expect_equal(particle_test$update(c(10,10,10))$values,c(5.2,8.9,7.3),tolerance = 0.01)
})

test_that("particle update_personal_best_fitness",{
  particle_test$update_personal_best_fitness()
  expect_equal(particle_test$personal_best_values,c(5.2,8.9,7.3),tolerance=0.01)
  expect_equal(particle_test$personal_best_fitness,21.5,tolerance=0.01)
})

test_that('Particle print',{
  expect_output(particle_test$print(),'Particle:')
  expect_output(particle_test$print(),'Values 1:')
  expect_output(particle_test$print(),'Values 2:')
  expect_output(particle_test$print(),'Values 3:')
  expect_output(particle_test$print(),'fitness :')
})
