## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(rgl)

## ----setup--------------------------------------------------------------------
set.seed(42)
library(particle.swarm.optimisation)

## ----prepare------------------------------------------------------------------
range_of_value <- list(c(1,300),c(1,300),c(1,300))
fitness_function <- function(values){
  return(values[[1]]+values[[2]]+values[[3]])
}

## ----particle_new-------------------------------------------------------------
exemple <- Particle$new(values_ranges = range_of_value,
                           values = c(50,120,187),
                           fitness_function = fitness_function,
                           acceleration_coefficient = c(0.3,0.4),
                           inertia = 0.4)
print(exemple)

## ----particle_get_fitness-----------------------------------------------------
exemple$get_fitness() # 50+120+187
print(exemple) 

## ----Particle_update----------------------------------------------------------
print(paste('best fitness before :',exemple$personal_best_fitness,sep = ' '))
exemple$update(swarm_best = c(200,300,300)) # the swarm best is just a random value here
print(exemple)
print(paste('best fitness after :',exemple$personal_best_fitness,sep = ' '))

## ----particle_update_personal_best_fitness------------------------------------
exemple$update_personal_best_fitness() # do nothing because the update method of the previous chunck also call this method.

## ----particle_print-----------------------------------------------------------
print(exemple)
# or :
exemple$print()

## ----ParticleSwarm_new--------------------------------------------------------
swarm_exemple <- ParticleSwarm$new(pop_size = 10,
                                    ranges_of_values = range_of_value,
                                    fitness_function = fitness_function,
                                    max_it = 10,
                                    values_names = list('a','b','c'),
                                    acceleration_coefficient_range = list(c(0,1),c(0,1)),
                                    inertia = 0.4)

## ----ParticleSwarm_generate_pop-----------------------------------------------
swarm_exemple$generate_pop(verbose = FALSE)
print(swarm_exemple)

## ----ParticleSwarm_move_the_swarm---------------------------------------------
swarm_exemple$move_the_swarm(verbose = FALSE)
print(swarm_exemple)

## ----ParticleSwarm_save_the_pop, eval=FALSE-----------------------------------
#  swarm_exemple$save_pop()

## ----ParticleSwarm_plot_2d----------------------------------------------------
swarm_exemple$plot_the_swarm_2D(nb_it=0,save_file=FALSE)

## ----ParticleSwarm_plot_3D, eval=FALSE----------------------------------------
#  swarm_exemple$plot_the_swarm_3D(nb_it=0,save_file=FALSE)

## ----ParticleSwarm_run--------------------------------------------------------
swarm_exemple$run(verbose = FALSE,plot = FALSE,save_file = FALSE)
print(swarm_exemple)

