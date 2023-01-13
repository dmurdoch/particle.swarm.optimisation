#' @title Swarm
#' @description Particle Swarm, used to launch the Particle Swarm Optimisation, The PSO is used to maximise the fitness.
#' @import rgl
#' @importFrom  R6 R6Class
#' @export
#' @examples
#' # In this example we use the PSO to solve the following equation:
#' # a * 5 + b * 25 + 10 = 15
#'
#' fitness_function <- function(values){
#'   a <- values[1]
#'   b <- values[2]
#'   particule_result <- a*5 + b*25 + 10
#'   difference <- 15 - particule_result
#'   fitness <- 1 - abs(difference)
#'   return(fitness)
#' }
#'
#' values_ranges <- list(c(-10^3,10^3),c(-10^3,10^3))
#'
#' swarm <- ParticleSwarm$new(pop_size = 200,
#'                            values_names = list("a","b"),
#'                            fitness_function = fitness_function,
#'                            max_it = 75,
#'                            acceleration_coefficient_range = list(c(0,1),c(0,1)),
#'                            inertia = 0.5,
#'                            ranges_of_values = values_ranges)
#' swarm$run(plot = FALSE,verbose = FALSE,save_file = FALSE)
#' # the solution is :
#' swarm$swarm_best_values
#' swarm$swarm_best_values[[1]]*5 + swarm$swarm_best_values[[2]] *25 + 10
ParticleSwarm <- R6Class('ParticleSwarm',
                          private = list(
                            #' @field pop_size (numeric) number of particles in the swarm
                            .pop_size = NA,
                            #' @field ranges_of_values (list) range for each value for the particle
                            .ranges_of_values = NA,
                            #' @field values_names (list) list of names for each value (optionnal)
                            .values_names = NA,
                            #' @field pop (list) list of particle in the swarm
                            .pop = list(),
                            #' @field fitness_function (function) fitness function used to find the fitness of the particle
                            .fitness_function = NA,
                            #' @field list_fitness (list) list of fitness of the particles
                            .list_fitness = list(),
                            #' @field max_it (numeric) maximum number of iteration
                            .max_it = NA,
                            #' @field acceleration_coefficient_range (list) coefficient c1 and c2 for the particles
                            .acceleration_coefficient_range = NA,
                            #' @field swarm_best_fitness (numeric) best fitness of the swarm
                            .swarm_best_fitness = NA,
                            #' @field swarm_best_values (numeric) values of the particle with the best fitness
                            .swarm_best_values = NA,
                            #' @field inertia (numeric) inertia of the particles
                            .inertia = NA
                          ),
                          active = list(
                            pop_size = function(value){
                              if (missing(value)) {
                                private$.pop_size
                              } else {
                                stop("`$pop_size can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            },
                            ranges_of_values = function(value){
                              if (missing(value)) {
                                private$.ranges_of_values
                              } else {
                                stop("`$ranges_of_values can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            },
                            values_names = function(value){
                              if (missing(value)) {
                                private$.values_names
                              } else {
                                stop("$values_names can't be changed after the creation of the Swarm",call. = FALSE)
                              }
                            },
                            pop = function(value){
                              if (missing(value)) {
                                private$.pop
                              } else {
                                stop("`$pop can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            },
                            fitness_function = function(value){
                              if (missing(value)) {
                                private$.fitness_function
                              } else {
                                stop("`$fitness_function can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            },
                            list_fitness = function(value){
                              if (missing(value)) {
                                private$.list_fitness
                              } else {
                                stop("`$list_fitness can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            },
                            max_it = function(value){
                              if (missing(value)) {
                                private$.list_fitness
                              } else {
                                private$.max_it <- value
                              }
                            },
                            acceleration_coefficient_range = function(value){
                              if (missing(value)) {
                                private$.acceleration_coefficient_range
                              } else {
                                stop("`$acceleration_coefficient_range can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            },
                            swarm_best_fitness = function(value){
                              if (missing(value)) {
                                private$.swarm_best_fitness
                              } else {
                                stop("`$swarm_best_fitness can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            },
                            swarm_best_values = function(value){
                              if (missing(value)) {
                                private$.swarm_best_values
                              } else {
                                stop("`$swarm_best_values can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            },
                            inertia = function(value){
                              if (missing(value)) {
                                private$.inertia
                              } else {
                                stop("`$inertia can't be changed after the creation of the Swarm", call. = FALSE)
                              }
                            }
                          ),
                          public = list(
                            #' @description
                            #' Create a new ParticleSwarm object.
                            #' @param pop_size number of individu in the swarm. (numeric)
                            #' @param ranges_of_values range for each value of the particle (min and max). (List)
                            #' @param values_names list of names for each value (character)
                            #' @param fitness_function function used to test the Particle and find his fitness. (function)
                            #' @param max_it Maximum number of iteration for the PSO. (numeric)
                            #' @param acceleration_coefficient_range a vector of four values (min and max for c1 and c2) (numeric)
                            #' @param inertia The inertia for the particle (the influence of the previous velocity on the next velocity). (numeric)
                            #' @examples
                            #' # Create a ParticleSwarm object
                            #' swarm <- ParticleSwarm$new(pop_size=20,
                            #'                             values_names=c('a','b'),
                            #'                             max_it=20,
                            #'                             fitness_function = function(values){return(values[1]+values[2])},
                            #'                             acceleration_coefficient=list(c(0.5,1),c(0.5,1)),
                            #'                             inertia=0.5,
                            #'                             ranges_of_values=list(c(-100,100),c(-100,100)))
                            #' @return A new `ParticleSwarm` object.
                            initialize = function(pop_size,
                                                  values_names,
                                                  fitness_function,
                                                  max_it,
                                                  acceleration_coefficient_range,
                                                  inertia,
                                                  ranges_of_values){
                              if (is.list(ranges_of_values)){
                                private$.ranges_of_values <- ranges_of_values
                              } else {stop("ERROR ranges_of_values need to be a list.")}
                              if(is.function(fitness_function)){
                                private$.fitness_function <- fitness_function
                              } else{stop('ERROR fitness_function need to be a function')}
                              if (length(acceleration_coefficient_range) != 2){
                                stop('ERROR acceleration_coefficient_range need to be four numeric values c(min_c1,max_c1,min_c2,max_c2)')
                              }
                              private$.acceleration_coefficient_range <- acceleration_coefficient_range
                              if (is.numeric(inertia)){
                                private$.inertia <- inertia
                              } else {stop("inertia need to be a numeric value")}
                              if (is.numeric(max_it)){
                                if (length(max_it) == 1){
                                  private$.max_it <- max_it
                                } else{stop('ERROR max_it need to be one number')}
                              } else {stop('ERROR max_it need to be a numeric')}
                              if (is.numeric(pop_size)){
                                private$.pop_size <- pop_size
                              } else {stop('ERROR pop_size need to be a numeric')}
                              if (!missing(values_names)){
                                private$.values_names <- values_names
                              }
                            },
                            #' @description
                            #' Make the Particle Swarm Optimisation
                            #' @param verbose print the different step (iteration and individu)
                            #' @param plot plot the result of each iteration (only for 2D or 3D problem)
                            #' @param save_file save the population of each Iteration in a file and save the plot if plot=TRUE
                            #' @param dir_name name of the directory, default value is PSO_pop
                            #' @return self
                            #' @examples
                            #' # Create a ParticleSwarm object
                            #' swarm <- ParticleSwarm$new(pop_size=20,
                            #'                             values_names=c('a','b'),
                            #'                             max_it=20,
                            #'                             fitness_function = function(values){return(values[1]+values[2])},
                            #'                             acceleration_coefficient=list(c(0.5,1),c(0.5,1)),
                            #'                             inertia=0.5,
                            #'                             ranges_of_values=list(c(-100,100),c(-100,100)))
                            #' # run the PSO
                            #' swarm$run(verbose = FALSE,
                            #'           plot = FALSE,
                            #'           save_file = FALSE)
                            #' # return the best result:
                            #' print(swarm$swarm_best_values)
                            run=function(verbose = TRUE, plot = TRUE, save_file = FALSE, dir_name='PSO_pop'){
                              if (save_file){
                                if (!dir.exists(dir_name)){
                                  dir.create(dir_name)
                                }
                              }
                              self$generate_pop(verbose)
                              nb_dim <- length(private$.ranges_of_values)
                              for (iteration in 1:private$.max_it){
                                self$move_the_swarm(verbose)
                                if (nb_dim == 2 && plot){
                                  self$plot_the_swarm_2D(iteration,save_file)
                                } else if (nb_dim == 3 && plot){
                                  self$plot_the_swarm_3D(iteration,save_file)
                                }
                                if (save_file){
                                  self$save_pop(iteration,dir_name)
                                }
                                if (verbose){
                                  print(paste('iteration',iteration,sep = ' '))
                                }
                              }
                              invisible(self)
                            },
                            #' @description
                            #' create the population of the swarm (this method is automatically called by the run method)
                            #' @param verbose print the advancement or not
                            #' @return self
                            generate_pop=function(verbose = TRUE){
                              while (length(private$.pop) != private$.pop_size) {
                                if (verbose){
                                  print(paste('individu ',length(private$.pop)+1,sep = ''))
                                }
                                values <- numeric()
                                for (i in private$.ranges_of_values) {
                                  values <- append(values,runif(n = 1,min = i[1],max = i[2]))
                                }
                                coef <- c(runif(n = 1,
                                                min = unlist(private$.acceleration_coefficient_range[1])[1],
                                                max = unlist(private$.acceleration_coefficient_range[1])[2]),
                                          runif(n = 1,
                                                min = unlist(private$.acceleration_coefficient_range[2])[1],
                                                max = unlist(private$.acceleration_coefficient_range[2])[2]))
                                individu <- Particle$new(values=values,
                                                          values_ranges=private$.ranges_of_values,
                                                          fitness_function=private$.fitness_function,
                                                          acceleration_coefficient=coef,
                                                          inertia=private$.inertia)
                                individu$get_fitness()
                                individu$update_personal_best_fitness()

                                if (is.na(private$.swarm_best_fitness)){
                                  private$.swarm_best_values <- individu$values
                                  private$.swarm_best_fitness <- individu$fitness
                                } else if (individu$fitness > private$.swarm_best_fitness){
                                  private$.swarm_best_values <- individu$values
                                  private$.swarm_best_fitness <- individu$fitness
                                }
                                private$.pop <- append(private$.pop,individu)
                              }
                              invisible(self)
                            },
                            #' @description
                            #' The method used to change the location of each particle (this method is automatically called by the run method)
                            #' @param verbose print or not the advancement
                            #' @return self
                            move_the_swarm=function(verbose){
                              c <- 0
                              for (individue in private$.pop){
                                c <- c + 1
                                individue$update(private$.swarm_best_values)
                                if (verbose){
                                  print(paste("individu",c,sep = " "))
                                }
                              }
                              for (individue in private$.pop){
                                if (individue$fitness >= private$.swarm_best_fitness){
                                  private$.swarm_best_fitness <- individue$fitness
                                  private$.swarm_best_values <- individue$values
                                }
                              }
                              invisible(self)
                            },
                            #' @description
                            #' The method used to save the values and fitness of the population in a CSV file (this method is automatically called by the run method if you have chosen to save the result)
                            #' @param nb_it number of the iteration, used to create the name of the csv file
                            #' @param dir_name Name of the directory
                            #' @return self
                            save_pop=function(nb_it,dir_name){
                              pop_result <- data.frame()
                              value <- c(0)
                              for (i in private$.pop){
                                for (val in i$values){
                                  value <- cbind(value,val)
                                }
                                pop_result <- rbind(pop_result,cbind(value,i$fitness))
                                value <- c(0)
                              }
                              pop_result <- pop_result[,-1]
                              if (length(pop_result)!=0){
                                names(pop_result) <- c(private$.values_names,'accuracy')
                              }
                              write.csv(pop_result,file = paste(paste(dir_name,"/Iteration",sep=''),nb_it,sep = "_"))
                              invisible(self)
                            },
                            #' @description
                            #' method used to plot a 2D plot (this method is automatically called by the run method if you have chosen to plot the swarm)
                            #' @param nb_it number of the iteration used to save the plot as a png
                            #' @param save_file save the plot as a file
                            #' @return self
                            plot_the_swarm_2D=function(nb_it,save_file){
                              x <- numeric()
                              y <- numeric()
                              for (i in private$.pop){
                                x <- c(x,i$values[1])
                                y <- c(y,i$values[2])
                              }
                              xlim <- c(min(private$.ranges_of_values[[1]]),max(private$.ranges_of_values[[1]]))
                              ylim <- c(min(private$.ranges_of_values[[2]]),max(private$.ranges_of_values[[2]]))
                              plot(x,y,
                                   type='p',
                                   xlim=xlim,
                                   ylim=ylim,
                                   pch=20,
                                   xlab=private$.values_names[[1]],
                                   ylab=private$.values_names[[2]])
                              if(save_file){
                                png(paste('iteration',nb_it,".png",sep=''))
                                plot(x,y,
                                     type='p',
                                     xlim=xlim,
                                     ylim=ylim,
                                     pch=20,
                                     xlab=private$.values_names[[1]],
                                     ylab=private$.values_names[[2]])
                                dev.off()
                              }
                              invisible(self)
                            },
                            #' @description
                            #' method used to plot a 3D plot
                            #' @param nb_it number of the iteration used to save the plot as a png (this method is automatically called by the run method if you have chosen to plot the swarm)
                            #' @param save_file save the plot as a file
                            #' @return self
                            plot_the_swarm_3D=function(nb_it,save_file){
                              x <- numeric()
                              y <- numeric()
                              z <- numeric()
                              for (i in private$.pop){
                                x <- c(x,i$values[1])
                                y <- c(y,i$values[2])
                                z <- c(z,i$values[3])
                              }
                              xlim <- c(min(private$.ranges_of_values[[1]]),max(private$.ranges_of_values[[1]]))
                              ylim <- c(min(private$.ranges_of_values[[2]]),max(private$.ranges_of_values[[2]]))
                              zlim <- c(min(private$.ranges_of_values[[3]]),max(private$.ranges_of_values[[3]]))
                              clear3d()
                              bg3d(color = 'white')
                              plot3d(x,y,z,
                                     type="s",
                                     radius=10,
                                     col="red",
                                     xlim=xlim,
                                     ylim=ylim,
                                     zlim=zlim,
                                     xlab = private$.values_names[[1]],
                                     ylab = private$.values_names[[2]],
                                     zlab = private$.values_names[[3]])
                              if(save_file){
                                rgl.snapshot(paste('iteration',nb_it,".png",sep = '_'))
                              }
                              invisible(self)
                            },
                            #' @description
                            #' Print the current result of the population
                            print=function(){
                              pop_result <- data.frame()
                              value <- c(0)
                              for (i in private$.pop){
                                for (val in i$values){
                                  value <- cbind(value,val)
                                }
                                pop_result <- rbind(pop_result,cbind(value,i$fitness))
                                value <- c(0)
                              }
                              pop_result <- pop_result[,-1]
                              if (length(pop_result)!=0){
                                names(pop_result) <- c(private$.values_names,'accuracy')
                              }
                              print('Population result : ')
                              print(pop_result)
                            }
                          )
)
