#' @title Particle
#' @description
#' Class for the Particles used in the Particle Swarm Optimisation, It is call by the Particle Swarm object to make the population.
#' @importFrom  R6 R6Class
#' @export
#' @examples
#' # If you use the Particle Swarm Object there is no need to manually create the Particle
#' # But if you want to use the Particle for another project:
#'
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
#' particle_example <- Particle$new(values_ranges = values_ranges,
#'                                  values = c(0,0),
#'                                  fitness_function = fitness_function,
#'                                  acceleration_coefficient = c(0.5,0.5),
#'                                  inertia = 0.4)
#' print(particle_example)
#' particle_example$get_fitness()
#' print(particle_example)
#' particle_example$update(c(10,25))
#' print(particle_example)
Particle <- R6::R6Class('Particle',
                     private = list(
                       #' @field values_ranges (list) max and min for each value of the particle
                       .values_ranges = NA,
                       #' @field values (numeric) values of the particle (his position in space)
                       .values = NA,
                       #' @field fitness (numeric) fitness of the particle (his score)
                       .fitness = NA,
                       #' @field fitness_function (function) function used to find the fitness
                       .fitness_function = NA,
                       #' @field personal_best_values (numeric) Best values of the particle
                       .personal_best_values = NA,
                       #' @field personal_best_fitness (numeric) Fitness of the best values
                       .personal_best_fitness = 0,
                       #' @field velocity (numeric) Velocity of the particle (one velocity for each values)
                       .velocity = 0,
                       #' @field acceleration_coefficient (numeric) coefficient c1 and c2 (for personal and global best)
                       .acceleration_coefficient = NA,
                       #' @field inertia (numeric) inertia of the particle
                       .inertia = NA
                     ),
                     active = list(
                       values_ranges = function(value){
                         if (missing(value)) {
                           private$.values_ranges
                         } else {
                           stop("`$values_range can't be changed after the creation of the particle", call. = FALSE)
                         }
                       },
                       values = function(value){
                         if (missing(value)) {
                           private$.values
                         } else {
                           stop("`$values can't be changed after the creation of the particle (changed by the update method", call. = FALSE)
                         }
                       },
                       fitness = function(value){
                         if (missing(value)) {
                           private$.fitness
                         } else {
                           stop("`$fitness can't be changed", call. = FALSE)
                         }
                       },
                       fitness_function = function(value){
                         if (missing(value)) {
                           private$.fitness_function
                         } else {
                           stop("`$fitness_function can't be changed after the creation of the particle", call. = FALSE)
                         }
                       },
                       personal_best_values = function(value){
                         if (missing(value)) {
                           private$.personal_best_values
                         } else {
                           stop("`$personal_best_values can't be changed by the user", call. = FALSE)
                         }
                       },
                       personal_best_fitness = function(value){
                         if (missing(value)) {
                           private$.personal_best_fitness
                         } else {
                           stop("`$personal_best_fitness can't be changed by the user", call. = FALSE)
                         }
                       },
                       velocity = function(value){
                         if (missing(value)) {
                           private$.velocity
                         } else {
                           stop("`$velocity can't be changed by the user", call. = FALSE)
                         }
                       },
                       acceleration_coefficient = function(value){
                         if (missing(value)) {
                           private$.acceleration_coefficient
                         } else {
                           stop("`$acceleration_coefficient can't be changed after the creation of the particle", call. = FALSE)
                         }
                       },
                       inertia = function(value){
                         if (missing(value)) {
                           private$.inertia
                         } else {
                           stop("`$inertia can't be changed after the creation of the particle", call. = FALSE)
                         }
                       }
                     ),
                     public = list(
                       #' @description
                       #' Create a new Particle object.
                       #' @param values_ranges range for each value of the particle (min and max), his size need to be the same as values. (List)
                       #' @param values, values of the particles. (numeric)
                       #' @param fitness_function function used to test the Particle and find his fitness. (function)
                       #' @param acceleration_coefficient a vector of two values, one for c1 (the personal coefficient), and one for c2 (the global coefficient). (numeric)
                       #' @param inertia The inertia of the particle (the influence of the previous velocity on the next velocity). (numeric)
                       #' @return A new `Particle` object.
                       initialize = function(values_ranges,
                                             values,
                                             fitness_function,
                                             acceleration_coefficient,
                                             inertia){
                         if (is.list(values_ranges)){
                           if (length(values_ranges) != length(values)){
                             stop('ERROR The length of values_ranges and values need to be the same.')
                           }
                           private$.values_ranges <- values_ranges
                         } else {stop("ERROR Values_ranges need to be a list.")}
                         if (is.numeric(values)){
                           private$.values <- values
                         } else{stop('ERROR Values need to be numeric.')}
                         if(is.function(fitness_function)){
                           private$.fitness_function <- fitness_function
                         } else{stop('ERROR fitness_function need to be a function')}
                         private$.personal_best_values <- values
                         if (length(acceleration_coefficient) != 2){
                           stop('ERROR acceleration_coefficient need to be two numeric values c(c1,c2)')
                         }
                         private$.acceleration_coefficient <- acceleration_coefficient
                         if (is.numeric(inertia)){
                           private$.inertia <- inertia
                         } else {stop("inertia need to be a numeric value")}
                         private$.velocity <- rep(0,length(values))
                       }, # Initializer of the class
                       #' @description
                       #' Calculate the fitness of the particle with the fitness function and save it in self$fitness
                       #' @return self
                       get_fitness=function(){
                         private$.fitness <- self$fitness_function(private$.values)
                         invisible(self)
                       },
                       #' @description
                       #' Update Particle's position and velocity.
                       #' @param swarm_best the best values of the swarm used to update the velocity
                       #' @return self
                       update=function(swarm_best){
                         c1 <- private$.acceleration_coefficient[1]
                         c2 <- private$.acceleration_coefficient[2]
                         r1 <- runif(n = 1,min = 0,max = 1)
                         r2 <- runif(n = 1,min = 0,max = 1)
                         for (index in 1:length(private$.values)){
                           # function v = v*i + c1*random*(pbest-currentval) + c2*random*(gbest-currentval)
                           perso_ecart <- (private$.personal_best_values[index] - private$.values[index])
                           global_ecart <- (swarm_best[index] - private$.values[index])
                           global_attac <- c2*r2*global_ecart
                           perso_attrac <- c1*r1*perso_ecart
                           private$.velocity[index] <- (private$.inertia * private$.velocity[index] + perso_attrac + global_attac)
                           #end
                           private$.values[index] <- private$.values[index] + private$.velocity[index]
                           if (private$.values[index] > max(unlist(private$.values_ranges[index]))){
                             private$.values[index] <- max(unlist(private$.values_ranges[index]))
                           }
                           else if (private$.values[index] < min(unlist(private$.values_ranges[index]))){
                             private$.values[index] <- min(unlist(private$.values_ranges[index]))
                           }
                         }
                         self$get_fitness()
                         if (private$.fitness > private$.personal_best_fitness){
                           self$update_personal_best_fitness()
                         }
                         invisible(self)
                       },
                       #' @description
                       #' Update the Particle's best values and fitness.
                       #' @return self
                       update_personal_best_fitness=function(){
                         private$.personal_best_fitness <- private$.fitness
                         private$.personal_best_values <- private$.values
                         invisible(self)
                       },
                       #' @description
                       #' print the current values of the particle and his fitness
                       print = function(){
                         cat('Particle: \n')
                         for (index in 1:length(private$.values)){
                           cat('Values ', index, ': ', private$.values[index],'\n',sep = '')
                         }
                         cat('fitness : ',private$.fitness, '\n',sep = '')
                       }
                     )
)

