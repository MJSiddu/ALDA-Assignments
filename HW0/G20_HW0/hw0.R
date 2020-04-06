#######################
# ALDA: hw0.R
# Instructor: Dr. Thomas Price
# Mention your team details here
# Group: G20
# Amal Sony (asony)
# Siddu Madhure Jayanna (smadhur)
# Mohd Sharique Khan (mkhan8)
#########################
require(ggplot2)
set.seed(123)
# no install.packages or rm(list=ls(all=T)) in this function


intro_to_r <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: a  list:  [generated vector (type: double), mean of vector (type: double), median of the vector (type: double), max value of vector (type: double), min value of the vector (type: double)]
  new_vector <- runif(num_values,min=0,max=100)
  list(new_vector = new_vector,new_mean = mean(new_vector),new_median = median(new_vector),new_max = max(new_vector),new_min = min(new_vector))
}

intro_to_plotting <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: two plots (saved to disk, no return value), descriptions for which have been provided in the hw0 document. 
  new_vector <- runif(num_values,min=0,max=100)
  qplot(new_vector,new_vector)
  ggsave("G20_plot01.pdf")
  qplot(new_vector,new_vector^2)
  ggsave("G20_plot02.pdf")
}

# do not call either function in this script 
# this script is only for function definitions
# if you wish to test your script, call your functions separately in a separate script.