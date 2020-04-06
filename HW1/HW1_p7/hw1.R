###########
# HW1
# Mention your team details here
#
#
#
############

# You may use the following libraries:
# If you get an error when running these lines, 
# make sure to install the respective libraries
require(ggplot2)
require(reshape2)
require(data.table)
require(philentropy)
require(plyr)
require(utils)

# read data matrix
read_data <- function(path = './hw1_word_frequency.csv') {
  # Note 1: DO NOT change the function arguments
  # Input: path: type: string, output: a matrix containing data from hw1_word_frequency.csv
  # Write code here to read the csv file as a matrix and return it.
  data = read.csv(path, sep=",", header = FALSE)
  matrix_data <- as.matrix(data)  
}

calculate_matrix <- function(data_matrix, method_name){
  # NOTE: This function has already been implemented for you.
  # DO NOT modifiy this function.
  # Input: data_matrix: type: matrix n_sentences x sentence_length,
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # Input: method_name: type: string, can be one of the following values: ('calculate_euclidean', 'calculate_cosine', 'calculate_manhattan', 'calculate_chebyshev')
  # output: a 155 x 155 matrix of type double, containing the distance/similarity calculation using method_name between 
  # every pair of sentences 
  # This function has already been implemented for you. It takes the data matrix and method name, outputs the distance
  # matrix based on the method name.
  # Programming logic for selecting every pair wise rows from the data matrix has already been provided for you
  # for euclidean, cosine and manhattan. You are only required to write the logic to calculate the actual distances for a pair of vectors p and q
  # in the corresponding functions listed above.
  # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
  # library is already optimized. Read the documentation and figure out how to compute the distance matrix
  # without loops for chebyshev
  distance_matrix = matrix(0L, nrow = nrow(data_matrix), ncol = nrow(data_matrix))
  if(method_name %in% c("calculate_euclidean", "calculate_cosine", "calculate_manhattan")){
    # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(data_matrix))){
      for(j in seq(i, nrow(data_matrix))){
        distance_matrix[i,j] <- do.call(method_name, list(unlist(data_matrix[i,]), unlist(data_matrix[j,])))
        distance_matrix[j,i] <- distance_matrix[i,j]
      }
    }
  }else if(method_name == "calculate_chebyshev"){
    # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
    # library is already optimized. Read the documentation and figure out how to compute the distance matrix
    # without loops for chebyshev
    distance_matrix <- calculate_chebyshev(data_matrix)
  }
  return(distance_matrix)
}

calculate_euclidean <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the euclidean distance between the vectors p and q
  # Write code here to calculate the euclidean distance between pair of vectors p and q
  sum_val = 0.0
  for (i in seq_along(p)) {
    sum_val = sum_val + ((p[i] - q[i]) ^ 2)
  }
  res = sqrt(sum_val)
  return(res)
}


calculate_cosine <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the cosine distance between the vectors p and q
  # Write code here to calculate the cosine distance between pair of vectors p and q
  dot_prod = p %*% q
  dot_prod_p = p %*% p
  dot_prod_q = q %*% q
  res = dot_prod[1][1]/(sqrt(dot_prod_p[1][1]) * sqrt(dot_prod_q[1][1]))
  return(1-res)
}

calculate_manhattan <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the manhattan distance between the vectors p and q
  # Write code here to calculate the manhattan distance between pair of vectors p and q
  sum_val = 0.0
  for (i in seq_along(p)) {
    sum_val = sum_val + abs(p[i] - q[i])
  }
  return(sum_val)
}

calculate_chebyshev <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 155 matrix of type double, containing the chebyshev distance between every pair of sentences
  # Write code here to calculate chebyshev distance given an original data matrix of size 155 x 200
  distance(data_matrix, method = "chebyshev")
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize_data <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 200 matrix of type double, containing the normalized values in [0, 1] range per row.
  # Write code here to normalize data_matrix
  return (t(apply(data_matrix, 1, normalize)))
}

analyze_normalization <- function(data_matrix, normalized_data_matrix){
  # Input: data_matrix, normalized_data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # data_matrix refers to the original data_matrix, and normalized_data_matrix refers to the normalized version (i.e., output from normalize_data)
  # Output: a 155 x 155 matrix of type double containing the updated euclidean distance using the normalized_data_matrix
  # Also generate the plot(s) that were requested in the question and save them to the pdf.
  # Write code here to generate the output requested as well as any plots/analyses requested.
  euclidean_dist = calculate_matrix(data_matrix, 'calculate_euclidean')
  updated_euclidean_dist = calculate_matrix(normalized_data_matrix, 'calculate_euclidean')
  x <- 1:155
  pdf("G20_plot01.pdf")
  plot(x, euclidean_dist[1,x], type="o", col="blue", pch="o", lty=1, ylim=c(0,15))
  points(x, updated_euclidean_dist[1,x], col="red", pch="*")
  lines(x, updated_euclidean_dist[1,x], col="red",lty=2)
  dev.off()
  plot_distance_matrix(euclidean_dist)
  ggsave("G20_plot02.pdf")
  return (updated_euclidean_dist)
  
  # comments from siddu
  # recompute the euclidean distance using the method you implemented in the previous section - done
  # Do you notice any diﬀerence in the two distance matrices? - todo
  # In the function analyze normalization, write some R code to compare the two outcomes. - todo
  # Identify at least 3 properties of the distance matrix that change when using normalized data. - todo 
  # In the PDF, list the changes you identiﬁed and explain why they occurred. Use both numeric calculations 
  # and visualizations to justify your answers. (Hint: use the hist function, and the provided plot distance matrix function.) - todo
}

# This function visualizes a distance matrix, with color indicating distance
plot_distance_matrix <- function(distance_matrix) {
  ggplot(data = melt(distance_matrix), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    scale_x_discrete(name="Row Number") + scale_y_discrete(name="Row Number") +
    scale_fill_continuous(name="Distance")
}
