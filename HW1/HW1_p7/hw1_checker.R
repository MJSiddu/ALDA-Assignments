##############
# Use this function to check your results
# DO NOT submit this script
# TA will use something similar to this 
# (in addition to other verifications and possibly a different dataset) to grade your script
#############

rm(list = ls(all = T))

# set working directory to point to the folder containing hw1.R

# source hw1.R
source('./hw1.R')

# read data in matrix format
data_matrix <- read_data('./hw1_word_frequency.csv')

# The full distance matrix may take some time to calculate. If you 
# want to test your code quickly, you can use a subset of the data:
# data_matrix <- read_data('./hw1_word_frequency_small.csv')

# calculate each distance, measure exec. time
start_time <- Sys.time()
euclidean_result <- calculate_matrix(data_matrix, 'calculate_euclidean')
time.taken <- Sys.time() - start_time
print(paste('euclidean exec time = ',time.taken, 'seconds'))

start_time <- Sys.time()
cosine_result <- calculate_matrix(data_matrix, 'calculate_cosine')
time.taken <- Sys.time() - start_time
print(paste('cosine exec time = ',time.taken, 'seconds'))

start_time <- Sys.time()
manhattan_result <- calculate_matrix(data_matrix, 'calculate_manhattan')
time.taken <- Sys.time() - start_time
print(paste('manhattan exec time = ',time.taken, 'seconds'))

start_time <- Sys.time()
chebyshev_result <- calculate_matrix(data_matrix, 'calculate_chebyshev')
time.taken <- Sys.time() - start_time
print(paste('chebyshev exec time = ',time.taken, 'seconds'))

data_matrix_normalized <- normalize_data(data_matrix)
euclidean_distance_normalized <- analyze_normalization(data_matrix, data_matrix_normalized)
