########
# HW4 
# Instructor: Dr. Thomas Price
# Specify your team details here
# 
# Group G20
# Amal Sony (asony)
# Mohd Sharique Khan (mkhan8)
# Siddu Madhure Jayanna (smadhur)
#########
library(dplyr)
library(ggplot2)
library('caret') # for cross validation
library('nnet') # for neural networks
alda_calculate_sse <- function(data_df, cluster_assignments){
  # Calculate overall SSE
  # Input:
    # data_df: data frame that has been given to you by the TA (x,y attributes)
    # cluster_assignments: cluster assignments that have been generated using any of the clustering algorithms
  # Output:
    # A single value of type double, which is the total SSE of the clusters.
  data_df$cluster <- cluster_assignments
  n_clusters <- nlevels(as.factor(data_df$cluster))
  
  sse <- 0
  for(i in 1:n_clusters) {
    sub <- data_df[data_df$cluster==i,]
    x <- mean(sub$x)
    y <- mean(sub$y)
    
    for(j in 1:nrow(sub)) {
      row <- sub[j,]
      d <- (row$x-x)^2 + (row$y-y)^2
      sse <- sse + d
    }
  }
  
  return (sse)
}



alda_kmeans_elbow_plot <- function(data_df, k_values){
  # ~ 8-10 lines
  # Input:
    # data_df: Original data frame supplied to you by the TA
    # k_values: A vector of values of k for k means clustering
  
  # General Information:
    # Run k means for all the values specified in k_values, generate elbow plot
    # Use alda_cluster with kmeans as your clustering type
    # (you can see an example this function call in hw4_checker.R for k = 2, now repeat it for all k_values)
  
  # Output:
    # Nothing, simply generate a plot and save it to disk as "GroupNumber_elbow.png"
  
  plot_data <- data.frame(k=k_values)
  plot_data$sse <- 0
  
  for(i in 1:nrow(plot_data)) {
    row <- plot_data[i,]
    kmeans_result <- alda_cluster(data_df, n_clusters = row$k, clustering_type = "kmeans")
    kmeans_sse <- alda_calculate_sse(data_df, cluster_assignments = kmeans_result)
    plot_data$sse[i] <- kmeans_sse
  }
  
  qplot(plot_data$k,plot_data$sse, xlab = 'k', ylab = 'SSE')
  ggsave("G20_elbow.png");
}

alda_cluster <- function(data_df, n_clusters, clustering_type){
  set.seed(100) # this is v. important from reproducibility point of view
  # Perform specified clustering
  
  # Inputs:
  # data_df: The dataset provided to you, 2-dimensional (x1,x2)
  # n_clusters: number of clusters to be created
  # clustering_type: can be one of "kmeans" or "single-link" or "complete-link"
  
  # Outputs:
  # Cluster assignments for the specified method (vector, with length = nrow(data_df) and values ranging from 1 to n_clusters)
  if(clustering_type == "kmeans"){
    # ~ 1-2 lines
    # allowed packages for kmeans: R-base, stats, dplyr
    # set the max number of iterations to 100, number of random restarts = 1 (let's not break the TA's computer! )
    # choose "Lloyd" as the algorithm
    cluster_vector <- kmeans(data_df, n_clusters, iter.max = 100, nstart = 1, algorithm = c("Lloyd"), trace=FALSE)
    return (as.vector(cluster_vector$cluster))
    
  }else if(clustering_type == "single-link"){
    # ~ 3-5 lines
    # Allowed packages for single-link: R-base, stats, dplyr
    # Use euclidean distance for distance calculation (Hint: Look at dist method from stats package)
    # Note 1: Can you use the data_df directly for hclust, or do you need to compute something first?
            # What does 'd' mean in hclust? 
    # Note 2: Does hclust return the clusters assignments directly, or does it return a dendrogram? 
            # Hint 2: Look up the stats package for a method to cut the tree at n_clusters
            # Visualize the dendrogram - paste this dendrogram in your PDF 
    
    clusters <- hclust(dist(data_df, method = "euclidean", diag = FALSE, upper = FALSE, p = 2), method = "single", members = NULL)
    cluster_vector <- cutree(clusters, n_clusters)
    return (cluster_vector)
    
  }else{ #complete link clustering is default
    # ~ 3-5 lines
    # Allowed packages for single-link: R-base, stats, dplyr
    # Use euclidean distance for distance calculation (Hint: Look at dist method from stats package)
    # Note 1: Can you use the data_df directly for hclust, or do you need to compute something first?
    # What does 'd' mean in hclust? 
    # Note 2: Does hclust return the clusters assignments directly, or does it return a dendrogram? 
    # Hint 2: Look up the stats package for a method to cut the dendrogram at n_clusters
    # Visualize the dendrogram - paste this dendrogram in your PDF 
    clusters <- hclust(dist(data_df, method = "euclidean", diag = FALSE, upper = FALSE, p = 2), method = "complete", members = NULL)
    cluster_vector <- cutree(clusters, n_clusters)
    return (cluster_vector)
  }
}



alda_nn <- function(x_train, x_test, y_train, parameter_grid){
  set.seed(100) # this is v. important from reproducibility point of view
  # ~4-7 lines
  # Perform classification using artificial neural networks using the nnet library
  
  # Inputs:
  # x_train: training data frame(4 variables, x1-x4)
  # x_test: test data frame(4 variables, x1-x4)
  # y_train: dependent variable for training data (can be one of the following classes: 0,1,2)
  # parameter_grid: grid of parameters has already been given to you in hw4_checker
  
  # General information
  # Both training data and test data have already been scaled - so you don't need to scale it once again.
  # 1. Use the nnet library 
  # 2. Perform 10 fold cross validation without replacement using caret and nnet
  # 3. Note that I am giving you x_train and x_test as separate variables - do you need to combine them like you did in the previous hws?
  # 4. Perform training using 10-fold cross validation without replacement:
    # 4.1  Use accuracy as your metric
    # 4.2  Use nnet as your method
    # 4.3  Use parameter_grid as your grid
  # 5. Predict using the trained model on the test dataset (x_test)
  
  # Output:
  # A list with two elements, first element = model generated, 
  # second element = predictions on test data (factor)
  
  # NOTE 1: doing as.vector(as.factor(...)) turns it into numeric datatype.
  # NOTE 2: Best way to ensure that your output is of type factor, with the same levels is factor(your_variable, levels=c(specify all your levels here))
  # NOTE 3: If you want to know the best parameters caret chose for you, you can just do print(your trained model using caret), which will print out the final values used for the model
  # NOTE 4: Setting trace = TRUE could help you get insight into how the procedure is done, but set it to FALSE when submitting to reduce clutter 
  # NOTE 5: Remember, there is a penalty for unnecessary print/View function calls in your code.
  # Methods you need to read about:
  # train() (from caret), predict(), nnet()
  
  # allowed packages: R-base, nnet, caret, dplyr
  
  datadf <- data.frame(x=x_train, y=as.factor(y_train))
  colnames(datadf) <- c("x1", "x2","x3","x4","class")
  data_ctrl <- trainControl(method = "cv", number = 10)
  nn_model <- train(class ~ ., 
                   data = datadf,
                   method = "nnet",
                   metric = "Accuracy",
                   trControl = data_ctrl,
                   tuneGrid = parameter_grid)
  y_test <- predict(nn_model, x_test)
  return(list(nn_model,as.factor(y_test)))
}


