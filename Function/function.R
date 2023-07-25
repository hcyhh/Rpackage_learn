# ------------------------------------------------------------------#
#                        常用函数整理
#
#
#                                                        by 何从源
# ------------------------------------------------------------------#


# 查看缺失数据函数 ----------------------------------------------------------------
QS <- function(data){
  columns <- c(colnames(data))
  show <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(data) = columns
  colnames(show) <- names(data)
  for (i in 1:ncol(data)) {
    show[1,i] <-sum(is.na(data[colnames(data)[i]]))
  }
  show
}

# 初步统计描述 ------------------------------------------------------------------
stat_desc_2lists<- function(dat) {
  # Initialize empty lists to store results
  num_results <- list()
  char_results <- list()
  
  # Get the names of variables in the dataset
  var_names <- names(dat)
  
  # Loop over each variable
  for (var in var_names) {
    # Get the type of the variable
    var_type <- class(dat[[var]])
    
    # Perform statistical description based on variable type
    if (var_type == "numeric") {
      num_results[[var]] <- summary(dat[[var]])
    } else if (var_type %in% c("factor", "character")) {
      if (length(table(dat[[var]])) > 10) {
        char_results[[var]] <- "Abnormal Variable"
      } else {
        char_results[[var]] <- table(dat[[var]])
      }
    }
  }
  
  # Return the results lists
  return(list(Numeric = num_results, Character = char_results))
}