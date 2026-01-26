library(rpart)
library(nnet)
library(randomForest)

theme_update(plot.title = element_text(hjust=0.5, size=20),
             plot.subtitle = element_text(hjust=0.5, size=15),
             panel.background = element_rect(fill = "white"),
             axis.line = element_line(color = "darkgrey"),
             panel.grid.major = element_line(color="darkgrey"),
             panel.grid.minor = element_line(color="lightgrey")
             )

#' Categorization plot
#'
#' @param user_fun user selected function for categorization
#' @param stat_item percentage or number of persons
#' @param gender total, man, or woman
#' @param variables user selected variables
#'
#' @returns
#' @export
#'
#' @examples
cat_plot <- function(user_fun, stat_item, gender=NULL, variables){
  # get data based on statistic
  if(stat_item == "Percentage of persons"){
    d1 <- percentage_fact[,-2]
  }else{
    d1 <- population_fact[,-2]
  }

  all_variables <- c(variables, gender, "ranks")
  d2 <- d1[,which(names(d1) %in% all_variables)]

  # call function user called with data required
  if(user_fun == "class_tree"){
    fun_data <- regression_tree(d2)
  }
  if(user_fun == "mult_reg"){
    fun_data <- multinomial_regression(d2)
  }
  if(user_fun == "rand_for"){
    fun_data <- random_forest(d2)
  }
  return(fun_data)
}



#' Split data - function to split data into training and validation sets
#'
#' @param N number of rows in data frame
#'
#' @returns training and validation sets for data
#' @export
#'
#' @examples
split_data <- function(N){
  # split data
  training <- sample(1:N, size = N*0.75)
  validation <- setdiff(1:N, training)

  return(list(training=training, validation=validation))
}



#' Create labels for plot
#'
#' @param tab1 table of training results
#' @param tab2 table of validation results
#'
#' @returns table with plot labels and locations
#'
#' @examples
#'

pl_lab <- function(tab1, tab2){
  # create 9x2 table
  just <- expand.grid(hjust = c(1:3), vjust=c(1:3))
  just$label <- NA

  # add validation and training data
  for(r in 1:nrow(just)){
    tmp1 <- tab1[just$hjust[r], just$vjust[r]]
    tmp2 <- tab2[just$hjust[r], just$vjust[r]]
    just$label[r] <- paste0("Validation: ", tmp2, "\n Training: ", tmp1)
  }
  return(just)
}



#' Calculate prediction accuracy
#'
#' @param tab square table
#'
#' @returns accuracy of predictions
#'
#' @examples
pred_accuracy <- function(tab){
  return(sum(diag(tab)) / sum(tab))
}


#' Generate plot showing actual and predicted values
#'
#' @param obj data frame of original data
#' @param tab1 training accuracy table
#' @param tab2 validation accuracy table
#' @param set_id set of training and validation data
#' @param t1 title for plot
#'
#' @returns p1 ggplot of given data
#'
#' @examples
generate_plot <- function(obj, tab1, tab2, set_id, t1, pred1, pred2){
  # set plot labels
  just <- pl_lab(tab1, tab2)
  subtitle <- paste0("Training Accuracy: ",
                     round(pred_accuracy(tab1),3),
                     ", Validation Accuracy: ",
                     round(pred_accuracy((tab2)), 3))

  # generate plot
  p1 <- ggplot() +
    geom_point(aes(x=obj$ranks[set_id$training], y=pred1),
               color = "cadetblue1", position = "jitter") +
    geom_point(aes(x=obj$ranks[set_id$validation], y=pred2),
               color = "gold", position = "jitter") +
    xlab("Actual Rank") + ylab("Predicted Rank") +
    annotate("text", x=just$hjust, y=just$vjust, label=just$label) +
    labs(title = t1, subtitle = subtitle)

  return(p1)
}

#' Regression tree categorizations
#'
#' @param obj data frame containing user requested variables
#'
#' @returns
#' @export
#'
#' @examples
regression_tree <- function(obj){
  # split data
  N <- nrow(obj)
  set_id <- split_data(N)

  # fit training data
  fit <- rpart(formula = ranks ~ .,
               data = obj,
               subset = set_id$training)
  train_pred <- predict(fit, type = "class")
  train_tab <- table(actual=obj$ranks[set_id$training],
                     predicted=train_pred)

  # fit validation data
  val_pred <- predict(fit, type="class",
                      newdata=obj[set_id$validation,])
  val_tab <- table(actual=obj$ranks[set_id$validation],
                   predicted=val_pred)

  # get plot
  t1 <- "Categorization using Regression Trees"
  return(generate_plot(obj, train_tab, val_tab, set_id, t1, train_pred,val_pred))
}

#' Multinomial Regression plot generator
#'
#' @param obj data frame containing user requested variables
#'
#' @returns
#' @export
#'
#' @examples
multinomial_regression <- function(obj){
  # split data
  N <- nrow(obj)
  set_id <- split_data(N)

  # fit training data
  fit <- multinom(ranks ~ ., data = obj, subset = set_id$training)
  train_pred <- predict(fit, type = "class")
  train_tab <- table(actual=obj$ranks[set_id$training],
                     predicted=train_pred)

  # fit validation data
  val_pred <- predict(fit, type="class",
                      newdata=obj[set_id$validation,])
  val_tab <- table(actual=obj$ranks[set_id$validation],
                   predicted=val_pred)

  # get plot
  t1 <- "Categorization using Multinomial Regression"
  return(generate_plot(obj, train_tab, val_tab, set_id, t1, train_pred,val_pred))
}

#' Random forest classification
#'
#' @param obj data frame containing user requested variables
#'
#' @returns
#' @export
#'
#' @examples
random_forest <- function(obj){
  # split data
  N <- nrow(obj)
  set_id <- split_data(N)

  # rename "Total, all persons" column if present
  if("Total, all persons" %in% names(obj)){
    colnames(obj)[which(names(obj) == "Total, all persons")] <- "Total"
  }

  # fit training data
  fit_rf <- randomForest(formula = ranks ~ ., data = obj[set_id$training,], importance = TRUE)
  train_pred <- predict(fit_rf, type = "class")
  val_pred <- predict(fit_rf, type = "class", newdata = obj[set_id$validation,])

  # summary statistics for accuracy
  train_tab <- table(actual=obj$ranks[set_id$training],
                     predicted=train_pred)
  val_tab <- table(actual=obj$ranks[set_id$validation],
                   predicted=val_pred)

  # get plot
  t1 <- "Categorization using Random Forests"
  return(generate_plot(obj,train_tab,val_tab,set_id,t1,train_pred,val_pred))
}
