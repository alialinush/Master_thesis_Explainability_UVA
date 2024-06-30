
# SURVEY - DESCRIPTIVE AND INFERENTIAL STATISTICS - SCRIPT

library(tidyverse)
library(readxl)
library(ggbeeswarm)

df <- read_excel ("/Users/alinabaciu/Desktop/uva_master_thesis/data/processed/survey_data.xlsx")

qrange <- function(r){
  return(function(x) list(ymin=quantile(x,(100-r)/200), y=quantile(x,.5),
                          ymax=quantile(x,1-(100-r)/200)))} 

# First question: trust and CP certain level

percentage_sampled_yes <- sum(df[["first_question"]] == "Yes") / nrow(df) * 100
percentage_sampled_no  <- sum(df[["first_question"]] == "No") / nrow(df) * 100

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["first_question"]])
}

difference_in_ratios <- function() {
  
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled) * 100
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled) * 100
  
  ratio_sampled <- percentage_sampled_yes / (100 - percentage_sampled_yes)
  ratio_sampled
}

bs_dist <- tibble( d_in_means=replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

mean_sd <- mean(filtered_bs_dist[["d_in_means"]])

mean(filtered_bs_dist[["d_in_means"]] ) # 1.63
sd(filtered_bs_dist[["d_in_means"]] ) 

plot <-ggplot(filtered_bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
stat_summary(fun = "mean",col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip() 

ggplot(filtered_bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # 0.57
upper <- plot_data$ymax[1] # 3.4
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

-----------------------------------------------

observed_yes_ratio <- sum(df[["first_question"]] == "Yes") / nrow(df)
observed_no_ratio <- sum(df[["first_question"]] == "No") / nrow(df)
observed_diff <- abs(observed_yes_ratio - observed_no_ratio)

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["first_question"]])
}

difference_in_ratios <- function() {
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled)
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled)
  abs(percentage_sampled_yes - percentage_sampled_no)
}

bs_dist <- tibble(d_in_means = replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

p_value <- mean(filtered_bs_dist$d_in_means >= observed_diff)

cat("p-value:", p_value, "\n")

# Second question: comprehension and CP certain level

percentage_sampled_yes <- sum(df[["second_question"]] == "Yes") / nrow(df) * 100
percentage_sampled_no  <- sum(df[["second_question"]] == "No") / nrow(df) * 100

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["second_question"]])
}

difference_in_ratios <- function() {
  
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled) * 100
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled) * 100
  
  ratio_sampled <- percentage_sampled_yes / (100 - percentage_sampled_yes)
  ratio_sampled
}

bs_dist <- tibble(d_in_means=replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

mean(filtered_bs_dist[["d_in_means"]] ) # 4.24
sd(filtered_bs_dist[["d_in_means"]] )

plot <-ggplot(filtered_bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(fun = "mean",col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # 1.44
upper <- plot_data$ymax[1] # 10
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

----------------------------------------------

observed_yes_ratio <- sum(df[["second_question"]] == "Yes") / nrow(df)
observed_no_ratio <- sum(df[["second_question"]] == "No") / nrow(df)
observed_diff <- abs(observed_yes_ratio - observed_no_ratio)

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["second_question"]])
}

difference_in_ratios <- function() {
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled)
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled)
  abs(percentage_sampled_yes - percentage_sampled_no)
}

bs_dist <- tibble(d_in_means = replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

p_value <- mean(filtered_bs_dist$d_in_means >= observed_diff)

cat("p-value:", p_value, "\n")

# Third question: transparency and CP certain level

percentage_sampled_yes <- sum(df[["third_question"]] == "Yes") / nrow(df) * 100
percentage_sampled_no  <- sum(df[["third_question"]] == "No") / nrow(df) * 100

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["third_question"]])
}

difference_in_ratios <- function() {
  
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled) * 100
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled) * 100
  
  ratio_sampled <- percentage_sampled_yes / (100 - percentage_sampled_yes)
  ratio_sampled
}

bs_dist <- tibble( d_in_means=replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

mean(filtered_bs_dist[["d_in_means"]]) # 1.67
sd(filtered_bs_dist[["d_in_means"]] ) 

plot <-ggplot(filtered_bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(fun = "mean",col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # 0.57
upper <- plot_data$ymax[1] # 4.5
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

-----------------------------------------------
  
observed_yes_ratio <- sum(df[["third_question"]] == "Yes") / nrow(df)
observed_no_ratio <- sum(df[["third_question"]] == "No") / nrow(df)
observed_diff <- abs(observed_yes_ratio - observed_no_ratio)

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["third_question"]])
}

difference_in_ratios <- function() {
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled)
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled)
  abs(percentage_sampled_yes - percentage_sampled_no)
}

bs_dist <- tibble(d_in_means = replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

p_value <- mean(filtered_bs_dist$d_in_means >= observed_diff)

cat("p-value:", p_value, "\n")

# Fourth question: trust and CP uncertain level

percentage_sampled_yes <- sum(df[["fourth_question"]] == "Yes") / nrow(df) * 100
percentage_sampled_no  <- sum(df[["fourth_question"]] == "No") / nrow(df) * 100

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["fourth_question"]])
}

difference_in_ratios <- function() {
  
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled) * 100
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled) * 100
  
  ratio_sampled <- percentage_sampled_no / (100 - percentage_sampled_no)
  ratio_sampled
}

bs_dist <- tibble( d_in_means=replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

mean(filtered_bs_dist[["d_in_means"]]) # 16.1
sd(filtered_bs_dist[["d_in_means"]])

plot <-ggplot(filtered_bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(fun = "mean",col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # 6.33
upper <- plot_data$ymax[1] # 21
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

-----------------------------------------------
  
observed_yes_ratio <- sum(df[["fourth_question"]] == "Yes") / nrow(df)
observed_no_ratio <- sum(df[["fourth_question"]] == "No") / nrow(df)
observed_diff <- abs(observed_yes_ratio - observed_no_ratio)

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["fourth_question"]])
}

difference_in_ratios <- function() {
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled)
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled)
  abs(percentage_sampled_yes - percentage_sampled_no)
}

bs_dist <- tibble(d_in_means = replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

p_value <- mean(filtered_bs_dist$d_in_means >= observed_diff)

cat("p-value:", p_value, "\n")

# Fifth question: comprehension and CP uncertain level

percentage_sampled_yes <- sum(df[["fifth_question"]] == "Yes") / nrow(df) * 100
percentage_sampled_no  <- sum(df[["fifth_question"]] == "No") / nrow(df) * 100

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["fifth_question"]])
}

difference_in_ratios <- function() {
  
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled) * 100
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled) * 100
  
  ratio_sampled <- percentage_sampled_no / (100 - percentage_sampled_no)
  ratio_sampled
}

bs_dist <- tibble( d_in_means=replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

mean(filtered_bs_dist[["d_in_means"]]) # 3.18
sd(filtered_bs_dist[["d_in_means"]])

plot <-ggplot(filtered_bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(fun = "mean",col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # 1.2
upper <- plot_data$ymax[1] # 10
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

-----------------------------------------------

observed_yes_ratio <- sum(df[["fifth_question"]] == "Yes") / nrow(df)
observed_no_ratio <- sum(df[["fifth_question"]] == "No") / nrow(df)
observed_diff <- abs(observed_yes_ratio - observed_no_ratio)

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["fifth_question"]])
}

difference_in_ratios <- function() {
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled)
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled)
  abs(percentage_sampled_yes - percentage_sampled_no)
}

bs_dist <- tibble(d_in_means = replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

p_value <- mean(filtered_bs_dist$d_in_means >= observed_diff)

cat("p-value:", p_value, "\n")

# Sixth question: transparency and CP uncertain level

percentage_sampled_yes <- sum(df[["sixth_question"]] == "Yes") / nrow(df) * 100
percentage_sampled_no  <- sum(df[["sixth_question"]] == "No") / nrow(df) * 100

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["sixth_question"]])
}

difference_in_ratios <- function() {
  
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled) * 100
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled) * 100
  
  ratio_sampled <- percentage_sampled_yes / (100 - percentage_sampled_yes)
  ratio_sampled
}

bs_dist <- tibble( d_in_means=replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

mean(filtered_bs_dist[["d_in_means"]]) # 1.39
sd(filtered_bs_dist[["d_in_means"]])

plot <-ggplot(filtered_bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(fun = "mean",col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # 0.57
upper <- plot_data$ymax[1] # 3.4
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

----------------------------------------------
  
observed_yes_ratio <- sum(df[["sixth_question"]] == "Yes") / nrow(df)
observed_no_ratio <- sum(df[["sixth_question"]] == "No") / nrow(df)
observed_diff <- abs(observed_yes_ratio - observed_no_ratio)

sample <- function(){ 
  return(slice_sample(df, n = nrow(df), replace = TRUE)[["sixth_question"]])
}

difference_in_ratios <- function() {
  sampled <- sample()
  percentage_sampled_yes <- sum(sampled == "Yes") / length(sampled)
  percentage_sampled_no  <- sum(sampled == "No") / length(sampled)
  abs(percentage_sampled_yes - percentage_sampled_no)
}

bs_dist <- tibble(d_in_means = replicate(1000, difference_in_ratios()))

filtered_bs_dist <- bs_dist %>% 
  filter(!is.infinite(d_in_means) & !is.nan(d_in_means))

p_value <- mean(filtered_bs_dist$d_in_means >= observed_diff)

cat("p-value:", p_value, "\n")
