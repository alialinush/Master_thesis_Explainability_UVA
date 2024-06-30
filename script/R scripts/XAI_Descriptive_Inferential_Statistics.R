
# XAI - DESCRIPTIVE AND INFERENTIAL STATISTICS - SCRIPT

# XAI - SHAP & LIME

library(tidyverse)
library(ggbeeswarm)

df <- read_csv ("/Users/alinabaciu/Desktop/master_thesis_uva/data/processed/merged_df_part1.csv")

qrange <- function(r){
  return(function(x) list(ymin=quantile(x,(100-r)/200), y=quantile(x,.5),
                          ymax=quantile(x,1-(100-r)/200)))} 

df_uncertain = filter(df,df[['level']]=='uncertain')
df_certain = filter(df,df[['level']]=='certain')

xbar_uncertain <-mean(df_uncertain[["percentage_positive"]])
xbar_certain <- mean(df_certain[["percentage_positive"]])

difference = xbar_certain - xbar_uncertain

sample_uncertain <- function(){ return(
  slice_sample(df_uncertain,n=nrow(df_uncertain),replace=TRUE)[["percentage_positive"]]) }
sample_certain <- function(){ return(
  slice_sample(df_certain,n=nrow(df_certain),replace=TRUE)[["percentage_positive"]]) }

bs_dist <- tibble( d_in_means=replicate(1000,mean( sample_certain() ) - mean( sample_uncertain() ) ) )
mean( bs_dist[["d_in_means"]] )
sd( bs_dist[["d_in_means"]] )

plot <-ggplot(bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # 4.03
upper <- plot_data$ymax[1] # 6.03
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

df_uncertain["percentage_positive"] <- df_uncertain[["percentage_positive"]]-xbar_uncertain+xbar_certain

bs_dist[["extreme"]] <- bs_dist[["d_in_means"]] > (xbar_certain-xbar_uncertain) |
bs_dist[["d_in_means"]] < - (xbar_certain-xbar_uncertain)

p_value = sum(bs_dist>(xbar_certain-xbar_uncertain))/1000
p_value 

# XAI - GUIDED PROTOTYPES & CEML

df <- read_csv ("/Users/alinabaciu/Desktop/master_thesis_uva/data/processed/merged_df_part2.csv")

df_uncertain = filter(df,df[['level']]=='uncertain')
df_certain = filter(df,df[['level']]=='certain')

xbar_uncertain <-mean(df_uncertain[["percentage_higher"]])
xbar_certain <- mean(df_certain[["percentage_higher"]])

difference = xbar_uncertain - xbar_certain

sample_uncertain <- function(){ return(
  slice_sample(df_uncertain,n=nrow(df_uncertain),replace=TRUE)[["percentage_higher"]]) }
sample_certain <- function(){ return(
  slice_sample(df_certain,n=nrow(df_certain),replace=TRUE)[["percentage_higher"]]) }

bs_dist <- tibble( d_in_means=replicate(1000,mean( sample_uncertain() ) - mean( sample_certain() ) ) )
mean( bs_dist[["d_in_means"]] )
sd( bs_dist[["d_in_means"]] )

plot <-ggplot(bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # - 0.27
upper <- plot_data$ymax[1] # 6.44
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

df_uncertain["percentage_higher"] <- df_uncertain[["percentage_higher"]]-xbar_uncertain+xbar_certain

bs_dist[["extreme"]] <- bs_dist[["d_in_means"]] > (xbar_uncertain-xbar_certain) |
  bs_dist[["d_in_means"]] < - (xbar_uncertain-xbar_certain)

p_value = sum(bs_dist>(xbar_uncertain-xbar_certain))/1000
p_value 

df <- read_csv ("/Users/alinabaciu/Desktop/master_thesis_uva/data/processed/merged_df_part2.csv")

df_uncertain = filter(df,df[['level']]=='uncertain')
df_certain = filter(df,df[['level']]=='certain')

xbar_uncertain <-mean(df_uncertain[["percentage_lower"]])
xbar_certain <- mean(df_certain[["percentage_lower"]])

difference = xbar_uncertain - xbar_certain

sample_uncertain <- function(){ return(
  slice_sample(df_uncertain,n=nrow(df_uncertain),replace=TRUE)[["percentage_lower"]]) }
sample_certain <- function(){ return(
  slice_sample(df_certain,n=nrow(df_certain),replace=TRUE)[["percentage_lower"]]) }

bs_dist <- tibble( d_in_means=replicate(1000,mean( sample_uncertain() ) - mean( sample_certain() ) ) )
mean( bs_dist[["d_in_means"]] )
sd( bs_dist[["d_in_means"]] )

plot <-ggplot(bs_dist,aes(x="",y=d_in_means))+geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar") + xlab("") + coord_flip()

plot_data <- ggplot_build(plot)$data[[2]]

lower <- plot_data$ymin[1] # - 0.27
upper <- plot_data$ymax[1] # 6.44
cat("Lower Confidence Interval:", lower, "\n")
cat("Upper Confidence Interval:", upper, "\n")

df_uncertain["percentage_lower"] <- df_uncertain[["percentage_lower"]]-xbar_uncertain+xbar_certain

bs_dist[["extreme"]] <- bs_dist[["d_in_means"]] > (xbar_uncertain-xbar_certain) |
  bs_dist[["d_in_means"]] < - (xbar_uncertain-xbar_certain)

p_value = sum(bs_dist>(xbar_uncertain-xbar_certain))/1000
p_value 



