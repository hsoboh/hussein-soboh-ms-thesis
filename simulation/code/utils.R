library("moments")#skewness and kurtosis

calc_stats<-function(sample, scaled_sample = NULL){
  if(do_scaling == TRUE){
    if(is.null(scaled_sample)){
      s <- scale(sample)   
    }else{
      s <- scaled_sample
    }
  }else{
    s <- sample
  }
  #Calc features
  size <- length(s)
  mean_ <- round(mean(s), 5)
  median_ <- round(median(s),5)
  sd_ <- sd(s)
  skewness_ <- round(skewness(s), 5)
  kurtosis_ <- round(kurtosis(s), 5)
  outliers <- find_outliers(s)
  
  sigma_1_ratio <- length(which(abs(s - mean_) <= 1*sd_ ))/size
  sigma_2_ratio <- length(which(abs(s - mean_) <= 2*sd_ ))/size
  sigma_3_ratio <- length(which(abs(s - mean_) <= 3*sd_ ))/size
  
  outliers_minor_ratio <- length(outliers$minor) / size
  outliers_extream_ratio <- length(outliers$extreme) / size
  
  stats <- list("size" = size,
                "mean" = mean_,
                "median" = median_,
                "mean_median_diff" = abs(mean_ - median_) / sd_,
                "sd" = sd_,
                "skewness" = skewness_,
                "kurtosis" = kurtosis_,
                "outliers_minor_ratio" = outliers_minor_ratio,
                "outliers_extream_ratio" = outliers_extream_ratio,
                "sigma_1_ratio" = sigma_1_ratio,
                "sigma_2_ratio" = sigma_2_ratio,
                "sigma_3_ratio" = sigma_3_ratio)
  return(stats)
} 

find_outliers<-function(data){
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  
  minor_threshold_upper = (iqr * 1.5) + upperq
  minor_threshold_lower = lowerq - (iqr * 1.5)
  
  extreme_threshold_upper = (iqr * 3) + upperq
  extreme_threshold_lower = lowerq - (iqr * 3)
  
  outliers = list()
  outliers[["minor"]] = data[(data < minor_threshold_lower) | (data > minor_threshold_upper )]
  outliers[["extreme"]] = data[(data < extreme_threshold_lower) | (data > extreme_threshold_upper )]
  
  return(outliers)
}

model_names <- c("rf", "gbm", "svmRadial") 

round <- "2_tuned"

do_scaling <- TRUE

out_dir <- paste("./../out/", round, sep = "")
models_dir <- paste(out_dir, "/models", sep = "")
stats_dir <- paste(out_dir, "/stats", sep = "")
power_dir <- paste(out_dir, "/power", sep = "")

dir.create(out_dir, showWarnings = FALSE)
dir.create(models_dir, showWarnings = FALSE)
dir.create(stats_dir, showWarnings = FALSE)
dir.create(power_dir, showWarnings = FALSE)

stats_file <- paste(stats_dir, "/statistics.csv", sep = "")
summary_stats_file <- paste(stats_dir, "/summary_statistics.csv", sep = "")
power_thresholds_file <- paste(stats_dir, "/power_thresholds.csv", sep = "")

data_dir <- "D:/master/ASDS860 - Thesis/thesis/simulation/data/scaled"
data_files_dir <- paste(data_dir, "/files", sep = "")
dir.create(data_dir, showWarnings = FALSE)
dir.create(data_files_dir, showWarnings = FALSE)
data_file <- paste(data_dir, "/data.csv", sep = "")


