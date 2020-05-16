library("moments")#skewness and kurtosis

calc_sample_features<-function(s){
  scaled_s <- scale(s)
  stats <- calc_stats(scaled_s)
  #features <- stats[c("size", "mean", "median", "skewness", "kurtosis", "outliers_minor", "outliers_extream", )]
  return(stats)
}

calc_stats<-function(scaled_s){
  #Calc features
  size <- length(scaled_s)
  mean_ <- round(mean(scaled_s), 5)
  median_ <- round(median(scaled_s),5)
  sd_ <- sd(scaled_s)
  skewness_ <- round(skewness(scaled_s), 5)
  kurtosis_ <- round(kurtosis(scaled_s), 5)
  outliers <- find_outliers(scaled_s)
  
  sigma_1_ratio <- length(which(abs(scaled_s) <= 1 ))/size
  sigma_2_ratio <- length(which(abs(scaled_s) <= 2 ))/size
  sigma_3_ratio <- length(which(abs(scaled_s) <= 3 ))/size
  
  outliers_minor <- length(outliers$minor)
  outliers_extream <- length(outliers$extreme)
  
  stats <- list("size" = size,
                "mean" = mean_,
                "median" = median_,
                "mean_median" = mean_ - median_,
                "sd" = sd_,
                "skewness" = skewness_,
                "kurtosis" = kurtosis_,
                "outliers_minor" = outliers_minor,
                "outliers_extream" = outliers_extream,
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

model_names <- c("rf", "gbm", "svmRadialSigma")

round <- "baseline"

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

data_dir <- "D:/master/ASDS860 - Thesis/thesis/simulation/data/baseline"
data_files_dir <- paste(data_dir, "/files", sep = "")
dir.create(data_dir, showWarnings = FALSE)
dir.create(data_files_dir, showWarnings = FALSE)
data_file <- paste(data_dir, "/data.csv", sep = "")


