###### EXPLORATION ######

mypath <- "C:/Users/milus/Downloads/thesis 101/exploration"
filenames <- list.files(path=mypath, full.names=TRUE)

#read the files in as plaintext
csv_list <- lapply(filenames , readLines)

#remove the header from all but the first file
csv_list[-1] <- sapply(csv_list[-1], "[", 2)

#unlist to create a character vector
csv_list <- unlist(csv_list)

#write the csv as one single file
writeLines(text=csv_list,
           con="all_exploration_csvs_combined.csv")


#read the csv as one single file
data_exploration <- read.csv("all_exploration_csvs_combined.csv")



data <- read.csv("teo_results.csv", header = T)
ps <- c(0.1, 0.2, 0.4, 1)

means <- data.frame()
for(m in 1:2) {
  for(j in 1:3) {
    for(i in 1:4) {
      
      x <- data[which (data[2] == ps[j] & data[3] == (m-1)), 3+i]
      means[j+(m-1)*3, i] <- mean(x)
      
    }
    
  }
  
}
colnames(means) <- c(paste("Trial", 1:4))
rownames(means) <- c(paste("Control", ps), paste("Yoked", ps))

x <- c(0, 30, 70)
var(x)
















# graphs/visualise
# confirmatory vs exploratory analyses
# check the APA guidelines for figures
# check assumptions tests for main analyses: e.g. Levene's test and Shapiro-Wilk test?
