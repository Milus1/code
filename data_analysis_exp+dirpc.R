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

#removing incomplete responses
data_exploration <- data_exploration[-which(data_exploration[ ,5] == "CTRL"), ]
data_exploration <- data_exploration[-which(data_exploration[ ,5] == "YOK"), ]

# removing participants who didn't complete the whole study above the exploration task



#creating a data frame with means of exploration per participant per block of trials
explo <- data.frame()
index <- list(a = seq(from = 4, to = 52, by = 2), b = seq(from = 54, to = 102, by = 2), c = seq(from = 104, to = 152, by = 2), d = seq(from = 154, to = 202, by = 2))

for(i in 1:nrow(data_exploration)) { 
  
  for(j in 1:4){ 
    explo[i, j] <- mean(as.numeric(data_exploration[i, index[[j]]]))
  } 
  
}
colnames(explo) <- c(paste("Explo Block", 1:4))
data_exploration <- cbind(data_exploration, explo)





#computing mean and standard deviation of exploration rate per condition per block of trials
ps <- c(0.1, 0.2, 0.4, 1)
cond <- c("CTRL", "YOK")


# for the reward frequency condition p = 0.1
means_p_0.1 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
      means_p_0.1[j, i] <- mean(data_exploration[which(data_exploration[, 2] == ps[1] & data_exploration[, 3] == cond[j]), 204+i])
  }
} 

colnames(means_p_0.1) <- c(paste("Explo Block", 1:4))
rownames(means_p_0.1) <- c(paste("Control", ps[1]), paste("Yoked", ps[1]))



# for the reward frequency condition p = 0.2
means_p_0.2 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    means_p_0.2[j, i] <- mean(data_exploration[which(data_exploration[, 2] == ps[2] & data_exploration[, 3] == cond[j]), 204+i])
  }
} 
colnames(means_p_0.2) <- c(paste("Explo Block", 1:4))
rownames(means_p_0.2) <- c(paste("Control", ps[2]), paste("Yoked", ps[2]))


# for the reward frequency condition p = 0.4
means_p_0.4 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    means_p_0.4[j, i] <- mean(data_exploration[which(data_exploration[, 2] == ps[3] & data_exploration[, 3] == cond[j]), 204+i])
  }
} 
colnames(means_p_0.4) <- c(paste("Explo Block", 1:4))
rownames(means_p_0.4) <- c(paste("Control", ps[3]), paste("Yoked", ps[3]))


# for the reward frequency condition p = 1
means_p_1 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    means_p_1[j, i] <- mean(data_exploration[which(data_exploration[, 2] == ps[4] & data_exploration[, 3] == cond[j]), 204+i])
  }
} 
colnames(means_p_1) <- c(paste("Explo Block", 1:4))
rownames(means_p_1) <- c(paste("Control", ps[4]), paste("Yoked", ps[4]))


means_p_0.1
means_p_0.2
means_p_0.4
means_p_1


par(mfrow = c(2, 2))

# p = 0.1
plot(x = 1:4, y = means_p_0.1[2, ], type = "l", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0, 1), main = "p reward = 0.1", lwd = 2, xaxt = "n")
axis(side=1, at=seq(1,4,1))
lines(x = 1:4, y = means_p_0.1[1, ], col = "green", lwd = 2)
legend(x = "bottomleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)

lines(x = rep(1:4, each = 2), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208][[4]]), type = "p", col = "red", lwd = 2)

lines(x = rep(1:4, each = 6), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[1]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[1]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[1]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[1]), 205:208][[4]]), col = "green", lwd = 2, type = "p")



# p = 0.2
plot(x = 1:4, y = means_p_0.2[2, ], type = "l", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0, 1), main = "p reward = 0.2", lwd = 2, xaxt = "n")
axis(side=1, at=seq(1,4,1))
lines(x = 1:4, y = means_p_0.2[1, ], col = "green", lwd = 2)
legend(x = "bottomleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)

lines(x = rep(1:4, each = 6), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[2]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[2]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[2]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[2]), 205:208][[4]]), type = "p", col = "red", lwd = 2)

lines(x = rep(1:4, each = 3), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[1]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[1]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[1]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[1]), 205:208][[4]]), col = "green", lwd = 2, type = "p")





# p = 0.4
plot(x = 1:4, y = means_p_0.4[2, ], type = "l", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0, 1), main = "p reward = 0.4", lwd = 2, xaxt = "n")
axis(side=1, at=seq(1,4,1))
lines(x = 1:4, y = means_p_0.4[1, ], col = "green", lwd = 2)
legend(x = "bottomleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)

lines(x = rep(1:4, each = 5), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[2]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[2]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[2]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[2]), 205:208][[4]]), type = "p", col = "red", lwd = 2)

lines(x = rep(1:4, each = 8), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[1]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[1]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[1]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[1]), 205:208][[4]]), col = "green", lwd = 2, type = "p")



# p = 1
plot(x = 1:4, y = means_p_1[2, ], type = "l", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0, 1), main = "p reward = 1", lwd = 2, xaxt = "n")
axis(side=1, at=seq(1,4,1))
lines(x = 1:4, y = means_p_1[1, ], col = "green", lwd = 2)
legend(x = "bottomleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)

lines(x = rep(1:4, each = 7), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[2]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[2]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[2]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[2]), 205:208][[4]]), type = "p", col = "red", lwd = 2)

lines(x = rep(1:4, each = 8), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[1]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[1]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[1]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[1]), 205:208][[4]]), col = "green", lwd = 2, type = "p")






###### DIRECT MEASURE OF PERCEIVED CONTROL ######

install.packages("plyr")
library(plyr)
pc1path <- "C:/Users/miluš/Downloads/thesis 101/directPC" 
data_dir_pc <- ldply(list.files(path=pc1path, full.names=TRUE), read.csv, header=TRUE)

# recode Strongly Disagree to 1 and Strongly Agree to 10

for(j in 1:ncol(data_dir_pc)) {
  for(i in 1:nrow(data_dir_pc)) {
    if(data_dir_pc[i, j] == "Strongly Disagree") {
      data_dir_pc[i, j] <- 1
    } else if (data_dir_pc[i, j] == "Strongly Agree") {
      data_dir_pc[i, j] <- 10
    }
  }
}


# covert to numeric
for(i in 1:4) {
  data_dir_pc[, 1+i] <- as.numeric(data_dir_pc[, 1+i])
}


# merging the exploration data with the direct measure of PC data
remove_rows <- numeric()

for(i in 1:nrow(data_exploration)) {
  if(sum(data_exploration[i, 1] == data_dir_pc[, 1]) == 0) {
    remove_rows <- c(remove_rows, i)
  } 
}

data_exploration <- data_exploration[-remove_rows, ]


data_exploration[ ,1] <- sort(data_exploration[ ,1], decreasing = F)
data_dir_pc[ ,1] <- sort(data_dir_pc[ ,1], decreasing = F)

exp_dirpc <- cbind(data_exploration, data_dir_pc[ ,2:5])

head(data_exploration[, 1]) == head(data_dir_pc[, 1])



# knowing how many people from each condition participated in the study
ps <- c(0.1, 0.2, 0.4, 1)
cond <- c("CTRL", "YOK")
sample <- data.frame()

for(j in 1:4) {
  for(i in 1:2) {
    sample[i, j] <- sum(exp_dirpc[, 2] == ps[j] & exp_dirpc[, 3] == cond[i])
    
  }
}
colnames(sample) <- c(ps)
rownames(sample) <- c(cond)




# mean of direct measure of perceived controllability per condition per block of trials

# for the reward frequency condition p = 0.1
dir_p_0.1 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    dir_p_0.1[j, i] <- mean(exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[j]), 208+i])
  }
} 

for(i in 1:2) {
  for(j in 1:2) {
    dir_p_0.1[i, j] <- mean(as.numeric(dir_p_0.1[i, (2*j-1):(2*j)]))
  }
}
dir_p_0.1 <- dir_p_0.1[1:2, 1:2]
colnames(dir_p_0.1) <- c(paste("Half", 1:2))
rownames(dir_p_0.1) <- c(paste("Control", ps[1]), paste("Yoked", ps[1]))



# for the reward frequency condition p = 0.2
dir_p_0.2 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    dir_p_0.2[j, i] <- mean(exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[j]), 208+i])
  }
} 

for(i in 1:2) {
  for(j in 1:2) {
    dir_p_0.2[i, j] <- mean(as.numeric(dir_p_0.2[i, (2*j-1):(2*j)]))
  }
}
dir_p_0.2 <- dir_p_0.2[1:2, 1:2]
colnames(dir_p_0.2) <- c(paste("Half", 1:2))
rownames(dir_p_0.2) <- c(paste("Control", ps[2]), paste("Yoked", ps[2]))


# for the reward frequency condition p = 0.4
dir_p_0.4 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    dir_p_0.4[j, i] <- mean(exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[j]), 208+i])
  }
} 

for(i in 1:2) {
  for(j in 1:2) {
    dir_p_0.4[i, j] <- mean(as.numeric(dir_p_0.4[i, (2*j-1):(2*j)]))
  }
}
dir_p_0.4 <- dir_p_0.4[1:2, 1:2]
colnames(dir_p_0.4) <- c(paste("Half", 1:2))
rownames(dir_p_0.4) <- c(paste("Control", ps[3]), paste("Yoked", ps[3]))




# for the reward frequency condition p = 1
dir_p_1 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    dir_p_1[j, i] <- mean(exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[j]), 208+i])
  }
} 

for(i in 1:2) {
  for(j in 1:2) {
    dir_p_1[i, j] <- mean(as.numeric(dir_p_1[i, (2*j-1):(2*j)]))
  }
}
dir_p_1 <- dir_p_1[1:2, 1:2]
colnames(dir_p_1) <- c(paste("Half", 1:2))
rownames(dir_p_1) <- c(paste("Control", ps[4]), paste("Yoked", ps[4]))


dir_p_0.1
dir_p_0.2
dir_p_0.4
dir_p_1


par(mfrow = c(2, 2))

# p = 0.1
plot(x = 1:2, y = dir_p_0.1[2, ], type = "l", xlab = "Half", ylab = "Explicit PC", col = "red", ylim = c(0, 10), main = "p reward = 0.1", lwd = 1.5, xaxt = "n")
lines(x = 1:2, y = dir_p_0.1[2, ], col = "red", lwd = 2, type = "p")
axis(side=1, at=seq(1,2,1))
lines(x = 1:2, y = dir_p_0.1[1, ], col = "green", lwd = 1.5, type = "l")
lines(x = 1:2, y = dir_p_0.1[1, ], col = "green", lwd = 2, type = "p")
legend(x = "topleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)


# p = 0.2
plot(x = 1:2, y = dir_p_0.2[2, ], type = "l", xlab = "Half", ylab = "Explicit PC", col = "red", ylim = c(0, 10), main = "p reward = 0.2", lwd = 1.5, xaxt = "n")
lines(x = 1:2, y = dir_p_0.2[2, ], col = "red", lwd = 2, type = "p")
axis(side=1, at=seq(1,2,1))
lines(x = 1:2, y = dir_p_0.2[1, ], col = "green", lwd = 1.5, type = "l")
lines(x = 1:2, y = dir_p_0.2[1, ], col = "green", lwd = 2, type = "p")
legend(x = "topleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)


# p = 0.4
plot(x = 1:2, y = dir_p_0.4[2, ], type = "l", xlab = "Half", ylab = "Explicit PC", col = "red", ylim = c(0, 10), main = "p reward = 0.4", lwd = 1.5, xaxt = "n")
lines(x = 1:2, y = dir_p_0.4[2, ], col = "red", lwd = 2, type = "p")
axis(side=1, at=seq(1,2,1))
lines(x = 1:2, y = dir_p_0.4[1, ], col = "green", lwd = 1.5, type = "l")
lines(x = 1:2, y = dir_p_0.4[1, ], col = "green", lwd = 2, type = "p")
legend(x = "topleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)

# p = 1
plot(x = 1:2, y = dir_p_1[2, ], type = "l", xlab = "Half", ylab = "Explicit PC", col = "red", ylim = c(0, 10), main = "p reward = 1", lwd = 1.5, xaxt = "n")
lines(x = 1:2, y = dir_p_1[2, ], col = "red", lwd = 2, type = "p")
axis(side=1, at=seq(1,2,1))
lines(x = 1:2, y = dir_p_1[1, ], col = "green", lwd = 1.5, type = "l")
lines(x = 1:2, y = dir_p_1[1, ], col = "green", lwd = 2, type = "p")
legend(x = "topleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)




# visualizing the data


plot(x = rep(1:4, each = 2), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208][[4]]), type = "p", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0, 1), main = "p reward = 0.1", lwd = 2, xaxt = "n")
axis(side=1, at=seq(1,4,1))

lines(x = rep(1:4, each = 6), y = c(exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[1]), 205:208][[1]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[1]), 205:208][[2]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[1]), 205:208][[3]], exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[1]), 205:208][[4]]), col = "green", lwd = 2, type = "p")

legend(x = "bottomleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)




as.numeric(exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208])



as.vector(exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[2]), 205:208])[[1]]





install.packages("rstatix")
library(rstatix)


levene_test()




means_p_0.1
means_p_0.2
means_p_0.4
means_p_1


final_exp <- exp_dirpc[ , c(1:3 , 205:208)]


final_exp <- final_exp %>% gather(key = "time", value = "explore", `Explo Block 1`, `Explo Block 2`, `Explo Block 3`, `Explo Block 4`) %>% convert_as_factor(time)


summary <- final_exp %>%
  group_by(Reward.Frequency, Control.Condition, time) %>%
  get_summary_stats(explore, type = "mean_sd")


final_exp %>%
  group_by(Reward.Frequency, Control.Condition, time) %>%
  identify_outliers(explore)





final_exp %>%
  group_by(Reward.Frequency, Control.Condition, time) %>%
  shapiro_test(explore)


ggqqplot(final_exp, "explore", ggtheme = theme_bw()) +
  facet_grid(explore ~ Reward.Frequency, labeller = "label_both")


final_exp %>%
  group_by(time) %>%
  levene_test(explore ~ Reward.Frequency*Control.Condition)


res.aov <- anova_test(
  data = final_exp, dv = explore, wid = id,
  within = time, between = c(Reward.Frequency, Control.Condition)
)
get_anova_table(res.aov)



install.packages("afex")
library(afex)

mixed_anova <- aov_ez(
  id = "ID",
  dv = "explore",
  data = final_exp,
  between = "Reward.Frequency",
  within = "time"
)





# calculate time clicks
# graphs/visualise
# confirmatory vs exploratory analyses

# calculate time clicks
# graphs/visualise
# confirmatory vs exploratory analyses
# check the APA guidelines for figures
# check assumptions tests for main analyses: e.g. Levene's test and Shapiro-Wilk test?
# analyse times between clicks: e.g. average the time between the firts 10 and last 10 clicks and compare between conditions
