library(stringr)

#convert into the right data type
PC2$ID <- as.numeric(PC2$ID)
PC2$TotalScoreGuess <- as.numeric(PC2$TotalScoreGuess)
PC2$TimeTaken <- as.numeric(PC2$TimeTaken)

#remove all data from before we started collecting
PC2 <- PC2[PC2$ID>=20211210000000,] 

#remove participants with time taken longer than 25 minutes or quicker than 30 seconds
PC2 <- PC2[PC2$TimeTaken<25 & PC2$TimeTaken > 0.5,]

#remove all participants with an NA value
PC2 <- PC2[PC2$ID != PC2[apply(is.na(PC2),1,sum),1],] 

#calculate the grey squares
PC2 <- cbind(PC2,'GreySquares' = str_count(PC2$board.ID,"1")) #add column 

