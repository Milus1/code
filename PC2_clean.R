library(stringr)

PC2$ID <- as.numeric(PC2$ID)
PC2$TotalScoreGuess <- as.numeric(PC2$TotalScoreGuess)
PC2$TimeTaken <- as.numeric(PC2$TimeTaken)

PC2 <- PC2[PC2$ID>=20211210000000,]
PC2 <- PC2[PC2$TimeTaken<25 & PC2$TimeTaken > 0.5,]
PC2 <- cbind(PC2,'GreySquares' = str_count(PC2$board.ID,"1"))
