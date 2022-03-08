setwd("/Users/sramasub/Documents/Personal/Reco-Auction/codes/Set-Wise/")

teams <- c("Akshay/Arjun", 
           "Vicky/Adhok",
           "Dom/Anja",
           "Vijay",
           "Shankar",
           "Raghu",
           "Swamy",
           "Venkat/Gopal")

presetBalance <- 90
adhocBalance <- c(2.8, 1.9, 1.9, 2.1, 2.4, 1.4, 2.8, 2)
startingBalance <- presetBalance + adhocBalance

startingBalanceDf <- data.frame("Owner"=teams, "startingBalance"=startingBalance)
