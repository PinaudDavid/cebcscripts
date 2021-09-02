###############################
#### a function to group successive TRUE events of testing a vector with the same group number.
#### return a vector (same length as test) giving the group number (NA if the result of the test is FALSE)

####  David Pinaud 2021-09-02 R version 4.1.1 (2021-08-10)


grpt <- function(test) {    
		a1 <- test
		a2 <- a1
		a3 <- a1
		a2[2:length(a1)] <-  a1[1:(length(a1)-1)]  # décalage vers le bas 
		a2[1] <- FALSE
		a3 <- ifelse(a1 != a2, T, F)  # détection des entrées et des sorties des séquences
		grp <- vector(mode="numeric", length=length(a1))
		indice <- 1
		for (j in 1:length(a1))
		    {
			    if (a1[j]==TRUE & a3[j]==TRUE)   # changement
			     {
				     grp[j] <- indice  # numérotation des séquences
				     indice <- indice + 1
			     }
		      if (a1[j]==TRUE & a3[j]==FALSE)  # cas où la séquence est la même
		       {
			       grp[j] <- grp[j-1]
		       }
		        
		    }
		rm(a1, a2, a3) 
		grp[grp==0] <- NA
    return(grp)
  }

  
# exemple

dat <- runif(50) 
plot(dat, t="b")
abline(h=0.3, col="red")
grpt(dat > 0.3)  
  
  