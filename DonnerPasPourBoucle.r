# pour savoir où on en est quand une boucle tourne, affichage de l'état à chaque 25 cas :


for(i in 1:nrow(df))
	{
	XXXX
	YYYY
	...
	
	if(i %% 25 ==0) { cat(paste("Pas", i, "sur", nrow(df)), "\n") }
}			
