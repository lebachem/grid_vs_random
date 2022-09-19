#-------------------------------------------------------------------------------------------------------#
# Grid-Search oder Random-Search?                                                                       #
#                                                                                                       #
# Autor:   Michael Lebacher                                                                             #
# Kontakt: michael.lebacher@gmx.de)                                                                     #
# Datum:   13/09/2022                                                                                   #
#-------------------------------------------------------------------------------------------------------#

# Import von Bibliotheken und Daten ----
library(mlbench)
library(ranger)
library(plot.matrix)
library(RColorBrewer)
library(plot3D)
data(BostonHousing)


# Globale Parameter ----

# Seed
set.seed(42)
#set.seed(12)

# Grafikparameter
par(mar=c(5.1, 4.1, 4.1, 4.1))

# Definition des Gitters
grid_tree = 1:40  # i (Y-Axe) Zeilen
grid_depth = 1:40 # j (X-Axe) Spalten

# Definition der Farbpalette
pal = colorRampPalette(c("white", "darkblue"))

# Globale Funktionen ----


# Trainieren und evaluieren basierend auf den Hyperparametern num_trees (Anzahl Bäume)
# und max depth (maximale Tiefe der Entscheidungsbäume)
train_evaluate = function(num_trees, max_depth){
  #'  num_tress[Int]: Hyperparameter für die Anzahl der Bäume
  #'  max_depth[Int]: Maximale Tiefe der Bäume

  # Trainingsprozess
  model = ranger(medv~ .,data=train, num.trees = num_trees, max.depth = max_depth,seed=42)
  
  # Prädiktion
  y_hat = predict(model,data=val)$predictions
  
  # Evaluierung des RMSE
  RMSE = sqrt(mean((y_hat-val$medv)^2))
  
  return (RMSE)
  }

# Unstrukturiere Zufallssuche (Unstructed Random Search)
unstructured_random = function(n_trials=5){
  #' n_trials[int]: Anzahl von Zufallsziehungen aus dem Parameterraum

    # Zufallszieung on Indices
    s = arrayInd(sample(length(grid),n_trials),dim(grid))
    i =s[,1]
    j =s[,2]
    
    # Schleife zur Berechnung des RMSE
    save = c()
    for (idx in 1:length(i)){
        save = c(save, train_evaluate(i[idx],j[idx]))
    }
    
    return(list(RMSE=save,i = i, j = j))
  
}


# Strukturierte Random Search (structured random search)
structured_random = function(n_iter=3, n_trials=10,mix=T){
  #' n_iter[int]: Anzahl von Iterationen mit Ziehungen
  #' n_trials[int]: Anzahl von (Zufalls)-Ziehungen aus dem Parameterraum
  #' mix[bool]: Kombination aus Random Search (n_iter=1) und lokaler Grid Search (n_iter>1):T
  #'            Kombination aus Random Search (n_iter=1) und lokalem Random Search (n_iter>1):F
  
  set.seed(5)
  
  # Container zur Speicherung von Ergebnissen
  memory_i = c()
  memory_j = c()
  memory_opt_i =c()
  memory_opt_j =c()
  
  # Initialisierung mit Random Search 
  iter = unstructured_random(n_trials)
  memory_i = c( memory_i, iter$i)
  memory_j = c( memory_j, iter$j)
  
  # Evaluierung des minimalen RMSE
  min_MSE = min(iter$RMSE)
  
  # Speichern des optimalen Punktes
  opt_point = which(iter$RMSE==min_MSE,arr.ind=T)[1]
  
  i_opt = iter$i[opt_point]
  j_opt = iter$j[opt_point]
  
  memory_opt_i =c(memory_opt_i, i_opt)
  memory_opt_j =c(memory_opt_j,j_opt)
  
  
  # Lokale Suche auf einem 9*9 Gitter
  if (n_iter-1>0){
  for (k_ in 1:(n_iter-1)){
    
    if (mix==T){
      # Durchsuchung des gesamten lokalen Raums
    s = arrayInd(sample(9*9,9*9,replace=F),c(9,9))
    }
    
    if (mix==F){
      # Ziehung von n_trials Strichproben im lokalen Raum
      s = arrayInd(sample(9*9,n_trials,replace=F),c(9,9))
    }
    
    # Berechnung der Differenzmaske
    s_mask = s - cbind(rep(5,dim(s)[1]),rep(5,dim(s)[1]))
    
    index_set = s_mask +  cbind(rep(i_opt,dim(s_mask)[1]),rep(j_opt,dim(s_mask)[1]))
    
    i =index_set[,1]
    j =index_set[,2]
    
    # Falls der Suchraum verlassen wird setzen wir die Punkte auch den kleinsten bzw. größsten
    # Wert im Suchraum
    i[i>max(grid_tree)]  = max(grid_tree)
    j[j>max(grid_depth)] = max(grid_depth)
    i[i<min(grid_tree)]  = min(grid_tree)
    j[j<min(grid_depth)] = min(grid_depth)
    
    memory_i = c(memory_i, i)
    memory_j = c(memory_j, j)
    
    # Speichern der Ergebnisse
    save = c()
    for (idx in 1:length(i)){
      save = c(save, train_evaluate(i[idx],j[idx]))
    }
    
    # Neusetzung des optimalen Punkte falls ein neues Optimum gefunden wurde
    if (min(save)<min_MSE){
    p_ = which(save==min(save), arr.ind = T)
    i_opt = i[p_][length(p_)]
    j_opt = j[p_][length(p_)]
    memory_opt_i =c(memory_opt_i, i_opt)
    memory_opt_j =c(memory_opt_j, j_opt)
    
    min_MSE = min(save)
    }
  }
  }
  
  return(list(result=c(memory_opt_i[length(memory_opt_i)],memory_opt_j[length(memory_opt_j)]),i=memory_i, j = memory_j,i_opt=memory_opt_i, j_opt = memory_opt_j))
  
}

# Datenvorbereitung ----

# Definition der Datenaufteilung in Trainings- und Validierungsset (70/30)
split = sample(c(rep(0, 0.7 * nrow(BostonHousing)), rep(1, 0.3 * nrow(BostonHousing))))

# Trainingsdatensatz
train = BostonHousing[split==0,]
# Validierungsdatensatz
val = BostonHousing[split==1,]

# Definition des Gitters
grid = matrix(data = NA,nrow = length(grid_tree), ncol=length(grid_depth))

# Evaluierung ----

# Grid Search
start_time1 = Sys.time()
for (i in 1:length(grid_tree)){
  for (j in 1:length(grid_depth)){
    
    grid[i,j] = train_evaluate(i,j)
    
  }
}
end_time1 = Sys.time()


# Ergebnisse: 
minimal_RMSE = min(grid)
print(minimal_RMSE)

delta1 = end_time1 - start_time1
print(delta1)

# Random Search
start_time2 = Sys.time()
unstructured_r = unstructured_random(n_trials=50)
end_time2 = Sys.time()

random_MSE = min(unstructured_r$RMSE)
print(random_MSE)

delta2 = end_time2 - start_time2
print(delta2)

# Ergebnisse:
start_time3 = Sys.time()
p_star_sr_full = structured_random(n_iter=2,n_trials=50)
end_time3 = Sys.time()

p_star_sr = p_star_sr_full$result
structured_random_RMSE = grid[p_star_sr[1],p_star_sr[2]]
print(structured_random_RMSE)

delta3 = end_time3 - start_time3
print(delta3)

# Visualisierung ----


# Leeres Gitter
plot(grid, col='white',xlab='Anzahl Bäume', ylab='Tiefe der Bäume')


# 3D Perspectivplot
persp3D(z = grid, theta = 160, phi=20,col=pal(120),xlab='Anzahl Bäume', ylab='Tiefe der Bäume',zlab='RMSE')

# Gitter mit Farbkodierung (die log(log(log())) Transformation wird angewandt zur besseren Sichtbarmachung)
plot(log(log(log(grid))), col=pal(8), xlab='Anzahl Bäume', ylab='Tiefe der Bäume')

# Optimaler Punkt (Grid Search)
p_star = which(grid==min(grid), arr.ind=TRUE)
points(p_star[2], length(grid_depth)-p_star[1]+1, cex=2, col='black', lwd=5)

# Random Search Punkte
for (k in 1:length(unstructured_r$RMSE)) {
points(unstructured_r$j[k],length(grid_depth)-unstructured_r$i[k]+1,cex=1,col='darkblue',lwd=1.5)
}

# Optimaler Punkt (Random Search)
m = which(unstructured_r$RMSE==min(unstructured_r$RMSE),arr.ind=TRUE)
p_star_rS = c(unstructured_r$i[m],unstructured_r$j[m])

points(p_star_rS[2],length(grid_depth)-p_star_rS[1]+1,cex=2,col='darkblue',lwd=5)


# Kombination Random Search, Grid Search Punkte
for (k in 1:50) {
  points(p_star_sr_full$j[k],length(grid_depth)-p_star_sr_full$i[k]+1,cex=1,col='red')
}

for (k in 1:length(p_star_sr_full$j)) {
  points(p_star_sr_full$j[k],length(grid_depth)-p_star_sr_full$i[k]+1,cex=1,col='red')
}

# Optimaler Punkt (Random Search, Grid Search)
points(p_star_sr_full$result[2],length(grid_depth)-p_star_sr_full$result[1]+1,cex=1.5,col='red',lwd=4)



