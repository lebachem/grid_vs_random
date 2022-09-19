# Grid-Search oder Random Search?
In diesem Repository befindet sich eine R File die es erlaubt die Visualisierungen zu dem Lehrvideo: "Grid-Search oder Random Search" zu replizieren und anhand eines beliebigen Problems mit 2 Hyperparmetern zu illustrieren.

## Getting Started
Installiere R auf deinem Rechner

https://www.r-project.org/foundation/

gegebenenfalls auch noch R Studio

https://www.rstudio.com/

Damit kann das Skript ausgeführt werden.

## Benutzung

 Die dazugehörigen Parameter werden direkt im Skript gesetzt.

### Bereiche für die der Hyperparameterraum aufgespannt werden soll (grid_tree = Anzahl der Bäume, grid_depth = Tiefe der Bäume)
 ~~~ 
grid_tree = 1:40  # i (Y-Axe) Zeilen
grid_depth = 1:40 # j (X-Axe) Spalten
 ~~~ 
 
