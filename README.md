# IMDB Movie Genre Classification 

## [Dataset Decription](https://github.com/ddong63/IMDB_Genre_Prediction/blob/master/Report.md#data-visualization)

The most popular movie genres are drama, documentary, comedy, action and romance. The least popular movie genres are short, game show, film noir and talk show. As the least popular genres are irrelavant and caused unevenness, they are not included in this analysis.

There are 28 unique genres, with an imbalanced distribution and various level of missingness. 

```
##  [1] "Romance"     "Documentary" "News"        "Sport"       "Biography"  
##  [6] "Drama"       "Crime"       "Adventure"   "Fantasy"     "Comedy"     
## [11] "War"         "Family"      "History"     "Sci.Fi"      "Western"    
## [16] "Thriller"    "Mystery"     "Horror"      "Action"      "Music"      
## [21] "Short"       "Animation"   "Musical"     "Film.Noir"   "Talk.Show"  
## [26] "Adult"       "Reality.TV"  "Game.Show"
```

## [Model Comparison](https://github.com/ddong63/IMDB_Genre_Prediction/blob/master/Report.md#modeling)

Two algorithms were performed (random ferns multilabel algorithm and multivariate random forest), with six differnt algorithm adaptation methods. The models was represented as multiple binary trees in order to simplify the problem. The best model were resampled, however the accuracy did not improved a lot.

The model with best presicions are using binery relvance (70%), and nested stacking (79%), and stacked generalization (77%). The model with best F1 are: classifier chains (53%), dependent binary relevance (55%), and stacked generalization (46%). Random forest performs the worst. This suggests an uneven distribution in the training data.


### Key features

Take one output (Thriller) as an example. The binery relvance model was applied to Thriller model. The key features to predict it is a Thriller are:

Whether a director is know for Thriller
Movie run time
Movie popularity
Average rating
Director birth year
Whether it is a documentary (This is still unclear, but requires further diagostics and adjust the uneven data sampling problem)

## Challenges

- Imbanlanced data distribution with a nested mulilabel structure, a stratefied sampling method needs to be applied to sample a balance data.
- The evaluation for feature selection for multi-label text classification is tricky
- Missing values

Please find the full report in [here](https://github.com/ddong63/IMDB_Genre_Prediction/blob/master/Report.md)

