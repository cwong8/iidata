setwd("C:/Users/Christopher/Desktop/Yellow Taxi/")
yellowtaxi <- as.data.frame(fread("yellow_tripdata_2015-12.csv"))

library(data.table)

DataSampler = function(data){
  row.sample = sapply(1:nrow(data), function(i){
    decision = sample(c(TRUE, FALSE), 1, prob = c(1/100, 99/100))
    return(decision)
    })
  newdata <- data[c(row.sample), ]
  # Optional row name cleaning
  rownames(newdata) <- 1:nrow(newdata)
  return(newdata)
}

RussianRoulette = function(data){
  # Load up the bullets
  primed.gun = sapply(1:ncol(data), function(j){
    load.gun = sapply(1:nrow(data), function(i){
      # Adjust probabilities as needed
      is.it.dead = sample(c("It dead.", "How did it miss"), 1, prob = c(1/10, 9/10))
      return(is.it.dead)
    })
    return(load.gun)
  })
  # Firing holes into the data
  indices = which(primed.gun == "It dead.", arr.ind = TRUE)
  replace(data, indices, values = NA)
}

yellowtaxi.compact.clean <- DataSampler(yellowtaxi)
yellowtaxi.compact.dirty <- RussianRoulette(yellowtaxi.compact.clean)

write.csv(yellowtaxi.compact.clean, "Yellow Taxi Data (Clean).csv", row.names = FALSE)
write.table(yellowtaxi.compact.clean, file = "Yellow Taxi Data (Clean).dat", row.names = FALSE, col.names = TRUE, sep = "\t")
