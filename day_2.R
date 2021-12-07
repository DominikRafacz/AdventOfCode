library(readr)

dat <- read_delim("data/day_2.csv", col_names = FALSE, delim = " ")
directions <- dat$X1 
values <- dat$X2

### part 1
(sum(values[directions == "down"]) - sum(values[directions == "up"])) * sum(values[directions == "forward"])

### part 2
aim <- cumsum(ifelse(directions == "down", values, ifelse(directions == "up", -values, 0)))
sum(ifelse(directions == "forward", aim * values, 0)) * sum(values[directions == "forward"]) 
