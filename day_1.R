library(readr)

input <- read_csv("data/day_1.csv", col_names = FALSE)[[1]]

### part 1
# calc how many differences are positive
sum(input[2:2000] - input[1:1999] > 0)

### part 2
# calc windows
windows <- input[1:1998] + input[2:1999] + input[3:2000]

# calc differences and how many of them are positive
sum(windows[2:1998] - windows[1:1997] > 0)
