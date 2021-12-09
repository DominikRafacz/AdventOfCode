library(dplyr)

dat <- readLines("data/day_9.txt") %>%
  strsplit("") %>% 
  sapply(as.numeric) %>%
  t()

above <- rbind(matrix(Inf, ncol = 100), dat[1:99, ])
below <- rbind(dat[2:100, ], matrix(Inf, ncol = 100))
onleft <- cbind(matrix(Inf, nrow = 100), dat[, 1:99])
onrigth <- cbind(dat[, 2:100], matrix(Inf, nrow = 100))

is_low_point <- (dat < above) & (dat < below) & (dat < onleft) & (dat < onrigth)

sum(dat[is_low_point] + 1)


### part 2

low_points_indices <- which(is_low_point, arr.ind = TRUE)

POS_MAX <- 100
POS_MIN <- 1

generate_neighbours <- function(x, y) {
  rbind(
    matrix(numeric(0), ncol = 2, dimnames = list(character(0), c("row", "col"))),
    if (y + 1 <= POS_MAX) c(y + 1, x) else NULL,
    if (x + 1 <= POS_MAX) c(y, x + 1) else NULL,
    if (y - 1 >= POS_MIN) c(y - 1, x) else NULL,
    if (x - 1 >= POS_MIN) c(y, x - 1) else NULL
  )
}

filter_non_9 <- function(neighbours) {
  do.call(rbind,
          lapply(1:nrow(neighbours),
                 function(ind) if (dat[neighbours[ind, "row"], neighbours[ind, "col"]] == 9) NULL else neighbours[ind, ])
          )
}

filter_already_inbasin_neighbours <- function(neighbours,
                                              inbasin_points) {
  do.call(rbind,
          lapply(1:nrow(neighbours), 
                 function(ind) if (any(apply(inbasin_points, 1,
                                             function(inbasin_pos) neighbours[ind, "row"] == inbasin_pos[1] && neighbours[ind, "col"] == inbasin_pos[2]
                 ))) NULL else neighbours[ind, ]
          )
  )
}

sizes <- apply(low_points_indices, 1, function(position) {
  unfinished_basin_points <- matrix(position, ncol = 2)
  finished_basin_points <- matrix(numeric(0), ncol = 2)
  
  while(nrow(unfinished_basin_points) > 0) {
    point_y <- unfinished_basin_points[1, 1]
    point_x <- unfinished_basin_points[1, 2]
    neighbours <- generate_neighbours(point_x, point_y)
    non_9_neighbours <- filter_non_9(neighbours)
    non_already_inbasin_neighbours <- filter_already_inbasin_neighbours(non_9_neighbours,
                                                                        rbind(unfinished_basin_points,
                                                                              finished_basin_points)
                                                                        )
    finished_basin_points <- rbind(finished_basin_points, unfinished_basin_points[1, ])
    unfinished_basin_points <- rbind(unfinished_basin_points[-1, ], non_already_inbasin_neighbours)
  }
  
  nrow(finished_basin_points)
})

top_vals <- sort(sizes, decreasing = TRUE)[1:3]
top_vals[1] * top_vals[2] * top_vals[3]
