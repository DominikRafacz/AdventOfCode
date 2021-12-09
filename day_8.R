library(readr)
library(dplyr)

dat <- read_delim("data/day_8.csv", col_names = c(1:10, "sep", paste("digit_", 1:4, sep = "")))[, -11]

### part 1

dat %>%
  select(starts_with("digit")) %>%
  as.list() %>%
  unlist() %>%
  .[nchar(.) %in% c(2, 3, 4, 7)] %>%
  length()

### part 2

reality <- list(
  `1` = c("c", "f"),
  `2` = c("a", "c", "d", "e", "g"),
  `3` = c("a", "c", "d", "f", "g"),
  `4` = c("b", "c", "d", "f"),
  `5` = c("a", "b", "d", "f", "g"),
  `6` = c("a", "b", "d", "e", "f", "g"),
  `7` = c("a", "c", "f"),
  `8` = c("a", "b", "c", "d", "e", "f", "g"),
  `9` = c("a", "b", "c", "d", "f", "g"),
  `0` = c("a", "b", "c", "e", "f", "g")
)



apply(dat, 1, function(row) {
  combinations <- row[1:10]
  code <- row[11:14]
  
  # for the rest of this function, 0 is the same as 10
  
  # n-th element of this vector is the position of digit n in "combinations" vector
  number_indices <- numeric(10)
  
  nchars <- nchar(combinations)
  number_indices[1] <- which(nchars == 2)
  number_indices[4] <- which(nchars == 4)
  number_indices[7] <- which(nchars == 3)
  number_indices[8] <- which(nchars == 7)
  
  # 5-segmented digits positions and 6-segmented digits positions
  five_segmented_indices <- which(nchars == 5)
  six_segmented_indices <- which(nchars == 6)
  
  #splitting codes into separate letters
  segments_splitted <- lapply(combinations, function(code) strsplit(code, "")[[1]])
  
  mapping <- character(7)
  names(mapping) <- letters[1:7]
  
  
  # set of segments for seven minus set of segments for one is segment which should correspond to 'A'
  mapping["a"] <- setdiff(segments_splitted[[number_indices[7]]], segments_splitted[[number_indices[1]]]) 
  
  # digit 4 is included fully only in 9 among all digits represented by 6 segments
  number_indices[9] <- six_segmented_indices[sapply(segments_splitted[six_segmented_indices], function(digit) all(segments_splitted[[number_indices[4]]] %in% digit))]
  
  # digit 1 is included fully only in 0 and 9 among all digits represented by 6 segments
  number_indices[6] <- six_segmented_indices[!sapply(segments_splitted[six_segmented_indices], function(digit) all(segments_splitted[[number_indices[1]]] %in% digit))]
  
  # the other digit represented by 6 segments is 0
  number_indices[10] <- setdiff(six_segmented_indices, number_indices[c(9, 6)])
  
  # set of segments for 6 minus set of segments for 9 is segment which should correspond to 'E'
  mapping["e"] <- setdiff(segments_splitted[[number_indices[6]]], segments_splitted[[number_indices[9]]]) 
  
  # set of segments for 9 minus set of segments for 6 is segment which should correspond to 'C'
  mapping["c"] <- setdiff(segments_splitted[[number_indices[9]]], segments_splitted[[number_indices[6]]]) 
  
  # digit 1 is included fully only in 3 among all digits represented by 5 segments
  number_indices[3] <- five_segmented_indices[sapply(segments_splitted[five_segmented_indices], function(digit) all(segments_splitted[[number_indices[1]]] %in% digit))]
  
  # 7 minus A anc C is F
  mapping["f"] <- setdiff(segments_splitted[[number_indices[7]]], mapping[c("a", "c")])
  
  # 9 minus 3 is B
  mapping["b"] <- setdiff(segments_splitted[[number_indices[9]]], segments_splitted[[number_indices[3]]])
  
  # 0 minus almost everything is G
  mapping["g"] <- setdiff(segments_splitted[[number_indices[10]]], mapping[c("a", "b", "c", "e", "f")])
  
  # the last letter is D
  mapping["d"] <- setdiff(letters[1:7], mapping)
  
  
  # create reverse mapping
  rev_mapping <- names(mapping)
  names(rev_mapping) <- mapping
  
  # map what should be mapped
  mapped <- lapply(strsplit(code, ""), function(digit) rev_mapping[digit])
  
  # match it with real segments for digits
  as.numeric(paste(as.character(
    sapply(mapped, function(digit) c(1:9, 0)[
      sapply(reality, function(real_digit) setequal(digit, real_digit))
      ])), collapse=""))
}) %>% sum()
