library(dplyr)
library(tidylog, warn.conflicts = FALSE)

# Sample data
df1 <- data.frame(
  id = 1:100000,
  value = rnorm(100000)
)

df2 <- data.frame(
  id = sample(1:100000, 50000),
  category = sample(letters[1:5], 50000, replace = TRUE)
)

left_join <- df1 %>% left_join(df2, by = "id")

semi_join <- df1 %>% semi_join(df2, by = "id")