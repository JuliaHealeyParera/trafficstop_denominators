# install.packages("gt") # just once
# install.packages("gtsummary") # just once

library(gt)
library(gtsummary)

my_diamonds_gt = diamonds |> head(10) |> gt()
my_diamonds_gt
gtsave(my_diamonds_gt, paste("my diamonds", today(), ".html"))

my_diamonds_gt |> saveRDS(paste("my diamonds", today(), ".rds"))
load(paste("my diamonds", today(), ".rds"))

diamonds |> head(10) |> tbl_summary(by = cut)
