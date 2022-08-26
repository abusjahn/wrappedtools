# I'm not sure it's possible to do the same "saveRDS %>% readRDS" kind of methodology with ggplot2 objects
# because I'm not too clear on compressing to or decompressing from binary files


# # out1 <- ggplot(mtcars, aes(wt, mpg)) +
# #   geom_point() +
# #   scale_y_log10(breaks = logrange_5)
# # out2 <- ggplot(mtcars, aes(wt, mpg)) +
# #   geom_point() +
# #   scale_y_log10(breaks = logrange_123456789)
# # saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/logrange_1_out.rda')
# test_that("logrange_1() with defaults and options set", {
#   expected <- readRDS('logrange_1_out.rda')
#   expect_equal(ggplot(mtcars, aes(wt, mpg)) +
#                  geom_point() +
#                  scale_y_log10(breaks = logrange_5), expected[[1]])
#   expect_equal(ggplot(mtcars, aes(wt, mpg)) +
#                  geom_point() +
#                  scale_y_log10(breaks = logrange_123456789), 
#                expected[[2]])
# })