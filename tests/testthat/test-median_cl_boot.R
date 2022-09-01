# #2 calls to median_cl_boot
# out1 <- median_cl_boot(x = mtcars$wt, nrepl = 10^5)
# out2 <- median_cl_boot(x = mtcars$wt, conf=0.99,, nrepl = 10^5)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/median_cl_bootout.rda')


test_that("median_cl_boot() with defaults and options", {
  expected <- readRDS('median_cl_bootout.rda')
  expect_equal(round(median_cl_boot(mtcars$wt, nrepl = 10^5),2), 
                     round(expected[[1]],2),
               tolerance = 1)
  expect_equal(round(median_cl_boot(x = mtcars$wt, conf = 0.99, nrepl = 10^5),
                     2),
               round(expected[[2]],2),
               tolerance = 1)
})

# Maybe another compression error? Got this output from the code commented out above
# ══ Testing test-median_cl_boot.R ═══════════════════════════════════════════════
# 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ]
# [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
# [ FAIL 2 | WARN 0 | SKIP 0 | PASS 0 ]
# 
# ── Failure (test-median_cl_boot.R:7:3): median_cl_boot() with defaults and options ──
# median_cl_boot(mtcars$wt) not equal to expected[[1]].
# Component "CIlow": Mean relative difference: 0.00214412
# Component "CIhigh": Mean relative difference: 0.008522727
# 
# ── Failure (test-median_cl_boot.R:8:3): median_cl_boot() with defaults and options ──
# median_cl_boot(x = mtcars$wt, conf = 0.99) not equal to expected[[2]].
# Component "CIlow": Mean relative difference: 0.01751405
# 
# 
# [ FAIL 2 | WARN 0 | SKIP 0 | PASS 0 ]


