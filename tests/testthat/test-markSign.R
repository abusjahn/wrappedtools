# This function maybe needs some rearranging of "or equal to" operators because...
# 
# wrappedtools: markSign(0.05) # shouldn't it be p < 0.05 for this sign, not <=, ??
# [1] *
#   Levels: *** ** * + n.s.
# wrappedtools: markSign(0.0495)
# [1] *
#   Levels: *** ** * + n.s.
# wrappedtools: markSign(0.051)
# [1] +
#   Levels: *** ** * + n.s.

test_that("markSign() with defaults and options set", {
  expect_equal(as.character(markSign(.05)), '*')
  expect_equal(as.character(markSign(.0501)), '+')
  expect_equal(as.character(markSign(.101)), 'n.s.')
  expect_equal(as.character(markSign(.005,
                                     plabel = c('not sure',
                                                'possibly',
                                                'probably',
                                                'pretty likely',
                                                'almost sure'))), 
               'pretty likely')
})
