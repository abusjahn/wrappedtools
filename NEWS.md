# wrappedtools 0.9.1
- functions from abandoned package Biotech reimplemented
- edgecase of unique values in compare2qualvars fixed
- tests adjusted to R 4.3.0

# wrappedtools 0.8.2
- import of tidyverse is dropped (as suggested by Hadley)
- gather/spread are replace by pivot_longer/pivot_wider

# wrappedtools 0.8.1
- pairwise_fisher_test is now using MonteCarlo with 10^5 replications
- faketrial dataset has new column Responder, useful in logistic regression
- compare2qualvars has a new option to test subgroups by recoding other levels into other, default is not to do this
- compare2numvars returns ungrouped result tibble
- magrittr pipe has been replaced by native pipe
- compound assignment (aka 2-way pipe %<>%) has been removed and is no longer exported

# wrappedtools 0.8.0
- new function surprisal (Shannon information from p-value)
- formatP can add surprisal to output
- ggcormat has now p-labels for .05,.01, and .001 labels
- roundR can handle input 0 in a more useful way
- new function medianse duplicating se_median (deprecated) for naming consistency
- some parameter names were anglicized (old one still exist for compatibility)
- function compare_n_numvars now allows non-parametric analysis
- function roundR handles rounding 0 now properly
- help has been extended, new examples have been added 
- function print_kable is now deprecated, as package flextable offers way more power
- function tests have been added 
- default for FindVars is now more obvious, symbols are only returned if requested

# wrappedtools 0.7.9
- formatP returns vector or matrix, depending on input pIn,
this makes piping easier
- formatP handles wrong inputs by returning NA, e.g. after failed try()
- meansd adds spaces around ±
- compare2numvars handles cases with more than 2 groups by producing an error
- cortestR converts input to matrix
- cortestR has bugfix and empty string as changed default for space
- compare2numvars has bugfix: Overall n now takes only cases with valid grouping
variable content

# wrappedtools 0.7.8
- moved testthat from imports to suggests

# wrappedtools 0.7.7
- bugfix in function compare2numvars
- extended vignette

# wrappedtools 0.7.6
- released on CRAN
- added missing value tags
- put package names in single quotes

# wrappedtools 0.7.5
- added documentation tags and examples
- applied styler tidyverse style
- dropped some functions needing further development

# wrappedtools 0.7.4   
- this is the first version ready to be released on CRAN after passing all checks   