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