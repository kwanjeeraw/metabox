# metabox installation method.

In R, run following code.

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("kwanjeeraw/metabox")

library(metabox)
library(opencpu)
opencpu$browse('library/metabox/www') 
```
