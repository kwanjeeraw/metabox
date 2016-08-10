# metabox installation method.

In R, run following code.

```r
if (!require("devtools"))
install.packages('devtools', repos="http://cran.rstudio.com/")
library(devtools)
source('https://bioconductor.org/biocLite.R')
biocLite(c('impute','preprocessCore','GO.db','AnnotationDbi','WGCNA','piano','qpgraph','BioNet'))
install_github('kwanjeeraw/metabox')

library(metabox)
library(opencpu)
opencpu$browse('library/metabox/www')
```
