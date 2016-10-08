# metabox
a toolbox for metabolomic data analysis, visualization and ‘omic’ integration

Version: 1.1 (13 September 2016)

Description
=========
An R-based web applications for data processing, statistical analysis, integrative visual exploration and functional analysis with several approaches (such as functional class scoring, overrepresentation analysis and WordCloud generation).

Installation and Running metabox
=========
  1. Require [R software](https://www.r-project.org/)
  2. Require [opencpu](https://www.opencpu.org/)
  3. Install metabox using the following commands
```
#Install devtools R package, if not exist
install.packages('devtools', repos="http://cran.rstudio.com/")
library(devtools)

#Install dependent packages
source('https://bioconductor.org/biocLite.R')
biocLite(c('impute','preprocessCore','GO.db','AnnotationDbi','WGCNA','piano','qpgraph','BioNet','ChemmineR'))

#Install metabox
install_github('kwanjeeraw/metabox')

#Install OpenCPU single-user server
install.packages("opencpu")
library(opencpu)

## Run metabox on a web browser
opencpu$browse("library/metabox/www")

```

Documentation
=========
see [homepage](http://kwanjeeraw.github.io/metabox/)

License
=========
[GNU General Public License (v3)](https://github.com/kwanjeeraw/metabox/blob/master/LICENSE)
