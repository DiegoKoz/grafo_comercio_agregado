#citas
r <- citation()
tidyverse <- citation("tidyverse")
ggridges <- citation("ggridges")
ggthemes <- citation("ggthemes")
ggrepel <- citation("ggrepel")
ggbiplot <- citation("ggbiplot")
rjson <- citation("rjson")
qdapRegex <- citation("qdapRegex")
pryr <- citation("pryr")
igraph <- citation("igraph")
countrycode <- citation("countrycode")
RColorBrewer <- citation("RColorBrewer")
xlsx <- citation("xlsx")

toBibtex(c(r,
           tidyverse,
           rjson,
           qdapRegex,
           pryr,
           ggridges,
           igraph,
           countrycode,
           ggthemes,
           RColorBrewer,
           ggrepel,
           xlsx,
           ggbiplot))
