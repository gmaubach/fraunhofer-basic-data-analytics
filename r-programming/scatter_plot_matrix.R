require(ggplot2)
require(GGally)
require(lattice)
# Visualizing with a scatterplot matrix (1/5)
selection <- which(apply(data, 2, function(x) {!(all(is.element(x, c(0,1))))}))

# define a function to render the diagonal of the scatter plot matrix in a nicer way
multi_colored_kde <- function(data, mapping, ...){
  ggplot(data = data, mapping=mapping) +
    geom_density(mapping = aes_string(color="Class"), fill=NA)
}

# ggpairs-function from package GGally 
print(
ggpairs(data, columns=selection, title= "Scatterplot matrix", 
        mapping=ggplot2::aes_string(color="Class"),
        upper="blank",
        diag  = list(continuous=multi_colored_kde),
        lower = list(continuous=wrap("points", alpha=0.2)),
        axisLabels= "none")
)