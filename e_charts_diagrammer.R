library(echarts4r)
library(RColorBrewer)

# define e_charts_diagrammer function that converts nodes and edges dataframe into echarts4r friendly lists 

e_charts_diagrammer <- function(nodes0 = NULL, edges0 = NULL, animationlength = 3000){
  nodeslist <- list()
  for (i in 1:nrow(nodes0)){
    nodeslist[[i]] <- list("name" = nodes0$name[i], 
                           "x" = nodes0$x[i], 
                           "y" = nodes0$y[i], 
                           "symbol" = nodes0$symbol[i], 
                           "symbolSize" = nodes0$symbolSize[i],
                           "label" = list("fontSize" = nodes0$label.fontsize[i]),
                           "category" = nodes0$category[i])
  }
  edgeslist <- list()
  for (i in 1:nrow(edges0)){
    edgeslist[[i]] <- list("source" = edges0$source[i],
                           "target" = edges0$target[i],
                           "symbol" = list(edges0$edgeSymbol.start[i], edges0$edgeSymbol.end[i]),
                           "symbolSize" = list(edges0$symbolSize.start[i], edges0$symbolSize.end[i]),
                           "label" = list("show" = edges0$label.show[i], "fontSize" = edges0$label.fontsize[i]),
                           "lineStyle" = list("width" = edges0$lineStyle.width[i], 
                                              "curveness" = edges0$lineStyle.curveness[i],
                                              "color" = edges0$linestyle.color[i]))
  }
  categorieslist <- list()
  for (i in 1:nrow(nodes0)){
    categorieslist[[i]] <- list("name" = nodes0$category[i])
  }
  opts <- list(
    series = list(
      list(
        type = 'graph',
        layout = 'none',
        roam = TRUE,
        data = nodeslist,
        links = edgeslist,
        categories = categorieslist,
        animationDuration = animationlength
      )
    )
  )
  diagram <- 
    e_charts() |>
    e_list(opts)
  return(diagram)
}

# define nodes 

nodes <- data.frame(name = c("this", "is", "cool!"),
                    x = c(100, 300, 200),
                    y = c(100, 100, 500),
                    symbol = c("diamand", "rectangle", "circle"),
                    symbolSize = c(50, 50, 100),
                    category = c("this...", "is...", "cool!"),
                    label.fontsize = c(20, 20, 40))

# define edges

edges <- data.frame(source = c("this", "is", "cool!", "cool!"),
                    target = c("cool!", "cool!", "this", "is"),
                    edgeSymbol.start = c("circle", "circle", "circle", "circle"),
                    edgeSymbol.end = c("arrow", "arrow", "arrow", "arrow"),
                    symbolSize.start = c(10, 10, 10, 10),
                    symbolSize.end = c(20, 20, 20, 20),
                    label.show = c(FALSE, FALSE, TRUE, FALSE),
                    label.fontsize = c(20, 20, 20, 20),
                    lineStyle.width = c(5, 5, 10, 5),
                    lineStyle.curveness = c(0, 0.2, 0.8, 0),
                    linestyle.color = c("source","#000", "target", "sourcde"))

# run the e_charts_diagrammer

e_charts_diagrammer(nodes, edges) |>
e_color(brewer.pal(n = 3, name = "Set3")) |>
e_labels(position = 'inside', fontWeight = "bold", color = "#000") |>
e_legend(itemGap = 50, orient="vertical", left = 0, 
             selected = list("this..." = FALSE,
                             "is..." = FALSE,
                             "cool!" = FALSE))
    
 
