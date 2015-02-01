helper.function <- function()
{
  return(1)
}

helper.function <- function()
{
     return(1)
}


.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
     napply <- function(names, fn) sapply(names, function(x)
          fn(get(x, pos = pos)))
     names <- ls(pos = pos, pattern = pattern)
     obj.class <- napply(names, function(x) as.character(class(x))[1])
     obj.mode <- napply(names, mode)
     obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
     obj.prettysize <- napply(names, function(x) {
          capture.output(print(object.size(x), units = "auto")) })
     obj.size <- napply(names, object.size)
     obj.dim <- t(napply(names, function(x)
          as.numeric(dim(x))[1:2]))
     vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
     obj.dim[vec, 1] <- napply(names, length)[vec]
     out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
     names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
     if (!missing(order.by)) { out <- out[order(out[[order.by]], decreasing=decreasing), ] }
     if (head) { out <- head(out, n) }
     out
}

lsos <- function(..., n=10) {
     .ls.objects(..., order.by="Size", decreasing=TRUE, 
                 head=TRUE, n=n)
}







library(ggplot2)
tema <- theme_bw() 
theme_set(tema)
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
               "#CC79A7", '#000000' ,'#CCCC99')

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
col.fill <-  scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
col.ptos <-   scale_colour_manual(values=cbPalette)


coord.etiquetas <- function(df, x, y, 
                            etiq = "etiq", size = 3.5) {
     plot(df[, x], df[, y])
     salida.plabel <- pointLabel(df[, x], df[, y], 
                                 df[, etiq], 
                                 doPlot = TRUE, 
                                 cex = 2*size/5, xpd=TRUE)  
     dev.off()
     df$a <- salida.plabel$x
     df$b <- salida.plabel$y
     df
}    

facet_wrap_labeller <- function(gg.plot,labels=NULL) {
     #works with R 3.0.1 and ggplot2 0.9.3.1
     require(gridExtra)
     
     g <- ggplotGrob(gg.plot)
     gg <- g$grobs      
     strips <- grep("strip_t", names(gg))
     
     for(ii in seq_along(labels))  {
          modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                             grep=TRUE, global=TRUE)
          gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
     }
     
     g$grobs <- gg
     class(g) = c("arrange", "ggplot",class(g)) 
     g
}
