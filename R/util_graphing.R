# util_graphing


library( grid )     # for units
library( scales )
library( ggplot2 )
library( Hmisc )

ug.text_size <- 8
ug.line_size <- .2

ug.ht <- theme(    #   remove the gray background
  panel.background    = element_blank() ,
  #   make the major gridlines light gray and thin
  panel.grid.major.y  = element_line( size=ug.line_size, colour="black" ) ,
  #   suppress the vertical grid lines
  panel.grid.major.x  = element_blank() ,
  #   suppress the minor grid lines
  panel.grid.minor    = element_blank() ,
  #   adjust the axis ticks
  axis.ticks    = element_line( size=ug.line_size , colour="black" ),
  #   move the y-axis over to the left 
  axis.title.x  = element_text( vjust=-.5 , color="black", angle=0, size=ug.text_size ) ,
  axis.text     = element_text( vjust=0 , color="black", angle=0, size=ug.text_size ) ,
  axis.title.y 	= element_text( vjust=.2 , color="black", angle=90, size=ug.text_size ) ,
  plot.title		= element_text( color="black", angle=0, size=ug.text_size ) 
)

ug.stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data=fun, colour="black", geom=geom, ...)
}

#  define the dodge object for equivalent error bar and geom_bar
ug.dodge <- position_dodge( width=0.9 )

# 
ug.se_error_bar <- ug.stat_sum_df( "mean_cl_boot", fun.y="mean",
                                geom="errorbar", width=.3, 
                                conf.int=.68, position=ug.dodge) 

