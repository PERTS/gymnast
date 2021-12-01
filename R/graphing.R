modules::import(
  'ggplot2',
  'element_blank',
  'element_line',
  'element_text',
  'position_dodge',
  'stat_summary',
  'theme'
)

text_size <- 8
line_size <- .2

ht <- theme(    #   remove the gray background
  panel.background    = element_blank() ,
  #   make the major gridlines light gray and thin
  panel.grid.major.y  = element_line( size=line_size, colour="black" ) ,
  #   suppress the vertical grid lines
  panel.grid.major.x  = element_blank() ,
  #   suppress the minor grid lines
  panel.grid.minor    = element_blank() ,
  #   adjust the axis ticks
  axis.ticks    = element_line( size=line_size , colour="black" ),
  #   move the y-axis over to the left
  axis.title.x  = element_text( vjust=-.5 , color="black", angle=0, size=text_size ) ,
  axis.text     = element_text( vjust=0 , color="black", angle=0, size=text_size ) ,
  axis.title.y  = element_text( vjust=.2 , color="black", angle=90, size=text_size ) ,
  plot.title    = element_text( color="black", angle=0, size=text_size )
)

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data=fun, colour="black", geom=geom, ...)
}

#  define the dodge object for equivalent error bar and geom_bar
dodge <- position_dodge( width=0.9 )

# an error bar using bootstrapped confidence intervals, +/- 1 standard error
se_error_bar <- stat_sum_df( fun="mean_cl_boot",
                                geom="errorbar", width=.3,
                                fun.args=list(conf.int=.68),
                                position=dodge)

