##############################################

library(sf)
library(plyr)
library(dplyr)


repoPath <- 'C:/Users/jstudyvin/GoogleDrive/Rcode/wind/windAC/'


plot <- st_read('plot.shp')

turbine <- st_read('turbine.shp')

plot

plot(st_geometry(plot))

plot(st_geometry(turbine),add=TRUE)




plotType <- ddply(plot,~turbName,function(d){
    ##d <- subset(plot,turbName==1)

    thisPlot <- data.frame(turbName = as.character(unique(d$turbName)))
    thisType <- as.character(d$plotType)

    thisPlot$plotType <- ifelse(any(grepl('road|pad',thisType)),'roadPad','full')

    thisPlot

})


plot2 <- summarize(group_by(.data=plot,turbName))

plot3 <- dplyr::left_join(plot2,plotType)


graphics.off()
for(i in 1:nrow(plot3)){
    plot(st_geometry(plot3[i,]))
    print(st_drop_geometry(plot3)[i,'plotType'])
    readline(prompt='next?\n')
}#end if else



plot3
turbine$id <- NULL

turbineSpatial <- list(turbinePlots=plot3,turbinePoints=turbine)


save(turbineSpatial,file=paste0(repoPath,'/windAC/data/turbineSpatial.RData'))

