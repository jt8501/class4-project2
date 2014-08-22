Plot1 <- function() {
        if("summarySCC_PM25.rds" %in% dir()) {
                dat <- readRDS("summarySCC_PM25.rds")
        }
        if("Source_Classification_Code.rds" %in% dir()) {
                codebk <- readRDS("Source_Classification_Code.rds")
        }
        
        png(filename="Plot1.png")
        yrsum <- aggregate(dat$Emission, by=list(dat$year), FUN=sum)
        yrsum$x <- yrsum$x / 1000
        plot(yrsum, 
             main=expression('Total PM'[2.5]*' Measured in the U.S.'), 
             xlab="Year", ylab=expression('PM'[2.5]*' (in thousands of tons)'), 
             pch = 20, 
             cex=1.5, 
             col="red")
        lines(yrsum)
        dev.off()
}