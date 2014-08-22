Plot2 <- function() {
        if("summarySCC_PM25.rds" %in% dir()) {
                dat <- readRDS("summarySCC_PM25.rds")
        }
        if("Source_Classification_Code.rds" %in% dir()) {
                codebk <- readRDS("Source_Classification_Code.rds")
        }
        
        png(filename="Plot2.png")
        balt <- dat[dat$fips=="24510",c(4,6)]
        balt <- aggregate(balt$Emissions, by=list(balt$year), FUN=sum)
        plot(balt, 
             main=expression('PM'[2.5]*' Measured in Baltimore City, MD'), 
             xlab="Year", ylab=expression('PM'[2.5]*' (in tons)'), 
             pch = 20, 
             cex=1.5, 
             col="red")
        lines(balt)
        dev.off()
}