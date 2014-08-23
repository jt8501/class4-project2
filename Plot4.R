Plot4 <- function() {
        if("summarySCC_PM25.rds" %in% dir()) {
                dat <- readRDS("summarySCC_PM25.rds")
        }
        if("Source_Classification_Code.rds" %in% dir()) {
                codebk <- readRDS("Source_Classification_Code.rds")
        }
        
        ## Identify all codebook numbers for coal sources, excluding charcoal.
        ## Subset those numbers from the codebook, then subset the dataset
        ## using the "coal" codes.  This yields the appropriate data subset, cdat.
        good <- grep("[^char]coal", codebk$EI.Sector, ignore.case=TRUE)
        scc <- codebk[good,1]
        cdat <- dat[dat$SCC %in% scc, c(4,6)]
        
        cagg <- aggregate(cdat$Emissions, by=list(cdat$year), FUN=sum)
        colnames(cagg) <- c("Year", "Emissions")
        cagg$Emissions <- as.numeric(cagg$Emissions) / 1000
        
        png("Plot4.png")
        p <- qplot(cagg$Year, 
              cagg$Emissions, 
              cagg, 
              main=expression('PM'[2.5]*' Emissions from Coal in the U.S.'), 
              xlab="Year", 
              ylab=expression('PM'[2.5]*' Emissions (in thousands of tons)'), 
              geom=c("point", "line"))
        print(p)
        dev.off()
        
}