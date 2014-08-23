Plot5 <- function() {
        if("summarySCC_PM25.rds" %in% dir()) {
                dat <- readRDS("summarySCC_PM25.rds")
        }
        if("Source_Classification_Code.rds" %in% dir()) {
                codebk <- readRDS("Source_Classification_Code.rds")
        }
        
        ## Gathering emissions data from all "motor vehicle sources" in 
        ## Baltimore City.  "Motor Vehicle" does not distinguish personal
        ## vehicles from commercial/industrial, so all information classified
        ## by the EPA as "Mobile" and within Balt. City is being captured.
        good <- grep("Mobile", codebk$EI.Sector)
        scc <- codebk[good,1]
        mvdat <- dat[(dat$SCC %in% scc) & dat$fips=="24510", c(4,6)]
        
        ## Sum Emission totals by year and re-name columns of summed data
        mvagg <- aggregate(mvdat$Emissions, by=list(mvdat$year), FUN=sum)
        colnames(mvagg) <- c("Year", "Emissions")
        
        png("Plot5.png")
        p <- qplot(mvagg$Year,
              mvagg$Emissions,
              mvagg,
              main=expression('PM'[2.5]*' Emissions from Motor Vehicles in Baltimore City'),
              xlab="Year",
              ylab=expression('PM'[2.5]*' Emissions (in tons)'),
              geom=c("point", "line"))
        print(p)
        dev.off()
}