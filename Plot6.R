Plot6 <- function() {
        if("summarySCC_PM25.rds" %in% dir()) {
                dat <- readRDS("summarySCC_PM25.rds")
        }
        if("Source_Classification_Code.rds" %in% dir()) {
                codebk <- readRDS("Source_Classification_Code.rds")
        }
        
        ## Gathering emissions data from all "motor vehicle sources" in 
        ## Baltimore City and LA County.  "Motor Vehicle" does not distinguish 
        ## personal vehicles from commercial/industrial, so all information 
        ## classified by the EPA as "Mobile" in these areas is being captured.
        good <- grep("Mobile", codebk$EI.Sector)
        scc <- codebk[good,1]
        baltmv <- dat[(dat$SCC %in% scc) & dat$fips=="24510", c(4,6)]
        lacomv <- dat[(dat$SCC %in% scc) & dat$fips=="06037", c(4,6)]
        
        ## 1) Sum Emission totals by year
        ## 2) Add column with location name to each data set before joining them
        ## 3) Re-name columns of aggregate data
        btmvagg <- aggregate(baltmv$Emissions, by=list(mvdat$year), FUN=sum)
        lamvagg <- aggregate(lacomv$Emissions, by=list(lacomv$year), FUN=sum)
        btmvagg <- cbind(btmvagg, "Baltimore City")
        lamvagg <- cbind(lamvagg, "Los Angeles County")
        colnames(btmvagg) <- c("Year", "Emissions", "Location")
        colnames(lamvagg) <- c("Year", "Emissions", "Location")
        mvcomp <- rbind(btmvagg, lamvagg)
        
        png("Plot6.png")
        p <- qplot(x=Year,
                   y=Emissions,
                   data=mvcomp,
                   color=Location,
                   main=expression('PM'[2.5]*' Emissions from Motor Vehicles'),
                   xlab="Year",
                   ylab=expression('PM'[2.5]*' Emissions (in tons)'),
                   geom=c("point", "line"))
        print(p)
        dev.off()
}