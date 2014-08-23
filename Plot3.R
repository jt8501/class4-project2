Plot3 <- function() {
        ## ATTENTION GRADERS: If you know of a way to manipulate this data
        ## in fewer steps, I'd love to know how!
        
        if("summarySCC_PM25.rds" %in% dir()) {
                dat <- readRDS("summarySCC_PM25.rds")
        }
        if("Source_Classification_Code.rds" %in% dir()) {
                codebk <- readRDS("Source_Classification_Code.rds")
        }
        
        pt <- dat[dat$type=="POINT" & dat$fips=="24510", c(1,4,5,6)]
        npt <- dat[dat$type=="NONPOINT" & dat$fips=="24510", c(1,4,5,6)]
        rd <- dat[dat$type=="ON-ROAD" & dat$fips=="24510", c(1,4,5,6)]
        nrd <- dat[dat$type=="NON-ROAD" & dat$fips=="24510", c(1,4,5,6)]
        
        ptagg <- cbind(aggregate(pt$Emissions, by=list(pt$year), FUN=sum), type="POINT")
        nptagg <- cbind(aggregate(npt$Emissions, by=list(npt$year), FUN=sum), type="NONPOINT")
        rdagg <- cbind(aggregate(rd$Emissions, by=list(rd$year), FUN=sum), type="ON-ROAD")
        nrdagg <- cbind(aggregate(nrd$Emissions, by=list(nrd$year), FUN=sum), type="NON-ROAD")
        
        datagg <- rbind(ptagg, nptagg, rdagg, nrdagg)
        colnames(datagg) <- c("year", "emissions", "type")
        
        png("Plot3.png")
        
        p <- qplot(x=year,
              y=emissions,
              data=datagg,
              color=type,
              geom=c("point", "line"),
              main=expression('PM'[2.5]*' Emissions By Type in Baltimore City'),
              xlab="Year", ylab=expression('PM'[2.5]*' Emissions (in tons)'))
        print(p)
        
        dev.off()
}