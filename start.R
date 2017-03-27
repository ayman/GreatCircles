### Making a nice geo-path plot with a very bad map projection.
### by @ayman http://shamurai.com
###
### Using a dump from http://openflights.org/ we'll make a pretty map
### with great circle plots to show you every where you flew.  Then
### we'll break it down by carrier.
###
### For the point of this instructional, you'll need the following librarires:
###   car
###   maps
###   ggplot2
###   geosphere
###   parallel
###   treemap
### the last one just being for the fun of it.

### The Airport data comes in the following format.
airport.cols <- c("id", "name", "city", "country", "faa", "icao", 
                  "latitude", "longitude", "altitude", "timezone", "dst")

### And with the following classes on each column.
airport.classes <- c("factor", "character", "character", "character",
                     "character", "character", "numeric", "numeric",
                     "numeric", "numeric", "character")

### Read it in...I forget where this was found.
airports <- read.csv("airports.csv",
                     header=TRUE,
                     colClasses=airport.classes,
                     col.names=airport.cols)

### Our flights, from openflights.org looks like this
### http://openflights.org/help/csv.html but the sample dataset only
### has the first 5 columns.  I'll include this for the sample
### commented out.
## flight.cols <- c("Date", "From", "To", "Flight_Number", "Airline",
##                  "Distance", "Duration", "Seat", "Seat_Type", "Class",
##                  "Reason", "Plane", "Registration", "Trip", "Note",
##                  "From_OID", "To_OID", "Airline_OID", "Plane_OID")
## ### And the following classes. 
## flight.classes <- c("character",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "numeric",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "character",
##                     "character")
flight.cols <- c("Row", "From", "To", "Airline")
### And the following classes. 
flight.classes <- c("character",
                    "character",
                    "character",
                    "character")
### Read that in.
flights <- read.csv("my-flights.csv",
                    header=TRUE,
                    colClasses=flight.classes,
                    col.names=flight.cols)

### Just for fun, lets randomize it a bit...so we get new maps each
### run.
flights <- transform(flights,
                     From=sample(From),
                     To=sample(To),
                     Airline=sample(Airline))

### In effect we want to join the long/lat points for each airport
### (from/to).
###     airline    lat       lon     elat      elon
###   US Airways 35.214 -80.94314 37.61897 -122.3749
getLongLatPairs <- function(row, airports, flights) {
    start <- airports[(airports[ ,5] == flights$From[row]), ]
    end <- airports[(airports[ ,5] == flights$To[row]), ]
    data.frame(airline=flights$Airline[row],
               lat=as.numeric(start$latitude),
               lon=as.numeric(start$longitude),
               elat=as.numeric(end$latitude),
               elon=as.numeric(end$longitude),
               ecountry=end$country)
}

### Do this for every flight via a list apply.  Notice we pulled out
### the number of flights as this gets called at each lapply.
num.flights <- dim(flights)[1]
lpaths <- lapply(1:num.flights,
                 getLongLatPairs,
                 airports,
                 flights)

### Flatten the list into one data.frame.
paths <- do.call("rbind", lpaths)

### Factor things
paths$airline <- factor(paths$airline)
paths$ecountry <- factor(paths$ecountry)

### get some numbers
num.paths <- dim(paths)[1]
range.paths <- 1:num.paths

### Load in some classes we need now.
library(maps)
library(ggplot2)
library(geosphere)
library(parallel)

### Get a crummy map projection of the world.
getWorldMap <- function(long="long", lat="lat") {
    worldmap <- data.frame(map("world", plot = FALSE)[c("x","y")])
    worldmap <- rbind(worldmap, NA,
                      data.frame(map('world',
                                     ## xlim = xlim,
                                     ## ylim = ylim,
                                     plot = FALSE)[c("x","y")]))
    names(worldmap) <- c(long, lat)
    return (worldmap)
}

### Make a base ggplot.
gd <- data.frame()
gcp <- ggplot(gd, aes(x=lon, y=lat))
worldmap <- getWorldMap("lon", "lat")
gcp <- gcp + geom_path(data=worldmap, colour="gray")

### Here we need to make the archs for the great circle paths.
makeArchs <- function(x, paths=paths) {
    path <- paths[x, ]
    this.gc <- gcIntermediate(c(path$lon, path$lat),
                              c(path$elon, path$elat),
                              n=98,  ## number of segments
                              addStartEnd=TRUE,
                              breakAtDateLine=TRUE)

    ### The problem here is gcIntermediate returns a matrix of points
    ### if there is one arch or a list of size 2, each with a matrix
    ### for each arch.
    num.lists <- 2
    if (!is.list(this.gc))  {
        num.lists <- 1
        this.gc <- list(this.gc)
    }
    size.one <- dim(this.gc[[1]])[1] - 1
    size.two <- 0
    if (length(this.gc) == 2) {
        size.two <- dim(this.gc[[2]])[1] - 1
    }

    points <- matrix(ncol=4, nrow=(size.one + size.two))
    dimnames(points) <- list(NULL, c("lon", "lat", "elon", "elat"))

    for (j in 1:num.lists) {
        sub.gc <- this.gc[[j]]
        rows <- dim(sub.gc)[1]
        matrix.sub <- matrix(nrow=rows, ncol=4)
        matrix.sub[,1:2] <- sub.gc
        matrix.sub[1:(rows-1),3:4] <- sub.gc[2:rows,]
        matrix.sub <- matrix.sub[1:(rows-1),]
        if (j == 1) {
            points[1:size.one,] <- matrix.sub
        } else {
            points[(size.one + 1):(size.two + size.one),] <- matrix.sub
        }
    }
    return (points)
}

### How many cores do we have?  If we have more than 1, lets use n-1.
num.cores = parallel:::detectCores()
if (num.cores != 1) num.cores <-  num.cores - 1

### Make the archs across the cores. 
archs <- mclapply(range.paths,
                  makeArchs,
                  paths=paths,
                  mc.cores=num.cores,
                  mc.preschedule=TRUE) 

### Making the list.
row.count <- mclapply(1:num.paths,
                      function(x) { return(dim(archs[[x]])[1]) },
                      mc.cores=3, mc.preschedule=TRUE)
row.count <- sum(unlist(row.count))
geo.matrix <- matrix(nrow=row.count, ncol=4)
airline.matrix <- matrix(rep("", row.count), nrow=row.count, ncol=1)

j <- 1
while (j < (row.count - 1)) {
    for (k in 1:num.paths) {
        m <- archs[[k]]
        m.length <- dim(m)[1]
        geo.matrix[j:(j + m.length - 1),] <- m
        airline.matrix[j:(j + m.length - 1)] <- rep(as.character(paths[k,1]),
                                                    (m.length - 1))
        j <- j + m.length # - 1
    }
}

dimnames(geo.matrix) <- list(NULL, c("lon", "lat", "elon", "elat"))
geo.frame <- as.data.frame(geo.matrix)
geo.frame$airline <- as.factor(as.character(airline.matrix))

### Adding plots.
gcpp <- gcp + geom_segment(aes(xend=elon,yend=elat, alpha=airline),
                          data=geo.frame,
                          colour="blue",
                          alpha=1/4)

### Adding city points
unlabled.plot <- gcpp + geom_point(aes(x=lon, y=lat),
                  data=paths,
                  colour="black",
                  alpha=1/2,
                  size=1)

## Trim it
library(car)
planes <- sort(summary(paths$airline), decreasing=TRUE)
planes.top <- planes[1:6]
planes.bottom <- planes[7:length(planes)]
geo.frame$Airline <- factor(geo.frame$airline, levels=factor(names(planes)))
geo.frame$Airline <- recode(geo.frame$Airline, "names(planes.bottom)='Other'")
final.plot <- gcp + geom_segment(aes(xend=elon,yend=elat, colour=Airline),
                                 data=geo.frame,
                                 alpha=2/5) +
    geom_point(aes(x=lon, y=lat),
               data=paths,
               colour="black",
               alpha=1/2,
               size=1)

### Ok, I only care about a few airlines...gonna hard code this.
legend.labels <- c("United Airlines", "Lufthansa", "US Airways",
                   "Singapore Airlines", "Air Canada", "Air New Zealand",
                   "Other")

### Lets pick a colorbrewer set if we want to show frequency vs
### category.
### http://colorbrewer2.org/?type=qualitative&scheme=Paired&n=7
legend.values <- c("United Airlines"="#a6ceff", "Lufthansa"="#e31a1c",
                   "US Airways"="#fb9a99", "Singapore Airlines"="#33A02C",
                   "Air Canada"="#b2df8a", "Air New Zealand"="#1f78b4",
                   "Other"="#fdbf6f")

### Fin.
final.plot + scale_colour_manual(values=legend.values, breaks=legend.labels)
### ggsave(filename="final.plot.pdf", plot=final.plot, width=14)
ggsave(filename="final.plot.png", plot=final.plot, width=7, height=3)
(final.plot)

## Make TreeMap of Countries (main) by Airline (subcategory)
air2country <- data.frame(airline=paths$airline, country=paths$ecountry)
a2c <- as.data.frame(table(air2country))
a2c <- a2c[a2c$Freq > 0, ]

library(treemap)
# tmPlot(a2c, index=c("country", "airline"), vSize="Freq")
treemap(a2c,
        index = c("country", "airline"),
        vSize = "Freq",
        vColor = "airline",
        type = "categorical",
        title = "Who I fly to where",
        title.legend = "",
        algorithm = "pivotSize",
        sortID = "Freq",
        fontsize.legend = 9,
        fontsize.labels = 9,
        lowerbound.cex.labels = 0.25,
        overlap.labels = 0,
        align.labels = list(c("right", "bottom"),
                            c("left", "top")))
