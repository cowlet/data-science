# Pull data from Open Glasgow's traffic events feed, and parse
# into a CSV file of interesting traffic information.
# http://data.glasgow.gov.uk/dataset/glasgow-road-network-traffic-events-feed

library("RJSONIO")
library("ggmap")

# Parse a situationRecord for its interesting details
parse.situation <- function(sit)
{
    # Location descriptor position varies between a single point, and a list of points.
    # Try to fetch the descriptor. This will work if it's a single point.
    loc.descriptor <- sit$situationRecord$groupOfLocations$locationContainedInGroup$tpegpointLocation$point$name$descriptor
    
    # If it is a list of points, the descriptor will be null instead:
    if (is.null(loc.descriptor))
    {
        # In this case, use the first point's descriptor instead.
        points.list <- sit$situationRecord$groupOfLocations$locationContainedInGroup$tpegpointLocation$point$name
        loc.descriptor <- points.list[[1]]$descriptor
    }

    return (data.frame(as.POSIXct(strptime(sit$situationRecord$situationRecordCreationTime, format="%Y-%m-%dT%H:%M:%S")),
                       as.POSIXct(strptime(sit$situationRecord$validity$validityTimeSpecification[1], format="%Y-%m-%dT%H:%M:%S")),
                       as.POSIXct(strptime(sit$situationRecord$validity$validityTimeSpecification[2], format="%Y-%m-%dT%H:%M:%S")),
                       sit$situationRecord$validity$validityStatus,
                       sit$situationRecord[[length(sit$situationRecord)]],
                       loc.descriptor,
                       as.numeric(sit$situationRecord$groupOfLocations$locationContainedInGroup$tpegpointLocation$point$pointCoordinates[1]),
                       as.numeric(sit$situationRecord$groupOfLocations$locationContainedInGroup$tpegpointLocation$point$pointCoordinates[2]),
                       sit$situationRecord$nonGeneralPublicComment$comment,
                       row.names=NULL))
}

# The source file has a missing newline at the end, so suppress the warning
json <- readLines(url("http://dashboard.glasgow.gov.uk/api/live/trafficEvents.php?type=json"), warn=FALSE)
data.json <- fromJSON(json)

# Apply parse.situation to all traffic event "situations". These are stored in the JSON
# data as an array of objects at `payloadPublication.situation`.
data <- do.call(rbind, Map(parse.situation, data.json$payloadPublication$situation))
colnames(data) <- c("creation", "start", "end", "status", "event", "location", "latitude", "longitude", "description")

# Store the parsed information
write.table(data, file="traffic_events.csv", sep=",", row.names=FALSE)

# Grab a map of Glasgow image
map.img <- get_googlemap(center="Glasgow,UK", zoom=12, maptype="roadmap")

# Generate event colours: ongoing in red; past or future events in blue
colour <- mapply(function (s, e) { if (s < Sys.time() && Sys.time() < e) { "red" } else { "blue" }}, data[,2], data[,3], SIMPLIFY=TRUE)

# Plot the map with traffic incidents on top
print(ggmap(map.img) + geom_point(aes(x=longitude, y=latitude, shape=event, color=colour), data=cbind(data, colour), size=3) + scale_colour_identity())

