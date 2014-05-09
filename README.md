GreatCircles
============

A demo of making great circle plots in R from airport codes using a
very bad map projection.  The code here is setup to use a dump from
http://openflights.org/ but the sample data is just a subset.  The
full code is inline and commented out if you get a full export. We'll
make a pretty map with great circle plots to show you every where you
flew.  Then we'll break it down by carrier.

For the point of this instructional, you'll need the following librarires:

* `car`
* `maps`
* `ggplot2`
* `geosphere`
* `multicore`

the last one just being for the fun of it really.

The code itself is documented explaining the step by step.  Just
source the `start.R` code.  This code was put together for a talk to
the [Singapore R Users group](http://www.meetup.com/R-User-Group-SG/).

The data in `flights.csv` is fictional.  Just so you know that Virgin
America doesn't have a SFO to SFO flight.

[@ayman](https://github.com/ayman)
