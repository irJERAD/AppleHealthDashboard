---
title: "Apple Health Dashboard"
output: 
  flexdashboard::flex_dashboard:
    storyboard: true
    social: menu
    source: embed
---

```{r setup, include=FALSE}
# This script will be shown in the source code 
# but hidden from the dashboard for practical purposes

# upload libraries to be used in any of the code blocks at once ahead of time
library(flexdashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(XML)
library(plotly)
library(wesanderson)

# Set path to export
exportPath <- "~/Documents/Data-Apps/AppleHealthDashboard/apple_health_export/export.xml"

#load apple health export.xml file
xml <- xmlParse(exportPath)

#transform xml file to data frame - select the Record rows from the xml file
df <- XML:::xmlAttrsToDataFrame(xml["//Record"])

#make value variable numeric
## TODO fix error NAs introduced by coercion  (looks like a single extra row is added)
df$value <- as.numeric(as.character(df$value))

#make endDate in a date time variable POSIXct using lubridate with eastern time zone
df$endDate <-ymd_hms(df$endDate,tz="America/Los_Angeles")


##add in year month date dayofweek hour columns
df$month<-format(df$endDate,"%m")
df$year<-format(df$endDate,"%Y")
df$date<-format(df$endDate,"%Y-%m-%d")
df$dayofweek <-wday(df$endDate, label=TRUE, abbr=FALSE)
df$hour <-format(df$endDate,"%H")

# create sub data frames for quicker interaction
heart <- df %>% filter(type == 'HKQuantityTypeIdentifierHeartRate')
steps <- df %>% filter(type == 'HKQuantityTypeIdentifierStepCount')
mass <- df %>% filter(type == 'HKQuantityTypeIdentifierBodyMass')
```

### Leaflet is a JavaScript library for creating dynamic maps that support panning and zooming along with various annotations.

```{r map of home}
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-122.4125580, lat=37.7781400, popup="Current Residence of Jerad")
```

***

https://github.com/irJERAD/AppleHealthDashboard

- Here you can see a Pin of my home in San Francisco!

- Interactive panning/zooming

- Compose maps using arbitrary combinations of map tiles, markers, polygons, lines, popups, and GeoJSON.

- Create maps right from the R console or RStudio

- Embed maps in knitr/R Markdown documents and Shiny apps

- Easily render Spatial objects from the sp package, or data frames with latitude/longitude columns

- Use map bounds and mouse events to drive Shiny logic


### ggplot2 - while well known and used - has a very diverse set of plot types; while old can be boring the selection is indispensable to data exploration and discovery.

```{r heatmap steps}
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(date,dayofweek,hour) %>% 
  summarize(steps=sum(value)) %>% 
  group_by(hour,dayofweek) %>% 
  summarize(steps=sum(steps)) %>% 
  arrange(desc(steps)) %>%
  #print table steps by date by month by year
  ggplot(aes(x=dayofweek, y=hour, fill=steps)) + 
  geom_tile() + 
  scale_fill_continuous(labels = scales::comma, low = 'white', high = 'red') +
  theme_bw() + 
  theme(panel.grid.major = element_blank())
```

***

Here I used the [ggplot2](https://github.com/hadley/ggplot2-book) libraries geometric tile plotting ability to create a heat map of my walking data

- Week Days around 6pm were the busiest time
    - This makes sense since that is when I am walking home from work

- This Library was written by one of my favorite data scientists


### More ggplot this time box plot - should make this a lattice

```{r step by step}
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(dayofweek,date,year) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by date by month by year
  ggplot(aes(x=dayofweek, y=steps)) + 
  geom_boxplot(aes(fill=(year))) +
  # uses nice default blue-ish palette
  scale_fill_brewer() +
  # sets background theme to my rediculous standards
  # Note: still not completely satisfied but productivity beckons me
  theme(panel.grid.major.x = element_blank(), panel.grid.minor =
          element_blank(), panel.grid.major.y = element_line(color = "gray",
                                                             size = 0.2),
        panel.background = element_rect(color = "black", fill = "#fbfbfb")) + scale_fill_brewer()
```

***

With 2017 only just starting (I am writing this at 10am on Jan 5th and only started last evening). A boxplot does really help much.

I considered filtering out this new year. However I am determined to impliment an automation that will continue to take my exported data and update this page - mostly because this is a static site and I love getting as much as possible from as little as necessary

### Plotly provides bindings to the plotly.js library and allows you to easily translate your ggplot2 graphics into an interactive web-based version.
```{r ggplot + Plotly}
# plotly with ggplot2 for interactive graphics
h <- heart %>%
  # text is the tool tip
  ggplot(aes(x = date, y = value,
             # create custome hover tooltip variables
             text = (paste("BPM:", value,
                           "<br>Source:", sourceName,
                           "<br>Date", date)))) +
  # Color and implied key
  geom_point(aes(colour=sourceName)) +
  scale_color_manual(name = "Source of<br>Collected Data", values = wes_palette(n=5, name = "Darjeeling")) +
  labs(title = "My Heart Rate From April 13th, 2015<br> through current data Export Jan 5th 2017",
       x = "2015 --- Date --- 2017", y = "Beats per min") +
  theme(plot.title = element_text(size = 10,
                                  color = "#FF0000",
                                  face = "bold"),
        legend.text = element_text(size = 6,
                                   color = "#00c8ff",
                                   face = "italic"),
        legend.title = element_text(size = 13,
                                    color = "#00A08A",
                                    face = "italic"),
        axis.title.x = element_text(size = 12,
                                    color = "#ffffff"),  
        axis.title.y = element_text(size = 12,
                                    color = "#ffffff"),
        plot.background = element_rect(fill = "#5BBCD6"))

## TODO nticks on x axis $xaxis$ntick needs to be a number greater than 0
# sets the number of ticks on the x axis

## TODO tick angle $colorbar$tickangle -90 is vertical
# Create plotly object
p <- plotly_build(h)
# center legend
# replace plotly objects annotations settings
p$x$layout$annotations[[1]]$y = 0.4
p$x$layout$legend$y = 0
# Customize the hoveron tooltip content
# need to go through each variable in legend source
for(i in 1:length(p$x$data)){
  p$x$data[[i]]$text <- paste("Beats Per Min:", heart$value,
                              "<br>Source:", heart$sourceName,
                              "<br>Date", heart$date)
}
p
```

***
**Lessons on Plotly + ggplot2 = ggplotly():**

- I definitely got a little over zealous on this one
    - So many colors, too many colors
        - Did I really need all those colors?
        - I hope you got a chuckle out of those colors because I know they didn't add anything of substance

- I was WAY over committed
    - adding JavaScript, HTML, and interactive elements to my favorite plotting tool, of course I would get a little... obsessed
    - This added a lot of time to things I could have just done using plotly alone and gotten faster and better results, but that's a cost to knowledge I am well acquainted with and one I respect

- I should just make Plotly my go to library for interactive graphics
    - This thing is *way* cool
    - sorry Hadley ;(
    
### Plotly makes it easy to comprehensively place multiple graphs over each other
```{r line over bar graph}
# line chart over bar chart with plotly
# only keep index values that match up with Body Mass records [43]
keep <- c(which(df$type == 'HKQuantityTypeIdentifierBodyMass'))
ksteps <- steps[keep,]
kdf <- df[keep,]$date

# create d data frame with mass, steps and date
d <- data.frame(date = as.Date(kdf))
d$steps <- ksteps$value
d$mass <- mass$value

## Convert date to western converntion of Month, Day, Year
# NOTE: I believe using a date object causes the gaps in the graph
# ^ this makes sense since time is continuously accounted for along the xaxis
d$date <- as.Date(d$date, format(d$date, format = "%m.%d.%Y"))

## organize and consolidate data frame
# sources have multiple readings on the same day as different amounts
# by creating single days and using the variables mean, a good representation should be made
d <- d %>%
  dplyr::arrange(date) %>%
  group_by(date) %>%
  summarise(steps = mean(steps), mass = mean(mass))

# d$date <- format(d$dateas.Date(d$date, format = paste("%d", "of", "%b"))
# Build the annotations
#format(d$date, format="%d of %b")

high.mass <- list(
  xref = 'x2',
  yref = 'y2',
  x = d$date[20],
  y = d$mass[20],
  xanchor = 'left',
  yanchor = 'middle',
  text = ~paste('<b>My lowest weight was ', d$mass[10], 'on', d$date[10],"</b>"),
  font = list(family = 'Arial',
              size = 15,
              color = 'rgba(251,90,140,1)'),
  bgcolor = 'rgba(55,217,5,1)',
  bordercolor = 'rgba(220,220,220,1)',
  borderwidth = 2,
  ## define arrow settings
  showarrow = TRUE,
  arrowcolor = 'rgba(251,90,140,1)',
  arrowwidth = 3,
  arrowhead = 7,
  ayref = 'y1',
  ay = '125',
  # set arrow x offset relative to x2
  axref = 'x',
  ax = '2015-04-30'
)
 
low.mass1.5 <- list(
  xref = 'x2',
  yref = 'y2',
  x = d$date[10],
  y = d$mass[10],
  xanchor = 'left',
  yanchor = 'middle',
  text = ~paste('<b>My lowest weight was ', d$mass[10], 'on', d$date[10],"</b>"),
  font = list(family = 'Arial',
              size = 15,
              color = 'rgba(251,90,140,1)'),
  bgcolor = 'rgba(55,217,5,1)',
  bordercolor = 'rgba(220,220,220,1)',
  borderwidth = 2,
  ## define arrow settings
  showarrow = TRUE,
  arrowcolor = 'rgba(251,90,140,1)',
  arrowwidth = 3,
  arrowhead = 7,
  ayref = 'y1',
  ay = '-75',
  # set arrow x offset relative to x2
  axref = 'x',
  ax = '2015-05-25')

p1 <- plot_ly(d) %>%
  ## Playing with multiple plots in plotly using suplot
  add_trace(x = ~date, y = ~steps, type = 'bar', name = 'Steps',
            hoverinfo = 'text',
            text = ~paste("I took", steps, "steps on", date)) %>%
  add_trace(x = ~date, y = ~mass, type = 'scatter', mode = 'lines+markers',
            line = list(size = 5, shape = "spline"),
            name= 'Lbs',
            yaxis = 'y2',
            hoverinfo = 'text',
            text = ~paste("I weighed in at", mass, "on", date)) %>%
  layout(title = paste("Number of steps taken with my weight drawing a line across time<br>",
                       "<sup>Showing nothing of significance ",
                       "as my Body Mass doesn't change often</sup>"),
         xaxis = list(title = "Date", #tickformat = "%d of %b",
                      ticklen = 5, tickangle = 0, position = 0.0,
                      tickmode = 'array', tickvals = d$date, ticktext = as.character(d$date)),
         yaxis = list(side = 'left', title = 'Steps taken',
                      showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(title = 'My Weight in Pounds', side = 'right',
                       overlaying = "y",
                       showgrid = FALSE, zeroline = FALSE),
         annotations = list(low.mass1.5, high.mass))
p1

## original annotation without spaced date xaxis
# low.mass <- list(
#   xref = 'x2',
#   yref = 'y2',
#   x = d$date[10],
#   y = d$mass[10],
#   xanchor = 'left',
#   yanchor = 'middle',
#   text = ~paste('My lowest weight was ', d$mass[10], 'on', d$date[10]),
#   font = list(family = 'Arial',
#               size = 12,
#               color = 'rgba(251,90,140,1)',
#               bgcolor = 'rbba(55,217,5)'),
#   borderwidth = 5,
#   showarrow = FALSE)
#
# p1 <- plot_ly(d) %>%
# ## Playing with multiple plots in plotly using suplot
#   add_trace(x = ~date, y = ~d$steps, type = 'bar', name = 'Steps',
#             hoverinfo = 'text',
#             text = ~paste("I took", steps, "steps on", date)) %>%
#   add_trace(x = ~date, y = ~mass, type = 'scatter', mode = 'lines+markers',
#             line = list(size = 5, shape = "spline"),
#             name= 'Lbs',
#             yaxis = 'y2',
#             hoverinfo = 'text',
#             text = ~ paste("I weighed in at", mass, "on", date)) %>%
#   layout(title = paste("Number of steps taken with my weight drawing a line across time<br>",
#                        "<sup>Showing nothing of significance ",
#                        "as my Body Mass doesn't change often</sup>"),
#          xaxis = list(title = "2015", type = 'category',#tickformat = "%d of %b",
#                       ticklen = 5, tickangle = -45, position = 0.0,
#                       tickmode = 'array', tickvals = d$date, ticktext = as.character(d$date)),
#          yaxis = list(side = 'left', title = 'Steps taken',
#                       showgrid = FALSE, zeroline = FALSE),
#          yaxis2 = list(title = 'My Weight in Pounds', side = 'right',
#                        overlaying = "y",
#                        showgrid = FALSE, zeroline = FALSE),
#          annotations = low.mass)
# p1
# 
# p2 <- plot_ly(d) %>%
#   dplyr::arrange(date) %>%
#   group_by(date) %>%
#   summarise(steps = mean(steps), mass = mean(mass)) %>%
#   add_trace(x = ~date, y = ~d$steps, type = 'bar', name = 'Steps',
#             hoverinfo = 'text',
#             text = ~paste("I took", steps, "steps on", date)) %>%
#   add_trace(x = ~date, y = ~mass, type = 'scatter', mode = 'lines',
#             name= 'Lbs',
#             yaxis = 'y2',
#             hoverinfo = 'text',
#             text = ~ paste("I weighed in at", mass, "on", date)) %>%
#   layout(title = paste("Number of steps taken with my weight drawing a line across time<br>",
#                        "<sup>Showing nothing of significance ",
#                        "as my Body Mass doesn't change often</sup>"),
#          xaxis = list(title = "", type = 'category'),
#          yaxis = list(side = 'left', title = 'Steps taken',
#                       showgrid = FALSE, zeroline = FALSE),
#          yaxis2 = list(side = 'right', overlaying = "y", title = 'My Weight in Pounds',
#                        showgrid = FALSE, zeroline = FALSE))
## Try and fix subplot view
#p <- subplot(p1, p2, nrows = 2, shareX = TRUE, shareY = FALSE)
#p
```

I have always had a difficult time accepting graphs on top of graphs.  
I have always felt the story could have been better told in a more interesting and comprehensible way.  
While I do not take credit for finding a good example of some graph on graph action that adds to the data world, it was fun playing with Plotly's library to create this completely useless graph. As you can see my Body Mass hass rarely changed and it has nothing to do with total steps taken on any day.  

There is also a huge issue here of an increadible lack of data. Body Mass is recorded by my scale and I have had a bad habbit of keeping it updated.