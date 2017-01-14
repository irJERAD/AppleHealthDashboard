library(dplyr)
library(ggplot2)
library(lubridate)
library(XML)
library(plotly)
#Sys.setenv("plotly_username"="irJERAD")
#Sys.setenv("plotly_api_key"="r9QimQhziXsXTjFTlcbg")
# Libraries for colors
library(wesanderson)
library(RColorBrewer)

# Set path to export
exportPath <- "~/Documents/Data-Apps/AppleHealthDashboard/apple_health_export/export.xml"

#load apple health export.xml file
xml <- xmlParse(exportPath)

#transform xml file to data frame - select the Record rows from the xml file
df <- XML:::xmlAttrsToDataFrame(xml["//Record"])

#make value variable numeric
## TODO fix error NAs introduced by coercion  (looks like a single extra row is added)
df$value <- as.numeric(as.character(df$value))

#make endDate in a date time variable POSIXct using lubridate with Pacific time zone
df$endDate <-ymd_hms(df$endDate,tz="America/Los_Angeles")


##add in year month date dayofweek hour columns
df$month<-format(df$endDate,"%m")
df$year<-format(df$endDate,"%Y")
df$date<-format(df$endDate,"%Y-%m-%d")
df$dayofweek <-wday(df$endDate, label=TRUE, abbr=FALSE)
df$hour <-format(df$endDate,"%H")

# subset data frame
steps <- df %>% filter(type == 'HKQuantityTypeIdentifierStepCount')
distWR <- df %>% filter(type == 'HKQuantityTypeIdentifierDistanceWalkingRunning')
heart <- df %>% filter(type == 'HKQuantityTypeIdentifierHeartRate')
mass <- df %>% filter(type == 'HKQuantityTypeIdentifierBodyMass')

# Boxplot of heart mean(heartrate) per day factored by year
heart %>%
  filter(type == 'HKQuantityTypeIdentifierHeartRate') %>%
  group_by(dayofweek,date,year) %>%
  summarize(steps=mean(value)) %>%
  #print table steps by date by day of week
  ggplot(aes(x=dayofweek, y=steps)) + 
  geom_boxplot(aes(fill=(year))) + 
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())

# Heat Map of steps
steps %>%
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



# steps summary statistics by month for 2015
steps %>%
  group_by(date,month,year) %>%
  summarize(steps=sum(value)) %>%
  filter(year==2015) %>%
  group_by(month) %>%
  summarize(mean = round(mean(steps), 2), sd = round(sd(steps), 2), 
            median = round(median(steps), 2), max = round(max(steps), 2), 
            min = round(min(steps), 2),`25%`= quantile(steps, probs=0.25),
            `75%`= quantile(steps, probs=0.75))
# steps boxplot data by day of week year
steps %>%
  group_by(dayofweek,date,year) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by date by month by year
  #print (n=100) %>%
  ggplot(aes(x=dayofweek, y=steps)) + 
  geom_boxplot(aes(fill=(year))) + 
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())
# steps summary statistics by day of week for 2015
steps %>%
  group_by(dayofweek,date,year) %>%
  summarize(steps=sum(value)) %>%
  filter(year==2015) %>%
  group_by(dayofweek) %>%
  summarize(mean = round(mean(steps), 2), sd = round(sd(steps), 2), 
            median = round(median(steps), 2), max = round(max(steps), 2), 
            min = round(min(steps), 2),`25%`= quantile(steps, probs=0.25),
            `75%`= quantile(steps, probs=0.75)) %>%
  arrange(desc(median))


#steps heatmap day of week hour of day
steps %>%
  group_by(date,dayofweek,hour) %>% 
  summarize(steps=sum(value)) %>% 
  group_by(hour,dayofweek) %>% 
  summarize(steps=sum(steps)) %>% 
  arrange(desc(steps)) %>%
  #print table steps by date by month by year
  #print (n=100) %>%
  ggplot(aes(x=dayofweek, y=hour, fill=steps)) + 
  geom_tile() + 
  scale_fill_continuous(labels = scales::comma, low = 'white', high = 'red') +
  theme_bw() + 
  theme(panel.grid.major = element_blank())


# plotly with ggplot2 for interactive graphics
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
  labs(title = "My Heart Rate From April 13th, 2015 through current data Export Jan 5th 2017",
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
## first usage of ggplotly() to create plot
# ggplotly(h, tooltip = "text")


### --- 
# Function that translates 24 hours clock to 12 hours with AM and PM
## -- TO FIX -- ## The format.date has %p which prints AM or PM
f <- function(x){if(x == 0){x <- paste("12 AM")} else if(x == 12){paste("12 PM")} else if(x > 12){ x <- paste(x - 12, "PM")} else {paste(x, "AM")}}

# create new varable for the 12 hours clock
steps$hour12 <- lapply(as.numeric(steps$hour), f)

### TODO unfinished
## plotly heatmap
p <- steps %>%
  group_by(date,dayofweek,hour12) %>%
  summarise(stp = sum(value)) %>%
  group_by(hour12, dayofweek) %>%
  summarise(stp = sum(stp)) %>%
  arrange(desc(stp)) %>%
  plot_ly(z = steps$hour12, type = "heatmap")


#########


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
format(d$date, format="%d of %b")

low.mass <- list(
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
  #ay = 140
  ayref = 'y | 1',
  ay = -125,
  # set arrow x offset relative to x2
  axref = 'x2',
  ax = 300)

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
         annotations = low.mass)
p1

p2 <- plot_ly(d) %>%
  # dplyr::arrange(date) %>%
  # group_by(date) %>%
  # summarise(steps = mean(steps), mass = mean(mass)) %>%
  add_trace(x = ~date, y = ~steps, type = 'bar', name = 'Steps',
            hoverinfo = 'text',
            text = ~paste("I took", steps, "steps on", date)) %>%
  add_trace(x = ~date, y = ~mass, type = 'scatter', mode = 'lines',
            name= 'Lbs',
            yaxis = 'y2',
            hoverinfo = 'text',
            text = ~paste("I weighed in at", mass, "on", date)) %>%
  layout(title = paste("Number of steps taken with my weight drawing a line across time<br>",
                       "<sup>Showing nothing of significance ",
                       "as my Body Mass doesn't change often</sup>"),
         xaxis = list(title = "", type = 'category'),
         yaxis = list(side = 'left', title = 'Steps taken',
                      showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'My Weight in Pounds',
                       showgrid = FALSE, zeroline = FALSE))

p <- subplot(p1, p2, nrows = 2)
p
p1
p2

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