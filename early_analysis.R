library(Rfacebook)

setwd("C:/Users/Usuario/Documents/My_R_projects")

fb_oauth <- fbOAuth(app_id="", app_secret="", extended_permissions = TRUE)

save(fb_oauth, file="fb_oauth")

load("fb_oauth")

rd <- getPage("RevolucionDemocraticaChile", reactions = TRUE, fb_oauth, n = 1000, since='2017/01/01')
gj <- getPage("giorgiodiputado", reactions = TRUE, fb_oauth, n = 1000, since='2017/01/01')
fa <- getPage("FrenteAmpliodeChile", reactions = TRUE, fb_oauth, n = 1000, since='2017/01/01')
am <- getPage("Mayolistas", reactions = TRUE, fb_oauth, n = 1000, since='2017/01/01')

write.csv2(rd, "rd.csv")
write.csv2(gj, "gj.csv")
write.csv2(fa, "fa.csv")
write.csv2(am, "am.csv")

rd <- read.csv2("rd.csv", stringsAsFactors = F)
gj <- read.csv2("gj.csv", stringsAsFactors = F)
fa <- read.csv2("fa.csv", stringsAsFactors = F)
am <- read.csv2("am.csv", stringsAsFactors = F)

## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
## aggregate metric counts over month (day)
aggregate.metric <- function(metric) {
  m <- aggregate(rd[[paste0(metric, "_count")]], list(day = rd$day), 
                 mean)
  m$day <- as.Date(m$day)
  m$metric <- metric
  return(m)
}

my_aggregate_metric <- function(metric, facebook_data) {
  m <- aggregate(facebook_data[[paste0(metric, "_count")]], list(day = facebook_data$day), 
                 mean)
  m$day <- as.Date(m$day)
  m$metric <- metric
  return(m)
}

my_aggregate_metric("angry", rd)


# create data frame with average metric counts per day
rd$datetime <- format.facebook.date(rd$created_time)
rd$month <- format(rd$datetime, "%Y-%m")
rd$day <- format(rd$datetime, "%Y-%m-%d")
df.list <- lapply(c("angry", "comments", "shares"), my_aggregate_metric, facebook_data = rd)
df <- do.call(rbind, df1.list)

# visualize evolution in metric
library(ggplot2)
library(scales)
ggplot(df, aes(x = day, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(date_breaks = "day", labels = date_format("%d")) + 
  theme_bw() + 
  theme(axis.title.x = element_blank())





aggregate.metric <- function(metric) {
  m <- aggregate(later[[paste0(metric, "_count")]], list(day = later$day), 
                 mean)
  m$day <- as.Date(m$day)
  m$metric <- metric
  return(m)
}
# create data frame with average metric counts per day
later$datetime <- format.facebook.date(later$created_time)
later$month <- format(later$datetime, "%Y-%m")
later$day <- format(later$datetime, "%Y-%m-%d")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df1 <- do.call(rbind, df.list)

# visualize evolution in metric
library(ggplot2)
library(scales)
ggplot(df1, aes(x = day, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(date_breaks = "day", labels = date_format("%d")) + 
  theme_bw() + 
  theme(axis.title.x = element_blank())



