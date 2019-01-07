
# setting environment -----------------------------------------------------

library(Rfacebook)
library(ggplot2)
library(scales)
library(zoo)

setwd("~/")

fb_oauth <- fbOAuth(app_id="", app_secret="", extended_permissions = TRUE)

save(fb_oauth, file="fb_oauth")

load("fb_oauth")

# building functions ------------------------------------------------------

format_facebook_date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

my_aggregate_metric <- function(metric, facebook_data) {
  m <- aggregate(facebook_data[[paste0(metric, "_count")]], list(day = facebook_data$day), 
                 mean)
  m$day <- as.Date(m$day)
  m$metric <- metric
  m$m_av <- coredata(rollapply(zoo(m$x, m$day), 7, mean, na.rm = T, align = "right", partial = T))
  return(m)
}

my_utility <- function(data, metrics){
  data$datetime <- format_facebook_date(data$created_time)
  data$month <- format(data$datetime, "%Y-%m")
  data$day <- format(data$datetime, "%Y-%m-%d")
  df.list <- lapply(metrics, my_aggregate_metric, facebook_data = data)
  df <- do.call(rbind, df.list)
  return(df)
}

# loading data ------------------------------------------------------------

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

a_post <- getPost(post = "1101539456638107_1309367002522017",  n = 3000, token = fb_oauth, reactions = T)
comments <- a_post$comments
reactions <- a_post$reactions

reactions$comments <- as.numeric(reactions$from_name %in% comments$from_name)

table(reactions$comments)

reactions$angry <- 0
reactions$angry[reactions$from_type == "ANGRY"] <- 1

summary(glm(reactions$comments ~ 1, family = "binomial"))

summary(glm(reactions$comments ~ reactions$angry, family = "binomial"))
my_glm <- summary(glm(reactions$comments ~ reactions$angry, family = "binomial"))
exp(my_glm$coefficients[,1])


(table(reactions$from_type))

a_comment <- getCommentReplies(comment_id = "1309367002522017_1309375542521163", n = 100, token = fb_oauth)

# creating metrics --------------------------------------------------------

metrics <- c("comments", "angry", "haha")

df_rd <- my_utility(rd, metrics)

df_gj <- my_utility(gj, metrics)

df_fa <- my_utility(fa, metrics)

# visualize evolution in metric

ggplot(df_rd, aes(x = day, y = x, group = metric, color = metric)) + 
  geom_line(aes(color = metric), size = .5, alpha = .4) + 
  geom_line(aes(x = day, y = m_av, color = metric), size = 1) + 
  scale_x_date(date_breaks = "week", labels = date_format("%d-%m-%Y")) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  ylab("Average_count_per_day") + 
  ggtitle("Facebook page trends Revolucion Democratica", subtitle = "Moving Average (7 days) in solid line")

ggplot(df_gj, aes(x = day, y = x, group = metric, color = metric)) + 
  geom_line(aes(color = metric), size = .5, alpha = .4) + 
  geom_line(aes(x = day, y = m_av, color = metric), size = 1) + 
  scale_x_date(date_breaks = "week", labels = date_format("%d-%m-%Y")) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  ylab("Average_count_per_day") + 
  ggtitle("Facebook page trends Giorgio Jackson Diputado", subtitle = "Moving Average (7 days) in solid line")

ggplot(df_fa, aes(x = day, y = x, group = metric, color = metric)) + 
  geom_line(aes(color = metric), size = .5, alpha = .4) + 
  geom_line(aes(x = day, y = m_av, color = metric), size = 1) + 
  scale_x_date(date_breaks = "week", labels = date_format("%d-%m-%Y")) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  ylab("Average_count_per_day") + 
  ggtitle("Facebook page trends Frente Amplio", subtitle = "Moving Average (7 days) in solid line")

ggplot(reactions, aes(from_type)) +
  geom_bar(aes(fill = as.factor(comments))) +
  xlab("Reaction type") +
  guides(fill=guide_legend(title="Comment?")) +
  ggtitle("Facebook post reactions count")
