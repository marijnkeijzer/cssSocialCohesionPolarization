library('foreign')
library('haven')
library('here')
library('ggplot2')
library("pals")

theme_set(
  theme_linedraw() +
    theme(
      text = element_text(size = 8),
      axis.text = element_text(size = 7),
      legend.key.spacing.y=unit(-0.2,'cm'),
      legend.key = element_rect(fill = "transparent")
    )
)

load(here('data_affpol/affective_polarization.Rdata'))

meta_parties <- meta_parties[order(meta_parties$study,meta_parties$letter),]
europe_acr <- c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")
europe <- c("Belgium","Switzerland","Germany","Spain","Finland","France",
               "Great Britain","Hungary","Ireland","Netherlands","Norway",
               "Poland","Portugal","Sweden","Slovenia")
north_america_acr <- c("CA","MX","US")
north_america <- c("Canada", "Mexico", "United States of America")
df <- df[df$country %in% c(europe,north_america),]
df$country <- factor(df$country, levels=c(europe,north_america), labels=c(europe_acr,north_america_acr))

# calculate affective polarization scores from feeling thermometer
### #df <- df[sample(1:nrow(df), 5000),]
### df <- df[!is.na(df$letter),]
### inloveindex <- match(tolower(df$letter), letters[1:9])+5
### df$inlove <- NA
### for(i in 1:nrow(df)){
###  cat("processing ", i, " of ", nrow(df), " (", round(i/nrow(df)*100,0), "%)", "\r", sep="")
###  df$inlove[i] <- df[i, inloveindex[i]]
###  flush.console()
### } 
### df <- df[!is.na(df$inlove),]
### 
### df$outlove <- NA
### for(i in 1:nrow(df)) {
###  # track progress
###  cat("processing ", i, " of ", nrow(df), " (", round(i/nrow(df)*100,0), "%)", "\r", sep="")
### 
###  outlovevalues <- df[i, 6:14]
###  outloveweights <- meta_parties$percvote[df$study[i]==meta_parties$study]
###  outlovevalues <- outlovevalues[-match(tolower(df$letter[i]), letters[1:9])]
###  outloveweights <- outloveweights[-match(tolower(df$letter[i]), letters[1:9])]
###  outlovevalues <- outlovevalues * outloveweights
### 
###  df$outlove[i] <- sum(outlovevalues, na.rm=T) / sum(outloveweights, na.rm=TRUE)
###  flush.console()
### }
### df$ap <- abs(df$inlove - df$outlove * 10)
### save(df, file='data_affpol/ap_scores_europe-northamerica.Rdata')
load('data_affpol/ap_scores_europe-northamerica.Rdata')

ggplot(df, aes(x=inlove)) + geom_histogram()
ggplot(df, aes(x=outlove)) + geom_histogram()
ggplot(df, aes(x=ap)) + geom_histogram()

df$continent <- NA
df$continent[df$country %in% europe_acr] <- 'Europe'
df$continent[df$country %in% north_america_acr] <- 'North America'

ggplot(df[df$country=="DE",], aes(x=year, y=ap, color=country)) + stat_summary(fun.y='mean', geom='line')
ggplot(df[df$country=="GB",], aes(x=year, y=ap, color=country)) + stat_summary(fun.y='mean', geom='line')
ggplot(df[df$country=="FR",], aes(x=year, y=ap, color=country)) + stat_summary(fun.y='mean', geom='line')

country_year <- data.frame(
  country=c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI","CA","MX","US"),
  year=   c(1999,2007,2009,2004,2003,2007,2005,1998,2011,2006,2013,2001,2015,2014,2004,2019,2012,2020)
)
country_year_avg_ap <- aggregate(ap ~ country + year, data = df, FUN = mean)
# select only the observations that are in country_year
country_year_avg_ap <- country_year_avg_ap[paste(country_year_avg_ap$country,country_year_avg_ap$year) %in% paste(country_year$country,country_year$year),]
country_year_avg_ap$continent <- NA
country_year_avg_ap$continent[country_year_avg_ap$country %in% europe_acr] <- 'Europe'
country_year_avg_ap$continent[country_year_avg_ap$country %in% north_america_acr] <- 'North America'


(p1 <- ggplot(df, aes(x=year, y=ap)) +
  geom_abline(intercept=0, slope=0, color='black', lwd=.15) +
  #geom_smooth(aes(color=country), method='lm', lwd=.5, se=F, span=.5, alpha=.5) +
  stat_summary(aes(group=country, color=continent), geom='line', lwd=.3, se=F, span=.5, alpha=0.5) +
  #stat_summary(aes(group=country), geom='point', shape=16, size=3) +
  #stat_summary(aes(fill=country), geom='point', alpha=.5, shape=21, color='white') +
  geom_smooth(aes(color=continent), method='lm', lwd=1, se=F, span=.5) +
  geom_label(data=country_year_avg_ap, aes(x=year,y=ap,label=country,color=continent), size=2, position=position_dodge2(width=0.5)) +
  scale_color_manual(name=NULL, values=c("blue","red")) +
  scale_fill_manual(name=NULL, values=cols25(18)) +
  scale_y_continuous(name="affective polarization score") + 
  scale_x_continuous(name=NULL, breaks=seq(1996,2020,3)) + 
  coord_cartesian(ylim=c(17, 64)) +
  guides(byrow=TRUE) +
  theme(panel.grid.minor=element_blank(), legend.position=c(0.16,0.93), legend.background = element_rect(fill=NA)))
ggsave('fig_affect.png', width=3.75, height=4)
ggsave('fig_affect.tiff', width=3.75, height=4)

