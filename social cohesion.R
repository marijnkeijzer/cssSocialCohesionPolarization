library('here')
library('psych')
library('ggplot2')
library('patchwork')
library('pals')

theme_set(
  theme_linedraw() +
    theme(
      text = element_text(size = 8),
      axis.text = element_text(size = 7),
      legend.key.spacing.y=unit(-0.2,'cm')
    )
)

## https://ess.sikt.no/en/data-builder/?rounds=0+1+2+3+4+5+6+7+8+9+10+11&seriesVersion=904&tab=download&variables=0.8+1.0.58-62

df <- read.csv(here('data_soccoh/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.csv'))

df <- df[df$cntry %in% c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI"),]

df$trstlgl[df$trstlgl > 10] <- NA
df$trstplc[df$trstplc > 10] <- NA
df$trstplt[df$trstplt > 10] <- NA
df$trstprl[df$trstprl > 10] <- NA
df$trstprt[df$trstprt > 10] <- NA
df$ppltrst[df$ppltrst > 10] <- NA

df$year <- 2000 + df$essround*2

ggplot(df, aes(x=ppltrst)) +
  geom_histogram(bins=11, fill='lightblue', color='black') +
  scale_x_continuous(breaks=seq(0,10,1))

factors <- fa(r = df[,c('trstlgl','trstplc','trstplt','trstprl','trstprt')], rotate = "varimax")
summary(factors)
print(factors)
# 1 factor is sufficient
factors <- fa(r= df[,c('trstlgl','trstplc','trstplt','trstprl','trstprt')], nfactors = 1, rotate = "varimax")
df$instrst <- factors$scores[,1]

df <- df[!is.na(df$instrst),]

set.seed(1969)
cntry_year <- data.frame(
  cntry=c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI"),
  year=sample(seq(2004,2022,2),15,replace=TRUE)
)

cntry_year_avg_instrst <- aggregate(instrst ~ cntry + year, data = df, FUN = mean)
cntry_year_avg_ppltrst <- aggregate(ppltrst ~ cntry + year, data = df, FUN = mean)
# select only the observations that are in cntry_year
cntry_year_avg_instrst <- cntry_year_avg_instrst[paste(cntry_year_avg_instrst$cntry,cntry_year_avg_instrst$year) %in% paste(cntry_year$cntry,cntry_year$year),]
cntry_year_avg_ppltrst <- cntry_year_avg_ppltrst[paste(cntry_year_avg_ppltrst$cntry,cntry_year_avg_ppltrst$year) %in% paste(cntry_year$cntry,cntry_year$year),]

# PANEL A
(p1 <- ggplot(df, aes(x = year, y = instrst)) +
  stat_summary(geom= "line", fun = 'mean', lwd=0.3, aes(color=cntry, group=cntry)) +
  stat_summary(geom= "line", fun = 'mean', lwd=1, color='blue') +
  geom_label(data=cntry_year_avg_instrst, aes(x=year,y=instrst,label=cntry,color=cntry),size=2, position=position_dodge2(width=0.5)) +
  scale_x_continuous(breaks=seq(2004,2022,2)) +
  scale_color_manual(values=cols25(15)) +
  labs(y = "institutional trust", x=NULL, color=NULL) +
  theme(legend.position = 'none', panel.grid.minor=element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)))
# PANEL B
(p2 <- ggplot(df, aes(x=year, y=ppltrst)) +
  stat_summary(geom= "line", fun = 'mean', lwd=0.3, aes(color=cntry, group=cntry)) +
  stat_summary(geom= "line", fun = 'mean', lwd=1, color='blue') +
  geom_label(data=cntry_year_avg_ppltrst, aes(x=year,y=ppltrst,label=cntry,color=cntry),size=2, position=position_dodge2(width=0.5)) +
  scale_x_continuous(breaks=seq(2004,2022,2)) +
  scale_color_manual(values=cols25(15)) +
  labs(y = "interpersonal trust", x=NULL, color=NULL) +
  theme(legend.position = 'none', panel.grid.minor=element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)))

# COMBINED FIGURE
p1 + p2 + 
  plot_layout(guides="collect") +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12, face="bold"))

ggsave("fig_cohesion.png",  height = 5, width = 6.9)  
ggsave("fig_cohesion.tiff", height = 5, width = 6.9)  


