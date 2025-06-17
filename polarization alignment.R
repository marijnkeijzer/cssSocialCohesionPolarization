library("tidyverse")
library("arrow")
library("gt")
library("pals")
library("patchwork")
library("ggplot2")

theme_set(
  theme_linedraw() +
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 7)
  )
)

## DATA WRANGLING ----

# Data download and basic preparation: 
# This URL leads to the selection of variables at the ESS download wizard
# https://ess.sikt.no/en/data-builder/?rounds=0+1+2+3+4+5+6+7+8+9+10+11&seriesVersion=884&tab=variables&variables=1.0.21-23_30_70
# Downloaded as CSV

# Read from zip and save as parquet
read_csv(unz("data_polali/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.zip", "ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.csv")) |>
  filter(freehms <= 5, gincdif <= 5, impcntr <= 4, lrscale <= 10, euftf <= 10) |>
  mutate(
    # Scaling data to values within [-1, 1].
    # v' = -1 + 2 * (v-m)/(M-m), where m is the lowest, M the highest possible answer
    across(c(freehms, gincdif), ~ -1 + 2 * (.x - 1)/(5-1)),
    across(c(lrscale, euftf), ~ -1 + 2 * (.x - 0)/(10-0)),
    impcntr = -1 + 2* (impcntr - 1)/(4-1),
    # Flipping some scales: some questions are asked in a "negative" sense:
    # e.g. -1 --> "more immigrants" and 1 --> "less immigrants"
    across(c(freehms, gincdif, impcntr), ~ .x * -1),
    year = (essround*2 + 2000), 
    year = if_else(year == 2022, 2023, false = year)
  ) |> write_parquet("data_polali/ESS1-11.parquet")


## DATA ANALYSIS ----

# Data notes: ESS Waves 1 (2002) and 5 (2010) do not have euftf, so they are not in
# ESS Wave 11 is 2023 (not 2022) because of COVID19_Pandemic

normalize_pc <- function(value) -sign(value[3])*length(value)*value/sum(abs(value))
formatpercentage <- function(x) paste0(round(100*x, digits = 1), "%")
ess <- read_parquet("data_polali/ESS1-11.parquet")
attitudenames <- c("freehms", "gincdif", "lrscale", "impcntr", "euftf")
attitudedesc <- c(#"Homosexual", "Equality", "Political Right", "Immigrants", "EU")
 "Pro gay/lesbian",
 "Pro redistribution",
 "Political right",
 "Pro immigrants",
 "Pro EU")
attitudedesc <- c("Homosexuality", "Redistribution", "Left-right", "Immigrants", "EU")
  
# These are the countries which have data in all years covered
cntrynames <- c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")

essPCA <- ess |> 	filter(cntry %in% c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")) |> 
 select(all_of(attitudenames)) |> 
 prcomp(center = T, scale. = T)
ess_all <- essPCA$rotation |> as_tibble() |> 
	add_column(topic = factor(attitudenames), .before = 1) |> 
	add_column(topic_desc = factor(attitudedesc), .before = 2) |> 
	pivot_longer(starts_with("PC")) |> 
	arrange(name) |> 
	mutate(value = normalize_pc(value), .by = name) |> 
	mutate(expVar = essPCA$sdev^2/5,  .by = c(topic_desc, topic)) |>
	mutate(name = paste0(name, " (", formatpercentage(expVar), ")"))
# ESS by year
ess_y <- ess |> 
	filter(cntry %in% c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")) |> 
 nest(.by = year) |> 
	mutate(pca = data |> map(\(x) x |> select(all_of(attitudenames)) |> prcomp(center = T, scale. = T)),
								pca_varexp = pca |> map(\(x) x$sdev^2/5), 
								pca_pcs = map2(pca, pca_varexp, 
																							\(x,y) x$rotation |> as_tibble() |> 	
																								add_column(topic = factor(attitudenames), .before = 1) |> 
																								add_column(topic_desc = factor(attitudedesc), .before = 2) |> 
																								pivot_longer(starts_with("PC")) |> 
																								arrange(name) |> 
																								mutate(value = normalize_pc(value), .by = name) |> 
																								mutate(expVar = y,  .by = c(topic_desc, topic)) |>
																								mutate(name = paste0(name, " (", formatpercentage(expVar), ")")) ))
# ESS by year and country
ess_yc <- ess |> nest(.by = c(year, cntry)) |> 
	mutate(pca = data |> map(\(x) x |> select(all_of(attitudenames)) |> prcomp(center = T, scale. = T)),
								pca_varexp = pca |> map(\(x) x$sdev^2/5), 
								pca_pcs = map2(pca, pca_varexp, 
																							\(x,y) x$rotation |> as_tibble() |> 	
																								add_column(topic = factor(attitudenames), .before = 1) |> 
																								add_column(topic_desc = factor(attitudedesc), .before = 2) |> 
																								pivot_longer(starts_with("PC")) |> 
																								arrange(name) |> 
																								mutate(value = normalize_pc(value), .by = name) |> 
																								mutate(expVar = y,  .by = c(topic_desc, topic)) |>
																								mutate(name = paste0(name, " (", formatpercentage(expVar), " explained Variance)")) ))
save(ess_all, ess_y, ess_yc, file = "data_polali/PCAs")


# Figure

PC_plot <- function(data, title) data |> 
 ggplot(aes(y = topic_desc |> fct_rev(), x = value, fill = value)) + 
 #geom_vline(xintercept = c(-1, 1), color = "darkgray") +
 geom_col() +
 #scale_fill_gradient2(low = "red", mid = "white", high="green", midpoint = 0, limits = c(-2,2), oob = scales::squish) +
  scale_fill_gradientn(colors=c(rep("red",100),"white",rep("green",100)), limits = c(-2,2), oob = scales::squish) +
  facet_wrap(~name, ncol = 5) + 
 labs(x = "",y="") +#, title = title) +
 guides(fill = "none") +
  geom_vline(xintercept=0, color='black', lwd=0.3) +
  theme_linedraw()

## ESS all 2002, 2023
ess_y <- ess |> 
 filter(cntry %in% c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")) |> 
 nest(.by = year) |> 
 mutate(pca = data |> map(\(x) x |> select(all_of(attitudenames)) |> prcomp(center = T, scale. = T)),
        pca_varexp = pca |> map(\(x) x$sdev^2/5), 
        pca_pcs = map2(pca, pca_varexp, 
                       \(x,y) x$rotation |> as_tibble() |> 	
                        add_column(topic = factor(attitudenames), .before = 1) |> 
                        add_column(topic_desc = factor(attitudedesc), .before = 2) |> 
                        pivot_longer(starts_with("PC")) |> 
                        arrange(name) |> 
                        mutate(value = normalize_pc(value), .by = name) |> 
                        mutate(expVar = y,  .by = c(topic_desc, topic)) |>
                        mutate(name = paste0(name, " (", formatpercentage(expVar), ")")) ))
ESS04 <- ess_y |> filter(year == 2004) |> pull(pca_pcs) |> first() #|> PC_plot() + labs(tag = "2002")
ESS23 <- ess_y |> filter(year == 2023) |> pull(pca_pcs) |> first() #|> PC_plot() + labs(tag = "2023") 

ESS04$topic_desc <- factor(ESS04$topic_desc, levels = attitudedesc[c(3,5,1,4,2)])
ESS23$topic_desc <- factor(ESS23$topic_desc, levels = attitudedesc[c(3,5,1,4,2)])

pcaplot <- function(data, PC1=TRUE, LABS=FALSE, TITLE=NULL){
  if(PC1){
    data <- data[data$name==data$name[1],]
  } else {
    data <- data[data$name!=data$name[1],]
  }
  p <- ggplot(data, aes(x=value,y=topic_desc |> fct_rev(),fill=value)) +
    geom_col() + 
    geom_vline(xintercept=0, lwd=0.3) +
    facet_wrap(~name, ncol=2) +
    scale_fill_gradientn(colors=c(rep("red",100),"white",rep("darkgreen",100)), limits = c(-2,2), oob = scales::squish) +
    scale_x_continuous(limits=c(-2.4,2.4), name=NULL) +
    ggtitle(TITLE) +
    theme(legend.position = 'none', axis.ticks=element_blank(),
          strip.background=element_rect(fill='grey90', color='grey90'), strip.text=element_text(color='black'),
          #plot.margin = unit(c(0, 0, 0, 0), "null")
          )
  if(LABS) {
    p <- p + scale_y_discrete(name="")
  } else {
    p <- p + scale_y_discrete(labels=NULL, name="")
  }
  return(p)
}


# PANEL A
(p1 <- pcaplot(ESS04, PC1=TRUE, LABS=TRUE, TITLE="2004"))
(p2 <- pcaplot(ESS04, PC1=FALSE) + theme(axis.text.x=element_text(size=5)))
(p3 <- pcaplot(ESS23, PC1=TRUE, TITLE="2023"))
(p4 <- pcaplot(ESS23, PC1=FALSE) + theme(axis.text.x=element_text(size=5)))
(p1 + p3) / (p2 + p4)


## Plot Time
ess_y_expVarP1 <- ess_y |> 
 mutate(`PC1 explained Variance` = pca_varexp |> map_dbl(\(x) x[1]),
        cntry = "All") |> 
 select(Year = year, cntry, `PC1 explained Variance` )
ess_y_PC = ess_y |> select(year, pca_pcs) |> unnest(cols = c(pca_pcs))
df <- ess_yc |> select(cntry, year, pca_pcs) |> unnest(cols = c(pca_pcs)) |> 
 filter(word(name, 1) == "PC1") |> 
 mutate(PC = word(name, 1)) |> 
 select(cntry, year, PC, expVar) |> distinct() |> 
 pivot_wider(names_from = year, values_from = expVar) |> 
 na.omit() |>
 pivot_longer(`2004`:`2023`, names_to = "Year", values_to = "PC1 explained Variance") |> 
 mutate(Year = as.numeric(Year)) |>
  rename(year = Year, pc1ev = `PC1 explained Variance`)

set.seed(6919)
cntry_year <- data.frame(
  cntry=c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI"),
  year=c(2023,2020,2004,2006,2018,2014,2023,2004,2008,2018,2008,2012,2020,2012,2016)
)

cntry_year_avg_pc1ev <- aggregate(pc1ev ~ cntry + year, data = df, FUN = mean)
# select only the observations that are in cntry_year
cntry_year_avg_pc1ev <- cntry_year_avg_pc1ev[paste(cntry_year_avg_pc1ev$cntry,cntry_year_avg_pc1ev$year) %in% paste(cntry_year$cntry,cntry_year$year),]

# PANEL B
(p5 <- ggplot(df, aes(year, pc1ev, color = cntry)) +
    geom_line(alpha=1, lwd=0.3) +
    #geom_point(color='white', shape=16, size=2) +
    #geom_point(alpha=.5, shape=16, size=1.2) +
    #stat_smooth(data=ess_y_expVarP1, method='lm', se=F, color='blue') +
    geom_line(data=ess_y_expVarP1, aes(Year, `PC1 explained Variance`, color=cntry), linewidth=1) +
    geom_label(data=cntry_year_avg_pc1ev, aes(x=year,y=pc1ev,label=cntry,color=cntry),size=2, position=position_dodge2(width=0.5)) +
    scale_color_manual(name=NULL, values = c("blue",cols25(15)) |> unname()) +
    scale_x_continuous(name=NULL, breaks=c(2004,2006,2008,2012,2014,2016,2018,2020,2023), minor_breaks = c()) +
    scale_y_continuous(name="PC1 explained variance", labels = scales::percent, limits = c(0.2,0.43)) +
    guides(linewidth = "none", color='none') +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)))

# COMBINED FIGURE
plot_spacer() + 
  wrap_elements((p1 + p3) / (p2 + p4)) + #& theme(plot.tag.position=c(0.12,1))) -
  plot_spacer() +
  p5 + 
  plot_layout(widths=c(-.4,3,-.2,2)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12, face="bold"), plot.tag.position=c(0.08,1))

ggsave("fig_align.png", height = 4.2, width = 6.9)
ggsave("fig_align.tiff", height = 4.2, width = 6.9)
 