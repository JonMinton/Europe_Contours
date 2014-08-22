
# Code for producing shaded contour plots for Europe

# Jon Minton
# 17 August 2014

# 1)  clear the workspace

rm(list=ls())

# 2) load a function that makes installing and loading R packages a bit 
# easier

source("scripts/LoadPackages.r")

# 3) use this function

RequiredPackages(
  c(
    "plyr",
    "reshape2",
    "lattice",
    "ggplot2",
    "stringr",
    "car",
    "RColorBrewer"
  )
)

######################################################################################
# SOURCE DATA

# 4) load human mortality database (HMD) data on population counts and death counts
# in the 'tidy data' format suggested by Hadley Wickham 
counts <- read.csv("data/counts.csv")
# (For the code used to convert the existing files to the 'tidy' please contact me)

# 5) load a file which shows which of the HMD countries are part of Europe

country_codes <- read.csv("Data/country_codes__new.csv", stringsAsFactors=F)

europe_codes <- country_codes$short[country_codes$europe==1]
######################################################################################
# DERIVED DATA

# 6) find the subset of counts data which is of European countries


counts_eu <- subset(
  counts,
  subset=country %in% europe_codes                  
)

# 7) aggregate up count data from all available European nations
counts_eu_all <- ddply(
  counts_eu,
  .(sex, year, age),
  summarise,
  n_countries=length(death_count),
  death_count=sum(death_count),
  population_count=sum(population_count)
)


# 8) want to produce a simple summary of this
counts_summaries <- ddply(
  tmp <- subset(counts_eu_all, subset=sex=="total"),
  .(year),
  summarise,
  n_countries=median(n_countries),
  population_count=sum(population_count)
)

# When was the earliest country's data available?
country_by_earliest_year <- ddply(counts_eu, .(country), summarise, earliest=min(year))
country_by_earliest_year <- arrange(country_by_earliest_year, earliest)

# 9) rates for all of Europe

rates_eu_all <- mutate(counts_eu_all, death_rate=death_count/population_count)

##################################################################################################
# FIGURES

# Comments from Danny

# 
# However, more important than any of this the axis of the contour maps need to 
# be labelled on the actual diagrams and 

# 1) If the Diagram can me labelled “Probability of mortality 
# in an given year by sex and single year of age, enumerated Europe, 1751-2010” 
# that would make it much clearer. By stating ‘enumerated’ you make it clear it 
# is not all of Europe. You could add a note below the diagram saying the 
# definition of Europe expands geographically over the period. [DONE]

# 2) Y scale needs to be labelled “single year of age” with text rotated by 90 degrees, 
# [DONE]


# 3) X scale needs to be labelled “single year in which population and mortality was 
# enumerated” or something like that. 

# 4) And the scale needs to be very clearly labbled, maybe in text rotated by 90 degrees 
# say “probability of death within the year (1=100%=certain death).” 

# 5) the text which says female and male needs to be larger. 


# Figure 1: Contour plot
#lattice.options(default.theme = standard.theme(color = FALSE))

# MONOCHROME VERSION OF GRAPH, FOR PRINT
trellis.device(
  tiff(
    "figures/fig_01__contour_all_europe.tiff",  
    height=1000, width=2000
  ),
  color = FALSE
)
g1 <- contourplot(
  death_rate ~ year * age | sex, 
  data=subset(rates_eu_all, subset=sex!="total" & age <=80), 
  region=T, 
#  col.regions=rev(heat.colors(200)), 
  col.regions=rev(gray(0:199/199)),
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="single age of death",
  xlab="single year in which population and death counts were enumerated",
  cex=1.4,

  main=NULL)
print(g1)

dev.off()


# COLOUR VERSION OF GRAPH, FOR ONLINE VERSION OF PAPER
trellis.device(
  tiff(
    "figures/fig_01COLOUR__contour_all_europe.tiff",  
    height=1000, width=2000
  ),
  color = TRUE
)
g1 <- contourplot(
  death_rate ~ year * age | sex, 
  data=subset(rates_eu_all, subset=sex!="total" & age <=80), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
#  col.regions=rev(gray(0:199/199)),
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="single age of death",
  xlab="single year in which population and death counts were enumerated",
  cex=1.4,
  
  main=NULL)
print(g1)

dev.off()





trellis.device(
  device=tiff(
    "figures/fig_01b__contour_all_europe_log.tiff",  
    height=1000, width=2000
    ),
  color = FALSE)
g1 <- contourplot(
  log(death_rate) ~ year * age | sex, 
  data=subset(rates_eu_all, subset=sex!="total" & age <=80), 
  region=T, 
  #  col.regions=rev(heat.colors(200)), 
  col.regions=rev(gray(0:199/199)),
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="single age of death",
  xlab="single year in which population and death counts were enumerated",
  cex=1.4,
  
  main=NULL)
print(g1)

dev.off()











# 
# 
# 
# 
# # Figure 2: Changes in population and number of countries in Europe in HMD over time
# 
# # Figure 2a) 
# # Number of countries for which data are available
# #tiff("figures/n_countries.tiff", width=3, height=3, units="cm", res=300)
# g1 <- ggplot(data=counts_summaries) + aes(x=year, y=n_countries) + geom_bar(stat="identity", width=1)
# g2 <- g1 + labs(y="Number of European countries in HMD", x="Year")
# print(g2)
# #dev.off()
# 
# 
# 
# # Figure 2b)
# # Total population size in Billions
# g1 <- ggplot(data=counts_summaries) +aes(x=year, y=population_count/1000000000) + geom_area()
# g2 <- g1 + labs(y="Population (Billion)", x="Year")
# print(g2)
# 
# 



