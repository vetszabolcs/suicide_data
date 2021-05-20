library(plotly)
library(htmlwidgets)
library(tidyverse)
library(egg)
library(ggpubr)
library(RColorBrewer)
library(grid)
library(gridExtra)


df_link <- "https://github.com/vetszabolcs/suicide_data/raw/main/master.csv"
orig <- read.csv(df_link, encoding = "UTF-8")
colnames(orig)[1] <- "country"
orig$generation <-  factor(orig$generation, levels = c("G.I. Generation", "Silent", "Boomers",
                                                       "Generation X", "Millenials", "Generation Z"))


# personalised aggregate function
aggreGate <- function(data_f, by="country", by2="year"){
  new_df <- data_f
  for (x in unique(new_df[,by])){
    for (y in unique(new_df[,by2])){
      rows <- which(new_df[,by]== x & new_df[,by2]== y)
      new_df[rows, "average"] <- sum(new_df[rows,"suicides_no"]) / sum(new_df[rows,"population"]) * 100000
    }}
  return(new_df)}


# handling possible font error
if (!"Arial" %in% extrafont::fonts()){
  extrafont::font_import(pattern="ari")}
windowsFonts("Arial" = windowsFont("Arial"))

# function that creates a ggplot and returns a plotly object while printing both
# rewrites gg_plot object if existing (!!!)
annual_plot_lines <- function(Data, Lines=country, line_label="Country", gg_name="gg_plot", title){
  #ggplot - annual data  aggregated by Lines
  annual_gg <- Data %>% ggplot(aes(x=year, y=average, 
                                   colour={{Lines}}, group={{Lines}},
                                   text = paste(paste(line_label, ": ", sep=""), {{Lines}}, "\n",
                                                "Year: ", year, "\n",
                                                "Suicide/100k: ", average, sep=""))) +
    xlim(min(Data$year)-1, max(Data$year)) #ajdusting xaxis title to labels 
  #adding lines and legend
  gg_plot <<-  (annual_gg + geom_line() + labs(colour=line_label)+
                  xlab("Year") + ylab("Suicides per 100k people")+
                  theme(text = element_text(family = "Arial", size = 14),
                        axis.title = element_text(face="bold", size=16),
                        legend.title = element_text(face="bold", size=16)))+
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5, face="bold", size=20))
  print(gg_plot)
  #removing legend
  no_legend <- gg_plot + theme(legend.title = element_blank())
  
  widget <- (#making it interactive
    ggplotly(no_legend, tooltip = "text") %>% 
      #adding plotly based legend title
      layout(legend=list(title=list(text= paste('<b>', line_label, '\n</b>', sep=""),
                                    font=list(family = "Arial", size=17))))
  )
  print(widget)
  return(widget)
}

#aggregate by country and year
df <- aggreGate(orig)
Plotly <- annual_plot_lines(df, title = "General population")
ggplot1 <- gg_plot
#save widget to working directory
#saveWidget(as_widget(annual_plotly), "Annual_sucidie.html")

#-----------------------------------------------------------------------------------
# male population only

#data
df2 <- orig[orig$sex=="male",]
#aggreGate
df2 <- aggreGate(df2)


# creating plots
Plotly2 <- annual_plot_lines(df2, title = "Male population")
ggplot2 <- gg_plot
#-------------------------------------------------------------------------------
# females only

df3 <- orig[orig$sex=="female",]
df3 <- aggreGate(df3)
Plotly3 <- annual_plot_lines(df3, title="Female population")
ggplot3 <- gg_plot
ggplot3+theme(legend.position = "bottom")
#-------------------------------------------------------------------------------
# aggregate by generation
df4 <- aggreGate(orig, "generation", "year")

Plotly4 <- annual_plot_lines(df4, Lines = generation, line_label = "Generation",
                             title = "Across Generations")
ggplot4 <- gg_plot
#-------------------------------------------------------------------------------
# "top" suicide countries on barplots
top_suicide_countries <- attributes(head(sort(tapply(df$average, list(df$country), mean),
                                              decreasing = T),6))$dimnames[[1]]

df5 <- orig[df$country %in% top_suicide_countries,]
df5 <- aggreGate(df5, "country")


ggplot5 <- ggplot(df5, aes(country, average, group = as.factor(year), fill=as.factor(year)))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Across top countries")+
  scale_y_continuous(limits=c(10,52),oob = scales::rescale_none)+
  xlab("Country")+ylab("Suicides per 100k people")+
  labs(fill="Year")+
  theme(text = element_text(family = "Arial", size = 14))+
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size=20))+
  theme(axis.title = element_text(face="bold", size=16))+
  theme(legend.title = element_text(face="bold", size=16))+
  scale_fill_hue(l = 50, c = 50)

# by coutries and generations
df6 <- orig[orig$country %in% top_suicide_countries,]
df6 <- aggreGate(df6, "country", "generation")

ggplot6 <- ggplot(df6, aes(country, average, fill = generation))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Across top countries and generations")+
  xlab("Country")+ylab("Suicides per 100k people")+
  labs(fill="Generation")+
  theme(text = element_text(family = "Arial", size = 14))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size=20))+
  theme(axis.title = element_text(face="bold", size=16))+
  theme(legend.title = element_text(size = 16,face="bold"))+
  scale_fill_brewer(palette="OrRd", direction = -1)

#------------------------------------------------------------------------------
# Creating subplots
plot1_2 <- ggplot1 + theme(legend.position = "none")+
  theme(axis.title.x = element_blank())

plot2_2 <- ggplot2 + theme(legend.position = "none")+
  theme(axis.title.x = element_text(size=16))

plot3_2 <- ggplot3 + theme(legend.position = "none")+
  theme(axis.title.x = element_blank())

plot4_2 <- ggplot4

plot5_2 <- ggplot5 + theme(legend.position = "none")+
  theme(axis.title.x = element_blank())

plot6_2 <- ggplot6 + theme(axis.title.x = element_blank())

country_colors <- as_ggplot(get_legend(ggplot1))
year_colors <- as_ggplot(get_legend(ggplot5))

main <- as_ggplot(grid.arrange(plot1_2,plot5_2,plot3_2,
                     plot6_2,plot2_2,plot4_2,
                     top = textGrob("Suicide rates from 1987 to 2016",
                                    gp=gpar(fontsize=20,fontfamily="Arial",
                                            fontface="bold"))))

#-------------------------------------------------------------------------------
# Saving plots in a subfolder 
# If Plot folder does not exist in the working directory
# it will be created there

# fun to save plotlys
save_plotLYs <- function(vector_of_names){
  dir.create(paste(getwd(), "Plots", sep="/"), showWarnings = F)
  dir.create(paste(getwd(), "Plots/Plotly", sep="/"), showWarnings = F)
  for (p in vector_of_names){
    setwd("./Plots/Plotly")
    saveWidget(as_widget(get(p)),
               paste(gsub(".*> | </.*","", get(p)$x$layout$title$text), ".html", sep=""))
    setwd("../..")
  }
}

# fun to save ggplots in png
save_GGplots <- function(vector_of_names, w, h, output = F){
  dir.create(paste(getwd(), "Plots", sep="/"), showWarnings = F)
  dir.create(paste(getwd(), "Plots/GGplot", sep="/"), showWarnings = F)
  for (p in vector_of_names){
    setwd("./Plots/GGplot")
    if (output != F){
      png(output, w, h)
      print({get(p)})
      dev.off()
      setwd("../..")}
    else{
      png(paste(get(p)$labels$title,".png", sep=""), w, h)
      print({get(p)})
      dev.off()
      setwd("../..")}
    }
}

save_plotLYs(vector_of_names =  ls()[grep("Plotly", ls())])

save_GGplots(vector_of_names =  ls()[grep("ggplot", ls())],
             w = 2048,
             h = 768)

save_GGplots("main",output = "Facet.png", 2048,768)
save_GGplots("country_colors", w=2048,768, output = "Country_colors.png")
save_GGplots("year_colors", w=2048,768, output = "Year_colors.png")

# As time passes it seems like the rate of suicide slightly decreases
# although in some countries it has increased in the past few years. 
# Suicide rate in Lithuania is still extremely high comared
# to the rest of european countries and Hungary is still in the top
# 6 coutries that has the highest suicide rate. 
# Committing suicide is more frequent in the older generations and 
# also among the male population - in some countries the latter shows
# more than 5 fold increase. 
