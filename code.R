library(plotly)
library(htmlwidgets)
library(tidyverse)
library(egg)
library(ggpubr)
library(RColorBrewer)
library(grid)
library(gridExtra)


df_link <- "https://github.com/vetszabolcs/suicide_data/raw/main/master.csv"
df <- read.csv(df_link, encoding = "UTF-8")
colnames(df)[1] <- "country"

#aggregate by country
for (c in unique(df$country)){
  for (y in unique(df$year)){
    df$average[df$country == c & df$year == y] <- 
      tapply(df$suicides_no[df$country==c & df$year == y],
             list(df$country[df$country==c & df$year == y]),
             function(x){
               sum(x) / sum(df$population[df$country == c & df$year == y]) * 100000})
  }
}

#handling possible font error
if (!"Arial" %in% extrafont::fonts()){
  extrafont::font_import(pattern="ari")}
windowsFonts("Arial" = windowsFont("Arial"))

#function that creates a ggplot and returns a plotly object while printing both
#rewrites gg_plot object if existing (!!!)
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
                  theme(text = element_text(family = "Arial"),
                        axis.title = element_text(family = "Arial", face="bold", size=12),
                        legend.title = element_text(family = "Arial", face="bold", size=12)))+
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5, face="bold"))
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


Plotly <- annual_plot_lines(df, title = "General population")
plot1 <- gg_plot
#save widget to working directory
#saveWidget(as_widget(annual_plotly), "Annual_sucidie.html")

#-----------------------------------------------------------------------------------

#slicing to males only

#personalised aggregate function
aggreGate <- function(data_f, by="country"){
  new_df <- data_f
  colnames(new_df)[1] <- "country"
  for (c in unique(new_df[,by])){
    for (y in unique(new_df$year)){
      new_df$average[new_df[,by] == c & new_df$year == y] <- 
        tapply(new_df$suicides_no[new_df[,by]==c & new_df$year == y],
               list(new_df[,by][new_df[,by]==c & new_df$year == y]),
               function(x){
                 sum(x) / sum(new_df$population[new_df[,by] == c & new_df$year == y]) * 100000})
    }
  }
  return(new_df)
}

#getting data
df2 <- read.csv(df_link, encoding = "UTF-8")
#slicing data
df2 <- df2[df2$sex=="male",]#
#aggreGate
df2 <- aggreGate(df2)
#creating plots
Plotly2 <- annual_plot_lines(df2, title = "Male population")
plot2 <- gg_plot
#-------------------------------------------------------------------------------
#previous method with females only

df3 <- read.csv(df_link, encoding = "UTF-8")

df3 <- df3[df3$sex=="female",]

df3 <- aggreGate(df3)

Plotly3 <- annual_plot_lines(df3, title="Female population")
plot3 <- gg_plot
#-------------------------------------------------------------------------------
#aggregate by generation

df4 <- read.csv(df_link, encoding = "UTF-8")

df4 <- aggreGate(df4, "generation")

Plotly4 <- annual_plot_lines(df4, Lines = generation, line_label = "Generation",
                             title = "Across Generations")
plot4 <- gg_plot
#-------------------------------------------------------------------------------
#"top" suicide countries on barplots
top_suicide_countries <- attributes(head(sort(tapply(df$average, list(df$country), mean),
                     decreasing = T),6))$dimnames[[1]]
df5 <- df[df$country %in% top_suicide_countries,]


plot5 <- df5 %>% ggplot(aes(country, average, group = as.factor(year), fill=as.factor(year)))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits=c(20,50),oob = scales::rescale_none)+
  ggtitle("Across top countries")+
  xlab("Country")+ylab("Suicides per 100k people")+
  labs(fill="Year")+
  theme(plot.title = element_text(hjust = 0.4, face = "bold"))+
  theme(axis.title = element_text(face="bold", size=12))+
  theme(legend.title = element_text(face="bold"))+
  scale_fill_hue(l = 50, c = 50)

plot6 <- df5 %>% ggplot(aes(country, average, fill = generation))+
  geom_bar(stat = "identity", position = "dodge")+
 scale_y_continuous(limits=c(20,50),oob = scales::rescale_none)+
  ggtitle("Across top countries and generations")+
  xlab("Country")+ylab("Suicides per 100k people")+
  labs(fill="Generation")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size=12))+
  theme(axis.title = element_text(face="bold"))+
  theme(legend.title = element_text(size = 12,face="bold"))+
  scale_fill_brewer(palette="OrRd")


plot1_2 <- plot1 + theme(legend.position = "none")+
  theme(axis.title.x = element_blank())

plot2_2 <- plot2 + theme(legend.position = "none")+
  theme(axis.title.x = element_text(size=12))

plot3_2 <- plot3 + theme(legend.position = "none")+
  theme(axis.title.x = element_blank())

plot4_2 <- plot4

plot5_2 <- plot5 + theme(legend.position = "none")+
  theme(axis.title.x = element_blank())

plot6_2 <- plot6 + theme(axis.title.x = element_blank())

country_colors <- as_ggplot(get_legend(plot1))
year_colors <- as_ggplot(get_legend(plot5))

main <- grid.arrange(plot1_2,plot5_2,plot3_2,
             plot6_2,plot2_2,plot4_2,
             top = textGrob("Suicide rates from 1987 to 2016",
             gp=gpar(fontsize=20,fontfamily="Arial",fontface="bold")))


Plotly
Plotly2
Plotly3
Plotly4

for (p in ls()[grep("Plotly",ls())]){
  saveWidget(as_widget(get(p)),
             paste(p, ".html", sep=""))
}

# As time passes it seems like the rate of suicide slightly decreases
# although in some countries it has increased in the past few years. 
# Suicide rate in Lithuania is still extremely high comared
# to the rest of european countries and Hungary is still in the top
# 6 coutries that has the highest suicide rate. 
# Committing suicide is more frequent in the older generations and 
# also among the male population - in some countries the latter shows
# more than 5 fold increase. 
