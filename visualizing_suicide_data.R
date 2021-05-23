library(ggplot2)
library(plotly)
library(htmlwidgets)
library(egg)
library(gridExtra)
library(grid)


setwd("D:/Users/witen/Desktop/Suli/R/beadandó/b2")
df_link <- "https://github.com/vetszabolcs/suicide_data/raw/main/master.csv"
df <- read.csv(df_link)
colnames(df)[1] <- "country"

# kiegészítő adatok forrása: https://unstats.un.org/unsd/methodology/m49/overview/
developing <- c("Algeria","Egypt","Libya","Morocco","Sudan","Tunisia","Western Sahara","British Indian Ocean Territory","Burundi","Comoros","Djibouti","Eritrea","Ethiopia","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Mozambique","Réunion","Rwanda","Seychelles","Somalia","South Sudan","Uganda","United Republic of Tanzania","Zambia","Zimbabwe","Angola","Cameroon","Central African Republic","Chad","Congo","Democratic Republic of the Congo","Equatorial Guinea","Gabon","Sao Tome and Principe","Botswana","Eswatini","Lesotho","Namibia","South Africa","Benin","Burkina Faso","Cabo Verde","Côte d’Ivoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Saint Helena","Senegal","Sierra Leone","Togo","Anguilla","Antigua and Barbuda","Aruba","Bahamas","Barbados","Bonaire","British Virgin Islands","Cayman Islands","Cuba","Curaçao","Dominica","Dominican Republic","Grenada","Guadeloupe","Haiti","Jamaica","Martinique","Montserrat","Puerto Rico","Saint Barthélemy","Saint Kitts and Nevis","Saint Lucia","Saint Martin (French Part)","Saint Vincent and the Grenadines","Sint Maarten (Dutch part)","Trinidad and Tobago","Turks and Caicos Islands","United States Virgin Islands","Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama","Argentina","Bolivia (Plurinational State of)","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guiana","Guyana","Paraguay","Peru","South Georgia and the South Sandwich Islands","Suriname","Uruguay","Venezuela (Bolivarian Republic of)","Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan","China","China","China","Democratic People's Republic of Korea","Mongolia","Republic of Korea","Brunei Darussalam","Cambodia","Indonesia","Lao People's Democratic Republic","Malaysia","Myanmar","Philippines","Singapore","Thailand","Timor-Leste","Viet Nam","Afghanistan","Bangladesh","Bhutan","India","Iran (Islamic Republic of)","Maldives","Nepal","Pakistan","Sri Lanka","Armenia","Azerbaijan","Bahrain","Georgia","Iraq","Jordan","Kuwait","Lebanon","Oman","Qatar","Saudi Arabia","State of Palestine","Syrian Arab Republic","Turkey","United Arab Emirates","Yemen","Fiji","New Caledonia","Papua New Guinea","Solomon Islands","Vanuatu","Guam","Kiribati","Marshall Islands","Micronesia (Federated States of)","Nauru","Northern Mariana Islands","Palau","United States Minor Outlying Islands","American Samoa","Cook Islands","French Polynesia","Niue","Pitcairn","Samoa","Tokelau","Tonga","Tuvalu","Wallis and Futuna Islands")
developing <- unique(gsub("-|\\(.*", "", developing))

developed <- c("Bermuda","Canada","Greenland","Saint Pierre and Miquelon","United States of America","Japan","Cyprus","Israel","Belarus","Bulgaria","Czech Republic","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia","Ukraine","Åland Islands","Guernsey","Jersey","Sark","Denmark","Estonia","Faroe Islands","Finland","Iceland","Ireland","Isle of Man","Latvia","Lithuania","Norway","Svalbard and Jan Mayen Islands","Sweden","United Kingdom of Great Britain and Northern Ireland","Albania","Andorra","Bosnia and Herzegovina","Croatia","Gibraltar","Greece","Holy See","Italy","Malta","Montenegro","North Macedonia","Portugal","San Marino","Serbia","Slovenia","Spain","Austria","Belgium","France","Germany","Liechtenstein","Luxembourg","Monaco","Netherlands","Switzerland","Australia","Christmas Island","Cocos (Keeling) Islands","Heard Island and McDonald Islands","New Zealand","Norfolk Island")
developed <- unique(gsub("-|\\(.*", "", developed))
developed[grep("United", developed, ignore.case = T)]
unique(df$country)[grep("United", unique(df$country), ignore.case = T)]
developed[grep("United", developed, ignore.case = T)][1] <- "United States"
developed[grep("United", developed, ignore.case = T)][2] <- "United Kingdom"

post_soc <- c("Czech Republic", "Estonia", "Lithuania", "Poland",
  "Slovakia", "Hungary", "Croatia", "Latvia", "Romania", "Russian Federation",
  "Bulgaria", "Serbia", "Ukraine")
west <- c("Austria","Belgium","France","Germany","Liechtenstein","Luxembourg","Monaco",
  "Netherlands","Switzerland")
east <-  c("Belarus","Bulgaria","Czech Republic","Hungary","Poland","Republic of Moldova","Romania","Russian Federation","Slovakia",
          "Ukraine")
north <- c("Denmark","Estonia","Faroe Islands","Finland","Iceland","Ireland","Isle of Man","Latvia","Lithuania","Norway","Svalbard and Jan Mayen Islands","Sweden","United Kingdom")
south <- c("Albania","Andorra","Bosnia and Herzegovina","Croatia","Gibraltar","Greece","Holy See","Italy","Malta","Montenegro","North Macedonia","Portugal","San Marino","Serbia","Slovenia","Spain")


unique(df$country)
df$developed <- NA
df$developed[df$country %in% developed] <- 1
df$developed[df$country %in% developing] <- 0
unique(df$country[is.na(df$developed)])

developed[grep("Czech", developed)] <- "Czech Republic"
developing[grep("Vinc", developing)] <- "Saint Vincent and Grenadines"
developing[grep("Mac", developing)]
developed[grep("Mac", developed)]
df <- df[!df$country=="Macau",]

df$developed[df$country %in% developed] <- "Developed"
df$developed[df$country %in% developing] <- "Developing"

eur <- df[df$country %in% west | df$country %in% east | df$country %in% south |
            df$country %in% north | df$country %in% post_soc,]

eur$post_soc <- "Non-postsocialist"
eur$post_soc[eur$country %in% post_soc] <- "Postsocialist"
unique(eur$country[!eur$post_soc == 1])
eur$post_soc <- as.character(eur$post_soc)

eur$region <- NA
eur$region[eur$country %in% east] <- "east"
eur$region[eur$country %in% west] <- "west"
eur$region[eur$country %in% north] <- "north"
eur$region[eur$country %in% south] <- "south"
unique(eur$country[is.na(eur$region)])

#-------------------------------------------------------------------------------
aggreGate <- function(data_f, by="country", by2="year", new_col){
  new_df <- data_f
  for (x in unique(new_df[,by])){
    for (y in unique(new_df[,by2])){
      rows <- which(new_df[,by]== x & new_df[,by2]== y)
      new_df[rows, new_col] <- sum(new_df[rows,"suicides_no"]) / sum(new_df[rows,"population"]) * 100000
    }}
  return(new_df)}
#-------------------------------------------------------------------------------

df$yearly <- NA
for (y in unique(df$year)){
  rows <- which(df$year == y)
  df[rows, "yearly"] <- sum(df[rows,"suicides_no"]) / sum(df[rows,"population"]) * 100000
}

unique(df$year)

png("yearly.png", 1280,720)
gg0 <- ggplot(df, aes(year, yearly, colour = yearly*-1))+
  geom_line(size=1.5)+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(1985, 2016, 6))+
  labs(title = "Yearly suicide", x = "Year", y = "Suicide / 100k people")+
  theme(axis.title.x = element_text(hjust = 0.515,
        margin=margin(5,0,0,0)))+
  theme(axis.title = element_text(face = "bold", size = 16))+
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))+
  theme(axis.text = element_text(size=14))
dev.off()

#-------------------------------------------------------------------------------
my_theme <- theme(axis.title.x = element_text(hjust = 0.52,
                                  margin=margin(5,0,0,0)))+
  theme(axis.title = element_text(face = "bold", size = 16))+
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))+
  theme(axis.text = element_text(size=14))
#-------------------------------------------------------------------------------


df <- aggreGate(df, "year", "developed", "dev_yearly")

png("dev_line.png", 1280,720)
gg1 <- ggplot(df, aes(year, dev_yearly, group = developed, colour = developed))+
  geom_line(size=1.5)+
  labs(title = "Yearly suicide in developed vs. developing countires", 
       x = "Year", y = "Suicide / 100k people")+
  scale_x_continuous(breaks = seq(1985, 2016, 6))+
  theme(legend.title = element_blank())+
  my_theme+
  theme(legend.text = element_text(size = 14))
dev.off()  

png("dev_boxplot.png", 1280,720)
gg2 <- ggplot(df, aes(developed, dev_yearly))+
  geom_point(alpha = 0.05, position=position_jitter(height=.05, width=.38),
             colour = "red")+
  geom_boxplot(aes(colour = dev_yearly), alpha = 0.5, outlier.colour = NA)+
  labs(title = "Yearly suicide in developed vs. developing countires", 
       x = "Country type", y = "Suicide / 100k people")+
  my_theme
dev.off()
  

df <- aggreGate(df, "country", "year", "country_yearly")
df <- df[order(df$country_yearly),]
tail(unique(df$country), 25)

png("top10.png", 1280,720)
top10 <- ggplot(df[df$country %in% tail(unique(df$country), 10),],
       aes(x=year, y=country_yearly, 
           colour=country, group=country,
           text = paste("Country:", country, "\n",
                        "Year: ", year, "\n",
                        "Suicide/100k: ", round(country_yearly), sep="")))+
  geom_line(size=1.1)+
  labs(title = "Yearly suicide in top 10 countries", 
       x = "Year", y = "Suicide / 100k people",
       color = "Country")+
  theme(legend.title = element_text(face = "bold", size = 16))+
  theme(legend.text = element_text(size = 14))+
  theme(axis.title.x = element_text(hjust = 0.515))+
  scale_x_continuous(breaks = seq(1985, 2016, 6))+
  my_theme+
  geom_point(aes(color=country), cex = 1.5)
dev.off()


top10_plotly <- ggplotly(top10+theme(legend.title = element_blank()), tooltip = "text") %>% 
layout(legend=list(title=list(text='<b>  Country\n</b>',
                              font=list(family = "Arial", size=20)))) %>% as_widget()
saveWidget(top10_plotly, "top10_plotly.html")

api_create(top10_plotly, "yearly_suicide_top_10_countries")


#----------------------------------------------------------------------------------

eur <- aggreGate(eur, "country", "year", "country_yearly")
eur <- eur[order(eur$country_yearly),]
tail(unique(eur$country), 25)

all(df$country_yearly[df$country == "Hungary" & df$year == 2015] == 
eur$country_yearly[eur$country == "Hungary" & eur$year == 2015])

png("top21_eur.png", 1280,720)
top10_eur <- ggplot(eur[eur$country %in% tail(unique(eur$country), 10),],
                aes(x=year, y=country_yearly, 
                    colour=country, group=country,
                    text = paste("Country:", country, "\n",
                                 "Year: ", year, "\n",
                                 "Suicide/100k: ", round(country_yearly), sep="")))+
  geom_line(size=1.1)+
  labs(title = "Yearly suicide in top 10 european countries", 
       x = "Year", y = "Suicide / 100k people",
       color = "Country")+
  theme(legend.title = element_text(face = "bold", size = 16))+
  theme(legend.text = element_text(size = 14))+
  theme(axis.title.x = element_text(hjust = 0.515))+
  scale_x_continuous(breaks = seq(1985, 2016, 6))+
  my_theme+
  geom_point(aes(color=country), cex = 1.5)
dev.off()

top10_eur_plotly <- ggplotly(top10_eur+theme(legend.title = element_blank()), tooltip = "text") %>% 
  layout(legend=list(title=list(text='<b>  Country\n</b>',
                                font=list(family = "Arial", size=20)))) %>% as_widget()
saveWidget(top10_eur_plotly, "top10_eur_plotly.html")
api_create(top10_eur_plotly, "yearly_suicide_top_10_eur_countries")


eur <- aggreGate(eur, "country", "year", "per_country_year")
gg_soc <- png("gdp_socialism.png", 1280,720)
options(scipen=10000)
gg4 <- ggplot(eur,aes(gdp_per_capita...., per_country_year, colour = post_soc))+
  geom_point(size=3)+
  labs(title = "Suicide and GDP in post -and non-postsocialist\neuropean countries", 
       x = "Yearly GDP per capital", y = "Yearly Suicide / 100k people")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 14))+
  theme(axis.title.x = element_text(hjust = 0.515))+
  my_theme+
  theme(axis.title.x = element_text(margin=margin(10,0,0,0)))+
  theme(axis.title.y = element_text(margin=margin(0,10,0,0)))
dev.off()



eur <- aggreGate(eur, "region","age","per_region_age")

png("regions_and_age.png", 1280,720)
gg5 <- ggplot(eur,aes(region, per_region_age, fill = age))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Regions and age")+
  xlab("Region")+ylab("Suicide / 100k people")+
  labs(fill="Age")+
  theme(text = element_text(size = 14))+
  my_theme+
  scale_fill_hue(l = 50, c = 50)
dev.off()

eur <- aggreGate(eur, "region", "age", "per_region_sex")
eur$age <- factor(eur$age, levels = c("5-14 years",  "15-24 years", "25-34 years",
                                      "35-54 years","55-74 years", "75+ years"))

png("facet.png", 1280,1280)
grid.arrange(gg0+theme(axis.title.x = element_blank()),
                       gg5+theme(legend.position = "bottom"),
                       top10+theme(axis.title.x = element_blank())+
                         theme(legend.position = "bottom"),
                       gg4+theme(legend.position = "bottom"),
                       gg1+theme(axis.title.x = element_blank())+
                         theme(legend.position = "bottom"),
                       gg2+theme(legend.position = "bottom"), nrow = 3, ncol=2,
          top = textGrob("Suicide rates from 1987 to 2016\n",
                         gp=gpar(fontsize=20, fontface="bold")))
dev.off()


# A 90-es évek óta gyorsan és viszonylag egyenletesen csökken az öngyilkosságok globális száma, igaz,
# néhány évben, így 2016-ban is számos országban visszaesés volt azonosítható.
# Az öngyilkosságok számát tekintve Magyarország továbbra is az élmezőnyben szerepel, 2016-ban a 6. helyen állt.
# Hatalmas szakadék tátong a fejlődő és a fejlett országok öngyilkossági rátái között, illetve a kelet-nyugat,
# posztszocialista - nem poszt szocialista régiók esetén is egyértelmű a differenciálódás.
