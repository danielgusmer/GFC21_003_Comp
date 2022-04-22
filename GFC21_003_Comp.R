install.packages("tidyverse")
library(tidyverse)
install.packages("rmarkdown")
library(rmarkdown)
install.packages("ggrepel")
library(ggrepel)

# Loading in the csv files containing the data
GFC21_003_Comp <- read_csv("GFC21_003_Comp_Anon.csv")
GFC21_003_Comp_24 <- read_csv("GFC21_003_Comp_R2_Anon.csv")

# Filtering the data to contain either our products or competitors
comp <- GFC21_003_Comp %>% 
  filter(product %in% c("A","B","C","Hybrid","ME Hard Selzter HG Low Dose"))
MEHSHG <- GFC21_003_Comp %>% 
  filter(product %in% c("ME Hard Seltzer HG"))
comp24 <- GFC21_003_Comp_24 %>% 
  filter(product %in% c("A","B","C","Hybrid"))
MEHSHG24 <- GFC21_003_Comp_24 %>% 
  filter(product %in% c("ME Hard Seltzer HG"))

# Colorblind-friendly palette with black
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
lineVariation <- c("dashed","twodash","longdash","dotdash","solid","dotted")

# Plot looking at alcohol production between our product and competitors at 16 Plato over time
ggplot(data=GFC21_003_Comp,aes(x=day,y=alcohol*100,color=product,linetype=product))+
  geom_smooth(data=MEHSHG)+
  geom_line(data=comp,size=.75)+
  labs(title="16°Plato Fermentation Curve",
       subtitle="Alcohol",
       x="Days",
       y="Alcohol (%)",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,15,2))

ggsave("16p_comp_alcohol.png",width=8.5,height=5)

# Plot looking at plato reduction between our product and competitors at 16 Plato over time
ggplot(data=GFC21_003_Comp,aes(x=day,y=plato,color=product,linetype=product))+
  geom_smooth(data=MEHSHG)+
  geom_line(data=comp,size=.75)+
  labs(title="16°Plato Fermentation Curve",
       subtitle="Plato",
       x="Days",
       y="°Plato",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,15,2))+
  scale_y_continuous(breaks=seq(-5,16,2.5))

ggsave("16p_comp_plato.png",width=8.5,height=5)

# Plot looking at pH change between our product and competitors at 16 Plato over time
ggplot(data=GFC21_003_Comp,aes(x=day,y=pH,color=product,linetype=product))+
  geom_smooth(data=MEHSHG)+
  geom_line(data=comp,size=.75)+
  labs(title="16°Plato Fermentation Curve",
       subtitle="pH",
       x="Days",
       y="pH",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,15,2))

ggsave("16p_comp_pH.png",width=8.5,height=5)

# Plot looking at viability between our product and competitors at 16 Plato over time
# NOTE: data=comp[!is.na(comp$viability),] filters out all the NA cells in the viability
# column, effectively getting rid of the breaks in lines
ggplot(data=GFC21_003_Comp,aes(x=day,y=viability*100,color=product,linetype=product))+
  geom_smooth(data=MEHSHG)+
  geom_line(data=comp[!is.na(comp$viability),],size=.75)+
  labs(title="16°Plato Fermentation Curve",
       subtitle="Viability",
       x="Days",
       y="Viability (%)",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)

ggsave("16p_comp_viability.png",width=8.5,height=5)

# Plot looking at alcohol production between our product and competitors at 24 Plato over time
ggplot(data=GFC21_003_Comp_24,aes(x=day,y=alcohol*100,color=product,linetype=product))+
  geom_smooth(data=MEHSHG24)+
  geom_line(data=comp24,size=.75)+
  labs(title="24°Plato Fermentation Curve",
       subtitle="Alcohol",
       x="Days",
       y="Alcohol (%)",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,23,2))

ggsave("24p_comp_alcohol.png",width=8.5,height=5)

# Plot looking at plato reduction between our product and competitors at 24 Plato over time
ggplot(data=GFC21_003_Comp_24,aes(x=day,y=plato,color=product,linetype=product))+
  geom_smooth(data=MEHSHG24)+
  geom_line(data=comp24,size=.75)+
  labs(title="24°Plato Fermentation Curve",
       subtitle="Plato",
       x="Days",
       y="°Plato",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,23,2))+
  scale_y_continuous(breaks=seq(-5,25,5))

ggsave("24p_comp_plato.png",width=8.5,height=5)

# Plot looking at pH change between our product and competitors at 24 Plato over time
ggplot(data=GFC21_003_Comp_24,aes(x=day,y=pH,color=product,linetype=product))+
  geom_smooth(data=MEHSHG24)+
  geom_line(data=comp24,size=.75)+
  labs(title="24°Plato Fermentation Curve",
       subtitle="pH",
       x="Days",
       y="pH",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,23,2))

ggsave("24p_comp_pH.png",width=8.5,height=5)

# Plot looking at viability between our product and competitors at 24 Plato over time
# NOTE: data=comp[!is.na(comp$viability),] filters out all the NA cells in the viability
# column, effectively getting rid of the breaks in lines
ggplot(data=GFC21_003_Comp_24,aes(x=day,y=viability*100,color=product,linetype=product))+
  geom_smooth(data=MEHSHG24)+
  geom_line(data=comp24[!is.na(comp24$viability),],size=.75)+
  labs(title="24°Plato Fermentation Curve",
       subtitle="Viability",
       x="Days",
       y="Viability (%)",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)

ggsave("24p_comp_viability.png",width=8.5,height=5)