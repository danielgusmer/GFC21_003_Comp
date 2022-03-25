install.packages("tidyverse")
library(tidyverse)
install.packages("rmarkdown")
library(rmarkdown)
install.packages("ggrepel")
library(ggrepel)

# Loading in the csv files containing the data
GFC21_003_Comp <- read_csv("GFC21_003_Comp_Anon.csv")

# Filtering the data to contain either our products or competitors
comp <- GFC21_003_Comp %>% 
  filter(product %in% c("A","B","C","Hybrid","ME Hard Selzter HG Low Dose"))
MEHSHG <- GFC21_003_Comp %>% 
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
  scale_linetype_manual(values=lineVariation)

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
  scale_linetype_manual(values=lineVariation)

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
  scale_linetype_manual(values=lineVariation)

ggsave("16p_comp_pH.png",width=8.5,height=5)