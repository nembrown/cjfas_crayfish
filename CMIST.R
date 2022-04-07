
# loading libraries -------------------------------------------------------
#devtools::install_github("https://github.com/remi-daigle/CMISTR")
library(CMISTR)
library(ggplot2)
library(plyr)
library(tidyverse)
library(patchwork)

# For mapping
library(sf)
library(raster)
library(ggmap)

# Loops for each species score --------------------------------------------------
theme_set(theme_bw())

### Marbled crayfish
marble_risk<-read.csv("C:Inputs//marble_risk.csv")
rownames(marble_risk)<-marble_risk$region
marble_risk_simple<-marble_risk[,-c(1:3)]
colnames(marble_risk_simple)<-NULL
rownames(marble_risk_simple)<-NULL

marble_uncertainties<-read.csv("C:Inputs//marble_certainty_321.csv")
rownames(marble_uncertainties)<-marble_uncertainties$region
marble_uncertainties_simple<-marble_uncertainties[,-c(1:3)]
colnames(marble_uncertainties_simple)<-NULL
rownames(marble_uncertainties_simple)<-NULL

score_marble <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
    new_marble<-CMISTScore(as.numeric(marble_risk_simple[r,]),as.numeric(marble_uncertainties_simple[r,]))
    score_marble[nrow(score_marble)+1, ]<-new_marble
    }

rownames(score_marble)<-rownames(marble_risk)
colnames(score_marble)<-c("CMIST_Score", "CMIST_Upper", "CMIST_Lower", "Likelihood_Score", "Likelihood_Upper", "Likelihood_Lower", "Impact_Score", "Impact_Upper", "Impact_Lower")

score_marble_id<-score_marble
score_marble_id$region<-rownames(score_marble_id)
score_marble_id$species<-"Marble"

#visualize
ggplot(score_marble, aes(y=CMIST_Score, x=rownames(score_marble))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


############### Red swamp crayfish
redswamp_risk<-read.csv("C:Inputs//redswamp_risk.csv")
rownames(redswamp_risk)<-redswamp_risk$region
redswamp_risk_simple<-redswamp_risk[,-c(1:3)]
colnames(redswamp_risk_simple)<-NULL
rownames(redswamp_risk_simple)<-NULL

redswamp_uncertainties<-read.csv("C:Inputs//redswamp_certainty_321.csv")
rownames(redswamp_uncertainties)<-redswamp_uncertainties$ID
redswamp_uncertainties_simple<-redswamp_uncertainties[,-c(1:3)]
redswamp_uncertainties_simple<-redswamp_uncertainties_simple
colnames(redswamp_uncertainties_simple)<-NULL
rownames(redswamp_uncertainties_simple)<-NULL

score_redswamp <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_redswamp<-CMISTScore(as.numeric(redswamp_risk_simple[r,]),as.numeric(redswamp_uncertainties_simple[r,]))
  score_redswamp[nrow(score_redswamp)+1, ]<-new_redswamp
}

rownames(score_redswamp)<-rownames(redswamp_risk)
colnames(score_redswamp)<-c("CMIST_Score", "CMIST_Upper", "CMIST_Lower", "Likelihood_Score", "Likelihood_Upper", "Likelihood_Lower", "Impact_Score", "Impact_Upper", "Impact_Lower")

score_redswamp_id<-score_redswamp
score_redswamp_id$region<-rownames(score_redswamp_id)
score_redswamp_id$species<-"Red Swamp"

ggplot(score_redswamp, aes(y=CMIST_Score, x=rownames(score_redswamp))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

######## White River crayfish
whiteriver_risk<-read.csv("C:Inputs//whiteriver_risk.csv")
rownames(whiteriver_risk)<-whiteriver_risk$region
whiteriver_risk_simple<-whiteriver_risk[,-c(1:3)]
colnames(whiteriver_risk_simple)<-NULL
rownames(whiteriver_risk_simple)<-NULL

whiteriver_uncertainties<-read.csv("C:Inputs//whiteriver_certainty_321.csv")
rownames(whiteriver_uncertainties)<-whiteriver_uncertainties$ID
whiteriver_uncertainties_simple<-whiteriver_uncertainties[,-c(1:3)]
whiteriver_uncertainties_simple<-whiteriver_uncertainties_simple
colnames(whiteriver_uncertainties_simple)<-NULL
rownames(whiteriver_uncertainties_simple)<-NULL

score_whiteriver <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_whiteriver<-CMISTScore(as.numeric(whiteriver_risk_simple[r,]),as.numeric(whiteriver_uncertainties_simple[r,]))
  score_whiteriver[nrow(score_whiteriver)+1, ]<-new_whiteriver
}

rownames(score_whiteriver)<-rownames(whiteriver_risk)
colnames(score_whiteriver)<-c("CMIST_Score", "CMIST_Upper", "CMIST_Lower", "Likelihood_Score", "Likelihood_Upper", "Likelihood_Lower", "Impact_Score", "Impact_Upper", "Impact_Lower")

score_whiteriver_id<-score_whiteriver
score_whiteriver_id$region<-rownames(score_whiteriver_id)
score_whiteriver_id$species<-"White River"

ggplot(score_whiteriver, aes(y=CMIST_Score, x=rownames(score_whiteriver))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


############### Rusty crayfish
rusty_risk<-read.csv("C:Inputs//rusty_risk.csv")
rownames(rusty_risk)<-rusty_risk$region
rusty_risk_simple<-rusty_risk[,-c(1:3)]
colnames(rusty_risk_simple)<-NULL
rownames(rusty_risk_simple)<-NULL

rusty_uncertainties<-read.csv("C:Inputs//rusty_certainty_321.csv")
rownames(rusty_uncertainties)<-rusty_uncertainties$ID
rusty_uncertainties_simple<-rusty_uncertainties[,-c(1:3)]
rusty_uncertainties_simple<-rusty_uncertainties_simple
colnames(rusty_uncertainties_simple)<-NULL
rownames(rusty_uncertainties_simple)<-NULL

score_rusty <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_rusty<-CMISTScore(as.numeric(rusty_risk_simple[r,]),as.numeric(rusty_uncertainties_simple[r,]))
  score_rusty[nrow(score_rusty)+1, ]<-new_rusty
}

rownames(score_rusty)<-rownames(rusty_risk)
colnames(score_rusty)<-c("CMIST_Score", "CMIST_Upper", "CMIST_Lower", "Likelihood_Score", "Likelihood_Upper", "Likelihood_Lower", "Impact_Score", "Impact_Upper", "Impact_Lower")

score_rusty_id<-score_rusty
score_rusty_id$region<-rownames(score_rusty_id)
score_rusty_id$species<-"Rusty"

ggplot(score_rusty, aes(y=CMIST_Score, x=rownames(score_rusty))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


############### Spinycheek crayfish
spinycheek_risk<-read.csv("C:Inputs//spinycheek_risk.csv")
rownames(spinycheek_risk)<-spinycheek_risk$region
spinycheek_risk_simple<-spinycheek_risk[,-c(1:3)]
colnames(spinycheek_risk_simple)<-NULL
rownames(spinycheek_risk_simple)<-NULL

spinycheek_uncertainties<-read.csv("C:Inputs//spinycheek_certainty_321.csv")
rownames(spinycheek_uncertainties)<-spinycheek_uncertainties$ID
spinycheek_uncertainties_simple<-spinycheek_uncertainties[,-c(1:3)]
spinycheek_uncertainties_simple<-spinycheek_uncertainties_simple
colnames(spinycheek_uncertainties_simple)<-NULL
rownames(spinycheek_uncertainties_simple)<-NULL

score_spinycheek <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_spinycheek<-CMISTScore(as.numeric(spinycheek_risk_simple[r,]),as.numeric(spinycheek_uncertainties_simple[r,]))
  score_spinycheek[nrow(score_spinycheek)+1, ]<-new_spinycheek
}

rownames(score_spinycheek)<-rownames(spinycheek_risk)
colnames(score_spinycheek)<-c("CMIST_Score", "CMIST_Upper", "CMIST_Lower", "Likelihood_Score", "Likelihood_Upper", "Likelihood_Lower", "Impact_Score", "Impact_Upper", "Impact_Lower")

score_spinycheek_id<-score_spinycheek
score_spinycheek_id$region<-rownames(score_spinycheek_id)
score_spinycheek_id$species<-"Spiny-cheek"
score_spinycheek_id

ggplot(score_spinycheek, aes(y=CMIST_Score, x=rownames(score_spinycheek))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

############### Virile crayfish
virile_risk<-read.csv("C:Inputs//virile_risk.csv")
virile_risk<-virile_risk[-c(17:21),] #Take out regions where crayfish is considered native
rownames(virile_risk)<-virile_risk$region
virile_risk_simple<-virile_risk[,-c(1:3)]
colnames(virile_risk_simple)<-NULL
rownames(virile_risk_simple)<-NULL

virile_uncertainties<-read.csv("C:Inputs//virile_certainty_321.csv")
virile_uncertainties<-virile_uncertainties[-c(17:21),]

rownames(virile_uncertainties)<-virile_uncertainties$ID
virile_uncertainties_simple<-virile_uncertainties[,-c(1:3)]
virile_uncertainties_simple<-virile_uncertainties_simple
colnames(virile_uncertainties_simple)<-NULL
rownames(virile_uncertainties_simple)<-NULL

score_virile <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:16) {
  new_virile<-CMISTScore(as.numeric(virile_risk_simple[r,]),as.numeric(virile_uncertainties_simple[r,]))
  score_virile[nrow(score_virile)+1, ]<-new_virile
}

rownames(score_virile)<-rownames(virile_risk)
colnames(score_virile)<-c("CMIST_Score", "CMIST_Upper", "CMIST_Lower", "Likelihood_Score", "Likelihood_Upper", "Likelihood_Lower", "Impact_Score", "Impact_Upper", "Impact_Lower")

score_virile_id<-score_virile
score_virile_id$region<-rownames(score_virile_id)
score_virile_id$species<-"Virile"

ggplot(score_virile, aes(y=CMIST_Score, x=rownames(score_virile))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

############### Signal crayfish
signal_risk<-read.csv("C:Inputs//signal_risk.csv")
signal_risk<-signal_risk[-21,] #Take out regions where crayfish is considered native

rownames(signal_risk)<-signal_risk$region
signal_risk_simple<-signal_risk[,-c(1:3)]
colnames(signal_risk_simple)<-NULL
rownames(signal_risk_simple)<-NULL

signal_uncertainties<-read.csv("C:Inputs//signal_certainty_321.csv")
signal_uncertainties<-signal_uncertainties[-21,]

rownames(signal_uncertainties)<-signal_uncertainties$ID
signal_uncertainties_simple<-signal_uncertainties[,-c(1:3)]
signal_uncertainties_simple<-signal_uncertainties_simple
colnames(signal_uncertainties_simple)<-NULL
rownames(signal_uncertainties_simple)<-NULL

score_signal <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:20) {
  new_signal<-CMISTScore(as.numeric(signal_risk_simple[r,]),as.numeric(signal_uncertainties_simple[r,]))
  score_signal[nrow(score_signal)+1, ]<-new_signal
}

rownames(score_signal)<-rownames(signal_risk)
colnames(score_signal)<-c("CMIST_Score", "CMIST_Upper", "CMIST_Lower", "Likelihood_Score", "Likelihood_Upper", "Likelihood_Lower", "Impact_Score", "Impact_Upper", "Impact_Lower")

score_signal_id<-score_signal
score_signal_id$region<-rownames(score_signal_id)
score_signal_id$species<-"Signal"
score_signal_id

ggplot(score_signal, aes(y=CMIST_Score, x=rownames(score_signal))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

############### Allegheny crayfish
allegheny_risk<-read.csv("C:Inputs//allegheny_risk.csv")
rownames(allegheny_risk)<-allegheny_risk$region
allegheny_risk_simple<-allegheny_risk[,-c(1:3)]
colnames(allegheny_risk_simple)<-NULL
rownames(allegheny_risk_simple)<-NULL

allegheny_uncertainties<-read.csv("C:Inputs//allegheny_certainty_321.csv")
rownames(allegheny_uncertainties)<-allegheny_uncertainties$ID
allegheny_uncertainties_simple<-allegheny_uncertainties[,-c(1:3)]
allegheny_uncertainties_simple<-allegheny_uncertainties_simple
colnames(allegheny_uncertainties_simple)<-NULL
rownames(allegheny_uncertainties_simple)<-NULL

score_allegheny <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_allegheny<-CMISTScore(as.numeric(allegheny_risk_simple[r,]),as.numeric(allegheny_uncertainties_simple[r,]))
  score_allegheny[nrow(score_allegheny)+1, ]<-new_allegheny
}

rownames(score_allegheny)<-rownames(allegheny_risk)
colnames(score_allegheny)<-c("CMIST_Score", "CMIST_Upper", "CMIST_Lower", "Likelihood_Score", "Likelihood_Upper", "Likelihood_Lower", "Impact_Score", "Impact_Upper", "Impact_Lower")

score_allegheny_id<-score_allegheny
score_allegheny_id$region<-rownames(score_allegheny_id)
score_allegheny_id$species<-"Allegheny"

ggplot(score_allegheny, aes(y=CMIST_Score, x=rownames(score_allegheny))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()



# Combining scores and creating score plot (Fig 1)------------------------------------------------

#Combining all species scores
scores_combined<-rbind(score_marble_id, score_redswamp_id, score_whiteriver_id, score_rusty_id, 
                       score_spinycheek_id, score_allegheny_id, score_virile_id, score_signal_id)

species.region.expand<-expand.grid(region=unique(score_spinycheek_id$region), 
                                   species=c( "Rusty" ,"Signal", "Virile","Red Swamp",
                                             "Spiny-cheek", "White River", "Allegheny",  "Marble"))

scores_combined<-merge(scores_combined, species.region.expand, all=T)

scores_combined$species<-plyr::revalue(scores_combined$species, c("Spiny-cheek" = "spiny-cheek", "Rusty" ="rusty", 
                                                                  "Allegheny"="Allegheny", 
                                                                  "Virile"="virile", "Signal"="signal",
                                                    "White River"="white river","Red Swamp"="red swamp", "Marble"="marbled"))
scores_combined$species<-ordered(scores_combined$species, levels=c( "spiny-cheek", "rusty", "Allegheny", "virile", "signal",
  "white river","red swamp", "marbled"))

#Colour scheme
brewerset3<-c("Allegheny"="#8DD3C7", "marbled" = "#FFFF02" , "red swamp"="#BEBADA", "rusty"= "#FB8072" ,"signal"= "#80B1D3", 
              "virile"= "#FDB462" ,"spiny-cheek" ="#B3DE69", "white river"= "#FCCDE5")


scores_combined$region<-factor(scores_combined$region, levels=c(
                                "101", "102", "103", "104", "105", "106", "107", "108", "109", 
                                "110", "111", "112", "113", "114", 
                                "115", "116", "117", 
                                "118", "119", "120", "142"))

scores_combined$region_named<-scores_combined$region
levels(scores_combined$region_named)<-c(
 "101"= "101 Alaskan \nCoastal","102"= "102 Upper Yukon","103"= "103 Pacific \nCoastal", "104"= "104 Upper \nMackenzie", 
 "105"= "105 Lower \nMackenzie","106"= "106 Arctic \nCoastal","107"= "107 Upper \nSaskatchewan","108"= "108 Middle \nSaskatchewan", 
 "109"= "109 Winnipeg \nLakes","110"= "110 Southern \nHudson Bay","111"= "111 Western \nHudson Bay","112"= "112 Arctic \nArchepelago", 
"113"=  "113 Eastern \nHudson Bay","114"= "114 Gulf of \nSt. Lawrence", 
"115"=  "115 Atlantic \nIslands","116"= "116 Great Lakes","117"= "117 St. Lawrence", 
"118"=  "118 Atlantic \nDrainages","119"= "119 Scotia Fundy", 
"120"=  "120 Columbia \nGlaciate","142"= "142 Upper \nMissouri")

scores_combined$pattern.decision<-"NA"

for (i in 1:length(scores_combined$CMIST_Score)) {
  if(is.na(scores_combined$CMIST_Score[i])==TRUE) {
         scores_combined$pattern.decision[i] <- "yes"
  } else {
         scores_combined$pattern.decision[i] <- "no" 
  }
  }

##Part of figure 1
all_score_plot<-ggplot(scores_combined, aes(y=CMIST_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=3, position=position_dodge(width=0.5))+scale_colour_manual(values=brewerset3)+
  xlab("Ecoregion")+ylab("Adjusted CMIST score")+ scale_y_continuous(limits = c(1, 9), breaks = c(1,3,5,7,9))+
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

all_score_plot

ggsave(all_score_plot, file="Plots/all_score_plot.png", dpi=600)

#Likelihood
all_likelihood_plot<-ggplot(scores_combined, aes(y=Likelihood_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))+scale_colour_manual(values=brewerset3)
all_likelihood_plot
ggsave(all_likelihood_plot, file="Plots/all_likelihood_plot.png", dpi=600)

#Impact
all_impact_plot<-ggplot(scores_combined, aes(y=Impact_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=Impact_Lower, ymax=Impact_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))+scale_colour_manual(values=brewerset3)
ggsave(all_impact_plot, file="Plots/all_impact_plot.png", dpi=600)
all_impact_plot



# Calculations for numbers referenced in text  -----------------------------

scores_combined_max<-scores_combined %>% group_by(region) %>% top_n(1, CMIST_Score) 
scores_combined_max_species<-scores_combined %>% group_by(species) %>% top_n(1, CMIST_Score) 
scores_combined$confsize<-scores_combined$CMIST_Upper-scores_combined$CMIST_Lower
scores_combined_conf_mean<-scores_combined %>% group_by(species) %>%  summarise_if(is.numeric, mean, na.rm=TRUE) 
scores_combined_conf_mean_reg<-scores_combined %>% group_by(region) %>%  summarise_if(is.numeric, mean, na.rm=TRUE) 

##### calculations for numbers in text
max(na.omit(scores_combined$CMIST_Score))
scores_combined$CMIST_Lower[scores_combined$CMIST_Score==max(na.omit(scores_combined$CMIST_Score))]
scores_combined$CMIST_Upper[scores_combined$CMIST_Score==max(na.omit(scores_combined$CMIST_Score))]
min(na.omit(scores_combined$CMIST_Score))
scores_combined$CMIST_Lower[scores_combined$CMIST_Score==min(na.omit(scores_combined$CMIST_Score))]
scores_combined$CMIST_Upper[scores_combined$CMIST_Score==min(na.omit(scores_combined$CMIST_Score))]

max(na.omit(scores_combined$Likelihood_Score))
scores_combined$Likelihood_Lower[scores_combined$Likelihood_Score==max(na.omit(scores_combined$Likelihood_Score))]
scores_combined$Likelihood_Upper[scores_combined$Likelihood_Score==max(na.omit(scores_combined$Likelihood_Score))]
min(na.omit(scores_combined$Likelihood_Score))
scores_combined$Likelihood_Lower[scores_combined$Likelihood_Score==min(na.omit(scores_combined$Likelihood_Score))]
scores_combined$Likelihood_Upper[scores_combined$Likelihood_Score==min(na.omit(scores_combined$Likelihood_Score))]

max(na.omit(scores_combined$Impact_Score))
scores_combined$Impact_Lower[scores_combined$Impact_Score==max(na.omit(scores_combined$Impact_Score))]
scores_combined$Impact_Upper[scores_combined$Impact_Score==max(na.omit(scores_combined$Impact_Score))]
min(na.omit(scores_combined$Impact_Score))
scores_combined$Impact_Lower[scores_combined$Impact_Score==min(na.omit(scores_combined$Impact_Score))]
scores_combined$Impact_Upper[scores_combined$Impact_Score==min(na.omit(scores_combined$Impact_Score))]


# Plotting for Maps (Fig 1 and Fig 2) --------------------------------------------------------------------
#FEOW base maps from Abell et al. 2008
feow_sf<-sf::st_read("C:Inputs//feow_hydrosheds.shp")
feow_sf_crop<- feow_sf[feow_sf$FEOW_ID==c(101:120, 142), ]
feow_sf_crop_2 <- feow_sf_crop[, -2]
feow_sf_crop_2<-merge(feow_sf_crop_2, scores_combined[,c("species", "region", "CMIST_Score", "pattern.decision")], by.x=c("FEOW_ID"), by.y=c("region"))

# Provinces base map from GADM
provinces <- getData("GADM", country="CAN", level=1)

# Map part of Figure 1 generation
region_id_num_plot<-ggplot(feow_sf_crop_2) + geom_sf(data= feow_sf_crop_2, fill = "#eeeeee")+
  borders(database="lakes", fill="black", colour="black")+
  borders(database=provinces, linetype="dashed")+
  borders(database="world", regions="canada", colour="#767676")+
  geom_sf_label(data= feow_sf_crop_2, aes(label = FEOW_ID), colour="black", fill="white")+
  xlim(-170, -50) + ylim(40, 85)+ 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

# region_id_num_plot
all_score_plot_id<-region_id_num_plot + all_score_plot + plot_layout(ncol=1, heights=c(2,1)) 
# all_score_plot_id
ggsave(all_score_plot_id, file="Plots/fig.1.tiff", unit="cm",width=18.2,height=10, dpi=600)


#Figure 2 generation
#colour coding
na.value.forplot <- 'grey'
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(min(na.omit(feow_sf_crop_2$CMIST_Score)), max(na.omit(feow_sf_crop_2$CMIST_Score))), na.value='grey')
sc_1_9 <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 9), na.value='grey', guide="colourbar", breaks=c(1,3,5,7,9))

species_names = levels( scores_combined$species)
species_plots = list()
pattern.list<-c("yes"="grey", "no"="none")


# Loop for plotting map coloured by CMIST_Score in each region 
for(species_ in species_names) {
  species_plots[[species_]] = ggplot(feow_sf_crop_2 %>% filter(species == species_)) + 
                              geom_sf(aes(fill = CMIST_Score, color='NA'))+ sc_1_9+
                              scale_color_manual(values = 'grey', labels = 'Native range', name=NULL) +
                              guides(color = guide_legend(override.aes = list(fill = na.value.forplot)))+
                              borders(database="lakes", fill="black", colour="black")+
                              borders(database=provinces, linetype="dashed")+
                              borders(database="world", regions="canada", colour="#767676")+
                              xlim(-170, -50) + ylim(40, 85)  + ggtitle(paste0(species_))+
                              theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                    axis.text.y=element_blank(),axis.ticks=element_blank(),
                                    axis.title.x=element_blank(),
                                    axis.title.y=element_blank(),
                                    panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                    panel.grid.minor=element_blank(),plot.background=element_blank())+
                              theme(plot.margin = margin(0, 0, 0, 0, "cm"))
  
    # print(species_plots[[species_]])
     ggsave(species_plots[[species_]], file=paste0("Plots/plot_", species_,".png"), dpi=300)
                          }

###Generating figure 2
allspp_plot <-  wrap_plots( species_plots, ncol = 2) + plot_layout(guides = 'collect') 
allspp_plot
ggsave(allspp_plot, file="Plots/fig.2.tiff", unit="cm",width=18.2,height=23.7, dpi=600)


# Biplots for likelihood vs. invasion (Fig 3) -----------------------------------------------------------------

regions = unique(scores_combined$region)
region_plots = list()

for(region_ in regions) {
  region_plots[[region_]] = ggplot(scores_combined %>% filter(region == region_), aes(y=Likelihood_Score, x=Impact_Score, colour=species)) + 
    ggtitle(scores_combined$region_named[scores_combined$region == region_])+
    xlab("Impact of Invasion") + theme(axis.title = element_text(size = 10)) + ylab("Likelihood of Invasion")+
    theme(plot.title = element_text(size=10))+
    geom_rect(data=NULL,aes(xmin=2,xmax=3,ymin=2,ymax=3), colour="black", fill = NA, linetype="dashed")+
    geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper), size=1, position=position_dodge(width=0.1)) +
    geom_errorbarh(aes(xmin=Impact_Lower, xmax=Impact_Upper), size=1) +
    geom_point(size=3,position=position_dodge(width=0.1) )+
    ylim(1,3.1)+xlim(1,3.1)+
    scale_color_manual(values=brewerset3, drop=FALSE, limits=species_names)+
    coord_equal()
  # print(region_plots[[region_]])
  # ggsave(region_plots[[region_]], file=paste0("Plots/plot_", region_,".png"), dpi=300)
}

#Generating figure 3
allregions_plot <-  wrap_plots( region_plots, ncol = 7) + plot_layout(guides = 'collect') 
allregions_plot
ggsave(allregions_plot, file="Plots/fig3.tiff", unit="cm",width=18.2,height=23.7, dpi=600)

 