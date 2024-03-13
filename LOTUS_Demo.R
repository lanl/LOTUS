
##################################################################################################################################
### LOTUS_Illustration.R
### 24 JAN 2024 
##################################################################################################################################


##################################################################################################################################
### Set Up Workspace
##################################################################################################################################
### Start from clean workspace
rm(list=ls())

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
#                 CHANGE ONLY HERE                      #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #                                         
### Set Working Directories...                          #
# For Saving Objects                                    #
SAVE_path = "~/Desktop/00_SaveDatHere/"                 #
FIG_path = paste(SAVE_path, "Figures/", sep="")         #
# For Loading Data                                      #
LOAD_path = "~/Desktop/"                                #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

### Load necessary libraries
package.vect <- c('stats', 'ggplot2', 'reshape2', 'dplyr', 'tidyr','data.table', 'gridExtra', 'rstatix', 'ranger', 
                  'scales', 'lubridate', 'raster', 'RColorBrewer', 'stringr', 'ggforce')
package.check <- lapply(package.vect, function(x){
     if(!require(x, character.only = TRUE)){
          install.packages(x, dependencies = TRUE)
          library(x, character.only = TRUE)
     }
})
##################################################################################################################################

##################################################################################################################################
##### Make Orbit Plots for Paper
##################################################################################################################################
### Read in Data
dat_to_plot=fread(file=paste(LOAD_path, 'Orbit_Data/Orbit_471.csv', sep=''))
# Load IBEX Color Palette
ibex_palette = read.csv(paste(LOAD_path, 'ibex_rgb.csv', sep=''))
# Define Orbit of Interest 
OoI = 471



### Make Plots
# Figure 1a: Raw Data, All ESAs................................................................................................... Figure 1a
{
Raw_Data = ggplot(subset(dat_to_plot, ESA_Sweep!=1)) +
        geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill = Counts)) +
        facet_grid(paste0('ESA', ESA_Sweep)~., scales = 'free_x') +
        theme_classic() +
        xlab('Date') +
        ylab('Angle Bin') +
        theme(panel.spacing.x = unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
              legend.key.width = unit(2,"cm"), legend.position="bottom") +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"), 
                         expand = c(0, 0)) +
        theme(legend.position = '') +
        scale_fill_gradientn('ENA Rate', values = rescale(c(0,1,5,10,100)), 
                             colors = c('black', 'blue', 'gold', 'orange', 'red'),
                             limits = c(0,100),guide="colourbar") +
        labs(title = paste0('Observed Data for Orbit ', OoI)) +
        scale_alpha_continuous('Good Times', range = c(0.1,1), breaks = c(0,1),labels = c('No','Yes')) +
        theme(axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12))
}
ggsave(plot=Raw_Data, 
       filename=paste(FIG_path, "Fig1a_Orbit ", OoI, "_RawData.jpg", sep=''), 
       width=9, height=7, units="in")

# Figure 1b: SME Labels, All ESAs................................................................................................. Figure 1b
{
SME_Labels = ggplot(subset(dat_to_plot, ESA_Sweep!=1)) +
        geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill = Counts, alpha = Label)) +
        facet_grid(paste0('ESA',ESA_Sweep)~., scales = 'free_x') +
        theme_classic() +
        xlab('Date') +
        ylab('Angle Bin') +
        theme(panel.spacing.x=unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
              legend.key.width = unit(2,"cm"), legend.position="bottom") +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"), 
                         expand = c(0, 0)) +
        scale_fill_gradientn('ENA Rate', values = rescale(c(0,1,5,10,100)), 
                             colors = c('black', 'blue', 'gold', 'orange', 'red'), 
                             limits = c(0,100),guide="colourbar") +
        labs(title = paste0('Manual SME Labels for Orbit ', OoI)) +
        scale_alpha_continuous('Good Times', range = c(0.1,1), breaks = c(0,1), labels = c('No','Yes')) + 
        theme(axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12))
}
ggsave(plot=SME_Labels, 
       filename=paste(FIG_path, "Fig1b_Orbit ", OoI, "_SMELabels.jpg", sep=''), 
       width=9, height=7, units="in")
this.esa=6
for(which.ESA in c(2,3,4,5,6)){
    # Original Plot 
    {
        Original = ggplot(subset(dat_to_plot, ESA%in%which.ESA)) +
                   geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill = Counts)) +
                   facet_grid(paste0('ESA', ESA)~., scales = 'free_x') +
                   theme_classic() +
                   xlab('Date') +
                   ylab('Angle Bin') +
                   theme(panel.spacing.x = unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                         legend.key.width = unit(2,"cm"), legend.position="bottom") +
                   scale_y_continuous(expand = c(0, 0)) +
                   scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"), 
                                    expand = c(0, 0)) +
                   theme(legend.position = '') +
                   scale_fill_gradientn('ENA Rate', values = rescale(c(0,1,5,10,100)), 
                                        colors = c('black', 'blue', 'gold', 'orange', 'red'),
                                        limits = c(0,100),guide="colourbar") +
                   labs(title = paste0('Observed Data for Orbit ', OoI)) +
                   scale_alpha_continuous('Good Times', range = c(0.1,1), breaks = c(0,1),labels = c('No','Yes')) +
                   theme(axis.title.x = element_text(size=16),
                         axis.title.y = element_text(size=16), 
                         axis.text.x = element_text(size=14),
                         axis.text.y = element_text(size=14))
     }
    # SME Labels
    {
          SME = ggplot(subset(dat_to_plot, ESA%in%which.ESA)) +
               geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill = Counts, alpha = Label)) +
               facet_grid(paste0('ESA',ESA_Sweep)~., scales = 'free_x') +
               theme_classic() +
               xlab('Date') +
               ylab('Angle Bin') +
               theme(panel.spacing.x=unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                     legend.key.width = unit(2,"cm"), legend.position="") +
               scale_y_continuous(expand = c(0, 0)) +
               scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"), 
                                expand = c(0, 0)) +
               scale_fill_gradientn('ENA Rate', values = rescale(c(0,1,5,10,100)), 
                                    colors = c('black', 'blue', 'gold', 'orange', 'red'), 
                                    limits = c(0,100),guide="colourbar") +
               labs(title = paste0('Manual SME Labels for Orbit ', OoI)) +
               scale_alpha_continuous('Good Times', range = c(0.1,1), breaks = c(0,1), labels = c('No','Yes')) + 
               theme(axis.title.x = element_text(size=16),
                     axis.title.y = element_text(size=16), 
                     axis.text.x = element_text(size=14),
                     axis.text.y = element_text(size=14))
     }
    # LOTUS Stage 1 Heat Map 
    {
     L1.heat = ggplot(subset(dat_to_plot, ESA%in%which.ESA)) +
               geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill = L1_Probs)) +
               facet_grid(paste0('ESA',ESA_Sweep)~., scales = 'free_x') +
               theme_classic() +
               xlab('Date') +
               ylab('Angle Bin') +
               theme(panel.spacing.x=unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                     legend.key.width = unit(2,"cm"), legend.position="") +
               scale_y_continuous(expand = c(0, 0)) +
               scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"), 
                                expand = c(0, 0)) +
               theme(legend.position = '') +
               scale_fill_gradientn('ENA Rate', values = rescale(c(0, 0.4, 1)), 
                                    colors=c("tomato", "white", "steelblue"), 
                                    limits = c(0,1), guide="colourbar") +
               labs(title = paste0('LOTUS Stage 1: Per-Observation Probabilities for Orbit ', OoI)) +
               theme(axis.title.x = element_text(size=16),
                     axis.title.y = element_text(size=16), 
                     axis.text.x = element_text(size=14),
                     axis.text.y = element_text(size=14))
     }
    # LOTUS Stage 2 Heat Map 
    {
     L2.heat = ggplot(subset(dat_to_plot, ESA%in%which.ESA)) +
               geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill = L2_Probs_SPICE)) +
               facet_grid(paste0('ESA',ESA_Sweep)~., scales = 'free_x') +
               theme_classic() +
               xlab('Date') + 
               ylab('Angle Bin')+
               theme(panel.spacing.x=unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                     legend.key.width = unit(2,"cm"), legend.position="") +
               scale_y_continuous(expand = c(0, 0)) +
               scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"),
                                expand = c(0, 0)) +
               theme(legend.position = '') +
               scale_fill_gradientn('ENA Rate', values = rescale(c(0, 0.4, 1)), 
                                    colors=c("tomato", "white", "steelblue"), 
                                    limits = c(0,1), guide="colourbar") +
               labs(title = paste0('LOTUS Stage 2: Probabilities for Orbit ', OoI)) +
               theme(axis.title.x = element_text(size=16),
                     axis.title.y = element_text(size=16), 
                     axis.text.x = element_text(size=14),
                     axis.text.y = element_text(size=14))
     }
    # LOTUS Stage 3 Heat Map
    {
     L3.heat = ggplot(subset(dat_to_plot, ESA%in%which.ESA)) +
               geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill = L3_Probs)) +
               facet_grid(paste0('ESA',ESA_Sweep)~., scales = 'free_x') +
               theme_classic() +
               xlab('Date') +
               ylab('Angle Bin') +
               theme(panel.spacing.x=unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                     legend.key.width = unit(2,"cm"), legend.position="bottom") +
               scale_y_continuous(expand = c(0, 0)) +
               scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"),
                                expand = c(0, 0)) +
               theme(legend.position = 'bottom') +
               scale_fill_gradientn('Probability of Good Time', values = rescale(c(0, 0.4, 1)), 
                                    colors=c("tomato", "white","steelblue"), 
                                    limits = c(0,1), guide="colourbar") +
               labs(title = paste0('LOTUS Stage 3: Adjusted Probabilities for Orbit ',OoI)) +
               theme(axis.title.x = element_text(size=16),
                     axis.title.y = element_text(size=16), 
                     axis.text.x = element_text(size=14),
                     axis.text.y = element_text(size=14))
     }
    Orbit_Probs = grid.arrange(Original, SME, L1.heat, L2.heat, L3.heat, 
                               nrow=5, ncol=1, 
                               heights=c(0.7, 0.7, 0.7, 0.7, 1.0), 
                               left=paste("Progression of Heat Maps for ESA ", which.ESA, sep=''))
    if(which.ESA==this.esa){
        # Figure 2a: Probabilistic Heat Maps...................................................................................... Figure 2a
        ggsave(plot=Orbit_Probs, 
               filename=paste(FIG_path, "Fig2a_Orbit", OoI, "_ESA", this.esa, "_Probs.jpg", sep=''), 
               width=11, height=10, units="in") 
    } else{
        # Figure A19a through A22a: Probabilistic Heat Maps....................................................................... Figures 17a - 22a
        ggsave(plot=Orbit_Probs, 
               filename=paste(FIG_path, "Fig", 17+which.ESA, "a_Orbit", OoI, "_ESA", which.ESA, "_Probs.jpg", sep=''), 
               width=11, height=10, units="in")
    }
    

    # LOTUS Stage 1 Labels
    {
         L1 = ggplot(subset(dat_to_plot, ESA%in%which.ESA))+
              geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill=Counts, alpha = L1_Labs)) +
              facet_grid(paste0('ESA',ESA_Sweep)~., scales = 'free_x') +
              theme_classic() +
              xlab('Date') +
              ylab('Angle Bin') +
              theme(panel.spacing.x=unit(0.1, "lines"), panel.background = element_rect(fill = 'white'),
                    legend.key.width = unit(2,"cm"), legend.position="") +
              scale_y_continuous(expand = c(0, 0)) +
              scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"),
                               expand = c(0, 0)) +
              theme(legend.position = '') +
              scale_fill_gradientn('ENA Rate', values = rescale(c(0,1,5,10,100)),
                                   colors = c('black', 'blue', 'gold', 'orange', 'red'),
                                   limits = c(0,100),guide="colourbar") +
              labs(title = paste0('LOTUS Stage 1: Per-Observation Labels for Orbit ', OoI)) +
              scale_alpha_continuous('Good Times', range = c(0.1,1), breaks = c(0,1),labels = c('No','Yes')) +
              theme(axis.title.x = element_text(size=16),
                    axis.title.y = element_text(size=16),
                    axis.text.x = element_text(size=14),
                    axis.text.y = element_text(size=14))
     }
    # LOTUS Stage 2 Labels
    {
          L2 = ggplot(subset(dat_to_plot, ESA%in%which.ESA)) +
               geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill=Counts, alpha = L2_Labs)) +
               facet_grid(paste0('ESA',ESA_Sweep)~., scales = 'free_x') +
               theme_classic() +
               xlab('Date') +
               ylab('Angle Bin') +
               theme(panel.spacing.x=unit(0.1, "lines"), panel.background = element_rect(fill = 'white'),
                     legend.key.width = unit(2,"cm"), legend.position="") +
               scale_y_continuous(expand = c(0, 0)) +
               scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"),
                                expand = c(0, 0)) +
               theme(legend.position = '') +
               scale_fill_gradientn('ENA Rate', values = rescale(c(0,1,5,10,100)),
                                    colors = c('black', 'blue', 'gold', 'orange', 'red'),
                                    limits = c(0,100), guide="colourbar") +
               labs(title = paste0('LOTUS Stage 2: Probabilistic Labels for Orbit ', OoI)) +
               scale_alpha_continuous('Good Times', range = c(0.1,1), breaks = c(0,1),labels = c('No','Yes')) +
               theme(axis.title.x = element_text(size=16),
                     axis.title.y = element_text(size=16),
                     axis.text.x = element_text(size=14),
                     axis.text.y = element_text(size=14))
     }
    # LOTUS Stage 3 Labels
    {
          L3 = ggplot(subset(dat_to_plot, ESA%in%which.ESA))+
               geom_tile(aes(x = Time_UTC, y = Angle_Bin, fill=Counts, alpha = L3_Labs)) +
               facet_grid(paste0('ESA',ESA_Sweep)~., scales = 'free_x') +
               theme_classic() +
               xlab('Date') +
               ylab('Angle Bin') +
               theme(panel.spacing.x=unit(0.1, "lines"), panel.background = element_rect(fill = 'white'),
                     legend.key.width = unit(2,"cm"), legend.position="bottom") +
               scale_y_continuous(expand = c(0, 0)) +
               scale_x_datetime(breaks=scales::date_breaks("1 day"), labels=scales::date_format("%Y-%m-%d"),
                                expand = c(0, 0))+
               theme(legend.position = 'bottom')+
               scale_fill_gradientn('ENA Rate', values = rescale(c(0,1,5,10,100)),
                                    colors = c('black', 'blue', 'gold', 'orange', 'red'),
                                    limits = c(0,100),guide="colourbar") +
               labs(title = paste0('LOTUS Stage 3: Adjusted Labels for Orbit ', OoI)) +
               scale_alpha_continuous('Good Times', range = c(0.1,1), breaks = c(0,1), labels = c('No','Yes')) +
               theme(axis.title.x = element_text(size=16),
                     axis.title.y = element_text(size=16),
                     axis.text.x = element_text(size=14),
                     axis.text.y = element_text(size=14))
     }
    Orbit_Labs = grid.arrange(Original, SME, L1, L2, L3,
                              nrow=5, ncol=1,
                              heights=c(0.7, 0.7, 0.7, 0.7, 1.0),
                              left=paste("Progression of Labels for ESA ", which.ESA, sep=''))
    if(which.ESA==this.esa){
        # Figure 2b: Labeled Data................................................................................................. Figure 2b
        ggsave(plot=Orbit_Labs,
               filename=paste(FIG_path, "Fig2b_Orbit", OoI, "_ESA", this.esa, "_Labs.jpg", sep=''),
               width=11, height=10, units="in")
    } else{
        # Figure A19b through A22b: Probabilistic Heat Maps....................................................................... Figure 17a - 22a
        ggsave(plot=Orbit_Labs,
               filename=paste(FIG_path, "Fig", 17+which.ESA, 'a_Orbit', OoI, "_ESA", which.ESA, "_Labs.jpg", sep=''),
               width=11, height=10, units="in")
    }
}
##################################################################################################################################


##################################################################################################################################
##### Explore Performance 
##################################################################################################################################
# Define ESAs used for Map Making
Map_ESAs <- c(2,3,4,5,6)

LOTUS_dat=fread(file=paste(LOAD_path, 'Orbit_Data/ENA_Rates.csv', sep=''))


#####################################
# GGPlot - Compare Rates for Orbit  #
#          of Interest (461)        #
#####################################
### Subset Data to Interesting Orbit
InterestingOrbit = subset(LOTUS_dat, ESA_Sweep%in%Map_ESAs & LOTUS_dat$Orbit==OoI)
InterestingOrbit$L3_ENARate = InterestingOrbit$L2_ENARate = InterestingOrbit$L1_ENARate = rep(0, nrow(InterestingOrbit))

### Get Rates for Labels - Table 1 
for(i in 2:6){
     InterestingOrbit$L3_ENARate[InterestingOrbit$ESA_Sweep==i] = 
         round(mean((InterestingOrbit$SME_Rate/InterestingOrbit$L3_Rate)[InterestingOrbit$ESA==i & 
                                                                         InterestingOrbit$L3_Rate!=0], na.rm=T), 3)
     InterestingOrbit$L1_ENARate[InterestingOrbit$ESA_Sweep==i] = 
         round(mean((InterestingOrbit$SME_Rate/InterestingOrbit$L1_Rate)[InterestingOrbit$ESA==i & 
                                                                         InterestingOrbit$L1_Rate!=0], na.rm=T), 3)
}

### Make Plots
# Figure 3: Scatter Plot Comparing Rates.......................................................................................... Figure 3
{
     # Make Plots to Compare Automatically Culled ENA Rates to Manually Culled ENA Rates
     SME_v_L1 <- ggplot(InterestingOrbit, aes(x=SME_Rate, y=L1_Rate)) + 
          geom_point(aes(col=as.factor(ESA_Sweep))) + 
          geom_abline() + 
          xlab("Manual SME ENA Rate") + 
          xlim(0,0.7) +
          ylab("LOTUS Stage 1 ENA Rate") + 
          ylim(0,0.7) +
          labs(title=paste("Orbit ", OoI, ": Ratios of ENA Rates", sep="")) +
          facet_grid(cols=vars(ESA_Sweep)) + 
          geom_label(aes(label=L1_ENARate), x=0.5, y=0.2) + 
          scale_colour_manual("ESA", values=c('lightskyblue', 'steelblue2', 'steelblue3', 'steelblue', 'steelblue4')) + 
          theme(legend.position="")
     
     # SME_v_L2 <- ggplot(InterestingOrbit, aes(x=SME_Rate, y=L2_Rate)) + 
     #      geom_point(aes(col=as.factor(ESA_Sweep))) + 
     #      geom_abline() + 
     #      xlab("Manual SME ENA Rate") + 
     #      xlim(0,0.75) +
     #      ylab("LOTUS Stage 2 ENA Rate") + 
     #      ylim(0,0.7) +
     #      labs(title=paste("Orbit ", OoI, ": Ratios of ENA Rates", sep="")) +
     #      facet_grid(cols=vars(ESA_Sweep)) + 
     #      geom_label(aes(label=L2_ENARate), x=0.5, y=0.2) + 
     #      scale_colour_manual("ESA", values=c('lightskyblue', 'steelblue2', 'steelblue3', 'steelblue', 'steelblue4')) + 
     #      theme(legend.position="")
     
     SME_v_L3 <- ggplot(InterestingOrbit, aes(x=SME_Rate, y=L3_Rate)) + 
          geom_point(aes(col=as.factor(ESA_Sweep))) + 
          geom_abline() + 
          xlab("Manual SME ENA Rate") + 
          xlim(0,0.75) +
          ylab("LOTUS Stage 3 ENA Rate") + 
          ylim(0,0.7) +
          labs(title=paste("Orbit ", OoI, ": Ratios of ENA Rates", sep="")) +
          facet_grid(cols=vars(ESA_Sweep)) + 
          geom_label(aes(label=L3_ENARate), x=0.5, y=0.2) + 
          scale_colour_manual("ESA", values=c('lightskyblue', 'steelblue2', 'steelblue3', 'steelblue', 'steelblue4')) + 
          theme(legend.position="bottom")
    
     OoI_Scatter = grid.arrange(SME_v_L1,
                                #SME_v_L2, 
                                SME_v_L3,
                                nrow=2, ncol=1, heights=c(0.85, 1))
}
ggsave(plot=OoI_Scatter, 
       filename=paste(FIG_path, "Fig3_Orbit ", OoI, "_ENARates_Scatter.jpg", sep=''), 
       width=8, height=5, units="in")

#####################################
# GGPlot - Compare Rates for all    # 
#          Orbits                   #
#####################################
### Make Plots 

# Figure 4: Box Plots of ENA Rate Ratios of SME to LOTUS.......................................................................... Figure 4
{
     melt.Orbit = melt(LOTUS_dat[, c(11,12)]/LOTUS_dat[, c(10,10)])
     melt.Orbit = cbind(melt.Orbit, LOTUS_dat[,1])
     melt.Orbit = melt.Orbit[-which(melt.Orbit$ESA==1),]
     levels(melt.Orbit$variable) = c("LOTUS Stage 1", "LOTUS Stage 3")
     
     all_Boxplot_Ratio = ggplot(subset(melt.Orbit, variable!="LOTUS Stage 2"), aes(x=value, y=variable, col=as.factor(ESA))) + 
                         geom_boxplot(outlier.size=0.5, outlier.shape=NA)  +
                         xlim(c(0,2)) +
                         scale_colour_manual(values=c('lightskyblue', 'skyblue2', 'steelblue2', 
                                                      'steelblue3', 'steelblue', 'steelblue4')) +
                         guides(col=guide_legend(title="ESA", position="bottom", nrow=1)) +
                         theme(legend.position="bottom") + 
                         xlab("ENA Rate Ratio") + ylab("") + 
                         labs(title="ENA Rates for Data Labelled Manually and with LOTUS for all Orbits")
}
ggsave(plot=all_Boxplot_Ratio, 
       filename=paste(FIG_path, "Fig4_AllOrbits_ENARates_Boxplot_Ratio.jpg", sep=''), 
       width=7, height=2.75, units="in")

### Look at Summary Statistics
# Compare SME to LOTUS Stage 1 Rate Ratios
summary((LOTUS_dat$SME_Rate/LOTUS_dat$L1_Rate)[is.finite(LOTUS_dat$SME_Rate/LOTUS_dat$L1_Rate) & 
                                               LOTUS_dat$ESA_Sweep %in% Map_ESAs])
# Compare SME to LOTUS Stage 3 ENA Rate Ratios
summary((LOTUS_dat$SME_Rate/LOTUS_dat$L3_Rate)[is.finite(LOTUS_dat$SME_Rate/LOTUS_dat$L3_Rate) & 
                                               LOTUS_dat$ESA_Sweep %in% Map_ESAs])


#####################################
# GGPLOT - Color by Exposure Times  #
#####################################
### Make Plots 
# Figure 5: Scatter plot of ENA Rate Ratios for LOTUS Stage 3 with Densities...................................................... Figure 5
{
     ### Prepare to Plot
     # Define Exposure Time Bins
     SME_exposure_time_ls <- quantile(LOTUS_dat$Time_SME, c(0, 0.10, 0.25, 0.5, 0.75, 0.90, 1.0))
     # Figure out which data fall into which bins 
     LOTUS_dat <- cbind(LOTUS_dat, SME_Time_Group=rep(0, nrow(LOTUS_dat)))
     for(i in 2:length(SME_exposure_time_ls)){
         LOTUS_dat$SME_Time_Group[which(LOTUS_dat$Time_SME >= SME_exposure_time_ls[i-1] & 
                                         LOTUS_dat$Time_SME <= SME_exposure_time_ls[i])] <- i-1
     }
     # Get ENA Rates for Labels
     LOTUS_dat$ENARate_L3 = rep(0, nrow(LOTUS_dat))
     for(i in 2:6){
          for(j in 1:6){
                LOTUS_dat$ENARate_L3[LOTUS_dat$ESA_Sweep==i & LOTUS_dat$SME_Time_Group==j] = 
                    round(mean((LOTUS_dat$SME_Rate/LOTUS_dat$L3_Rate)[LOTUS_dat$ESA==i & 
                                                                      LOTUS_dat$L3_Rate!=0 & 
                                                                      LOTUS_dat$SME_Time_Group==j], na.rm=T), 3)
          }
     }
     # Create Labels for Plots    
     Time_labs = paste('Time Group ', 1:6, sep="")
     names(Time_labs) = paste(1:6)
     ESA_labs = paste('ESA ', 1:6, sep='')
     names(ESA_labs) = paste(1:6)
     # Create Object to Store Plots
     SME_v_L3_Density = list()

     ### Make Plot 
     for(TG in 1:6){
        ann_text = mean(LOTUS_dat$ENARate_L3[LOTUS_dat$SME_Time_Group==TG & LOTUS_dat$ESA_Sweep!=1])
        ann_text = data.frame(ENARate_L3=round(ann_text,3), x=0.75, y=0.25)
        SME_v_L3_Density[[TG]] = ggplot(subset(LOTUS_dat, ESA_Sweep%in%Map_ESAs & SME_Time_Group==TG), 
                                        aes(x=SME_Rate, y=L3_Rate, col=as.factor(ESA_Sweep))) + 
                                  geom_point(alpha=0.25) +    
                                  geom_label(data=ann_text, aes(x=x, y=y, label=ENARate_L3), 
                                             inherit.aes = F, size=6.25) +
                                  geom_abline() +
                                  labs(x="Manual SME ENA Rate", y="LOTUS Stage 3 ENA Rate",
                                  title="LOTUS Stage 3 ENA Rates by Exposure Time and ESA") +
                                  scale_colour_manual(values=rep("steelblue4", 6)) + #c('lightskyblue', 'steelblue2', 'steelblue3', 
                                                             #  'steelblue', 'steelblue4')) +
                                  theme(legend.position="",
                                  axis.text.x=element_text(size=10), 
                                  axis.text.y=element_text(size=10),
                                  axis.title.x=element_text(size=12), 
                                  axis.title.y=element_text(size=12)) +
                                  ylim(0,1) + xlim(0,1) +
                                  geom_density2d(colour='ivory') +
                                  facet_grid(rows=vars(SME_Time_Group), #SME_Time_Group~ESA_Sweep 
                                             labeller = labeller(SME_Time_Group=Time_labs)) 
        if(TG==6){
                SME_v_L3_Density[[TG]] = SME_v_L3_Density[[TG]] + 
                                          guides(col=guide_legend(title="ESA", position="bottom", nrow=1, 
                                                                  override.aes = list(alpha = 1)), alpha='none') +
                                          theme(legend.position="")#"bottom")
        }
     }
     ExposureTime_Scatter = arrangeGrob(grobs=SME_v_L3_Density[1:6], 
                                        nrow=3, ncol=2, heights=c(0.8, 0.8, 1))
}
ggsave(plot=ExposureTime_Scatter, 
       filename=paste(FIG_path, "Fig5_AllOrbits_ENARates_ExposureTime_Scatter.jpg", sep=''), 
       width=12, height=8, units="in")

### Look at Summary Statistics 
# Compare SME ENA Rates to LOTUS Stage 1 ENA Rates
summary((LOTUS_dat$SME_Rate/LOTUS_dat$L1_Rate)[which(is.finite(LOTUS_dat$SME_Rate/LOTUS_dat$L1_Rate) & 
                                               LOTUS_dat$SME_Time_Group>1)])
# Compare SME ENA Rates to LOTUS Stage 3 ENA Rates 
summary((LOTUS_dat$SME_Rate/LOTUS_dat$L3_Rate)[which(is.finite(LOTUS_dat$SME_Rate/LOTUS_dat$L3_Rate) & 
                                               LOTUS_dat$SME_Time_Group>1)])
################################################################################################


################################################################################################
##### Make Maps 
################################################################################################

#####################################
# ISOC-Like Maps                    #
#####################################

### Load Data 
SME_dat = read.csv(file = paste(LOAD_path, 'ISOC_Data/ISOC_SME.csv', sep=''))
L1_dat = read.csv(file = paste(LOAD_path, 'ISOC_Data/ISOC_L1.csv', sep=''))
L3_dat = read.csv(file = paste(LOAD_path, 'ISOC_Data/ISOC_L3.csv', sep=''))

### Get Map Statistics 
{
# Create empty storage vectors 
ks_test_pvals = ks_test_stats = t_test_pvals = t_test_stats = matrix(0, nrow=(length(unique(L3_dat$time_gorup)))*5, ncol=4) 
ks_test_pvals = as.data.frame(ks_test_pvals)
ks_test_stats = as.data.frame(ks_test_stats)
t_test_pvals = as.data.frame(t_test_pvals)
t_test_stats = as.data.frame(t_test_stats)
# Create counters 
count=1
count.time = 1
count.esa = 1
# Get Statistics
for(time in unique(L1_dat$time_group)[-length(unique(L1_dat$time_group))]){
        SME_sub = subset(SME_dat, time_group==time)
        SME_sub = SME_sub[,c(1,2,3,6)]
        
        L3_sub = subset(L3_dat, time_group==time)
        L3_sub = L3_sub[,c(1,2,3,6)]
        
        L1_sub = subset(L1_dat, time_group==time)
        L1_sub = L1_sub[,c(1,2,3,6)]

        for(ESA in 2:6){
                SME_ESA_mean = subset(SME_sub, esa==ESA)[,c(1,2,3)]
                SME_ESA_mean = as.matrix(spread(SME_ESA_mean, lat, ena_flux_prop))[,-1]

                L3_ESA_mean = subset(L3_sub, esa==ESA)[,c(1,2,3)]
                L3_ESA_mean = as.matrix(spread(L3_ESA_mean, lat, ena_flux_prop))[,-1]

                L1_ESA_mean = subset(L1_sub, esa==ESA)[,c(1,2,3)]
                L1_ESA_mean = as.matrix(spread(L1_ESA_mean, lat, ena_flux_prop))[,-1]

                ### KS test - distributional 
                ks_test_pvals[count, ] <- data.frame(V1=time, 
                                                     V2=ESA, 
                                                     V3=ks.test(SME_ESA_mean, L3_ESA_mean, alternative='two.sided')$p.value, 
                                                     V4=ks.test(SME_ESA_mean, L1_ESA_mean, alternative='two.sided')$p.value)
                ks_test_stats[count, ] <- data.frame(V1=time, 
                                                     V2=ESA, 
                                                     V3=ks.test(SME_ESA_mean, L3_ESA_mean, alternative='two.sided')$statistic, 
                                                     V4=ks.test(SME_ESA_mean, L1_ESA_mean, alternative='two.sided')$statistic)
                
                if(length(which(is.na(SME_ESA_mean)))!=0){
                        SME_ESA_mean = SME_ESA_mean[-which(is.na(SME_ESA_mean))]
                }
                if(length(which(is.na(L3_ESA_mean)))!=0){
                        L3_ESA_mean = L3_ESA_mean[-which(is.na(L3_ESA_mean))]
                }
                if(length(which(is.na(L1_ESA_mean)))!=0){
                        L1_ESA_mean = L1_ESA_mean[-which(is.na(L1_ESA_mean))]
                }
                
                L3_tmp = ccf(ts(c(L3_ESA_mean)), ts(c(SME_ESA_mean)), lag.max=3, plot=F)
                SME_tmp = ccf(ts(c(SME_ESA_mean)), ts(c(SME_ESA_mean)), lag.max=3, plot=F)
                L1_tmp = ccf(ts(c(L1_ESA_mean)), ts(c(SME_ESA_mean)), lag.max=3, plot=F)
                
                t_test_pvals[count, ]  <- data.frame(V1=time,
                                                         V2=ESA,
                                                         V3=t.test(SME_tmp$acf, L3_tmp$acf, paired=TRUE, 'two.sided')$p.value,
                                                         V4=t.test(SME_tmp$acf, L1_tmp$acf, paired=TRUE, 'two.sided')$p.value)
                
                t_test_stats[count, ]  <- data.frame(V1=time,
                                                         V2=ESA,
                                                         V3=t.test(SME_tmp$acf, L3_tmp$acf, paired=TRUE, 'two.sided')$statistic,
                                                         V4=t.test(SME_tmp$acf, L1_tmp$acf, paired=TRUE, 'two.sided')$statistic)
                
                count = count + 1
                count.esa = count.esa+1
        }
        count.time = count.time + 1
}
# Set alpha level 
alpha=0.01
# Look at eCDF Tests - Do the Two Maps Come From the Same Distribution?
{
# Look at Statistics by ESA 
# Set a threshold
ks_threshold=round(max(abs(ks_test_stats$V3[ks_test_pvals$V3>=alpha])),4)
ks_by_ESA = sapply(2:6, function(x, ks_test_pvals, alpha) c(mean(ks_test_pvals$V3[ks_test_pvals$V2==x]>=alpha), 
                                                mean(ks_test_pvals$V4[ks_test_pvals$V2==x]>=alpha)), 
       ks_test_pvals, alpha)
print(ks_by_ESA)
# Look at Statistics by Map
ks_by_Map = sapply(unique(ks_test_pvals[,1]), function(x, ks_test_pvals, alpha) c(mean(ks_test_pvals$V3[ks_test_pvals$V1==x]>=alpha), 
                                                                      mean(ks_test_pvals$V4[ks_test_pvals$V1==x]>=alpha)), 
       ks_test_pvals, alpha)
print(ks_by_Map)
} 
# Look at CCF Tests - Do the Two Maps Vary Similarly?
{
t_threshold=round(max(abs(t_test_stats$V3[t_test_pvals$V3>=alpha])),4)
# Look at Statistics by ESA 
T_by_ESA=sapply(2:6, function(x, t_test_pvals, alpha) c(mean(t_test_pvals$V3[t_test_pvals$V2==x]>=alpha), 
                                                   mean(t_test_pvals$V4[t_test_pvals$V2==x]>=alpha)), 
       t_test_pvals, alpha)
print(T_by_ESA)
# Look at Statistics by Map
T_by_Map = sapply(unique(t_test_pvals[,1]), function(x, t_test_pvals, alpha) c(mean(t_test_pvals$V3[t_test_pvals$V1==x]>=alpha), 
                                                                            mean(t_test_pvals$V4[t_test_pvals$V1==x]>=alpha)), 
       t_test_pvals, alpha)
print(T_by_Map)
}
}

### Make Statistics Plots 
time="2019B"
# Figure 8: eCDF Plots (ISOC Maps)................................................................................................ Figure 8
{
     ccf_plot = ecdf_plot = list()
     
     SME_sub = subset(SME_dat, time_group==time)
     SME_sub = SME_sub[,c(1,2,3,6)]
     
     L3_sub = subset(L3_dat, time_group==time)
     L3_sub = L3_sub[,c(1,2,3,6)]
     
     L1_sub = subset(L1_dat, time_group==time)
     L1_sub = L1_sub[,c(1,2,3,6)]
     
     for(ESA in 2:6){
          SME_ESA_mean = subset(SME_sub, esa==ESA)[,c(1,2,3)]
          SME_ESA_mean = as.matrix(spread(SME_ESA_mean, lat, ena_flux_prop))[,-1]
          
          L3_ESA_mean = subset(L3_sub, esa==ESA)[,c(1,2,3)]
          L3_ESA_mean = as.matrix(spread(L3_ESA_mean, lat, ena_flux_prop))[,-1]
          
          L1_ESA_mean = subset(L1_sub, esa==ESA)[,c(1,2,3)]
          L1_ESA_mean = as.matrix(spread(L1_ESA_mean, lat, ena_flux_prop))[,-1]
          
          # Make eCDF plots
          tmp_ecdf_dat <- data.frame(SME=c(SME_ESA_mean),
                                     L3=c(L3_ESA_mean),
                                     L1=c(L1_ESA_mean))
          tmp_ecdf_dat = reshape2::melt(tmp_ecdf_dat)
          plot_df = data.frame(variable=tmp_ecdf_dat[,1], mean=tmp_ecdf_dat[,2])
          levels(plot_df$variable)=c("Manual SME Map", "LOTUS Stage 3 Map", "LOTUS Stage 1 Map")
          ecdf_plot[[ESA]] = ggplot(plot_df, aes(x = mean, col=variable, fill=variable)) +
               geom_ribbon(aes(x = mean,
                               ymin = ..y..-sqrt(log(2/0.01)/(2*16200)),
                               ymax = ..y..+sqrt(log(2/0.01)/(2*16200))),
                           stat = "ecdf",
                           alpha=0.2, linewidth=0.25) +
               geom_step(stat = "ecdf") +
               ylab("eCDF") + xlab("Map") + 
               ggtitle(paste("Comparison of Map eCDFs for ESA ", ESA, sep='')) +
               theme(legend.position="", legend.title=element_blank())
          if(ESA==6){
               ecdf_plot[[ESA]] = ecdf_plot[[ESA]] + 
                    theme(legend.position="bottom", legend.title=element_blank())
          }
          
          if(length(which(is.na(SME_ESA_mean)))!=0){
               SME_ESA_mean = SME_ESA_mean[-which(is.na(SME_ESA_mean))]
          }
          if(length(which(is.na(L3_ESA_mean)))!=0){
               L3_ESA_mean = L3_ESA_mean[-which(is.na(L3_ESA_mean))]
          }
          if(length(which(is.na(L1_ESA_mean)))!=0){
               L1_ESA_mean = L1_ESA_mean[-which(is.na(L1_ESA_mean))]
          }
          
          L3_tmp = ccf(ts(c(L3_ESA_mean)), ts(c(SME_ESA_mean)), lag.max=3, plot=F)
          SME_tmp = ccf(ts(c(SME_ESA_mean)), ts(c(SME_ESA_mean)), lag.max=3, plot=F)
          L1_tmp = ccf(ts(c(L1_ESA_mean)), ts(c(SME_ESA_mean)), lag.max=3, plot=F)
          
          tmp_df <- cbind(rbind(with(SME_tmp, data.frame(lag, acf)),
                                with(L1_tmp, data.frame(lag, acf)),
                                with(L3_tmp, data.frame(lag, acf))),
                          Type=rep(c("Manual SME Map", "RoboPaul", "Lotus"), each=7))
          tmp_df$Type = as.factor(tmp_df$Type)
          tmp_df$lag = tmp_df$lag + c(rep(0,7), rep(-0.1, 7), rep(0.1, 7))
          levels(tmp_df$Type)=c("LOTUS Stage 3", "Manual SME Map", "LOTUS Stage 1 Map")
          ccf_plot[[ESA]] <- ggplot(data = tmp_df, mapping = aes(x = lag, y = acf, group=Type, colour=Type)) +
               geom_hline(aes(yintercept = 0)) +
               geom_segment(mapping = aes(xend = lag, yend = 0)) +
               labs(title=paste('CCF: ', time, ' ESA', ESA, sep="")) +
               theme(legend.position="")
          if(ESA==6){
               ccf_plot[[ESA]] = ccf_plot[[ESA]]  +
                    theme(legend.position="bottom")
          }
     }
     grid.arrange(ecdf_plot[[2]], ecdf_plot[[3]], ecdf_plot[[4]], ecdf_plot[[5]], ecdf_plot[[6]],
                  ccf_plot[[2]], ccf_plot[[3]], ccf_plot[[4]], ccf_plot[[5]], ccf_plot[[6]], ncol=5, nrow=2)
     
     eCDF_other = grid.arrange(grobs=ecdf_plot[2:5], ncol=4)
}
ggsave(plot=eCDF_other, 
       filename=paste(FIG_path, "Fig8_ISOC_eCDF_ESA2through5_", time,".jpg", sep=''), 
       width=16, height=3, units="in")
ggsave(plot=ecdf_plot[[6]], 
       filename=paste(FIG_path, "Fig8_ISOC_eCDF_ESA6_", time,".jpg", sep=''), 
       width=5, height=4, units="in")

# Figure 9: Box Plots of eCDF (D) Test Statistic (ISOC Maps)...................................................................... Figure 9
{
     D_stats_dat = melt(ks_test_stats, id.vars=c(1,2), measure.vars=c(3,4))
     levels(D_stats_dat$variable) = c("LOTUS Stage 3", "LOTUS Stage 1")
     D_stats_plot = ggplot(D_stats_dat, aes(col=as.factor(V2), x=value, y=as.factor(variable))) + 
          geom_vline(xintercept=0.0541, linetype=2) +
          geom_boxplot() + 
          scale_colour_manual("ESA", values=c('lightskyblue', 'steelblue2', 'steelblue3', 'steelblue', 'steelblue4')) +
          labs(title="Values of D for LOTUS Stages 1 and 3") + 
          ylab("") + xlab("Value of Test Statistic") + 
          theme(legend.position="bottom", 
                axis.text.x = element_text(size=11),
                axis.text.y = element_text(size=11),
                axis.title.x = element_text(size=14), 
                axis.title.y = element_text(size=14)) 
}
ggsave(plot=D_stats_plot, 
       filename=paste(FIG_path, "Fig9_ISOC_DStat_Boxplots.jpg", sep=''), 
       width=5, height=3)

# Figure 10: CCF Plots (ISOC Maps)................................................................................................ Figure 10
CCF_other = grid.arrange(grobs=ccf_plot[2:5], ncol=4)
ggsave(plot=CCF_other, 
       filename=paste(FIG_path, "Fig10_ISOC_CCF_ESA2through5_", time,".jpg", sep=''), 
       width=16, height=3, units="in")
ggsave(plot=ccf_plot[[6]], 
       filename=paste(FIG_path, "FIG10_ISOC_CCF_ESA6_", time,".jpg", sep=''), 
       width=5, height=4, units="in")

### Make Maps
time='2019B'
# Figure 6: ISOC-Like Maps, ESA 6 (ISOC Maps)..................................................................................... Figure 6
{
SME_plot <- L3_plot <- L1_plot <- L3_diff_plot <- L1_diff_plot <- list()
SME_sub = subset(SME_dat, time_group==time)
L3_sub = subset(L3_dat, time_group==time)
L1_sub = subset(L1_dat, time_group==time)
for(ESA in 2:6){
        SME_ESA_tmp = subset(SME_sub, esa==ESA)[,c(1,2,3,4)]
        SME_ESA_mean = as.matrix(pivot_wider(SME_ESA_tmp[,-4], names_from=lon, values_from=ena_flux_prop))[,-1]
        SME_ESA_weight = as.matrix(pivot_wider(SME_ESA_tmp[,-3], names_from=lon, values_from=total_exposure_time))[,-1]
        
        L3_ESA_tmp = subset(L3_sub, esa==ESA)[,c(1,2,3,4)]
        L3_ESA_mean = as.matrix(pivot_wider(L3_ESA_tmp[,-4], names_from=lon, values_from=ena_flux_prop))[,-1]
        L3_ESA_weight = as.matrix(pivot_wider(L3_ESA_tmp[,-3], names_from=lon, values_from=total_exposure_time))[,-1]

        L1_ESA_tmp = subset(L1_sub, esa==ESA)[,c(1,2,3,4)]
        L1_ESA_mean = as.matrix(pivot_wider(L1_ESA_tmp[,-4], names_from=lon, values_from=ena_flux_prop))[,-1]
        L1_ESA_weight = as.matrix(pivot_wider(L1_ESA_tmp[,-3], names_from=lon, values_from=total_exposure_time))[,-1]

        SME_melt = cbind(reshape2::melt(SME_ESA_mean), weight=reshape2::melt(SME_ESA_weight)[,3])
        L3_melt = cbind(reshape2::melt(L3_ESA_mean), weight=reshape2::melt(L3_ESA_weight)[,3])
        L1_melt = cbind(reshape2::melt(L1_ESA_mean), weight=reshape2::melt(L1_ESA_weight)[,3])
        
        max.val = max(SME_melt[,3], L3_melt[,3], L1_melt[,3])
        max.val = ceiling(max.val*100)/100
        
        SME_plot[[ESA]] = ggplot(SME_melt, aes(x = Var2, y = Var1)) + 
                geom_tile(aes(fill=value)) + 
                scale_fill_gradientn("Flux", 
                                     colors = ibex_palette$hex,
                                     limits = c(0,max.val), guide="colourbar") +
                labs(x="Ecliptic Longitude", y="Ecliptic Latitude", title=paste("Manual SME Labelled Map, ESA ", ESA)) +
                theme(panel.spacing.x = unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                      legend.key.width = unit(2,"cm"), legend.position="") +
                theme(axis.text.x=element_text(size=14),
                      axis.text.y=element_text(size=14),
                      axis.title.x=element_text(size=16),
                      axis.title.y=element_text(size=16))
       
        L3_plot[[ESA]] = ggplot(L3_melt, aes(x = Var2, y = Var1)) +
                geom_tile(aes(fill=value)) +
                scale_fill_gradientn("Flux",
                                     colors = ibex_palette$hex,
                                     limits = c(0,max.val), guide="colourbar") +
                labs(x="Ecliptic Longitude", y="Ecliptic Latitude", title=paste("LOTUS Stage 3 Labelled Map, ESA ", ESA)) +
                theme(panel.spacing.x = unit(0.1, "lines"), panel.background = element_rect(fill = 'white'),
                      legend.key.width = unit(2,"cm"), legend.position="")+
                theme(axis.text.x=element_text(size=14),
                      axis.text.y=element_text(size=14),
                      axis.title.x=element_text(size=16),
                      axis.title.y=element_text(size=16))
        if(ESA!=6){
                L3_plot[[ESA]] = L3_plot[[ESA]] + theme(legend.position="bottom")
        }
        
        L1_plot[[ESA]] = ggplot(L1_melt, aes(x = Var2, y = Var1)) + 
                geom_tile(aes(fill=value)) + 
                scale_fill_gradientn("Flux", 
                                     colors = ibex_palette$hex,
                                     limits = c(0,max.val), guide="colourbar") +
                labs(x="Ecliptic Longitude", y="Ecliptic Latitude", title=paste("LOTUS Stage 1 Labelled Map, ESA ", ESA)) +
                theme(panel.spacing.x = unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                      legend.key.width = unit(2,"cm"), legend.position="bottom")+
                theme(axis.text.x=element_text(size=14),
                      axis.text.y=element_text(size=14),
                      axis.title.x=element_text(size=16),
                      axis.title.y=element_text(size=16))

        L3_tmp_melt = L3_melt
        L1_tmp_melt = L1_melt
        L3_tmp_melt[,3] = (SME_melt[,3]-L3_melt[,3])/SME_melt[,3] * 100
        L1_tmp_melt[,3] = (SME_melt[,3]-L1_melt[,3])/SME_melt[,3] * 100
        min_val = min(L3_tmp_melt[,3], L1_tmp_melt[,3])
        max_val = max(L3_tmp_melt[,3], L1_tmp_melt[,3])
        L3_tmp_melt = L3_tmp_melt[is.finite(L3_tmp_melt[,3]),]
        L1_tmp_melt = L1_tmp_melt[is.finite(L1_tmp_melt[,3]),]
        L3_tmp_melt[abs(L3_tmp_melt[,3])>100,3] = ifelse(L3_tmp_melt[abs(L3_tmp_melt[,3])>100,3]>100, 100, -100)
        L1_tmp_melt[abs(L1_tmp_melt[,3])>100,3] = ifelse(L1_tmp_melt[abs(L1_tmp_melt[,3])>100,3]>100, 100, -100)
        
        L3_diff_plot[[ESA]] = ggplot(L3_tmp_melt, aes(x = Var2, y = Var1)) + 
                geom_tile(aes(fill=value)) + #, alpha=sqrt(weight/max(weight)))) + 
                scale_fill_gradient2("Flux Pct Diff", 
                                     low="gold",
                                     mid="white",
                                     high='red',
                                     midpoint=0,
                                     limits = c(-100,100), guide="colourbar") +
                labs(x="Ecliptic Longitude", y="Ecliptic Latitude", title=paste("Difference Map: Manual SME and LOTUS Stage 3")) +
                theme(panel.spacing.x = unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                      legend.key.width = unit(2,"cm"), legend.position="")+
                theme(axis.text.x=element_text(size=14),
                      axis.text.y=element_text(size=14),
                      axis.title.x=element_text(size=16),
                      axis.title.y=element_text(size=16))
        if(ESA!=6){
                L3_diff_plot[[ESA]] = L3_diff_plot[[ESA]] + theme(legend.position="bottom")
        }
        
        L1_diff_plot[[ESA]] = ggplot(L1_tmp_melt, aes(x = Var2, y = Var1)) + 
                geom_tile(aes(fill=value)) + #, alpha=sqrt(weight/max(weight)))) + 
                scale_fill_gradient2("Flux Pct Diff", 
                                     low="gold",
                                     mid="white",
                                     high='red',
                                     midpoint=0,
                                     limits = c(-100, 100), guide="colourbar") +
                labs(x="Ecliptic Longitude", y="Ecliptic Latitude", title=paste("Difference Map: Manual SME and LOTUS Stage 1")) +
                theme(panel.spacing.x = unit(0.1, "lines"), panel.background = element_rect(fill = 'white'), 
                      legend.key.width = unit(2,"cm"), legend.position="bottom")+
                theme(axis.text.x=element_text(size=14),
                      axis.text.y=element_text(size=14),
                      axis.title.x=element_text(size=16),
                      axis.title.y=element_text(size=16))
}
}
ggsave(plot=SME_plot[[6]], filename=paste(FIG_path, "Fig6_ISOC_Maps_SME_ESA6_", time,".jpg", sep=''), 
       width=5, height=3, units="in")
L_maps = marrangeGrob(grobs=list(L3_plot[[6]], L1_plot[[6]], L3_diff_plot[[6]], L1_diff_plot[[6]]), 
                      nrow=2, ncol=2, 
                      heights=c(0.8, 1))
ggsave(plot=L_maps, filename=paste(FIG_path, "Fig6_ISOC_Maps_LOTUS_ESA6_", time,".jpg", sep=''), 
       width=12, height=8, units="in")

# Figure 7a: ISOC-Like Maps, Other ESAs, L3 (ISOC Maps)........................................................................... Figure 7a
Other_maps = marrangeGrob(grobs=list(SME_plot[[2]], L3_plot[[2]], L3_diff_plot[[2]],
                                     SME_plot[[3]], L3_plot[[3]], L3_diff_plot[[3]],
                                     SME_plot[[4]], L3_plot[[4]], L3_diff_plot[[4]],
                                     SME_plot[[5]], L3_plot[[5]], L3_diff_plot[[5]]), 
                          nrow=3, ncol=4, 
                          heights=c(0.8, 1, 1), top=F)
ggsave(plot=Other_maps, filename=paste(FIG_path, "Fig7a_ISOC_Maps_L3_", time,".jpg", sep=''), 
       width=20, height=10, units="in")

# Figure 7b: ISOC-Like Maps, Other ESAs, L1 (ISOC Maps)........................................................................... Figure 7b
Other_maps = marrangeGrob(grobs=list(SME_plot[[2]], L1_plot[[2]], L1_diff_plot[[2]],
                                     SME_plot[[3]], L1_plot[[3]], L1_diff_plot[[3]],
                                     SME_plot[[4]], L1_plot[[4]], L1_diff_plot[[4]],
                                     SME_plot[[5]], L1_plot[[5]], L1_diff_plot[[5]]), 
                          nrow=3, ncol=4, 
                          heights=c(0.8, 1, 1), top=F)
ggsave(plot=Other_maps, filename=paste(FIG_path, "Fig7b_ISOC_Maps_L1_", time,".jpg", sep=''), 
       width=20, height=10, units="in")

### Evaluate Maps Using Lins Correspondence
# Organize Data 
{
ISOC_WIDE = SME_dat[,c('lon','lat','time_group','esa'),]
ISOC_WIDE$SME = SME_dat$ena_flux_prop
ISOC_WIDE = merge(ISOC_WIDE, data.frame(L1_dat[,c('lon','lat','time_group','esa'),], 
                                        L1 = L1_dat$ena_flux_prop),
                  by = c('lon','lat','time_group','esa'), all.x = T, all.y = T)
gc()
ISOC_WIDE = merge(ISOC_WIDE, data.frame(L3_dat[,c('lon','lat','time_group','esa'),], 
                                        L3 = L3_dat$ena_flux_prop),
                  by = c('lon','lat','time_group','esa'), all.x = T, all.y = T)
gc()
THRESH = 0.001
ISOC_WIDE = ISOC_WIDE[ISOC_WIDE$SME > THRESH & ISOC_WIDE$L1 > THRESH & ISOC_WIDE$L3 > THRESH,]
# Get L3 and SME correlations
ISOC_WIDE = ISOC_WIDE %>% dplyr::group_by(time_group, esa) %>%
        dplyr::mutate(cor_spearman_L3 = cor(L3, SME, method = 'spearman', use = 'pairwise.complete.obs'),
                      cor_lins_L3 = as.numeric(unlist(epiR::epi.ccc(L3, SME)$rho.c[1])))
gc()
# Get L1 and SME correlations
ISOC_WIDE = ISOC_WIDE %>% dplyr::group_by(time_group, esa) %>%
        dplyr::mutate(cor_spearman_L1 = cor(L1, SME, method = 'spearman', use = 'pairwise.complete.obs'),
                      cor_lins_L1 = as.numeric(unlist(epiR::epi.ccc(L1, SME)$rho.c[1])))
gc()
ISOC_WIDE= ISOC_WIDE[!is.na(ISOC_WIDE$time_group),]
ISOC_WIDE= ISOC_WIDE[ISOC_WIDE$time_group != '2022A' & !is.na(ISOC_WIDE$time_group),]
}
# Figure 11: Lins Concordance Plot (ISOC Maps).................................................................................... Figure 11
{
plot_df  = rbind(data.frame(ISOC_WIDE[,c('esa','time_group')], cor_spearman = ISOC_WIDE$cor_spearman_L3, cor_lins = ISOC_WIDE$cor_lins_L3, 
                               Type = 'SME vs. LOTUS Stage 3'), 
                    data.frame(ISOC_WIDE[,c('esa','time_group')], cor_spearman = ISOC_WIDE$cor_spearman_L1, cor_lins = ISOC_WIDE$cor_lins_L1, 
                               Type = 'SME vs. LOTUS Stage 1'))        

Lins_plot = ggplot(plot_df[!duplicated(paste0(plot_df$Type ,'_', plot_df$time_group, '_', plot_df$esa)),])+
        geom_line(aes(x = factor(time_group), y = cor_lins, col=factor(esa), linetype = factor(Type), group = factor(Type)), linewidth = 1)+
        geom_point(aes(x=factor(time_group), y = cor_lins, col=factor(esa)),  alpha = 1)+
        xlab('Map')+
        ylab('Lins Concordance')+
        labs(title = 'Lins Concordances for All Maps')+
        facet_grid(paste0('ESA: ',esa)~.)+
        scale_colour_manual(values=c('lightskyblue', 'steelblue2', 'steelblue3', 'steelblue', 'steelblue4')) +
        guides(col=guide_legend(title="ESA", position="bottom", nrow=1), 
               linetype=guide_legend(title="method", position="bottom", nrow=1)) + 
        theme(legend.position="bottom", 
              axis.title.x=element_text(size=14),
              axis.title.y=element_text(size=14),
              axis.text.x=element_text(size=12, angle=45, hjust=1), 
              axis.text.y=element_text(size=12)) + 
        scale_linetype_manual(values=c(4,1)) +
        geom_point(data=subset(plot_df, time_group=="2019B"), col="red", aes(x=factor(time_group), y=cor_lins))
}
ggsave(plot=Lins_plot, filename=paste(FIG_path, "Fig11_ISOC_Lins_", time,".jpg", sep=''), 
       width=8, height=6, units="in")
################################################################################################