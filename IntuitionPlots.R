#### Preliminaries ----
library(dplyr)
library(here)
library(ggplot2)
library(MASS)
library(reshape2)
library(latex2exp)


source(here("fte_theme.R"))
source(here("rbind.all.columns.R"))


##### Intuition plots showing individual draws -----

numPeriods <- 3
N <- 10^6 #number of observations
N_individual_plot <- 100 #number of obs to show on plot
mu_treatment <- c(-1,0,1) * 6 /2 #mean for treated group by time
mu_control <- c(0,0,0) #mean for control group by time
sigma <- 1 #pop SE for pre-trend

#Sigma is the variance matrix for the group-level means in each of 3 time periods
# We set each variance to 1/4 sigma, so that the variance of the DiD (difference of 4 group-level means)
# is precisely sigma
Sigma <- diag(numPeriods) * sigma^2 / 2^2 
thresholdT <- 1.96

sigmaPrePeriodCoef <- sigma #prePeriod SE is just sigma

set.seed(1)

#Draw the group-level means for treated and control groups for all three time periods
Ymat_Treatment <- MASS::mvrnorm(n = N, mu = mu_treatment, Sigma = Sigma)
Ymat_Control <- MASS::mvrnorm(n = N, mu = mu_control, Sigma = Sigma)

#Take differences between treatment and control in each period
DifMat <- Ymat_Treatment - Ymat_Control

#Post-treatment diff in diff is between last period and 2nd period
DifInDif <- DifMat[,3] - DifMat[,2]

#Calculate which pre-treatment diffs are significant (using pop SE)
significantPretrend <- abs( DifMat[,2] - DifMat[,1] ) > thresholdT * sigmaPrePeriodCoef

#Put the diffences by time period into a DF
df <- data.frame(dif = DifMat, significantPretrend = significantPretrend ) %>%
        mutate(id = row_number()) %>%
        melt(., id = c("id", "significantPretrend")) %>%
        mutate(t = case_when( variable == "dif.1" ~ -1,
                              variable == "dif.2" ~ 0,
                              variable == "dif.3" ~ 1)) %>%
  rename(dif = value)

#Create a DF with the population means of the diffs
populationMeansDF <- data.frame(t = c(-1,0,1),
                                dif = mu_treatment - mu_control)

#Plot the population means
ggplot(populationMeansDF,
       aes(x = t,
           y = dif,
           color = "Population Means") ) +
  geom_line() + geom_point(alpha = 1) +
   scale_y_continuous(breaks = c(-3,0,3), limits = c(-5,5)) + fte_theme() +
  labs(color = "", shape = "") +
  scale_x_continuous(breaks = c(-1,0,1), minor_breaks = NULL) +
  scale_color_manual(values = "black") +
  ggtitle("Difference Between Treatment and Control By Period") +
  ylab(TeX("$\\Delta \\bar{Y}_t")) +
  theme(axis.title.y=element_text(angle=0,vjust =0.5)) +
  theme(legend.position = c(.2,.83),
        legend.background = element_rect(fill = "transparent"))

ggsave(here("Figures/PopulationMeans.png"),
         width = 6, height = 3)

df <- df %>%   mutate(`Significant Pretrend` = ifelse(significantPretrend, "Significant Pre-trend", "Insignificant Pre-trend") )

#For the plot, restrict the # of observations
DFForPlot <- 
df %>% 
  filter(id <= N_individual_plot) %>%
  arrange(-significantPretrend)

#Plot the diffs without highlighting
ggplot(data = rbind.all.columns(DFForPlot %>% mutate(Data = "Draws of Data"),
                    populationMeansDF %>% mutate(Data = "Population Means") ) %>%
         mutate(alpha = ifelse(significantPretrend,.5,.5)),
       aes(x = t,
           y = dif,
           group = id,
           alpha =alpha,
           color = Data,
           shape = Data)) +
  guides(alpha = F) +
  geom_line() + geom_point(alpha = 1) +
  scale_color_manual( values = c(brewer.pal(5, "Greys")[3], "black")
                      #brewer.pal(3, "Set1")[1])
  ) +
   scale_y_continuous(breaks = c(-3,0,3), limits = c(-5,5)) + fte_theme() +
  labs(color = "", shape = "") +
  scale_x_continuous(breaks = c(-1,0,1), minor_breaks = NULL) +
  theme(legend.position = c(.2,.8),
        legend.background = element_rect(fill = "transparent")) +
  ggtitle("Simulated Draws") +
  ylab(TeX("$\\Delta \\bar{Y}_t")) +
  theme(axis.title.y=element_text(angle=0,vjust =0.5))

  ggsave(here("Figures/DataDraws_Unhighlighted.png"),
         width = 6, height = 3)

combinedDF <- 
  rbind.all.columns(
    DFForPlot %>%
      mutate(alpha = ifelse(significantPretrend,.95,10)) %>%
      mutate(Data = `Significant Pretrend`),
    populationMeansDF %>% mutate(Data = "Population Means")
  ) %>% mutate( Data = factor(.$Data, levels = c("Insignificant Pre-trend", "Significant Pre-trend", "Population Means"))
  )              

#Plot the diffs with highlighting
ggplot(data =
         combinedDF,   
       aes(x = t,
           y = dif,
           group = id,
           color = Data,
           shape = Data,
           alpha =alpha)) +
  guides(alpha = F) +
  geom_line() + geom_point(alpha = 0.5) +
  scale_color_manual( values = c(brewer.pal(3, "Set1")[2],
                                 brewer.pal(3, "Greys")[3],
                                 #brewer.pal(6, "YlOrRd")[2],
                                 "black")
                      #brewer.pal(3, "Set1")[1])
  ) +
   scale_y_continuous(breaks = c(-3,0,3), limits = c(-5,5)) + fte_theme() +
  labs(color = "", shape = "") +
  scale_x_continuous(breaks = c(-1,0,1), minor_breaks = NULL) +
  theme(legend.position = c(.27,.8),
        #panel.background = element_rect(fill = "white", color = "white"),
        legend.background = element_rect(fill = "transparent")) +
  #legend.background = element_rect(fill = theme_get()$panel.background$fill)) +
  #ggtitle("Difference Between Treatment and Control") +
  ggtitle("Simulated Draws") +
  ylab(TeX("$\\Delta \\bar{Y}_t")) +
  theme(axis.title.y=element_text(angle=0,vjust =0.5))

  ggsave(here("Figures/DataDraws_Highlighted.png"),
         width = 6, height =3)


  #Summarize the diffs by significant pre-trend or not
combinedSummaryDF <-bind_rows( 
                    df %>% 
                      group_by(t, `Significant Pretrend`) %>%
                      summarise(dif = mean(dif)) %>%
                      mutate(Data = `Significant Pretrend`),
                    populationMeansDF %>% mutate(Data = "Population Means",
                                                 `Significant Pretrend` = NA) )
                    
#Plot the summary
p <- ggplot(data = combinedSummaryDF %>% filter(Data != "Significant Pre-trend") ,
       aes(x = t,
           y = dif,
           color = Data,
           shape = Data)) +
  geom_line(alpha = 1) + geom_point() +
  scale_color_manual( values = c(brewer.pal(3, "Set1")[2],
                                 #brewer.pal(3, "Greys")[3],
                                 "black") ) +
  scale_y_continuous(breaks = c(-3,0,3), limits = c(-5,5)) +
  fte_theme() +
  theme(legend.position =  c(.27,.9),
        legend.background = element_rect(fill = "transparent")) +
  ylab(TeX("$\\Delta \\bar{Y}_t")) +
  theme(axis.title.y=element_text(angle=0,vjust =0.5)) +
  labs(color = "", shape = "") +
  scale_x_continuous(breaks = c(-1,0,1), minor_breaks = NULL) +
  ggtitle("Average Over 1 Million Draws")

  ggsave(here("Figures/PopulationMeans_PopulationAndInsignificant.png"),
         width = 6, height = 3)

lookUPSummaryDF <- function(Data, t, variable = "dif"){
 value <-  combinedSummaryDF[which(combinedSummaryDF$Data == Data &
                    #combinedSummaryDF$Data == Data  &
                    combinedSummaryDF$t == t ), variable] 

 return(value)
}

difPreConditional <-  lookUPSummaryDF("Insignificant Pre-trend", 0) - lookUPSummaryDF("Insignificant Pre-trend", -1)
difPreConditional <- as.double( round(difPreConditional, digits = 1)  )
  
difPostConditional <-  lookUPSummaryDF("Insignificant Pre-trend", 1) - lookUPSummaryDF("Insignificant Pre-trend", 0)
difPostConditional <- as.double( round(difPostConditional, digits = 1)  )

p + annotate("text", x = -0.5, y = 0, label = paste0("Pre-period DiD: ", diff(mu_treatment-mu_control)[1])) +
  annotate("text", x = -0.25, y = -5/2, label = paste0("Pre-period DiD: ", sprintf("%.1f", difPreConditional)),
           color = brewer.pal(3, "Set1")[2]) +
  annotate("text", x = .5, y = 6.5/2, label = paste0("Post-period DiD: ", diff(mu_treatment-mu_control)[2])) +
  annotate("text", x = .6, y = -1.5/2, label = paste0("Post-period DiD: ", sprintf("%.1f", difPostConditional)),
           color = brewer.pal(3, "Set1")[2])

  ggsave(here("Figures/PopulationMeans_PopulationAndInsignificant_Annotated.png"),
         width = 6, height = 3)


ggplot(
  data = data.frame(DifInDif = DifInDif,
                    significantPretrend = significantPretrend ),
  aes(x = DifInDif, fill = NULL)
) + stat_density(geom = "line", aes(color = "Unconditional")) +
  stat_density( data = . %>% filter(!significantPretrend), 
                geom = "line",
               aes( color = "Insignificant Pre-trend")) +
  # stat_density( data = . %>% filter(significantPretrend), 
  #               geom = "line",
  #               aes( color = "Significant Pre-trend")) +
  fte_theme() +
  scale_color_manual( values = c(brewer.pal(3, "Set1")[2],
                                 #brewer.pal(3, "Greys")[3],
                                 "black") ) +
  xlab("Difference-in-Difference Coefficient") +
  theme(legend.position = c(.17,.8),
        legend.background = element_rect(fill = "transparent")) +
  labs(color = "") + ylab("Density") +
  ggtitle(TeX("Distribution of   $\\hat{\\beta}_{1}$"))

  ggsave(here("Figures/Distributions_PopulationAndInsiginificant.png"),
         width = 6, height = 3)



#Plot the diff at period 0
ggplot(
  data = df %>% filter(t ==0),
  aes(x = dif, fill = NULL)
) + stat_density(geom = "line", aes(color = "Population Distribution")) +
  stat_density( data = . %>% filter(!significantPretrend), 
                geom = "line",
                aes( color = "Insignificant Pre-trend")) +
  # stat_density( data = . %>% filter(significantPretrend), 
  #               geom = "line",
  #               aes( color = "Significant Pre-trend")) +
  fte_theme() +
  scale_color_manual( values = c(brewer.pal(3, "Set1")[2],
                                 #brewer.pal(3, "Greys")[3],
                                 "black") ) +
  xlab(TeX("$\\Delta \\bar{Y}_0$")) +
  theme(legend.position = c(.17,.8),
        legend.background = element_rect(fill = "transparent")) +
  labs(color = "") + ylab("Density") +
  ggtitle(TeX("Distribution of $\\Delta \\bar{Y}_0$"))

  ggsave(here("Figures/DistributionDeltaY0_PopulationAndInsiginificant.png"),
         width = 6, height = 3)



