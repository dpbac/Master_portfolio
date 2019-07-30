## READ IN DATA ####
source("scripts/lmem_analysis_cleaning.R")
source("scripts/lmem_analysis_statistics.R")

## LOAD PACKAGES ####
library(ggplot2)
library(RColorBrewer) # needed for some extra colours in one of the graphs

## LOAD NOT STANDARDIZED DATA

df_flow_not_standardized = read.csv("data/flow_data_expanded_2019-07-04.csv")

data_figs <- df_flow_not_standardized
# data_figs <- df_flow

data_figs <- data_figs %>%
  # changing the order of levels of var_mood
  mutate(var_mood = factor(var_mood, levels = c("low","high"))) %>%
  # changing the order of levels of var_tempo
  mutate(var_tempo = factor(var_tempo, levels = c("low","high")))

str(data_figs)

# Tests 

data_flow_figs = data_figs %>%
                  group_by(participant_id, var_mood, var_tempo, level_extreme) %>%
                  summarise(mean_flow = mean(flow_score)) %>%
                  ungroup()

data_flow_figs

## MAKE FIGURES ####
## SET COLORS FOR FIGURES ####
cols = brewer.pal(5, "RdBu")
col_con = cols[1]
col_incon = cols[5]

# Accuracy figure
flow_mean_box.plot = ggplot(data_flow_figs, aes(x = var_mood, y = mean_flow,
                                               fill = var_tempo)) +
            geom_boxplot() +
            ylim(0.2, 0.8) +
            geom_hline(yintercept = 0.5) + 
            facet_grid(~level_extreme) +
            scale_fill_manual(values = c(col_con, col_incon))+
            # Add a title
            ggtitle("Distribution of flow score considering variation\n on mood, tempo, and different levels of extremness") +
            # Customize the x-axis
            xlab("Variation on Mood (var_mood)") +
            # Customize the y-axis
           ylab("Flow score") +
          # Additional paramaters for displaying plot
          theme(text=element_text(size=14), title=element_text(size=14)) +
          labs(fill = "Variation on Tempo\n (var_tempo)")

pdf("figures/flow_avg_boxplot.pdf")
flow_mean_box.plot
dev.off()

histogram.plot = ggplot(df_flow, aes(x = flow_score, fill = var_tempo)) +
                  geom_histogram(bins = 30, position = "dodge") +
                  facet_grid(level_extreme ~ var_mood) +
                  scale_fill_manual(values = c(col_con, col_incon))


histogram.plot

# Flow boxplot
flow_boxplot.plot = ggplot(df_flow, aes(x = var_mood, y = flow_score, fill = var_tempo)) +
                    geom_boxplot() +
                    facet_grid(~level_extreme) +
                    scale_fill_manual(values = c(col_con, col_incon))


flow_boxplot.plot




## summarize df_flow by playlists (proxy for variation in mood, tempo and also extremeness level)

data_figs_sum = data_figs %>%
  group_by(playlist) %>%
  summarise(mean_flow_score = mean(flow_score) ) %>%
  ungroup()

data_figs_sum

## MAKE FIGURES ####

flow_score_mean.plot = ggplot(data_figs_sum, aes(x = playlist, y = mean_flow_score)) +
  geom_bar(stat = "identity") +
  ylim(-1, 1) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

pdf("figures/flow_score_mean.pdf")
flow_score_mean.plot
dev.off()

# For some reason I'm having problems to re-order the levels to better visualize. I'll build the plot in python.

## MAKE FIGURES OF STATISTICS ANALYSIS ####

# Compares residuals to the fitted items.  The format of the scatter
# plot suggests that the assumption of homoscedasticity is respected.

residuals.plot = plot(fitted(m_h2_companionship),resid(m_h2_companionship,type="pearson"), xlab = "fitted items", ylab = "residuals") +# this will create the plot
abline(0,0, col="red")


# Histogram of residuals

x <- df$.resid
h<-hist(x, breaks=10, col="lightblue", xlab="residuals - Final model",
        main="Histogram with Normal Curve", ylim=c(0,200))
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2) 


# QQ-plot to check for normality of residuals

qqnorm(resid(m_h2_companionship)) +
qqline(resid(m_h2_companionship), col= "red") # add a perfect fit line

# Between quantiles -1 and 1 is fine but out of it not which is also observed in the histogram.

# QQ-plot for random effects
qqnorm(ranef(m_h2_companionship)$participant_id[,1] ) +
qqline(ranef(m_h2_companionship)$participant_id[,1], col= "red")


# caterpillar plots for the random-effect terms

re1 <- ranef(m_h2_companionship, condVar=TRUE, whichel = 'participant_id')

print(re1)
# plot(re1)
dotplot(re1)

