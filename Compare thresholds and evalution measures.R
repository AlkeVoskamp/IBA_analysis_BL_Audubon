library(ggplot2)

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/")
eval_data <- read.csv("SDM_evaluation_table.csv")
head(eval_data)
plot(MaxKappa_thres_GAM~MaxTSS_thres_GAM,data=eval_data)
plot(MaxKappa_thres_GLM~MaxTSS_thres_GLM,data=eval_data)
plot(MaxKappa_thres_GBM~MaxTSS_thres_GBM,data=eval_data)
plot(MaxKappa_thres_RF~MaxTSS_thres_RF,data=eval_data)

summary(lm(MaxTSS_thres_GAM~MaxKappa_thres_GAM, eval_data))$adj.r.squared 
PGAM <- ggplot(data = eval_data) + 
        geom_point(aes(x = MaxTSS_thres_GAM, y = MaxKappa_thres_GAM), size = 1, colour = "black") +
        stat_smooth(aes(x = MaxTSS_thres_GAM, y = MaxKappa_thres_GAM), method = "lm",
              formula = y ~ poly(x, 1), se = FALSE, colour = "red")+
        annotate("text", x=0.75, y=0.25, label= "R-squared = 0.297") 
plot(PGAM)
  
summary(lm(MaxTSS_thres_GLM~MaxKappa_thres_GLM, eval_data))$adj.r.squared 
PGLM <- ggplot(data = eval_data) + 
  geom_point(aes(x = MaxTSS_thres_GLM, y = MaxKappa_thres_GLM), size = 1, colour = "black") +
  stat_smooth(aes(x = MaxTSS_thres_GLM, y = MaxKappa_thres_GLM), method = "lm",
              formula = y ~ poly(x, 1), se = FALSE, colour = "red")+
  annotate("text", x=0.75, y=0.25, label= "R-squared = 0.648") 
plot(PGLM)

summary(lm(MaxTSS_thres_GBM~MaxKappa_thres_GBM, eval_data))$adj.r.squared 
PGBM <- ggplot(data = eval_data) + 
  geom_point(aes(x = MaxTSS_thres_GBM, y = MaxKappa_thres_GBM), size = 1, colour = "black") +
  stat_smooth(aes(x = MaxTSS_thres_GBM, y = MaxKappa_thres_GBM), method = "lm",
              formula = y ~ poly(x, 1), se = FALSE, colour = "red")+
  annotate("text", x=0.75, y=0.25, label= "R-squared = 0.402") 
plot(PGBM)

summary(lm(MaxTSS_thres_RF~MaxKappa_thres_RF, eval_data))$adj.r.squared 
PRF <- ggplot(data = eval_data) + 
  geom_point(aes(x = MaxTSS_thres_RF, y = MaxKappa_thres_RF), size = 1, colour = "black") +
  stat_smooth(aes(x = MaxTSS_thres_RF, y = MaxKappa_thres_RF), method = "lm",
              formula = y ~ poly(x, 1), se = FALSE, colour = "red")+
  annotate("text", x=0.75, y=0.25, label= "R-squared = 0.568") 
plot(PRF)



summary(lm(AUC_GAM~Boyce_GAM, eval_data))$adj.r.squared 
AUCGAM <- ggplot(data = eval_data) + 
  geom_point(aes(x = AUC_GAM, y = Boyce_GAM), size = 1, colour = "black") +
  stat_smooth(aes(x = AUC_GAM, y = Boyce_GAM), method = "lm",
              formula = y ~ poly(x, 1), se = FALSE, colour = "red")+
  annotate("text", x=0.75, y=0.25, label= "R-squared = 0.054") 
plot(AUCGAM)

summary(lm(Boyce_GLM~AUC_GLM, eval_data))$adj.r.squared 
AUCGLM <- ggplot(data = eval_data) + 
  geom_point(aes(x = AUC_GLM, y = Boyce_GLM), size = 1, colour = "black") +
  stat_smooth(aes(x = AUC_GLM, y = Boyce_GLM), method = "lm",
              formula = y ~ poly(x, 1), se = FALSE, colour = "red")+
  annotate("text", x=0.75, y=0.25, label= "R-squared = 0.054") 
plot(AUCGLM)

