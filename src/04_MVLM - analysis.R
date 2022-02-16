## ________________________________________________________________________

## Title:    04_MVLM - analysis.R
## Purpose:  Analyse movement data in multivariate linear model (MVLM) framework
## Author:   Dominic Henry
## Date:     16/02/2022

## Libraries
library(tidyverse)
library(car)
## ________________________________________________________________________

# Import data -------------------------------------------------------------

## Decide on which subset of data to work with

## Remove birds tracked for less than 6 weeks & the BARSTR outlier
df <- read_csv("data output/MVLM/movement_metrics_all_birds.csv") %>%
  filter(!pttid  %in% c("7711701","7712201","7712301")) %>%
  filter(pttid != "7713303")

## Outlier removed
# df <- read_csv("data output/MVLM/movement_metrics.csv") %>%
#   filter(pttid != "7713303")

## All data
# df <- read_csv("data output/MVLM/movement_metrics_all_birds.csv")

## Remove birds tracked for less than 6 weeks
# df <- read_csv("data output/MVLM/movement_metrics_all_birds.csv") %>%
#   filter(!pttid  %in% c("7711701","7712201","7712301"))

## Scale data
df_sc <- df %>%
  mutate_at(vars(mean_dd:sd_relang), list(~scale(.) %>% as.vector))

# Plot data ---------------------------------------------------------------
box1 <- ggplot(df, aes(x = site, y = mean_dd)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "Mean daily distance") +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())

box2 <- ggplot(df, aes(x = site, y = sd_dd)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of distance moved") +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())

box3 <- ggplot (df, aes(x = site, y = mean_absang )) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "Mean absolute turning angle") +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())

box4 <- ggplot (df, aes(x = site, y = sd_meandisp)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of mean displacement") +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())

box5 <- ggplot (df, aes(x = site, y = sd_relang)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of relative turning angle") +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=12,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())

ggpubr::ggarrange (box1,box2,box3,box4,box5,ncol=2,nrow=3)
ggsave("data output/individualvars_boxplots_outlier_rm.pdf",
       width = 8, height = 10)
# ggsave("data output/individualvars_boxplots_all_birds.pdf", width = 8, height = 10)

# Run mvlm ----------------------------------------------------------------

mv_mod <- lm(cbind(mean_dd, sd_dd, mean_absang, sd_meandisp, sd_relang) ~ site, data = df_sc)
mv_mod

summary(mv_mod)

broom::tidy(mv_mod) %>%
  write_csv("data output/MVLM/mvlm_table_outlier_rm.csv")

# broom::tidy(mv_mod) %>%
#   write_csv("data output/MVLM/mvlm_table_allbirds.csv")

hist(resid(mv_mod)[,1]);hist(resid(mv_mod)[,2]);hist(resid(mv_mod)[,3])
fitted(mv_mod)
(manova_move <- Anova(mv_mod))
summary(manova_move)

linearHypothesis(mv_mod, "(Intercept) = siteBARSTR", verbose=TRUE)
linearHypothesis(mv_mod, "siteBARSTR = siteSTR", verbose=TRUE)
linearHypothesis(mv_mod, "siteBARSTR = siteVOE", verbose=TRUE)

linearHypothesis(mv_mod, "siteSTR = siteVOE", verbose=TRUE)
linearHypothesis(mv_mod, "siteBARSTR = siteJOZ", verbose=TRUE)
linearHypothesis(mv_mod, "siteBARSTR = siteMAN", verbose=TRUE)

linearHypothesis(mv_mod, "siteMAN = siteJOZ", verbose=TRUE)


linearHypothesis(mv_mod, "(Intercept) = siteSTR", verbose=TRUE)
linearHypothesis(mv_mod, "(Intercept) = siteVOE", verbose=TRUE)

linearHypothesis(mv_mod, "(Intercept) = siteMAN", verbose=TRUE)
linearHypothesis(mv_mod, "(Intercept) = siteJOZ", verbose=TRUE)

linearHypothesis(mv_mod, "siteMAN = siteSTR", verbose=TRUE)
linearHypothesis(mv_mod, "siteJOZ = siteSTR", verbose=TRUE)

linearHypothesis(mv_mod, "siteMAN = siteVOE", verbose=TRUE)
linearHypothesis(mv_mod, "siteJOZ = siteVOE", verbose=TRUE)


# ALL BIRDS: Plot data with regression results -------------------------------

df <- read_csv("data output/MVLM/movement_metrics_all_birds.csv") %>%
  filter(!pttid  %in% c("7711701","7712201","7712301"))

box1 <- ggplot(df, aes(x = site, y = mean_dd)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "Mean daily distance", limits = c(0, 9000)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())+
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,29)) == fval, italic(P) == p, italic(R^2) == rsqr),
                              list(fval=1.44, p=0.36, rsqr = 0.16)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)

box1

box2 <- ggplot(df, aes(x = site, y = sd_dd)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of distance moved", limits = c(0, 35000)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank()) +
  annotate(geom = 'text',
           label =substitute(list(italic(F(5,29)) == fval, italic(P) == p, italic(R^2) == rsqr),
                             list(fval=1.82, p=0.14, rsqr = 0.23)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)

box2

box3 <- ggplot (df, aes(x = site, y = mean_absang )) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "Mean absolute turning angle", limits = c(-0.7, 0.4)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank()) +
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,29)) == fval, italic(P) < p, italic(R^2) == rsqr),
                              list(fval=9.54, p=0.001, rsqr = 0.62)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)
box3

box4 <- ggplot (df, aes(x = site, y = sd_meandisp)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of mean displacement", limits = c(0, 2.5e+11)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank()) +
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,29)) == fval, italic(P) < p, italic(R^2) == rsqr),
                              list(fval=3.81, p=0.05, rsqr = 0.39)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)

box4

box5 <- ggplot (df, aes(x = site, y = sd_relang)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of relative turning angle", limits = c(1.9, 2.3)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=12,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())+
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,29)) == fval, italic(P) < p, italic(R^2) == rsqr),
                              list(fval=3.34, p=0.05, rsqr = 0.36)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)

box5

ggpubr::ggarrange (box1,box2,box3,box4,box5,ncol=2,nrow=3)
ggsave("data output/MVLM/individualvars_boxplots_all_birds_anova.pdf",
       width = 8, height = 10)

# OUTLIER RM: Plot data with regression results ---------------------------

df <- read_csv("data output/movement_metrics_all_birds.csv") %>%
  filter(!pttid  %in% c("7711701","7712201","7712301")) %>%
  filter(pttid != "7713303")

box1 <- ggplot(df, aes(x = site, y = mean_dd)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "Mean daily distance", limits = c(0, 4000)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())+
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,28)) == fval, italic(P) < p, italic(R^2) == rsqr),
                              list(fval=2.58, p=0.05, rsqr = 0.31)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)

box1

box2 <- ggplot(df, aes(x = site, y = sd_dd)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of distance moved", limits = c(0, 20000)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank()) +
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,28)) == fval, italic(P) < p, italic(R^2) == rsqr),
                              list(fval=4.81, p=0.05, rsqr = 0.46)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)

box2

box3 <- ggplot (df, aes(x = site, y = mean_absang )) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "Mean absolute turning angle", limits = c(-0.7, 0.4)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank()) +
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,28)) == fval, italic(P) < p, italic(R^2) == rsqr),
                              list(fval=14.42, p=0.001, rsqr = 0.72)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)
box3

box4 <- ggplot (df, aes(x = site, y = sd_meandisp)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of mean displacement", limits = c(0, 2.2e+11)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=10,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank()) +
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,28)) == fval, italic(P) < p, italic(R^2) == rsqr),
                              list(fval=6.76, p=0.001, rsqr = 0.54)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)

box4

box5 <- ggplot (df, aes(x = site, y = sd_relang)) +
  theme_bw() +
  geom_boxplot() + scale_y_continuous(name = "SD of relative turning angle", limits = c(1.9, 2.3)) +
  geom_dotplot(colour="black",aes(),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_x_discrete(name = "Moult location") +
  theme(axis.text.x=element_text(face="bold",size=14,colour="black",angle = -90, hjust = 0)) +
  theme(axis.text.y=element_text(face="bold",size=12,colour="black", hjust = 0)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16,face="bold")) +
  theme(axis.title.x=element_blank())+
  annotate(geom = 'text',
           label = substitute(list(italic(F(5,28)) == fval, italic(P) < p, italic(R^2) == rsqr),
                              list(fval=2.91, p=0.05, rsqr = 0.34)),
           x = -Inf, y = Inf, hjust = -0.03, vjust = 1.1)

box5

ggpubr::ggarrange (box1,box2,box3,box4,box5,ncol=2,nrow=3)
ggsave("data output/individualvars_boxplots_outlier_rm_anova.pdf",
       width = 8, height = 10)


# Appendix Correlation Scatterplot ----------------------------------------
library(GGally)

## Correlation matrix - all birds
df <- read_csv("data output/MVLM/movement_metrics_all_birds.csv") %>%
  filter(!pttid  %in% c("7711701","7712201","7712301")) %>%
  select(ndays,nfixes,nfix_day,
         mean_dd, sd_dd, mean_absang, sd_meandisp,sd_relang)

df

ggpairs(df, upper = list(continuous = wrap("cor", size = 5, colour="black", stars = FALSE))) +
  theme_bw(base_size = 12)

ggsave("data output/MVLM/correlation_matrix_all_birds.pdf", width = 16, height = 9)

## Correlation matrix - outlier removed

df <- read_csv("data output/MVLM/movement_metrics_all_birds.csv") %>%
  filter(!pttid  %in% c("7711701","7712201","7712301")) %>%
  filter(pttid != "7713303") %>%
  select(ndays,nfixes,nfix_day,
         mean_dd, sd_dd, mean_absang, sd_meandisp,sd_relang)

ggpairs(df, upper = list(continuous = wrap("cor", size = 5, colour="black", stars = FALSE))) +
  theme_bw(base_size = 12)

ggsave("data output/MVLM/correlation_matrix_outlier_rm.pdf",
       width = 16, height = 9)

## Correlation tests
cor(df)
cor_results <- Hmisc::rcorr(as.matrix(df))
cor_results

as.data.frame(cor_results$r) %>%
  bind_rows(as.data.frame(cor_results$P)) %>%
  write_csv("data output/MVLM/corr_results_r.csv")

# Bonferroni adjustment
cor_results$P[,1:3]
pval_bon <- p.adjust(cor_results$P[,1:3], method = "bonferroni")

cor_results$P[,1:3] <- pval_bon
cor_results$P[,1:3]

cor.test(df$nfix_day,df$mean_dd)
cor.test(df$nfix_day,df$sd_dd)

