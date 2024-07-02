
library('tidyverse')
library('scales')
library('ggsignif')
library(ggplot2)
library(ggpubr)
library('likert')
library(afex)
library(stringr)
library(emmeans)
library(car)
library(lmerTest)
library(coin)

two_color = c('#04bcc4',
              '#fc746c')

tukey_detect<-function(dv=df_screentime$value,Tukey_crit=1.5){
  IQR=IQR(dv,na.rm = TRUE)
  Quant_25=quantile(dv,probs=0.25,na.rm = TRUE)
  Quant_75=quantile(dv,probs=0.75,na.rm = TRUE)
  upper=Quant_75+Tukey_crit*IQR
  lower=Quant_25-Tukey_crit*IQR
  outlier_Tukey=ifelse(dv>upper,1,ifelse(dv<lower,1,0))
  as.numeric(paste(outlier_Tukey))
}

df <- read.csv('./questionnaire_answers_r_formatted.csv', header=TRUE)

df_attrak_mean <- read.csv('./attrak_mean.csv', header=TRUE)
df_attrak_full <- read.csv('./attrak_diff_full.csv', header=TRUE)

colnames(df)
names(df)[names(df) == "ï..ID"] <- "ID"
names(df_attrak_mean)[names(df_attrak_mean) == "ï..Question"] <- "Question"
names(df_attrak_full)[names(df_attrak_full) == "ï..ID"] <- "ID"



# demographic info
N = length(df$ID)

age_mean <- mean(df$Age)
sd_age <- sd(df$Age)
ggdensity(df$Age)

# gender split
female_count = sum(df$Gender == 2)
female_perc = 100*female_count/N
male_count = N-female_count
male_perc = 100-female_perc


df$Heat <- factor(df$Heat)
df$Order <- factor(df$Order)
df$Topic <- factor(df$Topic)

#############
## Pre presentation confidence
#############
mean(df$PrePresentation[df$Order == 1])
mean(df$PrePresentation[df$Order == 2])
mean(df$PrePresentation[df$Heat == 0])
mean(df$PrePresentation[df$Heat == 1])
mean(df$PrePresentation[df$Topic == 1])
mean(df$PrePresentation[df$Topic == 2])

prepresentation_box <- ggplot(df, aes(x = Heat, y = PrePresentation, fill=Heat)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Thermal Feedback") + 
  scale_y_continuous(name = "Response") +
  theme_classic() +
  ggtitle("Pre-presentation Confidence") +
  theme(text = element_text(size=22), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
prepresentation_box


qqPlot(df$PrePresentation)
shapiro.test(df$PrePresentation)
t.test(df$PrePresentation[df$Heat == 0], df$PrePresentation[df$Heat == 1], paired=TRUE)
t.test(df$PrePresentation[df$Order == 1], df$PrePresentation[df$Order == 2], paired=TRUE)
wilcox.test(df$PrePresentation[df$Heat == 0], df$PrePresentation[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")
wilcoxsign_test(df$PrePresentation[df$Heat == 0]~df$PrePresentation[df$Heat == 1], distribution="exact")
effect_size_stress = 1.2115/sqrt(N)
effect_size_stress

wilcox.test(df$PrePresentation[df$Order == 1], df$PrePresentation[df$Order == 2], paired=TRUE, p.adjust.method = "bonf")
wilcox.test(df$PrePresentation[df$Topic == 1], df$PrePresentation[df$Topic == 2], paired=TRUE, p.adjust.method = "bonf")
wilcoxsign_test(df$PrePresentation[df$Topic == 1]~df$PrePresentation[df$Topic == 2], distribution="exact")
effect_size_stress = 1.1368/sqrt(N)
effect_size_stress
# aov_rm_pre <- df %>% aov_car(PrePresentation~Heat + Error(ID / Heat*Order),
#                          data=., include_aov= TRUE)
# aov_rm_pre

# no significant change in condifence based on heat or order



#############
## NASA TLX
#############
df$NASATLX_SUM <- df$NASATLX_1 + df$NASATLX_2 + df$NASATLX_3 + df$NASATLX_4 + df$NASATLX_5 + df$NASATLX_6

nasatlx_box <- ggplot(df, aes(x = Heat, y = NASATLX_SUM, fill=Heat)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Thermal Feedback", labels=c("Off","On")) + 
  scale_y_continuous(name = "Total Score") +
  scale_fill_manual(values= two_color) +
  theme_classic() +
  ggtitle("NASA-TLX") +
  theme(text = element_text(size=25), legend.position="off", plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
nasatlx_box

ggsave( "test_nasa.pdf", width=9, height=6)


qqPlot(df$NASATLX_SUM)
shapiro.test(df$NASATLX_SUM)
t.test(df$NASATLX_SUM[df$Heat == 0], df$NASATLX_SUM[df$Heat == 1], paired=TRUE)
wilcox.test(df$NASATLX_SUM[df$Heat == 0], df$NASATLX_SUM[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")
# aov_rm_nasa <- df %>% aov_car(NASATLX_SUM~Order +Error(ID / Heat ),
#                              data=., include_aov= TRUE)
# aov_rm_nasa
# no significant increase in workload, yay!


#############
## Presentation Quality
#############
presentation_questions <- c("How satisfied are you with the presentation?",
  "How engaging was the presentation?",
  "How nervous were you during the presentation?",
  "What is the overall quality of the presentation?",
  "How much of a personal connection did you feel with the audience?",
  "How aware were you of (non-verbal) feedback from your audience?",
  "How easy was it to respond to the (non-verbal) feedback from the audience?")
  
df_pq <- data.frame("Pres_Qual" = df$Presentation_Quality_1 + df$Presentation_Quality_2 +
  df$Presentation_Quality_3 + df$Presentation_Quality_4 +
  df$Presentation_Quality_5 + df$Presentation_Quality_6 +
  df$Presentation_Quality_7)

df_pq$Presentation_Quality_1 <- factor(df$Presentation_Quality_1, levels = seq(1,7))
df_pq$Presentation_Quality_2 <- factor(df$Presentation_Quality_2, levels = seq(1,7))
df_pq$Presentation_Quality_3 <- factor(df$Presentation_Quality_3, levels = seq(1,7))
df_pq$Presentation_Quality_4 <- factor(df$Presentation_Quality_4, levels = seq(1,7))
df_pq$Presentation_Quality_5 <- factor(df$Presentation_Quality_5, levels = seq(1,7))
df_pq$Presentation_Quality_6 <- factor(df$Presentation_Quality_6, levels = seq(1,7))
df_pq$Presentation_Quality_7 <- factor(df$Presentation_Quality_7, levels = seq(1,7))

firstcol_pres_qual = which(colnames(df_pq)=="Presentation_Quality_1")
lastcol_pres_qual = which(colnames(df_pq)=="Presentation_Quality_7")
names(df_pq)[c(firstcol_pres_qual:lastcol_pres_qual)] <- presentation_questions
likert_pres_qual = likert(df_pq[c(firstcol_pres_qual:lastcol_pres_qual)], grouping = df$Heat)
likert_pres_qual
plot(likert_pres_qual) + theme(plot.title = element_text(hjust=0.5)) + ggtitle("Perceived Presentation Quality")

df$Pres_Qual <- df$Presentation_Quality_1 + df$Presentation_Quality_2 +
                      df$Presentation_Quality_3 + df$Presentation_Quality_4 +
                      df$Presentation_Quality_5 + df$Presentation_Quality_6 +
                      df$Presentation_Quality_7

pres_qual_box <- ggplot(df, aes(x = Heat, y = Pres_Qual, fill=Heat)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Thermal Feedback") + 
  scale_y_continuous(name = "Total Score") +
  theme_classic() +
  ggtitle("Perceived Presentation Quality") +
  theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
pres_qual_box


qqPlot(df$Pres_Qual)
shapiro.test(df$Pres_Qual)
t.test(df$Pres_Qual[df$Heat == 0], df$Pres_Qual[df$Heat == 1], paired=TRUE)
t.test(df$Pres_Qual[df$Order == 1], df$Pres_Qual[df$Order == 2], paired=TRUE)
wilcox.test(df$Pres_Qual[df$Heat == 0], df$Pres_Qual[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")
wilcox.test(df$Pres_Qual[df$Order == 1], df$Pres_Qual[df$Order == 2], paired=TRUE, p.adjust.method = "bonf")
# aov_rm <- df %>% aov_car(NASATLX_SUM~Heat +Error(ID / Heat ),
#                          data=., include_aov= TRUE)
# aov_rm

df$PresConnection <- df$Presentation_Quality_5 + df$Presentation_Quality_6
wilcox.test(df$PresConnection[df$Heat == 0], df$PresConnection[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")
wilcox.test(df$Presentation_Quality_7[df$Heat == 0], df$Presentation_Quality_7[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")

#############
## Perceived Stress
#############


df_stress <- data.frame("Stress" = df$Presentation_Quality_8)

df_stress$Stress <- factor(df_stress$Stress, levels = seq(1,7))



likert_pres_qual = likert(df_stress, grouping=df$Heat)
likert_pres_qual
plot(likert_pres_qual) + theme(plot.title = element_text(hjust=0.5)) + ggtitle("Perceived Stress")


stress_box <- ggplot(df, aes(x = Heat, y = Presentation_Quality_8, fill=Heat)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Thermal Feedback", labels=c("Off","On")) +
  scale_y_continuous(name = "Response") +
  scale_fill_manual(values= two_color) +
  theme_classic() +
  ggtitle("Perceived Stress") +
  theme(text = element_text(size=25), legend.position="none", plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
stress_box

ggsave( "test_stress.pdf", width=9, height=6)

qqPlot(df$Presentation_Quality_8)
shapiro.test(df$Presentation_Quality_8)
t.test(df$Presentation_Quality_8[df$Heat == 0], df$Presentation_Quality_8[df$Heat == 1], paired=TRUE)
t.test(df$Presentation_Quality_8[df$Order == 1], df$Presentation_Quality_8[df$Order == 2], paired=TRUE)
wilcox.test(df$Presentation_Quality_8[df$Heat == 0], df$Presentation_Quality_8[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")
wilcoxsign_test(df$Presentation_Quality_8[df$Heat == 0]~df$Presentation_Quality_8[df$Heat == 1], distribution="exact")
effect_size_stress = 0.50441/sqrt(N)
effect_size_stress
# aov_rm <- df %>% aov_car(NASATLX_SUM~Heat +Error(ID / Heat ),
#                          data=., include_aov= TRUE)
# aov_rm


#############
## Donation
#############


donation_box <- ggplot(df, aes(x = Heat, y = Donations, fill=Heat)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Thermal Feedback") + 
  scale_y_continuous(name = "Response") +
  theme_classic() +
  ggtitle("Donation") +
  theme(text = element_text(size=22), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
donation_box


qqPlot(df$Donations)
shapiro.test(df$Donations)
t.test(df$Donations[df$Heat == 0], df$Donations[df$Heat == 1], paired=TRUE)
t.test(df$Donations[df$Order == 1], df$Donations[df$Order == 2], paired=TRUE)
wilcox.test(df$Donations[df$Heat == 0], df$Donations[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")
wilcox.test(df$Donations[df$Order == 1], df$Donations[df$Order == 2], paired=TRUE, p.adjust.method = "bonf")
# aov_rm <- df %>% aov_car(NASATLX_SUM~Heat +Error(ID / Heat ),
#                          data=., include_aov= TRUE)
# aov_rm





#############
##Social connectedness
#############


df$social_sum <- df$Social_Connectedness_1 + df$Social_Connectedness_2

social_box <- ggplot(df, aes(x = Heat, y = social_sum, fill=Heat)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Thermal Feedback") + 
  scale_y_continuous(name = "Response") +
  theme_classic() +
  ggtitle("Social") +
  theme(text = element_text(size=22), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
social_box

social_bar <- ggplot(df, aes(x = PrePresentation, y = social_sum, color=Heat)) +
  geom_point() + 
  scale_x_discrete(name = "Presentation Confidence", limits=c(0,1)) + 
  scale_y_continuous(name = "Response", breaks=pretty_breaks(), limits=c(0,10), expand=c(0,0)) +
  theme_classic() +
  ggtitle("Social") +
  theme(text = element_text(size=22), legend.position="bottom", plot.title = element_text(hjust=0.5))
social_bar




qqPlot(df$social_sum)
shapiro.test(df$social_sum)
t.test(df$social_sum[df$Heat == 0], df$social_sum[df$Heat == 1], paired=TRUE)
t.test(df$social_sum[df$Order == 1], df$social_sum[df$Order == 2], paired=TRUE)
wilcox.test(df$social_sum[df$Heat == 0], df$social_sum[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")
# aov_rm <- df %>% aov_car(NASATLX_SUM~Heat +Error(ID / Heat ),
#                          data=., include_aov= TRUE)
# aov_rm




#############
## Thermal Feedback
#############
thermal_questions <- c("How much do you feel the thermal feedback helped you deliver the presentation?",
                            "How distracting was the thermal feedback when delivering the presentation?",
                            "How much would you like to give future presentations with thermal feedback?")


df_thermal <- data.frame("Thermal_1" = df$Thermal_1[df$Thermal_1 != 0],
                         "Thermal_2" = df$Thermal_2[df$Thermal_2 != 0],
                         "Thermal_3" = df$Thermal_3[df$Thermal_3 != 0])

df_thermal$Thermal_1 <- factor(df_thermal$Thermal_1, levels = seq(1,7))
df_thermal$Thermal_2 <- factor(df_thermal$Thermal_2, levels = seq(1,7))
df_thermal$Thermal_3 <- factor(df_thermal$Thermal_3, levels = seq(1,7))


names(df_thermal) <- thermal_questions
likert_thermal = likert(df_thermal)
likert_thermal
plot(likert_thermal) + theme(plot.title = element_text(hjust=0.5)) + ggtitle("Feedback on Thermal Feedback")




#############
## Thermal Feedback
#############
attrak_questions <- c("technical - human",
                      "complicated - simple",
                      "impractical - practical",
                      "cumbersome - straightforward",
                      "unpredictable - predictable",
                      "confusing - clearly structured",
                      "unruly - manageable",
                      
                      "isolating - connective",
                      "unprofessional - professional",
                      "tacky - stylish",
                      "cheap - premium",
                      "alienating - integrating",
                      "separates me from people - connecting",
                      "unpresentable - presentable",
                      
                      "conventional - inventive",
                      "unimaginative - creative",
                      "cautious - bold",
                      "conservative - innovative",
                      "dull - captivating",
                      "undemanding - challenging",
                      "ordinary - novel",
                      
                      "unpleasant - pleasant",
                      "ugly - attractive",
                      "disagreeable - likeable",
                      "rejecting - inviting",
                      "bad - good",
                      "repelling - appealing",
                      "discouraging - motivating"
                      )

attrak_questions_prag <- c("technical - human",
                      "complicated - simple",
                      "impractical - practical",
                      "cumbersome - straightforward",
                      "unpredictable - predictable",
                      "confusing - clearly structured",
                      "unruly - manageable"
)
attrak_questions_ident <- c(
                      "isolating - connective",
                      "unprofessional - professional",
                      "tacky - stylish",
                      "cheap - premium",
                      "alienating - integrating",
                      "separates me from people - connecting",
                      "unpresentable - presentable"
)
attrak_questions_stim <- c(
                      
                      "conventional - inventive",
                      "unimaginative - creative",
                      "cautious - bold",
                      "conservative - innovative",
                      "dull - captivating",
                      "undemanding - challenging",
                      "ordinary - novel"
)
attrak_questions_att <- c(
                      
                      "unpleasant - pleasant",
                      "ugly - attractive",
                      "disagreeable - likeable",
                      "rejecting - inviting",
                      "bad - good",
                      "repelling - appealing",
                      "discouraging - motivating"
)






df_attrak <- data.frame("Attrak_1" = na.omit(df$AttrakDiff_1),
                         "Attrak_2" = na.omit(df$AttrakDiff_2),
                         "Attrak_3" = na.omit(df$AttrakDiff_3),
                        "Attrak_4" = na.omit(df$AttrakDiff_4),
                        "Attrak_5" = na.omit(df$AttrakDiff_5),
                        "Attrak_6" = na.omit(df$AttrakDiff_6),
                        "Attrak_7" = na.omit(df$AttrakDiff_7),
                        "Attrak_8" = na.omit(df$AttrakDiff_8),
                        "Attrak_9" = na.omit(df$AttrakDiff_9),
                        "Attrak_10" = na.omit(df$AttrakDiff_10),
                        "Attrak_11" = na.omit(df$AttrakDiff_11),
                        "Attrak_12" = na.omit(df$AttrakDiff_12),
                        "Attrak_13" = na.omit(df$AttrakDiff_13),
                        "Attrak_14" = na.omit(df$AttrakDiff_14),
                        "Attrak_15" = na.omit(df$AttrakDiff_15),
                        "Attrak_16" = na.omit(df$AttrakDiff_16),
                        "Attrak_17" = na.omit(df$AttrakDiff_17),
                        "Attrak_18" = na.omit(df$AttrakDiff_18),
                        "Attrak_19" = na.omit(df$AttrakDiff_19),
                        "Attrak_20" = na.omit(df$AttrakDiff_20),
                        "Attrak_21" = na.omit(df$AttrakDiff_21),
                        "Attrak_22" = na.omit(df$AttrakDiff_22),
                        "Attrak_23" = na.omit(df$AttrakDiff_23),
                        "Attrak_24" = na.omit(df$AttrakDiff_24),
                        "Attrak_25" = na.omit(df$AttrakDiff_25),
                        "Attrak_26" = na.omit(df$AttrakDiff_26),
                        "Attrak_27" = na.omit(df$AttrakDiff_27),
                        "Attrak_28" = na.omit(df$AttrakDiff_28))

df_attrak[1:28] <- lapply(df_attrak[1:28], function(x) factor(x, seq(-3,3)))

names(df_attrak) <- attrak_questions
likert_attrak = likert(df_attrak)
likert_attrak
likert_attrak$Item <- factor(likert_attrak$Item, levels = attrak_questions)
plot(likert_attrak, group.order=attrak_questions) + theme(plot.title = element_text(hjust=0.5)) + ggtitle("AttrakDiff Scores on Thermal Feedback")


likert_attrak_prag = likert(df_attrak[1:7])
likert_attrak_prag
likert_attrak_prag$Item <- factor(likert_attrak_prag$Item, levels = attrak_questions_prag)
plot(likert_attrak_prag, group.order=attrak_questions_prag) + theme(plot.title = element_text(hjust=0.5)) + ggtitle("AttrakDiff - Pragmatic Quality")


likert_attrak_ident = likert(df_attrak[8:14])
likert_attrak_ident
likert_attrak_ident$Item <- factor(likert_attrak_ident$Item, levels = attrak_questions_ident)
plot(likert_attrak_ident, group.order=attrak_questions_ident) + theme(plot.title = element_text(hjust=0.5)) + ggtitle("AttrakDiff - Hedonic Quality Identity")

likert_attrak_stim = likert(df_attrak[15:21])
likert_attrak_stim
likert_attrak_stim$Item <- factor(likert_attrak_stim$Item, levels = attrak_questions_stim)
plot(likert_attrak_stim, group.order=attrak_questions_stim) + theme(plot.title = element_text(hjust=0.5)) + ggtitle("AttrakDiff - Hedonic Quality Stimulation")

likert_attrak_att = likert(df_attrak[22:28])
likert_attrak_att
likert_attrak_att$Item <- factor(likert_attrak_att$Item, levels = attrak_questions_att)
plot(likert_attrak_att, group.order=attrak_questions_att) + theme(plot.title = element_text(hjust=0.5)) + ggtitle("AttrakDiff - Attractiveness")




### need to format this so all the answers are in one column

df_attrak_mean <- df_attrak_mean %>% map_df(rev)
df_attrak_mean$Question <- as.numeric(factor(df_attrak_mean$Question))
df_attrak_mean$negative <- factor(df_attrak_mean$negative)
df_attrak_mean$positive <- factor(df_attrak_mean$positive)

# df$i.Panas_negative<-df$afraid+df$ashamed+df$hostile+df$nervous+df$upset
df_attrak_mean$Mean


int_plot <- ggplot(df_attrak_mean, aes(x=Mean, Question)) +
  geom_path(color="#008080") +
  geom_point(color="#008080") +
  geom_vline(xintercept=0, linetype="longdash") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    text = element_text(size = 24),
    legend.title = element_blank()
  ) +
  scale_y_continuous(
    breaks = 1:length(df_attrak_mean$positive),
    labels = df_attrak_mean$negative,
    sec.axis = sec_axis(~.,
                        breaks = 1:length(df_attrak_mean$positive),
                        labels = df_attrak_mean$positive
    )
  ) +
  scale_x_continuous(breaks = seq(-3, 3)) +
  xlab("") +
  ylab("") +
  # theme(strip.background = element_rect(colour = "black", fill = "white"),  legend.position = "bottom", legend.background = element_blank()) +
  coord_cartesian(xlim = c(-3, 3)) 
int_plot

ggsave( "test_attrak.pdf", width=8, height=12)


df_attrak_full_mean <- data.frame("ID"=seq(1,21))
df_attrak_full_mean$PQ <- tapply(df_attrak_full$Value[df_attrak_full$Group=="PQ"], df_attrak_full$ID[df_attrak_full$Group=="PQ"], mean)
df_attrak_full_mean$HQ <- tapply(df_attrak_full$Value[df_attrak_full$Group=="HQ"], df_attrak_full$ID[df_attrak_full$Group=="HQ"], mean)
mean(df_attrak_full_mean$PQ)
mean(df_attrak_full_mean$HQ)

attrak_full_plot <- ggplot(df_attrak_full_mean, aes(PQ, HQ, group=ID)) +
  geom_point() +
  scale_x_continuous(limits  = c(-3, 3), breaks = c(-3,-1,1,3), expand=c(0,0)) +
  scale_y_continuous(limits = c(-3, 3), breaks = c(-3,-1,1,3), expand=c(0,0)) +
  xlab("Pragmatic Qualities") +
  ylab("Hedonic Qualities") +
  theme(
    panel.grid.major = element_line(size=1, linetype="solid"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    text = element_text(family = "Arial Black", size = 12),
    legend.title = element_blank()
  )
attrak_full_plot


# do 95 confidence for each and draw rectangle
error <- qnorm(0.975)*sd(df_attrak_full_mean$PQ)/sqrt(length(df_attrak_full_mean$PQ))
pq_left <- mean(df_attrak_full_mean$PQ) - error
pq_right <- mean(df_attrak_full_mean$PQ) + error

error <- qnorm(0.975)*sd(df_attrak_full_mean$HQ)/sqrt(length(df_attrak_full_mean$HQ))
hq_left <- mean(df_attrak_full_mean$HQ) - error
hq_right <- mean(df_attrak_full_mean$HQ) + error

df_attrak_rect <- data.frame(x1=pq_left,
                   x2=pq_right,
                   y1=hq_left,
                   y2=hq_right)
attrak_full_plot <- ggplot() +
  geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4) +
  geom_rect(data=df_attrak_rect, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2)) +
  scale_x_continuous(limits  = c(-3, 3), breaks = c(-3,-1,1,3), expand=c(0,0)) +
  scale_y_continuous(limits = c(-3, 3), breaks = c(-3,-1,1,3), expand=c(0,0)) +
  xlab("Pragmatic Qualities") +
  ylab("Hedonic Qualities") +
  theme(
    panel.grid.major = element_line(size=1, linetype="solid"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    text = element_text(family = "Arial Black", size = 12),
    legend.title = element_blank()
  )
attrak_full_plot

###############
# Anxiety
df$Anxiety <- df$Anxiety_1 + df$Anxiety_3 +
  df$Anxiety_3

df$Anxiety


###########
# PRCS

df$PRCS <- df$PRCS_1 + df$PRCS_2 +
  df$PRCS_3 +df$PRCS_4 + df$PRCS_5 + df$PRCS_6 + df$PRCS_7 +
  df$PRCS_8 + df$PRCS_9
df$PRCS
anxiety_plot <- ggplot(df, aes(x = Anxiety, y = PRCS)) +
  geom_point() + 
  scale_x_discrete(name = "Anxiety") + 
  scale_y_continuous(name = "Public Speaking Fear") +
  theme_classic() +
  ggtitle("Perceived Presentation Quality") +
  theme(text = element_text(size=22), legend.position="bottom", plot.title = element_text(hjust=0.5))
anxiety_plot


qqPlot(df$Pres_Qual)
shapiro.test(df$Pres_Qual)
t.test(df$Pres_Qual[df$Heat == 0], df$Pres_Qual[df$Heat == 1], paired=TRUE)
t.test(df$Pres_Qual[df$Order == 1], df$Pres_Qual[df$Order == 2], paired=TRUE)
wilcox.test(df$Pres_Qual[df$Heat == 0], df$Pres_Qual[df$Heat == 1], paired=TRUE, p.adjust.method = "bonf")
wilcox.test(df$Pres_Qual[df$Order == 1], df$Pres_Qual[df$Order == 2], paired=TRUE, p.adjust.method = "bonf")
# aov_rm <- df %>% aov_car(NASATLX_SUM~Heat +Error(ID / Heat ),
#                          data=., include_aov= TRUE)
# aov_rm




########
# Correlations

?cor.test
cor.test(df$Pres_Qual,df$PrePresentation) # NO
cor.test(df$NASATLX_SUM,df$PrePresentation) # NO
cor.test(df$social_sum,df$PrePresentation) # No
cor.test(df$Presentation_Quality_8,df$PrePresentation) # NO
cor.test(df$Donations,df$PrePresentation) # No
cor.test(df$Proficiency,df$PrePresentation) # NO


cor.test(df$PrePresentation,df$Pres_Qual) # NO
cor.test(df$NASATLX_SUM,df$Pres_Qual) # NO
cor.test(df$social_sum,df$Pres_Qual) # YES
cor.test(df$Presentation_Quality_8,df$Pres_Qual) # NO
cor.test(df$Donations,df$Pres_Qual) # YES
cor.test(df$Proficiency,df$Pres_Qual) # NO


cor.test(df$PrePresentation,df$Presentation_Quality_8) # NO
cor.test(df$NASATLX_SUM,df$Presentation_Quality_8) # YES
cor.test(df$social_sum,df$Presentation_Quality_8) # NO
cor.test(df$Pres_Qual,df$Presentation_Quality_8) # NO
cor.test(df$Donations,df$Presentation_Quality_8) # NO
cor.test(df$Proficiency,df$Presentation_Quality_8) # NO


cor.test(df$PrePresentation,df$NASATLX_SUM) # NO
cor.test(df$Presentation_Quality_8,df$NASATLX_SUM) # YES
cor.test(df$social_sum,df$NASATLX_SUM) # NO
cor.test(df$Pres_Qual,df$NASATLX_SUM) # NO
cor.test(df$Donations,df$NASATLX_SUM) # NO
cor.test(df$Proficiency,df$NASATLX_SUM) # NO


cor.test(df$PrePresentation,df$Anxiety) # Yes
cor.test(df$Presentation_Quality_8,df$Anxiety) # YES
cor.test(df$social_sum,df$Anxiety) # NO
cor.test(df$Pres_Qual,df$Anxiety) # NO
cor.test(df$Donations,df$Anxiety) # NO
cor.test(df$Proficiency,df$Anxiety) # YES

cor.test(df$PrePresentation,df$PRCS) # Yes
cor.test(df$Presentation_Quality_8,df$PRCS) # YES
cor.test(df$social_sum,df$PRCS) # NO
cor.test(df$Pres_Qual,df$PRCS) # NO
cor.test(df$Donations,df$PRCS) # NO
cor.test(df$Proficiency,df$PRCS) # YES


cor.test(df[df$Heat==1,]$social_sum-df[df$Heat==0,]$social_sum,df[df$Heat==1,]$Pres_Qual) #no
cor.test(df[df$Heat==1,]$social_sum,df[df$Heat==1,]$Pres_Qual) #yes
cor.test(df[df$Heat==0,]$social_sum,df[df$Heat==0,]$Pres_Qual) #no
cor.test(df$social_sum,df$Pres_Qual) #yes
plot(df$social_sum,df$Pres_Qual)
plot(df[df$Heat==1,]$social_sum,df[df$Heat==1,]$Pres_Qual)

df$Thermal_Feedback[df$Heat == 1] <- "On"
df$Thermal_Feedback[df$Heat == 0] <- "Off"

cor.test(df$Proficiency,df$PrePresentation) # NO
cor.test(df$Proficiency,df$social_conn) # NO
cor.test(df$Proficiency,df$Pres_Qual) # NO
mean(df$Proficiency)
min(df$Proficiency)
max(df$Proficiency)


df$social_conn <- 10-df$social_sum
social_cor_plot <- ggplot(df, aes(x = Pres_Qual, y = social_conn, color=Thermal_Feedback, fill=Thermal_Feedback)) +
  geom_point(size=2) + 
  scale_x_continuous(name = "Perceived Presentation Quality") + 
  scale_y_continuous(name = "Social Connectedness") +
  theme_classic() +
  theme(text = element_text(family='ArialMT', size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
  geom_smooth(method=lm, se=TRUE, alpha=0.2) +
  labs(color="Thermal Feedback", fill="Thermal Feedback" ) +
  scale_color_manual(values= two_color )+ 
  scale_fill_manual(values= two_color)+
  # scale_color_manual(values= c("blue", "red")) + 
  # scale_fill_manual(values= c("blue", "red")) +
  stat_cor(method = "pearson", label.x = 27, size=10)
social_cor_plot

ggsave( "test_social.pdf", width=16, height=8, device=cairo_pdf())



cor.test(df[df$Heat==1,]$Donations-df[df$Heat==0,]$Donations,df[df$Heat==1,]$Pres_Qual) #yes
cor.test(df[df$Heat==1,]$Donations,df[df$Heat==1,]$Pres_Qual) #yes
cor.test(df[df$Heat==0,]$Donations,df[df$Heat==0,]$Pres_Qual) #no
cor.test(df$Donations,df$Pres_Qual) #yes
plot(df$Donations,df$Pres_Qual)



donations_cor_plot <- ggplot(df, aes(x = Pres_Qual, y = 100*Donations/5, color=Thermal_Feedback, fill=Thermal_Feedback)) +
  geom_point(size=2) + 
  scale_x_continuous(name = "Perceived Presentation Quality") + 
  scale_y_continuous(name = "Expected % of Audience that Donated") +
  theme_classic() +
  theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
  geom_smooth(method=lm, se=TRUE, alpha=0.2) +
  labs(color="Thermal Feedback", fill="Thermal Feedback" ) +
  scale_color_manual(values= c("blue", "red")) + 
  scale_fill_manual(values= c("blue", "red")) + 
  stat_cor(method = "pearson", label.x = 27, size=10) 
donations_cor_plot

ggsave( "test_donations.pdf", width=12, height=8, device=cairo_pdf())


cor.test(df[df$Heat==1,]$NASATLX_SUM-df[df$Heat==0,]$NASATLX_SUM,df[df$Heat==1,]$Presentation_Quality_8) #YES
cor.test(df[df$Heat==1,]$NASATLX_SUM,df[df$Heat==1,]$Presentation_Quality_8) #YES
cor.test(df[df$Heat==0,]$NASATLX_SUM,df[df$Heat==0,]$Presentation_Quality_8) #NO
plot(df[df$Heat==1,]$NASATLX_SUM,df[df$Heat==1,]$Presentation_Quality_8)
plot(df[df$Heat==0,]$NASATLX_SUM,df[df$Heat==0,]$Presentation_Quality_8)



cor.test(df[df$Heat==1,]$Donations-df[df$Heat==0,]$Donations,df[df$Heat==1,]$PRCS) #no
cor.test(df[df$Heat==1,]$Donations,df[df$Heat==1,]$PRCS) #yes
cor.test(df[df$Heat==0,]$Donations,df[df$Heat==0,]$PRCS) #no
cor.test(df$Donations,df$PRCS) #yes
plot(df$Donations,df$PRCS)
plot(df[df$Heat==1,]$Donations,df[df$Heat==1,]$PRCS)
plot(df[df$Heat==0,]$Donations,df[df$Heat==0,]$PRCS)


cor.test(df[df$Heat==1,]$Presentation_Quality_8-df[df$Heat==0,]$Presentation_Quality_8,df[df$Heat==1,]$PRCS) #no
cor.test(df[df$Heat==1,]$Presentation_Quality_8,df[df$Heat==1,]$PRCS) #yes
cor.test(df[df$Heat==0,]$Presentation_Quality_8,df[df$Heat==0,]$PRCS) #no
cor.test(df$Presentation_Quality_8,df$PRCS) #yes
plot(df$Presentation_Quality_8,df$PRCS)
plot(df[df$Heat==1,]$Presentation_Quality_8,df[df$Heat==1,]$PRCS)
plot(df[df$Heat==0,]$Presentation_Quality_8,df[df$Heat==0,]$PRCS)



df_eyetracker <- data.frame("time" = rep(seq(0,180),20),
                            "ID" = rep(1:20, each=181),
                            "fixation" = rep(0,181*20))
df_eyetracker$fixation[df_eyetracker$ID==1 & df_eyetracker$time==90] <- 1
df_eyetracker$fixation[df_eyetracker$ID==1 & df_eyetracker$time==139] <- 1
df_eyetracker$fixation[df_eyetracker$ID==1 & df_eyetracker$time==144] <- 1
df_eyetracker$fixation[df_eyetracker$ID==4 & df_eyetracker$time==63] <- 1
df_eyetracker$fixation[df_eyetracker$ID==4 & df_eyetracker$time==167] <- 1
df_eyetracker$fixation[df_eyetracker$ID==13 & df_eyetracker$time==132] <- 1
df_eyetracker$ID <- factor(df_eyetracker$ID)

eyetracker_plot <- ggplot(df_eyetracker, aes(x = time, y = fixation, color=ID)) +
  geom_point() +
  geom_path() +
  scale_x_continuous(name = "Time (s)") + 
  scale_y_continuous(name = "Looking at Bracelet", limits=c(0,1), breaks=c(0,1)) +
  theme_classic() +
  theme(text = element_text(size=22), legend.position="bottom", plot.title = element_text(hjust=0.5))
eyetracker_plot

df_eyetracker$heat <- rep(0,181*20)
df_eyetracker$heat[df_eyetracker$time >= 50 & df_eyetracker$time < 80] <- -2
df_eyetracker$heat[df_eyetracker$time >= 80 & df_eyetracker$time < 110] <- -1
df_eyetracker$heat[df_eyetracker$time >= 110 & df_eyetracker$time < 130] <- 1
df_eyetracker$heat[df_eyetracker$time >= 130 & df_eyetracker$time < 165] <- 2
df_eyetracker$heat[df_eyetracker$time >= 165 & df_eyetracker$time <= 180] <- 1
df_eyetracker$positive <- rep("grey",181*20)
df_eyetracker$positive[df_eyetracker$heat > 0] <- "red"
df_eyetracker$positive[df_eyetracker$heat < 0] <- "blue"
df_eyetracker$positive <- factor(df_eyetracker$positive)

heat_plot <- ggplot(df_eyetracker, aes(x = time, y = heat, color=positive)) +
  geom_point(show.legend = FALSE, shape="-", size=10) +
  scale_x_continuous(name = "Time (s)") + 
  scale_y_continuous(name = "Thermal Signal") +
  theme_classic() +
  theme(text = element_text(size=22),  plot.title = element_text(hjust=0.5)) +
  scale_color_manual(values= c("blue", "black", "red"))
heat_plot

