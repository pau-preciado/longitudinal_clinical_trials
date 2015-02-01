library(ProjectTemplate)
reload.project()

# Format (long)
head(diastolic)

diastolic <- diastolic %>%
     rename(row = X)

names(diastolic) <- tolower(names(diastolic))

head(diastolic)

diast1 <-diastolic %>%
     select(subject, trt, age, sex, dbp1:dbp5) %>%
     gather(measure, dbp, dbp1:dbp5) %>%
     tbl_df()

diast1 <- diast1 %>% 
     mutate(measure = as.numeric(str_sub(variable, 4, -1))) %>%
     select(subject, trt, age, sex, measure, value)

diast1

# Lets make some plots

ggplot(diast1, aes(x = measure, y = value, group = subject, color = trt)) +
     geom_line() +
     geom_point() +
     geom_smooth(aes(group = trt))


ggplot(diast1, aes(x = factor(measure), y = value, color = trt)) +
     geom_boxplot()

agr1 <- diast1 %>% group_by(trt, sex, measure) %>%
     summarise(m.val = mean(value), n = length(value),
               sd = sd(value)/sqrt(n)) 

ggplot(agr1, aes(x = measure, y = m.val, ymin = m.val - 2*sd,
                 ymax = m.val + 2*sd, color = trt)) +
            geom_point() + geom_line() + 
            geom_linerange() +
     facet_wrap(~sex)

# Exploratory: Fit a linear model for each subject
#####################################

get.coef <- function(m1){
     df1 <- data.frame(int = m1$coefficients[1], slope = m1$coefficients[2])
     rownames(df1) <- NULL
     df1
}

lm.subject <- diast1 %>% group_by(subject) %>%
     do(mod = lm(value ~ measure, data = .))

coef.lm <- 
     cbind(subject = 1:40, lm.subject %>% do(data.frame(int = coef(.$mod)[1])),
           lm.subject %>% do(data.frame(slope = coef(.$mod)[2])))

coef.lm <- coef.lm %>% left_join(select(diastolic, subject, trt, age, sex))

summary(coef.lm)

hist.int <- ggplot(coef.lm, aes(x = int, group = trt, fill = trt)) + 
     geom_histogram(alpha = 0.5)

hist.slope <- ggplot(coef.lm, aes(x = slope, group = trt, fill = trt)) + 
     geom_histogram(alpha = 0.5) + coord_flip()+
     theme(legend.position = "none")

scatter <- ggplot(coef.lm, aes(x = int, y = slope, color = trt, group = trt)) +
     geom_point() +
     theme(legend.position = "none")

empty <- ggplot() + geom_point(aes(1,1), colour="white")+
     theme(axis.ticks=element_blank(), 
          panel.background=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),           
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank())

grid.arrange(hist.int, empty, scatter, hist.slope, 
             ncol = 2, nrow=2, widths=c(2.5, 1), heights=c(1, 2))


# Model for the coeficients

m1 <- lm(slope ~ int + trt, data = coef.lm)
summary(m1)

MASS:::dropterm(m1)


# Multilevel
################

m1.ml <- lmer(value ~ trt + measure + (measure | subject), data = diast1)
arm:::display(m1.ml)

m2.ml <- lmer(value ~ trt + measure + age + (measure | subject), data = diast1)
arm:::display(m2.ml)
summary(m2.ml)

anova(m1.ml, m2.ml)

# Residual analysis

res1 <- fortify(m2.ml)
res1

# QQ norm
par(mfrow = c(1,2))
qqnorm(filter(res1, trt == "A")$.resid, 
       main = "Normal Q-Q Plot for residuals\nTreatment A",
       cex.main = 1, pch = 16, col = "gray50")
qqline(filter(res1, trt == "A")$.resid, col = 2)
qqnorm(filter(res1, trt == "B")$.resid, 
       main = "Normal Q-Q Plot for residuals\nTreatment B",
       cex.main = 1, pch = 16, col = "gray50")
qqline(filter(res1, trt == "B")$.resid, col = 2)
par(mfrow = c(1,1))

ks.test(filter(res1, trt == "B")$.scresid, "pnorm")
ks.test(filter(res1, trt == "A")$.scresid, "pnorm")

# Fitted vs residuals
ggplot(res1, aes(x = .fitted, y = .scresid)) +
     geom_point() +
     geom_hline(yintercept = c(-2,2), lty = 2) +
     facet_wrap(~trt, scales = "free")




