library('ggplot2')

setwd("~/code/ses-vs-college-attendance-els02")

data <- read.csv ('data/data.csv')


summary <- matrix(nrow=0, ncol=3)
colnames(summary) <- c('decile', 'pct', 'first_pei_level')

for (i in 1:10){
  props <- prop.table(table(data$first_pei[data$byses1_percentile == i]))
  temp_mat <- matrix(nrow=4, ncol=3)
  
  temp_mat[1:4,1] = i
  
  temp_mat[1,2] = props['no college']*100
  temp_mat[1,3] = 'No College'
  temp_mat[2,2] = props['less than 2-years']*100
  temp_mat[2,3] = '< 2-year'
  temp_mat[3,2] = props['2-years']*100
  temp_mat[3,3] = '2-year'
  temp_mat[4,2] = props['4-years or more']*100
  temp_mat[4,3] = '4-year'
  summary <- rbind(summary, temp_mat)
}


summary_data <- data.frame(summary)
summary_data$pct <- as.numeric(as.character(summary_data$pct))

summary_data$decile <- ordered(summary_data$decile, levels=seq(1:10))
summary_data$first_pei_level <- ordered(summary_data$first_pei_level, levels=c('No College', '< 2-year', '2-year', '4-year'))


p <-  ggplot(data=summary_data, aes(x=decile, y=pct, fill=first_pei_level)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  xlab('SES Decile') + 
  ylab('Percent') +
  ylim(0,100) +
  ggtitle("SES Decile vs. Level of First Attended College\n(Educational Longitudinal Study of 2002, Public Data)") +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('figures/all_levels_v_decile.png')

## now just do four-year

four_yr_summary <- matrix(ncol=2, nrow=10)
colnames(four_yr_summary) <- c('decile', 'pct')

four_yr_summary[1:10,1] <- seq(1:10)

for (i in 1:10){
  props <- prop.table(table(data$first_pei_4yr[data$byses1_percentile == i]))
  four_yr_summary[i,2] <- props['1']*100
}


four_yr_summary <- data.frame(four_yr_summary)
four_yr_summary$pct <- as.numeric(as.character(four_yr_summary$pct))

four_yr_summary$decile <- ordered(four_yr_summary$decile, levels=seq(1:10))

p <- ggplot(data=four_yr_summary, aes(x=decile, y=pct)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  xlab('SES Decile') + 
  ylab('Percent') +
  ylim(0,100) +
  ggtitle("Percent Attended Four-year College (1st College Attended) by SES Decile \n(Educational Longitudinal Study of 2002, Public Data)") +
  theme(legend.title=element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) 

ggsave('figures/4yr_v_decile.png')



## now, for middle quintile math scores
four_yr_summary_w_math <- matrix(nrow=0, ncol=3)
colnames(four_yr_summary_w_math) <- c('decile', 'pct', 'cat')

for (i in 1:10){
  props_all <- prop.table(table(data$first_pei_4yr[data$byses1_percentile == i]))
  props_q3 <- prop.table(table(data$first_pei_4yr[data$byses1_percentile == i & data$bytxmstd_quintile_wtd==3]))
  temp_mat <- matrix(nrow=2, ncol=3)
  
  temp_mat[1:2,1] = i
  
  temp_mat[1,2] = props_all['1']*100
  temp_mat[1,3] = 'All Students'
  temp_mat[2,2] = props_q3['1']*100
  temp_mat[2,3] = 'Middle Quintile, Std. Math Scores'
  
  four_yr_summary_w_math <- rbind(four_yr_summary_w_math, temp_mat)
}

four_yr_summary_w_math <- data.frame(four_yr_summary_w_math)
four_yr_summary_w_math$pct <- as.numeric(as.character(four_yr_summary_w_math$pct))

four_yr_summary_w_math$decile <- ordered(four_yr_summary_w_math$decile, levels=seq(1:10))
four_yr_summary_w_math$cat <- ordered(four_yr_summary_w_math$cat, levels=c('All Students', 'Middle Quintile, Std. Math Scores'))

p <-  ggplot(data=four_yr_summary_w_math, aes(x=decile, y=pct, fill=cat)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  xlab('SES Decile') + 
  ylab('Percent') +
  ylim(0,100) +
  ggtitle("Percent Attended Four-year College (1st College Attended) by SES Decile\n(Educational Longitudinal Study of 2002, Public Data)") +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('figures/4yr_attend_with_math_q3_v_decile.png')

