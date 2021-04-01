full_comparison_df <- 
  Bigtbl %>%
  filter(study_type == 'Interventional') %>% 
  filter(study_first_submitted_date >= ymd('20071001')) %>%
  filter(study_first_submitted_date < ymd('20200101')) %>%
  filter(nct_id %nin% (full_gi_df %>% pull(nct_id)))

full_gi_df <- surg_onc_df
fgi_df <- 
  full_gi_df %>%
  bpivotwider_single_factor_to_logical(column = 'industry_any2b', add_prefix = 'funding2b', makenames = TRUE) %>%
  mutate(year_trial = year(study_first_submitted_date)) 

fcomparison_df <- 
  full_comparison_df %>%
  bpivotwider_single_factor_to_logical(column = 'industry_any2b', add_prefix = 'funding2b', makenames = TRUE) %>%
  mutate(year_trial = year(study_first_submitted_date)) 

gi_trial_growth_global <-
  bgenerateSummaryGrowthDataStatistics(fgi_df,
                                       additional_columns = list(
                                         'sponsor' = c('funding2b_US.Govt', 'funding2b_Industry', 'funding2b_Other')
                                       ), date_limits = c(2007, 2020))

gi_trial_growth_data_global <- gi_trial_growth_global[["Data"]]
gi_trial_growth_statistics_global <- gi_trial_growth_global[["Statistics"]]

comparison_trial_growth_global <-
  bgenerateSummaryGrowthDataStatistics(fcomparison_df,
                                       additional_columns = list(
                                         'sponsor' = c('funding2b_US.Govt', 'funding2b_Industry', 'funding2b_Other')
                                       ), date_limits = c(2007, 2020))

comparison_trial_growth_data_global <- comparison_trial_growth_global[["Data"]]
comparison_trial_growth_statistics_global <- comparison_trial_growth_global[["Statistics"]]

color1 <- '#981B1E'# looks similar to crimson and firebrick, but I think this is JAMA gi header?

fig_df_1a_yearly_gi_global <- gi_trial_growth_data_global %>% select(year_trial, total_trials_all)
max_yearly_gi_global <- max(fig_df_1a_yearly_gi_global$total_trials_all)
gg_fig_1a_yearlytotal_gi_global <-
  ggplot(data = fig_df_1a_yearly_gi_global,
         aes(x = year_trial, y = total_trials_all)) + 
  geom_bar(stat = 'identity',
           fill = color1,
           width = 0.5) +
  geom_text(aes(label = total_trials_all),
            size = 3,
            vjust = -0.5) + 
  scale_x_continuous(breaks = c(2008:2020)) + 
  scale_y_continuous(breaks = seq(0, max_yearly_gi_global *1.5, roundUpNice(max_yearly_gi_global/8, nice = c(1,5,10))),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)) +
  coord_cartesian(ylim = c(0, pretty(max_yearly_gi_global*9/8)[2])) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew Clinical\nTrials Per Year');



fig_df_1c_yearly_gi_pct_all_global <- 
  left_join(gi_trial_growth_data_global %>% select(year_trial, total_trials_all),
            comparison_trial_growth_data_global %>% select(year_trial, total_trials_all),
            by = 'year_trial', suffix = c('_gi', '_comparison')) %>%
  mutate(total_trials_all_combined = total_trials_all_gi + total_trials_all_comparison) %>%
  mutate(gi_pct_all_combined = total_trials_all_gi / total_trials_all_combined) %>% 
  mutate(constant = 'constant') %>%
  mutate(label_gi_pct_all_combined = bpadding(num = gi_pct_all_combined, width = 6, makepercent = TRUE, num_decimals = 2))

gg_fig_1c_yearlypct_gi_combined_global <- 
  ggplot(data = fig_df_1c_yearly_gi_pct_all_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), 
             y = gi_pct_all_combined,
             group = constant)) + # not sure if it's a bug but we have to specify a group variable here for this
  geom_line(color = color1, 
            size = 2) +
  geom_point(fill = color1, 
             color = color1, 
             shape = 21, size = 5, alpha = 1) +
  geom_text(aes(label = label_gi_pct_all_combined),
            size = 3,
            vjust = -2.0,
            hjust = 0.35) + 
  scale_y_continuous(breaks = seq(0, 1, 0.04),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, .01)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', 
       y = '\nProportion of All\nClinical Trials');


fig_df_1d_yearly_sponsor_gi_global <- 
  gi_trial_growth_data_global %>%
  select(year_trial, total_trials_all, total_trials_sponsor, starts_with('funding2b')) 

fig_df_1d_yearly_sponsor_gi_pct_global <- 
  fig_df_1d_yearly_sponsor_gi_global %>%
  select(year_trial, ends_with('_pct')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_pct$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'gi')

subcolor13 <- '#2F4F4F' # dark slate gray
subcolor14 <- '#FFA500' # orange
subcolor15 <- '#00B2EE' # deep sky blue

g_industry_color5 <- c(subcolor13, subcolor14, subcolor15) # brannon JAMA colors
gg_fig_1d_yearly_sponsor_gi_pct_global <- 
  ggplot(data = fig_df_1d_yearly_sponsor_gi_pct_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, 1.0)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); 


fig_df_1b_yearly_enrollment_gi_global <-
  full_gi_df %>%
  mutate(year_trial = year(study_first_submitted_date)) %>%
  filter(br_studystatus == 'Completed') %>%
  select(year_trial, enrollment, enrollment_type, study_first_submitted_date, actual_duration) %>%
  mutate(new_enrollment = Hmisc::cut2(x = enrollment, 
                                      cuts = c(0,1, seq(30, 300, 30), Inf)),
         cap_enroll_2000 = case_when(enrollment > 1000 ~ as.double(1000), TRUE ~ as.double(enrollment))) %>%
  mutate(new_actduration = Hmisc::cut2(actual_duration, c(0, 10, 20, 30, 40, 50, Inf)))


gg_fig_1b_yearly_enrollment_gi_global <- 
  ggplot(data = fig_df_1b_yearly_enrollment_gi_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = cap_enroll_2000)) + 
  geom_jitter(alpha = 0.3, color = color1, width = 0.1, size = 3) +
  geom_boxplot(outlier.colour = NA, 
               alpha = 0,
               coef = 0, 
               outlier.size = 0,
               width = 0.5,
               lwd = 1.0, # thickness of boxplot
               fatten = 1 # thickness of median line
  ) + 
  theme_bw() + 
  # scale_y_continuous(breaks = c(seq(0, 500, 100)), limits = c(0, 500)) + # this removes data! use coord_cartesian to set limits
  # see here for padding labels: https://stackoverflow.com/questions/34677551/is-it-possible-to-fix-axis-margin-with-ggplot2
  scale_y_continuous(breaks = c(seq(0, 500, 50)),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)
                     # labels = function(label) sprintf('%5.0f', label) # width = 4, decimals = 0, padding = space
  ) + 
  coord_cartesian(ylim = c(0, 500)) + 
  xlab(label = 'Year of Trial Submission') + 
  ylab(label = '\nEnrollment Per \nCompleted Trial') +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0)));


gg_fig_1_4pane_gi_global_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 4, ncol = 1,
                                                   heights = c(2, 1, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D')) %>%
annotate_figure(top= text_grob('Characteristics of Surgical Oncology Trials Over Time', face = 'bold'))

gg_fig_1_4pane_gi_global_c
