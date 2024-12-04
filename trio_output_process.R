library(tidyverse)
library(ggsci)

### Adolescents #
results <- list.files(pattern = ".csv")

results

scl_youth <- read.csv(results[1])


result_df_JP <- scl_youth %>%
  mutate(best_fit = case_when(model == "mo" ~ "yes",
                              TRUE ~"no")) %>%
  filter(model != "null") %>%
  filter(variable != "mp") %>%
  filter(variable != "e") %>%
  mutate(value = round(value,3),
         Percent_variance = value*100, 
         model = factor(model, levels = c("full","mo","fo","direct")),
         model = recode(model, 
                        "full" = "Full", 
                        "mo" = "MO", 
                        "fo" = "FO",
                        "direct" = "Direct"),
         variable = case_when(variable == "o" ~ "\nDirect - Child \n",
                              variable == "m" ~ "\nIndirect - Maternal \n", 
                              variable == "p" ~ "\nIndirect - Paternal \n", 
                              variable == "om" ~ "Maternal/Child\nCovariance\n",
                              variable == "op" ~ "Paternal/Child\nCovariance\n"),
         label = case_when(best_fit == "yes" ~ "*"))
 

ggplot(result_df_JP, aes(fill=variable, y=Percent_variance, x=model, color = "white")) + 
  geom_bar(position="stack", stat="identity", color = NA) +
  scale_alpha_discrete(range = c(0.68, 1)) +
  scale_color_manual(values = c("white")) +
  scale_fill_manual(values = c("#5b9bd5", "#f0904e", "#a9d18e", "#9751cb", "#e65454")) +
  theme_classic()+
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        strip.text.x = element_text(size = 14, color = "black"), # size = 18 when using timepoint 1 etc.
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.text.y = element_text(size = 21, color = "black"),
        axis.text.x = element_text(size =16, color = "black"),
        axis.title = element_text(size = 24),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18)) +
  labs(y = "Variance explained (%)",
       title = "",
       fill = "Genetic effects",
       x = "Model") 


# MOTHERS #

results <- list.files(pattern = ".csv")

results

scl_mother_1 <- read.csv(results[13])%>%
  mutate(outcome = "3 weeks")
scl_mother_2 <- read.csv(results[16])%>%
  mutate(outcome = "5 weeks")
scl_mother_3 <- read.csv(results[19])%>%
  mutate(outcome = "7 weeks")

result_df_mothers <- rbind(scl_mother_1,scl_mother_2) %>%
  rbind(scl_mother_3)%>%
mutate(best_fit = case_when((outcome == "Timepoint 2" | outcome == "Timepoint 3") & model == "mf" ~ "yes",
                            (outcome == "Timepoint 1") & model == "direct" ~ "yes",
                            TRUE ~"no"))%>%
  filter(model != "null") %>%
  filter(variable != "mp") %>%
  filter(variable != "e") %>%
  mutate(value = round(value,3),
         Percent_variance = value*100, 
         model = factor(model, levels = c("full","mo", "mf", "direct")),
         model = recode(model, 
                        "full" = "Full", 
                        "mo" = "MO", 
                        "mf" = "MF",
                        "direct" = "Direct"),
         variable = case_when(variable == "p" ~ "\nIndirect - Paternal\n", 
                              variable == "m" ~ "\nDirect - Maternal\n", 
                              variable == "o" ~ "\nIndirect - Child\n",
                              variable == "om" ~ "Maternal/Child\nCovariance\n",
                              variable == "op" ~ "Paternal/Child\nCovariance\n"),
         variable = factor(variable, levels = c(
           "\nDirect - Maternal\n",
           "\nIndirect - Paternal\n",
           "\nIndirect - Child\n",
           "Maternal/Child\nCovariance\n",
           "Paternal/Child\nCovariance\n"
         )),
         label = case_when(best_fit == "yes" ~ "*"))

ggplot(result_df_mothers, aes(fill=variable, y=Percent_variance, x=model, color = "white")) + 
  geom_bar(position="stack", stat="identity", color = NA) +
  facet_grid(~ outcome, switch = "y") +
  scale_color_manual(values = c("white")) +
  scale_fill_manual(values = c("#f0904e", "#a9d18e","#5b9bd5", "#9751cb", "#e65454")) +
  theme_classic() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        strip.text.x = element_text(size = 14, color = "black"),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.text.y = element_text(size = 21, color = "black"),
        axis.text.x = element_text(size =16, color = "black"),
        axis.title = element_text(size = 24),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18)) +
  labs(y = "Variance explained (%)",
       title = "Timepoints after the Covid-19 lockdown",
       fill = "Genetic effects",
       x = "Model") +
  guides(color = "none" )



## Fathers

results <- list.files(pattern = ".csv")

results

scl_father_1 <- read.csv(results[4])%>%
  mutate(outcome = "Timepoint 1")
scl_father_2 <- read.csv(results[7])%>%
  mutate(outcome = "Timepoint 2")
scl_father_3 <- read.csv(results[10])%>%
  mutate(outcome = "Timepoint 3")

result_df_fathers <- rbind(scl_father_1,scl_father_2) %>%
  rbind(scl_father_3)%>%
  mutate(best_fit = case_when((outcome == "Timepoint 1" | outcome == "Timepoint 2") & model == "direct" ~ "yes",
                              (outcome == "Timepoint 3") & model == "nocov" ~ "yes",
                              TRUE ~"no"))%>%
  filter(model != "null") %>%
  filter(variable != "mp") %>% 
  filter(variable != "e") %>%
  mutate(value = round(value,3),
         Percent_variance = value*100, 
         model = factor(model, levels = c("full","fo", "mf", "direct")),
         model = recode(model, 
                        "full" = "Full", 
                        "fo" = "FO", 
                        "mf" = "MF",
                        "direct" = "Direct"),
         variable = case_when(variable == "p" ~ "\nDirect - Paternal\n", 
                              variable == "m" ~ "\nIndirect - Maternal\n", 
                              variable == "o" ~ "\nIndirect - Child\n",
                              variable == "om" ~ "Maternal/Child\nCovariance\n",
                              variable == "op" ~ "Paternal/Child\nCovariance\n"),
         variable = factor(variable, levels = c(
           "\nDirect - Paternal\n",
           "\nIndirect - Maternal\n",
           "\nIndirect - Child\n",
           "Maternal/Child\nCovariance\n",
           "Paternal/Child\nCovariance\n"
         )))


ggplot(result_df_fathers, aes(fill=variable, y=Percent_variance, x=model, color = "white")) + 
  geom_bar(position="stack", stat="identity", color = NA) +
  facet_grid(~ outcome, switch = "y") +
  # scale_alpha_discrete(range = c(0.68, 1)) +
  scale_color_manual(values = c("white")) +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = c("#a9d18e","#f0904e","#5b9bd5", "#9751cb", "#e65454")) +
  theme_classic() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        strip.text.x = element_text(size = 14, color = "black"), 
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.text.y = element_text(size = 21, color = "black"),
        axis.text.x = element_text(size =16, color = "black"),
        axis.title = element_text(size = 24),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18)) +
  labs(y = "Variance explained (%)",
       title = "Timepoints after the Covid-19 lockdown",
       fill = "Genetic effects",
       x = "Model") +
  guides(color = "none" )

