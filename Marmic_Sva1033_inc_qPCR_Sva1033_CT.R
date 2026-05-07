## calculate and plot Sva1033 qPCR data from marmic Sva1033 incubations Makarena 2026
# qPCR did not work properly, standard curve was very bad. try to make sense out of data just with CT values


# load packages ####
library(tidyverse)
library(readxl)
library(patchwork)

# import data ####
setwd("H:/Maka/Lab rotation/")

mdata <- read_tsv("Marmic_inc_Maka_DNA.conc.for.qPCR.txt") %>% 
  # calculate dilution of sample added into qPCR reaction
  mutate(dil = conc.ng.ul/DNA.qPCR.ng*2)

template <- read_tsv("mdu sva1033 qPCR template.txt", col_names = T, col_types = "ccccccccccccc") %>% 
  gather(key = "key", value = "Sample", -pcr.plate) %>% # reshapes into long format with 3 columns: pcr.plate with A,B,C, etc., key with 1-12 and Sample with actual sample name
  mutate(key = as.numeric(key)) %>%              # makes key column numeric
  arrange(pcr.plate, key) %>%                    # sorts so start with all A in pcr.plate column and 1-12 in key column and then only goes to B
  mutate(pcr.plate = paste0(pcr.plate, key)) %>% # pastes letter and number resulting in A1, A2 etc. in pcr.plate column
  select(Well.Position = pcr.plate, Sample)          # selects only new Well column, former pcr.plate column and Sample column

results <- read_excel("mdu sva1033 qPCR results.xls", sheet = "Results", skip = 43, .name_repair = make.names)


# join data and calculate ####
## join results with template and metadata
join1 <- left_join(template, results, by = "Well.Position") %>% 
  # drop rows which have no entry for sample
  drop_na(Sample) %>% 
  left_join(mdata, by = "Sample")

calc1 <- join1 %>% 
  # reduce file to relevant columns
  select(Sample, CT, conc.ng.ul, DNA.qPCR.ng, dil) %>% 
  mutate(
    # add treatment column
    treatment = factor(case_when(Sample %in% c("F1 T3", "F2 T3", "F3 T3") ~ "F",
                                 Sample %in% c("FG1 T3", "FG2 T3", "FG3 T3") ~ "FG",
                                 Sample %in% c("FP1 T3", "FP2 T3", "FP3 T3") ~ "FP",
                                 Sample %in% c("L1 T3", "L2 T3", "L3 T3") ~ "L", 
                                 Sample %in% c("LP1 T3", "LP2 T3", "LP3 T3") ~ "LP",
                                 Sample == "Inoculum" ~ "Inoculum"),
                       levels =  c("Inoculum", "F", "FG", "FP", "L", "LP")),
    # nicer sample names
    sample.names = gsub(" T3", "", Sample),
    
    # calculate 1/CT to get larger numbers for lower CT values. set "Undetermined" to 0
    CT_rev = if_else(CT == "Undetermined", 0,
                     1/as.numeric(CT)),
    # calculate in the dilution
    CT_rev_dil = CT_rev * dil,
    # calculate in how much DNA was supplied to reaction
    CT_rev_DNA = CT_rev/DNA.qPCR.ng) %>% 
  arrange(treatment)

# calculate mean and sd per sample
calc2 <- calc1 %>% 
  group_by(Sample) %>% 
  summarise(mean = mean(CT_rev_dil), 
            sd = sd(CT_rev_dil))

# plot ####

bp_rev_CT_dil <- ggplot(calc1, aes(color = treatment)) +
  geom_boxplot(aes(x = sample.names, y = CT_rev_dil)) +
  labs(title = "1/CT considering sample dilution",
       x = "Samples",
       y = "1/CT (dilution)") +
  theme_bw()

bp_DNA.conc <- ggplot(calc1, aes(color = treatment)) +
  geom_boxplot(aes(x = sample.names, y = conc.ng.ul)) +
  labs(title = "DNA concentration ng/µL",
       x = "Samples",
       y = "ng/µL") +
  theme_bw()


comb_plot <- bp_rev_CT_dil + bp_DNA.conc + guide_area() +
  plot_layout(guides = "collect", nrow = 2) &
  theme(legend.box = "horizontal", 
        axis.text = element_text(size = 5),
        plot.title = element_text(size = 10))

comb_plot

ggsave("Marmic_Sva1033inc_qPCR_Sva1033_CT.values.png", comb_plot, width = 25, height = 15, units = "cm", dpi = 600)

