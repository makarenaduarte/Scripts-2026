library(tidyverse)

df <- data.frame(
  Sample = c("FG1","FG2","FG3","FP1","FP2","FP3","L1","L2","L3",
             "F1","F2","F3","LP1","LP2","LP3"),
  T1 = c(2.7,0.0,49.5,0.0,0.0,0.0,29.3,37.3,30.3,0.0,0.0,0.0,81.9,87.6,79.1),
  T2 = c(129.1,186.9,143.7,22.5,253.5,251.7,262.1,269.7,273.0,
         238.4,237.9,238.8,254.8,354.2,326.2),
  T3 = c(444.9,472.5,577.7,54.6,651.5,585.1,468.1,632.8,499.2,
         436.1,536.2,696.3,194.2,449.7,634.2)
)

df_long <- df %>%
  pivot_longer(cols = starts_with("T"),
               names_to = "Time",
               values_to = "Value") %>%
  mutate(Day = recode(Time,
                      "T1" = 1,
                      "T2" = 19,
                      "T3" = 33))
df_long <- df_long %>%
  mutate(
    Treatment = str_extract(Sample, "^[A-Z]+"),
    Treatment = recode(Treatment,
                       "F"  = "Ferrihydrite",
                       "FG" = "Ferrihydrite ground",
                       "FP" = "Ferrihydrite pulses",
                       "L"  = "Lepidocrocite",
                       "LP" = "Lepidocrocite pulses"
    )
  )
df_long$Treatment <- factor(df_long$Treatment,
                            levels = c("Ferrihydrite",
                                       "Ferrihydrite ground",
                                       "Ferrihydrite pulses",
                                       "Lepidocrocite",
                                       "Lepidocrocite pulses"))
my_colors <- c(
  "Ferrihydrite" = "#1b9e77",
  "Ferrihydrite ground"  = "#e7298a",
  "Ferrihydrite pulses"  = "#7570b3",
  "Lepidocrocite"        = "#d95f02",
  "Lepidocrocite pulses" = "blue"
)

ggplot(df_long, aes(x = Day, y = Value, group = Sample)) +
  geom_line(alpha = 0.5) +
  geom_point() +
  labs(x = "Day", y = "Value", title = "All replicates over time") +
  theme_minimal()
df_mean <- df_long %>%
  group_by(Treatment, Day) %>%
  summarise(mean_value = mean(Value), .groups = "drop")

ggplot(df_mean, aes(x = Day, y = mean_value, color = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Day", y = "Mean Value") +
  theme_minimal()

df_summary <- df_long %>%
  group_by(Treatment, Day) %>%
  summarise(mean = mean(Value),
            sd = sd(Value),
            .groups = "drop")

ggplot(df_summary, aes(x = Day, y = mean, color = Treatment)) +
  geom_line(linewidth = 1, linetype = "dotted") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 1) +
  scale_x_continuous(
    breaks = c(1, 19, 33),   
    limits = c(0, 35),       
    expand = c(0, 0)         
  ) +
  labs(x = "Days", y = "Iron II µM") +
  theme_minimal()