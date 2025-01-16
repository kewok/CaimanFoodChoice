# @author: Sergio Balaguera-Reina

source('mvProbit.R')
library(tidyverse)
library(patchwork)

datamerged <- cbind(medem, expResult_noInt)
a <- datamerged %>%
  mutate(Subspecies = as.factor(Subspecies)) %>%
  ggplot(mapping = aes(x = Length, y = V1, colour = Subspecies, shape = Subspecies)) +
  geom_point(mapping = aes(shape = Subspecies)) +
  labs(y = "Pr(Arthropods in stomach)", x = "", subtitle = "A", color = "Subspecies") +
  scale_color_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c("red", "blue", "green", "purple")) +
  scale_shape_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c(15, 16, 17, 8)) +
  theme_bw()

datamerged_Int <- cbind(medem, expResult)
b <- datamerged_Int %>%
  mutate(Subspecies = as.factor(Subspecies)) %>%
  ggplot(mapping = aes(x = Length, y = V1, colour = Subspecies, shape = Subspecies)) +
  geom_point(mapping = aes(shape = Subspecies)) +
  labs(y = "", x = "", subtitle = "B", color = "Subspecies") +
  scale_color_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c("red", "blue", "green", "purple")) +
  scale_shape_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c(15, 16, 17, 8)) +
  theme_bw()

c <- datamerged %>%
  mutate(Subspecies = as.factor(Subspecies)) %>%
  ggplot(mapping = aes(x = Length, y = V3, colour = Subspecies, shape = Subspecies)) +
  geom_point(mapping = aes(shape = Subspecies)) +
  labs(y = "Pr(Fish in stomach)", x = "", subtitle = "C", color = "Subspecies") +
  scale_color_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c("red", "blue", "green", "purple")) +
  scale_shape_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c(15, 16, 17, 8)) +
  theme_bw()

d <- datamerged_Int %>%
  mutate(Subspecies = as.factor(Subspecies)) %>%
  ggplot(mapping = aes(x = Length, y = V3, colour = Subspecies, shape = Subspecies)) +
  geom_point(mapping = aes(shape = Subspecies)) +
  labs(y = "", x = "", subtitle = "D", color = "Subspecies") +
  scale_color_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c("red", "blue", "green", "purple")) +
  scale_shape_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c(15, 16, 17, 8)) +
  theme_bw()

e <- datamerged %>%
  mutate(Subspecies = as.factor(Subspecies)) %>%
  ggplot(mapping = aes(x = Length, y = V4, colour = Subspecies, shape = Subspecies)) +
  geom_point(mapping = aes(shape = Subspecies)) +
  labs(y = "Pr(Tetrapods in stomach)", x = "TL (cm)", subtitle = "E", color = "Subspecies") +
  scale_color_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c("red", "blue", "green", "purple")) +
  scale_shape_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c(15, 16, 17, 8)) +
  theme_bw()

f <- datamerged_Int %>%
  mutate(Subspecies = as.factor(Subspecies)) %>%
  ggplot(mapping = aes(x = Length, y = V4, colour = Subspecies, shape = Subspecies)) +
  geom_point(mapping = aes(shape = Subspecies)) +
  labs(y = "", x = "TL (cm)", subtitle = "F", color = "Subspecies") +
  scale_color_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c("red", "blue", "green", "purple")) +
  scale_shape_manual(labels = c(expression(italic("C. c. apaporiensis")), 
                                expression(italic("C. c. chiapasius")), 
                                expression(italic("C. c. fusus")),
                                expression(italic("C. c. crocodilus"))), 
                     values = c(15, 16, 17, 8)) +
  theme_bw()

(a + b) / (c + d) / (e + f) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("./Fig1_ggplot.jpeg", plot = last_plot(), height = 2000, width = 1700, dpi = 250, units = "px")

