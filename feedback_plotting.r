library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(ggpubr)

setwd("c:/Users/Peter/Dropbox/Teaching/")

data <- read_csv("19-05-29.csv",
                 col_types = cols(
                   Level = col_character(),
                   Subject = col_character(),
                   Useful = col_integer(),
                   Notes = col_integer(),
                   Knowlegeable = col_integer(),
                   Speed = col_integer()
                 ))

pad_theme = theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# The academic levels of the students
level <- data %>% ggplot(aes(x=factor(Level))) + 
  geom_bar(aes(fill=factor(Level)), show.legend=FALSE) + 
  labs(title="Academic level of student", x="") + 
  theme_minimal()
  pad_theme

# The Subjects of the students
subject <- data %>% filter(Subject!="-") %>% 
  ggplot(aes(x=Subject)) + 
  geom_bar(aes(fill=factor(Subject)), show.legend=FALSE) + 
  labs(title="Subject area of student", x="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust=0.5)) + 
  pad_theme

?theme_minimal

labels=c("Strongly\nDisgree", "Disagree", "Somewhat\nAgree", "Agree", "Strongly\nAgree")

# Was the course useful?
p1 <- data %>%  ggplot(aes(x=factor(Useful, levels=1:5, labels=labels))) + 
  geom_bar(aes(fill=Useful), show.legend=FALSE) + 
  scale_x_discrete(drop=F) +
  labs(title="The training will be useful", x="", y="Count") + 
  theme_minimal() + 
  pad_theme

# Were the notes usful?
p2 <- data %>%  ggplot(aes(x=factor(Notes, levels=1:5, labels=labels))) + 
  geom_bar(aes(fill=Notes), show.legend=FALSE) + 
  scale_x_discrete(drop=F) +
  labs(title="The notes were clear", x="", y="Count") + 
  theme_minimal() +
  pad_theme

#Was the trainer knowlegeable?
p3 <- data %>%  ggplot(aes(x=factor(Knowlegeable, levels=1:5, labels=labels))) + 
  geom_bar(aes(fill=Knowlegeable), show.legend=FALSE) + 
  scale_x_discrete(drop=F) +
  labs(title="The trainer was knowledgeable", x="", y="Count") + 
  theme_minimal() + 
  pad_theme


speed_labels=c("Too Slow", "A bit slow", "Just right", "A bit fast", "Too fast")
# The speed of the deliver was?
p4 <- data %>%  ggplot(aes(x=factor(Speed, levels=1:5, labels=speed_labels))) + 
  geom_bar(aes(fill=Speed), show.legend=FALSE) + 
  scale_x_discrete(drop=F) +
  labs(title="The speed of the delivery was:", x="", y="Count") + 
  theme_minimal() +
  pad_theme

grid <- grid.arrange(subject, level, p1, p2, p3, p4, 
             nrow = 3,
             top = text_grob("R Training Session Feedback - 29/05/19", size=16))

ggsave("feedback.pdf", grid, height=10, width=8)
