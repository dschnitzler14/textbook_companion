fruit <- read.csv("/Users/Danny_1/Library/CloudStorage/OneDrive-MSBMedicalSchoolBerlin/Projects/shiny_textbook_companion/Textbook_Companion_op2/Fruit.csv")


boxplot(Size ~ Fruit, data = fruit)
stripchart(Size ~ Fruit, data = fruit, add = TRUE, vertical = TRUE,
           method = "jitter", pch = 21, col = "maroon", bg = "bisque")

hist(fruit)