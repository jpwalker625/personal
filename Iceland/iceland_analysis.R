library(forcats)
library(pander)
library(ggplot2)
library(pals)
library(ggthemes)

df <- read.csv("iceland_finances.csv")

df$Stage <- fct_recode(df$Stage, exclude = "Exclude", include = "Include")

df$Category <- fct_recode(df$Category, activity = "Activity", lodging = "Lodging", dining = "Dining",
                          merchandise = "Merchandise", travel = "Travel", 'gas/automotive' = "Gas/Automotive")


category_totals <- aggregate(list("Total.Cost" = df$Debit), by = list(Category = df$Category), sum)
type_totals <- aggregate(Debit ~ Category + Type, data = df, sum)

save.image(file = "category_totals.jpg", print(pander(category_totals)))

plot1 <- ggplot(data = df, aes(Category,Debit, fill = Category)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = Debit),position = 'stack')

my_pal <- brewer.paired(26)
plot2 <- ggplot(type_totals, aes(Category, Debit, fill = Type, label = paste(Type, Debit, sep = "-"))) +
  geom_bar(stat = 'identity') +
  geom_text(data = type_totals, size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = my_pal) +
  ggtitle("Iceland Vacation Expenses Breakdown") +
  theme_economist_white() +
  theme(axis.title = element_text(face = "bold"))
  
  
plot2
