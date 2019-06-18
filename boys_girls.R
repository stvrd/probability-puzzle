library(tidyverse)

n <- 20000

# Create n/2 families
fam <- as.numeric(runif(n)>0.5) %>%
  matrix(ncol = 2) %>%
  data.frame() %>% mutate(SUM = X1 + X2)

fam$SUM %>% table %>% barplot

# only choose the families which have at least one boy
fam  %>% filter(SUM>0) %>% select(SUM) %>% table %>% barplot

fam %>% filter(X1==1) %>% select(X2) %>% table %>% barplot
fam %>% filter(X2==1) %>% select(X1) %>% table %>% barplot
fam %>% filter(X2==1|X1 == 1) %>% transmute(SUM-1) %>%  table %>% barplot
