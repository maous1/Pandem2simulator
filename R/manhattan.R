# library(tibble)
#
# test = datavaleur
# test$Pr...z..= p.adjust(p = datavaleur$Pr...z.., method = "BH")
# test %>% filter(Pr...z..< 0.05)
#
# test = datavaleur %>% rownames_to_column(var = "pos_nucl")%>% mutate(position = as.numeric(gsub("[^0-9]", "", pos_nucl))) %>%
#   mutate(gene = case_when(
#     between(x = position,left = 266,right = 13483) ~ "ORF1a",
#     between(x = position,left = 13484,right = 21555) ~ "ORF1b",
#     between(x = position,left = 21563,right = 25384) ~ "ORF2 (s)",
#     between(x = position,left = 25393,right = 26281) ~ "ORF3",
#     between(x = position,left = 26245,right = 26472) ~ "ORF4",
#     between(x = position,left = 26523,right = 27191) ~ "ORF5",
#     between(x = position,left = 27202,right = 27387) ~ "ORF6",
#     between(x = position,left = 27394,right = 27887) ~ "ORF7",
#     between(x = position,left = 27894,right = 28259) ~ "ORF8",
#     between(x = position,left = 28274,right = 29533) ~ "ORF9",
#     between(x = position,left = 29558,right = 29674) ~ "ORF10",
#     TRUE ~ "Other"
#   ))
#
# p <- ggplot(test, aes(position,-log(Pr...z..,base = 10),colour = gene)) +
#   geom_point() + ggtitle("Manhattan plot")+ xlab("position") + ylab("-log(P valeur)")
# p
#
# test = test %>% filter(-log(Pr...z..)>75)
