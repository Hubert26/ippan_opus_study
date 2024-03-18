
# Model dla zmiennej mediatora
model.m <- lm(Cortisol_ng_ml ~ 1, data = esm_cortisol_data_aggregated)

# Model dla zmiennej wynikowej
model.y <- lm(f1_PLEs_sum ~ Cortisol_ng_ml, data = esm_cortisol_data_aggregated)

# Wywołanie funkcji mediate()
mediacja <- mediate(model.m, model.y, sims = 1000, mediator = "Cortisol_ng_ml", data = esm_cortisol_data_aggregated)



db <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                 x1 = c(9, 8, 7, 6, 5, 4, 3, 2, 1),
                 x2 = c(9, 9, 7, 7, 5, 5, 3, 3, 1),
                 x3 = c(1, 1, 1, 1, 1, 1, 1, 1, 1))

model.M <- lm(x2 ~ x1 + x3 + y, db)  # Dodanie zmiennej y do modelu M
model.Y <- lm(y ~ x1 + x2 + x3, db)

results <- mediate(model.M, model.Y, treat = "x1", mediator = "x2", boot = TRUE, sims = 500)
summary(results)