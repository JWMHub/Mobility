# Line 1-19 for Figure 4: The proportion of variance explained by the first 10 PCs. ----
factors <- factors[factors$POP_DEN > 1000, ]
row.names(factors) <- factors[,2]
fac_pca <- prcomp(factors, scale = TRUE)
pca.data <- fac_pca$x[, 1:3]

vars <- (fac_pca$sdev)^2
props <- vars / sum(vars)
cumu.props <- cumsum(props)
var_scree <- data.frame(index=1:29, var=fac_pca$sdev^2, props=props, cumu.props=cumu.props)

ggplot(data = var_scree[1:10,]) + 
  geom_bar(mapping = aes(x = index, y = props), stat="identity", fill="steelblue") +
  geom_point(mapping = aes(x = index, y = cumu.props), size=3.5, col="#D55E00") + 
  geom_line(mapping = aes(x = index, y = cumu.props), col="#D55E00") + 
  scale_x_continuous(breaks = c(1:10))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2)) + 
  theme(text = element_text(size = 15)) +
  labs(title="",x = "PCs", y = "Proportion of variance explained")

# Line 22 for Supplementary Table S2: The variable loadings of the first 3 PCs ----
fac_pca$rotation[, 1:3]

# Line 25-30 for Supplementary Table S3: Regression associations between mobility patterns and the number of confirmed COVID 19 cases ----
Nb_u1 <- glm.nb(cases ~ scale_flow, data = data_frame)
Nb_u2 <- glm.nb(cases ~ scale_intraflow, data = data_frame)
Nb_u3 <- glm.nb(cases ~ scale_interflow, data = data_frame)
Nb_u4 <- glm.nb(cases ~ scale_intraflow + scale_interflow, data = data_frame)
summary(Nb_u1) # Nb_u2, Nb_u3, Nb_u4
pR2(Nb_u1) # Nb_u2, Nb_u3, Nb_u4

# Line 33-35 for Supplementary Table S4: Comparing coefficients of the sum of intra-district flow and inflow before and after the COVID 19 alert ----
Nb_p1 <- glm.nb(cases ~ scale_flow*period, data = data_frame)
summary(Nb_p1)
pR2(Nb_p1)

# Line 38-40 for Supplementary Table S5: Comparing coefficients of inflow before and after the COVID 19 alert ----
Nb_p2 <- glm.nb(cases ~ scale_interflow*period, data = data_frame)
summary(Nb_p2)
pR2(Nb_p2)

# Line 43-48 for Table 1 and Supplementary Table S7: Regression associations among mobility patterns, PCs, and the number of confirmed COVID 19 cases ----
Nb_m1 <- glm.nb(cases ~ scale_flow + PC1 + PC2 + PC3, data = data_frame)
Nb_m2 <- glm.nb(cases ~ scale_intraflow + PC1 + PC2 + PC3, data = data_frame)
Nb_m3 <- glm.nb(cases ~ scale_interflow + PC1 + PC2 + PC3, data = data_frame)
Nb_m4 <- glm.nb(cases ~ scale_intraflow + scale_interflow + PC1 + PC2 + PC3, data = data_frame)
summary(Nb_m1) # Nb_m2, Nb_m3, Nb_m4
pR2(Nb_m1) # Nb_m2, Nb_m3, Nb_m4

# Line 51-55 for Figure 6a and Supplementary Table S8: Regression associations among the intra-district flow of people aged 15-59 and ???60 years, PCs, and the number of confirmed COVID 19 cases ----
Nb_a1 <- glm.nb(cases ~ scale_intraflow_1559 + PC1 + PC2 + PC3, data = data_frame)
Nb_a2 <- glm.nb(cases ~ scale_intraflow_60 + PC1 + PC2 + PC3, data = data_frame)
Nb_a3 <- glm.nb(cases ~ scale_intraflow_1559 + scale_intraflow_60, data = data_frame)
summary(Nb_a1) # Nb_a2, Nb_a3
pR2(Nb_a1) # Nb_a2, Nb_a3

# Line 58-62 for Figure 6b and Supplementary Table S9: Regression associations among the inflow of people aged 15-59 and ???60 years, PCs, and the number of confirmed COVID 19 cases ----
Nb_a4 <- glm.nb(cases ~ scale_interflow_1559 + PC1 + PC2 + PC3, data = data_frame)
Nb_a5 <- glm.nb(cases ~ scale_interflow_60 + PC1 + PC2 + PC3, data = data_frame)
Nb_a6 <- glm.nb(cases ~ scale_interflow_1559 + scale_interflow_60, data = data_frame)
summary(Nb_a4) # Nb_a5, Nb_a6
pR2(Nb_a4) # Nb_a5, Nb_a6

