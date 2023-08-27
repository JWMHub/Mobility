# Line 2-21 for Figure 4: The proportion of variance explained by the first 10 PCs ----
factors <- read.csv(file = "socioeconomic_factors.csv")
factors <- factors[factors$POP_DEN > 1000, ]
factors <- factors[,-1:-3]
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

# Line 24 for Supplementary Table S2: The variable loadings of the first 3 PCs ----
fac_pca$rotation[, 1:3]

# Line 27-37 for Figure 5: Pearson correlations among the number of confirmed COVID 19 cases, intra-district flow, and district inflow ----
ggplot(data_frame, aes(x=cases, y=scale_intraflow, color=period, shape=period)) +
  geom_point(size= 10) + 
  geom_smooth(method=lm, se=T) + 
  scale_color_brewer(palette="Dark2") + 
  theme(text = element_text(size = 50), legend.position="none")

ggplot(data_frame, aes(x=cases, y=scale_interflow, color=period, shape=period)) +
  geom_point(size= 10) + 
  geom_smooth(method=lm, se=T) + 
  scale_color_brewer(palette="Dark2") + 
  theme(text = element_text(size = 50), legend.position="none")

# Line 40-45 for Supplementary Table S3: Regression associations between mobility patterns and the number of confirmed COVID 19 cases ----
Nb_u1 <- glm.nb(cases ~ scale_flow, data = data_frame)
Nb_u2 <- glm.nb(cases ~ scale_intraflow, data = data_frame)
Nb_u3 <- glm.nb(cases ~ scale_interflow, data = data_frame)
Nb_u4 <- glm.nb(cases ~ scale_intraflow + scale_interflow, data = data_frame)
summary(Nb_u1) # Nb_u2, Nb_u3, Nb_u4
pR2(Nb_u1) # Nb_u2, Nb_u3, Nb_u4

# Line 48-50 for Supplementary Table S4: Comparing coefficients of the sum of intra-district flow and inflow before and after the COVID 19 alert ----
Nb_p1 <- glm.nb(cases ~ scale_flow*period, data = data_frame)
summary(Nb_p1)
pR2(Nb_p1)

# Line 53-55 for Supplementary Table S5: Comparing coefficients of inflow before and after the COVID 19 alert ----
Nb_p2 <- glm.nb(cases ~ scale_interflow*period, data = data_frame)
summary(Nb_p2)
pR2(Nb_p2)

# Line 58-59 for Supplementary Table S6: Collinearity diagnostics between intra- and inter-district flows in the six periods ----
rcorr(as.matrix(data_frame[, c("scale_intraflow", "scale_interflow")]))
vif(Nb_u1) #Nb_u2, Nb_u3, Nb_u4

# Line 62-67 for Table 1 and Supplementary Table S7: Regression associations among mobility patterns, PCs, and the number of confirmed COVID 19 cases ----
Nb_m1 <- glm.nb(cases ~ scale_flow + PC1 + PC2 + PC3, data = data_frame)
Nb_m2 <- glm.nb(cases ~ scale_intraflow + PC1 + PC2 + PC3, data = data_frame)
Nb_m3 <- glm.nb(cases ~ scale_interflow + PC1 + PC2 + PC3, data = data_frame)
Nb_m4 <- glm.nb(cases ~ scale_intraflow + scale_interflow + PC1 + PC2 + PC3, data = data_frame)
summary(Nb_m1) # Nb_m2, Nb_m3, Nb_m4
pR2(Nb_m1) # Nb_m2, Nb_m3, Nb_m4

# Line 70-74 for Figure 6a and Supplementary Table S8: Regression associations among the intra-district flow of people aged 15-59 and ???60 years, PCs, and the number of confirmed COVID 19 cases ----
Nb_a1 <- glm.nb(cases ~ scale_intraflow_1559 + PC1 + PC2 + PC3, data = data_frame)
Nb_a2 <- glm.nb(cases ~ scale_intraflow_60 + PC1 + PC2 + PC3, data = data_frame)
Nb_a3 <- glm.nb(cases ~ scale_intraflow_1559 + scale_intraflow_60, data = data_frame)
summary(Nb_a1) # Nb_a2, Nb_a3
pR2(Nb_a1) # Nb_a2, Nb_a3

# Line 77-81 for Figure 6b and Supplementary Table S9: Regression associations among the inflow of people aged 15-59 and ???60 years, PCs, and the number of confirmed COVID 19 cases ----
Nb_a4 <- glm.nb(cases ~ scale_interflow_1559 + PC1 + PC2 + PC3, data = data_frame)
Nb_a5 <- glm.nb(cases ~ scale_interflow_60 + PC1 + PC2 + PC3, data = data_frame)
Nb_a6 <- glm.nb(cases ~ scale_interflow_1559 + scale_interflow_60, data = data_frame)
summary(Nb_a4) # Nb_a5, Nb_a6
pR2(Nb_a4) # Nb_a5, Nb_a6

