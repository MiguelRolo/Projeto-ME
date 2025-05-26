# Testes de Ajustamento do Qui-Quadrado

# Carregar dados
library(readr)
student_mat_v2 <- read_delim("student-mat-v2.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

# ==============================================================================
# 1. TESTE PARA DISTRIBUIÇÃO DISCRETA: POISSON (Faltas - absences)
# ==============================================================================

# Dados observados
absences_data <- student_mat_v2$absences

# Estatísticas descritivas
cat("Estatísticas das Faltas:\n")
cat("Média:", mean(absences_data), "\n")
cat("Variância:", var(absences_data), "\n")
cat("Mínimo:", min(absences_data), "\n")
cat("Máximo:", max(absences_data), "\n")

# Estimar parâmetro lambda da Poisson (média)
lambda_est <- mean(absences_data)

# Agrupar classes para garantir frequências esperadas >= 5
# Vamos usar classes: 0, 1, 2, 3, 4+
absences_grouped <- ifelse(absences_data >= 4, 4, absences_data)
freq_obs_grouped <- table(absences_grouped)

# Frequências esperadas sob distribuição Poisson
n <- length(absences_data)
classes <- as.numeric(names(freq_obs_grouped))
freq_esp_poisson <- numeric(length(freq_obs_grouped))

for(i in 1:(length(classes)-1)) {
  k <- classes[i]
  freq_esp_poisson[i] <- n * dpois(k, lambda_est)
}
# Última classe (4+) - probabilidade complementar P(X >= 4)
freq_esp_poisson[length(classes)] <- n * (1 - ppois(3, lambda_est))

names(freq_esp_poisson) <- names(freq_obs_grouped)

# Verificar condições de aplicabilidade
cat("\n=== VERIFICAÇÃO DAS CONDIÇÕES ===\n")
cat("Frequências esperadas:\n")
print(freq_esp_poisson)
cat("Frequências esperadas < 5:", sum(freq_esp_poisson < 5), "\n")
cat("Percentagem de freq. esperadas < 5:", round(100*sum(freq_esp_poisson < 5)/length(freq_esp_poisson), 1), "%\n")

# Teste Qui-Quadrado para Poisson
cat("\n=== TESTE DE AJUSTAMENTO À DISTRIBUIÇÃO POISSON ===\n")
cat("H0: Os dados seguem distribuição Poisson com λ =", round(lambda_est, 4), "\n")
cat("H1: Os dados não seguem distribuição Poisson\n\n")

print("Frequências Observadas vs Esperadas:")
comparison_poisson <- data.frame(
  Classe = names(freq_obs_grouped),
  Observada = as.numeric(freq_obs_grouped),
  Esperada = round(freq_esp_poisson, 2)
)
print(comparison_poisson)

# Calcular estatística qui-quadrado manualmente
qui_quad_stat <- sum((as.numeric(freq_obs_grouped) - freq_esp_poisson)^2 / freq_esp_poisson)

# Graus de liberdade corrigidos (classes - parâmetros estimados - 1)
gl_poisson <- length(freq_obs_grouped) - 1 - 1  # -1 parâmetro estimado (lambda)
p_value_poisson <- 1 - pchisq(qui_quad_stat, gl_poisson)

cat("\nResultados do Teste:\n")
cat("Estatística Qui-Quadrado:", round(qui_quad_stat, 4), "\n")
cat("Graus de Liberdade:", gl_poisson, "\n")
cat("P-value:", round(p_value_poisson, 4), "\n")
cat("Valor crítico (α=0.05):", round(qchisq(0.95, gl_poisson), 4), "\n")

if(p_value_poisson > 0.05) {
  cat("Conclusão (α=0.05): NÃO REJEITAMOS H0 - dados seguem distribuição Poisson\n")
} else {
  cat("Conclusão (α=0.05): REJEITAMOS H0 - dados não seguem distribuição Poisson\n")
}

# ==============================================================================
# 2. TESTE PARA DISTRIBUIÇÃO CONTÍNUA: NORMAL (Notas G1)
# ==============================================================================

# Dados observados (remover valores NA)
G1_data <- student_mat_v2$G1[!is.na(student_mat_v2$G1)]

# Estatísticas descritivas
cat("\n\nEstatísticas das Notas G1:\n")
cat("Média:", round(mean(G1_data), 4), "\n")
cat("Desvio Padrão:", round(sd(G1_data), 4), "\n")
cat("Mínimo:", min(G1_data), "\n")
cat("Máximo:", max(G1_data), "\n")

# Estimar parâmetros da Normal
mu_est <- mean(G1_data)
sigma_est <- sd(G1_data)

# Criar classes para o teste - usar intervalos baseados nos quartis
breaks_G1 <- quantile(G1_data, probs = c(0, 0.25, 0.50, 0.75, 1.0))
breaks_G1[1] <- breaks_G1[1] - 0.1  # Ajustar limite inferior
breaks_G1[length(breaks_G1)] <- breaks_G1[length(breaks_G1)] + 0.1  # Ajustar limite superior

# Agrupar dados em classes
G1_classes <- cut(G1_data, breaks = breaks_G1, include.lowest = TRUE)
freq_obs_G1 <- table(G1_classes)

# Frequências esperadas sob distribuição Normal
n_G1 <- length(G1_data)
freq_esp_normal <- numeric(length(freq_obs_G1))

for(i in 1:length(freq_obs_G1)) {
  lower <- breaks_G1[i]
  upper <- breaks_G1[i+1]
  prob <- pnorm(upper, mu_est, sigma_est) - pnorm(lower, mu_est, sigma_est)
  freq_esp_normal[i] <- n_G1 * prob
}

# Verificar condições de aplicabilidade
cat("\n=== VERIFICAÇÃO DAS CONDIÇÕES ===\n")
cat("Frequências esperadas:\n")
print(round(freq_esp_normal, 2))
cat("Frequências esperadas < 5:", sum(freq_esp_normal < 5), "\n")
cat("Percentagem de freq. esperadas < 5:", round(100*sum(freq_esp_normal < 5)/length(freq_esp_normal), 1), "%\n")

# Teste Qui-Quadrado para Normal
cat("\n=== TESTE DE AJUSTAMENTO À DISTRIBUIÇÃO NORMAL ===\n")
cat("H0: Os dados seguem distribuição Normal com μ =", round(mu_est, 2), "e σ =", round(sigma_est, 2), "\n")
cat("H1: Os dados não seguem distribuição Normal\n\n")

print("Frequências Observadas vs Esperadas:")
comparison_normal <- data.frame(
  Classe = names(freq_obs_G1),
  Observada = as.numeric(freq_obs_G1),
  Esperada = round(freq_esp_normal, 2)
)
print(comparison_normal)

# Calcular estatística qui-quadrado manualmente
qui_quad_stat_normal <- sum((as.numeric(freq_obs_G1) - freq_esp_normal)^2 / freq_esp_normal)

# Graus de liberdade corrigidos (classes - parâmetros estimados - 1)
gl_normal <- length(freq_obs_G1) - 2 - 1  # -2 parâmetros estimados (μ, σ)
p_value_normal <- 1 - pchisq(qui_quad_stat_normal, gl_normal)

cat("\nResultados do Teste:\n")
cat("Estatística Qui-Quadrado:", round(qui_quad_stat_normal, 4), "\n")
cat("Graus de Liberdade:", gl_normal, "\n")
cat("P-value:", round(p_value_normal, 4), "\n")
cat("Valor crítico (α=0.05):", round(qchisq(0.95, gl_normal), 4), "\n")

if(p_value_normal > 0.05) {
  cat("Conclusão (α=0.05): NÃO REJEITAMOS H0 - dados seguem distribuição Normal\n")
} else {
  cat("Conclusão (α=0.05): REJEITAMOS H0 - dados não seguem distribuição Normal\n")
}

# ==============================================================================
# 3. GRÁFICOS PARA VISUALIZAÇÃO
# ==============================================================================

# Configurar layout para múltiplos gráficos
par(mfrow=c(2,2))

# Gráfico 1: Histograma das faltas com distribuição Poisson teórica
hist(absences_data, breaks=0:(max(absences_data)+1), freq=FALSE, 
     main="Distribuição das Faltas vs Poisson Teórica", 
     xlab="Número de Faltas", ylab="Densidade",
     col="lightblue", border="white")
points(0:max(absences_data), dpois(0:max(absences_data), lambda_est), 
       col="red", pch=16, cex=1.2)
legend("topright", c("Observado", "Poisson Teórica"), 
       col=c("lightblue", "red"), pch=c(15, 16))

# Gráfico 2: Comparação frequências observadas vs esperadas (Poisson)
barplot(rbind(comparison_poisson$Observada, comparison_poisson$Esperada),
        beside=TRUE, col=c("lightblue", "red"),
        main="Frequências: Observadas vs Esperadas (Poisson)",
        xlab="Classes", ylab="Frequência",
        names.arg=comparison_poisson$Classe)
legend("topright", c("Observadas", "Esperadas"), 
       fill=c("lightblue", "red"))

# Gráfico 3: Histograma das notas com distribuição Normal teórica
hist(G1_data, freq=FALSE, breaks=15,
     main="Distribuição das Notas G1 vs Normal Teórica", 
     xlab="Notas G1", ylab="Densidade",
     col="lightgreen", border="white")
curve(dnorm(x, mu_est, sigma_est), add=TRUE, col="blue", lwd=2)
legend("topright", c("Observado", "Normal Teórica"), 
       col=c("lightgreen", "blue"), lwd=c(1, 2))

# Gráfico 4: Q-Q plot para Normal
qqnorm(G1_data, main="Q-Q Plot: Notas G1 vs Normal",
       pch=16, col="darkgreen")
qqline(G1_data, col="blue", lwd=2)

# Restaurar layout original
par(mfrow=c(1,1))

# ==============================================================================
# 4. RESUMO DOS RESULTADOS
# ==============================================================================

cat("\n\n=== RESUMO DOS TESTES DE AJUSTAMENTO ===\n")
cat("1. Faltas (absences) vs Distribuição Poisson:\n")
cat("   - Estatística χ²:", round(qui_quad_stat, 4), "\n")
cat("   - Graus de liberdade:", gl_poisson, "\n")
cat("   - P-value:", round(p_value_poisson, 4), "\n")
if(p_value_poisson > 0.05) {
  cat("   - Conclusão: NÃO REJEITAMOS H0 - Ajuste ADEQUADO\n\n")
} else {
  cat("   - Conclusão: REJEITAMOS H0 - Ajuste INADEQUADO\n\n")
}

cat("2. Notas G1 vs Distribuição Normal:\n")
cat("   - Estatística χ²:", round(qui_quad_stat_normal, 4), "\n")
cat("   - Graus de liberdade:", gl_normal, "\n")
cat("   - P-value:", round(p_value_normal, 4), "\n")
if(p_value_normal > 0.05) {
  cat("   - Conclusão: NÃO REJEITAMOS H0 - Ajuste ADEQUADO\n")
} else {
  cat("   - Conclusão: REJEITAMOS H0 - Ajuste INADEQUADO\n")
}

# ==============================================================================
# 5. EXEMPLO ADICIONAL: TESTE COM DADOS SIMULADOS (para demonstrar rejeição)
# ==============================================================================

cat("\n\n=== EXEMPLO ADICIONAL: DADOS QUE NÃO SEGUEM POISSON ===\n")

# Simular dados que claramente não seguem Poisson (distribuição uniforme)
set.seed(123)
dados_nao_poisson <- sample(0:10, size=200, replace=TRUE)  # Uniforme discreta

# Testar se seguem Poisson
lambda_sim <- mean(dados_nao_poisson)
freq_obs_sim <- table(dados_nao_poisson)
n_sim <- length(dados_nao_poisson)

# Calcular frequências esperadas
classes_sim <- as.numeric(names(freq_obs_sim))
freq_esp_sim <- n_sim * dpois(classes_sim, lambda_sim)

# Estatística qui-quadrado
qui_quad_sim <- sum((as.numeric(freq_obs_sim) - freq_esp_sim)^2 / freq_esp_sim)
gl_sim <- length(freq_obs_sim) - 1 - 1
p_value_sim <- 1 - pchisq(qui_quad_sim, gl_sim)

cat("Dados simulados (distribuição uniforme):\n")
cat("Testando H0: seguem Poisson com λ =", round(lambda_sim, 2), "\n")
cat("Estatística χ²:", round(qui_quad_sim, 4), "\n")
cat("P-value:", round(p_value_sim, 6), "\n")
if(p_value_sim > 0.05) {
  cat("Conclusão: NÃO REJEITAMOS H0\n")
} else {
  cat("Conclusão: REJEITAMOS H0 - como esperado!\n")
}