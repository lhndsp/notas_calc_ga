set.seed(123)  # para reprodutibilidade

N <- seq(10, 1e4)     # número de simulações
Omega <- 1:6          

# Função que simula n jogadas e devolve a frequência da face 3
freq_face3 <- function(n) {
  mean(sample(Omega, n, replace = TRUE) == 3)
}

# Aplica a função a cada n de N (vetorizado)
f_n <- sapply(N, freq_face3)

# Gráfico
plot(N, f_n, type = "l", lwd = 2, col = "steelblue",
     main = "Frequência relativa da face 3",
     xlab = "Número de simulações",
     ylab = "Frequência observada")
abline(h = 1/6, col = "red", lty = 2)  # valor teórico
