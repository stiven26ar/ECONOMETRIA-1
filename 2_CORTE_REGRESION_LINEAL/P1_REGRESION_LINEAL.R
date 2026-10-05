install.packages("readxl")
library(readxl)
datos <- read_excel("C:/Users/stive/OneDrive/Escritorio/Beta_estimado (1).xlsx")
head(datos)
View(datos)
#asigancion de variables 

X <- datos$`Tasa de interes`
Y <- datos$`Tasa de ahorro`
n <- length(X)

#promedios 

media_x <- mean(X)
media_y <- mean(Y)

#desviaciones (x_i = X_i - media_X, y_i = Y_i - media_y)

x_dev <- X - media_x
y_dev <- Y - media_y

#productos cuadrados de la desviacion 

xy_dev <- x_dev * y_dev
x_dev_sq <- x_dev^2
y_dev_sq <- y_dev^2

#ESTIMACION DE LA PENDIENTE (β2)

beta2 <-sum(xy_dev) / sum(x_dev_sq)

#ESTIMACION DEL INTERCEPTO (β1)

beta1 <- media_y - (beta2 * media_x)

cat("intecepto (beta1):", beta1, "\n")
cat("pendiente (beta2):", beta2, "\n")

#VALORES AJUSTADOS / PRIDICHOS

Y_hat <- beta1 + (beta2 * X)

#RESIDUOS (ERRORES)

e_i <- Y - Y_hat
e_i_sq <- e_i^2

#SUMA TOTAL DE CUADRADOS 

SST <- sum((Y - media_y)^2)

#SUMA DE CUADRADOS DEL MODELO 

SSR <- sum((Y_hat - media_y)^2)

#COEFICIENTE DE DETERMINACION 

R2 <- SSR / SST

#SUMA DE LOS CUADRADOS DE LOS ERRORES 

SSE <- sum(e_i_sq)

#TABLA ANOVA

gl_SSR <- 1
gl_SSE <- n -2
gl_SST <- n - 1

MS_SSR <- SSR / gl_SSR
MS_SSE <- SSE / gl_SSE

F_estadistico <- MS_SSR /MS_SSE

#CREAR TABLA ANOVA 

tabla_anova <- data.frame(Fuente = c("Modelo (SSR)", "Residuos (SSE)", "Total (SST)"), SS = c(SSR, SSE, SST), G_DE_L = c(gl_SSR, gl_SSE, gl_SST), CUADRADO_MEDIO = c(MS_SSE, MS_SSR, NA))
print(tabla_anova)
cat("F_estadistico:", F_estadistico, "/n")

#REGRESION LINEAL DIRECTA 

modelo <- lm(Y ~ X)
summary(modelo)

#Agregar las columnas de desviaciones, productos y residuos a la tabla original
datos$x_dev        <- x_dev         # (X - media_X)
datos$y_dev        <- y_dev         # (Y - media_Y)
datos$xy_dev       <- xy_dev        # (x * y)
datos$x_dev_sq     <- x_dev_sq      # (x^2)
datos$Y_estimado   <- Y_hat         # Y estimado (Y_hat)
datos$Residuo_e    <- e_i           # Residuos (e_i)
datos$Residuo_e_sq <- e_i_sq        # Residuos al cuadrado (e_i^2)

# Abrir la tabla actualizada en una pestaña
View(datos)




