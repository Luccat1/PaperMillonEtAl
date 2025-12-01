library(readxl)
library(dplyr)
library(tidyr)
library(irr)
library(purrr)

datos <- read_excel("INE VE Segmented Data.xlsx", sheet = "presencia_largo")

# Asegurar tipos
datos <- datos %>%
  mutate(
    Entrevista = as.factor(Entrevista),
    Pregunta   = as.integer(Pregunta),
    Coder      = factor(Coder),
    Code_full  = factor(Code_full),
    Unidad     = factor(Unidad)
  )

unidades <- datos %>% distinct(Entrevista, Pregunta, Unidad)
codigos <- levels(datos$Code_full)

matriz_codigo <- function(cod) {
  df_cod <- datos %>%
    filter(Code_full == cod) %>%
    right_join(unidades %>% tidyr::crossing(Coder = levels(datos$Coder)),
      by = c("Entrevista", "Pregunta", "Unidad", "Coder")
    ) %>%
    mutate(Presencia = ifelse(is.na(Presencia), 0L, Presencia)) %>%
    arrange(Entrevista, Pregunta, Unidad, Coder)

  wide <- df_cod %>%
    select(Unidad, Coder, Presencia) %>%
    pivot_wider(names_from = Coder, values_from = Presencia)

  wide[, c("J", "P")]
}

res_kappa <- map_dfr(codigos, \(cod) {
  mat <- matriz_codigo(cod)

  # Calcular Kappa original
  k <- suppressWarnings(kappa2(mat, weight = "unweighted"))

  # Obtener tabla 2x2 (J vs P)
  # Asumimos columnas 1 y 2 son los codificadores
  c1 <- mat[[1]]
  c2 <- mat[[2]]

  # a: J=1, P=1
  a <- sum(c1 == 1 & c2 == 1)
  # b: J=1, P=0
  b <- sum(c1 == 1 & c2 == 0)
  # c: J=0, P=1
  c <- sum(c1 == 0 & c2 == 1)
  # d: J=0, P=0
  d <- sum(c1 == 0 & c2 == 0)

  n <- nrow(mat)

  # Índices de Byrt et al. (1993)
  # Prevalence Index (PI): |a - d| / n
  pi_val <- abs(a - d) / n

  # Bias Index (BI): |b - c| / n
  bi_val <- abs(b - c) / n

  # PABAK: 2 * Po - 1
  po <- (a + d) / n
  pabak <- 2 * po - 1

  # Acuerdo Esperado (Pe)
  pe <- ((a + b) * (a + c) + (c + d) * (b + d)) / (n^2)

  # Bootstrap para IC 95% de Kappa
  # Hacemos 1000 réplicas remuestreando las filas de la matriz
  set.seed(123) # Reproducibilidad
  boot_k <- replicate(1000, {
    idx <- sample(seq_len(n), n, replace = TRUE)
    mat_boot <- mat[idx, ]
    suppressWarnings(kappa2(mat_boot, weight = "unweighted")$value)
  })

  ci_lower <- quantile(boot_k, 0.025, na.rm = TRUE)
  ci_upper <- quantile(boot_k, 0.975, na.rm = TRUE)

  tibble(
    Code_full = cod,
    Kappa     = k$value,
    Po        = po,
    Pe        = pe,
    a         = a,
    b         = b,
    c         = c,
    d         = d,
    PI        = pi_val,
    BI        = bi_val,
    PABAK     = pabak,
    CI_Lower  = ci_lower,
    CI_Upper  = ci_upper
  )
})

# Función de interpretación de Landis & Koch (1977)
interpret_kappa <- function(k) {
  if (is.na(k)) {
    return("No calculable")
  }
  if (k < 0) {
    return("Pobre")
  }
  if (k <= 0.20) {
    return("Leve")
  }
  if (k <= 0.40) {
    return("Aceptable")
  }
  if (k <= 0.60) {
    return("Moderado")
  }
  if (k <= 0.80) {
    return("Sustancial")
  }
  return("Casi perfecto")
}

# Crear reporte extendido
reporte_kappa_extendido <- res_kappa %>%
  mutate(
    Interpretacion = map_chr(Kappa, interpret_kappa),
    # Redondear para presentación
    across(c(Kappa, Po, Pe, PI, BI, PABAK, CI_Lower, CI_Upper), ~ round(., 3))
  )

# --- 1. Resumen Global ---
resumen_global <- reporte_kappa_extendido %>%
  summarise(
    Media_Kappa   = mean(Kappa, na.rm = TRUE),
    Mediana_Kappa = median(Kappa, na.rm = TRUE),
    Min_Kappa     = min(Kappa, na.rm = TRUE),
    Max_Kappa     = max(Kappa, na.rm = TRUE)
  )

print("--- Resumen Global de Kappa ---")
print(resumen_global)

# --- 2. Conteo por Categoría Landis & Koch ---
conteo_categorias <- reporte_kappa_extendido %>%
  count(Interpretacion) %>%
  arrange(desc(n))

print("--- Conteo por Categoría (Landis & Koch) ---")
print(conteo_categorias)

# --- 3. Visualizaciones ---
library(ggplot2)

# A. Histograma de Kappa
p1 <- ggplot(reporte_kappa_extendido, aes(x = Kappa)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
  labs(title = "Distribución de Kappa por Código", x = "Kappa de Cohen", y = "Frecuencia") +
  theme_minimal()

print(p1)

# B. Kappa vs Prevalencia (PI)
# Comentario: La "paradoja del Kappa" ocurre cuando el Kappa es bajo a pesar de un alto acuerdo observado (Po).
# Esto suele suceder cuando la prevalencia es muy alta o muy baja (PI alto).
# En el gráfico, esperamos ver Kappas más bajos a medida que el PI aumenta (se aleja de 0).
p2 <- ggplot(reporte_kappa_extendido, aes(x = PI, y = Kappa)) +
  geom_point(alpha = 0.7, color = "darkred") +
  geom_smooth(method = "loess", se = FALSE, color = "gray") +
  labs(
    title = "Kappa vs Índice de Prevalencia (PI)",
    subtitle = "PI alto indica alta desproporción en la presencia del código (muy frecuente o muy raro).\nEsto tiende a penalizar el Kappa (Paradoja del Kappa).",
    x = "Índice de Prevalencia (PI)",
    y = "Kappa de Cohen"
  ) +
  theme_minimal()

print(p2)

# C. Kappa vs PABAK
# Comentario: PABAK ajusta el Kappa por prevalencia y sesgo.
# Si PABAK es mucho mayor que Kappa, sugiere que el Kappa bajo se debe a la prevalencia/sesgo
# y no necesariamente a un desacuerdo real.
p3 <- ggplot(reporte_kappa_extendido, aes(x = Kappa, y = PABAK)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Kappa vs PABAK",
    subtitle = "Puntos sobre la línea diagonal indican que PABAK > Kappa (ajuste por prevalencia).",
    x = "Kappa de Cohen",
    y = "PABAK (Prevalence-Adjusted Bias-Adjusted Kappa)"
  ) +
  theme_minimal()

print(p3)

# Exportar reporte extendido
write.csv(reporte_kappa_extendido, "Reporte_Kappa_Extendido.csv", row.names = FALSE)
message("\nReporte extendido guardado como 'Reporte_Kappa_Extendido.csv'")

# --- 4. Alfa de Krippendorff Global ---
# Asunción: Datos nominales, dos codificadores fijos, unidades independientes.
# Se considera "Global" al pool de todas las decisiones (Unidad x Código).

# Preparar datos: Matriz de Codificador (filas) x (Unidad + Código) (columnas)
datos_long <- datos %>%
  select(Entrevista, Pregunta, Unidad, Coder, Code_full, Presencia) %>%
  mutate(Item_ID = paste(Unidad, Code_full, sep = "_")) %>%
  select(Coder, Item_ID, Presencia)

# Asegurar que todos los items tengan datos para ambos coders (llenar con NA si falta, aunque idealmente 0)
# En este caso, asumimos que si no está en la base original pero es una unidad válida, es 0.
# Pero para Krippendorff necesitamos la matriz completa.
matriz_kripp <- datos_long %>%
  pivot_wider(names_from = Item_ID, values_from = Presencia, values_fill = 0) %>%
  select(-Coder) %>%
  as.matrix()

# Calcular Alfa Global Puntual
alpha_global <- kripp.alpha(matriz_kripp, method = "nominal")
print("--- Alfa de Krippendorff Global ---")
print(alpha_global)

# Bootstrap para IC 95% del Alfa Global
# Remuestreo por UNIDADES (Entrevista + Pregunta), no por celdas individuales, para mantener la estructura de dependencia.
unidades_unicas <- unique(datos$Unidad)
n_unidades <- length(unidades_unicas)

set.seed(123)
boot_alpha <- replicate(100, { # Usamos 100 para rapidez, aumentar a 1000 para reporte final
  # Muestrear unidades con reemplazo
  unidades_boot <- sample(unidades_unicas, n_unidades, replace = TRUE)

  # Construir dataset bootstrap
  # Nota: Esto puede ser lento con muchos datos.
  # Una forma eficiente es filtrar el long data.

  # Mapeo de unidades seleccionadas (puede haber duplicados, así que necesitamos un ID único por réplica)
  map_units <- tibble(Unidad = unidades_boot, ID_Replica = seq_along(unidades_boot))

  datos_boot <- datos %>%
    inner_join(map_units, by = "Unidad", relationship = "many-to-many") %>%
    mutate(Item_ID_Boot = paste(ID_Replica, Code_full, sep = "_")) %>%
    select(Coder, Item_ID_Boot, Presencia) %>%
    distinct(Coder, Item_ID_Boot, .keep_all = TRUE) # Asegurar unicidad

  mat_boot <- datos_boot %>%
    pivot_wider(names_from = Item_ID_Boot, values_from = Presencia, values_fill = 0) %>%
    select(-Coder) %>%
    as.matrix()

  # Manejar caso de matriz vacía o constante
  if (ncol(mat_boot) < 2 || length(unique(as.vector(mat_boot))) < 2) {
    return(NA)
  }

  kripp.alpha(mat_boot, method = "nominal")$value
})

ci_alpha <- quantile(boot_alpha, c(0.025, 0.975), na.rm = TRUE)
cat(sprintf("Alfa Global: %.3f, IC 95%%: [%.3f, %.3f]\n", alpha_global$value, ci_alpha[1], ci_alpha[2]))


# --- 5. Análisis de Desacuerdos ---
# Filtrar códigos clave (Kappa >= 0.6 o lo que se desee)
codigos_clave <- reporte_kappa_extendido %>%
  filter(Kappa >= 0.6)

if (nrow(codigos_clave) > 0) {
  datos_desacuerdo <- codigos_clave %>%
    mutate(
      Total_Unidades = a + b + c + d,
      Prop_Desacuerdo = (b + c) / Total_Unidades
    )

  # Gráfico Kappa vs Proporción de Desacuerdos
  p4 <- ggplot(datos_desacuerdo, aes(x = Kappa, y = Prop_Desacuerdo, label = Code_full)) +
    geom_point(color = "purple", size = 3) +
    geom_text(vjust = -0.5, size = 3, check_overlap = TRUE) +
    labs(
      title = "Kappa vs Proporción de Desacuerdos (Códigos Clave)",
      x = "Kappa de Cohen",
      y = "Proporción de Desacuerdos (b+c)/N"
    ) +
    theme_minimal()

  print(p4)
} else {
  message("No hay códigos con Kappa >= 0.6 para el análisis de desacuerdos.")
}

# --- 6. Análisis por Familias ---
# Crear mapeo dummy de familias (Reemplazar con datos reales)
set.seed(42)
familias_map <- tibble(
  Codigo = codigos,
  Familia = sample(c("Evaluación", "Contenido", "Forma", "Contexto"), length(codigos), replace = TRUE)
)

# Unir con reporte
resumen_familias <- reporte_kappa_extendido %>%
  left_join(familias_map, by = c("Code_full" = "Codigo")) %>%
  group_by(Familia) %>%
  summarise(
    N_Codigos = n(),
    Media_Kappa = mean(Kappa, na.rm = TRUE),
    Mediana_Kappa = median(Kappa, na.rm = TRUE),
    Min_Kappa = min(Kappa, na.rm = TRUE),
    Max_Kappa = max(Kappa, na.rm = TRUE),
    Rango_Kappa = Max_Kappa - Min_Kappa
  )

print("--- Resumen por Familias ---")
print(resumen_familias)

# Gráfico Boxplot por Familia
p5 <- ggplot(
  reporte_kappa_extendido %>% left_join(familias_map, by = c("Code_full" = "Codigo")),
  aes(x = Familia, y = Kappa, fill = Familia)
) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Distribución de Kappa por Familia de Códigos",
    x = "Familia",
    y = "Kappa de Cohen"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p5)

# Exportar resumen familias
write.csv(resumen_familias, "Resumen_Familias.csv", row.names = FALSE)
