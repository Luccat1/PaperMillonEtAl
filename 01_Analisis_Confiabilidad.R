# ==============================================================================
# SCRIPT DE ANÁLISIS DE CONFIABILIDAD INTEREVALUADOR
# ==============================================================================
# Este script realiza un análisis estadístico para evaluar qué tan de acuerdo están
# dos evaluadores (codificadores) al analizar las mismas entrevistas.
#
# Se calculan varias métricas:
# 1. Kappa de Cohen: Medida estándar de acuerdo para dos evaluadores.
# 2. Índices de Prevalencia y Sesgo: Para entender por qué el Kappa puede ser bajo.
# 3. PABAK: Kappa ajustado por prevalencia y sesgo.
# 4. Alfa de Krippendorff Global: Una medida general para todo el sistema de códigos.
# ==============================================================================

# --- 1. CARGA DE LIBRERÍAS ---
# Las librerías son colecciones de funciones extra que no vienen en el R básico.
library(readxl) # Para leer archivos de Excel (.xlsx)
library(dplyr) # Para manipular datos (filtrar, seleccionar, crear columnas)
library(tidyr) # Para reorganizar datos (de formato largo a ancho y viceversa)
library(irr) # Para calcular coeficientes de confiabilidad (Kappa, Krippendorff)
library(purrr) # Para aplicar funciones a listas de elementos (iterar)
library(ggplot2) # Para crear gráficos de alta calidad

# --- 2. CARGA DE DATOS ---
# Leemos el archivo Excel que contiene los datos de presencia/ausencia de códigos.
# Se asume que la hoja "presencia_largo" tiene una fila por cada código en cada unidad de análisis.
datos <- read_excel("INE_VE_PresenciaBinaria.xlsx", sheet = "presencia_largo")

# --- 3. LIMPIEZA Y PREPARACIÓN DE DATOS ---
# Aseguramos que R entienda qué tipo de dato es cada columna.
# - as.factor: Convierte texto a "categoría" (útil para grupos).
# - as.integer: Convierte a números enteros.
datos <- datos %>%
  mutate(
    Entrevista = as.factor(Entrevista), # ID de la entrevista
    Pregunta   = as.integer(Pregunta), # Número de pregunta
    Coder      = factor(Coder), # Identificador del codificador (ej. J, P)
    Code_full  = factor(Code_full), # Nombre completo del código
    Unidad     = factor(Unidad) # Unidad de análisis única (Entrevista + Pregunta)
  )

# Creamos una lista de todas las unidades únicas y todos los códigos únicos
unidades <- datos %>% distinct(Entrevista, Pregunta, Unidad)
codigos <- levels(datos$Code_full)

# --- 4. FUNCIÓN PARA CREAR MATRIZ DE ACUERDO ---
# Esta función toma un código específico y crea una tabla donde:
# - Cada fila es una unidad de análisis.
# - Las columnas son los dos codificadores (J y P).
# - Las celdas tienen 1 (presencia) o 0 (ausencia).
matriz_codigo <- function(cod) {
  df_cod <- datos %>%
    filter(Code_full == cod) %>% # Filtramos solo los datos de ESTE código
    # Unimos con todas las unidades posibles para asegurar que no falten datos
    # (si un codificador no marcó nada, debe aparecer como 0, no desaparecer)
    right_join(unidades %>% tidyr::crossing(Coder = levels(datos$Coder)),
      by = c("Entrevista", "Pregunta", "Unidad", "Coder")
    ) %>%
    mutate(Presencia = ifelse(is.na(Presencia), 0L, Presencia)) %>% # Convertimos NAs a 0
    arrange(Entrevista, Pregunta, Unidad, Coder)

  # Transformamos a formato "ancho": una columna para cada codificador
  wide <- df_cod %>%
    select(Unidad, Coder, Presencia) %>%
    pivot_wider(names_from = Coder, values_from = Presencia)

  # Devolvemos solo las columnas de los codificadores (J y P)
  wide[, c("J", "P")]
}

# --- 5. CÁLCULO DE MÉTRICAS POR CÓDIGO ---
# Aquí iteramos sobre CADA código de la lista 'codigos' y calculamos sus estadísticas.
# 'map_dfr' aplica la función a cada código y une los resultados en una tabla.
res_kappa <- map_dfr(codigos, \(cod) {
  mat <- matriz_codigo(cod) # Obtenemos la matriz de 1s y 0s para este código

  # A. Kappa de Cohen
  # suppressWarnings evita que R se queje si hay acuerdo perfecto (varianza 0)
  k <- suppressWarnings(kappa2(mat, weight = "unweighted"))

  # B. Tabla de Contingencia 2x2
  # Contamos cuántas veces coincidieron o discreparon los codificadores.
  # c1 y c2 son las columnas de decisiones de cada codificador.
  c1 <- mat[[1]]
  c2 <- mat[[2]]

  # a: Ambos dijeron SÍ (1, 1) -> Acuerdo positivo
  a <- sum(c1 == 1 & c2 == 1)
  # b: Codificador 1 dijo SÍ, Codificador 2 dijo NO (1, 0) -> Desacuerdo
  b <- sum(c1 == 1 & c2 == 0)
  # c: Codificador 1 dijo NO, Codificador 2 dijo SÍ (0, 1) -> Desacuerdo
  c <- sum(c1 == 0 & c2 == 1)
  # d: Ambos dijeron NO (0, 0) -> Acuerdo negativo
  d <- sum(c1 == 0 & c2 == 0)

  n <- nrow(mat) # Total de unidades analizadas

  # C. Índices Avanzados (Byrt et al., 1993)
  # Prevalence Index (PI): Mide qué tan desbalanceada es la presencia del código.
  # Si PI es alto, el código aparece muy poco o mucho, lo que baja el Kappa artificialmente.
  pi_val <- abs(a - d) / n

  # Bias Index (BI): Mide si un codificador tiende a marcar "SÍ" más que el otro.
  bi_val <- abs(b - c) / n

  # PABAK (Prevalence-Adjusted Bias-Adjusted Kappa):
  # Es el Kappa corregido por los problemas de prevalencia y sesgo.
  # A veces es una mejor medida de la "verdadera" capacidad de acuerdo.
  po <- (a + d) / n # Acuerdo Observado (proporción de coincidencias)
  pabak <- 2 * po - 1

  # Acuerdo Esperado (Pe): Probabilidad de coincidir por puro azar
  pe <- ((a + b) * (a + c) + (c + d) * (b + d)) / (n^2)

  # D. Intervalo de Confianza (Bootstrap)
  # Como Kappa es una estimación, calculamos un rango donde probablemente esté el valor real.
  # Usamos "bootstrap": simulamos 1000 veces el estudio tomando muestras aleatorias de los datos.
  set.seed(123) # Fijamos semilla para que los resultados sean repetibles
  boot_k <- replicate(1000, {
    idx <- sample(seq_len(n), n, replace = TRUE) # Muestreo con reemplazo
    mat_boot <- mat[idx, ]
    suppressWarnings(kappa2(mat_boot, weight = "unweighted")$value)
  })

  # Tomamos los percentiles 2.5% y 97.5% para el IC del 95%
  ci_lower <- quantile(boot_k, 0.025, na.rm = TRUE)
  ci_upper <- quantile(boot_k, 0.975, na.rm = TRUE)

  # Guardamos todo en una fila de la tabla de resultados
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

# --- 6. INTERPRETACIÓN Y FORMATO ---
# Función para ponerle nombre al valor de Kappa según Landis & Koch (1977)
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

# Aplicamos formato y redondeo a la tabla final
reporte_kappa_extendido <- res_kappa %>%
  mutate(
    Interpretacion = map_chr(Kappa, interpret_kappa),
    # Redondeamos a 3 decimales para que se vea ordenado
    across(c(Kappa, Po, Pe, PI, BI, PABAK, CI_Lower, CI_Upper), ~ round(., 3))
  )

# --- 7. RESÚMENES GLOBALES ---

# Resumen estadístico general de todos los Kappas
resumen_global <- reporte_kappa_extendido %>%
  summarise(
    Media_Kappa   = mean(Kappa, na.rm = TRUE),
    Mediana_Kappa = median(Kappa, na.rm = TRUE),
    Min_Kappa     = min(Kappa, na.rm = TRUE),
    Max_Kappa     = max(Kappa, na.rm = TRUE)
  )

print("--- Resumen Global de Kappa ---")
print(resumen_global)

# Conteo de cuántos códigos caen en cada categoría de calidad
conteo_categorias <- reporte_kappa_extendido %>%
  count(Interpretacion) %>%
  arrange(desc(n))

print("--- Conteo por Categoría (Landis & Koch) ---")
print(conteo_categorias)

# --- 8. VISUALIZACIONES ---

# A. Histograma: ¿Cómo se distribuyen los valores de Kappa?
p1 <- ggplot(reporte_kappa_extendido, aes(x = Kappa)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
  labs(title = "Distribución de Kappa por Código", x = "Kappa de Cohen", y = "Frecuencia") +
  theme_minimal()

print(p1)

# B. Kappa vs Prevalencia (PI): La "Paradoja del Kappa"
# Muestra si los Kappas bajos se deben a que el código es muy raro o muy común.
# Si PI es alto (cerca de 1) y Kappa bajo, es probable que sea un artefacto estadístico.
p2 <- ggplot(reporte_kappa_extendido, aes(x = PI, y = Kappa)) +
  geom_point(alpha = 0.7, color = "darkred") +
  geom_smooth(method = "loess", se = FALSE, color = "gray") +
  labs(
    title = "Kappa vs Índice de Prevalencia (PI)",
    subtitle = "PI alto indica alta desproporción (código muy frecuente o muy raro).\nEsto tiende a penalizar el Kappa (Paradoja del Kappa).",
    x = "Índice de Prevalencia (PI)",
    y = "Kappa de Cohen"
  ) +
  theme_minimal()

print(p2)

# C. Kappa vs PABAK
# Compara el Kappa original con el ajustado.
# Puntos sobre la línea punteada indican que el ajuste mejoró el valor.
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

# Guardamos el reporte detallado en un archivo CSV
write.csv(reporte_kappa_extendido, "Reporte_Kappa_Extendido.csv", row.names = FALSE)
message("\nReporte extendido guardado como 'Reporte_Kappa_Extendido.csv'")

# --- 9. ALFA DE KRIPPENDORFF GLOBAL ---
# Esta métrica evalúa el acuerdo de TODO el sistema de códigos a la vez,
# no código por código. Es útil para dar un número único de la calidad del estudio.

# Preparamos los datos en formato matriz
datos_long <- datos %>%
  select(Entrevista, Pregunta, Unidad, Coder, Code_full, Presencia) %>%
  mutate(Item_ID = paste(Unidad, Code_full, sep = "_")) %>%
  select(Coder, Item_ID, Presencia)

matriz_kripp <- datos_long %>%
  pivot_wider(names_from = Item_ID, values_from = Presencia, values_fill = 0) %>%
  select(-Coder) %>%
  as.matrix()

# Calculamos el Alfa puntual
alpha_global <- kripp.alpha(matriz_kripp, method = "nominal")
print("--- Alfa de Krippendorff Global ---")
print(alpha_global)

# Bootstrap para el IC del Alfa Global
# (Esto puede tardar un poco, reducimos a 100 réplicas para el ejemplo)
unidades_unicas <- unique(datos$Unidad)
n_unidades <- length(unidades_unicas)

set.seed(123)
boot_alpha <- replicate(100, {
  unidades_boot <- sample(unidades_unicas, n_unidades, replace = TRUE)
  map_units <- tibble(Unidad = unidades_boot, ID_Replica = seq_along(unidades_boot))

  datos_boot <- datos %>%
    inner_join(map_units, by = "Unidad", relationship = "many-to-many") %>%
    mutate(Item_ID_Boot = paste(ID_Replica, Code_full, sep = "_")) %>%
    select(Coder, Item_ID_Boot, Presencia) %>%
    distinct(Coder, Item_ID_Boot, .keep_all = TRUE)

  mat_boot <- datos_boot %>%
    pivot_wider(names_from = Item_ID_Boot, values_from = Presencia, values_fill = 0) %>%
    select(-Coder) %>%
    as.matrix()

  if (ncol(mat_boot) < 2 || length(unique(as.vector(mat_boot))) < 2) {
    return(NA)
  }
  kripp.alpha(mat_boot, method = "nominal")$value
})

ci_alpha <- quantile(boot_alpha, c(0.025, 0.975), na.rm = TRUE)
cat(sprintf("Alfa Global: %.3f, IC 95%%: [%.3f, %.3f]\n", alpha_global$value, ci_alpha[1], ci_alpha[2]))

# --- 10. ANÁLISIS DE DESACUERDOS ---
# Identificamos códigos con buen Kappa pero que aún tienen desacuerdos, para ver dónde mejorar.
codigos_clave <- reporte_kappa_extendido %>% filter(Kappa >= 0.6)

if (nrow(codigos_clave) > 0) {
  datos_desacuerdo <- codigos_clave %>%
    mutate(
      Total_Unidades = a + b + c + d,
      Prop_Desacuerdo = (b + c) / Total_Unidades # % de veces que no estuvieron de acuerdo
    )

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

# --- 11. ANÁLISIS POR FAMILIAS ---
# Agrupamos los códigos en "Familias" o dimensiones para ver si algunas áreas son más difíciles de codificar.
# NOTA: Aquí usamos familias aleatorias de ejemplo. DEBES REEMPLAZAR ESTO con tus datos reales.
set.seed(42)
familias_map <- tibble(
  Codigo = codigos,
  Familia = sample(c("Evaluación", "Contenido", "Forma", "Contexto"), length(codigos), replace = TRUE)
)

# Unimos y resumimos
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

# Gráfico de cajas (Boxplot) para comparar familias
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

write.csv(resumen_familias, "Resumen_Familias.csv", row.names = FALSE)
