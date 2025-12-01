# 📊 Análisis de Confiabilidad Interevaluador

Este repositorio contiene los datos derivados y los scripts de análisis utilizados para calcular la confiabilidad interevaluador en un estudio cualitativo basado en entrevistas semiestructuradas.

El objetivo principal es evaluar la consistencia en la aplicación de un sistema de códigos temáticos por parte de dos analistas.

## 🚀 Análisis Realizados

- **Kappa de Cohen** por código.
- **Índices de Prevalencia y Sesgo** (Byrt et al., 1993) + **PABAK**.
- **Resúmenes por familias** de códigos (dimensiones axiales).
- **Alfa de Krippendorff** nominal global (exploratorio).

> **Nota:** El artículo asociado tiene un enfoque principalmente cualitativo; estos análisis cuantitativos documentan la consistencia del sistema de codificación.

---

## 📂 Estructura del Repositorio

| Carpeta | Descripción |
| :--- | :--- |
| **`data/`** | Datos derivados de la codificación. |
| `INE_VE_PresenciaBinaria.csv` | Matriz binaria (0/1) por unidad de análisis. |
| `Reporte_Confiabilidad.csv` | Resumen básico: κ, Po, Pe. |
| `Reporte_Kappa_Extendido.csv` | Resumen detallado: κ, PI, BI, PABAK, IC 95%. |
| `Resumen_Familias.csv` | Estadísticos agregados por familias. |
| **`R/`** | Scripts de análisis. |
| `01_Analisis_Confiabilidad.R` | **Script principal**. Carga datos, calcula métricas y genera reportes. |
| **`output/`** | Resultados generados (tablas y gráficos). |

---

## 💻 Requisitos

- **R** (≥ 4.0)
- Paquetes necesarios:
  - `tidyverse`
  - `readxl`
  - `irr`
  - `krippendorff` (o equivalente)
  - `boot`
  - `ggplot2`

---

## 🔄 Reproducibilidad

### Opción A: Clonar desde GitHub

1. Clonar el repositorio:

    ```bash
    git clone https://github.com/Luccat1/PaperMillonEtAl.git
    ```

2. Abrir el proyecto en RStudio.
3. Ejecutar el script principal:

    ```r
    source("R/01_Analisis_Confiabilidad.R")
    ```

### Opción B: Descargar desde OSF

1. Descargar desde [OSF Project](https://osf.io/nyqj8/).
2. Descomprimir y abrir en RStudio.
3. Ejecutar `R/01_Analisis_Confiabilidad.R`.

---

## 🛡️ Datos y Privacidad

Los archivos en `data/` contienen **únicamente datos derivados** (presencia/ausencia de códigos).
*Las entrevistas originales no se incluyen para proteger la confidencialidad de los participantes, conforme a los protocolos éticos de la PUCV.*

---

## 📝 Cita Sugerida

Si utiliza estos recursos, por favor cite:

> Autor/es. (Año). *Confiabilidad interevaluador en la codificación de entrevistas* [Repositorio de datos y código]. OSF / GitHub. DOI / URL

---

## 📬 Contacto

Para dudas sobre el análisis:

**Luciano Cataldo Alvarado**  
📧 [luciano.cataldo@pucv.cl](mailto:luciano.cataldo@pucv.cl)  
📧 [lcataldoalvarado@gmail.com](mailto:lcataldoalvarado@gmail.com)
