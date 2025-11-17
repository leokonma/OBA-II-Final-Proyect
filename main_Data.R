# ================================================================
# main.R — Preparación de datos para optimización y simulación
# ================================================================

# ==== 0) Librerías ====
library(igraph)

# ==== 1) Directorio de trabajo ====
setwd("C:/Users/leodo/OneDrive/Escritorio/optimization/third delivery/OBA-II-Final-Proyect")

# ==== 2) Parámetros base ====
N_OUTER       <- 11     # número de estaciones externas (clientes)
N_SURPLUS     <- 4      # número de estaciones con superávit
MIN_DELIVERED <- 35     # mínimo de unidades entregadas (para futuros métodos)
S0            <- 10     # stock inicial en el punto central
lambda        <- 1000   # parámetro de penalización (para futuras pruebas)
t_per         <- 2      # tiempo por bloque (minutos por unidad de distancia)

# ==== 3) Cargar módulos ====
source("stations.R")       # funciones: abbr(), make_mapping()
source("graph_utils.R")    # funciones: make_graph_with_weights()
source("demand_supply.R")  # funciones: build_demand_supply()
source("plot_utils.R")     # funciones: plot_network()

# ==== 4) Lista completa de estaciones ====
all_names <- c(
  "CIVIVOX MENDILLORRI","C/ Concejo de Sarriguren 2","MENDILLORRI ALTO","C/ Concejo Sagaseta 16",
  "CONSTRUCCIONES LAMBERTO ITURRAMA","Iturrama 14","PIO XII - SANCHO EL FUERTE","Avda. Pio XII 13",
  "AVENIDA MARCELO CELAYETA","Avda. Marcelo Celayeta 64 (Biblioteca)","AVDA. VILLAVA","Avda. de Villava 40 (Biblioteca)",
  "C/ AOIZ","C/ Aoiz 37 (Centro de Salud Ensanche)","PLAZA COMPAÑIA","Plaza Compañia",
  "BUZTINTXURI CENTRO DE SALUD","Avda. Gipuzkoa 13","ERRIPAGAÑA","C/ Londres 12",
  "PLAZA TOROS","C/ Amaya 1","C/ SANGÜESA","C/ Sangüesa 23",
  "C/ ERMITAGAÑA - AVDA. NAVARRA","C/ Ermitagaña 3 - Avda. Navarra","C/ ESQUIROZ","C/ Esquiroz 2 - Avda. Sancho El Fuerte",
  "C/ MONASTERIO DE LA OLIVA","C/ Monasterio de la Oliva 35 (P. Asunción)","AZPILAGAÑA","C/ Rio Alzania (Plaza Manuel Turrillas)",
  "PASEO ANELIER","C/ Ochagavía 20","RENFE","C/ Muelle 6",
  "CARLOS III - LEIRE","Avda. Carlos III 18","ANTONIUTTI","Parque Antoniutti",
  "C/ BENJAMÍN DE TUDELA","C/ Benjamín de Tudela 2","CENTRO MATERNALIA","",
  "LEZKAIRU (C/ MARÍA LACUNZA I)","C/ Maria Lacunza 9","AVDA. DE BAYONA","Plaza Obispo Irurita 1",
  "ERMITAGAÑA","C/ Ermitagaña 31 (Ikastola Jaso)","PLAZA SAN FRANCISCO","C/ Ansoleaga 26",
  "RINCÓN DE LA ADUANA","C/ Nueva (Frente al parking R. Aduana)","LEZKAIRU (C/ CATALUÑA)","C/ Cataluña 20",
  "BUZTINTXURI - AVDA. GIPUZKOA","Avda. Gipuzkoa 34","C/ OLITE","C/ Olite 32",
  "PIO XII (LARRAONA)","Avda. Pio XII 45 (Colegio Larraona)","HOSPITALES","C/ Irunlarrea (Especialidades CHN)",
  "TEATRO GAYARRE","Avda. Carlos III 5","UPNA","C/ Cataluña (Parking)",
  "CARLOS III - GORRITI","Avda. Carlos III 59","MENDILLORRI BAJO","C/ Concejo de Ardanaz - C/ Señorío de Echalaz",
  "C/ TUDELA","C/ Tudela 1","LEZKAIRU (C/ MUTILVA ALTA)","C/ Mutilva Alta - C/ José Manuel Baena",
  "UNIVERSIDAD DE NAVARRA","UNAV - Explanada edificio Periodismo","TXANTREA SUR","C/ Cascante 4",
  "PLAZA DE LA CRUZ","C/ San Fermín","C/ SANDUZELAI","C/ Sanduzelai 19 (Centro de salud San Jorge)",
  "YAMAGUCHI","Plaza Yamaguchi 1","ETXABAKOITZ NORTE","C/ Remiro de Goñi 42",
  "ITURRAMA - FUENTE DEL HIERRO","Fuente del Hierro 17","JUSTICIA","Calle Cuesta de la Reina, 9, 31011 Pamplona",
  "SAN JORGE","Plaza Doctor Ortiz de Landazuri","BUZTINTXURI","C/ Santos Ochandategui 50, C/ Ventura Rodríguez 50",
  "ROCHAPEA","C/Joaquín Beunza 43","TXANTREA","C/Canal 25 (Plaza Almiradio de Navascues)",
  "MENDILLORRI","Calle San Guillén, 1 – 31016 Pamplona","ENSANCHE/ZABALGUNEA","C/ Aoiz, 24 – 31004 Pamplona",
  "UNAV - (Parking Edificio Central)","Aparcamiento de Ismael Sanchez Bella","ETXABAKOITZ","C/ Virgen del Soto, 2-4, 31009 – Pamplona",
  "ROCHAPEA - CAPUCHINOS","Avda. Marcelo Celayeta 131 (C. Capuchinos)","LEZKAIRU (MARÍA LACUNZA II)","C/ María Lacunza 32",
  "MILAGROSA","C/ Blas de Laserna 56","TXANTREA – ORVINA III","C/ Miravalles 8 – C/ Fermín Daoiz",
  "TXANTREA-SUBIZA","Calle Subiza (Trasera Centro Salud)","HOSPITALES-BOTICARIO","Hospitales Calle Boticario Viñaburu",
  "PLAZAOLA","Calle Plazaola","TRINITARIOS","Paseo Trinitarios"
)

# ==== 5) Crear mapping (abreviaciones + punto central PT) ====
mp <- make_mapping(all_names, N_OUTER, PT = "PT", PT_name = "CENTRAL", seed = 123)
mapping    <- mp$mapping
outer_abbr <- mp$outer_abbr
PT         <- mp$PT

# ==== 6) Crear grafo con distancias Manhattan ====
gg <- make_graph_with_weights(outer_abbr, PT = PT)
g         <- gg$g
coords    <- gg$coords
manhattan <- gg$manhattan

# ==== 7) Generar demanda y superávit ====

# Crear demanda y superávit usando las estaciones externas (sin incluir PT)
ds <- build_demand_supply(
  outer_abbr = outer_abbr,
  N_OUTER = N_OUTER,
  N_SURPLUS = N_SURPLUS,
  PT = PT
)

# Asignar resultados a variables globales
D  <- ds$D   # si la función devuelve versión extendida
S  <- ds$S
d  <- ds$demand
s  <- ds$surplus
S0 <- S0     # mantenemos el stock inicial que ya venía de antes

