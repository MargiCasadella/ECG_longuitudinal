

#########################
# LLEGIM EL FITXER .HEA #
#########################

# Llegir el fitxer .hea com un fitxer de text
file_hea <- "ECGPCG0003.hea"
hea_lines <- readLines(file_hea)

# Mostrar les primeres línies del fitxer per veure'n l'estructura
head(hea_lines)



#########################
# LLEGIR EL FITXER .DAT #
#########################

# Ruta al fitxer .dat
file_dat <- "ECGPCG0003.dat"

# Llegir les dades binàries
n_mostres <- 240000  # Nombre total de mostres (segons la línia 1 del fitxer .hea)
n_canals <- 2  # Nombre de canals

# Llegir les dades binàries, assumint que les mostres són enteres de 2 bytes
dat <- readBin(file_dat, what = integer(), n = n_mostres * n_canals, size = 2, endian = "little")

# Reshape les dades per tenir una matriu amb 2 canals
dat_matrix <- matrix(dat, ncol = n_canals, byrow = TRUE)

# Veure les primeres files
head(dat_matrix)




#####################################################
# CONVERTIR LES DADES A UN DATAFRAME I AFEGIR TEMPS #
#####################################################

# Freqüència de mostreig
sampling_rate <- 8000  # Com s'indica a la línia 1 del fitxer .hea

# Crear el temps en segons
temps <- seq(0, (n_mostres - 1)) / sampling_rate

# Crear un dataframe amb les dades i el temps
df <- data.frame(time = temps, ECG = dat_matrix[, 1], PCG = dat_matrix[, 2])

# Veure les primeres files
head(df)



##############################
# VISUALITZACIÓ DE LES DADES #
##############################

# Graficar les dades
plot(df$time, df$ECG, type = "l", col = "black", xlab = "Temps (segons)", ylab = "Amplitude", main = "Electrocardiograma (ECG)")

# Gràfica del PCG (no necessari)
plot(df$time, df$PCG, type = "l", col = "red", xlab = "Temps (segons)", ylab = "Amplitude", main = "Fonocardiograma (PCG)")



# Gràfica del ECG en 1 segon
plot(df$time[1:8000], df$ECG[1:8000], type = "l", col = "blue", xlab = "Temps (segons)", ylab = "Amplitude", main = "Electrocardiograma (ECG) 1 segon")



#######################################
# DATA.FRAME DE LES DADES NECESSÀRIES #
#######################################

data.ECG <- data.frame("time" = df$time,
                       "ECG" = df$ECG)

summary(data.ECG)



######################################################
######################################################
######################################################


library(pracma)

# Trobar els pics R
peaks <- findpeaks(data.ECG$ECG, 
                   
                   # nombre mínim de valors creixents abans d'un pic
                   nups = 1, # el senyal ha de pujar almenys 2 valors abans del pic
                   
                   # nombre mínim de valors decreixents després d'un pic
                   ndowns = 1, # el senyal ha de baixar almenys 2 valors després del pic
                   
                   # valor mínim que ha de tenir un pic perquè sigui considerat un pic
                   minpeakheight = 20000,
                   
                   # distància mínima entre pics
                   minpeakdistance = 8000 * 0.5)  # 4000 mostres
                   
                  # threshold = mean(data.ECG$ECG))

# Extreure els temps dels pics R
R_times <- data.ECG$time[peaks[, 2]]


# Graficar el senyal ECG i marcar els pics R
plot(data.ECG$time, data.ECG$ECG, type = "l", col = "black", 
     xlab = "Temps (s)", ylab = "ECG", main = "Detecció de Pics R")
points(R_times, data.ECG$ECG[peaks[, 2]], col = "orange", pch = 19)


