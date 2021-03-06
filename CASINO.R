##################################
# DECISION MODELS
# Federico Comotto
# Massimiliano Scardovelli C.
# Gabriele Caldara
# Eugenio Andreose
################################
options(scipen = 999) #tolgo la notazione scientifica

library(rstudioapi)
#Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())

library(readxl)
slot = read_excel("Lucky Duck Entertainment revenue 2013.xls", na = c(".", "NA", "NaN"))

#alcuni pacchetti 
library(funModeling)
library(dplyr)
library(lubridate)

#status
status = df_status(slot, print_results = F)
summary(slot$Month) #12 mesi

###################
# CATEGORIZATION
##################
tot = slot %>%
  group_by(Casino, Denomination, MachineType, Section, Month) %>%
  summarise(numero_macchine = sum(NoMachines),
            ricavi_totali = sum(GrossRevenue),
            ricavo_unitario = ricavi_totali/numero_macchine,
            giocate_totali = sum(Plays),
            giocate_unitarie = round(giocate_totali/numero_macchine),
            ricavo_per_giocata = ricavi_totali/giocate_totali) %>%
  arrange(Month, Section, MachineType, Denomination) %>%
  mutate(tipo = paste0(MachineType, "_", Denomination))

tot = as.data.frame(tot)

#################
# EXPLORATION
################

ricavi_mese = tot %>% 
  group_by(Month) %>%
  summarise(ricavo_medio_totale = mean(ricavi_totali),
            ricavo_medio_unitario = mean(ricavo_unitario),
            numero_macchine_medie = mean(numero_macchine),
            varianza_macchine = sqrt(var(numero_macchine)))

ggplot(data=ricavi_mese, aes(x=ricavi_mese$Month, y = ricavi_mese$ricavo_medio_unitario)) +
  geom_line(alpha=.5, size=1, color="#880011") +
  ggtitle("Ricavi per Mese di LDE") +
  labs(x="Mese", y="Ricavo medio unitario") +
  theme_classic()
#NO ANDAMENTO LINEARE
#NB: le linee servono per sottolineare il fenomeno altalenante. Non esiste rilevazione infra mese.

ricavi_mese$Month = as.factor(ricavi_mese$Month)

ggplot(data=ricavi_mese, aes(x=ricavi_mese$numero_macchine_medie,
                             y=ricavi_mese$ricavo_medio_totale,
                             colour = Month)) +
  geom_point(alpha=.4, size=4) +
  ggtitle("Pi� macchine hai pi� guadagni? In linea di massima SI!") +
  labs(x="Numero medio macchine", y="Ricavi medi totali") + 
  theme_minimal()
#In generale vale pi� macchine hai pi� guadagni

ggplot(data=ricavi_mese, aes(x=ricavi_mese$numero_macchine_medie,
                             y=ricavi_mese$ricavo_medio_unitario,
                             colour = Month)) +
  geom_point(alpha=.4, size=4) +
  ggtitle("Tante macchine non significa ricavi unitari elevati!") +
  labs(x="Numero medio macchine", y="Ricavi medi unitari") + 
  theme_minimal()
#� comunque utile analizzare i ricavi indipendentemente dalla capienza, ergo i ricavi unitari.

ricavi_categoria = tot %>%
  group_by(Denomination, MachineType) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario)) %>%
  mutate(type = paste0(Denomination,sep = "_", MachineType))

ggplot(data=ricavi_categoria, aes(x=ricavi_categoria$type, y = ricavi_categoria$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Categoria di LDE") +
  labs(x="Categoria", y="Ricavo medio unitario") +
  theme_classic() #[QUESTO DEVE ESSERE UN BAR PLOT]

ricavi_sezione = tot %>% 
  group_by(Section) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario))

ggplot(data=ricavi_sezione, aes(x=ricavi_sezione$Section, y = ricavi_sezione$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Sezione di LDE") +
  labs(x="Sezione", y="Ricavo medio unitario") +
  theme_classic() #[QUESTO DEVE ESSERE UN BAR PLOT]

ricavi_casino = tot %>% 
  group_by(Casino) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario))

ggplot(data=ricavi_casino, aes(x=ricavi_casino$Casino, y = ricavi_casino$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Casino di LDE") +
  labs(x="Casino", y="Ricavo medio unitario") +
  theme_classic() #[QUESTO DEVE ESSERE UN BAR PLOT]

tot %>%
  group_by(tipo) %>%
  summarise(n = n()) %>%
  View()
#14 categorie al massimo 

tot %>% group_by(Casino, tipo, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(tipo, Section) %>%
  group_by(Section) %>%
  summarise(n = n()) %>%
  View()
#numero totale categorie per sezione di LDE nell'arco dei 12 mesi 

tot %>% group_by(Casino, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Casino, Section) %>%
  group_by(Casino, Section) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_numero_categorie = sqrt(var(n))) %>%
  View()
#numero medio di categorie per sezione nell'arco dei 12 mesi

tot %>% group_by(Casino, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Casino, Section) %>%
  group_by(Month) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_numero_categorie = sqrt(var(n))) %>%
  View()
#numero medio di categorie per mese

##########################
# MODELING: settembre
##########################
set = tot %>%
  filter(Month == ymd("2011-09-01")) %>%
  arrange(Casino)

set = as.data.frame(set)

f_obj = round(set$ricavo_unitario) #ricavi unitari = da MAX

#quante macchine ci sono ogni mese nei 2 casino? 
tot %>% 
  group_by(Casino, Month) %>%
  summarise(n_macchine = sum(numero_macchine)) %>%
  arrange(Month) %>%
  View()

#Vincolo 1: numero massimo di macchine per casino nel mese di settembre 
library(caret)
Vincolo1 =  function(df){
  "df = dataset del mese
  dummy + trasposizione"
  A = predict(dummyVars(~ Casino, data = df), newdata = df)
  A = t(A)
  return(A)
} #approccio valido se ho una sola colonna

A = Vincolo1(set)

#Vettore B del modello lineare
b = c(849, 230) #upper bound
constraints = c("<=", "<=") 

library(linprog)
sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints) #solver 

#risultato 1
summary(sol)
shadow_price = sol$con

#risultato sul dataset
set[which(sol$solution != 0),] %>%
  arrange(Casino, tipo) %>%
  View()
#Avendo solo questo vincolo le macchine vengo piazzate dove il ricavo unitario � massimo. Mi sembra ragionevole
#E'chiaro che occorre tenere in considerazione altri vincoli.

#Vincolo 2: proporzione macchine in ciascuna sezione dei 2 Casino
set %>%
  group_by(Casino, Section) %>%
  summarise(Num_macchine = sum(numero_macchine)) %>%
  mutate(prop_per_mese = round(Num_macchine / sum(Num_macchine), 2)) %>%
  View()

#Per Aries ciascuna sezione deve avere un numero di macchine: 0.2 >= x <= 0.3
#Per Libra ciascuna sezione deve avere un numero di macchine: 0.15 >= x <= 0.35
#Si tratta di valori arbitrari dedotti dai dati

#Matrice A:
#1) creo un vettore di 1 e di 0 -> dove le variabilid decisionali sono attive varr� 1 altrimenti 0;
#2) popolo la matrice A (modello lineare) attraverso il vettore creato
Vincolo2 = function(df){
  "df = dataset del mese"
  "funzione per costruire il vincolo 2"
  x = unique(df$Section)
  y = unique(df$Casino)
  a = c()
  for (t in 1:length(y)) {
    for (i in 1:length(x)) {
      for (k in 1:2) {
        a = append(a, ifelse(df$Section == x[i] & df$Casino == y[t], 1, 0))
      }
    }
  }
  A = matrix(a,16,78, byrow = T)
  return(A)
}

#Vincolo 2  
A1 = Vincolo2(set)

#Vincolo 1 e Vincolo 2 (Matrice A)
A = rbind(A, A1)

#Vettore B aggiornato 
b = c(b,
      rep(c(round(0.2*849), round(0.3*849)), 4), rep(c(round(0.15*230), round(0.35*230)), 4))

constraints = c(constraints,
                rep(c(">=", "<="), 8))

#Risultato 2
sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)
shadow_price = sol$con

#risultato sul dataset
set[which(sol$solution != 0),] %>%
  arrange(Casino, tipo) %>%
  View()

#forse le macchine con pi� giocate sono quelle pi� utilizzate?
ggplot(data=set, aes(x=set$giocate_totali, y = set$numero_macchine)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("a settembre, pi� macchine ci sono pi� si gioca? Si") +
  labs(x="Giocate Totali", y="Numero di Macchine") +
  theme_classic() #si

cor(set$giocate_totali,set$numero_macchine) #0.97  

#ma � vero per per tutto l'anno?
ggplot(data=tot, aes(x=tot$giocate_totali, y = tot$numero_macchine)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Pi� macchine ci sono pi� si gioca? Si") +
  labs(x="Giocate Totali", y="Numero di Macchine") +
  theme_classic() #si

cor(tot$giocate_totali, tot$numero_macchine) #0.93  

#forse il numero di giocate dipende dal wage minimo
ggplot(data=set, aes(x= set$Denomination, y=set$giocate_totali)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Pi� monete piccole puoi inserire pi� giochi? No!") +
  labs(x="Wage minimo", y="giocate totali") +
  theme_classic() #ni

cor(set$Denomination, set$giocate_totali) #bassa dipendenza lineare [non vale la pena]

#Vincolo 3: numero macchine di ciascuna categoria non nullo e in funzione delle giocate totali
#occorre discretizzare le giocate totali e poi settare valori upper e lower in funzione della categoria

#partiamo valutando se posso usare la medesima discretizzaztione per entrambi i casino
set$Casino = as.factor(set$Casino)

p = ggplot(set, aes(x=Casino, y=giocate_totali, fill = Casino)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  labs(y = "Giocate totali") +
  ggtitle("Distribuzione giocate totali settembre") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_dark()

data_summary <- function(x) {
  "funzione per mettere intervallo di confidenza attorno alla media"
  m = mean(x)
  ymin = t.test(x)$conf.int[1]
  ymax = t.test(x)$conf.int[2]
  return(c(y=m,ymin=ymin,ymax=ymax))
}

p + stat_summary(fun.data = data_summary, 
                 geom="pointrange",
                 color="red", 
                 size = 0.7)

t.test(set$giocate_totali ~ set$Casino) #distribuzioni significativamente diverse: non posso usare la congiunta

#distribuzione giocate totali dei 2 casin�
summary(set[set$Casino == "Aries", "giocate_totali"])
summary(set[set$Casino == "Libra", "giocate_totali"])

#utilizzo il pacchetto funmodeling per discretizzare
help("discretize_df")

#dataframe in input per la discretizzaztione: d_bins e d_bins1
#si pu� usare il comdando "discretize_get_bins" ma non permette il settaggio con i quantili
#il risultato sarebbe lo stesso daframe con valori chiaramenti diversi

d_bins = data_frame(variable = "giocate_totali",
                cuts = paste0("122810","|","601962","|","1665180","|","inf")) #da summary precedente [DA AUTOMATIZZARE]
aries_discrettizato = discretize_df(set %>% filter(Casino=="Aries"),
                                  data_bins = d_bins,
                                  stringsAsFactors = T)

d_bins1 = data_frame(variable = "giocate_totali",
                    cuts = paste0("80424","|","209740","|","617728","|","inf")) #da summary precedente [DA AUTOMATIZZARE]
libra_discrettizato = discretize_df(set %>% filter(Casino=="Libra"),
                                    data_bins = d_bins1,
                                    stringsAsFactors = T)
set_discretizzato = rbind(aries_discrettizato, libra_discrettizato) #creo nuovo dataset discretizzato

set$giocate_totali_discr = set_discretizzato$giocate_totali #aggiungo colonna discretizzata al dataset originale
set = set[c(1:9,13,10:12)] #riordino colonne per avere le categorie vicine ai valori discretizzati

#ora raggruppo per giocate_unitarie che sono state discretizzate e calcolo alcune statistiche per decidere la soglia
gruppi = set %>% 
  group_by(giocate_totali_discr, Casino) %>%
  summarise(media_macchine = mean(numero_macchine),
            massimo_macchine = max(numero_macchine),
            mediana_macchine = median(numero_macchine)) %>%
  arrange(Casino)
#utilizziamo il massimo numero di macchine per le soglie

Vincolo3 = function(df) {
  "funzione per creare la matrice
  A del quarto vincolo"
  matrice = diag(nrow(df)) #matrice diagonale della stessa grandezza delle variabili decisionali 
  matrice = cbind(matrice, as.character(df$giocate_totali_discr))
  matrice = as.data.frame(matrice)
  bin = as.character(sort(unique(df$giocate_totali_discr))) #vettore delle giocate totali discretizzate 
  aries = which(matrice[,79] == bin[4]) #memorizzo le categorie cui riga della matrice A non deve essere replicata poich�
  libra = which(matrice[,79] == bin[8]) #presentano solo un lower bound (1 vincolo e non 2)
  matrice1 = matrice[c(aries,libra),]
  matrice2 = matrice[-c(aries, libra),]
  matrice2 = matrice2[rep(seq_len(nrow(matrice2)), each= 2),] #raddoppio righe relativi alle variabili con upper &
  #lower bound (doppio vincolo per il solver)
  A = rbind(matrice1, matrice2)
  A$index <- as.numeric(row.names(A)) #salvo il nome delle righe, in formato numerico, in una nuova colonna 
  A = A %>%
    arrange(index) #risistemo il dataframe
}

A2 = Vincolo3(set) #ecco la nostra bella matrice A

b_Vincolo4 = function(df, A, g) {
  "df = dataset del mese
  A = matrice A costruita con Vincolo4
  g = dataset gruppi costruito per trovare i bound"
  bin = as.character(sort(unique(df$giocate_totali_discr)))
  minori = A[duplicated(A[1:79]),] #saranno tutti i < 
  maggiori = A[!duplicated(A[1:79]),] #saranno tutti i >=
  minori$constraints = "<" 
  maggiori$constraints = ">="
  categorie = as.character(maggiori$V79)
  bound = g$massimo_macchine
  b1 = c()
  for (i in categorie) {
    if (i == bin[1] | i == bin[5]){
      b1 = append(b1, 1)
    }
    else {
      if (i == bin[2]) {
        b1 = append(b1, bound[1])
      }
      else {
        if (i == bin[3]) {
          b1 = append(b1, bound[2])
        }
        else {
          if (i == bin[4]) {
            b1 = append(b1, bound[3])
          }
          else {
            if (i == bin[6]) {
              b1 = append(b1, bound[5])
            }
            else {
              if (i == bin[7]) {
                b1 = append(b1, bound[6])
              }
              else {
                b1 = append(b1, bound[7])
              }
            }
          }
        }
      }
    }
  }
  categorie1 = as.character(minori$V79)
  b2 = c()
  for (i in categorie1) {
    if (i == bin[1]){
      b2 = append(b2, bound[1])
    }
    else {
      if (i == bin[2]) {
        b2 = append(b2, bound[2])
      }
      else {
        if ( i == bin[3]) {
          b2 = append(b2, bound[3])
        }
        else {
          if (i == bin[5]) {
            b2 = append(b2, bound[5])
          }
          else {
            if (i == bin[6]) {
              b2 = append(b2, bound[6])
            }
            else {
              b2 = append(b2, bound[7])
            }
          }
        }
      }
    }
  }
  maggiori$b = b1
  minori$b = b2
  final = rbind(maggiori,minori)
  final = final  %>%
    arrange(index)
  return(final)
}

A2 = b_Vincolo4(set, A2, gruppi)

A = rbind(A, as.matrix(A2[1:78]))
A = apply(A, 2, as.numeric)
b = c(b,A2$b)
constraints = c(constraints, A2$constraints)

sol = solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)
shadow_price = sol$con

#risultato sul dataset
set[which(sol$solution != 0),] %>%
  arrange(Casino, tipo) %>%
  View()

#CONSIDERAZIONE SUCCESSIVA
#ora che abbiamo raggruppato in categorie le giocate, possiamo valutare se quest'ultilme dipendono dalla categoria
#si potrebbere dedurre una certa preferenza dei consumatori
chisq.test(set_discretizzato$giocate_totali, set_discretizzato$tipo) #dipendenza

###########################################################################
#come potrebbe essermi utile per i vincoli?
#che vincolo potrei mettere?
###########################################################################