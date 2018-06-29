library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())

library(readxl)
d = read_excel("Lucky Duck Entertainment revenue 2013.xls", na = c(".", "NA", "NaN"))

#Status Dataset
library(funModeling)
library(dplyr)

status = df_status(d, print_results = F)

summary(d$Month) #circa 12 mesi

###### I incontro ######
# CATEGORIZATION ##
###################

#Per trovare il numero massimo di macchine allocabili in ciascun casinç:
#1) raggruppiamo per casinò -> creo 2 dataset apposta;
#2) raggruppo per mese e sommo il numero di macchine;
#3) considero il numero più alto: se quel mese ci sono state così tante macchine allora possono starci sempre
#il punto 3 che avevamo individuato al primo incontro forse ora è inutile! Vogliamo un altro tipo di
#categorizzazione

d %>%
  group_by(Month) %>%
  summarise(n_mac = sum(NoMachines)) #macchine totali di LDE per mese

d_aries = d %>%
  filter(Casino == "Aries")
d_aries %>%
  group_by(Month) %>%
  summarise(n_mac = sum(NoMachines)) #macchine totali Aries per mese = OK!

d_libra = d %>%
  filter(Casino == "Libra")

d_libra %>%
  group_by(Month) %>%
  summarise(n_mac = sum(NoMachines))#macchine totali Libra per mese = OK!

#numero di "macchine" che posso avere
slots_unique <- d %>%
  group_by(Denomination, MachineName, MachineType, Model, Manufacturer) %>%
  summarise(monthRevenue = 
              mean(GrossRevenuePerMachine),
              monthPlays = mean(PlaysPerMachine),
              revenuePerPlay = monthRevenue / monthPlays , count = n())
  View(slots_unique) #556

d_aries %>% 
  filter(NoMachines > 1,
         Denomination == "0.02",
         MachineType == "reel"
         ) %>%
  arrange(MachineName, Model, Month) %>%
  View()
  
###### 27/06 ######
# CATEGORIZATION ##
###################

status

unique(d$Denomination) #6 unique value
unique(d$MachineType) #3 unique value 

#each Machine type could have 6 possible denomination

glimpse(d)

#suddivido i CASINO: ipotesi, le macchine non possono essere spostate da un casino all'altro

#Aries
d_aries = d %>%
  filter(Casino == "Aries")

status_aries = df_status(d_aries, F)

#d_libra = d %>%
#  filter(Casino != d_aries$Casino)

#Libra
d_libra = d %>%
  filter(Casino == "Libra")

status_libra = df_status(d_libra, F)

library(lubridate)

#Considero solo il mese di Settembre 
sept_lib = d_libra %>%
  filter (Month == ymd("2011-09-01")) #Voi usate pure as.Date()

sum(sept_lib$NoMachines) #numero macchine mese di settembre = 230 (Uguale al raggruppamento fatto al I incontro)

sept_lib %>% 
  group_by(Denomination, MachineType) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination) %>%
  View()#ok

#giocate per macchina potrebbe essere il coin in? -> da MAX
#profitto per macchina -> da MAX

#### 28/06/2018 #####
### PROVA 2 #########
#####################

#suddivisione anche per sezioni...sempre mese di Settembre e casino Libra

section_sept_lib = sept_lib %>% 
  group_by(Denomination, MachineType, Section) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination, Section) #ok
View(es)  

#Considero solo il mese di Ottobre

opt_lib = d_libra %>%
  filter (Month == ymd("2011-10-01")) #Voi usate pure as.Date()

sum(opt_lib$NoMachines) #numero macchine mese di settembre = 230 (Uguale al raggruppamento fatto al I incontro)

opt_lib %>% 
  group_by(Denomination, MachineType) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination) #ok

#Si ottengono 12 categorie rispetto alle 14 presenti al mese di settembre, che ormai abbiamo
#appurato essere quello con più valori[più veritiero]. Quindi anche in questo caso avremmo 
#Missing Value  

#suddivisione anche per sezioni...sempre mese di Ottobre e casino Libra
section_opt_lib = opt_lib %>% 
  group_by(Denomination, MachineType, Section) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination, Section) #ok

#Considero solo il mese di Novembre

nov_lib = d_libra %>%
  filter (Month == ymd("2011-11-01")) #Voi usate pure as.Date()

sum(nov_lib$NoMachines) #numero macchine mese di settembre = 230 (Uguale al raggruppamento fatto al I incontro)

nov_lib %>% 
  group_by(Denomination, MachineType) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination) #ok

#Si ottengono 12 categorie rispetto alle 14 presenti al mese di settembre, che ormai abbiamo
#appurato essere quello con più valori[più veritiero]. Quindi anche in questo caso avremmo 
#Missing Value  

#suddivisione anche per sezioni...sempre mese di Ottobre e casino Libra
section_nov_lib = opt_lib %>% 
  group_by(Denomination, MachineType, Section) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination, Section) #ok
View(es)

#Replica di quanto fatto in precedenza ma per tutti i mesi: ipotizzando che le categorie
#siano 14 [guardo il mese di settembre - quello più completo], dovrei avere 14 * 12 = 168
#osservazioni -> in realtà sono 147 [si ha però la serie storica a tutti gli effetti]

Sistemato1 = d_libra %>% 
  group_by(Denomination, MachineType, Month) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination) #ok

#un'osservazione in questo dataset corrisponde ad una determinata categoria in un determinato mese
#viene fornito il numero di macchine di quella categoria in quel mese e da lì si ricavano i valori
#monetari -> bisogna aggregare anche forecast etc...

#######################################

Sistemato2 = d_libra %>% 
  group_by(Denomination, MachineType, Section, Month) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination, Section) #ok

#un'osservazione corrisponde ad una determinata categoria in una determinata sezione del casino Libra,
#questo dataset è strettamente dipendente a Sistemato1: guardate la categoria 0.01 reel. Ci sono
#esattamente 8 osservazioni come in Sistemato1, ma in questo caso abbiamo informazioni circa lo
#spostamento tra le sezioni. Problema: perdiamo la serie storica perchè se è possibile che alcune
#categorie di macchine, che presentano più di una macchina (num_macchine > 1) vengano divise tra 2
#o più sezioni in uno stesso mese. Esempio lampante è la categoria 0.02 reel [vedete].

#esempio CHAVEZ

isopectic <- d %>% filter( Casino == "Aries", MachineName == "Isopectic Encoding", Denomination == 2, MachineType == "video", Model == "Mirier") %>% 
  arrange(Month) 
doppi <- d %>% group_by(Casino, Section, Month, Denomination, MachineType, MachineName, Model, Manufacturer) %>%
  mutate(n = n()) %>% arrange(Month,  Casino, Section, MachineName, Denomination) %>% filter(n == 2)

###########################################################################################################################################


#######################
# "MODELLO SEMPLICE" #
######################
#Lavoriamo solamente su Aries per iniziare

df = d_aries %>%
  group_by(Denomination, MachineType, Section, Month) %>%
  summarise(numero_macchine = sum(NoMachines),
            ricavi_totali = sum(GrossRevenue),
            ricavo_unitario = ricavi_totali/numero_macchine,
            giocate_totali = sum(Plays),
            giocate_unitarie = round(giocate_totali/numero_macchine),
            ricavo_per_giocata = ricavi_totali/giocate_totali) %>%
  arrange(Month, Section, MachineType, Denomination)
df

if(require(linprog)==FALSE) install.packages("linprog")
library(linprog)

#Modello con vincolo semplice legato solo al quantitativo mensile 

f_obj = df$ricavo_unitario #ricavi unitari = da MAX

a =  c(rep(1,473),
  ifelse(df$Month == ymd("2011-09-01"), 1, 0),
  ifelse(df$Month == ymd("2011-09-01"), 1, 0),
  ifelse(df$Month == ymd("2011-10-01"), 1, 0),
  ifelse(df$Month == ymd("2011-10-01"), 1, 0),
  ifelse(df$Month == ymd("2011-11-01"), 1, 0),
  ifelse(df$Month == ymd("2011-11-01"), 1, 0),
  ifelse(df$Month == ymd("2011-12-01"), 1, 0),
  ifelse(df$Month == ymd("2011-12-01"), 1, 0),
  ifelse(df$Month == ymd("2012-01-01"), 1, 0),
  ifelse(df$Month == ymd("2012-01-01"), 1, 0),
  ifelse(df$Month == ymd("2012-02-01"), 1, 0),
  ifelse(df$Month == ymd("2012-02-01"), 1, 0),
  ifelse(df$Month == ymd("2012-03-01"), 1, 0),
  ifelse(df$Month == ymd("2012-03-01"), 1, 0),
  ifelse(df$Month == ymd("2012-04-01"), 1, 0),
  ifelse(df$Month == ymd("2012-04-01"), 1, 0),
  ifelse(df$Month == ymd("2012-05-01"), 1, 0),
  ifelse(df$Month == ymd("2012-05-01"), 1, 0),
  ifelse(df$Month == ymd("2012-06-01"), 1, 0),
  ifelse(df$Month == ymd("2012-06-01"), 1, 0),
  ifelse(df$Month == ymd("2012-07-01"), 1, 0),
  ifelse(df$Month == ymd("2012-07-01"), 1, 0),
  ifelse(df$Month == ymd("2012-08-01"), 1, 0),
  ifelse(df$Month == ymd("2012-08-01"), 1, 0))

A = matrix(a,25,473, byrow = T) #Matrice A del modello, 1 se valore attivo, 0 altrimenti 

b = c(5896, rep(c(404, 849), 12)) #upper and lower bound
constraints = c("<=", rep(c(">=", "<="), 12)) 

sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)

shadow_price = sol$con

#proviamo ad aggiungere qualche vincolo

#proporzioni delle sezioni per mese 
df %>%
  group_by(Month, Section) %>%
  summarise(Num_macchine = sum(numero_macchine)) %>%
  mutate(prop_per_mese = round(Num_macchine / sum(Num_macchine), 2)) %>% #il mutate perde un livello dopo il summarise
  View()
