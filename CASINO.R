#Caricamento Dataset 
#setwd("C:\\Users\\Donatella\\Desktop\\bicocca\\decision models\\project\\progetto_finale")
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

#Per trovare il numero massimo di macchine allocabili in ciascun casinò:
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

sept_lib = d_libra %>%
  filter (Month == as.Date("2011-09-01")) 

sum(sept_lib$NoMachines) #numero macchine mese di settembre = 230 (Uguale al raggruppamento fatto al I incontro)

sept_lib %>% 
  group_by(Denomination, MachineType) %>%
  summarise(num_macchine = sum(NoMachines),
            profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
            giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
  arrange(MachineType, Denomination) #ok

#giocate per macchina potrebbe essere il coin in? -> da MAX
#profitto per macchina -> da MAX


