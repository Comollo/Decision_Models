# library(rstudioapi)
# # Getting the path of your current open file
# current_path = rstudioapi::getActiveDocumentContext()$path 
# setwd(dirname(current_path ))
# print(getwd())
# 
# library(readxl)
# d = read_excel("Lucky Duck Entertainment revenue 2013.xls", na = c(".", "NA", "NaN"))
# 
# #Status Dataset
# library(funModeling)
# library(dplyr)
# 
# status = df_status(d, print_results = F)
# 
# summary(d$Month) #circa 12 mesi
# 
# ###### I incontro ######
# # CATEGORIZATION ##
# ###################
# 
# #Per trovare il numero massimo di macchine allocabili in ciascun casinç:
# #1) raggruppiamo per casinò -> creo 2 dataset apposta;
# #2) raggruppo per mese e sommo il numero di macchine;
# #3) considero il numero più alto: se quel mese ci sono state così tante macchine allora possono starci sempre
# #il punto 3 che avevamo individuato al primo incontro forse ora è inutile! Vogliamo un altro tipo di
# #categorizzazione
# 
# d %>%
#   group_by(Month) %>%
#   summarise(n_mac = sum(NoMachines)) #macchine totali di LDE per mese
# 
# d_aries = d %>%
#   filter(Casino == "Aries")
# d_aries %>%
#   group_by(Month) %>%
#   summarise(n_mac = sum(NoMachines)) #macchine totali Aries per mese = OK!
# 
# d_libra = d %>%
#   filter(Casino == "Libra")
# 
# d_libra %>%
#   group_by(Month) %>%
#   summarise(n_mac = sum(NoMachines))#macchine totali Libra per mese = OK!
# 
# #numero di "macchine" che posso avere
# slots_unique <- d %>%
#   group_by(Denomination, MachineName, MachineType, Model, Manufacturer) %>%
#   summarise(monthRevenue = 
#               mean(GrossRevenuePerMachine),
#               monthPlays = mean(PlaysPerMachine),
#               revenuePerPlay = monthRevenue / monthPlays , count = n())
#   View(slots_unique) #556
# 
# d_aries %>% 
#   filter(NoMachines > 1,
#          Denomination == "0.02",
#          MachineType == "reel"
#          ) %>%
#   arrange(MachineName, Model, Month) %>%
#   View()
#   
# ###### 27/06 ######
# # CATEGORIZATION ##
# ###################
# 
# status
# 
# unique(d$Denomination) #6 unique value
# unique(d$MachineType) #3 unique value 
# 
# #each Machine type could have 6 possible denomination
# 
# glimpse(d)
# 
# #suddivido i CASINO: ipotesi, le macchine non possono essere spostate da un casino all'altro
# 
# #Aries
# d_aries = d %>%
#   filter(Casino == "Aries")
# 
# status_aries = df_status(d_aries, F)
# 
# #d_libra = d %>%
# #  filter(Casino != d_aries$Casino)
# 
# #Libra
# d_libra = d %>%
#   filter(Casino == "Libra")
# 
# status_libra = df_status(d_libra, F)
# 
# library(lubridate)
# 
# #Considero solo il mese di Settembre 
# sept_lib = d_libra %>%
#   filter (Month == ymd("2011-09-01")) #Voi usate pure as.Date()
# 
# sum(sept_lib$NoMachines) #numero macchine mese di settembre = 230 (Uguale al raggruppamento fatto al I incontro)
# 
# sept_lib %>% 
#   group_by(Denomination, MachineType) %>%
#   summarise(num_macchine = sum(NoMachines),
#             profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
#             giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
#   arrange(MachineType, Denomination) %>%
#   View()#ok
# 
# #giocate per macchina potrebbe essere il coin in? -> da MAX
# #profitto per macchina -> da MAX
# 
# #### 28/06/2018 #####
# ### PROVA 2 #########
# #####################
# 
# #suddivisione anche per sezioni...sempre mese di Settembre e casino Libra
# 
# section_sept_lib = sept_lib %>% 
#   group_by(Denomination, MachineType, Section) %>%
#   summarise(num_macchine = sum(NoMachines),
#             profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
#             giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
#   arrange(MachineType, Denomination, Section) #ok
# View(es)  
# 
# #Considero solo il mese di Ottobre
# 
# opt_lib = d_libra %>%
#   filter (Month == ymd("2011-10-01")) #Voi usate pure as.Date()
# 
# sum(opt_lib$NoMachines) #numero macchine mese di settembre = 230 (Uguale al raggruppamento fatto al I incontro)
# 
# opt_lib %>% 
#   group_by(Denomination, MachineType) %>%
#   summarise(num_macchine = sum(NoMachines),
#             profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
#             giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
#   arrange(MachineType, Denomination) #ok
# 
# #Si ottengono 12 categorie rispetto alle 14 presenti al mese di settembre, che ormai abbiamo
# #appurato essere quello con più valori[più veritiero]. Quindi anche in questo caso avremmo 
# #Missing Value  
# 
# #suddivisione anche per sezioni...sempre mese di Ottobre e casino Libra
# section_opt_lib = opt_lib %>% 
#   group_by(Denomination, MachineType, Section) %>%
#   summarise(num_macchine = sum(NoMachines),
#             profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
#             giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
#   arrange(MachineType, Denomination, Section) #ok
# 
# #Considero solo il mese di Novembre
# 
# nov_lib = d_libra %>%
#   filter (Month == ymd("2011-11-01")) #Voi usate pure as.Date()
# 
# sum(nov_lib$NoMachines) #numero macchine mese di settembre = 230 (Uguale al raggruppamento fatto al I incontro)
# 
# nov_lib %>% 
#   group_by(Denomination, MachineType) %>%
#   summarise(num_macchine = sum(NoMachines),
#             profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
#             giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
#   arrange(MachineType, Denomination) #ok
# 
# #Si ottengono 12 categorie rispetto alle 14 presenti al mese di settembre, che ormai abbiamo
# #appurato essere quello con più valori[più veritiero]. Quindi anche in questo caso avremmo 
# #Missing Value  
# 
# #suddivisione anche per sezioni...sempre mese di Ottobre e casino Libra
# section_nov_lib = opt_lib %>% 
#   group_by(Denomination, MachineType, Section) %>%
#   summarise(num_macchine = sum(NoMachines),
#             profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
#             giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
#   arrange(MachineType, Denomination, Section) #ok
# View(es)
# 
# #Replica di quanto fatto in precedenza ma per tutti i mesi: ipotizzando che le categorie
# #siano 14 [guardo il mese di settembre - quello più completo], dovrei avere 14 * 12 = 168
# #osservazioni -> in realtà sono 147 [si ha però la serie storica a tutti gli effetti]
# 
# Sistemato1 = d_libra %>% 
#   group_by(Denomination, MachineType, Month) %>%
#   summarise(num_macchine = sum(NoMachines),
#             profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
#             giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
#   arrange(MachineType, Denomination) #ok
# 
# #un'osservazione in questo dataset corrisponde ad una determinata categoria in un determinato mese
# #viene fornito il numero di macchine di quella categoria in quel mese e da lì si ricavano i valori
# #monetari -> bisogna aggregare anche forecast etc...
# 
# #######################################
# 
# Sistemato2 = d_libra %>% 
#   group_by(Denomination, MachineType, Section, Month) %>%
#   summarise(num_macchine = sum(NoMachines),
#             profitto_per_macchina = sum(GrossRevenue)/num_macchine, #profitto per macchina di quella categoria
#             giocate_per_macchina = sum(Plays)/num_macchine) %>% #giocata per macchina di quella categoria 
#   arrange(MachineType, Denomination, Section) #ok
# 
# #un'osservazione corrisponde ad una determinata categoria in una determinata sezione del casino Libra,
# #questo dataset è strettamente dipendente a Sistemato1: guardate la categoria 0.01 reel. Ci sono
# #esattamente 8 osservazioni come in Sistemato1, ma in questo caso abbiamo informazioni circa lo
# #spostamento tra le sezioni. Problema: perdiamo la serie storica perchè se è possibile che alcune
# #categorie di macchine, che presentano più di una macchina (num_macchine > 1) vengano divise tra 2
# #o più sezioni in uno stesso mese. Esempio lampante è la categoria 0.02 reel [vedete].
# 
# #esempio CHAVEZ
# 
# isopectic <- d %>% filter( Casino == "Aries", MachineName == "Isopectic Encoding", Denomination == 2, MachineType == "video", Model == "Mirier") %>% 
#   arrange(Month) 
# doppi <- d %>% group_by(Casino, Section, Month, Denomination, MachineType, MachineName, Model, Manufacturer) %>%
#   mutate(n = n()) %>% arrange(Month,  Casino, Section, MachineName, Denomination) %>% filter(n == 2)

####################
# STRATEGIA 1: MODELLO MACRO DIVISO PER CASINO
####################
options(scipen = 999)

library(rstudioapi)
#Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())

library(readxl)
slot = read_excel("Lucky Duck Entertainment revenue 2013.xls", na = c(".", "NA", "NaN"))

library(funModeling)
library(dplyr)
library(lubridate)

#status
status = df_status(d, print_results = F)
summary(d$Month) #12 mesi

#aries
d_aries = slot %>%
  filter(Casino == "Aries")

#libra
d_libra = slot %>%
  filter(Casino == "Libra")


#keep calm: first exploration -> only aries

df = d_aries %>%
  group_by(Denomination, MachineType, Section, Month) %>%
  summarise(numero_macchine = sum(NoMachines),
            ricavi_totali = sum(GrossRevenue),
            ricavo_unitario = ricavi_totali/numero_macchine,
            giocate_totali = sum(Plays),
            giocate_unitarie = round(giocate_totali/numero_macchine),
            ricavo_per_giocata = ricavi_totali/giocate_totali) %>%
  arrange(Month, Section, MachineType, Denomination) %>%
  mutate(tipo = paste0(MachineType, "_", Denomination))

library(ggplot2)
ggplot(data=slot, aes(x=slot$Month, y = slot$GrossRevenuePerMachine)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  labs(x="Month", y="Gross Revenue per Machine") +
  theme_classic()

ricavi_mese = df %>% 
  group_by(Month) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario), 
            numero_macchine_medie = mean(numero_macchine),
            varianza_macchine = sqrt(var(numero_macchine)))

ggplot(data=ricavi_mese, aes(x=ricavi_mese$Month, y = ricavi_mese$ricavo_medio_unitario)) +
  geom_line(alpha=.5, size=1, color="#880011") +
  ggtitle("Ricavi per Mese") +
  labs(x="Mese", y="Ricavo medio unitario") +
  theme_classic()

ricavi_mese$Month = as.factor(ricavi_mese$Month)

ggplot(data=ricavi_mese, aes(x=ricavi_mese$numero_macchine_medie,
                             y=ricavi_mese$ricavo_medio_unitario,
                             colour = Month)) +
  geom_point(alpha=.4, size=4) +
  ggtitle("Ricavi e Numero macchine mensili") +
  labs(x="Numero medio macchine", y="Ricavi medi unitari") + 
  theme_minimal()

ricavi_categoria = df %>%
  group_by(Denomination, MachineType) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario)) %>%
  mutate(type = paste0(Denomination,sep = "_", MachineType))


ggplot(data=ricavi_categoria, aes(x=ricavi_categoria$type, y = ricavi_categoria$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Categoria") +
  labs(x="Categoria", y="Ricavo medio unitario") +
  theme_classic()

ricavi_sezione = df %>% 
  group_by(Section) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario))

ggplot(data=ricavi_sezione, aes(x=ricavi_sezione$Section, y = ricavi_sezione$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Sezione") +
  labs(x="Sezione", y="Ricavo medio unitario") +
  theme_classic()

df %>%
  group_by(Denomination, MachineType) %>%
  summarise(n = n()) %>%
  View()#13 categorie al massimo

categorie_mese = 
  df %>% 
  group_by(Month, Denomination, MachineType) %>% 
  summarise(n =n()) %>% 
  group_by(Month) %>%
  summarise(numero_categorie = n()) #alla fine restano più o meno 13 = raggruppamento per categoria ha senso[per aries]

df %>% 
  group_by(Section, Month) %>% 
  summarise(n =n()) %>%
  View()

# ggplot(data=df, aes(x=df$ricavo_unitario)) +
#   geom_histogram(fill="#880011") +  
#   labs(x="Ricavo unitario", y="Count\nof Records") +
#   facet_wrap(~df$Month) +
#   theme_minimal()

df %>% group_by(tipo, Section) %>%
  summarise(n = n()) %>%
  arrange(tipo, Section) %>%
  group_by(Section) %>%
  summarise(n = n()) %>%
  View() #Nelle sezioni le categorie sono più o meno le stesse per tutto l'anno  = Ha senso questo raggruppamento 

df %>% group_by(tipo, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(tipo, Section) %>%
  group_by(Section) %>%
  summarise(n = n()) %>%
  View() 

df %>% group_by(Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Section) %>%
  View()

df %>% group_by(Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Section) %>%
  group_by(Section) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_categorie = sqrt(var(n))) %>%
  View()

df %>% group_by(Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Section) %>%
  group_by(Month) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_categorie = sqrt(var(n))) %>%
  View()

#######################
# "Modello semplice" #
######################
#Lavoriamo solamente su Aries per iniziare

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
  mutate(prop_per_mese = round(Num_macchine / sum(Num_macchine), 2)) %>%
  View()#il mutate perde un livello dopo il summarise

#Cerco di capire come individuare le variabili per un mese...poi estendo a tutti e 12 i mesi

#numeratore proporzione
# ifelse(df$Month == ymd("2011-09-01")  & df$Section == "Boundary", 1, 0) #ok

#denominatore 
# ifelse(df$Month == ymd("2011-09-01"), 1, 0) #ok

#vincolo base

#0.2 <= numeratore/denominatore <= 0.3
#ma si può? forse occorre scriverlo in maniera diversa? 
#No non è lineare!

x = df %>%
  group_by(Month) %>%
  summarise(Num_macchine = sum(numero_macchine)) 

variazione = round(sqrt(var(x$Num_macchine))) #potremmo usare questo per definire gli upper e lower 

d = unique(ymd(df$Month))
s = unique(df$Section)
a1 = c()
for (i in 1:12) {
  for (k in 1:4) {
    for (t in 1:2) {
      a1 = append(a1, ifelse(df$Month == d[i] & df$Section ==s[k], 1, 0))
    }
  }
}

A1 = matrix(a1,96,473, byrow = T)
A = rbind(A, A1)

b = c(5896, rep(c(404, 849), 12)) #upper and lower bound

inferiore = c()
superiore = c()
for (i in 1:length(x$Num_macchine)) {
  inferiore[i] = x$Num_macchine[i] - variazione
  superiore[i] = x$Num_macchine[i] + variazione
}

v = c(rbind(inferiore,superiore)) #unire 2 vettori in modo alternato 

b = c(5896, rep(c(404, 849), 12), rep(c(round(0.2*404),round(0.3*849)), 48))
constraints = c("<=", rep(c(">=", "<="), 12), rep(c(">=", "<="), 48))

sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)

shadow_price = sol$con
#dovrebbero essere giusti

#bisogna mettere dei vincoli legato al numero di categorie minime in ciascuna sezione e per ciascun mese

#################
# STRATEGIA 2: OTTIMIZZO MESE PER MESE MA PER ENTRAMBI I CASINO
#################

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

ricavi_mese = tot %>% 
  group_by(Month) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario), 
            numero_macchine_medie = mean(numero_macchine),
            varianza_macchine = sqrt(var(numero_macchine)))

ggplot(data=ricavi_mese, aes(x=ricavi_mese$Month, y = ricavi_mese$ricavo_medio_unitario)) +
  geom_line(alpha=.5, size=1, color="#880011") +
  ggtitle("Ricavi per Mese") +
  labs(x="Mese", y="Ricavo medio unitario") +
  theme_classic() #non è un andamento lineare!

ricavi_mese$Month = as.factor(ricavi_mese$Month) 
ggplot(data=ricavi_mese, aes(x=ricavi_mese$numero_macchine_medie,
                             y=ricavi_mese$ricavo_medio_unitario,
                             colour = Month)) +
  geom_point(alpha=.4, size=4) +
  ggtitle("Ricavi e Numero macchine mensili") +
  labs(x="Numero medio macchine", y="Ricavi medi unitari") + 
  theme_minimal()

ricavi_categoria = tot %>%
  group_by(Denomination, MachineType) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario)) %>%
  mutate(type = paste0(Denomination,sep = "_", MachineType))


ggplot(data=ricavi_categoria, aes(x=ricavi_categoria$type, y = ricavi_categoria$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Categoria") +
  labs(x="Categoria", y="Ricavo medio unitario") +
  theme_classic()

ricavi_sezione = tot %>% 
  group_by(Section) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario))

ggplot(data=ricavi_sezione, aes(x=ricavi_sezione$Section, y = ricavi_sezione$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Sezione") +
  labs(x="Sezione", y="Ricavo medio unitario") +
  theme_classic()

tot %>%
  group_by(tipo) %>%
  summarise(n = n()) %>%
  View()#14 categorie al massimo

tot %>% 
  group_by(Casino,Section, Month) %>% 
  summarise(n =n()) %>%
  View()

tot %>% group_by(Casino, tipo, Section) %>%
  summarise(n = n()) %>%
  arrange(tipo, Section) %>%
  group_by(Section) %>%
  summarise(n = n()) %>%
  View() #Modifica

tot %>% group_by(Casino, tipo, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(tipo, Section) %>%
  group_by(Section) %>%
  summarise(n = n()) %>%
  View()

tot %>% group_by(Casino, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Casino, Section) %>%
  group_by(Casino, Section) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_categorie = sqrt(var(n))) %>%
  View() 

tot %>% group_by(Casino, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Casino, Section) %>%
  group_by(Month) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_categorie = sqrt(var(n))) %>%
  View() #Modifica?

##########
# Modello solo per settembre
#########

set = tot %>%
  filter(Month == ymd("2011-09-01")) %>%
  arrange(Casino)

f_obj = round(set$ricavo_unitario) #ricavi unitari = da MAX

#quante macchine ci sono al mese nei 2 casino? 
tot %>% 
  group_by(Casino, Month) %>%
  summarise(n_macchine = sum(numero_macchine)) %>%
  arrange(Month) %>%
  View()

#costruzione vincolo
ai = c(ifelse(set$Casino == "Aries", 1, 0),
       ifelse(set$Casino == "Libra", 1, 0))
A = matrix(ai,
           2
           ,
           78,
           byrow = T)#Matrice A del modello, 1 se valore attivo, 0 altrimenti 

b = c(849, 230) #upper and lower bound
constraints = c("<=", "<=") 

library(linprog)
sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)

shadow_price = sol$con #solo con questo vincolo le macchine vengo piazzate dove il ricavo unitario è massimo

#proviamo ad aggiungere qualche vincolo

#la proporzione di macchine in ciascuna sezione deve essere tra la percentuale minima e quella massima
set %>%
  group_by(Casino, Section) %>%
  summarise(Num_macchine = sum(numero_macchine)) %>%
  mutate(prop_per_mese = round(Num_macchine / sum(Num_macchine), 2)) %>%
  View()#il mutate perde un livello dopo il summarise


s = unique(set$Section)
casino = unique(set$Casino)
a1 = c()

for (t in 1:2) {
  for (i in 1:4) {
    for (k in 1:2) {
      a1 = append(a1, ifelse(set$Section == s[i] & set$Casino == casino[t], 1, 0))
    }
  }
}

A1 = matrix(a1,16,78, byrow = T)
A = rbind(A, A1)

b = c(5896, rep(c(404, 849), 12)) #upper and lower bound

# inferiore = c()
# superiore = c()
# for (i in 1:length(x$Num_macchine)) {
#   inferiore[i] = x$Num_macchine[i] - variazione
#   superiore[i] = x$Num_macchine[i] + variazione
# }

# v = c(rbind(inferiore,superiore)) #unire 2 vettori in modo alternato 

b = c(849, 230, rep(c(round(0.2*849), round(0.3*849)), 4), rep(c(round(0.15*230), round(0.4*230)), 4))
constraints = c("<=", "<=", rep(c(">=", "<="), 8))

sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)

shadow_price = sol$con

#anche le categorie non dovrebbero essere nulle??
#forse si -> valutiamo

set %>%
  group_by(Casino, tipo) %>%
  summarise(n = n(), macchine = sum(numero_macchine)) %>%
  View()
#almeno una categoria in ciascun casino! onesto 

tipo = unique(set$tipo)
casino = unique(set$Casino)
a2 = c()

for (t in 1:2) {
  for (i in 1:14) {
      a2 = append(a2, ifelse(set$tipo == tipo[i] & set$Casino == casino[t], 1, 0))
    }
  }


A2 = matrix(a2,28,78, byrow = T) #dovrebbe esserci una riga di tutti 0; presumo sia la 14 perchè in aries ci sono 13 e non 14 categorie

#check
all(rep(0,78) == A2[14,0]) #yes -> da rimuovere

#c'è un errore nella costruzione della matrice A2 -> forse no proviamo!    
A2 = A2[c(1:13,15:28),]
A = rbind(A, A2)


b = c(849, 230, #vincoli 1
      rep(c(round(0.2*849), round(0.3*849)), 4), rep(c(round(0.15*230), round(0.4*230)), 4), #vincoli 2
      rep(1,27)) #vincoli 3
constraints = c("<=", "<=",
                rep(c(">=", "<="), 8),
                rep(">=", 27))

sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)

shadow_price = sol$con
