# ####################################################################
# ### Abschlussklausur Statistik   2019                            ###
# ####################################################################
#
#  Antworten Sie bitte unter Angabe der Fragenummer unterhalb der
#  Fragestellungen,
#  Textantworten bitte als Kommentar formatieren.
pacman::p_load(tidyverse, wrappedtools,rlist)
f1<-list()

# F1: Generieren Sie folgende Vektoren von Zahlen:
#   f1a: Zahlen von 75 bis 50
f1a<-75:50
f1<-list.append(f1, f1a)
#   f1b: 500 gleichverteilte Zufallszahlen zwischen 18 und 85
f1b<-runif(500,18,85)
f1<-list.append(f1, f1b)
#   f1c: 500 normalverteilte Zufallszahlen mit Mittelwert=100
#        und SD=10
f1c<-rnorm(500,100,10)
f1<-list.append(f1, f1c)
#   f1d: 100 poissonverteilte Zufallszahlen mit
#        lambda=10
f1d<-rpois(100,10)
f1<-list.append(f1, f1d)
#   f1e: 6 Lottozahlen zwischen 1 und 49.
f1e<-sort(sample(1:49,6,F))
f1<-list.append(f1, f1e)
#
# F2: Testen Sie, ob die von Ihnen generierten Zahlen aus F1
#     aus einer Normalverteilung stammen,
#     Bonus: geben Sie als
#        Ergebnis in einer Tabelle die Wahrscheinlichkeit
#     an, mit der nur eine zufällige Abweichung vorliegt
normaltest<-data.frame(var=c('f1a','f1b','f1c','f1d','f1e'),
                       pNormal=NA,normalverteilt='nein')
normaltest$pNormal[1]<-formatP(ksnormal(f1a)$p.value)
normaltest$pNormal[2]<-formatP(ksnormal(f1b)$p.value)
normaltest$pNormal[3]<-formatP(ksnormal(f1c)$p.value)
normaltest$pNormal[4]<-formatP(ksnormal(f1d)$p.value)
normaltest$pNormal[5]<-formatP(ksnormal(f1e)$p.value)
#alternativ für Liste:
for (var_i in 1:5){
  normaltest$pNormal[var_i]<-
    formatP(ksnormal(f1[[var_i]])$p.value)
}
normaltest$pNormal <- 
  map_dbl(f1,.f = function(x){shapiro.test(x)$p.value})

map(f1,ksnormal) %>% map('p.value') %>% map_chr(formatP)

normaltest$normalverteilt[which(
  as.numeric(normaltest$pNormal)>.05)]<-'ja'

#
# F3: Erklären Sie die Begriffe 
#     f3a: Standardabweichung
#     f3b: Standardfehler
#     f3c: Signifikanz
#     f3d: statistische Power
#     f3e: Sensitivität
#     f3f: Spezifität

# F4: Benennen Sie die Klasse statistischer Verfahren für die folgenden
#     Fragestellungen:
#   f4a: Multipler Mittelwertsvergleich zwischen 3 oder mehr Gruppen für eine
#        normalverteilte Messgröße
# ANOVA
#   f4b: Vergleich der zentralen Tendenz einer ordinalskalierten Variable
#        zwischen zwei Gruppen
# U-Test /Wilcoxon-Test
#
#F5: Verwenden Sie jetzt den Datensatz bordeaux 
#    aus dem Paket DiscriMiner.
#     F5a: Erzeugen Sie einen Boxplot der Regenmenge, getrennt nach Qualität.
#        Bonus: Verwenden Sie dazu ggplot.
rawdata <- mutate(bordeaux,
                  quality=factor(quality,
                                 levels = c('good',
                                            'medium',
                                            'bad')))
ggplot(rawdata,aes(quality,rain))+
  geom_boxplot()
#     f5b: Testen Sie, ob die Unterschiede zufällig sein können,
#        formulieren Sie dazu die Nullhypothese.
#     f5c: Erzeugen Sie eine Ergebnistabelle mit deskriptiver Statistik 
#        für "temperature" und "rain" für die Qualitätsgruppen.
result <- rawdata %>% group_by(quality) %>% 
  summarize(Temp=meansd(temperature),
            Rain=meansd(rain))
p_temp <- anova(lm(temperature~quality,rawdata))$`Pr(>F)`[1]
p_rain <- anova(lm(rain~quality,rawdata))$`Pr(>F)`[1]
result <- add_row(result,quality='p',
                  Temp=formatP(p_temp),
                  Rain=formatP(p_rain))
#     f5d: Exportieren Sie diese Tabelle in eine Textdatei
#     mit einem ; als Trennzeichen.
write.table(x = result,file = 'res.txt',sep = ';')
# F6: Beispieldatensatz aus Paket DiscriMiner:
#     infarctus
#     Kardiovaskuläre Kennwerte bei 101 Infarktpatienten
rawdata <- infarctus
ordvars <- FindVars(c('F','I','P'),
                    exclude = 'O')
#     f6a Stellen Sie für die verschiedenen Prognose-Level graphisch dar:
#     - "FRCAR" "INCAR" "INSYS" "PRDIA" "PAPUL" "PVENT" "REPUL"
rawdata %>% gather(Measure,Value,-PRONO) %>% 
  ggplot(aes(PRONO,Value))+
  geom_boxplot()+
  facet_wrap(Measure~.,scales = 'free')
#     f6b Testen Sie, ob nur zufällige Unterschiede in diesen
#     Variablen zwischen den Prognosegruppen bestehen.
compare2numvars(rawdata,ordvars$names,'PRONO',
                gaussian = F)
#  F7: Erzeugen Sie eine Matrix mit dem Namen Teilnehmer 
#       mit 3 Spalten und 20 Zeilen 
Teilnehmer<-matrix('',nrow=20,ncol=3,
                   dimnames = list(
                     NULL,
                     c('ID','Vorname','Nachname')))

#     F7a Schreiben Sie Ihren Vornamen in Zeile 2 
#         Spalte 2 und Ihren
#         Nachnamen in Zeile 2 Spalte 3
Teilnehmer[2,2:3]<-c('Andreas','Busjahn')

#     F7b: Füllen Sie Spalte 1 mit den Werten "Teilnehmer 1" bis
#           "Teilnehmer 20"
Teilnehmer[,1]<-paste('Teilnehmer',1:20)

#
# F8: Programmieren Sie eine Schleife mit einem
#     Index von 1 bis 10, die jeweils den Text 'Durchgang'
#     und den Schleifenindex ausgibt
for(i in seq_len(10)) {
  print(paste('Durchgang',i))
}
# 
# ####################################################################
# ### Viel Erfolg!                                                 ###
# ####################################################################
