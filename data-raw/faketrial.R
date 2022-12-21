## code to prepare `faketrial` dataset goes here
library(tidyverse)
n <- 300
set.seed(1958)
fake_meds <- shinipsum::lorem_words
fake_meds <- fake_meds[which(nchar(fake_meds)>10)] |> unique() |>  
  str_to_title() |> 
  paste(a='Med',b=_,c='FakePharm')
faketrial <- tibble(
  Sex=sample(c('female','male'),n,TRUE) %>% factor(),
  Agegroup=factor(
    rep(c('young','middle','old'),each=n/3),
    levels=c('young','middle','old')),
  Treatment=factor(
    rep(c('sham','OP'),n/2),
    levels = c('sham','OP')),
  HR=rnorm(n,200,20),
  sysRR=rnorm(n,100,10)+
    (Treatment=='OP')*
    # (Agegroup %in% c('young','middle'))*
    (Agegroup!='old')*20+
    (Agegroup=='old')*20,
  diaRR=rnorm(n,70,10)+
    (Treatment=='OP')*
    # (Agegroup %in% c('young','middle'))*
    (Agegroup!='old')*20+
    (Agegroup=='old')*20,
  Responder=factor(sysRR>=120,
                   levels=c(FALSE,TRUE),
                   labels=c('no','yes')))

for(med_i in fake_meds){
  faketrial <- mutate(faketrial,
                    !!sym(med_i):=sample(c('y','n'),n,TRUE,
                                         prob = c(.1,.9)))
}
for(lab_i in seq_len(10)){
  faketrial <- mutate(faketrial,
                    !!sym(paste('Biomarker',lab_i,'[units]')):=
                      rlnorm(n = n,meanlog = 5+
                               ((lab_i%%2)*(Sex=='male')*.25),sdlog = .1))
}

usethis::use_data(faketrial, overwrite = TRUE, version = 3)
