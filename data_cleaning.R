library(tidyverse)
library(tidytext)
library(tm)

train <- read.csv("train.csv",encoding="UTF-8")
test<- read.csv("test.csv",encoding="UTF-8")

submission <- read.csv("submission_prediction.csv")




map_df(3L:7L, ~unnest_tokens(train, ngram, text, token = "ngrams", n = 3)) %>% 
  count(ngram) %>%
  filter(!is.na(ngram)) %>% 
  slice_max(n, n = 11)

#Búsqueda de ngrams
map_df(3L:7L, ~unnest_tokens(test, ngram, text, token = "ngrams", n = 3)) %>% 
  count(ngram) %>%
  filter(!is.na(ngram)) %>% 
  slice_max(n, n = 11)


submisson_text <- submission %>% 
  
  left_join(test) %>% 
  mutate(name_2=case_when(
    
    str_detect(text,"(?i)dr[\\.\\s]")~"Uribe",
    str_detect(text,"¿Cuál Verdad")~"Uribe",
    str_detect(text,"(?i)15\\spuntos")~"Uribe",
    str_detect(text,"(?i)comunidad\\sinforma")~"Uribe",
    str_detect(text,"(?i)bolsa\\sny")~"Uribe",
    str_detect(text,"(?i)^foro\\smedellín")~"Uribe",
    str_detect(text,"(?i)cedemocratico")~"Uribe",
    str_detect(text,"(?i)duele")~"Uribe",
    str_detect(text,"(?i)falleció")~"Uribe",
    str_detect(text,"(?i)foroaniversario")~"Uribe",

    str_detect(text,"(?i)alza")~"Uribe",
    
    str_detect(text,"(?i)gbno")~"Uribe",
    str_detect(text,"(?i)¡familias!")~"Uribe",
    str_detect(text,"(?i)cardenal")~"Uribe",
    str_detect(text,"(?i)paola")~"Uribe",
    str_detect(text,"^\"+|^“")~"Uribe",
    
    str_detect(text,"(?i)unicornio")~"Uribe",
    str_detect(text,"(?i)oposición\\sconstructiva")~"Uribe",
    str_detect(text,"(?i)energía\\sen\\seuropa")~"Uribe",
    str_detect(text,"(?i)commonwealth")~"Uribe",
    str_detect(text,"(?i)cero\\sdroga")~"Uribe",
    str_detect(text,"(?i)odebrecht")~"Uribe",
    str_detect(text,"(?i)judía")~"Uribe",
    str_detect(text,"(?i)centro\\sdemocrático")~"Uribe",
    str_detect(text,"(?i)todo\\ses\\suribe")~"Uribe",
    str_detect(text,"(?i)atropella")~"Uribe",
    str_detect(text,"(?i)contracultura")~"Uribe",
    str_detect(text,"(?i)araújo")~"Uribe",
    str_detect(text,"(?i)santos")~"Uribe",
    str_detect(text,"CONFIANZA")~"Uribe",
    str_detect(text,"(?i)impune")&!str_detect(text,"(?i)atraquen")~"Uribe",
    str_detect(text,"MariaFdaCabal")~"Uribe",
    str_detect(text,"curandero")~"Uribe",
    str_detect(text,"(?i)confianza\\sprivada")~"Uribe",
    str_detect(text,"(?i)consumo\\spopular")~"Uribe",
    str_detect(text,"(?i)sobretasa\\stributaria")~"Uribe",
    
    str_detect(text,"(?i)patria")~"Uribe",
    str_detect(text,"(?i)politización")~"Uribe",
    str_detect(text,"(?i)se\\spuede\\sleer")~"Uribe",
    
    str_detect(text,"(?i)\\sHodgson\\s")~"Uribe",
    str_detect(text,"(?i)turbinas\\sde")~"Uribe",
    str_detect(text,"(?i)confianza\\sde\\sinversión")~"Uribe",
    str_detect(text,"(?i)medicina\\sprepagada")~"Uribe",
    str_detect(text,"(?i)gobierno\\sduque")~"Uribe",
    str_detect(text,"(?i)ayuda\\spor\\sfavor")~"Uribe",
  
    
    str_detect(text,"^#EnVivo")~"Lopez",
    str_detect(text,"#ParcerosPorBogotá")~"Lopez",
    str_detect(text,"@Bogota")~"Lopez",
    str_detect(text,"(?i)^#aestahora")~"Lopez",
    str_detect(text,"(?i)@metrobogota")~"Lopez",
    str_detect(text,"(?i)#DialogosVinculantes")~"Lopez",
    str_detect(text,"(?i)gran\\slogro")~"Lopez",
    str_detect(text,"(?i)@policiabogota")~"Lopez",
    str_detect(text,"(?i)¡hola!\\sgracias\\spor")~"Lopez",
    str_detect(text,"(?i)harry")~"Lopez",
    str_detect(text,"(?i)pmu")&!str_detect(text,"(?i)hidroituango")&!str_detect(text,"(?i)san\\sandr")~"Lopez",
    str_detect(text,"(?i)#nomás")~"Lopez",
    str_detect(text,"(?i)PresupuestoBogotá2023")~"Lopez",
    str_detect(text,"(?i)nuevafotodeperfil")~"Lopez",
    
    str_detect(text,"(?i)jotape")~"Lopez",
    str_detect(text,"(?i)representadas")~"Lopez",
    
    str_detect(text,"(?i)atraquen")~"Lopez",
    str_detect(text,"(?i)seguridad\\sciudadana")~"Lopez",
    str_detect(text,"(?i)cámara")~"Lopez",
    str_detect(text,"(?i)plaza\\sde\\sarmas")~"Lopez",
    str_detect(text,"(?i)parceros")~"Lopez",
    
    str_detect(text,"(?i)#regiónmetropolitana")~"Lopez",
    str_detect(text,"(?i)querido\\s@DCoronell")~"Lopez",
    
    str_detect(text,"(?i)correcto\\sy\\sverdadero")~"Lopez",
    
    
    str_detect(text,"(?i)^me\\sreun")~"Petro",
    str_detect(text,"(?i)^RT")~"Petro",
    str_detect(text,"(?i)dialogo\\sregional")~"Petro",
    str_detect(text,"(?i)hidroituango")&!str_detect(text,"(?i)turbinas\\sde")~"Petro",
    str_detect(text,"(?i)[\\s@]lula")~"Petro",
    str_detect(text,"(?i)brasil")&!str_detect(text,"(?i)odebrecht")~"Petro",
    str_detect(text,"(?i)etb")~"Petro",
    str_detect(text,"(?i)condecoré")~"Petro",
    str_detect(text,"(?i)justicia//ssocial")~"Petro",
    str_detect(text,"(?i)ministro\\sde\\shacienda")~"Petro",
    str_detect(text,"(?i)ha\\ssido\\saprobado")~"Petro",
    str_detect(text,"(?i)despojaron")~"Petro",
    str_detect(text,"(?i)contratos\\sde\\sexp")~"Petro",
    str_detect(text,"(?i)san\\sjosé\\sdel\\sguaviare")~"Petro",
    str_detect(text,"(?i)cambio\\sde\\sdeuda")~"Petro",
    str_detect(text,"(?i)^\\.@roybarreras")~"Petro",
    str_detect(text,"(?i)saludo\\sespecial")~"Petro",
    str_detect(text,"(?i)progresividad")~"Petro",
    str_detect(text,"(?i)rebote")~"Petro",
    
    str_detect(text,"(?i)respetrán")~"Petro",
    str_detect(text,"(?i)mazzucato")~"Petro",
    str_detect(text,"(?i)susana")~"Petro",
    str_detect(text,"(?i)designado")~"Petro",
    str_detect(text,"(?i)bosques\\sy\\sclima")~"Petro",
    str_detect(text,"(?i)despojada")~"Petro",
    
    str_detect(text,"(?i)san\\sandr")~"Petro",
    str_detect(text,"(?i)maduro")~"Petro",
    
    str_detect(text,"(?i)irene\\svélez")~"Petro",
    
    
    str_detect(text,"(?i)barrancabermeja")~"Petro",
    str_detect(text,"(?i)the\\seconomist")~"Petro",
    str_detect(text,"(?i)alcaldesas")~"Petro",
    str_detect(text,"(?i)campeonas\\smundiales")&!str_detect(text,"#MujeresPoderosas")~"Petro",
    str_detect(text,"(?i)biodiverso")~"Petro",
    str_detect(text,"(?i)mexi")~"Petro",
    str_detect(text,"PAE")~"Petro",
    str_detect(text,"(?i)ministerio\\sde\\svivienda")~"Petro",
    str_detect(text,"(?i)testigos\\sfalsos")~"Petro",
    str_detect(text,"(?i)contra\\sel\\shambre")~"Petro",
    str_detect(text,"\\sSAE.")~"Petro",
    str_detect(text,"(?i)^decrecer")~"Petro",
   str_detect(text,"(?i)hospitales\\sintervenidos")~"Petro",
    str_detect(text,"(?i)femenina\\ssub")&!str_detect(text,"(?i)patria")~"Petro",
    str_detect(text,"(?i)aracataca")~"Petro",
    str_detect(text,"(?i)mejoramientos")~"Petro",
   str_detect(text,"(?i)titulares")~"Petro",
    str_detect(text,"(?i)100\\sdías")~"Petro",
    str_detect(text,"(?i)14\\sbillones")~"Petro",
    str_detect(text,"(?i)congreso\\sganadero")~"Petro",
    str_detect(text,"(?i)dirigencia\\scomunal")~"Petro",
    str_detect(text,"(?i)estimada\\scristina")~"Petro",
    str_detect(text,"(?i)nacionales\\sde\\simportados")~"Petro",
    str_detect(text,"(?i)ministerio\\sde\\sigualdad")~"Petro",
    str_detect(text,"(?i)piojó")~"Petro",
    str_detect(text,"(?i)antinarcóticos")~"Petro",
    str_detect(text,"(?i)sub\\scampeones\\sdel\\smundo")~"Petro",
    str_detect(text,"(?i)abrazo\\sde\\ssolidaridad")~"Petro",
    str_detect(text,"(?i)fortaleceremos")~"Petro",
    str_detect(text,"(?i)hato\\sgrande")~"Petro",
    str_detect(text,"(?i)comfort")~"Petro",
    str_detect(text,"(?i)gaitán")~"Petro",
    str_detect(text,"(?i)ausencia\\sde\\sdiálogo")~"Petro",
    
    str_detect(text,"(?i)finalistas")~"Petro",
    
    
    
    
    
    
    TRUE~name),
  
  name_name_2=ifelse((name!=name_2)&name_2!="",1,0)
  
           
           
           )

#hasta el submission 7 van 148 nuevas correcciones
submisson_text_2 <- submisson_text %>% 
  select(-name,-text,-name_name_2
         
         ) %>% 
  rename(name=name_2)



write.csv(submisson_text_2,"submisson_14.csv",row.names = F)

