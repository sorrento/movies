

# LECTURA ---------------------------------------------------------------------------------------------------------

library(data.table)
library(dplyr)
rm(list = ls())

# FUNCIONES -------------------------------------------------------------------------------------------------------

# Obtiene la foto a partir del nombre y genera un objeto ggplot
agrega.foto <- function(actor){
  # actor <- 'Matt Damon'
  require(grid) #pintar la imagen
  require(rvest) #consulta web
  
  
  name <- gsub(' ', '+', tolower(actor))
  url <- paste0("https://www.google.com/search?tbm=isch&source=hp&q=", name ,"&oq=matt+damon&sclient=img")
  page <- read_html(url)
  
  
  node <- html_nodes(page,xpath = '//img')
  
  url.file <- html_attr(node[[2]], name = 'src')
  download.file(url.file,'temp.jpg', mode = 'wb')
  
  cat(url.file)
  
  img <- jpeg::readJPEG('temp.jpg')
  g_pic <- rasterGrob(img, interpolate=TRUE)
  
  g <- ggplot() #para no tener que lidiar con la ubicación (que tape puntos)
  g2 <- g +
    # theme(panel.grid.major = element_blank(), 
    #       panel.grid.minor = element_blank(), 
    #       plot.title = element_text(size = rel(1.5), face = "bold"),
    #       axis.ticks = element_blank()) + 
    annotation_custom(g_pic)
  
  return(g2)
}


# IMDB ------------------------------------------------------------------------------------------------------------

# https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset?select=IMDb+title_principals.csv

dt.imdb <- fread('data_in/IMDB/IMDb movies.csv', encoding = 'UTF-8')
# names(dt.imdb)

# separamos los actores.. este dataset estan ordenados por importancia... (créditos)
  anames <- paste0('a', 1:20)
  dt.imdb <- dt.imdb %>% tidyr::separate(actors, anames, sep =  ", ", remove = F)  

# gross como número
dt.imdb[, usa_gross_M := usa_gross_income %>% stringr::str_replace('\\$', '') %>%  as.numeric()]
dt.imdb[, usa_gross_M := usa_gross_M/1000000]
  # dt.imdb[, .N,.(usa_gross_income, usa_gross_M)]

# IMDB ACTORES ---------------------------------------------------------------------------------------------------------

  # ONLY USA recently
  dt.actors.wide <- dt.imdb[country == 'USA' & year > 1980, c('title', 'imdb_title_id', anames), with=F]
  dt.actors.long <- melt(dt.actors.wide, c('title','imdb_title_id'),
                         measure.vars = anames, variable.name = 'cast.num', value.name = 'actor')[!is.na(actor)][order(title)]
  dt.actors.long[, cast.num := as.numeric(gsub('a', '', cast.num))]


 
 dt.actors.summary <- dt.actors.long[,.(n.movies=.N, median= median(cast.num), min = min(cast.num),
                                        max = max(cast.num), mean = mean(cast.num), sd = sd(cast.num, na.rm = T)),
                                     .(actor)][order(median)]
 
 dt.casting.summary <- dcast(dt.actors.long, actor~ paste0('a',stringr::str_pad(cast.num, 2, pad = "0")))
 dt.casting.summary <- dt.casting.summary[actor != '']
 
 dt.actors.summary.all <- merge(dt.actors.summary, dt.casting.summary, 'actor')
 
 
 # La evolucion de papeles de un actor
 buf <- merge(dt.actors.long, dt.imdb[,.(imdb_title_id, year, usa_gross_M,budget)])
  act <- 'Thomas Downey'
 

# EVOLUCION DE CASTING --------------------------------------------------------------------------------------------

  library(ggplot2)
  library(ggpubr)
  # act <- dt.actors.summary[n.movies >10]$actor %>% sample(1)
  (act <- dt.actors.summary[n.movies >10 & mean < 3]$actor %>% sample(1))
  (data <- buf[actor == act][order(year)])
  
  g0 <- ggplot(data, aes(year, usa_gross_M)) +geom_line()+geom_point()+ geom_point(data = data[cast.num == 1], color = 'red')+
    labs(title= paste('Pasta generada '))
  
  g1 <- ggplot(data, aes(year, cast.num)) +geom_line()+geom_point()+ geom_point(data = data[cast.num == 1], color = 'red')+
    scale_y_reverse()+ labs(title= paste('Roles de ', act))
  
  g2 <- ggplot(data, aes(usa_gross_M, cast.num)) +geom_point()+ labs(title= paste('pasta generada USA ')) + 
    scale_y_reverse() + geom_point(data = data[cast.num == 1], color = 'red')
  
  
  ggarrange(ggarrange(g1, g0, ncol = 1), g2)
  
 
 # PARECE QUE NO ESTÁ BIEN EN ORDEN DEL CASTING
 # dt.imdb[title == 'Hard Target']$actors # vandamme sale como 10, y en realidad es prota
 
 #  dt.actors.summary[n.movies > 5]  %>% View('actors')
 # dt.actors.summary.all[n.movies > 15]  %>% View('actors')
 #  
  

# TMDB --------------------------------------------------------------------------------------
  require(bit64)
  require(tidyverse)
  require(dplyr)
  require(jsonlite)
  require(data.table)
 
 # https://www.kaggle.com/tmdb/tmdb-movie-metadata?select=tmdb_5000_movies.csv
  
 dt.tmdb.movies <- fread('data_in/TMDB/tmdb_5000_movies.csv')
 credits <- read_csv('data_in/TMDB/tmdb_5000_credits.csv',col_names=TRUE,na="NA") #lo leo para hacer el parse json fácilmente

#  dt.tmdb.movies[,.(keywords)] %>% sample_n(2)
# dt.tmdb.movies[, .(title, revenue)] %>% sample_n(10)

 #no está la de vandamme
 # dt.tmdb.movies[title %like% 'Hard']$title
 # dt.tmdb.movies[title %like% 'target']$title

credit <- credits %>% filter(nchar(cast)>2) %>% mutate(js=lapply(cast,fromJSON)) %>% unnest(js) %>% select(-c(crew,cast_id,credit_id,id, cast))
dt.credits <- credit %>% as.data.table() 
dt.credits[name %like%  'Niro']

dt.movies <- dt.tmdb.movies[,.(movie_id = id, title, budget.M = budget / 1000000, revenue.M =revenue / 1000000, 
                               popularity, runtime, vote_average, release_date= as.Date(release_date))][order(release_date)]


dt.credits[, order2:= ifelse(order < 15, order, 15)]

dt.credits.sum <- dt.credits[,.(n.movie = .N, mean = mean(order), median = median(order), min = min(order), max = max(order), 
              sd = sd(order)), .( name)]

dt.credits.sum <- merge(
  dt.credits.sum,
  dcast(dt.credits, name ~ order2), 'name'
  )
                        
dt.credits.sum[n.movie > 10] %>% View()
dt.credits.sum[n.movie > 10 & min == 0] %>% View()

# TMDB EVOLUCION DE CASTING --------------------------------------------------------------------------------------------

  require(ggpubr)
  library(ggrepel)
  # act <- 'Emma Watson'
  # (act <- dt.credits$name %>% sample(1))
  # (act <- dt.credits[, .N, name][N>10 & mean >3]$name %>% sample(1))
  (act <- dt.credits.sum[n.movie >10 & mean < 1]$name %>% sample(1))

(actor.summary <- merge(dt.movies, 
      dt.credits[name ==  act], c('movie_id', 'title'), sort = F) 
)

data <- actor.summary
g0 <-
  ggplot(data, aes(release_date, revenue.M, group = 1, label = title)) + 
    geom_vline(xintercept = data$release_date, alpha = .1, color = 'blue')+ 
    geom_line()+
    # geom_point(aes(color = as.factor(order)))+ 
    geom_point()+ 
    geom_label_repel(nudge_x      = -0.35, size =2.3,
                     direction    = "y",
                     hjust        = .5,
                     segment.color = 'red',
                     segment.size = .2)+
    labs(title= paste('Pasta generada ')) +
    theme(axis.text.x = element_text(angle = 65, hjust = 1),
          axis.title.x = element_blank())

if(nrow( data[order == 0])>0){
  g0 <- g0 + geom_point(data = data[order == 0], color = 'red')
}

  g1 <-
    ggplot(data, aes(release_date, order2, group = 1, label = title)) +
    geom_line()+geom_point()+ 
      # geom_point(data = data[order == 0], color = 'red')+
    scale_y_reverse()+ labs(title= paste('Roles de ', act)) +
     geom_label_repel(nudge_x      = -0.35, size =2.3,
                     direction    = "y",
                     hjust        = .5,
                     segment.color = 'red',
                     segment.size = .2) 
  if(nrow( data[order == 0])>0){
    g1 <-
      g1 +geom_point(data = data[order == 0], color = 'red')
  }
  
  g2 <-
    ggplot(data, aes(revenue.M, order2)) +geom_point()+ labs(title= paste('pasta generada ')) + 
    scale_y_reverse() + geom_point(data = data[order == 0], color = 'red')
  

  # ggarrange(ggarrange(g1, g0, ncol = 1), g2)
  g3 <- agrega.foto(act)
  Sys.sleep(1)
  ggarrange(g1, g2,g0, g3)

# VALOR DEL ACTOR -------------------------------------------------------------------------------------------------

# lo calculamos de acuerdo a la polularidad / pasta de las películas en que ha estado en los últimos 5 años
  # sólo los 7 primeros actores
  
  
  
# oredenamos las peliculas según su ranking ese año, por pasta
  # dt.movie.year <- 
    dt.movies.sorted <- dt.movies[order(year(release_date), -revenue.M)]
      # dt.movies.sorted[year(release_date) == 2016]
  
    ranking.movies <- dt.movies.sorted[ , .(movie_id, title, revenue.M, ranking = 1:.N, total = .N) ,
                    .(year = year(release_date))] 
    ranking.movies[, percentil := (1 - ranking / total)]
  
  # ranking.movies[percentil >.95]
  

  dt.credits.short <- merge(
    dt.credits[order < 7],
    ranking.movies, c('title', 'movie_id'))
  
  # (act <- dt.credits.sum[n.movie >10 & mean < 1]$name %>% sample(1))
  # (data <- dt.credits.short[name == act][order(year)])
  # agrega.foto(act)
  # ggplot(data, aes(year, percentil)) + geom_line() + geom_point()
  
  # la media de votos tiene que ver con la popularidad?
    # ggplot(dt.movies[release_date >'1990-01-01'], aes(popularity, vote_average))+ geom_point(alpha = .2)
    # ggplot(dt.movies[popularity <25], aes(popularity, vote_average))+ geom_point(alpha = .2)
    # dt.movies[popularity <25]
    # 
  

# ACTOR HOTNESS ---------------------------------------------------------------------------------------------------

  # CALCULAR HOTNESS DE ACTORES CON EL NÚMERO DE PELÍCULAS Y EL PERCENTIL 10 DE LAS PELICULAS DE LOS ÚLTUMOS 3 AÑOS
  # (el percentil lo tomo porque algunas pelis no tienen registrado cuánto ganaron, o el actor tuvon un bajoón puntual)
  data <- dt.credits.short
  data[, ':=' (y1 = year - 1, y2 = year - 2, y3 = year - 3)]
  
  y1 <- merge(
    # data.table(name = act, year = 1990:2016),
    CJ(name = dt.credits.short$name %>% unique, year = 1990:2016),
    data[,.(movie_id, title, name, year = year +1, percentil)], 
    c('year', 'name'))
  
  y2 <- merge(
    # data.table(name = act, year = 1990:2016),
    CJ(name = dt.credits.short$name %>% unique, year = 1990:2016),
    data[,.(movie_id, title, name, year = year + 2, percentil)], 
    c('year', 'name'))
  
  y3 <- merge(
    # data.table(name = act, year = 1990:2016),
    CJ(name = dt.credits.short$name %>% unique, year = 1990:2016),
    data[,.(movie_id, title, name, year = year + 3, percentil)], 
    c('year', 'name'))
  
  
  res <- rbind(y1,y2,y3)[order(name, year)]
  
  # y <- 2015
   res[name == act & year == y]
   res[year == y]$percentil %>% median
  # data[between(year, y-4, y-1)]
  
  # res[,.(n=.N, q9 =quantile(1-percentil, .1)), .(year, name)]       
  resumen <- res[,.(n=.N, suma =sum(percentil)), .(year, name)][order(year ,-suma)]       
  resumen[, ranking.actor.year := 1:.N, .(year)]
  
  # (act <- dt.credits.sum[n.movie >10 & mean < 1]$name %>% sample(1))
  (uno <- bests[ranking.actor.year==1] %>% sample_n(1))
  act <- uno$name
    # act <- 'Harrison Ford'
    # uno$year <- 2001
  data[name == act & between(year, uno$year-4, uno$year-1)][order(year),.(year, title, percentil, revenue.M)]
  ggplot(resumen[name == act], aes(year, suma)) + geom_line() + geom_point()  +labs(title = paste(act, uno$year)) +
    geom_vline(xintercept = uno$year)
  agrega.foto(act)
  dt.credits.short[name == act][order(year),.(year, title, percentil, revenue.M)]
  
  # veamos los más hot por año
  bests <- resumen[ranking.actor.year <6 ] 
  bests %>% View
  
  resumen[year == 1994] %>% head(20)

# Predecir el éxito por el casting --------------------------------------------------------------------------------
 # # Cuánto pesa la historia, el casting, el director, la productora y el budget?
 #  
 #  hacer lo mismo que con actore con director, estudio, guinista, etc.
  # luego hacemos un modelo para predecir la pasta normalizada al año anterior, 
  # podemos hacer una árbol de decisión modelo shap para ver puntualmente, 
  # ver cuánto acertamos, 
  #en qué películas fallamos por mucho , para ver si hay otros factores
  #usar el género también, posiblemente
  

  