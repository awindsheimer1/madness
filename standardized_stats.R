if(F){
  library(rvest)
  library(tidyverse)
}
years <- c(2022)
start <- Sys.time()
total <- 0

teams <- data.frame()
for(y in years){
  school_stats <- paste0("https://www.sports-reference.com/cbb/seasons/", 
                         y, "-school-stats.html")
  
  school_games <- (read_html(school_stats) %>%
    html_table())[[1]]
  school_games <- school_games[which(school_games[,2]!=""&school_games[,2]!="School"),]
  school_keep <- which(school_games[,3]!="0")
  school_games <- school_games[school_keep,]
  names(school_games)[[2]] <- "teams"
  ncaa_teams <- grepl("NCAA", school_games$teams, fixed=T)
  
  school_stats <- read_html(school_stats) %>% 
    html_nodes(xpath='//td/a') %>%
    html_attr("href")
  school_stats <- paste0("https://www.sports-reference.com", school_stats)
  school_stats <- school_stats[school_keep]
  school_sch <- gsub(".html", "-schedule.html", school_stats)
  school_gl <- gsub(".html", "-gamelogs.html", school_stats)
  
  team_names <- substr(school_stats, start=46,stop=nchar(school_stats))
  team_names <- substr(team_names, start=0, stop=nchar(team_names)-5)
  
  slen <- length(school_stats)
  
  games_played <- matrix(0, nrow=(slen+1)*2, ncol=1)
  srs_b <- matrix(0, nrow=(slen+1)*2, ncol=1)
  stl_b <- matrix(0, nrow=(slen+1)*2, ncol=1)
  tov_b <- matrix(0, nrow=(slen+1)*2, ncol=1)
  blk_b <- matrix(0, nrow=(slen+1)*2, ncol=1)
  trb_b <- matrix(0, nrow=(slen+1)*2, ncol=1)
  
  M <- matrix(0, nrow=slen, ncol=slen)
  i<-1
  
  for(j in school_sch){
    
    links <- read_html(j) %>% 
      html_nodes(xpath='//td/a') %>%
      html_attr("href")
    t <- (read_html(j) %>%
      html_table())[[2]]
    t <- t[which(t$Conf!="Conf"),]
    
    games <- which(t$Type!="NCAA"&t$Conf!="")
    g <- length(games)
    
    gl <- (read_html(school_gl[[i]]) %>% 
      html_table())[[1]]
    gl <- gl[which(gl[,8]!="School" & gl[,8]!="FG"),]
    gl <- gl[games,]
    
    gl_links <- read_html(school_gl[[i]]) %>% 
      html_nodes(xpath='//td/a') %>%
      html_attr("href")
    gl_links <- gl_links[grepl("schools", gl_links)]
    gl_links <- gl_links[1:g]
    gl_links <- paste0("https://www.sports-reference.com", gl_links)
    
    for(G in 1:nrow(gl)){
      index <- which(school_stats==gl_links[[G]])
      
      if(length(index)==0) {g <- g-1; next;}
      #print(paste0(G, " ", index))
      M[i, index] <- M[i, index]-1

      trb_b[[i]] <- trb_b[[i]]+as.numeric(gl[G, 18][[1]])
      stl_b[[i]] <- stl_b[[i]]+as.numeric(gl[G, 20][[1]])
      blk_b[[i]] <- blk_b[[i]]+as.numeric(gl[G, 21][[1]])
      tov_b[[i]] <- tov_b[[i]]+as.numeric(gl[G, 22][[1]])
      srs_b[[i]] <- srs_b[[i]]+as.numeric(gl[G, 6][[1]])

      trb_b[[i+slen]] <- trb_b[[i+slen]]+as.numeric(gl[G, 35][[1]])
      stl_b[[i+slen]] <- stl_b[[i+slen]]+as.numeric(gl[G, 37][[1]])
      blk_b[[i+slen]] <- blk_b[[i+slen]]+as.numeric(gl[G, 38][[1]])
      tov_b[[i+slen]] <- tov_b[[i+slen]]+as.numeric(gl[G, 39][[1]])
      srs_b[[i+slen]] <- srs_b[[i+slen]]+as.numeric(gl[G, 7][[1]])
      
      #print(trb_b[[i]])
    }
    
    games_played[[i]] <- games_played[[i+slen]] <- g
    print(paste0(i, " ", Sys.time()-start, " ", total+Sys.time()-start))
    total <- total+Sys.time()-start
    start <- Sys.time()
    i<-i+1
  }
  
  I <- diag(slen)
  half_gp <- games_played[1:slen]
  M <- M*as.vector(half_gp^-1)
  sum_off <- c(rep(1, slen), rep(0, slen))
  sum_def <- c(rep(0, slen), rep(1, slen))
  A <- rbind(cbind(I, M), cbind(M, I), sum_off, sum_def)
  asvd <- svd(A)
  adiag <- diag(1/asvd$d)
  adiag[slen*2, slen*2] <- 0
  A_mod <- asvd$v %*% adiag %*% t(asvd$u)
  
  mean_trb <- sum(trb_b[1:slen])/sum(games_played[1:slen])
  trb_b <- trb_b*(games_played^-1)
  trb_b[1:slen] <- trb_b[1:slen]-mean_trb
  trb_b[(slen+1):(2*slen)] <- mean_trb - trb_b[(slen+1):(2*slen)]
  trb_b[(length(trb_b)-1):length(trb_b)] <- 0

  mean_stl <- sum(stl_b[1:slen])/sum(games_played[1:slen])
  stl_b <- stl_b*(games_played^-1)
  stl_b[1:slen] <- stl_b[1:slen]-mean_stl
  stl_b[(slen+1):(2*slen)] <- mean_stl - stl_b[(slen+1):(2*slen)]
  stl_b[(length(stl_b)-1):length(stl_b)] <- 0

  mean_blk <- sum(blk_b[1:slen])/sum(games_played[1:slen])
  blk_b <- blk_b*(games_played^-1)
  blk_b[1:slen] <- blk_b[1:slen]-mean_blk
  blk_b[(slen+1):(2*slen)] <- mean_blk - blk_b[(slen+1):(2*slen)]
  blk_b[(length(blk_b)-1):length(blk_b)] <- 0

  mean_tov <- sum(tov_b[1:slen])/sum(games_played[1:slen])
  tov_b <- tov_b*(games_played^-1)
  tov_b[1:slen] <- tov_b[1:slen]-mean_tov
  tov_b[(slen+1):(2*slen)] <- mean_tov - tov_b[(slen+1):(2*slen)]
  tov_b[(length(tov_b)-1):length(tov_b)] <- 0

  mean_srs <- sum(srs_b[1:slen])/sum(games_played[1:slen])
  srs_b <- srs_b*(games_played^-1)
  srs_b[1:slen] <- srs_b[1:slen]-mean_srs
  srs_b[(slen+1):(2*slen)] <- mean_srs - srs_b[(slen+1):(2*slen)]
  srs_b[(length(srs_b)-1):length(srs_b)] <- 0

  trb_solve <- A_mod %*% trb_b
  stl_solve <- A_mod %*% stl_b
  blk_solve <- A_mod %*% blk_b
  tov_solve <- -1*(A_mod %*% tov_b)
  srs_solve <- A_mod %*% srs_b
  
  df <- data.frame(team_names, 
                   trb_solve[1:slen], 
                   trb_solve[(slen+1):(2*slen)],
                   trb_solve[1:slen]+
                   trb_solve[(slen+1):(2*slen)],
                   stl_solve[1:slen], 
                   stl_solve[(slen+1):(2*slen)],
                   stl_solve[1:slen]+
                   stl_solve[(slen+1):(2*slen)],
                   blk_solve[1:slen], 
                   blk_solve[(slen+1):(2*slen)],
                   blk_solve[1:slen]+
                   blk_solve[(slen+1):(2*slen)],
                   tov_solve[1:slen], 
                   tov_solve[(slen+1):(2*slen)],
                   tov_solve[1:slen]+ 
                   tov_solve[(slen+1):(2*slen)],
                   srs_solve[1:slen], 
                   srs_solve[(slen+1):(2*slen)],
                   srs_solve[1:slen]+ 
                   srs_solve[(slen+1):(2*slen)])
  
 names(df) <- c("team/year", 
                "trb_collected", "trb_prevented", "trb_rating", 
                "stl_forced", "stl_avoided", "stl_rating", 
                "blk_forced", "blk_avoided", "blk_rating", 
                "tov_forced", "tov_avoided", "tov_rating", 
                "osrs", "dsrs", "srs")
 

  
 df[,c(2:11)] <- round(df[,c(2:11)], 3)
 df <- df[ncaa_teams,]
 #View(df[,c(1, 4, 7, 10, 13, 16)])
 
 teams <- rbind(teams, df)
}

View(teams)
