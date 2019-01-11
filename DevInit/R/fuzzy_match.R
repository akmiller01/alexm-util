list.of.packages <- c("stringdist")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

substr_matches = function(x, y){
  if(grepl(x,y,ignore.case=T,useBytes=T) | grepl(y,x,ignore.case=T,useBytes=T)){return(T)}
  return(F)
}
substr_matches = Vectorize(substr_matches)

substrmatchmatrix = function(x, y){
  res = matrix(,nrow=length(x),ncol=length(y))
  for(i in 1:length(y)){
    res[,i] = substr_matches(x,y[i])
  }
  return(res)
}

fuzzy = function(x, y, max.attempts = 3, min.dist = 0){
  u_x = unique(x)
  u_y = unique(y)
  u_x_matches = c()
  dist_mat = stringdistmatrix(u_x, u_y)
  substr_mat = substrmatchmatrix(u_x, u_y)
  no_matches = colSums(dist_mat==0)==0
  # Each row represents one obs of x, with each col representing distances to y
  for(i in 1:nrow(dist_mat)){
    row = dist_mat[i,]
    row_min = min(row)
    # Exact match, add it and move on
    if(row_min<=min.dist){
      u_x_match_index = which.min(row)
      u_x_match = u_y[u_x_match_index]
      u_x_matches = c(u_x_matches,u_x_match)
    # No small matches anywhere, throw out
    }else{
      # First try all substring matches
      this_u_x = u_x[i]
      row_ss = substr_mat[i,]
      if(max(row_ss)==1){
        substring_max_indicies = which(row_ss==T)
        for(ss_guess in substring_max_indicies){
          u_x_match_guess = u_y[ss_guess]
          prompt_message = paste0("Does '",this_u_x,"' match '",u_x_match_guess,"'? (y/[n]/esc): ")
          prompt_answer = readline(prompt_message)
          if(prompt_answer=="y"){
            u_x_match = u_x_match_guess
            break
          }
        }
      }else{
        # Then try all string distance matches
        attempt = 1
        u_x_match = NA
        row.df = data.frame(dist=row,index=c(1:length(row)))
        row.df = row.df[no_matches,]
        row.df = row.df[order(row.df$dist),]
        while(attempt<=max.attempts & nrow(row.df)>0){
          u_x_match_index_guess = row.df[attempt,"index"]
          u_x_match_guess = u_y[u_x_match_index_guess]
          prompt_message = paste0("Does '",this_u_x,"' match '",u_x_match_guess,"'? (y/[n]/esc): ")
          prompt_answer = readline(prompt_message)
          if(prompt_answer=="y"){
            u_x_match = u_x_match_guess
            break
          }else{
          }
          attempt = attempt + 1
        } 
      }
      u_x_matches = c(u_x_matches,u_x_match)
    }
  }
  return(data.frame(x=u_x,y=u_x_matches))
}
