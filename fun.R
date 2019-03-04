make_wheel <- function(seed = NA){
  
  if(!is.na(seed)){set.seed(seed)}
  
  outer_letters <- letters
  outer_numbers <- seq(1:length(outer_letters))
  inner_numbers <- sample(outer_numbers)
  inner_letters <- sample(outer_letters)
  
  wheel <- data.frame("outer_letters" = outer_letters, 
                      "outer_numbers" = outer_numbers, 
                      "inner_numbers" = inner_numbers, 
                      "inner_letters" = inner_letters)
  return(wheel)
}

align_wheel <- function(wheel, num = 2){
  idx1 <- num
  idx2 <- which(wheel[,3] == num)
  offset <- idx1 - idx2
  new_wheel <- wheel
  new_wheel$inner_numbers <- roll_shift(dat = new_wheel$inner_numbers, num = offset)
  new_wheel$inner_letters <- roll_shift(dat = new_wheel$inner_letters, num = offset)
  return(new_wheel)
}

plot_wheel <- function(wheel, outer = TRUE, inner = TRUE){
  d  <- wheel
  names(d) <- c("olet", "onum", "inum", "ilet")
  d[,2] <- as.numeric(d[,2])
  d[,3] <- as.numeric(d[,3])
  outer <- ggplot() + 
    scale_x_continuous(name="x") + 
    scale_y_continuous(name="y") +
    geom_rect(mapping = aes(ymin = 0, ymax = 8, xmin = (1:nrow(d))-0.5, xmax = (1:nrow(d))+0.5), color = "black", fill = "white", alpha = 1)  +
    # https://stackoverflow.com/questions/19438638/rotation-of-labels-to-follow-x-axis-in-ggplot2-polar-projection
    geom_rect(mapping = aes(ymin = 0, ymax = 7, xmin = (1:nrow(d))-0.5, xmax = (1:nrow(d))+0.5), color = "black", fill = "white", alpha = 1) +
    geom_rect(mapping = aes(ymin = 0, ymax = 6, xmin = (1:nrow(d))-0.5, xmax = (1:nrow(d))+0.5), color = "black", fill = "white", alpha = 1) +
    geom_text(aes(x = 1:nrow(d), y = 7.5, label=d[,1]), size=4) +
    geom_text(aes(x = 1:nrow(d), y = 6.5, label=d[,2]), size=4) +
    coord_polar() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  inner <- ggplot() + 
    scale_x_continuous(name="x") + 
    scale_y_continuous(name="y") +
    geom_rect(mapping = aes(ymin = 0, ymax = 6, xmin = (1:nrow(d))-0.5, xmax = (1:nrow(d))+0.5), color = "black", fill = "white", alpha = 1)  +
    # https://stackoverflow.com/questions/19438638/rotation-of-labels-to-follow-x-axis-in-ggplot2-polar-projection
    geom_rect(mapping = aes(ymin = 0, ymax = 5, xmin = (1:nrow(d))-0.5, xmax = (1:nrow(d))+0.5), color = "black", fill = "white", alpha = 1) +
    geom_rect(mapping = aes(ymin = 0, ymax = 4, xmin = (1:nrow(d))-0.5, xmax = (1:nrow(d))+0.5), color = "black", fill = "white", alpha = 1) +
    geom_text(aes(x = 1:nrow(d), y = 5.5, label=d[,3]), size=4) +
    geom_text(aes(x = 1:nrow(d), y = 4.5, label=d[,4]), size=4) +
    coord_polar() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  return(list(outer, inner))
}


roll_shift <- function(dat, num){
  n <- abs(num)
  d <- dat
  if(num > 0){
    shifted <- shift(d, n, type = "lag")
    rolled <- shifted
    rolled[1:n] <- d[(length(d)-n+1):length(d)]
    return(rolled)
  }
  else if(num < 0){
    shifted <- shift(d, n, type = "lead")
    rolled <- shifted
    rolled[(length(d)-n+1):length(d)] <- d[1:n]
    return(rolled)
  }
  else{
    warning("Shifted by 0 elements.")
    return(d)
  }
}

save_wheel <- function(wheel_plots){
  ggsave("outer_wheel.pdf", 
         plot = wheel_plots[[1]],
         width = 6, height = 6, 
         units = "in",
         dpi = 300)
  ggsave("inner_wheel.pdf", 
         plot = wheel_plots[[2]],
         width = 4.2, height = 4.2, 
         units = "in",
         dpi = 300)
}

encode_string <- function(string, wheel){
  s <- strsplit(string, "")[[1]]
  s_encoded <- sapply(s, function(z) encode_char(z, wheel))
  return(paste(s_encoded, collapse = ""))
}

encode_char <- function(char, wheel){
  c <- tolower(char)
  if(c == " "){return(" ")}
  else if (c %in% letters){
    w <- wheel
    idx <- which(w$inner_letters == c)
    encoded_char <- as.character(w$outer_letters[idx])
    return(encoded_char)
  }
  else{
    return(char)
  }
}

decode_string <- function(string, wheel){
  s <- strsplit(string, "")[[1]]
  s_decoded <- sapply(s, function(z) decode_char(z, wheel))
  return(paste(s_decoded, collapse = ""))
}

decode_char <- function(char, wheel){
  c <- tolower(char)
  if(c == " "){return(" ")}
  else if (c %in% letters){
    w <- wheel
    idx <- which(w$outer_letters == c)
    decoded_char <- as.character(w$inner_letters[idx])
    return(decoded_char)
  }
  else{
    return(char)
  }
}
