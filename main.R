# Load and/or install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load("data.table", "ggplot2", "rstudioapi")

# Get the location of the script
location <- (rstudioapi::getActiveDocumentContext()$path)
location <- gsub("main.R", "", location)

# And run all the functions in fun.R
source(file.path(location, "fun.R"))

# Fun part:

### Create a wheel and align it to the number of your choice
my_wheel <- make_wheel(seed = 1)
align_wheel(wheel = my_wheel, num = 13)

### Take a look at your wheel
View(my_wheel)

### Plot the outer and inner wheels
wheel_plots <- plot_wheel(my_wheel)
# Outer wheel
wheel_plots[[1]]
# Inner wheel
wheel_plots[[2]]

### Save for printing
save_wheel(wheel_plots) # This will just save it to your current folder as PDF. 

### Encode some text
s <- "She sells sea shells on the sea shore; 
The shells that she sells are sea shells I'm sure. 
So if she sells sea shells on the sea shore, 
I'm sure that the shells are sea shore shells."
cat(s)
enc_s <- encode_string(s, my_wheel)
cat(enc_s)

### Decode some text
dec_s <- decode_string(enc_s, my_wheel)
cat(dec_s)
