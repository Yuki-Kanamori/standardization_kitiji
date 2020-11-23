require(tidyverse)

setwd("/Users/Yuki/Dropbox/sokouo1/make_VASTdata/Numbers")
path = "/Users/Yuki/Dropbox/sokouo1/make_VASTdata/Numbers"
files = list.files(path)
kiti = NA
for (i in 1:length(files)){
  data = read.csv(paste0(files[i])) %>% filter(和名 == "キチジ") %>% mutate(file = paste0(files[i])) %>% mutate(year = as.numeric(str_sub(file, 5, 8)))
  # unique(data$和名)
  kiti = rbind(kiti, data)
}
summary(kiti)
write.csv(kiti, "kiti.csv", fileEncoding = "CP932")
