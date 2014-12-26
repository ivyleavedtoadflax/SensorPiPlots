
require(dplyr)
require(magrittr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(testthat)
#require(ggmap)

argv <- commandArgs(trailingOnly = TRUE)

# Define facet wrap labelling function to name to boxes of each facet

facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}

test_that(
  "Arguments are properly formatted as 'YYYY-MM-DD', optionally 'YYYY-MM-DD HH:MM:SS'.",
expect_match(argv[1],perl=TRUE,"\\d{4}\\-\\d{2}\\-\\d{2}(\\ \\d{2}\\:\\d{2}\\:\\d{2})?")
)


SensorPiB_sql <- src_sqlite("~/Dropbox/Apps/SensorPi/SensorPiB.db")
Log_sql <- src_sqlite("~/Dropbox/Apps/SensorPi/Log.db")


SensorPiB <- tbl(
  SensorPiB_sql,
  sql(
    paste(
      "select * from SensorPiB where timestamp > '",
      argv[1],
      "'",
      sep = ""
    )
  )
) %>% 
  collect

Log <- tbl(
  Log_sql,
  sql(
    paste(
      "select * from temp where timsetamp > '",
      argv[1],
      "'",
      sep = ""
    )
  )
) %>% 
  collect


p <- SensorPiB %>% 
  dplyr::mutate(
    timestamp = ymd_hms(timestamp), 
    light = log(light)*-1
  ) %>% 
  gather(
    variable, 
    value, 
    temp1:humidity
  ) %>% 
  dplyr::mutate(
    variable1 = ifelse(grepl("temp",variable),"temp",variable)
  ) %>%
  ggplot(
    aes(
      x = timestamp, 
      y = value, 
      colour = variable 
    )
  )+
  geom_path()+
  facet_wrap(
    ~variable1,
    scales = "free",
    ncol = 1)+
  theme(
    legend.position = "right"
  ) +
  scale_colour_discrete(
    labels = c(
      "Internal temperature",
      "External temperature",
      "Internal temperature 2",
      "Light (relative values)",
      "Relative humidity"
    )
  )+
  xlab(
    "Timestamp"
  )

facet_labels <- c(
  expression(Light~(relative~values)),
  expression(Internal~relative~humidity~(percent)),
  expression(Temperature~(~degree~C))  
  )
  
p1 <- facet_wrap_labeller(
  p,
  facet_labels
  )


pdf("sensorpib.pdf",width=8,height=10)
p1
dev.off()



p <- Log %>% 
  dplyr::mutate(
    timestamp = ymd_hms(timsetamp), 
    light = log(light)*-1
  ) %>% 
  gather(
    variable, 
    value, 
    temp1:humidity
  ) %>% 
  dplyr::mutate(
    variable1 = ifelse(grepl("temp",variable),"temp",variable)
  ) %>%
  ggplot(
    aes(
      x = timestamp, 
      y = value, 
      colour = variable 
    )
  )+
  geom_path()+
  facet_wrap(
    ~variable1,
    scales = "free",
    ncol = 1)+
  theme(
    legend.position = "right"
  ) +
  scale_colour_discrete(
    labels = c(
      "Internal temperature",
      "External temperature",
      "Internal temperature 2",
      "Light (relative values)",
      "Relative humidity"
    )
  )+
  xlab(
    "Timestamp"
  )

facet_labels <- c(
  expression(Light~(relative~values)),
  expression(Internal~relative~humidity~(percent)),
  expression(Temperature~(~degree~C))  
)

p1 <- facet_wrap_labeller(
  p,
  facet_labels
)

pdf("log.pdf",width=8,height=10)
p1
dev.off()
