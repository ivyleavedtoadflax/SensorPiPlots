
require(dplyr)
require(magrittr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(testthat)
#require(ggmap)
require(RPostgreSQL)
source("server_cred.R")


argv <- commandArgs(trailingOnly = TRUE)

# Debugging
argv <- "2015-01-01 00:00:00"

# Test arguments

regex <- "\\d{4}(\\-[0-1][0-9])?(\\-\\d{2})?(\\ \\d{2})?(\\:\\d{2})?(\\:\\d{2})?"

test_that(
  "First date argument present and properly formatted",
  #"Arguments are properly formatted as 'YYYY-MM-DD', optionally 'YYYY-MM-DD HH:MM:SS'.",
{
  expect_true(argv %>% length %in% 1:3)
  expect_match(argv[1], perl = TRUE, regex)
}
)

if (!is.na(argv[2])) {
  test_that(
    "Second date argument properly formatted",
    #"Arguments are properly formatted as 'YYYY-MM-DD', optionally 'YYYY-MM-DD HH:MM:SS'.",
{
  expect_match(argv[2], perl = TRUE, regex)
}
  )
}

# Define facet wrap labelling function to name to boxes of each facet

facet_wrap_labeller <- function(gg.plot, labels = NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    
    modgrob <- getGrob(
      gg[[strips[ii]]], 
      "strip.text", 
      grep = TRUE, 
      global = TRUE
      )
    
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(
      modgrob, 
      label = labels[ii]
      )
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}


SensorPiB_sql <- src_postgres(
  dbname = cred.dbname,
  host = cred.host,
  user = cred.user,
  password = cred.password
  )

# Function to create sql queries from arguments

date_query <- function(tab_name,col_name) {
  
  ifelse(
    (argv %>% length > 2),
    sql_out <- sql(
      paste(
        "select * from ",
        tab_name,
        " where ",
        col_name,
        " > '",
        argv[1],
        "' and ",
        col_name,
        " < '",
        argv[2],
        "'",
        sep = ""
      )
    ),
    sql_out <- sql(
      paste(
        "select * from ",
        tab_name,
        " where ",
        col_name,
        " > '",
        argv[1],
        "'",
        sep = ""
      )
    )
  )
  
  return(sql_out)
  
}

SensorPiB <- tbl(
  SensorPiB_sql,
  date_query("sensorpi","timestamp")
) %>% 
  collect

SensorPiB_elec <- tbl(
  SensorPiB_sql,
  date_query("elec","timestamp")
) %>% 
  collect

p <- SensorPiB  %>%
  dplyr::group_by(rpi) %>%
  dplyr::mutate(
    int_light = log(int_light)*-1
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    timestamp = ymd_hms(timestamp)
  ) %>% 
  gather(
    variable, 
    value, 
    int_temp1:int_humidity
  ) %>%
  dplyr::mutate(
    variable1 = ifelse(grepl("temp",variable),"temp",variable),
    value = ifelse(grepl("temp",variable) & value > 60,NA,value)
  ) %>%
  dplyr::arrange(
    timestamp
    ) %>%
  dplyr::(
    
    )
  ggplot(
    aes(
      x = timestamp, 
      y = value, 
      colour = variable,
      shape = rpi
    )
  )+
  geom_path()+
  facet_wrap(
    ~variable1,
    scales = "free",
    ncol = 1
    )+
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


pdf("psql.pdf",width=8,height=10)
p1
dev.off()
