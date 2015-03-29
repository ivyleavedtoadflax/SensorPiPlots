
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

# Test arguments
# debugging only:
# argv <- c("2015-02-12 00:00:00")

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
  collect %>%
  dplyr::mutate(
    timestamp = ymd_hms(timestamp)
    ) %>%
  dplyr::arrange(
    timestamp
    )

elec <- tbl(
  SensorPiB_sql,
  date_query("elec","timestamp")
) %>% 
  collect %>%
  dplyr::mutate(
    timestamp = ymd_hms(timestamp)
  ) %>%
  dplyr::select(
    timestamp,value
  ) 

# Below implementation leads to mismatched plots between tables

# %>%
#   tidyr::gather(
#     variable,
#     value,
#     value
#     ) %>%
#   dplyr::mutate(
#     rpi = "SensorPiA+"
#     ) %>%
#   dplyr::select(
#     timestamp,rpi,variable,value
#     )

SensorPiB %<>% merge(
    elec,
    all = TRUE
    ) %>%
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
    int_temp1:value
  ) %>%
  dplyr::mutate(
    variable1 = ifelse(grepl("temp",variable),"temp",variable),
    value = ifelse(grepl("temp",variable) & value > 60,NA,value),
    rpi = factor(rpi)
  ) %>% 
  dplyr::arrange(
    timestamp
    )

p <- SensorPiB %>% ggplot(
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
      "Internal temperature",
      "External temperature",
      "Light (relative values)",
      "Relative humidity",
      "Electricity consumption"
    )
  )+
  xlab(
    "Timestamp"
  )

#+
#  geom_smooth(
#    data = subset(SensorPiB, variable =="value"),
#    col = "red"
#  )

facet_labels <- c(
  expression(Light~(relative~values)),
  expression(Internal~relative~humidity~(percent)),
  expression(Electricity~consumption~(Kw~h^-1)),
  expression(Temperature~(~degree~C))
)
  
p1 <- facet_wrap_labeller(
  p,
  facet_labels
  )


pdf("psql.pdf",width=8,height=12)
p1
dev.off()
