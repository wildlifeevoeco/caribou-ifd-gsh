


library(data.table)

switch <- data.table(dir = c("on", "stay", "off",
                             "on", 
                             "on", "stay", "stay",
                             "on", "stay", "stay",
                             "on", "stay", "off",
                             "on", "off",
                             "off",
                             "on"),
                     ID = c("1", "1", "1",
                            "2", 
                            "3", "3", "3",
                            "4", "4", "4",
                            "5", "5", "5",
                            "6", "6",
                            "7", "8"))

switch[, .N, by = "dir"]
