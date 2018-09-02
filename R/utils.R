create_this_week <- function(){
  thisweek <- data.frame(
    day = c("friday", "saturday", "sunday"),
    morning = c(76, 71, 70),
    afternoon = c(88, 85, 83)
  )
  save(thisweek, file = "data/thisweek.rda")

}
