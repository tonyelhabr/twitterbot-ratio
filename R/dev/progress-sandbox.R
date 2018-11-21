
library("progress")

# f <- function(x, .pb) {
#   Sys.sleep(0.1)
#   # message(x)
#   print(x)
#   .pb$tick()
#   invisible(x)
# }
# pb <- progress::progress_bar$new(total = length(letters))
# map_chr(letters, f, .pb=pb)


pb <- progress_bar$new(total = 100)
f <- function() {
  pb$tick(0)
  # Sys.sleep(3)
  for (i in 1:100) {
    Sys.sleep(1 / 100)
    message(i)
    pb$tick()
  }
}
f()

pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = 100,
  clear = FALSE,
  width = 60
  )
for (i in 1:100) {
  Sys.sleep(1 / 100)
  pb$tick()
  message(i)
}
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent in :elapsed",
  total = 100,
  clear = FALSE,
  width= 60
  )
for (i in 1:100) {
  Sys.sleep(1 / 100)
  pb$tick()
  message(i)
}

pb <- progress_bar$new(
  format = "[:bar] :percent eta :eta\n",
  total = 500,
  width = 80
  )
f <- function() {
  for (i in 1:500) {
    Sys.sleep(1 / 500)
    message(i)
    pb$tick()
  }
}
f()
