create_plot <- function(n = 10) {
  plot(1:n, 1:n, main = "Wykres z helpers", xlab = "X", ylab = "Y")
}

greet <- function(name = "Świat") {
  paste("Witaj,", name)
}
