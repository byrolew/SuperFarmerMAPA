library(randomForest)
library(devtools)
library(testthat)
library(stringr)
library(SuperFarmerMAPA)

context("Czy aktualizacja stanu stada po rzucie kostkami przebiega poprawnie?")

test_that("Nie mamy malego psa, lis zabija kroliki",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(10, 0, 0, 0, 0, 0, 0)
	names(stock_status) <- animals
	die1_result <- "krolik"
	die2_result <- "lis"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["krolik"]],0)
})

test_that("Mamy 2 male psy, lis zabija jednego z nich i nie zjada krolikow",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(2, 0, 0, 0, 0, 2, 0)
	names(stock_status) <- animals
	die1_result <- "krolik"
	die2_result <- "lis"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["krolik"]],3)
	expect_equal(stock_status[["maly_pies"]],1)
})

test_that("Mamy duzego i malego psa, lis i wilk zabijaja psy, bez uszczerbku dla pozostalych zwierzat",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(1, 1, 1, 1, 1, 1, 1)
	names(stock_status) <- animals
	die1_result <- "wilk"
	die2_result <- "lis"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["krolik"]],1)
	expect_equal(stock_status[["owca"]],1)
	expect_equal(stock_status[["swinia"]],1)
	expect_equal(stock_status[["krowa"]],1)
	expect_equal(stock_status[["kon"]],1)
	expect_equal(stock_status[["maly_pies"]],0)
	expect_equal(stock_status[["duzy_pies"]],0)
})

test_that("Nie mamy psow, wilk i lis zjadaja wszystkie zwierzeta oprocz konia",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(1, 1, 1, 1, 1, 0, 0)
	names(stock_status) <- animals
	die1_result <- "wilk"
	die2_result <- "lis"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["krolik"]],0)
	expect_equal(stock_status[["owca"]],0)
	expect_equal(stock_status[["swinia"]],0)
	expect_equal(stock_status[["krowa"]],0)
	expect_equal(stock_status[["kon"]],1)
	expect_equal(stock_status[["maly_pies"]],0)
	expect_equal(stock_status[["duzy_pies"]],0)
})

test_that("Nie mamy duzego psa, mamy malego psa, wilk zjada wszystkie zwierzeta oprocz konia i malego psa",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(1, 1, 1, 1, 1, 1, 0)
	names(stock_status) <- animals
	die1_result <- "wilk"
	die2_result <- "krolik"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["krolik"]],0)
	expect_equal(stock_status[["owca"]],0)
	expect_equal(stock_status[["swinia"]],0)
	expect_equal(stock_status[["krowa"]],0)
	expect_equal(stock_status[["kon"]],1)
	expect_equal(stock_status[["maly_pies"]],1)
	expect_equal(stock_status[["duzy_pies"]],0)
})

test_that("Wilk zjada tylko jednego duzego psa",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(1, 1, 1, 1, 1, 1, 2)
	names(stock_status) <- animals
	die1_result <- "wilk"
	die2_result <- "krolik"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["krolik"]],2)
	expect_equal(stock_status[["owca"]],1)
	expect_equal(stock_status[["swinia"]],1)
	expect_equal(stock_status[["krowa"]],1)
	expect_equal(stock_status[["kon"]],1)
	expect_equal(stock_status[["maly_pies"]],1)
	expect_equal(stock_status[["duzy_pies"]],1)
})

test_that("Rozmnazaja sie kroliki i owce",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(5, 7, 0, 0, 0, 0, 0)
	names(stock_status) <- animals
	die1_result <- "krolik"
	die2_result <- "owca"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["krolik"]],8)
	expect_equal(stock_status[["owca"]],11)
})

test_that("Rozmnazaja sie swinie i krowy, nie da sie przekroczyc maksymalnej liczby krow",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(0, 0, 2, 4, 0, 0, 0)
	names(stock_status) <- animals
	die1_result <- "krowa"
	die2_result <- "swinia"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["swinia"]],3)
	expect_equal(stock_status[["krowa"]],6)
})

test_that("Nie da sie przekroczyc maksymalnej liczby zwierzat danego gatunku w stadzie (na przykladzie krolikow i owiec",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(59, 23, 0, 0, 0, 0, 0)
	names(stock_status) <- animals
	die1_result <- "krolik"
	die2_result <- "owca"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["krolik"]],60)
	expect_equal(stock_status[["owca"]],24)
})

test_that("Jezeli na obu kostkach wypadnie to samo, a gracz nie ma zwierzat danego gatunku to otrzyma jedno zwierze",{
animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(0, 0, 0, 0, 0, 0, 0)
	names(stock_status) <- animals
	die1_result <- "owca"
	die2_result <- "owca"
	stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
	expect_equal(stock_status[["owca"]],1)
})

context("Testy poprawnosci definicji zmiennych?")

test_that("Czy wyniki na kostkach to zmienne tekstowe?",{
	die1 <- c("krolik", "krolik", "krolik", "krolik", "krolik", "krolik", "owca", "owca", "owca", "swinia", "krowa", "wilk")
	die2 <- c("krolik", "krolik", "krolik", "krolik", "krolik", "krolik", "owca", "owca", "swinia", "swinia", "kon", "lis")
	die1_result <- sample(die1, 1)
    die2_result <- sample(die2, 1)
	expect_is(die1_result,"character")
	expect_is(die2_result,"character")
})

test_that("Czy stock_status i max_stock sa wektorami?",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	stock_status <- c(0, 0, 0, 0, 0, 0, 0)
	names(stock_status) <- animals
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	expect_true(is.vector(stock_status))
	expect_true(is.vector(max_stock))
})

test_that("Czy funkcja gra zwraca poprawna wartosc? - test na strategii: strategia_rf",{
	
	expect_is(gra(strategia_rf),"numeric")
})

context("Czy warunek wygranej jest dobrze zdefiniowany")

test_that("Czy wygrana zachodzi gdy spelnione sa zasady gry?",{
	expect_true(win(c(1,1,1,1,1,0,0)))
})

test_that("Czy wygrana nie zachodzi gdy nie sa spelnione zasady gry?",{
	expect_false(win(c(1,1,1,0,1,1,1)))
})

context("Czy wymiany przebiegaja poprawnie?")

test_that("Czy funkcja wymiany jednego konia na pozostale zwierzeta dziala poprawnie? - czyli czy mam 2 konie to wygrywam",{
	animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
	max_stock <- c(60, 24, 20, 12, 6, 4, 2)
	names(max_stock) <- animals
	stock_status <- c(0, 0, 0, 0, 2, 0, 0)
	names(stock_status) <- animals
	stock_status <- change_horse_on_other_animals(stock_status)
	expect_equal(stock_status[["krolik"]],6)
	expect_equal(stock_status[["owca"]],3)
	expect_equal(stock_status[["swinia"]],1)
	expect_equal(stock_status[["krowa"]],1)
	expect_equal(stock_status[["kon"]],1)
	expect_equal(stock_status[["maly_pies"]],0)
	expect_equal(stock_status[["duzy_pies"]],0)
})



