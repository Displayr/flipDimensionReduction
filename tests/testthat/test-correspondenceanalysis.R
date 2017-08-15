context("Correspondence Analysis")

for (output in c("Scatterplot", "Moonplot", "Text"))
    test_that(paste0("Bug DS-1075", output),
          {
              data("colas", package = "flipExampleData")
              z = xtabs(~d1 + d2, data = colas)
              z = z[rowSums(z) > 0, colSums(z) > 0]
              expect_error(capture.output(suppressWarnings(print(CorrespondenceAnalysis(z, output = output)))), NA)#, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")              z = xtabs(~d1 + d3, data = colas)
              z = xtabs(~d1 + d3, data = colas)
              expect_error(capture.output(suppressWarnings(print(CorrespondenceAnalysis(z, output = output)))), NA)#, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")              z = xtabs(~d1 + d3, data = colas)
          })
x <- matrix(c(0.3004, 0.6864, 0.4975, 0.2908, 0.2781, 0.2642, 0.1916, 0.284,  0.3514, 0.2534, 0.2089,
                           c(  0.0198, 0.4604, 0.2151, 0.5235, 0.1151, 0.12,   0.5457, 0.3041, 0.06312,    0.384,  0.06064),
                           c(  0.01114,    0.4111, 0.1904, 0.4494, 0.06931,    0.1112, 0.4716, 0.2859, 0.0495, 0.3296, 0.03837),
                           c(  0.01114,    0.2373, 0.089,  0.2707, 0.05322,    0.06436,    0.2756, 0.1656, 0.02967,    0.1916, 0.02228),
                           c(  0.0198, 0.177,  0.07054,    0.0297, 0.0396, 0.02719,    0.0136, 0.02847,    0.0198, 0.02847,    0.02472),
                           c(  0.4543, 0.1275, 0.07673,    0.02847,    0.07293,    0.1077, 0.01609,    0.05198,    0.321,  0.01856,    0.0297),
                           c(  0.06807,    0.1089, 0.06064,    0.0198, 0.1174, 0.04084,    0.01609,    0.01733,    0.03465,    0.01361,    0.03589),
                           c(  0.08168,    0.224,  0.1015, 0.04579,    0.04815,    0.04084,    0.03094,    0.05562,    0.05322,    0.04084,    0.02847)),nrow=8,byrow=TRUE)
x.with.labels <- x
dimnames(x.with.labels) <- list(Brand=c('Coke','V',"Red Bull","Lift Plus",'Diet.Coke','Fanta','Lift','Pepsi'),
                                       Attribute=c('Kids', 'Teens',    "Enjoy life",   'Picks you up', 'Refreshes',    'Cheers you up',    'Energy',   'Up-to-date',   'Fun',  'When tired',   'Relax'))

output = "Scatterplot"

test_that("Row/column names",
        {
            res0 <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
            expect_equal(res0$row.column.names, c("Brand", "Attribute"))
            res1 <- CorrespondenceAnalysis(x)
            expect_equal(res1$row.column.names, c("Rows", "Columns"))
            attr(x, "row.column.names") <- c("ABC", "DEF")
            res2 <- CorrespondenceAnalysis(x)
            expect_equal(res2$row.column.names, c("ABC", "DEF"))
        })

for (output in c("Scatterplot", "Moonplot", "Text"))
    test_that(paste0("CorrespondenceAnalysis is OK (mainly GetTidyTwoDimensionalArray) with ", output),
              {
        expect_error(CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET"), NA)
        expect_error(CorrespondenceAnalysis(x, output=output), NA)
        # 3D array with no names
        z <- array(NA, c(8,11,2))
        z[,,1] <- x
        expect_that(CorrespondenceAnalysis(z, output = output), throws_error())
        dimnames(z) <- list(dimnames(x.with.labels)[[1]], dimnames(x.with.labels)[[2]], 1:2)
        expect_error(suppressWarnings(CorrespondenceAnalysis(z, output = output)), NA)
    })

for (output in c("Scatterplot", "Moonplot",  "Text"))
    test_that(paste("CorrespondenceAnalysis prints", output),
    {
        expect_error(CorrespondenceAnalysis(x.with.labels, row.names.to.remove = "NET",  column.names.to.remove = "NET", output = output), NA)
        expect_error(CorrespondenceAnalysis(x,output = output), NA)
    })

test_that(paste("CorrespondenceAnalysis negative values"),
          {
              x2 <- x
              x2[1, 1] <- -1
              expect_error(CorrespondenceAnalysis(x2, output = output), "Input tables must not contain negative values.")
          })


test_that("Row and column labels",
          {
                x <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
                expect_equal(x$row.column.names, c("Brand",  "Attribute"))

                attr(x.with.labels, "row.column.names") <- c("My rows", "My columns")
                x <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
                expect_equal(x$row.column.names, c("Brand",  "Attribute"))

                names(dimnames(x.with.labels)) <- NULL
                x <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
                expect_equal(x$row.column.names, c("My rows", "My columns"))

                attr(x.with.labels, "row.column.names") <- NULL
                x <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
                expect_equal(x$row.column.names, c("Rows",  "Columns"))

                xd <- array(runif(9), dim = c(3, 3, 3), dimnames = list(A = c("a","a","a"), B = c("a","a","a"), C = c("a","a","a")))
                expect_warning(x <- CorrespondenceAnalysis(xd), "first statistic")
                expect_equal(x$row.column.names, c("A",  "B"))

                names(dimnames(xd)) <- NULL
                expect_warning(x <- CorrespondenceAnalysis(xd), "first statistic")
                expect_equal(x$row.column.names, c("Rows",  "Columns"))

                attr(xd, "row.column.names") <- c("Alpha", "Beta")
                expect_warning(x <- CorrespondenceAnalysis(xd), "first statistic")
                expect_equal(x$row.column.names, c("Alpha",  "Beta"))

          })


test_that("Logos",
          {
              urls <- sprintf("https://dl.dropboxusercontent.com/u/539177224/%s_grey.svg",
                              c("apple","baby","car","stickman","stickwoman","chicken","cow","thumbsup","rocket","tools"))
              data("colas", package = "flipExampleData")
              z = xtabs(~d1 + d2, data = colas)
              z = z[rowSums(z) > 0, colSums(z) > 0]
              colnames(z) <- LETTERS[1:8]
              expect_error(suppressWarnings(print(CorrespondenceAnalysis(z, logos=urls[1:9]))), NA)
              expect_error(suppressWarnings(print(CorrespondenceAnalysis(z, logos=urls[1:9], transpose=T))))
              expect_error(suppressWarnings(print(CorrespondenceAnalysis(z, logos=urls[1:4]))))

              z2 <- z + runif(72)
              zz <- list(z, z2)
              expect_error(suppressWarnings(print(CorrespondenceAnalysis(zz, logos=urls[1:9]))), NA)
              expect_error(suppressWarnings(print(CorrespondenceAnalysis(zz, logos=urls[1:9], transpose=T))))
              expect_error(suppressWarnings(print(CorrespondenceAnalysis(zz, logos=urls[1:4]))))

              rownames(zz[[2]])[1] <- "Error"
              expect_error(suppressWarnings(print(CorrespondenceAnalysis(zz, logos=urls[1:9]))))
          })

test_that("Empty rows/columns",
          {
              data("colas", package = "flipExampleData")
              z = xtabs(~d1 + d2, data = colas)
              z = z[rowSums(z) > 0, colSums(z) > 0]

              ze <- z
              ze[,3] <- 0
              expect_error(CorrespondenceAnalysis(ze), "Column")
              expect_error(CorrespondenceAnalysis(ze, transpose = T), "Column")
              ze[1,] <- NA
              expect_error(CorrespondenceAnalysis(ze), "Row '18 to 24' contains only zeros or NAs.")
          })


test_that("Bubble charts",
          {
                expect_error(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart"))
                bsizes = x.with.labels[,1]
                expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes)), NA)
                names(bsizes)[2] = "Dog"
                expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes)), "The bubble sizes must contain the same names")
                names(bsizes)[2] = names(bsizes)[1]
                expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes)), "There are duplicate bubble size names.")
                expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = 1:length(bsizes))), "The bubble sizes need to be named")

                expect_error(print(suppressWarnings(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes[-1]))))
                expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = x.with.labels[,1] *10000, bubble.title = "Importance (%)")), NA)

                set.seed(12332)
                x = matrix(round(runif(100)*100), 10, dimnames = list(letters[1:10], LETTERS[1:10]))
                sizes = 1:10
                names(sizes) = letters[1:10]
                sizes = sample(sizes, length(sizes))
                expect_error(print(CorrespondenceAnalysis(x, output = "Bubble Chart", bubble.size = sizes)), NA)
                names(sizes) = LETTERS[1:10]
                expect_error(print(CorrespondenceAnalysis(x, output = "Bubble Chart", bubble.size = sizes)))
})



test_that("Bubble charts for residuals",
          {
resids <- structure(c(10.1912517245498, 8.87722315268985, 12.2523879409318,
-15.3349817581089, 1.27050404382534, -6.14725729323782, -5.22342282259855,
-1.22207137969193, -5.09150374064002, NaN, -5.35563098521719,
-6.75735112142719, -8.14229791628786, 6.35534631941382, -4.1421652919085,
6.95174752903645, 10.4628872578677, 1.75673095241278, -0.371256603026594,
NaN, -4.1144095194129, -4.65870727092094, -6.81912169431022,
7.67632518003232, 0.597083653127377, -6.16203478005691, -2.17790606879645,
4.15901306142041, 15.3304761446746, NaN, 0.36006593920307, 0.929996916160007,
8.77645240532682, -5.76180199409422, 0.813953615485186, 2.17544926008851,
1.43363676832796, -1.64716793174101, -8.50975702290193, NaN,
-4.97794072254543, -4.84035753976007, -5.57490388776299, 10.9379372243159,
-1.58104835716255, -6.43411636514869, -2.76411006885418, -0.79498220649854,
15.01163505095, NaN, 1.20194679194781, 3.509927544558, -2.37773279858116,
-2.43144244176132, 2.05791769625638, 7.19243442312617, -1.96077928709078,
-0.553783081701012, -7.25008624702196, NaN, 0.170783287992522,
0.170039264494472, 4.8698143442217, -2.10898678290637, -1.31402042974571,
0.853317818145202, 1.06398935501804, -1.11766800412424, -2.40672822956725,
NaN), .Dim = c(10L, 7L), statistic = "z-Statistic", .Dimnames = list(
    c("Cleanliness", "Health", "Safety", "Cost", "Food", "Not being understood",
    "Friendliness of the people", "Boredom", "None of these",
    "NET"), c("Mexico", "France", "Great Britain", "Egypt", "Australia",
    "China", "NET")), name = "Residuals", questions = c("Q9",
"SUMMARY"))

tab <- structure(c(52.4539877300613, 51.2269938650307, 78.5276073619632,
8.89570552147239, 22.6993865030675, 17.4846625766871, 11.3496932515337,
3.06748466257669, 10.7361963190184, 100, 8.89570552147239, 6.74846625766871,
14.7239263803681, 60.1226993865031, 6.74846625766871, 43.2515337423313,
40.1840490797546, 4.9079754601227, 16.2576687116564, 100, 6.13496932515337,
5.8282208588957, 9.50920245398773, 51.2269938650307, 10.7361963190184,
4.9079754601227, 7.97546012269939, 5.21472392638037, 38.0368098159509,
100, 35.5828220858896, 38.9570552147239, 77.6073619631902, 44.1717791411043,
27.9141104294479, 47.5460122699387, 34.0490797546012, 3.98773006134969,
7.66871165644172, 100, 3.68098159509202, 4.60122699386503, 11.0429447852761,
57.0552147239264, 6.44171779141104, 3.37423312883436, 6.13496932515337,
1.22699386503067, 35.5828220858896, 100, 36.8098159509202, 44.7852760736196,
45.7055214723926, 52.1472392638037, 30.0613496932515, 60.1226993865031,
24.5398773006135, 5.21472392638037, 10.1226993865031, 100, 63.8036809815951,
65.6441717791411, 89.8773006134969, 78.5276073619632, 49.6932515337423,
72.0858895705521, 61.6564417177914, 15.3374233128834, 50.6134969325153,
100), .Dim = c(10L, 7L), statistic = "%", .Dimnames = list(c("Cleanliness",
"Health", "Safety", "Cost", "Food", "Not being understood", "Friendliness of the people",
"Boredom", "None of these", "NET"), c("Mexico", "France", "Great Britain",
"Egypt", "Australia", "China", "NET")), name = "Q9", questions = c("Q9",
"SUMMARY"))



expect_error(CorrespondenceAnalysis(tab, output = "Bubble Chart", row.color = rep(c("Red", "Green", "Blue"),3),
                       bubble.size = tab[-nrow(tab), 3]), NA)
expect_error(CorrespondenceAnalysis(tab, output = "Bubble Chart", row.color = rep(c("Red", "Green", "Blue"),3),
                    col.color = rep(c("Red", "Green", "Blue"),2),
                       bubble.size = tab[-nrow(tab), 3]), NA)
expect_error(CorrespondenceAnalysis(tab, output = "Bubble Chart",
                                    col.color = rep(c("Red", "Green", "Blue"),2),
                       bubble.size = tab[-nrow(tab), 3]), NA)
 })


test_that("focus",{
    for (output in c("Scatterplot", "Moonplot", "Text"))

                  {
                      for (focus in c(colnames(x.with.labels), rownames(x.with.labels))) {
                          expect_error(ca <- CorrespondenceAnalysis(x.with.labels, output = output, focus = focus, normalization = "Row principal",
                                                                    row.names.to.remove = "NET",  column.names.to.remove = "NET"), NA)
                          expect_error(capture.output(print(ca)), NA)
                      }
                  }


})


test_that("Diagnostics",{
    expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Diagnostics", normalization = "Row principal",
                                              row.names.to.remove = "NET",  column.names.to.remove = "NET")), NA)
    expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Diagnostics", focus = "V", normalization = "Row principal",
                                 row.names.to.remove = "NET",  column.names.to.remove = "NET")), "Output should not be set to 'Diagnostics' when 'Focus' has been set.")
})


data("colas", package = "flipExampleData")
z <- xtabs(~d1 + d2, data = colas)
z <- z[rowSums(z) > 0, colSums(z) > 0]

for (n in c("Principal", "Row principal", "Column principal", "None", "Row principal (scaled)", "Column principal (scaled)"))
    test_that(paste0("CorrespondenceAnalysis: focus by normalization ", n),
              {
                  for (focus in c(colnames(z), rownames(z))) {
                      expect_error(ca <- CorrespondenceAnalysis(z, output = "Scatterplot",
                                                                normalization = n, focus = focus), NA)
                      expect_error(capture.output(print(ca)), NA)
                  }
              }
    )


test_that(paste0("CorrespondenceAnalysis: supplementary points"),
      {
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary = "Coke"), NA)
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary = "missing"), "Supplementary rows or columns 'missing'.")
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary =
                                                paste0(rownames(x.with.labels), collapse = ",")), "At least 2 rows and 2 columns.")
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary =
                                                paste0(rownames(x.with.labels[c(-1, -2)]), collapse = ",")), NA)
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary = "Coke", focus = "coke"), NA)
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary = "FANTA, lift", focus = "coke"), NA)
      }
)


test_that(paste0("CorrespondenceAnalysis: mirroring"),
          {
              ca1 <- CorrespondenceAnalysis(x.with.labels)
              ca2 <- CorrespondenceAnalysis(x.with.labels, mirror.vertical = T, mirror.horizontal = T)
              expect_equal(-1 * ca1$original$rowcoord[, 1:2], ca2$original$rowcoord[, 1:2], tolerance = 0.000001, scale = 1)
              expect_equal(-1 * ca1$original$colcoord[, 1:2], ca2$original$colcoord[, 1:2], tolerance = 0.000001, scale = 1)
          }
)


test_that(paste0("CorrespondenceAnalysis: font sizes"),
          {
              print(CorrespondenceAnalysis(x.with.labels))
              for (f in c(5, 10, 15))
                  print(CorrespondenceAnalysis(x.with.labels),
                        title.font.size = f,
                           x.title.font.size = f,
                        y.title.font.size = f,
                        labels.font.size = f,
                        axis.font.size = f,
                        legend.font.size = f)
          }
)




test_that("Correspondence analysis quality",
          {
              expect_error(CAQuality(CorrespondenceAnalysis(x.with.labels)), NA)
              expect_error(CAQuality(CorrespondenceAnalysis(x.with.labels, focus = "Lift", supplementary = "Pepsi")), NA)
          })
