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
              c(  0.0198, 0.4604, 0.2151, 0.5235, 0.1151,
                0.12,   0.5457, 0.3041, 0.06312,    0.384,  0.06064),
              c(  0.01114,    0.4111, 0.1904, 0.4494, 0.06931,
                0.1112, 0.4716, 0.2859, 0.0495, 0.3296, 0.03837),
              c(  0.01114,    0.2373, 0.089,  0.2707, 0.05322,
                0.06436,    0.2756, 0.1656, 0.02967,    0.1916,
                0.02228),
              c(  0.0198, 0.177,  0.07054,    0.0297, 0.0396, 0.02719,
                0.0136, 0.02847,    0.0198, 0.02847,    0.02472),
              c(  0.4543, 0.1275, 0.07673,    0.02847,    0.07293,
                0.1077, 0.01609,    0.05198,    0.321,  0.01856,
                0.0297),
              c(  0.06807,    0.1089, 0.06064,    0.0198, 0.1174,
                0.04084,    0.01609,    0.01733,    0.03465,
                0.01361,    0.03589),
              c(  0.08168,    0.224,  0.1015, 0.04579,    0.04815,
                0.04084,    0.03094,    0.05562,    0.05322,
                0.04084,    0.02847)),nrow=8,byrow=TRUE)
x.with.labels <- x
dimnames(x.with.labels) <- list(Brand=c('Coke','V',"Red Bull","Lift Plus",'Diet.Coke','Fanta','Lift','Pepsi'),
                                Attribute=c('Kids', 'Teens',
                                            "Enjoy life",
                                            'Picks you up',
                                            'Refreshes',
                                            'Cheers you up',
                                            'Energy',   'Up-to-date',
                                            'Fun',  'When tired',
                                            'Relax'))

output = "Scatterplot"

test_that("Transpose occurs before row names removed",
{
    x.mult <- list(A = x.with.labels, B = x.with.labels + 1)
    attr(x.mult[[1]], "name") <- "A"
    attr(x.mult[[2]], "name") <- "B"
    res.mult <- CorrespondenceAnalysis(x.mult, transpose = TRUE, row.names.to.remove = "Fun")
    expect_equal(rownames(attr(res.mult, "ChartData")),
        c("A: Kids", "A: Teens", "A: Enjoy life", "A: Picks you up",
            "A: Refreshes", "A: Cheers you up", "A: Energy", "A: Up-to-date",
            "A: When tired", "A: Relax", "B: Kids", "B: Teens", "B: Enjoy life",
            "B: Picks you up", "B: Refreshes", "B: Cheers you up", "B: Energy",
            "B: Up-to-date", "B: When tired", "B: Relax", "Coke", "V", "Red Bull",
            "Lift Plus", "Diet.Coke", "Fanta", "Lift", "Pepsi"))

    res <- CorrespondenceAnalysis(x.with.labels, transpose = TRUE, row.names.to.remove = "Fun")
    expect_equal(rownames(attr(res, "ChartData")),
        c("Kids", "Teens", "Enjoy life", "Picks you up", "Refreshes",
            "Cheers you up", "Energy", "Up-to-date", "When tired", "Relax",
            "Coke", "V", "Red Bull", "Lift Plus", "Diet.Coke", "Fanta", "Lift",
            "Pepsi"))
})

test_that("Row/column names",
        {
            res0 <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
            expect_equal(res0$row.column.names, c("Brand", "Attribute"))
            res1 <- CorrespondenceAnalysis(x, show.gridlines = FALSE)
            expect_equal(res1$row.column.names, c("Rows", "Columns"))
            attr(x, "row.column.names") <- c("ABC", "DEF")
            res2 <- CorrespondenceAnalysis(x)
            expect_equal(res2$row.column.names, c("ABC", "DEF"))
        })

for (output in c("Scatterplot", "Moonplot", "Text"))
    test_that(paste0("CorrespondenceAnalysis is OK (mainly GetTidyTwoDimensionalArray) with ", output),
              {
        expect_error(res <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET"), NA)
        expect_equal(attr(res, "ChartType"), "X Y Scatter")
        expect_equal(attr(res, "ChartData"),
                     structure(list(`Dimension 1 (77.5%)` = c(-0.242824578883389,
                      0.447982869795063, 0.471121043396925, 0.464537497309872, -0.0913332694950237,
                      -1.2733742886416, -0.500402183998914, -0.27373105043976, -1.31538815485031,
                      0.037206733329165, -0.0787671942486661, 0.488530282325173, -0.29457007350688,
                      -0.235625518884957, 0.601327833549599, 0.287370010414348, -0.923056165508813,
                      0.453772049498003, -0.25000257344162), `Dimension 2 (17.0%)` = c(-0.202539757759669,
                      0.112245635990019, 0.132030273935906, 0.146363526885915, -0.537816212784355,
                      0.466468539885024, -0.497665196949348, -0.261639180363143, 0.368144823830639,
                      -0.23730729602076, -0.293995121319046, 0.176112525700419, -0.419761150156407,
                      -0.0573900558313915, 0.274787466614528, 0.0802524115772252, 0.233157762736366,
                      0.122368362334109, -0.454640909480694), Group = c("Brand", "Brand",
                      "Brand", "Brand", "Brand", "Brand", "Brand", "Brand", "Attribute",
                      "Attribute", "Attribute", "Attribute", "Attribute", "Attribute",
                      "Attribute", "Attribute", "Attribute", "Attribute", "Attribute"
                      )), class = "data.frame", row.names = c("Coke", "V", "Red Bull",
                      "Lift Plus", "Diet.Coke", "Fanta", "Lift", "Pepsi", "Kids", "Teens",
                      "Enjoy life", "Picks you up", "Refreshes", "Cheers you up", "Energy",
                      "Up-to-date", "Fun", "When tired", "Relax"), scatter.variable.indices = c(x = 1,
                      y = 2, sizes = NA, colors = 3)))

        expect_error(CorrespondenceAnalysis(x, output=output), NA)
        # 3D array with no names
        z <- array(NA, c(8,11,2))
        z[,,1] <- x
        attr(z, "name") <- "My QTable"
        attr(z, "questions") <- "SUMMARY"
        expect_warning(CorrespondenceAnalysis(z, output = output))
        dimnames(z) <- list(dimnames(x.with.labels)[[1]], dimnames(x.with.labels)[[2]], 1:2)
        expect_warning(CorrespondenceAnalysis(z, output = output))
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
              expect_error(CorrespondenceAnalysis(x2, output = output), "Input tables cannot contain negative values.")
          })


test_that("Row and column labels",
          {
                x <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
                expect_equal(x$row.column.names, c("Brand",  "Attribute"))

                attr(x.with.labels, "row.column.names") <- c("", "")
                x <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
                expect_equal(x$row.column.names, c("",  ""))

                attr(x.with.labels, "row.column.names") <- c("My rows", "My columns")
                x <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
                expect_equal(x$row.column.names, c("My rows",  "My columns"))

                names(dimnames(x.with.labels)) <- NULL
                attr(x.with.labels, "row.column.names") <- NULL
                x <- CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
                expect_equal(x$row.column.names, c("Rows",  "Columns"))

                xd <- array(runif(9), dim = c(3, 3, 3), dimnames = list(A = c("a","a","a"), B = c("a","a","a"), C = c("a","a","a")))
                attr(xd, "questions") <- "NET"
                attr(xd, "name") <- "Super Sweet QTable"
                expect_warning(x <- CorrespondenceAnalysis(xd), "Multiple statistics")
                expect_equal(x$row.column.names, c("A",  "B"))

                names(dimnames(xd)) <- NULL
                expect_warning(x <- CorrespondenceAnalysis(xd), "^Multiple statistics")
                expect_equal(x$row.column.names, c("Rows",  "Columns"))

                attr(xd, "row.column.names") <- c("Alpha", "Beta")
                expect_warning(x <- CorrespondenceAnalysis(xd), "^Multiple statistics")
                expect_equal(x$row.column.names, c("Alpha",  "Beta"))

          })


test_that("Logos",
          {
              urls <- sprintf("https://displayrcors.displayr.com/images/%s_grey.svg",
                              c("apple","baby","car","stickman","stickwoman","chicken","cow","thumbsup","rocket","tools"))
              data("colas", package = "flipExampleData")
              z = xtabs(~d1 + d2, data = colas)
              z = z[rowSums(z) > 0, colSums(z) > 0]
              colnames(z) <- LETTERS[1:8]
              expect_error(CorrespondenceAnalysis(z, logos=urls[1:9]), NA)
              expect_warning(print(CorrespondenceAnalysis(z, logos=urls[1:9], transpose=T)),
                             "Number of URLs supplied in logos (9) is not equal to the number of columns", fixed = TRUE)
              expect_warning(print(CorrespondenceAnalysis(z, logos=urls[1:4])),
                             "Number of URLs supplied in logos (4) is not equal to the number of rows in the table", fixed = TRUE)

              z2 <- z + runif(72)
              zz <- list(z, z2)
              expect_warning(CorrespondenceAnalysis(zz, logos=urls[1:9]), "Tables have been automatically assigned names")
              attr(zz[[1]], "name") <- "T1"
              attr(zz[[2]], "name") <- "T2"
              expect_warning(print(CorrespondenceAnalysis(zz, logos=urls[1:9], transpose=T)),
                             "Number of URLs supplied in logos (9) is not equal to the number of columns in the table", fixed = TRUE)
              expect_warning(print(CorrespondenceAnalysis(zz, logos=urls[1:4])),
                             "Number of URLs supplied in logos (4) is not equal to the number of rows in the table", fixed = TRUE)

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
                expect_error(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", show.gridlines = FALSE))
                bsizes = x.with.labels[,1]
                expect_error(res <- CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes), NA)
                expect_equal(attr(res, "ChartType"), "Bubble")
                expect_equal(attr(res, "ChartData"),
                             structure(list(`Dimension 1 (77.5%)` = c(-0.242824578883389,
                              0.447982869795063, 0.471121043396925, 0.464537497309872, -0.0913332694950237,
                              -1.2733742886416, -0.500402183998914, -0.27373105043976, -1.31538815485031,
                              0.037206733329165, -0.0787671942486661, 0.488530282325173, -0.29457007350688,
                              -0.235625518884957, 0.601327833549599, 0.287370010414348, -0.923056165508813,
                              0.453772049498003, -0.25000257344162),
                              `Dimension 2 (17.0%)` = c(-0.202539757759669,
                              0.112245635990019, 0.132030273935906, 0.146363526885915, -0.537816212784355,
                              0.466468539885024, -0.497665196949348, -0.261639180363143, 0.368144823830639,
                              -0.23730729602076, -0.293995121319046, 0.176112525700419, -0.419761150156407,
                              -0.0573900558313915, 0.274787466614528, 0.0802524115772252, 0.233157762736366,
                              0.122368362334109, -0.454640909480694), Size = c(0.3004, 0.0198,
                              0.01114, 0.01114, 0.0198, 0.4543, 0.06807, 0.08168, 0.00605733333333333,
                              0.00605733333333333, 0.00605733333333333, 0.00605733333333333,
                              0.00605733333333333, 0.00605733333333333, 0.00605733333333333,
                              0.00605733333333333, 0.00605733333333333, 0.00605733333333333,
                              0.00605733333333333), Group = c("Brand", "Brand", "Brand", "Brand",
                              "Brand", "Brand", "Brand", "Brand", "Attribute", "Attribute",
                              "Attribute", "Attribute", "Attribute", "Attribute", "Attribute",
                              "Attribute", "Attribute", "Attribute", "Attribute")),
                              class = "data.frame", row.names = c("Coke",
                              "V", "Red Bull", "Lift Plus", "Diet.Coke", "Fanta", "Lift", "Pepsi",
                              "Kids", "Teens", "Enjoy life", "Picks you up", "Refreshes", "Cheers you up",
                              "Energy", "Up-to-date", "Fun", "When tired", "Relax"),
                              scatter.variable.indices = c(x = 1, y = 2, sizes = 3, colors = 4)))

                expect_error(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes, row.names.to.remove = "Coke"), NA)
                expect_error(CorrespondenceAnalysis(t(x.with.labels), output = "Bubble Chart", transpose = TRUE,
                    bubble.size = bsizes, row.names.to.remove = "Coke"), NA)

                expect_error(CorrespondenceAnalysis(t(x.with.labels), output = "Bubble Chart", transpose = TRUE,
                    bubble.size = bsizes, column.names.to.remove = "Coke"), NA)
                expect_error(CorrespondenceAnalysis(t(x.with.labels), output = "Bubble Chart", transpose = TRUE,
                    bubble.size = bsizes, row.names.to.remove = "Coke"), NA)
                expect_error(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes, transpose = TRUE),
                    "To use a bubble chart, the table of bubble sizes needs to include all the row labels used in the analysis. The values for 'Kids', 'Teens', 'Enjoy life', 'Picks you up', 'Refreshes', 'Cheers you up', 'Energy', 'Up-to-date', 'Fun', 'When tired' and 'Relax' are missing.",
                    fixed = TRUE)

                expect_warning(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart",
                    bubble.size = c(Extra = 1, More = 2, bsizes)), "Bubble sizes for 'Extra', 'More' were not used as they do not appear in the row labels used in the analysis.")
                expect_error(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes[-1]),
                    "The value for 'Coke' is missing.")
                expect_error(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart",
                    bubble.size = c(bsizes[-1], 'coke ' = 2)), NA)

                names(bsizes)[2] = "Dog"
                expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes)), "The value for 'V' is missing")
                names(bsizes)[2] = names(bsizes)[1]
                expect_warning(expect_error((CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes)),
                        "The value for 'V' is missing"), "The table of bubble sizes contains duplicated row labels. Only the value from the first duplicate of 'Coke' was used.", fixed = TRUE)
                expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = 1:length(bsizes))),
                    "The table of bubble sizes need to be named to match the row labels used in the analysis.")

                expect_error(print(suppressWarnings(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes[-1]))))
                expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = x.with.labels[,1] *10000, bubble.title = "Importance (%)")), NA)

                expect_warning(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = x.with.labels),
                    "The table of bubble sizes contains more than one column. Only the first column of bubble sizes was used.")

                set.seed(12332)
                x = matrix(round(runif(100)*100), 10, dimnames = list(letters[1:10], LETTERS[1:10]))
                sizes = 1:10
                names(sizes) = letters[1:10]
                sizes = sample(sizes, length(sizes))
                expect_error(print(CorrespondenceAnalysis(x, output = "Bubble Chart", bubble.size = sizes)), NA)
                # ignore case
                names(sizes) = LETTERS[1:10]
                expect_error(print(CorrespondenceAnalysis(x, output = "Bubble Chart", bubble.size = sizes)), NA)
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

z <- z[, 3:4]
test_that("CorrespondenceAnalysis: focus 1D warning",
          {
                expect_warning(ca <- CorrespondenceAnalysis(z, output = "Scatterplot", focus = "25 to 29"),
                               "Output is one dimensional and focus has no effect.")
          }
)

test_that(paste0("CorrespondenceAnalysis: supplementary points"),
      {
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary = "Coke"), NA)
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary = "missing"), "Supplementary rows or columns 'missing'.")
        expect_error(CorrespondenceAnalysis(x.with.labels, supplementary =
                                                paste0(rownames(x.with.labels)[-1], collapse = ",")), "At least 2 rows and 2 columns.")
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


test_that("Reg. Test: Corr. Anal. of a Table - correspondence.analysis1",
{
    x <- structure(c("25.2327048028994", "31.2881504763389", "30.9835063713764",
    "17.5546469946982", "33.2850525773139", "21.3918060868462", "14.8891326905278",
    "99.999999992966", "32.1856718881157", "39.0384865558457", "38.7008828633533",
    "23.043980297302", "41.2332045095876", "27.6781998955081", "19.7445739902877",
    "99.9999999949984", "19.3901751154243", "24.5030129463578", "24.24112651755",
    "13.1766253337998", "26.232192530017", "16.2453846830681", "11.0864828737829",
    "99.9999999901313", "22.3980226347181", "28.0282076203562", "27.7424891486241",
    "15.4048099182854", "29.9076106591684", "18.8796593933267", "13.0142006255211",
    "99.9999999917754", "25.6383779654847", "31.7498370032718", "31.4427546535753",
    "17.866375634708", "33.7617270166789", "21.753694692716", "15.1622330335653",
    "99.9999999931149", "25.3986416353672", "31.4771442945425", "31.1714948165763",
    "17.682032200925", "33.4802311923131", "21.5397609802415", "15.0006948800344",
    "99.9999999930275", "18.6950249396378", "23.6784065423265", "23.422599150159",
    "12.6692243563911", "25.3689477903583", "15.6411012525024", "10.6496959686249",
    "99.9999999896761", "25.1773988570791", "31.2251150086532", "30.9208084668817",
    "17.5122283403448", "33.2199390423663", "21.342515643933", "14.8519946407419",
    "99.9999999929453", "38.3662387414414", "45.6491679510049", "45.2968741467223",
    "28.1990522244833", "47.9233721374194", "33.4197641302466", "24.3955306686821",
    "99.9999999961865", "87.274341983319", "90.2471601302471", "90.1213865181602",
    "81.22746990624", "91.0222544777148", "84.6863241611387", "78.0460628231802",
    "99.9999999996539", "B I", "C D E F G H", "C D G H", "B I", "C D E F G H",
    "c d g H", "H i", "-", "i", "C D E F G H", "A C D G H", "I",
    "C D E F G H", "A C D G H", "A D e H I", "-", "A B I", "E F H",
    "D H", "B I", "D E F G H", "h", "A B D E F H I", "-", "A B C E F G I",
    "E F H", "h", "A B C E F G I", "F H", "h", "H", "-", "A B C G I",
    "F", "A C D G H", "A B C I", "D F H", "A B C D f G H I", "a D H I",
    "-", "A B C G I", "", "A B C D E G H", "A B C g I", "", "A B C D G H",
    "A D H I", "-", "a B I", "E F H", "D H", "a B c I", "D E F H",
    "", "A B C D E F H I", "-", "A B C D E F G I", "E F", "", "A B C D E F G I",
    "", "", "", "-", "", "A B C D E F G H", "A B C D E G H", "",
    "A B C D E F G H", "A B C D G H", "H", "-", "-", "-", "-", "-",
    "-", "-", "-", "-"), .Dim = c(8L, 10L, 2L), .Dimnames = list(
        c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
        "Pepsi Max", "None of these", "NET"), c("Feminine", "Health-conscious",
        "Innocent", "Older", "Open to new experiences", "Rebellious",
        "Sleepy", "Traditional", "Weight-conscious", "NET"), c("Expected %",
        "Column Comparisons")), name = "q5", questions = c("q5",
                                                         "SUMMARY"))

    expect_warning(CorrespondenceAnalysis(x), "^Multiple statistics")
})

test_that("Duplicated labels are displayed nicely",
{
    dat <- structure(c(2.63157894736842, 0, 7.89473684210526, 13.1578947368421,
            10.5263157894737, 10.5263157894737, 13.1578947368421, 13.1578947368421,
            28.9473684210526, 100, 2.54237288135593, 4.23728813559322, 3.38983050847458,
            11.0169491525424, 12.7118644067797, 11.0169491525424, 9.32203389830508,
            17.7966101694915, 27.9661016949153, 100, 4.80769230769231, 1.92307692307692,
            6.73076923076923, 10.5769230769231, 12.5, 11.5384615384615, 6.73076923076923,
            15.3846153846154, 29.8076923076923, 100, 0, 1.49253731343284,
            10.4477611940298, 14.9253731343284, 13.4328358208955, 7.46268656716418,
            7.46268656716418, 16.4179104477612, 28.3582089552239, 100, 2.75229357798165,
            2.44648318042813, 6.42201834862385, 11.9266055045872, 12.5382262996942,
            10.3975535168196, 8.56269113149847, 16.2079510703364, 28.7461773700306,
            100), statistic = "Column %", .Dim = c(10L, 5L), .Dimnames = list(
                c("Every or nearly every day", "4 to 5 days a week", "2 to 3 days a week",
                "Once a week", "Once every 2 weeks", "Once a month", "Once every 3 months",
                "Once or twice a year", "Never", "NET"), c("Never", "Rarely (once or twice in a year)",
                "Quite often (about once every month)", "Every chance I get (every week I look for new competitions t",
                "NET")), name = "Q12. How  often do you drink cola with alcohol by Q13. Competition participation", questions = c("Q12. How  often do you drink cola with alcohol",
            "Q13. Competition participation"))
    ca <- CorrespondenceAnalysis(dat)
    expect_equal(rownames(attr(ca, "ChartData"))[2], "4 to 5 days a week")
})

test_that("Missing values",
{
    xx <- list(table.Q14 = structure(c(NA, 20.3525641025641, NA, 16.3461538461538,
            15.7852564102564, 26.9230769230769, 58.4134615384615, 22.9967948717949,
            14.1826923076923, NA, 21.6346153846154, NA, NA, 23.7179487179487,
            NA, 26.9230769230769, 16.3461538461538, 38.5416666666667, 35.0961538461538,
            12.2596153846154, 17.9487179487179, NA, 11.1378205128205, NA,
            NA, 7.7724358974359, NA, 4.40705128205128, 8.65384615384615,
            36.2179487179487, 79.4871794871795, 9.53525641025641, 2.88461538461538,
            NA, 8.65384615384615, NA, NA, 28.0448717948718, NA, 14.8237179487179,
            26.6826923076923, 23.4775641025641, 41.3461538461538, 22.4358974358974,
            13.5416666666667, NA, 13.0608974358974, NA, NA, 29.4070512820513,
            NA, 31.8910256410256, 21.9551282051282, 46.7147435897436, 41.5865384615385,
            14.5032051282051, 23.9583333333333, NA, 13.5416666666667, NA,
            NA, 14.3429487179487, NA, 12.7403846153846, 10.5769230769231,
            17.6282051282051, 70.8333333333333, 16.5064102564103, 11.0576923076923,
            NA, 14.4230769230769, NA, NA, 45.0320512820513, NA, 13.5416666666667,
            31.4903846153846, 52.3237179487179, 39.8237179487179, 27.1634615384615,
            15.3846153846154, NA, 21.3942307692308, NA, NA, 43.6698717948718,
            NA, 12.4198717948718, 27.4038461538462, 58.4134615384615, 42.0673076923077,
            23.7179487179487, 11.5384615384615, NA, 13.3814102564103, NA,
            NA, 24.6794871794872, NA, 11.3782051282051, 21.875, 40.5448717948718,
            36.8589743589744, 22.9166666666667, 10.2564102564103, NA, 24.9198717948718,
            NA, NA, 35.0160256410256, NA, 18.4294871794872, 28.9262820512821,
            46.875, 47.1955128205128, 25.400641025641, 20.5128205128205,
            NA, 22.4358974358974, NA, NA, 83.4935897435898, NA, 69.4711538461538,
            80.7692307692308, 93.75, 92.8685897435898, 65.3044871794872,
            68.1089743589744, NA, 73.7179487179487, NA), .Dim = 12:11, statistic = "%", .Dimnames = list(
                c("Burger Shack", "Burger Chef", "Nuovo Burger", "Lucky's Pizza",
                "Southern Fried Chicken", "Arnold's", "Pret'a'pane", "Ma's burgers",
                "Nero's Pizza", "Bread Basket", "None of These", "NET"),
                c("Care about the quality of their food", "Good value for money",
                "Has healthy food options", "Has the best tasting food",
                "Is Affordable", "Their food is always fresh", "Has a good range of drinks to choose from",
                "Easy to eat when you're on the go", "Offers a real variety of food",
                "Clean and well-kept restaurants", "NET")), name = "table.Q14", questions = c("Q14",
            "SUMMARY")), table.Q14.2 = structure(c(NA, 18.9531680440771,
            8.09716599190283, 13.7741046831956, 15.702479338843, 25.1790633608815,
            53.9393939393939, 19.504132231405, 13.4435261707989, NA, 26.4462809917355,
            NA, NA, 23.0853994490358, 6.07287449392713, 26.4462809917355,
            16.969696969697, 35.5371900826446, 32.1763085399449, 12.3415977961433,
            22.2038567493113, NA, 14.7107438016529, NA, NA, 7.32782369146005,
            3.23886639676113, 4.79338842975207, 8.70523415977961, 35.0413223140496,
            79.9449035812672, 9.09090909090909, 3.91184573002755, NA, 9.6969696969697,
            NA, NA, 22.7548209366391, 12.9554655870445, 11.9559228650138,
            27.0523415977961, 21.5977961432507, 40.6060606060606, 18.9531680440771,
            13.1129476584022, NA, 17.3553719008264, NA, NA, 28.099173553719,
            8.09716599190283, 33.3333333333333, 23.9669421487603, 44.8484848484849,
            37.465564738292, 15.0964187327824, 27.5482093663912, NA, 15.9779614325069,
            NA, NA, 13.5537190082645, 8.50202429149797, 10.7438016528926,
            11.0743801652893, 16.969696969697, 69.9173553719008, 14.2148760330579,
            10.0275482093664, NA, 15.5922865013774, NA, NA, 41.9834710743802,
            14.17004048583, 15.1515151515152, 29.2011019283747, 50.6887052341598,
            38.2920110192837, 26.7217630853994, 15.8677685950413, NA, 23.3608815426997,
            NA, NA, 42.6997245179063, 12.1457489878543, 11.0192837465565,
            25.7851239669421, 57.3002754820937, 40.3305785123967, 22.534435261708,
            12.396694214876, NA, 15.2066115702479, NA, NA, 22.9201101928375,
            11.7408906882591, 9.47658402203857, 21.3223140495868, 41.5426997245179,
            32.2314049586777, 19.9449035812672, 9.20110192837465, NA, 28.4848484848485,
            NA, NA, 33.2782369146005, 16.1943319838057, 17.4104683195592,
            25.7851239669421, 45.8953168044077, 46.3911845730028, 24.7382920110193,
            18.5674931129477, NA, 24.4628099173554, NA, NA, 81.7079889807163,
            40.4858299595142, 68.3195592286501, 78.7878787878788, 92.9476584022039,
            92.7823691460055, 64.1322314049587, 66.6115702479339, NA, 76.0881542699724,
            NA), .Dim = 12:11, statistic = "%", .Dimnames = list(c("Burger Shack",
            "Burger Chef", "Nuovo Burger", "Lucky's Pizza", "Southern Fried Chicken",
            "Arnold's", "Pret'a'pane", "Ma's burgers", "Nero's Pizza", "Bread Basket",
            "None of These", "NET"), c("Care about the quality of their food",
            "Good value for money", "Has healthy food options", "Has the best tasting food",
            "Is Affordable", "Their food is always fresh", "Has a good range of drinks to choose from",
            "Easy to eat when you're on the go", "Offers a real variety of food",
            "Clean and well-kept restaurants", "NET")), name = "table.Q14.2", questions = c("Q14",
            "SUMMARY")), table.Q14.3 = structure(c(30.4651162790698, 16.0893854748603,
            13.6312849162011, 13.854748603352, 14.3575418994413, 22.4581005586592,
            49.608938547486, 15.8100558659218, 12.5698324022346, 25.1162790697674,
            26.7039106145251, 100, 8.13953488372093, 25.0837988826816, 6.81564245810056,
            24.6927374301676, 19.9441340782123, 33.0167597765363, 29.4972067039106,
            10.7821229050279, 19.4413407821229, 5.81395348837209, 13.5195530726257,
            100, 11.3953488372093, 7.15083798882682, 3.5195530726257, 3.79888268156425,
            7.87709497206704, 31.0614525139665, 74.9720670391062, 7.76536312849162,
            3.07262569832402, 44.8837209302326, 11.3966480446927, 100, 30.6976744186047,
            22.7374301675978, 14.804469273743, 14.0782122905028, 25.9776536312849,
            20.6145251396648, 35.2513966480447, 16.0893854748603, 11.5083798882682,
            13.953488372093, 15.9776536312849, 100, 7.90697674418605, 32.2346368715084,
            8.10055865921788, 32.6815642458101, 25.5865921787709, 45.6983240223464,
            38.5474860335196, 14.5251396648045, 28.1564245810056, 10.6976744186047,
            14.0223463687151, 100, 24.4186046511628, 12.9050279329609, 7.93296089385475,
            11.0055865921788, 10.1675977653631, 15.4189944134078, 64.4134078212291,
            11.340782122905, 10.3351955307263, 30, 17.3743016759777, 100,
            19.3023255813953, 38.1564245810056, 17.877094972067, 12.0670391061453,
            26.9832402234637, 47.9329608938547, 36.2011173184358, 21.7877094972067,
            13.072625698324, 10.2325581395349, 25.3072625698324, 100, 11.8604651162791,
            38.9385474860335, 12.122905027933, 13.2402234636872, 24.804469273743,
            55.8659217877095, 40.0558659217877, 19.7206703910615, 12.9050279329609,
            16.2790697674419, 14.804469273743, 100, 14.8837209302326, 21.8994413407821,
            12.9050279329609, 11.1731843575419, 20.5586592178771, 38.9944134078212,
            29.9441340782123, 17.9329608938547, 10.1117318435754, 10.9302325581395,
            28.9385474860335, 100, 33.4883720930233, 28.659217877095, 17.7653631284916,
            16.2569832402235, 22.1787709497207, 40.9497206703911, 41.6201117318436,
            21.5642458100559, 16.2569832402235, 20, 24.3016759776536, 100,
            62.3255813953488, 79.7765363128492, 45.9217877094972, 67.2625698324022,
            77.8212290502793, 91.8994413407821, 90.1117318435754, 59.8324022346369,
            65.8100558659218, 63.2558139534884, 75.2513966480447, 100), .Dim = 12:11, statistic = "%", .Dimnames = list(
                c("Burger Shack", "Burger Chef", "Nuovo Burger", "Lucky's Pizza",
                "Southern Fried Chicken", "Arnold's", "Pret'a'pane", "Ma's burgers",
                "Nero's Pizza", "Bread Basket", "None of These", "NET"),
                c("Care about the quality of their food", "Good value for money",
                "Has healthy food options", "Has the best tasting food",
                "Is Affordable", "Their food is always fresh", "Has a good range of drinks to choose from",
                "Easy to eat when you're on the go", "Offers a real variety of food",
                "Clean and well-kept restaurants", "NET")), name = "table.Q14.3", questions = c("Q14",
            "SUMMARY")))

            expect_error(CorrespondenceAnalysis(xx), "Row 'Burger Shack', 'Nuovo Burger', 'Bread Basket' contains only zeros or NAs.", fixed = TRUE)
            expect_error(CorrespondenceAnalysis(xx, transpose = TRUE), "Row 'Burger Shack', 'Nuovo Burger', 'Bread Basket' contains only zeros or NAs.", fixed = TRUE)

            expect_error(CorrespondenceAnalysis(xx[[1]]), "Row 'Burger Shack', 'Nuovo Burger', 'Bread Basket' contains only zeros or NAs.", fixed = TRUE)
            expect_error(CorrespondenceAnalysis(t(xx[[1]])), "Column 'Burger Shack', 'Nuovo Burger', 'Bread Basket' contains only zeros or NAs.", fixed = TRUE)
            expect_error(CorrespondenceAnalysis(xx[[1]], transpose = TRUE), "Row 'Burger Shack', 'Nuovo Burger', 'Bread Basket' contains only zeros or NAs.", fixed = TRUE)

            expect_error(CorrespondenceAnalysis(xx, row.names.to.remove = "NET, Total, SUM, Burger Shack, Nuovo Burger, Bread Basket"), NA)
            expect_error(CorrespondenceAnalysis(xx[[1]], row.names.to.remove = "NET, Total, SUM, Burger Shack, Nuovo Burger, Bread Basket"), NA)

            xx[[1]][2,2] <- NA
            expect_error(CorrespondenceAnalysis(xx[[1]], row.names.to.remove = "NET, Total, SUM, Burger Shack, Nuovo Burger, Bread Basket"), "Input table cannot contain missing or infinite values")

})

test_that("Output contains the right class for extension buttons", {
  # NOTE: if any of the tests below fail due to class names changing, ALL
  #       extension buttons in the wiki that refer to this class name should
  #       be updated with the new class name.
    result <- CorrespondenceAnalysis(x.with.labels)

    expect_true(inherits(result, "CorrespondenceAnalysis"))
})
