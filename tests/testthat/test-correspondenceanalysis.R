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
CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")


for (output in c("Scatterplot", "Moonplot", "Text"))
    test_that(paste0("CorrespondenceAnalysis is OK (mainly GetTidyTwoDimensionalArray) with ", output),
              {
        expect_error(CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET"), NA)
        expect_error(CorrespondenceAnalysis(x), NA)
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



test_that("Bubble charts",
          {
#                 expect_error(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart"))
#                 bsizes = x.with.labels[,1]
#                 expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes)), NA)
#                 expect_error(print(suppressWarnings(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = bsizes[-1]))))
#                 expect_error(print(CorrespondenceAnalysis(x.with.labels, output = "Bubble Chart", bubble.size = x.with.labels[,1] *10000, bubble.title = "Importance (%)")), NA)
#
#
#
#
#                 zzz = structure(c(26.9113149847095, 29.3577981651376, 22.6299694189602,
# 29.6636085626911, 17.737003058104, 22.3241590214067, 33.6391437308869,
# 55.045871559633, 45.8715596330275, 17.1253822629969, 18.6544342507645,
# 41.5902140672783, 40.6727828746177, 12.8440366972477, 29.6636085626911,
# 24.7706422018349, 4.58715596330275, 51.9877675840979, 23.5474006116208,
# 31.4984709480122, 17.4311926605505, 18.348623853211, 31.4984709480122,
# 9.1743119266055, 19.8776758409786, 3.05810397553517, 42.2018348623853,
# 31.1926605504587, 25.9938837920489, 58.4097859327217, 33.6391437308869,
# 64.5259938837921, 14.0672782874618, 44.0366972477064, 96.9418960244648,
# 27.82874617737, 36.085626911315, 29.6636085626911, 68.1957186544342,
# 54.1284403669725, 6.42201834862385, 55.3516819571865, 1.8348623853211,
# 30.8868501529052, 48.0122324159021, 22.9357798165138, 21.1009174311927,
# 26.9113149847095, 9.1743119266055, 34.2507645259939, 64.5259938837921,
# 65.1376146788991, 22.6299694189602, 49.8470948012232, 26.2996941896024,
# 31.8042813455657, 70.3363914373089, 27.82874617737, 9.1743119266055,
# 55.3516819571865, 92.3547400611621, 18.6544342507645, 6.72782874617737,
# 32.1100917431193, 34.2507645259939, 45.8715596330275, 0.611620795107034,
# 33.9449541284404, 32.1100917431193, 99.0825688073395, 22.6299694189602,
# 20.4892966360856, 19.2660550458716, 12.8440366972477, 21.4067278287462,
# 57.4923547400612, 14.3730886850153, 58.7155963302752, 11.3149847094801,
# 22.9357798165138, 7.64525993883792, 7.95107033639144, 11.0091743119266,
# 22.9357798165138, 20.1834862385321, 2.44648318042813, 21.7125382262997,
# 9.1743119266055, 13.1498470948012, 4.58715596330275, 4.58715596330275,
# 22.6299694189602, 18.0428134556575, 23.5474006116208, 3.36391437308868,
# 14.6788990825688, 14.9847094801223, 10.3975535168196, 19.5718654434251,
# 13.1498470948012, 15.2905198776758, 76.4525993883792, 18.960244648318,
# 11.0091743119266, 96.0244648318043, 23.2415902140673, 28.1345565749235,
# 14.9847094801223, 7.95107033639144, 12.5382262996942, 60.5504587155963,
# 18.348623853211, 57.7981651376147, 12.5382262996942, 13.7614678899083,
# 15.5963302752294, 13.7614678899083, 14.3730886850153, 43.4250764525994,
# 12.8440366972477, 0.611620795107034, 9.1743119266055, 16.2079510703364,
# 12.2324159021407, 3.97553516819572, 4.28134556574923, 10.3975535168196,
# 14.9847094801223, 29.6636085626911, 0.917431192660551, 3.36391437308868,
# 24.1590214067278, 22.3241590214067, 14.0672782874618, 18.6544342507645,
# 13.1498470948012, 76.4525993883792, 15.5963302752294, 19.2660550458716,
# 97.5535168195719, 24.4648318042813, 8.25688073394496, 25.6880733944954,
# 7.95107033639144, 14.0672782874618, 9.78593272171254, 8.25688073394496,
# 17.4311926605505, 13.1498470948012, 22.6299694189602, 27.82874617737,
# 18.960244648318, 17.737003058104, 29.9694189602446, 24.7706422018349,
# 9.48012232415902, 8.56269113149847, 11.9266055045872, 11.3149847094801,
# 15.9021406727829, 18.960244648318, 8.86850152905199, 22.6299694189602,
# 38.8379204892966, 14.9847094801223, 2.75229357798165, 7.64525993883792,
# 25.3822629969419, 24.4648318042813, 8.56269113149847, 17.737003058104,
# 5.5045871559633, 38.2262996941896, 7.03363914373089, 71.2538226299694,
# 15.5963302752294, 37.3088685015291, 18.6544342507645, 36.3914373088685,
# 15.9021406727829, 10.0917431192661, 47.4006116207951, 30.8868501529052,
# 43.7308868501529, 15.5963302752294, 36.085626911315, 40.3669724770642,
# 33.9449541284404, 7.3394495412844, 23.5474006116208, 36.3914373088685,
# 6.42201834862385, 50.4587155963303, 48.6238532110092, 44.6483180428135,
# 45.2599388379205, 14.0672782874618, 26.2996941896024, 6.42201834862385,
# 37.0030581039755, 3.97553516819572, 52.9051987767584, 37.6146788990826,
# 12.8440366972477, 40.9785932721713, 27.217125382263, 40.6727828746177,
# 11.9266055045872, 49.5412844036697, 97.8593272171254, 14.0672782874618,
# 24.4648318042813, 17.1253822629969, 33.3333333333333, 36.3914373088685,
# 8.56269113149847, 34.2507645259939, 2.14067278287462, 16.2079510703364,
# 30.5810397553517, 16.8195718654434, 12.5382262996942, 14.0672782874618,
# 9.78593272171254, 19.5718654434251, 33.3333333333333, 37.9204892966361,
# 15.5963302752294, 31.1926605504587, 17.737003058104, 22.3241590214067,
# 37.9204892966361, 12.2324159021407, 14.3730886850153, 24.4648318042813,
# 53.822629969419, 23.8532110091743, 9.48012232415902, 12.8440366972477,
# 16.2079510703364, 35.1681957186544, 0, 19.8776758409786, 21.7125382262997,
# 97.2477064220184, 22.1057230231542, 26.2996941896024, 21.1446046308432,
# 28.0471821756225, 24.5958934032329, 25.0327653997379, 30.2315421581477,
# 31.9790301441678, 24.8143294014854, 24.3774574049803, 20.7951070336391,
# 22.3241590214067, 22.6736566186107, 19.3534294451726, 23.5474006116208,
# 24.5085190039318, 21.9309742245522, 25.4259501965924, 27.129750982962,
# 20.6640454346876, 20.6640454346876, 26.0812581913499, 21.9309742245522,
# 18.7418086500655, 22.2804718217562, 24.8580166011359, 26.343381389253,
# 20.4456094364351, 20.2708606378331, 27.1734381826125, 26.867627785059,
# 37.7457404980341, 21.7999126256007, 26.3870685889034, 93.7090432503277
# ), .Dim = c(35L, 8L), statistic = "Column %", .Dimnames = list(
#     c("beautiful", "carefree", "charming", "confident", "down-to-earth",
#     "feminine", "fun", "health-conscious", "hip", "honest", "humorous",
#     "imaginative", "individualistic", "innocent", "intelligent",
#     "masculine", "older", "open to new experiences", "outdoorsy",
#     "rebellious", "reckless", "reliable", "sexy", "sleepy", "tough",
#     "traditional", "trying to be cool", "unconventional", "up-to-date",
#     "upper-class", "urban", "weight-conscious", "wholesome",
#     "youthful", "NET"), c("Coke Zero", "Coke", "Diet Coke", "Diet Pepsi",
#     "None of these", "Pepsi Max", "Pepsi", "NET")), name = "Performance by Brand", questions = c("Performance",
# "Brand [largeColaStacked.sav]"))

          })
# zSize = zzz[, 1]
# zSize = zSize[-length(zSize)]
# print(dim(zzz))
# print(length(zSize))
# CorrespondenceAnalysis(zzz, output = "Bubble Chart", bubble.size = zSize)
