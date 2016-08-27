context("Correspondence Analysis")

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

output = "ggplot2"
CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")
output = "Scatterplot"
CorrespondenceAnalysis(x.with.labels, output = output, row.names.to.remove = "NET",  column.names.to.remove = "NET")


for (output in c("Scatterplot", "Moonplot", "Text", "ggplot2"))
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

for (output in c("Scatterplot", "Moonplot", "ggplot2", "Text"))
    test_that(paste("CorrespondenceAnalysis prints", output),
    {
        expect_error(CorrespondenceAnalysis(x.with.labels, row.names.to.remove = "NET",  column.names.to.remove = "NET", output = output), NA)
        expect_error(CorrespondenceAnalysis(x,output = output), NA)
    })
#
# test_that("Normalization warning",
# {
#     output =
#     expect_that(CorrespondenceAnalysis(x.with.labels, row.names.to.remove = "NET",column.names.to.remove = "NET",
#                                        output = "Moonplot"),
#                 gives_warning())
# })
#
# test_that("Normalization warning",
# {
#     expect_that(CorrespondenceAnalysis(x.with.labels, normalization = "Row principal", row.names.to.remove = "NET",column.names.to.remove = "NET", output = "Moonplot"),
#                 not(gives_warning("'Normalization'")))
# })
#
