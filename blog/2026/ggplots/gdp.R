# https://r-charts.com/evolution/newggslopegraph/

library(CGPfunctions)
library(readxl)
gdp <- read_excel("Desktop/Omega/02_UCO/04_Curso21-22/R/dataviz/gdp.xlsx")

newggslopegraph(gdp, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "J. Caro",
                XTextSize = 18,    # Size of the times
                YTextSize = 5,     # Size of the groups
                TitleTextSize = 14,
                SubTitleTextSize = 12,
                CaptionTextSize = 10,
                TitleJustify = "left",
                SubTitleJustify = "left",
                CaptionJustify = "right",
                DataTextSize = 2.5) # Size of the data)
