
dictSegments = fread("dictSegments.csv")

dictOrganic = c("Non Organic", "Organic") # IMF only
dictCSS = c("Other Core", "Sleep", "Hungry") # Base & Base Plus
dictProtein = c("Cow", "Soy", "Goat", "Other Protein Origin")

dictPS = dictSegments[, unique(PS)]
dictPS0 = dictSegments[, unique(PS0)]
dictPS2 = dictSegments[, unique(PS2)]
dictPS3 = dictSegments[, unique(PS3)]
dictPSV = dictSegments[, unique(PSV)]

dictStorage = c("Chilled", "Non Chilled")
dictFlavoured = c("Flavoured", "Unflavoured") # AMN, IMF
dictForm = c("Liquid", "Not Applicable", "Powder", "Pure", "Solid") # Not applicable for PL only
dictPromoPack = c("Regular", "Multi", "Bundle", "Promo")
dictPackage = c("Can",
                "Carton",
                "Foiled Can",
                "Glass",
                "Plastic Bottle",
                "Plastic Box",
                "Plastic Cup",
                "Pouch",
                "Soft",
                "Tetra Pak")

dict.months = data.table(
  Mnb = 1:12,
  month.name = c(
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )
)

dict.channel = data.table(
  channel.nutricia = c(
    "Big MT",
    "Pharma channel",
    "Others",
    "Baby stores channel",
    "E-Stores Channel",
    "Health & Beauty",
    "Modern trade channel",
    "Others",
    "Pharma channel"
  ),
  Channel = c(
    "MT",
    "PHARMA",
    "OTHERS",
    "OTHERS",
    "OTHERS",
    "OTHERS",
    "MT",
    "OTHERS",
    "PHARMA"
  )
)

dictWeeks = data.table(Wnb = 1:53,
                       Mnb = c(rep(1, 4),
                               rep(2, 4),
                               rep(3, 5),
                               rep(4, 4),
                               rep(5, 4),
                               rep(6, 5),
                               rep(7, 4),
                               rep(8, 4),
                               rep(9, 5),
                               rep(10, 4),
                               rep(11, 4),
                               rep(12, 6)
                       ))
dictCalendar = fread("dictCalendar.csv")

df.sku.proxima = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima.csv")
df.sku.nielsen = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen.csv")
df.matrix = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.matrix.csv")
dictRegions = fread("dictRegions.csv")
# dictEC = fread("/home/serhii/Documents/Work/Nutricia/Scripts/Pivot4/dictEC.csv")
# dictAC = fread("/home/serhii/Documents/Work/Nutricia/Scripts/Pivot4/dictAC.csv")
dict.price.segments = fread("PriceSegments.csv")
dictScent = fread("dictScents.csv")
# dictOPMP = fread("/home/serhii/Documents/Work/Nutricia/Data/df.opmp.csv")
dictWeeks = fread("dictWeeks.csv")
dictCalendar = fread("dictCalendar.csv")
dictOPMP.pharma = fread("/home/serhii/Documents/Work/Nutricia/Data/df.opmp.pharma.csv")
dictOPMP.mt = fread("/home/serhii/Documents/Work/Nutricia/Data/df.opmp.mt.csv")
dictOPMP.amn = fread("/home/serhii/Documents/Work/Nutricia/Data/df.opmp.amn.csv")
dictOPMP.ec = fread("/home/serhii/Documents/Work/Nutricia/Data/df.opmp.ec.csv")