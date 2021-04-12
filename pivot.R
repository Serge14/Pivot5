library(data.table)
library(stringi)
# library(disk.frame)

source("functions.R")

# df.p = csv_to_disk.frame("/home/serhii/Documents/Work/Nutricia/Data/BF_PH_2015-2021M1.csv",
#                          outdir = "tmpdf", overwrite = TRUE,
#                          in_chunk_size = 100000)

df.p  = fread("/home/serhii/Documents/Work/Nutricia/Data/BF_PH_2015-2020M12.csv")
df.p[, SKU := NULL]
df.temp = fread("/home/serhii/Documents/Work/Nutricia/Data/BF_PH_2021M2.csv")
df.temp[, SKU := NULL]
df.p = rbindlist(list(df.p, df.temp))
rm(df.temp)


df.ec = fread("/home/serhii/Documents/Work/Nutricia/Data/N_EC_2018-2020M12.csv")
df.ec.op = fread("/home/serhii/Documents/Work/Nutricia/Data/df.ec.op.csv")
df.n = fread("/home/serhii/Documents/Work/Nutricia/Data/N_MT_2015-2021M1.csv")
df.amn = fread("/home/serhii/Documents/Work/Nutricia/Data/AMN_PH_2015-2021M1.csv")
df.sku.proxima = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima.csv")
df.sku.nielsen = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen.csv")
df.matrix = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.matrix.csv")
dictRegions = fread("/home/serhii/Documents/Work/Nutricia/Scripts/Pivot4/dictRegions.csv")
dictEC = fread("/home/serhii/Documents/Work/Nutricia/Scripts/Pivot4/dictEC.csv")
dictAC = fread("/home/serhii/Documents/Work/Nutricia/Scripts/Pivot4/dictAC.csv")
dict.price.segments = fread("/home/serhii/Documents/Work/Nutricia/Scripts/Pivot4/PriceSegments.csv")
dictScent = fread("/home/serhii/Documents/Work/Nutricia/Scripts/Pivot4/dictScents.csv")


### Check periods, select the same range of data

min.year = max(df.n[, min(Ynb)],
               df.p[, min(Ynb)],
               df.amn[, min(Ynb)])

# Select same range
df.n = df.n[Ynb >= min.year]
df.p = df.p[Ynb >= min.year]
df.amn = df.amn[Ynb >= min.year]

# Check if all files have all periods
periods.equal = check.periods.equal(df.n, df.p, df.amn)

if (periods.equal != TRUE) {
   print("Datasets contain different number of periods, check datasets")}
   


# Matrix
df.matrix[, SKU2 := SKU]
# df.matrix[SKU2 == "OTHER ITEMS PRIVATE LABEL", SKU2 := paste(SKU2, DANONE.SUB.SEGMENT)]

# Nielsen
# df.n[SKU2 == "OTHER ITEMS PRIVATE LABEL", SKU2 := paste(SKU2, DANONE.SUB.SEGMENT)]
# df.n = df.n[, .(SKU2, Region, Ynb, Mnb, Volume, Value)]

# Convert regions

df.n[df.sku.nielsen, on = "SKU2", ID := i.ID]

df.n[is.na(ID) | ID == "", .N]
df.n = df.n[ID > 0]

df.n = df.n[df.matrix, on = "ID",
            `:=`(Brand = i.Brand,
                 SubBrand = i.SubBrand,
                 Organic = i.Organic,
                 CSS = i.CSS,
                 Items.in.pack = i.Items.in.pack,
                 Size = i.Size,
                 Age = i.Age,
                 Scent = i.Scent,
                 Protein = i.Protein,
                 Flavoured = i.Flavoured,
                 Company = i.Company,
                 PS0 = i.PS0,
                 PS2 = i.PS2,
                 PS3 = i.PS3,
                 PS = i.PS,
                 PSV = i.PSV,
                 Form = i.Form,
                 Package = i.Package,
                 Storage = i.Storage,
                 PromoPack  = i.PromoPack)]

df.n[, c("SKU2", "SKU") := NULL]
# names(df.n)[1] = "Region.MT"

# Pharma
df.amn = df.amn[Category.pharma != "ДП", 
                .(ID.morion, SKU2, Region.pharma, Ynb, Mnb, Items, Value)]
df.p = df.p[, .(ID.morion, SKU2, Region.pharma, Ynb, Mnb, Items, Value)]

all(names(df.amn) == names(df.p))

df.p = rbindlist(list(df.p, df.amn), use.names = TRUE)

df.p[df.sku.proxima, on = c("ID.morion", "SKU2"), ID := i.ID]
df.p[is.na(ID) | ID == "", .N]
# df.p[ID == -1, .N]
# df.p[ID == -1, unique(SKU2)]

df.p = df.p[ID > 0]

df.p = df.p[df.matrix, on = "ID",
            `:=`(Brand = i.Brand,
                 SubBrand = i.SubBrand,
                 Organic = i.Organic,
                 CSS = i.CSS,
                 Items.in.pack = i.Items.in.pack,
                 Size = i.Size,
                 Age = i.Age,
                 Scent = i.Scent,
                 Protein = i.Protein,
                 Flavoured = i.Flavoured,
                 Company = i.Company,
                 PS0 = i.PS0,
                 PS2 = i.PS2,
                 PS3 = i.PS3,
                 PS = i.PS,
                 PSV = i.PSV,
                 Form = i.Form,
                 Package = i.Package,
                 Storage = i.Storage,
                 PromoPack  = i.PromoPack)]

df.p[, c("SKU2", "ID.morion") := NULL]

# df.p = df.p[!is.na(Brand)]
# df.n = df.n[!is.na(Brand)]

calculate.volume(df.p)

df.p[, Items := NULL]

# Convert regions

# ECOM
# names(df.n)[1] = "Channel"
# df.n[, Region := Channel]

df.n[dictRegions[Channel == "MT"], 
     on = c(Region.nielsen = "Region.channel"),
     Region := i.Region]

df.n = df.n[!is.na(Region)]


df.p[dictRegions[Channel == "PHARMA"], 
     on = c(Region.pharma = "Region.channel"),
     Region := i.Region]

df.p[is.na(Region), .N]

# Join tables
df.n[, "Region.nielsen" := NULL]
df.p[, "Region.pharma" := NULL]

df.n[, Channel := "MT"]
df.p[, Channel := "PHARMA"]

all(names(df.n) %in% names(df.p))

df = rbindlist(list(df.p, df.n), use.names = TRUE)

# ECOM
# df = df.n

# Volume correction
correct.volume(df)

# Add SKUs

df[Items.in.pack == 1,
   SKU := paste(Brand, SubBrand, Size, Age, Scent, PS, Form, Package, PromoPack)]
df[Items.in.pack != 1,
   SKU := paste0(Brand, " ", SubBrand, " ", Items.in.pack, "*", Size, 
                 " ", Age, " ", Scent, " ", PS, " ", Form, " ", Package, 
                 " ", PromoPack)]

df[Brand == "Private Label", SKU := "Private Label SKUs"]

# Add Scent2 & ScentType

df.matrix[, 
          Scent2 := ifelse(PS3 == "Instant Cereals", 
                           mapply(unlist(define.scent.type), Scent)[1],
                           ""), by = 1:nrow(df.matrix)]

df.matrix[, 
          ScentType := ifelse(PS3 == "Instant Cereals", 
                              mapply(unlist(define.scent.type), Scent)[2],
                              ""), by = 1:nrow(df.matrix)]

df[df.matrix, on = "ID", `:=`(Scent2 = i.Scent2,
                              ScentType = i.ScentType)]


df = df[, .(Volume = sum(Volume), 
            Value = sum(Value)), 
        by = .(SKU, Brand, SubBrand, Organic, CSS, Items.in.pack, Size,
            Age, Scent, Scent2, ScentType, Protein, Flavoured, Company,
            PS0, PS2, PS3, PS, PSV,
            Form, Package, Storage, PromoPack, 
            Region, Channel, Ynb, Mnb)]

extrapolate(df)

add.price.segments(df)

price.check = TRUE

# check if any price segments are missing
if (df[(is.na(PriceSegment) | PriceSegment == "" |
        is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
      (PS0 == "IMF" & Form != "Liquid"), .N] > 0) {
   
  
   price.check = FALSE
   
   print("No price segments for IMF brands. Add price segments to the dictionary!")
   
   print(
   df[(is.na(PriceSegment) | PriceSegment == "" |
          is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
         (PS0 == "IMF" & Form != "Liquid"), 
      .(Brand = sort(unique(Brand)))]
   )
   
   df[PS0 == "IMF" & Form != "Liquid" & Ynb == max(Ynb),
      .(Price = sum(Value)/sum(Volume),
        Volume = sum(VolumeC),
        Value = sum(ValueC)), 
      by = .(Brand, PriceSegment, GlobalPriceSegment)][order(-Price)]
   
}

if (df[(is.na(PriceSegment) | PriceSegment == "" |
        is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
       (PS0 == "AMN"), .N] > 0) {
   
   price.check = FALSE
   
   print("No price segments for AMN brands. Add price segments to the dictionary!")
   
   print(
      df[(is.na(PriceSegment) | PriceSegment == "" |
             is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
            (PS0 == "AMN"), 
         .(Brand = sort(unique(Brand)))]
   )
   
   df[PS0 == "AMN" & Ynb == max(Ynb),
      .(Price = sum(Value)/sum(Volume),
        Volume = sum(VolumeC),
        Value = sum(ValueC)), 
      by = .(Brand, PriceSegment, GlobalPriceSegment)][order(-Price)]
   
} 

if (df[(is.na(PriceSegment) | PriceSegment == "" |
        is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
      (PS2 == "Dry Food"), .N] > 0) {
   
   price.check = FALSE
   
   print("No price segments for DF brands. Add price segments to the dictionary!")
   
   print(
   df[(is.na(PriceSegment) | PriceSegment == "" |
          is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
         (PS2 == "Dry Food"), 
      .(Brand = sort(unique(Brand)))]
   )
   
   df[PS2 == "Dry Food" & Ynb == max(Ynb),
      .(Price = sum(Value)/sum(Volume),
        Volume = sum(VolumeC),
        Value = sum(ValueC)), 
      by = .(Brand, PriceSegment, GlobalPriceSegment)][order(-Price)]
   
} 


if (df[(is.na(PriceSegment) | PriceSegment == "" |
        is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
      (PS3 == "Savoury Meal" | PS3 == "Fruits"), .N] > 0){
   
   price.check = FALSE
   
   print("No price segments for Puree brands. Add price segments to the dictionary!")
   
   print(
   df[(is.na(PriceSegment) | PriceSegment == "" |
          is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
         (PS3 == "Savoury Meal" | PS3 == "Fruits"),
      .(Brand = sort(unique(Brand)))]
   )
   
   df[PS3 == "Savoury Meal" | PS3 == "Fruits" & Ynb == max(Ynb),
      .(Price = sum(Value)/sum(Volume),
        Volume = sum(VolumeC),
        Value = sum(ValueC)), 
      by = .(Brand, PriceSegment, GlobalPriceSegment)][order(-Price)]
   
   
}

if (price.check == FALSE){
   
   print("Add price segments!")
}

add.acidified(df)

df = df[, .(SKU, Brand, SubBrand, Organic, CSS, Items.in.pack, Size,
            Age, Scent, Scent2, ScentType, Acidified, Protein, Flavoured, Company,
            PS0, PS2, PS3, PS, PSV,
            Form, Package, Storage, PromoPack, PriceSegment, GlobalPriceSegment,
            Region, Channel, Ynb, Mnb, 
            Volume, Value, EC, AC, VolumeC, ValueC)]

fwrite(df,
       "/home/serhii/Documents/Work/Nutricia/Data/202101/df.csv",
       row.names = FALSE)

fwrite(df[, .(SKU, Brand, SubBrand, Size,
              Age, Scent, Scent2, ScentType, Acidified, Company,
              PS0, PS2, PS3, PS, 
              Form, Package, PromoPack, PriceSegment,
              Region, Channel, Ynb, Mnb, 
              Volume, Value, EC, AC, VolumeC, ValueC)],
       "/home/serhii/Documents/Work/Nutricia/Data/202101/df.pivot.csv",
       row.names = FALSE)


fwrite(df[Ynb >= 2020], 
       "/home/serhii/Documents/Work/Nutricia/Data/202101/df.short.csv", 
       row.names = FALSE)

fwrite(df[, .(SKU, Brand, SubBrand, Size,
              Age, Scent, Scent2, ScentType, Acidified, Company,
              PS0, PS2, PS3, PS,
              Form, Package, PromoPack, PriceSegment,
              Region, Channel, Ynb, Mnb,
              Volume, Value)],
       "/home/serhii/Documents/Work/Nutricia/Data/eCommerce/df.ecom.csv",
       row.names = FALSE)


# Price check
# df[, Price := Value/Volume]
# sd/(mean*sqrt(n))


# EC
df.ec[df.ec.op, 
      on = c("SKU2", "Region.nielsen", "Ynb", "Mnb"), 
      OP := i.OP]
df.ec[is.na(OP), OP := 1]
df.ec[, Volume := Volume/OP]

df = fread("/home/serhii/Documents/Temp/a2_important.csv")
df[df.ec, on = c("SKU2", "Ynb", "Mnb", "Volume","Value"), 
   region.nielsen := i.Region.nielsen]
write.csv(df, "/home/serhii/Documents/Temp/a4.csv", row.names = F)
