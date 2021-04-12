library(data.table)
library(stringi)

source("functions.R")
source("dictionaries.R")

df.matrix = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.matrix.csv")

### Step 1. Proxima - Baby Food

# Step 1.1. Read raw data
df = fread("/home/serhii/Documents/Work/Nutricia/Data/202102/P_2021_2.csv",
           check.names = TRUE)
df.sku.proxima = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima.csv")
df.p  = fread("/home/serhii/Documents/Work/Nutricia/Data/BF_PH_2021M1.csv")

# Save a copy of existing descriptions
fwrite(df.sku.proxima, 
       paste0("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima-", 
              Sys.Date(), ".csv"), 
       row.names = FALSE)

# Step 1.2. Define new descriptions
df.new = export.new.skus(df, df.sku.proxima)

# Step 1.3. Append new descriptions to existing ones

# Just in case the data contain Size.pharma that is not used
# It is excluded by the following line
cols = names(df.new)[names(df.new) %in% names(df.sku.proxima)]

df.sku.proxima = rbindlist(list(df.sku.proxima, df.new[, mget(cols)]), 
                           fill = TRUE)
similar.description(df.sku.proxima)

# Step 1.4. Save updated file of descriptions
fwrite(df.sku.proxima,
       "/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima.csv",
       row.names = FALSE)

# Step 1.5. Update sales file
df = raw.data.processing(df)
df.p = append.new.data(df, df.p, 0.25)
check.historical.data(df.p)

fwrite(df.p, "/home/serhii/Documents/Work/Nutricia/Data/BF_PH_2021M2.csv",
       row.names = FALSE)


### Step 2. Proxima - AMN

# Step 2.1. Read raw data
df = fread("/home/serhii/Documents/Work/Nutricia/Data/202102/AMN_2021_2.csv",
           check.names = TRUE)
df.sku.proxima = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima.csv")
df.amn  = fread("/home/serhii/Documents/Work/Nutricia/Data/AMN_PH_2015-2021M1.csv")

# Save a copy of existing descriptions
fwrite(df.sku.proxima, 
       paste0("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima-", 
              Sys.Date(), ".csv"), 
       row.names = FALSE)

# Step 2.2. Define new descriptions
df.new = export.new.skus(df, df.sku.proxima)
dim(df.new)

# Step 2.3. Append new descriptions to existing ones

# Just in case the data contain Size.pharma that is not used
# It is excluded by the following line
cols = names(df.new)[names(df.new) %in% names(df.sku.proxima)]

df.sku.proxima = rbindlist(list(df.sku.proxima, df.new[, mget(cols)]), 
                           fill = TRUE)
similar.description(df.sku.proxima)

# Step 2.4. Save updated file of descriptions
fwrite(df.sku.proxima,
       "/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima.csv",
       row.names = FALSE)

# Step 2.5. Update sales file
df = raw.data.processing(df)
df.amn = append.new.data(df, df.amn, 0.90)
check.historical.data(df.amn)

fwrite(df.amn, "/home/serhii/Documents/Work/Nutricia/Data/AMN_PH_2015-2021M2.csv",
       row.names = FALSE)

### Step 3 Nielsen

# Step 3.1. Read raw data
df = fread("/home/serhii/Documents/Work/Nutricia/Data/202102/N_2021_2.csv",
           check.names = TRUE)
df.sku.nielsen = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen.csv")
df.n = fread("/home/serhii/Documents/Work/Nutricia/Data/N_MT_2015-2020M12.csv")

# Save a copy of existing descriptions
fwrite(df.sku.nielsen, 
       paste0("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen-", 
              Sys.Date(), ".csv"), 
       row.names = FALSE)

# Step 3.2. Define new descriptions
df.new = export.new.skus(df, df.sku.nielsen)

# Step 3.3. Append new descriptions to existing ones

# Just in case the data contain Size.pharma that is not used
# It is excluded by the following line
cols = names(df.new)[names(df.new) %in% names(df.sku.nielsen)]

df.sku.nielsen = rbindlist(list(df.sku.nielsen, df.new[, mget(cols)]), 
                           fill = TRUE)

# Step 3.4. Save updated file of descriptions
fwrite(df.sku.nielsen,
       "/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen.csv",
       row.names = FALSE)

# Step 3.5. Update sales file
df = raw.data.processing(df)
df.n = append.new.data(df, df.n, 0.25)
check.historical.data(df.n)

fwrite(df.n, "/home/serhii/Documents/Work/Nutricia/Data/N_MT_2015-2021M2.csv",
       row.names = FALSE)

### Step 4 Nielsen eCom

# Step 4.1. Read raw data
df = fread("/home/serhii/Documents/Work/Nutricia/Data/eCommerce/N_EC_2018-2021.csv",
           check.names = TRUE)
df.sku.nielsen = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen.csv")
# df.n.ec  = fread("/home/serhii/Documents/Work/Nutricia/Data/N_MT_2015-2020M04.csv")

# Save a copy of existing descriptions
fwrite(df.sku.nielsen, 
       paste0("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen-", 
              Sys.Date(), ".csv"), 
       row.names = FALSE)

# Step 4.2. Define new descriptions
df.new = export.new.skus(df, df.sku.nielsen)

# Step 4.3. Append new descriptions to existing ones

# Just in case the data contain Size.pharma that is not used
# It is excluded by the following line
cols = names(df.new)[names(df.new) %in% names(df.sku.nielsen)]

df.sku.nielsen = rbindlist(list(df.sku.nielsen, df.new[, mget(cols)]), 
                           fill = TRUE)

# Step 4.4. Save updated file of descriptions
fwrite(df.sku.nielsen,
       "/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen.csv",
       row.names = FALSE)

# Step 4.5. Update sales file
names(df)[1] = "Region.nielsen"

df = raw.data.processing(df)
df.ec = append.new.data(df, df.ec, 0.20)
check.historical.data(df.n)

fwrite(df, "/home/serhii/Documents/Work/Nutricia/Data/N_EC_2018-2020M12.csv",
       row.names = FALSE)

### Step 5. Check matrix of attributes
df.matrix = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.matrix.csv")

fwrite(df.matrix, 
       paste0("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.matrix ", 
              Sys.Date(), ".csv"), 
       row.names = FALSE)

check.sku.matrix(df.matrix)
check.attributes(df.matrix)

fwrite(df.matrix, "/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.matrix.csv",
       row.names = FALSE)

### Check OP & MP

# Level 1
df[, Price := Value/Volume]
df[, rse(Price), by = .(Region.nielsen, SKU2)][V1 > 0.1][order(-V1)]
suspicious = df[, rse(Price), by = .(Region.nielsen, SKU2)][V1 > 0.1, SKU2]
a=df[SKU2 %in% suspicious]

write.csv(a, "/home/serhii/Documents/Work/Nutricia/Data/eCommerce/c2.csv", row.names = F)

# Level 2
# Apply corrections and add coding
df.ec = fread("/home/serhii/Documents/Work/Nutricia/Data/eCommerce/df.ec_2018-2021.csv")
dictOPMP = fread("/home/serhii/Documents/Work/Nutricia/Data/df.opmp.csv")

df.ec[dictOPMP, 
      on = c("SKU2", "Ynb", "Wnb", "Region.nielsen"), 
      OPMP := i.opmp]
df.ec[df.sku.nielsen, on = "SKU2", ID := i.ID]

df.ec[is.na(ID) | ID == "", .N]
df.ec = df.ec[ID > 0]

df.ec[is.na(OPMP), OPMP := 1]
df.ec[, Volume := Volume * OPMP]

df.ec = df.ec[df.matrix, on = "ID",
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

df.ec[Items.in.pack == 1,
   SKU := paste(Brand, SubBrand, Size, Age, Scent, PS, Form, Package, PromoPack)]
df.ec[Items.in.pack != 1,
   SKU := paste0(Brand, " ", SubBrand, " ", Items.in.pack, "*", Size, 
                 " ", Age, " ", Scent, " ", PS, " ", Form, " ", Package, 
                 " ", PromoPack)]

df.ec = df.ec[, .(Volume = sum(Volume),
                  Value = sum(Value)),
              by = .(SKU, Ynb, Wnb, Region.nielsen)]

df.ec[, Price := Value/Volume]
df.ec[, rse(Price), by = .(Region.nielsen, SKU)][V1 > 0.1][order(-V1)]
suspicious = df.ec[, rse(Price), by = .(Region.nielsen, SKU)][V1 > 0.1, SKU]
a=df.ec[SKU %in% suspicious]

write.csv(a, "/home/serhii/Documents/Work/Nutricia/Data/eCommerce/c4.csv", row.names = F)


### Step 6. Generate pivot
