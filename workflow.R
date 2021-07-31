library(data.table)
library(stringi)

source("functions.R")
source("dictionaries.R")

df.matrix = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.matrix.csv")

### Step X. Process secondary sales
df.ss = fread("/home/serhii/Documents/Work/Nutricia/Data/202105/df.ss.csv", check.name = TRUE)
df.ss.main = fread("/home/serhii/Documents/Work/Nutricia/Data/202106/secondary.sales.main.csv", check.name = TRUE)
df.ss.ps = fread("/home/serhii/Documents/Work/Nutricia/Data/202106/secondary.sales.ps.csv", check.name = TRUE)

df.ss = prepare.secondary.sales(df.ss.main, df.ss.ps, df.ss)

fwrite(df.ss, "/home/serhii/Documents/Work/Nutricia/Data/202106/df.ss.csv", 
       row.names = FALSE)



### Step 1. Proxima - Baby Food

# Step 1.1. Read raw data
df = fread("/home/serhii/Documents/Work/Nutricia/Data/202106/P_2021_6.csv",
           check.names = TRUE)
df.sku.proxima = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima.csv")
df.p  = fread("/home/serhii/Documents/Work/Nutricia/Data/BF_PH_2021M5.csv")

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

fwrite(df.p, "/home/serhii/Documents/Work/Nutricia/Data/BF_PH_2021M6.csv",
       row.names = FALSE)


### Step 2. Proxima - AMN

# Step 2.1. Read raw data
df = fread("/home/serhii/Documents/Work/Nutricia/Data/202106/AMN_2021_6.csv",
           check.names = TRUE)
df.sku.proxima = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.proxima.csv")
df.amn  = fread("/home/serhii/Documents/Work/Nutricia/Data/AMN_PH_2015-2021M5.csv")

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
df.amn = append.new.data(df, df.amn, 0.50)
check.historical.data(df.amn)

fwrite(df.amn, "/home/serhii/Documents/Work/Nutricia/Data/AMN_PH_2015-2021M6.csv",
       row.names = FALSE)

### Step 3 Nielsen

# Step 3.1. Read raw data
df = fread("/home/serhii/Documents/Work/Nutricia/Data/202106/N_2021_6.csv",
           check.names = TRUE)
df.sku.nielsen = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen.csv")
df.n = fread("/home/serhii/Documents/Work/Nutricia/Data/N_MT_2015-2021M5.csv")

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

fwrite(df.n, "/home/serhii/Documents/Work/Nutricia/Data/N_MT_2015-2021M6.csv",
       row.names = FALSE)

### Step 4 Nielsen eCom

# Step 4.1. Read raw data
df = fread("/home/serhii/Documents/Work/Nutricia/Data/202106/N_EC_2021_6.csv",
           check.names = TRUE)
df.sku.nielsen = fread("/home/serhii/Documents/Work/Nutricia/Data/Dictionaries/df.sku.nielsen.csv")
df.ec = fread("/home/serhii/Documents/Work/Nutricia/Data/N_EC_2018-2021M5.csv")

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
# names(df)[1] = "Region.nielsen"

df = raw.data.processing(df)
all(names(df) == names(df.ec))
df.ec = rbindlist(list(df.ec, df))
tail(df.ec[, .(.N, Volume = sum(Volume), Value = sum(Value)), 
           by = .(Ynb, Wnb)], 20)

# df = raw.data.processing(df)
# df.ec = append.new.data(df, df.ec, 0.25)
# check.historical.data(df.ec)

fwrite(df.ec, "/home/serhii/Documents/Work/Nutricia/Data/N_EC_2018-2021M6.csv",
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
