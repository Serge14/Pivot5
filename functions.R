convert.regions = function(df, channel) {
  dict.regions.nielsen = c(
    "KYIV RA&OM",
    "CENTER RA&OM",
    "NORTH-EAST RA&OM",
    "SOUTH-EAST RA&OM",
    "EAST RA&OM",
    "SOUTH RA&OM",
    "WEST RA&OM"
  )
  
  if (all(dict.regions.nielsen %in% df[, unique(Region)]) &
      df[Region %in% dict.regions.nielsen, length(unique(Region))] == 7) {
    df = df[Region %in% dict.regions.nielsen]
    
    df[Region == "KYIV RA&OM" |
         Region == "CENTER RA&OM", Region := "Center"]
    df[Region == "NORTH-EAST RA&OM" | Region == "SOUTH-EAST RA&OM" |
         Region == "EAST RA&OM", Region := "East"]
    df[Region == "SOUTH RA&OM", Region := "South"]
    df[Region == "WEST RA&OM", Region := "West"]
    
    return(df)
    
  } else {
    print("Check regions, something is wrong!")
    
  }
}


calculate.volume = function(df) {
  df[, `:=`(
    Size.Temp = as.numeric(stri_extract_first_regex(Size, "[0-9]{0,}.?[0-9]+")),
    UOM.Temp = stri_extract_first_regex(Size, "[a-zA-Z]+")
  )]
  
  dictUOM = c("Gr", "Ml", "Lt", "Kg")
  
  if (df[!(UOM.Temp %in% dictUOM), .N] > 0) {
    print("Check the unit of measure")
    print(df[!(UOM.Temp %in% dictUOM), UOM.Temp])
  }
  
  df[UOM.Temp %in% c("Gr", "Ml"), VolumeFactor := Size.Temp / 1000]
  df[UOM.Temp %in% c("Kg", "Lt"), VolumeFactor := Size.Temp]
  
  # check size
  
  # df[, VolumeCorrection := 1]
  # df[PS0 == "IMF" & Form == "Liquid", VolumeCorrection := 7]
  # # df[PS0 == "AMN" & Form == "Liquid", VolumeCorrection := 4.5]
  # df[PS == "Enhanced Recovery" & Form == "Liquid", VolumeCorrection := 4.801183]
  # df[PS == "Recovery" & Form == "Liquid", VolumeCorrection := 2.892422]
  # df[PS == "Metabolics" & Form == "Liquid", VolumeCorrection := 5.430002]
  # df[PS == "Faltering Growth" & Form == "Liquid", VolumeCorrection := 3.416514]
  # 
  # # df[PS == "Condensed Milk" & Form == "Liquid", VolumeCorrection := 3.5]
  # df[PS == "Liquid Cereals" &
  #      Form == "Liquid", VolumeCorrection := 3.5]
  
  # df[, Volume := Items * VolumeFactor * Items.in.pack / VolumeCorrection]
  df[, Volume := Items * VolumeFactor * Items.in.pack]
  
  df[, c("Size.Temp", "UOM.Temp", "VolumeFactor") := NULL]
  # df[, c("Size.Temp", "UOM.Temp", "VolumeFactor", "VolumeCorrection") := NULL]
  
}

correct.volume = function(df){
  
  df[, VolumeCorrection := 1]
  df[PS0 == "IMF" & Form == "Liquid", VolumeCorrection := 7]
  # df[PS0 == "AMN" & Form == "Liquid", VolumeCorrection := 4.5]
  df[PS == "Enhanced Recovery" & Form == "Liquid", VolumeCorrection := 4.801183]
  df[PS == "Recovery" & Form == "Liquid", VolumeCorrection := 2.892422]
  df[PS == "Metabolics" & Form == "Liquid", VolumeCorrection := 5.430002]
  df[PS == "Faltering Growth" & Form == "Liquid", VolumeCorrection := 3.416514]
  
  # df[PS == "Condensed Milk" & Form == "Liquid", VolumeCorrection := 3.5]
  df[PS == "Liquid Cereals" &
       Form == "Liquid", VolumeCorrection := 3.5]
  
  df[, Volume := Volume / VolumeCorrection]
  
  df[, VolumeCorrection := NULL]
  
}

check.sku.matrix = function(df) {
  cols = c(
    "Brand",
    "SubBrand",
    "Organic",
    "CSS",
    "Items.in.pack",
    "Size",
    "Age",
    "Scent",
    "Protein",
    "Flavoured",
    "Company",
    "PS0",
    "PS2",
    "PS3",
    "PS",
    "PSV",
    "Form",
    "Package",
    "Storage",
    "PromoPack"
  )
  
  print(df[, .N, by = cols][N > 1])
  
  print(df[, .N, by = ID][N > 1])
  
}

extrapolate = function(df) {
  df[dictEC, on = c(PS3 = "Segment", Channel = "Channel"), EC := i.EC]
  df[PS == "Hypoallergenic" | PS == "Digestive Comfort",
     EC := dictEC[.SD, .(EC), on = c(Segment = "PS", Channel = "Channel")]]
  
  df[, AC := 1]
  
  df[(PS3 == "Base" | PS3 == "Base Plus") & Channel == "PHARMA",
     AC := dictAC[.SD, .(BP.pharma), on = c("Ynb", "Mnb")]]
  
  df[PS3 == "Specials" & Channel == "MT",
     AC := dictAC[.SD, .(Specials.MT), on = c("Ynb", "Mnb")]]
  
  df[PS3 == "Specials" & Channel == "PHARMA",
     AC := dictAC[.SD, .(Specials.pharma), on = c("Ynb", "Mnb")]]
  
  df[PS == "Digestive Comfort" & Channel == "PHARMA",
     AC := dictAC[.SD, .(DC.pharma), on = c("Ynb", "Mnb")]]
  
  df[PS == "Digestive Comfort" & Channel == "MT",
     AC := dictAC[.SD, .(DC.MT), on = c("Ynb", "Mnb")]]
  
  df[PS == "Hypoallergenic" & Channel == "PHARMA",
     AC := dictAC[.SD, .(HA.pharma), on = c("Ynb", "Mnb")]]
  
  df[PS == "Hypoallergenic" & Channel == "MT",
     AC := dictAC[.SD, .(HA.MT), on = c("Ynb", "Mnb")]]
  
  # Extra corrections
  df[Ynb == 2020 & Mnb == 12 & PS3 == "Base",
     AC := AC*1.05]
  
  df[Ynb == 2020 & Mnb == 12 & PS3 == "Cereal Biscuits" & 
       Company == "Nutricia",
     AC := AC*1.05]
  
  df[Ynb == 2020 & Mnb == 12 & 
       (PS3 == "Fruits" | PS3 == "Savoury Meal") & 
       Company == "Nutricia",
     AC := AC*1.05]
 
  # df[Ynb == 2021 & Mnb == 1 & PS3 == "Cereal Biscuits" & 
  #      Company == "Nutricia",
  #    AC := AC*1.15]
  
  df[, `:=`(VolumeC = Volume * EC * AC,
            ValueC = Value * EC * AC)]
  
}

add.price.segments = function(df) {
  # Local price segments
  df[PS0 == "IMF",
     PriceSegment := dict.price.segments[Segment == "IMF"][.SD, .(PriceSegment), on = "Brand"]]
  
  df[PS2 == "Dry Food",
     PriceSegment := dict.price.segments[Segment == "Dry Food"][.SD, .(PriceSegment), on = "Brand"]]
  
  df[PS3 == "Fruits" | PS3 == "Savoury Meal",
     PriceSegment := dict.price.segments[Segment == "Puree"][.SD, .(PriceSegment), on = "Brand"]]
  
  df[PS0 == "AMN",
     PriceSegment := dict.price.segments[Segment == "AMN"][.SD, .(PriceSegment), on = "Brand"]]
  
  # Global Price segments
  df[PS0 == "IMF",
     GlobalPriceSegment := dict.price.segments[Segment == "IMF"][.SD, .(GlobalPriceSegment), on = "Brand"]]
  
  df[PS2 == "Dry Food",
     GlobalPriceSegment := dict.price.segments[Segment == "Dry Food"][.SD, .(GlobalPriceSegment), on = "Brand"]]
  
  df[PS3 == "Fruits" | PS3 == "Savoury Meal",
     GlobalPriceSegment := dict.price.segments[Segment == "Puree"][.SD, .(GlobalPriceSegment), on = "Brand"]]
  
  df[PS0 == "AMN",
     GlobalPriceSegment := dict.price.segments[Segment == "AMN"][.SD, .(GlobalPriceSegment), on = "Brand"]]
  
}

add.acidified = function(df) {
  df[PS == "Digestive Comfort", Acidified := "Non-Acidified"]
  df[PS == "Digestive Comfort" &
       (
         stri_detect_fixed(tolower(SKU), "bifid", ignore.case = TRUE) |
           stri_detect_fixed(tolower(SKU), "kisl", ignore.case = TRUE) |
           stri_detect_regex(SubBrand, "^Km")
       ),
     Acidified := "Acidified"]
  
  # return(df)
  
}

define.scent.type = function(a) {
  v = c(0, 0)
  
  a = stri_trim(stri_replace_all_regex(a, "[0-9]+", ""))
  b = stri_trim(unlist(stri_split_fixed(a, "-")))
  
  
  if (all(b %in% dictScent$V2)) {
    dictScent[V2 %in% b, .N, by = V3]
    if (dictScent[V3 == "Fruit"][V2 %in% b, .N] > 0) {
      v[1] = dictScent[V2 %in% b, .N, by = V3][V3 == "Fruit", N]
    }
    
    if (dictScent[V3 == "Grain"][V2 %in% b, .N] > 0) {
      v[2] = dictScent[V2 %in% b, .N, by = V3][V3 == "Grain", N]
    }
    
    c1 = dictScent[V3 == "Fruit"][V2 %in% b, V2]
    c2 = dictScent[V3 == "Grain"][V2 %in% b, V2]
    
    if (v[1] >= 1 &
        v[1] <= 2) {
      Scent2 = stri_c(sort(c1), collapse = "-")
    }
    
    if (v[1] == 0) {
      Scent2 = "Without"
    }
    if (v[1] > 2) {
      Scent2 = "Multifruit"
    }
    
    if (v[2] >= 1 & v[2] <= 2) {
      Type = stri_c(sort(c2), collapse = "-")
    }
    
    if (v[2] == 0) {
      Type = "Without"
    }
    if (v[2] > 2) {
      Type = "Multigrain"
    }
    
    if ("Cereals" %in% c2) {
      Type = "Multigrain"
    }
    if (a == "" | is.na(a)) {
      Scent2 = "Unknown"
      Type = "Unknown"
    }
    
    # print(Scent2)
    return(list(Scent2, Type))
    # return(Scent2)
  } else {
    print(paste("Unknown scents: ", b[!(b %in% dictScent$V2)]))
    # stop("Some scents are new")
  }
}

add.brand = function(df, df.existing){
  
  brand.column = names(df.existing)[stri_detect_fixed(tolower(names(df.existing)), "brand")]
  brand.column = brand.column[brand.column != "Brand"]
  
  dictBrands = df.existing[, .(Brand = unique(Brand)), by = brand.column]
  
  # clean brand
  
  dictBrands = dictBrands[!(Brand == "" | is.na(Brand))]
  brands.list = dictBrands[, .N, brand.column][N == 1, get(brand.column)]
  
  dictBrands = dictBrands[get(brand.column) %in% brands.list]
  
  df[dictBrands, on = brand.column, Brand := i.Brand]
  
}

add.brand2 = function(df, df.existing, df.matrix, acceptable.p) {
  
  if (between(acceptable.p, 0, 1)) {check = TRUE}
  
  if (check == TRUE) {
    df.existing[df.matrix, on = "ID", Brand := i.Brand]
    
    brand.column = names(df.existing)[stri_detect_fixed(tolower(names(df.existing)), "brand")]
    
    dictBrands = df.existing[, .N, by = brand.column]
    
    brand.column = brand.column[brand.column != "Brand"]
    
    dictBrands[, p := prop.table(N), by = brand.column][, N := NULL][, .SD[which.max(p)], by = brand.column]
    
    # clean brand
    dictBrands = dictBrands[p > acceptable.p &
                              !(is.na(Brand) | Brand == "") &
                              !(is.na(get(brand.column)) |
                                  get(brand.column) == "")]
    
    df[dictBrands, on = brand.column, Brand := i.Brand]
    
  } else {
    print("Wrong range of p. It must be between 0 and 1. Change the value and try again!")
    
  }
  
}



addSize = function(df) {
  # Size
  # df[, Size := stri_extract_first_regex(SKU2, "[0-9]+[GML]+")]

  
  df[, Size := stri_extract_first_regex(tolower(SKU2), "[0-9]*[.,]*[0-9]+\\s*(g|ml|мл|гр?|л(?!е))")]
  
  df[, Size := stri_replace_first_fixed(Size, "G", "Gr")]
  df[, Size := stri_replace_first_fixed(Size, "ML", "Ml")]
  df[, Size := stri_replace_first_fixed(Size, "гр", "Gr")]
  df[, Size := stri_replace_first_fixed(Size, "г", "Gr")]
  df[, Size := stri_replace_first_fixed(Size, "мл", "Ml")]
  df[, Size := stri_replace_first_regex(Size, "(?<!м)л", "Lt")]
  df[, Size := stri_replace_first_fixed(Size, ",", ".")]
  
  df[, Size := stri_trim_both(Size)]
  df[, Size := stri_replace_all_fixed(Size, " ", "")]
  
}

add.cleaned.sku = function(df, cols) {
  
  # doesn't work for cases when only SKU column is passed
  
  cols = c(cols[!("SKU" == cols)], "SKU2")
  text.to.remove = c("[®|™|\"\"]")
  
  df[, SKU2 := stri_replace_all_regex(SKU, text.to.remove, " ")]
  df[, (cols) := lapply(.SD, function(x)
    stri_replace_all_regex(x, "\\s+", " ")),
    .SDcols = cols]
  df[, (cols) := lapply(.SD, function(x)
    stri_trim_both(x)), .SDcols = cols]
  df[, (cols) := lapply(.SD, function(x)
    stri_replace_all_fixed(x, " ,", ",")),
    .SDcols = cols]
  df[, SKU2 := stri_replace_all_regex(SKU2, ",$", "")]
  df[, SKU := stri_replace_all_fixed(SKU, '\"\"', '"')]
  df[, SKU2 := stri_replace_all_fixed(SKU2, ",,", ",")]
  df[, SKU := stri_replace_all_fixed(SKU, ", ,", ",")]
  
  if (any(stri_detect_fixed(names(df), "DANONE.SUB.SEGMENT"))) {
    df[SKU2 == "OTHER ITEMS PRIVATE LABEL",
       SKU2 := toupper(paste(SKU2, DANONE.SUB.SEGMENT))]
  }
  
  if (any(stri_detect_fixed(names(df), "ID.morion"))) {
   
    df[, ID.morion := as.integer(ID.morion)]
    # df[, Ynb := as.integer(Ynb)]
    # df[, Mnb := as.integer(Mnb)]
  }
  
  return(df)
}

remove.zero.sales = function(df) {
  sales.columns = names(df)[stri_detect_regex(names(df), "Items|Value|Volume")]
  
  filter.formula = paste(sales.columns, collapse = "+")
  filter.formula = paste0("(", filter.formula, ") > 0")
  filter.formula = parse(text = filter.formula)
  
  df = df[eval(filter.formula)]
  return(df)
}

addPS023 = function(df) {
  # the function adds PS0, PS2, PS3
  
  ### PS0, PS3
  df[dictSegments[, .(PS = unique(PS)), by = .(PS3, PS0)],
     on = "PS",
     `:=`(PS0.Temp = i.PS0,
          PS3.Temp = i.PS3)]
  
  ### PS2
  #IF from 0 to 6, but actually from less than 6 months
  df[PS0 == "IMF" &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) < 6 &
       !stri_detect_fixed(Age, "Y"),
     PS2.Temp := "IF"]
  
  # FO (6-12) starts from 6 to less than 12
  df[PS0 == "IMF" &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) >= 6 &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) < 12 &
       !stri_detect_fixed(Age, "Y"),
     PS2.Temp := "FO"]
  
  # GUM 12+
  df[PS0 == "IMF" &
       as.numeric(stri_extract_first_regex(Age, "[0-9]{1,2}")) >= 12 &
       !stri_detect_fixed(Age, "Y"),
     PS2.Temp := "Gum"]
  
  df[PS0 == "IMF" & stri_detect_fixed(Age, "Y"),
     PS2.Temp := "Gum"]
  
  # Others are not stated
  df[PS0 == "IMF" & is.na(PS2.Temp), PS2.Temp := "N/S"]
  
  df[PS0 != "IMF",
     PS2.Temp := dictSegments[PS0 != "IMF",
                              .(PS = unique(PS)),
                              by = PS2][.SD, .(PS2), on = "PS"]]
  
  # All others
  df[is.na(PS2.Temp), PS2.Temp := ""]
  
  
  
}


check.attributes = function(df) {
  
  last.period = df[, last(Period)]
  df1 = df[Period != last.period]
  df = df[Period == last.period]
  
  attributes = c(
    "Brand",
    "SubBrand",
    "Items.in.pack",
    "Age",
    "Scent",
    "Company",
    "Organic",
    "CSS",
    "Protein",
    "Flavoured",
    "PS0",
    "PS2",
    "PS3",
    "PS",
    "PSV",
    "Form",
    "Package",
    "Storage",
    "PromoPack"
  )
  
  # Pre-processing of values
  # Form to title
  df[, (attributes) := lapply(.SD, function(x) {
    stri_replace_all_regex(x, "\\s+", " ")
  }),
  .SDcols = attributes]
  
  df[, (attributes[-c(3:4, 11:15)]) := lapply(.SD, function(x) {
    stri_trans_totitle(x)
  }),
  .SDcols = attributes[-c(3:4, 11:15)]]
  
  df[Company == "Adp", Company := "ADP"]
  
  # print("Grammar...")
  
  for (i in attributes) {
    # Size - separate function
    # Age - ?
    # Scent
    
    if (i %in% c("Brand", "SubBrand", "Items.in.pack", "Company", "Age")) {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% df1[, get(i)]])]
      
    } else if (i == "PS") {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPS])]
      
    } else if (i == "PS0") {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPS0])]
      
    } else if (i == "PS2") {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPS2])]
      
    } else if (i == "PS3") {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPS3])]
      
    } else if (i == "PSV") {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPSV])]
      
    } else if (i == "Organic") {
      # list1 = df[, unique(get(i))][!(df[PS0 == "IMF" |
      #                                     PS0 == "AMN", unique(get(i)) %in% dictOrganic])]
      
      list1 = df[PS0 %in% c("IMF", "AMN"), 
                 unique(get(i))[!unique(get(i)) %in% dictOrganic]]
      
    } else if (i == "CSS") {
      # list1 = df[, unique(get(i))][!(df[PS0 == "IMF" |
      #                                     PS0 == "AMN", unique(get(i)) %in% dictCSS])]
      
      list1 = df[PS0 %in% c("IMF", "AMN"), 
                 unique(get(i))[!unique(get(i)) %in% dictCSS]]
      
    } else if (i == "Protein") {
      # list1 = df[, unique(get(i))][!(df[PS0 == "IMF" |
      #                                     PS0 == "AMN", unique(get(i)) %in% dictProtein])]

      list1 = df[PS0 %in% c("IMF", "AMN"), 
                 unique(get(i))[!unique(get(i)) %in% dictProtein]]

      
    } else if (i == "Flavoured") {
      # list1 = df[, unique(get(i))][!(df[PS0 == "IMF" |
      #                                     PS0 == "AMN", unique(get(i)) %in% dictFlavoured])]
      
      list1 = df[PS0 %in% c("IMF", "AMN"), 
                 unique(get(i))[!unique(get(i)) %in% dictFlavoured]]
      
    } else if (i == "Form") {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictForm])]
      
    } else if (i == "Package") {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPackage])]
      
    } else if (i == "Storage") {
      # list1 = df[, unique(get(i))][!(df[PS0 == "IMF" |
      #                                     PS0 == "AMN", unique(get(i)) %in% dictStorage])]
      
      list1 = df[PS0 %in% c("IMF", "AMN"), 
                 unique(get(i))[!unique(get(i)) %in% dictStorage]]
      
    } else if (i == "PromoPack") {
      list1 = df[, unique(get(i))][!(df[, unique(get(i)) %in% dictPromoPack])]
      
    }
    
    if (length(list1) > 0) {
      cat("\n")
      print(paste0("Suspicious ", i, "s:"))
      cat("\n")
      print(list1)
      cat("\n")
      print("SKUs:")
      print(df[get(i) %in% list1, get(i), by = SKU])
      cat("\n")
      
    } else {
      print(paste0("Grammar of ", i, ": OK"))
      
    }
  }
  
  # Company-Brand
  # print("Pair Company-Brand")
  list1 = df[, unique(paste0(Brand, "-", Company))][!(df[, unique(paste0(Brand, "-", Company))] %in%
                                                        df1[, unique(paste0(Brand, "-", Company))])]
  
  if (length(list1) > 0) {
    cat("\n")
    print(paste0("Suspicious Brand-Company piar(s):"))
    cat("\n")
    print(list1)
    cat("\n")
    print("SKUs:")
    print(df[paste0(Brand, "-", Company) %in% list1,
             .(Brand, Company), by = SKU])
    cat("\n")
    
  } else {
    print(paste0("Brand-Company: OK"))
    
  }
  
  # Scent: grammar, sorting, extra spaces
  df[, Scent := stri_replace_all_regex(Scent, "\\s*-\\s*", "-")]
  
  list1 = df[, unique(unlist(stri_split_fixed(Scent, "-")))]
  dictScent = df1[, unique(unlist(stri_split_fixed(Scent, "-")))]
  
  list1 = list1[!(list1 %in% dictScent)]
  
  if (length(list1) > 0) {
    cat("\n")
    print(paste0("Suspicious Scent(s):"))
    cat("\n")
    print(list1)
    cat("\n")
    print("SKUs:")
    print(df[stri_detect_regex(Scent, paste(list1, collapse = "|")),
             .(Brand, Scent), by = SKU])
    cat("\n")
    
  } else {
    print(paste0("Scent: OK"))
    
  }
  
  df[, Scent := sapply(stri_split_fixed(Scent, "-"), function(x)
    paste(sort(x), collapse = "-"))]
  
  # Check correct segments
  
  # print("Checking correct segments PS0, PS2, PS3...")
  
  df = addPS023(df)
  
  # PS0
  if (df[(PS0 != PS0.Temp) | is.na(PS0.Temp) | PS0 == "", .N] > 0) {
    cat("\n")
    print("Suspicious PS0 or PS:")
    cat("\n")
    print(df[(PS0 != PS0.Temp) | is.na(PS0.Temp) | PS0 == "",
             SKU,
             by = .(PS0, PS0.Temp, PS)])
    cat("\n")
    
  } else {
    print("Logic PS0: OK")
  }
  
  # PS2
  if (df[(PS2 != PS2.Temp) | is.na(PS2.Temp) | PS2 == "", .N] > 0) {
    cat("\n")
    print("Suspicious PS2 or PS:")
    cat("\n")
    print(df[(PS2 != PS2.Temp) | is.na(PS2.Temp) | PS2 == "",
             SKU,
             by = .(PS2, PS2.Temp, PS)])
    cat("\n")
    
  } else {
    print("Logic PS2: OK")
  }
  
  # PS3
  
  if (df[(PS3 != PS3.Temp) | is.na(PS3.Temp) | PS3 == "", .N] > 0) {
    cat("\n")
    print("Suspicious PS3 or PS:")
    cat("\n")
    print(df[(PS3 != PS3.Temp) | is.na(PS3.Temp) | PS3 == "",
             SKU,
             by = .(PS3, PS3.Temp, PS)])
    cat("\n")
    
  } else {
    print("Logic PS3: OK")
  }
  
  # PSV
  is.OK.PSV = TRUE
  
  # Check for PS3 & PS0 correspondence #### REWORK for PS, not PS3
  
  # dictPSVPS3 = c("Specials",
  #                "Enhanced Recovery",
  #                "Faltering Growth",
  #                "Metabolics",
  #                "Recovery")
  
  dictPSVPS = c("Allergy Treatment",
                "Faltering Growth",
                "Soy",
                "DR-NL",
                "Enhanced Recovery",
                "Digestive Comfort",
                "Recovery"
  )
  
  # if (df[(is.na(PSV) | PSV == "") & (PS3 %in% dictPSVPS3), .N] > 0 |
  #     df[!(is.na(PSV) |
  #          PSV == "") & !(PS3 %in% dictPSVPS3), .N] > 0) {
  #   is.OK.PSV = FALSE
  #   print("PSV must be only for Specials and AMN")
  #   print(df[(is.na(PSV) | PSV == "") & (PS3 %in% dictPSVPS3),
  #            SKU,
  #            by = .(PSV, PS3)])
  #   print(df[!(is.na(PSV) | PSV == "") & !(PS3 %in% dictPSVPS3),
  #            SKU,
  #            by = .(PSV, PS3)])
  #   cat("\n")
  # }
  
  if (df[(is.na(PSV) | PSV == "") & (PS %in% dictPSVPS), .N] > 0 |
      df[!(is.na(PSV) |
           PSV == "") & !(PS %in% dictPSVPS), .N] > 0) {
    is.OK.PSV = FALSE
    print("PSV must be only for selected PS only. See dictinoary for details.")
    print(df[(is.na(PSV) | PSV == "") & (PS %in% dictPSVPS),
             SKU,
             by = .(PSV, PS)])
    print(df[!(is.na(PSV) | PSV == "") & !(PS %in% dictPSVPS),
             SKU,
             by = .(PSV, PS)])
    cat("\n")
  }
  
  # AT
  
  if (df[PS == "Allergy Treatment" &
         !(PSV %in% c("AAF", "EHF")), .N > 0] |
      df[PS != "Allergy Treatment" &
         (PSV %in% c("AAF", "EHF")), .N > 0]) {
    is.OK.PSV = FALSE
    
    print("Allergy Treatment and AAF or EHF must correspond")
    print(df[PS == "Allergy Treatment" & !(PSV %in% c("AAF",
                                                      "EHF")),
             SKU,
             by = .(PSV, PS)])
    print(df[PS != "Allergy Treatment" & (PSV %in% c("AAF",
                                                     "EHF")),
             SKU,
             by = .(PSV, PS)])
    cat("\n")
    
  }
  
  # DC
  
  if (df[PS == "Digestive Comfort" &
         !(PSV %in% c("Digestive Comfort Core",
                      "Fermented & Other")), .N] > 0 |
      df[PS != "Digestive Comfort" &
         (PSV %in% c("Digestive Comfort Core",
                     "Fermented & Other")), .N] > 0) {
    is.OK.PSV = FALSE
    
    print("Digestive Comfort and Digestive Comfort Core or Fermented & Other musr correspond")
    print(df[PS == "Digestive Comfort" &
               !(PSV %in% c("Digestive Comfort Core",
                            "Fermented & Other")),
             SKU,
             by = .(PSV, PS)])
    print(df[PS != "Digestive Comfort" &
               (PSV %in% c("Digestive Comfort Core",
                           "Fermented & Other")),
             SKU,
             by = .(PSV, PS)])
    cat("\n")
    
  }
  
  # DR-NL
  
  if (df[PS == "DR-NL" & PSV != "Reduced Lactose", .N] > 0 |
      df[PS != "DR-NL" & PSV == "Reduced Lactose", .N] > 0) {
    is.OK.PSV = FALSE
    
    print("DR-NL must correspond to Reduced Lactose")
    print(df[PS == "DR-NL" & PSV != "Reduced Lactose",
             SKU,
             by = .(PSV, PS)])
    print(df[PS != "DR-NL" & PSV == "Reduced Lactose",
             SKU,
             by = .(PSV, PS)])
    
    cat("\n")
    
  }
  
  # Enhanced Recovery
  
  if (df[PS == "Enhanced Recovery" & PSV != "Up-Age Tube", .N] > 0 |
      df[PS != "Enhanced Recovery" & PSV == "Up-Age Tube", .N] > 0) {
    is.OK.PSV = FALSE
    
    print("Enhanced Recovery must correspond to Up-Age Tube")
    print(df[PS == "Enhanced Recovery" & PSV != "Up-Age Tube",
             SKU,
             by = .(PSV, PS)])
    print(df[PS != "Enhanced Recovery" & PSV == "Up-Age Tube",
             SKU,
             by = .(PSV, PS)])
    
    cat("\n")
    
  }
  
  # Faltering Growth
  
  if (df[(PS == "Faltering Growth" |
          PS == "Recovery") & PSV != "Up-Age Oral", .N] > 0 |
      df[!(PS == "Faltering Growth" |
           PS == "Recovery") & PSV == "Up-Age Oral", .N] > 0) {
    is.OK.PSV = FALSE
    
    print("Faltering Growth and Recovery must correspond to Up-Age Oral")
    print(df[(PS == "Faltering Growth" |
                PS == "Recovery") & PSV != "Up-Age Oral",
             SKU,
             by = .(PSV, PS)])
    print(df[!(PS == "Faltering Growth" |
                 PS == "Recovery") & PSV == "Up-Age Oral",
             SKU,
             by = .(PSV, PS)])
    
    cat("\n")
    
  }
  
  # Soy
  
  if (df[PS == "Soy" & !(PSV %in% c("DC Soy", "AT Soy")), .N] > 0 |
      df[PS != "Soy" & PSV %in% c("DC Soy", "AT Soy"), .N] > 0) {
    is.OK.PSV = FALSE
    
    print("Soy must correspond to DC Soy or AT Soy")
    print(df[(PS == "Soy") & !(PSV %in% c("DC Soy", "AT Soy")),
             SKU,
             by = .(PSV, PS)])
    print(df[(PS != "Soy") & PSV %in% c("DC Soy", "AT Soy"),
             SKU,
             by = .(PSV, PS)])
    
    cat("\n")
    
  }
  
  if (is.OK.PSV == TRUE) {
    print("Logic PSV: OK")
  } else {
    print("Check PSV, correct necessary cells and try again.")
  }
  
  # Organic
  
 if (df[!(is.na(Organic) | Organic == "") & 
        !(PS0 %in% c("IMF", "AMN")), .N] > 0 |
     df[(is.na(Organic) | Organic == "") & 
        (PS0 %in% c("IMF", "AMN")), .N] > 0) {
    
    print("Organic must be set for all IMF & AMN and only for them")
    print(df[!(is.na(Organic) | Organic == "") & 
               !(PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, Organic)])
    print(df[(is.na(Organic) | Organic == "") & 
               (PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, Organic)])
    
    cat("\n")
    
  }
  
  # CSS  CAN BE MADE IN A LOOP
  
  is.OK.CSS = TRUE
  
  if (df[!(is.na(CSS) | CSS == "") & 
         !(PS0 %in% c("IMF", "AMN")), .N] > 0 |
      df[(is.na(CSS) | CSS == "") & 
         (PS0 %in% c("IMF", "AMN")), .N] > 0) {
    
    is.OK.CSS = FALSE
    
    print("CSS must be set for all IMF & AMN and only for them")
    print(df[!(is.na(CSS) | CSS == "") & 
               !(PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, CSS)])
    print(df[(is.na(CSS) | CSS == "") & 
               (PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, CSS)])
    
    cat("\n")
    
  }
  
  if (df[CSS != "Other Core" & PS0 == "AMN", .N] > 0) {
    
    is.OK.CSS = FALSE
    
    print("AMN must be Other Core only!")
    print(df[CSS != "Other Core" & PS0 == "AMN",
             SKU,
             by = .(PS0, CSS)])
    
    cat("\n")
    
  }
  
  if (df[CSS %in% c("Hungry", "Sleep") & 
         !(PS3 %in% c("Base", "Base Plus")), .N] > 0) {
    
    is.OK.CSS = FALSE
    
    print("Hungry & Sleep can be either Base or Base Plus!")
    print(df[CSS %in% c("Hungry", "Sleep") & 
               !(PS3 %in% c("Base", "Base Plus")),
             SKU,
             by = .(PS3, CSS)])
    cat("\n")
    
  }
  
    
  
  if (is.OK.CSS == TRUE) {
    print("Logic CSS: OK")
  } else {
    print("Check CSS, correct necessary cells and try again.")
  }
  
  # Protein
  
  is.OK.Protein = TRUE
  
  if (df[!(is.na(Protein) | Protein == "") & 
         !(PS0 %in% c("IMF", "AMN")), .N] > 0 |
      df[(is.na(Protein) | Protein == "") & 
         (PS0 %in% c("IMF", "AMN")), .N] > 0) {
    
    is.OK.Protein = FALSE
    
    print("Protein must be set for all IMF & AMN and only for them")
    print(df[!(is.na(Protein) | Protein == "") & 
               !(PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, Protein)])
    print(df[(is.na(Protein) | Protein == "") & 
               (PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, Protein)])
    
    cat("\n")
    
  }
  
  if (df[Protein == "Goat" & PS != "Goat", .N] > 0 |
      df[Protein != "Goat" & PS == "Goat", .N] > 0) {
    
    is.OK.Protein = FALSE
    
    print("Goat must be Goat both in Protein and PS.")
    print(df[Protein == "Goat" & PS != "Goat",
             SKU,
             by = .(PS0, Protein)])
    print(df[Protein != "Goat" & PS == "Goat",
             SKU,
             by = .(PS0, Protein)])
    
    cat("\n")
    
  }
  
  if (df[Protein != "Soy" & PS == "Soy", .N] > 0) {
    
    is.OK.Protein = FALSE
    
    print("If PS = Soy then Protein also must be Soy.")
    print(df[Protein != "Soy" & PS == "Soy",
             SKU,
             by = .(PS0, Protein)])
    
    cat("\n")
    
  }
  
  if (df[Protein == "Soy" & !(PS == "Soy" | PS0 == "AMN"), .N] > 0) {
    
    is.OK.Protein = FALSE
    
    print("If PS = Soy then Protein also must be Soy.")
    print(df[Protein = "Soy" & !(PS == "Soy" | PS0 == "AMN"),
             SKU,
             by = .(PS0, PS, Protein)])
    
    cat("\n")
    
  }
  
  if (df[Protein == "Other protein origin" & 
         !(PS == "Allergy Treatment" | 
           PS == "DR-NL" | 
           PS0 == "AMN"), .N] > 0) {
    
    is.OK.Protein = FALSE
    
    print("If PS = Soy then Protein also must be Soy.")
    print(df[Protein == "Other protein origin" & 
               !(PS == "Allergy Treatment" | 
                   PS == "DR-NL" | 
                   PS0 == "AMN"),
             SKU,
             by = .(PS0, PS, Protein)])
    
    cat("\n")
    
  }
  
  
  if (is.OK.Protein == TRUE) {
    print("Logic Protein: OK")
  } else {
    print("Check Protein, correct necessary cells and try again.")
  }
  
  # Flavoured
  
  is.OK.Flavoured = TRUE
  
  if (df[!(is.na(Flavoured) | Flavoured == "") & 
         !(PS0 %in% c("IMF", "AMN")), .N] > 0 |
      df[(is.na(Flavoured) | Flavoured == "") & 
         (PS0 %in% c("IMF", "AMN")), .N] > 0) {
    
    is.OK.Flavoured = FALSE
    
    print("Flavoured must be set for all IMF & AMN and only for them")
    print(df[!(is.na(Flavoured) | Flavoured == "") & 
               !(PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, Flavoured)])
    print(df[(is.na(Flavoured) | Flavoured == "") & 
               (PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, Flavoured)])
    
    cat("\n")
    
  }
 
  ### FLAVOURED TO BE FINISHED
  
  dictPSUnflavoured = df1[Flavoured == "Unflavoured", 
                        unique(unlist(stri_split_fixed(Scent, "-")))]
   
  dictPSUnflavoured = c("Goat Milk",
                        "N/S",
                        "Natural",
                        "Proteins",
                        "Sour Milk",
                        "Soy")
  
  dictPSFlavoured = df1[Flavoured == "Flavoured", 
                        unique(unlist(stri_split_fixed(Scent, "-")))]
  
  dictPSFlavoured = c(
    "Apple",
    "Banana",
    "Biscuits",
    "Buckwheat",
    "Buckwheat-Oat",
    "Carrot-Rice",
    "Cereals",
    "Chocolate",
    "Corn",
    "Fennel",
    "Forest Fruits",
    "Fruits",
    "Fruits-Ginger",
    # "Lentil",
    "Mokko",
    "Oat",
    "Orange",
    # "Parsnip",
    "Raspberry",
    "Red Fruits",
    "Rice",
    "Strawberry",
    "Vanilla",
    "Vegetables"
  )

   if (any(dictPSFlavoured %in% dictPSUnflavoured)){
     
     print("Criteria for classification on Flavoured/Unflovoured are not clear.
           Check dictionaries")
   } else {
     df[PS0 == "IMF" & Scent %in% dictPSFlavoured,
        Flavoured.Temp := "Flavoured"]
     df[PS0 == "IMF" & is.na(Flavoured.Temp),
        Flavoured.Temp := "Unflavoured"]
     
     if (df[PS0 == "IMF" & Flavoured != Flavoured.Temp, .N] > 0){
       
       print("Check the following Flavoured classification:")
       print(df[Flavoured != Flavoured.Temp,
                SKU,
                by = .(Flavoured, Flavoured.Temp)])
     }
     
   }
  
  # Storage
  
  is.OK.Storage = TRUE
  
  if (df[!(is.na(Storage) | Storage == "") & 
         !(PS0 %in% c("IMF", "AMN")), .N] > 0 |
      df[(is.na(Storage) | Storage == "") & 
         (PS0 %in% c("IMF", "AMN")), .N] > 0) {
    
    is.OK.Storage = FALSE
    
    print("Storage must be set for all IMF & AMN and only for them")
    print(df[!(is.na(Storage) | Storage == "") & 
               !(PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, Storage)])
    print(df[(is.na(Storage) | Storage == "") & 
               (PS0 %in% c("IMF", "AMN")),
             SKU,
             by = .(PS0, Storage)])
    
    cat("\n")
    
  }
  
  if (df[Form == "Powder" & Storage != "Non Chilled" &
         PS0 %in% c("IMF", "AMN"), .N] > 0) {
    
    is.OK.Storage = FALSE
    
    print("Powder must be Non-chilled")
    print(df[Form == "Powder" & Storage != "Non Chilled" &
               PS0 %in% c("IMF", "AMN"),
             SKU,
             by = .(PS0, Storage)])
    cat("\n")
    
  }
  
  if (is.OK.Storage == TRUE) {
    print("Logic Storage: OK")
  } else {
    print("Check Storage, correct necessary cells and try again.")
  }
  
  
  # PromoPack
  
  if (df[Items.in.pack > 1 & PromoPack == "Regular", .N] > 0 |
      df[Items.in.pack == 1 & PromoPack != "Regular", .N] > 0){
    
    print("Regular pack must have 1 items in pack, if items in pack > 1 it cannot be Regular.")
    print(df[Items.in.pack > 1 & PromoPack == "Regular",
             SKU,
             by = .(PromoPack, Items.in.pack)])
    print(df[Items.in.pack == 1 & PromoPack != "Regular",
             SKU,
             by = .(PromoPack, Items.in.pack)])
    
    cat("\n")
    
  }
  
  if (df[Items.in.pack > 1 & PromoPack != "Multi", .N] > 0 |
      df[Items.in.pack == 1 & PromoPack != "Regular", .N] > 0){
    
    print("Regular pack must have 1 items in pack, if items in pack > 1 it cannot be Regular.")
    print("Multi pack must have items in pack > 1")
    print(df[Items.in.pack > 1 & PromoPack != "Multi",
             SKU,
             by = .(PromoPack, Items.in.pack)])
    print(df[Items.in.pack == 1 & PromoPack != "Regular",
             SKU,
             by = .(PromoPack, Items.in.pack)])
    
    cat("\n")
    
  }
  
  # Size
  check.UOM.db(df)
  
  # Clean
  df[, PS0.Temp := NULL]
  df[, PS2.Temp := NULL]
  df[, PS3.Temp := NULL]
  df[, Flavoured.Temp := NULL]
  
}

check.UOM.db = function(df) {
  names(df)[1] = "SKU"
  # Check the style
  
  if (df[!stri_detect_regex(Size, "^([0-9]+\\*)*[0-9]*\\.?[0-9]+(Gr|Ml|Lt)") &
         Brand != "Private label", .N] > 0) {
    cat("\n")
    print("Strange Size style:")
    print(df[!stri_detect_regex(Size, "^([0-9]+\\*)*[0-9]*\\.?[0-9]+(Gr|Ml|Lt)") &
               Brand != "Private label",
             .(Size = unique(Size)),
             by = SKU])
    
  } else {
    print("Style size: OK")
  }
  
  # cat("\n")
  # print("Strange Size numbers:")
  dt = df[Brand != "Private label", .(Size = unique(Size)), by = SKU]
  dt[, Size.Value := stri_extract_all_regex(Size, "[0-9]*\\.?[0-9]+(?!\\*|x|X)")]
  dt[, Size.UOM := stri_extract_last_regex(Size, "[a-zA-Z]+")]
  
  if (dt[(Size.UOM == "Gr" &
          (Size.Value < 10 | Size.Value > 1000)) |
         (Size.UOM == "Kg" & (Size.Value < 0.1 | Size.Value > 1)) |
         (Size.UOM == "Ml" &
          (Size.Value < 50 | Size.Value > 1000)) |
         (Size.UOM == "Lt" &
          (Size.Value < 0.1 | Size.Value > 5)), .N] > 0) {
    cat("\n")
    print("Strange size numbers:")
    print(dt[(Size.UOM == "Gr" &
                (Size.Value < 10 | Size.Value > 1000)) |
               (Size.UOM == "Kg" &
                  (Size.Value < 0.1 | Size.Value > 1)) |
               (Size.UOM == "Ml" &
                  (Size.Value < 50 | Size.Value > 1000)) |
               (Size.UOM == "Lt" &
                  (Size.Value < 0.1 | Size.Value > 5)),
             Size, by = SKU])
    
  } else {
    print("Size numbers: OK")
  }
  
  rm(dt)
  
}


raw.data.processing = function(df) {
  # This is just columns names, for convinience
  column.names = names(df)
  
  # Define names that doesn't contain items or values in the name
  cols = names(df)[!(
    stri_detect_fixed(tolower(column.names), "items") |
      stri_detect_fixed(tolower(column.names), "volume") |
      stri_detect_fixed(tolower(column.names), "value")
  )]
  
  # Columns in the data file should belong this list
  columns.check = all(
    cols %in% c(
      "Region.pharma",
      "Category.pharma",
      "Segment.pharma",
      "Brand.pharma",
      "ID.morion",
      "SKU",
      "Company.pharma",
      "Size.pharma",
      "Size",
      "Region.nielsen",
      "SKU",
      "BRAND",
      "BRAND.OWNER",
      "DANONE.SEGMENT",
      "DANONE.SUB.SEGMENT",
      "PRODUCT.FORM",
      "TYPE...BABY.PRODUCT",
      "PRODUCT.BASE"
    )
  )
  
  sales.columns.check = all(
    stri_detect_regex(
      tolower(setdiff(column.names, cols)),
      # "[items]{5}.[0-9]{4}.[1-9]{1,2}|[value]{5}0{3}.[0-9]{4}.[1-9]{1,2}|[volume]{6}0{3}.[0-9]{4}.[1-9]{1,2}"
      "[items]{5}.[0-9]{4}.[w]{0,1}[1-9]{1,2}|[value]{5}0{3}.[0-9]{4}.[w]{0,1}[1-9]{1,2}|[volume]{6}0{3}.[0-9]{4}.[w]{0,1}[1-9]{1,2}"
    )
  )
  
  weekly.data.all = all(
    stri_detect_regex(
      tolower(setdiff(column.names, cols)),
      "[w][0-9]{1,2}$"
    )
  )
  
  weekly.data.any = any(
    stri_detect_regex(
      tolower(setdiff(column.names, cols)),
      "[w][0-9]{1,2}$"
    )
  )
  
  if (weekly.data.all == weekly.data.any & weekly.data.all == TRUE){
    weekly.data = TRUE  
  }
  
  check = columns.check & sales.columns.check
  
  if (weekly.data.all != weekly.data.any) {
    check = FALSE
    print("Check periods, looks like a mix of weekly and monthly data!")
    }
  
  if (check != TRUE) {
    if (columns.check == FALSE) {
      print("Some columns are missing.")
    }
    if (sales.columns.check == FALSE) {
      print("Something wrong with the names of the sales columns.")
    }
    
  } else  {
    columns.order = c(
      "ID.morion",
      "SKU",
      "SKU2",
      "Category.pharma",
      "Segment.pharma",
      "Size",
      "Region.pharma",
      "Region.nielsen",
      "Items.in.pack"
    )
    
    columns.for.processing = cols[cols %in% columns.order]
    
    df = add.cleaned.sku(df, columns.for.processing)
    # df[, SKU := NULL]
    
    # columns.for.processing = stri_replace_all_fixed(columns.for.processing,
    #                                                 "SKU", "SKU2")
    # cols = stri_replace_all_fixed(cols, "SKU", "SKU2")
    
    columns.for.processing = c(columns.for.processing, "SKU2")
    cols = c(cols, "SKU2")
    
    formula = columns.for.processing[order(match(columns.for.processing, columns.order))]
    formula = c(columns.for.processing, "Ynb", "Pnb")
    formula = as.formula(paste(paste(formula, collapse = " + "),
                               "Measure", sep = " ~ "))
    
    df = melt.data.table(df, id.vars = cols)
    df[, c("Measure", "Ynb", "Pnb") := tstrsplit(variable, ".", fixed = TRUE)]
    
    df[, Pnb := stri_replace_all_fixed(Pnb, "w", "")]
    
    df = dcast.data.table(df,
                          formula,
                          value.var = "value",
                          fun.aggregate = sum)
    
    if (any(stri_detect_fixed(names(df), "Value000"))) {
      df = df[, Value := Value000 * 1000]
      df[, Value000 := NULL]
      
    }
    
    if (any(stri_detect_fixed(names(df), "Volume000"))) {
      df = df[, Volume := Volume000 * 1000]
      df[, Volume000 := NULL]
      
    }
    
    if (any(stri_detect_fixed(names(df), "Items000"))) {
      df = df[, Items := Items000 * 1000]
      df[, Items000 := NULL]
      
    }
    
    df = remove.zero.sales(df)
    
    # df = add.cleaned.sku(df, columns.for.processing)
    
    df[, Ynb := as.integer(Ynb)]
    df[, Pnb := as.integer(Pnb)]
    
    df = df[, mget(names(df)[order(match(names(df), columns.order))])]
    
    df = df[order(Ynb, Pnb)]
    
    # print("Rows:")
    # print(df[, .N, by = .(Ynb, Mnb)])
    
    print("Checks:")
    
    if (any(stri_detect_fixed(names(df), "Items"))) {
      print(df[, .(.N,
                   Items = sum(Items),
                   Value = sum(Value)),
               by = .(Ynb, Pnb)])
      
    } else if (any(stri_detect_fixed(names(df), "Volume"))) {
      print(df[, .(.N,
                   Volume = sum(Volume),
                   Value = sum(Value)),
               by = .(Ynb, Pnb)])
    }
    
    if (weekly.data == TRUE) {
      names(df)[names(df) == "Pnb"] = "Wnb"
    } else {
      names(df)[names(df) == "Pnb"] = "Mnb"
    }
    
    # df = df[order(Ynb, Mnb)]
    
    print("Checks are OK")
    print("The file containing the latest period(s) is generated.")
    
    return(df)
    
  }
  
}

export.new.skus = function(df, df.existing){
  
  is.pharma.df = any(stri_detect_fixed(tolower(names(df)), "morion"))
  
  if (is.pharma.df == TRUE) {
    unique.ID = c("ID.morion", "SKU2")  
    
  } else {unique.ID = "SKU2"}
  
  original.columns = names(df)[!stri_detect_regex(names(df), "SKU|Region|Items|Value|Volume")]
  cols.united = unique(c(unique.ID, original.columns))
  
  df = remove.zero.sales(df)
  df = add.cleaned.sku(df, original.columns)
  
  # df = df[, .N, by = c(unique.ID, "SKU", original.columns)][, N := NULL]
  # df = df[, .N, by = eval(unique(c(unique.ID, original.columns)))][, N := NULL]

  
  df = df[, .N, by = cols.united][, N := NULL]
  
  # df.new = df[!df.existing[!(is.na(ID) | ID == "")], on = unique.ID]
  df.new = df[!df.existing, on = unique.ID]
  
  # rework for all sources (require change of nielsen data)
  # df.new[dictCompanyBrand, on = c(BRAND = "NielsenBrand"), Brand := i.RTRIBrand]
  
  # add.brand(df.new, df.existing)
  add.brand2(df.new, df.existing, df.matrix, 0.6)
  addSize(df.new)
  
  # Can be moved to similar.description function, as well as ID columns
  if (is.pharma.df == TRUE){
    df.new[, Comments := as.character(NA)]
    
  }
  
  # Extra columns
  df.new[, `:=`(ID = as.integer(NA),
                Date = as.character(Sys.time()))]
  
  return(df.new)
  
}


append.new.data = function(df.new.data,
                           df.historical.data,
                           acceptable.difference = 0.15) {
  # check periods!!!
  # check columns names
  # check price
  # check sales
  # if eveything is ok - append
  
  if (all(names(df.historical.data) %in% names(df.new.data))) {
    print("Column names check is OK")
    df.new.data = df.new.data[, mget(names(df.historical.data))]
    column.names.check = TRUE
    
  } else {
    print("Dataset lacks following columns:")
    print(names(df.historical.data)[!(names(df.historical.data) %in% names(df.new.data))])
    column.names.check = FALSE
    
  }
  
  avg.rows.per.period = df.historical.data[, .N, by = .(Ynb, Mnb)][, mean(tail(N, 6))]
  avg.rows.per.period.new = df.new.data[, .N, by = .(Ynb, Mnb)][, mean(N)]
  avg.rows.ratio = avg.rows.per.period.new / avg.rows.per.period
  
  if (any(stri_detect_fixed(names(df.new.data), "Items"))) {
    
    avg.price = df.historical.data[, .(Price = sum(Value) / sum(Items)),
                                   by = .(Ynb, Mnb)][, mean(tail(Price, 6))]
    avg.price.new = df.new.data[, .(Price = sum(Value) / sum(Items)),
                                by = .(Ynb, Mnb)][, mean(tail(Price, 6))]
    
  } else if (any(stri_detect_fixed(names(df.new.data), "Volume"))){
    
    avg.price = df.historical.data[, .(Price = sum(Value) / sum(Volume)),
                                   by = .(Ynb, Mnb)][, mean(tail(Price, 6))]
    avg.price.new = df.new.data[, .(Price = sum(Value) / sum(Volume)),
                                by = .(Ynb, Mnb)][, mean(tail(Price, 6))]
  }
  
  avg.price.ratio = avg.price.new / avg.price
  
  avg.sales.value = df.historical.data[, .(Value = sum(Value)),
                                       by = .(Ynb, Mnb)][, mean(tail(Value, 6))]
  avg.sales.value.new = df.new.data[, .(Value = sum(Value)),
                                    by = .(Ynb, Mnb)][, mean(tail(Value, 6))]
  
  avg.sales.value.ratio = avg.sales.value.new / avg.sales.value
  
  if (avg.rows.ratio < (1 + acceptable.difference) &
      avg.rows.ratio > (1 - acceptable.difference)) {
    print("The number of records check is OK.")
    avg.rows.ratio.check = TRUE
    
  } else {
    print("Suspicious number of records in the new file.")
    avg.rows.ratio.check = FALSE
  }
  
  if (avg.price.ratio < (1 + acceptable.difference) &
      avg.price.ratio > (1 - acceptable.difference)) {
    print("The price check is OK.")
    avg.price.ratio.check = TRUE
    
  } else {
    print("Suspicious price in the new data.")
    avg.price.ratio.check = FALSE
  }
  
  if (avg.sales.value.ratio < (1 + acceptable.difference) &
      avg.sales.value.ratio > (1 - acceptable.difference)) {
    print("The sales value check is OK.")
    avg.sales.value.ratio.check = TRUE
    
  } else {
    print("Suspicious sales value in the new data.")
    avg.sales.value.ratio.check = FALSE
  }
  
  check = column.names.check &
    avg.rows.ratio.check &
    avg.price.ratio.check &
    avg.sales.value.ratio.check
  
  if (check == TRUE) {
    df.new.data = df.new.data[, mget(names(df.historical.data))]
    df.historical.data = rbindlist(list(df.historical.data, df.new.data))
    df.historical.data = df.historical.data[order(Ynb, Mnb)]
    
    print("The file is updated with the new periods")
    
    return(df.historical.data)
    
  } else {
    print("Check data, correct if necessary and try again!")
    
  }
  
}

check.historical.data = function(df) {
  
  # print(df[, .N, by = .(Ynb, Mnb)])
  
  if (any(stri_detect_fixed(tolower(names(df)), "volume"))) {
    print(df[, .(.N, Volume = sum(Volume), Value = sum(Value)), 
             by = .(Ynb, Mnb)])
  } else {
    print(df[, .(.N, Items = sum(Items), Value = sum(Value)), 
             by = .(Ynb, Mnb)])
    
  }
}

similar.description = function(df) {
  
  # Check that the right file is obtained
  
  df.temp = df[, .N, by = ID.morion][N > 1]
  df[df.temp, on = "ID.morion", Comment := "Same ID.morion"]
  
  df.temp = df.sku.proxima[, .N, by = SKU2][N > 1]
  df[df.temp, on = "SKU2", Comment := "Same SKU name"]
  
  df.temp = df.sku.proxima[, .N, by = .(ID.morion, SKU2)][N > 1]
  df[df.temp, on = c("ID.morion", "SKU2"),
     Comment := "Duplicate line"]
  
  rm(df.temp)
  
}

check.periods.equal = function(...) {
  
  df.list = list(...)
  df.list = lapply(df.list, function(x) x[, .(Mnb = unique(Mnb)), by = Ynb])
  
  periods.equal = all(sapply(df.list, function(x) all.equal(x, df.list[[1]])))
  
  # Clean memory
  rm(df.list)
  
  return(periods.equal)
}

rse <- function(x) {sd(x)/(mean(x)*sqrt(length(x)))}

get.nielsen.name = function(sku.name) {
  return(df.sku.nielsen[SKU3 == sku.name, SKU2])
}


rolling.sum <- function(x, n = 3) {
  (cs <- cumsum(x)) - c(rep(0, n), cs[1:(length(x)-n)])
}


check.brand.price = function(df, Year = NULL, Month = NULL){
  
  if (is.null(Year)) {Year = df[, max(Ynb)]}
  if (is.null(Month)) {Month = df[Ynb == max(Ynb), max(Mnb)]}
  
  df[, SKU.price := ValueC/VolumeC]
  df[, brand.price := sum(ValueC)/sum(VolumeC), 
     by =.(Brand, Ynb, Mnb, PS3, Form)]
  df[, index := SKU.price/brand.price]
  
  return(df[Ynb == Year & 
       Mnb == Month & 
       (index < 0.8 | index > 1.2)])
  
}