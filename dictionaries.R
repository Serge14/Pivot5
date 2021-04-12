
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

