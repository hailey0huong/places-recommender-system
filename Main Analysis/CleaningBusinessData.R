#install.packages("rjson")
#install.packages('RJSONIO')
#install.packages('plyr')
#install.packages("stringr")

library(rjson)
library(RJSONIO) 
library(plyr)
library(stringr)

# Sets the working directory. 
setwd('/home/arcticnerd/Documents/School work/IIT/CS 571/Project')

# Function for reading in JSON file
convertJSON <- function(f){ 
  dat <- scan(f,what=character(),sep="\n") 
  return(do.call(rbind.fill, lapply(dat,function(x) data.frame(lapply(fromJSON(x),paste,collapse=" | "),stringsAsFactors=FALSE)))) 
}

# Call to read in JSON file
dat.bus <- convertJSON("business.json")

# Passing the dataframe to another varable
restaurant <- dat.bus
# Removing all businesses that are not open
restaurant <- restaurant[restaurant$is_open == 1,]
# Deleting the is_open column
restaurant$is_open <- NULL
# Changing categories and cities to lowercase
restaurant$categories <- tolower(restaurant$categories)
restaurant$city <- tolower(restaurant$city)
# Listing the values in the category field to be removed 
catItemsRemove <- c('restaurant', 'restaurants')
# Selects only the businesses that have the category restaurant
restaurant <- subset(restaurant, grepl(paste(catItemsRemove, collapse="|"), restaurant$categories))
# Removing businesses not in the US
restaurant <- restaurant[restaurant$state != 'ON',]
restaurant <- restaurant[restaurant$state != 'CON',]
restaurant <- restaurant[restaurant$state != 'XWY',]
restaurant <- restaurant[restaurant$state != 'QC',]
restaurant <- restaurant[restaurant$state != 'XGM',]
restaurant <- restaurant[restaurant$state != 'AB',]
restaurant <- restaurant[restaurant$state != 'BC',]
restaurant <- restaurant[restaurant$state != 'VT',]
restaurant <- restaurant[restaurant$state != 'BAS',]
restaurant <- restaurant[restaurant$state != 'XGL',]
# removing misslabed Canadan businesses
restaurant <- restaurant[restaurant$business_id != 'sSlMkHBYFOMYbrYG5Jg0Bw', ]
restaurant <- restaurant[restaurant$business_id != 'wfe31gLa1qtAgiCVI0cH2g', ]
# fixing mislabled states
restaurant[restaurant$business_id == 'UcCJfq0PElINpflaTH8tPA', ]$state <- 'NV'


# part 2
catItemsRemove <- c(catItemsRemove, "food", "nightlife", "event planning & services", "lounges", "venues & event spaces", 
                    "flowers & gifts", "art galleries", "arts & entertainment", "shopping", "shopping centers", "museums", 
                    "florists", "automotive", "gas stations", "grocery", "internet cafes", "active life", "golf", "fashion", 
                    "men's clothing", "accessories", "department stores", "shoe stores", "women's clothing", "public services & government", 
                    "mailbox centers", "car wash", "local services", "performing arts", "dance studios", "fitness & instruction", "music venues", 
                    "ethical grocery", "dance clubs", "hotels & travel", "hotels", "casinos", "cooking classes", "arts & crafts", "party & event planning", 
                    "restaurant supplies", "wholesalers", "professional services", "tours", "landmarks & historical buildings", "bed & breakfast",
                    "gift shops", "wedding planning", "resorts", "wholesale stores", "grilling equipment", "furniture reupholstery", "home & garden", 
                    "jazz & blues", "swimming pools", "mini golf", "herbs & spices", "parks", "recreation centers", "stadiums & arenas", "amateur sports teams",
                    "health markets", "cannabis dispensaries", "indoor playcentre", "arcades", "community service/non-profit", "personal chefs", 
                    "playgrounds", "kids activities", "tennis", "travel services", "hiking", "visitor centers", "beaches", "skating rinks", "adult entertainment",
                    "strip clubs", "knife sharpening", "education", "art classes", "street art", "skin care", "day spas", "beauty & spas", 
                    "massage", "nail salons", "pool halls", "organic stores", "laser tag", "horse racing", "bowling", "clowns", "bus tours", 
                    "antiques", "magicians", "marketing", "boat charters", "air duct cleaning", "fire protection services", "home cleaning", "office cleaning",
                    "home services", "international grocery", "pets","comedy clubs", "hair salons", "wine tasting classes", "tasting classes", 
                    "real estate", "social clubs", "dry cleaning & laundry", "contractors", "apartments", "drugstores", "dentists", "golf lessons",
                    "massage therapy", "cosmetic dentists", "health & medical", "bingo halls", "country dance halls", "art schools", "airport shuttles",
                    "outdoor furniture stores", "adult education", "specialty schools", "laundry services", "dry cleaning", "transportation", 
                    "party bus rentals", "personal assistants", "car rental", "security services", "personal shopping", "limos", "real estate services",
                    "climbing", "trampoline parks", "pet services", "waxing", "sailing", "hair removal", "hair stylists", "ethnic grocery", "laser hair removal",
                    "festivals", "religious organizations", "churches", "outlet stores", "nutritionists", "cooking schools", "bar crawl", 
                    "doctors", "family practice", "team building activities", "appliances & repair", "eyelash service", "nail technicians",
                    "car dealers", "amusement parks", "yelp events", "mags", "books", "general dentistry", "sporting goods", "music & video", 
                    "movers", "psychologists", "bookstores", "reiki", "psychics", "counseling & mental health", "life coach", "meditation centers",
                    "supernatural readings", "eatertainment", "barbers", "police departments", "discount store", "office equipment", "cheese tasting classes",
                    "financial services", "check cashing/pay-day loans", "christmas trees", "animal shelters", "currency exchange", "vitamins & supplements",
                    "bounce house rentals", "aquariums", "jewelry", "csa", "party equipment rentals", "aquarium services", "hobby shops", "pet stores",
                    "local fish stores", "cinema", "spray tanning", "tanning beds", "tanning", "middle schools & high schools", "botanical gardens",
                    "elementary schools", "preschools", "hospitals", "parking", "tobacco shops", "hostels", "coffee & tea supplies", "libraries",
                    "cardiologists", "emergency medicine", "pediatricians", "cosmetics & beauty supply", "hair extensions", "pool & billiards",
                    "photographers", "event photography", "session photography", "golf equipment shops", "landscaping", "nurseries & gardening",
                    "gardeners", "tickets", "ticket sales", "campgrounds", "lakes", "shaved snow", "vintage & consignment", "used", "musicians",
                    "keys & locksmiths", "photography stores & services", "pharmacy", "used bookstore", "boat repair", "marinas", "boat dealers",
                    "flooring", "foundation repair", "masonry/concrete", "party supplies", "bartending schools", "septic services", "town car service",
                    "tableware", "uniforms", "building supplies", "hardware stores", "colleges & universities", "skydiving", "soccer", "rv parks",
                    "food banks", "vinyl records", "videos & video game rental", "sports wear", "metal fabricators", "trainers", "food tours", 
                    "pet adoption", "wedding chapels", "toy stores", "couriers & delivery services", "leisure centers", "ice delivery", "zoos", 
                    "cultural center", "pilates", "furniture rental", "auto repair", "fur clothing", "photo booth rentals", "furniture repair", 
                    "lighting fixtures & equipment", "electronics", "djs", "audio/visual equipment rental", "airports", "blow dry/out services", 
                    "wigs", "security systems", "business consulting", "electricians", "rock climbing", "printing services", "medical spas",
                    "screen printing", "towing", "plumbing", "head shops", "gun/rifle ranges", "souvenir shops", "cannabis clinics", "cannabis collective", 
                    "gyms", "bike rentals", "piercing", "pawn shops", "tattoo", "oaxacan", "funeral services & cemeteries",
                    "race tracks", "rehabilitation center", "weight loss centers", "mortgage brokers", "batting cages", "fireplace services",
                    "floral designers", "guest houses", "horseback riding", "paint-your-own pottery", "paint & sip", "vacation rentals",
                    "boating",  "cards & stationery",  "golf equipment",  "vehicle wraps",  "vehicle wraps",  "golf equipment", "cards & stationery",
                    "boating","auto customization",  "signmaking",  "auto glass services" , "car window tinting", "banks & credit unions", 
                    "martial arts", "yoga", "community centers",  "laundromat", "home decor", "feng shui",  "bike repair/maintenance", 
                    "bikes", "hats", "trophy shops", "tabletop games", "eyewear & opticians", "mobile phones", "special education", "optometrists", 
                    "tires", "veterinarians", "graphic design", "vape shops", "studio taping", "plus size fashion", "bespoke clothing", "formal wear", 
                    "art supplies", "lawyers", "tax services", "wills",  "trusts", "tax law", "bankruptcy law", "estate planning law", "& probates",
                    "courthouses",  "transmission repair", "windshield installation & repair", "oil change stations", "insurance", "auto parts & supplies",
                    "auto insurance", "body shops", "handyman", "animal physical therapy", "dog walkers", "holistic animal care",  "private tutors", 
                    "pet training", "animal assisted therapy", "archery", "home window tinting", "art museums", "airport lounges", "taxis", "walking tours",
                    "historical tours", "baby gear & furniture", "pumpkin patches", "pick your own farms", "auto upholstery", "auto detailing",  "motorcycle repair", 
                    "ski resorts", "rv repair",  "real estate agents",  "traditional clothing", "parenting classes", "physical therapy", "occupational therapy",
                    "reflexology", "airport terminals", "acupuncture", "commercial truck repair", "truck rental",  "university housing", "immigration law", 
                    "bridal", "health retreats", "glass & mirrors", "door sales/installation",  "hunting & fishing supplies", "outdoor gear", "hot tub & pool",
                    "pool & hot tub service", "golf cart dealers", "thrift stores", "holiday decorations", "makeup artists", "heating & air conditioning/hvac",
                    "aircraft repairs", "roofing", "siding", "interior design", "airsoft", "clothing rental", "screen printing/t-shirt printing", "painters",
                    "drywall installation & repair", "advertising", "web design", "brazilian jiu-jitsu",  "summer camps", "officiants", "shared office spaces",
                    "post offices",  "public markets", "kids hair salons",  "property management", "engraving", "mass media", "cigar bars",  "estate liquidation",
                    "ophthalmologists", "employment agencies", "cideries", "virtual reality centers", "dance schools", "hotel bar",  "naturopathic/holistic", 
                    "medical centers", "service stations", "unofficial yelp events", "water heater installation/repair", "medical cannabis referrals", 
                    "diagnostic services", "laboratory testing", "eyebrow services", "pressure washers",  "window washing", "gutter services", 
                    "software development", "divorce & family law", "personal injury law", "windows installation")




# Fuction for removing the items listed in catItemsRemove from the category field
breakCategory <- function(inCat, pattern){
  inCat <- paste(setdiff(unlist(lapply(strsplit(inCat, ","), str_trim)), pattern), collapse = ", ")
}

# Loop for using the breakCategory function on the category field, should be replaced with an apply()
for (i in 1:nrow(restaurant)){
  restaurant$categories[i] <- breakCategory(restaurant$categories[[i]], catItemsRemove)
}

# Places a NA value in all blank category entries, this happens when all fields are removed by breakCategory
restaurant$categories[restaurant$categories == ""] <- NA


#correcting city names AZ
# levels(as.factor((restaurant[restaurant$state == 'AZ',]$city)))

for(i in 1:nrow(restaurant[restaurant$state == 'AZ',])){
  if (restaurant[restaurant$state == 'AZ',]$city[i] == 'glendale az'){
    restaurant[restaurant$state == 'AZ',]$city[i] <- 'glendale'
  } else if (restaurant[restaurant$state == 'AZ',]$city[i] == 'mesa az'){
    restaurant[restaurant$state == 'AZ',]$city[i] <- 'mesa'
  } else if (restaurant[restaurant$state == 'AZ',]$city[i] == 'phx' | restaurant[restaurant$state == 'AZ',]$city[i] == 'central' 
             | restaurant[restaurant$state == 'AZ',]$city[i] == 'central city' | restaurant[restaurant$state == 'AZ',]$city[i] == 'ahwatukee' 
             | restaurant[restaurant$state == 'AZ',]$city[i] == 'central city village' | restaurant[restaurant$state == 'AZ',]$city[i] == 'laveen'
             | restaurant[restaurant$state == 'AZ',]$city[i] == 'laveen village' | restaurant[restaurant$state == 'AZ',]$city[i] == 'phoenix valley'){
    restaurant[restaurant$state == 'AZ',]$city[i] <- 'phoenix'
  }  else if (restaurant[restaurant$state == 'AZ',]$city[i] == 'scottdale'){
    restaurant[restaurant$state == 'AZ',]$city[i] <- 'scottsdale'
  }  else if (restaurant[restaurant$state == 'AZ',]$city[i] == 'sun city west'){
    restaurant[restaurant$state == 'AZ',]$city[i] <- 'sun city'
  }
}

#correcting city names NC
# levels(as.factor((restaurant[restaurant$state == 'NC',]$city)))

for(i in 1:nrow(restaurant[restaurant$state == 'NC',])){
  if (restaurant[restaurant$state == 'NC',]$city[i] == 'harrisbug'){
    restaurant[restaurant$state == 'NC',]$city[i] <- 'harrisburg'
  } else if (restaurant[restaurant$state == 'NC',]$city[i] == 'mint  hill'){
    restaurant[restaurant$state == 'NC',]$city[i] <- 'mint hill'
  }  else if (restaurant[restaurant$state == 'NC',]$city[i] == 'mt holly' | restaurant[restaurant$state == 'NC',]$city[i] == 'mt. holly'){
    restaurant[restaurant$state == 'NC',]$city[i] <- 'mount holly'
  }  else if (restaurant[restaurant$state == 'NC',]$city[i] == 'blakeney' | restaurant[restaurant$state == 'NC',]$city[i] == 'harrisburg'){
    restaurant[restaurant$state == 'NC',]$city[i] <- 'charlotte'
  }  else if (restaurant[restaurant$state == 'NC',]$city[i] == 'concord mills'){
    restaurant[restaurant$state == 'NC',]$city[i] <- 'concord'
  }  
}

#correcting city names NV
# levels(as.factor((restaurant[restaurant$state == 'NV',]$city)))

for(i in 1:nrow(restaurant[restaurant$state == 'NV',])){
  if (restaurant[restaurant$state == 'NV',]$city[i] == 'las  vegas' | restaurant[restaurant$state == 'NV',]$city[i] == 'lasvegas' 
      | restaurant[restaurant$state == 'NV',]$city[i] == 'south las vegas'){
    restaurant[restaurant$state == 'NV',]$city[i] <- 'las vegas'
  } else if (restaurant[restaurant$state == 'NV',]$city[i] == 'nellis afb'){
    restaurant[restaurant$state == 'NV',]$city[i] <- 'nellis air force base'
  } else if (restaurant[restaurant$state == 'NV',]$city[i] == 'n las vegas' | restaurant[restaurant$state == 'NV',]$city[i] == 'n. las vegas'){
    restaurant[restaurant$state == 'NV',]$city[i] <- 'north las vegas'
  }
}

# correcting city names OH
# levels(as.factor((restaurant[restaurant$state == 'OH',]$city)))

for(i in 1:nrow(restaurant[restaurant$state == 'OH',])){
  if (restaurant[restaurant$state == 'OH',]$city[i] == 'auburn twp'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'auburn township'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'cleveland hghts.'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'cleveland heights'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'cuyahoga fls'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'cuyahoga falls'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'garfield hts'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'garfield heights'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'mentor on the' | restaurant[restaurant$state == 'OH',]$city[i] == 'mentor-on-the-lake'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'mentor on the lake'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'north olmstead'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'north olmsted'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'richmond height'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'richmond heights'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'warrensville hts.'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'warrensville heights'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'warrensville hts.'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'warrensville heights'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'bath'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'bath township'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'brookpark'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'brook park'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'concord'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'concord township'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'hyland heights'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'highland heights'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'mayfield village'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'mayfield'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'n ridgeville'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'north ridgeville'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'olmsted township'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'olmsted falls'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'orange village'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'orange'
  } else if (restaurant[restaurant$state == 'OH',]$city[i] == 'sheffield village'){
    restaurant[restaurant$state == 'OH',]$city[i] <- 'sheffield'
  }
}

# correcting city names PA
# levels(as.factor((restaurant[restaurant$state == 'PA',]$city)))

for(i in 1:nrow(restaurant[restaurant$state == 'PA',])){
  if (restaurant[restaurant$state == 'PA',]$city[i] == 'bellvue'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'bellevue'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'canonsburd'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'canonsburg'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'east mc keesport'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'east mckeesport'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'moon twp'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'moon township'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'mt lebanon' | restaurant[restaurant$state == 'PA',]$city[i] == 'mount lebanon'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'mt. lebanon'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'north huntingdon'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'north huntington'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'robinson twp.'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'robinson township'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'upper st clair'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'upper saint clair'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'downtown' | restaurant[restaurant$state == 'PA',]$city[i] == 'east liberty' 
             | restaurant[restaurant$state == 'PA',]$city[i] == 'lower lawrenceville' | restaurant[restaurant$state == 'PA',]$city[i] == 'oakland'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'pittsburgh'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'mc donald'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'mcdonald'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'mc kees rocks'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'mckees rocks'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'mc murray' | restaurant[restaurant$state == 'PA',]$city[i] == 'mcmurray'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'peters township'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'mccandless township'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'mccandless'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'moon township'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'moon'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'north huntington'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'north huntingdon'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'penn hills township'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'penn hills'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'plum boro'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'plum'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'robinson township'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'robinson'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'ross township'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'ross township'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'rostraver'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'rostraver township'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'south park'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'south park township'
  } else if (restaurant[restaurant$state == 'PA',]$city[i] == 'upper saint clair'){
    restaurant[restaurant$state == 'PA',]$city[i] <- 'upper st. clair'
  }
}

# correcting city names WI
# levels(as.factor((restaurant[restaurant$state == 'WI',]$city)))

for(i in 1:nrow(restaurant[restaurant$state == 'WI',])){
  if (restaurant[restaurant$state == 'WI',]$city[i] == 'mc farland'){
    restaurant[restaurant$state == 'WI',]$city[i] <- 'mcfarland'
  } else if (restaurant[restaurant$state == 'WI',]$city[i] == 'sun praiie'){
    restaurant[restaurant$state == 'WI',]$city[i] <- 'sun prairie'
  } else if (restaurant[restaurant$state == 'WI',]$city[i] == 'deforest'){
    restaurant[restaurant$state == 'WI',]$city[i] <- 'de forest'
  }
}


                    






catList <- c()
for(i in 1:nrow(restaurant)){
  catList <- c(catList, unlist(strsplit(restaurant$categories[i], ", ")))
}

catNames <- unique(catList, fromLast = FALSE)


num.iters <- length(catNames)
l <- vector('list', num.iters) 
for (i in 1:num.iters) {
  l[[i]] <- 0                       # the column data
  names(l)[i] <- paste('Col', i, sep='.')  # the column name 
} 
# do.call(cbind, l)  # ... if your cols are the same datatype and you want a matrix
temp <- cbind(restaurant, data.frame(l))
names(temp) <- c(names(restaurant), catNames)

temp[temp$categories == "",] <- NA

library(stats)
temp <- temp[complete.cases(temp),]


for (i in 1:nrow(temp)){
  hold <- strsplit(temp$categories[i], split = ", ")
  for (j in 1:length(hold[[1]])){
    temp[i,hold[[1]][j]] <- 1
  }
}



# writes the dataframe as a cvs file
write.csv(temp, file = "business.csv",row.names=FALSE)
