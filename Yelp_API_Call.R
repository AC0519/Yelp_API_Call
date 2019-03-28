####libraries----
library(tidyverse)
library(ggplot2)
library(yelpr)


##########Extract all Alexandria Va restaurants from Yelp----


key <- #Get your own


#built using yelpr library
restaurant_function <- function(key, location, term, offset){
  business_search(api_key = key,
                  location = location,
                  term = term,
                  offset = offset,
                  limit = 50)
}


#For loop to get 1000 restaurants back at one time

Alexandria_Restaurants <- data.frame()
  
for (offset_val in seq(0,1000,50)) {
  print(offset_val)
  temp <- restaurant_function(key, 'Alexandria', 'restaurants', offset_val) %>% 
    .$businesses %>% 
    mutate(categories = map(categories, flatten_chr)) %>% 
    mutate(categories = lapply(categories, paste, collapse = ", ") %>% unlist()) %>% 
    mutate(latitude = .$coordinates$latitude) %>% 
    mutate(longitude = .$coordinates$longitude) %>% 
    select(-coordinates) %>% 
    mutate(transactions = lapply(transactions, paste, collapse = ", ") %>% unlist()) %>% 
    mutate(address = .$location$display_address)  %>% 
    mutate(address = lapply(address, paste, collapse = ", ") %>% unlist()) %>% 
    select(-location)
  Alexandria_Restaurants <- bind_rows(Alexandria_Restaurants, temp)
}


####once obtained read in data using this----
Alexandria_Restaurants <- read_csv("Alexandria_Restaurants.csv")

#############Cleaning Data
#Only Alexandria addresses with desired columns----

ar <- Alexandria_Restaurants %>% 
        select(-id, -alias, -is_closed, -transactions, -phone, -distance, -image_url) %>% 
        rename("reviews" = "review_count", "phone" = "display_phone") %>% 
        filter(str_detect(address, "Alexandria"))

ar$rating <- as.factor(ar$rating)



###adjusting all catagories to fit into single name type----
#Using this to see how many of each are in the categories column
sum(str_detect(tolower(ar$name),"peri"))

#seperating like this for further analysis

bakery <- ar %>% 
  filter(str_detect(tolower(categories), "baker"))



###Break down is order dependent based off domain knowledge decisions

ar$categories[grepl("baker", tolower(ar$categories))] <- "Bakery"
ar$categories[grepl("kiwi pie shop", tolower(ar$name))] <- "Bakery" #searching name column not categories like the                                                                         others
ar$categories[grepl("peruvian", tolower(ar$categories))] <- "Peruvian"
ar$categories[grepl("puertorican", tolower(ar$categories))] <- "Puerto Rican"
ar$categories[grepl("pakistani", tolower(ar$categories))] <- "Pakistani"
ar$categories[grepl("russian", tolower(ar$categories))] <- "Russian"
ar$categories[grepl("leban", tolower(ar$categories))] <- "Lebanese"
ar$categories[grepl("afghan", tolower(ar$categories))] <- "Afghani"
ar$categories[grepl("fast food", tolower(ar$categories))] <- "Fast Food"
ar$categories[grepl("japanese", tolower(ar$categories))] <- "Japanese/Sushi"
ar$categories[grepl("chinese", tolower(ar$categories))] <- "Chinese"
ar$categories[grepl("indian", tolower(ar$categories))] <- "Indian"
ar$categories[grepl("ethiopian", tolower(ar$categories))] <- "Ethiopian"
ar$categories[grepl("bbq", tolower(ar$categories))] <- "BBQ"
ar$categories[grepl("southern", tolower(ar$categories))] <- "Southern"
ar$categories[grepl("cajun", tolower(ar$categories))] <- "Cajun"
ar$categories[grepl("mexican", tolower(ar$categories))] <- "Mexican"
ar$categories[grepl("latin", tolower(ar$categories))] <- "Mexican"
ar$categories[grepl("french", tolower(ar$categories))] <- "French"
ar$categories[grepl("german", tolower(ar$categories))] <- "German"
ar$categories[grepl("vietnamese", tolower(ar$categories))] <- "Vietnamese"
ar$categories[grepl("diner", tolower(ar$categories))] <- "Diner"
ar$categories[grepl("pizza", tolower(ar$name))] <- "Pizza" #searching name column not categories like the others
ar$categories[grepl("pizzeria", tolower(ar$name))] <- "Pizza" #same as above

pizza_holdouts <- c("Fat City Kitchen", "RedRocks Neapolitan Bistro", "Juliano's")
pizza_holdouts <- str_c(pizza_holdouts, collapse = "|")
ar$categories[str_detect(ar$name, pizza_holdouts)] <- "Pizza"


ar$categories[grepl("african", tolower(ar$categories))] <- "African"

ar$categories[grepl("italian", tolower(ar$categories))] <- "Italian"
ar$categories[grepl("the italian place", tolower(ar$name))] <- "Italian" #by name
ar$categories[grepl("taverna", tolower(ar$name))] <- "Greek" #searching name column not categories like the others
ar$categories[grepl("mediterranean", tolower(ar$categories))] <- "Mediterranean"
ar$categories[grepl("greek", tolower(ar$categories))] <- "Greek"
ar$categories[grepl("cheesesteak", tolower(ar$categories))] <- "Cheesesteak"
ar$categories[grepl("al's steak house", tolower(ar$name))] <- "Cheesesteak"
ar$categories[grepl("\\bcheese\\b", tolower(ar$categories))] <- "Cheese and Wine" #//b sets a boudary on the word so                                                                                it does not overwrite the cheesesteaks
ar$categories[grepl("irish", tolower(ar$categories))] <- "Irish Pub"
ar$categories[grepl("fishnchips", tolower(ar$categories))] <- "Irish Pub"
ar$categories[grepl("bagel", tolower(ar$name))] <- "Breakfast/Brunch" #searching name column not categories 
ar$categories[grepl("ihop", tolower(ar$name))] <- "Breakfast/Brunch" #same as above
ar$categories[grepl("chicken", tolower(ar$categories))] <- "Wings"
ar$categories[grepl("donuts", tolower(ar$name))] <- "Donuts" #searching name column not categories like the others

#to extract coffee shops
coffee_shop <- c("Stomping Ground", "Killer ESP", "Extra Perks Coffee Shop", "Java Grill", "Bon Vivant Cafe")
coffee_shops <- str_c(coffee_shop, collapse = "|")
ar$categories[str_detect(ar$name, coffee_shops)] <- "Coffee Shop"

ar$categories[grepl("korean", tolower(ar$categories))] <- "Korean"
ar$categories[grepl("belgian", tolower(ar$categories))] <- "Belgian"
ar$categories[grepl("nepalese", tolower(ar$categories))] <- "Nepalese"
ar$categories[grepl("deli", tolower(ar$categories))] <- "Deli"

#deli holdouts
make_deli <- c("Market 2 Market","cafe gloria","sherwood gourmet deli", "Uptowner Cafe")
make_deli <- str_c(make_deli, collapse = "|")
ar$categories[str_detect(ar$name, make_deli)] <- "Deli"



ar$categories[grepl("thai", tolower(ar$categories))] <- "Thai"
ar$categories[grepl("sushi", tolower(ar$categories))] <- "Japanese/Sushi"
ar$categories[grepl("maya's", tolower(ar$name))] <- "Japanese/Sushi" #by name not like most others
ar$categories[grepl("tapas", tolower(ar$categories))] <- "Tapas"

#eliminate a couple of restaurants in the seafood lookup
not_sfood <- c("Burtons Grill & Bar of Alexandria", "Union Street Public House", "Bistro Eighteen90", "Noodles & Company")
not_sfood <- str_c(not_sfood, collapse = "|")
ar$categories[str_detect(ar$name, not_sfood)] <- "American"

ar$categories[grepl("seafood", tolower(ar$categories))] <- "Seafood"
ar$categories[grepl("american", tolower(ar$categories))] <- "American"

#American hold outs
make_american <- c("Mackie's Bar and Grill", "Holy Cow", "Pendleton Carryout", "Shooter McGee's", "Great American Steak and Buffet", "Outback Steakhouse", "Panera Bread","Casa de Sullivan","sweetgreen", "Streets Market & Cafe", "1823 Cafe", "Nando's PERi-PERi", "Peri Peri Original")
make_american <- str_c(make_american, collapse = "|")
ar$categories[str_detect(ar$name, make_american)] <- "American"

ar$categories[grepl("sonoma cellar", tolower(ar$name))] <- "Wine Tasting" #by name, not like most
ar$categories[grepl("breakfast", tolower(ar$categories))] <- "Breakfast"
ar$categories[grepl("tropical smoothie cafe", tolower(ar$name))] <- "Breakfast" #by name, not like most
ar$categories[grepl("middle eastern", tolower(ar$categories))] <- "Middle Eastern"
ar$categories[grepl("dimsum", tolower(ar$categories))] <- "Chinese"

#Mexican hold outs
make_mexican <- c("Tequila & Taco", "Las Catrachitas", "Rosita's Restaurant", "Tippy's Taco House")
make_mexican <- str_c(make_mexican, collapse = "|")
ar$categories[str_detect(ar$name, make_mexican)] <- "Mexican"

#get rid of fake restaurant
ar <- ar[-345,]

###Where are the NAs in the data----
sum(is.na(ar))

ar %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

#which do not have phone numbers
missingphone <- which(is.na(ar$phone))
missingphone <- ar[missingphone, ]

phone <-  function(phone){
  if(is.na(phone)){
    return("Phone Not Provided")
  }else{
    return(phone)
  }
}

ar$phone <- sapply(ar$phone, phone)


#which do not have prices
missingprice <- which(is.na(ar$price))
missingprice <- ar[missingprice, ]

#Fill in missing price with $$ because this is average except dunkin donuts
ar$price[grepl("Dunkin' Donuts", ar$name)] <- "$"

price <-  function(price){
  if(is.na(price)){
    return("$$")
  }else{
    return(price)
  }
}

ar$price <- sapply(ar$price, price)

ar$price <-  as.factor(ar$price)



#####Write data for Tableau Viz----
write_csv(ar, "Alexandria_Restaurants_Cleaned_Data.csv")






