# Use this to find current OPE etc info on schools
# Use crosswalking to correct VA info on OPE etc

### Installing packages for web scraping

#install.packages('rvest')
library(rvest)
library(stringr)


######### Define Functions ################
#info

parsed.info = function(x){
  # Build an index holding the desired info
  # Push to the global environment for use in other functions
  inst_index <<- which(# institute names and addresses
    grepl("^General information:", x)
  ) - 1 # institute names are one index before the General info line
  
  # these get pushed into the global environment closure in order
  # to manage missing info of other lines
  num_records <<- sum(grepl("^General information:", x))
  full_index <<- 1:num_records
  
  # institute name
  ##Below REGEX finds things like: ...University5060... or
  ## ...UniversityPalatino Drive or ...University , ...
  ## retains the first part which must be cleaned up
  inst_name_regex <- 
    "^.*?([:lower:][:upper:]|[:lower:][:digit:]| ,)"
  inst_extract <- str_extract(
    x[inst_index],
    inst_name_regex
  )
  IN <- str_trim(# remove whitespace on the edges (last)
    str_sub(# first, substring out leading and trailing characters
      inst_extract
      ,2,-2 
    ))
  
  # Address
  ## delete the university name and any punctuation
  ## requires walking back the initial extract a bit
  inst_extract <- str_remove(
    inst_extract, "\\w$"# deletes a trailing character or number
  )
  ADDR <- str_remove(# now delete that part to get the address
    x[inst_index],
    inst_extract
  )
  
  ## detect any addresses with extra text at the front and remove it
  extra_index <- grepl(
    "[[:alpha:]][[:digit:]]"# has letters directly abutting numbers
    ,ADDR
  )
  ADDR[extra_index] <- str_remove(
    ADDR[extra_index], '^\\D*'# delete until reaching a digit
  )
  
  ## now eat away from the back of ADDR
  ## Note that some addresses are missing info on the front end
  ### pull Zip code info then delete from the copy, do the same for others
  ZP <- str_extract(ADDR, "\\d{5}(-?\\d{4})?$")
  ADDR <- str_remove(ADDR, " \\d{5}(-?\\d{4})?$")
  
  ST <- str_extract(ADDR, ",( \\w*)*$")# state name is next
  ST <- str_remove(ST, ", ?")# have to delete a comma and space(?)
  ADDR <- str_remove(ADDR, ",( \\w*)*$")
  
  CTY <- str_extract(ADDR, ",?( \\w*)*$")# City is next
  CTY <- str_remove(CTY, ",? ?")# have to delete a comma(?) and space(?)
  ADDR <- str_remove(ADDR, ",?( \\w*)*$")
  
  # collect phone number (two lines after the inst_index)
  PH <- x[inst_index + 2]
  
  return(data.frame(IN,ADDR,CTY,ST,ZP,PH))
}

parsed.type = function(x){
  # get the index, one after the Type line
  type_index <- which(grepl("^Type:", x)) + 1
  type <- str_split(
    x[type_index],
    ",",
    n = 2,# first two values only
    simplify = T # pass to a matrix
    )
  TY1 <- type[,1] # reformat for column names
  TY2 <- type[,2]
  return(data.frame(TY1,TY2))
}

parsed.awards = function(x){
  awards_index <- which(grepl(
    "^Awards offered:", x
  )) + 1# on the next line
  if (length(awards_index) < num_records){# missing data
    AO <- rep(NA, num_records) # data holder, start with all missing
    for (i in awards_index){# replace NA's with any existing records
      for (j in full_index){
        if (inst_index[j] == inst_index[num_records] &#last record edge case
            inst_index[j] < i){
          AO[j] <- x[i]# i is the index in the original data
          break# break out to the outer for statement to skip needless checks
        }
        if (inst_index[j] < i & inst_index[j+1] > i){#value between records
          AO[j] <- x[i]
          break# break out to the outer for statement to skip needless checks
        }
        # No else statement leaves missing indices NA in the tmp object
      }
    }
  } else {
    AO <- x[awards_index]# a full data set
  }
  return(data.frame(AO))
}

parsed.setting = function(x){
  campus_index <- which(grepl("^Campus setting:",x))+1
  if (length(campus_index) < num_records){# missing data
    camp_setting <- rep(NA, num_records) # data holder, start with all missing
    for (i in campus_index){# replace NA's with any existing records
      for (j in full_index){
        if (inst_index[j] == inst_index[num_records] &#last record edge case
            inst_index[j] < i){
          camp_setting[j] <- x[i]# i is the index in the original data
          break# break out to the outer for statement to skip needless checks
        }
        if (inst_index[j] < i & inst_index[j+1] > i){#value between records
          camp_setting[j] <- x[i]
          break# break out to the outer for statement to skip needless checks
        }
        # No else statement leaves missing indices NA in the tmp object
      }
    }
  } else {
    camp_setting <- x[campus_index]# a full data set
  }
  return(data.frame(camp_setting))
}

parsed.housing = function(x){
  housing_index <- which(grepl(
    "^Campus housing:", x
  )) + 1
  if (length(housing_index) < num_records){# missing data
    camp_housing <- rep(NA, num_records) # data holder, start with all missing
    for (i in housing_index){# replace NA's with any existing records
      for (j in full_index){
        if (inst_index[j] == inst_index[num_records] &#last record edge case
            inst_index[j] < i){
          camp_housing[j] <- x[i]# i is the index in the original data
          break# break out to the outer for statement to skip needless checks
        }
        if (inst_index[j] < i & inst_index[j+1] > i){#value between records
          camp_housing[j] <- x[i]
          break# break out to the outer for statement to skip needless checks
        }
        # No else statement leaves missing indices NA in the tmp object
      }
    }
  } else {
    camp_housing <- x[housing_index]# a full data set
  }
  return(data.frame(camp_housing))
}

parsed.population = function(x){
  pop_index <- which(grepl(
    "^Student population:", x
  )) + 1
  if (length(pop_index) < num_records){# missing data
    population <- rep(NA, num_records) # data holder, start with all missing
    for (i in pop_index){# replace NA's with any existing records
      for (j in full_index){
        if (inst_index[j] == inst_index[num_records] &#last record edge case
            inst_index[j] < i){
          population[j] <- x[i]# i is the index in the original data
          break# break out to the outer for statement to skip needless checks
        }
        if (inst_index[j] < i & inst_index[j+1] > i){#value between records
          population[j] <- x[i]
          break# break out to the outer for statement to skip needless checks
        }
        # No else statement leaves missing indices NA in the tmp object
      }
    }
  } else {
    population <- x[pop_index]# a full data set
  }
  return(data.frame(population))
}

parsed.stf_ratio = function(x){
  staff_index <- which(grepl('^Student-to-faculty ratio:', x))+1
  if (length(staff_index) < num_records){# missing data
    stf_ratio <- rep(NA, num_records) # data holder, start with all missing
    for (i in staff_index){# replace NA's with any existing records
      for (j in full_index){
        if (inst_index[j] == inst_index[num_records] &#last record edge case
            inst_index[j] < i){
          stf_ratio[j] <- x[i]# i is the index in the original data
          break# break out to the outer for statement to skip needless checks
        }
        if (inst_index[j] < i & inst_index[j+1] > i){#value between records
          stf_ratio[j] <- x[i]
          break# break out to the outer for statement to skip needless checks
        }
        # No else statement leaves missing indices NA in the tmp object
      }
    }
  } else {
    stf_ratio <- x[staff_index]# a full data set
  }
  return(data.frame(stf_ratio))
}

id_search = function(x){
  ipeds_index <- which(grepl("^IPEDS", x))
  ipeds_id <- x[ipeds_index]
  return(ipeds_id)
}

parsed.id = function(x){#takes above output and extracts ID's
  ipeds = str_sub(x, start = 11, end = 16)#positionally locked
  ope <- str_sub(x, start = 28, end = -3)
  return(data.frame(ipeds, ope))
}

parsed.data = function(x){
  info = parsed.info(x)
  type = parsed.type(x)
  iped_ope = parsed.id(id_search(x))
  awards = parsed.awards(x)
  setting = parsed.setting(x)
  housing = parsed.housing(x)
  population = parsed.population(x)
  stf_ratio = parsed.stf_ratio(x)
  return(
    data.frame(info,type,iped_ope,awards,setting,
               housing,population,stf_ratio))
}

## tester function if you get errors with size of output
# tester = function(x){
#   info = dim(parsed.info(x))
#   type = dim(parsed.type(x))
#   iped_ope = dim(parsed.id(id_search(x)))
#   awards = dim(parsed.awards(x))
#   setting = dim(parsed.setting(x))
#   housing = dim(parsed.housing(x))
#   population = dim(parsed.population(x))
#   stf_ratio = dim(parsed.stf_ratio(x))
#   return(
#     data.frame(info,type,iped_ope,awards,setting,
#                housing,population,stf_ratio))
# }

############# Scrape ####################
### Set up a pause for the loop, 1 second
# Node for getting the # of pages and information
# td, ctl00_cphCollegeNavBody_ucResultsMain_divPagingControls

## Main
### define some of the vars
html_link = 'https://nces.ed.gov/collegenavigator/?s='
states = c('AL','AK','AZ','AR','CA&lc=1','CA&lc=2',#rotate through CA locales
           'CA&lc=2','CA&lc=4','CO','CT','DE',     #ex: lc=1 is city
           'DC','FL','GA','HI','ID','IL','IN','IA',
           'KS','KY','LA','ME','MD','MA','MI',
           'MN','MS','MO','MT','NE','NV','NH','NJ',
           'NM','NY','NC','ND','OH','OK','OR',
           'PA','RI','SC','SD','TN','TX','UT','VT',
           'VA','WA','WV','WI','WY','AS','FM',
           'GU','MH','MP','PW','PR','VI')

main = function(html_link, states){
  df = c()
  for (i in 1:length(states)){
    Sys.sleep(1)
    link = read_html(paste0(html_link, states[i]))
    data = link %>%
      html_nodes('td , #ctl00_cphCollegeNavBody_ucResultsMain_divPagingControls')%>%
      html_text()
    parsed_data = parsed.data(data)
    df = rbind(df,parsed_data)
    print(paste("State", i,':','Success Page: 1'))
    if (grepl('^Previous Page',tail(data, n=1))){
      last_str = tail(data, n=1)
      last_str_split = str_split(last_str, '\\|')
      last_num = last_str_split[[1]][
        length(last_str_split[[1]])
        ]
      last_digit = as.numeric(str_extract(last_num,"[:digit:][:digit:]|[:digit:]"))
      for (j in 2:last_digit){
        Sys.sleep(1)
        link = read_html(
          paste0(html_link, states[i],'&pg=',j))
        data = link %>%
          html_nodes(
            'td , #ctl00_cphCollegeNavBody_ucResultsMain_divPagingControls'
            ) %>%
          html_text()
        parsed_data = parsed.data(data)
        df = rbind(df,parsed_data)
        print(paste("State", i,':','Success','Page:',j))
      }
    } else {
      next()
    }
  }
  return(df)
}

df = main(html_link,states)

write.csv(df,'OPE_IDs_Webscraped.csv')

  


