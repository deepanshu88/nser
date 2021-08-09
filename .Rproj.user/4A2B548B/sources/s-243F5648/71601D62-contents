NSERequest <- function(URL, timeout = 5) {

# Get Cookies
  request <- GET(url = "https://www.nseindia.com/",
                 add_headers("user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36",
                                                                'accept-language' = 'en,gu;q=0.9,hi;q=0.8',
                                                                 'accept-encoding' = 'gzip, deflate, br',
                                                                'Connection' = 'keep-alive',
                                                                'Cache-Control' = 'max-age=0', 'DNT' = '1', 'Upgrade-Insecure-Requests' = '1',
                             'Sec-Fetch-User' = '?1',
                             'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
                             'Sec-Fetch-Site' = 'none',
                             'Sec-Fetch-Mode' = 'navigate'),
                 timeout(timeout))

  ck <- cookies(request)$value
  names(ck) <- cookies(request)$name

  r <- GET(url = URL,
           add_headers("user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36",
                       'accept-language' = 'en,gu;q=0.9,hi;q=0.8',
                       'accept-encoding' = 'gzip, deflate, br',
                       'Connection' = 'keep-alive',
                       'Cache-Control' = 'max-age=0', 'DNT' = '1', 'Upgrade-Insecure-Requests' = '1',
                       'Sec-Fetch-User' = '?1',
                       'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
                       'Sec-Fetch-Site' = 'none',
                       'Sec-Fetch-Mode' = 'navigate'),
           timeout(timeout),
           set_cookies(.cookies = ck))

  response <- content(r, as = "text", encoding = "UTF-8")
  JSON <- fromJSON(response, flatten = TRUE)

  return(JSON)

}

#' Tells if market is opened or not
#'
#' It returns status if market is opened or not
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' df <- MarketStatus()
#' }

MarketStatus <- function() {

  URL <- "https://www.nseindia.com/api/marketStatus"

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  df <- bind_rows(j)

  return(df)

}


#' Holidays List of the current year
#'
#' It returns holidays calendar of the current year.
#'
#' @param type trading or clearing
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' df <- Holidays()
#' }

Holidays <- function(type="trading") {

  if(tolower(type)=="clearing") {
    URL <-  'https://www.nseindia.com/api/holiday-master?type=clearing'
  } else {
    URL <-  'https://www.nseindia.com/api/holiday-master?type=trading'
  }

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  df <- bind_rows(j, .id = "Product")


  return(df)

}


#' Filing Information of financial results
#'
#' It returns filing Information of financial results
#'
#' @param period  "quarterly", "annual". By default it is "quarterly"
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' df <- FilingInfo()
#' }

FilingInfo <- function(period = "quarterly") {

  URL = paste0('https://www.nseindia.com/api/corporates-financial-results?index=equities','&period=', period)

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  df <- bind_rows(j)


  return(df)

}



#' Extract Financial Results of a Company
#'
#' It returns financial results of a company
#'
#' @param Symbol Symbol of the stock you are looking for. For e.g. 'RELIANCE' for Reliance Industries Limited
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' df <- FinancialResults(Symbol = "ACC")
#' }

FinancialResults <- function(Symbol) {

  Symbol <- toupper(gsub('&','%26',Symbol))

  URL = paste0('https://www.nseindia.com/api/results-comparision?symbol=',Symbol)

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  df <- bind_rows(j)


  return(df)

}



#' Upcoming events in NSE
#'
#' It returns upcoming events in the national stock exchange of India.
#
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' df <- Events()
#' }


Events <- function() {

  URL <-  'https://www.nseindia.com/api/event-calendar'

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  df <- bind_rows(j)


  return(df)

}


# -------------------
# Block Deal
# -------------------

# BlockDeal <- function() {
#
#   URL <-  'https://nseindia.com/api/block-deal'
#
#   j <- tryCatch(NSERequest(URL),
#                 error = function(e) {
#                   message("Trying Again..Wait")
#                   rm(list = ls())
#                   NSERequest(URL)
#                 })
#
#   df <- bind_rows(j)
#
#
#   return(df)
#
# }
#
# df <- BlockDeal()

# -------------------
# Circular
# -------------------

# Circular <- function() {
#
#   URL <-  'https://nseindia.com/api/latest-circular'
#
#   j <- tryCatch(NSERequest(URL),
#                 error = function(e) {
#                   message("Trying Again..Wait")
#                   rm(list = ls())
#                   NSERequest(URL)
#                 })
#
#   df <- bind_rows(j)
#
#
#   return(df)
#
# }
#
# df <- Circular()

#' Top Gainers
#'
#' It returns list of stocks which are top gainers in the NSE.
#
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' df <- TopGainers()
#' }

TopGainers <- function() {

  URL <- "https://www.nseindia.com/api/equity-stockIndices?index=SECURITIES%20IN%20F%26O"

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  df <- j$data

  return(df)

}


#' Top Losers
#'
#' It returns top losers in the national stock exchange of India.
#
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' df <- TopLosers()
#' }


TopLosers <- function() {

  URL <- "https://www.nseindia.com/api/equity-stockIndices?index=SECURITIES%20IN%20F%26O"

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  df <- j$data
  df <- df %>% dplyr::arrange(pChange)

  return(df)

}

#' Most Active Stocks
#'
#' It returns most active stocks in the national stock exchange of India.
#'
#' @param Type "securities", "etf", "sme". By default it is securities
#' @param OrderBy "volume", "value". By default it is volume
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' df <- MostActive(OrderBy = "value")
#' }


MostActive <- function(Type = "securities" , OrderBy = "volume") {

  URL <- paste0("https://www.nseindia.com/api/live-analysis-most-active-",
                Type, "?index=", OrderBy)

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  df <- j$data

  return(df)

}

#' Option Chain
#'
#' It returns option chain.
#'
#' @param Symbol Stock's symbol. You can also indices like BANKNIFTY, NIFTY etc.
#' @param Expiry Expiry. Try either "latest" or "all".
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
# OutDf <- OptionChain('ACC')
# OutDf <- OptionChain('ACC', Expiry = "latest")
#' }

OptionChain <- function(Symbol, Expiry = "all") {

  # NSE Option Chain
  if(trimws(toupper(Symbol)) %in% c('NIFTY', 'BANKNIFTY', 'FINNIFTY')) {
    link <- 'https://www.nseindia.com/api/option-chain-indices?symbol='
  } else {
    link <- 'https://www.nseindia.com/api/option-chain-equities?symbol='
  }

  Symbol <- gsub('&','%26',Symbol)

  URL = paste0(link, trimws(toupper(Symbol)))

  j <- tryCatch(NSERequest(URL),
                 error = function(e) {
                   message("Trying Again..Wait")
                   rm(list = ls())
                   NSERequest(URL)
                 })

  if(Expiry  == "latest"){

    # Latest Expiry
    df <- j$filtered$data

  } else {

    # All Expiry
    df <- j$records$data

  }

  return(df)

}

#' Securities in F&O
#'
#' It returns stocks which are traded in Future and Options.
#'
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' OutDf <- SecuritiesFO()
#' }

SecuritiesFO <- function() {

  link <- 'https://www.nseindia.com/api/equity-stockIndices?index=SECURITIES%20IN%20F%26O'

  URL = paste0(link)

  j <- tryCatch(NSERequest(URL),
                 error = function(e) {
                   message("Trying Again..Wait")
                   rm(list = ls())
                   NSERequest(URL)
                 })


  # Data
  df <- j$data

  return(df)

}


# ---------------------------------
# Option Chain (All F&O Symbols)
# ---------------------------------

# Loop through all the Symbols
# SymbolsList <- df$symbol
# OutDf.All <- data.frame(stringsAsFactors = F)
# for (i in 1:length(SymbolsList)) {
#   OutDf <- OptionChain(SymbolsList[i])
#   OutDf.All <- rbind(OutDf.All,OutDf)
# }

#' Stocks in Nify50
#'
#' It returns information about stocks which are traded in Nifty 50.
#'
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' OutDf <- NIFTY50()
#' }

NIFTY50 <- function() {

  NIFTY50 <- 'https://www.nseindia.com/api/equity-stockIndices?index=NIFTY%2050'

  link <- c(NIFTY50)
  df.all <- data.frame(stringsAsFactors = F)

  for (i in 1:length(link)) {

    URL = paste0(link[i])
    j <- tryCatch(NSERequest(URL),
                  error = function(e) {
                    message("Trying Again..Wait")
                    rm(list = ls())
                    NSERequest(URL)
                  })

    df <- j$data
    df.all <- rbind(df.all, df)

  }

  return(df.all)

}

# Equities <- function() {
#
#   NIFTY50 <- 'https://www.nseindia.com/api/equity-stockIndices?index=NIFTY%2050'
#   NIFTYNEXT50 <- 'https://www.nseindia.com/api/equity-stockIndices?index=NIFTY%20NEXT%2050'
#   MIDSMALLCAP400 <- 'https://www.nseindia.com/api/equity-stockIndices?index=NIFTY%20MIDSMALLCAP%20400'
#
#   link <- c(NIFTY50, NIFTYNEXT50, MIDSMALLCAP400)
#   df.all <- data.frame(stringsAsFactors = F)
#
#   for (i in 1:length(link)) {
#
#   URL = paste0(link[i])
#   j <- tryCatch(NSERequest(URL),
#                 error = function(e) {
#                   message("Trying Again..Wait")
#                   rm(list = ls())
#                   NSERequest(URL)
#                 })
#
#   df <- j$data
#   df.all <- rbind(df.all, df)
#
#   }
#
#   return(df.all)
#
# }
#
# Quotes <- Equities()


#' Indices in NSE
#'
#' It returns information about indices which are traded in NSE.
#'
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' OutDf <- Indices()
#' }

Indices <- function() {

  link <- 'https://www.nseindia.com/api/allIndices'

  URL = paste0(link)

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })


  # Data
  df <- j$data

  return(df)

}


#' Fetch quote of a stock traded in NSE
#'
#' It returns information about a stock traded in NSE.
#'
#' @param Symbol Stock's symbol
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' QuoteDf <- GetQuote('acc')
#' }


GetQuote <- function(Symbol) {

  # NSE Link
  link <- 'https://www.nseindia.com/api/quote-equity?symbol='

  Symbol <- gsub('&','%26',Symbol)

  URL = paste0(link, trimws(toupper(Symbol)))

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })


  # Stocks
    df <-  j$priceInfo
    df0 <- data.frame(do.call(cbind, j$metadata), stringsAsFactors = F)

    # Cleaning
    dfs <- lapply(df[1:11], data.frame, stringsAsFactors = FALSE)
    dfs2 <- suppressMessages(bind_cols(dfs))
    colnames(dfs2) <- names(dfs)
    dfsAll <- cbind(df[12][[1]], dfs2)
    dfsAll <- cbind(df0, dfsAll)


  return(dfsAll)

}


#' Fetch quote of a derivate traded in NSE
#'
#' It returns information about a derivate traded in NSE.
#'
#' @param Symbol Stock's symbol
#' @param Type options or future
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' QuoteDf <- GetDerivativeQuote('banknifty')
#' QuoteDf <- GetDerivativeQuote('banknifty', Type = "future")
#' }

GetDerivativeQuote <- function(Symbol, Type = "options") {

  # NSE Link
  link <- 'https://www.nseindia.com/api/quote-derivative?symbol='

  Symbol <- gsub('&','%26',Symbol)

  URL = paste0(link, trimws(toupper(Symbol)))

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  if(grepl("NIFTY", toupper(Symbol))) {
    helper <- "Index Options"
    helper1 <- "Index Futures"
  } else {
    helper <- "Stock Options"
    helper1 <- "Stock Futures"
  }

  # Stocks
  df01 <- data.frame(do.call(cbind, j$stocks), stringsAsFactors = F) %>%
    {if(tolower(Type) %in% "options") {
          dplyr::filter(., metadata.instrumentType %in% helper)
      }
        else {
          dplyr::filter(., metadata.instrumentType %in% helper1)
      }

    }

  return(df01)

}

#' Fetch historical data of a stock traded in NSE
#'
#' It returns historical information about a stock traded in NSE.
#'
#' @param Symbol Stock's symbol
#' @param StartDate Starting Date (should be in %d-%m-%Y format like 20-07-2021)
#' @param EndDate End Date (should be in %d-%m-%Y format like 24-07-2021)
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' HistoricalDf <- Historical('ACC', "01-01-2020", "24-07-2021")
#' }


Historical <- function(Symbol, StartDate, EndDate) {

  # Link
  link <- "https://www.nseindia.com/api/historical/cm/equity?symbol="

  Symbol <- gsub('&','%26',Symbol)

  StartDate0 <- as.Date(StartDate, tryFormats = c("%d-%m-%Y"))
  EndDate <- as.Date(EndDate, tryFormats = c("%d-%m-%Y"))

  interval <- as.numeric(EndDate - StartDate0)
  Ntimes <- ceiling(interval / 50)
  df <- data.frame(stringsAsFactors = F)

  # Loop
  for (i in 1:Ntimes) {

  StartDate <- max(StartDate0, EndDate - 50)

  URL = paste0(link,
               trimws(toupper(Symbol)),
               "&series=[%22",
               "EQ",
               "%22]&from=",
               format(StartDate, c("%d-%m-%Y")),
               "&to=",
               format(EndDate, c("%d-%m-%Y"))
               )

  j <- tryCatch(NSERequest(URL),
                error = function(e) {
                  message("Trying Again..Wait")
                  rm(list = ls())
                  NSERequest(URL)
                })

  EndDate <- StartDate

  df0 <- j$data
  df <- bind_rows(df, df0)


  }



  return(df)

}

#' Fetch historical data of derivate traded in NSE
#'
#' It returns historical information about a derivative traded in NSE.
#'
#' @param Symbol Stock's symbol
#' @param StartDate Starting Date (should be in %d-%m-%Y format like 20-07-2021)
#' @param EndDate End Date (should be in %d-%m-%Y format like 24-07-2021)
#' @param InstrumentType "options" or "futures"
#' @param ExpiryDate Expiry Date (should be in %d-%m-%Y format like 29-07-2021)
#' @param StrikePrice Strike Price
#' @param OptionType Option Type
#'
#' @export
#' @author Deepanshu Bhalla
#' @examples
#' \dontrun{
#' HistoricalDf <- HistoricalDerivate(Symbol = 'ITC',
#' StartDate = "01-07-2021",
#' EndDate = "24-07-2021",
#' InstrumentType = "options",
#' ExpiryDate = "29-07-2021")
#' }

HistoricalDerivate <- function(Symbol,
                               StartDate,
                               EndDate,
                               InstrumentType,
                               ExpiryDate,
                               StrikePrice = "",
                               OptionType = "") {

  # Link
  link <- "https://www.nseindia.com/api/historical/fo/derivatives?"

  InstrumentType <- tolower(InstrumentType)

  if(InstrumentType == "options") {
    if(grepl("NIFTY", toupper(Symbol))) {
      InstrumentType = "OPTIDX"
    } else {
      InstrumentType = "OPTSTK"
    }
  }

  if(InstrumentType == "futures") {
    if(grepl("NIFTY", toupper(Symbol))) {
        InstrumentType = "FUTIDX"
      } else {
        InstrumentType = "FUTSTK"
    }
  }

  if((InstrumentType %in% c("OPTIDX" , "OPTSTK")) & (nchar(StrikePrice)>0)) {
    StrikePrice = sprintf("%.2f", StrikePrice)
  }

  Symbol <- gsub('&','%26',Symbol)
  ExpiryDate <- format(as.Date(ExpiryDate, c("%d-%m-%Y")), c("%d-%b-%Y"))

  extraURL <-   paste0("&optionType=",
                  OptionType,
                  "&strikePrice=",
                  StrikePrice,
                  "&expiryDate=",
                  ExpiryDate,
                  "&instrumentType=",
                  InstrumentType,
                  "&symbol=",
                  Symbol)

  StartDate0 <- as.Date(StartDate, tryFormats = c("%d-%m-%Y"))
  EndDate <- as.Date(EndDate, tryFormats = c("%d-%m-%Y"))

  interval <- as.numeric(EndDate - StartDate0)
  Ntimes <- ceiling(interval / 50)
  df <- data.frame(stringsAsFactors = F)

  # Loop
  for (i in 1:Ntimes) {

    StartDate <- max(StartDate0, EndDate - 50)

    URL = paste0(link,
                 "&from=",
                 format(StartDate, c("%d-%m-%Y")),
                 "&to=",
                 format(EndDate, c("%d-%m-%Y")),
                 extraURL
    )

    j <- tryCatch(NSERequest(URL),
                  error = function(e) {
                    message("Trying Again..Wait")
                    rm(list = ls())
                    NSERequest(URL)
                  })

    EndDate <- StartDate

    df0 <- j$data
    df <- bind_rows(df, df0)


  }



  return(df)

}


# -----------------
# IntraDay
# -----------------
# IntraDay <- function(Symbol) {
#
#   Symbol <- toupper(gsub('&','%26',Symbol))
#
#   link <- 'https://www.nseindia.com/api/chart-databyindex?index='
#   URL = paste0(link, Symbol, "EQN")
#
#   j <- tryCatch(NSERequest(URL),
#                 error = function(e) {
#                   message("Trying Again..Wait")
#                   rm(list = ls())
#                   NSERequest(URL)
#                 })
#
#
#   # Data
#   options(scipen = 999)
#   df <- data.frame(j$grapthData) %>% setNames(c("DateTime", "Price"))
#   df <- df %>%
#     mutate(DateTime = as.POSIXct(DateTime/1000, origin = "1970-01-01", tz = "GMT"),
#            Symbol = Symbol) %>%
#     select(Symbol, everything())
#
#   return(df)
#
# }
#
# df <- IntraDay('RELIANCE')
