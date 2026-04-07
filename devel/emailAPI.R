library(httr)
library(rjson)
# SAT PHONE https://blog.pivotel.com/texting-and-sending-sms-to-a-satellite-phone
# MAYBE PYTHON GMAIL https://stackabuse.com/how-to-send-emails-with-gmail-using-python/

# turboSMTP API send email - Free until June 2023 then we can pay for a batch of messages
# https://serversmtp.com/turbo-api/#webhooks
# auth

authURL <- 'https://dashboard.serversmtp.com/api/authorize'
email <- 'tnsakai@gmail.com'

toemail <- '9496379809@msg.fi.google.com'

# tryAuth <- POST(url=authURL, body = list(email=email, password=pw, no_expire=1), encode = 'json')
# 
# authKey <- fromJSON(rawToChar(tryAuth$content))$auth

emURL <- 'https://api.turbo-smtp.com/api/v2/mail/send'
toemail <- c(toemail, email)
headList <- list(authuser = email,
                 authpass = pw)
bodList <- list(from=email,
                to = paste0(toemail, collapse=','),
                content = 'We messagin bro')

tryEmail <- POST(url=emURL,
                 # config=config(add_headers(authuser = email, authpass = pw)),
                 body = c(headList, bodList))

fromJSON(rawToChar(tryEmail$content))
sendTurboEmail <- function(to, message) {
    authEmail <- 'tnsakai@gmail.com'
    
    url <- 'https://api.turbo-smtp.com/api/v2/mail/send'
    headList <- list(authuser = authEmail,
                     authpass = pw)
    
    bodList <- list(from=authEmail,
                    to = paste0(to, collapse=','),
                    content=message)
    tryEmail <- POST(url=url,
                     body = c(headList, bodList))
    response <- fromJSON(rawToChar(tryEmail$content))
}
# sendinblue - UGH IT SENDS A LOGO WHICH TAKES UP THE WHOLE TEXT MESSAGE

library(httr)

url <- "https://api.sendinblue.com/v3/smtp/email"

body <- toJSON(list(sender=c('email'='tnsakai@gmail.com'),
                    to = list(list('email'='9496379809@msg.fi.google.com', name='taiki')),
                    textContent = 'test buoy message',
                    subject = 'test',
                    replyTo = c('email'='tnsakai@gmail.com')))
response <- VERB("POST", url, add_headers('api-key' = apikey), content_type("application/json"), accept("application/json"), 
                 body = body
)
                 

fromJSON(content(response, "text"))

# netcore UGH have to configure sending domain. Also doesn't seem to be working even
# with my verified email

url <- 'https://emailapi.netcorecloud.net/v5/mail/send'
toemail <- 'tnsakai@gmail.com'
toemail <- '9496379809@msg.fi.google.com'
body <- toJSON(list(from=c('email'='tnsakai@pepisandbox.com'),
                    personalizations = list(list('to'=list(list(email=toemail)))),
                    content = list(list(type='html', 
                                        value='test buoy message')),
                    subject = 'test',
                    reply_to = 'tnsakai@gmail.com'))

response <- VERB("POST", url, add_headers('api_key' = apikey), content_type("application/json"), 
                 body = body
)

# what <- POST(url=url, config = add_headers('api_key'=apikey), body=body, encode = 'raw')
# fromJSON(content(what, 'text'))
fromJSON(content(response, "text"))

# sendgrid I had problems creating account

# mailersend req domain
