install.packages("pageviews")
library(simts)
library(pageviews)

wiki_mobile = project_pageviews(granularity = "daily", start = "2017090123", end = "2018102123",platform = "mobile-app")
Xt_mobile=gts(wiki_mobile$views)
plot(Xt_mobile)

wiki_desk = project_pageviews(granularity = "daily", start = "2017090123", end = "2018102123",platform = "desktop")
Xt_desk=gts(wiki_desk$views)
plot(Xt_desk)

wiki_Sil = article_pageviews(article = 'Silvio_Berlusconi', start = "2018090100", end = "2018102100")
Xt_Sil=gts(wiki_Sil$views)
plot(Xt_Sil)

wiki_Bey = article_pageviews(article = 'Beyonce', start = "2018090100", end = "2018102100")
Xt_Bey=gts(wiki_Bey$views)
plot(Xt_Bey)

wiki_Noam = article_pageviews(article = 'Noam_Chomsky', start = "2018090100", end = "2018102100")
Xt_Noam=gts(wiki_Noam$views)
plot(Xt_Noam)

wiki_lazio = article_pageviews(article = 'SS_Lazio', start = "2018090100", end = "2018102100")
Xt_lazio=gts(wiki_lazio$views)
plot(Xt_lazio)

wiki_Thanks = article_pageviews(article = 'Thanksgiving', start = "2018090100", end = "2018102100")
Xt_Thanks=gts(wiki_Thanks$views)
plot(Xt_Thanks)