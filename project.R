#install.packages("pageviews")
library(simts)
library(pageviews)

wiki_mobile = project_pageviews(granularity = "daily", start = "2017090100", end = "2018110400",platform = "mobile-app")
Xt_mobile=gts(wiki_mobile$views)
plot(Xt_mobile)

wiki_desk = project_pageviews(granularity = "daily", start = "2017090100", end = "2018110400",platform = "desktop")
Xt_desk=gts(wiki_desk$views)
plot(Xt_desk)

wiki_Sil = article_pageviews(article = 'Silvio_Berlusconi', start = "2018090100", end = "2018110400")
Xt_Sil=gts(wiki_Sil$views)
plot(Xt_Sil)

wiki_Bey = article_pageviews(article = 'Beyonce', start = "2018090100", end = "2018110400")
Xt_Bey=gts(wiki_Bey$views)
plot(Xt_Bey)

wiki_Noam = article_pageviews(article = 'Noam_Chomsky', start = "2018090100", end = "2018110400")
Xt_Noam=gts(wiki_Noam$views)
plot(Xt_Noam)

wiki_lazio = article_pageviews(article = 'SS_Lazio', start = "2018090100", end = "2018110400")
Xt_lazio=gts(wiki_lazio$views)
plot(Xt_lazio)

wiki_Thanks = article_pageviews(article = 'Thanksgiving', start = "2018090100", end = "2018110400")
Xt_Thanks=gts(wiki_Thanks$views)
plot(Xt_Thanks)



plot(auto_corr(Xt_mobile))
plot(auto_corr(Xt_desk))
plot(auto_corr(Xt_Sil))
plot(auto_corr(Xt_Bey))
plot(auto_corr(Xt_Noam))
plot(auto_corr(Xt_lazio))
plot(auto_corr(Xt_Thanks))


mod_mobile = estimate(SARIMA(ar = 1, ma = 1, i = 1, sar = 1, si = 2, sma = 1, s = 7),Xt_mobile,method = "rgmwm")
check(mod_mobile)

mod_mobile = estimate(ARIMA(2, 2, 2),Xt_mobile,method = "rgmwm")
check(mod_mobile)

mod_mobile = estimate(SARMA(1, 0, 0, 0),Xt_mobile,method = "rgmwm")
check(mod_mobile)

mod_Sil = estimate(AR(8),Xt_Sil)
check(mod_Sil)
pred_sil <- predict(mod_Sil, n.ahead = 5, level = 0.95)
point_sil <- pred_sil$pred
forecast_sil <- gts(c(Xt_Sil, point_sil))
plot(forecast_sil)

mod_Bey = estimate(AR(12),Xt_Bey)
check(mod_Bey)
pred_bey <- predict(mod_Bey, n.ahead = 5, level = 0.95)
point_bey <- pred_bey$pred
forecast_bey <- gts(c(Xt_Bey, point_bey))
plot(forecast_bey)

mod_Noam = estimate(AR(8),Xt_Noam)
check(mod_Noam)
pred_noam <- predict(mod_Noam, n.ahead = 5, level = 0.95)
point_noam <- pred_noam$pred
forecast_noam <- gts(c(Xt_Noam, point_noam))
plot(forecast_noam)

mod_lazio_1 = estimate(AR(10),Xt_lazio)
check(mod_lazio_1)
pred_lazio <- predict(mod_lazio_1, n.ahead = 5, level = 0.95)
point_lazio <- pred_lazio$pred
forecast_lazio <- gts(c(Xt_lazio, point_lazio))
plot(forecast_lazio)
