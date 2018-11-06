#install.packages("pageviews")
library(simts)
library(pageviews)

wiki_mobile = project_pageviews(granularity = "daily", start = "2017040100", end = "2018110500",platform = "mobile-app")
Xt_mobile=gts(wiki_mobile$views)
plot(Xt_mobile)

wiki_desk = project_pageviews(granularity = "daily", start = "2017040100", end = "2018110500",platform = "desktop")
Xt_desk=gts(wiki_desk$views)
plot(Xt_desk)

wiki_Sil = article_pageviews(article = 'Silvio_Berlusconi', start = "2018090100", end = "2018110400")
Xt_Sil=gts(wiki_Sil$views)
plot(Xt_Sil)

wiki_Bey = article_pageviews(article = 'Beyonce', start = "2018090100", end = "2018110500")
Xt_Bey=gts(wiki_Bey$views)
plot(Xt_Bey)

wiki_Noam = article_pageviews(article = 'Noam_Chomsky', start = "2018090100", end = "2018110500")
Xt_Noam=gts(wiki_Noam$views)
plot(Xt_Noam)

wiki_lazio = article_pageviews(article = 'SS_Lazio', start = "2018090100", end = "2018110500")
Xt_lazio=gts(wiki_lazio$views)
plot(Xt_lazio)

wiki_Thanks = article_pageviews(article = 'Thanksgiving', start = "2017090100", end = "2017123000")
Xt_Thanks=gts(wiki_Thanks$views)
plot(Xt_Thanks)



plot(auto_corr(Xt_mobile), main = 'mobile')
plot(auto_corr(Xt_mobile, pacf = TRUE), main = 'mobile')

plot(auto_corr(Xt_desk),main = 'desk')
plot(auto_corr(Xt_desk,pacf = TRUE),main = 'desk')

plot(auto_corr(Xt_Sil), main = 'Sil')
plot(auto_corr(Xt_Sil,pacf = TRUE), main = 'Sil')

plot(auto_corr(Xt_Bey), main = 'Bey')
plot(auto_corr(Xt_Bey,pacf = TRUE), main = 'Bey')

plot(auto_corr(Xt_Noam), main = 'Noam')
plot(auto_corr(Xt_Noam,pacf = TRUE), main = 'Noam')

plot(auto_corr(Xt_lazio), main = 'lazio')
plot(auto_corr(Xt_lazio,pacf = TRUE), main = 'lazio')

plot(auto_corr(Xt_Thanks), main = 'Thanks')
plot(auto_corr(Xt_Thanks,pacf = TRUE), main = 'Thanks')


mod_mobile = estimate(SARIMA(ar = 4, i = 0, ma = 0, sar = 1, si = 0, sma = 0, s = 7), Xt_mobile,
                      method = "rgmwm")
check(mod_mobile)
pred_mobile <- predict(mod_mobile, n.ahead = 10, level = 0.95)
point_mobile <- pred_mobile$pred
forecast_mobile <- gts(c(Xt_mobile, point_mobile))
plot(forecast_mobile)

#!!!
mod_desk = estimate(SARIMA(ar = 4, i = 0, ma = 0, sar = 1, si = 0, sma = 0, s = 7), Xt_desk,
                    method = "rgmwm")
check(mod_desk)
pred_desk <- predict(mod_desk, n.ahead = 10, level = 0.95)
point_desk <- pred_desk$pred
forecast_desk <- gts(c(Xt_desk, point_desk))
plot(forecast_desk)

select(AR(8), Xt_Sil, include.mean = TRUE, criterion = "aic", plot = TRUE)
mod_Sil = estimate(AR(1),Xt_Sil)
check(mod_Sil)
pred_sil <- predict(mod_Sil, n.ahead = 1, level = 0.95)
point_sil <- pred_sil$pred
forecast_sil <- gts(c(Xt_Sil, point_sil))
plot(forecast_sil)

select(AR(8), Xt_Bey, include.mean = TRUE, criterion = "aic", plot = TRUE)
mod_Bey = estimate(AR(1),Xt_Bey)
check(mod_Bey)
pred_bey <- predict(mod_Bey, n.ahead = 1, level = 0.95)
point_bey <- pred_bey$pred
forecast_bey <- gts(c(Xt_Bey, point_bey))
plot(forecast_bey)

select(AR(5), Xt_Noam, include.mean = TRUE, criterion = "aic", plot = TRUE)
mod_Noam = estimate(AR(1),Xt_Noam)
check(mod_Noam)
pred_noam <- predict(mod_Noam, n.ahead = 1, level = 0.95)
point_noam <- pred_noam$pred
forecast_noam <- gts(c(Xt_Noam, point_noam))
plot(forecast_noam)

select(AR(3), Xt_lazio, include.mean = TRUE, criterion = "aic", plot = TRUE)
mod_lazio_1 = estimate(AR(0.1),Xt_lazio)
check(mod_lazio_1)
pred_lazio <- predict(mod_lazio_1, n.ahead = 1, level = 0.95)
point_lazio <- pred_lazio$pred
forecast_lazio <- gts(c(Xt_lazio, point_lazio))
plot(forecast_lazio)
pred_lazio$CI0.95
pred_lazio$pred

#!!!!!
select(AR(4), Xt_Thanks, include.mean = TRUE, criterion = "aic", plot = TRUE)
mod_Thanks = estimate(AR(1),Xt_Thanks)
check(mod_Thanks)
pred_Thanks <- predict(mod_Thanks, n.ahead = 1, level = 0.95)
point_Thanks <- pred_Thanks$pred
forecast_Thanks <- gts(c(Xt_Thanks, point_Thanks))
plot(forecast_Thanks)
pred_Thanks$CI0.95
pred_Thanks$pred