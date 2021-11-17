Weather analysis models
================
Arkadiusz Drąg
6/7/2021

``` r
str(data)
```

    ## 'data.frame':    599 obs. of  22 variables:
    ##  $ id                    : int  0 1 2 6 8 9 10 12 14 18 ...
    ##  $ DWD_ID                : int  1 3 44 71 73 78 91 98 116 132 ...
    ##  $ STATION.NAME          : chr  "Aach" "Aachen" "Gro\xdfenkneten" "Albstadt-Badkap" ...
    ##  $ FEDERAL.STATE         : chr  "Baden-W\xfcrttemberg" "Nordrhein-Westfalen" "Niedersachsen" "Baden-W\xfcrttemberg" ...
    ##  $ LAT                   : num  47.8 50.8 52.9 48.2 48.6 ...
    ##  $ LON                   : num  8.85 6.09 8.24 8.98 13.05 ...
    ##  $ ALTITUDE              : num  478 202 44 759 340 65 300 780 213 750 ...
    ##  $ PERIOD                : chr  "1931-1986" "1851-2011" "1971-2016" "1986-2016" ...
    ##  $ RECORD.LENGTH         : int  55 160 45 30 64 55 38 67 67 33 ...
    ##  $ MEAN.ANNUAL.AIR.TEMP  : num  8.2 9.8 9.2 7.4 8.4 9.3 8.2 5.1 8.4 5.7 ...
    ##  $ MEAN.MONTHLY.MAX.TEMP : num  13.1 13.6 13.2 12.2 13.4 13.4 12.7 8.9 12.9 9.2 ...
    ##  $ MEAN.MONTHLY.MIN.TEMP : num  3.5 6.3 5.4 3.3 3.9 5.2 4.1 2.2 4.2 2.7 ...
    ##  $ MEAN.ANNUAL.WIND.SPEED: num  2 3 2 2 1 2 3 3 2 3 ...
    ##  $ MEAN.CLOUD.COVER      : num  67 67 67 66 65 67 72 72 66 64 ...
    ##  $ MEAN.ANNUAL.SUNSHINE  : num  NA 1531 1459 1725 1595 ...
    ##  $ MEAN.ANNUAL.RAINFALL  : num  755 820 759 919 790 794 657 NA NA 915 ...
    ##  $ MAX.MONTHLY.WIND.SPEED: num  2 3 3 2 2 2 3 4 3 3 ...
    ##  $ MAX.AIR.TEMP          : num  32.5 32.3 32.4 30.2 33 32.2 31.6 27.6 33.2 29 ...
    ##  $ MAX.WIND.SPEED        : num  NA 30.2 29.9 NA NA NA NA NA NA NA ...
    ##  $ MAX.RAINFALL          : num  39 36 32 43 43 33 37 NA NA 40 ...
    ##  $ MIN.AIR.TEMP          : num  -16.3 -10.9 -12.6 -15.5 -19.2 -13.3 -15.2 -15.7 -17.5 -17.2 ...
    ##  $ MEAN.RANGE.AIR.TEMP   : num  9.6 7.3 7.8 8.9 9.5 8.2 8.6 6.7 8.6 6.5 ...

``` r
data <- within(data, rm('id', 'DWD_ID', 'PERIOD', 'STATION.NAME', 'FEDERAL.STATE'))
head(data)
```

    ##       LAT     LON ALTITUDE RECORD.LENGTH MEAN.ANNUAL.AIR.TEMP
    ## 1 47.8413  8.8493      478            55                  8.2
    ## 2 50.7827  6.0941      202           160                  9.8
    ## 3 52.9335  8.2370       44            45                  9.2
    ## 4 48.2156  8.9784      759            30                  7.4
    ## 5 48.6159 13.0506      340            64                  8.4
    ## 6 52.4853  7.9126       65            55                  9.3
    ##   MEAN.MONTHLY.MAX.TEMP MEAN.MONTHLY.MIN.TEMP MEAN.ANNUAL.WIND.SPEED
    ## 1                  13.1                   3.5                      2
    ## 2                  13.6                   6.3                      3
    ## 3                  13.2                   5.4                      2
    ## 4                  12.2                   3.3                      2
    ## 5                  13.4                   3.9                      1
    ## 6                  13.4                   5.2                      2
    ##   MEAN.CLOUD.COVER MEAN.ANNUAL.SUNSHINE MEAN.ANNUAL.RAINFALL
    ## 1               67                   NA                  755
    ## 2               67                 1531                  820
    ## 3               67                 1459                  759
    ## 4               66                 1725                  919
    ## 5               65                 1595                  790
    ## 6               67                   NA                  794
    ##   MAX.MONTHLY.WIND.SPEED MAX.AIR.TEMP MAX.WIND.SPEED MAX.RAINFALL MIN.AIR.TEMP
    ## 1                      2         32.5             NA           39        -16.3
    ## 2                      3         32.3           30.2           36        -10.9
    ## 3                      3         32.4           29.9           32        -12.6
    ## 4                      2         30.2             NA           43        -15.5
    ## 5                      2         33.0             NA           43        -19.2
    ## 6                      2         32.2             NA           33        -13.3
    ##   MEAN.RANGE.AIR.TEMP
    ## 1                 9.6
    ## 2                 7.3
    ## 3                 7.8
    ## 4                 8.9
    ## 5                 9.5
    ## 6                 8.2

``` r
data <- data[complete.cases(data),]
```

``` r
dim(data)
```

    ## [1] 204  17

``` r
library(caTools)

Y = data[,1]
msk <- sample.split(Y, SplitRatio=0.8)
```

``` r
X_train <- data[msk,]
X_test <- data[!msk,]


y_train <- data.frame(X_train[,'MEAN.ANNUAL.RAINFALL'])
y_test <- X_test[,'MEAN.ANNUAL.RAINFALL']
```

``` r
library(GGally)
```

    ## Loading required package: ggplot2

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
ggpairs(X_train)
```

![](MS_trenowanie_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
correlation <- cor(X_train)
```

``` r
library(corrplot)
```

    ## corrplot 0.91 loaded

``` r
corrplot(correlation, method = 'number', type = 'lower')
```

![](MS_trenowanie_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
high_cor<-X_train[abs(correlation['MEAN.ANNUAL.RAINFALL',])>=0.5]
df<-as.data.frame(high_cor)
```

``` r
ggpairs(df)
```

![](MS_trenowanie_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
model_rainfall <- lm(MEAN.ANNUAL.RAINFALL~1, data = X_train)
summary(model_rainfall)
```

    ## 
    ## Call:
    ## lm(formula = MEAN.ANNUAL.RAINFALL ~ 1, data = X_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -293.98 -136.98  -45.98   61.02 1034.02 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   739.98      15.88   46.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 202.8 on 162 degrees of freedom

``` r
mean(X_train$MEAN.ANNUAL.RAINFALL)
```

    ## [1] 739.9755

``` r
RMSE <- sqrt(mean(model_rainfall$residuals^2))

RMSE
```

    ## [1] 202.1741

``` r
predicted <- predict(model_rainfall, X_test)
RMSE_predicted <- sqrt(mean((predicted-X_test$MEAN.ANNUAL.RAINFALL)^2))

RMSE_predicted
```

    ## [1] 323.6655

``` r
par(mfrow=c(1,2))
plot(X_train$MEAN.ANNUAL.RAINFALL)
abline(model_rainfall)
plot(X_test$MEAN.ANNUAL.RAINFALL)
abline(model_rainfall)
```

![](MS_trenowanie_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
cor(X_train, method = "pearson")
```

    ##                                LAT          LON    ALTITUDE RECORD.LENGTH
    ## LAT                     1.00000000  0.123931787 -0.67642020  -0.023320031
    ## LON                     0.12393179  1.000000000  0.05727692   0.048645394
    ## ALTITUDE               -0.67642020  0.057276924  1.00000000  -0.027211714
    ## RECORD.LENGTH          -0.02332003  0.048645394 -0.02721171   1.000000000
    ## MEAN.ANNUAL.AIR.TEMP    0.18799971 -0.267807248 -0.81407817  -0.016081022
    ## MEAN.MONTHLY.MAX.TEMP  -0.03586960 -0.179603188 -0.68668134   0.028483077
    ## MEAN.MONTHLY.MIN.TEMP   0.36881036 -0.313761878 -0.79248912  -0.036334355
    ## MEAN.ANNUAL.WIND.SPEED  0.34399558  0.012414214  0.14624784  -0.043350768
    ## MEAN.CLOUD.COVER       -0.05734706 -0.056232897  0.26962130  -0.050165291
    ## MEAN.ANNUAL.SUNSHINE   -0.16603740  0.024686231  0.03538207   0.096168045
    ## MEAN.ANNUAL.RAINFALL   -0.44919559 -0.271603060  0.75216106  -0.108353813
    ## MAX.MONTHLY.WIND.SPEED  0.32800219 -0.028966400  0.17297300  -0.005867135
    ## MAX.AIR.TEMP           -0.14014795  0.031757843 -0.52655918   0.076541258
    ## MAX.WIND.SPEED          0.17034894 -0.008759438  0.19966292  -0.020269828
    ## MAX.RAINFALL           -0.59341890  0.093544495  0.80820937  -0.098342869
    ## MIN.AIR.TEMP            0.41281644 -0.534589543 -0.46120442  -0.077813176
    ## MEAN.RANGE.AIR.TEMP    -0.44506521  0.080103077 -0.12336116   0.088945907
    ##                        MEAN.ANNUAL.AIR.TEMP MEAN.MONTHLY.MAX.TEMP
    ## LAT                              0.18799971           -0.03586960
    ## LON                             -0.26780725           -0.17960319
    ## ALTITUDE                        -0.81407817           -0.68668134
    ## RECORD.LENGTH                   -0.01608102            0.02848308
    ## MEAN.ANNUAL.AIR.TEMP             1.00000000            0.93136166
    ## MEAN.MONTHLY.MAX.TEMP            0.93136166            1.00000000
    ## MEAN.MONTHLY.MIN.TEMP            0.90045754            0.70504833
    ## MEAN.ANNUAL.WIND.SPEED          -0.31693182           -0.54836949
    ## MEAN.CLOUD.COVER                -0.33121728           -0.31574107
    ## MEAN.ANNUAL.SUNSHINE             0.09533693            0.08737811
    ## MEAN.ANNUAL.RAINFALL            -0.61666748           -0.58235989
    ## MAX.MONTHLY.WIND.SPEED          -0.35315648           -0.57091499
    ## MAX.AIR.TEMP                     0.75892471            0.90997258
    ## MAX.WIND.SPEED                  -0.27836269           -0.43670086
    ## MAX.RAINFALL                    -0.60131061           -0.50073885
    ## MIN.AIR.TEMP                     0.47853233            0.18668462
    ## MEAN.RANGE.AIR.TEMP              0.35431722            0.65581130
    ##                        MEAN.MONTHLY.MIN.TEMP MEAN.ANNUAL.WIND.SPEED
    ## LAT                              0.368810358            0.343995584
    ## LON                             -0.313761878            0.012414214
    ## ALTITUDE                        -0.792489121            0.146247836
    ## RECORD.LENGTH                   -0.036334355           -0.043350768
    ## MEAN.ANNUAL.AIR.TEMP             0.900457538           -0.316931817
    ## MEAN.MONTHLY.MAX.TEMP            0.705048332           -0.548369486
    ## MEAN.MONTHLY.MIN.TEMP            1.000000000            0.003950164
    ## MEAN.ANNUAL.WIND.SPEED           0.003950164            1.000000000
    ## MEAN.CLOUD.COVER                -0.316580724            0.034941990
    ## MEAN.ANNUAL.SUNSHINE             0.120831798            0.116306916
    ## MEAN.ANNUAL.RAINFALL            -0.522849056            0.171188425
    ## MAX.MONTHLY.WIND.SPEED          -0.048866376            0.776936510
    ## MAX.AIR.TEMP                     0.464496136           -0.598874534
    ## MAX.WIND.SPEED                  -0.064069720            0.604780660
    ## MAX.RAINFALL                    -0.582299760            0.072433222
    ## MIN.AIR.TEMP                     0.766250147            0.375655217
    ## MEAN.RANGE.AIR.TEMP             -0.069102922           -0.777990169
    ##                        MEAN.CLOUD.COVER MEAN.ANNUAL.SUNSHINE
    ## LAT                         -0.05734706         -0.166037405
    ## LON                         -0.05623290          0.024686231
    ## ALTITUDE                     0.26962130          0.035382068
    ## RECORD.LENGTH               -0.05016529          0.096168045
    ## MEAN.ANNUAL.AIR.TEMP        -0.33121728          0.095336933
    ## MEAN.MONTHLY.MAX.TEMP       -0.31574107          0.087378105
    ## MEAN.MONTHLY.MIN.TEMP       -0.31658072          0.120831798
    ## MEAN.ANNUAL.WIND.SPEED       0.03494199          0.116306916
    ## MEAN.CLOUD.COVER             1.00000000         -0.494028570
    ## MEAN.ANNUAL.SUNSHINE        -0.49402857          1.000000000
    ## MEAN.ANNUAL.RAINFALL         0.24743395          0.022085199
    ## MAX.MONTHLY.WIND.SPEED       0.04982599          0.091898143
    ## MAX.AIR.TEMP                -0.19363593          0.035570043
    ## MAX.WIND.SPEED               0.07587166          0.181892790
    ## MAX.RAINFALL                 0.20039453          0.042276619
    ## MIN.AIR.TEMP                -0.12913661          0.062837635
    ## MEAN.RANGE.AIR.TEMP         -0.10290174         -0.007150812
    ##                        MEAN.ANNUAL.RAINFALL MAX.MONTHLY.WIND.SPEED MAX.AIR.TEMP
    ## LAT                              -0.4491956            0.328002191  -0.14014795
    ## LON                              -0.2716031           -0.028966400   0.03175784
    ## ALTITUDE                          0.7521611            0.172972995  -0.52655918
    ## RECORD.LENGTH                    -0.1083538           -0.005867135   0.07654126
    ## MEAN.ANNUAL.AIR.TEMP             -0.6166675           -0.353156477   0.75892471
    ## MEAN.MONTHLY.MAX.TEMP            -0.5823599           -0.570914994   0.90997258
    ## MEAN.MONTHLY.MIN.TEMP            -0.5228491           -0.048866376   0.46449614
    ## MEAN.ANNUAL.WIND.SPEED            0.1711884            0.776936510  -0.59887453
    ## MEAN.CLOUD.COVER                  0.2474340            0.049825991  -0.19363593
    ## MEAN.ANNUAL.SUNSHINE              0.0220852            0.091898143   0.03557004
    ## MEAN.ANNUAL.RAINFALL              1.0000000            0.172305355  -0.58966959
    ## MAX.MONTHLY.WIND.SPEED            0.1723054            1.000000000  -0.61747521
    ## MAX.AIR.TEMP                     -0.5896696           -0.617475214   1.00000000
    ## MAX.WIND.SPEED                    0.1883078            0.725762977  -0.44336720
    ## MAX.RAINFALL                      0.8300019            0.063402569  -0.41699067
    ## MIN.AIR.TEMP                     -0.1087321            0.314871708  -0.09257413
    ## MEAN.RANGE.AIR.TEMP              -0.2649710           -0.752834784   0.78539250
    ##                        MAX.WIND.SPEED MAX.RAINFALL MIN.AIR.TEMP
    ## LAT                       0.170348941  -0.59341890   0.41281644
    ## LON                      -0.008759438   0.09354450  -0.53458954
    ## ALTITUDE                  0.199662922   0.80820937  -0.46120442
    ## RECORD.LENGTH            -0.020269828  -0.09834287  -0.07781318
    ## MEAN.ANNUAL.AIR.TEMP     -0.278362688  -0.60131061   0.47853233
    ## MEAN.MONTHLY.MAX.TEMP    -0.436700856  -0.50073885   0.18668462
    ## MEAN.MONTHLY.MIN.TEMP    -0.064069720  -0.58229976   0.76625015
    ## MEAN.ANNUAL.WIND.SPEED    0.604780660   0.07243322   0.37565522
    ## MEAN.CLOUD.COVER          0.075871662   0.20039453  -0.12913661
    ## MEAN.ANNUAL.SUNSHINE      0.181892790   0.04227662   0.06283763
    ## MEAN.ANNUAL.RAINFALL      0.188307838   0.83000185  -0.10873205
    ## MAX.MONTHLY.WIND.SPEED    0.725762977   0.06340257   0.31487171
    ## MAX.AIR.TEMP             -0.443367196  -0.41699067  -0.09257413
    ## MAX.WIND.SPEED            1.000000000   0.13083460   0.18459232
    ## MAX.RAINFALL              0.130834605   1.00000000  -0.35194387
    ## MIN.AIR.TEMP              0.184592324  -0.35194387   1.00000000
    ## MEAN.RANGE.AIR.TEMP      -0.547451449  -0.08479331  -0.55233067
    ##                        MEAN.RANGE.AIR.TEMP
    ## LAT                           -0.445065210
    ## LON                            0.080103077
    ## ALTITUDE                      -0.123361158
    ## RECORD.LENGTH                  0.088945907
    ## MEAN.ANNUAL.AIR.TEMP           0.354317225
    ## MEAN.MONTHLY.MAX.TEMP          0.655811301
    ## MEAN.MONTHLY.MIN.TEMP         -0.069102922
    ## MEAN.ANNUAL.WIND.SPEED        -0.777990169
    ## MEAN.CLOUD.COVER              -0.102901743
    ## MEAN.ANNUAL.SUNSHINE          -0.007150812
    ## MEAN.ANNUAL.RAINFALL          -0.264971043
    ## MAX.MONTHLY.WIND.SPEED        -0.752834784
    ## MAX.AIR.TEMP                   0.785392501
    ## MAX.WIND.SPEED                -0.547451449
    ## MAX.RAINFALL                  -0.084793314
    ## MIN.AIR.TEMP                  -0.552330666
    ## MEAN.RANGE.AIR.TEMP            1.000000000

``` r
model_rainfall2 <- lm(MEAN.ANNUAL.RAINFALL ~ ALTITUDE, data = X_train)
RMSE2 <- sqrt(mean(model_rainfall2$residuals^2))

RMSE2
```

    ## [1] 133.2285

``` r
predicted2 <- predict(model_rainfall2, X_test)
RMSE_predicted2 <- sqrt(mean((predicted2-X_test$MEAN.ANNUAL.RAINFALL)^2))

RMSE_predicted2
```

    ## [1] 214.7028

``` r
par(mfrow=c(1,2))
plot(X_train$MEAN.ANNUAL.RAINFALL)
abline(model_rainfall2)
plot(X_test$MEAN.ANNUAL.RAINFALL)
abline(model_rainfall2)
```

![](MS_trenowanie_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
model_rainfall3 <- lm(MEAN.ANNUAL.RAINFALL ~ MAX.RAINFALL, data = X_train)
RMSE3 <- sqrt(mean(model_rainfall3$residuals^2))

RMSE3
```

    ## [1] 112.7647

``` r
predicted3 <- predict(model_rainfall3, X_test)
RMSE_predicted3 <- sqrt(mean((predicted3-X_test$MEAN.ANNUAL.RAINFALL))^2)

RMSE_predicted3
```

    ## [1] 0.5328149

?

``` r
par(mfrow=c(1,2))
plot(X_train$MEAN.ANNUAL.RAINFALL)
abline(model_rainfall3)
plot(X_test$MEAN.ANNUAL.RAINFALL)
abline(model_rainfall3)
```

![](MS_trenowanie_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
model_rainfall4 <- lm(MEAN.ANNUAL.RAINFALL ~ MAX.RAINFALL + ALTITUDE, data = X_train)
RMSE4 <- sqrt(mean(model_rainfall4$residuals^2))
RMSE4
```

    ## [1] 109.2519

``` r
predicted4 <- predict(model_rainfall4, X_test)
RMSE_predicted4 <- sqrt(mean(predicted4-X_test$MEAN.ANNUAL.RAINFALL))
```

    ## Warning in sqrt(mean(predicted4 - X_test$MEAN.ANNUAL.RAINFALL)): wyprodukowano
    ## wartości NaN

``` r
RMSE_predicted4
```

    ## [1] NaN
