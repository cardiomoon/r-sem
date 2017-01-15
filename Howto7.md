---
title: "Multiple Groups"
author: "Keon-Woong Moon"
output: html_document
---

In this chapter, I will show you how to perform multiple group analysis. 

## Select Data and Edit Structural Equation 

Please select the  `Confirmatory Factor Analysis` among the `Select Example` selectInput. This selection will set the data to `HolzingerSwineford1939`(arrow) and make the structural equation as follows(arrow).

```
visual  =~ x1 + x2 + x3 
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
```

![41.png](fig/41.png)

## Select `group` option

Among the Analysis/Summary Options, please select `school` as a group variable(1). Press `do Analysis` button(arrow).

![42.png](fig/42.png)


## Results of Analysis(1)

You can get the results of analysis followed by results for group 1 and group 2 seperately.

![43.png](fig/43.png)

## Results of Analysis(2)

![44.png](fig/44.png)

## Results of Analysis(3)

![45.png](fig/45.png)

## Plots for Multiple Groups

You can also get the resultant plots separately(In this example, you will be able to get 2 plots).

![46s.png](fig/46s.png)
![47s.png](fig/47s.png)

## Measurement Invariance

If you are interested in testing the measurement invariance of a CFA model across several groups, select the measurementInvariance checkbox and press the `do Analysis` button. 

![48.png](fig/48.png)

You can get the results of measurementInvariance function of semTools package. 

![49.png](fig/49.png)
![50.png](fig/50.png)

