---
title: "Structural Equation Modeling with R"
author: "Keon-Woong Moon"
output: html_document
---


# Where to test

http://r-meta.org:3838/r-sem


# Getting Started

Welcome to the "Structural Equation Modeling(SEM) with R". In this chapter, you can perform the first `structural equation modeling` of your own using the sample data in a minute. 

## Perform SEM In A Minute

### The first screen : Select language 

When you load the SEM app, you can see this screen. Currently, this app support English and Korean. You can select your preferred language(arrow).

![1.png](fig/1.png)


### Steps for SEM

Performing SEM with sample data is very simple. You can finish in five steps. 

#### 1. Select/Upload Data 

... `HolzingerSwineford1939` was selected as a default data.

If you want to know about the data, press the `show help Data` button.

![2.png](fig/2.png)

In the popup window, the informations about the data will be displayed.

![3.png](fig/3.png)

#### 2. Edit Data
... No action required.

#### 3. Insert/Edit Structural Equation  

The classic Holzinger and Swineford (1939) dataset consists of mental ability test scores of seventh- and eighth-grade children from two different schools (Pasteur and Grant-White). In the original dataset (available in the MBESS package), there are scores for 26 tests. However, a smaller subset with 9 variables is more widely used in the literature. A Confirmatory Factor Analysis(CFA) model that is often proposed for these 9 variables consists of three latent variables (or factors), each with three indicators:

* a `visual` factor measured by 3 variables: x1, x2 and x3
* a `textual` factor measured by 3 variables: x4, x5 and x6
* a `speed` factor measured by 3 variables: x7, x8 and x9

You can insert/edit the structural equation easily by the following steps. 

1. Choose left side variable(s) or enter the latent variable 
2. Select operator
3. Choose right side variable(s)

(1) First, please enter the latent variable `visual` in the text input(arrow). 

![4.png](fig/4.png)

As you enter the latent variable `visual`, the left side of equation will be made and the operator `=~` will be added(arrow). The operator `=~` is used for latent variable definition which means `is measured by`. 

![5.png](fig/5.png)

The current set of operators is summarized in the table below.

formula type              | operator | mnemonic
--------------------------|----------|------------
latent variable selection | `=~`     | is measured by
regression                | `~`      | is regressed on
(residual) (co)variance   | `~~`     | is correlated with
intercept                 | `~1`     | intercept
defined parameter         | `:=`     | is predefined as
equality constriant       | `==`     | equals
non-equalty constraint    | `<`      | is less than
non-equalty constraint    | `>`      | is greater than


(2) And then, please select the rigth side variables - `x1`,`x2` and `x3` - among the variables displayed in the selectInput.

![6.png](fig/6.png)

(3) After selection of three variables, press the `add to equation` button.

![7.png](fig/7.png)

The temporary equation just made will be added to the equation. The latent variable input box(1), temporary equation(2) and the selectInput(for rigth side variables) will be initialized.

![8.png](fig/8.png)

Please repeat the step (1)-(3) to add the equation:

`textual =~ x4 + x5 + x6`

`speed =~ x7 + x8 + x9`

#### 4. Analysis/Summary options 
... No action required.

#### 5. Plot options 

... No action required.

#### 6. Press the `Do anlaysis` button(arow).

![9.png](fig/9.png)

You can get the analysis results followed by the plot(s) representing SEM.

![10.png](fig/10.png)
![11.png](fig/11.png)

### Easier step

You can bypass all the steps by `select Example`. If you choose the example `Confirmatory Factor Anlaysis`, the equation will be made automatically. And then, you can press the `do Anlaysis` button.

![12.png](fig/12.png)

### Download Results

You can download Report(1) as PDF(\*.pdf), HTML(\*.html,default) format. You can download plots(2) with png, svg or pdf format. You can download plot(s) as a powerpoint file(\*.pptx)(3).

![13.png](fig/13.png)


