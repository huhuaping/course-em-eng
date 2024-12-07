---
pagetitle: "Advanced Econometrics III"
unitcode: "(Econometrics III)"
unitname: "Advanced Econometrics III"
author: "Hu Huaping (胡华平)"
email: "huhuaping01 at hotmail.com"
university: "NWAFU (西北农林科技大学)"
department: "School of Economics and Management (经济管理学院)"
subtitle: "Chapter 17. Endogeneity and Instumental Variables"
date: "2024-09-25"
keep-md: true
format: 
  revealjs: 
    footer: "Chapter 17. Endogeneity and Instumental Variables"
---




## 环境准备{visibility="hidden"}









# 标题页 {visibility="hidden"}

## <br>Advanced Econometrics III{background-image="pic/slide-front-page.jpg" #etc5523-title .center-h style="font-size:65px" visibility="uncounted"}

### **(Econometrics III)**{.monash-black .center-h style="font-family:'sans-serif'; font-size:65px !important"}

<br>

### Chapter 17. Endogeneity and Instumental Variables{.monash-blue .center-h style="font-size:70px !important"}

<br>

#### Hu Huaping (胡华平) {.center-h .monash-black}

#### huhuaping01 at hotmail.com {.center-h .monash-black style="font-family:'sans-serif';"}

#### School of Economics and Management (经济管理学院) {.center-h .monash-black}

<br>

#### 2024-09-25 {.center-h .monash-black}



::: {.cell layout-align="center"}

:::




::: {.notes}
Good evening everyone. Welcome to my class.I am teacher Hu Huaping.Here is my email.You can contact me when you have any questions with our course.In this part, we will learn Simultaneous Equation Models together by using almost eight lessons.
:::


# Part 2：Simultaneous Equation Models (SEM){.monash-bg-blue .mcenter visibility="uncounted"}

::: {.font-110}

[Chapter 17. Endogeneity and Instrumental Variables]{.red}

Chapter 18. Why Should We Concern SEM ?

Chapter 19. What is the Identification Problem ?

Chapter 20. How to Estimate SEM ?

:::

::: {.notes}
As you see this part contains four chapters.Firstly, we will go through Chapter 17. Regressor Endogeneity problems and instrumental Variables solutions will be discussed in this chapter. Anyway, this chapter will be a good start for learning SEM.The next three chapters focus closely on SEM. We will answer three important questions in turn.In Chapter 18, we will know SEM is important in social science and it also brings new challenges to us.Large SEM system always contains lots of parameters need to be solved and will face with the identification problems. We will give you guides and rules to check the SEM identification status in Chapter 19.Finally, we will discuss different SEM estimation approaches in Chapter 20, including 2SLS, Three-stage least squares (3SLS) and full information maximum likelihood (FIML)  method.
:::


# Chapter 17. Endogeneity and Instumental Variables{#chapter17 .monash-bg-blue .mcenter}

[17.1 Definition and source of endogeneity](#definition)

[17.2 Estimation problem with endogeneity](#problem)

[17.3 IV and choices](#IV)

[17.4 Two-stage least squares method](#TSLS)

[17.5 Testing instrument validity](#validity)

[17.6 Testing regressor endogeneity](#endogeneity)

::: {.notes}
[source](https://web.sgh.waw.pl/~mrubas/AdvEcon/pdf/T2_Endogeneity.pdf)So let us start the first chapter.In this chapter:- You will see how the method of **instrumental variables** (IV) can be used to solve the problem of **endogeneity** due to one or more regressors.- Also we will learn the method of **two stage least squares** in section 17.4. 2SLS method is second in popularity only to ordinary least squares for estimating linear equations in econometrics.- And some useful testing techniques will be introduced to check instrument validity and regressor endogeneity. These content will be uncovered in the last two sections.
:::


# 17.1 Definition and sources of endogeneity{#definition .monash-bg-blue .mcenter}

::: {.notes}
In this section we will talk about the definition of endogeneity and its main sources.The story begins with the stochastic regressors.
:::


## Review

### the CLRM assumptions

Let's revise the classic linear regression model   assumptions(CLRM):

- **A1**: The true model is$\boldsymbol{y= X \beta+\epsilon}$.

- **A2**:$\forall i, E(\epsilon_i|X) = 0$ (conditional zero mean)  more about this later.

- **A3**:$Var(\epsilon|X) = E(\epsilon \epsilon^{\prime}|X) = \sigma^2I$ (identical conditional variance ).

- **A4**:$\boldsymbol{X}$ has full column rank.

- **A5**: (for inference purposes,$\boldsymbol{\epsilon} \sim  N(\boldsymbol{0}, σ^2\boldsymbol{I}))$.

Under **A1-A4**（also namely **CLRM**）, the OLS is **Gauss-Markov** efficient.

Under **A1-A5**, we donote **N-CLRM**.

::: {.notes}
we have discussed these assumptions before the class. And we denote the Gauss-Markov model assumption system as classical linear regression model (CLRM).- **Assumption 1**: The  model is assumed to be true and be linear with parameters.- **Assumption 2**: the disturbance term has conditional zero expectation. And we will relax this assumption later.- **Assumption 3**: this means the conditional variance of disturcances is identity given X. the other way to express assumption 3 is that the  disturbance term is homoscedasticity['hɔməusi,dæs'tisəti].- **Assumption 4**: The regressor matrix X has full column rank. Which means that none of the regressors is the perfect linear function on the rest of the regressors.- **Assumption 5**: We will bring in the norm distribution assumption on the disburtance. This assumption aims for hypothesis testing purposes.Joint with A2, A3, and A5, the disturbance is assumed to be normally identical independent distribution. We just write i.i.d for shortly.With assumptions **A1-A4**, We note **CLRM**. Under CLRM the OLS is **Gauss-Markov** efficient. Which means under CLRM, when we use OLS method we will obtain the Best-Linear-unbiased estimators on the true parameters. [hand writing]While, with assumptions **A1-A5**, we used to denote as N-CLRM. we can conduct kinds of inference and tests based on N-CLRM.Now let us focus on assumption 2.
:::

##

### The A2 assumptions

For the population regression model(PRM):

$$
\begin{align}
Y_i &=  \beta_1 +\beta_2X_i + u_i && \text{(PRM)}
\end{align}
$$

- **CLRM A2** assumes that X is fixed (given) or independent of the error term. The regressors$X$ **are not** random variables. At this point, we can use the OLS method and get **BLUE**(Best Linear Unbiased Estimator).

$$
\begin{align}
Cov(X_i, u_i)= 0; \quad
E(X_i u_i)= 0
\end{align}
$$

- If the above A2 assumption is violated, the independent variable$X$ is related to the random disturbance term. In this case, OLS estimation will no longer get **BLUE**, and **instrumental variable method** (IV) should be used for estimation.

$$
\begin{align}
Cov(X_i, u_i) \neq 0 ; \quad
E(X_i u_i) \neq 0
\end{align}
$$

::: {.notes}
If A2 is violated, then the endogeneity problem arise.Now we go head to define the endogenous variable.事实上，无论$X_i$与$u_i$是否相关，我们都可以采用**IV法**得到**BLUE**。
:::


## Good model

###  Random experiment

**Randomized controlled experiment **: Ideally, the value of the independent variable$X$ is randomly changed (refer to the **reason**), and then we look at the change in the dependent variable$Y$ (refer to the **result**).

$$
\begin{equation}
\boldsymbol{y= X \beta+u}
\end{equation}
$$

- If$Y_i$ and$X_i$ does exist systematic relationship (linear relationship), then change$X_i$ causes the corresponding change of$Y_i$.

- Any other random factors will be added to the random disturbance$u_i$. The effect of the random disturbance on the change of $Y_i$ should be **independent** to the effect of$X_i$ on the change of$Y_i$.


##

### Exogenous regressors

**Exogenous regressors**: If independent variables$X_i$ is **perfectly random** (randomly assigned) as mentioned above , then they are called **exogenous regressor**. More precisely, they can be defined as:

::: {.callout-note}

**Strictly Exogeneity**：

$$
E\left(u_{i} \mid X_{1}, \ldots, X_{N}\right)=E\left(u_{i} \mid \mathbf{x}\right)=0
$$

:::


::: {.notes}
因为在**随机控制实验**，给定样本$i$和样本$j$，自变量的取值分别为$X_i$和$X_j$，它们应该是相互独立的。因此可以把上述假设进一步简化为：> **同期外生性假设**（contemporaneously exogeneity）：$E(u_i|X_{i})=0, \text{for } i =1, \ldots,N$。**同期**表示的是在同样的时间点上，或所有的截面观测单位在某个时间上获得的观测。大样本情况下OLS方法在大样本情形下，上述**严格外生性假设**可以进一步转换为**同期不相关假设**：- $E(u_i)=0$，而且- $\operatorname{cov}(X_i, u_i)=0$因为我们可以证明（证明略），在大样本情况OLS方法下：- $E(u_i|X_{i})=0 \quad \Rightarrow \quad E(u_i)=0$- $E(u_i|X_{i})=0 \quad \Rightarrow \quad \operatorname{cov}(X_i, u_i)=0$
:::


## Endogeneity

### Definition of endogeneity

We use the term **endogeneity** frequently in econometrics.

Also this concept is used broadly to describe any situation where a regressor is **correlated** with the error term.

- Assume that we have the bivariate linear model

$$
\begin{equation}
Y_{i}=\beta_{0}+\beta_{1} X_{i}+\epsilon_{i}
\end{equation}
$$

- The explanatory variable$X$ is said to be **[En]{.red}dogenous** if it is correlatedwith$\epsilon$.

$$
\begin{align}
Cov(X_i, \epsilon_i) \neq 0 ; \quad
E(X_i \epsilon_i) \neq 0
\end{align}
$$

- And if$X$ is **uncorrelated** with$\epsilon$, it is said to be **[Ex]{.red}ogenous**.

$$
\begin{align}
Cov(X_i, \epsilon_i) = 0 ; \quad
E(X_i \epsilon_i) = 0
\end{align}
$$

::: {.notes}
Next, we should ask what the sources of endogeneity come from.
:::


##

### Sources of endogeneity

In applied econometrics, endogeneity usually arises in one of four ways:

- **Omitted variables**: when the model is set incorrectly.

- **Measurement errors** in the regressors.

- **Autocorrelation** of the error term in autoregressive models.

- **Simultaneity**: when$\boldsymbol{Y}$ and$\boldsymbol{X}$ are simultaneously determined, as in the supply/demand model (we will go to explain it in the next three chapter).


::: {.notes}
So, let us go through all of these four situations quickly.
:::

## Source 1: Omitted variables

### The assumed true model & mis-specified model


Suppose that the **"assumed true model"** for wage determination is:

$$
\begin{align}
Wage_{i}=\beta_{1}+\beta_{2} Edu_{i}+\beta_{3} Abl_{i}+\epsilon_{i}  \quad \text{(the assumed true model)}
\end{align}
$$

However, because the individual's **ability variable**($Abl$) is often not directly observed, so we often can't put it into the model, and build a **mis-specified model**.

$$
\begin{align}
Wage_{i}=\alpha_{1}+\alpha_{2} Edu_{i}+v_{i}  \quad \text{(the mis-specified model)}
\end{align}
$$

- Where **ability variable**($Abl$) is included in the new disturbance$v_i$, and$v_{i}=\beta_{3} abl_{i}+u_{i}$

- Obviously, in the mis-specified model, we ignore the **ability variable**($Abl$), while  variable **years of education**($Edu$) is actually related to it.

- So in the mis-specified  model,$cov(Edu_i, v_i) \neq 0$, thus **years of education**($Edu$) may cause the endogenous problem.

::: {.notes}
why the variables are Omitted?  Because people run a model without considering the necessary  or important variables we should.We will explain later that **Omission** of relevant variable will result in **inconsistent estimates** because **A2** is not hold.Now, let us talk about the second source of endogeneity. the Measurement error in the regressors.
:::


##

### SCSS代码

以下SCSS代码文件存放路径为：`mycss/text-box.scss`

```
@mixin text-box($border-color, $background-color, $width, $height, $left, $top, $font-weight,$font-color) {
border: 1px; /* 设置边框 */
border-radius: 10%; /* 设置边框为圆弧角 */
border-color:  $border-color;
background-color: $background-color;
position: absolute;
z-index: 999;
width: $width;
height: $height;
left: $left; /* 距离左*/
top: $top;  /* 距离上边*/
text-align: left; /* 设置文本居中显示 */
font-weight: $font-weight; /* 文本加粗或正常 */
color: $font-color; /* 文本加粗或正常 */
}

.box4 {
@include text-box(#cceffc, #cceffc, 450px, 80px, 130px, 190px, bold, #000);
}

.box3 {
@include text-box(#cceffc, #cceffc, 440px, 80px, 650px, 190px, bold, #000);
}

.box1 {
@include text-box(#ffce33, #ffce33, 320px, 65px, 660px, 360px, bold, #000);
}
```

##

### Omitted variables (demo 1/4)

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-ommit-sequence/chpt11-ommit-sequence-04.png)

::: {.absolute top=230px left=70px width=640px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-A4 -->
Assumed "TRUE MODEL":
:::

##

### Omitted variables (demo  2/4)

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-ommit-sequence/chpt11-ommit-sequence-03.png)

::: {.absolute top=230px left=70px width=640px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-A4 -->
Assumed "TRUE MODEL":
:::

::: {.absolute top=230px left=840px width=650px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-A3 -->
"A's mis-specification MODEL":
:::

##

### Omitted variables (demo 3/4)

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-ommit-sequence/chpt11-ommit-sequence-02.png)

::: {.absolute top=230px left=70px width=640px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-A4 -->
Assumed "TRUE MODEL":
:::

::: {.absolute top=230px left=840px width=650px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-A3 -->
"A's mis-specification MODEL":
:::

##

### Omitted variables (demo 4/4)

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-ommit-sequence/chpt11-ommit-sequence-01.png)

::: {.absolute top=230px left=70px width=640px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-A4 -->
Assumed "TRUE MODEL":
:::

::: {.absolute top=230px left=840px width=650px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-A3 -->
"A's mis-specification MODEL":
:::



::: {.absolute top=460px left=850px width=400px height=135px style="background-color:#ffce33; font-weight:bold; color:#000; text-align:center; z-index:999; font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #ffce33;border: 1px;"}
<!-- .box-A1 -->
Omitted$\neq$ Disappeared
:::



## Source 2: Measurement errors

### The assumed true model & mis-specified model

Again, let's consider the **"assumed true model"**:

$$
\begin{align}
Wage_{i}=\beta_{1}+\beta_{2} Edu_{i}+\beta_{3} Abl_{i}+\epsilon_{i}  \quad \text{(the assumed true model)}
\end{align}
$$

It is hard to observe individual's **ability variable**($Abl$), and somebody will instead to use the variable **IQ score**($IQ_i$), and construct the  mis-specified **"proxy variable" model**:


$$
\begin {align}
Wage_i=\alpha_{1}+\alpha_{2} Edu_i+\alpha_{3} IQ_i+v_i \quad \text{(the mis-specified model)}
\end {align}
$$

::: {.callout-note}

- It should exist stuffs ($Abl\_other_i$) which the model does not include (due to the measurement error). So the measurement errors ($Abl\_other_i$) will go to the disturbance term$v_i$ in the mis-specified model.

- And we know that measurement errors ($Abl\_other_i$) will be correlated with the education variable. Thus $cov(Edu_i, v_i) \neq 0$, and the **education variable**($Edu$) may cause the endogenouse problem.

:::

::: {.notes}
In practice, the observation value of a regressor is not accurate, in most time is only the "approximation".So there is **measurement error** in the regressor in our model.
:::

## 

### Measurement errors (demo 1/4)

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-measure-sequence/chpt11-measure-sequence-04.png)


<!-- <div class="box-B4"> -->
<!-- Assumed TRUE MODEL: -->
<!-- </div> -->

::: {.absolute top=230px left=55px width=640px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-B4 -->
Assumed TRUE MODEL:
:::


## 

### Measurement errors (demo 2/4)

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-measure-sequence/chpt11-measure-sequence-03.png)


::: {.absolute top=230px left=55px width=640px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-B4 -->
Assumed TRUE MODEL:
:::


::: {.absolute top=230px left=840px width=650px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-B3 -->
B's mis-specification MODEL:
:::



## 

### Measurement errors (demo 3/4)

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-measure-sequence/chpt11-measure-sequence-02.png)


::: {.absolute top=230px left=55px width=640px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-B4 -->
Assumed TRUE MODEL:
:::


::: {.absolute top=230px left=840px width=650px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-B3 -->
B's mis-specification MODEL:
:::


## 

### Measurement errors (demo 4/4)

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-measure-sequence/chpt11-measure-sequence-01.png)


::: {.absolute top=230px left=55px width=640px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-B4 -->
Assumed TRUE MODEL:
:::


::: {.absolute top=230px left=840px width=650px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;"}
<!-- .box-B3 -->
B's mis-specification MODEL:
:::

<!-- <div class="box-B1"> -->
<!-- Measure Error$\neq$ Disappeared -->
<!-- </div> -->

::: {.absolute top=540px left=1000px width=400px height=135px style="background-color:#ffce33; font-weight:bold; color:#000; text-align:center; z-index:999; font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #ffce33;border: 1px;"}
<!-- .box-B1 -->
Measure Error$\neq$ Disappeared
:::


## Source 3: Autocorrelation

### The autoregressive model with AR(1)

**Autoregressive model**: Lag variable of dependent variable($Y_{t-1}, \ldots, Y_{t-p},\ldots$) appears in the model as regressors.

$$
\begin {align}
Y_t=\beta_{1}+\beta_{2} Y_{t-1}+\beta_{3}X_t+u_t
\end {align}
$$


If the disturbance term determined following a first-order autocorrelation AR(1):


$$
\begin {align}
u_t=\rho u_{t-1}+ \epsilon_t
\end {align}
$$

Then, it is obvious that $cov(Y_{t-1}, u_{t-1}) \neq 0$ and $cov(Y_{t-1}, u_{t}) \neq 0$.

Thus the **lag dependent variable** ($Y_{t-1}$)  will cause the endogeneity problem in the **Autoregressive model**.

##

### Autocorrelation (demo 1) 1/4

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-04.png)

<!-- <div class="box-C4"> -->
<!-- Assumed TRUE MODEL: AR(1) -->
<!-- </div> -->

::: {.absolute top=225px left=95px width=800px height=70px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;"}
<!-- .box-C4 -->
Assumed TRUE MODEL: AR(1)
:::



##

### Autocorrelation (demo 1)2/4 

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-03.png)

::: {.absolute top=225px left=95px width=800px height=70px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;"}
<!-- .box-C4 -->
Assumed TRUE MODEL: AR(1)
:::

::: {.absolute top=330px left=740px width=230px height=90px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-C31 -->
(main model)
:::

::: {.absolute top=450px left=490px width=250px height=90px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-C32 -->
(auxiliary model)
:::


##

### Autocorrelation (demo 1)3/4 

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-02.png)

::: {.absolute top=225px left=95px width=800px height=70px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;"}
<!-- .box-C4 -->
Assumed TRUE MODEL: AR(1)
:::

::: {.absolute top=330px left=740px width=230px height=90px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-C31 -->
(main model)
:::

::: {.absolute top=450px left=490px width=250px height=90px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-C32 -->
(auxiliary model)
:::

##

### Autocorrelation (demo 1) 4/4

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-01.png)

::: {.absolute top=225px left=95px width=800px height=70px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;"}
<!-- .box-C4 -->
Assumed TRUE MODEL: AR(1)
:::

::: {.absolute top=330px left=740px width=230px height=90px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-C31 -->
(main model)
:::

::: {.absolute top=450px left=490px width=250px height=90px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-C32 -->
(auxiliary model)
:::

<!-- <div class="box-C1"> -->
<!-- Hidden$\neq$ Disappeared -->
<!-- </div> -->

::: {.absolute top=310px left=1060px width=400px height=145px style="background-color:#ffce33; font-weight:bold; color:#000; text-align:center; z-index:999; font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #ffce33;border: 1px;"}
<!-- .box-C1 -->
Hidden$\neq$ Disappeared
:::


##

### Autocorrelation (demo 2) 1/5


An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-part2-05.png)

::: {.absolute top=210px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs5 -->
(main model)
:::



##

### Autocorrelation (demo 2) 2/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-part2-04.png)

::: {.absolute top=210px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs5 -->
(main model)
:::

::: {.absolute top=395px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs4 -->
(derived model)
:::


##

### Autocorrelation (demo 2) 3/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-part2-03.png)

::: {.absolute top=210px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs5 -->
(main model)
:::

::: {.absolute top=395px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs4 -->
(derived model)
:::



##

### Autocorrelation (demo 4) 1/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-part2-02.png)

::: {.absolute top=210px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs5 -->
(main model)
:::

::: {.absolute top=395px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs4 -->
(derived model)
:::

<!-- <div class="box-Cs2"> -->
<!-- (auxiliary model) -->
<!-- </div> -->

::: {.absolute top=720px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs2 -->
(auxiliary model)
:::

##

### Autocorrelation (demo 2) 5/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-ar-sequence/chpt11-ar-sequence-part2-01.png)

::: {.absolute top=210px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs5 -->
(main model)
:::

::: {.absolute top=395px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs4 -->
(derived model)
:::

::: {.absolute top=720px left=190px width=300px height=80px style="background-color:#cceffc; font-weight:normal; color:#000; text-align:center; z-index:999;font-size:85%!important;display: flex;align-items: center;justify-content: center;border-radius: 10%;border-color: #cceffc;border: 1px;"}
<!-- .box-Cs2 -->
(auxiliary model)
:::


## Source 4: Simultaneity

### The simultaneous equation model system


For the equations system of supply and demand:

$$
\begin{cases}
\begin{align}
\text { Demand: } & Q_{i}=\alpha_{1}+\alpha_{2} P_{i}+u_{d i} \\
\text { Supply: } & Q_{i}=\beta_{1}+\beta_{2} P_{i} + u_{s i}
\end{align}
\end{cases}
$$

As we all know, because of the price $P_i$ will both affect supply and the demand $Q_i$, And vice versa. There is a feedback cycle mechanism in this system.

So, we can get $cov(P_i, u_{di}) \neq 0$, and $cov(P_i, u_{si}) \neq 0$, which will cause the endogenous problem finally.

::: {.notes}
/ˌsɪmltəˈneɪət/
:::


##

### Simultaneity (demo 1) 1/4

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-04.png)

:::{.absolute top=285px left=700px width=100px height=100px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-D4 -->
(Demand
:::


##

### Simultaneity (demo 1) 2/4

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-03.png)

:::{.absolute top=285px left=800px width=150px height=80px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-D4 -->
(Demand
:::


::: {.absolute top=400px left=815px width=150px height=80px style="background-color:#cceffc; font-weight:bold; color:#000; text-align:left; z-index:999;font-size:80%!important;"}
<!-- .box-D3 -->
(Supply
:::


##

### Simultaneity (demo 1) 3/4

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-02.png)

<div class="box-D4">
(Demand
</div>

<div class="box-D3">
(Supply
</div>


##

### Simultaneity (demo 1) 4/4

Here we show a visual demonstration on this situation:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-01.png)

<div class="box-D4">
(Demand
</div>

<div class="box-D3">
(Supply
</div>

<div class="box-D1">
Hidden$\neq$ Disappeared
</div>

##

### 代码：序列动画

<!---demo2 ar endo begion--->



::: {.cell layout-align="center"}

:::




##

### Simultaneity (demo 2) 1/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-part-05.png)

<div class="box-Ds5">
D:
</div>


##

### Simultaneity (demo 2) 2/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-part-04.png)

<div class="box-Ds5">
D:
</div>

<div class="box-Ds4">
S:
</div>



##

### Simultaneity (demo 2) 3/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-part-03.png)

<div class="box-Ds5">
D:
</div>

<div class="box-Ds4">
S:
</div>


##

### Simultaneity (demo 2) 4/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-part-02.png)

<div class="box-Ds5">
D:
</div>

<div class="box-Ds4">
S:
</div>


##

### Simultaneity (demo 2) 5/5

An intuitive demonstration is show as follows:

![](pic/chpt11-endogeneity-sem-sequence/chpt11-sem-sequence-part-01.png)

<div class="box-Ds5">
D:
</div>

<div class="box-Ds4">
S:
</div>


# 17.2 Estimation problem with endogeneity{#problem .monash-bg-blue .mcenter}

::: {.notes}
The question is what will happen on OLS estimators with model contains endogenous regressors.So, let us illustrate these estimation problems in detail in this section.we will discuss along two different situations.
:::

## Inconsistent estimates

### mis-specified model with measurement error

Consider the simple regression model:

$$
\begin{align}
Y_{i}=\beta_{0}+\beta_{1} X_{i}+\epsilon_{i} \quad \text{(1)}
\end{align}
$$

We would like to measure the effect of the variable$X$ on$Y$, but we
can observe only an imperfect measure of it (i.e. a **proxy variable**),
which is

$$
\begin{align}
X^{\ast}_{i}=X_{i} - v_{i} \quad \text{(2)}
\end{align}
$$

Where$v_i$ is a random disturbance with mean 0 and variance$\sigma_{v}^{2}$.

Further, let's assume that$X_i, \epsilon_i$ and$v_i$ are **pairwise independent**.

::: {.notes}
In the second situation, OLS estimators will be also inconsistent with measurement error on regressors.
:::

##

### Proxy variable and the composite error term

Given the assumed true model (1):

$$
\begin{align}
Y_{i} &=\beta_{0}+\beta_{1} X_{i}+\epsilon_{i} && \text{ eq(1) assumed true model }
\end{align}
$$

with the proxy variable$X^{\ast}_i$, we may use the error specified model (4):

$$
\begin{align}
X^{\ast}_i &= X_i - v_i && \text{ eq(2) proxy variable }\\
X_i &= X^{\ast}_i + v_i && \text{ eq(3)}\\
Y_{i} &=\beta_{0}+\beta_{1} X^{\ast}_{i}+u_{i} && \text{ eq(4) error specified model}
\end{align}
$$

We can substitute eq (3) into the model (1) to obtain eq(5)

$$
\begin{align}
Y_{i} =\beta_{0}+\beta_{1} X_{i}^{*}+\epsilon_{i}
=\beta_{0}+\beta_{1}\left(X^{\ast}_{i} + v_{i}\right)+\epsilon_{i}
=\beta_{0}+\beta_{1} X^{\ast}_{i}+\left(\epsilon_{i} + \beta_{1} v_{i}\right)  \quad \text{eq(5)}
\end{align}
$$

which means$u_{i}=\left(\epsilon_{i}+\beta_{1} v_{i}\right)$ in the error specified model. As we know, the OLS consistent estimator of$\beta_{1}$ in the last equation[**requires**]{.red}$\operatorname{Cov}\left(X^{\ast}_{i}, u_{i}\right)= 0$.

::: {.notes}
For simple, we just replace the $X_i$ in the assumed true model 1, with the proxy variable $X^{\ast}_i$, and get the error specified model 4. Meanwhile the measurement error will fall into the composite error term $u_i$. As you see.
:::

##

### The error measurement induce problem of endogeneity 

Note that$E\left(u_{i}\right)=E\left(\epsilon_{i} +\beta_{1} v_{i}\right)=E\left(\epsilon_{i}\right)+\beta_{1} E\left(v_{i}\right)=0$


However,

$$
\begin{aligned}
\operatorname{Cov}\left(X^{\ast}_{i}, u_{i}\right) &=E\left[\left(X^{\ast}_{i}-E\left(X^{\ast}_{i}\right)\right)\left(u_{i}-E\left(u_{i}\right)\right)\right] \\ &=E\left(X^{\ast}_{i} u_{i}\right) \\
&=E\left[\left(X_{i}-v_{i}\right)\left(\epsilon_{i} +\beta_{1} v_{i}\right)\right] \\
&=E\left[X_{i} \epsilon_{i}+\beta_{1} X_{i} v_{i}- v_{i}\epsilon_{i}- \beta_{1} v_{i}^{2}\right]
\leftarrow \quad \text{(pairwise independent)}  \\
&=-E\left(\beta_{1} v_{i}^{2}\right) \\
&=-\beta_{1} \operatorname{Var}\left(v_{i}\right) \\
&=-\beta_{1} \sigma_{v_i}^{2} \neq 0
\end{aligned}
$$

Thus,$X$ in model (4) is [**endogenous**]{.red} and we expect the OLS estimator of$\beta_{1}$ to be **inconsistent**.

::: {.notes}
So far as we know, both omitted variable(s) and error measurement will cause endogeneity, and result in violation of A2.Let us sum up all these cases in general.
:::

##

### OLS estimates will be biased

In general, when **A2** is violated, we expect OLS estimates to be **biased**:

The OLS estimators of $\hat{\beta}$ is

$$
\begin{align}
\widehat{\beta} &=\beta+\left(X^{\prime} X\right)^{-1} X^{\prime} \epsilon && \text{(6)}
\end{align}
$$

and we can take expectation on both sides.

$$
\begin{equation}
\begin{aligned}
E(\widehat{\beta}) &=\beta+E\left(\left(X^{\prime} X\right)^{-1} X^{\prime} \epsilon\right) \\
&=\beta+E\left(E\left(\left(X^{\prime} X\right)^{-1} X^{\prime} \epsilon | X\right)\right) \\
&=\beta+E\left(\left(X^{\prime} X\right)^{-1} X^{\prime} E(\epsilon | X)\right) \neq \beta
\end{aligned}
\end{equation}
$$


If **A2**$E(\epsilon | X) = 0$ is violated, which means$E(\epsilon | X) \neq 0$, the OLS estimator is **biased**.

::: {.notes}
The result will show that the OLS estimator is not equal to its true parameter if **A2** is violated.> Here, you should know that the value take expectation on conditional expectation is equal to unconditional expectation.
:::

## 

### conditions for consistent estimates

Let's see under what conditions we can establish consistency.

$$
\begin{equation}
\begin{aligned}
p \lim \widehat{\beta} &=\beta+p \lim \left(\left(X^{\prime} X\right)^{-1} X^{\prime} \epsilon\right)
=\beta+p \lim \left(\left(\frac{1}{n} X^{\prime} X\right)^{-1} \frac{1}{n} X^{\prime} \epsilon\right) \\
&=\beta+p \lim \left(\frac{1}{n} X^{\prime} X\right)^{-1} \times p \lim \left(\frac{1}{n} X^{\prime} \epsilon\right)
\end{aligned}
\end{equation}
$$

By the WLLN (Weak Law of Large Numbers)

$$
\begin{equation}
\frac{1}{n} X^{\prime} \epsilon=\frac{1}{n} \sum_{i=1}^{n} X_{i} \epsilon_{i} \xrightarrow{p} E\left(X_{i} \epsilon_{i}\right)
\end{equation}
$$

Hence$\widehat{\beta}$ is consistent if$E\left(X_{i} \epsilon_{i}\right)=0$ for all $i$. The condition$E\left(X_{i} \epsilon_{i}\right)=0$ is more likely to be satisfied than A2$E(\epsilon | X) = 0$. Thus, a large class of estimators that cannot be proved to be **unbiased** are **consistent**.

::: {.notes}
Let's see under what conditions we can establish consistency.We can take the probability limits on both sides of equation (6).we will derive that the probability limits of this part will equal to expectation of $X_i$ times $\epsilon_i$We have dived deeply with theory evidence.Now, I will show you an example.
:::

## Case: Wage example

### The origin model

Consider the following "error specified" wage model:

$$
\begin{align}
lwage_i = \beta_1 +\beta_2educ_i + \beta_3exper_i +\beta_4expersq_i + e_i
\end{align}
$$

The difficulty with this model is that the error term may include some unobserved attributes, such as personal **ability**, that determine both wage and education.

In other words, the independent variable  **educ**  is correlated with the error term. And it is endogenous variable.

> **Note**:

> We will use **years of schooling** as the proxy variable of **educ** in practice, and it surely bring in error measurement issues as we have mentioned.

::: {.notes}
Consider the following error specified wage model, which the logarithmic (ˌlɒɡəˈrɪðmɪk) wage is the function of education,labor market experience and its quadratic term.As what we have discussed before. The difficulty with this model is that the error term may include some unobserved attributes, such as personal **ability**, that determine both wage and education.> There is one more thing we should remind. We will use **years of schooling** as the proxy variable of **educ** in practice, and it surely bring in error measurement issues as we have mentioned.Let us show all variables of our dataset.
:::

##

### Variables in dataset

With data set `wooldridge::mroz`, researchers were interest in the return to education for married women.




::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-01da130b5a2a2fc603fc" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-01da130b5a2a2fc603fc">{"x":{"filter":"none","vertical":false,"extensions":["Buttons"],"caption":"<caption>Variables and Labels<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22"],[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22],["educ","exper","expersq","fatheduc","lwage","motheduc","wage","age","city","faminc","hours","husage","huseduc","hushrs","huswage","inlf","kidsge6","kidslt6","mtr","nwifeinc","repwage","unem"],[" years of schooling"," actual labor mkt exper"," exper^2"," father's years of schooling"," log(wage)"," mother's years of schooling"," est. wage from earn, hrs"," woman's age in yrs"," =1 if live in SMSA"," family income, 1975"," hours worked, 1975"," husband's age"," husband's years of schooling"," hours worked by husband, 1975"," husband's hourly wage, 1975"," =1 if in lab frce, 1975","  kids 6-18","  kids  under 6 years"," fed. marg. tax rte facing woman"," (faminc - wage*hours)/1000"," rep. wage at interview in 1976"," unem. rate in county of resid."]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>index<\/th>\n      <th>vars<\/th>\n      <th>labels<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"Btip","pageLength":6,"rownames":false,"columnDefs":[{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"index","targets":1},{"name":"vars","targets":2},{"name":"labels","targets":3}],"buttons":["copy","csv","excel"],"initComplete":"function(settings, json) {\n$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});\n}","order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[6,10,25,50,100]},"callback":"function(table) {\n$(\"button.buttons-copy\").css(\"background\",\"yellow\");\n                $(\"button.buttons-csv\").css(\"background\",\"orange\");\n                $(\"button.buttons-excel\").css(\"background\",\"#a88d32\");\n                $(\"button.dt-button\").css(\"padding\",\"0.2em 0.2em\");\n                $(\"button.dt-button\").css(\"font-size\",\"0.6em\");\n                $(\"button.dt-button\").css(\"margin-right\",\"0.2em\");\n                $(\"button.dt-button\").css(\"margin-bottom\",\"0.1em\");\n                $(\"button.dt-button\").css(\"line-height\",\"1em\");\n                return table;\n}"},"evals":["options.initComplete","callback"],"jsHooks":[]}</script>
```


:::
:::




::: {.notes}
Other variables include father education, mother education, which are both measured by schooling years, etc (ɪt ˈsetərə).
:::

##

### Raw dataset




::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-6cc4b5f7e2ba65356006" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-6cc4b5f7e2ba65356006">{"x":{"filter":"none","vertical":false,"extensions":["Buttons"],"caption":"<caption>dataset n=(428)<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284","285","286","287","288","289","290","291","292","293","294","295","296","297","298","299","300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345","346","347","348","349","350","351","352","353","354","355","356","357","358","359","360","361","362","363","364","365","366","367","368","369","370","371","372","373","374","375","376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423","424","425","426","427","428"],[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428],[1.210153698921204,0.3285121023654938,1.514137744903564,0.09212332218885422,1.524272203445435,1.556480050086975,2.120259523391724,2.059634208679199,0.7543363571166992,1.544899344444275,1.401921629905701,1.524272203445435,0.7339532375335693,0.8183690905570984,1.302831172943115,0.2980283796787262,1.167609572410583,1.643839359283447,0.6931471824645996,2.021931648254395,1.254247546195984,1.272957682609558,1.178655028343201,1.178655028343201,0.7675586938858032,1.331811785697937,1.386294364929199,1.553269624710083,1.981814861297607,1.769360423088074,0.430807888507843,0.8997548222541809,1.766629695892334,1.272957682609558,1.336788892745972,0.9017048478126526,0.8651236891746521,1.511847138404846,1.72602915763855,2.683142423629761,0.9852942824363708,1.365938544273376,0.9450336694717407,1.512376189231873,0.6931471824645996,1.244788408279419,0.7011649012565613,1.519863247871399,0.8209685683250427,0.9698315262794495,0.828508198261261,0.09430964291095734,0.1625438928604126,0.4700036346912384,0.6292484402656555,1.397160172462463,2.265443801879883,2.084541082382202,1.525838851928711,0.762160062789917,1.48160457611084,1.262826442718506,0.9996755719184875,1.832581520080566,2.479307651519775,1.279015302658081,1.937935590744019,1.070452809333801,1.12392258644104,1.321755886077881,1.744999766349792,1.301743626594543,1.641866445541382,2.107020139694214,1.46706759929657,1.605811357498169,-1.029739379882812,1.08768618106842,0,0.9382086992263794,-0.1505903750658035,0,1.073670506477356,1.265848398208618,0.4863689839839935,2.120259523391724,1.129852533340454,0.9932518005371094,1.658627986907959,0.3474121987819672,1.568324208259583,0.5108456015586853,0.1148454323410988,-0.6931471824645996,-0.3364522755146027,1.028225541114807,1.58068859577179,0.5558946132659912,0.9014207124710083,0.8843045830726624,0.4282045960426331,1.058415055274963,0.8783395886421204,1.654908299446106,1.321755886077881,0.3285121023654938,1.386294364929199,1.172884583473206,1.224187135696411,0.2876570820808411,2.23026180267334,1.504077434539795,1.531152009963989,1.375157594680786,1.760268807411194,-0.6931471824645996,1.406489133834839,1.791759490966797,1.299292087554932,1.351003885269165,1.016280889511108,1.075343608856201,1.478964686393738,1.689486742019653,2.288597822189331,-1.822631120681763,-0.9607651829719543,1.290994167327881,0.8648711442947388,1.540452122688293,0.6162121295928955,1.648658633232117,1.193498134613037,2.143976211547852,0.7244035601615906,0.9416075348854065,0.7827593684196472,1.832581520080566,1.203962802886963,1.491644859313965,1.892132639884949,2.130894899368286,1.48060405254364,0.8943313360214233,0.2025325447320938,0.4855078160762787,1.098612308502197,1.553269624710083,0.1215979680418968,2.001804351806641,1.495036602020264,0.9052298069000244,0.6325475573539734,1.386294364929199,2.102913856506348,1.959643959999084,0.5108456015586853,1.236923933029175,1.443312525749207,1.021659255027771,0.6361534595489502,1.616453289985657,0.2231435477733612,1.049807071685791,1.415051937103271,0.5753766298294067,2.60668158531189,1.517914533615112,0.7550415992736816,1.094972372055054,0.9421143531799316,1.724942803382874,1.031546115875244,0.474369078874588,0.8109301924705505,0.7092666029930115,1.710549473762512,0.4602688848972321,1.331811785697937,1.098612308502197,2.157998561859131,1.437581300735474,1.544899344444275,1.41059672832489,3.218875885009766,0.9681618809700012,1.791759490966797,1.688729524612427,-0.4091719686985016,0.2231435477733612,0.8221558332443237,1.24170196056366,1.427124381065369,1.497097492218018,0.5596157908439636,1.300028204917908,1.884429812431335,0.9555113911628723,1.582087278366089,1.755614042282104,1.513103246688843,2.251891613006592,2.364432334899902,0.1053504794836044,1.399728775024414,0.988462507724762,1.090647339820862,1.154614448547363,1.266947627067566,2.885191679000854,1.228880047798157,1.203962802886963,1.357380270957947,0.8377236127853394,0.5369611382484436,0.7487238049507141,2.295872688293457,1.107803225517273,0.6208452582359314,-2.054163694381714,1.892012000083923,1.729724526405334,0.4693784117698669,0.9808416962623596,2.069492340087891,1.675188183784485,1.386294364929199,1.799214959144592,1.832581520080566,1.090647339820862,1.443123579025269,1.250360131263733,1.602312564849854,1.018558502197266,1.297053217887878,1.685194492340088,-0.4209848940372467,1.562094688415527,2.146527528762817,2.347462892532349,0.9698315262794495,1.924146413803101,1.62672758102417,-0.03926072642207146,1.460148692131042,1.955393552780151,0.9263598918914795,2.066191673278809,1.422843217849731,2.10103178024292,2.261461019515991,0.7013137936592102,2.031012535095215,1.162369251251221,0.4700036346912384,1.41059672832489,0.3930551111698151,1.290994167327881,0,0.9571254849433899,0.5596157908439636,1.568615913391113,1.710187911987305,1.41059672832489,0.2231435477733612,0.5108456015586853,1.332392454147339,0.8601858615875244,2.322779893875122,1.91959547996521,1.976106762886047,0.8954347372055054,0.18123759329319,0.4953058362007141,0.5777924060821533,1.07881772518158,1.603198528289795,0.6208452582359314,2.083894014358521,1.379169106483459,1.112383723258972,1.067121624946594,1.118806958198547,1.588541030883789,1.390311241149902,1.714806437492371,0.2010615319013596,0.9872710108757019,0.9835006594657898,2.233170747756958,1.143617510795593,-0.6113829016685486,2.153052091598511,1.299837350845337,0.8409204483032227,1.058484435081482,1.152658462524414,1.293575882911682,1.832581520080566,2.327180147171021,1.166146278381348,2.034993171691895,0.6792510747909546,1.547136902809143,0.7530185580253601,0.8472836017608643,0.8711259961128235,0.2282504737377167,0.08965782821178436,1.321755886077881,1.196101903915405,1.636118769645691,1.892012000083923,1.518308997154236,2.472159147262573,1.321755886077881,1.473641037940979,1.369478821754456,1.203962802886963,1.198729157447815,1.270209908485413,0.4700036346912384,0.7999816536903381,1.565945625305176,1.758978009223938,0.858025848865509,0.6931471824645996,0.6418538689613342,1.633740186691284,1.703747630119324,1.844004034996033,1.966118812561035,0.8649974465370178,0.9333052039146423,0.7792331576347351,0.9555113911628723,1.316247344017029,1.475906491279602,1.491397261619568,1.455750465393066,0.5108456015586853,1.180438041687012,1.688489437103271,0.7907274961471558,1.401798605918884,-0.4335560202598572,1.683171510696411,-1.766676664352417,3.155595064163208,2.259521007537842,1.306926369667053,0.7984976768493652,0.5590441823005676,0.1479026228189468,1.944494843482971,1.378337860107422,3.064745187759399,-0.7419173121452332,0.7657003998756409,0.619392991065979,1.465452075004578,2.18925952911377,1.021659255027771,0.9770094752311707,0.9162907600402832,2.905096054077148,-0.1996711939573288,0.6931471824645996,2.733392953872681,1.868334650993347,2.120259523391724,1.515193223953247,0.9146093130111694,1.499556064605713,0.803077220916748,0.7280316352844238,0.5164099931716919,1.22644829750061,0.9162907600402832,1.376471281051636,1.828974962234497,1.368283152580261,1.064710736274719,1.406489133834839,1.047318935394287,1.948093414306641,1.078001379966736,0.6539384722709656,1.927891612052917,1.361027836799622,0.6931471824645996,1.604686617851257,0.1839036494493484,3.113515377044678,1.926829218864441,1.2701256275177,0.6826927065849304,1.68106997013092,0.5562959909439087,1.628220438957214,0.9162907600402832,1.341558456420898,0,1.122231245040894,0.5401707887649536,1.391505718231201,1.697173953056335,3.218875885009766,0.871167778968811,1.167329549789429,1.216987729072571,0.5753766298294067,1.151615738868713,0.9942512512207031,0.5263249278068542,-1.543182134628296,1.91204309463501,0.554287314414978,0.9162907600402832,1.500939130783081,0.9446837902069092,1.241268634796143,1.564984321594238,0.8380264639854431,1.668857097625732,1.769428610801697,1.22644829750061,1.406489133834839],[12,12,12,12,14,12,16,12,12,12,12,11,12,12,10,11,12,12,12,12,16,12,13,12,12,17,12,12,17,12,11,16,13,12,16,11,12,10,14,17,12,12,16,12,12,12,16,12,12,12,12,12,12,8,10,16,14,17,14,12,14,12,8,12,12,8,17,12,12,12,12,12,9,10,12,12,12,17,15,12,6,14,12,14,9,17,13,9,15,12,12,12,12,12,12,12,12,13,12,13,12,12,12,16,12,13,11,12,12,12,17,14,16,17,12,11,12,12,17,10,13,11,12,16,17,12,16,12,16,8,12,12,12,13,11,12,12,14,12,12,12,17,14,12,9,12,12,12,14,16,17,15,12,16,17,17,12,16,13,12,11,16,14,16,12,9,17,14,12,12,11,12,12,10,12,5,17,11,12,12,14,11,12,14,12,10,16,13,12,12,12,11,12,9,13,12,12,12,13,16,12,16,17,12,12,9,12,12,13,12,12,12,12,10,12,16,12,11,12,10,12,12,12,12,16,17,12,17,12,12,12,8,12,13,12,12,8,12,17,17,12,13,12,12,12,12,9,10,12,16,13,8,16,13,12,11,13,12,12,10,12,17,15,16,10,11,12,12,14,16,14,8,7,12,12,14,12,12,12,14,16,12,12,12,13,13,10,12,12,12,12,14,17,10,9,12,12,16,12,17,12,17,11,16,11,13,11,8,11,12,10,17,12,12,17,14,12,12,12,12,12,12,9,10,12,12,12,12,12,17,12,17,12,10,12,12,12,12,12,12,16,13,13,12,16,17,12,14,12,17,12,14,12,12,17,16,16,12,9,12,12,16,14,12,12,11,12,16,17,17,14,12,14,12,10,12,13,16,12,7,16,14,12,10,12,16,10,12,14,12,6,15,12,17,14,13,6,16,14,15,14,8,14,12,12,12,12,12,12,8,12,17,12,12,14,13,17,8,12,11,12,12,17,10,12,13,12,12],[14,5,15,6,7,33,11,35,24,21,15,14,0,14,6,9,20,6,23,9,5,11,18,15,4,21,31,9,7,7,32,11,16,14,27,0,17,28,24,11,1,14,6,10,6,4,10,22,16,6,12,32,15,17,34,9,37,10,35,6,19,10,11,15,12,12,14,11,9,24,12,13,29,11,13,19,2,24,9,6,22,30,10,6,29,29,36,19,8,13,16,11,15,6,13,22,24,2,6,2,2,14,9,11,9,6,19,26,19,3,7,28,13,9,15,20,29,9,1,8,19,23,3,13,8,17,4,15,11,7,0,0,10,8,2,4,6,18,3,22,33,28,23,27,11,6,11,14,17,17,14,11,7,8,6,8,4,25,24,11,19,9,19,14,22,6,23,15,6,11,2,22,10,14,12,9,13,18,8,11,9,9,14,9,2,12,15,11,7,9,19,11,8,13,4,7,19,14,14,3,9,7,7,14,29,19,14,16,10,12,24,6,9,14,26,7,4,15,23,1,29,9,6,11,17,6,7,2,24,4,11,25,11,2,19,7,2,20,10,19,17,12,11,6,10,4,2,13,21,9,4,2,19,4,9,14,6,24,1,13,3,10,16,9,19,4,10,5,7,3,38,16,13,1,7,15,10,2,19,25,25,7,15,11,25,19,4,14,19,18,14,11,4,29,21,24,19,31,28,15,27,13,4,10,8,4,18,3,11,8,10,33,19,35,21,7,18,4,12,16,14,3,1,27,12,6,9,2,6,9,16,22,26,11,11,15,13,6,20,17,8,13,15,14,14,6,24,10,2,9,23,12,8,16,10,7,19,2,9,14,9,16,7,6,22,9,9,14,17,12,13,8,10,16,1,6,4,8,4,15,7,14,16,15,23,19,4,12,12,25,14,14,11,7,18,4,37,13,14,17,5,2,0,3,21,20,19,4,19,11,14,8,13,24,1,1,3,4,21,10,13,9,14,2,21,22,14,7],[196,25,225,36,49,1089,121,1225,576,441,225,196,0,196,36,81,400,36,529,81,25,121,324,225,16,441,961,81,49,49,1024,121,256,196,729,0,289,784,576,121,1,196,36,100,36,16,100,484,256,36,144,1024,225,289,1156,81,1369,100,1225,36,361,100,121,225,144,144,196,121,81,576,144,169,841,121,169,361,4,576,81,36,484,900,100,36,841,841,1296,361,64,169,256,121,225,36,169,484,576,4,36,4,4,196,81,121,81,36,361,676,361,9,49,784,169,81,225,400,841,81,1,64,361,529,9,169,64,289,16,225,121,49,0,0,100,64,4,16,36,324,9,484,1089,784,529,729,121,36,121,196,289,289,196,121,49,64,36,64,16,625,576,121,361,81,361,196,484,36,529,225,36,121,4,484,100,196,144,81,169,324,64,121,81,81,196,81,4,144,225,121,49,81,361,121,64,169,16,49,361,196,196,9,81,49,49,196,841,361,196,256,100,144,576,36,81,196,676,49,16,225,529,1,841,81,36,121,289,36,49,4,576,16,121,625,121,4,361,49,4,400,100,361,289,144,121,36,100,16,4,169,441,81,16,4,361,16,81,196,36,576,1,169,9,100,256,81,361,16,100,25,49,9,1444,256,169,1,49,225,100,4,361,625,625,49,225,121,625,361,16,196,361,324,196,121,16,841,441,576,361,961,784,225,729,169,16,100,64,16,324,9,121,64,100,1089,361,1225,441,49,324,16,144,256,196,9,1,729,144,36,81,4,36,81,256,484,676,121,121,225,169,36,400,289,64,169,225,196,196,36,576,100,4,81,529,144,64,256,100,49,361,4,81,196,81,256,49,36,484,81,81,196,289,144,169,64,100,256,1,36,16,64,16,225,49,196,256,225,529,361,16,144,144,625,196,196,121,49,324,16,1369,169,196,289,25,4,0,9,441,400,361,16,361,121,196,64,169,576,1,1,9,16,441,100,169,81,196,4,441,484,196,49],[7,7,7,7,14,7,7,3,7,7,3,7,16,10,7,10,7,12,7,7,16,10,3,7,7,14,7,7,12,12,7,3,10,14,12,3,3,3,7,17,12,9,16,3,7,7,16,10,7,7,7,3,7,7,3,12,7,17,7,7,3,12,7,7,7,12,16,7,7,7,12,10,9,0,10,14,7,3,12,12,7,17,3,7,7,12,7,7,12,10,0,12,10,7,7,7,3,12,7,12,7,7,10,14,7,12,7,7,10,7,12,7,7,17,7,7,7,10,10,12,7,12,7,10,7,10,7,7,7,7,7,7,16,12,7,3,7,7,7,12,7,12,12,14,7,7,7,12,12,14,10,12,7,16,7,17,3,10,9,7,3,16,12,7,7,7,12,3,7,7,7,7,10,10,7,12,17,10,7,7,12,7,12,7,7,7,7,14,7,12,7,7,12,3,7,12,12,7,7,14,12,17,17,7,7,10,7,7,7,3,0,7,12,7,7,12,7,7,12,7,7,3,7,7,10,12,7,12,7,7,7,7,12,7,7,7,7,7,14,17,7,10,7,7,12,7,7,9,7,14,7,3,16,3,16,7,16,12,7,7,12,12,12,16,7,9,7,12,12,10,7,12,7,7,3,10,17,7,3,3,12,7,7,7,10,7,10,7,9,9,12,12,12,7,9,12,7,12,7,12,12,14,7,14,10,12,7,7,12,7,7,12,12,12,12,14,10,7,7,7,7,7,7,7,7,7,12,12,7,14,10,12,7,7,12,7,10,12,10,7,14,7,12,12,7,16,0,12,7,17,7,12,3,7,14,7,12,12,7,7,14,7,12,12,7,7,7,7,3,12,7,7,14,10,10,10,10,12,3,7,16,7,7,0,7,7,10,7,12,7,7,7,7,16,12,10,7,7,16,7,7,7,12,10,10,10,7,10,12,12,7,17,12,16,10,16,7,7,9,3,7,7,7,7,7,7,16,12],[12,7,12,7,12,14,14,3,7,7,12,14,16,10,7,16,10,12,7,12,10,12,7,7,12,16,3,3,12,12,7,3,12,7,12,10,3,10,7,14,12,9,14,3,12,12,14,10,7,12,7,7,12,7,7,12,7,17,17,12,14,12,7,7,7,12,12,12,7,12,12,10,7,0,7,12,7,3,10,7,12,12,7,7,7,7,7,7,7,10,7,12,10,12,7,7,7,14,7,12,12,7,7,14,12,10,7,7,7,7,12,7,12,10,10,7,7,7,12,7,7,12,14,12,7,10,7,7,12,10,7,7,12,10,7,12,7,7,7,7,3,12,16,7,3,12,7,12,12,16,12,12,7,14,7,10,7,14,7,7,12,12,17,7,7,3,12,7,7,7,3,7,10,10,12,7,14,10,7,7,10,12,12,7,7,7,12,7,12,12,12,10,12,10,12,12,12,7,12,12,12,12,16,7,16,7,7,10,12,10,0,7,12,12,10,12,3,7,12,10,7,7,7,7,12,12,7,12,7,10,10,7,12,17,7,7,7,7,12,14,7,12,7,7,16,7,10,12,7,16,10,3,16,7,12,7,7,7,12,12,7,10,14,16,7,10,7,14,14,12,7,7,3,7,7,7,12,10,7,3,12,7,12,7,10,7,0,7,10,9,12,12,12,3,9,12,12,14,7,12,12,12,7,12,10,12,7,7,3,12,7,16,12,12,7,14,7,7,12,10,7,3,7,7,10,7,7,12,12,12,10,14,7,7,14,10,10,7,7,7,14,7,12,14,14,14,0,16,7,12,7,10,7,10,10,14,7,12,7,7,12,14,12,7,7,7,12,16,3,16,7,16,7,10,10,10,10,12,10,7,16,7,7,7,7,7,10,10,12,7,7,7,7,14,14,7,7,7,16,12,7,7,12,12,12,10,3,12,12,12,7,16,12,10,10,12,7,7,7,7,7,7,7,7,7,7,12,12]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>lwage<\/th>\n      <th>educ<\/th>\n      <th>exper<\/th>\n      <th>expersq<\/th>\n      <th>fatheduc<\/th>\n      <th>motheduc<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"Btip","pageLength":8,"rownames":false,"columnDefs":[{"targets":2,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 2, 3, \",\", \".\", null);\n  }"},{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"id","targets":1},{"name":"lwage","targets":2},{"name":"educ","targets":3},{"name":"exper","targets":4},{"name":"expersq","targets":5},{"name":"fatheduc","targets":6},{"name":"motheduc","targets":7}],"buttons":["copy","csv","excel"],"initComplete":"function(settings, json) {\n$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});\n}","order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[8,10,25,50,100]},"callback":"function(table) {\n$(\"button.buttons-copy\").css(\"background\",\"yellow\");\n                $(\"button.buttons-csv\").css(\"background\",\"orange\");\n                $(\"button.buttons-excel\").css(\"background\",\"#a88d32\");\n                $(\"button.dt-button\").css(\"padding\",\"0.2em 0.2em\");\n                $(\"button.dt-button\").css(\"font-size\",\"0.6em\");\n                $(\"button.dt-button\").css(\"margin-right\",\"0.2em\");\n                $(\"button.dt-button\").css(\"margin-bottom\",\"0.1em\");\n                $(\"button.dt-button\").css(\"line-height\",\"1em\");\n                return table;\n}"},"evals":["options.columnDefs.0.render","options.initComplete","callback"],"jsHooks":[]}</script>
```


:::
:::




::: {.aside}
Try yourself! Click and download the `.csv`file
:::

::: {.notes}
We will use 428 observations totally after delete rows with missing wage value.
:::


##

### The scatter



::: {.cell layout-align="center"}
::: {.cell-output-display}
![](images/lecture-17-endogeneity-eng.rmarkdown/unnamed-chunk-7-1.png){fig-align='center' width=960}
:::
:::



##

### The mis-specified model

The Assumed TRUE model is

$$
\begin{align}
log(wage_i)=\beta_{0}+\beta_{1} edu_{i}+\beta_{2} exper_{i}+\beta_{3} exper^2_{i}+\beta_{4} ability_{i}+\epsilon_{i}
\end{align}
$$

Now, let's consider the mis-specified model

$$
\begin{align}
log(wage_i)=\alpha_{0}+\alpha_{1} edu_{i}+\alpha_{2} exper_{i}+\alpha_{3} exper^2_{i}+u_{i} \quad \text{(omitted ability)}
\end{align}
$$

- In the theory analysis, we have known that this model is mis-specified due to the important omitted variables(`ability`) and it will be dropped to the disturbance term$u_i$.

- While the regressor `edu` is correlated with the disturbance term$u_i$ (formally the omitted variables `ability`), thus the regressor `edu` is endogenous variable!


##

### Using OLS method directly: tidy report

Of course, you can conduct the OLS regression directly without considering problems due to endogeneity, and may obtain the inconsistent estimators (as we have proved).





::: {.cell layout-align="center"}
$$
\begin{aligned}
\begin{split}
lwage_i=&+\beta_{1}+\beta_{2}educ_i+\beta_{3}exper_i+\beta_{4}expersq_i+u_i
\end{split}
\end{aligned}
$$
:::



The OLS regression resuts is



::: {.cell layout-align="center"}
$$
\begin{alignedat}{999}
\begin{split}
&\widehat{lwage}=&&-0.52&&+0.11educ_i&&+0.04exper_i&&-0.00expersq_i\\ 
&(s)&&(0.1986)&&(0.0141)&&(0.0132)&&(0.0004)\\ 
&(t)&&(-2.63)&&(+7.60)&&(+3.15)&&(-2.06)\\ 
&(over)&&n=428&&\hat{\sigma}=0.6664 && &&\\ 
&(fit)&&R^2=0.1568&&\bar{R}^2=0.1509 && &&\\ 
&(Ftest)&&F^*=26.29&&p=0.0000 && &&
\end{split}
\end{alignedat}
$$
:::




This looks good, but we know it is not reliable due to endogeneity behind this "error specified" model.

::: {.notes}
You can type and run these R codes and then get the following tidy OLS results.You may see that:- The t statistic of estimators are given in this row.- And this row shows the standard error of the estimators.- The t test of all coefficients are significant with its abslute t value larger than 2.- And the F test is also significant with a small p value.This looks good, but we know it is not reliable due to endogeneity behind this "error specified" model.
:::


##

### Using OLS method directly: raw report

<!---.scroll-output[--->



::: {.cell layout-align="center"}

```{.r .cell-code}
mod_origin <- formula(lwage ~ educ +exper+expersq)
ols_origin <- lm(formula = mod_origin, data = mroz)
summary(ols_origin)
```
:::



<!---]--->


# 17.3 IV and the choices{#IV .monash-bg-blue .mcenter}

::: {.notes}
How to handle the endogenous variable problems?There is a way to go ahead, and we will resort to use instrumental variables in this section.
:::


## IV: problem and the motivation

We have seen that OLS estimator of$\beta$ is inconsistent when one or more regressors is endogenous .

The **problems** of OLS arise because we imposed$E(X_i\epsilon_i)=0$, which means we believe the sample data with

$$
\boldsymbol{X^{\prime} {e}=0}
$$

When in fact error terms and regressors are correlated$E(X_i\epsilon_i) \neq 0$.


## IV: conditions review

Suppose we can find a set of **explanatory variables**$\boldsymbol{Z}$ satisfying two
conditions:

- **Relevance**:$\boldsymbol{Z}$ is correlated with$\boldsymbol{X}$

- **Exogeneity**:$\boldsymbol{Z}$ is not correlated with$\boldsymbol{\epsilon}$

These variables ($\boldsymbol{Z}$, in matrix form) can be used for consistent estimation and are known as [**Instrumental Variables (IV)**]{.red} .



## IV: Estimators

Our instrumental variable estimator,$\hat{\beta}_{IV}$ is defined in terms of the following "**normal equation**" (**moment condition**, to be more precise)

$$
\begin{align}
\boldsymbol{Z^{\prime} \hat{\epsilon}=Z^{\prime}\left(y-X \hat{\beta}_{IV}\right)=0}
\end{align}
$$

and thus, provided that$Z^{\prime} X$ is **square** and **non singular**,

$$
\begin{align}
\boldsymbol{\hat{\beta}_{IV}=\left(Z^{\prime} X\right)^{-1} Z^{\prime} y}
\end{align}
$$

The condition that$\boldsymbol{Z^{\prime} X}$ is square and non singular, intuitively, is satisfied when we have as many instruments as regressors (a situation that is called **exact identification**).

However$\boldsymbol{\hat{\beta}_{IV}}$ is generally **biased** in **finite sample**, but we can show that it is still **consistent**.

::: {.notes}
So let us prove this point.
:::


## IV: Consistent estimator

$\boldsymbol{\hat{\beta}_{IV}}$ is consistent. Since:

$$
\begin{align}
\boldsymbol{\hat{\beta}_{IV}
=\left(Z^{\prime} X\right)^{-1} Z^{\prime} y
=\left(Z^{\prime} X\right)^{-1} Z^{\prime} (X\beta +\epsilon)
=\beta+\left(Z^{\prime} X\right)^{-1} Z^{\prime} \epsilon}
\end{align}
$$

$$
\begin{align}
p \lim \boldsymbol{{\hat{\beta}_{IV}}}
&=\boldsymbol{\beta+p \lim \left(\left(Z^{\prime} X\right)^{-1} Z^{\prime} \epsilon\right)} \\
&=\boldsymbol{\beta+\left(p \lim \left(\frac{1}{n} Z^{\prime} X\right)\right)^{-1} \operatorname{plim}\left(\frac{1}{n} Z^{\prime} \epsilon\right)
=\beta}
\end{align}
$$


:::: {.columns}

::: {.column width="40%"}


- Relevance guarantees

$$
\begin{align}
p \lim \left(\frac{1}{n} \boldsymbol{Z^{\prime} X}\right)
&=p \lim \left(\frac{1}{n} \sum z_{i} X_{i}^{\prime}\right) \\
&=E\left(Z_{i} X_{i}^{\prime}\right) \neq 0
\end{align}
$$

:::


::: {.column width="40%"}


- Exogeneity ensures

$$
\begin{align}
p \lim \left(\frac{1}{n} \boldsymbol{Z^{\prime} \epsilon}\right)
&=p \lim \left(\frac{1}{n} \sum Z_{i} \epsilon_{i}\right) \\
&=E\left(Z_{i} \epsilon_{i}\right)=0
\end{align}
$$
:::

::::

::: {.notes}
The IV estimator $\boldsymbol{\hat{\beta}_{IV}}$ is consistent, Since the two instrument conditions are guaranteed .For simple, the instrument relevance condition guarantees correlation between $Z_i$ and $X_i$, and the instrument exogeneity condition ensures uncorrelations between $Z_i$ and error term $\epsilon_i$.Thus, the IV estimator is consistent under probability limits.
:::


## IV: Inference

The natural estimator for$\sigma^{2}$ is

$$
\begin{align}
\hat{\sigma}_{I V}^{2}
=\frac{\sum e_{i}^{2}}{n-k}
=\frac{\boldsymbol{\left(y-X \hat{\beta}_{IV}\right)^{\prime}\left(y-X \hat{\beta}_{I V}\right)}}{n-k}
\end{align}
$$

can be shown to be consistent (not proved here).

Thus, we can perform hypothesis testing based on IV estimators$\boldsymbol{\hat{\beta}_{IV}}$.

::: {.notes}
For the purpose of inference, we need to get the consistent estimate of the variance of disturbance $\sigma^2$.The natural estimator for the variance of stochastic disturbance $\sigma^{2}$ is $\hat{\sigma}_{I V}^{2}$ as show below.And we don't try to prove it here.After that, we can perform hypothesis testing based on IV estimators $\boldsymbol{\hat{\beta}_{IV}}$.
:::


## IV: Choices of instruments

However, finding **valid instruments** is the most difficult part of IV estimation in practice.

> Good instruments need not only to be exogenous, but also need be highly correlative with the regressors.

> **Joke**: If you can find a valid instrumental variable, you can get  PhD from MIT.


Without a proof, we say that the **asymptotic variance** of$\hat{\beta}_{I V}$ is

$$
\begin{align}
\operatorname{Var}\left(\boldsymbol{\hat{\beta}_{I V}}\right)=\sigma^{2}\boldsymbol{\left(Z^{\prime} X\right)^{-1}\left(Z^{\prime} Z\right)\left(X^{\prime} Z\right)^{-1}}
\end{align}
$$

Where$\boldsymbol{X^{\prime} Z}$ is the matrix of covariances between instruments and regressors.

If such correlation is low,$\boldsymbol{X^{\prime} Z}$ will have elements close to zero and hence$\boldsymbol{\left(X^{\prime} Z\right)^{-1}}$ will have huge elements. Thus,$\operatorname{Var}\left(\boldsymbol{\hat{\beta}_{IV}}\right)$ will be very large.

::: {.notes}
So, what is the real challenge?___Without a proof, we say that the **asymptotic ('æsɪmp,toʊtɪk) variance** of $\hat{\beta}_{I V}$ takes following output.If such correlation is low, this covariances matrix $\boldsymbol{X^{\prime} Z}$ will have elements close to zero and hence its inverse matrix $\boldsymbol{\left(X^{\prime} Z\right)^{-1}}$ will have huge elements.And then, the asymptotic variance  of IV estimator $\operatorname{Var}\left(\boldsymbol{\hat{\beta}_{IV}}\right)$ will be very large.This means the IV estimation will be useless.
:::


## IV: Extension of choices

The **common strategy** is  to construct$\boldsymbol{Z}=(X_{ex}, X^{\ast})$ generally.

- Variables$X_{ex}$ in$\boldsymbol{X}=(X_{ex}, X_{en})$ are the assumed **exogenous** variables included in
model.

- Other exogenous variables$\boldsymbol{X^{\ast}}$  are "close" to the model, but do not enter the model explicitly.



If$X$ can be shown to be exogenous, then$X=Z$ and Gauss-Markov efficiency is recovered.

> **Instrumental Variable Estimators** [do not]{.red} have any efficiency properties .

> We can only talk about **relative efficiency**. It means that we can only choose the optimal set of instruments. Such that our estimator is the best we can obtain within all the class of possible instrumental variable estimators.

::: {.notes}
So, it is really big challenge to find the valid instruments and control the low level of asymptotic variance.While we can find a solution if we are luckly.___We should remind one other thing.___So we will ask that with what conditions we can excute this common strategy?
:::


## IV: Many available instruments

In case there are more **instruments** than **[en]{.red}dogenous variables** (**over-identification**), we want to choose those instruments that have the highest correlation with$X$ and hence give the lowest possible variance.

The best choice is obtained by using the fitted values of an OLS regression of each column of$\boldsymbol{X}$ on all instruments$\boldsymbol{Z}$, that is (after running$k$ regressions, one for each column of$\boldsymbol{X}$)

$$
\begin{align}
\boldsymbol{\hat{X}=Z\left(Z^{\prime} Z\right)^{-1} Z^{\prime} X=ZF}
\end{align}
$$

We now use$\boldsymbol{\hat{X}}$ as instrument, which is$\boldsymbol{\hat{\beta}_{I V}=\left(\hat{X}^{\prime} X\right)^{-1} \hat{X}^{\prime} y}$

We notice that (try to prove this):

$$
\begin{align}
\boldsymbol{\hat{\beta}_{I V} =\left(\hat{X}^{\prime} X\right)^{-1} \hat{X}^{\prime} y=\left(X^{\prime} Z\left(Z^{\prime} Z\right)^{-1} Z^{\prime} X\right)^{-1} X^{\prime} Z\left(Z^{\prime} Z\right)^{-1} Z^{\prime} y }
\end{align}
$$

::: {.notes}
The clue is show below, and you may try to prove it.The last results show that when we have more instruments than endogenous variables $\boldsymbol{\hat{\beta}_{IV}}$ can be computed in 2 steps (2SLS).
:::


## IV: solution with omitted variables

### Revall the wage case

Let's go back to our example of the wage equation. Assume we are modeling wage as a function of **education** and **ability**.

$$
Wage_{i}=\beta_{0}+\beta_{1} Edu_{i}+\beta_{2} Abl_{i}+\epsilon_{i}
$$

However, individual's ability is clearly something that is not observed or measured and hence cannot be included in the model. since, ability is not included in the model it is included in the error term.

$$
Wage_{i}=\beta_{0}+\beta_{1} Edu_{i}+e_{i}, \quad \text {where} \quad e_{i}=\beta_{2} Abl_{i}+\epsilon_{i}
$$

The problem is that ability not only affects wages but the more able individuals may spend more years in school, causing a positive correlation between the error term and education,$\operatorname{cov}(Edu_i, e_i)>0$ .

Thus,$Educ$ is an **endogenous variable**.

##

### Conditions for valid instruments

If we can find a valid instrument for `Educ` we can estimate$\beta_{1}$ using IV method. Suppose that we have a variable$Z$ that satisfies the following conditions

:::: {.columns}

::: {.column width="40%"}


- $Z$ does [not]{.red} directly affect wages


- $Z$ is [uncorrelated]{.red} with$e$ (exogeneity),  i.e.

$$
\operatorname{Cov}(e, z)=0  \quad \text{(4)}
$$

> since$e_{i}=\beta_{2} Abl_{i}+\epsilon_{i}$,$Z$ must be uncorrelated with ability.
:::


::: {.column width="40%"}

-$Z$ is (at least partially) [correlated]{.red} with the endogenous variable,i.e. Education (relevance),

$$
\operatorname{Cov}(Z, Edu) \neq 0 \quad \text{(5)}
$$

> Such condition can be tested($\alpha_2$) by using a simple regression$:

$$
Edu_{i}=\alpha_{1}+\alpha_{2} Z_{i}+u_{i}
$$

:::

::::

Then,$Z$ is a valid instrument for$Educ_i$. We showed earlier that the IV estimator of$\beta_{1}$ is consistent.

::: {.notes}
And the instrumental variable $Z$ should satisfies both relevance condition and exogenous condition as we have mentioned before.So, what are these appropriate instruments for education?
:::

##

### Available options of instruments

Several economists have used **family background variables** as IVs for education.

- For example, **mother's education** is positively correlated with child's education, so it may satisfies condition of **Relevance**.

- Also, **father's education** is positively correlated with child's education, and it may satisfies condition of **Relevance**.


::: {.incremental}

> The problem is that mother's or father 's education might also be correlated with child's ability(genetic inherited), in which case the condition of  **Exogeneity** fails.

:::


::: {.incremental}

- Another IV for education that has been used by economists is the number of **siblings** while growing up.

> Typically, having more siblings is associated with lower average levels of education and it should be uncorrelated with innate ability.


::: {.notes}
Anyway, it is worth to try using mother education as the instrument.
:::

:::

##

### proxy variable model


Also, let us consider the case with measurement error in the independent variable.

For some reason, a variable$X_i$ cannot be directly observed (data is not available), so it is common to find a **imperfect measure**(the **proxy variable**) for it.

The **"real model"** for wage is assumed to be:


$$
\begin {align}
\log (Wage_i)=\beta_{0}+\beta_{1} Edu_i+\beta_{2} Abl_i+u_i
\end {align}
$$


However, since the ability variable ($Abl_i$) cannot be observed, we often replace it with the **IQ level** variable ($IQ_i$) and construct the following **"proxy variable model"** :


$$
\begin {align}
\log (Wage_i)=\beta_{0}+\beta_{1} Edu_i+\beta_{2} IQ_i+u_i^{\ast}
\end {align}
$$

At this point, **intelligence level**$IQ_i$ can be considered as a potential instrument for **ability**$abil_i$.

::: {.notes}
NOT show because this case may cause confuse.Now we have got some available instruments, and the following question is how to process the IV estimation.In the next section, we will talk about Two-stage least squares method.
:::


# 17.4 Two-stage least squares method{#TSLS .monash-bg-blue .mcenter}

::: {.notes}
In this section, we will discuss how to perform Two-stage least squares estimation procedure.
:::

## Two-stage least squares

### glance

When we have **more** instruments than endogenous variables,$\boldsymbol{\hat{\beta}_{IV}}$ can be computed in 2 steps:

- **Step 1**: Regress each column of$X$ on all the instruments ($Z$ ,in matrix form ). For each column of$X$, get the fitted values and combine them into the matrix$\hat{X}$.

- **Step 2**: Regress${Y}$ on$\hat{X}$

And, this procedure is named **two-stage least squares** or **2SLS** or **TSLS**.


##

### indentification


Consider the model setting

$$
\begin{align}
Y_{i}=\beta_{0}+\sum_{j=1}^{k} \beta_{j} X_{j i}+\sum_{s=1}^{r} \beta_{k+s} W_{ri}+\epsilon_{i}
\end{align}
$$

where$\left(X_{1 i}, \ldots, X_{k i}\right)$ are **endogenous regressors**,$\left(W_{1 i}, \ldots, W_{r i}\right)$ are **exogenous regressors** and there are$m$ **instrumental variables**$\left(Z_{1 i}, \ldots, Z_{m i}\right)$ satisfying instrument relevance and instrument exogeneity conditions.

- When$m=k$ ,the coefficients are **exactly identified**.

- When$m>k$ ,the coefficients are **overidentified**.

- When$m<k$, the coefficients are **underidentified**.

- Finnaly, coefficients can be identified only when$m \geq k$.


::: {.notes}
Because the model identification is the most important thing before applying the estimation procedure.So, We should overview the model status explicitly.We will denote the general model format as below.
:::

##

### The procedure


- **Stage 1**: Regress$X_{1i}$ on constant, all the instruments$\left(Z_{1i}, \ldots, Z_{m i}\right)$ and all exogenous regressors$\left(W_{1i}, \ldots, W_{ri}\right)$ using OLS and obtain the fitted values$\hat{X}_{1 i}$ . Repeat this to get$\left(\hat{X}_{1 i}, \ldots, \hat{X}_{k i}\right)$

- **Stage 2**: Regress$Y_{i}$ on constant,$\left(\hat{X}_{1 i}, \ldots, \hat{X}_{k i}\right)$ and $\left(W_{1 i}, \ldots, W_{r i}\right)$ usingOLS to obtain$\left(\hat{\beta}_{0}^{IV}, \hat{\beta}_{1}^{IV}, \ldots, \hat{\beta}_{k+r}^{IV}\right)$

::: {.notes}
So, in case with “exactly identification” and “over-identification”, we can go ahead with the **Two-Stage Least Squares** as a “whole” solution for IV estimation.
:::

##

### The solutions

We can conduct the **2SLS** procedure with following two solutions:

- use the **"Step-by-Step solution"** methods without variance correction.

- use the  **"Integrated solution"** with variance correction.


.notes[

**Notice**:

DO NOT use **"Step-by-Step solution"** solution in your paper! It is only for teaching purpose here.

In `R` ecosystem, we have two packages to execute the  **Integrated solution**:

- We can use `systemfit` package function `systemfit::systemfit()`.

- Or we may use `ARE` package function `ARE::ivreg()`.

]

::: {.notes}
Let us apply these solutions to the empirical wage examples.
:::

## TSLS: Step-by-step solution

### Stage 1: model

First, let's try to use $matheduc$ as instrument of endogenous variable $educ$.

**Stage 1 of 2SLS**: with mother education as instrument

we can obtain the fitted variable $\widehat{educ}$ by conduct the following **step 1** OLS regression

$$
\begin{align}
\widehat{educ} = \hat{\gamma}_1 +\hat{\gamma}_2exper + \hat{\gamma}_3expersq +\hat{\gamma}_4mothereduc
\end{align}
$$

::: {.notes}
Again, let us do the demo of two-stage least squares procedure based on the wage example.In the firs stage, we can obtain the fitted variable $\widehat{educ}$ by conduct the following OLS regression.
:::

##

### Stage 1: OLS estimate

Here we obtain the OLS results of **Stage 1 of 2SLS**:



::: {.cell layout-align="center"}

```{.r .cell-code}
# modle setting
mod_step1 <- formula(educ~exper + expersq + motheduc)  
# OLS estimation
ols_step1 <- lm(formula = mod_step1, data = mroz)  
```
:::

::: {.cell layout-align="center"}
$$
\begin{alignedat}{999}
\begin{split}
&\widehat{educ}=&&+9.78&&+0.05exper_i&&-0.00expersq_i&&+0.27motheduc_i\\ 
&(s)&&(0.4239)&&(0.0417)&&(0.0012)&&(0.0311)\\ 
&(t)&&(+23.06)&&(+1.17)&&(-1.03)&&(+8.60)\\ 
&(fit)&&R^2=0.1527&&\bar{R}^2=0.1467 && &&\\ 
&(Ftest)&&F^*=25.47&&p=0.0000 && &&
\end{split}
\end{alignedat}
$$
:::



The t -value for coefficient of $mothereduc$  is so large (larger than 2), indicating a strong correlation between this instrument and the endogenous variable  $educ$  even after controlling for other variables.

::: {.notes}
we should note that the t-value for coefficient of $mothereduc$  is larger than 2. and the t test is significant. This  means there is a strong correlation between the instrument $motheduc$ and the endogenous variable  $educ$  even when we control all other variables.
:::

##

### Stage 1: OLS predicted values

Along with the regression of **Stage 1 of 2SLS**, we will extract the fitted value $\widehat{educ}$ and add them into new data set.



::: {.cell layout-align="center"}

```{.r .cell-code}
# add fitted educ to data set
mroz_add <- mroz %>% mutate(educHat = fitted(ols_step1)) 
```
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-ef587b62bffc5069e60b" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-ef587b62bffc5069e60b">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284","285","286","287","288","289","290","291","292","293","294","295","296","297","298","299","300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345","346","347","348","349","350","351","352","353","354","355","356","357","358","359","360","361","362","363","364","365","366","367","368","369","370","371","372","373","374","375","376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423","424","425","426","427","428"],[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428],[1.210153698921204,0.3285121023654938,1.514137744903564,0.09212332218885422,1.524272203445435,1.556480050086975,2.120259523391724,2.059634208679199,0.7543363571166992,1.544899344444275,1.401921629905701,1.524272203445435,0.7339532375335693,0.8183690905570984,1.302831172943115,0.2980283796787262,1.167609572410583,1.643839359283447,0.6931471824645996,2.021931648254395,1.254247546195984,1.272957682609558,1.178655028343201,1.178655028343201,0.7675586938858032,1.331811785697937,1.386294364929199,1.553269624710083,1.981814861297607,1.769360423088074,0.430807888507843,0.8997548222541809,1.766629695892334,1.272957682609558,1.336788892745972,0.9017048478126526,0.8651236891746521,1.511847138404846,1.72602915763855,2.683142423629761,0.9852942824363708,1.365938544273376,0.9450336694717407,1.512376189231873,0.6931471824645996,1.244788408279419,0.7011649012565613,1.519863247871399,0.8209685683250427,0.9698315262794495,0.828508198261261,0.09430964291095734,0.1625438928604126,0.4700036346912384,0.6292484402656555,1.397160172462463,2.265443801879883,2.084541082382202,1.525838851928711,0.762160062789917,1.48160457611084,1.262826442718506,0.9996755719184875,1.832581520080566,2.479307651519775,1.279015302658081,1.937935590744019,1.070452809333801,1.12392258644104,1.321755886077881,1.744999766349792,1.301743626594543,1.641866445541382,2.107020139694214,1.46706759929657,1.605811357498169,-1.029739379882812,1.08768618106842,0,0.9382086992263794,-0.1505903750658035,0,1.073670506477356,1.265848398208618,0.4863689839839935,2.120259523391724,1.129852533340454,0.9932518005371094,1.658627986907959,0.3474121987819672,1.568324208259583,0.5108456015586853,0.1148454323410988,-0.6931471824645996,-0.3364522755146027,1.028225541114807,1.58068859577179,0.5558946132659912,0.9014207124710083,0.8843045830726624,0.4282045960426331,1.058415055274963,0.8783395886421204,1.654908299446106,1.321755886077881,0.3285121023654938,1.386294364929199,1.172884583473206,1.224187135696411,0.2876570820808411,2.23026180267334,1.504077434539795,1.531152009963989,1.375157594680786,1.760268807411194,-0.6931471824645996,1.406489133834839,1.791759490966797,1.299292087554932,1.351003885269165,1.016280889511108,1.075343608856201,1.478964686393738,1.689486742019653,2.288597822189331,-1.822631120681763,-0.9607651829719543,1.290994167327881,0.8648711442947388,1.540452122688293,0.6162121295928955,1.648658633232117,1.193498134613037,2.143976211547852,0.7244035601615906,0.9416075348854065,0.7827593684196472,1.832581520080566,1.203962802886963,1.491644859313965,1.892132639884949,2.130894899368286,1.48060405254364,0.8943313360214233,0.2025325447320938,0.4855078160762787,1.098612308502197,1.553269624710083,0.1215979680418968,2.001804351806641,1.495036602020264,0.9052298069000244,0.6325475573539734,1.386294364929199,2.102913856506348,1.959643959999084,0.5108456015586853,1.236923933029175,1.443312525749207,1.021659255027771,0.6361534595489502,1.616453289985657,0.2231435477733612,1.049807071685791,1.415051937103271,0.5753766298294067,2.60668158531189,1.517914533615112,0.7550415992736816,1.094972372055054,0.9421143531799316,1.724942803382874,1.031546115875244,0.474369078874588,0.8109301924705505,0.7092666029930115,1.710549473762512,0.4602688848972321,1.331811785697937,1.098612308502197,2.157998561859131,1.437581300735474,1.544899344444275,1.41059672832489,3.218875885009766,0.9681618809700012,1.791759490966797,1.688729524612427,-0.4091719686985016,0.2231435477733612,0.8221558332443237,1.24170196056366,1.427124381065369,1.497097492218018,0.5596157908439636,1.300028204917908,1.884429812431335,0.9555113911628723,1.582087278366089,1.755614042282104,1.513103246688843,2.251891613006592,2.364432334899902,0.1053504794836044,1.399728775024414,0.988462507724762,1.090647339820862,1.154614448547363,1.266947627067566,2.885191679000854,1.228880047798157,1.203962802886963,1.357380270957947,0.8377236127853394,0.5369611382484436,0.7487238049507141,2.295872688293457,1.107803225517273,0.6208452582359314,-2.054163694381714,1.892012000083923,1.729724526405334,0.4693784117698669,0.9808416962623596,2.069492340087891,1.675188183784485,1.386294364929199,1.799214959144592,1.832581520080566,1.090647339820862,1.443123579025269,1.250360131263733,1.602312564849854,1.018558502197266,1.297053217887878,1.685194492340088,-0.4209848940372467,1.562094688415527,2.146527528762817,2.347462892532349,0.9698315262794495,1.924146413803101,1.62672758102417,-0.03926072642207146,1.460148692131042,1.955393552780151,0.9263598918914795,2.066191673278809,1.422843217849731,2.10103178024292,2.261461019515991,0.7013137936592102,2.031012535095215,1.162369251251221,0.4700036346912384,1.41059672832489,0.3930551111698151,1.290994167327881,0,0.9571254849433899,0.5596157908439636,1.568615913391113,1.710187911987305,1.41059672832489,0.2231435477733612,0.5108456015586853,1.332392454147339,0.8601858615875244,2.322779893875122,1.91959547996521,1.976106762886047,0.8954347372055054,0.18123759329319,0.4953058362007141,0.5777924060821533,1.07881772518158,1.603198528289795,0.6208452582359314,2.083894014358521,1.379169106483459,1.112383723258972,1.067121624946594,1.118806958198547,1.588541030883789,1.390311241149902,1.714806437492371,0.2010615319013596,0.9872710108757019,0.9835006594657898,2.233170747756958,1.143617510795593,-0.6113829016685486,2.153052091598511,1.299837350845337,0.8409204483032227,1.058484435081482,1.152658462524414,1.293575882911682,1.832581520080566,2.327180147171021,1.166146278381348,2.034993171691895,0.6792510747909546,1.547136902809143,0.7530185580253601,0.8472836017608643,0.8711259961128235,0.2282504737377167,0.08965782821178436,1.321755886077881,1.196101903915405,1.636118769645691,1.892012000083923,1.518308997154236,2.472159147262573,1.321755886077881,1.473641037940979,1.369478821754456,1.203962802886963,1.198729157447815,1.270209908485413,0.4700036346912384,0.7999816536903381,1.565945625305176,1.758978009223938,0.858025848865509,0.6931471824645996,0.6418538689613342,1.633740186691284,1.703747630119324,1.844004034996033,1.966118812561035,0.8649974465370178,0.9333052039146423,0.7792331576347351,0.9555113911628723,1.316247344017029,1.475906491279602,1.491397261619568,1.455750465393066,0.5108456015586853,1.180438041687012,1.688489437103271,0.7907274961471558,1.401798605918884,-0.4335560202598572,1.683171510696411,-1.766676664352417,3.155595064163208,2.259521007537842,1.306926369667053,0.7984976768493652,0.5590441823005676,0.1479026228189468,1.944494843482971,1.378337860107422,3.064745187759399,-0.7419173121452332,0.7657003998756409,0.619392991065979,1.465452075004578,2.18925952911377,1.021659255027771,0.9770094752311707,0.9162907600402832,2.905096054077148,-0.1996711939573288,0.6931471824645996,2.733392953872681,1.868334650993347,2.120259523391724,1.515193223953247,0.9146093130111694,1.499556064605713,0.803077220916748,0.7280316352844238,0.5164099931716919,1.22644829750061,0.9162907600402832,1.376471281051636,1.828974962234497,1.368283152580261,1.064710736274719,1.406489133834839,1.047318935394287,1.948093414306641,1.078001379966736,0.6539384722709656,1.927891612052917,1.361027836799622,0.6931471824645996,1.604686617851257,0.1839036494493484,3.113515377044678,1.926829218864441,1.2701256275177,0.6826927065849304,1.68106997013092,0.5562959909439087,1.628220438957214,0.9162907600402832,1.341558456420898,0,1.122231245040894,0.5401707887649536,1.391505718231201,1.697173953056335,3.218875885009766,0.871167778968811,1.167329549789429,1.216987729072571,0.5753766298294067,1.151615738868713,0.9942512512207031,0.5263249278068542,-1.543182134628296,1.91204309463501,0.554287314414978,0.9162907600402832,1.500939130783081,0.9446837902069092,1.241268634796143,1.564984321594238,0.8380264639854431,1.668857097625732,1.769428610801697,1.22644829750061,1.406489133834839],[12,12,12,12,14,12,16,12,12,12,12,11,12,12,10,11,12,12,12,12,16,12,13,12,12,17,12,12,17,12,11,16,13,12,16,11,12,10,14,17,12,12,16,12,12,12,16,12,12,12,12,12,12,8,10,16,14,17,14,12,14,12,8,12,12,8,17,12,12,12,12,12,9,10,12,12,12,17,15,12,6,14,12,14,9,17,13,9,15,12,12,12,12,12,12,12,12,13,12,13,12,12,12,16,12,13,11,12,12,12,17,14,16,17,12,11,12,12,17,10,13,11,12,16,17,12,16,12,16,8,12,12,12,13,11,12,12,14,12,12,12,17,14,12,9,12,12,12,14,16,17,15,12,16,17,17,12,16,13,12,11,16,14,16,12,9,17,14,12,12,11,12,12,10,12,5,17,11,12,12,14,11,12,14,12,10,16,13,12,12,12,11,12,9,13,12,12,12,13,16,12,16,17,12,12,9,12,12,13,12,12,12,12,10,12,16,12,11,12,10,12,12,12,12,16,17,12,17,12,12,12,8,12,13,12,12,8,12,17,17,12,13,12,12,12,12,9,10,12,16,13,8,16,13,12,11,13,12,12,10,12,17,15,16,10,11,12,12,14,16,14,8,7,12,12,14,12,12,12,14,16,12,12,12,13,13,10,12,12,12,12,14,17,10,9,12,12,16,12,17,12,17,11,16,11,13,11,8,11,12,10,17,12,12,17,14,12,12,12,12,12,12,9,10,12,12,12,12,12,17,12,17,12,10,12,12,12,12,12,12,16,13,13,12,16,17,12,14,12,17,12,14,12,12,17,16,16,12,9,12,12,16,14,12,12,11,12,16,17,17,14,12,14,12,10,12,13,16,12,7,16,14,12,10,12,16,10,12,14,12,6,15,12,17,14,13,6,16,14,15,14,8,14,12,12,12,12,12,12,8,12,17,12,12,14,13,17,8,12,11,12,12,17,10,12,13,12,12],[14,5,15,6,7,33,11,35,24,21,15,14,0,14,6,9,20,6,23,9,5,11,18,15,4,21,31,9,7,7,32,11,16,14,27,0,17,28,24,11,1,14,6,10,6,4,10,22,16,6,12,32,15,17,34,9,37,10,35,6,19,10,11,15,12,12,14,11,9,24,12,13,29,11,13,19,2,24,9,6,22,30,10,6,29,29,36,19,8,13,16,11,15,6,13,22,24,2,6,2,2,14,9,11,9,6,19,26,19,3,7,28,13,9,15,20,29,9,1,8,19,23,3,13,8,17,4,15,11,7,0,0,10,8,2,4,6,18,3,22,33,28,23,27,11,6,11,14,17,17,14,11,7,8,6,8,4,25,24,11,19,9,19,14,22,6,23,15,6,11,2,22,10,14,12,9,13,18,8,11,9,9,14,9,2,12,15,11,7,9,19,11,8,13,4,7,19,14,14,3,9,7,7,14,29,19,14,16,10,12,24,6,9,14,26,7,4,15,23,1,29,9,6,11,17,6,7,2,24,4,11,25,11,2,19,7,2,20,10,19,17,12,11,6,10,4,2,13,21,9,4,2,19,4,9,14,6,24,1,13,3,10,16,9,19,4,10,5,7,3,38,16,13,1,7,15,10,2,19,25,25,7,15,11,25,19,4,14,19,18,14,11,4,29,21,24,19,31,28,15,27,13,4,10,8,4,18,3,11,8,10,33,19,35,21,7,18,4,12,16,14,3,1,27,12,6,9,2,6,9,16,22,26,11,11,15,13,6,20,17,8,13,15,14,14,6,24,10,2,9,23,12,8,16,10,7,19,2,9,14,9,16,7,6,22,9,9,14,17,12,13,8,10,16,1,6,4,8,4,15,7,14,16,15,23,19,4,12,12,25,14,14,11,7,18,4,37,13,14,17,5,2,0,3,21,20,19,4,19,11,14,8,13,24,1,1,3,4,21,10,13,9,14,2,21,22,14,7],[196,25,225,36,49,1089,121,1225,576,441,225,196,0,196,36,81,400,36,529,81,25,121,324,225,16,441,961,81,49,49,1024,121,256,196,729,0,289,784,576,121,1,196,36,100,36,16,100,484,256,36,144,1024,225,289,1156,81,1369,100,1225,36,361,100,121,225,144,144,196,121,81,576,144,169,841,121,169,361,4,576,81,36,484,900,100,36,841,841,1296,361,64,169,256,121,225,36,169,484,576,4,36,4,4,196,81,121,81,36,361,676,361,9,49,784,169,81,225,400,841,81,1,64,361,529,9,169,64,289,16,225,121,49,0,0,100,64,4,16,36,324,9,484,1089,784,529,729,121,36,121,196,289,289,196,121,49,64,36,64,16,625,576,121,361,81,361,196,484,36,529,225,36,121,4,484,100,196,144,81,169,324,64,121,81,81,196,81,4,144,225,121,49,81,361,121,64,169,16,49,361,196,196,9,81,49,49,196,841,361,196,256,100,144,576,36,81,196,676,49,16,225,529,1,841,81,36,121,289,36,49,4,576,16,121,625,121,4,361,49,4,400,100,361,289,144,121,36,100,16,4,169,441,81,16,4,361,16,81,196,36,576,1,169,9,100,256,81,361,16,100,25,49,9,1444,256,169,1,49,225,100,4,361,625,625,49,225,121,625,361,16,196,361,324,196,121,16,841,441,576,361,961,784,225,729,169,16,100,64,16,324,9,121,64,100,1089,361,1225,441,49,324,16,144,256,196,9,1,729,144,36,81,4,36,81,256,484,676,121,121,225,169,36,400,289,64,169,225,196,196,36,576,100,4,81,529,144,64,256,100,49,361,4,81,196,81,256,49,36,484,81,81,196,289,144,169,64,100,256,1,36,16,64,16,225,49,196,256,225,529,361,16,144,144,625,196,196,121,49,324,16,1369,169,196,289,25,4,0,9,441,400,361,16,361,121,196,64,169,576,1,1,9,16,441,100,169,81,196,4,441,484,196,49],[7,7,7,7,14,7,7,3,7,7,3,7,16,10,7,10,7,12,7,7,16,10,3,7,7,14,7,7,12,12,7,3,10,14,12,3,3,3,7,17,12,9,16,3,7,7,16,10,7,7,7,3,7,7,3,12,7,17,7,7,3,12,7,7,7,12,16,7,7,7,12,10,9,0,10,14,7,3,12,12,7,17,3,7,7,12,7,7,12,10,0,12,10,7,7,7,3,12,7,12,7,7,10,14,7,12,7,7,10,7,12,7,7,17,7,7,7,10,10,12,7,12,7,10,7,10,7,7,7,7,7,7,16,12,7,3,7,7,7,12,7,12,12,14,7,7,7,12,12,14,10,12,7,16,7,17,3,10,9,7,3,16,12,7,7,7,12,3,7,7,7,7,10,10,7,12,17,10,7,7,12,7,12,7,7,7,7,14,7,12,7,7,12,3,7,12,12,7,7,14,12,17,17,7,7,10,7,7,7,3,0,7,12,7,7,12,7,7,12,7,7,3,7,7,10,12,7,12,7,7,7,7,12,7,7,7,7,7,14,17,7,10,7,7,12,7,7,9,7,14,7,3,16,3,16,7,16,12,7,7,12,12,12,16,7,9,7,12,12,10,7,12,7,7,3,10,17,7,3,3,12,7,7,7,10,7,10,7,9,9,12,12,12,7,9,12,7,12,7,12,12,14,7,14,10,12,7,7,12,7,7,12,12,12,12,14,10,7,7,7,7,7,7,7,7,7,12,12,7,14,10,12,7,7,12,7,10,12,10,7,14,7,12,12,7,16,0,12,7,17,7,12,3,7,14,7,12,12,7,7,14,7,12,12,7,7,7,7,3,12,7,7,14,10,10,10,10,12,3,7,16,7,7,0,7,7,10,7,12,7,7,7,7,16,12,10,7,7,16,7,7,7,12,10,10,10,7,10,12,12,7,17,12,16,10,16,7,7,9,3,7,7,7,7,7,7,16,12],[12,7,12,7,12,14,14,3,7,7,12,14,16,10,7,16,10,12,7,12,10,12,7,7,12,16,3,3,12,12,7,3,12,7,12,10,3,10,7,14,12,9,14,3,12,12,14,10,7,12,7,7,12,7,7,12,7,17,17,12,14,12,7,7,7,12,12,12,7,12,12,10,7,0,7,12,7,3,10,7,12,12,7,7,7,7,7,7,7,10,7,12,10,12,7,7,7,14,7,12,12,7,7,14,12,10,7,7,7,7,12,7,12,10,10,7,7,7,12,7,7,12,14,12,7,10,7,7,12,10,7,7,12,10,7,12,7,7,7,7,3,12,16,7,3,12,7,12,12,16,12,12,7,14,7,10,7,14,7,7,12,12,17,7,7,3,12,7,7,7,3,7,10,10,12,7,14,10,7,7,10,12,12,7,7,7,12,7,12,12,12,10,12,10,12,12,12,7,12,12,12,12,16,7,16,7,7,10,12,10,0,7,12,12,10,12,3,7,12,10,7,7,7,7,12,12,7,12,7,10,10,7,12,17,7,7,7,7,12,14,7,12,7,7,16,7,10,12,7,16,10,3,16,7,12,7,7,7,12,12,7,10,14,16,7,10,7,14,14,12,7,7,3,7,7,7,12,10,7,3,12,7,12,7,10,7,0,7,10,9,12,12,12,3,9,12,12,14,7,12,12,12,7,12,10,12,7,7,3,12,7,16,12,12,7,14,7,7,12,10,7,3,7,7,10,7,7,12,12,12,10,14,7,7,14,10,10,7,7,7,14,7,12,14,14,14,0,16,7,12,7,10,7,10,10,14,7,12,7,7,12,14,12,7,7,7,12,16,3,16,7,16,7,10,10,10,10,12,10,7,16,7,7,7,7,7,10,10,12,7,7,7,7,14,14,7,7,7,16,12,7,7,12,12,12,10,3,12,12,12,7,16,12,10,10,12,7,7,7,7,7,7,7,7,7,7,12,12],[13.42036466548422,11.86121922986205,13.43207528132518,11.89598901522093,13.26665071609442,13.74012376382735,13.90524165646517,10.71902302758776,12.08372093085125,12.1100802020396,13.43207528132518,13.95574628366802,14.05815563570166,12.88498304729926,11.89598901522094,14.39414287344911,12.91681479315288,13.23444306068189,12.09506948452707,13.32337963708035,12.66429165713861,13.36986003828079,12.1133803037106,12.09362123586423,13.16234136001772,14.51929748386931,10.86177818025326,10.91416235525064,13.26665071609442,13.26665071609442,11.90069582337521,10.96064275645108,13.44122376722034,12.08191062002269,13.37275653560648,12.45201078114852,11.03859284133942,12.81577784396079,12.08372093085125,13.90524165646517,13.03497283442366,12.61729223820707,13.76982467886627,10.93868362082405,13.23444306068189,13.16234136001772,13.88328252083813,12.9069283355331,12.10276972175939,13.23444306068189,12.05080299850049,11.90069582337521,13.43207528132518,12.10935607770818,11.82931824704246,13.32337963708035,11.70303590794554,14.6863549481147,14.46669435487842,13.23444306068189,13.98867806341197,13.34790090265376,12.03140599281983,12.09362123586423,12.05080299850049,13.38925704396144,13.42036466548364,13.36986003828079,11.9849255916194,13.4221749763122,13.38925704396144,12.87071030151135,11.98854621327652,10.15757032917451,12.06763787423478,13.45329644522759,11.74153709410711,11.01295769448249,12.78799801889597,11.89598901522094,13.44230995371748,13.30027892538341,12.0094468571928,11.89598901522094,11.98854621327652,11.98854621327652,11.74769215092422,12.11484239976664,11.95784219609962,12.87071030151135,12.10276972175939,13.36986003828079,12.8966936631408,13.23444306068189,12.06763787423478,12.10385590825653,12.08372093085125,13.61537275775244,11.89598901522094,13.07999113956806,13.07999113956806,12.08191062002269,11.9849255916194,13.90524165646517,13.32337963708035,12.69906144249751,12.11484239976664,12.05333743366048,12.11484239976664,11.78399326930513,13.26665071609442,12.01270541668422,13.40609191969573,12.78799801889597,12.8966936631408,12.11374236587631,11.98854621327652,11.9849255916194,13.03497283442366,11.95784219609962,12.11484239976664,13.43352352998803,13.65782893295046,13.40609191969573,11.95784219609962,12.91242850498475,11.82388731455677,12.09362123586423,13.36986003828079,12.73126909791004,11.64893835387195,11.64893835387195,13.34790090265376,12.76091462337619,11.74153709410711,13.16234136001772,11.89598901522094,12.1133803037106,11.78399326930513,12.10385590825653,10.79552486381326,13.35115946214517,14.50428676635678,12.03430249014554,10.96064275645108,13.23444306068189,12.03140599281984,13.42036466548364,13.44781012316913,14.51857335953789,13.42036466548364,13.36986003828079,11.92819667063347,13.83167785974495,11.89598901522094,12.76091462337619,11.82388731455677,13.94364591087438,12.08372093085125,12.03140599281984,13.45329644522759,13.32337963708035,14.79175049068854,12.08191062002269,12.10385590825653,10.82522577885218,13.43352352998803,12.09362123586423,11.89598901522094,12.03140599281984,10.67077385773835,12.10385590825653,12.81251928446937,12.88498304729926,13.38925704396144,11.9849255916194,13.94147353788011,12.91645273098717,11.95784219609962,12.03140599281984,12.78799801889597,13.32337963708035,13.42036466548364,11.9849255916194,11.74153709410711,12.05080299850049,13.43207528132518,12.03140599281984,13.26665071609442,13.32337963708035,13.45329644522759,12.83447842009641,13.29629624156057,12.87071030151135,13.16234136001772,13.26665071609442,13.45329644522759,12.08191062002269,13.42036466548364,13.12244731476608,13.32337963708035,13.26665071609442,14.33741395246318,12.08191062002269,14.39776349510623,12.11484239976664,12.08191062002269,12.90584214903596,13.34790090265376,12.85387542577706,10.20988526720592,11.89598901522094,13.32337963708035,13.42036466548364,12.85640986093705,13.26665071609442,10.75312407818801,12.09362123586423,13.43352352998803,12.49959121623928,11.98854621327652,11.9849255916194,11.89598901522094,12.03140599281984,13.44781012316913,13.23444306068189,11.92819667063347,13.07999113956806,12.08372093085125,12.62695974183334,12.83447842009641,12.06981024722905,13.36986003828079,14.41844518502901,12.11484239976664,11.92819667063347,11.74153709410711,12.11374236587631,13.34790090265376,13.98867806341197,12.10935607770818,13.38925704396144,12.03140599281984,11.89598901522094,14.41866413902251,11.82388731455677,12.54460952138368,13.40609191969573,12.1100802020396,14.39414287344911,12.62695974183334,10.67077385773835,14.52405968159635,11.82388731455677,13.32337963708035,12.08191062002269,11.89598901522094,12.08372093085125,13.03497283442366,13.40609191969573,11.78399326930513,12.81251928446937,13.97660538540472,14.39414287344911,12.11484239976664,12.62695974183334,12.0094468571928,13.73505489350737,13.8020323342788,13.12244731476608,11.65581753502048,12.10276972175939,10.99687463786602,11.69651878896271,11.92819667063347,12.09362123586423,13.34790090265376,12.54460952138368,12.11484239976664,10.99904701086029,13.40826429269,11.92819667063347,13.43207528132518,12.03140599281984,12.87288267450562,12.11484239976664,9.950051650911444,12.08191062002269,12.91791482704321,12.64876192189498,13.42036466548364,13.36986003828079,13.16234136001772,10.91778297690776,12.64546182022398,13.4221749763122,13.45329644522759,13.80637708026735,12.01270541668422,13.43207528132518,13.37275653560648,13.40609191969573,11.82388731455677,13.34790090265376,12.76091462337619,13.16234136001772,12.1133803037106,11.78399326930513,10.96064275645108,13.29629624156057,12.00944685719281,14.27550538201173,13.45329644522759,13.12824030941747,12.1100802020396,13.8020323342788,12.1133803037106,11.82388731455677,13.38925704396144,12.90584214903596,12.08191062002269,10.71323003293637,11.69651878896271,12.03430249014554,12.85387542577706,11.89598901522094,11.9849255916194,13.07999113956806,13.23444306068189,13.32337963708035,12.90584214903596,13.97769157190186,12.05333743366048,12.03140599281984,13.90524165646517,12.8966936631408,12.87071030151135,11.89598901522094,12.11374236587631,12.10935607770818,13.83167785974495,12.06763787423478,13.43207528132518,13.95574628366802,13.95574628366802,13.76982467886627,10.20988526720592,14.41866413902251,11.74153709410711,13.32337963708035,12.09506948452707,12.85387542577706,11.95784219609962,12.90584214903596,12.81251928446937,13.8020323342788,12.11484239976664,13.07999113956806,11.9849255916194,12.08191062002269,13.32337963708035,13.97660538540472,13.26665071609442,11.89598901522094,12.10385590825653,11.9849255916194,13.32337963708035,14.4911279018524,11.03859284133942,14.4600202803302,12.06763787423478,14.36705947792933,12.0094468571928,12.90584214903596,12.49959121623928,12.69906144249751,12.62695974183334,13.29629624156057,12.62695974183334,12.09362123586423,14.33741395246318,12.08191062002269,12.10276972175939,12.09362123586423,12.09506948452707,12.11484239976664,12.62695974183334,12.85387542577706,13.38925704396144,12.06981024722905,12.08191062002269,12.08191062002269,12.03140599281984,13.8020323342788,13.98721596735593,11.82388731455677,11.70303590794554,12.06763787423478,14.4911279018524,13.44781012316913,11.86121922986204,11.74153709410711,12.9873923993329,13.12244731476608,13.44853424750055,12.91681479315288,11.04407916339788,13.16234136001772,13.45329644522759,13.36986003828079,12.08191062002269,14.36705947792933,13.40609191969573,12.88679335812782,12.49959121623928,13.03497283442366,11.78399326930513,11.82388731455677,12.1100802020396,12.0094468571928,12.06763787423478,11.9849255916194,12.08191062002269,11.74153709410711,12.1100802020396,12.10385590825653,13.42036466548364,13.26665071609442]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>lwage<\/th>\n      <th>educ<\/th>\n      <th>exper<\/th>\n      <th>expersq<\/th>\n      <th>fatheduc<\/th>\n      <th>motheduc<\/th>\n      <th>educHat<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tip","pageLength":5,"rownames":false,"columnDefs":[{"targets":2,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 2, 3, \",\", \".\", null);\n  }"},{"targets":8,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 2, 3, \",\", \".\", null);\n  }"},{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"id","targets":1},{"name":"lwage","targets":2},{"name":"educ","targets":3},{"name":"exper","targets":4},{"name":"expersq","targets":5},{"name":"fatheduc","targets":6},{"name":"motheduc","targets":7},{"name":"educHat","targets":8}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":["options.columnDefs.0.render","options.columnDefs.1.render"],"jsHooks":[]}</script>
```


:::
:::




##

### Stage 2: model

**Stage 2 of 2SLS**: with mother education as instrument

In the second stage, we will regress log(wage) on the $\widehat{educ}$  from stage 1 and experience and its quadratic term exp square.


$$
\begin{align}
lwage = \hat{\beta}_1 +\hat{\beta}_2\widehat{educ} + \hat{\beta}_3exper +\hat{\beta}_4expersq + \hat{\epsilon}
\end{align}
$$



::: {.cell layout-align="center"}

```{.r .cell-code}
mod_step2 <- formula(lwage~educHat + exper + expersq)
ols_step2 <- lm(formula = mod_step2, data = mroz_add)
```
:::



##

### Stage 2: OLS estimate

By using the new data set (`mroz_add`), the result of the explicit 2SLS procedure are shown as below.



::: {.cell layout-align="center"}
$$
\begin{alignedat}{999}
\begin{split}
&\widehat{lwage}=&&+0.20&&+0.05educHat_i&&+0.04exper_i&&-0.00expersq_i\\ 
&(s)&&(0.4933)&&(0.0391)&&(0.0142)&&(0.0004)\\ 
&(t)&&(+0.40)&&(+1.26)&&(+3.17)&&(-2.17)\\ 
&(fit)&&R^2=0.0456&&\bar{R}^2=0.0388 && &&\\ 
&(Ftest)&&F^*=6.75&&p=0.0002 && &&
\end{split}
\end{alignedat}
$$
:::




::: {.callout-note}

Keep in mind, however, that the **standard errors** calculated in this way are incorrect (Why?).

:::


::: {.notes}
while the t-test on the coefficient of education is not significant because the t statistics is less than the critical value 2. But the model F-test is significant with small p value here.
:::

## TSLS: Integrated solution

### The whole story

We need a **Integrated solution** for following reasons:

- We should obtain the correct estimated error for test and inference.

- We should avoid tedious steps in the former step-by-step routine. When the model contains more than one endogenous regressors and there are lots available instruments, then the step-by-step solution will get extremely tedious.


##

### The `R` toolbox

In `R` ecosystem, we have two packages to execute the  integrated solution:

- We can use `systemfit` package function `systemfit::systemfit()`.

- Or we may use `ARE` package function `ARE::ivreg()`.



Both of these tools can conduct the integrated solution, and will adjust the variance of estimators automatically.



## example 1: TSLS with only `motheduc` as IV

### The TSLS model

In order to get the correct estimated error, we need use the  **"integrated solution"** for 2SLS. And we will process the estimation with proper software and tools.

Firstly, let's consider using $motheduc$ as the only instrument for $educ$.

$$
\begin{cases}
\begin{align}
\widehat{educ} &= \hat{\gamma}_1 +\hat{\gamma}_2exper + \hat{\gamma}_3expersq +\hat{\gamma}_4motheduc  && \text{(stage 1)}\\
lwage & = \hat{\beta}_1 +\hat{\beta}_2\widehat{educ} + \hat{\beta}_3exper +\hat{\beta}_4expersq + \hat{\epsilon}  && \text{(stage 2)}
\end{align}
\end{cases}
$$




##

### The TSLS results (tidy table)



::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-64e408a2b3fc6e323145" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-64e408a2b3fc6e323145">{"x":{"filter":"none","vertical":false,"caption":"<caption>2SLS result(`motheduc` as instrument)<\/caption>","data":[["1","2","3","4","5","6","7","8"],["eq1","eq1","eq1","eq1","eq2","eq2","eq2","eq2"],["(Intercept)","exper","expersq","motheduc","(Intercept)","educ","exper","expersq"],[9.775102690226637,0.04886150006395119,-0.001281064973186706,0.2676908090921895,0.1981860564727007,0.04926295335038281,0.04485584787359646,-0.0009220761624694296],[0.4238886153570713,0.04166926042173844,0.001244905624400281,0.03112979662060363,0.4728772295378676,0.0374360256307117,0.0135768173487378,0.000406381308328036],[23.06054547370275,1.17260300685492,-1.029045855426867,8.599182717275291,0.4191067873290992,1.31592369970942,3.303855883261669,-2.268992553479153],[0,0.2416134223719495,0.3040447864773395,0,0.6753503302668307,0.1889106699097165,0.001034570786957012,0.02377054666535239]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>eq<\/th>\n      <th>vars<\/th>\n      <th>Estimate<\/th>\n      <th>Std. Error<\/th>\n      <th>t value<\/th>\n      <th>Pr(&gt;|t|)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","pageLength":10,"rownames":false,"columnDefs":[{"targets":3,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":4,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":5,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":6,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"eq","targets":1},{"name":"vars","targets":2},{"name":"Estimate","targets":3},{"name":"Std. Error","targets":4},{"name":"t value","targets":5},{"name":"Pr(>|t|)","targets":6}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":["options.columnDefs.0.render","options.columnDefs.1.render","options.columnDefs.2.render","options.columnDefs.3.render"],"jsHooks":[]}</script>
```


:::
:::





- The t-test for variable `educ` is significant (p-value less than 0.05).

::: {.aside}
**Note** : The corresponding code of `R` programming is in the following slides. The table results use the report from the `systemfit::systemfit()` function.
:::


##

### The TSLS code: using R function `systemfit::systemfit()`

The R code using `systemfit::systemfit()` as follows:



::: {.cell layout-align="center"}

```{.r .cell-code}
# load pkg
require(systemfit)
# set two models
eq_1 <- educ ~  exper + expersq + motheduc
eq_2 <- lwage ~ educ + exper + expersq
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~  exper + expersq + motheduc
# fit models
fit.sys <- systemfit(
sys, inst=instr,
method="2SLS", data = mroz)
# summary of model fit
smry.system_m <- summary(fit.sys)
```
:::



##

### The TSLS raw result: using R function `systemfit::systemfit()`

The following is the 2SLS analysis report using `systemfit::systemfit() `:

<!---.scroll-box-16[--->



::: {.cell layout-align="center"}

```{.r .cell-code}
smry.system_m
```

::: {.cell-output .cell-output-stdout}

```

systemfit results 
method: 2SLS 

         N  DF     SSR detRCov   OLS-R2 McElroy-R2
system 856 848 2085.49 1.96552 0.150003   0.112323

      N  DF      SSR      MSE     RMSE       R2   Adj R2
eq1 428 424 1889.658 4.456742 2.111100 0.152694 0.146699
eq2 428 424  195.829 0.461861 0.679604 0.123130 0.116926

The covariance matrix of the residuals
         eq1      eq2
eq1 4.456742 0.304759
eq2 0.304759 0.461861

The correlations of the residuals
         eq1      eq2
eq1 1.000000 0.212418
eq2 0.212418 1.000000


2SLS estimates for 'eq1' (equation 1)
Model Formula: educ ~ exper + expersq + motheduc
Instruments: ~exper + expersq + motheduc

               Estimate  Std. Error  t value Pr(>|t|)    
(Intercept)  9.77510269  0.42388862 23.06055  < 2e-16 ***
exper        0.04886150  0.04166926  1.17260  0.24161    
expersq     -0.00128106  0.00124491 -1.02905  0.30404    
motheduc     0.26769081  0.03112980  8.59918  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.1111 on 424 degrees of freedom
Number of observations: 428 Degrees of Freedom: 424 
SSR: 1889.658428 MSE: 4.456742 Root MSE: 2.1111 
Multiple R-Squared: 0.152694 Adjusted R-Squared: 0.146699 


2SLS estimates for 'eq2' (equation 2)
Model Formula: lwage ~ educ + exper + expersq
Instruments: ~exper + expersq + motheduc

                Estimate   Std. Error  t value  Pr(>|t|)   
(Intercept)  0.198186056  0.472877230  0.41911 0.6753503   
educ         0.049262953  0.037436026  1.31592 0.1889107   
exper        0.044855848  0.013576817  3.30386 0.0010346 **
expersq     -0.000922076  0.000406381 -2.26899 0.0237705 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.679604 on 424 degrees of freedom
Number of observations: 428 Degrees of Freedom: 424 
SSR: 195.829058 MSE: 0.461861 Root MSE: 0.679604 
Multiple R-Squared: 0.12313 Adjusted R-Squared: 0.116926 
```


:::
:::



<!---]--->

::: {.aside}
**NOTE** : `systemfit::systemfit()` simultaneously reports the analysis results of two equations in 2SLS!
:::

##

### The TSLS code: using R function `ARE::ivreg()`

The R code using `ARE::ivreg()` as follows:

<!---.scroll-box-16[--->




::: {.cell layout-align="center"}

```{.r .cell-code}
# load pkg
require(AER)
# specify model
mod_iv_m <- formula(lwage ~ educ + exper + expersq
| motheduc + exper + expersq)
# fit model
lm_iv_m <- ivreg(formula = mod_iv_m, data = mroz)
# summary of model fit
smry.ivm <- summary(lm_iv_m)
```
:::



<!---]--->

##

### The TSLS code: using R function `ARE::ivreg()`

The following is the 2SLS analysis report using `ARE::ivreg()`:

<!---.scroll-box-16[--->



::: {.cell layout-align="center"}

```{.r .cell-code}
smry.ivm
```

::: {.cell-output .cell-output-stdout}

```

Call:
ivreg(formula = mod_iv_m, data = mroz)

Residuals:
     Min       1Q   Median       3Q      Max 
-3.10804 -0.32633  0.06024  0.36772  2.34351 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.1981861  0.4728772   0.419  0.67535   
educ         0.0492630  0.0374360   1.316  0.18891   
exper        0.0448558  0.0135768   3.304  0.00103 **
expersq     -0.0009221  0.0004064  -2.269  0.02377 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6796 on 424 degrees of freedom
Multiple R-Squared: 0.1231,	Adjusted R-squared: 0.1169 
Wald test: 7.348 on 3 and 424 DF,  p-value: 8.228e-05 
```


:::
:::



<!---]--->

::: {.aside}
**Note**: `ARE::ivreg()` Only reports the result of the last equation of 2SLS, not include the first equation!
:::

::: {.notes}
We can see that the t-test for$educ$ is not significant.We should note that the instruments (motheduc + exper + expersq) are included as whole behind the procedure in this code chunk. But we do not  see these instruments in the output.We can see that the t-test on the coefficient of education is still not significant.
:::


## example 2: TSLS with only `fatheduc` as IV

### The TSLS model

Now let's consider using $fatheduc$ as the only instrument for $educ$.

$$
\begin{cases}
\begin{align}
\widehat{educ} &= \hat{\gamma}_1 +\hat{\gamma}_2exper + \hat{\gamma}_3expersq +\hat{\gamma}_4fatheduc  && \text{(stage 1)}\\
lwage & = \hat{\beta}_1 +\hat{\beta}_2\widehat{educ} + \hat{\beta}_3exper +\hat{\beta}_4expersq + \hat{\epsilon}  && \text{(stage 2)}
\end{align}
\end{cases}
$$

We will repeat the whole procedure with `R`.

::: {.notes}
We will repeat the whole procedure with `R`.
:::

##

### The TSLS results (tidy table)




::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-8761abbe6c18d64e59e7" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-8761abbe6c18d64e59e7">{"x":{"filter":"none","vertical":false,"caption":"<caption>2SLS result(`fatheduc` as instrument)<\/caption>","data":[["1","2","3","4","5","6","7","8"],["eq1","eq1","eq1","eq1","eq2","eq2","eq2","eq2"],["(Intercept)","exper","expersq","fatheduc","(Intercept)","educ","exper","expersq"],[9.88703428977956,0.0468243339408936,-0.001150382546545442,0.2705061011723723,-0.06111693330746015,0.07022629127205504,0.0436715881293298,-0.0008821549586141926],[0.3956077875642711,0.04110742425261585,0.001228568028189189,0.02887859434343325,0.4364461275559863,0.03444269413256235,0.01340012103140733,0.0004009170075461476],[24.992011281308,1.139072437454262,-0.9363604783375439,9.367010663865054,-0.1400331666354863,2.038931420456527,3.259044304672467,-2.200343068540568],[0,0.2553160733348245,0.3496206296559616,0,0.8887002806250497,0.04207657247632368,0.001207928423314186,0.02832119375282405]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>eq<\/th>\n      <th>vars<\/th>\n      <th>Estimate<\/th>\n      <th>Std. Error<\/th>\n      <th>t value<\/th>\n      <th>Pr(&gt;|t|)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","pageLength":10,"rownames":false,"columnDefs":[{"targets":3,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":4,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":5,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":6,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"eq","targets":1},{"name":"vars","targets":2},{"name":"Estimate","targets":3},{"name":"Std. Error","targets":4},{"name":"t value","targets":5},{"name":"Pr(>|t|)","targets":6}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":["options.columnDefs.0.render","options.columnDefs.1.render","options.columnDefs.2.render","options.columnDefs.3.render"],"jsHooks":[]}</script>
```


:::
:::





- The t-test for variable `educ` is significant (p-value less than 0.05).


::: {.aside}
**Note** : The corresponding code of `R` programming is in the following slides. The table results use the report from the `systemfit::systemfit()` function.
:::


##

### The TSLS code: using R function `systemfit::systemfit()`

The R code using `systemfit::systemfit()` as follows:



::: {.cell layout-align="center"}

```{.r .cell-code}
# load pkg
require(systemfit)
# set two models
eq_1 <- educ ~  exper + expersq + fatheduc
eq_2 <- lwage ~ educ + exper + expersq
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~ exper + expersq + fatheduc
# fit models
fit.sys <- systemfit(
  formula = sys, inst=instr,
  method="2SLS", data = mroz
  )
# summary of model fit
smry.system_f <- summary(fit.sys)
```
:::



##

### The TSLS raw result: using R function `systemfit::systemfit()`

The following is the 2SLS analysis report using `systemfit::systemfit() `:

<!---.scroll-box-16[--->



::: {.cell layout-align="center"}

```{.r .cell-code}
smry.system_f
```

::: {.cell-output .cell-output-stdout}

```

systemfit results 
method: 2SLS 

         N  DF     SSR detRCov   OLS-R2 McElroy-R2
system 856 848 2030.11 1.91943 0.172575   0.134508

      N  DF      SSR      MSE     RMSE       R2   Adj R2
eq1 428 424 1838.719 4.336602 2.082451 0.175535 0.169701
eq2 428 424  191.387 0.451384 0.671851 0.143022 0.136959

The covariance matrix of the residuals
         eq1      eq2
eq1 4.336602 0.195036
eq2 0.195036 0.451384

The correlations of the residuals
         eq1      eq2
eq1 1.000000 0.139402
eq2 0.139402 1.000000


2SLS estimates for 'eq1' (equation 1)
Model Formula: educ ~ exper + expersq + fatheduc
Instruments: ~exper + expersq + fatheduc

               Estimate  Std. Error  t value Pr(>|t|)    
(Intercept)  9.88703429  0.39560779 24.99201  < 2e-16 ***
exper        0.04682433  0.04110742  1.13907  0.25532    
expersq     -0.00115038  0.00122857 -0.93636  0.34962    
fatheduc     0.27050610  0.02887859  9.36701  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.082451 on 424 degrees of freedom
Number of observations: 428 Degrees of Freedom: 424 
SSR: 1838.719104 MSE: 4.336602 Root MSE: 2.082451 
Multiple R-Squared: 0.175535 Adjusted R-Squared: 0.169701 


2SLS estimates for 'eq2' (equation 2)
Model Formula: lwage ~ educ + exper + expersq
Instruments: ~exper + expersq + fatheduc

                Estimate   Std. Error  t value  Pr(>|t|)   
(Intercept) -0.061116933  0.436446128 -0.14003 0.8887003   
educ         0.070226291  0.034442694  2.03893 0.0420766 * 
exper        0.043671588  0.013400121  3.25904 0.0012079 **
expersq     -0.000882155  0.000400917 -2.20034 0.0283212 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.671851 on 424 degrees of freedom
Number of observations: 428 Degrees of Freedom: 424 
SSR: 191.386653 MSE: 0.451384 Root MSE: 0.671851 
Multiple R-Squared: 0.143022 Adjusted R-Squared: 0.136959 
```


:::
:::



<!---]--->

::: {.aside}
**NOTE** : `systemfit::systemfit()` simultaneously reports the analysis results of two equations in 2SLS!
:::

##

### The TSLS code: using R function `ARE::ivreg()`


The R code using `ARE::ivreg()` as follows:

<!---.scroll-box-16[--->



::: {.cell layout-align="center"}

```{.r .cell-code}
# load pkg
require(AER)
# specify model
mod_iv_f <- formula(
  lwage ~ educ + exper + expersq
  | fatheduc + exper + expersq
  )
# fit model
lm_iv_f <- ivreg(formula = mod_iv_f, data = mroz)
# summary of model fit
smry.ivf <- summary(lm_iv_f)
```
:::



<!---]--->

::: {.notes}
We can see the insturments (fatheduc + exper + expersq) are included as whole behind the procedure in this code chunk.While, We can find that the t-test on the coefficient of education is significant now with its p value less than 0.05.
:::


##

### The TSLS raw result: using R function `ARE::ivreg()`

The following is the 2SLS analysis report using `ARE::ivreg()`:

<!---.scroll-box-16[--->



::: {.cell layout-align="center"}

```{.r .cell-code}
smry.ivf
```

::: {.cell-output .cell-output-stdout}

```

Call:
ivreg(formula = mod_iv_f, data = mroz)

Residuals:
     Min       1Q   Median       3Q      Max 
-3.09170 -0.32776  0.05006  0.37365  2.35346 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
(Intercept) -0.0611169  0.4364461  -0.140  0.88870   
educ         0.0702263  0.0344427   2.039  0.04208 * 
exper        0.0436716  0.0134001   3.259  0.00121 **
expersq     -0.0008822  0.0004009  -2.200  0.02832 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6719 on 424 degrees of freedom
Multiple R-Squared: 0.143,	Adjusted R-squared: 0.137 
Wald test: 8.314 on 3 and 424 DF,  p-value: 2.201e-05 
```


:::
:::



<!---]--->

::: {.aside}
**Note**: `ARE::ivreg()` Only reports the result of the last equation of 2SLS, not include the first equation!
:::




## example 3: TSLS with both `mothedu` and `fatheduc` as IVs

### The TSLS model

Also, we can use both $motheduc$ and $fatheduc$ as instruments for $educ$.

$$
\begin{cases}
\begin{align}
\widehat{educ} &= \hat{\gamma}_1 +\hat{\gamma}_2exper + \hat{\beta}_3expersq +\hat{\beta}_4motheduc + \hat{\beta}_5fatheduc  && \text{(stage 1)}\\
lwage & = \hat{\beta}_1 +\hat{\beta}_2\widehat{educ} + \hat{\beta}_3exper +\hat{\beta}_4expersq + \hat{\epsilon}  && \text{(stage 2)}
\end{align}
\end{cases}
$$


##

### The TSLS results (tidy table)



::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-13bc14fb3f2f561cc4ab" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-13bc14fb3f2f561cc4ab">{"x":{"filter":"none","vertical":false,"caption":"<caption>2SLS result(`motheduc` and `fatheduc` as instruments)<\/caption>","data":[["1","2","3","4","5","6","7","8","9"],["eq1","eq1","eq1","eq1","eq1","eq2","eq2","eq2","eq2"],["(Intercept)","exper","expersq","motheduc","fatheduc","(Intercept)","educ","exper","expersq"],[9.102640109600149,0.0452254233687072,-0.001009090957170813,0.1575970327485924,0.1895484101549553,0.04810030693242318,0.06139662866013517,0.04417039294876216,-0.0008989695881555132],[0.4265613672307983,0.04025071238007708,0.001203344812335161,0.03589411554669008,0.03375646678192469,0.4003280776043034,0.03143669564470841,0.0134324755294433,0.0004016856118761809],[21.33957927013819,1.123593116605123,-0.8385717433830233,4.390609166663949,5.615173275671472,0.1201522192004904,1.953024241288851,3.288328562515743,-2.237993001433717],[0,0.2618229379317158,0.4021832850606946,1.429840367217494e-05,3.561512373906339e-08,0.9044194793608125,0.05147417391522291,0.001091838425269831,0.02574002733425673]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>eq<\/th>\n      <th>vars<\/th>\n      <th>Estimate<\/th>\n      <th>Std. Error<\/th>\n      <th>t value<\/th>\n      <th>Pr(&gt;|t|)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","pageLength":10,"rownames":false,"columnDefs":[{"targets":3,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":4,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":5,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":6,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"eq","targets":1},{"name":"vars","targets":2},{"name":"Estimate","targets":3},{"name":"Std. Error","targets":4},{"name":"t value","targets":5},{"name":"Pr(>|t|)","targets":6}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":["options.columnDefs.0.render","options.columnDefs.1.render","options.columnDefs.2.render","options.columnDefs.3.render"],"jsHooks":[]}</script>
```


:::
:::





::: {.aside}
**Note** : The corresponding code of `R` programming is in the following slides. The table results use the report from the `systemfit::systemfit()` function.
:::

::: {.notes}
We can see that the t-test result of the coefficient of variable `educ` is significant (p-value less than 0.1).The insturments (motheduc +fatheduc + exper + expersq) are included behind the procedure in this code chunk.And we can find that the t-test on the coefficient of education is significant with its p value less than 0.1.
:::

##

### The TSLS code: using R function  `systemfit::systemfit()`


The R code using `systemfit::systemfit()` as follows:



::: {.cell layout-align="center"}

```{.r .cell-code}
# load pkg
require(systemfit)
# set two models
eq_1 <- educ ~ exper + expersq + motheduc + fatheduc
eq_2 <- lwage ~ educ + exper + expersq
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~ exper + expersq + motheduc + fatheduc
# fit models
fit.sys <- systemfit(
sys, inst=instr,
method="2SLS", data = mroz)
# summary of model fit
smry.system_mf <- summary(fit.sys)
```
:::



##

### The TSLS raw result: using R function `systemfit::systemfit()`


The following is the 2SLS analysis report using `systemfit::systemfit() `:

<!---.scroll-box-16[--->



::: {.cell layout-align="center"}

```{.r .cell-code}
smry.system_mf
```

::: {.cell-output .cell-output-stdout}

```

systemfit results 
method: 2SLS 

         N  DF    SSR detRCov   OLS-R2 McElroy-R2
system 856 847 1951.6 1.83425 0.204575   0.149485

      N  DF     SSR      MSE     RMSE       R2   Adj R2
eq1 428 423 1758.58 4.157388 2.038967 0.211471 0.204014
eq2 428 424  193.02 0.455236 0.674712 0.135708 0.129593

The covariance matrix of the residuals
         eq1      eq2
eq1 4.157388 0.241536
eq2 0.241536 0.455236

The correlations of the residuals
         eq1      eq2
eq1 1.000000 0.175571
eq2 0.175571 1.000000


2SLS estimates for 'eq1' (equation 1)
Model Formula: educ ~ exper + expersq + motheduc + fatheduc
Instruments: ~exper + expersq + motheduc + fatheduc

               Estimate  Std. Error  t value   Pr(>|t|)    
(Intercept)  9.10264011  0.42656137 21.33958 < 2.22e-16 ***
exper        0.04522542  0.04025071  1.12359    0.26182    
expersq     -0.00100909  0.00120334 -0.83857    0.40218    
motheduc     0.15759703  0.03589412  4.39061 1.4298e-05 ***
fatheduc     0.18954841  0.03375647  5.61517 3.5615e-08 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.038967 on 423 degrees of freedom
Number of observations: 428 Degrees of Freedom: 423 
SSR: 1758.575263 MSE: 4.157388 Root MSE: 2.038967 
Multiple R-Squared: 0.211471 Adjusted R-Squared: 0.204014 


2SLS estimates for 'eq2' (equation 2)
Model Formula: lwage ~ educ + exper + expersq
Instruments: ~exper + expersq + motheduc + fatheduc

                Estimate   Std. Error  t value  Pr(>|t|)   
(Intercept)  0.048100307  0.400328078  0.12015 0.9044195   
educ         0.061396629  0.031436696  1.95302 0.0514742 . 
exper        0.044170393  0.013432476  3.28833 0.0010918 **
expersq     -0.000898970  0.000401686 -2.23799 0.0257400 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.674712 on 424 degrees of freedom
Number of observations: 428 Degrees of Freedom: 424 
SSR: 193.020015 MSE: 0.455236 Root MSE: 0.674712 
Multiple R-Squared: 0.135708 Adjusted R-Squared: 0.129593 
```


:::
:::



<!---]--->

::: {.aside}
**NOTE** : `systemfit::systemfit()` simultaneously reports the analysis results of two equations in 2SLS!
:::

##

### The TSLS code: using R function `ARE::ivreg()`


The R code using `ARE::ivreg()` as follows:

<!---.scroll-box-16[--->



::: {.cell layout-align="center"}

:::



<!---]--->

##

### The TSLS raw result: using R function`ARE::ivreg()`


The following is the 2SLS analysis report using `ARE::ivreg()`:

<!---.scroll-box-16[--->



::: {.cell layout-align="center"}

```{.r .cell-code}
smry.ivmf
```

::: {.cell-output .cell-output-stdout}

```

Call:
ivreg(formula = mod_iv_mf, data = mroz)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.0986 -0.3196  0.0551  0.3689  2.3493 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.0481003  0.4003281   0.120  0.90442   
educ         0.0613966  0.0314367   1.953  0.05147 . 
exper        0.0441704  0.0134325   3.288  0.00109 **
expersq     -0.0008990  0.0004017  -2.238  0.02574 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6747 on 424 degrees of freedom
Multiple R-Squared: 0.1357,	Adjusted R-squared: 0.1296 
Wald test: 8.141 on 3 and 424 DF,  p-value: 2.787e-05 
```


:::
:::



<!---]--->

::: {.aside}
**Note**: `ARE::ivreg()` Only reports the result of the last equation of 2SLS, not include the first equation!
:::

## Solutions comparison

### Glance of mutiple models 

Until now, we obtain totally **Five** estimation results with different model settings or solutions:

a. Error specification model with OLS regression directly.

b. (**Step-by-Step solution**) Explicit 2SLS estimation **without** variance correction (IV regression step by step with only $matheduc$ as instrument).

c. (**Integrated solution**) Dedicated IV estimation **with** variance correction ( using `R` tools of `systemfit::systemfit()` or `ARE::ivreg()`).

- The IV model with only$motheduc$ as instrument for endogenous variable $edu$

- The IV model with only$fatheduc$ as instrument for endogenous variable $edu$

- The IV model with  both$motheduc$ and$fatheduc$ as instruments

For the purpose of comparison, all results will show in next slide.

::: {.notes}
we use `R` function `ARE::ivreg()` to get the IV estimation **with** variance correction with the last three model considering different instruments.
:::

##

### tidy reports (png)


![](pic/chpt17-iv-comparison.png){width=90%}

##

### Stidy reports (html){.scrollable}


::: {style="font-size:60%"}




```{=html}

<table style="text-align:center"><tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="5">Dependent variable: lwage</td></tr>
<tr><td></td><td colspan="5" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>OLS</td><td>explicit 2SLS</td><td>IV mothereduc</td><td>IV fathereduc</td><td>IV mothereduc and fathereduc</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Constant</td><td>-0.5220<sup>***</sup></td><td>0.1982</td><td>0.1982</td><td>-0.0611</td><td>0.0481</td></tr>
<tr><td style="text-align:left"></td><td>(0.1986)</td><td>(0.4933)</td><td>(0.4729)</td><td>(0.4364)</td><td>(0.4003)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">educ</td><td>0.1075<sup>***</sup></td><td></td><td>0.0493</td><td>0.0702<sup>**</sup></td><td>0.0614<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0141)</td><td></td><td>(0.0374)</td><td>(0.0344)</td><td>(0.0314)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">educHat</td><td></td><td>0.0493</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.0391)</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">exper</td><td>0.0416<sup>***</sup></td><td>0.0449<sup>***</sup></td><td>0.0449<sup>***</sup></td><td>0.0437<sup>***</sup></td><td>0.0442<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0132)</td><td>(0.0142)</td><td>(0.0136)</td><td>(0.0134)</td><td>(0.0134)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">expersq</td><td>-0.0008<sup>**</sup></td><td>-0.0009<sup>**</sup></td><td>-0.0009<sup>**</sup></td><td>-0.0009<sup>**</sup></td><td>-0.0009<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0004)</td><td>(0.0004)</td><td>(0.0004)</td><td>(0.0004)</td><td>(0.0004)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>428</td><td>428</td><td>428</td><td>428</td><td>428</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.1568</td><td>0.0456</td><td>0.1231</td><td>0.1430</td><td>0.1357</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.1509</td><td>0.0388</td><td>0.1169</td><td>0.1370</td><td>0.1296</td></tr>
<tr><td style="text-align:left">Residual Std. Error (df = 424)</td><td>0.6664</td><td>0.7090</td><td>0.6796</td><td>0.6719</td><td>0.6747</td></tr>
<tr><td style="text-align:left">F Statistic (df = 3; 424)</td><td>26.2862<sup>***</sup></td><td>6.7510<sup>***</sup></td><td></td><td></td><td></td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr></table>

```




:::



::: {.notes}
see online book "Principles of Econometrics with R"（10.1 The Instrumental Variables (IV) Method，[free online](https://bookdown.org/ccolonescu/RPoE4/random-regressors.html#the-instrumental-variables-iv-method)）
:::

##

### Report tips

- The second column shows the result of the direct OLS estimation, and the third column shows the result of explicit 2SLS estimation without variance correction.

- While the last three column shows the results of IV solution with variance correction.

- And we should also remind that the  $educ$  in the IV model is equivalent to the  $educHat$  in 2SLS.

- The value within the bracket is the standard error of the estimator.

##

### Report insights

So the key points of this comparison including:

- Firstly, the table shows that the importance of education in determining wage decreases in the IV model (3) (4) and (5) with the coefficients 0.049, 0.07, 0.061 respectively. And the standard error also decrease along IV estimation (3) , (4) and (5).

- Secondly, It also shows that the explicit 2SLS model (2) and the IV model with only $motheduc$  instrument yield the same coefficients, but the **standard errors** are different. The standard error in explicit 2SLS is 0.039, which is little large than the standard error 0.037 in IV estimation.

- Thirdly, the t-test of the coefficient on education shows no significance when we use `motheduc` as the only instrument for education. You can compare this under the  explicit 2SLS estimation or IV estimation.

- Fourthly, we can fully feel and understand the **relative estimated efficiency** of 2SLS!

##

### Further thinking

After the empirical comparison, we will be even more confused with these results.

While, new question will arise inside our mind.

- Which estimation is the best?

- How to judge and evaluate different instrument choices?

We will discuss these topics in the next section.


# 17.5 Testing Instrument validity{#validity .monash-bg-blue .mcenter}

::: {.notes}
As we know, valid instruments should satisfy both relevance condition and exogeneity condition.So, let us check these conditions in this section.
:::


## Instrument vality

Consider the general model

$$
\begin{align}
Y_{i}=\beta_{0}+\sum_{j=1}^{k} \beta_{j} X_{j i}+\sum_{s=1}^{r} \beta_{k+s} W_{ri}+\epsilon_{i}
\end{align}
$$

> - $Y_{i}$ is the dependent variable
- $\beta_{0}, \ldots, \beta_{k+1}$ are $1+k+r$ unknown regression coefficients
- $X_{1 i}, \ldots, X_{k i}$ are $k$ endogenous regressors
- $W_{1 i}, \ldots, W_{r i}$ are $r$ exogenous regressors which are uncorrelated with $u_{i}$
- $u_{i}$ is the error term
- $Z_{1 i}, \ldots, Z_{m i}$ are $m$ instrumental variables

**Instrument valid** means satisfy both Relevance and Exogeneity conditions.

:::: {.columns}

::: {.column width="40%"}


$$
E\left(Z_{i} X_{i}^{\prime}\right) \neq 0
$$

:::


::: {.column width="40%"}


$$
E\left(Z_{i} \epsilon_{i}\right)=0
$$

:::

::::

::: {.notes}
Consider the general model as we have done.
:::

## Instrument Relevance

In practice, **Instrument Relevance** also means that:

If there are$k$ endogenous variables and$m$ instruments$Z$, and$m \geq k$, it must hold that the exogenous vector

$$
\left(\hat{X}_{1 i}^{*}, \ldots, \hat{X}_{k i}^{*}, W_{1 i}, \ldots, W_{r i}, 1\right)
$$

should [not]{.red} be **perfectly multicollinear**.

> **Where**:
> - $\hat{X}_{1i}^{\ast}, \ldots, \hat{X}_{ki}^{\ast}$ are the predicted values from the  $k$ first stage regressions.
> - 1 denotes the constant regressor which equals 1 for all observations.


::: {.notes}
While the concept of **Instrument Relevance** is much tricky.So, what is the meaning of **Instrument Relevance**?___Obviously, the perfect multicollinear is the rare fact and can be get rid with careful inspection.What we really need to pay attention is the contrary fact which is called **weak instruments**.
:::

## Weak instrument: introduction

### Definition

Instruments that explain little variation in the endogenous regressor $X$  are called **weak instruments**.

Formally,  When$\operatorname{corr}\left(Z_{i}, X_{i}\right)$ is close to zero,$z_{i}$ is called a weak instrument.

- Consider a simple one regressor model$Y_{i}=\beta_{0}+\beta_{1} X_{i}+\epsilon_{i}$


- The IV estimator of$\beta_{1}$ is$\widehat{\beta}_{1}^{IV}=\frac{\sum_{i=1}^{n}\left(Z_{i}-\bar{Z}\right)\left(Y_{i}-\bar{Y}\right)}{\sum_{i=1}^{n}\left(Z_{i}-\bar{Z}\right)\left(X_{i}-\bar{X}\right)}$

> Note that$\frac{1}{n} \sum_{i=1}^{n}\left(Z_{i}-\bar{Z}\right)\left(Y_{i}-\bar{Y}\right) \xrightarrow{p} \operatorname{Cov}\left(Z_{i}, Y_{i}\right)$

> and$\frac{1}{n} \sum_{i=1}^{n}\left(Z_{i}-\bar{Z}\right)\left(X_{i}-\bar{X}\right) \xrightarrow{p} \operatorname{Cov}\left(Z_{i}, X_{i}\right)$.

- Thus,if$\operatorname{Cov}\left(Z_{i},X_{i}\right) \approx 0$, then $\widehat{\beta}_{1}^{IV}$ is useless.

::: {.notes}
Let me give you an example.
:::

## Birth weight example: weak instrument

### Edogeneity and choice of instrument

We focus on the effect of cigarette smoking on the infant birth weight. Without other
explanatory variables, the model is

-$bwght =$  child birth weight, in ounces.

-$packs =$ packs smoked per day while pregnant.

-$cigprice=$ cigarette price in home state


$$
\begin{align}
\log (\text {bwght})=\beta_{0}+\beta_{1} \text {packs}+u_{i}
\end{align}
$$

- We might worry that packs is cor-related
with other health factors or the availability of good prenatal care, so that$packs$ and$u_i$ might be  correlated.

- A possible instrumental variable for$packs$ is the average price of cigarettes ($cigprice$) in the state . We will assume that$cigprice$ and$u_i$ are uncorrelated.

::: {.aside}
We will use data set from `wooldridge::bwght`.
:::

::: {.notes}
the assumed TRUE model may be$$
\begin{align}\log (\text {bwght})=\beta_{0}+\beta_{1} \text {packs}+ ( \beta_2 care\_surport)+u_{i}\end{align}
$$-$cigs =$ number of cigarettes smoked by the mother while pregnant, per day.-$faminc=$ annual family income, in thousands of dollars.
:::

##

### TSLS estimation


However, by regressing $packs$ on $cigprice$ in stage 1, we find basically no effect.




::: {.cell layout-align="center"}
$$
\begin{alignedat}{999}
\begin{split}
&\widehat{packs}=&&+0.0674&&+0.0003cigprice_i\\ 
&(s)&&(0.1025)&&(0.0008)\\ 
&(t)&&(+0.66)&&(+0.36)\\ 
&(Ftest)&&F^*=0.13&&p=0.7179
\end{split}
\end{alignedat}
$$
:::





If we insist to use $cigprice$ as instrument, and run the stage 2 OLS, we will find



::: {.cell layout-align="center"}
$$
\begin{alignedat}{999}
\begin{split}
&\widehat{lbwght}=&&+4.4481&&+2.9887packs\_hat_i\\ 
&(s)&&(0.1843)&&(1.7654)\\ 
&(t)&&(+24.13)&&(+1.69)\\ 
&(Ftest)&&F^*=2.87&&p=0.0907
\end{split}
\end{alignedat}
$$
:::




::: {.aside}
Obviously, this estimation is meaningless (Why?).The $cigprice$ behaves as a **weak instrument**, and the problem was already exposed in stage 1 regression.
:::

::: {.notes}
- because there is huge standard error and not significant on coefficient of packs.- and also it has the wrong sign on coefficient of packs, which should not be positive.As what we have discussed, the result is unbelievable since the cigprice is a weak instrument for packs.
:::

##

### The strategy with weak instruments

The weak instrument ($Z_i$ and$X_i$ is week correlated) led to an **important finding**: even with very large sample
sizes the 2SLS estimator can be biased and a distribution that is very different from standard normal (Staiger and Stock 1997).

There are two ways to proceed if instruments are weak:

- Discard the **weak instruments** and/or find **stronger instruments**.

> While the former is only an option if the unknown coefficients remain identified when the weak instruments are discarded, the latter can be difficult and even may require a redesign of the whole study.

- Stick with the weak instruments but use methods that improve upon TSLS.

> Such as **limited information maximum likelihood estimation (LIML)**.


::: {.notes}
So, what should we do if the instruments are weak or some of them are weak?
:::

## Restricted F-test (weak instrument test solution 1)

### Only one endogenous regressor in the model

In case with a **single** endogenous regressor, we can take the  **F-test** to check the **Weak instrument**.


::: {.callout-note}

The basic idea of the F-test is very simple:

If the estimated coefficients of **all instruments** in the **first-stage** of a 2SLS estimation are **zero**, the instruments do not explain any of the variation in the$X$ which clearly violates the relevance assumption.

:::


##

### The procudure

We may use the following rule of thumb:

- Conduct the **first-stage regression** of a 2SLS estimation

$$
\begin{align}
X_{i}=\hat{\gamma}_{0}+\hat{\gamma}_{1} W_{1 i}+\ldots+\hat{\gamma}_{p} W_{p i}+ \hat{\theta}_{1} Z_{1 i}+\ldots+\hat{\theta}_{q} Z_{q i}+v_{i} \quad \text{(3)}
\end{align}
$$

- Test the restricted joint hypothesis$H_0: \hat{\theta}_1=\ldots=\hat{\theta}_q=0$ by compute the$F$-statistic. We call this **Restricted F-test**  which is different with the **Classical overall F-test**.

- If the$F$-statistic is less than  critical value, the instruments are **weak**.

::: {.callout-note}

The rule of thumb is easily implemented in `R`. Run the first-stage regression using `lm()` and subsequently compute the restricted $F$-statistic by `R` function of `car::linearHypothesis()`.

:::

::: {.aside}
The Classical overall F-test has$H_0:\gamma_1 = \cdots = \gamma_p =\hat{\theta}_1=\ldots=\hat{\theta}_q=0$
:::

::: {.notes}
Also, you may ask that how do you know the instruments are weak or some of them are weak?We will test this considering with different situations.
:::

## wage example: restricted F-test


##

### Model setting


For all  three IV model, we can test instrument(s) relevance respectively.

$$
\begin{align}
educ &= \gamma_1 +\gamma_2exper +\gamma_2expersq + \theta_1motheduc  +v
&& \text{(relevance test 1)}\\
educ &= \gamma_1 +\gamma_2exper +\gamma_2expersq + \theta_2fatheduc +v
&& \text{(relevance test 2)} \\
educ &= \gamma_1 +\gamma_2exper +\gamma_2expersq + \theta_1motheduc + \theta_2fatheduc +v
&& \text{(relevance test 3)}
\end{align}
$$

::: {.notes}
And we will test the weak instrument issues by using restricted F test.
:::

##

### `motheduc` IV: Restricted F-test(model and hypothesis)

Consider model 1:

$$
\begin{align}
educ &= \gamma_1 +\gamma_2exper +\gamma_3expersq + \theta_1motheduc  +v
\end{align}
$$

The restricted F-test' null hypothesis:$H_0: \theta_1  =0$.

We will test whether `motheduc` are week instruments.


##

### `motheduc` IV: Restricted F-test(R Code and result)

The result show that the p-value of$F^{\ast}$ is much smaller than 0.01. Null hypothesis $H_0$ was rejected. `motheduc` is  **instruments relevance** (exogeneity valid).






::: {.cell layout-align="center"}

```{.r .cell-code}
library("car")
mod_relevance1 <- formula(educ ~ exper +expersq + motheduc)
ols_relevance1 <- lm(formula = mod_relevance1, data = mroz)
ftest_constrain_m <- linearHypothesis(
  ols_relevance1, 
  c("motheduc=0")
  )
```
:::

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}

```
Linear hypothesis test

Hypothesis:
motheduc = 0

Model 1: restricted model
Model 2: educ ~ exper + expersq + motheduc

  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1    425 2219.2                                  
2    424 1889.7  1    329.56 73.946 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::






##

### Test comparision: Classic F-test (hypothsis and result)

<!-- > Note: **Restricted F test** (`rscales::number(F_r1, 0.01)`) is different with the **classical OLS F test**(show bellow `rscales::number(F_c1, 0.01)`). -->

$$
\begin{align}
educ &= \gamma_1 +\gamma_2exper +\gamma_2expersq + \theta_1motheduc  +v
\end{align}
$$

The classic OLS  F-test' null hypothesis:$H_0: \gamma_2 = \gamma_3= \theta_1  =0$.

The OLS estimation results are:



::: {.cell layout-align="center"}
$$
\begin{alignedat}{999}
\begin{split}
&\widehat{educ}=&&+9.78&&+0.05exper_i&&-0.00expersq_i&&+0.27motheduc_i\\ 
&(s)&&(0.4239)&&(0.0417)&&(0.0012)&&(0.0311)\\ 
&(t)&&(+23.06)&&(+1.17)&&(-1.03)&&(+8.60)
\end{split}
\end{alignedat}
$$
:::




::: {.notes}
Restricted F test take the Null hypotheis with  coefficients before the instruments all euqal to zero, While the classical F test take the Null hypotheis with coefficients before all regressors equal to zero.
:::

##

### `fatheduc` IV: Restricted F-test (model and hypothesis)


Consider model 2:

$$
\begin{align}
educ &= \gamma_1 +\gamma_2exper +\gamma_3expersq + \theta_1fatheduc +v
&& \text{(relevance test 2)}
\end{align}
$$

The restricted F-test' null hypothesis:$H_0: \theta_1  =0$.

We will test whether `fatheduc` are week instruments.


##

### `fatheduc` IV: Restricted F-test (R Code and result)

The result show that the p-value of$F^{\ast}$ is much smaller than 0.01. Null hypothesis $H_0$ was rejected. `fatheduc` is  **instruments relevance** (exogeneity valid).




::: {.cell layout-align="center"}

```{.r .cell-code}
mod_relevance2 <- formula(educ ~ exper +expersq  + fatheduc)
ols_relevance2 <- lm(formula = mod_relevance2, data = mroz)
ftest_constrain_f <- linearHypothesis(
  ols_relevance2, c("fatheduc=0")
  )
```
:::

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}

```
Linear hypothesis test

Hypothesis:
fatheduc = 0

Model 1: restricted model
Model 2: educ ~ exper + expersq + fatheduc

  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1    425 2219.2                                  
2    424 1838.7  1     380.5 87.741 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::





##

### `motheduc` and `fatheduc` IV: restricted F-test (model and hypothesis)

Consider model 3:

$$
\begin{align}
educ &= \gamma_1 +\gamma_2exper +\gamma_3expersq + \theta_1motheduc + \theta_2fatheduc +v
&& \text{(relevance test 3)}
\end{align}
$$

The restricted F-test' null hypothesis:$H_0: \theta_1 = \theta_2 =0$.

We will test whether `motheduc` and `fatheduc` are week instruments.


##

### `motheduc` and `fatheduc` IV: restricted F-test (R code and result)

The result show that the p-value of$F^{\ast}$ is much smaller than 0.01. Null hypothesis $H_0$ was rejected. `fatheduc` and `motheduc` are  **instruments relevance** (exogeneity valid).




::: {.cell layout-align="center"}

```{.r .cell-code}
mod_relevance3 <- formula(educ ~ exper +expersq + motheduc + fatheduc)
ols_relevance3 <- lm(formula = mod_relevance3, data = mroz)
ftest_constrain_mf <- linearHypothesis(
  ols_relevance3, 
  c("motheduc=0", "fatheduc=0")
)
```
:::

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}

```
Linear hypothesis test

Hypothesis:
motheduc = 0
fatheduc = 0

Model 1: restricted model
Model 2: educ ~ exper + expersq + motheduc + fatheduc

  Res.Df    RSS Df Sum of Sq    F    Pr(>F)    
1    425 2219.2                                
2    423 1758.6  2    460.64 55.4 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::




::: {.notes}
In sum, all relecance model test are significant. And we can conclude that the instrumenta mothereducation and father education satisify the relevance condition.Until now, we show the relevance F-test with situation that contains only one endogenous regressor.Next, I will give a two endogeous variables example by using Cragg-Donald test.
:::

## Cragg-Donald test(weak instrument test solution 2)

### More than one endogenous regressor in the model

The former test for weak instruments might be unreliable with **more than** one endogenous regressor, though, because there is indeed one$F$-statistic for each endogenous regressor.

An alternative is the **Cragg-Donald test** based on the following statistic:

$$
\begin{align}
F=\frac{N-G-B}{L} \frac{r_{B}^{2}}{1-r_{B}^{2}}
\end{align}
$$

- where:$G$ is the number of exogenous regressors;$B$ is the number of endogenous regressors;$L$ is the number of external instruments;$r_B$ is the lowest canonical correlation.

> **Canonical correlation** is a measure of the correlation between the endogenous and the exogenous variables, which can be calculated by the function `cancor()` in  `R`.

::: {.notes}
external (ɪkˈstɜːnl)Canonical (kəˈnɒnɪkl)
:::


## Work hour example: Crag-Donald test

### Case backgound

Let us construct another IV model with two endogenous regressors. We assumed the following work hours determination model:

$$
\begin{equation}
hushrs=\beta_{1}+\beta_{2} mtr+\beta_{3} educ+\beta_{4} kidsl6+\beta_{5} nwifeinc+e
\end{equation}
$$

> - $hushrs$: work hours of husband, 1975
- $mtr$: federal marriage tax rate on woman
- $kidslt6$: have kids < 6 years (dummy variable)
- $nwifeinc$: wife’s net income


There are:

- Two **endogenous variables**: $educ$ and $mtr$
- Two **exogenous regressors**:  $nwifeinc$ and  $kidslt6$
- And two external **instruments**: $motheduc$  and  $fatheduc$.

##

### Cragg-Donald test: `R` code

The data set is still `mroz`, restricted to women that are in the labor force($inlf=1$).



::: {.cell layout-align="center"}

```{.r .cell-code}
# filter samples
mroz1 <- wooldridge::mroz %>% 
  filter(wage>0, inlf==1)
# set parameters
N <- nrow(mroz1); G <- 2; B <- 2; L <- 2
# for endogenous variables
x1 <- resid(lm( mtr ~ kidslt6 + nwifeinc, data = mroz1))
x2 <- resid(lm( educ ~ kidslt6 + nwifeinc, data = mroz1))
# for instruments
z1 <-resid(lm(motheduc ~ kidslt6 + nwifeinc, data = mroz1))
z2 <-resid(lm(fatheduc ~ kidslt6 + nwifeinc, data=mroz1))
# column bind
X <- cbind(x1,x2)
Y <- cbind(z1,z2)
# calculate Canonical correlation
rB <- min(cancor(X,Y)$cor)
# obtain the F statistics
CraggDonaldF <- ((N-G-L)/L)/((1-rB^2)/rB^2)
```
:::




##

### Cragg-Donald test: `R` result

Run these code lines, we can obtain the results:



::: {#tbl-cragg .cell layout-align="center" tbl-cap='Cragg-Donald test results'}
::: {.cell-output-display}


| G | L | B |  N  |   rb   | CraggDonaldF |
|:-:|:-:|:-:|:---:|:------:|:------------:|
| 2 | 2 | 2 | 428 | 0.0218 |    0.1008    |


:::
:::




The result show the Cragg-Donald  $F=$ 0.1008 , which is much smaller than **the critical value** `4.58`<sup>[1]</sup>.

This test can not rejects the null hypothesis, thus we may conclude that some of these instruments are **weak**.

::: {.aside}
[1] The critical value can be found in table 10E.1 at: Hill C, Griffiths W, Lim G. Principles of econometrics[M]. John Wiley & Sons, 2018.
:::

::: {.notes}
You can inquire the critical values in Table 10E.1 of the textbook,Hill, Griffiths, and Lim 2011.
:::

## Instrument Exogeneity

###  the difficulty

**Instrument Exogeneity** means all$m$ instruments must be uncorrelated with the error term,

$$
Cov{(Z_{1 i}, \epsilon_{i})}=0; \quad \ldots; \quad Cov{(Z_{mi}, \epsilon_{i})}=0.
$$

- In the context of the simple IV estimator, we will find that the exogeneity requirement **can not** be tested. (Why?)

- However, if we have more instruments than we need, we can effectively test whether **some of** them are uncorrelated with the structural error.

::: {.notes}
As we know , when we call a instrument is validity, we should also check that it satisfy the exogeneity condition.
:::


##

### Over-identification case

Under **over-identification**$(m>k)$, consistent IV estimation with (multiple) different combinations of instruments is possible.

> If instruments are exogenous, the obtained estimates should be **similar**.

> If estimates are very **different**, some or all instruments may [not]{.red} be exogenous.

The **Overidentifying Restrictions Test** (**J test**) formally check this.

- The null hypothesis is Instrument Exogeneity.

$$
H_{0}: E\left(Z_{h i} \epsilon_{i}\right)=0, \text { for all } h=1,2, \dots, m
$$


##  J-test (Instrument Exogeneity test solution)

### J-test procedure (1/2)

The **overidentifying restrictions test** (also called the  $J$-test, or **Sargan test**) is an approach to test the hypothesis that the additional instruments are exogenous.

Procedure of overidentifying restrictions test is:

- **Step 1**: Compute the **IV regression residuals** :

$$
\widehat{\epsilon}_{i}^{IV}=Y_{i}-\left(\hat{\beta}_{0}^{ IV}+\sum_{j=1}^{k} \hat{\beta}_{j}^{IV} X_{j i}+\sum_{s=1}^{r} \hat{\beta}_{k+s}^{IV} W_{s i}\right)
$$

- **Step 2**: Run the **auxiliary regression**: regress the IV residuals on instruments and exogenous regressors. And test the joint hypothesis$H_{0}: \alpha_{1}=0, \ldots, \alpha_{m}=0$

$$
\widehat{\epsilon}_{i}^{IV}=\theta_{0}+\sum_{h=1}^{m} \theta_{h} Z_{h i}+\sum_{s=1}^{r} \gamma_{s} W_{s i}+v_{i} \quad \text{(2)}
$$

::: {.notes}
auxiliary (ɔːɡˈzɪliəri)
:::

##

### J-test procedure (2/2)

- **Step3**: Compute the **J statistic**:$J=m F$

> where$F$ is the F-statistic of the$m$ restrictions$H_0: \theta_{1}=\ldots=\theta_{m}=0$ in eq(2)

Under the **null hypothesis**,$J$ statistic is distributed as$\chi^{2}(m-k)$ approximately for large samples($k=$ numbers of endogenous regressor
).

$$
\boldsymbol{J} \sim \chi^{2}({m-k})
$$

> IF $J$ is **less** than **critical value**, it means that all instruments are [ex]{.red}ogenous.

> IF $J$ is **larger** than **critical value**, it mean that some of the instruments are [en]{.red}ogenous.

- We can apply the  $J$-test by using `R` function `linearHypothesis()`.

::: {.notes}
approximately (əˈprɒksɪmətli)
:::

## Wage example: J-test

### Models and the auxiliary regression

Again, we can use both $matheduc$ and $fatheduc$ as instruments for $educ$.

Thus, the IV model is over-identification, and we can test the exogeneity of both these two instruments by using **J-test**.

The 2SLS model will be set as below.

$$
\begin{cases}
\begin{align}
\widehat{educ} &= \hat{\gamma}_1 +\hat{\gamma}_2exper + \hat{\beta}_3expersq +\hat{\beta}_4motheduc + \hat{\beta}_5fatheduc  && \text{(stage 1)}\\
lwage & = \hat{\beta}_1 +\hat{\beta}_2\widehat{educ} + \hat{\beta}_3exper +\hat{\beta}_4expersq + \hat{\epsilon}  && \text{(stage 2)}
\end{align}
\end{cases}
$$

And the auxiliary regression should be

$$
\begin{align}
\hat{\epsilon}^{IV} &= \hat{\alpha}_1 +\hat{\alpha}_2exper + \hat{\alpha}_3expersq +\hat{\theta}_1motheduc + \hat{\theta}_2fatheduc  + v && \text{(auxiliary model)}
\end{align}
$$


##

### TSLS: `R` code and the residuals

We have done the 2SLS estimation before, here is the `R` code (by using `ivreg::ivreg()` function):



::: {.cell layout-align="center"}

```{.r .cell-code}
# set model formula
mod_iv_mf <- formula(lwage ~ educ + exper + expersq
                     | motheduc + fatheduc + exper + expersq)
# fit IV model
lm_iv_mf <- ivreg(formula = mod_iv_mf, data = mroz)
```
:::



After the 2SLS estimation, we can obtain the IV residuals of the second stage:



::: {.cell layout-align="center"}

```{.r .cell-code}
# obtain residual of IV regression, add to data set
mroz_resid <- mroz %>%
  mutate(resid_iv_mf = residuals(lm_iv_mf))
```
:::



##

### Residuals and new data set



::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-1f25073b53e6b815793d" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-1f25073b53e6b815793d">{"x":{"filter":"none","vertical":false,"caption":"<caption>Data set with the 2SLS residuals<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284","285","286","287","288","289","290","291","292","293","294","295","296","297","298","299","300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345","346","347","348","349","350","351","352","353","354","355","356","357","358","359","360","361","362","363","364","365","366","367","368","369","370","371","372","373","374","375","376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423","424","425","426","427","428"],[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428],[1.210153698921204,0.3285121023654938,1.514137744903564,0.09212332218885422,1.524272203445435,1.556480050086975,2.120259523391724,2.059634208679199,0.7543363571166992,1.544899344444275,1.401921629905701,1.524272203445435,0.7339532375335693,0.8183690905570984,1.302831172943115,0.2980283796787262,1.167609572410583,1.643839359283447,0.6931471824645996,2.021931648254395,1.254247546195984,1.272957682609558,1.178655028343201,1.178655028343201,0.7675586938858032,1.331811785697937,1.386294364929199,1.553269624710083,1.981814861297607,1.769360423088074,0.430807888507843,0.8997548222541809,1.766629695892334,1.272957682609558,1.336788892745972,0.9017048478126526,0.8651236891746521,1.511847138404846,1.72602915763855,2.683142423629761,0.9852942824363708,1.365938544273376,0.9450336694717407,1.512376189231873,0.6931471824645996,1.244788408279419,0.7011649012565613,1.519863247871399,0.8209685683250427,0.9698315262794495,0.828508198261261,0.09430964291095734,0.1625438928604126,0.4700036346912384,0.6292484402656555,1.397160172462463,2.265443801879883,2.084541082382202,1.525838851928711,0.762160062789917,1.48160457611084,1.262826442718506,0.9996755719184875,1.832581520080566,2.479307651519775,1.279015302658081,1.937935590744019,1.070452809333801,1.12392258644104,1.321755886077881,1.744999766349792,1.301743626594543,1.641866445541382,2.107020139694214,1.46706759929657,1.605811357498169,-1.029739379882812,1.08768618106842,0,0.9382086992263794,-0.1505903750658035,0,1.073670506477356,1.265848398208618,0.4863689839839935,2.120259523391724,1.129852533340454,0.9932518005371094,1.658627986907959,0.3474121987819672,1.568324208259583,0.5108456015586853,0.1148454323410988,-0.6931471824645996,-0.3364522755146027,1.028225541114807,1.58068859577179,0.5558946132659912,0.9014207124710083,0.8843045830726624,0.4282045960426331,1.058415055274963,0.8783395886421204,1.654908299446106,1.321755886077881,0.3285121023654938,1.386294364929199,1.172884583473206,1.224187135696411,0.2876570820808411,2.23026180267334,1.504077434539795,1.531152009963989,1.375157594680786,1.760268807411194,-0.6931471824645996,1.406489133834839,1.791759490966797,1.299292087554932,1.351003885269165,1.016280889511108,1.075343608856201,1.478964686393738,1.689486742019653,2.288597822189331,-1.822631120681763,-0.9607651829719543,1.290994167327881,0.8648711442947388,1.540452122688293,0.6162121295928955,1.648658633232117,1.193498134613037,2.143976211547852,0.7244035601615906,0.9416075348854065,0.7827593684196472,1.832581520080566,1.203962802886963,1.491644859313965,1.892132639884949,2.130894899368286,1.48060405254364,0.8943313360214233,0.2025325447320938,0.4855078160762787,1.098612308502197,1.553269624710083,0.1215979680418968,2.001804351806641,1.495036602020264,0.9052298069000244,0.6325475573539734,1.386294364929199,2.102913856506348,1.959643959999084,0.5108456015586853,1.236923933029175,1.443312525749207,1.021659255027771,0.6361534595489502,1.616453289985657,0.2231435477733612,1.049807071685791,1.415051937103271,0.5753766298294067,2.60668158531189,1.517914533615112,0.7550415992736816,1.094972372055054,0.9421143531799316,1.724942803382874,1.031546115875244,0.474369078874588,0.8109301924705505,0.7092666029930115,1.710549473762512,0.4602688848972321,1.331811785697937,1.098612308502197,2.157998561859131,1.437581300735474,1.544899344444275,1.41059672832489,3.218875885009766,0.9681618809700012,1.791759490966797,1.688729524612427,-0.4091719686985016,0.2231435477733612,0.8221558332443237,1.24170196056366,1.427124381065369,1.497097492218018,0.5596157908439636,1.300028204917908,1.884429812431335,0.9555113911628723,1.582087278366089,1.755614042282104,1.513103246688843,2.251891613006592,2.364432334899902,0.1053504794836044,1.399728775024414,0.988462507724762,1.090647339820862,1.154614448547363,1.266947627067566,2.885191679000854,1.228880047798157,1.203962802886963,1.357380270957947,0.8377236127853394,0.5369611382484436,0.7487238049507141,2.295872688293457,1.107803225517273,0.6208452582359314,-2.054163694381714,1.892012000083923,1.729724526405334,0.4693784117698669,0.9808416962623596,2.069492340087891,1.675188183784485,1.386294364929199,1.799214959144592,1.832581520080566,1.090647339820862,1.443123579025269,1.250360131263733,1.602312564849854,1.018558502197266,1.297053217887878,1.685194492340088,-0.4209848940372467,1.562094688415527,2.146527528762817,2.347462892532349,0.9698315262794495,1.924146413803101,1.62672758102417,-0.03926072642207146,1.460148692131042,1.955393552780151,0.9263598918914795,2.066191673278809,1.422843217849731,2.10103178024292,2.261461019515991,0.7013137936592102,2.031012535095215,1.162369251251221,0.4700036346912384,1.41059672832489,0.3930551111698151,1.290994167327881,0,0.9571254849433899,0.5596157908439636,1.568615913391113,1.710187911987305,1.41059672832489,0.2231435477733612,0.5108456015586853,1.332392454147339,0.8601858615875244,2.322779893875122,1.91959547996521,1.976106762886047,0.8954347372055054,0.18123759329319,0.4953058362007141,0.5777924060821533,1.07881772518158,1.603198528289795,0.6208452582359314,2.083894014358521,1.379169106483459,1.112383723258972,1.067121624946594,1.118806958198547,1.588541030883789,1.390311241149902,1.714806437492371,0.2010615319013596,0.9872710108757019,0.9835006594657898,2.233170747756958,1.143617510795593,-0.6113829016685486,2.153052091598511,1.299837350845337,0.8409204483032227,1.058484435081482,1.152658462524414,1.293575882911682,1.832581520080566,2.327180147171021,1.166146278381348,2.034993171691895,0.6792510747909546,1.547136902809143,0.7530185580253601,0.8472836017608643,0.8711259961128235,0.2282504737377167,0.08965782821178436,1.321755886077881,1.196101903915405,1.636118769645691,1.892012000083923,1.518308997154236,2.472159147262573,1.321755886077881,1.473641037940979,1.369478821754456,1.203962802886963,1.198729157447815,1.270209908485413,0.4700036346912384,0.7999816536903381,1.565945625305176,1.758978009223938,0.858025848865509,0.6931471824645996,0.6418538689613342,1.633740186691284,1.703747630119324,1.844004034996033,1.966118812561035,0.8649974465370178,0.9333052039146423,0.7792331576347351,0.9555113911628723,1.316247344017029,1.475906491279602,1.491397261619568,1.455750465393066,0.5108456015586853,1.180438041687012,1.688489437103271,0.7907274961471558,1.401798605918884,-0.4335560202598572,1.683171510696411,-1.766676664352417,3.155595064163208,2.259521007537842,1.306926369667053,0.7984976768493652,0.5590441823005676,0.1479026228189468,1.944494843482971,1.378337860107422,3.064745187759399,-0.7419173121452332,0.7657003998756409,0.619392991065979,1.465452075004578,2.18925952911377,1.021659255027771,0.9770094752311707,0.9162907600402832,2.905096054077148,-0.1996711939573288,0.6931471824645996,2.733392953872681,1.868334650993347,2.120259523391724,1.515193223953247,0.9146093130111694,1.499556064605713,0.803077220916748,0.7280316352844238,0.5164099931716919,1.22644829750061,0.9162907600402832,1.376471281051636,1.828974962234497,1.368283152580261,1.064710736274719,1.406489133834839,1.047318935394287,1.948093414306641,1.078001379966736,0.6539384722709656,1.927891612052917,1.361027836799622,0.6931471824645996,1.604686617851257,0.1839036494493484,3.113515377044678,1.926829218864441,1.2701256275177,0.6826927065849304,1.68106997013092,0.5562959909439087,1.628220438957214,0.9162907600402832,1.341558456420898,0,1.122231245040894,0.5401707887649536,1.391505718231201,1.697173953056335,3.218875885009766,0.871167778968811,1.167329549789429,1.216987729072571,0.5753766298294067,1.151615738868713,0.9942512512207031,0.5263249278068542,-1.543182134628296,1.91204309463501,0.554287314414978,0.9162907600402832,1.500939130783081,0.9446837902069092,1.241268634796143,1.564984321594238,0.8380264639854431,1.668857097625732,1.769428610801697,1.22644829750061,1.406489133834839],[12,12,12,12,14,12,16,12,12,12,12,11,12,12,10,11,12,12,12,12,16,12,13,12,12,17,12,12,17,12,11,16,13,12,16,11,12,10,14,17,12,12,16,12,12,12,16,12,12,12,12,12,12,8,10,16,14,17,14,12,14,12,8,12,12,8,17,12,12,12,12,12,9,10,12,12,12,17,15,12,6,14,12,14,9,17,13,9,15,12,12,12,12,12,12,12,12,13,12,13,12,12,12,16,12,13,11,12,12,12,17,14,16,17,12,11,12,12,17,10,13,11,12,16,17,12,16,12,16,8,12,12,12,13,11,12,12,14,12,12,12,17,14,12,9,12,12,12,14,16,17,15,12,16,17,17,12,16,13,12,11,16,14,16,12,9,17,14,12,12,11,12,12,10,12,5,17,11,12,12,14,11,12,14,12,10,16,13,12,12,12,11,12,9,13,12,12,12,13,16,12,16,17,12,12,9,12,12,13,12,12,12,12,10,12,16,12,11,12,10,12,12,12,12,16,17,12,17,12,12,12,8,12,13,12,12,8,12,17,17,12,13,12,12,12,12,9,10,12,16,13,8,16,13,12,11,13,12,12,10,12,17,15,16,10,11,12,12,14,16,14,8,7,12,12,14,12,12,12,14,16,12,12,12,13,13,10,12,12,12,12,14,17,10,9,12,12,16,12,17,12,17,11,16,11,13,11,8,11,12,10,17,12,12,17,14,12,12,12,12,12,12,9,10,12,12,12,12,12,17,12,17,12,10,12,12,12,12,12,12,16,13,13,12,16,17,12,14,12,17,12,14,12,12,17,16,16,12,9,12,12,16,14,12,12,11,12,16,17,17,14,12,14,12,10,12,13,16,12,7,16,14,12,10,12,16,10,12,14,12,6,15,12,17,14,13,6,16,14,15,14,8,14,12,12,12,12,12,12,8,12,17,12,12,14,13,17,8,12,11,12,12,17,10,12,13,12,12],[14,5,15,6,7,33,11,35,24,21,15,14,0,14,6,9,20,6,23,9,5,11,18,15,4,21,31,9,7,7,32,11,16,14,27,0,17,28,24,11,1,14,6,10,6,4,10,22,16,6,12,32,15,17,34,9,37,10,35,6,19,10,11,15,12,12,14,11,9,24,12,13,29,11,13,19,2,24,9,6,22,30,10,6,29,29,36,19,8,13,16,11,15,6,13,22,24,2,6,2,2,14,9,11,9,6,19,26,19,3,7,28,13,9,15,20,29,9,1,8,19,23,3,13,8,17,4,15,11,7,0,0,10,8,2,4,6,18,3,22,33,28,23,27,11,6,11,14,17,17,14,11,7,8,6,8,4,25,24,11,19,9,19,14,22,6,23,15,6,11,2,22,10,14,12,9,13,18,8,11,9,9,14,9,2,12,15,11,7,9,19,11,8,13,4,7,19,14,14,3,9,7,7,14,29,19,14,16,10,12,24,6,9,14,26,7,4,15,23,1,29,9,6,11,17,6,7,2,24,4,11,25,11,2,19,7,2,20,10,19,17,12,11,6,10,4,2,13,21,9,4,2,19,4,9,14,6,24,1,13,3,10,16,9,19,4,10,5,7,3,38,16,13,1,7,15,10,2,19,25,25,7,15,11,25,19,4,14,19,18,14,11,4,29,21,24,19,31,28,15,27,13,4,10,8,4,18,3,11,8,10,33,19,35,21,7,18,4,12,16,14,3,1,27,12,6,9,2,6,9,16,22,26,11,11,15,13,6,20,17,8,13,15,14,14,6,24,10,2,9,23,12,8,16,10,7,19,2,9,14,9,16,7,6,22,9,9,14,17,12,13,8,10,16,1,6,4,8,4,15,7,14,16,15,23,19,4,12,12,25,14,14,11,7,18,4,37,13,14,17,5,2,0,3,21,20,19,4,19,11,14,8,13,24,1,1,3,4,21,10,13,9,14,2,21,22,14,7],[196,25,225,36,49,1089,121,1225,576,441,225,196,0,196,36,81,400,36,529,81,25,121,324,225,16,441,961,81,49,49,1024,121,256,196,729,0,289,784,576,121,1,196,36,100,36,16,100,484,256,36,144,1024,225,289,1156,81,1369,100,1225,36,361,100,121,225,144,144,196,121,81,576,144,169,841,121,169,361,4,576,81,36,484,900,100,36,841,841,1296,361,64,169,256,121,225,36,169,484,576,4,36,4,4,196,81,121,81,36,361,676,361,9,49,784,169,81,225,400,841,81,1,64,361,529,9,169,64,289,16,225,121,49,0,0,100,64,4,16,36,324,9,484,1089,784,529,729,121,36,121,196,289,289,196,121,49,64,36,64,16,625,576,121,361,81,361,196,484,36,529,225,36,121,4,484,100,196,144,81,169,324,64,121,81,81,196,81,4,144,225,121,49,81,361,121,64,169,16,49,361,196,196,9,81,49,49,196,841,361,196,256,100,144,576,36,81,196,676,49,16,225,529,1,841,81,36,121,289,36,49,4,576,16,121,625,121,4,361,49,4,400,100,361,289,144,121,36,100,16,4,169,441,81,16,4,361,16,81,196,36,576,1,169,9,100,256,81,361,16,100,25,49,9,1444,256,169,1,49,225,100,4,361,625,625,49,225,121,625,361,16,196,361,324,196,121,16,841,441,576,361,961,784,225,729,169,16,100,64,16,324,9,121,64,100,1089,361,1225,441,49,324,16,144,256,196,9,1,729,144,36,81,4,36,81,256,484,676,121,121,225,169,36,400,289,64,169,225,196,196,36,576,100,4,81,529,144,64,256,100,49,361,4,81,196,81,256,49,36,484,81,81,196,289,144,169,64,100,256,1,36,16,64,16,225,49,196,256,225,529,361,16,144,144,625,196,196,121,49,324,16,1369,169,196,289,25,4,0,9,441,400,361,16,361,121,196,64,169,576,1,1,9,16,441,100,169,81,196,4,441,484,196,49],[7,7,7,7,14,7,7,3,7,7,3,7,16,10,7,10,7,12,7,7,16,10,3,7,7,14,7,7,12,12,7,3,10,14,12,3,3,3,7,17,12,9,16,3,7,7,16,10,7,7,7,3,7,7,3,12,7,17,7,7,3,12,7,7,7,12,16,7,7,7,12,10,9,0,10,14,7,3,12,12,7,17,3,7,7,12,7,7,12,10,0,12,10,7,7,7,3,12,7,12,7,7,10,14,7,12,7,7,10,7,12,7,7,17,7,7,7,10,10,12,7,12,7,10,7,10,7,7,7,7,7,7,16,12,7,3,7,7,7,12,7,12,12,14,7,7,7,12,12,14,10,12,7,16,7,17,3,10,9,7,3,16,12,7,7,7,12,3,7,7,7,7,10,10,7,12,17,10,7,7,12,7,12,7,7,7,7,14,7,12,7,7,12,3,7,12,12,7,7,14,12,17,17,7,7,10,7,7,7,3,0,7,12,7,7,12,7,7,12,7,7,3,7,7,10,12,7,12,7,7,7,7,12,7,7,7,7,7,14,17,7,10,7,7,12,7,7,9,7,14,7,3,16,3,16,7,16,12,7,7,12,12,12,16,7,9,7,12,12,10,7,12,7,7,3,10,17,7,3,3,12,7,7,7,10,7,10,7,9,9,12,12,12,7,9,12,7,12,7,12,12,14,7,14,10,12,7,7,12,7,7,12,12,12,12,14,10,7,7,7,7,7,7,7,7,7,12,12,7,14,10,12,7,7,12,7,10,12,10,7,14,7,12,12,7,16,0,12,7,17,7,12,3,7,14,7,12,12,7,7,14,7,12,12,7,7,7,7,3,12,7,7,14,10,10,10,10,12,3,7,16,7,7,0,7,7,10,7,12,7,7,7,7,16,12,10,7,7,16,7,7,7,12,10,10,10,7,10,12,12,7,17,12,16,10,16,7,7,9,3,7,7,7,7,7,7,16,12],[12,7,12,7,12,14,14,3,7,7,12,14,16,10,7,16,10,12,7,12,10,12,7,7,12,16,3,3,12,12,7,3,12,7,12,10,3,10,7,14,12,9,14,3,12,12,14,10,7,12,7,7,12,7,7,12,7,17,17,12,14,12,7,7,7,12,12,12,7,12,12,10,7,0,7,12,7,3,10,7,12,12,7,7,7,7,7,7,7,10,7,12,10,12,7,7,7,14,7,12,12,7,7,14,12,10,7,7,7,7,12,7,12,10,10,7,7,7,12,7,7,12,14,12,7,10,7,7,12,10,7,7,12,10,7,12,7,7,7,7,3,12,16,7,3,12,7,12,12,16,12,12,7,14,7,10,7,14,7,7,12,12,17,7,7,3,12,7,7,7,3,7,10,10,12,7,14,10,7,7,10,12,12,7,7,7,12,7,12,12,12,10,12,10,12,12,12,7,12,12,12,12,16,7,16,7,7,10,12,10,0,7,12,12,10,12,3,7,12,10,7,7,7,7,12,12,7,12,7,10,10,7,12,17,7,7,7,7,12,14,7,12,7,7,16,7,10,12,7,16,10,3,16,7,12,7,7,7,12,12,7,10,14,16,7,10,7,14,14,12,7,7,3,7,7,7,12,10,7,3,12,7,12,7,10,7,0,7,10,9,12,12,12,3,9,12,12,14,7,12,12,12,7,12,10,12,7,7,3,12,7,16,12,12,7,14,7,7,12,10,7,3,7,7,10,7,7,12,12,12,10,14,7,7,14,10,10,7,7,7,14,7,12,14,14,14,0,16,7,12,7,10,7,10,10,14,7,12,7,7,12,14,12,7,7,7,12,16,3,16,7,16,7,10,10,10,10,12,10,7,16,7,7,7,7,7,10,10,12,7,7,7,7,14,14,7,7,7,16,12,7,7,12,12,12,10,3,12,12,12,7,16,12,10,10,12,7,7,7,7,7,7,7,7,7,7,12,12],[-0.01689361393701838,-0.6547254735284581,0.2689901571530899,-0.9253959811841497,0.3514758544493812,0.292975113425143,0.7127141556275087,0.8300483501089928,-0.5728064417300516,0.2289068300428165,0.1567740421552262,0.358621519247367,-0.05090661332045698,-0.4086782223011236,0.4081051268904199,-0.750151842413413,-0.1410703021564883,0.6263200559104434,-0.632076794076698,0.9123547975021009,0.02542345566141502,0.1109988294859601,-0.1714023776863196,-0.06649255940727383,-0.1795992153527856,-0.2911638720042924,0.09606210688098571,0.4436927739577894,0.6248286263210914,0.7193573314123289,-0.7855630497751813,-0.507790545510034,0.4437831437657636,0.04591036975133611,-0.230909252599889,0.1782416256187805,-0.4108306308313943,0.3178016994197019,0.2760931014714907,1.214200427205391,0.1571630082217373,0.1388912314151545,-0.31807214854188,0.3757093677057712,-0.3243721209084043,0.2976304990408302,-0.6810884349101567,0.1983560328118654,-0.4404813551413733,-0.04768777709355443,-0.3569447472835228,-1.183457924032221,-1.082603694890062,-0.5603641706741909,-0.4954026696182108,0.04199680706955311,0.9541755207862375,0.6408911175553298,0.1734597360381958,-0.2553592405830869,0.05924202323415684,0.1261596211924045,0.08330323343550661,0.5874339323300919,1.293854705974992,0.3391488717539144,0.4039051345850253,-0.09150604378979676,0.01434573568874642,-0.005386912768869934,0.5595468208050087,0.09459452780488498,0.5162885087924938,1.067854543890924,0.2599185005069113,0.3062420619417945,-1.899344138281742,-0.5464397610791014,-1.293766736732756,-0.0793106041466245,-1.103717818164411,-1.423692267297247,-0.06299631504874537,0.1255358375153057,-0.6392089527648945,0.5035085573616014,-0.1414735060796277,-0.1221276090388022,0.3937491601253218,-0.8597369000076913,0.3068742847931665,-0.6511132515649127,-1.130302155409376,-1.710666485837604,-1.543601374304261,-0.2932816739447264,0.2535457969250388,-0.3751067737930928,-0.1160985909019956,-0.04669680398642162,-0.4414001623562966,-0.1686322575832586,-0.2312372621101733,0.2473629316818911,0.2121790353255872,-0.7504038296676643,0.1481216980329789,-0.1527020424555183,-0.07538215985996333,-0.6216232213260737,0.8732755676968238,0.06444548091403357,0.07841639653371413,-0.04140239937227852,0.5151212196607193,-1.940430428371517,0.09672131110548809,0.6821826402145033,0.164177670039527,0.393108201787299,-0.3446850347054202,-0.1884837390249421,0.569684382986823,0.2367511285893782,0.9009257380863853,-3.098585440687809,-2.15350960685116,0.04584657957740634,-0.5426742234694761,0.7360355456531659,-0.1686477212611308,0.8637987823780904,0.05683131308693579,1.001890642085523,-0.08380456957718485,-0.005550374353182286,-0.2347599349533567,0.4211274853908917,0.2946824994800481,0.1701376442544313,0.6286277032231167,0.5070730597620621,0.03258681868203417,-0.4277802946838207,-0.7752364224110413,-0.5320114872967252,-0.06334654462140077,0.326222311851861,-1.277149609284458,0.4802635171599772,-0.03899385413872958,-0.4409189322040363,-0.4174555343217714,0.06001890948640787,0.7784114098325725,0.5719718758961387,-0.4363123076799035,-0.3359262635873348,0.05477309824230137,-0.140299598095827,-0.6020192073472701,0.2612899245927465,-1.199219005103322,-0.4228267558130478,0.09354472204373798,-0.2579527875631342,0.974474465469821,0.1499736885443292,-0.2624777040993223,-0.06698648106854432,0.1339062234411562,0.40343558832334,-0.1051207056508572,-0.6298849766633254,-0.3745227530742332,0.02946615286179777,0.1964172316720827,-0.7669952638119799,0.2511228448957625,-0.06334654462140077,0.9256284537865289,0.3894010786433344,0.3178520315860529,0.1782266202522882,2.349271126610836,-0.09449780725447399,0.3010253885757053,0.4653740428286746,-1.459175060374246,-0.8864333029789324,-0.4774134623120507,0.141139736100216,0.3464354402631942,0.4741382794088218,-0.4489387470547794,0.2500251132421629,0.584860516874961,-0.2715359216953497,0.2936433368477125,0.6007472242345728,0.4035263959365492,0.9563020066902301,1.007446099923386,-1.121696833374618,0.08996095229506329,-0.1269169018511496,-0.1363999730373602,-0.1068354749190528,0.06888417688131043,1.699738733456071,-0.09826275104859405,0.186443499513959,0.2478034202056532,-0.2665304427525741,-0.7886254876802803,-0.5468658013656476,1.348714779054868,-0.0759477335730474,-0.7043787183053662,-2.759501711276039,0.5822441773545726,0.6201476756530409,-0.5481408916031369,-0.1811171568612384,0.5479515054412272,0.3506857371107097,0.3362912732534544,0.6226270574448913,0.5054387212338156,0.143489430582273,0.2811647259016705,0.1686829639284571,0.4403537117262555,0.08755711513818165,-0.002516077668496042,0.6351914006643431,-1.045003137795559,0.2534148138484555,0.702877563935945,0.7409104536752029,-0.3061227937265969,0.6772968395981624,0.4647687279005719,-1.056780029795075,0.3234818706049412,1.008235643541563,0.2409450194730126,0.9818358318094589,0.1068507034482731,0.7458684148500097,1.252906481617248,0.0772955499008976,0.4858567248982235,0.1538147133524776,-0.6395732160610552,0.2449460441268225,-0.685860820863343,-0.03614863151886993,-0.8281312742146335,-0.1272303565259598,-0.3496645125629512,0.1249659485642409,0.2645481025404259,0.05543336293197987,-0.9536324904627047,-0.3749156790197492,0.1957256326212375,-0.1230517143064275,1.149983544879069,0.7647286619176783,0.6880908079553036,-0.1204286716202936,-0.7189283621956969,-0.3328254380139194,-0.4722106855935915,-0.2891231198892035,0.4665317067636936,-0.2487595001629983,0.784324718802146,-0.0708878328127418,-0.4604664733575374,0.01711853327084945,-0.1263406295519272,0.426582177760191,0.001650930513855009,0.353840513275842,-0.6233031200169207,-0.2397763019825201,-0.3160686360905847,0.9445099703875919,-0.08342980206262873,-1.896135012112455,0.8989110390591506,0.1128627854362945,-0.2908821801177726,-0.2686583637652689,-0.1469108330319604,-0.2422428897771485,0.5157428237751134,0.7750494161197745,-0.1559653523238964,0.520860929601465,-0.2065102057874799,0.1648835666424251,-0.2662737541166602,-0.1612709361378788,-0.3561381525963885,-0.435443315028581,-1.010904396251659,0.2410669452757064,0.1822283397096127,0.06563068968308738,0.5924427045275489,0.2887231385840294,0.8491834895603438,0.1489595370818275,0.1849802605716129,0.4223209125158668,0.01850985734217914,-0.0627207660186011,0.0431625956271906,-0.4392766687156764,0.1560402654561674,0.3666272519202405,0.5735250636791542,-0.1594934545074949,-0.416429668287694,-0.2277508894375955,0.6162208833182803,0.2871876360662591,0.5825541115296167,0.3376284542007306,-0.4605891793917061,-0.1058603918886472,-0.3827256954888629,-0.2896361965876022,0.1090982452273703,0.4583871879065982,0.1827173870524961,0.17979614538702,-0.815429853884106,-0.08810768576280115,0.3819452206926426,-0.4363198167110662,-0.07083522157995459,-1.758058466933632,0.3560287118496603,-3.026136743198827,2.285990305764278,0.8429610134847771,-0.01829760687424442,-0.5097485260157268,-0.5216447585016069,-1.113547300647469,0.5008448786560988,0.08274825379106021,1.519589377562408,-1.611522070544163,-0.1596865648961898,-0.607654321792243,0.355875224252284,0.6822230910067364,-0.1511370939682823,-0.04050982814183324,-0.4052164550192503,1.856915831985009,-1.309248044709622,-0.7794866450342393,1.150455490565863,0.3758985621477922,0.7903171672817568,0.4345042831510726,-0.3448507658352402,0.2381061411392968,0.09773920402242309,-0.2894876680885801,-0.4921445447270512,-0.099827157942181,-0.03086714919830558,0.4383068366019325,0.5333853559181354,0.01844258240173091,-0.1967391871916968,0.2841348034046727,-0.2779050411470105,0.4029376041096493,0.2536367280484556,-0.5315144732738182,0.6196454091878254,0.03376415482372863,-0.165520358432697,0.1934494190125726,-0.9780552036742496,1.756529142068162,0.5153751841747662,0.2615710896189571,-0.1374025452274812,0.2283343567006453,-0.7935445792346216,0.1680762329707053,-0.1897400731739771,0.7175402126625858,-0.9076531081743348,0.2129509416339788,-0.7758217256365048,0.08282584366412937,0.397604657499961,2.271717975771177,-0.4284015165875634,0.2509572113064478,-0.01005958378565119,-0.812295454273539,-0.0555333599209451,-0.3328915476260477,-0.4245996037280877,-2.432710037503083,0.6957796479273237,-0.1472840801829937,-0.3997017543611752,0.4256689379171341,-0.2624653085827493,0.131691784043849,0.03095386543524503,0.09121496290682196,0.3528645832242741,0.3865247670820091,-0.000599015357611643,0.3564860421590941]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>lwage<\/th>\n      <th>educ<\/th>\n      <th>exper<\/th>\n      <th>expersq<\/th>\n      <th>fatheduc<\/th>\n      <th>motheduc<\/th>\n      <th>resid_iv_mf<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tip","pageLength":10,"rownames":false,"columnDefs":[{"targets":8,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 4, 3, \",\", \".\", null);\n  }"},{"targets":2,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 2, 3, \",\", \".\", null);\n  }"},{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"id","targets":1},{"name":"lwage","targets":2},{"name":"educ","targets":3},{"name":"exper","targets":4},{"name":"expersq","targets":5},{"name":"fatheduc","targets":6},{"name":"motheduc","targets":7},{"name":"resid_iv_mf","targets":8}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":["options.columnDefs.0.render","options.columnDefs.1.render"],"jsHooks":[]}</script>
```


:::
:::



::: {.notes}
This table shows the  new data set after adding the IV estimation residuals.
:::

##

### Run the auxiliary regression


We run the auxiliary regression with `R` code lines:



::: {.cell layout-align="center"}

```{.r .cell-code}
# set model formula
mod_jtest <- formula(resid_iv_mf ~ exper +expersq +motheduc +fatheduc)
# OLS estimate
lm_jtest <- lm(formula = mod_jtest, data = mroz_resid)
```
:::



Then we can obtain the OLS estimation results.


<!---.scroll-box-20[--->



::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = mod_jtest, data = mroz_resid)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.1012 -0.3124  0.0478  0.3602  2.3441 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)  1.096e-02  1.413e-01   0.078    0.938
exper       -1.833e-05  1.333e-02  -0.001    0.999
expersq      7.341e-07  3.985e-04   0.002    0.999
motheduc    -6.607e-03  1.189e-02  -0.556    0.579
fatheduc     5.782e-03  1.118e-02   0.517    0.605

Residual standard error: 0.6752 on 423 degrees of freedom
Multiple R-squared:  0.0008833,	Adjusted R-squared:  -0.008565 
F-statistic: 0.0935 on 4 and 423 DF,  p-value: 0.9845
```


:::
:::



<!---]--->


::: {.notes}
Remind that the model summary gives a F test result, which is differnt with the F statistic in  J-test.
:::

##

### J-test: using the restricted F statistics

As what we have done before, We conduct the restrict F-test for the auxiliary regression.

We will restrict jointly with $\theta_1 = \theta_2 =0$, and using the R function `linearHypothesis()`:



::: {.cell layout-align="center"}

```{.r .cell-code}
restricted_ftest <- linearHypothesis(
  lm_jtest, c("motheduc = 0", "fatheduc = 0"), 
  test = "F"
  )
# extract the F statistics value
F_star <- restricted_ftest[2, 5]
```
:::

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}

```
Linear hypothesis test

Hypothesis:
motheduc = 0
fatheduc = 0

Model 1: restricted model
Model 2: resid_iv_mf ~ exper + expersq + motheduc + fatheduc

  Res.Df    RSS Df Sum of Sq     F Pr(>F)
1    425 193.02                          
2    423 192.85  2    0.1705 0.187 0.8295
```


:::
:::



The restricted F-statistics is 0.1870 (with round digits 4 here ).

::: {.notes}
Please pay attention to the code `c("motheduc = 0", "fatheduc = 0")`
:::

##

### J-test: calculate the J statistics by hand

Finally, We can calculate J-statistic by hand or obtain it by using special tools.


- Calculate J-statistic by hand



::: {.cell layout-align="center"}

```{.r .cell-code}
# numbers of instruments
m <- 2
# calculate J statistics
J_calc <- m*restricted_f
```
:::




- The calculated J-statistic is 0.3740 (with round digits 4 here ).


##

### J-test: using the ${\chi^2}$ statistics

Also, We can obtain J-statistic by using special tools.



::: {.cell layout-align="center"}

```{.r .cell-code}
jtest <- linearHypothesis(
  lm_jtest, c("motheduc = 0", "fatheduc = 0"), 
  test = "Chisq"
  )
# extract chi square value
J_star <- jtest[2, 5]
```
:::

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}

```
Linear hypothesis test

Hypothesis:
motheduc = 0
fatheduc = 0

Model 1: restricted model
Model 2: resid_iv_mf ~ exper + expersq + motheduc + fatheduc

  Res.Df    RSS Df Sum of Sq Chisq Pr(>Chisq)
1    425 193.02                              
2    423 192.85  2    0.1705 0.374     0.8294
```


:::
:::




- We obtain the J-statistic 0.3740 (with round digits 4 here ). It's the same as what we have calculated by hand!

::: {.notes}
In `R`, we can use the function `linearHypothesis(.,  test = "Chisq")` by setting argument `test = "Chisq"`.Please check that the relations between restricted F statistics and the $\chi^2$ statistics.
:::

##

### J-test: adjust the freedoms

::: {.callout-note}

**Caution**: In this case the$p$-Value reported by `linearHypothesis(., test = "Chisq")` is wrong because the degrees of freedom are set to  2, and the correct freedom should be $f=(m-k)=1$.

:::

- We have obtain the J statistics${\chi^2}^{\ast} =0.3740$, and its correct freedom is $f=(m-k)=1$.

- Then we may compute the correct$p$-Value of this the J statistics 0.5408 (by using function `pchisq()` in `R`).



::: {.cell layout-align="center"}

```{.r .cell-code}
# correct freedoms
f <- m -1
# compute correct p-value for J-statistic
pchi <- pchisq(J_star, df = f, lower.tail = FALSE)
```
:::



::: {.notes}
This differs from the degree of overidentification ($f=m−k=2−1=1$). So the$J$-statistic is$\chi^2(1)$ distributed instead of following a  $\chi^2(2)$ distribution as assumed defaultly by `linearHypothesis()`.
:::

##

### J-test: the conclutions

Now we can get the conclusions of J-test.

Since the p-value of  J-test(0.5408)is larger than the criteria value 0.1,  we can't reject the null hypothesis that both instruments are exogenous.

This means both instruments(
`motheduc` and `fatheduc`) are **exogenous**.



::: {.notes}
Finally, we go through all instrument validity tests in this section.the next section we will illustrate how to test regressor endogeneity.
:::

## Class Exercises

### Three IVs for `educ`

For the wage case, and the origin model(Mis-specificated) was assumed to have only one endogenous variable(`educ`)

$$
\begin{align}
lwage = {\beta}_1 +{\beta}_2{educ} + {\beta}_3exper +{\beta}_4expersq + {\epsilon}
\end{align}
$$


If we add husband’s education(`huseduc`) to the IV list, then we will have totally three IVs  (`fatheduc`, `motheduc` and `huseduc`) for `educ`.

-  Use these three IVs to obtain TSLS results. Compare the TSLS results when using two IVs(`fatheduc` and `motheduc`) which we have got.

- Conduct the over-identification test (J-test).




# 17.6 Testing Regressor endogeneity{#endogeneity .monash-bg-blue .mcenter}

::: {.notes}
In this section, we focus mainly on regressor endogeneity issues.
:::


## Regressor Endogeneity

### The concepts

It is the researcher’s responsibility to specify which variables are endogenous and which are exogenous. So, how can we test the regressor endogeneity?

Since OLS is in general more efficient than IV (recall that if Gauss-Markov assumptions hold OLS is BLUE), we don't want to use IV when we don't need to get the consistent estimators.

Of course, if we really want to get a consistent estimator, we also need to check whether the endogenous regressors are really **endogenous** in the model.

So we should test following hypothesis:

$$
H_{0}: \operatorname{Cov}(X, \epsilon)=0 \text { vs. } H_{1}: \operatorname{Cov}(X, \epsilon) \neq 0
$$



## Hausman test(regressor endogeneity test solution)

### Introduction of Hausman test

`Hausman` tells us that we should use OLS if we fail to reject$H_{0}$. And we should use IV estimation if we reject$H_{0}$

Let's see how to construct a `Hausman test`. While the idea is very simple.

- If$X$ is **[ex]{.red}ogenous** in fact, then both OLS and IV are consistent, but OLS estimates are more efficient than IV estimates.

- If$X$ is **[en]{.red}dogenous** in fact, then the results from OLS estimators are different, while results obtained by IV (eg. 2SLS) are consistent.

::: {.aside}
In fact, the general class of tests are called**Durbin-Wu-Hausman** tests, **Wu-Hausman** tests, or **Hausman** tests.
:::

##

### The idea Hausman test

We can compare the difference between estimates computed using both OLS and IV.

- If the difference is **small**, we can conjecture that both OLS and IV are consistent and the small difference between the estimates is not systematic.
- If the difference is **large** this is due to the fact that OLS estimates are not consistent. We should use IV in this case.


##

### The Hausman statistics


The **Hausman test** takes the following statistics form ($k=$ numbers of edogenous regressor
)

$$
\begin{align}
\hat{H}=n\boldsymbol{\left[\hat{\beta}_{IV}-\hat{\beta}_{\text {OLS}}\right] ^{\prime}\left[\operatorname{Var}\left(\hat{\beta}_{IV}-\hat{\beta}_{\text {OLS}}\right)\right]^{-1}\left[\hat{\beta}_{IV}-\hat{\beta}_{\text {OLS}}\right]} \xrightarrow{d} \chi^{2}(k)
\end{align}
$$

- If$\hat{H}$ is less than the critical$\chi^2$ value, we can not reject the null hypothesis, and the regressor should **not be endogenous**.

- If$\hat{H}$ is **larger** than the critical$\chi^2$ value,
the null hypothesis is rejected , and the regressor should **be endogenous**.

::: {.aside}
The three authors' approaches yield  the same statistic except for possible differences regarding the choice of$\hat{\sigma}^2$.- Durbin (1954) proposed setting$\hat{\sigma}^2$ to be the OLS estimator of$\sigma^2$.- Wu (1973) proposed a set of possible estimator$\hat{\sigma}^2$.- Hausman (1978) proposed a Wald statistic.
:::

##

### The procedure of Hausman test

The origin model is

$$
\begin{align}
Y_{i}=\hat{\alpha}_{0} + \alpha_1 X_i+\hat{\beta}_{1} W_{1 i}+\ldots+\hat{\beta}_{p} W_{p i} +u_{i} \quad \text{(origin model)}
\end{align}
$$

- Conduct the **first-stage regression** of 2SLS estimation and obtain the residuals $v_i$.

$$
\begin{align}
X_{i}=\hat{\gamma}_{0}+\hat{\gamma}_{1} W_{1 i}+\ldots+\hat{\gamma}_{p} W_{p i}+ \hat{\theta}_{1} Z_{1 i}+\ldots+\hat{\theta}_{q} Z_{q i}+v_{i} \quad \text{(reduced model)}
\end{align}
$$

-  Then estimate the control function by least squares

$$
\begin{align}
Y_{i}=\hat{\delta}_{0} + \hat{\delta}_1 X_i + \hat{\delta}_{1} W_{1 i}+\ldots+\hat{\delta}_{p} W_{p i}+ \hat{\lambda}_{1} v_i+ u_{i} \quad \text{(control model)}
\end{align}
$$

- Conduct the **Restricted F-test** with$H_0: \lambda_1=0$ (**Wu-Hausman F-test**).

- If the$F$-statistic is lager than  critical value, the regressor$X_i$ is **Endogenous**.

::: {.aside}
The restricted F statistics is equivalent to the square of t statistics of $\hat{\lambda}$ in the  control function, which is$t^2_{\hat{\lambda}} = F^{\ast} \quad \text{(Wu-Hausman F)}$
:::

## Wage example: Hausman test

### The origin model and instruments 

The origin model is

$$
\begin{aligned}
lwage & = \hat{\alpha}_0 +\hat{\alpha}_1 {educ} + \hat{\beta}_1 exper +\hat{\beta}_2 expersq + u_i  && \text{(origin model)}
\end{aligned}
$$



Again, we use both $matheduc$ and $fatheduc$ as instruments for $educ$ in our IV model setting.

$$
\begin{cases}
\begin{align}
{educ} &= \hat{\gamma}_0 +\hat{\gamma}_1exper + \hat{\gamma}_2expersq + \hat{\theta}_1motheduc + \hat{\theta}_2fatheduc +v_i && \text{(stage 1)}\\
lwage & = \hat{\eta}_1 +\hat{\eta}_2\widehat{educ} + \hat{\eta}_3exper +\hat{\eta}_4expersq + e_i  && \text{(stage 2)}
\end{align}
\end{cases}
$$


##

### The `R` solutions for Hausman test


In `R`, we have at least two equivalent solutions to conduct Hausman test:


- Solution 1 (Automatically) : We can use IV model **diagnose tool** to check the Hausman test results. In fact, `R` function `summary(lm_iv_mf, diagnostics = TRUE)` by setting `diagnostics = TRUE` will give you these results (**Wu-Hausman F**).

- Solution 2 (Calculate by hand) : With Step-by-step calculation  according to  the procedures, you can obtain the **Wu-Hausman F** statistics.

So let's try both of these solutions!

##

### Solution 1: Wu-Hausman test diagnose for the IV model (R code)


<!---.scroll-box-18[--->




::: {.cell layout-align="center"}

```{.r .cell-code}
# load pkg
require(AER)
# specify model
mod_iv_mf <- formula(
  lwage ~ educ + exper + expersq
  | motheduc + fatheduc + exper + expersq
  )
# fit model
lm_iv_mf <- ivreg(formula = mod_iv_mf, data = mroz)
# summrise and diagnose the model result
summary(lm_iv_mf, diagnostics = TRUE)
```
:::




##

### Solution 1: Wu-Hausman test diagnose for the IV model (R result)




::: {.cell layout-align="center"}

```{.r .cell-code}
### ==== solution 1 for Hausman test  (full model diagnose) ====
summary(lm_iv_mf, diagnostics = TRUE)
```

::: {.cell-output .cell-output-stdout}

```

Call:
ivreg(formula = mod_iv_mf, data = mroz)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.0986 -0.3196  0.0551  0.3689  2.3493 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.0481003  0.4003281   0.120  0.90442   
educ         0.0613966  0.0314367   1.953  0.05147 . 
exper        0.0441704  0.0134325   3.288  0.00109 **
expersq     -0.0008990  0.0004017  -2.238  0.02574 * 

Diagnostic tests:
                 df1 df2 statistic p-value    
Weak instruments   2 423    55.400  <2e-16 ***
Wu-Hausman         1 423     2.793  0.0954 .  
Sargan             1  NA     0.378  0.5386    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6747 on 424 degrees of freedom
Multiple R-Squared: 0.1357,	Adjusted R-squared: 0.1296 
Wald test: 8.141 on 3 and 424 DF,  p-value: 2.787e-05 
```


:::
:::





##

### Solution 2: the equivalent F statistics of Hausman test (R code)

<!---.scroll-box-18[--->




::: {.cell layout-align="center"}

```{.r .cell-code}
### ==== solution 2 for Hausman test (calculate) ====
## guide with Hansen's chpt 12.29 Endogeneity test
## reduced function for endogenous education
red_mf <- formula(educ ~ exper + expersq + motheduc + fatheduc)
fit_red_mf<- lm(formula = red_mf, data = mroz)
## extract residual u2 and combined new dataset
resid_mf <- data.frame(resid_mf = resid(fit_red_mf))
tbl_mf <- cbind(mroz, resid_mf)
## control function OLS estimation
control_mf <- formula(lwage ~ educ +exper + expersq  + resid_mf)
fit_control_mf <- lm(formula = control_mf, data = tbl_mf)
smry_control_mf <- summary(fit_control_mf)

## extract t statistics of alpha
t_star_resid <- pull(
as_tibble(t(smry_control_mf$coefficients[,"t value"])),
"resid_mf")
## calculate equivalent F statistics
restricted_F_mf <- linearHypothesis(model = fit_control_mf, "resid_mf=0")
F_star_resid <- restricted_F_mf$F[2]
p_F_resid <- restricted_F_mf$`Pr(>F)`[2]
```
:::



##

### Solution 2: the equivalent F statistics of Hausman test (R result)



::: {.cell layout-align="center"}

```{.r .cell-code}
# the OLS result of control model
smry_control_mf
```

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = control_mf, data = tbl_mf)

Residuals:
     Min       1Q   Median       3Q      Max 
-3.03743 -0.30775  0.04191  0.40361  2.33303 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.0481003  0.3945753   0.122 0.903033    
educ         0.0613966  0.0309849   1.981 0.048182 *  
exper        0.0441704  0.0132394   3.336 0.000924 ***
expersq     -0.0008990  0.0003959  -2.271 0.023672 *  
resid_mf     0.0581666  0.0348073   1.671 0.095441 .  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.665 on 423 degrees of freedom
Multiple R-squared:  0.1624,	Adjusted R-squared:  0.1544 
F-statistic:  20.5 on 4 and 423 DF,  p-value: 1.888e-15
```


:::
:::

::: {.cell layout-align="center"}

```{.r .cell-code}
# t statistics
number(t_star_resid, 0.0001)
```

::: {.cell-output .cell-output-stdout}

```
[1] "1.6711"
```


:::

```{.r .cell-code}
# F statistics and probability
number(F_star_resid, 0.01)
```

::: {.cell-output .cell-output-stdout}

```
[1] "2.79"
```


:::

```{.r .cell-code}
number(p_F_resid, 0.001)
```

::: {.cell-output .cell-output-stdout}

```
[1] "0.095"
```


:::
:::





##

### Conclusions of Hausman test

The results for the `lwage` origin equation are as follows:

- **(Wu-)Hausman test** for endogeneity: **barely reject** the null that the variable of concern is uncorrelated with the error term, indicating that  `educ`  is marginally endogenous. The Hausman statistics$\hat{H}= {\chi^2}^{\ast} = 2.79$, and its p-value is 0.095.

- **Weak instruments test**: **rejects** the null hypothesis(Weak instruments). At least one of these instruments(`motheduc` or `fatheduc`) is strong. The **restricted F-test** statistics$F^{\ast}_R = 55.4$, and its p-value is 0.0000.

- **Sargan overidentifying restrictions**(Instruments exogeneity J-test): **does not** reject the null. The extra instruments (`motheduc` and `fatheduc`)  are valid (both are exogenous, and are uncorrelated with the error term).


::: {.notes}
So far, We have finished both the instrument validity test and the regressor endogeneity test.Now, I will show you two examples. You can download the data set and go through all these test we have discussed.
:::


# chapter Summary

- An **instrumental variable** must have two properties:

- (1) it must be exogenous, that is, uncorrelated with the error term of the structural equation;
- (2) it must be partially correlated with the endogenous explanatory variable.

> Finding a variable with these two properties is usually challenging.

- Though we can **never** test whether [all]{.red} IVs are **exogenous**, we can test that at least [some of]{.red} them are.

- When we have valid instrumental variables, we can test whether an explanatory variable is **endogenous**.

- The method of **two stage least squares**  is used routinely in the empirical social sciences.

>  But when instruments are poor, then 2SLS can be **worse** than OLS.


# Exercise and Computation{#exercise .monash-bg-blue .mcenter}



::: {.cell layout-align="center"}

:::





## Card wage case

### Introduction

With data set `Card1995.dta`, researchers were interest in the return (`log(Wage)`) to education (`edu`), and they mainly focus the effect of students living region nearby the college.

In a inﬂuential paper David Card (1995) suggested if a potential student lives close to a college this reduces the cost of attendance and thereby raises the likelihood that the student will attend college.

However, college proximity does not directly affect a student’s skills or abilities so should not have a direct effect on his or her market wage.

The origin model is


$$

\begin{aligned}
lwage & = \hat{\alpha}_0 +\hat{\alpha}_1 {educ} + \hat{\alpha}_3 exp +\hat{\alpha}_4 exp2  +\hat{\alpha}_5 black +\hat{\alpha}_6 south +\hat{\alpha}_7 urban + u_i
\end{aligned}

$$


::: {.aside}
Please follow our accompany course repository at <https://github.com/huhuaping/course-emiii-accompany> or <https://gitee.com/kevinhhp/course-emiii-accompany>.
:::


##

### variables




::: {#tbl-vars .cell layout-align="center"}

:::

::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-3c01ac17c4f786d539f6" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-3c01ac17c4f786d539f6">{"x":{"filter":"none","vertical":false,"extensions":["Buttons"],"caption":"<caption>variables and definition<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"],["obs","lwage","edu","exp","exp2","black","south","urban","college","public","private","age","age2","momedu","dadedu"],["index","quantity variable: log of wage","quantity variable: education years","quantity variable: working years","quantity variable: square working years/100","dummy: 1=black; 0=nonblack","dummy: 1=southern area; 0= other","dummy: 1=live in urban; 0= other","dummy: 1=college nearby; 0= other","dummy: 1=public college nearby; 0= other","dummy: 1=private college nearby; 0= other","quantity variable: age (years)","quantity variable: age square /100","quantity variable: mother' education years","quantity variable: father' education years"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>variable<\/th>\n      <th>definition<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"Btip","pageLength":8,"rownames":false,"columnDefs":[{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"variable","targets":1},{"name":"definition","targets":2}],"buttons":["copy","csv","excel"],"initComplete":"function(settings, json) {\n$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});\n}","order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[8,10,25,50,100]},"callback":"function(table) {\n$(\"button.buttons-copy\").css(\"background\",\"yellow\");\n                $(\"button.buttons-csv\").css(\"background\",\"orange\");\n                $(\"button.buttons-excel\").css(\"background\",\"#a88d32\");\n                $(\"button.dt-button\").css(\"padding\",\"0.2em 0.2em\");\n                $(\"button.dt-button\").css(\"font-size\",\"0.6em\");\n                $(\"button.dt-button\").css(\"margin-right\",\"0.2em\");\n                $(\"button.dt-button\").css(\"margin-bottom\",\"0.1em\");\n                $(\"button.dt-button\").css(\"line-height\",\"1em\");\n                return table;\n}"},"evals":["options.initComplete","callback"],"jsHooks":[]}</script>
```


:::
:::



##

### Models and IV list sets


Let's consider following estimation solutions:

a. Error specification model with OLS regression directly.


b. Equivalent IVs for endogenous regressors (just- identificaion)

- The IV model using `college` as instruments for `educ`

- The IV model using (`college`, `age`, `age2`) as instruments for (`edu`,`exp`,`exp2`)

c. Abundant IVs for endogenous regressors (over-identification)

- The IV model using both (`public`,`private`) as instruments for `educ`

- The IV model using both (`public`,`private`,`age`,`age2`) as instruments for (`edu`, `exp`,`exp2`)


##

### Equivalent IVs: one instrument VS one endogenous regressor

we will use `college` as instruments for `educ` in our IV model setting.

$$
\begin{cases}
\begin{align}
{edu} &= \hat{\gamma}_0 +\hat{\gamma}_1exp + \hat{\gamma}_2exp2 + \hat{\gamma}_3black + \hat{\gamma}_4south + \hat{\gamma}_5urban + \hat{\theta}_1college +v_i && \text{(stage 1)}\\
lwage & = \hat{\eta}_1 +\hat{\eta}_2\widehat{edu} + \hat{\eta}_3exp +\hat{\eta}_4exp2 +\hat{\eta}_5 black +\hat{\eta}_6 south +\hat{\eta}_7 urban+ e_i  && \text{(stage 2)}
\end{align}
\end{cases}
$$


##

### Equivalent IVs: three instruments VS threeendogenous regressors

we will use (`college`, `age`, `age2`) as instruments for (`edu`,`exp`,`exp2`) in our IV model setting.

$$
\begin{cases}
\begin{align}
{edu} &= \hat{\gamma}_0 +\hat{\gamma}_1age + \hat{\gamma}_2age2 + \hat{\gamma}_3black + \hat{\gamma}_4south + \hat{\gamma}_5urban + \hat{\theta}_1college +v_{1i} && \text{(1 of stage 1)}\\
{exp} &= \hat{\lambda}_0 +\hat{\lambda}_1age + \hat{\lambda}_2age2 + \hat{\lambda}_3black + \hat{\lambda}_4south + \hat{\lambda}_5urban + \hat{\lambda}_1college +v_{2i} && \text{(2 of stage 1)}\\
{exp2} &= \hat{\delta}_0 +\hat{\delta}_1age + \hat{\delta}_2age2 + \hat{\delta}_3black + \hat{\delta}_4south + \hat{\delta}_5urban + \hat{\delta}_1college +v_{3i} && \text{(3 of stage 1)}\\
lwage & = \hat{\eta}_1 +\hat{\eta}_2\widehat{edu} + \hat{\eta}_3exp +\hat{\eta}_4exp2 +\hat{\eta}_5 black +\hat{\eta}_6 south +\hat{\eta}_7 urban+ e_i  && \text{(stage 2)}
\end{align}
\end{cases}
$$


##

### Abundant IVs: two instruments VS one endogenous regressor

we will use both (`public`,`private`) as instruments for `educ` in our IV model setting.

$$
\begin{cases}
\begin{align}
{edu} &= \hat{\gamma}_0 +\hat{\gamma}_1exp + \hat{\gamma}_2exp2 + \hat{\gamma}_3black + \hat{\gamma}_4south + \hat{\gamma}_5urban &&
\\&+ \hat{\theta}_1public + \hat{\theta}_2private +v_i && \text{(stage 1)}\\
lwage & = \hat{\eta}_1 +\hat{\eta}_2\widehat{edu} + \hat{\eta}_3exp +\hat{\eta}_4exp2 +\hat{\eta}_5 black &&\\ &+\hat{\eta}_6 south +\hat{\eta}_7 urban+ e_i  && \text{(stage 2)}
\end{align}
\end{cases}
$$


##

### Abundant IVs: four instruments VS three endogenous regressors

we will use both (`public`,`private`,`age`,`age2`) as instruments for (`edu`, `exp`,`exp2`) in our IV model setting.

$$
\begin{cases}
\begin{align}
{edu} &= \hat{\gamma}_0 +\hat{\gamma}_1age + \hat{\gamma}_2age2 + \hat{\gamma}_3black + \hat{\gamma}_4south + \hat{\gamma}_5urban &&\\
& + \hat{\theta}_1public + \hat{\theta}_2private +v_{1i} && \text{(1 of stage 1)}\\
{exp} &= \hat{\lambda}_0 +\hat{\lambda}_1age + \hat{\lambda}_2age2 + \hat{\lambda}_3black + \hat{\lambda}_4south + \hat{\lambda}_5urban &&\\
&+ \hat{\lambda}_1public + \hat{\lambda}_2private +v_{2i} && \text{(2 of stage 1)}\\
{exp2} &= \hat{\delta}_0 +\hat{\delta}_1age + \hat{\delta}_2age2 + \hat{\delta}_3black + \hat{\delta}_4south + \hat{\delta}_5urban &&\\
&+ \hat{\delta}_1public + \hat{\delta}_2private +v_{3i} && \text{(3 of stage 1)}\\
lwage & = \hat{\eta}_1 +\hat{\eta}_2\widehat{edu} + \hat{\eta}_3exp +\hat{\eta}_4exp2 +\hat{\eta}_5 black &&\\
&+\hat{\eta}_6 south +\hat{\eta}_7 urban+ e_i  && \text{(stage 2)}
\end{align}
\end{cases}
$$


##

### Exercise tasks (1/2): compare all models

<!---.scroll-box-20[--->



::: {.cell layout-align="center"}
::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-4cdd85d781113d362e5e" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-4cdd85d781113d362e5e">{"x":{"filter":"none","vertical":false,"caption":"<caption>The stage 2 results for lwage<\/caption>","data":[["1","2","3","4","5","6","7"],["(Intercept)","edu","exp","exp2","black","south","urban"],["4.7337<br/>(0.0676)","0.0740<br/>(0.0035)","0.0836<br/>(0.0066)","-0.2241<br/>(0.0318)","-0.1896<br/>(0.0176)","-0.1249<br/>(0.0151)","0.1614<br/>(0.0156)"],["3.7528<br/>(0.8293)","0.1323<br/>(0.0492)","0.1075<br/>(0.0213)","-0.2284<br/>(0.0334)","-0.1308<br/>(0.0529)","-0.1049<br/>(0.0231)","0.1313<br/>(0.0301)"],["4.0657<br/>(0.6085)","0.1329<br/>(0.0514)","0.0560<br/>(0.0260)","-0.0796<br/>(0.1340)","-0.1031<br/>(0.0774)","-0.0982<br/>(0.0288)","0.1080<br/>(0.0497)"],["3.2680<br/>(0.6872)","0.1611<br/>(0.0408)","0.1193<br/>(0.0182)","-0.2305<br/>(0.0350)","-0.1017<br/>(0.0453)","-0.0950<br/>(0.0217)","0.1164<br/>(0.0271)"],["3.7481<br/>(0.4834)","0.1597<br/>(0.0409)","0.0470<br/>(0.0250)","-0.0323<br/>(0.1281)","-0.0640<br/>(0.0630)","-0.0857<br/>(0.0256)","0.0835<br/>(0.0412)"],["3.2220<br/>(0.7015)","0.1638<br/>(0.0416)","0.1204<br/>(0.0185)","-0.2307<br/>(0.0352)","-0.0990<br/>(0.0461)","-0.0941<br/>(0.0219)","0.1150<br/>(0.0275)"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>term<\/th>\n      <th>OLS<\/th>\n      <th>IV_c<\/th>\n      <th>IV_ca<\/th>\n      <th>2SLS_pp<\/th>\n      <th>2SLS_ppa<\/th>\n      <th>LIML_pp<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","pageLength":15,"rownames":false,"columnDefs":[{"className":"dt-center","targets":"_all"},{"visible":false,"targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"term","targets":1},{"name":"OLS","targets":2},{"name":"IV_c","targets":3},{"name":"IV_ca","targets":4},{"name":"2SLS_pp","targets":5},{"name":"2SLS_ppa","targets":6},{"name":"LIML_pp","targets":7}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[10,15,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```


:::
:::



<!---]--->

##

### Exercise tasks (2/2): conduct several tests

So we should conduct several tests as we have learned.

- Weak instrument test (Restricted F test or Cragg-donald test)

- Instrument Exogeneity test (J-test )

- Regressor Endogeneity test (Wu-Hausman test)


Find these results and get the conclusion!

## Exercise example: Card (1995)

###  Background and models

In Card (1995) education is assumed to be endogenous due to omitted **ability** or **measurement error**. The standard wage function

$$
\ln \left(w a g e_{i}\right)=\beta_{0}+\beta_{1} E d u c_{i}+\sum_{m=1}^{M} \gamma_{m} W_{m i}+\varepsilon_{i}
$$

is estimated by **Two Stage Least Squares** using a **binary instrument**, which takes value 1 if there is an **accredited 4-year public college in the neighborhood** (in the "local labour market"), 0 otherwise.

> It is argued that the presence of a local college decreases the cost of further education (transportation and accommodation costs) and particularly affects the schooling decisions of individuals with poor family backgrounds.

The set of exogenous explanatory regressors$W$ includes variables like race, years of potential labour market experience, region of residence and some family background characteristics.

##

### proxy variables and instruments

The dataset is available online at
http://davidcard.berkeley.edu/data_sets.html and consists of 3010
observations from the National Longitudinal Survey of Young Men.

- **Education** is measured by the years of completed schooling and varies
we between 2 and 18 years.

> To overcome the small sample problem, you might group the years of education into four educational levels: less than high school, high school graduate, some college and post-college education (a modified version of Acemoglu and Autor (2010) education grouping).

- Since the **actual labour market experience** is not available in the dataset, Card (1995) constructs a potential experience as **age-education-6**.

> Since all individuals in the sample are of similar age (24-34), people with the same years of schooling have similar levels of potential experience.

## Exercise example: Angrist and Krueger (1991)

###  Background and models

The data is available online at http://economics.mit.edu/faculty/angrist/data1/data/angkru1991
and consists of observations from 1980 Census documented in Census of Population and Housing, 1980: Public Use Microdata Samples.

The sample consists of men born in the United States between 1930-1949 divided into two cohorts: those born in the 30's (329509 observations) and those born in the 40's (486926 observations).

**Angrist and Krueger** (1991) estimate the conventional linear earnings function

$$
\begin{align}
\ln \left(w a g e_{i}\right)=\beta E d u c_{i}+\sum_{c} \delta_{c} Y_{c i}+\sum_{s=1}^{S} \gamma_{s} W_{s i}+\varepsilon_{i}
\end{align}
$$

for each cohort separately, by 2SLS using the **quarter of birth** as an instrument for (assumed) endogenous **education**.

##

### Available instruments

- They observe that individuals born earlier in the year (first two quarters) have less schooling than those born later in the year.

> It is a consequence of **the compulsory schooling laws**, as individuals born in the first quarters of the year reach ***the minimum school leaving age*** at the lower grade and might legally leave school with less education.


- The main criticism of Angrist and Krueger (1991) analysis, pointed out by Bound, Jaeger and Baker (1995) is that the quarter of birth is a **weak instrument**.

- A second criticism of Angrist and Krueger (1991) results, discussed by Bound and Jaeger (1996) is that quarter of birth might be **correlated** with unobserved ability and hence does [not]{.red} satisfy the **instrumental exogeneity condition**.








## End Of This Chapter{.center-h background-color="aliceblue"  background-image="pic/thank-you-gif-funny-little-yellow.gif" background-size="600px"  background-position="center" background-opacity="0.8" style="font-size:75px !important"}



::: {.notes}


So we finished all content of chapter 17.

The next three chapters will focus on SEM closely.

See you in the next class.

if you have questions, please let me know. you can leave messages by QQ or email.

Thanks. Goodbye!

:::
