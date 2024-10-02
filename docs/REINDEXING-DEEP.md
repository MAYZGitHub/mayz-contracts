# Re-indexing Methodology for Investment Funds

## Table of Contents
- [Re-indexing Methodology for Investment Funds](#re-indexing-methodology-for-investment-funds)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Investment Unit](#investment-unit)
    - [Example of an Investment Unit:](#example-of-an-investment-unit)
    - [Total Investment Units (TIU):](#total-investment-units-tiu)
    - [Note about decimals in the investment unit (IU):](#note-about-decimals-in-the-investment-unit-iu)
  - [Calculation of Fundamental Scaling Factor (K)](#calculation-of-fundamental-scaling-factor-k)
  - [Nomenclature and Formulas](#nomenclature-and-formulas)
  - [Token Removal Process](#token-removal-process)
    - [1 Calculate Scaling Factor for token (SF\_i)](#1-calculate-scaling-factor-for-token-sf_i)
      - [1.1 Choose Desired Target Removal Quantity per IU (TRQ\_IU\_i) to get Scaling Factor (SF\_i)](#11-choose-desired-target-removal-quantity-per-iu-trq_iu_i-to-get-scaling-factor-sf_i)
        - [1.1.1 Mnimal Removal Quantity (Q\_IU\_min)](#111-mnimal-removal-quantity-q_iu_min)
        - [1.1.2 Select Target Removal Quantity (TRQ\_IU\_i)](#112-select-target-removal-quantity-trq_iu_i)
        - [1.1.3 Calculate Estimated Scaling Factor (ESF\_i)](#113-calculate-estimated-scaling-factor-esf_i)
        - [1.1.4 Calculate Scaling Factor Index (SFI\_i)](#114-calculate-scaling-factor-index-sfi_i)
        - [1.1.5 Calculate Scaling Factor (SF\_i)](#115-calculate-scaling-factor-sf_i)
      - [1.2 Choose QI manually to get Scaling Factor (SF\_i)](#12-choose-qi-manually-to-get-scaling-factor-sf_i)
    - [2 Removal Quantity per IU for Token i (RQ\_IU\_i)](#2-removal-quantity-per-iu-for-token-i-rq_iu_i)
    - [3 Calculate Removal Quantity Total (RQ\_T\_i)](#3-calculate-removal-quantity-total-rq_t_i)
    - [4 Calculate Aggregate Value of Removed Token (AV\_i)](#4-calculate-aggregate-value-of-removed-token-av_i)
    - [5 Repeat for Other Tokens](#5-repeat-for-other-tokens)
    - [6 Calculate Cumulative Value of Removed Tokens (CVRT)](#6-calculate-cumulative-value-of-removed-tokens-cvrt)
  - [Token Addition Process](#token-addition-process)
    - [1 Select Tokens to Add](#1-select-tokens-to-add)
    - [2 Adjust Scaling Factor Indices for New Tokens (SFI\_i)](#2-adjust-scaling-factor-indices-for-new-tokens-sfi_i)
      - [2.1 Iterative Method](#21-iterative-method)
      - [2.2 Direct calculation Method](#22-direct-calculation-method)
        - [2.2.1 Maximum SFI for Individual Tokens](#221-maximum-sfi-for-individual-tokens)
        - [2.2.2 Balanced SFI for Multiple Tokens](#222-balanced-sfi-for-multiple-tokens)
          - [2.2.2.1 Method 1: Equal SFIs for All Tokens](#2221-method-1-equal-sfis-for-all-tokens)
          - [2.2.2.2 Method 2: SFIs Based on Price Ratios](#2222-method-2-sfis-based-on-price-ratios)
    - [3 Add Quantity per IU for Token i (AQ\_IU\_i)](#3-add-quantity-per-iu-for-token-i-aq_iu_i)
    - [4 Add Quantity Total (AQ\_T\_i)](#4-add-quantity-total-aq_t_i)
    - [5 Calculate Aggregate Value of Added Token (AV\_i)](#5-calculate-aggregate-value-of-added-token-av_i)
    - [6 Calculate Cumulative Value of Added Tokens (CVRT)](#6-calculate-cumulative-value-of-added-tokens-cvrt)
  - [Calculate Balance](#calculate-balance)
  - [Token Addition Process](#token-addition-process-1)
    - [1 Adjust Scaling Factor Indice for ADA (SFI\_ADA)](#1-adjust-scaling-factor-indice-for-ada-sfi_ada)
      - [1.1 Iterative Method](#11-iterative-method)
      - [1.2 Simplified Calculation](#12-simplified-calculation)
    - [1.3 Add Quantity per IU for ADA (AQ\_IU\_ADA)](#13-add-quantity-per-iu-for-ada-aq_iu_ada)
    - [1.4 Add Quantity Total (AQ\_T\_ADA)](#14-add-quantity-total-aq_t_ada)
    - [1.5 Calculate Aggregate Value of Added ADA (AV\_ADA)](#15-calculate-aggregate-value-of-added-ada-av_ada)
  - [Calculate Final Balance](#calculate-final-balance)
  - [Conclusion](#conclusion)


## Introduction

This document outlines the methodology for re-indexing an investment fund. The process involves removing certain tokens from the investment unit and adding others, while maintaining balance and granularity. This methodology is designed for fund managers and users to understand the mathematical principles behind the re-indexing process.

## Investment Unit

An Investment Unit (IU) is a standardized representation of the fund's composition. It consists of different tokens, each with a specific quantity. To maintain precision and avoid using floating-point numbers, all token quantities in the IU are multiplied by 100. This approach allows for two decimal places of precision while using integer arithmetic.

### Example of an Investment Unit:
- Token A: 400 (representing 4.00 in operations)
- Token B: 1500 (representing 15.00 in operations)

### Total Investment Units (TIU):
Total Investment Units: 3,456 (representing the total number of investment units in the fund)

Using this total, we can calculate the real quantity of tokens deposited:
- Token A: 4.00 * 3,456 = 13,824 real tokens
- Token B: 15.00 * 3,456 = 51,840 real tokens

### Note about decimals in the investment unit (IU):
In the investment unit (IU), token values are stored as numbers multiplied by 100 to avoid using decimals directly. This approach allows the system to represent numbers with two decimal places, while still performing calculations using integers only. For example:
- If the IU shows a value of "1500" for a token, this actually represent 15.00.

However, real tokens in the blockchain cannot be fractional. Cardano tokens are always required to be whole, integer values (e.g., you can't hold 0.5 or 0.75 of a token). This limitation means that when performing deposits, withdrawals, or any operations performed with real tokens must always result in whole integer values of tokens.

TODO: explicar que hay fracciones y decimales pero son solo metadatos y de como se muestran los valores integers.

This creates the following requirement:
- If the token value in the IU contains 1 decimal place, it implies that deposits or withdrawals must be made in multiples of 10 IUs so final resulting amount is integer:

Example with UI amount "50" (representing 0.5):
```
Deposit of 10 IUs: 0.5 * 10 = 5 tokens (valid)
Deposit of 15 IUs: 0.5 * 15 = 7.5 tokens (invalid, fractional)
```

- If the token value in the IU contains 2 decimal places, then deposits or withdrawals must be made in multiples of 100 IUs:

Example with UI amount "5" (representing 0.05):
```
Deposit of 100 IUs: 0.05 * 100 = 5 tokens (valid)
Deposit of 150 IUs: 0.05 * 150 = 7.5 tokens (invalid, fractional)
```

This ensures that, after the multiplication and division operations within the IU system, the final result is a whole integer value of real tokens.

## Calculation of Fundamental Scaling Factor (K)

The Fundamental Scaling Factor (K) is a crucial constant used throughout the re-indexing process. It's calculated using the formula:

```
K = TIU / GCD(100, TIU)
```

Where GCD is the Greatest Common Divisor, and TIU is the Total Investment Units.

Example:
- TIU: 3,456
- GCD(100, 3,456) = 4
- K = 3,456 / 4 = 864

This K value ensures that all subsequent calculations maintain the required divisibility properties.

## Nomenclature and Formulas

- Fundamental Scaling Factor (K) = TIU / GCD(100, TIU)

- Quantity Index (QI) = Any number. (With 1, get minimal Removal Quantity per IU)
- Add/Removal Quantity per IU (Q_IU) = QI * ((100 * K) / TIU)
- Minimal Add/Removal Quantity per IU (Q_IU_min) = 1 * ((100 * K) / TIU) = ((100 * K) / TIU) 

- Target Removal Quantity per IU (TRQ_IU_i). Must be >= Q_IU_min
- Estimated Scaling Factor for Token i (ESF_i) = floor((TRQ_IU_i * TIU) / 100)
  
- Scaling Factor Index for Token i (SFI_i) = floor(ESF_i / K)

- A: Scaling Factor for Token i (SF_i) = SFI_i * K
- B: Scaling Factor for Token i (SF_i) = QI * K

- Removal Quantity per IU for Token i (RQ_IU_i) = (SF_i * 100) / TIU

- A: Removal Quantity Total for Token i (RQ_T_i) = (RQ_IU_i * TIU) / 100 = SF_i
- B: Removal Quantity Total for Token i (RQ_T_i) = SF_i

- Add Quantity per IU for Token i (AQ_IU_i) = (SF_i * 100) / TIU

- A: Add Quantity Total for Token i (AQ_T_i) = (AQ_IU_i * TIU) / 100 = SF_i
- B: Add Quantity Total for Token i (AQ_T_i) = SF_i

- A. Aggregate Value of Token i (AV_i) = (RQ_T_i or AQ_T_i) * Price_i 
- B. Aggregate Value of Token i (AV_i) = SF_i * Price_i

- Cumulative Value of Removed Tokens (CVRT) = Sum(AV_i) for all removed tokens
- Cumulative Value of Added Tokens (CVAT) = Sum(AV_i) for all added tokens
  
- Balance = CVRT - CVAT
- Final Balance = CVRT - (CVAT + AV_ADA)

- Scaling Factor Index for Token ADA (SFI_ADA) = Balance / K = (CVRT - CVAT) / K
  
- A. Aggregate Value of Token ADA (AV_ADA) = AQ_T_ADA * Price_ADA = AQ_T_ADA * 1 = AQ_T_ADA
- B. Aggregate Value of Token ADA (AV_ADA) = SF_ADA * Price_ADA = SF_ADA * 1 = SF_ADA

## Token Removal Process

Example:
- Token A: Price = 21
- Token B: Price = 17

Get SF_i and then continue to calculate Removal Quantity per IU for Token i (RQ_IU_i)
  
### 1 Calculate Scaling Factor for token (SF_i) 

1. Calculate minimal removal quantity (Q_IU_min), choose desired quantity (TRQ_IU_i), and calculate SF_i.
2. You can use the formula Add/Removal Quantity per IU (Q_IU) to find all valid values directly.

#### 1.1 Choose Desired Target Removal Quantity per IU (TRQ_IU_i) to get Scaling Factor (SF_i)

- Choose a Target Removal Quantity per IU (TRQ_IU_i) for each token in the investment unit. 
- No cualquier valor de desired es posible. Debe cumplir: TRQ_IU_i must be > Q_IU_min

##### 1.1.1 Mnimal Removal Quantity (Q_IU_min)

- QI = 1 sirve para calcular Q_IU_min y con eso poner un valor minimo de Target Removal Quantity (TRQ_IU_i). 
- Valores menores generarían Estimated Scaling Factor = 0
- Valores mayores garantizan Estimated Scaling Factor >= 1

```
QI = 1
Q_IU_min = QI * ((100 * K) / TIU)
Q_IU_min = 1 * ((100 * 864) / 3,456) = 25
```

##### 1.1.2 Select Target Removal Quantity (TRQ_IU_i)

- Puedo setear cualquier valor mayor a 25. 
  
Example for Token A:

- Target Removal Quantity per IU for Token A (TRQ_IU_A): 116

##### 1.1.3 Calculate Estimated Scaling Factor (ESF_i)

```
ESF_i = floor((TRQ_IU_i * TIU) / 100)
ESF_A = floor((TRQ_IU_A * TIU) / 100)
ESF_A = floor((116 * 3,456) / 100) = 4,008
```

##### 1.1.4 Calculate Scaling Factor Index (SFI_i)

```
SFI_i = floor(ESF_i / K)
SFI_A = floor(ESF_A / K)
SFI_A = floor(4,008 / 864) = 4
```

##### 1.1.5 Calculate Scaling Factor (SF_i)

```
SF_i = SFI_i * K
SF_A = SFI_A * K
SF_A = 4 * 864 = 3,456
```

#### 1.2 Choose QI manually to get Scaling Factor (SF_i)

- Modificando los valores de QI, se puede calcular SF_i directamente.

```
QI = 4
SF_i = QI * K
SF_A = QI * K
SF_A = 4 * 864 = 3,456
```

### 2 Removal Quantity per IU for Token i (RQ_IU_i)

```
RQ_IU_i = (SF_i * 100) / TIU
RQ_IU_A = (SF_A * 100) / TIU
RQ_IU_A = (3,456 * 100) / 3,456 = 100
```

### 3 Calculate Removal Quantity Total (RQ_T_i)

```
RQ_T_i = (RQ_IU_i * TIU) / 100 = SF_i
RQ_T_A = (RQ_IU_A * TIU) / 100
RQ_T_A = (100 * 3,456) / 100 = 3,456
```

This calculation results in an integer because TIU * RQ_IU_i is always a multiple of 100, which is ensured by using the Fundamental Scaling Factor (K) in the calculations.

Also, this is a simplificated method:

```
RQ_T_i = SF_i
RQ_T_A = SF_A
RQ_T_A = 3,456
```

### 4 Calculate Aggregate Value of Removed Token (AV_i)

```
AV_i = RQ_T_i * Price_i
AV_A = RQ_T_A * Price_A
AV_A = 3,456 * 98 = 338,688
```

We can simplify the calculation of AV_i directly from SF_i:

```
AV_i = SF_i * Price_i
AV_A = SF_A * Price_A
AV_A = 3,456 * 98 = 338,688
```

### 5 Repeat for Other Tokens

Repeat steps for all tokens to be removed.

Example for Token B (Price_B = 7):
- TRQ_IU_B: 680
- ESF_B = floor((680 * 3,456) / 100) = 23,500
- SFI_B = floor(23,500 / 864) = 27
- SF_B = 27 * 864 = 23,328
- RQ_IU_B = (23,328 * 100) / 3,456 = 675
- RQ_T_B = (675 * 3,456) / 100 = 23,328
- AV_B = 23,328 * 7 = 163,296

### 6 Calculate Cumulative Value of Removed Tokens (CVRT)

```
CVRT = Sum(AV_i) for all removed tokens
CVRT = AV_A + AV_B = 338,688 + 163,296 = 501,984
```

## Token Addition Process

The goal is to add tokens that conserve the total value removed while satisfying the condition that the addition amount for each token multiplied by the total IU is a multiple of 100.

Balance Equation: CVRT - CVAT = 0

### 1 Select Tokens to Add

Choose tokens to add to replace the removed tokens. We need the prices for these new tokens.

Example:
- Token C: Price = 21
- Token D: Price = 17

### 2 Adjust Scaling Factor Indices for New Tokens (SFI_i)

We will present two methods for calculating the Scaling Factor Indices (SFI_i) for the new tokens: an iterative method and a direct calculation method.

#### 2.1 Iterative Method

Start with Scaling Factor Indices for Token C (SFI_C) and Token D (SFI_D) at 0. Incrementally increase each index and calculate the total value to be added. We'll demonstrate this process step by step:

Step 1: SFI_C = 0, SFI_D = 0
- SF_C = 0 * K = 0
- AV_C = SF_C * Price_C = 0 * 21 = 0
- SF_D = 0 * K = 0
- AV_D = SF_D * Price_D = 0 * 17 = 0
- CVAT = AV_C + AV_D = 0 + 0 = 0
- Balance = CVRT - CVAT = 501,984 - 0 = 501,984 (positive)

Because the balance is positive, we continue to the next iteration.

Step 2: SFI_C = 1, SFI_D = 0
- SF_C = 1 * K = 864
- AV_C = SF_C * Price_C = 864 * 21 = 18,144
- SF_D = 0 * K = 0
- AV_D = SF_D * Price_D = 0 * 17 = 0
- CVAT = 18,144 + 0 = 18,144
- Balance = 501,984 - 18,144 = 483,840 (positive)

[Several steps omitted for brevity]

Step 30: SFI_C = 15, SFI_D = 15
- SF_C = 15 * K = 12,960
- AV_C = SF_C * Price_C = 12,960 * 21 = 272,160
- SF_D = 15 * K = 12,960
- AV_D = SF_D * Price_D = 12,960 * 17 = 220,320
- CVAT = 272,160 + 220,320 = 492,480
- Balance = 501,984 - 492,480 = 9,504 (positive)

Step 31: SFI_C = 16, SFI_D = 15
- SF_C = 16 * K = 13,824
- AV_C = SF_C * Price_C = 13,824 * 21 = 290,304
- SF_D = 15 * K = 12,960
- AV_D = SF_D * Price_D = 12,960 * 17 = 220,320
- CVAT = 290,304 + 220,320 = 510,624
- Balance = 501,984 - 510,624 = -8,640 (negative)

We stop at Step 30 because Step 31 results in a negative balance.

#### 2.2 Direct calculation Method

We can optimize the process and avoid iteration by directly calculating the Scaling Factor Indices (SFI) for the tokens being added. We'll explore several approaches:

##### 2.2.1 Maximum SFI for Individual Tokens

Calculate the maximum possible SFI for each token if it were to be used exclusively:

```
SFI_i_max = floor(CVRT / (K * Price_i))
```

For Token C:
```
SFI_C_max = floor(501,984 / (864 * 21)) = 27
```

For Token D:
```
SFI_D_max = floor(501,984 / (864 * 17)) = 34
```

This calculation provides an upper limit for each token's SFI. It's useful for understanding the potential range of values and can be used when only one token is being added. However, it doesn't account for balancing multiple tokens simultaneously.

These maximum values can also serve as upper bounds when searching through iterations, potentially reducing the search space.

##### 2.2.2 Balanced SFI for Multiple Tokens

To find balanced SFIs for multiple tokens, we aim to maximize the total value added without exceeding CVRT.

For two tokens, the general equation is:

```
SFI_C * K * Price_C + SFI_D * K * Price_D ≤ CVRT
```

This can be generalized for n tokens as:

```
∑(SFI_i * K * Price_i) ≤ CVRT, for i = 1 to n
```

###### 2.2.2.1 Method 1: Equal SFIs for All Tokens

To find equal SFIs for all tokens, we can use the following approach:

1. Set SFI for all tokens to a common variable x. 
2. Set SFI_C = SFI_D = x and solve the inequality:

```
x * K * Price_C + x * K * Price_D ≤ CVRT
x * K * (Price_C + Price_D) ≤ CVRT
x ≤ CVRT / (K * (Price_C + Price_D))
```

3. Take the floor of the result to get the integer SFI:

```
SFI_common = floor(CVRT / (K * (Price_C + Price_D)))
```

For our example:
```
SFI_common = floor(501,984 / (864 * (21 + 17)))
           = floor(15.29)
           = 15
```

Therefore, SFI_C = SFI_D = 15

This method can be extended to more than two tokens by summing all prices in the denominator.

###### 2.2.2.2 Method 2: SFIs Based on Price Ratios

To find SFIs that maintain the same overall value for each token based on their price proportions:

1. Calculate price ratios:
```
Ratio_C = Price_D / (Price_C + Price_D)
Ratio_D = Price_C / (Price_C + Price_D)
```

2. Set up equations:
```
SFI_C = x * Ratio_C
SFI_D = x * Ratio_D
```

3. Solve for x:
```
x ≤ CVRT / (K * (Ratio_C * Price_C + Ratio_D * Price_D))
```

4. Calculate SFIs:
```
x = floor(CVRT / (K * (Ratio_C * Price_C + Ratio_D * Price_D)))
SFI_C = floor(x * Ratio_C)
SFI_D = floor(x * Ratio_D)
```

For our example:
```
Ratio_C = 17 / (21 + 17) = 0.4474
Ratio_D = 21 / (21 + 17) = 0.5526

x = floor(501,984 / (864 * (0.4474 * 21 + 0.5526 * 17)))
  = floor(30.8458)
  = 30

SFI_C = floor(30 * 0.4474) = 13
SFI_D = floor(30 * 0.5526) = 16
```

This method provides SFIs that better reflect the price differences between tokens while maintaining overall value balance.

Note1: For more than two tokens, the principle remains the same, but the calculations become more complex. Users may need to implement custom algorithms for scenarios involving many tokens.

Note2: This method aproximates SFI_C and SFI_D to produce AV_C and AV_D (total value of each token) that respect the ratio of prices, but is not maximixed to cover all the balance: SFI_C or SFI_D can still be increased slightly to reduce the most the difference with CVRT.

### 3 Add Quantity per IU for Token i (AQ_IU_i)

With SFI_C = 15 and SFI_D = 15

```
SF_C = 15 * K = 12,960
AQ_IU_C = (SF_C * 100) / TIU
AQ_IU_C = (12,960 * 100) / 3,456 = 375
```

```
SF_D = 15 * K = 12,960
AQ_IU_D = (SF_D * 100) / TIU
AQ_IU_D = (12,960 * 100) / 3,456 = 375
```

### 4 Add Quantity Total (AQ_T_i)

```
AQ_T_C = SF_C = 12,960
AQ_T_D = SF_D = 12,960
```

### 5 Calculate Aggregate Value of Added Token (AV_i)

```
AV_C = SF_C * Price_C = 12,960 * 21 = 272,160
AV_D = SF_D * Price_D = 12,960 * 17 = 220,320
```

### 6 Calculate Cumulative Value of Added Tokens (CVRT)

```
CVAT = Sum(AV_i) for all added tokens
CVRT = AV_D + AV_C = 272,160 + 220,320 = 492,480
```

## Calculate Balance

There might be remaining balance. 

For instance:
- CVRT = 501,984
- CVAT = 492,480
- Balance = CVRT - CVAT = 501,984 - 492,480 = 9,504

This balance will be addressed using ADA in the next step.

## Token Addition Process

### 1 Adjust Scaling Factor Indice for ADA (SFI_ADA)

- The price of ADA is 1
- The amount of ADA to add in each IU (AQ_IU_ADA) multiplied by the total IU must be a multiple of 100.
- The remaining balance to cover (9,504 in this example) must be to be divisible by the by K.
  
We'll show both the iterative steps and the direct calculation:

#### 1.1 Iterative Method

Initialize ADA Scaling Factor Index (SFI_ADA) at 0 and increment until the total balance reaches zero.

Step 1: SFI_ADA = 0
- SF_ADA = 0 * K = 0
- AV_ADA = SF_ADA = 0
- Remaining balance = 9,504 - 0 = 9,504

Step 2: SFI_ADA = 1
- SF_ADA = 1 * K = 864
- AV_ADA = SF_ADA = 864
- Remaining balance = 9,504 - 864 = 8,640

[Several steps omitted for brevity]

Step 11: SFI_ADA = 11
- SF_ADA = 11 * K = 9,504
- AV_ADA = SF_ADA = 9,504 = 9,504
- Remaining balance = 9,504 - 9,504 = 0

We stop at Step 11 because the balance has reached zero.

Final quantities with SFI_ADA = 11
- SF_ADA = 11 * K = 9,504
- AQ_IU_ADA = (SF_ADA * 100) / TIU = (9,504 * 100) / 3,456 = 275
- AQ_T_ADA = (AQ_IU_i * TIU) / 100 = (275 * 3,456) / 100 = 9504

#### 1.2 Simplified Calculation

We can directly calculate SFI_ADA:

```
SFI_ADA = Balance / K
SFI_ADA = 9,504 / 864 = 11
```

### 1.3 Add Quantity per IU for ADA (AQ_IU_ADA)

With SFI_ADA = 11

```
SF_ADA = 11 * K = 9,504
AQ_IU_ADA = (SF_C * 100) / TIU
AQ_IU_ADA = (9,504 * 100) / 3,456 = 275
```

### 1.4 Add Quantity Total (AQ_T_ADA)

```
AQ_T_ADA = SF_ADA = 9,504
```

### 1.5 Calculate Aggregate Value of Added ADA (AV_ADA)

```
AV_ADA = SF_ADA = 9,504
```

## Calculate Final Balance

For instance:
- CVRT = 501,984
- CVAT = 492,480
- AV_ADA = 9,504
- Final Balance = CVRT - (CVAT + AV_ADA) = 501,984 - (492,480 + 9,504) = 0 

Resulting final balance must be zero.

## Conclusion

This methodology uses the adjustment of Scaling Factor Indices for each removed and added token, ensuring that the resulting values maintain appropriate granularity. The step-by-step process demonstrates how the balance is carefully managed throughout the re-indexing operation.

Key points:
1. All calculations use integer arithmetic, leveraging the x100 multiplication in the investment unit definition.
2. The Fundamental Scaling Factor (K) ensures divisibility constraints are met.
3. The process of adding tokens involves incremental adjustments until just before a negative balance would occur.
4. ADA is used as a final adjustment to achieve perfect balance.

Fund managers can use this methodology to perform accurate and balanced re-indexing operations, ensuring that the fund's composition can be updated while maintaining the integrity of the investment units.