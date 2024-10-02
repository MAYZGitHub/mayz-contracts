# Tests for the MAYZ Protocol

## Folder structure

```shell
.
├── Tests
│   ├── Performance
│   ├── PropertyBasedTests
│   │   ├── Contracts
│   │   │   ├── Fund
│   │   │   └── Protocol
│   │   └── Helpers
│   └── UnitTests
│       ├── Contracts
│       │   ├── Fund
│       │   ├── InvestUnit
│       │   ├── Protocol
│       │   └── SwapOffer
│       └── Helpers
├── TestUtils.hs
└── ValidatorTester.hs
```

## ValidatorTester

`ValidatorTester.hs` is designed for testing validators and minting policies within the MAYZ protocol. It provides functions for testing these components against a variety of script contexts, allowing for thorough validation under different scenarios.

## TestUtils

`TestUtils.hs` contains mock data and utility functions for testing the MAYZ protocol's contracts and helper functions. It includes ScriptContexts, Datums, Redeemers, UTxOs, Validators, and Minting Policies, which can be adjusted to test different transaction scenarios and validate script contexts.

## Running the Tests

You can run all the tests at once with the `cabal test` command or a single suite with `cabal test nameOfTheSuite`. You can optionally add the flag `--test-show-details=always` to get more detailed results. Currently, the available suites are `UnitTests`, `PropertyBasedTests`, and `Performance`.

## UnitTests

The UnitTests directory contains tests for both Helpers and Contracts, which include Validators and Minting Policies. These tests are focused on verifying specific and individual cases, assessing whether transactions should successfully pass or fail the on-chain checks.

Currently, the result of these tests is the following:

```
  Contracts Tests
    Protocol Tests
      Protocol Validator Tests
        Datum not changed must succeed:                                                                        OK (12.59s)
        Updating modifiable fields must succeed:                                                               OK
        Updating pdScriptPolicyID_CS must fail:                                                                OK
        Updating pdScriptValidator_Hash must fail:                                                             OK
        Updating minAda must fail:                                                                             OK (0.01s)
        Changing value must fail:                                                                              OK
        Not signed by admins must fail:                                                                        OK
        Too big range must fail:                                                                               OK
        No protocol output must fail:                                                                          OK
        More than one protocol output must fail:                                                               OK (0.01s)
        Double satisfaction must fail:                                                                         OK
        Double satisfaction would succeed if both inputs had the same NFT:                                     OK (0.01s)
      Testing Protocol Minting Policy
        Successful case:                                                                                       OK (0.51s)
        Not consuming UTxO referenced in MP parameter must fail:                                               OK
        Minting a different amount should fail:                                                                OK
    Fund Tests
      Testing Fund Validator
        Update Datum tests
          Successful case:                                                                                     OK (28.00s)
          Emptying the list of admins should succeed:                                                          OK
          Changing FundPolicyID should fail:                                                                   OK (0.01s)
          Changing Fund UTxO value should fail:                                                                OK
        Burn the FundID of a Fund UTxO
          Successful case:                                                                                     OK
          Not burning FundID should fail:                                                                      OK
          Minting another FundID should fail:                                                                  OK
        Add Fund Holding tests
          Successful case:                                                                                     OK
          Not minting Fund Holding ID NFT should fail:                                                         OK
          Changing Fund UTxO value should fail:                                                                OK
          Not signed by admin should fail:                                                                     OK
          Not incrementing count should fail:                                                                  OK (0.01s)
          Not incrementing index should fail:                                                                  OK
        Burn the FundHoldingID of a Fund Holding UTxO
          Successful case:                                                                                     OK
          Not burning Fund Holding ID should fail:                                                                   OK
          Minting another Fund Holding ID should fail:                                                               OK
          Not decreasing holdingsCount should fail:                                                            OK (0.01s)
          Increasing holdingsCount should fail:                                                                OK
          Changing Fund UTxO value should fail:                                                                OK
      Testing Fund Minting Policy
        Tests for Fund ID minting
          Minting ID correctly must succeed:                                                                          OK (0.01s)
          Not including Protocol input ref should fail:                                                        OK (0.01s)
          Not including fund output should fail:                                                               OK
          Not including invest unit output should fail:                                                        OK
          Not including utxo in policy parameter as input should fail:                                         OK
          Not minting Fund ID should fail:                                                                 OK (0.01s)
          Not minting Invest Unit ID NFT should fail:                                                          OK
          Fund utxo address different from the one in policy param should fail:                                OK
          Having initial holdings count different from 0 should fail:                                          OK
          MAYZ in fund output not according to FundCategory of datum should fail:                                 OK (0.01s)
          IU output without minAda specified in datum should fail:                                             OK
        Tests for Fund ID burning
          Burning ID correctly must succeed:                                                                          OK
          Not burning fund ID NFT should fail:                                                                 OK (0.01s)
          Not burning invest unit ID NFT should fail:                                                          OK
        Tests for fund FT minting
          Minting FT correctly must succeed:                                                                          OK (0.01s)
          Not minting FT should fail:                                                                          OK
          Not having Fund UTxO as ref input should fail:                                                       OK
          Not having Fund Holding UTxO as input should fail:                                                   OK
          Incorrect redeemer for Fund Holding UTxO should fail:                                                OK
        Tests for fund FT burning
          Burning FT correctly must succeed:                                                                          OK (0.01s)
          Not burning FT should fail:                                                                          OK
          Not having Fund UTxO as ref input should fail:                                                       OK (0.01s)
          Not having Fund Holding UTxO as input should fail:                                                   OK
          Incorrect redeemer for Fund Holding UTxO should fail:                                                OK
    Fund Holding Tests
      Testing Fund Holding Validator
        Deposit in the fund tests
          Depositing correctly must succeed:                                                                          OK (0.04s)
          Depositing without minting FT should fail:                                                           OK (0.02s)
          Depositing without increasing FT minted subtotal should fail:                                        OK (0.04s)
          Depositing without paying invest units should fail:                                                  OK (0.04s)
          Depositing outside valid range should fail:                                                          OK (0.04s)
        Withdraw from the fund tests
          Withdrawing correctly must succeed:                                                                         OK (0.04s)
          Withdrawing without burning FT should fail:                                                          OK (0.04s)
          Withdrawing without updating FT minted subtotal should fail:                                         OK (0.02s)
          Withdrawing without user recovering commissions should fail:                                         OK (0.04s)
        Collect commissions by Protocol admin tests
          Collecting commissions correctly must succeed:                                                              OK (0.01s)
          Collecting commissions without updating remaining commissions should fail:                           OK
          Collecting commissions without updating Fund Holding value should fail:                              OK
          Trying to collect more commissions than the available should fail:                                   OK (0.01s)
          Trying to send collected commissions to a random user without adding an admin signatory should fail: FAIL (0.01s)
            tests/Tests/UnitTests/Contracts/Fund/Holding/Validator.hs:297:
            expected: ["not isSignedByAny admins"]
             but got: []
          Collecting commissions outside valid range should fail:                                              OK (0.01s)
        Collect commissions by Managers tests
          Collecting commissions correctly must succeed:                                                              OK (0.01s)
          Collecting commissions without updating remaining commissions should fail:                           OK (0.01s)
          Collecting commissions without updating Fund Holding value should fail:                              OK (0.01s)
          Trying to collect more commissions than the available should fail:                                   OK (0.01s)
          Trying to send collected commissions to a random user without adding an admin signatory should fail: FAIL (0.01s)
            tests/Tests/UnitTests/Contracts/Fund/Holding/Validator.hs:417:
            expected: ["not isSignedByAny admins"]
             but got: []
          Collecting commissions outside valid range should fail:                                              OK (0.01s)
        Collect commissions by Delegators tests
          Collecting commissions correctly must succeed:                                                              OK (0.01s)
          Collecting commissions without updating remaining commissions should fail:                           OK
          Collecting commissions without updating Fund Holding value should fail:                              OK (0.01s)
          Trying to collect more commissions than the available should fail:                                   OK (0.01s)
          Collecting commissions outside valid range should fail:                                              OK (0.01s)
        Re-index fund tests
          Re-index correctly must succeed:                                                                            OK (0.01s)
          Not including protocol input ref should fail:                                                        OK
          Not including some Fund Holding as ref should fail:                                                       OK (0.01s)
          Not including invest unit input should fail:                                                         OK (0.01s)
          Not having a Fund Holding output should fail:                                                        OK (0.01s)
          Updating Fund Holding should fail:                                                             OK
          Not updating the Fund Holding value should fail:                                                          OK (0.01s)
          Having an incorrect redeemer for the Invest Unit validator should fail:                              OK
        Delete Fund Holding tests
          Delete Fund Holding correctly must succeed:                                                                 OK (0.01s)
          Not including fund admin sign should fail:                                                           OK (0.01s)
          Not including Fund UTXO as input should fail:                                                        OK (0.01s)
          Not burning Fund Holding ID should fail:                                                             OK (0.01s)
          Too big range should fail:                                                                           OK (0.01s)
      Tests for FundHolding ID NFT minting
        Successful case:                                                                                       FAIL (0.01s)
          tests/Tests/UnitTests/Contracts/Fund/Holding/MintingPolicy.hs:44:
          expected: []
           but got: ["isThisPolicyDifferent"]
        Not including Fund UTxO input should fail:                                                             OK (0.01s)
        Not including Fund Holding UTxO output should fail:                                                     OK
        Not including AddFundHolding Redeemer should fail:                                                     FAIL (0.01s)
          tests/Tests/UnitTests/Contracts/Fund/Holding/MintingPolicy.hs:68:
          expected: ["not isCorrect_Redeemer_Fund"]
           but got: ["isThisPolicyDifferent","not isCorrect_Redeemer_Fund"]
        Including a wrong Fund Holding Datum should fail:                                                  FAIL
          tests/Tests/UnitTests/Contracts/Fund/Holding/MintingPolicy.hs:104:
          expected: ["not isCorrect_Output_FundHolding_Datum"]
           but got: ["isThisPolicyDifferent","not isCorrect_Output_FundHolding_Datum"]
        Including a wrong Fund Holding Value should fail:                                                  FAIL (0.01s)
          tests/Tests/UnitTests/Contracts/Fund/Holding/MintingPolicy.hs:127:
          expected: ["not isCorrect_Output_FundHolding_Value"]
           but got: ["isThisPolicyDifferent","not isCorrect_Output_FundHolding_Value"]
        Including a wrong Fund Holding UTxO Address should fail:                                                FAIL (0.01s)
          tests/Tests/UnitTests/Contracts/Fund/Holding/MintingPolicy.hs:150:
          expected: ["not isCorrect_Outputs_Addresses"]
           but got: ["isThisPolicyDifferent","not isCorrect_Outputs_Addresses"]
    InvestUnit Tests
      Testing InvestUnit Validator
        ReIndexing the InvestUnit
          Successful case:                                                                                     OK (0.01s)
          Validation interval not having LowerBound should fail:                                               OK
          Not including Protocol UTxO as InputRef should fail:                                                 OK
          InvestUnit tokens not having a price should fail:                                                    OK (0.01s)
          New InvestUnit tokens not having having corresponding price in ADA should fail:                      OK (0.01s)
          Including wrong InvestUnit in InvestUnit UTxO's Datum should fail:                                   OK
          Changing the value of the output InvestUnit UTxO should fail:                                        OK (0.01s)
          Using an incorrect Oracle signature should fail:                                                     OK (0.01s)
          Using an incorrect Oracle time range should fail:                                                    OK (0.01s)
          Not including FundHolding ReIndexing Redeemer should fail:                                           OK (0.01s)
          Not including Fund UTxO as InputRef should fail:                                                     OK
          Not including Fund Holding UTxO as Input should fail:                                                OK (0.01s)
    SwapOffer Tests
      Testing SwapOffer Minting Policy
        Tests for SwapOffer ID NFT minting
          Minting ID correctly must succeed:                                                                          OK (9.37s)
          Not having a Protocol input ref should fail:                                                         FAIL
            tests/Tests/UnitTests/Contracts/SwapOffer/MintingPolicy.hs:60:
            expected: ["Expected exactly one Protocol input ref"]
             but got: []
          Not having a SwapOffer output should fail:                                                           OK
          Minting an asset with wrong token name should fail:                                                  OK
          Paying to a wrong address should fail:                                                               OK
          SwapOffer with initial total_FT_Earned different from 0 should fail:                                 OK
          Not paying Ada available amount specified in datum should fail:                                      OK
        Tests for SwapOffer ID NFT burning
          Burning ID correctly must succeed:                                                                          OK
          Burning an asset with wrong token name should fail:                                                  OK
  Helpers Tests
    Value Tests
      Plutus Ledger Value Tests
        flattenValue to list of tuples:                                                                        OK
    Comission Tests
      Commissions tests
        setAndLoosePrecisionGetOnlyNum:                                                                        OK
        setAndLoosePrecision1e6GetOnlyNumerator:                                                               OK
        powInteger:                                                                                            OK
        powRational:                                                                                           OK
        calculateDepositCommissionsUsingMonths
          Integral remaining months:                                                                           OK
          Fractional remaining months:                                                                         OK
          Less than one remaining month:                                                                       OK
        calculateWithdrawCommissionsUsingMonths
          Integral remaining months:                                                                           OK
          Fractional remaining months:                                                                         OK
          Less than one remaining month:                                                                       OK

8 out of 136 tests failed (52.03s)
```

The two failing cases in the Fund Holding validator tests are due to the lack of signatory checks upon the collecting of commissions.

The failing case in the SwapOffer minting tests is due to a mismatch between the docs and the code: there is no check implemented for the Protocol input ref.

The failing cases in the Fund Holding ID NFT minting are due to the `isPolicyDifferent` check, which is the only `traceIfTrue` condition in the on-chain code.

## PropertyBasedTests

The `PropertyBasedTests` directory features tests that assess the properties of Helpers and Contracts, including Validators and Minting Policies. These tests utilize arbitrarily or randomly generated inputs and scenarios to ensure that the code functions correctly under a diverse array of conditions, providing a robust evaluation of its behavior and reliability.

Currently, the result of these tests is the following:

```
Property-Based Tests
  Contracts Tests
    Protocol Validator Tests
      Changing the Protocol UTxO value must fail:                                                              OK (3.12s)
        +++ OK, passed 200 tests.
    Protocol Minting Policy Tests
      Minting an invalid amount of ProtocolID NFT must fail:                                                   OK (2.72s)
        +++ OK, passed 200 tests.
      Spending a different UTxO must fail:                                                                     OK (2.88s)
        +++ OK, passed 200 tests.
    Fund Validator Tests
      Update Datum tests
        Updating admins with arbitrary list should succeed:                                                    OK (3.19s)
          +++ OK, passed 200 tests.
        Modifying Fund UTxO value should fail:                                                                 OK (3.13s)
          +++ OK, passed 200 tests.
        Modifying datum fields other than admins should fail:                                                  OK (3.17s)
          +++ OK, passed 200 tests.
      Add Fund Holding Tests
        Adding a Fund Holding with random creator to random input datum should succeed:                             OK (3.34s)
          +++ OK, passed 200 tests.
        Modifying Fund UTxO value should fail:                                                                 OK (3.71s)
          +++ OK, passed 200 tests.
      Delete Fund Holding Tests
        Deleting a Fund Holding with random creator from random input datum should succeed:                         OK (3.67s)
          +++ OK, passed 200 tests.
        Modifying Fund UTxO value should fail:                                                                 OK (3.61s)
          +++ OK, passed 200 tests.
    Fund Minting Policy Tests
      Tests for FundID NFT minting
        Minting an invalid amount of FundID NFT must fail:                                                     OK (3.28s)
          +++ OK, passed 200 tests.
        Minting an invalid amount of InvestUnitID NFT must fail:                                               OK (3.36s)
          +++ OK, passed 200 tests.
        Different Fund UTxO Address from the one in Policy Param must fail:                                    OK (3.37s)
          +++ OK, passed 200 tests.
        Having the initial holdings count different from 0 must fail:                                          OK (3.48s)
          +++ OK, passed 200 tests.
        Paying trash tokens to the Fund UTxO must fail:                                                        OK (3.78s)
          +++ OK, passed 200 tests.
        Paying incorrect amount of MAYZ Tokens to the Fund UTxO must fail:                                     OK (3.46s)
          +++ OK, passed 200 tests.
        Not paying correct minADA to InvestUnit UTxO must fail:                                               OK (3.95s)
          +++ OK, passed 200 tests.
      Tests for FundID NFT burning
        Burning an invalid amount of FundID NFT must fail:                                                     OK (3.71s)
          +++ OK, passed 200 tests.
        Burning an invalid amount of InvestUnitID NFT must fail:                                               OK (3.40s)
          +++ OK, passed 200 tests.
    FundHolding Validator Tests
      Deposit Tests
        Minted FT amount is as expected:                                                                       OK (10.04s)
          +++ OK, passed 200 tests.
      Withdraw Tests
        Burnt FT amount is as expected:                                                                        OK (10.87s)
          +++ OK, passed 200 tests.
        Value taken from Fund Holding is as expected:                                                          OK (10.18s)
          +++ OK, passed 200 tests.
      Collect Commissions Tests
        Sending Protocol Admins' commissions to a random user without adding an admin signatory should fail:   FAIL (0.02s)
          *** Failed! Falsified (after 1 test):
          Collect_Protocol_Commissions_Params {getCPPFundHoldingDatumType = FundHoldingDatumType {hdFundHolding_Index = 0, hdSubtotal_FT_Minted_Accumulated = 0, hdSubtotal_FT_Minted = 0, hdSubtotal_FT_Circulation = 0, hdSubtotal_FT_Commissions = 0, hdSubtotal_FT_Commissions_Acumulated = 0, hdSubtotal_FT_Commissions_Rate1e6_PerMonth = 0, hdSubtotal_FT_Commissions_Collected_Protocol = 0, hdSubtotal_FT_Commissions_Collected_Delegators = 0, hdSubtotal_FT_Commissions_Collected_Managers = 0, hdMinADA = 0}, getCPPWithdrawAmount = 0}
          Use --quickcheck-replay=681805 to reproduce.
          Use -p '/Sending Protocol Admins'\'' commissions to a random user without adding an admin signatory should fail/' to rerun this test only.
        Sending Fund Admins' commissions to a random user without adding an admin signatory should fail:       FAIL (0.02s)
          *** Failed! Falsified (after 1 test):
          Collect_Managers_Params {getCFAPFundHoldingDatumType = FundHoldingDatumType {hdFundHolding_Index = 0, hdSubtotal_FT_Minted_Accumulated = 0, hdSubtotal_FT_Minted = 0, hdSubtotal_FT_Circulation = 0, hdSubtotal_FT_Commissions = 0, hdSubtotal_FT_Commissions_Acumulated = 0, hdSubtotal_FT_Commissions_Rate1e6_PerMonth = 0, hdSubtotal_FT_Commissions_Collected_Protocol = 0, hdSubtotal_FT_Commissions_Collected_Delegators = 0, hdSubtotal_FT_Commissions_Collected_Managers = 0, hdMinADA = 0}, getCFAPWithdrawAmount = 0}
          Use --quickcheck-replay=599672 to reproduce.
          Use -p '/Sending Fund Admins'\'' commissions to a random user without adding an admin signatory should fail/' to rerun this test only.
        Withdrawing delegators' commissions within available range should succeed:                             OK (3.72s)
          +++ OK, passed 200 tests.
      Re-index Tests
        Varying n Fund Holdings and tokens to add/remove:                                                      OK (16.39s)
          +++ OK, passed 200 tests.
    Fund Holding Minting Policy Tests
      Tests for FundHoldingID NFT minting
        Minting an invalid amount of FundHoldingID NFT must fail:                                              OK (2.82s)
          +++ OK, passed 200 tests.
      Tests for FundHoldingID NFT burning
        Burning an invalid amount of FundHoldingID NFT must fail:                                              OK (2.72s)
          +++ OK, passed 200 tests.
    Invest Unit Validator Tests
      ReIndexing Tests
        ReIndexing varying initial invest unit, tokens to add, tokens to  to remove and prices should succeed: OK (9.43s)
          +++ OK, passed 200 tests.
        ReIndexing changing the invest unit value should fail:                                                 OK (10.28s)
          +++ OK, passed 200 tests.
  Helpers Tests
    Value Tests
      Ledger.Value Tests
        flattenValue preserves total token count:                                                              OK
          +++ OK, passed 200 tests.
        flattenValue matches PlutusTx flattenValue:                                                            FAIL
          *** Failed! Falsified (after 1 test):
          Value (Map [])
          Use --quickcheck-replay=349448 to reproduce.
          Use -p '/flattenValue matches PlutusTx flattenValue/' to rerun this test only.
        isEqValue behaves same as default equality:                                                            OK
          +++ OK, passed 200 tests.
        flattenValueToValue is the inverse of flattenValue:                                                    OK
          +++ OK, passed 200 tests.

3 out of 34 tests failed (140.88s)
```
The tests for commissions collecting by Protocol and Fund admins also fail in this case because of the lack of signatories check.

## Performance Tests

The Performance folder includes tests regarding use of resources.

The current results are the following:

```
All Performance Tests
  Testing resources usage:
    PowRational should use less than 3.5Mb:                                     OK (17.01s)
      +++ OK, passed 500 tests.
    CalculateDepositCommissionsUsingMonths should use less than 6.5Mb:           OK (2.91s)
      +++ OK, passed 500 tests; 510 discarded.
    Deposits should use less than 10Mb:                                         FAIL (0.02s)
      *** Failed! Falsified (after 1 test):
      TestParamsDeposit [(5160483b613d4c3741625b525f4146303f35563f64555e5e3d4932555e47453d3939304d434d5e4d303d3252593f5f33435d434152465743414932595a5b494b,"drzpg",1),(564f3b5436484445403d4145593a5a475e35386356495b5140355c43513a4f4d314c5e4c563b35524a604a573c444a5f3b565f513f3a303c3c4159425c595b33,"pvlvr",1),(3b4a653533585939623832503e5539634d3c563963464c605446375a565d403352553549534246555845433c3d4b354865423d61484c554c3d3b454a5c5e563d,"xblwi",1),(52396344624f5b63615d3e3d4f4a3c474438423c344f4c4a3e644e5543605a5b433e3660375d3631433c5a51603d626660423d554f425d6551463b525c415645,"kenpf",1),(364a413860554232414f4f4256325d555932634c305e4d39484e5f364a4c326231585c5f5a624e424a5545384150534264403948613730513563553f42364730,"hjxxb",1),(52363055643f665e3164554e373f3057334c53574356523a3361653b3345544f4f5d34444934555a4d52334557664443533041625b3b323149394e46663a5031,"mvqns",1),(3831404a4c383c54363e36444f3e51583a474466583444325e3b633f37365d4945484c374d5a4e50564c30515b453c625b645c63565f57464046565b3e5f313f,"ddnrr",1),(5060513c5366516338405e3f404b645a5360493e6555344660374045534d534c5f4e405d333052305a5545323e3f655a324a5a413f4651433e4f325d654c593f,"oruid",1)] 3365867 (POSIXTime {getPOSIXTime = 1754934358533}) (POSIXTime {getPOSIXTime = 1692092082998}) 9701248211
      (ExBudget {exBudgetCPU = ExCPU 3714905596, exBudgetMemory = ExMemory 10534915},[])
    Deposits should be valid:                                                   OK (0.15s)
      +++ OK, passed 500 tests; 498 discarded.
    PowRational Optimized should use less than 1.4Mb:                           OK (51.36s)
      +++ OK, passed 1000 tests.
    CalculateDepositCommissionsUsingMonths Optimized should use less than 2.5Mb: FAIL (47.17s)
      *** Failed! Falsified (after 920 tests):
      TestParamsCalculateDepositCommissionsUsingMonths 2721919 (POSIXTime {getPOSIXTime = 1754276622060}) (POSIXTime {getPOSIXTime = 1693213063732}) 9438310218
      (ExBudget {exBudgetCPU = ExCPU 1201867489, exBudgetMemory = ExMemory 2504696},[])
    Deposits Optimized with 10 max tokens should use less than 6.2Mb:           OK (7.31s)
      +++ OK, passed 10 tests; 11 discarded.
    Deposits Optimized should be valid:                                         OK (0.13s)
      +++ OK, passed 500 tests; 421 discarded.
  Testing validator with some sensible values:
    User 1 locks and user 2 takes with Good Oracle PK - succeeds:               OK (0.05s)
    User 1 locks and user 2 takes with Bad Oracle PK - fails  :                 OK

2 out of 10 tests failed (126.15s)
```
