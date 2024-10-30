# MAYZ Protocol Smart Contract Architecture

## Table of Contents
- [MAYZ Protocol Smart Contract Architecture](#mayz-protocol-smart-contract-architecture)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Component Hierarchy](#component-hierarchy)
  - [Understanding UTXOs and Their Identification](#understanding-utxos-and-their-identification)
    - [The UTXO Model vs Account Model](#the-utxo-model-vs-account-model)
    - [Why UTXO Identification Matters](#why-utxo-identification-matters)
    - [Solution: Identification Tokens](#solution-identification-tokens)
  - [Concurrency Through Multiple Holdings](#concurrency-through-multiple-holdings)
  - [Multi-Script Operations](#multi-script-operations)
  - [Transaction Types \& Scripts Involved](#transaction-types--scripts-involved)
    - [Protocol Management](#protocol-management)
    - [Fund Operations](#fund-operations)
    - [User Operations](#user-operations)
    - [Fund Management](#fund-management)
  - [Key Design Principles](#key-design-principles)

## Overview
The MAYZ Protocol manages decentralized investment funds on Cardano using a UTXO-based architecture. This document explains the complete system structure and operation.

## Component Hierarchy

```
Protocol Level
├── Protocol UTXO (Protocol ID Token)
│   └── Protocol Datum (global settings)
│
Fund Level
├── Fund UTXO (Fund ID Token)
│   └── Fund Datum (fund settings)
│
├── Invest Unit UTXO (Invest Unit ID Token)
│   └── Invest Unit Datum (token composition)
│
└── Fund Holdings UTXOs (Fund Holding ID Tokens)
    ├── Holding UTXO 1 
    ├── Holding UTXO 2
    └── Holding UTXO N
```

## Understanding UTXOs and Their Identification

### The UTXO Model vs Account Model
Traditional Account Model:
- Fixed address holds state
- Balance updates in place
- Single state per contract

Cardano UTXO Model:
- UTXOs are like boxes containing tokens and data
- Create new/destroy old in each tx
- Anyone can create UTXOs with any data
- Need way to identify official UTXOs

### Why UTXO Identification Matters
Key challenges with UTXOs:
- Anyone can create a transaction that creates new UTXOs
- Any UTXO can hold any datum (data)
- Only validators can restrict how UTXOs are spent
- But validators can't prevent UTXO creation

Example Problem:
```
Real Protocol UTXO:
{
    datum: {commissionRate: 5%}
}

Attacker's UTXO:
{
    datum: {commissionRate: 1%}  // Trying to pay less commission
}
```

This creates several issues:
- No way to distinguish real from fake UTXOs
- Could process wrong parameters
- No inherent authenticity

### Solution: Identification Tokens
We add special tokens that can only be minted under strict rules:

```
Real Protocol UTXO:
{
    datum: {commissionRate: 5%},
    tokens: ["PROTOCOL-ID-123"]  // Can only be minted by protocol
}

Attacker's UTXO:
{
    datum: {commissionRate: 1%},
    tokens: []  // Cannot add protocol ID token!
}
```

How it works:
1. Special tokens mark official UTXOs
2. Minting policies control token creation
3. Validators only accept UTXOs with correct tokens

Real World Analogy:
- Anyone can print a document
- Only governments can add official seals
- Banks only accept documents with valid seals
- Our validators only accept UTXOs with valid ID tokens

## Concurrency Through Multiple Holdings

Problem:
- UTXO gets locked during transaction
- With single UTXO, only one user operation at a time
- Creates bottleneck

Solution:
- Multiple Fund Holding UTXOs
- Users can pick any available holding
- Parallel operations possible

Example:
- Alice deposits using Holding 1
- Bob withdraws using Holding 2
- Charlie deposits using Holding 3
- All happen simultaneously

## Multi-Script Operations

Many operations require multiple scripts working together:

1. Deposit Operation:
   - Fund Holding Validator: Checks token deposit
   - Fund Policy: Mints FT tokens
   - Both verify operation validity

2. Re-indexing Operation:
   - Invest Unit Validator: Validates new composition
   - Fund Holding Validator: Handles token exchange
   - Both ensure value preservation

## Transaction Types & Scripts Involved

### Protocol Management
- Create Protocol
  * Scripts: Protocol Policy
  * Purpose: One-time setup

- Update Protocol Settings  
  * Scripts: Protocol Validator
  * Purpose: Change global parameters

### Fund Operations
- Create Fund
  * Scripts: Fund Policy, Protocol Validator
  * Purpose: Initialize new fund

- Update Fund Settings
  * Scripts: Fund Validator
  * Purpose: Modify fund parameters

- Add/Remove Holdings
  * Scripts: Fund Validator, Fund Holding Policy
  * Purpose: Manage concurrent capacity

### User Operations
- Deposit
  * Scripts: Fund Holding Validator, Fund Policy
  * Purpose: Add tokens, mint FTs

- Withdraw
  * Scripts: Fund Holding Validator, Fund Policy
  * Purpose: Remove tokens, burn FTs

### Fund Management
- Re-index Fund
  * Scripts: Invest Unit Validator, Fund Holding Validator
  * Purpose: Change fund composition

- Balance Holdings
  * Scripts: Fund Holding Validator
  * Purpose: Redistribute tokens

## Key Design Principles

1. State Management
   - UTXOs are immutable
   - Create new instead of updating
   - Validators ensure valid transitions

2. Concurrency
   - Multiple holdings enable parallel ops
   - Users choose available UTXOs
   - No central bottleneck

3. Security
   - Identification tokens prevent fraud
   - Multiple validators check operations
   - Strict minting policies

4. Flexibility
   - Modular script design
   - Clean separation of concerns
   - Extensible architecture

This architecture enables secure, concurrent fund management while maintaining protocol integrity through specialized validators and identification tokens.