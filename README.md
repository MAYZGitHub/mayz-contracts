# MAYZ Protocol: Smart Contracts

## Table of Contents
- [MAYZ Protocol: Smart Contracts](#mayz-protocol-smart-contracts)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Key Features](#key-features)
  - [Architecture Overview](#architecture-overview)
  - [Smart Contracts](#smart-contracts)
  - [Key Concepts](#key-concepts)
  - [Repository Structure](#repository-structure)
  - [Development Environment](#development-environment)
  - [Testing](#testing)
  - [Deployment](#deployment)
  - [Security Considerations](#security-considerations)
  - [Token Economics](#token-economics)
  - [Oracle Integration](#oracle-integration)
  - [Development Roadmap](#development-roadmap)
  - [Community and Support](#community-and-support)
  - [Contributing](#contributing)
  - [Additional Documentation](#additional-documentation)

## Introduction
Welcome to the MAYZ Protocol Smart Contracts Repository. This repository contains the core smart contracts that power the MAYZ Protocol, a decentralized finance (DeFi) ecosystem built on the Cardano blockchain. Our contracts are written in Plutus, Cardano's native smart contract language, aiming to democratize finance by providing a permissionless platform for creating and managing investment funds, facilitating liquidity, and empowering users through innovative mechanisms.

For a detailed overview of the protocol's smart contracts, datums, redeemers, and their interactions, please refer to the Smart Contracts Documentation.

## Key Features
- **Decentralized Investment Funds**: Create, manage, and participate in investment funds composed of Cardano native tokens.
- **Dynamic Fund Tokens (FTs)**: Represent fund shares with real-time valuation based on underlying assets.
- **Swap Offers**: Exchange ADA for Fund Tokens and vice versa through a decentralized marketplace.
- **Oracle-Powered Pricing**: Accurate and up-to-date asset valuations through a robust oracle system.
- **Flexible Re-indexing**: Adapt fund compositions to market conditions while maintaining transparency.
- **Multi-UTXO Fund Holdings**: Optimize concurrency and scalability through innovative fund structure design.
- **Delegation System**: Allow MAYZ token holders to delegate their tokens to funds and earn commissions.

## Architecture Overview
The MAYZ Protocol is built on a modular architecture, leveraging Cardano's extended UTXO model. Our system comprises interconnected components:
- **Fund Factory**: Facilitates decentralized investment fund creation and management.
- **Swap Offer Engine**: Enables liquidity provision and token exchanges.
- **Oracle Service**: Provides real-time price data for asset valuation.
- **Re-indexing Mechanism**: Allows dynamic fund composition adjustments.
- **Multi-UTXO Management**: Optimizes fund operations and user concurrency.
- **Delegation System**: Manages MAYZ token delegations and commission distributions.

## Smart Contracts
Our protocol utilizes advanced smart contracts for secure and efficient operations:
- **Protocol Contract**: Manages global protocol parameters and upgrades.
- **Fund Contract**: Manages the creation, operation, and liquidation of funds.
- **Fund Holding Contract**: Handles individual fund holdings and optimizes concurrency.
- **Invest Unit Contract**: Manages representation and valuation of fund compositions.
- **SwapOffer Contract**: Facilitates ADA and Fund Token exchanges.
- **Delegation Contract**: Manages MAYZ token delegations to funds.
- **Script Contract**: Oversees deployed scripts on-chain.

For detailed information on each contract, refer to the [Smart Contracts Documentation](./docs/SMART-CONTRACTS.md).

## Key Concepts
- **Investment Unit (IU)**: Defines token composition of each fund.
- **Fund Tokens (FTs)**: Represent shares in a fund, minted on deposit and burned on withdrawal.
- **Multi-UTXO Fund Holdings**: Improve concurrency and scalability with multiple UTXOs.
- **Re-indexing Process**: Adjusts fund composition while maintaining total fund value.
- **Commission System**: Paid upfront for the remaining fund lifetime, with partial refunds upon early withdrawal.
- **Delegation**: MAYZ token holders can delegate tokens to funds to earn a share of commissions.

For in-depth explanations, see our [Key Concepts Guide](./docs/KEY-CONCEPTS.md)..

## Repository Structure
```
mayz-protocol-contracts/
├── LICENSE
├── README.md
├── app/
│   ├── cli.hs
│   └── deploy.hs
├── docs/
│   ├── REINDEXING-BASICS.md
│   └── REINDEXING-DEEP.md
├── export/
│   ├── test/
│       ├── various Plutus scripts (.plutus)
│       └── deploy.json
├── src/
│   ├── Generic/
│   │   ├── Various Helpers and Types (e.g., CLIHelpers, DeployHelpers, etc.)
│   └── Protocol/
│       ├── Contracts (BuyOrder, Delegation, Fund, etc.)
│       └── Helpers and Types
├── tests/
│   ├── TestUtils/
│   │   ├── ContextGenerators, Helpers, TxGenerators, etc.
│   ├── Contracts (InitialData, TxContext, TxSpecs)
│   └── TestTree (PropertyTests, UnitTests, etc.)
├── cabal.project
├── mayz-protocol.cabal
├── fourmolu.yaml
├── hie.yaml
└── misc config, workspace, and export files
```

## Development Environment
- **Haskell version**: [specify version]
- **The Glorious Glasgow Haskell Compilation System**: version 8.10.7
- **Cabal library**: compiled using version 3.6.2.0

To set up the development environment:
1. Clone this repository
2. Run `cabal build` to compile the contracts

## Testing
We employ a comprehensive testing strategy:
- Unit tests for individual contract functions
- Property-based tests for edge cases
- Performance tests for resource optimization

TODO: hablar de automatic test, agregar comandos para ejecutar todos y cada uno de los test

Run tests with:
```
cabal test
```

For more information, refer to the [Testing Documentation](./docs/TESTING.mdd).

## Deployment
For detailed deployment instructions, refer to the [Deployment Guide](./docs/DEPLOYMENT.md).

## Security Considerations
- **Rounding errors**: Commission calculations use high-precision arithmetic.
- **Oracle dependency**: Price manipulations are mitigated through multi-source oracles.
- **Multi-UTXO design**: Enhances concurrency and resists certain types of attacks.

We are committed to ongoing security audits. For more information, see our [Security Policy](./docs/SECURITY.md).

## Token Economics
The $MAYZ token serves multiple crucial functions:
- **Fund Creation**: Lock $MAYZ to create and manage funds.
- **Liquidity Provision**: Stake $MAYZ to create Swap Offers and earn commissions.
- **Delegation**: Delegate $MAYZ to funds to earn a share of commissions.
- **Incentivization**: Reward active participants and align community interests.

For more information, see the [Token Economics Documentation](./docs/TOKENOMICS.md).

## Oracle Integration
Our oracle system:
- Aggregates price data from multiple sources
- Implements error detection and correction mechanisms
- Provides real-time updates for fair valuations and swaps
- Integrates with smart contracts for on-chain price verification

For more information, refer to the [Oracle Documentation](./docs/ORACLE.md).

## Development Roadmap
[TODO: Include key points from the development roadmap section]

For our full development roadmap, visit the [Project Roadmap](./docs/ROADMAP.md).

## Community and Support
[TODO: Include all relevant links. Add link to dApp]

Join our vibrant community and stay updated:
- [Website](https://mayz.io)
- [Twitter](https://twitter.com/MAYZprotocol)
- [Discord](https://discord.gg/mayzprotocol)
- [Medium](https://medium.com/@MAYZprotocol)

For technical support, open a ticket in our GitHub repository or reach out on [Discord](https://discord.gg/mayzprotocol).

## Contributing
We welcome contributions. Please follow these steps:
1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

Ensure your code adheres to our style guide and passes all tests. See our [Contributing Guidelines](./docs/CONTRIBUTING.md) for more information.

## Additional Documentation
- **Re-indexing Basics**: Overview of the fund re-indexing process. [Link](./docs/REINDEXING-BASICS.md).
- **Re-indexing Deep Dive**: Detailed explanation of re-indexing methodology. [Link](./docs/REINDEXING-DEEP.md).
- **Smart Contracts Documentation**: Comprehensive guide to all contracts, datums, and redeemers. [Link](./docs/SMART-CONTRACTS.md).
- **Testing Documentation**: Detailed information on our testing approach. [Link](./docs/TESTING.md).
- **Key Concepts Guide**: In-depth explanations of core protocol concepts. [Link](./docs/KEY-CONCEPTS.md).
- **Deployment Guide**: Instructions for deploying MAYZ Protocol. [Link](./docs/DEPLOYMENT.md).
- **Token Economics Documentation**: Detailed breakdown of the MAYZ token ecosystem. [Link](./docs/TOKENOMICS.md).
- **Oracle Documentation**: Information on our oracle implementation. [Link](./docs/ORACLE.md).
- **Project Roadmap**: Future development plans for MAYZ Protocol. [Link](./docs/ROADMAP.md).
- **Contributing Guidelines**: See our Contributing Guidelines. [Link](./docs/CONTRIBUTING.md).

For more documentation, refer to the docs/ directory. If you have any questions, feel free to open an issue or reach out on [Discord](https://discord.gg/mayzprotocol).
