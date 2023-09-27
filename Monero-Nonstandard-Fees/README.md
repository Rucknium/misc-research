# Monero Nonstandard Fees

Monero has four standard fee levels, but not every Monero wallet implementation creates transactions that pay one of the standard fee levels. Wallets that pay nonstandard fees can reduce their users' privacy. See [Rucknium (2023) "Discussion Note: Formula for Accuracy of Guessing Monero Real Spends Using Fungibility Defects"](https://github.com/Rucknium/misc-research/tree/main/Monero-Fungibility-Defect-Classifier/pdf) for more explanation.

The data in this directory can help identify which wallet implementations may be creating transactions with nonstandard fees. The developers of the wallets can be contacted and asked to fix their implementations.

## Identification of nonstandard fees in Monero

The tabulation of nonstandard fees will use "nanonero" as the basic unit. A [nanonero](https://web.getmonero.org/resources/moneropedia/denominations.html) is 0.000000001 XMR. In other words, it is 1/1000th of the smallest Monero unit, the piconero. In the tables in the  `data ` directory, fee per byte is rounded down to the nearest integer nanonero. The complete operation is `floor( (tx_fee/tx_size_bytes)/1000 )`.

Except when the dynamic block/fee algorithm is raising block size and fees, a `get_fee_estimate` RPC call to `monerod` will return these four values for nanoneros per byte: 20, 80, 320, 4000. These four levels are supposed to give transactions different priorities: "slow, normal, fast, fastest". However, since Monero blocks are usually not full, paying a higher fee usually does not mean that a transaction will be confirmed in a block any faster than a lower fee unless mining pool operators update their block templates more frequently when they receive transactions with higher fees. Any fees outside of these four values are considered nonstandard.

The `get_fee_estimate` RPC call also returns a suggestion for a "quantization mask" of 10 nanoneros. Monero transactions are supposed to round up the total fee (not fee per byte) of a transaction to 10 nanoneros of precision. Transactions do not have to follow the suggestion because is not required by blockchain consensus rules. Only 880 of the 7.9 million Monero transactions since the August 2022 hard fork do not follow the quantization mask suggestion. These transactions will not be separately tabulated because they are so rare.

## Determining which wallets are creating transactions with nonstandard fees

The timing of transactions can help form hypotheses about which wallets may be using nonstandard fees. Many wallets ceased functioning when the Monero upgraded its privacy features on August 14, 2022 with a hard fork (network upgrade). The developers of these wallets were not prepared for the required changes in transaction format: increase ring size from 11 to 16, Bulletproofs+ for smaller transaction sizes, view tags for faster wallet syncing, etc.

According to my research, at least eight wallets did not upgrade in time for the hard fork. Monerujo, Feather, Cake, and of course the GUI/CLI wallets did upgrade in time. Below is a table of wallets that were not ready for the hard fork and the wallets' probable date of return to operation. After the hard fork date, but before these wallets were fixed, almost all transactions on the blockchain had standard fee levels. After the wallets were fixed, the percentage of transactions with nonstandard fees increased to about 10%. The timing of nonstandard transactions appearing on the blockchain and the fix date of the wallets could be hints about which wallets are responsible for the nonstandard fees.

| Wallet   | Fix Date               | Source                                                                                                                    |
|----------|------------------------|:-------------------------------------------------------------------------------------------------------------------------:|
| WooKey   | 2022-08-19             | [■](https://github.com/WooKeyWallet/monero-wallet-android-app/releases/tag/v2.2.1)                                        |
| Exodus   | 2022-08-25             | [■](https://twitter.com/exodus_io/status/1562918301181034496)                                                             |
| Edge     | 2022-08-27             | [■](https://twitter.com/EdgeWallet/status/1563584457361149952)                                                            |
| MyMonero | 2022-08-29             | [■](https://twitter.com/MyMonero/status/1564149853478760448)                                                              |
| Zelcore  | Before 2022-11-17(?)   | [■](https://twitter.com/zelcore_io/status/1593287952041357318)                                                            |
| Guarda   | 2023-02-07             | [■](https://web.archive.org/web/20230207112954/https://www.reddit.com/r/GuardaWallet/comments/10vgd90/monero_xmr_wallet/) |
| Atomic   | 2023-02-21             | [■](https://twitter.com/AtomicWallet/status/1628019129553657856)                                                          |
| Coinomi  | Still nonfunctional(?) | [■](https://reddit.com/r/COINOMI/comments/14u5gp3/does_coinomi_still_support_monero/)                                     |

Wallets accessible to ordinary users are not the only wallet implementations creating transactions on the Monero blockchain. Services like centralized exchanges also create transactions. Downtime/uptime of withdrawal capability of these services can provide clues about which set of transactions with nonstandard fees they may be creating. moneroj.net has a record of Binance withdrawal suspensions: https://moneroj.net/withdrawals/

A researcher could create transactions with nonstandard wallets to provide evidence that a specific wallet is responsible for certain types of nonstandard fees. The fees of any transaction can be viewed by inputting its transaction ID into a block explorer like https://xmrchain.net/ . Note that` xmrchain.net`'s definition of kB is 1024 bytes, not 1000 bytes.

## Tabulated data

In the `data` directory are tables of the number of transactions that use each fee level since the August 2022 hard fork. There are two versions of each table. One version tabulates by day. The other version tabulates by ISO week, which is a way to give numbers to weeks in a calendar that span every Monday to Sunday. The https://www.epochconverter.com/weeks web page has a table that converts ISO week numbers to dates.

Only transactions that have exactly two outputs are included in the table. Most transactions have two outputs. Blockchain consensus rules require that transactions have at least two outputs. If the entire balance of inputs in a transaction are spent to one output, the other output has a XMR value of zero. Only wallets that enable a "pay-to-many" transaction type can create transactions with more than two outputs. The definition of "standard" fee is more complicated when the number of outputs in a transaction is greater than two (see section 7.3.2 Dynamic block weight of _Zero to Monero 2.0_), so they have been excluded from this version of the tables.

I have identified five "clusters" of nonstandard fees. These are:
1. 500-520 nanoneros per byte
2. 98-109 nanoneros per byte
3. 29-32 nanoneros per byte
4. 240600, 342450, and 444300 nanoneros fee total (about 160 nanoneros/byte for txs with 1, 2, and 3 inputs)
5. 31720000 and 45300000 nanoneros fee total

In `fee-clusters-by-week.csv` and `fee-clusters-by-day.csv` these clusters are labeled in columns as `500_520_fee_per_byte`, `98_109_fee_per_byte`, `29_32_fee_per_byte`,  `24_34_44_fee`, and `317_453_fee`, respectively.

Using new draft research, [Rucknium (2023) "Discussion Note: Formula for Accuracy of Guessing Monero Real Spends Using Fungibility Defects"](https://github.com/Rucknium/misc-research/tree/main/Monero-Fungibility-Defect-Classifier/pdf), the risk to the privacy of users who are using wallets with these fee levels can be estimated. Rucknium (2023) develops a formula for the probability that a simple classifier can correctly guess the real spend in a ring when a ring is in a transaction with a specific nonstandard fee level. This value is the Positive Predictive Value (PPV). Higher PPV means higher privacy risk. Completely random guessing between a ring's 16 ring members would produce a PPV of 1/16 = 6.25%. Rucknium (2023) also provides formulas for the proportion of transaction outputs on the blockchain with a specific nonstandard fee (`beta`) and the probability that a ring's real spend contains a wallet's change output (`mu_C`).

The PPV and `beta` can set priorities for which fee clusters to investigate first. PPV is a metric of the level of privacy risk to users who are using the wallets that create the nonstandard fees. `beta` is a rough estimate of the proportion of uses who are using each fee level. (The exact proportion of transactions with each nonstandard fee level can be checked directly in `fee-clusters-by-week.csv` and `fee-clusters-by-day.csv`.) Below is a table for the fee clusters estimated from the 8-week period July 31, 2023 to September 24, 2023.

**Estimated PPV, beta, and mu_C, in percent**
| fee                    | PPV   | beta | mu_C  |
|------------------------|-------|------|-------|
| `500_520_fee_per_byte` | 37.94 | 1.69 | 38.63 |
| `98_109_fee_per_byte`  | 19.97 | 5.09 | 21.67 |
| `29_32_fee_per_byte`   | 62.19 | 0.36 | 61.41 |
| `24_34_44_fee`         | 36.59 | 3.00 | 40.93 |
| `317_453_fee`          | 41.10 | 0.34 | 38.20 |

Note: It is believed that the wallet implementation creating `24_34_44_fee` transactions has been identified and a fix is being developed.

The files `raw-fee-counts-by-week.csv`, `raw-fee-counts-by-day.csv`, `raw-fee-counts-by-week-prevalence-sort.csv`, and `raw-fee-counts-by-day-prevalence-sort.csv` tabulate transactions by fee per byte without grouping them into clusters. Each fee level to the nanonero precision is tabulated. The `prevalence-sort` orders columns so that the more common fee levels are closest to the left side of the table.

The `example-tx-ids-by-fee.csv` file lists ten random IDs for transactions that pay each level of fee per byte in the `raw-fee-counts` files. If fewer than 10 transactions pay a given fee level, then all the transaction IDs are listed. Transaction IDs for transactions that pay 240600, 342450, 444300, 31720000, and 45300000 nanoneros fee total are listed at the top of the table with the `_fee_tx_id` suffix. Transaction IDs can be searched in `xmrchain.net` to view examples of transactions that pay each fee level.

## Code

The code to reproduce the data is in the `code` directory.


