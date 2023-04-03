![example workflow](https://github.com/Chibikuri/RuLa/actions/workflows/build_test.yaml/badge.svg)
[![codecov](https://codecov.io/gh/Chibikuri/RuLa/branch/main/graph/badge.svg?token=M4GYZPQEHY)](https://codecov.io/gh/Chibikuri/RuLa)

# RuLa
A programming language for RuleSet-based quantum repeaters. *RuleSet* [^ruleset][^architecture] is a instruction set for a quantum repeater that allows us to generate reliable entanglement between end nodes.

## Design
RuLa is designed to translate higher-level discription of RuleSet into lower-level RuleSet instructions [^architecture]. The output file of RuLa is now JSON format that contains a set of RuleSet instructions.

For more details on the design decisions and system architectures, please refer to [my master thesis](https://aqua.sfc.wide.ad.jp/publications/cocori_mthesis.pdf)
## Usage
RuLa requires Rust compiler ([rustc](https://www.rust-lang.org/tools/install)) that is grater than or equal version 1.63.0. For convenience, Rust package manager [cargo](https://doc.rust-lang.org/cargo/) is also recommended to install to compile RuLa program.


[^ruleset]:[Matsuo, Takaaki, Clément Durand, and Rodney Van Meter. "Quantum link bootstrapping using a RuleSet-based communication protocol." Physical Review A 100.5 (2019): 052320.](https://journals.aps.org/pra/abstract/10.1103/PhysRevA.100.052320) 

[^architecture]:[Van Meter, Rodney, et al. "A quantum internet architecture." 2022 IEEE International Conference on Quantum Computing and Engineering (QCE). IEEE, 2022.](https://ieeexplore.ieee.org/abstract/document/9951258)