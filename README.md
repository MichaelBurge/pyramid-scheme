# Pyramid Scheme

Pyramid is a dialect of the Scheme programming language targeting the Ethereum Virtual Machine(EVM).

Here is an example of a Pyramid program: 
```scheme
(begin
  (define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
  (factorial 5))
```

## Getting Started

Pyramid Scheme is under heavy construction. Join our [public Discord channel](https://discord.gg/854RH6x) if you'd like to contribute or use Pyramid to develop Ethereum contracts.

After installing the prerequisites(listed below), 

```bash
$ ./pyramid -t tests/0001-factorial.pmd 
(Test Passed:  return 120 120 ((s . 14066) (g . 47052) (z . 3591)))
```

To deploy that program on a test network:
* Install [Ganache CLI](https://github.com/trufflesuite/ganache-cli)
* Start an EthereumJS test chain: `ganache-cli -l 10000000`. It will output a list of "Available Accounts".
* Change the `from` variable in [deploy.js](deploy.js) to one of the test accounts.
* Change the `code` variable to be the hex string that the Pyramid compiler output
* Install the [Go Ethereum](https://github.com/ethereum/go-ethereum) client.
* Attach to the EthereumJS chain: `geth attach http://localhost:8545`.
* Copy the contents of `deploy.js` into the `geth` Javascript prompt.

You can see advanced command line options using `pyramid --help`.

### Prerequisites

* Install the Racket language runtime at https://download.racket-lang.org
* Install necessary packages using "raco pkg install <name>". The packages are:
  * [binaryio-lib](https://docs.racket-lang.org/binaryio/index.html)
  * [errortrace-lib](https://docs.racket-lang.org/errortrace/using-errortrace.html)
* Ensure the `keccak-256sum` tool is available on your PATH
  * https://github.com/maandree/sha3sum
  
### Editor

I recommend Emacs with `racket-mode`. You should add Pyramid's `.pmd` extension to the list of `racket-mode` file extensions:

```
(add-to-list 'auto-mode-alist '("\\.pmd\\'" . racket-mode))
```

## License

This project is licensed under the GNU AGPL 3.0 - see the [LICENSE.md](LICENSE.md) file for details

## Built With

* [Racket](http://racket-lang.org/)

## Authors

* [Michael Burge](https://twitter.com/taurineandcode) - *Main Developer*
