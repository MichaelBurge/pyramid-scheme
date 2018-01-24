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
$ ./pyramid examples/factorial.pmd
<long hex string>
```

To deploy that program on a test network:
* Use the [testrpc](https://www.npmjs.com/package/ethereumjs-testrpc) tool to start a test EthereumJS instance
* Change the `from` variable in [deploy.js](deploy.js) to one of the test addresses that `testrpc` output.
* Change the `code` variable to be the hex string that the Pyramid compiler output
* Copy the contents of `deploy.js` into the `testrpc` Javascript prompt.

You can see advanced command line options using `pyramid --help`.

### Prerequisites

* Install the Racket language runtime at https://download.racket-lang.org
* Install necessary packages using "raco pkg install <name>". The packages are:
  * [binaryio-lib](https://docs.racket-lang.org/binaryio/index.html)
  * [errortrace-lib](https://docs.racket-lang.org/errortrace/using-errortrace.html)
  
### Editor

I recommend Emacs with `racket-mode`. You should add Pyramid's `.pmd` extension to the list of `racket-mode` file extensions:

```
(add-to-list 'auto-mode-alist '("\\.pmd\\'" . racket-mode))
```

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Built With

* [Racket](http://racket-lang.org/)

## Authors

* [Michael Burge](https://twitter.com/taurineandcode) - *Main Developer*
