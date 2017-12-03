# pyramid-scheme

An EVM backend for SICP Scheme. This dialect of Scheme is named Pyramid.

Here is an example of a Pyramid program: 
```scheme
(define prog-factorial 
  '(begin
     (define (factorial n)
       (if (= n 1)
           1
           (* (factorial (- n 1)) n)))
     (factorial 5)))

```

## Getting Started

Pyramid Scheme is under heavy construction. Join our [public Discord channel](https://discord.gg/854RH6x) if you'd like to contribute or use Pyramid to develop Ethereum contracts.

For now, "run-tests.rkt" is the entry point. To compile an example program there:

* Pick an example program like prog-factorial
* Use "(full-debug-output prog-factorial)" in "run-tests.rkt" to get a bytecode listing
* Use the "testrpc" tool to start a test EthereumJS instance
* Paste the hexadecimal string from step 2 into "deploy.js", along with a wallet from step 3.
* Paste the contents of "deploy.js" into a Javascript prompt attached to the Ethereum instance in step 3.

### Prerequisites

* Install the Racket language runtime at https://download.racket-lang.org
* Install necessary packages using "raco pkg install <name>". The packages are:
  [binaryio-lib](https://docs.racket-lang.org/binaryio/index.html)
  [errortrace-lib](https://docs.racket-lang.org/errortrace/using-errortrace.html)
  
## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Deployment

See [deploy.js](deploy.js) for an example deploy script.

1. Start a test Ethereum network using [testrpc](https://www.npmjs.com/package/ethereumjs-testrpc).
2. Replace the "from" variable in deploy.js with a wallet number that testrpc printed to the terminal.
3. Replace the "code" variable with the output of the Pyramid compiler
4. Run the contents of deploy.js on a Javascript console attached to the testrpc instance.

## Built With

* [Racket](http://racket-lang.org/)


## Authors

* **Michael Burge** - *Initial work* - [MichaelBurge](https://twitter.com/taurineandcode)
