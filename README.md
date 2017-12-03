# pyramid-scheme

An EVM backend for SICP Scheme. This dialect of Scheme is named Pyramid.

## Getting Started

The "run-tests.rkt" is the entry point I was using to get deployable bytecode. My workflow was:

Pick an example program like prog-factorial
Run "(full-debug-output prog-factorial)" in "run-tests.rkt" to get a bytecode listing
Use the "testrpc" tool to start a test EthereumJS instance
Paste the hexadecimal string from step 2 into "deploy.js", along with a wallet from step 3.
Paste the contents of "deploy.js" into a Javascript prompt attached to the Ethereum instance in step 3.
### Prerequisites

Install the Racket language runtime at https://download.racket-lang.org

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details


<!-- ```
Give examples
```

### Installing

A step by step series of examples that tell you have to get a development env running

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.


## Acknowledgments

* Hat tip to anyone who's code was used
* Inspiration
* etc
-->
