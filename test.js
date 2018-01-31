var alice   = "0x5d4e477aa63875a5a3185b11d61c50adacb97d16";
var bob     = "0xccd6078ded3ee8fd2a3896f3f822bf9930556bf0";
var charlie = "0x0806e6c3ab8722044a1e5c204d38d4b8af7635a0";

var gas = 1000000;
var contractAddress = "0xfdd4767b1783201b4e2093b26f6c8c40ba1a2ca3";

var receipt;

var g_transactionHash;

var printUserBalances = function() {
    console.log("Contract", JSON.stringify(web3.eth.getBalance(contractAddress)));
    console.log("Alice",    JSON.stringify(web3.eth.getBalance(alice)));
    console.log("Bob",      JSON.stringify(web3.eth.getBalance(bob)));
    console.log("Charlie",  JSON.stringify(web3.eth.getBalance(charlie)));
};

var result;
var g_o;

var leftpad = function(x) { return "0x000000000000000000000000" + x.slice(2); };

var printBalances = function(err, transactionHash) {
    console.log(err, transactionHash);
    if (!err) {
        g_transactionHash = transactionHash;
        receipt = web3.eth.getTransactionReceipt(transactionHash);
        console.log("Receipt",  JSON.stringify(receipt));
        printUserBalances();
    } else {
        console.log(err);
    }
};

var test = function(o) {
    o.gas = gas;
    o.to = contractAddress;
    if (o.data) {
        o.data = leftpad(o.data);
    }
    g_o = o;
    var result = web3.eth.call(g_o);
    console.log("Result", result);
    web3.eth.sendTransaction(o, printBalances);
    return result;
};

// (init (sender 'alice) (value 0))
var baseline = web3.eth.getBalance(alice);

// (sender 'bob)     (value 100) (assert-balance 'contract 0) (assert-balance 'alice 100) (assert-return 100))

test({ from: bob, value: 100 });

// (txn (sender 'bob)     (value 150) (assert-balance 'contract 0) (assert-balance 'alice 250) (assert-return 250))

test({ from: bob, value: 150 });

// (txn (sender 'charlie)             (data (sender 'charlie))                                 (assert-return 0))

test({ from: charlie, data: charlie });

// (txn (sender 'charlie)             (data (sender 'bob))                                     (assert-return 250))

test({ from: charlie, data: bob });

// (sender 'charlie) (value 100) (data (sender 'bob))         (assert-balance 'alice 350) (assert-return 250))

test({ from: charlie, data: bob, value: 100 });

// (sender 'charlie) (value 100) (data (sender 'charlie))     (assert-balance 'alice 450) (assert-return 200))

test({ from: charlie, data: charlie, value: 100 });

// (txn (sender 'charlie) (value 0) (assert-return 200) (assert-balance 'alice    450) (assert-balance 'bob      0) (assert-balance 'charlie  0) (assert-balance 'contract 0))

test({ from: charlie, value: 0 });
