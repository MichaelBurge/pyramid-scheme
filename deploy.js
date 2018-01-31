var from = alice;
var code = "insert pyramid compiler output here";

var gas = 10000000;

var g_transactionHash;

web3.eth.sendTransaction({from: from, data: code, gas: gas}, function(err, transactionHash) {
    if (!err) {
        g_transactionHash = transactionHash;
    } else {
        console.log("ERROR", err);
    }

});

var receipt = web3.eth.getTransactionReceipt(g_transactionHash);

web3.eth.call({ from: from, to: receipt.contractAddress, gas: gas });
