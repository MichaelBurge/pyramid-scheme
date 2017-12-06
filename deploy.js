var from = "0x0703722a41e9ec7a0497043380a3cc6b16eb242d";
var code = "paste the output of the Pyramid compiler here";
var gas = 10000000;

var g_transactionHash;

web3.eth.sendTransaction({from: from, data: code, gas: gas}, function(err, transactionHash) {
    if (!err)
        g_transactionHash = transactionHash;
});

var receipt = web3.eth.getTransactionReceipt(g_transactionHash);

web3.eth.call({ from: from, to: receipt.contractAddress });
