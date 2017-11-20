var from = "0x0703722a41e9ec7a0497043380a3cc6b16eb242d";
var code = "604f600c600039604f6000f36080606052600160069060200260605180910160605280918252906020015060205260026000906104d2919060200260605180910160605280918252906020019182529060200150602001602090f3";
var gas = 10000000;

var g_transactionHash;

web3.eth.sendTransaction({from: from, data: code, gas: gas}, function(err, transactionHash) {
    if (!err)
        g_transactionHash = transactionHash;
});

var receipt = web3.eth.getTransactionReceipt(g_transactionHash);

web3.eth.call({ from: from, to: receipt.contractAddress });
