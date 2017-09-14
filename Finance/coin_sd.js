#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
currency = process.argv[2]===undefined?"bitcoin":process.argv[2],
days = process.argv.length>3?parseInt(process.argv[3]):30;

var url = "https://graphs.coinmarketcap.com/currencies/"+currency;
request(url,parseData);

function parseData(e,r,b){
    if(b){
        var data = JSON.parse(b).price_usd.reverse(),
        prices = data.map(function(d){return d[1];}).slice(0,days),
        currPrice = prices[0],
        dates = data.map(function(d){return d[0];}).slice(0,days),
        stdDev = standardDev(prices),
        now = moment.unix(dates[0]/1000),
        then = moment.unix(dates[days-1]/1000);
    console.log("The standard deviation of daily changes for "+currency+" the last "+prices.length+" trading days"+
                " (from "+now.format("MMMM Do, YYYY")+
                " to "+then.format("MMMM Do, YYYY")+
                ") is "+stdDev.toFixed(2)+
                ", or "+decodeURI('%C2%B1')+((stdDev/currPrice)*100).toFixed(2)+"% of the latest close."
                );
    console.log("");
    console.log("Today's 68% confidence interval is from "+(currPrice-(1*stdDev)).toFixed(2)+" to "+(currPrice+(1*stdDev)).toFixed(2)+".");
    console.log("");
    console.log("Today's 95% confidence interval is from "+(currPrice-(2*stdDev)).toFixed(2)+" to "+(currPrice+(2*stdDev)).toFixed(2)+".");
    }
}

function standardDev(values){
    var avg = average(values);
    var squareDiffs = values.map(function(value){
        var diff = value - avg;
        var sqrDiff = diff * diff;
        return sqrDiff;
    });
    var avgSquareDiff = average(squareDiffs);
    var stdDev = Math.sqrt(avgSquareDiff);
    return stdDev;
};
 
function average(data){
    var sum = data.reduce(function(sum, value){
        return sum + value;
    }, 0);
    var avg = sum / data.length;
    return avg;
};