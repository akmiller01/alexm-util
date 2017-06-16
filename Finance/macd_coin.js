#!/usr/bin/env node
var request = require('request'),
moment = require('moment'),
currency = process.argv[2],
days = process.argv.length>3?parseInt(process.argv[3]):33;

var url = "https://graphs.coinmarketcap.com/currencies/"+currency;
request(url,parseData);

function ema(prices,days){
    var num = 0,
    den = 0,
    alpha = (2/(days+1));
    for(var i = 0; i < days; i++){
        var price = prices[i];
        num += Math.pow((1-alpha),i)*price;
        den += Math.pow((1-alpha),i);
    }
    return num/den;
}

function macd(data){
    var macd = ema(data,12) - ema(data,26),
    macds = [];
    for(var i = 2; i < 9; i++){
        var tempMacd = ema(data.slice(i),12) - ema(data.slice(i),26);
        macds.push(tempMacd);
    }
    var trigger = ema(macds,macds.length);
    return([macd,trigger]);
}

function parseData(e,r,b){
    if(b){
        console.log("Date,MACD,Trigger,Price,Forecast");
        var raw = JSON.parse(b).price_usd.reverse();
        for(var i = 0; i < days; i++){
            var data = raw.slice(i),
            prices = data.map(function(d){return d[1];}),
            latest = moment.unix(data[0][0]/1000).format("YYYY-MM-DD"),
            macdArr = macd(prices),
            thisMacd = macdArr[0],
            thisTrigger = macdArr[1],
            forecast = thisMacd>thisTrigger?"BULL":"BEAR";
            console.log(latest+","+thisMacd+","+thisTrigger+","+prices[0]+","+forecast);
        }
    }
}