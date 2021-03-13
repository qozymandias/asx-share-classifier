#!/usr/bin/python3
"""
Module documentation.
"""

# Imports
import sys
import time
import os
import pandas as pd
import datetime
import pandas_datareader.data as web
from pandas import Series, DataFrame
import matplotlib.pyplot as plt
from matplotlib import style
import matplotlib as mpl
import csv
import re
from bs4 import BeautifulSoup
from urllib.request import urlopen, URLError
import json

def createList( soup, u):
    alldata = soup.find_all("tbody")

    try:
        table1 = alldata[0].find_all("tr")
    except:
        table1=None
    try:
        table2 = alldata[1].find_all("tr")
    except:
        table2 = None
    
    l={}
    u=list()

    for i in range(0,len(table1)):
        try:
            table1_td = table1[i].find_all("td")
        except:
            table1_td = None 
        l[table1_td[0].text] = table1_td[1].text
        u.append(l)
        l ={}

    for i in range(0,len(table2)):
        try:
            table2_td = table2[i].find_all("td")
        except:
            table2_td = None 
        l[table2_td[0].text] = table2_td[1].text
        u.append(l)
        l ={}

    return u

def getValue(allTd, keyStatistic):
    for t in allTd:
        tdValue = t.find(text=re.compile(re.escape("%s" % keyStatistic)))
        if tdValue:
            #print '"'+t.get_text()+'",'
            return tdValue.findNext('td').text
            #return tdValue.parent.nextSibling.text
    return "NA"

# example CBA.AX
def getFunStats(writeFile, tickers, keyStatistics):
    result = pd.DataFrame(index = tickers, columns = keyStatistics)
    #print("[")
    with open(writeFile, 'w') as f:
        infos = "["
        f.write("[\n")
        for (i, ticker) in enumerate(tickers):
            infos1 = ""
            url = 'http://finance.yahoo.com/q/ks?s='+ticker+'+Key+Statistics'
            try:
                resp = urlopen(url)
            except URLError as e:
                time.sleep(5)
                continue
            soup = BeautifulSoup(resp.read(), 'html.parser')
    #tdj 
            allTd = soup.find_all('td',attrs={'class':'yfnc_tablehead1'})
            newL = []
            try:
                l = [ re.sub("'", '"', re.sub('[{}]', '', str(x))) for x in createList(soup, result)]

                infos1 += "{" + "\"name\":"+ '"' + str(ticker)[:-3] + '"' +  "," + ",".join(l) + "}"

                newL = transformL(infos1)

            except:
                continue


            #d =  { 
            #     , 'stats' : json.loads(",".join(l)) }

            #dicL.append(d)
    #        print("{'name': '" + str(ticker)[:-3] +"',")
    #        print("'stats':{")
    #        print(",".join(l))
    #        print("}}")
            f.write("{}\n".format(newL))

            if (i != len(tickers) -1):
                f.write(",\n")
                
            #
            #infos += newL 

    #        for keyStatistic in keyStatistics:
    #            result.loc[ticker, keyStatistic] = getValue(allTd, keyStatistic)
    #
    #    result.to_csv("fundamentalData.csv", header=True, index=True, index_label="ticker")
        #infos += "]"
        f.write("]\n")

#
#    with open('output.json', 'w') as f:
#        f.write("[\n")
#        for (i, item) in enumerate(newInfos):
#            if (i != len(newInfos) -1):
#                com = ","
#            else:
#                com = ""
#            f.write("{}{}\n".format(item, com ))
#        f.write("]\n")

    #print (json.dumps(newInfos))

def transformL(infos):

    #print ( infos)
    share = json.loads(infos)
    
    #newInfos = []
    #for share in j:
    newS = {}
    score = []
    name =  ""
    try: 
        name = share['name']
        for k,v in share.items():
            if k and k != 'name' and v != 'N/A':
                m = 1
                if v[-1] == 'k' or v[-1] == 'M' or v[-1] == 'B' or v[-1] == '%':
                    if v[-1] == 'k':
                        m = 1000
                    elif v[-1] == 'M':
                        m = 1000000
                    elif v[-1] == 'B':
                        m = 100000000

                    v = v[:-1]
                score.append(float(v)*float(m))  #*(1/float(len(share)-1))
    except:
        newS['name'] = ""
        newS['score'] = []
        return newS

    newS['name'] = name
    newS['score'] = score

    #    newInfos.append(newS)

    return json.dumps(newS)


def main():
    args = sys.argv[1:]
    keyStatistics = [
        "Market Cap ",
        "Enterprise Value",
        "Trailing P/E ",
        "Forward P/E ",
        "PEG Ratio ",
        "Price/Sales (ttm)",
        "Price/Book (mrq)",
        "Enterprise Value/Revenue (ttm)",
        "Enterprise Value/EBITDA (ttm)",
        "Fiscal Year Ends",
        "Most Recent Quarter (mrq)",
        "Profit Margin (ttm)",
        "Operating Margin (ttm)",
        "Return on Assets (ttm)",
        "Return on Equity (ttm)",
        "Revenue (ttm)",
        "Revenue Per Share (ttm)",
        "Qtrly Revenue Growth (yoy)",
        "Gross Profit (ttm)",
        "EBITDA (ttm)",
        "Net Income Avl to Common (ttm)",
        "Diluted EPS (ttm)",
        "Qtrly Earnings Growth (yoy)",
        "Total Cash (mrq)",
        "Total Cash Per Share (mrq)",
        "Total Debt (mrq)",
        "Total Debt/Equity (mrq)",
        "Current Ratio (mrq)",
        "Book Value Per Share (mrq)",
        "Operating Cash Flow (ttm)",
        "Levered Free Cash Flow (ttm)",
        "Beta",
        "52-Week Change",
        "S&P500 52-Week Change",
        "52-Week High ",
        "52-Week Low ",
        "50-Day Moving Average",
        "200-Day Moving Average",
        "Avg Vol (3 month)",
        "Avg Vol (10 day)",
        "Shares Outstanding",
        "Float",
        "% Held by Insiders",
        "% Held by Institutions",
        "Shares Short ",
        "Short Ratio ",
        "Short % of Float ",
        "Shares Short (prior month)",
        "Forward Annual Dividend Rate",
        "Forward Annual Dividend Yield",
        "Trailing Annual Dividend Yield",
        "Trailing Annual Dividend Yield",
        "5 Year Average Dividend Yield",
        "Payout Ratio",
        "Dividend Date",
        "Ex-Dividend Date",
        "Last Split Factor (new per old)",
        "Last Split Date"
    ]
#
    if not args:
        print('usage: [--flags options] [inputs] ')
        sys.exit(1)

    #l = ["CBA.AX", "TNT.AX", "TWE.AX"]

    filename = './asx-listed-companies.csv'

    tickers = []
    with open(filename, newline='') as csvfile:
        stockreader = csv.reader(csvfile, delimiter=' ', quotechar=',')
        for i, row in enumerate(stockreader):
            if i == 0 or i == 1:
                continue
#            if i > 4:
#                break
            x = str((row[0].split(','))[0]) + ".AX"
            #print(x)
            tickers.append(x)

    getFunStats(args[0], tickers, keyStatistics)
#            start = datetime.datetime(2020, 9, 21)
#            end = datetime.datetime(2020, 9, 25)
#
#            df = web.DataReader(x, 'yahoo', start, end)
#            print(df.tail())



#    for x in l:
        #print(df)

#        close_px = df['Adj Close']
#        mavg = close_px.rolling(window=100).mean()
#        print(mavg)

#        mpl.rc('figure', figsize=(8, 7))
#        mpl.__version__
#
#        # Adjusting the style of matplotlib
#        style.use('ggplot')
#
#        close_px.plot(label=x)
#        mavg.plot(label='mavg')
#        plt.legend()

if __name__ == '__main__':
    main()
