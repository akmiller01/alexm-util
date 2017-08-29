#!/usr/bin/python
# -*- coding: utf-8 -*-

from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_CENTER
import copy
import pandas as pd
import pdb
import numbers

style = getSampleStyleSheet()
whiteParaStyle = ParagraphStyle('whiteParaStyle',parent=style['BodyText'],textColor="white",alignment=TA_CENTER)
greyParaStyle = ParagraphStyle('greyParaStyle',parent=style['BodyText'],textColor="#443e42")
greyCenterParaStyle = ParagraphStyle('greyParaStyle',parent=style['BodyText'],textColor="#443e42",alignment=TA_CENTER)
#Read a CSV to make this data?
dataDictionary = {"Eastern Africa":{}}
dataDictionary["Eastern Africa"]["country"] = "Eastern Africa"
dataDictionary["Eastern Africa"]["table1"] = [
    [Paragraph("Gross Domestic Product (GDP) per capita (PPP) (n = 17)",style=greyParaStyle),"1,894",2014]
    ,["US$1.25/day (%) (n = 14)","50","2002-2012"]
    ,["US$2/day (%) (n = 14)","75","2002-2012"]
    ]
dataDictionary["Eastern Africa"]["table2"] = [
    [Paragraph("Under five mortality rate (deaths per 1000 live births) (n = 18)",style=greyParaStyle),"70",2013]
    ]
dataDictionary["Eastern Africa"]["table3"] = [
    ["Population (000) (n = 18)","393,625",2015]
    ,["Under-5 population (000) (n = 18)","63,703",2015]
    ,["Urban (%) (n = 18)","25",2015]
    ,[">65 years (000) (n = 18)","6",2015]
    ]
dataDictionary["Eastern Africa"]["table4"] = [
    ["Number of children under 5 affected (000)","",""]
    ,[Paragraph("Stunting<super>a</super> (n = 18)",style=greyParaStyle),"23,968",2014]
    ,[Paragraph("Wasting<super>a</super> (n = 18)",style=greyParaStyle),"4,205",2014]
    ,[Paragraph("Overweight<super>a</super> (n = 18)",style=greyParaStyle),"3,016",2014]
    ]
dataDictionary["Eastern Africa"]["table5"] = [
    ["Percentage of children under 5 affected","",""]
    ,[Paragraph("Stunting<super>a</super> (n = 18)",style=greyParaStyle),"38",2014]
    ,[Paragraph("Wasting<super>a</super> (n = 18)",style=greyParaStyle),"7",2014]
    ,[Paragraph("Overweight<super>a</super> (n = 18)",style=greyParaStyle),"5",2014]
    ,[Paragraph("Low birth weight<super>b</super> (n = 15)",style=greyParaStyle),"14","2000-2011"]
    ]
dataDictionary["Eastern Africa"]["table6"] = [
    [Paragraph("Adolescent overweight<super>a</super>",style=greyParaStyle),"NA","NA"]
    ,[Paragraph("Adolescent obesity<super>a</super>",style=greyParaStyle),"NA","NA"]
    ,[Paragraph("Women of reproductive age, thinness<super>b</super> (n = 13)",style=greyParaStyle),15,"2002-2012"]
    ,[Paragraph("Women of reproductive age, short stature<super>b</super> (n = 13)",style=greyParaStyle),2,"2002-2012"]
]
dataDictionary["Eastern Africa"]["table7"] = [
    [Paragraph("Women of reproductive age with anemia<super>a</super> (n = 17)",style=greyParaStyle),"",""]
    ,["Total population affected (000)",format(22105,",d"),2011]
    ,["Total population affected (%)",format(28,"d"),2011]
    ,[Paragraph("Vitamin A deficiency in children 6-59 months old (%)<super>b</super> (n=17)",style=greyParaStyle),format(48,"d"),2013]
    ,[Paragraph("Population classification of iodine nutrition (age group 5-19)<super>c</super> (n = 10)",style=greyParaStyle),Paragraph("Mild iodine deficiency/Optimal iodine nutrition/Risk of adverse health consequences/Risk of iodine-induced hyperthyroidism",style=greyParaStyle),"1993-2000"]
]
dataDictionary["Eastern Africa"]["table8"] = [
    [
        Paragraph("<b>Under-5 stunting, 2015<super>a</super></b>",style=whiteParaStyle)
        ,Paragraph("<b>Under-5 wasting, 2015<super>b</super></b>",style=whiteParaStyle)
        ,Paragraph("<b>Under-5 overweight, 2015<super>a</super></b>",style=whiteParaStyle)
        ,Paragraph("<b>WRA Anemia, 2011<super>b</super></b>",style=whiteParaStyle)
        ,Paragraph("<b>EBF, 2014-2015<super>a</super></b>",style=whiteParaStyle)
     ]
    ,["1/16 on course","7/16 on course","11/15 on course","2/17 on course","6/12 on course"]
]
dataDictionary["Eastern Africa"]["table8a"] = [
    [
        Paragraph("<b>Adult female obesity, 2015<super>a</super></b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult male obesity, 2015<super>a</super></b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult female diabetes, 2015<super>a</super></b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult male diabetes, 2015<super>a</super></b>",style=whiteParaStyle)
     ]
    ,["X/Y on course","X/Y on course","X/Y on course","X/Y on course"]
]
dataDictionary["Eastern Africa"]["table9"] = [
    [Paragraph("Severe acute malnutrition, geographic coverage<super>a</super> (n = 12)",style=greyParaStyle),"9","2012"]
    ,[Paragraph("Vitamin A supplementation, full coverage<super>b</super> (n = x)",style=greyParaStyle),"65","2013"]
    ,[Paragraph("Children under 5 with diarrhea receiving ORS<super>b</super> (n = x)",style=greyParaStyle),"44","2011"]
    ,[Paragraph("Immunization coverage, DTP3<super>b</super> (n = x)",style=greyParaStyle),"78","2013"]
    ,[Paragraph("Iodized salt consumption<super>b</super> (n = x)",style=greyParaStyle),"87","2006"]
]
dataDictionary["Eastern Africa"]["table10"] = [
    ["Minimum acceptable diet (n = x)","6","2011"]
    ,["Minimum dietary diversity (n = x)","13","2011"]
]
dataDictionary["Eastern Africa"]["table11"] = [
    [Paragraph("Early childbearing: births by age 18 (%)<super>a</super> (n = x)",style=greyParaStyle),"33","2011"]
    ,[Paragraph("Gender Inequality Index (score*)<super>b</super> (n = x)",style=greyParaStyle),"0.529","2013"]
    ,[Paragraph("Gender Inequality Index (country rank)<super>b</super> (n = x)",style=greyParaStyle),"155","2013"]
]
dataDictionary["Eastern Africa"]["table12"] = [
    ["Physicians (n = x)","0.117","2005"]
    ,["Nurses and midwives (n = x)","1.306","2005"]
    ,["Community health workers (n = x)","0.188","2005"]
]
dataDictionary["Eastern Africa"]["table13"] = [
    [Paragraph("National implementation of the International Code of Marketing of Breast-milk Substitutes<super>a</super> (n = x)",style=greyParaStyle),"Law","2014"]
    ,[Paragraph("Extent of constitutional right to food<super>b</super> (n = x)",style=greyParaStyle),"High","2003"]
    ,[Paragraph("Maternity protection (Convention 183)<super>c</super> (n = x)",style=greyParaStyle),"No","2011"]
    ,[Paragraph("Wheat fortification legislation<super>d</super> (n = x)",style=greyParaStyle),"Mandatory","2015"]
    ,[Paragraph("Undernutrition mentioned in national development plans and economic growth strategies<super>e</super> (n = x)",style=greyParaStyle),"Rank: 39/126","2010-2015"]
    ,[Paragraph("Overnutrition mentioned in national development plans and economic growth strategies<super>e</super> (n = x)",style=greyParaStyle),"Rank: 57/116","2010-2015"]
]
dataDictionary["Eastern Africa"]["table14"] = [
    [Paragraph("All major NCDs (n = x)",style=greyParaStyle),Paragraph("Available, partially implemented",style=greyParaStyle),"2010"]
]

# dataDictionary["Afghanistan"] = copy.deepcopy(dataDictionary["Eastern Africa"])

missingVals = [" ",".",""]
def safeFormat(x,commas=False,precision=0):
    if pd.isnull(x):
        return "NA"
    elif x in missingVals:
        return "NA"
    else:
        if not isinstance(x,numbers.Number):
            return x
        if precision == 0:
            x = int(x)
        else:
            x = round(x,precision)
        if commas:
            return format(x,",")
        else:
            return x

# dat = pd.read_csv("regional_data.csv")
for region in dataDictionary.keys():
    # row = dat.loc[(dat.region==region)].iloc[0]
    dataDictionary[region]["region"] = region


tableStyles = {}
tableStyles["table1"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table2"] = [
    ('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table3"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('BACKGROUND',(0,3),(-1,3),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table4"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('BACKGROUND',(0,3),(-1,3),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,1),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEABOVE',(0,1),(-1,1),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('SPAN',(0,0),(-1,0))
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table5"] = tableStyles["table4"]
tableStyles["table6"] = tableStyles["table3"]
tableStyles["table7"] = [
    ('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEABOVE',(0,1),(-1,1),1,"#f79c2a")
    ,('LINEABOVE',(0,3),(-1,3),1,"#f79c2a")
    ,('LINEBELOW',(0,4),(-1,4),1,"#f79c2a")
    ,('SPAN',(0,0),(-1,0))
    ,('LINEAFTER',(0,1),(1,2),.5,"#fbcd99")
    ,('LINEBELOW',(0,1),(-1,1),.5,"#fbcd99")
    ,('LINEAFTER',(0,3),(1,-1),1,"#fbcd99")
    ,('LINEABOVE',(0,-1),(-1,-1),1,"#fbcd99")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
]
tableStyles["table8"] = [
    ('TEXTCOLOR',(0,0),(-1,0),"white")
    # ,('BACKGROUND',(0,0),(-1,0),"#204d5e")
    # ,('BACKGROUND',(0,1),(-1,1),"white")
    # ,('GRID',(0,0),(-1,-1),1,"#386170")
    ,('ALIGN',(0,0),(-1,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('TEXTCOLOR',(0,1),(-1,-1),"#443e42")
    ]
tableStyles["table8a"] = tableStyles["table8"]
tableStyles["table9"] = tableStyles["table3"]
tableStyles["table10"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table11"] = tableStyles["table10"]
tableStyles["table12"] = tableStyles["table10"]
tableStyles["table13"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('BACKGROUND',(0,3),(-1,3),"#fef5e7")
    ,('BACKGROUND',(0,5),(-1,5),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table14"] = tableStyles["table10"]