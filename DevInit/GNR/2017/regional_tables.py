#!/usr/bin/python
# -*- coding: utf-8 -*-

from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_CENTER
import copy
import pandas as pd
import pdb
import numbers
import re

style = getSampleStyleSheet()
whiteParaStyle = ParagraphStyle('whiteParaStyle',parent=style['BodyText'],textColor="white",alignment=TA_CENTER)
greyParaStyle = ParagraphStyle('greyParaStyle',parent=style['BodyText'],textColor="#443e42")
greyCenterParaStyle = ParagraphStyle('greyParaStyle',parent=style['BodyText'],textColor="#443e42",alignment=TA_CENTER)
offCourseStyle = ParagraphStyle('offCourseStyle',parent=style['BodyText'],textColor="#d83110",alignment=TA_CENTER)
progressStyle = ParagraphStyle('progressStyle',parent=style['BodyText'],textColor="#f39000",alignment=TA_CENTER)
onCourseStyle = ParagraphStyle('onCourseStyle',parent=style['BodyText'],textColor="#d8da00",alignment=TA_CENTER)

def condStyle(progress):
    if progress=="On course":
        return onCourseStyle
    elif progress=="No progress or worsening":
        return offCourseStyle
    elif progress=="Some progress":
        return progressStyle
    elif progress=="Off course":
        return offCourseStyle
    else:
        return greyCenterParaStyle
    
def replaceDash(x):
    x = str(x)
    y = re.sub(r"((?:^|[^{])\d+)-(\d+[^}])",u"\\1\u2013\\2", x)
    return y
missingVals = [" ",".","","Insufficient data to make assessment"]
def safeFormat(x,commas=False,precision=0):
    if pd.isnull(x):
        return "NA"
    elif x in missingVals:
        return "NA"
    else:
        if not isinstance(x,numbers.Number):
            return replaceDash(x)
        if precision == 0:
            x = int(x)
        else:
            x = round(x,precision)
        if commas:
            return format(x,",")
        else:
            return x
    

#Read a CSV to make this data?
dataDictionary = {"Eastern Africa":{}}
dataDictionary["Eastern Africa"]["table1"] = [
    [Paragraph("GDP per capita (PPP$) (n=14)",style=greyParaStyle),"2,002",safeFormat(2016)]
    ,["$1.90/day (%) (n=13)","49",safeFormat("2000-2013")]
    ,["$3.10/day (%) (n=13)","75",safeFormat("2000-2013")]
    ]
dataDictionary["Eastern Africa"]["table2"] = [
    [Paragraph("Under five mortality rate (deaths per 1,000 live births) (n=18)",style=greyParaStyle),"61",safeFormat(2015)]
    ]
dataDictionary["Eastern Africa"]["table3"] = [
    ["Population (thousands) (n=18)","420,907",safeFormat(2017)]
    ,[Paragraph("Under-5 population (thousands) (n=18)",style=greyParaStyle),"66,906",safeFormat(2017)]
    ,["Urban (%) (n=18)","27",safeFormat(2017)]
    ,[">65 years (%) (n=18)","3",safeFormat(2017)]
    ]
dataDictionary["Eastern Africa"]["table4"] = [
    ["Number of children under 5 affected (millions)","",""]
    ,[Paragraph("Stunting<super>1</super> (n=17)",style=greyParaStyle),"24",safeFormat(2016)]
    ,[Paragraph("Wasting<super>1</super> (n=17)",style=greyParaStyle),"4",safeFormat(2016)]
    ,[Paragraph("Overweight<super>1</super> (n=17)",style=greyParaStyle),"3",safeFormat(2016)]
    ]
dataDictionary["Eastern Africa"]["table5"] = [
    ["Percentage of children under 5 affected","",""]
    ,[Paragraph("Stunting<super>1</super> (n=17)",style=greyParaStyle),"36",safeFormat(2016)]
    ,[Paragraph("Wasting<super>1</super> (n=17)",style=greyParaStyle),"7",safeFormat(2016)]
    ,[Paragraph("Severe Wasting<super>1</super> (n=16)",style=greyParaStyle),"2",safeFormat(2016)]
    ,[Paragraph("Overweight<super>1</super> (n=17)",style=greyParaStyle),"5",safeFormat(2016)]
    ,[Paragraph("Low birth weight<super>2</super> (n=15)",style=greyParaStyle),"14",safeFormat("2000-2012")]
    ]
dataDictionary["Eastern Africa"]["table6"] = [
    [Paragraph("Adolescent overweight<super>1</super> (n=0)",style=greyParaStyle),"NA",safeFormat("NA")]
    ,[Paragraph("Adolescent obesity<super>1</super> (n=0)",style=greyParaStyle),"NA",safeFormat("2003-2014")]
    ,[Paragraph("Women of reproductive age, thinness<super>2</super> (n=13)",style=greyParaStyle),16,safeFormat("1994-2015")]
    ,[Paragraph("Women of reproductive age, short stature<super>2</super> (n=13)",style=greyParaStyle),3,safeFormat("1994-2015")]
]
dataDictionary["Eastern Africa"]["table7"] = [
    [Paragraph("Women of reproductive age with anaemia<super>1</super> (n=18)",style=greyParaStyle),"",""]
    ,["Total population affected (thousands)",format(30054,",d"),safeFormat(2016)]
    ,["Total population affected (%)",format(48,"d"),safeFormat(2016)]
    ,[Paragraph(u"Vitamin A deficiency in children 6\u201359 months old (%)<super>2</super> (n=17)",style=greyParaStyle),format(48,"d"),safeFormat(2013)]
    ,[Paragraph(u"Population classification of iodine nutrition (age group 5\u201319 years)<super>3</super> (n=10)",style=greyParaStyle),Paragraph("Mild iodine deficiency",style=greyParaStyle),safeFormat("1996-2001")]
]
dataDictionary["Eastern Africa"]["table8"] = [
    [
        Paragraph("<b>Under-5 stunting</b>",style=whiteParaStyle)
        ,Paragraph("<b>Under-5 wasting</b>",style=whiteParaStyle)
        ,Paragraph("<b>Under-5 overweight</b>",style=whiteParaStyle)
        ,Paragraph("<b>WRA Anaemia</b>",style=whiteParaStyle)
        ,Paragraph("<b>EBF</b>",style=whiteParaStyle)
     ]
    ,["1/18 on course","5/18 on course","4/18 on course","0/18 on course","4/18 on course"]
]
dataDictionary["Eastern Africa"]["table8a"] = [
    [
        Paragraph("<b>Adult female obesity</b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult male obesity</b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult female diabetes</b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult male diabetes</b>",style=whiteParaStyle)
     ]
    ,["0/18 on course","0/18 on course","0/18 on course","0/18 on course"]
]
dataDictionary["Eastern Africa"]["table9"] = [
    [Paragraph("Severe acute malnutrition, geographic coverage<super>1</super> (n=12)",style=greyParaStyle),"60",safeFormat("2012")]
    ,[Paragraph("Vitamin A supplementation, full coverage<super>2</super> (n=14)",style=greyParaStyle),"66",safeFormat("2014")]
    ,[Paragraph("Children under 5 with diarrhoea receiving ORS<super>2</super> (n=16)",style=greyParaStyle),"41",safeFormat("2000-2016")]
    ,[Paragraph("Immunization coverage, DTP3<super>3</super> (n=18)",style=greyParaStyle),"81",safeFormat("2016")]
    ,[Paragraph("Iodized salt consumption<super>2</super> (n=14)",style=greyParaStyle),"51",safeFormat("2000-2013")]
]
dataDictionary["Eastern Africa"]["table10"] = [
    [Paragraph(u"Exclusive breastfeeding 0\u20135 months (n=11)",style=greyParaStyle),"9",safeFormat("2010-2016")]
    ,[Paragraph(u"Minimum acceptable diet 6\u201323 months (n=11)",style=greyParaStyle),"9",safeFormat("2010-2016")]
    ,[Paragraph(u"Minimum dietary diversity 6\u201323 months (n=11)",style=greyParaStyle),"20",safeFormat("2010-2016")]
]
dataDictionary["Eastern Africa"]["table11"] = [
    [Paragraph("Early childbearing: births by age 18 (%)<super>1</super> (n=14)",style=greyParaStyle),"33",safeFormat("2011")]
    ,[Paragraph("Gender Inequality Index (score*)<super>2</super> (n=11)",style=greyParaStyle),"0.53",safeFormat("2015")]
    ,[Paragraph("Female secondary education enrolment rate (%)<super>3</super> (n=12)",style=greyParaStyle),"31",safeFormat("2006-2012")]
]
dataDictionary["Eastern Africa"]["table12"] = [
    ["Physicians (n=17)","0.08",safeFormat("2001-2005")]
    ,["Nurses and midwives (n=17)","0.56",safeFormat("1994-2015")]
    ,["Community health workers (n=10)","0.38",safeFormat("1992-2013")]
]
dataDictionary["Eastern Africa"]["table13"] = [
    [Paragraph("National implementation of the International Code of Marketing of Breast-milk Substitutes<super>1</super> (n=18)",style=greyParaStyle),Paragraph("Full provisions in law",style=greyCenterParaStyle),safeFormat("2016")]
    ,[Paragraph("Extent of constitutional right to food<super>2</super> (n=12)",style=greyParaStyle),Paragraph("Medium",style=greyCenterParaStyle),safeFormat("2003")]
    ,[Paragraph("Maternity Protection Convention 183<super>3</super> (n=17)",style=greyParaStyle),Paragraph("Partial",style=greyCenterParaStyle),safeFormat("2011")]
    ,[Paragraph("Wheat fortification legislation<super>4</super> (n=17)",style=greyParaStyle),Paragraph("Mandatory",style=greyCenterParaStyle),safeFormat("2015")]
]
dataDictionary["Eastern Africa"]["table14"] = [
    [Paragraph("All major NCDs (n=15)",style=greyParaStyle),Paragraph("No/Yes",style=greyParaStyle),safeFormat("2015")]
]

dataDictionary["Eastern Africa"]["c5note"] = "<i>Note: n=17. Data are population-weighted means.</i>"
dataDictionary["Eastern Africa"]["c6note"] = "<i>Note: BMI: body mass index. n=17. Data are population-weighted means.</i>"
dataDictionary["Eastern Africa"]["c9note"] = "<i>Note: n is between 7 and 12 depending on the indicator and year.</i>"
dataDictionary["Eastern Africa"]["c11note"] = "<i>n is between 16 and 18 depending on the indicator and year.</i>"
dataDictionary["Eastern Africa"]["c12note"] = "<i>n is between 16 and 18 depending on the indicator and year.</i>"
dataDictionary["Eastern Africa"]["c13note"] = "<i>Note: n is between 9 and 10 depending on the indicator and year.</i>"
dataDictionary["Eastern Africa"]["c14note"] = "<i>12 SUN member countries.</i>"
dataDictionary["Eastern Africa"]["progressYear"] = "PROGRESS AGAINST GLOBAL NUTRITION TARGETS 2017"

dataDictionary["Global"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Australia and New Zealand"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Caribbean"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Central America"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Central Asia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
# dataDictionary["Eastern Africa"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Eastern Asia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Eastern Europe"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Melanesia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Micronesia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Middle Africa"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Northern Africa"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Northern America"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Northern Europe"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Polynesia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["South America"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["South-Eastern Asia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Southern Africa"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Southern Asia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Southern Europe"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Western Africa"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Western Asia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Western Europe"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Africa"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Asia"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Europe"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["LAC"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["N.America"] = copy.deepcopy(dataDictionary["Eastern Africa"])
dataDictionary["Oceania"] = copy.deepcopy(dataDictionary["Eastern Africa"])


def replaceDash(x):
    x = str(x)
    y = re.sub(r"((?:^|[^{])\d+)-(\d+[^}])",u"\\1\u2013\\2", x)
    return y
missingVals = [" ",".","","Insufficient data to make assessment"]
def safeFormat(x,commas=False,precision=0):
    if pd.isnull(x):
        return "NA"
    elif x in missingVals:
        return "NA"
    else:
        try:
            x = float(x)
        except:
            x = x
        if not isinstance(x,numbers.Number):
            return replaceDash(x)
        if precision == 0:
            x = int(x)
        else:
            x = round(x,precision)
        if commas:
            return format(x,",")
        else:
            return x
        
def safeFloat(x):
    try:
        return float(x)
    except:
        return 0


dat = pd.read_csv("agg_long.csv")
agg_key = {
    "Global":"NUTRITION GLOBAL PROFILE"
    ,"Australia and New Zealand":"NUTRITION SUBREGIONAL PROFILE"
    ,"Caribbean":"NUTRITION SUBREGIONAL PROFILE"
    ,"Central America":"NUTRITION SUBREGIONAL PROFILE"
    ,"Central Asia":"NUTRITION SUBREGIONAL PROFILE"
    ,"Eastern Africa":"NUTRITION SUBREGIONAL PROFILE"
    ,"Eastern Asia":"NUTRITION SUBREGIONAL PROFILE"
    ,"Eastern Europe":"NUTRITION SUBREGIONAL PROFILE"
    ,"Melanesia":"NUTRITION SUBREGIONAL PROFILE"
    ,"Micronesia":"NUTRITION SUBREGIONAL PROFILE"
    ,"Middle Africa":"NUTRITION SUBREGIONAL PROFILE"
    ,"Northern Africa":"NUTRITION SUBREGIONAL PROFILE"
    ,"Northern America":"NUTRITION SUBREGIONAL PROFILE"
    ,"Northern Europe":"NUTRITION SUBREGIONAL PROFILE"
    ,"Polynesia":"NUTRITION SUBREGIONAL PROFILE"
    ,"South America":"NUTRITION SUBREGIONAL PROFILE"
    ,"South-Eastern Asia":"NUTRITION SUBREGIONAL PROFILE"
    ,"Southern Africa":"NUTRITION SUBREGIONAL PROFILE"
    ,"Southern Asia":"NUTRITION SUBREGIONAL PROFILE"
    ,"Southern Europe":"NUTRITION SUBREGIONAL PROFILE"
    ,"Western Africa":"NUTRITION SUBREGIONAL PROFILE"
    ,"Western Asia":"NUTRITION SUBREGIONAL PROFILE"
    ,"Western Europe":"NUTRITION SUBREGIONAL PROFILE"
    ,"Africa":"NUTRITION REGIONAL PROFILE"
    ,"Asia":"NUTRITION REGIONAL PROFILE"
    ,"Europe":"NUTRITION REGIONAL PROFILE"
    ,"LAC":"NUTRITION REGIONAL PROFILE"
    ,"N.America":"NUTRITION REGIONAL PROFILE"
    ,"Oceania":"NUTRITION REGIONAL PROFILE"
}
for region in dataDictionary.keys():
    regionDat = dat.loc[(dat.region==region)]
    dataDictionary[region]["region"] = region
    dataDictionary[region]["regionBool"] = agg_key[region]
    # dataDictionary["Eastern Africa"]["table1"] = [
    #     [Paragraph("GDP per capita (PPP$) (n=14)",style=greyParaStyle),"2,002",safeFormat(2016)]
    #     ,["$1.90/day (%) (n=13)","49",safeFormat("2000-2013")]
    #     ,["$3.10/day (%) (n=13)","75",safeFormat("2000-2013")]
    #     ]
    row1 = regionDat.loc[(regionDat.indicator=="GDP per capita (PPP)")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Poverty rates $1.90")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Poverty rates $3.10")].iloc[0]
    dataDictionary[region]["table1"] = [
        [Paragraph("GDP per capita (PPP$) (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),safeFormat(row1["value"],True),safeFormat(row1["year"])]
        ,["$1.90/day (%) (n={})".format(safeFormat(row2["n"])),safeFormat(row2["value"]),safeFormat(row2["year"])]
        ,["$3.10/day (%) (n={})".format(safeFormat(row3["n"])),safeFormat(row3["value"]),safeFormat(row3["year"])]
        ]
    # dataDictionary["Eastern Africa"]["table2"] = [
    #     [Paragraph("Under five mortality rate (deaths per 1,000 live births) (n=18)",style=greyParaStyle),"61",safeFormat(2015)]
    #     ]
    row1 = regionDat.loc[(regionDat.indicator=="Under-5 mortality rate (deaths per 1000 live births)")].iloc[0]
    dataDictionary[region]["table2"] = [
        [Paragraph("Under five mortality rate (deaths per 1,000 live births) (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),safeFormat(row1["value"]),safeFormat(row1["year"])]
        ]
    # dataDictionary["Eastern Africa"]["table3"] = [
    #     ["Population (thousands) (n=18)","420,907",safeFormat(2017)]
    #     ,[Paragraph("Under-5 population (thousands) (n=18)",style=greyParaStyle),"66,906",safeFormat(2017)]
    #     ,["Urban (%) (n=18)","27",safeFormat(2017)]
    #     ,[">65 years (%) (n=18)","3",safeFormat(2017)]
    #     ]
    row1 = regionDat.loc[(regionDat.indicator=="Total population (000)")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Under-5 population (000)")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Urban population (%)")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator==">65 population (%)")].iloc[0]
    dataDictionary[region]["table3"] = [
        ["Population (thousands) (n={})".format(safeFormat(row1["n"])),safeFormat(row1["value"],True),safeFormat(row1["year"])]
        ,[Paragraph("Under-5 population (thousands) (n={})".format(safeFormat(row2["n"])),style=greyParaStyle),safeFormat(row2["value"],True),safeFormat(row2["year"])]
        ,["Urban (%) (n={})".format(safeFormat(row3["n"])),safeFormat(row3["value"]),safeFormat(row3["year"])]
        ,[">65 years (%) (n={})".format(safeFormat(row4["n"])),safeFormat(row4["value"]),safeFormat(row4["year"])]
        ]
    # dataDictionary["Eastern Africa"]["table4"] = [
    #     ["Number of children under 5 affected (millions)","",""]
    #     ,[Paragraph("Stunting<super>1</super> (n=17)",style=greyParaStyle),"24",safeFormat(2016)]
    #     ,[Paragraph("Wasting<super>1</super> (n=17)",style=greyParaStyle),"4",safeFormat(2016)]
    #     ,[Paragraph("Overweight<super>1</super> (n=17)",style=greyParaStyle),"3",safeFormat(2016)]
    #     ]
    row1 = regionDat.loc[(regionDat.indicator=="Stunting (millions)")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Wasting (millions)")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Overweight (millions)")].iloc[0]
    dataDictionary[region]["table4"] = [
        ["Number of children under 5 affected (millions)","",""]
        ,[Paragraph("Stunting<super>1</super> (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),safeFormat(row1["value"],True),safeFormat(row1["year"])]
        ,[Paragraph("Wasting<super>1</super> (n={})".format(safeFormat(row2["n"])),style=greyParaStyle),safeFormat(row2["value"],True),safeFormat(row2["year"])]
        ,[Paragraph("Overweight<super>1</super> (n={})".format(safeFormat(row3["n"])),style=greyParaStyle),safeFormat(row3["value"],True),safeFormat(row3["year"])]
        ]
    # dataDictionary["Eastern Africa"]["table5"] = [
    #     ["Percentage of children under 5 affected","",""]
    #     ,[Paragraph("Stunting<super>1</super> (n=17)",style=greyParaStyle),"36",safeFormat(2016)]
    #     ,[Paragraph("Wasting<super>1</super> (n=17)",style=greyParaStyle),"7",safeFormat(2016)]
    #     ,[Paragraph("Severe Wasting<super>1</super> (n=16)",style=greyParaStyle),"2",safeFormat(2016)]
    #     ,[Paragraph("Overweight<super>1</super> (n=17)",style=greyParaStyle),"5",safeFormat(2016)]
    #     ,[Paragraph("Low birth weight<super>2</super> (n=15)",style=greyParaStyle),"14",safeFormat("2000-2012")]
    #     ]
    row1 = regionDat.loc[(regionDat.indicator=="Stunting (%)")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Wasting (%)")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Severe wasting (%)")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Overweight (%)")].iloc[0]
    row5 = regionDat.loc[(regionDat.indicator=="Low birth weight (%)")].iloc[0]
    dataDictionary[region]["table5"] = [
        ["Percentage of children under 5 affected","",""]
        ,[Paragraph("Stunting<super>1</super> (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),safeFormat(row1["value"]),safeFormat(row1["year"])]
        ,[Paragraph("Wasting<super>1</super> (n={})".format(safeFormat(row2["n"])),style=greyParaStyle),safeFormat(row2["value"]),safeFormat(row2["year"])]
        ,[Paragraph("Severe Wasting<super>1</super> (n={})".format(safeFormat(row3["n"])),style=greyParaStyle),safeFormat(row3["value"]),safeFormat(row3["year"])]
        ,[Paragraph("Overweight<super>1</super> (n={})".format(safeFormat(row4["n"])),style=greyParaStyle),safeFormat(row4["value"]),safeFormat(row4["year"])]
        ,[Paragraph("Low birth weight<super>2</super> (n={})".format(safeFormat(row5["n"])),style=greyParaStyle),safeFormat(row5["value"]),safeFormat(row5["year"])]
        ]
    # dataDictionary["Eastern Africa"]["table6"] = [
    #     [Paragraph("Adolescent overweight<super>1</super> (n=0)",style=greyParaStyle),"NA",safeFormat("NA")]
    #     ,[Paragraph("Adolescent obesity<super>1</super> (n=0)",style=greyParaStyle),"NA",safeFormat("2003-2014")]
    #     ,[Paragraph("Women of reproductive age, thinness<super>2</super> (n=13)",style=greyParaStyle),16,safeFormat("1994-2015")]
    #     ,[Paragraph("Women of reproductive age, short stature<super>2</super> (n=13)",style=greyParaStyle),3,safeFormat("1994-2015")]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="Adolescent overweight (%)")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Adolescent obesity (%)")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Women of reproductive age, thinness (%)")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Women of reproductive age, short stature (%)")].iloc[0]
    dataDictionary[region]["table6"] = [
        [Paragraph("Adolescent overweight<super>1</super> (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),safeFormat(row1["value"]),safeFormat(row1["year"])]
        ,[Paragraph("Adolescent obesity<super>1</super> (n={})".format(safeFormat(row2["n"])),style=greyParaStyle),safeFormat(row2["value"]),safeFormat(row2["year"])]
        ,[Paragraph("Women of reproductive age, thinness<super>2</super> (n={})".format(safeFormat(row3["n"])),style=greyParaStyle),safeFormat(row3["value"]),safeFormat(row3["year"])]
        ,[Paragraph("Women of reproductive age, short stature<super>2</super> (n={})".format(safeFormat(row4["n"])),style=greyParaStyle),safeFormat(row4["value"]),safeFormat(row4["year"])]
    ]
    # dataDictionary["Eastern Africa"]["table7"] = [
    #     [Paragraph("Women of reproductive age with anaemia<super>1</super> (n=18)",style=greyParaStyle),"",""]
    #     ,["Total population affected (thousands)",format(30054,",d"),safeFormat(2016)]
    #     ,["Total population affected (%)",format(48,"d"),safeFormat(2016)]
    #     ,[Paragraph(u"Vitamin A deficiency in children 6\u201359 months old (%)<super>2</super> (n=17)",style=greyParaStyle),format(48,"d"),safeFormat(2013)]
    #     ,[Paragraph(u"Population classification of iodine nutrition (age group 5\u201319 years)<super>3</super> (n=10)",style=greyParaStyle),Paragraph("Mild iodine deficiency",style=greyParaStyle),safeFormat("1996-2001")]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="Women of reproductive age with anaemia (000)")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Women of reproductive age with anaemia (000)")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Women of reproductive age with anaemia (%)")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Vitamin A deficiency in children 6-59 months (%)")].iloc[0]
    row5 = regionDat.loc[(regionDat.indicator=="Population classification of iodine status")].iloc[0]
    dataDictionary[region]["table7"] = [
        [Paragraph("Women of reproductive age with anaemia<super>1</super> (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),"",""]
        ,["Total population affected (thousands)",safeFormat(row2["value"],True),safeFormat(row2["year"])]
        ,["Total population affected (%)",safeFormat(row3["value"]),safeFormat(row3["year"])]
        ,[Paragraph(u"Vitamin A deficiency in children 6\u201359 months old (%)<super>2</super> (n={})".format(safeFormat(row4["n"])),style=greyParaStyle),safeFormat(row4["value"]),safeFormat(row4["year"])]
        ,[Paragraph(u"Population classification of iodine nutrition (age group 5\u201319 years)<super>3</super> (n={})".format(safeFormat(row5["n"])),style=greyParaStyle),Paragraph(safeFormat(row5["value"]),style=greyParaStyle),safeFormat(row5["year"])]
    ]
    # dataDictionary["Eastern Africa"]["table8"] = [
    #     [
    #         Paragraph("<b>Under-5 stunting</b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Under-5 wasting</b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Under-5 overweight</b>",style=whiteParaStyle)
    #         ,Paragraph("<b>WRA Anaemia</b>",style=whiteParaStyle)
    #         ,Paragraph("<b>EBF</b>",style=whiteParaStyle)
    #      ]
    #     ,["1/18 on course","5/18 on course","4/18 on course","0/18 on course","4/18 on course"]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="Stunting.No progress or worsening")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Stunting.On course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Stunting.Some progress")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Stunting.Insufficient data to make assessment")].iloc[0]
    
    stuntingN = safeFloat(row2["value"])
    stuntingD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])+safeFloat(row4["value"])
    
    row1 = regionDat.loc[(regionDat.indicator=="Wasting.No progress or worsening")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Wasting.On course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Wasting.Some progress")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Wasting.Insufficient data to make assessment")].iloc[0]
    
    wastingN = safeFloat(row2["value"])
    wastingD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])+safeFloat(row4["value"])
    
    row1 = regionDat.loc[(regionDat.indicator=="Overweight.Off course")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Overweight.On course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Overweight.Insufficient data to make assessment")].iloc[0]
    
    overweightN = safeFloat(row2["value"])
    overweightD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])
    
    row1 = regionDat.loc[(regionDat.indicator=="Anaemia.No progress or worsening")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Anaemia.On course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Anaemia.Some progress")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Anaemia.Insufficient data to make assessment")].iloc[0]
    
    anaemiaN = safeFloat(row2["value"])
    anaemiaD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])+safeFloat(row4["value"])
    
    row1 = regionDat.loc[(regionDat.indicator=="EBF.No progress or worsening")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="EBF.On course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="EBF.Some progress")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="EBF.Insufficient data to make assessment")].iloc[0]
    
    ebfN = safeFloat(row2["value"])
    ebfD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])+safeFloat(row4["value"])
    
    dataDictionary[region]["table8"] = [
        [
            Paragraph("<b>Under-5 stunting</b>",style=whiteParaStyle)
            ,Paragraph("<b>Under-5 wasting</b>",style=whiteParaStyle)
            ,Paragraph("<b>Under-5 overweight</b>",style=whiteParaStyle)
            ,Paragraph("<b>WRA Anaemia</b>",style=whiteParaStyle)
            ,Paragraph("<b>EBF</b>",style=whiteParaStyle)
         ]
        ,[
            "{}/{} on course".format(safeFormat(stuntingN),safeFormat(stuntingD))
            ,"{}/{} on course".format(safeFormat(wastingN),safeFormat(wastingD))
            ,"{}/{} on course".format(safeFormat(overweightN),safeFormat(overweightD))
            ,"{}/{} on course".format(safeFormat(anaemiaN),safeFormat(anaemiaD))
            ,"{}/{} on course".format(safeFormat(ebfN),safeFormat(ebfD))
        ]
    ]
    # dataDictionary["Eastern Africa"]["table8a"] = [
    #     [
    #         Paragraph("<b>Adult female obesity</b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Adult male obesity</b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Adult female diabetes</b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Adult male diabetes</b>",style=whiteParaStyle)
    #      ]
    #     ,["0/18 on course","0/18 on course","0/18 on course","0/18 on course"]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="Adult obesity, female.Insufficient data to make assessment")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Adult obesity, female.Off course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Adult obesity, female.On course")].iloc[0]
    
    aofN = safeFloat(row3["value"])
    aofD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])
    
    row1 = regionDat.loc[(regionDat.indicator=="Adult obesity, male.Insufficient data to make assessment")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Adult obesity, male.Off course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Adult obesity, male.On course")].iloc[0]
    
    aomN = safeFloat(row3["value"])
    aomD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])
    
    row1 = regionDat.loc[(regionDat.indicator=="Adult diabetes, female.Insufficient data to make assessment")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Adult diabetes, female.Off course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Adult diabetes, female.On course")].iloc[0]
    
    adfN = safeFloat(row3["value"])
    adfD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])
    
    row1 = regionDat.loc[(regionDat.indicator=="Adult diabetes male.Insufficient data to make assessment")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Adult diabetes male.Off course")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Adult diabetes male.On course")].iloc[0]
    
    admN = safeFloat(row3["value"])
    admD = safeFloat(row1["value"])+safeFloat(row2["value"])+safeFloat(row3["value"])
    
    dataDictionary[region]["table8a"] = [
        [
            Paragraph("<b>Adult female obesity</b>",style=whiteParaStyle)
            ,Paragraph("<b>Adult male obesity</b>",style=whiteParaStyle)
            ,Paragraph("<b>Adult female diabetes</b>",style=whiteParaStyle)
            ,Paragraph("<b>Adult male diabetes</b>",style=whiteParaStyle)
         ]
        ,[
            "{}/{} on course".format(safeFormat(aofN),safeFormat(aofD))
            ,"{}/{} on course".format(safeFormat(aomN),safeFormat(aomD))
            ,"{}/{} on course".format(safeFormat(adfN),safeFormat(adfD))
            ,"{}/{} on course".format(safeFormat(admN),safeFormat(admD))
        ]
    ]
    # dataDictionary["Eastern Africa"]["table9"] = [
    #     [Paragraph("Severe acute malnutrition, geographic coverage<super>1</super> (n=12)",style=greyParaStyle),"60",safeFormat("2012")]
    #     ,[Paragraph("Vitamin A supplementation, full coverage<super>2</super> (n=14)",style=greyParaStyle),"66",safeFormat("2014")]
    #     ,[Paragraph("Children under 5 with diarrhoea receiving ORS<super>2</super> (n=16)",style=greyParaStyle),"41",safeFormat("2000-2016")]
    #     ,[Paragraph("Immunization coverage, DTP3<super>3</super> (n=18)",style=greyParaStyle),"81",safeFormat("2016")]
    #     ,[Paragraph("Iodized salt consumption<super>2</super> (n=14)",style=greyParaStyle),"51",safeFormat("2000-2013")]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="Severe acute malnutrition, geographic coverage")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Vitamin A supplementation, full coverage")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Children under 5 with diarrhoea receiving ORS")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Immunization coverage (DTP3)")].iloc[0]
    row5 = regionDat.loc[(regionDat.indicator=="Iodized salt consumption")].iloc[0]
    dataDictionary[region]["table9"] = [
        [Paragraph("Severe acute malnutrition, geographic coverage<super>1</super> (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),safeFormat(row1["value"]),safeFormat(row1["year"])]
        ,[Paragraph("Vitamin A supplementation, full coverage<super>2</super> (n={})".format(safeFormat(row2["n"])),style=greyParaStyle),safeFormat(row2["value"]),safeFormat(row2["year"])]
        ,[Paragraph("Children under 5 with diarrhoea receiving ORS<super>2</super> (n={})".format(safeFormat(row3["n"])),style=greyParaStyle),safeFormat(row3["value"]),safeFormat(row3["year"])]
        ,[Paragraph("Immunization coverage, DTP3<super>3</super> (n={})".format(safeFormat(row4["n"])),style=greyParaStyle),safeFormat(row4["value"]),safeFormat(row4["year"])]
        ,[Paragraph("Iodized salt consumption<super>2</super> (n={})".format(safeFormat(row5["n"])),style=greyParaStyle),safeFormat(row5["value"]),safeFormat(row5["year"])]
    ]
    # dataDictionary["Eastern Africa"]["table10"] = [
    #     [Paragraph(u"Exclusive breastfeeding 0\u20135 months (n=11)",style=greyParaStyle),"9",safeFormat("2010-2016")]
    #     ,[Paragraph(u"Minimum acceptable diet 6\u201323 months (n=11)",style=greyParaStyle),"9",safeFormat("2010-2016")]
    #     ,[Paragraph(u"Minimum dietary diversity 6\u201323 months (n=11)",style=greyParaStyle),"20",safeFormat("2010-2016")]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="Exclusive breastfeeding 0-5 months (%)")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Minimum acceptable diet 6-23 months (%)")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Minimum dietary diversity 6-23 months (%)")].iloc[0]
    dataDictionary[region]["table10"] = [
        [Paragraph(u"Exclusive breastfeeding 0\u20135 months (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),safeFormat(row1["value"]),safeFormat(row1["year"])]
        ,[Paragraph(u"Minimum acceptable diet 6\u201323 months (n={})".format(safeFormat(row2["n"])),style=greyParaStyle),safeFormat(row2["value"]),safeFormat(row2["year"])]
        ,[Paragraph(u"Minimum dietary diversity 6\u201323 months (n={})".format(safeFormat(row3["n"])),style=greyParaStyle),safeFormat(row3["value"]),safeFormat(row3["year"])]
    ]
    # dataDictionary["Eastern Africa"]["table11"] = [
    #     [Paragraph("Early childbearing: births by age 18 (%)<super>1</super> (n=14)",style=greyParaStyle),"33",safeFormat("2011")]
    #     ,[Paragraph("Gender Inequality Index (score*)<super>2</super> (n=11)",style=greyParaStyle),"0.53",safeFormat("2015")]
    #     ,[Paragraph("Female secondary education enrolment rate (%)<super>3</super> (n=12)",style=greyParaStyle),"31",safeFormat("2006-2012")]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="Early childbearing: births by age 18 (%)")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Gender Inequality Index (score)")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Female secondary education enrolment rate")].iloc[0]
    dataDictionary[region]["table11"] = [
        [Paragraph("Early childbearing: births by age 18 (%)<super>1</super> (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),safeFormat(row1["value"]),safeFormat(row1["year"])]
        ,[Paragraph("Gender Inequality Index (score*)<super>2</super> (n={})".format(safeFormat(row2["n"])),style=greyParaStyle),safeFormat(row2["value"],False,2),safeFormat(row2["year"])]
        ,[Paragraph("Female secondary education enrolment rate (%)<super>3</super> (n={})".format(safeFormat(row3["n"])),style=greyParaStyle),safeFormat(row3["value"]),safeFormat(row3["year"])]
    ]
    # dataDictionary["Eastern Africa"]["table12"] = [
    #     ["Physicians (n=17)","0.08",safeFormat("2001-2005")]
    #     ,["Nurses and midwives (n=17)","0.56",safeFormat("1994-2015")]
    #     ,["Community health workers (n=10)","0.38",safeFormat("1992-2013")]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="Physicians")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Nurses and midwives")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Community health workers")].iloc[0]
    dataDictionary[region]["table12"] = [
        ["Physicians (n={})".format(safeFormat(row1["n"])),safeFormat(row1["value"],False,2),safeFormat(row1["year"])]
        ,["Nurses and midwives (n={})".format(safeFormat(row2["n"])),safeFormat(row2["value"],False,2),safeFormat(row2["year"])]
        ,["Community health workers (n={})".format(safeFormat(row3["n"])),safeFormat(row3["value"],False,2),safeFormat(row3["year"])]
    ]
    # dataDictionary["Eastern Africa"]["table13"] = [
    #     [Paragraph("National implementation of the International Code of Marketing of Breast-milk Substitutes<super>1</super> (n=18)",style=greyParaStyle),Paragraph("Full provisions in law",style=greyCenterParaStyle),safeFormat("2016")]
    #     ,[Paragraph("Extent of constitutional right to food<super>2</super> (n=12)",style=greyParaStyle),Paragraph("Medium",style=greyCenterParaStyle),safeFormat("2003")]
    #     ,[Paragraph("Maternity Protection Convention 183<super>3</super> (n=17)",style=greyParaStyle),Paragraph("Partial",style=greyCenterParaStyle),safeFormat("2011")]
    #     ,[Paragraph("Wheat fortification legislation<super>4</super> (n=17)",style=greyParaStyle),Paragraph("Mandatory",style=greyCenterParaStyle),safeFormat("2015")]
    # ]
    row1 = regionDat.loc[(regionDat.indicator=="National Implementation of the International Code of Marketing of Breast Milk Substitutes")].iloc[0]
    row2 = regionDat.loc[(regionDat.indicator=="Extent of Constitutional Right to Food")].iloc[0]
    row3 = regionDat.loc[(regionDat.indicator=="Maternity protection (Convention 183)")].iloc[0]
    row4 = regionDat.loc[(regionDat.indicator=="Wheat fortification legislation ")].iloc[0]
    dataDictionary[region]["table13"] = [
        [Paragraph("National implementation of the International Code of Marketing of Breast-milk Substitutes<super>1</super> (n={})".format(safeFormat(row1["n"])),style=greyParaStyle),Paragraph(safeFormat(row1["value"]),style=greyCenterParaStyle),safeFormat(row1["year"])]
        ,[Paragraph("Extent of constitutional right to food<super>2</super> (n={})".format(safeFormat(row2["n"])),style=greyParaStyle),Paragraph(safeFormat(row2["value"]),style=greyCenterParaStyle),safeFormat(row2["year"])]
        ,[Paragraph("Maternity Protection Convention 183<super>3</super> (n={})".format(safeFormat(row3["n"])),style=greyParaStyle),Paragraph(safeFormat(row3["value"]),style=greyCenterParaStyle),safeFormat(row3["year"])]
        ,[Paragraph("Wheat fortification legislation<super>4</super> (n={})".format(safeFormat(row4["n"])),style=greyParaStyle),Paragraph(safeFormat(row4["value"]),style=greyCenterParaStyle),safeFormat(row4["year"])]
    ]
    # dataDictionary["Eastern Africa"]["table14"] = [
    #     [Paragraph("All major NCDs (n=15)",style=greyParaStyle),Paragraph("No/Yes",style=greyParaStyle),safeFormat("2015")]
    # ]
    row = regionDat.loc[(regionDat.indicator=="Existence of evidence-based national guidelines/protocols/standards for the management of major NCDs through a primary care approach")].iloc[0]
    dataDictionary[region]["table14"] = [
        [Paragraph("All major NCDs (n={})".format(safeFormat(row["n"])),style=greyParaStyle),Paragraph("No/Yes",style=greyParaStyle),safeFormat(row["year"])]
    ]
    # 
    # dataDictionary["Eastern Africa"]["c5note"] = "<i>Note: n=17. Data are population-weighted means.</i>"
    # dataDictionary["Eastern Africa"]["c6note"] = "<i>Note: BMI: body mass index. n=17. Data are population-weighted means.</i>"
    # dataDictionary["Eastern Africa"]["c9note"] = "<i>Note: n is between 7 and 12 depending on the indicator and year.</i>"
    # dataDictionary["Eastern Africa"]["c11note"] = "<i>n is between 16 and 18 depending on the indicator and year.</i>"
    # dataDictionary["Eastern Africa"]["c12note"] = "<i>n is between 16 and 18 depending on the indicator and year.</i>"
    # dataDictionary["Eastern Africa"]["c13note"] = "<i>Note: n is between 9 and 10 depending on the indicator and year.</i>"
    # dataDictionary["Eastern Africa"]["c14note"] = "<i>12 SUN member countries.</i>"
    # dataDictionary["Eastern Africa"]["progressYear"] = "PROGRESS AGAINST GLOBAL NUTRITION TARGETS 2017"
    row = regionDat.loc[(regionDat.indicator=="Raised blood pressure, Male (%)")].iloc[0]
    dataDictionary[region]["c5note"] = "<i>Note: n={}. Data are population-weighted means.</i>".format(safeFormat(row["n"]))
    
    row = regionDat.loc[(regionDat.indicator=="Adult overweight and obesity, Male (%)")].iloc[0]
    dataDictionary[region]["c6note"] = "<i>Note: BMI: body mass index. n={}. Data are population-weighted means.</i>".format(safeFormat(row["n"]))
    
    c9indicators = [
        "Undernourishment (%)"
        ,"Fruit and veg (grams)"
        ,"Non-staples (%)" 
    ]
    rows = regionDat[regionDat['indicator'].isin(c9indicators)]
    validRows = rows[pd.notnull(rows['value'])]
    if len(validRows)>0:
        nmin = safeFormat(min(validRows["n"]))
        nmax = safeFormat(max(validRows["n"]))
        if nmin!=nmax:
            dataDictionary[region]["c9note"] = "<i>Note: n is between {} and {} depending on the indicator and year.</i>".format(nmin,nmax)
        else:
            dataDictionary[region]["c9note"] = "<i>Note: n={}.</i>".format(nmin)
    else:
         dataDictionary[region]["c9note"] = ""
    
    c11indicators = [
        "Drinking.Basic"
        ,"Drinking.Limited"
        ,"Drinking.Surface water"
        ,"Drinking.Unimproved"
        ,"Drinking.Safely managed"
    ]
    rows = regionDat[regionDat['indicator'].isin(c11indicators)]
    validRows = rows[pd.notnull(rows['value'])]
    if len(validRows)>0:
        nmin = safeFormat(min(validRows["n"]))
        nmax = safeFormat(max(validRows["n"]))
        if nmin!=nmax:
            dataDictionary[region]["c11note"] = "<i>n is between {} and {} depending on the indicator and year.</i>".format(nmin,nmax)
        else:
            dataDictionary[region]["c11note"] = "<i>n={}.</i>".format(nmin)
    else:
         dataDictionary[region]["c11note"] = ""
        
    
    c12indicators = [
        "Sanitation.Basic"
        ,"Sanitation.Limited"
        ,"Sanitation.Safely managed"
        ,"Sanitation.Unimproved"
        ,"Sanitation.Open defecation"
    ]
    rows = regionDat[regionDat['indicator'].isin(c9indicators)]
    validRows = rows[pd.notnull(rows['value'])]
    if len(validRows)>0:
        nmin = safeFormat(min(validRows["n"]))
        nmax = safeFormat(max(validRows["n"]))
        if nmin!=nmax:
            dataDictionary[region]["c12note"] = "<i>n is between {} and {} depending on the indicator and year.</i>".format(nmin,nmax)
        else:
            dataDictionary[region]["c12note"] = "<i>n={}.</i>".format(nmin)
    else:
         dataDictionary[region]["c12note"] = ""
    
    c13indicators = [
        "Agriculture"
        ,"Education"
        ,"Health"
        ,"Social protection"
    ]
    rows = regionDat[regionDat['indicator'].isin(c9indicators)]
    validRows = rows[pd.notnull(rows['value'])]
    if len(validRows)>0:
        nmin = safeFormat(min(validRows["n"]))
        nmax = safeFormat(max(validRows["n"]))
        if nmin!=nmax:
            dataDictionary[region]["c13note"] = "<i>Note: n is between {} and {} depending on the indicator and year.</i>".format(nmin,nmax)
        else:
            dataDictionary[region]["c13note"] = "<i>Note: n={}.</i>".format(nmin)
    else:
         dataDictionary[region]["c13note"] = ""
    
    row = regionDat.loc[(regionDat.indicator=="Bringing people into a shared space for action (%)")].iloc[0]
    dataDictionary[region]["c14note"] = "<i>{} SUN member countries.</i>".format(safeFormat(row["n"]))
    
    if region=='Global':
        progressYear = 2014
    else:
        progressYear = 2017
    dataDictionary[region]["progressYear"] = "PROGRESS AGAINST GLOBAL NUTRITION TARGETS {}".format(safeFormat(progressYear))


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
tableStyles["table5"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('BACKGROUND',(0,3),(-1,3),"#fef5e7")
    ,('BACKGROUND',(0,5),(-1,5),"#fef5e7")
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
    ,('BACKGROUND',(0,3),(-1,3),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table14"] = tableStyles["table10"]