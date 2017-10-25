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

#Read a CSV to make this data?
dataDictionary = {"Eastern Africa":{}}
dataDictionary["Eastern Africa"]["country"] = "Eastern Africa"
dataDictionary["Eastern Africa"]["table1"] = [
    [Paragraph("Gross Domestic Product (GDP) per capita (PPP) (n = 14)",style=greyParaStyle),"2,002",2016]
    ,["$1.90/day (%) (n = 13)","49","2000-2013"]
    ,["$3.10/day (%) (n = 13)","75","2000-2013"]
    ]
dataDictionary["Eastern Africa"]["table2"] = [
    [Paragraph("Under five mortality rate (deaths per 1000 live births) (n = 18)",style=greyParaStyle),"61",2015]
    ]
dataDictionary["Eastern Africa"]["table3"] = [
    ["Population (thousands) (n = 18)","420,907",2017]
    ,[Paragraph("Under-5 population (thousands) (n = 18)",style=greyParaStyle),"66,906",2017]
    ,["Urban (%) (n = 18)","27",2017]
    ,[">65 years (%) (n = 18)","3",2017]
    ]
dataDictionary["Eastern Africa"]["table4"] = [
    ["Number of children under 5 affected (millions)","",""]
    ,[Paragraph("Stunting<super>1</super> (n = 17)",style=greyParaStyle),"24",2016]
    ,[Paragraph("Wasting<super>1</super> (n = 17)",style=greyParaStyle),"4",2016]
    ,[Paragraph("Overweight<super>1</super> (n = 17)",style=greyParaStyle),"3",2016]
    ]
dataDictionary["Eastern Africa"]["table5"] = [
    ["Percentage of children under 5 affected","",""]
    ,[Paragraph("Stunting<super>1</super> (n = 17)",style=greyParaStyle),"36",2016]
    ,[Paragraph("Wasting<super>1</super> (n = 17)",style=greyParaStyle),"7",2016]
    ,[Paragraph("Overweight<super>1</super> (n = 17)",style=greyParaStyle),"5",2016]
    ,[Paragraph("Low birth weight<super>2</super> (n = 15)",style=greyParaStyle),"14","2000-2012"]
    ]
dataDictionary["Eastern Africa"]["table6"] = [
    [Paragraph("Adolescent overweight<super>1</super> (n = 0)",style=greyParaStyle),"NA","NA"]
    ,[Paragraph("Adolescent obesity<super>1</super> (n = 0)",style=greyParaStyle),"NA","2003-2014"]
    ,[Paragraph("Women of reproductive age, thinness<super>2</super> (n = 13)",style=greyParaStyle),16,"1994-2015"]
    ,[Paragraph("Women of reproductive age, short stature<super>2</super> (n = 13)",style=greyParaStyle),3,"1994-2015"]
]
dataDictionary["Eastern Africa"]["table7"] = [
    [Paragraph("Women of reproductive age with anaemia<super>1</super> (n = 18)",style=greyParaStyle),"",""]
    ,["Total population affected (thousands)",format(30054,",d"),2016]
    ,["Total population affected (%)",format(48,"d"),2016]
    ,[Paragraph(u"Vitamin A deficiency in children 6\u201359 months old (%)<super>2</super> (n = 17)",style=greyParaStyle),format(48,"d"),2013]
    ,[Paragraph(u"Population classification of iodine nutrition (age group 5\u201319 years)<super>3</super> (n = 10)",style=greyParaStyle),Paragraph("Mild iodine deficience",style=greyParaStyle),"1996-2001"]
]
dataDictionary["Eastern Africa"]["table8"] = [
    [
        Paragraph("<b>Under-5 stunting</b>",style=whiteParaStyle)
        ,Paragraph("<b>Under-5 wasting</b>",style=whiteParaStyle)
        ,Paragraph("<b>Under-5 overweight</b>",style=whiteParaStyle)
        ,Paragraph("<b>Anaemia</b>",style=whiteParaStyle)
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
    [Paragraph("Severe acute malnutrition, geographic coverage<super>1</super> (n = 12)",style=greyParaStyle),"60","2012"]
    ,[Paragraph("Vitamin A supplementation, full coverage<super>2</super> (n = 14)",style=greyParaStyle),"66","2014"]
    ,[Paragraph("Children under 5 with diarrhea receiving ORS<super>2</super> (n = 16)",style=greyParaStyle),"41","2000-2016"]
    ,[Paragraph("Immunization coverage, DTP3<super>3</super> (n = 18)",style=greyParaStyle),"81","2016"]
    ,[Paragraph("Iodized salt consumption<super>2</super> (n = 14)",style=greyParaStyle),"51","2000-2013"]
]
dataDictionary["Eastern Africa"]["table10"] = [
    ["Minimum acceptable diet (n = 11)","9","2010-2016"]
    ,["Minimum dietary diversity (n = 11)","20","2010-2016"]
]
dataDictionary["Eastern Africa"]["table11"] = [
    [Paragraph("Early childbearing: births by age 18 (%)<super>1</super> (n = 14)",style=greyParaStyle),"33","2011"]
    ,[Paragraph("Gender Inequality Index (score*)<super>2</super> (n = 11)",style=greyParaStyle),"0.53","2015"]
    ,[Paragraph("Female secondary education enrolment rate (%)<super>3</super> (n = 12)",style=greyParaStyle),"31","2006-2012"]
]
dataDictionary["Eastern Africa"]["table12"] = [
    ["Physicians (n = 17)","0.08","2001-2005"]
    ,["Nurses and midwives (n = 17)","0.56","1994-2015"]
    ,["Community health workers (n = 10)","0.38","1992-2013"]
]
dataDictionary["Eastern Africa"]["table13"] = [
    [Paragraph("National implementation of the International Code of Marketing of Breast-milk Substitutes<super>1</super> (n = 18)",style=greyParaStyle),Paragraph("Full provisions in law",style=greyCenterParaStyle),"2016"]
    ,[Paragraph("Extent of constitutional right to food<super>2</super> (n = 12)",style=greyParaStyle),Paragraph("Medium",style=greyCenterParaStyle),"2003"]
    ,[Paragraph("Maternity Protection Convention 183<super>3</super> (n = 17)",style=greyParaStyle),Paragraph("Partial",style=greyCenterParaStyle),"2011"]
    ,[Paragraph("Wheat fortification legislation<super>4</super> (n = 17)",style=greyParaStyle),Paragraph("Mandatory",style=greyCenterParaStyle),"2015"]
]
dataDictionary["Eastern Africa"]["table14"] = [
    [Paragraph("All major NCDs (n = 15)",style=greyParaStyle),Paragraph("No/Yes",style=greyParaStyle),"2015"]
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