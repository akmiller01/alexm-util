from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_CENTER

style = getSampleStyleSheet()
#Normally this would be pulled in programmatically, but I'm placing it here for testing
dataDictionary = {}
dataDictionary["table1"] = [["Gini index score*","Gini index rank**","Year"],[45,108,2012]]
dataDictionary["table2"] = [
    ["Population (000)",format(40141,",d"),2015]
    ,["Under-5 population (000)",format(7470,",d"),2015]
    ,["Urban (%)",format(17,",d"),2015]
    ,[">65 years (%)",format(5,",d"),2015]
    ]
dataDictionary["table3"] = [
    ["Number of children under 5 affected (000)","",""]
    ,[Paragraph("Stunting<super>a</super>",style=style["BodyText"]),format(2373,",d"),2012]
    ,[Paragraph("Wasting<super>a</super>",style=style["BodyText"]),format(298,",d"),2012]
    ,[Paragraph("Overweight<super>a</super>",style=style["BodyText"]),format(402,",d"),2012]
    ,["Percentage of children under 5 affected","",""]
    ,[Paragraph("Wasting<super>a</super>",style=style["BodyText"]),format(4,"d"),2012]
    ,[Paragraph("Severe wasting<super>a</super>",style=style["BodyText"]),format(0,"d"),2012]
    ,[Paragraph("Overweight<super>a</super>",style=style["BodyText"]),format(6,"d"),2012]
    ,[Paragraph("Low birth weight<super>b</super>",style=style["BodyText"]),format(12,"d"),2011]
    ]
dataDictionary["table4"] = [
    [Paragraph("Adolescent overweight<super>a</super>",style=style["BodyText"]),format(7,"d"),2003]
    ,[Paragraph("Adolescent obesity<super>a</super>",style=style["BodyText"]),format(1,"d"),2003]
    ,[Paragraph("Women of reproductive age, thinness<super>b</super>",style=style["BodyText"]),format(10,"d"),2011]
    ,[Paragraph("Women of reproductive age, short stature<super>b</super>",style=style["BodyText"]),format(1,"d"),2011]
]
dataDictionary["table5"] = [
    [Paragraph("<b>Women of reproductive age with anemia<super>a</super></b>",style=style["BodyText"]),"",""]
    ,["Total population affected (000)",format(2022,",d"),2011]
    ,["Total population affected (%)",format(27,"d"),2011]
    ,[Paragraph("Vitamin A deficiency in children 6-59 months old (%)<super>b</super>",style=style["BodyText"]),format(39,"d"),2013]
    ,[Paragraph("Population classification of iodone nutrition (age group 6-12)<super>c</super>",style=style["BodyText"]),Paragraph("Risk of adverse health consequences (iodone-induced hyperthyroidism, auto-immune thyroid diseases)",style=style["BodyText"]),1999]
]
table6ParaStyle = ParagraphStyle('table6ParaStyle',parent=style['BodyText'],textColor="white",alignment=TA_CENTER)
dataDictionary["table6"] = [
    [
        Paragraph("<b>Under-5 stunting, 2012<super>a</super></b>",style=table6ParaStyle)
        ,Paragraph("<b>Under-5 wasting, 2012<super>b</super></b>",style=table6ParaStyle)
        ,Paragraph("<b>Under-5 overweight, 2012<super>a</super></b>",style=table6ParaStyle)
        ,Paragraph("<b>WRA Anemia, 2011<super>b</super></b>",style=table6ParaStyle)
        ,Paragraph("<b>EBF, 2011<super>a</super></b>",style=table6ParaStyle)
     ]
    ,["Off course, some progress","On course","On course, good progress","Off course","On course"]
]
tableStyles = {}
tableStyles["table1"] = [
    ('TEXTCOLOR',(0,0),(-1,-1),"white")
    ,('BACKGROUND',(0,0),(2,0),"#6dc163")
    ,('FONTNAME',(0,0),(2,0),"Arial-Bold")
    ,('FONTNAME',(0,1),(2,1),"Arial")
    ,('BACKGROUND',(0,1),(2,1),"#f79c2a")
    ,('GRID',(0,0),(-1,-1),1,"white")
    ,('ALIGN',(0,0),(-1,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ]
tableStyles["table2"] = [
    ('BACKGROUND',(0,0),(-1,0),"#fee8ce")
    ,('BACKGROUND',(0,2),(-1,2),"#fee8ce")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ]
tableStyles["table3"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fee8ce")
    ,('BACKGROUND',(0,3),(-1,3),"#fee8ce")
    ,('BACKGROUND',(0,5),(-1,5),"#fee8ce")
    ,('BACKGROUND',(0,7),(-1,7),"#fee8ce")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,1),(1,3),1,"#f79c2a")
    ,('BOX',(1,5),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEABOVE',(0,1),(-1,1),1,"#f79c2a")
    ,('LINEABOVE',(0,4),(-1,4),1,"#f79c2a")
    ,('LINEABOVE',(0,5),(-1,5),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('SPAN',(0,0),(-1,0))
    ,('FONTNAME',(0,0),(-1,0),"Arial-Bold")
    ,('SPAN',(0,4),(-1,4))
    ,('FONTNAME',(0,4),(-1,4),"Arial-Bold")
    ]
tableStyles["table4"] = tableStyles["table2"]
tableStyles["table5"] = [
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
]
tableStyles["table6"] = [
    ('TEXTCOLOR',(0,0),(-1,0),"white")
    ,('BACKGROUND',(0,0),(-1,0),"#204d5e")
    ,('BACKGROUND',(0,1),(-1,1),"white")
    ,('GRID',(0,0),(-1,-1),1,"#386170")
    ,('ALIGN',(0,0),(-1,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ]