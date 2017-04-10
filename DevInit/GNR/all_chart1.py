import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pdb
import matplotlib
from matplotlib.patches import Rectangle
from matplotlib.lines import Line2D
from matplotlib.font_manager import FontProperties

font = {
    #'family' : 'Helvetica'
        #,'weight' : 'bold'
        'size'   : 16,'stretch':'condensed'}

matplotlib.rc('font', **font)

def autolabel(rects, ax,color="#000000",fontsize=12,fontweight="normal",maximum=None):
    # Get y-axis height to calculate label position from.
    fp = FontProperties(weight=fontweight)
    (y_bottom, y_top) = ax.get_ylim()
    if maximum is None:
        y_height = y_top - y_bottom
    else:
        y_height = maximum
    for rect in rects:
        if isinstance(rect,Rectangle):
            height = rect.get_height()
            
    
            # Fraction of axis height taken up by this rectangle
            p_height = (height / y_height)
    
            # If we can fit the label above the column, do that;
            # otherwise, put it inside the column.
            if p_height > 0.95: # arbitrary; 95% looked good to me.
                label_position = height - (y_height * 0.05)
            else:
                label_position = height + (y_height * 0.01)
    
            if  pd.notnull(height):
                ax.text(rect.get_x() + rect.get_width()/2., label_position,
                    '%.1f' % height,
                    ha='center', va='bottom',color=color,fontsize=fontsize,fontproperties=fp)
        elif isinstance(rect,Line2D):
            heights = rect.get_ydata()
            xs = rect.get_xdata()
            ys = rect.get_ydata()
            lw = rect.get_lw()
            for i in range(0,len(heights)):
                height = heights[i]
                x = xs[i]
                y = ys[i]
                if pd.notnull(height):
                    ax.text(x, y+lw*2,
                        '%s' % format(int(height),",d"),
                        ha='center', va='bottom',color=color,fontsize=fontsize,fontproperties=fp)
allDat = pd.read_csv("GNR2015_pov.csv")
for i in range(1,50):
    df = allDat[i-1:i]
    country = df["country"][i-1]
    
    sets = [
        df[["yearpov125_1","poverty125_1"]]
        ,df[["yearpov125_2","poverty125_2"]]
        ,df[["yearpov125_3","poverty125_3"]]
        ,df[["yearpov125_4","poverty125_4"]]
        ,df[["yearpov2_1","poverty2_1"]]
        ,df[["yearpov2_2","poverty2_2"]]
        ,df[["yearpov2_3","poverty2_3"]]
        ,df[["yearpov2_4","poverty2_4"]]
        ,df[["yearGDP_1","GDP_1"]]
        ,df[["yearGDP_2","GDP_2"]]
        ,df[["yearGDP_3","GDP_3"]]
        ,df[["yearGDP_4","GDP_4"]]
    ]
    sets[0].columns = ["year","US$1.25/day (%)"]
    sets[1].columns = ["year","US$1.25/day (%)"]
    sets[2].columns = ["year","US$1.25/day (%)"]
    sets[3].columns = ["year","US$1.25/day (%)"]
    pov125 = pd.concat([sets[0],sets[1],sets[2],sets[3]])
    sets[4].columns = ["year","US$2/day (%)"]
    sets[5].columns = ["year","US$2/day (%)"]
    sets[6].columns = ["year","US$2/day (%)"]
    sets[7].columns = ["year","US$2/day (%)"]
    pov2 = pd.concat([sets[4],sets[5],sets[6],sets[7]])
    sets[8].columns = ["year","GDP per capita PPP ($)"]
    sets[9].columns = ["year","GDP per capita PPP ($)"]
    sets[10].columns = ["year","GDP per capita PPP ($)"]
    sets[11].columns = ["year","GDP per capita PPP ($)"]
    gdp = pd.concat([sets[8],sets[9],sets[10],sets[11]])
    
    df = pd.merge(pov125,pov2,on="year", how='outer')
    df = pd.merge(df,gdp,on="year", how='outer')
    missing = df.isnull().sum().sum()
    if missing < 10:
        df.year = df.year.astype(int)
        df = df.sort_index(by="year")
        
        # Setting the positions and width for the bars
        pos = list(range(len(df["US$1.25/day (%)"])))
        width = 0.4
        
        # Plotting the bars
        fig, ax = plt.subplots(figsize=(8.8,5))
        
        # Create a bar with pre_score data,
        # in position pos,
        rect1 = plt.bar(pos,
                #using df['pre_score'] data,
                df["US$1.25/day (%)"],
                # of width
                width,
                # with alpha 0.5
                alpha=1,
                # with color
                color='#dce127',
                # with label the first value in first_name
                label=df['year'][0])
        jointMax = max(df["US$1.25/day (%)"].max(), df["US$2/day (%)"].max())*1.5
        autolabel(rect1,ax,maximum=jointMax)
        
        # Create a bar with mid_score data,
        # in position pos + some width buffer,
        rect2 = plt.bar([p + width for p in pos],
                #using df['mid_score'] data,
                df["US$2/day (%)"],
                # of width
                width,
                # with alpha 0.5
                alpha=1,
                # with color
                color='#f8991d',
                # with label the second value in first_name
                label=df['year'][1])
        
        autolabel(rect2,ax,maximum=jointMax)
        
        # Setting the x-axis and y-axis limits
        plt.xlim(min(pos)-width, max(pos)+width*2)
        plt.ylim([0, jointMax ])
        
        # Adding the legend and showing the plot
        plt.legend(["US$1.25/day (%)", "US$2/day (%)"], loc='upper left',ncol=2,framealpha=0)
        #Remove yaxis
        frame1 = plt.gca()
        frame1.axes.get_yaxis().set_visible(False)
        
        ax2 = ax.twinx()
        line = plt.plot([p + width*0.5 for p in pos],
                df["GDP per capita PPP ($)"],
                color='#47b7b9',
                lw=5)
        
        autolabel(line,ax2,'#47b7b9',fontsize=16,fontweight="bold")
        
        
        # Adding the legend and showing the plot
        plt.legend(["GDP per capita\nPPP ($)"], loc='upper right',framealpha=0)
        # Setting the x-axis and y-axis limits
        plt.xlim(min(pos)-width, max(pos)+width*2)
        plt.ylim([0, df["GDP per capita PPP ($)"].max()*1.5 ])
        
        # Set the position of the x ticks
        ax.set_xticks([p + .5 * width for p in pos])
        ax2.set_xticks([p + .5 * width for p in pos])
        
        # Set the labels for the x ticks
        ax.set_xticklabels(df['year'])
        ax2.set_xticklabels(df['year'])
        
        #Remove yaxis
        frame2 = plt.gca()
        frame2.axes.get_yaxis().set_visible(False)
        
        #Remove frame
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        #ax.spines['bottom'].set_visible(False)
        ax.spines['left'].set_visible(False)
        ax2.spines['top'].set_visible(False)
        ax2.spines['right'].set_visible(False)
        #ax.spines['bottom'].set_visible(False)
        ax2.spines['left'].set_visible(False)
        
        fig.tight_layout()
        
        plt.savefig('/Users/Alex/charts/%s.png' % country,transparent=True,bbox_inches="tight")