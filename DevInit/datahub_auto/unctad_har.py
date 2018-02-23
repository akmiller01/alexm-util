import requests
import pandas as pd
from bs4 import BeautifulSoup
import pdb

url = "http://unctadstat.unctad.org/wds/TableViewer/getData.aspx?ReportId=96740&row=1&col=1&rowCount=200&colCount=100"
params = {
    "CS_ActiveXEnabled":"false",
    "CS_FramesInHelp":"True",
    "CS_HelpPage":"",
    "CS_IVTReportIsOpened":"False",
    "CS_InHelp":"False",
    "CS_NextPage":"",
    "CS_Printable":"False",
    "CS_ReportTitle":"",
    "CS_SaveMode":"True",
    "CS_TableauHeight":"272",
    "CS_TableauWidth":"1920",
    "CS_TargetPage":"",
    "CS_bIVTReportHasMapTab":"False",
    "CS_demoIndex":"0",
    "CS_langSwitch":"",
    "IF_ActiveFolder":"27",
    "IF_Mode":"0",
    "IF_NextPage":"",
    "IF_PageBeforeSummary":"",
    "IF_ReportID":"126049",
    "IF_ReportName":"",
    "IF_ReportType":"1",
    "IF_SearchExactWord":"True",
    "IF_SearchProperties":"",
    "IF_SearchString":"",
    "IF_SearchType":"",
    "LG_reprompt":"",
    "LG_targetpage":"",
    "PR_ActiveProfileList":"",
    "PR_Mode":"0",
    "PR_NextPage":"",
    "PR_TableDefaultLang":"default",
    "PR_TableLang":"default",
    "RF_Action":"",
    "RF_ActionFolderPath":"",
    "RF_ActionInContentsPane":"",
    "RF_CopyId":"",
    "RF_CopyName":"",
    "RF_CopyPathSource":"",
    "RF_CopyType":"",
    "RF_DelFolder":"",
    "RF_NewFolder":"",
    "RF_NextPage":"",
    "RF_Rename":"",
    "RF_ReportID":"126049",
    "RF_ReportType":"0",
    "RF_SearchString":"",
    "RF_SelectedId":"",
    "sCS_AppPath":"%2Fwds",
    "sCS_ChosenLang":"en",
    "sCS_DownloadLimit":"500000",
    "sCS_SpawnWindow":"True",
    "sCS_referer":"",
    "sRF_ActivePath":"P%2C5%2C27",
    "sRF_Expanded":"%2CP%2C5%2C",
    "sRF_InManageReportsMode":"False",
    "sRF_IncludePA":"False",
    "sRF_Mode":"",
    "sRF_PanePosition":"300",
    "sRF_PosX":"0",
    "sRF_PosY":"0",
    "sRF_PreviousTask":"",
    "sRF_SameTitle":"True",
    "sRF_ScopedSearching":"False",
    "sRF_ScrollPosition":"",
    "sRF_SearchExactWordBuf":"True",
    "sRF_SearchFolder":"0",
    "sRF_SearchFromMap":"False",
    "sRF_SearchPerform":"False",
    "sRF_SearchProperties":"",
    "sRF_SearchRangeBuf":"",
    "sRF_SearchReportIDs":"",
    "sRF_SearchStringBuf":"",
    "sRF_SearchTypeBuf":"",
    "sRF_ShowFolders":"1",
    "sRF_SortAscending":"False",
    "sRF_SortField":"",
    "sRF_Task":"0",
    "sRF_User":"",
    "sRF_ViewTop":"0"
}
fdi_results = []
response = requests.post(url=url,params=params)

raw_content = response.content
soup = BeautifulSoup(raw_content,"lxml")

col_labels = soup.find_all("collabel")
row_header = soup.find("rowdim").find("dimlabel").text
headers = [row_header] + [col_label.text for col_label in col_labels]

rows = soup.find_all("row")
for row in rows:
    row_label = row.find("rowlabel").text
    cells = row.find_all("c")
    output_row = [row_label]
    for cell in cells:
        try:
            output = cell['v']
        except KeyError:
            output = None
        output_row.append(output)
    fdi_results.append(output_row)
                       
data = pd.DataFrame(fdi_results)
data.columns = headers

data.to_csv("C:/Users/Alex/Documents/Data/unctad_fdi.csv",index=False)
