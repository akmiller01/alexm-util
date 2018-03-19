import pandas as pd
import psycopg2
from psycopg2 import sql
import StringIO

def cleanColumns(columns):
    cols = []
    for col in columns:
        col = col.replace(' ', '_')
        cols.append(col)
    return cols

def pd_to_sql(df,table_name,conn):
    data = StringIO.StringIO()
    df.columns = cleanColumns(df.columns)
    df.to_csv(data,header=False,index=False)
    data.seek(0)
    cur = conn.cursor()
    cur.execute("DROP TABLE {}".format(table_name))
    empty_table = pd.io.sql.get_schema(df,table_name,con=conn)
    empty_table = empty_table.replace('"','')
    cur.execute(empty_table)
    cur.copy_from(data,table_name,sep=",")
    conn.close()