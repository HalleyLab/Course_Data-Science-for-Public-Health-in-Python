import streamlit as st
import pandas as pd
import requests as rq
import bs4
import plotly.express as px
import re

st.title("GDP by Country Stacked within Regions")

@st.cache_data
def load_data():
    url = "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"
    headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"}
    page = rq.get(url, headers=headers)
    bs4page = bs4.BeautifulSoup(page.text, "html.parser")
    tables = bs4page.find("table", {"class": "wikitable"})
    gdp = pd.read_html(str(tables))[0]
    gdp = gdp.dropna(how='all')
    gdp.columns = ['Area', 'IMF', 'World Bank', 'UN']

    gdp = gdp[~gdp['IMF'].astype(str).str.contains('N/a|—', case=False, na=False)]
    gdp['Area'] = gdp['Area'].astype(str).apply(lambda x: re.sub(r'\[.*?\]()', '', x).strip())
    
    for col in ['IMF', 'World Bank', 'UN']:
        gdp[col] = gdp[col].astype(str).str.split(r'\(|\[|（').str[0]
        gdp[col] = gdp[col].str.replace(r'[^\d]', '', regex=True)
        gdp[col] = pd.to_numeric(gdp[col], errors='coerce')
        
    gdp = gdp.dropna(subset=['Area']).reset_index(drop=True)
    gdp = gdp[gdp['Area'] != 'World']

    url = "https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_the_United_Nations_geoscheme"
    headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"}
    page = rq.get(url, headers=headers)
    bs4page =  bs4.BeautifulSoup(page.text, "html.parser")
    tables = bs4page.find("table", {"class": "wikitable"})
    regions = pd.read_html(str(tables))[0].iloc[:, 0].copy()
    regions = regions.dropna().reset_index(drop=True)

    def getcountry(area):
        area = area.lower()
        if 'macau' in area or 'macao' in area or 'taiwan' in area: 
            return 'China'
        for name in regions:
            country_or_area = str(name)
            if area.lower() in country_or_area.lower():
                testname = re.split(r'\(|\[|（', country_or_area)[0].strip()
                if ',' in testname:
                    country = testname.split(',', 1)[0].strip()
                    return country
                else:
                    return area
        return area

    gdp['Country'] = gdp['Area'].apply(getcountry)
    gdp['Country'] = gdp['Country'].str.title()
    gdp = gdp.sort_values(by='IMF', ascending=True)
    return gdp

with st.spinner("Scraping and processing data..."):
    df = load_data()

source = st.selectbox("Select Data Source:", ["IMF", "UN", "World Bank"])

df_plot = df.dropna(subset=[source]).sort_values(by=source, ascending=True)

fig = px.bar(
    df_plot,
    x="Country",
    y=source,
    color="Area",
    labels={source: "GDP (Million USD)", "Region": "Region", "Area": "Country"},
    barmode="stack",
)

fig.update_layout(
    showlegend=False,
    font={'family': 'Helvetica'},
    xaxis={'categoryorder': 'total descending', 'tickangle': -45},
    yaxis={'type': 'log', 'title': f'GDP (Million USD - Log Scale'},
    title={'text': f"GDP by Country Stacked within Regions (Source: {source})", 'x': 0.5, 'y': 0.9, 'xanchor': 'center'}
)

st.plotly_chart(fig, use_container_width=True)