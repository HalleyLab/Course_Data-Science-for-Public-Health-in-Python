import streamlit as st
import pandas as pd
import plotly.graph_objects as go

st.set_page_config(layout="wide")

@st.cache_data
def load():
    data = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    drop_list = ["Diamond Princess", "MS Zaandam", "Summer Olympics 2020", "Winter Olympics 2022", "Antarctica"]
    
    def f(file):
        df = pd.read_csv(data + file)
        df = df[~df["Country/Region"].isin(drop_list)]
        df = df.groupby("Country/Region").sum(numeric_only=True)
        df = df.drop(columns=["Lat", "Long"], errors="ignore").T
        df.index = pd.to_datetime(df.index, errors="coerce")
        df.index.name = "Date"
        df = df.ffill().fillna(0)
        df = df.cummax()
        return df
    return f("time_series_covid19_confirmed_global.csv"), f("time_series_covid19_deaths_global.csv"), f("time_series_covid19_recovered_global.csv")

if "countries_map" not in st.session_state: st.session_state.countries_map = {}
if "opacity" not in st.session_state: st.session_state.opacity = {}
confirmed, deaths, recovered = load()

milestones = {
    "WHO Pandemic": "2020-03-11",
    "1M Global Deaths": "2020-09-28",
    "First Vaccine": "2020-12-08",
    "Delta VOC": "2021-05-11",
    "Omicron VOC": "2021-11-26",
    "End of Emergency": "2023-05-05"
}

with st.sidebar:
    st.title("Settings")
    theme = st.selectbox("Appearance", ["Dark Mode", "Light Mode"])
    dark = True if theme == "Dark Mode" else False
    st.divider()
    start_date = confirmed.index.min().to_pydatetime()
    end_date = confirmed.index.max().to_pydatetime()
    date_range = st.date_input("Date Range", [start_date, end_date], min_value=start_date, max_value=end_date)
    st.divider()
    selected_events = st.multiselect("Milestones", list(milestones.keys()), default=[])
    st.divider()
    st.subheader("Statistics")
    statistics = st.empty()

background_color, side_color, text_color, grid_color, widget_color = ("#111", "#1a1a1a", "#eee", "#333", "#262626") if dark else ("#fff", "#f5f5f5", "#111", "#ddd", "#f0f2f6")
theme_name = "plotly_dark" if dark else "plotly_white"

st.markdown(f"""
    <style>
    header[data-testid="stHeader"] {{
            background-color: rgba(0,0,0,0) !important;
    }}
    .stApp {{
            background-color: {background_color} !important;
    }}
    [data-testid="stSidebar"] {{
        background-color: {side_color} !important;
        border-right: 1px solid {grid_color};
    }}
    .block-container {{
        padding: 0rem 3rem !important;
    }}
    
    .stApp h1, .stApp h2, .stApp h3, .stApp p, .stApp label, .stApp li {{ 
        color: {text_color} !important; 
    }}
    h1 {{
        font-weight: 700;
        margin-top: 10px !important;
    }}
    
    div[data-baseweb="select"] > div,
    div[data-testid="stDateInput"] > div {{ 
        background-color: {widget_color} !important; 
        border: 1px solid {grid_color} !important;
        border-radius: 8px !important;
        box-shadow: none !important;
        outline: none !important;
    }}
    
    div[data-baseweb="select"] > div:focus-within,
    div[data-testid="stDateInput"] > div:focus-within {{
        border: 1px solid {grid_color} !important;
        box-shadow: none !important;
        outline: none !important;
    }}
    
    div[data-baseweb="select"] * {{
        color: {text_color} !important;
    }}
    
    div[data-baseweb="select"] input {{
        -webkit-text-fill-color: {text_color} !important;
    }}
    
    div[data-baseweb="select"] input::placeholder {{
        -webkit-text-fill-color: {text_color} !important;
        opacity: 0.7 !important;
    }}
    
    div[data-baseweb="select"] svg {{
        fill: {text_color} !important;
    }}
    
    div[data-baseweb="select"] > div > div {{
        background-color: transparent !important;
    }}
    
    ul[role="listbox"],
    ul[role="listbox"] li[role="option"],
    ul[role="listbox"] li[role="option"] span {{
        background-color: {widget_color} !important;
        color: {text_color} !important;
    }}
    ul[role="listbox"] li[role="option"]:hover,
    ul[role="listbox"] li[role="option"]:hover span {{
        background-color: {grid_color} !important;
    }}
    
    div[data-testid="stDateInput"] > div > div,
    div[data-testid="stDateInput"] div[data-baseweb="input"],
    div[data-testid="stDateInput"] div[data-baseweb="base-input"] {{
        background-color: transparent !important; 
    }}
    
    div[data-testid="stDateInput"] input {{
        color: {text_color} !important;
        -webkit-text-fill-color: {text_color} !important;
        background-color: transparent !important;
    }}
    
    div[data-baseweb="calendar"] [role="button"]:hover {{
        background-color: {grid_color} !important;
    }}

    div[data-baseweb="calendar"] [aria-selected="true"],
    div[data-baseweb="calendar"] [aria-selected="true"] > div {{
        background-color: #ff4b4b !important;
        color: #ffffff !important;
    }}

    .stMultiSelect [data-baseweb="tag"] {{ 
        background-color: {grid_color} !important; 
    }}
    .stMultiSelect [data-baseweb="tag"] span {{
        color: {text_color} !important;
    }}

    button[data-testid="stSidebarCollapseButton"] svg,
    button[kind="headerNoPadding"] svg {{
        fill: {text_color} !important;
        color: {text_color} !important;
    }}

    input[type="color"] {{ 
        height: 25px !important; 
    }}
    </style>
    <h1 style='text-align: center;'>COVID-19 Dashboard</h1>
""", unsafe_allow_html=True)

avg = 1
_, table1, table2, table3, _ = st.columns([0.5, 1.2, 5.5, 2.3, 0.5])

with table1:
    mode = st.selectbox("Mode", ["Cumulative", "Daily"])
with table2:
    if mode == "Daily":
        column1, column2 = st.columns([1, 2.5])
        with column1: avg = st.slider("Smoothing", 1, 14, 7)
        with column2: selected_countries = st.multiselect("Nations", confirmed.columns.tolist(), default=["US"])
    else:
        selected_countries = st.multiselect("Nations", confirmed.columns.tolist(), default=["US"])
with table3:
    selected_metrics = st.multiselect("Metrics", ["Confirmed", "Deaths", "Recovered"], default=["Confirmed"])

pal = ["#5856D6", "#FF9F0A", "#30D158", "#FF375F", "#64D2FF", "#BF5AF2", "#FFD60A", "#8E8E93"]

if selected_countries and selected_metrics:
    for countries in selected_countries:
        for metrics in selected_metrics:
            label = f"{countries} ({metrics})"
            if label not in st.session_state.countries_map:
                st.session_state.countries_map[label] = pal[len(st.session_state.countries_map) % len(pal)]
            if label not in st.session_state.opacity:
                st.session_state.opacity[label] = 1.0

    main_col, right_col = st.columns([8.8, 1.2])
    with right_col:
        st.markdown("<br><br>", unsafe_allow_html=True)
        customs = st.selectbox("Style", selected_countries)
        for metrics in selected_metrics:
            label = f"{customs} ({metrics})"
            st.markdown(f"**{metrics}**")
            st.session_state.countries_map[label] = st.color_picker("C", st.session_state.countries_map[label], key=f"c_{label}", label_visibility="collapsed")
            st.session_state.opacity[label] = st.slider("O", 0.1, 1.0, st.session_state.opacity[label], 0.1, key=f"o_{label}", label_visibility="collapsed")
            
    with main_col:
        fig = go.Figure()
        dfs = {"Confirmed": confirmed, "Deaths": deaths, "Recovered": recovered}
        weights = {"Confirmed": 2.5, "Deaths": 1.5, "Recovered": 2.0}
        start, end = (date_range[0], date_range[1]) if len(date_range) == 2 else (start_date, end_date)
        
        stats_content = f"**Current Period**\n"
        stats_content += f"* Span: {(pd.to_datetime(end)-pd.to_datetime(start)).days} days\n\n"
        
        for country in selected_countries:
            stats_content += f"**{country}**\n"
            confirm_val = confirmed[country].loc[pd.to_datetime(end)]
            death_val = deaths[country].loc[pd.to_datetime(end)]
            recover_val = recovered[country].loc[pd.to_datetime(end)]
            
            stats_content += f"* Confirmed: {int(confirm_val):,}\n"
            stats_content += f"* Deaths: {int(death_val):,}\n"
            stats_content += f"* Recovered: {int(recover_val):,}\n"
            
            if confirm_val > 0:
                mortality = (death_val / confirm_val) * 100
                stats_content += f"* Mortality Rate: {mortality:.2f}%\n"
            stats_content += "\n"
        
        statistics.markdown(stats_content)

        for metrics in selected_metrics:
            tmp = dfs[metrics][selected_countries].loc[pd.to_datetime(start):pd.to_datetime(end)].copy()
            if mode == "Daily":
                tmp = tmp.diff().fillna(0).clip(lower=0)
                if avg > 1: tmp = tmp.rolling(window=avg).mean()
            for country in selected_countries:
                label = f"{country} ({metrics})"
                fig.add_trace(go.Scatter(
                    x=tmp.index, y=tmp[country], name=label, mode="lines",
                    line=dict(color=st.session_state.countries_map[label], width=weights[metrics]),
                    opacity=st.session_state.opacity[label],
                    fill='tozeroy' if mode == "Daily" and len(selected_countries) == 1 else None
                ))

        sorted_events = sorted(selected_events, key=lambda x: pd.to_datetime(milestones[x]))
        for i, ev in enumerate(sorted_events):
            d = pd.to_datetime(milestones[ev])
            if pd.to_datetime(start) <= d <= pd.to_datetime(end):
                fig.add_vline(x=d, line_dash="dash", line_color=grid_color)
                y_val = 1.0 - (i % 2) * 0.05
                fig.add_annotation(
                    x=d, y=y_val, yref="paper", text=ev, 
                    showarrow=False, font=dict(color=text_color, size=10),
                    xanchor="left", xshift=4
                )

        y_type = "log" if mode == "Cumulative" else "linear"
        y_title = "Number of People (Log Scale)" if mode == "Cumulative" else "Number of People"
        
        fig.update_layout(
            template=theme_name,
            paper_bgcolor="rgba(0,0,0,0)",
            plot_bgcolor="rgba(0,0,0,0)",
            hovermode="x unified",
            height=700,
            margin=dict(l=20, r=20, t=60, b=20),
            legend=dict(
                orientation="h",
                yanchor="bottom",
                y=1.05,
                xanchor="right",
                x=1,
                font=dict(color=text_color)
            ),
            xaxis=dict(
                title=dict(text="Date", font=dict(color=text_color)),
                showgrid=False, 
                tickfont=dict(color=text_color),
                tickformat="%b\n%Y",
                dtick="M1",
                showline=True,
                linecolor=grid_color,
                linewidth=1
            ),
            yaxis=dict(
                title=dict(text=y_title, font=dict(color=text_color)),
                type=y_type, 
                showgrid=True, 
                gridcolor=grid_color, 
                tickfont=dict(color=text_color),
                showline=True,
                linecolor=grid_color,
                linewidth=1
            )
        )
        st.plotly_chart(fig, use_container_width=True)
