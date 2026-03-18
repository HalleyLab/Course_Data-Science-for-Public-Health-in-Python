import streamlit as st
import pandas as pd

st.title("Map Generator")

col1, col2 = st.columns(2)

with col1:
    st.subheader("Latitude")
    lat = st.number_input("Latitude Value (0.00-90.00)", min_value=0.0, max_value=90.0, format="%.2f")
    lat_dir = st.radio("Direction", ["North", "South"])

with col2:
    st.subheader("Longitude")
    lon = st.number_input("Longtitude Value (0.00-180.00)", min_value=0.0, max_value=180.0, format="%.2f")
    lon_dir = st.radio("Direction", ["East", "West"])

if st.button("Generate Map"):
    lat = lat if "North" in lat_dir else -lat
    lon = lon if "East" in lon_dir else -lon
    data = pd.DataFrame({'lat': [lat], 'lon': [lon]})
    st.success(f"Mapping coordinates: {lat:.2f}, {lon:.2f}")
    st.map(data)