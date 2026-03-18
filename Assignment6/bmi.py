import streamlit as st

st.title("BMI Calculator")

col1, col2 = st.columns(2)

with col1:
    height = st.number_input("Enter Height (meters)", min_value=0.01, value=3.00)

with col2:
    weight = st.number_input("Enter Weight (kilograms)", min_value=0.0, value=500.0)

if st.button("Calculate BMI"):
    bmi = weight / (height ** 2)
    st.success(f"Your Body Mass Index is: **{bmi:.1f}**")