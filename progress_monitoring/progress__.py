import streamlit as st
import pandas as pd
import datetime
import plotly.express as px
import plotly.graph_objects as go
import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime
from PIL import Image
# Convert strings to DataFrames
from io import StringIO
import streamlit as st
import pydeck as pdk




#read data 
hospital_list = pd.read_csv('data/Hospital_List.csv')

pre_audit_df = pd.read_csv('data/pre_audit.csv')
network_analysis_df = pd.read_csv('data/network_analysis.csv')
energy_audit_df = pd.read_csv('data/energy_audit.csv')
solar_feasibility_df = pd.read_csv('data/solar_feasibility.csv')
environmental_impact_df = pd.read_csv('data/environmental_impact.csv')
Geospatial_progress_df = pd.read_csv('data/Geospatial_progress.csv')
map_df = pd.read_csv('data/map.csv')

# or equivalently
pre_audit_count = pre_audit_df.shape[0]
network_analysis_count = network_analysis_df.shape[0]
energy_audit_count = energy_audit_df.shape[0]
solar_feasibility_count = solar_feasibility_df.shape[0]
environmental_impact_count = environmental_impact_df.shape[0]
Geospatial_progress_count = Geospatial_progress_df.shape[0]

data_collected = (pre_audit_count + network_analysis_count + energy_audit_count + solar_feasibility_count + environmental_impact_count + Geospatial_progress_count)

Progress = (pre_audit_count)/180 * 100


deficit = 100 - Progress


Pre_Audit_progress = f"{round((pre_audit_count / 36) * 100)}%"
Network_Analysis_progress = "0%"
Energy_Audit_progress = "0%"
Solar_Feasibility_progress = f"0%"
Environmental_impact_progress = "0%"
Geospatial_progress = "0%"




# Add status column based on whether hospital name appears in audited data
hospital_list["Pre Audit"] = hospital_list["What is the name of the hospital?"].apply(
    lambda name: "✅" if name in pre_audit_df["What is the name of the hospital?"].values else "❌"
)

# Add status column based on whether hospital name appears in audited data
hospital_list["Network Analysis"] = hospital_list["What is the name of the hospital?"].apply(
    lambda name: "✅" if name in network_analysis_df["What is the name of the hospital?"].values else "❌"
)

# Add status column based on whether hospital name appears in audited data
hospital_list["Energy Audit"] = hospital_list["What is the name of the hospital?"].apply(
    lambda name: "✅" if name in energy_audit_df["What is the name of the hospital?"].values else "❌"
)

# Add status column based on whether hospital name appears in audited data
hospital_list["Solar Feasibility"] = hospital_list["What is the name of the hospital?"].apply(
    lambda name: "✅" if name in solar_feasibility_df["What is the name of the hospital?"].values else "❌"
)

# Add status column based on whether hospital name appears in audited data
hospital_list["Environmental Impact"] = hospital_list["What is the name of the hospital?"].apply(
    lambda name: "✅" if name in environmental_impact_df["What is the name of the hospital?"].values else "❌"
)


# Add status column based on whether hospital name appears in audited data
hospital_list["Geospatial"] = hospital_list["What is the name of the hospital?"].apply(
    lambda name: "✅" if name in Geospatial_progress_df["What is the name of the hospital?"].values else "❌"
)





# --- Status Table ---
#status_table = pd.DataFrame({
#    "Name of Hospital": ["Hospital 1", "Hospital 2", "Hospital 3", "Hospital 4", "Hospital 5"],
#    "Pre Audit": ["✅", "✅", "✅", "", "❌"],
#    "Network Analysis": ["❌", "🟡", "🟡", "", "❌"],
#    "Energy Audit": ["❌", "", "🟡", "✅", "✅"],
#    "Solar Feasibility": ["✅", "", "", "✅", "✅"],
#    "Environmental Impact": ["❌", "", "", "", "❌"]
#})




# Set page configuration
st.set_page_config(page_title="NESIP", layout="wide")

# Create two columns: one for the logo, one for the title
col1, col2 = st.columns([0.5, 6])  # Adjust column width ratio as needed

# Add logo to the left column
with col1:
    st.image("https://www.vista-advisory.com/wp-content/uploads/2024/07/image-18.png", width=100)

# Add title and motto to the right column
with col2:
    st.markdown("""
        <h3 style='margin-bottom: 0px;'>ENERGY AUDIT OF 32 HOSPITALS FOR LAGOS STATE</h3>
        <hr style='border:1px solid #ddd; margin: 5px 0;'>
        <p style='font-size: 14px; color: #555;'>Data collection Tracking Dashboard</p>
    """, unsafe_allow_html=True)






#st.write("""
#Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s...
#""")

# --- Top Cards ---
col1, col2, col3 = st.columns(3)

with col1:
    st.metric(label="Hospitals Completed", value="0 out of 31", delta=f"{Progress:.1f}% completed")


with col2:
    st.write("###### Start Date")
    st.info("27th of May, 2025")

with col3:
    st.write("###### Estimated End Date")
    st.info("31st of July, 2025")

st.markdown("---")

# --- Progress Ring ---
col4, col5, col6 = st.columns([2, 2, 2])

with col4:
    st.markdown("##### Progress (Completion Rate)")
    # Create a donut chart with Plotly
    fig = go.Figure(data=[
        go.Pie(
            values=[deficit, Progress],
            labels=["Pending", "Completed"],
            hole=0.7,
            textinfo='label+percent',
            marker_colors=["#E6EAF5", "#0D1A73"]
        )
    ])
    fig.update_layout(showlegend=False, margin=dict(t=0, b=0, l=0, r=0), width=300, height=300)
    st.plotly_chart(fig)

with col5:

    st.markdown("##### Completion Rate Per Deliverable")
    
    metrics = {
        "Pre Audit": Pre_Audit_progress,
        "Network Analysis": "0%",
        "Energy Audit": "0%",
        "Solar Feasibility": "0%",
        "Environmental Impact": "0%",
        "Geospatial": "0%"
    }

    cols = st.columns(2)
    items = list(metrics.items())
    for i in range(0, len(items), 2):
        row = st.columns(2)
        for j in range(2):
            if i + j < len(items):
                key, value = items[i + j]
                with row[j]:
                    st.metric(label=key, value=value)


with col6:
    st.markdown("##### Geospatial (Completed Hospitals)")

    # Dummy data with placeholder lat/lon
    map_df = pd.DataFrame({'lat': [0], 'lon': [0]})

    # Define view
    view = pdk.ViewState(latitude=0, longitude=0, zoom=1.5)

    # Define map layer
    layer = pdk.Layer(
        "ScatterplotLayer",
        data=map_df,
        get_position='[lon, lat]',
        get_radius=100,
        get_color='[0, 0, 255, 160]',
        pickable=True
    )

    # Set height here
    r = pdk.Deck(
        layers=[layer],
        initial_view_state=view,
        map_style="mapbox://styles/mapbox/light-v9",
        height=250  # ✅ Control height here
    )

    # Render in Streamlit
    st.pydeck_chart(r)
 

# --- Sample Data for the Bar Chart ---
activities = ["Pre Audit", "Network Analysis", "Energy Audit", "Solar Feasibility", "Environmental Impact"]
data = {
    "Hospital Count": [32, 32, 32, 32, 32],
    "Hospital Collected": [pre_audit_count, 0, 0, 0, 0],
    "Verified/Passed Check": [0, 0, 0, 0, 0],
    "Unverified Data": [pre_audit_count, 0, 0, 0, 0],
}

# --- Title ---
st.markdown("### Data Quality")

# --- Bar Chart ---
fig = go.Figure()

colors = {
    "Hospital Count" : "#068744",
    "Hospital Collected": "#36A2EB",
    "Verified/Passed Check": "#064c87",
    "Unverified Data" : "#f9bc6c",
}

for key in data:
    fig.add_trace(go.Bar(
        name=key,
        x=activities,
        y=data[key],
        marker_color=colors[key]
    ))

fig.update_layout(
    barmode='group',
    title="Activity Data Overview",
    xaxis_title="Activity",
    yaxis_title="Count",
    height=400
)

# Layout: Chart + Metrics
col1, col2 = st.columns([3, 1])

with col1:
    st.plotly_chart(fig, use_container_width=True)

with col2:
    st.metric("Data Expected", "192")
    st.metric("Data Collected", data_collected)
    st.metric("Verified Data", "0")

# --- Dropdown Filter ---
# Create the selectbox with dynamic hospital names
hospital_names = ["All"] + hospital_list["What is the name of the hospital?"].unique().tolist()
selected_hospital = st.selectbox("Select Hospital", hospital_names)

# Filter the table based on selection
if selected_hospital == "All":
    filtered_table = hospital_list
else:
    filtered_table = hospital_list[hospital_list["What is the name of the hospital?"] == selected_hospital]



st.dataframe(filtered_table, use_container_width=True)

# --- Legend ---
st.markdown("""
✅ **Completed** &nbsp;&nbsp;&nbsp;
🟡 **In progress** &nbsp;&nbsp;&nbsp;
❌ **Not completed**
""")
