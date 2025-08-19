import streamlit as st
import pandas as pd
import datetime
import plotly.express as px
import plotly.graph_objects as go
import matplotlib.pyplot as plt
import numpy as np
import streamlit as st
import pydeck as pdk



#read data 
hospital_list = pd.read_csv('data/Hospital_List.csv')

pre_audit_df = pd.read_csv('data/pre_audit.csv')
network_analysis_df = pd.read_csv('data/network_analysis.csv')
energy_audit_df = pd.read_csv('data/energy_audit.csv')
solar_feasibility_df = pd.read_csv('data/solar_feasibility.csv')
environmental_impact_df = pd.read_csv('data/environmental_impact.csv')
Power_Analyzer_progress_df = pd.read_csv('data/Power_Analyzer_progress.csv')
map_df = pd.read_csv('data/map.csv')

# or equivalently
pre_audit_count = pre_audit_df['What is the name of the hospital?'].nunique()
network_analysis_count = network_analysis_df['What is the name of the hospital?'].nunique()
##################################
# --- Step 1: Replace "Others" with Other_Name ---
energy_audit_df["Final_Building"] = energy_audit_df.apply(
    lambda row: row["If Others, Kindly Input Building Name"] if row["Building Name"] == "Others" else row["Building Name"], axis=1
)

# --- Step 2: Split multiple buildings separated by ";" ---
energy_audit_df_expanded = (
    energy_audit_df.assign(Final_Building=energy_audit_df["Final_Building"].str.split(r";\s*"))
    .explode("Final_Building")
    .dropna(subset=["Final_Building"])
)

# Strip whitespace
energy_audit_df_expanded["Final_Building"] = energy_audit_df_expanded["Final_Building"].str.strip()

# --- Step 3: Get unique buildings per hospital ---
df_unique = (
    energy_audit_df_expanded[["What is the name of the hospital?", "Final_Building"]]
    .drop_duplicates()
    .groupby("What is the name of the hospital?")
    .size()
    .reset_index(name="Unique_Audited")
)

# --- Step 4: Merge with totals and calculate % completion ---
ea_progress = network_analysis_df.merge(df_unique, on="What is the name of the hospital?", how="left")
ea_progress["Unique_Audited"] = ea_progress["Unique_Audited"].fillna(0).astype(int)
ea_progress["Completion_Percent"] = (ea_progress["Unique_Audited"] / ea_progress["Number of building at the hospital"] * 100).round(2)


energy_audit_count =  ea_progress["Unique_Audited"].sum()

expected_ea_count = ea_progress["Number of building at the hospital"].sum()

################################################

solar_feasibility_count = solar_feasibility_df.shape[0]
environmental_impact_count = environmental_impact_df['What is the name of the hospital?'].nunique()
Power_Analyzer_progress_count = Power_Analyzer_progress_df.shape[0]

data_collected = (pre_audit_count + network_analysis_count + energy_audit_count + solar_feasibility_count + environmental_impact_count + Power_Analyzer_progress_count)

data_expected  =  32+32+32+expected_ea_count+64+expected_ea_count

Progress = (data_collected)/data_expected * 100



deficit = 100 - Progress


Pre_Audit_progress = f"{round((pre_audit_count / 32) * 100)}%"
Network_Analysis_progress =  f"{round((network_analysis_count / 32) * 100)}%"
Energy_Audit_progress = f"{round((energy_audit_count / expected_ea_count) * 100)}%"
Solar_Feasibility_progress = f"0%"
Environmental_impact_progress = f"{round((environmental_impact_count / 32) * 100)}%"
Power_Analyzer_progress = "0%"

# Split the Geolocation column into two separate float columns
geolocation_df = network_analysis_df.copy()

# Split Geolocation column into lat/lon
geolocation_df[['lat', 'lon']] = geolocation_df['Geolocation'].str.split(',', expand=True)
geolocation_df['lat'] = geolocation_df['lat'].astype(float)
geolocation_df['lon'] = geolocation_df['lon'].astype(float)

# Keep only useful columns
geolocation_df = geolocation_df[['What is the name of the hospital?', 'Local Government', 'Contact Person', 'Type of Hospital/Specialization', 'Number of building at the hospital', 'Which Power sources are available?', 'lat', 'lon']]

geolocation_df = geolocation_df.rename(columns={"What is the name of the hospital?": "Hospital"})

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
hospital_list["Power Analyzer"] = hospital_list["What is the name of the hospital?"].apply(
    lambda name: "✅" if name in Power_Analyzer_progress_df["What is the name of the hospital?"].values else "❌"
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
    st.metric(label="Hospitals Completed", value="0 out of 32", delta=f"{Progress:.1f}% completed")



with col2:
    st.write("###### Start Date")
    st.info("27th of May, 2025")

with col3:
    st.write("###### Estimated End Date")
    st.info("30th of September, 2025")

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
        "MotorBike Survey & Network Analysis": Network_Analysis_progress,
        "Energy Audit & SE": Energy_Audit_progress,
        "Solar Feasibility": "0%",
        "Environmental Survey": Environmental_impact_progress,
        "Power Analyzer": "0%"
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
 
    map_df = pd.DataFrame(geolocation_df)

    st.markdown("##### 🏥 Geospatial (Completed Hospitals)")

    # Define view (centered around Lagos hospitals)
    view = pdk.ViewState(
        latitude=6.5,
        longitude=3.3,
        zoom=9,
        pitch=0
    )

    # Scatterplot layer for hospital markers
    layer = pdk.Layer(
        "ScatterplotLayer",
        data=map_df,
        get_position='[lon, lat]',
        get_radius=300,
        get_color='[255, 0, 0, 180]',  # red markers
        pickable=True
    )

    # Add tooltip with hospital name
    tooltip = {
        "html": "<b>Hospital:</b> {Hospital}<br/><b>Lat:</b> {lat}<br/><b>Lon:</b> {lon}",
        "style": {"backgroundColor": "white", "color": "black"}
    }


    # Deck
    r = pdk.Deck(
        layers=[layer],
        initial_view_state=view,
        map_style="mapbox://styles/mapbox/light-v9",
        tooltip=tooltip,
        height=500
    )

    # Render in Streamlit
    st.pydeck_chart(r)

 

import streamlit as st
import pandas as pd
import plotly.graph_objects as go

# --- Sample Data for the Bar Chart ---
activities = ["Pre Audit", "Motorbile & Network Analysis", "Energy Audit & SE", "Solar Feasibility", "Environmental Survey", "Power Analyzer"]
data = {
    "Hospital Count": [32, 32, 32, 32, 32, 32],
    "Hospital Collected": [pre_audit_count, network_analysis_count, 0, 0, environmental_impact_count, 0],
    "Verified/Passed Check": [0, 0, 0, 0, 0, 0],
    "Unverified Data": [pre_audit_count, network_analysis_count, 0, 0, environmental_impact_count, 0],
}

# --- Title ---
st.markdown("### Data Count Tracker and Quality Check")

# --- Bar Chart ---
fig = go.Figure()

colors = {
    "Hospital Count" : "#068744",
    "Hospital Collected": "#36A2EB",
    #"Verified/Passed Check": "#064c87",
    #"Unverified Data" : "#f9bc6c",
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
    st.metric("Data Expected", data_expected)
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
