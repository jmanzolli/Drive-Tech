from inst.python.functions import *

def run_gurobi(data, log_file=None):
    try:
        # data = read_file("./inst/data/v2/input.xlsx")
        model = setModel(
            data,
            time_limit=60,
            mipgap=0,
            solver="gurobi",
            log_file=log_file,
            status=True,
        )

        output = result(model)

        return run_analytics(output)
    
    except Exception as e:
        print(e)
        return False

def run_analytics(data):
    energy_df = data["ENERGY"]
    soc_df = data["SOC"]
    power_df = data["POWER"]
    charger_enabled_df = data["CHARGERS_ENABLED"]
    charger_assigned_df = data["CHARGERS_ASSIGNED"]

    # Drop any Unnamed columns if they still exist
    energy_df = energy_df.loc[:, ~energy_df.columns.str.contains("^Unnamed")]
    soc_df = soc_df.loc[:, ~soc_df.columns.str.contains("^Unnamed")]
    power_df = power_df.loc[:, ~power_df.columns.str.contains("^Unnamed")]
    charger_enabled_df = charger_enabled_df.loc[:, ~charger_enabled_df.columns.str.contains("^Unnamed")]
    charger_assigned_df = charger_assigned_df.loc[:, ~charger_assigned_df.columns.str.contains("^Unnamed")]

    # Calculate total energy consumption (fleet and per bus)
    total_energy_consumption_per_bus = energy_df.max() - energy_df.min()
    total_energy_consumption_fleet = total_energy_consumption_per_bus.sum()

    # Calculate total charging time (fleet and per bus) based on energy increase
    charging_step_value = 15  # in minutes
    energy_diff = energy_df.diff()
    charging_time_per_bus = (energy_diff > 0).sum() * (
        charging_step_value / 60
    )  # converting minutes to hours
    total_charging_time_fleet = charging_time_per_bus.sum()

    # Calculate average energy consumption (fleet)
    average_energy_consumption_fleet = total_energy_consumption_fleet / len(
        total_energy_consumption_per_bus
    )

    # Calculate average charging time (fleet)
    average_charging_time_fleet = (
        total_charging_time_fleet / len(charging_time_per_bus)
        if len(charging_time_per_bus) > 0
        else 0
    )

    # Calculate maximum and minimum SOC (per bus)
    max_soc_per_bus = soc_df.max()
    min_soc_per_bus = soc_df.min()

    # Calculate average SOC (per bus)
    average_soc_per_bus = soc_df.mean()

    # Calculate charging window (per bus)
    charging_window_per_bus = (
        charger_assigned_df[charger_assigned_df["Charging_Status"] == 1]
        .groupby("Bus")["Time"]
        .agg(["min", "max"])
    )

    # Calculate charger number (per bus)
    charger_number_per_bus = charger_assigned_df.groupby("Bus")["Charger"].max()

    # Calculate state percentages (idle, running, charging)
    # Determine state for each timestep and bus
    states = pd.DataFrame(index=energy_diff.index, columns=energy_diff.columns)
    states[energy_diff > 0] = "Charging"
    states[energy_diff < 0] = "Running"
    states[energy_diff == 0] = "Idle"

    # Calculate state percentages for each bus
    state_percentage_per_bus = states.apply(
        lambda col: col.value_counts(normalize=True) * 100
    ).fillna(0)

    # Calculate fleet state percentages
    fleet_states = states.apply(pd.Series.value_counts, axis=1).fillna(0)
    fleet_states = fleet_states.div(fleet_states.sum(axis=1), axis=0) * 100
    fleet_idle_percentage = (
        fleet_states["Idle"].mean() if "Idle" in fleet_states.columns else 0
    )
    fleet_running_percentage = (
        fleet_states["Running"].mean() if "Running" in fleet_states.columns else 0
    )
    fleet_charging_percentage = (
        fleet_states["Charging"].mean() if "Charging" in fleet_states.columns else 0
    )

    # Display the results
    print(f"Total Energy Consumption (Fleet): {total_energy_consumption_fleet} kWh")
    print(f"Total Energy Consumption (Per Bus):")
    print(total_energy_consumption_per_bus)

    print(f"Total Charging Time (Fleet): {total_charging_time_fleet} hours")
    print(f"Total Charging Time (Per Bus):")
    print(charging_time_per_bus)

    print(f"Average Energy Consumption (Fleet): {average_energy_consumption_fleet} kWh")
    print(f"Average Charging Time (Fleet): {average_charging_time_fleet} hours")

    print(f"Maximum SOC (Per Bus):")
    print(max_soc_per_bus)
    print(f"Minimum SOC (Per Bus):")
    print(min_soc_per_bus)
    print(f"Average SOC (Per Bus):")
    print(average_soc_per_bus)

    print(f"Charging Window (Per Bus):")
    print(charging_window_per_bus)

    print(f"Charger Number (Per Bus):")
    print(charger_number_per_bus)

    print(
        f"State Percentages (Fleet): Idle: {fleet_idle_percentage:.2f}%, Running: {fleet_running_percentage:.2f}%, Charging: {fleet_charging_percentage:.2f}%"
    )
    print(f"State Percentages (Per Bus):")
    print(state_percentage_per_bus)

    results = {
        'Total Energy Consumption (Fleet)': [total_energy_consumption_fleet],
        'Total Charging Time (Fleet)': [total_charging_time_fleet],
        'Average Energy Consumption (Fleet)': [average_energy_consumption_fleet],
        'Average Charging Time (Fleet)': [average_charging_time_fleet],
        'Fleet Idle Percentage': [fleet_idle_percentage],
        'Fleet Running Percentage': [fleet_running_percentage],
        'Fleet Charging Percentage': [fleet_charging_percentage],
    }

    data['ANALYTICS'] = {
        'Energy Consumption Per Bus': pd.DataFrame(total_energy_consumption_per_bus),
        'Charging Time Per Bus': pd.DataFrame(charging_time_per_bus),
        'Max SOC Per Bus': pd.DataFrame(max_soc_per_bus),
        'Min SOC Per Bus': pd.DataFrame(min_soc_per_bus),
        'Avg SOC Per Bus': pd.DataFrame(average_soc_per_bus),
        'Charging Window Per Bus': pd.DataFrame(charging_window_per_bus),
        'Charger Number Per Bus': pd.DataFrame(charger_number_per_bus),
        'State Percentage Per Bus (%)': pd.DataFrame(state_percentage_per_bus),
        'Summary': pd.DataFrame(results)
    }
    
    return(data)
