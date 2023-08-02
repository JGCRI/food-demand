# FOOD DEMAND CONSUMER DISAGREGATION
# Ellie Lochner (2022) and Leeya Pressburger (2023)

# Setup ----------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgcam)
library(RColorBrewer)
library(ggh4x)

# Read databases...
read_dbs <- 0 #  to read, 0 to skip

if(read_dbs) {
  # Single consumer scenarios
  conn_single <- localDBConn('./databases', 'db_single_consumer')
  conn_single_2p6 <- localDBConn('./databases', 'db_single_consumer_2p6')

  # Multiple consumer scenarios
  conn_multiple <- localDBConn('./databases', 'db_multiple_consumers')
  conn_multiple_2p6 <- localDBConn('./databases', 'db_multiple_consumers_2p6')

  # Multiple consumer + trade scenarios
  conn_trade_high <- localDBConn('./databases', 'db_trade_high')
  conn_trade_default <- localDBConn('./databases', 'db_trade_default')
  conn_trade_low <- localDBConn('./databases', 'db_trade_low')

  prj <- addScenario(conn_single, './dat/food_demand.dat', 'SingleConsumer', './scripts and queries/batch_query_food_consumer_disag.xml', clobber = TRUE)
  prj <- addScenario(conn_single_2p6, './dat/food_demand.dat', 'SingleConsumer_2p6', './scripts and queries/batch_query_food_consumer_disag.xml', clobber = TRUE)

  prj <- addScenario(conn_multiple, './dat/food_demand.dat', 'MultipleConsumers', './scripts and queries/batch_query_food_consumer_disag.xml', clobber = TRUE)
  prj <- addScenario(conn_multiple_2p6, './dat/food_demand.dat', 'MultipleConsumers_2p6', './scripts and queries/batch_query_food_consumer_disag.xml', clobber = TRUE)

  prj <- addScenario(conn_trade_high, './dat/food_demand.dat', 'HighTrade', './scripts and queries/batch_query_food_consumer_disag.xml', clobber = TRUE)
  prj <- addScenario(conn_trade_default, './dat/food_demand.dat', 'DefaultTrade', './scripts and queries/batch_query_food_consumer_disag.xml', clobber = TRUE)
  prj <- addScenario(conn_trade_low, './dat/food_demand.dat', 'LowTrade', './scripts and queries/batch_query_food_consumer_disag.xml', clobber = TRUE)

} else {
  # ... or load GCAM results:
  prj <- loadProject('./dat/food_demand.dat')
}

# Define variables ---------------------------------------
regions_to_plot <- c("China", "India", "USA", "Africa_Eastern")
figure_start_year <- 2015

scenario_order = c("SingleConsumer",
                   "SingleConsumer_2p6",
                   "MultipleConsumers",
                   "MultipleConsumers_2p6",
                   "HighTrade",
                   "DefaultTrade",
                   "LowTrade")

scen_linetype = c("SingleConsumer" = "solid",
                  "SingleConsumer_2p6" = "dashed",
                  "MultipleConsumers" = "solid",
                  "MultipleConsumers_2p6" = "dashed",
                  "HighTrade" = "dashed",
                  "DefaultTrade" = "solid",
                  "LowTrade" = "dotted")

scen_colors = c("SingleConsumer" = "#00008B",
                "SingleConsumer_2p6" = "#00008B",
                "MultipleConsumers" = "#01665e",
                "MultipleConsumers_2p6" = "#01665e",
                "HighTrade" = "#A3A3FF",
                "DefaultTrade" = "#A3A3FF",
                "LowTrade" ="#A3A3FF")

# Staple commodities
staples <- c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat")

# Only plot scenarios listed in scenario_order
prj_original <- prj
prj_list <- list()
for(i in scenario_order){prj_list[[i]] <- prj[[i]]}
prj <- prj_list
rm(prj_list)

# Figures ----------------------------------------------
## Total food demand ===================================
# Start by getting total pop and gdp
pop <- getQuery(prj, "population by region") %>%
  filter(region %in% regions_to_plot, year >= figure_start_year) %>%
  mutate(value = value * 1000) %>% # Convert from thous ppl to total ppl
  select(-Units) %>%
  rename(population = value)

gdp_pc <- getQuery(prj, "GDP per capita PPP by region") %>%
  filter(region %in% regions_to_plot, year >= figure_start_year) %>%
  rename(GDP_pc = value) %>%
  select(-Units)

# Get food demand and join with pop and income data
food_demand_pc_total <- getQuery(prj, "food demand") %>%
  filter(region %in% regions_to_plot, year >= figure_start_year) %>%
  select(-"gcam-consumer") %>%
  group_by(Units, scenario, region, nodeinput, input, year) %>%
  summarize(value = sum(value)) %>%
  left_join(pop, by = c("scenario", "region", "year")) %>%
  mutate(value = value * 1e9/365/population,
         Units = "Kcal/capita/day") %>%  # Convert from Pcal/year to Kcal/per/day
  left_join(gdp_pc, by = c("scenario", "region", "year"))

# Reorder scenario names for plot legends
food_demand_pc_total$scenario <- factor(food_demand_pc_total$scenario, levels = scenario_order)

# Plot
for (region_name in regions_to_plot) {

  # Plot TOTAL with GDP on x-axis
  g <- ggplot(filter(food_demand_pc_total, region == region_name)) +
    geom_line(aes(GDP_pc, value, color = scenario, linetype = scenario)) +
    theme_bw() +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    facet_wrap(~input) +
    ggtitle(paste0(region_name, " Food Consumption by GDP")) +
    labs(y = "kcal/capita/day",
         x = "per capita GDP (2015$)") +
  ggsave(filename = paste0("figures/food_demand/", region_name, "_FoodDemand_xGDP.png"),
         plot = g, width = 7.3, height = 3.2)

  # Plot with year on x-axis
  g <- ggplot(filter(food_demand_pc_total, region == region_name)) +
    geom_line(aes(year, value, color = scenario, linetype = scenario)) +
    theme_bw() +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    facet_wrap(~input) +
    ggtitle(paste0(region_name, " Food Consumption by Year")) +
    labs(y = "kcal/capita/day",
         x = "year") +
  ggsave(filename = paste0("figures/food_demand/", region_name, "_FoodDemand_xYear.png"),
         plot = g, width = 7.3, height = 3.2)
}

## Consumption =============================================
# Food consumption by commodity type (specific)
# join with population table to get per capita results
food_consump_type_pc <- getQuery(prj, "food consumption by type (specific)") %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  # Add staples vs nonstaples label
  mutate(sector = if_else(technology %in% staples, "FoodDemand_Staples", "FoodDemand_NonStaples")) %>%
  left_join(pop, by = c("scenario", "region", "year")) %>%
  mutate(value = value * 1e9/365/population,
         Units = "kcal/per/day") %>%
  select(-subsector...5) %>%
  rename(subsector = subsector...4)
food_consump_type_pc$scenario <- factor(food_consump_type_pc$scenario, levels = scenario_order)

# Plot
for (region_name in regions_to_plot) {
  # Separate by staples and nonstaples
  filter(food_consump_type_pc, region == region_name,
         sector == "FoodDemand_Staples") -> staples_consump_plot
  filter(food_consump_type_pc, region == region_name,
         sector == "FoodDemand_NonStaples") -> nonstaples_consump_plot

  # Plot: Staples consumption
  g <- ggplot(staples_consump_plot) + geom_line(aes(x=year, y=value, color = scenario, linetype = scenario)) +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    facet_wrap(~technology, scales = "free_y") +
    theme_bw() +
    labs(y = "kcal/per/day") +
    ggtitle(paste0(region_name, " Staples Consumption by Type"))
  ggsave(filename = paste0("figures/consumption/", region_name, "_stap_consump.png"),
         plot = g, width = 8.6, height = 5.3)

  # Plot: Non-Staples Consumption
  g <- ggplot(nonstaples_consump_plot) + geom_line(aes(x=year, y=value, color = scenario, linetype = scenario)) +
    facet_wrap(~technology, scales = "free_y") +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    theme_bw() +
    labs(y = "kcal/per/day") +
    ggtitle(paste0(region_name, " NonStaples Consumption by Type"))
  ggsave(filename = paste0("figures/consumption/", region_name, "_nonstap_consump.png"),
         plot = g, width = 8.6, height = 5.3)

  # Also do these as stacked bar charts
  # Staples
  g <- ggplot(staples_consump_plot) +
    geom_bar(aes(x=year, y=value, fill = technology), stat = "identity") +
    facet_wrap(~scenario, scales = "free_y") +
    scale_fill_manual(values = brewer.pal(5, "Set2")) +
    theme_bw() +
    labs(y = "kcal/per/day",
         title = (paste0(region_name, " Staples Consumption by Type"))) +
  ggsave(filename = paste0("figures/consumption/", region_name, "_stap_consump_BARCHART.png"),
         plot = g, width = 8.0, height = 5.3)

  # Non-staples
  g <- ggplot(nonstaples_consump_plot) + geom_bar(aes(x=year, y=value, fill = technology), stat = "identity") +
    facet_wrap(~scenario, scales = "free_y") +
    scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(17)) +
    theme_bw() +
    labs(y = "kcal/per/day",
         title = (paste0(region_name, " NonStaples Consumption by Type")))
  ggsave(filename = paste0("figures/consumption/", region_name, "_nonstap_consump_BARCHART.png"),
         plot = g, width = 8.0, height = 5.3)

  # Use rchart diff plots
  # Reformat data to match gcamextractor style, feed into rchart

  # Staples
  staples_bar_plot <- staples_consump_plot %>%
    rename(class = technology,
           x = year,
           classLabel = subsector,
           units = Units,
           param = sector) %>%
    mutate(xLabel = "Year")

  g <- rchart::chart(staples_bar_plot,
                     save = F,
                     scenRef = "SingleConsumer",
                     chart_type = "class_diff_absolute",
                     title = paste0(region_name, ": Staples Consumption Difference Between Reference and Other Scenarios"))

  ggsave(filename = paste0("figures/consumption/", region_name, "_stap_consump_BARCHART_diff_plot.png"),
         plot = g$chart_class_diff_absolute, width = 36, height = 6, units = "in")


  #Non-Staples
  nonstaples_bar_plot <- nonstaples_consump_plot %>%
    rename(class = technology,
           x = year,
           classLabel = subsector,
           units = Units,
           param = sector) %>%
    mutate(xLabel = "Year")

  g <- rchart::chart(nonstaples_bar_plot,
                     save = F,
                     scenRef = "SingleConsumer",
                     chart_type = "class_diff_absolute",
                     title = paste0(region_name, ": Non-Staples Consumption Difference Between Reference and Other Scenarios"))


  ggsave(filename = paste0("figures/consumption/", region_name, "_nonstap_consump_BARCHART_diff_plot.png"),
         plot = g$chart_class_diff_absolute, width = 36, height = 6, units = "in")

}

### Consumption by group #######################################
# Plot: Food Demand By Group
fd_groups <- getQuery(prj, "food demand") %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  rename(gcam.consumer = "gcam-consumer") %>%
  mutate(gcam.consumer = if_else(gcam.consumer == "FoodDemand", "FoodDemand_RefConsumer", gcam.consumer))

# TOTAL CALORIES: Add up nonstaples and staples calories by getting rid of input column and then summing
fd_groups_total <- fd_groups %>%
  select(-input) %>%
  group_by(scenario, region, gcam.consumer, Units, year, nodeinput) %>%
  summarize(value = sum(value)) %>%
  mutate(input = "Total")

# Bind nonstaples, staples, and total calories
fd <- bind_rows(fd_groups, fd_groups_total)

# Get GDP shares for each scenario
gdp_shares_groups <- getQuery(prj, "subregional income") %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  rename(gcam.consumer = "gcam-consumer") %>%
  mutate(gcam.consumer = if_else(gcam.consumer == "FoodDemand", "FoodDemand_RefConsumer", gcam.consumer)) %>%
  rename(GDP_pc = value)

# Get subregional population for each scenario
pop_groups <- getQuery(prj, "subregional population") %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  rename(gcam.consumer = "gcam-consumer") %>%
  mutate(gcam.consumer = if_else(gcam.consumer == "FoodDemand", "FoodDemand_RefConsumer", gcam.consumer)) %>%
  rename(population = value) %>%
  mutate(population = population*1000) #unit conversion from thous ppl to ppl

# Join with pop and GDP info
fd_pc <- fd %>%
  left_join(gdp_shares_groups, by=c("scenario", "region", "year", "gcam.consumer")) %>%
  left_join(pop_groups, by=c("scenario", "region", "year", "gcam.consumer")) %>%
  mutate(input = gsub("FoodDemand_", "", input)) %>%
  mutate(value = value/365 * 1e9 / population)
fd_pc$scenario <- factor(fd_pc$scenario, levels = scenario_order)

# For scales: making sure scale is the same across regions
max_staples <- max(filter(fd_pc, input == "Staples")$value)
min_staples <- min(filter(fd_pc, input == "Staples")$value)
max_nonstaples <- max(filter(fd_pc, input == "NonStaples")$value)
min_nonstaples <- min(filter(fd_pc, input == "NonStaples")$value)
max_total <- max(filter(fd_pc, input == "Total")$value)
min_total <- min(filter(fd_pc, input == "Total")$value)

# Sum per capita food demand for total demand
fd_pc_total <- fd_pc %>%
  select(-gcam.consumer, -Units.x, -Units.y, -nodeinput, -Units, -GDP_pc, -population) %>%
  group_by(scenario, region, year, input) %>%
  summarize(value = sum(value)) %>%
  mutate(value = if_else(grepl("Multiple | Trade", scenario), value/10, value)) %>%
  filter(input != "Total", region %in% regions_to_plot)
fd_pc_total$scenario <- factor(fd_pc_total$scenario, levels = scenario_order)

# Plot: facetted by region, so don't need a loop
# Staples:
g <- ggplot(filter(fd_pc_total, input == "Staples")) +
  geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  facet_wrap(~region) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  theme_bw() +
  labs(y = "kcal/per/day",
       title = "Staples Consumption")
ggsave("figures/consumption/staples_consumption.png", g, width = 7.2, height = 4.2)

# Non-Staples:
g <- ggplot(filter(fd_pc_total, input == "NonStaples")) + geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  facet_wrap(~region) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  theme_bw() +
  labs(y = "kcal/per/day",
       title = "NonStaples Consumption")
ggsave("figures/consumption/nonstaples_consumption.png", g, width = 7.2, height = 4.2)

# Same plot, but comparing staples and non staples on the same graph
# Staples consumption
g <- ggplot(filter(fd_pc_total, input == "Staples")) +
  geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  facet_wrap(~region) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  theme_bw() +
  labs(y = "kcal/per/day",
       title = "Staples Consumption")
ggsave("figures/consumption/staples_consumption_kcal_region.png", g, width = 7.2, height = 4.2)

# Non-Staples consumption
g <- ggplot(filter(fd_pc_total, input == "NonStaples")) +
  geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  facet_wrap(~region) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  theme_bw() +
  labs(y = "kcal/per/day",
       title = "NonStaples Consumption")
ggsave("figures/consumption/nonstaples_consumption_kcal_region.png", g, width = 7.2, height = 4.2)

# Total Consumption
fd_pc_total_cals <- fd_pc_total %>% mutate(input = "Total") %>%
  group_by(scenario, region, year, input) %>%
  summarize(value = sum(value))

g <- ggplot(fd_pc_total_cals) +
  geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  facet_wrap(~region) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  theme_bw() +
  labs(y = "kcal/per/day",
       title = "Total Calorie Consumption")
ggsave("figures/consumption/totalcal_consumption.png", g, width = 7.2, height = 4.2)

# Plot share of staples over total
staples_shares <- fd_pc_total %>%
  filter(input == "Staples") %>%
  left_join(fd_pc_total_cals, by = c("scenario", "region", "year")) %>%
  rename(staples = value.x,
         total = value.y) %>%
  select(-c(input.x, input.y)) %>%
  mutate(share = staples / total)

g <- ggplot(staples_shares, aes(year, share, color = scenario, linetype = scenario)) +
  geom_line() +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  facet_wrap(~region) +
  ylab("Fraction of total consumption") +
  ggtitle("Share of staples consumption over total consumption") +
  theme_bw()

ggsave("figures/consumption/staples_consumption_shares.png", g, width = 7.2, height = 4.2)

g <- ggplot(filter(fd_pc_total, input == "Staples")) +
  geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  facet_wrap(~region) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  theme_bw() +
  ggtitle("Staples Consumption") +
  ylab("Kcal/per/day")
ggsave("figures/consumption/staples_consumption.png", g, width = 7.2, height = 4.2)

### Consumption by group, by region ##################################
# Plot by groups
for (region_name in regions_to_plot) {

  # Filter for region
  fd_filtered <- fd_pc %>%
    filter(region == region_name)

  #Specify colors and line sizes
  colors_10 <- c(brewer.pal(n=9, "Set1"), "#66C2A5", "#000000") # plus black
  colors_10_noref <- c(brewer.pal(n=9, "Set1"), "#66C2A5")
  sizes_10 <- c(0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.7)
  sizes_10_noref <- c(0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4)


  # Order consumer groups correctly
  fd_filtered$gcam.consumer <- factor(fd_filtered$gcam.consumer,
                                      levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                 "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                 "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                 "FoodDemand_Group10", "FoodDemand_RefConsumer"))

  # Filter out ref consumer for some figures
  fd_filtered %>% filter(gcam.consumer != "FoodDemand_RefConsumer") -> fd_filtered_noref

  # Plot By Group
  g <- ggplot(fd_filtered) +
    geom_line(aes(GDP_pc, value, color = gcam.consumer, size = gcam.consumer), size = 1.5) +
    theme_bw() +
    scale_color_manual(values = colors_10) +
    scale_size_manual(values = sizes_10) +
    facet_grid(rows = vars(input), cols = vars(scenario), scales = "free") +
    labs(x = "per capita GDP (2015$)",
         y = "kcal/per/day",
         title = paste0(region_name, " Food Demand By Group"))

  g +
    facetted_pos_scales(
      y = list(
        input == "Staples" ~ scale_y_continuous(limits = c(round(min_staples/0.25)*0.25, round(max_staples/0.25)*0.25)),
        input == "NonStaples" ~ scale_y_continuous(limits = c(round(min_nonstaples/0.25)*0.25, round(max_nonstaples/0.25)*0.25)),
        input == "Total" ~ scale_y_continuous(limits = c(round(min_total/0.25)*0.25, round(max_total/0.25)*0.25))
      )
    ) -> g

  ggsave(filename = paste0("figures/consumption/", region_name, "_fd_bygroup_xgdp.png"),
         plot = g, width = 7.5, height = 5.5)

  # Facet vertically, regions side by side
  g <- ggplot(filter(fd_filtered, scenario %in% c("SingleConsumer", "MultipleConsumers"))) +
    geom_line(aes(GDP_pc, value, color = gcam.consumer, size = gcam.consumer), size = 1.5) +
    theme_bw() +
    scale_color_manual(values = colors_10) +
    scale_size_manual(values = sizes_10) +
    facet_wrap(~input, nrow = 3, scales = "free_y") +
    ggtitle(paste0(region_name, " Food Demand: Reference deciles")) +
    ylab("kcal/per/day") + xlab("per capita GDP (2015$)")

  g +
    facetted_pos_scales(
      y = list(
        input == "Staples" ~ scale_y_continuous(limits = c(round(min_staples/0.25)*0.25, round(max_staples/0.25)*0.25)),
        input == "NonStaples" ~ scale_y_continuous(limits = c(round(min_nonstaples/0.25)*0.25, round(max_nonstaples/0.25)*0.25)),
        input == "Total" ~ scale_y_continuous(limits = c(round(min_total/0.25)*0.25, round(max_total/0.25)*0.25))
      )
    ) -> g

  ggsave(filename = paste0("figures/consumption/", region_name, "_fd_bygroup_xgdp_ref_scenario.png"),
         plot = g, width = 7.5, height = 5.5)

  # Same thing: but w/o ref consumer
  g <- ggplot(fd_filtered_noref) +
    geom_line(aes(GDP_pc, value, color = gcam.consumer, size = gcam.consumer), size = 1.5) +
    theme_bw() +
    scale_color_manual(values = colors_10_noref) +
    scale_size_manual(values = sizes_10_noref) +
    facet_grid(rows = vars(input), cols = vars(scenario), scales = "free") +
    ggtitle(paste0(region_name, " Food Demand By Group")) +
    ylab("kcal/per/day") + xlab("per capita GDP (2015$)")

  g +
    facetted_pos_scales(
      y = list(
        input == "Staples" ~ scale_y_continuous(limits = c(round(min_staples/0.25)*0.25, round(max_staples/0.25)*0.25)),
        input == "NonStaples" ~ scale_y_continuous(limits = c(round(min_nonstaples/0.25)*0.25, round(max_nonstaples/0.25)*0.25)),
        input == "Total" ~ scale_y_continuous(limits = c(round(min_total/0.25)*0.25, round(max_total/0.25)*0.25))
      )
    ) -> g

  ggsave(filename = paste0("figures/consumption/", region_name, "_fd_bygroup_xgdp_noref.png"),
         plot = g, width = 6.9, height = 5.5)

  # Compute difference between ref consumer and each decile
  fd_ref_decile <- fd_filtered %>%
    select(-c(nodeinput, Units.y, Units, population)) %>%
    spread(scenario, value)

  # Isolate reference consumer information
  unique(fd_ref_decile$SingleConsumer)[2:19] -> ref_nonstaples
  unique(fd_ref_decile$SingleConsumer)[20:37] -> ref_staples
  unique(fd_ref_decile$SingleConsumer)[38:55] -> ref_total
  ref <- c(ref_nonstaples, ref_staples, ref_total)
  ref_10 <- rep_len(ref, length.out = 10 * length(ref))

  unique(fd_ref_decile$SingleConsumer_2p6)[2:19] -> policy_nonstaples
  unique(fd_ref_decile$SingleConsumer_2p6)[20:37] -> policy_staples
  unique(fd_ref_decile$SingleConsumer_2p6)[38:55] -> policy_total
  policy <- c(policy_nonstaples, policy_staples, policy_total)
  policy_10 <- rep_len(policy, length.out = 10 * length(policy))

  # Compute differences: single and multiple consumers
  fd_diff_ref_decile <- fd_ref_decile %>%
    select(-c(SingleConsumer, SingleConsumer_2p6, HighTrade, DefaultTrade, LowTrade)) %>%
    filter(gcam.consumer != "FoodDemand_RefConsumer") %>%
    mutate(SingleConsumer = ref_10,
           SingleConsumer_2p6 = policy_10,
           perdif_Ref = (MultipleConsumers - SingleConsumer)/SingleConsumer *100,
           perdif_2p6 = (MultipleConsumers_2p6 - SingleConsumer_2p6)/SingleConsumer_2p6 *100,
           dif_Ref = MultipleConsumers - SingleConsumer,
           dif_2p6 = MultipleConsumers_2p6 - SingleConsumer_2p6) %>%
    select(-SingleConsumer, -SingleConsumer_2p6, -MultipleConsumers, -MultipleConsumers_2p6) %>%
    gather(type, value, -c(region, year, Units.x, input, gcam.consumer, GDP_pc)) %>%
    filter(!is.na(value)) %>%
    separate(type, c("type", "policy"), remove = FALSE)

  # Plot - absolute differences by year
  g <- ggplot(filter(fd_diff_ref_decile,
                     type == "dif")) +
    geom_line(aes(year, value, color = gcam.consumer), size = 1) +
    theme_bw() +
    facet_grid(input~policy) +
    ggtitle(paste0(region_name, ": Difference in Consumption, Multiple vs. Single Consumer")) +
    ylab("Difference") +
    scale_color_manual(values = colors_10) +
    theme(plot.title = element_text(size=10)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

  ggsave(paste0("figures/consumption/", region_name, "_consumption_dif_xyear.png"), g, width = 6.9, height = 5.5)

  #...and by gdp
  g <- ggplot(filter(fd_diff_ref_decile,
                     type == "dif")) +
    geom_line(aes(GDP_pc, value, color = gcam.consumer), size = 1) +
    theme_bw() +
    facet_grid(input~policy) +
    ggtitle(paste0(region_name, ": Difference in Consumption, Ten vs. One Consumer")) +
    ylab("Difference") +
    xlab("per capita GDP (2015$)") +
    scale_color_manual(values = colors_10) +
    theme(plot.title = element_text(size=10)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

  ggsave(paste0("figures/consumption/", region_name, "_consumption_dif_xgdp.png"), g, width = 6.9, height = 5.5)

  # Plot - percent differences by year
  g <- ggplot(filter(fd_diff_ref_decile,
                     type == "perdif")) +
    geom_line(aes(year, value, color = gcam.consumer), size = 1) +
    theme_bw() +
    facet_grid(input~policy) +
    ggtitle(paste0(region_name, ": % Difference in Consumption, Ten vs. One Consumer")) +
    ylab("% Difference") +
    scale_color_manual(values = colors_10) +
    theme(plot.title = element_text(size=10)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

  ggsave(paste0("figures/consumption/", region_name, "_consumption_perdif_xyear.png"), g, width = 6.9, height = 5.5)

  #... and by gdp
  g <- ggplot(filter(fd_diff_ref_decile,
                     type == "perdif")) +
    geom_line(aes(GDP_pc, value, color = gcam.consumer), size = 1) +
    theme_bw() +
    facet_grid(input~policy) +
    ggtitle(paste0(region_name, ": % Difference in Consumption, Ten vs. One Consumer")) +
    ylab("% Difference") + xlab("per capita GDP (2015$)") +
    scale_color_manual(values = colors_10) +
    theme(plot.title = element_text(size=10)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

  ggsave(paste0("figures/consumption/", region_name, "_consumption_perdif_xgdp.png"), g, width = 6.9, height = 5.5)

  # Compute differences: multiple consumers, no trade v trade
  fd_diff_ref_trade <- fd_ref_decile %>%
    select(-c(SingleConsumer, SingleConsumer_2p6)) %>%
    filter(gcam.consumer != "FoodDemand_RefConsumer") %>%
    mutate(perdif_HighTrade = (HighTrade - MultipleConsumers)/MultipleConsumers *100,
           perdif_DefaultTrade = (DefaultTrade - MultipleConsumers)/MultipleConsumers *100,
           perdif_LowTrade = (LowTrade - MultipleConsumers)/MultipleConsumers *100,
           dif_HighTrade = HighTrade - MultipleConsumers,
           dif_DefaultTrade = DefaultTrade - MultipleConsumers,
           dif_LowTrade = LowTrade - MultipleConsumers) %>%
    select(-MultipleConsumers, -MultipleConsumers_2p6, -HighTrade, -DefaultTrade, -LowTrade) %>%
    gather(type, value, -c(region, year, Units.x, input, gcam.consumer, GDP_pc)) %>%
    filter(!is.na(value)) %>%
    separate(type, c("type", "policy"), remove = FALSE)

  # Plot - absolute differences by year
  g <- ggplot(filter(fd_diff_ref_trade,
                     type == "dif")) +
    geom_line(aes(year, value, color = gcam.consumer), size = 1) +
    theme_bw() +
    facet_grid(input~policy) +
    ggtitle(paste0(region_name, ": Difference in Consumption, Trade vs No Trade")) +
    ylab("Difference") +
    scale_color_manual(values = colors_10) +
    theme(plot.title = element_text(size=10)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

  ggsave(paste0("figures/consumption/", region_name, "_consumption_dif_trade_xyear.png"), g, width = 6.9, height = 5.5)

  #...and by gdp
  g <- ggplot(filter(fd_diff_ref_trade,
                     type == "dif")) +
    geom_line(aes(GDP_pc, value, color = gcam.consumer), size = 1) +
    theme_bw() +
    facet_grid(input~policy) +
    ggtitle(paste0(region_name, ": Difference in Consumption, Trade vs No Trade")) +
    ylab("Difference") + xlab("per capita GDP (2015$)") +
    scale_color_manual(values = colors_10) +
    theme(plot.title = element_text(size=10)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

  ggsave(paste0("figures/consumption/", region_name, "_consumption_dif_trade_xgdp.png"), g, width = 6.9, height = 5.5)

  # Plot - percent differences by year
  g <- ggplot(filter(fd_diff_ref_trade,
                     type == "perdif")) +
    geom_line(aes(year, value, color = gcam.consumer), size = 1) +
    theme_bw() +
    facet_grid(input~policy) +
    ggtitle(paste0(region_name, ": % Difference in Consumption, Trade vs No Trade")) +
    ylab("% Difference") +
    scale_color_manual(values = colors_10) +
    theme(plot.title = element_text(size=10)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

  ggsave(paste0("figures/consumption/", region_name, "_consumption_perdif_trade_xyear.png"), g, width = 6.9, height = 5.5)

  #... and by gdp
  g <- ggplot(filter(fd_diff_ref_trade,
                     type == "perdif")) +
    geom_line(aes(GDP_pc, value, color = gcam.consumer), size = 1) +
    theme_bw() +
    facet_grid(input~policy) +
    ggtitle(paste0(region_name, ": % Difference in Consumption, Trade vs No Trade")) +
    ylab("% Difference") + xlab("per capita GDP (2015$)") +
    scale_color_manual(values = colors_10) +
    theme(plot.title = element_text(size=10)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

  ggsave(paste0("figures/consumption/", region_name, "_consumption_perdif_trade_xgdp.png"), g, width = 6.9, height = 5.5)

  # Plot share of staples in total consumption by decile
  staples_shares_plot <-  fd_filtered %>%
    group_by(scenario) %>%
    filter(scenario %in% c("SingleConsumer", "MultipleConsumers")) %>%
    select(Units.x, scenario, region, gcam.consumer, input, year, value) %>%
    pivot_wider(names_from = input, values_from = value) %>%
    mutate(share = Staples / Total)

  staples_with_mean <- staples_shares_plot %>%
    filter(scenario == "MultipleConsumers") %>%
    group_by(scenario, region, year, Units.x) %>%
    summarize(share = mean(share)) %>%
    mutate(gcam.consumer = "MultipleConsumers_Average") %>%
    bind_rows(staples_shares_plot)

  staples_with_mean$gcam.consumer <- factor(staples_with_mean$gcam.consumer, levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                                                        "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                                                        "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                                                        "FoodDemand_Group10", "FoodDemand_RefConsumer", "MultipleConsumers_Average"))

  g <- staples_with_mean %>%
    ggplot() +
    geom_line(aes(year, share, color = gcam.consumer, size = gcam.consumer), size = 1.5) +
    theme_bw() +
    scale_color_manual(values = c(colors_10, "thistle1")) +
    scale_size_manual(values = c(sizes_10, "thistle1")) +
    ylim(0,1) +
    ggtitle(paste0(region_name, ": Share of staples consumption over total consumption"),
            subtitle = "Ten Consumers, Reference")

  ggsave(filename = paste0("figures/consumption/", region_name, "_fd_bygroup_xyear_ref_shares.png"),
         plot = g, width = 6.9, height = 5.5)

  # As a barchart
  shares_bar <- fd_filtered %>%
    filter(scenario %in% c("MultipleConsumers", "SingleConsumer")) %>%
    #, "TenConsumers_2p6")) %>%
    select(Units.x, scenario, region, gcam.consumer, input, year, value) %>%
    pivot_wider(names_from = input, values_from = value) %>%
    mutate(share = Staples / Total) %>%
    filter(year == 2020) # HARD CODED

  # Find and append the ten consumer average
  avg <- shares_bar %>%
    filter(scenario == "MultipleConsumers") %>%
    summarize(mean = mean(share))

  plot_data <- bind_rows(shares_bar, tibble("Units.x" = "Pcal/yr",
                                           "scenario" = "TenConsumers",
                                           "region" = region_name,
                                           "gcam.consumer" = as.factor("FoodDemand_MultipleConsumer_Average"),
                                           "year" = unique(shares_bar$year),
                                           "share" = avg$mean))

  g <-  plot_data %>%
    ggplot(aes(gcam.consumer, share, color = gcam.consumer, fill = gcam.consumer)) +
    #  stat_summary(geom="col",width=0.8,position=position_dodge()) +
    #  geom_col() +
    geom_bar(position="dodge", stat="identity") +
    theme_bw() +
    scale_color_manual(values = c(colors_10, "thistle1")) +
    scale_fill_manual(values = c(colors_10, "thistle1")) +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank()) +
    ggtitle(paste0(region_name, ": Share of staples in total consumption"),
            subtitle = paste0("Reference, ", unique(shares_bar$year))) +
    ylab("Share of total") +
    theme(axis.text.x = element_blank())

  ggsave(filename = paste0("figures/consumption/", region_name, "_fd_bygroup_xyear_ref_shares_BARCHART_", unique(plot_data$year), ".png"),
         plot = g, width = 6.9, height = 5.5)

  # Just one consumer and ten consumer
  single_staples_share <- staples_shares_plot %>%
    select(-c(NonStaples, gcam.consumer)) %>%
    filter(scenario == "SingleConsumer")

  multiple_staples_share <- staples_shares_plot %>%
    select(-NonStaples) %>%
    filter(scenario == "MultipleConsumers") %>%
    group_by(year, region) %>%
    mutate(Total = sum(Total),
           Staples = sum(Staples),
           share = Staples / Total) %>%
    select(-gcam.consumer)

  multiple_staples_share_clean <- multiple_staples_share[0:18, 0:7]

  shares <- bind_rows(single_staples_share, multiple_staples_share_clean)

  # Percent difference of shares
  # Reformat data to match gcamextractor style, feed into rchart
  # staples_perdiff <- fd_filtered_noref %>%
  #   filter(scenario %in% c("TenConsumers", "TenConsumers_2p6")) %>%
  #   select(Units.x, scenario, region, gcam.consumer, input, year, value) %>%
  #   pivot_wider(names_from = input, values_from = value) %>%
  #   mutate(share = Staples / Total) %>%
  #   filter(year == 2100,) %>%
  #   select(-c(Staples, NonStaples, Total)) %>%
  #   rename(# class = technology,
  #          x = year,
  #          # classLabel = subsector,
  #          units = Units.x,
  #          param = sector) %>%
  #   mutate(xLabel = "Year")
  #
  # g <- rchart::chart(staples_bar_plot,
  #                    save = F,
  #                    scenRef = "TenConsumers",
  #                    chart_type = "class_diff_absolute",
  #                    title = paste0(region_name, ": Staples Consumption Difference Between Reference and Other Scenarios"))
  #
  # ggsave(filename = paste0("figures/", region_name, "_stap_consump_BARCHART_diff_plot.png"),
  #        plot = g$chart_class_diff_absolute, width = 36, height = 6, units = "in")

}

## Food prices ====================================================
# Food Prices: Staples and NonStaples (NOT by commodity)
fd_prices <- getQuery(prj, "food demand prices") %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  mutate(value = value * gcamextractor::gdp_deflator(2015,2005),
         Units = "2015$/Mcal/Day") %>%
  mutate(input = gsub("FoodDemand_", "", input)) %>%
  rename(gcam.consumer = "gcam-consumer") %>%
  # Just filter for ref and group 1, since all 10 groups will have same food prices
  filter(gcam.consumer %in% c("FoodDemand", "FoodDemand_Group1"))
fd_prices$scenario <- factor(fd_prices$scenario, levels = scenario_order)


#Plot food prices for staples and nonstaples, facet by region
g <- ggplot(filter(fd_prices,
                   input == "Staples")) +
  geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  facet_wrap(~region) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  theme_bw() +
  ggtitle("Staples Food Prices") +
  ylab("2015$/Mcal/Day")
ggsave("figures/prices/staples_prices.png", g, width = 7.2, height = 4.2)

g <- ggplot(filter(fd_prices,
                   input == "NonStaples")) +
  geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  facet_wrap(~region) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  theme_bw() +
  ggtitle("NonStaples Food Prices") +
  ylab("2015$/Mcal/Day")
ggsave("figures/prices/nonstaples_prices.png", g, width = 7.2, height = 4.2)

# Also plot with faceted staples/nonstaples, and separate regions into two separate plots
for(region_name in regions_to_plot){
  filter(fd_prices, region == region_name) -> fd_prices_filtered
  g <- ggplot(fd_prices_filtered) + geom_line(aes(year, value, color = scenario, linetype = scenario)) +
    facet_wrap(~input, scales = "free_y") +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    theme_bw() +
    ylab("2015$/Mcal/Day") +
    ggtitle(paste0(region_name, " Food Prices"))
  ggsave(paste0("figures/prices/", region_name, "_food_prices.png"),
         plot = g, width = 6.5, height = 3.5)
}

# Calculate percent change and absolute differences
fd_prices_changes <- fd_prices %>%
  filter(year >= figure_start_year,
         region %in% regions_to_plot) %>%
  mutate(gcam.consumer = "FoodDemand") %>%
  spread(scenario, value) %>%
  mutate(perdif_Ref = (MultipleConsumers - SingleConsumer)/SingleConsumer *100,
         perdif_2p6 = (MultipleConsumers_2p6 - SingleConsumer_2p6)/SingleConsumer_2p6 *100,
         dif_Ref = MultipleConsumers - SingleConsumer,
         dif_2p6 = MultipleConsumers_2p6 - SingleConsumer_2p6) %>%
  select(-SingleConsumer, -SingleConsumer_2p6, -MultipleConsumers, -MultipleConsumers_2p6) %>%
  gather(type, value, -c(region, year, Units, gcam.consumer, nodeinput, input)) %>%
  filter(!is.na(value)) %>%
  separate(type, c("type", "policy"), remove = FALSE)

# Plot percent changes in prices
g <- ggplot(filter(fd_prices_changes,
                   type == "perdif")) +
  geom_line(aes(year, value, color = region)) +
  theme_bw() +
  facet_grid(input~policy) +
  ggtitle("Percent Difference in Food Prices, Ten vs. One Consumer") +
  ylab("% difference") +
  theme(plot.title = element_text(size=10)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

ggsave("figures/prices/prices_staples_nonstaples_perdif.png", g, width = 6.8, height = 4.2)

# Just staples
g <- ggplot(filter(fd_prices_changes,
                   type == "perdif",
                   input == "Staples")) +
  geom_line(aes(year, value, color = region)) +
  theme_bw() +
  facet_grid(input~policy) +
  ggtitle("Percent Difference in Staples Prices, Ten vs. One Consumer") +
  ylab("% difference") +
  theme(plot.title = element_text(size=10)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

ggsave("figures/prices/prices_staples_perdif.png", g, width = 6.8, height = 4.2)

# Just non-staples
g <- ggplot(filter(fd_prices_changes,
                   type == "perdif",
                   input == "NonStaples")) +
  geom_line(aes(year, value, color = region)) +
  theme_bw() +
  facet_grid(input~policy) +
  ggtitle("Percent Difference in Non-Staples Prices, Ten vs. One Consumer") +
  ylab("% difference") +
  theme(plot.title = element_text(size=10)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

ggsave("figures/prices/prices_nonstaples_perdif.png", g, width = 6.8, height = 4.2)

# Absolute difference
g <- ggplot(filter(fd_prices_changes,
                   type == "dif")) +
  geom_line(aes(year, value, color = region)) +
  theme_bw() +
  facet_grid(input~policy) +
  ggtitle("Difference in Food Prices, Ten vs. One Consumer") +
  ylab("Difference") +
  theme(plot.title = element_text(size=10)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")

ggsave("figures/prices/prices_staples_nonstaples_dif.png", g, width = 6.8, height = 4.2)

# Staple commodity prices
staples_commodity_prices <- getQuery(prj, "ag commodity prices") %>%
  filter(sector %in% staples ) %>%
  filter(year >= figure_start_year, region %in% regions_to_plot)
staples_commodity_prices$scenario = factor(staples_commodity_prices$scenario,
                                           levels=scenario_order)

for (region_name in regions_to_plot) {
  g <- ggplot(filter(staples_commodity_prices, region == region_name)) + geom_line(aes(year, value, color = sector)) +
    theme_bw() +
    scale_color_brewer(palette = "Set1") +
    facet_wrap(~scenario) +
    ylab("1975$/kg") +
    ggtitle(paste0(region_name, " Staples Commodity Prices"))

  ggsave(filename = paste0("figures/prices/", region_name, "_staples_prices.png"),
         plot = g, width = 8.0, height = 5.2)

}

## CO2 emissions ================================================
# FFI CO2
# Get regional CO2 emissions and then sum for global emissions
getQuery(prj, "CO2 emissions by region") %>%
  mutate(value = value*44/12, Units = "MtCO2") %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  select(-region) %>%
  group_by(Units, scenario, year) %>%
  summarize(value = sum(value)) -> ffico2_em

# LUC CO2
getQuery(prj, "LUC emissions by region") %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  select(-landleaf, -region) %>%
  group_by(year, Units, scenario) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(value = value * 44/12, Units = "MtCO2") -> luc_em

# Add together to get net CO2 emissions
left_join(ffico2_em, luc_em, by = c("year", "scenario", "Units")) %>%
  mutate(value = value.x + value.y) -> netco2_em

# Reorder scenario names for plot legends
ffico2_em$scenario <- factor(ffico2_em$scenario, levels = scenario_order)
luc_em$scenario <- factor(luc_em$scenario, levels = scenario_order)
netco2_em$scenario <- factor(netco2_em$scenario, levels = scenario_order)

# Plot Globally
#FFI CO2
g <- ggplot(ffico2_em) +
  geom_line(aes(x=year, y=value, color = scenario, linetype = scenario)) +
  theme_bw() +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  ylab("MtCO2") +
  ggtitle("Global FFI CO2 Emissions")
ggsave(filename = "figures/emissions/global_ffico2_em.png", plot = g, width = 6.8, height = 4.2)

#LUC CO2
g <- ggplot(luc_em) + geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  theme_bw() +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  ylab("MtCO2") + ggtitle("Global LUC CO2 Emissions")
ggsave(filename = "figures/emissions/global_lucem.png", plot = g, width = 6.8, height = 4.2)

#NET CO2
g <- ggplot(netco2_em) + geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  theme_bw() +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype) +
  ylab("MtCO2") + ggtitle("Global Net CO2 Emissions")
ggsave(filename ="figures/emissions/global_netco2_em.png", plot = g, width = 6.8, height = 4.2)

## Land allocation ==============================================
land_allocation_regional <- getQuery(prj, "aggregated land allocation") %>%
  filter(year >= figure_start_year,
         region %in% regions_to_plot)

land_allocation_global <- land_allocation_regional %>%
  select(-region) %>%
  group_by(Units, scenario, landleaf, year) %>%
  summarize(value = sum(value)) %>%
  mutate(region = "Global")

land_allocation <- bind_rows(land_allocation_global, land_allocation_regional)
land_allocation$scenario <- factor(land_allocation$scenario,
                                   levels = c("SingleConsumer", "MultipleConsumers",
                                              "SingleConsumer_2p6", "MultipleConsumers_2p6"))

# Filter for necessary land sectors
land_allocation %>%
  filter(landleaf %in% c("crops", "pasture (grazed)", "otherarable")) -> land_allocation

# Calulate percent dif in land cover
land_per_dif <- land_allocation %>%
  spread(scenario, value) %>%
  mutate(Reference = if_else(SingleConsumer != 0, (MultipleConsumers - SingleConsumer)/SingleConsumer*100, 0),
         RCP26 = if_else(SingleConsumer_2p6 != 0, (MultipleConsumers_2p6 - SingleConsumer_2p6)/SingleConsumer_2p6*100, 0)) %>%
  select(-SingleConsumer, -SingleConsumer_2p6, -MultipleConsumers, -MultipleConsumers_2p6) %>%
  gather(type, value, -c(region, year, Units, landleaf))

# Faceted Line Plots: (note: this could be put into for loop)
for(region_name in unique(land_allocation$region)) {
  # Filter by region
  filter(land_allocation, region == region_name) -> land_allocation_filtered
  filter(land_per_dif, region == region_name) -> land_per_dif_filtered

  # Plot
  g <- ggplot(land_allocation_filtered) + geom_line(aes(x=year, y=value, color = scenario, linetype = scenario)) +
    facet_wrap(~landleaf, scales = "free_y") +
    theme_bw() +
    ylab("thous km2") +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    ggtitle(paste0(region_name, " Land Allocation by Type"))
  ggsave(filename = paste0("figures/land/landalloc_,", region_name, ".png"),
         plot = g, width =7.1, height = 4.5)

  g <- ggplot(land_per_dif_filtered) +
    geom_line(aes(x=year, y=value, color = type)) +
    facet_wrap(~landleaf) +
    theme_bw() +
    ylab("% difference") +
    theme(plot.title = element_text(size=13)) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black") +
    ggtitle(paste0(region_name, " Percent Difference in Land, Ten v. One Consumer")) +
    theme(axis.title=element_text(size=14,face="bold"))
  ggsave(filename = paste0("figures/land/landalloc_dif_", region_name, ".png"),
         plot = g, width = 6.4, height = 4.7)
}

## Refining and water =============================================
refining <- getQuery(prj, "refined liquids production by tech") %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  spread(scenario, value) %>%
  mutate(perdif_Ref = (TenConsumers - OneConsumer)/OneConsumer *100,
         perdif_2p6 = (TenConsumers_2p6 - OneConsumer_2p6)/OneConsumer_2p6 *100,
         dif_Ref = TenConsumers - OneConsumer,
         dif_2p6 = TenConsumers_2p6 - OneConsumer_2p6) %>%
  select(-OneConsumer, -OneConsumer_2p6, -TenConsumers, -TenConsumers_2p6)%>%
  gather(type, value, -c(region, year, Units, sector, subsector, technology, output)) %>%
  filter(!is.na(value)) %>%
  separate(type, c("type", "policy"), remove = FALSE)
refining$policy <- factor(refining$policy, levels = c("Ref", "2p6"))
refinecolors <- colorRampPalette(brewer.pal(9, "Set1"))(14)

# Absolute dif
g <- ggplot(filter(refining, type == "dif")) + geom_bar(aes(year, value, fill = technology), stat = "identity") +
  facet_grid(cols = vars(region), rows = vars(policy)) +
  theme_bw() +
  scale_fill_manual(values = c(brewer.pal(9, "Set1"), brewer.pal(5, "Set2"))) +
  ggtitle("Difference in Refined Liquids Production, Ten v. One Consumer") +
  ylab("EJ")
ggsave("figures/refining/refined_liq.png", g, width = 7.1, height = 4.2)

# Percent Dif
g <- ggplot(filter(refining, type == "perdif")) + geom_bar(aes(year, value, fill = technology), stat = "identity") +
  facet_grid(cols = vars(region), rows = vars(policy)) +
  theme_bw() +
  scale_fill_manual(values = refinecolors) +
  ggtitle("Percent Difference in Refined Liquids Production, Ten v. One Consumer") +
  ylab("EJ")
ggsave("figures/refining/refined_liq.png", g, width = 7.1, height = 4.2)

# Irrigation Water, similar to above
water_query <- getQuery(prj, "irrigation water withdrawals by crop type")

water <- water_query %>%
  filter(year >= figure_start_year, region %in% regions_to_plot) %>%
  select(-sector) %>%
  group_by(Units, region, year, scenario) %>%
  summarize(value = sum(value)) %>%
  spread(scenario, value) %>%
  mutate(perdif_Ref = (TenConsumers - OneConsumer)/OneConsumer *100,
         perdif_2p6 = (TenConsumers_2p6 - OneConsumer_2p6)/OneConsumer_2p6 *100,
         dif_Ref = TenConsumers - OneConsumer,
         dif_2p6 = TenConsumers_2p6 - OneConsumer_2p6) %>%
  select(-OneConsumer, -OneConsumer_2p6, -TenConsumers, -TenConsumers_2p6) %>%
  gather(type, value, -c(region, year, Units)) %>%
  filter(!is.na(value)) %>%
  separate(type, c("type", "policy"), remove = FALSE)
water$policy <- factor(water$policy, levels = c("Ref", "2p6"))

# Plot PERCENT dif
g <- ggplot(filter(water, type == "perdif")) +
  geom_line(aes(year, value, color = region)) +
  theme_bw() +
  facet_wrap(~policy) +
  ggtitle("Percent Difference in Total Irrigation Water Withdrawals, Ten vs. One Consumer") +
  ylab("% difference") +
  theme(plot.title = element_text(size=10)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")
ggsave("figures/water/water_perdif.png", g, width = 6.8, height = 4.2)

#... and plot absolute differences
g <- ggplot(filter(water,
                   type == "dif")) +
  geom_line(aes(year, value, color = region)) +
  theme_bw() +
  facet_wrap(~policy) +
  ggtitle("Difference in Total Irrigation Water Withdrawals, Ten vs. One Consumer") +
  ylab("Difference") +
  theme(plot.title = element_text(size=10)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black")
ggsave("figures/water/water_dif.png", g, width = 6.8, height = 4.2)

# Plot global water withdrawals
water_region <- getQuery(prj,"irrigation water withdrawals by region")

water_global <- water_region %>%
  filter(year >= figure_start_year) %>%
  group_by(Units, year, scenario) %>%
  summarize(value = sum(value))

g <- ggplot(water_global) +
  geom_line(aes(year, value, color = scenario, linetype = scenario)) +
  theme_bw() +
  ggtitle("Global Water Withdrawals") +
  ylab("km^3") +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_linetype)

ggsave("figures/water/water_withdrawals_global.png", g, width = 6.8, height = 4.2)

# By region
for (region_name in regions_to_plot) {
  g <- ggplot(filter(water_region, region == region_name)) +
    geom_line(aes(year, value, color = scenario, linetype = scenario)) +
    theme_bw() +
    ggtitle(paste0(region_name, " Water Withdrawals")) +
    ylab("km^3") +
    theme(plot.title = element_text(size=10)) +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype)
  ggsave(paste0("figures/water/water_withdrawals_", region_name, ".png"), g, width = 6.8, height = 4.2)
}

## Production =============================================
# Ag production by commodity
ag_production <- getQuery(prj, "ag production by crop type") %>%
  filter(region %in% regions_to_plot,
         year >= figure_start_year)

ag_production_staples <- ag_production %>%
  filter(sector %in% staples)

ag_production_nonstaples <- ag_production %>%
  filter(sector %in% c("FiberCrop", "Fruits", "Legumes", "MiscCrop", "NutsSeeds",
                       "OilCrop", "OilPalm", "Soybean",
                       "SugarCrop", "Vegetables"))

meat_dairy_production <- getQuery(prj, "meat and dairy production by type") %>%
  filter(region %in% regions_to_plot,
         year >= figure_start_year)

ag_yield <- getQuery(prj, "ag tech yield") %>%
  filter(region %in% regions_to_plot,
         year >= figure_start_year,
         !(sector %in% c("biomass", "FodderGradd", "FodderHerb", "Forest", "Pasture", "UnmanagedLand"))) %>%
  group_by(scenario, region, year, sector) %>%
  summarise(value = sum(value)) %>%
  ungroup()


for (region_name in regions_to_plot) {
  # Production by staples
  g <- ggplot(filter(ag_production_staples, region == region_name)) +
    geom_line(aes(year, value, color = scenario, linetype = scenario)) +
    theme_bw() +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    facet_wrap(~sector, scales = "free_y") +
    ggtitle(paste0(region_name, " Agriculture Production by Commodity - Staples")) +
    ylab("Mt") + xlab("Year")
  ggsave(filename = paste0("figures/production/", region_name, "_ag_production_staples.png"),
         plot = g, width = 8.6, height = 5.3)

  # Production by nonstaples
  g <- ggplot(filter(ag_production_nonstaples, region == region_name)) +
    geom_line(aes(year, value, color = scenario, linetype = scenario)) +
    theme_bw() +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    facet_wrap(~sector, scales = "free_y") +
    ggtitle(paste0(region_name, " Agriculture Production by Commodity - Non-Staples")) +
    ylab("Mt") + xlab("Year")
  ggsave(filename = paste0("figures/production/", region_name, "_ag_production_nonstaples.png"),
         plot = g, width = 8.6, height = 5.3)

  # Meat and dairy production
  g <- ggplot(filter(meat_dairy_production, region == region_name)) +
    geom_line(aes(year, value, color = scenario, linetype = scenario)) +
    theme_bw() +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    facet_wrap(~sector, scales = "free_y") +
    ggtitle(paste0(region_name, " Agriculture Production by Commodity - Meat and Dairy")) +
    ylab("Mt") + xlab("Year")
  ggsave(filename = paste0("figures/production/", region_name, "_production_meat_dairy.png"),
         plot = g, width = 8.6, height = 5.3)

  # Ag yield
  g <- ggplot(filter(ag_yield, region == region_name, sector %in% staples)) +
    geom_line(aes(year, value, color = scenario, linetype = scenario)) +
    theme_bw() +
    scale_color_manual(values = scen_colors) +
    scale_linetype_manual(values = scen_linetype) +
    facet_wrap(~sector) +
    ggtitle(paste0(region_name, " Agriculture Yield by Commodity")) +
    ylab("Amount per area - FIX TODO") + xlab("Year")
  ggsave(filename = paste0("figures/production/", region_name, "_ag_yield.png"),
         plot = g, width = 8.6, height = 5.3)

}

## Trade ========================================================
# Read in production
# Ag production by commodity
production <- getQuery(prj, "ag production by crop type") %>%
  bind_rows(meat_dairy_production) %>%
  filter(region %in% regions_to_plot,
         year >= figure_start_year,
         Units == "Mt")

total_production <- production %>%
  group_by(scenario, region, year) %>%
  summarize(total_production = sum(value))

# Consumption
consumption_crop <- getQuery(prj, "demand balances by crop commodity") %>%
  filter(region %in% regions_to_plot,
         year >= figure_start_year,
         Units == "Mt")
consumption_meat_dairy <- getQuery(prj, "demand balances by meat and dairy commodity") %>%
  filter(region %in% regions_to_plot,
         year >= figure_start_year)

consumption <- bind_rows(consumption_crop, consumption_meat_dairy)

total_consumption <- consumption %>%
  group_by(scenario, region, year) %>%
  summarize(total_consumption = sum(value))

# Check output by tech
output <- getQuery(prj, "outputs by tech") %>%
  filter(region %in% regions_to_plot,
         year >= figure_start_year,
         sector %in% c("regional beef", "regional biomass", "regional biomassOil",
                       "regional corn", "regional corn for ethanol",
                       "regional dairy", "regional fibercrop", "regional fruits",
                       "regional legumes", "regional misccrop", "regional nuts_seeds",
                       "regional oilcrop", "regional oilpalm", "regional othergrain",
                       "regional pork", "regional poultry", "regional rice",
                       "regional root_tuber", "regional sheepgoat", "regional soybean",
                       "regional sugarcrop", "regional vegetables", "regional wheat"),
         grepl("imported", technology))

total_output <- output %>%
  group_by(scenario, region, year) %>%
  summarize(total_imports = sum(value))

equation <- left_join(total_production, total_consumption) %>%
  left_join(total_output) %>%
  mutate(total_exports = (total_production - total_consumption - total_imports)) %>%
  mutate(percent_share_imp_cons = (total_imports / total_consumption) * 100)

# Reorder scenarios for plotting
equation$scenario <- factor(equation$scenario, levels = scenario_order)

# Write csv
write.csv(equation, "cons_prod_im_ex.csv")

# Find trade by commodity - staples
cons_stap <- consumption %>%
  filter(input %in% c("regional corn", "regional othergrain",
                      "regional rice", "regional root_tuber",
                      "regional wheat", "regional sugarcrop")) %>%
  dplyr::mutate(input = dplyr::case_when(
    input == "regional corn" ~ "Corn",
    input == "regional othergrain" ~ "OtherGrain",
    input == "regional rice" ~ "Rice",
    input == "regional root_tuber" ~ "RootTuber",
    input == "regional wheat" ~ "Wheat",
    input == "regional sugarcrop" ~ "SugarCrop", T ~ input)) %>%
  rename(type = input,
         consumption = value) %>%
  select(-sector) %>%
  group_by(scenario, region, type, year) %>%
  mutate(consumption = sum(consumption)) %>%
  distinct(type, .keep_all = T)

prod_stap <- production %>%
  filter(output %in% c(staples, "SugarCrop")) %>%
  rename(type = output,
         production = value) %>%
  select(-sector)

output_stap <- output %>%
  filter(output %in% c("regional corn", "regional othergrain",
                       "regional rice", "regional root_tuber",
                       "regional wheat", "regional sugarcrop")) %>%
  dplyr::mutate(output = dplyr::case_when(
    output == "regional corn" ~ "Corn",
    output == "regional othergrain" ~ "OtherGrain",
    output == "regional rice" ~ "Rice",
    output == "regional root_tuber" ~ "RootTuber",
    output == "regional wheat" ~ "Wheat",
    output == "regional sugarcrop" ~ "SugarCrop", T ~ output)) %>%
  rename(imports = value,
         type = output) %>%
  select(-subsector, -technology, - sector) %>%
  group_by(scenario, region, type, year) %>%
  mutate(imports = sum(imports)) %>%
  distinct(type, .keep_all = TRUE)

equation_stap <- left_join(prod_stap, cons_stap) %>%
  left_join(output_stap) %>%
  ungroup() %>%
  mutate(exports = production - consumption - imports)

# Plot data for imports vs exports
imex <- equation %>%
  mutate(total_exports = total_exports * -1)

imex_diffs <- equation %>%
  select(-total_production, -total_consumption, -percent_share_imp_cons) %>%
  filter(scenario %in% c("OneConsumer", "OneConsumer_2p6", "TenConsumers", "TenConsumers_2p6")) %>%
  pivot_longer(c(total_imports, total_exports), names_to = "trade", values_to = "value") %>%
  spread(scenario, value) %>%
  mutate(perdif_OneConsumer = (OneConsumer_2p6 - OneConsumer)/OneConsumer *100,
         perdif_TenConsumers = (TenConsumers_2p6 - TenConsumers)/TenConsumers *100,
         dif_OneConsumer = OneConsumer_2p6 - OneConsumer,
         dif_TenConsumers = TenConsumers_2p6 - TenConsumers) %>%
  select(-OneConsumer, -OneConsumer_2p6, -TenConsumers, -TenConsumers_2p6) %>%
  gather(type, value, -c(region, year, trade)) %>%
  filter(!is.na(value)) %>%
  separate(type, c("type", "policy"), remove = FALSE)

# Plot imports vs exports
for(region_name in regions_to_plot) {
  g <- imex %>%
    filter(region == region_name) %>%
    ggplot(aes(year)) +
    geom_col(aes(y = total_exports, fill = "Exports")) +
    geom_col(aes(y = total_imports, fill = "Imports")) +
    facet_wrap(~scenario, nrow = 4, ncol = 2) +
    theme_bw() +
    scale_fill_manual(values = c("firebrick4", "darkblue"),
                      breaks = c("Imports", "Exports"),
                      labels = c("Imports", "Exports")) +
    labs(x = "Year",
         y = "Trade (Mt)",
         title = paste0(region_name, ": Imports and exports"),
         legend = "Trade")

  ggsave(filename = paste0("figures/trade/", region_name, "_imports_exports.png"), plot = g,
         width = 6, height = 9, units = "in")
}

# Plot imports vs exports by commodity
for(region_name in regions_to_plot) {
  g <- equation_stap %>%
    filter(region == region_name,
           scenario == "TenConsumers") %>%
    ggplot(aes(year)) +
    geom_col(aes(y = exports, fill = "Exports")) +
    geom_col(aes(y = imports, fill = "Imports")) +
    facet_wrap(~type, nrow = 4, ncol = 2) +
    theme_bw() +
    scale_fill_manual(values = c("firebrick4", "darkblue"),
                      breaks = c("Imports", "Exports"),
                      labels = c("Imports", "Exports")) +
    labs(x = "Year",
         y = "Trade (Mt)",
         title = paste0(region_name, ": Imports and exports"),
         subtitle = "Ten Consumers, Reference",
         legend = "Trade")

  ggsave(filename = paste0("figures/trade/", region_name, "_imports_exports_commodity.png"), plot = g,
         width = 6, height = 9, units = "in")
}

# Plot share of imports in total consumption
for(region_name in regions_to_plot){
  g <- equation %>%
    filter(region == region_name) %>%
    ggplot(aes(x = year, y = percent_share_imp_cons, fill = scenario)) +
    geom_col() +
    theme_bw() +
    facet_wrap(~scenario, nrow = 4, ncol = 2) +
    scale_fill_manual(values = scen_colors) +
    labs(x = "Year",
         y = "% share",
         title = paste0(region_name, ": Percent share of imports in total consumption")) +
    theme(legend.position = "none")

  ggsave(filename = paste0("figures/trade/", region_name, "_pershare_imp_cons.png"), plot = g,
         width = 6, height = 9, units = "in")
}

# Plot difference in imports vs exports
# % diff
for(region_name in regions_to_plot){
  g <- imex_diffs%>%
    filter(region == region_name,
           type == "perdif") %>%
    ggplot(aes(x = year, y = value, color = trade), size = 1) +
    geom_line() +
    geom_hline(aes(yintercept = 0), color = "black", linetype = "dotted") +
    theme_bw() +
    facet_wrap(~policy) +
    scale_color_manual(values = c("firebrick4", "darkblue"),
                      breaks = c("total_imports", "total_exports"),
                      labels = c("Imports", "Exports")) +
    labs(x = "Year",
         y = "% diff",
         title = paste0(region_name, ": Percent Difference in Imports and Exports"),
         subtitle = "Policy vs. Reference")

  ggsave(filename = paste0("figures/trade/", region_name, "_imports_exports_perdiff.png"), plot = g,
         width = 6.8, height = 4.2, units = "in")
}

# absolute diff
for(region_name in regions_to_plot){
  g <- imex_diffs%>%
    filter(region == region_name,
           type == "dif") %>%
    ggplot(aes(x = year, y = value, color = trade)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
    theme_bw() +
    facet_wrap(~policy) +
    scale_color_manual(values = c("firebrick4", "darkblue"),
                       breaks = c("total_imports", "total_exports"),
                       labels = c("Imports", "Exports")) +
    labs(x = "Year",
         y = "Difference",
         title = paste0(region_name, ": Difference in Imports and Exports"),
         subtitle = "Policy vs. Reference")

  ggsave(filename = paste0("figures/trade/", region_name, "_imports_exports_diff.png"), plot = g,
         width = 6.8, height = 4.2, units = "in")
}

# Plot production and consumption
for(region_name in regions_to_plot) {
  g <- equation %>%
    pivot_longer(c(total_production, total_consumption), names_to = "input", values_to = "value") %>%
    filter(region == region_name) %>%
    ggplot(aes(year)) +
    geom_line(aes(y = value, color = input), size = 1.5) +
    facet_wrap(~scenario, nrow = 4, ncol = 2) +
    theme_bw()  +
    scale_color_manual(values = c("firebrick4", "darkblue"),
                       labels = c("Consumption", "Production")) +
    labs(x = "Year",
         y = "Mt",
         title = paste0(region_name, ": Production and consumption"),
         legend = "Mt") ; g

  ggsave(filename = paste0("figures/trade/", region_name, "_production_consumption.png"), plot = g,
         width = 6, height = 9, units = "in")
}

# Plot production and consumption by commodity
for(region_name in regions_to_plot) {
  g <- equation_stap %>%
    pivot_longer(c(production, consumption), names_to = "input", values_to = "value") %>%
    filter(region == region_name,
           scenario %in% c("TenConsumers", "TenConsumers_2p6")) %>%
    ggplot(aes(year)) +
    geom_line(aes(y = value, color = input, linetype = scenario), size = 1.5) +
    facet_wrap(~type, nrow = 4, ncol = 2, scales = "free_y") +
    theme_bw() +
    scale_color_manual(values = c("firebrick4", "darkblue"),
                       labels = c("Consumption", "Production")) +
    labs(x = "Year",
         y = "Mt",
         title = paste0(region_name, ": Production and consumption"),
         subtitle = "Ten Consumers, Reference and Policy",
         legend = "Mt") ; g

  ggsave(filename = paste0("figures/trade/", region_name, "_production_consumption_commodity.png"), plot = g,
         width = 6, height = 9, units = "in")
}

