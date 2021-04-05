# d is a data frame of the original dataset where rows are observations and columns are the variables 
d <- read_csv("Share of ICT goods as percentage of total trade, annual.csv")

# remove unnecessary columns that will not be used for data analysis:
# remove variables Partner and Partner Label, which contain the same values for all observations
if(length(unique(d$Partner)) == 1 & length(unique(d$`Partner Label`)) == 1) { 
  d_rm1 <- select(d, -Partner, -`Partner Label`)
}
# remove variables IctProductCategory and IctProductCategory Label, which contain the same values for all observations
if(length(unique(d$IctProductCategory)) == 1 & length(unique(d$`IctProductCategory Label`)) == 1) { 
  d_rm2 <- select(d_rm1, -IctProductCategory, -`IctProductCategory Label`, -X11) 
}

# round the percentage of total merchandise values to four sigdigs
d_rm2[["Percentage of total merchandise trade"]] <- round(d_rm2$`Percentage of total merchandise trade`, digits = 4)


# Aggrevate data for Canada and plot as a line graph, comparing imports and exports over the years
d_group_can <- filter(d_rm2, `Economy Label` == "Canada" & (`Flow Label` == "Imports" | `Flow Label` == "Exports"))
ggplot(d_group_can, aes(x = Year, y = `Percentage of total merchandise trade`, group = `Flow Label`, color = `Flow Label`)) +
  geom_line() +
  geom_point() +
  labs(title = "Canada's Share of ICT Goods in total merchandise imports and exports", subtitle = "year 2000 - 2019") +
  theme(plot.title = element_text(color = "blue", face = "bold.italic", hjust = 0.5), plot.subtitle = element_text(face = "italic", hjust = 0.5))
  
# Filter and plot only the countries in the European Union(2020...) in exports category 
eu <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
d_group_eu <- filter(d_rm2, `Economy Label` %in% eu)
d_group_eu_exp <- filter(d_group_eu, `Flow Label` == "Exports")

ggplot(d_group_eu_exp, aes(x = Year, y = `Percentage of total merchandise trade`)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap("`Economy Label`") +
  labs(title = "European Union: Share of ICT Goods in total merchandise exports", subtitle = "year 2000 - 2019") +
  theme(plot.title = element_text(color = "blue", face = "bold.italic", hjust = 0.5), plot.subtitle = element_text(face = "italic", hjust = 0.5))

# Filter and plot import data based on the classifications of economies based on income by World Bank 
wb <- c("High-income economies (World Bank)", "Upper-middle-income economies (World Bank)", "Low-income economies (World Bank)", "Lower-middle-income economies (World Bank)")
d_group_wb <- filter(d_rm2, `Economy Label` %in% wb)
d_group_wb <- filter(d_group_wb, `Flow Label` == "Imports")
ggplot(d_group_wb, aes(x = Year, y = `Percentage of total merchandise trade`, group = `Economy Label`, color = `Economy Label`)) +
  geom_line() +
  labs(title = "Share of ICT Goods in total merchandise imports", subtitle = "World Bank classification of economy by income level") +
  theme(plot.title = element_text(color = "blue", face = "bold.italic", hjust = 0.5), plot.subtitle = element_text(face = "bold.italic", hjust = 0.5))

