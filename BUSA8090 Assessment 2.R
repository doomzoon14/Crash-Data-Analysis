library("tidyverse")
install.packages("ggthemes")
library(ggthemes)


df <- read_csv("crash_incidents.csv") 
factor_vars <- c(
  "Crash_Severity", "Crash_Month", "Crash_Day_Of_Week", "Crash_Nature", "Crash_Type",
  "Crash_Street", "Loc_Suburb", "Loc_Local_Government_Area", "Loc_Police_Division", "Loc_Police_District", 
  "Loc_Police_Region", "Loc_Queensland_Transport_Region", "Loc_Main_Roads_Region", 
  "Loc_ABS_Statistical_Area_2", "Loc_ABS_Statistical_Area_3", "Loc_ABS_Statistical_Area_4",
  "Loc_ABS_Remoteness", "Loc_State_Electorate", "Loc_Federal_Electorate",
  "Crash_Controlling_Authority", "Crash_Roadway_Feature", "Crash_Traffic_Control", 
  "Crash_Speed_Limit", "Crash_Road_Surface_Condition", "Crash_Atmospheric_Condition", 
  "Crash_Lighting_Condition", "Crash_Road_Horiz_Align", "Crash_Road_Vert_Align",
  "Crash_DCA_Code", "Crash_DCA_Description", "Crash_DCA_Group_Description"
)

df <- df %>%
  mutate(across(all_of(factor_vars), as.factor))
view(head(df, 5))

#Removing Null Values
colSums(is.na(df))
df <- df %>% select(-"Crash_Street_Intersecting", -"State_Road_Name", -"DCA_Key_Approach_Dir")
colSums(is.na(df))
df <- na.omit(df)
colSums(is.na(df))
sum(is.na(df))

#Download Clean Data for Tableau
write.csv(df, "clean_crash_data.csv", row.names = FALSE)

#Total Crash Casualty By Hours they occurred
ggplot(data=df) +
  geom_col(mapping = aes(x = Crash_Hour, y = Count_Casualty_Total, fill = "Casualties")) + 
  scale_fill_manual(values = c("Casualties" = "red")) +
  xlab("Hours")+
  ylab("Casualty Total")+
  theme(plot.background=element_rect(fill='grey'))+
  theme(panel.grid.major=element_line(color="blue"))+
  theme(panel.grid.minor=element_line(color="purple"))+
  labs(fill = "legend")+
  annotate("text", label="Peak Hour", x=17,y=35000)+
  annotate("text", label="School Start Hour", x=7,y=30000)+
  ggtitle("Total Crash Casualty By the Hours they occured")

# Location of Multi-Vehicle Crashes during Peak Hour that resulted in hospitlisation on 2023

library(ggmap)
register_google(key="AIzaSyDqLmeXjRktRQ7NfZvKzJJgKWNqI1nqXFY")
qld <- geocode("Brisbane, Queensland")

df_m <- df %>% filter(Crash_Severity == "Hospitalisation", Crash_Type == "Multi-Vehicle",
                           Crash_Hour == "15", Crash_Year == "2023")

ggmap(get_map(qld, zoom = 10, maptype = "roadmap"))+
  geom_point(mapping=aes(x=Crash_Longitude, y=Crash_Latitude), color="red",data=df_m)+
  geom_text(aes(x = 153, y = -27.5, label = "CBD"), color = "black", size = 5)
  