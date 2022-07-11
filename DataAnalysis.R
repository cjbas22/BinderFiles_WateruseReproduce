# Data Analysis
# Camilo B.
# 10/6/21 - modified: 04/25/22 
# - modified based on reviewers comments on:  07/07/2022


# Figure 2
ev %>%
  filter(Label != 'irrigation') %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(sda = paste0(.$Site, .$dt)) %>%
  filter(sda %in% dayp$fd) %>% # filter only full days
  add_column(oc = sites$N_Residents[match(.$Site, sites$SiteID)]) %>% # ocupants per site
  group_by(Site, dt, oc) %>%
  summarise(dailyvol = sum(OriginalVolume)) %>% # daily volume 
  mutate(dvolpc = dailyvol/oc) %>% # daily volume per  capita
  group_by(Site) %>%
  summarise(advolpc = mean(dvolpc) * gl) -> ind_pcd # indoor per capita a day

ind_pcd %>%
  mutate(group_LC = case_when(advolpc < quantile(ind_pcd$advolpc, 0.33) ~ 'Low',
                              advolpc > quantile(ind_pcd$advolpc, 0.33) & advolpc < quantile(ind_pcd$advolpc, 0.66)  ~ 'Medium',
                              advolpc > quantile(ind_pcd$advolpc, 0.66) ~ 'High')) -> ind_pcd

ev %>%
  filter(Label != 'irrigation') %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(sda = paste0(.$Site, .$dt)) %>%
  filter(sda %in% dayp$fd) %>% # filter only full days
  group_by(Site, dt, Label) %>%
  summarise(v = sum(OriginalVolume), n = n()) %>% # daily vol per capita 
  mutate(vpo = v/n) %>% # volume per ocurrence
  group_by(Site, Label) %>%
  summarise(avpo = mean(vpo) * gl) -> avpo #%>% # average volume per ocurrence

avpo$Label = factor(avpo$Label, levels = c("faucet", "shower", "toilet", "bathtub", "clotheswasher")) # Fix later
# average volume per occurrence

colsp1 <- c("faucet" = "#44AA99", "shower" = "#DDCC77", "toilet" = "#CC6677", "bathtub" = "#88CCEE", "clotheswasher" = "#882255")
labelsp1 <- c("Faucet", "Shower", "Toilet", "Bathtub", "Clothes Washer")

avpo %>%
  arrange(Site,Label) %>%
  spread(Label, avpo) %>%
  gather(Label, value, faucet:clotheswasher, factor_key = TRUE) %>%
  add_column(groupLC = ind_pcd$group_LC[match(.$Site, ind_pcd$Site)]) %>%
  group_by(groupLC, Label) %>%
  summarise(mwug = mean(value, na.rm = TRUE)) %>% # average water use per label and group
  mutate(groupLC = factor(groupLC, levels = c("Low", "Medium", "High"))) %>% # -> perc_values_test
  ggplot(aes(groupLC, mwug, fill = Label)) +
  #ggplot(aes(groupLC, mwug/gl, fill = Label)) +
  geom_bar(position="stack", stat="identity", width = 0.8) +
  scale_fill_manual(values = colsp1, labels = labelsp1) + 
  xlab("") + ylab("Volume (L)") +
  #xlab("") + ylab("Volume (gal)") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  theme_bw() + #theme(legend.position = "top", legend.key.width= unit(1, 'cm')) 
  theme(legend.position = "none") +
  labs(subtitle = "a)") -> np1 # new plot 1 - grouped


# Percentage differences - View values
avpo %>%
  arrange(Site,Label) %>%
  spread(Label, avpo) %>%
  gather(Label, value, faucet:clotheswasher, factor_key = TRUE) %>%
  mutate(Site = factor(Site, levels = ind_pcd$Site[order(ind_pcd$advolpc)])) %>% # define levels for the barplot
  add_column(groupLC = ind_pcd$group_LC[match(.$Site, ind_pcd$Site)]) %>%
  group_by(groupLC, Label) %>%
  summarise(mwug = mean(value, na.rm = TRUE)) %>% # average water use per label and group
  mutate(groupLC = factor(groupLC, levels = c("Low", "Medium", "High"))) %>%
  filter(Label == 'faucet')
  #filter(Label == 'clotheswasher')
  #filter(Label == 'toilet')
  #filter(Label == 'shower')
  #filter(Label == 'bathtub')

# number of events #adnepc[adnepc$Site == 9 & adnepc$Label == 'faucet',]$anepcd = NA
# Number of events per capita a day
ev %>%
  filter(Label != 'irrigation') %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(sda = paste0(.$Site, .$dt)) %>%
  filter(sda %in% dayp$fd) %>% # filter only full days
  add_column(oc = sites$N_Residents[match(.$Site, sites$SiteID)]) %>% # ocupants per site
  add_column(ndays = dayp$nd[match(.$Site, dayp$Site)]) %>%
  group_by(Site, Label, ndays, oc) %>%
  summarise(tne = n()) %>% # Total number of events
  mutate(anepcd = tne/(ndays*oc)) %>% # average number of events per capita a day
  ungroup() %>%
  select(Site, Label, anepcd) -> adnepc

colsp1 <- c("faucet" = "#44AA99", "shower" = "#DDCC77", "toilet" = "#CC6677", "bathtub" = "#88CCEE", "clotheswasher" = "#882255")
adnepc %>%
  arrange(Site,Label) %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  spread(Label, anepcd) %>%
  mutate(Site = factor(Site, levels = ind_pcd$Site[order(ind_pcd$advolpc)])) %>% # define levels for the barplot
  gather(Label, value, bathtub:toilet, factor_key = TRUE) %>%
  add_column(groupLC = ind_pcd$group_LC[match(.$Site, ind_pcd$Site)]) %>%
  group_by(groupLC, Label) %>%
  summarise(mnepc = round(mean(value, na.rm = TRUE),2)) %>% # average number of venets per capita per grou and label
  mutate(groupLC = factor(groupLC, levels = c("Low", "Medium", "High"))) %>%
  mutate(Label  = factor(Label , levels = c("faucet", "shower", "toilet", "bathtub", "clotheswasher"))) %>% # define levels for the barplot
  ggplot(aes(groupLC, mnepc, fill = Label)) +
  geom_bar(position="stack", stat="identity", width = 0.8) +
  scale_fill_manual(values = colsp1) + 
  xlab("Group") + ylab("Number of events") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  theme_bw() + theme(legend.position = "none") +
  theme(legend.position = "none") +
  labs(subtitle = "b)") -> np2

# daily per capita average per label
ev %>%
  filter(Label != 'irrigation') %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  #filter(Site == id) %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(sda = paste0(.$Site, .$dt)) %>%
  filter(sda %in% dayp$fd) %>% # filter only full days
  add_column(oc = sites$N_Residents[match(.$Site, sites$SiteID)]) %>% # ocupants per site
  group_by(Site, Label, dt, oc) %>%
  summarise(dailyvol = sum(OriginalVolume)) %>% # daily volume 
  mutate(dvolpc = dailyvol/oc) %>% # daily volume per  capita
  group_by(Site, Label) %>%
  summarise(advolpc = mean(dvolpc) * gl) -> davpcpl # Daily volume per capita per label

davpcpl %>%
  arrange(Site,Label) %>%
  add_column(groupLC = ind_pcd$group_LC[match(.$Site, ind_pcd$Site)]) %>%
  group_by(groupLC, Label) %>%
  summarise(totvol = sum(advolpc)) %>%
  spread(Label, totvol) %>%
  rowwise() %>%
  mutate(total = sum(c(faucet,shower,toilet,bathtub,clotheswasher), na.rm = TRUE)) %>%
  mutate(fp = faucet / total) %>%
  mutate(sp = shower / total) %>%
  mutate(tp = toilet / total) %>%
  mutate(bp = bathtub / total) %>%
  mutate(cwp = clotheswasher / total) %>% 
  select(groupLC, fp,sp,tp,bp,cwp) -> ind_perc_g

ev %>%
  filter(Label != 'irrigation') %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(sda = paste0(.$Site, .$dt)) %>%
  filter(sda %in% dayp$fd) %>% # filter only full days
  add_column(oc = sites$N_Residents[match(.$Site, sites$SiteID)]) %>% # ocupants per site
  group_by(Site, dt, oc) %>%
  summarise(dailyvol = sum(OriginalVolume)) %>% # daily volume per label
  mutate(dvolpc = dailyvol/oc) %>% # daily volume per label per capita
  group_by(Site) %>%
  summarise(advolpc = mean(dvolpc) * gl) %>% #-> davpcpl # daily average volume per capita per label
  add_column(groupLC = ind_pcd$group_LC[match(.$Site, ind_pcd$Site)]) %>%
  group_by(groupLC) %>%
  summarise(advol_g = mean(advolpc)) -> ind_pcd_g # indoor per capita a day Liter

bind_cols(ind_pcd_g, ind_perc_g, .name_repair = "minimal") %>%
  select(-1) %>%
  rowwise() %>%
  mutate(faucet = advol_g*fp) %>%
  mutate(shower = advol_g*sp) %>%
  mutate(toilet = advol_g*tp) %>%
  mutate(bathtub = advol_g*bp) %>%
  mutate(clotheswasher = advol_g*cwp) %>% 
  select(groupLC, faucet, shower, toilet, bathtub, clotheswasher) %>%
  gather(Label, value, faucet:clotheswasher, factor_key = TRUE) %>%
  mutate(groupLC = factor(groupLC, levels = c("Low", "Medium", "High"))) %>% 
  ggplot(aes(groupLC, value, fill = Label)) +
  #ggplot(aes(groupLC, value/gl, fill = Label)) +
  geom_bar(position="stack", stat="identity", width = 0.8) +
  scale_fill_manual(values = colsp1, labels = labelsp1) + 
  xlab("") + ylab("Volume (L)") +
  #xlab("") + ylab("Volume (gal)") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  theme_bw() + 
  labs(subtitle = "c)") -> np3

grid.arrange(np1, np2, np3, nrow = 1, widths = c(0.27,0.27,0.46)) -> np
ggsave("Figur2_IndoorStats_Grouped.png",plot = np, width = 8, height = 8*0.4, units = "in")




# Figure 3
ev %>%
  filter(Label != 'irrigation') %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  add_column(h = hour(.$StartTime)) %>%
  group_by(Site, h) %>%
  summarise(hwu = sum(OriginalVolume)) %>% # hourly aggregated volumes
  group_by(Site) %>%
  mutate(totwu = sum(hwu)) %>%
  mutate(hpwu = hwu*100/totwu) %>%# percentage of hourly water use
  ungroup() %>%
  add_row(Site = 18, h = 3, hpwu = 0) %>%
  add_row(Site = 23, h = 3, hpwu = 0) %>%
  add_row(Site = 25, h = 3, hpwu = 0) %>%
  ggplot(aes(h, as.factor(Site), fill = hpwu)) +
  geom_tile() + 
  #theme_ipsum() +
  scale_x_continuous(breaks = seq(0, 23, by = 2), expand = c(0, 0)) +
  #scale_fill_gradientn(breaks = c(0,2,4,6,8,10,15,20)) +
  scale_fill_viridis(direction = -1, option = "mako") + 
  xlab("Hour") + ylab("Site") +
  labs(fill = "Percentage of total water use (%)") +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  theme(legend.position="top", legend.key.height= unit(0.4, 'cm'), legend.key.width= unit(2.5, 'cm'))
ggsave("Figure3_TotalHourlyWU_selected.png", width = 8, height = 8, units = "in")  



# Outdoor Water Use
# weeks of data during dummer months

# percentage of the volume used for irrigation
ev %>% 
  filter(month(.$StartTime) %in% c(5,6,7,8,9,10)) %>%
  mutate(totvol = sum(OriginalVolume)) %>% # toatl volume collected may-oct
  filter(Label == 'irrigation') %>%
  mutate(outvol = sum(OriginalVolume)) %>% # irrigation volume
  slice(1) %>%
  summarise(outvol*100/totvol) # percentage of the volume collected used for irrigation

# Volume collected between May-October
ev %>%
  filter(month(.$StartTime) %in% c(5,6,7,8,9,10)) %>%
  summarise(sum(OriginalVolume) * gl)

# number of weeks of data collected between May-Oct
#qc %>%
ev %>%
  filter(month(.$StartTime) %in% c(5,6,7,8,9,10)) %>%
  add_column(wk = isoweek(.$StartTime)) %>%
  select(Site, wk) %>% # weeks per site
  distinct() %>% # select unique values only
  #group_by(Site) %>% 
  summarise(n = n()) # number of weeks per site
  
  


# Figure 4 weekly outdoor stat
# Outdoor water use stats
ev %>%
  filter(Site != 21) %>%
  filter(Site != 22) %>%
  filter(Label == "irrigation") %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(wk = isoweek(.$dt)) %>%
  add_column(y = year(.$StartTime)) %>%
  filter(month(dt) %in% c(4,5,6,7,8,9,10)) %>%
  mutate(ws = paste0(Site,"_", wk,"_", y)) %>% # Site Date Week - to filter full weeks only
  filter(ws %in% fws$fws) %>% # Filter only full weeks of data
  group_by(Site, wk, y) %>%
  summarise(wkoutuse = sum(OriginalVolume) * 0.133681 ) %>% # weekly outdoor volume - in ftˆ3
  add_column(IrrAr = sites$Irr_Area[match(.$Site, sites$SiteID)]) %>%
  mutate(lndwuse = (wkoutuse / IrrAr) * 12) %>% # Landscape Weekly Water Use - inches
  add_column(city = sites$City[match(.$Site, sites$SiteID)]) %>%
  mutate(ft_id = paste0(city,"_", wk,"_", y)) %>% # Column to match and add LWNeed
  add_column(LWND = wd$lndwneed[match(.$ft_id, wd$ftid)]) %>% # Weekly Landscape Water Needs for each city - inches
  mutate(LWND = replace(LWND, which(LWND < 0), 0)) %>% # Replace negative landscape needs with 0 
  add_column(IrrA =sites$Irr_Area[match(.$Site, sites$SiteID)]) %>%
  add_column(IrrType = sites$IrrigationType[match(.$Site, sites$SiteID)]) %>%
  #group_by(Site) %>% mutate(n = n()) %>% # number of weeks per site
  mutate(LIR = lndwuse/LWND) -> OutWuStats


# Plot 1 - Number of 
OutWuStats %>%
  filter(LWND > 0) %>% #View()
  mutate(n = n()) %>% # number of weeks per site
  #group_by(Site, wk) %>%
  filter(row_number()==1) %>%
  ggplot(aes(as.factor(Site), n)) + geom_col() +
  scale_y_continuous(expand = c(0, 0.01), limits = c(0,12.9), breaks = seq(0,12,3)) +
  facet_grid(~ IrrType, scales = "free", space = "free_x") +
  #labs(y = "Number of Weeks", subtitle = "a)") +
  labs(y = "Number of Weeks") +
  theme(plot.subtitle=element_text(size=9)) +
  theme(axis.title.x=element_blank()) -> poutn1

# Plot - Area
OutWuStats %>%
  filter(LWND > 0) %>% #View()
  group_by(Site, IrrType) %>%
  filter(row_number()==1) %>%
  ggplot(aes(as.factor(Site), IrrAr * sqft_sqm)) + geom_col() + # Irr Area in m2
  #ggplot(aes(as.factor(Site), IrrAr/1000)) + geom_col() + # Irr Area in ft2
  scale_y_continuous(expand = c(0.01, 0.01)) +
  facet_grid(~ IrrType, scales = "free", space = "free_x") +
  #labs(y = bquote('Landscape Area'~(m^2)), subtitle = "b)") +
  labs(y = bquote('Landscape Area'~(m^2))) +
  #labs(y = bquote('Landscape Area'~(10^3 ~ ftˆ2))) +
  theme(plot.subtitle=element_text(size=9)) +
  theme(axis.title.x=element_blank()) -> poutn2


# Plot - Average volume
OutWuStats %>%
  filter(LWND > 0) %>% #View()
  group_by(Site, IrrType) %>% 
  summarise(awvol = mean(wkoutuse) * cf_l / 1000) %>% # average weekly outdoor use in m3 -> FinLIR #->  FinLIR
  ggplot(aes(as.factor(Site), awvol)) + geom_col() + 
  #ggplot(aes(as.factor(Site), awvol * 264.172 / 1000)) + geom_col() +  # average weekly outdoor use in 1000 gal
  scale_y_continuous(expand = c(0.01, 0.5)) +
  facet_grid(~ IrrType, scales = "free", space = "free_x") +
  #labs(y = bquote('Volume'~(m^3)), subtitle = "c)") +
  labs(y = bquote('Volume'~(m^3))) +
  #labs(y = bquote('Volume'~(10^3 ~ gal))) +
  theme(plot.subtitle=element_text(size=9)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  theme(axis.title.x=element_blank()) -> poutn3

# Plot - LIR
OutWuStats %>%
  filter(LWND > 0) %>% # FinLIR #
  ggplot(aes(as.factor(Site), LIR)) + geom_point(shape = 3, size = 3) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.25)) +
  facet_grid(~ IrrType, scales = "free", space = "free_x") +
  geom_hline(yintercept = 1, linetype = 2, colour = "deepskyblue3") +
  geom_hline(yintercept = 2, linetype = 2, colour = "forestgreen") +
  geom_hline(yintercept = 3, linetype = 2, colour = "brown3") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  theme(plot.subtitle=element_text(size=9)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin= -Inf, ymax=1, fill= '#88CCEE', alpha= 0.25) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin= 1, ymax=2, fill= '#117733', alpha= 0.25) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin= 2, ymax=3, fill= '#DDCC77', alpha= 0.25) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin= 3, ymax=Inf, fill= '#882255', alpha= 0.25) +
  #labs(y = "LIR", subtitle = "d)", x = "Site") -> poutn4
  labs(y = "LIR", x = "Site") -> poutn4

#plot_grid(poutn1, poutn2, poutn3, poutn4, ncol=1, align="v", rel_heights = c(0.2, 0.2, 0.2, 0.4)) -> poutn
plot_grid(poutn2, poutn3, poutn4, ncol=1, align="v", rel_heights = c(0.3, 0.3, 0.4)) -> poutn
poutn
ggsave("Figure4_OutUse_Complete.png", poutn, width = 9, height = 6, units = "in")


OutWuStats %>%
  filter(LWND > 0) %>% # FinLIR #
  filter(LIR > 3) # excesive water use


# Average duration of Irrigation events
ev %>%
  filter(Site != 21) %>%
  filter(Site != 22) %>%
  filter(Label == "irrigation") %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(wk = isoweek(.$StartTime)) %>%
  add_column(y = year(.$StartTime)) %>%
  filter(month(dt) %in% c(4,5,6,7,8,9,10)) %>%
  summarise(mean(Duration)) # Average duration of irrigation events

# Max flow rate - Irrigation events
ev %>%
  filter(Site != 21) %>%
  filter(Site != 22) %>%
  filter(Label == "irrigation") %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(wk = isoweek(.$StartTime)) %>%
  add_column(y = year(.$StartTime)) %>%
  filter(month(dt) %in% c(4,5,6,7,8,9,10)) %>%
  filter(Duration > 30) %>%
  group_by(Site) %>%
  summarise(mxfr = max(OriginalFlowRate) *gl) %>% arrange(desc(mxfr)) %>% print( n = 30)



# Figure 4
# Weeks where LIR is undefined
OutWuStats %>% ungroup() %>%
  filter(LWND == 0) %>%
  select(y, wk) %>%
  distinct()

# ranking of users based on IrrArea
sites %>%
  select(SiteID, Irr_Area, IrrigationType) %>%
  arrange(Irr_Area) %>%
  mutate(n = 1:31) %>% print(n = 32)

# Plot 1 - Number of 
OutWuStats %>%
  filter(LWND == 0) %>% #View()
  mutate(n = n()) %>% # number of weeks per site
  filter(row_number()==1) %>%
  ggplot(aes(as.factor(Site), n)) + geom_col() +
  facet_grid(~ IrrType, scales = "free", space = "free_x") +
  labs(y = "Number of Weeks") +
  scale_y_continuous(limits = c(0,5.3), expand = c(0,0)) +
  theme(plot.subtitle=element_text(size=9)) +
  theme(plot.tag.position = c(0.08,0.88), plot.tag = element_text(hjust =0, size=11)) +
  labs(tag = "a)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) -> poutInf1

# Plot - Average volume
OutWuStats %>%
  filter(LWND == 0) %>% #View()
  group_by(Site, IrrType) %>%
  summarise(awvol = mean(wkoutuse) * cf_l / 1000) %>% # average weekly outdoor use in m3 -> InfLIR #
  ggplot(aes(as.factor(Site), awvol)) + geom_col() +
  scale_y_continuous(expand = c(0, 0.5)) +
  facet_grid(~ IrrType, scales = "free", space = "free_x") +
  labs(y = bquote('Volume'~(m^3)), x = "Site") +
  theme(plot.subtitle=element_text(size=9)) +
  theme(plot.tag.position = c(0.08,0.93), plot.tag = element_text(hjust =0, size=11)) +
  labs(tag = "b)") +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) -> poutInf2

plot_grid(poutInf1, poutInf2, ncol=1, align="v") -> outInf
outInf
ggsave("Figure5_OutUse_Complete_LIR-INF.png", outInf, width = 9, height = 3, units = "in")


# Differences between outdoor volume in weeks where LIR is finite vs weeks when LIR is infinite
OutWuStats %>%
  filter(LWND == 0) %>% #View()
  group_by(Site, IrrType) %>%
  summarise(awvol = mean(wkoutuse) * cf_l / 1000) -> InfLIR #
  
OutWuStats %>%
  filter(LWND > 0) %>% 
  group_by(Site, IrrType) %>% 
  summarise(awvol = mean(wkoutuse) * cf_l / 1000) -> FinLIR
  
left_join(InfLIR, FinLIR, by = c('Site', 'IrrType')) %>% 
  rename(volInf = awvol.x, volFn = awvol.y) %>%
  mutate(Dif = (volInf - volFn)*100/volFn) #%>%
  #ungroup() %>% summarise(sum(volInf))


# Figure 6
# Analysis of monthly outdoor water use.
# Winter months
MonthlyWUD %>%
  filter(Consumption > 0) %>% # remove months with no water use 
  filter(month(.$Date) %in% c(11,12,1,2,3)) %>%
  group_by(City, SiteID) %>% summarise(wau = sum(Consumption)/n(), nwm = n()) %>% 
  ungroup() %>% group_by(City) %>% # au is the average monthly use for each City, Site and Year
  filter(nwm > 2) -> MWinterAve # monthly average water use during winter months - removing users with less than 2 months

# Summer months
MonthlyWUD %>%
  filter(Consumption > 0) %>%  # remove months with no water use 
  filter(month(.$Date) %in% c(5,6,7,8,9,10)) %>%
  group_by(City, SiteID) %>% summarise(sau = sum(Consumption)/n(), nsm = n()) %>% 
  ungroup() %>% group_by(City) %>% # au is the average monthly use for each City, Site and Year
  filter(nsm > 5) -> MSummerAve # monthly average water use during winter months - removing users with less than 5 months

full_join(MSummerAve, MWinterAve, by=c("City","SiteID")) %>%
  #drop_na() %>%
  filter(SiteID != 0) %>% # Check what site 0 is
  mutate(outuse = sau - wau) %>%
  #group_by(City) %>%
  mutate(ranks = order(order(outuse)) * 100/ n()) %>% # this is already goruped by city
  filter(paste0(City, SiteID) %in% paste0(sites$City,sites$City_SiteID)) %>%
  add_column(id = sites$SiteID[match(paste0(.$City,.$SiteID), paste0(sites$City,sites$City_SiteID))]) -> ssrankings

# Classify users as Low, Medium, or High, based on their outdoor water use ranking
ssrankings %>%
  mutate(group_LC = case_when(ranks <= 33 ~ 'Low',
                              ranks > 33 & ranks <= 66  ~ 'Medium',
                              ranks > 66 ~ 'High')) %>%
  arrange(group_LC, id) -> ssrankings # 

colorb_p3 <- c("#CC6677", "#009E73", "#DDCC77")

ssrankings %>%
  filter(id != 21) %>% filter(id != 22) %>%
  add_column(IrrType = sites$IrrigationType[match(.$id, sites$SiteID)]) %>%
  add_column(IrrAr = sites$Irr_Area[match(.$id, sites$SiteID)] * sqft_sqm) %>% # irrigable area in m2
  mutate(irr_h = outuse * 1000/IrrAr) %>% #View()
  ggplot(aes(reorder(as.factor(id), outuse, sum), irr_h, fill = group_LC)) + # irr in mm
  #ggplot(aes(reorder(as.factor(id), outuse, sum), irr_h * 0.0393701, fill = group_LC)) + # irr in in
  geom_col_pattern(aes(pattern = IrrType), #color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.01,
                   pattern_spacing = 0.04,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorb_p3) +
  #geom_col_pattern(aes(pattern = IrrType)) +
  scale_pattern_manual(values = c(Hose = "stripe", SprinklerSystem = "none")) +
  labs(x = "Site", y = bquote('Irrigation'~(mm)), pattern = "Irrigation Type", fill = "Level") +
  #labs(x = "Site", y = bquote('Irrigation (in)'), pattern = "Irrigation Type", fill = "Level") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  #theme_minimal() +
  facet_grid(~ City, scales = "free_x", space = "free") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme(legend.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.tag.position = c(0.067,0.5), plot.tag = element_text(hjust =0, size=11)) +
  labs(tag = "a)") -> pOutLevel1

ssrankings %>%
  filter(id != 21) %>% filter(id != 22) %>%
  add_column(IrrType = sites$IrrigationType[match(.$id, sites$SiteID)]) %>%
  ggplot(aes(reorder(as.factor(id), outuse, sum), outuse  *gl, fill = group_LC)) + 
  #ggplot(aes(reorder(as.factor(id), outuse, sum), outuse, fill = group_LC)) + # out wa use in gal
  geom_col_pattern(aes(pattern = IrrType), #color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.01,
                   pattern_spacing = 0.04,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorb_p3) +
  #geom_col_pattern(aes(pattern = IrrType)) +
  scale_pattern_manual(values = c(Hose = "stripe", SprinklerSystem = "none")) +
  labs(x = "Site", y = bquote('Volume'~(m^3)), pattern = "Irrigation Type", fill = "Level") +
  #labs(x = "Site", y = bquote('Volume'~(10^3 ~ gal)), pattern = "Irrigation Type", fill = "Level") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  facet_grid(~ City, scales = "free_x", space = "free") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(plot.tag.position = c(0.067,0.85), plot.tag = element_text(hjust =0, size=11)) +
  labs(tag = "b)") -> pOutLevel2

plot_grid(pOutLevel1, pOutLevel2, ncol=1, align="v")
ggsave("Figure6_OutUse_Level.png", width = 11, height = 4, units = "in")






# Figure 7
# New Figure: efficient and inefficient appliances
colorb_p3 <- c("#009E73", "#DDCC77", "#CC6677")

ev %>%
  filter(Label %in% c('shower')) %>%
  select(Site, Duration, OriginalFlowRate) %>%
  rename(vol = OriginalFlowRate) %>%
  mutate(ef = case_when(vol <= 2 ~ "Efficient (WaterSense)", vol > 2 & vol <= 2.5 ~ "Compliant (Federal Standard)", 
                        #vol > 1.78 & vol <= 3.6 ~ "TT_90",
                        vol > 2.5 ~ "Inefficient")) %>% # classification based on the volume
  group_by(Site) %>%
  mutate(tt = n()) %>% # total number of toilets
  group_by(Site, ef, tt) %>%
  summarise(nt = n()) %>%
  mutate(perctlt = nt*100/tt) %>%
  filter(perctlt > 5) %>% # Remove categories with less than 5% - to reduce the influence of unintentional use
  group_by(Site) %>% mutate(sum_p = sum(perctlt)) %>%
  mutate(perctlt = perctlt*(100/sum_p)) %>%
  mutate(ef = factor(ef, levels = unique(.$ef)[c(2,1,3)])) %>% # define levels for the barplot
  ggplot(aes(as.factor(Site), perctlt, group_by(Site), fill = ef)) + geom_bar(stat = "identity", alpha = 0.95) +
  theme_minimal() +
  scale_fill_manual(values = colorb_p3) +
  xlab("Site") + ylab("Percentage of shower events") +
  theme(legend.title = element_blank()) +
  theme(legend.position="top") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0.01, 0)) -> ef1

ev %>%
  filter(Label %in% c('toilet')) %>%
  select(Site, Duration, OriginalVolume) %>%
  rename(vol = OriginalVolume) %>%
  mutate(ef = case_when(vol <= 1.28 ~ "Efficient (WaterSense)", vol > 1.28 & vol <= 1.6 ~ "Compliant (Federal Standard)", 
                        #vol > 1.78 & vol <= 3.6 ~ "TT_90",
                        vol > 1.6 ~ "Inefficient")) %>% # classification based on the volume
  group_by(Site) %>%
  mutate(tt = n()) %>% # total number of toilets
  group_by(Site, ef, tt) %>%
  summarise(nt = n()) %>%
  mutate(perctlt = nt*100/tt) %>%
  filter(perctlt > 5) %>% # Remove categories with less than 5% - to reduce the influence of unintentional use
  group_by(Site) %>% mutate(sum_p = sum(perctlt)) %>%
  mutate(perctlt = perctlt*(100/sum_p)) %>%
  mutate(ef = factor(ef, levels = unique(.$ef)[c(3,1,2)])) %>% # define levels for the barplot
  ggplot(aes(as.factor(Site), perctlt, group_by(Site), fill = ef)) + geom_bar(stat = "identity", alpha = 0.95) +
  theme_minimal() +
  scale_fill_manual(values = colorb_p3) +
  xlab("Property #") + ylab("Percentage of toilet flushes") +
  theme(legend.title = element_blank()) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0.01, 0)) -> ef2

ev %>%
  filter(Label %in% c('faucet')) %>%
  select(Site, Duration, OriginalFlowRate) %>%
  rename(vol = OriginalFlowRate) %>%
  mutate(ef = case_when(vol <= 1.5 ~ "Efficient (WaterSense)", vol > 1.5 & vol <= 2.2 ~ "Compliant (Federal Standard)", 
                        #vol > 1.78 & vol <= 3.6 ~ "TT_90",
                        vol > 2.2 ~ "Inefficient")) %>% # classification based on the volume
  group_by(Site) %>%
  mutate(tt = n()) %>% # total number of toilets
  group_by(Site, ef, tt) %>%
  summarise(nt = n()) %>%
  mutate(perctlt = nt*100/tt) %>%
  filter(perctlt > 5) %>% # Remove categories with less than 5% - to reduce the influence of unintentional use
  group_by(Site) %>% mutate(sum_p = sum(perctlt)) %>%
  mutate(perctlt = perctlt*(100/sum_p)) %>%
  mutate(ef = factor(ef, levels = unique(.$ef)[c(2,1,3)])) %>% # define levels for the barplot
  ggplot(aes(as.factor(Site), perctlt, group_by(Site), fill = ef)) + geom_bar(stat = "identity", alpha = 0.95) +
  theme_minimal() +
  scale_fill_manual(values = colorb_p3) +
  xlab("Site") + ylab("Percentage of faucet events") +
  theme(legend.title = element_blank()) +
  theme(legend.position="top") +
  scale_y_continuous(expand = c(0.01, 0)) +
  theme(legend.position="none") -> ef3
plot_grid(ef1, ef2, ef3, ncol=1, align="v", rel_heights = c(0.35, 0.3, 0.35)) -> Ef_Fig
Ef_Fig
ggsave("Figure7_EfficiencyRevision.png",Ef_Fig, width = 9, height = 8.5, units = "in")




# Efficiency of water using fixtures
# Showers
summary(ev[ev$Label == 'shower',]$Duration) # duration

# Faucet events
# cumulative distribution of volumes
ev %>%
  filter(Label == 'faucet') %>%
  select(Site, Duration, OriginalFlowRate, OriginalVolume) %>%
  mutate(vol = round(OriginalVolume, 4)) %>% # volume per faucet event
  arrange(vol) %>%
  group_by(vol) %>%
  summarise(n = n()) %>% # number of aucet events ny flow rate rounded
  mutate(freq = n / sum(n)) %>% # percentage of events per flow rate
  mutate(cumfr = round(cumsum(freq),2)) %>%  # cumulative disttribution of faucet volumes
  mutate(vol_l = round(vol*gl,2)) #%>% View()

# cumuulative duration of faucet events
ev %>%
  filter(Label == 'faucet') %>%
  select(Site, Duration, OriginalFlowRate, OriginalVolume) %>%
  mutate(dur = round(Duration, 1)) %>% # duration of faucet event - rounded to the nearest
  arrange(dur) %>%
  group_by(dur) %>%
  summarise(n = n()) %>% # number of aucet events ny flow rate rounded
  mutate(freq = n / sum(n)) %>% # percentage of events per flow rate
  mutate(cumfr = round(cumsum(freq),2)) #%>% View() # cumulative disttribution of faucet volumes

# The same for flow rate
ev %>%
  filter(Label == 'faucet') %>%
  select(Site, Duration, OriginalFlowRate, OriginalVolume) %>%
  mutate(fr = round(OriginalFlowRate, 2)) %>%
  arrange(fr) %>%
  group_by(fr) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(cumfr = round(cumsum(freq),2)) %>% 
  mutate(fr_l = round(fr,2)) #%>% View()

# faucet events with FR > 8.3 LPM
ev %>%
  filter(Label == 'faucet') %>%
  select(Site, Duration, OriginalFlowRate, OriginalVolume) %>%
  mutate(fr = round(OriginalFlowRate, 2)* gl) %>%
  arrange(fr) %>%
  filter(fr > 8.3) #%>% View()

# average duration of faucet events - in seconds
ev %>%
  filter(Label == 'faucet') %>%
  summarise(mean(Duration)*60)






# Figure 8# Indoor Water Use Weekly Variability
# Sites with more than 4 full weeks of data
ev %>%
  filter(Label != 'irrigation') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(wk = isoweek(.$dt)) %>%
  mutate(ws = paste0(Site,"_", wk,"_", year(dt))) %>%
  filter(ws %in% fws$fws) %>%
  group_by(Site, wk) %>% summarise(wv = sum(OriginalVolume)) %>% 
  summarise(n = n()) %>% filter(n > 4) -> sites_4w # these sites have more than 4 full weeks of data
#pal <- viridis_pal(option = "H")(50)  # n = number of colors seeked
#pal <- brewer.pal(n = 9, name = 'Blues')

ev %>%
  filter(Site %in% sites_4w$Site) %>%
  filter(Label != 'irrigation') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(wk = isoweek(.$dt), y = year(.$dt)) %>%
  mutate(ws = paste0(Site,"_", wk,"_", y)) %>%
  filter(ws %in% fws$fws) %>% # filter full weeks only
  mutate(month = month(dt)) %>%
  mutate(season = ifelse(month %in% 10:12, "Oct - Dec", # adding a season variable to color as suggested by Reviewer 1.
                         ifelse(month %in% 1:3, "Jan - Mar",
                                ifelse(month %in% 4:6, "Apr - Jun",
                                       "Jul - Sep")))) %>%
  mutate(season = factor(season, levels = c("Jan - Mar", "Apr - Jun", "Jul - Sep", "Oct - Dec"))) %>%
  group_by(Site, y, wk, season) %>%
  summarise(wv = sum(OriginalVolume)) %>%
  #ggplot(aes(y = wv *gl /1000, x = factor(Site), colour = wk)) +
  ggplot(aes(y = wv/1000, x = factor(Site), colour = season, shape = season)) +
  geom_point(size = 3) + 
  labs(x = 'Site', y = bquote('Volume'~(10^3 ~ L))) +
  labs(colour = paste0("Period of\n the year\n"), shape = paste0("Period of\n the year\n")) 
ggsave("Figure8_WeeklyVolumeVar.png", width = 9, height = 3, units = "in")



# weekly vol and sd
ev %>%
  filter(Site %in% sites_4w$Site) %>%
  filter(Label != 'irrigation') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(wk = isoweek(.$dt)) %>%
  #filter(month(.$dt) %in% c(10,11,12,1,2,3)) %>%
  #mutate(ws = paste0(Site,"_", wk,"_", year(dt))) %>%
  #filter(ws %in% fws$fws) %>% # filter full weeks only
  group_by(Site, wk) %>%
  summarise(wv = sum(OriginalVolume)) %>%
  group_by(Site) %>%
  summarise(avewv = mean(wv), sd = sd(wv), n = n(), iqr = IQR(wv)) %>%
  add_column(oc = sites$N_Residents[match(.$Site, sites$SiteID)]) %>%
  add_column(CV = .$sd * 100 / .$avewv) 


# Figure 9
# hourly water use stacked area chart
ev %>%
  filter(Site == 19) %>% # this plot was done for site 19 only
  filter(Label != 'irrigation') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(wk = isoweek(.$dt)) %>%
  mutate(ws = paste0(Site,"_", wk,"_", year(dt))) %>%
  filter(ws %in% fws$fws) %>% # filter full weeks only
  add_column(h = hour(.$StartTime)) %>%
  group_by(dt, h) %>%
  summarise(mhv = mean(OriginalVolume)) -> hwu19 # horly water use at site 19

# Create a df with all hours for the same number of days contained in hw19
f_df19 <- tibble(dt = rep(unique(hwu19$dt),each = 24), h =rep(0:23,length(unique(hwu19$dt))), mhv = 0) %>% arrange(dt, h)

lt_pt = sample(c('solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash'), 35, replace = TRUE)

full_join(f_df19, hwu19, by = c('h', 'dt')) %>%
  mutate(mhv = replace_na(mhv.y, 0)) %>% # replace hours that don't have a value with 0 (no event was recorded at these hours)
  select(dt, h, mhv) %>% # select only the variables of interest
  add_column(Site = 19) %>%
  add_column(y = year(.$dt), wk = week(.$dt)) %>%
  mutate(ws = paste0(Site,"_", wk,"_", y)) %>%
  filter(ws %in% fws$fws) %>% # filter full weeks only
  group_by(y, wk, h) %>%
  summarise(mwhu = sum(mhv))  %>%
  add_column(yewe = paste0(.$y,"-",.$wk)) %>%
  ggplot(aes(x = h, y = mwhu * gl, color = yewe, linetype = yewe)) + 
  #ggplot(aes(x = h, y = mwhu, color = yewe, linetype = yewe)) + 
  geom_line(size = 0.5) +
  xlab("Hour") + ylab("Volume (L)") +
  #xlab("Hour") + ylab("Volume (gal)") +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.1))) +
  scale_linetype_manual(values = lt_pt) +
  theme(legend.position="top") +
  labs(color ='Year-Week', linetype ='Year-Week') +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,24,2)) +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  theme(legend.key.width = unit(1.8,"line"))

ggsave("Figure9_HourlyWeekly_WU_19.png", width = 10, height = 5, units = "in")


# Figure 10
# Weekly variation of indoor water use distribution 
#colsp10 <- c("Bathtub" = "orange1", "Clothes Washer" = "plum3", "Faucet" = "aquamarine3", "Shower" = "indianred2", "Toilet" = "deepskyblue3")
colsp10 <- c("orange1", "plum3", "aquamarine3", "indianred2",  "deepskyblue3")
#labelsp1 <- c("Faucet", "Shower", "Toilet", "Bathtub", "Clothes Washer")

ev %>%
  filter(Site == 19) %>%
  filter(Label != 'irrigation') %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(wk = isoweek(.$dt), y = year(.$dt)) %>%
  mutate(ws = paste0(Site,"_", wk,"_", y)) %>%
  filter(ws %in% fws$fws) %>% 
  group_by(Site, y, wk, Label) %>%
  summarise(wvolcat = sum(OriginalVolume)) %>%
  group_by(Site, y, wk) %>%
  mutate(twvol = sum(wvolcat)) %>%
  mutate(perwcat = wvolcat *100 / twvol) %>%
  mutate(yewk = paste0(y,'-', wk)) %>%
  mutate(lab = case_when(Label == 'bathtub' ~ "Bathtub",Label == 'clotheswasher' ~ "Clothes Washer",
                         Label == 'faucet' ~ "Faucet",Label == 'shower' ~ "Shower", Label == 'toilet' ~ 'Toilet')) %>%
  mutate(level = case_when(wk %in% c(39,40,41) ~ 1, # adding a variable to facet plots as suggested by Rev 1
                           wk %in% c(46,47,48) ~ 2,
                           wk %in% c(25)       ~ 3,
                           wk %in% c(31,32,33,34,35) ~ 4,
                           wk %in% c(37)             ~ 5,
                           wk %in% c(2,3,4,5)        ~ 6)) %>%
  
#   mutate(wm = case_when(m %in% c(5,6,7,8,9,10) ~ "Summer", m %in% c(11,12,1,2,3,4) ~ "Winter")) -> tt_data
  ggplot(aes(yewk, perwcat, group = Label, linetype = lab, colour = lab, shape = lab)) + 
  geom_line(size = 0.9) + geom_point(size = 3) +
  facet_grid(.~ level, scales = "free_x", space = "free_x")   +
  theme(legend.position="top") +
  scale_color_manual(values = colsp10) + 
  scale_linetype_manual(values = c(2,4,5,6,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  #scale_x_discrete(expand = c(0,0)) +
  labs(x = "Date (Year-Week)", y = "Percentage of indoor water use (%)", colour = "Label", shape = "Label", linetype = "Label") +
  theme(legend.key.width = unit(3,"line")) +
  theme(strip.background = element_blank(), strip.text.x = element_blank())
ggsave("Figure10_WeeklyPercent_WU_19.png", width = 9, height = 4, units = "in") 


# Winter vs Summer differences - Table 5 and Table 6 
ev %>%
  filter(Label != 'irrigation') %>%
  filter(Label != 'unknown') %>%
  filter(Label != 'unclassified') %>%
  add_column(m = month(.$StartTime), dt = as.Date(.$StartTime)) %>%
  add_column(sda = paste0(.$Site, .$dt)) %>%
  filter(sda %in% dayp$fd) %>% # filter only full days
  mutate(wm = case_when(m %in% c(5,6,7,8,9,10) ~ "Summer", m %in% c(11,12,1,2,3,4) ~ "Winter")) -> tt_data

# Function to evaluate  a site chage in winter vs summer
ttest_f <- function(x){
  tt_data %>% filter(Site == x) -> tt_data_s
  return(rbind(tt_data_s %>%
                 filter(Label %in% c('faucet', 'shower')) %>%
                 group_by(Label) %>%
                 summarise(ttest = list(t.test(Duration ~ wm))) %>%
                 mutate(ttest = map(ttest, tidy)) %>%
                 unnest(cols = c(ttest)) %>%
                 select(estimate1, estimate2, p.value, Label) %>%
                 add_column(p = "Duration", Site = x),
               tt_data_s %>%
                 group_by(Label, dt, wm) %>%
                 summarise(n = n()) %>%
                 group_by(Label) %>%
                 summarise(ttest = list(t.test(n ~ wm))) %>%
                 mutate(ttest = map(ttest, tidy)) %>%
                 unnest(cols = c(ttest)) %>%
                 select(estimate1, estimate2, p.value, Label) %>%
                 add_column(p = "Frequency", Site = x),
               tt_data_s %>%
                 #group_by(Label, dt, wm) %>%
                 #summarise(v = sum(OriginalVolume)) %>%
                 group_by(Label) %>%
                 summarise(ttest = list(t.test(OriginalVolume ~ wm))) %>%
                 mutate(ttest = map(ttest, tidy)) %>%
                 unnest(cols = c(ttest)) %>%
                 select(estimate1, estimate2, p.value, Label) %>%
                 add_column(p = "Volume", Site = x)))}


ttest_all <- map_df(c(3,4,5,7,9,19,22,24,26,27), ttest_f) # sites with enough winter and summer data
ttest_all # results of the t test to compare winter and summer events

ttest_all %>% filter(p != 'Duration') %>% 
  group_by(Site) #%>% summarise(n())

ttest_all %>% filter(Site == 3) %>% print(n = 12)

# parameters of interest and p < 0.05
ttest_all %>%
  filter(p != 'Duration') %>%
  filter(!(p == 'Volume' & Label == 'toilet')) %>%
  filter(!(p == 'Volume' & Label == 'clotheswasher')) %>%
  filter(p.value < 0.05) %>% 
  #filter(Label == 'faucet') %>%
  arrange(p, Label) %>% print(n = 100)






















#---------- Appendix B 

# Figure B1
# Average per capita daily water use 
# 1) From Monthly Records
MonthlyWUD %>%
  filter(SiteID %in% sites$City_SiteID) %>% # Filter participants only
  filter(Consumption > 0) %>% # remove months with 0 water use
  group_by(SiteID) %>%
  summarise(adwu = mean(Consumption) *12 *1000/365) %>% # average daily water use per household
  add_column(sID = sites$SiteID[match(.$SiteID, sites$City_SiteID)]) %>%
  add_column(oc = sites$N_Residents[match(.$SiteID, sites$City_SiteID)]) %>%
  mutate(pdwu = adwu * gl / oc) %>% # per capita daily water use L
  ungroup() %>% summarise(mean(pdwu)) # average per capita daily water use

# 2) from the high-temporal resolution data
dayp %>%
  summarise(tppd = mean(tpu)) %>% # average daily water use, in pulses
  add_column(oc = sites$N_Residents[match(.$Site, sites$SiteID)]) %>%
  add_column(res = sites$MeterResolution[match(.$Site, sites$SiteID)]) %>%
  mutate(pdwu = tppd * res * gl /oc) %>% # per capita daily water use L
  ungroup() %>% summarise(mean(pdwu)) # average per capita daily water use

# Data collection - 'May trough October' - percentage of data collected during these months
dim(dayp %>% ungroup() %>% filter(month(.$dt) %in% c(5,6,7,8,9,10)))[1] *100 / dim(dayp)[1]
# Figure B1. Average Monthly Water Use by City.
MonthlyWUD %>%
  mutate(dt_f = floor_date(.$Date, 'month')) %>% # the value for January reflecst January water use - for visualization use this date
  group_by(City, dt_f) %>%
  summarise(mc = mean(Consumption)) %>%
  #ggplot(aes(dt_f, mc * gl, color = City, linetype = City, shape = City)) + 
  ggplot(aes(dt_f, mc, color = City, linetype = City, shape = City)) + 
  geom_line() + geom_point(size = 2) +
  #labs(y = bquote('Volume'~(10ˆ3 ~ gal))) + 
  labs(y = bquote('Volume'~(10^3 ~ gal))) +
  theme_bw() +
  theme(axis.title.x=element_blank(), legend.position=c(.1,.8)) +
  theme(axis.text.x = element_text(angle = 40, hjust=1)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", expand = c(0.01, 0.01))
ggsave("FigureB1_AverageMonthlyWaterUse.png", width =7.5, height = 2.8, units = "in") # Save Figure 1



# Appendix - Figure B2
MonthlyWUD %>%
  filter(City == 'Logan' & year(Date) %in% c(2017,2018) | City == 'Providence' & year(Date) %in% c(2018,2019)) %>% # Filter the last two years for each city
  filter(Consumption > 0) %>% # remove accounts with no water use 
  add_column(Y = year(.$Date)) %>%
  group_by(City, SiteID, Y) %>%
  summarise(au = sum(Consumption)/n(), nm = n()) %>% # au is the average monthly use for each City, Site and Year
  ungroup() %>% group_by(City, Y) %>% 
  filter(nm > 8) %>% # remove accounts lerss than 8 months of data 
  mutate(ranks = order(order(au)) * 100/ n()) %>% # this is the position of each user each year - the rankings
  filter(paste0(City,SiteID) %in% paste0(sites$City,sites$City_SiteID)) %>%
  add_column(id = sites$SiteID[match(paste0(.$City,.$SiteID), paste0(sites$City,sites$City_SiteID))]) -> rankings # Annual ranking for participant sites

# Linetypes
linetypes = c(apply(expand.grid(c(2,4), c(1,2,4,8,"A")), 1, paste, collapse=""), apply(expand.grid(c(2,4,8), c(2,4), c(5,"F"), 2), 1, paste, collapse=""), "4284B4F4", "228F61A4")

set.seed(123)
rankings %>%
  filter(City == "Logan") %>%
  ggplot(aes(Y, ranks, shape = factor(id), colour = factor(id), linetype = factor(id))) +
  geom_line(alpha = 0.7) + geom_point(size = 4) +
  scale_linetype_manual(values = linetypes) + 
  #scale_colour_manual(values = sample(1:50)) + 
  scale_shape_manual(values = sample(1:20)) + 
  labs(subtitle="a)", x = "Year", y = "Percentiles") +
  scale_x_continuous(breaks = c(2017, 2018)) +
  theme(legend.key.width= unit(1.5, 'cm')) + 
  labs(color='SiteID', shape = 'SiteID', linetype = 'SiteID') -> pt1 # Annual User rankings - 2 years

rankings %>%
  filter(City == "Providence") %>%
  ggplot(aes(Y, ranks, shape = factor(id), colour = factor(id), linetype = factor(id))) +
  geom_line(alpha = 0.7) + geom_point(size = 4) +
  scale_linetype_manual(values = linetypes) + 
  #scale_colour_manual(values = sample(1:50)) + 
  scale_shape_manual(values = sample(1:20)) + 
  labs(subtitle="b)", x = "Year", y = "Percentiles") +
  scale_x_continuous(breaks = c(2018, 2019)) +
  theme(legend.key.width= unit(1.5, 'cm')) + 
  labs(color='SiteID', shape = 'SiteID', linetype = 'SiteID') -> pt2 # same plot than pt1 but for Providence

rankings %>%
  add_column(oc = sites$N_Residents[match(paste0(.$City, .$SiteID), paste0(sites$City, sites$City_SiteID))]) %>%
  group_by(id, oc) %>%
  summarise(adwu = mean(au)*12000/365) %>% # average daily water use in G
  mutate(pcdwu = adwu*gl/oc, x = "a") %>% # per capita daily water use L
  ggplot(aes(x, pcdwu, colour = as.factor(id), shape = as.factor(id))) + 
  geom_jitter(size = 4) +
  scale_shape_manual(values = sample(1:20, size = 33, replace = TRUE)) +
  labs(subtitle="c)", y = "Volume (L)",x = "", shape = "SiteID", colour = "SiteID") +
  theme(axis.text.x=element_blank()) +
  guides(colour=guide_legend(ncol=1), shape=guide_legend(ncol=1)) -> pt3

grid.arrange(pt1, pt2, pt3, nrow = 1, widths = c(0.38, 0.38, 0.24)) -> p
ggsave("FigureB2_RankingIndPC_Daily_Lit.png",plot = p, width = 9.2, height = 9.2*0.8, units = "in")

rankings %>%
  add_column(IT = sites$IrrigationType[match(paste0(.$City, .$SiteID), paste0(sites$City, sites$City_SiteID))]) %>%
  filter(IT != 'Hose') %>% #-> SS_IrrigatorsRanking
  group_by(id) %>% summarise(rl = mean(ranks)) %>%
  arrange(rl) %>% print(n = 25)


# Average indoor water use
ev %>%
  filter(Label != 'irrigation') %>% # Remove Irrigation
  add_column(day = as.Date(.$StartTime)) %>%
  add_column(sda = paste0(.$Site, .$day)) %>%
  filter(sda %in% dayp$fd) %>% # filter only full days
  add_column(oc = sites$N_Residents[match(.$Site, sites$SiteID)]) %>% # ocupants per site
  group_by(Site, day, oc) %>%
  summarise(dailyvol = sum(OriginalVolume)) %>% # daily indoor volume 
  mutate(dvolpc = dailyvol/oc) %>% # daily indoor volume per capita
  group_by(Site) %>% 
  summarise(advolpc = mean(dvolpc) * gl) %>% # daily average volume per capita per Site, in Liters
  ungroup() %>% summarise(mean(advolpc)) # average per capita daily water use



# Table B1. Appendix
ev %>%
  filter(Label != 'irrigation') %>% # Remove Irrigation
  #filter(Label != 'unknown') %>% # Remove unknown
  add_column(dt = as.Date(.$StartTime)) %>%
  add_column(sda = paste0(.$Site, .$dt)) %>%
  filter(sda %in% dayp$fd) %>% # select full days only
  group_by(Label) %>%
  summarise(awupl = sum(OriginalVolume)) %>% 
  mutate(totwu = sum(awupl)) %>%
  #mutate(voldist = awupl * 174/ totwu) # Average per capita per label water use
  mutate(prcdist = awupl * 100/ totwu) # percent of indoor water use



# Figure B3
set.seed(123)
ev %>%
  filter(Label %in% c('shower')) %>%
  select(Site, Duration, OriginalFlowRate) %>%
  group_by(Site) %>%
  summarise(mfr = mean(OriginalFlowRate), mdr = mean(Duration)) %>% #filter(mfr > 2)
  ggplot(aes(mfr * gl, mdr, shape = factor(Site), colour = factor(Site))) +
  geom_point(size = 4) +
  scale_shape_manual(values = rep(sample(1:20),2)) +
  xlab("Average Flow Rate (Lpm)") + ylab("Average Duration (min)") +
  theme(legend.position="top") +
  labs(color='Site', shape = 'Site') +
  guides(shape = guide_legend(nrow = 2)) #+
ggsave("FigureB3_Showers.png", width = 9, height = 4, units = "in")

# Figure B4
# Toilets
ev %>%
  filter(Label == 'toilet') %>%
  select(Site, OriginalVolume) %>%
  #filter(Site != 18) %>%
  group_by(Site) %>% #summarise(n()) %>% print(n=35)
  #ungroup() %>% summarise(mtf = mean(OriginalVolume), sdtf = sd(OriginalVolume))
  #arrange(OriginalVolume) %>%
  #slice(5:(n() - 15)) %>%
  ggplot(aes(factor(Site), OriginalVolume * gl, fill = factor(Site))) +
  geom_violin() + theme(legend.position = "none") +
  xlab("Site") + ylab("Volume (L)") #+ coord_flip()
ggsave("FigureB4_toilets.png", width = 8, height = 4, units = "in")

# Figure B5. Faucet Events
ev %>%
  filter(Label == 'faucet') %>%
  select(Site, Duration, OriginalFlowRate) %>% # Flow rate of faucet events
  ggplot(aes(as.factor(Site), OriginalFlowRate * gl)) + geom_boxplot(outlier.shape = NA) +
  ylab("Flow Rate (L/min)") + xlab("Site") +
  coord_cartesian(ylim =  c(0, 13)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(subtitle = "a)") -> pfct1

ev %>%
  filter(Label == 'faucet') %>%
  select(Site, Duration, OriginalFlowRate) %>% # Duration of faucet events
  ggplot(aes(as.factor(Site), Duration * 60)) + geom_boxplot(outlier.shape = NA) +
  ylab("Duration (s)") + xlab("Site") +
  coord_cartesian(ylim =  c(0, 170)) +
  labs(subtitle = "b)") -> pfct2

grid.arrange(pfct1, pfct2, nrow = 2) -> p
ggsave("FigureB5_Fct_Dur_FR.png",plot = p, width = 8, height = 5, units = "in")

# End
print("End of Analyses")
