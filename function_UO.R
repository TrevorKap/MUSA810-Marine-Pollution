# load_city_data: load any mdt data file in the repository, can be applied ot a list
# input of function:
# city: name of city

load_city_data <- function(city) {
  url <- paste0("https://raw.githubusercontent.com/TrevorKap/MUSA810-Marine-Pollution/main/Data/mdt-data", city, ".csv")
  data <- read_csv(url)
  
  # Convert latitude and longitude columns to numeric
  data$latitude <- as.numeric(data$latitude)
  data$longitude <- as.numeric(data$longitude)
  
  # Create spatial object
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"),crs = 4326, agr = "constant")%>%
    st_transform('EPSG:32643')
  return(data_sf)
}


# load_city_kml:
# input of function:
# city: name of city

load_city_kml <- function(city) {
  kml_url <- paste0("https://raw.githubusercontent.com/TrevorKap/MUSA810-Marine-Pollution/main/Data/", city, ".kml")
  kml_data <- st_read(kml_url)%>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  return(kml_data)
}

# load_city_kml_meter:
# input of function:
# city: name of city

load_city_kml_meter <- function(city) {
  kml_url <- paste0("https://raw.githubusercontent.com/TrevorKap/MUSA810-Marine-Pollution/main/Data/", city, ".kml")
  kml_data <- st_read(kml_url) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
    st_transform('EPSG:32643')
  return(kml_data)
}

# point_data: load the OSM data
# input of function: 
# boundary: the bbox boundary of the city wanted to analysis (type: sf_dataset)
# keys: the main category of the OSM (type: string)
# values: the sub category of the OSM (type: string/list)
# legends: the type marking the dataset (type: string)

point_data <- function(boundary,keys,values,legends){
  temp <- opq(bbox = boundary) %>%
    add_osm_feature(key = keys, value = values)%>%
    osmdata_sf()
  
  if (nrow(temp$osm_points) == 0) {
    temp_point <- st_sf(osm_id = integer(), geometry = st_sfc(crs = 4326))%>%
      mutate(legend = legends)%>%
      st_transform('EPSG:32643')
  } else {
    temp_point <- pluck(temp,'osm_points')%>%
      dplyr::select(osm_id,geometry)%>%
      st_transform('EPSG:32643')%>%
      mutate(legend = legends)
  }
  
  return(temp_point)
}

# countfishnet: create the fishnet for the dataset that 'point_data' create
# input of function: 
# fishnet: the fishnet that of the city
# dataset: the POI dataset generated from 'point_data' 

countfishnet <- function(fishnet,dataset){
  net <- 
    dplyr::select(dataset) %>% 
    mutate(count = 1) %>% 
    aggregate(., fishnet, sum) %>%
    mutate(count = replace_na(count, 0),
           uniqueID = 1:n(),
           cvID = sample(round(nrow(fishnet) / 24), 
                         size=nrow(fishnet), replace = TRUE))
  return(net)
}

# knnfishnet: create the knnfishnet for the dataset that 'point_data' create
# input of function: 
# fishnet: the fishnet that of the city
# dataset: the POI dataset generated from 'point_data' 
# knum: the parameter of KNN, the normal situation is 3 (type: numeric)

knnfishnet <- function(fishnet,dataset,knum,label){
  if(nrow(dataset) == 0){
    vars_net <- fishnet %>%
      mutate(!!label := 0,
             item.nn = 0)
  } else{
    vars_net <- dataset%>%
      st_join(fishnet, join=st_within) %>%
      st_drop_geometry() %>%
      group_by(uniqueID,legend) %>%
      summarize(count = n()) %>%
      left_join(fishnet, ., by = "uniqueID") %>%
      spread(legend, count, fill=0) %>%
      dplyr::select(-`<NA>`) %>%
      ungroup()
    vars_net <- vars_net %>%
      mutate(item.nn = nn_function(st_c(st_coid(vars_net)), 
                                   st_c(dataset),
                                   k = knum))
  }
  return(vars_net)
}

pn_gen <- function(stor_df){
  for (i in 1:nrow(stor_df)) {
    tw_p <- point_data(temp_bbox,stor_df[i,1],unlist(stor_df[i,3]),stor_df[i,2])
    tw_nn <- knnfishnet(temp_fish,tw_p,3,stor_df[i,2])
    final_net <- left_join(final_net, st_drop_geometry(tw_nn), by="uniqueID") %>%
      rename_with(~ paste0(stor_df[i, 2], "_nn"), .cols = matches("item.nn"))}
  return(final_net)
}

point_nn_gen <- function(temp_bbox,temp_fish,litter_net){
  water_point <- point_data(temp_bbox, 'water',c('canal','drain','ditch'),'water')
  water_nn_net <- knnfishnet(temp_fish,water_point,3,'water')
  
  waste_point <- point_data(temp_bbox, 'amenity',c('waste_basket','waste_disposal','waste_transfer_station','recycling'),'waste')
  waste_nn_net <- knnfishnet(temp_fish,waste_point,3,'waste')
  
  rstrt_point <- point_data(temp_bbox, 'amenity',c('restaurant','pub','bar'),'restaurant')
  rstrt_nn_net <- knnfishnet(temp_fish,rstrt_point,3,'restaurant')
  
  road_point <- point_data(temp_bbox, 'highway','residential','road')
  road_nn_net <- knnfishnet(temp_fish,road_point,3,'road')
  
  indstr_point <- point_data(temp_bbox, 'landuse','industrial','industrial')
  indstr_nn_net <- knnfishnet(temp_fish,indstr_point,3,'industrial')
  
  rsdnt_point <- point_data(temp_bbox, 'landuse','residential','residential')
  rsdnt_nn_net <- knnfishnet(temp_fish,rsdnt_point,3,'residential')
  
  rtl_point <- point_data(temp_bbox, 'landuse','retail','retail')
  rtl_nn_net <- knnfishnet(temp_fish,rtl_point,3,'retail')
  
  final_net <- left_join(litter_net, st_drop_geometry(water_nn_net), by="uniqueID") %>%
    left_join(.,st_drop_geometry(rstrt_nn_net),by="uniqueID")%>%
    left_join(.,st_drop_geometry(road_nn_net), by="uniqueID")%>%
    left_join(.,st_drop_geometry(indstr_nn_net), by="uniqueID")%>%
    left_join(.,st_drop_geometry(rsdnt_nn_net), by="uniqueID")%>%
    left_join(.,st_drop_geometry(rtl_nn_net), by="uniqueID")%>%
    rename(water_nn = item.nn.x,
           restaurant_nn = item.nn.y,
           road_nn = item.nn.x.x,
           indstr_nn = item.nn.y.y,
           rsdnt_nn = item.nn.x.x.x,
           rtl_nn = item.nn.y.y.y)
  
  return(final_net)
}

pop_process <- function(temp_point,fishnet,epsg){
  df_points <- temp_point%>%st_transform(paste('EPSG:',epsg))
  joined_data <- st_join(fishnet, df_points)
  
  avg_pop <- joined_data %>%
    group_by(uniqueID) %>%
    summarise(avg_pop = mean(Population.Count, na.rm = TRUE))
  avg_pop[is.na(avg_pop)] <- 0
  
  sum_pop <- joined_data %>%
    group_by(uniqueID) %>%
    summarise(sum_pop = sum(Population.Count, na.rm = TRUE))
  return(list(avg_pop = avg_pop, sum_pop = sum_pop))
}

add_pop <- function(pop_result,final_net){
  temp_a_pop <- pop_result$avg_pop
  temp_s_pop <- pop_result$sum_pop
  final_net <- left_join(final_net,st_drop_geometry(temp_a_pop), by="uniqueID")%>%
    left_join(.,st_drop_geometry(temp_s_pop), by="uniqueID")
  return(final_net)
}
# nn_visual: plot the fishnet in NN distance
# input of function: 
# nn_data: the POI dataset generated from 'knnfishnet' 
# titles: the category od the nn_data (e.g. restaurant, water, road, etc.) (type: string)

nn_visual <- function(nn_data,titles){
  temp <- 
    dplyr::select(nn_data, ends_with(".nn")) %>%
    gather(Variable, value, -geometry)
  
  ggplot() +
    geom_sf(data = temp, aes(fill=value), colour=NA) +
    scale_fill_viridis(name="NN Distance") +
    labs(title= paste(titles,"knn distance"))+
    mapTheme()+
    theme(plot.title = element_text(size = 10, hjust = 0))
}

visual_count <- function(net_one,variable){
  ggplot() +
    geom_sf(data = net_one, aes(fill = net_one[[variable]]), color = NA) +
    scale_fill_viridis_c() +
    labs(title = paste(variable,"count for the fishnet")) +
    mapTheme()
}

visual_cotinuous <- function(net_one,variable){
  ggplot() +
    geom_sf(data = net_one, aes(fill = net_one[[variable]]), color = NA) +
    scale_fill_continuous() +
    labs(title = paste(variable,"count for the fishnet")) +
    mapTheme()
}

# visual_count_net: plot the fishnet and point distribution 
# input of function: 
# net_one: the dataset that 'countfishnet' created 
# point_one: the dataset that 'point_data' create
# boundary: the ACTUAL boundary of the city wanted to analysis (type: sf_dataset)
# variable_name: the category od the net_one (e.g. restaurant, water, road, etc.) (type: string)

visual_count_net <- function(net_one,point_one,boundary,variable_name){
  grid.arrange(
    ggplot() +
      geom_sf(data = net_one, aes(fill = count), color = NA) +
      scale_fill_viridis() +
      labs(title = paste(variable_name,"count for the fishnet")) +
      mapTheme(),
    ggplot() + 
      geom_sf(data = boundary) +
      geom_sf(data = point_one, colour="red", size=0.2, show.legend = "point") +
      labs(title= paste(variable_name,", Chennai")) +
      mapTheme(title_size = 14),
    nrow = 1
  )
}

# lc_mr_visual: visualize the result of local moran's I result
# input of function: 
# mr_dataset: the dataset from the local moran analysis
# final: the dataset that combined all the variables already
# legends: the category od the mr_dataset (e.g. restaurant, water, road, etc.) (type: string)

lc_mr_visual <- function(mr_dataset,final,legends){
  temp <- cbind(mr_dataset, as.data.frame(final)) %>% 
    st_sf() %>%
    dplyr::select(count = legends, 
                  Local_Morans_I = Ii, 
                  P_Value = `Pr(z != E(Ii))`) %>%
    mutate(Significant_Hotspots = ifelse(P_Value <= 0.001, 1, 0)) %>%
    gather(Variable, Value, -geometry)
  
  vars <- unique(temp$Variable)
  varList <- list()
  
  for(i in vars){
    varList[[i]] <- 
      ggplot() +
      geom_sf(data = filter(temp, Variable == i), 
              aes(fill = Value), colour=NA) +
      scale_fill_viridis(name="") +
      labs(title=i) +
      mapTheme(title_size = 14) + theme(legend.position="bottom")}
  
  do.call(grid.arrange,c(varList, ncol = 4, top = paste(legends,", Local Morans I statistics")))
}

# risk_visualize: visualize the risk prediction place of the model result
# input of function: 
# model_data: the model dataset (default is from crossValidate, but it could be work on model result dataset with prediction column)
# litter_data: the litter dataset that combining the final dataset

risk_visualize <- function(model_data,litter_data){
  ml_breaks <- classIntervals(model_data$Prediction, 
                              n = 5, "fisher")
  
  litter_risk_sf <- model_data %>%
    mutate(label = "Risk Predictions",
           Risk_Category =classInt::findCols(ml_breaks),
           Risk_Category = case_when(
             Risk_Category == 5 ~ "5th",
             Risk_Category == 4 ~ "4th",
             Risk_Category == 3 ~ "3rd",
             Risk_Category == 2 ~ "2nd",
             Risk_Category == 1 ~ "1st")) %>%
    cbind(
      aggregate(
        dplyr::select(litter_data) %>% mutate(litterCount = 1), ., sum) %>%
        mutate(litterCount = replace_na(litterCount, 0))) %>%
    dplyr::select(label,Risk_Category, litterCount)
  
  ggplot() +
    geom_sf(data = litter_risk_sf, aes(fill = Risk_Category), colour = NA) +
    geom_sf(data = litter_data, size = .3, colour = "red") +
    scale_fill_viridis(discrete = TRUE) +
    labs(title="Litter Risk Predictions",
         subtitle="") +
    mapTheme(title_size = 14)
}

# lm_col: add moran's significant point into the final dataset
# input of function: 
# final_net
# lm_data: the dataset created by local moran;s I analysis

lm_col <- function(final_net, lm_data) {
  # First, handle the NAs in the lm_data fifth column
  lm_data[, 5][is.na(lm_data[, 5])] <- 1 # Setting to 1 ensures they become 0 in col_name
  
  # Mutate to create col_name based on lm_data
  temp <- final_net %>%
    mutate(col_name = ifelse(lm_data[, 5] <= 0.001, 1, 0)) %>%
    mutate(col_name_dis = ifelse(col_name == 1,
                                 nn_function(st_c(st_coid(final_net)),
                                             st_c(st_coid(filter(final_net, col_name == 1))), 
                                             k = 1),
                                 0)) # Set col_name_dis to 0 if col_name is 0
  return(temp)
}

risk_level <-function(model_data,approach){
  ml_breaks <- classIntervals(model_data$Prediction, 
                              n = 5, approach)
  
  litter_risk_sf <- model_data %>%
    mutate(label = "Risk Predictions",
           Risk_Category =classInt::findCols(ml_breaks),
           Risk_Category = case_when(
             Risk_Category == 5 ~ "5th",
             Risk_Category == 4 ~ "4th",
             Risk_Category == 3 ~ "3rd",
             Risk_Category == 2 ~ "2nd",
             Risk_Category == 1 ~ "1st"))
  return(litter_risk_sf)}

moran_agg <- function(final_net){
  final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE) 
  final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE) 
  
  local_morans_rst <- localmoran(final_net$restaurant, final_net.weights, zero.policy=TRUE) %>% as.data.frame()
  local_morans_road <- localmoran(final_net$road, final_net.weights, zero.policy=TRUE) %>% as.data.frame()
  local_morans_wtr <- localmoran(final_net$water, final_net.weights, zero.policy=TRUE) %>% as.data.frame()
  local_morans_sp <- localmoran(final_net$sum_pop, final_net.weights, zero.policy=TRUE) %>% as.data.frame()
  local_morans_ap <- localmoran(final_net$avg_pop, final_net.weights, zero.policy=TRUE) %>% as.data.frame()
  local_morans_indstr <- localmoran(final_net$industrial, final_net.weights, zero.policy=TRUE) %>% as.data.frame()
  local_morans_rsdnt <- localmoran(final_net$residential, final_net.weights, zero.policy=TRUE) %>% as.data.frame()
  local_morans_rtl <- localmoran(final_net$retail, final_net.weights, zero.policy=TRUE) %>% as.data.frame()
  
  final_net <- lm_col(final_net,local_morans_wtr) 
  final_net <- final_net%>% rename(wtr_sig = col_name, wtr_sig_dis = col_name_dis) 
  final_net <- lm_col(final_net,local_morans_road)
  final_net <- final_net%>% rename(road_sig = col_name, road_sig_dis = col_name_dis)
  final_net <- lm_col(final_net,local_morans_rst)
  final_net <- final_net%>% rename(rst_sig = col_name, rst_sig_dis = col_name_dis)
  final_net <- lm_col(final_net,local_morans_indstr)
  final_net <- final_net%>% rename(indstr_sig = col_name, indstr_sig_dis = col_name_dis)
  final_net <- lm_col(final_net,local_morans_rsdnt)
  final_net <- final_net%>% rename(rsdnt_sig = col_name, rsdnt_sig_dis = col_name_dis)
  final_net <- lm_col(final_net,local_morans_rtl)
  final_net <- final_net%>% rename(rtl_sig = col_name, rtl_sig_dis = col_name_dis)
  return(final_net)
}

moran_gen <- function(final_net,stor_df){
  final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE) 
  final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE) 
  for (i in 1:nrow(stor_df))  {
    local_morans_wtr <- localmoran(final_net[[stor_df[i, 2]]], final_net.weights, zero.policy=TRUE) %>% as.data.frame()
    final_net <- lm_col(final_net,local_morans_wtr)%>%
      rename_with(~ paste0(stor_df[i, 2], "_sig_dis"), .cols = matches("col_name_dis")) %>%
      rename_with(~ paste0(stor_df[i, 2], "_sig"), .cols = matches("col_name"))}
  return(final_net)
}

# error_visual: visualize the error of model
# input of function: 
# model_data: the model result dataset (tips: should contain Prediction, count column)

error_visual <- function(model_data){
  error_by_reg_and_fold <- 
    model_data %>%
    group_by(uniqueID) %>% 
    summarize(Mean_Error = mean(Prediction - count, na.rm = T),
              MAE = mean(abs(Mean_Error), na.rm = T),
              SD_MAE = mean(abs(Mean_Error), na.rm = T)) %>%
    ungroup()
  
  error_by_reg_and_fold %>% arrange(desc(MAE))
  error_by_reg_and_fold %>% arrange(MAE)
  
  error_by_reg_and_fold %>%
    ggplot(aes(MAE)) + 
    geom_histogram(bins = 100, colour="black", fill = "#FDE725FF") +
    scale_x_continuous(breaks = seq(0, 11, by = 1)) + 
    labs(title="Distribution of MAE", subtitle = "LOGO-CV",
         x="Mean Absolute Error", y="Count") 
}

# model_process: to get the prediction variable
# input of function: 
# dataset: the data need to be predict
# model: the ML model
model_process <- function(dataset,model){
  df_rf_rst <- dataset%>%
    mutate(Prediction = predict(model,dataset,type = 'response'),
           Mean_Error = mean(Prediction - count,na.rm = T),
           MAE = mean(abs(Mean_Error), na.rm = T),
           SD_MAE = mean(abs(Mean_Error), na.rm = T))%>%
    dplyr::select(uniqueID,count,Prediction)
  return(df_rf_rst)
}

# model_process: to get the model performance result
# input of function: 
# dataset: the data need to be predict
# model: the ML model
model_result <- function(dataset,model){
  df_rf_rst <- dataset%>%
    mutate(Prediction = predict(model,dataset,type = 'response'))%>%
    summarise(Mean_Error = mean(Prediction - count,na.rm = T),
              MAE = mean(abs(Mean_Error), na.rm = T),
              SD_MAE = mean(abs(Mean_Error), na.rm = T))%>%
    dplyr::select(Mean_Error,MAE,SD_MAE)
  return(df_rf_rst)
}

# approach is from "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box"
# risk_v: visualize the result of risk map
# input of function: 
# model_data: the dataset with Prediction column
# litter_data: the litter data
# appraoch: see from above
risk_v <- function(model_data,litter_data,approach,model){
  ml_breaks <- classIntervals(model_data$Prediction, 
                              n = 5, approach)
  
  litter_risk_sf <- model_data %>%
    mutate(label = "Risk Predictions",
           Risk_Category =classInt::findCols(ml_breaks),
           Risk_Category = case_when(
             Risk_Category == 5 ~ "5th",
             Risk_Category == 4 ~ "4th",
             Risk_Category == 3 ~ "3rd",
             Risk_Category == 2 ~ "2nd",
             Risk_Category == 1 ~ "1st"))
  
  ggplot() +
    geom_sf(data = litter_risk_sf, aes(fill = Risk_Category), colour = NA) +
    geom_sf(data = litter_data, size = .3, colour = "red") +
    scale_fill_viridis(discrete = TRUE) +
    labs(title=paste("Litter Risk Predictions",approach,sep = '--'),
         subtitle= model) +
    mapTheme(title_size = 8)
}

# risk_v: visualize all the result of risk map
# input of function: 
# dataset: the dataset with Prediction column
partytime <- function(dataset){
  grid.arrange(risk_v(dataset,litter_p,"sd"),
               risk_v(dataset,litter_p,"equal"),
               risk_v(dataset,litter_p,"pretty"),
               risk_v(dataset,litter_p,"quantile"),
               risk_v(dataset,litter_p,"kmeans"),
               risk_v(dataset,litter_p,"hclust"),ncol=3)
  
  grid.arrange(
    risk_v(dataset,litter_p,"bclust"),
    risk_v(dataset,litter_p,"fisher"),
    risk_v(dataset,litter_p,"jenks"),
    risk_v(dataset,litter_p,"dpih"),
    risk_v(dataset,litter_p,"headtails"),
    risk_v(dataset,litter_p,"maximum"),ncol=3)
}


create_fish <- function(boudary){
  fishnet <- st_make_grid(boudary,
                          cellsize = 500, 
                          square = TRUE) %>%
    .[boudary] %>%            
    st_sf() %>%
    mutate(uniqueID = 1:n())
  return(fishnet)
}

# the boundary is not after projection 
get_bbox <- function(boundary){
  bbox_temp <- st_bbox(boundary)
  bbox_list_unnamed <- unname(as.list(bbox_temp))
  bbox_list <- c(bbox_list_unnamed[[1]][1],bbox_list_unnamed[[2]][1],bbox_list_unnamed[[3]][1],bbox_list_unnamed[[4]][1])
  return(bbox_list)
}

raster_process <- function(img,boundary){
  boundary <- st_zm(boundary)
  tifCropped <- crop(img, extent(boundary)) # extract raster based on boundary
  tifClipped <- mask(tifCropped, boundary) # clip the raster based on boundary
  polys1 = rasterToPolygons(tifClipped) # convert raster to polygon (it will take some time, it's OK)
  sf_object <- st_as_sf(polys1) 
  temp <- as.data.frame(sf_object) # convert to sf dataframe
  temp_sf <- st_as_sf(temp)
  df_points <- st_centroid(temp_sf)
  return(df_points)
}