
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

lm_col <- function(final_net,lm_data){
  temp <- final_net %>% 
    mutate(col_name = 
             ifelse(lm_data[,5] <= 0.001, 1, 0)) %>%
    mutate(col_name_dis = 
             nn_function(st_c(st_coid(final_net)),
                         st_c(st_coid(filter(final_net, 
                                             col_name == 1))), 
                         k = 1))
  return(temp)
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
risk_v <- function(model_data,litter_data,approach){
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
    labs(title=paste("Litter Risk Predictions",approach,sep = '--'),
         subtitle="") +
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
  fishnet <- st_make_grid(chen_bdry,
                          cellsize = 500, 
                          square = TRUE) %>%
    .[chen_bdry] %>%            
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
  tifCropped <- crop(img, extent(boundary)) # extract raster based on boundary
  tifClipped <- mask(tifCropped, boundary) # clip the raster based on boundary
  polys1 = rasterToPolygons(tifClipped) # convert raster to polygon (it will take some time, it's OK)
  sf_object <- st_as_sf(polys1) 
  temp <- as.data.frame(sf_object) # convert to sf dataframe
  temp_sf <- st_as_sf(temp)
  df_points <- st_centroid(temp_sf)
  return(df_points)
}