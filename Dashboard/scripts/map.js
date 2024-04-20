import { cityLocation } from "../data/cityLatLon.js";
import { allCities } from "../data/allCitiesPred.js";

export function changeMap(city) {
  map.setView(cityLocation[city], 12);
}

// initialize map
// mapbox access token
const mapboxAccessToken =
  "pk.eyJ1IjoieGlhb2Zhbi05OCIsImEiOiJjbG1tYTUyeDYwZ3Z0MnJsMXp5bzlhbmhuIn0.o4NFKmmhKwaWErRm16MjHA";

// initialize map view
const map = L.map("map").setView(cityLocation["Chennai"], 12);

// add base maps and combine to groups
const topoMap = L.tileLayer(
  "https://api.mapbox.com/styles/v1/xiaofan-98/clv45r2lq025w01p6esq67faj/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoieGlhb2Zhbi05OCIsImEiOiJjbG1tYTUyeDYwZ3Z0MnJsMXp5bzlhbmhuIn0.o4NFKmmhKwaWErRm16MjHA",
  {
    maxZoom: 19,
    tileSize: 512,
    zoomOffset: -1,
    attribution:
      'Map data &copy; <a href="https://www.mapbox.com/">Mapbox</a> contributors, ' +
      '<a href="https://creativecommons.org/licenses/by/4.0/">CC-BY-4.0</a>',
    id: "xiaofan-98/cltnsajw4028d01qe473s5rio", // Mapbox style ID
    accessToken: mapboxAccessToken,
  }
);

const streetMap = L.tileLayer(
  `https://api.mapbox.com/styles/v1/xiaofan-98/cltnsajw4028d01qe473s5rio/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoieGlhb2Zhbi05OCIsImEiOiJjbG1tYTUyeDYwZ3Z0MnJsMXp5bzlhbmhuIn0.o4NFKmmhKwaWErRm16MjHA`,
  {
    maxZoom: 19,
    tileSize: 512,
    zoomOffset: -1,
    attribution:
      'Map data &copy; <a href="https://www.mapbox.com/">Mapbox</a> contributors, ' +
      '<a href="https://creativecommons.org/licenses/by/4.0/">CC-BY-4.0</a>',
    id: "xiaofan-98/cltnsajw4028d01qe473s5rio", // Mapbox style ID
    accessToken: mapboxAccessToken,
  }
);

const baseMaps = {
  StreetMap: streetMap,
  topoMap: topoMap,
};

// set color range func
function getColor(x) {
  switch (x) {
    case 5:
      return "#A6325A";
    case 4:
      return "#D35269";
    case 3:
      return "#F3A881";
    case 2:
      return "#FAC98B";
    case 1:
      return "#FFEAAE";
    default:
      return "#FFEDA0";
  }
}

// set style for grids
function style(feature) {
  return {
    fillColor: getColor(feature.properties.Risk_Category), // color by risk level
    weight: 1.5,
    opacity: 1,
    color: "#ffffff",
    dashArray: "",
    fillOpacity: 0.8,
  };
}

// add interaction
// highlight - hovering on
function highlightFeature(event) {
  const layer = event.target;

  layer.setStyle({
    weight: 5,
    color: "#ffffff",
    dashArray: "",
    fillOpacity: 1,
  });

  layer.bringToFront();
}

// default - mouse off
function resetHighlight(event) {
  const layer = event.target;

  layer.setStyle({
    weight: 1.5,
    opacity: 1,
    color: "#ffffff",
    dashArray: "",
    fillOpacity: 1,
  });
}

// zoom in - clicking
function zoomToFeature(event) {
  map.flyToBounds(event.target.getBounds(), {
    duration: 1,
    easeLinearity: 0.25,
  });
}

// onEachFeature to add the listeners on grids:
function onEachFeature(feature, layer) {
  layer.on({
    mouseover: function (event) {
      highlightFeature(event); // Highlight the feature when hovered
      this.openPopup(); // Open the popup when hovered
    },
    mouseout: function (event) {
      resetHighlight(event); // Reset the highlight when mouseout
      this.closePopup(); // Close the popup when mouseout
    },
    click: zoomToFeature,
  });

  layer.bindPopup(
    "<h4>Grid Risk</h4>" +
      (feature.properties
        ? "<b>" +
          feature.properties.Risk_Category +
          " risk level</b><br />" +
          feature.properties.count +
          " count<br />" +
          feature.properties.water_nn.toFixed(1) +
          "m /water_nn<br />" +
          feature.properties.restaurant_nn.toFixed(1) +
          "m /restaurant_nn"
        : "No data available")
  );
}

// Create layer groups for different risk levels
const group1 = L.layerGroup(); // Risk Level 1
const group2 = L.layerGroup(); // Risk Level 2
const group3 = L.layerGroup(); // Risk Level 3
const group4 = L.layerGroup(); // Risk Level 4
const group5 = L.layerGroup(); // Risk Level 5

// Populate layer groups based on risk levels
allCities.features.forEach((feature) => {
  const riskLevel = feature.properties.Risk_Category;
  const layer = L.geoJson(feature, {
    style: style,
    onEachFeature: onEachFeature,
  });

  switch (riskLevel) {
    case 1:
      group1.addLayer(layer);
      break;
    case 2:
      group2.addLayer(layer);
      break;
    case 3:
      group3.addLayer(layer);
      break;
    case 4:
      group4.addLayer(layer);
      break;
    case 5:
      group5.addLayer(layer);
      break;
    default:
      // Add to group6 if there's an unknown risk level
      group6.addLayer(layer);
  }
});

// Define control for layer groups with all layers checked by default
const overlayMaps = {
  "Risk Level 1": group1,
  "Risk Level 2": group2,
  "Risk Level 3": group3,
  "Risk Level 4": group4,
  "Risk Level 5": group5,
};

// add legend to map
const legend = L.control({ position: "bottomright" });

legend.onAdd = function (map) {
  const div = L.DomUtil.create("div", "info legend"),
    grades = [1, 2, 3, 4, 5]; // Adjusted grades array

  // loop through our density intervals and generate a label with a colored square for each interval
  for (var i = 0; i < grades.length; i++) {
    div.innerHTML +=
      '<i style="background:' +
      getColor(grades[i]) + // Use getColor function with the current grade
      '"></i> ' +
      "Risk Level " +
      grades[i] +
      "<br>";
  }
  return div;
};

legend.addTo(map);

// Iterate through overlayMaps and set all layers as checked
Object.keys(overlayMaps).forEach((key) => {
  map.addLayer(overlayMaps[key]);
});

// Add layer control to the map with all layers checked by default
const layercontrol = L.control
  .layers({ StreetMap: streetMap, TopoMap: topoMap }, overlayMaps, {
    collapsed: false,
  })
  .addTo(map);

// Select StreetMap by default
streetMap.addTo(map);
