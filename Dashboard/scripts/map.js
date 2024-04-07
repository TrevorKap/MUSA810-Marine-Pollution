import { testData } from "../data/final_net_0401.js";
import { cityLocation } from "../data/cityLatLon.js";

export function changeMap(city) {
  map.setView(cityLocation[city], 12);
}

// initialize map
// mapbox access token
const mapboxAccessToken =
  "pk.eyJ1IjoieGlhb2Zhbi05OCIsImEiOiJjbG1tYTUyeDYwZ3Z0MnJsMXp5bzlhbmhuIn0.o4NFKmmhKwaWErRm16MjHA"; // Replace this with your Mapbox access token

// initialize map view
const map = L.map("map").setView(cityLocation["Chennai"], 12);

// add map tiles from mapbox
L.tileLayer(
  "https://api.mapbox.com/styles/v1/xiaofan-98/cltnsajw4028d01qe473s5rio/tiles/{z}/{x}/{y}?access_token=" +
    mapboxAccessToken,
  {
    maxZoom: 19,
    tileSize: 512,
    zoomOffset: -1,
    attribution:
      'Map data &copy; <a href="https://www.mapbox.com/">Mapbox</a> contributors, ' +
      '<a href="https://creativecommons.org/licenses/by/4.0/">CC-BY-4.0</a>',
  }
).addTo(map);

// set color range func
function getColor(x) {
  return x > 100
    ? "#9B2850"
    : x > 80
    ? "#CE284B"
    : x > 60
    ? "#E66F35"
    : x > 40
    ? "#F09A30"
    : x > 20
    ? "#FEC932"
    : x > 0
    ? "#F7DC8E"
    : "#FFEDA0";
}

// set style for grids
function style(feature) {
  return {
    fillColor: getColor(feature.properties.count), // color by range
    weight: 1.5,
    opacity: 1,
    color: "#ffffff",
    dashArray: "",
    fillOpacity: 0.7,
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
    "<h4>Litter Risk</h4>" +
      (feature.properties
        ? "<b>" +
          feature.properties.count +
          " count" +
          "</b><br />" +
          feature.properties.water_nn.toFixed(1) +
          "m /water_nn" +
          "</b><br />" +
          feature.properties.restaurant_nn.toFixed(1) +
          "m /restaurant_nn"
        : "No data available")
  );
}

// Create layer groups for different count ranges
const group1 = L.layerGroup();
const group2 = L.layerGroup();
const group3 = L.layerGroup();
const group4 = L.layerGroup();
const group5 = L.layerGroup();
const group6 = L.layerGroup();

// Populate layer groups based on count ranges
testData.features.forEach((feature) => {
  const count = feature.properties.count;
  const layer = L.geoJson(feature, {
    style: style,
    onEachFeature: onEachFeature,
  });

  if (count > 100) {
    group1.addLayer(layer);
  } else if (count > 80) {
    group2.addLayer(layer);
  } else if (count > 60) {
    group3.addLayer(layer);
  } else if (count > 40) {
    group4.addLayer(layer);
  } else if (count > 20) {
    group5.addLayer(layer);
  } else {
    group6.addLayer(layer);
  }
});

// Define control for layer groups with all layers checked by default
const overlayMaps = {
  "Count > 100": group1,
  "Count > 80": group2,
  "Count > 60": group3,
  "Count > 40": group4,
  "Count > 20": group5,
  "Count <= 20": group6,
};

// add legend to map
const legend = L.control({ position: "bottomright" });

legend.onAdd = function (map) {
  const div = L.DomUtil.create("div", "info legend"),
    grades = [0, 20, 40, 60, 80, 100],
    labels = [];

  // loop through our density intervals and generate a label with a colored square for each interval
  for (var i = 0; i < grades.length; i++) {
    div.innerHTML +=
      '<i style="background:' +
      getColor(grades[i] + 1) +
      '"></i> ' +
      grades[i] +
      (grades[i + 1] ? "&ndash;" + grades[i + 1] + "<br>" : "+");
  }
  return div;
};

legend.addTo(map);

// add geojson to map
// const geojson = L.geoJson(chennai, {
//     style: style,
//     onEachFeature: onEachFeature
// }).addTo(map);

// Iterate through overlayMaps and set all layers as checked
Object.keys(overlayMaps).forEach((key) => {
  map.addLayer(overlayMaps[key]);
});

// Add layer control to the map with all layers checked by default
const layercontrol = L.control
  .layers(null, overlayMaps, { collapsed: false })
  .addTo(map);
