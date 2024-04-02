import { testData } from "../data/final_net_0401.js";

// divide all data by city to be used for rendering key indicators

export const cityObj = {
  Mumbai: [],
  Chennai: [],
  Bangkok: [],
  Santiago: [], // Add more cities if needed
};

// Check if cityData contains the features property
if (testData.features) {
  // Iterate through the features and organize them by city_country
  testData.features.forEach((feature) => {
    const city = feature.properties.city;
    if (cityObj.hasOwnProperty(city)) {
      cityObj[city].push(feature);
    } else {
      // If city doesn't exist in cityObj, create a new key
      cityObj[city] = [feature];
    }
  });
}
