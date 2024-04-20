import { allCities } from "../data/allCitiesPred.js";

// divide all data by city to be used for rendering key indicators

export const cityObj = {
  Mumbai: [],
  Chennai: [],
  Bangkok: [],
  Santiago: [], // Add more cities if needed
  Bangkok: [],
  Can_Tho: [],
  Melaka: [],
  Panana_City: [],
  Pune: [],
  Salvador: [],
  Santa_Fe: [],
  Semarang: [],
};

// Check if cityData contains the features property
if (allCities.features) {
  // Iterate through the features and organize them by city_country
  allCities.features.forEach((feature) => {
    const city = feature.properties.city;
    if (cityObj.hasOwnProperty(city)) {
      cityObj[city].push(feature);
    } else {
      // If city doesn't exist in cityObj, create a new key
      cityObj[city] = [feature];
    }
  });
}
