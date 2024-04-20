import { allCities } from "../data/allCitiesPred.js";

function extractCityAndCountry(data) {
  // Create an empty object to store unique city-country pairs
  const cityCountryMap = {};

  // Iterate over each feature in the dataset
  data.features.forEach((feature) => {
    const city = feature.properties.city;
    const country = feature.properties.country;

    // Create a unique key by concatenating city and country
    const key = city + ", " + country;

    // Add the key to the object if it doesn't exist already
    if (!cityCountryMap[key]) {
      cityCountryMap[key] = true;
    }
  });

  // Extract unique city-country pairs from the object keys
  const uniqueCityCountryPairs = Object.keys(cityCountryMap);

  // Split city and country and return as an array of objects
  const cityCountryArray = uniqueCityCountryPairs.map((pair) => {
    const [city, country] = pair.split(", ");
    return { city, country };
  });

  return cityCountryArray;
}

// Usage example
const citiesAndCountries = extractCityAndCountry(allCities);
console.log(citiesAndCountries);
