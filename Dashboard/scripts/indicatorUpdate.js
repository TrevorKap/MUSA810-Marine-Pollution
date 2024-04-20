import { cityObj } from "./sortData.js";
import { litterData } from "../data/category.js";

// litter by type
export function percentageLvl(cityName) {
  const percentageLvl = document.querySelector(".js-percentage");
  percentageLvl.innerHTML = `${litterData[cityName].plasticPerc}`;
}

// calculate total litter counts for each city
export function totalCount(cityName) {
  let total = 0;

  cityObj[cityName].forEach((feature) => {
    total += feature.properties.count;
  });

  const totalC = document.querySelector(".js-total-count");
  totalC.innerHTML = `${total}`;
}

// key factors
// calculate average numbers for each factor
function calculateAverage(data, propertyName) {
  let totalCount = 0;
  const totalFeatures = data.length;

  data.forEach((feature) => {
    totalCount += feature.properties[propertyName];
  });

  return totalFeatures > 0 ? totalCount / totalFeatures : 0;
}

// Example usage:
export function updateIndicator(data) {
  const avgRisk = calculateAverage(cityObj[data], "Risk_Category").toFixed(2);
  const avgWaste = calculateAverage(cityObj[data], "waste_nn").toFixed(2);
  const avgRestaurant = calculateAverage(cityObj[data], "restaurant").toFixed(
    2
  );
  const avgWaterDis = calculateAverage(cityObj[data], "water_nn").toFixed(2);
  // add more later...

  // update risklevel
  const riskElm = document.querySelector(".js-number");
  riskElm.innerHTML = `${avgRisk}`;

  // Update the indicators in the html
  const countElm = document.querySelector(".js-waste");
  countElm.innerHTML = `
        <span class="number">${avgWaste}</span>
        <span class="number-stat">m</span>
        <p>Waste Avg Distance</p>
    `;

  const restaurantElm = document.querySelector(".js-restaraunt");
  restaurantElm.innerHTML = `
        <div class="number">${avgRestaurant}</div>
        <p>Restaurant Avg Num</p>
    `;

  const waterElm = document.querySelector(".js-water");
  waterElm.innerHTML = `
        <div class="number">${avgWaterDis}</div>
        <p>Water Avg Distance</p>
    `;
}
