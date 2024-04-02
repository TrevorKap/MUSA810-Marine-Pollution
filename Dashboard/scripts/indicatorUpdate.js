import { cityObj } from "./sortData.js";

// Generic function to calculate average numbers
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
  const avgCount = calculateAverage(cityObj[data], "count").toFixed(2);
  const avgRestaurant = calculateAverage(cityObj[data], "restaurant").toFixed(
    2
  );
  const avgWaterDis = calculateAverage(cityObj[data], "water_nn").toFixed(2);
  // add more later...

  // Update the indicators in the html
  const countElm = document.querySelector(".js-count");
  countElm.innerHTML = `
        Count Avg Num: ${avgCount}
    `;

  const restaurantElm = document.querySelector(".js-restaraunt");
  restaurantElm.innerHTML = `
        Restaurant Avg Num: ${avgRestaurant}
    `;

  const waterElm = document.querySelector(".js-water");
  waterElm.innerHTML = `
        Water Distance Average: ${avgWaterDis}m
    `;
}
