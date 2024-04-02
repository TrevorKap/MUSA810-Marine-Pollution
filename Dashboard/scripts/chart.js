import { litterCategory } from "../data/category.js";

// Extract the required data from your JSON
const types = ["Plastic", "Paper", "Glass", "Cloth", "Cigarette"];

// default empty state

const render = document.querySelector(".js-chart");
render.innerHTML = `
    <div class="js-empty-state">
      <img src="./imgs/empty.svg">
      <p>Select a city to see the litter types</p>
    </div>
  `;

// when select city, chart update
export function matchCity(cityName) {
  render.innerHTML = ``;

  const percentages = litterCategory[cityName];

  // Define the chart configuration using the extracted data
  const options = {
    series: [
      {
        data: percentages,
      },
    ],
    chart: {
      type: "bar",
      height: 200,
    },
    plotOptions: {
      bar: {
        borderRadius: 4,
        horizontal: true,
      },
    },
    dataLabels: {
      enabled: true,
    },
    xaxis: {
      categories: types,
      labels: {
        align: "left", // Set x-axis labels alignment to left
      },
    },
    yaxis: {
      categories: percentages,
    },
    tooltip: {
      y: {
        formatter: function (val) {
          return "Percentage: " + val + "%";
        },
      },
    },
  };

  const chart = new ApexCharts(render, options);
  chart.render();
}
