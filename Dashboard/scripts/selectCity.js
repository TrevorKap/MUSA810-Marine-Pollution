import { matchCity } from "./chart.js";
import {
  updateIndicator,
  percentageLvl,
  totalCount,
} from "./indicatorUpdate.js";
import { changeMap } from "./map.js";

// Get references to the 1 and 2 dropdowns
const firstDropdown = document.getElementById("first-dropdown");
const secondDropdown = document.getElementById("second-dropdown");

// Set default value for chart and indicators
document.addEventListener("DOMContentLoaded", function () {
  matchCity(secondDropdown.value);
  updateIndicator(secondDropdown.value);
  percentageLvl(secondDropdown.value);
});

// Add event listener to the first dropdown
firstDropdown.addEventListener("change", function () {
  // Clear existing options in the second dropdown
  secondDropdown.innerHTML = "";

  // Get the selected value from the first dropdown
  const selectedValue = firstDropdown.value;

  // Populate the second dropdown based on the selected value from the first dropdown
  switch (selectedValue) {
    case "India":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Chennai">Chennai</option>
                <option value="Mumbai">Mumbai</option>
                <option value="Surat">Surat</option>
                <option value="Pune">Pune</option>
            `;
      break;
    case "Chile":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Santiago">Santiago</option>
            `;
      break;
    case "Thailand":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Bangkok">Bangkok</option>
            `;
      break;
    case "Vietnam":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Can_Tho">Can_Tho</option>
            `;
      break;
    case "Malaysia":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Melaka">Melaka</option>
            `;
      break;
    case "Panama":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Panama_City">Panama_City</option>
            `;
      break;
    case "Argentina":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Santa_Fe">Santa_Fe</option>
            `;
      break;
    case "Brazil":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Salvador">Salvador</option>
            `;
      break;
    case "Indonesia":
      secondDropdown.innerHTML = `
                <option value="Select City" disabled selected>Select City</option>
                <option value="Semarang">Semarang</option>
            `;
      break;
    default:
      // If no category is selected, keep the second dropdown empty
      break;
  }
});

// Add event listener to the second dropdown
secondDropdown.addEventListener("change", function () {
  // update chart
  matchCity(secondDropdown.value);

  // update figures
  updateIndicator(secondDropdown.value);
  percentageLvl(secondDropdown.value);
  totalCount(secondDropdown.value);

  // update map
  changeMap(secondDropdown.value);
});
