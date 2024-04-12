document.addEventListener("DOMContentLoaded", function () {
  // Get the modal
  const modal = document.querySelector(".js-modal");
  console.log("Modal:", modal);

  // Get the <span> element that closes the modal
  const span = document.querySelector(".js-close");
  console.log("Close button:", span);

  // When the user clicks on <span> (x) or anywhere outside of the modal, close the modal
  function closeModal() {
    modal.style.display = "none";
  }

  if (span) {
    span.onclick = closeModal;
  }

  window.onclick = function (event) {
    if (event.target == modal) {
      closeModal();
    }
  };
});
