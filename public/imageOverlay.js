function showOverlay(image) {
    let overlay = document.getElementById("image-overlay");
    overlay.innerHTML = `<img src="${image}" />`;
    overlay.style.visibility = "visible";
}

function closeOverlay() {
    let overlay = document.getElementById("image-overlay");
    overlay.innerHTML = "";
    overlay.style.visibility = "hidden";
}
