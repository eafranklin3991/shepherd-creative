function toggleMenu() {
    const menu = document.getElementById("menu");
    menu.style.display = menu.style.display === "block" ? "none" : "block";
}

const pitchVideo = document.querySelector("#pitch-video");
pitchVideo.setAttribute('preload', 'auto');
pitchVideo.load();