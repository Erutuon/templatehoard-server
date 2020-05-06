"use strict";

const toggle = function () {
	this.displayed = !this.displayed;
	this.style.display = this.displayed ? null : "none";
	this.toggleButton.textContent = this.displayed ? "hide more info" : "show more info";
};

const onload = function(func) {
	if (document.readyState !== 'loading') {
		func();
	} else {
		document.addEventListener('DOMContentLoaded', func);
	}
}

// Toggle search explanation.
onload(function() {
	Array.prototype.forEach.call(document.getElementsByClassName("more-info"), function(elem) {
		const button = document.createElement("div");
		button.classList.add("toggle-button");
		$(button).on("click", function(e) {
			elem.toggle();
		});
		
		elem.toggleButton = button;
		elem.displayed = true;
		elem.toggle = toggle;
		elem.insertAdjacentElement("beforebegin", button);
		elem.toggle();
	})
});