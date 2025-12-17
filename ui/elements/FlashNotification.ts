import TargetLocator from "../TargetLocator";

export default class FlashNotification extends HTMLElement {
  connectedCallback() {
    const locate = new TargetLocator("flash-notification", this);

    const buttonTarget = locate.target("button");
    if (!buttonTarget) {
      return;
    }

    this.classList.add("overflow-hidden", "transition-all");

    buttonTarget.addEventListener("click", () => {
      // Set the current specific height to transition from as values
      // like "auto" don't work.
      const { height } = this.getBoundingClientRect();
      this.style.height = `${height}px`;

      // Then a little while later...
      window.requestAnimationFrame(() => {
        this.style.height = "0";

        // When it's done transitioning...
        this.addEventListener("transitionend", () => {
          this.remove();
        });
      });
    });

    // Display the button as we now handle the click
    buttonTarget.classList.remove("hidden");
  }
}
