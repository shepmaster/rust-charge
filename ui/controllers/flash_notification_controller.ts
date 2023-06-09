import { Controller } from "@hotwired/stimulus";

export default class FlashNotificationController extends Controller<HTMLDivElement> {
  static targets = ["button"];

  declare readonly hasButtonTarget: boolean;
  declare readonly buttonTarget: HTMLButtonElement;
  declare readonly buttonTargets: HTMLButtonElement[];

  connect() {
    this.element.classList.add("overflow-hidden", "transition-all");

    this.buttonTarget.addEventListener("click", () => {
      // Set the current specific height to transition from as values
      // like "auto" don't work.
      const { height } = this.element.getBoundingClientRect();
      this.element.style.height = `${height}px`;

      // Then a little while later...
      window.requestAnimationFrame(() => {
        this.element.style.height = "0";

        // When it's done transitioning...
        this.element.addEventListener("transitionend", () => {
          this.element.remove();
        });
      });
    });

    // Display the button as we now handle the click
    this.buttonTarget.classList.remove("hidden");
  }
}
