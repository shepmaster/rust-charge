import { Controller } from "@hotwired/stimulus";

export default class DetectTimezoneController extends Controller {
  static targets = ["button", "select"];

  declare readonly buttonTarget: HTMLButtonElement;
  declare readonly selectTarget: HTMLSelectElement;

  connect() {
    this.buttonTarget.classList.remove("hidden");

    this.buttonTarget.addEventListener("click", (e) => {
      const { timeZone } = Intl.DateTimeFormat().resolvedOptions();
      this.selectTarget.value = timeZone;
      e.preventDefault();
    });
  }
}
